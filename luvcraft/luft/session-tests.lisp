;;;; Two worlds in one image, over the loopback network (#OLWSWF).
(in-package #:luvcraft.luft.game.tests)

(defun make-resident (scene keys &optional (collision-keys keys))
  "Materialize KEYS in SCENE from its source and edits, as streaming would,
with collision for COLLISION-KEYS."
  (let ((source (terrain:streaming-scene-source scene)))
    (dolist (key keys)
      (setf (gethash key (terrain:streaming-scene-store scene))
            (terrain:materialize-world-source-chunk
             source key 1 :edits (terrain:capture-world-source-chunk-edits source key))))
    (terrain:rebuild-authored-world-resident-values scene keys collision-keys)
    scene))

(defparameter *session-chunk* (luft:chunk-key-at 1024 1024))

(defun session-test-world ()
  "A small host world with one resident chunk and its player Ada."
  (let* ((block-world (luvcraft:make-empty-little-block-world :seed 291 :chunk-height 16))
         (source (game:luvcraft-world-luft-source block-world))
         (world (host:make-luvcraft-world
                 :block-world block-world
                 :scene (make-resident (game:make-luvcraft-streaming-scene source)
                                       (list *session-chunk*)))))
    (host:add-world-player
     world (host:make-luvcraft-player
            :id "ada" :name "Ada"
            :character (terrain:make-walking-character
                        :position (v:make-vec3 1060.5d0 1060.5d0 (float (ground world 1060 1060) 1d0)))))
    world))

(defun ground (world x y)
  "One above the highest solid cell of column X, Y in WORLD's scene."
  (loop for z from 60 downto 0
        when (terrain:scene-material-placement-at (host:world-scene world) (cell world x y z))
          return (1+ z)
        finally (return 0)))

(defun cell (world x y z)
  (luft:make-site (terrain:scene-domain (host:world-scene world)) x y z luft:+cell-extent+ 1))

(defun air-cell (world x y &optional (above 2))
  "A cell ABOVE cells over the ground at X, Y: clear of any fixture's support."
  (cell world x y (+ (ground world x y) above)))

(defun pump (sessions &key (until (constantly nil)) (seconds 5) (step 1/60) advance)
  "Advance SESSIONS until UNTIL is true, at most SECONDS of wall time.
ADVANCE also steps each session's world's simulation, by the wall time each
round took, as a frame does, and none of it discarded: remote tracks play back on the wall clock, so a
simulation stepped by a fixed STEP would run ahead of or behind its sender's
clock.  Without ADVANCE each round is STEP."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second)))
        (then (get-internal-real-time)))
    (loop
      (let* ((now (get-internal-real-time))
             (step (if advance (/ (- now then) internal-time-units-per-second) step)))
        (setf then now)
        (dolist (session sessions)
          (when advance
            ;; In pieces under the simulation's catch-up limit, so a slow
            ;; round under load is run rather than discarded.
            (loop with simulation = (host:world-simulation (host:session-world session))
                  for left = step then (- left piece)
                  for piece = (min left 1/40)
                  while (plusp left)
                  do (terrain:advance-world-simulation simulation piece)))
          (host:advance-world-session session step)))
      (when (funcall until) (return t))
      (when (> (get-internal-real-time) deadline) (return nil))
      (sleep 0.005))))

(defun join-while-pumping (host-session network identity name)
  "Join HOST-SESSION's world as IDENTITY from another thread while pumping
the host; return the guest's world, player, session, and pose."
  (let* ((result nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (setf result (multiple-value-list
                                  (handler-case
                                      (host:join-luvcraft-world "host" :loopback network
                                                                       :identity identity :name name)
                                    (error (condition) condition)))))
                  :name "joining guest")))
    (pump (list host-session) :until (lambda () (not (sb-thread:thread-alive-p thread))))
    (sb-thread:join-thread thread)
    (when (typep (first result) 'error) (error (first result)))
    (values-list result)))

(defun journal-tail (world from)
  "WORLD's journal events after sequence FROM, as comparable lists."
  (let ((journal (terrain:world-source-journal (host:world-source world))))
    (loop for event across (terrain:world-journal-events journal)
          when (> (terrain:world-event-sequence event) from)
            collect (list (terrain:world-event-sequence event) (terrain:world-event-actor event)
                          (terrain:world-event-site event) (terrain:world-event-before event)
                          (terrain:world-event-after event)))))

(defun edit-set (world)
  (let ((pairs nil))
    (maphash (lambda (site value) (push (cons site value) pairs))
             (terrain:world-source-edits (host:world-source world)))
    (sort pairs #'< :key #'car)))

(defun worlds-agree-p (host guest from)
  "Whether HOST's and GUEST's journals agree after FROM and in their edit sets,
and their resident chunk hashes are equal."
  (and (equal (journal-tail host from) (journal-tail guest from))
       (equal (edit-set host) (edit-set guest))
       (equal (terrain:streaming-scene-chunk-hash (host:world-scene host) *session-chunk*)
              (terrain:streaming-scene-chunk-hash (host:world-scene guest) *session-chunk*))))

(defmacro with-host ((world session network) &body body)
  `(let* ((,network (luvcraft.wire.net:make-loopback-network))
          (,world (session-test-world))
          (,session (host:host-luvcraft-world ,world (host:find-world-player ,world "ada")
                                              :loopback ,network :name "host")))
     (unwind-protect (progn ,@body)
       (host:close-world-session ,session))))

(defun join-resident (session network identity name)
  "Join as IDENTITY and make the test chunk resident in the guest's world."
  (multiple-value-bind (world player guest pose) (join-while-pumping session network identity name)
    (make-resident (host:world-scene world) (list *session-chunk*))
    (values world player guest pose)))

(defun applied-p (guest host-world)
  "Whether GUEST has every reply it asked for and stands at HOST-WORLD's head.
Until the host has its requests, both stand at the same old head."
  (lambda ()
    (and (= (length (host::session-replies guest)) (host::session-next-request guest))
         (= (host:session-applied guest)
            (terrain:world-journal-head (terrain:world-source-journal (host:world-source host-world)))))))

(define-test a-guest-catches-up-while-the-host-builds
  (with-host (world session network)
    (let* ((ada (host:find-world-player world "ada"))
           (stone (game:block-placement luvcraft::*stone-block*))
           (planks (game:block-placement luvcraft::*planks-block*))
           (film (luvcraft::ensure-film "session-catch-up"))
           (cells (loop for x from 1030 below 1033 collect (air-cell world x 1030)))
           (reel (air-cell world 1036 1030)))
      (dolist (cell cells)
        (is eq :edited (nth-value 1 (host:player-edit-world-cell world ada cell stone))))
      ;; A film travels as its entity's save description.
      (luvcraft:add-block-to-inventory (luvcraft:player-inventory ada) film 1)
      (setf (luvcraft:player-selected-block ada) film)
      (is eq :edited (nth-value 1 (host:player-edit-world-cell world ada reel
                                                               (game:block-placement film))))
      (let ((watermark nil) (guest-world nil) (guest nil)
            (thread nil) (result nil))
        ;; Bea says hello; the host welcomes her at the current head ...
        (setf thread (sb-thread:make-thread
                      (lambda ()
                        (setf result (multiple-value-list
                                      (host:join-luvcraft-world "host" :loopback network
                                                                       :identity "bea" :name "Bea"))))))
        (true (pump (list session) :until (lambda () (host::session-guests session))))
        (setf watermark (terrain:world-journal-head
                         (terrain:world-source-journal (host:world-source world))))
        (is = 4 watermark)
        ;; ... and goes on building before her world even exists.
        (is eq :edited (nth-value 1 (host:player-edit-world-cell world ada (first cells) planks)))
        (is eq :edited (nth-value 1 (host:player-edit-world-cell world ada (second cells) nil)))
        (pump (list session) :seconds 0.1)
        (sb-thread:join-thread thread)
        (setf guest-world (first result) guest (third result))
        (is = watermark (host:session-applied guest))
        ;; The edit set arrived compacted, film entity and all.
        (is eq film (host::world-authored-block guest-world reel))
        (make-resident (host:world-scene guest-world) (list *session-chunk*))
        (true (pump (list session guest) :until (applied-p guest world)))
        ;; Neither drops nor repeats an event: the tails after the watermark
        ;; agree, sequence for sequence, and so do the cells and chunk hashes.
        (is = 2 (length (journal-tail guest-world watermark)))
        (true (worlds-agree-p world guest-world watermark))
        (is eq planks (terrain:scene-material-placement-at (host:world-scene guest-world) (first cells)))
        (is eq nil (terrain:scene-material-placement-at (host:world-scene guest-world) (second cells)))
        (is eq stone (terrain:scene-material-placement-at (host:world-scene guest-world) (third cells)))
        ;; Each sees the other as a remote player.
        (true (pump (list session guest)
                    :until (lambda () (host:find-world-player guest-world "ada"))))
        (true (host:player-track (host:find-world-player world "bea")))
        (true (host:player-track (host:find-world-player guest-world "ada")))
        ;; A guest's own edit is a request; it shows once the host confirms it.
        (let* ((bea (host:find-world-player guest-world "bea"))
               (target (air-cell guest-world 1032 1034)))
          (is eq :requested (nth-value 1 (host:player-edit-world-cell guest-world bea target stone)))
          (is eq nil (terrain:scene-material-placement-at (host:world-scene guest-world) target))
          (true (pump (list session guest) :until (applied-p guest world)))
          (is eq stone (terrain:scene-material-placement-at (host:world-scene guest-world) target))
          (is eq stone (terrain:scene-material-placement-at (host:world-scene world) target))
          (is equal "bea" (terrain:world-event-actor
                           (aref (terrain:world-journal-events
                                  (terrain:world-source-journal (host:world-source world)))
                                 (1- (length (terrain:world-journal-events
                                              (terrain:world-source-journal (host:world-source world)))))))))
        (true (worlds-agree-p world guest-world watermark))
        (host:close-world-session guest)))))

(define-test two-players-filling-one-cell-have-one-winner
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (let* ((ada (host:find-world-player world "ada"))
             (from (host:session-applied guest))
             (target (air-cell world 1040 1036))
             (stone (game:block-placement luvcraft::*stone-block*))
             (planks (game:block-placement luvcraft::*planks-block*)))
        ;; Bea asks for stone from the air she sees; before the host hears
        ;; her, Ada fills the same cell with planks.
        (is eq :requested (nth-value 1 (host:player-edit-world-cell guest-world bea target stone)))
        (is eq :edited (nth-value 1 (host:player-edit-world-cell world ada target planks)))
        (true (pump (list session guest) :until (lambda () (host:session-replies guest))))
        (true (pump (list session guest) :until (applied-p guest world)))
        (let ((reply (first (host:session-replies guest))))
          (is eq :rejected (luvcraft.wire:edit-reply-message-outcome reply))
          (is eq :stale-before (luvcraft.wire:edit-reply-message-reason reply)))
        (is eq planks (terrain:scene-material-placement-at (host:world-scene world) target))
        (is eq planks (terrain:scene-material-placement-at (host:world-scene guest-world) target))
        (is = 1 (length (journal-tail world from)))
        (true (worlds-agree-p world guest-world from))
        (host:close-world-session guest)))))

(define-test each-player-undoes-only-its-own-edits
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (let* ((ada (host:find-world-player world "ada"))
             (from (host:session-applied guest))
             (mine (air-cell world 1030 1040))
             (hers (air-cell world 1031 1040))
             (stone (game:block-placement luvcraft::*stone-block*))
             (bricks (game:block-placement luvcraft::*bricks-block*)))
        (host:player-edit-world-cell guest-world bea hers stone)
        (true (pump (list session guest) :until (applied-p guest world)))
        (host:player-edit-world-cell world ada mine bricks)
        (true (pump (list session guest) :until (applied-p guest world)))
        ;; Bea's undo takes back her stone and leaves Ada's bricks.
        (host:session-undo guest)
        (true (pump (list session guest)
                    :until (lambda ()
                             (and (funcall (applied-p guest world))
                                  (null (terrain:scene-material-placement-at
                                         (host:world-scene guest-world) hers))))))
        (dolist (w (list world guest-world))
          (is eq nil (terrain:scene-material-placement-at (host:world-scene w) hers))
          (is eq bricks (terrain:scene-material-placement-at (host:world-scene w) mine)))
        ;; Her mirror of the journal knows the undo was hers, by its own ids.
        (let ((journal (terrain:world-source-journal (host:world-source guest-world))))
          (is eq nil (terrain:world-journal-undoable-transaction journal "bea"))
          (true (terrain:world-journal-redoable-transaction journal "bea")))
        ;; Ada's undo takes back her bricks.
        (is eq :edited (nth-value 1 (host:session-undo session)))
        (true (pump (list session guest) :until (applied-p guest world)))
        (dolist (w (list world guest-world))
          (is eq nil (terrain:scene-material-placement-at (host:world-scene w) mine)))
        (true (worlds-agree-p world guest-world from))
        (host:close-world-session guest)))))

(define-test a-guest-leaves-and-rejoins-where-it-left
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (let* ((ada (host:find-world-player world "ada"))
             (stone (game:block-placement luvcraft::*stone-block*))
             (mine (air-cell world 1034 1044))
             (meanwhile (air-cell world 1035 1044)))
        (host:add-world-player guest-world bea)
        (setf (host:player-character bea)
              (terrain:make-walking-character
               :position (v:make-vec3 1050.5d0 1044.5d0 (float (ground guest-world 1050 1044) 1d0))))
        (host:player-edit-world-cell guest-world bea mine stone)
        (true (pump (list session guest) :advance t
                    :until (lambda ()
                             (and (funcall (applied-p guest world))
                                  (host:track-shown (host:player-track (host:find-world-player world "bea")))))))
        ;; A second Bea is turned away while the first plays.
        (fail (join-while-pumping session network "bea" "Another Bea"))
        (host:close-world-session guest)
        (true (pump (list session) :until (lambda () (null (host:find-world-player world "bea")))))
        (host:player-edit-world-cell world ada meanwhile stone)
        ;; She comes back to a fresh world raised from the host's, where she left.
        (multiple-value-bind (again bea-again guest-again pose) (join-resident session network "bea" "Bea")
          (true (not (eq again guest-world)))
          (true (< (abs (- (aref (luvcraft.wire:pose-position pose) 0) 1050.5d0)) 1d0))
          (true (pump (list session guest-again) :until (applied-p guest-again world)))
          (dolist (target (list mine meanwhile))
            (is eq stone (terrain:scene-material-placement-at (host:world-scene again) target)))
          (true (worlds-agree-p world again (host:session-applied guest-again)))
          (true (equal (edit-set world) (edit-set again)))
          ;; The host kept her history: her undo still takes back her stone.
          (host:session-undo guest-again)
          (true (pump (list session guest-again)
                      :until (lambda () (null (terrain:scene-material-placement-at
                                               (host:world-scene again) mine)))))
          (is eq stone (terrain:scene-material-placement-at (host:world-scene world) meanwhile))
          (is eq nil (terrain:scene-material-placement-at (host:world-scene world) mine))
          (true (equal (edit-set world) (edit-set again)))
          (true bea-again)
          (host:close-world-session guest-again))))))

(define-test poses-travel-stamped-with-the-simulation-tick
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (setf (host:player-character bea)
            (terrain:make-walking-character
             :position (v:make-vec3 1050.5d0 1040.5d0 (float (ground guest-world 1050 1040) 1d0))))
      (host:add-world-player guest-world bea)
      (terrain:set-character-movement (host:player-character bea) 1d0 0d0)
      (let ((track nil))
        (true (pump (list session guest) :advance t :seconds 5
                    :until (lambda ()
                             (let ((player (host:find-world-player world "bea")))
                               (setf track (and player (host:player-track player)))
                               (and track (> (length (host::track-snapshots track)) 10))))))
        (let* ((newest (let ((snapshots (host::track-snapshots track)))
                         (aref snapshots (1- (length snapshots)))))
               (shown (host:track-shown track))
               (body (terrain:body-position (terrain:character-body (host:player-character bea)))))
          ;; Stamped with Bea's simulation tick, at 120 a second.
          (true (<= (* 120 (host:pose-snapshot-sent newest)) (host:world-tick guest-world)))
          (true (integerp (round (* 120 (host:pose-snapshot-sent newest)))))
          ;; Drawn a little behind where she walks, never ahead of her: by
          ;; the track's delay and the newest pose's age, at her speed.  Both
          ;; grow under load, when rounds and arrivals are uneven.
          (true shown)
          (let* ((age (- (/ (host:world-tick guest-world) 120d0) (host:pose-snapshot-sent newest)))
                 (speed (v:vec3-x (terrain:body-velocity
                                   (terrain:character-body (host:player-character bea)))))
                 (behind (* speed (+ (host:track-delay track) age))))
            (true (< (- (v:vec3-x body) behind 0.5) (aref (host:pose-snapshot-position shown) 0)
                     (+ (v:vec3-x body) 0.5)))))
        (host:close-world-session guest)))))

(define-test a-remote-track-plays-a-little-behind-and-leaps-teleports
  (let ((track (host:make-remote-track)))
    ;; Snapshots at 30 Hz of a walk at 4 cells a second, arriving on time.
    (loop for i from 0 below 30
          for now = (* i 1/30)
          do (host:push-pose-snapshot
              track (host:make-pose-snapshot :sent (+ 100d0 now)
                                             :position (vector (+ 10d0 (* 4 now)) 0d0 0d0))
              (float now 1d0))
             (host:advance-remote-track track (float now 1d0) 1/30))
    (let* ((now (float 29/30 1d0))
           (shown (host:advance-remote-track track (+ now 1/60) 1/60))
           (x (aref (host:pose-snapshot-position shown) 0))
           (newest (+ 10d0 (* 4 now))))
      ;; Between 0.05 and 0.3 seconds behind the newest word.
      (true (<= 0.05d0 (host:track-delay track) 0.3d0))
      (true (< (- newest (* 4 0.35)) x newest)))
    ;; A leap far too long to have been walked is drawn as a teleport.
    (host:push-pose-snapshot track (host:make-pose-snapshot :sent 101.1d0
                                                            :position (vector 500d0 0d0 0d0))
                             1.1d0)
    (loop repeat 30 for now from 1.1d0 by 1/60 do (host:advance-remote-track track now 1/60))
    (is = 500d0 (aref (host:pose-snapshot-position (host:track-shown track)) 0))
    ;; Silence for longer than its patience: the player has gone.
    (is eq nil (host:advance-remote-track track 10d0 1/60))))

(define-test peers-log-chunks-whose-hashes-differ
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (declare (ignore bea))
      (true (pump (list session guest) :until (applied-p guest world)))
      (host:report-chunk-hashes guest)
      (true (pump (list session guest) :until (lambda () (plusp (host:session-matched-chunks session)))))
      (is eq nil (host:session-mismatched-chunks session))
      ;; A cell changed behind the journal's back is a divergence.
      (true (terrain:edit-streaming-scene-cell (host:world-scene guest-world)
                                               (air-cell guest-world 1044 1044)
                                               (game:block-placement luvcraft::*stone-block*)))
      (host:report-chunk-hashes guest)
      (true (pump (list session guest) :until (lambda () (host:session-mismatched-chunks session))))
      (is = *session-chunk* (first (first (host:session-mismatched-chunks session))))
      (host:close-world-session guest))))

(define-test a-peer-report-arriving-mid-hash-is-still-compared
  ;; Both peers report every two seconds from their start, so a guest's
  ;; report often lands while the host is hashing its own.
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (declare (ignore guest-world bea))
      (true (pump (list session guest) :until (applied-p guest world)))
      (flet ((posted-p (s) (lambda () (plusp (sb-concurrency:mailbox-count
                                              (host::session-hash-results s))))))
        (dolist (s (list session guest))
          (true (pump (list s) :until (lambda () (null (host::session-hash-job s)))))
          (setf (host::session-hash-due s) 1d12))
        ;; The host's own report is hashed but not yet taken up by a frame.
        (true (host:report-chunk-hashes session))
        (true (pump nil :until (posted-p session)))
        (true (host:report-chunk-hashes guest))
        (true (pump nil :until (posted-p guest)))
        (host:advance-world-session guest 0)
        ;; One host frame receives the guest's report while its job runs.
        (host:advance-world-session session 0)
        (true (pump nil :until (lambda ()
                                 (host::finish-chunk-hashes session)
                                 (plusp (host:session-matched-chunks session)))
                    :seconds 2))
        (is eq nil (host:session-mismatched-chunks session)))
      (host:close-world-session guest))))

(define-test a-guest-builds-where-the-host-has-no-collision
  ;; The host simulates only its own player's neighbourhood; a guest may
  ;; build in any chunk the host shows.
  (with-host (world session network)
    (let ((far (luft:chunk-key-at 1088 1024)))
      (make-resident (host:world-scene world) (list *session-chunk* far) (list *session-chunk*))
      (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
        (make-resident (host:world-scene guest-world) (list *session-chunk* far))
        (let ((target (air-cell world 1100 1030))
              (ground (air-cell world 1102 1030 -1))
              (stone (game:block-placement luvcraft::*stone-block*)))
          (is = far (luft:site-chunk-key target))
          (true (terrain:scene-material-placement-at (host:world-scene world) ground))
          ;; She builds there, and digs: the host knows that ground only by
          ;; its resident fibers.
          (host:player-edit-world-cell guest-world bea target stone)
          (host:player-edit-world-cell guest-world bea ground nil)
          (true (pump (list session guest) :until (applied-p guest world)))
          (is = 2 (host:session-applied guest))
          (dolist (w (list world guest-world))
            (is eq stone (terrain:scene-material-placement-at (host:world-scene w) target))
            (is eq nil (terrain:scene-material-placement-at (host:world-scene w) ground)))
          (is equal (terrain:streaming-scene-chunk-hash (host:world-scene world) far)
              (terrain:streaming-scene-chunk-hash (host:world-scene guest-world) far))
          (host:close-world-session guest))))))

(define-test sequenced-events-wait-while-residency-moves
  ;; Between two residency windows a published owner can read a chunk that
  ;; has already left the store; an event applied then would mesh it.
  (let* ((world (session-test-world))
         (scene (host:world-scene world))
         (ada (host:find-world-player world "ada"))
         (edge (luft:chunk-key-at (* 64 18) (* 64 16)))
         (target (air-cell world 1030 1030)))
    (setf (gethash edge (terrain:streaming-scene-loaded scene)) 1)
    (is eq :queued (nth-value 1 (host:player-edit-world-cell
                                 world ada target (game:block-placement luvcraft::*stone-block*))))
    (is eq nil (terrain:scene-material-placement-at scene target))
    (remhash edge (terrain:streaming-scene-loaded scene))
    (is = 1 (terrain:apply-queued-world-events scene nil nil))
    (true (terrain:scene-material-placement-at scene target))))

;;; Interest (#I6O4J6).  More chunks are resident for meshing than collide.

(define-test creatures-wait-where-their-ground-has-no-collision
  ;; After a seam crossing, or as a save loads, a creature can stand in a
  ;; chunk that is resident for meshing but outside every collision window.
  ;; Absent collision is air, so it used to fall there forever.
  (let* ((block-world (luvcraft:make-empty-little-block-world :seed 291 :chunk-height 16))
         (source (game:luvcraft-world-luft-source block-world))
         (near (luft:chunk-key-at 1024 1024))
         (far (luft:chunk-key-at 1152 1024))
         (scene (make-resident (game:make-luvcraft-streaming-scene source)
                               (list near (luft:chunk-key-at 1088 1024) far)
                               (list near)))
         (world (host:make-luvcraft-world :block-world block-world :scene scene))
         (x 1182) (y 1050)
         (z (ground world x y))
         (turtle (make-instance 'luvcraft::turtle :seed 3
                                :position (v:make-vec3 (- x 1024 -0.5d0) (float z 1d0)
                                                       (- y 1024 -0.5d0))))
         (gnome (host::make-inhabitant world :gnome
                                       (v:make-vec3 (+ x 3.5d0) (+ y 0.5d0)
                                                    (float (ground world (+ x 3) y) 1d0))))
         ;; Controls: in collision, one of each drops from the air to the ground.
         (falling-turtle (make-instance 'luvcraft::turtle :seed 4
                                        :position (v:make-vec3 30.5d0 (+ 3d0 (ground world 1054 1030))
                                                               6.5d0)))
         (falling-gnome (host::make-inhabitant world :gnome
                                               (v:make-vec3 1040.5d0 1040.5d0
                                                            (+ 3d0 (ground world 1040 1040))))))
    (setf (host::world-animals world) (list turtle falling-turtle)
          (host::world-animals-initialized-p world) t
          (host::world-agents world) (list gnome falling-gnome)
          (host::world-agents-initialized-p world) t)
    (true (host::cell-resident-p world x y) "the far chunk is resident for meshing")
    (true (not (terrain:scene-collision-resident-p scene x y)) "but has no collision")
    (flet ((advance (seconds)
             (dotimes (i (round seconds 1/60))
               (host::advance-world-animals world 1/60)
               (host::advance-world-companions world 1/60)))
           (turtle-z (turtle) (v:vec3-y (luvcraft::critter-position turtle)))
           (gnome-z (gnome) (v:vec3-z (host::inhabitant-position gnome))))
      (advance 2)
      (is = z (turtle-z turtle))
      (is = (ground world (+ x 3) y) (gnome-z gnome))
      (true (< (abs (- (ground world 1054 1030) (turtle-z falling-turtle))) 0.01))
      (true (< (abs (- (ground world 1040 1040) (gnome-z falling-gnome))) 0.01))
      ;; Once collision covers them, they stand on it.
      (terrain:install-streaming-collision scene (list near far))
      (advance 2)
      (true (< (abs (- z (turtle-z turtle))) 0.01))
      (true (< (abs (- (ground world (+ x 3) y) (gnome-z gnome))) 0.01)))))

(defun stream-scene (scene producer x y &key (until (constantly t)) (seconds 20))
  "Retarget SCENE around X, Y on PRODUCER and install every result, as a view's
frames would, until UNTIL holds with nothing in flight."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (flet ((idle-p ()
             (and (zerop (hash-table-count (terrain:streaming-scene-load-outstanding scene)))
                  (null (terrain:streaming-scene-replacement scene)))))
      (loop
        ;; Settled when a retarget with nothing in flight demands nothing.
        (let* ((idle-p (idle-p))
               (quiet-p (null (terrain:retarget-streaming-scene scene producer 1 x y))))
          (when (and idle-p quiet-p (idle-p) (funcall until)) (return t)))
        (loop for (result present-p)
                = (multiple-value-list (luv.production:receive-production-result-no-hang producer))
              while present-p
              do (if (typep (luv.production:production-result-request result)
                            'terrain::streaming-mesh-request)
                     (terrain::receive-streaming-mesh-result scene result)
                     (terrain::receive-streaming-source-result scene result)))
        (let ((replacement (terrain:streaming-scene-replacement scene)))
          (when (and replacement (terrain:streaming-replacement-result replacement))
            (when (terrain:streaming-replacement-request replacement)
              (setf (terrain:streaming-scene-mesh-cache scene)
                    (terrain:streaming-mesh-result-mesh-cache
                     (terrain:streaming-replacement-result replacement))))
            (setf (terrain:streaming-scene-replacement scene) nil)))
        (when (> (get-internal-real-time) deadline) (return nil))
        (sleep 0.005)))))

(define-test the-host-collides-beside-a-distant-guest
  (let ((producer (luv.production:make-single-worker-production-system :name "interest")))
    (unwind-protect
         (with-host (world session network)
           (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
             (let* ((scene (host:world-scene world))
                    (ada (terrain:body-position
                          (terrain:character-body (host:player-character
                                                   (host:find-world-player world "ada")))))
                    ;; Five chunks east of Ada: outside her collision and her view.
                    (x 1380) (y 1060)
                    (far (luft:chunk-key-at x y))
                    (z (progn (make-resident (host:world-scene guest-world) (list far))
                              (ground guest-world x y)))
                    (body (terrain:character-body
                           (setf (host:player-character bea) (terrain:make-walking-character)))))
               (flet ((stream (&rest arguments)
                        (apply #'stream-scene scene producer (v:vec3-x ada) (v:vec3-y ada)
                               arguments)))
                 (true (stream))
                 (true (not (terrain:scene-collision-resident-p scene x y)))
                 ;; Bea walks there; her poses declare her focus on the host.
                 (setf (v:vec3-x (terrain:body-position body)) (+ x 0.5d0)
                       (v:vec3-y (terrain:body-position body)) (+ y 0.5d0)
                       (v:vec3-z (terrain:body-position body)) (float z 1d0))
                 (true (pump (list session guest)
                             :until (lambda ()
                                      (eql far (gethash "bea" (terrain:streaming-scene-interest
                                                               scene))))))
                 (true (stream :until (lambda () (terrain:scene-collision-resident-p scene x y))))
                 ;; Collision is resident around both players; publication
                 ;; follows only Ada.
                 (true (terrain:scene-collision-resident-p scene (+ x 64) y))
                 (true (terrain:scene-collision-resident-p scene (floor (v:vec3-x ada)) (floor (v:vec3-y ada))))
                 (true (every (lambda (key)
                                (<= (abs (- (luft:chunk-key-x key) (luft:chunk-key-x *session-chunk*))) 1))
                              (terrain::streaming-scene-loaded-source-keys scene)))
                 (true (not (nth-value 1 (gethash far (terrain:streaming-scene-loaded scene)))))
                 ;; A body walking beside Bea in the host's simulation stays on
                 ;; the ground the guest sees there.
                 (let ((walker (terrain:make-walking-character
                                :position (v:make-vec3 (+ x 0.5d0) (+ y 0.5d0) (+ z 2d0))))
                       (simulation (host:world-simulation world)))
                   (terrain:add-simulation-character simulation walker)
                   (terrain:set-character-movement walker 1d0 0d0)
                   (dotimes (i 60) (terrain:advance-world-simulation simulation 1/60))
                   (let ((p (terrain:body-position (terrain:character-body walker))))
                     (true (> (v:vec3-x p) (+ x 2)))
                     (true (terrain:body-grounded-p (terrain:character-body walker)))
                     (is = (ground guest-world (floor (v:vec3-x p)) (floor (v:vec3-y p)))
                         (round (v:vec3-z p))))
                   (terrain:remove-simulation-character simulation walker))
                 ;; When she leaves, her focus and its collision go.
                 (host:close-world-session guest)
                 (true (pump (list session)
                             :until (lambda ()
                                      (null (terrain:streaming-scene-interest-keys scene)))))
                 (true (stream :until (lambda ()
                                        (not (terrain:scene-collision-resident-p scene x y)))))
                 (true (not (nth-value 1 (gethash far (terrain:streaming-scene-store scene)))))))))
      (luv.production:stop-production-system producer))))

(define-test a-remote-player-is-a-stranger-to-local-physics
  ;; Bea stands still in her world; Ada's host world meets her as its track
  ;; draws her: Ada cannot walk through her, a ball bounces off her, and
  ;; neither world places a block in the other's body (#VM5TN6).
  (with-host (world session network)
    (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
      (let* ((ground (ground guest-world 1050 1060))
             (ada (host:find-world-player world "ada"))
             (ada-body (terrain:character-body (host:player-character ada)))
             (stone (game:block-placement luvcraft::*stone-block*)))
        (setf (host:player-character bea)
              (terrain:make-walking-character
               :position (v:make-vec3 1050.5d0 1060.5d0 (float ground 1d0))))
        (host:add-world-player guest-world bea)
        (true (pump (list session guest) :advance t
                    :until (lambda ()
                             (and (terrain:simulation-strangers (host:world-simulation world))
                                  (terrain:simulation-strangers (host:world-simulation guest-world))))))
        ;; Ada walks west into her and stops at her side; unhindered, two
        ;; seconds would take her ten cells past.
        (terrain:set-character-movement (host:player-character ada) -4d0 0d0)
        (dotimes (i 120) (terrain:advance-world-simulation (host:world-simulation world) 1/60))
        (true (< 1051.09 (v:vec3-x (terrain:body-position ada-body)) 1051.5))
        (terrain:set-character-movement (host:player-character ada) 0d0 0d0)
        ;; A ball thrown at Bea from the west comes back west.  Physics is
        ;; Y-up and 1024 cells off: her body spans x 26.2..26.8 there.
        (let* ((physics (host::ensure-world-physics world))
               (ball (luvcraft::spawn-physics-body physics 23.5 (+ ground 0.7) 36.5
                                                   :radius 0.25 :vx 6.0 :restitution 0.5))
               (farthest 0.0))
          (dotimes (i 90)
            (host::advance-world-balls world luvcraft::*physics-step-seconds*)
            (setf farthest (max farthest (luvcraft::physics-body-position physics ball))))
          (true (< farthest 26.1))
          (true (< (luvcraft::physics-body-position physics ball) 25.9)))
        ;; Placement into either body is refused where it is asked for, once
        ;; Bea's world has seen where Ada stopped.
        (is eq :occupied (nth-value 1 (host:player-edit-world-cell
                                       world ada (cell world 1050 1060 (1+ ground)) stone)))
        (true (pump (list session guest) :advance t
                    :until (lambda ()
                             (let ((stranger (first (terrain:simulation-strangers
                                                     (host:world-simulation guest-world)))))
                               (< (v:vec3-x (terrain:body-position stranger)) 1051.6)))))
        (is eq :occupied (nth-value 1 (host:player-edit-world-cell
                                       guest-world bea (cell guest-world 1051 1060 ground) stone)))
        (host:close-world-session guest)))))

(define-test a-remote-rider-sits-on-its-turtle
  ;; Bea rides the host's turtle from her world; the host seats her body
  ;; astride it, facing its way and not walking, in the avatar's Sit pose,
  ;; and stands her on her feet again when she gets off (#AVKN70).
  (with-host (world session network)
    (let ((turtle (first (host-turtles world))))
      (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
        (let* ((mount (replica-of guest-world world turtle))
               (character (terrain:make-walking-character
                           :position (host::animal-luft-position mount))))
          (setf (host:player-character bea) character)
          (terrain:add-simulation-character (host:world-simulation guest-world) character)
          (host::begin-world-ride guest-world bea mount)
          (flet ((rein ()
                   (luvcraft::urge-critter mount 1d0 0.4d0 1/60)
                   (setf (terrain:body-position (terrain:character-body character))
                         (host::animal-luft-position mount)))
                 (placement ()
                   (let ((rider (host:find-world-player world "bea")))
                     (host::remote-body-placement world rider
                                                  (host:track-shown (host:player-track rider)))))
                 (knee-reach ()
                   ;; How far forward of its feet the body the host draws reaches,
                   ;; in figure units: a sitter's knees, a stander's toes.
                   (let* ((rider (host:find-world-player world "bea"))
                          (avatar (make-instance 'host::remote-avatar
                                                 :session nil :x 0 :y 0 :z 0
                                                 :position (v:make-vec3 0 0 0)))
                          (mesh (luvcraft.agent::companion-mesh :avatar))
                          (palette (make-array (* 12 (luvcraft.agent::companion-mesh-bone-count mesh))
                                               :element-type 'single-float))
                          (skinned (make-array (* 12 (truncate (length (luvcraft.agent::companion-mesh-vertices mesh)) 9))
                                               :element-type 'single-float)))
                     (host::pose-remote-avatar world avatar rider
                                               (host:track-shown (host:player-track rider)) 1)
                     (luvcraft.agent::sample-agent-palette avatar mesh palette)
                     (luvcraft.agent::skin-companion-palette mesh palette skinned)
                     (loop for i from 2 below (length skinned) by 12 maximize (aref skinned i)))))
            (true (live-until (list world guest-world)
                              (lambda () (equal "bea" (host::turtle-rider-id world turtle)))
                              :each #'rein))
            (live (list world guest-world) 0.5 :each #'rein)
            (multiple-value-bind (seat facing walking-p) (placement)
              (let ((back (+ (luvcraft::critter-y turtle) (luvcraft::critter-height turtle))))
                ;; Hips on the shell: the soles sink below its back, not to the ground.
                (true (< (- back 1) (v:vec3-y seat) (- back 0.5)))
                (true (< (distance (list (v:vec3-x seat) (v:vec3-z seat))
                                   (list (luvcraft::critter-x turtle) (luvcraft::critter-z turtle)))
                         0.6))
                (true (< (abs (- facing (luvcraft::critter-yaw turtle))) 0.2))
                (is eq nil walking-p)))
            (let ((seated (knee-reach)))
              (host::end-world-ride guest-world bea mount)
              (true (live-until (list world guest-world)
                                (lambda () (null (host::turtle-rider-id world turtle)))))
              ;; Until the track shows her still, her last poses walk with the turtle.
              (live (list world guest-world) 0.5)
              (let ((standing (knee-reach)))
                ;; Knees forward over the shell, not an Idle stance's toes.
                (true (> seated (+ standing 0.25)))
                (true (< standing 0.3))))
            ;; Off the turtle, her body stands where her pose says.
            (let ((seat (placement))
                  (shown (host:track-shown (host:player-track (host:find-world-player world "bea")))))
              (is = (aref (host:pose-snapshot-position shown) 2) (v:vec3-y seat))))
          (host:close-world-session guest))))))

(defun free-port ()
  "A TCP port nothing listens on just now."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
         (progn (sb-bsd-sockets:socket-bind socket #(0 0 0 0) 0)
                (nth-value 1 (sb-bsd-sockets:socket-name socket)))
      (sb-bsd-sockets:socket-close socket))))

(define-test a-game-that-cannot-open-its-view-lets-go-of-its-session
  ;; As on a machine with no display: the canvas fails after the host has
  ;; opened its ports, or after a guest has joined.  Neither may stay open.
  (sb-int:encapsulate 'host::start-luvcraft-view 'no-display
                      (lambda (function &rest arguments)
                        (declare (ignore function arguments))
                        (error "SDL video initialization failed: no displays")))
  (unwind-protect
       (let ((port (free-port)))
         ;; The host's port is free again for the next try.
         (fail (host:play :host t :port port :seed 3 :pathname nil :player-id "ada" :name "Ada"))
         (let* ((world (session-test-world))
                (session (host:host-luvcraft-world world (host:find-world-player world "ada")
                                                   :port port)))
           (unwind-protect
                (progn
                  (true session)
                  ;; A guest that joined and could not show the world leaves it.
                  (let ((thread (sb-thread:make-thread
                                 (lambda ()
                                   (ignore-errors
                                    (host:play :join (format nil "localhost:~D" port)
                                               :player-id "bea" :name "Bea")))
                                 :name "joining a world with no display")))
                    (true (pump (list session) :seconds 15
                                :until (lambda ()
                                         (and (not (sb-thread:thread-alive-p thread))
                                              (null (host:find-world-player world "bea"))))))
                    (sb-thread:join-thread thread :default nil)))
             (host:close-world-session session))))
    (sb-int:unencapsulate 'host::start-luvcraft-view 'no-display)))
