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
ADVANCE also steps each session's world's simulation by STEP."
  (let ((deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (dolist (session sessions)
        (when advance
          (terrain:advance-world-simulation
           (host:world-simulation (host:session-world session)) step))
        (host:advance-world-session session step))
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
  (lambda ()
    (= (host:session-applied guest)
       (terrain:world-journal-head (terrain:world-source-journal (host:world-source host-world))))))

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
          ;; Drawn a little behind where she walks, never ahead of her.
          (true shown)
          (true (< (- (v:vec3-x body) 3) (aref (host:pose-snapshot-position shown) 0)
                   (+ (v:vec3-x body) 0.5))))
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
