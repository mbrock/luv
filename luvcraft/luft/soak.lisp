;;;; A soak (#3E8LY0): bots walk, dig, and build by replaying input tapes in a
;;;; host's world and a guest's, both headless, and afterwards the two must
;;;; agree on every journal event and every resident chunk.
;;;;
;;;; A bot plays once alone in a scratch world raised from the same recipe,
;;;; steering toward random waypoints near the spawn, jumping and tunnelling
;;;; when stuck, and every second or two digging the cell ahead, building
;;;; beside or in front of itself, or taking back its last edit.  What it did
;;;; is its tape: one input frame per tick, its edits and undos as actions.
;;;; The soak writes each tape, reads it back, and replays it, so the tape's
;;;; file format is part of what it checks.
;;;;
;;;; Replayed against a live session, the walk soon departs from the
;;;; recording -- a guest's edits show only once the host confirms them, and
;;;; the other bot builds in the way -- but the edits stay where they were
;;;; recorded.  Some are refused as stale or occupied; that is the point.
(in-package #:luvcraft.luft.game)

(defparameter *soak-blocks*
  '(:stone :planks :bricks :red-wool :yellow-wool :blue-wool :dirt :grass :crystal)
  "The blocks a soak bot builds with.")

;;; Worlds.

(defun soak-cell (world x y z)
  (luft:make-site (terrain:scene-domain (world-scene world)) x y z luft:+cell-extent+ 1))

(defun soak-solid-p (world x y z)
  (and (terrain:scene-material-placement-at (world-scene world) (soak-cell world x y z)) t))

(defun soak-ground (world x y)
  "One above the highest solid cell of column X, Y."
  (loop for z from (1- (game::luvcraft-world-height (world-block-world world))) downto 0
        when (soak-solid-p world x y z) return (1+ z)
        finally (return 0)))

(defun soak-chunk-keys (world radius)
  "The chunk keys within RADIUS chunks of WORLD's spawn."
  (multiple-value-bind (x y) (terrain:world-source-spawn (world-source world))
    (loop for dy from (- radius) to radius
          nconc (loop for dx from (- radius) to radius
                      collect (luft:chunk-key-at (+ x (* dx luft:+chunk-size+))
                                                 (+ y (* dy luft:+chunk-size+)))))))

(defun make-soak-resident (world keys)
  "Make KEYS resident in WORLD's scene, with collision, from its source and
edits, as streaming around a player would."
  (let* ((scene (world-scene world))
         (source (terrain:streaming-scene-source scene)))
    (dolist (key keys)
      (setf (gethash key (terrain:streaming-scene-store scene))
            (terrain:materialize-world-source-chunk
             source key 1 :edits (terrain:capture-world-source-chunk-edits source key))))
    (terrain:rebuild-authored-world-resident-values scene keys keys)
    world))

(defun make-soak-world (&key (seed 1) (relief :alpine) (height luvcraft::*little-world-height*)
                          (radius 1))
  "A headless world of SEED and RELIEF with the chunks within RADIUS of its
spawn resident."
  (let* ((block-world (luvcraft:make-empty-little-block-world
                       :seed seed :relief relief :chunk-height height))
         (world (make-luvcraft-world
                 :block-world block-world
                 :scene (game:make-luvcraft-streaming-scene
                         (game:luvcraft-world-luft-source block-world)))))
    (make-soak-resident world (soak-chunk-keys world radius))))

(defun soak-start (world index)
  "Where soak bot INDEX starts: beside the spawn, on the ground."
  (multiple-value-bind (x y) (terrain:world-source-spawn (world-source world))
    (let ((x (+ x (if (evenp index) 3 -3))) (y (+ y index)))
      (vec3:make-vec3 (+ x 0.5d0) (+ y 0.5d0) (float (soak-ground world x y) 1d0)))))

(defun add-soak-character (world player index)
  "Give PLAYER a walking character at soak bot INDEX's start in WORLD."
  (setf (player-character player)
        (terrain:make-walking-character :position (soak-start world index)))
  (add-world-player world player))

;;; Actions, as tapes carry them and a view records them: (:EDIT X Y Z
;;; BLOCK-DESCRIPTION), (:UNDO), and (:REDO).

(defun perform-tape-action (world player action)
  "Do ACTION as PLAYER in WORLD; return the edit or events and the status."
  (ecase (first action)
    (:edit
     (destructuring-bind (x y z description) (rest action)
       (player-edit-world-cell world player (soak-cell world x y z)
                               (luvcraft::restore-block-value description))))
    (:undo
     (if (world-session world)
         (session-undo (world-session world))
         (terrain:undo-world-events (world-scene world) (world-edit-actor player))))
    (:redo
     (let ((session (world-session world)))
       (if (typep session 'guest-session)
           (send-edit-request session nil :redo)
           (terrain:redo-world-events (world-scene world) (world-edit-actor player)))))))

(defun tape-replayer (world player tape &key (start (world-tick world)) (on-action (constantly nil)))
  "A BEFORE-TICK function steering PLAYER in WORLD by TAPE from tick START,
doing each action before its frame and calling ON-ACTION with it and what it
returned.  After the tape PLAYER stands still."
  (let ((still (terrain:make-input-frame)))
    (lambda (tick)
      (let ((index (- tick start)))
        (cond ((< -1 index (terrain:input-tape-length tape))
               (dolist (action (terrain:input-tape-actions-at tape index))
                 (funcall on-action action
                          (multiple-value-list (perform-tape-action world player action))))
               (steer-luvcraft-player player (terrain:input-tape-frame tape index)))
              ((= index (terrain:input-tape-length tape))
               (steer-luvcraft-player player still)))))))

(defun tape-finished-p (world tape start)
  (> (- (world-tick world) start) (terrain:input-tape-length tape)))

;;; The bot.

(defun wrap-angle (angle)
  (- angle (* 2 pi (round angle (* 2 pi)))))

(defun newest-block-of-another (world player)
  "The site of the newest block placed by someone other than PLAYER that is
still there, or NIL."
  (let ((events (terrain:world-journal-events (terrain:world-source-journal (world-source world)))))
    (loop for index from (1- (length events)) downto (max 0 (- (length events) 20))
          for event = (aref events index)
          when (and (terrain:world-event-after event)
                    (not (equal (terrain:world-event-actor event) (world-edit-actor player)))
                    (terrain:scene-material-placement-at (world-scene world)
                                                         (terrain:world-event-site event)))
            return (terrain:world-event-site event))))

(defun make-soak-bot (world player tape &key (seed 1) (radius 20))
  "A BEFORE-TICK function playing PLAYER in WORLD as a bot that wanders
within RADIUS cells of where it starts, recording itself on TAPE."
  (let* ((random (sb-ext:seed-random-state seed))
         (character (player-character player))
         (body (terrain:character-body character))
         (home (let ((p (terrain:body-position body))) (cons (vec3:vec3-x p) (vec3:vec3-y p))))
         (flag (multiple-value-bind (x y) (terrain:world-source-spawn (world-source world))
                 (list x (+ y 6) (soak-ground world x (+ y 6)))))
         (waypoint nil) (mark nil) (stuck 0) (ticks 0)
         (action-due (+ 60 (random 120 random))))
    (labels ((here () (terrain:body-position body))
             (choose-waypoint ()
               (setf waypoint (cons (+ (car home) (- (random (* 2d0 radius) random) radius))
                                    (+ (cdr home) (- (random (* 2d0 radius) random) radius)))
                     stuck 0))
             (edit (x y z block)
               (when (and (<= 1 z) (< z (1- (game::luvcraft-world-height (world-block-world world)))))
                 (let ((action (list :edit x y z (luvcraft::block-save-description block))))
                   (when (perform-tape-action world player action)
                     (terrain:record-input-action tape action)))))
             (act ()
               (let* ((p (here))
                      (yaw (terrain:character-look-yaw character))
                      (feet (floor (+ (vec3:vec3-z p) 0.01d0)))
                      (ahead-x (floor (+ (vec3:vec3-x p) (* 1.6d0 (cos yaw)))))
                      (ahead-y (floor (+ (vec3:vec3-y p) (* 1.6d0 (sin yaw)))))
                      (side (if (zerop (random 2 random)) 1 -1))
                      (side-x (floor (+ (vec3:vec3-x p) (* 2d0 (cos yaw)) (* side 2d0 (- (sin yaw))))))
                      (side-y (floor (+ (vec3:vec3-y p) (* 2d0 (sin yaw)) (* side 2d0 (cos yaw)))))
                      (block (luvcraft:block-kind-named
                              (elt *soak-blocks* (random (length *soak-blocks*) random))))
                      (roll (random 100 random)))
                 (cond
                   ;; Dig the cell ahead, or the ground under it.
                   ((< roll 35)
                    (if (soak-solid-p world ahead-x ahead-y feet)
                        (edit ahead-x ahead-y feet nil)
                        (edit ahead-x ahead-y (1- feet) nil)))
                   ;; Build up a column beside the way.
                   ((< roll 80)
                    (let ((z (loop for z from feet below (+ feet 4)
                                   unless (soak-solid-p world side-x side-y z) return z)))
                      (when z (edit side-x side-y z block))))
                   ;; Take down the newest block someone else placed:
                   ;; replayed, the two often race for it.
                   ((< roll 85)
                    (let ((theirs (newest-block-of-another world player)))
                      (when theirs
                        (edit (luft:site-x theirs) (luft:site-y theirs) (luft:site-z theirs) nil))))
                   ;; Wall the way, to dig through later.
                   ((< roll 92)
                    (unless (soak-solid-p world ahead-x ahead-y feet)
                      (edit ahead-x ahead-y feet block)))
                   (t
                    (when (perform-tape-action world player '(:undo))
                      (terrain:record-input-action tape '(:undo)))))))
             (tunnel ()
               ;; Stuck for a while: dig through what is ahead.
               (let* ((p (here))
                      (yaw (terrain:character-look-yaw character))
                      (feet (floor (+ (vec3:vec3-z p) 0.01d0)))
                      (x (floor (+ (vec3:vec3-x p) (* 1.2d0 (cos yaw)))))
                      (y (floor (+ (vec3:vec3-y p) (* 1.2d0 (sin yaw))))))
                 (dolist (z (list feet (1+ feet)))
                   (when (soak-solid-p world x y z) (edit x y z nil))))))
      (choose-waypoint)
      (lambda (tick)
        (declare (ignore tick))
        (incf ticks)
        (let* ((p (here))
               (x (vec3:vec3-x p)) (y (vec3:vec3-y p))
               (jump nil))
          (when (< (+ (expt (- (car waypoint) x) 2) (expt (- (cdr waypoint) y) 2)) 2.25d0)
            (choose-waypoint))
          (when (zerop (mod ticks 60))
            (if (and mark (< (+ (expt (- (car mark) x) 2) (expt (- (cdr mark) y) 2)) 0.25d0))
                (progn (incf stuck) (setf jump t))
                (setf stuck 0))
            (setf mark (cons x y))
            (when (= stuck 3) (tunnel))
            (when (> stuck 8) (choose-waypoint)))
          ;; Every ten seconds both bots claim the same flag cell with
          ;; their own blocks, on the same tick of their tapes: replayed,
          ;; the guest asks from a stale view and the host refuses it.
          (when (zerop (mod ticks 1200))
            (destructuring-bind (fx fy fz) flag
              (let ((mine (game:block-placement
                           (luvcraft:block-kind-named
                            (elt *soak-blocks* (mod seed (length *soak-blocks*)))))))
                (if (eq mine (terrain:scene-material-placement-at (world-scene world)
                                                                  (soak-cell world fx fy fz)))
                    (edit fx fy fz nil)
                    (edit fx fy fz (game::luvcraft-block-for-placement mine))))))
          (when (>= ticks action-due)
            (act)
            (setf action-due (+ ticks 90 (random 150 random))))
          (let* ((turn (wrap-angle (- (atan (- (cdr waypoint) y) (- (car waypoint) x))
                                      (terrain:character-look-yaw character))))
                 (frame (terrain:make-input-frame
                         :forward (if (< (abs turn) 0.7) 1.0 0.2)
                         ;; Small corrections wait, so the tape's runs stay long.
                         :yaw-delta (if (< (abs turn) 0.02)
                                        0.0
                                        (float (max -0.05 (min 0.05 turn)) 1.0))
                         :jump-p jump)))
            (terrain:record-input-frame tape frame)
            (steer-luvcraft-player player frame)))))))

(defun record-soak-tapes (ids seconds &key (seed 1) (relief :alpine) (radius 1) (bot-seed 7))
  "Record one tape per player id in IDS: bots playing SECONDS together in a
scratch world of SEED and RELIEF, alone but for each other."
  (let* ((world (make-soak-world :seed seed :relief relief :radius radius))
         (bots (loop for id in ids
                     for index from 0
                     for player = (add-soak-character
                                   world (make-luvcraft-player :id id :name id) index)
                     for tape = (terrain:make-input-tape)
                     collect (cons tape (make-soak-bot world player tape
                                                       :seed (+ bot-seed index))))))
    (dotimes (i (round (* seconds 60)))
      (advance-luvcraft-world world 1/60
                              :before-tick (lambda (tick)
                                             (dolist (bot bots) (funcall (cdr bot) tick)))))
    (mapcar #'car bots)))

(defun save-and-reload-tape (tape pathname)
  "TAPE as written to PATHNAME and read back."
  (terrain:read-input-tape (terrain:write-input-tape tape pathname)))

;;; Agreement.

(defun describe-journal-value (value)
  (luvcraft::block-save-description (values (journal-value-block value))))

(defun soak-journal-events (world from)
  "WORLD's journal events after sequence FROM as comparable lists:
sequence, actor, site, and the blocks before and after."
  (let ((journal (terrain:world-source-journal (world-source world))))
    (loop for event across (terrain:world-journal-events journal)
          when (> (terrain:world-event-sequence event) from)
            collect (list (terrain:world-event-sequence event)
                          (terrain:world-event-actor event)
                          (terrain:world-event-site event)
                          (describe-journal-value (terrain:world-event-before event))
                          (describe-journal-value (terrain:world-event-after event))))))

(defun soak-digest (form)
  (terrain::octets-hex
   (sb-md5:md5sum-sequence
    (terrain::text-octets (with-standard-io-syntax (prin1-to-string form))))))

(defun soak-world-summary (world keys from)
  "WORLD's journal head, a digest of its events after FROM, and the hash of
each of KEYS, (KEY . HEX)."
  (values (terrain:world-journal-head (terrain:world-source-journal (world-source world)))
          (soak-digest (soak-journal-events world from))
          (loop for key in keys
                collect (cons key (terrain:streaming-scene-chunk-hash (world-scene world) key)))))

(defun soak-divergences (host guest keys from)
  "How GUEST's world differs from HOST's after sequence FROM: the first
event that differs and every chunk of KEYS whose hash differs, with the
sequences of the events in it."
  (let* ((theirs (soak-journal-events host from))
         (ours (soak-journal-events guest from))
         (divergences nil))
    (loop for h = (pop theirs) for g = (pop ours)
          while (or h g)
          unless (equal h g)
            do (push (list :event (or (first h) (first g)) :host h :guest g) divergences)
               (return))
    (dolist (key keys)
      (let ((h (terrain:streaming-scene-chunk-hash (world-scene host) key))
            (g (terrain:streaming-scene-chunk-hash (world-scene guest) key)))
        (unless (equal h g)
          (push (list :chunk (list (luft:chunk-key-x key) (luft:chunk-key-y key))
                      :host h :guest g
                      :sequences (loop for event in (soak-journal-events host 0)
                                       when (= key (luft:site-chunk-key (third event)))
                                         collect (first event)))
                divergences))))
    (nreverse divergences)))

;;; Running.

(defun soak-log (control &rest arguments)
  (format t "~&;; soak: ~?~%" control arguments)
  (finish-output))

(defun tally (table key)
  (incf (getf (car table) key 0)))

(defun counting-actions (counts)
  "An ON-ACTION function counting each action's status in COUNTS, a cons."
  (lambda (action results)
    (declare (ignore action))
    (tally counts (or (second results) :done))))

(defun guest-settled-p (guest host-world)
  (and (= (length (session-replies guest)) (session-next-request guest))
       (= (session-applied guest)
          (terrain:world-journal-head (terrain:world-source-journal (world-source host-world))))
       (null (terrain:streaming-scene-pending-events (world-scene (session-world guest))))))

(defun pace (start rounds step)
  "Sleep until ROUNDS of STEP seconds have passed since START in wall time."
  (let ((ahead (- (+ start (* rounds step)) (wall-seconds))))
    (when (plusp ahead) (sleep ahead))))

(defun run-soak (&key (seconds 600) (seed 1) (relief :alpine) (radius 1) (bot-seed 7)
                   (transport :tcp) (paced t) (tape-directory #p"build/soak/")
                   (step 1/60) (progress-every 30))
  "Soak one host and one guest in this image for SECONDS of play, each a
bot replaying a recorded tape, over TRANSPORT, :TCP on localhost or
:LOOPBACK.  PACED runs at wall-clock speed.  Return a plist: whether they
agree, the divergences, and what the bots did."
  (soak-log "Recording ~Ds of bots in seed ~D (~(~A~))." seconds seed relief)
  (let* ((tapes (loop for tape in (record-soak-tapes '("soak-host" "soak-guest") seconds
                                                     :seed seed :relief relief :radius radius
                                                     :bot-seed bot-seed)
                      for name in '("host" "guest")
                      collect (save-and-reload-tape
                               tape (merge-pathnames (format nil "~A-~D.tape" name seed)
                                                     tape-directory))))
         (network (and (eq transport :loopback) (luvcraft.wire.net:make-loopback-network)))
         (world (make-soak-world :seed seed :relief relief :radius radius))
         (keys (soak-chunk-keys world radius))
         (host-player (add-soak-character
                       world (make-luvcraft-player :id "soak-host" :name "Host bot") 0))
         (session (host-luvcraft-world world host-player :port 0 :loopback network
                                                    :name "host"))
         (guest nil))
    (soak-log "Tapes of ~D and ~D ticks with ~D and ~D actions, in ~A."
              (terrain:input-tape-length (first tapes)) (terrain:input-tape-length (second tapes))
              (length (terrain:input-tape-actions (first tapes)))
              (length (terrain:input-tape-actions (second tapes)))
              tape-directory)
    (unwind-protect
         (let* ((target (if network "host" (format nil "localhost:~D" (net:endpoint-port
                                                                      (session-endpoint session)))))
                (joined (join-soak-guest session target network))
                (guest-world (first joined))
                (guest-player (second joined))
                (from (session-watermark (setf guest (third joined))))
                (host-counts (list nil)) (guest-counts (list nil))
                (host-start (world-tick world))
                (host-replay (tape-replayer world host-player (first tapes)
                                            :on-action (counting-actions host-counts)))
                (guest-replay nil) (guest-start nil)
                (start (wall-seconds)) (rounds 0))
           (make-soak-resident guest-world keys)
           (add-soak-character guest-world guest-player 1)
           (setf guest-start (world-tick guest-world)
                 guest-replay (tape-replayer guest-world guest-player (second tapes)
                                             :on-action (counting-actions guest-counts)))
           (soak-log "Guest joined at sequence ~D over ~(~A~); playing." from transport)
           (flet ((round-of-play (host-tick guest-tick)
                    (advance-luvcraft-world world step :before-tick host-tick)
                    (advance-luvcraft-world guest-world step :before-tick guest-tick)
                    (incf rounds)
                    (when paced (pace start rounds step))
                    (when (and progress-every (zerop (mod rounds (round progress-every step))))
                      (soak-log "~5,1Fs: host at ~D, guest at ~D, ~D chunk hashes matched, ~D differed."
                                (* rounds step) (session-journal-head session) (session-applied guest)
                                (session-matched-chunks session)
                                (+ (length (session-mismatched-chunks session))
                                   (length (session-mismatched-chunks guest)))))))
             (loop until (and (tape-finished-p world (first tapes) host-start)
                              (tape-finished-p guest-world (second tapes) guest-start))
                   do (round-of-play host-replay guest-replay))
             ;; Let everything in flight land.
             (let ((deadline (+ (wall-seconds) 30)))
               (loop until (or (guest-settled-p guest world) (> (wall-seconds) deadline))
                     do (round-of-play nil nil)
                        (unless paced (sleep 0.002)))))
           (let* ((divergences (soak-divergences world guest-world keys from))
                  (settled (guest-settled-p guest world))
                  (rejected (loop for reply in (session-replies guest)
                                  when (eq (wire:edit-reply-message-outcome reply) :rejected)
                                    collect (wire:edit-reply-message-reason reply)))
                  (result
                    (list :agree (and settled (null divergences)
                                      (null (session-mismatched-chunks session))
                                      (null (session-mismatched-chunks guest)))
                          :settled settled
                          :seconds (* rounds step)
                          :events (- (session-journal-head session) from)
                          :chunks (length keys)
                          :host-actions (car host-counts)
                          :guest-actions (car guest-counts)
                          :guest-rejected (let ((counts nil))
                                            (dolist (reason rejected counts)
                                              (incf (getf counts reason 0))))
                          :hashes-matched-live (+ (session-matched-chunks session)
                                                  (session-matched-chunks guest))
                          :hashes-differed-live (append (session-mismatched-chunks session)
                                                        (session-mismatched-chunks guest))
                          :divergences divergences)))
             (multiple-value-bind (head digest) (soak-world-summary world keys from)
               (soak-log "Journal head ~D, events after ~D digest ~A." head from digest))
             (soak-log "~:[DIVERGED~;Agreed~] after ~,1Fs: ~D events, ~D chunks; host bot ~S, ~
                        guest bot ~S, rejected ~S; ~D live hash comparisons."
                       (getf result :agree) (getf result :seconds) (getf result :events)
                       (length keys) (car host-counts) (car guest-counts)
                       (getf result :guest-rejected) (getf result :hashes-matched-live))
             (unless settled (soak-log "The guest never caught up with the host."))
             (dolist (divergence (append divergences (getf result :hashes-differed-live)))
               (soak-log "Divergence: ~S" divergence))
             result))
      (when guest (close-world-session guest))
      (close-world-session session))))

(defun join-soak-guest (session target network)
  "Join SESSION's world at TARGET as the guest bot from another thread
while pumping the host; return the guest's world, player, and session."
  (let* ((result nil)
         (thread (sb-thread:make-thread
                  (lambda ()
                    (setf result (handler-case
                                     (multiple-value-list
                                      (join-luvcraft-world target :loopback network
                                                                  :identity "soak-guest"
                                                                  :name "Guest bot"))
                                   (error (condition) condition))))
                  :name "soak guest joining")))
    (loop while (sb-thread:thread-alive-p thread)
          do (advance-world-session session 1/60)
             (sleep 0.005))
    (sb-thread:join-thread thread)
    (when (typep result 'error) (error result))
    result))

;;; Two machines.  The host and the guest each record their own bot alone,
;;; play over TCP, and print what they hold; peers compare every resident
;;; chunk's hash at equal journal points as they play.

(defun print-soak-summary (world keys from session)
  "Print WORLD's journal and chunk hashes, for diffing with the peer's."
  (multiple-value-bind (head digest hashes) (soak-world-summary world keys from)
    (format t "~&soak tag ~A~%" (terrain:world-source-tag (world-source world)))
    (format t "soak journal head ~D events-after ~D digest ~A~%" head from digest)
    (loop for (key . hex) in hashes
          do (format t "soak chunk ~D ~D ~A~%" (luft:chunk-key-x key) (luft:chunk-key-y key) hex))
    (format t "soak hashes matched ~D differed ~D~%"
            (session-matched-chunks session) (length (session-mismatched-chunks session)))
    (loop for (key ours theirs watermark) in (session-mismatched-chunks session)
          do (format t "soak DIVERGED chunk ~D ~D at sequence ~D: ours ~A, theirs ~A~%"
                     (luft:chunk-key-x key) (luft:chunk-key-y key) watermark ours theirs))
    (finish-output)
    (null (session-mismatched-chunks session))))

(defun call-paced (step function)
  "Call FUNCTION each STEP of wall time until it returns true."
  (let ((start (wall-seconds)))
    (loop for rounds from 1
          until (funcall function)
          do (pace start rounds step))))

(defun soak-tape (id seconds &rest options &key tape-directory seed &allow-other-keys)
  "Record bot ID alone for SECONDS, written to TAPE-DIRECTORY and read back."
  (save-and-reload-tape
   (first (apply #'record-soak-tapes (list id) seconds
                 (alexandria:remove-from-plist options :tape-directory)))
   (merge-pathnames (format nil "~A-~D.tape" id seed) tape-directory)))

(defun run-soak-host (&key (seconds 600) (seed 1) (relief :alpine) (radius 1) (bot-seed 7)
                        (port wire:+default-port+) (tape-directory #p"build/soak/") (step 1/60)
                        lobby)
  "Host a soak on PORT: wait for a guest, play the host bot's tape while
it plays, and summarize once it leaves.  LOBBY true lists the host in the
tailnet lobby, as a hosting view does.  Return whether no chunk differed."
  (let* ((tape (soak-tape "soak-host" seconds :seed seed :relief relief :radius radius
                                              :bot-seed bot-seed :tape-directory tape-directory))
         (world (make-soak-world :seed seed :relief relief :radius radius))
         (keys (soak-chunk-keys world radius))
         (player (add-soak-character world (make-luvcraft-player :id "soak-host" :name "Host bot") 0))
         (session (host-luvcraft-world world player :port port))
         (*chunk-hashes-per-report* (length keys))
         (client (and lobby (luv.lobby:make-lobby-client :name "Soak host")))
         (advert-due 0d0) (listed nil)
         (replay nil) (start nil) (seen nil))
    (soak-log "Hosting seed ~D (~(~A~)), tag ~A, on port ~D; waiting for a guest."
              seed relief (terrain:world-source-tag (world-source world))
              (net:endpoint-port (session-endpoint session)))
    (when client (luv.lobby:start-lobby-client client))
    (unwind-protect
         (progn
           (call-paced
            step (lambda ()
                   (advance-luvcraft-world world step :before-tick replay)
                   (when (and client (> (wall-seconds) advert-due))
                     (setf advert-due (+ (wall-seconds) *advert-period*))
                     (let ((address (luv.lobby:lobby-client-local-address client))
                           (key (concatenate 'string *session-advert-prefix*
                                             (luv.lobby:lobby-client-id client))))
                       (when (and address (not listed))
                         (setf listed t)
                         (soak-log "Listed in the lobby at ~A:~D." address
                                   (net:endpoint-port (session-endpoint session))))
                       (setf (luv.lobby:lobby-client-own-value client key)
                             (and address (session-advert session address)))))
                   (when (and (null replay) (session-guests session))
                     (setf seen t start (world-tick world)
                           replay (tape-replayer world player tape))
                     (soak-log "A guest joined; the host bot plays."))
                   ;; A guest whose link dropped has its place kept a while.
                   (and seen (null (session-guests session)) (null (session-away session))
                        (tape-finished-p world tape start))))
           (soak-log "The guest left at sequence ~D." (session-journal-head session))
           (print-soak-summary world keys 0 session))
      (when client (luv.lobby:stop-lobby-client client))
      (close-world-session session))))

(defun find-soak-host-in-lobby (&key (timeout 90))
  "The \"address:port\" a soak host lists in the tailnet lobby."
  (let ((client (luv.lobby:make-lobby-client :name "Soak guest"))
        (deadline (+ (wall-seconds) timeout)))
    (luv.lobby:start-lobby-client client)
    (unwind-protect
         (loop
           (let ((present (mapcar #'luv.lobby:lobby-peer-id
                                  (luv.lobby:lobby-snapshot-peers
                                   (luv.lobby:lobby-client-snapshot client)))))
             (loop for (key . text) in (luv.lobby:lobby-client-values-under
                                        client *session-advert-prefix*)
                   for advert = (parse-session-advert text)
                   when (and advert (string= (getf advert :host) "Host bot")
                             (member (subseq key (length *session-advert-prefix*)) present
                                     :test #'string=))
                     do (let ((target (format nil "~A:~D" (getf advert :address) (getf advert :port))))
                          (soak-log "The lobby lists the soak host at ~A." target)
                          (return-from find-soak-host-in-lobby target))))
           (when (> (wall-seconds) deadline)
             (error "No soak host in the lobby within ~D seconds." timeout))
           (sleep 1/4))
      (luv.lobby:stop-lobby-client client))))

(defun run-soak-guest (target &key (seconds 600) (seed 1) (relief :alpine) (radius 1) (bot-seed 7)
                                (tape-directory #p"build/soak/") (step 1/60))
  "Join the soak host at TARGET, play the guest bot's tape, settle, and
wait for one comparison of every resident chunk with the host's before
leaving.  Return whether no chunk differed."
  (let ((tape (soak-tape "soak-guest" seconds :seed seed :relief relief :radius radius
                                              :bot-seed (1+ bot-seed) :tape-directory tape-directory))
        (target (if (string-equal target "lobby") (find-soak-host-in-lobby) target)))
    (destructuring-bind (world player session pose)
        (multiple-value-list (join-luvcraft-world target :identity "soak-guest" :name "Guest bot"))
      (declare (ignore pose))
      (unwind-protect
           (let* ((keys (soak-chunk-keys world radius))
                  (*chunk-hashes-per-report* (length keys))
                  (start nil) (replay nil) (counts (list nil)))
             (unless (equal (getf (cddr (terrain:world-source-recipe (world-source world))) :seed) seed)
               (error "The host plays ~S; pass its --seed and --relief."
                      (terrain:world-source-recipe (world-source world))))
             (make-soak-resident world keys)
             (add-soak-character world player 1)
             (setf start (world-tick world)
                   replay (tape-replayer world player tape :on-action (counting-actions counts)))
             (soak-log "Joined ~A at sequence ~D; the guest bot plays." target (session-watermark session))
             (call-paced step (lambda ()
                                (advance-luvcraft-world world step :before-tick replay)
                                ;; A dropped link reconnects meanwhile.
                                (or (tape-finished-p world tape start)
                                    (eq (session-state session) :closed))))
             (soak-log "Tape done: ~S.  Waiting for the host's events and a whole chunk comparison."
                       (car counts))
             (let ((deadline (+ (wall-seconds) 90))
                   (matched nil) (head nil) (since 0d0))
               (call-paced
                step (lambda ()
                       (advance-luvcraft-world world step)
                       (let ((now (session-watermark-now session)))
                         ;; Count matches from the first report after the
                         ;; journal has rested three seconds.
                         (unless (and now (eql now head))
                           (setf head now since (wall-seconds) matched nil))
                         (when (and head (null matched) (> (wall-seconds) (+ since 3)))
                           (setf matched (session-matched-chunks session))))
                       (or (and matched
                                (>= (session-matched-chunks session) (+ matched (length keys))))
                           (session-mismatched-chunks session)
                           (eq (session-state session) :closed)
                           (> (wall-seconds) deadline)))))
             (soak-log "Rejected requests: ~S."
                       (let ((reasons nil))
                         (dolist (reply (session-replies session) reasons)
                           (when (eq (wire:edit-reply-message-outcome reply) :rejected)
                             (incf (getf reasons (wire:edit-reply-message-reason reply) 0))))))
             (print-soak-summary world keys (session-watermark session) session))
        (close-world-session session)))))

;;; The command line: scripts/luvcraft-soak.

(defun soak-main (arguments)
  "Run a soak as ARGUMENTS say and exit 0 if the peers agreed."
  (let ((options nil) (mode :both) (target nil) (transport :tcp))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (flet ((value () (or (pop arguments) (error "~A wants a value." argument)))
                      (number (string) (parse-integer string)))
                 (cond
                   ((string= argument "--minutes") (setf (getf options :seconds) (* 60 (number (value)))))
                   ((string= argument "--seconds") (setf (getf options :seconds) (number (value))))
                   ((string= argument "--seed") (setf (getf options :seed) (number (value))))
                   ((string= argument "--bot-seed") (setf (getf options :bot-seed) (number (value))))
                   ((string= argument "--radius") (setf (getf options :radius) (number (value))))
                   ((string= argument "--relief")
                    (setf (getf options :relief) (intern (string-upcase (value)) '#:keyword)))
                   ((string= argument "--tapes") (setf (getf options :tape-directory)
                                                       (uiop:ensure-directory-pathname (value))))
                   ((string= argument "--host") (setf mode :host))
                   ((string= argument "--port") (setf (getf options :port) (number (value))))
                   ((string= argument "--join") (setf mode :guest target (value)))
                   ((string= argument "--loopback") (setf transport :loopback))
                   ((string= argument "--lobby") (setf (getf options :lobby) t))
                   (t (error "Unknown argument ~A." argument))))))
    (let ((agreed (ecase mode
                    (:both (getf (apply #'run-soak :transport transport
                                        (alexandria:remove-from-plist options :port :lobby))
                                 :agree))
                    (:host (apply #'run-soak-host options))
                    (:guest (apply #'run-soak-guest target
                                   (alexandria:remove-from-plist options :port :lobby))))))
      (soak-log (if agreed "The peers agree." "The peers DIVERGED."))
      (uiop:quit (if agreed 0 1)))))
