;;;; A dropped link is not a goodbye (#3JTHM3).
;;;;
;;;; When a guest's link drops without a bye, its host keeps the guest's
;;;; place -- its body where it was last seen, its rides, its record, the
;;;; terrain around it -- for *RECONNECT-GRACE* seconds.  The guest keeps its
;;;; world and dials again, saying hello with the host's epoch and the last
;;;; sequence its journal holds.  The host answers with a welcome at that
;;;; very watermark, no edit set, and the events after it: the catch-up
;;;; handoff of #2SGDU2 again, from the guest's own point, so no event is
;;;; lost or recorded twice.  A host that restarted has another epoch and
;;;; says so; resuming across a restart is not attempted.
(in-package #:luvcraft.luft.game)

(defparameter *reconnect-grace* 30
  "Seconds a host keeps the place of a guest whose link dropped.")

(defparameter *reconnect-patience* 60
  "Seconds a guest keeps dialling a host it lost before giving up.")

(defparameter *reconnect-interval* 1
  "Seconds between a guest's attempts to reach its host again.")

(defparameter *resume-wait* 5
  "Seconds a guest waits for the welcome that resumes it, once connected.")

(defgeneric lose-link (session peer detail)
  (:documentation "PEER's link to SESSION dropped, for DETAIL, without a goodbye."))

(defgeneric advance-session-link (session)
  (:documentation "At a frame boundary: let go of guests away too long, or
dial a lost host again.")
  (:method ((session world-session)) nil))

(defun journal-events-after (session from)
  "SESSION's journal events after sequence FROM, as wire events."
  (let* ((journal (session-journal session))
         (events (terrain:world-journal-events journal)))
    (loop for index from (- from (terrain:world-journal-watermark journal)) below (length events)
          collect (event-wire session (aref events index)))))

;;; The host.

(defmethod lose-link ((session host-session) peer detail)
  (let ((player (guest-peer-player session peer)))
    (when player
      (setf (session-guests session) (remove peer (session-guests session) :key #'car))
      (push (cons player (+ (wall-seconds) *reconnect-grace*)) (session-away session))
      (session-log session "Lost ~A's link (~A); keeping its place for ~D seconds."
                   (player-name player) detail *reconnect-grace*))))

(defmethod advance-session-link ((session host-session))
  (let ((now (wall-seconds)))
    (loop for (player . deadline) in (session-away session)
          when (> now deadline) do (depart-player session player :away-too-long))))

(defun refuse-hello (session peer reason text)
  (session-log session "Turned a guest away: ~A" text)
  (net:send-message (session-endpoint session) (wire:make-bye-message :reason reason :text text) peer)
  (net:close-peer (session-endpoint session) peer)
  nil)

(defun resume-guest (session peer hello)
  "Welcome back the guest saying HELLO on PEER from the sequence it resumes."
  (let* ((endpoint (session-endpoint session))
         (world (session-world session))
         (identity (wire:hello-message-identity hello))
         (name (wire:hello-message-name hello))
         (from (wire:hello-message-resume hello))
         (journal (session-journal session))
         (player (find-world-player world identity)))
    (cond
      ((/= (wire:hello-message-epoch hello) (session-epoch session))
       (refuse-hello session peer :cannot-resume
                     "This host has restarted since you joined; join again."))
      ((not (<= (terrain:world-journal-watermark journal) from (session-journal-head session)))
       (refuse-hello session peer :cannot-resume
                     (format nil "This host no longer has events after ~D; join again." from)))
      ((and player (not (player-track player)))
       (refuse-hello session peer :identity-taken "That player is the host."))
      (t
       ;; A link the host has not yet seen drop is this guest's old one.
       (let ((old (car (rassoc player (session-guests session)))))
         (when old
           (setf (session-guests session) (remove old (session-guests session) :key #'car))
           (net:close-peer endpoint old)))
       (setf (session-away session) (remove player (session-away session) :key #'car))
       ;; Everyone else has every event up to the head; this guest gets the
       ;; ones after its own point, on the stream its welcome opens.
       (broadcast-journal session)
       (let ((kept player))
         (let* ((player (or player (admit-player-record
                                    session (ensure-remote-player session identity name))))
                (pose (or (player-last-pose session player) (spawn-pose session identity)))
                (events (journal-events-after session from)))
           (net:send-message
            endpoint
            (wire:make-welcome-message
             :epoch (session-epoch session)
             :recipe (world-wire-recipe (world-source world))
             :vocabulary (map 'list #'symbol-name (luvcraft:material-vocabulary-table))
             :watermark from
             :entities (world-entity-snapshot world)
             :player (player-wire-record session player pose))
            peer)
           (when events
             (net:send-message endpoint (wire:make-events-message :events events) peer))
           (dolist (other (world-players world))
             (unless (eq other player)
               (net:send-message endpoint
                                 (wire:make-player-update-message
                                  :presence :present
                                  :player (player-wire-record session other
                                                              (player-last-pose session other)))
                                 peer)))
           ;; Others saw a guest whose place was kept stand still; one away
           ;; too long left, and now joins again.
           (unless kept
             (dolist (guest (session-guests session))
               (net:send-message endpoint (wire:make-player-update-message
                                           :presence :joined
                                           :player (player-wire-record session player pose))
                                 (car guest))))
           (push (cons peer player) (session-guests session))
           (welcome-companions session peer)
           (session-log session "~A (~A) resumed at sequence ~D, ~D event~:P behind."
                        name identity from (length events))
           player))))))

(defmethod handle-session-message :around ((session host-session) peer (message wire:hello-message))
  (let ((player (find-world-player (session-world session) (wire:hello-message-identity message))))
    (cond ((guest-peer-player session peer) (call-next-method))
          ((wire:hello-message-resume message) (resume-guest session peer message))
          (t
           ;; A guest that comes back without resuming -- its image
           ;; restarted -- takes the place kept for it as a fresh join.
           (when (and player (assoc player (session-away session)))
             (depart-player session player :rejoined))
           (call-next-method)))))

;;; The guest.

(defstruct reconnection
  "A guest's way back to its host."
  (deadline 0d0) (due 0d0) (waiting 0d0) (tries 0)
  (attempt nil) (results (sb-concurrency:make-mailbox :name "luvcraft reconnection")))

(defmethod lose-link ((session guest-session) peer detail)
  (declare (ignore peer))
  (unless (eq (session-state session) :closed)
    (let ((now (wall-seconds)))
      (net:stop-endpoint (session-endpoint session))
      (unless (session-link session)
        (setf (session-link session)
              (make-reconnection :deadline (+ now *reconnect-patience*) :due now))
        (session-log session "Lost the host (~A); reconnecting from sequence ~D."
                     detail (session-applied session)))
      ;; The first try goes at once; a failed one waits its turn.
      (setf (reconnection-due (session-link session))
            (+ now (if (eq (session-state session) :open) 0 *reconnect-interval*))
            (session-state session) :reconnecting))))

(defun dial-host (session link)
  "Connect to SESSION's host again on a worker and say hello, resuming."
  (let* ((old (session-endpoint session))
         (target (net::endpoint-target old))
         (name (net:endpoint-name old))
         (transport (net::endpoint-transport old))
         (loopback (and (typep transport 'net::loopback-transport)
                        (net::loopback-transport-network transport)))
         (datagrams (net::endpoint-datagrams old))
         (hello (wire:make-hello-message :identity (player-id (session-local-player session))
                                         :name (player-name (session-local-player session))
                                         :resume (session-applied session)
                                         :epoch (session-epoch session)))
         (results (reconnection-results link)))
    (incf (reconnection-tries link))
    (setf (reconnection-attempt link)
          (sb-thread:make-thread
           (lambda ()
             (sb-concurrency:send-message
              results
              (handler-case
                  (let ((endpoint (net:start-guest target :name name :loopback loopback
                                                          :datagrams datagrams)))
                    (handler-bind ((error (lambda (condition)
                                            (declare (ignore condition))
                                            (net:stop-endpoint endpoint))))
                      (net:send-message endpoint hello))
                    endpoint)
                (error (condition) condition))))
           :name "luvcraft reconnecting"))))

(defmethod advance-session-link ((session guest-session))
  (let ((link (session-link session))
        (now (wall-seconds)))
    (when link
      (let ((result (sb-concurrency:receive-message-no-hang (reconnection-results link))))
        (when result
          (setf (reconnection-attempt link) nil)
          (cond ((not (typep result 'net:endpoint))
                 (setf (reconnection-due link) (+ now *reconnect-interval*)))
                ((eq (session-state session) :reconnecting)
                 (setf (session-endpoint session) result
                       (session-state session) :resuming
                       (reconnection-waiting link) (+ now *resume-wait*)))
                (t (net:stop-endpoint result)))))
      (case (session-state session)
        (:reconnecting
         (cond ((reconnection-attempt link))
               ((> now (reconnection-deadline link))
                (setf (session-state session) :closed)
                (session-log session "Gave up on the host after ~D tr~:@P." (reconnection-tries link)))
               ((>= now (reconnection-due link))
                (dial-host session link))))
        (:resuming
         (when (> now (reconnection-waiting link))
           (lose-link session nil :no-welcome)))))))

(defmethod handle-session-message ((session guest-session) peer (message wire:welcome-message))
  (let ((world (session-world session)))
    (cond
      ((not (and (eq (session-state session) :resuming)
                 (= (wire:welcome-message-epoch message) (session-epoch session))
                 (= (wire:welcome-message-watermark message) (session-applied session))
                 (string= (terrain:world-source-tag (world-source world))
                          (tag-hex (wire:message-world-tag message)))))
       (session-log session "The host's welcome does not resume this world; leaving.")
       (setf (session-state session) :closed
             (session-link session) nil))
      (t
       (setf (session-peer session) peer
             (session-state session) :open
             (session-link session) nil)
       ;; Who is here now is what the host says next.
       (dolist (player (session-remote-players session))
         (forget-remote-player session (player-id player)))
       (take-entity-snapshot world (wire:welcome-message-entities message))
       ;; This player's record is its own, kept here; the host hears it again.
       (send-player-record session :force t)
       (session-log session "Resumed at sequence ~D." (session-applied session))))))

(defun abandon-reconnection (session)
  "Stop dialling SESSION's host, and let go of what an attempt connected."
  (let ((link (and (typep session 'guest-session) (session-link session))))
    (when link
      (setf (session-link session) nil)
      (let ((attempt (reconnection-attempt link)))
        (when attempt (sb-thread:join-thread attempt :default nil :timeout 8)))
      (loop for result = (sb-concurrency:receive-message-no-hang (reconnection-results link))
            while result
            when (typep result 'net:endpoint) do (net:stop-endpoint result)))))
