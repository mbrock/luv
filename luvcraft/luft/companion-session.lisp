;;;; Companions shared by every player (#EA88ON).
;;;;
;;;; The host keeps the companions: their providers, their tools, and their
;;;; bodies, which walk in its world.  Every guest sees a copy of each, a
;;;; mirror inhabitant, which its host's COMPANION-MESSAGEs keep up to date
;;;; whenever what it was asked or said, the player it attends to, or its
;;;; journey changes, and whose body follows COMPANION-POSE-MESSAGEs at the
;;;; players' 30 Hz through a remote track.  A guest who talks to one sends
;;;; a COMPANION-TELL-MESSAGE, which the host tells its companion as from
;;;; that guest; both then see the same reply.  A companion's edits go
;;;; through the host's sequencer as any edit does, with the companion's
;;;; stable id as their actor.
(in-package #:luvcraft.luft.game)

(defparameter *companion-said-limit* 8000
  "The most characters of a companion's line that travel.")

(defun companion-wire-pose (session inhabitant)
  "INHABITANT's body as a wire pose: a companion walks as a player does."
  (let* ((character (inhabitant-character inhabitant))
         (body (terrain:character-body character))
         (p (terrain:body-position body))
         (v (terrain:body-velocity body)))
    (wire:make-pose
     :tick (world-tick (session-world session))
     :position (vector (vec3:vec3-x p) (vec3:vec3-y p) (vec3:vec3-z p))
     :velocity (vector (vec3:vec3-x v) (vec3:vec3-y v) (vec3:vec3-z v))
     :yaw (wire:wrap-yaw (atan (terrain:character-heading-y character)
                               (terrain:character-heading-x character)))
     :gait (coerce (terrain:character-gait character) 'double-float))))

(defun companion-wire (inhabitant &key (presence :present) (pose (wire:make-pose)))
  "INHABITANT as a wire companion record."
  (multiple-value-bind (activity tool) (inhabitant-activity inhabitant)
    (let ((said (or (inhabitant-said inhabitant) ""))
          (destination (inhabitant-destination inhabitant)))
      (wire:make-companion-record
       :id (inhabitant-id inhabitant) :kind (inhabitant-kind inhabitant) :presence presence
       :asked (or (inhabitant-asked inhabitant) "")
       :said (subseq said 0 (min (length said) *companion-said-limit*))
       :said-serial (ldb (byte 32 0) (inhabitant-said-serial inhabitant))
       :attending (or (inhabitant-attending inhabitant) "")
       :activity activity :tool (or tool "")
       :destination (and destination (site-wire destination))
       :pose pose :owner (or (inhabitant-owner inhabitant) "")))))

(defun companion-attention-position (world inhabitant)
  "Where the player INHABITANT attends to stands in WORLD, or NIL."
  (let ((player (and (inhabitant-attending inhabitant)
                     (find-world-player world (inhabitant-attending inhabitant)))))
    (when player
      (if (player-track player)
          (let ((shown (track-shown (player-track player))))
            (and shown (let ((p (pose-snapshot-position shown)))
                         (vec3:make-vec3 (coerce (svref p 0) 'single-float)
                                         (coerce (svref p 1) 'single-float)
                                         (coerce (svref p 2) 'single-float)))))
          (and (player-character player)
               (terrain:body-position (terrain:character-body (player-character player))))))))

(defun host-companions (session)
  "The companions SESSION's world keeps itself."
  (remove-if #'inhabitant-mirror-p (world-agents (session-world session))))

;;; The host.

(defun welcome-companions (session peer)
  "Show the guest on PEER every companion as it is now."
  (dolist (inhabitant (host-companions session))
    (net:send-message (session-endpoint session)
                      (wire:make-companion-message
                       :companion (companion-wire inhabitant
                                                  :pose (companion-wire-pose session inhabitant)))
                      peer)))

(defun broadcast-companion-changes (session)
  "Tell every guest about each companion whose words, attention, or journey
changed since they were last told, and each that has gone."
  (let ((sent (session-companions-sent session))
        (here (host-companions session)))
    (dolist (inhabitant here)
      ;; The pose travels on its own; only the rest makes news.
      (let ((still (companion-wire inhabitant)))
        (unless (equalp still (gethash (inhabitant-id inhabitant) sent))
          (setf (gethash (inhabitant-id inhabitant) sent) still)
          (session-broadcast
           session (wire:make-companion-message
                    :companion (companion-wire inhabitant
                                               :pose (companion-wire-pose session inhabitant)))))))
    (dolist (id (loop for id being the hash-keys of sent
                      unless (find id here :key #'inhabitant-id :test #'equal)
                        collect id))
      (let ((gone (gethash id sent)))
        (remhash id sent)
        (setf (wire:companion-record-presence gone) :left)
        (session-broadcast session (wire:make-companion-message :companion gone))))))

(defgeneric broadcast-companion-poses (session)
  (:documentation "Send peers the bodies of the companions SESSION's world keeps.")
  (:method ((session world-session)) nil)
  (:method ((session host-session))
    (when (session-guests session)
      (dolist (inhabitant (host-companions session))
        (session-broadcast session
                           (wire:make-companion-pose-message
                            :companion (inhabitant-id inhabitant)
                            :pose (companion-wire-pose session inhabitant)))))))

(defmethod handle-session-message ((session host-session) peer (message wire:companion-tell-message))
  (let* ((player (guest-peer-player session peer))
         (world (session-world session))
         (id (wire:companion-tell-message-companion message))
         (inhabitant (find-world-inhabitant world id)))
    (when player
      (handler-case
          (progn
            (unless (and inhabitant (not (inhabitant-mirror-p inhabitant)))
              (error "No companion ~A is here." id))
            (session-log session "~A tells ~A: ~A" (player-name player) id
                         (wire:companion-tell-message-text message))
            ;; Its tools run in the host's view, or in the world itself.
            (tell-inhabitant (or (first (world-views world)) world) inhabitant
                             (wire:companion-tell-message-text message)
                             :asker (player-id player)))
        (error (condition)
          (net:send-message (session-endpoint session)
                            (wire:make-chat-message :speaker id
                                                    :text (princ-to-string condition))
                            peer))))))

;;; The guest.

(defun ask-host-companion (application inhabitant text)
  "Ask the host to tell its companion INHABITANT TEXT, from APPLICATION's world."
  (let* ((world (if (typep application 'luvcraft-world) application (viewer-world application)))
         (session (world-session world)))
    (unless (and (typep session 'guest-session) (eq (session-state session) :open))
      (error "The host is not connected; its companion cannot hear you."))
    (setf (inhabitant-asked inhabitant) text
          (inhabitant-attending inhabitant) (player-id (session-local-player session))
          (inhabitant-mirrored-activity inhabitant) :opening)
    (net:send-message (session-endpoint session)
                      (wire:make-companion-tell-message :companion (inhabitant-id inhabitant)
                                                        :text text)
                      (session-peer session))
    inhabitant))

(defun pose-wire-position (pose)
  (let ((p (wire:pose-position pose)))
    (vec3:make-vec3 (coerce (aref p 0) 'single-float) (coerce (aref p 1) 'single-float)
                    (coerce (aref p 2) 'single-float))))

(defun mirror-companion (session record)
  "A guest's copy of the host's companion RECORD describes, now in its world."
  (let* ((world (session-world session))
         (pose (wire:companion-record-pose record))
         (inhabitant (make-inhabitant world (wire:companion-record-kind record)
                                      (pose-wire-position pose)
                                      :id (wire:companion-record-id record))))
    (setf (inhabitant-track inhabitant) (make-remote-track))
    (push-pose-snapshot (inhabitant-track inhabitant) (pose-wire-snapshot session pose)
                        (wall-seconds))
    (setf (world-agents world) (append (world-agents world) (list inhabitant)))
    inhabitant))

(defun update-mirror-companion (session inhabitant record &key joining)
  "Bring the guest's copy INHABITANT to what the host's RECORD says.  What
it said before JOINING is history, not a fresh subtitle."
  (flet ((blank-nil (string) (if (string= string "") nil string)))
    (setf (inhabitant-owner inhabitant) (blank-nil (wire:companion-record-owner record))
          (inhabitant-asked inhabitant) (blank-nil (wire:companion-record-asked record))
          (inhabitant-attending inhabitant) (blank-nil (wire:companion-record-attending record))
          (inhabitant-mirrored-activity inhabitant) (wire:companion-record-activity record)
          (inhabitant-mirrored-tool inhabitant) (blank-nil (wire:companion-record-tool record))
          (inhabitant-mirrored-destination inhabitant)
          (let ((site (wire:companion-record-destination record)))
            (and site (wire-cell (session-world session) site))))
    (unless (= (wire:companion-record-said-serial record) (inhabitant-said-serial inhabitant))
      (setf (inhabitant-said inhabitant) (copy-seq (wire:companion-record-said record))
            (inhabitant-said-serial inhabitant) (wire:companion-record-said-serial record))
      (when joining
        (setf (inhabitant-notified inhabitant) (inhabitant-said inhabitant))))
    inhabitant))

(defun forget-mirror-companion (session inhabitant)
  (let ((world (session-world session))
        (presentation (inhabitant-presentation inhabitant)))
    (setf (world-agents world) (remove inhabitant (world-agents world)))
    (when presentation
      (render:remove-viewer-overlay (luvcraft.agent::gnome-session presentation) presentation))))

(defmethod handle-session-message ((session guest-session) peer (message wire:companion-message))
  (declare (ignore peer))
  (let* ((record (wire:companion-message-companion message))
         (inhabitant (find-world-inhabitant (session-world session)
                                            (wire:companion-record-id record))))
    (ecase (wire:companion-record-presence record)
      (:present
       (if inhabitant
           (update-mirror-companion session inhabitant record)
           (address-summoned-companion
            session (update-mirror-companion session (mirror-companion session record) record
                                             :joining t))))
      (:left
       (when inhabitant (forget-mirror-companion session inhabitant))))))

(defmethod handle-session-message ((session guest-session) peer (message wire:companion-pose-message))
  (declare (ignore peer))
  (let ((inhabitant (find-world-inhabitant (session-world session)
                                           (wire:companion-pose-message-companion message))))
    (when (and inhabitant (inhabitant-mirror-p inhabitant))
      (push-pose-snapshot (inhabitant-track inhabitant)
                          (pose-wire-snapshot session (wire:companion-pose-message-pose message))
                          (wall-seconds)))))

(defun advance-companion-mirror (inhabitant seconds)
  "Move a guest's copy of a companion along its track by SECONDS."
  (let ((track (inhabitant-track inhabitant))
        (character (inhabitant-character inhabitant)))
    (advance-remote-track track (wall-seconds) seconds)
    (let ((shown (track-shown track)))
      (when shown
        (let ((p (pose-snapshot-position shown))
              (yaw (pose-snapshot-yaw shown)))
          (setf (terrain:body-position (terrain:character-body character))
                (vec3:make-vec3 (coerce (svref p 0) 'single-float)
                                (coerce (svref p 1) 'single-float)
                                (coerce (svref p 2) 'single-float))
                (terrain:character-heading-x character) (coerce (cos yaw) 'single-float)
                (terrain:character-heading-y character) (coerce (sin yaw) 'single-float)))))))
