;;;; The players table across a session (#Y3S82T): a guest finds its stock
;;;; where it left it.
;;;;
;;;; A guest owns its inventory, hotbar, held block, and creative mode, and
;;;; tells its host its record whenever any of them changes, and as it
;;;; leaves.  The host keeps the record of every player who has been: a
;;;; guest who leaves leaves its record, posed where it was last seen, in the
;;;; world's absent records, which the host's save writes as its players
;;;; table.  A guest who joins again gets that record in the welcome.  The
;;;; guest's own saves are never touched, and nothing of the remote world is
;;;; kept on the guest's machine.
;;;;
;;;; On the wire a record is a WIRE:PLAYER-RECORD whose blocks are the
;;;; host's vocabulary offsets, as events' are.
(in-package #:luvcraft.luft.game)

(defparameter *player-record-period* 1/2
  "Seconds between a guest's checks for a change in its own record.")

(defun session-offsets (session)
  "Material name -> the offset that names it on SESSION's wire, or NIL for this image's own."
  (and (typep session 'guest-session) (session-host-offsets session)))

(defun record-wire-pose (source record tick)
  "RECORD's pose in SOURCE as a wire pose, or NIL when it has none."
  (multiple-value-bind (position yaw pitch flying) (game:luvcraft-resume-luft-pose source record)
    (and position
         (wire:make-pose :tick tick
                         :position (map 'vector (lambda (x) (coerce x 'double-float)) position)
                         :yaw (wire:wrap-yaw (coerce yaw 'double-float))
                         :pitch (max (- (/ pi 2)) (min (/ pi 2) (coerce pitch 'double-float)))
                         :flying (and flying t)))))

(defun record-wire (session record &optional pose)
  "Player RECORD as a wire player record, posed at wire POSE or at its own."
  (let ((offsets (session-offsets session))
        (world (session-world session)))
    (flet ((wire-block (block)
             ;; A block the far side cannot name stays out of what it is told.
             (and block (identity-wire block offsets))))
      (wire:make-player-record
       :identity (getf record :id)
       :name (or (getf record :name) (getf record :id))
       :pose (or pose (record-wire-pose (world-source world) record (world-tick world))
                 (wire:make-pose))
       :creative (and (getf record :creative-p) t)
       :held (wire-block (getf record :selected-block))
       :simple (and (getf record :simple-inventory-p) t)
       :kit (remove nil (mapcar #'wire-block (getf record :building-kit)))
       :inventory (loop for (block quantity) in (getf record :inventory)
                        for wire = (wire-block block)
                        when wire collect (wire:make-stack :block wire :count quantity))))))

(defun wire-record (source remap wire)
  "The player record WIRE describes, its blocks resolved through REMAP and
its pose in SOURCE.  An empty inventory is a new player's: no stock at all."
  (flet ((local-block (block)
           (and block (ignore-errors (wire-identity block remap)))))
    (let* ((pose (wire:player-record-pose wire))
           (record (list :id (wire:player-record-identity wire)
                         :name (wire:player-record-name wire))))
      (when (wire:player-record-inventory wire)
        (setf record
              (append record
                      (list :creative-p (wire:player-record-creative wire)
                            :inventory (loop for stack in (wire:player-record-inventory wire)
                                             for block = (local-block (wire:stack-block stack))
                                             when block
                                               collect (list block (wire:stack-count stack)))
                            :simple-inventory-p (wire:player-record-simple wire)
                            :building-kit (remove nil (mapcar #'local-block
                                                              (wire:player-record-kit wire)))
                            :selected-block (local-block (wire:player-record-held wire))))))
      (record-with-pose record source (coerce (wire:pose-position pose) 'list)
                        (wire:pose-yaw pose) (wire:pose-pitch pose) (wire:pose-flying pose)))))

(defun player-wire-record (session player pose)
  "PLAYER's record in SESSION's world as a wire record, posed at wire POSE."
  (record-wire session (player-table-record (session-world session) player) pose))

(defun remote-player-table-record (world player)
  "A remote PLAYER's record: the one it last sent, posed where its track last saw it."
  (let* ((record (copy-list (player-saved-record player)))
         (session (world-session world))
         (pose (and session (player-last-pose session player))))
    (setf (getf record :id) (player-id player)
          (getf record :name) (player-name player))
    (if pose
        (record-with-pose record (world-source world) (coerce (wire:pose-position pose) 'list)
                          (wire:pose-yaw pose) (wire:pose-pitch pose) (wire:pose-flying pose))
        record)))

;;; The host.

(defun saved-player-pose (session identity)
  "Where the player IDENTITY was when it left, as a wire pose, or NIL."
  (let* ((world (session-world session))
         (record (absent-player-record world identity)))
    (and record (record-wire-pose (world-source world) record (world-tick world)))))

(defun admit-player-record (session player)
  "Give the guest PLAYER the record it left, if it has been here before."
  (let ((record (take-absent-player-record (session-world session) (player-id player))))
    (when record
      (setf (player-saved-record player) record))
    player))

(defun keep-departed-player (session player)
  "Keep the record of PLAYER, who is leaving, posed where it was last seen."
  (let ((world (session-world session)))
    (keep-absent-player-record world (player-table-record world player))))

(defmethod handle-session-message ((session host-session) peer (message wire:player-update-message))
  (let ((player (guest-peer-player session peer))
        (record (wire:player-update-message-player message)))
    ;; A guest speaks only for its own record.
    (when (and player (equal (player-id player) (wire:player-record-identity record)))
      (setf (player-saved-record player)
            (wire-record (world-source (session-world session)) (session-wire-remap session)
                         record)))))

;;; The guest.

(defun send-player-record (session &key force)
  "Tell the host the local player's record when it has changed, or with FORCE."
  (let* ((player (session-local-player session))
         (world (session-world session))
         ;; The pose travels at 30 Hz anyway; only stock and hands make news.
         (still (record-wire session (player-stock-record player) (wire:make-pose))))
    (when (and (eq (session-state session) :open)
               (or force (not (equalp still (session-record-sent session)))))
      (setf (session-record-sent session) still)
      (net:send-message (session-endpoint session)
                        (wire:make-player-update-message
                         :presence :present
                         :player (record-wire session (player-table-record world player)
                                              (local-pose session)))
                        (session-peer session)))))

(defun advance-player-record (session now)
  "At a guest's frame boundary, send its record when it is due and has changed."
  (when (and (typep session 'guest-session) (>= now (session-record-due session)))
    (setf (session-record-due session) (+ now *player-record-period*))
    (send-player-record session)))
