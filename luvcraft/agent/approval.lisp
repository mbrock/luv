(in-package #:luvcraft.agent)

;;; A parked tool call is a semantic object, not a closure waiting to happen.
;;; The canvas owns every transition; the provider thread only waits on the
;;; private mailbox through SETTLE-COMMAND-RESULT (#T6PEVY).

(defclass tool-approval ()
  ((agent :initarg :agent :reader tool-approval-agent)
   (presence :initarg :presence :reader tool-approval-presence)
   (session :initarg :session :reader tool-approval-session)
   (state :initform :proposed :accessor tool-approval-state)
   (note :initform nil :accessor tool-approval-note)
   (created-at :initform (get-internal-real-time)
               :reader tool-approval-created-at)
   (mailbox :initform (sb-concurrency:make-mailbox :name "tool approval")
            :reader tool-approval-mailbox)
   (lock :initform (sb-thread:make-mutex :name "tool approval")
         :reader tool-approval-lock))
  (:documentation
   "One inert proposed tool effect waiting for a player's decision."))

(defgeneric validate-tool-approval (approval)
  (:documentation "Signal when APPROVAL can no longer be committed."))

(defgeneric commit-tool-approval (approval)
  (:documentation "Commit APPROVAL's already validated semantic payload."))

(defgeneric detach-tool-approval (approval)
  (:documentation "Remove APPROVAL's temporary presence and owned resources."))

(defgeneric tool-approval-focus-camera-pose (approval presence session)
  (:documentation "Return the cinematic pose for APPROVAL, or NIL."))

(defmethod validate-tool-approval ((approval tool-approval))
  (declare (ignore approval))
  t)

(defmethod commit-tool-approval ((approval tool-approval))
  (error "~S has no commit method." approval))

(defmethod detach-tool-approval ((approval tool-approval))
  (declare (ignore approval))
  nil)

(defmethod tool-approval-focus-camera-pose
    ((approval tool-approval) presence session)
  (declare (ignore approval presence session))
  nil)

(defun finish-tool-approval (approval state &optional note)
  "Publish APPROVAL's terminal STATE exactly once and wake its tool call."
  (let ((finished-p nil))
    (sb-thread:with-mutex ((tool-approval-lock approval))
      (when (eq :proposed (tool-approval-state approval))
        (setf (tool-approval-state approval) state
              (tool-approval-note approval) note
              finished-p t)))
    (when finished-p
      (unwind-protect
           (detach-tool-approval approval)
        (sb-concurrency:send-message (tool-approval-mailbox approval)
                                     approval)))
    approval))

(defgeneric approve-tool-approval (approval)
  (:documentation "Validate and commit APPROVAL, or finish it as failed."))

(defgeneric deny-tool-approval (approval &optional note)
  (:documentation "Reject APPROVAL without changing its subject."))

(defgeneric steer-tool-approval (approval note)
  (:documentation "Return APPROVAL to its proposer with revision guidance."))

(defmethod approve-tool-approval ((approval tool-approval))
  ;; Decisions originate on the canvas thread, so validation and publication
  ;; remain one serialized transition with respect to the world.
  (when (eq :proposed (tool-approval-state approval))
    (handler-case
        (progn
          (validate-tool-approval approval)
          (commit-tool-approval approval)
          (finish-tool-approval approval :approved))
      (error (condition)
        (finish-tool-approval approval :failed (format nil "~A" condition)))))
  approval)

(defmethod deny-tool-approval ((approval tool-approval) &optional note)
  (finish-tool-approval approval :denied note))

(defmethod steer-tool-approval ((approval tool-approval) note)
  (finish-tool-approval approval :changes-requested note))

(defmethod settle-command-result ((command t) (approval tool-approval))
  (declare (ignore command))
  (if (eq :proposed (tool-approval-state approval))
      (sb-concurrency:receive-message (tool-approval-mailbox approval))
      approval))
