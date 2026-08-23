(in-package #:luv.application-agent)

;;; Typed transcript records are the durable boundary shared by plain text,
;;; retained developer panels, and application-specific world presentation.

(defclass tool-call ()
  ((tool :initarg :tool :reader tool-call-tool)
   (command :initarg :command :initform nil :accessor tool-call-command)
   (arguments :initarg :arguments :reader tool-call-arguments)
   (status :initform :running :accessor tool-call-status)
   (started :initform (get-internal-real-time) :reader tool-call-started)
   (finished :initform nil :accessor tool-call-finished)
   (result :initform nil :accessor tool-call-result)
   (error :initform nil :accessor tool-call-error)
   (output :initform "" :accessor tool-call-output)))

(defun tool-call-name (call)
  (openai:tool-name (tool-call-tool call)))

(defun tool-call-elapsed-seconds (call)
  (/ (- (or (tool-call-finished call) (get-internal-real-time))
        (tool-call-started call))
     (float internal-time-units-per-second 1.0)))

(defmethod print-object ((call tool-call) stream)
  (print-unreadable-object (call stream :type t)
    (format stream "~A ~(~A~) ~,1Fs"
            (tool-call-name call)
            (tool-call-status call)
            (tool-call-elapsed-seconds call))))

(defclass turn ()
  ((prompt :initarg :prompt :reader turn-prompt)
   (thought :initform "" :accessor turn-thought)
   (text :initform "" :accessor turn-text)
   (%calls :initform '() :accessor %turn-calls)
   (status :initform :queued :accessor turn-status)
   (error :initform nil :accessor turn-error)
   (response :initform nil :accessor turn-response)
   (started :initform nil :accessor turn-started)
   (finished :initform nil :accessor turn-finished)
   (thread :initform nil :accessor turn-thread)
   (lock :initform (sb-thread:make-mutex :name "application agent turn")
         :reader turn-lock)
   (completion :initform
               (sb-thread:make-waitqueue :name "application agent turn complete")
               :reader turn-completion)))

(defun finish-turn (turn)
  "Publish TURN's completion once and wake every waiter."
  (sb-thread:with-mutex ((turn-lock turn))
    (unless (turn-finished turn)
      (setf (turn-finished turn) (get-internal-real-time))
      (sb-thread:condition-broadcast (turn-completion turn))))
  turn)

(defun wait-for-turn (turn &key timeout)
  "Return TURN when complete, or NIL when TIMEOUT seconds elapse."
  (let ((deadline
          (and timeout
               (+ (get-internal-real-time)
                  (round (* timeout internal-time-units-per-second))))))
    (sb-thread:with-mutex ((turn-lock turn))
      (loop until (turn-finished turn)
            do (let ((remaining
                       (and deadline
                            (/ (- deadline (get-internal-real-time))
                               (float internal-time-units-per-second 1.0)))))
                 (when (and remaining (not (plusp remaining)))
                   (return-from wait-for-turn nil))
                 (unless (sb-thread:condition-wait
                          (turn-completion turn) (turn-lock turn)
                          :timeout remaining)
                   (return-from wait-for-turn nil))))))
  turn)

(defun turn-calls (turn)
  "Return a stable newest-first snapshot of TURN's tool calls."
  (sb-thread:with-mutex ((turn-lock turn))
    (copy-list (%turn-calls turn))))

(defun push-turn-call (turn call)
  (sb-thread:with-mutex ((turn-lock turn))
    (push call (%turn-calls turn)))
  call)

(defmethod print-object ((turn turn) stream)
  (print-unreadable-object (turn stream :type t)
    (format stream "~(~A~) ~D call~:P ~S"
            (turn-status turn)
            (length (turn-calls turn))
            (let ((prompt (turn-prompt turn)))
              (if (> (length prompt) 40)
                  (concatenate 'string (subseq prompt 0 40) "...")
                  prompt)))))

(defun turn-calls-in-order (turn)
  (reverse (turn-calls turn)))

(defun tool-call-arguments-text (call)
  (format nil "~{~(~A~)=~A~^ ~}"
          (loop for (key . value) in (tool-call-arguments call)
                collect key collect value)))

(defun transcript-lines (turn)
  "Return TURN as plain lines suitable for a REPL or log."
  (let ((lines '()))
    (flet ((line (control &rest arguments)
             (push (apply #'format nil control arguments) lines)))
      (line "> ~A" (turn-prompt turn))
      (unless (string= (turn-thought turn) "")
        (line "  (thinking) ~A" (turn-thought turn)))
      (dolist (call (turn-calls-in-order turn))
        (line "  [~A] ~A ~A ~,1Fs~@[ -- ~A~]"
              (case (tool-call-status call)
                (:ok "ok")
                (:error "!!")
                (otherwise ".."))
              (tool-call-name call)
              (tool-call-arguments-text call)
              (tool-call-elapsed-seconds call)
              (and (tool-call-error call)
                   (princ-to-string (tool-call-error call))))
        (unless (string= (tool-call-output call) "")
          (dolist (text-line
                   (uiop:split-string (tool-call-output call)
                                      :separator '(#\Newline)))
            (line "      ~A" text-line))))
      (unless (string= (turn-text turn) "")
        (line "~A" (turn-text turn)))
      (when (turn-error turn)
        (line "  failed: ~A" (turn-error turn))))
    (nreverse lines)))

(defun print-transcript (turn &optional (stream *standard-output*))
  (dolist (line (transcript-lines turn))
    (write-line line stream))
  turn)
