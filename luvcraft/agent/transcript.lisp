(in-package #:luvcraft.agent)

;;; The transcript: typed records the HUD, a wall, and a plain stream draw from.
;;;
;;; A TURN is one prompt and what came of it; a TOOL-CALL is one command the
;;; model ran inside a turn.  They are data first (#YY6EYZ): the cassette
;;; view, the model view, and PRINT-TRANSCRIPT are each a way of reading the
;;; same objects, and none of them needs the turn rerun.

(defclass tool-call ()
  ((tool :initarg :tool :reader tool-call-tool)
   (command :initarg :command :initform nil :accessor tool-call-command
            :documentation "The CLIM command, (NAME . ARGUMENTS), once parsed.")
   (arguments :initarg :arguments :reader tool-call-arguments
              :documentation "The JSON arguments as the model sent them.")
   (status :initform :running :accessor tool-call-status
           :documentation "One of :RUNNING :OK :ERROR.")
   (started :initform (get-internal-real-time) :reader tool-call-started)
   (finished :initform nil :accessor tool-call-finished)
   (result :initform nil :accessor tool-call-result
           :documentation "The command's value, or NIL.")
   (error :initform nil :accessor tool-call-error)
   (output :initform "" :accessor tool-call-output
           :documentation "What the model was told: the result under the model view.")))

(defun tool-call-name (call)
  (openai:tool-name (tool-call-tool call)))

(defun tool-call-elapsed-seconds (call)
  (/ (- (or (tool-call-finished call) (get-internal-real-time))
        (tool-call-started call))
     (float internal-time-units-per-second 1.0)))

(defmethod print-object ((call tool-call) stream)
  (print-unreadable-object (call stream :type t)
    (format stream "~A ~(~A~) ~,1Fs" (tool-call-name call)
            (tool-call-status call) (tool-call-elapsed-seconds call))))

(defclass turn ()
  ((prompt :initarg :prompt :reader turn-prompt)
   (thought :initform "" :accessor turn-thought
            :documentation "The reasoning summary so far, for the bubble.")
   (text :initform "" :accessor turn-text
         :documentation "The assistant's text so far.")
   (calls :initform '() :accessor turn-calls
          :documentation "Tool calls, newest first.")
   (status :initform :thinking :accessor turn-status
           :documentation "One of :THINKING :WORKING :DONE :FAILED.")
   (error :initform nil :accessor turn-error)
   (response :initform nil :accessor turn-response)
   (started :initform (get-internal-real-time) :reader turn-started)
   (finished :initform nil :accessor turn-finished)
   (thread :initform nil :accessor turn-thread)))

(defmethod print-object ((turn turn) stream)
  (print-unreadable-object (turn stream :type t)
    (format stream "~(~A~) ~D call~:P ~S" (turn-status turn)
            (length (turn-calls turn))
            (let ((prompt (turn-prompt turn)))
              (if (> (length prompt) 40)
                  (concatenate 'string (subseq prompt 0 40) "...")
                  prompt)))))

(defun turn-calls-in-order (turn)
  (reverse (turn-calls turn)))

;;; ---------------------------------------------------------------------
;;; Plain text, for a session with no window at all.

(defun transcript-lines (turn)
  "TURN as lines of plain text: the no-window rendering."
  (let ((lines '()))
    (flet ((line (control &rest arguments)
             (push (apply #'format nil control arguments) lines)))
      (line "> ~A" (turn-prompt turn))
      (unless (string= (turn-thought turn) "")
        (line "  (thinking) ~A" (turn-thought turn)))
      (dolist (call (turn-calls-in-order turn))
        (line "  [~A] ~A ~A ~,1Fs~@[ -- ~A~]"
              (case (tool-call-status call) (:ok "ok") (:error "!!") (t ".."))
              (tool-call-name call)
              (tool-call-arguments-text call)
              (tool-call-elapsed-seconds call)
              (and (tool-call-error call) (princ-to-string (tool-call-error call))))
        (let ((output (tool-call-output call)))
          (unless (string= output "")
            (dolist (text-line (uiop:split-string output :separator '(#\Newline)))
              (line "      ~A" text-line)))))
      (unless (string= (turn-text turn) "")
        (line "~A" (turn-text turn)))
      (when (turn-error turn)
        (line "  failed: ~A" (turn-error turn))))
    (nreverse lines)))

(defun tool-call-arguments-text (call)
  "The model's arguments as one short line."
  (let ((arguments (tool-call-arguments call)))
    (format nil "~{~(~A~)=~A~^ ~}"
            (loop for (key . value) in arguments
                  collect key collect value))))

(defun print-transcript (turn &optional (stream *standard-output*))
  (dolist (line (transcript-lines turn))
    (write-line line stream))
  turn)
