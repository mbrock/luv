(in-package #:luv)

;;; Validation as conditions.
;;;
;;; The Khronos validation layer is the only thing that ever says a Vulkan
;;; call was wrong.  Without it the driver simply does something -- usually
;;; the thing you meant, sometimes nothing, occasionally a command buffer
;;; that never completes -- and the mistake surfaces hours later as a black
;;; frame or a wedged present.  Its messages therefore deserve the same
;;; standing as any other error in the image, rather than a line of text on
;;; a stream nobody is reading.
;;;
;;; Two moments matter, and they are not the same moment:
;;;
;;;   The layer calls back on the thread that made the offending call, with
;;;   that call's Lisp frames still on the stack.  That is the only instant
;;;   at which the culprit can be named, so the callback logs the message
;;;   and captures a backtrace right there.
;;;
;;;   The callback returns into the middle of a Vulkan driver.  Unwinding
;;;   from there -- which is what signalling an error would do -- leaves the
;;;   driver holding whatever it was holding.  So nothing is signalled until
;;;   a safe point outside the call, where WITH-VULKAN-VALIDATION turns
;;;   whatever the extent collected into one condition.
;;;
;;; The layer is not loaded unless someone asks for it, so all of this costs
;;; nothing in an ordinary run:
;;;
;;;   VK_LOADER_LAYERS_ENABLE='*validation*' ./sly start

(defparameter *vulkan-validation-enabled-p* t
  "Whether to install luv's own debug messenger on a new Vulkan instance.

The messenger only ever hears from a layer that is loaded, so leaving this
true costs nothing in a run without VK_LOADER_LAYERS_ENABLE.  Bind it to NIL
to keep a provider quiet even when the layer is present.")

(defparameter *vulkan-validation-backtrace-p* t
  "Whether the callback captures a Lisp backtrace for each message.

The backtrace is the whole value of catching the message where it happens:
the text names the Vulkan call, and only the stack names the code that made
it.  Turning this off leaves the message with nothing but its own words.")

(defstruct (vulkan-validation-note (:constructor %make-vulkan-validation-note))
  "One thing the validation layer said, and where Lisp was when it said it."
  severity
  types
  id-name
  id-number
  text
  backtrace)

(defun vulkan-validation-note-error-p (note)
  (and (member :error (vulkan-validation-note-severity note)) t))

(defvar *vulkan-validation-collecting-p* nil
  "Whether the current dynamic extent retains messages for a later check.")

(defvar *vulkan-validation-notes* nil
  "Messages retained by the innermost WITH-VULKAN-VALIDATION extent.")

(defun report-vulkan-validation-problem (condition stream)
  (let ((notes (vulkan-validation-problem-notes condition)))
    (format stream "Vulkan validation~@[ during ~S~]: ~D message~:P."
            (vulkan-validation-problem-operation condition) (length notes))
    (dolist (note notes)
      (format stream "~2%~{~A~^, ~}~@[ [~A]~]~%~A"
              (vulkan-validation-note-severity note)
              (vulkan-validation-note-id-name note)
              (vulkan-validation-note-text note))
      (when (vulkan-validation-note-backtrace note)
        (format stream "~&~A" (vulkan-validation-note-backtrace note))))))

(define-condition vulkan-validation-problem ()
  ((operation
    :initarg :operation
    :initform nil
    :reader vulkan-validation-problem-operation)
   (notes
    :initarg :notes
    :initform nil
    :reader vulkan-validation-problem-notes))
  (:documentation "What the validation layer said during one operation."))

(define-condition vulkan-validation-failure (vulkan-validation-problem error)
  ()
  (:report report-vulkan-validation-problem)
  (:documentation "The validation layer rejected a call made during OPERATION.

The API call itself returned normally -- a layer message is not a result
code -- so this is signalled at the first safe point after it, carrying the
backtrace captured while the offending frames were still on the stack."))

(define-condition vulkan-validation-complaint (vulkan-validation-problem warning)
  ()
  (:report report-vulkan-validation-problem)
  (:documentation "The validation layer warned about a call made during
OPERATION."))

(defun note-vulkan-debug-message (message)
  "Receive one message from the Vulkan layer stack.

This runs inside the call that produced the message, so it says its piece to
the log immediately -- a run that then wedges in the driver still leaves the
complaint behind -- and retains it for the enclosing extent to signal."
  (let ((note
          (%make-vulkan-validation-note
           :severity (lvk:debug-message-severity message)
           :types (lvk:debug-message-types message)
           :id-name (lvk:debug-message-id-name message)
           :id-number (lvk:debug-message-id-number message)
           :text (lvk:debug-message-text message)
           :backtrace (when *vulkan-validation-backtrace-p*
                        (ignore-errors (capture-backtrace-string))))))
    (log-event :vulkan "~{~A~^, ~}~@[ [~A]~] ~A~@[~%~A~]"
               (vulkan-validation-note-severity note)
               (vulkan-validation-note-id-name note)
               (vulkan-validation-note-text note)
               (vulkan-validation-note-backtrace note))
    (when *vulkan-validation-collecting-p*
      (push note *vulkan-validation-notes*))
    note))

(defun check-vulkan-validation (operation)
  "Signal whatever the current extent collected, then forget it.

An error outranks a warning: when the layer rejected anything at all, that
is the condition worth having, and the warnings are carried along inside it
rather than raised separately."
  (let ((notes (nreverse *vulkan-validation-notes*)))
    (setf *vulkan-validation-notes* nil)
    (when notes
      (if (some #'vulkan-validation-note-error-p notes)
          (error 'vulkan-validation-failure :operation operation :notes notes)
          (warn 'vulkan-validation-complaint
                :operation operation :notes notes))))
  (values))

(defun call-with-vulkan-validation (operation function)
  (let ((*vulkan-validation-collecting-p* t)
        (*vulkan-validation-notes* nil))
    (multiple-value-prog1 (funcall function)
      (check-vulkan-validation operation))))

(defmacro with-vulkan-validation ((&optional operation) &body body)
  "Run BODY, then signal what the validation layer said while it ran.

BODY leaving by its own error is left alone: that error is the news, and the
layer's account of it is already in the log."
  `(call-with-vulkan-validation ,operation (lambda () ,@body)))
