;;;; One line per event, for the parts of luv that would otherwise be silent.
;;;;
;;;; luvcraft says nothing at all in ordinary operation.  That is comfortable
;;;; until something stops working, and then there is no record of a window
;;;; that never appeared, a canvas that stopped pumping, or a swapchain
;;;; rebuilt on every frame.  A timestamped, categorised line on standard
;;;; error costs nothing while things work and is the whole story when they
;;;; do not.
;;;;
;;;; This is deliberately not a logging framework.  There are no levels, no
;;;; appenders, and no configuration file: a category, a format string, and a
;;;; stream that ./sly log already tails.  Anything that happens once per
;;;; frame does not belong here -- the trace zones in trace.lisp exist for
;;;; that.

(in-package #:luv)

(defparameter *log-enabled-p* t
  "Whether LOG-EVENT writes anything at all.")

(defparameter *log-categories* t
  "T to print every category, or a list of the category names to print.")

(defvar *log-stream* nil
  "Where LOG-EVENT writes, or NIL for the calling thread's *ERROR-OUTPUT*.

A Slynk worker rebinds *ERROR-OUTPUT* to its connection, so leaving this NIL
means an event logged while answering ./sly appears in that answer, while the
canvas and watchdog threads keep writing to the image's own error stream and
therefore to ./sly log.")

(defvar *log-lock* (sb-thread:make-mutex :name "luv log")
  "Held only while one line is written, so threads cannot interleave halves.")

(defconstant +unix-to-universal-time+
  (encode-universal-time 0 0 0 1 1 1970 0)
  "Seconds between the Unix epoch and the Common Lisp universal time epoch.")

(defun log-category-enabled-p (category)
  (or (eq t *log-categories*)
      (and (listp *log-categories*) (member category *log-categories*))))

(defun log-timestamp-string ()
  "Return the wall clock as HH:MM:SS.mmm, in the local zone."
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (multiple-value-bind (second minute hour)
        (decode-universal-time (+ seconds +unix-to-universal-time+))
      (format nil "~2,'0D:~2,'0D:~2,'0D.~3,'0D"
              hour minute second (floor microseconds 1000)))))

(defun log-event (category control &rest arguments)
  "Write one timestamped line about CATEGORY, formatted from CONTROL.

CATEGORY is a keyword naming the subsystem -- :canvas, :watchdog, :vulkan --
so a reader can tell at a glance which machine is talking."
  (when (and *log-enabled-p* (log-category-enabled-p category))
    (let ((text (apply #'format nil control arguments))
          (stamp (log-timestamp-string))
          (stream (or *log-stream* *error-output*)))
      (sb-thread:with-recursive-lock (*log-lock*)
        (format stream "~&luv ~A ~(~A~) ~A~%" stamp category text)
        (force-output stream))))
  (values))
