;;;; A general protocol for reifying the calls of an API as objects.
;;;;
;;;; This does for calls what the condition system does for situations:
;;;; each entry point of a reified API is a class whose instances are
;;;; invocations, with the arguments in slots and hierarchical dispatch
;;;; over both what is invoked and who invokes it.  An API adopts the
;;;; protocol by subclassing INVOCATION-CLASS with its own metadata,
;;;; INVOCATION with its own families, and INVOKER with its own ways of
;;;; performing a call; luv's Vulkan boundary (vulkan.lisp, DEFVKFUN) is
;;;; the first adopter.

(in-package #:luv.invocation)

(defclass invocation-class (closer-mop:standard-class)
  ((argument-specs
    :initform nil
    :accessor invocation-class-arguments))
  (:documentation
   "The metaclass of invocation classes.  Reified APIs subclass it to hang
their own definition metadata on each entry point's class metaobject."))

(defmethod closer-mop:validate-superclass
    ((class invocation-class) (superclass closer-mop:standard-class))
  (declare (ignore class superclass))
  t)

(defclass invocation ()
  ((sequence
    :initform nil
    :accessor invocation-sequence)
   (timestamp
    :initform nil
    :accessor invocation-timestamp)
   (duration
    :initform nil
    :accessor invocation-duration)
   (thread
    :initform nil
    :accessor invocation-thread)
   (values
    :initform nil
    :accessor invocation-values)
   (status
    :initform nil
    :accessor invocation-status)
   (condition
    :initform nil
    :accessor invocation-condition))
  (:metaclass invocation-class)
  (:documentation
   "One call, reified.  The concrete class names the entry point, its
direct slots carry the arguments, and the slots here are bookkeeping
filled in by a recording invoker."))

(defun invocation-name (invocation)
  (class-name (class-of invocation)))

(defun invocation-arguments (invocation)
  "Return INVOCATION's arguments as a list of (NAME VALUE) pairs."
  (loop for (name nil) in (invocation-class-arguments (class-of invocation))
        collect (list name
                      (if (slot-boundp invocation name)
                          (slot-value invocation name)
                          :unbound))))

(defmethod print-object ((invocation invocation) stream)
  (print-unreadable-object (invocation stream :type nil :identity nil)
    (format stream "~A~@[ #~D~]"
            (invocation-name invocation)
            (invocation-sequence invocation))))

(defclass invoker ()
  ()
  (:documentation
   "Something that can perform invocations.  A reified API binds one to a
special variable; subclasses observe or replace calls with methods."))

(defgeneric invoke (invoker invocation)
  (:documentation "Perform INVOCATION according to INVOKER."))

;;; Durable snapshots of argument and result values.

(defun snapshot-invocation-value (value &optional (depth 0))
  "Copy VALUE into durable, printable trace data without retaining C memory."
  (cond
    ((cffi:pointerp value)
     (list :pointer (cffi:pointer-address value)))
    ((or (null value) (symbolp value) (numberp value) (characterp value))
     value)
    ((stringp value) (copy-seq value))
    ((>= depth 6) (list :object (type-of value)))
    ((consp value)
     (cons (snapshot-invocation-value (car value) (1+ depth))
           (snapshot-invocation-value (cdr value) (1+ depth))))
    ((vectorp value)
     (map 'vector
          (lambda (item)
            (snapshot-invocation-value item (1+ depth)))
          value))
    (t
     (list :object (type-of value)
           (handler-case (princ-to-string value)
             (error () "<unprintable>"))))))

(defun snapshot-invocation-arguments (invocation)
  "Make INVOCATION durable by snapshotting its argument slots in place."
  (loop for (name nil) in (invocation-class-arguments (class-of invocation))
        when (slot-boundp invocation name)
          do (setf (slot-value invocation name)
                   (snapshot-invocation-value
                    (slot-value invocation name)))))

;;; Structured tracing is one invoker mixin wrapping the call.

(defstruct (invocation-trace
             (:constructor %make-invocation-trace)
             (:conc-name %invocation-trace-))
  (started-at 0.0d0)
  stopped-at
  (next-sequence 0)
  (events (make-array 0 :adjustable t :fill-pointer 0))
  #+sb-thread
  (lock (sb-thread:make-mutex :name "luv invocation trace")))

(defun invocation-trace-now ()
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defmacro with-invocation-trace-lock ((trace) &body body)
  #+sb-thread
  `(sb-thread:with-mutex ((%invocation-trace-lock ,trace)) ,@body)
  #-sb-thread
  `(progn ,@body))

(defun make-invocation-trace ()
  (%make-invocation-trace :started-at (invocation-trace-now)))

(defun stop-invocation-trace (trace)
  (setf (%invocation-trace-stopped-at trace) (invocation-trace-now))
  trace)

(defun invocation-trace-events (trace)
  "Return TRACE's invocations in call-start order as a fresh list."
  (with-invocation-trace-lock (trace)
    (sort (coerce (copy-seq (%invocation-trace-events trace)) 'list)
          #'< :key #'invocation-sequence)))

(defun invocation-thread-name ()
  #+sb-thread
  (or (sb-thread:thread-name sb-thread:*current-thread*) "unnamed thread")
  #-sb-thread
  "single thread")

(defun reserve-invocation-sequence (trace)
  (with-invocation-trace-lock (trace)
    (prog1 (%invocation-trace-next-sequence trace)
      (incf (%invocation-trace-next-sequence trace)))))

(defun record-invocation (trace invocation)
  (with-invocation-trace-lock (trace)
    (vector-push-extend invocation (%invocation-trace-events trace)))
  invocation)

(defclass tracing-invoker ()
  ((trace
    :initarg :trace
    :reader tracing-invoker-trace))
  (:documentation
   "An invoker mixin that performs each call and retains the invocation,
with timing and results, in an INVOCATION-TRACE."))

(defmethod invoke :around ((invoker tracing-invoker) (call invocation))
  (let ((trace (tracing-invoker-trace invoker))
        (started-at (invocation-trace-now)))
    (setf (invocation-sequence call) (reserve-invocation-sequence trace)
          (invocation-timestamp call) (- started-at
                                         (%invocation-trace-started-at trace))
          (invocation-thread call) (invocation-thread-name)
          (invocation-status call) :signaled)
    (handler-bind
        ((error
           (lambda (condition)
             (setf (invocation-condition call)
                   (list :type (type-of condition)
                         :message
                         (handler-case (princ-to-string condition)
                           (error () "<unprintable condition>")))))))
      (unwind-protect
           (let ((results (multiple-value-list (call-next-method))))
             (setf (invocation-status call) :returned
                   (invocation-values call)
                   (mapcar #'snapshot-invocation-value results))
             (values-list results))
        (setf (invocation-duration call) (- (invocation-trace-now) started-at))
        (snapshot-invocation-arguments call)
        (record-invocation trace call)))))
