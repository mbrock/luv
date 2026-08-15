;;;; Opt-in nested CPU timing zones for live and benchmark measurements.

(in-package #:luv)

(defstruct (cpu-trace-zone
             (:constructor %make-cpu-trace-zone)
             (:conc-name cpu-trace-zone-))
  name
  (parent-index -1 :type fixnum)
  (depth 0 :type fixnum)
  (started-at 0 :type integer)
  (ended-at 0 :type integer))

(defstruct (cpu-trace
             (:constructor %make-cpu-trace)
             (:conc-name %cpu-trace-))
  label
  (zones (make-array 32 :initial-element nil) :type vector)
  (zone-count 0 :type fixnum)
  (stack (make-array 16 :element-type 'fixnum :initial-element -1)
         :type vector)
  (depth 0 :type fixnum))

(defvar *cpu-trace* nil
  "The current opt-in CPU trace, or NIL on the ordinary execution path.")

(defun make-cpu-trace (&key label)
  "Make a reusable nested CPU trace buffer named LABEL."
  (%make-cpu-trace :label label))

(defun reset-cpu-trace (trace)
  "Forget TRACE's zones while retaining its allocated storage for reuse."
  (setf (%cpu-trace-zone-count trace) 0
        (%cpu-trace-depth trace) 0)
  trace)

(defun grow-cpu-trace-vector (vector minimum-size &key element-type)
  (let ((size (max minimum-size (* 2 (length vector)))))
    (adjust-array vector size
                  :element-type (or element-type (array-element-type vector))
                  :initial-element (if element-type -1 nil))))

(defun begin-cpu-trace-zone (trace name)
  (let* ((index (%cpu-trace-zone-count trace))
         (depth (%cpu-trace-depth trace))
         (zones (%cpu-trace-zones trace))
         (stack (%cpu-trace-stack trace)))
    (when (= index (length zones))
      (setf zones (grow-cpu-trace-vector zones (1+ index))
            (%cpu-trace-zones trace) zones))
    (when (= depth (length stack))
      (setf stack
            (grow-cpu-trace-vector stack (1+ depth) :element-type 'fixnum)
            (%cpu-trace-stack trace) stack))
    (let ((zone (or (aref zones index) (%make-cpu-trace-zone))))
      (setf (aref zones index) zone
            (cpu-trace-zone-name zone) name
            (cpu-trace-zone-parent-index zone)
            (if (zerop depth) -1 (aref stack (1- depth)))
            (cpu-trace-zone-depth zone) depth
            (cpu-trace-zone-started-at zone) (get-internal-real-time)
            (cpu-trace-zone-ended-at zone) 0
            (aref stack depth) index
            (%cpu-trace-zone-count trace) (1+ index)
            (%cpu-trace-depth trace) (1+ depth))
      index)))

(defun end-cpu-trace-zone (trace index)
  (let ((depth (%cpu-trace-depth trace)))
    (unless (and (plusp depth)
                 (= index (aref (%cpu-trace-stack trace) (1- depth))))
      (error "CPU trace zones must end in nested order."))
    (setf (cpu-trace-zone-ended-at
           (aref (%cpu-trace-zones trace) index))
          (get-internal-real-time)
          (%cpu-trace-depth trace) (1- depth)))
  (values))

(defmacro with-cpu-trace ((trace) &body body)
  "Reset TRACE, bind it dynamically, and record zones established by BODY."
  (let ((active (gensym "TRACE")))
    `(let* ((,active (reset-cpu-trace ,trace))
            (*cpu-trace* ,active))
       ,@body)))

(defmacro with-cpu-trace-zone ((name) &body body)
  "Measure BODY as nested zone NAME when CPU tracing is active.

The disabled path is one special-variable test and does not allocate.  Active
traces reuse their zone storage after the first capture.  #OHNIWM"
  (let ((trace (gensym "TRACE"))
        (index (gensym "ZONE")))
    `(let ((,trace *cpu-trace*))
       (if ,trace
           (let ((,index (begin-cpu-trace-zone ,trace ,name)))
             (unwind-protect
                  (progn ,@body)
               (end-cpu-trace-zone ,trace ,index)))
           (progn ,@body)))))

(defun cpu-trace-zones (trace)
  "Return TRACE's completed zones in start order."
  (loop for index below (%cpu-trace-zone-count trace)
        collect (aref (%cpu-trace-zones trace) index)))

(defun cpu-trace-zone-seconds (zone)
  (/ (- (cpu-trace-zone-ended-at zone)
        (cpu-trace-zone-started-at zone))
     (coerce internal-time-units-per-second 'double-float)))

(defun cpu-trace-zone-self-seconds (trace index)
  (let* ((zones (%cpu-trace-zones trace))
         (zone (aref zones index))
         (child-ticks 0))
    (loop for child-index below (%cpu-trace-zone-count trace)
          for child = (aref zones child-index)
          when (= index (cpu-trace-zone-parent-index child))
            do (incf child-ticks
                     (- (cpu-trace-zone-ended-at child)
                        (cpu-trace-zone-started-at child))))
    (/ (- (- (cpu-trace-zone-ended-at zone)
             (cpu-trace-zone-started-at zone))
          child-ticks)
       (coerce internal-time-units-per-second 'double-float))))

(defun print-cpu-trace (trace &optional (stream *standard-output*))
  "Print TRACE as a bounded nested inclusive/self-time table."
  (format stream "CPU trace~@[ ~A~]~%" (%cpu-trace-label trace))
  (format stream "  zone                                  inclusive      self~%")
  (loop for index below (%cpu-trace-zone-count trace)
        for zone = (aref (%cpu-trace-zones trace) index)
        do (format stream "  ~V@T~(~A~)~45T~8,3F ms  ~8,3F ms~%"
                   (* 2 (cpu-trace-zone-depth zone))
                   (cpu-trace-zone-name zone)
                   (* 1000d0 (cpu-trace-zone-seconds zone))
                   (* 1000d0 (cpu-trace-zone-self-seconds trace index))))
  trace)
