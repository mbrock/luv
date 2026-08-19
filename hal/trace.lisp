;;;; Opt-in nested CPU timing zones for live and benchmark measurements.

(in-package #:luv)

(defstruct (cpu-trace-zone
             (:constructor %make-cpu-trace-zone)
             (:conc-name cpu-trace-zone-))
  name
  (parent-index -1 :type fixnum)
  (depth 0 :type fixnum)
  (started-at 0 :type integer)
  (ended-at 0 :type integer)
  (started-dynamic-usage 0 :type integer)
  (started-bytes-freed 0 :type integer)
  (bytes-consed 0 :type integer)
  (started-gc-run-time 0 :type integer)
  (gc-run-time 0 :type integer)
  (started-garbage-collections 0 :type fixnum)
  (garbage-collections 0 :type fixnum))

(defstruct (cpu-trace
             (:constructor %make-cpu-trace)
             (:conc-name %cpu-trace-))
  label
  (zones (make-array 32 :initial-element nil) :type vector)
  (zone-count 0 :type fixnum)
  (stack (make-array 16 :element-type 'fixnum :initial-element -1)
         :type vector)
  (depth 0 :type fixnum)
  (garbage-collections 0 :type fixnum))

(defstruct runtime-observation
  "Process allocation and garbage-collection evidence for one dynamic extent."
  (elapsed-seconds 0d0 :type double-float)
  (bytes-consed 0 :type integer)
  (gc-seconds 0d0 :type double-float)
  (garbage-collections 0 :type fixnum))

(defvar *cpu-trace* nil
  "The current opt-in CPU trace, or NIL on the ordinary execution path.")

(defvar *measurement-gc-hook-lock*
  (sb-thread:make-mutex :name "luv measurement GC hooks"))

(defun add-measurement-gc-hook (hook)
  (sb-thread:with-mutex (*measurement-gc-hook-lock*)
    (push hook sb-ext:*after-gc-hooks*)))

(defun remove-measurement-gc-hook (hook)
  (sb-thread:with-mutex (*measurement-gc-hook-lock*)
    (setf sb-ext:*after-gc-hooks*
          (delete hook sb-ext:*after-gc-hooks* :test #'eq))))

(declaim (inline measured-allocation-delta))

(defun measured-allocation-delta
    (started-usage started-freed ended-usage ended-freed)
  "Compute allocated bytes from SBCL's non-allocating profiler counters."
  (if (eql started-freed ended-freed)
      (- ended-usage started-usage)
      (- (+ ended-usage ended-freed) started-usage started-freed)))

(defmacro with-runtime-observation ((observation) &body body)
  "Measure time, allocation, GC time, and collections while executing BODY.

OBSERVATION is reset in place and BODY's values are preserved.  SBCL's byte
and GC clocks are process-wide; in a multithreaded image this deliberately
attributes concurrent runtime activity during the observed extent too."
  (let ((result (gensym "OBSERVATION"))
        (hook (gensym "GC-HOOK"))
        (gc-count (gensym "GC-COUNT"))
        (started-at (gensym "STARTED-AT"))
        (started-usage (gensym "STARTED-USAGE"))
        (started-freed (gensym "STARTED-FREED"))
        (started-gc (gensym "STARTED-GC"))
        (finished-p (gensym "FINISHED-P")))
    `(let* ((,result ,observation)
            (,gc-count 0)
            (,hook (lambda () (incf ,gc-count)))
            (,started-usage 0)
            (,started-freed 0)
            (,started-gc 0)
            (,started-at 0)
            (,finished-p nil))
       (add-measurement-gc-hook ,hook)
       (setf ,started-usage (sb-kernel:dynamic-usage)
             ,started-freed sb-kernel::*n-bytes-freed-or-purified*
             ,started-gc sb-ext:*gc-run-time*
             ,started-at (get-internal-real-time))
       (flet ((finish-observation ()
                (unless ,finished-p
                  (let ((ended-at (get-internal-real-time))
                        (ended-usage (sb-kernel:dynamic-usage))
                        (ended-freed
                          sb-kernel::*n-bytes-freed-or-purified*)
                        (ended-gc sb-ext:*gc-run-time*))
                    (setf (runtime-observation-elapsed-seconds ,result)
                          (/ (- ended-at ,started-at)
                             (coerce internal-time-units-per-second
                                     'double-float))
                          (runtime-observation-bytes-consed ,result)
                          (measured-allocation-delta
                           ,started-usage ,started-freed
                           ended-usage ended-freed)
                          (runtime-observation-gc-seconds ,result)
                          (/ (- ended-gc ,started-gc)
                             (coerce internal-time-units-per-second
                                     'double-float))
                          (runtime-observation-garbage-collections ,result)
                          ,gc-count
                          ,finished-p t)))))
         (unwind-protect
              (multiple-value-prog1 (progn ,@body)
                (finish-observation))
           (finish-observation)
           (remove-measurement-gc-hook ,hook))))))

(defun make-cpu-trace (&key label)
  "Make a reusable nested CPU trace buffer named LABEL."
  (%make-cpu-trace :label label))

(defun reset-cpu-trace (trace)
  "Forget TRACE's zones while retaining its allocated storage for reuse."
  (setf (%cpu-trace-zone-count trace) 0
        (%cpu-trace-depth trace) 0
        (%cpu-trace-garbage-collections trace) 0)
  trace)

(defun cpu-trace-garbage-collections (trace)
  (%cpu-trace-garbage-collections trace))

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
            (cpu-trace-zone-started-dynamic-usage zone)
            (sb-kernel:dynamic-usage)
            (cpu-trace-zone-started-bytes-freed zone)
            sb-kernel::*n-bytes-freed-or-purified*
            (cpu-trace-zone-started-gc-run-time zone) sb-ext:*gc-run-time*
            (cpu-trace-zone-started-garbage-collections zone)
            (%cpu-trace-garbage-collections trace)
            (cpu-trace-zone-started-at zone) (get-internal-real-time)
            (cpu-trace-zone-ended-at zone) 0
            (cpu-trace-zone-bytes-consed zone) 0
            (cpu-trace-zone-gc-run-time zone) 0
            (cpu-trace-zone-garbage-collections zone) 0
            (aref stack depth) index
            (%cpu-trace-zone-count trace) (1+ index)
            (%cpu-trace-depth trace) (1+ depth))
      index)))

(defun end-cpu-trace-zone (trace index)
  (let ((depth (%cpu-trace-depth trace)))
    (unless (and (plusp depth)
                 (= index (aref (%cpu-trace-stack trace) (1- depth))))
      (error "CPU trace zones must end in nested order."))
    (let ((zone (aref (%cpu-trace-zones trace) index))
          (ended-at (get-internal-real-time))
          (ended-usage (sb-kernel:dynamic-usage))
          (ended-freed sb-kernel::*n-bytes-freed-or-purified*)
          (ended-gc sb-ext:*gc-run-time*)
          (ended-gc-count (%cpu-trace-garbage-collections trace)))
      (setf (cpu-trace-zone-ended-at zone) ended-at
            (cpu-trace-zone-bytes-consed zone)
            (measured-allocation-delta
             (cpu-trace-zone-started-dynamic-usage zone)
             (cpu-trace-zone-started-bytes-freed zone)
             ended-usage ended-freed)
            (cpu-trace-zone-gc-run-time zone)
            (- ended-gc (cpu-trace-zone-started-gc-run-time zone))
            (cpu-trace-zone-garbage-collections zone)
            (- ended-gc-count
               (cpu-trace-zone-started-garbage-collections zone))
            (%cpu-trace-depth trace) (1- depth))))
  (values))

(defmacro with-cpu-trace ((trace) &body body)
  "Reset TRACE, bind it dynamically, and record zones established by BODY."
  (let ((active (gensym "TRACE"))
        (hook (gensym "GC-HOOK")))
    `(let* ((,active (reset-cpu-trace ,trace))
            (,hook
              (lambda ()
                (incf (%cpu-trace-garbage-collections ,active)))))
       (add-measurement-gc-hook ,hook)
       (unwind-protect
            (let ((*cpu-trace* ,active))
              ,@body)
         (remove-measurement-gc-hook ,hook)))))

(defmacro with-cpu-trace-zone
    ((name &key (tracy-value nil tracy-value-supplied-p)) &body body)
  "Measure BODY as nested zone NAME for whichever measurement is watching.

TRACY-VALUE, when supplied, is attached to the Tracy zone at exit.  It does
not affect the bounded CPU trace, whose zones retain time and runtime costs.

Two independent things may be: a Tracy viewer attached to this image, and an
opt-in CPU-TRACE capture.  Each disabled path is one special-variable test and
does not allocate, so instrumenting a frame path costs nothing when nobody is
measuring it.  Active traces reuse their zone storage after the first capture.

The Tracy zone is the outer one.  When only Tracy is watching there is no
bookkeeping inside it to measure, and when a CPU-TRACE capture is running its
own cost belongs to the zone it is attributed to rather than being hidden from
it.  #OHNIWM"
  (let ((trace (gensym "TRACE"))
        (index (gensym "ZONE")))
    `(with-tracy-zone
         (,name ,@(when tracy-value-supplied-p
                    `(:value ,tracy-value)))
       (let* ((,trace *cpu-trace*)
              (,index (and ,trace (begin-cpu-trace-zone ,trace ,name))))
         ;; BODY occurs once so nested instrumentation does not multiply the
         ;; compiler's input.  TRACE, rather than INDEX, guards cleanup because
         ;; zero is a valid first zone index.
         (unwind-protect
              (progn ,@body)
           (when ,trace
             (end-cpu-trace-zone ,trace ,index)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun zoned-definition-name-and-options (specification)
    (if (and (consp specification)
             (keywordp (second specification)))
        (let ((name (first specification))
              (options (rest specification)))
          (unless (evenp (length options))
            (error "Odd zoned definition option list ~S." options))
          (loop for key in options by #'cddr
                unless (member key '(:zone :value))
                  do (error "Unknown zoned definition option ~S." key))
          (values name
                  (getf options :zone)
                  (not (null (member :zone options)))
                  (getf options :value)
                  (not (null (member :value options)))))
        (values specification nil nil nil nil)))

  (defun zoned-symbol-name (symbol)
    (let ((package (symbol-package symbol)))
      (format nil "~(~A/~A~)"
              (if package (package-name package) "anonymous")
              (symbol-name symbol))))

  (defun zoned-function-name (name)
    (etypecase name
      (symbol (zoned-symbol-name name))
      (cons
       (destructuring-bind (operator symbol) name
         (unless (eq operator 'setf)
           (error "Invalid function name ~S." name))
         (format nil "~A/setf" (zoned-symbol-name symbol))))))

  (defun zoned-specializer-name (specializer)
    (etypecase specializer
      (symbol (zoned-symbol-name specializer))
      (cons
       (destructuring-bind (operator value) specializer
         (unless (eq operator 'eql)
           (error "Invalid method specializer ~S." specializer))
         (format nil "eql/~(~A~)" value)))))

  (defun zoned-method-name (name qualifiers specialized-lambda-list)
    (let ((specializers
            (loop for parameter in specialized-lambda-list
                  until (member parameter lambda-list-keywords)
                  when (and (consp parameter)
                            (not (eq (second parameter) t)))
                    collect (zoned-specializer-name (second parameter)))))
      (format nil "~A~@[<~{~A~^,~}>~]~@[{~{~(~A~)~^,~}}~]"
              (zoned-function-name name)
              specializers
              qualifiers)))

  (defun zoned-definition-body (body)
    (let ((docstring (and (stringp (first body)) (pop body)))
          declarations)
      (loop while (and (consp (first body))
                       (eq 'declare (first (first body))))
            do (push (pop body) declarations))
      (values docstring (nreverse declarations) body))))

(defmacro zone (specification &body body)
  "Measure BODY with concise ambient zone syntax.

SPECIFICATION is either a zone name or (NAME :VALUE FORM).  This is the
ordinary spelling for a meaningful region inside a definition; ZDEFUN and
ZDEFMETHOD cover whole definitions."
  (if (consp specification)
      (destructuring-bind (name &key (value nil value-supplied-p)) specification
        `(with-cpu-trace-zone
             (,name ,@(when value-supplied-p `(:tracy-value ,value)))
           ,@body))
      `(with-cpu-trace-zone (,specification)
         ,@body)))

(defmacro zdefun (name-and-options lambda-list &body body)
  "Define a function whose complete dynamic extent is an ambient trace zone.

The inferred name is PACKAGE/FUNCTION.  Use (NAME :ZONE ZONE-NAME) to provide
a more semantic name and :VALUE FORM to attach a Tracy work count."
  (multiple-value-bind (name explicit-zone zone-supplied-p
                        value value-supplied-p)
      (zoned-definition-name-and-options name-and-options)
    (multiple-value-bind (docstring declarations forms)
        (zoned-definition-body body)
      (let ((zone-name (if zone-supplied-p
                           explicit-zone
                           (zoned-function-name name))))
        `(defun ,name ,lambda-list
           ,@(when docstring (list docstring))
           ,@declarations
           (with-cpu-trace-zone
               (,zone-name
                ,@(when value-supplied-p `(:tracy-value ,value)))
             ,@forms))))))

(defmacro zdefmethod (name-and-options &rest method-tail)
  "Define a method whose complete dynamic extent is an ambient trace zone.

The inferred name includes the generic function, non-T required specializers,
and qualifiers.  :ZONE and :VALUE have the same meaning as in ZDEFUN."
  (multiple-value-bind (name explicit-zone zone-supplied-p
                        value value-supplied-p)
      (zoned-definition-name-and-options name-and-options)
    (let ((qualifiers nil)
          (tail method-tail))
      (loop while (and tail (atom (first tail)))
            do (push (pop tail) qualifiers))
      (setf qualifiers (nreverse qualifiers))
      (unless (and tail (listp (first tail)))
        (error "ZDEFMETHOD ~S has no specialized lambda list." name))
      (let ((specialized-lambda-list (pop tail)))
        (multiple-value-bind (docstring declarations forms)
            (zoned-definition-body tail)
          (let ((zone-name
                  (if zone-supplied-p
                      explicit-zone
                      (zoned-method-name
                       name qualifiers specialized-lambda-list))))
            `(defmethod ,name ,@qualifiers ,specialized-lambda-list
               ,@(when docstring (list docstring))
               ,@declarations
               (with-cpu-trace-zone
                   (,zone-name
                    ,@(when value-supplied-p `(:tracy-value ,value)))
                 ,@forms))))))))

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

(defun cpu-trace-zone-self-bytes-consed (trace index)
  (let* ((zones (%cpu-trace-zones trace))
         (zone (aref zones index))
         (child-bytes 0))
    (loop for child-index below (%cpu-trace-zone-count trace)
          for child = (aref zones child-index)
          when (= index (cpu-trace-zone-parent-index child))
            do (incf child-bytes (cpu-trace-zone-bytes-consed child)))
    (- (cpu-trace-zone-bytes-consed zone) child-bytes)))

(defun cpu-trace-zone-gc-seconds (zone)
  (/ (cpu-trace-zone-gc-run-time zone)
     (coerce internal-time-units-per-second 'double-float)))

(defun cpu-trace-zone-self-gc-seconds (trace index)
  (let* ((zones (%cpu-trace-zones trace))
         (zone (aref zones index))
         (child-ticks 0))
    (loop for child-index below (%cpu-trace-zone-count trace)
          for child = (aref zones child-index)
          when (= index (cpu-trace-zone-parent-index child))
            do (incf child-ticks (cpu-trace-zone-gc-run-time child)))
    (/ (- (cpu-trace-zone-gc-run-time zone) child-ticks)
       (coerce internal-time-units-per-second 'double-float))))

(defun print-cpu-trace (trace &optional (stream *standard-output*))
  "Print TRACE as a bounded time, allocation, and GC table."
  (format stream "CPU trace~@[ ~A~] (~D garbage collection~:P)~%"
          (%cpu-trace-label trace)
          (%cpu-trace-garbage-collections trace))
  (format stream
          "  zone                                  inclusive      self       allocated    self alloc       GC~%")
  (loop for index below (%cpu-trace-zone-count trace)
        for zone = (aref (%cpu-trace-zones trace) index)
        do (format stream
                   "  ~V@T~(~A~)~45T~8,3F ms  ~8,3F ms  ~10,1F KiB  ~10,1F KiB  ~7,3F ms~%"
                   (* 2 (cpu-trace-zone-depth zone))
                   (cpu-trace-zone-name zone)
                   (* 1000d0 (cpu-trace-zone-seconds zone))
                   (* 1000d0 (cpu-trace-zone-self-seconds trace index))
                   (/ (cpu-trace-zone-bytes-consed zone) 1024d0)
                   (/ (cpu-trace-zone-self-bytes-consed trace index) 1024d0)
                   (* 1000d0 (cpu-trace-zone-gc-seconds zone))))
  trace)
