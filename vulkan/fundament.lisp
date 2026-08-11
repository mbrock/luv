;;;; Binding machinery for luv's owned Vulkan vocabulary.
;;;;
;;;; This file owns the loader, result translation, tagged-struct filler,
;;;; temporary foreign argument helpers, and invocation/tracing bridge.  The
;;;; actual Vulkan treaty text lives in defs.lisp.

(in-package #:luv.vulkan)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library vulkan-loader
    (:darwin (:or "libvulkan.1.dylib" "libvulkan.dylib"))
    (:unix (:or "libvulkan.so.1" "libvulkan.so"))
    (:windows "vulkan-1.dll")))

(cffi:use-foreign-library vulkan-loader)

;;; Conditions and result translation.

(define-condition vulkan-call-error (error)
  ((operation
    :initarg :operation
    :reader vulkan-call-error-operation)
   (result
    :initarg :result
    :reader vulkan-call-error-result))
  (:report
   (lambda (condition stream)
     (format stream "Vulkan call ~S failed with VkResult ~D."
             (vulkan-call-error-operation condition)
             (vulkan-call-error-result condition)))))

(defvar *vulkan-operation* :unknown-vulkan-operation)
(defvar *accepted-results* '(:success))

(cffi:define-foreign-type checked-result-type ()
  ()
  (:actual-type :int32)
  (:simple-parser checked-result))

(defmethod cffi:translate-from-foreign
    (value (type checked-result-type))
  (declare (ignore type))
  (let ((result (cffi:foreign-enum-keyword 'result value :errorp nil)))
    (unless (member result *accepted-results*)
      (error 'vulkan-call-error
             :operation *vulkan-operation*
             :result value))
    result))

(defmacro with-vulkan-results ((operation &rest accepted-results) &body body)
  `(let ((*vulkan-operation* ,operation)
         (*accepted-results* ',(or accepted-results '(:success))))
     ,@body))

;;; Struct declarations remain explicit treaty text.  DEFVKSTRUCT supplies the
;;; standard tagged-struct header and retains the declaration as Lisp data for
;;; increasingly capable fillers later.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *struct-descriptions* (make-hash-table)))

(defmacro defvkstruct (name (&key s-type) &body slots)
  (let ((all-slots
          (append (when s-type
                    '((s-type structure-type)
                      (p-next :pointer)))
                  slots)))
    `(progn
       (cffi:defcstruct ,name ,@all-slots)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *struct-descriptions*)
               ',(list :s-type s-type :slots (mapcar #'first all-slots)))))))

(defun clear-foreign-object (pointer type &optional (count 1))
  (loop for index below (* count (cffi:foreign-type-size type))
        do (setf (cffi:mem-aref pointer :uint8 index) 0))
  pointer)

(defun fill-vk (pointer type &rest fields)
  (let* ((description
           (or (gethash type *struct-descriptions*)
               (error "Unknown Vulkan struct ~S." type)))
         (foreign-type `(:struct ,type))
         (slots (getf description :slots)))
    (clear-foreign-object pointer foreign-type)
    (let ((s-type (getf description :s-type)))
      (when s-type
        (setf (cffi:foreign-slot-value pointer foreign-type 's-type) s-type
              (cffi:foreign-slot-value pointer foreign-type 'p-next)
              (cffi:null-pointer))))
    (loop for (field value) on fields by #'cddr
          for slot = (find (symbol-name field) slots
                           :key #'symbol-name :test #'string=)
          unless slot
            do (error "~S is not a slot of Vulkan struct ~S." field type)
          do (setf (cffi:foreign-slot-value pointer foreign-type slot) value))
    pointer))

(defmacro with-vk ((variable type &rest fields) &body body)
  `(cffi:with-foreign-object (,variable '(:struct ,type))
     (fill-vk ,variable ',type ,@fields)
     ,@body))

;;; Arguments which own temporary foreign storage.

(cffi:define-foreign-type string-list-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser string-list))

(defmethod cffi:translate-to-foreign
    (strings (type string-list-type))
  (declare (ignore type))
  (if (null strings)
      (values (cffi:null-pointer) nil)
      (let ((pointers nil)
            (array nil))
        (unwind-protect
             (progn
               (dolist (string strings)
                 (push (cffi:foreign-string-alloc string) pointers))
               (setf pointers (nreverse pointers)
                     array (cffi:foreign-alloc
                            :pointer :count (length pointers)))
               (loop for pointer in pointers
                     for index from 0
                     do (setf (cffi:mem-aref array :pointer index)
                              pointer))
               (values array pointers))
          (unless array
            (mapc #'cffi:foreign-string-free pointers))))))

(defmethod cffi:free-translated-object
    (pointer (type string-list-type) strings)
  (declare (ignore type))
  (mapc #'cffi:foreign-string-free strings)
  (unless (cffi:null-pointer-p pointer)
    (cffi:foreign-free pointer)))

(defmacro with-translated-values (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (variable value type) (first bindings)
        (let ((parameter (gensym "PARAMETER")))
          `(multiple-value-bind (,variable ,parameter)
               (cffi:convert-to-foreign ,value ',type)
             (unwind-protect
                  (with-translated-values ,(rest bindings) ,@body)
               (cffi:free-converted-object
                ,variable ',type ,parameter)))))))

(defmacro with-foreign-array ((pointer type values) &body body)
  (let ((items (gensym "ITEMS"))
        (index (gensym "INDEX"))
        (item (gensym "ITEM")))
    `(let ((,items ,values))
       (if (zerop (length ,items))
           (let ((,pointer (cffi:null-pointer)))
             ,@body)
           (cffi:with-foreign-object (,pointer ',type (length ,items))
             (loop for ,index below (length ,items)
                   for ,item = (elt ,items ,index)
                   do (setf (cffi:mem-aref ,pointer ',type ,index) ,item))
             ,@body)))))

;;; The Vulkan face of the invocation protocol (invocation.lisp).  Every
;;; foreign entry point is a class whose instances are invocations: the
;;; arguments live in slots, INVOKE performs the crossing, and the FFI
;;; object bound to *VULKAN-FFI* decides what a crossing means.  Tracing,
;;; validation, and mocking are methods, not modes.

(defclass vulkan-function-class (invocation-class)
  ((foreign-name
    :initform nil
    :accessor vulkan-function-foreign-name)
   (return-type
    :initform nil
    :accessor vulkan-function-return-type)))

(defclass vulkan-invocation (invocation)
  ()
  (:metaclass vulkan-function-class)
  (:documentation "One call across the Vulkan boundary, reified."))

(defclass vulkan-command (vulkan-invocation)
  ()
  (:metaclass vulkan-function-class)
  (:documentation
   "Invocations of vkCmd* entry points, which record into command buffers."))

(defun invocation-foreign-name (invocation)
  (vulkan-function-foreign-name (class-of invocation)))

(defmethod print-object ((invocation vulkan-invocation) stream)
  (print-unreadable-object (invocation stream :type nil :identity nil)
    (format stream "~A~@[ #~D~]"
            (or (invocation-foreign-name invocation)
                (invocation-name invocation))
            (invocation-sequence invocation))))

(defclass vulkan-ffi (invoker)
  ()
  (:documentation
   "The plain FFI: INVOKE crosses straight into the driver."))

(defvar *vulkan-ffi* (make-instance 'vulkan-ffi)
  "The FFI object every Vulkan invocation passes through.  Rebind or SETF
it to an FFI subclass instance to trace, check, or mock the boundary.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vulkan-lisp-name (foreign-name)
    "Intern and export vkCamelCase FOREIGN-NAME as LUV.VK:CAMEL-CASE."
    (let ((base
            (with-output-to-string (out)
              (loop for index from 2 below (length foreign-name)
                    for char = (char foreign-name index)
                    do (when (and (> index 2)
                                  (upper-case-p char)
                                  (or (lower-case-p
                                       (char foreign-name (1- index)))
                                      (and (< (1+ index) (length foreign-name))
                                           (lower-case-p
                                            (char foreign-name (1+ index))))))
                         (write-char #\- out))
                       (write-char (char-upcase char) out)))))
      (let ((symbol (intern base '#:luv.vk)))
        (export symbol '#:luv.vk)
        symbol)))

  (defun configure-vulkan-function-class
      (class foreign-name return-type argument-specs)
    (setf (vulkan-function-foreign-name class) foreign-name
          (vulkan-function-return-type class) return-type
          (invocation-class-arguments class) argument-specs)
    class))

(defun vulkan-function-description (name)
  "Return definition metadata retained by DEFVKFUN for the VK class NAME."
  (let ((class (find-class name nil)))
    (when (typep class 'vulkan-function-class)
      (list :foreign-name (vulkan-function-foreign-name class)
            :return-type (vulkan-function-return-type class)
            :arguments (invocation-class-arguments class)))))

(defmacro defvkfun (foreign-name return-type &body arguments)
  "Define a Vulkan entry point as a class of invocations plus its protocol.

FOREIGN-NAME becomes a symbol in the LUV.VK package (vkCreateImage becomes
VK:CREATE-IMAGE) naming three things at once: the invocation class whose
slots are the arguments, the function that makes an invocation and hands it
to *VULKAN-FFI*, and the INVOKE primary method that performs the actual
foreign call."
  (let* ((lisp-name (vulkan-lisp-name foreign-name))
         (raw-name (intern (format nil "%~A" lisp-name) '#:luv.vulkan))
         (argument-names (mapcar #'first arguments))
         (superclass (if (and (> (length foreign-name) 5)
                              (string= "vkCmd" foreign-name :end2 5))
                         'vulkan-command
                         'vulkan-invocation)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',lisp-name '#:luv.vk))
       (cffi:defcfun (,foreign-name ,raw-name :library vulkan-loader)
           ,return-type
         ,@arguments)
       (defclass ,lisp-name (,superclass)
         ,(loop for name in argument-names
                collect
                `(,name :initarg ,(intern (symbol-name name) :keyword)))
         (:metaclass vulkan-function-class))
       (eval-when (:load-toplevel :execute)
         (configure-vulkan-function-class
          (find-class ',lisp-name) ,foreign-name ',return-type ',arguments))
       (defmethod invoke ((ffi vulkan-ffi) (call ,lisp-name))
         (with-slots ,argument-names call
           (,raw-name ,@argument-names)))
       (defun ,lisp-name ,argument-names
         (invoke
          *vulkan-ffi*
          (make-instance ',lisp-name
                         ,@(loop for name in argument-names
                                 append
                                 (list (intern (symbol-name name) :keyword)
                                       name)))))
       ',lisp-name)))

(defmacro defvkproc (foreign-name return-type &body arguments)
  "Define an instance extension command resolved through vkGetInstanceProcAddr."
  (let* ((lisp-name (vulkan-lisp-name foreign-name))
         (argument-names (mapcar #'first arguments)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',lisp-name '#:luv.vk))
       (defclass ,lisp-name (vulkan-invocation)
         ,(loop for name in argument-names
                collect
                `(,name :initarg ,(intern (symbol-name name) :keyword)))
         (:metaclass vulkan-function-class))
       (eval-when (:load-toplevel :execute)
         (configure-vulkan-function-class
          (find-class ',lisp-name) ,foreign-name ',return-type ',arguments))
       (defmethod invoke ((ffi vulkan-ffi) (call ,lisp-name))
         (declare (ignore ffi))
         (with-slots ,argument-names call
           (cffi:foreign-funcall-pointer
            (instance-procedure ,(first argument-names) ,foreign-name)
            ()
            ,@(loop for (name type) in arguments append (list type name))
            ,return-type)))
       (defun ,lisp-name ,argument-names
         (invoke
          *vulkan-ffi*
          (make-instance ',lisp-name
                         ,@(loop for name in argument-names
                                 append
                                 (list (intern (symbol-name name) :keyword)
                                       name)))))
       ',lisp-name)))

;;; Structured tracing is the general TRACING-INVOKER mixin applied to the
;;; Vulkan FFI, plus Vulkan-shaped ways of starting, scoping, and reading
;;; a trace.

(defclass tracing-ffi (tracing-invoker vulkan-ffi)
  ()
  (:documentation
   "An FFI that performs each crossing and retains the invocation, with
timing and results, in an INVOCATION-TRACE."))

(defun start-vulkan-trace ()
  "Start a process-wide structured trace of calls crossing into Vulkan."
  (when (typep *vulkan-ffi* 'tracing-ffi)
    (error "A Vulkan trace is already active."))
  (let ((trace (make-invocation-trace)))
    (setf *vulkan-ffi* (make-instance 'tracing-ffi :trace trace))
    trace))

(defun stop-vulkan-trace ()
  "Stop and return the active Vulkan trace, or NIL when none is active."
  (when (typep *vulkan-ffi* 'tracing-ffi)
    (let ((trace (tracing-invoker-trace *vulkan-ffi*)))
      (setf *vulkan-ffi* (make-instance 'vulkan-ffi))
      (stop-invocation-trace trace))))

(defun current-vulkan-trace ()
  "Return the trace *VULKAN-FFI* is recording into, if any."
  (when (typep *vulkan-ffi* 'tracing-ffi)
    (tracing-invoker-trace *vulkan-ffi*)))

(defmacro with-vulkan-trace ((trace) &body body)
  "Run BODY with this thread's Vulkan calls recorded into a fresh trace.

Binds *VULKAN-FFI* for BODY's dynamic extent and binds TRACE to the
INVOCATION-TRACE being recorded; the trace is stopped when BODY exits."
  `(let* ((,trace (make-invocation-trace))
          (*vulkan-ffi* (make-instance 'tracing-ffi :trace ,trace)))
     (unwind-protect (progn ,@body)
       (stop-invocation-trace ,trace))))

(defun vulkan-trace-presentation-intervals (trace &key include-prefix)
  "Return completed event intervals between vkQueuePresentKHR calls.

Each interval excludes its opening presentation and includes its closing
presentation.  INCLUDE-PREFIX also returns the possibly partial interval from
the beginning of TRACE through its first presentation."
  (let ((interval nil)
        (intervals nil)
        (saw-presentation nil))
    (dolist (event (invocation-trace-events trace) (nreverse intervals))
      (push event interval)
      (when (string= "vkQueuePresentKHR"
                     (invocation-foreign-name event))
        (when (or saw-presentation include-prefix)
          (push (nreverse interval) intervals))
        (setf interval nil
              saw-presentation t)))))
