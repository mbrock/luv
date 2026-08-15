;;;; Binding machinery for luv's owned Vulkan vocabulary.
;;;;
;;;; This file owns the loader, result translation, tagged-struct filler,
;;;; temporary foreign argument helpers, and opt-in tracing.  The
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

;;; Direct entry points with backend-local, opt-in trace events.

(defstruct vulkan-function-definition
  name foreign-name return-type arguments command-p)

(defvar *vulkan-function-definitions* (make-hash-table :test #'eq))

(defstruct vulkan-call-event
  sequence timestamp duration thread name foreign-name arguments values
  status condition)

(defstruct (vulkan-trace
             (:constructor %make-vulkan-trace)
             (:conc-name %vulkan-trace-))
  (started-at 0.0d0)
  stopped-at
  (next-sequence 0)
  (events (make-array 0 :adjustable t :fill-pointer 0))
  #+sb-thread
  (lock (sb-thread:make-mutex :name "luv Vulkan trace")))

(defvar *vulkan-trace* nil
  "The active Vulkan trace, or NIL on the ordinary direct FFI path.")

(defun vulkan-trace-now ()
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defmacro with-vulkan-trace-lock ((trace) &body body)
  #+sb-thread
  `(sb-thread:with-mutex ((%vulkan-trace-lock ,trace)) ,@body)
  #-sb-thread
  `(progn ,@body))

(defun make-vulkan-trace ()
  (%make-vulkan-trace :started-at (vulkan-trace-now)))

(defun finish-vulkan-trace (trace)
  (setf (%vulkan-trace-stopped-at trace) (vulkan-trace-now))
  trace)

(defun vulkan-trace-events (trace)
  "Return trace events in call-start order as a fresh list."
  (with-vulkan-trace-lock (trace)
    (sort (coerce (copy-seq (%vulkan-trace-events trace)) 'list)
          #'< :key #'vulkan-call-event-sequence)))

(defun vulkan-trace-thread-name ()
  #+sb-thread
  (or (sb-thread:thread-name sb-thread:*current-thread*) "unnamed thread")
  #-sb-thread
  "single thread")

(defun snapshot-vulkan-value (value &optional (depth 0))
  (cond
    ((cffi:pointerp value)
     (list :pointer (cffi:pointer-address value)))
    ((or (null value) (symbolp value) (numberp value) (characterp value))
     value)
    ((stringp value) (copy-seq value))
    ((>= depth 6) (list :object (type-of value)))
    ((consp value)
     (cons (snapshot-vulkan-value (car value) (1+ depth))
           (snapshot-vulkan-value (cdr value) (1+ depth))))
    ((vectorp value)
     (map 'vector
          (lambda (item) (snapshot-vulkan-value item (1+ depth)))
          value))
    (t
     (list :object (type-of value)
           (handler-case (princ-to-string value)
             (error () "<unprintable>"))))))

(defun call-with-vulkan-trace-event (trace definition arguments function)
  (let* ((started-at (vulkan-trace-now))
         (event
           (make-vulkan-call-event
            :sequence
            (with-vulkan-trace-lock (trace)
              (prog1 (%vulkan-trace-next-sequence trace)
                (incf (%vulkan-trace-next-sequence trace))))
            :timestamp (- started-at (%vulkan-trace-started-at trace))
            :thread (vulkan-trace-thread-name)
            :name (vulkan-function-definition-name definition)
            :foreign-name (vulkan-function-definition-foreign-name definition)
            :arguments
            (mapcar (lambda (argument)
                      (list (first argument)
                            (snapshot-vulkan-value (second argument))))
                    arguments)
            :status :signaled)))
    (handler-bind
        ((error
           (lambda (condition)
             (setf (vulkan-call-event-condition event)
                   (list :type (type-of condition)
                         :message
                         (handler-case (princ-to-string condition)
                           (error () "<unprintable condition>")))))))
      (unwind-protect
           (let ((results (multiple-value-list (funcall function))))
             (setf (vulkan-call-event-status event) :returned
                   (vulkan-call-event-values event)
                   (mapcar #'snapshot-vulkan-value results))
             (values-list results))
        (setf (vulkan-call-event-duration event)
              (- (vulkan-trace-now) started-at))
        (with-vulkan-trace-lock (trace)
          (vector-push-extend event (%vulkan-trace-events trace)))))))

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

  (defun register-vulkan-function-definition
      (name foreign-name return-type argument-specs)
    (let ((definition
            (make-vulkan-function-definition
             :name name :foreign-name foreign-name :return-type return-type
             :arguments argument-specs
             :command-p (and (> (length foreign-name) 5)
                             (string= "vkCmd" foreign-name :end2 5)))))
      (setf (gethash name *vulkan-function-definitions*) definition)
      definition)))

(defun vulkan-function-description (name)
  "Return definition metadata retained by DEFVKFUN for VK function NAME."
  (let ((definition (gethash name *vulkan-function-definitions*)))
    (when definition
      (list :foreign-name (vulkan-function-definition-foreign-name definition)
            :return-type (vulkan-function-definition-return-type definition)
            :arguments (vulkan-function-definition-arguments definition)
            :command-p (vulkan-function-definition-command-p definition)))))

(defmacro defvkfun (foreign-name return-type &body arguments)
  "Define a direct Vulkan entry point with opt-in trace instrumentation."
  (let* ((lisp-name (vulkan-lisp-name foreign-name))
         (raw-name (intern (format nil "%~A" lisp-name) '#:luv.vulkan))
         (argument-names (mapcar #'first arguments))
         (definition-name
           (intern (format nil "*~A-DEFINITION*" lisp-name) '#:luv.vulkan)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',lisp-name '#:luv.vk))
       (cffi:defcfun (,foreign-name ,raw-name :library vulkan-loader)
           ,return-type
         ,@arguments)
       (defparameter ,definition-name
         (register-vulkan-function-definition
          ',lisp-name ,foreign-name ',return-type ',arguments))
       (defun ,lisp-name ,argument-names
         (flet ((call () (,raw-name ,@argument-names)))
           (if *vulkan-trace*
               (call-with-vulkan-trace-event
                *vulkan-trace* ,definition-name
                (list ,@(loop for name in argument-names
                              collect `(list ',name ,name)))
                #'call)
               (call))))
       ',lisp-name)))

(defmacro defvkproc (foreign-name return-type &body arguments)
  "Define an instance extension command resolved through vkGetInstanceProcAddr."
  (let* ((lisp-name (vulkan-lisp-name foreign-name))
         (argument-names (mapcar #'first arguments))
         (definition-name
           (intern (format nil "*~A-DEFINITION*" lisp-name) '#:luv.vulkan)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',lisp-name '#:luv.vk))
       (defparameter ,definition-name
         (register-vulkan-function-definition
          ',lisp-name ,foreign-name ',return-type ',arguments))
       (defun ,lisp-name ,argument-names
         (flet ((call ()
                  (cffi:foreign-funcall-pointer
                   (instance-procedure ,(first argument-names) ,foreign-name)
                   ()
                   ,@(loop for (name type) in arguments append (list type name))
                   ,return-type)))
           (if *vulkan-trace*
               (call-with-vulkan-trace-event
                *vulkan-trace* ,definition-name
                (list ,@(loop for name in argument-names
                              collect `(list ',name ,name)))
                #'call)
               (call))))
       ',lisp-name)))

(defun start-vulkan-trace ()
  "Start a process-wide structured trace of calls crossing into Vulkan."
  (when *vulkan-trace*
    (error "A Vulkan trace is already active."))
  (setf *vulkan-trace* (make-vulkan-trace)))

(defun stop-vulkan-trace ()
  "Stop and return the active Vulkan trace, or NIL when none is active."
  (when *vulkan-trace*
    (let ((trace *vulkan-trace*))
      (setf *vulkan-trace* nil)
      (finish-vulkan-trace trace))))

(defun current-vulkan-trace ()
  "Return the active Vulkan trace, if any."
  *vulkan-trace*)

(defmacro with-vulkan-trace ((trace) &body body)
  "Run BODY with this thread's Vulkan calls recorded into a fresh trace.

Binds the backend-local trace only for BODY's dynamic extent."
  `(let* ((,trace (make-vulkan-trace))
          (*vulkan-trace* ,trace))
     (unwind-protect (progn ,@body)
       (finish-vulkan-trace ,trace))))

(defun vulkan-trace-presentation-intervals (trace &key include-prefix)
  "Return completed event intervals between vkQueuePresentKHR calls.

Each interval excludes its opening presentation and includes its closing
presentation.  INCLUDE-PREFIX also returns the possibly partial interval from
the beginning of TRACE through its first presentation."
  (let ((interval nil)
        (intervals nil)
        (saw-presentation nil))
    (dolist (event (vulkan-trace-events trace) (nreverse intervals))
      (push event interval)
      (when (string= "vkQueuePresentKHR"
                     (vulkan-call-event-foreign-name event))
        (when (or saw-presentation include-prefix)
          (push (nreverse interval) intervals))
        (setf interval nil
              saw-presentation t)))))
