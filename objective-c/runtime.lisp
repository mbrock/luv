;;;; A small declared Objective-C foreign object system.
;;;;
;;;; Classes and objects preserve native identity at the Lisp boundary.  A
;;;; message declaration is a class of invocations whose metaclass owns the
;;;; selector, exact ABI, and ownership convention.  INVOKE dispatches on both
;;;; the runtime and that message class; tracing composes as an invoker mixin.

(in-package #:luv.objective-c)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library objective-c-runtime-library
    (:darwin (:or "libobjc.A.dylib" "libobjc.dylib")))
  (cffi:define-foreign-library foundation-framework
    (:darwin (:framework "Foundation"))))

(cffi:use-foreign-library objective-c-runtime-library)
(cffi:use-foreign-library foundation-framework)

(defmacro with-objective-c-native-environment (&body body)
  "Run BODY with the floating-point environment expected by Apple frameworks."
  #+sbcl
  `(sb-int:with-float-traps-masked
       (:invalid :divide-by-zero :overflow :underflow :inexact)
     ,@body)
  #-sbcl
  `(progn ,@body))

(cffi:defcfun ("objc_getClass" %objc-get-class
               :library objective-c-runtime-library)
    :pointer
  (name :string))

(cffi:defcfun ("sel_registerName" %sel-register-name
               :library objective-c-runtime-library)
    :pointer
  (name :string))

(cffi:defcfun ("object_getClassName" %object-get-class-name
               :library objective-c-runtime-library)
    :pointer
  (object :pointer))

(define-condition objective-c-error (error) ())

(define-condition objective-c-message-error (objective-c-error)
  ((message :initarg :message :reader objective-c-exception-message)
   (receiver :initarg :receiver :reader objective-c-exception-receiver)
   (selector :initarg :selector :reader objective-c-exception-selector)
   (name :initarg :name :reader objective-c-exception-name)
   (reason :initarg :reason :reader objective-c-exception-reason)
   (call-stack :initarg :call-stack :reader objective-c-exception-call-stack)))

(define-condition objective-c-exception (objective-c-message-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Objective-C exception ~A while sending ~A: ~A"
             (or (objective-c-exception-name condition) "<unnamed>")
             (objective-c-exception-selector condition)
             (or (objective-c-exception-reason condition) "<no reason>")))))

(define-condition objective-c-bridge-error (objective-c-message-error)
  ()
  (:report
   (lambda (condition stream)
     (format stream "Objective-C bridge rejected ~A: ~A"
             (objective-c-exception-selector condition)
             (or (objective-c-exception-reason condition) "<no reason>"))))
  (:documentation
   "The native boundary could not perform a declared Objective-C message."))

(define-condition unknown-objective-c-class (objective-c-error)
  ((name :initarg :name :reader unknown-objective-c-class-name))
  (:report
   (lambda (condition stream)
     (format stream "Objective-C class ~S is not registered."
             (unknown-objective-c-class-name condition)))))

(define-condition released-objective-c-object (objective-c-error)
  ((object :initarg :object :reader released-objective-c-object-object))
  (:report
   (lambda (condition stream)
     (format stream "Objective-C object ~S has already been released."
             (released-objective-c-object-object condition)))))

(define-condition objective-c-ownership-error (objective-c-error)
  ((object :initarg :object :reader objective-c-ownership-error-object))
  (:report
   (lambda (condition stream)
     (format stream "Objective-C object ~S does not own a retain to consume."
             (objective-c-ownership-error-object condition)))))

(defclass objective-c-receiver ()
  ()
  (:documentation "Something that can receive an Objective-C message."))

(defclass objective-c-class (objective-c-receiver)
  ((name :initarg :name :reader objective-c-class-name)
   (pointer :initarg :pointer :reader %objective-c-class-pointer))
  (:documentation "A borrowed Objective-C runtime Class identity."))

(defclass objective-c-object (objective-c-receiver)
  ((pointer :initarg :pointer :reader %objective-c-object-pointer)
   (class-name :initarg :class-name :reader objective-c-object-class-name)
   (protocol-name :initarg :protocol-name :initform nil
                  :reader objective-c-object-protocol-name)
   (ownership :initarg :ownership :reader objective-c-object-ownership)
   (released-p :initform nil :accessor objective-c-object-released-p))
  (:documentation
   "One native Objective-C pointer and exactly one owned or borrowed claim."))

(defmethod print-object ((class objective-c-class) stream)
  (print-unreadable-object (class stream :type t :identity nil)
    (format stream "~A 0x~X" (objective-c-class-name class)
            (cffi:pointer-address (%objective-c-class-pointer class)))))

(defmethod print-object ((object objective-c-object) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A~@[ as ~A~] ~A~:[~; released~] 0x~X"
            (objective-c-object-class-name object)
            (objective-c-object-protocol-name object)
            (objective-c-object-ownership object)
            (objective-c-object-released-p object)
            (cffi:pointer-address (%objective-c-object-pointer object)))))

(defgeneric objective-c-pointer (receiver)
  (:documentation "Return RECEIVER's live native pointer for one ABI crossing."))

(defmethod objective-c-pointer ((receiver objective-c-class))
  (%objective-c-class-pointer receiver))

(defmethod objective-c-pointer ((object objective-c-object))
  (when (objective-c-object-released-p object)
    (error 'released-objective-c-object :object object))
  (%objective-c-object-pointer object))

(defmethod objective-c-pointer ((nothing null))
  (cffi:null-pointer))

(defun find-objective-c-class (name)
  "Return the registered Objective-C class NAME as a borrowed receiver."
  (let ((pointer (%objc-get-class name)))
    (when (cffi:null-pointer-p pointer)
      (error 'unknown-objective-c-class :name name))
    (make-instance 'objective-c-class :name name :pointer pointer)))

(defun objective-c-runtime-class-name (pointer)
  (let ((name (%object-get-class-name pointer)))
    (if (cffi:null-pointer-p name)
        "<unknown>"
        (cffi:foreign-string-to-lisp name))))

(defun wrap-objective-c-object
    (pointer &key (ownership :borrowed) protocol-name)
  "Wrap POINTER with one explicit :OWNED or :BORROWED claim; NIL represents nil."
  (when (cffi:null-pointer-p pointer)
    (return-from wrap-objective-c-object nil))
  (unless (member ownership '(:owned :borrowed))
    (error "Unknown Objective-C ownership ~S." ownership))
  (make-instance 'objective-c-object
                 :pointer pointer
                 :class-name (objective-c-runtime-class-name pointer)
                 :protocol-name protocol-name
                 :ownership ownership))

(defun objective-c-object= (left right)
  "Whether LEFT and RIGHT wrap the same native Objective-C identity."
  (and (typep left 'objective-c-object)
       (typep right 'objective-c-object)
       (= (cffi:pointer-address (objective-c-pointer left))
          (cffi:pointer-address (objective-c-pointer right)))))

(defstruct objective-c-message-definition
  name selector selector-pointer result-type result-ownership result-class-name
  consumes-receiver-p arguments)

(defvar *objective-c-message-definitions* (make-hash-table :test #'eq))

(defstruct objective-c-message-event
  sequence timestamp duration thread name arguments values status condition)

(defstruct (objective-c-trace
             (:constructor %make-objective-c-trace)
             (:conc-name %objective-c-trace-))
  (started-at 0.0d0)
  (next-sequence 0)
  (events (make-array 0 :adjustable t :fill-pointer 0))
  #+sb-thread
  (lock (sb-thread:make-mutex :name "luv Objective-C trace")))

(defvar *objective-c-trace* nil
  "The current opt-in Objective-C trace, or NIL on the ordinary direct path.")

(defun objective-c-trace-now ()
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defmacro with-objective-c-trace-lock ((trace) &body body)
  #+sb-thread
  `(sb-thread:with-mutex ((%objective-c-trace-lock ,trace)) ,@body)
  #-sb-thread
  `(progn ,@body))

(defun make-objective-c-trace ()
  (%make-objective-c-trace :started-at (objective-c-trace-now)))

(defun objective-c-trace-events (trace)
  (with-objective-c-trace-lock (trace)
    (sort (coerce (copy-seq (%objective-c-trace-events trace)) 'list)
          #'< :key #'objective-c-message-event-sequence)))

(defun objective-c-trace-thread-name ()
  #+sb-thread
  (or (sb-thread:thread-name sb-thread:*current-thread*) "unnamed thread")
  #-sb-thread
  "single thread")

(defun snapshot-objective-c-value (value &optional (depth 0))
  (cond
    ((typep value 'objective-c-class)
     (list :objective-c-class
           :name (objective-c-class-name value)
           :pointer (cffi:pointer-address (%objective-c-class-pointer value))))
    ((typep value 'objective-c-object)
     (list :objective-c-object
           :class (objective-c-object-class-name value)
           :protocol (objective-c-object-protocol-name value)
           :ownership (objective-c-object-ownership value)
           :released (objective-c-object-released-p value)
           :pointer (cffi:pointer-address (%objective-c-object-pointer value))))
    ((cffi:pointerp value)
     (list :pointer (cffi:pointer-address value)))
    ((or (null value) (symbolp value) (numberp value) (characterp value))
     value)
    ((stringp value) (copy-seq value))
    ((>= depth 6) (list :object (type-of value)))
    ((consp value)
     (cons (snapshot-objective-c-value (car value) (1+ depth))
           (snapshot-objective-c-value (cdr value) (1+ depth))))
    ((vectorp value)
     (map 'vector
          (lambda (item)
            (snapshot-objective-c-value item (1+ depth)))
          value))
    (t
     (list :object (type-of value)
           (handler-case (princ-to-string value)
             (error () "<unprintable>"))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun objective-c-foreign-type (type)
    (case type
      ((:object :class :selector) :pointer)
      (otherwise type)))

  (defun validate-objective-c-message-declaration
      (name result-type ownership consumes-receiver-p arguments)
    (when (and (eq result-type :object) (null ownership))
      (error "Objective-C object result ~S must declare :OWNERSHIP." name))
    (when (and (not (eq result-type :object)) ownership)
      (error "Non-object Objective-C result ~S cannot declare :OWNERSHIP."
             name))
    (unless (member ownership '(nil :owned :borrowed))
      (error "Unknown Objective-C result ownership ~S in ~S." ownership name))
    (dolist (argument arguments)
      (unless (and (listp argument) (= (length argument) 2)
                   (symbolp (first argument)))
        (error "Malformed Objective-C argument ~S in ~S." argument name)))
    (when (and consumes-receiver-p (eq result-type :object))
      (error "Consuming Objective-C message ~S cannot also return an object."
             name)))

  (defun register-objective-c-message-definition
      (name selector result-type ownership result-class-name
       consumes-receiver-p arguments)
    (let ((definition
            (make-objective-c-message-definition
             :name name :selector selector
             :selector-pointer (%sel-register-name selector)
             :result-type result-type :result-ownership ownership
             :result-class-name result-class-name
             :consumes-receiver-p consumes-receiver-p
             :arguments (cons '(receiver :object) arguments))))
      (setf (gethash name *objective-c-message-definitions*) definition)
      definition))

  (defun objective-c-argument-form (name type)
    (case type
      ((:object :class) `(objective-c-pointer ,name))
      (otherwise name))))

(defun objective-c-message-description (name)
  "Return the selector, ABI, and ownership metadata for one declared message."
  (let ((definition
          (or (gethash name *objective-c-message-definitions*)
              (error "No Objective-C message named ~S." name))))
    (list :selector (objective-c-message-definition-selector definition)
          :result-type (objective-c-message-definition-result-type definition)
          :result-ownership
          (objective-c-message-definition-result-ownership definition)
          :result-class
          (objective-c-message-definition-result-class-name definition)
          :consumes-receiver
          (objective-c-message-definition-consumes-receiver-p definition)
          :argument-types
          (objective-c-message-definition-arguments definition))))

(defun objective-c-message-event-description (event)
  "Return one opt-in trace EVENT with its declared ABI metadata."
  (append (objective-c-message-description
           (objective-c-message-event-name event))
          (list :status (objective-c-message-event-status event)
                :arguments (objective-c-message-event-arguments event)
                :values (objective-c-message-event-values event)
                :condition (objective-c-message-event-condition event))))

(defvar *objective-c-exception-policy* :unchecked
  "How declared messages cross the native boundary: :CATCH or :UNCHECKED.

:UNCHECKED calls objc_msgSend directly and is the ordinary path.  :CATCH uses
the native NSInvocation exception bridge for an explicit diagnostic scope.")

(defmacro with-unchecked-objective-c-messages (() &body body)
  "Send declared messages in BODY directly, without catching NSException."
  `(let ((*objective-c-exception-policy* :unchecked))
     ,@body))

(defmacro with-objective-c-exception-handling (() &body body)
  "Catch NSException in BODY, even inside an unchecked dynamic context."
  `(let ((*objective-c-exception-policy* :catch))
     ,@body))

(defmacro with-objective-c-trace ((trace) &body body)
  "Run BODY with declared sends recorded as backend-local trace events."
  `(let* ((,trace (make-objective-c-trace))
          (*objective-c-trace* ,trace))
     ,@body))

(defvar *objective-c-message-send-pointer*
  (cffi:foreign-symbol-pointer "objc_msgSend"
                               :library 'objective-c-runtime-library)
  "The stable libobjc dispatch entry, resolved once when this runtime loads.

Resolving this symbol for every declared message used to put dlsym on the
per-frame path hundreds of times.  #WEE1DX")

(defgeneric check-consumable-objective-c-receiver (receiver)
  (:documentation "Validate that RECEIVER owns the retain a message consumes."))

(defmethod check-consumable-objective-c-receiver
    ((receiver objective-c-object))
  (unless (eq (objective-c-object-ownership receiver) :owned)
    (error 'objective-c-ownership-error :object receiver))
  (objective-c-pointer receiver))

(defun translate-objective-c-result (value definition)
  (if (eq (objective-c-message-definition-result-type definition) :object)
      (wrap-objective-c-object
       value
       :ownership
       (objective-c-message-definition-result-ownership definition)
       :protocol-name
       (objective-c-message-definition-result-class-name definition))
      value))

(defun call-with-objective-c-message-trace
    (trace definition arguments function)
  (let* ((started-at (objective-c-trace-now))
         (event
           (make-objective-c-message-event
            :sequence
            (with-objective-c-trace-lock (trace)
              (prog1 (%objective-c-trace-next-sequence trace)
                (incf (%objective-c-trace-next-sequence trace))))
            :timestamp (- started-at (%objective-c-trace-started-at trace))
            :thread (objective-c-trace-thread-name)
            :name (objective-c-message-definition-name definition)
            :arguments
            (mapcar (lambda (argument)
                      (list (first argument)
                            (snapshot-objective-c-value (second argument))))
                    arguments)
            :status :signaled)))
    (handler-bind
        ((error
           (lambda (condition)
             (setf (objective-c-message-event-condition event)
                   (list :type (type-of condition)
                         :message
                         (handler-case (princ-to-string condition)
                           (error () "<unprintable condition>")))))))
      (unwind-protect
           (let ((results (multiple-value-list (funcall function))))
             (setf (objective-c-message-event-status event) :returned
                   (objective-c-message-event-values event)
                   (mapcar #'snapshot-objective-c-value results))
             (values-list results))
        (setf (objective-c-message-event-duration event)
              (- (objective-c-trace-now) started-at))
        (with-objective-c-trace-lock (trace)
          (vector-push-extend event (%objective-c-trace-events trace)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun caught-objective-c-message-send-form
      (definition receiver result-type arguments)
    (let ((result-storage (gensym "RESULT-STORAGE"))
          (argument-array (gensym "ARGUMENT-ARRAY"))
          (argument-size-array (gensym "ARGUMENT-SIZE-ARRAY"))
          (argument-storages
            (loop repeat (length arguments) collect (gensym "ARGUMENT"))))
      (labels
          ((call-form (result result-size)
             (if arguments
                 `(cffi:with-foreign-objects
                      ((,argument-array :pointer ,(length arguments))
                       (,argument-size-array :size ,(length arguments)))
                    ,@(loop for storage in argument-storages
                            for index from 0
                            for (nil type) in arguments
                            collect
                            `(setf
                              (cffi:mem-aref ,argument-array :pointer ,index)
                              ,storage
                              (cffi:mem-aref ,argument-size-array :size ,index)
                              (cffi:foreign-type-size
                               ',(objective-c-foreign-type type))))
                    (call-with-objective-c-exception-boundary
                     ,definition ,receiver ,result ,result-size
                     ,argument-array ,argument-size-array ,(length arguments)))
                 `(call-with-objective-c-exception-boundary
                   ,definition ,receiver ,result ,result-size
                   (cffi:null-pointer) (cffi:null-pointer) 0)))
           (result-form ()
             (if (eq result-type :void)
                 `(progn
                    ,(call-form '(cffi:null-pointer) 0)
                    nil)
                 (let ((foreign-result-type
                         (objective-c-foreign-type result-type)))
                   `(cffi:with-foreign-object
                        (,result-storage ',foreign-result-type)
                      ,(call-form
                        result-storage
                        `(cffi:foreign-type-size ',foreign-result-type))
                      (cffi:mem-ref ,result-storage ',foreign-result-type)))))
           (argument-forms (remaining-arguments remaining-storages)
             (if remaining-arguments
                 (destructuring-bind ((name type) . tail)
                     remaining-arguments
                   `(cffi:with-foreign-object
                        (,(first remaining-storages)
                         ',(objective-c-foreign-type type))
                      (setf
                       (cffi:mem-ref
                        ,(first remaining-storages)
                        ',(objective-c-foreign-type type))
                       ,(objective-c-argument-form name type))
                      ,(argument-forms tail (rest remaining-storages))))
                 (result-form))))
        (argument-forms arguments argument-storages))))

  (defun unchecked-objective-c-message-send-form
      (definition receiver result-type arguments)
    `(cffi:foreign-funcall-pointer
      *objective-c-message-send-pointer* ()
      :pointer (objective-c-pointer ,receiver)
      :pointer
      (objective-c-message-definition-selector-pointer ,definition)
      ,@(loop for (argument-name argument-type) in arguments
              append
              (list (objective-c-foreign-type argument-type)
                    (objective-c-argument-form
                     argument-name argument-type)))
      ,(objective-c-foreign-type result-type))))

(defmacro define-objective-c-message
    (name (selector result-type &key ownership class consumes-receiver)
     &body arguments)
  "Define NAME as an inspectable, ABI-typed class and message-sending function."
  (validate-objective-c-message-declaration
   name result-type ownership consumes-receiver arguments)
  (let ((argument-names (mapcar #'first arguments))
        (definition-name
          (intern (format nil "*~A-DEFINITION*" name)
                  (symbol-package name))))
    `(progn
       (defparameter ,definition-name
         (register-objective-c-message-definition
          ',name ,selector ',result-type ',ownership ,class
          ,consumes-receiver ',arguments))
       (defun ,name (receiver ,@argument-names)
         ,(when consumes-receiver
            '(check-consumable-objective-c-receiver receiver))
         (flet ((send ()
                  (let ((result
                          (with-objective-c-native-environment
                            (ecase *objective-c-exception-policy*
                              (:catch
                               ,(caught-objective-c-message-send-form
                                 definition-name 'receiver result-type
                                 arguments))
                              (:unchecked
                               ,(unchecked-objective-c-message-send-form
                                 definition-name 'receiver result-type
                                 arguments))))))
                    ,(when consumes-receiver
                       '(setf (objective-c-object-released-p receiver) t))
                    (translate-objective-c-result result ,definition-name))))
           (if *objective-c-trace*
               (call-with-objective-c-message-trace
                *objective-c-trace* ,definition-name
                (list (list 'receiver receiver)
                      ,@(loop for argument-name in argument-names
                              collect `(list ',argument-name ,argument-name)))
                #'send)
               (send))))
       ',name)))
