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

(defmethod snapshot-invocation-object ((class objective-c-class))
  (list :objective-c-class
        :name (objective-c-class-name class)
        :pointer (cffi:pointer-address (%objective-c-class-pointer class))))

(defmethod snapshot-invocation-object ((object objective-c-object))
  (list :objective-c-object
        :class (objective-c-object-class-name object)
        :protocol (objective-c-object-protocol-name object)
        :ownership (objective-c-object-ownership object)
        :released (objective-c-object-released-p object)
        :pointer (cffi:pointer-address (%objective-c-object-pointer object))))

(defclass objective-c-message-class (invocation-class)
  ((selector :initform nil :accessor objective-c-message-selector)
   (selector-pointer :initform nil
                     :accessor objective-c-message-selector-pointer)
   (result-type :initform nil :accessor objective-c-message-result-type)
   (result-ownership :initform nil
                     :accessor objective-c-message-result-ownership)
   (result-class-name :initform nil
                      :accessor objective-c-message-result-class-name)
   (consumes-receiver-p :initform nil
                        :accessor objective-c-message-consumes-receiver-p)))

(defclass objective-c-message (invocation)
  ((receiver :initarg :receiver :reader objective-c-message-receiver))
  (:metaclass objective-c-message-class)
  (:documentation "One declared and ABI-typed Objective-C message send."))

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

  (defun configure-objective-c-message-class
      (class selector result-type ownership result-class-name
       consumes-receiver-p arguments)
    (setf (objective-c-message-selector class) selector
          (objective-c-message-selector-pointer class)
          (%sel-register-name selector)
          (objective-c-message-result-type class) result-type
          (objective-c-message-result-ownership class) ownership
          (objective-c-message-result-class-name class) result-class-name
          (objective-c-message-consumes-receiver-p class) consumes-receiver-p
          (invocation-class-arguments class)
          (cons '(receiver :object) arguments))
    class)

  (defun objective-c-argument-form (name type)
    (case type
      ((:object :class) `(objective-c-pointer ,name))
      (otherwise name))))

(defgeneric objective-c-message-class-of (designator)
  (:documentation "Resolve a message name, invocation, or metaobject to its class."))

(defmethod objective-c-message-class-of ((name symbol))
  (objective-c-message-class-of (find-class name)))

(defmethod objective-c-message-class-of ((message objective-c-message))
  (class-of message))

(defmethod objective-c-message-class-of ((class objective-c-message-class))
  class)

(defun objective-c-message-description (designator)
  "Return the selector, ABI, and ownership metadata for one declared message."
  (let ((class (objective-c-message-class-of designator)))
    (list :selector (objective-c-message-selector class)
          :result-type (objective-c-message-result-type class)
          :result-ownership (objective-c-message-result-ownership class)
          :result-class (objective-c-message-result-class-name class)
          :consumes-receiver
          (objective-c-message-consumes-receiver-p class)
          :argument-types (invocation-class-arguments class))))

(defun objective-c-invocation-description (invocation)
  "Return durable trace data plus declared ABI metadata for INVOCATION."
  (append (objective-c-message-description invocation)
          (list :status (invocation-status invocation)
                :arguments (invocation-arguments invocation)
                :values (invocation-values invocation)
                :condition (invocation-condition invocation))))

(defclass objective-c-runtime (invoker)
  ()
  (:documentation
   "A runtime that sends declared messages according to the dynamic policy."))

(defvar *objective-c-runtime* (make-instance 'objective-c-runtime)
  "The invoker through which every declared Objective-C message passes.")

(defvar *objective-c-exception-policy* :catch
  "How declared messages cross the native boundary: :CATCH or :UNCHECKED.

:CATCH uses NSInvocation and turns NSException into Lisp conditions.
:UNCHECKED calls objc_msgSend directly and must never encounter NSException.
See #P6RUG7.")

(defmacro with-unchecked-objective-c-messages (() &body body)
  "Send declared messages in BODY directly, without catching NSException."
  `(let ((*objective-c-exception-policy* :unchecked))
     ,@body))

(defmacro with-objective-c-exception-handling (() &body body)
  "Catch NSException in BODY, even inside an unchecked dynamic context."
  `(let ((*objective-c-exception-policy* :catch))
     ,@body))

(defun objective-c-message-send-pointer ()
  (cffi:foreign-symbol-pointer "objc_msgSend"
                               :library 'objective-c-runtime-library))

(defgeneric check-consumable-objective-c-receiver (receiver)
  (:documentation "Validate that RECEIVER owns the retain a message consumes."))

(defmethod check-consumable-objective-c-receiver
    ((receiver objective-c-object))
  (unless (eq (objective-c-object-ownership receiver) :owned)
    (error 'objective-c-ownership-error :object receiver))
  (objective-c-pointer receiver))

(defun translate-objective-c-result (value message-class)
  (if (eq (objective-c-message-result-type message-class) :object)
      (wrap-objective-c-object
       value
       :ownership (objective-c-message-result-ownership message-class)
       :protocol-name (objective-c-message-result-class-name message-class))
      value))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun caught-objective-c-message-send-form
      (message receiver result-type arguments)
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
                     ,message ,receiver ,result ,result-size
                     ,argument-array ,argument-size-array ,(length arguments)))
                 `(call-with-objective-c-exception-boundary
                   ,message ,receiver ,result ,result-size
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
      (message receiver result-type arguments)
    `(cffi:foreign-funcall-pointer
      (objective-c-message-send-pointer) ()
      :pointer (objective-c-pointer ,receiver)
      :pointer (objective-c-message-selector-pointer (class-of ,message))
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
  (let ((argument-names (mapcar #'first arguments)))
    `(progn
       (defclass ,name (objective-c-message)
         ,(loop for argument-name in argument-names
                collect `(,argument-name
                          :initarg ,(intern (symbol-name argument-name) :keyword)))
         (:metaclass objective-c-message-class))
       (eval-when (:load-toplevel :execute)
         (configure-objective-c-message-class
          (find-class ',name) ,selector ',result-type ',ownership ,class
          ,consumes-receiver ',arguments))
       (defmethod invoke ((runtime objective-c-runtime) (message ,name))
         (declare (ignore runtime))
         (with-slots (receiver ,@argument-names) message
           ,(when consumes-receiver
              '(check-consumable-objective-c-receiver receiver))
           (let ((result
                   (with-objective-c-native-environment
                     (ecase *objective-c-exception-policy*
                       (:catch
                        ,(caught-objective-c-message-send-form
                          'message 'receiver result-type arguments))
                       (:unchecked
                        ,(unchecked-objective-c-message-send-form
                          'message 'receiver result-type arguments))))))
             ,(when consumes-receiver
                '(setf (objective-c-object-released-p receiver) t))
             (translate-objective-c-result result (class-of message)))))
       (defun ,name (receiver ,@argument-names)
         (invoke
          *objective-c-runtime*
          (make-instance ',name :receiver receiver
                         ,@(loop for argument-name in argument-names
                                 append
                                 (list (intern (symbol-name argument-name)
                                               :keyword)
                                       argument-name)))))
       ',name)))

(defclass tracing-objective-c-runtime (tracing-invoker objective-c-runtime)
  ()
  (:documentation "An Objective-C runtime that records every declared send."))

(defmacro with-objective-c-trace ((trace) &body body)
  "Run BODY with this thread's declared messages recorded into TRACE."
  `(let* ((,trace (make-invocation-trace))
          (*objective-c-runtime*
            (make-instance 'tracing-objective-c-runtime :trace ,trace)))
     (unwind-protect (progn ,@body)
       (stop-invocation-trace ,trace))))
