;;; Quantity declarations on ordinary Common Lisp classes and structures.

(in-package #:luv.arithmetic.records)

(define-condition quantity-slot-conflict (error)
  ((class
    :initarg :class
    :reader quantity-slot-conflict-class)
   (slot-name
    :initarg :slot-name
    :reader quantity-slot-conflict-slot-name)
   (declarations
    :initarg :declarations
    :reader quantity-slot-conflict-declarations))
  (:report
   (lambda (condition stream)
     (format stream "Class ~S inherits conflicting quantity declarations for slot ~S."
             (class-name
              (quantity-slot-conflict-class condition))
             (quantity-slot-conflict-slot-name condition)))))

(defgeneric record-slot-name (declaration)
  (:documentation "Return the slot name described by DECLARATION."))

(defclass quantity-direct-slot-definition
    (closer-mop:standard-direct-slot-definition)
  ((quantity-options
    :initarg :quantity
    :reader quantity-slot-options)
   (quantity-specification
    :initform nil
    :accessor quantity-slot-specification)))

(defmethod initialize-instance :after
    ((slot quantity-direct-slot-definition) &key)
  (setf (quantity-slot-specification slot)
        (math:make-declared-quantity-specification
         (quantity-slot-options slot))))

(defvar *effective-quantity-slot* nil)

(defclass quantity-effective-slot-definition
    (closer-mop:standard-effective-slot-definition)
  ((quantity-specification
    :initform
    (math:declaration-quantity-specification *effective-quantity-slot*)
    :reader quantity-slot-specification)
   (quantity-source-form
    :initform (math:declaration-source-form *effective-quantity-slot*)
    :reader quantity-slot-source-form)))

(defclass quantity-class (closer-mop:standard-class) ()
  (:documentation
   "A standard class whose annotated slots retain quantity declarations.

Slot access and instance representation remain ordinary CLOS.  The metaclass
only makes definition-time meaning inspectable and inheritable.  #OXBSAY"))

(defmethod closer-mop:validate-superclass
    ((class quantity-class) (superclass closer-mop:standard-class))
  (declare (ignore class superclass))
  t)

(defun quantity-option-present-p (initargs)
  (loop for (key) on initargs by #'cddr
        thereis (eq key :quantity)))

(defmethod closer-mop:direct-slot-definition-class
    ((class quantity-class) &rest initargs)
  (if (quantity-option-present-p initargs)
      (find-class 'quantity-direct-slot-definition)
      (call-next-method)))

(defmethod math:declaration-representation-type
    ((slot quantity-direct-slot-definition))
  (closer-mop:slot-definition-type slot))

(defmethod math:declaration-quantity-specification
    ((slot quantity-direct-slot-definition))
  (quantity-slot-specification slot))

(defmethod math:declaration-quantity-layout
    ((slot quantity-direct-slot-definition))
  (declare (ignore slot))
  nil)

(defmethod math:declaration-source-form
    ((slot quantity-direct-slot-definition))
  (list (closer-mop:slot-definition-name slot)
        :type (closer-mop:slot-definition-type slot)
        :quantity (quantity-slot-options slot)))

(defmethod record-slot-name ((slot quantity-direct-slot-definition))
  (closer-mop:slot-definition-name slot))

(defmethod math:declaration-representation-type
    ((slot quantity-effective-slot-definition))
  (closer-mop:slot-definition-type slot))

(defmethod math:declaration-quantity-specification
    ((slot quantity-effective-slot-definition))
  (quantity-slot-specification slot))

(defmethod math:declaration-quantity-layout
    ((slot quantity-effective-slot-definition))
  (declare (ignore slot))
  nil)

(defmethod math:declaration-source-form
    ((slot quantity-effective-slot-definition))
  (quantity-slot-source-form slot))

(defmethod record-slot-name ((slot quantity-effective-slot-definition))
  (closer-mop:slot-definition-name slot))

(defun quantity-slot-declaration= (left right)
  (let ((left-specification
          (math:declaration-quantity-specification left))
        (right-specification
          (math:declaration-quantity-specification right)))
    (or (and (null left-specification) (null right-specification))
        (and left-specification right-specification
             (math:quantity-specification=
              left-specification right-specification)))))

(defmethod closer-mop:compute-effective-slot-definition
    ((class quantity-class) name direct-slots)
  (let ((quantity-slots
          (remove-if-not
           (lambda (slot) (typep slot 'quantity-direct-slot-definition))
           direct-slots)))
    (when (and quantity-slots
               (not (every (lambda (slot)
                             (quantity-slot-declaration=
                              (first quantity-slots) slot))
                           (rest quantity-slots))))
      (error 'quantity-slot-conflict
             :class class :slot-name name :declarations quantity-slots))
    (let ((*effective-quantity-slot* (first quantity-slots)))
      (call-next-method))))

(defmethod closer-mop:effective-slot-definition-class
    ((class quantity-class) &rest initargs)
  (declare (ignore initargs))
  (if *effective-quantity-slot*
      (find-class 'quantity-effective-slot-definition)
      (call-next-method)))

(defclass structure-slot-declaration (math:represented-value-declaration)
  ((record-name
    :initarg :record-name
    :reader structure-slot-record-name)
   (slot-name
    :initarg :slot-name
    :reader record-slot-name)))

(defclass structure-declaration ()
  ((name
    :initarg :name
    :reader structure-declaration-name)
   (slot-declarations
    :initarg :slot-declarations
    :reader structure-declaration-slot-declarations))
  (:documentation
   "The replaceable semantic schema emitted beside one ordinary DEFSTRUCT."))

(defgeneric structure-declaration-for (name)
  (:documentation "Return the semantic structure schema named by NAME, or NIL."))

(defmethod structure-declaration-for (name)
  (declare (ignore name))
  nil)

(defgeneric record-slot-declarations (record)
  (:documentation "Return every quantity-bearing slot declaration in RECORD."))

(defmethod record-slot-declarations ((record structure-declaration))
  (structure-declaration-slot-declarations record))

(defmethod record-slot-declarations ((class quantity-class))
  (unless (closer-mop:class-finalized-p class)
    (closer-mop:finalize-inheritance class))
  (remove-if-not
   (lambda (slot) (typep slot 'quantity-effective-slot-definition))
   (closer-mop:class-slots class)))

(defmethod record-slot-declarations ((name symbol))
  (let ((structure (structure-declaration-for name)))
    (cond (structure (record-slot-declarations structure))
          ((let ((class (find-class name nil)))
             (and (typep class 'quantity-class)
                  (record-slot-declarations class))))
          (t nil))))

(defun record-slot-declaration (record slot-name)
  "Return RECORD's quantity-bearing SLOT-NAME declaration, or NIL."
  (find slot-name (record-slot-declarations record)
        :key #'record-slot-name :test #'eq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun split-quantity-structure-slot (description)
    (when (symbolp description)
      (return-from split-quantity-structure-slot
        (values description nil)))
    (destructuring-bind (name &optional initform &rest options) description
      (when (oddp (length options))
        (error "Malformed DEFSTRUCT slot options in ~S." description))
      (let ((quantity-present-p nil)
            (quantity nil)
            (ordinary-options nil))
        (loop for (key value) on options by #'cddr do
          (if (eq key :quantity)
              (progn
                (when quantity-present-p
                  (error "Duplicate :QUANTITY option in ~S." description))
                (setf quantity-present-p t
                      quantity value))
              (setf ordinary-options
                    (append ordinary-options (list key value)))))
        (values
         `(,name ,initform ,@ordinary-options)
         (and quantity-present-p
              (list :name name
                    :representation-type (or (getf ordinary-options :type) t)
                    :quantity quantity
                    :source-form description)))))))

(defmacro define-quantity-struct (name-and-options &body slot-descriptions)
  "Define an ordinary structure whose annotated slots publish quantity meaning.

The runtime representation and accessors are still those of DEFSTRUCT. #FLRFU8"
  (let ((name (if (symbolp name-and-options)
                  name-and-options
                  (first name-and-options)))
        (ordinary-slots nil)
        (semantic-slots nil))
    (dolist (description slot-descriptions)
      (multiple-value-bind (ordinary semantic)
          (split-quantity-structure-slot description)
        (push ordinary ordinary-slots)
        (when semantic (push semantic semantic-slots))))
    (setf ordinary-slots (nreverse ordinary-slots)
          semantic-slots (nreverse semantic-slots))
    `(progn
       (defstruct ,name-and-options ,@ordinary-slots)
       (defmethod structure-declaration-for ((record-name (eql ',name)))
         (declare (ignore record-name))
         (load-time-value
          (make-instance
           'structure-declaration
           :name ',name
           :slot-declarations
           (list
            ,@(loop for slot in semantic-slots
                    collect
                    `(make-instance
                      'structure-slot-declaration
                      :record-name ',name
                      :slot-name ',(getf slot :name)
                      :representation-type ',(getf slot :representation-type)
                      :quantity-specification
                      (math:make-declared-quantity-specification
                       ',(getf slot :quantity))
                      :source-form ',(getf slot :source-form)))))))
       ',name)))
