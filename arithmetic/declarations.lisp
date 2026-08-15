;;; Backend-neutral declarations connect semantic quantity judgments to
;;; represented values without making the representation itself semantic.

(in-package #:luv.arithmetic)

(defgeneric declaration-representation-type (declaration)
  (:documentation
   "Return DECLARATION's backend or Common Lisp representation type, or NIL."))

(defgeneric declaration-quantity-specification (declaration)
  (:documentation
   "Return DECLARATION's homogeneous quantity specification, or NIL."))

(defgeneric declaration-quantity-layout (declaration)
  (:documentation
   "Return DECLARATION's heterogeneous quantity layout, or NIL."))

(defgeneric declaration-source-form (declaration)
  (:documentation "Return the source form which established DECLARATION."))

(defclass represented-value-declaration ()
  ((representation-type
    :initarg :representation-type
    :initform nil
    :reader declaration-representation-type)
   (quantity-specification
    :initarg :quantity-specification
    :initform nil
    :reader declaration-quantity-specification)
   (quantity-layout
    :initarg :quantity-layout
    :initform nil
    :reader declaration-quantity-layout)
   (source-form
    :initarg :source-form
    :initform nil
    :reader declaration-source-form))
  (:documentation
   "One represented value's machine form and optional semantic meaning.

The representation type and quantity judgment are deliberately parallel:
two declarations may both use VEC3 while denoting different quantities, and
one quantity may acquire different representations in different backends.
#OXBSAY"))

(defmethod initialize-instance :after
    ((declaration represented-value-declaration) &key)
  (let ((specification
          (declaration-quantity-specification declaration))
        (layout (declaration-quantity-layout declaration)))
    (unless (or (null specification)
                (typep specification 'quantity-specification))
      (error "A represented value needs a quantity specification, not ~S."
             specification))
    (unless (or (null layout) (typep layout 'quantity-layout))
      (error "A represented value needs a quantity layout, not ~S." layout))))

(defun make-represented-value-declaration
    (&key representation-type quantity-specification quantity-layout source-form)
  "Describe one represented value without wrapping any runtime occurrence."
  (make-instance 'represented-value-declaration
                 :representation-type representation-type
                 :quantity-specification quantity-specification
                 :quantity-layout quantity-layout
                 :source-form source-form))

(defun declaration-quantity-checked-p (declaration)
  "Whether DECLARATION states homogeneous or component quantity meaning."
  (or (declaration-quantity-specification declaration)
      (declaration-quantity-layout declaration)))

(defun make-declared-quantity-specification
    (options &key (default-tensor-order 0))
  "Parse one source declaration plist into a quantity specification.

Return NIL when OPTIONS contains no quantity attributes.  This is the common
source seam used by arithmetic parameters, shader interfaces, and semantic
storage declarations; callers retain ownership of their source-specific error
conditions and representation-derived tensor defaults."
  (destructuring-bind
      (&key (quantity nil quantity-supplied-p)
            (dimension nil dimension-supplied-p)
            (unit nil unit-supplied-p)
            (tensor-order default-tensor-order tensor-order-supplied-p)
            (affine-p nil affine-p-supplied-p)
            (character nil character-supplied-p))
      options
    (when (or quantity-supplied-p dimension-supplied-p unit-supplied-p
              tensor-order-supplied-p affine-p-supplied-p
              character-supplied-p)
      (apply
       #'make-quantity-specification quantity
       (append
        (and dimension-supplied-p (list :dimension dimension))
        (list :unit unit :tensor-order tensor-order)
        (and affine-p-supplied-p (list :affine-p affine-p))
        (and character-supplied-p (list :character character)))))))
