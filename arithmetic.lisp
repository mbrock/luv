;;; Semantic arithmetic independent of any one execution backend.
;;;
;;; Specifications are compile-time boundary objects.  Runtime scalar and
;;; vector lanes remain ordinary unboxed data; an arithmetic graph, field
;;; operation, or shader compiler asks this protocol whether its operations
;;; are meaningful before choosing a backend representation.

(in-package #:luv.arithmetic)

(defclass dimension ()
  ((factors
    :initarg :factors
    :reader dimension-factors))
  (:documentation
   "A canonical product of symbolic base dimensions raised to rational powers."))

(defun dimension-factor-name (factor)
  (let ((symbol (car factor)))
    (format nil "~A::~A"
            (or (and (symbol-package symbol)
                     (package-name (symbol-package symbol)))
                "")
            (symbol-name symbol))))

(defun canonical-dimension-factors (factors)
  (let ((powers (make-hash-table :test #'eq)))
    (dolist (factor factors)
      (let ((base (car factor))
            (exponent (if (and (consp (cdr factor))
                               (null (cddr factor)))
                          (second factor)
                          (cdr factor))))
        (unless (and (symbolp base) (rationalp exponent))
          (error "Invalid dimension factor ~S." factor))
        (incf (gethash base powers 0) exponent)))
    (sort (loop for base being the hash-keys of powers
                  using (hash-value exponent)
                unless (zerop exponent)
                  collect (cons base exponent))
          #'string< :key #'dimension-factor-name)))

(defun make-dimension (&optional designator)
  "Return a canonical dimension from NIL, one base symbol, or factor pairs.

Each factor pair has the form (BASE EXPONENT), where EXPONENT is rational."
  (etypecase designator
    (null (make-instance 'dimension :factors nil))
    (dimension designator)
    (symbol (make-instance 'dimension
                           :factors (list (cons designator 1))))
    (list (make-instance 'dimension
                         :factors (canonical-dimension-factors designator)))))

(defmethod print-object ((dimension dimension) stream)
  (print-unreadable-object (dimension stream :type t)
    (if (dimension-factors dimension)
        (format stream "~{~A~^ ~}"
                (mapcar (lambda (factor)
                          (if (= (cdr factor) 1)
                              (car factor)
                              (format nil "~A^~A" (car factor) (cdr factor))))
                        (dimension-factors dimension)))
        (write-string "1" stream))))

(defun dimension= (left right)
  (let ((left (make-dimension left))
        (right (make-dimension right)))
    (and (= (length (dimension-factors left))
            (length (dimension-factors right)))
         (every (lambda (left-factor right-factor)
                  (and (eq (car left-factor) (car right-factor))
                       (= (cdr left-factor) (cdr right-factor))))
                (dimension-factors left)
                (dimension-factors right)))))

(defun dimensionless-p (dimension)
  (null (dimension-factors (make-dimension dimension))))

(defun multiply-dimensions (left right)
  (make-dimension
   (append (dimension-factors (make-dimension left))
           (dimension-factors (make-dimension right)))))

(defun exponentiate-dimension (dimension exponent)
  (unless (rationalp exponent)
    (error "A dimension exponent must be rational, not ~S." exponent))
  (make-dimension
   (mapcar (lambda (factor)
             (list (car factor) (* exponent (cdr factor))))
           (dimension-factors (make-dimension dimension)))))

(defun divide-dimensions (numerator denominator)
  (multiply-dimensions numerator
                       (exponentiate-dimension denominator -1)))

(defclass unit-expression ()
  ((factors
    :initarg :factors
    :reader unit-expression-factors))
  (:documentation
   "A canonical symbolic unit product.  Distinct bases are not converted."))

(defun make-unit-expression (&optional designator)
  "Return a canonical exact unit from NIL, one unit symbol, or factor pairs.

This layer deliberately knows no scale conversions: :METRE and :KILOMETRE
remain different bases until an explicit conversion operation is introduced."
  (etypecase designator
    (null (make-instance 'unit-expression :factors nil))
    (unit-expression designator)
    (symbol (make-instance 'unit-expression
                           :factors (list (cons designator 1))))
    (list (make-instance 'unit-expression
                         :factors (canonical-dimension-factors designator)))))

(defmethod print-object ((unit unit-expression) stream)
  (print-unreadable-object (unit stream :type t)
    (if (unit-expression-factors unit)
        (format stream "~{~A~^ ~}"
                (mapcar (lambda (factor)
                          (if (= (cdr factor) 1)
                              (car factor)
                              (format nil "~A^~A" (car factor) (cdr factor))))
                        (unit-expression-factors unit)))
        (write-string "1" stream))))

(defun unit-expression= (left right)
  (let ((left (make-unit-expression left))
        (right (make-unit-expression right)))
    (and (= (length (unit-expression-factors left))
            (length (unit-expression-factors right)))
         (every (lambda (left-factor right-factor)
                  (and (eq (car left-factor) (car right-factor))
                       (= (cdr left-factor) (cdr right-factor))))
                (unit-expression-factors left)
                (unit-expression-factors right)))))

(defun unitless-p (unit)
  (null (unit-expression-factors (make-unit-expression unit))))

(defun multiply-unit-expressions (left right)
  (make-unit-expression
   (append (unit-expression-factors (make-unit-expression left))
           (unit-expression-factors (make-unit-expression right)))))

(defun exponentiate-unit-expression (unit exponent)
  (unless (rationalp exponent)
    (error "A unit exponent must be rational, not ~S." exponent))
  (make-unit-expression
   (mapcar (lambda (factor)
             (list (car factor) (* exponent (cdr factor))))
           (unit-expression-factors (make-unit-expression unit)))))

(defun divide-unit-expressions (numerator denominator)
  (multiply-unit-expressions
   numerator (exponentiate-unit-expression denominator -1)))

(defclass quantity-specification ()
  ((name
    :initarg :name
    :initform nil
    :reader quantity-specification-name)
   (dimension
    :initarg :dimension
    :reader quantity-specification-dimension)
   (unit
    :initarg :unit
    :reader quantity-specification-unit)
   (tensor-order
    :initarg :tensor-order
    :initform 0
    :reader quantity-specification-tensor-order)
   (affine-p
    :initarg :affine-p
    :initform nil
    :reader quantity-specification-affine-p))
  (:documentation
   "The semantic meaning of a value, separate from its machine representation."))

(defun make-quantity-specification
    (name &key dimension unit (tensor-order 0) affine-p)
  (unless (typep tensor-order '(integer 0 *))
    (error "A tensor order must be a non-negative integer, not ~S."
           tensor-order))
  (make-instance 'quantity-specification
                 :name name
                 :dimension (make-dimension dimension)
                 :unit (make-unit-expression unit)
                 :tensor-order tensor-order
                 :affine-p (not (null affine-p))))

(defmethod print-object ((specification quantity-specification) stream)
  (print-unreadable-object (specification stream :type t)
    (format stream "~S ~A [~A] order ~D~:[~; point~]"
            (quantity-specification-name specification)
            (quantity-specification-dimension specification)
            (quantity-specification-unit specification)
            (quantity-specification-tensor-order specification)
            (quantity-specification-affine-p specification))))

(defun quantity-specification= (left right)
  (and (eq (quantity-specification-name left)
           (quantity-specification-name right))
       (dimension= (quantity-specification-dimension left)
                   (quantity-specification-dimension right))
       (unit-expression= (quantity-specification-unit left)
                         (quantity-specification-unit right))
       (= (quantity-specification-tensor-order left)
          (quantity-specification-tensor-order right))
       (eq (quantity-specification-affine-p left)
           (quantity-specification-affine-p right))))

(define-condition quantity-operation-error (error)
  ((operator
    :initarg :operator
    :reader quantity-operation-error-operator)
   (specifications
    :initarg :specifications
    :reader quantity-operation-error-specifications)
   (reason
    :initarg :reason
    :reader quantity-operation-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Cannot derive ~S over quantity specifications ~S: ~A."
             (quantity-operation-error-operator condition)
             (quantity-operation-error-specifications condition)
             (quantity-operation-error-reason condition)))))

(defun quantity-operation-error (operator specifications reason)
  (error 'quantity-operation-error
         :operator operator :specifications specifications :reason reason))

(defun same-quantity-space-p (left right)
  (and (eq (quantity-specification-name left)
           (quantity-specification-name right))
       (dimension= (quantity-specification-dimension left)
                   (quantity-specification-dimension right))
       (= (quantity-specification-tensor-order left)
          (quantity-specification-tensor-order right))))

(defun derived-specification (dimension unit tensor-order)
  (make-quantity-specification nil
                               :dimension dimension
                               :unit unit
                               :tensor-order tensor-order))

(defun scalar-number-specification-p (specification)
  (and (null (quantity-specification-name specification))
       (dimensionless-p (quantity-specification-dimension specification))
       (unitless-p (quantity-specification-unit specification))
       (zerop (quantity-specification-tensor-order specification))
       (not (quantity-specification-affine-p specification))))

(defun additive-pair (operator left right)
  (unless (same-quantity-space-p left right)
    (quantity-operation-error operator (list left right)
                              :different-quantity-spaces))
  (unless (unit-expression= (quantity-specification-unit left)
                            (quantity-specification-unit right))
    (quantity-operation-error operator (list left right)
                              :different-units))
  (let ((left-point-p (quantity-specification-affine-p left))
        (right-point-p (quantity-specification-affine-p right)))
    (ecase operator
      (+
       (when (and left-point-p right-point-p)
         (quantity-operation-error operator (list left right)
                                   :cannot-add-points))
       (make-quantity-specification
        (quantity-specification-name left)
        :dimension (quantity-specification-dimension left)
        :unit (quantity-specification-unit left)
        :tensor-order (quantity-specification-tensor-order left)
        :affine-p (or left-point-p right-point-p)))
      (-
       (when (and (not left-point-p) right-point-p)
         (quantity-operation-error operator (list left right)
                                   :cannot-subtract-point-from-difference))
       (make-quantity-specification
        (quantity-specification-name left)
        :dimension (quantity-specification-dimension left)
        :unit (quantity-specification-unit left)
        :tensor-order (quantity-specification-tensor-order left)
        :affine-p (and left-point-p (not right-point-p)))))))

(defun product-tensor-order (operator left right)
  (let ((left-order (quantity-specification-tensor-order left))
        (right-order (quantity-specification-tensor-order right)))
    (cond ((zerop left-order) right-order)
          ((zerop right-order) left-order)
          ((= left-order right-order) left-order)
          (t
           (quantity-operation-error operator (list left right)
                                     :incompatible-tensor-orders)))))

(defun multiplicative-pair (operator left right)
  (when (or (quantity-specification-affine-p left)
            (quantity-specification-affine-p right))
    (quantity-operation-error operator (list left right)
                              :cannot-scale-affine-point))
  (cond ((scalar-number-specification-p left) right)
        ((scalar-number-specification-p right) left)
        (t
         (derived-specification
          (multiply-dimensions
           (quantity-specification-dimension left)
           (quantity-specification-dimension right))
          (multiply-unit-expressions
           (quantity-specification-unit left)
           (quantity-specification-unit right))
          (product-tensor-order operator left right)))))

(defun compatible-pair (operator left right)
  (unless (quantity-specification= left right)
    (quantity-operation-error operator (list left right)
                              (if (unit-expression=
                                   (quantity-specification-unit left)
                                   (quantity-specification-unit right))
                                  :incompatible-quantities
                                  :different-units)))
  left)

(defun interpret-quantity-specification (derived interpretation)
  "Give a compatible anonymous DERIVED specification an explicit meaning.

This is a semantic interpretation, never a numerical unit conversion.  An
already named quantity may only retain its name; anonymous derived results may
acquire one when their dimension, exact unit, tensor order, and affine
character agree with INTERPRETATION."
  (unless (and (or (null derived)
                   (null (quantity-specification-name derived))
                   (eq (quantity-specification-name derived)
                       (quantity-specification-name interpretation)))
               (or (null derived)
                   (and (dimension=
                         (quantity-specification-dimension derived)
                         (quantity-specification-dimension interpretation))
                        (unit-expression=
                         (quantity-specification-unit derived)
                         (quantity-specification-unit interpretation))
                        (= (quantity-specification-tensor-order derived)
                           (quantity-specification-tensor-order interpretation))
                        (eq (quantity-specification-affine-p derived)
                            (quantity-specification-affine-p interpretation)))))
    (quantity-operation-error 'interpret (list derived interpretation)
                              :incompatible-interpretation))
  interpretation)

(defgeneric derive-quantity-specification (operator &rest operands)
  (:documentation
   "Derive the semantic result of applying OPERATOR to quantity OPERANDS."))

(defmethod derive-quantity-specification (operator &rest operands)
  (quantity-operation-error operator operands :unknown-operator))

(defmethod derive-quantity-specification ((operator (eql '+)) &rest operands)
  (unless operands
    (quantity-operation-error operator operands :missing-operands))
  (reduce (lambda (left right) (additive-pair operator left right)) operands))

(defmethod derive-quantity-specification ((operator (eql '-)) &rest operands)
  (unless operands
    (quantity-operation-error operator operands :missing-operands))
  (if (= (length operands) 1)
      (let ((operand (first operands)))
        (when (quantity-specification-affine-p operand)
          (quantity-operation-error operator operands :cannot-negate-point))
        operand)
      (reduce (lambda (left right) (additive-pair operator left right))
              (rest operands) :initial-value (first operands))))

(defmethod derive-quantity-specification ((operator (eql '*)) &rest operands)
  (unless operands
    (quantity-operation-error operator operands :missing-operands))
  (reduce (lambda (left right)
            (multiplicative-pair operator left right))
          (rest operands) :initial-value (first operands)))

(defmethod derive-quantity-specification ((operator (eql '/)) &rest operands)
  (unless (= (length operands) 2)
    (quantity-operation-error operator operands :division-arity))
  (destructuring-bind (numerator denominator) operands
    (when (or (quantity-specification-affine-p numerator)
              (quantity-specification-affine-p denominator))
      (quantity-operation-error operator operands :cannot-divide-affine-point))
    (if (scalar-number-specification-p denominator)
        numerator
        (derived-specification
         (divide-dimensions
          (quantity-specification-dimension numerator)
          (quantity-specification-dimension denominator))
         (divide-unit-expressions
          (quantity-specification-unit numerator)
          (quantity-specification-unit denominator))
         (product-tensor-order operator numerator denominator)))))

(defmethod derive-quantity-specification ((operator (eql 'dot)) &rest operands)
  (unless (= (length operands) 2)
    (quantity-operation-error operator operands :dot-arity))
  (destructuring-bind (left right) operands
    (when (or (quantity-specification-affine-p left)
              (quantity-specification-affine-p right)
              (/= (quantity-specification-tensor-order left) 1)
              (/= (quantity-specification-tensor-order right) 1))
      (quantity-operation-error operator operands :dot-requires-vectors))
    (derived-specification
     (multiply-dimensions
      (quantity-specification-dimension left)
      (quantity-specification-dimension right))
     (multiply-unit-expressions
      (quantity-specification-unit left)
      (quantity-specification-unit right))
     0)))

(defmethod derive-quantity-specification ((operator (eql 'min)) &rest operands)
  (unless (>= (length operands) 2)
    (quantity-operation-error operator operands :missing-operands))
  (reduce (lambda (left right) (compatible-pair operator left right))
          (rest operands) :initial-value (first operands)))

(defmethod derive-quantity-specification ((operator (eql 'max)) &rest operands)
  (unless (>= (length operands) 2)
    (quantity-operation-error operator operands :missing-operands))
  (reduce (lambda (left right) (compatible-pair operator left right))
          (rest operands) :initial-value (first operands)))
