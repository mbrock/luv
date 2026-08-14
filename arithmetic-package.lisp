(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; DEFPACKAGE warns instead of retracting stale exports in a live image.
  (let ((package (find-package '#:luv.arithmetic)))
    (when package
      (multiple-value-bind (symbol status)
          (find-symbol "DEFINE-QUANTITY-COMPONENTS" package)
        (when (eq status :external)
          (unexport symbol package))))))

(defpackage #:luv.arithmetic
  (:use #:cl)
  (:documentation
   "Semantic specifications and dimensional algebra for compiled arithmetic.")
  (:export #:dot
           #:dimension
           #:dimension-factors
           #:make-dimension
           #:dimension=
           #:dimensionless-p
           #:multiply-dimensions
           #:divide-dimensions
           #:exponentiate-dimension
           #:unit-definition
           #:unit-definition-name
           #:unit-definition-dimension
           #:unit-definition-magnitude
           #:unit-definition-basis
           #:unit-definition-identity-p
           #:unit-definition-quantity-kind
           #:unit-definition-for
           #:define-unit
           #:undefined-unit
           #:undefined-unit-name
           #:unit-expression
           #:unit-expression-factors
           #:make-unit-expression
           #:unit-expression=
           #:unitless-p
           #:unit-expression-dimension
           #:unit-expression-magnitude
           #:unit-expression-basis
           #:unit-conversion-factor
           #:multiply-unit-expressions
           #:divide-unit-expressions
           #:quantity-kind-definition
           #:quantity-kind-definition-name
           #:quantity-kind-definition-parent
           #:quantity-kind-definition-dimension
           #:quantity-kind-definition-for
           #:define-quantity-kind
           #:quantity-kind-subkind-p
           #:quantity-definition
           #:quantity-definition-name
           #:quantity-definition-kind
           #:quantity-definition-components
           #:quantity-definition-for
           #:define-quantity
           #:quantity-specification
           #:make-quantity-specification
           #:quantity-specification-name
           #:quantity-specification-dimension
           #:quantity-specification-unit
           #:quantity-specification-kind
           #:quantity-specification-tensor-order
           #:quantity-specification-affine-p
           #:quantity-specification=
           #:dimensionless-quantity-specification-p
           #:quantity-projection
           #:make-quantity-projection
           #:quantity-projection-positions
           #:quantity-projection-specification
           #:quantity-layout
           #:make-quantity-layout
           #:quantity-layout-extent
           #:quantity-layout-projections
           #:quantity-layout=
           #:project-quantity-layout
           #:quantity-component-names
           #:project-quantity-specification
           #:quantity-operation-error
           #:quantity-operation-error-operator
           #:quantity-operation-error-specifications
           #:quantity-operation-error-reason
           #:interpret-quantity-specification
           #:convert-quantity-specification-unit
           #:derive-quantity-specification))
