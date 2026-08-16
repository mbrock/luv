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
  (:shadow #:step)
  (:documentation
   "Semantic specifications and dimensional algebra for compiled arithmetic.")
  (:export #:dot
           #:clamp
           #:mix
           #:smoothstep
           #:step
           #:normalize
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
           #:quantity-definition-non-negative-p
           #:quantity-definition-character
           #:quantity-definition-for
           #:define-quantity
           #:quantity-specification
           #:make-quantity-specification
           #:quantity-specification-name
           #:quantity-specification-dimension
           #:quantity-specification-unit
           #:quantity-specification-kind
           #:quantity-specification-tensor-order
           #:quantity-specification-character
           #:quantity-specification-affine-p
           #:quantity-specification-absolute-p
           #:quantity-specification-difference-p
           #:quantity-specification-non-negative-p
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
           #:repeated-quantity-layout
           #:make-repeated-quantity-layout
           #:repeated-quantity-layout-element-layout
           #:repeated-quantity-layout-stride
           #:make-declared-quantity-specification
           #:declaration-compatibility-error
           #:declaration-compatibility-error-actual
           #:declaration-compatibility-error-expected
           #:declaration-compatibility-error-reason
           #:ensure-declarations-compatible
           #:represented-value-declaration
           #:make-represented-value-declaration
           #:declaration-representation-type
           #:declaration-quantity-specification
           #:declaration-quantity-layout
           #:declaration-source-form
           #:declaration-quantity-checked-p
           #:value-declaration-for
           #:define-quantity-constant
           #:quantity-component-names
           #:project-quantity-specification
           #:quantity-operation-error
           #:quantity-operation-error-operator
           #:quantity-operation-error-specifications
           #:quantity-operation-error-reason
           #:interpret-quantity-specification
           #:convert-quantity-specification-unit
           #:derive-quantity-specification))
