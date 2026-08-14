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
           #:unit-expression
           #:unit-expression-factors
           #:make-unit-expression
           #:unit-expression=
           #:unitless-p
           #:multiply-unit-expressions
           #:divide-unit-expressions
           #:quantity-specification
           #:make-quantity-specification
           #:quantity-specification-name
           #:quantity-specification-dimension
           #:quantity-specification-unit
           #:quantity-specification-tensor-order
           #:quantity-specification-affine-p
           #:quantity-specification=
           #:quantity-operation-error
           #:quantity-operation-error-operator
           #:quantity-operation-error-specifications
           #:quantity-operation-error-reason
           #:derive-quantity-specification))
