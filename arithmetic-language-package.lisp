(defpackage #:luv.arithmetic.language
  (:use #:cl)
  (:local-nicknames (#:math #:luv.arithmetic))
  (:import-from #:luv.arithmetic
                #:dot #:clamp #:mix #:smoothstep #:normalize)
  (:shadowing-import-from #:luv.arithmetic #:step)
  (:export #:arithmetic-language-error
           #:arithmetic-language-error-form
           #:arithmetic-language-error-reason
           #:arithmetic-language-error-details
           #:arithmetic-named-object
           #:arithmetic-object-name
           #:arithmetic-object-source-form
           #:arithmetic-parameter
           #:arithmetic-parameter-quantity-specification
           #:arithmetic-parameter-quantity-layout
           #:arithmetic-binding
           #:arithmetic-binding-expression
           #:arithmetic-expression
           #:arithmetic-expression-quantity-specification
           #:arithmetic-expression-quantity-layout
           #:arithmetic-expression-source-form
           #:arithmetic-expression-name
           #:arithmetic-literal
           #:arithmetic-literal-value
           #:arithmetic-reference
           #:arithmetic-reference-target
           #:arithmetic-call
           #:arithmetic-call-operator
           #:arithmetic-call-operands
           #:arithmetic-call-parameters
           #:arithmetic-quantity-boundary
           #:arithmetic-quantity-boundary-operand
           #:arithmetic-quantity-construction
           #:arithmetic-quantity-assumption
           #:arithmetic-interpretation
           #:arithmetic-representation
           #:arithmetic-unit-conversion
           #:arithmetic-unit-conversion-operand
           #:arithmetic-unit-conversion-factor
           #:arithmetic-expression-quantity-checked-p
           #:arithmetic-expression-form
           #:arithmetic-expression-children
           #:arithmetic-reference-target-name
           #:arithmetic-reference-target-quantity-checked-p
           #:arithmetic-reference-target-quantity-specification
           #:arithmetic-reference-target-quantity-layout
           #:arithmetic-function-definition
           #:arithmetic-function-parameters
           #:arithmetic-function-bindings
           #:arithmetic-function-result
           #:arithmetic-function-expressions
           #:arithmetic-function-definition-for
           #:define-arithmetic-function
           #:parse-arithmetic-function-definition
           #:parse-arithmetic-expression
           #:parse-arithmetic-operator-call
           #:infer-arithmetic-call-quantity-specification
           #:arithmetic-operator-p
           #:define-arithmetic-operator
           #:arithmetic-constant-expression-p
           #:quantity
           #:assume-quantity
           #:interpret
           #:representation
           #:convert-unit))
