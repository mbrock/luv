(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; These portable operator names originally belonged to LUV.SPIR-V.  Remove
  ;; stale home symbols before importing their backend-neutral identities in a
  ;; live image; existing expression objects retain the old symbols and methods
  ;; until their definitions are reparsed.
  (let ((package (find-package '#:luv.spir-v)))
    (when package
      (dolist (name '("CLAMP" "MIX" "SMOOTHSTEP" "STEP" "NORMALIZE"
                      "DERIVATIVE-X" "DERIVATIVE-Y"
                      "QUANTITY" "ASSUME-QUANTITY" "INTERPRET"
                      "REPRESENTATION" "CONVERT-UNIT" "COUNTED-FOLD"))
        (multiple-value-bind (symbol status) (find-symbol name package)
          (when (and (member status '(:internal :external))
                     (eq (symbol-package symbol) package))
            (unintern symbol package)))))))

(defpackage #:luv.spir-v
  (:nicknames #:spv)
  (:use #:cl)
  (:import-from #:luv.arithmetic
                #:dot #:clamp #:mix #:smoothstep #:normalize)
  (:import-from #:luv.arithmetic.language
                #:quantity #:assume-quantity #:interpret #:representation
                #:convert-unit #:counted-fold)
  (:shadowing-import-from #:luv.arithmetic #:step)
  (:local-nicknames (#:math #:luv.arithmetic)
                    (#:lang #:luv.arithmetic.language))
  (:shadow #:function #:load #:return #:variable)
  (:export #:spir-v-error
           #:spir-v-error-form
           #:spir-v-error-reason
           #:spir-v-error-details
           #:define-instruction
           #:define-enumeration
           #:instruction
           #:instruction-class
           #:instruction-name
           #:instruction-opcode
           #:instruction-result-convention
           #:instruction-result-id
           #:instruction-result-type
           #:instruction-form
           #:parse-instruction
           #:parse-module
           #:assemble
           #:write-spir-v
           #:spir-v-module
           #:spir-v-module-version
           #:spir-v-module-generator
           #:spir-v-module-capabilities
           #:spir-v-module-extensions
           #:spir-v-module-extended-instruction-imports
           #:spir-v-extended-instruction-import
           #:spir-v-extended-instruction-import-result-id
           #:spir-v-extended-instruction-import-name
           #:spir-v-module-addressing-model
           #:spir-v-module-memory-model
           #:spir-v-module-entry-points
           #:spir-v-module-execution-modes
           #:spir-v-module-debug-instructions
           #:spir-v-module-annotations
           #:spir-v-module-global-declarations
           #:spir-v-module-function-definitions
           #:spir-v-entry-point
           #:spir-v-entry-point-execution-model
           #:spir-v-entry-point-function
           #:spir-v-entry-point-name
           #:spir-v-entry-point-interfaces
           #:spir-v-execution-mode
           #:spir-v-execution-mode-function
           #:spir-v-execution-mode-name
           #:spir-v-execution-mode-literals
           #:spir-v-function-definition
           #:spir-v-function-result-id
           #:spir-v-function-return-type
           #:spir-v-function-control
           #:spir-v-function-type
           #:spir-v-function-parameters
           #:spir-v-function-basic-blocks
           #:spir-v-basic-block
           #:spir-v-basic-block-label
           #:spir-v-basic-block-instructions
           #:lower-spir-v
           #:assemble-spir-v-module
           #:shader-language-error
           #:shader-language-error-form
           #:shader-language-error-reason
           #:shader-language-error-details
           #:shader-type
           #:shader-type-name
           #:shader-type-component-count
           #:shader-type-scalar-kind
           #:shader-type-opaque-kind
           #:shader-type-sample-result-type
           #:shader-type-image-depth-p
           #:find-shader-type
           #:shader-type=
           #:shader-object-name
           #:shader-object-source-form
           #:shader-variable-declaration
           #:shader-declaration-type
           #:shader-declaration-quantity-specification
           #:shader-declaration-quantity-layout
           #:shader-interface-variable
           #:shader-interface-direction
           #:shader-interface-location
           #:shader-interface-built-in
           #:shader-resource
           #:shader-resource-descriptor-set
           #:shader-resource-binding
           #:shader-resource-sample-quantity-specification
           #:shader-resource-sample-quantity-layout
           #:shader-resource-sample-transfer
           #:shader-uniform-block
           #:shader-uniform-block-members
           #:shader-uniform-block-byte-size
           #:shader-uniform-member
           #:shader-uniform-member-block
           #:shader-uniform-member-index
           #:shader-uniform-member-offset
           #:shader-task-payload
           #:shader-task-payload-fields
           #:shader-task-payload-field
           #:shader-task-payload-field-payload
           #:shader-task-payload-field-index
           #:shader-task-payload-field-element-count
           #:task-payload-definition-for
           #:define-task-payload
           #:shader-map-definition
           #:shader-projective-map-definition
           #:shader-map-domain-type
           #:shader-map-domain-quantity-specification
           #:shader-projective-map-homogeneous-type
           #:shader-projective-map-sample-type
           #:shader-projective-map-sample-quantity-layout
           #:shader-projective-map-coordinate-scale
           #:shader-projective-map-coordinate-offset
           #:shader-map-definition-for
           #:define-projective-shader-map
           #:shader-binding
           #:shader-binding-expression
           #:shader-function-parameter-binding
           #:shader-expression
           #:shader-expression-type
           #:shader-expression-quantity-specification
           #:shader-expression-quantity-layout
           #:shader-expression-quantity-checked-p
           #:shader-expression-materialized-p
           #:shader-expression-source-form
           #:shader-expression-name
           #:shader-literal
           #:shader-literal-value
           #:shader-reference
           #:shader-reference-target
           #:shader-call
           #:shader-call-operator
           #:shader-call-operands
           #:shader-call-parameters
           #:shader-function-definition
           #:shader-function
           #:shader-function-parameters
           #:shader-function-body
           #:shader-function-definition-for
           #:define-shader-function
           #:shader-function-call
           #:shader-function-call-definition
           #:shader-function-call-arguments
           #:shader-function-call-bindings
           #:shader-function-call-result
           #:shader-conditional
           #:shader-counted-fold
           #:shader-map-application
           #:shader-map-application-definition
           #:shader-map-application-point
           #:shader-map-application-rows
           #:shader-map-projection
           #:shader-map-projection-application
           #:shader-interpretation
           #:shader-interpretation-operand
           #:shader-quantity-construction
           #:shader-quantity-construction-operand
           #:shader-quantity-assumption
           #:shader-quantity-assumption-operand
           #:shader-representation
           #:shader-representation-operand
           #:shader-unit-conversion
           #:shader-unit-conversion-operand
           #:shader-unit-conversion-factor
           #:shader-expression-form
           #:shader-expression-children
           ;; The shader-language vocabulary.  Arithmetic operators are CL's
           ;; own symbols; these are the words CL does not have, plus the
           ;; protocol for defining and implementing new operators.
           #:dot
           #:sample
           #:sample-compare
           #:texel-load
           #:derivative-x
           #:derivative-y
           #:mix
           #:uint
           #:float
           #:uvec2
           #:uvec3
           #:uvec4
           #:vec2
           #:vec3
           #:vec4
           #:swizzle
           #:clamp
           #:smoothstep
           #:step
           #:normalize
           #:quantity
           #:assume-quantity
           #:interpret
           #:representation
           #:project-point
           #:project-sample
           #:convert-unit
           #:mod
           #:counted-fold
           #:payload-element
           #:set-output
           #:set-mesh-output-counts
           #:set-mesh-vertex
           #:set-mesh-primitive
           #:set-payload
           #:set-payload-element
           #:emit-mesh-workgroups
           #:shader-operator
           #:define-shader-operator
           #:shader-operator-p
           #:shader-abstraction
           #:define-shader-abstraction
           #:shader-abstraction-p
           #:shader-source-revision
           #:shader-abstraction-revision
           #:expand-shader-abstraction-call
           #:expand-shader-source-form
           #:shadow-depth-test
           #:shadow-visibility
           #:parse-shader-operator-call
           #:infer-shader-call-type
           #:lower-shader-call
           #:lower-shader-statement
           #:lower-shader-map-component-values
           #:shader-operator-result-name
           #:binary-arithmetic-instruction
           #:shader-output-assignment
           #:shader-statement
           #:shader-statement-source-form
           #:shader-statement-expressions
           #:shader-assignment-output
           #:shader-assignment-value
           #:shader-assignment-source-form
           #:shader-specification
           #:shader-specification-stage
           #:shader-specification-inputs
           #:shader-specification-outputs
           #:shader-specification-resources
           #:shader-specification-workgroup-size
           #:shader-specification-task-payload
           #:shader-specification-mesh-output
           #:shader-specification-bindings
           #:shader-specification-statements
           #:shader-specification-expressions
           #:shader-conditional-statement
           #:shader-conditional-statement-condition
           #:shader-conditional-statement-statements
           #:shader-mesh-output
           #:shader-mesh-output-topology
           #:shader-mesh-output-max-vertices
           #:shader-mesh-output-max-primitives
           #:shader-mesh-output-vertex-outputs
           #:shader-mesh-output-primitive-outputs
           #:shader-mesh-output-counts
           #:shader-mesh-output-vertex-count
           #:shader-mesh-output-primitive-count
           #:shader-mesh-vertex-store
           #:shader-mesh-vertex-store-index
           #:shader-mesh-vertex-store-values
           #:shader-mesh-primitive-store
           #:shader-mesh-primitive-store-index
           #:shader-mesh-primitive-store-indices
           #:shader-mesh-primitive-store-values
           #:shader-task-payload-store
           #:shader-task-payload-store-field
           #:shader-task-payload-store-index
           #:shader-task-payload-store-value
           #:shader-emit-mesh-workgroups
           #:shader-emit-mesh-workgroups-counts
           #:shader-payload-element
           #:shader-payload-element-field
           #:shader-payload-element-index
           #:shader-expression-uniformity
           #:shader-expression-workgroup-uniform-p
           #:define-shader
           #:define-shader-method
           #:shader-specification-for
           #:shader-definition-dependent
           #:make-shader-definition-dependent
           #:shader-definition-dependent-arguments
           #:shader-definition-change-pending-p
           #:shader-definition-change-snapshot
           #:acknowledge-shader-definition-change
           #:release-shader-definition-dependent
           #:parse-shader-specification
           #:lower-shader-specification
           #:compile-shader-specification
           #:shader-lowering
           #:shader-lowering-specification
           #:shader-lowering-module
           #:shader-lowering-expression-instructions
           #:shader-lowering-instruction-expressions
           #:shader-lowering-diagnostics
           #:shader-module
           #:assemble-shader-specification
           #:gradient-compute-module
           #:gradient-compute-shader))
