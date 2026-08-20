(defpackage #:luv.spir-v
  (:nicknames #:spv)
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step)
  (:local-nicknames (#:lang #:luv.arithmetic.language))
  ;; These names are literal SPIR-V instructions rather than Common Lisp or
  ;; shared-shader operators.  They belong to this backend package.
  (:shadow #:dot #:function #:load #:return #:variable)
  (:documentation
   "Literal SPIR-V instructions, modules, and shader lowering.")
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
           #:lower-shader-statement
           #:shader-operator-result-name
           #:binary-arithmetic-instruction
           #:assemble-spir-v-module
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
