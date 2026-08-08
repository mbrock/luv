(defpackage #:luv.spir-v
  (:nicknames #:spv)
  (:use #:cl)
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
           #:gradient-compute-shader))
