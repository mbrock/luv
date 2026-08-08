(defpackage #:luv.spir-v
  (:nicknames #:spv)
  (:use #:cl)
  (:export #:spir-v-error
           #:spir-v-error-form
           #:spir-v-error-reason
           #:spir-v-error-details
           #:define-instruction
           #:define-enumeration
           #:assemble
           #:write-spir-v
           #:gradient-compute-shader))
