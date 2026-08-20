(defpackage #:luv.wgsl
  (:use #:cl)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:lang #:luv.arithmetic.language))
  (:documentation
   "Structured WebGPU Shading Language lowering for luv's shader graph.")
  (:export #:wgsl-target
           #:wgsl-target-overrides
           #:wgsl-source-occurrence
           #:wgsl-source-occurrence-expression
           #:wgsl-source-occurrence-text
           #:wgsl-override
           #:wgsl-override-name
           #:wgsl-override-identifier
           #:wgsl-override-type
           #:wgsl-override-default
           #:wgsl-document
           #:wgsl-document-target
           #:wgsl-document-specification
           #:wgsl-document-source
           #:wgsl-document-overrides
           #:wgsl-document-expression-occurrences
           #:wgsl-document-occurrence-expression
           #:compile-wgsl
           #:write-wgsl))
