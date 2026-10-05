(defpackage #:luv.hlsl
  (:use #:cl)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:lang #:luv.arithmetic.language))
  (:documentation
   "Structured HLSL lowering of luv's shader graph for DXC and Direct3D 12.")
  (:export #:hlsl-target
           #:hlsl-target-shader-model
           #:*shader-model-6-target*
           #:hlsl-profile
           #:hlsl-identifier
           #:hlsl-source-occurrence
           #:hlsl-source-occurrence-expression
           #:hlsl-source-occurrence-text
           #:hlsl-field
           #:hlsl-field-type
           #:hlsl-field-name
           #:hlsl-field-semantic
           #:hlsl-field-interpolation
           #:hlsl-field-origin
           #:hlsl-structure-declaration
           #:hlsl-structure-name
           #:hlsl-structure-fields
           #:hlsl-resource-declaration
           #:hlsl-resource-type
           #:hlsl-resource-name
           #:hlsl-resource-register
           #:hlsl-resource-origin
           #:hlsl-constant-buffer-declaration
           #:hlsl-constant-buffer-structure
           #:hlsl-parameter
           #:hlsl-parameter-type
           #:hlsl-parameter-name
           #:hlsl-parameter-semantic
           #:hlsl-parameter-origin
           #:hlsl-variable-statement
           #:hlsl-variable-statement-name
           #:hlsl-variable-statement-origin
           #:hlsl-output-statement
           #:hlsl-output-statement-origin
           #:hlsl-entry-point
           #:hlsl-entry-point-stage
           #:hlsl-entry-point-name
           #:hlsl-entry-point-return-type
           #:hlsl-entry-point-parameters
           #:hlsl-entry-point-statements
           #:hlsl-document
           #:hlsl-document-target
           #:hlsl-document-specification
           #:hlsl-document-declarations
           #:hlsl-document-entry-point
           #:hlsl-document-profile
           #:hlsl-document-source
           #:hlsl-document-expression-occurrences
           #:hlsl-document-occurrence-expression
           #:compile-hlsl
           #:write-hlsl))
