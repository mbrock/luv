(defpackage #:luv.msl
  (:use #:cl)
  (:local-nicknames (#:spv #:luv.spir-v))
  (:documentation
   "Structured Metal Shading Language lowering for luv's shader graph.")
  (:export #:msl-target
           #:msl-target-language-version
           #:*metal-4-target*
           #:msl-source-occurrence
           #:msl-source-occurrence-expression
           #:msl-source-occurrence-text
           #:msl-field
           #:msl-field-type
           #:msl-field-name
           #:msl-field-attribute
           #:msl-structure-declaration
           #:msl-structure-name
           #:msl-structure-fields
           #:msl-parameter
           #:msl-parameter-type
           #:msl-parameter-name
           #:msl-parameter-attribute
           #:msl-variable-statement
           #:msl-variable-statement-type
           #:msl-variable-statement-name
           #:msl-variable-statement-value
           #:msl-output-statement
           #:msl-output-statement-field
           #:msl-output-statement-value
           #:msl-entry-point
           #:msl-entry-point-stage
           #:msl-entry-point-return-type
           #:msl-entry-point-name
           #:msl-entry-point-parameters
           #:msl-entry-point-statements
           #:msl-document
           #:msl-document-target
           #:msl-document-specification
           #:msl-document-declarations
           #:msl-document-entry-point
           #:msl-document-source
           #:msl-document-expression-occurrences
           #:msl-document-occurrence-expression
           #:compile-msl
           #:write-msl))
