(defpackage #:luv.wiki
  (:use #:cl)
  (:documentation
   "The opinionated luv web framework: Org and Lisp-source pages as objects,
Spinneret rendering, S-expression CSS and JavaScript, shared web resources,
and equivalent dynamic Clack hosting and static publication.")
  (:export
   ;; Element model
   #:element
   #:element-children
   #:document
   #:document-title
   #:document-pathname
   #:document-name
   #:document-keywords
   #:document-figures
   #:heading
   #:heading-level
   #:heading-title
   #:heading-keyword
   #:heading-properties
   #:heading-id
   #:heading-document
   #:paragraph
   #:plain-list
   #:plain-list-ordered-p
   #:list-item
   #:example-block
   #:block-text
   #:src-block
   #:src-block-language
   #:table
   #:table-rows
   #:table-header-p
   ;; Inline objects
   #:inline-object
   #:emphasis
   #:emphasis-kind
   #:link
   #:link-protocol
   #:link-path
   #:link-search
   #:mention
   #:mention-id
   #:math
   #:math-text
   #:math-display-p
   #:reference-id
   ;; Reading
   #:read-org-file
   #:read-org-string
   #:*work-mark-keywords*
   #:map-elements
   ;; Corpus and site
   #:site
   #:make-site
   #:site-documents
   #:site-figures
   #:site-backlinks
   #:find-figure
   #:figure-href
   #:document-mentions
   #:dangling-mentions
   #:*site*
   #:render-html
   #:render-page
   #:render-document-string
   #:write-site
   #:site-page-name
   ;; Lisp source
   #:lisp-node
   #:node-start
   #:node-end
   #:node-text
   #:lisp-list
   #:lisp-vector
   #:lisp-symbol
   #:lisp-symbol-name
   #:lisp-symbol-package
   #:lisp-atom
   #:lisp-string
   #:lisp-number
   #:lisp-character
   #:lisp-prefix
   #:lisp-conditional
   #:lisp-comment
   #:lisp-skipped
   #:read-lisp-string
   #:read-lisp-file
   #:definition
   #:definition-kind
   #:definition-name
   #:definition-qualifiers
   #:definition-line
   #:definition-mentions
   #:definition-node
   #:file-definitions
   #:find-definition
   #:site-definitions
   #:site-code-references
   #:dangling-code-mentions
   #:render-lisp-source
   #:render-definition
   #:code-source-files
   #:code-definitions
   #:code-sources
   #:source-file
   #:source-file-pathname
   #:source-file-relative-path
   #:source-file-system-name
   #:source-file-nodes
   #:source-file-definitions
   #:read-source-file
   #:source-page-name
   #:site-source-files
   #:site-resources
   #:find-named-definition
   #:definition-page-href
   #:render-source-page
   #:render-source-index
   #:*code-systems*
   ;; Web resources
   #:resource
   #:resource-path
   #:resource-output-path
   #:resource-content-type
   #:resource-label
   #:resource-description
   #:resource-kind
   #:generated-resource
   #:file-resource
   #:make-generated-resource
   #:make-file-resource
   #:register-resource-provider
   #:website-resources
   #:website-navigation
   #:find-resource
   #:resource-response
   #:publish-site
   #:serve-site
   ;; ASDF
   #:org-file
   #:org-file-document
   #:render-op
   #:site-output-directory))

(defpackage #:luv.wiki.browser
  (:use #:cl)
  (:local-nicknames (#:ps #:parenscript))
  (:export #:await #:async-defun #:async-lambda))

(defpackage #:luv.css
  (:use #:cl)
  (:shadow #:declaration)
  (:documentation
   "A small CSS compiler: a reader syntax for quantities and --references, a
tree of style groups, rules, at-rules, declarations, selectors, and values
built by DEFINE-STYLE and RULE, and WRITE-CSS, the text backend over it.")
  (:export
   ;; Reader syntax
   #:syntax
   ;; Values
   #:dimension #:dimension-number #:dimension-unit
   #:slash #:slash-left #:slash-right
   #:variable-reference #:variable-reference-name #:var
   #:function-call #:function-call-name #:function-call-arguments
   #:define-css-function
   #:clamp #:minmax #:repeat #:fit-content #:calc #:url
   #:rgb #:rgba #:hsl #:attr #:translate #:scale #:rotate
   #:color-mix #:font-stack #:comma-list #:quoted
   ;; Selectors
   #:selector #:complex-selector #:selector-list #:selector-text
   #:parse-selector #:combine-selectors
   ;; The tree
   #:node #:declaration #:declaration-property #:declaration-values
   #:container #:container-children
   #:rule #:rule-selector
   #:at-rule #:at-rule-name #:at-rule-prelude
   #:style #:style-name #:style-documentation
   #:add-child #:add-item
   #:make-rule #:make-at-rule #:ensure-style #:find-style #:*styles*
   #:define-style #:declarations #:custom-property
   ;; Text backend
   #:write-css #:write-css-value #:css-value-text #:css-text #:stylesheet-text))

(defpackage #:luv.wiki.style
  (:use #:cl #:luv.css)
  (:shadowing-import-from #:luv.css #:declaration)
  (:documentation "The wiki's stylesheet, as LUV.CSS style definitions."))
