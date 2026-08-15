;;;; A small CSS compiler: the stylesheet as Lisp definitions.
;;;;
;;;; The site's stylesheet is written as DEFINE-STYLE forms and compiled to
;;;; CSS text when the site is rendered, the way the shader language compiles
;;;; to SPIR-V and MSL from forms.  So the rules that draw a layout role can
;;;; sit beside the layout that assigns it, be re-evaluated in a running
;;;; image, and be computed by ordinary Lisp.
;;;;
;;;; The compiler has three parts.  A reader syntax (the LUV.CSS:SYNTAX
;;;; readtable) reads CSS quantities and custom-property references as
;;;; objects: 0.85rem and 100% are DIMENSIONs, 0.72rem/1 a SLASH pair, and
;;;; --ink a VARIABLE-REFERENCE, so no number-and-unit symbols are interned.
;;;; A syntax tree of CLOS instances -- STYLE, RULE, AT-RULE, DECLARATION,
;;;; selectors, and values -- built by the RULE and DEFINE-STYLE macros, whose
;;;; bodies alternate :property keywords with values, where bare symbols are
;;;; CSS words (grid, solid, inherit), strings are written verbatim, and
;;;; lists are Lisp: (clamp 1rem 4vw 3rem) calls the CSS-function CLAMP,
;;;; (gutter) calls a helper of your own, and a form may return values,
;;;; declarations, or rules, which join the enclosing rule.  Nested rules are
;;;; ("selector" . body) with & standing for the parent, and (:media "query"
;;;; . body) is an at-rule.  Finally WRITE-CSS is the text backend, a generic
;;;; over the tree; another backend can walk the same objects.

(in-package #:luv.css)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun css-name (symbol)
    "The CSS spelling of SYMBOL: its name in lowercase, unless the name was
escaped to keep lowercase letters, when it is kept as written."
    (let ((name (symbol-name symbol)))
      (if (some #'lower-case-p name) name (string-downcase name)))))

;;; Values

(defclass dimension ()
  ((number :initarg :number :reader dimension-number)
   (unit :initarg :unit :reader dimension-unit))
  (:documentation "A number with a unit: 0.85rem, 100%, 0.15s.  Read from
0.85rem by the CSS syntax; plain numbers stay numbers."))

(defclass slash ()
  ((left :initarg :left :reader slash-left)
   (right :initarg :right :reader slash-right))
  (:documentation "Two values joined by a slash, as in the font shorthand
0.72rem/1 or grid-area 1/3."))

(defclass variable-reference ()
  ((name :initarg :name :reader variable-reference-name))
  (:documentation "A reference to the custom property --NAME, written var(--name).
Read from --name by the CSS syntax, or made by VAR."))

(defclass function-call ()
  ((name :initarg :name :reader function-call-name)
   (arguments :initarg :arguments :reader function-call-arguments))
  (:documentation "A CSS function call: NAME(ARGUMENT, ARGUMENT).  An argument
that is a list is a space-separated run."))

(defclass comma-list ()
  ((values :initarg :values :reader comma-list-values))
  (:documentation "Comma-separated values, as in a font stack."))

(defclass quoted-string ()
  ((text :initarg :text :reader quoted-string-text))
  (:documentation "A CSS string literal, written in double quotes.  A Lisp
string is written verbatim, so this is how a font name or content is quoted."))

(defmethod print-object ((object dimension) stream)
  (print-unreadable-object (object stream :type t)
    (write-css-value object stream)))

(defmethod print-object ((object variable-reference) stream)
  (print-unreadable-object (object stream :type t)
    (write-css-value object stream)))

(defmethod print-object ((object slash) stream)
  (print-unreadable-object (object stream :type t)
    (write-css-value object stream)))

;; The reader returns these as literal objects, so compiled files must dump them.
(defmethod make-load-form ((object dimension) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod make-load-form ((object slash) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmethod make-load-form ((object variable-reference) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defun dimension (number unit)
  (make-instance 'dimension :number number :unit (string-downcase (string unit))))

(defun var (name)
  "A reference to the custom property NAME, a string or symbol without the --."
  (make-instance 'variable-reference :name (string-downcase (string name))))

(defun quoted (text)
  "TEXT as a CSS string literal."
  (make-instance 'quoted-string :text text))

(defun comma-list (&rest values)
  (make-instance 'comma-list :values values))

(defmacro define-css-function (name &optional (css-name (css-name name)))
  "Define NAME as a Lisp function building the CSS function call CSS-NAME(...)."
  `(defun ,name (&rest arguments)
     (make-instance 'function-call :name ,css-name :arguments arguments)))

(define-css-function clamp)
(define-css-function minmax)
(define-css-function repeat)
(define-css-function fit-content)
(define-css-function calc)
(define-css-function url)
(define-css-function rgb)
(define-css-function rgba)
(define-css-function hsl)
(define-css-function attr)
(define-css-function translate)
(define-css-function scale)
(define-css-function rotate)

(defun color-mix (&rest colors)
  "The CSS color-mix() of COLORS in the sRGB space, a percentage after a
colour weighting it: (color-mix --paper 94% --accent)."
  (let ((groups '()))
    (dolist (item colors)
      (if (and (typep item 'dimension) (string= (dimension-unit item) "%") groups)
          (setf (first groups) (append (first groups) (list item)))
          (push (list item) groups)))
    (make-instance 'function-call
                   :name "color-mix"
                   :arguments (list* '(in srgb) (nreverse groups)))))

(defun font-stack (&rest families)
  "A comma-separated font-family list; strings are quoted, symbols bare."
  (apply #'comma-list
         (mapcar (lambda (family) (if (stringp family) (quoted family) family))
                 families)))

;;; Reader syntax

(defun read-token-text (stream)
  "The characters of the token starting at STREAM, up to whitespace or a
terminating macro character."
  (with-output-to-string (out)
    (loop for char = (peek-char nil stream nil nil)
          while (and char
                     (not (member char '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)))
                     (not (find char "\"'(),;`")))
          do (write-char (read-char stream) out))))

(defun read-standard-object (text)
  "TEXT read with the standard syntax in the current package."
  (let ((*readtable* (named-readtables:find-readtable :standard)))
    (values (read-from-string text))))

(defun parse-number (text)
  "TEXT as a number, or NIL when it is not one."
  (and (plusp (length text))
       (every (lambda (char) (or (digit-char-p char) (find char "+-.eE/"))) text)
       (let ((object (ignore-errors (read-standard-object text))))
         (and (numberp object) object))))

(defun parse-quantity (text)
  "TEXT as a number or DIMENSION, or NIL."
  (or (parse-number text)
      (let ((unit-start (let ((position (position-if-not (lambda (char) (or (alpha-char-p char) (char= char #\%)))
                                                         text :from-end t)))
                          (and position (1+ position)))))
        (and unit-start
             (< unit-start (length text))
             (let ((number (parse-number (subseq text 0 unit-start))))
               (and number (dimension number (subseq text unit-start))))))))

(defun parse-css-token (text)
  "The object TEXT denotes under the CSS syntax: a number, a DIMENSION, a
SLASH of two quantities, a VARIABLE-REFERENCE for --name, or else what the
standard reader makes of it."
  (or (parse-quantity text)
      (let ((slash (position #\/ text)))
        (and slash
             (let ((left (parse-quantity (subseq text 0 slash)))
                   (right (parse-quantity (subseq text (1+ slash)))))
               (and left right (make-instance 'slash :left left :right right)))))
      (and (> (length text) 2)
           (string= "--" text :end2 2)
           (var (subseq text 2)))
      (read-standard-object text)))

(defun read-css-token (stream char)
  "Reader macro function for the digits and hyphen under the CSS syntax."
  (unread-char char stream)
  (parse-css-token (read-token-text stream)))

(named-readtables:defreadtable syntax
  (:merge :standard)
  (:macro-char #\0 'read-css-token t)
  (:macro-char #\1 'read-css-token t)
  (:macro-char #\2 'read-css-token t)
  (:macro-char #\3 'read-css-token t)
  (:macro-char #\4 'read-css-token t)
  (:macro-char #\5 'read-css-token t)
  (:macro-char #\6 'read-css-token t)
  (:macro-char #\7 'read-css-token t)
  (:macro-char #\8 'read-css-token t)
  (:macro-char #\9 'read-css-token t)
  (:macro-char #\- 'read-css-token t))

;;; Selectors

(defclass selector () ())

(defclass complex-selector (selector)
  ((text :initarg :text :reader selector-text))
  (:documentation "One selector as CSS text, ".door > .title"; & in it stands
for the parent selector when nested."))

(defclass selector-list (selector)
  ((selectors :initarg :selectors :reader selector-list-selectors))
  (:documentation "A comma-separated group of selectors, matching any of them."))

(defgeneric parse-selector (designator)
  (:documentation "The selector DESIGNATOR names: a string is one selector, a
list of strings a group.")
  (:method ((designator selector)) designator)
  (:method ((designator string)) (make-instance 'complex-selector :text designator))
  (:method ((designator list))
    (make-instance 'selector-list :selectors (mapcar #'parse-selector designator))))

(defgeneric selector-alternatives (selector)
  (:documentation "The complex selectors SELECTOR is a group of.")
  (:method ((selector complex-selector)) (list selector))
  (:method ((selector selector-list))
    (mapcan #'selector-alternatives (selector-list-selectors selector))))

(defmethod selector-text ((selector selector-list))
  (format nil "~{~A~^, ~}" (mapcar #'selector-text (selector-alternatives selector))))

(defgeneric combine-selectors (parent child)
  (:documentation "CHILD nested under PARENT: each alternative of CHILD against
each of PARENT, & standing for the parent, otherwise a descendant.")
  (:method ((parent null) child) child)
  (:method ((parent complex-selector) (child complex-selector))
    (let ((parent-text (selector-text parent))
          (child-text (selector-text child)))
      (make-instance 'complex-selector
                     :text (if (search "&" child-text)
                               (replace-all child-text "&" parent-text)
                               (concatenate 'string parent-text " " child-text)))))
  (:method ((parent selector) (child selector))
    (make-instance 'selector-list
                   :selectors (loop for p in (selector-alternatives parent)
                                    append (loop for c in (selector-alternatives child)
                                                 collect (combine-selectors p c))))))

(defun replace-all (string part replacement)
  (with-output-to-string (out)
    (loop with start = 0
          for position = (search part string :start2 start)
          do (write-string string out :start start :end (or position (length string)))
          while position
          do (write-string replacement out)
             (setf start (+ position (length part))))))

;;; The tree

(defclass node () ()
  (:documentation "Anything in a stylesheet: a declaration, rule, at-rule, or style group."))

(defclass declaration (node)
  ((property :initarg :property :reader declaration-property)
   (values :initarg :values :initform '() :accessor declaration-values))
  (:documentation "PROPERTY: VALUES; -- the property a keyword, the values a
list written space-separated."))

(defclass container (node)
  ((children :initform '() :accessor container-children))
  (:documentation "A node holding declarations, rules, and at-rules in order."))

(defclass rule (container)
  ((selector :initarg :selector :reader rule-selector))
  (:documentation "SELECTOR { declarations }, and nested rules and at-rules
whose selectors combine with this one."))

(defclass at-rule (container)
  ((name :initarg :name :reader at-rule-name)
   (prelude :initarg :prelude :reader at-rule-prelude))
  (:documentation "@NAME PRELUDE { children }.  Inside a rule, its
declarations apply to that rule's selector under the query."))

(defclass style (container)
  ((name :initarg :name :reader style-name)
   (documentation :initarg :documentation :initform nil :accessor style-documentation))
  (:documentation "A named group of rules, compiled in definition order under
its documentation as a comment."))

(defgeneric add-child (container node)
  (:documentation "Append NODE to CONTAINER's children.")
  (:method ((container container) (node node))
    (push node (container-children container)))
  (:method ((container style) (node declaration))
    (error "A declaration ~S outside any rule in style ~A."
           node (style-name container))))

(defgeneric add-item (container item declaration)
  (:documentation "Add ITEM, one evaluated form of a rule body, to CONTAINER.
DECLARATION is the open declaration values append to; the value returned is
the declaration open after ITEM.")
  (:method ((container container) (item null) declaration)
    declaration)
  (:method ((container container) (item symbol) declaration)
    (cond ((keywordp item)
           (let ((new (make-instance 'declaration :property item)))
             (add-child container new)
             new))
          (t (call-next-method))))
  (:method ((container container) (item node) declaration)
    (add-child container item)
    declaration)
  (:method ((container container) (item cons) declaration)
    (dolist (element item declaration)
      (setf declaration (add-item container element declaration))))
  (:method ((container container) item declaration)
    (unless declaration
      (error "Value ~S in ~S has no property." item container))
    (setf (declaration-values declaration)
          (append (declaration-values declaration) (list item)))
    declaration))

(defun add-body (container items)
  "Add ITEMS to CONTAINER in order, then fix the order of its children."
  (let ((declaration nil))
    (dolist (item items)
      (setf declaration (add-item container item declaration))))
  (setf (container-children container) (nreverse (container-children container)))
  container)

(defun make-rule (selector items)
  (add-body (make-instance 'rule :selector (parse-selector selector)) items))

(defun make-at-rule (name prelude items)
  (add-body (make-instance 'at-rule :name name :prelude prelude) items))

(defvar *styles* '()
  "STYLE objects in the order they were first defined; redefining a name
replaces its rules in place, so a re-evaluated group keeps its position.")

(defun ensure-style (name documentation items)
  "Define or redefine the style NAME; the first definition fixes its order."
  (let ((style (or (find name *styles* :key #'style-name)
                   (let ((new (make-instance 'style :name name)))
                     (setf *styles* (append *styles* (list new)))
                     new))))
    (setf (style-documentation style) documentation
          (container-children style) '())
    (add-body style items)))

(defun find-style (name)
  (find name *styles* :key #'style-name))

;;; The macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun selector-designator-p (object)
    (or (stringp object)
        (and (consp object) (every #'stringp object))))

  (defun body-item-form (item)
    "The form for ITEM of a rule body: keywords are properties, other symbols
CSS words, (\"selector\" . body) a nested rule, (:name prelude . body) an
at-rule, and any other list a Lisp form."
    (typecase item
      (keyword item)
      (symbol `',item)
      (cons (let ((head (first item)))
              (cond ((selector-designator-p head)
                     `(rule ,head ,@(rest item)))
                    ((keywordp head)
                     `(at-rule ,head ,(second item) ,@(cddr item)))
                    (t item))))
      (t item))))

(defmacro rule (selector &body body)
  "A rule for SELECTOR, a string, a list of strings, or a form; BODY as
DEFINE-STYLE describes.  Bare symbols in BODY are CSS words, not variables:
a body computed from Lisp values is a list for MAKE-RULE."
  `(make-rule ,(if (selector-designator-p selector) `',selector selector)
              (list ,@(mapcar #'body-item-form body))))

(defmacro at-rule (name prelude &body body)
  "The at-rule @NAME PRELUDE with BODY as DEFINE-STYLE describes."
  `(make-at-rule ,name ,prelude (list ,@(mapcar #'body-item-form body))))

(defmacro declarations (&body body)
  "The declarations BODY makes, read as a rule body, for a rule to include:
the way to write a mixin."
  `(container-children
    (add-body (make-instance 'container) (list ,@(mapcar #'body-item-form body)))))

(defun custom-property (name &rest values)
  "The declaration of the custom property --NAME with VALUES."
  (make-instance 'declaration
                 :property (intern (format nil "--~:@(~A~)" name) :keyword)
                 :values values))

(defmacro define-style (name &body body)
  "Define the style group NAME: an optional docstring, then rules.

A rule is (SELECTOR . BODY), SELECTOR a string or a list of strings.  BODY
alternates :property keywords with values -- bare symbols are CSS words,
strings are written verbatim, numbers and 0.85rem quantities and --name
references are themselves -- and lists are Lisp forms: a CSS function such
as (clamp 1rem 4vw 3rem), or any function returning values, declarations,
or rules to include.  Nested rules (\"selector\" . body) combine with the
parent, & standing for it; (:media \"query\" . body) is an at-rule."
  (let ((documentation (and (stringp (first body)) (pop body))))
    `(ensure-style ',name ,documentation (list ,@(mapcar #'body-item-form body)))))

;;; Text backend

(defvar *blank-line-pending* nil
  "Whether the next block written should be preceded by a blank line.")

(defun write-indent (indent stream)
  (loop repeat (* 2 indent) do (write-char #\Space stream)))

(defun begin-block (stream)
  (when *blank-line-pending*
    (terpri stream))
  (setf *blank-line-pending* nil))

(defun end-block ()
  (setf *blank-line-pending* t))

(defgeneric write-css-value (value stream)
  (:documentation "Write VALUE as CSS text.")
  (:method ((value symbol) stream) (write-string (css-name value) stream))
  (:method ((value string) stream) (write-string value stream))
  (:method ((value integer) stream) (princ value stream))
  (:method ((value float) stream)
    (let ((*read-default-float-format* (type-of value)))
      (princ value stream)))
  (:method ((value ratio) stream) (write-css-value (float value 1.0) stream))
  (:method ((value list) stream) (write-css-values value stream))
  (:method ((value dimension) stream)
    (write-css-value (dimension-number value) stream)
    (write-string (dimension-unit value) stream))
  (:method ((value slash) stream)
    (write-css-value (slash-left value) stream)
    (write-char #\/ stream)
    (write-css-value (slash-right value) stream))
  (:method ((value variable-reference) stream)
    (format stream "var(--~A)" (variable-reference-name value)))
  (:method ((value function-call) stream)
    (write-string (function-call-name value) stream)
    (write-char #\( stream)
    (loop for (argument . more) on (function-call-arguments value)
          do (write-css-value argument stream)
             (when more (write-string ", " stream)))
    (write-char #\) stream))
  (:method ((value comma-list) stream)
    (loop for (item . more) on (comma-list-values value)
          do (write-css-value item stream)
             (when more (write-string ", " stream))))
  (:method ((value quoted-string) stream)
    (write-char #\" stream)
    (loop for char across (quoted-string-text value)
          do (when (find char "\"\\") (write-char #\\ stream))
             (write-char char stream))
    (write-char #\" stream)))

(defun write-css-values (values stream)
  "Write VALUES space-separated."
  (loop for (value . more) on values
        do (write-css-value value stream)
           (when more (write-char #\Space stream))))

(defun css-value-text (value)
  (with-output-to-string (out) (write-css-value value out)))

(defgeneric write-css (node stream &key parent indent)
  (:documentation "Write NODE as CSS text to STREAM.  PARENT is the selector
NODE is nested under, INDENT the block depth."))

(defmethod write-css ((declaration declaration) stream &key parent (indent 0))
  (declare (ignore parent))
  (when (null (declaration-values declaration))
    (error "Declaration ~A has no value." (declaration-property declaration)))
  (write-indent indent stream)
  (format stream "~A: " (css-name (declaration-property declaration)))
  (write-css-values (declaration-values declaration) stream)
  (format stream ";~%"))

(defun write-declaration-block (selector declarations stream indent)
  (begin-block stream)
  (write-indent indent stream)
  (format stream "~A {~%" (selector-text selector))
  (dolist (declaration declarations)
    (write-css declaration stream :indent (1+ indent)))
  (write-indent indent stream)
  (format stream "}~%")
  (end-block))

(defun partition-children (container)
  "CONTAINER's declarations, and its other children, in order."
  (let ((children (container-children container)))
    (values (remove-if-not (lambda (child) (typep child 'declaration)) children)
            (remove-if (lambda (child) (typep child 'declaration)) children))))

(defmethod write-css ((rule rule) stream &key parent (indent 0))
  (let ((selector (combine-selectors parent (rule-selector rule))))
    (multiple-value-bind (declarations nested) (partition-children rule)
      (when declarations
        (write-declaration-block selector declarations stream indent))
      (dolist (node nested)
        (write-css node stream :parent selector :indent indent)))))

(defmethod write-css ((at-rule at-rule) stream &key parent (indent 0))
  (begin-block stream)
  (write-indent indent stream)
  (format stream "@~A ~A {~%" (css-name (at-rule-name at-rule)) (at-rule-prelude at-rule))
  (multiple-value-bind (declarations nested) (partition-children at-rule)
    (when declarations
      (unless parent
        (error "Declarations directly inside @~A ~A at top level."
               (css-name (at-rule-name at-rule)) (at-rule-prelude at-rule)))
      (write-declaration-block parent declarations stream (1+ indent)))
    (dolist (node nested)
      (write-css node stream :parent parent :indent (1+ indent))))
  (write-indent indent stream)
  (format stream "}~%")
  (end-block))

(defmethod write-css ((style style) stream &key parent (indent 0))
  (when (style-documentation style)
    (begin-block stream)
    (write-indent indent stream)
    (format stream "/* ~A */~%" (style-documentation style))
    (end-block))
  (dolist (node (container-children style))
    (write-css node stream :parent parent :indent indent)))

(defun css-text (&rest nodes)
  "The CSS text of NODES."
  (let ((*blank-line-pending* nil))
    (with-output-to-string (out)
      (dolist (node nodes)
        (write-css node out)))))

(defun stylesheet-text (&optional (styles *styles*))
  "The whole stylesheet: every style group in order."
  (apply #'css-text styles))
