;;;; Rendering Lisp source as dexp boxes.
;;;;
;;;; After wisp's structure editor: every list is a flex-wrapping box whose
;;;; left and right borders are its parentheses; atoms are inline spans.
;;;; Forms flow horizontally while they fit and wrap when they do not, so
;;;; the layout is responsive without any line-breaking logic.  The only
;;;; structural knowledge is a small table of operator roles: which leading
;;;; children are the head and which are body forms that should take the
;;;; full width and stack.  Roles are assigned here, in Lisp; the stylesheet
;;;; needs only `.lisp .body { flex-basis: 100% }`.

(in-package #:luv.wiki)

(defvar *lisp-role* nil
  "The layout role of the node being rendered: \"operator\", \"body\", or NIL.")

(defvar *lisp-package* nil
  "The package prefix considered current while rendering, hidden on symbols.")

(defvar *docstring-p* nil
  "True while rendering a string in documentation position.")

(defvar *prose-parameters* '()
  "Symbol names of the lambda list of the form whose docstring is being
rendered; an uppercase word naming one is a parameter reference.")

;;; Layouts
;;;
;;; A list is drawn according to a LAYOUT object.  The layout is chosen by
;;; the list's operator and by the role its parent gave it: a LET chooses a
;;; BINDINGS-LAYOUT for itself, and the child it marks as \"bindings\" is
;;; drawn by a GRID-LAYOUT no matter what its first element is.  Each layout
;;; answers CHILD-ROLE for the children and RENDER-LAYOUT for the whole;
;;; the stylesheet only knows the roles.

(defclass layout () ()
  (:documentation "How the children of a list are arranged."))

(defclass flow-layout (layout) ()
  (:documentation "The default: children flow and wrap; the first symbol is
the operator."))

(defclass body-layout (layout)
  ((head-count :initarg :head-count :initform 0 :accessor layout-head-count
               :documentation "How many arguments after the operator are the
head; the rest are body forms that take the full width."))
  (:documentation "Head arguments inline, then body forms stacked."))

(defclass bindings-layout (body-layout)
  ((bindings-index :initarg :bindings-index :initform 1 :accessor layout-bindings-index
                   :documentation "The index of the argument that is a list of
bindings, drawn as a two-column grid."))
  (:documentation "A binding form: LET, FLET, DEFCLASS, DO."))

(defclass variables-layout (body-layout) ()
  (:documentation "MULTIPLE-VALUE-BIND and friends: the first argument is a
list of variables drawn like a lambda list."))

(defclass spec-layout (body-layout) ()
  (:documentation "DOLIST, WITH-OPEN-FILE: the first argument is one
(var form ...) clause, not a list of bindings."))

(defclass lambda-layout (body-layout) ()
  (:documentation "A defining or lambda form whose head ends with a lambda
list: the lambda list's sublists are (var default) clauses."))

(defclass method-layout (lambda-layout) ()
  (:documentation "DEFMETHOD: the head runs through qualifiers to the lambda list."))

(defclass clauses-layout (body-layout) ()
  (:documentation "COND, CASE, HANDLER-CASE: body forms are clauses whose
first element is a key or test, not an operator, and whose rest stacks."))

(defclass grid-layout (layout) ()
  (:documentation "A list of bindings or slots: each element a clause,
aligned in two columns, name and rest."))

(defclass clause-layout (layout) ()
  (:documentation "A binding or slot: a name and the rest, no operator."))

(defclass stacked-clause-layout (clause-layout) ()
  (:documentation "A COND-style clause: a test, then body forms stacked."))

(defclass lambda-list-layout (layout) ()
  (:documentation "A lambda list: parameters flow; (var default) sublists are
clauses, not calls."))

(defclass loop-layout (layout) ()
  (:documentation "LOOP: its clause keywords start new rows."))

(defvar *operator-layouts* (make-hash-table :test 'equal)
  "Downcased operator name -> LAYOUT instance.")

(defmacro define-layout (names class &rest initargs)
  "Give each operator in NAMES (strings) a fresh CLASS layout."
  `(dolist (name ',(if (listp names) names (list names)))
     (setf (gethash name *operator-layouts*) (make-instance ',class ,@initargs))))

(define-layout ("defun" "defmacro" "defgeneric" "deftype" "define-modify-macro") lambda-layout :head-count 2)
(define-layout ("lambda" "returning" "fn") lambda-layout :head-count 1)
(define-layout ("defmethod") method-layout)
(define-layout ("let" "let*" "flet" "labels" "macrolet" "symbol-macrolet" "handler-bind")
  bindings-layout :head-count 1 :bindings-index 1)
(define-layout ("multiple-value-bind" "destructuring-bind" "with-slots" "with-accessors")
  variables-layout :head-count 2)
(define-layout ("dolist" "dotimes" "with-open-file" "with-output-to-string"
                "with-input-from-string" "with-open-stream" "with-simple-restart")
  spec-layout :head-count 1)
(define-layout ("do" "do*") bindings-layout :head-count 2 :bindings-index 1)
(define-layout ("defclass" "define-condition") bindings-layout :head-count 3 :bindings-index 3)
(define-layout ("cond" "case" "ecase" "typecase" "etypecase" "handler-case"
                "restart-case" "defstruct" "defpackage" "defsystem")
  clauses-layout :head-count 1)
(define-layout ("cond") clauses-layout :head-count 0)
(define-layout ("defstruct" "defpackage" "defsystem" "define-application-frame") clauses-layout :head-count 1)
(define-layout ("defvar" "defparameter" "defconstant" "deftest" "define-command"
                "define-presentation-type" "declaim" "declare" "block" "prog1"
                "when" "unless" "if" "unwind-protect" "eval-when" "with-html")
  body-layout :head-count 1)
(define-layout ("progn" "with-html-string") body-layout :head-count 0)
(define-layout ("define-presentation-method") body-layout :head-count 2)
(define-layout ("loop") loop-layout)

(defparameter *loop-keywords*
  '("named" "with" "for" "as" "initially" "finally" "repeat" "while" "until"
    "always" "never" "thereis" "do" "doing" "collect" "collecting" "append"
    "appending" "nconc" "nconcing" "count" "counting" "sum" "summing" "maximize"
    "maximizing" "minimize" "minimizing" "when" "if" "unless" "else" "end" "return"))

(defun operator-layout (name list)
  "The layout for operator NAME of LIST, from the table or, for operators
that only follow a naming convention, by inspecting the form: a WITH- form
has one head argument; a DEF form whose second argument is a list is a
lambda form; any other DEF form is a name followed by options."
  (let ((name (string-downcase name))
        (arguments (argument-children list)))
    (or (gethash name *operator-layouts*)
        (cond ((starts-with "with-" name) (make-instance 'body-layout :head-count 1))
              ((and (starts-with "def" name) (typep (third arguments) 'lisp-list))
               (make-instance 'lambda-layout :head-count 2))
              ((starts-with "def" name) (make-instance 'body-layout :head-count 1))
              (t (make-instance 'flow-layout))))))

(defgeneric list-layout (list role)
  (:documentation "The LAYOUT for LIST given the ROLE its parent assigned.")
  (:method ((list lisp-list) role)
    (cond ((role-p role "bindings") (make-instance 'grid-layout))
          ((role-p role "lambda-list") (make-instance 'lambda-list-layout))
          ((role-p role "clause") (make-instance 'clause-layout))
          ((role-p role "stacked-clause") (make-instance 'stacked-clause-layout))
          (t (let ((operator (symbol-node-name (first (element-children list)))))
               (if operator (operator-layout operator list) (make-instance 'flow-layout)))))))

(defun role-p (role name)
  (and role (member name (uiop:split-string role) :test #'string=)))

(defun argument-children (list)
  "The children of LIST that count as arguments: everything but comments."
  (remove-if (lambda (c) (typep c 'lisp-comment)) (element-children list)))

(defgeneric child-role (layout list index child)
  (:documentation "The role string for CHILD, the INDEXth argument of LIST
under LAYOUT (comments are not counted and never asked), or NIL.")
  (:method ((layout layout) list index child)
    (declare (ignore list))
    (and (= index 0) (typep child 'lisp-symbol) "operator")))

(defmethod child-role ((layout body-layout) list index child)
  (cond ((> index (layout-head-count layout)) "body")
        (t (call-next-method))))

(defmethod child-role ((layout bindings-layout) list index child)
  (cond ((and (= index (layout-bindings-index layout)) (typep child 'lisp-list)) "bindings")
        (t (call-next-method))))

(defmethod child-role ((layout variables-layout) list index child)
  (cond ((and (= index 1) (typep child 'lisp-list)) "lambda-list")
        (t (call-next-method))))

(defmethod child-role ((layout spec-layout) list index child)
  (cond ((and (= index 1) (typep child 'lisp-list)) "clause")
        (t (call-next-method))))

(defmethod child-role ((layout lambda-layout) list index child)
  (cond ((and (<= index (layout-head-count layout)) (typep child 'lisp-list)) "lambda-list")
        (t (call-next-method))))

(defmethod child-role ((layout lambda-list-layout) list index child)
  (declare (ignore list index))
  (and (typep child 'lisp-list) "clause"))

(defmethod child-role ((layout method-layout) list index child)
  "Qualifiers precede the lambda list; the first list after the name is it."
  (let* ((arguments (argument-children list))
         (lambda-list (position-if (lambda (c) (typep c 'lisp-list)) arguments :start 2)))
    (cond ((and lambda-list (= index lambda-list)) "lambda-list")
          ((and lambda-list (> index lambda-list)) "body")
          ((= index 0) "operator")
          (t nil))))

(defmethod child-role ((layout clauses-layout) list index child)
  (cond ((and (> index (layout-head-count layout)) (typep child 'lisp-list)) "body stacked-clause")
        (t (call-next-method))))

(defmethod child-role ((layout grid-layout) list index child)
  (declare (ignore list index))
  (and (typep child 'lisp-list) "clause"))

(defmethod child-role ((layout clause-layout) list index child)
  (declare (ignore list index child))
  nil)

(defmethod child-role ((layout stacked-clause-layout) list index child)
  (declare (ignore list child))
  (and (> index 0) "body"))

(defmethod child-role ((layout loop-layout) list index child)
  (declare (ignore list))
  (cond ((= index 0) "operator")
        ((and (typep child 'lisp-symbol)
              (null (lisp-symbol-package child))
              (member (string-downcase (lisp-symbol-name child)) *loop-keywords* :test #'string=))
         "row-start")
        (t nil)))

(defun lambda-list-of (layout list)
  "The lambda list of LIST under LAYOUT, if the layout has one."
  (typecase layout
    (method-layout (find-if (lambda (c) (typep c 'lisp-list)) (nthcdr 2 (argument-children list))))
    (lambda-layout (let ((head (subseq (argument-children list) 1
                                       (min (1+ (layout-head-count layout))
                                            (length (argument-children list))))))
                     (find-if (lambda (c) (typep c 'lisp-list)) head :from-end t)))
    (t nil)))

(defun docstring-position-p (layout list index previous child)
  "True when CHILD is a string in documentation position: the first body
form of a defining form, or the value after :documentation."
  (and (typep child 'lisp-string)
       (or (and (typep layout 'body-layout)
                (= index (1+ (layout-head-count layout)))
                (defining-operator-p (or (symbol-node-name (first (element-children list))) "")))
           (and (typep layout 'method-layout) (equal (child-role layout list index child) "body")
                (let ((before (nth (1- index) (argument-children list))))
                  (typep before 'lisp-list)))
           (and (typep previous 'lisp-symbol)
                (equal (lisp-symbol-package previous) "KEYWORD")
                (string-equal (lisp-symbol-name previous) "documentation")))))

(defgeneric render-layout (layout list)
  (:documentation "Emit the children of LIST arranged by LAYOUT, inside the
list's own box, which the caller has opened."))

(defun keyword-symbol-p (node)
  (and (typep node 'lisp-symbol) (equal (lisp-symbol-package node) "KEYWORD")))

(defun keyword-pairs-start (arguments)
  "The index in ARGUMENTS from which the rest is :keyword value pairs, or NIL.
The tail must have even length and a keyword at every even offset; the
earliest such start after the operator wins."
  (let ((n (length arguments)))
    (loop for start from 1 below n
          when (and (evenp (- n start))
                    (loop for i from start below n by 2
                          always (keyword-symbol-p (nth i arguments))))
            return start)))

(defun render-children-with-roles (layout list)
  "Emit every child of LIST with the role LAYOUT assigns it.  Comments are
drawn where they occur and are not counted.  A trailing run of :keyword
value pairs is drawn pair by pair, each in a .pair container that keeps
the key with its value; the pair takes the value's role."
  (let* ((arguments (argument-children list))
         (pairs-start (unless (typep layout '(or loop-layout lambda-list-layout grid-layout))
                        (keyword-pairs-start arguments)))
         (index -1)
         (previous nil)
         (remaining (element-children list)))
    (flet ((emit (child role)
             (let ((*docstring-p* (docstring-position-p layout list index previous child))
                   (*lisp-role* role))
               (render-html child))))
      (loop while remaining
            do (let ((child (pop remaining)))
                 (cond ((typep child 'lisp-comment)
                        (let ((*lisp-role* nil)) (render-html child)))
                       (t
                        (incf index)
                        (let ((role (child-role layout list index child)))
                          (when (and (role-p role "row-start") (> index 1))
                            (spinneret:with-html (:span.break)))
                          (if (and pairs-start (>= index pairs-start) (evenp (- index pairs-start))
                                   (find-if-not (lambda (c) (typep c 'lisp-comment)) remaining))
                              ;; A key: draw it with its value in one pair.
                              (let* ((value (find-if-not (lambda (c) (typep c 'lisp-comment)) remaining))
                                     (value-role (child-role layout list (1+ index) value)))
                                (spinneret:with-html
                                  (:span :class (let ((*lisp-role* value-role)) (role-class "pair"))
                                         (emit child nil)
                                         ;; Comments between key and value stay in order.
                                         (loop for c = (pop remaining)
                                               do (if (typep c 'lisp-comment)
                                                      (let ((*lisp-role* nil)) (render-html c))
                                                      (progn (incf index) (emit c nil) (return))))))
                                (setf previous value))
                              (progn (emit child role)
                                     (setf previous child)))))))))))

(defmethod render-layout ((layout layout) list)
  (render-children-with-roles layout list))

(defmethod render-layout ((layout clause-layout) list)
  "A clause: its first element in the name column, the rest in one flowing
cell, so the parent's grid can align them."
  (let ((children (element-children list)))
    (let ((*lisp-role* nil))
      (when children (render-html (first children))))
    (when (rest children)
      (spinneret:with-html
        (:span.rest
         (let ((*lisp-role* nil))
           (dolist (child (rest children))
             (render-html child))))))))

(defmethod render-layout ((layout stacked-clause-layout) list)
  "A COND-style clause: the test inline, then body forms stacked; drawn by
the general routine, whose roles do that."
  (render-children-with-roles layout list))

(defun render-text-with-mentions (text)
  "Write TEXT, turning #ID figure mentions into links like prose does."
  (let ((start 0))
    (loop for i from 0 below (length text)
          do (when (char= (char text i) #\#)
               (let ((end (mention-end text i)))
                 (when end
                   (spinneret:html (subseq text start i))
                   (render-html (make-instance 'mention :id (subseq text (1+ i) end)))
                   (setf start end)))))
    (spinneret:html (subseq text start))))

(defun role-class (&rest classes)
  (format nil "~{~A~^ ~}" (remove nil (cons *lisp-role* classes))))

(defmethod render-html ((list lisp-list))
  (let* ((operator (symbol-node-name (first (element-children list))))
         (layout (list-layout list *lisp-role*))
         (lambda-list (lambda-list-of layout list))
         (*prose-parameters* (if lambda-list
                                 (append (lambda-list-parameters lambda-list) *prose-parameters*)
                                 *prose-parameters*)))
    (spinneret:with-html
      (:div :class (role-class "list")
            :data-callee (and operator (not (typep layout '(or clause-layout grid-layout)))
                              (string-downcase operator))
            (render-layout layout list)))))

(defmethod render-html ((vector lisp-vector))
  (spinneret:with-html
    (:div :class (role-class "list" "vector")
          (render-layout (make-instance 'flow-layout) vector))))

(defmethod render-html ((symbol lisp-symbol))
  (let* ((package (lisp-symbol-package symbol))
         (name (lisp-symbol-name symbol))
         (keyword-p (equal package "KEYWORD"))
         (current-p (or (null package)
                        (and (stringp package) *lisp-package*
                             (string-equal package *lisp-package*)))))
    (spinneret:with-html
      (:span :class (role-class "symbol" (and keyword-p "keyword"))
             :data-symbol-name name
             (let* ((definition (and (not keyword-p) (not (eq package :uninterned))
                                     (find-named-definition name)))
                    (href (and definition (definition-page-href definition))))
               (if href
                   (:a.definition-link :href href :title (format nil "~A ~A, ~A:~D"
                                                                 (definition-kind definition)
                                                                 (definition-name definition)
                                                                 (definition-file-name definition)
                                                                 (definition-line definition))
                       (render-symbol-text symbol package current-p keyword-p))
                   (render-symbol-text symbol package current-p keyword-p)))))))

(defun render-symbol-text (symbol package current-p keyword-p)
  "The package prefix, if shown, and the name of SYMBOL."
  (spinneret:with-html
    (cond (keyword-p
           (:span.package ":"))
          ((eq package :uninterned)
           (:span.package "#:"))
          ((not current-p)
           (:span.package (string-downcase package)
                          (if (lisp-symbol-external-p symbol) ":" "::"))))
    (:span.name (string-downcase (lisp-symbol-name symbol)))))

(defmethod render-html ((atom lisp-atom))
  (spinneret:with-html (:span :class (role-class "atom") (node-text atom))))

(defclass symbol-reference (inline-object)
  ((name :initarg :name :accessor symbol-reference-name))
  (:documentation "An uppercase word in code prose that names a symbol: a
parameter of the enclosing form or a definition in the corpus."))

(defun symbol-word-p (word)
  "True when WORD is written the way docstrings write symbols: at least two
characters, all uppercase letters, digits, or symbol punctuation, with at
least one letter."
  (and (>= (length word) 2)
       (some #'alpha-char-p word)
       (every (lambda (c) (or (upper-case-p c) (digit-char-p c)
                              (member c '(#\- #\* #\+ #\% #\/ #\< #\> #\= #\!))))
              word)))

(defun symbol-reference-p (word)
  "An uppercase WORD is a symbol reference when it names a parameter of the
enclosing form, a definition in the corpus, or is a *special* name."
  (and (symbol-word-p word)
       (or (member word *prose-parameters* :test #'string-equal)
           (and *site* (gethash (bare-name word) (site-definition-table *site*)))
           (and (> (length word) 2) (char= (char word 0) #\*) (char= (char word (1- (length word))) #\*)))))

(defun mark-symbol-references (inlines)
  "Split the strings among INLINES so that symbol words become
SYMBOL-REFERENCE objects."
  (loop for inline in inlines
        append (if (stringp inline)
                   (split-symbol-references inline)
                   (progn
                     (when (typep inline '(or emphasis link))
                       (unless (and (typep inline 'emphasis)
                                    (member (emphasis-kind inline) '(:verbatim :code)))
                         (setf (element-children inline)
                               (mark-symbol-references (element-children inline)))))
                     (list inline)))))

(defun split-symbol-references (string)
  (let ((result '())
        (start 0)
        (i 0)
        (n (length string)))
    (flet ((word-char-p (c) (or (alphanumericp c) (member c '(#\- #\* #\+ #\% #\/ #\< #\> #\= #\!)))))
      (loop while (< i n)
            do (if (word-char-p (char string i))
                   (let ((end (or (position-if-not #'word-char-p string :start i) n)))
                     (let ((word (subseq string i end)))
                       (when (symbol-reference-p word)
                         (when (> i start) (push (subseq string start i) result))
                         (push (make-instance 'symbol-reference :name word) result)
                         (setf start end)))
                     (setf i end))
                   (incf i))))
    (when (< start n) (push (subseq string start) result))
    (nreverse result)))

(defmethod render-html ((reference symbol-reference))
  "Drawn like a symbol in the boxes: lowercase code, linked to its
definition when the corpus has one."
  (let* ((name (symbol-reference-name reference))
         (definition (find-named-definition name))
         (href (and definition (definition-page-href definition))))
    (spinneret:with-html
      (if href
          (:a.definition-link :href href
                              :title (format nil "~A ~A, ~A:~D" (definition-kind definition)
                                             (definition-name definition)
                                             (definition-file-name definition)
                                             (definition-line definition))
                              (:code.symbol (string-downcase name)))
          (:code.symbol (string-downcase name))))))

(defun mark-prose-references (element)
  "Turn symbol words in the paragraphs of ELEMENT into references."
  (map-elements (lambda (e)
                  (when (typep e 'paragraph)
                    (setf (element-children e) (mark-symbol-references (element-children e)))))
                element))

(defun render-code-prose (text)
  "Render TEXT, the content of a docstring or comment, as wiki prose:
paragraphs, lists, and inline markup, with #ID mentions as links and
uppercase symbol words as symbol references."
  (let ((*prose-from-code* t))
    (dolist (block (read-blocks (coerce (uiop:split-string text :separator '(#\Newline)) 'vector)))
      (mark-prose-references block)
      (render-html block))))

(defun lambda-list-parameters (list)
  "The parameter names in the lambda list LIST (a lisp-list), including
those inside specializer or default forms and skipping &keywords."
  (let ((names '()))
    (labels ((walk (node)
               (typecase node
                 (lisp-symbol (let ((name (lisp-symbol-name node)))
                                (unless (char= (char name 0) #\&) (pushnew name names :test #'string=))))
                 (lisp-list (let ((first (first (element-children node))))
                              ;; (var default) or (var specializer): only the first
                              (when (typep first 'lisp-symbol)
                                (pushnew (lisp-symbol-name first) names :test #'string=)))))))
      (mapc #'walk (element-children list)))
    names))

(defun string-node-content (string)
  "The characters of a string literal STRING, without the quotes and with
\\\" and \\\\ escapes undone."
  (let ((text (node-text string)))
    (with-output-to-string (out)
      (loop with i = 1
            while (< i (1- (length text)))
            do (let ((c (char text i)))
                 (if (and (char= c #\\) (< (1+ i) (1- (length text))))
                     (progn (write-char (char text (1+ i)) out) (incf i 2))
                     (progn (write-char c out) (incf i))))))))

(defun dedent (text)
  "Remove the indentation common to every non-blank line after the first."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (rest (remove-if #'blank-line-p (rest lines)))
         (indent (if rest (reduce #'min (mapcar #'indentation rest)) 0)))
    (format nil "~{~A~^~%~}"
            (cons (first lines)
                  (mapcar (lambda (line) (subseq line (min indent (length line))))
                          (rest lines))))))

(defmethod render-html ((string lisp-string))
  (let ((text (node-text string)))
    (spinneret:with-html
      (if (or *docstring-p* (find #\Newline text))
          ;; Documentation, or any multi-line string, is rendered as prose.
          (:div :class (role-class "string" "prose")
                (render-code-prose (dedent (string-node-content string))))
          (:span :class (role-class "string") (render-text-with-mentions text))))))

(defun comment-content (comment)
  "The text of COMMENT without its ; prefixes or #| |# delimiters."
  (let ((text (node-text comment)))
    (if (starts-with "#|" text)
        (string-trim '(#\Space #\Newline)
                     (subseq text 2 (max 2 (- (length text) (if (ends-with "|#" text) 2 0)))))
        (format nil "~{~A~^~%~}"
                (mapcar (lambda (line)
                          (let* ((line (string-left-trim '(#\Space #\Tab) line))
                                 (semis (or (position-if-not (lambda (c) (char= c #\;)) line)
                                            (length line))))
                            (string-left-trim '(#\Space) (subseq line semis))))
                        (uiop:split-string text :separator '(#\Newline)))))))

(defun ends-with (suffix string)
  (and (>= (length string) (length suffix))
       (string= suffix string :start2 (- (length string) (length suffix)))))

(defmethod render-html ((number lisp-number))
  (spinneret:with-html (:span :class (role-class "number") (node-text number))))

(defmethod render-html ((character lisp-character))
  (spinneret:with-html (:span :class (role-class "character") (node-text character))))

(defmethod render-html ((prefix lisp-prefix))
  (spinneret:with-html
    (:span :class (role-class "prefixed")
           (:span.prefix (lisp-prefix-string prefix))
           (let ((*lisp-role* nil))
             (render-html (lisp-prefix-child prefix))))))

(defmethod render-html ((conditional lisp-conditional))
  (spinneret:with-html
    (:span :class (role-class "conditional")
           (:span.prefix (lisp-conditional-sign conditional))
           (let ((*lisp-role* nil))
             (when (lisp-conditional-feature conditional)
               (render-html (lisp-conditional-feature conditional)))
             (if (lisp-conditional-form conditional)
                 (render-html (lisp-conditional-form conditional))
                 ;; The skipped branch is shown as its text.
                 (:span.skipped (string-left-trim
                                 " " (subseq (node-text conditional)
                                             (if (lisp-conditional-feature conditional)
                                                 (- (node-end (lisp-conditional-feature conditional))
                                                    (node-start conditional))
                                                 2)))))))))

(defmethod render-html ((comment lisp-comment))
  (spinneret:with-html
    (:div :class (role-class "comment" "prose")
          (render-code-prose (comment-content comment)))))

(defmethod render-html ((skipped lisp-skipped))
  (spinneret:with-html
    (:span :class (role-class "skipped") (node-text skipped))))

(defun render-lisp-nodes (nodes &key package)
  "Emit a .lisp container holding NODES rendered as dexp boxes."
  (let ((*lisp-package* package))
    (spinneret:with-html
      (:div.lisp
       (dolist (node nodes)
         (let ((*lisp-role* nil))
           (render-html node)))))))

(defun render-lisp-source (text &key package)
  "Read TEXT and render it structurally; on any failure fall back to a
plain <pre>, so a page never loses its code."
  (let ((nodes (handler-case (read-lisp-string text)
                 (error (condition)
                   (warn "Rendering Lisp source as text: ~A" condition)
                   nil))))
    (if nodes
        (render-lisp-nodes nodes :package package)
        (spinneret:with-html (:pre.src :data-language "lisp" (:code text))))))
