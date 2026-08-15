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

(defparameter *operator-head-counts*
  '(("defun" . 2) ("defmacro" . 2) ("defgeneric" . 2) ("defclass" . 3)
    ("defstruct" . 1) ("deftype" . 2) ("define-condition" . 3) ("defpackage" . 1)
    ("defvar" . 1) ("defparameter" . 1) ("defconstant" . 1) ("deftest" . 1)
    ("defsystem" . 1) ("lambda" . 1) ("let" . 1) ("let*" . 1) ("flet" . 1)
    ("labels" . 1) ("macrolet" . 1) ("symbol-macrolet" . 1)
    ("multiple-value-bind" . 2) ("destructuring-bind" . 2)
    ("when" . 1) ("unless" . 1) ("if" . 1) ("cond" . 0) ("case" . 1) ("ecase" . 1)
    ("typecase" . 1) ("etypecase" . 1) ("handler-case" . 1) ("handler-bind" . 1)
    ("restart-case" . 1) ("unwind-protect" . 1) ("progn" . 0) ("prog1" . 1)
    ("block" . 1) ("dolist" . 1) ("dotimes" . 1) ("do" . 2) ("do*" . 2) ("loop" . 0)
    ("eval-when" . 1) ("with-slots" . 2) ("with-accessors" . 2)
    ("with-open-file" . 1) ("with-output-to-string" . 1) ("with-html" . 0)
    ("with-html-string" . 0) ("define-presentation-method" . 2)
    ("define-application-frame" . 2) ("define-command" . 1)
    ("labels" . 1) ("returning" . 1) ("fn" . 1))
  "For an operator, how many children after it form the head; the rest are
body forms.  Operators not listed flow inline, except that any WITH- or
DEF- operator gets a head of one or two respectively.")

(defun operator-head-count (name)
  "The number of head arguments for operator NAME, or NIL for none."
  (let ((name (string-downcase name)))
    (cond ((assoc name *operator-head-counts* :test #'string=)
           (cdr (assoc name *operator-head-counts* :test #'string=)))
          ((starts-with "with-" name) 1)
          ((starts-with "define-" name) 2)
          ((starts-with "def" name) 2)
          (t nil))))

(defun list-body-start (list)
  "The index in LIST's children where body forms begin, or NIL if the
list has no body role.  DEFMETHOD's head runs through its qualifiers and
lambda list."
  (let* ((children (element-children list))
         (operator (symbol-node-name (first children))))
    (when operator
      (if (string-equal operator "defmethod")
          (let ((lambda-list (position-if (lambda (c) (typep c 'lisp-list)) children :start 2)))
            (and lambda-list (1+ lambda-list)))
          (let ((count (operator-head-count operator)))
            (and count (+ 1 count)))))))

(defparameter *clause-lists*
  '(("let" . 1) ("let*" . 1) ("flet" . 1) ("labels" . 1) ("macrolet" . 1)
    ("symbol-macrolet" . 1) ("with-slots" . 1) ("with-accessors" . 1)
    ("destructuring-bind" . 1) ("multiple-value-bind" . 1) ("do" . 1) ("do*" . 1)
    ("handler-bind" . 1) ("defclass" . 3) ("define-condition" . 3)
    ("defun" . 2) ("defmacro" . 2) ("defgeneric" . 2) ("deftype" . 2) ("lambda" . 1)
    ("defmethod" . :lambda-list)
    ("cond" . :body) ("case" . :body) ("ecase" . :body) ("typecase" . :body)
    ("etypecase" . :body) ("handler-case" . :body) ("restart-case" . :body)
    ("defstruct" . :body) ("defpackage" . :body) ("defsystem" . :body)
    ("defgeneric" . :body))
  "Operators whose argument at the given index is a list of clauses (or
whose body forms are clauses, marked :body): the first element of a clause
is a name or key, not an operator.")

(defun list-clause-roles (list)
  "Return (values bindings-index clauses-p): the index of a clause-list
child, and whether body children of LIST are themselves clauses."
  (let* ((operator (symbol-node-name (first (element-children list))))
         (entry (and operator (assoc (string-downcase operator) *clause-lists* :test #'string=))))
    (cond ((null entry) (values nil nil))
          ((eq (cdr entry) :body) (values nil t))
          ((eq (cdr entry) :lambda-list)
           ;; DEFMETHOD's lambda list follows the name and any qualifiers.
           (values (position-if (lambda (c) (typep c 'lisp-list))
                                (element-children list) :start 2)
                   nil))
          (t (values (cdr entry) nil)))))

(defun render-lisp-children (list)
  (let ((own-role *lisp-role*)
        (body-start (list-body-start list)))
    (multiple-value-bind (bindings-index clauses-p) (list-clause-roles list)
      ;; Comments do not count as arguments when assigning roles.
      (loop with i = -1
            with previous = nil
            for child in (element-children list)
            do (unless (typep child 'lisp-comment) (incf i))
               (let ((*docstring-p*
                       (and (typep child 'lisp-string)
                            (or (and body-start (= i body-start) (defining-operator-p (or (symbol-node-name (first (element-children list))) "")))
                                (and (typep previous 'lisp-symbol)
                                     (equal (lisp-symbol-package previous) "KEYWORD")
                                     (string-equal (lisp-symbol-name previous) "documentation")))))
                     (*lisp-role*
                       (cond ((typep child 'lisp-comment) nil)
                             ((and bindings-index (= i bindings-index)) "bindings")
                             ((and body-start (>= i body-start))
                              (if clauses-p "body clause" "body"))
                             ((and (equal own-role "bindings") (typep child 'lisp-list)) "clause")
                             ((and (= i 0) (typep child 'lisp-symbol)
                                   (not (member own-role '("clause" "bindings" "body clause")
                                                :test #'equal)))
                              "operator")
                             (t nil))))
                 (render-html child))
               (unless (typep child 'lisp-comment) (setf previous child))))))



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
  (let ((operator (symbol-node-name (first (element-children list)))))
    (spinneret:with-html
      (:div :class (role-class "list")
            :data-callee (and operator (string-downcase operator))
            (render-lisp-children list)))))

(defmethod render-html ((vector lisp-vector))
  (spinneret:with-html
    (:div :class (role-class "list" "vector")
          (render-lisp-children vector))))

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

(defun render-code-prose (text)
  "Render TEXT, the content of a docstring or comment, as wiki prose:
paragraphs, lists, and inline markup, with #ID mentions as links."
  (let ((*prose-from-code* t))
    (dolist (block (read-blocks (coerce (uiop:split-string text :separator '(#\Newline)) 'vector)))
      (render-html block))))

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
