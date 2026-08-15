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

(defun current-package-name ()
  "The current package as a plain uppercase name, for operator lookups."
  (and *lisp-package* (string-upcase (string-trim "#:\"" *lisp-package*))))

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

(defclass clauses-layout (body-layout)
  ((clause-head-count :initarg :clause-head-count :initform 1 :accessor layout-clause-head-count
                      :documentation "How many leading elements of each clause
are its key: one for COND and CASE, two for HANDLER-CASE, whose clauses
name a type and then a lambda list."))
  (:documentation "COND, CASE, HANDLER-CASE: body forms are clauses whose
first element is a key or test, not an operator, and whose rest stacks;
drawn as a table of key and rest."))

(defclass grid-layout (layout) ()
  (:documentation "A list of bindings or slots: each element a clause,
aligned in two columns, name and rest."))

(defclass clause-layout (layout) ()
  (:documentation "A binding or slot: a name and the rest, no operator."))

(defclass stacked-clause-layout (clause-layout)
  ((head-count :initarg :head-count :initform 1 :accessor layout-head-count)
   (lambda-list-index :initarg :lambda-list-index :initform nil :accessor layout-lambda-list-index
                      :documentation "The index of a lambda list among the head
elements, as in a HANDLER-CASE clause, or NIL."))
  (:documentation "A COND-style clause: a key or test, then body forms stacked."))

(defclass lambda-list-layout (layout) ()
  (:documentation "A lambda list: parameters flow; (var default) sublists are
clauses, not calls."))

(defclass loop-layout (layout) ()
  (:documentation "LOOP: its clause keywords start new rows."))

(defclass pairs-layout (layout) ()
  (:documentation "SETF, SETQ, PSETF: the arguments are place value pairs;
with more than one pair each is a row of a two-column table."))

(defvar *operator-layouts* (make-hash-table :test 'equal)
  "Downcased operator name -> LAYOUT instance.")

(defmacro define-layout (names class &rest initargs)
  "Give each operator in NAMES (strings) a fresh CLASS layout."
  `(dolist (name ',(if (listp names) names (list names)))
     (setf (gethash name *operator-layouts*) (make-instance ',class ,@initargs))))

(define-layout ("defun" "defmacro" "defgeneric" "deftype" "define-modify-macro") lambda-layout :head-count 2)
(define-layout ("lambda") lambda-layout :head-count 1)
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
(define-layout ("case" "ecase" "ccase" "typecase" "etypecase" "ctypecase")
  clauses-layout :head-count 1)
(define-layout ("handler-case" "restart-case")
  clauses-layout :head-count 1 :clause-head-count 2)
(define-layout ("cond") clauses-layout :head-count 0)
(define-layout ("defstruct" "defpackage") clauses-layout :head-count 1)
(define-layout ("defvar" "defparameter" "defconstant" "declaim" "declare" "block"
                "prog1" "when" "unless" "if" "unwind-protect" "eval-when")
  body-layout :head-count 1)
(define-layout ("progn") body-layout :head-count 0)
(define-layout ("loop") loop-layout)
(define-layout ("setf" "setq" "psetf" "psetq") pairs-layout)

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
  (let* ((name (string-downcase name))
         (arguments (argument-children list))
         (head (first arguments))
         (facts (operator-facts name (or (and (typep head 'lisp-symbol)
                                              (stringp (lisp-symbol-package head))
                                              (lisp-symbol-package head))
                                         (current-package-name)))))
    (or (gethash name *operator-layouts*)
        (and facts (arglist-layout facts name))
        (cond ((starts-with "with-" name) (make-instance 'body-layout :head-count 1))
              ((and (starts-with "def" name) (typep (third arguments) 'lisp-list))
               (make-instance 'lambda-layout :head-count 2))
              ((starts-with "def" name) (make-instance 'body-layout :head-count 1))
              (t (make-instance 'flow-layout))))))

(defgeneric list-layout (list role)
  (:documentation "The LAYOUT for LIST given the ROLE its parent assigned.")
  (:method ((list lisp-list) role)
    (cond ((and (role-p role "bindings")
                (some (lambda (c) (typep c 'lisp-list)) (element-children list)))
           (make-instance 'grid-layout))
          ((role-p role "bindings") (make-instance 'lambda-list-layout))
          ((role-p role "lambda-list") (make-instance 'lambda-list-layout))
          ((role-p role "clause") (make-instance 'clause-layout))
          ((role-p role "handler-clause")
           (make-instance 'stacked-clause-layout :head-count 2 :lambda-list-index 1))
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
  (cond ((and (> index (layout-head-count layout)) (typep child 'lisp-list))
         (if (> (layout-clause-head-count layout) 1)
             "body stacked-clause handler-clause"
             "body stacked-clause"))
        (t (call-next-method))))

(defmethod child-role ((layout grid-layout) list index child)
  (declare (ignore list index))
  (and (typep child 'lisp-list) "clause"))

(defmethod child-role ((layout clause-layout) list index child)
  (declare (ignore list index child))
  nil)

(defmethod child-role ((layout stacked-clause-layout) list index child)
  (declare (ignore list))
  (cond ((>= index (layout-head-count layout)) "body")
        ((and (eql index (layout-lambda-list-index layout)) (typep child 'lisp-list)) "lambda-list")
        (t nil)))

(defmethod child-role ((layout pairs-layout) list index child)
  "Values are body forms when there is more than one pair, so the pairs
stack as rows; a single pair stays inline."
  (declare (ignore child))
  (cond ((= index 0) "operator")
        ((and (evenp index) (> (length (argument-children list)) 3)) "body")
        (t nil)))

(defmethod child-role ((layout loop-layout) list index child)
  (declare (ignore list))
  (cond ((= index 0) "operator")
        ((and (typep child 'lisp-symbol)
              (null (lisp-symbol-package child))
              (member (string-downcase (lisp-symbol-name child)) *loop-keywords* :test #'string=))
         "row-start")
        (t nil)))

;;; Layouts derived from real lambda lists (see introspect.lisp)

(defvar *arglists* (make-hash-table :test 'equal)
  "(PACKAGE . NAME) -> facts plist gathered by scripts/wiki introspect, and
NAME -> facts for lookup without a package.")

(defun load-arglists (pathname)
  "Read the operator facts written by WRITE-ARGLISTS into *ARGLISTS*."
  (clrhash *arglists*)
  (when (probe-file pathname)
    (with-open-file (in pathname)
      (with-standard-io-syntax
        (let ((*package* (find-package :cl-user))
              (*read-eval* nil))
          (loop for entry = (read in nil nil)
                while entry
                do (destructuring-bind ((package . name) &rest facts) entry
                     (setf (gethash (cons package name) *arglists*) facts)
                     (unless (gethash name *arglists*)
                       (setf (gethash name *arglists*) facts))))))))
  (hash-table-count *arglists*))

(defun operator-facts (name package)
  "The introspected facts for operator NAME in PACKAGE (a name or NIL)."
  (let ((name (string-upcase name)))
    (or (and package (gethash (cons (string-upcase package) name) *arglists*))
        (gethash name *arglists*))))

(defclass derived-layout (layout)
  ((kind :initarg :kind :accessor layout-kind)
   (name :initarg :name :initform nil :accessor layout-name)
   (head-count :initarg :head-count :initform 0 :accessor layout-head-count)
   (body-p :initarg :body-p :initform nil :accessor layout-body-p)
   (pairs-start :initarg :pairs-start :initform nil :accessor layout-pairs-start
                :documentation "Argument index where &KEY pairs begin, or NIL.")
   (roles :initarg :roles :initform '() :accessor layout-roles
          :documentation "Alist of (argument-index . role) from the lambda list:
a destructuring pattern is a clause, a parameter named BINDINGS or SLOTS
a binding grid, one named LAMBDA-LIST a lambda list.")
   (body-role :initarg :body-role :initform "body" :accessor layout-body-role))
  (:documentation "A layout computed from an operator's real lambda list."))

(defun parameter-role (parameter)
  "The role a parameter's name or shape suggests for the argument in its
position: a destructuring pattern is a clause; a name that says BINDINGS or
SLOTS is a binding grid; LAMBDA-LIST is a lambda list."
  (cond ((consp parameter) "clause")
        ((stringp parameter)
         (cond ((search "LAMBDA-LIST" parameter) "lambda-list")
               ((search "ARGLIST" parameter) "lambda-list")
               ((member parameter '("ARGS" "VARS" "VARIABLES" "PARAMETERS") :test #'string=) "lambda-list")
               ((search "BINDING" parameter) "bindings")
               ((search "DEFINITIONS" parameter) "bindings")
               ((search "SLOT" parameter) "bindings")
               (t nil)))
        (t nil)))

(defun arglist-layout (facts &optional name)
  "Derive a layout from FACTS, walking the lambda list at its base level."
  (let ((kind (getf facts :kind))
        (lambda-list (getf facts :lambda-list))
        (index 0)
        (roles '())
        (body-p nil)
        (pairs-start nil)
        (body-role "body")
        (state :required))
    (loop with rest = lambda-list
          while rest
          do (let ((parameter (pop rest)))
               (case parameter
                 ((:&whole :&environment) (pop rest))
                 (:&optional (setf state :optional))
                 (:&aux (return))
                 (:&key (setf pairs-start (1+ index)) (return))
                 ((:&rest :&body)
                  (let ((name (pop rest)))
                    (when (or (eq parameter :&body) (member kind '(:macro :special-operator)))
                      (setf body-p (or (eq parameter :&body) (member kind '(:macro :special-operator))))
                      (when (and (stringp name)
                                 (or (search "CLAUSE" name) (search "CASE" name)
                                     (search "SLOT" name)))
                        (setf body-role "body stacked-clause"))))
                  ;; Anything after &rest/&body except &key does not count.
                  (loop while (and rest (not (eq (first rest) :&key))) do (pop rest))
                  (when (eq (first rest) :&key)
                    (setf pairs-start (1+ index))
                    (return)))
                 (t
                  (incf index)
                  (let ((role (parameter-role (if (and (eq state :optional) (consp parameter))
                                                  (first parameter)
                                                  parameter))))
                    (when role (push (cons index role) roles)))))))
    (make-instance 'derived-layout
                   :kind kind
                   :name name
                   :head-count index
                   :body-p (and body-p t)
                   :pairs-start pairs-start
                   :roles (nreverse roles)
                   :body-role body-role)))

(defmethod child-role ((layout derived-layout) list index child)
  (let ((entry (assoc index (layout-roles layout))))
    (cond ((and entry (typep child 'lisp-list)) (cdr entry))
          ;; The keyword options of a defining macro stack as rows.
          ((and (layout-pairs-start layout) (>= index (layout-pairs-start layout))
                (eq (layout-kind layout) :macro)
                (layout-name layout) (starts-with "def" (layout-name layout)))
           "body")
          ((and (layout-body-p layout) (> index (layout-head-count layout))
                (or (null (layout-pairs-start layout)) (< index (layout-pairs-start layout))))
           (if (typep child 'lisp-list) (layout-body-role layout) "body"))
          ((= index 0) (and (typep child 'lisp-symbol) "operator"))
          (t nil))))

(defgeneric layout-pairs-index (layout arguments)
  (:documentation "The argument index where key value pairs begin under
LAYOUT, or NIL; the default guesses a trailing run of :keyword value
pairs from the arguments themselves.")
  (:method ((layout layout) arguments)
    (keyword-pairs-start arguments))
  (:method ((layout loop-layout) arguments) nil)
  (:method ((layout lambda-list-layout) arguments) nil)
  (:method ((layout grid-layout) arguments) nil)
  (:method ((layout pairs-layout) arguments) 1)
  (:method ((layout derived-layout) arguments)
    (or (layout-pairs-start layout) (call-next-method))))

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

;;; Drawing
;;;
;;; The children of a list are first grouped into ITEMs -- a comment, one
;;; child with its role, or a :keyword value pair -- and then drawn.  A list
;;; with body items is STACKED: its head items go in a .head row and each
;;; body item is a row of its own, so the box is a grid of rows and hugs its
;;; widest row instead of stretching to the parent (#993QQQ).

(defstruct (item (:constructor make-item (kind role node &key value comments index previous)))
  "One drawn unit of a list: KIND is :comment, :child, or :pair.  ROLE is
the role the layout gave it (a pair takes its value's role); NODE the
child or the key; VALUE and COMMENTS the pair's value and the comments
between key and value; INDEX and PREVIOUS the argument index and previous
argument, for docstring position."
  kind role node value comments index previous)

(defun item-body-p (item)
  (role-p (item-role item) "body"))

(defun layout-items (layout list)
  "The children of LIST grouped for drawing under LAYOUT.  Comments are
kept where they occur and are not counted.  A trailing run of :keyword
value pairs is grouped pair by pair, keeping the key with its value."
  (let* ((arguments (argument-children list))
         (pairs-start (layout-pairs-index layout arguments))
         (index -1)
         (previous nil)
         (remaining (element-children list))
         (items '()))
    (loop while remaining
          do (let ((child (pop remaining)))
               (cond ((typep child 'lisp-comment)
                      (push (make-item :comment nil child) items))
                     (t
                      (incf index)
                      (let ((role (child-role layout list index child)))
                        (if (and pairs-start (>= index pairs-start) (evenp (- index pairs-start))
                                 (find-if-not (lambda (c) (typep c 'lisp-comment)) remaining))
                            ;; A key: group it with its value.
                            (let ((comments '())
                                  (value nil))
                              (loop for c = (pop remaining)
                                    do (if (typep c 'lisp-comment)
                                           (push c comments)
                                           (progn (setf value c) (return))))
                              (incf index)
                              (push (make-item :pair (child-role layout list index value) child
                                               :value value :comments (nreverse comments)
                                               :index index :previous previous)
                                    items)
                              (setf previous value))
                            (progn
                              (push (make-item :child role child :index index :previous previous) items)
                              (setf previous child))))))))
    (nreverse items)))

(defun render-item (layout list item)
  "Draw ITEM of LIST under LAYOUT."
  (flet ((emit (child role index previous)
           (let ((*docstring-p* (docstring-position-p layout list index previous child))
                 (*lisp-role* role))
             (render-html child))))
    (ecase (item-kind item)
      (:comment (let ((*lisp-role* nil)) (render-html (item-node item))))
      (:child
       (when (and (role-p (item-role item) "row-start") (> (item-index item) 1))
         (spinneret:with-html (:span.break)))
       (emit (item-node item) (item-role item) (item-index item) (item-previous item)))
      (:pair
       (spinneret:with-html
         (:span :class (let ((*lisp-role* (item-role item))) (role-class "pair"))
                (emit (item-node item) nil (1- (item-index item)) (item-previous item))
                (dolist (c (item-comments item))
                  (let ((*lisp-role* nil)) (render-html c)))
                (emit (item-value item) nil (item-index item) (item-node item))))))))

(defun split-head-items (items)
  "ITEMS before the first body item as the head, and the rest as rows;
comments just before the first body item are rows, not head."
  (let ((position (position-if #'item-body-p items)))
    (if (null position)
        (values items '())
        (progn
          (loop while (and (> position 0) (eq (item-kind (nth (1- position) items)) :comment))
                do (decf position))
          (values (subseq items 0 position) (nthcdr position items))))))

(defun render-children-with-roles (layout list)
  "Emit every child of LIST with the role LAYOUT assigns it.  When some
child is a body form the list is stacked: the head children go in a .head
span and every body form is a row."
  (let ((items (layout-items layout list)))
    (if (some #'item-body-p items)
        (multiple-value-bind (head rows) (split-head-items items)
          (spinneret:with-html
            (:span.head (dolist (item head) (render-item layout list item))))
          (dolist (item rows) (render-item layout list item)))
        (dolist (item items) (render-item layout list item)))))

(defgeneric layout-classes (layout list)
  (:documentation "Extra CSS classes for the box of LIST under LAYOUT.")
  (:method ((layout layout) list)
    (and (some #'item-body-p (layout-items layout list)) '("stacked")))
  (:method ((layout clauses-layout) list)
    (list* "clauses" (call-next-method)))
  (:method ((layout pairs-layout) list)
    (let ((classes (call-next-method)))
      (if classes (list* "pairs" classes) classes)))
  (:method ((layout clause-layout) list)
    (declare (ignore list))
    '())
  (:method ((layout grid-layout) list)
    (declare (ignore list))
    '()))

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
  "A COND-style clause, one row of its parent's clause table (#4175NC): the
key or test in a .head cell, then the body forms stacked in a .rest cell."
  (let ((items (layout-items layout list)))
    (multiple-value-bind (head rows) (split-head-items items)
      (spinneret:with-html
        (:span.head (dolist (item head) (render-item layout list item)))
        (when rows
          (:span.rest (dolist (item rows) (render-item layout list item))))))))

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

(defgeneric layout-callee-p (layout)
  (:documentation "True when the first symbol of a list under LAYOUT names
an operator, worth a data-callee attribute; false for clauses, binding
grids, and lambda lists, whose first element is data.")
  (:method ((layout layout)) t)
  (:method ((layout clause-layout)) nil)
  (:method ((layout grid-layout)) nil)
  (:method ((layout lambda-list-layout)) nil))

(defmethod render-html ((list lisp-list))
  (let* ((operator (symbol-node-name (first (element-children list))))
         (layout (list-layout list *lisp-role*))
         (lambda-list (lambda-list-of layout list))
         (*prose-parameters* (if lambda-list
                                 (append (lambda-list-parameters lambda-list) *prose-parameters*)
                                 *prose-parameters*)))
    (spinneret:with-html
      (:div :class (apply #'role-class "list" (layout-classes layout list))
            :data-callee (and operator (layout-callee-p layout) (string-downcase operator))
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
                   (:a.definition-link :href href
                                       :data-card (definition-card-id definition)
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
          (:a.definition-link :href href :data-card (definition-card-id definition)
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
