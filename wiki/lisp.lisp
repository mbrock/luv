;;;; Reading Lisp source without loading it.
;;;;
;;;; The site is built by luv-wiki, which does not load luv.  To show
;;;; definitions in place we read source files with Eclector into a tree of
;;;; LISP-NODE objects: symbols become tokens (package prefix and name as
;;;; written), every node keeps its source range, comments and skipped input
;;;; are kept as nodes, and nothing is interned or evaluated.  Over the
;;;; top-level forms of the files ASDF lists for luv we build a DEFINITION
;;;; index: kind, name, file, line range, and the #ID figure mentions inside.

(in-package #:luv.wiki)

;;; Nodes

(defclass lisp-node ()
  ((start :initarg :start :initform nil :accessor node-start)
   (end :initarg :end :initform nil :accessor node-end)
   (text :initarg :text :initform nil :accessor node-text
         :documentation "The source text of the node."))
  (:documentation "A piece of Lisp source with its character range."))

(defclass lisp-list (lisp-node)
  ((children :initarg :children :initform '() :accessor element-children))
  (:documentation "A parenthesized list; CHILDREN are nodes in order."))

(defclass lisp-vector (lisp-list) ()
  (:documentation "A #( ) vector literal."))

(defclass lisp-symbol (lisp-node)
  ((name :initarg :name :accessor lisp-symbol-name)
   (package :initarg :package :initform nil :accessor lisp-symbol-package
            :documentation "The package prefix as written, or NIL for the
current package; \"KEYWORD\" for keywords; :uninterned for #:.")
   (external-p :initarg :external-p :initform nil :accessor lisp-symbol-external-p))
  (:documentation "A symbol token; nothing is interned."))

(defclass lisp-atom (lisp-node) ()
  (:documentation "Any other atom, kept as its source text: numbers, strings,
characters, pathnames, and reader-macro results we do not model."))

(defclass lisp-string (lisp-atom) ())
(defclass lisp-number (lisp-atom) ())
(defclass lisp-character (lisp-atom) ())

(defclass lisp-prefix (lisp-node)
  ((prefix :initarg :prefix :accessor lisp-prefix-string
           :documentation "One of ' #' ` , ,@ #.")
   (child :initarg :child :accessor lisp-prefix-child))
  (:documentation "A quote-like prefix applied to one form."))

(defclass lisp-conditional (lisp-node)
  ((sign :initarg :sign :accessor lisp-conditional-sign
         :documentation "The string \"#+\" or \"#-\".")
   (feature :initarg :feature :accessor lisp-conditional-feature)
   (form :initarg :form :initform nil :accessor lisp-conditional-form
         :documentation "The guarded form node, or NIL when it was skipped;
then TEXT of the whole node still shows it."))
  (:documentation "A #+ or #- guarded form."))

(defclass lisp-comment (lisp-node) ()
  (:documentation "A ; or #| |# comment, TEXT included."))

(defclass lisp-skipped (lisp-node) ()
  (:documentation "Input the reader skipped for another reason."))

(defmethod print-object ((node lisp-node) stream)
  (print-unreadable-object (node stream :type t)
    (let ((text (node-text node)))
      (when text
        (format stream "~S" (if (> (length text) 40)
                                (concatenate 'string (subseq text 0 37) "...")
                                text))))))

(defun node-line (node line-starts)
  "The 1-based line on which NODE starts, given the vector LINE-STARTS."
  (line-of-position (node-start node) line-starts))

(defun line-of-position (position line-starts)
  (let ((low 0) (high (1- (length line-starts))))
    ;; Binary search for the last line start <= POSITION.
    (loop while (< low high)
          do (let ((mid (ceiling (+ low high) 2)))
               (if (<= (aref line-starts mid) position)
                   (setf low mid)
                   (setf high (1- mid)))))
    (1+ low)))

(defun line-starts (text)
  (let ((starts (list 0)))
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline) do (push (1+ i) starts))
    (coerce (nreverse starts) 'vector)))

;;; The Eclector client

(defclass source-reader (eclector.parse-result:parse-result-client)
  ((text :initarg :text :accessor reader-text
         :documentation "The whole source text, for slicing node text."))
  (:documentation "An Eclector client that builds LISP-NODE trees."))

(defstruct (symbol-token (:constructor make-symbol-token (package name external-p)))
  package name external-p)

(defmethod eclector.reader:interpret-symbol ((client source-reader) input-stream
                                             package-indicator symbol-name internp)
  (declare (ignore input-stream))
  (make-symbol-token (cond ((eq package-indicator :current) nil)
                           ((eq package-indicator :keyword) "KEYWORD")
                           ((null package-indicator) :uninterned)
                           (t package-indicator))
                     symbol-name
                     (and (stringp package-indicator) (not internp))))

;;; Quote-like reader macros return marker lists so that
;;; MAKE-EXPRESSION-RESULT can recognize them; the material is a node.

(defmethod eclector.reader:wrap-in-quote ((client source-reader) material)
  (list :prefix "'" material))
(defmethod eclector.reader:wrap-in-function ((client source-reader) name)
  (list :prefix "#'" name))
(defmethod eclector.reader:wrap-in-quasiquote ((client source-reader) form)
  (list :prefix "`" form))
(defmethod eclector.reader:wrap-in-unquote ((client source-reader) form)
  (list :prefix "," form))
(defmethod eclector.reader:wrap-in-unquote-splicing ((client source-reader) form)
  (list :prefix ",@" form))

(defmethod eclector.reader:evaluate-expression ((client source-reader) expression)
  "#. is never evaluated; it becomes a prefix node."
  (list :prefix "#." expression))

(defmethod eclector.reader:evaluate-feature-expression ((client source-reader) expression)
  "Feature expressions are tokens, not symbols; evaluate them against this
image's *FEATURES* by name so the reader can proceed.  Both outcomes are
kept visible: a true branch becomes a LISP-CONDITIONAL with its form and a
false branch a LISP-CONDITIONAL whose text still shows it."
  (labels ((feature-p (x)
             (typecase x
               (symbol-token
                (and (member (symbol-token-name x) *features* :test #'string-equal) t))
               (cons
                (let ((operator (and (symbol-token-p (first x))
                                     (string-upcase (symbol-token-name (first x))))))
                  (cond ((equal operator "AND") (every #'feature-p (rest x)))
                        ((equal operator "OR") (some #'feature-p (rest x)))
                        ((equal operator "NOT") (not (feature-p (second x))))
                        (t nil))))
               (t nil))))
    (feature-p expression)))

(defmethod eclector.reader:check-feature-expression ((client source-reader) expression)
  (declare (ignore expression))
  t)

(defmethod eclector.reader:make-structure-instance ((client source-reader) name initargs)
  (list :structure name initargs))

(defun source-slice (client source)
  (subseq (reader-text client) (car source) (cdr source)))

(defun conditional-source-p (client source)
  "Return \"#+\" or \"#-\" when the source range begins with one."
  (let ((text (reader-text client))
        (start (car source)))
    (and (< (1+ start) (length text))
         (char= (char text start) #\#)
         (member (char text (1+ start)) '(#\+ #\-))
         (subseq text start (+ start 2)))))

(defmethod eclector.parse-result:make-expression-result
    ((client source-reader) result children source)
  (let ((start (car source))
        (end (cdr source))
        (text (source-slice client source)))
    (flet ((node (class &rest initargs)
             (apply #'make-instance class :start start :end end :text text initargs)))
      (cond
        ;; #+feature form / #-feature form: two children, the feature and the form.
        ((and (= (length children) 2) (conditional-source-p client source))
         (node 'lisp-conditional
               :sign (conditional-source-p client source)
               :feature (first children)
               :form (second children)))
        ((and (consp result) (eq (first result) :prefix))
         (node 'lisp-prefix :prefix (second result)
                            :child (or (first (last children)) (third result))))
        ((symbol-token-p result)
         (node 'lisp-symbol
               :name (symbol-token-name result)
               :package (symbol-token-package result)
               :external-p (symbol-token-external-p result)))
        ((stringp result) (node 'lisp-string))
        ((numberp result) (node 'lisp-number))
        ((characterp result) (node 'lisp-character))
        ((vectorp result) (node 'lisp-vector :children children))
        ((consp result) (node 'lisp-list :children children))
        ((null result)
         (if (and (plusp (length text)) (char= (char text 0) #\())
             (node 'lisp-list :children children)
             (node 'lisp-symbol :name "NIL" :package nil)))
        (t (node 'lisp-atom))))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client source-reader) stream reason children source)
  (declare (ignore stream))
  (let* ((text (string-right-trim '(#\Newline #\Return) (source-slice client source)))
         (class (cond ((and (consp reason) (eq (car reason) :line-comment)) 'lisp-comment)
                      ((eq reason :block-comment) 'lisp-comment)
                      ((and (consp reason) (member (car reason) '(:sharpsign-plus :sharpsign-minus)))
                       'lisp-conditional)
                      (t 'lisp-skipped))))
    (if (eq class 'lisp-conditional)
        (make-instance class :start (car source) :end (cdr source) :text text
                             :sign (if (eq (car reason) :sharpsign-plus) "#+" "#-")
                             :feature (first children)
                             :form nil)
        (make-instance class :start (car source) :end (cdr source) :text text))))

;;; Reading files

(defun read-lisp-string (text &key (name "string"))
  "Read TEXT into a list of top-level LISP-NODEs, comments included, in
order.  Reader errors are recovered from where Eclector offers a restart and
otherwise end the read; NAME labels warnings."
  (let* ((client (make-instance 'source-reader :text text))
         (stream (make-string-input-stream text))
         (nodes '())
         (eof (list :eof)))
    (loop
      (multiple-value-bind (result orphans)
          (handler-bind ((error
                           (lambda (condition)
                             (let ((restart (find-restart 'eclector.reader:recover condition)))
                               (if restart
                                   (invoke-restart restart)
                                   (progn
                                     (warn "Stopped reading ~A: ~A" name condition)
                                     (return)))))))
            (eclector.parse-result:read client stream nil eof))
        ;; READ returns the parse result and the orphan results (comments
        ;; and other skipped input before it), or EOF and the orphans.
        (setf nodes (append (reverse orphans) nodes))
        (when (eq result eof) (return))
        (push result nodes)))
    (merge-adjacent-comments (nreverse nodes) text)))

(defun merge-adjacent-comments (nodes text)
  "Join runs of line comments separated only by a newline into one
LISP-COMMENT, recursively inside lists, so a comment block is one node."
  (let ((result '()))
    (dolist (node nodes)
      (let ((previous (first result)))
        (cond ((and (typep node 'lisp-comment) (typep previous 'lisp-comment)
                    (adjacent-lines-p previous node text))
               (setf (node-end previous) (node-end node)
                     (node-text previous) (string-right-trim
                                           '(#\Newline #\Return)
                                           (subseq text (node-start previous) (node-end node)))))
              (t
               (when (typep node 'lisp-list)
                 (setf (element-children node)
                       (merge-adjacent-comments (element-children node) text)))
               (push node result)))))
    (nreverse result)))

(defun adjacent-lines-p (a b text)
  "True when node B starts on the line after the last line of node A, with
only indentation between: consecutive comment lines.  A's range may or may
not include its trailing newline, so count from the end of its text."
  (let* ((a-end (+ (node-start a) (length (node-text a))))
         (between (subseq text a-end (node-start b))))
    (and (= 1 (count #\Newline between))
         (every (lambda (c) (member c '(#\Newline #\Return #\Space #\Tab))) between))))

(defun read-lisp-file (pathname)
  (read-lisp-string (uiop:read-file-string pathname) :name (namestring pathname)))

;;; Definitions

(defclass definition ()
  ((kind :initarg :kind :accessor definition-kind
         :documentation "The operator name, downcased, e.g. \"defun\".")
   (name :initarg :name :accessor definition-name
         :documentation "The name as written, e.g. \"foo\" or \"(setf foo)\".")
   (qualifiers :initarg :qualifiers :initform '() :accessor definition-qualifiers)
   (specializers :initarg :specializers :initform '() :accessor definition-specializers
                 :documentation "For a method, the specializer texts of its
required parameters, e.g. (\"block-world\" \"t\").")
   (package :initarg :package :initform nil :accessor definition-package
            :documentation "The IN-PACKAGE in force, as written, or NIL.")
   (pathname :initarg :pathname :accessor definition-pathname)
   (line :initarg :line :accessor definition-line)
   (end-line :initarg :end-line :accessor definition-end-line)
   (node :initarg :node :accessor definition-node)
   (comments :initarg :comments :initform '() :accessor definition-comments
             :documentation "Comment nodes immediately preceding the form.")
   (mentions :initarg :mentions :initform '() :accessor definition-mentions
             :documentation "Figure IDs mentioned in the form or its comments."))
  (:documentation "One top-level defining form of a source file."))

(defmethod print-object ((definition definition) stream)
  (print-unreadable-object (definition stream :type t)
    (format stream "~A ~A" (definition-kind definition) (definition-name definition))))

(defun definition-file-name (definition)
  (file-namestring (definition-pathname definition)))

(defun symbol-node-name (node)
  (and (typep node 'lisp-symbol) (lisp-symbol-name node)))

(defun text-mentions (text)
  "Figure IDs mentioned as #ID in TEXT, in order, without duplicates."
  (let ((ids '()))
    (loop for i from 0 below (length text)
          do (when (char= (char text i) #\#)
               (let ((end (mention-end text i)))
                 (when end
                   (pushnew (subseq text (1+ i) end) ids :test #'string=)))))
    (nreverse ids)))

(defparameter *defining-operators*
  '("defun" "defmacro" "defgeneric" "defmethod" "defclass" "defstruct" "deftype"
    "defvar" "defparameter" "defconstant" "define-condition" "defpackage"
    "define-symbol-macro" "defsetf" "define-modify-macro" "deftest")
  "Operators whose top-level forms become definitions.  Any other operator
whose name starts with DEF or DEFINE- is accepted too.")

(defun defining-operator-p (name)
  (let ((name (string-downcase name)))
    (or (member name *defining-operators* :test #'string=)
        (starts-with "def" name))))

(defun form-definition (node comments pathname line-starts package)
  "Make a DEFINITION for the top-level list NODE if it defines something."
  (let* ((children (element-children node))
         (operator (symbol-node-name (first children))))
    (when (and operator (defining-operator-p operator) (second children))
      (let* ((name-node (second children))
             (qualifiers (loop for child in (cddr children)
                               while (typep child 'lisp-symbol)
                               collect (node-text child)))
             (method-p (string-equal operator "defmethod"))
             (lambda-list (and method-p (find-if (lambda (c) (typep c 'lisp-list)) (cddr children))))
             (specializers (and lambda-list
                                (loop for parameter in (element-children lambda-list)
                                      until (and (typep parameter 'lisp-symbol)
                                                 (char= (char (lisp-symbol-name parameter) 0) #\&))
                                      collect (if (and (typep parameter 'lisp-list)
                                                       (second (element-children parameter)))
                                                  (node-text (second (element-children parameter)))
                                                  "t"))))
             (texts (cons (node-text node) (mapcar #'node-text comments))))
        (make-instance 'definition
                       :kind (string-downcase operator)
                       :name (node-text name-node)
                       :qualifiers (if method-p qualifiers '())
                       :specializers specializers
                       :package package
                       :pathname pathname
                       :line (node-line node line-starts)
                       :end-line (line-of-position (max 0 (1- (node-end node))) line-starts)
                       :node node
                       :comments comments
                       :mentions (remove-duplicates (mapcan #'text-mentions texts)
                                                    :test #'string= :from-end t))))))

(defun nodes-definitions (nodes pathname line-starts)
  "The DEFINITIONs among the top-level NODES of a file, in order.  Comments
that precede a form belong to it, and IN-PACKAGE forms set the package."
  (let ((package nil)
        (pending '())
        (definitions '()))
    (dolist (node nodes)
      (typecase node
        (lisp-comment (push node pending))
        (lisp-list
         (let ((operator (symbol-node-name (first (element-children node)))))
           (when (and operator (string-equal operator "in-package"))
             (setf package (node-text (second (element-children node)))))
           (let ((definition (form-definition node (reverse pending) pathname line-starts package)))
             (when definition (push definition definitions)))
           (setf pending '())))
        (t (setf pending '()))))
    (nreverse definitions)))

(defun file-definitions (pathname &optional (text (uiop:read-file-string pathname)))
  "The DEFINITIONs of the source file at PATHNAME, in order."
  (nodes-definitions (read-lisp-string text :name (namestring pathname))
                     pathname
                     (line-starts text)))

(defun definition-references (definitions)
  "A hash table from figure ID to the DEFINITIONs mentioning it."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (definition definitions)
      (dolist (id (definition-mentions definition))
        (push definition (gethash id table))))
    (maphash (lambda (id list) (setf (gethash id table) (nreverse list))) table)
    table))

(defun find-definition (name definitions &key kind)
  "The first definition whose name matches NAME, case-insensitively, with or
without a package prefix on either side."
  (flet ((bare (string)
           (let ((colon (position #\: string :from-end t)))
             (if colon (subseq string (1+ colon)) string))))
    (find-if (lambda (definition)
               (and (or (null kind) (string-equal kind (definition-kind definition)))
                    (or (string-equal name (definition-name definition))
                        (string-equal (bare name) (bare (definition-name definition))))))
             definitions)))
