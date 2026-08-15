;;;; An Org-subset reader for the luv wiki.
;;;;
;;;; The wiki pages are Org files, but they use a deliberately small and
;;;; regular subset: a title keyword, headings with property drawers,
;;;; paragraphs, plain and ordered lists, example and source blocks, simple
;;;; tables, bracket links, the light #ABC123 figure mention, and the six
;;;; inline emphasis markers.  This reader turns that subset into a tree of
;;;; CLOS elements.  It is not a general Org parser and does not try to be:
;;;; anything it does not recognize is kept as paragraph text, so a page can
;;;; never fail to build because of an unfamiliar construct.

(in-package #:luv.wiki)

;;; Element model

(defclass element ()
  ((children :initarg :children :initform '() :accessor element-children))
  (:documentation "A block-level piece of an Org document."))

(defclass document (element)
  ((pathname :initarg :pathname :initform nil :accessor document-pathname)
   (name :initarg :name :initform nil :accessor document-name
         :documentation "The page name, normally the file name without type.")
   (keywords :initarg :keywords :initform '() :accessor document-keywords
             :documentation "An alist of (\"title\" . \"value\") file keywords."))
  (:documentation "One wiki page: file keywords and a tree of headings."))

(defclass heading (element)
  ((level :initarg :level :accessor heading-level)
   (title :initarg :title :accessor heading-title
          :documentation "The heading title as a list of inline objects.")
   (keyword :initarg :keyword :initform nil :accessor heading-keyword
            :documentation "A work-mark status word such as NEXT, or NIL.")
   (properties :initarg :properties :initform '() :accessor heading-properties)
   (document :initarg :document :initform nil :accessor heading-document))
  (:documentation "An Org headline with its section and subheadings."))

(defclass paragraph (element) ()
  (:documentation "A run of prose; CHILDREN are inline objects."))

(defclass plain-list (element)
  ((ordered-p :initarg :ordered-p :initform nil :accessor plain-list-ordered-p))
  (:documentation "A bulleted or numbered list of LIST-ITEMs."))

(defclass list-item (element) ()
  (:documentation "One list item; CHILDREN are blocks, usually a paragraph."))

(defclass example-block (element)
  ((text :initarg :text :accessor block-text))
  (:documentation "A #+begin_example block, kept verbatim."))

(defclass src-block (example-block)
  ((language :initarg :language :initform nil :accessor src-block-language))
  (:documentation "A #+begin_src block with its language."))

(defclass table (element)
  ((rows :initarg :rows :accessor table-rows
         :documentation "A list of rows; each row is a list of inline lists.")
   (header-p :initarg :header-p :initform nil :accessor table-header-p
             :documentation "True when the first row is separated by a rule."))
  (:documentation "A simple Org table without formulas."))

;;; Inline objects.  Plain text is represented by strings.

(defclass inline-object () ())

(defclass emphasis (inline-object)
  ((kind :initarg :kind :accessor emphasis-kind
         :documentation "One of :bold :italic :underline :strike :verbatim :code.")
   (children :initarg :children :initform '() :accessor element-children))
  (:documentation "Emphasized text; VERBATIM and CODE hold one string child."))

(defclass link (inline-object)
  ((protocol :initarg :protocol :initform nil :accessor link-protocol
             :documentation "The link scheme as a string, e.g. \"file\", or NIL.")
   (path :initarg :path :accessor link-path)
   (search :initarg :search :initform nil :accessor link-search
           :documentation "The ::search part of a file link, if any.")
   (children :initarg :children :initform '() :accessor element-children
             :documentation "The description inlines, or NIL for a bare link."))
  (:documentation "An Org bracket link."))

(defclass mention (inline-object)
  ((id :initarg :id :accessor mention-id))
  (:documentation "A light #ABC123 reference to a figure."))

(defgeneric reference-id (inline)
  (:documentation "The figure ID an inline object refers to, or NIL.")
  (:method ((inline t)) nil)
  (:method ((inline mention)) (mention-id inline))
  (:method ((inline link))
    (when (equal (link-protocol inline) "id")
      (link-path inline))))

(defmethod heading-id ((heading heading))
  (cdr (assoc "ID" (heading-properties heading) :test #'string-equal)))

(defmethod document-title ((document document))
  (cdr (assoc "title" (document-keywords document) :test #'string-equal)))

(defmethod print-object ((heading heading) stream)
  (print-unreadable-object (heading stream :type t)
    (format stream "~A ~S" (heading-id heading) (inline-text (heading-title heading)))))

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type t)
    (format stream "~A" (document-name document))))

(defun map-elements (function element)
  "Call FUNCTION on ELEMENT and, depth first, on every block-level descendant."
  (funcall function element)
  (dolist (child (element-children element))
    (when (typep child 'element)
      (map-elements function child))))

(defun document-figures (document)
  "All headings in DOCUMENT that carry an ID, in document order."
  (let ((figures '()))
    (map-elements (lambda (element)
                    (when (and (typep element 'heading) (heading-id element))
                      (push element figures)))
                  document)
    (nreverse figures)))

(defun inline-text (inlines)
  "The plain text of a list of inline objects, dropping markup."
  (with-output-to-string (out)
    (labels ((walk (x)
               (etypecase x
                 (string (write-string x out))
                 (mention (format out "#~A" (mention-id x)))
                 (link (if (element-children x)
                           (mapc #'walk (element-children x))
                           (write-string (link-path x) out)))
                 (emphasis (mapc #'walk (element-children x))))))
      (mapc #'walk inlines))))

;;; Line utilities

(defparameter *work-mark-keywords* '("NEXT" "TODO" "WAIT" "DONE" "IDEA")
  "Status words that turn a heading into a work mark; see index.org.")

(defun blank-line-p (line)
  (every (lambda (c) (member c '(#\Space #\Tab))) line))

(defun indentation (line)
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line)
      (length line)))

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Return) string))

(defun starts-with (prefix string &key (start 0))
  (and (<= (+ start (length prefix)) (length string))
       (string-equal prefix string :start2 start :end2 (+ start (length prefix)))))

(defun keyword-line-p (line)
  "Return (values name value) when LINE is a #+name: value keyword line."
  (when (starts-with "#+" line)
    (let ((colon (position #\: line :start 2)))
      (when colon
        (values (subseq line 2 colon) (trim (subseq line (1+ colon))))))))

(defun heading-line-p (line)
  "Return the star count when LINE is an Org headline."
  (let ((stars (or (position-if-not (lambda (c) (char= c #\*)) line) (length line))))
    (and (plusp stars)
         (< stars (length line))
         (char= (char line stars) #\Space)
         stars)))

(defun list-item-start (line)
  "When LINE begins a list item, return (values indent ordered-p body-start)."
  (let* ((indent (indentation line))
         (rest (subseq line indent)))
    (cond ((and (>= (length rest) 2)
                (member (char rest 0) '(#\- #\+))
                (char= (char rest 1) #\Space))
           (values indent nil (+ indent 2)))
          ((and (>= (length rest) 2)
                (char= (char rest 0) #\*)
                (char= (char rest 1) #\Space)
                (plusp indent))
           (values indent nil (+ indent 2)))
          (t
           (let ((digits (or (position-if-not #'digit-char-p rest) (length rest))))
             (when (and (plusp digits)
                        (< (1+ digits) (length rest))
                        (member (char rest digits) '(#\. #\)))
                        (char= (char rest (1+ digits)) #\Space))
               (values indent t (+ indent digits 2))))))))

(defun block-begin-line-p (line)
  "Return (values kind parameters) for a #+begin_KIND line."
  (let ((line (trim line)))
    (when (starts-with "#+begin_" line)
      (let* ((space (or (position #\Space line) (length line)))
             (kind (string-downcase (subseq line 8 space)))
             (parameters (trim (subseq line space))))
        (values kind parameters)))))

(defun block-end-line-p (line kind)
  (string-equal (trim line) (concatenate 'string "#+end_" kind)))

(defun table-line-p (line)
  (let ((line (trim line)))
    (and (plusp (length line)) (char= (char line 0) #\|))))

;;; Inline reader

(defparameter *emphasis-markers*
  '((#\* . :bold) (#\/ . :italic) (#\_ . :underline)
    (#\+ . :strike) (#\= . :verbatim) (#\~ . :code)))

(defun emphasis-pre-char-p (c)
  (or (null c) (member c '(#\Space #\Tab #\Newline #\- #\( #\{ #\' #\" #\[))))

(defun emphasis-post-char-p (c)
  (or (null c)
      (member c '(#\Space #\Tab #\Newline #\- #\. #\, #\; #\: #\! #\? #\'
                  #\) #\} #\" #\[ #\]))))

(defun char-before (string index)
  (and (plusp index) (char string (1- index))))

(defun char-after (string index)
  (and (< (1+ index) (length string)) (char string (1+ index))))

(defun mention-end (string start)
  "If a #ABC123 mention begins at START, return the index after it."
  (let ((end (+ start 7)))
    (when (and (<= end (length string))
               (char= (char string start) #\#)
               (emphasis-pre-char-p (char-before string start))
               (loop for i from (1+ start) below end
                     for c = (char string i)
                     always (or (digit-char-p c) (upper-case-p c)))
               (not (and (< end (length string))
                         (alphanumericp (char string end)))))
      end)))

(defun read-link (string start)
  "If a [[...]] link begins at START, return (values link end)."
  (when (starts-with "[[" string :start start)
    (let ((close (search "]]" string :start2 (+ start 2))))
      (when close
        (let* ((inner (subseq string (+ start 2) close))
               (sep (search "][" inner))
               (target (if sep (subseq inner 0 sep) inner))
               (description (and sep (subseq inner (+ sep 2))))
               (colon (position #\: target))
               (protocol (and colon
                              (every #'alpha-char-p (subseq target 0 colon))
                              (plusp colon)
                              (subseq target 0 colon)))
               (path (if protocol (subseq target (1+ colon)) target))
               (search (search "::" path))
               (link (make-instance
                      'link
                      :protocol protocol
                      :path (if (and search (equal protocol "file")) (subseq path 0 search) path)
                      :search (and search (equal protocol "file") (subseq path (+ search 2)))
                      :children (and description (read-inlines description)))))
          ;; The description of a link may not itself contain a link, so
          ;; a stray "]]" inside it cannot fool us here.
          (values link (+ close 2)))))))

(defun read-emphasis (string start)
  "If an emphasis span begins at START, return (values emphasis end)."
  (let* ((marker (char string start))
         (kind (cdr (assoc marker *emphasis-markers*))))
    (when (and kind
               (emphasis-pre-char-p (char-before string start))
               (let ((next (char-after string start)))
                 (and next (not (member next '(#\Space #\Tab #\Newline)))
                      (not (char= next marker)))))
      (loop for end from (+ start 2) below (length string)
            for c = (char string end)
            do (cond ((char= c #\Newline)
                      ;; Emphasis may span one line break but not a blank line.
                      (when (and (< (1+ end) (length string))
                                 (char= (char string (1+ end)) #\Newline))
                        (return nil)))
                     ((and (char= c marker)
                           (not (member (char string (1- end)) '(#\Space #\Tab #\Newline)))
                           (emphasis-post-char-p (char-after string end)))
                      (let ((inner (subseq string (1+ start) end)))
                        (return
                          (values (make-instance
                                   'emphasis
                                   :kind kind
                                   :children (if (member kind '(:verbatim :code))
                                                 (list inner)
                                                 (read-inlines inner)))
                                  (1+ end))))))))))

(defun read-inlines (string)
  "Read STRING into a list of strings and inline objects."
  (let ((result '())
        (text (make-string-output-stream))
        (i 0)
        (n (length string)))
    (flet ((flush ()
             (let ((s (get-output-stream-string text)))
               (when (plusp (length s)) (push s result))))
           (emit (object end)
             (push object result)
             (setf i end)))
      (loop while (< i n)
            do (let ((c (char string i)))
                 (multiple-value-bind (object end)
                     (case c
                       (#\[ (read-link string i))
                       (#\# (let ((end (mention-end string i)))
                              (and end (values (make-instance 'mention :id (subseq string (1+ i) end)) end))))
                       (t (read-emphasis string i)))
                   (cond (object (flush) (emit object end))
                         (t (write-char c text) (incf i))))))
      (flush))
    (nreverse result)))

;;; Block reader
;;;
;;; The block reader works on a vector of lines and an index.  Each reader
;;; function takes the lines and a start index and returns (values element
;;; next-index), or NIL when the construct does not begin there.

(defun read-block-element (lines i)
  (multiple-value-bind (kind parameters) (block-begin-line-p (aref lines i))
    (when kind
      (let ((end (loop for j from (1+ i) below (length lines)
                       when (block-end-line-p (aref lines j) kind) return j)))
        (when end
          (let* ((body (loop for j from (1+ i) below end collect (aref lines j)))
                 (indent (reduce #'min (mapcar #'indentation (remove-if #'blank-line-p body))
                                 :initial-value most-positive-fixnum))
                 (text (format nil "~{~A~^~%~}"
                               (mapcar (lambda (line)
                                         (if (blank-line-p line) "" (subseq line (min indent (length line)))))
                                       body))))
            (values (if (string= kind "src")
                        (make-instance 'src-block :language
                                       (let ((space (position #\Space parameters)))
                                         (and (plusp (length parameters))
                                              (subseq parameters 0 space)))
                                       :text text)
                        (make-instance 'example-block :text text))
                    (1+ end))))))))

(defun read-table (lines i)
  (when (table-line-p (aref lines i))
    (let ((rows '())
          (header-p nil)
          (j i))
      (loop while (and (< j (length lines)) (table-line-p (aref lines j)))
            do (let ((line (trim (aref lines j))))
                 (if (starts-with "|-" line)
                     (when (= (length rows) 1) (setf header-p t))
                     (push (mapcar (lambda (cell) (read-inlines (trim cell)))
                                   (let ((cells (split-on-bar (string-trim "|" line))))
                                     cells))
                           rows)))
               (incf j))
      (values (make-instance 'table :rows (nreverse rows) :header-p header-p) j))))

(defun split-on-bar (string)
  (loop with start = 0
        for bar = (position #\| string :start start)
        collect (subseq string start bar)
        while bar
        do (setf start (1+ bar))))

(defun read-list (lines i)
  "Read a plain list whose first item begins at line I."
  (multiple-value-bind (indent ordered-p) (list-item-start (aref lines i))
    (when indent
      (let ((items '())
            (j i))
        (loop
          (unless (< j (length lines)) (return))
          (multiple-value-bind (item-indent item-ordered-p body-start)
              (list-item-start (aref lines j))
            (unless (and item-indent (= item-indent indent) (eq item-ordered-p ordered-p))
              (return))
            ;; Gather the item's lines: the first line's body, then every
            ;; following line indented deeper than the bullet, allowing a
            ;; blank line inside the item when more indented text follows.
            (let ((body (list (subseq (aref lines j) body-start)))
                  (k (1+ j)))
              (loop while (< k (length lines))
                    do (let ((line (aref lines k)))
                         (cond ((blank-line-p line)
                                (if (and (< (1+ k) (length lines))
                                         (not (blank-line-p (aref lines (1+ k))))
                                         (> (indentation (aref lines (1+ k))) indent))
                                    (push "" body)
                                    (return)))
                               ((> (indentation line) indent)
                                (push (subseq line (min (length line) body-start)) body))
                               (t (return))))
                       (incf k))
              (push (make-instance 'list-item
                                   :children (read-blocks (coerce (nreverse body) 'vector)))
                    items)
              (setf j k))))
        (values (make-instance 'plain-list :ordered-p ordered-p :children (nreverse items))
                j)))))

(defun read-paragraph (lines i)
  (let ((j i))
    (loop while (and (< j (length lines))
                     (not (blank-line-p (aref lines j)))
                     (or (= j i)
                         (not (or (list-item-start (aref lines j))
                                  (block-begin-line-p (aref lines j))
                                  (table-line-p (aref lines j))))))
          do (incf j))
    (values (make-instance 'paragraph
                           :children (read-inlines
                                      (format nil "~{~A~^~%~}"
                                              (loop for k from i below j collect (trim (aref lines k))))))
            j)))

(defparameter *block-readers*
  '(read-block-element read-table read-list read-paragraph)
  "Block readers tried in order; READ-PARAGRAPH always succeeds.")

(defun read-block (lines i)
  "Read the block beginning at line I, returning (values element next-index)."
  (dolist (reader *block-readers*)
    (multiple-value-bind (element next) (funcall reader lines i)
      (when element
        (return (values element next))))))

(defun read-blocks (lines)
  "Read the section body LINES (a vector) into a list of block elements."
  (let ((blocks '())
        (i 0))
    (loop while (< i (length lines))
          do (if (blank-line-p (aref lines i))
                 (incf i)
                 (multiple-value-bind (element next) (read-block lines i)
                   (push element blocks)
                   (setf i next))))
    (nreverse blocks)))

;;; Document reader

(defun read-property-drawer (lines i)
  "If a property drawer begins at line I, return (values alist next-index)."
  (when (and (< i (length lines)) (string-equal (trim (aref lines i)) ":PROPERTIES:"))
    (let ((properties '()))
      (loop for j from (1+ i) below (length lines)
            for line = (trim (aref lines j))
            do (cond ((string-equal line ":END:")
                      (return-from read-property-drawer (values (nreverse properties) (1+ j))))
                     ((and (plusp (length line)) (char= (char line 0) #\:))
                      (let ((close (position #\: line :start 1)))
                        (when close
                          (push (cons (subseq line 1 close) (trim (subseq line (1+ close))))
                                properties))))))
      nil)))

(defun read-heading-line (line stars)
  "Split a headline into (values keyword title-inlines)."
  (let* ((rest (trim (subseq line stars)))
         (space (position #\Space rest))
         (word (subseq rest 0 space)))
    (if (and space (member word *work-mark-keywords* :test #'string=))
        (values word (read-inlines (trim (subseq rest space))))
        (values nil (read-inlines rest)))))

(defun read-org-lines (lines &key name pathname)
  "Read the vector LINES of an Org file into a DOCUMENT."
  (let* ((document (make-instance 'document :name name :pathname pathname))
         (keywords '())
         (i 0)
         (n (length lines)))
    ;; File keywords come first; then the preamble section, then headings.
    (loop while (and (< i n)
                     (or (blank-line-p (aref lines i))
                         (keyword-line-p (aref lines i))))
          do (multiple-value-bind (key value) (keyword-line-p (aref lines i))
               (when key (push (cons key value) keywords)))
             (incf i))
    (setf (document-keywords document) (nreverse keywords))
    (labels ((section-end (start)
               (or (position-if #'heading-line-p lines :start start) n))
             (read-section (start)
               (let ((end (section-end start)))
                 (values (read-blocks (subseq lines start end)) end)))
             (read-headings (start min-level)
               ;; Read consecutive headings of level >= MIN-LEVEL starting at
               ;; START; return (values headings next-index).
               (let ((headings '())
                     (i start))
                 (loop
                   (unless (< i n) (return))
                   (let ((level (heading-line-p (aref lines i))))
                     (unless (and level (>= level min-level)) (return))
                     (multiple-value-bind (keyword title) (read-heading-line (aref lines i) level)
                       (multiple-value-bind (properties after-drawer) (read-property-drawer lines (1+ i))
                         (multiple-value-bind (blocks section-end)
                             (read-section (or after-drawer (1+ i)))
                           (multiple-value-bind (children next)
                               (read-headings section-end (1+ level))
                             (push (make-instance 'heading
                                                  :level level
                                                  :title title
                                                  :keyword keyword
                                                  :properties properties
                                                  :document document
                                                  :children (append blocks children))
                                   headings)
                             (setf i next)))))))
                 (values (nreverse headings) i))))
      (multiple-value-bind (preamble after) (read-section i)
        (multiple-value-bind (headings next) (read-headings after 1)
          (declare (ignore next))
          (setf (element-children document) (append preamble headings)))))
    document))

(defun read-org-string (string &key name pathname)
  (read-org-lines (coerce (uiop:split-string string :separator '(#\Newline)) 'vector)
                  :name name :pathname pathname))

(defun read-org-file (pathname &key (name (pathname-name pathname)))
  "Read the Org file at PATHNAME into a DOCUMENT."
  (read-org-lines (coerce (uiop:read-file-lines pathname) 'vector)
                  :name name :pathname pathname))
