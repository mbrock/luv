;;;; Browsing the system's source from the wiki.
;;;;
;;;; A SOURCE-FILE is one Lisp file read by the Eclector client: its text,
;;;; top-level nodes, and definitions.  Each becomes a page under source/
;;;; drawn entirely as dexp boxes, with every top-level form anchored by
;;;; its line and a table of its definitions on top; source.html lists the
;;;; files by system.  Symbols that name a definition anywhere in the corpus
;;;; link to it, "Referenced from" summaries and lisp: links point into
;;;; these pages, and #ID mentions in code point back at the wiki.

(in-package #:luv.wiki)

(defclass source-file ()
  ((pathname :initarg :pathname :accessor source-file-pathname)
   (relative-path :initarg :relative-path :accessor source-file-relative-path
                  :documentation "The path relative to the repository root, e.g.
\"luvcraft/mesher.lisp\".")
   (system-name :initarg :system-name :accessor source-file-system-name)
   (text :initarg :text :accessor source-file-text)
   (nodes :initarg :nodes :accessor source-file-nodes)
   (line-starts :initarg :line-starts :accessor source-file-line-starts)
   (definitions :initarg :definitions :initform '() :accessor source-file-definitions))
  (:documentation "One Lisp source file of the system as read by luv-wiki."))

(defmethod print-object ((file source-file) stream)
  (print-unreadable-object (file stream :type t)
    (format stream "~A" (source-file-relative-path file))))

(defun read-source-file (pathname &key relative-path system-name)
  "Read the file at PATHNAME into a SOURCE-FILE with its definitions."
  (let* ((text (uiop:read-file-string pathname))
         (nodes (read-lisp-string text :name (namestring pathname)))
         (line-starts (line-starts text)))
    (make-instance 'source-file
                   :pathname pathname
                   :relative-path (or relative-path (file-namestring pathname))
                   :system-name system-name
                   :text text
                   :nodes nodes
                   :line-starts line-starts
                   :definitions (nodes-definitions nodes pathname line-starts))))

(defun source-file-package (file)
  "The package named by the file's first IN-PACKAGE form, as written."
  (dolist (node (source-file-nodes file))
    (when (typep node 'lisp-list)
      (let ((children (element-children node)))
        (when (and (symbol-node-name (first children))
                   (string-equal (symbol-node-name (first children)) "in-package")
                   (second children))
          (return (node-text (second children))))))))

(defun source-page-name (file)
  "The site-relative page for FILE, e.g. \"source/luvcraft/mesher.lisp.html\"."
  (concatenate 'string "source/" (source-file-relative-path file) ".html"))

(defun page-prefix-for (page-name)
  "The relative prefix that leads from PAGE-NAME back to the site root."
  (with-output-to-string (out)
    (loop repeat (count #\/ page-name) do (write-string "../" out))))

(defun definition-source-file (definition &optional (site *site*))
  (and site
       (find (definition-pathname definition) (site-source-files site)
             :key #'source-file-pathname :test #'equal)))

(defun definition-page-href (definition &optional (site *site*))
  "The href of DEFINITION's line in its source page, or NIL when the site
has no page for its file."
  (let ((file (definition-source-file definition site)))
    (when file
      (format nil "~A~A#L~D" *page-prefix* (source-page-name file) (definition-line definition)))))

;;; Rendering

(defun kind-label (kind)
  "A short label for a definition KIND: defmethod -> method, define-x -> x."
  (cond ((string= kind "defun") "function")
        ((string= kind "defparameter") "parameter")
        ((string= kind "defconstant") "constant")
        ((string= kind "defvar") "variable")
        ((starts-with "define-" kind) (subseq kind 7))
        ((starts-with "def" kind) (subseq kind 3))
        (t kind)))

(defun render-definition-entry (definition &key href)
  "One line of a definitions index: kind badge, name, method signature, line."
  (spinneret:with-html
    (:li.definition-entry
     (:span :class (format nil "kind kind-~A" (definition-kind definition))
            :title (definition-kind definition)
            (kind-label (definition-kind definition)))
     (:a.name :href href (definition-name definition))
     (let ((specializers (definition-specializers definition)))
       ;; Trailing T specializers say nothing; drop them.
       (loop while (and specializers (string= (car (last specializers)) "t"))
             do (setf specializers (butlast specializers)))
       (when (or (definition-qualifiers definition) specializers)
         (:span.signature
          (format nil "~{~A ~}~@[(~{~A~^ ~})~]"
                  (definition-qualifiers definition) specializers))))
     (:span.line (format nil "~D" (definition-line definition))))))

(defun render-source-toc (file &key (prefix ""))
  "The definitions of FILE as a scannable list linking to their lines."
  (let ((definitions (source-file-definitions file)))
    (when definitions
      (spinneret:with-html
        (:nav.definitions
         (:ul
          (dolist (definition definitions)
            (render-definition-entry
             definition
             :href (format nil "~A#L~D" prefix (definition-line definition))))))))))

(defun render-source-page (file)
  "Emit the page for FILE: its definitions table and every top-level form
as dexp boxes, each anchored by its starting line."
  (let* ((page (source-page-name file))
         (*page-prefix* (page-prefix-for page))
         (*rendering-document* nil)
         (line-starts (source-file-line-starts file))
         (title (source-file-relative-path file)))
    (render-page-frame
     title
     (lambda ()
       (spinneret:with-html
         (:h1.source-title title)
         (:p.source-meta
          (when (source-file-system-name file)
            (spinneret:html "system ")
            (:code (source-file-system-name file))
            (spinneret:html " · "))
          (format nil "~D definition~:P · " (length (source-file-definitions file)))
          (:a :href (concatenate 'string (site-source-url *site*) (source-file-relative-path file))
              "on GitHub"))
         (render-source-toc file)
         (let ((*lisp-package* (source-file-package file)))
           (:div.lisp.source
            (dolist (node (source-file-nodes file))
              (let ((*lisp-role* nil))
                (:div.toplevel :id (format nil "L~D" (node-line node line-starts))
                  (render-html node))))))))
     :body-class "source-page")))

(defun render-source-index (site)
  "Emit source.html: the files of the corpus grouped by system."
  (let ((*page-prefix* "")
        (*rendering-document* nil)
        (groups '()))
    (dolist (file (site-source-files site))
      (let ((group (assoc (source-file-system-name file) groups :test #'equal)))
        (if group
            (push file (cdr group))
            (push (list (source-file-system-name file) file) groups))))
    (setf groups (sort (mapcar (lambda (g) (cons (car g) (reverse (cdr g)))) groups)
                       #'string< :key (lambda (g) (or (car g) ""))))
    (render-page-frame
     "Source"
     (lambda ()
       (spinneret:with-html
         (:h1 "Source")
         (:p "Every source file of the systems the wiki reads, drawn as dexp boxes.
Definitions are anchored by line; symbols link to their definitions; "
             (:code "#ID") " mentions link to figures.")
         (dolist (group groups)
           (:section.source-group
            (:h2 (:code (or (car group) "other")))
            (dolist (file (cdr group))
              (:details.source-file
               (:summary
                (:a :href (source-page-name file) (source-file-relative-path file))
                (:span.count (format nil "~D" (length (source-file-definitions file)))))
               (render-source-toc file :prefix (source-page-name file)))))))))))
