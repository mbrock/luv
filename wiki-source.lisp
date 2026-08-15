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

(defun render-definition-entry (definition &key href (name-p t))
  "One line of a definitions index: kind, name (or only the method signature
when NAME-P is false, under its generic), with file:line as the tooltip."
  (let ((specializers (definition-specializers definition)))
    ;; Trailing T specializers say nothing; drop them.
    (loop while (and specializers (string= (car (last specializers)) "t"))
          do (setf specializers (butlast specializers)))
    (spinneret:with-html
      (:li :class (if name-p "definition-entry" "definition-entry method-entry")
       (:span :class (format nil "kind kind-~A" (definition-kind definition))
              (definition-kind definition))
       (:a.name :href href
                :title (format nil "~A:~D" (definition-file-name definition) (definition-line definition))
                (if name-p
                    (definition-name definition)
                    (spinneret:html ""))
                (when (or (definition-qualifiers definition) specializers)
                  (:span.signature
                   (format nil "~{~A ~}~@[(~{~A~^ ~})~]"
                           (definition-qualifiers definition) specializers))))))))

(defun group-methods (definitions)
  "DEFINITIONS in order, but with each method attached to the generic (or
first method) of the same name: a list of (definition . methods)."
  (let ((groups '())
        (index (make-hash-table :test 'equalp)))
    (dolist (definition definitions)
      (let ((name (definition-name definition)))
        (cond ((string= (definition-kind definition) "defgeneric")
               (let ((existing (gethash name index)))
                 (if (and existing (string= (definition-kind (car existing)) "defmethod"))
                     ;; Methods came first: the generic takes over the group.
                     (setf (car existing) definition
                           (cdr existing) (cons (car existing) (cdr existing)))
                     (let ((group (cons definition '())))
                       (setf (gethash name index) group)
                       (push group groups)))))
              ((string= (definition-kind definition) "defmethod")
               (let ((existing (gethash name index)))
                 (if existing
                     (push definition (cdr existing))
                     (let ((group (cons definition '())))
                       (setf (gethash name index) group)
                       (push group groups)))))
              (t (push (cons definition '()) groups)))))
    (mapcar (lambda (group) (cons (car group) (reverse (cdr group))))
            (nreverse groups))))

(defun render-source-toc (file &key (prefix ""))
  "The definitions of FILE as a scannable list linking to their lines,
methods grouped under their generic."
  (let ((definitions (source-file-definitions file)))
    (when definitions
      (spinneret:with-html
        (:nav.definitions
         (:ul
          (loop for (head . methods) in (group-methods definitions)
                do (render-definition-entry
                    head :href (format nil "~A#L~D" prefix (definition-line head)))
                   (dolist (method methods)
                     (render-definition-entry
                      method :href (format nil "~A#L~D" prefix (definition-line method))
                      :name-p nil)))))))))

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

(defun render-file-summary (file)
  "The file's relative path with the file name itself emphasized."
  (let* ((path (source-file-relative-path file))
         (slash (position #\/ path :from-end t)))
    (spinneret:with-html
      (:a.path :href (source-page-name file)
               (when slash (:span.directory (subseq path 0 (1+ slash))))
               (:span.file (subseq path (if slash (1+ slash) 0))))
      (:span.count (format nil "~D" (length (source-file-definitions file)))))))

(defun render-source-index (site)
  "Emit source.html: the ASDF systems of the code in dependency order, each
with its description, dependencies, and files, the files expandable to their
definitions."
  (let ((*page-prefix* "")
        (*rendering-document* nil))
    (render-page-frame
     "Source"
     (lambda ()
       (spinneret:with-html
         (:h1 "Source")
         (:p "The systems of luv in dependency order, fundamentals first, with
their files.  Open a file to see its definitions; symbols in the pages link
to their definitions and " (:code "#ID") " mentions link to figures.")
         (dolist (entry (site-systems site))
           (:section.system :id (concatenate 'string "system-" (substitute #\- #\/ (system-entry-name entry)))
            (:h2 (:code (system-entry-name entry)))
            (when (system-entry-description entry)
              (:p.description (system-entry-description entry)))
            (when (system-entry-depends-on entry)
              (:p.depends "depends on "
                (loop for name in (system-entry-depends-on entry)
                      for first = t then nil
                      do (unless first (spinneret:html ", "))
                         (:a :href (concatenate 'string "#system-" (substitute #\- #\/ name))
                             (:code name)))))
            (dolist (file (system-entry-files entry))
              (:details.source-file
               (:summary (render-file-summary file))
               (render-source-toc file :prefix (source-page-name file)))))))))))
