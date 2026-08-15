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
         (*page-kind* "source-file")
         (*page-definition-cards* (make-hash-table :test 'eq))
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
                  (render-html node))))))
         (render-definition-cards)
         (render-figure-cards
          (loop for definition in (source-file-definitions file)
                append (definition-mentions definition)))))
     :body-class "wide source-page"
     :status title
     :right (or (source-file-system-name file) "source"))))

(defvar *file-cards* nil
  "While the source index renders: files whose definitions get a card.")

(defun file-card-id (file)
  (format nil "file-~A" (substitute-if #\- (lambda (c) (member c '(#\/ #\.)))
                                       (source-file-relative-path file))))

(defun render-file-entry (file)
  "A file's relative path linking to its page, the file name emphasized,
and a count button that shows the definitions in a popover."
  (let* ((path (source-file-relative-path file))
         (slash (position #\/ path :from-end t)))
    (push file *file-cards*)
    (spinneret:with-html
      (:span.file-entry
       (:a.path :href (source-page-name file)
                (when slash (:span.directory (subseq path 0 (1+ slash))))
                (:span.file (subseq path (if slash (1+ slash) 0))))
       (:button.count :type "button" :data-card (file-card-id file)
                      :title "definitions"
                      (format nil "~D" (length (source-file-definitions file))))))))

(defun render-file-cards ()
  "Hidden cards holding each listed file's definitions, for the popover."
  (spinneret:with-html
    (:div.figure-cards :hidden t
      (dolist (file (reverse *file-cards*))
        (:div.figure-card.file-card :id (file-card-id file)
          (:a.card-title :href (source-page-name file) (source-file-relative-path file))
          (render-source-toc file :prefix (source-page-name file)))))))

(defun graph-systems (site)
  "The systems worth drawing: not test systems and not the aggregate root
system that merely depends on everything."
  (remove-if (lambda (entry)
               (let ((name (system-entry-name entry)))
                 (or (search "/tests" name)
                     (not (find #\/ name)))))
             (site-systems site)))

(defun transitive-reduction (edges)
  "EDGES is a list of (from . to).  Return the edges not implied by a longer
path, so a layered drawing shows only the essential dependencies."
  (let ((successors (make-hash-table :test 'equal)))
    (loop for (from . to) in edges do (push to (gethash from successors)))
    (labels ((reaches-p (from to &optional seen)
               ;; Is there a path FROM -> ... -> TO of length >= 2?
               (some (lambda (next)
                       (and (not (member next seen :test #'equal))
                            (or (and (not (equal next to))
                                     (member to (gethash next successors) :test #'equal))
                                (and (not (equal next to))
                                     (reaches-p next to (cons next seen))))))
                     (gethash from successors))))
      (remove-if (lambda (edge) (reaches-p (car edge) (cdr edge))) edges))))

(defun render-system-graph (site)
  "The systems and their essential dependencies as a Mermaid flowchart,
fundamentals at the top."
  (let* ((entries (graph-systems site))
         (names (mapcar #'system-entry-name entries))
         (edges (loop for entry in entries
                      append (loop for dependency in (system-entry-depends-on entry)
                                   when (member dependency names :test #'string=)
                                     collect (cons dependency (system-entry-name entry)))))
         (edges (transitive-reduction edges)))
    (flet ((node (name) (substitute #\_ #\/ name)))
      (spinneret:with-html
        (:pre.mermaid.system-graph
         (with-output-to-string (out)
           (format out "%%{init: {\"flowchart\": {\"nodeSpacing\": 14, \"rankSpacing\": 30, \"curve\": \"basis\"}}}%%~%")
           (format out "flowchart TB~%")
           (dolist (name names)
             (format out "  ~A[\"~A\"]~%" (node name) (subseq name 4)))
           (loop for (from . to) in edges
                 do (format out "  ~A --> ~A~%" (node from) (node to)))))))))

(defun render-source-index (site)
  "Emit source.html: the dependency graph of the systems, then one dense
table in dependency order — system, description, files — where a file's
count opens its definitions in a popover."
  (let ((*page-prefix* "")
        (*page-kind* "source")
        (*rendering-document* nil)
        (*file-cards* '()))
    (flet ((system-anchor (name) (concatenate 'string "system-" (substitute #\- #\/ name))))
      (render-page-frame
       "Source"
       (lambda ()
         (spinneret:with-html
           (:h1 "Source")
           (:p.lede "The systems of luv, fundamentals first.  A file's count opens its
definitions; symbols in the pages link to their definitions and "
                    (:code "#ID") " mentions link to figures.")
           (:p.graph-note "Dependencies between the systems, essential edges only (test
systems and the aggregate " (:code "luv") " left out); names drop the "
                          (:code "luv/") " prefix.")
           (render-system-graph site)
           (:table.systems
            (:thead (:tr (:th "system") (:th "description") (:th "files")))
            (:tbody
             (dolist (entry (site-systems site))
               (:tr :id (system-anchor (system-entry-name entry))
                (:td.system-name (system-entry-name entry))
                (:td.system-description (or (system-entry-description entry) ""))
                (:td.system-files
                 (dolist (file (system-entry-files entry))
                   (render-file-entry file)))))))
           (render-file-cards)))
       :body-class "wide source-index"))))
