;;;; Browsing the system's source from the wiki.
;;;;
;;;; A SOURCE-FILE is one Lisp file read by the Eclector client: its text,
;;;; top-level nodes, and definitions.  Each becomes a page under source/
;;;; drawn entirely as dexp boxes, with every top-level form anchored by
;;;; its line and a table of its definitions on top, and a sidebar of every
;;;; system's files beside it; source.html lists the files by system.
;;;; Symbols that name a definition anywhere in the corpus
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

(defun render-file-path (path)
  "PATH as a directory part in the muted face and the file name emphasized."
  (let ((slash (position #\/ path :from-end t)))
    (spinneret:with-html
      (when slash (:span.directory (subseq path 0 (1+ slash))))
      (:span.file (subseq path (if slash (1+ slash) 0))))))

(defun render-source-sidebar (current &optional (site *site*))
  "The systems and their files as a compact list beside a source page: one
DETAILS per system, the one holding CURRENT open and CURRENT marked."
  (spinneret:with-html
    (:aside.source-nav :aria-label "Source files"
      (:p.source-nav-title
       (:a :href (concatenate 'string *page-prefix* "source.html") "Source"))
      (dolist (entry (site-systems site))
        (let ((files (system-entry-files entry)))
          (when files
            (:details.source-system :open (and (member current files) t)
              (:summary
               (:span.system (short-system-name (system-entry-name entry)))
               (:span.count (format nil "~D" (length files))))
              (:ul
               (dolist (file files)
                 (:li :class (if (eq file current) "current" nil)
                   (:a :href (concatenate 'string *page-prefix* (source-page-name file))
                       (render-file-path (source-file-relative-path file)))))))))))))

(defun render-mobile-source-nav (current &optional (site *site*))
  "A small in-flow file navigator where the complete sidebar does not fit."
  (let ((entry (find (source-file-system-name current) (site-systems site)
                     :key #'system-entry-name :test #'string=)))
    (when entry
      (spinneret:with-html
        (:details.mobile-source-nav
         (:summary
          (:span "Browse ") (:strong (system-entry-name entry))
          (:span (format nil " · ~D files" (length (system-entry-files entry)))))
         (:ul
          (dolist (file (system-entry-files entry))
            (:li :class (if (eq file current) "current" nil)
             (:a :href (concatenate 'string *page-prefix* (source-page-name file))
                 (render-file-path (source-file-relative-path file)))))))))))

(defun render-source-page (file)
  "Emit the page for FILE: its definitions table and every top-level form
as dexp boxes, each anchored by its starting line, with the sidebar of all
files beside it on wide screens."
  (let* ((page (source-page-name file))
         (*page-prefix* (page-prefix-for page))
         (*page-kind* "source-file")
         (*page-definition-cards* (make-hash-table :test 'eq))
         (*rendering-document* nil)
         (line-starts (source-file-line-starts file))
         (system-name (source-file-system-name file))
         (title (source-file-relative-path file)))
    (render-page-frame
     title
     (lambda ()
       (spinneret:with-html
         (render-source-sidebar file)
         (:article.source-body
          (:h1.source-title title)
          (:p.source-meta
           (when system-name
             (spinneret:html "system ")
             (:code system-name)
             (spinneret:html " · "))
           (format nil "~D definition~:P · " (length (source-file-definitions file)))
           (:a :href (concatenate 'string (site-source-url *site*) (source-file-relative-path file))
               "on GitHub"))
          (render-mobile-source-nav file)
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
                 append (definition-mentions definition))))))
     :body-class "wide source-page"
     :crumbs (append (list (cons "Source" "source.html"))
                     (when system-name
                       (list (cons system-name
                                   (concatenate 'string "source.html#" (system-anchor system-name)))))
                     (list (cons title nil)))
     :right (lambda ()
              (spinneret:with-html
                (:a :href (concatenate 'string (site-source-url *site*) title)
                    (file-namestring (source-file-pathname file))))))))

(defvar *file-cards* nil
  "While the source index renders: files whose definitions get a card.")

(defun file-card-id (file)
  (format nil "file-~A" (substitute-if #\- (lambda (c) (member c '(#\/ #\.)))
                                       (source-file-relative-path file))))

(defun render-file-entry (file)
  "A file's relative path linking to its page, the file name emphasized,
and a count button that shows the definitions in a popover."
  (progn
    (push file *file-cards*)
    (spinneret:with-html
      (:span.file-entry
       (:a.path :href (source-page-name file)
                (render-file-path (source-file-relative-path file)))
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
                 (or (search "/test" name)
                     (string= name "luv"))))
             (site-systems site)))

(defun short-system-name (name)
  "NAME without the leading luv/ that nearly every system shares."
  (if (starts-with "luv/" name) (subseq name 4) name))

(defun system-anchor (name)
  "The id of NAME's row in the source index: the name itself, slashes and
all, since luv-wiki-site and luv-wiki are different systems."
  (concatenate 'string "system-" name))

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
             (format out "  ~A[\"~A\"]~%" (node name) (short-system-name name)))
           (loop for (from . to) in edges
                 do (format out "  ~A --> ~A~%" (node from) (node to)))))))))

(defun change-local-href (change &optional (site *site*))
  "A changed path's page in this site, when the browser covers it."
  (let* ((path (commit-change-path change))
         (source (find path (site-source-files site)
                       :key #'source-file-relative-path :test #'string=)))
    (cond (source (concatenate 'string *page-prefix* (source-page-name source)))
          ((and (starts-with "wiki/" path)
                (string= (or (pathname-type path) "") "org"))
           (let ((document (find (pathname-name path) (site-documents site)
                                 :key #'document-name :test #'string=)))
             (and document
                  (concatenate 'string *page-prefix* (site-page-name document))))))))

(defun render-change-path (change)
  (let ((href (change-local-href change)))
    (if href
        (spinneret:with-html (:a :href href :title (commit-change-path change)
                                 (commit-change-path change)))
        (spinneret:with-html (:span (commit-change-path change))))))

(defun commit-totals (commit)
  (values (loop for change in (repository-commit-changes commit)
                sum (or (commit-change-additions change) 0))
          (loop for change in (repository-commit-changes commit)
                sum (or (commit-change-deletions change) 0))))

(defun hot-commit-paths (commits &key (limit 10))
  "Paths touched by the most of COMMITS, ranked by commit count."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (commit commits)
      (dolist (path (remove-duplicates
                     (mapcar #'commit-change-path (repository-commit-changes commit))
                     :test #'string=))
        (incf (gethash path counts 0))))
    (let ((ranked
            (sort (loop for path being the hash-keys of counts using (hash-value count)
                        collect (cons path count))
                  (lambda (a b)
                    (if (= (cdr a) (cdr b))
                        (string< (car a) (car b))
                        (> (cdr a) (cdr b)))))))
      (subseq ranked 0 (min limit (length ranked))))))

(defun render-recent-activity (site)
  "Recent repository work, with local links where the wiki can browse a path."
  (let ((commits (site-commits site)))
    (when commits
      (spinneret:with-html
        (:section.activity :id "activity"
         (:div.section-heading
          (:h2 "Recent activity")
          (:p (format nil "~D commit~:P available in this checkout, ~A through ~A."
                      (length commits)
                      (subseq (repository-commit-date (car (last commits))) 0 10)
                      (subseq (repository-commit-date (first commits)) 0 10))))
         (:div.hot-paths
          (:h3 "Most active paths")
          (:ol
           (dolist (entry (hot-commit-paths commits))
             (let ((change (make-instance 'commit-change :path (car entry)
                                          :additions nil :deletions nil)))
               (:li (render-change-path change)
                    (:span (format nil "~D commit~:P" (cdr entry))))))))
         (:ol.commit-feed
          (dolist (commit commits)
            (multiple-value-bind (additions deletions) (commit-totals commit)
              (:li.commit
               (:div.commit-heading
                (:a.commit-subject
                 :href (concatenate 'string *page-prefix* (commit-page-name commit))
                 (repository-commit-subject commit))
                (:a.commit-id :href (commit-github-url commit)
                              (repository-commit-short-id commit)))
               (:p.commit-meta
                (:time :datetime (repository-commit-date commit)
                       (format nil "~A ~A"
                               (subseq (repository-commit-date commit) 0 10)
                               (subseq (repository-commit-date commit) 11 16)))
                " · " (repository-commit-author commit)
                (:span.commit-diffstat
                 (format nil " · +~D −~D · ~D file~:P"
                         additions deletions
                         (length (repository-commit-changes commit)))))
               (:details.commit-files
                (:summary "Changed paths")
                (:ul
                 (dolist (change (repository-commit-changes commit))
                   (:li (render-change-path change)
                        (when (commit-change-additions change)
                          (:span.change-stat
                           (format nil "+~D −~D"
                                   (commit-change-additions change)
                                   (commit-change-deletions change)))))))))))))))))

(defun render-commit-page (commit site)
  "A commit's metadata everywhere, and its patch when rendered by live Clack."
  (let ((*page-prefix* "../")
        (*page-kind* "source")
        (*rendering-document* nil))
    (render-page-frame
     (repository-commit-subject commit)
     (lambda ()
       (spinneret:with-html
         (:article.commit-page
          (:p.commit-kicker "Commit " (:code (repository-commit-short-id commit)))
          (:h1 (repository-commit-subject commit))
          (:p.commit-byline
           (repository-commit-author commit) " · "
           (:time :datetime (repository-commit-date commit)
                  (repository-commit-date commit))
           " · " (:a :href (commit-github-url commit) "on GitHub"))
          (:h2 "Changed paths")
          (:ul.commit-page-files
           (dolist (change (repository-commit-changes commit))
             (:li (render-change-path change)
                  (when (commit-change-additions change)
                    (:span.change-stat
                     (format nil "+~D −~D"
                             (commit-change-additions change)
                             (commit-change-deletions change)))))))
          (if *dynamic-server-p*
              (let ((patch (commit-patch commit (site-source-directory site))))
                (if patch
                    (progn (:h2 "Patch") (:pre.commit-patch (:code patch)))
                    (:p.patch-note "This patch is unavailable or too large; use GitHub for the full diff.")))
              (:p.patch-note "The live wiki renders this patch from its checkout; this static build links to GitHub instead.")))))
     :body-class "wide commit-view"
     :crumbs (list (cons "Source" "source.html")
                   (cons "Recent activity" "source.html#activity")
                   (cons (repository-commit-short-id commit) nil)))))

(defun render-system-card (entry site)
  (let ((dependents
          (remove-if-not (lambda (other)
                           (member (system-entry-name entry)
                                   (system-entry-depends-on other) :test #'string=))
                         (site-systems site))))
    (spinneret:with-html
      (:article.system-card :id (system-anchor (system-entry-name entry))
       (:div.system-heading
        (:h3 (system-entry-name entry))
        (:span (format nil "~D file~:P" (length (system-entry-files entry)))))
       (when (system-entry-description entry)
         (:p.system-description (system-entry-description entry)))
       (when (system-entry-depends-on entry)
         (:p.system-relations (:strong "Uses ")
          (loop for name in (system-entry-depends-on entry)
                for first = t then nil
                do (unless first (spinneret:html ", "))
                   (:a :href (format nil "#~A" (system-anchor name)) name))))
       (when dependents
         (:p.system-relations (:strong "Used by ")
          (loop for dependent in dependents
                for first = t then nil
                do (unless first (spinneret:html ", "))
                   (:a :href (format nil "#~A"
                                           (system-anchor (system-entry-name dependent)))
                       (system-entry-name dependent)))))
       (when (system-entry-files entry)
         (:details.system-file-list
          (:summary "Browse files")
          (:div
           (dolist (file (system-entry-files entry))
             (render-file-entry file)))))))))

(defun render-source-index (site)
  "Emit source.html: recent work and the browsed ASDF systems."
  (let ((*page-prefix* "")
        (*page-kind* "source")
        (*rendering-document* nil)
        (*file-cards* '()))
    (render-page-frame
     "Source"
     (lambda ()
       (spinneret:with-html
         (:h1 "Source")
         (:p.lede "Implementation evidence: what has changed recently, and the Lisp source
that the wiki can read structurally rather than merely print.")
         (render-recent-activity site)
         (:section.source-catalogue :id "catalogue"
          (:div.section-heading
           (:h2 "Source catalogue")
           (:p (format nil "~D files in ~D ASDF systems, ordered prerequisites first."
                       (length (site-source-files site)) (length (site-systems site)))))
          (:p.scope-note "Included: Common Lisp components registered under the "
                         (:code "luv") ", " (:code "luvcraft") ", " (:code "mcluv") ", "
                         (:code "luft") ", " (:code "luv-wiki") ", and "
                         (:code "luv-wiki-site") " families. Test systems appear beside the
rest; scripts, Nix/deployment files, assets, native sources, and other systems do not.")
          (:div.system-cards
           (dolist (entry (site-systems site))
             (render-system-card entry site))))
         (:details.dependency-diagnostic
          (:summary "Dependency diagnostic")
          (:p.graph-note "Essential ASDF edges only. Test systems and the aggregate "
                         (:code "luv") " root are omitted; labels drop the "
                         (:code "luv/") " prefix.")
          (render-system-graph site))
         (render-file-cards)))
     :body-class "wide source-index")))
