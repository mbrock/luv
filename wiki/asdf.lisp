;;;; The wiki as an ASDF system.
;;;;
;;;; ASDF is a build system for Lisp, but its model is general: components
;;;; have input files, operations have outputs, and the plan is a dependency
;;;; graph over (operation . component) actions.  cffi-toolchain uses that
;;;; generality to compile C files; here we use it for Org pages.
;;;;
;;;; An ORG-FILE is a static-file component (so the ordinary compile-op is a
;;;; no-op) whose LOAD-OP reads the page into a DOCUMENT and remembers it on
;;;; the component.  "Loading the wiki" therefore means having the corpus as
;;;; Lisp objects in the image, ready for the figure index or an inspector.
;;;;
;;;; RENDER-OP is a downward, selfward operation: rendering the system renders
;;;; every page; rendering a page first loads it, and also loads the whole
;;;; system, because mentions and backlinks cross pages.  It writes HTML into
;;;; the site directory rather than the fasl cache, so its OUTPUT-FILES asks
;;;; not to be translated.  A static-file such as an image is simply
;;;; copied.  With :BUILD-OPERATION set, (asdf:make :luv-wiki-site) builds the site.

(in-package #:luv.wiki)

(defclass org-file (asdf:static-file)
  ((type :initform "org")
   (document :initform nil :accessor org-file-document
             :documentation "The DOCUMENT read by the last LOAD-OP, or NIL."))
  (:documentation "One Org page of the wiki as an ASDF component."))

(defclass render-op (asdf:downward-operation asdf:selfward-operation)
  ()
  (:documentation "Render a wiki system, page, or asset into the static site."))

(defmethod asdf:selfward-operation ((o render-op)) 'asdf:load-op)
(defmethod asdf:downward-operation ((o render-op)) 'render-op)

(defgeneric site-output-directory (system)
  (:documentation "Where RENDER-OP writes the site for SYSTEM.")
  (:method ((system asdf:system))
    (asdf:system-relative-pathname system "build/wiki/")))

;;; Loading: an Org page becomes a DOCUMENT.

(defmethod asdf:perform ((o asdf:load-op) (c org-file))
  (setf (org-file-document c)
        (read-org-file (asdf:component-pathname c) :name (asdf:component-name c))))

(defmethod asdf:perform ((o asdf:load-source-op) (c org-file))
  (asdf:perform (asdf:make-operation 'asdf:load-op) c))

(defun system-org-files (system)
  "All ORG-FILE components of SYSTEM, in system order."
  (let ((files '()))
    (labels ((walk (component)
               (typecase component
                 (org-file (push component files))
                 (asdf:parent-component
                  (mapc #'walk (asdf:component-children component))))))
      (walk system))
    (nreverse files)))

(defun system-documents (system)
  "The DOCUMENTs of SYSTEM's loaded org files."
  (remove nil (mapcar #'org-file-document (system-org-files system))))

(defparameter *code-systems*
  '("luv" "luvcraft" "mcluv" "luft" "luv-wiki" "luv-wiki-site")
  "Primary names of the systems whose source files the site reads for
definitions and figure mentions.  luv-wiki is among them so the site shows
how it renders itself; its tests fabricate figure IDs, which is why
DANGLING-CODE-MENTIONS passes over test systems.")

(defun code-source-components ()
  "Alist of (pathname . system-name) for the cl-source-file components of
every registered system whose primary name is in *CODE-SYSTEMS*, without
loading anything."
  (let ((files '()))
    (dolist (name (asdf:registered-systems))
      (when (member (asdf:primary-system-name name) *code-systems* :test #'string=)
        (labels ((walk (component)
                   (typecase component
                     (asdf:cl-source-file
                      (let ((pathname (asdf:component-pathname component)))
                        (unless (assoc pathname files :test #'equal)
                          (push (cons pathname name) files))))
                     (asdf:parent-component (mapc #'walk (asdf:component-children component))))))
          (walk (asdf:find-system name)))))
    (sort files #'string< :key (lambda (entry) (namestring (car entry))))))

(defun code-source-files ()
  (mapcar #'car (code-source-components)))

(defclass system-entry ()
  ((name :initarg :name :accessor system-entry-name)
   (description :initarg :description :initform nil :accessor system-entry-description)
   (depends-on :initarg :depends-on :initform '() :accessor system-entry-depends-on
               :documentation "Names of the code systems this one depends on.")
   (files :initarg :files :initform '() :accessor system-entry-files
          :documentation "SOURCE-FILEs of this system's components."))
  (:documentation "One of the code systems, for the source index."))

(defun code-systems (source-files)
  "SYSTEM-ENTRYs for the registered code systems, in dependency order:
a system comes after everything it depends on, ties broken by name, and
each SOURCE-FILE of SOURCE-FILES attached to its system."
  (let* ((names (remove-if-not (lambda (n) (member (asdf:primary-system-name n) *code-systems*
                                                    :test #'string=))
                               (asdf:registered-systems)))
         (entries (mapcar (lambda (name)
                            (let ((system (asdf:registered-system name)))
                              (make-instance
                               'system-entry
                               :name name
                               :description (asdf:system-description system)
                               :depends-on (sort (remove-if-not
                                                  (lambda (d) (member d names :test #'string=))
                                                  (mapcar (lambda (d) (string-downcase
                                                                       (if (consp d) (second d) d)))
                                                          (asdf:system-depends-on system)))
                                                 #'string<)
                               :files (remove name source-files
                                              :key #'source-file-system-name :test-not #'string=))))
                          ;; Alphabetical, but test systems after the rest so the
                          ;; topological pass places them behind their subjects.
                          (sort (copy-list names)
                                (lambda (a b)
                                  (let ((ta (and (search "/test" a) t))
                                        (tb (and (search "/test" b) t)))
                                    (if (eq ta tb) (string< a b) tb))))))
         (ordered '()))
    ;; Topological order: repeatedly take the first entry whose dependencies
    ;; are all placed.
    (loop while entries
          do (let ((next (or (find-if (lambda (e)
                                        (every (lambda (d) (find d ordered :key #'system-entry-name
                                                                            :test #'string=))
                                               (system-entry-depends-on e)))
                                      entries)
                             (first entries))))
               (push next ordered)
               (setf entries (remove next entries))))
    (nreverse ordered)))

(defun arglists-file (system)
  "The introspected operator lambda lists the renderer reads, if present."
  (let ((pathname (asdf:system-relative-pathname system "wiki/arglists.sexp")))
    (and (probe-file pathname) (list pathname))))

(defvar *source-cache* (make-hash-table :test 'equal)
  "Pathname namestring -> (write-date . source-file), so an unchanged file
is not read again within one image.")

(defun code-sources (&key (root (uiop:getcwd)))
  "The SOURCE-FILEs of the code systems, reading a file only when it changed."
  (loop for (pathname . system-name) in (code-source-components)
        for key = (namestring pathname)
        for date = (file-write-date pathname)
        for cached = (gethash key *source-cache*)
        collect (if (and cached (eql (car cached) date))
                    (cdr cached)
                    (let ((file (read-source-file
                                 pathname
                                 :relative-path (namestring (uiop:enough-pathname pathname root))
                                 :system-name system-name)))
                      (setf (gethash key *source-cache*) (cons date file))
                      file))))

(defun code-definitions ()
  "The DEFINITIONs of all code source files."
  (loop for file in (code-sources) append (source-file-definitions file)))

(defun system-site (system)
  (let ((root (asdf:system-source-directory system)))
    (load-arglists (merge-pathnames "wiki/arglists.sexp" root))
    (let ((sources (code-sources :root root)))
      (make-site (system-documents system)
                 :source-files sources
                 :systems (code-systems sources)
                 :commits (read-git-history root)
                 :source-directory root))))

;;; Rendering

(defmethod asdf:component-depends-on ((o render-op) (c org-file))
  "A page's rendering depends on the whole system being loaded, since
mentions and backlinks resolve across pages."
  `((asdf:load-op ,(asdf:component-system c)) ,@(call-next-method)))

(defmethod asdf:input-files ((o render-op) (c org-file))
  "Every page is an input to every page: a new figure or mention anywhere
can change the links and backlinks rendered here; so is every source file,
whose definitions may reference this page's figures."
  (append (mapcar #'asdf:component-pathname (system-org-files (asdf:component-system c)))
          (code-source-files)
          *figure-source-files*
          (arglists-file (asdf:component-system c))))

(defmethod asdf:output-files ((o render-op) (c org-file))
  (values (list (merge-pathnames (make-pathname :name (asdf:component-name c) :type "html")
                                 (site-output-directory (asdf:component-system c))))
          t))

(defmethod asdf:perform ((o render-op) (c org-file))
  (let* ((system (asdf:component-system c))
         (site (system-site system))
         (document (org-file-document c))
         (output (asdf:output-file o c)))
    (write-html-file output (lambda ()
                              (let ((*site* site))
                                (render-page document))))))

(defmethod asdf:input-files ((o render-op) (c asdf:static-file))
  (list (asdf:component-pathname c)))

(defmethod asdf:output-files ((o render-op) (c asdf:static-file))
  "An asset keeps its path relative to its module, so wiki/images/x.png
lands at images/x.png in the site."
  (values (list (merge-pathnames (uiop:enough-pathname
                                  (asdf:component-pathname c)
                                  (asdf:component-pathname (asdf:component-parent c)))
                                 (site-output-directory (asdf:component-system c))))
          t))

(defmethod asdf:perform ((o render-op) (c asdf:static-file))
  (let ((output (asdf:output-file o c)))
    (ensure-directories-exist output)
    (uiop:copy-file (asdf:component-pathname c) output)))

;;; The system itself contributes the figures index.

(defun style-source-files ()
  "The Lisp files the stylesheet is compiled from, so editing a style
re-renders the site."
  (mapcar (lambda (name) (asdf:system-relative-pathname :luv-wiki name))
          '("wiki/css.lisp" "wiki/style.lisp")))

(defmethod asdf:input-files ((o render-op) (s asdf:system))
  (append (mapcar #'asdf:component-pathname (system-org-files s))
          (code-source-files)
          *figure-source-files*
          (arglists-file s)
          (style-source-files)))

(defmethod asdf:output-files ((o render-op) (s asdf:system))
  "Every dynamically routable resource has the same static output path."
  (let ((directory (site-output-directory s)))
    (values (loop for resource in (website-resources (system-site s))
                  collect (merge-pathnames (resource-output-path resource)
                                           directory))
            t)))

(defmethod asdf:perform ((o render-op) (s asdf:system))
  (let ((site (system-site s))
        (directory (site-output-directory s)))
    (publish-site site directory)
    (let ((dangling (dangling-mentions site)))
      (when dangling
        (warn "Dangling figure mentions:~{~%  ~A: ~{~A~^ ~}~}"
              (loop for (document . ids) in dangling
                    collect (document-name document) collect ids))))
    (let ((dangling (dangling-code-mentions site)))
      (when dangling
        (warn "Dangling figure mentions in code:~{~%  ~A: ~{~A~^ ~}~}"
              (loop for (definition . ids) in dangling
                    collect (format nil "~A ~A (~A:~D)"
                                    (definition-kind definition) (definition-name definition)
                                    (definition-file-name definition) (definition-line definition))
                    collect ids))))))


;;; A defsystem form names these classes as strings that ASDF reads in its
;;; own package, e.g. :default-component-class "luv.wiki:org-file" and
;;; :build-operation "luv.wiki:render-op", so we need not intern anything
;;; into the ASDF package.
