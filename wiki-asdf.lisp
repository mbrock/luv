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
;;;; not to be translated.  A static-file such as the stylesheet is simply
;;;; copied.  With :BUILD-OPERATION set, (asdf:make :luv/wiki) builds the site.

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

(defun system-site (system)
  (make-site (system-documents system)))

;;; Rendering

(defmethod asdf:component-depends-on ((o render-op) (c org-file))
  "A page's rendering depends on the whole system being loaded, since
mentions and backlinks resolve across pages."
  `((asdf:load-op ,(asdf:component-system c)) ,@(call-next-method)))

(defmethod asdf:input-files ((o render-op) (c org-file))
  "Every page is an input to every page: a new figure or mention anywhere
can change the links and backlinks rendered here."
  (mapcar #'asdf:component-pathname (system-org-files (asdf:component-system c))))

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
  (values (list (merge-pathnames (file-namestring (asdf:component-pathname c))
                                 (site-output-directory (asdf:component-system c))))
          t))

(defmethod asdf:perform ((o render-op) (c asdf:static-file))
  (let ((output (asdf:output-file o c)))
    (ensure-directories-exist output)
    (uiop:copy-file (asdf:component-pathname c) output)))

;;; The system itself contributes the figures index.

(defmethod asdf:input-files ((o render-op) (s asdf:system))
  (mapcar #'asdf:component-pathname (system-org-files s)))

(defmethod asdf:output-files ((o render-op) (s asdf:system))
  (values (list (merge-pathnames "figures.html" (site-output-directory s))) t))

(defmethod asdf:perform ((o render-op) (s asdf:system))
  (let ((site (system-site s)))
    (write-html-file (asdf:output-file o s)
                     (lambda ()
                       (let ((*site* site))
                         (render-figures-page site))))
    (let ((dangling (dangling-mentions site)))
      (when dangling
        (warn "Dangling figure mentions:~{~%  ~A: ~{~A~^ ~}~}"
              (loop for (document . ids) in dangling
                    collect (document-name document) collect ids))))))


;;; A defsystem form names these classes as strings that ASDF reads in its
;;; own package, e.g. :default-component-class "luv.wiki:org-file" and
;;; :build-operation "luv.wiki:render-op", so we need not intern anything
;;; into the ASDF package.
