;;;; Rendering the wiki corpus as a static HTML site with Spinneret.
;;;;
;;;; A SITE holds every document of the corpus together with the figure
;;;; index derived from their headings: which page owns each ID, and which
;;;; figures mention which.  RENDER-HTML is the generic that emits Spinneret
;;;; markup for an element while *SITE* supplies cross-page resolution.

(in-package #:luv.wiki)

(defclass site ()
  ((documents :initarg :documents :initform '() :accessor site-documents)
   (definitions :initarg :definitions :initform '() :accessor site-definitions
                :documentation "DEFINITIONs read from the source files, if any.")
   (code-references :initform (make-hash-table :test 'equal) :accessor site-code-references
                    :documentation "Figure ID -> list of DEFINITIONs mentioning it.")
   (figures :initform (make-hash-table :test 'equal) :accessor site-figures
            :documentation "Figure ID -> HEADING.")
   (backlinks :initform (make-hash-table :test 'equal) :accessor site-backlinks
              :documentation "Figure ID -> list of HEADINGs whose text mentions it.")
   (source-url :initarg :source-url :initform "https://github.com/mbrock/luv/blob/main/"
               :accessor site-source-url
               :documentation "Base URL for file: links into the repository.")
   (source-directory :initarg :source-directory :initform nil :accessor site-source-directory
                     :documentation "The repository root that SOURCE-URL corresponds to."))
  (:documentation "The whole wiki corpus and its disposable derived indexes."))

(defvar *site* nil
  "The SITE whose indexes resolve mentions and links while rendering.")

(defun make-site (documents &rest initargs)
  "Build a SITE over DOCUMENTS and index their figures and mentions."
  (let ((site (apply #'make-instance 'site :documents documents initargs)))
    (dolist (document documents)
      (dolist (figure (document-figures document))
        (setf (gethash (heading-id figure) (site-figures site)) figure)))
    (dolist (document documents)
      (dolist (figure (document-figures document))
        (dolist (id (remove-duplicates (heading-mentions figure) :test #'equal))
          (unless (equal id (heading-id figure))
            (push figure (gethash id (site-backlinks site)))))))
    (maphash (lambda (id figures)
               (setf (gethash id (site-backlinks site)) (nreverse figures)))
             (site-backlinks site))
    (setf (site-code-references site) (definition-references (site-definitions site)))
    site))

(defun dangling-code-mentions (site)
  "An alist of (definition . ids) for code mentions no figure resolves."
  (loop for definition in (site-definitions site)
        for dangling = (remove-if (lambda (id) (find-figure id site))
                                  (definition-mentions definition))
        when dangling collect (cons definition dangling)))

(defun find-figure (id &optional (site *site*))
  (and site (gethash id (site-figures site))))

(defun site-page-name (document)
  (concatenate 'string (document-name document) ".html"))

(defun figure-href (id &key (site *site*) from)
  "The href for figure ID from the page FROM (a document), or NIL if dangling."
  (let ((figure (find-figure id site)))
    (when figure
      (let ((document (heading-document figure)))
        (if (and from (eq document from))
            (format nil "#~A" id)
            (format nil "~A#~A" (site-page-name document) id))))))

(defun map-inlines (function element)
  "Call FUNCTION on every inline object reachable from block ELEMENT,
including headings' titles and table cells, but not into subheadings."
  (labels ((walk-inlines (inlines)
             (dolist (x inlines)
               (unless (stringp x)
                 (funcall function x)
                 (when (typep x '(or emphasis link))
                   (walk-inlines (element-children x))))))
           (walk (element)
             (typecase element
               (heading (walk-inlines (heading-title element)))
               (paragraph (walk-inlines (element-children element)))
               (table (dolist (row (table-rows element))
                        (mapc #'walk-inlines row))))
             (unless (typep element '(or paragraph table))
               (dolist (child (element-children element))
                 (unless (typep child 'heading)
                   (walk child))))))
    (walk element)))

(defun heading-mentions (heading)
  "IDs referred to in HEADING's own section text, not its subheadings."
  (let ((ids '()))
    (map-inlines (lambda (inline)
                   (let ((id (reference-id inline)))
                     (when id (push id ids))))
                 heading)
    (nreverse ids)))

(defun document-mentions (document)
  "All figure IDs mentioned anywhere in DOCUMENT, in document order."
  (let ((ids '()))
    (labels ((collect (element)
               (map-inlines (lambda (inline)
                              (let ((id (reference-id inline)))
                                (when id (push id ids))))
                            element))
             (walk (element)
               (if (typep element 'heading)
                   (progn (collect element)
                          (dolist (child (element-children element))
                            (when (typep child 'heading) (walk child))))
                   (collect element))))
      (mapc #'walk (element-children document)))
    (remove-duplicates (nreverse ids) :test #'equal :from-end t)))

(defun dangling-mentions (site)
  "An alist of (document . ids) for mentions no figure in SITE resolves."
  (loop for document in (site-documents site)
        for dangling = (remove-if (lambda (id) (find-figure id site))
                                  (document-mentions document))
        when dangling collect (cons document dangling)))

;;; Rendering

(defvar *rendering-document* nil
  "The document whose page is being rendered; makes same-page links relative.")

(defgeneric render-html (element)
  (:documentation
   "Emit Spinneret markup for ELEMENT into SPINNERET:*HTML*.  Strings are
plain text; each element and inline class contributes its own method."))

(defmethod render-html :around ((element t))
  ;; Spinneret passes the value of every body form to SPINNERET:HTML, whose
  ;; default method still emits a pending space for a non-NIL object.
  ;; Rendering is for effect only, so return nothing.
  (call-next-method)
  (values))

(defmethod render-html ((string string))
  ;; SPINNERET:HTML is Spinneret's own generic for writing an object as
  ;; escaped text; a bare form at the top of WITH-HTML is not printed.
  (spinneret:html string))

(defun render-inlines (inlines)
  (mapc #'render-html inlines)
  (values))

(defmethod render-html ((element element))
  (mapc #'render-html (element-children element)))

(defmethod render-html ((paragraph paragraph))
  (let ((inlines (remove-if (lambda (x) (and (stringp x) (blank-line-p x)))
                            (element-children paragraph))))
    (spinneret:with-html
      ;; A paragraph that is only an image link is a figure of its own.
      (cond ((and (= (length inlines) 1) (typep (first inlines) 'link)
                  (image-link-p (first inlines)))
             (:figure.image (:img :src (link-path (first inlines)) :alt "")))
            ((and (= (length inlines) 1) (typep (first inlines) 'link)
                  (link-definition (first inlines)))
             (render-definition (link-definition (first inlines)) :open t))
            (t (:p (render-inlines (element-children paragraph))))))))

(defmethod render-html ((list plain-list))
  (spinneret:with-html
    (if (plain-list-ordered-p list)
        (:ol (mapc #'render-html (element-children list)))
        (:ul (mapc #'render-html (element-children list))))))

(defmethod render-html ((item list-item))
  (spinneret:with-html
    (:li (let ((children (element-children item)))
           ;; A single paragraph item renders inline; richer items keep blocks.
           (if (and (= (length children) 1) (typep (first children) 'paragraph))
               (render-inlines (element-children (first children)))
               (mapc #'render-html children))))))

(defmethod render-html ((block example-block))
  (spinneret:with-html (:pre.example (block-text block))))

(defmethod render-html ((block src-block))
  (if (equal (src-block-language block) "lisp")
      (render-lisp-source (block-text block))
      (spinneret:with-html
        (:pre.src :data-language (src-block-language block)
                  (:code (block-text block))))))

(defmethod render-html ((table table))
  (spinneret:with-html
    (:table
     (loop for row in (table-rows table)
           for first = t then nil
           do (:tr (dolist (cell row)
                     (if (and first (table-header-p table))
                         (:th (render-inlines cell))
                         (:td (render-inlines cell)))))))))

(defparameter *emphasis-tags*
  '((:bold "b") (:italic "i") (:underline "u") (:strike "s")
    (:verbatim "code" "verbatim") (:code "code"))
  "HTML tag and optional class for each emphasis kind.")

(defmethod render-html ((emphasis emphasis))
  (destructuring-bind (tag &optional class)
      (cdr (assoc (emphasis-kind emphasis) *emphasis-tags*))
    (spinneret:with-html
      (:tag :name tag :class class
            (render-inlines (element-children emphasis))))))

(defmethod render-html ((mention mention))
  (let* ((id (mention-id mention))
         (figure (find-figure id))
         (href (figure-href id :from *rendering-document*)))
    (spinneret:with-html
      (if href
          (:a.mention :href href :title (inline-text (heading-title figure))
                      (format nil "#~A" id))
          (:span.mention.dangling :title "No figure has this ID"
                                  (format nil "#~A" id))))))

(defgeneric link-href (protocol link)
  (:documentation
   "The href to use for LINK whose scheme is PROTOCOL (a keyword or NIL),
or NIL when the link cannot be resolved into the site.")
  (:method ((protocol t) (link link)) nil))

(defmethod link-href ((protocol (eql :https)) link)
  (format nil "https:~A" (link-path link)))

(defmethod link-href ((protocol (eql :http)) link)
  (format nil "http:~A" (link-path link)))

(defmethod link-href ((protocol (eql :id)) link)
  (figure-href (link-path link) :from *rendering-document*))

(defparameter *image-types* '("png" "jpg" "jpeg" "gif" "svg" "webp"))

(defun image-link-p (link)
  "True for a bare file: link to an image inside the wiki directory."
  (and (equal (link-protocol link) "file")
       (null (element-children link))
       (not (starts-with "../" (link-path link)))
       (member (pathname-type (link-path link)) *image-types* :test #'string-equal)))

(defmethod link-href ((protocol (eql :file)) link)
  "A file: link to another wiki page becomes a page link; a link into the
repository points at the source on GitHub; anything else is unresolved."
  (let* ((path (link-path link))
         (org-p (and (> (length path) 4) (string= ".org" path :start2 (- (length path) 4)))))
    (cond ((and org-p (not (find #\/ path)))
           (concatenate 'string (subseq path 0 (- (length path) 4)) ".html"))
          ((starts-with "../../" path) nil)
          ((starts-with "../" path)
           (concatenate 'string (site-source-url *site*) (subseq path 3)))
          (t nil))))

(defun link-definition (link)
  "The DEFINITION a lisp: link names, or NIL."
  (and *site* (equal (link-protocol link) "lisp")
       (find-definition (link-path link) (site-definitions *site*))))

(defun definition-source-url (definition)
  (format nil "~A~A#L~D"
          (site-source-url *site*)
          (uiop:enough-pathname (definition-pathname definition)
                                (site-source-directory *site*))
          (definition-line definition)))

(defmethod link-href ((protocol (eql :lisp)) link)
  (let ((definition (link-definition link)))
    (and definition (definition-source-url definition))))

(defun render-definition (definition &key open)
  "A disclosure block: the definition's head, file, and source link as the
summary, and the form drawn as dexp boxes inside."
  (spinneret:with-html
    (:details.definition :open open
      (:summary
       (:span.kind (definition-kind definition))
       " "
       (:span.name (definition-name definition))
       (dolist (qualifier (definition-qualifiers definition))
         (spinneret:html " ")
         (:span.qualifier qualifier))
       " "
       (:a.source :href (definition-source-url definition)
                  (format nil "~A:~D" (definition-file-name definition) (definition-line definition))))
      (render-lisp-nodes (append (definition-comments definition)
                                 (list (definition-node definition)))
                         :package (definition-package definition)))))

(defmethod link-href ((protocol null) link)
  "A bare [[target]] with no scheme is a wiki page name if a page exists."
  (let ((name (link-path link)))
    (when (and *site* (find name (site-documents *site*) :key #'document-name :test #'string=))
      (concatenate 'string name ".html"))))

(defmethod render-html ((link link))
  (let* ((protocol (and (link-protocol link)
                        (intern (string-upcase (link-protocol link)) :keyword)))
         (href (link-href protocol link))
         (description (or (element-children link) (list (link-path link)))))
    (spinneret:with-html
      (cond ((image-link-p link)
             (:img.inline :src (link-path link) :alt ""))
            ((and (eq protocol :id) (null (element-children link)))
             ;; A bare [[id:X]] reads like the light mention #X.
             (render-html (make-instance 'mention :id (link-path link))))
            (href
             (:a :href href (render-inlines description)))
            (t
             (:span.unresolved-link :title (format nil "~@[~A:~]~A" (link-protocol link) (link-path link))
                                    (render-inlines description)))))))

(defun render-heading-title (heading)
  (spinneret:with-html
    (when (heading-keyword heading)
      (:span :class (format nil "mark mark-~(~A~)" (heading-keyword heading))
             (heading-keyword heading))
      (spinneret:html " "))
    (render-inlines (heading-title heading))))

(defmethod render-html ((heading heading))
  (let* ((id (heading-id heading))
         (backlinks (and id (gethash id (site-backlinks *site*))))
         (references (and id (gethash id (site-code-references *site*)))))
    (spinneret:with-html
      (:section :id id :class (if (heading-keyword heading) "figure work-mark" "figure")
        (:h* (render-heading-title heading)
             (when id
               (spinneret:html " ")
               (:a.figure-id :href (format nil "#~A" id) :title "Permalink to this figure"
                             (format nil "#~A" id))))
        (dolist (child (element-children heading))
          (unless (typep child 'heading) (render-html child)))
        (when backlinks
          (:p.backlinks "Mentioned in: "
            (loop for figure in backlinks
                  for first = t then nil
                  do (unless first (spinneret:html ", "))
                     (:a :href (figure-href (heading-id figure) :from *rendering-document*)
                         (render-inlines (heading-title figure))))))
        (when references
          (:div.code-references
           (:p.backlinks "Referenced from code:")
           (dolist (definition references)
             (render-definition definition))))
        (dolist (child (element-children heading))
          (when (typep child 'heading) (render-html child)))))))

(defun render-page (document)
  "Emit the whole HTML page for DOCUMENT."
  (let ((*rendering-document* document)
        (title (or (document-title document) (document-name document))))
    (spinneret:with-html
      (:doctype)
      (:html :lang "en"
        (:head
         (:meta :charset "utf-8")
         (:meta :name "viewport" :content "width=device-width, initial-scale=1")
         (:title title)
         (:link :rel "stylesheet" :href "style.css"))
        (:body
         (:header.site-header
          (:nav
           (:a :href "index.html" "luv wiki")
           " · "
           (:a :href "figures.html" "figures")
           " · "
           (:a :href (concatenate 'string (site-source-url *site*) "wiki/"
                                  (document-name document) ".org")
               "source")))
         (:main
          (:h1 title)
          (dolist (child (element-children document))
            (render-html child)))
         (:footer.site-footer
          "Rendered from Org by luv.wiki."))))))

(defun render-figures-page (site)
  "Emit an index page listing every figure and its work-mark status."
  (let ((*rendering-document* nil))
    (spinneret:with-html
      (:doctype)
      (:html :lang "en"
        (:head
         (:meta :charset "utf-8")
         (:meta :name "viewport" :content "width=device-width, initial-scale=1")
         (:title "Figures")
         (:link :rel "stylesheet" :href "style.css"))
        (:body
         (:header.site-header (:nav (:a :href "index.html" "luv wiki")))
         (:main
          (:h1 "Figures")
          (:p "Every addressable heading in the wiki, by page.  Work marks show their status.")
          (dolist (document (site-documents site))
            (let ((figures (document-figures document)))
              (when figures
                (:section
                 (:h2 (:a :href (site-page-name document)
                          (or (document-title document) (document-name document))))
                 (:ul.figure-list
                  (dolist (figure figures)
                    (:li :class (format nil "level-~D" (heading-level figure))
                         (:a.figure-id :href (figure-href (heading-id figure) :site site)
                                       (format nil "#~A" (heading-id figure)))
                         " "
                         (render-heading-title figure)))))))))
         (:footer.site-footer "Rendered from Org by luv.wiki."))))))

(defun call-with-html-output (stream thunk)
  "Call THUNK with Spinneret writing exact, compact HTML to STREAM.  The
pretty printer would fill text and insert spaces between dynamically written
strings, turning *figure* into \"figure \"; the wiki's text carries its own
spacing, so both are turned off."
  (let ((spinneret:*html* stream)
        (spinneret:*suppress-inserted-spaces* t)
        (*print-pretty* nil))
    (funcall thunk)))

(defun render-document-string (document &optional (site *site*))
  "Render DOCUMENT to an HTML string within SITE."
  (let ((*site* site))
    (with-output-to-string (out)
      (call-with-html-output out (lambda () (render-page document))))))

(defun write-html-file (pathname thunk)
  (ensure-directories-exist pathname)
  (with-open-file (out pathname :direction :output :if-exists :supersede
                                :external-format :utf-8)
    (call-with-html-output out thunk))
  pathname)

(defun write-site (site directory &key stylesheet)
  "Write every page of SITE, the figures index, and STYLESHEET into DIRECTORY."
  (let ((*site* site)
        (directory (uiop:ensure-directory-pathname directory)))
    (dolist (document (site-documents site))
      (write-html-file (merge-pathnames (site-page-name document) directory)
                       (lambda () (render-page document))))
    (write-html-file (merge-pathnames "figures.html" directory)
                     (lambda () (render-figures-page site)))
    (when stylesheet
      (uiop:copy-file stylesheet (merge-pathnames "style.css" directory)))
    directory))
