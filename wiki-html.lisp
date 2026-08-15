;;;; Rendering the wiki corpus as a static HTML site with Spinneret.
;;;;
;;;; A SITE holds every document of the corpus together with the figure
;;;; index derived from their headings: which page owns each ID, and which
;;;; figures mention which.  RENDER-HTML is the generic that emits Spinneret
;;;; markup for an element while *SITE* supplies cross-page resolution.

(in-package #:luv.wiki)

(defclass site ()
  ((documents :initarg :documents :initform '() :accessor site-documents)
   (source-files :initarg :source-files :initform '() :accessor site-source-files
                 :documentation "SOURCE-FILEs of the systems the site browses.")
   (systems :initarg :systems :initform '() :accessor site-systems
            :documentation "SYSTEM-ENTRYs in dependency order, for the source index.")
   (definitions :initarg :definitions :initform '() :accessor site-definitions
                :documentation "DEFINITIONs read from the source files, if any.")
   (definition-table :initform (make-hash-table :test 'equalp) :accessor site-definition-table
                     :documentation "Bare definition name -> list of DEFINITIONs.")
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
    (unless (site-definitions site)
      (setf (site-definitions site)
            (loop for file in (site-source-files site) append (source-file-definitions file))))
    (setf (site-code-references site) (definition-references (site-definitions site)))
    (dolist (definition (site-definitions site))
      (push definition (gethash (bare-name (definition-name definition))
                                (site-definition-table site))))
    (maphash (lambda (name list) (setf (gethash name (site-definition-table site)) (nreverse list)))
             (site-definition-table site))
    site))

(defun bare-name (name)
  "NAME without any package prefix: \"luv.wiki:foo\" -> \"foo\"."
  (let ((colon (position #\: name :from-end t)))
    (if colon (subseq name (1+ colon)) name)))

(defun find-named-definition (name &optional (site *site*))
  "The best definition for the bare symbol NAME: a defining form of the
generic, function, macro, or class before any method, else the first."
  (let ((candidates (and site (gethash (bare-name name) (site-definition-table site)))))
    (or (find-if (lambda (d) (member (definition-kind d)
                                     '("defgeneric" "defun" "defmacro" "defclass" "defstruct"
                                       "defvar" "defparameter" "defconstant" "define-condition")
                                     :test #'string=))
                 candidates)
        (first candidates))))

(defun test-definition-p (definition site)
  "Is DEFINITION in a file of a test system?  Tests fabricate figure IDs
to exercise the reader, so their mentions are not expected to resolve."
  (let ((file (definition-source-file definition site)))
    (and file
         (source-file-system-name file)
         (search "/tests" (source-file-system-name file))
         t)))

(defun dangling-code-mentions (site)
  "An alist of (definition . ids) for code mentions no figure resolves,
leaving out the definitions of test systems."
  (loop for definition in (site-definitions site)
        for dangling = (and (not (test-definition-p definition site))
                            (remove-if (lambda (id) (find-figure id site))
                                       (definition-mentions definition)))
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
            (format nil "~A~A#~A" *page-prefix* (site-page-name document) id))))))

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

(defvar *page-prefix* ""
  "The relative path from the page being rendered back to the site root,
\"\" for top-level pages and \"../\" or deeper for source pages.")

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
  (let ((language (src-block-language block)))
    (cond ((equal language "lisp") (render-lisp-source (block-text block)))
          ((equal language "mermaid")
           ;; Mermaid draws these at load time; the text remains readable.
           (spinneret:with-html (:pre.mermaid (block-text block))))
          (t (spinneret:with-html
               (:pre.src :data-language language (:code (block-text block))))))))

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

(defmethod render-html ((math math))
  "TeX source in a .math element; site.js renders it with KaTeX."
  (spinneret:with-html
    (if (math-display-p math)
        (:div.math.display (math-text math))
        (:span.math (math-text math)))))

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
           (concatenate 'string *page-prefix* (subseq path 0 (- (length path) 4)) ".html"))
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
    (and definition
         (or (definition-page-href definition) (definition-source-url definition)))))

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
       (:a.source :href (or (definition-page-href definition) (definition-source-url definition))
                  (format nil "~A:~D" (definition-file-name definition) (definition-line definition)))
       (when (definition-page-href definition)
         (spinneret:html " ")
         (:a.github :href (definition-source-url definition) :title "On GitHub" "↗")))
      (render-lisp-nodes (append (definition-comments definition)
                                 (list (definition-node definition)))
                         :package (definition-package definition)))))

(defmethod link-href ((protocol null) link)
  "A bare [[target]] with no scheme is a wiki page name if a page exists."
  (let ((name (link-path link)))
    (when (and *site* (find name (site-documents *site*) :key #'document-name :test #'string=))
      (concatenate 'string *page-prefix* name ".html"))))

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

(defvar *page-kind* "page"
  "A short word for the status bar: what kind of page is being rendered.")

(defun render-crumbs (crumbs)
  "The breadcrumb trail of the status bar: CRUMBS is a list of (label . href),
the last one the current page, its href ignored."
  (spinneret:with-html
    (:nav.crumbs :aria-label "Breadcrumb"
      (loop for (crumb . rest) on crumbs
            for first = t then nil
            do (unless first (:span.crumb-sep :aria-hidden "true" "›"))
               (if (and rest (cdr crumb))
                   (:a.crumb :href (concatenate 'string *page-prefix* (cdr crumb)) (car crumb))
                   (:span.crumb.current :aria-current "page" (car crumb)))))))

(defun render-page-frame (title body &key body-class (kind *page-kind*)
                                          (crumbs (list (cons title nil))) (right kind))
  "Emit a whole HTML page with the site chrome around the output of BODY:
the library band with the site's three doors, a status bar with the page's
breadcrumb trail on the left and RIGHT (a string, or a function emitting
markup) on the right, the main column, and a footer."
  (flet ((href (name) (concatenate 'string *page-prefix* name)))
    (spinneret:with-html
      (:doctype)
      (:html :lang "en"
        (:head
         (:meta :charset "utf-8")
         (:meta :name "viewport" :content "width=device-width, initial-scale=1")
         (:title title)
         (:link :rel "preconnect" :href "https://fonts.googleapis.com")
         (:link :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin "")
         (:link :rel "stylesheet"
                :href "https://fonts.googleapis.com/css2?family=Public+Sans:ital,wght@0,100..900;1,100..900&display=swap")
         (:link :rel "stylesheet" :href (href "style.css"))
         (:script :src (href "site.js") :defer t))
        (:body :class body-class
         (:header.library
          (:div.library-heading
           (:p.eyebrow "luv")
           (:h1 (:a :href (href "index.html") "Workshop wiki")))
          (:nav.doors
           (:a :class (if (member kind '("page" "pages") :test #'equal) "door selected" "door")
               :href (href "pages.html")
               (:span.door-title "Pages")
               (:span.door-meta (format nil "~D pages of design memory"
                                        (length (site-documents *site*)))))
           (:a :class (if (equal kind "work") "door selected" "door") :href (href "work.html")
               (:span.door-title "Work")
               (:span.door-meta "work marks by status"))
           (when (site-source-files *site*)
             (:a :class (if (member kind '("source" "source-file") :test #'equal) "door selected" "door")
                 :href (href "source.html")
                 (:span.door-title "Source")
                 (:span.door-meta (format nil "~D systems, ~D files"
                                          (length (site-systems *site*))
                                          (length (site-source-files *site*))))))))
         (:div.status
          (:span.status-left (render-crumbs crumbs))
          (:span.status-right
           (cond (*rendering-document*
                  (:a :href (concatenate 'string (site-source-url *site*) "wiki/"
                                         (document-name *rendering-document*) ".org")
                      (concatenate 'string (document-name *rendering-document*) ".org")))
                 ((functionp right) (funcall right))
                 (t right))))
         (:main (funcall body))
         (:footer.site-footer
          "Rendered from Org and Lisp by luv.wiki."))))))

(defvar *page-definition-cards* nil
  "While a page renders: a hash table from DEFINITION to its card id, filled
by every definition link drawn on the page.")

(defun definition-card-id (definition)
  "Register DEFINITION for a card on the current page and return the id."
  (when *page-definition-cards*
    (or (gethash definition *page-definition-cards*)
        (setf (gethash definition *page-definition-cards*)
              (format nil "def-~D" (hash-table-count *page-definition-cards*))))))

(defun definition-docstring (definition)
  "The documentation string of DEFINITION's form as plain text, or NIL: the
first string among the arguments after the head, or the value after
:documentation."
  (let ((children (element-children (definition-node definition)))
        (previous nil))
    (flet ((documentation-keyword-p (node)
             (and (typep node 'lisp-symbol)
                  (equal (lisp-symbol-package node) "KEYWORD")
                  (string-equal (lisp-symbol-name node) "documentation"))))
      (dolist (child (cddr children))
        (cond ((and (typep child 'lisp-string)
                    (or (documentation-keyword-p previous)
                        (find #\Newline (node-text child))
                        (not (typep previous 'lisp-list))))
               (return (string-node-content child)))
              ;; (:documentation "...") as a DEFCLASS or DEFGENERIC option.
              ((and (typep child 'lisp-list)
                    (documentation-keyword-p (first (element-children child)))
                    (typep (second (element-children child)) 'lisp-string))
               (return (string-node-content (second (element-children child))))))
        (unless (typep child 'lisp-comment) (setf previous child))))))

(defun definition-lambda-list-text (definition)
  "The lambda list of DEFINITION as written in the source, or NIL."
  (let ((children (element-children (definition-node definition))))
    (when (member (definition-kind definition)
                  '("defun" "defmacro" "defgeneric" "defmethod" "define-command" "deftype")
                  :test #'string=)
      (let ((list (find-if (lambda (c) (typep c 'lisp-list)) (cddr children))))
        (and list (node-text list))))))

(defun render-definition-cards ()
  "Emit hidden cards for the definitions linked on this page."
  (when (and *page-definition-cards* (plusp (hash-table-count *page-definition-cards*)))
    (spinneret:with-html
      (:div.figure-cards :hidden t
        (maphash
         (lambda (definition id)
           (:div.figure-card :id id
             (:a.card-title :href (or (definition-page-href definition)
                                      (definition-source-url definition))
                            (:span.card-kind (definition-kind definition))
                            " "
                            (definition-name definition)
                            (dolist (qualifier (definition-qualifiers definition))
                              (spinneret:html " ")
                              (:span.qualifier qualifier)))
             (let ((lambda-list (definition-lambda-list-text definition)))
               (when lambda-list
                 (:code.card-lambda-list lambda-list)))
             (:span.card-meta
              (format nil "~A:~D" (definition-file-name definition) (definition-line definition))
              (when (definition-package definition)
                (spinneret:html " · ")
                (:span (string-trim "#:\"" (definition-package definition)))))
             (let ((docstring (definition-docstring definition)))
               (when docstring
                 (:p.card-excerpt
                  (let ((text (substitute #\Space #\Newline docstring)))
                    (if (> (length text) 320)
                        (concatenate 'string (subseq text 0 (or (position #\Space text :from-end t :end 320) 320)) "…")
                        text)))))))
         *page-definition-cards*)))))

(defun figure-excerpt (figure &optional (limit 320))
  "The opening prose of FIGURE's own section as plain text: paragraphs and
list items in order until about LIMIT characters, cut at a word boundary."
  (let ((pieces '())
        (length 0))
    (flet ((add (text)
             (push text pieces)
             (incf length (length text))))
      (block collect
        (dolist (child (element-children figure))
          (typecase child
            (paragraph (add (inline-text (element-children child))))
            (plain-list
             (dolist (item (element-children child))
               (let ((paragraph (find-if (lambda (c) (typep c 'paragraph)) (element-children item))))
                 (when paragraph
                   (add (concatenate 'string "– " (inline-text (element-children paragraph))))))))
            (heading (return-from collect)))
          (when (> length limit) (return-from collect)))))
    (let ((text (format nil "~{~A~^ ~}" (nreverse pieces))))
      (cond ((zerop (length text)) nil)
            ((<= (length text) limit) text)
            (t (let ((cut (or (position #\Space text :from-end t :end limit) limit)))
                 (concatenate 'string (subseq text 0 cut) "…")))))))

(defun render-figure-cards (ids)
  "Emit hidden cards for the figures IDS, which the page's script shows as
popovers when a mention is hovered or tapped."
  (let ((figures (remove nil (mapcar (lambda (id) (find-figure id)) (remove-duplicates ids :test #'equal)))))
    (when figures
      (spinneret:with-html
        (:div.figure-cards :hidden t
          (dolist (figure figures)
            (let ((id (heading-id figure))
                  (document (heading-document figure)))
              (:div.figure-card :id (concatenate 'string "card-" id)
                (:a.card-title :href (figure-href id :from *rendering-document*)
                               (render-heading-title figure))
                (:span.card-meta
                 (concatenate 'string (document-name document) ".org")
                 (spinneret:html " · ")
                 (:span.card-id (concatenate 'string "#" id)))
                (let ((excerpt (figure-excerpt figure)))
                  (when excerpt (:p.card-excerpt excerpt)))))))))))

(defun render-page (document)
  "Emit the whole HTML page for DOCUMENT."
  (let ((*rendering-document* document)
        (*page-prefix* "")
        (*page-definition-cards* (make-hash-table :test 'eq))
        (title (or (document-title document) (document-name document))))
    (render-page-frame
     title
     (lambda ()
       (spinneret:with-html
         (:h1 title)
         (dolist (child (element-children document))
           (render-html child))
         (render-definition-cards)
         (render-figure-cards
          (append (document-mentions document)
                  ;; Code references shown on this page mention figures too.
                  (loop for figure in (document-figures document)
                        append (loop for definition in (gethash (heading-id figure)
                                                                (site-code-references *site*))
                                     append (definition-mentions definition)))))))
     :crumbs (if (string= (document-name document) "index")
                 (list (cons title nil))
                 (list (cons "Pages" "pages.html") (cons title nil))))))

(defun render-pages-page (site)
  "Emit pages.html: every wiki page with its headings, each a link to its
figure, work marks flagged; a dense table."
  (let ((*rendering-document* nil)
        (*page-prefix* "")
        (*page-kind* "pages"))
    (render-page-frame
     "Pages"
     (lambda ()
       (spinneret:with-html
         (:h1 "Pages")
         (:p.lede "Every page of the wiki with its headings.  Each heading is a figure with a
stable ID; work marks carry their status.")
         (:table.pages
          (:tbody
           (dolist (document (site-documents site))
             (:tr
              (:td.page-title
               (:a :href (site-page-name document)
                   (or (document-title document) (document-name document))))
              (:td.page-headings
               (dolist (figure (document-figures document))
                 (:a :class (format nil "heading level-~D~@[ marked~]"
                                    (heading-level figure) (heading-keyword figure))
                     :href (figure-href (heading-id figure) :site site)
                     (render-heading-title figure))))))))))
     :body-class "wide")))

(defparameter *work-statuses*
  '(("NEXT" . "the current best small bets")
    ("TODO" . "visible and likely, not yet selected")
    ("WAIT" . "blocked on outside evidence or another step")
    ("IDEA" . "tempting, not allowed to steer implementation yet")
    ("DONE" . "closed, with the evidence that closed them")))

(defun render-work-page (site)
  "Emit work.html: the work marks by status, each with its page and intent."
  (let ((*rendering-document* nil)
        (*page-prefix* "")
        (*page-kind* "work")
        (marks '()))
    (dolist (document (site-documents site))
      (dolist (figure (document-figures document))
        (when (heading-keyword figure) (push figure marks))))
    (setf marks (nreverse marks))
    (render-page-frame
     "Work"
     (lambda ()
       (spinneret:with-html
         (:h1 "Work")
         (:p.lede "The work marks of the wiki: figures whose title starts with a status word.
They live beside the design they move; this is only a view.")
         (loop for (status . meaning) in *work-statuses*
               for these = (remove status marks :key #'heading-keyword :test-not #'string=)
               when these
                 do (:section.work-status
                     (:h2 (:span :class (format nil "mark mark-~(~A~)" status) status)
                          " " (:span.status-meaning meaning))
                     (:table.work
                      (:tbody
                       (dolist (mark these)
                         (:tr
                          (:td.work-title
                           (:a :href (figure-href (heading-id mark) :site site)
                               (render-inlines (heading-title mark)))
                           (:span.work-page (document-name (heading-document mark))))
                          (:td.work-intent
                           (let ((excerpt (figure-excerpt mark 260)))
                             (when excerpt excerpt)))))))))))
     :body-class "wide")))

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

(defun write-stylesheet (directory)
  "Compile the style definitions and write them as style.css in DIRECTORY."
  (let ((pathname (merge-pathnames "style.css" directory)))
    (ensure-directories-exist pathname)
    (with-open-file (out pathname :direction :output :if-exists :supersede
                                  :external-format :utf-8)
      (write-string (luv.css:stylesheet-text) out))
    pathname))

(defun write-site (site directory &key (stylesheet t))
  "Write every page of SITE, the figures index, and, unless STYLESHEET is
NIL, the compiled stylesheet into DIRECTORY."
  (let ((*site* site)
        (directory (uiop:ensure-directory-pathname directory)))
    (dolist (document (site-documents site))
      (write-html-file (merge-pathnames (site-page-name document) directory)
                       (lambda () (render-page document))))
    (write-html-file (merge-pathnames "pages.html" directory)
                     (lambda () (render-pages-page site)))
    (write-html-file (merge-pathnames "work.html" directory)
                     (lambda () (render-work-page site)))
    (when (site-source-files site)
      (write-html-file (merge-pathnames "source.html" directory)
                       (lambda () (render-source-index site)))
      (dolist (file (site-source-files site))
        (write-html-file (merge-pathnames (source-page-name file) directory)
                         (lambda () (render-source-page file)))))
    (when stylesheet
      (write-stylesheet directory))
    directory))
