;;;; Typst output for printable wiki pages and Typst-native web diagrams.

(in-package #:luv.wiki)

(defparameter *wiki-public-url* "https://mbrock.github.io/luv/")

(defun typst-string (string)
  "STRING as a quoted Typst string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for character across string
          do (case character
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char character out))))
    (write-char #\" out)))

(defvar *typst-document* nil)
(defvar *typst-stream* nil)
(defvar *typst-body-font* nil)

(defgeneric render-typst (element)
  (:documentation "Emit Typst markup for ELEMENT to *TYPST-STREAM*."))

(defun typst-prose-text (string)
  "Collapse Org source whitespace as browsers do for ordinary prose."
  (let ((space-before nil))
    (with-output-to-string (out)
      (loop for character across string
            for space = (member character '(#\Space #\Tab #\Newline #\Return))
            unless (and space space-before)
              do (write-char (if space #\Space character) out)
            do (setf space-before space)))))

(defmethod render-typst ((string string))
  (format *typst-stream* "#text(~A)" (typst-string (typst-prose-text string))))

(defun render-typst-inlines (inlines)
  (mapc #'render-typst inlines))

(defmethod render-typst ((element element))
  (mapc #'render-typst (element-children element)))

(defmethod render-typst ((paragraph paragraph))
  (write-string "#par[" *typst-stream*)
  (render-typst-inlines (element-children paragraph))
  (format *typst-stream* "]~%~%"))

(defmethod render-typst ((list plain-list))
  (format *typst-stream* "#~A(~%" (if (plain-list-ordered-p list) "enum" "list"))
  (dolist (item (element-children list))
    (write-string "  [" *typst-stream*)
    (render-typst item)
    (format *typst-stream* "],~%"))
  (format *typst-stream* ")~%~%"))

(defmethod render-typst ((item list-item))
  (mapc #'render-typst (element-children item)))

(defmethod render-typst ((block example-block))
  (format *typst-stream* "#example(~A)~%~%" (typst-string (block-text block))))

(defun typst-dexp-style (node role)
  (cond ((role-p role "operator") "operator")
        ((typep node 'lisp-comment) "comment")
        ((typep node 'lisp-string) "string")
        ((or (typep node 'lisp-number) (typep node 'lisp-character)) "number")
        ((and (typep node 'lisp-symbol)
              (equal (lisp-symbol-package node) "KEYWORD"))
         "keyword")
        ((or (typep node 'lisp-conditional) (typep node 'lisp-skipped)) "muted")
        (t "symbol")))

(defun write-typst-dexp-array (objects writer)
  (write-char #\( *typst-stream*)
  (dolist (object objects)
    (funcall writer object)
    (write-char #\, *typst-stream*))
  (write-char #\) *typst-stream*))

(defun write-typst-dexp-token (node role)
  (format *typst-stream* "(kind: \"atom\", style: ~A, text: ~A)"
          (typst-string (typst-dexp-style node role))
          (typst-string (node-text node))))

(defun write-typst-dexp-item (item)
  (ecase (item-kind item)
    (:comment (write-typst-dexp-node (item-node item)))
    (:child (write-typst-dexp-node (item-node item) (item-role item)))
    (:pair
     (write-string "(kind: \"pair\", items: " *typst-stream*)
     (write-typst-dexp-array
      (append (list (item-node item)) (item-comments item) (list (item-value item)))
      (lambda (node) (write-typst-dexp-node node)))
     (write-char #\) *typst-stream*))))

(defun write-typst-dexp-list (list role)
  (let* ((operator (symbol-node-name (first (element-children list))))
         (layout (list-layout list role))
         (items (layout-items layout list))
         (stacked-p (some #'item-body-p items))
         (structure (cond (stacked-p "stacked")
                          ((typep layout 'grid-layout) "column")
                          (t "choice"))))
    (format *typst-stream* "(kind: \"list\", structure: ~A, callee: ~A, "
            (typst-string structure)
            (if (and operator (layout-callee-p layout)) "true" "false"))
    (if stacked-p
        (multiple-value-bind (head rows) (split-head-items items)
          (write-string "head: " *typst-stream*)
          (write-typst-dexp-array head #'write-typst-dexp-item)
          (write-string ", rows: " *typst-stream*)
          (write-typst-dexp-array rows #'write-typst-dexp-item))
        (progn
          (write-string "items: " *typst-stream*)
          (write-typst-dexp-array items #'write-typst-dexp-item)))
    (write-char #\) *typst-stream*)))

(defun write-typst-dexp-node (node &optional role)
  (typecase node
    (lisp-vector
     (write-string "(kind: \"prefix\", prefix: \"#\", child: " *typst-stream*)
     (write-typst-dexp-list node role)
     (write-char #\) *typst-stream*))
    (lisp-list (write-typst-dexp-list node role))
    (lisp-prefix
     (format *typst-stream* "(kind: \"prefix\", prefix: ~A, child: "
             (typst-string (lisp-prefix-string node)))
     (write-typst-dexp-node (lisp-prefix-child node))
     (write-char #\) *typst-stream*))
    (t (write-typst-dexp-token node role))))

(defun render-typst-lisp-source (text)
  "Read Lisp source and emit semantic DEXP data; fall back to raw text when
the source has no readable nodes."
  (let ((nodes (handler-case (read-lisp-string text)
                 (error (condition)
                   (warn "Rendering Lisp source as raw Typst: ~A" condition)
                   nil))))
    (if nodes
        (progn
          (write-string "#dexp-source(" *typst-stream*)
          (write-typst-dexp-array nodes (lambda (node) (write-typst-dexp-node node)))
          (format *typst-stream* ")~%~%"))
        (format *typst-stream* "#raw(~A, block: true, lang: \"lisp\")~%~%"
                (typst-string text)))))

(defmethod render-typst ((block src-block))
  (cond ((string= (src-block-language block) "typst-diagram")
         (format *typst-stream* "#block(width: 100%)[~%~A~%]~%~%" (block-text block)))
        ((string= (src-block-language block) "lisp-figure")
         (write-figure-typst
          (site-lisp-figure block *typst-document* *site*) *typst-stream*)
         (terpri *typst-stream*))
        ((string= (src-block-language block) "lisp")
         (render-typst-lisp-source (block-text block)))
        (t
         (format *typst-stream* "#raw(~A, block: true~@[, lang: ~A~])~%~%"
                 (typst-string (block-text block))
                 (and (src-block-language block)
                      (typst-string (src-block-language block)))))))

(defmethod render-typst ((table table))
  (let ((columns (length (first (table-rows table)))))
    (format *typst-stream* "#table(columns: ~D,~%" columns)
    (loop for row in (table-rows table)
          for first = t then nil
          do (when (and first (table-header-p table))
               (write-string "  table.header(" *typst-stream*))
             (loop for cell in row
                   for first-cell = t then nil
                   do (unless (or first-cell (not (and first (table-header-p table))))
                        (write-char #\, *typst-stream*))
                      (write-string "[" *typst-stream*)
                      (render-typst-inlines cell)
                      (write-string "]" *typst-stream*)
                      (unless (and first (table-header-p table))
                        (write-char #\, *typst-stream*)))
             (when (and first (table-header-p table))
               (write-string ")," *typst-stream*))
             (terpri *typst-stream*))
    (format *typst-stream* ")~%~%")))

(defmethod render-typst ((emphasis emphasis))
  (let ((function (case (emphasis-kind emphasis)
                    (:bold "strong")
                    (:italic "emph")
                    ((:verbatim :code) "raw")
                    (t "text"))))
    (if (member (emphasis-kind emphasis) '(:verbatim :code))
        (format *typst-stream* "#~A(~A)" function
                (typst-string (inline-text (element-children emphasis))))
        (progn
          (format *typst-stream* "#~A[" function)
          (render-typst-inlines (element-children emphasis))
          (write-char #\] *typst-stream*)))))

(defun typst-web-link (link)
  (cond ((member (link-protocol link) '("https" "http") :test #'string=)
         (format nil "~A:~A" (link-protocol link) (link-path link)))
        ((string= (or (link-protocol link) "") "file")
         (format nil "~A~A.html" *wiki-public-url* (pathname-name (link-path link))))
        ((string= (or (link-protocol link) "") "id")
         (concatenate 'string *wiki-public-url*
                      (site-page-name (heading-document (find-figure (link-path link))))
                      "#" (link-path link)))
        (t nil)))

(defmethod render-typst ((link link))
  (let ((description (or (element-children link) (list (link-path link))))
        (id (and (string= (or (link-protocol link) "") "id") (link-path link)))
        (page-link-p (string= (or (link-protocol link) "") "file")))
    (cond ((and id (find id (document-figures *typst-document*)
                         :key #'heading-id :test #'string=))
           (format *typst-stream* "#link(label(~A))[" (typst-string id))
           (render-typst-inlines description)
           (write-char #\] *typst-stream*))
          ((typst-web-link link)
           (format *typst-stream* "#link(~A)[" (typst-string (typst-web-link link)))
           (when page-link-p
             (write-string "#smallcaps[" *typst-stream*))
           (render-typst-inlines description)
           (when page-link-p
             (write-char #\] *typst-stream*))
           (write-char #\] *typst-stream*))
          (t (render-typst-inlines description)))))

(defmethod render-typst ((mention mention))
  (let ((id (mention-id mention)))
    (if (find id (document-figures *typst-document*) :key #'heading-id :test #'string=)
        (format *typst-stream* "#link(label(~A))[#text(~A)]"
                (typst-string id) (typst-string (concatenate 'string "#" id)))
        (let ((figure (find-figure id)))
          (if figure
              (format *typst-stream* "#link(~A)[#text(~A)]"
                      (typst-string
                       (concatenate 'string *wiki-public-url*
                                    (site-page-name (heading-document figure)) "#" id))
                      (typst-string (concatenate 'string "#" id)))
              (format *typst-stream* "#text(~A)"
                      (typst-string (concatenate 'string "#" id))))))))

(defmethod render-typst ((math math))
  ;; Org math is TeX, not Typst math. Preserve the source honestly until the
  ;; reader grows a deliberate translation boundary.
  (format *typst-stream* "#raw(~A)" (typst-string (math-text math))))

(defmethod render-typst ((heading heading))
  (format *typst-stream* "#heading(level: ~D)[" (heading-level heading))
  (when (heading-keyword heading)
    (format *typst-stream* "#text(fill: accent, weight: \"bold\")[~A ]"
            (heading-keyword heading)))
  (render-typst-inlines (heading-title heading))
  (format *typst-stream* "]~@[ <~A>~]~%~%" (heading-id heading))
  (dolist (child (element-children heading))
    (render-typst child)))

(defun render-typst-document (document stream)
  "Write a complete Typst source document for DOCUMENT to STREAM."
  (let ((*typst-document* document)
        (*typst-stream* stream))
    (format stream "#import ~A: workshop, example, dexp-source, accent~%"
            (typst-string "../../wiki/typst-template.typ"))
    (format stream "#show: workshop.with(title: ~A~@[, body-font: (~A, \"Libertinus Serif\")~])~%~%"
            (typst-string (or (document-title document) (document-name document)))
            (and *typst-body-font* (typst-string *typst-body-font*)))
    (mapc #'render-typst (element-children document))))

(defun typst-command (root &rest arguments)
  (uiop:run-program (append (list "typst" "compile" "--root" (namestring root)
                                  "--font-path" (namestring (merge-pathnames "fonts/" root)))
                            arguments)
                    :output *standard-output* :error-output *error-output*))

(defun typst-equity-available-p (root)
  (handler-case
      (with-open-file (stream (merge-pathnames "fonts/equity/EquityOTA-Regular.otf" root)
                              :element-type '(unsigned-byte 8))
        (plusp (file-length stream)))
    (file-error () nil)))

(defun write-typst-pdf (document site output root)
  "Render DOCUMENT through Typst to OUTPUT, retaining generated source nearby."
  (prepare-site-figures site)
  (let* ((*site* site)
         (*typst-body-font*
           (and (typst-equity-available-p root) "Equity OT A"))
         (source (merge-pathnames
                  (make-pathname :name (document-name document) :type "typ")
                  (merge-pathnames "build/typst/" root))))
    (ensure-directories-exist source)
    (with-open-file (stream source :direction :output :if-exists :supersede
                                    :external-format :utf-8)
      (render-typst-document document stream))
    (ensure-directories-exist output)
    (typst-command root (namestring source) (namestring output))
    output))

(defun typst-diagram-blocks (document)
  (let ((blocks '()))
    (map-elements (lambda (element)
                    (when (and (typep element 'src-block)
                               (string= (src-block-language element) "typst-diagram"))
                      (push element blocks)))
                  document)
    (nreverse blocks)))

(defun typst-diagram-name (block document)
  (let ((position (position block (typst-diagram-blocks document) :test #'eq)))
    (unless position (error "Diagram block is not in ~A." (document-name document)))
    (format nil "~A-~D.svg" (document-name document) (1+ position))))

(defvar *typst-diagram-cache* (make-hash-table :test 'equal))

(defun typst-diagram-document (block site)
  (or (find-if (lambda (document)
                 (find block (typst-diagram-blocks document) :test #'eq))
               (site-documents site))
      (error "Diagram block does not belong to this site.")))

(defun render-typst-diagram-svg (block site)
  (or (gethash (block-text block) *typst-diagram-cache*)
      (let* ((root (site-source-directory site))
             (name (typst-diagram-name block (typst-diagram-document block site)))
             (source (merge-pathnames (make-pathname :name (pathname-name name) :type "typ")
                                      (merge-pathnames "build/typst/diagrams/" root)))
             (output (merge-pathnames name (merge-pathnames "build/typst/diagrams/" root))))
        (ensure-directories-exist source)
        (with-open-file (stream source :direction :output :if-exists :supersede
                                       :external-format :utf-8)
          (format stream "#set page(width: auto, height: auto, margin: 7pt, fill: rgb(\"#f7f6f2\"))~%")
          (format stream "#set text(font: ~A, size: 9pt)~%~A~%"
                  (if (typst-equity-available-p root)
                      "(\"Equity OT A\", \"Libertinus Serif\")"
                      "\"Libertinus Serif\"")
                  (block-text block)))
        (typst-command root (namestring source) (namestring output))
        (setf (gethash (block-text block) *typst-diagram-cache*)
              (uiop:read-file-string output)))))

(defun render-typst-diagram-html (block)
  (spinneret:with-html
    (if *rendering-document*
        (let ((name (typst-diagram-name block *rendering-document*)))
          (:figure.typst-diagram
           (:img :src (concatenate 'string *page-prefix* "diagrams/" name)
                 :alt "" :loading "lazy")))
        (:pre.src :data-language "typst-diagram" (:code (block-text block))))))

(defun typst-diagram-resources (site)
  (loop for document in (site-documents site)
        append (loop for block in (typst-diagram-blocks document)
                     for name = (typst-diagram-name block document)
                     collect (let ((block block))
                               (make-generated-resource
                                (concatenate 'string "/diagrams/" name)
                                (concatenate 'string "diagrams/" name)
                                "image/svg+xml"
                                (lambda () (render-typst-diagram-svg block site)))))))

(register-resource-provider 'typst-diagrams #'typst-diagram-resources)

;;; Lisp figures

(defun lisp-figure-document (block site)
  (or (find-if (lambda (document)
                 (find block (lisp-figure-blocks document) :test #'eq))
               (site-documents site))
      (error "Lisp figure block does not belong to this site.")))

(defun site-lisp-figure (block document site)
  (or (gethash block (site-figure-values site))
      (setf (gethash block (site-figure-values site))
            (evaluate-lisp-figure block document))))

(defun prepare-site-figures (site)
  "Evaluate and validate every trusted lisp-figure block in SITE once."
  (dolist (document (site-documents site) site)
    (dolist (block (lisp-figure-blocks document))
      (site-lisp-figure block document site))))

(defun lisp-figure-name (block document)
  (let ((position (position block (lisp-figure-blocks document) :test #'eq)))
    (unless position (error "Lisp figure block is not in ~A." (document-name document)))
    (format nil "~A-figure-~D.svg" (document-name document) (1+ position))))

(defun render-lisp-figure-svg (block site)
  (or (gethash block (site-figure-svgs site))
      (let* ((document (lisp-figure-document block site))
             (figure (site-lisp-figure block document site))
             (root (site-source-directory site))
             (name (lisp-figure-name block document))
             (source (merge-pathnames (make-pathname :name (pathname-name name) :type "typ")
                                      (merge-pathnames "build/typst/figures/" root)))
             (output (merge-pathnames name (merge-pathnames "build/typst/figures/" root))))
        (ensure-directories-exist source)
        (with-open-file (stream source :direction :output :if-exists :supersede
                                       :external-format :utf-8)
          (format stream "#set page(width: auto, height: auto, margin: 7pt, fill: rgb(\"#f7f6f2\"))~%")
          (format stream "#set text(font: ~A, size: 9pt)~%"
                  (if (typst-equity-available-p root)
                      "(\"Equity OT A\", \"Libertinus Serif\")"
                      "\"Libertinus Serif\""))
          (write-figure-typst figure stream))
        (typst-command root (namestring source) (namestring output))
        (setf (gethash block (site-figure-svgs site))
              (uiop:read-file-string output)))))

(defun render-lisp-figure-html (block)
  (if *rendering-document*
      (let* ((figure (site-lisp-figure block *rendering-document* *site*))
             (name (lisp-figure-name block *rendering-document*)))
        (spinneret:with-html
          (:figure.lisp-figure
           (:img :src (concatenate 'string *page-prefix* "figures/" name)
                 :alt (figure-alt-text figure) :loading "lazy"))))
      (spinneret:with-html
        (:pre.src :data-language "lisp-figure" (:code (block-text block))))))

(defun lisp-figure-resources (site)
  (loop for document in (site-documents site)
        append (loop for block in (lisp-figure-blocks document)
                     for name = (lisp-figure-name block document)
                     collect (let ((block block))
                               (make-generated-resource
                                (concatenate 'string "/figures/" name)
                                (concatenate 'string "figures/" name)
                                "image/svg+xml"
                                (lambda () (render-lisp-figure-svg block site)))))))

(register-resource-provider 'lisp-figures #'lisp-figure-resources)
