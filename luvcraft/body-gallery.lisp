;;;; The browser edge for luvcraft's analytic agent bodies.
;;;;
;;;; A WEB-BODY-TYPE is the small piece of application knowledge that the
;;;; browser needs around the shader graph: which shader role names the body,
;;;; which knob group belongs to it, and the conservative sphere that its
;;;; billboard rasterizes.  Everything else -- WGSL and knob metadata -- is
;;;; derived from the same Lisp objects that the native game uses.

(in-package #:luvcraft.web)

(defclass web-body-type ()
  ((id :initarg :id :reader web-body-id)
   (label :initarg :label :reader web-body-label)
   (role :initarg :role :reader web-body-role)
   (knob-group :initarg :knob-group :reader web-body-knob-group)
   (center-height :initarg :center-height :reader web-body-center-height)
   (radius :initarg :radius :reader web-body-radius))
  (:documentation
   "One native analytic body as a browser-facing shader and framing preset."))

(defparameter *web-body-types*
  (list (make-instance 'web-body-type
                       :id "gnome" :label "Gnome" :role :gnome-sdf
                       ;; Covers GNOME-FIGURE-RADIUS throughout every
                       ;; published knob range, including the tallest leaned hat.
                       :knob-group :gnome :center-height 0.85 :radius 1.90)
        (make-instance 'web-body-type
                       :id "cat" :label "Cat" :role :cat-sdf
                       ;; Covers CAT-FIGURE-RADIUS throughout its knob ranges.
                       :knob-group :cat :center-height 0.64 :radius 1.16)))

(defun web-body-types ()
  "The ordered body catalog shown by the web gallery."
  (copy-list *web-body-types*))

(defclass body-gallery-bundle ()
  ((body :initarg :body :reader body-gallery-bundle-body)
   (vertex :initarg :vertex :reader body-gallery-bundle-vertex)
   (fragment :initarg :fragment :reader body-gallery-bundle-fragment)))

(defun compile-body-gallery ()
  "Compile every catalog body from the shared shader graph to WGSL."
  (loop for body in (web-body-types)
        for knobs = (luvcraft:knobs-in-group (web-body-knob-group body))
        for overrides = (mapcar #'luvcraft:knob-name knobs)
        collect
        (make-instance
         'body-gallery-bundle
         :body body
         :vertex (wgsl:compile-wgsl
                  (shader:shader-specification-for (web-body-role body) :vertex)
                  :overrides overrides)
         :fragment (wgsl:compile-wgsl
                    (shader:shader-specification-for (web-body-role body) :fragment)
                    :overrides overrides))))

(defun web-name (name)
  (string-downcase (string name)))

(defun override-for-knob (knob bundle)
  (let ((name (luvcraft:knob-name knob)))
    (or (find name (wgsl:wgsl-document-overrides
                    (body-gallery-bundle-vertex bundle))
              :key #'wgsl:wgsl-override-name)
        (find name (wgsl:wgsl-document-overrides
                    (body-gallery-bundle-fragment bundle))
              :key #'wgsl:wgsl-override-name))))

(defun knob-stages (knob bundle)
  (let ((name (luvcraft:knob-name knob)))
    (loop for (stage document)
            in `(("vertex" ,(body-gallery-bundle-vertex bundle))
                 ("fragment" ,(body-gallery-bundle-fragment bundle)))
          when (find name (wgsl:wgsl-document-overrides document)
                     :key #'wgsl:wgsl-override-name)
            collect stage)))

(defun knob-json-object (knob bundle)
  (let ((override (override-for-knob knob bundle)))
    (unless override
      (error "Body knob ~S did not occur in either compiled shader."
             (luvcraft:knob-name knob)))
    `(("name" . ,(web-name (luvcraft:knob-name knob)))
      ("identifier" . ,(wgsl:wgsl-override-identifier override))
      ("label" . ,(luvcraft:knob-label knob))
      ("documentation" . ,(or (luvcraft:knob-documentation knob) ""))
      ("default" . ,(luvcraft:knob-value knob nil))
      ("minimum" . ,(luvcraft:knob-minimum knob))
      ("maximum" . ,(luvcraft:knob-maximum knob))
      ("step" . ,(luvcraft:knob-step knob))
      ("unit" . ,(luvcraft:knob-unit-label knob))
      ("stages" . ,(knob-stages knob bundle)))))

(defun bundle-json-object (bundle base-path)
  (let* ((body (body-gallery-bundle-body bundle))
         (id (web-body-id body))
         (knobs (luvcraft:knobs-in-group (web-body-knob-group body))))
    `(("id" . ,id)
      ("label" . ,(web-body-label body))
      ("centerHeight" . ,(web-body-center-height body))
      ("radius" . ,(web-body-radius body))
      ("statureKnob" . ,(format nil "~A-stature" id))
      ("vertexUrl" . ,(format nil "~A/body/~A/vertex.wgsl" base-path id))
      ("fragmentUrl" . ,(format nil "~A/body/~A/fragment.wgsl" base-path id))
      ("knobs" . ,(mapcar
                    (lambda (knob) (knob-json-object knob bundle))
                    knobs)))))

(defun body-gallery-json (&optional (bundles (compile-body-gallery))
                           (base-path "/bodies"))
  "Encode the catalog and actual native knob metadata for JavaScript."
  (cl-json:encode-json-alist-to-string
   `(("bodies" . ,(mapcar (lambda (bundle)
                             (bundle-json-object bundle base-path))
                           bundles)))))

(defun gallery-asset (name)
  (uiop:read-file-string
   (asdf:system-relative-pathname
    "luvcraft/web" (format nil "luvcraft/web/~A" name))))

(defun find-bundle (id bundles)
  (find id bundles :test #'string=
        :key (lambda (bundle)
               (web-body-id (body-gallery-bundle-body bundle)))))

(defun body-shader-response (path bundles)
  (let* ((parts (uiop:split-string path :separator '(#\/)))
         (id (third parts))
         (file (fourth parts))
         (bundle (and (= 4 (length parts)) (find-bundle id bundles))))
    (when bundle
      (cond ((string= file "vertex.wgsl")
             (wgsl:wgsl-document-source
              (body-gallery-bundle-vertex bundle)))
            ((string= file "fragment.wgsl")
             (wgsl:wgsl-document-source
              (body-gallery-bundle-fragment bundle)))))))

(defclass body-gallery-page (web-page)
  ((bundles :initarg :bundles :reader body-gallery-page-bundles)
   (catalog-json :initarg :catalog-json :reader body-gallery-page-catalog-json)))

(defun make-body-gallery-page (&optional (bundles (compile-body-gallery)))
  (make-instance 'body-gallery-page
                 :path "/bodies"
                 :label "Agent bodies"
                 :description "Gnomes and cats, compiled from luv shaders to live WebGPU."
                 :bundles bundles
                 :catalog-json (body-gallery-json bundles)))

(defmethod respond-to-web-request ((page body-gallery-page) path)
  (cond ((or (string= path "/") (string= path "/index.html"))
         (ok-response "text/html; charset=utf-8"
                      (gallery-asset "body-gallery.html")))
        ((string= path "/gallery.js")
         (ok-response "text/javascript; charset=utf-8"
                      (gallery-asset "body-gallery.js")))
        ((string= path "/gallery.css")
         (ok-response "text/css; charset=utf-8"
                      (gallery-asset "body-gallery.css")))
        ((string= path "/bodies.json")
         (ok-response "application/json; charset=utf-8"
                      (body-gallery-page-catalog-json page)))
        (t
         (let ((source (body-shader-response
                        path (body-gallery-page-bundles page))))
           (if source
               (ok-response "text/wgsl; charset=utf-8" source)
               (call-next-method))))))

(defun make-luvcraft-web-application (&key bundles)
  "Build the luvcraft web application and its currently installed pages."
  (make-web-application
   (if bundles
       (make-body-gallery-page bundles)
       (make-body-gallery-page))))

(defun serve-luvcraft-web (&key (host "127.0.0.1") (port 8765))
  "Compile the web-facing artifacts once, then serve luvcraft until stopped."
  (serve-web-application (make-luvcraft-web-application)
                         :host host :port port))
