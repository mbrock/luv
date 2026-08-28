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

(defun render-body-gallery (site)
  "Render the SDF playground inside the wiki's common page frame."
  (let ((luv.wiki::*site* site)
        (luv.wiki::*rendering-document* nil)
        (luv.wiki::*page-prefix* "../")
        (luv.wiki::*page-kind* "bodies"))
    (luv.wiki::render-page-frame
     "Agent bodies"
     (lambda ()
       (spinneret:with-html
         (:div.body-layout
           (:section.body-stage :aria-label "Agent body viewer"
             (:canvas#body-canvas)
             (:div.body-stage-copy
               (:p.eyebrow "Luvcraft field notes")
               (:h1#body-title "Agent bodies")
               (:p#status "Waking WebGPU…"))
             (:p.orbit-hint "drag to orbit · scroll to approach"))
           (:aside.body-workbench
             (:header
               (:p.eyebrow "Body cabinet")
               (:nav#body-picker :aria-label "Choose an agent body"))
             (:div#knobs.knobs)
             (:footer
               (:button#reset :type "button" "Reset this creature")
               (:span "WGSL compiled by luv"))))
         (:script :type "module" :src "/bodies/gallery.js")))
     :layout :workspace
     :body-class "body-playground"
     :kind "bodies"
     :crumbs '(("Agent bodies"))
     :right "SDF playground")))

(defun shader-resources (bundles)
  (loop for bundle in bundles
        for body = (body-gallery-bundle-body bundle)
        for id = (web-body-id body)
        append
        (loop for (stage document)
                in `(("vertex" ,(body-gallery-bundle-vertex bundle))
                     ("fragment" ,(body-gallery-bundle-fragment bundle)))
              for source = (wgsl:wgsl-document-source document)
              for path = (format nil "/bodies/body/~A/~A.wgsl" id stage)
              for output = (format nil "bodies/body/~A/~A.wgsl" id stage)
              collect (let ((source source))
                        (luv.wiki:make-generated-resource
                         path output "text/wgsl; charset=utf-8"
                         (lambda () source))))))

(defun body-gallery-resources (site)
  "Compile the gallery once and expose its page, data, program, and shaders."
  (let* ((bundles (compile-body-gallery))
         (catalog (body-gallery-json bundles)))
    (append
     (list
      (luv.wiki:make-generated-resource
       "/bodies/" "bodies/index.html" "text/html; charset=utf-8"
       (lambda ()
         (with-output-to-string (stream)
           (luv.wiki::call-with-html-output
            stream (lambda () (render-body-gallery site)))))
       :label "Bodies"
       :description "live analytic SDF creatures"
       :kind "bodies")
      (luv.wiki:make-generated-resource
       "/bodies/gallery.js" "bodies/gallery.js"
       "text/javascript; charset=utf-8" #'body-gallery-javascript)
      (luv.wiki:make-generated-resource
       "/bodies/bodies.json" "bodies/bodies.json"
       "application/json; charset=utf-8" (lambda () catalog)))
     (shader-resources bundles))))

(luv.wiki:register-resource-provider 'body-gallery #'body-gallery-resources)
