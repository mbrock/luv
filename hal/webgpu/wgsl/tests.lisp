(in-package #:luvcraft.tests)

(shader:define-shader wgsl-position-probe
    (:stage :vertex
     :inputs ((position :vec4 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)))
  (shader:set-output clip-position position))

(deftest wgsl-is-a-structured-sibling-shader-target
  (let* ((specification (wgsl-position-probe))
         (first (wgsl:compile-wgsl specification))
         (second (wgsl:compile-wgsl specification))
         (source (wgsl:wgsl-document-source first)))
    (ok (typep first 'wgsl:wgsl-document))
    (ok (eq specification (wgsl:wgsl-document-specification first)))
    (ok (string= source (wgsl:wgsl-document-source second)))
    (ok (search "@vertex" source))
    (ok (search "@builtin(position)" source))
    ;; The shared graph retains Vulkan's framebuffer-oriented clip Y. The
    ;; target ABI must undo that convention for WebGPU, as MSL does for Metal.
    (ok (search "vec4<f32>((stage_in.position).x, -(stage_in.position).y"
                source))))

(deftest native-agent-bodies-compile-to-webgpu-overrides
  (let* ((bundles (luvcraft.web:compile-body-gallery))
         (gnome (first bundles))
         (cat (second bundles))
         (gnome-fragment
           (luvcraft.web:body-gallery-bundle-fragment gnome))
         (cat-fragment
           (luvcraft.web:body-gallery-bundle-fragment cat)))
    (ok (= 2 (length bundles)))
    (ok (string= "gnome"
                 (luvcraft.web:web-body-id
                  (luvcraft.web:body-gallery-bundle-body gnome))))
    (ok (string= "cat"
                 (luvcraft.web:web-body-id
                  (luvcraft.web:body-gallery-bundle-body cat))))
    (ok (= 18 (length (wgsl:wgsl-document-overrides gnome-fragment))))
    (ok (= 8 (length (wgsl:wgsl-document-overrides cat-fragment))))
    (ok (null (wgsl:wgsl-document-overrides
               (luvcraft.web:body-gallery-bundle-vertex gnome))))
    (ok (search "@location(2) figure_facing: vec4<f32>"
                (wgsl:wgsl-document-source
                 (luvcraft.web:body-gallery-bundle-vertex gnome))))
    (ok (search "override knob_gnome_stature: f32 = 0.8f;"
                (wgsl:wgsl-document-source gnome-fragment)))
    (ok (search "override knob_cat_stripe_strength: f32 = 0.72f;"
                (wgsl:wgsl-document-source cat-fragment)))
    (dolist (source (list (wgsl:wgsl-document-source gnome-fragment)
                          (wgsl:wgsl-document-source cat-fragment)))
      (ok (search "break;" source)))
    (flet ((shading-follows-hit-guard (source marker)
             (let ((guard (search "if ((coverage > 0.0f)) {" source))
                   (shading (search marker source)))
               (and guard shading (< guard shading)))))
      (ok (shading-follows-hit-guard
           (wgsl:wgsl-document-source gnome-fragment)
           "let gnome_shaded_color"))
      (ok (shading-follows-hit-guard
           (wgsl:wgsl-document-source cat-fragment)
           "let cat_shaded_color")))
    (let ((json (luvcraft.web:body-gallery-json bundles)))
      (ok (char= #\{ (char json 0)))
      (ok (search "\"statureKnob\":\"gnome-stature\"" json))
      (ok (search "\"identifier\":\"knob_cat_tail_reach\"" json)))))

(deftest luvcraft-web-mounts-its-semantic-pages
  (let ((directory
          (uiop:ensure-directory-pathname
           (merge-pathnames "luvcraft-showcase-test/"
                            (uiop:temporary-directory)))))
    (unwind-protect
         (progn
           (ensure-directories-exist directory)
           (with-open-file (stream (merge-pathnames "manifest.sexp" directory)
                                   :direction :output :if-exists :supersede)
             (write '(:version 1 :source-revision "abc123def456"
                      :captures
                      ((:name "proposal-still" :figure "Y7X7WK"
                        :kind :image :file "Y7X7WK-proposal-still.png")
                       (:name "proposal-orbit" :figure "Y7X7WK"
                        :kind :video :file "Y7X7WK-proposal-orbit.mp4")))
                    :stream stream))
           (let* ((bundles (luvcraft.web:compile-body-gallery))
                  (application
                    (luvcraft.web:make-luvcraft-web-application
                     :bundles bundles :showcase-directory directory))
                  (index
                    (luvcraft.web:respond-to-web-request application "/"))
                  (gallery
                    (luvcraft.web:respond-to-web-request
                     application "/bodies/"))
                  (showcase
                    (luvcraft.web:respond-to-web-request
                     application "/showcase/"))
                  (catalog
                    (luvcraft.web:respond-to-web-request
                     application "/bodies/bodies.json"))
                  (gallery-js
                    (luvcraft.web:respond-to-web-request
                     application "/bodies/gallery.js"))
                  (shader
                    (luvcraft.web:respond-to-web-request
                     application "/bodies/body/gnome/fragment.wgsl"))
                  (missing
                    (luvcraft.web:respond-to-web-request application "/nope")))
             (ok (string= "200 OK"
                          (luvcraft.web:web-response-status index)))
             (ok (search "href=\"/bodies/\""
                         (luvcraft.web:web-response-body index)))
             (ok (search "href=\"/showcase/\""
                         (luvcraft.web:web-response-body index)))
             (ok (string= "200 OK"
                          (luvcraft.web:web-response-status gallery)))
             (ok (search "/bodies/gallery.js"
                         (luvcraft.web:web-response-body gallery)))
             (ok (search
                  "\"vertexUrl\":\"\\/bodies\\/body\\/gnome\\/vertex.wgsl\""
                  (luvcraft.web:web-response-body catalog)))
             (ok (search "const FRAME_INTERVAL = 1000 / 60;"
                         (luvcraft.web:web-response-body gallery-js)))
             (ok (search "const RENDER_SCALE = 1;"
                         (luvcraft.web:web-response-body gallery-js)))
             (ok (search "if (!cameraBufferDirty) return;"
                         (luvcraft.web:web-response-body gallery-js)))
             (ok (search "@fragment"
                         (luvcraft.web:web-response-body shader)))
             (ok (search "/showcase/media/Y7X7WK-proposal-still.png"
                         (luvcraft.web:web-response-body showcase)))
             (ok (search "/showcase/media/Y7X7WK-proposal-orbit.mp4"
                         (luvcraft.web:web-response-body showcase)))
             (ok (search "abc123def456"
                         (luvcraft.web:web-response-body showcase)))
             (ok (string= "404 Not Found"
                          (luvcraft.web:web-response-status missing)))))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore))))
