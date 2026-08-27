(in-package #:luvcraft.tests)

(shader:define-shader wgsl-position-probe
    (:stage :vertex
     :inputs ((position :vec4 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)))
  (shader:set-output clip-position position))

(define-test wgsl-is-a-structured-sibling-shader-target
  (let* ((specification (wgsl-position-probe))
         (first (wgsl:compile-wgsl specification))
         (second (wgsl:compile-wgsl specification))
         (source (wgsl:wgsl-document-source first)))
    (true (typep first 'wgsl:wgsl-document))
    (true (eq specification (wgsl:wgsl-document-specification first)))
    (true (string= source (wgsl:wgsl-document-source second)))
    (true (search "@vertex" source))
    (true (search "@builtin(position)" source))
    ;; The shared graph retains Vulkan's framebuffer-oriented clip Y. The
    ;; target ABI must undo that convention for WebGPU, as MSL does for Metal.
    (true (search "vec4<f32>((stage_in.position).x, -(stage_in.position).y"
                  source))))

(define-test native-agent-bodies-compile-to-webgpu-overrides
  (let* ((bundles (luvcraft.web:compile-body-gallery))
         (gnome (first bundles))
         (cat (second bundles))
         (gnome-fragment
           (luvcraft.web:body-gallery-bundle-fragment gnome))
         (cat-fragment
           (luvcraft.web:body-gallery-bundle-fragment cat)))
    (true (= 2 (length bundles)))
    (true (string= "gnome"
                   (luvcraft.web:web-body-id
                    (luvcraft.web:body-gallery-bundle-body gnome))))
    (true (string= "cat"
                   (luvcraft.web:web-body-id
                    (luvcraft.web:body-gallery-bundle-body cat))))
    (true (= 18 (length (wgsl:wgsl-document-overrides gnome-fragment))))
    (true (= 8 (length (wgsl:wgsl-document-overrides cat-fragment))))
    (true (null (wgsl:wgsl-document-overrides
                 (luvcraft.web:body-gallery-bundle-vertex gnome))))
    (true (search "@location(2) figure_facing: vec4<f32>"
                  (wgsl:wgsl-document-source
                   (luvcraft.web:body-gallery-bundle-vertex gnome))))
    (true (search "override knob_gnome_stature: f32 = 0.8f;"
                  (wgsl:wgsl-document-source gnome-fragment)))
    (true (search "override knob_cat_stripe_strength: f32 = 0.72f;"
                  (wgsl:wgsl-document-source cat-fragment)))
    (dolist (source (list (wgsl:wgsl-document-source gnome-fragment)
                          (wgsl:wgsl-document-source cat-fragment)))
      (true (search "break;" source)))
    (flet ((shading-follows-hit-guard (source marker)
             (let ((guard (search "if ((coverage > 0.0f)) {" source))
                   (shading (search marker source)))
               (and guard shading (< guard shading)))))
      (true (shading-follows-hit-guard
             (wgsl:wgsl-document-source gnome-fragment)
             "let gnome_shaded_color"))
      (true (shading-follows-hit-guard
             (wgsl:wgsl-document-source cat-fragment)
             "let cat_shaded_color")))
    (let ((json (luvcraft.web:body-gallery-json bundles)))
      (true (char= #\{ (char json 0)))
      (true (search "\"statureKnob\":\"gnome-stature\"" json))
      (true (search "\"identifier\":\"knob_cat_tail_reach\"" json)))))

(define-test luvcraft-web-mounts-its-semantic-pages
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
                        :kind :image :file "Y7X7WK-proposal-still.png"
                        :layout :landscape :width 1200 :height 800
                        :variants
                        ((:file "Y7X7WK-proposal-still-480w.webp"
                          :type "image/webp" :width 480 :height 320)
                         (:file "Y7X7WK-proposal-still-768w.webp"
                          :type "image/webp" :width 768 :height 512)))
                       (:name "proposal-orbit" :figure "Y7X7WK"
                        :kind :video :file "Y7X7WK-proposal-orbit.mp4"
                        :layout :portrait :width 720 :height 1280
                        :poster
                        (:file "Y7X7WK-proposal-orbit-poster-480w.webp"
                         :type "image/webp" :width 480 :height 854))))
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
             (true (= 200 (first index)))
             (true (search "href=\"/bodies/\""
                           (first (third index))))
             (true (search "href=\"/showcase/\""
                           (first (third index))))
             (true (= 200 (first gallery)))
             (true (search "/bodies/gallery.js"
                           (first (third gallery))))
             (true (search
                    "\"vertexUrl\":\"\\/bodies\\/body\\/gnome\\/vertex.wgsl\""
                    (first (third catalog))))
             (true (search "const FRAME_INTERVAL = 1000 / 60;"
                           (first (third gallery-js))))
             (true (search "const RENDER_SCALE = 1;"
                           (first (third gallery-js))))
             (true (search "if (!cameraBufferDirty) return;"
                           (first (third gallery-js))))
             (true (search "new URLSearchParams(location.hash.slice(1))"
                           (first (third gallery-js))))
             (true (search "parameters.set(knob.name"
                           (first (third gallery-js))))
             (true (search
                    "history.replaceState(null, \"\", `#${parameters}`)"
                    (first (third gallery-js))))
             (true (search "@fragment"
                           (first (third shader))))
             (true (search "/showcase/media/Y7X7WK-proposal-still.png"
                           (first (third showcase))))
             (true (search "src=\"/showcase/media/Y7X7WK-proposal-still-768w.webp\""
                           (first (third showcase))))
             (true (search "Y7X7WK-proposal-still-480w.webp 480w, /showcase/media/Y7X7WK-proposal-still-768w.webp 768w"
                           (first (third showcase))))
             (true (search "sizes=\"(max-width: 59.5rem)"
                           (first (third showcase))))
             (true (search "title=\"Open full-resolution image\""
                           (first (third showcase))))
             (true (search "/showcase/media/Y7X7WK-proposal-orbit.mp4"
                           (first (third showcase))))
             (true (search "class=portrait><div class=\"media portrait\">"
                           (first (third showcase))))
             (true (search "preload=none poster=\"/showcase/media/Y7X7WK-proposal-orbit-poster-480w.webp\""
                           (first (third showcase))))
             (true (search ".media.portrait img,.media.portrait video{object-fit:contain}"
                           (first (third showcase))))
             (true (search "abc123def456"
                           (first (third showcase))))
             (true (= 404 (first missing)))))
      (uiop:delete-directory-tree directory :validate t
                                            :if-does-not-exist :ignore))))
