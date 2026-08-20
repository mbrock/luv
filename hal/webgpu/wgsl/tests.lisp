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
  (let* ((bundles (luvcraft.body-gallery:compile-body-gallery))
         (gnome (first bundles))
         (cat (second bundles))
         (gnome-fragment
           (luvcraft.body-gallery:body-gallery-bundle-fragment gnome))
         (cat-fragment
           (luvcraft.body-gallery:body-gallery-bundle-fragment cat)))
    (ok (= 2 (length bundles)))
    (ok (string= "gnome"
                 (luvcraft.body-gallery:web-body-id
                  (luvcraft.body-gallery:body-gallery-bundle-body gnome))))
    (ok (string= "cat"
                 (luvcraft.body-gallery:web-body-id
                  (luvcraft.body-gallery:body-gallery-bundle-body cat))))
    (ok (= 18 (length (wgsl:wgsl-document-overrides gnome-fragment))))
    (ok (= 8 (length (wgsl:wgsl-document-overrides cat-fragment))))
    (ok (null (wgsl:wgsl-document-overrides
               (luvcraft.body-gallery:body-gallery-bundle-vertex gnome))))
    (ok (search "@location(2) figure_facing: vec4<f32>"
                (wgsl:wgsl-document-source
                 (luvcraft.body-gallery:body-gallery-bundle-vertex gnome))))
    (ok (search "override knob_gnome_stature: f32 = 0.8f;"
                (wgsl:wgsl-document-source gnome-fragment)))
    (ok (search "override knob_cat_stripe_strength: f32 = 0.72f;"
                (wgsl:wgsl-document-source cat-fragment)))
    (let ((json (luvcraft.body-gallery:body-gallery-json bundles)))
      (ok (char= #\{ (char json 0)))
      (ok (search "\"statureKnob\":\"gnome-stature\"" json))
      (ok (search "\"identifier\":\"knob_cat_tail_reach\"" json)))))
