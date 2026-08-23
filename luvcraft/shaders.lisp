;;; The visible block-world materials as typed mathematical specifications.
;;;
;;; These shader methods are luvcraft's hot-replaceable source of truth: the
;;; live pipelines watch their CLOS role/stage coordinates through MOP
;;; dependents and rebuild on redefinition.  The crosshair vertex stage at the
;;; end is a literal SPIR-V module rather than a mathematical specification.

(in-package #:luvcraft.shaders)

;;; A matrix is representation; this is the meaning of the operation it
;;; participates in.  The four dense rows arrive through the frame ABI, but
;;; applying them is a checked map from an affine cell-valued lattice point to
;;; homogeneous light clip.  A separate checked projection produces the
;;; deliberately packed shadow sample tuple used by the fragment stage.
(define-projective-shader-map :world-to-light
  :domain-type :vec3
  :domain-quantity :world-position
  :domain-unit :cell
  :domain-affine-p t
  :sample-type :vec3
  :sample-components
  ((:xy :quantity :shadow-uv :unit :one)
   (:z :quantity :shadow-depth :unit :one))
  :coordinate-scale (1/2 1/2 1)
  :coordinate-offset (1/2 1/2 0))

;;; The knobs folded into these shaders.  Each is a special this file's
;;; source names where it once held a literal; the parser folds the current
;;; value in, and the pipeline that folded it rebuilds when it moves
;;; (LUVCRAFT/KNOBS.LISP).  Turning one is a shader rebuild, not a uniform
;;; write, so they are the few literals worth that: the ones art direction
;;; keeps asking about.

(defparameter *sun-disc-scale* 4.0
  "How many times its true angular radius the sun's disc is drawn at.")
(defparameter *sun-disc-radiance* 39.0
  "The disc's radiance above display white, which the bloom feeds on.")
(defparameter *direct-light-gain* 0.80
  "The direct sun's diffuse intensity on a lit block face.")
(defparameter *screen-curvature* 0.40
  "How far a terminal faceplate's normal bulges toward its rim.")
(defparameter *scanline-count* 240.0
  "Raster lines across a terminal faceplate's height.")
(defparameter *scanline-depth* 0.20
  "How dark the raster's gaps go, as a fraction of the picture.")
(defparameter *screen-effect-ceiling* 0.6
  "The most a faceplate's raster, hum, flicker, and grain may take.")

(luvcraft:define-knob sun-disc-scale
    (:group :sun :quantity (:quantity :sun-disc-scale :unit :one)
     :unit-label "×" :minimum 1.0 :maximum 12.0 :step 0.5)
    *sun-disc-scale*)
(luvcraft:define-knob sun-disc-radiance
    (:group :sun :quantity (:quantity :sun-disc-radiance :unit :one)
     :minimum 0.0 :maximum 100.0 :step 1.0)
    *sun-disc-radiance*)
(luvcraft:define-knob direct-light-gain
    (:group :sun :quantity (:quantity :direct-light-gain :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 6.0 :step 0.05)
    *direct-light-gain*)
(luvcraft:define-knob screen-curvature
    (:group :terminal :quantity (:quantity :screen-curvature :unit :one)
     :minimum 0.0 :maximum 1.5 :step 0.05)
    *screen-curvature*)
(luvcraft:define-knob scanline-count
    (:group :terminal :quantity (:quantity :scanline-count :unit :one)
     :minimum 60.0 :maximum 1000.0 :step 20.0)
    *scanline-count*)
(luvcraft:define-knob scanline-depth
    (:group :terminal :quantity (:quantity :screen-effect-strength :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.05)
    *scanline-depth*)
(luvcraft:define-knob screen-effect-ceiling
    (:group :terminal :quantity (:quantity :screen-effect-strength :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.05)
    *screen-effect-ceiling*)

;;; The sky's own knobs.  Everything else about the sky arrives through the
;;; frame environment from the day profile; these four are the art direction
;;; the profile has no opinion about.

(defparameter *sky-scatter-gain* 0.70
  "How strongly the atmosphere's phase function brightens the sun's quarter.")
(defparameter *cloud-coverage* 1.35
  "The profile's cloudiness against art direction; one leaves it alone.")
(defparameter *star-brightness* 1.7
  "The radiance of a first-magnitude star, above display white.")
(defparameter *moon-radiance* 4.6
  "The full moon's radiance above display white; the bloom feeds on it.")
(defparameter *cloud-altitude* 80.0
  "The cumulus deck's world height, which both the sky and the ground read.")
(defparameter *cloud-shadow-depth* 0.72
  "How much of the direct sun a cloud takes from the ground beneath it.")

(luvcraft:define-knob sky-scatter-gain
    (:group :sky :quantity (:quantity :scatter-gain :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 3.0 :step 0.05)
    *sky-scatter-gain*)
(luvcraft:define-knob cloud-coverage
    (:group :sky :quantity (:quantity :cloud-coverage :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 2.0 :step 0.05)
    *cloud-coverage*)
(luvcraft:define-knob star-brightness
    (:group :sky :quantity (:quantity :star-brightness :unit :one)
     :minimum 0.0 :maximum 6.0 :step 0.1)
    *star-brightness*)
(luvcraft:define-knob moon-radiance
    (:group :sky :quantity (:quantity :moon-radiance :unit :one)
     :minimum 0.0 :maximum 20.0 :step 0.2)
    *moon-radiance*)
(luvcraft:define-knob cloud-altitude
    (:group :sky :quantity (:quantity :cloud-altitude :unit :one)
     :minimum 60.0 :maximum 900.0 :step 10.0)
    *cloud-altitude*)
(luvcraft:define-knob cloud-shadow-depth
    (:group :sky :label "cloud shadow"
     :quantity (:quantity :cloud-coverage :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.02)
    *cloud-shadow-depth*)

;;; The surface knobs.  A block face is one tile of a sixteen-texel atlas
;;; and a thousand of them are in view at once, so how far apart two faces
;;; of the same material drift, and how the light they return is split
;;; between the sun, the sky, and the ground, are the questions art
;;; direction actually asks about the world's materials.

(defparameter *surface-detail* 0.85
  "How far the procedural weathering moves one face away from the next.")
(defparameter *surface-roughness* 1.35
  "The whole palette's micro-roughness against what its relief implies.")
(defparameter *specular-gain* 1.95
  "How much of the sun's microfacet lobe a block surface returns.")
(defparameter *ambient-bounce* 1.05
  "How much of the ground's own colour a downward face is lit by.")

(luvcraft:define-knob surface-detail
    (:group :surface :quantity (:quantity :surface-detail :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 3.0 :step 0.05)
    *surface-detail*)
(luvcraft:define-knob surface-roughness
    (:group :surface :quantity (:quantity :surface-roughness :unit :one)
     :unit-label "×" :minimum 0.3 :maximum 2.0 :step 0.05)
    *surface-roughness*)
(luvcraft:define-knob specular-gain
    (:group :surface :quantity (:quantity :specular-gain :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 4.0 :step 0.05)
    *specular-gain*)
(luvcraft:define-knob ambient-bounce
    (:group :surface :quantity (:quantity :ambient-bounce :unit :one)
     :minimum 0.0 :maximum 1.5 :step 0.05)
    *ambient-bounce*)

;;; What presentation does to the graded image after the filmic curve: the
;;; two grading controls every colourist reaches for first, and the lens's
;;; own small dispersion.  They are folded literals rather than uniform
;;; lanes because they are art direction, changed by hand and then left.

(defparameter *chromatic-aberration* 0.70
  "How far the lens splits red from blue at the corners, in texels.")
(defparameter *grade-saturation* 1.10
  "Saturation of the presented image, one being the filmic curve's own.")
(defparameter *grade-contrast* 0.16
  "How far the presented image is pulled toward an S-curve.")

(luvcraft:define-knob chromatic-aberration
    (:group :grading :label "aberration"
     :quantity (:quantity :chromatic-aberration :unit :one)
     :minimum 0.0 :maximum 3.0 :step 0.05)
    *chromatic-aberration*)
(luvcraft:define-knob grade-saturation
    (:group :grading :label "saturation"
     :quantity (:quantity :grade-saturation :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 2.0 :step 0.02)
    *grade-saturation*)
(luvcraft:define-knob grade-contrast
    (:group :grading :label "contrast"
     :quantity (:quantity :grade-contrast :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.02)
    *grade-contrast*)

;;; Every stage which reads the frame environment declares the same uniform
;;; block at binding 2: identical member order and offsets are an ABI
;;; requirement, so the member list is written once and spliced at read time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *frame-uniform-members*
    '((camera-vector :vec4      ; camera position, w unused
                     :components
                     ((:xyz :quantity :world-position
                            :unit :cell)))
      (right-vector :vec4
                    :components
                    ((:xyz :quantity :world-direction :unit :one)))
      (up-vector :vec4
                 :components
                 ((:xyz :quantity :world-direction :unit :one)))
      (forward-vector :vec4
                      :components
                      ((:xyz :quantity :world-direction :unit :one)))
      (projection-vector :vec4  ; x scale, y scale, z scale, z offset
                         :components
                         ((:x :quantity :projection-scale :unit :one)
                          (:y :quantity :projection-scale :unit :one)
                          (:z :quantity :projection-scale :unit :one)
                          (:w :quantity :view-distance :unit :cell)))
      (fog-vector :vec4         ; fog near, fog far, elapsed time, cloudiness
                  :components
                  ((:x :quantity :view-distance :unit :cell)
                   (:y :quantity :view-distance :unit :cell)
                   (:z :quantity :sky-time :unit :second)
                   (:w :quantity :cloudiness :unit :one)))
      (sun-vector :vec4         ; sun direction, day factor
                  :components
                  ((:xyz :quantity :world-direction :unit :one)
                   (:w :quantity :day-factor :unit :one)))
      (sun-color-vector :vec4   ; sun colour, angular width
                        :components
                        ((:xyz :quantity :linear-rgb :unit :one)
                         (:w :quantity :sun-disc-coordinate :unit :one)))
      (zenith-vector :vec4      ; zenith colour, w target height in pixels
                     :components
                     ((:xyz :quantity :linear-rgb :unit :one)
                      (:w :quantity :target-pixel-extent :unit :one)))
      (horizon-vector :vec4     ; horizon colour, w target width in pixels
                      :components
                      ((:xyz :quantity :linear-rgb :unit :one)
                       (:w :quantity :target-pixel-extent :unit :one)))
      (ambient-vector :vec4     ; ambient colour, exposure
                      :components
                      ((:xyz :quantity :linear-rgb :unit :one)))
      (fog-color-vector :vec4   ; fog colour, w shadow diagnostic selector
                        :components
                        ((:xyz :quantity :linear-rgb :unit :one)
                         (:w :quantity :shadow-diagnostic :unit :one)))
      (shadow-control-vector :vec4 ; texel u/v, base bias, slope bias
                             :components
                             ((:xy :quantity :shadow-uv :unit :one
                                   :character :difference)
                              (:z :quantity :shadow-depth :unit :one
                                  :character :difference)
                              (:w :quantity :shadow-depth :unit :one
                                  :character :difference)))
      (shadow-filter-vector :vec4 ; depth span, world/texel, min/max radius
                            :components
                            ((:x :quantity :world-distance :unit :cell)
                             (:y :quantity :world-distance :unit :cell)
                             (:z :quantity :shadow-filter-radius :unit :one)
                             (:w :quantity :shadow-filter-radius :unit :one)))
      (atlas-vector :vec4       ; atlas texel width, remaining lanes reserved
                    :components
                    ((:x :quantity :atlas-texel-width :unit :one)))
      (shadow-row-x :vec4)      ; light-space clip x from world position
      (shadow-row-y :vec4)      ; light-space clip y from world position
      (shadow-row-z :vec4)      ; light-space depth from world position
      (shadow-row-w :vec4))     ; light-space homogeneous w
    "The one frame-environment uniform layout shared by all scene stages."))

(define-shader-method shader-specification-for
    block-world-vertex-specification
    ((role (eql :block-surface)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((world-position :vec3 :location 0
                              :quantity :world-position
                              :unit :cell)
              (local-uv-shade-input :vec3 :location 1
                              :components
                              ((:xy :quantity :tile-local-uv :unit :one)
                               (:z :quantity :ambient-occlusion :unit :one)))
              (normal-input :vec3 :location 2
                            :quantity :world-direction :unit :one)
              (light-input :vec3 :location 3
                           :components
                           ((:x :quantity :sky-light-level :unit :one)
                            (:y :quantity :block-light-level :unit :one)
                            (:z :quantity :material-emission :unit :one)))
              (tile-edge-input :vec2 :location 4
                               :components
                               ((:x :quantity :atlas-tile-offset :unit :one)
                                (:y :quantity :packed-edge-shaping :unit :one))))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv-shade-output :vec3 :location 0)
               (normal-output :vec3 :location 1
                              :quantity :world-direction :unit :one)
               (fog-output :float :location 2
                           :quantity :fog-amount :unit :one)
               (light-output :vec3 :location 3
                             :components
                             ((:x :quantity :sky-light-level :unit :one)
                              (:y :quantity :block-light-level :unit :one)
                              (:z :quantity :material-emission :unit :one)))
               (shadow-uv-output :vec2 :location 4
                                 :quantity :shadow-uv
                                 :unit :one)
               (shadow-depth-output :float :location 5
                                    :quantity :shadow-depth
                                    :unit :one)
               ;; The fragment stage shapes its own normal from where the
               ;; fragment sits inside its cell, so it needs the world point
               ;; rather than only the face's flat normal.
               (world-position-output :vec3 :location 6
                                      :quantity :world-position
                                      :unit :cell)
               (edge-shaping-output :vec4 :location 7)
               (tile-offset-output :float :location 8
                                   :quantity :atlas-tile-offset :unit :one
                                   :interpolation :flat))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (swizzle camera-vector :xyz))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (relative (- world-position camera))
         (view-x (dot relative (swizzle right-vector :xyz)))
         (view-y (dot relative (swizzle up-vector :xyz)))
         (view-z (interpret (dot relative (swizzle forward-vector :xyz))
                            :quantity :view-distance :unit :cell))
         (fog-near (swizzle fog-vector :x))
         (fog-far (swizzle fog-vector :y))
         (fog-amount
           (luvcraft.arithmetic:fog-amount-at-view-distance
            view-z fog-near fog-far))
         (x-scale (swizzle projection-vector :x))
         (y-scale (swizzle projection-vector :y))
         (z-scale (swizzle projection-vector :z))
         (z-offset (swizzle projection-vector :w))
         (clip-x (* view-x x-scale))
         (clip-y (- (* view-y y-scale)))
         (clip-z (+ (interpret (* view-z z-scale)
                              :quantity :view-distance :unit :cell)
                    z-offset))
         ;; Homogeneous clip is a heterogeneous GPU representation: XYZ and W
         ;; do not constitute one semantic vector quantity.
         (clip (vec4 (representation clip-x)
                     (representation clip-y)
                     (representation clip-z)
                     (representation view-z)))
         (shadow-projection
           (project-sample
            (project-point :world-to-light world-position
                           shadow-row-x shadow-row-y
                           shadow-row-z shadow-row-w)))
         ;; Projecting either field lowers the shared homogeneous map once.
         ;; Keeping depth first preserves the established instruction order.
         (shadow-depth (swizzle shadow-projection :z))
         (local-uv (representation (swizzle local-uv-shade-input :xy)))
         (tile-offset (representation (swizzle tile-edge-input :x)))
         (atlas-width (representation (swizzle atlas-vector :x)))
         ;; Resolve normalized atlas coordinates at vertex execution time.
         ;; They are recomputed every draw from the renderer-owned width, but
         ;; retain the old interpolation and exact half-texel endpoint values.
         (atlas-uv
           (vec2 (/ (+ tile-offset (swizzle local-uv :x))
                    (/ atlas-width 16.0))
                 (swizzle local-uv :y)))
         (uv-shade
           (vec3 (swizzle atlas-uv :x) (swizzle atlas-uv :y)
                 (representation (swizzle local-uv-shade-input :z))))
         ;; Four -1/0/1 edge classifications occupy one exact base-three
         ;; scalar in the vertex stream.  Unpack before interpolation; each
         ;; face supplies the same code at all of its vertices.
         (packed (representation (swizzle tile-edge-input :y)))
         (u-low-digit (floor (/ packed 27.0)))
         (after-u-low (- packed (* u-low-digit 27.0)))
         (u-high-digit (floor (/ after-u-low 9.0)))
         (after-u-high (- after-u-low (* u-high-digit 9.0)))
         (v-low-digit (floor (/ after-u-high 3.0)))
         (v-high-digit (- after-u-high (* v-low-digit 3.0)))
         (edge-shaping
           (vec4 (- u-low-digit 1.0) (- u-high-digit 1.0)
                 (- v-low-digit 1.0) (- v-high-digit 1.0))))
    (set-output clip-position clip)
    (set-output uv-shade-output uv-shade)
    (set-output normal-output normal-input)
    (set-output fog-output fog-amount)
    (set-output light-output light-input)
    (set-output shadow-uv-output (swizzle shadow-projection :xy))
    (set-output shadow-depth-output shadow-depth)
    (set-output world-position-output world-position)
    (set-output edge-shaping-output edge-shaping)
    (set-output tile-offset-output (swizzle tile-edge-input :x))))

(defun block-world-vertex-specification ()
  (shader-specification-for :block-surface :vertex))

(defun block-world-camera-uniform-block ()
  "The frame uniform block exactly as the vertex specification declares it."
  (find-if (lambda (resource) (typep resource 'shader-uniform-block))
           (shader-specification-resources
            (block-world-vertex-specification))))

(defun block-world-vertex-lowering ()
  "Compile the camera transform and retain expression-to-SSA provenance."
  (spv:compile-shader-specification (block-world-vertex-specification)))

(defun block-world-vertex-module ()
  (spv:shader-lowering-module (block-world-vertex-lowering)))

(defun block-world-vertex-shader ()
  (spv:assemble-spir-v-module (block-world-vertex-module)))

;;; World text is ordinary scene geometry.  Its model transform has already
;;; placed each glyph quad in world coordinates; this stage applies the same
;;; camera view and perspective projection as block surfaces, while preserving
;;; the em-space coordinate and current projected pixel scale Slug consumes.

(define-shader-method shader-specification-for
    block-world-text-vertex-specification
    ((role (eql :slug-world-text)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (world-origin :vec3 :location 1)
              (world-right-edge :vec3 :location 2)
              (world-up-edge :vec3 :location 3)
              (outline-low :vec3 :location 4)
              (outline-high :vec3 :location 5)
              (atlas-input :vec3 :location 6)
              (band-low :vec3 :location 7)
              (band-high :vec3 :location 8))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-atlas-base :vec2 :location 1)
               (render-band-bounds :vec4 :location 2)
               (render-band-counts :vec2 :location 3)
               (render-color :vec4 :location 4))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  ;; Dynamic dilation, after the reference's vertex stage: the quad grows
  ;; past the outline by a fixed number of pixels, however large or small
  ;; the glyph lands on screen, so the pixel filter's half-width is always
  ;; inside it and no more.  A vertex knows its own depth and the frame
  ;; knows the target's height, which together give the pixel length of
  ;; each em edge; the corner then slides outward along both edges by
  ;; the dilation over that length.  Em coordinates slide with it, so the
  ;; outline stays where it was.
  (let* ((camera (swizzle camera-vector :xyz))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         (corner (swizzle quad-corner :xy))
         (undilated-position
           (+ world-origin
              (* world-right-edge (swizzle corner :x))
              (* world-up-edge (swizzle corner :y))))
         (undilated-relative
           (- undilated-position (representation camera)))
         (undilated-depth (max (dot undilated-relative forward) 0.001))
         (pixels-per-unit
           (/ (* (representation (swizzle projection-vector :y))
                 (representation (swizzle zenith-vector :w))
                 0.5)
              undilated-depth))
         (right-edge-pixels
           (* pixels-per-unit
              (sqrt (+ (* (dot world-right-edge right)
                          (dot world-right-edge right))
                       (* (dot world-right-edge up)
                          (dot world-right-edge up))))))
         (up-edge-pixels
           (* pixels-per-unit
              (sqrt (+ (* (dot world-up-edge right)
                          (dot world-up-edge right))
                       (* (dot world-up-edge up)
                          (dot world-up-edge up))))))
         (dilation
           (* luv.slug:slug-dilation-pixels luv.slug:slug-filter-width))
         (dilated-corner
           (+ corner
              (* (- (* corner 2.0) (vec2 1.0 1.0))
                 (vec2 (/ dilation (max right-edge-pixels 0.001))
                       (/ dilation (max up-edge-pixels 0.001))))))
         (world-position
           (assume-quantity
            (+ world-origin
               (* world-right-edge (swizzle dilated-corner :x))
               (* world-up-edge (swizzle dilated-corner :y)))
            :quantity :world-position :unit :cell))
         (outline-coordinate
           (+ (swizzle outline-low :xy)
              (* (- (swizzle outline-high :xy)
                    (swizzle outline-low :xy))
                 dilated-corner)))
         (relative (- world-position camera))
         (view-x (dot relative (swizzle right-vector :xyz)))
         (view-y (dot relative (swizzle up-vector :xyz)))
         (view-z (interpret (dot relative (swizzle forward-vector :xyz))
                            :quantity :view-distance :unit :cell))
         (x-scale (swizzle projection-vector :x))
         (y-scale (swizzle projection-vector :y))
         (z-scale (swizzle projection-vector :z))
         (z-offset (swizzle projection-vector :w))
         (clip-x (* view-x x-scale))
         (clip-y (- (* view-y y-scale)))
         (clip-z (+ (interpret (* view-z z-scale)
                              :quantity :view-distance :unit :cell)
                    z-offset))
         (clip (vec4 (representation clip-x)
                     (representation clip-y)
                     (representation clip-z)
                     (representation view-z))))
    (set-output clip-position clip)
    (set-output render-coordinate outline-coordinate)
    (set-output render-atlas-base (swizzle atlas-input :xy))
    (set-output render-band-bounds
                (vec4 (swizzle band-low :xy)
                      (swizzle band-high :xy)))
    (set-output render-band-counts
                (vec2 (swizzle outline-low :z)
                      (swizzle outline-high :z)))
    ;; The instance record's three spare Z lanes carry the linear ink colour.
    (set-output render-color
                (vec4 (swizzle atlas-input :z)
                      (swizzle band-low :z)
                      (swizzle band-high :z)
                      1.0))))

(defun block-world-text-vertex-specification ()
  (shader-specification-for :slug-world-text :vertex))

;;; Terminal cell backgrounds are analytic world rectangles.  The instance
;;; carries the exact painted rectangle; the vertex stage inflates the quad by
;;; a small margin and hands the fragment stage a rectangle-local coordinate,
;;; where the same signed-distance coverage as the GUI round-rects resolves the
;;; edge against the screen gradient.  They share the text run's frame bind
;;; group and premultiplied blending.

(define-shader-method shader-specification-for
    terminal-cell-vertex-specification
    ((role (eql :terminal-cell)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (world-origin :vec3 :location 1)
              (world-right-edge :vec3 :location 2)
              (world-up-edge :vec3 :location 3)
              (ink-input :vec3 :location 4
                         :quantity :linear-rgb :unit :one))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-color :vec4 :location 2))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((width (sqrt (dot world-right-edge world-right-edge)))
         (height (sqrt (dot world-up-edge world-up-edge)))
         ;; Inflate by a fraction of the row height on every side so the
         ;; antialiased edge has room to fall off outside the exact rectangle.
         (margin (* height 0.08))
         (u (- (* (swizzle quad-corner :x) (+ width (* 2.0 margin))) margin))
         (v (- (* (swizzle quad-corner :y) (+ height (* 2.0 margin))) margin))
         (world-position
           (assume-quantity
            (+ world-origin
               (* world-right-edge (/ u width))
               (* world-up-edge (/ v height)))
            :quantity :world-position :unit :cell))
         (camera (swizzle camera-vector :xyz))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (relative (- world-position camera))
         (view-x (dot relative (swizzle right-vector :xyz)))
         (view-y (dot relative (swizzle up-vector :xyz)))
         (view-z (interpret (dot relative (swizzle forward-vector :xyz))
                            :quantity :view-distance :unit :cell))
         (x-scale (swizzle projection-vector :x))
         (y-scale (swizzle projection-vector :y))
         (z-scale (swizzle projection-vector :z))
         (z-offset (swizzle projection-vector :w))
         (clip-x (* view-x x-scale))
         (clip-y (- (* view-y y-scale)))
         (clip-z (+ (interpret (* view-z z-scale)
                              :quantity :view-distance :unit :cell)
                    z-offset))
         (clip (vec4 (representation clip-x)
                     (representation clip-y)
                     (representation clip-z)
                     (representation view-z))))
    (set-output clip-position clip)
    (set-output render-coordinate
                (vec2 (- u (* width 0.5)) (- v (* height 0.5))))
    (set-output render-half-size-radius
                (vec3 (* width 0.5) (* height 0.5) 0.0))
    (set-output render-color (vec4 (representation ink-input) 1.0))))

(define-shader-method shader-specification-for
    terminal-cell-fragment-specification
    ((role (eql :terminal-cell)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size-radius :vec3 :location 1)
              (color :vec4 :location 2))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((coverage
           (luv.analytic:roundrect-coverage render-coordinate half-size-radius)))
    (set-output color-output (* color coverage))))

;;; The terminal is a tube behind glass, drawn as three coplanar rectangles.
;;; This vertex stage serves the two outer ones: the phosphor plate beneath
;;; the cell backgrounds and glyphs, and the faceplate glass above them.  Both
;;; want the same rectangle-local coordinate, the same world position for
;;; view-dependent shading, and the surface's own frame, so the stage hands
;;; down the normalized right and up edges alongside the outward normal.  The
;;; instance layout stays the cell run's, with the fourth vector carrying the
;;; normal, so one vertex buffer description serves every terminal rectangle.

(define-shader-method shader-specification-for
    terminal-screen-vertex-specification
    ((role (eql :terminal-screen)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (world-origin :vec3 :location 1)
              (world-right-edge :vec3 :location 2)
              (world-up-edge :vec3 :location 3)
              (normal-input :vec3 :location 4))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size :vec2 :location 1)
               (render-world-position :vec3 :location 2)
               (render-normal :vec3 :location 3)
               (render-right :vec3 :location 4)
               (render-up :vec3 :location 5))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((width (sqrt (dot world-right-edge world-right-edge)))
         (height (sqrt (dot world-up-edge world-up-edge)))
         (u (* (swizzle quad-corner :x) width))
         (v (* (swizzle quad-corner :y) height))
         (world-position
           (assume-quantity
            (+ world-origin
               (* world-right-edge (swizzle quad-corner :x))
               (* world-up-edge (swizzle quad-corner :y)))
            :quantity :world-position :unit :cell))
         (camera (swizzle camera-vector :xyz))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (relative (- world-position camera))
         (view-x (dot relative (swizzle right-vector :xyz)))
         (view-y (dot relative (swizzle up-vector :xyz)))
         (view-z (interpret (dot relative (swizzle forward-vector :xyz))
                            :quantity :view-distance :unit :cell))
         (x-scale (swizzle projection-vector :x))
         (y-scale (swizzle projection-vector :y))
         (z-scale (swizzle projection-vector :z))
         (z-offset (swizzle projection-vector :w))
         (clip-x (* view-x x-scale))
         (clip-y (- (* view-y y-scale)))
         (clip-z (+ (interpret (* view-z z-scale)
                              :quantity :view-distance :unit :cell)
                    z-offset))
         (clip (vec4 (representation clip-x)
                     (representation clip-y)
                     (representation clip-z)
                     (representation view-z))))
    (set-output clip-position clip)
    (set-output render-coordinate
                (vec2 (- u (* width 0.5)) (- v (* height 0.5))))
    (set-output render-half-size (vec2 (* width 0.5) (* height 0.5)))
    (set-output render-world-position (representation world-position))
    (set-output render-normal normal-input)
    (set-output render-right (* world-right-edge (/ 1.0 width)))
    (set-output render-up (* world-up-edge (/ 1.0 height)))))

;;; Where the tube's active area stops and its bezel begins is a fact about
;;; the panel, not about either material, so both stages ask the same two
;;; questions instead of repeating a literal that could drift apart.

(define-shader-function terminal-panel-edge-distance (coordinate half-size)
  "World-cell distance from a panel-local point to the nearest panel edge."
  (min (- (swizzle half-size :x) (abs (swizzle coordinate :x)))
       (- (swizzle half-size :y) (abs (swizzle coordinate :y)))))

(define-shader-function terminal-screen-coverage (edge-distance)
  "One inside the tube's active area, zero on the bezel, resolved between."
  (smoothstep 0.11 0.122 edge-distance))

;;; The phosphor plate is the back of the tube: what the glyphs are painted
;;; on, and what remains when the terminal is blank.  It is deliberately not
;;; the glass -- reflections belong on the faceplate above the text, where
;;; they can actually veil it -- so this stage only answers what the tube
;;; emits and how its recess and bezel catch the daylight of the world it
;;; hangs in.

(define-shader-method shader-specification-for
    terminal-screen-fragment-specification
    ((role (eql :terminal-screen)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size :vec2 :location 1)
              (world-position :vec3 :location 2)
              (normal-input :vec3 :location 3)
              (right-input :vec3 :location 4)
              (up-input :vec3 :location 5))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (day-factor (representation (swizzle sun-vector :w)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (normal (normalize normal-input))
         (view (normalize (- camera world-position)))
         (n-dot-l (max 0.0 (dot normal sun-direction)))
         (irradiance (+ ambient (* sun-color (* n-dot-l day-factor))))
         (edge-distance
           (terminal-panel-edge-distance render-coordinate half-size))
         (screen-mask (terminal-screen-coverage edge-distance))
         ;; Panel-local coordinates normalized to the corners, so the tube's
         ;; falloff is a property of the picture rather than of its size.
         (u (/ (swizzle render-coordinate :x) (swizzle half-size :x)))
         (v (/ (swizzle render-coordinate :y) (swizzle half-size :y)))
         (radius-squared (+ (* u u) (* v v)))
         ;; The screen sits behind the bezel, so shade falls in from the lip,
         ;; and a tube's own glass is brightest through the middle.
         (inset-shade (smoothstep 0.0 0.5 (- edge-distance 0.11)))
         (centre-weight (mix 1.0 0.55 (clamp (* radius-squared 0.7) 0.0 1.0)))
         (vignette (* (mix 0.26 1.0 inset-shade) centre-weight))
         (glass-albedo (vec3 0.009 0.009 0.012))
         ;; A faint idling phosphor: warm enough to read as a lit tube rather
         ;; than a hole, dim enough to stay black next to any real glyph.
         (phosphor (vec3 0.0090 0.0112 0.0104))
         (screen-color (* (+ (* glass-albedo irradiance) phosphor) vignette))
         ;; The bezel is matte dark plastic: a lit inner lip where it turns
         ;; toward the screen, and a slightly brighter outer chamfer.
         (lip (smoothstep 0.08 0.11 edge-distance))
         (chamfer (smoothstep 0.05 0.0 edge-distance))
         (bezel-albedo
           (mix (mix (vec3 0.021 0.020 0.022) (vec3 0.088 0.088 0.096) lip)
                (vec3 0.042 0.040 0.041) chamfer))
         (half-vector (normalize (+ sun-direction view)))
         (n-dot-h (max 0.0 (dot normal half-vector)))
         (bezel-specular
           (* sun-color (* (expt n-dot-h 42.0) 0.09 day-factor)))
         (bezel-color (+ (* bezel-albedo irradiance) bezel-specular))
         (rgb (mix bezel-color screen-color screen-mask)))
    (set-output color-output (vec4 rgb 1.0))))

;;; The faceplate is the glass in front of the picture, drawn last so that
;;; everything it does happens *to* the text rather than behind it.  It is the
;;; whole analogue argument of the terminal, and every term in it is one
;;; physical claim about a tube seen through coated glass:
;;;
;;;   - the raster is a fixed line count belonging to the panel, so it holds
;;;     still in the world instead of crawling with the camera, and it fades
;;;     out before it can alias rather than shimmering at a distance;
;;;   - the glass is very slightly convex, expressed only as a bent normal,
;;;     so the room wraps around the rim while every glyph stays exactly
;;;     where the glyph pass drew it -- curvature as light, not as geometry;
;;;   - mains hum and a little grain keep the picture from being perfectly
;;;     still, at amplitudes chosen to be felt rather than seen.
;;;
;;; Premultiplied alpha makes one draw do both halves of that: the alpha lane
;;; attenuates the picture underneath (raster, corners, hum, grain) and the
;;; colour lane adds what the glass itself sends back (sky, sun, room).

(define-shader-method shader-specification-for
    terminal-faceplate-fragment-specification
    ((role (eql :terminal-faceplate)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size :vec2 :location 1)
              (world-position :vec3 :location 2)
              (normal-input :vec3 :location 3)
              (right-input :vec3 :location 4)
              (up-input :vec3 :location 5))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (day-factor (representation (swizzle sun-vector :w)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (zenith (representation (swizzle zenith-vector :xyz)))
         (horizon (representation (swizzle horizon-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (elapsed (representation (swizzle fog-vector :z)))
         (flat-normal (normalize normal-input))
         (right (normalize right-input))
         (up (normalize up-input))
         (view (normalize (- camera world-position)))
         (half-x (swizzle half-size :x))
         (half-y (swizzle half-size :y))
         (u (/ (swizzle render-coordinate :x) half-x))
         (v (/ (swizzle render-coordinate :y) half-y))
         ;; A cubic bulge leaves the middle of the plate flat and turns only
         ;; the last part of the way to the rim, which is what a real face
         ;; does and what keeps the reflection from sliding about.
         (bulge screen-curvature)
         (normal
           (normalize
            (+ flat-normal
               (+ (* right (* bulge (* u (* u u))))
                  (* up (* bulge (* v (* v v))))))))
         (n-dot-v (max 0.0 (dot normal view)))
         ;; Schlick Fresnel for coated glass: a low floor rising toward
         ;; grazing, so a frontal view stays dark and an oblique one lights up.
         (fresnel (+ 0.021 (* 0.979 (expt (- 1.0 n-dot-v) 5.0))))
         ;; The mirrored view ray samples the same two-colour sky gradient the
         ;; sky dome draws, so the glass reflects the day it stands in.
         (reflected (- (* normal (* 2.0 (dot normal view))) view))
         (sky-height (smoothstep -0.06 0.5 (swizzle reflected :y)))
         (sky-reflection (* (mix horizon zenith sky-height) (* fresnel 0.72)))
         (room-reflection (* ambient (* fresnel 0.55)))
         ;; A tight glint plus a broad sheen; the bent normal drags both of
         ;; them into a curved streak near the rim.
         (half-vector (normalize (+ sun-direction view)))
         (n-dot-h (max 0.0 (dot normal half-vector)))
         (glint (* sun-color (* (expt n-dot-h 200.0) 2.4 day-factor)))
         (sheen (* sun-color (* (expt n-dot-h 26.0) 0.10 day-factor)))
         (edge-distance
           (terminal-panel-edge-distance render-coordinate half-size))
         (screen-mask (terminal-screen-coverage edge-distance))
         ;; The raster: a fixed number of lines across this panel's height,
         ;; exactly as a tube has a fixed line count whatever its diagonal.
         (line-pitch (/ (* 2.0 half-y) scanline-count))
         (vertical-dx (derivative-x (swizzle render-coordinate :y)))
         (vertical-dy (derivative-y (swizzle render-coordinate :y)))
         (cells-per-pixel
           (max (sqrt (+ (* vertical-dx vertical-dx)
                         (* vertical-dy vertical-dy)))
                1e-6))
         (pixels-per-line (/ line-pitch cells-per-pixel))
         ;; Below two pixels a line cannot be drawn, only aliased, so the
         ;; raster leaves rather than turning into moire at a distance.
         (raster-fade (smoothstep 1.8 3.6 pixels-per-line))
         (line-phase (/ (swizzle render-coordinate :y) line-pitch))
         (line-profile (+ 0.5 (* 0.5 (cos (* 6.2831855 line-phase)))))
         (scanline (* scanline-depth raster-fade (- 1.0 line-profile)))
         ;; Attenuating the picture is only half of a raster.  A tonemapped
         ;; frame puts bright text on the shoulder of the filmic curve, where
         ;; a multiply barely moves it, and puts a dark terminal's background
         ;; near black, where a multiply moves it not at all.  So the tube's
         ;; own idle glow below carries the beam instead: energy-preserving
         ;; about one, so the raster shapes that glow without lighting it.
         (beam (mix 1.0 (+ 0.55 (* 0.9 line-profile)) raster-fade))
         ;; A slow bar drifting up the picture and a fast shimmer on top of
         ;; it: the two ways a tube betrays the mains it is plugged into.
         (hum-phase (fract (- (* v 0.5) (* elapsed 0.052))))
         (hum-band (expt (+ 0.5 (* 0.5 (cos (* 6.2831855 hum-phase)))) 4.0))
         (hum (* 0.020 hum-band))
         (flicker (* 0.008 (+ 0.5 (* 0.5 (sin (* elapsed 47.3))))))
         ;; Grain is resampled a couple of dozen times a second rather than
         ;; every frame, so it reads as emulsion instead of as a frame rate.
         (grain
           (lattice-hash
            (vec3 (* (swizzle render-coordinate :x) 617.0)
                  (* (swizzle render-coordinate :y) 613.0)
                  (floor (* elapsed 26.0)))))
         (speckle (* 0.018 grain))
         ;; The corners of a tube are always darker than its middle, and this
         ;; is the one such falloff that also reaches the text.
         (corner-shade (smoothstep 0.55 1.35 (+ (* u u) (* v v))))
         (corner (* 0.16 corner-shade))
         ;; Glass scatters a little of the room forward instead of reflecting
         ;; all of it, and a tube idles with a little phosphor of its own.
         ;; Together they lift the black off the floor -- the difference
         ;; between a screen and a hole in the wall -- and because the corner
         ;; falloff multiplies them, this is where the shape of the tube
         ;; actually becomes visible on an otherwise dark terminal.
         (haze
           (* (+ (vec3 0.0072 0.0084 0.0104) (* ambient 0.045))
              (* beam (mix 1.0 0.30 corner-shade))))
         (attenuation
           (clamp (+ scanline corner hum flicker speckle) 0.0 screen-effect-ceiling))
         (emission (+ sky-reflection room-reflection glint sheen haze)))
    (set-output color-output
                (vec4 (* emission screen-mask) (* attenuation screen-mask)))))

;;; The phone's screen is not a tube.  It is a sheet of flat glass over a
;;; dark panel, and everything the wall's faceplate argues about -- raster,
;;; hum, grain, the tube's convex bulge and its darker corners -- is exactly
;;; what a retina slab does not have.  What it has instead is a rounded
;;; edge, a Fresnel reflection of the sky it is held under, one tight glint,
;;; and, because someone has been holding it, fingerprints: patches of oil
;;; that scatter a little of the room forward where clean glass would not.
;;; Both stages below share the wall's vertex stage; only the material is
;;; the phone's own.

(define-shader-function phone-glass-distance (coordinate half-size radius)
  "Signed distance from a panel-local point to the rounded glass edge,
negative inside.  RADIUS is the corner radius in world cells."
  (let* ((qx (max (- (abs (swizzle coordinate :x))
                     (- (swizzle half-size :x) radius))
                  0.0))
         (qy (max (- (abs (swizzle coordinate :y))
                     (- (swizzle half-size :y) radius))
                  0.0)))
    (- (sqrt (+ (* qx qx) (* qy qy))) radius)))

(define-shader-function phone-glass-coverage (coordinate half-size radius)
  "One on the glass, zero past its rounded edge, resolved over one pixel."
  (let* ((distance (phone-glass-distance coordinate half-size radius))
         (dx (derivative-x (swizzle coordinate :x)))
         (dy (derivative-y (swizzle coordinate :x)))
         (pixel (max (sqrt (+ (* dx dx) (* dy dy))) 1e-5)))
    (- 1.0 (smoothstep (- pixel) pixel distance))))

(define-shader-method shader-specification-for
    phone-screen-fragment-specification
    ((role (eql :phone-screen)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size :vec2 :location 1)
              (world-position :vec3 :location 2)
              (normal-input :vec3 :location 3)
              (right-input :vec3 :location 4)
              (up-input :vec3 :location 5))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((sun-direction (representation (swizzle sun-vector :xyz)))
         (day-factor (representation (swizzle sun-vector :w)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (normal (normalize normal-input))
         (n-dot-l (max 0.0 (dot normal sun-direction)))
         (irradiance (+ ambient (* sun-color (* n-dot-l day-factor))))
         (coverage (phone-glass-coverage render-coordinate half-size 0.02))
         ;; The panel behind the glass: black, and lit only by what little
         ;; the room puts into it.  Off, it should read as a dark mirror.
         (panel-albedo (vec3 0.005 0.005 0.006))
         (rgb (* panel-albedo irradiance)))
    (set-output color-output (vec4 (* rgb coverage) coverage))))

(define-shader-method shader-specification-for
    phone-glass-fragment-specification
    ((role (eql :phone-glass)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size :vec2 :location 1)
              (world-position :vec3 :location 2)
              (normal-input :vec3 :location 3)
              (right-input :vec3 :location 4)
              (up-input :vec3 :location 5))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (day-factor (representation (swizzle sun-vector :w)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (zenith (representation (swizzle zenith-vector :xyz)))
         (horizon (representation (swizzle horizon-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (normal (normalize normal-input))
         (view (normalize (- camera world-position)))
         (n-dot-v (max 0.0 (dot normal view)))
         ;; Coated glass: dark seen straight on, a mirror seen at a slant.
         (fresnel (+ 0.018 (* 0.982 (expt (- 1.0 n-dot-v) 5.0))))
         (reflected (- (* normal (* 2.0 (dot normal view))) view))
         (sky-height (smoothstep -0.06 0.5 (swizzle reflected :y)))
         (sky-reflection (* (mix horizon zenith sky-height) (* fresnel 0.8)))
         (room-reflection (* ambient (* fresnel 0.5)))
         (half-vector (normalize (+ sun-direction view)))
         (n-dot-h (max 0.0 (dot normal half-vector)))
         (glint (* sun-color (* (expt n-dot-h 320.0) 2.0 day-factor)))
         ;; Fingerprints.  Oil on the glass scatters light forward over a
         ;; broad lobe where the clean coating would send it away, so the
         ;; smudges only appear where there is light to scatter -- near
         ;; the sun's sheen and at a slant to the sky -- and never as paint.
         (u (swizzle render-coordinate :x))
         (v (swizzle render-coordinate :y))
         (smudge-field
           (lattice-fractal-noise
            (vec3 (* u 38.0) (* v 27.0) 3.7)))
         (smudge (* (smoothstep 0.56 0.74 smudge-field) 0.22))
         (sheen (* sun-color (* (expt n-dot-h 14.0) 0.05 day-factor)))
         (scatter (* (+ sheen (* (+ ambient (* horizon 0.4)) 0.035))
                     smudge))
         (coverage (phone-glass-coverage render-coordinate half-size 0.02))
         (emission (+ sky-reflection room-reflection glint scatter)))
    ;; The glass takes nothing away from the picture under it: alpha zero,
    ;; and only what it sends back is added.
    (set-output color-output (vec4 (* emission coverage) 0.0))))

;;; A video screen is the plainest world rectangle luvcraft draws: the same
;;; instance record as a terminal cell, but the fragment stage reads a decoded
;;; picture instead of computing a material.  The picture is the only thing on
;;; it, so there is no shading model here at all -- the texture is sRGB and the
;;; hardware decode hands the scene target the linear radiance it wants.
;;;
;;; The one non-obvious line is the flipped V.  A quad corner runs from zero at
;;; the bottom to one at the top, and a decoded frame's first row is its top,
;;; so a screen that did not flip would play the film upside down.

(define-shader-method shader-specification-for
    video-screen-vertex-specification
    ((role (eql :video-screen)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (world-origin :vec3 :location 1)
              (world-right-edge :vec3 :location 2)
              (world-up-edge :vec3 :location 3))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-uv :vec2 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((corner-x (swizzle quad-corner :x))
         (corner-y (swizzle quad-corner :y))
         (world-position
           (assume-quantity
            (+ world-origin
               (* world-right-edge corner-x)
               (* world-up-edge corner-y))
            :quantity :world-position :unit :cell))
         (camera (swizzle camera-vector :xyz))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (relative (- world-position camera))
         (view-x (dot relative (swizzle right-vector :xyz)))
         (view-y (dot relative (swizzle up-vector :xyz)))
         (view-z (interpret (dot relative (swizzle forward-vector :xyz))
                            :quantity :view-distance :unit :cell))
         (x-scale (swizzle projection-vector :x))
         (y-scale (swizzle projection-vector :y))
         (z-scale (swizzle projection-vector :z))
         (z-offset (swizzle projection-vector :w))
         (clip-x (* view-x x-scale))
         (clip-y (- (* view-y y-scale)))
         (clip-z (+ (interpret (* view-z z-scale)
                               :quantity :view-distance :unit :cell)
                    z-offset))
         (clip (vec4 (representation clip-x)
                     (representation clip-y)
                     (representation clip-z)
                     (representation view-z))))
    (set-output clip-position clip)
    (set-output render-uv (vec2 corner-x (- 1.0 corner-y)))))

(define-shader-method shader-specification-for
    video-screen-fragment-specification
    ((role (eql :video-screen)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0
                        :quantity :texture-uv :unit :one))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((video-picture :texture-2d :set 0 :binding 0
                     :sample-transfer :srgb-to-linear
                     :sample-components
                     ((:rgb :quantity :linear-rgb :unit :one)))
      (video-sampler :sampler :set 0 :binding 1)
      (frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  ;; The scene pass writes raw radiance: exposure and the filmic curve belong
  ;; to the post chain, so a screen that emits its own picture writes the
  ;; decoded picture and stops there.
  (let* ((picture (sample video-picture video-sampler uv-input))
         (radiance (swizzle picture :rgb)))
    (set-output color-output (vec4 (representation radiance) 1.0))))

;;; A portal is another game's picture arriving on a wall, and it should read
;;; as exactly that: a transmission, not a window and not a movie.  The panel
;;; is the whole terminal face; the picture sits inside it on a dark matte
;;; with the same margin the text grid keeps, so the wall's proportions do
;;; not change when the shell becomes a portal.  What marks the picture as
;;; coming from elsewhere is done to its signal, not to the glass (the
;;; faceplate above still supplies raster, hum, and reflections as it does
;;; for text):
;;;
;;;   - the colour channels do not quite converge -- a radial chromatic
;;;     fringe, nothing at the centre and a few texels at the corners;
;;;   - alternate lines are displaced by a texel or so on a slow beat, a
;;;     field interlace that never fully locks;
;;;   - a soft sync bar climbs the picture, brighter than the hum bar the
;;;     glass adds, so the two are visibly different mechanisms;
;;;   - the picture warms up over half a second when the link opens instead
;;;     of appearing, and its edge is a thin phosphor line on the matte.
;;;
;;; The uniform carries the picture rectangle in panel UV and the moment the
;;; link opened; everything else is the frame environment's clock.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *portal-uniform-members*
    '((picture-rect :vec4)      ; picture u0, v0, u1, v1 within the panel
      (portal-control :vec4))   ; opened-at seconds, texel u, texel v, unused
    "The uniform layout of one portal panel."))

(define-shader-method shader-specification-for
    portal-screen-fragment-specification
    ((role (eql :portal-screen)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0
                        :quantity :texture-uv :unit :one))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((portal-picture :texture-2d :set 0 :binding 0
                      :sample-transfer :srgb-to-linear
                      :sample-components
                      ((:rgb :quantity :linear-rgb :unit :one)))
      (portal-sampler :sampler :set 0 :binding 1)
      (frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)
      (portal-state :uniform-block :set 0 :binding 3
                    :members #.*portal-uniform-members*)))
  (let* ((elapsed (representation (swizzle fog-vector :z)))
         (opened (swizzle portal-control :x))
         (texel (swizzle portal-control :yz))
         (rect-min (swizzle picture-rect :xy))
         (rect-max (swizzle picture-rect :zw))
         (rect-size (- rect-max rect-min))
         ;; Panel UV to picture UV; the picture is where both lie in [0,1].
         (uv (representation uv-input))
         (picture-uv (/ (- uv rect-min) rect-size))
         (px (swizzle picture-uv :x))
         (py (swizzle picture-uv :y))
         (inside-x (* (step 0.0 px) (step px 1.0)))
         (inside-y (* (step 0.0 py) (step py 1.0)))
         (inside (* inside-x inside-y))
         ;; Warm-up: half a second from black to picture.
         (age (- elapsed opened))
         (warmth (smoothstep 0.0 0.55 age))
         ;; Field interlace: odd picture lines drift a texel sideways on a
         ;; slow beat, even lines the other way, never quite locking.
         (line (floor (/ py (* 2.0 (swizzle texel :y)))))
         (field (- (* 2.0 (fract (* line 0.5))) 0.5))
         (weave (* field (* 2.5 (swizzle texel :x))
                   (sin (+ (* elapsed 1.7) (* py 9.0)))))
         ;; The odd field is a shade dimmer than the even one, so the
         ;; interlace shows as texture where the weave alone would not.
         (field-shade (- 1.0 (* 0.10 (+ 0.5 field))))
         (woven (+ picture-uv (vec2 weave 0.0)))
         ;; Chromatic fringe: red and blue pulled apart radially, growing
         ;; with the square of the distance from the centre.
         (centered (- woven (vec2 0.5 0.5)))
         (radius2 (dot centered centered))
         (fringe (* centered (* radius2 (* 22.0 (swizzle texel :x)))))
         (red (swizzle (representation
                        (sample portal-picture portal-sampler
                                (assume-quantity (+ woven fringe)
                                                 :quantity :texture-uv :unit :one)))
                       :x))
         (green (swizzle (representation
                          (sample portal-picture portal-sampler
                                  (assume-quantity woven
                                                   :quantity :texture-uv :unit :one)))
                         :y))
         (blue (swizzle (representation
                         (sample portal-picture portal-sampler
                                 (assume-quantity (- woven fringe)
                                                  :quantity :texture-uv :unit :one)))
                        :z))
         (picture (vec3 red green blue))
         ;; The sync bar: a soft band climbing the picture every few seconds.
         (bar-phase (fract (- (* py 0.7) (* elapsed 0.11))))
         (bar (expt (+ 0.5 (* 0.5 (cos (* 6.2831855 bar-phase)))) 6.0))
         (lifted (* picture (* field-shade (+ 1.0 (* 0.16 bar)))))
         ;; A little vignette of the picture's own, inside the frame.
         (vignette (- 1.0 (* 0.22 (smoothstep 0.30 0.95 radius2))))
         (signal (* lifted (* vignette warmth)))
         ;; The matte and its phosphor edge.  Distance to the picture edge in
         ;; panel UV; the edge line is a couple of texels wide and glows a
         ;; little further into the matte.
         (edge-dx (max (- (swizzle rect-min :x) (swizzle uv :x))
                       (- (swizzle uv :x) (swizzle rect-max :x))))
         (edge-dy (max (- (swizzle rect-min :y) (swizzle uv :y))
                       (- (swizzle uv :y) (swizzle rect-max :y))))
         (outside-distance (max 0.0 (max edge-dx edge-dy)))
         (line-width (* 2.5 (swizzle texel :x) (swizzle rect-size :x)))
         (edge-line (- 1.0 (smoothstep 0.0 line-width outside-distance)))
         (edge-glow (- 1.0 (smoothstep 0.0 (* 12.0 line-width) outside-distance)))
         (phosphor (vec3 0.16 0.62 0.55))
         (matte (vec3 0.0075 0.0080 0.0100))
         (matte-color
           (+ matte
              (* phosphor (* warmth (+ (* 0.55 edge-line) (* 0.06 edge-glow))))))
         (rgb (mix matte-color signal inside)))
    (set-output color-output (vec4 rgb 1.0))))

(define-shader-method shader-specification-for
    video-screen-hardware-fragment-specification
    ((role (eql :video-screen-hardware)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0
                        :quantity :texture-uv :unit :one))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((video-luma :texture-2d :set 0 :binding 0)
      (video-sampler :sampler :set 0 :binding 1)
      (frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)
      (video-chroma :texture-2d :set 0 :binding 3)))
  ;; VideoToolbox's ordinary bi-planar output is video-range NV12.  Sampling
  ;; the two CVMetalTextures performs no copy; this matrix is the only colour
  ;; conversion between the decoder surface and the HDR scene target.
  (let* ((y (- (swizzle (representation
                          (sample video-luma video-sampler uv-input)) :x)
               (/ 16.0 255.0)))
         (uv (- (swizzle (representation
                           (sample video-chroma video-sampler uv-input)) :xy)
                (vec2 0.5 0.5)))
         (u (swizzle uv :x))
         (v (swizzle uv :y))
         (r (+ (* 1.164383 y) (* 1.792741 v)))
         (g (- (* 1.164383 y) (* 0.213249 u) (* 0.532909 v)))
         (b (+ (* 1.164383 y) (* 2.112402 u))))
    (set-output color-output (vec4 r g b 1.0))))

(defmethod shader-specification-for
    ((role (eql :slug-world-text)) (stage (eql :fragment)))
  (declare (ignore role stage))
  (luv.slug:slug-atlas-fragment-specification))

(defun block-world-text-fragment-specification ()
  (shader-specification-for :slug-world-text :fragment))

(define-shader-method shader-specification-for
    block-world-fragment-specification
    ((role (eql :block-surface)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs
     ((uv-shade-input :vec3 :location 0
                      :components
                      ((:xy :quantity :texture-uv :unit :one)
                       (:z :quantity :ambient-occlusion :unit :one)))
      (normal-input :vec3 :location 1
                    :quantity :world-direction :unit :one)
      (fog-input :float :location 2 :quantity :fog-amount :unit :one)
      (light-input :vec3 :location 3
                   :components
                   ((:x :quantity :sky-light-level :unit :one)
                    (:y :quantity :block-light-level :unit :one)
                    (:z :quantity :material-emission :unit :one)))
      (shadow-uv-input :vec2 :location 4
                       :quantity :shadow-uv :unit :one)
      (shadow-depth-input :float :location 5
                          :quantity :shadow-depth :unit :one)
      (world-position-input :vec3 :location 6
                            :quantity :world-position :unit :cell)
      (edge-shaping-input :vec4 :location 7)
      (tile-offset-input :float :location 8
                         :quantity :atlas-tile-offset :unit :one
                         :interpolation :flat))
     :outputs ((color-output :vec4 :location 0))
     :resources ((block-atlas :texture-2d :set 0 :binding 0
                              :sample-transfer :srgb-to-linear
                              :sample-components
                              ((:rgb :quantity :linear-rgb :unit :one)))
                 (block-sampler :sampler :set 0 :binding 1)
                 (frame-state :uniform-block :set 0 :binding 2
                              :members #.*frame-uniform-members*)
                 (shadow-map :depth-texture-2d :set 0 :binding 3
                             :sample-components
                             ((:x :quantity :shadow-depth :unit :one)))
                 (shadow-sampler :sampler :set 0 :binding 4)
                 (shadow-comparison-sampler :sampler :set 0 :binding 5)
                 (block-normal-atlas :texture-2d :set 0 :binding 6
                                     :sample-transfer :identity
                                     :sample-components
                                     ((:rgb :quantity
                                            :surface-normal-sample
                                            :unit :one)
                                      (:a :quantity :surface-relief
                                          :unit :one)))))
  (let* ((uv-shade uv-shade-input)
         (uv (swizzle uv-shade :xy))
         (ao (swizzle uv-shade :z))
         (normal normal-input)
         ;; --- the shaped surface -------------------------------------------
         ;; A block face is flat geometry, but it should not read as a flat
         ;; material.  Two shapings ride on the same face: a bevel that rounds
         ;; the last sixth of a cell toward its edge, and a per-texel relief
         ;; read straight out of the atlas the material is already painted in.
         ;; Both are pure normal perturbation, so no mesh changes.
         (flat-normal (representation normal-input))
         (surface-point (representation world-position-input))
         (eye (representation (swizzle camera-vector :xyz)))
         (cell (fract surface-point))
         (axis-x (abs (swizzle flat-normal :x)))
         (axis-y (abs (swizzle flat-normal :y)))
         ;; A block face's own normal selects its two in-plane world axes, so
         ;; one expression serves all six faces without a branch.
         (tangent-u (vec3 (- 1.0 axis-x) 0.0 axis-x))
         (tangent-v (vec3 0.0 (- 1.0 axis-y) axis-y))
         (plane-u (mix (swizzle cell :x) (swizzle cell :z) axis-x))
         (plane-v (mix (swizzle cell :y) (swizzle cell :z) axis-y))
         ;; The atlas coordinate and its screen derivative size both shapings.
         ;; The normal itself is already the central difference of the same
         ;; procedural relief field that paints the colour atlas.
         (atlas-u (representation (swizzle uv :x)))
         (atlas-v (representation (swizzle uv :y)))
         (tile-count 11.0)
         (tile-texels 16.0)
         ;; Texels per pixel: one cell is TILE-TEXELS of them.
         (footprint
           (max (* (abs (derivative-x atlas-u)) (* tile-count tile-texels))
                (* (abs (derivative-y atlas-v)) tile-texels)))
         ;; Both shapings are sub-block detail, so both have to fade out as
         ;; soon as they stop resolving on screen; otherwise distant terrain
         ;; sparkles and shows a lit grid instead of a surface.  One measured
         ;; footprint drives both, at their own scales.
         (relief-fade (- 1.0 (smoothstep 0.30 1.10 footprint)))
         (bevel-fade (- 1.0 (smoothstep 1.60 5.00 footprint)))
         ;; The mesher's edge classification decides what each boundary does.
         ;; A convex edge rounds the surface over; a concave one fillets it
         ;; into the inner corner; a flush one -- the middle of an open plain,
         ;; where the face simply continues into an identically oriented
         ;; neighbour -- leaves the surface alone.  Signs come straight from
         ;; the vertex lane, so the same ramp expression serves all three.
         (edge edge-shaping-input)
         (edge-u-low (swizzle edge :x))
         (edge-u-high (swizzle edge :y))
         (edge-v-low (swizzle edge :z))
         (edge-v-high (swizzle edge :w))
         ;; A round-over is only legible if it is a few pixels wide.  Held to
         ;; a fixed fraction of a cell it dwindles to nothing a short way off,
         ;; and every block in the middle distance flattens back into a card;
         ;; grown with the footprint it keeps its shape until it is genuinely
         ;; too small to resolve.  The upper bound stops it from eating the
         ;; face it is supposed to be an edge of.
         (bevel-width (clamp (* 0.115 footprint) 0.105 0.185))
         (ramp-u-low (smoothstep bevel-width 0.0 plane-u))
         (ramp-u-high (smoothstep (- 1.0 bevel-width) 1.0 plane-u))
         (ramp-v-low (smoothstep bevel-width 0.0 plane-v))
         (ramp-v-high (smoothstep (- 1.0 bevel-width) 1.0 plane-v))
         (bevel-u (- (* ramp-u-high edge-u-high) (* ramp-u-low edge-u-low)))
         (bevel-v (- (* ramp-v-high edge-v-high) (* ramp-v-low edge-v-low)))
         (bevel-lean (+ (* tangent-u bevel-u) (* tangent-v bevel-v)))
         ;; An inner corner is a crevice, and a crevice gathers occlusion.
         ;; Only the filleted edges contribute; a rounded-over outer edge is
         ;; more exposed than the face it belongs to, not less.
         (crease
           (+ (* ramp-u-low (max 0.0 (- edge-u-low)))
              (* ramp-u-high (max 0.0 (- edge-u-high)))
              (* ramp-v-low (max 0.0 (- edge-v-low)))
              (* ramp-v-high (max 0.0 (- edge-v-high)))))
         (seam (clamp crease 0.0 1.0))
         ;; One linear texture read replaces the four height taps formerly
         ;; used here.  The CPU generator clamps those taps within each tile,
         ;; normalizes the result, and stores the source height in alpha for
         ;; inspection.  Fade the decoded tangent normal back toward the flat
         ;; face as its texels become sub-pixel.
         (normal-sample
           (representation
            (swizzle (sample block-normal-atlas block-sampler uv) :rgb)))
         (tangent-normal (- (* normal-sample 2.0) (vec3 1.0 1.0 1.0)))
         (relief-x (* (swizzle tangent-normal :x) relief-fade))
         (relief-y (* (swizzle tangent-normal :y) relief-fade))
         (relief-z (mix 1.0 (swizzle tangent-normal :z) relief-fade))
         ;; A real round-over is an arc, not a leaning plane.  Adding a scaled
         ;; tangent to the face normal can only ever reach the arctangent of
         ;; that scale, so a strong edge needs an implausible scale and still
         ;; bunches its shading into the last sliver.  Rotating the normal
         ;; through an angle instead sweeps it evenly from the face toward the
         ;; edge, which is what a fillet of that radius actually does.  The
         ;; guarded denominator matters: over the flat middle of a face the
         ;; lean is exactly zero, and the angle is zero with it.
         (lean-magnitude (sqrt (dot bevel-lean bevel-lean)))
         (lean-direction (/ bevel-lean (max 0.0001 lean-magnitude)))
         (bevel-turn
           (* (min 1.0 lean-magnitude) (* 0.90 bevel-fade)))
         (rounded
           (+ (* flat-normal (cos bevel-turn)) (* lean-direction (sin bevel-turn))))
         (seam-occlusion (- 1.0 (* 0.34 (* seam bevel-fade))))
         ;; --- the weathering -----------------------------------------------
         ;; The atlas has one tile per material and the world has thousands of
         ;; faces per tile, so on its own every grass block is the same card
         ;; seen again.  Three fields fix that and none of them costs a texel:
         ;; a hash of the cell a face belongs to, constant across that face and
         ;; different on the next one; value noise over the world point itself,
         ;; which knows nothing about where faces begin and so drifts across a
         ;; whole plain the way real ground does; and a grain finer than a cell
         ;; whose own gradient leans the surface, so the ground is not merely
         ;; painted unevenly but lit unevenly.  The cell is found by stepping
         ;; half a cell back along the face's own normal, which lands inside
         ;; the block whichever of the six faces this is.  #3AVEKC
         (face-cell (floor (- surface-point (* flat-normal 0.5))))
         (face-seed (lattice-hash (+ face-cell (* flat-normal 0.37))))
         (face-hue (lattice-hash (+ face-cell (vec3 5.21 1.37 9.13))))
         (patch (lattice-fractal-noise (* surface-point 0.075)))
         (grain-point (* surface-point 1.05))
         (grain (lattice-noise grain-point))
         ;; Two more taps of the same grain, a step along each of the face's
         ;; own in-plane axes: their differences are the field's gradient
         ;; there, which is exactly the tilt a bump of it would give.
         (grain-u (lattice-noise (+ grain-point (* tangent-u 0.55))))
         (grain-v (lattice-noise (+ grain-point (* tangent-v 0.55))))
         (weathering
           (clamp
            (* surface-detail
               (+ (* (- patch 0.5) 1.05)
                  (+ (* (- face-seed 0.5) 0.30)
                     (* (- grain 0.5) (* 0.30 bevel-fade)))))
            -0.42 0.42))
         ;; Brighter patches are also warmer and duller ones cooler, and a
         ;; face's own constant drifts its hue as well as its value: a plain
         ;; of grass wants to be many greens, not one green at many
         ;; brightnesses.
         (hue-drift
           (clamp
            (* surface-detail
               (+ (* (- face-hue 0.5) 0.36) (* (- patch 0.5) 0.44)))
            -0.35 0.35))
         (weathered-tint
           (+ (vec3 1.0 1.0 1.0)
              (+ (* (vec3 1.12 1.0 0.84) weathering)
                 (* (vec3 0.13 0.02 -0.11) hue-drift))))
         ;; The grain is a cell-wide feature, so it survives to the distance
         ;; a cell does rather than the distance a texel does.
         (bump-strength (* 0.85 (* bevel-fade surface-detail)))
         (shaped
           (normalize (+ (+ (* rounded relief-z)
                            (* tangent-u
                               (+ relief-x (* (- grain grain-u) bump-strength))))
                         (* tangent-v
                            (+ relief-y
                               (* (- grain grain-v) bump-strength))))))
         (shading-normal
           (assume-quantity shaped :quantity :world-direction :unit :one))
         ;; --- the micro-surface --------------------------------------------
         ;; The atlas normal is this material's own slope at texel scale, and
         ;; the best evidence available about what happens below a texel: a
         ;; steep texel is a rough material.  When the relief fades out with
         ;; distance its variance has to go somewhere, and the honest place is
         ;; the roughness -- Toksvig's argument -- so a highlight keeps its
         ;; size across the draw distance instead of sharpening into a
         ;; sparkle on every distant face at once.  #2T8CCH
         (relief-slope
           (sqrt (+ (* (swizzle tangent-normal :x) (swizzle tangent-normal :x))
                    (* (swizzle tangent-normal :y)
                       (swizzle tangent-normal :y)))))
         (roughness
           (clamp
            (* surface-roughness
               (+ (mix 0.40 0.92 (clamp (* relief-slope 1.7) 0.0 1.0))
                  (+ (* 0.28 (- 1.0 relief-fade))
                     (* 0.10 (- grain 0.5)))))
            0.16 0.99))
         (sun-direction (swizzle sun-vector :xyz))
         (n-dot-l (max 0.0 (dot shading-normal sun-direction)))
         (shadow-coordinate shadow-uv-input)
         (shadow-u (swizzle shadow-coordinate :x))
         (shadow-v (swizzle shadow-coordinate :y))
         (shadow-in-bounds
           (* (step (quantity 0.0 :quantity :shadow-u :unit :one)
                    shadow-u)
              (step shadow-u
                    (quantity 1.0 :quantity :shadow-u :unit :one))
              (step (quantity 0.0 :quantity :shadow-v :unit :one)
                    shadow-v)
              (step shadow-v
                    (quantity 1.0 :quantity :shadow-v :unit :one))))
         (shadow-texel-size (swizzle shadow-control-vector :xy))
         (shadow-base-bias (swizzle shadow-control-vector :z))
         (shadow-slope-bias (swizzle shadow-control-vector :w))
         (shadow-bias
           (+ shadow-base-bias (* shadow-slope-bias (- 1.0 n-dot-l))))
         ;; A PCF tap displaced across a receiver plane must compare against
         ;; that plane's depth at the tap, not the center fragment depth.
         ;; With the orthographic shadow rows, the analytical UV depth gradient
         ;; is the face normal expressed in the light basis and scaled by the
         ;; world-span/depth-span ratio.  Its denominator is bounded only near
         ;; grazing incidence, where direct sun is negligible.  This prevents
         ;; wide kernels from reading the receiver's own slope as an occluder.
         (shadow-right
           (assume-quantity (normalize (swizzle shadow-row-x :xyz))
                            :quantity :world-direction :unit :one))
         (shadow-up
           (assume-quantity (normalize (swizzle shadow-row-y :xyz))
                            :quantity :world-direction :unit :one))
         (shadow-forward
           (assume-quantity (normalize (swizzle shadow-row-z :xyz))
                            :quantity :world-direction :unit :one))
         (shadow-normal-forward
           (min -0.05 (dot normal shadow-forward)))
         (shadow-depth-span (swizzle shadow-filter-vector :x))
         (shadow-world-units-per-texel
           (swizzle shadow-filter-vector :y))
         (shadow-world-span
           (interpret
            (/ shadow-world-units-per-texel (swizzle shadow-texel-size :x))
            :quantity :world-distance :unit :cell))
         (shadow-span-ratio (/ shadow-world-span shadow-depth-span))
         (shadow-depth-gradient
           (interpret
            (vec2
             (- (assume-quantity
                 (representation
                  (* (/ (dot normal shadow-right) shadow-normal-forward)
                     shadow-span-ratio))
                 :quantity :shadow-depth-gradient :unit :one))
             (- (assume-quantity
                 (representation
                  (* (/ (dot normal shadow-up) shadow-normal-forward)
                     shadow-span-ratio))
                 :quantity :shadow-depth-gradient :unit :one)))
            :quantity :shadow-depth-gradient :unit :one))
         (shadow-center-depth
           (swizzle
            (sample shadow-map shadow-sampler shadow-coordinate) :x))
         (receiver-depth shadow-depth-input)
         (shadow-blocker-separation
           (interpret
            (max (quantity 0.0 :quantity :shadow-depth :unit :one
                           :character :difference)
                 (- (- receiver-depth shadow-bias)
                    shadow-center-depth))
            :quantity :shadow-depth :unit :one :character :absolute))
         (shadow-minimum-radius (swizzle shadow-filter-vector :z))
         (shadow-maximum-radius (swizzle shadow-filter-vector :w))
         (sun-angular-width (swizzle sun-color-vector :w))
         (shadow-penumbra-world-radius
           (interpret
            (* (* shadow-blocker-separation shadow-depth-span)
               sun-angular-width)
            :quantity :world-distance :unit :cell))
         (shadow-filter-radius
           (clamp
            (+ shadow-minimum-radius
               (interpret
                (/ shadow-penumbra-world-radius
                   shadow-world-units-per-texel)
                :quantity :shadow-filter-radius :unit :one))
            shadow-minimum-radius shadow-maximum-radius))
         (shadow-sample
           (shadow-visibility
            shadow-map shadow-comparison-sampler
            shadow-coordinate receiver-depth shadow-depth-gradient
            shadow-texel-size shadow-bias shadow-filter-radius))
         (sampled-shadow (mix 1.0 shadow-sample shadow-in-bounds))
         ;; A surface turned away from the light has no shadow decision to
         ;; make: its own geometry already excludes the sun, the receiver
         ;; plane's depth runs almost parallel to the light so every filter
         ;; tap lands a large and increasingly ill-conditioned distance away,
         ;; and the map's answer degenerates into noise.  Near noon that is
         ;; every vertical face in the world at once, which is why the
         ;; shimmer was worst there.  Fading the decision out toward "lit"
         ;; exactly where it stops meaning anything costs nothing visually --
         ;; the same cosine multiplies the direct term to zero -- and gives
         ;; the specular lobe and the diagnostic a stable value instead of
         ;; the noise.
         ;;
         ;; The lower edge is where the receiver-plane denominator above
         ;; starts being clamped: SHADOW-FORWARD is the negated sun, so that
         ;; dot product is exactly -N-DOT-L, and the clamp engages precisely
         ;; below 0.05.  The two mechanisms therefore hand over rather than
         ;; overlap, and the clamp never has to carry a visible decision.
         (shadow-relevance (smoothstep 0.05 0.22 n-dot-l))
         (direct-shadow (mix 1.0 sampled-shadow shadow-relevance))
         ;; The mesh carries normalized raw light readings; every response
         ;; curve and balance below is an art parameter editable live
         ;; without remeshing the world.
         (sky-input (swizzle light-input :x))
         (block-input (swizzle light-input :y))
         (emission-input (swizzle light-input :z))
         (sky-level (* sky-input sky-input))
         (block-level (* block-input block-input))
         (day-factor (swizzle sun-vector :w))
         ;; Lateral skylight gives ambient visibility but not a hard sun
         ;; beam; the shadow map gates only the direct solar term.
         (sun-visibility
           (smoothstep
            (quantity 0.90 :quantity :sky-light-level :unit :one)
            (quantity 1.0 :quantity :sky-light-level :unit :one)
            sky-input))
         (ambient (swizzle ambient-vector :xyz))
         (sun-color (swizzle sun-color-vector :xyz))
         ;; --- the deck's shadow --------------------------------------------
         ;; The sky draws a cumulus deck on a plane at a fixed world height,
         ;; and the ground under it should know.  Following the sun up from
         ;; this point to that plane and asking the very same field the very
         ;; same coverage question is the whole of it: the deck's own shadow
         ;; sweeping across the world, which is what keeps a plain at noon
         ;; from being one flat sheet of light.  The threshold is wider than
         ;; the sky's, because a shadow thrown from that far off is blurred
         ;; by the sun's own angular width long before it lands.  #RSGLTL
         (elapsed (representation (swizzle fog-vector :z)))
         (cloudiness (representation (swizzle fog-vector :w)))
         (sun-climb (max 0.15 (representation (swizzle sun-direction :y))))
         (deck-reach
           (/ (max 8.0 (- cloud-altitude (swizzle surface-point :y)))
              sun-climb))
         (deck-meeting
           (+ (+ surface-point (* (representation sun-direction) deck-reach))
              (vec3 (* elapsed 2.2) 0.0 (* elapsed 1.2))))
         (deck-cover (cloud-fractal-noise (* deck-meeting 0.0044)))
         (deck-coverage
           (mix 0.82 0.46 (clamp (* cloudiness cloud-coverage) 0.0 1.0)))
         (cloud-shadow
           (assume-quantity
            (- 1.0
               (* cloud-shadow-depth
                  (smoothstep deck-coverage (+ deck-coverage 0.20)
                              deck-cover)))
            :quantity :cloud-shadow :unit :one))
         ;; Occlusion bites harder than the raw mesh reading: the corner where
         ;; three blocks meet is what tells the eye these are solid volumes.
         (occlusion
           (interpret
            (* (expt ao 1.8)
               (assume-quantity seam-occlusion
                                :quantity :ambient-occlusion :unit :one))
            :quantity :ambient-occlusion :unit :one))
         ;; --- what is not the sun ------------------------------------------
         ;; Everything above a face that is not the sun is still the sky, and
         ;; the sky is not one colour: a face turned up sees the zenith, a
         ;; face turned sideways sees the horizon, and a face turned down sees
         ;; what the ground bounced.  One interpolation over the normal's own
         ;; vertical component is the whole of it, and it is the difference
         ;; between blocks standing in a world and blocks floating in a grey
         ;; room.  The profile's ambient colour stays in the mixture: it is
         ;; the art direction's say over light the geometry cannot explain.
         (facing-up
           (* 0.5 (+ 1.0 (representation (swizzle shading-normal :y)))))
         (dome-color
           (mix (representation (swizzle horizon-vector :xyz))
                (representation (swizzle zenith-vector :xyz))
                (* facing-up facing-up)))
         (bounce-color
           (* (representation (swizzle fog-color-vector :xyz))
              ambient-bounce))
         (environment
           (assume-quantity
            (mix bounce-color
                 (mix (representation ambient) dome-color 0.62)
                 facing-up)
            :quantity :linear-rgb :unit :one))
         ;; A small floor keeps unlit geometry barely readable rather than
         ;; a void; caves stay dark for the right reason.  The ambient term
         ;; is deliberately weaker, and the sun correspondingly stronger,
         ;; than a display-referred renderer could afford: the filmic curve
         ;; on presentation is what brings the sunlit half back down.
         (sky-light
           (interpret (* environment (+ 0.030 (* 0.86 sky-level)) occlusion)
                      :quantity :linear-rgb :unit :one))
         (sun-light
           (interpret
            (* sun-color
               (* direct-light-gain n-dot-l sun-visibility day-factor direct-shadow
                  cloud-shadow (mix 0.55 1.0 ao)))
            :quantity :linear-rgb :unit :one))
         (torch-color
           (quantity (vec3 1.0 0.82 0.58)
                     :quantity :linear-rgb :unit :one))
         (local-light
           (interpret (* torch-color block-level)
                      :quantity :linear-rgb :unit :one))
         (albedo
           (assume-quantity
            (* (representation
                (swizzle (sample block-atlas block-sampler uv) :rgb))
               weathered-tint)
            :quantity :linear-rgb :unit :one))
         (reflected
           (interpret
            (* albedo (+ sky-light sun-light local-light))
            :quantity :linear-rgb :unit :one))
         ;; --- the microfacet lobe ------------------------------------------
         ;; One GGX lobe off the shaped normal, with Smith's height-correlated
         ;; visibility and a dielectric's Fresnel: blocks are not metal, so
         ;; four per cent at normal incidence and the whole of it at a grazing
         ;; one.  The roughness above decides everything about its shape, so a
         ;; polished face and a tufted one differ here without differing
         ;; anywhere else.
         (view-direction
           (assume-quantity (normalize (- eye surface-point))
                            :quantity :world-direction :unit :one))
         (half-vector
           (assume-quantity
            (normalize (+ (normalize (- eye surface-point))
                          (representation sun-direction)))
            :quantity :world-direction :unit :one))
         (n-dot-h (max 0.0 (dot shading-normal half-vector)))
         (n-dot-v (max 0.0 (dot shading-normal view-direction)))
         (v-dot-h (max 0.0 (dot view-direction half-vector)))
         (cosine-light (representation n-dot-l))
         (cosine-view (representation n-dot-v))
         (cosine-half (representation n-dot-h))
         (alpha (* roughness roughness))
         (alpha-squared (* alpha alpha))
         (distribution-denominator
           (+ (* (* cosine-half cosine-half) (- alpha-squared 1.0)) 1.0))
         (distribution
           (/ alpha-squared
              (max 0.0001
                   (* 3.14159265
                      (* distribution-denominator distribution-denominator)))))
         ;; Smith's height-correlated visibility already carries the
         ;; 1/(4 cos cos) the microfacet specular would otherwise divide by.
         (visibility-light
           (* cosine-view
              (sqrt (+ (* (* cosine-light cosine-light) (- 1.0 alpha-squared))
                       alpha-squared))))
         (visibility-view
           (* cosine-light
              (sqrt (+ (* (* cosine-view cosine-view) (- 1.0 alpha-squared))
                       alpha-squared))))
         (visibility (/ 0.5 (max 0.0001 (+ visibility-light visibility-view))))
         (fresnel
           (+ 0.04 (* 0.96 (expt (- 1.0 (representation v-dot-h)) 5.0))))
         ;; The cosine belongs in the specular lobe as much as in the diffuse
         ;; one: without it a highlight can appear on a face the sun cannot
         ;; reach, carrying whatever the shadow map happened to say there.
         (sun-specular
           (* (representation sun-color)
              (* (* distribution (* visibility fresnel))
                 (* cosine-light
                    (* (representation sun-visibility)
                       (* (representation day-factor)
                          (* (representation direct-shadow)
                             (* (representation cloud-shadow)
                                (* (representation occlusion)
                                   specular-gain)))))))))
         ;; The same sky again, in the direction the surface actually
         ;; reflects: the sheen that tells a smooth face from a rough one
         ;; out of the sun, and the one term that makes a block look like it
         ;; is standing under this weather rather than under a light bulb.
         (reflection-direction
           (- (* (representation shading-normal) (* 2.0 cosine-view))
              (representation view-direction)))
         (reflection-up
           (clamp (* 0.5 (+ 1.0 (swizzle reflection-direction :y))) 0.0 1.0))
         (reflected-sky
           (mix (representation (swizzle horizon-vector :xyz))
                (representation (swizzle zenith-vector :xyz))
                (* reflection-up reflection-up)))
         (sheen
           (* (- 1.0 roughness)
              (+ 0.035 (* 0.32 (expt (- 1.0 cosine-view) 5.0)))))
         (ambient-specular
           (* reflected-sky
              (* sheen (* (representation sky-level)
                          (representation occlusion)))))
         (specular
           (assume-quantity (+ sun-specular ambient-specular)
                            :quantity :linear-rgb :unit :one))
         (radiance
           (+ reflected specular
              (interpret (* albedo emission-input)
                         :quantity :linear-rgb :unit :one)))
         ;; Distant terrain fades into exactly the colour the sky's own ground
         ;; half arrives at, warmed where the ray runs toward a low sun: the
         ;; aerial perspective that puts a mile of air between here and the
         ;; horizon.
         (look-direction (normalize (- surface-point eye)))
         (sun-elevation (representation (swizzle sun-direction :y)))
         (low-sun
           (* (representation day-factor)
              (- 1.0 (smoothstep 0.02 0.45 sun-elevation))))
         (fog-color
           (assume-quantity
            (aerial-perspective-color
             (representation (swizzle fog-color-vector :xyz))
             look-direction (representation sun-direction)
             low-sun (representation day-factor))
            :quantity :linear-rgb :unit :one))
         (fog-amount fog-input)
         (fogged (mix radiance fog-color fog-amount))
         (normal-rgba
           (assume-quantity
            (vec4
             (representation fogged)
             (representation
              (quantity 1.0 :quantity :opacity :unit :one)))
            :quantity :linear-rgba :unit :one))
         (shadow-diagnostic (swizzle fog-color-vector :w))
         (shadow-rgba
           (assume-quantity
            (vec4 (representation
                   (vec3 direct-shadow direct-shadow direct-shadow))
                  (representation
                   (quantity 1.0 :quantity :opacity :unit :one)))
            :quantity :linear-rgba :unit :one))
         (rgba (mix normal-rgba shadow-rgba shadow-diagnostic)))
    (set-output color-output rgba)))
(defun block-world-fragment-specification ()
  (shader-specification-for :block-surface :fragment))

(defun block-world-fragment-lowering ()
  "Compile the block material and retain its expression-to-SSA provenance."
  (spv:compile-shader-specification (block-world-fragment-specification)))

(defun block-world-fragment-module ()
  "Lower the readable block material to luv's structured SPIR-V module."
  (spv:shader-lowering-module (block-world-fragment-lowering)))

(defun block-world-fragment-shader ()
  (spv:assemble-spir-v-module (block-world-fragment-module)))

;;; The sky is a fullscreen triangle drawn before block geometry with depth
;;; writes disabled.  Its vertex stage reconstructs a per-corner view ray
;;; from the camera basis; its fragment stage is pure image mathematics over
;;; the interpolated ray and the frame environment lanes.

(define-shader-method shader-specification-for
    block-world-sky-vertex-specification
    ((role (eql :sky)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((corner-position :vec3 :location 0
                               :quantity :clip-coordinate :unit :one))
     :outputs ((clip-position :vec4 :built-in :position)
               (ray-output :vec3 :location 0
                           :quantity :world-direction :unit :one))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((corner corner-position)
         (x (swizzle corner :x))
         (y (swizzle corner :y))
         (z (swizzle corner :z))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (x-scale (swizzle projection-vector :x))
         (y-scale (swizzle projection-vector :y))
         ;; Invert the block vertex projection: clip x,y back to view-space
         ;; slopes, including its y flip, so the ray agrees with the world.
         ;; These coordinates are measured from clip-space's zero origin.
         ;; Representation makes that origin choice explicit and erased: no
         ;; runtime subtraction is needed to treat either coordinate as a
         ;; displacement before scaling it back into view space.
         (clip-x-displacement
           (assume-quantity (representation x)
                            :quantity :clip-x-coordinate :unit :one
                            :character :difference))
         (clip-y-displacement
           (assume-quantity (representation y)
                            :quantity :clip-y-coordinate :unit :one
                            :character :difference))
         (view-x (/ clip-x-displacement x-scale))
         (view-y
           (- (/ clip-y-displacement y-scale)))
         (ray (+ (* right view-x) (* up view-y) forward))
         (clip (vec4 (representation x)
                     (representation y)
                     (representation z)
                     1.0)))
    (set-output clip-position clip)
    (set-output ray-output ray)))

(defun block-world-sky-vertex-specification ()
  (shader-specification-for :sky :vertex))

(defun block-world-sky-vertex-module ()
  (spv:shader-lowering-module
   (spv:compile-shader-specification (block-world-sky-vertex-specification))))

(defun block-world-sky-vertex-shader ()
  (spv:assemble-spir-v-module (block-world-sky-vertex-module)))

;;; Lattice value noise is the whole procedural vocabulary this file needs: a
;;; hash of an integer lattice index, trilinear smoothing between its corners,
;;; and a few octaves with the domain rotated between them so the cubic grain
;;; never reads as a grid.  It is written once here and reused by the cloud
;;; decks, the star field, the moon's maria, and the weathering that makes one
;;; block face differ from the next.

(define-shader-function lattice-hash (site)
  "Hash one integer lattice site into the unit interval.

Deliberately trigonometry-free, and deliberately a function of the whole
site rather than of a collapsed scalar index.  Hashing x + 57y + 113z
through a sine makes every plane of constant index share a hash, and at
star-field frequencies those planes are plainly visible as streaks across
the sky.  Folding the site's own components against each other has no
preferred direction, and costs less than a sine besides."
  (let* ((scattered (fract (* site 0.1031)))
         (shift (dot scattered
                     (+ (swizzle scattered :zyx)
                        (vec3 31.32 31.32 31.32))))
         (folded (+ scattered (vec3 shift shift shift))))
    (fract (* (+ (swizzle folded :x) (swizzle folded :y))
              (swizzle folded :z)))))

(define-shader-function lattice-noise (point)
  "Smoothly interpolated value noise over an integer lattice."
  (let* ((lattice (floor point))
         (offset (fract point))
         ;; The classic smoothstep weight: continuous first derivative at the
         ;; cell boundaries, which is what keeps the clouds from creasing.
         (weight (* offset (* offset (- (vec3 3.0 3.0 3.0) (* offset 2.0)))))
         (u (swizzle weight :x))
         (v (swizzle weight :y))
         (w (swizzle weight :z))
         (near-low (mix (lattice-hash lattice)
                        (lattice-hash (+ lattice (vec3 1.0 0.0 0.0))) u))
         (near-high (mix (lattice-hash (+ lattice (vec3 0.0 1.0 0.0)))
                         (lattice-hash (+ lattice (vec3 1.0 1.0 0.0))) u))
         (far-low (mix (lattice-hash (+ lattice (vec3 0.0 0.0 1.0)))
                       (lattice-hash (+ lattice (vec3 1.0 0.0 1.0))) u))
         (far-high (mix (lattice-hash (+ lattice (vec3 0.0 1.0 1.0)))
                        (lattice-hash (+ lattice (vec3 1.0 1.0 1.0))) u)))
    (mix (mix near-low near-high v) (mix far-low far-high v) w)))

(define-shader-function lattice-fractal-noise (point)
  "Three octaves of value noise; the domain rotates to hide the lattice.

The fold's state carries the rotating sample point and the running sum
together, so the noise body is emitted once and executed three times rather
than inlined three times."
  (let* ((accumulated
           (counted-fold (octave 3.0 state (vec4 point 0.0))
             (let* ((sample-point (swizzle state :xyz))
                    (x (swizzle sample-point :x))
                    (y (swizzle sample-point :y))
                    (z (swizzle sample-point :z))
                    (gain (expt 0.5 (+ octave 1.0)))
                    (total (+ (swizzle state :w)
                              (* (lattice-noise sample-point) gain)))
                    (rotated
                      (* (vec3 (+ (* 0.8 x) (* 0.6 z))
                               (+ y 7.31)
                               (+ (* -0.6 x) (* 0.8 z)))
                         2.03)))
               (vec4 rotated total)))))
    (swizzle accumulated :w)))

(define-shader-function cloud-fractal-noise (point)
  "Four octaves of the same noise: a cloud deck's shape down to its wisps.

A deck's silhouette is decided by the first octave and its edges by the
last, so this is the one place in the sky worth paying for a fourth.  The
weights are the same halving series, renormalized, so the field still spans
the unit interval and a coverage threshold means the same thing it does
with three."
  (let* ((accumulated
           (counted-fold (octave 4.0 state (vec4 point 0.0))
             (let* ((sample-point (swizzle state :xyz))
                    (x (swizzle sample-point :x))
                    (y (swizzle sample-point :y))
                    (z (swizzle sample-point :z))
                    (gain (expt 0.5 (+ octave 1.0)))
                    (total (+ (swizzle state :w)
                              (* (lattice-noise sample-point) gain)))
                    (rotated
                      (* (vec3 (+ (* 0.78 x) (* 0.63 z))
                               (+ y 11.17)
                               (+ (* -0.63 x) (* 0.78 z)))
                         2.11)))
               (vec4 rotated total)))))
    (* (swizzle accumulated :w) 1.067)))

(define-shader-function henyey-greenstein (cosine asymmetry)
  "The Henyey-Greenstein phase function: how much light a haze sends on at
COSINE off its original direction, for a medium of the given ASYMMETRY.

One expression replaces the two fitted powers the sky used to brighten
around the sun.  A phase function's shape is the reason a low sun has a
huge soft glow and a high one a tight one: the same medium, the same
number, a different angle."
  (let* ((squared (* asymmetry asymmetry))
         (denominator
           (max 0.0001 (+ (+ 1.0 squared) (* -2.0 (* asymmetry cosine))))))
    (/ (- 1.0 squared) (* 12.566371 (expt denominator 1.5)))))

(define-shader-function aerial-perspective-color
    (fog-color direction sun-direction low-sun day-factor)
  "The colour something arbitrarily far away takes, seen along DIRECTION.

Distant terrain and the sky's own ground half must agree exactly, or the
edge of the resident world draws itself as a line.  They agree by asking
this: the profile's fog colour, warmed where the ray runs toward a low sun
and brightened by the same haze the sky's halo is made of, so a ridge in
the east at dawn glows and one in the west stays cool.  #8X33G2"
  (let* ((alignment (dot direction sun-direction))
         (toward (max 0.0 alignment))
         (glow (henyey-greenstein alignment 0.66))
         (warm (mix (vec3 1.08 0.74 0.46) (vec3 1.25 0.52 0.27) low-sun)))
    (+ (* fog-color (- 1.0 (* 0.18 (* low-sun toward))))
       (* warm (* day-factor
                  (+ (* glow 0.055)
                     (* low-sun (* (* toward toward) 0.22))))))))

(define-shader-function star-light (direction elapsed)
  "One star per lattice cell of the sky, at a hashed place in its cell.

Value noise raised to a power -- what the star field used to be -- makes
smooth blobs the size of its own lattice, and they smear into streaks
wherever the interpolation runs along a cell edge.  A star is a point, so
this hashes the cell to a position, a magnitude, and a twinkling phase, and
draws a small gaussian around it.  Magnitude is a steep power of its hash:
a few bright stars and a great many faint ones, which is the actual
distribution overhead."
  (let* ((point (* direction 74.0))
         (cell (floor point))
         (local (fract point))
         (place-x (lattice-hash (+ cell (vec3 19.7 5.3 11.1))))
         (place-y (lattice-hash (+ cell (vec3 3.1 23.9 7.7))))
         (place-z (lattice-hash (+ cell (vec3 41.3 13.7 29.5))))
         ;; Keep the star off its cell's boundary so no star is ever cut in
         ;; half by the next cell's gaussian falling off first.
         (centre (+ (vec3 0.25 0.25 0.25)
                    (* (vec3 place-x place-y place-z) 0.5)))
         (offset (- local centre))
         (radius (dot offset offset))
         (magnitude (expt place-z 9.0))
         (twinkle (+ 0.74 (* 0.26 (sin (+ (* elapsed 2.3)
                                          (* place-x 43.0))))))
         (spread (exp (* -230.0 radius))))
    (* magnitude (* twinkle spread))))

;;; The sky is image mathematics over a view ray and the frame environment.
;;; What it argues is that everything up there is one atmosphere seen at
;;; different depths:
;;;
;;;   - the vertical gradient runs the whole way from horizon to zenith
;;;     rather than resolving in the first few degrees, because that is what
;;;     the optical depth along a ray actually does;
;;;   - haze thickens toward the horizon, and the sunrise band it carries
;;;     belongs to the sun's own quarter of the compass, not all the way
;;;     round;
;;;   - one Henyey-Greenstein phase function, evaluated at two asymmetries,
;;;     is the whole glow around the sun;
;;;   - two cloud decks, each a plane at a fixed height, so the ray meets
;;;     them farther out the closer it runs to level: thin cirrus far above,
;;;     and the cumulus sheet whose own shadow, sampled once along the deck
;;;     toward the sun, gives it a lit face and a dark underside;
;;;   - at night, points for stars, a band for the galaxy, and the moon
;;;     opposite the sun;
;;;   - a compact solar disc pushed well past display white so the lens
;;;     chain has something real to bloom.
;;;
;;; Everything below the horizon arrives at the exact fog colour distant
;;; terrain fades to, so silhouettes meet the sky without a seam, and only
;;; then darkens -- gently, because a stronger falloff shows up as a step
;;; where the last resident chunk ends.  #9SSXDJ records why an HDR path
;;; wants a sky with something genuinely bright in it, and #8X33G2 what each
;;; of these terms replaced.

(define-shader-method shader-specification-for
    block-world-sky-fragment-specification
    ((role (eql :sky)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((ray-input :vec3 :location 0
                         :quantity :world-direction :unit :one))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((direction (normalize (representation ray-input)))
         (elevation (swizzle direction :y))
         (above (max elevation 0.0))
         (eye (representation (swizzle camera-vector :xyz)))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (day-factor (representation (swizzle sun-vector :w)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (sun-width (representation (swizzle sun-color-vector :w)))
         (zenith (representation (swizzle zenith-vector :xyz)))
         (horizon (representation (swizzle horizon-vector :xyz)))
         (fog-color (representation (swizzle fog-color-vector :xyz)))
         (elapsed (representation (swizzle fog-vector :z)))
         (cloudiness (representation (swizzle fog-vector :w)))
         ;; One number decides how much of the sky is sunrise: a sun near the
         ;; horizon warms the haze, the scatter, and the cloud faces together.
         (sun-elevation (swizzle sun-direction :y))
         (low-sun (* day-factor (- 1.0 (smoothstep 0.02 0.45 sun-elevation))))
         (alignment (dot direction sun-direction))
         (toward-sun (max 0.0 alignment))
         ;; The sun's half of the sky, measured on the ground plane: a sunrise
         ;; band belongs in the east, and the west should stay blue.
         (level-ray (vec3 (swizzle direction :x) 0.0 (swizzle direction :z)))
         (level-sun
           (vec3 (swizzle sun-direction :x) 0.0 (swizzle sun-direction :z)))
         (azimuth
           (max 0.0
                (/ (dot level-ray level-sun)
                   (max 0.001
                        (* (sqrt (dot level-ray level-ray))
                           (sqrt (dot level-sun level-sun)))))))
         ;; --- the atmosphere ---------------------------------------------
         ;; The vertical gradient is the sky's own colour at depth; a small
         ;; exponent spreads it across the whole hemisphere instead of
         ;; resolving it in the first few degrees, which is what an optical
         ;; depth along the ray actually does.
         (gradient (expt (clamp elevation 0.0 1.0) 0.42))
         (base (mix horizon zenith gradient))
         (haze (exp (* -9.0 above)))
         ;; A sunrise is sunlight that has come the long way through the
         ;; atmosphere, so the band it paints is the sun's own colour at that
         ;; hour, laid exactly where the ray runs both low and toward it.  Away
         ;; from the sun's quarter of the compass the sky stays its own colour,
         ;; which is what keeps the west blue while the east burns.
         (warm-color (* sun-color 0.82))
         (warm-band
           (* low-sun (* haze (+ 0.12 (* 0.88 (* azimuth azimuth))))))
         (hazed (mix base warm-color (clamp (* 0.92 warm-band) 0.0 1.0)))
         (broad (henyey-greenstein alignment 0.62))
         (tight (henyey-greenstein alignment 0.90))
         (halo-tint (mix (vec3 1.0 0.97 0.90) (vec3 1.0 0.52 0.24) low-sun))
         (halo-depth (mix 0.70 1.60 haze))
         (halo
           (* (+ (* broad (+ 0.030 (* 0.055 low-sun)))
                 (* tight (+ 0.014 (* 0.055 low-sun))))
              (* day-factor (* halo-depth sky-scatter-gain))))
         (scattered (+ hazed (* halo-tint halo)))
         ;; --- night ------------------------------------------------------
         (night (- 1.0 (smoothstep 0.0 0.28 day-factor)))
         ;; The galaxy is a great circle of the sky, so one fixed axis and the
         ;; ray's distance from its plane is the whole band.
         (galaxy-axis (vec3 0.42 0.55 -0.72))
         (galaxy-distance (dot direction galaxy-axis))
         (galaxy-band (exp (* -20.0 (* galaxy-distance galaxy-distance))))
         (galaxy-structure
           (+ (* (lattice-noise (* direction 15.0)) 0.58)
              (* (lattice-noise (* direction 44.0)) 0.42)))
         (galaxy
           (* galaxy-band
              (* (+ 0.10 (* 1.25 galaxy-structure))
                 ;; A dust lane is the band's own darkness, not an absence of
                 ;; stars, so it multiplies rather than subtracts.
                 (- 1.0 (* 0.55 (smoothstep 0.42 0.66
                                            (lattice-noise
                                             (* direction 7.0))))))))
         (star-visibility
           (* night (smoothstep -0.02 0.14 elevation)))
         (stars
           (* (star-light direction elapsed)
              (* star-brightness (* star-visibility (+ 1.0 (* 1.6 galaxy-band))))))
         (starred
           (+ (* (vec3 0.60 0.66 0.94) (* galaxy (* star-visibility 0.17)))
              (* (vec3 0.92 0.94 1.0) stars)))
         ;; The moon rides opposite the sun, which puts it up for exactly the
         ;; hours the sun is not, and always full: the simple sky this world
         ;; wants, and the one that lights its nights.
         (moon-direction (* sun-direction -1.0))
         (moon-alignment (dot direction moon-direction))
         (moon-radius (* sun-width 1.7))
         (moon-limb (* 0.5 (* moon-radius moon-radius)))
         (moon-disc
           (smoothstep (- 1.0 moon-limb) (- 1.0 (* 0.80 moon-limb))
                       moon-alignment))
         ;; The component of the ray across the moon's own direction, in units
         ;; of its radius: the disc's face, which the maria are painted on.
         (moon-face
           (/ (- direction (* moon-direction moon-alignment))
              (max 0.0001 moon-radius)))
         (moon-maria (lattice-noise (+ (* moon-face 1.8) (vec3 13.0 5.0 9.0))))
         (moon-shape
           (* (+ 0.70 (* 0.50 moon-maria))
              (- 1.0 (* 0.40 (clamp (dot moon-face moon-face) 0.0 1.0)))))
         (moon-glow (* (expt (max 0.0 moon-alignment) 220.0) 0.30))
         (lunar
           (* (vec3 0.94 0.95 1.0)
              (* night (+ (* moon-disc (* moon-radiance moon-shape))
                          moon-glow))))
         (nightly (+ scattered (+ starred lunar)))
         ;; --- the cloud decks --------------------------------------------
         (cirrus-point
           (+ (* direction (/ 2400.0 (max elevation 0.02)))
              (vec3 (* elapsed 6.0) 0.0 (* elapsed 2.0))))
         (cirrus-field
           (lattice-fractal-noise
            (* cirrus-point (vec3 0.00105 0.00105 0.00225))))
         (cirrus
           (* (smoothstep 0.56 0.88 cirrus-field)
              (* (smoothstep 0.02 0.26 elevation)
                 (* (- 1.0 (* 0.55 (clamp cloudiness 0.0 1.0))) 0.45))))
         (cirrus-color
           (* (mix (vec3 1.10 1.12 1.18) (vec3 1.38 0.84 0.56)
                   (* low-sun low-sun))
              (max day-factor 0.05)))
         (with-cirrus (mix nightly cirrus-color cirrus))
         ;; The deck is a plane at a fixed height in the world, not a fixed
         ;; height above the camera: the ground has to be able to find the
         ;; same plane along the sun and read the same field there, or the
         ;; shadows sweeping over it would belong to some other sky.  #RSGLTL
         (deck-rise (max 8.0 (- cloud-altitude (swizzle eye :y))))
         (deck-point
           (+ (+ eye (* direction (/ deck-rise (max elevation 0.014))))
              (vec3 (* elapsed 2.2) 0.0 (* elapsed 1.2))))
         (deck-scale 0.0044)
         (cloud-field (cloud-fractal-noise (* deck-point deck-scale)))
         ;; Coverage is the profile's cloudiness against the knob; the edge
         ;; softens toward the horizon, where a deck's detail is far smaller
         ;; than a pixel and a hard edge could only shimmer.
         (coverage
           (mix 0.82 0.46 (clamp (* cloudiness cloud-coverage) 0.0 1.0)))
         (softness (mix 0.26 0.055 (smoothstep 0.012 0.34 elevation)))
         (deck-mask (smoothstep 0.006 0.055 elevation))
         (cloud-density
           (* deck-mask (smoothstep coverage (+ coverage softness) cloud-field)))
         (core (clamp cloud-density 0.0 1.0))
         ;; What lies between this piece of deck and the sun, sampled along
         ;; the deck itself: the shape's own shadow, and so its lit face.
         (shadow-field
           (cloud-fractal-noise
            (* (+ deck-point (* level-sun 300.0)) deck-scale)))
         (shadow-density
           (smoothstep coverage (+ coverage softness) shadow-field))
         (cloud-light (- 1.0 (* 0.70 shadow-density)))
         (cloud-lit
           (mix (vec3 1.22 1.20 1.16) (vec3 1.40 0.74 0.36)
                (* low-sun low-sun)))
         (cloud-dark
           (mix (vec3 0.50 0.57 0.74) (vec3 0.44 0.38 0.52)
                (* low-sun low-sun)))
         (cloud-body
           (mix cloud-dark cloud-lit (* cloud-light (- 1.0 (* 0.45 core)))))
         ;; Silver lining: thin edges facing the sun glow, dense cores do not.
         (silver
           (* cloud-lit
              (* (expt toward-sun 14.0)
                 (* (- 1.0 core) (+ 0.30 (* 0.85 low-sun))))))
         ;; After sunset a deck is lit by the sky alone, so it takes the
         ;; sky's own colour; a neutral grey at one twentieth reads warm
         ;; against a night zenith and the eye calls it dust.
         (night-tint
           (mix (vec3 0.44 0.52 0.80) (vec3 1.0 1.0 1.0)
                (smoothstep 0.0 0.35 day-factor)))
         (cloud-color
           (* (* (+ cloud-body silver) night-tint) (max day-factor 0.06)))
         ;; A deck recedes into the same haze the sky's own horizon does.
         (cloud-reach (smoothstep 0.010 0.11 elevation))
         (clouded
           (mix with-cirrus (mix hazed cloud-color cloud-reach)
                (* cloud-density 0.94)))
         ;; --- the ground half --------------------------------------------
         ;; Below the horizon this shader stands in for terrain too far off to
         ;; be resident, so it must arrive at exactly the colour distant
         ;; terrain fades to.  It may still say where the land is: the ray's
         ;; own meeting with the ground plane gives a point to sample, and a
         ;; wide, weak relief over it keeps the lower half from being one flat
         ;; wall of fog.  The relief fades out at the horizon, where the plane
         ;; runs away faster than a pixel can resolve it.
         (aerial
           (aerial-perspective-color fog-color direction sun-direction
                                     low-sun day-factor))
         (descent (max 0.004 (- elevation)))
         (ground-point
           (+ eye (* direction (/ (max 1.0 (swizzle eye :y)) descent))))
         (land (lattice-fractal-noise (* ground-point 0.0021)))
         (land-relief
           (* (- land 0.5) (smoothstep 0.0 -0.22 elevation)))
         (depth-below (smoothstep 0.0 -0.35 elevation))
         (ground
           (* aerial (+ (- 1.0 (* 0.26 depth-below)) (* 0.44 land-relief))))
         (grounded (mix clouded ground (smoothstep 0.060 -0.006 elevation)))
         ;; --- the sun ----------------------------------------------------
         ;; The disc is drawn at a few times the sun's true angular radius,
         ;; the way every game sun is, and deliberately far above display
         ;; white: the floating point attachment keeps it, the bright pass
         ;; feeds on it, and the filmic curve rolls it into a hot core
         ;; instead of a flat clipped patch.  For a small angle the cosine
         ;; threshold of an angular radius is one less half its square.
         (disc-radius (* sun-width sun-disc-scale))
         (disc-limb (* 0.5 (* disc-radius disc-radius)))
         (disc
           (smoothstep (- 1.0 disc-limb) (- 1.0 (* 0.56 disc-limb)) alignment))
         ;; A star's disc is brightest at its centre; without the limb
         ;; darkening a sun drawn this large reads as a sticker.
         (disc-radial
           (clamp (/ (- 1.0 alignment) (max 0.000001 disc-limb)) 0.0 1.0))
         (limb-darkening (- 1.0 (* 0.45 (* disc-radial disc-radial))))
         (corona
           (+ (* (expt toward-sun 900.0) 0.8) (* (expt toward-sun 130.0) 0.22)))
         (occlusion (- 1.0 (* core 0.94)))
         (solar
           (* sun-color
              (* day-factor
                 (+ (* disc (* sun-disc-radiance (* occlusion limb-darkening)))
                    (* corona (* 2.0 occlusion))))))
         (rgb (+ grounded solar)))
    (set-output color-output (vec4 rgb 1.0))))

(defun block-world-sky-fragment-specification ()
  (shader-specification-for :sky :fragment))

(defun block-world-sky-fragment-module ()
  (spv:shader-lowering-module
   (spv:compile-shader-specification (block-world-sky-fragment-specification))))

(defun block-world-sky-fragment-shader ()
  (spv:assemble-spir-v-module (block-world-sky-fragment-module)))

;;; Presentation is where linear scene radiance becomes a display image.  The
;;; scene attachment is floating point, so a sun disc or a specular glint
;;; arrives far above display white; the lens stack composes in that linear
;;; space, exposure scales it, and a filmic curve rolls it off.  Screen center
;;; lies on the framed terminal, so its depth is the focus plane; only farther
;;; fragments receive the wide nine-tap blur.  The HUD is drawn afterward,
;;; already graded, straight onto the presentation image.

;;; The presentation stages share one small uniform block, written once per
;;; frame by LUVCRAFT-POST-UNIFORM-DATA.  Member order is an ABI requirement
;;; exactly as it is for the frame environment, so it is written once here.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *post-uniform-members*
    '((post-control :vec4)    ; texel u, texel v, focus blur, exposure
      (lens-control :vec4)    ; bloom gain, shaft gain, vignette, threshold
      (sun-screen :vec4)      ; sun screen u, v, on-screen weight, diagnostic
      (bloom-control :vec4)   ; chain texel u, v, shaft decay, elapsed time
      (presentation-control :vec4)) ; output texel u, v, reserved, reserved
    "The presentation uniform layout shared by the post and bloom stages."))

;;; Narkowicz's fitted ACES curve.  Its toe keeps shadows from turning to mud,
;;; its shoulder compresses the sun and the crystal glow into a hot core
;;; instead of a flat clipped patch, and it desaturates highlights the way
;;; film does.
(define-shader-function aces-filmic (radiance)
  "Map linear scene radiance to display-linear [0,1] through the ACES fit."
  (let* ((numerator
           (* radiance (+ (* radiance 2.51) (vec3 0.03 0.03 0.03))))
         (denominator
           (+ (* radiance (+ (* radiance 2.43) (vec3 0.59 0.59 0.59)))
              (vec3 0.14 0.14 0.14))))
    (clamp (/ numerator denominator)
           (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))))

(define-shader-method shader-specification-for
    focus-post-vertex-specification
    ((role (eql :focus-post)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((corner-position :vec3 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv-output :vec2 :location 0)))
  (let* ((x (swizzle corner-position :x))
         (y (swizzle corner-position :y)))
    (set-output clip-position (vec4 x y (swizzle corner-position :z) 1.0))
    (set-output uv-output (vec2 (* (+ x 1.0) 0.5)
                                (* (+ y 1.0) 0.5)))))

;;; The bloom and light-shaft chain runs on a quarter-resolution pair of
;;; floating-point attachments.  Bright fragments are extracted once, blurred
;;; separably, and then swept radially away from the solar disc; presentation
;;; adds both back into linear scene radiance before the filmic curve.  Every
;;; stage sees the same bind group shape -- source texture, linear sampler,
;;; presentation uniforms -- so one layout serves the whole chain, and the
;;; stage that reads it is chosen by CLOS role rather than by a mode lane.
;;; #IC14P3 records the shape this stack took and where it left its plan.

(define-shader-method shader-specification-for
    bloom-bright-fragment-specification
    ((role (eql :bloom-bright)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((source-color :texture-2d :set 0 :binding 0
                    :sample-transfer :identity)
      (source-sampler :sampler :set 0 :binding 1)
      (post-state :uniform-block :set 0 :binding 2
                  :members #.*post-uniform-members*)))
  (let* ((texel (swizzle post-control :xy))
         (exposure (swizzle post-control :w))
         (threshold (swizzle lens-control :w))
         (dx (* texel (vec2 1.0 0.0)))
         (dy (* texel (vec2 0.0 1.0)))
         ;; Four scene taps per chain texel.  The chain is a quarter of the
         ;; frame's width, so a single tap would alias exactly the highlights
         ;; this pass exists to smear.
         (box
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input (+ dx dy))) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (+ uv-input (- dx dy))) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input (- dx dy))) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input (+ dx dy))) :xyz))
              0.25))
         ;; The chain works in exposed units, so its contribution stays in
         ;; step with the scene when exposure moves.
         (radiance (* box exposure))
         (luminance (dot radiance (vec3 0.2126 0.7152 0.0722)))
         (knee (smoothstep threshold (+ threshold 0.75) luminance)))
    (set-output color-output (vec4 (* radiance knee) 1.0))))

;;; A thirteen-tap gaussian expressed as seven linearly filtered samples:
;;; each offset lands between the two texels whose weights it combines.  The
;;; host runs the pair twice, which convolves the kernel with itself and puts
;;; the standing deviation at about four chain texels -- a wide, soft glow
;;; rather than a halo tight enough to read as an outline.  The two directions
;;; are separate roles rather than a uniform lane, so each pipeline is exactly
;;; the code it runs.

(define-shader-method shader-specification-for
    bloom-horizontal-fragment-specification
    ((role (eql :bloom-horizontal)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((source-color :texture-2d :set 0 :binding 0
                    :sample-transfer :identity)
      (source-sampler :sampler :set 0 :binding 1)
      (post-state :uniform-block :set 0 :binding 2
                  :members #.*post-uniform-members*)))
  (let* ((span (vec2 (swizzle bloom-control :x) 0.0))
         (near-offset (* span 1.4585))
         (mid-offset (* span 3.4038))
         (far-offset (* span 5.3510))
         (center
           (* (swizzle (sample source-color source-sampler uv-input) :xyz)
              0.1370))
         (near
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input near-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input near-offset)) :xyz))
              0.2393))
         (mid
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input mid-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input mid-offset)) :xyz))
              0.1394))
         (far
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input far-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input far-offset)) :xyz))
              0.0527)))
    (set-output color-output (vec4 (+ (+ center near) (+ mid far)) 1.0))))

(define-shader-method shader-specification-for
    bloom-vertical-fragment-specification
    ((role (eql :bloom-vertical)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((source-color :texture-2d :set 0 :binding 0
                    :sample-transfer :identity)
      (source-sampler :sampler :set 0 :binding 1)
      (post-state :uniform-block :set 0 :binding 2
                  :members #.*post-uniform-members*)))
  (let* ((span (vec2 0.0 (swizzle bloom-control :y)))
         (near-offset (* span 1.4585))
         (mid-offset (* span 3.4038))
         (far-offset (* span 5.3510))
         (center
           (* (swizzle (sample source-color source-sampler uv-input) :xyz)
              0.1370))
         (near
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input near-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input near-offset)) :xyz))
              0.2393))
         (mid
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input mid-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input mid-offset)) :xyz))
              0.1394))
         (far
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input far-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input far-offset)) :xyz))
              0.0527)))
    (set-output color-output (vec4 (+ (+ center near) (+ mid far)) 1.0))))

;;; Crepuscular rays as a screen-space gather: march the blurred bright image
;;; toward the solar disc and accumulate what is still lit, attenuating with
;;; distance.  Terrain that occludes the sun is dark in the bright image, so
;;; the rays break into beams around leaves and rooftops for free.  The host
;;; fades SUN-SCREEN's weight out as the disc leaves the frame.

(define-shader-method shader-specification-for
    sun-shaft-fragment-specification
    ((role (eql :sun-shafts)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((source-color :texture-2d :set 0 :binding 0
                    :sample-transfer :identity)
      (source-sampler :sampler :set 0 :binding 1)
      (post-state :uniform-block :set 0 :binding 2
                  :members #.*post-uniform-members*)))
  (let* ((sun-uv (swizzle sun-screen :xy))
         (weight (swizzle sun-screen :z))
         (decay (swizzle bloom-control :z))
         (delta (* (- sun-uv uv-input) 0.03125))
         (gathered
           (counted-fold (tap 32.0 total (vec3 0.0 0.0 0.0))
             (+ total
                (* (swizzle
                    (sample source-color source-sampler
                            (+ uv-input (* delta tap)))
                    :xyz)
                   (expt decay tap))))))
    (set-output color-output (vec4 (* gathered (* weight 0.03125)) 1.0))))

(define-shader-method shader-specification-for
    focus-post-fragment-specification
    ((role (eql :focus-post)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((uv-input :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((scene-color :texture-2d :set 0 :binding 0
                   :sample-transfer :identity)
      (scene-sampler :sampler :set 0 :binding 1)
      (scene-depth :depth-texture-2d :set 0 :binding 2)
      (post-state :uniform-block :set 0 :binding 3
                  :members #.*post-uniform-members*)
      (bloom-color :texture-2d :set 0 :binding 4
                   :sample-transfer :identity)
      (shaft-color :texture-2d :set 0 :binding 5
                   :sample-transfer :identity)
      (depth-sampler :sampler :set 0 :binding 6)))
  (let* ((texel (swizzle post-control :xy))
         (presentation-texel (swizzle presentation-control :xy))
         (active (swizzle post-control :z))
         (exposure (swizzle post-control :w))
         (centered (- uv-input (vec2 0.5 0.5)))
         (radial (dot centered centered))
         ;; A lens does not focus every wavelength on the same circle, and the
         ;; error grows with the distance off axis.  Three taps a fraction of
         ;; a texel apart is the whole of it: nothing at the centre of the
         ;; frame, a hair of colour at its corners.
         (dispersion (* centered (* chromatic-aberration (* radial 0.0045))))
         (sharp
           (vec4
            (swizzle
             (sample scene-color scene-sampler (+ uv-input dispersion)) :x)
            (swizzle (sample scene-color scene-sampler uv-input) :y)
            (swizzle
             (sample scene-color scene-sampler (- uv-input dispersion)) :z)
            1.0))
         (depth (swizzle (sample scene-depth depth-sampler uv-input) :x))
         (focus-depth
           (swizzle (sample scene-depth depth-sampler (vec2 0.5 0.5)) :x))
         (blur-amount
           (* active (smoothstep 0.0015 0.018 (- depth focus-depth))))
         (dx (* texel (vec2 5.0 0.0)))
         (dy (* texel (vec2 0.0 5.0)))
         (blurred
           (* (+ (* sharp 4.0)
                 (* (sample scene-color scene-sampler (+ uv-input dx)) 2.0)
                 (* (sample scene-color scene-sampler (- uv-input dx)) 2.0)
                 (* (sample scene-color scene-sampler (+ uv-input dy)) 2.0)
                 (* (sample scene-color scene-sampler (- uv-input dy)) 2.0)
                 (sample scene-color scene-sampler (+ (+ uv-input dx) dy))
                 (sample scene-color scene-sampler (+ (- uv-input dx) dy))
                 (sample scene-color scene-sampler (- (+ uv-input dx) dy))
                 (sample scene-color scene-sampler (- (- uv-input dx) dy)))
              0.0625))
         (radiance (swizzle (mix sharp blurred blur-amount) :xyz))
         ;; The chain already carries exposed units; the scene does not.
         (bloom
           (swizzle (sample bloom-color scene-sampler uv-input) :xyz))
         (shafts
           (swizzle (sample shaft-color scene-sampler uv-input) :xyz))
         (exposed
           (+ (* radiance exposure)
              (* bloom (swizzle lens-control :x))
              (* shafts (swizzle lens-control :y))))
         (graded (aces-filmic exposed))
         ;; The two grading controls a colourist reaches for first, in the
         ;; order they belong in: saturation about the image's own luminance,
         ;; then contrast as a blend toward a smoothstep, which steepens the
         ;; midtones without ever clipping either end the way a gain about a
         ;; pivot would.
         (luminance (dot graded (vec3 0.2126 0.7152 0.0722)))
         (grey (vec3 luminance luminance luminance))
         (saturated (+ grey (* (- graded grey) grade-saturation)))
         (curved
           (* saturated (* saturated (- (vec3 3.0 3.0 3.0)
                                        (* saturated 2.0)))))
         (contrasted (mix saturated curved grade-contrast))
         ;; A restrained corner falloff; the frame should feel photographed,
         ;; not port-holed.
         (vignette
           (- 1.0 (* (swizzle lens-control :z) (smoothstep 0.10 0.75 radial))))
         ;; Eight bits per channel cannot hold a sky gradient: a wide, slowly
         ;; changing surface crosses a quantization step every few dozen
         ;; pixels and the eye reads the step as a contour.  Interleaved
         ;; gradient noise, scaled so it is about half a step wherever the
         ;; image sits on the transfer curve, turns the contour into a grain
         ;; too fine to see.
         (pixel (/ uv-input presentation-texel))
         (dither-phase
           (+ (* (swizzle pixel :x) 0.06711056) (* (swizzle pixel :y)
                                                   0.00583715)))
         (dither (- (fract (* 52.9829189 (fract dither-phase))) 0.5))
         (dither-scale (* 0.0060 (expt (+ luminance 0.0025) 0.55)))
         (presented
           (mix (+ (* contrasted vignette)
                   (vec3 (* dither dither-scale) (* dither dither-scale)
                         (* dither dither-scale)))
                radiance
                ;; The shadow diagnostic is a measurement, not a picture: when
                ;; the scene pass writes raw visibility instead of radiance,
                ;; exposure, the filmic curve, the lens chain, the grade, and
                ;; the vignette would each distort exactly the quantity being
                ;; measured, so presentation hands the value straight through.
                (swizzle sun-screen :w))))
    (set-output color-output (vec4 presented 1.0))))

(defun focus-post-fragment-specification ()
  (shader-specification-for :focus-post :fragment))

(defun focus-post-uniform-block ()
  "The presentation uniform block exactly as the post stage declares it."
  (find-if (lambda (resource) (typep resource 'shader-uniform-block))
           (shader-specification-resources
            (focus-post-fragment-specification))))

;;; The shadow-map pass renders the same block mesh into stored light-space
;;; depth.  The block fragment material samples that product with explicit
;;; percentage-closer filtering and receiver bias above.

(define-shader-method shader-specification-for
    block-world-shadow-vertex-specification
    ((role (eql :block-shadow)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((world-position :vec3 :location 0
                              :quantity :world-position
                              :unit :cell))
     :outputs ((clip-position :vec4 :built-in :position))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((clip
           (project-point :world-to-light world-position
                          shadow-row-x shadow-row-y
                          shadow-row-z shadow-row-w)))
    (set-output clip-position clip)))

(defun block-world-shadow-vertex-specification ()
  (shader-specification-for :block-shadow :vertex))

(defun block-world-shadow-vertex-module ()
  (spv:shader-lowering-module
   (spv:compile-shader-specification (block-world-shadow-vertex-specification))))

(defun block-world-shadow-vertex-shader ()
  (spv:assemble-spir-v-module (block-world-shadow-vertex-module)))

;;; The crosshair is deliberately another tiny mathematical material rather
;;; than a magic fixed-function colour.  Its positions and ink are dense vertex
;;; data, so the shared graph needs no backend-specific vertex-index operation.

(define-shader-method shader-specification-for
    block-world-crosshair-vertex-specification
    ((role (eql :block-crosshair)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((screen-position :vec3 :location 0
                               :quantity :clip-coordinate :unit :one)
              (ink-input :vec3 :location 1
                         :quantity :linear-rgb :unit :one))
     :outputs ((clip-position :vec4 :built-in :position)
               (ink-output :vec3 :location 0
                           :quantity :linear-rgb :unit :one)))
  (let* ((clip
           (vec4 (representation (swizzle screen-position :x))
                 (representation (swizzle screen-position :y))
                 (representation (swizzle screen-position :z))
                 1.0)))
    (set-output clip-position clip)
    (set-output ink-output ink-input)))

(defun block-world-crosshair-vertex-specification ()
  (shader-specification-for :block-crosshair :vertex))

(define-shader-method shader-specification-for
    block-world-crosshair-fragment-specification
    ((role (eql :block-crosshair)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((ink-input :vec3 :location 0
                         :quantity :linear-rgb :unit :one))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((ink ink-input)
         (rgba
           (assume-quantity
            (vec4
             (representation ink)
             (representation
              (quantity 1.0 :quantity :opacity :unit :one)))
            :quantity :linear-rgba :unit :one)))
    (set-output color-output rgba)))

(defun block-world-crosshair-fragment-specification ()
  (shader-specification-for :block-crosshair :fragment))

(defun block-world-crosshair-fragment-module ()
  (spv:shader-lowering-module
   (spv:compile-shader-specification
    (block-world-crosshair-fragment-specification))))

(defun block-world-crosshair-fragment-shader ()
  (spv:assemble-spir-v-module (block-world-crosshair-fragment-module)))

;;; The software cursor is one quad carrying a cursor-local coordinate; the
;;; arrow itself is a signed distance field the fragment stage evaluates.  A
;;; pointer is nearly all long diagonals, so triangle soup stairsteps badly at
;;; any size, and its silhouette, its outline, its rounded corners and its
;;; drop shadow all fall out of thresholds of the one distance.

(defconstant +luvcraft-cursor-corner-radius+ 1.3
  "How far the pointer's convex corners are rounded, in design units.")

(defconstant +luvcraft-cursor-outline-width+ 0.8
  "How wide the pointer's dark border is, in design units.

Chosen against the corner radius rather than on its own: the white body is
the silhouette inset by this much, so its corners keep whatever rounding the
difference between the two leaves them.")

(defun luvcraft-cursor-outline ()
  "The pointer's visible corners, tip first, in its own design grid.

The tip is the hotspot, so it sits at the origin exactly.  Rounding does not
move any of these: a corner's arc is tangent to both its edges and reaches
back out to the corner itself, which is why this table can be read as the
shape you see rather than as a construction for it.

  A  the tip, and the hotspot
  B  the heel, foot of the vertical left edge
  C  where the tail leaves the head
  D  the tail's outer corner
  E  the tail's inner corner
  F  where the tail rejoins the head
  G  foot of the diagonal right edge"
  '((0.0   0.0)
    (0.0  16.4)
    (3.4  13.8)
    (6.3  20.6)
    (10.1 18.9)
    (7.6  12.4)
    (12.4 11.0)))

(defun luvcraft-cursor-extent ()
  "The design-grid rectangle the pointer is drawn inside, as WIDTH and HEIGHT."
  (let ((outline (luvcraft-cursor-outline)))
    (list (reduce #'max outline :key #'first)
          (reduce #'max outline :key #'second))))

(define-shader-abstraction cursor-outline-width ()
  "The pointer's dark border, in design units."
  +luvcraft-cursor-outline-width+)

(define-shader-abstraction cursor-arrow-height ()
  "How far down the design grid the pointer reaches."
  (second (luvcraft-cursor-extent)))

(defun cursor-outline-edge-normal (from to)
  "The outward unit normal of the outline edge running FROM to TO."
  (let* ((ex (- (first to) (first from)))
         (ey (- (second to) (second from)))
         (length (max (sqrt (+ (* ex ex) (* ey ey))) 1e-6)))
    (list (/ (- ey) length) (/ ex length))))

(defun inset-cursor-outline (outline radius)
  "Move every edge of OUTLINE inward by RADIUS and meet the corners again.

Growing the distance to this back by RADIUS restores OUTLINE with each convex
corner replaced by an arc of that radius.  A reflex corner offsets outward
instead and so stays crisp, which is what the notch between head and tail
wants."
  (loop with count = (length outline)
        for index below count
        for corner = (nth index outline)
        for previous = (nth (mod (- index 1) count) outline)
        for next = (nth (mod (+ index 1) count) outline)
        for entering = (cursor-outline-edge-normal previous corner)
        for leaving = (cursor-outline-edge-normal corner next)
        for determinant = (- (* (first entering) (second leaving))
                             (* (second entering) (first leaving)))
        collect
        (if (< (abs determinant) 1e-6)
            ;; A straight-through corner has no bisector to solve for; both
            ;; edges want the same offset, so take it directly.
            (list (- (first corner) (* radius (first leaving)))
                  (- (second corner) (* radius (second leaving))))
            (list (+ (first corner)
                     (/ (* (- radius) (- (second leaving) (second entering)))
                        determinant))
                  (+ (second corner)
                     (/ (* (- radius) (- (first entering) (first leaving)))
                        determinant))))))

(defun cursor-outline-distance-form (point corners)
  "Source for the exact signed distance from POINT to the polygon CORNERS.

The pointer's notch is reflex, so the distance is taken the general way --
nearest point over every edge, sign from a crossing count -- rather than by
intersecting half planes, which would quietly shave the notch off."
  (flet ((edges (operator)
           (loop with count = (length corners)
                 for index below count
                 for corner = (nth index corners)
                 for previous = (nth (mod (- index 1) count) corners)
                 collect `(,operator ,point
                                     ,(first corner) ,(second corner)
                                     ,(first previous) ,(second previous)))))
    `(* (* ,@(edges 'cursor-edge-crossing))
        (sqrt (max (min ,@(edges 'cursor-edge-squared-distance)) 1e-12)))))

(define-shader-function cursor-edge-squared-distance (point px py qx qy)
  "Squared distance from POINT to the outline edge between P and Q."
  (let* ((ex (- qx px))
         (ey (- qy py))
         (wx (- (swizzle point :x) px))
         (wy (- (swizzle point :y) py))
         (along
           (clamp (/ (+ (* wx ex) (* wy ey))
                     (max (+ (* ex ex) (* ey ey)) 1e-6))
                  0.0 1.0))
         (bx (- wx (* ex along)))
         (by (- wy (* ey along))))
    (+ (* bx bx) (* by by))))

(define-shader-function cursor-edge-crossing (point px py qx qy)
  "-1 when a ray from POINT crosses the outline edge between P and Q, else 1.

Multiplying one of these per edge counts the crossings, so the product is
negative exactly inside the outline however it winds."
  (let* ((ex (- qx px))
         (ey (- qy py))
         (wx (- (swizzle point :x) px))
         (wy (- (swizzle point :y) py))
         (past-p (step py (swizzle point :y)))
         (before-q (- 1.0 (step qy (swizzle point :y))))
         (left-of (step (* ey wx) (* ex wy)))
         (crossing
           (+ (* past-p before-q left-of)
              (* (- 1.0 past-p) (- 1.0 before-q) (- 1.0 left-of)))))
    (- 1.0 (* 2.0 crossing))))

(define-shader-abstraction cursor-arrow-distance (point)
  "Signed distance from POINT to the pointer, negative inside.

The corners are rounded off by +LUVCRAFT-CURSOR-CORNER-RADIUS+, which is what
keeps the heel, the shoulder and the tail from reading as the barbs of a fir
tree, and what leaves the white body still curved once its dark outline has
disappeared into a dark background."
  (let ((radius +luvcraft-cursor-corner-radius+))
    `(- ,(cursor-outline-distance-form
          point (inset-cursor-outline (luvcraft-cursor-outline) radius))
        ,radius)))

(define-shader-method shader-specification-for
    luvcraft-cursor-vertex-specification
    ((role (eql :cursor)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((screen-position :vec3 :location 0
                               :quantity :clip-coordinate :unit :one)
              (cursor-input :vec2 :location 1
                            :quantity :cursor-coordinate :unit :one))
     :outputs ((clip-position :vec4 :built-in :position)
               (cursor-output :vec2 :location 0)))
  (let* ((clip
           (vec4 (representation (swizzle screen-position :x))
                 (representation (swizzle screen-position :y))
                 (representation (swizzle screen-position :z))
                 1.0)))
    (set-output clip-position clip)
    (set-output cursor-output (representation cursor-input))))

(define-shader-method shader-specification-for
    luvcraft-cursor-fragment-specification
    ((role (eql :cursor)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((cursor-input :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0)))
  (let* (;; One pixel measured in the cursor's own units, so the same source
         ;; resolves its edges wherever the arrow is scaled to.
         (dx (derivative-x (swizzle cursor-input :x)))
         (dy (derivative-y (swizzle cursor-input :x)))
         (pixel (max (sqrt (+ (* dx dx) (* dy dy))) 1e-4))
         (distance (cursor-arrow-distance cursor-input))
         ;; A soft shadow down and to the right lifts the arrow off bright sky
         ;; and pale inventory panels alike without a second silhouette.
         (shadow-point (- cursor-input (vec2 1.1 1.6)))
         (shadow-distance (cursor-arrow-distance shadow-point))
         (shadow (* 0.40 (- 1.0 (smoothstep -0.4 2.8 shadow-distance))))
         ;; The silhouette, and the white body inset from it by the outline.
         ;; The body keeps whatever rounding the outline width leaves it, so
         ;; the two widths are chosen together.
         (body (- 1.0 (smoothstep (- pixel) pixel distance)))
         (core
           (- 1.0
              (smoothstep (- pixel) pixel
                          (+ distance (cursor-outline-width)))))
         (outline-ink (vec3 0.045 0.050 0.060))
         ;; The body is not flat white but carries a little light along its
         ;; length: warm and bright at the tip, where the eye is meant to go,
         ;; cooling as it falls back into the tail.
         (sheen
           (clamp (/ (swizzle cursor-input :y) (cursor-arrow-height)) 0.0 1.0))
         (body-ink
           (mix (vec3 0.985 0.976 0.952) (vec3 0.855 0.868 0.900) sheen))
         (ink (mix outline-ink body-ink core))
         ;; Premultiplied: the arrow covers its own share and the shadow only
         ;; darkens whatever the arrow itself left uncovered.
         (alpha (+ body (* shadow (- 1.0 body))))
         (rgba
           (assume-quantity
            (vec4 (* ink body) alpha)
            :quantity :linear-rgba :unit :one)))
    (set-output color-output rgba)))

;;; ---------------------------------------------------------------------
;;; The physics bodies as spheres, drawn the honest way.
;;;
;;; Every ball, marble, drop, and gobbet is really a sphere in the physics,
;;; and here it finally looks like one: each body is a camera-facing quad
;;; whose fragments send an exact ray at the analytic sphere -- one closed
;;; quadratic, no marching -- and shade the true surface point with the true
;;; normal.  The instance stream carries the body's centre and radius, its
;;; orientation quaternion, an albedo and emission the CPU resolved from the
;;; body-kind palette, and the sampled voxel light levels.  Patterns are
;;; painted in the body's own frame: the fragment carries the world normal
;;; back through the quaternion's conjugate, so a ball's band, a beach
;;; ball's panels, and a smiley's face all tumble as the body rolls.
;;;
;;; The lowering has no fragment-depth output, so the quad cannot write the
;;; sphere's own depth.  Instead the vertex stage slides the proxy toward
;;; the eye by one radius: its depth is then conservatively near for every
;;; point on the sphere, and a ball resting in a hollow is not eaten by the
;;; terrain in front of its centre plane.  The draw itself tests depth
;;; without writing it and blends premultiplied, with the CPU handing over
;;; instances farthest first.

(define-shader-function physics-sphere-length (vector)
  "Euclidean length, floored away from the origin's derivative singularity."
  (sqrt (max (dot vector vector) 1e-12)))

(define-shader-function physics-sphere-cross (a b)
  "The cross product, spelled out; the shader language keeps no such operator."
  (vec3 (- (* (swizzle a :y) (swizzle b :z)) (* (swizzle a :z) (swizzle b :y)))
        (- (* (swizzle a :z) (swizzle b :x)) (* (swizzle a :x) (swizzle b :z)))
        (- (* (swizzle a :x) (swizzle b :y)) (* (swizzle a :y) (swizzle b :x)))))

(define-shader-function physics-sphere-unspin (vector spin)
  "VECTOR carried into the body's own frame: a turn by SPIN's conjugate.

The physics keeps a unit quaternion (x y z w) purely for the eye.  A pattern
painted on the body's own axes therefore visibly turns with the body when
the world-space surface normal is unspun through the quaternion before the
pattern reads it."
  (let* ((axis (vec3 (- (swizzle spin :x))
                     (- (swizzle spin :y))
                     (- (swizzle spin :z))))
         (turn (* (physics-sphere-cross axis vector) 2.0)))
    (+ vector (+ (* turn (swizzle spin :w))
                 (physics-sphere-cross axis turn)))))

(define-shader-function physics-sphere-band-color (albedo local)
  "The classic ball's pale band: a latitude ring in the pattern frame,
so it sweeps visibly over the ball as it rolls."
  (let* ((band (- 1.0 (smoothstep 0.10 0.22 (abs (swizzle local :y))))))
    (mix albedo (vec3 0.89 0.84 0.75) (* band 0.85))))

(define-shader-function physics-sphere-marble-color (albedo local)
  "Glass with a vein through it: value noise in the pattern frame."
  (let* ((vein (smoothstep 0.42 0.72 (lattice-noise (* local 2.6)))))
    (mix albedo (vec3 0.94 0.97 1.0) (* vein 0.55))))

(define-shader-function physics-sphere-beach-color (local)
  "Six panels meeting at the poles, without an arc tangent.

Three half-planes at sixty degrees classify the pattern-frame longitude:
one sector satisfies exactly one of them and its neighbour exactly two, so
the count alternates white with colour around the ball, and which pair
holds names the colour.  A small white cap covers each pole, where every
panel converges the way a real beach ball's cap does."
  (let* ((x (swizzle local :x))
         (z (swizzle local :z))
         (s0 (step 0.0 x))
         (s1 (step 0.0 (+ (* -0.5 x) (* 0.8660254 z))))
         (s2 (step 0.0 (- (* -0.5 x) (* 0.8660254 z))))
         (white (clamp (- 2.0 (+ s0 (+ s1 s2))) 0.0 1.0))
         (red (* s0 (* s1 (- 1.0 s2))))
         (yellow (* (- 1.0 s0) (* s1 s2)))
         (blue (* s0 (* (- 1.0 s1) s2)))
         (panel (+ (* (vec3 0.97 0.95 0.92) white)
                   (+ (* (vec3 0.92 0.16 0.13) red)
                      (+ (* (vec3 0.99 0.80 0.12) yellow)
                         (* (vec3 0.16 0.36 0.90) blue)))))
         (cap (smoothstep 0.92 0.97 (abs (swizzle local :y)))))
    (mix panel (vec3 0.97 0.95 0.92) cap)))

(define-shader-function physics-sphere-smiley-color (albedo local)
  "Two eyes and a smile on the pattern frame's +z half, so the face tumbles.

The features are placed by direction from the centre, the same trick the
gnome's face uses: a direction does not care what radius the ball was drawn
at.  The smile is the lower arc of a ring around a point above the mouth,
masked to the face's lower half."
  (let* ((mirrored (vec3 (abs (swizzle local :x))
                         (swizzle local :y)
                         (swizzle local :z)))
         (eye (- 1.0 (smoothstep 0.13 0.19
                                 (physics-sphere-length
                                  (- mirrored (vec3 0.30 0.32 0.90))))))
         (ring (physics-sphere-length (- local (vec3 0.0 0.35 0.94))))
         (smile (* (- 1.0 (smoothstep 0.05 0.10 (abs (- ring 0.62))))
                   (smoothstep 0.02 0.12 (- 0.0 (swizzle local :y)))))
         (ink (clamp (+ eye smile) 0.0 1.0)))
    (mix albedo (vec3 0.16 0.10 0.05) ink)))

(define-shader-method shader-specification-for
    physics-sphere-vertex-specification
    ((role (eql :physics-sphere)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (sphere-instance :vec4 :location 1)
              (spin-instance :vec4 :location 2)
              (tint-instance :vec4 :location 3)
              (shine-instance :vec4 :location 4))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position :vec3 :location 0)
               (sphere-output :vec4 :location 1)
               (spin-output :vec4 :location 2)
               (tint-output :vec4 :location 3)
               (light-fog-output :vec4 :location 4))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((center (swizzle sphere-instance :xyz))
         (radius (max (swizzle sphere-instance :w) 1e-4))
         (camera (representation (swizzle camera-vector :xyz)))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         (corner-x (- (* (swizzle quad-corner :x) 2.0) 1.0))
         (corner-y (- (* (swizzle quad-corner :y) 2.0) 1.0))
         (to-camera (- camera center))
         (camera-distance (physics-sphere-length to-camera))
         ;; The proxy plane slides toward the eye by one radius, stopping
         ;; short of the eye itself: its interpolated depth is then nearer
         ;; than any point of the sphere, so the depth test against terrain
         ;; can only err by letting a hidden ball show through a sliver, not
         ;; by eating a resting ball at its contact circle.
         (slide (min radius (max 0.0 (- camera-distance 0.25))))
         (proxy-center (+ center (* (/ to-camera camera-distance) slide)))
         ;; Exactly the half-extent the silhouette needs from the slid
         ;; plane, capped so a ball against the eye stays finite.
         (silhouette-run
           (sqrt (max (- (* camera-distance camera-distance)
                         (* radius radius))
                      0.001)))
         (half-extent
           (min (* radius 4.0)
                (* radius (/ (- camera-distance slide) silhouette-run))))
         (world-position
           (+ proxy-center (+ (* right (* corner-x half-extent))
                              (* up (* corner-y half-extent)))))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward))
         (x-scale (representation (swizzle projection-vector :x)))
         (y-scale (representation (swizzle projection-vector :y)))
         (z-scale (representation (swizzle projection-vector :z)))
         (z-offset (representation (swizzle projection-vector :w)))
         ;; Fog is measured once at the centre; a body is small enough to
         ;; take one answer, and the quadratic law matches the terrain's.
         (center-view-z (dot (- center camera) forward))
         (fog-near (representation (swizzle fog-vector :x)))
         (fog-far (representation (swizzle fog-vector :y)))
         (fog-progress
           (clamp (/ (- center-view-z fog-near)
                     (max 1.0 (- fog-far fog-near)))
                  0.0 1.0))
         (fog-amount (* fog-progress fog-progress)))
    (set-output clip-position
                (vec4 (* view-x x-scale)
                      (- (* view-y y-scale))
                      (+ (* view-z z-scale) z-offset)
                      view-z))
    (set-output proxy-world-position world-position)
    (set-output sphere-output sphere-instance)
    (set-output spin-output spin-instance)
    (set-output tint-output tint-instance)
    (set-output light-fog-output
                (vec4 (swizzle shine-instance :x)
                      (swizzle shine-instance :y)
                      (swizzle shine-instance :z)
                      fog-amount))))

(define-shader-method shader-specification-for
    physics-sphere-fragment-specification
    ((role (eql :physics-sphere)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0)
              (sphere-input :vec4 :location 1)
              (spin-input :vec4 :location 2)
              (tint-input :vec4 :location 3)
              (light-fog-input :vec4 :location 4))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (center (swizzle sphere-input :xyz))
         (radius (max (swizzle sphere-input :w) 1e-4))
         (ray (normalize (- proxy-world-position camera)))
         ;; The closed-form ray/sphere meeting: no march for a quadric.
         (to-center (- camera center))
         (half-b (dot to-center ray))
         (gap (- (dot to-center to-center) (* radius radius)))
         (discriminant (- (* half-b half-b) gap))
         (coverage (step 0.0 discriminant))
         (span (sqrt (max discriminant 0.0)))
         (travel (max (- (- half-b) span) 0.0))
         (point (+ camera (* ray travel)))
         (normal (normalize (- point center)))
         ;; The pattern frame: the normal unspun by the body's quaternion.
         (local (physics-sphere-unspin normal spin-input))
         (pattern (swizzle light-fog-input :z))
         (base (swizzle tint-input :xyz))
         (is-band (* (step 0.5 pattern) (step pattern 1.5)))
         (is-marble (* (step 1.5 pattern) (step pattern 2.5)))
         (is-beach (* (step 2.5 pattern) (step pattern 3.5)))
         (is-smiley (step 3.5 pattern))
         (albedo
           (mix (mix (mix (mix base
                               (physics-sphere-band-color base local)
                               is-band)
                          (physics-sphere-marble-color base local)
                          is-marble)
                     (physics-sphere-beach-color local)
                     is-beach)
                (physics-sphere-smiley-color base local)
                is-smiley))
         ;; The block world's light, in miniature: the sampled voxel levels
         ;; response-curved the same way, the same sun gate, the same dome
         ;; and torch colours, a wrapped diffuse in place of the flat one.
         (sky-input (swizzle light-fog-input :x))
         (block-input (swizzle light-fog-input :y))
         (sky-level (* sky-input sky-input))
         (block-level (* block-input block-input))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (day-factor (representation (swizzle sun-vector :w)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (sun-visibility (smoothstep 0.90 1.0 sky-input))
         (lambert (dot normal sun-direction))
         (wrapped (max 0.0 (/ (+ lambert 0.30) 1.30)))
         (facing-up (* 0.5 (+ 1.0 (swizzle normal :y))))
         (dome (mix (representation (swizzle horizon-vector :xyz))
                    (representation (swizzle zenith-vector :xyz))
                    (* facing-up facing-up)))
         (bounce (* (representation (swizzle fog-color-vector :xyz)) 0.9))
         (environment (mix bounce (mix ambient dome 0.62) facing-up))
         (sky-light (* environment (+ 0.030 (* 0.86 sky-level))))
         (sun-light
           (* sun-color
              (* direct-light-gain
                 (* wrapped (* sun-visibility day-factor)))))
         (torch (* (vec3 1.0 0.82 0.58) block-level))
         (emission (swizzle tint-input :w))
         ;; One soft lobe for rubber, a tight bright one for glass, and a
         ;; glassy rim only the marble wears.
         (halfway (normalize (- sun-direction ray)))
         (gloss (+ 18.0 (* 46.0 is-marble)))
         (shine (+ 0.16 (* 0.40 is-marble)))
         (specular
           (* sun-color
              (* shine
                 (* (expt (max 0.0 (dot normal halfway)) gloss)
                    (* sun-visibility
                       (* day-factor (step 0.0 lambert)))))))
         (view-facing (max 0.0 (dot normal (* ray -1.0))))
         (rim (* is-marble (* 0.22 (expt (- 1.0 view-facing) 3.0))))
         (radiance
           (+ (* albedo (+ sky-light (+ sun-light torch)))
              (+ specular
                 (+ (vec3 rim rim rim) (* albedo emission)))))
         ;; The same aerial perspective the terrain fades into.
         (sun-elevation (swizzle sun-direction :y))
         (low-sun (* day-factor (- 1.0 (smoothstep 0.02 0.45 sun-elevation))))
         (fog-color
           (aerial-perspective-color
            (representation (swizzle fog-color-vector :xyz))
            ray sun-direction low-sun day-factor))
         (fogged (mix radiance fog-color (swizzle light-fog-input :w))))
    ;; Premultiplied, so a missed fragment leaves no trace of the quad.
    (set-output color-output (vec4 (* fogged coverage) coverage))))
