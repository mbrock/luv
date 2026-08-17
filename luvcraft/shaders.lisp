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
      (fog-vector :vec4         ; fog near, fog far, unused, unused
                  :components
                  ((:x :quantity :view-distance :unit :cell)
                   (:y :quantity :view-distance :unit :cell)))
      (sun-vector :vec4         ; sun direction, day factor
                  :components
                  ((:xyz :quantity :world-direction :unit :one)
                   (:w :quantity :day-factor :unit :one)))
      (sun-color-vector :vec4   ; sun colour, angular width
                        :components
                        ((:xyz :quantity :linear-rgb :unit :one)
                         (:w :quantity :sun-disc-coordinate :unit :one)))
      (zenith-vector :vec4      ; zenith colour, w unused
                     :components
                     ((:xyz :quantity :linear-rgb :unit :one)))
      (horizon-vector :vec4     ; horizon colour, w unused
                      :components
                      ((:xyz :quantity :linear-rgb :unit :one)))
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
              (uv-shade-input :vec3 :location 1
                              :components
                              ((:xy :quantity :texture-uv :unit :one)
                               (:z :quantity :ambient-occlusion :unit :one)))
              (normal-input :vec3 :location 2
                            :quantity :world-direction :unit :one)
              (light-input :vec3 :location 3
                           :components
                           ((:x :quantity :sky-light-level :unit :one)
                            (:y :quantity :block-light-level :unit :one)
                            (:z :quantity :material-emission :unit :one))))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv-shade-output :vec3 :location 0
                                :components
                                ((:xy :quantity :texture-uv :unit :one)
                                 (:z :quantity :ambient-occlusion :unit :one)))
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
                                    :unit :one))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (swizzle camera-vector :xyz))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (interpret (dot relative forward)
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
         (shadow-depth (swizzle shadow-projection :z)))
    (set-output clip-position clip)
    (set-output uv-shade-output uv-shade-input)
    (set-output normal-output normal-input)
    (set-output fog-output fog-amount)
    (set-output light-output light-input)
    (set-output shadow-uv-output (swizzle shadow-projection :xy))
    (set-output shadow-depth-output shadow-depth)))

(defun block-world-vertex-specification ()
  (shader-specification-for :block-surface :vertex))

(defun block-world-camera-uniform-block ()
  "The frame uniform block exactly as the vertex specification declares it."
  (find-if (lambda (resource) (typep resource 'shader-uniform-block))
           (shader-specification-resources
            (block-world-vertex-specification))))

(defun block-world-vertex-lowering ()
  "Compile the camera transform and retain expression-to-SSA provenance."
  (compile-shader-specification (block-world-vertex-specification)))

(defun block-world-vertex-module ()
  (shader-lowering-module (block-world-vertex-lowering)))

(defun block-world-vertex-shader ()
  (assemble-spir-v-module (block-world-vertex-module)))

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
  (let* ((world-position
           (assume-quantity
            (+ world-origin
               (* world-right-edge (swizzle quad-corner :x))
               (* world-up-edge (swizzle quad-corner :y)))
            :quantity :world-position :unit :cell))
         (outline-coordinate
           (+ (swizzle outline-low :xy)
              (* (- (swizzle outline-high :xy)
                    (swizzle outline-low :xy))
                 (swizzle quad-corner :xy))))
         (camera (swizzle camera-vector :xyz))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (interpret (dot relative forward)
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
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (interpret (dot relative forward)
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

;;; The terminal screen panel is one analytic world rectangle spanning the
;;; whole display surface, drawn beneath the cell backgrounds and glyphs.  Its
;;; material is the "magic television" glass of #7ZM22R rendered as a
;;; view-dependent surface rather than an atlas tile: dark glass with a
;;; Fresnel sky reflection and a sun glint, a bezel band with a lit inner
;;; lip, and an inset-style vignette; a faint phosphor floor keeps the screen
;;; readable as a screen at night.  It shares the cell run's instance layout,
;;; with the fourth vector carrying the outward face normal.

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
               (render-normal :vec3 :location 3))
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
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (interpret (dot relative forward)
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
    (set-output render-normal normal-input)))

(define-shader-method shader-specification-for
    terminal-screen-fragment-specification
    ((role (eql :terminal-screen)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size :vec2 :location 1)
              (world-position :vec3 :location 2)
              (normal-input :vec3 :location 3))
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
         (n-dot-l (max 0.0 (dot normal sun-direction)))
         ;; Schlick Fresnel for coated display glass: a low floor rising
         ;; toward grazing, so a frontal view stays black and an oblique one
         ;; picks up the sky.
         (fresnel (+ 0.018 (* 0.982 (expt (- 1.0 n-dot-v) 5.0))))
         ;; The mirrored view ray samples the same two-colour sky gradient
         ;; the sky dome draws, so the glass reflects the day it stands in.
         (reflected (- (* normal (* 2.0 (dot normal view))) view))
         (sky-height (smoothstep -0.04 0.45 (swizzle reflected :y)))
         (sky-reflection (* (mix horizon zenith sky-height) (* fresnel 0.8)))
         ;; A tight glint plus a broad sheen from the sun; both are held
         ;; back at normal incidence so the screen reads as glass, not chrome.
         (half-vector (normalize (+ sun-direction view)))
         (n-dot-h (max 0.0 (dot normal half-vector)))
         (glint (* (expt n-dot-h 160.0) 2.0))
         (sheen (* (expt n-dot-h 20.0) 0.12))
         (specular
           (* sun-color (* (+ glint sheen) day-factor (+ 0.3 fresnel))))
         ;; Rectangle geometry: distance to the panel edge in world cells.
         (edge-distance
           (min (- (swizzle half-size :x) (abs (swizzle render-coordinate :x)))
                (- (swizzle half-size :y) (abs (swizzle render-coordinate :y)))))
         (bezel-width 0.11)
         (screen-mask (smoothstep bezel-width (+ bezel-width 0.012) edge-distance))
         ;; The screen sits behind the bezel: shade falls in from the lip.
         (inset-shade (smoothstep 0.0 0.45 (- edge-distance bezel-width)))
         (vignette (mix 0.30 1.0 inset-shade))
         (irradiance (+ ambient (* sun-color (* n-dot-l day-factor))))
         (glass-albedo (vec3 0.008 0.009 0.011))
         (phosphor (vec3 0.004 0.009 0.007))
         (screen-color
           (* (+ (* glass-albedo irradiance) phosphor sky-reflection specular)
              vignette))
         ;; The bezel is a matte dark frame with a lit inner lip.
         (lip (smoothstep (- bezel-width 0.03) bezel-width edge-distance))
         (bezel-albedo (mix (vec3 0.02 0.02 0.022) (vec3 0.09 0.09 0.10) lip))
         (bezel-color (+ (* bezel-albedo irradiance) (* specular 0.15)))
         (rgb (mix bezel-color screen-color screen-mask)))
    (set-output color-output (vec4 rgb 1.0))))

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
                          :quantity :shadow-depth :unit :one))
     :outputs ((color-output :vec4 :location 0))
     :resources ((block-atlas :texture-2d :set 0 :binding 0
                              :sample-transfer :srgb-to-linear
                              :sample-components
                              ((:rgb :quantity :linear-rgb :unit :one)
                               (:a :quantity :opacity :unit :one)))
                 (block-sampler :sampler :set 0 :binding 1)
                 (frame-state :uniform-block :set 0 :binding 2
                              :members #.*frame-uniform-members*)
                 (shadow-map :depth-texture-2d :set 0 :binding 3
                             :sample-components
                             ((:x :quantity :shadow-depth :unit :one)))
                 (shadow-sampler :sampler :set 0 :binding 4)
                 (shadow-comparison-sampler :sampler :set 0 :binding 5)))
  (let* ((uv-shade uv-shade-input)
         (uv (swizzle uv-shade :xy))
         (ao (swizzle uv-shade :z))
         (normal normal-input)
         (sun-direction (swizzle sun-vector :xyz))
         (n-dot-l (max 0.0 (dot normal sun-direction)))
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
         (direct-shadow (mix 1.0 shadow-sample shadow-in-bounds))
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
         ;; A small floor keeps unlit geometry barely readable rather than
         ;; a void; caves stay dark for the right reason.
         (sky-light
           (interpret (* ambient (+ 0.06 (* 1.34 sky-level)) ao)
                      :quantity :linear-rgb :unit :one))
         (sun-light
           (interpret
            (* sun-color
               (* n-dot-l sun-visibility day-factor direct-shadow))
            :quantity :linear-rgb :unit :one))
         (torch-color
           (quantity (vec3 1.0 0.82 0.58)
                     :quantity :linear-rgb :unit :one))
         (local-light
           (interpret (* torch-color block-level)
                      :quantity :linear-rgb :unit :one))
         (albedo
           (swizzle (sample block-atlas block-sampler uv) :rgb))
         (reflected
           (interpret
            (* albedo (+ sky-light sun-light local-light))
            :quantity :linear-rgb :unit :one))
         (radiance
           (+ reflected
              (interpret (* albedo emission-input)
                         :quantity :linear-rgb :unit :one)))
         (fog-color (swizzle fog-color-vector :xyz))
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
  (compile-shader-specification (block-world-fragment-specification)))

(defun block-world-fragment-module ()
  "Lower the readable block material to luv's structured SPIR-V module."
  (shader-lowering-module (block-world-fragment-lowering)))

(defun block-world-fragment-shader ()
  (assemble-spir-v-module (block-world-fragment-module)))

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
  (shader-lowering-module
   (compile-shader-specification (block-world-sky-vertex-specification))))

(defun block-world-sky-vertex-shader ()
  (assemble-spir-v-module (block-world-sky-vertex-module)))

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
  (let* ((unit (normalize ray-input))
         (elevation (swizzle unit :y))
         (zenith (swizzle zenith-vector :xyz))
         (horizon (swizzle horizon-vector :xyz))
         ;; A soft vertical gradient with a wide horizon band; the band sits
         ;; slightly below level so distant terrain meets the fog colour.
         (height
           (smoothstep
            (quantity -0.04 :quantity :world-y-direction :unit :one)
            (quantity 0.45 :quantity :world-y-direction :unit :one)
            elevation))
         (base (mix horizon zenith height))
         (sun-direction (swizzle sun-vector :xyz))
         (day-factor (swizzle sun-vector :w))
         (sun-color (swizzle sun-color-vector :xyz))
         (sun-width (swizzle sun-color-vector :w))
         (alignment
           (interpret (max 0.0 (dot unit sun-direction))
                      :quantity :sun-disc-coordinate :unit :one))
         (disc-outer
           (- (quantity 1.0 :quantity :sun-disc-coordinate :unit :one)
              (* sun-width 3.0)))
         (disc-inner
           (- (quantity 1.0 :quantity :sun-disc-coordinate :unit :one)
              sun-width))
         (disc (smoothstep disc-outer disc-inner alignment))
         (glow (* (expt alignment 24.0) 0.35))
         (sun-radiance
           (interpret (* sun-color (+ disc glow) day-factor)
                      :quantity :linear-rgb :unit :one))
         (rgb (+ base sun-radiance))
         (rgba
           (assume-quantity
            (vec4
             (representation rgb)
             (representation
              (quantity 1.0 :quantity :opacity :unit :one)))
            :quantity :linear-rgba :unit :one)))
    (set-output color-output rgba)))

(defun block-world-sky-fragment-specification ()
  (shader-specification-for :sky :fragment))

(defun block-world-sky-fragment-module ()
  (shader-lowering-module
   (compile-shader-specification (block-world-sky-fragment-specification))))

(defun block-world-sky-fragment-shader ()
  (assemble-spir-v-module (block-world-sky-fragment-module)))

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
    '((post-control :vec4)   ; texel u, texel v, focus blur, exposure
      (lens-control :vec4)   ; bloom gain, shaft gain, vignette, grain
      (sun-screen :vec4))    ; sun screen u, v, on-screen weight, aspect
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
                  :members #.*post-uniform-members*)))
  (let* ((texel (swizzle post-control :xy))
         (active (swizzle post-control :z))
         (exposure (swizzle post-control :w))
         (sharp (sample scene-color scene-sampler uv-input))
         (depth (swizzle (sample scene-depth scene-sampler uv-input) :x))
         (focus-depth
           (swizzle (sample scene-depth scene-sampler (vec2 0.5 0.5)) :x))
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
         (exposed (* radiance exposure))
         (graded (aces-filmic exposed))
         ;; A restrained corner falloff; the frame should feel photographed,
         ;; not port-holed.
         (centered (- uv-input (vec2 0.5 0.5)))
         (vignette
           (- 1.0
              (* (swizzle lens-control :z)
                 (smoothstep 0.10 0.75 (dot centered centered))))))
    (set-output color-output (vec4 (* graded vignette) 1.0))))

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
  (shader-lowering-module
   (compile-shader-specification (block-world-shadow-vertex-specification))))

(defun block-world-shadow-vertex-shader ()
  (assemble-spir-v-module (block-world-shadow-vertex-module)))

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
  (shader-lowering-module
   (compile-shader-specification
    (block-world-crosshair-fragment-specification))))

(defun block-world-crosshair-fragment-shader ()
  (assemble-spir-v-module (block-world-crosshair-fragment-module)))


(defun block-world-crosshair-vertex-module ()
  "Pass screen-space crosshair vertices and their ink to the fragment stage."
  (make-instance
   'spir-v-module
   :entry-points
   (list (make-instance
          'spir-v-entry-point :execution-model 'vertex
          :function '%main
          :interfaces '(%screen-position %ink %position %ink-output)))
   :annotations
   '((decorate %screen-position location 0)
     (decorate %ink location 1)
     (decorate %position built-in (enum built-in position))
     (decorate %ink-output location 0))
   :global-declarations
   '((%void type-void)
     (%float type-float 32)
     (%vec3 type-vector %float 3)
     (%vec4 type-vector %float 4)
     (%input-vec3-pointer type-pointer input %vec3)
     (%output-vec3-pointer type-pointer output %vec3)
     (%output-vec4-pointer type-pointer output %vec4)
     (%function-type type-function %void)
     (%one constant %float 1.0)
     (%screen-position variable %input-vec3-pointer input)
     (%ink variable %input-vec3-pointer input)
     (%position variable %output-vec4-pointer output)
     (%ink-output variable %output-vec3-pointer output))
   :function-definitions
   (list
    (make-instance
     'spir-v-function-definition
     :result-id '%main :return-type '%void
     :function-type '%function-type
     :basic-blocks
     (list
      (make-instance
       'spir-v-basic-block :label '%entry
       :instructions
       '((%screen load %vec3 %screen-position)
         (%ink-value load %vec3 %ink)
         (%screen-x composite-extract %float %screen 0)
         (%screen-y composite-extract %float %screen 1)
         (%screen-z composite-extract %float %screen 2)
         (%clip-position composite-construct
                         %vec4 %screen-x %screen-y %screen-z %one)
         (store %position %clip-position)
         (store %ink-output %ink-value)
         (return))))))))

(defun block-world-crosshair-vertex-shader ()
  (assemble-spir-v-module (block-world-crosshair-vertex-module)))
