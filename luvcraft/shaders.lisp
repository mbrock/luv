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
                            (:z :quantity :material-emission :unit :one)))
              ;; What the mesher knows about this face's four in-plane edges
              ;; and the fragment stage cannot see: concave, flush, or convex.
              (edge-shaping-input :vec4 :location 4
                                  :quantity :edge-shaping :unit :one))
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
                                    :unit :one)
               ;; The fragment stage shapes its own normal from where the
               ;; fragment sits inside its cell, so it needs the world point
               ;; rather than only the face's flat normal.
               (world-position-output :vec3 :location 6
                                      :quantity :world-position
                                      :unit :cell)
               (edge-shaping-output :vec4 :location 7
                                    :quantity :edge-shaping :unit :one))
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
    (set-output shadow-depth-output shadow-depth)
    (set-output world-position-output world-position)
    (set-output edge-shaping-output edge-shaping-input)))

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
         (bulge 0.40)
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
         (line-pitch (/ (* 2.0 half-y) 240.0))
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
         (scanline (* 0.20 raster-fade (- 1.0 line-profile)))
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
           (clamp (+ scanline corner hum flicker speckle) 0.0 0.6))
         (emission (+ sky-reflection room-reflection glint sheen haze)))
    (set-output color-output
                (vec4 (* emission screen-mask) (* attenuation screen-mask)))))

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
      (edge-shaping-input :vec4 :location 7
                          :quantity :edge-shaping :unit :one))
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
         (edge (representation edge-shaping-input))
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
         (shaped
           (normalize (+ (* rounded relief-z)
                         (* tangent-u relief-x)
                         (* tangent-v relief-y))))
         (shading-normal
           (assume-quantity shaped :quantity :world-direction :unit :one))
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
         ;; Occlusion bites harder than the raw mesh reading: the corner where
         ;; three blocks meet is what tells the eye these are solid volumes.
         (occlusion
           (interpret
            (* (expt ao 1.7)
               (assume-quantity seam-occlusion
                                :quantity :ambient-occlusion :unit :one))
            :quantity :ambient-occlusion :unit :one))
         ;; A small floor keeps unlit geometry barely readable rather than
         ;; a void; caves stay dark for the right reason.  The ambient term
         ;; is deliberately weaker, and the sun correspondingly stronger,
         ;; than a display-referred renderer could afford: the filmic curve
         ;; on presentation is what brings the sunlit half back down.
         (sky-light
           (interpret (* ambient (+ 0.030 (* 0.86 sky-level)) occlusion)
                      :quantity :linear-rgb :unit :one))
         (sun-light
           (interpret
            (* sun-color
               (* 2.35 n-dot-l sun-visibility day-factor direct-shadow
                  (mix 0.55 1.0 ao)))
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
         ;; One specular lobe off the same shaped normal.  Blocks are matte,
         ;; so it is a narrow Fresnel-weighted highlight that mostly shows at
         ;; grazing angles: wet-looking stone at a distance, a glint on a
         ;; bevel up close, and nothing at all on a face seen head on.
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
         (fresnel (+ 0.030 (* 0.22 (expt (- 1.0 n-dot-v) 5.0))))
         ;; The cosine belongs in the specular lobe as much as in the diffuse
         ;; one: without it a highlight can appear on a face the sun cannot
         ;; reach, carrying whatever the shadow map happened to say there.
         (specular
           (interpret
            (* sun-color
               (* 4.5 (expt n-dot-h 26.0) fresnel n-dot-l
                  sun-visibility day-factor direct-shadow occlusion))
            :quantity :linear-rgb :unit :one))
         (radiance
           (+ reflected specular
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

;;; Lattice value noise is the whole procedural vocabulary the sky needs: a
;;; hash of an integer lattice index, trilinear smoothing between its corners,
;;; and a few octaves with the domain rotated between them so the cubic grain
;;; never reads as a grid.  It is written once here and reused by the cloud
;;; deck, the star field, and the block surface's own detail.

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

;;; The sky is image mathematics over a view ray and the frame environment:
;;; a vertical gradient between the profile's two colours, a warm low band and
;;; two Mie lobes around the sun, a drifting cloud deck projected onto a plane
;;; at cloud height, stars at night, and a compact solar disc pushed well past
;;; display white so the lens chain has something real to bloom.  Everything
;;; below the horizon fades into the exact fog colour distant terrain fades
;;; to, so silhouettes meet the sky without a seam.  #9SSXDJ records why an
;;; HDR path wants a sky with something genuinely bright in it.

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
         (low-sun
           (* day-factor
              (- 1.0 (smoothstep 0.10 0.55 (swizzle sun-direction :y)))))
         (above (smoothstep -0.02 0.30 elevation))
         (base (mix horizon zenith (expt above 0.70)))
         (band (expt (clamp (- 1.0 (abs elevation)) 0.0 1.0) 9.0))
         (hazed (mix base (vec3 1.05 0.60 0.32) (* 0.32 (* low-sun band))))
         (alignment (max 0.0 (dot direction sun-direction)))
         ;; Two Mie lobes: a broad brightening of the sun's quarter of the
         ;; sky, and a tighter shaft that hugs the disc.
         (broad (expt alignment 5.0))
         (tight (expt alignment 42.0))
         (scatter-tint
           (mix (vec3 1.0 0.96 0.86) (vec3 1.0 0.58 0.28) low-sun))
         (scattered
           (+ hazed
              (* scatter-tint
                 (* day-factor
                    (+ (* broad (+ 0.05 (* 0.10 low-sun)))
                       (* tight (+ 0.10 (* 0.22 low-sun))))))))
         ;; The cloud deck is a plane at a fixed height: the ray meets it
         ;; farther out the closer it runs to level, which is exactly the
         ;; perspective foreshortening real cloud decks show.
         (deck-mask (smoothstep 0.035 0.17 elevation))
         (deck-point
           (+ (* direction (/ 260.0 (max elevation 0.05)))
              (vec3 (* elapsed 1.7) 0.0 (* elapsed 0.9))))
         (cloud-field (lattice-fractal-noise (* deck-point 0.0062)))
         (cloud-edge (mix 0.66 0.36 (clamp cloudiness 0.0 1.0)))
         (cloud-density
           (* deck-mask
              (smoothstep cloud-edge (+ cloud-edge 0.11) cloud-field)))
         (core (expt (clamp cloud-density 0.0 1.0) 0.75))
         (cloud-lit
           (mix (vec3 1.15 1.13 1.10) (vec3 1.25 0.86 0.58)
                (* low-sun low-sun)))
         (cloud-shade
           (mix (vec3 0.50 0.58 0.74) (vec3 0.46 0.41 0.55)
                (* low-sun low-sun)))
         ;; Silver lining: thin edges facing the sun glow, dense cores do not.
         (silver
           (* cloud-lit
              (* (expt alignment 16.0)
                 (* (- 1.0 core) (+ 0.35 (* 0.75 low-sun))))))
         (cloud-color
           (* (+ (mix cloud-lit cloud-shade (* 0.82 core)) silver)
              (max day-factor 0.05)))
         (clouded (mix scattered cloud-color (* cloud-density 0.92)))
         (night (- 1.0 (smoothstep 0.0 0.35 day-factor)))
         (star-field (expt (lattice-noise (* direction 190.0)) 24.0))
         (starred
           (+ clouded
              (* (vec3 0.85 0.90 1.0)
                 (* star-field
                    (* night (* 1.4 (smoothstep 0.02 0.35 elevation)))))))
         ;; Only the last few degrees above level become fog; anything wider
         ;; washes the whole sky into the haze the fog is supposed to explain.
         (horizon-blend (smoothstep 0.085 -0.01 elevation))
         (grounded (mix starred fog-color horizon-blend))
         ;; The disc is drawn at a few times the sun's true angular radius,
         ;; the way every game sun is, and deliberately far above display
         ;; white: the floating point attachment keeps it, the bright pass
         ;; feeds on it, and the filmic curve rolls it into a hot core
         ;; instead of a flat clipped patch.  For a small angle the cosine
         ;; threshold of an angular radius is one less half its square.
         (disc-radius (* sun-width 4.0))
         (disc-limb (* 0.5 (* disc-radius disc-radius)))
         (disc
           (smoothstep (- 1.0 disc-limb) (- 1.0 (* 0.56 disc-limb)) alignment))
         (corona
           (+ (* (expt alignment 900.0) 0.8) (* (expt alignment 120.0) 0.22)))
         (occlusion (- 1.0 (* (clamp cloud-density 0.0 1.0) 0.94)))
         (solar
           (* sun-color
              (* day-factor
                 (+ (* disc (* 30.0 occlusion))
                    (* corona (* 2.0 occlusion))))))
         (rgb (+ grounded solar)))
    (set-output color-output (vec4 rgb 1.0))))

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
    '((post-control :vec4)    ; texel u, texel v, focus blur, exposure
      (lens-control :vec4)    ; bloom gain, shaft gain, vignette, threshold
      (sun-screen :vec4)      ; sun screen u, v, on-screen weight, diagnostic
      (bloom-control :vec4))  ; chain texel u, v, shaft decay, elapsed time
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

;;; A nine-tap gaussian expressed as five linearly filtered samples: each
;;; offset lands between the two texels whose weights it combines.  The two
;;; directions are separate roles rather than a uniform lane, so each pipeline
;;; is exactly the code it runs.

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
         (near-offset (* span 1.3846154))
         (far-offset (* span 3.2307692))
         (center
           (* (swizzle (sample source-color source-sampler uv-input) :xyz)
              0.2270270))
         (near
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input near-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input near-offset)) :xyz))
              0.3162162))
         (far
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input far-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input far-offset)) :xyz))
              0.0702700)))
    (set-output color-output (vec4 (+ center near far) 1.0))))

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
         (near-offset (* span 1.3846154))
         (far-offset (* span 3.2307692))
         (center
           (* (swizzle (sample source-color source-sampler uv-input) :xyz)
              0.2270270))
         (near
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input near-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input near-offset)) :xyz))
              0.3162162))
         (far
           (* (+ (swizzle (sample source-color source-sampler
                                  (+ uv-input far-offset)) :xyz)
                 (swizzle (sample source-color source-sampler
                                  (- uv-input far-offset)) :xyz))
              0.0702700)))
    (set-output color-output (vec4 (+ center near far) 1.0))))

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
         (active (swizzle post-control :z))
         (exposure (swizzle post-control :w))
         (sharp (sample scene-color scene-sampler uv-input))
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
         ;; A restrained corner falloff; the frame should feel photographed,
         ;; not port-holed.
         (centered (- uv-input (vec2 0.5 0.5)))
         (vignette
           (- 1.0
              (* (swizzle lens-control :z)
                 (smoothstep 0.10 0.75 (dot centered centered)))))
         ;; The shadow diagnostic is a measurement, not a picture: when the
         ;; scene pass is writing raw visibility instead of radiance, exposure,
         ;; the filmic curve, the lens chain, and the vignette would all
         ;; distort exactly the quantity being measured, so presentation hands
         ;; the value straight through.
         (diagnostic (swizzle sun-screen :w))
         (presented (mix (* graded vignette) radiance diagnostic)))
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
