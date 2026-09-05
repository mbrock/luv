(in-package #:luft.render.shaders)

;;; Site-stream rendering. One UVec4 instance selects a lattice base and a
;;; canonical fixed-stride template; the template vertex is a small exact
;;; offset plus geometric attributes. The CPU classifies sites and the vertex
;;; shader realizes the renderer-global triangle and quad populations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *screen-ambient-occlusion-strength* 0.38)
  (defvar *screen-ambient-occlusion-radius* 0.95)
  (defvar *ambient-pigment-strength* 0.82)
  ;; Match Luvcraft's scene-linear lens defaults.  LUFT uses the same bright
  ;; signal and gain while retaining its compact presentation gather.
  (defvar *highlight-glow-threshold* 1.5)
  (defvar *highlight-glow-strength* 0.22))

(define-shader-function unpack-terrain-tone (packed)
  "Decode one RGB8 terrain descriptor into scene-linear colour."
  (vec3 (/ (float (ldb (byte 8 0) packed)) 255.0)
        (/ (float (ldb (byte 8 8) packed)) 255.0)
        (/ (float (ldb (byte 8 16) packed)) 255.0)))

(define-shader-function terrain-material-sample
    (code selected descriptor normal)
  "Return one selected tone and its unit weight, or the additive identity."
  (let* ((upness (swizzle (representation normal) :z))
         (packed
           (if (> upness 0.35) (swizzle descriptor :x)
               (if (< upness -0.35) (swizzle descriptor :z)
                   (swizzle descriptor :y)))))
    (if (> selected (uint 0.0))
        (if (> code (uint 0.0))
            (vec4 (unpack-terrain-tone packed) 1.0)
            (vec4 0.0 0.0 0.0 0.0))
        (vec4 0.0 0.0 0.0 0.0))))

(define-shader-function paper-hash (site)
  (let* ((scattered (fract (* site 0.1031)))
         (shift (dot scattered
                     (+ (swizzle scattered :zyx)
                        (vec3 31.32 31.32 31.32))))
         (folded (+ scattered (vec3 shift shift shift))))
    (fract (* (+ (swizzle folded :x) (swizzle folded :y))
              (swizzle folded :z)))))

(define-shader-function paper-noise (point)
  (let* ((lattice (floor point))
         (offset (fract point))
         (weight (* offset (* offset (- (vec3 3.0 3.0 3.0)
                                        (* offset 2.0)))))
         (u (swizzle weight :x))
         (v (swizzle weight :y))
         (w (swizzle weight :z))
         (near-low (mix (paper-hash lattice)
                        (paper-hash (+ lattice (vec3 1.0 0.0 0.0))) u))
         (near-high (mix (paper-hash (+ lattice (vec3 0.0 1.0 0.0)))
                         (paper-hash (+ lattice (vec3 1.0 1.0 0.0))) u))
         (far-low (mix (paper-hash (+ lattice (vec3 0.0 0.0 1.0)))
                       (paper-hash (+ lattice (vec3 1.0 0.0 1.0))) u))
         (far-high (mix (paper-hash (+ lattice (vec3 0.0 1.0 1.0)))
                        (paper-hash (+ lattice (vec3 1.0 1.0 1.0))) u)))
    (mix (mix near-low near-high v) (mix far-low far-high v) w)))

(define-shader-function scene-relative-luminance (radiance)
  "Reduce checked linear RGB radiance to its named relative luminance."
  (let* ((radiance
           (interpret radiance :quantity quantities:scene-radiance
                                :unit :one))
         (weights
           (assume-quantity (vec3 0.2126 0.7152 0.0722) :unit :one)))
    (interpret (dot radiance weights)
               :quantity quantities:scene-luminance :unit :one)))

(define-shader-function paper-tonemap (radiance)
  "Cross from scene radiance into bounded display-linear colour."
  (let* ((radiance
           (representation
            (interpret radiance :quantity quantities:scene-radiance
                                 :unit :one)))
         (numerator
           (* radiance (+ (* radiance 2.51) (vec3 0.03 0.03 0.03))))
         (denominator
           (+ (* radiance (+ (* radiance 2.43) (vec3 0.59 0.59 0.59)))
              (vec3 0.14 0.14 0.14))))
    ;; The fitted polynomial is a transfer function, not homogeneous
    ;; radiometric arithmetic.  Its output starts a distinct colour space.
    (assume-quantity
     (clamp (/ numerator denominator)
            (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))
     :quantity quantities:presented-color :unit :one)))

(define-shader-function paper-grade (color)
  "Keep cool shade and warm paper luminous after highlight compression."
  (let* ((color
           (representation
            (interpret color :quantity quantities:presented-color
                             :unit :one)))
         (luminance (dot color (vec3 0.2126 0.7152 0.0722)))
         (temperature (smoothstep 0.30 0.78 luminance))
         (split-tone
           (mix (vec3 0.93 0.99 1.08) (vec3 1.08 1.01 0.88)
                temperature))
         (toned (* color split-tone))
         (toned-luminance (dot toned (vec3 0.2126 0.7152 0.0722)))
         (grey (vec3 toned-luminance toned-luminance toned-luminance))
         (saturated (+ grey (* (- toned grey) 1.12)))
         (curved
           (* saturated
              (* saturated
                 (- (vec3 3.0 3.0 3.0) (* saturated 2.0)))))
         (contrasted (mix saturated curved 0.14))
         (black (vec3 0.0 0.0 0.0))
         (white (vec3 1.0 1.0 1.0)))
    (assume-quantity
     (clamp contrasted black white)
     :quantity quantities:presented-color :unit :one)))

(define-shader-function highlight-energy (value)
  "Keep only genuinely luminous scene-linear colour for the paper glow."
  (let* ((color
           (assume-quantity (swizzle value :xyz)
                            :quantity quantities:scene-radiance :unit :one))
         (luminance (scene-relative-luminance color))
         (gate
           (smoothstep
            (quantity #.*highlight-glow-threshold*
                      :quantity quantities:scene-luminance :unit :one)
            (quantity 1.55 :quantity quantities:scene-luminance :unit :one)
            luminance)))
    (* color gate)))

(define-shader-function mesh-view-clip
    (point position right up forward projection divisor)
  (let* ((relative (- point (swizzle position :xyz)))
         ;; The view coordinates are checked cell-valued projections.  The
         ;; following homogeneous row is deliberately representation: its Z
         ;; offset and W divisor change meaning with projection mode.
         (view-x (representation
                  (dot relative (swizzle right :xyz))))
         (view-y (representation
                  (dot relative (swizzle up :xyz))))
         (view-z (representation
                  (dot relative (swizzle forward :xyz)))))
    (vec4 (* view-x (swizzle projection :x))
          (- (* view-y (swizzle projection :y)))
          (+ (* view-z (swizzle projection :z))
             (swizzle projection :w))
          (mix 1.0 view-z divisor))))

(define-shader-function star-view-rejection (clip projection divisor jitter)
  "Conservatively reject a star's radius-two cell sphere in homogeneous clip.

Every atlas vertex lies within one cell on each axis of its owning site.
The wider sphere includes that cube; jitter expands the side planes too.
#FIZQQ6"
  (let* ((extent (* (abs projection) 2.0))
         (w-extent (* divisor 2.0))
         (w-max (+ (swizzle clip :w) w-extent))
         (jitter-w (+ (abs (swizzle clip :w)) w-extent)))
    (max
     (if (> (abs (swizzle clip :x))
            (+ w-max (swizzle extent :x) (* (abs (swizzle jitter :x)) jitter-w))) 1.0 0.0)
     (max
      (if (> (abs (swizzle clip :y))
             (+ w-max (swizzle extent :y) (* (abs (swizzle jitter :y)) jitter-w))) 1.0 0.0)
      (max (if (< (+ (swizzle clip :z) (swizzle extent :z)) 0.0) 1.0 0.0)
           (if (> (- (swizzle clip :z) (swizzle extent :z)) w-max) 1.0 0.0))))))

(define-shader-function mesh-clip-uv (clip)
  (assume-quantity
   (+ (* (/ (swizzle clip :xy) (swizzle clip :w)) 0.5)
      (vec2 0.5 0.5))
   :quantity quantities:texture-coordinate :unit :one))

(define-shader-function mesh-temporal-motion (previous-clip current-clip)
  "Return the unjittered previous-minus-current motion MetalFX expects.

The scaler receives the current sampling offset independently through
JITTER-OFFSET-{X,Y}.  Its default contract consumes these vectors directly,
so adding either frame's Halton offset here would invent screen-wide motion
for completely static geometry."
  (representation
   (- (mesh-clip-uv previous-clip)
      (mesh-clip-uv current-clip))))

(define-shader-function light-clip-position
    (world-position row-x row-y row-z row-w)
  ;; A homogeneous projective row is representation, not four compatible
  ;; spatial quantities.  Erase the checked point exactly at that boundary.
  (let* ((point (vec4 (representation world-position) 1.0)))
    (vec4 (dot point row-x) (dot point row-y)
          (dot point row-z) (dot point row-w))))

(define-shader-function soft-shadow-visibility
    (shadow-map shadow-sampler shadow-sample normal sun shadow-control)
  "Five comparison-filtered taps forming one restrained paper-soft shadow."
  ;; Comparison sampling combines a projective coordinate, slope bias, and
  ;; integer filter radius.  Their interface meanings are checked; the fixed
  ;; comparison-filter program is the explicit representation seam.
  (let* ((shadow-sample (representation shadow-sample))
         (uv (swizzle shadow-sample :xy))
         (depth (swizzle shadow-sample :z))
         (u (swizzle uv :x))
         (v (swizzle uv :y))
         (in-bounds
           (* (step 0.0 u) (step u 1.0)
              (step 0.0 v) (step v 1.0)
              (step 0.0 depth) (step depth 1.0)))
         (facing (max 0.0 (dot normal sun)))
         (bias (+ (swizzle shadow-control :z)
                  (* 0.00125 (- 1.0 facing))))
         (radius (* (swizzle shadow-control :xy)
                    (swizzle shadow-control :w)))
         (visibility
           (/ (+ (sample-compare shadow-map shadow-sampler uv (- depth bias))
                 (sample-compare shadow-map shadow-sampler
                                 (+ uv (vec2 (swizzle radius :x) 0.0))
                                 (- depth bias))
                 (sample-compare shadow-map shadow-sampler
                                 (- uv (vec2 (swizzle radius :x) 0.0))
                                 (- depth bias))
                 (sample-compare shadow-map shadow-sampler
                                 (+ uv (vec2 0.0 (swizzle radius :y)))
                                 (- depth bias))
                 (sample-compare shadow-map shadow-sampler
                                 (- uv (vec2 0.0 (swizzle radius :y)))
                                 (- depth bias)))
              5.0)))
    (mix 1.0 visibility in-bounds)))

;;; A small, rounded traveler for the sanctuary bridge.  The proxy is only a
;;; conservative raster bound; every visible contour below comes from this
;;; analytic field.  LUFT is Z-up, unlike luvcraft's first-person figures.

(define-live-shader mesh-vertex-specification
    (:stage :mesh
     :workgroup-size (32 1 1)
     :inputs ((lane :uint :built-in :local-invocation-index)
              (group :uvec3 :built-in :workgroup-id))
     :resources ((sites :storage-buffer :binding 0 :element :uvec4)
                 (star-templates :storage-buffer :binding 1 :element :uvec4)
                 (terrain-appearances :storage-buffer :binding 3 :element :uvec2)
                 (material-descriptors :storage-buffer :binding 6 :element :uvec4)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23)))
     :mesh-output
     (:topology :triangles
      :max-vertices 75
      :max-primitives 25
      :vertex
      ((clip-position :vec4 :built-in :position)
       (world-position-output :vec3 :location 0
                              :quantity quantities:world-position
                              :unit quantities:cell)
       (mesh-normal-output :vec3 :location 1 :interpolation :flat
                           :quantity quantities:world-orientation :unit :one)
       (current-clip-output :vec4 :location 2)
       (previous-clip-output :vec4 :location 3)
       (shadow-sample-output :vec3 :location 4
                             :quantity quantities:shadow-coordinate :unit :one)
       (material-tone-output :vec3 :location 5 :interpolation :flat))))
  (let* ((site (buffer-element sites (swizzle group :x)))
         (centre
           (assume-quantity
            (vec3 (float (swizzle site :x)) (float (swizzle site :y))
                  (float (swizzle site :z)))
            :quantity quantities:world-position :unit quantities:cell))
         (divisor (swizzle (representation render-parameters) :z))
         (centre-clip (mesh-view-clip centre camera-position camera-right camera-up
                                      camera-forward camera-projection divisor))
         (appearance
           (buffer-element terrain-appearances (swizzle group :x)))
         (code-0 (ldb (byte 8 0) (swizzle appearance :x)))
         (code-1 (ldb (byte 8 8) (swizzle appearance :x)))
         (code-2 (ldb (byte 8 16) (swizzle appearance :x)))
         (code-3 (ldb (byte 8 24) (swizzle appearance :x)))
         (code-4 (ldb (byte 8 0) (swizzle appearance :y)))
         (code-5 (ldb (byte 8 8) (swizzle appearance :y)))
         (code-6 (ldb (byte 8 16) (swizzle appearance :y)))
         (code-7 (ldb (byte 8 24) (swizzle appearance :y)))
         (descriptor-0 (buffer-element material-descriptors code-0))
         (descriptor-1 (buffer-element material-descriptors code-1))
         (descriptor-2 (buffer-element material-descriptors code-2))
         (descriptor-3 (buffer-element material-descriptors code-3))
         (descriptor-4 (buffer-element material-descriptors code-4))
         (descriptor-5 (buffer-element material-descriptors code-5))
         (descriptor-6 (buffer-element material-descriptors code-6))
         (descriptor-7 (buffer-element material-descriptors code-7))
         (block (* (swizzle site :w) (uint 76.0)))
         (triangle-count
           (if (> (star-view-rejection centre-clip camera-projection divisor
                                       (representation (swizzle temporal-parameters :xy))) 0.5)
               (uint 0.0)
               (swizzle (buffer-element star-templates block) :x)))
         (vertex-count (* triangle-count (uint 3.0)))
         (safe-lane (if (< lane (uint 25.0)) lane (uint 24.0)))
         (first-record (+ block (uint 1.0) (* safe-lane (uint 3.0))))
         (record-0 (buffer-element star-templates first-record))
         (record-1
           (buffer-element star-templates (+ first-record (uint 1.0))))
         (record-2
           (buffer-element star-templates (+ first-record (uint 2.0))))
         (site-origin
           (vec3 (float (swizzle site :x))
                 (float (swizzle site :y))
                 (float (swizzle site :z))))
         (world-0
           (assume-quantity
            (+ site-origin
               (/ (- (vec3 (float (swizzle record-0 :x))
                           (float (swizzle record-0 :y))
                           (float (swizzle record-0 :z)))
                     (vec3 8.0 8.0 8.0))
                  8.0))
            :quantity quantities:world-position :unit quantities:cell))
         (world-1
           (assume-quantity
            (+ site-origin
               (/ (- (vec3 (float (swizzle record-1 :x))
                           (float (swizzle record-1 :y))
                           (float (swizzle record-1 :z)))
                     (vec3 8.0 8.0 8.0))
                  8.0))
            :quantity quantities:world-position :unit quantities:cell))
         (world-2
           (assume-quantity
            (+ site-origin
               (/ (- (vec3 (float (swizzle record-2 :x))
                           (float (swizzle record-2 :y))
                           (float (swizzle record-2 :z)))
                     (vec3 8.0 8.0 8.0))
                  8.0))
            :quantity quantities:world-position :unit quantities:cell))
         (edge-a (- (representation world-1) (representation world-0)))
         (edge-b (- (representation world-2) (representation world-0)))
         (normal
           (assume-quantity
            (normalize
             (vec3 (- (* (swizzle edge-a :y) (swizzle edge-b :z))
                      (* (swizzle edge-a :z) (swizzle edge-b :y)))
                   (- (* (swizzle edge-a :z) (swizzle edge-b :x))
                      (* (swizzle edge-a :x) (swizzle edge-b :z)))
                   (- (* (swizzle edge-a :x) (swizzle edge-b :y))
                      (* (swizzle edge-a :y) (swizzle edge-b :x)))))
            :quantity quantities:world-orientation :unit :one))
         (sample-mask (ldb (byte 8 0) (swizzle record-0 :w)))
         (sample-0
           (terrain-material-sample
            code-0 (ldb (byte 1 0) sample-mask) descriptor-0 normal))
         (sample-1
           (terrain-material-sample
            code-1 (ldb (byte 1 1) sample-mask) descriptor-1 normal))
         (sample-2
           (terrain-material-sample
            code-2 (ldb (byte 1 2) sample-mask) descriptor-2 normal))
         (sample-3
           (terrain-material-sample
            code-3 (ldb (byte 1 3) sample-mask) descriptor-3 normal))
         (sample-4
           (terrain-material-sample
            code-4 (ldb (byte 1 4) sample-mask) descriptor-4 normal))
         (sample-5
           (terrain-material-sample
            code-5 (ldb (byte 1 5) sample-mask) descriptor-5 normal))
         (sample-6
           (terrain-material-sample
            code-6 (ldb (byte 1 6) sample-mask) descriptor-6 normal))
         (sample-7
           (terrain-material-sample
            code-7 (ldb (byte 1 7) sample-mask) descriptor-7 normal))
         (tone-total
           (+ sample-0 sample-1 sample-2 sample-3
              sample-4 sample-5 sample-6 sample-7))
         (material-tone
           (/ (swizzle tone-total :xyz)
              (max (swizzle tone-total :w) 1.0)))
         (clip-0
           (mesh-view-clip world-0 camera-position camera-right camera-up
                           camera-forward camera-projection
                           (swizzle (representation render-parameters) :z)))
         (clip-1
           (mesh-view-clip world-1 camera-position camera-right camera-up
                           camera-forward camera-projection
                           (swizzle (representation render-parameters) :z)))
         (clip-2
           (mesh-view-clip world-2 camera-position camera-right camera-up
                           camera-forward camera-projection
                           (swizzle (representation render-parameters) :z)))
         (previous-0
           (mesh-view-clip world-0 previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle (representation render-parameters) :z)))
         (previous-1
           (mesh-view-clip world-1 previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle (representation render-parameters) :z)))
         (previous-2
           (mesh-view-clip world-2 previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle (representation render-parameters) :z)))
         (light-0 (light-clip-position world-0 shadow-row-x shadow-row-y
                                       shadow-row-z shadow-row-w))
         (light-1 (light-clip-position world-1 shadow-row-x shadow-row-y
                                       shadow-row-z shadow-row-w))
         (light-2 (light-clip-position world-2 shadow-row-x shadow-row-y
                                       shadow-row-z shadow-row-w))
         (jitter (representation (swizzle temporal-parameters :xy)))
         (vertex-0 (* lane (uint 3.0)))
         (vertex-1 (+ vertex-0 (uint 1.0)))
         (vertex-2 (+ vertex-0 (uint 2.0))))
    (set-mesh-output-counts vertex-count triangle-count)
    (when (< lane triangle-count)
        (set-mesh-vertex
         vertex-0
         (clip-position
          (vec4 (+ (swizzle clip-0 :x)
                   (* (swizzle jitter :x) (swizzle clip-0 :w)))
                (+ (swizzle clip-0 :y)
                   (* (swizzle jitter :y) (swizzle clip-0 :w)))
                (swizzle clip-0 :z) (swizzle clip-0 :w)))
         (world-position-output world-0) (mesh-normal-output normal)
         (current-clip-output clip-0) (previous-clip-output previous-0)
         (shadow-sample-output
          (assume-quantity
           (vec3 (+ (* (swizzle light-0 :x) 0.5) 0.5)
                 (+ (* (swizzle light-0 :y) 0.5) 0.5)
                 (swizzle light-0 :z))
           :quantity quantities:shadow-coordinate :unit :one))
         (material-tone-output material-tone))
        (set-mesh-vertex
         vertex-1
         (clip-position
          (vec4 (+ (swizzle clip-1 :x)
                   (* (swizzle jitter :x) (swizzle clip-1 :w)))
                (+ (swizzle clip-1 :y)
                   (* (swizzle jitter :y) (swizzle clip-1 :w)))
                (swizzle clip-1 :z) (swizzle clip-1 :w)))
         (world-position-output world-1) (mesh-normal-output normal)
         (current-clip-output clip-1) (previous-clip-output previous-1)
         (shadow-sample-output
          (assume-quantity
           (vec3 (+ (* (swizzle light-1 :x) 0.5) 0.5)
                 (+ (* (swizzle light-1 :y) 0.5) 0.5)
                 (swizzle light-1 :z))
           :quantity quantities:shadow-coordinate :unit :one))
         (material-tone-output material-tone))
        (set-mesh-vertex
         vertex-2
         (clip-position
          (vec4 (+ (swizzle clip-2 :x)
                   (* (swizzle jitter :x) (swizzle clip-2 :w)))
                (+ (swizzle clip-2 :y)
                   (* (swizzle jitter :y) (swizzle clip-2 :w)))
                (swizzle clip-2 :z) (swizzle clip-2 :w)))
         (world-position-output world-2) (mesh-normal-output normal)
         (current-clip-output clip-2) (previous-clip-output previous-2)
         (shadow-sample-output
          (assume-quantity
           (vec3 (+ (* (swizzle light-2 :x) 0.5) 0.5)
                 (+ (* (swizzle light-2 :y) 0.5) 0.5)
                 (swizzle light-2 :z))
           :quantity quantities:shadow-coordinate :unit :one))
         (material-tone-output material-tone))
        (set-mesh-primitive lane (uvec3 vertex-0 vertex-1 vertex-2)))))

(define-live-shader star-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0
                              :quantity quantities:world-position
                              :unit quantities:cell)
              (mesh-normal :vec3 :location 1 :interpolation :flat
                           :quantity quantities:world-orientation :unit :one)
              (current-clip :vec4 :location 2)
              (previous-clip :vec4 :location 3)
              (shadow-sample :vec3 :location 4
                             :quantity quantities:shadow-coordinate :unit :one)
              (material-tone :vec3 :location 5 :interpolation :flat))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23))
                 (shadow-map :depth-texture-2d :binding 4)
                 (shadow-sampler :sampler :binding 5)))
  (let* ((normal (normalize (representation mesh-normal)))
         (sun (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (sky (representation (swizzle sky-color-vector :xyz)))
         (ground (representation (swizzle ground-color-vector :xyz)))
         (upness (swizzle normal :z))
         (base material-tone)
         (facing (max 0.0 (dot normal sun)))
         (visibility
           (soft-shadow-visibility
            shadow-map shadow-sampler shadow-sample normal sun
            (representation shadow-control)))
         (sky-weight (+ 0.5 (* 0.5 upness)))
         (ambient (+ (* ground (- 1.0 sky-weight)) (* sky sky-weight)))
         (radiance
           (* base (+ (* ambient 0.72)
                      (* sun-color (* visibility facing)))))
         (camera-delta
           (representation
            (- world-position (swizzle camera-position :xyz))))
         (distance (sqrt (dot camera-delta camera-delta)))
         (fog (smoothstep 165.0 300.0 distance))
         (final (mix radiance sky fog)))
    (set-output color-output (vec4 final 1.0))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))

(define-live-shader shadow-vertex-specification
    (:stage :mesh
     :workgroup-size (32 1 1)
     :inputs ((lane :uint :built-in :local-invocation-index)
              (group :uvec3 :built-in :workgroup-id))
     :resources ((sites :storage-buffer :binding 0 :element :uvec4)
                 (star-templates :storage-buffer :binding 1 :element :uvec4)
                 (camera-state :uniform-block :binding 2
                  :members #.(scene-uniform-prefix 23)))
     :mesh-output
     (:topology :triangles :max-vertices 75 :max-primitives 25
      :vertex ((clip-position :vec4 :built-in :position))))
  (let* ((site (buffer-element sites (swizzle group :x)))
         (centre
           (assume-quantity
            (vec3 (float (swizzle site :x)) (float (swizzle site :y))
                  (float (swizzle site :z)))
            :quantity quantities:world-position :unit quantities:cell))
         (centre-clip (light-clip-position centre shadow-row-x shadow-row-y
                                          shadow-row-z shadow-row-w))
         ;; The shadow transform is orthographic. Row norms convert the same
         ;; conservative world sphere into independent clip-space radii.
         (projection
           (vec4 (sqrt (dot (swizzle shadow-row-x :xyz) (swizzle shadow-row-x :xyz)))
                 (sqrt (dot (swizzle shadow-row-y :xyz) (swizzle shadow-row-y :xyz)))
                 (sqrt (dot (swizzle shadow-row-z :xyz) (swizzle shadow-row-z :xyz))) 0.0))
         (block (* (swizzle site :w) (uint 76.0)))
         (triangle-count
           (if (> (star-view-rejection centre-clip projection 0.0 (vec2 0.0 0.0)) 0.5)
               (uint 0.0)
               (swizzle (buffer-element star-templates block) :x)))
         (safe-lane (if (< lane (uint 25.0)) lane (uint 24.0)))
         (first-record (+ block (uint 1.0) (* safe-lane (uint 3.0))))
         (record-0 (buffer-element star-templates first-record))
         (record-1
           (buffer-element star-templates (+ first-record (uint 1.0))))
         (record-2
           (buffer-element star-templates (+ first-record (uint 2.0))))
         (origin
           (vec3 (float (swizzle site :x))
                 (float (swizzle site :y))
                 (float (swizzle site :z))))
         (world-0
           (assume-quantity
            (+ origin (/ (- (vec3 (float (swizzle record-0 :x))
                                  (float (swizzle record-0 :y))
                                  (float (swizzle record-0 :z)))
                            (vec3 8.0 8.0 8.0)) 8.0))
            :quantity quantities:world-position :unit quantities:cell))
         (world-1
           (assume-quantity
            (+ origin (/ (- (vec3 (float (swizzle record-1 :x))
                                  (float (swizzle record-1 :y))
                                  (float (swizzle record-1 :z)))
                            (vec3 8.0 8.0 8.0)) 8.0))
            :quantity quantities:world-position :unit quantities:cell))
         (world-2
           (assume-quantity
            (+ origin (/ (- (vec3 (float (swizzle record-2 :x))
                                  (float (swizzle record-2 :y))
                                  (float (swizzle record-2 :z)))
                            (vec3 8.0 8.0 8.0)) 8.0))
            :quantity quantities:world-position :unit quantities:cell))
         (vertex-0 (* lane (uint 3.0)))
         (vertex-1 (+ vertex-0 (uint 1.0)))
         (vertex-2 (+ vertex-0 (uint 2.0))))
    (set-mesh-output-counts (* triangle-count (uint 3.0)) triangle-count)
    (when (< lane triangle-count)
        (set-mesh-vertex
         vertex-0
         (clip-position (light-clip-position world-0 shadow-row-x shadow-row-y
                                             shadow-row-z shadow-row-w)))
        (set-mesh-vertex
         vertex-1
         (clip-position (light-clip-position world-1 shadow-row-x shadow-row-y
                                             shadow-row-z shadow-row-w)))
        (set-mesh-vertex
         vertex-2
         (clip-position (light-clip-position world-2 shadow-row-x shadow-row-y
                                             shadow-row-z shadow-row-w)))
        (set-mesh-primitive lane (uvec3 vertex-0 vertex-1 vertex-2)))))

(define-live-shader lattice-point-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (marker-coordinate-output :vec2 :location 0)
               (current-clip-output :vec4 :location 1)
               (previous-clip-output :vec4 :location 2)
               (marker-kind-output :float :location 3
                                   :interpolation :flat))
     :resources ((lattice-points :storage-buffer :binding 0 :element :uvec4)
                 (camera-state :uniform-block :binding 1
                  :members #.(scene-uniform-prefix 14))))
  (let* ((record (buffer-element lattice-points instance-index))
         (world-position
           (assume-quantity
            (/ (vec3 (float (swizzle record :x))
                     (float (swizzle record :y))
                     (float (swizzle record :z)))
               8.0)
            :quantity quantities:world-position :unit quantities:cell))
         (index (float vertex-index))
         (right (if (= index 2.0) 1.0
                    (if (= index 3.0) 1.0
                        (if (= index 5.0) 1.0 0.0))))
         (bottom (if (= index 1.0) 1.0
                     (if (= index 4.0) 1.0
                         (if (= index 5.0) 1.0 0.0))))
         (coordinate (vec2 (- (* right 2.0) 1.0)
                           (- (* bottom 2.0) 1.0)))
         (marker-kind (float (swizzle record :w)))
         (current-clip
           (mesh-view-clip world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle (representation render-parameters) :z)))
         (previous-clip
           (mesh-view-clip world-position previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle (representation render-parameters) :z)))
         (pixel-size
           (representation (swizzle inspection-parameters :zw)))
         (radius (if (> marker-kind 1.5) 8.5
                     (if (> marker-kind 0.5) 6.5 2.6)))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output
     clip-position
     (vec4 (+ (+ (swizzle current-clip :x)
                 (* (swizzle jitter :x) (swizzle current-clip :w)))
              (* (swizzle coordinate :x)
                 (* (* radius 2.0) (swizzle pixel-size :x))
                 (swizzle current-clip :w)))
           (+ (+ (swizzle current-clip :y)
                 (* (swizzle jitter :y) (swizzle current-clip :w)))
              (* (swizzle coordinate :y)
                 (* (* radius 2.0) (swizzle pixel-size :y))
                 (swizzle current-clip :w)))
           (- (swizzle current-clip :z)
              (* 0.00015 (swizzle current-clip :w)))
           (swizzle current-clip :w)))
    (set-output marker-coordinate-output coordinate)
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)
    (set-output marker-kind-output marker-kind)))

(define-live-shader lattice-point-fragment-specification
    (:stage :fragment
     :inputs ((marker-coordinate :vec2 :location 0)
              (current-clip :vec4 :location 1)
              (previous-clip :vec4 :location 2)
              (marker-kind :float :location 3 :interpolation :flat))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1)))
  (let* ((radius (sqrt (dot marker-coordinate marker-coordinate)))
         (coverage (- 1.0 (smoothstep 0.82 1.0 radius)))
         (vertex-site (if (> marker-kind 1.5) 1.0 0.0))
         (mesh-point (if (> marker-kind 0.5) 1.0 0.0))
         (center (- 1.0 (smoothstep (if (> mesh-point 0.5) 0.40 0.18)
                                    (if (> mesh-point 0.5) 0.62 0.52)
                                    radius)))
         (rim (vec3 0.035 0.075 0.095))
         (ink (vec3 1.0 0.30 0.10))
         (site (vec3 0.20 0.95 1.0))
         (color (mix (mix rim ink center) site vertex-site))
         (alpha (* coverage 0.96)))
    (set-output color-output (vec4 (* color alpha) alpha))
    (set-output motion-output
                (- (mesh-clip-uv previous-clip)
                   (mesh-clip-uv current-clip)))))

(define-live-shader present-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (ndc-output :vec2 :location 0)))
  (let* ((index (float vertex-index))
         (x (if (< index 0.5) -1.0 (if (< index 1.5) 3.0 -1.0)))
         (y (if (< index 1.5) -1.0 3.0)))
    (set-output clip-position (vec4 x y 0.0 1.0))
    (set-output ndc-output (vec2 x y))))

(define-shader-function view-depth (depth projection divisor)
  (if (< divisor 0.5)
      (/ (- depth (swizzle projection :w)) (swizzle projection :z))
      (/ (swizzle projection :w)
         (- depth (swizzle projection :z)))))

(define-shader-function depth-occlusion (centre sample radius)
  (let* ((delta (- centre sample))
         (near (smoothstep 0.025 (* radius 0.42) delta))
         (far (- 1.0 (smoothstep (* radius 0.62) radius delta))))
    (* near far)))

(define-shader-function rgb-to-ycocg (rgb)
  "Put RGB into a luminance/chroma space whose box clips history usefully."
  (vec3 (+ (* (swizzle rgb :x) 0.25)
           (* (swizzle rgb :y) 0.50)
           (* (swizzle rgb :z) 0.25))
        (* 0.5 (- (swizzle rgb :x) (swizzle rgb :z)))
        (+ (* (swizzle rgb :x) -0.25)
           (* (swizzle rgb :y) 0.50)
           (* (swizzle rgb :z) -0.25))))

(define-shader-function ycocg-to-rgb (value)
  "Invert RGB-TO-YCOCG."
  (vec3 (+ (swizzle value :x) (swizzle value :y)
           (- (swizzle value :z)))
        (+ (swizzle value :x) (swizzle value :z))
        (+ (swizzle value :x) (- (swizzle value :y))
           (- (swizzle value :z)))))

(define-live-shader temporal-resolve-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((current :texture-2d :binding 0 :sample-transfer :identity)
      (motion-texture :texture-2d :binding 1 :sample-transfer :identity)
      (history :texture-2d :binding 2 :sample-transfer :identity)
      (temporal-sampler :sampler :binding 3)
      (camera-state :uniform-block :binding 4
       :members #.(scene-uniform-prefix 13))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         ;; INSPECTION-PARAMETERS.ZW is the inverse internal scene extent.
         ;; The resolve target and history are full-size, but the current
         ;; neighbourhood and integer motion lookup remain in input pixels.
         (texel
           (representation (swizzle inspection-parameters :zw)))
         (dx (vec2 (swizzle texel :x) 0.0))
         (dy (vec2 0.0 (swizzle texel :y)))
         (centre (sample current temporal-sampler uv))
         (c00 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (- (- uv dx) dy))
                        :xyz)))
         (c10 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (- uv dy)) :xyz)))
         (c20 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ (- uv dy) dx))
                        :xyz)))
         (c01 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (- uv dx)) :xyz)))
         (c11 (rgb-to-ycocg (swizzle centre :xyz)))
         (c21 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ uv dx)) :xyz)))
         (c02 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ (- uv dx) dy))
                        :xyz)))
         (c12 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ uv dy)) :xyz)))
         (c22 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ (+ uv dx) dy))
                        :xyz)))
         (neighbourhood-min (min c00 c10 c20 c01 c11 c21 c02 c12 c22))
         (neighbourhood-max (max c00 c10 c20 c01 c11 c21 c02 c12 c22))
         ;; Motion excludes jitter and resolved history lives on the fixed
         ;; output grid. A static point therefore reads the same history UV:
         ;; following Halton here would move the resolve instead of gathering
         ;; different subpixel samples into one output pixel.
         (pixel (uvec2 (uint (/ (swizzle uv :x) (swizzle texel :x)))
                       (uint (/ (swizzle uv :y) (swizzle texel :y)))))
         (velocity (swizzle (texel-load motion-texture pixel) :xy))
         (history-uv (+ uv velocity))
         (inside
           (* (* (step 0.0 (swizzle history-uv :x))
                 (step (swizzle history-uv :x) 1.0))
              (* (step 0.0 (swizzle history-uv :y))
                 (step (swizzle history-uv :y) 1.0))))
         (old (rgb-to-ycocg
               (swizzle (sample history temporal-sampler history-uv) :xyz)))
         (clipped (clamp old neighbourhood-min neighbourhood-max))
         (speed (clamp (* (sqrt (dot velocity velocity)) 48.0) 0.0 1.0))
         ;; The otherwise-unused W components of these previous-view lanes
         ;; carry resolve validity and weight without changing the frame ABI.
         (history-weight
           (* (* (swizzle (representation previous-camera-position) :w)
                 inside)
              (* (swizzle (representation previous-camera-right) :w)
                 (- 1.0 (* speed 0.35)))))
         (resolved (ycocg-to-rgb (mix c11 clipped history-weight))))
    ;; Alpha is current-frame focus metadata, never temporal colour history.
    (set-output color-output (vec4 resolved (swizzle centre :w)))))

(define-live-shader present-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity)
                 (scene-sampler :sampler :binding 1)
                 (scene-depth :depth-texture-2d :binding 2)
                 (camera-state :uniform-block :binding 3
                  :members #.(scene-uniform-prefix 17))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (value (sample scene scene-sampler uv))
         (auto-exposure (swizzle sky-color-vector :w))
         (texel
           (representation (swizzle inspection-parameters :zw)))
         (near (* texel 3.0))
         (far (* texel 11.0))
         ;; One low-cost, deliberately broad gather.  Linear sampling and the
         ;; two rings make luminous paper bleed across an edge without erasing
         ;; the edge itself or turning the whole frame into fog.
         (glow
           (* 0.125
              (+
               (highlight-energy
                (sample scene scene-sampler
                        (+ uv (vec2 (swizzle near :x) 0.0))))
               (highlight-energy
                (sample scene scene-sampler
                        (+ uv (vec2 (- (swizzle near :x)) 0.0))))
               (highlight-energy
                (sample scene scene-sampler
                        (+ uv (vec2 0.0 (swizzle near :y)))))
               (highlight-energy
                (sample scene scene-sampler
                        (+ uv (vec2 0.0 (- (swizzle near :y))))))
               (highlight-energy (sample scene scene-sampler (+ uv far)))
               (highlight-energy (sample scene scene-sampler (- uv far)))
               (highlight-energy
                (sample
                 scene scene-sampler
                 (+ uv (vec2 (swizzle far :x) (- (swizzle far :y))))))
               (highlight-energy
                (sample
                 scene scene-sampler
                 (+ uv (vec2 (- (swizzle far :x)) (swizzle far :y))))))))
         ;; Geometry depth carries the subpixel projection jitter consumed by
         ;; MetalFX; presentation UVs do not.  Sample depth at the same current
         ;; geometry location so the AO does not crawl across a resolved edge.
         (depth-uv
           (+ uv
              (* (representation (swizzle temporal-parameters :xy)) 0.5)))
         (depth (swizzle (sample scene-depth scene-sampler depth-uv) :x))
         (divisor (swizzle (representation render-parameters) :z))
         (centre (view-depth depth camera-projection divisor))
         (perspective-scale (if (< divisor 0.5) 1.0 (/ 1.0 centre)))
         (outer
           (* (swizzle camera-projection :xy)
              (* 0.5 #.*screen-ambient-occlusion-radius*
                 perspective-scale)))
         (inner (* outer 0.42))
         (diagonal (* outer 0.70710678))
         (occlusion
           (* (/ 1.0 12.0)
              (+
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 (swizzle inner :x) 0.0)))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample
                                   scene-depth scene-sampler
                                   (+ depth-uv
                                      (vec2 (- (swizzle inner :x)) 0.0)))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 0.0
                                                   (swizzle inner :y))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 0.0
                                                   (- (swizzle inner :y)))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 (swizzle outer :x) 0.0)))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample
                                   scene-depth scene-sampler
                                   (+ depth-uv
                                      (vec2 (- (swizzle outer :x)) 0.0)))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 0.0
                                                   (swizzle outer :y))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample scene-depth scene-sampler
                                          (+ depth-uv
                                             (vec2 0.0
                                                   (- (swizzle outer :y)))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle (sample scene-depth scene-sampler
                                                  (+ depth-uv diagonal))
                                          :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle (sample scene-depth scene-sampler
                                                  (- depth-uv diagonal))
                                          :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample
                                   scene-depth scene-sampler
                                   (+ depth-uv
                                      (vec2 (swizzle diagonal :x)
                                            (- (swizzle diagonal :y)))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*)
               (depth-occlusion centre
                                (view-depth
                                 (swizzle
                                  (sample
                                   scene-depth scene-sampler
                                   (+ depth-uv
                                      (vec2 (- (swizzle diagonal :x))
                                            (swizzle diagonal :y))))
                                  :x)
                                 camera-projection divisor)
                                #.*screen-ambient-occlusion-radius*))))
         (accessibility
           (if (< depth 0.9999)
               (- 1.0 (* #.*screen-ambient-occlusion-strength*
                         (min 1.0 (* occlusion 1.6))))
               1.0))
         (pooling
           (* #.*ambient-pigment-strength*
              (min 1.0 (* occlusion 1.6))))
         ;; A common cool shadow pigment makes the depth signal read as an
         ;; illustrated wash rather than neutral post-process darkening.
         (shadow-pigment (vec3 0.76 0.88 1.03))
         (shadowed (* (swizzle value :xyz) accessibility))
         (pigmented (* shadowed
                       (mix (vec3 1.0 1.0 1.0)
                            shadow-pigment pooling)))
         ;; A gentle screen-space tilt shift holds the player's projected
         ;; height crisp and gives the near/far route a miniature depth cue.
         ;; It is deliberately only a five-tap gather after MetalFX resolves.
         (player-clip
           (mesh-view-clip (swizzle character-parameters :xyz)
                           camera-position camera-right camera-up
                           camera-forward camera-projection divisor))
         (focus-y
           (+ (representation
               (swizzle (mesh-clip-uv player-clip) :y))
              (* (swizzle (representation temporal-parameters) :y) 0.5)))
         (tilt
           (smoothstep 0.16 0.52 (abs (- (swizzle uv :y) focus-y))))
         (blur-radius (* texel 2.6))
         (blurred
           (* 0.2
              (+ (swizzle value :xyz)
                 (swizzle (sample scene scene-sampler
                                  (+ uv (vec2 (swizzle blur-radius :x) 0.0)))
                          :xyz)
                 (swizzle (sample scene scene-sampler
                                  (- uv (vec2 (swizzle blur-radius :x) 0.0)))
                          :xyz)
                 (swizzle (sample scene scene-sampler
                                  (+ uv (vec2 0.0 (swizzle blur-radius :y))))
                          :xyz)
                 (swizzle (sample scene scene-sampler
                                  (- uv (vec2 0.0 (swizzle blur-radius :y))))
                          :xyz))))
         (bloomed
           (+ (assume-quantity
               (swizzle pigmented :xyz)
               :quantity quantities:scene-radiance :unit :one)
              (* glow #.*highlight-glow-strength*)))
         (glowing
           (mix bloomed
                (assume-quantity blurred
                                 :quantity quantities:scene-radiance
                                 :unit :one)
                (assume-quantity (if (< divisor 0.5) (* tilt 0.52) 0.0) :unit :one)))
         ;; MetalFX has already reconstructed GLowing at this point.  Grade
         ;; it once, with a little exposure headroom for the sunlit grass and
         ;; the wizard's HDR spell rather than clipping both into parchment.
         ;; Sky is now HDR scene radiance, so it participates in metering and
         ;; receives exactly the same exposure and paper grade as geometry.
         ;; Keep geometry-only AO and tilt-shift out of background pixels.
         (radiance
           (if (< depth 0.9999) glowing
               (assume-quantity
                (swizzle value :xyz)
                :quantity quantities:scene-radiance :unit :one)))
         (exposed-radiance
           (interpret (* radiance auto-exposure)
                      :quantity quantities:scene-radiance :unit :one))
         (presented
           (paper-grade (paper-tonemap exposed-radiance)))
         (cross-pixel (abs (/ (* ndc 0.5) texel)))
         (cross-long (max (swizzle cross-pixel :x) (swizzle cross-pixel :y)))
         (cross-short (min (swizzle cross-pixel :x) (swizzle cross-pixel :y)))
         (crosshair (if (> (swizzle (representation camera-position) :w) 0.5)
                        (if (< cross-long 7.0)
                            (if (< cross-short 1.6) 1.0 0.0) 0.0) 0.0))
         (cross-color (if (< cross-long 6.0)
                          (if (< cross-short 0.7) (vec3 0.95 0.95 0.95)
                              (vec3 0.08 0.08 0.08))
                          (vec3 0.08 0.08 0.08))))
    (set-output color-output
                (vec4 (mix (representation presented) cross-color crosshair)
                      1.0))))

(define-live-shader torch-flame-composite-copy-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity)
                 (scene-sampler :sampler :binding 1)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5))))
    (set-output color-output (sample scene scene-sampler uv))))
