(in-package #:luft.render.shaders)

;;; A face record is one UVec4.  The vertex stage pulls one record per
;;; instance and realizes one of its twenty-four implicit points.  The four
;;; complete vertex stars in the shape word are the discrete geometric source;
;;; CPU and GPU derive centres, edge points, and miter half-points from them.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *stock-tooth* 0.055
    "How deeply the fine matte tooth modulates every stock's tone.

Lonely Mountains: Downhill maps a fine noise onto all objects to give
them materiality -- the paper look -- over flat palette colours.  The
stock style's figure, mottle, and drift are each per-material; this one
grain is shared by everything, which is what makes the picture read as
printed on one sheet.")

  (defvar *paper-variation* 0.11
    "How far one cell's tone may drift from its neighbour's, in value and
in warmth.  Lonely Mountains: Downhill varies each object's colour a little
against a shared palette so no two trees are the same object twice; a block
world has the same problem in a starker form."))

(define-shader-function paper-hash (site)
  "Hash one integer lattice site into the unit interval, without a sine."
  (let* ((scattered (fract (* site 0.1031)))
         (shift (dot scattered
                     (+ (swizzle scattered :zyx)
                        (vec3 31.32 31.32 31.32))))
         (folded (+ scattered (vec3 shift shift shift))))
    (fract (* (+ (swizzle folded :x) (swizzle folded :y))
              (swizzle folded :z)))))

(define-shader-function paper-noise (point)
  "Smoothly interpolated value noise over an integer lattice."
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

(define-shader-function stock-tooth (point)
  "The shared fine matte tooth, centred on one."
  (let* ((coarse (paper-noise (* point 11.0)))
         (fine (paper-noise (* point 31.0))))
    (+ 1.0
       (* #.*stock-tooth* (- (+ (* 0.55 coarse) (* 0.45 fine)) 0.5)))))

(define-shader-function paper-tonemap (radiance)
  "Map linear radiance through the fitted ACES display curve."
  (let* ((numerator
           (* radiance (+ (* radiance 2.51) (vec3 0.03 0.03 0.03))))
         (denominator
           (+ (* radiance (+ (* radiance 2.43) (vec3 0.59 0.59 0.59)))
              (vec3 0.14 0.14 0.14))))
    (clamp (/ numerator denominator)
           (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))))

(define-shader-function face-view-clip
    (point position right up forward projection divisor)
  "Project one world point without temporal jitter."
  (let* ((relative (- point (swizzle position :xyz)))
         (view-x (dot relative (swizzle right :xyz)))
         (view-y (dot relative (swizzle up :xyz)))
         (view-z (dot relative (swizzle forward :xyz))))
    (vec4 (* view-x (swizzle projection :x))
          (- (* view-y (swizzle projection :y)))
          (+ (* view-z (swizzle projection :z))
             (swizzle projection :w))
          (mix 1.0 view-z divisor))))

(define-shader-function face-clip-uv (clip)
  (+ (* (/ (swizzle clip :xy) (swizzle clip :w)) 0.5)
     (vec2 0.5 0.5)))

(define-shader-function vertex-star-count (star)
  (+ (+ (+ (uint (ldb (byte 1 0) star))
           (uint (ldb (byte 1 1) star)))
        (+ (uint (ldb (byte 1 2) star))
           (uint (ldb (byte 1 3) star))))
     (+ (+ (uint (ldb (byte 1 4) star))
           (uint (ldb (byte 1 5) star)))
        (+ (uint (ldb (byte 1 6) star))
           (uint (ldb (byte 1 7) star))))))

(define-shader-function vertex-star-minority-bit (star count sample)
  (let* ((solid (float (ldb (byte 1 sample) star)))
         (four (uint 4.0)))
    (if (= count four)
        0.0
        (if (< count four) solid (- 1.0 solid)))))

(define-shader-function vertex-star-moment (star)
  (let* ((count (vertex-star-count star))
         (b0 (vertex-star-minority-bit star count (uint 0.0)))
         (b1 (vertex-star-minority-bit star count (uint 1.0)))
         (b2 (vertex-star-minority-bit star count (uint 2.0)))
         (b3 (vertex-star-minority-bit star count (uint 3.0)))
         (b4 (vertex-star-minority-bit star count (uint 4.0)))
         (b5 (vertex-star-minority-bit star count (uint 5.0)))
         (b6 (vertex-star-minority-bit star count (uint 6.0)))
         (b7 (vertex-star-minority-bit star count (uint 7.0)))
         (mx (+ (- (+ b1 b3) (+ b0 b2))
                (- (+ b5 b7) (+ b4 b6))))
         (my (+ (- (+ b2 b3) (+ b0 b1))
                (- (+ b6 b7) (+ b4 b5))))
         (mz (- (+ (+ b4 b5) (+ b6 b7))
                (+ (+ b0 b1) (+ b2 b3)))))
    (vec4 mx my mz (float count))))

(define-shader-function vertex-star-miter (moment)
  (let* ((m (abs (swizzle moment :xyz)))
         (count (swizzle moment :w))
         (minority (min count (- 8.0 count)))
         (maximum (max (swizzle m :x)
                       (max (swizzle m :y) (swizzle m :z))))
         (product (* (swizzle m :x) (* (swizzle m :y) (swizzle m :z)))))
    (if (= minority 3.0)
        (if (= maximum 3.0) (= product 3.0) (= count -1.0))
        (= count -1.0))))

(define-shader-function vertex-star-center-offset (star width)
  (let* ((moment (vertex-star-moment star))
         (m (swizzle moment :xyz))
         (q (signum m))
         (absolute (abs m))
         (unit-diagonal
           (if (= (swizzle absolute :x) 1.0)
               (if (= (swizzle absolute :y) 1.0)
                   (= (swizzle absolute :z) 1.0)
                   (= width -1.0))
               (= width -1.0)))
         (ordinary-reach (if unit-diagonal 0.6666667 0.5))
         (radius (* width 0.5))
         (diagonal (* radius 0.70710677))
         (miter-scale
           (vec3 (if (= (swizzle absolute :x) 3.0) radius diagonal)
                 (if (= (swizzle absolute :y) 3.0) radius diagonal)
                 (if (= (swizzle absolute :z) 3.0) radius diagonal))))
    (if (vertex-star-miter moment)
        (* q miter-scale)
        (* q (* width ordinary-reach)))))

(define-shader-function vertex-half-edge-count (star axis direction)
  (if (= axis (uint 0.0))
      (if (< direction 0.0)
          (+ (+ (uint (ldb (byte 1 0) star))
                (uint (ldb (byte 1 2) star)))
             (+ (uint (ldb (byte 1 4) star))
                (uint (ldb (byte 1 6) star))))
          (+ (+ (uint (ldb (byte 1 1) star))
                (uint (ldb (byte 1 3) star)))
             (+ (uint (ldb (byte 1 5) star))
                (uint (ldb (byte 1 7) star)))))
      (if (= axis (uint 1.0))
          (if (< direction 0.0)
              (+ (+ (uint (ldb (byte 1 0) star))
                    (uint (ldb (byte 1 1) star)))
                 (+ (uint (ldb (byte 1 4) star))
                    (uint (ldb (byte 1 5) star))))
              (+ (+ (uint (ldb (byte 1 2) star))
                    (uint (ldb (byte 1 3) star)))
                 (+ (uint (ldb (byte 1 6) star))
                    (uint (ldb (byte 1 7) star)))))
          (if (< direction 0.0)
              (+ (+ (uint (ldb (byte 1 0) star))
                    (uint (ldb (byte 1 1) star)))
                 (+ (uint (ldb (byte 1 2) star))
                    (uint (ldb (byte 1 3) star))))
              (+ (+ (uint (ldb (byte 1 4) star))
                    (uint (ldb (byte 1 5) star)))
                 (+ (uint (ldb (byte 1 6) star))
                    (uint (ldb (byte 1 7) star))))))))

(define-shader-function vertex-star-half-offset
    (star axis direction axis-vector edge-q width)
  (let* ((moment (vertex-star-moment star))
         (m (swizzle moment :xyz))
         (q (signum m))
         (absolute (abs m))
         (dominant
           (vec3 (if (= (swizzle absolute :x) 3.0) (swizzle q :x) 0.0)
                 (if (= (swizzle absolute :y) 3.0) (swizzle q :y) 0.0)
                 (if (= (swizzle absolute :z) 3.0) (swizzle q :z) 0.0)))
         (axis-dominant (dot absolute axis-vector))
         (axis-q (dot q axis-vector))
         (other (- (- q dominant) (* axis-vector axis-q)))
         (radius (* width 0.5))
         (arc-end
           (+ (* dominant radius)
              (* other radius)))
         (center (vertex-star-center-offset star width))
         (ring (+ (* axis-vector (* direction width))
                  (* edge-q radius))))
    (if (vertex-star-miter moment)
        (if (= axis-dominant 3.0)
            (mix center ring 0.5)
            (if (= direction (- axis-q))
                arc-end
                (mix center ring 0.5)))
        (mix center ring 0.5))))

(define-shader-function construction-mask-bit (packed bit)
  (if (< bit (uint 16.0))
      (float (ldb (byte 1 bit) (uint (swizzle packed :x))))
      (float (ldb (byte 1 (- bit (uint 16.0)))
                  (uint (swizzle packed :y))))))

(define-shader face-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-position-output :vec3 :location 0)
               (face-normal-output :vec3 :location 1
                                   :interpolation :flat)
               (stock-output :float :location 2
                             :interpolation :flat)
               (lattice-output :vec2 :location 3)
               (current-clip-output :vec4 :location 4)
               (previous-clip-output :vec4 :location 5)
               (construction-mask-output :vec2 :location 6
                                         :interpolation :flat))
     :resources ((faces :storage-buffer :binding 0 :element :uvec4)
                 (camera-state :uniform-block :binding 1
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (chamfer-parameters :vec4)
                            (previous-camera-position :vec4)
                            (previous-camera-right :vec4)
                            (previous-camera-up :vec4)
                            (previous-camera-forward :vec4)
                            (previous-camera-projection :vec4)
                            (temporal-parameters :vec4)
                            (inspection-parameters :vec4)))))
  (let* ((record (buffer-element faces instance-index))
         (site-low (swizzle record :x))
         (site-high (swizzle record :y))
         (shape (swizzle record :z))
         (construction-mask (swizzle record :w))
         (extent (uint (ldb (byte 3 0) site-low)))
         (negative (uint (ldb (byte 1 3) site-low)))
         (zero (uint 0.0))
         (one (uint 1.0))
         (two (uint 2.0))
         (three (uint 3.0))
         (four (uint 4.0))
         (sixteen (uint 16.0))
         (grid-point (< vertex-index sixteen))
         (grid-index (if grid-point vertex-index zero))
         (extra-index (if grid-point zero (- vertex-index sixteen)))
         (i (/ grid-index four))
         (j (mod grid-index four))
         (i-boundary (if (= i zero) (= i zero) (= i three)))
         (j-boundary (if (= j zero) (= j zero) (= j three)))
         (point-kind
           (if grid-point
               (if i-boundary
                   (if j-boundary two one)
                   (if j-boundary one zero))
               three))
         (extra-corner (/ extra-index two))
         (extra-tangent-v (= (mod extra-index two) one))
         (i-high (if (= i two) (= i two) (= i three)))
         (j-high (if (= j two) (= j two) (= j three)))
         (corner-index
           (if grid-point
               (+ (if i-high two zero) (if j-high one zero))
               extra-corner))
         (corner-u-high (= (/ corner-index two) one))
         (corner-v-high (= (mod corner-index two) one))
         (star (uint (ldb (byte 8 (* corner-index (uint 8.0))) shape)))
         (x (float (ldb (byte 24 4) site-low)))
         (y (float (+ (ldb (byte 4 28) site-low)
                      (* (ldb (byte 20 0) site-high) (uint 16.0)))))
         (z (float (ldb (byte 8 20) site-high)))
         (stock (float (ldb (byte 4 28) site-high)))
         (u (if (= extent (uint 6.0))
                (vec3 0.0 1.0 0.0)
                (vec3 1.0 0.0 0.0)))
         (v (if (= extent (uint 3.0))
                (vec3 0.0 1.0 0.0)
                (vec3 0.0 0.0 1.0)))
         (u-axis (if (= extent (uint 6.0)) one zero))
         (v-axis (if (= extent (uint 3.0)) one two))
         (canonical-normal
           (if (= extent (uint 3.0))
               (vec3 0.0 0.0 1.0)
               (if (= extent (uint 5.0))
                   (vec3 0.0 -1.0 0.0)
                   (vec3 1.0 0.0 0.0))))
         (face-normal (if (= negative one)
                          (* canonical-normal -1.0)
                          canonical-normal))
         (width (swizzle chamfer-parameters :x))
         (lambda-i (if (= i zero) 0.0
                       (if (= i one) width
                           (if (= i (uint 2.0)) (- 1.0 width) 1.0))))
         (lambda-j (if (= j zero) 0.0
                       (if (= j one) width
                           (if (= j two) (- 1.0 width) 1.0))))
         (face-anchor (vec3 x y z))
         (flat-position (+ face-anchor (+ (* u lambda-i) (* v lambda-j))))
         (vertex-position
           (+ face-anchor
              (+ (* u (if corner-u-high 1.0 0.0))
                 (* v (if corner-v-high 1.0 0.0)))))
         (grid-edge-axis (if i-boundary v-axis u-axis))
         (grid-edge-direction
           (if i-boundary
               (if (= j one) 1.0 -1.0)
               (if (= i one) 1.0 -1.0)))
         (grid-edge-count
           (vertex-half-edge-count
            star grid-edge-axis grid-edge-direction))
         (edge-code
           (if (= grid-edge-count one)
               one
               (if (= grid-edge-count two) zero two)))
         (grid-edge-outward-tangent (if i-boundary u v))
         (grid-edge-side
           (if i-boundary
               (if (= i zero) -1.0 1.0)
               (if (= j zero) -1.0 1.0)))
         (grid-edge-outward
           (* grid-edge-outward-tangent grid-edge-side))
         (edge-normal-sign (if (= edge-code one) -1.0 1.0))
         (grid-edge-q
           (if (= edge-code zero)
               (vec3 0.0 0.0 0.0)
               (- (* face-normal edge-normal-sign) grid-edge-outward)))
         (half-axis (if extra-tangent-v v-axis u-axis))
         (half-axis-vector (if extra-tangent-v v u))
         (half-direction
           (if extra-tangent-v
               (if corner-v-high -1.0 1.0)
               (if corner-u-high -1.0 1.0)))
         (half-outward-tangent (if extra-tangent-v u v))
         (half-outward-side
           (if extra-tangent-v
               (if corner-u-high 1.0 -1.0)
               (if corner-v-high 1.0 -1.0)))
         (half-outward (* half-outward-tangent half-outward-side))
         (half-edge-count
           (vertex-half-edge-count star half-axis half-direction))
         (half-edge-code
           (if (= half-edge-count one)
               one
               (if (= half-edge-count two) zero two)))
         (half-normal-sign (if (= half-edge-code one) -1.0 1.0))
         (half-edge-q
           (if (= half-edge-code zero)
               (vec3 0.0 0.0 0.0)
               (- (* face-normal half-normal-sign) half-outward)))
         (center-offset (vertex-star-center-offset star width))
         (half-offset
           (vertex-star-half-offset
            star half-axis half-direction half-axis-vector half-edge-q width))
         (world-position
           (if (= point-kind three)
               (+ vertex-position half-offset)
               (if (= point-kind two)
                   (+ vertex-position center-offset)
                   (if (= point-kind one)
                       (+ flat-position (* grid-edge-q (* width 0.5)))
                       flat-position))))
         (lattice-position
           (if (= point-kind three)
               (if extra-tangent-v
                   (vec2 (if corner-u-high 3.0 0.0)
                         (if corner-v-high 2.5 0.5))
                   (vec2 (if corner-u-high 2.5 0.5)
                         (if corner-v-high 3.0 0.0)))
               (vec2 (float i) (float j))))
         (current-clip
           (face-view-clip world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle chamfer-parameters :z)))
         (previous-clip
           (face-view-clip world-position previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle temporal-parameters :z)))
         (jitter (swizzle temporal-parameters :xy)))
    ;; The projection lane is packed so that both projections share these
    ;; three rows; only the homogeneous divisor tells them apart.  Dividing
    ;; by the view depth is what makes a picture perspective, and dividing
    ;; by one is what makes it isometric.
    (set-output clip-position
                (vec4 (+ (swizzle current-clip :x)
                         (* (swizzle jitter :x) (swizzle current-clip :w)))
                      (+ (swizzle current-clip :y)
                         (* (swizzle jitter :y) (swizzle current-clip :w)))
                      (swizzle current-clip :z)
                      (swizzle current-clip :w)))
    (set-output world-position-output world-position)
    (set-output face-normal-output face-normal)
    (set-output stock-output stock)
    ;; The extra half-points sit midway in the topology parameter domain even
    ;; when their world positions move to the circular miter endpoints.
    (set-output lattice-output lattice-position)
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)
    ;; Split 29 bits into exact float lanes; the fragment stage queries each
    ;; lane before recombining, so no large integer passes through float32.
    (set-output construction-mask-output
                (vec2 (float (ldb (byte 16 0) construction-mask))
                      (float (ldb (byte 13 16) construction-mask))))))

(define-shader face-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0)
              (face-normal :vec3 :location 1 :interpolation :flat)
              (stock :float :location 2 :interpolation :flat)
              (lattice :vec2 :location 3)
              (current-clip :vec4 :location 4)
              (previous-clip :vec4 :location 5)
              (construction-mask-value :vec2 :location 6
                                       :interpolation :flat))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     ;; The same block the vertex stage reads, declared identically so the
     ;; one uniform buffer serves both stages.
     :resources ((camera-state :uniform-block :binding 1
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (chamfer-parameters :vec4)
                            (previous-camera-position :vec4)
                            (previous-camera-right :vec4)
                            (previous-camera-up :vec4)
                            (previous-camera-forward :vec4)
                            (previous-camera-projection :vec4)
                            (temporal-parameters :vec4)
                            (inspection-parameters :vec4)))))
  (let* ((dx (derivative-x world-position))
         (dy (derivative-y world-position))
         (geometric-normal
           (normalize
            (vec3 (- (* (swizzle dx :y) (swizzle dy :z))
                     (* (swizzle dx :z) (swizzle dy :y)))
                  (- (* (swizzle dx :z) (swizzle dy :x))
                     (* (swizzle dx :x) (swizzle dy :z)))
                  (- (* (swizzle dx :x) (swizzle dy :y))
                     (* (swizzle dx :y) (swizzle dy :x))))))
         (normal (if (< (dot geometric-normal face-normal) 0.0)
                     (* geometric-normal -1.0)
                     geometric-normal))
         (tone (if (< stock 0.5)
                   (vec3 0.17 0.36 0.11)
                   (if (< stock 1.5)
                       (vec3 0.42 0.32 0.21)
                       (if (< stock 2.5)
                           (vec3 0.24 0.18 0.13)
                           (vec3 0.53 0.49 0.39)))))
         ;; Stable patch-scale value drift and an independent warmth field.
         (cell (floor (- world-position (* normal 0.25))))
         (patch (- (paper-noise (* cell 0.21)) 0.5))
         (jitter (- (paper-hash cell) 0.5))
         (warm-patch
           (paper-noise (+ (* cell 0.13) (vec3 19.7 7.3 3.1))))
         (value (+ 1.0
                   (* #.*paper-variation*
                      (+ (* 1.35 patch) (* 0.45 jitter)))))
         (warmth (mix (vec3 0.965 0.99 1.04) (vec3 1.04 1.01 0.96)
                      warm-patch))
         (base (* tone (* warmth value)))
         ;; The old :GOLDEN light: honeyed low sun, blue sky fill, and warm
         ;; ground bounce.  Half-Lambert wraps the terminator like folded paper.
         (sun (normalize (vec3 -0.62 0.38 0.34)))
         (sun-color (vec3 1.22 1.00 0.72))
         (sky (vec3 0.60 0.75 0.96))
         (ground (vec3 0.40 0.34 0.26))
         (fill-direction (normalize (vec3 0.55 -0.40 0.32)))
         (facing (dot normal sun))
         (wrapped (expt (clamp (+ 0.5 (* 0.5 facing)) 0.0 1.0) 1.55))
         (fill (* 0.30 (max 0.0 (dot normal fill-direction))))
         (upness (swizzle normal :z))
         (sky-weight (+ 0.5 (* 0.5 upness)))
         (ground-weight (- 0.5 (* 0.5 upness)))
         (light (+ (* sun-color wrapped)
                   (+ (* sky (+ (* 0.44 sky-weight) fill))
                      (* ground (* 0.44 ground-weight)))))
         (lit (* base (* light (stock-tooth world-position))))
         (mapped-paper (paper-tonemap (* lit 1.16)))
         (camera-delta (- world-position (swizzle camera-position :xyz)))
         (distance (sqrt (dot camera-delta camera-delta)))
         (fog (smoothstep 165.0 300.0 distance))
         (paper (mix mapped-paper sky fog))
         ;; Distance to the fixed raster topology.  The five ordinary quads
         ;; keep their old diagonal.  A corner cell is a six-vertex patch with
         ;; three spokes: centre-to-inner and one half-edge-to-inner from each
         ;; incident strip.  Derivatives establish screen-space width only;
         ;; RECORD.W says which shared edges actually turn between facets.
         (u (swizzle lattice :x))
         (v (swizzle lattice :y))
         (cell-u (floor (clamp u 0.0 2.9999)))
         (cell-v (floor (clamp v 0.0 2.9999)))
         (local-u (- u cell-u))
         (local-v (- v cell-v))
         (corner-u
           (if (= cell-u 0.0) (= cell-u 0.0) (= cell-u 2.0)))
         (corner-v
           (if (= cell-v 0.0) (= cell-v 0.0) (= cell-v 2.0)))
         (corner-cell
           (if corner-u corner-v (= cell-u -1.0)))
         (corner-factor (if corner-cell 1.0 0.0))
         (ordinary-factor (- 1.0 corner-factor))
         (corner-u-high (if (= cell-u 2.0) 1.0 0.0))
         (corner-v-high (if (= cell-v 2.0) 1.0 0.0))
         (corner-a (if (= corner-u-high 1.0)
                       (- 1.0 local-u) local-u))
         (corner-b (if (= corner-v-high 1.0)
                       (- 1.0 local-v) local-v))
         (diagonal-toward-c00
           (if (< cell-u 2.0) (< cell-v 2.0) (= cell-v 2.0)))
         (d (if diagonal-toward-c00
                (- local-u local-v)
                (- (+ local-u local-v) 1.0)))
         (lattice-dx (derivative-x lattice))
         (lattice-dy (derivative-y lattice))
         (a-dx (if (= corner-u-high 1.0)
                   (- (swizzle lattice-dx :x))
                   (swizzle lattice-dx :x)))
         (a-dy (if (= corner-u-high 1.0)
                   (- (swizzle lattice-dy :x))
                   (swizzle lattice-dy :x)))
         (b-dx (if (= corner-v-high 1.0)
                   (- (swizzle lattice-dx :y))
                   (swizzle lattice-dx :y)))
         (b-dy (if (= corner-v-high 1.0)
                   (- (swizzle lattice-dy :y))
                   (swizzle lattice-dy :y)))
         (u-width (+ (abs (swizzle lattice-dx :x))
                     (abs (swizzle lattice-dy :x))))
         (v-width (+ (abs (swizzle lattice-dx :y))
                     (abs (swizzle lattice-dy :y))))
         (d-width
           (if diagonal-toward-c00
               (+ (abs (- (swizzle lattice-dx :x)
                          (swizzle lattice-dx :y)))
                  (abs (- (swizzle lattice-dy :x)
                          (swizzle lattice-dy :y))))
               (+ (abs (+ (swizzle lattice-dx :x)
                          (swizzle lattice-dx :y)))
                  (abs (+ (swizzle lattice-dy :x)
                          (swizzle lattice-dy :y))))))
         (corner-d0 (- corner-a corner-b))
         (corner-d1 (- (- (* corner-a 2.0) corner-b) 1.0))
         (corner-d2 (+ (- corner-a (* corner-b 2.0)) 1.0))
         (corner-d0-width
           (+ (abs (- a-dx b-dx)) (abs (- a-dy b-dy))))
         (corner-d1-width
           (+ (abs (- (* a-dx 2.0) b-dx))
              (abs (- (* a-dy 2.0) b-dy))))
         (corner-d2-width
           (+ (abs (- a-dx (* b-dx 2.0)))
              (abs (- a-dy (* b-dy 2.0)))))
         (u-line (floor (+ u 0.5)))
         (v-line (floor (+ v 0.5)))
         (u-pixels (/ (abs (- u u-line)) (max u-width 0.000001)))
         (v-pixels (/ (abs (- v v-line)) (max v-width 0.000001)))
         (d-pixels (/ (abs d) (max d-width 0.000001)))
         (corner-d0-pixels
           (/ (abs corner-d0) (max corner-d0-width 0.000001)))
         (corner-d1-pixels
           (/ (abs corner-d1) (max corner-d1-width 0.000001)))
         (corner-d2-pixels
           (/ (abs corner-d2) (max corner-d2-width 0.000001)))
         (u-bit
           (uint (+ (* (clamp (- u-line 1.0) 0.0 1.0) 3.0) cell-v)))
         (v-bit
           (uint (+ 6.0
                    (+ (* (clamp (- v-line 1.0) 0.0 1.0) 3.0)
                       cell-u))))
         (d-bit
           (uint (if (= cell-u 0.0)
                     12.0
                     (if (= cell-u 1.0) (+ 13.0 cell-v) 16.0))))
         (corner-bit
           (uint (+ 17.0
                    (* 3.0 (+ (* corner-u-high 2.0) corner-v-high)))))
         (u-approved
           (if (= u-line 1.0)
               (construction-mask-bit construction-mask-value u-bit)
               (if (= u-line 2.0)
                   (construction-mask-bit construction-mask-value u-bit)
                   0.0)))
         (v-approved
           (if (= v-line 1.0)
               (construction-mask-bit construction-mask-value v-bit)
               (if (= v-line 2.0)
                   (construction-mask-bit construction-mask-value v-bit)
                   0.0)))
         (d-approved
           (* ordinary-factor
              (construction-mask-bit construction-mask-value d-bit)))
         (corner-d0-approved
           (* corner-factor
              (construction-mask-bit construction-mask-value corner-bit)))
         (corner-d1-approved
           (* corner-factor
              (construction-mask-bit construction-mask-value
                                     (+ corner-bit (uint 1.0)))))
         (corner-d2-approved
           (* corner-factor
              (construction-mask-bit construction-mask-value
                                     (+ corner-bit (uint 2.0)))))
         ;; Outer rims are literal one-site boundaries of the cubical face;
         ;; internal edges appear only when the adjacent triangle normals turn.
         (u-rim (- 1.0 (step 0.5 (min u-line (- 3.0 u-line)))))
         (v-rim (- 1.0 (step 0.5 (min v-line (- 3.0 v-line)))))
         (u-wire (- 1.0 (smoothstep 0.35 1.15 u-pixels)))
         (v-wire (- 1.0 (smoothstep 0.35 1.15 v-pixels)))
         (d-wire (- 1.0 (smoothstep 0.35 1.15 d-pixels)))
         (corner-d0-wire
           (- 1.0 (smoothstep 0.35 1.15 corner-d0-pixels)))
         (corner-d1-wire
           (- 1.0 (smoothstep 0.35 1.15 corner-d1-pixels)))
         (corner-d2-wire
           (- 1.0 (smoothstep 0.35 1.15 corner-d2-pixels)))
         (corner-construction-wire
           (max (* corner-d0-wire corner-d0-approved)
                (max (* corner-d1-wire corner-d1-approved)
                     (* corner-d2-wire corner-d2-approved))))
         (construction-wire
           (* (swizzle chamfer-parameters :y)
              (max (max (* u-wire (max (* u-rim 0.48) u-approved))
                        (* v-wire (max (* v-rim 0.48) v-approved)))
                   (max (* d-wire d-approved)
                        corner-construction-wire))))
         ;; #JM9807 now survives as precise drafting semantics: the global
         ;; layer draws actual facet-normal discontinuities, while the pointer
         ;; lens exposes the complete local triangulation without any area fill.
         ;; CURRENT-CLIP is deliberately unjittered.
         (fragment-uv (face-clip-uv current-clip))
         (pointer-delta
           (/ (- fragment-uv (swizzle inspection-parameters :xy))
              (max (swizzle inspection-parameters :zw)
                   (vec2 0.000001 0.000001))))
         (pointer-pixels (sqrt (dot pointer-delta pointer-delta)))
         (pointer-enabled (swizzle chamfer-parameters :w))
         (lens (* pointer-enabled
                  (- 1.0 (smoothstep 34.0 46.0 pointer-pixels))))
         (corner-mesh-wire
           (* corner-factor
              (max corner-d0-wire
                   (max corner-d1-wire corner-d2-wire))))
         (mesh-wire
           (* lens
              (max (max u-wire v-wire)
                   (max (* ordinary-factor d-wire) corner-mesh-wire))))
         (ring (* pointer-enabled
                  (- 1.0
                     (smoothstep 0.35 1.25
                                 (abs (- pointer-pixels 11.0))))))
         (tick-range (* (step 14.0 pointer-pixels)
                        (- 1.0 (step 23.0 pointer-pixels))))
         (vertical-tick
           (* pointer-enabled tick-range
              (- 1.0
                 (smoothstep 0.35 1.15
                             (abs (swizzle pointer-delta :x))))))
         (horizontal-tick
           (* pointer-enabled tick-range
              (- 1.0
                 (smoothstep 0.35 1.15
                             (abs (swizzle pointer-delta :y))))))
         (center (* pointer-enabled
                    (- 1.0 (smoothstep 0.7 1.8 pointer-pixels))))
         (reticle (max center (max ring (max vertical-tick horizontal-tick))))
         (construction-ink (vec3 0.055 0.16 0.22))
         (blueprint (vec3 0.30 0.90 0.94))
         (drafted (mix paper construction-ink construction-wire))
         (radiance (mix drafted blueprint (max (* mesh-wire 0.72) reticle))))
    (set-output color-output (vec4 radiance 1.0))
    (set-output motion-output
                (- (face-clip-uv previous-clip)
                   (face-clip-uv current-clip)))))

(define-shader present-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (ndc-output :vec2 :location 0)))
  (let* ((index (float vertex-index))
         (x (if (< index 0.5) -1.0 (if (< index 1.5) 3.0 -1.0)))
         (y (if (< index 1.5) -1.0 3.0)))
    (set-output clip-position (vec4 x y 0.0 1.0))
    (set-output ndc-output (vec2 x y))))

(define-shader present-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity)
                 (scene-sampler :sampler :binding 1)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (value (sample scene scene-sampler uv)))
    (set-output color-output (vec4 (swizzle value :xyz) 1.0))))

(define-shader inspector-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv-output :vec2 :location 0))
     :resources ((inspector-state :uniform-block :binding 0
                  :members ((inspector-rect :vec4)))))
  (let* ((index (float vertex-index))
         (right (if (= index 2.0) 1.0
                    (if (= index 3.0) 1.0
                        (if (= index 5.0) 1.0 0.0))))
         (bottom (if (= index 1.0) 1.0
                     (if (= index 4.0) 1.0
                         (if (= index 5.0) 1.0 0.0))))
         (x (mix (swizzle inspector-rect :x)
                 (swizzle inspector-rect :z) right))
         (y (mix (swizzle inspector-rect :y)
                 (swizzle inspector-rect :w) bottom)))
    (set-output clip-position (vec4 x y 0.0 1.0))
    (set-output uv-output (vec2 right bottom))))

(define-shader inspector-fragment-specification
    (:stage :fragment
     :inputs ((uv :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((inspector :texture-2d :binding 1
                  :sample-transfer :identity)
                 (inspector-sampler :sampler :binding 2)))
  ;; McCLIM's raster mirror has a top-left origin; sampled textures do not.
  (set-output color-output
              (sample inspector inspector-sampler
                      (vec2 (swizzle uv :x)
                            (- 1.0 (swizzle uv :y))))))
