(in-package #:luft.render.shaders)

;;; Site-stream rendering. One UVec4 instance selects a lattice base and a
;;; canonical fixed-stride template; the template vertex is a small exact
;;; offset plus geometric attributes. The CPU classifies sites and the vertex
;;; shader realizes the renderer-global triangle and quad populations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *stock-tooth* 0.055)
  (defvar *paper-variation* 0.11)
  (defvar *local-ambient-occlusion-strength* 0.28)
  (defvar *cut-edge-lift-strength* 0.24)
  (defvar *screen-ambient-occlusion-strength* 0.38)
  (defvar *screen-ambient-occlusion-radius* 0.95)
  (defvar *ambient-pigment-strength* 0.82))

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

(define-shader-function stock-tooth (point)
  (let* ((coarse (paper-noise (* point 11.0)))
         (fine (paper-noise (* point 31.0))))
    (+ 1.0
       (* #.*stock-tooth* (- (+ (* 0.55 coarse) (* 0.45 fine)) 0.5)))))

(define-shader-function paper-tonemap (radiance)
  (let* ((numerator
           (* radiance (+ (* radiance 2.51) (vec3 0.03 0.03 0.03))))
         (denominator
           (+ (* radiance (+ (* radiance 2.43) (vec3 0.59 0.59 0.59)))
              (vec3 0.14 0.14 0.14))))
    (clamp (/ numerator denominator)
           (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))))

(define-shader-function mesh-view-clip
    (point position right up forward projection divisor)
  (let* ((relative (- point (swizzle position :xyz)))
         (view-x (dot relative (swizzle right :xyz)))
         (view-y (dot relative (swizzle up :xyz)))
         (view-z (dot relative (swizzle forward :xyz))))
    (vec4 (* view-x (swizzle projection :x))
          (- (* view-y (swizzle projection :y)))
          (+ (* view-z (swizzle projection :z))
             (swizzle projection :w))
          (mix 1.0 view-z divisor))))

(define-shader-function mesh-clip-uv (clip)
  (+ (* (/ (swizzle clip :xy) (swizzle clip :w)) 0.5)
     (vec2 0.5 0.5)))

;;; A small, rounded traveler for the sanctuary bridge.  The proxy is only a
;;; conservative raster bound; every visible contour below comes from this
;;; analytic field.  LUFT is Z-up, unlike luvcraft's first-person figures.

(define-shader-function player-sdf-length (vector)
  (sqrt (max (dot vector vector) 1e-12)))

(define-shader-function player-sdf-smooth-union (first second width)
  (let* ((safe-width (max width 1e-5))
         (blend (clamp (+ 0.5 (* 0.5 (/ (- second first) safe-width)))
                       0.0 1.0)))
    (- (mix second first blend)
       (* width (* blend (- 1.0 blend))))))

(define-shader-function player-sdf-sphere (point center radius)
  (- (player-sdf-length (- point center)) radius))

(define-shader-function player-sdf-ellipsoid (point center radii)
  (let* ((offset (/ (- point center) radii))
         (outer (player-sdf-length offset))
         (gradient (player-sdf-length (/ offset radii))))
    (/ (* outer (- outer 1.0)) (max gradient 1e-6))))

(define-shader-function player-sdf-capsule (point beginning end radius)
  (let* ((offset (- point beginning))
         (axis (- end beginning))
         (fraction (clamp (/ (dot offset axis)
                             (max (dot axis axis) 1e-6))
                          0.0 1.0)))
    (- (player-sdf-length (- offset (* axis fraction))) radius)))

(define-shader-function player-walk-pose (point gait activity)
  "Walk the sample point back into the figure's gently weighted body frame."
  (vec3 (swizzle point :x)
        (swizzle point :y)
        (- (swizzle point :z)
           (* 0.060 (* activity (abs (sin gait)))))))

(define-shader-function player-tunic-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 0.0 1.67)
                        (vec3 0.50 0.32 0.64)))

(define-shader-function player-head-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 -0.035 2.56)
                        (vec3 0.39 0.35 0.38)))

(define-shader-function player-face-distance (point)
  (player-sdf-sphere point (vec3 0.0 0.335 2.54) 0.13))

(define-shader-function player-hair-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 -0.11 2.70)
                        (vec3 0.405 0.31 0.285)))

(define-shader-function player-collar-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 -0.015 2.20)
                        (vec3 0.39 0.31 0.14)))

(define-shader-function player-pack-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 -0.285 1.66)
                        (vec3 0.37 0.21 0.47)))

(define-shader-function player-leg-distance (point gait activity)
  (let* ((side (if (< (swizzle point :x) 0.0) -1.0 1.0))
         ;; The stride radius equals bridge-speed / gait-speed.  During each
         ;; stance half-cycle one ankle therefore cancels the root motion in
         ;; world space instead of skating across the stone.
         (swing (* side (* 0.279 (sin gait))))
         (lift (* 0.17
                  (* activity (max 0.0 (* side (cos gait))))))
         (hip (vec3 (* side 0.22) 0.0
                    (+ 1.27 (* 0.035 (* activity (abs (sin gait)))))))
         (ankle (vec3 (* side 0.22) swing (+ 0.26 lift)))
         (leg (player-sdf-capsule point hip ankle 0.17))
         (boot (player-sdf-ellipsoid
                point (vec3 (* side 0.22) (+ swing 0.09) (+ 0.16 lift))
                (vec3 0.22 0.31 0.16))))
    (player-sdf-smooth-union leg boot 0.075)))

(define-shader-function player-arm-distance (point gait)
  (let* ((side (if (< (swizzle point :x) 0.0) -1.0 1.0))
         (swing (* side (* -0.19 (sin gait))))
         (shoulder (vec3 (* side 0.46) 0.0 1.91))
         (hand (vec3 (* side 0.55) swing 1.18)))
    (player-sdf-capsule point shoulder hand 0.16)))

(define-shader-function player-hand-distance (point gait)
  (let* ((side (if (< (swizzle point :x) 0.0) -1.0 1.0))
         (swing (* side (* -0.19 (sin gait)))))
    (player-sdf-sphere point (vec3 (* side 0.55) swing 1.13) 0.19)))

(define-shader-function player-distance (point gait activity)
  (let* ((posed (player-walk-pose point gait activity))
         (body (player-sdf-smooth-union
                (player-tunic-distance posed)
                (player-head-distance posed) 0.095))
         (face (player-face-distance posed))
         (hair (player-hair-distance posed))
         (collar (player-collar-distance posed))
         (pack (player-pack-distance posed))
         ;; Legs use the un-bobbed frame: the planted boot stays on the deck
         ;; while the hips and upper body carry the weight shift.
         (legs (player-leg-distance point gait activity))
         (arms (player-arm-distance posed gait))
         (hands (player-hand-distance posed gait)))
    (player-sdf-smooth-union
     (player-sdf-smooth-union
      (player-sdf-smooth-union
       (player-sdf-smooth-union body face 0.055)
       (player-sdf-smooth-union hair collar 0.06) 0.055)
      (player-sdf-smooth-union pack legs 0.075) 0.065)
     (player-sdf-smooth-union arms hands 0.07) 0.075)))

(define-shader-function player-normal (point gait activity)
  (let* ((reach 0.004)
         (a (vec3 1.0 -1.0 -1.0))
         (b (vec3 -1.0 -1.0 1.0))
         (c (vec3 -1.0 1.0 -1.0))
         (d (vec3 1.0 1.0 1.0)))
    (normalize
     (+ (+ (* a (player-distance (+ point (* a reach)) gait activity))
           (* b (player-distance (+ point (* b reach)) gait activity)))
        (+ (* c (player-distance (+ point (* c reach)) gait activity))
           (* d (player-distance (+ point (* d reach)) gait activity)))))))

(define-shader-function player-albedo (point gait activity)
  (let* ((posed (player-walk-pose point gait activity))
         (tunic (min (player-tunic-distance posed)
                     (player-arm-distance posed gait)))
         (skin (min (min (player-head-distance posed)
                         (player-face-distance posed))
                    (player-hand-distance posed gait)))
         (hair (player-hair-distance posed))
         (collar (player-collar-distance posed))
         (pack (player-pack-distance posed))
         (legs (player-leg-distance point gait activity))
         (skin-near (step skin tunic))
         (first (mix (vec3 0.105 0.25 0.29)
                     (vec3 0.68 0.43 0.29) skin-near))
         (distance (min tunic skin))
         (hair-near (step hair distance))
         (second (mix first (vec3 0.115 0.075 0.050) hair-near))
         (distance-2 (min distance hair))
         (collar-near (step collar distance-2))
         (third (mix second (vec3 0.68 0.39 0.10) collar-near))
         (distance-3 (min distance-2 collar))
         (pack-near (step pack distance-3))
         (fourth (mix third (vec3 0.28 0.17 0.095) pack-near))
         (distance-4 (min distance-3 pack))
         (leg-near (step legs distance-4)))
    (mix fourth (vec3 0.16 0.12 0.095) leg-near)))

(define-shader player-sdf-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position-output :vec3 :location 0)
               (center-radius-output :vec4 :location 1)
               (current-clip-output :vec4 :location 2)
               (previous-clip-output :vec4 :location 3))
     :resources ((camera-state :uniform-block :binding 0
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (render-parameters :vec4)
                            (previous-camera-position :vec4)
                            (previous-camera-right :vec4)
                            (previous-camera-up :vec4)
                            (previous-camera-forward :vec4)
                            (previous-camera-projection :vec4)
                            (temporal-parameters :vec4)
                            (inspection-parameters :vec4)
                            (character-parameters :vec4)))))
  (let* ((index (float vertex-index))
         (right-corner (if (= index 2.0) 1.0
                           (if (= index 3.0) 1.0
                               (if (= index 5.0) 1.0 0.0))))
         (bottom-corner (if (= index 1.0) 1.0
                            (if (= index 4.0) 1.0
                                (if (= index 5.0) 1.0 0.0))))
         (corner (vec2 (- (* right-corner 2.0) 1.0)
                       (- (* bottom-corner 2.0) 1.0)))
         (time (swizzle character-parameters :w))
         (previous-time (- time 0.0166667))
         (path (* time 0.22))
         (previous-path (* previous-time 0.22))
         (center (vec3 (swizzle character-parameters :x)
                       (+ (swizzle character-parameters :y)
                          (* 10.5 (sin path)))
                       (swizzle character-parameters :z)))
         (previous-center (vec3 (swizzle character-parameters :x)
                                (+ (swizzle character-parameters :y)
                                   (* 10.5 (sin previous-path)))
                                (swizzle character-parameters :z)))
         (radius 1.72)
         (proxy-world-position
           (+ (- center (* (swizzle camera-forward :xyz) radius))
              (+ (* (swizzle camera-right :xyz)
                    (* (swizzle corner :x) radius))
                 (* (swizzle camera-up :xyz)
                    (* (swizzle corner :y) radius)))))
         (previous-proxy-world-position
           (+ (- previous-center
                 (* (swizzle previous-camera-forward :xyz) radius))
              (+ (* (swizzle previous-camera-right :xyz)
                    (* (swizzle corner :x) radius))
                 (* (swizzle previous-camera-up :xyz)
                    (* (swizzle corner :y) radius)))))
         (current-clip
           (mesh-view-clip proxy-world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle render-parameters :z)))
         (previous-clip
           (mesh-view-clip previous-proxy-world-position
                           previous-camera-position previous-camera-right
                           previous-camera-up previous-camera-forward
                           previous-camera-projection
                           (swizzle temporal-parameters :z)))
         (jitter (swizzle temporal-parameters :xy)))
    (set-output clip-position
                (vec4 (+ (swizzle current-clip :x)
                         (* (swizzle jitter :x) (swizzle current-clip :w)))
                      (+ (swizzle current-clip :y)
                         (* (swizzle jitter :y) (swizzle current-clip :w)))
                      (swizzle current-clip :z)
                      (swizzle current-clip :w)))
    (set-output proxy-world-position-output proxy-world-position)
    (set-output center-radius-output (vec4 center radius))
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)))

(define-shader player-sdf-fragment-specification
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0)
              (center-radius :vec4 :location 1)
              (current-clip :vec4 :location 2)
              (previous-clip :vec4 :location 3))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (render-parameters :vec4)
                            (previous-camera-position :vec4)
                            (previous-camera-right :vec4)
                            (previous-camera-up :vec4)
                            (previous-camera-forward :vec4)
                            (previous-camera-projection :vec4)
                            (temporal-parameters :vec4)
                            (inspection-parameters :vec4)
                            (character-parameters :vec4)))))
  (let* ((center (swizzle center-radius :xyz))
         (radius (swizzle center-radius :w))
         (time (swizzle character-parameters :w))
         (path (* time 0.22))
         (direction (if (< (cos path) 0.0) -1.0 1.0))
         (activity (abs (cos path)))
         ;; Six exact gait cycles from bridge centre to either turnaround.
         ;; Deriving phase from path position keeps cadence proportional to
         ;; travel speed and lets the figure settle before changing direction.
         (gait (* 37.6991118 (sin path)))
         (ray (if (< (swizzle render-parameters :z) 0.5)
                  (normalize (swizzle camera-forward :xyz))
                  (normalize (- proxy-world-position
                                (swizzle camera-position :xyz)))))
         (origin proxy-world-position)
         (travel
           (counted-fold (march 58.0 ray-distance 0.0)
             (let* ((world-point (+ origin (* ray ray-distance)))
                    (relative (- world-point center))
                    (local (vec3 (swizzle relative :x)
                                 (* direction (swizzle relative :y))
                                 (+ (swizzle relative :z) 1.48)))
                    (distance (player-distance local gait activity)))
               (if (< distance 0.0025)
                   ray-distance
                   (if (> ray-distance (* radius 2.0))
                       ray-distance
                       (+ ray-distance (max (* distance 0.82) 0.0025)))))))
         (world-point (+ origin (* ray travel)))
         (relative (- world-point center))
         (local (vec3 (swizzle relative :x)
                      (* direction (swizzle relative :y))
                      (+ (swizzle relative :z) 1.48)))
         (surface-distance (player-distance local gait activity))
         (coverage (* (step 0.0 time)
                      (- 1.0 (step 0.006 surface-distance))))
         (local-normal (player-normal local gait activity))
         (normal (normalize (vec3 (swizzle local-normal :x)
                                  (* direction (swizzle local-normal :y))
                                  (swizzle local-normal :z))))
         (albedo (player-albedo local gait activity))
         (sun (normalize (vec3 -0.62 0.38 0.34)))
         (sun-color (vec3 1.22 1.00 0.72))
         (sky (vec3 0.60 0.75 0.96))
         (ground (vec3 0.40 0.34 0.26))
         (wrapped (expt (clamp (+ 0.5 (* 0.5 (dot normal sun)))
                               0.0 1.0) 1.55))
         (upness (swizzle normal :z))
         (indirect (+ (* sky (* 0.44 (+ 0.5 (* 0.5 upness))))
                      (* ground (* 0.44 (- 0.5 (* 0.5 upness))))))
         (lit (* albedo (+ (* sun-color wrapped) indirect)))
         (paper (paper-tonemap (* lit 1.16)))
         (rim (expt (- 1.0 (max 0.0 (dot normal (* ray -1.0)))) 3.0))
         (radiance (+ paper (* (vec3 0.20 0.42 0.48) (* rim 0.07)))))
    (set-output color-output (vec4 (* radiance coverage) coverage))
    (set-output motion-output
                (- (mesh-clip-uv previous-clip)
                   (mesh-clip-uv current-clip)))))

(define-shader mesh-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-position-output :vec3 :location 0)
               (mesh-normal-output :vec3 :location 1 :interpolation :flat)
               (stock-output :float :location 2 :interpolation :flat)
               (barycentric-output :vec3 :location 3)
               (current-clip-output :vec4 :location 4)
               (previous-clip-output :vec4 :location 5)
               (kind-output :float :location 6 :interpolation :flat)
               (boundary-edge-mask-output :uint :location 7
                                          :interpolation :flat)
               (ambient-occlusion-output :float :location 8
                                          :interpolation :flat))
     :resources ((instances :storage-buffer :binding 0 :element :uvec4)
                 (template-vertices :storage-buffer :binding 1 :element :uvec4)
                 (camera-state :uniform-block :binding 2
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (render-parameters :vec4)
                            (previous-camera-position :vec4)
                            (previous-camera-right :vec4)
                            (previous-camera-up :vec4)
                            (previous-camera-forward :vec4)
                            (previous-camera-projection :vec4)
                            (temporal-parameters :vec4)
                            (inspection-parameters :vec4)
                            (character-parameters :vec4)))))
  (let* ((instance (buffer-element instances instance-index))
         (template-id
           (uint (ldb (byte 16 0) (swizzle instance :w))))
         (template-index
           (+ (* template-id (uint 6.0)) vertex-index))
         (template-vertex (buffer-element template-vertices template-index))
         (attributes (swizzle template-vertex :w))
         (world-position
           (/ (+ (* (vec3 (float (swizzle instance :x))
                          (float (swizzle instance :y))
                          (float (swizzle instance :z)))
                    8.0)
                 (- (vec3 (float (swizzle template-vertex :x))
                          (float (swizzle template-vertex :y))
                          (float (swizzle template-vertex :z)))
                    (vec3 16.0 16.0 16.0)))
              8.0))
         (mesh-normal
           (vec3 (- (float (ldb (byte 2 0) attributes)) 1.0)
                 (- (float (ldb (byte 2 2) attributes)) 1.0)
                 (- (float (ldb (byte 2 4) attributes)) 1.0)))
         (stock (float (ldb (byte 4 16) (swizzle instance :w))))
         (ambient-occlusion
           (/ (float (ldb (byte 2 20) (swizzle instance :w))) 3.0))
         (barycentric-index (uint (ldb (byte 2 6) attributes)))
         (kind (float (ldb (byte 2 8) attributes)))
         (boundary-edge-mask (uint (ldb (byte 3 10) attributes)))
         (barycentric
           (if (= barycentric-index (uint 0.0))
               (vec3 1.0 0.0 0.0)
               (if (= barycentric-index (uint 1.0))
                   (vec3 0.0 1.0 0.0)
                   (vec3 0.0 0.0 1.0))))
         (current-clip
           (mesh-view-clip world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle render-parameters :z)))
         (previous-clip
           (mesh-view-clip world-position previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle temporal-parameters :z)))
         (jitter (swizzle temporal-parameters :xy)))
    (set-output clip-position
                (vec4 (+ (swizzle current-clip :x)
                         (* (swizzle jitter :x) (swizzle current-clip :w)))
                      (+ (swizzle current-clip :y)
                         (* (swizzle jitter :y) (swizzle current-clip :w)))
                      (swizzle current-clip :z)
                      (swizzle current-clip :w)))
    (set-output world-position-output world-position)
    (set-output mesh-normal-output mesh-normal)
    (set-output stock-output stock)
    (set-output barycentric-output barycentric)
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)
    (set-output kind-output kind)
    (set-output boundary-edge-mask-output boundary-edge-mask)
    (set-output ambient-occlusion-output ambient-occlusion)))

(define-shader mesh-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0)
              (mesh-normal :vec3 :location 1 :interpolation :flat)
              (stock :float :location 2 :interpolation :flat)
              (barycentric :vec3 :location 3)
              (current-clip :vec4 :location 4)
              (previous-clip :vec4 :location 5)
              (kind :float :location 6 :interpolation :flat)
              (boundary-edge-mask :uint :location 7 :interpolation :flat)
              (ambient-occlusion :float :location 8 :interpolation :flat))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 2
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (render-parameters :vec4)
                            (previous-camera-position :vec4)
                            (previous-camera-right :vec4)
                            (previous-camera-up :vec4)
                            (previous-camera-forward :vec4)
                            (previous-camera-projection :vec4)
                            (temporal-parameters :vec4)
                            (inspection-parameters :vec4)
                            (character-parameters :vec4)))))
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
         (normal (if (< (dot geometric-normal mesh-normal) 0.0)
                     (* geometric-normal -1.0)
                     geometric-normal))
         (tone (if (< stock 0.5)
                   (vec3 0.17 0.36 0.11)
                   (if (< stock 1.5)
                       (vec3 0.42 0.32 0.21)
                       (if (< stock 2.5)
                           (vec3 0.24 0.18 0.13)
                           (vec3 0.53 0.49 0.39)))))
         (cell (floor (- world-position (* normal 0.25))))
         (patch (- (paper-noise (* cell 0.21)) 0.5))
         (jitter (- (paper-hash cell) 0.5))
         (warm-patch
           (paper-noise (+ (* cell 0.13) (vec3 19.7 7.3 3.1))))
         (value (+ 1.0 (* #.*paper-variation*
                           (+ (* 1.35 patch) (* 0.45 jitter)))))
         (warmth (mix (vec3 0.965 0.99 1.04) (vec3 1.04 1.01 0.96)
                      warm-patch))
         (piece-value (if (< kind 0.5) 1.0 (if (< kind 1.5) 0.96 1.04)))
         (base (* tone (* warmth (* value piece-value))))
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
         (indirect-light
           (+ (* sky (+ (* 0.44 sky-weight) fill))
              (* ground (* 0.44 ground-weight))))
         (ambient-accessibility
           (- 1.0 (* #.*local-ambient-occlusion-strength*
                     ambient-occlusion)))
         (light (+ (* sun-color wrapped)
                   (* indirect-light ambient-accessibility)))
         (lit (* base (* light (stock-tooth world-position))))
         (mapped-paper (paper-tonemap (* lit 1.16)))
         (camera-delta (- world-position (swizzle camera-position :xyz)))
         (distance (sqrt (dot camera-delta camera-delta)))
         (fog (smoothstep 165.0 300.0 distance))
         (paper (mix mapped-paper sky fog))
         (barycentric-dx (derivative-x barycentric))
         (barycentric-dy (derivative-y barycentric))
         (barycentric-width (+ (abs barycentric-dx) (abs barycentric-dy)))
         (edge-x (/ (swizzle barycentric :x)
                    (max (swizzle barycentric-width :x) 0.000001)))
         (edge-y (/ (swizzle barycentric :y)
                    (max (swizzle barycentric-width :y) 0.000001)))
         (edge-z (/ (swizzle barycentric :z)
                    (max (swizzle barycentric-width :z) 0.000001)))
         (edge-pixels (min (min edge-x edge-y) edge-z))
         (boundary-edge-pixels
           (min (min (if (= (ldb (byte 1 0) boundary-edge-mask) (uint 1.0))
                         edge-x 10000.0)
                     (if (= (ldb (byte 1 1) boundary-edge-mask) (uint 1.0))
                         edge-y 10000.0))
                (if (= (ldb (byte 1 2) boundary-edge-mask) (uint 1.0))
                    edge-z 10000.0)))
         (all-wire (- 1.0 (smoothstep 0.45 1.15 edge-pixels)))
         (boundary-wire
           (- 1.0 (smoothstep 0.45 1.15 boundary-edge-pixels)))
         (chamferness
           (clamp
            (* 2.4
               (- (+ (abs (swizzle normal :x))
                     (abs (swizzle normal :y))
                     (abs (swizzle normal :z)))
                  1.0))
            0.0 1.0))
         (cut-edge
           (* #.*cut-edge-lift-strength*
              chamferness
              (- 1.0 (smoothstep 0.20 1.45 boundary-edge-pixels))
              (+ 0.28 (* 0.72 wrapped))))
         (construction-wire
           (* (swizzle render-parameters :y)
              (min 1.0 (+ (* all-wire 0.18) (* boundary-wire 0.82)))))
         (fragment-uv (mesh-clip-uv current-clip))
         (pointer-delta
           (/ (- fragment-uv (swizzle inspection-parameters :xy))
              (max (swizzle inspection-parameters :zw)
                   (vec2 0.000001 0.000001))))
         (pointer-pixels (sqrt (dot pointer-delta pointer-delta)))
         (pointer-enabled (swizzle render-parameters :w))
         (ring (* pointer-enabled
                  (- 1.0
                     (smoothstep 0.35 1.25
                                 (abs (- pointer-pixels 11.0))))))
         (center (* pointer-enabled
                    (- 1.0 (smoothstep 0.7 1.8 pointer-pixels))))
         (reticle (max center ring))
         (construction-ink (vec3 0.055 0.16 0.22))
         (blueprint (vec3 0.30 0.90 0.94))
         (cut-paper (vec3 1.02 0.975 0.86))
         (edged (mix paper cut-paper cut-edge))
         (drafted (mix edged construction-ink construction-wire))
         (radiance (mix drafted blueprint reticle)))
    (set-output color-output (vec4 radiance 1.0))
    (set-output motion-output
                (- (mesh-clip-uv previous-clip)
                   (mesh-clip-uv current-clip)))))

(define-shader lattice-point-vertex-specification
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
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (render-parameters :vec4)
                            (previous-camera-position :vec4)
                            (previous-camera-right :vec4)
                            (previous-camera-up :vec4)
                            (previous-camera-forward :vec4)
                            (previous-camera-projection :vec4)
                            (temporal-parameters :vec4)
                            (inspection-parameters :vec4)
                            (character-parameters :vec4)))))
  (let* ((record (buffer-element lattice-points instance-index))
         (world-position
           (/ (vec3 (float (swizzle record :x))
                    (float (swizzle record :y))
                    (float (swizzle record :z)))
              8.0))
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
                           (swizzle render-parameters :z)))
         (previous-clip
           (mesh-view-clip world-position previous-camera-position
                           previous-camera-right previous-camera-up
                           previous-camera-forward previous-camera-projection
                           (swizzle temporal-parameters :z)))
         (pixel-size (swizzle inspection-parameters :zw))
         (radius (if (> marker-kind 1.5) 8.5
                     (if (> marker-kind 0.5) 6.5 2.6)))
         (jitter (swizzle temporal-parameters :xy)))
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

(define-shader lattice-point-fragment-specification
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

(define-shader present-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity)
                 (scene-sampler :sampler :binding 1)
                 (scene-depth :depth-texture-2d :binding 2)
                 (camera-state :uniform-block :binding 3
                  :members ((camera-position :vec4)
                            (camera-right :vec4)
                            (camera-up :vec4)
                            (camera-forward :vec4)
                            (camera-projection :vec4)
                            (render-parameters :vec4)
                            (previous-camera-position :vec4)
                            (previous-camera-right :vec4)
                            (previous-camera-up :vec4)
                            (previous-camera-forward :vec4)
                            (previous-camera-projection :vec4)
                            (temporal-parameters :vec4)
                            (inspection-parameters :vec4)))))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (value (sample scene scene-sampler uv))
         ;; Geometry depth carries the subpixel projection jitter consumed by
         ;; MetalFX; presentation UVs do not.  Sample depth at the same current
         ;; geometry location so the AO does not crawl across a resolved edge.
         (depth-uv (+ uv (* (swizzle temporal-parameters :xy) 0.5)))
         (depth (swizzle (sample scene-depth scene-sampler depth-uv) :x))
         (divisor (swizzle render-parameters :z))
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
                            shadow-pigment pooling))))
    (set-output color-output
                (vec4 pigmented 1.0))))
