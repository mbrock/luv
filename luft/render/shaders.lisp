(in-package #:luft.render.shaders)

;;; Site-stream rendering. One UVec4 instance selects a lattice base and a
;;; canonical fixed-stride template; the template vertex is a small exact
;;; offset plus geometric attributes. The CPU classifies sites and the vertex
;;; shader realizes the renderer-global triangle and quad populations.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *stock-tooth* 0.055)
  (defvar *paper-variation* 0.11)
  (defvar *local-ambient-occlusion-strength* 0.28)
  (defvar *screen-ambient-occlusion-strength* 0.38)
  (defvar *screen-ambient-occlusion-radius* 0.95)
  (defvar *ambient-pigment-strength* 0.82)
  (defvar *earth-set-stone-strength* 0.72)
  (defvar *highlight-glow-threshold* 0.82)
  (defvar *highlight-glow-strength* 0.24))

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

(define-shader-function paper-space (point)
  ;; Rotate the material field away from the voxel axes.  Because this is a
  ;; single world-space transform, adjacent facets still meet without seams.
  (let* ((x (swizzle point :x))
         (y (swizzle point :y))
         (z (swizzle point :z)))
    (vec3 (+ (* x 0.36) (* y 0.48) (* z -0.80))
          (+ (* x -0.80) (* y 0.60))
          (+ (* x 0.48) (* y 0.64) (* z 0.60)))))

(define-shader-function stock-tooth (point)
  (let* ((paper-point (paper-space point))
         (coarse
           (paper-noise
            (+ (vec3 (* (swizzle paper-point :x) 7.7)
                     (* (swizzle paper-point :y) 11.3)
                     (* (swizzle paper-point :z) 9.1))
               (vec3 13.7 3.1 21.9))))
         (fine
           (paper-noise
            (+ (vec3 (* (swizzle paper-point :z) 27.1)
                     (* (swizzle paper-point :x) 33.7)
                     (* (swizzle paper-point :y) 23.9))
               (vec3 4.3 29.1 11.7)))))
    (+ 1.0
       (* #.*stock-tooth* (- (+ (* 0.55 coarse) (* 0.45 fine)) 0.5)))))

(define-shader-function earth-set-stone-tone
    (point normal ambient-occlusion contact-variant
     stone-tone soil-tone subsoil-tone)
  "Weather one stone chamfer from its packed incident-substrate reading."
  (let* ((turf-set-p
           (if (< (abs contact-variant) 0.5) 1.0 0.0))
         (soil-set-p
           (if (< (abs (- contact-variant 1.0)) 0.5) 1.0 0.0))
         (deep-set-p
           (if (< (abs (- contact-variant 2.0)) 0.5) 1.0 0.0))
         (clump
           (paper-noise (+ (* point (vec3 1.25 1.25 4.5))
                           (vec3 7.1 19.3 3.7))))
         (grit (paper-noise (+ (* point 9.0) (vec3 31.7 5.9 13.1))))
         (break
           (paper-noise (+ (* point (vec3 3.1 3.1 7.7))
                           (vec3 2.9 41.3 17.1))))
         (coverage (+ (* turf-set-p 0.50)
                      (* soil-set-p 0.67)
                      (* deep-set-p 0.82)))
         (breakup
           (smoothstep
            0.43 0.61
            (+ (* 0.52 clump) (* 0.20 grit) (* 0.24 break)
               (* 0.24 ambient-occlusion)
               (* -0.13 (max 0.0 (swizzle normal :z)))
               #.*earth-set-stone-strength* -0.60)))
         (earth-weight
           (clamp (+ coverage (* 0.30 (- breakup 0.5))) 0.0 1.0))
         (earth-pigment
           (mix
            (mix soil-tone subsoil-tone
                 (+ (* 0.18 grit) (* 0.34 break) (* 0.22 soil-set-p)))
            subsoil-tone
            (* deep-set-p (+ 0.45 (* 0.25 grit)))))
         (weathered-stone
           (mix stone-tone soil-tone (+ 0.16 (* 0.10 grit)))))
    (* (mix weathered-stone earth-pigment earth-weight)
       (- 1.0 (* 0.20 (* earth-weight clump))))))

(define-shader-function turf-edge-tone
    (point normal grass-tone soil-tone)
  "Roll a grassy top into soil with a world-stable broken fringe."
  (let* ((fray (paper-noise (+ (* point 4.2) (vec3 11.7 3.1 29.3))))
         (turf-weight
           (smoothstep 0.42 0.78
                       (+ (max 0.0 (swizzle normal :z))
                          (* 0.24 (- fray 0.5))))))
    (mix soil-tone grass-tone turf-weight)))

(define-shader-function foundation-stone-tone
    (point stone-tone soil-tone subsoil-tone)
  "Let terrain climb an irregular fraction of its lowest exposed stone course."
  (let* ((weather
           (paper-noise (+ (* point (vec3 1.9 1.9 3.7))
                           (vec3 23.3 8.7 5.1))))
         (clump
           (paper-noise (+ (* point (vec3 0.63 0.63 1.15))
                           (vec3 3.7 27.1 14.3))))
         (grit (paper-noise (+ (* point 8.0) (vec3 5.7 17.9 37.1))))
         (height (fract (swizzle point :z)))
         (earth
           (- 1.0
              (smoothstep 0.07 0.61
                          (+ height (* 0.32 (- weather 0.5))
                             (* 0.20 (- clump 0.5))))))
         (weathered-stone
           (mix stone-tone soil-tone (+ 0.16 (* 0.10 grit))))
         (earth-pigment
           (mix soil-tone subsoil-tone (+ 0.25 (* 0.45 grit)))))
    (* (mix weathered-stone earth-pigment earth)
       (- 1.0 (* 0.12 earth)))))

(define-shader-function material-frame-point
    (point origin x-axis y-axis z-axis)
  "Express POINT in the authored material placement's coordinate frame."
  (let* ((relative (- point origin)))
    (vec3 (dot relative x-axis)
          (dot relative y-axis)
          (dot relative z-axis))))

(define-shader-function material-relief (point profile)
  "One continuous height field used for pigment, normal, and roughness."
  (let* ((granular
           (+ (* 0.62
                 (paper-noise (+ (* point 2.7) (vec3 11.3 5.7 23.9))))
              (* 0.38
                 (paper-noise (+ (* point 9.1) (vec3 3.1 29.7 7.3))))))
         (stone-grain
           (paper-noise (+ (* point 3.8) (vec3 31.1 7.9 13.7))))
         (stone-pit
           (paper-noise
            (+ (* (swizzle point :zxy) 10.7) (vec3 5.3 19.1 37.7))))
         (weathered-stone
           (+ (* 0.72 stone-grain) (* 0.28 stone-pit))))
    (- (if (< profile 1.5) granular weathered-stone) 0.5)))

(define-shader-function dressed-stone-tone (point normal stone-tone)
  "Suggest laid courses and hand-dressed blocks without texture coordinates."
  (let* ((x (swizzle point :x))
         (y (swizzle point :y))
         (z (swizzle point :z))
         (course-coordinate (* z 2.0))
         (course (floor course-coordinate))
         (jitter
           (- (paper-noise (+ (* point 1.7) (vec3 17.1 4.3 9.7))) 0.5))
         (tangent
           (if (> (abs (swizzle normal :x))
                  (abs (swizzle normal :y)))
               y x))
         (stagger (* 0.5 (fract (* course 0.5))))
         (course-phase
           (fract (+ course-coordinate (* jitter 0.085))))
         (joint-phase
           (fract (+ (/ tangent 1.35) stagger (* jitter 0.11))))
         (course-distance (min course-phase (- 1.0 course-phase)))
         (joint-distance (min joint-phase (- 1.0 joint-phase)))
         (vertical-face
           (- 1.0 (smoothstep 0.55 0.85 (abs (swizzle normal :z)))))
         (horizontal-mortar
           (- 1.0 (smoothstep 0.018 0.052 course-distance)))
         (vertical-mortar
           (* vertical-face
              (- 1.0 (smoothstep 0.016 0.046 joint-distance))))
         (broken-joint
           (smoothstep 0.46 0.78
                       (paper-noise
                        (+ (vec3 (floor (+ x stagger))
                                 (floor (+ y stagger)) course)
                           (vec3 5.3 21.7 11.9)))))
         (mortar
           (clamp (+ horizontal-mortar (* vertical-mortar broken-joint))
                  0.0 1.0))
         (block-value
           (paper-noise
            (+ (* (floor (+ point (vec3 stagger stagger 0.0))) 0.73)
               (vec3 29.1 7.7 3.1))))
         (dressed
           (* stone-tone (+ 0.90 (* 0.20 block-value)
                            (* 0.05 jitter))))
         (mortar-tone (* stone-tone 0.78)))
    (mix dressed mortar-tone (* mortar 0.58))))

(define-shader-function natural-earth-tone
    (point normal earth-tone depth)
  "Break exposed earth into continuous strata, clumps, and damp recesses."
  (let* ((broad
           (paper-noise (+ (* point (vec3 0.38 0.38 0.72))
                           (vec3 7.3 19.1 2.7))))
         (clump
           (paper-noise (+ (* point (vec3 2.3 2.3 3.7))
                           (vec3 31.7 5.1 13.9))))
         (stratum
           (paper-noise
            (vec3 (* (swizzle point :x) 0.18)
                  (* (swizzle point :y) 0.18)
                  (+ (* (swizzle point :z) 1.65) (* broad 0.75)))))
         (side-weight
           (- 1.0 (smoothstep 0.48 0.86 (abs (swizzle normal :z)))))
         (value
           (+ 0.84 (* broad 0.18) (* clump 0.10)
              (* side-weight (- stratum 0.5) 0.22)
              (* depth -0.06))))
    (* earth-tone value)))

(define-shader-function paper-tonemap (radiance)
  (let* ((numerator
           (* radiance (+ (* radiance 2.51) (vec3 0.03 0.03 0.03))))
         (denominator
           (+ (* radiance (+ (* radiance 2.43) (vec3 0.59 0.59 0.59)))
              (vec3 0.14 0.14 0.14))))
    (clamp (/ numerator denominator)
           (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))))

(define-shader-function paper-grade (color)
  "Keep cool shade and warm paper luminous after highlight compression."
  (let* ((luminance (dot color (vec3 0.2126 0.7152 0.0722)))
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
    (clamp contrasted black white)))

(define-shader-function highlight-energy (value)
  "Keep only genuinely luminous scene-linear colour for the paper glow."
  (let* ((color (swizzle value :xyz))
         (luminance (dot color (vec3 0.2126 0.7152 0.0722)))
         (gate (smoothstep #.*highlight-glow-threshold* 1.55 luminance)))
    (* color gate)))

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

(define-shader-function mesh-temporal-motion
    (previous-clip current-clip temporal-parameters)
  "Return previous-minus-current motion at the actual jittered raster sites."
  (- (+ (mesh-clip-uv previous-clip)
        (* (swizzle temporal-parameters :zw) 0.5))
     (+ (mesh-clip-uv current-clip)
        (* (swizzle temporal-parameters :xy) 0.5))))

(define-shader-function mesh-world-position (instance template-vertex)
  "Decode the one world position shared by the scene and shadow passes."
  (/ (+ (* (vec3 (float (swizzle instance :x))
                 (float (swizzle instance :y))
                 (float (swizzle instance :z)))
           8.0)
        (- (vec3 (float (swizzle template-vertex :x))
                 (float (swizzle template-vertex :y))
                 (float (swizzle template-vertex :z)))
           (vec3 16.0 16.0 16.0)))
     8.0))

(define-shader-function light-clip-position
    (world-position row-x row-y row-z row-w)
  (let* ((point (vec4 world-position 1.0)))
    (vec4 (dot point row-x) (dot point row-y)
          (dot point row-z) (dot point row-w))))

(define-shader-function soft-shadow-visibility
    (shadow-map shadow-sampler shadow-sample normal sun shadow-control)
  "Five comparison-filtered taps forming one restrained paper-soft shadow."
  (let* ((uv (swizzle shadow-sample :xy))
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

(define-shader-function player-step-coordinate (gait)
  (/ gait 3.14159265))

;;; Motion vocabulary.  Authored channels name the five readable walk poses,
;;; while contact placement and two-bone IK remain hard constraints beneath
;;; them.  The same segment primitive can drive idle, gesture, attack or
;;; interaction channels without coupling their timing to the locomotion math.

(define-shader-function player-motion-ease (amount)
  "Quintic ease with zero velocity and acceleration at both ends."
  (let* ((time (clamp amount 0.0 1.0)))
    (* (* time (* time time))
       (+ 10.0 (* time (+ -15.0 (* 6.0 time)))))))

(define-shader-function player-motion-segment
    (phase beginning end beginning-value end-value)
  (let* ((amount (/ (- phase beginning) (max (- end beginning) 1e-5))))
    (mix beginning-value end-value (player-motion-ease amount))))

(define-shader-function player-walk-pose-channel
    (phase contact down passing up next-contact)
  "Sample a semantic contact/down/passing/up/contact animation channel."
  (if (< phase 0.16)
      (player-motion-segment phase 0.0 0.16 contact down)
      (if (< phase 0.50)
          (player-motion-segment phase 0.16 0.50 down passing)
          (if (< phase 0.72)
              (player-motion-segment phase 0.50 0.72 passing up)
              (player-motion-segment phase 0.72 1.0 up next-contact)))))

(define-shader-function player-foot-cycle-phase (gait parity)
  (fract (* 0.5 (- (player-step-coordinate gait) parity))))

(define-shader-function player-foot-rocker-pitch (cycle-phase)
  "Heel rocker, flat ankle rocker, forefoot rocker, then swing dorsiflexion."
  (let* ((stance-time (* cycle-phase 2.0))
         (swing-time (* (- cycle-phase 0.5) 2.0)))
    (if (< cycle-phase 0.5)
        (if (< stance-time 0.18)
            (player-motion-segment stance-time 0.0 0.18 0.17 0.0)
            (if (< stance-time 0.72)
                0.0
                (player-motion-segment stance-time 0.72 1.0 0.0 -0.30)))
        (if (< swing-time 0.32)
            (player-motion-segment swing-time 0.0 0.32 -0.30 0.10)
            (if (< swing-time 0.78)
                0.10
                (player-motion-segment swing-time 0.78 1.0 0.10 0.17))))))

(define-shader-function player-pelvis-lift (gait)
  "Height of a fixed-length leg over the active stance contact.

Each half-step advances the root by 0.75 cells.  The support foot sits at
the half-step midpoint, so its fore-aft lever runs symmetrically from +D/2 to
-D/2 and the pelvis rises naturally over mid-stance."
  (let* ((step-length 0.75)
         (half-step (* step-length 0.5))
         ;; The ankle centre is Z=.26 and the hip is Z=1.27 at double
         ;; support.  This reach therefore makes the stance constraint agree
         ;; with the actual SDF joint centres, rather than merely producing a
         ;; plausible amount of bob.
         (leg-length 1.07737)
         (phase (fract (player-step-coordinate gait)))
         (support-offset (* step-length (- 0.5 phase)))
         (height (sqrt (max (- (* leg-length leg-length)
                               (* support-offset support-offset))
                            0.0)))
         (contact-height
           (sqrt (- (* leg-length leg-length) (* half-step half-step)))))
    (+ (- height contact-height)
       ;; Loading sinks after contact; maximum authored rise follows the
       ;; passing pose.  This rides on the inverted-pendulum constraint rather
       ;; than replacing it.
       (player-walk-pose-channel phase 0.0 -0.035 0.0 0.018 0.0))))

(define-shader-function player-walk-pose (point gait)
  "Move a sample with the pelvis lift and a quiet support-side weight shift."
  (let* ((phase (fract (player-step-coordinate gait)))
         (side-shift
           (player-walk-pose-channel phase 0.0 -0.020 0.030 0.018 0.0))
         (forward-set
           (player-walk-pose-channel phase 0.0 -0.018 0.012 0.006 0.0)))
    (vec3 (- (swizzle point :x) side-shift)
          (- (swizzle point :y) forward-set)
          (- (swizzle point :z) (player-pelvis-lift gait)))))

(define-shader-function player-head-pose (point gait)
  "Independent slow sway, glance and tilt keep the head out of marching time."
  (let* ((slow-phase (* gait 0.5))
         (wander-phase (* gait 0.25))
         (roll (* 0.055 (sin slow-phase)))
         (cosine (cos roll))
         (sine (sin roll))
         (translated
           (- point (vec3 (* 0.035 (sin slow-phase))
                          (* 0.022 (sin wander-phase))
                          (* 0.016 (sin (* gait 1.5))))))
         (centered (- translated (vec3 0.0 0.0 2.57))))
    (+ (vec3 0.0 0.0 2.57)
       (vec3 (+ (* cosine (swizzle centered :x))
                (* sine (swizzle centered :z)))
             (swizzle centered :y)
             (- (* cosine (swizzle centered :z))
                (* sine (swizzle centered :x)))))))

(define-shader-function player-tunic-distance (point)
  ;; A fitted upper coat flowing into a longer surcoat gives the little figure
  ;; a deliberate, upright silhouette without hiding the planted legs.
  (player-sdf-smooth-union
   (player-sdf-ellipsoid point (vec3 0.0 0.0 1.88)
                         (vec3 0.47 0.30 0.46))
   (player-sdf-ellipsoid point (vec3 0.0 -0.015 1.52)
                         (vec3 0.43 0.28 0.55))
   0.095))

(define-shader-function player-head-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 -0.035 2.57)
                        (vec3 0.35 0.32 0.36)))

(define-shader-function player-face-distance (point)
  (player-sdf-sphere point (vec3 0.0 0.305 2.55) 0.12))

(define-shader-function player-hair-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 -0.10 2.70)
                        (vec3 0.365 0.285 0.245)))

(define-shader-function player-collar-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 -0.015 2.18)
                        (vec3 0.48 0.325 0.16)))

(define-shader-function player-cape-distance (point)
  "A close, draped traveling cape rather than a bulbous adventurer pack."
  (player-sdf-ellipsoid point (vec3 0.0 -0.265 1.65)
                        (vec3 0.38 0.135 0.61)))

(define-shader-function player-belt-distance (point)
  (player-sdf-ellipsoid point (vec3 0.0 0.0 1.62)
                        (vec3 0.445 0.295 0.095)))

(define-shader-function player-one-leg-distance
    (point gait direction side parity)
  "A continuous two-bone leg field with an analytic knee solution."
  (let* ((step-length 0.75)
         (step-coordinate (player-step-coordinate gait))
         ;; One foot cycle spans two half-steps.  Its first half is stance;
         ;; its second half is the swing to the next fixed contact.
         (cycle (* 0.5 (- step-coordinate parity)))
         (cycle-index (floor cycle))
         (phase (player-foot-cycle-phase gait parity))
         (swing-time (clamp (* 2.0 (- phase 0.5)) 0.0 1.0))
         (swing-weight (player-motion-ease swing-time))
         (contact-step (+ parity (* 2.0 cycle-index)))
         (foot-world
           (* step-length (+ (+ contact-step 0.5)
                             (* 2.0 swing-weight))))
         (root-world (* step-length step-coordinate))
         (swing (* direction (- foot-world root-world)))
         (lift (* 0.19 (sin (* 3.14159265 swing-time))))
         (pelvis-lift (player-pelvis-lift gait))
         (hip (vec3 (* side 0.22) 0.0
                    (+ 1.27 pelvis-lift)))
         (foot-pitch (player-foot-rocker-pitch phase))
         (pitch-cosine (cos foot-pitch))
         (pitch-sine (sin foot-pitch))
         (baseline-center
           (vec3 (* side 0.22) (+ swing 0.09) (+ 0.16 lift)))
         ;; During stance the boot pivots around a stationary heel, lies flat
         ;; through weight acceptance, then pivots around a stationary toe.
         ;; During swing its centre follows the authored clearance arc.
         (stance-time (clamp (* phase 2.0) 0.0 1.0))
         (pivot
           (if (< phase 0.5)
               (if (< stance-time 0.55)
                   (vec3 (* side 0.22) (- (+ swing 0.09) 0.22) 0.0)
                   (vec3 (* side 0.22) (+ (+ swing 0.09) 0.23) 0.0))
               baseline-center))
         (center-offset (- baseline-center pivot))
         (boot-center
           (+ pivot
              (vec3 (swizzle center-offset :x)
                    (- (* pitch-cosine (swizzle center-offset :y))
                       (* pitch-sine (swizzle center-offset :z)))
                    (+ (* pitch-sine (swizzle center-offset :y))
                       (* pitch-cosine (swizzle center-offset :z))))))
         (ankle-offset (vec3 0.0 -0.09 0.10))
         (ankle
           (+ boot-center
              (vec3 0.0
                    (- (* pitch-cosine (swizzle ankle-offset :y))
                       (* pitch-sine (swizzle ankle-offset :z)))
                    (+ (* pitch-sine (swizzle ankle-offset :y))
                       (* pitch-cosine (swizzle ankle-offset :z))))))
         ;; Equal 0.59-cell bones solve the knee on the forward-bending side
         ;; of the hip/ankle chord.  Keeping a little flex even at maximum
         ;; stance reach gives the silhouette a readable knee instead of a
         ;; rubbery telescoping capsule.
         (axis (- ankle hip))
         (axis-length (player-sdf-length axis))
         (half-axis (* axis 0.5))
         (bone-length 0.59)
         (knee-reach
           (sqrt (max (- (* bone-length bone-length)
                         (* 0.25 (* axis-length axis-length)))
                      0.0)))
         (knee-forward
           (/ (vec3 0.0 (- (swizzle axis :z)) (swizzle axis :y))
              axis-length))
         (knee (+ hip (+ half-axis (* knee-forward knee-reach))))
         (thigh (player-sdf-capsule point hip knee 0.175))
         (shin (player-sdf-capsule point knee ankle 0.15))
         (leg (player-sdf-smooth-union thigh shin 0.065))
         (boot-offset (- point boot-center))
         (boot-point
           (+ boot-center
              (vec3 (swizzle boot-offset :x)
                    (+ (* pitch-cosine (swizzle boot-offset :y))
                       (* pitch-sine (swizzle boot-offset :z)))
                    (- (* pitch-cosine (swizzle boot-offset :z))
                       (* pitch-sine (swizzle boot-offset :y))))))
         (boot (player-sdf-ellipsoid
                boot-point boot-center (vec3 0.22 0.31 0.16))))
    (player-sdf-smooth-union leg boot 0.065)))

(define-shader-function player-leg-distance (point gait direction)
  ;; Evaluate both fields everywhere.  Selecting a leg from POINT.x used to
  ;; put a discontinuity through the centre plane, visibly slicing the
  ;; screen-left leg open whenever its diagonal silhouette crossed that plane.
  (min (player-one-leg-distance point gait direction -1.0 0.0)
       (player-one-leg-distance point gait direction 1.0 1.0)))

(define-shader-function player-one-arm-distance (point gait side)
  (let* ((reach (if (< side 0.0) 0.18 0.22))
         (swing (* side (* (* -1.0 reach) (sin gait))))
         (shoulder (vec3 (* side 0.44) 0.0 2.00))
         (elbow (vec3 (* side 0.50) (* swing 0.38) 1.63))
         (hand (vec3 (* side 0.50) swing 1.34))
         (upper (player-sdf-capsule point shoulder elbow 0.145))
         (lower (player-sdf-capsule point elbow hand 0.135)))
    (player-sdf-smooth-union upper lower 0.055)))

(define-shader-function player-arm-distance (point gait)
  (min (player-one-arm-distance point gait -1.0)
       (player-one-arm-distance point gait 1.0)))

(define-shader-function player-one-hand-distance (point gait side)
  (let* ((reach (if (< side 0.0) 0.18 0.22))
         (swing (* side (* (* -1.0 reach) (sin gait)))))
    (player-sdf-sphere point (vec3 (* side 0.50) swing 1.31) 0.155)))

(define-shader-function player-hand-distance (point gait)
  (min (player-one-hand-distance point gait -1.0)
       (player-one-hand-distance point gait 1.0)))

(define-shader-function player-distance (point gait direction)
  (let* ((posed (player-walk-pose point gait))
         (head-posed (player-head-pose posed gait))
         (body (player-sdf-smooth-union
                (player-tunic-distance posed)
                (player-head-distance head-posed) 0.095))
         (face (player-face-distance head-posed))
         (hair (player-hair-distance head-posed))
         (collar (player-collar-distance posed))
         (cape (player-cape-distance posed))
         (belt (player-belt-distance posed))
         ;; Legs use the un-bobbed frame: the planted boot stays on the deck
         ;; while the hips and upper body carry the weight shift.
         (legs (player-leg-distance point gait direction))
         (arms (player-arm-distance posed gait))
         (hands (player-hand-distance posed gait)))
    (player-sdf-smooth-union
     (player-sdf-smooth-union
      (player-sdf-smooth-union
       (player-sdf-smooth-union body face 0.055)
       (player-sdf-smooth-union hair collar 0.06) 0.055)
      (player-sdf-smooth-union
       (player-sdf-smooth-union cape belt 0.045) legs 0.06) 0.06)
     (player-sdf-smooth-union arms hands 0.07) 0.075)))

(define-shader-function player-normal (point gait direction)
  (let* ((reach 0.004)
         (a (vec3 1.0 -1.0 -1.0))
         (b (vec3 -1.0 -1.0 1.0))
         (c (vec3 -1.0 1.0 -1.0))
         (d (vec3 1.0 1.0 1.0)))
    (normalize
     (+ (+ (* a (player-distance (+ point (* a reach)) gait direction))
           (* b (player-distance (+ point (* b reach)) gait direction)))
        (+ (* c (player-distance (+ point (* c reach)) gait direction))
           (* d (player-distance (+ point (* d reach)) gait direction)))))))

(define-shader-function player-linen-tooth (point)
  "Low-contrast local-space warp, weft and irregular fibre for woven cloth."
  (let* ((warp (sin (* (swizzle point :x) 47.0)))
         (weft (sin (* (swizzle point :z) 53.0)))
         (fibre (- (paper-noise (* point 18.0)) 0.5)))
    (+ 1.0 (+ (* 0.018 (* warp weft)) (* 0.065 fibre)))))

(define-shader-function player-leather-tooth (point)
  (+ 1.0 (* 0.075 (- (paper-noise (* point 23.0)) 0.5))))

(define-shader-function player-albedo (point gait direction)
  (let* ((posed (player-walk-pose point gait))
         (head-posed (player-head-pose posed gait))
         (tunic (min (player-tunic-distance posed)
                     (player-arm-distance posed gait)))
         (skin (min (min (player-head-distance head-posed)
                         (player-face-distance head-posed))
                    (player-hand-distance posed gait)))
         (hair (player-hair-distance head-posed))
         (collar (player-collar-distance posed))
         (cape (player-cape-distance posed))
         (belt (player-belt-distance posed))
         (legs (player-leg-distance point gait direction))
         (linen (player-linen-tooth posed))
         (leather (player-leather-tooth posed))
         (skin-near (step skin tunic))
         (first (mix (* (vec3 0.31 0.385 0.35) linen)
                     (vec3 0.62 0.44 0.32) skin-near))
         (distance (min tunic skin))
         (hair-near (step hair distance))
         (second (mix first (vec3 0.15 0.105 0.075) hair-near))
         (distance-2 (min distance hair))
         (collar-near (step collar distance-2))
         (third (mix second (* (vec3 0.58 0.455 0.255) linen) collar-near))
         (distance-3 (min distance-2 collar))
         (cape-near (step cape distance-3))
         (fourth (mix third (* (vec3 0.37 0.30 0.205) linen) cape-near))
         (distance-4 (min distance-3 cape))
         (belt-near (step belt distance-4))
         (fifth (mix fourth (* (vec3 0.22 0.155 0.095) leather) belt-near))
         (distance-5 (min distance-4 belt))
         (leg-near (step legs distance-5)))
    (mix fifth (* (vec3 0.18 0.15 0.125) leather) leg-near)))

(define-shader-function player-world-direction (local heading)
  "Rotate a player-local direction into the horizontal world frame."
  (let* ((right (vec2 (swizzle heading :y) (- (swizzle heading :x)))))
    (vec3 (+ (* (swizzle right :x) (swizzle local :x))
             (* (swizzle heading :x) (swizzle local :y)))
          (+ (* (swizzle right :y) (swizzle local :x))
             (* (swizzle heading :y) (swizzle local :y)))
          (swizzle local :z))))

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
                            (character-parameters :vec4)
                            (sun-vector :vec4)
                            (sun-color-vector :vec4)
                            (sky-color-vector :vec4)
                            (ground-color-vector :vec4)
                            (shadow-row-x :vec4)
                            (shadow-row-y :vec4)
                            (shadow-row-z :vec4)
                            (shadow-row-w :vec4)
                            (shadow-control :vec4)
                            (previous-character-parameters :vec4)
                            (character-direction :vec4)))
                 (shadow-map :depth-texture-2d :binding 1)
                 (shadow-sampler :sampler :binding 2)))
  (let* ((index (float vertex-index))
         (right-corner (if (= index 2.0) 1.0
                           (if (= index 3.0) 1.0
                               (if (= index 5.0) 1.0 0.0))))
         (bottom-corner (if (= index 1.0) 1.0
                            (if (= index 4.0) 1.0
                                (if (= index 5.0) 1.0 0.0))))
         (corner (vec2 (- (* right-corner 2.0) 1.0)
                       (- (* bottom-corner 2.0) 1.0)))
         (center (vec3 (swizzle character-parameters :x)
                       (swizzle character-parameters :y)
                       (swizzle character-parameters :z)))
         (previous-center
           (vec3 (swizzle previous-character-parameters :x)
                 (swizzle previous-character-parameters :y)
                 (swizzle previous-character-parameters :z)))
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
                           (swizzle render-parameters :z)))
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
                            (character-parameters :vec4)
                            (sun-vector :vec4)
                            (sun-color-vector :vec4)
                            (sky-color-vector :vec4)
                            (ground-color-vector :vec4)
                            (shadow-row-x :vec4)
                            (shadow-row-y :vec4)
                            (shadow-row-z :vec4)
                            (shadow-row-w :vec4)
                            (shadow-control :vec4)
                            (previous-character-parameters :vec4)
                            (character-direction :vec4)))
                 (shadow-map :depth-texture-2d :binding 1)
                 (shadow-sampler :sampler :binding 2)))
  (let* ((center (swizzle center-radius :xyz))
         (radius (swizzle center-radius :w))
         (gait (swizzle character-parameters :w))
         (heading (normalize (swizzle character-direction :xy)))
         (player-right (vec2 (swizzle heading :y)
                             (- (swizzle heading :x))))
         (ray (if (< (swizzle render-parameters :z) 0.5)
                  (normalize (swizzle camera-forward :xyz))
                  (normalize (- proxy-world-position
                                (swizzle camera-position :xyz)))))
         (origin proxy-world-position)
         (travel
           (counted-fold (march 58.0 ray-distance 0.0)
             (let* ((world-point (+ origin (* ray ray-distance)))
                    (relative (- world-point center))
                    (relative-xy (swizzle relative :xy))
                    (local (vec3 (dot relative-xy player-right)
                                 (dot relative-xy heading)
                                 (+ (swizzle relative :z) 1.48)))
                    (distance (player-distance local gait 1.0)))
               (if (< distance 0.0025)
                   ray-distance
                   (if (> ray-distance (* radius 2.0))
                       ray-distance
                       (+ ray-distance (max (* distance 0.82) 0.0025)))))))
         (world-point (+ origin (* ray travel)))
         (relative (- world-point center))
         (relative-xy (swizzle relative :xy))
         (local (vec3 (dot relative-xy player-right)
                      (dot relative-xy heading)
                      (+ (swizzle relative :z) 1.48)))
         (surface-distance (player-distance local gait 1.0))
         (coverage (- 1.0 (step 0.006 surface-distance)))
         (local-normal (player-normal local gait 1.0))
         (normal (normalize (player-world-direction local-normal heading)))
         (albedo (player-albedo local gait 1.0))
         (sun (swizzle sun-vector :xyz))
         (sun-color (swizzle sun-color-vector :xyz))
         (sky (swizzle sky-color-vector :xyz))
         (ground (swizzle ground-color-vector :xyz))
         (facing (dot normal sun))
         (direct-shape (smoothstep 0.0 0.72 (max 0.0 facing)))
         (light-clip
           (light-clip-position world-point shadow-row-x shadow-row-y
                                shadow-row-z shadow-row-w))
         (player-shadow-sample
           (vec3 (+ (* (swizzle light-clip :x) 0.5) 0.5)
                 (+ (* (swizzle light-clip :y) 0.5) 0.5)
                 (swizzle light-clip :z)))
         (sampled-shadow
           (soft-shadow-visibility shadow-map shadow-sampler
                                   player-shadow-sample normal sun
                                   shadow-control))
         (direct-visibility
           (mix 1.0 sampled-shadow
                (smoothstep 0.03 0.18 (max 0.0 facing))))
         (upness (swizzle normal :z))
         (sky-weight (+ 0.5 (* 0.5 upness)))
         (ground-weight (- 0.5 (* 0.5 upness)))
         (indirect (+ (* sky (+ 0.15 (* 0.54 sky-weight)))
                      (* ground (+ 0.05 (* 0.34 ground-weight)))))
         (warm-return
           (* sun-color
              (* 0.085 direct-shape (- 1.0 direct-visibility))))
         (lit (* albedo
                 (+ (* sun-color (* direct-shape direct-visibility))
                    indirect warm-return)))
         (paper (* lit 1.08))
         (rim (expt (- 1.0 (max 0.0 (dot normal (* ray -1.0)))) 3.0))
         (radiance (+ paper (* (vec3 0.20 0.42 0.48) (* rim 0.07)))))
    (set-output color-output (vec4 (* radiance coverage) coverage))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip
                                      temporal-parameters))))

(define-shader mesh-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-position-output :vec3 :location 0)
               (mesh-normal-output :vec3 :location 1 :interpolation :flat)
               (assembly-output :float :location 2 :interpolation :flat)
               (barycentric-output :vec3 :location 3)
               (current-clip-output :vec4 :location 4)
               (previous-clip-output :vec4 :location 5)
               (shadow-sample-output :vec3 :location 6)
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
                            (character-parameters :vec4)
                            (sun-vector :vec4)
                            (sun-color-vector :vec4)
                            (sky-color-vector :vec4)
                            (ground-color-vector :vec4)
                            (shadow-row-x :vec4)
                            (shadow-row-y :vec4)
                            (shadow-row-z :vec4)
                            (shadow-row-w :vec4)
                            (shadow-control :vec4)))))
  (let* ((instance (buffer-element instances instance-index))
         (template-id
           (uint (ldb (byte 16 0) (swizzle instance :w))))
         (template-index
           (+ (* template-id (uint 6.0)) vertex-index))
         (template-vertex (buffer-element template-vertices template-index))
         (attributes (swizzle template-vertex :w))
         (world-position (mesh-world-position instance template-vertex))
         (mesh-normal
           (vec3 (- (float (ldb (byte 2 0) attributes)) 1.0)
                 (- (float (ldb (byte 2 2) attributes)) 1.0)
                 (- (float (ldb (byte 2 4) attributes)) 1.0)))
         (front-facing-p
           (>= (dot mesh-normal
                    (- (swizzle camera-position :xyz) world-position))
               0.0))
         (assembly-id (float (ldb (byte 12 16) (swizzle instance :w))))
         (ambient-occlusion
           (/ (float (ldb (byte 2 28) (swizzle instance :w))) 3.0))
         (barycentric-index (uint (ldb (byte 2 6) attributes)))
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
                           (swizzle render-parameters :z)))
         (light-clip
           (light-clip-position world-position shadow-row-x shadow-row-y
                                shadow-row-z shadow-row-w))
         (jitter (swizzle temporal-parameters :xy)))
    (set-output clip-position
                (if front-facing-p
                    (vec4 (+ (swizzle current-clip :x)
                             (* (swizzle jitter :x) (swizzle current-clip :w)))
                          (+ (swizzle current-clip :y)
                             (* (swizzle jitter :y) (swizzle current-clip :w)))
                          (swizzle current-clip :z)
                          (swizzle current-clip :w))
                    (vec4 2.0 2.0 2.0 1.0)))
    (set-output world-position-output world-position)
    (set-output mesh-normal-output mesh-normal)
    (set-output assembly-output assembly-id)
    (set-output barycentric-output barycentric)
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)
    (set-output shadow-sample-output
                (vec3 (+ (* (swizzle light-clip :x) 0.5) 0.5)
                      (+ (* (swizzle light-clip :y) 0.5) 0.5)
                      (swizzle light-clip :z)))
    (set-output boundary-edge-mask-output boundary-edge-mask)
    (set-output ambient-occlusion-output ambient-occlusion)))

(define-shader mesh-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0)
              (mesh-normal :vec3 :location 1 :interpolation :flat)
              (assembly-id :float :location 2 :interpolation :flat)
              (barycentric :vec3 :location 3)
              (current-clip :vec4 :location 4)
              (previous-clip :vec4 :location 5)
              (shadow-sample :vec3 :location 6)
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
                            (character-parameters :vec4)
                            (sun-vector :vec4)
                            (sun-color-vector :vec4)
                            (sky-color-vector :vec4)
                            (ground-color-vector :vec4)
                            (shadow-row-x :vec4)
                            (shadow-row-y :vec4)
                            (shadow-row-z :vec4)
                            (shadow-row-w :vec4)
                            (shadow-control :vec4)))
                 (material-descriptors :storage-buffer :binding 3
                  :element :vec4)
                 (shadow-map :depth-texture-2d :binding 4)
                 (shadow-sampler :sampler :binding 5)))
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
         (descriptor-row (uint (* assembly-id 7.0)))
         (primary
           (buffer-element material-descriptors descriptor-row))
         (secondary
           (buffer-element material-descriptors
                           (+ descriptor-row (uint 1.0))))
         (tertiary
           (buffer-element material-descriptors
                           (+ descriptor-row (uint 2.0))))
         (frame-origin
           (buffer-element material-descriptors
                           (+ descriptor-row (uint 3.0))))
         (frame-x
           (buffer-element material-descriptors
                           (+ descriptor-row (uint 4.0))))
         (frame-y
           (buffer-element material-descriptors
                           (+ descriptor-row (uint 5.0))))
         (frame-z
           (buffer-element material-descriptors
                           (+ descriptor-row (uint 6.0))))
         (material-point
           (material-frame-point
            world-position (swizzle frame-origin :xyz)
            (swizzle frame-x :xyz) (swizzle frame-y :xyz)
            (swizzle frame-z :xyz)))
         (relief-profile (swizzle frame-origin :w))
         (relief-height (material-relief material-point relief-profile))
         (shading-normal normal)
         (roughness
           (clamp (+ (swizzle tertiary :w)
                     (* 0.06 (abs relief-height)))
                  0.16 0.99))
         (paper-point (paper-space material-point))
         (primary-tone (swizzle primary :xyz))
         (secondary-tone (swizzle secondary :xyz))
         (tertiary-tone (swizzle tertiary :xyz))
         ;; `kernel` is a Metal address-space keyword, so keep the closed
         ;; material-kernel discriminator explicit in generated MSL.
         (kernel-code (swizzle primary :w))
         (contact-variant (swizzle secondary :w))
         (earth-set-p
           (if (< (abs (- kernel-code 1.0)) 0.5) 1.0 0.0))
         (tone
           (if (< kernel-code 0.5)
               primary-tone
               (if (< kernel-code 1.5)
                   (earth-set-stone-tone
                    material-point normal ambient-occlusion contact-variant
                    primary-tone secondary-tone tertiary-tone)
                   (if (< kernel-code 2.5)
                       (turf-edge-tone material-point normal
                                       primary-tone secondary-tone)
                       (if (< kernel-code 3.5)
                           (foundation-stone-tone
                            material-point primary-tone secondary-tone
                            tertiary-tone)
                           (if (< kernel-code 4.5)
                               (dressed-stone-tone
                                material-point normal primary-tone)
                               (if (< kernel-code 5.5)
                                   (natural-earth-tone
                                    material-point normal primary-tone 0.0)
                                   (if (< kernel-code 6.5)
                                       (natural-earth-tone
                                        material-point normal primary-tone 1.0)
                                       (natural-earth-tone
                                        material-point normal primary-tone
                                        -0.5)))))))))
         (bloom
           (paper-noise (+ (* paper-point 0.17) (vec3 2.7 17.1 8.3))))
         (mottle
           (paper-noise
            (+ (* (swizzle paper-point :yzx) 0.61)
               (vec3 19.7 7.3 3.1))))
         (fiber
           (paper-noise
            (+ (vec3 (* (swizzle paper-point :x) 2.3)
                     (* (swizzle paper-point :y) 5.1)
                     (* (swizzle paper-point :z) 3.7))
               (vec3 5.9 23.3 14.1))))
         (value (+ 1.0 (* #.*paper-variation*
                           (+ (* 0.90 (- bloom 0.5))
                              (* 0.55 (- mottle 0.5))
                              (* 0.25 (- fiber 0.5))))))
         (warmth (mix (vec3 0.965 0.99 1.04) (vec3 1.04 1.01 0.96)
                      (smoothstep 0.18 0.82 mottle)))
         (base (* tone (* warmth (* value (+ 1.0 (* relief-height 0.028))))))
         (sun (swizzle sun-vector :xyz))
         (sun-color (swizzle sun-color-vector :xyz))
         (sky (swizzle sky-color-vector :xyz))
         (ground (swizzle ground-color-vector :xyz))
         (facing (dot shading-normal sun))
         (direct-shape (smoothstep 0.0 0.72 (max 0.0 facing)))
         (sampled-shadow
           (soft-shadow-visibility shadow-map shadow-sampler shadow-sample
                                   shading-normal sun shadow-control))
         (direct-visibility
           (mix 1.0 sampled-shadow
                (smoothstep 0.03 0.18 (max 0.0 facing))))
         (upness (swizzle shading-normal :z))
         (sky-weight (+ 0.5 (* 0.5 upness)))
         (ground-weight (- 0.5 (* 0.5 upness)))
         (indirect-light
           ;; Sky and earth are illumination, not gray absence-of-sun.  The
           ;; constant terms keep vertical and turned-away planes chromatic;
           ;; the hemispheric terms still tell us which environment they see.
           (+ (* sky (+ 0.15 (* 0.54 sky-weight)))
              (* ground (+ 0.05 (* 0.34 ground-weight)))))
         (ambient-accessibility
           (- 1.0 (* #.*local-ambient-occlusion-strength*
                     ambient-occlusion)))
         ;; A sun-facing receiver inside a cast shadow still sees warm light
         ;; returned by the illuminated world around it.  This little wash is
         ;; deliberately conditional on occlusion: it animates sun patches
         ;; without turning north-facing planes orange.
         (warm-return
           (* sun-color
              (* 0.085 direct-shape (- 1.0 direct-visibility))))
         (light (+ (* sun-color (* direct-shape direct-visibility))
                   (* indirect-light ambient-accessibility)
                   warm-return))
         (view-direction
           (normalize (- (swizzle camera-position :xyz) world-position)))
         (half-vector (normalize (+ view-direction sun)))
         (n-dot-v (max 0.0 (dot shading-normal view-direction)))
         (n-dot-h (max 0.0 (dot shading-normal half-vector)))
         (n-dot-l (max facing 0.0))
         (v-dot-h (max 0.0 (dot view-direction half-vector)))
         (alpha (* roughness roughness))
         (alpha-squared (* alpha alpha))
         (distribution-denominator
           (+ (* (* n-dot-h n-dot-h) (- alpha-squared 1.0)) 1.0))
         (distribution
           (/ alpha-squared
              (max 0.0001
                   (* 3.14159265
                      (* distribution-denominator distribution-denominator)))))
         (visibility-light
           (* n-dot-v
              (sqrt (+ (* (* n-dot-l n-dot-l) (- 1.0 alpha-squared))
                       alpha-squared))))
         (visibility-view
           (* n-dot-l
              (sqrt (+ (* (* n-dot-v n-dot-v) (- 1.0 alpha-squared))
                       alpha-squared))))
         (visibility
           (/ 0.5 (max 0.0001 (+ visibility-light visibility-view))))
         (fresnel (+ 0.04 (* 0.96 (expt (- 1.0 v-dot-h) 5.0))))
         (specular
           (* sun-color
              (* (* 0.34 direct-visibility)
                 (* distribution
                    (* visibility (* fresnel n-dot-l))))))
         (lit (+ (* base (* light (stock-tooth material-point))) specular))
         ;; Keep scene radiance linear and HDR here.  The universal
         ;; presentation pass blooms, tone maps, and grades it exactly once.
         (mapped-paper (* lit 1.08))
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
         (drafted (mix paper construction-ink construction-wire))
         (radiance (mix drafted blueprint reticle)))
    (set-output color-output (vec4 radiance 1.0))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip
                                      temporal-parameters))))

(define-shader shadow-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position))
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
                            (character-parameters :vec4)
                            (sun-vector :vec4)
                            (sun-color-vector :vec4)
                            (sky-color-vector :vec4)
                            (ground-color-vector :vec4)
                            (shadow-row-x :vec4)
                            (shadow-row-y :vec4)
                            (shadow-row-z :vec4)
                            (shadow-row-w :vec4)
                            (shadow-control :vec4)))))
  (let* ((instance (buffer-element instances instance-index))
         (template-id (uint (ldb (byte 16 0) (swizzle instance :w))))
         (template-index (+ (* template-id (uint 6.0)) vertex-index))
         (template-vertex (buffer-element template-vertices template-index))
         (world-position (mesh-world-position instance template-vertex)))
    (set-output clip-position
                (light-clip-position world-position
                                     shadow-row-x shadow-row-y
                                     shadow-row-z shadow-row-w))))

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
                           (swizzle render-parameters :z)))
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
         (texel (swizzle inspection-parameters :zw))
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
                            shadow-pigment pooling)))
         ;; Keep the full-resolution pass to a single scene sample.  MetalFX
         ;; already reconstructs the luminance neighborhood; a second 22-tap
         ;; glow/AO filter here consumed the frame budget without resolving
         ;; additional scene detail.
         (glowing (swizzle value :xyz))
         (mapped (paper-grade (paper-tonemap (* glowing 1.02)))))
    (set-output color-output
                (vec4 mapped 1.0))))
