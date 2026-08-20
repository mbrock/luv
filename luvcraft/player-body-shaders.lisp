;;; The player's body as an analytic field.
;;;
;;; The complete neutral avatar below is the same kind of object as an SDF
;;; cat or gnome: torso, head, limbs, and hands are one smooth signed-distance
;;; field.  First person normally sees only the view-local arm slice of that
;;; body.  Its palms still come from BODY.LISP, so walking bob, breathing, and
;;; holding a phone remain authored once; a lower-screen proxy merely launches
;;; the rays which find the round sleeves, wrists, and hands.

(in-package #:luvcraft.shaders)

(defparameter *player-sdf-arm-radius* 0.040
  "Radius of the player's SDF forearms in view-space cells.")
(defparameter *player-sdf-hand-size* 0.045
  "Overall radius of the player's SDF hands.")
(defparameter *player-sdf-sleeve-fraction* 0.74
  "Fraction of shoulder-to-palm reach covered by the sleeve.")
(defparameter *player-sdf-shoulder-spread* 0.36
  "Half the separation between the viewmodel shoulders.")
(defparameter *player-sdf-smoothing* 0.020
  "Width of the soft SDF weld between each wrist and hand.")

(macrolet
    ((define-player-body-knob (name place quantity minimum maximum step label)
       `(luvcraft:define-knob ,name
            (:group :player :label ,label
             :quantity (:quantity ,quantity
                        :unit ,(if (eq quantity :figure-proportion) :one :cell))
             :minimum ,minimum :maximum ,maximum :step ,step)
          ,place)))
  (define-player-body-knob player-sdf-arm-radius *player-sdf-arm-radius*
    :figure-extent 0.035 0.14 0.005 "SDF arm radius")
  (define-player-body-knob player-sdf-hand-size *player-sdf-hand-size*
    :figure-extent 0.045 0.17 0.005 "SDF hand size")
  (define-player-body-knob player-sdf-sleeve-fraction *player-sdf-sleeve-fraction*
    :figure-proportion 0.25 0.92 0.02 "SDF sleeve length")
  (define-player-body-knob player-sdf-shoulder-spread *player-sdf-shoulder-spread*
    :figure-extent 0.16 0.55 0.01 "SDF shoulder spread")
  (define-player-body-knob player-sdf-smoothing *player-sdf-smoothing*
    :figure-extent 0.0 0.10 0.005 "SDF body smoothing"))

;;; ---------------------------------------------------------------------
;;; Shared field vocabulary and the complete avatar.

(define-shader-function player-sdf-length (vector)
  (sqrt (max (dot vector vector) 1e-12)))

(define-shader-function player-sdf-smooth-union
    (first-distance second-distance radius)
  (let* ((safe-radius (max radius 1e-5))
         (blend (clamp (+ 0.5 (* 0.5 (/ (- second-distance first-distance)
                                        safe-radius)))
                       0.0 1.0)))
    (- (mix second-distance first-distance blend)
       (* radius (* blend (- 1.0 blend))))))

(define-shader-function player-sdf-sphere-distance (point center radius)
  (- (player-sdf-length (- point center)) radius))

(define-shader-function player-sdf-ellipsoid-distance (point center radii)
  (let* ((offset (/ (- point center) radii))
         (outer (player-sdf-length offset))
         (gradient (player-sdf-length (/ offset radii))))
    (/ (* outer (- outer 1.0)) (max gradient 1e-6))))

(define-shader-function player-sdf-capsule-distance
    (point beginning end radius)
  (let* ((offset (- point beginning))
         (axis (- end beginning))
         (fraction (clamp (/ (dot offset axis)
                             (max (dot axis axis) 1e-6))
                          0.0 1.0)))
    (- (player-sdf-length (- offset (* axis fraction))) radius)))

(define-shader-function player-avatar-distance (point)
  "The player's complete neutral body, feet at y=0 and forward along +z."
  (let* ((torso (player-sdf-ellipsoid-distance
                 point (vec3 0.0 1.15 0.0) (vec3 0.30 0.46 0.19)))
         (head (player-sdf-sphere-distance point (vec3 0.0 1.67 0.0) 0.23))
         (mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z)))
         (leg (player-sdf-capsule-distance
               mirrored (vec3 0.14 0.10 0.0) (vec3 0.14 0.78 0.0) 0.115))
         (arm (player-sdf-capsule-distance
               mirrored (vec3 0.31 1.43 0.0) (vec3 0.42 0.86 0.03)
               player-sdf-arm-radius))
         (hand (player-sdf-ellipsoid-distance
                mirrored (vec3 0.42 0.80 0.05)
                (vec3 player-sdf-hand-size
                      (* player-sdf-hand-size 0.86)
                      (* player-sdf-hand-size 1.08)))))
    (player-sdf-smooth-union
     (player-sdf-smooth-union
      (player-sdf-smooth-union torso head 0.06) leg 0.055)
      (player-sdf-smooth-union arm hand player-sdf-smoothing) 0.045)))

;;; ---------------------------------------------------------------------
;;; The first-person slice of the same limb vocabulary.

(define-shader-function player-viewmodel-shoulder (palm side)
  (vec3 (* side player-sdf-shoulder-spread)
        -0.72
        0.37))

(define-shader-function player-viewmodel-sleeve-distance (point palm side)
  (let* ((shoulder (player-viewmodel-shoulder palm side))
         (cuff (mix shoulder palm player-sdf-sleeve-fraction)))
    (player-sdf-capsule-distance
     point shoulder cuff player-sdf-arm-radius)))

(define-shader-function player-viewmodel-skin-distance (point palm side)
  (let* ((shoulder (player-viewmodel-shoulder palm side))
         (wrist (mix shoulder palm (- player-sdf-sleeve-fraction 0.035)))
         (bare (player-sdf-capsule-distance
                point wrist palm (* player-sdf-arm-radius 0.90)))
         (hand (player-sdf-ellipsoid-distance
                point palm
                (vec3 player-sdf-hand-size
                      (* player-sdf-hand-size 0.70)
                      (* player-sdf-hand-size 1.08))))
         ;; The thumb is part of the hand-size vocabulary, not a fixed bead:
         ;; tuning the palm in the metabar preserves a recognizable hand.
         (thumb-center
           (+ palm
              (vec3 (* side (* player-sdf-hand-size -0.86))
                    (* player-sdf-hand-size 0.067)
                    (* player-sdf-hand-size -0.20))))
         (thumb (player-sdf-ellipsoid-distance
                 point thumb-center
                 (vec3 (* player-sdf-hand-size 0.57)
                       (* player-sdf-hand-size 0.63)
                       (* player-sdf-hand-size 0.73)))))
    (player-sdf-smooth-union
     bare (player-sdf-smooth-union hand thumb 0.018)
     player-sdf-smoothing)))

(define-shader-function player-viewmodel-distance (point left-palm right-palm)
  (let* ((left-sleeve
           (player-viewmodel-sleeve-distance point left-palm -1.0))
         (right-sleeve
           (player-viewmodel-sleeve-distance point right-palm 1.0))
         (left-skin (player-viewmodel-skin-distance point left-palm -1.0))
         (right-skin (player-viewmodel-skin-distance point right-palm 1.0)))
    (min (min left-sleeve right-sleeve) (min left-skin right-skin))))

(define-shader-function player-viewmodel-albedo (point left-palm right-palm)
  (let* ((sleeve (min (player-viewmodel-sleeve-distance point left-palm -1.0)
                      (player-viewmodel-sleeve-distance point right-palm 1.0)))
         (skin (min (player-viewmodel-skin-distance point left-palm -1.0)
                    (player-viewmodel-skin-distance point right-palm 1.0)))
         (skin-nearer (step skin sleeve))
         (sleeve-color (vec3 0.075 0.20 0.30))
         (skin-color (vec3 0.74 0.43 0.29)))
    (mix sleeve-color skin-color skin-nearer)))

(define-shader-function player-viewmodel-normal (point left-palm right-palm)
  (let* ((reach 0.0012)
         (a (vec3 1.0 -1.0 -1.0))
         (b (vec3 -1.0 -1.0 1.0))
         (c (vec3 -1.0 1.0 -1.0))
         (d (vec3 1.0 1.0 1.0)))
    (normalize
     (+ (+ (* a (player-viewmodel-distance
                 (+ point (* a reach)) left-palm right-palm))
           (* b (player-viewmodel-distance
                 (+ point (* b reach)) left-palm right-palm)))
        (+ (* c (player-viewmodel-distance
                 (+ point (* c reach)) left-palm right-palm))
           (* d (player-viewmodel-distance
                 (+ point (* d reach)) left-palm right-palm)))))))

;;; ---------------------------------------------------------------------
;;; Lower-screen proxy and marcher.

(define-shader-method shader-specification-for
    player-body-sdf-vertex-specification
    ((role (eql :player-body-sdf)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (left-palm-input :vec4 :location 1)
              (right-palm-input :vec4 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-view-position :vec3 :location 0)
               (left-palm-output :vec4 :location 1)
               (right-palm-output :vec4 :location 2))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((corner-x (- (* (swizzle quad-corner :x) 2.0) 1.0))
         ;; In this projection positive clip y is down the presented image.
         ;; The arms never need the top fifth of the screen.
         (corner-y (- (* (swizzle quad-corner :y) 1.18) 0.18))
         (x-scale (representation (swizzle projection-vector :x)))
         (y-scale (representation (swizzle projection-vector :y)))
         (view-position (vec3 (/ corner-x x-scale)
                              (- (/ corner-y y-scale))
                              1.0)))
    (set-output clip-position (vec4 corner-x corner-y 0.0 1.0))
    (set-output proxy-view-position view-position)
    (set-output left-palm-output left-palm-input)
    (set-output right-palm-output right-palm-input)))

(define-shader-method shader-specification-for
    player-body-sdf-fragment-specification
    ((role (eql :player-body-sdf)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((proxy-view-position :vec3 :location 0)
              (left-palm-input :vec4 :location 1)
              (right-palm-input :vec4 :location 2))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((left-palm (swizzle left-palm-input :xyz))
         (right-palm (swizzle right-palm-input :xyz))
         (ray (normalize proxy-view-position))
         (travel
           (counted-fold (march 52.0 ray-distance 0.035)
             (let* ((point (* ray ray-distance))
                    (distance
                      (player-viewmodel-distance point left-palm right-palm)))
               (if (< distance 0.0007)
                   ray-distance
                   (if (> ray-distance 1.55)
                       ray-distance
                       (+ ray-distance (max (* distance 0.86) 0.0007)))))))
         (point (* ray travel))
         (surface-distance
           (player-viewmodel-distance point left-palm right-palm))
         (coverage (- 1.0 (step 0.0028 surface-distance)))
         (local-normal (player-viewmodel-normal point left-palm right-palm))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         (normal (+ (* right (swizzle local-normal :x))
                    (+ (* up (swizzle local-normal :y))
                       (* forward (swizzle local-normal :z)))))
         (albedo (player-viewmodel-albedo point left-palm right-palm))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (lambert (dot normal sun-direction))
         (wrapped (max 0.0 (/ (+ lambert 0.42) 1.42)))
         (view-facing (max 0.0 (dot local-normal (* ray -1.0))))
         (rim (expt (- 1.0 view-facing) 3.0))
         (halfway (normalize (- sun-direction
                                (+ (* right (swizzle ray :x))
                                   (+ (* up (swizzle ray :y))
                                      (* forward (swizzle ray :z)))))))
         (specular (* 0.16 (expt (max 0.0 (dot normal halfway)) 28.0)))
         (illumination (+ (* ambient 0.90)
                          (* sun-color (+ 0.16 (* wrapped 0.95)))))
         (radiance (+ (+ (* albedo illumination)
                         (* sun-color (* specular coverage)))
                      (* (vec3 0.32 0.56 0.76) (* rim 0.08)))))
    (set-output color-output (vec4 (* radiance coverage) coverage))))
