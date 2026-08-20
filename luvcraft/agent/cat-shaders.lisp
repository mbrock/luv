;;; The cat agent's analytic body.  Like the gnome, a conservative billboard
;;; only launches rays; the visible animal is the zero set of this field.

(in-package #:luvcraft.shaders)

(defparameter *cat-stature* 0.82
  "World cells per cat figure unit; the sitting cat is 1.30 units high.")

(defparameter *cat-head-size* 0.285
  "The cat's head radius in figure units.")

(defparameter *cat-ear-height* 1.31
  "How high the ear tips stand above the cat's paws, in figure units.")

(defparameter *cat-tail-reach* 0.54
  "How far the question-mark tail sweeps out from the cat's side.")

(defparameter *cat-eye-size* 0.13
  "The angular radius of the cat's green eyes on its head.")

(defparameter *cat-stripe-frequency* 31.0
  "The tabby coat's vertical stripe frequency in figure space.")

(defparameter *cat-stripe-strength* 0.72
  "How strongly the tabby stripes darken the orange coat.")

(defparameter *cat-rim-light* 0.11
  "The warm edge light separating the cat from the world behind it.")

(macrolet
    ((define-cat-knob (name place quantity minimum maximum step documentation)
       `(luvcraft:define-knob ,name
            (:group :cat
             :quantity (:quantity ,quantity
                        :unit ,(if (eq quantity :figure-extent) :cell :one))
             :minimum ,minimum :maximum ,maximum :step ,step
             :documentation ,documentation)
          ,place)))
  (define-cat-knob cat-stature *cat-stature*
    :figure-proportion 0.45 1.40 0.025
    "The cat agent's scale in world cells per figure unit.")
  (define-cat-knob cat-head-size *cat-head-size*
    :figure-extent 0.18 0.45 0.005
    "The radius of the cat's head.")
  (define-cat-knob cat-ear-height *cat-ear-height*
    :figure-extent 1.08 1.75 0.01
    "The height of the cat's pointed ear tips.")
  (define-cat-knob cat-tail-reach *cat-tail-reach*
    :figure-extent 0.32 0.90 0.01
    "How far the cat's curled tail reaches to the side.")
  (define-cat-knob cat-eye-size *cat-eye-size*
    :figure-proportion 0.07 0.22 0.005
    "The angular radius of the cat's green eyes.")
  (define-cat-knob cat-stripe-frequency *cat-stripe-frequency*
    :figure-proportion 8.0 52.0 1.0
    "How many narrow tabby bands cross the cat's coat.")
  (define-cat-knob cat-stripe-strength *cat-stripe-strength*
    :figure-proportion 0.0 1.0 0.025
    "How darkly the tabby bands mark the orange coat.")
  (define-cat-knob cat-rim-light *cat-rim-light*
    :figure-proportion 0.0 0.6 0.01
    "Warm outline around the cat's SDF silhouette."))

(define-shader-function cat-capsule-distance (point beginning end radius)
  "Signed distance to the round segment from BEGINNING to END."
  (let* ((offset (- point beginning))
         (axis (- end beginning))
         (fraction (clamp (/ (dot offset axis)
                             (max (dot axis axis) 1e-6))
                          0.0 1.0)))
    (- (gnome-length (- offset (* axis fraction))) radius)))

(define-shader-function cat-body-distance (point)
  "The sitting torso, chest, and paired haunches."
  (let* ((torso (gnome-ellipsoid-distance
                 point (vec3 0.0 0.52 -0.06) (vec3 0.31 0.43 0.28)))
         (chest (gnome-ellipsoid-distance
                 point (vec3 0.0 0.52 0.17) (vec3 0.23 0.34 0.19)))
         (mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z)))
         (haunch (gnome-ellipsoid-distance
                  mirrored (vec3 0.24 0.28 -0.10) (vec3 0.25 0.27 0.28))))
    (gnome-smooth-union
     (gnome-smooth-union torso chest 0.10) haunch 0.09)))

(define-shader-function cat-head-distance (point)
  "The cat's round head."
  (gnome-ellipsoid-distance
   point (vec3 0.0 0.94 0.08)
   (vec3 cat-head-size
         (* cat-head-size 0.8947368)
         (* cat-head-size 0.9298246))))

(define-shader-function cat-ear-distance (point)
  "Two compact tapered ears, mirrored around the face."
  (let* ((ear-point
           (vec3 (- (abs (swizzle point :x)) 0.17)
                 (swizzle point :y)
                 (- (swizzle point :z) 0.045))))
    (gnome-round-cone-distance ear-point 1.03 cat-ear-height 0.135 0.012)))

(define-shader-function cat-muzzle-distance (point)
  "The paired pale pads and small chin projecting from the face."
  (let* ((mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z)))
         (pad (gnome-ellipsoid-distance
               mirrored (vec3 0.085 0.865 0.285) (vec3 0.115 0.09 0.105)))
         (chin (gnome-ellipsoid-distance
                point (vec3 0.0 0.80 0.245) (vec3 0.11 0.075 0.09))))
    (gnome-smooth-union pad chin 0.035)))

(define-shader-function cat-paw-distance (point)
  "Two front paws planted together in front of the sitting body."
  (let* ((mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z))))
    (gnome-ellipsoid-distance
     mirrored (vec3 0.135 0.105 0.205) (vec3 0.125 0.12 0.20))))

(define-shader-function cat-tail-distance (point)
  "A three-segment question-mark tail rising along the cat's left side."
  (let* ((lower (cat-capsule-distance
                 point (vec3 -0.24 0.24 -0.14)
                 (vec3 (- cat-tail-reach) 0.35 -0.10) 0.075))
         (middle (cat-capsule-distance
                  point (vec3 (- cat-tail-reach) 0.35 -0.10)
                  (vec3 (- cat-tail-reach) 0.67 -0.04) 0.072))
         (tip (cat-capsule-distance
               point (vec3 (- cat-tail-reach) 0.67 -0.04)
               (vec3 (- (* cat-tail-reach 0.74)) 0.86 0.04) 0.068)))
    (gnome-smooth-union
     (gnome-smooth-union lower middle 0.055) tip 0.05)))

(define-shader-function cat-distance (point)
  "The complete sitting cat, with soft joins but distinct silhouette parts."
  (let* ((body (cat-body-distance point))
         (head (cat-head-distance point))
         (ears (cat-ear-distance point))
         (muzzle (cat-muzzle-distance point))
         (paws (cat-paw-distance point))
         (tail (cat-tail-distance point)))
    (gnome-smooth-union
     (gnome-smooth-union
      (gnome-smooth-union
       (gnome-smooth-union (gnome-smooth-union body head 0.11) ears 0.035)
       muzzle 0.045)
      paws 0.06)
     tail 0.045)))

(define-shader-function cat-normal (point)
  "The cat field's tetrahedral four-sample gradient."
  (let* ((reach 0.0015)
         (a (vec3 1.0 -1.0 -1.0))
         (b (vec3 -1.0 -1.0 1.0))
         (c (vec3 -1.0 1.0 -1.0))
         (d (vec3 1.0 1.0 1.0)))
    (normalize
     (+ (+ (* a (cat-distance (+ point (* a reach))))
           (* b (cat-distance (+ point (* b reach)))))
        (+ (* c (cat-distance (+ point (* c reach))))
           (* d (cat-distance (+ point (* d reach)))))))))

(define-shader-function cat-face-detail (point albedo head-weight)
  "Paint green eyes, slit pupils, and a rose nose onto the head surface."
  (let* ((mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z)))
         (direction (normalize (- mirrored (vec3 0.0 0.94 0.08))))
         (eye-direction (normalize (vec3 0.39 0.16 0.86)))
         (eye-offset (- direction eye-direction))
         (eye-distance (gnome-length (* eye-offset (vec3 0.80 1.15 0.80))))
         (eye (- 1.0 (smoothstep (- cat-eye-size 0.025)
                                 (+ cat-eye-size 0.025)
                                 eye-distance)))
         (pupil-distance
           (gnome-length (* eye-offset (vec3 3.4 0.82 0.82))))
         (pupil (- 1.0 (smoothstep (* cat-eye-size 0.46)
                                   (* cat-eye-size 0.73)
                                   pupil-distance)))
         (glint (gnome-sphere-distance
                 direction (normalize (vec3 0.34 0.22 0.90)) 0.035))
         (glint-mask (- 1.0 (smoothstep 0.0 0.025 glint)))
         (nose-direction (normalize (vec3 0.0 -0.28 0.96)))
         (nose-distance
           (gnome-length (* (- direction nose-direction)
                            (vec3 0.65 1.55 0.80))))
         (nose (- 1.0 (smoothstep 0.085 0.125 nose-distance)))
         (eyed (mix albedo (vec3 0.31 0.68 0.30) (* eye head-weight)))
         (pupilled (mix eyed (vec3 0.015 0.012 0.010)
                         (* pupil head-weight)))
         (lit (mix pupilled (vec3 0.95 0.98 0.86)
                   (* glint-mask (* eye head-weight))))
         (nosed (mix lit (vec3 0.72 0.24 0.25) (* nose head-weight))))
    nosed))

(define-shader-function cat-albedo (point)
  "A warm tabby coat with pale chest, muzzle, and toes."
  (let* ((body (cat-body-distance point))
         (head (cat-head-distance point))
         (ears (cat-ear-distance point))
         (muzzle (cat-muzzle-distance point))
         (paws (cat-paw-distance point))
         (tail (cat-tail-distance point))
         (grain (- (lattice-noise (* point (vec3 22.0 18.0 22.0))) 0.5))
         (stripe-wave (sin (+ (* (swizzle point :y) cat-stripe-frequency)
                              (* (swizzle point :x)
                                 (* cat-stripe-frequency 0.2580645)))))
         (stripe (smoothstep 0.48 0.88 stripe-wave))
         (coat (* (mix (vec3 0.48 0.17 0.045) (vec3 0.16 0.055 0.025)
                       (* stripe cat-stripe-strength))
                  (+ 1.0 (* grain 0.18))))
         (cream (vec3 0.78 0.61 0.37))
         (ear-color (vec3 0.58 0.19 0.16))
         (near-head (step head body))
         (color-1 (mix coat coat near-head))
         (distance-1 (min body head))
         (near-ear (step ears distance-1))
         (color-2 (mix color-1 ear-color near-ear))
         (distance-2 (min distance-1 ears))
         (near-muzzle (step muzzle distance-2))
         (color-3 (mix color-2 cream near-muzzle))
         (distance-3 (min distance-2 muzzle))
         (near-paws (step paws distance-3))
         (color-4 (mix color-3 cream near-paws))
         (distance-4 (min distance-3 paws))
         (near-tail (step tail distance-4))
         (color-5 (mix color-4 coat near-tail))
         (head-weight
           (- 1.0 (smoothstep 0.0 0.040
                              (- head (min (min body ears)
                                           (min muzzle (min paws tail))))))))
    (cat-face-detail point color-5 head-weight)))

(define-shader-method shader-specification-for
    cat-sdf-vertex-specification
    ((role (eql :cat-sdf)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (sphere-center-radius :vec4 :location 1)
              (figure-facing :vec4 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position :vec3 :location 0)
               (sphere-output :vec4 :location 1)
               (facing-output :vec3 :location 2))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((center (swizzle sphere-center-radius :xyz))
         (radius (swizzle sphere-center-radius :w))
         (camera (representation (swizzle camera-vector :xyz)))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         (corner-x (- (* (swizzle quad-corner :x) 2.0) 1.0))
         (corner-y (- (* (swizzle quad-corner :y) 2.0) 1.0))
         (world-position
           (+ center (+ (* right (* corner-x radius))
                        (* up (* corner-y radius)))))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward))
         (x-scale (representation (swizzle projection-vector :x)))
         (y-scale (representation (swizzle projection-vector :y)))
         (z-scale (representation (swizzle projection-vector :z)))
         (z-offset (representation (swizzle projection-vector :w))))
    (set-output clip-position
                (vec4 (* view-x x-scale)
                      (- (* view-y y-scale))
                      (+ (* view-z z-scale) z-offset)
                      view-z))
    (set-output proxy-world-position world-position)
    (set-output sphere-output sphere-center-radius)
    (set-output facing-output (swizzle figure-facing :xyz))))

(define-shader-method shader-specification-for
    cat-sdf-fragment-specification
    ((role (eql :cat-sdf)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0)
              (sphere-input :vec4 :location 1)
              (facing-input :vec3 :location 2))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (center (swizzle sphere-input :xyz))
         (radius (swizzle sphere-input :w))
         (ray (normalize (- proxy-world-position camera)))
         (toward-eye (- camera center))
         (facing (normalize facing-input))
         (sideways (vec3 (swizzle facing :z) 0.0 (- (swizzle facing :x))))
         (local-camera (vec3 (/ (dot toward-eye sideways) cat-stature)
                             (+ (/ (swizzle toward-eye :y) cat-stature) 0.64)
                             (/ (dot toward-eye facing) cat-stature)))
         (local-ray (vec3 (dot ray sideways)
                          (swizzle ray :y)
                          (dot ray facing)))
         (figure-radius (/ radius cat-stature))
         (to-center (- local-camera (vec3 0.0 0.64 0.0)))
         (half-way (- (dot to-center local-ray)))
         (gap (- (dot to-center to-center) (* figure-radius figure-radius)))
         (discriminant (- (* half-way half-way) gap))
         (span (sqrt (max discriminant 0.0)))
         (entry (max (- half-way span) 0.0))
         (exit (+ half-way span))
         (travel
           (counted-fold (march 68.0 ray-distance entry)
             (let* ((point (+ local-camera (* local-ray ray-distance)))
                    (distance (cat-distance point)))
               (if (< distance 0.0008)
                   ray-distance
                   (if (> ray-distance exit)
                       ray-distance
                       (+ ray-distance (max (* distance 0.82) 0.0008)))))))
         (point (+ local-camera (* local-ray travel)))
         (surface-distance (cat-distance point))
         (coverage (* (- 1.0 (step 0.0035 surface-distance))
                      (- 1.0 (step 0.0 (- discriminant)))))
         (local-normal (cat-normal point))
         (normal (+ (* sideways (swizzle local-normal :x))
                    (+ (vec3 0.0 (swizzle local-normal :y) 0.0)
                       (* facing (swizzle local-normal :z)))))
         (albedo (cat-albedo point))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (lambert (dot normal sun-direction))
         (wrapped (max 0.0 (/ (+ lambert 0.38) 1.38)))
         (sky (+ 0.56 (* 0.44 (swizzle normal :y))))
         (occlusion (clamp (+ 0.38 (* 0.62 (smoothstep -0.08 0.42
                                                       (swizzle point :y))))
                           0.38 1.0))
         (view-facing (max 0.0 (dot normal (* ray -1.0))))
         (rim (expt (- 1.0 view-facing) 3.5))
         (halfway (normalize (- sun-direction ray)))
         (specular (* 0.18 (expt (max 0.0 (dot normal halfway)) 34.0)))
         (illumination
           (+ (* ambient (* sky occlusion))
              (* sun-color (+ 0.12 (* wrapped 1.12)))))
         (radiance
           (+ (+ (* albedo illumination)
                 (* sun-color (* specular coverage)))
              (* (vec3 0.95 0.63 0.32) (* rim cat-rim-light)))))
    (set-output color-output (vec4 (* radiance coverage) coverage))))
