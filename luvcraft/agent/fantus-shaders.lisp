;;; Fantus's analytic body.  NRK's purple felt toddler -- Fantorangen's
;;; little sibling -- sitting with legs out in front, as toddlers do: a big
;;; round head under a peach eye mask, a snout with a peach dot, an open
;;; smile, two enormous cupped ears, a thin neck, a nappy, a dummy on the
;;; chest, three-fingered hands, and hooves.  Like the cat, a conservative
;;; billboard only launches rays; the visible puppet is this field's zero set.  #JGEQ4U

(in-package #:luvcraft.shaders)

(defparameter *fantus-stature* 0.80
  "World cells per Fantus figure unit; the seated puppet is 1.32 units high.")

(defparameter *fantus-head-size* 1.0
  "Fantus's head scale about its own centre; 1 is the puppet's proportion.")

(defparameter *fantus-ear-size* 1.0
  "Scale of the two big cupped ears, hinged at the side of the head.")

(defparameter *fantus-ear-spread* 0.45
  "How far the ears' cups turn from straight forward toward the sides.")

(defparameter *fantus-eyelid* 0.38
  "Where the sleepy peach eyelid line sits on the eye, in eye radii above
its centre; lower is sleepier.")

(defparameter *fantus-smile* 1.0
  "How wide the open smile is opened, as a scale of its height.")

(defparameter *fantus-rim-light* 0.10
  "The cool edge light separating Fantus's felt from the world behind it.")

(macrolet
    ((define-fantus-knob (name place quantity minimum maximum step
                          documentation)
       `(luvcraft:define-knob ,name
            (:group :fantus
             :quantity (:quantity ,quantity
                        :unit ,(if (eq quantity :figure-extent) :cell :one))
             :minimum ,minimum :maximum ,maximum :step ,step
             :documentation ,documentation)
          ,place)))
  (define-fantus-knob fantus-stature *fantus-stature*
    :figure-proportion 0.45 1.40 0.025
    "Fantus's scale in world cells per figure unit.")
  (define-fantus-knob fantus-head-size *fantus-head-size*
    :figure-proportion 0.80 1.20 0.01
    "The scale of Fantus's head, face, and ears about the head's centre.")
  (define-fantus-knob fantus-ear-size *fantus-ear-size*
    :figure-proportion 0.60 1.35 0.01
    "The size of Fantus's big cupped ears.")
  (define-fantus-knob fantus-ear-spread *fantus-ear-spread*
    :figure-proportion 0.0 1.20 0.01
    "How far the ear cups turn outward from facing forward, in radians.")
  (define-fantus-knob fantus-eyelid *fantus-eyelid*
    :figure-proportion -0.60 0.90 0.01
    "The eyelid line on the eye, in eye radii; lower is sleepier.")
  (define-fantus-knob fantus-smile *fantus-smile*
    :figure-proportion 0.0 1.60 0.02
    "How wide Fantus's open smile is.")
  (define-fantus-knob fantus-rim-light *fantus-rim-light*
    :figure-proportion 0.0 0.6 0.01
    "Cool outline around Fantus's SDF silhouette."))

;;; ---------------------------------------------------------------------
;;; Frames and primitives

(define-shader-function fantus-mirrored (point)
  "POINT folded onto the right half: Fantus is bilaterally symmetric."
  (vec3 (abs (swizzle point :x)) (swizzle point :y) (swizzle point :z)))

(define-shader-function fantus-head-point (point)
  "The head's frame: POINT scaled about the head centre by FANTUS-HEAD-SIZE.

Every part above the neck is written at the puppet's proportion and sampled
here, so one knob grows the whole head without the face sliding off it."
  (+ (vec3 0.0 1.04 0.0)
     (/ (- point (vec3 0.0 1.04 0.0)) fantus-head-size)))

(define-shader-function fantus-torus-distance (point center major minor)
  "A ring of radius MAJOR and tube MINOR lying in the XY plane, facing +Z."
  (let* ((offset (- point center))
         (radial (- (gnome-length (vec2 (swizzle offset :x)
                                        (swizzle offset :y)))
                    major)))
    (- (gnome-length (vec2 radial (swizzle offset :z))) minor)))

(define-shader-function fantus-ear-point (point)
  "One ear's own frame, on the right side of the folded head.

The ear is a flattened ellipsoid whose cup faces local +Z.  It is turned
outward about the vertical by FANTUS-EAR-SPREAD and rolled so its broad top
leans away from the head, as the felt flops on the puppet."
  (let* ((offset (- (fantus-mirrored point)
                    (vec3 (+ 0.235 (* 0.18 fantus-ear-size)) 1.00 -0.03)))
         (yaw-cosine (cos fantus-ear-spread))
         (yaw-sine (sin fantus-ear-spread))
         (across (- (* (swizzle offset :x) yaw-cosine)
                    (* (swizzle offset :z) yaw-sine)))
         (out (+ (* (swizzle offset :x) yaw-sine)
                 (* (swizzle offset :z) yaw-cosine)))
         (roll-cosine 0.96891242)
         (roll-sine 0.24740396))
    (/ (vec3 (- (* across roll-cosine) (* (swizzle offset :y) roll-sine))
             (+ (* across roll-sine) (* (swizzle offset :y) roll-cosine))
             out)
       fantus-ear-size)))

(define-shader-function fantus-scaled-ellipsoid-distance (point center radii)
  "A bound on an ellipsoid's distance that stays honest deep inside it.

GNOME-ELLIPSOID-DISTANCE goes to zero at the centre, which is harmless for
a solid but fatal for a hollow: subtracted, it makes a phantom surface where
the cutter's centre is.  Scaling the unit-sphere distance by the smallest
radius is a lower bound everywhere, inside and out."
  (* (- (gnome-length (/ (- point center) radii)) 1.0)
     (min (min (swizzle radii :x) (swizzle radii :y)) (swizzle radii :z))))

(define-shader-function fantus-ear-paddle (ear)
  "Widen the ear toward its top so it is a paddle, not a disc.

The felt ear is narrow where it is sewn to the head and broad where it
flops.  Dividing across by a width that grows with height does that; the
0.80 on the distance pays for the stretch."
  (vec3 (/ (swizzle ear :x)
           (+ 0.80 (* 0.34 (smoothstep -0.24 0.20 (swizzle ear :y)))))
        (swizzle ear :y)
        (swizzle ear :z)))

;;; ---------------------------------------------------------------------
;;; The parts of Fantus
;;;
;;; As with the gnome and cat, each function answers for one material, and
;;; the albedo asks the same functions which part owns a hit.

(define-shader-function fantus-ear-distance (point)
  "Two big ears: flattened ellipsoids with a shallow cup pressed into the front.

The cup is a subtraction of a smaller, offset ellipsoid; the purple rim it
leaves is the felt hem the puppet's ears have.  Both distances are scaled
back by the ear size so the warped field remains a lower bound."
  (let* ((ear (fantus-ear-paddle (fantus-ear-point (fantus-head-point point))))
         (outer (gnome-ellipsoid-distance
                 ear (vec3 0.0 0.0 0.0) (vec3 0.17 0.235 0.040)))
         (cup (fantus-scaled-ellipsoid-distance
               ear (vec3 0.010 0.006 0.056) (vec3 0.140 0.200 0.034))))
    (* (* (max outer (- cup)) 0.80) (* fantus-ear-size fantus-head-size))))

(define-shader-function fantus-head-distance (point)
  "The big round head, its snout, the nose button, and the thin neck."
  (let* ((head-point (fantus-head-point point))
         (skull (gnome-ellipsoid-distance
                 head-point (vec3 0.0 1.045 -0.01) (vec3 0.27 0.28 0.26)))
         (muzzle (gnome-ellipsoid-distance
                  head-point (vec3 0.0 0.935 0.150) (vec3 0.165 0.125 0.140)))
         (bridge (cat-capsule-distance
                  head-point (vec3 0.0 0.975 0.215) (vec3 0.0 1.045 0.215)
                  0.055))
         (nose (gnome-ellipsoid-distance
                head-point (vec3 0.0 0.990 0.280) (vec3 0.060 0.055 0.055)))
         (face (* (gnome-smooth-union
                   (gnome-smooth-union
                    (gnome-smooth-union skull muzzle 0.07) bridge 0.05)
                   nose 0.025)
                  fantus-head-size))
         (neck (cat-capsule-distance
                point (vec3 0.0 0.58 -0.01) (vec3 0.0 0.86 0.0) 0.060)))
    (gnome-smooth-union face neck 0.05)))

(define-shader-function fantus-eye-distance (point)
  "Two large glossy eyeballs set into the mask."
  (* (gnome-sphere-distance (fantus-mirrored (fantus-head-point point))
                            (vec3 0.104 1.088 0.196) 0.068)
     fantus-head-size))

(define-shader-function fantus-body-distance (point)
  "The slender torso above the nappy, the thin arms, and the hands."
  (let* ((mirrored (fantus-mirrored point))
         (torso (gnome-ellipsoid-distance
                 point (vec3 0.0 0.43 -0.01) (vec3 0.155 0.215 0.125)))
         (upper-arm (cat-capsule-distance
                     mirrored (vec3 0.14 0.56 -0.01) (vec3 0.25 0.38 0.06)
                     0.044))
         (forearm (cat-capsule-distance
                   mirrored (vec3 0.25 0.38 0.06) (vec3 0.30 0.20 0.17)
                   0.040))
         (palm (gnome-ellipsoid-distance
                mirrored (vec3 0.305 0.165 0.205) (vec3 0.050 0.040 0.058)))
         (first-finger (cat-capsule-distance
                        mirrored (vec3 0.290 0.155 0.235)
                        (vec3 0.275 0.115 0.290) 0.019))
         (second-finger (cat-capsule-distance
                         mirrored (vec3 0.315 0.150 0.240)
                         (vec3 0.325 0.110 0.295) 0.019))
         (thumb (cat-capsule-distance
                 mirrored (vec3 0.335 0.165 0.215)
                 (vec3 0.370 0.135 0.250) 0.018))
         (hand (gnome-smooth-union
                (gnome-smooth-union
                 (gnome-smooth-union palm first-finger 0.02)
                 second-finger 0.02)
                thumb 0.02))
         (arm (gnome-smooth-union
               (gnome-smooth-union upper-arm forearm 0.03) hand 0.03)))
    (gnome-smooth-union torso arm 0.05)))

(define-shader-function fantus-leg-distance (point)
  "Two felt legs stretched forward along the ground from the nappy."
  (cat-capsule-distance
   (fantus-mirrored point) (vec3 0.105 0.125 0.04) (vec3 0.150 0.078 0.420)
   0.068))

(define-shader-function fantus-hoof-distance (point)
  "Grey hooves capping the legs, their flat soles turned toward the viewer."
  (max (gnome-ellipsoid-distance
        (fantus-mirrored point) (vec3 0.152 0.078 0.440)
        (vec3 0.074 0.074 0.062))
       (- (swizzle point :z) 0.488)))

(define-shader-function fantus-nappy-distance (point)
  "The white cloth nappy, a soft rounded bundle the legs come out of."
  (gnome-ellipsoid-distance
   point (vec3 0.0 0.19 -0.01) (vec3 0.225 0.165 0.195)))

(define-shader-function fantus-dummy-distance (point)
  "The yellow dummy hung on the chest: ring, shield, and teat."
  (let* ((ring (fantus-torus-distance
                point (vec3 0.0 0.505 0.140) 0.042 0.010))
         (shield (gnome-ellipsoid-distance
                  point (vec3 0.0 0.445 0.142) (vec3 0.062 0.032 0.014)))
         (teat (gnome-ellipsoid-distance
                point (vec3 0.0 0.395 0.150) (vec3 0.020 0.042 0.019))))
    (min (min ring shield) teat)))

(define-shader-function fantus-distance (point)
  "The complete seated Fantus, with felt joins but distinct silhouette parts."
  (let* ((head (fantus-head-distance point))
         (ears (fantus-ear-distance point))
         (eyes (fantus-eye-distance point))
         (body (fantus-body-distance point))
         (legs (fantus-leg-distance point))
         (hooves (fantus-hoof-distance point))
         (nappy (fantus-nappy-distance point))
         (dummy (fantus-dummy-distance point)))
    (min
     (gnome-smooth-union
      (gnome-smooth-union
       (gnome-smooth-union
        (gnome-smooth-union head ears 0.035)
        body 0.04)
       nappy 0.03)
      (gnome-smooth-union legs hooves 0.012)
      0.03)
     (min eyes dummy))))

(define-shader-function fantus-normal (point)
  "The Fantus field's tetrahedral four-sample gradient."
  (let* ((reach 0.0012)
         (a (vec3 1.0 -1.0 -1.0))
         (b (vec3 -1.0 -1.0 1.0))
         (c (vec3 -1.0 1.0 -1.0))
         (d (vec3 1.0 1.0 1.0)))
    (normalize
     (+ (+ (* a (fantus-distance (+ point (* a reach))))
           (* b (fantus-distance (+ point (* b reach)))))
        (+ (* c (fantus-distance (+ point (* c reach))))
           (* d (fantus-distance (+ point (* d reach)))))))))

;;; ---------------------------------------------------------------------
;;; Felt colours

(define-shader-function fantus-inside-ellipse (offset radii softness)
  "1 inside the ellipse of RADII about the origin of OFFSET, 0 outside."
  (- 1.0 (smoothstep (- 1.0 softness) (+ 1.0 softness)
                     (gnome-length (/ offset radii)))))

(define-shader-function fantus-face-paint (point purple peach)
  "The head's felt: the peach eye mask, the nose dot, and the open smile.

All marks are drawn in the head's frame, projected along Z onto the face,
so they stay on it at every head size."
  (let* ((face (fantus-head-point point))
         (mirrored (fantus-mirrored face))
         (front (smoothstep 0.02 0.10 (swizzle face :z)))
         ;; The mask: a soft peach disc around each eye, joined across the
         ;; brow, with the purple snout rising into it from below.
         (eye-disc (fantus-inside-ellipse
                    (- (vec2 (swizzle mirrored :x) (swizzle mirrored :y))
                       (vec2 0.106 1.095))
                    (vec2 0.128 0.122) 0.04))
         (brow (* (fantus-inside-ellipse
                   (- (vec2 (swizzle mirrored :x) (swizzle mirrored :y))
                      (vec2 0.0 1.135))
                   (vec2 0.165 0.080) 0.05)
                  1.0))
         (snout-rise (fantus-inside-ellipse
                      (- (vec2 (swizzle mirrored :x) (swizzle mirrored :y))
                         (vec2 0.0 1.000))
                      (vec2 0.068 0.075) 0.06))
         (mask (* front (* (max eye-disc brow) (- 1.0 snout-rise))))
         ;; The peach dot on the very end of the nose button.
         (nose-dot (* (smoothstep 0.24 0.30 (swizzle face :z))
                      (fantus-inside-ellipse
                       (- (vec2 (swizzle face :x) (swizzle face :y))
                          (vec2 0.0 0.993))
                       (vec2 0.030 0.027) 0.08)))
         ;; The smile: an open crescent under the snout, corners lifted.
         (across (/ (swizzle face :x) 0.150))
         (upper-lip (+ 0.928 (* 0.022 (* across across))))
         (lower-lip (- upper-lip (* (* 0.072 fantus-smile)
                                    (max (- 1.0 (* across across)) 0.0))))
         (in-mouth (* (* (smoothstep 0.12 0.18 (swizzle face :z))
                         (step (abs across) 1.0))
                      (* (smoothstep (- lower-lip 0.004) (+ lower-lip 0.004)
                                     (swizzle face :y))
                         (- 1.0 (smoothstep (- upper-lip 0.004)
                                            (+ upper-lip 0.004)
                                            (swizzle face :y))))))
         (tongue (* in-mouth
                    (fantus-inside-ellipse
                     (- (vec2 (swizzle face :x) (swizzle face :y))
                        (vec2 0.025 (- upper-lip 0.016)))
                     (vec2 0.060 0.018) 0.10)))
         (masked (mix purple peach mask))
         (dotted (mix masked peach nose-dot))
         (mouthed (mix dotted (vec3 0.035 0.010 0.018) in-mouth)))
    (mix mouthed (vec3 0.62 0.13 0.20) tongue)))

(define-shader-function fantus-eye-paint (point)
  "A dark glossy eye under a sleepy peach lid, with a felt seam on its edge."
  (let* ((eye (- (fantus-mirrored (fantus-head-point point))
                 (vec3 0.104 1.088 0.196)))
         (height (/ (+ (swizzle eye :y) (* 0.18 (swizzle eye :x))) 0.068))
         (lid (smoothstep (- fantus-eyelid 0.06) (+ fantus-eyelid 0.06)
                          height))
         (seam (* lid (- 1.0 (smoothstep 0.06 0.20
                                          (- height fantus-eyelid)))))
         (iris (mix (vec3 0.012 0.008 0.006) (vec3 0.09 0.05 0.03)
                    (smoothstep 0.2 1.0 (- 0.0 height))))
         (peach (vec3 0.80 0.47 0.20)))
    (mix (mix iris peach lid) (* peach 0.62) seam)))

(define-shader-function fantus-albedo (point)
  "Purple woollen felt with peach mask, ears, chest, and palms; a white
nappy hemmed in green; grey hooves; a yellow dummy."
  (let* ((head (fantus-head-distance point))
         (ears (fantus-ear-distance point))
         (eyes (fantus-eye-distance point))
         (body (fantus-body-distance point))
         (legs (fantus-leg-distance point))
         (hooves (fantus-hoof-distance point))
         (nappy (fantus-nappy-distance point))
         (dummy (fantus-dummy-distance point))
         (fuzz (- (lattice-noise (* point (vec3 61.0 61.0 61.0))) 0.5))
         (grain (- (lattice-noise (* point (vec3 17.0 17.0 17.0))) 0.5))
         (felt (+ 1.0 (+ (* fuzz 0.22) (* grain 0.10))))
         (purple (* (vec3 0.105 0.026 0.275) felt))
         (peach (* (vec3 0.80 0.47 0.20) (+ 1.0 (* fuzz 0.10))))
         ;; Ears: peach inside the cup, purple on the back and the hem.
         (ear (fantus-ear-paddle
               (fantus-ear-point (fantus-head-point point))))
         (ear-inside
           (* (smoothstep -0.024 -0.012 (swizzle ear :z))
              (fantus-inside-ellipse
               (- (vec2 (swizzle ear :x) (swizzle ear :y))
                  (vec2 0.010 0.006))
               (vec2 0.138 0.198) 0.03)))
         (ear-color (mix purple peach ear-inside))
         ;; The tall peach chest patch running down into the nappy.
         (chest (* (smoothstep 0.02 0.07 (swizzle point :z))
                   (fantus-inside-ellipse
                    (- (vec2 (swizzle point :x) (swizzle point :y))
                       (vec2 0.0 0.43))
                    (vec2 0.090 0.150) 0.08)))
         ;; Peach palms and finger pads, purple backs of the hands.
         (mirrored (fantus-mirrored point))
         (palm (* (step 0.22 (swizzle mirrored :x))
                  (- 1.0 (smoothstep 0.150 0.168 (swizzle point :y)))))
         (body-color (mix (mix purple peach chest) peach palm))
         ;; The nappy's green hems at the waist and the leg holes, and a
         ;; green snap on each hip.
         (cloth (* (vec3 0.80 0.80 0.75) (+ 1.0 (* grain 0.05))))
         (green (vec3 0.040 0.300 0.075))
         (waist-hem (smoothstep 0.285 0.300 (swizzle point :y)))
         (leg-hem (- 1.0 (smoothstep 0.018 0.030 legs)))
         (snap (- 1.0 (smoothstep 0.020 0.026
                                  (gnome-length
                                   (- mirrored (vec3 0.190 0.225 0.090))))))
         (nappy-color (mix cloth green (max (max waist-hem leg-hem) snap)))
         (hoof-color (* (vec3 0.30 0.29 0.27) (+ 1.0 (* grain 0.25))))
         (dummy-color (mix (vec3 0.78 0.58 0.06) (vec3 0.85 0.70 0.30)
                           (step (swizzle point :y) 0.425)))
         (head-color (fantus-face-paint point purple peach))
         ;; Ownership: the part whose own distance is least claims the hit.
         (color-1 (mix head-color ear-color (step ears head)))
         (distance-1 (min head ears))
         (color-2 (mix color-1 body-color (step body distance-1)))
         (distance-2 (min distance-1 body))
         (color-3 (mix color-2 purple (step legs distance-2)))
         (distance-3 (min distance-2 legs))
         (color-4 (mix color-3 hoof-color (step hooves distance-3)))
         (distance-4 (min distance-3 hooves))
         (color-5 (mix color-4 nappy-color (step nappy distance-4)))
         (distance-5 (min distance-4 nappy))
         (color-6 (mix color-5 dummy-color (step dummy distance-5)))
         (distance-6 (min distance-5 dummy)))
    (mix color-6 (fantus-eye-paint point) (step eyes distance-6))))

(define-shader-function fantus-gloss (point)
  "How glossy the hit is: the eyes and dummy shine, felt does not."
  (let* ((eyes (fantus-eye-distance point))
         (dummy (fantus-dummy-distance point))
         (rest (min (min (fantus-head-distance point)
                         (fantus-ear-distance point))
                    (min (fantus-body-distance point)
                         (fantus-nappy-distance point)))))
    (max (step eyes rest) (* 0.6 (step dummy rest)))))

(define-shader-function fantus-shaded-color
    (point ray sideways facing sun-direction sun-color ambient coverage)
  "Shade one confirmed Fantus surface hit behind the billboard hit guard."
  (let* ((local-normal (fantus-normal point))
         (normal (+ (* sideways (swizzle local-normal :x))
                    (+ (vec3 0.0 (swizzle local-normal :y) 0.0)
                       (* facing (swizzle local-normal :z)))))
         (albedo (fantus-albedo point))
         (gloss (fantus-gloss point))
         (lambert (dot normal sun-direction))
         (wrapped (max 0.0 (/ (+ lambert 0.45) 1.45)))
         (sky (+ 0.56 (* 0.44 (swizzle normal :y))))
         (occlusion (clamp (+ 0.40 (* 0.60 (smoothstep -0.02 0.36
                                                       (swizzle point :y))))
                           0.40 1.0))
         (view-facing (max 0.0 (dot normal (* ray -1.0))))
         ;; Felt scatters light at grazing angles; a soft sheen does it.
         (sheen (* 0.35 (expt (- 1.0 view-facing) 2.0)))
         (rim (expt (- 1.0 view-facing) 3.5))
         (halfway (normalize (- sun-direction ray)))
         (specular (* (mix 0.03 0.9 gloss)
                      (expt (max 0.0 (dot normal halfway))
                            (mix 12.0 90.0 gloss))))
         (illumination
           (+ (* ambient (* sky occlusion))
              (* sun-color (+ 0.12 (* wrapped 1.10)))))
         (radiance
           (+ (+ (* albedo (* illumination (+ 1.0 (* sheen (- 1.0 gloss)))))
                 (* sun-color (* specular coverage)))
              (* (vec3 0.62 0.55 0.95) (* rim fantus-rim-light)))))
    (vec4 (* radiance coverage) coverage)))

(define-shader-method shader-specification-for
    fantus-sdf-vertex-specification
    ((role (eql :fantus-sdf)) (stage (eql :vertex)))
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
    fantus-sdf-fragment-specification
    ((role (eql :fantus-sdf)) (stage (eql :fragment)))
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
         (local-camera (vec3 (/ (dot toward-eye sideways) fantus-stature)
                             (+ (/ (swizzle toward-eye :y) fantus-stature)
                                0.66)
                             (/ (dot toward-eye facing) fantus-stature)))
         (local-ray (vec3 (dot ray sideways)
                          (swizzle ray :y)
                          (dot ray facing)))
         (figure-radius (/ radius fantus-stature))
         (to-center (- local-camera (vec3 0.0 0.66 0.0)))
         (half-way (- (dot to-center local-ray)))
         (gap (- (dot to-center to-center) (* figure-radius figure-radius)))
         (discriminant (- (* half-way half-way) gap))
         (span (sqrt (max discriminant 0.0)))
         (entry (max (- half-way span) 0.0))
         (exit (+ half-way span))
         (initial-point (+ local-camera (* local-ray entry)))
         (initial-distance (fantus-distance initial-point))
         (initial-state
           (vec3 entry initial-distance
                 (max (- 1.0 (step 0.0008 initial-distance))
                      (step 0.000001 (- entry exit)))))
         (march-state
           (counted-fold
               (march 88.0 state initial-state
                :until (> (swizzle state :z) 0.5))
             (let* ((ray-distance (swizzle state :x))
                    (distance (swizzle state :y))
                    (next-distance
                      (+ ray-distance (max (* distance 0.80) 0.0006)))
                    (next-point (+ local-camera (* local-ray next-distance)))
                    (next-surface-distance (fantus-distance next-point)))
               (vec3 next-distance next-surface-distance
                     (max (- 1.0 (step 0.0008 next-surface-distance))
                          (step 0.000001 (- next-distance exit)))))))
         (travel (swizzle march-state :x))
         (point (+ local-camera (* local-ray travel)))
         (surface-distance (swizzle march-state :y))
         (coverage (* (- 1.0 (step 0.0035 surface-distance))
                      (- 1.0 (step 0.0 (- discriminant))))))
    (set-output color-output (vec4 0.0 0.0 0.0 0.0))
    (when (> coverage 0.0)
      (set-output
       color-output
       (fantus-shaded-color
        point ray sideways facing
        (representation (swizzle sun-vector :xyz))
        (representation (swizzle sun-color-vector :xyz))
        (representation (swizzle ambient-vector :xyz))
        coverage)))))
