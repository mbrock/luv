;;; Fantus's analytic body.  NRK's purple felt toddler -- Fantorangen's
;;; little sibling -- sitting with legs out in front, as toddlers do: a wide
;;; head under a peach mask, hazel eyes under felt lids, a short ringed trunk
;;; with a peach end, a smile cut across broad cheeks, two enormous cupped
;;; ears, a pear-shaped body with a peach belly, long arms, a nappy, a dummy,
;;; and round feet with cloven soles.  Like the cat, a conservative billboard
;;; only launches rays; the visible figure is this field's zero set.  #JGEQ4U

(in-package #:luvcraft.shaders)

(defparameter *fantus-stature* 0.80
  "World cells per Fantus figure unit; the seated puppet is 1.32 units high.")

(defparameter *fantus-head-size* 1.0
  "Fantus's head scale about its own centre; 1 is the reference proportion.")

(defparameter *fantus-ear-size* 1.0
  "Scale of the two big cupped ears, hinged at the side of the head.")

(defparameter *fantus-ear-spread* 0.45
  "How far the ears' cups turn from straight forward toward the sides.")

(defparameter *fantus-eye-oval* 0.78
  "Fantus's eye height over its width; below 1 the eyes are wide ovals.")

(defparameter *fantus-eyelid* 0.74
  "Where the peach upper lid's edge crosses the eye, in eye radii above its
centre; lower is sleepier, and the puppet's own look is near 0.1.")

(defparameter *fantus-smile* 1.0
  "How far the smile is parted in the middle; 0 is a closed line.")

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
  (define-fantus-knob fantus-eye-oval *fantus-eye-oval*
    :figure-proportion 0.55 1.40 0.01
    "Fantus's eye height over its width; 1 is round, below 1 wide.")
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
;;;
;;; Proportions are measured from NRK's 3D Fantus and the licensed plush,
;;; seated, with the ground at 0 and the top of the head at 1.32: the head is
;;; nearly half the figure and a third wider than it is tall, the ears span
;;; twice its width, and the arms reach the ground.

(define-shader-function fantus-mirrored (point)
  "POINT folded onto the right half: Fantus is bilaterally symmetric."
  (vec3 (abs (swizzle point :x)) (swizzle point :y) (swizzle point :z)))

(define-shader-function fantus-head-point (point)
  "The head's frame: POINT scaled about the head centre by FANTUS-HEAD-SIZE.

Every part above the neck is written at the reference proportion and sampled
here, so one knob grows the whole head without the face sliding off it."
  (+ (vec3 0.0 1.00 0.0)
     (/ (- point (vec3 0.0 1.00 0.0)) fantus-head-size)))

(define-shader-function fantus-scaled-ellipsoid-distance (point center radii)
  "A bound on an ellipsoid's distance that stays honest deep inside it.

GNOME-ELLIPSOID-DISTANCE goes to zero at the centre, which is harmless for
a solid but fatal for a hollow: subtracted, it makes a phantom surface where
the cutter's centre is.  Scaling the unit-sphere distance by the smallest
radius is a lower bound everywhere, inside and out."
  (* (- (gnome-length (/ (- point center) radii)) 1.0)
     (min (min (swizzle radii :x) (swizzle radii :y)) (swizzle radii :z))))

(define-shader-function fantus-smooth-intersection
    (first-distance second-distance radius)
  "The intersection with its crease rounded over by RADIUS."
  (- 0.0 (gnome-smooth-union (- 0.0 first-distance) (- 0.0 second-distance)
                             radius)))

(define-shader-function fantus-axis-round-cone-distance
    (point base tip base-radius tip-radius)
  "A round cone from BASE to TIP along any axis, tangent to both caps."
  (let* ((axis (- tip base))
         (height (gnome-length axis))
         (direction (/ axis height))
         (offset (- point base))
         (axial (dot offset direction))
         (radial (gnome-length (- offset (* direction axial))))
         (slope (clamp (/ (- base-radius tip-radius) height) -0.999 0.999))
         (run (sqrt (max (- 1.0 (* slope slope)) 1e-6)))
         (side (+ (* radial (- slope)) (* axial run))))
    (if (< side 0.0)
        (- (gnome-length (vec2 radial axial)) base-radius)
        (if (> side (* run height))
            (- (gnome-length (vec2 radial (- axial height))) tip-radius)
            (- (+ (* radial run) (* axial slope)) base-radius)))))

(define-shader-function fantus-torus-distance (point center major minor)
  "A ring of radius MAJOR and tube MINOR lying in the XY plane, facing +Z."
  (let* ((offset (- point center))
         (radial (- (gnome-length (vec2 (swizzle offset :x)
                                        (swizzle offset :y)))
                    major)))
    (- (gnome-length (vec2 radial (swizzle offset :z))) minor)))

(define-shader-function fantus-inside-ellipse (offset radii softness)
  "1 inside the ellipse of RADII about the origin of OFFSET, 0 outside."
  (- 1.0 (smoothstep (- 1.0 softness) (+ 1.0 softness)
                     (gnome-length (/ offset radii)))))

;;; ---------------------------------------------------------------------
;;; The head

(define-shader-function fantus-skull-distance (head)
  "The wide head and the broad cheeks the smile is cut across, in head frame.

The head is an ellipsoid a third wider than it is tall; the cheeks fill the
lower face forward and out so the face reads as a broad plane under the
mask rather than the front of a ball."
  (gnome-smooth-union
   (gnome-ellipsoid-distance head (vec3 0.0 1.005 -0.02)
                             (vec3 0.375 0.315 0.305))
   (gnome-ellipsoid-distance head (vec3 0.0 0.870 0.060)
                             (vec3 0.272 0.190 0.240))
   0.09))

(define-shader-function fantus-trunk-base () (vec3 0.0 0.925 0.150))
(define-shader-function fantus-trunk-tip () (vec3 0.0 0.872 0.408))

(define-shader-function fantus-trunk-axial (head)
  "How far along the trunk, from its root inside the face, HEAD lies."
  (dot (- head (fantus-trunk-base))
       (normalize (- (fantus-trunk-tip) (fantus-trunk-base)))))

(define-shader-function fantus-trunk-radial (head)
  "How far HEAD lies from the trunk's axis."
  (let* ((offset (- head (fantus-trunk-base)))
         (direction (normalize (- (fantus-trunk-tip) (fantus-trunk-base)))))
    (gnome-length (- offset (* direction (dot offset direction))))))

(define-shader-function fantus-trunk-distance (head)
  "The short trunk: a tapered, ringed felt tube with a flat end.

It leaves the face just below and between the eyes and points forward and a
little down.  Shallow rings are rippled into its radius, as the felt
gathers on the plush; the end is cut flat and pressed with a dimple."
  (let* ((axial (fantus-trunk-axial head))
         (ring (* 0.0045 (sin (* axial 110.0))))
         (tube (- (fantus-axis-round-cone-distance
                   head (fantus-trunk-base) (fantus-trunk-tip) 0.112 0.068)
                  ring))
         (flat (- axial 0.255))
         (dimple (gnome-ellipsoid-distance
                  head (vec3 0.0 0.874 0.416) (vec3 0.030 0.022 0.016))))
    (max (* (max tube flat) 0.92) (- dimple))))

(define-shader-function fantus-smile-slot (head)
  "The cavity of the smile: a curved slot cut across the cheeks.

The slot follows a parabola under the trunk with its corners raised, and is
open widest in the middle by FANTUS-SMILE.  The parabola distance is the
vertical miss divided by its slope's secant, a lower bound near the curve."
  (let* ((x (swizzle head :x))
         (across (/ x 0.205))
         (curve (+ 0.792 (* 1.55 (* x x))))
         (secant (sqrt (+ 1.0 (* (* 3.1 x) (* 3.1 x)))))
         (opening (+ 0.0045
                     (* (* 0.010 fantus-smile)
                        (max (- 1.0 (* across across)) 0.0))))
         (beyond (max (- (abs x) 0.205) 0.0))
         (slot (- (gnome-length (vec2 beyond
                                      (/ (- (swizzle head :y) curve) secant)))
                  opening)))
    (max slot (- 0.17 (swizzle head :z)))))

(define-shader-function fantus-eye-center () (vec3 0.188 1.050 0.205))

(define-shader-function fantus-eye-radii ()
  "The eyeball's half-extents: wide ovals, FANTUS-EYE-OVAL as tall as wide."
  (vec3 0.086 (* 0.086 fantus-eye-oval) 0.076))

(define-shader-function fantus-eye-distance (point)
  "Two large wide oval eyeballs set into the mask, protruding a third."
  (* (gnome-ellipsoid-distance (fantus-mirrored (fantus-head-point point))
                               (fantus-eye-center) (fantus-eye-radii))
     fantus-head-size))

(define-shader-function fantus-lid-distance (point)
  "The upper lids: a felt shell over each eye, cut along the lid line.

FANTUS-EYELID is the height of the cut in eye radii.  The line rises a
little toward the outer corner -- tipped the other way it reads as sorrow --
and the shell is joined to the face so the lid is skin, not a visor."
  (let* ((eye (- (fantus-mirrored (fantus-head-point point))
                 (fantus-eye-center)))
         (shell (gnome-ellipsoid-distance
                 eye (vec3 0.0 0.0 0.0)
                 (+ (fantus-eye-radii) (vec3 0.0055 0.0055 0.0055))))
         (line (- (- (swizzle eye :y) (* 0.05 (swizzle eye :x)))
                  (* fantus-eyelid (swizzle (fantus-eye-radii) :y)))))
    (* (fantus-smooth-intersection shell (- line) 0.012) fantus-head-size)))

(define-shader-function fantus-head-distance (point)
  "Skull, cheeks, trunk, and the short neck, with the smile cut in."
  (let* ((head (fantus-head-point point))
         (face (gnome-smooth-union (fantus-skull-distance head)
                                   (fantus-trunk-distance head)
                                   0.045))
         (smiling (max face (- (fantus-smile-slot head))))
         (neck (cat-capsule-distance
                point (vec3 0.0 0.60 -0.02) (vec3 0.0 0.80 -0.03) 0.105)))
    (gnome-smooth-union (* smiling fantus-head-size) neck 0.06)))

;;; ---------------------------------------------------------------------
;;; The ears

(define-shader-function fantus-ear-point (point)
  "One ear's own frame, on the right side of the folded head.

The ear is a flattened ellipsoid whose cup faces local +Z.  It is hinged on
the side of the head, turned outward about the vertical by
FANTUS-EAR-SPREAD, and rolled so its broad top leans away from the head by 34 degrees, as on the plush."
  (let* ((offset (- (fantus-mirrored point)
                    (vec3 (+ 0.290 (* 0.200 fantus-ear-size)) 1.045 -0.06)))
         (yaw-cosine (cos fantus-ear-spread))
         (yaw-sine (sin fantus-ear-spread))
         (across (- (* (swizzle offset :x) yaw-cosine)
                    (* (swizzle offset :z) yaw-sine)))
         (out (+ (* (swizzle offset :x) yaw-sine)
                 (* (swizzle offset :z) yaw-cosine)))
         (roll-cosine 0.82903757)
         (roll-sine 0.55919290))
    (/ (vec3 (- (* across roll-cosine) (* (swizzle offset :y) roll-sine))
             (+ (* across roll-sine) (* (swizzle offset :y) roll-cosine))
             out)
       fantus-ear-size)))

(define-shader-function fantus-ear-paddle (ear)
  "Widen the ear toward its top so it is a paddle, not a disc.

The felt ear is narrow where it is sewn to the head and broad where it
flops.  Dividing across by a width that grows with height does that; the
0.80 on the distance pays for the stretch."
  (vec3 (/ (swizzle ear :x)
           (+ 0.84 (* 0.26 (smoothstep -0.28 0.22 (swizzle ear :y)))))
        (swizzle ear :y)
        (swizzle ear :z)))

(define-shader-function fantus-ear-distance (point)
  "Two big ears: flattened ellipsoids with a cup pressed into the front.

The cup is the subtraction of a smaller, offset ellipsoid; the purple rim it
leaves is the felt hem.  Both are scaled back by the ear and head sizes so
the warped field remains a lower bound."
  (let* ((ear (fantus-ear-paddle
               (fantus-ear-point (fantus-head-point point))))
         (outer (gnome-ellipsoid-distance
                 ear (vec3 0.0 0.0 0.0) (vec3 0.172 0.292 0.042)))
         (cup (fantus-scaled-ellipsoid-distance
               ear (vec3 0.008 0.006 0.058) (vec3 0.138 0.254 0.036))))
    (* (* (max outer (- cup)) 0.80) (* fantus-ear-size fantus-head-size))))

;;; ---------------------------------------------------------------------
;;; The body

(define-shader-function fantus-body-distance (point)
  "The pear-shaped torso, the long thin arms, and the three-fingered hands.

The arms hang from narrow shoulders and the hands rest on the ground beside
the nappy, fingers forward, as the seated plush holds them."
  (let* ((mirrored (fantus-mirrored point))
         (torso (* (gnome-round-cone-distance
                    (vec3 (swizzle point :x) (swizzle point :y)
                          (/ (+ (swizzle point :z) 0.01) 0.86))
                    0.30 0.64 0.235 0.140)
                   0.86))
         (upper-arm (cat-capsule-distance
                     mirrored (vec3 0.165 0.600 -0.02) (vec3 0.330 0.390 0.0)
                     0.054))
         (forearm (cat-capsule-distance
                   mirrored (vec3 0.330 0.390 0.0) (vec3 0.432 0.170 0.055)
                   0.049))
         (palm (gnome-ellipsoid-distance
                mirrored (vec3 0.455 0.090 0.095) (vec3 0.064 0.048 0.080)))
         (inner-finger (cat-capsule-distance
                        mirrored (vec3 0.425 0.070 0.135)
                        (vec3 0.405 0.034 0.200) 0.023))
         (middle-finger (cat-capsule-distance
                         mirrored (vec3 0.460 0.070 0.145)
                         (vec3 0.468 0.034 0.215) 0.023))
         (outer-finger (cat-capsule-distance
                        mirrored (vec3 0.490 0.072 0.125)
                        (vec3 0.522 0.036 0.180) 0.022))
         (hand (gnome-smooth-union
                (gnome-smooth-union
                 (gnome-smooth-union palm inner-finger 0.022)
                 middle-finger 0.022)
                outer-finger 0.022))
         (arm (gnome-smooth-union
               (gnome-smooth-union upper-arm forearm 0.03) hand 0.035)))
    (gnome-smooth-union torso arm 0.05)))

(define-shader-function fantus-leg-distance (point)
  "Two short legs splayed forward and apart from the nappy."
  (cat-capsule-distance
   (fantus-mirrored point) (vec3 0.135 0.140 0.03) (vec3 0.245 0.120 0.300)
   0.084))

(define-shader-function fantus-sole-normal ()
  (normalize (vec3 0.42 0.08 1.0)))

(define-shader-function fantus-foot-distance (point)
  "Big round feet, their flat cloven soles turned toward the viewer."
  (let* ((foot (- (fantus-mirrored point) (vec3 0.272 0.118 0.360))))
    (max (gnome-ellipsoid-distance foot (vec3 0.0 0.0 0.0)
                                   (vec3 0.108 0.112 0.100))
         (- (dot foot (fantus-sole-normal)) 0.072))))

(define-shader-function fantus-nappy-distance (point)
  "The white cloth nappy, a soft rounded bundle the legs come out of."
  (gnome-ellipsoid-distance
   point (vec3 0.0 0.205 -0.005) (vec3 0.272 0.205 0.235)))

(define-shader-function fantus-dummy-distance (point)
  "The yellow dummy hung on the chest: ring, shield, and teat."
  (let* ((ring (fantus-torus-distance
                point (vec3 0.0 0.540 0.165) 0.040 0.010))
         (shield (gnome-ellipsoid-distance
                  point (vec3 0.0 0.482 0.168) (vec3 0.060 0.031 0.014)))
         (teat (gnome-ellipsoid-distance
                point (vec3 0.0 0.434 0.176) (vec3 0.020 0.040 0.019))))
    (min (min ring shield) teat)))

(define-shader-function fantus-distance (point)
  "The complete seated Fantus, with felt joins but distinct silhouette parts."
  (let* ((head (fantus-head-distance point))
         (ears (fantus-ear-distance point))
         (eyes (fantus-eye-distance point))
         (lids (fantus-lid-distance point))
         (body (fantus-body-distance point))
         (legs (fantus-leg-distance point))
         (feet (fantus-foot-distance point))
         (nappy (fantus-nappy-distance point))
         (dummy (fantus-dummy-distance point)))
    (min
     (gnome-smooth-union
      (gnome-smooth-union
       (gnome-smooth-union
        (gnome-smooth-union (gnome-smooth-union head lids 0.022) ears 0.035)
        body 0.04)
       nappy 0.03)
      (gnome-smooth-union legs feet 0.03)
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

(define-shader-function fantus-face-paint (point purple peach)
  "The head's felt: the peach mask, the trunk's peach end, and the smile.

The mask is a broad band across the upper face with a lobe around each eye;
the purple of the trunk rises into it from below as an arch.  Marks are
drawn in the head frame, projected along Z, so they hold at any head size."
  (let* ((head (fantus-head-point point))
         (mirrored (fantus-mirrored head))
         (face-xy (vec2 (swizzle mirrored :x) (swizzle mirrored :y)))
         (front (smoothstep 0.045 0.060 (swizzle head :z)))
         (band (fantus-inside-ellipse (- face-xy (vec2 0.0 1.100))
                                      (vec2 0.322 0.160) 0.025))
         (lobe (fantus-inside-ellipse (- face-xy (vec2 0.188 1.044))
                                      (+ (vec2 0.066 0.059)
                                         (swizzle (fantus-eye-radii) :xy))
                                      0.03))
         (arch (fantus-inside-ellipse (- face-xy (vec2 0.0 0.935))
                                      (vec2 0.108 0.178) 0.04))
         (mask (* front (* (max band lobe) (- 1.0 arch))))
         ;; A faint shadow line where the mask's felt meets the purple.
         (seam (* 4.0 (* mask (- 1.0 mask))))
         (masked (* (mix purple peach mask) (- 1.0 (* 0.30 seam))))
         ;; The trunk's flat end: a peach oval inset in a purple hem.
         (tip (* (smoothstep 0.247 0.253 (fantus-trunk-axial head))
                 (- 1.0 (smoothstep 0.046 0.052
                                    (fantus-trunk-radial head)))))
         (tipped (mix masked (* peach 0.92) tip))
         ;; The smile's dark mouth: anywhere inside or at the cut.
         (mouth (- 1.0 (smoothstep 0.0 0.006 (fantus-smile-slot head)))))
    (mix tipped (vec3 0.050 0.012 0.025) mouth)))

(define-shader-function fantus-eye-paint (point)
  "White sclera, an olive-hazel iris, a large round pupil, and a catchlight.

The catchlight is painted high and to Fantus's left of each pupil, where
one studio key light would put it in both eyes -- the eyes are folded onto
one side, so the light's side is unfolded by the sign of X -- and keeps the
eyes alive under any sun."
  (let* ((head (fantus-head-point point))
         ;; Directions are taken on the unit sphere the oval eye is stretched
         ;; from, so iris and pupil stretch with it.
         (eye (normalize (/ (- (fantus-mirrored head) (fantus-eye-center))
                            (fantus-eye-radii))))
         (side (- (* 2.0 (step 0.0 (swizzle head :x))) 1.0))
         (gaze (normalize (vec3 0.10 -0.04 1.0)))
         (away (gnome-length (- eye gaze)))
         (iris (- 1.0 (smoothstep 0.53 0.57 away)))
         (pupil (- 1.0 (smoothstep 0.27 0.31 away)))
         (ring (* iris (smoothstep 0.42 0.55 away)))
         (sclera (vec3 0.82 0.80 0.74))
         (hazel (mix (vec3 0.30 0.27 0.035) (vec3 0.12 0.10 0.012) ring))
         (glint (- 1.0 (smoothstep 0.075 0.095
                                   (gnome-length
                                    (- eye (normalize
                                            (vec3 (* 0.26 side) 0.20 0.95))))))))
    (mix (mix (mix sclera hazel iris) (vec3 0.008 0.007 0.006) pupil)
         (vec3 1.6 1.6 1.5) glint)))

(define-shader-function fantus-sole-paint (point purple)
  "A cream cloven print on the flat of each foot: a heart parted at the top."
  (let* ((foot (- (fantus-mirrored point) (vec3 0.272 0.118 0.360)))
         (on-sole (smoothstep 0.062 0.070 (dot foot (fantus-sole-normal))))
         (across (* (- (swizzle foot :x) (* 0.42 (swizzle foot :z)))
                    0.92211015))
         (up (swizzle foot :y))
         ;; Each toe is an ellipse leaning in toward the other at its foot,
         ;; so the pair closes to a point below and parts in a notch above.
         (right-toe (vec2 (- (* (- across 0.024) 0.921) (* up 0.389))
                          (+ (* up 0.921) (* (- across 0.024) 0.389))))
         (left-toe (vec2 (+ (* (+ across 0.024) 0.921) (* up 0.389))
                         (- (* up 0.921) (* (+ across 0.024) 0.389))))
         (lobes (max (fantus-inside-ellipse right-toe (vec2 0.030 0.056) 0.10)
                     (fantus-inside-ellipse left-toe (vec2 0.030 0.056) 0.10))))
    (mix purple (vec3 0.62 0.56 0.44) (* on-sole lobes))))

(define-shader-function fantus-albedo (point)
  "Purple felt with peach mask, ear cups, belly, and palms; a white nappy
with green hems, flaps, and buttons; cream soles; a yellow dummy."
  (let* ((head (fantus-head-distance point))
         (ears (fantus-ear-distance point))
         (eyes (fantus-eye-distance point))
         (lids (fantus-lid-distance point))
         (body (fantus-body-distance point))
         (legs (fantus-leg-distance point))
         (feet (fantus-foot-distance point))
         (nappy (fantus-nappy-distance point))
         (dummy (fantus-dummy-distance point))
         (fuzz (- (lattice-noise (* point (vec3 61.0 61.0 61.0))) 0.5))
         (grain (- (lattice-noise (* point (vec3 17.0 17.0 17.0))) 0.5))
         (felt (+ 1.0 (+ (* fuzz 0.20) (* grain 0.10))))
         (purple (* (vec3 0.215 0.034 0.330) felt))
         (peach (* (vec3 0.84 0.52 0.22) (+ 1.0 (* fuzz 0.10))))
         (mirrored (fantus-mirrored point))
         ;; Ears: peach inside the cup, purple on the back and the hem.
         (ear (fantus-ear-paddle
               (fantus-ear-point (fantus-head-point point))))
         (ear-inside
           (* (smoothstep -0.026 -0.012 (swizzle ear :z))
              (fantus-inside-ellipse
               (- (vec2 (swizzle ear :x) (swizzle ear :y))
                  (vec2 0.008 0.006))
               (vec2 0.136 0.252) 0.025)))
         (ear-color (mix purple peach ear-inside))
         ;; The tall peach belly running down into the nappy.
         (belly (* (smoothstep 0.03 0.09 (swizzle point :z))
                   (fantus-inside-ellipse
                    (- (vec2 (swizzle point :x) (swizzle point :y))
                       (vec2 0.0 0.455))
                    (vec2 0.150 0.205) 0.06)))
         ;; Peach palms and finger pads under purple backs.
         (palm (* (step 0.37 (swizzle mirrored :x))
                  (- 1.0 (smoothstep 0.060 0.080 (swizzle point :y)))))
         (body-color (mix (mix purple peach belly) peach palm))
         ;; The nappy: green hems where it meets the body and the legs, and
         ;; on each front hip a green-edged tab fastened by a green button.
         (cloth (* (vec3 0.82 0.82 0.78) (+ 1.0 (* grain 0.05))))
         (green (vec3 0.085 0.36 0.030))
         (waist-hem (- 1.0 (smoothstep 0.014 0.024 (- body nappy))))
         (leg-hem (- 1.0 (smoothstep 0.012 0.022 (- legs nappy))))
         (hip (- (vec2 (swizzle mirrored :x) (swizzle point :y))
                 (vec2 0.188 0.238)))
         (tab-turn (vec2 (+ (* (swizzle hip :x) 0.906) (* (swizzle hip :y) 0.423))
                         (- (* (swizzle hip :y) 0.906) (* (swizzle hip :x) 0.423))))
         (tab-reach (gnome-length (/ tab-turn (vec2 0.082 0.052))))
         (front-face (smoothstep 0.10 0.14 (swizzle point :z)))
         (tab-edge (* front-face
                      (- 1.0 (smoothstep 0.10 0.22 (abs (- tab-reach 1.0))))))
         (button (* front-face
                    (- 1.0 (smoothstep 0.022 0.027
                                       (gnome-length
                                        (- hip (vec2 -0.020 0.008)))))))
         (nappy-color (mix cloth green
                           (max (max waist-hem leg-hem)
                                (max tab-edge button))))
         (foot-color (fantus-sole-paint point purple))
         (dummy-color (mix (vec3 0.78 0.58 0.06) (vec3 0.85 0.70 0.30)
                           (step (swizzle point :y) 0.462)))
         (head-color (fantus-face-paint point purple peach))
         ;; Ownership: the part whose own distance is least claims the hit.
         (color-1 (mix head-color ear-color (step ears head)))
         (distance-1 (min head ears))
         (color-2 (mix color-1 body-color (step body distance-1)))
         (distance-2 (min distance-1 body))
         (color-3 (mix color-2 purple (step legs distance-2)))
         (distance-3 (min distance-2 legs))
         (color-4 (mix color-3 foot-color (step feet distance-3)))
         (distance-4 (min distance-3 feet))
         (color-5 (mix color-4 nappy-color (step nappy distance-4)))
         (distance-5 (min distance-4 nappy))
         (color-6 (mix color-5 dummy-color (step dummy distance-5)))
         (distance-6 (min distance-5 dummy))
         (color-7 (mix color-6 (* peach 0.96) (step lids distance-6)))
         (distance-7 (min distance-6 lids)))
    (mix color-7 (fantus-eye-paint point) (step eyes distance-7))))

(define-shader-function fantus-gloss (point)
  "How glossy the hit is: the eyes and dummy shine, felt does not."
  (let* ((eyes (fantus-eye-distance point))
         (dummy (fantus-dummy-distance point))
         (rest (min (min (fantus-head-distance point)
                         (fantus-lid-distance point))
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
