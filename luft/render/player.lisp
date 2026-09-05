(in-package #:luft.render)

;;; The player is a ray-marched character drawn into the scene pass. Its body,
;;; gait, surface detail, and tracing program live below this GPU interface.
;;; The frame supplies camera/pose uniforms and the sun's shadow map; this
;;; component owns only its program. NIL in its place omits the character.

(defclass player-drawing (pipeline-scene-drawing) ())

(defun make-player-drawing (device target-formats sample-count)
  (make-pipeline-scene-drawing
   'player-drawing device :label "luft player sdf"
   :vertex (shaders:player-sdf-vertex-specification)
   :fragment (shaders:player-sdf-fragment-specification)
   :targets (loop for format in target-formats
                  for first = t then nil
                  collect `(:format ,format
                            ,@(when first '(:blend :premultiplied-alpha))))
   :sample-count sample-count :depth-compare :less :vertex-count 6))

(defmethod make-scene-drawing-binding
    ((drawing player-drawing) device camera-buffer shadow-view shadow-sampler)
  (make-program-binding (scene-drawing-program drawing) device
                        :camera-state camera-buffer
                        :shadow-map shadow-view :shadow-sampler shadow-sampler))

(in-package #:luft.render.shaders)

;;; Shape and motion, then surface appearance, then the vertex/fragment entry
;;; points which project the character and trace that shape through each pixel.

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

(define-shader-function player-wizard-orb-distance (point)
  "The hermit's little captured firework: a true SDF sphere and four sparks.

Keeping the sparks in the same field means they clip and shade with the body
rather than becoming transparent billboard confetti."
  (min (player-sdf-sphere point (vec3 0.640 0.195 3.34) 0.235)
       (min (player-sdf-sphere point (vec3 0.640 0.195 3.69) 0.052)
            (min (player-sdf-sphere point (vec3 0.965 0.195 3.34) 0.044)
                 (min (player-sdf-sphere point
                                         (vec3 0.360 0.195 3.34) 0.044)
                      (player-sdf-sphere point
                                         (vec3 0.640 0.195 3.00) 0.040))))))

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
         (centered (- translated (vec3 0.0 0.0 2.10))))
    (+ (vec3 0.0 0.0 2.10)
       (vec3 (+ (* cosine (swizzle centered :x))
                (* sine (swizzle centered :z)))
             (swizzle centered :y)
             (- (* cosine (swizzle centered :z))
                (* sine (swizzle centered :x)))))))

;;; The traveler's parts.
;;;
;;; He is the sanctuary's own hermit: hat, beard, mantle, staff, and the
;;; copper lozenge the whole site is inlaid with, worn as embroidery.  The
;;; vocabulary is deliberately the one luvcraft's garden gnome already uses
;;; (LUVCRAFT/AGENT/SHADERS.LISP) -- a round cone for anything conical, a
;;; leaning hat frame, a nose that is a frank sphere -- rotated into LUFT's
;;; Z-up frame and given the walking legs that fellow does not have.

(define-shader-function player-sdf-round-cone
    (point base-height top-height base-radius top-radius)
  "A cone capped by a sphere at each end, standing on the Z axis.

The lateral surface is tangent to both caps, so the silhouette has no crease
where the cone meets its rounding."
  (let* ((height (max (- top-height base-height) 1e-4))
         (radial (player-sdf-length (vec2 (swizzle point :x)
                                          (swizzle point :y))))
         (axial (- (swizzle point :z) base-height))
         (slope (clamp (/ (- base-radius top-radius) height) -0.999 0.999))
         (run (sqrt (max (- 1.0 (* slope slope)) 1e-6)))
         (side (+ (* radial (- slope)) (* axial run))))
    (if (< side 0.0)
        (- (player-sdf-length (vec2 radial axial)) base-radius)
        (if (> side (* run height))
            (- (player-sdf-length (vec2 radial (- axial height))) top-radius)
            (- (+ (* radial run) (* axial slope)) base-radius)))))

(define-shader-function player-sdf-lozenge (point center radii)
  "An octahedron scaled by RADII: the site's diamond, as a solid.

The exact octahedron distance is scaled back by the smallest half-extent so
the warped field stays a lower bound and the march cannot overstep it."
  (let* ((folded (abs (/ (- point center) radii)))
         (extent (+ (+ (swizzle folded :x) (swizzle folded :y))
                    (swizzle folded :z)))
         (smallest (min (min (swizzle radii :x) (swizzle radii :y))
                        (swizzle radii :z))))
    (* (* (- extent 1.0) 0.57735026) smallest)))

(define-shader-function player-hat-point (point)
  "The hat's own frame: a domain warp that leans the cone's tip forward.

A straight cone is a traffic cone.  The lean is what makes it a hat someone
is wearing, and it costs one smoothstep."
  (let* ((rise (smoothstep 2.26 3.34 (swizzle point :z)))
         (lean (* rise rise)))
    (vec3 (- (swizzle point :x) (* 0.075 lean))
          (- (swizzle point :y) (* 0.235 lean))
          (swizzle point :z))))

(define-shader-function player-brim-point (point)
  "The brim's frame: a quadratic droop that curls its edge down.

A flat disc reads as a plate balanced on his head.  Lifting the sample
with the square of its radius is enough to make the felt sag under its own
weight, and the extra gradient it costs is paid back by the 0.9 below."
  (let* ((radial (+ (* (swizzle point :x) (swizzle point :x))
                    (* (swizzle point :y) (swizzle point :y)))))
    (vec3 (swizzle point :x)
          (swizzle point :y)
          (+ (swizzle point :z) (* 0.30 radial)))))

(define-shader-function player-hat-distance (point)
  "The tall leaning cone and the drooping brim it sits on.

The brim is wide enough to keep his eyes in shadow from the sanctuary's
high camera: what the player sees under it is a nose and a beard, which is
the whole of what this fellow is willing to say about himself."
  (let* ((cone (player-sdf-round-cone (player-hat-point point)
                                      2.24 3.34 0.435 0.022))
         (brim (* (player-sdf-ellipsoid (player-brim-point point)
                                        (vec3 0.0 0.015 2.30)
                                        (vec3 0.735 0.735 0.075))
                  0.9)))
    (player-sdf-smooth-union cone brim 0.05)))

(define-shader-function player-hatband-distance (point)
  "The copper band around the crown, standing a little proud of the felt."
  (player-sdf-round-cone (player-hat-point point) 2.32 2.47 0.412 0.372))

(define-shader-function player-head-distance (point)
  "Head and nose.  Almost all of the head is hat, brim, or beard."
  (let* ((head (player-sdf-ellipsoid point (vec3 0.0 -0.01 2.09)
                                     (vec3 0.295 0.285 0.295)))
         (nose (player-sdf-sphere point (vec3 0.0 0.345 2.015) 0.17)))
    (player-sdf-smooth-union head nose 0.045)))

(define-shader-function player-beard-distance (point)
  "A broad beard with a blunt point: the mass that gives him his gravity.

Its top edge is the lower bound of his face, so it sits just under the
nose, and its point falls to the middle of his chest."
  (let* ((body (player-sdf-ellipsoid point (vec3 0.0 0.185 1.815)
                                     (vec3 0.335 0.26 0.545)))
         ;; Cut level under the moustache and rounded off there.  An
         ;; uncut ellipsoid is widest at its middle, which puts a beard's
         ;; broadest point at his chest and its narrowest at his cheeks --
         ;; a cloud rather than a beard.
         (mass (player-sdf-smooth-cut
                body (- (swizzle point :z) 1.955) 0.13))
         (moustache (player-sdf-ellipsoid point (vec3 0.0 0.305 1.90)
                                          (vec3 0.235 0.155 0.072)))
         (hair (player-sdf-smooth-union mass moustache 0.055)))
    (- hair (player-beard-comb point))))

(define-shader-function player-sdf-smooth-cut (solid limit width)
  "SOLID intersected with a half space, with a rounded arris at the join."
  (- (player-sdf-smooth-union (- solid) (- limit) width)))

(define-shader-function player-sixfold-cosine (point)
  "COS 6*theta around the figure's own axis, without an inverse tangent.

The horizontal direction is squared three times over; what comes out is
exactly the sixth harmonic, so a flute stays on the cloth when the figure
turns instead of sliding around it like a projected stripe."
  (let* ((x (swizzle point :x))
         (y (swizzle point :y))
         (radial (max (sqrt (+ (* x x) (* y y))) 1e-4))
         (cosine (/ x radial))
         (sine (/ y radial))
         (cosine-2 (- (* cosine cosine) (* sine sine)))
         (sine-2 (* 2.0 (* cosine sine)))
         (cosine-4 (- (* cosine-2 cosine-2) (* sine-2 sine-2)))
         (sine-4 (* 2.0 (* cosine-2 sine-2))))
    (- (* cosine-4 cosine-2) (* sine-4 sine-2))))

(define-shader-function player-linen-fold (point)
  "Six vertical folds in hanging linen, deepening toward the hem.

Their depth is the difference between a robe and a lampshade, so it is the
number to reach for first.  A twelfth harmonic rides on the sixth at a
third of its depth: the linen then has a coarse drape and a fine crease
rather than one honest sine wave."
  (let* ((six (player-sixfold-cosine point))
         (twelve (- (* 2.0 (* six six)) 1.0))
         (depth (smoothstep 1.88 0.28 (swizzle point :z))))
    (* depth (+ (* 0.042 six) (* 0.013 twelve)))))

(define-shader-function player-beard-comb (point)
  "Strands combed down a beard, and the part a man leaves in the middle.

Measured about the same axis as the robe's flutes, the twelfth harmonic
crosses the beard's front about three times, which is a beard.  Any more
and it is corduroy."
  (let* ((six (player-sixfold-cosine point))
         (twelve (- (* 2.0 (* six six)) 1.0))
         (twenty-four (- (* 2.0 (* twelve twelve)) 1.0))
         (strands (* (+ (* 0.011 twelve) (* 0.006 twenty-four))
                     (smoothstep 1.94 1.16 (swizzle point :z))))
         ;; A shallow part.  Carved any deeper it stopped being a parting
         ;; and started being a window onto the robe underneath.
         (part (* 0.007 (- 1.0 (smoothstep 0.0 0.09
                                           (abs (swizzle point :x)))))))
    (- strands part)))

(define-shader-function player-robe-point (point gait)
  "The robe's own frame: hung from the shoulders, resting on the deck.

Above the waist the cloth rides with the body.  At the hem it stays where
gravity left it, so the walk reads as a hem that swings and trails rather
than as a garment sliding up and down the figure.  The pelvis lift is
subtracted back out with the same weight, which is what keeps a
floor-length robe on the floor while its wearer bobs over his stance foot."
  (let* ((phase (fract (player-step-coordinate gait)))
         (height (swizzle point :z))
         (drape (smoothstep 1.88 0.30 height))
         (carried (- 1.0 drape))
         (lift (* (player-pelvis-lift gait) carried))
         (side-shift
           (* (player-walk-pose-channel phase 0.0 -0.020 0.030 0.018 0.0)
              carried))
         (forward-set
           (* (player-walk-pose-channel phase 0.0 -0.018 0.012 0.006 0.0)
              carried))
         (swing (* (* 0.105 (sin gait)) drape))
         (trail (* drape (+ 0.125 (* 0.05 (cos gait))))))
    (vec3 (- (- (swizzle point :x) side-shift) swing)
          (+ (- (swizzle point :y) forward-set) trail)
          (- height lift))))

(define-shader-function player-robe-distance (point gait)
  "A floor-length linen robe: fitted at the chest, flaring to a swept hem.

The bell is a round cone, cut level just above the deck with a rounded
arris so the cloth pools rather than ending in a pastry edge, and fluted
by PLAYER-LINEN-FOLD.  The 0.78 pays for the warps: the fold, the sway and
the drape each steepen the field a little, and the march must never be
told the surface is further away than it is."
  (let* ((framed (player-robe-point point gait))
         (bell (player-sdf-round-cone framed 0.13 2.00 0.625 0.365))
         (chest (player-sdf-ellipsoid framed (vec3 0.0 0.0 1.86)
                                      (vec3 0.395 0.305 0.335)))
         (cloth (player-sdf-smooth-union bell chest 0.17))
         (fluted (- cloth (player-linen-fold framed))))
    (* (player-sdf-smooth-cut fluted (- 0.115 (swizzle framed :z)) 0.10)
       0.78)))

(define-shader-function player-mantle-point (point gait)
  "The mantle's frame: a shoulder cape whose skirt trails behind the walk."
  (let* ((drape (smoothstep 2.20 1.48 (swizzle point :z))))
    (vec3 (- (swizzle point :x) (* (* 0.065 (sin gait)) drape))
          (+ (swizzle point :y) (* 0.335 drape))
          (swizzle point :z))))

(define-shader-function player-mantle-distance (point gait)
  "A short traveling cape over the robe, open at the throat.

Its hem is the edge the sanctuary's own camera sees most, so the lozenge
braid the walls are inlaid with is embroidered along it."
  (let* ((framed (player-mantle-point point gait))
         (cape (player-sdf-round-cone framed 1.54 2.10 0.535 0.315))
         (fluted (- cape (* 0.55 (player-linen-fold framed)))))
    (* (player-sdf-smooth-cut fluted (- 1.575 (swizzle framed :z)) 0.075)
       0.82)))

(define-shader-function player-cowl-distance (point)
  "The mantle's rolled collar, gathered under the beard."
  (player-sdf-ellipsoid point (vec3 0.0 -0.03 2.135)
                        (vec3 0.395 0.315 0.125)))

(define-shader-function player-one-boot-distance
    (point gait direction side parity)
  "One boot on the contact solver's own foot, with nothing above the ankle.

The legs themselves are gone: under a floor-length robe they were a two-bone
chain nobody could see, paid for at every march step.  What survives is the
part that touches the world, so the stance foot still stands still."
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
         (foot-pitch (player-foot-rocker-pitch phase))
         (pitch-cosine (cos foot-pitch))
         (pitch-sine (sin foot-pitch))
         (baseline-center
           (vec3 (* side 0.185) (+ swing 0.09) (+ 0.135 lift)))
         ;; During stance the boot pivots around a stationary heel, lies flat
         ;; through weight acceptance, then pivots around a stationary toe.
         ;; During swing its centre follows the authored clearance arc.
         (stance-time (clamp (* phase 2.0) 0.0 1.0))
         (pivot
           (if (< phase 0.5)
               (if (< stance-time 0.55)
                   (vec3 (* side 0.185) (- (+ swing 0.09) 0.20) 0.0)
                   (vec3 (* side 0.185) (+ (+ swing 0.09) 0.21) 0.0))
               baseline-center))
         (center-offset (- baseline-center pivot))
         (boot-center
           (+ pivot
              (vec3 (swizzle center-offset :x)
                    (- (* pitch-cosine (swizzle center-offset :y))
                       (* pitch-sine (swizzle center-offset :z)))
                    (+ (* pitch-sine (swizzle center-offset :y))
                       (* pitch-cosine (swizzle center-offset :z))))))
         (boot-offset (- point boot-center))
         (boot-point
           (+ boot-center
              (vec3 (swizzle boot-offset :x)
                    (+ (* pitch-cosine (swizzle boot-offset :y))
                       (* pitch-sine (swizzle boot-offset :z)))
                    (- (* pitch-cosine (swizzle boot-offset :z))
                       (* pitch-sine (swizzle boot-offset :y)))))))
    (player-sdf-ellipsoid boot-point boot-center
                          (vec3 0.155 0.235 0.135))))

(define-shader-function player-boot-distance (point gait direction)
  ;; Evaluate both fields everywhere.  Selecting a foot from POINT.x used to
  ;; put a discontinuity through the centre plane, visibly slicing the
  ;; screen-left boot open whenever its silhouette crossed that plane.
  (min (player-one-boot-distance point gait direction -1.0 0.0)
       (player-one-boot-distance point gait direction 1.0 1.0)))

(define-shader-function player-free-hand-height (gait)
  (+ 1.335 (* 0.05 (cos gait))))

(define-shader-function player-free-hand-swing (gait)
  (* -0.235 (sin gait)))

(define-shader-function player-arm-distance (point gait)
  "One arm swinging free and one closed around the staff.

Both run under the mantle, so what the silhouette actually shows is a
sleeve cuff and a mitten: the shoulders belong to the garment."
  (let* ((swing (player-free-hand-swing gait))
         (free-height (player-free-hand-height gait))
         (free-shoulder (vec3 -0.375 0.0 1.96))
         (free-elbow (vec3 -0.545 (* swing 0.42) 1.66))
         (free-hand (vec3 -0.635 swing free-height))
         (free (player-sdf-smooth-union
                (player-sdf-capsule point free-shoulder free-elbow 0.135)
                (player-sdf-capsule point free-elbow free-hand 0.125)
                0.055))
         (grip-shoulder (vec3 0.375 0.0 1.96))
         (grip-elbow (vec3 0.540 0.075 1.71))
         (grip-hand (vec3 0.625 0.195 1.505))
         (grip (player-sdf-smooth-union
                (player-sdf-capsule point grip-shoulder grip-elbow 0.135)
                (player-sdf-capsule point grip-elbow grip-hand 0.125)
                0.055)))
    (min free grip)))

(define-shader-function player-hand-distance (point gait)
  (min (player-sdf-sphere
        point (vec3 -0.635 (player-free-hand-swing gait)
                    (player-free-hand-height gait))
        0.125)
       (player-sdf-sphere point (vec3 0.625 0.195 1.505) 0.128)))

(define-shader-function player-staff-point (point)
  "The staff's own frame, with its head raked back over the walker."
  (let* ((rise (clamp (/ (- (swizzle point :z) 0.10) 3.10) 0.0 1.0)))
    (vec3 (- (swizzle point :x) (* 0.055 rise))
          (+ (swizzle point :y) (* 0.115 rise))
          (swizzle point :z))))

(define-shader-function player-staff-distance (point)
  "A copper-shod walking staff, carried plumb while the walker bobs past it.

The shaft is evaluated outside the pelvis pose on purpose: a staff that
rose and fell with the hips would be a staff nobody was leaning on."
  (player-sdf-capsule (player-staff-point point)
                      (vec3 0.640 0.195 0.06)
                      (vec3 0.640 0.195 3.14)
                      0.045))

(define-shader-function player-staff-head-distance (point)
  "The lozenge finial: a copper frame with the site's own diamond set in it."
  (let* ((framed (player-staff-point point))
         (frame (player-sdf-lozenge framed (vec3 0.640 0.195 3.34)
                                    (vec3 0.195 0.055 0.295)))
         (collar (player-sdf-sphere framed (vec3 0.640 0.195 3.07) 0.065)))
    (player-sdf-smooth-union frame collar 0.035)))

(define-shader-function player-staff-gem-distance (point)
  (player-sdf-lozenge (player-staff-point point)
                      (vec3 0.640 0.195 3.34)
                      (vec3 0.112 0.082 0.172)))

(define-shader-function player-distance (point gait direction)
  (let* ((posed (player-walk-pose point gait))
         (head-posed (player-head-pose posed gait))
         ;; The two garments hang from the shoulders in their own frames, so
         ;; they take the unposed sample and carry the walk themselves.
         (garments
           (player-sdf-smooth-union
            (player-sdf-smooth-union (player-robe-distance point gait)
                                     (player-mantle-distance point gait)
                                     0.055)
            (player-cowl-distance posed) 0.06))
         (face (player-sdf-smooth-union
                (player-head-distance head-posed)
                (player-beard-distance head-posed) 0.045))
         (hat (player-sdf-smooth-union
               (player-hat-distance head-posed)
               (player-hatband-distance head-posed) 0.02))
         (boots (player-boot-distance point gait direction))
         (limbs (player-sdf-smooth-union
                 (player-arm-distance posed gait)
                 (player-hand-distance posed gait) 0.07))
         ;; The staff is held, not grown: it meets the mitten with a hard
         ;; edge rather than a fillet, and stands outside the walk pose.
         (staff (min (player-staff-distance point)
                     (min (player-staff-head-distance point)
                          (player-staff-gem-distance point)))))
    (min (min staff boots)
         (player-sdf-smooth-union
          (player-sdf-smooth-union
           (player-sdf-smooth-union garments face 0.055) hat 0.025)
          limbs 0.075))))

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

(define-shader-function player-beard-tooth (point)
  "Strands, sampled in a domain stretched the way a beard actually hangs."
  (+ 1.0 (* 0.10 (- (paper-noise (* point (vec3 26.0 26.0 6.0))) 0.5))))

;;; The lozenge braid.  Colour is settled once, at the confirmed hit, so
;;; the pattern can afford to be a real pattern rather than a stripe.

(define-shader-function player-lozenge-figure (first second)
  "One diamond lattice cell's outline weight, in a plane of two coordinates."
  (let* ((across (- (fract first) 0.5))
         (along (- (fract second) 0.5))
         (radius (+ (abs across) (abs along))))
    (* (smoothstep 0.115 0.185 radius)
       (- 1.0 (smoothstep 0.265 0.335 radius)))))

(define-shader-function player-lozenge-braid (point pitch)
  "The site's diamond motif, wrapped around the figure without a seam.

Two planar lattices -- one read across his front, one along his flank --
blended by which of them the surface actually faces.  A single planar
lattice smears into stripes exactly where the body turns away from it."
  (let* ((x (* pitch (swizzle point :x)))
         (y (* pitch (swizzle point :y)))
         (z (* pitch (swizzle point :z)))
         (front (player-lozenge-figure x z))
         (flank (player-lozenge-figure y z))
         (sideness (/ (abs (swizzle point :y))
                      (max (+ (abs (swizzle point :x))
                              (abs (swizzle point :y)))
                           1e-4))))
    (mix front flank (smoothstep 0.35 0.75 sideness))))

(define-shader-function player-hem-band (value center width)
  (- 1.0 (smoothstep (* width 0.6) width (abs (- value center)))))

(define-shader-function player-face-detail (point albedo skin-weight)
  "Two dark eyes and a heavy brow, placed by direction from the head centre.

They live almost entirely in the brim's shadow.  That is the intent: the
sanctuary's camera should find a nose and a beard, and only a closer look
should find somebody looking back."
  (let* ((mirrored (vec3 (abs (swizzle point :x))
                         (swizzle point :y)
                         (swizzle point :z)))
         (direction (normalize (- mirrored (vec3 0.0 -0.01 2.09))))
         (eye-direction (normalize (vec3 0.34 0.86 0.16)))
         (eye (player-sdf-length (- direction eye-direction)))
         (pupil (- 1.0 (smoothstep 0.105 0.145 eye)))
         (brow-direction (normalize (vec3 0.35 0.82 0.32)))
         (brow (- 1.0 (smoothstep 0.115 0.185
                                  (player-sdf-length
                                   (* (- direction brow-direction)
                                      (vec3 0.45 0.9 1.9))))))
         (eyed (mix albedo (vec3 0.045 0.038 0.048) (* pupil skin-weight)))
         (browed (mix eyed (vec3 0.72 0.70 0.665)
                      (* brow (* skin-weight 0.85)))))
    browed))

(define-shader-function player-albedo (point gait direction)
  "The nearest part decides the colour.

Cream linen, warm copper and one red stone: the walls of the sanctuary,
worn.  The copper appears only where the site's own inlay would -- along a
hem, around a crown, set in a finial -- so the figure and the architecture
read as the same culture rather than as a doll placed on a bridge."
  (let* ((posed (player-walk-pose point gait))
         (head-posed (player-head-pose posed gait))
         (robe-posed (player-robe-point point gait))
         (mantle-posed (player-mantle-point point gait))
         (robe (player-robe-distance point gait))
         (mantle (player-mantle-distance point gait))
         (cowl (player-cowl-distance posed))
         (skin (min (player-head-distance head-posed)
                    (player-hand-distance posed gait)))
         (beard (player-beard-distance head-posed))
         (hat (player-hat-distance head-posed))
         (hatband (player-hatband-distance head-posed))
         (sleeves (player-arm-distance posed gait))
         (boots (player-boot-distance point gait direction))
         (staff (min (player-staff-distance point)
                     (player-staff-head-distance point)))
         (gem (player-staff-gem-distance point))
         (linen (player-linen-tooth robe-posed))
         (leather (player-leather-tooth posed))
         (strand (player-beard-tooth head-posed))
         (copper (vec3 0.535 0.215 0.075))
         ;; Copper braid along both hems: a deep embroidered band at the
         ;; robe's skirt and a narrower one where the cape is cut.
         ;; The site's own tonemap compresses hard above half albedo, so
         ;; the parts that must separate separate by a wide value step and
         ;; by hue, not by a polite shade apart.
         (placket
           (* (* (- 1.0 (smoothstep 0.028 0.062
                                    (abs (swizzle robe-posed :x))))
                 (smoothstep 0.04 0.30 (swizzle robe-posed :y)))
              (smoothstep 0.16 0.45 (swizzle robe-posed :z))))
         (robe-braid
           (max (max (* (player-lozenge-braid robe-posed 3.4)
                        (player-hem-band (swizzle robe-posed :z) 0.46 0.27))
                     (player-hem-band (swizzle robe-posed :z) 0.165 0.055))
                placket))
         (mantle-braid
           (max (* (player-lozenge-braid mantle-posed 4.4)
                   (player-hem-band (swizzle mantle-posed :z) 1.735 0.10))
                (player-hem-band (swizzle mantle-posed :z) 1.605 0.038)))
         (robe-color
           (mix (* (vec3 0.575 0.515 0.410) linen) copper robe-braid))
         (mantle-color
           (mix (* (vec3 0.235 0.110 0.080) linen) copper mantle-braid))
         (cowl-color (* (vec3 0.215 0.098 0.072) linen))
         (skin-color (vec3 0.605 0.385 0.245))
         (beard-color (* (vec3 0.755 0.745 0.725) strand))
         (hat-color
           (mix (* (vec3 0.395 0.340 0.255) linen) copper
                (* (player-lozenge-braid (player-hat-point head-posed) 4.8)
                   (player-hem-band (swizzle head-posed :z) 2.80 0.32))))
         (band-color (* copper 0.92))
         (sleeve-color (* (vec3 0.235 0.110 0.080) linen))
         (boot-color (* (vec3 0.085 0.062 0.050) leather))
         (staff-color (* (vec3 0.395 0.175 0.062) leather))
         (gem-color (vec3 0.455 0.040 0.070))
         ;; A running nearest-so-far, folded by hand: STEP answers one when
         ;; the candidate is at least as near, and MIX takes it.
         (color-1 (mix robe-color mantle-color (step mantle robe)))
         (near-1 (min robe mantle))
         (color-2 (mix color-1 sleeve-color (step sleeves near-1)))
         (near-2 (min near-1 sleeves))
         (color-3 (mix color-2 cowl-color (step cowl near-2)))
         (near-3 (min near-2 cowl))
         (color-4 (mix color-3 skin-color (step skin near-3)))
         (near-4 (min near-3 skin))
         (color-5 (mix color-4 beard-color (step beard near-4)))
         (near-5 (min near-4 beard))
         (color-6 (mix color-5 hat-color (step hat near-5)))
         (near-6 (min near-5 hat))
         (color-7 (mix color-6 band-color (step hatband near-6)))
         (near-7 (min near-6 hatband))
         (color-8 (mix color-7 boot-color (step boots near-7)))
         (near-8 (min near-7 boots))
         (color-9 (mix color-8 staff-color (step staff near-8)))
         (near-9 (min near-8 staff))
         (color-10 (mix color-9 gem-color (step gem near-9)))
         ;; How much of this point is face, for the details painted on it.
         (skin-weight
           (- 1.0 (smoothstep 0.0 0.03 (- skin (min near-9 gem))))))
    (player-face-detail head-posed color-10 skin-weight)))

(define-shader-function player-sample-local (origin ray center travel)
  "The figure's own coordinates for a point TRAVEL along a world ray."
  (let* ((relative (- (+ origin (* ray travel)) center)))
    (vec3 (swizzle relative :x)
          (swizzle relative :y)
          (+ (swizzle relative :z) 1.48))))

(define-shader-function player-march-advance
    (ray-distance origin ray center gait direction)
  "One sphere-tracing step, or the answer negated once the surface is met."
  (let* ((sample (player-sample-local origin ray center ray-distance))
         (local (vec3 (swizzle sample :x)
                      (* direction (swizzle sample :y))
                      (swizzle sample :z)))
         (distance (player-distance local gait direction)))
    (if (< distance 0.0025)
        (- 0.0 ray-distance)
        (+ ray-distance (max (* distance 0.82) 0.0025)))))

(define-shader-function player-march-step
    (ray-distance origin ray center gait direction bound)
  "PLAYER-MARCH-ADVANCE, guarded by whether this ray is already finished.

A finished march carries its answer negated, so both endings -- the surface
found and the bound passed -- are visible here before the figure's field is
sampled.  The billboard is much wider than the body for the sake of the
shadow it casts, and nearly every fragment on it is a ray that misses him;
without this guard each of those paid fifty-eight whole evaluations of a
figure it never touched."
  (if (< ray-distance 0.0)
      ray-distance
      (if (> ray-distance bound)
          ray-distance
          (player-march-advance ray-distance origin ray center
                                gait direction))))

(define-shader-function player-contact-shade
    (origin ray center sun ground-height)
  "How dark the traveler's own shadow lies on the deck at this ray.

The figure is drawn as a billboard over a bridge that is flat where he
walks, so the shadow he owes the deck can be found analytically: intersect
the ray with the deck plane and measure the ground point against the
segment the sun would sweep his body along.  It is a stylised smear, not a
projection -- the sanctuary's real occluders already have a shadow map, and
what this repays is the one thing the map cannot give a billboard, which is
that he is standing on something."
  (let* ((descent (min (swizzle ray :z) -1e-4))
         (travel (/ (- ground-height (swizzle origin :z)) descent))
         (ground-point (+ (swizzle origin :xy) (* (swizzle ray :xy) travel)))
         ;; The sun's drift across the deck per unit of height, held to a
         ;; plausible maximum so a low sun cannot throw the smear off the
         ;; bridge entirely.
         (drift (/ (swizzle sun :xy) (max (swizzle sun :z) 0.55)))
         (foot (swizzle center :xy))
         (crown (- foot (* drift 1.55)))
         (offset (- ground-point foot))
         (axis (- crown foot))
         (along (clamp (/ (dot offset axis) (max (dot axis axis) 1e-5))
                       0.0 1.0))
         (across (player-sdf-length (- offset (* axis along))))
         ;; The puddle is deliberately a little wider than he is: an
         ;; isometric camera hides the deck directly under a robed figure,
         ;; so a shadow exactly his size is a shadow nobody ever sees.
         (width (mix 1.30 0.40 along))
         (edge (- across width))
         (shape (- 1.0 (smoothstep -0.40 0.10 edge)))
         (fade (mix 0.82 0.34 along))
         (behind (step 0.0 travel)))
    (* (* shape fade) behind)))

(define-shader-function player-world-direction (local heading)
  "Rotate a player-local direction into the horizontal world frame."
  (let* ((right (vec2 (swizzle heading :y) (- (swizzle heading :x)))))
    (vec3 (+ (* (swizzle right :x) (swizzle local :x))
             (* (swizzle heading :x) (swizzle local :y)))
          (+ (* (swizzle right :y) (swizzle local :x))
             (* (swizzle heading :y) (swizzle local :y)))
          (swizzle local :z))))

(define-live-shader player-sdf-vertex-specification
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position-output :vec3 :location 0
                                            :quantity quantities:world-position
                                            :unit quantities:cell)
               ;; Point plus radius is a deliberately heterogeneous private
               ;; packing seam; each side unpacks its checked constituents.
               (center-radius-output :vec4 :location 1)
               (current-clip-output :vec4 :location 2)
               (previous-clip-output :vec4 :location 3))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.*scene-uniform-members*)
                 (shadow-map :depth-texture-2d :binding 1)
                 (shadow-sampler :sampler :binding 2)))
  (let* ((index (float vertex-index))
         (right-corner (if (= index 2.0) 1.0
                           (if (= index 3.0) 1.0
                               (if (= index 5.0) 1.0 0.0))))
         (bottom-corner (if (= index 1.0) 1.0
                            (if (= index 4.0) 1.0
                                (if (= index 5.0) 1.0 0.0))))
         ;; The quad is not square.  The body needs a little over nine
         ;; tenths of the radius above its centre and no more; the shadow it
         ;; casts needs the deck below and beside it, which is where the
         ;; extra room goes rather than into a square nobody looks at.
         (across (- (* right-corner 2.0) 1.0))
         (vertical (- (* bottom-corner 2.0) 1.0))
         (corner
           (assume-quantity
            (vec2 (* across 1.45)
                  (if (> vertical 0.0) (* vertical 1.85) vertical))
            :unit :one))
         (center (swizzle character-parameters :xyz))
         (previous-center
           (swizzle previous-character-parameters :xyz))
         (radius
           (quantity 2.05 :quantity quantities:world-distance
                          :unit quantities:cell))
         (proxy-world-position
           (+ (- center
                 (interpret (* (swizzle camera-forward :xyz) radius)
                            :quantity quantities:world-position
                            :unit quantities:cell :character :difference))
              (+ (interpret
                  (* (swizzle camera-right :xyz)
                     (* (swizzle corner :x) radius))
                  :quantity quantities:world-position
                  :unit quantities:cell :character :difference)
                 (interpret
                  (* (swizzle camera-up :xyz)
                     (* (swizzle corner :y) radius))
                  :quantity quantities:world-position
                  :unit quantities:cell :character :difference))))
         (previous-proxy-world-position
           (+ (- previous-center
                 (interpret
                  (* (swizzle previous-camera-forward :xyz) radius)
                  :quantity quantities:world-position
                  :unit quantities:cell :character :difference))
              (+ (interpret
                  (* (swizzle previous-camera-right :xyz)
                     (* (swizzle corner :x) radius))
                  :quantity quantities:world-position
                  :unit quantities:cell :character :difference)
                 (interpret
                  (* (swizzle previous-camera-up :xyz)
                     (* (swizzle corner :y) radius))
                  :quantity quantities:world-position
                  :unit quantities:cell :character :difference))))
         (current-clip
           (mesh-view-clip proxy-world-position camera-position camera-right
                           camera-up camera-forward camera-projection
                           (swizzle (representation render-parameters) :z)))
         (previous-clip
           (mesh-view-clip previous-proxy-world-position
                           previous-camera-position previous-camera-right
                           previous-camera-up previous-camera-forward
                           previous-camera-projection
                           (swizzle (representation render-parameters) :z)))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (vec4 (+ (swizzle current-clip :x)
                         (* (swizzle jitter :x) (swizzle current-clip :w)))
                      (+ (swizzle current-clip :y)
                         (* (swizzle jitter :y) (swizzle current-clip :w)))
                      (swizzle current-clip :z)
                      (swizzle current-clip :w)))
    (set-output proxy-world-position-output proxy-world-position)
    (set-output center-radius-output
                (vec4 (representation center) (representation radius)))
    (set-output current-clip-output current-clip)
    (set-output previous-clip-output previous-clip)))

(define-live-shader player-sdf-fragment-specification
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0
                                    :quantity quantities:world-position
                                    :unit quantities:cell)
              (center-radius :vec4 :location 1)
              (current-clip :vec4 :location 2)
              (previous-clip :vec4 :location 3))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.*scene-uniform-members*)
                 (shadow-map :depth-texture-2d :binding 1)
                 (shadow-sampler :sampler :binding 2)))
  (let* ((center-point
           (assume-quantity (swizzle center-radius :xyz)
                            :quantity quantities:world-position
                            :unit quantities:cell))
         (radius-distance
           (assume-quantity (swizzle center-radius :w)
                            :quantity quantities:world-distance
                            :unit quantities:cell))
         (center (representation center-point))
         (radius (representation radius-distance))
         (gait (swizzle (representation character-parameters) :w))
         ;; The game controller rotates world space into the traveler's local
         ;; forward frame below, so the hermit's authored stride always runs
         ;; along local +Y.
         (direction 1.0)
         (heading
           (representation
            (normalize (swizzle character-direction :xy))))
         (player-right (vec2 (swizzle heading :y)
                             (- (swizzle heading :x))))
         (ray-direction
           (if (< (swizzle (representation render-parameters) :z) 0.5)
               (normalize (swizzle camera-forward :xyz))
               (assume-quantity
                (normalize
                 (representation
                  (- proxy-world-position
                     (swizzle camera-position :xyz))))
                :quantity quantities:world-direction :unit :one)))
         (ray (representation ray-direction))
         ;; The signed-distance program below is deliberately procedural
         ;; representation; checked point/distance arithmetic resumes at its
         ;; lighting and projection boundaries.
         (origin (representation proxy-world-position))
         ;; The hermit's field is rotated into his live walking heading.  The
         ;; branch study's shortcut marched in world axes, which diverged as
         ;; soon as a player turned and left a transparent robe behind.
         (travel
           (counted-fold (march 58.0 ray-distance 0.0)
             (let* ((world-point (+ origin (* ray ray-distance)))
                    (relative (- world-point center))
                    (relative-xy (swizzle relative :xy))
                    (local (vec3 (dot relative-xy player-right)
                                 (dot relative-xy heading)
                                 (+ (swizzle relative :z) 1.48)))
                    (distance
                      (min (player-distance local gait direction)
                           (player-wizard-orb-distance local))))
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
         (player-surface-distance
           (player-distance local gait direction))
         (orb-surface-distance
           (player-wizard-orb-distance local))
         (orb-p (< orb-surface-distance player-surface-distance))
         (surface-distance (min player-surface-distance orb-surface-distance))
         (coverage (- 1.0 (step 0.006 surface-distance)))
         (local-normal
           (if orb-p
               ;; The central sphere owns the visible normal.  Its tiny
               ;; firework motes use the same radial answer, which makes the
               ;; glow read as one light instead of pixel speckle.
               (normalize (- local (vec3 0.640 0.195 3.34)))
               (player-normal local gait direction)))
         (normal
           (normalize (player-world-direction local-normal heading)))
         (albedo (if orb-p (vec3 0.42 0.88 1.0)
                     (player-albedo local gait direction)))
         (sun-direction (swizzle sun-vector :xyz))
         (sun (representation sun-direction))
         (sun-color
           (representation (swizzle sun-color-vector :xyz)))
         (sky (representation (swizzle sky-color-vector :xyz)))
         (ground (representation (swizzle ground-color-vector :xyz)))
         (facing (dot normal sun))
         (direct-shape (smoothstep 0.0 0.72 (max 0.0 facing)))
         (light-clip
           (light-clip-position
            (assume-quantity world-point
                             :quantity quantities:world-position
                             :unit quantities:cell)
            shadow-row-x shadow-row-y shadow-row-z shadow-row-w))
         (player-shadow-sample
           (assume-quantity
            (vec3 (+ (* (swizzle light-clip :x) 0.5) 0.5)
                  (+ (* (swizzle light-clip :y) 0.5) 0.5)
                  (swizzle light-clip :z))
            :quantity quantities:shadow-coordinate :unit :one))
         (sampled-shadow
           (soft-shadow-visibility shadow-map shadow-sampler
                                   player-shadow-sample normal sun
                                   (representation shadow-control)))
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
         (radiance
           (+ paper
              (* (vec3 0.20 0.42 0.48) (* rim 0.07))
              ;; A real source of color: the orb is blue-white at the core,
              ;; warmed by a small peach firework halo.
              (if orb-p
                  (+ (vec3 0.32 0.85 1.10)
                     (* (vec3 1.0 0.34 0.10)
                        (smoothstep 0.02 0.11 (abs orb-surface-distance))))
                  (vec3 0.0 0.0 0.0))))
         ;; Where the body was missed, the same billboard still owes the deck
         ;; his shadow.  Its own sample of the sun's shadow map keeps him from
         ;; darkening ground the sanctuary is already shading.
         (deck-height (- (swizzle center :z) 1.48))
         (deck-point
           (+ origin (* ray (/ (- deck-height (swizzle origin :z))
                               (min (swizzle ray :z) -1e-4)))))
         ;; Lifted clear of the deck before it is looked up: sampling the
         ;; sun's depth map at a point lying exactly on the surface that
         ;; wrote it is the textbook way to shadow a floor with itself.
         (deck-clip
           (light-clip-position
            (assume-quantity (+ deck-point (vec3 0.0 0.0 0.09))
                             :quantity quantities:world-position
                             :unit quantities:cell)
            shadow-row-x shadow-row-y shadow-row-z shadow-row-w))
         (deck-lit
           (soft-shadow-visibility
            shadow-map shadow-sampler
            (assume-quantity
             (vec3 (+ (* (swizzle deck-clip :x) 0.5) 0.5)
                   (+ (* (swizzle deck-clip :y) 0.5) 0.5)
                   (swizzle deck-clip :z))
             :quantity quantities:shadow-coordinate :unit :one)
            (vec3 0.0 0.0 1.0) sun
            (representation shadow-control)))
         (contact
           (* (* (player-contact-shade origin ray center sun deck-height)
                 deck-lit)
              (- 1.0 coverage)))
         ;; This pass is premultiplied over the already-lit deck.  A fixed
         ;; positive "shadow pigment" becomes emissive wherever the deck is
         ;; darker than that pigment—the pale halo we were seeing around the
         ;; traveler.  Zero source radiance with positive alpha is pure
         ;; attenuation, so the analytic footprint can only make the receiver
         ;; darker.
         (shade-color (vec3 0.0 0.0 0.0))
         (composite-alpha (+ coverage contact)))
    (set-output color-output
                (vec4 (+ (* radiance coverage) shade-color) composite-alpha))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))
