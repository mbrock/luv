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

(define-live-shader torch-body-fragment-specification
    (:stage :fragment
     :inputs ((world-position :vec3 :location 0
                              :quantity quantities:world-position
                              :unit quantities:cell)
              (mesh-normal :vec3 :location 1 :interpolation :flat
                           :quantity quantities:world-orientation :unit :one)
              (current-clip :vec4 :location 4)
              (previous-clip :vec4 :location 5)
              (shadow-sample :vec3 :location 6
                             :quantity quantities:shadow-coordinate :unit :one)
              (voxel-light :vec3 :location 9))
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
         (facing (max 0.0 (dot normal sun)))
         (visibility
           (soft-shadow-visibility
            shadow-map shadow-sampler shadow-sample normal sun
            (representation shadow-control)))
         (upness (swizzle normal :z))
         (sky-weight (+ 0.5 (* 0.5 upness)))
         (ambient (+ (* ground (- 1.0 sky-weight)) (* sky sky-weight)))
         (bronze (vec3 0.47 0.17 0.04))
         (local-light (* 0.45 (* voxel-light voxel-light)))
         (radiance
           (* bronze
              (+ (* ambient 0.72)
                 (* sun-color (* visibility facing))
                 local-light)))
         (camera-delta
           (representation
            (- world-position (swizzle camera-position :xyz))))
         (distance (sqrt (dot camera-delta camera-delta)))
         (fog (smoothstep 165.0 300.0 distance)))
    (set-output color-output (vec4 (mix radiance sky fog) 1.0))
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

(define-shader-function sky-view-ray
    (ndc camera-right camera-up camera-forward camera-projection divisor)
  (let* ((perspective-ray
           (normalize
            (+ (swizzle camera-forward :xyz)
               (* (swizzle camera-right :xyz)
                  (assume-quantity
                   (/ (swizzle ndc :x)
                      (swizzle camera-projection :x))
                   :unit :one))
               (* (swizzle camera-up :xyz)
                  (assume-quantity
                   (/ (- (swizzle ndc :y))
                      (swizzle camera-projection :y))
                   :unit :one)))))
         (isometric-ray
           (normalize
            (+ (swizzle camera-forward :xyz)
               (* (swizzle camera-up :xyz)
                  (assume-quantity
                   (* (- (swizzle ndc :y)) 0.38) :unit :one))))))
    (mix isometric-ray perspective-ray
         (assume-quantity divisor :unit :one))))

(define-shader-function painted-sky-radiance
    (ndc camera-right camera-up camera-forward camera-projection divisor
     sun-vector sun-color-vector sky-color-vector)
  "Return view-stable late-afternoon HDR radiance before exposure or grading."
  (let* ((ray-direction
           (sky-view-ray ndc camera-right camera-up camera-forward
                         camera-projection divisor))
         ;; Cloud and watercolor shaping are procedural image mathematics;
         ;; the result re-enters the semantic ladder as scene radiance.
         (ray (representation ray-direction))
         (height (swizzle ray :z))
         (upness (clamp height 0.0 1.0))
         (horizon-weight (smoothstep -0.10 0.42 height))
         (base-sky
           (representation (swizzle sky-color-vector :xyz)))
         (horizon (vec3 0.58 0.78 1.06))
         (zenith (* base-sky (vec3 0.34 0.58 0.94)))
         (horizon-haze
           (- 1.0 (smoothstep 0.01 0.26 (abs height))))
         (atmosphere
           (mix (mix horizon zenith horizon-weight)
                (vec3 1.12 0.68 0.38)
                (* horizon-haze 0.13)))
         ;; Broad directional noise makes sparse watercolor cloud banks.  The
         ;; envelope keeps them above the haze and below the clear zenith.
         (cloud-point
           (vec3 (* (swizzle ray :x) 5.0)
                 (* (swizzle ray :y) 5.0)
                 (* height 12.0)))
         (cloud-coarse (paper-noise (+ cloud-point (vec3 3.1 11.7 5.3))))
         (cloud-fine
           (paper-noise
            (+ (* cloud-point (vec3 2.3 2.3 1.4))
               (vec3 17.9 2.7 31.1))))
         (cloud-shape
           (smoothstep 0.52 0.72 (+ (* cloud-coarse 0.72)
                                    (* cloud-fine 0.28))))
         (cloud-height
           (* (smoothstep 0.05 0.20 upness)
              (- 1.0 (smoothstep 0.58 0.90 upness))))
         (sun
           (representation (normalize (swizzle sun-vector :xyz))))
         (sun-facing (max 0.0 (dot ray sun)))
         (sun-halo (smoothstep 0.965 0.9992 sun-facing))
         (sun-disc (smoothstep 0.99925 0.99982 sun-facing))
         (cloud-light
           (mix (vec3 0.76 0.88 1.10)
                (vec3 1.34 0.78 0.42)
                (smoothstep 0.72 0.98 sun-facing)))
         (clouded
           (mix atmosphere cloud-light (* cloud-shape cloud-height 0.26)))
         (radiance
           (+ clouded
              (* (representation (swizzle sun-color-vector :xyz))
                 (+ (* sun-halo 0.10) (* sun-disc 2.8))))))
    (assume-quantity radiance
                     :quantity quantities:scene-radiance :unit :one)))

(define-live-shader sky-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 17))))
  (let* ((radiance
           (painted-sky-radiance
            ndc camera-right camera-up camera-forward camera-projection
            (swizzle (representation render-parameters) :z)
            sun-vector sun-color-vector
            sky-color-vector)))
    (set-output color-output (vec4 (representation radiance) 1.0))))

(define-live-shader sky-temporal-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 17))))
  (let* ((divisor (swizzle (representation render-parameters) :z))
         ;; The fullscreen triangle itself cannot move. Reconstruct the ray at
         ;; the same jittered sample location as geometry, as the original
         ;; Vulkan resolve did, and derive motion from that unjittered address.
         (sample-ndc
           (- ndc
              (representation (swizzle temporal-parameters :xy))))
         (ray (sky-view-ray sample-ndc camera-right camera-up camera-forward
                            camera-projection divisor))
         (radiance
           (painted-sky-radiance
            sample-ndc camera-right camera-up camera-forward camera-projection
            divisor
            sun-vector sun-color-vector sky-color-vector))
         (previous-z
           (representation
            (dot ray (swizzle previous-camera-forward :xyz))))
         (previous-clip
           (vec4 (* (representation
                     (dot ray (swizzle previous-camera-right :xyz)))
                    (swizzle previous-camera-projection :x))
                 (- (* (representation
                        (dot ray (swizzle previous-camera-up :xyz)))
                       (swizzle previous-camera-projection :y)))
                 0.0
                 (mix 1.0 previous-z divisor)))
         (current-clip
           (vec4 (swizzle sample-ndc :x) (swizzle sample-ndc :y) 0.0 1.0)))
    (set-output color-output (vec4 (representation radiance) 1.0))
    (set-output motion-output
                (mesh-temporal-motion previous-clip current-clip))))

(define-live-shader exposure-probe-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((scene :texture-2d :binding 0 :sample-transfer :identity
                        :sample-components
                        ((:xyz :quantity quantities:scene-radiance
                          :unit :one)))
                 (scene-sampler :sampler :binding 1)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         ;; Match Moppe's broad five-tap probe footprint before the 32x16
         ;; reduction. Encoding log luminance into UNORM makes the geometric
         ;; mean portable through the HAL's compact RGBA8 readback contract.
         (offset (vec2 0.008 0.014))
         (average
           (* (+ (swizzle (sample scene scene-sampler uv) :xyz)
                 (swizzle (sample scene scene-sampler (+ uv offset)) :xyz)
                 (swizzle (sample scene scene-sampler (- uv offset)) :xyz)
                 (swizzle
                  (sample scene scene-sampler
                          (+ uv (vec2 (swizzle offset :x)
                                      (- (swizzle offset :y))))) :xyz)
                 (swizzle
                  (sample scene scene-sampler
                          (+ uv (vec2 (- (swizzle offset :x))
                                      (swizzle offset :y)))) :xyz))
              0.2))
         (luminance
           (max
            (scene-relative-luminance average)
            (quantity 0.0001 :quantity quantities:scene-luminance
                             :unit :one)))
         ;; SCENE-LUMINANCE is relative to reference white 1.0.  LOG is the
         ;; explicit nonlinear encoding boundary, so only its normalized
         ;; representation enters the portable UNORM reduction.
         (encoded
           (clamp (/ (+ (log (representation luminance)) 9.21034)
                     11.98293)
                  0.0 1.0)))
    (set-output color-output (vec4 encoded encoded encoded 1.0))))

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
