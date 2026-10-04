(in-package #:luft.render)

;;; The atmosphere draws world radiance before geometry. Temporal rendering
;;; also needs the sky's motion, but no resident meshes or shadow inputs.

(defclass sky-drawing (pipeline-scene-drawing) ())

(defun make-sky-drawing (device target-formats sample-count)
  (make-pipeline-scene-drawing
   'sky-drawing device :label "luft HDR sky"
   :vertex (shaders:present-vertex-specification)
   :fragment (if (rest target-formats)
                 (shaders:sky-temporal-fragment-specification)
                 (shaders:sky-fragment-specification))
   :targets (mapcar (lambda (format) `(:format ,format)) target-formats)
   :sample-count sample-count :depth-compare :always :vertex-count 3))

(defmethod make-scene-drawing-binding
    ((drawing sky-drawing) device camera-buffer shadow-view shadow-sampler)
  (declare (ignore shadow-view shadow-sampler))
  (make-program-binding (scene-drawing-program drawing) device
                        :camera-state camera-buffer))

(in-package #:luft.render.shaders)

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

;;; The sky is image mathematics over a view ray and the frame environment,
;;; ported from Luvcraft's sky (luvcraft/shaders.lisp, the :SKY role) to
;;; Luft's Z-up lattice.  Everything up there is one atmosphere seen at
;;; different depths: a gradient spread over the whole hemisphere, haze that
;;; thickens toward the horizon and carries the sunset band only in the sun's
;;; own quarter of the compass, one Henyey-Greenstein glow around the sun, a
;;; cumulus deck at a fixed world height whose own shadow toward the sun
;;; gives it a lit face, and a compact solar disc pushed far past display
;;; white so the lens chain has something real to bloom.  At night there are
;;; points for stars, a band for the galaxy, and a full moon opposite the sun,
;;; all fixed to a sky that turns with the day clock.  Below the horizon the
;;; sky arrives at exactly the aerial colour distant terrain fades into.

(define-shader-function cloud-fractal-noise (point)
  "Four octaves of paper noise: a cloud deck's shape down to its wisps."
  (let* ((p1 (* point 2.11))
         (p2 (+ (* p1 2.11) (vec3 11.17 3.7 5.3)))
         (p3 (+ (* p2 2.11) (vec3 -7.3 13.1 2.9))))
    (* (+ (* (paper-noise point) 0.5)
          (* (paper-noise p1) 0.25)
          (* (paper-noise p2) 0.125)
          (* (paper-noise p3) 0.0625))
       1.067)))

(define-shader-function celestial-ray (ray pole angle)
  "Turn RAY back through the sky's rotation of ANGLE about the unit POLE.

The result is where RAY points among the fixed stars: the sun's orbit is
this same rotation, so a star field looked up by the result wheels across
the sky with the sun and moon instead of staying painted on the dome."
  (let* ((cosine (cos angle))
         (sine (sin angle))
         (pole-cross-ray
           (- (* (swizzle pole :yzx) (swizzle ray :zxy))
              (* (swizzle pole :zxy) (swizzle ray :yzx)))))
    (+ (* ray cosine)
       (* pole-cross-ray (- sine))
       (* pole (* (dot pole ray) (- 1.0 cosine))))))

(define-shader-function night-star-light (direction elapsed)
  "One star per lattice cell of the sky, at a hashed place in its cell.

A star is a point, so each cell hashes to a position, a magnitude, and a
twinkling phase, and draws a small gaussian there.  Magnitude is a steep
power of its hash: a few bright stars and a great many faint ones, which is
the actual distribution overhead.  Luvcraft's star field, unchanged."
  (let* ((point (* direction 74.0))
         (cell (floor point))
         (local (fract point))
         (place-x (paper-hash (+ cell (vec3 19.7 5.3 11.1))))
         (place-y (paper-hash (+ cell (vec3 3.1 23.9 7.7))))
         (place-z (paper-hash (+ cell (vec3 41.3 13.7 29.5))))
         ;; Keep the star off its cell's boundary so no star is ever cut in
         ;; half by the next cell's gaussian falling off first.
         (centre (+ (vec3 0.25 0.25 0.25)
                    (* (vec3 place-x place-y place-z) 0.5)))
         (offset (- local centre))
         (radius (dot offset offset))
         (magnitude (expt place-z 9.0))
         (twinkle (+ 0.74 (* 0.26 (sin (+ (* elapsed 2.3)
                                          (* place-x 43.0))))))
         (spread (exp (* -230.0 radius))))
    (* magnitude (* twinkle spread))))

(define-shader-function night-sky-radiance
    (ray celestial elevation moon celestial-moon night-vector parameters
     elapsed)
  "Stars, the galaxy, and the moon along RAY, before clouds cover them.

CELESTIAL and CELESTIAL-MOON are RAY and the moon's direction MOON turned
back among the fixed stars;
NIGHT-VECTOR's W says how far the sun has sunk, and PARAMETERS carry star
brightness, moon radiance, moon angular radius, and galaxy strength."
  (let* ((night (swizzle night-vector :w))
         (star-brightness (swizzle parameters :x))
         (moon-radiance (swizzle parameters :y))
         (moon-radius (swizzle parameters :z))
         (galaxy-strength (swizzle parameters :w))
         ;; The galaxy is a great circle of the sky, so one fixed axis and the
         ;; ray's distance from its plane is the whole band.  The axis leans
         ;; toward the celestial pole, so the band arches high over the late
         ;; evening rather than lying along the horizon haze.
         (galaxy-axis (vec3 -0.74 -0.09 0.67))
         (galaxy-distance (dot celestial galaxy-axis))
         (galaxy-band (exp (* -20.0 (* galaxy-distance galaxy-distance))))
         (galaxy-structure
           (+ (* (paper-noise (* celestial 15.0)) 0.58)
              (* (paper-noise (* celestial 44.0)) 0.42)))
         (galaxy
           (* galaxy-band
              (* (+ 0.10 (* 1.25 galaxy-structure))
                 ;; A dust lane is the band's own darkness, not an absence of
                 ;; stars, so it multiplies rather than subtracts.
                 (- 1.0 (* 0.55 (smoothstep 0.42 0.66
                                            (paper-noise
                                             (* celestial 7.0))))))))
         (star-visibility (* night (smoothstep -0.02 0.14 elevation)))
         (stars
           (* (night-star-light celestial elapsed)
              (* star-brightness
                 (* star-visibility (+ 1.0 (* 1.6 galaxy-band))))))
         (starred
           (+ (* (vec3 0.60 0.66 0.94)
                 (* galaxy (* star-visibility (* 0.17 galaxy-strength))))
              (* (vec3 0.92 0.94 1.0) stars)))
         ;; The moon rides opposite the sun, always full.  Its disc is drawn
         ;; a few times its true size and far above white for the bloom.
         (moon-alignment (dot ray moon))
         (moon-limb (* 0.5 (* moon-radius moon-radius)))
         (moon-disc
           (smoothstep (- 1.0 moon-limb) (- 1.0 (* 0.80 moon-limb))
                       moon-alignment))
         ;; The ray's offset across the moon's own direction, in units of its
         ;; radius, is the disc's face, which the maria are painted on.  It
         ;; is measured among the fixed stars, so the maria keep their places
         ;; on the disc as the moon crosses the sky.
         (moon-face
           (/ (- celestial (* celestial-moon moon-alignment))
              (max 0.0001 moon-radius)))
         ;; Broad dark seas with a little finer mottling, cut with a soft
         ;; threshold so they read as patches on the bright highlands even
         ;; where the tone curve has compressed the disc toward white.
         (maria-field
           (+ (* (paper-noise (+ (* moon-face 1.6) (vec3 13.0 5.0 9.0))) 0.72)
              (* (paper-noise (+ (* moon-face 4.3) (vec3 2.0 17.0 4.0)))
                 0.28)))
         (moon-maria (smoothstep 0.44 0.60 maria-field))
         (moon-shape
           (* (- 1.0 (* 0.56 moon-maria))
              (- 1.0 (* 0.25 (clamp (dot moon-face moon-face) 0.0 1.0)))))
         (moon-glow
           (+ (* (expt (max 0.0 moon-alignment) 2400.0) 0.10)
              (* (expt (max 0.0 moon-alignment) 260.0) 0.035)))
         (moon-up (smoothstep -0.01 0.01 elevation))
         (lunar
           (* (vec3 0.94 0.95 1.0)
              (* (* moon-up (mix 0.55 1.0 night))
                 (+ (* moon-disc (* moon-radiance moon-shape))
                    moon-glow)))))
    (+ starred lunar)))

(define-shader-function painted-sky-radiance
    (ray eye sun-vector sun-color-vector zenith-vector horizon-vector
     fog-vector parameters moon-vector pole-vector night-parameters)
  "Return the HDR sky radiance along RAY before exposure or grading.

The sun lanes carry the key light, which is the moon at night.  Every solar
term drawn from them is weighted by the day factor, which is zero by then;
the cloud deck's self-shadow is not, so the moon lights the clouds' faces."
  (let* ((ray (representation ray))
         (eye (swizzle (representation eye) :xyz))
         (sun (normalize (swizzle (representation sun-vector) :xyz)))
         (sun-color (swizzle (representation sun-color-vector) :xyz))
         (zenith (swizzle (representation zenith-vector) :xyz))
         (horizon (swizzle (representation horizon-vector) :xyz))
         (fog-color (swizzle (representation fog-vector) :xyz))
         (day-factor (swizzle (representation zenith-vector) :w))
         (cloudiness (swizzle (representation horizon-vector) :w))
         (elapsed (swizzle parameters :x))
         (elevation (swizzle ray :z))
         (above (max elevation 0.0))
         ;; One number decides how much of the sky is sunset: a sun near the
         ;; horizon warms the haze, the glow, and the cloud faces together.
         (low-sun
           (* day-factor (- 1.0 (smoothstep 0.02 0.45 (swizzle sun :z)))))
         (alignment (dot ray sun))
         (toward-sun (max 0.0 alignment))
         (level-ray (vec3 (swizzle ray :x) (swizzle ray :y) 0.0))
         (level-sun (vec3 (swizzle sun :x) (swizzle sun :y) 0.0))
         (azimuth
           (max 0.0
                (/ (dot level-ray level-sun)
                   (max 0.001 (* (sqrt (dot level-ray level-ray))
                                 (sqrt (dot level-sun level-sun)))))))
         ;; --- the atmosphere ---------------------------------------------
         ;; A small exponent spreads the gradient over the whole hemisphere,
         ;; which is what optical depth along a ray actually does.
         (gradient (expt (clamp elevation 0.0 1.0) 0.42))
         (base (mix horizon zenith gradient))
         (haze (exp (* -9.0 above)))
         (warm-band
           (* low-sun (* haze (+ 0.12 (* 0.88 (* azimuth azimuth))))))
         (hazed (mix base (* sun-color 0.82)
                     (clamp (* 0.92 warm-band) 0.0 1.0)))
         (broad (henyey-greenstein alignment 0.62))
         (tight (henyey-greenstein alignment 0.90))
         (halo-tint (mix (vec3 1.0 0.97 0.90) (vec3 1.0 0.52 0.24) low-sun))
         (halo
           (* (+ (* broad (+ 0.030 (* 0.055 low-sun)))
                 (* tight (+ 0.014 (* 0.055 low-sun))))
              (* day-factor (mix 0.70 1.60 haze))))
         (scattered (+ hazed (* halo-tint halo)))
         ;; --- the night --------------------------------------------------
         (moon (normalize (swizzle (representation moon-vector) :xyz)))
         (pole (swizzle (representation pole-vector) :xyz))
         (sky-turn (swizzle (representation pole-vector) :w))
         (nightly
           (+ scattered
              (night-sky-radiance
               ray (celestial-ray ray pole sky-turn) elevation moon
               (celestial-ray moon pole sky-turn)
               (representation moon-vector) night-parameters elapsed)))
         ;; --- the cloud deck ---------------------------------------------
         ;; A plane at a fixed world height, so the ray meets it farther out
         ;; the closer it runs to level and turning the head never slides it.
         (deck-rise (max 8.0 (- 230.0 (swizzle eye :z))))
         (deck-point
           (+ (+ eye (* ray (/ deck-rise (max elevation 0.014))))
              (vec3 (* elapsed 2.2) (* elapsed 1.2) 0.0)))
         (deck-scale 0.0044)
         (cloud-field (cloud-fractal-noise (* deck-point deck-scale)))
         (coverage (mix 0.82 0.46 (clamp cloudiness 0.0 1.0)))
         (softness (mix 0.26 0.055 (smoothstep 0.012 0.34 elevation)))
         (cloud-density
           (* (smoothstep 0.006 0.055 elevation)
              (smoothstep coverage (+ coverage softness) cloud-field)))
         (core (clamp cloud-density 0.0 1.0))
         ;; What lies between this piece of deck and the sun, sampled along
         ;; the deck itself: the shape's own shadow, and so its lit face.
         (shadow-field
           (cloud-fractal-noise
            (* (+ deck-point (* level-sun 300.0)) deck-scale)))
         (shadow-density
           (smoothstep coverage (+ coverage softness) shadow-field))
         (cloud-light (- 1.0 (* 0.70 shadow-density)))
         (cloud-lit
           (mix (vec3 1.22 1.20 1.16) (vec3 1.40 0.74 0.36)
                (* low-sun low-sun)))
         (cloud-dark
           (mix (vec3 0.56 0.62 0.78) (vec3 0.46 0.40 0.54)
                (* low-sun low-sun)))
         (cloud-body
           (mix cloud-dark cloud-lit (* cloud-light (- 1.0 (* 0.45 core)))))
         ;; Silver lining: thin edges facing the sun glow, dense cores do not.
         (silver
           (* cloud-lit
              (* (expt toward-sun 14.0)
                 (* (- 1.0 core) (+ 0.30 (* 0.85 low-sun))))))
         (night-tint
           (mix (vec3 0.44 0.52 0.80) (vec3 1.0 1.0 1.0)
                (smoothstep 0.0 0.35 day-factor)))
         (cloud-color
           (* (* (+ cloud-body silver) night-tint) (max day-factor 0.06)))
         (cloud-reach (smoothstep 0.010 0.11 elevation))
         (clouded
           (mix nightly (mix hazed cloud-color cloud-reach)
                (* cloud-density 0.94)))
         ;; --- the ground half --------------------------------------------
         (aerial (aerial-perspective-color fog-color ray sun day-factor))
         (depth-below (smoothstep 0.0 -0.35 elevation))
         (ground (* aerial (- 1.0 (* 0.22 depth-below))))
         (grounded
           (mix clouded ground (smoothstep 0.060 -0.006 elevation)))
         ;; --- the sun ----------------------------------------------------
         ;; Drawn at a few times the true angular radius, with limb darkening
         ;; so it reads as a star rather than a sticker, and far above white.
         (disc-radius 0.022)
         (disc-limb (* 0.5 (* disc-radius disc-radius)))
         (disc
           (smoothstep (- 1.0 disc-limb) (- 1.0 (* 0.56 disc-limb)) alignment))
         (disc-radial
           (clamp (/ (- 1.0 alignment) (max 0.000001 disc-limb)) 0.0 1.0))
         (limb-darkening (- 1.0 (* 0.45 (* disc-radial disc-radial))))
         (corona
           (+ (* (expt toward-sun 900.0) 0.8)
              (* (expt toward-sun 130.0) 0.22)))
         (occlusion (- 1.0 (* core 0.94)))
         (horizon-occlusion (smoothstep -0.01 0.01 elevation))
         (solar
           (* sun-color
              (* day-factor
                 (* horizon-occlusion
                    (+ (* disc (* 36.0 (* occlusion limb-darkening)))
                       (* corona (* 2.0 occlusion)))))))
         (radiance (+ grounded solar)))
    (assume-quantity radiance
                     :quantity quantities:scene-radiance :unit :one)))

(define-live-shader sky-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 34))))
  (let* ((ray (sky-view-ray ndc camera-right camera-up camera-forward
                            camera-projection
                            (swizzle (representation render-parameters) :z)))
         (radiance
           (painted-sky-radiance
            ray camera-position sun-vector sun-color-vector
            zenith-color-vector horizon-color-vector fog-color-vector
            atmosphere-parameters moon-vector celestial-pole-vector
            night-parameters)))
    (set-output color-output (vec4 (representation radiance) 1.0))))

(define-live-shader sky-temporal-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 34))))
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
            ray camera-position sun-vector sun-color-vector
            zenith-color-vector horizon-color-vector fog-color-vector
            atmosphere-parameters moon-vector celestial-pole-vector
            night-parameters))
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
