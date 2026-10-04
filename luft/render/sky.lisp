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
;;; white so the lens chain has something real to bloom.  Below the horizon
;;; the sky arrives at exactly the aerial colour distant terrain fades into.

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

(define-shader-function painted-sky-radiance
    (ray eye sun-vector sun-color-vector zenith-vector horizon-vector
     fog-vector parameters)
  "Return the HDR sky radiance along RAY before exposure or grading."
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
           (mix scattered (mix hazed cloud-color cloud-reach)
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
                  :members #.(scene-uniform-prefix 30))))
  (let* ((ray (sky-view-ray ndc camera-right camera-up camera-forward
                            camera-projection
                            (swizzle (representation render-parameters) :z)))
         (radiance
           (painted-sky-radiance
            ray camera-position sun-vector sun-color-vector
            zenith-color-vector horizon-color-vector fog-color-vector
            atmosphere-parameters)))
    (set-output color-output (vec4 (representation radiance) 1.0))))

(define-live-shader sky-temporal-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 30))))
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
            atmosphere-parameters))
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
