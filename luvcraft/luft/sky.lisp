;;;; Luvcraft's atmosphere is shared source, not a second port. Luft supplies
;;;; the moving night sky and moonlit terrain; its atelier keeps its own sky.
(in-package #:luvcraft.luft.game)

(defun make-luvcraft-light ()
  (make-instance 'render::light :name :luvcraft
                 :shadow-half-extent 96.0 :shadow-depth-radius 160.0
                 :shadow-base-bias 0.0006 :shadow-filter-radius 1.25))

(defmethod render::host-sky-hour ((viewer luvcraft-viewer))
  (luvcraft::sky-clock-hour (viewer-sky-clock viewer)))
(defmethod render::host-light ((viewer luvcraft-viewer))
  (viewer-sky-light viewer))
(defmethod (setf render::host-sky-hour) (value (viewer luvcraft-viewer))
  (setf (luvcraft::sky-clock-hour (viewer-sky-clock viewer)) value))
(defmethod render::host-day-length ((viewer luvcraft-viewer))
  (luvcraft::sky-clock-minutes-per-day (viewer-sky-clock viewer)))
(defmethod (setf render::host-day-length) (value (viewer luvcraft-viewer))
  (setf (luvcraft::sky-clock-minutes-per-day (viewer-sky-clock viewer))
        (coerce value 'double-float)))
(defmethod render::host-freeze-time ((viewer luvcraft-viewer))
  (luvcraft::sky-clock-paused-p (viewer-sky-clock viewer)))
(defmethod (setf render::host-freeze-time) (value (viewer luvcraft-viewer))
  (setf (luvcraft::sky-clock-paused-p (viewer-sky-clock viewer)) value))
(defmethod render::host-sun-orbit-tilt ((viewer luvcraft-viewer))
  (viewer-sun-orbit-tilt viewer))
(defmethod (setf render::host-sun-orbit-tilt) (value (viewer luvcraft-viewer))
  (setf (viewer-sun-orbit-tilt viewer) value))

(defmethod render::advance-viewer-sky ((viewer luvcraft-viewer) seconds)
  (luvcraft::advance-sky-clock (viewer-sky-clock viewer) seconds)
  (incf (viewer-sky-elapsed viewer) seconds))

(defparameter *daylight-look-strength* 1.0
  "Clear, warm daytime art direction. Zero restores the original atmosphere.")

(defun daylight-look-weight (day-factor)
  (* *daylight-look-strength* (luvcraft::sky-smoothstep 0.35 0.85 day-factor)))

(defun apply-daylight-look (light)
  "Tune existing light lanes on the CPU; preserve night and twilight colours."
  (let ((weight (daylight-look-weight (render::light-day-factor light))))
    (flet ((tint (color gains)
             (dotimes (i 3 color)
               (setf (aref color i)
                     (* (aref color i) (+ 1.0 (* weight (- (nth i gains) 1.0))))))))
      (tint (render::light-sun-color light) '(1.22 1.10 0.94))
      (tint (render::light-sky-color light) '(1.24 1.16 1.00))
      (tint (render::light-ground-color light) '(1.24 1.16 1.00))
      (tint (render::light-zenith-color light) '(0.84 1.00 1.12))
      (setf (render::light-cloudiness light)
            (* (render::light-cloudiness light) (- 1.0 (* weight 0.28))))))
  light)

(defun update-luvcraft-light (viewer light &optional parameters)
  "Apply the shared profile, transform its direction, and tune daytime light."
  (let* ((luvcraft::*sky-sun-orbit-tilt* (viewer-sun-orbit-tilt viewer))
         (sky (or parameters (luvcraft::sky-frame-parameters
                              (viewer-sky-clock viewer) (viewer-sky-profile viewer))))
         (sun (luvcraft::sky-frame-parameters-sun-direction sky)))
    (flet ((lane (color) (concatenate 'vector color #(1.0))))
      (setf (render::light-sky-origin-offset light) (viewer-sky-origin-offset viewer)
            (render::light-sun-direction light)
            (vec3:make-vec3 (vec3:vec3-x sun) (vec3:vec3-z sun) (vec3:vec3-y sun))
            (render::light-moon-direction light)
            (vec3:vec3-scale (render::light-sun-direction light) -1.0)
            (render::light-day-factor light) (luvcraft::sky-frame-parameters-day-factor sky)
            (render::light-night-factor light)
            (- 1.0 (luvcraft::sky-smoothstep
                    0.0 0.28 (luvcraft::sky-frame-parameters-day-factor sky)))
            (render::light-sun-color light) (lane (luvcraft::sky-frame-parameters-sun-color sky))
            (render::light-sky-color light) (lane (luvcraft::sky-frame-parameters-ambient-color sky))
            (render::light-ground-color light)
            (map 'vector (lambda (c) (* c 0.4)) (render::light-sky-color light))
            (render::light-zenith-color light) (lane (luvcraft::sky-frame-parameters-zenith-color sky))
            (render::light-horizon-color light) (lane (luvcraft::sky-frame-parameters-horizon-color sky))
            (render::light-fog-color light) (lane (luvcraft::sky-frame-parameters-fog-color sky))
            (render::light-cloudiness light) (luvcraft::sky-frame-parameters-cloudiness sky)
            (render::light-sun-angular-width light) (luvcraft::sky-frame-parameters-sun-angular-width sky))))
  (apply-daylight-look light))

(defmethod render::encode-viewer-frame :around
    ((viewer luvcraft-viewer) encoder surface-texture extent &key inspector-p)
  (declare (ignore encoder surface-texture extent inspector-p))
  (let* ((luvcraft::*sky-sun-orbit-tilt* (viewer-sun-orbit-tilt viewer))
         (sky (luvcraft::sky-frame-parameters (viewer-sky-clock viewer)
                                             (viewer-sky-profile viewer)))
         (daylight (daylight-look-weight (luvcraft::sky-frame-parameters-day-factor sky)))
         (render::*haze-density* (* render::*haze-density* (- 1.0 (* daylight 0.15))))
         (render::*vignette* (* render::*vignette* (- 1.0 (* daylight 0.45))))
         (render::*sky-hour* (* 24.0 (luvcraft::sky-clock-current-day-fraction
                                    (viewer-sky-clock viewer))))
        (render::*sky-elapsed* (viewer-sky-elapsed viewer))
        (render::*sky-sunset-azimuth* (vec3:make-vec3 -1.0 0.0 0.0))
        (render::*light* (viewer-sky-light viewer))
        (render::*sky-light-evaluator*
          (lambda (light hour) (declare (ignore hour))
            (update-luvcraft-light viewer light sky))))
    (call-next-method)))

(in-package #:luft.render.shaders)

(define-shader-function luvcraft-material-character (tone relief world normal distance)
  "Coarse painted flecks and olive foliage, fading before they become shimmer."
  (let* ((grass (< (abs (- relief 1.0)) 0.5))
         (leaves (< (abs (- relief 5.0)) 0.5))
         (snow (< (abs (- relief 7.0)) 0.5))
         (vegetation (if grass 1.0 (if leaves 1.0 0.0)))
         ;; A fixed world grid, like the old atlas texels, not screen-space grain.
         (cell (floor (+ (* world 12.0) (* normal 0.03))))
         (fleck (- (paper-hash cell) 0.5))
         (footprint (* distance 0.0015))
         (detail (- 1.0 (smoothstep 0.025 0.09 footprint)))
         (contrast (if snow 0.08 (mix 0.24 0.70 vegetation)))
         (tint (if grass (vec3 1.12 0.88 0.82)
                   (if leaves (vec3 1.35 0.93 0.80) (vec3 1.0 1.0 1.0)))))
    (let* ((paint (* tone tint))
           (luma (dot paint (vec3 0.2126 0.7152 0.0722)))
           (natural (mix paint (vec3 luma luma luma) (* vegetation 0.12))))
      (* natural (+ 1.0 (* contrast (* fleck detail)))))))

(define-shader-function luvcraft-blocklight (light gain)
  "The original scalar light's warm radiance, with the shared gain knob."
  (* (* light light) (vec3 1.0 0.82 0.58) (/ gain 0.7)))

(define-shader-function luvcraft-presentation-grade (color)
  (assume-quantity
   (luvcraft.shaders::photographic-grade
    (representation color)
    #.luvcraft.shaders::*grade-saturation* #.luvcraft.shaders::*grade-contrast*)
   :quantity quantities:presented-color :unit :one))

(define-shader-function luvcraft-surface-haze (eye point forward density)
  "Original quadratic view-depth fog; the density knob scales its 180-cell range."
  ;; At the engine's default density, keep the original profile's 0..180
  ;; cell fog range. Zero density leaves only the residency fade.
  (let* ((depth (* (max 0.0 (dot (- point eye) forward)) (/ density 0.007))))
    (representation
     (luvcraft.arithmetic:fog-amount-at-view-distance
      (assume-quantity depth :quantity :view-distance :unit :cell)
      (quantity 0.0 :quantity :view-distance :unit :cell)
      (quantity 180.0 :quantity :view-distance :unit :cell)))))

(define-shader-function luvcraft-atmospheric-surface
    (radiance world eye forward divisor character sun fog zenith parameters)
  "Original fog extinction, retaining Luft's shared sky-ground endpoint."
  (let* ((viewer (mix (swizzle character :xyz) (swizzle eye :xyz) divisor))
         (delta (- world viewer))
         (distance (sqrt (dot delta delta)))
         (direction (/ delta (max distance 0.001)))
         (haze (luvcraft-surface-haze viewer world forward (swizzle fog :w)))
         (edge (* divisor (smoothstep 180.0 310.0 distance)))
         (air (aerial-perspective-color (swizzle fog :xyz) direction sun
                                        (swizzle zenith :w))))
    (mix radiance air (max haze edge))))

(define-shader-function luvcraft-cloud-visibility (point sun elapsed cloudiness origin)
  (luvcraft.shaders::cloud-deck-visibility
   (- (swizzle point :xzy) (vec3 origin 0.0 origin))
   (swizzle sun :xzy) elapsed cloudiness))

(define-shader-function luvcraft-sky-radiance
    (ray camera-position sun-vector sun-color-vector zenith-color-vector
     horizon-color-vector fog-color-vector atmosphere-parameters moon-vector
     celestial-pole-vector night-parameters)
  (let* ((ray (representation ray))
         (moon (normalize (swizzle (representation moon-vector) :xyz)))
         (pole (swizzle (representation celestial-pole-vector) :xyz))
         (angle (swizzle (representation celestial-pole-vector) :w))
         (elapsed (swizzle atmosphere-parameters :x))
         (origin (swizzle (representation sun-color-vector) :w))
         (night
           (night-sky-radiance
            ray (celestial-ray ray pole angle) (swizzle ray :z) moon
            (celestial-ray moon pole angle) (representation moon-vector)
            night-parameters elapsed)))
    (luvcraft.shaders::sky-radiance
     (swizzle ray :xzy)
     (vec4 (- (swizzle (representation camera-position) :xzy)
              (vec3 origin 0.0 origin)) 0.0)
     (vec4 (- (swizzle moon :xzy))
           (swizzle (representation zenith-color-vector) :w))
     (vec4 (swizzle (representation sun-color-vector) :xyz)
           (swizzle (representation sun-vector) :w))
     (representation zenith-color-vector) (representation horizon-color-vector)
     (vec4 0.0 180.0 elapsed (swizzle (representation horizon-color-vector) :w))
     night
     (aerial-perspective-color
      (swizzle (representation fog-color-vector) :xyz) ray
      (swizzle (representation sun-vector) :xyz)
      (swizzle (representation zenith-color-vector) :w)))))

(define-live-shader luvcraft-sky-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 34))))
  (let* ((ray (sky-view-ray ndc camera-right camera-up camera-forward
                            camera-projection
                            (swizzle (representation render-parameters) :z))))
    (set-output color-output
                (vec4 (luvcraft-sky-radiance
                       ray camera-position sun-vector sun-color-vector
                       zenith-color-vector horizon-color-vector fog-color-vector
                       atmosphere-parameters moon-vector celestial-pole-vector
                       night-parameters) 1.0))))

(define-live-shader luvcraft-sky-temporal-fragment-specification
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0) (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0
                  :members #.(scene-uniform-prefix 34))))
  (let* ((divisor (swizzle (representation render-parameters) :z))
         (sample-ndc (- ndc (representation (swizzle temporal-parameters :xy))))
         (ray (sky-view-ray sample-ndc camera-right camera-up camera-forward camera-projection divisor))
         (previous-z (representation (dot ray (swizzle previous-camera-forward :xyz))))
         (previous-clip
           (vec4 (* (representation (dot ray (swizzle previous-camera-right :xyz)))
                    (swizzle previous-camera-projection :x))
                 (- (* (representation (dot ray (swizzle previous-camera-up :xyz)))
                       (swizzle previous-camera-projection :y)))
                 0.0 (mix 1.0 previous-z divisor)))
         (current-clip (vec4 (swizzle sample-ndc :x) (swizzle sample-ndc :y) 0.0 1.0)))
    (set-output color-output
                (vec4 (luvcraft-sky-radiance
                       ray camera-position sun-vector sun-color-vector
                       zenith-color-vector horizon-color-vector fog-color-vector
                       atmosphere-parameters moon-vector celestial-pole-vector
                       night-parameters) 1.0))
    (set-output motion-output (mesh-temporal-motion previous-clip current-clip))))

(in-package #:luvcraft.luft.game)

(defun make-luvcraft-image-finishing (device color-format)
  "Reuse the original photographic grade without the atelier's paper medium."
  (let ((luft.render.shaders::*presentation-grade-function*
          'luft.render.shaders::luvcraft-presentation-grade)
        (luft.render.shaders::*presentation-paper-p* nil))
    (render::make-image-finishing device color-format)))

(defun make-luvcraft-exposure (device)
  (declare (ignore device))
  (render:make-fixed-exposure luvcraft::*luvcraft-exposure*))

(defparameter *rounded-terrain-edges-p* t
  "Subtle normal-only rims on plain terrain; rebuild shaders after changing.")

(defun make-luvcraft-terrain-drawing (device target-formats sample-count)
  (let ((luft.render.shaders::*terrain-edge-shaping-p*
          (and *rounded-terrain-edges-p* (not render::*terrain-bevels-p*)))
        (luft.render.shaders::*terrain-material-character-function*
          'luft.render.shaders::luvcraft-material-character)
        (luft.render.shaders::*terrain-blocklight-function*
          'luft.render.shaders::luvcraft-blocklight)
        (luft.render.shaders::*terrain-cloud-visibility-function*
          'luft.render.shaders::luvcraft-cloud-visibility)
        (luft.render.shaders::*surface-atmosphere-function*
          'luft.render.shaders::luvcraft-atmospheric-surface))
    (render:make-terrain-drawing
     device target-formats sample-count
     :fragment (luft.render.shaders:star-fragment-specification))))

(defun make-luvcraft-torch-drawing (device target-formats sample-count)
  (let ((luft.render.shaders::*surface-atmosphere-function*
          'luft.render.shaders::luvcraft-atmospheric-surface))
    (render::make-framed-torch-drawing device target-formats sample-count)))

(defun make-luvcraft-sky-drawing (device target-formats sample-count)
  (render::make-pipeline-scene-drawing
   'render::sky-drawing device :label "Luvcraft shared atmosphere"
   :vertex (luft.render.shaders::present-vertex-specification)
   :fragment (if (rest target-formats)
                 (luft.render.shaders::luvcraft-sky-temporal-fragment-specification)
                 (luft.render.shaders::luvcraft-sky-fragment-specification))
   :targets (mapcar (lambda (format) `(:format ,format)) target-formats)
   :sample-count sample-count :depth-compare :always :vertex-count 3))
