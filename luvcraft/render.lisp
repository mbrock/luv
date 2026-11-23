;;; Frame uniform packing and texel-snapped shadow transform shared with Luft
;;; presentation adapters (held items, legacy companion fallback, creatures).

(in-package #:luvcraft)

(defconstant +luvcraft-shadow-map-size+ 2048)

(luv.arithmetic:define-quantity-constant
    +luvcraft-shadow-half-extent+ 64.0
  :type single-float
  :quantity (:quantity :world-distance :unit :cell))

(luv.arithmetic:define-quantity-constant
    +luvcraft-shadow-depth-radius+ 96.0
  :type single-float
  :quantity (:quantity :world-distance :unit :cell))

(defparameter *luvcraft-shadow-base-bias* 0.00405
  "Constant depth bias applied to every shadow comparison.")
(defparameter *luvcraft-shadow-slope-bias* 0.002
  "Depth bias scaled by the receiver's slope to the light.")
(defparameter *luvcraft-shadow-minimum-filter-radius* 8.0
  "PCF radius in shadow texels when the sun is a point.")
(defparameter *luvcraft-shadow-maximum-filter-radius* 24.0
  "PCF radius in shadow texels at the sun's widest.")
(defparameter *luvcraft-exposure* 0.45
  "Fixed exposure gain shared by Luvcraft on Luft.")

(defun shadow-frame-rows (camera sky &optional anchor)
  "Pack a texel-stable orthographic light-space transform as four vec4 rows."
  (let* ((center
           (make-vec3 (camera-x camera) (camera-y camera) (camera-z camera)))
         (forward
           (vec3-scale
            (vec3-normalize (sky-frame-parameters-sun-direction sky))
            -1.0))
         (reference (sky-sun-orbit-axis))
         (right (vec3-normalize (vec3-cross reference forward)))
         (up (vec3-cross forward right))
         (extent +luvcraft-shadow-half-extent+)
         (depth-radius +luvcraft-shadow-depth-radius+)
         (world-units-per-texel
           (/ (* 2.0 extent) +luvcraft-shadow-map-size+))
         (anchor (luv.arithmetic.lisp.vec3:snap-plane-origin
                  center anchor right up world-units-per-texel))
         (center-right (vec3-dot anchor right))
         (center-up (vec3-dot anchor up)))
    (flet ((lane (axis scale offset)
             (list (* (vec3-x axis) scale)
                   (* (vec3-y axis) scale)
                   (* (vec3-z axis) scale)
                   (coerce offset 'single-float))))
      (values
       (append
        (lane right (/ extent) (- (/ center-right extent)))
        (lane up (/ extent) (- (/ center-up extent)))
        (lane forward (/ (* 2.0 depth-radius))
              (- 0.5 (/ (vec3-dot anchor forward) (* 2.0 depth-radius))))
        '(0.0 0.0 0.0 1.0))
       anchor))))

(defun frame-uniform-data (session width height &key camera-lanes)
  "Pack the frame environment: camera lanes plus the evaluated sky."
  (let* ((camera-lanes (or camera-lanes
                           (camera-uniform-data
                            (luvcraft-session-camera session) width height)))
         (sky (sky-frame-parameters (luvcraft-session-sky-clock session)
                                    (luvcraft-session-sky-profile session)))
         (data (make-array (+ (length camera-lanes) 56)
                           :element-type 'single-float))
         (index (length camera-lanes)))
    (replace data camera-lanes)
    (flet ((emit (&rest values)
             (dolist (value values)
               (setf (aref data index) (coerce value 'single-float))
               (incf index)))
           (color (vector) (coerce vector 'list)))
      (let ((sun (sky-frame-parameters-sun-direction sky)))
        (emit (sky-frame-parameters-fog-near sky)
              (sky-frame-parameters-fog-far sky)
              (mod (or (luvcraft-session-last-frame-time session) 0d0) 3600d0)
              (sky-frame-parameters-cloudiness sky))
        (emit (vec3-x sun) (vec3-y sun) (vec3-z sun)
              (sky-frame-parameters-day-factor sky))
        (apply #'emit (append (color (sky-frame-parameters-sun-color sky))
                              (list (sky-frame-parameters-sun-angular-width
                                     sky))))
        (apply #'emit (append (color (sky-frame-parameters-zenith-color sky))
                              (list height)))
        (apply #'emit (append (color (sky-frame-parameters-horizon-color sky))
                              (list width)))
        (apply #'emit (append (color (sky-frame-parameters-ambient-color sky))
                              (list (sky-frame-parameters-exposure sky))))
        (apply #'emit
               (append (color (sky-frame-parameters-fog-color sky))
                       (list (if (luvcraft-session-shadow-diagnostic-p session)
                                 1.0
                                 0.0))))
        (emit (/ +luvcraft-shadow-map-size+)
              (/ +luvcraft-shadow-map-size+)
              *luvcraft-shadow-base-bias*
              *luvcraft-shadow-slope-bias*)
        (emit (* 2.0 +luvcraft-shadow-depth-radius+)
              (/ (* 2.0 +luvcraft-shadow-half-extent+)
                 +luvcraft-shadow-map-size+)
              *luvcraft-shadow-minimum-filter-radius*
              *luvcraft-shadow-maximum-filter-radius*)
        (emit (* +block-atlas-tile-size+ *block-atlas-tile-capacity*)
              0.0 0.0 0.0)
        (multiple-value-bind (rows anchor)
            (shadow-frame-rows (luvcraft-session-camera session) sky
                               (luvcraft-session-shadow-anchor session))
          (setf (luvcraft-session-shadow-anchor session) anchor)
          (apply #'emit rows))))
    (unless (= index (length data))
      (error "Frame uniform packing emitted ~D of ~D lanes."
             index (length data)))
    data))
