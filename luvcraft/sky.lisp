;;; The sky environment: a small session clock and a keyframed day profile.
;;;
;;; The environment is continuous and frame-wide: Lisp evaluates the clock and
;;; profile once per frame into SKY-FRAME-PARAMETERS, and shaders evaluate
;;; spatially varying image mathematics from those uniform lanes.  The clock
;;; is a session object with mutable runtime identity worth inspecting; the
;;; profile is ordinary data.  There is deliberately no sky class hierarchy
;;; until more than one actual sky model exists.

(in-package #:luvcraft)

(defclass sky-clock ()
  ((day-fraction
    :initarg :day-fraction :initform 0.40
    :type real
    :quantity (:quantity :day-fraction :unit :one)
    :accessor sky-clock-day-fraction
    :documentation "Time of day in [0,1): 0 midnight, 0.25 sunrise, 0.5 noon.")
   (rate
    :initarg :rate :initform (/ 1.0 600.0)
    :type real
    :quantity (:quantity :sky-cycle-rate :unit :hertz)
    :accessor sky-clock-rate
    :documentation "Day fractions per real second; the default day is 10 minutes.")
   (paused-p
    :initarg :paused-p :initform nil
    :accessor sky-clock-paused-p)
   (pinned-day-fraction
    :initarg :pinned-day-fraction :initform nil
    :type (or null real)
    :quantity (:quantity :day-fraction :unit :one)
    :accessor sky-clock-pinned-day-fraction
    :documentation
    "When set, evaluation reads this fixed time for deterministic capture."))
  (:documentation
   "Mutable time-of-day state owned by a luvcraft session.

SLY can pause it, set a time, change its rate, or pin it without restarting.")
  (:metaclass luv.arithmetic.records:quantity-class))

(defun advance-sky-clock (clock seconds)
  "Advance CLOCK by a bounded frame delta unless it is paused."
  (unless (sky-clock-paused-p clock)
    (setf (sky-clock-day-fraction clock)
          (mod (+ (sky-clock-day-fraction clock)
                  (* (sky-clock-rate clock) seconds))
               1.0)))
  clock)

(defun sky-clock-hour (clock)
  "CLOCK's time of day in hours, 0 to 24."
  (* 24.0 (sky-clock-day-fraction clock)))

(defun (setf sky-clock-hour) (hour clock)
  "Set CLOCK's time of day to HOUR, wrapping around midnight."
  (setf (sky-clock-day-fraction clock) (mod (/ hour 24.0) 1.0))
  hour)

(defun sky-clock-minutes-per-day (clock)
  "How many real minutes one of CLOCK's days lasts."
  (/ 1.0 (* 60.0 (sky-clock-rate clock))))

(defun (setf sky-clock-minutes-per-day) (minutes clock)
  (setf (sky-clock-rate clock) (/ 1.0 (* 60.0 minutes)))
  minutes)

(defun sky-clock-current-day-fraction (clock)
  (or (sky-clock-pinned-day-fraction clock)
      (sky-clock-day-fraction clock)))

;;; A profile is a cyclic sequence of keyframes over the day fraction.
;;; Colours are 3-element single-float vectors; scalars are single floats.

(luv.arithmetic.records:define-quantity-struct
    (sky-keyframe (:constructor make-sky-keyframe))
  (day-fraction 0.0 :type single-float
                :quantity (:quantity :day-fraction :unit :one))
  (zenith-color #(0.0 0.0 0.0) :type (simple-vector 3)
                 :quantity (:quantity :linear-rgb :unit :one
                            :tensor-order 1))
  (horizon-color #(0.0 0.0 0.0) :type (simple-vector 3)
                  :quantity (:quantity :linear-rgb :unit :one
                             :tensor-order 1))
  (sun-color #(0.0 0.0 0.0) :type (simple-vector 3)
             :quantity (:quantity :linear-rgb :unit :one
                        :tensor-order 1))
  (ambient-color #(0.0 0.0 0.0) :type (simple-vector 3)
                  :quantity (:quantity :linear-rgb :unit :one
                             :tensor-order 1))
  (fog-color #(0.0 0.0 0.0) :type (simple-vector 3)
             :quantity (:quantity :linear-rgb :unit :one
                        :tensor-order 1))
  (sun-angular-width 0.006 :type single-float
                     :quantity (:quantity :sun-angular-width :unit :radian))
  (exposure 1.0 :type single-float
            :quantity (:quantity :exposure :unit :one))
  (cloudiness 0.5 :type single-float
              :quantity (:quantity :cloudiness :unit :one))
  (fog-near 0.0 :type single-float
            :quantity (:quantity :view-distance :unit :cell))
  (fog-far 180.0 :type single-float
           :quantity (:quantity :view-distance :unit :cell)))

(defstruct (sky-profile (:constructor %make-sky-profile))
  (keyframes #() :type simple-vector))

(defun make-sky-profile (keyframes)
  "Make a profile from KEYFRAMES, sorted cyclically by day fraction."
  (%make-sky-profile
   :keyframes (sort (coerce keyframes 'simple-vector) #'<
                    :key #'sky-keyframe-day-fraction)))

(defun sky-lerp (from to amount)
  (+ from (* (- to from) amount)))

(defun sky-lerp-color (from to amount)
  (map '(simple-vector 3)
       (lambda (a b) (coerce (sky-lerp a b amount) 'single-float))
       from to))

(defun sky-smoothstep (edge0 edge1 value)
  (let ((normalized (max 0.0 (min 1.0 (/ (- value edge0)
                                         (- edge1 edge0))))))
    (* normalized normalized (- 3.0 (* 2.0 normalized)))))

(defun sky-profile-keyframes-around (profile day-fraction)
  "The keyframes bracketing DAY-FRACTION and the progress between them."
  (let* ((keyframes (sky-profile-keyframes profile))
         (count (length keyframes)))
    (when (zerop count)
      (error "Sky profile has no keyframes."))
    (if (= count 1)
        (values (aref keyframes 0) (aref keyframes 0) 0.0)
        (loop for index below count
              for next-index = (mod (1+ index) count)
              for start = (aref keyframes index)
              for end = (aref keyframes next-index)
              for start-time = (sky-keyframe-day-fraction start)
              for end-time = (sky-keyframe-day-fraction end)
              for span = (mod (- end-time start-time) 1.0)
              for offset = (mod (- day-fraction start-time) 1.0)
              when (and (plusp span) (< offset span))
                return (values start end (/ offset span))
              finally
                 ;; DAY-FRACTION coincides with a keyframe boundary.
                 (cl:return (values (aref keyframes 0)
                                    (aref keyframes 0) 0.0))))))

;;; One evaluated frame of environment state.  CPU trigonometry is right for
;;; a single sun direction; per-pixel sky shaping stays in the shader.

(luv.arithmetic.records:define-quantity-struct
    (sky-frame-parameters (:constructor %make-sky-frame-parameters))
  (day-fraction 0.0 :type single-float
                :quantity (:quantity :day-fraction :unit :one))
  (sun-direction (make-vec3 0.0 1.0 0.0) :type vec3
                 :quantity (:quantity :world-direction :unit :one
                            :tensor-order 1))
  (day-factor 1.0 :type single-float
              :quantity (:quantity :day-factor :unit :one))
  (sun-color #(0.0 0.0 0.0) :type (simple-vector 3)
             :quantity (:quantity :linear-rgb :unit :one
                        :tensor-order 1))
  (sun-angular-width 0.006 :type single-float
                     :quantity (:quantity :sun-angular-width :unit :radian))
  (zenith-color #(0.0 0.0 0.0) :type (simple-vector 3)
                 :quantity (:quantity :linear-rgb :unit :one
                            :tensor-order 1))
  (horizon-color #(0.0 0.0 0.0) :type (simple-vector 3)
                  :quantity (:quantity :linear-rgb :unit :one
                             :tensor-order 1))
  (ambient-color #(0.0 0.0 0.0) :type (simple-vector 3)
                  :quantity (:quantity :linear-rgb :unit :one
                             :tensor-order 1))
  (fog-color #(0.0 0.0 0.0) :type (simple-vector 3)
             :quantity (:quantity :linear-rgb :unit :one
                        :tensor-order 1))
  (exposure 1.0 :type single-float
            :quantity (:quantity :exposure :unit :one))
  (cloudiness 0.5 :type single-float
              :quantity (:quantity :cloudiness :unit :one))
  (fog-near 0.0 :type single-float
            :quantity (:quantity :view-distance :unit :cell))
  (fog-far 180.0 :type single-float
           :quantity (:quantity :view-distance :unit :cell)))

;;; The shader and CPU side share the definition in LUVCRAFT.QUANTITIES.  This
;;; binding checks its ordinary single-float ABI against the actual sky-frame
;;; slots once; calls below receive and return unwrapped numbers.
(defparameter *sky-fog-view-distance-declaration*
  (luv.arithmetic:make-represented-value-declaration
   :representation-type 'single-float
   :quantity-specification
   (luv.arithmetic:make-declared-quantity-specification
    '(:quantity :view-distance :unit :cell))
   :source-form '(view-distance :type single-float
                  :quantity (:quantity :view-distance :unit :cell))))

(defparameter *sky-fog-amount-declaration*
  (luv.arithmetic:make-represented-value-declaration
   :representation-type 'single-float
   :quantity-specification
   (luv.arithmetic:make-declared-quantity-specification
    '(:quantity :fog-amount :unit :one))
   :source-form '(fog-amount :type single-float
                  :quantity (:quantity :fog-amount :unit :one))))

(defparameter *sky-fog-amount-realization*
  (luv.arithmetic.lisp:make-lisp-arithmetic-realization
   'luvcraft.arithmetic:fog-amount-at-view-distance
   :parameter-representation-types
   '(single-float single-float single-float)
   :result-representation-type 'single-float))

(defparameter *sky-fog-amount-function*
  (luv.arithmetic.lisp:bind-lisp-arithmetic-realization
   *sky-fog-amount-realization*
   (list
    *sky-fog-view-distance-declaration*
    (luv.arithmetic.records:record-slot-declaration
     'sky-frame-parameters 'fog-near)
    (luv.arithmetic.records:record-slot-declaration
     'sky-frame-parameters 'fog-far))
   :actual-result-declaration *sky-fog-amount-declaration*))

(defun sky-fog-amount-at-distance (sky view-distance)
  "Return production fog attenuation at VIEW-DISTANCE for evaluated SKY."
  (check-type sky sky-frame-parameters)
  (check-type view-distance single-float)
  (funcall *sky-fog-amount-function*
           view-distance
           (sky-frame-parameters-fog-near sky)
           (sky-frame-parameters-fog-far sky)))

(defparameter *sky-sun-orbit-tilt* 0.28
  "How far the solar orbit leans out of the world X/Y plane.")

(define-knob sun-orbit-tilt
    (:group :sun :quantity (:quantity :sun-orbit-tilt :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.02)
    *sky-sun-orbit-tilt*)

(defun sky-sun-orbit-axis ()
  "The axis the sun revolves around: world Z, because the orbit is a circle
in the X/Y plane displaced along Z.

Anything which needs a stable reference direction transverse to the sun
should use this rather than world up.  The sun's angle to this axis is the
same at every hour, so a basis built from it is uniformly well conditioned;
a basis built from world up degenerates toward the sun's own direction as
the sun climbs."
  (make-vec3 0.0 0.0 1.0))

(defun sky-sun-direction (day-fraction)
  "The unit sun direction: rising at 0.25, zenith-adjacent at 0.5.

The orbit tilts slightly out of the X/Y plane so light stays directional
even at noon."
  (let* ((angle (* 2.0 pi (- day-fraction 0.25)))
         (x (coerce (cos angle) 'single-float))
         (y (coerce (sin angle) 'single-float))
         (z *sky-sun-orbit-tilt*)
         (length (sqrt (+ (* x x) (* y y) (* z z)))))
    (make-vec3 (/ x length)
               (/ y length)
               (coerce (/ z length) 'single-float))))

(defun sky-frame-parameters (clock profile)
  "Evaluate CLOCK against PROFILE once for the coming frame."
  (let* ((day-fraction
           (coerce (sky-clock-current-day-fraction clock) 'single-float))
         (sun-direction (sky-sun-direction day-fraction))
         ;; Day factor follows the sun's elevation with a soft twilight band,
         ;; so direct light fades out while the sun crosses the horizon.
         (day-factor (sky-smoothstep -0.06 0.16
                                     (vec3-y sun-direction))))
    (multiple-value-bind (start end progress)
        (sky-profile-keyframes-around profile day-fraction)
      (%make-sky-frame-parameters
       :day-fraction day-fraction
       :sun-direction sun-direction
       :day-factor (coerce day-factor 'single-float)
       :sun-color (sky-lerp-color (sky-keyframe-sun-color start)
                                  (sky-keyframe-sun-color end) progress)
       :sun-angular-width (sky-lerp (sky-keyframe-sun-angular-width start)
                                    (sky-keyframe-sun-angular-width end)
                                    progress)
       :zenith-color (sky-lerp-color (sky-keyframe-zenith-color start)
                                     (sky-keyframe-zenith-color end) progress)
       :horizon-color (sky-lerp-color (sky-keyframe-horizon-color start)
                                      (sky-keyframe-horizon-color end)
                                      progress)
       :ambient-color (sky-lerp-color (sky-keyframe-ambient-color start)
                                      (sky-keyframe-ambient-color end)
                                      progress)
       :fog-color (sky-lerp-color (sky-keyframe-fog-color start)
                                  (sky-keyframe-fog-color end) progress)
       :exposure (sky-lerp (sky-keyframe-exposure start)
                           (sky-keyframe-exposure end) progress)
       :cloudiness (coerce (sky-lerp (sky-keyframe-cloudiness start)
                                     (sky-keyframe-cloudiness end) progress)
                           'single-float)
       :fog-near (sky-lerp (sky-keyframe-fog-near start)
                           (sky-keyframe-fog-near end) progress)
       :fog-far (sky-lerp (sky-keyframe-fog-far start)
                          (sky-keyframe-fog-far end) progress)))))

(defun make-default-sky-profile ()
  "A clear-day profile whose noon matches the world's original literals."
  (flet ((keyframe (time zenith horizon sun ambient fog
                    &key (width 0.006) (cloudiness 0.5))
           (make-sky-keyframe
            :day-fraction time
            :cloudiness (coerce cloudiness 'single-float)
            :zenith-color (coerce (mapcar (lambda (c)
                                            (coerce c 'single-float))
                                          zenith)
                                  '(simple-vector 3))
            :horizon-color (coerce (mapcar (lambda (c)
                                             (coerce c 'single-float))
                                           horizon)
                                   '(simple-vector 3))
            :sun-color (coerce (mapcar (lambda (c)
                                         (coerce c 'single-float))
                                       sun)
                               '(simple-vector 3))
            :ambient-color (coerce (mapcar (lambda (c)
                                             (coerce c 'single-float))
                                           ambient)
                                   '(simple-vector 3))
            :fog-color (coerce (mapcar (lambda (c)
                                         (coerce c 'single-float))
                                       fog)
                               '(simple-vector 3))
            :sun-angular-width (coerce width 'single-float))))
    (make-sky-profile
     (list
      ;; midnight
      (keyframe 0.00
                '(0.006 0.010 0.032) '(0.020 0.028 0.070) '(0.0 0.0 0.0)
                '(0.055 0.070 0.135) '(0.018 0.026 0.062) :cloudiness 0.45)
      ;; pre-dawn
      (keyframe 0.21
                '(0.022 0.034 0.095) '(0.090 0.080 0.130) '(0.42 0.16 0.07)
                '(0.100 0.110 0.200) '(0.070 0.065 0.100) :width 0.015
                :cloudiness 0.50)
      ;; sunrise
      (keyframe 0.28
                '(0.07 0.19 0.52) '(0.46 0.42 0.56) '(1.55 0.72 0.34)
                '(0.32 0.31 0.42) '(0.50 0.38 0.36) :width 0.015
                :cloudiness 0.56)
      ;; morning
      (keyframe 0.38
                '(0.20 0.44 0.88) '(0.62 0.76 0.95) '(1.10 0.98 0.84)
                '(0.42 0.53 0.76) '(0.50 0.68 0.92) :cloudiness 0.50)
      ;; noon
      (keyframe 0.50
                '(0.16 0.40 0.92) '(0.58 0.75 0.96) '(1.08 1.02 0.92)
                '(0.44 0.56 0.80) '(0.46 0.68 0.94) :cloudiness 0.44)
      ;; afternoon
      (keyframe 0.62
                '(0.20 0.43 0.88) '(0.64 0.76 0.94) '(1.12 0.98 0.82)
                '(0.43 0.53 0.75) '(0.52 0.68 0.90) :cloudiness 0.50)
      ;; sunset
      (keyframe 0.72
                '(0.09 0.17 0.45) '(0.48 0.40 0.50) '(1.65 0.62 0.24)
                '(0.32 0.28 0.36) '(0.54 0.34 0.30) :width 0.015
                :cloudiness 0.58)
      ;; dusk
      (keyframe 0.79
                '(0.030 0.045 0.115) '(0.120 0.105 0.150) '(0.35 0.13 0.06)
                '(0.110 0.110 0.200) '(0.090 0.080 0.115) :width 0.015
                :cloudiness 0.50)
      ;; deep night
      (keyframe 0.90
                '(0.006 0.010 0.032) '(0.020 0.028 0.070) '(0.0 0.0 0.0)
                '(0.055 0.070 0.135) '(0.018 0.026 0.062) :cloudiness 0.45)))))
