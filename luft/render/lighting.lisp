(in-package #:luft.render)

;;; Lighting has identity at the frame boundary.  The shader sees only the
;;; dense lanes packed from this object once per frame; fragments do not carry
;;; objects or dispatch through the lighting vocabulary.

(defparameter +shadow-map-size+ 1024
  "Resolution of LUFT's single sun-shadow map.")

(defclass light ()
  ((name :initarg :name :reader light-name)
   (sun-direction :initarg :sun-direction :accessor light-sun-direction)
   (sun-color :initarg :sun-color :accessor light-sun-color)
   (sky-color :initarg :sky-color :accessor light-sky-color
              :documentation "Ambient radiance arriving from the upper hemisphere.")
   (ground-color :initarg :ground-color :accessor light-ground-color
                 :documentation "Ambient radiance bounced up from the ground.")
   (zenith-color :initarg :zenith-color :initform #(0.16 0.40 0.92)
                 :accessor light-zenith-color)
   (horizon-color :initarg :horizon-color :initform #(0.58 0.75 0.96)
                  :accessor light-horizon-color)
   (fog-color :initarg :fog-color :initform #(0.46 0.68 0.94)
              :accessor light-fog-color
              :documentation "The colour arbitrarily distant land fades into.")
   (day-factor :initarg :day-factor :initform 1.0 :accessor light-day-factor)
   (moon-direction :initarg :moon-direction
                   :initform (make-vec3 0.72 -0.43 -0.62)
                   :accessor light-moon-direction)
   (night-factor :initarg :night-factor :initform 0.0
                 :accessor light-night-factor
                 :documentation "How far into the night the sky is, 0 to 1.")
   (cloudiness :initarg :cloudiness :initform 0.5 :accessor light-cloudiness)
   (shadow-half-extent :initarg :shadow-half-extent
                       :reader light-shadow-half-extent)
   (shadow-depth-radius :initarg :shadow-depth-radius
                        :reader light-shadow-depth-radius)
   (shadow-base-bias :initarg :shadow-base-bias
                     :accessor light-shadow-base-bias)
   (shadow-filter-radius :initarg :shadow-filter-radius
                         :accessor light-shadow-filter-radius))
  (:documentation
   "One inspectable environment light, packed into raw per-frame GPU lanes.

The sky clock rewrites its sun, moon, and atmosphere slots every frame from
the day profile; the shadow slots are authored constants.  The world is lit
by one key light at a time -- the sun by day, the moon by night -- and the
shadow map follows whichever it is (LIGHT-KEY-DIRECTION)."))

(defvar *light* nil)

(setf *light*
      (ensure-semantic-instance
       *light* 'light
       :name :day
       :sun-direction (vec3-normalize (make-vec3 -0.72 0.43 0.62))
       :sun-color #(1.85 1.62 1.30 1.0)
       :sky-color #(0.30 0.42 0.62 1.0)
       :ground-color #(0.22 0.17 0.12 1.0)
       :shadow-half-extent 96.0
       :shadow-depth-radius 160.0
       :shadow-base-bias 0.0006
       :shadow-filter-radius 1.25))

;;; ---------------------------------------------------------------------------
;;; The day clock
;;;
;;; Luft's sky is one continuous environment evaluated once per frame: a clock
;;; gives the hour, a cyclic keyframe profile gives the colours, and the sun
;;; moves on a tilted circle so it is never quite overhead.  This is Luvcraft's
;;; sky model (luvcraft/sky.lisp) restated for Luft's Z-up lattice, with the
;;; colours re-balanced for paper: a softer, warmer sun and an airy blue
;;; ambient so that lit and shaded faces stay distinct planes of colour.

(defparameter *sky-hour* 15.5
  "Time of day in hours, 0 to 24.  Half past three is a warm afternoon.")

(defparameter *sky-minutes-per-day* nil
  "Real minutes per full day while the clock runs, or NIL for a still sky.")

(defparameter *sky-sun-orbit-tilt* 0.44
  "How far the solar orbit leans out of the vertical plane.")

(defparameter *sky-sunset-azimuth* (make-vec3 -0.515 0.857 0.0)
  "The horizontal direction the sun sets toward; it rises opposite.")

(defvar *sky-elapsed* 0.0
  "Real seconds the sky has run; drifts the cloud decks.")

(defun advance-sky-clock (seconds)
  "Advance *SKY-HOUR* by SECONDS of real time while the clock runs."
  (setf *sky-elapsed* (mod (+ *sky-elapsed* seconds) 3600.0))
  (when (and *sky-minutes-per-day* (plusp *sky-minutes-per-day*))
    (setf *sky-hour*
          (mod (+ *sky-hour* (/ (* 24.0 seconds)
                                (* 60.0 *sky-minutes-per-day*)))
               24.0)))
  *sky-hour*)

(defun sky-sun-direction (hour)
  "The unit sun direction at HOUR: rising at six, highest at noon."
  (let* ((angle (coerce (* 2.0 pi (- (/ hour 24.0) 0.25)) 'single-float))
         (setting *sky-sunset-azimuth*)
         ;; The tilt axis is horizontal and across the orbit.
         (across (vec3-cross (make-vec3 0.0 0.0 1.0) setting))
         (along (- (cos angle)))
         (height (sin angle))
         (tilt *sky-sun-orbit-tilt*))
    (vec3-normalize
     (make-vec3 (+ (* along (vec3-x setting)) (* tilt (vec3-x across)))
                (+ (* along (vec3-y setting)) (* tilt (vec3-y across)))
                (coerce height 'single-float)))))

(defstruct (sky-keyframe (:constructor make-sky-keyframe
                             (hour zenith horizon sun sky ground fog
                              &optional (cloudiness 0.5))))
  "One hour's atmosphere: dome colours, sun, two ambient lobes, and haze."
  hour zenith horizon sun sky ground fog cloudiness)

(defparameter *sky-profile*
  (list
   ;;                  hour  zenith              horizon             sun
   ;;                        sky ambient         ground bounce       fog
   (make-sky-keyframe 0.0  '(0.006 0.010 0.032) '(0.020 0.028 0.070) '(0.0 0.0 0.0)
                      '(0.050 0.065 0.130) '(0.020 0.020 0.030) '(0.018 0.026 0.062) 0.45)
   (make-sky-keyframe 5.0  '(0.022 0.034 0.095) '(0.090 0.080 0.130) '(0.42 0.16 0.07)
                      '(0.090 0.100 0.180) '(0.050 0.040 0.050) '(0.070 0.065 0.100) 0.50)
   (make-sky-keyframe 6.7  '(0.07 0.19 0.52) '(0.52 0.44 0.52) '(1.75 0.86 0.45)
                      '(0.26 0.27 0.38) '(0.20 0.13 0.10) '(0.50 0.40 0.40) 0.56)
   (make-sky-keyframe 9.0  '(0.18 0.40 0.86) '(0.60 0.74 0.92) '(1.80 1.60 1.32)
                      '(0.22 0.33 0.58) '(0.24 0.17 0.11) '(0.52 0.66 0.86) 0.50)
   (make-sky-keyframe 12.0 '(0.16 0.38 0.88) '(0.58 0.74 0.94) '(1.85 1.72 1.50)
                      '(0.22 0.34 0.62) '(0.25 0.18 0.12) '(0.50 0.66 0.88) 0.44)
   (make-sky-keyframe 15.0 '(0.17 0.38 0.84) '(0.66 0.74 0.88) '(1.95 1.60 1.18)
                      '(0.22 0.33 0.58) '(0.27 0.18 0.11) '(0.60 0.66 0.80) 0.50)
   (make-sky-keyframe 17.3 '(0.09 0.17 0.45) '(0.76 0.53 0.38) '(2.10 0.95 0.42)
                      '(0.26 0.25 0.36) '(0.24 0.13 0.09) '(0.68 0.47 0.37) 0.58)
   (make-sky-keyframe 19.0 '(0.030 0.045 0.115) '(0.120 0.105 0.150) '(0.35 0.13 0.06)
                      '(0.100 0.100 0.180) '(0.050 0.035 0.040) '(0.090 0.080 0.115) 0.50)
   (make-sky-keyframe 21.6 '(0.006 0.010 0.032) '(0.020 0.028 0.070) '(0.0 0.0 0.0)
                      '(0.050 0.065 0.130) '(0.020 0.020 0.030) '(0.018 0.026 0.062) 0.45))
  "Cyclic keyframes over the day, sorted by hour.")

(defun sky-keyframes-around (hour)
  "Return the bracketing keyframes of *SKY-PROFILE* and the progress between."
  (let* ((keys *sky-profile*)
         (count (length keys)))
    (loop for index below count
          for start = (nth index keys)
          for end = (nth (mod (1+ index) count) keys)
          for span = (mod (- (sky-keyframe-hour end) (sky-keyframe-hour start)) 24.0)
          for offset = (mod (- hour (sky-keyframe-hour start)) 24.0)
          when (and (plusp span) (< offset span))
            return (values start end (/ offset span))
          finally (return (values (first keys) (first keys) 0.0)))))

(defun %sky-smoothstep (edge0 edge1 value)
  (let ((x (max 0.0 (min 1.0 (/ (- value edge0) (- edge1 edge0))))))
    (* x x (- 3.0 (* 2.0 x)))))

(defun update-light-from-sky (light hour)
  "Rewrite LIGHT's sun and atmosphere for HOUR from *SKY-PROFILE*."
  (multiple-value-bind (start end progress) (sky-keyframes-around hour)
    (flet ((blend (reader &optional (fourth 1.0))
             (let ((from (funcall reader start)) (to (funcall reader end)))
               (concatenate
                'vector
                (mapcar (lambda (a b) (coerce (+ a (* (- b a) progress)) 'single-float))
                        from to)
                (list fourth)))))
      (let ((sun (sky-sun-direction hour)))
        (setf (light-sun-direction light) sun
              (light-moon-direction light) (vec3-scale sun -1.0)
              (light-night-factor light)
              (coerce (%sky-smoothstep -0.03 -0.26 (vec3-z sun)) 'single-float)
              (light-day-factor light)
              (coerce (%sky-smoothstep -0.06 0.16 (vec3-z sun)) 'single-float)
              (light-sun-color light) (blend #'sky-keyframe-sun)
              (light-sky-color light) (blend #'sky-keyframe-sky)
              (light-ground-color light) (blend #'sky-keyframe-ground)
              (light-zenith-color light) (blend #'sky-keyframe-zenith)
              (light-horizon-color light) (blend #'sky-keyframe-horizon)
              (light-fog-color light) (blend #'sky-keyframe-fog)
              (light-cloudiness light)
              (coerce (+ (sky-keyframe-cloudiness start)
                         (* (- (sky-keyframe-cloudiness end)
                               (sky-keyframe-cloudiness start))
                            progress))
                      'single-float)))))
  light)

(defun current-light ()
  "Return *LIGHT* evaluated at the current sky hour."
  (update-light-from-sky *light* *sky-hour*))

;;; ---------------------------------------------------------------------------
;;; The night
;;;
;;; Luvcraft's moon rides opposite the sun and is always full, so it is up for
;;; exactly the hours the sun is not.  The stars are fixed to the same turning
;;; sky: the sun's orbit is a rotation about one horizontal celestial pole, and
;;; the shader turns each view ray back by that rotation before it looks the
;;; stars up, so the constellations wheel overhead through the night.
;;;
;;; The moon also lights the world.  Rather than a second direct term, the key
;;; light (the sun lanes of the frame uniform, and the shadow map) passes from
;;; the sun to the moon while both stand at the horizon, where each one's own
;;; light has faded to nothing; so the handover cannot be seen, and moonlit
;;; ridges cast moon shadows.  Everything in the sky that is drawn from the sun
;;; lanes is weighted by the day factor, which is zero by then, except the
;;; cloud deck's self-shadow: at night the moon lights the clouds' faces.

(defparameter *star-brightness* 1.7
  "Radiance of a first-magnitude star, above display white.")

(defparameter *moon-radiance* 1.4
  "Radiance of the full moon's disc, above white so the bloom catches it.")

(defparameter *moon-angular-radius* 0.026
  "The drawn moon's angular radius in radians: a few times the true one.")

(defparameter *galaxy-strength* 0.35
  "Strength of the galaxy band behind the stars.")

(defparameter *moonlight-color* #(0.085 0.105 0.165)
  "Direct radiance of a high full moon: cool, and far dimmer than the sun.")

(defun sky-celestial-pole ()
  "The horizontal axis the sky turns about, across the sun's orbit."
  (vec3-normalize (vec3-cross (make-vec3 0.0 0.0 1.0) *sky-sunset-azimuth*)))

(defun sky-rotation-angle (hour)
  "The sky's turn about its pole at HOUR, in radians; zero at sunrise."
  (coerce (* 2.0 pi (- (/ hour 24.0) 0.25)) 'single-float))

(defun light-key-sun-p (light)
  "Whether the sun, rather than the moon, is LIGHT's key light."
  (> (vec3-z (light-sun-direction light)) -0.06))

(defun light-key-direction (light)
  "The direction of the body lighting the world: the sun, or the moon."
  (if (light-key-sun-p light)
      (light-sun-direction light)
      (light-moon-direction light)))

(defun light-key-color (light)
  "The key light's radiance, faded out at the horizon handover."
  (flet ((scaled (colour amount)
           (vector (* (aref colour 0) amount) (* (aref colour 1) amount)
                   (* (aref colour 2) amount) 1.0)))
    (if (light-key-sun-p light)
        (scaled (light-sun-color light)
                (coerce (%sky-smoothstep
                         -0.06 0.0 (vec3-z (light-sun-direction light)))
                        'single-float))
        (scaled *moonlight-color*
                (coerce (%sky-smoothstep
                         0.06 0.22 (vec3-z (light-moon-direction light)))
                        'single-float)))))

(update-light-from-sky *light* *sky-hour*)

(defun light-shadow-rows (light center)
  "Return a texel-stable orthographic world-to-shadow transform.

The first two rows map the square light plane to clip [-1,1].  The third maps
the signed light depth around CENTER to [0,1].  CENTER is snapped in the light
plane so camera translation cannot slide a shadow edge by a fraction of a
texel."
  (let* ((sun (light-key-direction light))
         (forward (vec3-scale sun -1.0))
         (world-up (make-vec3 0.0 0.0 1.0))
         (right (vec3-normalize (vec3-cross world-up forward)))
         (up (vec3-cross forward right))
         (extent (light-shadow-half-extent light))
         (depth-radius (light-shadow-depth-radius light))
         (world-units-per-texel (/ (* 2.0 extent) +shadow-map-size+))
         (center-right
           (* (round (/ (vec3-dot center right) world-units-per-texel))
              world-units-per-texel))
         (center-up
           (* (round (/ (vec3-dot center up) world-units-per-texel))
              world-units-per-texel))
         (center-forward (vec3-dot center forward)))
    (flet ((row (axis scale offset)
             (list (* (vec3-x axis) scale)
                   (* (vec3-y axis) scale)
                   (* (vec3-z axis) scale)
                   offset)))
      (append
       (row right (/ extent) (- (/ center-right extent)))
       (row up (/ extent) (- (/ center-up extent)))
       (row forward (/ (* 2.0 depth-radius))
            (- 0.5 (/ center-forward (* 2.0 depth-radius))))
       '(0.0 0.0 0.0 1.0)))))

(defun light-uniform-data (light center &optional (exposure 1.0f0))
  "Return LIGHT's nine vec4 lanes for the frame uniform ABI.

The sun lanes carry the key light: the sun by day and the moon by night."
  (flet ((vec3-lane (value fourth)
           (list (vec3-x value) (vec3-y value)
                 (vec3-z value) fourth)))
    (append
     (vec3-lane (light-key-direction light) 0.0)
     (coerce (light-key-color light) 'list)
     (list (aref (light-sky-color light) 0)
           (aref (light-sky-color light) 1)
           (aref (light-sky-color light) 2)
           exposure)
     (coerce (light-ground-color light) 'list)
     (light-shadow-rows light center)
     (list (/ +shadow-map-size+) (/ +shadow-map-size+)
           (light-shadow-base-bias light)
           (light-shadow-filter-radius light)))))

;;; The lens and atmosphere.  Bloom and shafts run on exposed scene-linear
;;; light after temporal reconstruction; the grade follows in presentation.

(defparameter *bloom-gain* 0.16
  "How much of the blurred bright-pass image is added back in exposed light.")

(defparameter *bloom-threshold* 1.1
  "Exposed luminance at which a pixel starts feeding the bloom chain.")

(defparameter *shaft-gain* 0.28
  "Strength of the radial sun shafts gathered from the bright image.")

(defparameter *shaft-decay* 0.955
  "Per-tap attenuation along a sun shaft; nearer one reaches further.")

(defparameter *vignette* 0.22
  "Corner falloff of the presented frame, as a fraction of full brightness.")

(defparameter *paper-grain* 0.035
  "Strength of the fixed paper fibre texture laid over the graded frame.")

(defparameter *haze-density* 0.007
  "Aerial-perspective extinction per cell of view distance near the ground.")

(defparameter *haze-height* 0.018
  "Exponential falloff of the haze with world height, per cell.")

;;; Knobs belong to the values packed below, not to a UI vocabulary.
(math:define-unit 'sky-hour :reference :second :magnitude 3600
                          :quantity-kind :duration)
(math:define-quantity-kind 'inverse-spatial-coordinate
  :dimension nil :parent :dimensionless)
(math:define-quantity 'haze-extinction :kind 'inverse-spatial-coordinate
  :non-negative-p t)
(math:define-quantity 'haze-height-falloff :kind 'inverse-spatial-coordinate
  :non-negative-p t)
(defmethod knobs:unit-abbreviation ((unit (eql 'sky-hour))) "h")
(defmethod knobs:unit-abbreviation ((unit (eql quantities:cell))) "cell")

(knobs:define-knob time-of-day
    (:group :sky :quantity (:quantity :duration :unit sky-hour)
     :minimum 0.0 :maximum 24.0 :step 0.25)
  *sky-hour*)
(knobs:define-knob star-brightness
    (:group :sky :quantity (:quantity quantities:scene-radiance :unit :one)
     :minimum 0.0 :maximum 6.0 :step 0.1)
  *star-brightness*)
(knobs:define-knob galaxy-strength
    (:group :sky :quantity (:quantity :proportion :unit :one)
     :minimum 0.0 :maximum 1.5 :step 0.05)
  *galaxy-strength*)
(knobs:define-knob moon-radiance
    (:group :sky :quantity (:quantity quantities:scene-radiance :unit :one)
     :minimum 0.0 :maximum 8.0 :step 0.1)
  *moon-radiance*)
(knobs:define-knob bloom-gain
    (:group :grading :quantity (:quantity :proportion :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.02)
  *bloom-gain*)
(knobs:define-knob bloom-threshold
    (:group :grading :quantity (:quantity quantities:scene-luminance :unit :one)
     :minimum 0.2 :maximum 4.0 :step 0.1)
  *bloom-threshold*)
(knobs:define-knob shaft-gain
    (:group :grading :label "sun shafts"
     :quantity (:quantity :proportion :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.02)
  *shaft-gain*)
(knobs:define-knob shaft-decay
    (:group :grading :label "shaft reach"
     :quantity (:quantity :proportion :unit :one)
     :minimum 0.8 :maximum 0.995 :step 0.005)
  *shaft-decay*)
(knobs:define-knob vignette
    (:group :grading :quantity (:quantity :proportion :unit :one)
     :minimum 0.0 :maximum 1.0 :step 0.02)
  *vignette*)
(knobs:define-knob paper-grain
    (:group :grading :quantity (:quantity :proportion :unit :one)
     :minimum 0.0 :maximum 0.15 :step 0.005)
  *paper-grain*)
(knobs:define-knob haze-density
    (:group :sky :quantity (:quantity haze-extinction :unit ((quantities:cell -1)))
     :minimum 0.0 :maximum 0.03 :step 0.001)
  *haze-density*)
(knobs:define-knob haze-height
    (:group :sky :label "haze falloff"
     :quantity (:quantity haze-height-falloff :unit ((quantities:cell -1)))
     :minimum 0.0 :maximum 0.08 :step 0.002)
  *haze-height*)
(knobs:define-knob shadow-base-bias
    (:group :shadows :quantity (:quantity quantities:shadow-bias :unit :one)
     :minimum 0.0 :maximum 0.005 :step 0.0001)
  (light-shadow-base-bias *light*))
(knobs:define-knob shadow-filter-radius
    (:group :shadows
     :quantity (:quantity quantities:shadow-filter-radius :unit :one)
     :unit-label " texels" :minimum 0.0 :maximum 5.0 :step 0.25)
  (light-shadow-filter-radius *light*))

(defun atmosphere-uniform-data
    (light &optional (elapsed 0.0) (hour *sky-hour*))
  "Return the nine vec4 lanes for the sky dome, haze, lens chain, and night.

ELAPSED drifts the cloud decks and twinkles the stars; it wraps hourly to stay
precise.  The lens-extent row is zero here; the renderer owns those images and
fills it.  HOUR turns the star field with the sun and moon."
  (flet ((colour (value fourth)
           (list (aref value 0) (aref value 1) (aref value 2) fourth)))
    (append
     (colour (light-zenith-color light) (light-day-factor light))
     (colour (light-horizon-color light) (light-cloudiness light))
     (colour (light-fog-color light) *haze-density*)
     (list *bloom-gain* *bloom-threshold* *shaft-gain* *shaft-decay*)
     (list (mod elapsed 3600.0) *vignette* *paper-grain* *haze-height*)
     (list 0.0 0.0 0.0 0.0)
     (let ((moon (light-moon-direction light)))
       (list (vec3-x moon) (vec3-y moon) (vec3-z moon)
             (light-night-factor light)))
     (let ((pole (sky-celestial-pole)))
       (list (vec3-x pole) (vec3-y pole) (vec3-z pole)
             (sky-rotation-angle hour)))
     (list *star-brightness* *moon-radiance* *moon-angular-radius*
           *galaxy-strength*))))
