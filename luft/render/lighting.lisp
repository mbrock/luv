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
   (cloudiness :initarg :cloudiness :initform 0.5 :accessor light-cloudiness)
   (shadow-half-extent :initarg :shadow-half-extent
                       :reader light-shadow-half-extent)
   (shadow-depth-radius :initarg :shadow-depth-radius
                        :reader light-shadow-depth-radius)
   (shadow-base-bias :initarg :shadow-base-bias
                     :reader light-shadow-base-bias)
   (shadow-filter-radius :initarg :shadow-filter-radius
                         :reader light-shadow-filter-radius))
  (:documentation
   "One inspectable environment light, packed into raw per-frame GPU lanes.

The sky clock rewrites its sun and atmosphere slots every frame from the day
profile; the shadow slots are authored constants."))

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

(defparameter *sky-sunset-azimuth* (make-vec3 0.0 1.0 0.0)
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
   (make-sky-keyframe 17.3 '(0.09 0.17 0.45) '(0.62 0.46 0.46) '(2.10 0.95 0.42)
                      '(0.26 0.25 0.36) '(0.24 0.13 0.09) '(0.62 0.42 0.38) 0.58)
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

(update-light-from-sky *light* *sky-hour*)

(defun light-shadow-rows (light center)
  "Return a texel-stable orthographic world-to-shadow transform.

The first two rows map the square light plane to clip [-1,1].  The third maps
the signed light depth around CENTER to [0,1].  CENTER is snapped in the light
plane so camera translation cannot slide a shadow edge by a fraction of a
texel."
  (let* ((sun (light-sun-direction light))
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
  "Return LIGHT's nine vec4 lanes for the frame uniform ABI."
  (flet ((vec3-lane (value fourth)
           (list (vec3-x value) (vec3-y value)
                 (vec3-z value) fourth)))
    (append
     (vec3-lane (light-sun-direction light) 0.0)
     (coerce (light-sun-color light) 'list)
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

(defparameter *haze-density* 0.0045
  "Aerial-perspective extinction per cell of view distance near the ground.")

(defparameter *haze-height* 0.018
  "Exponential falloff of the haze with world height, per cell.")

(defun atmosphere-uniform-data (light &optional (elapsed 0.0))
  "Return the six vec4 lanes for the sky dome, haze, and lens chain.

ELAPSED drifts the cloud decks; it wraps hourly to stay precise.  The final
lens-extent row is zero here; the renderer owns those images and fills it."
  (flet ((colour (value fourth)
           (list (aref value 0) (aref value 1) (aref value 2) fourth)))
    (append
     (colour (light-zenith-color light) (light-day-factor light))
     (colour (light-horizon-color light) (light-cloudiness light))
     (colour (light-fog-color light) *haze-density*)
     (list *bloom-gain* *bloom-threshold* *shaft-gain* *shaft-decay*)
     (list (mod elapsed 3600.0) *vignette* *paper-grain* *haze-height*)
     (list 0.0 0.0 0.0 0.0))))

;;; ---------------------------------------------------------------------------
;;; Realized torch light
;;;
;;; A torch is authored on a cubical face but emits from the wick of its final
;;; realized surface frame.  This layer translates that continuous point into
;;; the discrete max-plus sources consumed by LUFT's one voxel-light solver.
;;; Geometry, residency, and publication freshness remain owned by their
;;; callers; equal quantized sources intentionally name the same light result.

(deftype realized-light-site-vector ()
  '(simple-array luft:site (*)))

(deftype realized-light-rgb4-vector ()
  '(simple-array (unsigned-byte 12) (*)))

(defstruct (realized-light-seeds
             (:constructor %make-realized-light-seeds (sites lights))
             (:copier nil)
             (:conc-name %realized-light-seeds-))
  "Canonical parallel lanes for positive voxel-light sources.

SITES are strictly increasing positive cell sites.  LIGHTS are nonzero RGB4
values.  Repeated sites have already met by componentwise maximum.  The arrays
are owned by this value and are never mutated after construction."
  (sites (make-array 0 :element-type 'luft:site)
         :type realized-light-site-vector :read-only t)
  (lights (make-array 0 :element-type '(unsigned-byte 12))
          :type realized-light-rgb4-vector :read-only t))

(defstruct (realized-light-stamp
             (:constructor %make-realized-light-stamp
                 (authored-light-provenance authored-light-revision
                  seed-sites seed-lights))
             (:copier nil)
             (:conc-name %realized-light-stamp-))
  "Exact reusable identity of one realized torch-light solve.

AUTHORED-LIGHT-PROVENANCE is the caller's immutable finished-input token and is
compared by identity.  AUTHORED-LIGHT-REVISION names that input's non-torch
source and opacity revision.  The copied seed lanes name the geometry-dependent
torch input.  No hash stands in for any value."
  (authored-light-provenance nil :read-only t)
  (authored-light-revision 0 :type (integer 0 *) :read-only t)
  (seed-sites (make-array 0 :element-type 'luft:site)
              :type realized-light-site-vector :read-only t)
  (seed-lights (make-array 0 :element-type '(unsigned-byte 12))
               :type realized-light-rgb4-vector :read-only t))

(defstruct (realized-light-generation
             (:constructor %make-realized-light-generation (stamp field))
             (:copier nil)
             (:conc-name %realized-light-generation-))
  "One exact realized-source stamp and its immutable solved voxel-light field."
  (stamp nil :type realized-light-stamp :read-only t)
  (field nil :type luft:voxel-light-field :read-only t))

(define-condition unrealizable-torch-light-source (error)
  ((point :initarg :point :reader unrealizable-torch-light-source-point))
  (:report
   (lambda (condition stream)
     (format stream
             "Torch wick ~S has no positive in-domain authored-air light seed."
             (unrealizable-torch-light-source-point condition))))
  (:documentation
   "A realized torch wick is outside the source domain, occluded, or too dim."))

(declaim (inline %realized-light-finite-single-float
                 %quantize-max-plus-light-lane))

(defun %realized-light-finite-single-float (value role)
  (unless (realp value)
    (error "~A must be a real number, not ~S." role value))
  (let ((single (coerce value 'single-float)))
    (unless (and (= single single)
                 (<= (abs single) most-positive-single-float))
      (error "~A is not a finite single float: ~S." role value))
    single))

(defun %point-coordinate (point index role)
  (unless (and (typep point 'sequence) (<= 3 (length point)))
    (error "~A must contain at least three coordinates, not ~S." role point))
  (%realized-light-finite-single-float (elt point index) role))

(defun realized-torch-wick-point
    (origin normal scale &optional (wick-offset 0.5f0))
  "Return the exact single-float flame wick of a realized torch frame.

The result is ORIGIN + WICK-OFFSET * SCALE * NORMAL.  The default offset is
the canonical half-cell torch wick; a renderer may pass its shared flame
constant explicitly.  NORMAL must already be unit length, as required by the
body/flame frame ABI."
  (let* ((origin-x (%point-coordinate origin 0 "Torch-frame origin"))
         (origin-y (%point-coordinate origin 1 "Torch-frame origin"))
         (origin-z (%point-coordinate origin 2 "Torch-frame origin"))
         (normal-x (%point-coordinate normal 0 "Torch-frame normal"))
         (normal-y (%point-coordinate normal 1 "Torch-frame normal"))
         (normal-z (%point-coordinate normal 2 "Torch-frame normal"))
         (scale (%realized-light-finite-single-float scale "Torch-frame scale"))
         (wick-offset
           (%realized-light-finite-single-float
            wick-offset "Torch-frame wick offset"))
         (normal-length-squared
           (+ (* normal-x normal-x)
              (* normal-y normal-y)
              (* normal-z normal-z))))
    (unless (plusp scale)
      (error "Torch-frame scale must be positive, not ~S." scale))
    (unless (<= (abs (- normal-length-squared 1.0f0)) 2.0f-4)
      (error "Torch-frame normal is not unit length: ~S." normal))
    (let ((distance (* wick-offset scale)))
      (make-array
       3 :element-type 'single-float
       :initial-contents
       (list (+ origin-x (* distance normal-x))
             (+ origin-y (* distance normal-y))
             (+ origin-z (* distance normal-z)))))))

(defun %canonical-realized-light-seed-vectors (sites lights)
  "Copy, sort, remove zeroes, and componentwise-coalesce parallel lanes."
  (unless (= (length sites) (length lights))
    (error "Realized-light site and RGB4 lanes differ in length: ~D and ~D."
           (length sites) (length lights)))
  (let* ((capacity (length sites))
         (sorted-sites (make-array capacity :element-type 'luft:site))
         (sorted-lights
           (make-array capacity :element-type '(unsigned-byte 12)))
         (count 0))
    (dotimes (index capacity)
      (let ((site (elt sites index))
            (light (elt lights index)))
        (check-type site luft:site)
        (unless (and (= (luft:site-extent site) luft:+cell-extent+)
                     (luft:site-positive-p site))
          (error "A realized-light seed needs a positive cell site, not ~S."
                 site))
        (check-type light (unsigned-byte 12))
        (unless (zerop light)
          (setf (aref sorted-sites count) site
                (aref sorted-lights count) light)
          (incf count))))
    ;; Source construction is cold and sparse, but typed insertion sorting
    ;; keeps the retained representation and its ordering law conspicuous.
    (loop for index from 1 below count do
      (let ((site (aref sorted-sites index))
            (light (aref sorted-lights index))
            (destination index))
        (loop while (and (plusp destination)
                         (> (aref sorted-sites (1- destination)) site))
              do (setf (aref sorted-sites destination)
                       (aref sorted-sites (1- destination))
                       (aref sorted-lights destination)
                       (aref sorted-lights (1- destination)))
                 (decf destination))
        (setf (aref sorted-sites destination) site
              (aref sorted-lights destination) light)))
    (let ((unique-count 0))
      (dotimes (index count)
        (let ((site (aref sorted-sites index))
              (light (aref sorted-lights index)))
          (if (and (plusp unique-count)
                   (= site (aref sorted-sites (1- unique-count))))
              (setf (aref sorted-lights (1- unique-count))
                    (luft:voxel-light-componentwise-max
                     (aref sorted-lights (1- unique-count)) light))
              (progn
                (setf (aref sorted-sites unique-count) site
                      (aref sorted-lights unique-count) light)
                (incf unique-count)))))
      (let ((canonical-sites
              (make-array unique-count :element-type 'luft:site))
            (canonical-lights
              (make-array unique-count
                          :element-type '(unsigned-byte 12))))
        (replace canonical-sites sorted-sites :end2 unique-count)
        (replace canonical-lights sorted-lights :end2 unique-count)
        (values canonical-sites canonical-lights)))))

(defun make-realized-light-seeds (sites lights)
  "Own canonical typed copies of parallel SITE and packed-RGB4 sequences."
  (multiple-value-bind (canonical-sites canonical-lights)
      (%canonical-realized-light-seed-vectors sites lights)
    (%make-realized-light-seeds canonical-sites canonical-lights)))

(defun realized-light-seeds-count (seeds)
  (check-type seeds realized-light-seeds)
  (length (%realized-light-seeds-sites seeds)))

(defun realized-light-seeds-sites (seeds)
  "Return a copy of SEEDS' strictly increasing positive cell sites."
  (check-type seeds realized-light-seeds)
  (copy-seq (%realized-light-seeds-sites seeds)))

(defun realized-light-seeds-lights (seeds)
  "Return a copy of SEEDS' packed RGB4 lanes."
  (check-type seeds realized-light-seeds)
  (copy-seq (%realized-light-seeds-lights seeds)))

(defun merge-realized-light-seeds (&rest groups)
  "Join GROUPS by site with componentwise RGB4 maximum.

This is the duplicate-torch law: source order cannot change the solved field."
  (dolist (group groups)
    (check-type group realized-light-seeds))
  (let* ((count (reduce #'+ groups :key #'realized-light-seeds-count
                                   :initial-value 0))
         (sites (make-array count :element-type 'luft:site))
         (lights (make-array count :element-type '(unsigned-byte 12)))
         (offset 0))
    (dolist (group groups)
      (let ((group-sites (%realized-light-seeds-sites group))
            (group-lights (%realized-light-seeds-lights group))
            (group-count (realized-light-seeds-count group)))
        (replace sites group-sites :start1 offset)
        (replace lights group-lights :start1 offset)
        (incf offset group-count)))
    (make-realized-light-seeds sites lights)))

(defun %quantize-max-plus-light-lane (level distance)
  "Q(max(0, LEVEL-DISTANCE)), where Q(x)=floor(x+0.5)."
  (check-type level (integer 0 15))
  (let ((remaining (max 0.0d0 (- (coerce level 'double-float) distance))))
    (min luft:+maximum-voxel-light-level+
         (max 0 (floor (+ remaining 0.5d0))))))

(defun %attenuate-realized-light (light distance)
  (check-type light (unsigned-byte 12))
  (luft:pack-voxel-light
   (%quantize-max-plus-light-lane (luft:voxel-light-red light) distance)
   (%quantize-max-plus-light-lane (luft:voxel-light-green light) distance)
   (%quantize-max-plus-light-lane (luft:voxel-light-blue light) distance)))

(defun %map-realized-light-brackets
    (function domain point authored-occupied-p)
  "Call FUNCTION with every in-domain authored-air bracket cell and L1 range."
  (check-type domain luft:world-domain)
  (let* ((point-x (coerce (%point-coordinate point 0 "Light point")
                          'double-float))
         (point-y (coerce (%point-coordinate point 1 "Light point")
                          'double-float))
         (point-z (coerce (%point-coordinate point 2 "Light point")
                          'double-float)))
    (labels ((bracket (coordinate)
               (let ((cell-coordinate (- coordinate 0.5d0)))
                 (values (floor cell-coordinate)
                         (ceiling cell-coordinate))))
             (choices (low high)
               (if (= low high) (list low) (list low high))))
      (multiple-value-bind (x-low x-high) (bracket point-x)
        (multiple-value-bind (y-low y-high) (bracket point-y)
          (multiple-value-bind (z-low z-high) (bracket point-z)
            (dolist (x (choices x-low x-high))
              (dolist (y (choices y-low y-high))
                (dolist (z (choices z-low z-high))
                  (when (and (<= 0 x)
                             (< x (luft:world-domain-x-limit domain))
                             (<= 0 y)
                             (< y (luft:world-domain-y-limit domain))
                             (<= 0 z)
                             (< z luft:+top-z+))
                    (let ((cell
                            (luft:make-site
                             domain x y z luft:+cell-extent+ 1)))
                      (unless (funcall authored-occupied-p cell)
                        (funcall
                         function cell
                         (+ (abs (- point-x (+ x 0.5d0)))
                            (abs (- point-y (+ y 0.5d0)))
                            (abs (- point-z (+ z 0.5d0)))))))))))))))))

(defun realized-torch-light-seeds
    (domain authored-occupied-p wick-point authored-light)
  "Discretize one continuous torch wick for LUFT's max-plus light solver.

Each axis brackets WICK-POINT against cell centers I+0.5, so at most eight
cells are considered.  Only in-domain cells reported as air by the authored
semantic occupancy callback survive.  Each RGB4 lane receives
Q(max(0,L-L1(WICK,CENTER))).  A flat centered torch therefore produces the
exact same single source as the former adjacent-cell authoring path."
  (check-type authored-light (unsigned-byte 12))
  (unless (plusp authored-light)
    (error "A torch needs positive authored RGB4 emission, not ~S."
           authored-light))
  (let ((sites (make-array 8 :element-type 'luft:site))
        (lights (make-array 8 :element-type '(unsigned-byte 12)))
        (count 0))
    (%map-realized-light-brackets
     (lambda (cell distance)
       (let ((light (%attenuate-realized-light authored-light distance)))
         (unless (zerop light)
           (setf (aref sites count) cell
                 (aref lights count) light)
           (incf count))))
     domain wick-point authored-occupied-p)
    (when (zerop count)
      (error 'unrealizable-torch-light-source
             :point (copy-seq wick-point)))
    (make-realized-light-seeds
     (subseq sites 0 count) (subseq lights 0 count))))

(defun voxel-light-at-continuous-point
    (field point authored-occupied-p)
  "Max-plus sample immutable FIELD at a continuous authored-air point.

The same center brackets and quantized L1 law used to seed a realized torch
reconstruct its continuous light cone.  Occupied brackets are excluded by the
authored semantic occupancy callback; no settled field storage is mutated."
  (check-type field luft:voxel-light-field)
  (let ((answer 0))
    (%map-realized-light-brackets
     (lambda (cell distance)
       (setf answer
             (luft:voxel-light-componentwise-max
              answer
              (%attenuate-realized-light
               (luft:voxel-light-at-site field cell) distance))))
     (luft:voxel-light-field-domain field) point authored-occupied-p)
    answer))

(defun realized-torch-self-light
    (field wick-point authored-occupied-p authored-light)
  "Join the continuous field sample at WICK-POINT with the torch's own RGB4.

Self emission is a componentwise lower bound, while unrelated colored sources
may still contribute brighter lanes."
  (check-type authored-light (unsigned-byte 12))
  (luft:voxel-light-componentwise-max
   authored-light
   (voxel-light-at-continuous-point
    field wick-point authored-occupied-p)))

(defun make-realized-light-stamp
    (authored-light-provenance authored-light-revision seeds)
  "Own provenance, revision, and canonical realized seed lanes in a stamp."
  (unless authored-light-provenance
    (error "Realized light needs a non-NIL authored provenance token."))
  (check-type authored-light-revision (integer 0 *))
  (check-type seeds realized-light-seeds)
  (%make-realized-light-stamp
   authored-light-provenance
   authored-light-revision
   (copy-seq (%realized-light-seeds-sites seeds))
   (copy-seq (%realized-light-seeds-lights seeds))))

(defun realized-light-stamp-authored-light-provenance (stamp)
  (check-type stamp realized-light-stamp)
  (%realized-light-stamp-authored-light-provenance stamp))

(defun realized-light-stamp-authored-light-revision (stamp)
  (check-type stamp realized-light-stamp)
  (%realized-light-stamp-authored-light-revision stamp))

(defun realized-light-stamp-seed-sites (stamp)
  "Return a copy of STAMP's exact sorted cell lane."
  (check-type stamp realized-light-stamp)
  (copy-seq (%realized-light-stamp-seed-sites stamp)))

(defun realized-light-stamp-seed-lights (stamp)
  "Return a copy of STAMP's exact packed-RGB4 lane."
  (check-type stamp realized-light-stamp)
  (copy-seq (%realized-light-stamp-seed-lights stamp)))

(defun realized-light-stamp= (left right)
  "Whether two stamps justify reuse of the same realized torch-light solve."
  (and (typep left 'realized-light-stamp)
       (typep right 'realized-light-stamp)
       (eq (%realized-light-stamp-authored-light-provenance left)
           (%realized-light-stamp-authored-light-provenance right))
       (= (%realized-light-stamp-authored-light-revision left)
          (%realized-light-stamp-authored-light-revision right))
       (equalp (%realized-light-stamp-seed-sites left)
               (%realized-light-stamp-seed-sites right))
       (equalp (%realized-light-stamp-seed-lights left)
               (%realized-light-stamp-seed-lights right))))

(defun %realized-light-voxel-sources (authored-sources stamp)
  "Pack authored sources and STAMP's parallel realized lanes for the solver."
  (let* ((authored-count (length authored-sources))
         (sites (%realized-light-stamp-seed-sites stamp))
         (lights (%realized-light-stamp-seed-lights stamp))
         (sources
           (make-array (+ authored-count (length sites))
                       :element-type '(unsigned-byte 64)))
         (offset 0))
    (map nil
         (lambda (source)
           (check-type source (unsigned-byte 64))
           (setf (aref sources offset) source)
           (incf offset))
         authored-sources)
    (dotimes (index (length sites))
      (setf (aref sources offset)
            (luft:make-voxel-light-source
             (aref sites index) (aref lights index)))
      (incf offset))
    sources))

(defun make-realized-light-generation
    (authored-light-provenance authored-light-revision seeds field)
  "Own an exact realized-light stamp beside an already solved immutable FIELD."
  (check-type field luft:voxel-light-field)
  (let ((stamp
          (make-realized-light-stamp
           authored-light-provenance authored-light-revision seeds))
        (domain (luft:voxel-light-field-domain field)))
    (loop for site across (%realized-light-seeds-sites seeds)
          do (luft:checked-site domain site))
    (%make-realized-light-generation stamp field)))

(defun solve-realized-light-generation
    (domain material-cells opacity-table authored-sources
     authored-light-provenance authored-light-revision realized-seeds
     &key (field-revision authored-light-revision))
  "Solve and own one immutable realized torch-light generation.

AUTHORED-SOURCES, AUTHORED-LIGHT-PROVENANCE, and AUTHORED-LIGHT-REVISION name
the caller-owned non-torch light inputs.  REALIZED-SEEDS are the exact
sorted/coalesced torch sources.
FIELD-REVISION is only the legacy scalar carried by VOXEL-LIGHT-FIELD; reuse and
staleness decisions must compare the generation's exact stamp."
  (check-type domain luft:world-domain)
  (unless authored-light-provenance
    (error "Realized light needs a non-NIL authored provenance token."))
  (check-type authored-light-revision (integer 0 *))
  (check-type field-revision (integer 0 *))
  (check-type realized-seeds realized-light-seeds)
  (let* ((stamp
           (make-realized-light-stamp
            authored-light-provenance authored-light-revision realized-seeds))
         (sources (%realized-light-voxel-sources authored-sources stamp))
         (field
           (luft:solve-voxel-light
            domain (material-cell-reader material-cells) opacity-table sources
            :revision field-revision)))
    (%make-realized-light-generation stamp field)))

(defun realized-light-generation-stamp (generation)
  (check-type generation realized-light-generation)
  (%realized-light-generation-stamp generation))

(defun realized-light-generation-field (generation)
  (check-type generation realized-light-generation)
  (%realized-light-generation-field generation))
