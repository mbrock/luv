(in-package #:luft.render)

;;; Lighting has identity at the frame boundary.  The shader sees only the
;;; dense lanes packed from this object once per frame; fragments do not carry
;;; objects or dispatch through the lighting vocabulary.

(defparameter +shadow-map-size+ 1024
  "Resolution of LUFT's single sun-shadow map.")

(defclass light ()
  ((name :initarg :name :reader light-name)
   (sun-direction :initarg :sun-direction :reader light-sun-direction)
   (sun-color :initarg :sun-color :reader light-sun-color)
   (sky-color :initarg :sky-color :reader light-sky-color)
   (ground-color :initarg :ground-color :reader light-ground-color)
   (shadow-half-extent :initarg :shadow-half-extent
                       :reader light-shadow-half-extent)
   (shadow-depth-radius :initarg :shadow-depth-radius
                        :reader light-shadow-depth-radius)
   (shadow-base-bias :initarg :shadow-base-bias
                     :reader light-shadow-base-bias)
   (shadow-filter-radius :initarg :shadow-filter-radius
                         :reader light-shadow-filter-radius))
  (:documentation
   "One inspectable environment light, packed into raw per-frame GPU lanes."))

(defvar *light* nil)

(setf *light*
      (ensure-semantic-instance
       *light* 'light
       :name :late-afternoon
       :sun-direction (vec3:vec3-normalize (vec3:make-vec3 -0.58 0.36 0.73))
       :sun-color #(2.45 1.30 0.48 0.92)
       :sky-color #(0.33 0.52 1.02 1.0)
       :ground-color #(0.50 0.24 0.32 1.0)
       :shadow-half-extent 96.0
       :shadow-depth-radius 160.0
       :shadow-base-bias 0.00075
       :shadow-filter-radius 5.0))

(defun light-shadow-rows (light center)
  "Return a texel-stable orthographic world-to-shadow transform.

The first two rows map the square light plane to clip [-1,1].  The third maps
the signed light depth around CENTER to [0,1].  CENTER is snapped in the light
plane so camera translation cannot slide a shadow edge by a fraction of a
texel."
  (let* ((sun (light-sun-direction light))
         (forward (vec3:vec3-scale sun -1.0))
         (world-up (vec3:make-vec3 0.0 0.0 1.0))
         (right (vec3:vec3-normalize (vec3:vec3-cross world-up forward)))
         (up (vec3:vec3-cross forward right))
         (extent (light-shadow-half-extent light))
         (depth-radius (light-shadow-depth-radius light))
         (world-units-per-texel (/ (* 2.0 extent) +shadow-map-size+))
         (center-right
           (* (round (/ (vec3:vec3-dot center right) world-units-per-texel))
              world-units-per-texel))
         (center-up
           (* (round (/ (vec3:vec3-dot center up) world-units-per-texel))
              world-units-per-texel))
         (center-forward (vec3:vec3-dot center forward)))
    (flet ((row (axis scale offset)
             (list (* (vec3:vec3-x axis) scale)
                   (* (vec3:vec3-y axis) scale)
                   (* (vec3:vec3-z axis) scale)
                   offset)))
      (append
       (row right (/ extent) (- (/ center-right extent)))
       (row up (/ extent) (- (/ center-up extent)))
       (row forward (/ (* 2.0 depth-radius))
            (- 0.5 (/ center-forward (* 2.0 depth-radius))))
       '(0.0 0.0 0.0 1.0)))))

(defun light-uniform-data (light center)
  "Return LIGHT's nine vec4 lanes for the frame uniform ABI."
  (flet ((vec3-lane (value fourth)
           (list (vec3:vec3-x value) (vec3:vec3-y value)
                 (vec3:vec3-z value) fourth)))
    (append
     (vec3-lane (light-sun-direction light) 0.0)
     (coerce (light-sun-color light) 'list)
     (coerce (light-sky-color light) 'list)
     (coerce (light-ground-color light) 'list)
     (light-shadow-rows light center)
     (list (/ +shadow-map-size+) (/ +shadow-map-size+)
           (light-shadow-base-bias light)
           (light-shadow-filter-radius light)))))
