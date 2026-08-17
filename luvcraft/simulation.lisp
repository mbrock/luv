;;; The first-person view and the scalar reference player simulation.
;;;
;;; Camera mathematics stays small and inspectable: five vec4 values form the
;;; prefix of the frame uniform block.  The player
;;; controller is intentionally a small scalar reference simulation whose
;;; body is distinct from the view camera and whose AABB queries the voxel
;;; lattice directly.  This is the behavior later dense body/contact domains
;;; and SIMD kernels must preserve, not their final storage layout.

(in-package #:luvcraft)

(luv.arithmetic:define-quantity-constant
    +luvcraft-camera-near-distance+ 0.1
  :type single-float
  :quantity (:quantity :view-distance :unit :cell))
(luv.arithmetic:define-quantity-constant
    +luvcraft-camera-far-distance+ 180.0
  :type single-float
  :quantity (:quantity :view-distance :unit :cell))
(luv.arithmetic:define-quantity-constant
    +luvcraft-camera-vertical-field-of-view+ 1.2217305
  :type single-float
  :quantity (:quantity :camera-field-of-view :unit :radian)
  :documentation "The block-world camera's 70-degree vertical field of view.")
(luv.arithmetic:define-quantity-constant
    +luvcraft-camera-focused-vertical-field-of-view+ 0.87266463
  :type single-float
  :quantity (:quantity :camera-field-of-view :unit :radian)
  :documentation "The 50-degree vertical field of view used during modal focus.")

(defconstant +luvcraft-camera-focus-easing-rate+ 12.0)

(defstruct (camera-pose
             (:constructor make-camera-pose
                 (position yaw pitch field-of-view)))
  "One complete, freely movable view pose used by a modal focus transition."
  position
  yaw
  pitch
  field-of-view)

(defclass fly-camera ()
  ((position :initarg :position
             :initform (make-vec3 8.0 11.0 -6.0)
             :type vec3
             :quantity (:quantity :world-position :unit :cell
                        :tensor-order 1)
             :accessor camera-position)
   (yaw :initarg :yaw :initform 0.0 :type real
        :quantity (:quantity :camera-yaw :unit :radian)
        :accessor camera-yaw)
   (pitch :initarg :pitch :initform -0.28 :type real
          :quantity (:quantity :camera-pitch :unit :radian)
          :accessor camera-pitch)
   (field-of-view
    :initarg :field-of-view
    :initform +luvcraft-camera-vertical-field-of-view+
    :type real
    :quantity (:quantity :camera-field-of-view :unit :radian)
    :accessor camera-field-of-view)
   (sensitivity :initarg :sensitivity :initform 0.0025
                :type real
                :quantity (:quantity :look-sensitivity :unit :radian)
                :accessor camera-sensitivity))
  (:metaclass luv.arithmetic.records:quantity-class))

(defun camera-x (camera) (vec3-x (camera-position camera)))
(defun camera-y (camera) (vec3-y (camera-position camera)))
(defun camera-z (camera) (vec3-z (camera-position camera)))
(defun (setf camera-x) (value camera)
  (setf (vec3-x (camera-position camera)) value))
(defun (setf camera-y) (value camera)
  (setf (vec3-y (camera-position camera)) value))
(defun (setf camera-z) (value camera)
  (setf (vec3-z (camera-position camera)) value))

(defgeneric camera-basis (camera))
(defgeneric camera-uniform-data (camera width height))

(defmethod camera-basis ((camera fly-camera))
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (make-vec3 (* (sin yaw) (cos pitch))
                             (sin pitch)
                             (* (cos yaw) (cos pitch))))
         (right (make-vec3 (cos yaw) 0.0 (- (sin yaw))))
         (up (make-vec3 (- (* (sin pitch) (sin yaw)))
                        (cos pitch)
                        (- (* (sin pitch) (cos yaw))))))
    (values right up forward)))

(defun camera-key-down-p (keys &rest names)
  (some (lambda (name) (gethash name keys)) names))

(defmethod camera-uniform-data ((camera fly-camera) width height)
  "The five camera lanes of the frame uniform: position, basis, projection.

The environment lanes which complete the block are packed by
FRAME-UNIFORM-DATA from the session's sky clock and profile."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let* ((near +luvcraft-camera-near-distance+)
           (far +luvcraft-camera-far-distance+)
           (focal (/ (tan (/ (camera-field-of-view camera) 2.0))))
           (aspect (/ (coerce width 'single-float) height))
           (projection
             (make-vec3 (/ focal aspect) focal (/ far (- far near)))))
      (flet ((uniform-lane (vector fourth)
               (list (coerce (vec3-x vector) 'single-float)
                     (coerce (vec3-y vector) 'single-float)
                     (coerce (vec3-z vector) 'single-float)
                     (coerce fourth 'single-float))))
        (let* ((data
                 (make-array
                  20 :element-type 'single-float
                  :initial-contents
                  (append (uniform-lane (camera-position camera) 0.0)
                          (uniform-lane right 0.0)
                          (uniform-lane up 0.0)
                          (uniform-lane forward 0.0)
                          (uniform-lane
                           projection (/ (- (* far near)) (- far near))))))
               (declaration
                 (luv.arithmetic:value-declaration-for
                  :camera-uniform-data)))
          (unless (typep
                   data
                   (luv.arithmetic:declaration-representation-type
                    declaration))
            (error "Camera uniform data ~S does not satisfy ~S."
                   (type-of data)
                   (luv.arithmetic:declaration-representation-type
                    declaration)))
          data)))))

(defun copy-camera-position (position)
  (make-vec3 (vec3-x position) (vec3-y position) (vec3-z position)))

(defun camera-pose-from-camera (camera)
  (make-camera-pose
   (copy-camera-position (camera-position camera))
   (camera-yaw camera)
   (camera-pitch camera)
   (camera-field-of-view camera)))

(defun shortest-angle-difference (target current)
  (- (mod (+ (- target current) pi) (* 2 pi)) pi))

(defun set-camera-pose (camera pose)
  (let ((target (camera-pose-position pose))
        (position (camera-position camera)))
    (setf (vec3-x position) (vec3-x target)
          (vec3-y position) (vec3-y target)
          (vec3-z position) (vec3-z target)
          (camera-yaw camera) (camera-pose-yaw pose)
          (camera-pitch camera) (camera-pose-pitch pose)
          (camera-field-of-view camera) (camera-pose-field-of-view pose)))
  camera)

(defun advance-camera-focus (camera target seconds)
  "Ease CAMERA's full view pose toward TARGET and return the remaining error."
  (let* ((alpha
           (- 1.0
              (exp (- (* +luvcraft-camera-focus-easing-rate+ seconds)))))
         (position (camera-position camera))
         (target-position (camera-pose-position target)))
    (flet ((approach (current target)
             (+ current (* (- target current) alpha))))
      (setf (vec3-x position)
            (approach (vec3-x position) (vec3-x target-position))
            (vec3-y position)
            (approach (vec3-y position) (vec3-y target-position))
            (vec3-z position)
            (approach (vec3-z position) (vec3-z target-position))
            (camera-yaw camera)
            (+ (camera-yaw camera)
               (* (shortest-angle-difference
                   (camera-pose-yaw target) (camera-yaw camera))
                  alpha))
            (camera-pitch camera)
            (approach (camera-pitch camera) (camera-pose-pitch target))
            (camera-field-of-view camera)
            (approach (camera-field-of-view camera)
                      (camera-pose-field-of-view target))))
    (max (vec3-length
          (make-vec3 (- (vec3-x target-position) (vec3-x position))
                     (- (vec3-y target-position) (vec3-y position))
                     (- (vec3-z target-position) (vec3-z position))))
         (abs (shortest-angle-difference
               (camera-pose-yaw target) (camera-yaw camera)))
         (abs (- (camera-pose-pitch target) (camera-pitch camera)))
         (abs (- (camera-pose-field-of-view target)
                 (camera-field-of-view camera))))))

;;; The first player controller is intentionally a small scalar reference
;;; simulation.  Its body is distinct from the view camera, and its AABB
;;; queries the voxel lattice directly.  This is the behavior later dense
;;; body/contact domains and SIMD kernels must preserve, not their final
;;; storage layout.

(luv.arithmetic:define-quantity-constant
    +player-physics-step+ (/ 1d0 120d0)
  :type double-float
  :quantity (:quantity :frame-duration :unit :second))
(luv.arithmetic:define-quantity-constant
    +player-collision-epsilon+ 1d-7
  :type double-float
  :quantity (:quantity :world-distance :unit :cell))
(luv.arithmetic:define-quantity-constant
    +player-step-height+ 1d0
  :type double-float
  :quantity (:quantity :world-distance :unit :cell))
(luv.arithmetic:define-quantity-constant
    +player-terminal-fall-speed+ -50d0
  :type double-float
  :quantity (:quantity :world-velocity
             :unit ((:cell 1) (:second -1))))

(luv.arithmetic.lisp:define-lisp-arithmetic-function
    %predict-world-position
    ((position :quantity :world-position :unit :cell
               :tensor-order 1)
     (velocity :quantity :world-velocity
               :unit ((:cell 1) (:second -1)) :tensor-order 1)
     (elapsed :quantity :frame-duration :unit :second))
  (+ position
     (luv.arithmetic.language:interpret
      (* velocity elapsed)
      :quantity :world-position :unit :cell
      :tensor-order 1 :character :difference)))

(defclass block-world-player ()
  ((position :initarg :position
             :initform (make-vec3 0d0 0d0 0d0)
             :type vec3
             :quantity (:quantity :world-position :unit :cell
                        :tensor-order 1)
             :accessor player-position)
   (velocity :initarg :velocity
             :initform (make-vec3 0d0 0d0 0d0)
             :type vec3
             :quantity (:quantity :world-velocity
                        :unit ((:cell 1) (:second -1))
                        :tensor-order 1)
             :accessor player-velocity)
   (half-width :initarg :half-width :initform 0.30d0
               :type double-float
               :quantity (:quantity :player-half-width :unit :cell)
               :reader player-half-width)
   (height :initarg :height :initform 1.80d0
           :type double-float
           :quantity (:quantity :player-height :unit :cell)
           :reader player-height)
   (eye-height :initarg :eye-height :initform 1.62d0
               :type double-float
               :quantity (:quantity :player-eye-height :unit :cell)
               :reader player-eye-height)
   (walk-speed :initarg :walk-speed :initform 5.0d0
               :type double-float
               :quantity (:quantity :player-walk-speed
                          :unit ((:cell 1) (:second -1)))
               :reader player-walk-speed)
   (ground-acceleration :initarg :ground-acceleration :initform 45d0
                        :type double-float
                        :quantity (:quantity :player-acceleration
                                   :unit ((:cell 1) (:second -2)))
                        :reader player-ground-acceleration)
   (air-acceleration :initarg :air-acceleration :initform 14d0
                     :type double-float
                     :quantity (:quantity :player-acceleration
                                :unit ((:cell 1) (:second -2)))
                     :reader player-air-acceleration)
   (gravity :initarg :gravity :initform 24d0
            :type double-float
            :quantity (:quantity :gravity-magnitude
                       :unit ((:cell 1) (:second -2)))
            :reader player-gravity)
   (jump-speed :initarg :jump-speed :initform 8.0d0
               :type double-float
               :quantity (:quantity :player-jump-speed
                          :unit ((:cell 1) (:second -1)))
               :reader player-jump-speed)
   (grounded-p :initarg :grounded-p :initform nil
               :accessor player-grounded-p))
  (:metaclass luv.arithmetic.records:quantity-class))

(defparameter *player-frame-duration-declaration*
  (luv.arithmetic:make-represented-value-declaration
   :representation-type 'double-float
   :quantity-specification
   (luv.arithmetic:make-declared-quantity-specification
    '(:quantity :frame-duration :unit :second))
   :source-form '(seconds :type double-float
                  :quantity (:frame-duration :unit :second))))

(defparameter *predict-player-position-realization*
  (luv.arithmetic.lisp:make-lisp-arithmetic-realization
   '%predict-world-position
   :parameter-representation-types '(vec3 vec3 double-float)
   :result-representation-type 'vec3))

(defparameter *predict-player-position-function*
  (let ((position
          (luv.arithmetic.records:record-slot-declaration
           'block-world-player 'position))
        (velocity
          (luv.arithmetic.records:record-slot-declaration
           'block-world-player 'velocity)))
    (luv.arithmetic.lisp:bind-lisp-arithmetic-realization
     *predict-player-position-realization*
     (list position velocity *player-frame-duration-declaration*)
     :actual-result-declaration position)))

(defun predict-player-position (player seconds)
  "Predict unobstructed motion through the checked storage boundary. #GZ53LD"
  (check-type player block-world-player)
  (check-type seconds double-float)
  (funcall *predict-player-position-function*
           (player-position player) (player-velocity player) seconds))

(defun player-x (player) (vec3-x (player-position player)))
(defun player-y (player) (vec3-y (player-position player)))
(defun player-z (player) (vec3-z (player-position player)))
(defun player-velocity-x (player) (vec3-x (player-velocity player)))
(defun player-velocity-y (player) (vec3-y (player-velocity player)))
(defun player-velocity-z (player) (vec3-z (player-velocity player)))
(defun (setf player-x) (value player)
  (setf (vec3-x (player-position player)) value))
(defun (setf player-y) (value player)
  (setf (vec3-y (player-position player)) value))
(defun (setf player-z) (value player)
  (setf (vec3-z (player-position player)) value))
(defun (setf player-velocity-x) (value player)
  (setf (vec3-x (player-velocity player)) value))
(defun (setf player-velocity-y) (value player)
  (setf (vec3-y (player-velocity player)) value))
(defun (setf player-velocity-z) (value player)
  (setf (vec3-z (player-velocity player)) value))

(defun make-player-for-camera (camera)
  (make-instance 'block-world-player
                 :position
                 (make-vec3
                  (coerce (camera-x camera) 'double-float)
                  (- (coerce (camera-y camera) 'double-float) 1.62d0)
                  (coerce (camera-z camera) 'double-float))))

(defun sync-camera-to-player (camera player)
  (let ((position (camera-position camera)))
    (setf (vec3-x position) (player-x player)
          (vec3-y position) (+ (player-y player) (player-eye-height player))
          (vec3-z position) (player-z player)))
  camera)

(defun world-terrain-solid-p (world x y z)
  "Treat absent horizontal terrain and the lower world boundary as solid."
  (multiple-value-bind (block status) (world-block-at world x y z)
    (if (eq status :resident)
        (block-solid-p block)
        (let ((height
                (chunk-shape-height
                 (voxel-space-chunk-shape (block-world-space world)))))
          (< y height)))))

;;; A body is the small scalar contact abstraction which every walking thing
;;; in the world shares: an upright axis-aligned box, standing on its own
;;; position, swept one axis at a time against solid voxel cells.  The player
;;; controller remains its reference implementation, and a critter is the same
;;; sweep with different dimensions and a different mind above it, so the
;;; contact code below is written against this protocol rather than against
;;; either participant.

(defgeneric body-position (body)
  (:documentation "BODY's world position at the centre of its base."))
(defgeneric body-velocity (body)
  (:documentation "BODY's world velocity."))
(defgeneric body-half-width (body)
  (:documentation "Half the width of BODY's square horizontal footprint."))
(defgeneric body-height (body)
  (:documentation "How far BODY's box rises above its position."))
(defgeneric body-grounded-p (body)
  (:documentation "Whether BODY rested on solid terrain after its last step."))
(defgeneric (setf body-grounded-p) (grounded-p body))

(defmethod body-position ((body block-world-player)) (player-position body))
(defmethod body-velocity ((body block-world-player)) (player-velocity body))
(defmethod body-half-width ((body block-world-player))
  (player-half-width body))
(defmethod body-height ((body block-world-player)) (player-height body))
(defmethod body-grounded-p ((body block-world-player))
  (player-grounded-p body))
(defmethod (setf body-grounded-p) (grounded-p (body block-world-player))
  (setf (player-grounded-p body) grounded-p))

(defun body-x (body) (vec3-x (body-position body)))
(defun body-y (body) (vec3-y (body-position body)))
(defun body-z (body) (vec3-z (body-position body)))

(defun body-overlap-indices (minimum maximum)
  (values (floor (+ minimum +player-collision-epsilon+))
          (floor (- maximum +player-collision-epsilon+))))

(defun map-body-overlapping-blocks (function body world)
  (let ((half-width (body-half-width body)))
    (multiple-value-bind (minimum-x maximum-x)
        (body-overlap-indices (- (body-x body) half-width)
                             (+ (body-x body) half-width))
      (multiple-value-bind (minimum-y maximum-y)
          (body-overlap-indices (body-y body)
                               (+ (body-y body) (body-height body)))
        (multiple-value-bind (minimum-z maximum-z)
            (body-overlap-indices (- (body-z body) half-width)
                                 (+ (body-z body) half-width))
          (loop for x from minimum-x to maximum-x do
            (loop for y from minimum-y to maximum-y do
              (loop for z from minimum-z to maximum-z
                    when (world-terrain-solid-p world x y z)
                      do (funcall function x y z)))))))))

(defun move-body-axis (body world axis distance)
  "Move BODY along AXIS and clamp its AABB against solid voxel cells."
  (when (zerop distance)
    (return-from move-body-axis nil))
  (let ((position (body-position body))
        (velocity (body-velocity body))
        (collided-p nil))
    (incf (vec3-component position axis) distance)
    (map-body-overlapping-blocks
     (lambda (x y z)
       (let ((coordinate (ecase axis (:x x) (:y y) (:z z))))
         (setf collided-p t
               (vec3-component position axis)
               (if (plusp distance)
                   (min (vec3-component position axis)
                        (- coordinate
                           (ecase axis
                             ((:x :z) (body-half-width body))
                             (:y (body-height body)))
                           +player-collision-epsilon+))
                   (max (vec3-component position axis)
                        (+ coordinate 1d0
                           (ecase axis
                             ((:x :z) (body-half-width body))
                             (:y 0d0))
                           +player-collision-epsilon+))))))
     body world)
    (when collided-p
      (setf (vec3-component velocity axis) 0d0)
      (when (and (eq axis :y) (minusp distance))
        (setf (body-grounded-p body) t)))
    collided-p))

(defun move-toward (value target maximum-change)
  (cond ((< value target) (min target (+ value maximum-change)))
        ((> value target) (max target (- value maximum-change)))
        (t value)))

(defun player-overlaps-block-p (player x y z)
  (and player
       (< (- (player-x player) (player-half-width player)) (1+ x))
       (> (+ (player-x player) (player-half-width player)) x)
       (< (player-y player) (1+ y))
       (> (+ (player-y player) (player-height player)) y)
       (< (- (player-z player) (player-half-width player)) (1+ z))
       (> (+ (player-z player) (player-half-width player)) z)))

(defun body-position-clear-p (body world x y z)
  "Return true when BODY's AABB would be clear standing at X, Y, Z."
  (let ((half-width (body-half-width body)))
    (multiple-value-bind (minimum-x maximum-x)
        (body-overlap-indices (- x half-width) (+ x half-width))
      (multiple-value-bind (minimum-y maximum-y)
          (body-overlap-indices y (+ y (body-height body)))
        (multiple-value-bind (minimum-z maximum-z)
            (body-overlap-indices (- z half-width) (+ z half-width))
          (loop for block-x from minimum-x to maximum-x never
            (loop for block-y from minimum-y to maximum-y thereis
              (loop for block-z from minimum-z to maximum-z
                    thereis (world-terrain-solid-p
                             world block-x block-y block-z)))))))))

(defun step-block-world-player
    (player world camera pressed-keys seconds &key jump-p (sync-camera-p t))
  "Advance the scalar player controller by one small physics step."
  (let* ((yaw (camera-yaw camera))
         (forward-amount
           (- (if (camera-key-down-p pressed-keys :w :up) 1d0 0d0)
              (if (camera-key-down-p pressed-keys :s :down) 1d0 0d0)))
         (right-amount
           (- (if (camera-key-down-p pressed-keys :d :right) 1d0 0d0)
              (if (camera-key-down-p pressed-keys :a :left) 1d0 0d0)))
         (length (sqrt (+ (* forward-amount forward-amount)
                          (* right-amount right-amount))))
         (forward-amount (if (plusp length) (/ forward-amount length) 0d0))
         (right-amount (if (plusp length) (/ right-amount length) 0d0))
         (sprinting-p
           (camera-key-down-p pressed-keys :shift-left :shift-right))
         (speed (* (player-walk-speed player)
                   (if sprinting-p 1.65d0 1d0)))
         (target-x (* speed (+ (* (sin yaw) forward-amount)
                               (* (cos yaw) right-amount))))
         (target-z (* speed (+ (* (cos yaw) forward-amount)
                               (* (- (sin yaw)) right-amount))))
         (acceleration
           (if (player-grounded-p player)
               (player-ground-acceleration player)
               (player-air-acceleration player)))
         (maximum-change (* acceleration seconds))
         (grounded-p (player-grounded-p player)))
    (setf (player-velocity-x player)
          (move-toward (player-velocity-x player) target-x maximum-change)
          (player-velocity-z player)
          (move-toward (player-velocity-z player) target-z maximum-change))
    (when (and jump-p grounded-p)
      (setf (player-velocity-y player) (player-jump-speed player)
            (player-grounded-p player) nil))
    (let* ((distance-x (* (player-velocity-x player) seconds))
           (attempted-x (+ (player-x player) distance-x))
           (collided-x-p (move-body-axis player world :x distance-x))
           (distance-z (* (player-velocity-z player) seconds))
           (attempted-z (+ (player-z player) distance-z))
           (collided-z-p (move-body-axis player world :z distance-z))
           (step-y (+ (player-y player) +player-step-height+))
           (clear-above-x-p
             (and collided-x-p
                  (body-position-clear-p
                   player world attempted-x step-y (player-z player))))
           (clear-above-z-p
             (and collided-z-p
                  (body-position-clear-p
                   player world (player-x player) step-y attempted-z))))
      (when (and grounded-p
                 (not jump-p)
                 (plusp length)
                 (or clear-above-x-p clear-above-z-p))
        (setf (player-velocity-y player) (player-jump-speed player)
              (player-grounded-p player) nil)))
    (decf (player-velocity-y player) (* (player-gravity player) seconds))
    (setf (player-velocity-y player)
          (max +player-terminal-fall-speed+ (player-velocity-y player))
          (player-grounded-p player) nil)
    (move-body-axis player world :y (* (player-velocity-y player) seconds)))
  (when sync-camera-p
    (sync-camera-to-player camera player))
  player)
