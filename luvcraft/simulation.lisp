;;; The first-person view and the scalar reference player simulation.
;;;
;;; Camera mathematics stays small and inspectable: six vec4 values are
;;; written directly into a std140-compatible uniform block.  The player
;;; controller is intentionally a small scalar reference simulation whose
;;; body is distinct from the view camera and whose AABB queries the voxel
;;; lattice directly.  This is the behavior later dense body/contact domains
;;; and SIMD kernels must preserve, not their final storage layout.

(in-package #:luv)

(defclass fly-camera ()
  ((position :initarg :position
             :initform (make-vec3 8.0 11.0 -6.0)
             :accessor camera-position)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform -0.28 :accessor camera-pitch)
   (sensitivity :initarg :sensitivity :initform 0.0025
                :accessor camera-sensitivity)))

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
    (let* ((near 0.1)
           (far 180.0)
           (focal (/ (tan (/ (* 70.0 (/ pi 180.0)) 2.0))))
           (aspect (/ (coerce width 'single-float) height))
           (projection
             (make-vec3 (/ focal aspect) focal (/ far (- far near)))))
      (flet ((uniform-lane (vector fourth)
               (list (coerce (vec3-x vector) 'single-float)
                     (coerce (vec3-y vector) 'single-float)
                     (coerce (vec3-z vector) 'single-float)
                     (coerce fourth 'single-float))))
        (make-array
         20 :element-type 'single-float
         :initial-contents
         (append (uniform-lane (camera-position camera) 0.0)
                 (uniform-lane right 0.0)
                 (uniform-lane up 0.0)
                 (uniform-lane forward 0.0)
                 (uniform-lane
                  projection (/ (- (* far near)) (- far near)))))))))

;;; The first player controller is intentionally a small scalar reference
;;; simulation.  Its body is distinct from the view camera, and its AABB
;;; queries the voxel lattice directly.  This is the behavior later dense
;;; body/contact domains and SIMD kernels must preserve, not their final
;;; storage layout.

(defconstant +player-physics-step+ (/ 1d0 120d0))
(defconstant +player-collision-epsilon+ 1d-7)

(defclass block-world-player ()
  ((position :initarg :position
             :initform (make-vec3 0d0 0d0 0d0)
             :type vec3
             :quantity (:quantity :world-position :unit :metre
                        :tensor-order 1 :character :point)
             :accessor player-position)
   (velocity :initarg :velocity
             :initform (make-vec3 0d0 0d0 0d0)
             :type vec3
             :quantity (:quantity :world-velocity
                        :unit ((:metre 1) (:second -1))
                        :tensor-order 1)
             :accessor player-velocity)
   (half-width :initarg :half-width :initform 0.30d0
               :type double-float
               :quantity (:quantity :player-half-width :unit :metre)
               :reader player-half-width)
   (height :initarg :height :initform 1.80d0
           :type double-float
           :quantity (:quantity :player-height :unit :metre)
           :reader player-height)
   (eye-height :initarg :eye-height :initform 1.62d0
               :type double-float
               :quantity (:quantity :player-eye-height :unit :metre)
               :reader player-eye-height)
   (walk-speed :initarg :walk-speed :initform 5.0d0
               :type double-float
               :quantity (:quantity :player-walk-speed
                          :unit ((:metre 1) (:second -1)))
               :reader player-walk-speed)
   (ground-acceleration :initarg :ground-acceleration :initform 45d0
                        :type double-float
                        :quantity (:quantity :player-acceleration
                                   :unit ((:metre 1) (:second -2)))
                        :reader player-ground-acceleration)
   (air-acceleration :initarg :air-acceleration :initform 14d0
                     :type double-float
                     :quantity (:quantity :player-acceleration
                                :unit ((:metre 1) (:second -2)))
                     :reader player-air-acceleration)
   (gravity :initarg :gravity :initform 24d0
            :type double-float
            :quantity (:quantity :gravity-magnitude
                       :unit ((:metre 1) (:second -2)))
            :reader player-gravity)
   (jump-speed :initarg :jump-speed :initform 8.0d0
               :type double-float
               :quantity (:quantity :player-jump-speed
                          :unit ((:metre 1) (:second -1)))
               :reader player-jump-speed)
   (grounded-p :initarg :grounded-p :initform nil
               :accessor player-grounded-p))
  (:metaclass luv.arithmetic.records:quantity-class))

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

(defun player-terrain-solid-p (world x y z)
  "Treat absent horizontal terrain and the lower world boundary as solid."
  (multiple-value-bind (block status) (world-block-at world x y z)
    (if (eq status :resident)
        (block-solid-p block)
        (let ((height
                (chunk-shape-height
                 (voxel-space-chunk-shape (block-world-space world)))))
          (< y height)))))

(defun player-overlap-indices (minimum maximum)
  (values (floor (+ minimum +player-collision-epsilon+))
          (floor (- maximum +player-collision-epsilon+))))

(defun map-player-overlapping-blocks (function player world)
  (multiple-value-bind (minimum-x maximum-x)
      (player-overlap-indices (- (player-x player) (player-half-width player))
                              (+ (player-x player) (player-half-width player)))
    (multiple-value-bind (minimum-y maximum-y)
        (player-overlap-indices (player-y player)
                                (+ (player-y player) (player-height player)))
      (multiple-value-bind (minimum-z maximum-z)
          (player-overlap-indices
           (- (player-z player) (player-half-width player))
           (+ (player-z player) (player-half-width player)))
        (loop for x from minimum-x to maximum-x do
          (loop for y from minimum-y to maximum-y do
            (loop for z from minimum-z to maximum-z
                  when (player-terrain-solid-p world x y z)
                    do (funcall function x y z))))))))

(defun move-player-axis (player world axis distance)
  "Move PLAYER along AXIS and clamp its AABB against solid voxel cells."
  (when (zerop distance)
    (return-from move-player-axis nil))
  (let ((position (player-position player))
        (velocity (player-velocity player))
        (collided-p nil))
    (incf (vec3-component position axis) distance)
    (map-player-overlapping-blocks
     (lambda (x y z)
       (let ((coordinate (ecase axis (:x x) (:y y) (:z z))))
         (setf collided-p t
               (vec3-component position axis)
               (if (plusp distance)
                   (min (vec3-component position axis)
                        (- coordinate
                           (ecase axis
                             ((:x :z) (player-half-width player))
                             (:y (player-height player)))
                           +player-collision-epsilon+))
                   (max (vec3-component position axis)
                        (+ coordinate 1d0
                           (ecase axis
                             ((:x :z) (player-half-width player))
                             (:y 0d0))
                           +player-collision-epsilon+))))))
     player world)
    (when collided-p
      (setf (vec3-component velocity axis) 0d0)
      (when (and (eq axis :y) (minusp distance))
        (setf (player-grounded-p player) t)))
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

(defun player-position-clear-p (player world x y z)
  "Return true when PLAYER's AABB would be clear at X, Y, Z."
  (multiple-value-bind (minimum-x maximum-x)
      (player-overlap-indices (- x (player-half-width player))
                              (+ x (player-half-width player)))
    (multiple-value-bind (minimum-y maximum-y)
        (player-overlap-indices y (+ y (player-height player)))
      (multiple-value-bind (minimum-z maximum-z)
          (player-overlap-indices (- z (player-half-width player))
                                  (+ z (player-half-width player)))
        (loop for block-x from minimum-x to maximum-x never
          (loop for block-y from minimum-y to maximum-y thereis
            (loop for block-z from minimum-z to maximum-z
                  thereis (player-terrain-solid-p
                           world block-x block-y block-z))))))))

(defun step-block-world-player
    (player world camera pressed-keys seconds &key jump-p)
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
           (collided-x-p (move-player-axis player world :x distance-x))
           (distance-z (* (player-velocity-z player) seconds))
           (attempted-z (+ (player-z player) distance-z))
           (collided-z-p (move-player-axis player world :z distance-z))
           (step-y (+ (player-y player) 1d0))
           (clear-above-x-p
             (and collided-x-p
                  (player-position-clear-p
                   player world attempted-x step-y (player-z player))))
           (clear-above-z-p
             (and collided-z-p
                  (player-position-clear-p
                   player world (player-x player) step-y attempted-z))))
      (when (and grounded-p
                 (not jump-p)
                 (plusp length)
                 (or clear-above-x-p clear-above-z-p))
        (setf (player-velocity-y player) (player-jump-speed player)
              (player-grounded-p player) nil)))
    (decf (player-velocity-y player) (* (player-gravity player) seconds))
    (setf (player-velocity-y player)
          (max -50d0 (player-velocity-y player))
          (player-grounded-p player) nil)
    (move-player-axis player world :y (* (player-velocity-y player) seconds)))
  (sync-camera-to-player camera player)
  player)
