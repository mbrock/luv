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
  ((x :initarg :x :initform 8.0 :accessor camera-x)
   (y :initarg :y :initform 11.0 :accessor camera-y)
   (z :initarg :z :initform -6.0 :accessor camera-z)
   (yaw :initarg :yaw :initform 0.0 :accessor camera-yaw)
   (pitch :initarg :pitch :initform -0.28 :accessor camera-pitch)
   (sensitivity :initarg :sensitivity :initform 0.0025
                :accessor camera-sensitivity)))

(defun vec3 (x y z)
  (vector (coerce x 'single-float)
          (coerce y 'single-float)
          (coerce z 'single-float)))

(defun vec3-scale (vector scale)
  (vec3 (* (aref vector 0) scale)
        (* (aref vector 1) scale)
        (* (aref vector 2) scale)))

(defun vec3-add (&rest vectors)
  (vec3 (loop for vector in vectors sum (aref vector 0))
        (loop for vector in vectors sum (aref vector 1))
        (loop for vector in vectors sum (aref vector 2))))

(defun vec3-length (vector)
  (sqrt (loop for component across vector sum (* component component))))

(defun vec3-normalize (vector)
  (let ((length (vec3-length vector)))
    (if (plusp length) (vec3-scale vector (/ length)) vector)))

(defgeneric camera-basis (camera))
(defgeneric camera-uniform-data (camera width height))

(defmethod camera-basis ((camera fly-camera))
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3 (* (sin yaw) (cos pitch))
                        (sin pitch)
                        (* (cos yaw) (cos pitch))))
         (right (vec3 (cos yaw) 0.0 (- (sin yaw))))
         (up (vec3 (- (* (sin pitch) (sin yaw)))
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
             (vec3 (/ focal aspect) focal (/ far (- far near)))))
      (make-array
       20 :element-type 'single-float
       :initial-contents
       (list (coerce (camera-x camera) 'single-float)
             (coerce (camera-y camera) 'single-float)
             (coerce (camera-z camera) 'single-float) 0.0
             (aref right 0) (aref right 1) (aref right 2) 0.0
             (aref up 0) (aref up 1) (aref up 2) 0.0
             (aref forward 0) (aref forward 1) (aref forward 2) 0.0
             (aref projection 0) (aref projection 1) (aref projection 2)
             (coerce (/ (- (* far near)) (- far near))
                     'single-float))))))

;;; The first player controller is intentionally a small scalar reference
;;; simulation.  Its body is distinct from the view camera, and its AABB
;;; queries the voxel lattice directly.  This is the behavior later dense
;;; body/contact domains and SIMD kernels must preserve, not their final
;;; storage layout.

(defconstant +player-physics-step+ (/ 1d0 120d0))
(defconstant +player-collision-epsilon+ 1d-7)

(defclass block-world-player ()
  ((x :initarg :x :accessor player-x)
   (y :initarg :y :accessor player-y)
   (z :initarg :z :accessor player-z)
   (velocity-x :initarg :velocity-x :initform 0d0
               :accessor player-velocity-x)
   (velocity-y :initarg :velocity-y :initform 0d0
               :accessor player-velocity-y)
   (velocity-z :initarg :velocity-z :initform 0d0
               :accessor player-velocity-z)
   (half-width :initarg :half-width :initform 0.30d0
               :reader player-half-width)
   (height :initarg :height :initform 1.80d0 :reader player-height)
   (eye-height :initarg :eye-height :initform 1.62d0
               :reader player-eye-height)
   (walk-speed :initarg :walk-speed :initform 5.0d0
               :reader player-walk-speed)
   (ground-acceleration :initarg :ground-acceleration :initform 45d0
                        :reader player-ground-acceleration)
   (air-acceleration :initarg :air-acceleration :initform 14d0
                     :reader player-air-acceleration)
   (gravity :initarg :gravity :initform 24d0 :reader player-gravity)
   (jump-speed :initarg :jump-speed :initform 8.0d0
               :reader player-jump-speed)
   (grounded-p :initarg :grounded-p :initform nil
               :accessor player-grounded-p)))

(defun make-player-for-camera (camera)
  (make-instance 'block-world-player
                 :x (coerce (camera-x camera) 'double-float)
                 :y (- (coerce (camera-y camera) 'double-float) 1.62d0)
                 :z (coerce (camera-z camera) 'double-float)))

(defun sync-camera-to-player (camera player)
  (setf (camera-x camera) (player-x player)
        (camera-y camera) (+ (player-y player) (player-eye-height player))
        (camera-z camera) (player-z player))
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
  (let ((position-slot (ecase axis (:x 'x) (:y 'y) (:z 'z)))
        (velocity-slot
          (ecase axis
            (:x 'velocity-x) (:y 'velocity-y) (:z 'velocity-z)))
        (collided-p nil))
    (incf (slot-value player position-slot) distance)
    (map-player-overlapping-blocks
     (lambda (x y z)
       (let ((coordinate (ecase axis (:x x) (:y y) (:z z))))
         (setf collided-p t
               (slot-value player position-slot)
               (if (plusp distance)
                   (min (slot-value player position-slot)
                        (- coordinate
                           (ecase axis
                             ((:x :z) (player-half-width player))
                             (:y (player-height player)))
                           +player-collision-epsilon+))
                   (max (slot-value player position-slot)
                        (+ coordinate 1d0
                           (ecase axis
                             ((:x :z) (player-half-width player))
                             (:y 0d0))
                           +player-collision-epsilon+))))))
     player world)
    (when collided-p
      (setf (slot-value player velocity-slot) 0d0)
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
         (maximum-change (* acceleration seconds)))
    (setf (player-velocity-x player)
          (move-toward (player-velocity-x player) target-x maximum-change)
          (player-velocity-z player)
          (move-toward (player-velocity-z player) target-z maximum-change))
    (when (and jump-p (player-grounded-p player))
      (setf (player-velocity-y player) (player-jump-speed player)
            (player-grounded-p player) nil))
    (decf (player-velocity-y player) (* (player-gravity player) seconds))
    (setf (player-velocity-y player)
          (max -50d0 (player-velocity-y player))
          (player-grounded-p player) nil)
    (move-player-axis player world :x (* (player-velocity-x player) seconds))
    (move-player-axis player world :z (* (player-velocity-z player) seconds))
    (move-player-axis player world :y (* (player-velocity-y player) seconds)))
  (sync-camera-to-player camera player)
  player)
