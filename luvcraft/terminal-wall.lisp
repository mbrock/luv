;;; Safe-viewport terminal surface framing shared with Luft's terminal overlay.

(in-package #:luvcraft)

(defparameter *terminal-focus-azimuth* 9.0
  "Degrees the focused view stands to the side of the screen's own normal.")

(defparameter *terminal-focus-elevation* 4.5
  "Degrees the focused view stands above the screen's own centre.")

(defstruct (terminal-face-frame
             (:constructor make-terminal-face-frame (right up outward)))
  right
  up
  outward)

(defgeneric terminal-face-frame (face))

(defmethod terminal-face-frame ((face (eql :front)))
  (make-terminal-face-frame
   +voxel-negative-x+ +voxel-positive-y+ +voxel-positive-z+))

(defmethod terminal-face-frame ((face (eql :back)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-positive-y+ +voxel-negative-z+))

(defmethod terminal-face-frame ((face (eql :right)))
  (make-terminal-face-frame
   +voxel-positive-z+ +voxel-positive-y+ +voxel-positive-x+))

(defmethod terminal-face-frame ((face (eql :left)))
  (make-terminal-face-frame
   +voxel-negative-z+ +voxel-positive-y+ +voxel-negative-x+))

(defmethod terminal-face-frame ((face (eql :top)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-positive-z+ +voxel-positive-y+))

(defmethod terminal-face-frame ((face (eql :bottom)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-negative-z+ +voxel-negative-y+))

(defun terminal-offset-point (origin &rest vector-scales)
  (let ((x (vec3-x origin))
        (y (vec3-y origin))
        (z (vec3-z origin)))
    (loop for (vector scale) on vector-scales by #'cddr
          do (incf x (* (vec3-x vector) scale))
             (incf y (* (vec3-y vector) scale))
             (incf z (* (vec3-z vector) scale)))
    (make-vec3 x y z)))

(defun voxel-direction-vec3 (direction)
  (make-vec3 (coerce (voxel-direction-dx direction) 'single-float)
             (coerce (voxel-direction-dy direction) 'single-float)
             (coerce (voxel-direction-dz direction) 'single-float)))

(defclass terminal-surface ()
  ((world :initarg :world :reader terminal-surface-world)
   (material :initarg :material :reader terminal-surface-material)
   (absent-neighbor-policy
    :initarg :absent-neighbor-policy :initform :air
    :reader terminal-surface-absent-neighbor-policy)
   (face :initarg :face :reader terminal-surface-face)
   (origin :initarg :origin :reader terminal-surface-origin)
   (block-width :initarg :block-width :reader terminal-surface-width)
   (block-height :initarg :block-height :reader terminal-surface-height)))

(defgeneric terminal-surface-axes (surface))
(defgeneric terminal-surface-lower-left-point (surface &optional offset))
(defgeneric terminal-surface-physical-width (surface))
(defgeneric terminal-surface-physical-height (surface))

(defgeneric terminal-surface-panel-frame (surface session &optional offset)
  (:method (surface session &optional (offset 0.010))
    (declare (ignore session))
    (multiple-value-bind (right up outward) (terminal-surface-axes surface)
      (let* ((width (terminal-surface-physical-width surface))
             (height (terminal-surface-physical-height surface))
             (lower-left (terminal-surface-lower-left-point surface offset))
             (center (terminal-offset-point
                      lower-left right (/ width 2.0) up (/ height 2.0))))
        (values center
                (vec3-scale right (/ width 2.0))
                (vec3-scale up (- (/ height 2.0)))
                outward)))))

(defmethod terminal-surface-axes ((surface terminal-surface))
  (let ((frame (terminal-face-frame (terminal-surface-face surface))))
    (values (voxel-direction-vec3 (terminal-face-frame-right frame))
            (voxel-direction-vec3 (terminal-face-frame-up frame))
            (voxel-direction-vec3 (terminal-face-frame-outward frame)))))

(defun terminal-surface-axis-extent (surface direction)
  (let ((extent
          (voxel-space-cell-extent
           (block-world-space (terminal-surface-world surface)))))
    (+ (* (abs (voxel-direction-dx direction)) (vec3-x extent))
       (* (abs (voxel-direction-dy direction)) (vec3-y extent))
       (* (abs (voxel-direction-dz direction)) (vec3-z extent)))))

(defmethod terminal-surface-physical-width ((surface terminal-surface))
  (let ((frame (terminal-face-frame (terminal-surface-face surface))))
    (* (terminal-surface-width surface)
       (terminal-surface-axis-extent
        surface (terminal-face-frame-right frame)))))

(defmethod terminal-surface-physical-height ((surface terminal-surface))
  (let ((frame (terminal-face-frame (terminal-surface-face surface))))
    (* (terminal-surface-height surface)
       (terminal-surface-axis-extent
        surface (terminal-face-frame-up frame)))))

(defmethod terminal-surface-lower-left-point
    ((surface terminal-surface) &optional (offset 0.006))
  "Return the metric world point OFFSET cell-depths outside the lower left."
  (let* ((origin (terminal-surface-origin surface))
         (space (block-world-space (terminal-surface-world surface)))
         (cell-origin (world-coordinate-cell-origin space origin))
         (extent (voxel-space-cell-extent space))
         (frame (terminal-face-frame (terminal-surface-face surface)))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame)))
    (labels ((component (point-reader direction-reader)
               (let ((base (funcall point-reader cell-origin))
                     (cell-size (funcall point-reader extent))
                     (normal (funcall direction-reader outward))
                     (horizontal (funcall direction-reader right))
                     (vertical (funcall direction-reader up)))
                 (+ base
                    (* cell-size
                       (+ (cond ((plusp normal) 1.0)
                                ((minusp normal) 0.0)
                                ((minusp horizontal) 1.0)
                                ((minusp vertical) 1.0)
                                (t 0.0))
                          (* normal offset)))))))
      (make-vec3
       (component #'vec3-x #'voxel-direction-dx)
       (component #'vec3-y #'voxel-direction-dy)
       (component #'vec3-z #'voxel-direction-dz)))))

(defun terminal-focus-eye-direction (right up outward azimuth elevation)
  "The unit direction from a screen's centre toward a viewer standing at
AZIMUTH and ELEVATION degrees off that screen's outward normal."
  (let* ((radian (coerce (/ pi 180.0) 'single-float))
         (side (* azimuth radian))
         (rise (* elevation radian)))
    (vec3-normalize
     (terminal-offset-point
      (make-vec3 0.0 0.0 0.0)
      outward (* (cos rise) (cos side))
      right (* (cos rise) (sin side))
      up (sin rise)))))

(defun terminal-focus-camera-pose
    (surface viewport-width viewport-height
     &optional (left-inset 0.0) (top-inset 0.0)
               (right-inset 0.0) (bottom-inset 0.0))
  "Return a camera pose which contains SURFACE inside the safe view."
  (let* ((width (coerce viewport-width 'single-float))
         (height (coerce viewport-height 'single-float))
         (margin (* 0.06 (min width height)))
         (left-capacity (- 1.0 (/ (* 2.0 (+ left-inset margin)) width)))
         (right-capacity (- 1.0 (/ (* 2.0 (+ right-inset margin)) width)))
         (top-capacity (- 1.0 (/ (* 2.0 (+ top-inset margin)) height)))
         (bottom-capacity
           (- 1.0 (/ (* 2.0 (+ bottom-inset margin)) height)))
         (horizontal-capacity (min left-capacity right-capacity))
         (vertical-capacity (+ top-capacity bottom-capacity)))
    (unless (and (plusp horizontal-capacity) (plusp vertical-capacity))
      (error "Focus insets leave no safe viewport inside ~Dx~D."
             viewport-width viewport-height))
    (let* ((frame (terminal-face-frame (terminal-surface-face surface)))
           (right
             (voxel-direction-vec3 (terminal-face-frame-right frame)))
           (up (voxel-direction-vec3 (terminal-face-frame-up frame)))
           (outward
             (voxel-direction-vec3 (terminal-face-frame-outward frame)))
           (half-width (/ (terminal-surface-physical-width surface) 2.0))
           (half-height (/ (terminal-surface-physical-height surface) 2.0))
           (lower-left (terminal-surface-lower-left-point surface))
           (center
             (terminal-offset-point
              lower-left right half-width up half-height))
           (field-of-view +luvcraft-camera-focused-vertical-field-of-view+)
           (focal (/ (tan (/ field-of-view 2.0))))
           (aspect (/ width height))
           (eye-direction
             (terminal-focus-eye-direction
              right up outward
              *terminal-focus-azimuth* *terminal-focus-elevation*))
           (forward (vec3-scale eye-direction -1.0))
           (pitch (asin (max -1.0 (min 1.0 (vec3-y forward)))))
           (yaw
             (if (> (abs (cos pitch)) 1e-5)
                 (atan (vec3-x forward) (vec3-z forward))
                 (atan (- (vec3-z right)) (vec3-x right))))
           (camera-right (make-vec3 (cos yaw) 0.0 (- (sin yaw))))
           (camera-up (make-vec3 (- (* (sin pitch) (sin yaw)))
                                 (cos pitch)
                                 (- (* (sin pitch) (cos yaw)))))
           (vertical-shift
             (* half-height
                (/ (- top-capacity bottom-capacity) vertical-capacity)))
           (upward-capacity (max top-capacity 1e-3))
           (downward-capacity (max bottom-capacity 1e-3))
           (distance
             (max 1.5
                  (loop for column in (list (- half-width) half-width)
                        maximize
                        (loop for row in (list (- half-height) half-height)
                              maximize
                              (let* ((offset
                                       (terminal-offset-point
                                        (make-vec3 0.0 0.0 0.0)
                                        right column up row))
                                     (across
                                       (abs (vec3-dot offset camera-right)))
                                     (raised
                                       (+ (vec3-dot offset camera-up)
                                          vertical-shift))
                                     (across-need
                                       (/ (* across focal)
                                          (* aspect horizontal-capacity)))
                                     (raised-need
                                       (if (plusp raised)
                                           (/ (* raised focal) upward-capacity)
                                           (/ (* (- raised) focal)
                                              downward-capacity))))
                                (+ (vec3-dot offset eye-direction)
                                   (max across-need raised-need)))))))
           (position
             (terminal-offset-point
              center eye-direction distance camera-up (- vertical-shift))))
      (make-camera-pose position yaw pitch field-of-view))))
