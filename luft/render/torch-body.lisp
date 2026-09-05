(in-package #:luft.render)

;;; Canonical torch body
;;;
;;; This is deliberately an expanded triangle stream rather than another
;;; semantic mesh.  Each vertex is two Vec4 rows containing local position and
;;; flat local normal; W is padding.  The attachment frame is the only
;;; world-space placement representation.

(defun %torch-body-point (radius angle height)
  (vector (coerce (* radius (cos angle)) 'single-float)
          (coerce (* radius (sin angle)) 'single-float)
          (coerce height 'single-float)))

(defun %torch-body-emit-triangle
    (data point-a point-b point-c expected-normal)
  (let* ((ab-x (- (aref point-b 0) (aref point-a 0)))
         (ab-y (- (aref point-b 1) (aref point-a 1)))
         (ab-z (- (aref point-b 2) (aref point-a 2)))
         (ac-x (- (aref point-c 0) (aref point-a 0)))
         (ac-y (- (aref point-c 1) (aref point-a 1)))
         (ac-z (- (aref point-c 2) (aref point-a 2)))
         (normal-x (- (* ab-y ac-z) (* ab-z ac-y)))
         (normal-y (- (* ab-z ac-x) (* ab-x ac-z)))
         (normal-z (- (* ab-x ac-y) (* ab-y ac-x)))
         (facing (+ (* normal-x (aref expected-normal 0))
                    (* normal-y (aref expected-normal 1))
                    (* normal-z (aref expected-normal 2)))))
    (when (minusp facing)
      (rotatef point-b point-c)
      (setf normal-x (- normal-x)
            normal-y (- normal-y)
            normal-z (- normal-z)))
    (let ((normal-length
            (sqrt (+ (* normal-x normal-x)
                     (* normal-y normal-y)
                     (* normal-z normal-z)))))
      (when (zerop normal-length)
        (error "Degenerate canonical torch-body triangle: ~S ~S ~S."
               point-a point-b point-c))
      (setf normal-x (/ normal-x normal-length)
            normal-y (/ normal-y normal-length)
            normal-z (/ normal-z normal-length))
      (loop for point in (list point-a point-b point-c)
            do (vector-push-extend (aref point 0) data)
               (vector-push-extend (aref point 1) data)
               (vector-push-extend (aref point 2) data)
               (vector-push-extend 0.0f0 data)
               (vector-push-extend (coerce normal-x 'single-float) data)
               (vector-push-extend (coerce normal-y 'single-float) data)
               (vector-push-extend (coerce normal-z 'single-float) data)
               (vector-push-extend 0.0f0 data)))))

(defun %make-torch-body-vertex-data ()
  (let ((data
          (make-array 128 :element-type 'single-float
                           :adjustable t :fill-pointer 0))
        (bottom-radius 0.145f0)
        (socket-height 0.16f0)
        (socket-radius 0.078f0)
        (shaft-height 0.50f0)
        (shaft-radius 0.055f0)
        (bottom-centre #(0.0f0 0.0f0 0.0f0))
        (top-centre #(0.0f0 0.0f0 0.50f0)))
    (labels ((angle (index)
               (* 2.0f0 (coerce pi 'single-float)
                  (/ (mod index +torch-body-side-count+)
                     +torch-body-side-count+)))
             (ring-point (radius height index)
               (%torch-body-point radius (angle index) height))
             (radial-normal (index axial)
               (let ((middle (angle (+ index 0.5f0))))
                 (vector (coerce (cos middle) 'single-float)
                         (coerce (sin middle) 'single-float)
                         (coerce axial 'single-float))))
             (emit-frustum-side
                 (index lower-radius lower-height upper-radius upper-height)
               (let* ((next (1+ index))
                      (lower-a
                        (ring-point lower-radius lower-height index))
                      (lower-b
                        (ring-point lower-radius lower-height next))
                      (upper-a
                        (ring-point upper-radius upper-height index))
                      (upper-b
                        (ring-point upper-radius upper-height next))
                      (axial
                        (/ (- lower-radius upper-radius)
                           (- upper-height lower-height)))
                      (expected (radial-normal index axial)))
                 (%torch-body-emit-triangle
                  data lower-a lower-b upper-b expected)
                 (%torch-body-emit-triangle
                  data lower-a upper-b upper-a expected))))
      (dotimes (side +torch-body-side-count+)
        (let ((bottom-a (ring-point bottom-radius 0.0f0 side))
              (bottom-b (ring-point bottom-radius 0.0f0 (1+ side))))
          (%torch-body-emit-triangle
           data bottom-centre bottom-b bottom-a #(0.0f0 0.0f0 -1.0f0)))
        (emit-frustum-side
         side bottom-radius 0.0f0 socket-radius socket-height)
        (emit-frustum-side
         side socket-radius socket-height shaft-radius shaft-height)
        (let ((top-a (ring-point shaft-radius shaft-height side))
              (top-b (ring-point shaft-radius shaft-height (1+ side))))
          (%torch-body-emit-triangle
           data top-centre top-a top-b #(0.0f0 0.0f0 1.0f0)))))
    (make-array (length data) :element-type 'single-float
                              :initial-contents data)))

(defparameter *torch-body-vertex-data* (%make-torch-body-vertex-data))

(defun torch-body-vertex-count ()
  "Return the canonical expanded triangle vertex count for one torch body."
  (/ (length *torch-body-vertex-data*)
     +torch-body-vertex-scalar-count+))

(defun torch-body-vertex-data ()
  "Return a fresh float32 copy of the canonical two-Vec4-per-vertex body."
  (copy-seq *torch-body-vertex-data*))
