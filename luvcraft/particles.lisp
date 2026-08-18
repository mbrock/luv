;;; Short-lived block fragments for the direct manipulation feedback loop.
;;;
;;; The particle system is a semantic session-owned object, while its small
;;; moving population and per-frame render product stay as compact records and
;;; one dense interleaved vertex array.  Fragments reuse the ordinary block
;;; atlas and surface pipeline; this first effect deliberately does not cast
;;; shadows or participate in world collision.

(in-package #:luvcraft)

(defconstant +block-particle-burst-size+ 18)
(defconstant +maximum-block-particles+ 192)
(defconstant +block-particle-vertices-per-particle+
  (* 6 +block-mesh-vertices-per-face+))
(defconstant +block-particle-buffer-size+
  (* 4 +maximum-block-particles+
     +block-particle-vertices-per-particle+
     +block-mesh-floats-per-vertex+))
(defconstant +block-particle-gravity+ 14.0)

(defstruct (block-particle
             (:constructor make-block-particle
                 (block x y z velocity-x velocity-y velocity-z size lifetime)))
  (block *stone-block* :type block-kind)
  (x 0.0 :type single-float)
  (y 0.0 :type single-float)
  (z 0.0 :type single-float)
  (velocity-x 0.0 :type single-float)
  (velocity-y 0.0 :type single-float)
  (velocity-z 0.0 :type single-float)
  (size 0.12 :type single-float)
  (age 0.0 :type single-float)
  (lifetime 0.7 :type single-float))

(defclass block-particle-system ()
  ((particles
    :initform (make-array +maximum-block-particles+
                          :element-type 'block-particle
                          :adjustable t :fill-pointer 0)
    :reader block-particle-system-particles)))

(defun block-particle-count (system)
  "Return the number of live fragments in SYSTEM."
  (length (block-particle-system-particles system)))

(defun fractional-part (number)
  (- number (floor number)))

(defun block-particle-pattern (index x y z)
  "Return deterministic spread values for burst particle INDEX."
  (let* ((salt (+ (* x 0.7548777) (* y 0.5698403) (* z 0.4382891)))
         (u (fractional-part (+ salt (* index 0.618034))))
         (v (fractional-part (+ (* salt 1.732051) (* index 0.414214))))
         (w (fractional-part (+ (* salt 2.236068) (* index 0.732051)))))
    (values u v w)))

(defun discard-oldest-block-particles (particles count)
  (when (plusp count)
    (replace particles particles :start2 count)
    (decf (fill-pointer particles) count))
  particles)

(defun smash-block-particles (system block coordinate)
  "Emit a small deterministic fragment burst for BLOCK at COORDINATE."
  (check-type system block-particle-system)
  (check-type block block-kind)
  (let* ((particles (block-particle-system-particles system))
         (x (world-coordinate-x coordinate))
         (y (world-coordinate-y coordinate))
         (z (world-coordinate-z coordinate))
         (overflow (max 0 (- (+ (length particles)
                                +block-particle-burst-size+)
                             +maximum-block-particles+))))
    (discard-oldest-block-particles particles overflow)
    (dotimes (index +block-particle-burst-size+)
      (multiple-value-bind (u v w)
          (block-particle-pattern index x y z)
        (let* ((angle (* 2.0 pi u))
               (horizontal-speed (+ 1.4 (* 2.2 v)))
               (size (+ 0.09 (* 0.08 w))))
          (vector-push-extend
           (make-block-particle
            block
            (coerce (+ x 0.18 (* 0.64 v)) 'single-float)
            (coerce (+ y 0.18 (* 0.64 w)) 'single-float)
            (coerce (+ z 0.18 (* 0.64 u)) 'single-float)
            (coerce (* (cos angle) horizontal-speed) 'single-float)
            (coerce (+ 2.4 (* 3.2 w)) 'single-float)
            (coerce (* (sin angle) horizontal-speed) 'single-float)
            (coerce size 'single-float)
            (coerce (+ 0.55 (* 0.35 v)) 'single-float))
           particles)))))
  system)

(defun advance-block-particles (system seconds)
  "Advance SYSTEM by SECONDS and compact expired fragments in place."
  (check-type system block-particle-system)
  (let* ((particles (block-particle-system-particles system))
         (dt (coerce seconds 'single-float))
         (write-index 0))
    (dotimes (read-index (length particles))
      (let ((particle (aref particles read-index)))
        (incf (block-particle-age particle) dt)
        (when (< (block-particle-age particle)
                 (block-particle-lifetime particle))
          (decf (block-particle-velocity-y particle)
                (* +block-particle-gravity+ dt))
          (incf (block-particle-x particle)
                (* (block-particle-velocity-x particle) dt))
          (incf (block-particle-y particle)
                (* (block-particle-velocity-y particle) dt))
          (incf (block-particle-z particle)
                (* (block-particle-velocity-z particle) dt))
          (setf (aref particles write-index) particle)
          (incf write-index))))
    (setf (fill-pointer particles) write-index))
  system)

(defun emit-block-particle-face (vertices particle face shade)
  (let* ((block (block-particle-block particle))
         (corners (block-face-corners face))
         (normal (block-face-neighbor face))
         (nx (voxel-direction-dx normal))
         (ny (voxel-direction-dy normal))
         (nz (voxel-direction-dz normal))
         (tile (block-face-tile block face))
         (atlas-width (* +block-atlas-tile-size+ +block-atlas-tile-capacity+))
         (half-size (* 0.5 (block-particle-size particle))))
    (dolist (index '(0 1 2 0 2 3))
      (let* ((corner (nth index corners))
             (cx (first corner))
             (cy (second corner))
             (cz (third corner)))
        (multiple-value-bind (local-u local-v)
            (block-face-local-uv face corner)
          (push-block-vertex-components
           vertices
           (+ (block-particle-x particle) (* (- cx 0.5) (* 2.0 half-size)))
           (+ (block-particle-y particle) (* (- cy 0.5) (* 2.0 half-size)))
           (+ (block-particle-z particle) (* (- cz 0.5) (* 2.0 half-size)))
           (/ (+ (* tile +block-atlas-tile-size+) 0.5
                 (* local-u (1- +block-atlas-tile-size+)))
              atlas-width)
           (/ (+ 0.5 (* local-v (1- +block-atlas-tile-size+)))
              +block-atlas-tile-size+)
           shade nx ny nz 1.0 0.0 (block-surface-emission block)
           ;; A tumbling fragment has no neighbours at all, so every edge of
           ;; every face is a convex one.
           +block-face-edge-convex+ +block-face-edge-convex+
           +block-face-edge-convex+ +block-face-edge-convex+))))))

(defun block-particle-vertices (system)
  "Build the block-pipeline vertex stream for SYSTEM's current fragments."
  (let* ((particles (block-particle-system-particles system))
         (vertices
           (make-array
            (* (length particles) +block-particle-vertices-per-particle+
               +block-mesh-floats-per-vertex+)
            :element-type 'single-float :adjustable t :fill-pointer 0)))
    (dotimes (index (length particles))
      (let ((particle (aref particles index))
            (shade (+ 0.86 (* 0.035 (mod index 4)))))
        (dolist (face *block-faces*)
          (emit-block-particle-face vertices particle face shade))))
    vertices))
