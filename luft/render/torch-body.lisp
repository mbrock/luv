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
    (data point-a point-b point-c expected-normal &optional (part 0.0f0))
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
               (vector-push-extend part data)
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

;;; Canonical lantern bodies
;;;
;;; A lantern is a black iron cage around amber glass, under a pyramid roof
;;; with a cap and handle.  The position row's W lane names each triangle's
;;; part: 0 iron, 1 glass, 2 the glowing core.  The glass is opaque here --
;;; the scene has no transparency -- so the body fragment paints the flame's
;;; glow onto it from the core's position.  A standing lantern rises along
;;; the frame normal; a hanging one, whose normal points down, hangs from a
;;; short chain with the same cage mirrored, handle uppermost.

(defun %lantern-body-emit-box (data x0 x1 y0 y1 z0 z1 part &optional (flip-z nil))
  "Emit an axis-aligned box's twelve triangles, mirrored in Z by FLIP-Z."
  (flet ((point (x y z)
           (vector (coerce x 'single-float) (coerce y 'single-float)
                   (coerce (if flip-z (funcall flip-z z) z) 'single-float)))
         (normal (x y z)
           (vector (coerce x 'single-float) (coerce y 'single-float)
                   (coerce (if flip-z (- z) z) 'single-float))))
    (dolist (face
             `(((,x0 ,y0 ,z0) (,x1 ,y0 ,z0) (,x1 ,y1 ,z0) (,x0 ,y1 ,z0) (0 0 -1))
               ((,x0 ,y0 ,z1) (,x1 ,y0 ,z1) (,x1 ,y1 ,z1) (,x0 ,y1 ,z1) (0 0 1))
               ((,x0 ,y0 ,z0) (,x1 ,y0 ,z0) (,x1 ,y0 ,z1) (,x0 ,y0 ,z1) (0 -1 0))
               ((,x0 ,y1 ,z0) (,x1 ,y1 ,z0) (,x1 ,y1 ,z1) (,x0 ,y1 ,z1) (0 1 0))
               ((,x0 ,y0 ,z0) (,x0 ,y1 ,z0) (,x0 ,y1 ,z1) (,x0 ,y0 ,z1) (-1 0 0))
               ((,x1 ,y0 ,z0) (,x1 ,y1 ,z0) (,x1 ,y1 ,z1) (,x1 ,y0 ,z1) (1 0 0))))
      (destructuring-bind (a b c d n) face
        (let ((a (apply #'point a)) (b (apply #'point b))
              (c (apply #'point c)) (d (apply #'point d))
              (n (apply #'normal n)))
          (%torch-body-emit-triangle data a b c n part)
          (%torch-body-emit-triangle data a c d n part))))))

(defun %lantern-body-emit-pyramid (data half z0 z1 part &optional (flip-z nil))
  "Emit a square pyramid of base HALF-width from Z0 to an apex at Z1."
  (flet ((point (x y z)
           (vector (coerce x 'single-float) (coerce y 'single-float)
                   (coerce (if flip-z (funcall flip-z z) z) 'single-float))))
    (let ((apex (point 0 0 z1))
          (rise (/ half (- z1 z0)))
          (corners (list (point (- half) (- half) z0) (point half (- half) z0)
                         (point half half z0) (point (- half) half z0)))
          (sign (if flip-z -1 1)))
      (loop for (a b) on (append corners (list (first corners)))
            for (nx ny) in '((0 -1) (1 0) (0 1) (-1 0))
            while b
            do (%torch-body-emit-triangle
                data a b apex
                (vector (coerce nx 'single-float) (coerce ny 'single-float)
                        (coerce (* sign rise) 'single-float))
                part))
      (destructuring-bind (a b c d) corners
        (let ((down (vector 0.0f0 0.0f0 (coerce (- sign) 'single-float))))
          (%torch-body-emit-triangle data a b c down part)
          (%torch-body-emit-triangle data a c d down part))))))

(defparameter *lantern-core-height* 0.22f0
  "Height of a standing lantern's glowing core above its frame origin.")

(defun %lantern-body-emit-cage (data &optional flip-z)
  (let ((post 0.026) (corner 0.135))
    ;; Base plate, glass, and core.
    (%lantern-body-emit-box data -0.165 0.165 -0.165 0.165 0.0 0.045 0.0 flip-z)
    (%lantern-body-emit-box data -0.118 0.118 -0.118 0.118 0.045 0.385 1.0 flip-z)
    ;; Four corner posts and the rim the roof sits on.
    (dolist (x (list (- corner) corner))
      (dolist (y (list (- corner) corner))
        (%lantern-body-emit-box data (- x post) (+ x post) (- y post) (+ y post)
                                0.045 0.385 0.0 flip-z)))
    (%lantern-body-emit-box data -0.172 0.172 -0.172 0.172 0.385 0.425 0.0 flip-z)
    ;; Roof, cap, and an inverted-U handle.
    (%lantern-body-emit-pyramid data 0.155 0.425 0.545 0.0 flip-z)
    (%lantern-body-emit-box data -0.035 0.035 -0.035 0.035 0.525 0.565 0.0 flip-z)
    (%lantern-body-emit-box data -0.062 -0.042 -0.012 0.012 0.565 0.655 0.0 flip-z)
    (%lantern-body-emit-box data 0.042 0.062 -0.012 0.012 0.565 0.655 0.0 flip-z)
    (%lantern-body-emit-box data -0.062 0.062 -0.012 0.012 0.635 0.665 0.0 flip-z)))

(defun %make-lantern-body-vertex-data (hanging-p)
  (let ((data (make-array 512 :element-type 'single-float
                              :adjustable t :fill-pointer 0)))
    (if hanging-p
        (let ((chain 0.30))
          ;; Three links of alternating orientation down from the ceiling.
          (loop for index below 3
                for z0 = (* index (/ chain 3))
                for z1 = (+ z0 (/ chain 3) 0.012)
                do (if (evenp index)
                       (%lantern-body-emit-box data -0.024 0.024 -0.008 0.008 z0 z1 0.0)
                       (%lantern-body-emit-box data -0.008 0.008 -0.024 0.024 z0 z1 0.0)))
          ;; The cage mirrored so its handle meets the chain's end.
          (%lantern-body-emit-cage
           data (lambda (z) (+ chain (- 0.665 z)))))
        (%lantern-body-emit-cage data))
    (make-array (length data) :element-type 'single-float :initial-contents data)))

(defun lantern-core-local-height (form-code)
  "The local height of FORM-CODE's glowing core, where its light comes from."
  (ecase form-code
    (0 (+ +torch-flame-wick-offset+ (* 0.35 +torch-flame-length+)))
    (1 *lantern-core-height*)
    (2 (+ 0.30 (- 0.665 *lantern-core-height*)))))

(defun %make-attachment-body-vertex-data ()
  "Every drawn body padded to one stride: the torch, then the standing and
hanging lanterns, in form-code order.  Padding vertices sit at the frame
origin, so their triangles have no area."
  (let* ((stride +attachment-body-vertex-stride+)
         (forms (list *torch-body-vertex-data*
                      (%make-lantern-body-vertex-data nil)
                      (%make-lantern-body-vertex-data t)))
         (data (make-array (* (length forms) stride +torch-body-vertex-scalar-count+)
                           :element-type 'single-float :initial-element 0.0f0)))
    (loop for form in forms
          for index from 0
          for count = (/ (length form) +torch-body-vertex-scalar-count+)
          for start = (* index stride +torch-body-vertex-scalar-count+)
          do (when (> count stride)
               (error "Attachment body ~D has ~D vertices, over the ~D stride."
                      index count stride))
             (replace data form :start1 start)
             (loop for vertex from count below stride
                   do (setf (aref data (+ start (* vertex +torch-body-vertex-scalar-count+) 6))
                            1.0f0)))
    data))

(defparameter *attachment-body-vertex-data* (%make-attachment-body-vertex-data))

(defun attachment-body-vertex-data ()
  "A fresh copy of every drawn attachment body at its form's stride."
  (copy-seq *attachment-body-vertex-data*))

;;; Each frame's light shines from its core: a torch's flame, a lantern's
;;; glass.  Its RGB4 emission sets the colour and, in cells, the reach.

(defparameter *attachment-light-strength* 2.2
  "Scene-linear radiance of a full RGB4 level-15 attachment light.")

(defun write-attachment-point-lights (frame-data eye-x eye-y eye-z uniform)
  "Write the attachment lights of FRAME-DATA nearest the eye into UNIFORM.

Unused lights get a zero reach.  Returns the number written."
  (let* ((stride +torch-flame-instance-scalar-count+)
         (frames
           (loop for offset from 0 below (length frame-data) by stride
                 for flags = (aref frame-data (+ offset 7))
                 for light = (unpack-torch-body-frame-flags flags)
                 for height = (lantern-core-local-height (torch-frame-form-code flags))
                 for scale = (aref frame-data (+ offset 11))
                 for x = (+ (aref frame-data offset)
                            (* height scale (aref frame-data (+ offset 4))))
                 for y = (+ (aref frame-data (+ offset 1))
                            (* height scale (aref frame-data (+ offset 5))))
                 for z = (+ (aref frame-data (+ offset 2))
                            (* height scale (aref frame-data (+ offset 6))))
                 unless (zerop light)
                   collect (list (+ (expt (- x eye-x) 2) (expt (- y eye-y) 2)
                                    (expt (- z eye-z) 2))
                                 x y z light)))
         (nearest (subseq (sort frames #'< :key #'first)
                          0 (min +point-light-count+ (length frames)))))
    (fill uniform 0.0f0 :start +point-light-uniform-offset+
                        :end (+ +point-light-uniform-offset+ +point-light-scalar-count+))
    (loop for (nil x y z light) in nearest
          for base from +point-light-uniform-offset+ by 8
          for red = (ldb (byte 4 0) light)
          for green = (ldb (byte 4 4) light)
          for blue = (ldb (byte 4 8) light)
          for strength = (/ *attachment-light-strength* 15.0)
          do (replace uniform
                      (mapcar (lambda (value) (coerce value 'single-float))
                              (list x y z (1+ (max red green blue))
                                    (* red strength) (* green strength)
                                    (* blue strength) 0.0))
                      :start1 base))
    (length nearest)))
