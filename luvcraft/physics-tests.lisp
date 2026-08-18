;;; Executable claims for the sphere physics: what a pile does, what a
;;; rolling ball does not do, and that the wide kernels are the scalar ones.

(in-package #:luvcraft.tests)

(defun make-physics-test-world (&key (width 32))
  "A flat world: dirt at y=0, grass at y=1, WIDTH cells square from the origin."
  (let ((world (make-block-world)))
    (loop for chunk-x below (ceiling width 16) do
      (loop for chunk-z below (ceiling width 16) do
        (ensure-world-chunk world chunk-x 0 chunk-z)))
    (loop for x below width do
      (loop for z below width do
        (setf (world-block-at world x 0 z) luvcraft::*dirt-block*
              (world-block-at world x 1 z) luvcraft::*grass-block*)))
    world))

(defun step-physics (physics steps)
  (dotimes (i steps)
    (step-physics-world physics))
  physics)

(deftest a-dropped-ball-comes-to-rest-on-the-floor-and-sleeps
  (let* ((world (make-physics-test-world))
         (physics (make-physics-world :terrain world :kernels :scalar))
         (ball (spawn-physics-body physics 8.5 5.0 8.5 :radius 0.25 :restitution 0.3)))
    (step-physics physics 240)
    (multiple-value-bind (x y z) (physics-body-position physics ball)
      ;; Resting on the grass top at y=2, a slop under its radius.
      (ok (< (abs (- x 8.5)) 1e-3))
      (ok (< (abs (- z 8.5)) 1e-3))
      (ok (< (abs (- y (- 2.25 luvcraft::*physics-linear-slop*))) 0.01)))
    (ok (physics-body-sleeping-p physics ball))
    (ok (validate-physics-world physics))))

(deftest a-bouncy-ball-bounces-and-a-dead-one-does-not
  (let* ((world (make-physics-test-world))
         (physics (make-physics-world :terrain world :kernels :scalar))
         (bouncy (spawn-physics-body physics 4.5 4.0 4.5 :radius 0.25 :restitution 0.8))
         (dead (spawn-physics-body physics 12.5 4.0 12.5 :radius 0.25 :restitution 0.0))
         (bouncy-peak 0.0)
         (dead-peak 0.0)
         (landed-p nil))
    (dotimes (i 200)
      (step-physics-world physics)
      (multiple-value-bind (x y z) (physics-body-position physics bouncy)
        (declare (ignore x z))
        (multiple-value-bind (vx vy vz) (physics-body-velocity physics bouncy)
          (declare (ignore vx vz))
          (when (and (not landed-p) (< y 2.3)) (setf landed-p t))
          (when (and landed-p (plusp vy)) (setf bouncy-peak (max bouncy-peak y)))))
      (multiple-value-bind (x y z) (physics-body-position physics dead)
        (declare (ignore x z))
        (multiple-value-bind (vx vy vz) (physics-body-velocity physics dead)
          (declare (ignore vx vz))
          (when (and (plusp vy) (> (physics-world-step-count physics) 40))
            (setf dead-peak (max dead-peak y))))))
    ;; The bouncy ball rose again after landing; the dead one never did.
    (ok (> bouncy-peak 2.6))
    (ok (< dead-peak 2.3))))

(deftest a-rolling-ball-crosses-cell-seams-without-catching
  (let* ((world (make-physics-test-world))
         (physics (make-physics-world :terrain world :kernels :scalar))
         (ball (spawn-physics-body physics 2.5 2.245 8.5 :radius 0.25 :vx 4.0
                                                          :friction 0.5 :rolling-resistance 0.0))
         (max-rise 0.0)
         (max-vy 0.0))
    (dotimes (i 240)
      (step-physics-world physics)
      (multiple-value-bind (x y z) (physics-body-position physics ball)
        (declare (ignore x z))
        (setf max-rise (max max-rise (- y 2.245))))
      (multiple-value-bind (vx vy vz) (physics-body-velocity physics ball)
        (declare (ignore vx vz))
        (setf max-vy (max max-vy (abs vy)))))
    (multiple-value-bind (x y z) (physics-body-position physics ball)
      (declare (ignore y z))
      ;; It got somewhere: rolling friction is off, so it barely slowed.
      (ok (> x 12.0))
      ;; And it never hopped at a seam.
      (ok (< max-rise 0.02))
      (ok (< max-vy 0.3)))))

(deftest a-pile-sleeps-and-a-thrown-ball-wakes-it
  (let* ((world (make-physics-test-world))
         (physics (make-physics-world :terrain world :kernels :scalar))
         (pile (loop for i below 6
                     collect (spawn-physics-body physics 8.5 (+ 2.3 (* 0.6 i)) 8.5
                                                 :radius 0.25 :restitution 0.1))))
    (step-physics physics 400)
    (ok (every (lambda (handle) (physics-body-sleeping-p physics handle)) pile))
    (ok (zerop (luvcraft::physics-body-columns-length (physics-world-awake physics))))
    ;; A ball thrown into the pile wakes what it hits, and they wake others.
    (let ((thrown (spawn-physics-body physics 4.5 2.5 8.5 :radius 0.25 :vx 8.0)))
      (step-physics physics 60)
      (ok (physics-body-alive-p physics thrown))
      (ok (some (lambda (handle) (not (physics-body-sleeping-p physics handle))) pile))
      (ok (validate-physics-world physics)))))

(deftest a-moving-box-pushes-balls-out-of-its-way
  (let* ((world (make-physics-test-world))
         (physics (make-physics-world :terrain world :kernels :scalar))
         (ball (spawn-physics-body physics 8.5 2.5 8.5 :radius 0.25)))
    (step-physics physics 60)
    (multiple-value-bind (x0 y0 z0) (physics-body-position physics ball)
      (declare (ignore y0 z0))
      ;; A box a cell wide walks into the ball from the west at 3 cells/s.
      (loop for step from 0 below 60
            for box-x = (+ 6.0 (* 3.0 (/ step 60.0)))
            do (clear-physics-boxes physics)
               (post-physics-box physics box-x 2.0 8.0 (+ box-x 1.0) 3.8 9.0
                                 :vx 3.0 :owner :walker)
               (step-physics-world physics))
      (multiple-value-bind (x1 y1 z1) (physics-body-position physics ball)
        (declare (ignore y1 z1))
        (ok (> x1 (+ x0 0.5)))))))

(deftest contacts-begin-and-end-and-mortal-bodies-expire
  (let* ((world (make-physics-test-world))
         (physics (make-physics-world :terrain world :kernels :scalar))
         (ball (spawn-physics-body physics 8.5 3.0 8.5 :radius 0.25 :lifetime 1.0))
         (kinds nil))
    (dotimes (i 90)
      (step-physics-world physics)
      (dolist (event (physics-events physics))
        (pushnew (getf event :kind) kinds)))
    (ok (member :begin kinds))
    (ok (member :expired kinds))
    (ok (not (physics-body-alive-p physics ball)))
    ;; The slot is reused with a new generation, so the old handle stays dead.
    (let ((next (spawn-physics-body physics 8.5 3.0 8.5)))
      (ok (= (luvcraft::physics-handle-index next) (luvcraft::physics-handle-index ball)))
      (ok (/= next ball))
      (ok (physics-body-alive-p physics next))
      (ok (not (physics-body-alive-p physics ball))))))

(deftest a-terrain-edit-wakes-the-sleepers-above-it
  (let* ((world (make-physics-test-world))
         (physics (make-physics-world :terrain world :kernels :scalar))
         (ball (spawn-physics-body physics 8.5 2.5 8.5 :radius 0.25)))
    (step-physics physics 200)
    (ok (physics-body-sleeping-p physics ball))
    (setf (world-block-at world 8 1 8) nil)
    (ok (plusp (luvcraft::wake-physics-bodies-near physics 8.5 1.5 8.5 2.0)))
    (step-physics physics 120)
    (multiple-value-bind (x y z) (physics-body-position physics ball)
      (declare (ignore x z))
      ;; It fell into the hole.
      (ok (< y 1.9)))))

(defun physics-hash-after (kernels steps)
  (let* ((world (make-physics-test-world))
         (physics (make-physics-world :terrain world :kernels kernels)))
    (dotimes (i 200)
      (multiple-value-bind (layer rest) (floor i 25)
        (multiple-value-bind (row col) (floor rest 5)
          (spawn-physics-body physics
                              (+ 4.0 (* 1.6 col) (* 0.03 layer)) (+ 3.0 (* 0.6 layer))
                              (+ 4.0 (* 1.6 row))
                              :radius 0.3 :restitution 0.4 :friction 0.5))))
    (step-physics physics steps)
    (values (physics-world-state-hash physics) physics)))

(deftest the-wide-kernels-are-the-scalar-kernels-bit-for-bit
  (let ((wide (remove :scalar (luvcraft::available-physics-kernel-families))))
    (if (null wide)
        (skip "no wide kernel family on this machine")
        (multiple-value-bind (scalar-hash scalar-world) (physics-hash-after :scalar 120)
          (dolist (family wide)
            (multiple-value-bind (wide-hash wide-world) (physics-hash-after family 120)
              (ok (= scalar-hash wide-hash))
              (ok (= (luvcraft::physics-body-columns-length (physics-world-awake scalar-world))
                     (luvcraft::physics-body-columns-length (physics-world-awake wide-world))))))))))
