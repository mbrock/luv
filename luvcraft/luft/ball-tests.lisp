(in-package #:luvcraft.luft.game.tests)

(defun ball-test-scene (&key wall slope)
  (let* ((placement (game:block-placement luvcraft::*stone-block*))
         (builder (luft.terrain:make-scene-builder
                   :horizontal-bits 11
                   :material-vocabulary (game:luvcraft-palette-vocabulary game:*luvcraft-palette*)
                   :ground-material placement :architecture-material placement)))
    (loop for x from 1024 below 1088 do
      (loop for y from 1024 below 1088 do
        (loop for z below (if slope (max 1 (- 12 (floor (- x 1024) 2))) 2)
              do (luft.terrain:scene-builder-cell builder x y z))
        (when (and wall (= x 1044))
          (loop for z from 2 below 12 do (luft.terrain:scene-builder-cell builder x y z)))))
    (luft.terrain:make-streaming-scene (luft.terrain:finish-scene-builder builder))))

(defun ball-test-physics (scene)
  (luvcraft::make-physics-world
   :terrain (make-instance 'host::ball-terrain :scene scene) :kernels :scalar))

(define-test balls-use-luft-floor-bounce-and-rest
  (let* ((physics (ball-test-physics (ball-test-scene)))
         (ball (luvcraft::spawn-body-of-kind physics :ball 8.5 5.0 8.5))
         (landed nil) (peak 0.0))
    (dotimes (i 300)
      (luvcraft::step-physics-world physics)
      (let ((y (nth-value 1 (luvcraft::physics-body-position physics ball)))
            (vy (nth-value 1 (luvcraft::physics-body-velocity physics ball))))
        (when (< y 2.3) (setf landed t))
        (when (and landed (plusp vy)) (setf peak (max peak y)))))
    (true (> peak 2.6))
    (true (< (abs (- (nth-value 1 (luvcraft::physics-body-position physics ball))
                    (- 2.22 luvcraft::*physics-linear-slop*))) 0.02))
    (true (luvcraft::physics-body-sleeping-p physics ball))
    (true (luvcraft::validate-physics-world physics))))

(define-test rolling-balls-stop-at-luft-walls
  (let* ((physics (ball-test-physics (ball-test-scene :wall t)))
         (ball (luvcraft::spawn-physics-body physics 8.5 2.245 8.5
                                             :radius 0.25 :vx 7.0
                                             :restitution 0.0 :rolling-resistance 0.0)))
    (dotimes (i 240) (luvcraft::step-physics-world physics))
    (let ((x (luvcraft::physics-body-position physics ball)))
      (true (> x 15.0))
      (true (< x 19.8)))
    ;; The soft solver may leave a small recoil, but not forward travel.
    (true (< (abs (luvcraft::physics-body-velocity physics ball)) 0.2))))

(define-test balls-roll-down-luft-stair-slopes
  (let* ((physics (ball-test-physics (ball-test-scene :slope t)))
         (ball (luvcraft::spawn-physics-body physics 8.5 8.245 8.5
                                             :radius 0.25 :vx 2.0 :restitution 0.1
                                             :rolling-resistance 0.0)))
    (dotimes (i 240) (luvcraft::step-physics-world physics))
    (multiple-value-bind (x y z) (luvcraft::physics-body-position physics ball)
      (declare (ignore z))
      (true (> x 14.0))
      (true (< y 6.0)))))

(define-test balls-freeze-with-velocity-and-resume-when-terrain-returns
  (let* ((scene (ball-test-scene)) (physics (ball-test-physics scene))
         (ball (luvcraft::spawn-body-of-kind physics :ball 8.5 5.0 8.5 :vx 2.0))
         (key (luft:chunk-key-at 1032 1032))
         (store (luft.terrain:streaming-scene-store scene))
         (chunk (gethash key store)))
    (luvcraft::step-physics-world physics)
    (let ((position (multiple-value-list (luvcraft::physics-body-position physics ball)))
          (velocity (multiple-value-list (luvcraft::physics-body-velocity physics ball))))
      (remhash key store)
      (dotimes (i 120) (luvcraft::step-physics-world physics))
      (is equal position (multiple-value-list (luvcraft::physics-body-position physics ball)))
      (is equal velocity (multiple-value-list (luvcraft::physics-body-velocity physics ball)))
      (is = 1 (luvcraft::physics-world-body-count physics))
      (true (luvcraft::validate-physics-world physics))
      (setf (gethash key store) chunk)
      (luvcraft::step-physics-world physics)
      (true (> (luvcraft::physics-body-position physics ball) (first position))))))

(define-test original-ball-hand-controls-run-in-the-luft-host
  (let* ((scene (ball-test-scene))
         (viewer (make-instance 'host:luvcraft-viewer :source scene
                                :luvcraft-world (luvcraft:make-empty-little-block-world :seed 7)
                                :hotbar (list (game:block-placement luvcraft::*grass-block*))))
         (key (make-instance 'luv:canvas-key-press-event :timestamp 0 :key-name :b)))
    (setf (render:viewer-pointer-captured-p viewer) t)
    (true (render::viewer-key-command viewer key))
    (luv:handle-canvas-event viewer nil key)
    (true (host::viewer-ball-in-hand-p viewer))
    (true (search "right marbles" (mcluv:palette-hint viewer)))
    (setf (render:camera-position (render:viewer-camera viewer))
          (luv.arithmetic.lisp.vec3:make-vec3 1032.5 1032.5 5.0)
          (render:camera-yaw (render:viewer-camera viewer)) 0.0
          (render:camera-pitch (render:viewer-camera viewer)) 0.0)
    (loop repeat 5 do
      (luv:handle-canvas-event
       viewer nil (make-instance 'luv:canvas-pointer-button-press-event
                                 :timestamp 0 :x 10 :y 10 :button :left)))
    (let ((physics (host::viewer-physics viewer)))
      (is = 5 (luvcraft::physics-world-body-count physics))
      (is = 1 (loop for kind across (subseq (luvcraft::physics-body-columns-kind-lane
                                            (luvcraft::physics-world-awake physics)) 0 5)
                    count (= kind (luvcraft::body-kind-index :smiley-ball))))
      (luv:handle-canvas-event
       viewer nil (make-instance 'luv:canvas-pointer-button-press-event
                                 :timestamp 0 :x 10 :y 10 :button :right))
      (is = 17 (luvcraft::physics-world-body-count physics))
      (true (> (luvcraft::physics-body-velocity physics
                (aref (luvcraft::physics-body-columns-handle-lane
                       (luvcraft::physics-world-awake physics)) 0)) 13.0)))
    (luv:handle-canvas-event viewer nil key)
    (true (not (host::viewer-ball-in-hand-p viewer)))))
