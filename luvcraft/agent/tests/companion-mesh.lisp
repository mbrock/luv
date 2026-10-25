(in-package #:luvcraft.agent.tests)

(define-test companion-skinning-blends-bones-poses-and-loop-seam
  ;; Asymmetric weights, rotation and translation distinguish row/column
  ;; order, missing influences, translated normals, and the last/first seam.
  (flet ((floats (values)
           (make-array (length values) :element-type 'single-float
                                       :initial-contents (mapcar #'float values))))
    (let* ((identity '(1 0 0 0  0 1 0 0  0 0 1 0))
           (mesh (agent::make-companion-mesh
                  :vertices (floats '(1 2 3  0 1 0  0.2 0.4 0.8))
                  :joints (make-array 4 :element-type '(unsigned-byte 32)
                                       :initial-contents '(0 1 0 0))
                  :weights (floats '(0.25 0.75 0 0)) :bone-count 2 :frame-count 2
                  :poses (floats (append identity identity
                                         '(1 0 0 2  0 1 0 0  0 0 1 0)
                                         '(0 -1 0 0  1 0 0 0  0 0 1 0)
                                         '(1 0 0 0  0 1 0 4  0 0 1 0)
                                         '(-1 0 0 0  0 -1 0 0  0 0 1 0)))))
           (palette (make-array 24 :element-type 'single-float))
           (output (make-array 12 :element-type 'single-float)))
      (dolist (phase '(0.25d0 0.75d0 1.25d0))
        (agent::skin-companion-mesh mesh phase 0.5 palette output)
        (true (every (lambda (actual expected) (< (abs (- actual expected)) 1e-6))
                     output '(0.1875 1.3125 3  -0.1875 0.4375 0  0.2 0.4 0.8  1 2 3))))
      (agent::skin-companion-mesh mesh 0.25d0 0.0 palette output)
      (true (equalp output #(1.0 2.0 3.0  0.0 1.0 0.0  0.2 0.4 0.8  1.0 2.0 3.0))))))

(define-test companion-exports-are-grounded-and-fit-their-interaction-bounds
  (dolist (class '(agent:gnome agent:cat agent:fantus))
    (let* ((body (make-instance class :session nil :x 0 :y 0 :z 0
                                     :position (luvcraft::make-vec3 0d0 0d0 0d0)))
           (kind (agent::embodied-agent-save-kind body))
           (scale (agent::companion-stature kind))
           (mesh (agent::load-companion-mesh kind))
           (palette (make-array (* 12 (agent::companion-mesh-bone-count mesh))
                                :element-type 'single-float))
           (output (make-array (* 12 (/ (length (agent::companion-mesh-vertices mesh)) 9))
                               :element-type 'single-float))
           (highest 0.0) (radius 0.0))
      (multiple-value-bind (cx cy cz bound) (agent::embodied-agent-body-sphere body)
        ;; Every baked pose must retain a grounded foot, not just the idle pose.
        (loop for frame from -1 below (agent::companion-mesh-frame-count mesh)
              do (agent::skin-companion-mesh
                  mesh (/ (max 0 frame) (agent::companion-mesh-frame-count mesh))
                  (if (minusp frame) 0.0 1.0) palette output)
                 (let ((lowest 100.0))
                   (loop for v from 0 below (length output) by 12
                         for x = (* scale (aref output v))
                         for y = (* scale (aref output (+ v 1)))
                         for z = (* scale (aref output (+ v 2)))
                         do (setf lowest (min lowest y) highest (max highest y)
                                  radius (max radius (sqrt (+ (expt (- x cx) 2)
                                                              (expt (- y cy) 2)
                                                              (expt (- z cz) 2))))))
                   (true (< (abs lowest) 0.02))))
        (true (< (abs (- highest (luvcraft:body-height body))) 0.10))
        (true (< radius bound))))))

(define-test two-bone-ik-preserves-unequal-lengths-and-pole-side
  (let ((hip (luvcraft::make-vec3 2d0 3d0 -4d0)))
    (multiple-value-bind (knee ankle)
        (agent::solve-two-bone-chain hip (luvcraft::make-vec3 2d0 1d0 -4d0)
                                    (luvcraft::make-vec3 2d0 2d0 -1d0) 2d0 1d0)
      (true (< (abs (- (luvcraft::vec3-y knee) 1.25d0)) 1d-8))
      (true (< (abs (- (luvcraft::vec3-z knee) (+ -4d0 (sqrt 0.9375d0)))) 1d-8))
      (true (< (abs (- (luvcraft::vec3-y ankle) 1d0)) 1d-8)))
    ;; Outer and inner reach limits, coincident target, and poles parallel
    ;; to each coordinate axis: a fixed-Z fallback fails the final case.
    (dolist (offset '((0 0 0) (0 -0.1 0) (8 0 0) (0 -8 0) (0 0 8)))
      (let ((target (agent::ik-offset hip (apply #'luvcraft::make-vec3 offset))))
        (multiple-value-bind (knee ankle) (agent::solve-two-bone-chain hip target target 2d0 1d0)
          (true (< (abs (- 2 (luvcraft::vec3-length (agent::ik-difference knee hip)))) 1d-7))
          (true (< (abs (- 1 (luvcraft::vec3-length (agent::ik-difference ankle knee)))) 1d-7))
          (true (<= 1 (luvcraft::vec3-length (agent::ik-difference ankle hip)) 3)))))))

(defclass companion-test-ground ()
  ((height :initarg :height :initform (constantly 0d0) :accessor test-ground-height)))

(defmethod agent::companion-ground-height ((ground companion-test-ground) x z ceiling floor)
  (let ((height (funcall (test-ground-height ground) x z)))
    (when (and height (<= floor height ceiling)) height)))

(defun posed-companion-ankle (body leg &optional (toe-offset 0))
  (agent::companion-world-point
   body (agent::companion-palette-point
         (agent::companion-pose-palette (agent::companion-kinematics body))
         (third (agent::companion-leg-bones leg))
         (agent::ik-offset (agent::companion-leg-ankle leg) (luvcraft::make-vec3 0 0 toe-offset)))))

(define-test runtime-companion-contacts-lock-turn-settle-and-release
  (dolist (class '(agent:gnome agent:cat agent:fantus))
    (let* ((ground (make-instance 'companion-test-ground))
           (body (make-instance class :session ground :x 0 :y 0 :z 0
                                      :position (luvcraft::make-vec3 0d0 0d0 0d0)))
           (scale (agent::companion-stature (agent::embodied-agent-save-kind body)))
           (legs (agent::companion-rig-legs (agent::companion-mesh-rig
                                           (agent::companion-mesh (agent::embodied-agent-save-kind body)))))
           (previous nil) (contacts 0) (max-slide 0d0) (max-ground 0d0) (max-length-error 0d0))
      (agent::update-companion-gait body (/ 1d0 60))
      ;; Travel at authored speed, then stop mid-cycle and turn in place.
      ;; Check the actual palette used for skinning, not only target points.
      (dotimes (frame 240)
        (when (< frame 131)
          (setf (agent::embodied-agent-position body)
                (luvcraft::make-vec3 0d0 0d0 (* scale (/ (1+ frame) 60d0)))))
        (when (<= 150 frame 185)
          (incf (agent::embodied-agent-facing-yaw body) 0.025d0))
        (agent::update-companion-gait body (/ 1d0 60))
        (let* ((pose (agent::companion-kinematics body)) (palette (agent::companion-pose-palette pose))
               (feet (agent::companion-pose-feet pose))
               (points (mapcar (lambda (leg) (posed-companion-ankle body leg)) legs))
               (toes (mapcar (lambda (leg) (posed-companion-ankle body leg .1d0)) legs)))
          (loop for foot in feet for point in points for toe in toes for old in previous
                unless (or (agent::companion-foot-swing-p foot) (null (third old)))
                  do (incf contacts)
                     (setf max-slide
                           (max max-slide (luvcraft::vec3-length (agent::ik-difference point (first old)))
                                (luvcraft::vec3-length (agent::ik-difference toe (second old))))))
          (loop for leg in legs for foot in feet for point in points
                for bones = (agent::companion-leg-bones leg)
                for hip = (agent::companion-palette-point palette (first bones) (agent::companion-leg-hip leg))
                for knee = (agent::companion-palette-point palette (second bones) (agent::companion-leg-knee leg))
                for ankle = (agent::companion-palette-point palette (third bones) (agent::companion-leg-ankle leg))
                for sole-height = (* scale (luvcraft::vec3-y (agent::companion-leg-ankle leg)))
                for upper = (luvcraft::vec3-length (agent::ik-difference (agent::companion-leg-knee leg)
                                                                        (agent::companion-leg-hip leg)))
                for lower = (luvcraft::vec3-length (agent::ik-difference (agent::companion-leg-ankle leg)
                                                                        (agent::companion-leg-knee leg)))
                do (setf max-length-error
                         (max max-length-error
                              (abs (- upper (luvcraft::vec3-length (agent::ik-difference knee hip))))
                              (abs (- lower (luvcraft::vec3-length (agent::ik-difference ankle knee))))))
                   (unless (agent::companion-foot-swing-p foot)
                     (setf max-ground (max max-ground (abs (- (luvcraft::vec3-y point) sole-height))))))
          (setf previous (mapcar (lambda (p toe f) (list p toe (not (agent::companion-foot-swing-p f))))
                                 points toes feet))))
      (true (> contacts 100))
      (true (< max-length-error 1d-5) "~A changed segment length: ~A" class max-length-error)
      (true (< max-slide 1d-5) "~A stance slide: ~A" class max-slide)
      (true (< max-ground 1d-5) "~A sole height: ~A" class max-ground)
      (false (some #'agent::companion-foot-swing-p (agent::companion-pose-feet (agent::companion-kinematics body))))
      ;; A teleport and asymmetric footholds must not reuse old world anchors.
      (setf (test-ground-height ground) (lambda (x z) (declare (ignore z)) (if (> x 10) .04d0 -.02d0))
            (agent::embodied-agent-facing-yaw body) 0d0
            (luvcraft::vec3-x (agent::embodied-agent-position body)) 10d0
            (luvcraft::vec3-z (agent::embodied-agent-position body)) -7d0)
      (agent::update-companion-gait body (/ 1d0 60))
      (true (< (abs (- (luvcraft::vec3-x (posed-companion-ankle body (first legs)))
                       (+ 10 (* scale (luvcraft::vec3-x (agent::companion-leg-ankle (first legs))))))) 1d-5))
      (dotimes (i 30) (agent::update-companion-gait body (/ 1d0 60)))
      (dolist (leg legs)
        (let* ((point (posed-companion-ankle body leg))
               (expected (+ (if (> (luvcraft::vec3-x point) 10) .04d0 -.02d0)
                            (* scale (luvcraft::vec3-y (agent::companion-leg-ankle leg))))))
          (true (< (abs (- (luvcraft::vec3-y point) expected)) 1d-5))))
      ;; The lower foothold needs pelvis lowering. Scheduling steps against
      ;; the unlowered hip makes Fantus replant forever, despite enough reach.
      (setf (test-ground-height ground) (lambda (x z) (declare (ignore z)) (if (> x 10) 1d0 1.08d0))
            (luvcraft::vec3-y (agent::embodied-agent-position body)) 1.08d0)
      (let ((settled-error 0d0))
        (dotimes (frame 120)
          (agent::update-companion-gait body (/ 1d0 60))
          (when (>= frame 60)
            (dolist (leg legs)
              (let* ((point (posed-companion-ankle body leg))
                     (expected (+ (if (> (luvcraft::vec3-x point) 10) 1d0 1.08d0)
                                  (* scale (luvcraft::vec3-y (agent::companion-leg-ankle leg))))))
                (setf settled-error (max settled-error (abs (- (luvcraft::vec3-y point) expected))))))))
        (true (< settled-error 1d-5) "~A never settled on reachable uneven supports: ~A" class settled-error))
      (setf (test-ground-height ground) (constantly nil))
      (agent::update-companion-gait body (/ 1d0 60))
      (false (some #'agent::companion-foot-supported-p (agent::companion-pose-feet (agent::companion-kinematics body))))
      (setf (agent::embodied-agent-grounded-p body) nil)
      (agent::update-companion-gait body (/ 1d0 60))
      (false (some #'agent::companion-foot-point (agent::companion-pose-feet (agent::companion-kinematics body)))))))
