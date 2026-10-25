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
