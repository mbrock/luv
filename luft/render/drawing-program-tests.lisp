(in-package #:luft.render.tests)

(defun program-binding-probe (binding &optional (set 0))
  (luv.shader:parse-shader-specification
   'program-binding-probe
   `(:stage :fragment :outputs ((result :vec4 :location 0))
     :resources ((camera-state :uniform-block :set ,set :binding ,binding
                  :members ((position :vec4)))))
   '((luv.shader:set-output result (luv.shader:vec4 0.0 0.0 0.0 1.0)))))

(define-test program-bindings-follow-shader-renumbering-without-host-layout-changes
  (dolist (number '(0 7))
    (let* ((device (make-instance 'gpu-test-device))
           (program (render::make-drawing-program
                     device :vertex (luft.render.shaders:present-vertex-specification)
                     :fragment (program-binding-probe number)
                     :targets '((:format :rgba16-float))))
           (binding (render::make-program-binding program device :camera-state :camera)))
      (unwind-protect
           (progn
             (true (equal `((:binding ,number :type :uniform-buffer))
                          (luv::bind-group-layout-descriptor-entries
                           (test-gpu-descriptor (render::program-layout program)))))
             (true (equal `((:binding ,number :resource :camera))
                          (luv::bind-group-descriptor-entries (test-gpu-descriptor binding))))
             (let ((allocations (test-gpu-attempts device)))
               (fail (render::make-program-binding program device))
               (fail (render::make-program-binding program device :camera :camera))
               (fail (render::make-program-binding program device :camera-state nil))
               (fail (render::make-program-binding program device :camera-state :a :camera-state :b))
               (true (= allocations (test-gpu-attempts device)))))
        (luv:destroy binding)
        (render::release-program program)))))

(define-test unsupported-program-stages-and-sets-fail-before-allocation
  (let ((device (make-instance 'gpu-test-device)))
    (fail (render::make-drawing-program device :vertex (program-binding-probe 0)))
    (fail (render::make-drawing-program
           device :vertex (luft.render.shaders:present-vertex-specification)
           :fragment (program-binding-probe 0 1)))
    (true (zerop (test-gpu-attempts device)))))

(define-test mesh-programs-check-command-kind-before-encoding
  (let* ((device (make-instance 'gpu-test-device))
         (program (render:make-drawing-program
                   device :mesh (luft.render.shaders:terrain-shadow-mesh-specification)))
         (pass (make-instance 'gpu-test-encoder)))
    (unwind-protect
         (progn
           (true (typep (test-gpu-descriptor (render::program-pipeline program))
                        'luv::mesh-render-pipeline-descriptor))
           (fail (render:encode-program program pass :binding
                                        (luv:make-gpu-draw-command :vertex-count 3)))
           (true (null (test-gpu-commands pass)))
           (let ((command (luv:make-gpu-draw-mesh-command :x 17)))
             (render:encode-program program pass :binding command)
             (true (eq command (first (test-gpu-commands pass))))))
      (render:release-program program))))
