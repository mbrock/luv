(in-package #:luft.render.tests)

(define-test torch-drawing-rolls-back-every-allocation-prefix
  (let* ((device (make-instance 'gpu-test-device))
         (drawing (render:make-framed-torch-drawing device '(:rgba16-float :rg16-float) 4))
         (allocations (test-gpu-attempts device)))
    (render:release-torch-drawing drawing)
    (loop for failure from 1 to allocations do
      (let ((device (make-instance 'gpu-test-device :fail-at failure)))
        (fail (render:make-framed-torch-drawing device '(:rgba16-float :rg16-float) 4))
        (dolist (resource (test-gpu-resources device))
          (true (= 1 (test-gpu-release-attempts resource))))))))

(define-test torch-drawing-retains-failed-resources-for-retry
  (let* ((device (make-instance 'gpu-test-device))
         (drawing (render:make-framed-torch-drawing device '(:rgba16-float) 4))
         (failed (first (test-gpu-resources device))))
    (setf (test-gpu-fail-release-p failed) t)
    (fail (render:release-torch-drawing drawing))
    (true (equal (list failed) (render::owned-gpu-resources drawing)))
    (render:release-torch-drawing drawing)
    (render:release-torch-drawing drawing)
    (dolist (resource (test-gpu-resources device))
      (true (= (if (eq failed resource) 2 1) (test-gpu-release-attempts resource))))))

(define-test omitted-torches-allocate-no-program-or-frame-state-and-encode-nothing
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480) :torch-factory nil))
         (frame (render::make-renderer-frame-state renderer))
         (pass (make-instance 'gpu-test-encoder)))
    (unwind-protect
         (progn
           (true (null (render::renderer-torches renderer)))
           (true (null (render::renderer-frame-state-flame-effect-buffer frame)))
           (true (null (render::renderer-flame-bind-group renderer)))
           (true (null (render::renderer-frame-torch-body-bind-group renderer frame t)))
           (true (null (render::renderer-frame-flame-bind-group renderer frame)))
           (render:upload-torch-frame nil nil 1.0)
           (render:encode-torch-bodies nil pass nil 2 :shadow-p t)
           (render:encode-torch-bodies nil pass nil 2)
           (render:encode-torch-flames nil pass nil 2)
           (true (null (test-gpu-commands pass)))
           ;; Publications still own their instance data. Only the optional
           ;; program and effect upload disappear when drawing is omitted.
           (true (notany
                  (lambda (resource)
                    (let ((label (or (luv::gpu-descriptor-label
                                      (test-gpu-descriptor resource)) "")))
                      (or (search "torch-body" label)
                          (search "torch flame effect" label)
                          (search "torch flame layout" label))))
                  (test-gpu-resources device))))
      (render::destroy-renderer-frame-state frame)
      (render:destroy-renderer renderer))))

(define-test torch-drawing-uses-one-population-in-all-three-passes
  (let* ((device (make-instance 'gpu-test-device))
         (drawing (render:make-framed-torch-drawing device '(:rgba16-float) 4))
         (effect (render:make-torch-frame-buffer drawing device))
         (shadow (render:make-torch-body-binding
                  drawing device :instances :camera :shadow-view :shadow-sampler :shadow-p t))
         (body (render:make-torch-body-binding
                drawing device :instances :camera :shadow-view :shadow-sampler))
         (flame (render:make-torch-flame-binding
                 drawing device :instances :camera effect :opaque-depth))
         (pass (make-instance 'gpu-test-encoder)))
    (unwind-protect
         (progn
           (render:upload-torch-frame drawing effect 2.0)
           (render:encode-torch-bodies drawing pass shadow 2 :shadow-p t)
           (render:encode-torch-bodies drawing pass body 2)
           (render:encode-torch-flames drawing pass flame 2)
           (let ((draws (remove-if-not
                         (lambda (command) (typep command 'luv::gpu-draw-command))
                         (reverse (test-gpu-commands pass)))))
             (true (= 3 (length draws)))
             (true (equal (list (render::torch-body-vertex-count)
                               (render::torch-body-vertex-count) 6)
                          (mapcar #'luv::gpu-draw-command-vertex-count draws)))
             (true (every (lambda (draw) (= 2 (luv::gpu-draw-command-instance-count draw))) draws)))
           (dolist (binding (list shadow body flame))
             (true (eq :instances
                       (getf (first (luv::bind-group-descriptor-entries
                                     (test-gpu-descriptor binding))) :resource)))))
      (dolist (resource (list flame body shadow effect)) (luv:destroy resource))
      (render:release-torch-drawing drawing))))

(define-test torch-resize-replaces-depth-binding-and-keeps-the-drawing
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (drawing (render::renderer-torches renderer))
         (old-binding (render::renderer-flame-bind-group renderer)))
    (unwind-protect
         (progn
           (render::replace-renderer-target-generation renderer '(800 600))
           (true (eq drawing (render::renderer-torches renderer)))
           (true (not (eq old-binding (render::renderer-flame-bind-group renderer))))
           (true (= 1 (test-gpu-release-attempts old-binding))))
      (render:destroy-renderer renderer))))

(define-test authored-torch-requests-are-retained-without-fabricating-star-surface-frames
  (let ((builder (render::make-scene-builder)))
    (render::scene-builder-cell builder 10 10 0)
    (render::scene-builder-torch builder 10 10 0 :z :high)
    (let ((scene (render::finish-scene-builder builder)))
      (true (= 1 (length (render::scene-torches scene))))
      (multiple-value-bind (mesh generation) (render::make-render-mesh scene)
        (true (null (luft:surface-mesh-attachments mesh)))
        (true (eq (render::scene-authored-light-generation scene)
                  (render::scene-mesh-generation-light-generation generation)))))))
