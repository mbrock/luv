(in-package #:luft.render.tests)

(define-test renderer-composition-rolls-back-every-allocation-prefix
  (dolist (temporal '(nil t))
    (let* ((render::*temporal-upscaling-p* temporal)
           (device (make-instance 'gpu-test-device))
           (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
           (allocations (test-gpu-attempts device)))
      (render:destroy-renderer renderer)
      (render:destroy-renderer renderer)
      (dolist (resource (test-gpu-resources device))
        (true (= 1 (test-gpu-release-attempts resource))))
      (loop for failure from 1 to allocations do
        (let ((device (make-instance 'gpu-test-device :fail-at failure)))
          (fail (render:make-renderer device :bgra8-unorm '(640 480)))
          (dolist (resource (test-gpu-resources device))
            (true (= 1 (test-gpu-release-attempts resource)))))))))

(define-test renderer-retains-a-failed-nested-program-for-release-retry
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (pipeline (render::program-pipeline
                    (render::terrain-shadow-program (render::renderer-terrain renderer)))))
    (setf (test-gpu-fail-release-p pipeline) t)
    (fail (render:destroy-renderer renderer))
    (render:destroy-renderer renderer)
    (render:destroy-renderer renderer)
    (dolist (resource (test-gpu-resources device))
      (true (= (if (eq resource pipeline) 2 1) (test-gpu-release-attempts resource))))))

(defclass replacement-terrain (render::terrain-drawing) ())

(defmethod render::make-terrain-binding
    ((drawing replacement-terrain) device sites camera
     &key appearances descriptors shadow-map shadow-sampler shadow-p)
  (declare (ignore sites camera appearances descriptors shadow-map shadow-sampler shadow-p))
  (luv:create device (luv:make-bind-group-descriptor :label "replacement terrain")))

(defmethod render::encode-terrain
    ((drawing replacement-terrain) pass binding workgroups &key shadow-p)
  (true binding)
  (true (plusp workgroups))
  (luv:encode pass (if shadow-p :replacement-shadow :replacement-scene)))

(define-test renderer-composes-replacement-terrain-without-a-built-in-atlas-or-program
  (let* ((device (make-instance 'gpu-test-device))
         (factory (lambda (device formats samples)
                    (declare (ignore device formats samples))
                    (make-instance 'replacement-terrain)))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :terrain-factory factory :lattice-factory nil
                                         :sky-factory nil :player-factory nil :torch-factory nil))
         (frame (render::make-renderer-frame-state renderer))
         (pass (make-instance 'gpu-test-encoder))
         (builder (render::make-scene-builder)))
    (unwind-protect
         (progn
           (true (eq factory (getf (render::renderer-component-options renderer) :terrain-factory)))
           (true (null (render::renderer-lattice renderer)))
           (render::scene-builder-cell builder 10 10 0)
           (multiple-value-bind (mesh generation)
               (render::make-render-mesh (render::finish-scene-builder builder))
             (render::renderer-update-meshes renderer (list (cons 0 mesh)) nil
                                             :scene-generation generation))
           (render::encode-renderer-shadows renderer frame pass)
           ;; Construction mode is harmless when the diagnostic is omitted.
           (render::encode-renderer-scene renderer frame pass nil t)
           (true (equal '(:replacement-shadow :replacement-scene)
                        (remove-if-not (lambda (command)
                                         (member command '(:replacement-shadow :replacement-scene)))
                                       (reverse (test-gpu-commands pass))))))
      (render::destroy-renderer-frame-state frame)
      (render:destroy-renderer renderer))))

(define-test residency-replaces-only-publication-and-frames-own-the-torch-depth-join
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (frame (test-renderer-frame renderer))
         (target (render::renderer-target-generation renderer))
         (old-binding (render::renderer-frame-flame-bind-group renderer frame))
         (builder (render::make-scene-builder)))
    (unwind-protect
         (progn
           (render::scene-builder-cell builder 10 10 2)
           (multiple-value-bind (mesh generation)
               (render::make-render-mesh (render::finish-scene-builder builder))
             (render::renderer-update-meshes renderer (list (cons 0 mesh)) nil
                                             :scene-generation generation))
           (true (eq target (render::renderer-target-generation renderer)))
           (true (= 1 (test-gpu-release-attempts old-binding)))
           (true (not (eq old-binding (render::renderer-frame-flame-bind-group renderer frame)))))
      (render:destroy-renderer renderer))))

(define-test failed-frame-binding-release-remains-owned-for-retry
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (frame (test-renderer-frame renderer))
         (binding (render::renderer-frame-flame-bind-group renderer frame)))
    (setf (test-gpu-fail-release-p binding) t)
    (fail (render:destroy-renderer renderer))
    (render:destroy-renderer renderer)
    (dolist (resource (test-gpu-resources device))
      (true (= (if (eq resource binding) 2 1) (test-gpu-release-attempts resource))))))

(define-test retired-target-failure-keeps-the-new-target-and-retains-retry-custody
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (old (render::renderer-target-generation renderer))
         (failed (render::renderer-target-generation-scene-view old)))
    (setf (test-gpu-fail-release-p failed) t)
    (handler-bind ((warning #'muffle-warning))
      (render::replace-renderer-target-generation renderer '(800 600)))
    (true (not (eq old (render::renderer-target-generation renderer))))
    (true (member old (render::owned-gpu-resources renderer)))
    (render:destroy-renderer renderer)
    (dolist (resource (test-gpu-resources device))
      (true (= (if (eq resource failed) 2 1) (test-gpu-release-attempts resource))))))

(define-test lattice-diagnostics-draw-the-current-star-mesh
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :sky-factory nil :player-factory nil :torch-factory nil))
         (frame (test-renderer-frame renderer))
         (pass (make-instance 'gpu-test-encoder))
         (builder (render::make-scene-builder)))
    (unwind-protect
         (progn
           (render::scene-builder-cell builder 10 10 2)
           (multiple-value-bind (mesh generation)
               (render::make-render-mesh (render::finish-scene-builder builder))
             (let ((words (render::mesh-lattice-point-words mesh)))
               (true (plusp (length words)))
               (loop for offset from 0 below (length words) by 4 do
                 (true (<= 80 (aref words offset) 88))
                 (true (<= 80 (aref words (+ offset 1)) 88))
                 (true (<= 16 (aref words (+ offset 2)) 24))))
             (render::renderer-update-meshes renderer (list (cons 0 mesh)) nil
                                             :scene-generation generation))
           (render::encode-renderer-scene renderer frame pass nil t)
           (let ((draw (find-if (lambda (command) (typep command 'luv::gpu-draw-command))
                                (test-gpu-commands pass))))
             (true draw)
             (true (plusp (luv::gpu-draw-command-instance-count draw)))))
      (render:destroy-renderer renderer))))

(defvar *failing-upload-label* nil)

(defmethod luv:write-buffer :before ((buffer gpu-test-resource) data &key offset)
  (declare (ignore data offset))
  (when (and *failing-upload-label*
             (equal *failing-upload-label* (luv::gpu-descriptor-label (test-gpu-descriptor buffer))))
    (error "Injected upload failure.")))

(define-test failed-resident-upload-releases-its-buffer-before-publication
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (old (render::renderer-publication renderer))
         (before (test-gpu-resources device))
         (builder (render::make-scene-builder)))
    (unwind-protect
         (progn
           (render::scene-builder-cell builder 10 10 2)
           (multiple-value-bind (mesh generation)
               (render::make-render-mesh (render::finish-scene-builder builder))
             (let ((*failing-upload-label* "luft resident site instances"))
               (fail (render::renderer-update-meshes renderer (list (cons 0 mesh)) nil
                                                     :scene-generation generation))))
           (true (eq old (render::renderer-publication renderer)))
           (true (> (length (test-gpu-resources device)) (length before)))
           (loop for resource in (test-gpu-resources device) until (member resource before) do
             (true (= 1 (test-gpu-release-attempts resource)))))
      (render:destroy-renderer renderer))))

(define-test failed-resident-release-is-retried-without-double-releasing-its-other-buffers
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (builder (render::make-scene-builder)))
    (render::scene-builder-cell builder 10 10 2)
    (multiple-value-bind (mesh generation)
        (render::make-render-mesh (render::finish-scene-builder builder))
      (render::renderer-update-meshes renderer (list (cons 0 mesh)) nil :scene-generation generation))
    (let ((failed (render::resident-population-instance-buffer
                   (render::mesh-slot-resident (gethash 0 (render::renderer-mesh-slots renderer))))))
      (setf (test-gpu-fail-release-p failed) t)
      (fail (render:destroy-renderer renderer))
      (render:destroy-renderer renderer)
      (dolist (resource (test-gpu-resources device))
        (true (= (if (eq resource failed) 2 1) (test-gpu-release-attempts resource)))))))

(defclass gpu-test-scaler (gpu-test-resource luv:gpu-temporal-scaler) ())

(defmethod luv:create ((device gpu-test-device) (descriptor luv::temporal-scaler-descriptor))
  (change-class (call-next-method) 'gpu-test-scaler
                :input-size (luv::temporal-scaler-descriptor-input-size descriptor)
                :output-size (luv::temporal-scaler-descriptor-output-size descriptor)
                :color-usage '(:copy-src) :depth-usage nil
                :motion-usage '(:copy-src) :output-usage '(:copy-dst)))

(define-test native-reconstruction-remains-a-target-owned-hal-scaler
  (let* ((device (make-instance 'gpu-test-device))
         (factory (lambda (device)
                    (declare (ignore device))
                    (make-instance 'render:reconstruction :kind :metalfx)))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :reconstruction-factory factory))
         (frame (test-renderer-frame renderer))
         (pass (make-instance 'gpu-test-encoder)))
    (unwind-protect
         (progn
           (true (null (render::reconstruction-program (render::renderer-reconstruction renderer))))
           (true (equal '(480 360) (luv:gpu-temporal-scaler-input-size
                                    (render::renderer-temporal-scaler renderer))))
           (true (member :copy-src (luv::texture-descriptor-usage
                                   (test-gpu-descriptor (render::renderer-scene-texture renderer)))))
           (render::encode-renderer-scene renderer frame pass nil nil)
           (render::encode-renderer-reconstruction renderer frame pass #(0.1 -0.2) :view)
           (render::encode-renderer-composite renderer frame pass)
           (let* ((commands (reverse (test-gpu-commands pass)))
                  (signal (position-if (lambda (c) (typep c 'luv::gpu-signal-temporal-scaler-command)) commands))
                  (scale (position-if (lambda (c) (typep c 'luv::gpu-temporal-scale-command)) commands))
                  (wait (position-if (lambda (c) (typep c 'luv::gpu-wait-temporal-scaler-command)) commands)))
             (true (and signal scale wait (< signal scale wait))))
           (true (render::renderer-history-valid-p renderer))
           (true (not (render::renderer-history-used-p renderer)))
           (render::replace-renderer-target-generation renderer '(800 600))
           (true (not (render::renderer-history-valid-p renderer))))
      (render:destroy-renderer renderer))))
