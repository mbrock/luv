(in-package #:luft.render.tests)

(define-test figure-light-follows-the-published-terrain-field
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (domain (luft:make-world-domain :horizontal-bits 7))
         (cells (make-hash-table :test #'eql))
         (opacity (make-array 1 :element-type '(unsigned-byte 8) :initial-element 15))
         (site (luft:make-site domain 63 10 4 luft:+cell-extent+ 1))
         (figure (render:box-figure (make-vec3 63.5 10.5 4.5) (make-vec3 64.5 10.5 4.5)
                                    0.0 1.0 '(0.2 0.2 0.2) '(1.0 1.0 1.0))))
    (unwind-protect
         (dolist (state '(:sun :cave :lantern :removed))
           (let* ((field (luft:solve-voxel-light
                          domain cells opacity
                          (if (eq state :lantern)
                              (vector (luft:make-voxel-light-source site (luft:pack-voxel-light 15 8 2)))
                              #())))
                  (sky (luft:solve-voxel-light
                        domain cells opacity
                        (if (eq state :sun) (vector (luft:make-voxel-light-source site 15)) #())))
                  (mesh (luft:make-surface-mesh domain (make-array 0 :element-type '(unsigned-byte 32)))))
             (setf (luft:voxel-light-field-skylight field) sky
                   (luft:surface-mesh-voxel-light mesh) field)
             (render:renderer-set-mesh renderer (luft:chunk-key-at 63 10) mesh)
             (let ((data (render:pack-figure-instances
                          (list figure) (lambda (x y z) (render:renderer-light-at renderer x y z)))))
               (true (equalp (subseq data 20)
                             (coerce (case state
                                       (:sun '(0.0 0.0 0.0 1.0))
                                       (:lantern (list 1.0 (/ 8.0 15) (/ 2.0 15) 0.0))
                                       (t '(0.0 0.0 0.0 0.0))) 'vector))))
             ;; The neighbour reads its own publication, not this owner's
             ;; halo or the previous center's light across the chunk seam.
             (true (equal '(0.0 0.0 0.0 1.0) (render:renderer-light-at renderer 64.5 10.5 4.5)))))
      (render:destroy-renderer renderer))))

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
          ;; Parachute keeps every check's closure, so count here and drop
          ;; the recorded descriptors: each prefix's shaders would otherwise
          ;; stay reachable from the results for the rest of the run.
          (let ((unreleased (count-if-not
                             (lambda (resource)
                               (= 1 (test-gpu-release-attempts resource)))
                             (test-gpu-resources device))))
            (setf (test-gpu-resources device) nil)
            (true (zerop unreleased))))))))

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
     &key appearances lights descriptors shadow-map shadow-sampler shadow-p shadow-rectangles)
  (declare (ignore sites camera appearances lights descriptors shadow-map
                   shadow-sampler shadow-p shadow-rectangles))
  (luv:create device (luv:make-bind-group-descriptor :label "replacement terrain")))

(defmethod render::encode-terrain
    ((drawing replacement-terrain) pass binding resident &key shadow-p)
  (true binding)
  (true (plusp (render::render-population-site-count
                (render::resident-population-population resident))))
  (luv:encode pass (if shadow-p :replacement-shadow :replacement-scene)))

(define-test renderer-composes-replacement-terrain-without-a-built-in-atlas-or-program
  (let* ((device (make-instance 'gpu-test-device))
         (factory (lambda (device formats samples)
                    (declare (ignore device formats samples))
                    (make-instance 'replacement-terrain)))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :terrain-factory factory :lattice-factory nil
                                         :sky-factory nil :figure-factory nil :torch-factory nil))
         (frame (render::make-renderer-frame-state renderer))
         (pass (make-instance 'gpu-test-encoder))
         (builder (render::make-world-scene-builder)))
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
         (builder (render::make-world-scene-builder)))
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

(define-test superseded-bindings-leave-every-frame-cache-even-when-release-fails
  (dolist (replacement '(:targets :publication))
    (let* ((device (make-instance 'gpu-test-device))
           (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
           (context (make-instance 'gpu-test-context))
           (frames (loop for slot in '(:slot-a :slot-b)
                         collect (render::renderer-frame-state-for renderer context slot)))
           (drawing (render::renderer-sky renderer))
           ;; The sky's cache key is unchanged by either replacement.
           (bindings (loop for frame in frames
                           collect (render::renderer-frame-drawing-binding renderer frame drawing))))
      (unwind-protect
           (progn
             (dolist (binding bindings)
               (setf (test-gpu-fail-release-p binding) t))
             (handler-bind ((luv:release-warning #'muffle-warning))
               (ecase replacement
                 (:targets (render::replace-renderer-target-generation renderer '(800 600)))
                 (:publication
                  (let ((builder (render::make-world-scene-builder)))
                    (render::scene-builder-cell builder 10 10 2)
                    (multiple-value-bind (mesh generation)
                        (render::make-render-mesh (render::finish-scene-builder builder))
                      (render::renderer-update-meshes renderer (list (cons 0 mesh)) nil
                                                      :scene-generation generation))))))
             (loop for frame in frames for binding in bindings do
               (true (= 1 (test-gpu-release-attempts binding)))
               (true (zerop (hash-table-count (render::renderer-frame-state-bind-groups frame))))
               (true (not (eq binding (render::renderer-frame-drawing-binding renderer frame drawing)))))
             ;; Another invalidation must neither resurrect nor forget the
             ;; failed handles; final teardown retries only their release.
             (render::clear-renderer-frame-bind-groups renderer))
        (render:destroy-renderer renderer))
      (render:destroy-renderer renderer)
      (dolist (resource (test-gpu-resources device))
        (true (= (if (member resource bindings) 2 1) (test-gpu-release-attempts resource)))))))

(define-test frame-construction-rolls-back-every-allocation-prefix
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (before (test-gpu-attempts device))
         (frame (render::make-renderer-frame-state renderer))
         (allocations (- (test-gpu-attempts device) before)))
    (render::destroy-renderer-frame-state frame)
    (render:destroy-renderer renderer)
    (loop for failure from 1 to allocations do
      (let* ((device (make-instance 'gpu-test-device :fail-at (+ before failure)))
             (renderer (render:make-renderer device :bgra8-unorm '(640 480))))
        (unwind-protect
             (progn
               (fail (test-renderer-frame renderer))
               (true (zerop (luv:canvas-frame-resource-count (render::renderer-frame-resources renderer)))))
          (render:destroy-renderer renderer))
        (dolist (resource (test-gpu-resources device))
          (true (= 1 (test-gpu-release-attempts resource))))))))

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
                                         :sky-factory nil :figure-factory nil :torch-factory nil))
         (frame (test-renderer-frame renderer))
         (pass (make-instance 'gpu-test-encoder))
         (builder (render::make-world-scene-builder)))
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

(define-test failed-resident-upload-releases-its-slices-before-publication
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (old (render::renderer-publication renderer))
         (arenas (render::renderer-terrain-arenas renderer))
         (before (test-gpu-resources device))
         (builder (render::make-world-scene-builder)))
    (unwind-protect
         (progn
           (render::scene-builder-cell builder 10 10 2)
           (multiple-value-bind (mesh generation)
               (render::make-render-mesh (render::finish-scene-builder builder))
             (let ((*failing-upload-label* "luft terrain material descriptors"))
               (fail (render::renderer-update-meshes renderer (list (cons 0 mesh)) nil
                                                     :scene-generation generation))))
           (true (eq old (render::renderer-publication renderer)))
           (render::reclaim-terrain-arenas arenas)
           (true (zerop (getf (luv:gpu-buffer-arena-stats
                               (render::terrain-arenas-sites-arena arenas))
                              :used)))
           (true (> (length (test-gpu-resources device)) (length before)))
           (loop for resource in (test-gpu-resources device) until (member resource before) do
             (true (= 1 (test-gpu-release-attempts resource)))))
      (render:destroy-renderer renderer))))

(define-test failed-arena-release-is-retried-without-double-releasing-sibling-buffers
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (builder (render::make-world-scene-builder)))
    (render::scene-builder-cell builder 10 10 2)
    (multiple-value-bind (mesh generation)
        (render::make-render-mesh (render::finish-scene-builder builder))
      (render::renderer-update-meshes renderer (list (cons 0 mesh)) nil :scene-generation generation))
    (let ((failed (render::terrain-arenas-sites-buffer
                   (render::renderer-terrain-arenas renderer))))
      (setf (test-gpu-fail-release-p failed) t)
      (fail (render:destroy-renderer renderer))
      (render:destroy-renderer renderer)
      (dolist (resource (test-gpu-resources device))
        (true (= (if (eq resource failed) 2 1) (test-gpu-release-attempts resource)))))))

(define-test pooled-terrain-arenas-share-one-material-draw-and-recycle-slices
  (let* ((render::*terrain-visibility-p* t)
         (render::*temporal-upscaling-p* t)
         (device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :sky-factory nil :figure-factory nil
                                         :torch-factory nil :lattice-factory nil))
         (frame (test-renderer-frame renderer))
         (encoder (make-instance 'gpu-test-encoder))
         (arenas (render::renderer-terrain-arenas renderer)))
    (unwind-protect
         (let ((meshes
                 (loop for index below 4
                       for builder = (render::make-world-scene-builder)
                       do (render::scene-builder-cell builder (+ 10 (* index 16)) 10 2)
                       collect (cons index
                                     (nth-value 0 (render::make-render-mesh
                                                   (render::finish-scene-builder builder)))))))
           (render:renderer-set-meshes renderer meshes)
           ;; All four resident chunks share lockstep offsets in the same global arenas.
           (let ((offsets
                   (loop for index below 4
                         for resident = (render::mesh-slot-resident
                                         (gethash index (render::renderer-mesh-slots renderer)))
                         do (true (= (luv:gpu-buffer-slice-offset
                                      (render::resident-population-site-slice resident))
                                     (luv:gpu-buffer-slice-offset
                                      (render::resident-population-appearance-slice resident))
                                     (luv:gpu-buffer-slice-offset
                                      (render::resident-population-light-slice resident))))
                         collect (render::resident-population-site-offset resident))))
             (true (= 4 (length (remove-duplicates offsets)))))
           ;; Encoding four visible chunks emits ONE batched indirect visibility draw
           ;; and ONE fullscreen material draw (3 vertices).
           (render::encode-renderer-shadows renderer frame encoder)
           (render::encode-renderer-scene renderer frame encoder nil nil)
           (let ((material-draws
                   (remove-if-not
                    (lambda (cmd)
                      (and (typep cmd 'luv::gpu-draw-command)
                           (= 3 (luv::gpu-draw-command-vertex-count cmd))))
                    (test-gpu-commands encoder)))
                 (indirect-batches
                   (remove-if-not
                    (lambda (cmd)
                      (typep cmd 'luv::gpu-multi-draw-indexed-indirect-command))
                    (test-gpu-commands encoder))))
             (true (= 1 (length material-draws)))
             ;; One indirect batch for the shadow pass, one for the visibility pass.
             (true (= 2 (length indirect-batches)))
             (true (every (lambda (cmd)
                            (plusp (luv::gpu-multi-draw-indexed-indirect-command-draw-count cmd)))
                          indirect-batches)))
           ;; Removing all chunks frees and coalesces every arena back to one span.
           (render:renderer-clear-meshes renderer)
           (render::reclaim-terrain-arenas arenas)
           (let ((stats (luv:gpu-buffer-arena-stats
                         (render::terrain-arenas-sites-arena arenas))))
             (true (zerop (getf stats :used)))
             (true (= 1 (getf stats :free-span-count)))))
      (render:destroy-renderer renderer))))

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
           (render::encode-renderer-occlusion renderer frame pass)
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

(define-test shader-reconstruction-upscales-one-sample-into-alternating-history
  (let* ((render::*temporal-reconstruction* :shader)
         (device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (frame (test-renderer-frame renderer)))
    (flet ((labels-of (commands)
             (loop for command in commands
                   when (typep command 'luv::render-pass-descriptor)
                     collect (luv::gpu-descriptor-label command)))
           (output-view (commands label)
             (let ((pass (find label commands
                               :key (lambda (command)
                                      (and (typep command 'luv::render-pass-descriptor)
                                           (luv::gpu-descriptor-label command)))
                               :test #'equal)))
               (getf (first (luv::render-pass-descriptor-color-attachments pass)) :view))))
      (unwind-protect
           (let (outputs histories previous-coverage)
             (true (eq :shader (render::renderer-temporal-resolve-kind renderer)))
             (true (= 1 (render::renderer-scene-sample-count renderer)))
             (true (null (render::renderer-scene-msaa-view renderer)))
             (true (equal '(480 360) (render::renderer-render-extent renderer)))
             (true (equal '(640 480 1)
                          (luv::texture-descriptor-size
                           (test-gpu-descriptor (render::renderer-panel-color-texture renderer)))))
             (dotimes (index 3)
               (when previous-coverage
                 (true (eq previous-coverage (render::renderer-panel-coverage renderer :previous-p t))))
               (setf previous-coverage (render::renderer-panel-coverage renderer))
               (let ((pass (make-instance 'gpu-test-encoder)))
                 (render::encode-renderer-scene renderer frame pass nil nil)
                 (render::encode-renderer-occlusion renderer frame pass)
                 (render::encode-renderer-reconstruction renderer frame pass #(0.1 -0.2) :view)
                 (render::encode-renderer-composite renderer frame pass)
                 (let ((commands (reverse (test-gpu-commands pass))))
                   (true (null (find-if (lambda (c) (typep c 'luv::gpu-copy-texture-command))
                                        commands)))
                   (let ((order (labels-of commands)))
                     (true (< (position "luft temporal neighbourhood" order :test #'equal)
                              (position "luft temporal resolve" order :test #'equal))))
                   (push (output-view commands "luft temporal resolve") outputs)
                   (push (render::renderer-history-view renderer) histories))
                 (true (eq (= index 0) (not (render::renderer-history-used-p renderer))))
                 (incf (render::renderer-frame-index renderer))))
             (setf outputs (reverse outputs) histories (reverse histories))
             ;; Each frame writes the image the next one reads.
             (true (not (eq (first outputs) (second outputs))))
             (true (eq (first outputs) (third outputs)))
             (true (eq (first outputs) (second histories))))
        (render:destroy-renderer renderer)))))

(define-test scene-resolution-does-not-change-the-camera-projection
  (let* ((render::*temporal-reconstruction* :shader)
         (render::*dynamic-resolution-frame-rate* nil)
         (render::*render-scale* 0.4921875)
         (device (make-instance 'gpu-test-device))
         (context (make-instance 'gpu-test-context))
         (renderer (render:make-renderer device :bgra8-unorm '(1292 810)))
         (camera (make-instance 'render:fly-camera :position (make-vec3 19.0 31.0 7.0)
                                                   :yaw 0.7 :pitch -0.2 :field-of-view 0.96)))
    (unwind-protect
         (dolist (projection '(:perspective :isometric))
           (let* ((render::*projection* projection)
                  (expected (render::capture-frame-view camera 1292 810 #(0.0 0.0))))
             (dolist (scale '(0.4921875 0.5859375 0.75))
               (let ((render::*render-scale* scale))
                 (render::encode-renderer-view
                  renderer context (make-instance 'gpu-test-encoder)
                  :surface :view '(1292 810) camera :projection projection)
                 (let* ((view (render::renderer-previous-view renderer))
                        (jitter (render::frame-view-jitter view))
                        (input (render::renderer-render-extent renderer)))
                   (true (equalp (render::frame-view-projection expected)
                                 (render::frame-view-projection view)))
                   ;; Sampling still moves by fractions of the INPUT pixel.
                   (true (equalp jitter
                                 (render::temporal-jitter
                                  (1- (render::renderer-frame-index renderer))
                                  (first input) (second input)))))))))
      (render:destroy-renderer renderer))))

(defclass resolution-test-canvas (luv:canvas)
  ((offscreen-p :initarg :offscreen-p :reader luv:canvas-offscreen-p)))

(define-test fixed-resolution-ignores-slow-frames-without-changing-play
  (let* ((renderer (make-instance 'render::renderer :device nil))
         (display (make-instance 'resolution-test-canvas :offscreen-p nil))
         (offscreen (make-instance 'resolution-test-canvas :offscreen-p t)))
    (flet ((slow-frames (canvas)
             (setf (render::renderer-gpu-times renderer)
                   (make-list 120 :initial-element 0.2d0))
             (render::pace-renderer-resolution renderer canvas)))
      ;; Offscreen frames neither sample nor consume slow work as feedback.
      (slow-frames offscreen)
      (true (= 1.0 (luv:dynamic-resolution-share
                    (render::renderer-dynamic-resolution renderer))))
      (true (null (render::start-renderer-gpu-timing renderer offscreen 1d0)))
      ;; Real-display play still sheds resolution under the same load.
      (slow-frames display)
      (true (= 0.5 (luv:dynamic-resolution-share
                    (render::renderer-dynamic-resolution renderer))))
      (true (equal '(240 180)
                   (render::renderer-render-scale-extent renderer '(640 480))))
      ;; A capture ignores that pre-existing controller state. It also keeps
      ;; odd native extents rather than rounding them to an even scene size.
      (dolist (canvas (list display offscreen))
        (let ((luv:*fixed-render-scale* 1.0))
          (slow-frames canvas)
          (true (equal '(641 481)
                       (render::renderer-render-scale-extent renderer '(641 481)))))
        (let ((luv:*fixed-render-scale* 0.5))
          (true (equal '(320 240)
                       (render::renderer-render-scale-extent renderer '(640 480))))))
      (true (= 0.5 (luv:dynamic-resolution-share
                    (render::renderer-dynamic-resolution renderer)))))))

(define-test scene-only-resize-keeps-the-output-images-and-their-history
  (let* ((render::*temporal-reconstruction* :shader)
         (device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (frame (test-renderer-frame renderer))
         (pass (make-instance 'gpu-test-encoder)))
    (unwind-protect
         (let ((history (render::renderer-history-texture renderer))
               (composite (render::renderer-composite-texture renderer))
               (scene (render::renderer-scene-texture renderer))
               (old (render::renderer-target-generation renderer)))
           (render::encode-renderer-scene renderer frame pass nil nil)
           (render::encode-renderer-reconstruction renderer frame pass #(0.1 -0.2) :view)
           (true (render::renderer-history-valid-p renderer))
           ;; The GPU fell behind: draw three quarters of the ordinary size.
           (setf (luv::%dynamic-resolution-step
                  (render::renderer-dynamic-resolution renderer))
                 24)
           (render::ensure-renderer-extent renderer '(640 480))
           (true (equal '(360 270) (render::renderer-render-extent renderer)))
           (true (not (eq scene (render::renderer-scene-texture renderer))))
           (true (eq history (render::renderer-history-texture renderer)))
           (true (eq composite (render::renderer-composite-texture renderer)))
           (true (render::renderer-history-valid-p renderer))
           ;; Custody moved with the images: the retired generation no longer
           ;; owns them, so they were not released with it.
           (true (null (intersection (list history composite)
                                     (render::owned-gpu-resources old))))
           (true (zerop (test-gpu-release-attempts history)))
           (true (plusp (test-gpu-release-attempts scene)))
           ;; A new output size starts over.
           (render::ensure-renderer-extent renderer '(800 600))
           (true (not (render::renderer-history-valid-p renderer)))
           (true (= 1 (test-gpu-release-attempts history))))
      (render:destroy-renderer renderer))
    (dolist (resource (test-gpu-resources device))
      (true (= 1 (test-gpu-release-attempts resource))))))

(define-test star-patch-tables-index-exactly-the-star-triangles
  (dolist (bevels-p '(t nil))
    (multiple-value-bind (scene-words normals scene-indices shadow-words
                          shadow-indices ranges)
        (render::star-patch-tables bevels-p)
      (flet ((point (words index)
               (loop for lane below 3
                     collect (- (aref words (+ (* 4 index) lane))
                                render::+star-patch-coordinate-bias+))))
        (dotimes (star 256)
          (let* ((triangles (if bevels-p
                                (luft:star-atlas-owned-triangles star)
                                (luft:star-atlas-flat-owned-triangles star)))
                 (masks (if bevels-p
                            (luft:star-atlas-owned-appearance-masks star)
                            (luft:star-atlas-flat-owned-appearance-masks star)))
                 (corners (loop for triangle in triangles append triangle))
                 (range (aref ranges star)))
            ;; Both index lists replay their triangles corner for corner.
            (true (= (length corners) (aref range 1)))
            (true (equal corners
                         (loop for i from (aref range 0)
                               repeat (aref range 1)
                               collect (point scene-words (aref scene-indices i)))))
            (true (equal (loop for triangle in (render::star-shadow-triangles star bevels-p)
                               append triangle)
                         (loop for i from (aref range 2)
                               repeat (aref range 3)
                               collect (point shadow-words (aref shadow-indices i)))))
            ;; A shared scene vertex carries its triangle's mask and normal.
            (loop for triangle in triangles
                  for (material light) in masks
                  for corner from (aref range 0) by 3
                  for key = (render::star-triangle-normal-key triangle)
                  do (dotimes (k 3)
                       (let* ((fourth (aref scene-words
                                            (+ 3 (* 4 (aref scene-indices (+ corner k))))))
                              (normal (ldb (byte 16 16) fourth))
                              (length (sqrt (reduce #'+ (mapcar (lambda (x) (* x x)) key)))))
                         (true (= (logior material (ash light 8)) (ldb (byte 16 0) fourth)))
                         (true (< (abs (- (aref normals (* 4 normal))
                                          (/ (first key) length)))
                                  1e-6)))))))))))

(define-test body-relighting-survives-reused-star-geometry
  ;; No stars change here: a body-only owner must still upload its new light.
  (let* ((domain (luft:make-world-domain :x-bits 6 :y-bits 6))
         (sites (make-array 0 :element-type '(unsigned-byte 32)))
         (dark (luft:make-surface-mesh domain sites))
         (lit (luft:make-surface-mesh domain sites))
         (bodies (make-array 16 :element-type 'single-float :initial-element 0.0)))
    (setf (aref bodies 3) 2.0
          (luft:surface-mesh-bodies dark) bodies
          (luft:surface-mesh-bodies lit) (copy-seq bodies)
          (aref (luft:surface-mesh-bodies lit) 12) 0.8)
    (let* ((previous (render::prepare-render-mesh dark))
           (next (render::prepare-streamed-mesh
                  render::+render-population-preparer+ lit previous)))
      (true (= 0.8 (aref (render::render-population-body-data
                          (render::prepared-render-mesh-population next)) 12)))
      (true (= 0.0 (aref (render::render-population-body-data
                          (render::prepared-render-mesh-population previous)) 12))))))

(define-test sites-sort-by-star-with-their-appearance
  (let* ((stars '(7 3 7 200 3 7))
         (instances (make-array (* 4 (length stars)) :element-type '(unsigned-byte 32)))
         (appearances (make-array (* 2 (length stars)) :element-type '(unsigned-byte 32))))
    (loop for star in stars
          for site from 0
          do (setf (aref instances (* 4 site)) site
                   (aref instances (+ 3 (* 4 site))) star
                   (aref appearances (* 2 site)) (* 10 site)
                   (aref appearances (1+ (* 2 site))) (1+ (* 10 site))))
    (multiple-value-bind (sorted-instances sorted-appearances runs)
        (render::sort-sites-by-star instances appearances)
      (true (equal '((3 0 2) (7 2 3) (200 5 1)) runs))
      (true (equal '(3 3 7 7 7 200)
                   (loop for site below 6 collect (aref sorted-instances (+ 3 (* 4 site))))))
      ;; Each site keeps its own appearance words.
      (dotimes (slot 6)
        (let ((site (aref sorted-instances (* 4 slot))))
          (true (= (* 10 site) (aref sorted-appearances (* 2 slot))))
          (true (= (1+ (* 10 site)) (aref sorted-appearances (1+ (* 2 slot))))))))))

(define-test chunk-boxes-cull-only-when-wholly-outside-one-clip-plane
  ;; A perspective camera at the origin looking along +X, right along -Y,
  ;; up along +Z, with a 0.1-to-600 depth range; the sun looks straight down
  ;; over a 100-cell square.
  (let ((data (make-array 128 :element-type 'single-float :initial-element 0.0)))
    (setf (aref data 4) 0.0 (aref data 5) -1.0 (aref data 6) 0.0
          (aref data 8) 0.0 (aref data 9) 0.0 (aref data 10) 1.0
          (aref data 12) 1.0 (aref data 13) 0.0 (aref data 14) 0.0
          (aref data 16) 1.0 (aref data 17) 1.0
          (aref data 18) (/ 600.0 599.9) (aref data 19) (/ -60.0 599.9)
          (aref data 22) 1.0
          (aref data 72) 0.02 (aref data 77) 0.02
          (aref data 82) -0.001 (aref data 83) 0.5 (aref data 87) 1.0)
    (flet ((box (&rest corners)
             (make-array 6 :element-type 'single-float
                           :initial-contents (mapcar #'float corners)))
           (outside-p (box clip)
             (render::box-outside-clip-p box clip)))
      (let ((camera (render::camera-clip-function data))
            (sun (render::light-clip-function data)))
        (true (not (outside-p (box 5 -1 -1 6 1 1) camera)))
        (true (not (outside-p (box -1 -1 -1 1 1 1) camera))) ; around the eye
        (true (not (outside-p (box 5 4 -1 6 6 1) camera)))   ; across the side plane
        (true (outside-p (box -6 -1 -1 -5 1 1) camera)) ; behind
        (true (outside-p (box 5 50 -1 6 51 1) camera))  ; far beside
        (true (outside-p (box 700 -1 -1 701 1 1) camera)) ; beyond the far plane
        (true (not (outside-p (box 10 10 0 20 20 30) sun)))
        (true (outside-p (box 60 0 0 70 10 30) sun))))))

(define-test reversed-z-projection-maps-near-to-one-far-to-zero-and-inverts-cleanly
  (let ((render::*projection* :perspective))
    (multiple-value-bind (px py pz pw divisor)
        (render::projection-lane 1280 720 1.2
                                 render::+perspective-near+
                                 render::*perspective-far*)
      (declare (ignore px py))
      (true (= 1.0 divisor))
      (flet ((ndc-z (view-z)
               (coerce (/ (+ (* view-z pz) pw) view-z) 'single-float))
             (invert (depth)
               (/ pw (- depth pz))))
        (true (< (abs (- (ndc-z render::+perspective-near+) 1.0)) 1e-5))
        (true (< (abs (ndc-z render::*perspective-far*)) 1e-5))
        ;; Reversed-Z stays strictly monotonic and separates 1 cm at 1 km in f32.
        (true (> (ndc-z 1000.0) (ndc-z 1000.01)))
        (dolist (distance (list render::+perspective-near+ 1.0 50.0 500.0 2000.0
                                render::*perspective-far*))
          (let ((recovered (invert (ndc-z distance))))
            (true (< (/ (abs (- recovered distance)) (max 1.0 distance)) 1e-4))))
        ;; Frustum culling accepts distant boxes inside the extended far plane
        ;; and rejects boxes beyond it.
        (let ((data (make-array 128 :element-type 'single-float :initial-element 0.0)))
          (setf (aref data 5) -1.0 (aref data 10) 1.0 (aref data 12) 1.0
                (aref data 16) 1.0 (aref data 17) 1.0
                (aref data 18) pz (aref data 19) pw (aref data 22) 1.0)
          (let ((camera (render::camera-clip-function data)))
            (flet ((box (&rest corners)
                     (make-array 6 :element-type 'single-float
                                   :initial-contents (mapcar #'float corners))))
              (true (not (render::box-outside-clip-p (box 1500 -1 -1 1501 1 1) camera)))
              (true (render::box-outside-clip-p (box 9000 -1 -1 9001 1 1) camera))
              (true (render::box-outside-clip-p (box -6 -1 -1 -5 1 1) camera))))))))
  (let ((render::*projection* :isometric))
    (multiple-value-bind (px py pz pw divisor)
        (render::projection-lane 1280 720 1.2
                                 render::+orthographic-near+
                                 render::+orthographic-far+)
      (declare (ignore px py))
      (true (= 0.0 divisor))
      (flet ((ndc-z (view-z) (+ (* view-z pz) pw))
             (invert (depth) (/ (- depth pw) pz)))
        (true (< (abs (- (ndc-z render::+orthographic-near+) 1.0)) 1e-5))
        (true (< (abs (ndc-z render::+orthographic-far+)) 1e-5))
        (dolist (distance '(-4000.0 0.0 123.5 4000.0))
          (true (< (abs (- (invert (ndc-z distance)) distance)) 1e-2)))))))

(define-test bevelled-terrain-casts-shrunk-plain-faces
  (let ((cube (make-hash-table :test #'equal))
        (shadow-points nil)
        (bevel-points nil)
        (shadow-count 0))
    (setf (gethash '(0 0 0) cube) t)
    (maphash
     (lambda (site star)
       (flet ((global (point)
                (mapcar (lambda (c s) (+ c (* luft:+mesh-cell-size+ s))) point site)))
         (let ((shadow (render::star-shadow-triangles star t))
               (flat (luft:star-atlas-flat-owned-triangles star)))
           (true (= (length flat) (length shadow)))
           (incf shadow-count (length shadow))
           ;; Each cast triangle is its plain face moved one tick inward.
           (loop for cast in shadow
                 for face in flat
                 for inward = (mapcar #'- (render::star-triangle-normal-key face))
                 do (true (equal cast (loop for point in face
                                            collect (mapcar #'+ point inward))))
                    (dolist (point cast) (push (global point) shadow-points)))
           (dolist (triangle (luft:star-atlas-owned-triangles star))
             (dolist (point triangle) (push (global point) bevel-points))))))
     (luft::star-surface-sites cube))
    (true (= 12 shadow-count))
    ;; Each cast face lies one tick inside its cell's plane, at tick 1 or 7,
    ;; while still reaching the cell's edges along the face ...
    (dolist (point shadow-points)
      (true (some (lambda (c) (or (= c 1) (= c 7))) point))
      (true (every (lambda (c) (<= 0 c 8)) point)))
    ;; ... so the closed cube those planes bound is ticks 1..7, and no
    ;; bevelled vertex lies strictly inside it.
    (dolist (point bevel-points)
      (true (some (lambda (c) (or (<= c 1) (>= c 7))) point)))))

(defun check-shadow-rectangles (sites &optional rectangle-count)
  ;; Compare covered unit squares, not just total area: filling a hole while
  ;; dropping another square must fail. The oracle is the existing atlas.
  (let ((render::*bevelled-shadows-p* nil)
        (expected (make-hash-table :test #'equal))
        (actual (make-hash-table :test #'equal))
        (rectangles (render::merge-terrain-shadow-faces sites)))
    (loop for offset from 0 below (length sites) by 4 do
      (dolist (triangle (render::star-shadow-triangles (aref sites (+ offset 3)) t))
        (let* ((normal (render::star-triangle-normal-key triangle))
               (minimum
                 (loop for i below 3
                       collect (+ (* 8 (aref sites (+ offset i)))
                                  (reduce #'min triangle :key (lambda (p) (nth i p)))))))
          (incf (gethash (list normal minimum) expected 0)))))
    (loop for offset from 0 below (length rectangles) by 8
          for direction = (round (aref rectangles (+ offset 3)))
          for axis = (1- (abs direction))
          for u-axis = (mod (1+ axis) 3)
          for v-axis = (mod (+ axis 2) 3)
          for normal = (loop for i below 3 collect (if (= i axis) (signum direction) 0))
          do (loop for u below (round (aref rectangles (+ offset 4 u-axis))) do
               (loop for v below (round (aref rectangles (+ offset 4 v-axis))) do
                 (let ((minimum
                         (loop for i below 3
                               collect (+ (round (* 8 (aref rectangles (+ offset i))))
                                          (cond ((= i u-axis) (* 8 u))
                                                ((= i v-axis) (* 8 v))
                                                (t 0))))))
                   (incf (gethash (list normal minimum) actual 0) 2)))))
    (true (equalp expected actual))
    (when rectangle-count
      (true (= (* 8 rectangle-count) (length rectangles))))))

(define-test merged-shadow-rectangles-preserve-every-oriented-unit-face
  (flet ((solid-sites (cells)
           (let ((sites (make-array 0 :element-type '(unsigned-byte 32)
                                     :adjustable t :fill-pointer 0)))
             (maphash (lambda (point star)
                        (dolist (coordinate point) (vector-push-extend coordinate sites))
                        (vector-push-extend star sites))
                      (luft::star-surface-sites cells))
             (make-array (length sites) :element-type '(unsigned-byte 32)
                                       :initial-contents sites))))
    (check-shadow-rectangles (make-array 0 :element-type '(unsigned-byte 32)) 0)
    (dotimes (star 256)
      (check-shadow-rectangles
       (make-array 4 :element-type '(unsigned-byte 32)
                     :initial-contents (list 61 37 19 star))))
    ;; An asymmetric box crosses an owner boundary and compresses to six
    ;; rectangles. Then punch a tunnel and add a disconnected cell.
    (let ((cells (make-hash-table :test #'equal)))
      (loop for x from 62 below 67 do
        (loop for y from 9 below 12 do
          (loop for z from 3 below 5 do (setf (gethash (list x y z) cells) t))))
      (check-shadow-rectangles (solid-sites cells) 6)
      (loop for z from 3 below 5 do (remhash (list 64 10 z) cells))
      (setf (gethash '(70 13 6) cells) t)
      (let* ((sites (solid-sites cells))
             (left nil) (right nil))
        (check-shadow-rectangles sites)
        ;; Each resident must preserve its own surface, even if the other
        ;; side of a seam is absent or uses a different rectangle partition.
        (loop for offset from 0 below (length sites) by 4
              for record = (loop for i below 4 collect (aref sites (+ offset i)))
              do (if (< (first record) 64) (push record left) (push record right)))
        (dolist (records (list left right))
          (let ((words (apply #'append records)))
            (check-shadow-rectangles
             (make-array (length words) :element-type '(unsigned-byte 32)
                                       :initial-contents words))))))))

(define-test native-hud-follows-bounded-world-presentation
  (let* ((render::*output-pixel-budget* 1048576)
         (device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(3420 2146)))
         (frame (test-renderer-frame renderer))
         (encoder (make-instance 'gpu-test-encoder))
         (overlay-target nil))
    (unwind-protect
         (progn
           (true (equal '(1292 810) (render::renderer-extent renderer)))
           (true (equal '(968 608) (render::renderer-render-extent renderer)))
           (let ((luv:*fixed-render-scale* 1.0))
             (true (equal '(3420 2146)
                          (render::renderer-world-output-extent '(3420 2146)))))
           (render::encode-renderer-presentation
            renderer frame encoder :native-surface
            (lambda (pass)
              (setf overlay-target
                    (find-if (lambda (command)
                               (typep command 'luv::render-pass-descriptor))
                             (test-gpu-commands pass)))))
           (true (eq :native-surface
                     (getf (first (luv::render-pass-descriptor-color-attachments
                                   overlay-target)) :view)))
           (let ((passes (remove-if-not
                          (lambda (command) (typep command 'luv::render-pass-descriptor))
                          (reverse (test-gpu-commands encoder)))))
             (true (= 2 (length passes)))
             (true (eq (render::renderer-target-generation-presentation-view
                        (render::renderer-target-generation renderer))
                       (getf (first (luv::render-pass-descriptor-color-attachments
                                     (first passes))) :view)))))
      (render:destroy-renderer renderer))))

(defclass test-analytical-world-source (luft.terrain:world-source)
  ((elevation-fn :initarg :elevation-fn :reader test-source-elevation-fn))
  (:default-initargs
   :domain (luft:make-world-domain :x-bits 10 :y-bits 10)
   :material-vocabulary (luv.domains:make-identity-vocabulary-domain
                         :members (list (make-instance
                                         'luft.terrain:material-placement
                                         :name :stone
                                         :kind (make-instance
                                                'luft.terrain:material-kind
                                                :name :stone
                                                :base-tone '(0.5 0.5 0.5))))
                         :limit #xff)))

(defmethod luft.terrain:world-source-distant-terrain-p ((source test-analytical-world-source))
  t)

(defmethod luft.terrain:world-source-surface-column ((source test-analytical-world-source) x y)
  (let ((h (funcall (test-source-elevation-fn source) x y)))
    (when h
      (values h 1 1))))

(define-test distant-terrain-shaders-and-quad-packing
  ;; Both vertex and fragment specifications compile cleanly to SPIR-V and MSL.
  (let ((vs (luft.render.shaders:distant-terrain-vertex-specification))
        (fs (luft.render.shaders:distant-terrain-fragment-specification)))
    (true (plusp (length (luv.spir-v:assemble-shader-specification vs))))
    (true (plusp (length (luv.spir-v:assemble-shader-specification fs))))
    (true (plusp (length (luv.msl:msl-document-source (luv.msl:compile-msl vs)))))
    (true (plusp (length (luv.msl:msl-document-source (luv.msl:compile-msl fs))))))
  ;; Quad word packing preserves signed world coordinates, heights, face, stride, and AO.
  (multiple-value-bind (w0 w1 w2 w3)
      (render::pack-distant-quad-words -320 4096 12 48 3 8 8 42 7 210)
    (true (= -320 (- (ldb (byte 24 0) w0) render::+distant-coord-bias+)))
    (true (= 42 (ldb (byte 8 24) w0)))
    (true (= 4096 (- (ldb (byte 24 0) w1) render::+distant-coord-bias+)))
    (true (= 7 (ldb (byte 8 24) w1)))
    (true (= 12 (- (ldb (byte 12 0) w2) render::+distant-z-bias+)))
    (true (= 48 (- (ldb (byte 12 12) w2) render::+distant-z-bias+)))
    (true (= 210 (ldb (byte 8 24) w2)))
    (true (= 3 (ldb (byte 8 0) w3)))
    (true (= 8 (ldb (byte 8 8) w3)))
    (true (= 8 (ldb (byte 8 16) w3)))))

(define-test distant-terrain-nested-rings-align-without-gaps
  (dolist (pos '((0 0) (1056 1024) (-513 -129) (2047 63)))
    (destructuring-bind (cx cy) pos
      (let ((boxes (make-array render::+distant-level-count+)))
        (dotimes (level render::+distant-level-count+)
          (multiple-value-bind (min-x max-x min-y max-y)
              (render::distant-ring-bounds level cx cy)
            (setf (aref boxes level) (vector min-x max-x min-y max-y))
            (when (plusp level)
              (let ((inner (aref boxes (1- level))))
                ;; Inner ring's bounds in parent-tile units lie strictly inside outer ring.
                (true (zerop (mod (aref inner 0) 2)))
                (true (= 1 (mod (aref inner 1) 2)))
                (true (< min-x (ash (aref inner 0) -1)))
                (true (> max-x (ash (aref inner 1) -1)))
                (true (< min-y (ash (aref inner 2) -1)))
                (true (> max-y (ash (aref inner 3) -1)))
                ;; A parent tile at inner's center is covered; at outer edge is not.
                (true (render::distant-tile-covered-by-inner-ring-p
                       level (ash (aref inner 0) -1) (ash (aref inner 2) -1) inner))
                (true (not (render::distant-tile-covered-by-inner-ring-p
                            level min-x min-y inner)))))))
        ;; Outer ring guarantees >= 2560 cells in every direction around (cx, cy).
        (let* ((outer (aref boxes (1- render::+distant-level-count+)))
               (tile-size (render::distant-level-tile-size
                           (1- render::+distant-level-count+)))
               (min-wx (* (aref outer 0) tile-size))
               (max-wx (* (1+ (aref outer 1)) tile-size))
               (min-wy (* (aref outer 2) tile-size))
               (max-wy (* (1+ (aref outer 3)) tile-size)))
          (true (>= (- cx min-wx) 2560))
          (true (>= (- max-wx cx) 2560))
          (true (>= (- cy min-wy) 2560))
          (true (>= (- max-wy cy) 2560)))))))

(define-test distant-terrain-emits-skirts-batches-draws-and-reclaims-arena
  (let* ((source (make-instance 'test-analytical-world-source
                                :elevation-fn (lambda (x y)
                                                (if (< x 16) 24 10))))
         (device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (frame (test-renderer-frame renderer))
         (encoder (make-instance 'gpu-test-encoder)))
    (unwind-protect
         (progn
           ;; Tile (0, 0, 0) spans x in [0, 64) at stride 2: 32x32 tops + step cliff + outer skirts.
           (multiple-value-bind (words quad-count bounds)
               (render::build-distant-tile-quads source 0 0 0)
             (true (= (* quad-count 4) (length words)))
             ;; 1024 top quads + 32 interior cliff quads + 128 outer boundary skirts = 1184.
             (true (= 1184 quad-count))
             (true (equalp #(0.0 0.0 7.0 64.0 64.0 25.0) bounds)))
           ;; Incremental update respects budget and populates horizon distance.
           (let ((built (render::update-renderer-distant-terrain renderer source 32.0 32.0 :budget 12)))
             (true (= 12 built))
             (true (= 2500.0f0 (render::renderer-distant-terrain-horizon renderer))))
           ;; Scene pass emits a multi-draw-indexed-indirect batch for distant terrain.
           (render::encode-renderer-scene renderer frame encoder nil nil)
           (let ((indirect-batches
                   (remove-if-not
                    (lambda (cmd)
                      (typep cmd 'luv::gpu-multi-draw-indexed-indirect-command))
                    (test-gpu-commands encoder))))
             (true (= 1 (length indirect-batches)))
             (true (= 12 (luv::gpu-multi-draw-indexed-indirect-command-draw-count
                          (first indirect-batches)))))
           ;; Moving far away evicts old tiles and frees their arena slices cleanly.
           (render::update-renderer-distant-terrain renderer source 20000.0 20000.0 :budget 4)
           (render::reclaim-distant-terrain-arena (render::renderer-distant-terrain renderer))
           (let ((stats (render::distant-terrain-stats renderer)))
             (true (= 4 (getf stats :tile-count))))
           ;; Clearing frees all slices and resets horizon to 0.0.
           (render::clear-distant-terrain (render::renderer-distant-terrain renderer))
           (render::reclaim-distant-terrain-arena (render::renderer-distant-terrain renderer))
           (true (zerop (render::renderer-distant-terrain-horizon renderer)))
           (let ((arena-stats (getf (render::distant-terrain-stats renderer) :arena)))
             (true (zerop (getf arena-stats :used)))
             (true (= 1 (getf arena-stats :free-span-count)))))
      (render:destroy-renderer renderer))
    ;; Packed fixnum tile keys remain distinct across negative/positive coords and levels.
    (let ((keys (make-hash-table :test #'eql)))
      (dotimes (lvl 4)
        (loop for tx from -3 to 3 do
          (loop for ty from -3 to 3 do
            (let ((k (render::distant-tile-key lvl tx ty)))
              (true (typep k 'fixnum))
              (true (null (gethash k keys)))
              (setf (gethash k keys) t))))))))

(define-test temporal-jitter-is-zero-mean-and-stratified
  (let* ((period (length render::*temporal-jitter-samples*))
         (samples (loop for i below period
                        collect (render::temporal-jitter i 100 200))))
    (true (= 8 period))
    ;; Exact zero mean in both X and Y across one full cycle.
    (true (< (abs (loop for s in samples sum (aref s 0))) 1e-6))
    (true (< (abs (loop for s in samples sum (aref s 1))) 1e-6))
    ;; Cycle repeats cleanly with the period and scales inversely by dimensions.
    (true (equalp (first samples) (render::temporal-jitter period 100 200)))
    (let ((doubled (render::temporal-jitter 0 200 400)))
      (true (< (abs (- (* 2.0 (aref doubled 0)) (aref (first samples) 0))) 1e-6))
      (true (< (abs (- (* 2.0 (aref doubled 1)) (aref (first samples) 1))) 1e-6)))
    ;; Consecutive frames alternate vertical sign so subpixel horizontal slivers
    ;; never drift into a low-frequency beat.
    (dotimes (i period)
      (let ((a (aref (nth i samples) 1))
            (b (aref (nth (mod (1+ i) period) samples) 1)))
        (true (< (* a b) 0.0))))
    ;; Each 4-frame half-cycle visits all 4 quadrants and all 4 vertical strata.
    (dolist (start '(0 4))
      (let ((quadrants nil) (strata nil))
        (loop for i from start below (+ start 4) do
          (let* ((p (aref render::*temporal-jitter-samples* i))
                 (x (aref p 0))
                 (y (aref p 1)))
            (pushnew (list (plusp x) (plusp y)) quadrants :test #'equal)
            (pushnew (floor (* (+ y 0.5) 4.0)) strata)))
        (true (= 4 (length quadrants)))
        (true (= 4 (length strata)))))))

(define-test translucent-crystal-shaders-compile-with-motion-gated-reactive-mask
  (let* ((base (shaders:star-fragment-specification))
         (translucent (shaders:star-translucent-fragment-specification base t))
         (visibility (shaders:terrain-visibility-material-fragment-specification base))
         (outputs (luv.shader:shader-specification-outputs translucent))
         (msl (luv.msl:msl-document-source (luv.msl:compile-msl translucent))))
    (true (typep visibility 'luv.shader:shader-specification))
    (true (plusp (length (luv.msl:msl-document-source (luv.msl:compile-msl visibility)))))
    (true (= 3 (length outputs)))
    (true (find 2 outputs :key #'luv.shader:shader-interface-location))
    (true (search "reactive_output" msl))))
