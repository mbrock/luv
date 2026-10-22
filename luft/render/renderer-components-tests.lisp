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
  (declare (ignore sites camera appearances descriptors shadow-map shadow-sampler
                   shadow-p))
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
                  (let ((builder (render::make-scene-builder)))
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
           (let (outputs histories)
             (true (eq :shader (render::renderer-temporal-resolve-kind renderer)))
             (true (= 1 (render::renderer-scene-sample-count renderer)))
             (true (null (render::renderer-scene-msaa-view renderer)))
             (true (equal '(480 360) (render::renderer-render-extent renderer)))
             (dotimes (index 3)
               (let ((pass (make-instance 'gpu-test-encoder)))
                 (render::encode-renderer-scene renderer frame pass nil nil)
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
