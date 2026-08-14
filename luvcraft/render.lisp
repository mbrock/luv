;;; Frame encoding, input handling, and the luvcraft session lifecycle.
;;;
;;; The canvas frame callback is the ownership boundary for all GPU
;;; replacement: shader refresh, mesh publication, and uniform updates all
;;; happen here, on the thread that owns the swapchain.  This file also owns
;;; canvas event handling and the START/STOP pair that creates and releases
;;; every GPU resource the application uses.

(in-package #:luv)

(defclass cube-world-frame-state ()
  ((uniform-buffer :initarg :uniform-buffer
                   :reader cube-world-frame-uniform-buffer)
   (bind-group :initarg :bind-group :reader cube-world-frame-bind-group)))

(defconstant +block-world-crosshair-vertex-count+ 24)

(defun make-block-world-crosshair-vertices (width height)
  "Make an outlined pixel-sized crosshair in Vulkan clip coordinates."
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0)))
    (labels ((clip-x (pixels) (/ (* 2.0 pixels) width))
             (clip-y (pixels) (/ (* 2.0 pixels) height))
             (vertex (x y color)
               (dolist (component
                        (list (clip-x x) (clip-y y) 0.0
                              (first color) (second color) (third color)))
                 (vector-push-extend (coerce component 'single-float)
                                     vertices)))
             (rectangle (left top right bottom color)
               (dolist (corner (list (list left top) (list right top)
                                     (list right bottom) (list left top)
                                     (list right bottom) (list left bottom)))
                 (vertex (first corner) (second corner) color))))
      ;; Charcoal establishes a crisp edge on both snow and foliage; the
      ;; smaller white pair is emitted afterward and paints over it.
      (rectangle -2.25 -11.0 2.25 11.0 '(0.08 0.09 0.10))
      (rectangle -11.0 -2.25 11.0 2.25 '(0.08 0.09 0.10))
      (rectangle -0.75 -8.0 0.75 8.0 '(0.96 0.98 1.0))
      (rectangle -8.0 -0.75 8.0 0.75 '(0.96 0.98 1.0)))
    vertices))

(defun remember-cube-world-resource (demo resource)
  (push resource (cube-world-demo-resources demo))
  resource)

(defun cube-world-frame-state (demo surface-texture)
  (or (gethash surface-texture (cube-world-demo-frame-states demo))
      (let ((buffer nil) (bind-group nil) (completed-p nil))
        (unwind-protect
             (progn
               (setf buffer
                     (create
                      (cube-world-demo-device demo)
                      (make-buffer-descriptor
                       :label "block world camera uniform"
                       :size 96 :usage '(:uniform)))
                     bind-group
                     (create
                      (cube-world-demo-device demo)
                      (make-bind-group-descriptor
                       :label "block world frame bindings"
                       :layout (cube-world-demo-layout demo)
                       :entries
                       `((:binding 0
                          :resource ,(cube-world-demo-atlas-view demo))
                         (:binding 1
                          :resource ,(cube-world-demo-atlas-sampler demo))
                         (:binding 2 :resource ,buffer)))))
               (remember-cube-world-resource demo buffer)
               (remember-cube-world-resource demo bind-group)
               (let ((state
                       (make-instance
                        'cube-world-frame-state
                        :uniform-buffer buffer :bind-group bind-group)))
                 (setf (gethash surface-texture
                                (cube-world-demo-frame-states demo))
                       state
                       completed-p t)
                 state))
          (unless completed-p
            (when bind-group (destroy bind-group))
            (when buffer (destroy buffer)))))))

(defun encode-cube-world-frame
    (demo surface-texture encoder &key readback-buffer)
  ;; The canvas callback is the ownership boundary for all GPU replacement.
  ;; MOP notifications from SLY workers have only marked these artifacts dirty.
  (refresh-cube-world-shaders demo)
  (let* ((products (refresh-cube-world-mesh demo))
         (extent (canvas-extent (cube-world-demo-context demo)))
         (frame (cube-world-frame-state demo surface-texture)))
    (write-buffer
     (cube-world-frame-uniform-buffer frame)
     (camera-uniform-data
      (cube-world-demo-camera demo) (first extent) (second extent)))
    (let ((pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :color-attachments
              `((:view ,(cube-world-demo-color-view demo)
                 :load-op :clear :store-op :store
                 :clear-value #(0.43 0.68 0.92 1.0)))
              :depth-stencil-attachment
              `(:view ,(cube-world-demo-depth-view demo)
                :depth-load-op :clear :depth-store-op :discard
                :depth-clear-value 1.0)))))
      (set-pipeline pass (cube-world-demo-pipeline demo))
      (set-bind-group pass 0 (cube-world-frame-bind-group frame))
      (dolist (product products)
        (let ((mesh (cube-world-chunk-product-mesh product)))
          (when (plusp (block-mesh-vertex-count mesh))
            (set-vertex-buffer
             pass 0 (cube-world-chunk-product-vertex-buffer product))
            (draw pass (block-mesh-vertex-count mesh)))))
      (set-pipeline pass (cube-world-demo-crosshair-native-pipeline demo))
      (set-vertex-buffer
       pass 0 (cube-world-demo-crosshair-vertex-buffer demo))
      (draw pass +block-world-crosshair-vertex-count+)
      (end-pass pass))
    (when readback-buffer
      (encode
       encoder
       (make-gpu-copy-texture-to-buffer-command
        :source (cube-world-demo-color-texture demo)
        :destination readback-buffer)))
    (encode
     encoder
     (make-gpu-copy-texture-command
      :source (cube-world-demo-color-texture demo)
      :destination surface-texture))))

(defun render-cube-world-frame (demo timestamp)
  (when (cube-world-demo-running-p demo)
    (let* ((last (cube-world-demo-last-frame-time demo))
           (seconds (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0)))
      (setf (cube-world-demo-last-frame-time demo) timestamp)
      (let ((player (cube-world-demo-player demo)))
        (when player
          (incf (cube-world-demo-physics-accumulator demo) seconds)
          (loop while (>= (cube-world-demo-physics-accumulator demo)
                          +player-physics-step+)
                do (step-block-world-player
                    player (cube-world-demo-world demo)
                    (cube-world-demo-camera demo)
                    (cube-world-demo-pressed-keys demo)
                    +player-physics-step+
                    :jump-p (cube-world-demo-jump-requested-p demo))
                   (setf (cube-world-demo-jump-requested-p demo) nil)
                   (decf (cube-world-demo-physics-accumulator demo)
                         +player-physics-step+)))
      (maintain-cube-world-residency demo)
      (evict-cube-world-products demo)
      (present-canvas-frame
       (cube-world-demo-context demo)
       (lambda (surface-texture encoder)
         (encode-cube-world-frame demo surface-texture encoder)))))))

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (if (eq key :escape)
        (when (cube-world-demo-pointer-captured-p demo)
          (set-canvas-relative-pointer-mode canvas nil)
          (setf (cube-world-demo-pointer-captured-p demo) nil))
        (progn
          (setf (gethash key (cube-world-demo-pressed-keys demo)) t)
          (when (and (eq key :space)
                     (not (canvas-key-event-repeat-p event)))
            (setf (cube-world-demo-jump-requested-p demo) t))
          (unless (canvas-key-event-repeat-p event)
            (let* ((character (canvas-key-event-character event))
                   (number (and character (digit-char-p character))))
              (when (and number (<= 1 number 7))
                (select-cube-world-block demo number)))))))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (remhash (canvas-key-event-key-name event)
           (cube-world-demo-pressed-keys demo))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-pointer-button-press-event))
  (let ((button (canvas-pointer-event-button event)))
    (cond
      ((not (cube-world-demo-pointer-captured-p demo))
       (when (eq button :left)
         (set-canvas-relative-pointer-mode canvas t)
         (setf (cube-world-demo-pointer-captured-p demo) t)))
      ((eq button :left)
       (edit-cube-world-block demo :remove))
      ((eq button :right)
       (edit-cube-world-block demo :place))
      ((eq button :middle)
       (pick-cube-world-block demo))))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (when (cube-world-demo-pointer-captured-p demo)
    (let ((camera (cube-world-demo-camera demo)))
      (incf (camera-yaw camera)
            (* (canvas-pointer-event-delta-x event)
               (camera-sensitivity camera)))
      (setf (camera-pitch camera)
            (max -1.5
                 (min 1.5
                      (- (camera-pitch camera)
                         (* (canvas-pointer-event-delta-y event)
                            (camera-sensitivity camera))))))))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-window-focus-lost-event))
  (declare (ignore event))
  (clrhash (cube-world-demo-pressed-keys demo))
  (setf (cube-world-demo-jump-requested-p demo) nil)
  (when (cube-world-demo-pointer-captured-p demo)
    (set-canvas-relative-pointer-mode canvas nil)
    (setf (cube-world-demo-pointer-captured-p demo) nil))
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (setf (cube-world-demo-running-p demo) nil)
  nil)

(defmethod handle-canvas-event
    ((demo cube-world-demo) canvas (event canvas-event))
  (declare (ignore demo canvas event))
  nil)

(defun start-cube-world-demo (&key
                                (title "luv little block world — click, look, walk")
                                (width 960) (height 640)
                                (frames-per-second 60)
                                (visible-p t)
                                (world (make-empty-little-block-world))
                                (mesher (make-instance
                                         'exposed-face-mesher))
                                (camera (make-instance 'fly-camera))
                                player
                                (residency-radius 4)
                                (publication-limit 2)
                                (load-schedule-limit 4)
                                (mesh-capture-limit 1))
  "Open a little CPU-meshed block world.

Click to capture the pointer, look with the mouse, walk with WASD, and jump
with Space.  Once captured, left click removes the block at the centre of view
and right click places the selected block.  Number keys select materials,
middle click picks the targeted material, Shift sprints, and Escape releases
the pointer.

Pass :VISIBLE-P NIL to keep the SDL window hidden while still exercising the
real SDL/Vulkan surface and swapchain path.  Pass :FRAMES-PER-SECOND NIL for a
capture-only demand clock."
  (let ((canvas (make-sdl-canvas :title title :width width :height height
                                 :visible-p visible-p))
        (player (or player (make-player-for-camera camera)))
        (device nil) (context nil) (resources nil) (pipelines nil) (demo nil)
        (production-system nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (progn
           (setf device
                 (request-gpu-device
                  *gpu-provider* (make-device-descriptor :label title))
                 context
                 (make-canvas-context
                  canvas *gpu-provider*
                  (make-canvas-configuration :device device)))
           (setf production-system
                 (make-single-worker-production-system
                  :name "luvcraft chunk producer"))
           (flet ((keep (resource)
                    (push resource resources)
                    resource))
             (let* ((extent (canvas-extent context))
                  (color-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world color"
                       :size extent :dimensions :2d
                       :format (canvas-format context)
                       :usage '(:render-attachment :copy-src)))))
                  (color-view
                    (keep
                     (create device (make-texture-view-descriptor
                                     :texture color-texture))))
                  (depth-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world depth"
                       :size extent :dimensions :2d :format :depth32-float
                       :usage '(:render-attachment)))))
                  (depth-view
                    (keep
                     (create device (make-texture-view-descriptor
                                     :texture depth-texture))))
                  (atlas-width
                    (* +block-atlas-tile-size+ +block-atlas-tile-count+))
                  (atlas-height +block-atlas-tile-size+)
                  (atlas-data (make-block-texture-atlas))
                  (atlas-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world texture atlas"
                       :size (list atlas-width atlas-height)
                       :dimensions :2d :format :rgba8-unorm-srgb
                       :usage '(:copy-dst :texture-binding)))))
                  (atlas-view
                    (keep
                     (create device (make-texture-view-descriptor
                                     :texture atlas-texture))))
                  (atlas-sampler
                    (keep
                     (create device (make-sampler-descriptor
                                     :label "block world nearest sampler"
                                     :mag-filter :nearest
                                     :min-filter :nearest
                                     :mipmap-filter :nearest))))
                  (crosshair-vertices
                    (make-block-world-crosshair-vertices
                     (first extent) (second extent)))
                  (crosshair-vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "block world crosshair vertices"
                       :size (* 4 (length crosshair-vertices))
                       :usage '(:vertex)))))
                  (crosshair-vertex-module
                    (keep
                     (create
                      device
                      (make-shader-module-descriptor
                       :label "block world crosshair vertex shader"
                       :code (spv:block-world-crosshair-vertex-shader)))))
                  (layout
                    (keep
                     (create
                      device
                      (make-bind-group-layout-descriptor
                       :label "block world layout"
                       :entries '((:binding 0 :type :texture)
                                  (:binding 1 :type :sampler)
                                  (:binding 2 :type :uniform-buffer))))))
                  (pipeline
                    (let ((artifact
                            (make-live-shader-pipeline
                             :role :block-surface
                             :vertex-role :block-surface
                             :label "block world pipeline"
                             :device device :layout layout
                             :vertex-buffers
                             '((:array-stride 36
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3)
                                 (:shader-location 1 :offset 12
                                  :format :float32x3)
                                 (:shader-location 2 :offset 24
                                  :format :float32x3))))
                             :target-format (canvas-format context)
                             :primitive '(:topology :triangle-list)
                             :depth-stencil
                             '(:format :depth32-float
                               :depth-write-enabled t
                               :depth-compare :less))))
                      (push artifact pipelines)
                      artifact))
                  (crosshair-pipeline
                    (let ((artifact
                            (make-live-shader-pipeline
                             :role :block-crosshair
                             :label "block world crosshair pipeline"
                             :device device :layout layout
                             :vertex-module crosshair-vertex-module
                             :vertex-buffers
                             '((:array-stride 24
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3)
                                 (:shader-location 1 :offset 12
                                  :format :float32x3))))
                             :target-format (canvas-format context)
                             :primitive '(:topology :triangle-list)
                             :depth-stencil
                             '(:format :depth32-float
                               :depth-write-enabled nil
                               :depth-compare :always))))
                      (push artifact pipelines)
                      artifact))
                  (new-demo
                    (make-instance
                     'cube-world-demo
                     :canvas canvas :device device :context context
                     :world world :mesher mesher
                     :production-system production-system
                     :camera (sync-camera-to-player camera player)
                     :player player
                     :residency-radius residency-radius
                     :publication-limit publication-limit
                     :load-schedule-limit load-schedule-limit
                     :mesh-capture-limit mesh-capture-limit
                     :title-base title
                     :atlas-texture atlas-texture :atlas-view atlas-view
                     :atlas-sampler atlas-sampler
                     :color-texture color-texture :color-view color-view
                     :depth-texture depth-texture :depth-view depth-view
                     :layout layout :block-pipeline pipeline
                     :crosshair-vertex-buffer crosshair-vertex-buffer
                     :crosshair-pipeline crosshair-pipeline
                     :resources resources)))
             (write-buffer crosshair-vertex-buffer crosshair-vertices)
             (write-texture
              (device-queue device)
              (make-texture-copy :texture atlas-texture)
              atlas-data
              (make-texture-data-layout
               :bytes-per-row (* atlas-width 4)
               :rows-per-image atlas-height)
              (list atlas-width atlas-height))
             (setf demo new-demo)
             (update-cube-world-demo-title demo)
             (maintain-cube-world-residency demo)
             ;; Startup does not synchronously generate or mesh the whole
             ;; residency window.  The first frame may briefly show sky while
             ;; the nearest immutable products arrive.
             (refresh-cube-world-mesh demo)
             (setf (canvas-event-handler canvas) demo
                   (canvas-clock canvas)
                   (if frames-per-second
                       (make-cadence-clock
                        (lambda (native-canvas timestamp)
                          (declare (ignore native-canvas))
                          (render-cube-world-frame demo timestamp))
                        :frames-per-second frames-per-second)
                       (make-demand-clock))
                   completed-p t)
               demo)))
      (unless completed-p
        (when production-system
          (ignore-errors (stop-production-system production-system)))
        (when demo
          (ignore-errors (destroy-cube-world-chunk-products demo)))
        (dolist (pipeline pipelines)
          (ignore-errors (release-live-shader-pipeline pipeline)))
        (dolist (resource resources)
          (ignore-errors (destroy resource)))
        (close-canvas canvas)
        (when device (destroy device))))))

(defun stop-cube-world-demo (demo)
  "Stop DEMO and explicitly release all of its GPU and canvas resources."
  ;; A native close request may already have set this, but the resources still
  ;; belong to the demo until this explicit teardown.
  (setf (cube-world-demo-running-p demo) nil)
  (let ((canvas (cube-world-demo-canvas demo)))
    (when (eq :open (canvas-state canvas))
      (setf (canvas-clock canvas) (make-demand-clock))
      (when (cube-world-demo-pointer-captured-p demo)
        (ignore-errors (set-canvas-relative-pointer-mode canvas nil))
        (setf (cube-world-demo-pointer-captured-p demo) nil))
      ;; A synchronous no-op after changing the clock is a native-thread
      ;; barrier: an already-running frame has finished before teardown starts.
      (request-canvas-frame canvas (lambda (timestamp)
                                     (declare (ignore timestamp)))))
    (setf (canvas-event-handler canvas) nil)
    ;; Stop CPU publication before releasing any render-owned destination.
    (stop-production-system (cube-world-demo-production-system demo))
    (destroy-cube-world-chunk-products demo)
    (release-live-shader-pipeline (cube-world-demo-block-pipeline demo))
    (release-live-shader-pipeline (cube-world-demo-crosshair-pipeline demo))
    (dolist (resource (cube-world-demo-resources demo))
      (destroy resource))
    (setf (cube-world-demo-resources demo) nil)
    (close-canvas canvas))
  (destroy (cube-world-demo-device demo))
  (values))
