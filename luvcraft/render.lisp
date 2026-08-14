;;; Frame encoding, input handling, and the luvcraft session lifecycle.
;;;
;;; The canvas frame callback is the ownership boundary for all GPU
;;; replacement: shader refresh, mesh publication, and uniform updates all
;;; happen here, on the thread that owns the swapchain.  This file also owns
;;; canvas event handling and the START/STOP pair that creates and releases
;;; every GPU resource the application uses.

(in-package #:luv)

(defclass luvcraft-frame-state ()
  ((uniform-buffer :initarg :uniform-buffer
                   :reader luvcraft-frame-uniform-buffer)
   (bind-group :initarg :bind-group :reader luvcraft-frame-bind-group)))

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

(defun frame-uniform-data (session width height)
  "Pack the frame environment: camera lanes plus the evaluated sky.

Lane order must match *FRAME-UNIFORM-MEMBERS* exactly; the construction-time
check in BLOCK-WORLD-CAMERA-UNIFORM-SIZE keeps the two honest."
  (let* ((camera-lanes (camera-uniform-data
                        (luvcraft-session-camera session) width height))
         (sky (sky-frame-parameters (luvcraft-session-sky-clock session)
                                    (luvcraft-session-sky-profile session)))
         (data (make-array (+ (length camera-lanes) 28)
                           :element-type 'single-float))
         (index (length camera-lanes)))
    (replace data camera-lanes)
    (flet ((emit (&rest values)
             (dolist (value values)
               (setf (aref data index) (coerce value 'single-float))
               (incf index)))
           (color (vector) (coerce vector 'list)))
      (let ((sun (sky-frame-parameters-sun-direction sky)))
        (emit (sky-frame-parameters-fog-near sky)
              (sky-frame-parameters-fog-far sky) 0.0 0.0)
        (emit (aref sun 0) (aref sun 1) (aref sun 2)
              (sky-frame-parameters-day-factor sky))
        (apply #'emit (append (color (sky-frame-parameters-sun-color sky))
                              (list (sky-frame-parameters-sun-angular-width
                                     sky))))
        (apply #'emit (append (color (sky-frame-parameters-zenith-color sky))
                              (list 0.0)))
        (apply #'emit (append (color (sky-frame-parameters-horizon-color sky))
                              (list 0.0)))
        (apply #'emit (append (color (sky-frame-parameters-ambient-color sky))
                              (list (sky-frame-parameters-exposure sky))))
        (apply #'emit (append (color (sky-frame-parameters-fog-color sky))
                              (list 0.0)))))
    data))

(defun block-world-camera-uniform-size (session)
  "The frame buffer byte size derived from the shader-visible block layout.

Checked against the host's packed frame data at construction, so growing
the frame uniform cannot silently diverge between shader and host."
  (let ((size (spv:shader-uniform-block-byte-size
               (spv:block-world-camera-uniform-block)))
        (bytes (* 4 (length (frame-uniform-data session 1 1)))))
    (unless (= size bytes)
      (error "Frame uniform ABI mismatch: the shader block occupies ~D ~
              bytes but the host packs ~D." size bytes))
    size))

(defun remember-luvcraft-resource (session resource)
  (push resource (luvcraft-session-resources session))
  resource)

(defun luvcraft-frame-state (session surface-texture)
  (or (gethash surface-texture (luvcraft-session-frame-states session))
      (let ((buffer nil) (bind-group nil) (completed-p nil))
        (unwind-protect
             (progn
               (setf buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label "block world camera uniform"
                       :size (block-world-camera-uniform-size session)
                       :usage '(:uniform)))
                     bind-group
                     (create
                      (luvcraft-session-device session)
                      (make-bind-group-descriptor
                       :label "block world frame bindings"
                       :layout (luvcraft-session-layout session)
                       :entries
                       `((:binding 0
                          :resource ,(luvcraft-session-atlas-view session))
                         (:binding 1
                          :resource ,(luvcraft-session-atlas-sampler session))
                         (:binding 2 :resource ,buffer)))))
               (remember-luvcraft-resource session buffer)
               (remember-luvcraft-resource session bind-group)
               (let ((state
                       (make-instance
                        'luvcraft-frame-state
                        :uniform-buffer buffer :bind-group bind-group)))
                 (setf (gethash surface-texture
                                (luvcraft-session-frame-states session))
                       state
                       completed-p t)
                 state))
          (unless completed-p
            (when bind-group (destroy bind-group))
            (when buffer (destroy buffer)))))))

(defun encode-luvcraft-frame
    (session surface-texture encoder &key readback-buffer)
  ;; The canvas callback is the ownership boundary for all GPU replacement.
  ;; MOP notifications from SLY workers have only marked these artifacts dirty.
  (refresh-luvcraft-shaders session)
  (let* ((products (refresh-luvcraft-mesh session))
         (extent (canvas-extent (luvcraft-session-context session)))
         (frame (luvcraft-frame-state session surface-texture)))
    (write-buffer
     (luvcraft-frame-uniform-buffer frame)
     (frame-uniform-data session (first extent) (second extent)))
    (let ((pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :color-attachments
              `((:view ,(luvcraft-session-color-view session)
                 :load-op :clear :store-op :store
                 :clear-value #(0.43 0.68 0.92 1.0)))
              :depth-stencil-attachment
              `(:view ,(luvcraft-session-depth-view session)
                :depth-load-op :clear :depth-store-op :discard
                :depth-clear-value 1.0)))))
      ;; The sky triangle fills the frame before block geometry, with depth
      ;; writes disabled; the clear value remains only a safe fallback.
      (set-pipeline pass (luvcraft-session-sky-native-pipeline session))
      (set-bind-group pass 0 (luvcraft-frame-bind-group frame))
      (set-vertex-buffer
       pass 0 (luvcraft-session-sky-vertex-buffer session))
      (draw pass 3)
      (set-pipeline pass (luvcraft-session-pipeline session))
      (dolist (product products)
        (let ((mesh (luvcraft-chunk-product-mesh product)))
          (when (plusp (block-mesh-vertex-count mesh))
            (set-vertex-buffer
             pass 0 (luvcraft-chunk-product-vertex-buffer product))
            (draw pass (block-mesh-vertex-count mesh)))))
      (set-pipeline pass (luvcraft-session-crosshair-native-pipeline session))
      (set-vertex-buffer
       pass 0 (luvcraft-session-crosshair-vertex-buffer session))
      (draw pass +block-world-crosshair-vertex-count+)
      (end-pass pass))
    (when readback-buffer
      (encode
       encoder
       (make-gpu-copy-texture-to-buffer-command
        :source (luvcraft-session-color-texture session)
        :destination readback-buffer)))
    (encode
     encoder
     (make-gpu-copy-texture-command
      :source (luvcraft-session-color-texture session)
      :destination surface-texture))))

(defun render-luvcraft-frame (session timestamp)
  (when (luvcraft-session-running-p session)
    (let* ((last (luvcraft-session-last-frame-time session))
           (seconds (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0)))
      (setf (luvcraft-session-last-frame-time session) timestamp)
      (advance-sky-clock (luvcraft-session-sky-clock session) seconds)
      (let ((player (luvcraft-session-player session)))
        (when player
          (incf (luvcraft-session-physics-accumulator session) seconds)
          (loop while (>= (luvcraft-session-physics-accumulator session)
                          +player-physics-step+)
                do (step-block-world-player
                    player (luvcraft-session-world session)
                    (luvcraft-session-camera session)
                    (luvcraft-session-pressed-keys session)
                    +player-physics-step+
                    :jump-p (luvcraft-session-jump-requested-p session))
                   (setf (luvcraft-session-jump-requested-p session) nil)
                   (decf (luvcraft-session-physics-accumulator session)
                         +player-physics-step+)))
      (maintain-luvcraft-residency session)
      (evict-luvcraft-products session)
      (present-canvas-frame
       (luvcraft-session-context session)
       (lambda (surface-texture encoder)
         (encode-luvcraft-frame session surface-texture encoder)))))))

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (if (eq key :escape)
        (when (luvcraft-session-pointer-captured-p session)
          (set-canvas-relative-pointer-mode canvas nil)
          (setf (luvcraft-session-pointer-captured-p session) nil))
        (progn
          (setf (gethash key (luvcraft-session-pressed-keys session)) t)
          (when (and (eq key :space)
                     (not (canvas-key-event-repeat-p event)))
            (setf (luvcraft-session-jump-requested-p session) t))
          (unless (canvas-key-event-repeat-p event)
            (let* ((character (canvas-key-event-character event))
                   (number (and character (digit-char-p character))))
              (when (and number (<= 1 number 7))
                (select-luvcraft-block session number)))))))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (remhash (canvas-key-event-key-name event)
           (luvcraft-session-pressed-keys session))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-pointer-button-press-event))
  (let ((button (canvas-pointer-event-button event)))
    (cond
      ((not (luvcraft-session-pointer-captured-p session))
       (when (eq button :left)
         (set-canvas-relative-pointer-mode canvas t)
         (setf (luvcraft-session-pointer-captured-p session) t)))
      ((eq button :left)
       (edit-luvcraft-block session :remove))
      ((eq button :right)
       (edit-luvcraft-block session :place))
      ((eq button :middle)
       (pick-luvcraft-block session))))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (when (luvcraft-session-pointer-captured-p session)
    (let ((camera (luvcraft-session-camera session)))
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
    ((session luvcraft-session) canvas (event canvas-window-focus-lost-event))
  (declare (ignore event))
  (clrhash (luvcraft-session-pressed-keys session))
  (setf (luvcraft-session-jump-requested-p session) nil)
  (when (luvcraft-session-pointer-captured-p session)
    (set-canvas-relative-pointer-mode canvas nil)
    (setf (luvcraft-session-pointer-captured-p session) nil))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (setf (luvcraft-session-running-p session) nil)
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-event))
  (declare (ignore session canvas event))
  nil)

(defun start-luvcraft (&key
                                (title "luv little block world — click, look, walk")
                                (width 960) (height 640)
                                (frames-per-second 60)
                                (visible-p t)
                                (world (make-empty-little-block-world))
                                (mesher (make-instance
                                         'exposed-face-mesher))
                                (camera (make-instance 'fly-camera))
                                player
                                (sky-clock (make-instance 'sky-clock))
                                (sky-profile (make-default-sky-profile))
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
        (device nil) (context nil) (resources nil) (pipelines nil) (session nil)
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
                  (sky-vertices
                    (make-array
                     9 :element-type 'single-float
                     :initial-contents '(-1.0 -1.0 0.5
                                         3.0 -1.0 0.5
                                         -1.0 3.0 0.5)))
                  (sky-vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "block world sky corners"
                       :size (* 4 (length sky-vertices))
                       :usage '(:vertex)))))
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
                  (sky-pipeline
                    (let ((artifact
                            (make-live-shader-pipeline
                             :role :sky
                             :vertex-role :sky
                             :label "block world sky pipeline"
                             :device device :layout layout
                             :vertex-buffers
                             '((:array-stride 12
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3))))
                             :target-format (canvas-format context)
                             :primitive '(:topology :triangle-list)
                             :depth-stencil
                             '(:format :depth32-float
                               :depth-write-enabled nil
                               :depth-compare :always))))
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
                  (new-session
                    (make-instance
                     'luvcraft-session
                     :canvas canvas :device device :context context
                     :world world :mesher mesher
                     :production-system production-system
                     :camera (sync-camera-to-player camera player)
                     :player player
                     :sky-clock sky-clock :sky-profile sky-profile
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
                     :sky-vertex-buffer sky-vertex-buffer
                     :sky-pipeline sky-pipeline
                     :crosshair-vertex-buffer crosshair-vertex-buffer
                     :crosshair-pipeline crosshair-pipeline
                     :resources resources)))
             (write-buffer sky-vertex-buffer sky-vertices)
             (write-buffer crosshair-vertex-buffer crosshair-vertices)
             (write-texture
              (device-queue device)
              (make-texture-copy :texture atlas-texture)
              atlas-data
              (make-texture-data-layout
               :bytes-per-row (* atlas-width 4)
               :rows-per-image atlas-height)
              (list atlas-width atlas-height))
             (setf session new-session)
             (update-luvcraft-session-title session)
             (maintain-luvcraft-residency session)
             ;; Startup does not synchronously generate or mesh the whole
             ;; residency window.  The first frame may briefly show sky while
             ;; the nearest immutable products arrive.
             (refresh-luvcraft-mesh session)
             (setf (canvas-event-handler canvas) session
                   (canvas-clock canvas)
                   (if frames-per-second
                       (make-cadence-clock
                        (lambda (native-canvas timestamp)
                          (declare (ignore native-canvas))
                          (render-luvcraft-frame session timestamp))
                        :frames-per-second frames-per-second)
                       (make-demand-clock))
                   completed-p t)
               session)))
      (unless completed-p
        (when production-system
          (ignore-errors (stop-production-system production-system)))
        (when session
          (ignore-errors (destroy-luvcraft-chunk-products session)))
        (dolist (pipeline pipelines)
          (ignore-errors (release-live-shader-pipeline pipeline)))
        (dolist (resource resources)
          (ignore-errors (destroy resource)))
        (close-canvas canvas)
        (when device (destroy device))))))

(defun stop-luvcraft (session)
  "Stop SESSION and explicitly release all of its GPU and canvas resources."
  ;; A native close request may already have set this, but the resources still
  ;; belong to the session until this explicit teardown.
  (setf (luvcraft-session-running-p session) nil)
  (let ((canvas (luvcraft-session-canvas session)))
    (when (eq :open (canvas-state canvas))
      (setf (canvas-clock canvas) (make-demand-clock))
      (when (luvcraft-session-pointer-captured-p session)
        (ignore-errors (set-canvas-relative-pointer-mode canvas nil))
        (setf (luvcraft-session-pointer-captured-p session) nil))
      ;; A synchronous no-op after changing the clock is a native-thread
      ;; barrier: an already-running frame has finished before teardown starts.
      (request-canvas-frame canvas (lambda (timestamp)
                                     (declare (ignore timestamp)))))
    (setf (canvas-event-handler canvas) nil)
    ;; Stop CPU publication before releasing any render-owned destination.
    (stop-production-system (luvcraft-session-production-system session))
    (destroy-luvcraft-chunk-products session)
    (release-live-shader-pipeline (luvcraft-session-block-pipeline session))
    (release-live-shader-pipeline (luvcraft-session-sky-pipeline session))
    (release-live-shader-pipeline (luvcraft-session-crosshair-pipeline session))
    (dolist (resource (luvcraft-session-resources session))
      (destroy resource))
    (setf (luvcraft-session-resources session) nil)
    (close-canvas canvas))
  (destroy (luvcraft-session-device session))
  (values))
