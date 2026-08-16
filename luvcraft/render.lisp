;;; Frame encoding, input handling, and the luvcraft session lifecycle.
;;;
;;; The canvas frame callback is the ownership boundary for all GPU
;;; replacement: shader refresh, mesh publication, and uniform updates all
;;; happen here, on the thread that owns the swapchain.  This file also owns
;;; canvas event handling and the START/STOP pair that creates and releases
;;; every GPU resource the application uses.

(in-package #:luvcraft)

(defclass luvcraft-frame-state ()
  ((uniform-buffer :initarg :uniform-buffer
                   :reader luvcraft-frame-uniform-buffer)
   (scene-bind-group :initarg :scene-bind-group
                     :reader luvcraft-frame-scene-bind-group)
   (shadow-bind-group :initarg :shadow-bind-group
                      :reader luvcraft-frame-shadow-bind-group)
   (world-text-bind-groups :initarg :world-text-bind-groups :initform #()
                           :reader luvcraft-frame-world-text-bind-groups)))

(defconstant +block-world-crosshair-vertex-count+ 24)
(defconstant +luvcraft-shadow-map-size+ 2048)
(luv.arithmetic:define-quantity-constant
    +luvcraft-maximum-frame-duration+ 0.1d0
  :type double-float
  :quantity (:quantity :frame-duration :unit :second))
(luv.arithmetic:define-quantity-constant
    +luvcraft-shadow-half-extent+ 64.0
  :type single-float
  :quantity (:quantity :world-distance :unit :cell))
(luv.arithmetic:define-quantity-constant
    +luvcraft-shadow-depth-radius+ 96.0
  :type single-float
  :quantity (:quantity :world-distance :unit :cell))
(luv.arithmetic:define-quantity-constant
    +luvcraft-shadow-base-bias+ 0.00045
  :type single-float
  :quantity (:quantity :shadow-depth :unit :one :character :difference))
(luv.arithmetic:define-quantity-constant
    +luvcraft-shadow-slope-bias+ 0.0015
  :type single-float
  :quantity (:quantity :shadow-depth :unit :one :character :difference))

(luv.arithmetic:define-quantity-constant
    +luvcraft-shadow-minimum-filter-radius+ 2.0
  :type single-float
  :quantity (:quantity :shadow-filter-radius :unit :one))

(luv.arithmetic:define-quantity-constant
    +luvcraft-shadow-maximum-filter-radius+ 6.0
  :type single-float
  :quantity (:quantity :shadow-filter-radius :unit :one))

(defun ensure-block-atlas-sample-transfer (format)
  "Check the host texture format against the block shader's decoded result."
  (let* ((resource
           (find 'luvcraft.shaders::block-atlas
                 (luv.spir-v:shader-specification-resources
                  (luvcraft.shaders:block-world-fragment-specification))
                 :key #'luv.spir-v:shader-object-name))
         (expected (texture-format-sample-transfer format))
         (declared
           (and resource
                (luv.spir-v:shader-resource-sample-transfer resource))))
    (unless (eq expected declared)
      (error "Block atlas format ~S implies ~S sampling, but the shader declares ~S."
             format expected declared))
    format))

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
    (ensure-vertex-product-contract
     vertices :crosshair-vertices +block-world-crosshair-vertex-count+
     (luvcraft.shaders:block-world-crosshair-vertex-specification))))

(defun make-block-world-sky-vertices ()
  "Make the one fullscreen triangle in normalized clip coordinates."
  (ensure-vertex-product-contract
   (make-array
    9 :element-type 'single-float
    :initial-contents '(-1.0 -1.0 0.5
                        3.0 -1.0 0.5
                        -1.0 3.0 0.5))
   :sky-vertices 3 (luvcraft.shaders:block-world-sky-vertex-specification)))

(defun shadow-frame-rows (camera sky)
  "Pack a texel-stable orthographic light-space transform as four vec4 rows."
  (let* ((center
           ;; Preserve the original single-float shadow calculation before
           ;; the packed frame ABI converts its result.
           (make-vec3 (coerce (camera-x camera) 'single-float)
                      (coerce (camera-y camera) 'single-float)
                      (coerce (camera-z camera) 'single-float)))
         (forward
           (vec3-scale
            (vec3-normalize (sky-frame-parameters-sun-direction sky))
            -1.0))
         ;; The tilted solar orbit never reaches the world-Y pole, so its
         ;; projection is a safe continuous reference.  Switching to world Z
         ;; near noon used to rotate the whole shadow map discontinuously.
         (basis-up (make-vec3 0.0 1.0 0.0))
         (right (vec3-normalize (vec3-cross basis-up forward)))
         (up (vec3-cross forward right))
         (extent +luvcraft-shadow-half-extent+)
         (depth-radius +luvcraft-shadow-depth-radius+)
         (world-units-per-texel
           (/ (* 2.0 extent) +luvcraft-shadow-map-size+))
         (center-right
           (* (round (/ (vec3-dot center right) world-units-per-texel))
              world-units-per-texel))
         (center-up
           (* (round (/ (vec3-dot center up) world-units-per-texel))
              world-units-per-texel)))
    (flet ((lane (axis scale offset)
             (list (* (vec3-x axis) scale)
                   (* (vec3-y axis) scale)
                   (* (vec3-z axis) scale)
                   offset)))
      (append
       (lane right (/ extent) (- (/ center-right extent)))
       (lane up (/ extent) (- (/ center-up extent)))
       (lane forward (/ (* 2.0 depth-radius))
             (- 0.5 (/ (vec3-dot center forward) (* 2.0 depth-radius))))
       '(0.0 0.0 0.0 1.0)))))

(defun frame-uniform-data (session width height)
  "Pack the frame environment: camera lanes plus the evaluated sky.

Lane order must match *FRAME-UNIFORM-MEMBERS* exactly; the construction-time
check in BLOCK-WORLD-CAMERA-UNIFORM-SIZE keeps the two honest."
  (let* ((camera-lanes (camera-uniform-data
                        (luvcraft-session-camera session) width height))
         (sky (sky-frame-parameters (luvcraft-session-sky-clock session)
                                    (luvcraft-session-sky-profile session)))
         (data (make-array (+ (length camera-lanes) 52)
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
        (emit (vec3-x sun) (vec3-y sun) (vec3-z sun)
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
        (apply #'emit
               (append (color (sky-frame-parameters-fog-color sky))
                       (list (if (luvcraft-session-shadow-diagnostic-p session)
                                 1.0
                                 0.0))))
        (emit (/ +luvcraft-shadow-map-size+)
              (/ +luvcraft-shadow-map-size+)
              +luvcraft-shadow-base-bias+
              +luvcraft-shadow-slope-bias+)
        (emit (* 2.0 +luvcraft-shadow-depth-radius+)
              (/ (* 2.0 +luvcraft-shadow-half-extent+)
                 +luvcraft-shadow-map-size+)
              +luvcraft-shadow-minimum-filter-radius+
              +luvcraft-shadow-maximum-filter-radius+)
        (apply #'emit (shadow-frame-rows
                       (luvcraft-session-camera session) sky))))
    (unless (= index (length data))
      (error "Frame uniform packing emitted ~D of ~D lanes."
             index (length data)))
    (let ((declaration
            (luv.arithmetic:value-declaration-for :frame-uniform-data)))
      (unless (typep
               data
               (luv.arithmetic:declaration-representation-type declaration))
        (error "Frame uniform data ~S does not satisfy ~S."
               (type-of data)
               (luv.arithmetic:declaration-representation-type declaration))))
    data))

(defun frame-shader-uniform-product-layout (block)
  "Flatten BLOCK's byte-offset members into frame-buffer float positions.

This is deliberately luvcraft's fixed 32-bit-lane ABI adapter, not a claim
about general uniform-block packing.  The shader owns offsets and member
quantities; the host independently owns the product it writes."
  (let ((bytes (spv:shader-uniform-block-byte-size block))
        (projections nil))
    (unless (zerop (mod bytes 4))
      (error "Frame shader uniform size ~D is not a whole float lane count."
             bytes))
    (dolist (member (spv:shader-uniform-block-members block))
      (let* ((byte-offset (spv:shader-uniform-member-offset member))
             (width
               (spv:shader-type-component-count
                (luv.arithmetic:declaration-representation-type member)))
             (whole
               (luv.arithmetic:declaration-quantity-specification member))
             (layout
               (luv.arithmetic:declaration-quantity-layout member)))
        (unless (and width (zerop (mod byte-offset 4)))
          (error "Frame shader member ~S is not a 32-bit scalar-lane value."
                 (spv:shader-object-name member)))
        (let ((base (/ byte-offset 4)))
          (when whole
            (push
             (luv.arithmetic:make-quantity-projection
              (loop for position below width collect (+ base position))
              whole)
             projections))
          (when layout
            (unless (= width
                       (luv.arithmetic:quantity-layout-extent layout))
              (error "Frame shader member ~S has width ~D but layout ~D."
                     (spv:shader-object-name member) width
                     (luv.arithmetic:quantity-layout-extent layout)))
            (dolist (projection
                     (luv.arithmetic:quantity-layout-projections layout))
              (push
               (luv.arithmetic:make-quantity-projection
                (mapcar
                 (lambda (position) (+ base position))
                 (luv.arithmetic:quantity-projection-positions projection))
                (luv.arithmetic:quantity-projection-specification projection))
               projections))))))
    (luv.arithmetic:make-quantity-layout
     (/ bytes 4) (nreverse projections))))

(defun block-world-camera-uniform-size (session)
  "The frame buffer byte size derived from the shader-visible block layout.

Checked against the host's packed frame data at construction, so growing
the frame uniform cannot silently diverge between shader and host."
  (let* ((block (luvcraft.shaders:block-world-camera-uniform-block))
         (size (spv:shader-uniform-block-byte-size block))
         (declaration
           (luv.arithmetic:value-declaration-for :frame-uniform-data))
         (host-layout
           (luv.arithmetic:declaration-quantity-layout declaration))
         (shader-layout (frame-shader-uniform-product-layout block))
        (bytes (* 4 (length (frame-uniform-data session 1 1)))))
    (unless (= size bytes)
      (error "Frame uniform ABI mismatch: the shader block occupies ~D ~
              bytes but the host packs ~D." size bytes))
    (unless (luv.arithmetic:quantity-layout= host-layout shader-layout)
      (error "Frame uniform semantic ABI mismatch between ~S and shader ~S."
             (luv.arithmetic:declaration-source-form declaration)
             (mapcar #'spv:shader-object-name
                     (spv:shader-uniform-block-members block))))
    size))

(defun remember-luvcraft-resource (session resource)
  (push resource (luvcraft-session-resources session))
  resource)

(defun luvcraft-frame-state (session surface-texture)
  (let ((key
          (canvas-frame-resource-key
           (luvcraft-session-context session) surface-texture)))
    (or (gethash key (luvcraft-session-frame-states session))
      (let ((buffer nil)
            (scene-bind-group nil)
            (shadow-bind-group nil)
            (world-text-bind-groups #())
            (completed-p nil))
        (unwind-protect
             (progn
               (setf buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label "block world camera uniform"
                       :size (block-world-camera-uniform-size session)
                       :usage '(:uniform)))
                     scene-bind-group
                     (create
                      (luvcraft-session-device session)
                      (make-bind-group-descriptor
                       :label "block world scene bindings"
                       :layout (luvcraft-session-layout session)
                       :entries
                       `((:binding 0
                          :resource ,(luvcraft-session-atlas-view session))
                         (:binding 1
                          :resource ,(luvcraft-session-atlas-sampler session))
                         (:binding 2 :resource ,buffer)
                         (:binding 3
                          :resource ,(luvcraft-session-shadow-depth-view
                                       session))
                         (:binding 4
                          :resource ,(luvcraft-session-shadow-depth-sampler
                                       session))
                         (:binding 5
                          :resource
                          ,(luvcraft-session-shadow-comparison-sampler
                            session)))))
                     shadow-bind-group
                     (create
                      (luvcraft-session-device session)
                      (make-bind-group-descriptor
                       :label "block world shadow-pass bindings"
                       :layout (luvcraft-session-shadow-layout session)
                       :entries `((:binding 2 :resource ,buffer))))
                     world-text-bind-groups
                     (if (luvcraft-session-world-text session)
                         (make-world-text-frame-bind-groups
                          (luvcraft-session-world-text session)
                          (luvcraft-session-device session) buffer)
                         #()))
               (remember-luvcraft-resource session buffer)
               (remember-luvcraft-resource session scene-bind-group)
               (remember-luvcraft-resource session shadow-bind-group)
               (dolist (group
                         (remove-duplicates
                          (coerce world-text-bind-groups 'list) :test #'eq))
                 (remember-luvcraft-resource session group))
               (let ((state
                       (make-instance
                        'luvcraft-frame-state
                        :uniform-buffer buffer
                        :scene-bind-group scene-bind-group
                        :shadow-bind-group shadow-bind-group
                        :world-text-bind-groups world-text-bind-groups)))
                 (setf (gethash key
                                (luvcraft-session-frame-states session))
                       state
                       completed-p t)
                 state))
          (unless completed-p
            (dolist (group
                      (remove-duplicates
                       (coerce world-text-bind-groups 'list) :test #'eq))
              (destroy group))
            (when shadow-bind-group (destroy shadow-bind-group))
            (when scene-bind-group (destroy scene-bind-group))
            (when buffer (destroy buffer))))))))

(defun encode-luvcraft-frame
    (session surface-texture encoder &key readback-buffer sample)
  ;; The canvas callback is the ownership boundary for all GPU replacement.
  ;; MOP notifications from SLY workers have only marked these artifacts dirty.
  (with-luvcraft-frame-timing
      (sample luvcraft-frame-sample-shader-refresh-seconds
              :luvcraft/shader-refresh)
    (refresh-luvcraft-shaders session))
  (let* ((products
           (with-luvcraft-frame-timing
               (sample luvcraft-frame-sample-mesh-publication-seconds
                       :luvcraft/mesh-publication)
             (refresh-luvcraft-mesh session)))
         (extent (canvas-extent (luvcraft-session-context session)))
         (frame (luvcraft-frame-state session surface-texture)))
    (when (or sample (tracy-connected-p))
      (let ((mesh-vertices 0)
            (mesh-draws 0))
        (dolist (product products)
          (let ((vertices
                  (block-mesh-vertex-count
                   (luvcraft-chunk-product-mesh product))))
            (when (plusp vertices)
              (incf mesh-draws)
              (incf mesh-vertices vertices))))
        (let* ((resident-chunks
                (length
                 (resident-world-chunks (luvcraft-session-world session))))
              (pending-production
                (production-system-pending-count
                 (luvcraft-session-production-system session)))
              (staged-chunks
                (hash-table-count
                 (luvcraft-session-staged-chunk-products session)))
              (chunks (length products))
              (text-glyph-count
                (if (luvcraft-session-world-text session)
                    (length
                     (world-text-run-glyphs
                      (luvcraft-session-world-text session)))
                    0))
              (draws (+ 2 text-glyph-count (* 2 mesh-draws)))
              (vertices (+ +block-world-crosshair-vertex-count+ 3
                           (* 6 text-glyph-count)
                           (* 2 mesh-vertices))))
          (when sample
            (setf (luvcraft-frame-sample-resident-chunk-count sample)
                  resident-chunks
                  (luvcraft-frame-sample-pending-production-count sample)
                  pending-production
                  (luvcraft-frame-sample-staged-chunk-count sample)
                  staged-chunks
                  (luvcraft-frame-sample-chunk-count sample) chunks
                  (luvcraft-frame-sample-draw-count sample) draws
                  (luvcraft-frame-sample-vertex-count sample) vertices))
          ;; The same counts the benchmark records per sample, drawn
          ;; against the live timeline so a frame-time spike can be read
          ;; against the world that produced it.
          (tracy-plot "resident chunks" resident-chunks)
          (tracy-plot "pending production" pending-production)
          (tracy-plot "staged chunks" staged-chunks)
          (tracy-plot "drawable chunks" chunks)
          (tracy-plot "draws" draws)
          (tracy-plot "vertices" vertices)
          (let ((center (luvcraft-session-residency-center session)))
            (when center
              (tracy-plot "player chunk x" (first center))
              (tracy-plot "player chunk z" (second center)))))))
    (with-luvcraft-frame-timing
        (sample luvcraft-frame-sample-uniform-seconds
                :luvcraft/uniform-update)
      (write-buffer
       (luvcraft-frame-uniform-buffer frame)
       (frame-uniform-data session (first extent) (second extent)))
      (when (luvcraft-session-world-text session)
        (update-world-text-projected-scale
         (luvcraft-session-world-text session)
         (luvcraft-session-camera session) (second extent))))
    (with-luvcraft-frame-timing
        (sample luvcraft-frame-sample-shadow-encode-seconds
                :luvcraft/shadow-pass)
      (let ((pass
              (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :color-attachments nil
                :depth-stencil-attachment
                `(:view ,(luvcraft-session-shadow-depth-view session)
                  :depth-load-op :clear :depth-store-op :store
                  :depth-clear-value 1.0)))))
        (set-pipeline pass (luvcraft-session-shadow-native-pipeline session))
        (set-bind-group pass 0 (luvcraft-frame-shadow-bind-group frame))
        (dolist (product products)
          (let ((mesh (luvcraft-chunk-product-mesh product)))
            (when (plusp (block-mesh-vertex-count mesh))
              (set-vertex-buffer
               pass 0 (luvcraft-chunk-product-vertex-buffer product))
              (draw pass (block-mesh-vertex-count mesh)))))
        (end-pass pass))
      (prepare-texture
       encoder (luvcraft-session-shadow-depth-texture session)
       :texture-binding))
    (with-luvcraft-frame-timing
        (sample luvcraft-frame-sample-scene-encode-seconds
                :luvcraft/scene-pass)
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
        (set-bind-group pass 0 (luvcraft-frame-scene-bind-group frame))
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
        (when (luvcraft-session-world-text session)
          (let ((text (luvcraft-session-world-text session)))
            (set-pipeline pass (world-text-run-native-pipeline text))
            (set-vertex-buffer pass 0 (world-text-run-vertex-buffer text))
            (loop for bind-group across
                    (luvcraft-frame-world-text-bind-groups frame)
                  for first-vertex from 0 by 6
                  do (set-bind-group pass 0 bind-group)
                     (draw pass 6 1 first-vertex))))
        (dolist (overlay (reverse (luvcraft-session-overlays session)))
          (encode-luvcraft-overlay overlay session pass surface-texture))
        (set-pipeline pass (luvcraft-session-crosshair-native-pipeline session))
        (set-bind-group pass 0 (luvcraft-frame-scene-bind-group frame))
        (set-vertex-buffer
         pass 0 (luvcraft-session-crosshair-vertex-buffer session))
        (draw pass +block-world-crosshair-vertex-count+)
        (end-pass pass)))
    (with-luvcraft-frame-timing
        (sample luvcraft-frame-sample-surface-copy-encode-seconds
                :luvcraft/surface-copy)
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
        :destination surface-texture)))))

(defun render-luvcraft-frame (session timestamp &optional sample)
  (when (luvcraft-session-running-p session)
    (describe-luvcraft-tracy-plots)
    (let ((tracy-frame-start
            (and (tracy-connected-p) (get-internal-real-time))))
      (with-luvcraft-frame-timing
          (sample luvcraft-frame-sample-frame-seconds :luvcraft/frame)
        (with-luvcraft-frame-timing
            (sample luvcraft-frame-sample-simulation-seconds
                    :luvcraft/simulation)
          (let* ((last (luvcraft-session-last-frame-time session))
                 (seconds
                   (if last
                       (min +luvcraft-maximum-frame-duration+
                            (max 0d0 (- timestamp last)))
                       0d0)))
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
                               +player-physics-step+))))))
        (with-luvcraft-frame-timing
            (sample luvcraft-frame-sample-streaming-seconds
                    :luvcraft/streaming)
          (with-cpu-trace-zone (:streaming/reconcile-residency)
            (maintain-luvcraft-residency session))
          (with-cpu-trace-zone (:streaming/evict-products)
            (evict-luvcraft-products session)))
        (with-luvcraft-frame-timing
            (sample luvcraft-frame-sample-presentation-seconds
                    :luvcraft/presentation)
          (present-canvas-frame
           (luvcraft-session-context session)
           (lambda (surface-texture encoder)
             (encode-luvcraft-frame
              session surface-texture encoder :sample sample)))))
      (when tracy-frame-start
        (tracy-plot
         "frame CPU ms"
         (* 1000d0
            (/ (- (get-internal-real-time) tracy-frame-start)
               (coerce internal-time-units-per-second 'double-float))))
        (tracy-plot "60 Hz budget ms" (/ 1000d0 60d0)))
      ;; The frame mark closes Tracy's frame outside every zone above, which is
      ;; what lets the viewer draw a frame-time graph and say which frame a zone
      ;; belongs to.
      (tracy-frame-mark))))

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
              (when (and number
                         (<= 1 number (length (placeable-block-kinds))))
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
                                (selected-block *stone-block*)
                                checkpoint-writer
                                (provider *gpu-provider*)
                                (sky-clock (make-instance 'sky-clock))
                                (sky-profile (make-default-sky-profile))
                                (shadow-diagnostic-p nil)
                                (world-text-string "hello, world")
                                (world-text-font-pathname
                                  (cl-dejavu:font-pathname "DejaVuSans.ttf"))
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

Pass :PROVIDER to select the Vulkan or Metal relationship without changing
world, simulation, streaming, or frame orchestration.  Pass :VISIBLE-P NIL to
keep the SDL window hidden while still exercising the real presentation path.
Pass :FRAMES-PER-SECOND NIL for a capture-only demand clock."
  (let* ((canvas (make-sdl-canvas
                  :title title :width width :height height
                  ;; Keep the native window hidden until its first complete
                  ;; terrain frame has been presented.  Showing it here would
                  ;; expose black initialization and sky-only streaming states.
                  :visible-p nil
                  :presentation-api (luv::sdl-presentation-api-for provider)))
         (player (or player (make-player-for-camera camera)))
         (camera (sync-camera-to-player camera player))
         (device nil) (context nil) (resources nil) (pipelines nil)
         (world-text-glyph-cache nil)
         (world-text-run nil)
         (session nil) (production-system nil) (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (progn
           (setf device
                 (request-gpu-device
                  provider (make-device-descriptor :label title))
                 context
                 (make-canvas-context
                  canvas provider
                  (make-canvas-configuration :device device)))
           (setf production-system
                 (make-single-worker-production-system
                  :name "luvcraft chunk producer"))
           (flet ((keep (resource)
                    (push resource resources)
                    resource))
             (let* ((lighting-state (attach-lighting-state world))
                  (extent (canvas-extent context))
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
                  (shadow-depth-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world shadow depth"
                       :size (list +luvcraft-shadow-map-size+
                                   +luvcraft-shadow-map-size+)
                       :dimensions :2d :format :depth32-float
                       :usage '(:render-attachment :texture-binding)))))
                  (shadow-depth-view
                    (keep
                     (create device (make-texture-view-descriptor
                                     :texture shadow-depth-texture))))
                  (shadow-depth-sampler
                    (keep
                     (create device (make-sampler-descriptor
                                     :label "block world shadow sampler"
                                     :mag-filter :nearest
                                     :min-filter :nearest
                                     :mipmap-filter :nearest))))
                  (shadow-comparison-sampler
                    (keep
                     (create device (make-sampler-descriptor
                                     :label "block world shadow comparison sampler"
                                     :mag-filter :linear
                                     :min-filter :linear
                                     :mipmap-filter :nearest
                                     :compare :less-or-equal))))
                  (atlas-width
                    (* +block-atlas-tile-size+ +block-atlas-tile-count+))
                  (atlas-height +block-atlas-tile-size+)
                  (atlas-data (make-block-texture-atlas))
                  (atlas-format
                    (ensure-block-atlas-sample-transfer
                     +block-atlas-texture-format+))
                  (atlas-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world texture atlas"
                       :size (list atlas-width atlas-height)
                       :dimensions :2d :format atlas-format
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
                  (sky-vertices (make-block-world-sky-vertices))
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
                  (layout
                    (keep
                     (create
                      device
                      (make-bind-group-layout-descriptor
                       :label "block world layout"
                       :entries '((:binding 0 :type :texture)
                                  (:binding 1 :type :sampler)
                                  (:binding 2 :type :uniform-buffer)
                                  (:binding 3 :type :texture)
                                  (:binding 4 :type :sampler)
                                  (:binding 5 :type :sampler))))))
                  (shadow-layout
                    (keep
                     (create
                      device
                      (make-bind-group-layout-descriptor
                       :label "block world shadow-pass layout"
                       :entries '((:binding 2 :type :uniform-buffer))))))
                  (pipeline
                    (let ((artifact
                            (make-live-shader-pipeline
                             :role :block-surface
                             :vertex-role :block-surface
                             :label "block world pipeline"
                             :device device :layout layout
                             :vertex-buffers
                             '((:array-stride 48
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3)
                                 (:shader-location 1 :offset 12
                                  :format :float32x3)
                                 (:shader-location 2 :offset 24
                                  :format :float32x3)
                                 (:shader-location 3 :offset 36
                                  :format :float32x3))))
                             :target-format (canvas-format context)
                             :primitive '(:topology :triangle-list)
                             :depth-stencil
                             '(:format :depth32-float
                               :depth-write-enabled t
                               :depth-compare :less))))
                      (push artifact pipelines)
                      artifact))
                  (shadow-pipeline
                    (let ((artifact
                            (make-live-shader-pipeline
                             :role :block-shadow
                             :stage :vertex
                             :label "block world shadow pipeline"
                             :device device :layout shadow-layout
                             :vertex-buffers
                             '((:array-stride 48
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3)
                                 (:shader-location 1 :offset 12
                                  :format :float32x3)
                                 (:shader-location 2 :offset 24
                                  :format :float32x3)
                                 (:shader-location 3 :offset 36
                                  :format :float32x3))))
                             :target-format nil
                             :primitive '(:topology :triangle-list)
                             :depth-stencil
                             '(:format :depth32-float
                               :depth-write-enabled t
                               :depth-compare :less
                               :depth-store-op :store))))
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
                             :vertex-role :block-crosshair
                             :label "block world crosshair pipeline"
                             :device device :layout layout
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
                  (text-glyph-cache
                    (when world-text-string
                      (setf world-text-glyph-cache
                            (make-world-text-glyph-cache device))))
                  (text-run
                    (when text-glyph-cache
                      (setf world-text-run
                            (make-world-text-run
                             device text-glyph-cache camera
                             (canvas-format context)
                             world-text-string world-text-font-pathname))))
                  (new-session
                    (make-instance
                     'luvcraft-session
                     :canvas canvas :device device :context context
                     :world world :mesher mesher
                     :checkpoint-writer checkpoint-writer
                     :production-system production-system
                     :camera camera
                     :player player
                     :selected-block selected-block
                     :lighting-state lighting-state
                     :sky-clock sky-clock :sky-profile sky-profile
                     :shadow-diagnostic-p shadow-diagnostic-p
                     :residency-radius residency-radius
                     :publication-limit publication-limit
                     :load-schedule-limit load-schedule-limit
                     :mesh-capture-limit mesh-capture-limit
                     :title-base title
                     :atlas-texture atlas-texture :atlas-view atlas-view
                     :atlas-sampler atlas-sampler
                     :color-texture color-texture :color-view color-view
                     :depth-texture depth-texture :depth-view depth-view
                     :shadow-depth-texture shadow-depth-texture
                     :shadow-depth-view shadow-depth-view
                     :shadow-depth-sampler shadow-depth-sampler
                     :shadow-comparison-sampler shadow-comparison-sampler
                     :layout layout :shadow-layout shadow-layout
                     :block-pipeline pipeline
                     :shadow-pipeline shadow-pipeline
                     :sky-vertex-buffer sky-vertex-buffer
                     :sky-pipeline sky-pipeline
                     :crosshair-vertex-buffer crosshair-vertex-buffer
                     :crosshair-pipeline crosshair-pipeline
                     :world-text text-run
                     :world-text-glyph-cache text-glyph-cache
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
               (refresh-luvcraft-mesh session)
               (setf (canvas-event-handler canvas) session)
               (when visible-p
                 ;; Preserve asynchronous residency after startup, but do not
                 ;; publish the window until the nearest immutable mesh and one
                 ;; complete frame are ready.
                 (wait-for-luvcraft-products session :minimum 1)
                 (request-canvas-frame
                  canvas
                  (lambda (timestamp)
                    (render-luvcraft-frame session timestamp)))
                 (show-canvas canvas))
               (setf (canvas-clock canvas)
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
        (when world-text-run
          (ignore-errors (release-world-text-run world-text-run)))
        (when world-text-glyph-cache
          (ignore-errors
            (release-world-text-glyph-cache world-text-glyph-cache)))
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
    (dolist (overlay (luvcraft-session-overlays session))
      (release-luvcraft-overlay overlay))
    (setf (luvcraft-session-overlays session) nil)
    (release-live-shader-pipeline (luvcraft-session-block-pipeline session))
    (release-live-shader-pipeline (luvcraft-session-shadow-pipeline session))
    (release-live-shader-pipeline (luvcraft-session-sky-pipeline session))
    (release-live-shader-pipeline (luvcraft-session-crosshair-pipeline session))
    (dolist (resource (luvcraft-session-resources session))
      (destroy resource))
    (setf (luvcraft-session-resources session) nil)
    (when (luvcraft-session-world-text session)
      (release-world-text-run (luvcraft-session-world-text session)))
    (when (luvcraft-session-world-text-glyph-cache session)
      (release-world-text-glyph-cache
       (luvcraft-session-world-text-glyph-cache session)))
    (close-canvas canvas))
  (destroy (luvcraft-session-device session))
  (values))
