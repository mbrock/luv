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
   (post-uniform-buffer :initarg :post-uniform-buffer
                        :reader luvcraft-frame-post-uniform-buffer)
   (post-bind-group :initarg :post-bind-group
                    :reader luvcraft-frame-post-bind-group)
   ;; The lens chain reads three different sources through one layout: the
   ;; scene once, then each reduced attachment in turn.
   (bloom-scene-bind-group :initarg :bloom-scene-bind-group :initform nil
                           :reader luvcraft-frame-bloom-scene-bind-group)
   (bloom-primary-bind-group :initarg :bloom-primary-bind-group :initform nil
                             :reader luvcraft-frame-bloom-primary-bind-group)
   (bloom-secondary-bind-group :initarg :bloom-secondary-bind-group
                               :initform nil
                               :reader luvcraft-frame-bloom-secondary-bind-group)
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
         ;; Texel snapping stabilizes the map against translation but can do
         ;; nothing about rotation, so the free choice of roll about the light
         ;; axis is worth making well.  World up is a continuous reference --
         ;; the tilted orbit never reaches the Y pole -- but it is a badly
         ;; conditioned one: the sun's angle to it changes all day, the
         ;; transverse component collapses toward the orbit tilt as the sun
         ;; climbs, and the roll rate consequently peaks at noon, spinning the
         ;; whole texel grid under the world exactly when the sun is highest.
         ;; The sun's own axis of revolution keeps a constant angle to the sun
         ;; at every hour, so the basis it induces simply turns with the day.
         ;; #0604PY measures both the gain at noon and what it costs at dusk.
         (reference (sky-sun-orbit-axis))
         (right (vec3-normalize (vec3-cross reference forward)))
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

(defun frame-uniform-data (session width height &key camera-lanes)
  "Pack the frame environment: camera lanes plus the evaluated sky.

Lane order must match *FRAME-UNIFORM-MEMBERS* exactly; the construction-time
check in BLOCK-WORLD-CAMERA-UNIFORM-SIZE keeps the two honest.  CAMERA-LANES
may replace the session camera's own five lanes with a camera expressed in
some other space; the environment lanes are packed the same either way."
  (let* ((camera-lanes (or camera-lanes
                           (camera-uniform-data
                            (luvcraft-session-camera session) width height)))
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
        ;; The sky's cloud deck drifts, so the frame environment carries a
        ;; bounded elapsed time alongside the fog band it shares a lane with.
        (emit (sky-frame-parameters-fog-near sky)
              (sky-frame-parameters-fog-far sky)
              (mod (or (luvcraft-session-last-frame-time session) 0d0) 3600d0)
              (sky-frame-parameters-cloudiness sky))
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

;;; Grading is art direction, not architecture: these are the live knobs the
;;; presentation stack reads every frame, so a SLY eval can retune the whole
;;; look of the running game without rebuilding a pipeline.

(defparameter *luvcraft-exposure* 0.85
  "Overall scene exposure multiplied into the sky profile's own exposure.")

(defparameter *luvcraft-bloom-gain* 0.55
  "How much of the blurred bright-pass image is added back in linear light.")

(defparameter *luvcraft-shaft-gain* 0.85
  "How strongly sunlight scattered around the solar disc streaks the frame.")

(defparameter *luvcraft-vignette* 0.16
  "Corner falloff of the presented frame, as a fraction of full brightness.")

(defparameter *luvcraft-bloom-threshold* 1.5
  "Luminance at which a fragment starts contributing to the bloom chain.")

(defparameter *luvcraft-shaft-decay* 0.955
  "Per-tap attenuation along a light shaft; nearer one reaches further.")

(defun luvcraft-sun-screen-position (camera sky width height)
  "The sun's presentation UV and how strongly it counts as on screen.

The weight fades the solar lens effects out as the disc leaves the frame or
falls behind the camera, so a turn of the head does not pop the shafts."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let* ((sun (sky-frame-parameters-sun-direction sky))
           (view-z (vec3-dot forward sun)))
      (if (<= view-z 0.02)
          (values 0.5 0.5 0.0)
          (let* ((focal (/ (tan (/ (camera-field-of-view camera) 2.0))))
                 (aspect (/ (coerce width 'single-float) height))
                 (clip-x (/ (* (vec3-dot right sun) (/ focal aspect)) view-z))
                 (clip-y (- (/ (* (vec3-dot up sun) focal) view-z)))
                 (edge (max (abs clip-x) (abs clip-y)))
                 (weight (* (sky-smoothstep 1.45 0.80 edge)
                            (sky-smoothstep 0.02 0.30 view-z)
                            (sky-frame-parameters-day-factor sky))))
            (values (coerce (* 0.5 (+ clip-x 1.0)) 'single-float)
                    (coerce (* 0.5 (+ clip-y 1.0)) 'single-float)
                    (coerce weight 'single-float)))))))

(defun luvcraft-post-uniform-data (session width height)
  "Pack the presentation environment: texel size, lens gains, and the sun.

Lane order must match *POST-UNIFORM-MEMBERS* exactly; the construction-time
check in LUVCRAFT-POST-UNIFORM-SIZE keeps the two honest."
  (let* ((camera (luvcraft-session-camera session))
         (sky (sky-frame-parameters (luvcraft-session-sky-clock session)
                                    (luvcraft-session-sky-profile session))))
    (multiple-value-bind (sun-u sun-v sun-weight)
        (luvcraft-sun-screen-position camera sky width height)
      (let ((bloom-extent (luvcraft-bloom-extent (list width height)))
            (elapsed (or (luvcraft-session-last-frame-time session) 0d0)))
        (make-array
         16 :element-type 'single-float
         :initial-contents
         (mapcar
          (lambda (value) (coerce value 'single-float))
          (list (/ 1.0 width) (/ 1.0 height)
                (if (luvcraft-session-modal-focus session) 1.0 0.0)
                (* *luvcraft-exposure* (sky-frame-parameters-exposure sky))
                *luvcraft-bloom-gain*
                (* *luvcraft-shaft-gain* sun-weight)
                *luvcraft-vignette*
                *luvcraft-bloom-threshold*
                sun-u sun-v sun-weight
                (if (luvcraft-session-shadow-diagnostic-p session) 1.0 0.0)
                (/ 1.0 (first bloom-extent))
                (/ 1.0 (second bloom-extent))
                *luvcraft-shaft-decay*
                (mod elapsed 3600d0))))))))

(defun luvcraft-post-uniform-size (session)
  "The presentation buffer byte size derived from the shader-visible block."
  (let* ((block (luvcraft.shaders:focus-post-uniform-block))
         (size (spv:shader-uniform-block-byte-size block))
         (bytes (* 4 (length (luvcraft-post-uniform-data session 1 1)))))
    (unless (= size bytes)
      (error "Presentation uniform ABI mismatch: the shader block occupies ~
              ~D bytes but the host packs ~D." size bytes))
    size))

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

(defun forget-luvcraft-resource (session resource)
  "Drop RESOURCE from SESSION's release list, for a caller releasing it now."
  (setf (luvcraft-session-resources session)
        (delete resource (luvcraft-session-resources session) :test #'eq))
  resource)

(defun release-luvcraft-resource (session resource)
  (when resource
    (forget-luvcraft-resource session resource)
    (destroy resource))
  (values))

(defun make-luvcraft-bloom-bind-group (session uniform-buffer view label)
  "Bind one lens-chain source: a sampled texture, linear filter, uniforms."
  (create
   (luvcraft-session-device session)
   (make-bind-group-descriptor
    :label label
    :layout (luvcraft-session-bloom-layout session)
    :entries `((:binding 0 :resource ,view)
               (:binding 1
                :resource ,(luvcraft-session-linear-sampler session))
               (:binding 2 :resource ,uniform-buffer)))))

(defun luvcraft-bloom-extent (extent)
  "The reduced attachment size the lens chain runs at, at least one texel."
  (list (max 1 (floor (first extent) +luvcraft-bloom-divisor+))
        (max 1 (floor (second extent) +luvcraft-bloom-divisor+))))

;;; Everything below is sized to the frame rather than to the world, which
;;; means a window resize invalidates all of it at once.  Keeping the whole
;;; set behind one constructor is what lets START-LUVCRAFT and a live resize
;;; agree on formats and usages without either one drifting.

(defun make-luvcraft-frame-attachments (device context extent)
  "Create every frame-sized attachment for EXTENT as a plist of GPU objects.

The scene is drawn into a linear HDR colour attachment with its own depth
buffer, the lens chain runs at a reduced extent, and the tonemapped result
lands in a presentation image that is finally copied onto the drawable."
  (let ((bloom-extent (luvcraft-bloom-extent extent))
        (made nil)
        (completed-p nil))
    (flet ((texture (label size format usage)
             (let ((texture
                     (create device
                             (make-texture-descriptor
                              :label label :size size :dimensions :2d
                              :format format :usage usage))))
               (push texture made)
               (let ((view
                       (create device
                               (make-texture-view-descriptor
                                :texture texture))))
                 (push view made)
                 (values texture view)))))
      (unwind-protect
           (multiple-value-bind (color-texture color-view)
               (texture "block world color" extent
                        +luvcraft-scene-color-format+
                        '(:render-attachment :texture-binding :copy-src))
             (multiple-value-bind (depth-texture depth-view)
                 (texture "block world depth" extent :depth32-float
                          '(:render-attachment :texture-binding))
               (multiple-value-bind (bloom-primary-texture bloom-primary-view)
                   (texture "block world bloom primary" bloom-extent
                            +luvcraft-bloom-color-format+
                            '(:render-attachment :texture-binding))
                 (multiple-value-bind
                       (bloom-secondary-texture bloom-secondary-view)
                     (texture "block world bloom secondary" bloom-extent
                              +luvcraft-bloom-color-format+
                              '(:render-attachment :texture-binding))
                   (multiple-value-bind (presentation-texture presentation-view)
                       (texture "block world presentation color" extent
                                (canvas-format context)
                                '(:render-attachment :copy-src))
                     (setf completed-p t)
                     (list :render-extent extent
                           :color-texture color-texture
                           :color-view color-view
                           :depth-texture depth-texture
                           :depth-view depth-view
                           :bloom-primary-texture bloom-primary-texture
                           :bloom-primary-view bloom-primary-view
                           :bloom-secondary-texture bloom-secondary-texture
                           :bloom-secondary-view bloom-secondary-view
                           :presentation-texture presentation-texture
                           :presentation-view presentation-view))))))
        (unless completed-p
          (mapc #'destroy made))))))

(defun install-luvcraft-frame-attachments (session attachments)
  "Adopt ATTACHMENTS as SESSION's frame-sized images, releasing them with it."
  (flet ((adopt (key)
           (remember-luvcraft-resource session (getf attachments key))))
    (setf (luvcraft-session-color-texture session) (adopt :color-texture)
          (luvcraft-session-color-view session) (adopt :color-view)
          (luvcraft-session-depth-texture session) (adopt :depth-texture)
          (luvcraft-session-depth-view session) (adopt :depth-view)
          (luvcraft-session-bloom-primary-texture session)
          (adopt :bloom-primary-texture)
          (luvcraft-session-bloom-primary-view session)
          (adopt :bloom-primary-view)
          (luvcraft-session-bloom-secondary-texture session)
          (adopt :bloom-secondary-texture)
          (luvcraft-session-bloom-secondary-view session)
          (adopt :bloom-secondary-view)
          (luvcraft-session-presentation-texture session)
          (adopt :presentation-texture)
          (luvcraft-session-presentation-view session)
          (adopt :presentation-view)
          (luvcraft-session-render-extent session)
          (getf attachments :render-extent)))
  session)

(defun release-luvcraft-frame-attachments (session)
  "Release the frame-sized images SESSION is holding, if it has any yet."
  (dolist (resource
           (list (luvcraft-session-color-view session)
                 (luvcraft-session-color-texture session)
                 (luvcraft-session-depth-view session)
                 (luvcraft-session-depth-texture session)
                 (luvcraft-session-bloom-primary-view session)
                 (luvcraft-session-bloom-primary-texture session)
                 (luvcraft-session-bloom-secondary-view session)
                 (luvcraft-session-bloom-secondary-texture session)
                 (luvcraft-session-presentation-view session)
                 (luvcraft-session-presentation-texture session)))
    (release-luvcraft-resource session resource))
  (setf (luvcraft-session-render-extent session) nil)
  (values))

(defun make-luvcraft-lens-pipeline (device layout role label)
  "One fullscreen lens-chain stage, named by the fragment method it runs."
  (make-live-shader-pipeline
   :role role
   :vertex-role :focus-post
   :label label
   :device device :layout layout
   :vertex-buffers
   '((:array-stride 12
      :attributes ((:shader-location 0 :offset 0 :format :float32x3))))
   :target-format +luvcraft-bloom-color-format+
   :primitive '(:topology :triangle-list)
   :depth-stencil nil))

(defun luvcraft-frame-state (session surface-texture)
  (let ((key
          (canvas-frame-resource-key
           (luvcraft-session-context session) surface-texture)))
    (or (gethash key (luvcraft-session-frame-states session))
      (let ((buffer nil)
            (scene-bind-group nil)
            (shadow-bind-group nil)
            (post-uniform-buffer nil)
            (post-bind-group nil)
            (bloom-scene-bind-group nil)
            (bloom-primary-bind-group nil)
            (bloom-secondary-bind-group nil)
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
                            session))
                         (:binding 6
                          :resource
                          ,(luvcraft-session-normal-atlas-view session)))))
                     shadow-bind-group
                     (create
                      (luvcraft-session-device session)
                      (make-bind-group-descriptor
                       :label "block world shadow-pass bindings"
                       :layout (luvcraft-session-shadow-layout session)
                       :entries `((:binding 2 :resource ,buffer))))
                     post-uniform-buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label "block world focus post uniform"
                       :size (luvcraft-post-uniform-size session)
                       :usage '(:uniform)))
                     post-bind-group
                     (create
                      (luvcraft-session-device session)
                      (make-bind-group-descriptor
                       :label "block world focus post bindings"
                       :layout (luvcraft-session-post-layout session)
                       :entries
                       `((:binding 0
                          :resource ,(luvcraft-session-color-view session))
                         (:binding 1
                          :resource ,(luvcraft-session-linear-sampler session))
                         (:binding 2
                          :resource ,(luvcraft-session-depth-view session))
                         (:binding 3 :resource ,post-uniform-buffer)
                         (:binding 4
                          :resource ,(luvcraft-session-bloom-primary-view
                                       session))
                         (:binding 5
                          :resource ,(luvcraft-session-bloom-secondary-view
                                       session))
                         (:binding 6
                          :resource ,(luvcraft-session-atlas-sampler session)))))
                     bloom-scene-bind-group
                     (make-luvcraft-bloom-bind-group
                      session post-uniform-buffer
                      (luvcraft-session-color-view session)
                      "block world bloom scene bindings")
                     bloom-primary-bind-group
                     (make-luvcraft-bloom-bind-group
                      session post-uniform-buffer
                      (luvcraft-session-bloom-primary-view session)
                      "block world bloom primary bindings")
                     bloom-secondary-bind-group
                     (make-luvcraft-bloom-bind-group
                      session post-uniform-buffer
                      (luvcraft-session-bloom-secondary-view session)
                      "block world bloom secondary bindings")
                     world-text-bind-groups
                     (if (luvcraft-session-world-text session)
                         (make-world-text-frame-bind-groups
                          (luvcraft-session-world-text session)
                          (luvcraft-session-device session) buffer)
                         #()))
               (remember-luvcraft-resource session buffer)
               (remember-luvcraft-resource session scene-bind-group)
               (remember-luvcraft-resource session shadow-bind-group)
               (remember-luvcraft-resource session post-uniform-buffer)
               (remember-luvcraft-resource session post-bind-group)
               (remember-luvcraft-resource session bloom-scene-bind-group)
               (remember-luvcraft-resource session bloom-primary-bind-group)
               (remember-luvcraft-resource session bloom-secondary-bind-group)
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
                        :post-uniform-buffer post-uniform-buffer
                        :post-bind-group post-bind-group
                        :bloom-scene-bind-group bloom-scene-bind-group
                        :bloom-primary-bind-group bloom-primary-bind-group
                        :bloom-secondary-bind-group bloom-secondary-bind-group
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
            (when bloom-scene-bind-group (destroy bloom-scene-bind-group))
            (when bloom-primary-bind-group (destroy bloom-primary-bind-group))
            (when bloom-secondary-bind-group
              (destroy bloom-secondary-bind-group))
            (when post-bind-group (destroy post-bind-group))
            (when post-uniform-buffer (destroy post-uniform-buffer))
            (when buffer (destroy buffer))))))))

(defun discard-luvcraft-frame-states (session)
  "Forget every cached per-drawable binding, which names images that are gone.

The bind groups are keyed by drawable, not by size, so nothing else would
notice that their scene, depth, and lens-chain views belong to the previous
window.  Dropping them here makes the next frame rebuild them against the
attachments the session actually holds."
  (let ((states (luvcraft-session-frame-states session)))
    (maphash
     (lambda (key state)
       (declare (ignore key))
       (dolist (resource
                (list* (luvcraft-frame-scene-bind-group state)
                       (luvcraft-frame-shadow-bind-group state)
                       (luvcraft-frame-post-bind-group state)
                       (luvcraft-frame-bloom-scene-bind-group state)
                       (luvcraft-frame-bloom-primary-bind-group state)
                       (luvcraft-frame-bloom-secondary-bind-group state)
                       (luvcraft-frame-post-uniform-buffer state)
                       (luvcraft-frame-uniform-buffer state)
                       (remove-duplicates
                        (coerce (luvcraft-frame-world-text-bind-groups state)
                                'list)
                        :test #'eq)))
         (release-luvcraft-resource session resource)))
     states)
    (clrhash states))
  (values))

(defun ensure-luvcraft-frame-extent (session)
  "Rebuild SESSION's frame-sized images when the drawable has changed size.

A window resize gives the canvas a new drawable extent while every scene,
depth, lens-chain, and presentation image still has the old one; the final
copy onto the drawable is then a size mismatch, which is how a resize used
to end the game.  This runs at the top of a frame, inside the canvas
callback that owns GPU replacement, and after the backend has already
synchronized the drawable, so the extent asked for here is the extent this
frame will present to.  The outgoing images stay alive until the last
submission that used them completes."
  (let ((extent (canvas-extent (luvcraft-session-context session))))
    (unless (equal extent (luvcraft-session-render-extent session))
      (log-event :luvcraft "reframing ~{~D~^x~} to ~{~D~^x~}"
                 (or (luvcraft-session-render-extent session) '(0 0)) extent)
      (let ((attachments
              (make-luvcraft-frame-attachments
               (luvcraft-session-device session)
               (luvcraft-session-context session)
               extent)))
        (discard-luvcraft-frame-states session)
        (release-luvcraft-frame-attachments session)
        (install-luvcraft-frame-attachments session attachments))
      ;; The crosshair is measured in pixels around the centre of the frame,
      ;; so its clip-space vertices are only correct for one extent.
      (write-buffer
       (luvcraft-session-crosshair-vertex-buffer session)
       (make-block-world-crosshair-vertices (first extent) (second extent))))
    extent))

(defun encode-luvcraft-frame
    (session surface-texture encoder &key readback-buffer sample)
  ;; The canvas callback is the ownership boundary for all GPU replacement.
  ;; MOP notifications from SLY workers have only marked these artifacts dirty.
  (ensure-luvcraft-frame-extent session)
  (with-luvcraft-frame-timing
      (sample luvcraft-frame-sample-shader-refresh-seconds
              :luvcraft/shader-refresh)
    (refresh-luvcraft-shaders session))
  ;; The film's clock runs on the world's frames, so it advances here, before
  ;; anything is encoded: the upload is an ordinary queue write, not part of
  ;; this frame's command stream.
  (when (luvcraft-session-video-screen session)
    (advance-video-screen (luvcraft-session-video-screen session)
                          (luvcraft-session-device session)))
  (dolist (overlay (luvcraft-session-overlays session))
    (refresh-luvcraft-overlay overlay session))
  (let* ((products
           (with-luvcraft-frame-timing
               (sample luvcraft-frame-sample-mesh-publication-seconds
                       :luvcraft/mesh-publication)
             (refresh-luvcraft-mesh session)))
         (extent (canvas-extent (luvcraft-session-context session)))
         (frame (luvcraft-frame-state session surface-texture))
         (particle-vertices
           (block-particle-vertices
            (luvcraft-session-particle-system session)))
         (particle-vertex-count
           (/ (length particle-vertices) +block-mesh-floats-per-vertex+))
         (critter-vertices
           (critter-vertices (luvcraft-session-critters session)
                             (luvcraft-session-world session)))
         (critter-vertex-count
           (/ (length critter-vertices) +block-mesh-floats-per-vertex+))
         (body-vertices (player-body-vertices session))
         (body-vertex-count
           (/ (length body-vertices) +block-mesh-floats-per-vertex+)))
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
              (draws (+ 2 (if (plusp text-glyph-count) 1 0)
                        (* 2 mesh-draws)
                        (if (plusp particle-vertex-count) 1 0)
                        ;; The animals are drawn twice: once into the shadow
                        ;; map and once into the scene.
                        (if (plusp critter-vertex-count) 2 0)
                        (if (plusp body-vertex-count) 1 0)))
              (vertices (+ +block-world-crosshair-vertex-count+ 3
                           (* 6 text-glyph-count)
                           particle-vertex-count
                           body-vertex-count
                           (* 2 critter-vertex-count)
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
      (write-buffer
       (luvcraft-frame-post-uniform-buffer frame)
       (luvcraft-post-uniform-data session (first extent) (second extent)))
      (when (plusp particle-vertex-count)
        (write-buffer
         (luvcraft-session-particle-vertex-buffer session)
         particle-vertices))
      (when (plusp critter-vertex-count)
        (write-buffer
         (luvcraft-session-critter-vertex-buffer session)
         critter-vertices))
      (when (plusp body-vertex-count)
        (write-buffer
         (luvcraft-session-body-vertex-buffer session)
         body-vertices)))
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
                  :depth-load-op :clear
                  :depth-store-op :store
                  :depth-clear-value 1.0)))))
        (set-pipeline pass (luvcraft-session-shadow-native-pipeline session))
        (set-bind-group pass 0 (luvcraft-frame-shadow-bind-group frame))
        (dolist (product products)
          (let ((mesh (luvcraft-chunk-product-mesh product)))
            (when (plusp (block-mesh-vertex-count mesh))
              (set-vertex-buffer
               pass 0 (luvcraft-chunk-product-vertex-buffer product))
              (draw pass (block-mesh-vertex-count mesh)))))
        ;; An animal standing in the sun casts a shadow like anything else
        ;; solid; a tumbling smash fragment deliberately does not.
        (when (plusp critter-vertex-count)
          (set-vertex-buffer
           pass 0 (luvcraft-session-critter-vertex-buffer session))
          (draw pass critter-vertex-count))
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
                  :depth-load-op :clear :depth-store-op :store
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
        (when (plusp critter-vertex-count)
          (set-vertex-buffer
           pass 0 (luvcraft-session-critter-vertex-buffer session))
          (draw pass critter-vertex-count))
        ;; The player's own arms and whatever they hold, drawn in the scene
        ;; but not into the shadow map: a pair of floating forearms would
        ;; throw a shadow that explains nothing.
        (when (plusp body-vertex-count)
          (set-vertex-buffer
           pass 0 (luvcraft-session-body-vertex-buffer session))
          (draw pass body-vertex-count))
        (when (plusp particle-vertex-count)
          (set-vertex-buffer
           pass 0 (luvcraft-session-particle-vertex-buffer session))
          (draw pass particle-vertex-count))
        ;; Before the text, so a caption drawn over the screen wins.
        (when (luvcraft-session-video-screen session)
          (let* ((screen (luvcraft-session-video-screen session))
                 (group
                   (refresh-video-screen-bind-group
                    screen (luvcraft-session-device session)
                    (luvcraft-frame-uniform-buffer frame))))
            (set-pipeline pass (video-screen-native-pipeline screen))
            (set-vertex-buffer pass 0 (video-screen-vertex-buffer screen))
            (set-vertex-buffer pass 1 (video-screen-instance-buffer screen))
            (set-bind-group pass 0 group)
            (draw pass 6 1)))
        (when (luvcraft-session-world-text session)
          (let ((text (luvcraft-session-world-text session)))
            (set-pipeline pass (world-text-run-native-pipeline text))
            (set-vertex-buffer pass 0 (world-text-run-vertex-buffer text))
            (set-vertex-buffer pass 1 (world-text-run-instance-buffer text))
            (set-bind-group
             pass 0 (aref (luvcraft-frame-world-text-bind-groups frame) 0))
            (draw pass 6 (length (world-text-run-glyphs text)))))
        (dolist (overlay (reverse (luvcraft-session-overlays session)))
          (when (eq :scene (luvcraft-overlay-stage overlay))
            (encode-luvcraft-overlay overlay session pass surface-texture)))
        (unless (luvcraft-session-modal-focus session)
          (set-pipeline pass (luvcraft-session-crosshair-native-pipeline session))
          (set-bind-group pass 0 (luvcraft-frame-scene-bind-group frame))
          (set-vertex-buffer
           pass 0 (luvcraft-session-crosshair-vertex-buffer session))
          (draw pass +block-world-crosshair-vertex-count+))
        (end-pass pass))
      (prepare-texture encoder (luvcraft-session-color-texture session)
                       :texture-binding)
      (prepare-texture encoder (luvcraft-session-depth-texture session)
                       :texture-binding)
      ;; The lens chain: bright pass, separable blur, radial sun sweep.  Each
      ;; stage is one fullscreen triangle over the reduced attachment pair,
      ;; leaving the blurred bloom in the primary and the shafts in the
      ;; secondary for presentation to add back.
      (flet ((lens-stage (pipeline bind-group view texture)
               (let ((pass
                       (begin-render-pass
                        encoder
                        (make-render-pass-descriptor
                         :color-attachments
                         `((:view ,view :load-op :clear :store-op :store
                            :clear-value #(0.0 0.0 0.0 1.0)))
                         :depth-stencil-attachment nil))))
                 (set-pipeline pass (live-shader-pipeline-native-pipeline
                                     pipeline))
                 (set-bind-group pass 0 bind-group)
                 (set-vertex-buffer
                  pass 0 (luvcraft-session-sky-vertex-buffer session))
                 (draw pass 3)
                 (end-pass pass))
               (prepare-texture encoder texture :texture-binding)))
        (let ((primary-view (luvcraft-session-bloom-primary-view session))
              (primary (luvcraft-session-bloom-primary-texture session))
              (secondary-view (luvcraft-session-bloom-secondary-view session))
              (secondary (luvcraft-session-bloom-secondary-texture session)))
          (lens-stage (luvcraft-session-bloom-bright-pipeline session)
                      (luvcraft-frame-bloom-scene-bind-group frame)
                      primary-view primary)
          (lens-stage (luvcraft-session-bloom-horizontal-pipeline session)
                      (luvcraft-frame-bloom-primary-bind-group frame)
                      secondary-view secondary)
          (lens-stage (luvcraft-session-bloom-vertical-pipeline session)
                      (luvcraft-frame-bloom-secondary-bind-group frame)
                      primary-view primary)
          (lens-stage (luvcraft-session-sun-shaft-pipeline session)
                      (luvcraft-frame-bloom-primary-bind-group frame)
                      secondary-view secondary)))
      (let ((pass
              (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :color-attachments
                `((:view ,(luvcraft-session-presentation-view session)
                   :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 1.0)))
                :depth-stencil-attachment nil))))
        (set-pipeline pass (luvcraft-session-post-native-pipeline session))
        (set-bind-group pass 0 (luvcraft-frame-post-bind-group frame))
        (set-vertex-buffer pass 0 (luvcraft-session-sky-vertex-buffer session))
        (draw pass 3)
        (dolist (overlay (reverse (luvcraft-session-overlays session)))
          (when (eq :hud (luvcraft-overlay-stage overlay))
            (encode-luvcraft-overlay overlay session pass surface-texture)))
        (end-pass pass)))
    (with-luvcraft-frame-timing
        (sample luvcraft-frame-sample-surface-copy-encode-seconds
                :luvcraft/surface-copy)
      (when readback-buffer
        (encode
         encoder
         (make-gpu-copy-texture-to-buffer-command
          :source (luvcraft-session-presentation-texture session)
          :destination readback-buffer)))
      (encode
       encoder
       (make-gpu-copy-texture-command
        :source (luvcraft-session-presentation-texture session)
        :destination surface-texture)))))

(defun luvcraft-session-neighborhood-center (session)
  "Return the world X and Z the session's neighbourhood is centred on."
  (let ((player (luvcraft-session-player session))
        (camera (luvcraft-session-camera session)))
    (if player
        (values (player-x player) (player-z player))
        (values (camera-x camera) (camera-z camera)))))

(defun advance-luvcraft-critters (session seconds)
  "Advance SESSION's animals and keep its neighbourhood populated with them."
  (let ((critters (luvcraft-session-critters session))
        (world (luvcraft-session-world session)))
    (advance-critters critters world seconds)
    (multiple-value-bind (x z) (luvcraft-session-neighborhood-center session)
      (maintain-critter-population critters world x z))))

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
            (advance-block-particles
             (luvcraft-session-particle-system session) seconds)
            (advance-luvcraft-critters session seconds)
            ;; A moving interaction takes its turn after the world it moves in
            ;; and before the player controller, which stands down entirely
            ;; while something else is carrying the player.
            (let ((focus (luvcraft-session-modal-focus session)))
              (advance-luvcraft-focus focus session seconds)
              (when (and focus (luvcraft-focus-carries-player-p focus))
                (setf (luvcraft-session-physics-accumulator session) 0d0
                      (luvcraft-session-jump-requested-p session) nil)))
            (let ((player (luvcraft-session-player session))
                  (focus (luvcraft-session-modal-focus session)))
              (when (and player
                         (not (and focus
                                   (luvcraft-focus-carries-player-p focus))))
                (incf (luvcraft-session-physics-accumulator session) seconds)
                (loop while (>= (luvcraft-session-physics-accumulator session)
                                +player-physics-step+)
                      do (step-block-world-player
                          player (luvcraft-session-world session)
                          (luvcraft-session-camera session)
                          (luvcraft-session-pressed-keys session)
                          +player-physics-step+
                          :jump-p (luvcraft-session-jump-requested-p session)
                          :sync-camera-p
                          (not (luvcraft-session-focus-camera-active-p session)))
                         (setf (luvcraft-session-jump-requested-p session) nil)
                         (decf (luvcraft-session-physics-accumulator session)
                               +player-physics-step+))))
            (advance-luvcraft-focus-camera session seconds)
            (advance-player-body (luvcraft-session-body session)
                                 session seconds)))
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
  (when (and (eq :tab (canvas-key-event-key-name event))
             (or (null (luvcraft-session-modal-focus session))
                 (member :shift (canvas-key-event-modifiers event))))
    (unless (canvas-key-event-repeat-p event)
      (setf (luvcraft-session-focus-toggle-tab-down-p session) t)
      (toggle-luvcraft-session-focus session))
    (return-from handle-canvas-event nil))
  (when (and (eq :i (canvas-key-event-key-name event))
             (not (canvas-key-event-repeat-p event))
             (toggle-luvcraft-inventory session))
    (return-from handle-canvas-event nil))
  ;; Fullscreen belongs to the window rather than to anything focused inside
  ;; it, so F11 is taken before a terminal or an overlay can read it as text.
  (when (and (eq :f11 (canvas-key-event-key-name event))
             (not (canvas-key-event-repeat-p event)))
    (set-canvas-fullscreen canvas (not (canvas-fullscreen-p canvas)))
    (return-from handle-canvas-event nil))
  (when (dispatch-luvcraft-focus-event session canvas event)
    (return-from handle-canvas-event nil))
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
          ;; F takes the phone out or puts it away.
          (when (and (eq key :f) (not (canvas-key-event-repeat-p event)))
            (toggle-luvcraft-phone session))
          (unless (canvas-key-event-repeat-p event)
            (let* ((character (canvas-key-event-character event))
                   (number (and character (digit-char-p character))))
              (when (and number
                         (<= 1 number
                             (length
                              (block-inventory-quickbar-blocks
                               (luvcraft-session-inventory session)))))
                (select-luvcraft-block session number)))))))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-key-release-event))
  (when (and (eq :tab (canvas-key-event-key-name event))
             (luvcraft-session-focus-toggle-tab-down-p session))
    (setf (luvcraft-session-focus-toggle-tab-down-p session) nil)
    (return-from handle-canvas-event nil))
  (when (dispatch-luvcraft-focus-event session canvas event)
    (return-from handle-canvas-event nil))
  (remhash (canvas-key-event-key-name event)
           (luvcraft-session-pressed-keys session))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-pointer-wheel-event))
  "Offer a scroll to whatever owns focus, then to the overlays.

The player's own view does not scroll -- the wheel is not a camera control
here -- so an unconsumed wheel event is simply the end of the matter."
  (unless (dispatch-luvcraft-focus-event session canvas event)
    (dispatch-luvcraft-overlay-event session canvas event))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-pointer-button-press-event))
  (when (dispatch-luvcraft-focus-event session canvas event)
    (return-from handle-canvas-event nil))
  (when (and (not (luvcraft-session-pointer-captured-p session))
             (dispatch-luvcraft-overlay-event session canvas event))
    (return-from handle-canvas-event nil))
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
    ((session luvcraft-session) canvas
     (event canvas-pointer-button-release-event))
  (unless (dispatch-luvcraft-focus-event session canvas event)
    (unless (luvcraft-session-pointer-captured-p session)
      (dispatch-luvcraft-overlay-event session canvas event)))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-pointer-motion-event))
  (cond
    ((dispatch-luvcraft-focus-event session canvas event))
    ((luvcraft-session-pointer-captured-p session)
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
    (t
     (dispatch-luvcraft-overlay-event session canvas event)))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-window-focus-lost-event))
  (clear-luvcraft-player-input session)
  (dispatch-luvcraft-focus-event session canvas event)
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (setf (luvcraft-session-running-p session) nil)
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-event))
  (dispatch-luvcraft-focus-event session canvas event)
  nil)

(defun start-luvcraft (&key
                                (title "luv little block world — click, look, walk")
                                ;; NIL means "as much of this display as
                                ;; comfortably fits"; a capture asks for the
                                ;; exact frame it intends to write out.
                                (width nil) (height nil)
                                (frames-per-second 60)
                                (visible-p t)
                                (fullscreen-p nil)
                                (world (make-empty-little-block-world))
                                (mesher (make-instance
                                         'exposed-face-mesher))
                                (camera (make-instance 'fly-camera))
                                player
                                (selected-block *stone-block*)
                                (inventory (make-block-inventory))
                                ;; A hidden capture wanting animals in frame
                                ;; hands in a population it has already
                                ;; placed; the ordinary game grows its own
                                ;; around the player as it plays.
                                (critters
                                 (make-instance 'critter-population))
                                checkpoint-writer
                                (provider *gpu-provider*)
                                (sky-clock (make-instance 'sky-clock))
                                (sky-profile (make-default-sky-profile))
                                (shadow-diagnostic-p nil)
                                ;; The world-text banner is a proof of the
                                ;; Slug path, not scenery: a caller that
                                ;; wants one asks for it, and the ordinary
                                ;; game sky stays empty.
                                (world-text-string nil)
                                (world-text-font-pathname
                                  (cl-dejavu:font-pathname "DejaVuSans.ttf"))
                                (world-text-distance 8.0)
                                (world-text-lift 3.0)
                                (world-text-units-per-em 0.55)
                                (video-pathname nil)
                                (video-distance 13.0)
                                (video-lift 4.5)
                                (video-height 5.0)
                                (residency-radius 4)
                                (publication-limit 2)
                                (load-schedule-limit 4)
                                (mesh-capture-limit 1))
  "Open a little CPU-meshed block world.

Click to capture the pointer, look with the mouse, walk with WASD, and jump
with Space.  Once captured, left click removes the block at the centre of view
and right click places the selected block.  Number keys select materials,
middle click picks the targeted material, Shift sprints, and Escape releases
the pointer.  When a presentation extension supplies it, I opens the player
inventory.

Pass :PROVIDER to select the Vulkan or Metal relationship without changing
world, simulation, streaming, or frame orchestration.  Pass :VISIBLE-P NIL to
keep the SDL window hidden while still exercising the real presentation path.
Pass :FRAMES-PER-SECOND NIL for a capture-only demand clock.  Pass
:FULLSCREEN-P T to open on the whole display, and leave :WIDTH and :HEIGHT
NIL to let the display choose a comfortable window."
  (let* ((canvas (make-sdl-canvas
                  :title title :width width :height height
                  :fullscreen-p fullscreen-p
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
         (video-screen nil)
         (session nil) (production-system nil) (completed-p nil))
    ;; FFmpeg has to be dlopened before the canvas exists.  Film is now a live
    ;; terminal-wall mode, so waiting until the user opens its browser would
    ;; attempt the first dlopen under Cocoa's running canvas and hang.  Preload
    ;; the libraries here; no decoder or file is opened until Film is chosen.
    (libav:load-libav)
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
                  ;; Every frame-sized image comes from one constructor, which
                  ;; a live window resize calls again with the new extent.
                  (attachments
                    (make-luvcraft-frame-attachments device context extent))
                  (color-texture (keep (getf attachments :color-texture)))
                  (color-view (keep (getf attachments :color-view)))
                  (depth-texture (keep (getf attachments :depth-texture)))
                  (depth-view (keep (getf attachments :depth-view)))
                  (bloom-primary-texture
                    (keep (getf attachments :bloom-primary-texture)))
                  (bloom-primary-view
                    (keep (getf attachments :bloom-primary-view)))
                  (bloom-secondary-texture
                    (keep (getf attachments :bloom-secondary-texture)))
                  (bloom-secondary-view
                    (keep (getf attachments :bloom-secondary-view)))
                  (presentation-texture
                    (keep (getf attachments :presentation-texture)))
                  (presentation-view
                    (keep (getf attachments :presentation-view)))
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
                  (normal-atlas-data (make-block-normal-atlas))
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
                  (normal-atlas-texture
                    (keep
                     (create
                      device
                      (make-texture-descriptor
                       :label "block world normal atlas"
                       :size (list atlas-width atlas-height)
                       :dimensions :2d :format +block-normal-atlas-texture-format+
                       :usage '(:copy-dst :texture-binding)))))
                  (normal-atlas-view
                    (keep
                     (create device (make-texture-view-descriptor
                                     :texture normal-atlas-texture))))
                  (atlas-sampler
                    (keep
                     (create device (make-sampler-descriptor
                                     :label "block world nearest sampler"
                                     :mag-filter :nearest
                                     :min-filter :nearest
                                     :mipmap-filter :nearest))))
                  ;; Presentation and the lens chain read continuous images
                  ;; rather than a texel grid; the five-sample gaussian below
                  ;; is only a nine-tap kernel because it filters linearly.
                  (linear-sampler
                    (keep
                     (create device (make-sampler-descriptor
                                     :label "block world linear sampler"
                                     :mag-filter :linear
                                     :min-filter :linear
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
                  (particle-vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "block smash particle vertices"
                       :size +block-particle-buffer-size+
                       :usage '(:vertex)))))
                  (critter-vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "critter model vertices"
                       :size +critter-buffer-size+
                       :usage '(:vertex)))))
                  (body-vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "player body vertices"
                       :size +player-body-buffer-size+
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
                                  (:binding 5 :type :sampler)
                                  (:binding 6 :type :texture))))))
                  (shadow-layout
                    (keep
                     (create
                      device
                      (make-bind-group-layout-descriptor
                       :label "block world shadow-pass layout"
                       :entries '((:binding 2 :type :uniform-buffer))))))
                  (post-layout
                    (keep
                     (create
                      device
                      (make-bind-group-layout-descriptor
                       :label "block world focus post layout"
                       :entries '((:binding 0 :type :texture)
                                  (:binding 1 :type :sampler)
                                  (:binding 2 :type :texture)
                                  (:binding 3 :type :uniform-buffer)
                                  (:binding 4 :type :texture)
                                  (:binding 5 :type :texture)
                                  (:binding 6 :type :sampler))))))
                  (bloom-layout
                    (keep
                     (create
                      device
                      (make-bind-group-layout-descriptor
                       :label "block world lens chain layout"
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
                             '((:array-stride 64
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3)
                                 (:shader-location 1 :offset 12
                                  :format :float32x3)
                                 (:shader-location 2 :offset 24
                                  :format :float32x3)
                                 (:shader-location 3 :offset 36
                                  :format :float32x3)
                                 (:shader-location 4 :offset 48
                                  :format :float32x4))))
                             :target-format +luvcraft-scene-color-format+
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
                             '((:array-stride 64
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3)
                                 (:shader-location 1 :offset 12
                                  :format :float32x3)
                                 (:shader-location 2 :offset 24
                                  :format :float32x3)
                                 (:shader-location 3 :offset 36
                                  :format :float32x3)
                                 (:shader-location 4 :offset 48
                                  :format :float32x4))))
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
                             :target-format +luvcraft-scene-color-format+
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
                             :target-format +luvcraft-scene-color-format+
                             :primitive '(:topology :triangle-list)
                             :depth-stencil
                             '(:format :depth32-float
                               :depth-write-enabled nil
                               :depth-compare :always))))
                      (push artifact pipelines)
                      artifact))
                  (post-pipeline
                    (let ((artifact
                            (make-live-shader-pipeline
                             :role :focus-post
                             :vertex-role :focus-post
                             :label "block world focus post pipeline"
                             :device device :layout post-layout
                             :vertex-buffers
                             '((:array-stride 12
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3))))
                             :target-format (canvas-format context)
                             :primitive '(:topology :triangle-list)
                             :depth-stencil nil)))
                      (push artifact pipelines)
                      artifact))
                  ;; The four lens stages differ only in which mathematical
                  ;; fragment method they name; everything else about them is
                  ;; the same fullscreen triangle over the same layout.
                  (bloom-bright-pipeline
                    (let ((artifact
                            (make-luvcraft-lens-pipeline
                             device bloom-layout :bloom-bright
                             "block world bloom bright pipeline")))
                      (push artifact pipelines)
                      artifact))
                  (bloom-horizontal-pipeline
                    (let ((artifact
                            (make-luvcraft-lens-pipeline
                             device bloom-layout :bloom-horizontal
                             "block world bloom horizontal pipeline")))
                      (push artifact pipelines)
                      artifact))
                  (bloom-vertical-pipeline
                    (let ((artifact
                            (make-luvcraft-lens-pipeline
                             device bloom-layout :bloom-vertical
                             "block world bloom vertical pipeline")))
                      (push artifact pipelines)
                      artifact))
                  (sun-shaft-pipeline
                    (let ((artifact
                            (make-luvcraft-lens-pipeline
                             device bloom-layout :sun-shafts
                             "block world sun shaft pipeline")))
                      (push artifact pipelines)
                      artifact))
                  (text-glyph-cache
                    (when world-text-string
                      (setf world-text-glyph-cache
                            (luv.slug:make-slug-glyph-cache device))))
                  (text-run
                    (when text-glyph-cache
                      (setf world-text-run
                            (make-world-text-run
                             device text-glyph-cache camera
                             +luvcraft-scene-color-format+
                             world-text-string world-text-font-pathname
                             :distance world-text-distance
                             :lift world-text-lift
                             :world-units-per-em
                             world-text-units-per-em))))
                  (screen
                    (when video-pathname
                      (setf video-screen
                            (make-video-screen
                             device camera video-pathname
                             +luvcraft-scene-color-format+
                             :distance video-distance
                             :lift video-lift
                             :height video-height))))
                  (new-session
                    (make-instance
                     'luvcraft-session
                     :video-screen screen
                     :canvas canvas :device device :context context
                     :world world :mesher mesher
                     :checkpoint-writer checkpoint-writer
                     :production-system production-system
                     :camera camera
                     :player player
                     :selected-block selected-block
                     :inventory inventory
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
                     :normal-atlas-texture normal-atlas-texture
                     :normal-atlas-view normal-atlas-view
                     :render-extent extent
                     :color-texture color-texture :color-view color-view
                     :depth-texture depth-texture :depth-view depth-view
                     :presentation-texture presentation-texture
                     :presentation-view presentation-view
                     :shadow-depth-texture shadow-depth-texture
                     :shadow-depth-view shadow-depth-view
                     :shadow-depth-sampler shadow-depth-sampler
                     :shadow-comparison-sampler shadow-comparison-sampler
                     :layout layout :shadow-layout shadow-layout
                     :post-layout post-layout
                     :bloom-layout bloom-layout
                     :linear-sampler linear-sampler
                     :bloom-primary-texture bloom-primary-texture
                     :bloom-primary-view bloom-primary-view
                     :bloom-secondary-texture bloom-secondary-texture
                     :bloom-secondary-view bloom-secondary-view
                     :bloom-bright-pipeline bloom-bright-pipeline
                     :bloom-horizontal-pipeline bloom-horizontal-pipeline
                     :bloom-vertical-pipeline bloom-vertical-pipeline
                     :sun-shaft-pipeline sun-shaft-pipeline
                     :block-pipeline pipeline
                     :shadow-pipeline shadow-pipeline
                     :sky-vertex-buffer sky-vertex-buffer
                     :sky-pipeline sky-pipeline
                     :crosshair-vertex-buffer crosshair-vertex-buffer
                     :crosshair-pipeline crosshair-pipeline
                     :post-pipeline post-pipeline
                     :particle-vertex-buffer particle-vertex-buffer
                     :critter-vertex-buffer critter-vertex-buffer
                     :critters critters
                     :body-vertex-buffer body-vertex-buffer
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
               (write-texture
                (device-queue device)
                (make-texture-copy :texture normal-atlas-texture)
                normal-atlas-data
                (make-texture-data-layout
                 :bytes-per-row (* atlas-width 4)
                 :rows-per-image atlas-height)
                (list atlas-width atlas-height))
               (setf session new-session)
               (update-luvcraft-session-title session)
               (attach-luvcraft-hud session)
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
        ;; Startup failed and its condition is already on its way out, so
        ;; trouble releasing the half-built session is warned about rather
        ;; than signalled -- signalling here would replace the error that
        ;; actually explains why the game did not open.
        (with-release-warnings
          (when session
            (releasing :chunk-products
              (destroy-luvcraft-chunk-products session)))
          (dolist (pipeline pipelines)
            (releasing :pipeline (release-live-shader-pipeline pipeline)))
          (dolist (resource resources)
            (releasing :resource (destroy resource)))
          (when video-screen
            (releasing :video-screen (release-video-screen video-screen)))
          (when world-text-run
            (releasing :world-text (release-world-text-run world-text-run)))
          (when world-text-glyph-cache
            (releasing :glyph-cache
              (luv.slug:release-slug-glyph-cache world-text-glyph-cache)))
          (releasing :canvas (close-canvas canvas))
          (when device (releasing :device (destroy device))))))))

(defun stop-luvcraft (session)
  "Stop SESSION and explicitly release all of its GPU and canvas resources.

Every step runs whatever the ones before it did, so the window closes even
when something fails; the failures are then signalled together as
LUVCRAFT-RELEASE-ERROR.  See WITH-RELEASE-REPORT."
  ;; A native close request may already have set this, but the resources still
  ;; belong to the session until this explicit teardown.
  (setf (luvcraft-session-running-p session) nil)
  (with-release-report
    (releasing :focus (unfocus-luvcraft-session session))
    (let ((canvas (luvcraft-session-canvas session)))
      (releasing :canvas-quiescence
        (when (eq :open (canvas-state canvas))
          (setf (canvas-clock canvas) (make-demand-clock))
          (when (luvcraft-session-pointer-captured-p session)
            (releasing :pointer-capture
              (set-canvas-relative-pointer-mode canvas nil))
            (setf (luvcraft-session-pointer-captured-p session) nil))
          ;; A synchronous no-op after changing the clock is a native-thread
          ;; barrier: an already-running frame has finished before teardown
          ;; starts.
          (request-canvas-frame canvas (lambda (timestamp)
                                         (declare (ignore timestamp))))))
      (setf (canvas-event-handler canvas) nil)
      ;; Stop CPU publication before releasing any render-owned destination.
      (releasing :production-system
        (stop-production-system (luvcraft-session-production-system session)))
      (releasing :chunk-products (destroy-luvcraft-chunk-products session))
      (dolist (overlay (luvcraft-session-overlays session))
        (releasing :overlay (release-luvcraft-overlay overlay)))
      (setf (luvcraft-session-overlays session) nil)
      (dolist (pipeline
                (list (luvcraft-session-block-pipeline session)
                      (luvcraft-session-shadow-pipeline session)
                      (luvcraft-session-sky-pipeline session)
                      (luvcraft-session-crosshair-pipeline session)
                      (luvcraft-session-post-pipeline session)
                      (luvcraft-session-bloom-bright-pipeline session)
                      (luvcraft-session-bloom-horizontal-pipeline session)
                      (luvcraft-session-bloom-vertical-pipeline session)
                      (luvcraft-session-sun-shaft-pipeline session)))
        (when pipeline
          (releasing :pipeline (release-live-shader-pipeline pipeline))))
      (dolist (resource (luvcraft-session-resources session))
        (releasing :resource (destroy resource)))
      (setf (luvcraft-session-resources session) nil)
      (when (luvcraft-session-video-screen session)
        (releasing :video-screen
          (release-video-screen (luvcraft-session-video-screen session)))
        (setf (luvcraft-session-video-screen session) nil))
      (when (luvcraft-session-world-text session)
        (releasing :world-text
          (release-world-text-run (luvcraft-session-world-text session))))
      (when (luvcraft-session-world-text-glyph-cache session)
        (releasing :glyph-cache
          (luv.slug:release-slug-glyph-cache
           (luvcraft-session-world-text-glyph-cache session))))
      ;; The window and the device are last and are never skipped: they are
      ;; the two handles whose loss would leave something on the desktop that
      ;; nothing can close.
      (releasing :canvas (close-canvas canvas)))
    (releasing :device (destroy (luvcraft-session-device session))))
  (values))
