;;; Frame encoding, input handling, and the luvcraft session lifecycle.
;;;
;;; The canvas frame callback is the ownership boundary for all GPU
;;; replacement: shader refresh, mesh publication, and uniform updates all
;;; happen here, on the thread that owns the swapchain.  This file also owns
;;; canvas event handling and the START/STOP pair that creates and releases
;;; the renderer and the other explicitly owned application components.

(in-package #:luvcraft)

(defclass luvcraft-frame-state ()
  ((uniform-buffer :initarg :uniform-buffer
                   :reader luvcraft-frame-uniform-buffer)
   ;; WRITE-BUFFER writes mapped host-visible memory directly.  Animated
   ;; streams therefore belong to the presentation slot: reacquiring this
   ;; slot is the proof that the GPU has finished reading its previous data.
   (particle-vertex-buffer :initarg :particle-vertex-buffer
                           :reader luvcraft-frame-particle-vertex-buffer)
   (critter-vertex-buffer :initarg :critter-vertex-buffer
                          :reader luvcraft-frame-critter-vertex-buffer)
   (physics-vertex-buffer :initarg :physics-vertex-buffer
                          :reader luvcraft-frame-physics-vertex-buffer)
   (physics-instance-buffer :initarg :physics-instance-buffer
                            :reader luvcraft-frame-physics-instance-buffer)
   (body-vertex-buffer :initarg :body-vertex-buffer
                       :reader luvcraft-frame-body-vertex-buffer)
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

(defun luvcraft-frame-state-resources (state)
  "Return the unique GPU resources retained by one drawable frame STATE."
  (remove-duplicates
   (list* (luvcraft-frame-scene-bind-group state)
          (luvcraft-frame-shadow-bind-group state)
          (luvcraft-frame-post-bind-group state)
          (luvcraft-frame-bloom-scene-bind-group state)
          (luvcraft-frame-bloom-primary-bind-group state)
          (luvcraft-frame-bloom-secondary-bind-group state)
          (luvcraft-frame-post-uniform-buffer state)
          (luvcraft-frame-particle-vertex-buffer state)
          (luvcraft-frame-critter-vertex-buffer state)
          (luvcraft-frame-physics-vertex-buffer state)
          (luvcraft-frame-physics-instance-buffer state)
          (luvcraft-frame-body-vertex-buffer state)
          (luvcraft-frame-uniform-buffer state)
          (coerce (luvcraft-frame-world-text-bind-groups state) 'list))
   :test #'eq))

(defconstant +block-world-crosshair-vertex-count+ 24)
(defconstant +luvcraft-cursor-vertex-count+ 6)
(defconstant +luvcraft-cursor-scale+ 1.8
  "Framebuffer pixels per unit of the cursor shader's design grid.")
(defconstant +luvcraft-cursor-margin+ 5.0
  "Design-grid slack around the arrow for its shadow and antialiased edge.")
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
;;; The shadow's biases and filter radii are knobs: read at frame-pack time
;;; into the shadow control lanes, so a turn shows on the next frame.
(defparameter *luvcraft-shadow-base-bias* 0.00405
  "Constant depth bias applied to every shadow comparison.")
(defparameter *luvcraft-shadow-slope-bias* 0.002
  "Depth bias scaled by the receiver's slope to the light.")
(defparameter *luvcraft-shadow-minimum-filter-radius* 8.0
  "PCF radius in shadow texels when the sun is a point.")
(defparameter *luvcraft-shadow-maximum-filter-radius* 24.0
  "PCF radius in shadow texels at the sun's widest.")

(define-knob shadow-base-bias
    (:group :shadows :label "base bias"
     :quantity (:quantity :shadow-depth :unit :one :character :difference)
     :minimum 0.0 :maximum 0.005 :step 0.00005)
    *luvcraft-shadow-base-bias*)
(define-knob shadow-slope-bias
    (:group :shadows :label "slope bias"
     :quantity (:quantity :shadow-depth :unit :one :character :difference)
     :minimum 0.0 :maximum 0.01 :step 0.0001)
    *luvcraft-shadow-slope-bias*)
(define-knob shadow-minimum-filter-radius
    (:group :shadows :label "softness, sun small"
     :quantity (:quantity :shadow-filter-radius :unit :one)
     :unit-label " px" :minimum 0.0 :maximum 8.0 :step 0.5)
    *luvcraft-shadow-minimum-filter-radius*)
(define-knob shadow-maximum-filter-radius
    (:group :shadows :label "softness, sun wide"
     :quantity (:quantity :shadow-filter-radius :unit :one)
     :unit-label " px" :minimum 1.0 :maximum 24.0 :step 0.5)
    *luvcraft-shadow-maximum-filter-radius*)

(defun ensure-block-atlas-sample-transfer (format)
  "Check the host texture format against the block shader's decoded result."
  (let* ((resource
           (find 'luvcraft.shaders::block-atlas
                 (luv.shader:shader-specification-resources
                  (luvcraft.shaders:block-world-fragment-specification))
                 :key #'luv.shader:shader-object-name))
         (expected (texture-format-sample-transfer format))
         (declared
           (and resource
                (luv.shader:shader-resource-sample-transfer resource))))
    (unless (eq expected declared)
      (error "Block atlas format ~S implies ~S sampling, but the shader declares ~S."
             format expected declared))
    format))

(defun make-block-world-crosshair-vertices (width height &optional x y)
  "Make an outlined pixel-sized crosshair at screen X,Y, or at the centre."
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (origin-x (if x (- x (/ width 2.0)) 0.0))
        ;; The Vulkan viewport already turns clip Y into downward-growing
        ;; screen coordinates, so pointer Y keeps its sign here.
        (origin-y (if y (- y (/ height 2.0)) 0.0)))
    (labels ((clip-x (pixels) (/ (* 2.0 pixels) width))
             (clip-y (pixels) (/ (* 2.0 pixels) height))
             (vertex (x y color)
               (dolist (component
                        (list (clip-x (+ origin-x x))
                              (clip-y (+ origin-y y)) 0.0
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

(defun make-luvcraft-cursor-vertices (width height x y)
  "Make the quad the cursor shader draws its arrow inside, tip at screen X,Y.

Only the hotspot and the scale live here: each vertex carries its offset from
the tip in the shader's design grid, and the fragment stage turns that into
the outline, the fill, and the shadow."
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (origin-x (- x (/ width 2.0)))
        (origin-y (- y (/ height 2.0))))
    (labels ((vertex (local-x local-y)
               (dolist (component
                        (list (/ (* 2.0
                                    (+ origin-x
                                       (* +luvcraft-cursor-scale+ local-x)))
                                 width)
                              (/ (* 2.0
                                    (+ origin-y
                                       (* +luvcraft-cursor-scale+ local-y)))
                                 height)
                              0.0
                              local-x local-y))
                 (vector-push-extend (coerce component 'single-float)
                                     vertices))))
      ;; The arrow starts at the design grid's origin and runs to the extent
      ;; its own outline reaches; the margin leaves room for the antialiased
      ;; edge and the shadow cast down and to the right.
      (destructuring-bind (arrow-width arrow-height)
          (luvcraft.shaders:luvcraft-cursor-extent)
        (let* ((left (- +luvcraft-cursor-margin+))
               (top (- +luvcraft-cursor-margin+))
               (right (+ arrow-width +luvcraft-cursor-margin+))
               (bottom (+ arrow-height +luvcraft-cursor-margin+)))
          (vertex left top)
          (vertex right top)
          (vertex right bottom)
          (vertex left top)
          (vertex right bottom)
          (vertex left bottom))))
    (ensure-vertex-product-contract
     vertices :cursor-vertices +luvcraft-cursor-vertex-count+
     (luvcraft.shaders::shader-specification-for :cursor :vertex))))

(defun make-block-world-sky-vertices ()
  "Make the one fullscreen triangle in normalized clip coordinates."
  (ensure-vertex-product-contract
   (make-array
    9 :element-type 'single-float
    :initial-contents '(-1.0 -1.0 0.5
                        3.0 -1.0 0.5
                        -1.0 3.0 0.5))
   :sky-vertices 3 (luvcraft.shaders:block-world-sky-vertex-specification)))

(defun shadow-frame-rows (camera sky &optional anchor)
  "Pack a texel-stable orthographic light-space transform as four vec4 rows.

Returns the rows and the ANCHOR to hand back next frame.  The anchor is the
world point the light-space texel lattice is built around: it follows the
camera in whole texels of the current light basis, so camera translation
never moves the lattice by a fraction of a texel, and the sun's rotation
turns the lattice about the camera rather than about the world origin.
Without an anchor the camera position itself starts one."
  (let* ((center
           (make-vec3 (camera-x camera) (camera-y camera) (camera-z camera)))
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
         ;; Rotation still slides the lattice under the world, by the angle
         ;; times the distance from the pivot.  Snapping the camera to a
         ;; lattice through the world origin put that pivot at the origin, so
         ;; a player standing a few hundred units out watched every shadow
         ;; edge vibrate a texel many times a second however slow the day.
         ;; The pivot is instead a persistent anchor that walks toward the
         ;; camera in whole texels of this frame's basis: the lattice never
         ;; shifts under translation, and it rotates about a point within a
         ;; texel of the eye.  The anchor is kept in double precision so that
         ;; the walk does not itself wobble the lattice.  #QWTQ6R
         (anchor
           (let* ((start (or anchor center))
                  (delta (make-vec3 (- (vec3-x center) (vec3-x start))
                                    (- (vec3-y center) (vec3-y start))
                                    (- (vec3-z center) (vec3-z start))))
                  (along-right
                    (* (round (/ (vec3-dot delta right) world-units-per-texel))
                       world-units-per-texel))
                  (along-up
                    (* (round (/ (vec3-dot delta up) world-units-per-texel))
                       world-units-per-texel))
                  (along-forward (vec3-dot delta forward)))
             (flet ((walk (axis)
                      (coerce (+ (vec3-component start axis)
                                 (* along-right (vec3-component right axis))
                                 (* along-up (vec3-component up axis))
                                 (* along-forward
                                    (vec3-component forward axis)))
                              'double-float)))
               (make-vec3 (walk :x) (walk :y) (walk :z)))))
         (center-right (vec3-dot anchor right))
         (center-up (vec3-dot anchor up)))
    (flet ((lane (axis scale offset)
             (list (* (vec3-x axis) scale)
                   (* (vec3-y axis) scale)
                   (* (vec3-z axis) scale)
                   (coerce offset 'single-float))))
      (values
       (append
        (lane right (/ extent) (- (/ center-right extent)))
        (lane up (/ extent) (- (/ center-up extent)))
        (lane forward (/ (* 2.0 depth-radius))
              (- 0.5 (/ (vec3-dot anchor forward) (* 2.0 depth-radius))))
        '(0.0 0.0 0.0 1.0))
       anchor))))

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
         (data (make-array (+ (length camera-lanes) 56)
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
        ;; The zenith and horizon lanes' spare w carry the target's height
        ;; and width in pixels: what a vertex stage needs to size a pixel.
        (apply #'emit (append (color (sky-frame-parameters-zenith-color sky))
                              (list height)))
        (apply #'emit (append (color (sky-frame-parameters-horizon-color sky))
                              (list width)))
        (apply #'emit (append (color (sky-frame-parameters-ambient-color sky))
                              (list (sky-frame-parameters-exposure sky))))
        (apply #'emit
               (append (color (sky-frame-parameters-fog-color sky))
                       (list (if (luvcraft-session-shadow-diagnostic-p session)
                                 1.0
                                 0.0))))
        (emit (/ +luvcraft-shadow-map-size+)
              (/ +luvcraft-shadow-map-size+)
              *luvcraft-shadow-base-bias*
              *luvcraft-shadow-slope-bias*)
        (emit (* 2.0 +luvcraft-shadow-depth-radius+)
              (/ (* 2.0 +luvcraft-shadow-half-extent+)
                 +luvcraft-shadow-map-size+)
              *luvcraft-shadow-minimum-filter-radius*
              *luvcraft-shadow-maximum-filter-radius*)
        ;; The texture resource owns atlas extent.  Meshes retain only a tile
        ;; offset under the mapping which painted that resource, so replacing
        ;; it with a wider atlas needs no remesh.
        (let ((renderer (luvcraft-session-renderer session)))
          (emit (if (slot-boundp renderer 'atlas-texture)
                    (first (gpu-texture-size
                            (luvcraft-renderer-atlas-texture renderer)))
                    (* +block-atlas-tile-size+
                       *block-atlas-tile-capacity*))
                0.0 0.0 0.0))
        (multiple-value-bind (rows anchor)
            (shadow-frame-rows (luvcraft-session-camera session) sky
                               (luvcraft-session-shadow-anchor session))
          (setf (luvcraft-session-shadow-anchor session) anchor)
          (apply #'emit rows))))
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

(defparameter *luvcraft-exposure* 0.45
  "Overall scene exposure multiplied into the sky profile's own exposure.")

(defparameter *luvcraft-crosshair-p* t
  "Draw the crosshair.  A film shot is not aiming at anything.")

(defparameter *luvcraft-focus-blur-p* nil
  "Engage the focus-plane background blur without a modal focus.

The focus plane is the depth at the centre of the frame, so a film shot
that wants a subject sharp against a blurred background aims the centre
ray at ground or blocks standing at the subject's distance.  A global
rather than a binding because the presentation uniform is packed on the
canvas thread, not the thread directing the film.")

(defparameter *luvcraft-bloom-gain* 0.22
  "How much of the blurred bright-pass image is added back in linear light.

The chain's kernel is wide and runs twice, so this is a glow spread over a
sixth of the frame rather than a halo: past about a third it stops reading as
light around the sun and starts reading as fog over everything.")

(defparameter *luvcraft-shaft-gain* 0.30
  "How strongly sunlight scattered around the solar disc streaks the frame.")

(defparameter *luvcraft-vignette* 0.60
  "Corner falloff of the presented frame, as a fraction of full brightness.")

(defparameter *luvcraft-bloom-threshold* 1.5
  "Luminance at which a fragment starts contributing to the bloom chain.")

(defparameter *luvcraft-shaft-decay* 0.955
  "Per-tap attenuation along a light shaft; nearer one reaches further.")

(define-knob exposure
    (:group :grading :quantity (:quantity :exposure :unit :one)
     :unit-label "×" :minimum 0.2 :maximum 3.0 :step 0.05)
    *luvcraft-exposure*)
(define-knob bloom-threshold
    (:group :grading :quantity (:quantity :luminance-threshold :unit :one)
     :minimum 0.2 :maximum 6.0 :step 0.1)
    *luvcraft-bloom-threshold*)
(define-knob bloom-gain
    (:group :grading :quantity (:quantity :lens-gain :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 2.0 :step 0.05)
    *luvcraft-bloom-gain*)
(define-knob shaft-gain
    (:group :grading :label "light shafts"
     :quantity (:quantity :lens-gain :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 2.0 :step 0.05)
    *luvcraft-shaft-gain*)
(define-knob shaft-decay
    (:group :grading :label "shaft reach"
     :quantity (:quantity :shaft-decay :unit :one)
     :minimum 0.85 :maximum 0.999 :step 0.005)
    *luvcraft-shaft-decay*)
(define-knob vignette
    (:group :grading :quantity (:quantity :vignette-strength :unit :one)
     :minimum 0.0 :maximum 0.6 :step 0.02)
    *luvcraft-vignette*)

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
                (if (or (luvcraft-session-modal-focus session)
                        *luvcraft-focus-blur-p*)
                    1.0 0.0)
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
         (size (shader:shader-uniform-block-byte-size block))
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
  (let ((bytes (shader:shader-uniform-block-byte-size block))
        (projections nil))
    (unless (zerop (mod bytes 4))
      (error "Frame shader uniform size ~D is not a whole float lane count."
             bytes))
    (dolist (member (shader:shader-uniform-block-members block))
      (let* ((byte-offset (shader:shader-uniform-member-offset member))
             (width
               (shader:shader-type-component-count
                (luv.arithmetic:declaration-representation-type member)))
             (whole
               (luv.arithmetic:declaration-quantity-specification member))
             (layout
               (luv.arithmetic:declaration-quantity-layout member)))
        (unless (and width (zerop (mod byte-offset 4)))
          (error "Frame shader member ~S is not a 32-bit scalar-lane value."
                 (shader:shader-object-name member)))
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
                     (shader:shader-object-name member) width
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
         (size (shader:shader-uniform-block-byte-size block))
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
             (mapcar #'shader:shader-object-name
                     (shader:shader-uniform-block-members block))))
    size))

(defun remember-luvcraft-renderer-resource (renderer resource)
  "Adopt RESOURCE into RENDERER's single release inventory."
  (push resource (luvcraft-renderer-resources renderer))
  resource)

(defun forget-luvcraft-renderer-resource (renderer resource)
  "Drop RESOURCE from RENDERER's inventory for immediate release."
  (setf (luvcraft-renderer-resources renderer)
        (delete resource (luvcraft-renderer-resources renderer) :test #'eq))
  resource)

(defun release-luvcraft-renderer-resource (renderer resource)
  (when resource
    ;; A failed destruction must leave the only retry handle with its owner.
    (destroy resource)
    (forget-luvcraft-renderer-resource renderer resource))
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
          ;; Preserve the originating creation failure and make a best effort
          ;; to retire every partial candidate, even if one DESTROY also errs.
          (with-release-warnings
            (dolist (resource made)
              (releasing :frame-attachment-candidate
                (destroy resource)))))))))

(defun install-luvcraft-frame-attachments (renderer attachments)
  "Adopt and atomically publish ATTACHMENTS as one frame-sized cohort."
  (let* ((resources (luvcraft-frame-attachment-resources attachments))
         (inventory
           (append resources (luvcraft-renderer-resources renderer))))
    ;; Everything that may allocate is complete before either owner slot is
    ;; changed.  FRAME-ATTACHMENTS is the sole reader-visible publication.
    (setf (luvcraft-renderer-resources renderer) inventory
          (luvcraft-renderer-frame-attachments renderer) attachments))
  renderer)

(defun luvcraft-renderer-frame-attachment-resources (renderer)
  "Return RENDERER's currently published extent-sized GPU objects."
  (luvcraft-frame-attachment-resources
   (luvcraft-renderer-frame-attachments renderer)))

(defun release-luvcraft-frame-attachment-resources (renderer resources)
  "Release an unpublished or superseded attachment cohort from RENDERER."
  (with-release-report
    (dolist (resource resources)
      (releasing :frame-attachment
        (release-luvcraft-renderer-resource renderer resource))))
  (values))

(defun release-luvcraft-frame-attachments (renderer)
  "Release RENDERER's currently published frame-sized image cohort."
  (release-luvcraft-frame-attachment-resources
   renderer (luvcraft-renderer-frame-attachment-resources renderer))
  (setf (luvcraft-renderer-frame-attachments renderer) nil)
  (values))

(defun luvcraft-frame-attachment-resources (attachments)
  "Return the GPU objects in an attachment candidate, excluding its extent."
  (loop for (key value) on attachments by #'cddr
        unless (eq key :render-extent)
          collect value))

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

;;; ---------------------------------------------------------------------
;;; The physics sphere drawer: the live pipeline and shared quad behind the
;;; scene pass's instanced body draw.  It registers through the overlay
;;; protocol -- the renderer's own pipeline inventory is a closed slot set
;;; -- so REFRESH-LUVCRAFT-SHADERS rebuilds its shader methods and session
;;; teardown releases it, but its draw is not an overlay's: the bodies are
;;; encoded with the rest of the world geometry, where the cubes drew.

(defclass physics-sphere-drawer ()
  ((pipeline :initarg :pipeline :accessor physics-sphere-drawer-pipeline)
   (quad-buffer :initarg :quad-buffer
                :accessor physics-sphere-drawer-quad-buffer))
  (:documentation
   "Owns the sphere pipeline and the one six-vertex quad every body shares."))

(defmethod luvcraft-overlay-live-shader-pipelines
    ((drawer physics-sphere-drawer))
  (list (physics-sphere-drawer-pipeline drawer)))

(defmethod encode-luvcraft-overlay
    ((drawer physics-sphere-drawer) session pass surface-texture)
  ;; Deliberately nothing: the bodies are drawn from the scene pass proper,
  ;; before the player's arms and the particles, where the cubes once drew.
  (declare (ignore session pass surface-texture))
  nil)

(defmethod release-luvcraft-overlay ((drawer physics-sphere-drawer))
  (when (physics-sphere-drawer-pipeline drawer)
    (release-live-shader-pipeline (physics-sphere-drawer-pipeline drawer))
    (setf (physics-sphere-drawer-pipeline drawer) nil))
  (when (physics-sphere-drawer-quad-buffer drawer)
    (destroy (physics-sphere-drawer-quad-buffer drawer))
    (setf (physics-sphere-drawer-quad-buffer drawer) nil))
  (values))

(defun luvcraft-session-physics-sphere-drawer (session)
  (find-if (lambda (overlay) (typep overlay 'physics-sphere-drawer))
           (luvcraft-session-overlays session)))

(defun attach-physics-sphere-drawer (session)
  "Create the sphere pipeline and quad for SESSION and register the drawer."
  (let* ((device (luvcraft-session-device session))
         (vertex-data (make-world-text-quad-vertices))
         (quad-buffer nil)
         (pipeline nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf quad-buffer
                 (create
                  device
                  (make-buffer-descriptor
                   :label "physics sphere proxy quad"
                   :size (* 4 (length vertex-data))
                   :usage '(:vertex :copy-dst)))
                 pipeline
                 (make-live-shader-pipeline
                  :role :physics-sphere
                  :vertex-role :physics-sphere
                  :label "physics body sphere pipeline"
                  :device device
                  :layout (luvcraft-session-layout session)
                  :vertex-buffers
                  '((:array-stride 12
                     :attributes
                     ((:shader-location 0 :offset 0 :format :float32x3)))
                    (:array-stride 64 :step-mode :instance
                     :attributes
                     ((:shader-location 1 :offset 0 :format :float32x4)
                      (:shader-location 2 :offset 16 :format :float32x4)
                      (:shader-location 3 :offset 32 :format :float32x4)
                      (:shader-location 4 :offset 48 :format :float32x4))))
                  :target-format +luvcraft-scene-color-format+
                  :target-blend :premultiplied-alpha
                  :primitive '(:topology :triangle-list)
                  ;; The lowering has no fragment-depth output, so the quads
                  ;; test the terrain's depth without writing their own.
                  :depth-stencil
                  '(:format :depth32-float
                    :depth-write-enabled nil
                    :depth-compare :less)))
           (write-buffer quad-buffer vertex-data)
           (let ((drawer (make-instance 'physics-sphere-drawer
                                        :pipeline pipeline
                                        :quad-buffer quad-buffer)))
             (setf completed-p t)
             (add-luvcraft-overlay session drawer)
             drawer))
      (unless completed-p
        (when pipeline
          (ignore-errors (release-live-shader-pipeline pipeline)))
        (when quad-buffer (ignore-errors (destroy quad-buffer)))))))

(defun luvcraft-frame-state (session surface-texture)
  (let ((key
          (canvas-frame-resource-key
           (luvcraft-session-context session) surface-texture)))
    (or (gethash key (luvcraft-session-frame-states session))
      (let ((buffer nil)
            (particle-vertex-buffer nil)
            (critter-vertex-buffer nil)
            (physics-vertex-buffer nil)
            (physics-instance-buffer nil)
            (body-vertex-buffer nil)
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
                     particle-vertex-buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label "block smash particle vertices"
                       :size +block-particle-buffer-size+
                       :usage '(:vertex)))
                     critter-vertex-buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label "critter model vertices"
                       :size +critter-buffer-size+
                       :usage '(:vertex)))
                     physics-vertex-buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label "physics body shadow cube vertices"
                       :size +physics-shadow-vertex-buffer-size+
                       :usage '(:vertex)))
                     physics-instance-buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label "physics body sphere instances"
                       :size +physics-instance-buffer-size+
                       :usage '(:vertex)))
                     body-vertex-buffer
                     (create
                      (luvcraft-session-device session)
                      (make-buffer-descriptor
                       :label "player body vertices"
                       :size +player-body-buffer-size+
                       :usage '(:vertex)))
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
               (let ((state
                       (make-instance
                        'luvcraft-frame-state
                        :uniform-buffer buffer
                        :particle-vertex-buffer particle-vertex-buffer
                        :critter-vertex-buffer critter-vertex-buffer
                        :physics-vertex-buffer physics-vertex-buffer
                        :physics-instance-buffer physics-instance-buffer
                        :body-vertex-buffer body-vertex-buffer
                        :scene-bind-group scene-bind-group
                        :shadow-bind-group shadow-bind-group
                        :post-uniform-buffer post-uniform-buffer
                        :post-bind-group post-bind-group
                        :bloom-scene-bind-group bloom-scene-bind-group
                        :bloom-primary-bind-group bloom-primary-bind-group
                        :bloom-secondary-bind-group bloom-secondary-bind-group
                        :world-text-bind-groups world-text-bind-groups)))
                 (dolist (resource (luvcraft-frame-state-resources state))
                   (remember-luvcraft-renderer-resource
                    (luvcraft-session-renderer session) resource))
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
            (when body-vertex-buffer (destroy body-vertex-buffer))
            (when physics-instance-buffer (destroy physics-instance-buffer))
            (when physics-vertex-buffer (destroy physics-vertex-buffer))
            (when critter-vertex-buffer (destroy critter-vertex-buffer))
            (when particle-vertex-buffer (destroy particle-vertex-buffer))
            (when buffer (destroy buffer))))))))

(defun discard-luvcraft-frame-states (renderer)
  "Forget every cached per-drawable binding, which names images that are gone.

  The bind groups are keyed by drawable, not by size, so nothing else would
notice that their scene, depth, and lens-chain views belong to the previous
window.  Dropping them here makes the next frame rebuild them against the
attachments the renderer actually holds."
  (let ((states (luvcraft-renderer-frame-states renderer)))
    (with-release-report
      (maphash
       (lambda (key state)
         (declare (ignore key))
         (dolist (resource (luvcraft-frame-state-resources state))
           (releasing :frame-state-resource
             (release-luvcraft-renderer-resource renderer resource))))
       states)
      (clrhash states)))
  (values))

(defmethod release-luvcraft-component ((renderer luvcraft-renderer))
  "Release RENDERER's pipelines and GPU resources exactly once."
  (with-release-report
    (dolist (slot +luvcraft-renderer-pipeline-slots+)
      (when (and (slot-boundp renderer slot)
                 (slot-value renderer slot))
        (releasing :renderer-pipeline
          (release-live-shader-pipeline (slot-value renderer slot))
          ;; A failed pipeline stays named so a second release retries it.
          (slot-makunbound renderer slot))))
    (let ((retained nil))
      (dolist (resource
                (remove-duplicates (luvcraft-renderer-resources renderer)
                                   :test #'eq))
        (let ((released-p nil))
          (releasing :renderer-resource
            (destroy resource)
            (setf released-p t))
          (unless released-p
            (push resource retained))))
      ;; Likewise, failed resources remain in the one owner inventory.
      (setf (luvcraft-renderer-resources renderer) (nreverse retained)))
    (when (and (null (luvcraft-renderer-pipelines renderer))
               (null (luvcraft-renderer-resources renderer)))
      (setf (luvcraft-renderer-frame-attachments renderer) nil)
      (clrhash (luvcraft-renderer-frame-states renderer))))
  (values))

(defmethod resize-luvcraft-component
    ((renderer luvcraft-renderer) extent)
  "Transactionally replace every extent-sized image owned by RENDERER."
  (let ((attachments
          (make-luvcraft-frame-attachments
           (luvcraft-renderer-device renderer)
           (luvcraft-renderer-context renderer)
           extent))
        (old-attachments
          (luvcraft-renderer-frame-attachment-resources renderer))
        (installed-p nil))
    (unwind-protect
         (progn
           ;; The new cohort exists before any old object is released.  If
           ;; creation fails, MAKE-LUVCRAFT-FRAME-ATTACHMENTS destroys its
           ;; partial candidate and the installed renderer remains coherent.
           ;; Vertex writes also precede publication so a failed write leaves
           ;; the old extent installed and makes the next frame retry.
           (write-buffer
            (luvcraft-renderer-crosshair-vertex-buffer renderer)
            (make-block-world-crosshair-vertices
             (first extent) (second extent)))
           (write-buffer
            (luvcraft-renderer-cursor-vertex-buffer renderer)
            (make-luvcraft-cursor-vertices
             (first extent) (second extent)
             (/ (first extent) 2.0) (/ (second extent) 2.0)))
           (discard-luvcraft-frame-states renderer)
           (install-luvcraft-frame-attachments renderer attachments)
           (setf installed-p t)
           ;; Publication precedes retirement.  Even if a backend reports a
           ;; destruction failure, every reader now observes the complete new
           ;; cohort instead of a half-cleared renderer.
           (release-luvcraft-frame-attachment-resources
            renderer old-attachments))
      (unless installed-p
        (with-release-warnings
          (dolist (resource
                    (luvcraft-frame-attachment-resources attachments))
            (releasing :frame-attachment-candidate (destroy resource)))))))
  renderer)

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
      ;; Pointer intent belongs to the session.  The normal frame-boundary
      ;; update replaces the renderer's centred candidate and retains DIRTY-P
      ;; if its buffer write fails.
      (setf (luvcraft-session-pointer-dirty-p session) t)
      (resize-luvcraft-component (luvcraft-session-renderer session) extent))
    extent))

(zdefun (encode-luvcraft-frame :zone :luvcraft/encode-frame)
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
    (place-video-screen-listener (luvcraft-session-video-screen session)
                                 (luvcraft-session-camera session))
    (advance-video-screen (luvcraft-session-video-screen session)
                          (luvcraft-session-device session)))
  (let ((overlays (luvcraft-session-overlays session)))
    (zone (:luvcraft/refresh-overlays :value (length overlays))
      (dolist (overlay overlays)
        (guarding-luvcraft-overlay (session overlay :overlay-refresh)
          (refresh-luvcraft-overlay overlay session)))))
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
           (/ (length body-vertices) +block-mesh-floats-per-vertex+))
         (physics-body-count (luvcraft-physics-body-count session))
         (physics-shadow-vertex-count
           (luvcraft-physics-shadow-vertex-count session)))
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
                        ;; The bodies too: shadow cubes, then spheres.
                        (if (plusp physics-body-count) 2 0)
                        (if (plusp body-vertex-count) 1 0)))
              (vertices (+ +block-world-crosshair-vertex-count+ 3
                           (* 6 text-glyph-count)
                           particle-vertex-count
                           body-vertex-count
                           (* 2 critter-vertex-count)
                           physics-shadow-vertex-count
                           (* 6 physics-body-count)
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
         (luvcraft-frame-particle-vertex-buffer frame)
         particle-vertices))
      (when (plusp critter-vertex-count)
        (write-buffer
         (luvcraft-frame-critter-vertex-buffer frame)
         critter-vertices))
      (when (plusp body-vertex-count)
        (write-buffer
         (luvcraft-frame-body-vertex-buffer frame)
         body-vertices))
      (when (plusp physics-body-count)
        (write-buffer
         (luvcraft-frame-physics-vertex-buffer frame)
         (luvcraft-physics-shadow-stream session))
        (write-buffer
         (luvcraft-frame-physics-instance-buffer frame)
         (luvcraft-physics-instance-stream session))))
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
           pass 0 (luvcraft-frame-critter-vertex-buffer frame))
          (draw pass critter-vertex-count))
        ;; So do the balls, drops, and gobbets: things with weight.  The
        ;; scene draws them as true spheres; a depth map is happy with the
        ;; little turning cubes.
        (when (plusp physics-body-count)
          (set-vertex-buffer
           pass 0 (luvcraft-frame-physics-vertex-buffer frame))
          (draw pass physics-shadow-vertex-count))
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
           pass 0 (luvcraft-frame-critter-vertex-buffer frame))
          (draw pass critter-vertex-count))
        ;; The physics bodies, as ray-traced spheres: one instanced draw of
        ;; camera-facing quads, records already ordered farthest first
        ;; because the sphere pipeline blends without writing depth.  The
        ;; block pipeline and its bindings are restored afterwards for the
        ;; player body and the particles, which expect them.
        (when (plusp physics-body-count)
          (let ((drawer (luvcraft-session-physics-sphere-drawer session)))
            (when drawer
              (set-pipeline
               pass (live-shader-pipeline-native-pipeline
                     (physics-sphere-drawer-pipeline drawer)))
              (set-vertex-buffer
               pass 0 (physics-sphere-drawer-quad-buffer drawer))
              (set-vertex-buffer
               pass 1 (luvcraft-frame-physics-instance-buffer frame))
              (set-bind-group pass 0 (luvcraft-frame-scene-bind-group frame))
              (draw pass 6 physics-body-count)
              (set-pipeline pass (luvcraft-session-pipeline session))
              (set-bind-group
               pass 0 (luvcraft-frame-scene-bind-group frame)))))
        (when (plusp particle-vertex-count)
          (set-vertex-buffer
           pass 0 (luvcraft-frame-particle-vertex-buffer frame))
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
            (guarding-luvcraft-overlay (session overlay :overlay-encode)
              (encode-luvcraft-overlay overlay session pass surface-texture))))
        ;; First-person geometry is above every world participant regardless
        ;; of overlay attachment order.  It still lives in the scene texture
        ;; (and therefore the lens/grade chain), unlike the later HUD pass.
        (dolist (overlay (reverse (luvcraft-session-overlays session)))
          (when (eq :viewmodel (luvcraft-overlay-stage overlay))
            (guarding-luvcraft-overlay (session overlay :overlay-encode)
              (encode-luvcraft-overlay overlay session pass surface-texture))))
        ;; A held item's own geometry follows the analytic hands so glass,
        ;; buttons, and live displays remain legible in the grip; neither body
        ;; nor item enters the shadow map.
        (when (plusp body-vertex-count)
          (set-pipeline pass (luvcraft-session-pipeline session))
          (set-bind-group pass 0 (luvcraft-frame-scene-bind-group frame))
          (set-vertex-buffer
           pass 0 (luvcraft-frame-body-vertex-buffer frame))
          (draw pass body-vertex-count))
        (unless (or (luvcraft-session-modal-focus session)
                    (not *luvcraft-crosshair-p*))
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
          ;; The separable pair runs twice.  Convolving the thirteen-tap
          ;; kernel with itself widens the glow by the square root of two
          ;; without a second pair of attachments or a second pipeline, and
          ;; the second pass is what turns a visible halo into light.
          (dotimes (iteration 2)
            (lens-stage (luvcraft-session-bloom-horizontal-pipeline session)
                        (luvcraft-frame-bloom-primary-bind-group frame)
                        secondary-view secondary)
            (lens-stage (luvcraft-session-bloom-vertical-pipeline session)
                        (luvcraft-frame-bloom-secondary-bind-group frame)
                        primary-view primary))
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
            (guarding-luvcraft-overlay (session overlay :overlay-encode)
              (encode-luvcraft-overlay overlay session pass surface-texture))))
        ;; KMSDRM has no native cursor plane.  A focused CLIM view still gets
        ;; the same absolute pointer events as any other backend, so draw the
        ;; crosshair geometry at their last position after the HUD itself.
        (when (and (luvcraft-session-software-cursor-p session)
                   (luvcraft-session-modal-focus session))
          ;; Motion events publish only the newest pointer state.  Consume it
          ;; once at the frame boundary, no matter how many reports SDL drained.
          (when (luvcraft-session-pointer-dirty-p session)
            (write-buffer
             (luvcraft-session-cursor-vertex-buffer session)
             (make-luvcraft-cursor-vertices
              (first extent) (second extent)
              (or (luvcraft-session-pointer-x session) (/ (first extent) 2.0))
              (or (luvcraft-session-pointer-y session)
                  (/ (second extent) 2.0))))
            (setf (luvcraft-session-pointer-dirty-p session) nil))
          (set-pipeline pass (luvcraft-session-cursor-native-pipeline session))
          (set-bind-group pass 0 (luvcraft-frame-scene-bind-group frame))
          (set-vertex-buffer
           pass 0 (luvcraft-session-cursor-vertex-buffer session))
          (draw pass +luvcraft-cursor-vertex-count+))
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
        :destination surface-texture))
      (let ((mirror (luvcraft-session-frame-mirror session)))
        (when mirror
          (encode
           encoder
           (make-gpu-copy-texture-command
            :source (luvcraft-session-presentation-texture session)
            :destination mirror)))))))

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

(defun advance-luvcraft-session-to (session timestamp)
  "Advance SESSION to the time its acquired frame is expected to be visible."
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
    (unless (luvcraft-session-focus-camera-active-p session)
      (advance-luvcraft-keyboard-look session seconds))
    (advance-luvcraft-critters session seconds)
    (advance-luvcraft-physics session seconds)
    ;; Scene participants own their behavior while core owns the frame clock.
    ;; An embodied agent therefore advances here without making core depend on
    ;; the agent system which specializes this open protocol.
    (dolist (overlay (reverse (luvcraft-session-overlays session)))
      (guarding-luvcraft-overlay (session overlay :overlay-advance)
        (advance-luvcraft-overlay overlay session seconds)))
    ;; A moving interaction takes its turn after the world it moves in and
    ;; before the player controller, which stands down while it carries them.
    (let ((focus (luvcraft-session-modal-focus session))
          (intent (luvcraft-session-movement-intent session)))
      (advance-luvcraft-focus focus session seconds)
      (when (and focus (luvcraft-focus-carries-player-p focus))
        (setf (luvcraft-session-physics-accumulator session) 0d0
              (movement-intent-jump-requested-p intent) nil)))
    (let ((player (luvcraft-session-player session))
          (intent (luvcraft-session-movement-intent session))
          (focus (luvcraft-session-modal-focus session)))
      (when (and player
                 (not (and focus (luvcraft-focus-carries-player-p focus))))
        ;; Held controls have immediate authority over the player's legs.
        ;; Move To is still a useful player primitive, but touching movement
        ;; cancels it instead of fighting a hidden autopilot.
        (when (and (body-movement-action player)
                   (not (movement-intent-still-p intent)))
          (cancel-body-movement player "cancelled by manual player input"))
        (incf (luvcraft-session-physics-accumulator session) seconds)
        (zone (:simulation/player-steps
               :value
               (floor (luvcraft-session-physics-accumulator session)
                      +player-physics-step+))
          (loop while (>= (luvcraft-session-physics-accumulator session)
                          +player-physics-step+)
                do (if (body-movement-action player)
                       (progn
                         (advance-body-movement
                          player (luvcraft-session-world session)
                          +player-physics-step+)
                         (unless (luvcraft-session-focus-camera-active-p session)
                           (sync-camera-to-player
                            (luvcraft-session-camera session) player)))
                       (step-block-world-player
                        player (luvcraft-session-world session)
                        (luvcraft-session-camera session) intent
                        +player-physics-step+
                        :jump-p (movement-intent-jump-requested-p intent)
                        :sync-camera-p
                        (not (luvcraft-session-focus-camera-active-p session))))
                   (setf (movement-intent-jump-requested-p intent) nil)
                   (decf (luvcraft-session-physics-accumulator session)
                         +player-physics-step+)))))
    (advance-luvcraft-focus-camera session seconds)
    (advance-player-body (luvcraft-session-body session) session seconds)))

(defun render-luvcraft-frame (session timestamp &optional sample)
  (declare (ignore timestamp))
  (when (luvcraft-session-running-p session)
    (describe-luvcraft-tracy-plots)
    (let ((tracy-frame-start
            (and (tracy-connected-p) (get-internal-real-time)))
          (simulation-before
            (and sample
                 (luvcraft-frame-sample-simulation-seconds sample)))
          (streaming-before
            (and sample
                 (luvcraft-frame-sample-streaming-seconds sample))))
      (with-luvcraft-frame-timing
          (sample luvcraft-frame-sample-frame-seconds :luvcraft/frame)
        (with-luvcraft-frame-timing
            (sample luvcraft-frame-sample-presentation-seconds
                    :luvcraft/presentation)
          (present-canvas-frame
           (luvcraft-session-context session)
           (lambda (surface-texture encoder presentation-time)
             ;; Acquisition is the timing boundary: simulation and every
             ;; visual clock now describe the beat this particular drawable
             ;; is predicted to reach, not when the outer timer happened to
             ;; ask for work.
             (with-luvcraft-frame-timing
                 (sample luvcraft-frame-sample-simulation-seconds
                         :luvcraft/simulation)
               (advance-luvcraft-session-to session presentation-time))
             (with-luvcraft-frame-timing
                 (sample luvcraft-frame-sample-streaming-seconds
                         :luvcraft/streaming)
               (maintain-luvcraft-residency session)
               (evict-luvcraft-products session))
             (encode-luvcraft-frame
              session surface-texture encoder :sample sample))))
        ;; PRESENTATION encloses acquisition and submission now, so remove
        ;; the explicitly measured application phases nested inside it.  This
        ;; preserves the sample's old meaning while letting acquisition choose
        ;; the frame's target time.
        (when sample
          (decf (luvcraft-frame-sample-presentation-seconds sample)
                (+ (- (luvcraft-frame-sample-simulation-seconds sample)
                      simulation-before)
                   (- (luvcraft-frame-sample-streaming-seconds sample)
                      streaming-before)))))
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
    ((session luvcraft-session) canvas (event canvas-pointer-wheel-event))
  "Offer a scroll to whatever owns focus, then to the overlays.

The player's own view does not scroll -- the wheel is not a camera control
here -- so an unconsumed wheel event is simply the end of the matter."
  (unless (dispatch-luvcraft-focus-event session canvas event)
    (dispatch-luvcraft-overlay-event session canvas event))
  nil)

(defun note-luvcraft-pointer-position (session event)
  (setf (luvcraft-session-pointer-x session) (canvas-pointer-event-x event)
        (luvcraft-session-pointer-y session) (canvas-pointer-event-y event)
        (luvcraft-session-pointer-dirty-p session) t))

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-pointer-button-press-event))
  (note-luvcraft-pointer-position session event)
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
      ((let ((item (player-body-hand-item (luvcraft-session-body session))))
         (and item (hand-item-use item (luvcraft-session-body session)
                                  session button))))
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
  (note-luvcraft-pointer-position session event)
  (unless (dispatch-luvcraft-focus-event session canvas event)
    (unless (luvcraft-session-pointer-captured-p session)
      (dispatch-luvcraft-overlay-event session canvas event)))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-pointer-motion-event))
  (note-luvcraft-pointer-position session event)
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
  ;; The window losing the pointer outright ends any capture owed back to a
  ;; modal interaction: coming back to the game is a click either way.
  (setf (luvcraft-session-pointer-capture-suspended-p session) nil)
  (clear-luvcraft-player-input session)
  (dispatch-luvcraft-focus-event session canvas event)
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  (setf (luvcraft-session-running-p session) nil)
  ;; The window is going; the film's sound must not outlive it, playing on
  ;; from a session nobody can see until something releases it.
  (alexandria:when-let ((screen (luvcraft-session-video-screen session)))
    (hush-video-screen screen))
  nil)

(defmethod handle-canvas-event
    ((session luvcraft-session) canvas (event canvas-event))
  (dispatch-luvcraft-focus-event session canvas event)
  nil)

(defun %refresh-block-atlas (session)
  "Republish both block atlases into SESSION without invalidating its meshes.

If *BLOCK-ATLAS-TILE-CAPACITY* changed, replace the renderer-owned textures
and discard binding caches which name their old views.  Vertices carry only
tile-local coordinates and mapping-scoped offsets; the next frame publishes
the replacement texture's width through the frame uniform."
  (let* ((device (luvcraft-session-device session))
         (width (* +block-atlas-tile-size+ *block-atlas-tile-capacity*))
         (height +block-atlas-tile-size+)
         (layout (make-texture-data-layout
                  :bytes-per-row (* width 4) :rows-per-image height))
         (old-atlas (luvcraft-session-atlas-texture session))
         (old-normal (luvcraft-session-normal-atlas-texture session))
         (resize-p (not (equal (gpu-texture-size old-atlas)
                               (list width height 1))))
         (atlas (and (not resize-p) old-atlas))
         (normal (and (not resize-p) old-normal))
         (atlas-view nil)
         (normal-view nil)
         (published-p nil))
    (unwind-protect
         (progn
           (when resize-p
             (setf atlas
                   (create
                    device
                    (make-texture-descriptor
                     :label "block world texture atlas"
                     :size (list width height) :dimensions :2d
                     :format (ensure-block-atlas-sample-transfer
                              +block-atlas-texture-format+)
                     :usage '(:copy-dst :texture-binding)))
                   normal
                   (create
                    device
                    (make-texture-descriptor
                     :label "block world normal atlas"
                     :size (list width height) :dimensions :2d
                     :format +block-normal-atlas-texture-format+
                     :usage '(:copy-dst :texture-binding)))
                   atlas-view
                   (create device
                           (make-texture-view-descriptor :texture atlas))
                   normal-view
                   (create device
                           (make-texture-view-descriptor :texture normal))))
           (write-texture (device-queue device)
                          (make-texture-copy :texture atlas)
                          (make-block-texture-atlas)
                          layout (list width height))
           (write-texture (device-queue device)
                          (make-texture-copy :texture normal)
                          (make-block-normal-atlas)
                          layout (list width height))
           (when resize-p
             (let ((renderer (luvcraft-session-renderer session))
                   (old-atlas-view (luvcraft-session-atlas-view session))
                   (old-normal-view
                     (luvcraft-session-normal-atlas-view session))
                   (new-resources (list atlas normal atlas-view normal-view)))
               (discard-luvcraft-frame-states renderer)
               ;; Prepare the complete owner inventory before publishing any
               ;; of the four mutually dependent atlas handles.
               (setf (luvcraft-renderer-resources renderer)
                     (append new-resources
                             (luvcraft-renderer-resources renderer)))
               (setf (luvcraft-session-atlas-texture session) atlas
                     (luvcraft-session-atlas-view session) atlas-view
                     (luvcraft-session-normal-atlas-texture session) normal
                     (luvcraft-session-normal-atlas-view session) normal-view)
               ;; The candidate is now authoritative.  A failure while
               ;; retiring the predecessor must neither roll it back nor
               ;; destroy the newly published textures during unwind.
               (setf published-p t)
               (with-release-report
                 (dolist (resource (list old-atlas-view old-normal-view
                                         old-atlas old-normal))
                   (releasing :retired-block-atlas
                     (release-luvcraft-renderer-resource
                      renderer resource)))))))
      (when (and resize-p (not published-p))
        (with-release-warnings
          (dolist (resource
                    (remove nil (list atlas-view normal-view atlas normal)))
            (releasing :block-atlas-candidate
              (release-luvcraft-renderer-resource
               (luvcraft-session-renderer session) resource)))))))
  session)

(defun refresh-block-atlas (session)
  "Republish SESSION's block atlases while its canvas cannot render them."
  (call-with-canvas-frames-held
   (lambda () (%refresh-block-atlas session))
   (list (luvcraft-session-canvas session))))

(defun start-luvcraft (&key
                                (title "luv little block world — click, look, walk")
                                ;; NIL means "as much of this display as
                                ;; comfortably fits"; a capture asks for the
                                ;; exact frame it intends to write out.  A
                                ;; KMSDRM console can only present at a real
                                ;; display mode, so the environment may pin
                                ;; the canvas to the panel's native size.
                                (width (let ((value (uiop:getenv "LUVCRAFT_WIDTH")))
                                         (and value (parse-integer value))))
                                (height (let ((value (uiop:getenv "LUVCRAFT_HEIGHT")))
                                          (and value (parse-integer value))))
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
                                (residency-radius 8)
                                (publication-limit 2)
                                (load-schedule-limit 4)
                                (mesh-capture-limit 1))
  "Open a little CPU-meshed block world.

Click to capture the pointer, look with the mouse, walk with WASD, and jump
with Space.  Once captured, left click removes the block at the centre of view
and right click places the selected block.  Number keys select materials,
middle click picks the targeted material, Shift sprints, and Escape releases
the pointer.  In the complete game, I opens the player inventory.

Everything also works without a pointer: the arrow keys look around, E places
the selected block, X removes the targeted block, C picks its material, and M
toggles pointer capture for machines whose pointing device has no buttons.
Control-Q quits from anywhere, saving the world on the way out.

Pass :PROVIDER to select the Vulkan or Metal relationship without changing
world, simulation, streaming, or frame orchestration.  Pass :VISIBLE-P NIL to
keep the SDL window hidden while still exercising the real presentation path.
Pass :FRAMES-PER-SECOND NIL for a capture-only demand clock.  On KMSDRM any
non-NIL value selects display-paced animation because FIFO scanout, rather
than an independent host timer, owns the available frame rate.  Pass
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
         (renderer nil)
         (world-text-glyph-cache nil)
         (world-text-run nil)
         (video-screen nil)
         (session nil) (production-system nil) (completed-p nil))
    ;; FFmpeg has to be dlopened before the canvas exists.  Film is now a live
    ;; terminal-wall mode, so waiting until the user opens its browser would
    ;; attempt the first dlopen under Cocoa's running canvas and hang.  Preload
    ;; the libraries here; no decoder or file is opened until Film is chosen.
    (libav:load-libav)
    (retry-decoded-video-picture-release-backlog)
    (retry-video-screen-release-backlog)
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
                    (let ((attachments
                            (make-luvcraft-frame-attachments
                             device context extent)))
                      (mapc #'keep
                            (luvcraft-frame-attachment-resources attachments))
                      attachments))
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
                    (* +block-atlas-tile-size+ *block-atlas-tile-capacity*))
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
                  (cursor-vertices
                    (make-luvcraft-cursor-vertices
                     (first extent) (second extent)
                     (/ (first extent) 2.0) (/ (second extent) 2.0)))
                  (crosshair-vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "block world crosshair vertices"
                       :size (* 4 (length crosshair-vertices))
                       :usage '(:vertex)))))
                  (cursor-vertex-buffer
                    (keep
                     (create
                      device
                      (make-buffer-descriptor
                       :label "luvcraft software cursor vertices"
                       :size (* 4 (length cursor-vertices))
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
                             '((:array-stride 56
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
                                  :format :float32x2))))
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
                             '((:array-stride 56
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
                                  :format :float32x2))))
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
                  (cursor-pipeline
                    (let ((artifact
                            (make-live-shader-pipeline
                             :role :cursor
                             :vertex-role :cursor
                             :label "luvcraft software cursor pipeline"
                             :device device :layout layout
                             :vertex-buffers
                             '((:array-stride 20
                                :attributes
                                ((:shader-location 0 :offset 0
                                  :format :float32x3)
                                 (:shader-location 1 :offset 12
                                  :format :float32x2))))
                             :target-format (canvas-format context)
                             :target-blend :premultiplied-alpha
                             :primitive '(:topology :triangle-list)
                             :depth-stencil nil)))
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
                  (renderer-owner
                    (setf renderer
                          (make-instance
                           'luvcraft-renderer
                           :device device :context context
                           :atlas-texture atlas-texture
                           :atlas-view atlas-view
                           :atlas-sampler atlas-sampler
                           :normal-atlas-texture normal-atlas-texture
                           :normal-atlas-view normal-atlas-view
                           :frame-attachments attachments
                           :shadow-depth-texture shadow-depth-texture
                           :shadow-depth-view shadow-depth-view
                           :shadow-depth-sampler shadow-depth-sampler
                           :shadow-comparison-sampler shadow-comparison-sampler
                           :layout layout :shadow-layout shadow-layout
                           :post-layout post-layout :bloom-layout bloom-layout
                           :linear-sampler linear-sampler
                           :bloom-bright-pipeline bloom-bright-pipeline
                           :bloom-horizontal-pipeline bloom-horizontal-pipeline
                           :bloom-vertical-pipeline bloom-vertical-pipeline
                           :sun-shaft-pipeline sun-shaft-pipeline
                           :block-pipeline pipeline
                           :shadow-pipeline shadow-pipeline
                           :sky-vertex-buffer sky-vertex-buffer
                           :sky-pipeline sky-pipeline
                           :crosshair-vertex-buffer crosshair-vertex-buffer
                           :cursor-vertex-buffer cursor-vertex-buffer
                           :crosshair-pipeline crosshair-pipeline
                           :cursor-pipeline cursor-pipeline
                           :post-pipeline post-pipeline
                           :resources resources)))
                  (new-session
                    (make-instance
                     'luvcraft-session
                     :renderer renderer-owner
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
                     :software-cursor-p
                     (string-equal (or (uiop:getenv "SDL_VIDEODRIVER") "")
                                   "kmsdrm")
                     :critters critters
                     :world-text text-run
                     :world-text-glyph-cache text-glyph-cache)))
               (write-buffer sky-vertex-buffer sky-vertices)
               (write-buffer crosshair-vertex-buffer crosshair-vertices)
               (write-buffer cursor-vertex-buffer cursor-vertices)
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
               (attach-player-body-sdf session)
               (attach-physics-sphere-drawer session)
               (start-luvcraft-lobby session)
               (attach-luvcraft-hud session)
               (maintain-luvcraft-residency session)
               (refresh-luvcraft-mesh session)
               (setf (canvas-event-handler canvas) session)
               (when visible-p
                 ;; Preserve asynchronous residency after startup, but do not
                 ;; publish the window until the nearest immutable mesh is
                 ;; ready.
                 ;;
                 ;; The window is mapped before that first frame rather than
                 ;; after it.  A Wayland surface that has never been mapped
                 ;; receives no frame callbacks, and a FIFO present waits for
                 ;; one, so presenting first and showing afterwards is a wait
                 ;; for a frame that cannot arrive until the wait ends.
                 ;; Nothing can break that tie from outside either:
                 ;; SHOW-CANVAS runs on the canvas thread, which is the very
                 ;; thread the present is holding.  Waiting for the mesh is
                 ;; what keeps the window from opening on an empty world; the
                 ;; frame merely has to come second.
                 (wait-for-luvcraft-products session :minimum 1)
                 (show-canvas canvas)
                 (request-canvas-frame
                  canvas
                  (lambda (timestamp)
                    (render-luvcraft-frame session timestamp))))
               (setf (canvas-clock canvas)
                     (if frames-per-second
                         (let ((frame-function
                                 (lambda (native-canvas timestamp)
                                   (declare (ignore native-canvas))
                                   (render-luvcraft-frame session timestamp))))
                           (if (luv::sdl-canvas-direct-display-p canvas)
                               (make-presentation-clock frame-function)
                               (make-cadence-clock
                                frame-function
                                :frames-per-second frames-per-second)))
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
            (releasing :lobby (stop-luvcraft-lobby session))
            (releasing :chunk-products
              (destroy-luvcraft-chunk-products session))
            (releasing :focus (unfocus-luvcraft-session session))
            (dolist (overlay (luvcraft-session-overlays session))
              (releasing :overlay (release-luvcraft-overlay overlay)))
            (setf (luvcraft-session-overlays session) nil))
          (if renderer
              (releasing :renderer (release-luvcraft-component renderer))
              (progn
                (dolist (pipeline pipelines)
                  (releasing :pipeline
                    (release-live-shader-pipeline pipeline)))
                (dolist (resource resources)
                  (releasing :resource (destroy resource)))))
          (when video-screen
            (releasing :video-screen
              (release-video-screen-or-retain video-screen)))
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
    (releasing :lobby (stop-luvcraft-lobby session))
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
      ;; The session coordinates one renderer owner; it no longer reproduces
      ;; the renderer's pipeline and resource inventories during teardown.
      (releasing :renderer
        (release-luvcraft-component (luvcraft-session-renderer session)))
      (when (luvcraft-session-video-screen session)
        (let ((screen (luvcraft-session-video-screen session)))
          (releasing :video-screen
            (release-video-screen screen)
            (when (video-screen-released-p screen)
              (setf (luvcraft-session-video-screen session) nil)))))
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
