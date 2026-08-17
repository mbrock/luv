;;; The atelier: a small solid world, its surface chain, and the GPU objects
;;; that draw it.
;;;
;;; Nothing here meshes.  The world is a 3-chain of solid cells; REFRESH-SCENE
;;; takes its boundary, orders the resulting face terms by chunk, pads them to
;;; whole bricks, and measures a bounding sphere per brick.  The renderer
;;; uploads exactly those two arrays and one frame block, then dispatches one
;;; task workgroup per brick.

(in-package #:luft.render)

;;; ------------------------------------------------------------------------
;;; Scenes

(defconstant +brick-size+ shaders:+brick-size+)
(defconstant +chunk-bits+ 3
  "Terms are ordered by 8-cell chunk so a brick's faces stay close together.")

(defclass scene ()
  ((domain
    :initarg :domain
    :reader scene-domain)
   (solid
    :initarg :solid
    :reader scene-solid
    :documentation "The solid world: a 3-chain of unit cells.")
   (surface
    :initform nil
    :accessor scene-surface
    :documentation "The boundary of SOLID: exposed faces with unit signs.")
   (terms
    :initform nil
    :accessor scene-terms
    :documentation "Packed surface terms in brick order, zero-padded.")
   (bricks
    :initform nil
    :accessor scene-bricks
    :documentation "Four floats per brick: bounding sphere centre and radius.")
   (brick-count
    :initform 0
    :accessor scene-brick-count)
   (cell-bits
    :initform nil
    :accessor scene-cell-bits
    :documentation "The solid chain as dense (unsigned-byte 32) cell bits."))
  (:documentation "A solid world together with its drawable surface products."))

(defun make-scene (domain &key (solid (luft:make-solid-chain domain)))
  "Make a scene over DOMAIN and refresh its surface products once."
  (refresh-scene (make-instance 'scene :domain domain :solid solid)))

(defun term-chunk-key (term)
  "A fixnum ordering key grouping TERM's site by chunk, then by site."
  (let ((site (luft:packed-term-site term)))
    (logior (ash (ash (luft:site-z site) (- +chunk-bits+)) 45)
            (ash (ash (luft:site-y site) (- +chunk-bits+)) 24)
            (ash (luft:site-x site) (- +chunk-bits+)))))

(defun order-terms-by-chunk (terms)
  "Return a fresh copy of the site-ordered TERMS grouped by chunk."
  (stable-sort (copy-seq terms) #'< :key #'term-chunk-key))

(defun brick-spheres (terms brick-count)
  "Measure a bounding sphere for each brick of TERMS as four floats each."
  (let ((spheres (make-array (* 4 brick-count) :element-type 'single-float
                                               :initial-element 0.0)))
    (dotimes (brick brick-count spheres)
      (let ((low-x nil) (low-y nil) (low-z nil)
            (high-x nil) (high-y nil) (high-z nil))
        (loop for index from (* brick +brick-size+)
                below (* (1+ brick) +brick-size+)
              for term = (aref terms index)
              unless (zerop term)
                do (let* ((site (luft:packed-term-site term))
                          (x (luft:site-x site))
                          (y (luft:site-y site))
                          (z (luft:site-z site))
                          (x-high (+ x (if (luft:site-extends-p site :x) 1 0)))
                          (y-high (+ y (if (luft:site-extends-p site :y) 1 0)))
                          (z-high (+ z (if (luft:site-extends-p site :z) 1 0))))
                     (setf low-x (if low-x (min low-x x) x)
                           low-y (if low-y (min low-y y) y)
                           low-z (if low-z (min low-z z) z)
                           high-x (if high-x (max high-x x-high) x-high)
                           high-y (if high-y (max high-y y-high) y-high)
                           high-z (if high-z (max high-z z-high) z-high))))
        (when low-x
          (let* ((center-x (/ (+ low-x high-x) 2.0))
                 (center-y (/ (+ low-y high-y) 2.0))
                 (center-z (/ (+ low-z high-z) 2.0))
                 (radius (sqrt (+ (expt (- high-x center-x) 2)
                                  (expt (- high-y center-y) 2)
                                  (expt (- high-z center-z) 2)))))
            (setf (aref spheres (* 4 brick)) (coerce center-x 'single-float)
                  (aref spheres (+ 1 (* 4 brick))) (coerce center-y 'single-float)
                  (aref spheres (+ 2 (* 4 brick))) (coerce center-z 'single-float)
                  (aref spheres (+ 3 (* 4 brick)))
                  (coerce radius 'single-float))))))))

(defun refresh-scene (scene)
  "Recompute SCENE's surface chain, brick-ordered terms, and brick spheres."
  (let* ((surface (luft:surface-chain (scene-solid scene)))
         (ordered (order-terms-by-chunk (luft:chain-packed-terms surface)))
         (brick-count (max 1 (ceiling (length ordered) +brick-size+)))
         (terms (make-array (* brick-count +brick-size+)
                            :element-type '(unsigned-byte 64)
                            :initial-element 0)))
    (replace terms ordered)
    (setf (scene-surface scene) surface
          (scene-terms scene) terms
          (scene-brick-count scene) brick-count
          (scene-bricks scene) (brick-spheres terms brick-count)
          (scene-cell-bits scene) (luft:chain-cell-bits (scene-solid scene)))
    scene))

;;; ------------------------------------------------------------------------
;;; A demonstration world

(defun demo-height (x y)
  "Rolling ground with a plateau, in cells."
  (let ((rolling (+ 5.5
                    (* 2.4 (sin (/ x 6.0)) (cos (/ y 7.5)))
                    (* 1.2 (sin (/ (+ x y) 4.3)))))
        (plateau (if (and (<= 40 x 52) (<= 10 y 22)) 3.0 0.0)))
    (max 1 (floor (+ rolling plateau)))))

(defun make-demo-scene (&key (horizontal-bits 6))
  "A small textureless world: rolling ground, a tower, and a floating slab."
  (let* ((domain (luft:make-world-domain :horizontal-bits horizontal-bits))
         (period (luft:world-domain-x-period domain))
         (solid (luft:make-solid-chain domain)))
    (dotimes (x period)
      (dotimes (y period)
        (dotimes (z (demo-height x y))
          (setf (luft:solid-cell-p solid x y z) t))))
    ;; A hollow tower with a doorway.
    (loop for z from 1 below 22
          do (loop for x from 20 to 27
                   do (loop for y from 30 to 37
                            when (and (or (= x 20) (= x 27) (= y 30) (= y 37))
                                      (not (and (= y 30) (<= 23 x 24) (< z 9))))
                              do (setf (luft:solid-cell-p solid x y z) t))))
    ;; A floating slab, casting a clean shadow of empty air.
    (loop for x from 8 to 15
          do (loop for y from 8 to 12
                   do (setf (luft:solid-cell-p solid x y 14) t)))
    ;; A staircase up the plateau.
    (loop for step from 0 below 6
          do (loop for y from 14 to 18
                   do (loop for z from 0 to (+ 4 step)
                            do (setf (luft:solid-cell-p solid (- 39 step) y z)
                                     t))))
    (make-scene domain :solid solid)))

;;; ------------------------------------------------------------------------
;;; Camera

(defclass fly-camera ()
  ((position
    :initarg :position
    :accessor camera-position)
   (yaw
    :initarg :yaw
    :initform 0.0
    :accessor camera-yaw
    :documentation "Radians from +X toward +Y.")
   (pitch
    :initarg :pitch
    :initform 0.0
    :accessor camera-pitch
    :documentation "Radians above the horizon; Z is up.")
   (field-of-view
    :initarg :field-of-view
    :initform (* 70.0 (/ pi 180))
    :accessor camera-field-of-view)))

(defun make-fly-camera (&key (position (vec3:make-vec3 46.0 2.0 15.0))
                          (yaw 2.15) (pitch -0.22)
                          (field-of-view (* 70.0 (/ pi 180))))
  (make-instance 'fly-camera :position position :yaw yaw :pitch pitch
                             :field-of-view field-of-view))

(defun camera-basis (camera)
  "Return the camera's RIGHT, UP, and FORWARD unit vectors in a Z-up world."
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3:make-vec3 (* (cos yaw) (cos pitch))
                                  (* (sin yaw) (cos pitch))
                                  (sin pitch)))
         (right (vec3:make-vec3 (sin yaw) (- (cos yaw)) 0.0))
         (up (vec3:vec3-cross right forward)))
    (values right up forward)))

(defparameter *near-distance* 0.1)
(defparameter *far-distance* 400.0)
(defparameter *sun-direction*
  (vec3:vec3-normalize (vec3:make-vec3 0.52 0.30 0.62))
  "The direction toward the key light, low enough to model the terraces.")
(defparameter *sun-color* (vec3:make-vec3 1.05 0.96 0.82)
  "The key light's radiance, warm as afternoon sun.")
(defparameter *sheen-strength* 0.16
  "How brightly a face catches the sun's reflection; chamfers show it most.")
(defparameter *fill-direction*
  (vec3:vec3-normalize (vec3:make-vec3 -0.62 -0.55 0.24))
  "The direction toward the cool fill light opposite the sun.")
(defparameter *fill-strength* 0.30
  "The fill light's strength, which separates the faces the sun misses.")
(defparameter *ambient-light* 0.42
  "The strength of the ambient hemisphere: sky above, bounce below.")
(defparameter *ground-color* (vec3:make-vec3 0.34 0.30 0.24)
  "The bounce colour of the lower hemisphere.")
(defparameter *top-color* (vec3:make-vec3 0.17 0.36 0.11)
  "The material of an upward face: turf, in linear light.")
(defparameter *side-color* (vec3:make-vec3 0.42 0.32 0.21)
  "The material of a sideways face: the earth a cut exposes.")
(defparameter *bottom-color* (vec3:make-vec3 0.11 0.10 0.10)
  "The material of a downward face: an underside, seen rarely.")
(defparameter *shadow-strength* 1.0
  "How darkly the sun's walk shadows a point; zero switches shadows off.")
(defparameter *occlusion-strength* 0.75
  "How deeply the crowding of nearby cells darkens the ambient hemisphere.")
(defparameter *exposure* 1.15
  "Exposure of the 1 - exp(-x) curve the lit colour rolls off through.")
(defparameter *sky-color* (vec3:make-vec3 0.62 0.76 0.92))
(defparameter *fog-distance* 140.0)
(defparameter *bevel-radius* 0.22
  "The crease rounding radius, or chamfer width, in cells, below one half.")
(defparameter *sanding-width* 0.05
  "How far on either side of a chamfer's arris the :CHAMFER style softens
the facet normal, in cells.")

(defun frame-uniform-data (camera width height &optional domain)
  "Pack the frame block: camera, basis, projection, sun, sky, and domain lanes."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let* ((near *near-distance*)
           (far *far-distance*)
           (focal (/ (tan (/ (camera-field-of-view camera) 2.0))))
           (aspect (/ (coerce width 'single-float) height))
           (data (make-array 60 :element-type 'single-float))
           (index 0))
      (flet ((lane (vector fourth)
               (setf (aref data index) (coerce (vec3:vec3-x vector) 'single-float)
                     (aref data (+ index 1))
                     (coerce (vec3:vec3-y vector) 'single-float)
                     (aref data (+ index 2))
                     (coerce (vec3:vec3-z vector) 'single-float)
                     (aref data (+ index 3)) (coerce fourth 'single-float))
               (incf index 4)))
        (lane (camera-position camera) 0.0)
        (lane right 0.0)
        (lane up 0.0)
        (lane forward 0.0)
        (lane (vec3:make-vec3 (/ focal aspect) focal (/ far (- far near)))
              (/ (- (* far near)) (- far near)))
        (lane *sun-direction* *ambient-light*)
        (lane *sky-color* *fog-distance*)
        (lane (vec3:make-vec3
               (if domain (luft:world-domain-x-period domain) 1)
               (if domain (luft:world-domain-y-period domain) 1)
               *bevel-radius*)
              *sanding-width*)
        (lane *sun-color* *sheen-strength*)
        (lane *fill-direction* *fill-strength*)
        (lane *ground-color* *exposure*)
        (lane (vec3:make-vec3 *occlusion-strength* *shadow-strength* 0.0) 0.0)
        (lane *top-color* 0.0)
        (lane *side-color* 0.0)
        (lane *bottom-color* 0.0))
      data)))

;;; ------------------------------------------------------------------------
;;; Renderer

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   (owns-device-p :initarg :owns-device-p :initform nil
                  :reader renderer-owns-device-p)
   (scene :initarg :scene :accessor renderer-scene)
   (camera :initarg :camera :accessor renderer-camera)
   (extent :initarg :extent :reader renderer-extent)
   (color-format :initarg :color-format :reader renderer-color-format)
   (color-texture :initform nil :accessor renderer-color-texture)
   (color-view :initform nil :accessor renderer-color-view)
   (depth-texture :initform nil :accessor renderer-depth-texture)
   (depth-view :initform nil :accessor renderer-depth-view)
   (uniform-buffer :initform nil :accessor renderer-uniform-buffer)
   (terms-buffer :initform nil :accessor renderer-terms-buffer)
   (bricks-buffer :initform nil :accessor renderer-bricks-buffer)
   (cells-buffer :initform nil :accessor renderer-cells-buffer)
   (terms-capacity :initform 0 :accessor renderer-terms-capacity)
   (bricks-capacity :initform 0 :accessor renderer-bricks-capacity)
   (cells-capacity :initform 0 :accessor renderer-cells-capacity)
   (layout :initform nil :accessor renderer-layout)
   (bind-group :initform nil :accessor renderer-bind-group)
   (modules :initform nil :accessor renderer-modules)
   (pipelines :initform nil :accessor renderer-pipelines
              :documentation "A plist from style to mesh pipeline.")
   (style :initarg :style :initform :bevel :accessor renderer-style
          :documentation
          "Which pipeline draws: :FLAT, :BEVEL (rounded), or :CHAMFER.")
   (uploaded-scene :initform nil :accessor renderer-uploaded-scene))
  (:documentation "GPU resources drawing one scene from one camera."))

(defun frame-uniform-size ()
  (let ((size (spv:shader-uniform-block-byte-size (shaders:frame-uniform-block))))
    (unless (= size (* 4 (length (frame-uniform-data (make-fly-camera) 1 1))))
      (error "Frame block is ~D bytes but the host packs ~D."
             size (* 4 (length (frame-uniform-data (make-fly-camera) 1 1)))))
    size))

(defun renderer-pipeline (renderer &optional (style (renderer-style renderer)))
  (or (getf (renderer-pipelines renderer) style)
      (error "Renderer has no ~S pipeline." style)))

(defun create-renderer-targets (renderer)
  (let* ((device (renderer-device renderer))
         (extent (renderer-extent renderer))
         (color (create device
                        (make-texture-descriptor
                         :label "luft surface color"
                         :size extent :dimensions :2d
                         :format (renderer-color-format renderer)
                         :usage '(:render-attachment :copy-src))))
         (depth (create device
                        (make-texture-descriptor
                         :label "luft surface depth"
                         :size extent :dimensions :2d
                         :format :depth32-float
                         :usage '(:render-attachment)))))
    (setf (renderer-color-texture renderer) color
          (renderer-color-view renderer)
          (create device (make-texture-view-descriptor :texture color))
          (renderer-depth-texture renderer) depth
          (renderer-depth-view renderer)
          (create device (make-texture-view-descriptor :texture depth)))))

(defun create-renderer-pipeline (renderer)
  (let* ((device (renderer-device renderer))
         (task (create device
                       (make-shader-module-descriptor
                        :label "luft surface task"
                        :language :mathematical
                        :code (shaders:surface-task-shader))))
         (mesh (create device
                       (make-shader-module-descriptor
                        :label "luft surface mesh"
                        :language :mathematical
                        :code (shaders:surface-mesh-shader))))
         (bevel (create device
                        (make-shader-module-descriptor
                         :label "luft bevel mesh"
                         :language :mathematical
                         :code (shaders:bevel-mesh-shader))))
         (chamfer (create device
                          (make-shader-module-descriptor
                           :label "luft chamfer mesh"
                           :language :mathematical
                           :code (shaders:chamfer-mesh-shader))))
         (fragment (create device
                           (make-shader-module-descriptor
                            :label "luft surface fragment"
                            :language :mathematical
                            :code (shaders:surface-fragment-shader))))
         (chamfer-fragment
           (create device
                   (make-shader-module-descriptor
                    :label "luft chamfer fragment"
                    :language :mathematical
                    :code (shaders:chamfer-fragment-shader))))
         (layout (create device
                         (make-bind-group-layout-descriptor
                          :label "luft surface layout"
                          :entries
                          `((:binding ,shaders:+frame-binding+
                             :type :uniform-buffer)
                            (:binding ,shaders:+terms-binding+
                             :type :storage-buffer)
                            (:binding ,shaders:+bricks-binding+
                             :type :storage-buffer)
                            (:binding ,shaders:+cells-binding+
                             :type :storage-buffer))))))
    (flet ((pipeline (label mesh-module fragment-module)
             (create device
                     (make-mesh-render-pipeline-descriptor
                      :label label
                      :layout layout
                      :task `(:module ,task)
                      :mesh `(:module ,mesh-module)
                      :fragment
                      `(:module ,fragment-module
                        :targets ((:format
                                   ,(renderer-color-format renderer))))
                      :max-mesh-workgroups 1
                      :depth-stencil '(:format :depth32-float
                                       :depth-write-enabled t
                                       :depth-compare :less)))))
      (setf (renderer-modules renderer)
            (list task mesh bevel chamfer fragment chamfer-fragment)
            (renderer-layout renderer) layout
            (renderer-pipelines renderer)
            (list :flat (pipeline "luft surface pipeline" mesh fragment)
                  :bevel (pipeline "luft bevel pipeline" bevel fragment)
                  :chamfer (pipeline "luft chamfer pipeline"
                                     chamfer chamfer-fragment))))))

(defun make-renderer (&key scene camera device
                        (provider *gpu-provider*)
                        (width 1280) (height 800)
                        (color-format :rgba8-unorm-srgb)
                        (style :bevel))
  "Create every GPU object needed to draw SCENE from CAMERA at WIDTH by HEIGHT.

STYLE is :BEVEL or :FLAT and may be changed later with (SETF RENDERER-STYLE).
Without DEVICE, one is requested from PROVIDER and owned by the renderer."
  (let* ((owns-device-p (null device))
         (device (or device
                     (request-gpu-device
                      provider (make-device-descriptor :label "luft atelier"))))
         (renderer (make-instance 'renderer
                                  :device device :owns-device-p owns-device-p
                                  :scene scene :camera camera
                                  :extent (list width height)
                                  :color-format color-format
                                  :style style))
         (completed-p nil))
    (unwind-protect
         (progn
           (create-renderer-targets renderer)
           (setf (renderer-uniform-buffer renderer)
                 (create device
                         (make-buffer-descriptor
                          :label "luft frame block"
                          :size (frame-uniform-size)
                          :usage '(:uniform))))
           (create-renderer-pipeline renderer)
           (upload-scene renderer)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (destroy-renderer renderer)))))

(defun destroy-renderer (renderer)
  "Release every GPU object of RENDERER, and its device when it owns one."
  (dolist (resource (list* (renderer-bind-group renderer)
                           (renderer-layout renderer)
                           (loop for (nil pipeline) on (renderer-pipelines renderer)
                                   by #'cddr
                                 collect pipeline)))
    (when resource (ignore-errors (destroy resource))))
  (dolist (module (renderer-modules renderer))
    (ignore-errors (destroy module)))
  (dolist (resource (list (renderer-terms-buffer renderer)
                          (renderer-bricks-buffer renderer)
                          (renderer-cells-buffer renderer)
                          (renderer-uniform-buffer renderer)
                          (renderer-color-view renderer)
                          (renderer-color-texture renderer)
                          (renderer-depth-view renderer)
                          (renderer-depth-texture renderer)))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-bind-group renderer) nil
        (renderer-pipelines renderer) nil
        (renderer-layout renderer) nil
        (renderer-modules renderer) nil
        (renderer-terms-buffer renderer) nil
        (renderer-bricks-buffer renderer) nil
        (renderer-cells-buffer renderer) nil
        (renderer-uniform-buffer renderer) nil)
  (when (renderer-owns-device-p renderer)
    (ignore-errors (destroy (renderer-device renderer))))
  (values))

(defun ensure-storage-buffer (renderer accessor capacity-accessor
                              needed label)
  "Return a storage buffer of at least NEEDED bytes, recreating on growth.

The second value is true when a new buffer was created."
  (let ((buffer (funcall accessor renderer)))
    (if (and buffer (<= needed (funcall capacity-accessor renderer)))
        (values buffer nil)
        (let ((new (create (renderer-device renderer)
                           (make-buffer-descriptor
                            :label label :size needed
                            :usage '(:storage)))))
          (when buffer (destroy buffer))
          (funcall (fdefinition `(setf ,accessor)) new renderer)
          (funcall (fdefinition `(setf ,capacity-accessor)) needed renderer)
          (values new t)))))

(defun upload-scene (renderer &optional (scene (renderer-scene renderer)))
  "Upload SCENE's terms and brick spheres, rebinding when buffers grow."
  (let* ((terms (scene-terms scene))
         (bricks (scene-bricks scene))
         (rebind-p (null (renderer-bind-group renderer))))
    (multiple-value-bind (terms-buffer new-p)
        (ensure-storage-buffer renderer 'renderer-terms-buffer
                               'renderer-terms-capacity
                               (* 8 (length terms)) "luft surface terms")
      (when new-p (setf rebind-p t))
      (write-buffer terms-buffer terms))
    (multiple-value-bind (bricks-buffer new-p)
        (ensure-storage-buffer renderer 'renderer-bricks-buffer
                               'renderer-bricks-capacity
                               (* 4 (length bricks)) "luft surface bricks")
      (when new-p (setf rebind-p t))
      (write-buffer bricks-buffer bricks))
    (multiple-value-bind (cells-buffer new-p)
        (ensure-storage-buffer renderer 'renderer-cells-buffer
                               'renderer-cells-capacity
                               (* 4 (length (scene-cell-bits scene)))
                               "luft solid cells")
      (when new-p (setf rebind-p t))
      (write-buffer cells-buffer (scene-cell-bits scene)))
    (when rebind-p
      (when (renderer-bind-group renderer)
        (destroy (renderer-bind-group renderer)))
      (setf (renderer-bind-group renderer)
            (create (renderer-device renderer)
                    (make-bind-group-descriptor
                     :label "luft surface bindings"
                     :layout (renderer-layout renderer)
                     :entries
                     `((:binding ,shaders:+frame-binding+
                        :resource ,(renderer-uniform-buffer renderer))
                       (:binding ,shaders:+terms-binding+
                        :resource ,(renderer-terms-buffer renderer))
                       (:binding ,shaders:+bricks-binding+
                        :resource ,(renderer-bricks-buffer renderer))
                       (:binding ,shaders:+cells-binding+
                        :resource ,(renderer-cells-buffer renderer)))))))
    (setf (renderer-scene renderer) scene
          (renderer-uploaded-scene renderer) scene)
    renderer))

(defun encode-frame (renderer encoder)
  "Encode one frame of RENDERER's scene into its color texture on ENCODER."
  (let* ((extent (renderer-extent renderer))
         (scene (renderer-scene renderer))
         (sky *sky-color*))
    (unless (eq scene (renderer-uploaded-scene renderer))
      (upload-scene renderer scene))
    (write-buffer (renderer-uniform-buffer renderer)
                  (frame-uniform-data (renderer-camera renderer)
                                      (first extent) (second extent)
                                      (scene-domain scene)))
    (let ((pass (begin-render-pass
                 encoder
                 (make-render-pass-descriptor
                  :label "luft surface pass"
                  :color-attachments
                  `((:view ,(renderer-color-view renderer)
                     :load-op :clear :store-op :store
                     :clear-value ,(vector (vec3:vec3-x sky) (vec3:vec3-y sky)
                                           (vec3:vec3-z sky) 1.0)))
                  :depth-stencil-attachment
                  `(:view ,(renderer-depth-view renderer)
                    :depth-load-op :clear :depth-store-op :discard
                    :depth-clear-value 1.0)))))
      (set-pipeline pass (renderer-pipeline renderer))
      (set-bind-group pass 0 (renderer-bind-group renderer))
      (draw-mesh-workgroups pass (scene-brick-count scene))
      (end-pass pass))
    (renderer-color-texture renderer)))

(defun render-pixels (renderer)
  "Render one frame headlessly and return its packed pixel bytes.

The further values are the width, height, and colour format of the pixels."
  (let* ((device (renderer-device renderer))
         (extent (renderer-extent renderer))
         (readback (create device
                           (make-buffer-descriptor
                            :label "luft surface readback"
                            :size (* 4 (first extent) (second extent))
                            :usage '(:copy-dst))))
         (encoder nil)
         (command-buffer nil))
    (unwind-protect
         (progn
           (setf encoder (create device
                                 (make-command-encoder-descriptor
                                  :label "luft surface frame")))
           (encode-frame renderer encoder)
           (encode encoder
                   (make-gpu-copy-texture-to-buffer-command
                    :source (renderer-color-texture renderer)
                    :destination readback))
           (setf command-buffer (finish encoder))
           (submit (device-queue device) command-buffer)
           (values (read-buffer readback)
                   (first extent) (second extent)
                   (renderer-color-format renderer)))
      (when command-buffer (destroy command-buffer))
      (when encoder (destroy encoder))
      (destroy readback))))

(defun render-to-png (renderer pathname)
  "Render one frame headlessly and write it to PATHNAME as a PNG."
  (multiple-value-bind (pixels width height format) (render-pixels renderer)
    (ensure-directories-exist pathname)
    (write-rgba-png pathname pixels width height format)))

(defun capture-demo-png (pathname &key (width 1280) (height 800)
                                    (camera (make-fly-camera)))
  "Render the demonstration scene once to PATHNAME and release everything."
  (let ((renderer (make-renderer :scene (make-demo-scene) :camera camera
                                 :width width :height height)))
    (unwind-protect
         (render-to-png renderer pathname)
      (destroy-renderer renderer))))

;;; ------------------------------------------------------------------------
;;; Viewer: a window with a fly camera

(defvar *viewer* nil "The most recently started viewer.")

(defclass viewer (canvas-event-handler)
  ((canvas :initarg :canvas :reader viewer-canvas)
   (context :initarg :context :reader viewer-context)
   (renderer :initarg :renderer :accessor viewer-renderer)
   (pressed-keys :initform (make-hash-table :test #'eq)
                 :reader viewer-pressed-keys)
   (pointer-captured-p :initform nil :accessor viewer-pointer-captured-p)
   (running-p :initform t :accessor viewer-running-p)
   (last-timestamp :initform nil :accessor viewer-last-timestamp)
   (speed :initarg :speed :initform 12.0 :accessor viewer-speed)
   (sensitivity :initarg :sensitivity :initform 0.0032
                :accessor viewer-sensitivity)))

(defun viewer-key-down-p (viewer &rest names)
  (some (lambda (name) (gethash name (viewer-pressed-keys viewer))) names))

(defun advance-viewer-camera (viewer timestamp)
  (let* ((last (viewer-last-timestamp viewer))
         (dt (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0))
         (camera (renderer-camera (viewer-renderer viewer)))
         (step (* dt (viewer-speed viewer)
                  (if (viewer-key-down-p viewer :left-shift :right-shift)
                      3.0 1.0))))
    (setf (viewer-last-timestamp viewer) timestamp)
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore up))
      (flet ((move (direction amount)
               (setf (camera-position camera)
                     (let ((position (camera-position camera)))
                       (vec3:make-vec3
                        (+ (vec3:vec3-x position)
                           (* amount (vec3:vec3-x direction)))
                        (+ (vec3:vec3-y position)
                           (* amount (vec3:vec3-y direction)))
                        (+ (vec3:vec3-z position)
                           (* amount (vec3:vec3-z direction))))))))
        (when (viewer-key-down-p viewer :w :up) (move forward step))
        (when (viewer-key-down-p viewer :s :down) (move forward (- step)))
        (when (viewer-key-down-p viewer :d :right) (move right step))
        (when (viewer-key-down-p viewer :a :left) (move right (- step)))
        (when (viewer-key-down-p viewer :space :e)
          (move (vec3:make-vec3 0 0 1) step))
        (when (viewer-key-down-p viewer :left-control :q :c)
          (move (vec3:make-vec3 0 0 1) (- step)))))))

(defun render-viewer-frame (viewer timestamp)
  (unless (viewer-running-p viewer)
    (return-from render-viewer-frame nil))
  (advance-viewer-camera viewer timestamp)
  (present-canvas-frame
   (viewer-context viewer)
   (lambda (surface-texture encoder)
     (let ((color (encode-frame (viewer-renderer viewer) encoder)))
       (encode encoder
               (make-gpu-copy-texture-command
                :source color :destination surface-texture))))))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (if (eq key :escape)
        (when (viewer-pointer-captured-p viewer)
          (set-canvas-relative-pointer-mode canvas nil)
          (setf (viewer-pointer-captured-p viewer) nil))
        (setf (gethash key (viewer-pressed-keys viewer)) t)))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (remhash (canvas-key-event-key-name event) (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-button-press-event))
  (when (and (not (viewer-pointer-captured-p viewer))
             (eq :left (canvas-pointer-event-button event)))
    (set-canvas-relative-pointer-mode canvas t)
    (setf (viewer-pointer-captured-p viewer) t))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (when (viewer-pointer-captured-p viewer)
    (let ((camera (renderer-camera (viewer-renderer viewer)))
          (sensitivity (viewer-sensitivity viewer)))
      (decf (camera-yaw camera)
            (* (canvas-pointer-event-delta-x event) sensitivity))
      (setf (camera-pitch camera)
            (max -1.5 (min 1.5
                           (- (camera-pitch camera)
                              (* (canvas-pointer-event-delta-y event)
                                 sensitivity)))))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-focus-lost-event))
  (declare (ignore canvas))
  (clrhash (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  ;; Stop drawing; STOP-VIEWER releases the window from outside the event.
  (setf (viewer-running-p viewer) nil)
  nil)

(defmethod handle-canvas-event ((viewer viewer) canvas event)
  (declare (ignore viewer canvas event))
  nil)

(defun start-viewer (&key (scene (make-demo-scene))
                       (camera (make-fly-camera))
                       (title "luft atelier")
                       (width 1280) (height 800)
                       (frames-per-second 60)
                       (provider *gpu-provider*))
  "Open a window flying through SCENE and return the running VIEWER.

Click to capture the pointer, Escape to release it; WASD, Space, and C move.
The renderer stays available as (VIEWER-RENDERER *VIEWER*) for live tinkering."
  (let ((canvas (make-sdl-canvas
                 :title title :width width :height height :visible-p nil
                 :presentation-api (sdl-presentation-api-for provider)))
        (device nil)
        (renderer nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (let* ((device* (setf device
                               (request-gpu-device
                                provider
                                (make-device-descriptor :label title))))
                (context (make-canvas-context
                          canvas provider
                          (make-canvas-configuration :device device*)))
                (extent (canvas-extent context))
                (renderer* (setf renderer
                                 (make-renderer
                                  :scene scene :camera camera :device device*
                                  :width (first extent) :height (second extent)
                                  :color-format (canvas-format context))))
                (viewer (make-instance 'viewer :canvas canvas :context context
                                               :renderer renderer*)))
           (setf (canvas-event-handler canvas) viewer)
           (request-canvas-frame
            canvas (lambda (timestamp) (render-viewer-frame viewer timestamp)))
           (show-canvas canvas)
           (setf (canvas-clock canvas)
                 (make-cadence-clock
                  (lambda (native-canvas timestamp)
                    (declare (ignore native-canvas))
                    (render-viewer-frame viewer timestamp))
                  :frames-per-second frames-per-second))
           (setf completed-p t
                 *viewer* viewer)
           viewer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (close-canvas canvas)
        (when device (destroy device))))))

(defun stop-viewer (&optional (viewer *viewer*))
  "Close VIEWER's window and release its renderer and device."
  (when viewer
    (setf (viewer-running-p viewer) nil)
    (let* ((canvas (viewer-canvas viewer))
           (renderer (viewer-renderer viewer))
           (device (and renderer (renderer-device renderer))))
      (when (eq :open (canvas-state canvas))
        (setf (canvas-clock canvas) (make-demand-clock)))
      (when renderer
        (destroy-renderer renderer)
        (setf (viewer-renderer viewer) nil))
      (when (eq :open (canvas-state canvas))
        (close-canvas canvas))
      (when device
        (ignore-errors (destroy device))))
    (when (eq viewer *viewer*)
      (setf *viewer* nil)))
  (values))
