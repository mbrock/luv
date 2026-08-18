;;; The atelier: a small solid world, its surface chain, and the GPU objects
;;; that draw it.
;;;
;;; Nothing here meshes.  The world is a 3-chain of solid cells; REFRESH-SCENE
;;; takes its boundary, orders the resulting face sites by chunk, pads them to
;;; whole bricks, and measures a bounding sphere per brick.  The renderer
;;; uploads exactly those two arrays and one frame block, then dispatches one
;;; task workgroup per brick.

(in-package #:luft.render)

;;; ------------------------------------------------------------------------
;;; Scenes

(defconstant +brick-size+ shaders:+brick-size+)
(defconstant +chunk-bits+ 3
  "Sites are ordered by 8-cell chunk so a brick's faces stay close together.")

(defclass scene ()
  ((domain
    :initarg :domain
    :reader scene-domain)
   (solid
    :initarg :solid
    :reader scene-solid
    :documentation "The solid world: a 3-chain of positive cells.")
   (surface
    :initform nil
    :accessor scene-surface
    :documentation "The boundary of SOLID: exposed signed face sites.")
   (sites
    :initform nil
    :accessor scene-sites
    :documentation "Packed surface sites in brick order, zero-padded.")
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

(defun site-chunk-key (site)
  "A fixnum ordering key grouping signed site SITE by chunk, then by site."
  (logior (ash (ash (luft:site-z site) (- +chunk-bits+)) 45)
          (ash (ash (luft:site-y site) (- +chunk-bits+)) 24)
          (ash (luft:site-x site) (- +chunk-bits+))))

(defun order-sites-by-chunk (sites)
  "Return a fresh copy of the site-ordered SITES grouped by chunk."
  (stable-sort (copy-seq sites) #'< :key #'site-chunk-key))

(defun brick-spheres (sites brick-count)
  "Measure a bounding sphere for each brick of SITES as four floats each."
  (let ((spheres (make-array (* 4 brick-count) :element-type 'single-float
                                               :initial-element 0.0)))
    (dotimes (brick brick-count spheres)
      (let ((low-x nil) (low-y nil) (low-z nil)
            (high-x nil) (high-y nil) (high-z nil))
        (loop for index from (* brick +brick-size+)
                below (* (1+ brick) +brick-size+)
              for site = (aref sites index)
              unless (zerop site)
                do (let* ((x (luft:site-x site))
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
  "Recompute SCENE's surface chain, brick-ordered sites, and brick spheres."
  (let* ((surface (luft:surface-chain (scene-solid scene)))
         (ordered (order-sites-by-chunk (luft:chain-sites surface)))
         (brick-count (max 1 (ceiling (length ordered) +brick-size+)))
         (sites (make-array (* brick-count +brick-size+)
                            :element-type '(unsigned-byte 64)
                            :initial-element 0)))
    (replace sites ordered)
    (setf (scene-surface scene) surface
          (scene-sites scene) sites
          (scene-brick-count scene) brick-count
          (scene-bricks scene) (brick-spheres sites brick-count)
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

(defun fill-box (solid x0 x1 y0 y1 z0 z1 &optional (state t))
  "Set every cell of the closed box to STATE."
  (loop for x from x0 to x1
        do (loop for y from y0 to y1
                 do (loop for z from z0 to z1
                          do (setf (luft:solid-cell-p solid x y z) state)))))

(defun carve-ravine (solid)
  "A gap for the bridge to cross, cut where the ground was continuous."
  (loop for y from 40 to 56
        do (let ((width (+ 3 (floor (abs (- y 48)) 3))))
             (loop for x from (- 30 width) to (+ 30 width)
                   do (loop for z from 0 to 12
                            do (setf (luft:solid-cell-p solid x y z) nil))))))

(defun build-bridge (solid)
  "A deck across the ravine on two piers, with a parapet either side.

Something has to span a gap before a world has anywhere to stand and look
down from, and a deck one cell thick with a parapet at its edge is the
smallest thing that reads as built rather than as terrain."
  (let ((deck 9))
    ;; Piers down to whatever floor the ravine left.
    (dolist (x '(26 34))
      (fill-box solid x (1+ x) 44 45 0 (1- deck))
      (fill-box solid x (1+ x) 51 52 0 (1- deck)))
    ;; The deck, and a parapet along both sides with regular gaps.
    (fill-box solid 24 36 43 53 deck deck)
    (loop for x from 24 to 36
          unless (zerop (mod (- x 24) 4))
            do (setf (luft:solid-cell-p solid x 43 (+ deck 1)) t
                     (luft:solid-cell-p solid x 53 (+ deck 1)) t))
    ;; Ramps up to the deck at either end.
    (loop for step from 0 to 8
          do (fill-box solid (- 23 step) (- 23 step) 45 51
                       0 (max 0 (- deck 1 step)))
             (fill-box solid (+ 37 step) (+ 37 step) 45 51
                       0 (max 0 (- deck 1 step))))))

(defun build-balconies (solid)
  "Three balconies off the tower, each a slab with a lip and a doorway."
  (loop for (z side) in '((6 :east) (12 :north) (17 :east))
        do (ecase side
             (:east
              (fill-box solid 28 31 32 35 z z)
              (fill-box solid 31 31 32 35 (1+ z) (1+ z))
              (fill-box solid 28 31 32 32 (1+ z) (1+ z))
              (fill-box solid 28 31 35 35 (1+ z) (1+ z))
              ;; The doorway it is reached through.
              (fill-box solid 27 27 33 34 z (+ z 1) nil))
             (:north
              (fill-box solid 22 25 38 41 z z)
              (fill-box solid 22 25 41 41 (1+ z) (1+ z))
              (fill-box solid 22 22 38 41 (1+ z) (1+ z))
              (fill-box solid 25 25 38 41 (1+ z) (1+ z))
              (fill-box solid 23 24 37 37 z (+ z 1) nil)))))

(defun build-terraces (solid)
  "Stepped terraces below the tower: a hillside someone has taken in hand."
  (loop for step from 0 below 5
        for z = (+ 3 step)
        for near = (- 18 (* 2 step))
        do (fill-box solid near (+ near 1) (- 24 step) (+ 33 step) 0 z)
           ;; A low retaining wall along the front of each terrace.
           (loop for y from (- 24 step) to (+ 33 step)
                 unless (zerop (mod y 5))
                   do (setf (luft:solid-cell-p solid near y (1+ z)) t))))

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
    (carve-ravine solid)
    (build-bridge solid)
    (build-balconies solid)
    (build-terraces solid)
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
(defparameter *draw-sky* t
  "Whether the background is the gradient sky pass or the flat clear colour.")
(defparameter *focus-distance* 40.0
  "How far the lens is focused, in cells; also the alpha channel's scale.")
(defparameter *aperture* 0.0
  "How strongly the focus pass softens the distance; zero is a pinhole.")
(defparameter *fog-distance* 140.0)
(defparameter *bevel-radius* 0.22
  "The :BEVEL style's crease-rounding radius in cells, below one half.")
(defparameter *chamfer-width* 0.11
  "The :CHAMFER style's 45-degree crease relief in cells.

Wide enough that the planed facet reads as a face of its own and catches
the light as a band rather than a hairline, and still far short of the old
0.22-cell coves that made the world look carved.")
(defparameter *arris-softness* 0.004
  "The narrow shading transition where a chamfer meets its original face.")

(defun frame-uniform-data
    (camera width height &optional domain (surface-width *bevel-radius*))
  "Pack the frame block: camera, basis, projection, sun, sky, and domain lanes."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let* ((near *near-distance*)
           (far *far-distance*)
           (focal (/ (tan (/ (camera-field-of-view camera) 2.0))))
           (aspect (/ (coerce width 'single-float) height))
           (data (make-array 64 :element-type 'single-float))
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
               surface-width)
              *arris-softness*)
        (lane *sun-color* *sheen-strength*)
        (lane *fill-direction* *fill-strength*)
        (lane *ground-color* *exposure*)
        (lane (vec3:make-vec3 *occlusion-strength* *shadow-strength* 0.0) 0.0)
        (lane *top-color* 0.0)
        (lane *side-color* 0.0)
        (lane *bottom-color* 0.0)
        (lane (vec3:make-vec3 *focus-distance* *aperture*
                              (/ 1.0 (max 1 width)))
              (/ 1.0 (max 1 height))))
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
   (scene-texture :initform nil :accessor renderer-scene-texture)
   (scene-view :initform nil :accessor renderer-scene-view)
   (sampler :initform nil :accessor renderer-sampler)
   (lens-layout :initform nil :accessor renderer-lens-layout)
   (lens-bind-group :initform nil :accessor renderer-lens-bind-group)
   (uniform-buffer :initform nil :accessor renderer-uniform-buffer)
   (sites-buffer :initform nil :accessor renderer-sites-buffer)
   (bricks-buffer :initform nil :accessor renderer-bricks-buffer)
   (cells-buffer :initform nil :accessor renderer-cells-buffer)
   (sites-capacity :initform 0 :accessor renderer-sites-capacity)
   (bricks-capacity :initform 0 :accessor renderer-bricks-capacity)
   (cells-capacity :initform 0 :accessor renderer-cells-capacity)
   (layout :initform nil :accessor renderer-layout)
   (bind-group :initform nil :accessor renderer-bind-group)
   (modules :initform nil :accessor renderer-modules)
   (pipelines :initform nil :accessor renderer-pipelines
              :documentation "A plist from style to mesh pipeline.")
   (style :initarg :style :initform :bevel :accessor renderer-style
          :documentation
          "Which pipeline draws: :FLAT, :BEVEL (rounded), :CHAMFER, or :PAPER.")
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
                         :usage '(:render-attachment))))
         ;; The world is drawn here and read by the focus pass; COLOR is what
         ;; the focus pass writes and what a capture copies out.
         (scene (create device
                        (make-texture-descriptor
                         :label "luft scene color"
                         :size extent :dimensions :2d
                         :format (renderer-color-format renderer)
                         :usage '(:render-attachment :texture-binding)))))
    (setf (renderer-color-texture renderer) color
          (renderer-color-view renderer)
          (create device (make-texture-view-descriptor :texture color))
          (renderer-depth-texture renderer) depth
          (renderer-depth-view renderer)
          (create device (make-texture-view-descriptor :texture depth))
          (renderer-scene-texture renderer) scene
          (renderer-scene-view renderer)
          (create device (make-texture-view-descriptor :texture scene))
          (renderer-sampler renderer)
          (create device (make-sampler-descriptor
                          :label "luft scene sampler"
                          :mag-filter :linear :min-filter :linear)))))

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
         (paper-fragment
           (create device
                   (make-shader-module-descriptor
                    :label "luft paper fragment"
                    :language :mathematical
                    :code (shaders:paper-fragment-shader))))
         (sky-mesh (create device
                           (make-shader-module-descriptor
                            :label "luft sky mesh"
                            :language :mathematical
                            :code (shaders:sky-mesh-shader))))
         (sky-fragment
           (create device
                   (make-shader-module-descriptor
                    :label "luft sky fragment"
                    :language :mathematical
                    :code (shaders:sky-fragment-shader))))
         (lens-fragment
           (create device
                   (make-shader-module-descriptor
                    :label "luft lens fragment"
                    :language :mathematical
                    :code (shaders:lens-fragment-shader))))
         (lens-layout
           (create device
                   (make-bind-group-layout-descriptor
                    :label "luft lens layout"
                    :entries `((:binding ,shaders:+scene-binding+
                                :type :texture)
                               (:binding ,shaders:+sampler-binding+
                                :type :sampler)
                               (:binding ,shaders:+lens-frame-binding+
                                :type :uniform-buffer)))))
         (layout (create device
                         (make-bind-group-layout-descriptor
                          :label "luft surface layout"
                          :entries
                          `((:binding ,shaders:+frame-binding+
                             :type :uniform-buffer)
                            (:binding ,shaders:+sites-binding+
                             :type :storage-buffer)
                            (:binding ,shaders:+bricks-binding+
                             :type :storage-buffer)
                            (:binding ,shaders:+cells-binding+
                             :type :storage-buffer))))))
    (flet ((pipeline (label mesh-module fragment-module
                      &key (task-module task)
                           (group layout)
                           (depth '(:format :depth32-float
                                    :depth-write-enabled t
                                    :depth-compare :less)))
             (create device
                     (make-mesh-render-pipeline-descriptor
                      :label label
                      :layout group
                      :task (and task-module `(:module ,task-module))
                      :mesh `(:module ,mesh-module)
                      :fragment
                      `(:module ,fragment-module
                        :targets ((:format
                                   ,(renderer-color-format renderer))))
                      :max-mesh-workgroups 1
                      :depth-stencil depth))))
      (setf (renderer-modules renderer)
            (list task mesh bevel chamfer fragment chamfer-fragment
                  paper-fragment sky-mesh sky-fragment lens-fragment)
            (renderer-layout renderer) layout
            (renderer-lens-layout renderer) lens-layout
            (renderer-pipelines renderer)
            (list :flat (pipeline "luft surface pipeline" mesh fragment)
                  :bevel (pipeline "luft bevel pipeline" bevel fragment)
                  :chamfer (pipeline "luft chamfer pipeline"
                                     chamfer chamfer-fragment)
                  ;; The paper material draws the chamfered geometry: the
                  ;; glint it exists for lives on the planed facets.
                  :paper (pipeline "luft paper pipeline"
                                   chamfer paper-fragment)
                  ;; The background: no task stage to amplify, no depth to
                  ;; write, and it runs before anything that would hide it.
                  :sky (pipeline "luft sky pipeline" sky-mesh sky-fragment
                                 :task-module nil
                                 :depth '(:format :depth32-float
                                          :depth-write-enabled nil
                                          :depth-compare :always))
                  ;; The lens draws the frame the world was drawn into, so it
                  ;; binds a group of textures rather than the world's sites.
                  :lens (pipeline "luft lens pipeline" sky-mesh lens-fragment
                                  :task-module nil
                                  :group lens-layout
                                  :depth nil)))
      (setf (renderer-lens-bind-group renderer)
            (create device
                    (make-bind-group-descriptor
                     :label "luft lens bindings"
                     :layout lens-layout
                     :entries
                     `((:binding ,shaders:+scene-binding+
                        :resource ,(renderer-scene-view renderer))
                       (:binding ,shaders:+sampler-binding+
                        :resource ,(renderer-sampler renderer))
                       (:binding ,shaders:+lens-frame-binding+
                        :resource ,(renderer-uniform-buffer renderer)))))))))

(defun make-renderer (&key scene camera device
                        (provider *gpu-provider*)
                        (width 1280) (height 800)
                        (color-format :rgba8-unorm-srgb)
                        (style :bevel))
  "Create every GPU object needed to draw SCENE from CAMERA at WIDTH by HEIGHT.

STYLE is :FLAT, :BEVEL (rounded), :CHAMFER (subtle planar crease relief), or
:PAPER (the chamfered geometry in a matte, toothed material), and may be
changed later with (SETF RENDERER-STYLE).
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
  (dolist (resource (list (renderer-lens-bind-group renderer)
                          (renderer-lens-layout renderer)
                          (renderer-sampler renderer)
                          (renderer-scene-view renderer)
                          (renderer-scene-texture renderer)
                          (renderer-sites-buffer renderer)
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
        (renderer-sites-buffer renderer) nil
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
  "Upload SCENE's sites and brick spheres, rebinding when buffers grow."
  (let* ((sites (scene-sites scene))
         (bricks (scene-bricks scene))
         (rebind-p (null (renderer-bind-group renderer))))
    (multiple-value-bind (sites-buffer new-p)
        (ensure-storage-buffer renderer 'renderer-sites-buffer
                               'renderer-sites-capacity
                               (* 8 (length sites)) "luft surface sites")
      (when new-p (setf rebind-p t))
      (write-buffer sites-buffer sites))
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
                       (:binding ,shaders:+sites-binding+
                        :resource ,(renderer-sites-buffer renderer))
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
                                      (scene-domain scene)
                                      (if (member (renderer-style renderer)
                                                  '(:chamfer :paper))
                                          *chamfer-width*
                                          *bevel-radius*)))
    (let* ((lens-p (plusp *aperture*))
           ;; With a lens the world is drawn into the scene texture and the
           ;; focus pass writes the capture target; without one the world
           ;; draws straight into it and no frame is copied twice.
           (target (if lens-p
                       (renderer-scene-view renderer)
                       (renderer-color-view renderer)))
           (pass (begin-render-pass
                  encoder
                  (make-render-pass-descriptor
                   :label "luft surface pass"
                   :color-attachments
                   `((:view ,target
                      :load-op :clear :store-op :store
                      :clear-value ,(vector (vec3:vec3-x sky) (vec3:vec3-y sky)
                                            (vec3:vec3-z sky) 1.0)))
                   :depth-stencil-attachment
                   `(:view ,(renderer-depth-view renderer)
                     :depth-load-op :clear :depth-store-op :discard
                     :depth-clear-value 1.0)))))
      (when *draw-sky*
        (set-pipeline pass (renderer-pipeline renderer :sky))
        (set-bind-group pass 0 (renderer-bind-group renderer))
        (draw-mesh-workgroups pass 1))
      (set-pipeline pass (renderer-pipeline renderer))
      (set-bind-group pass 0 (renderer-bind-group renderer))
      (draw-mesh-workgroups pass (scene-brick-count scene))
      (end-pass pass)
      (when lens-p
        (prepare-texture encoder (renderer-scene-texture renderer)
                         :texture-binding)
        (let ((lens (begin-render-pass
                     encoder
                     (make-render-pass-descriptor
                      :label "luft lens pass"
                      :color-attachments
                      `((:view ,(renderer-color-view renderer)
                         :load-op :clear :store-op :store
                         :clear-value #(0.0 0.0 0.0 1.0)))))))
          (set-pipeline lens (renderer-pipeline renderer :lens))
          (set-bind-group lens 0 (renderer-lens-bind-group renderer))
          (draw-mesh-workgroups lens 1)
          (end-pass lens))))
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

(defparameter *srgb-to-linear*
  (let ((table (make-array 256 :element-type 'single-float)))
    (dotimes (index 256 table)
      (let ((value (/ (float index 1.0) 255.0)))
        (setf (aref table index)
              (if (<= value 0.04045)
                  (/ value 12.92)
                  (expt (/ (+ value 0.055) 1.055) 2.4))))))
  "One byte to its linear value: a 256-entry table beats a per-pixel EXPT.")

(defun linear-to-srgb-byte (value)
  (let ((clamped (min 1.0 (max 0.0 value))))
    (round (* 255.0
              (if (<= clamped 0.0031308)
                  (* 12.92 clamped)
                  (- (* 1.055 (expt clamped (/ 1.0 2.4))) 0.055))))))

(defun downsample-pixels (pixels width height factor &key (srgb-p t))
  "Average FACTOR by FACTOR blocks of PIXELS, in linear light.

Supersampling is the whole of the antialiasing here: there is no multisample
path, and averaging a rendered frame is the same thing one box filter later.
The average must be taken in linear light -- averaging sRGB bytes darkens
every edge, which is precisely where the eye is looking."
  (let* ((out-width (floor width factor))
         (out-height (floor height factor))
         (out (make-array (* 4 out-width out-height)
                          :element-type '(unsigned-byte 8)))
         (weight (/ 1.0 (* factor factor))))
    (dotimes (y out-height (values out out-width out-height))
      (dotimes (x out-width)
        (let ((red 0.0) (green 0.0) (blue 0.0) (alpha 0.0))
          (dotimes (dy factor)
            (dotimes (dx factor)
              (let ((offset (* 4 (+ (* (+ (* y factor) dy) width)
                                    (+ (* x factor) dx)))))
                (flet ((channel (index)
                         (let ((byte (aref pixels (+ offset index))))
                           (if srgb-p
                               (aref *srgb-to-linear* byte)
                               (/ (float byte 1.0) 255.0)))))
                  (incf red (channel 0))
                  (incf green (channel 1))
                  (incf blue (channel 2))
                  (incf alpha (/ (float (aref pixels (+ offset 3)) 1.0)
                                 255.0))))))
          (let ((offset (* 4 (+ (* y out-width) x))))
            (flet ((store (index value)
                     (setf (aref out (+ offset index))
                           (if srgb-p
                               (linear-to-srgb-byte (* value weight))
                               (round (* 255.0 (min 1.0 (* value weight))))))))
              (store 0 red)
              (store 1 green)
              (store 2 blue)
              (setf (aref out (+ offset 3))
                    (round (* 255.0 (min 1.0 (* alpha weight))))))))))))

(defun render-to-png (renderer pathname &key (downsample 1))
  "Render one frame headlessly and write it to PATHNAME as a PNG.

With DOWNSAMPLE above one the renderer is presumed to have been made that
many times oversize, and the frame is box-filtered down on the way out."
  (multiple-value-bind (pixels width height format) (render-pixels renderer)
    (ensure-directories-exist pathname)
    (if (> downsample 1)
        (multiple-value-bind (small small-width small-height)
            (downsample-pixels pixels width height downsample
                               :srgb-p (eq format :rgba8-unorm-srgb))
          (write-rgba-png pathname small small-width small-height format))
        (write-rgba-png pathname pixels width height format))))

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
