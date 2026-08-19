;;; McCLIM command surfaces inside luvcraft. The frame shares the world's
;;; native canvas and GPU device; its mirror retains painter-ordered semantic
;;; commands and dense buffers which the world or HUD pass replays directly.

(in-package #:mcluv)

(defclass luvcraft-widget-overlay (spinning-texture-compositor)
  ((session :initarg :session :reader widget-overlay-session)
   (frame :initarg :frame :reader widget-overlay-frame)
   (mirror :initarg :mirror :reader widget-overlay-mirror)
   ;; Where the widget is in the world.  Written once for a widget fixed to
   ;; a wall, and each frame for one carried in the hand.
   (center :initarg :center :initform nil :accessor widget-overlay-center)
   (right-axis :initarg :right-axis :initform nil
               :accessor widget-overlay-right-axis)
   (up-axis :initarg :up-axis :initform nil :accessor widget-overlay-up-axis)
   (normal-axis :initarg :normal-axis :initform nil
                :accessor widget-overlay-normal-axis)
   (height-scale :initarg :height-scale :initform 2.0
                 :reader widget-overlay-height-scale)
   (render-state :initform nil :accessor widget-overlay-render-state)
   (relief-layout :initform nil :accessor widget-overlay-relief-layout)
   (relief-vertex-module :initform nil
                         :accessor widget-overlay-relief-vertex-module)
   (relief-fragment-module :initform nil
                           :accessor widget-overlay-relief-fragment-module)
   (relief-pipeline :initform nil :accessor widget-overlay-relief-pipeline)))

(defclass luvcraft-direct-widget-overlay (luvcraft-widget-overlay)
  ((direct-device :initform nil :accessor direct-widget-device)
   (direct-target-format :initform nil :accessor direct-widget-target-format)
   (direct-depth-stencil :initform nil :accessor direct-widget-depth-stencil)
   (direct-shape-layout :initform nil :accessor direct-widget-shape-layout)
   (direct-image-layout :initform nil :accessor direct-widget-image-layout)
   (direct-image-sampler :initform nil :accessor direct-widget-image-sampler)
   (direct-pipelines :initform (make-hash-table :test #'eq)
                     :reader direct-widget-pipelines)
   (direct-resources :initform nil :accessor direct-widget-resources)
   (direct-frame-states :initform (make-hash-table :test #'eq)
                        :reader direct-widget-frame-states)
   (direct-image-bind-groups :initform (make-hash-table :test #'equal)
                             :reader direct-widget-image-bind-groups)
   (text-device :initform nil :accessor world-widget-text-device)
   (text-layout :initform nil :accessor world-widget-text-layout)
   (text-vertex-module :initform nil
                       :accessor world-widget-text-vertex-module)
   (text-fragment-module :initform nil
                         :accessor world-widget-text-fragment-module)
   (text-pipeline :initform nil :accessor world-widget-text-pipeline)
   (text-target-format :initform nil
                       :accessor world-widget-text-target-format)
   (text-depth-stencil :initform nil
                       :accessor world-widget-text-depth-stencil)
   (text-bind-groups
    :initform (make-hash-table :test #'equal)
    :reader world-widget-text-bind-groups))
  (:documentation
   "A McCLIM surface whose ordered semantic commands enter its final pass.

Embedded mirrors retain dense command buffers rather than a pane-sized raster
texture. Solids, analytic paint, images, and Slug text are evaluated at the
actual destination pixel."))

(defclass direct-widget-frame-state ()
  ((buffer :initarg :buffer :reader direct-widget-frame-buffer)
   (shape-bind-group :initarg :shape-bind-group
                     :reader direct-widget-frame-shape-bind-group)))

(defclass luvcraft-world-widget-overlay (luvcraft-direct-widget-overlay) ()
  (:documentation "A direct McCLIM surface mounted in the 3D scene."))

(defclass luvcraft-hud-widget-overlay (luvcraft-direct-widget-overlay) ()
  (:documentation "A direct McCLIM surface mounted in the presentation HUD."))

(eval-when (:load-toplevel :execute)
  ;; These method coordinates moved from the world-only class to the shared
  ;; direct compositor.  DEFMethod replaces one coordinate but cannot know
  ;; that an old coordinate has become obsolete in a durable image.
  (flet ((forget-method (name qualifiers specializer-names)
           (when (fboundp name)
             (let* ((generic (fdefinition name))
                    (specializers
                      (mapcar #'find-class specializer-names))
                    (method
                      (find-method generic qualifiers specializers nil)))
               (when method (remove-method generic method))))))
    (forget-method 'gpu-command-rasterized-p nil
                   '(luvcraft-world-widget-overlay
                     gpu-prepared-text-command))
    (forget-method 'gpu-command-rasterized-p nil
                   '(luvcraft-direct-widget-overlay
                     gpu-prepared-text-command))
    (forget-method 'release-raster-mirror-compositor '(:before)
                   '(luvcraft-world-widget-overlay))
    (forget-method 'luvcraft:encode-luvcraft-overlay '(:after)
                   '(luvcraft-world-widget-overlay t t t))))

(defmethod gpu-command-rasterized-p
    ((overlay luvcraft-direct-widget-overlay) command)
  (declare (ignore overlay command))
  nil)

(spv:define-shader world-widget-slug-vertex-specification
    (:stage :vertex
     :inputs ((position-alpha :vec3 :location 0)
              (outline-horizontal :vec3 :location 1)
              (atlas-vertical :vec3 :location 2)
              (band-low :vec3 :location 3)
              (band-high :vec3 :location 4)
              (color-input :vec3 :location 5))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-atlas-base :vec2 :location 1)
               (render-band-bounds :vec4 :location 2)
               (render-band-counts :vec2 :location 3)
               (render-color :vec4 :location 4)))
  (let* ((local-x (spv:swizzle position-alpha :x))
         (local-y (spv:swizzle position-alpha :y)))
    (spv:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (spv:set-output render-coordinate
                    (spv:swizzle outline-horizontal :xy))
    (spv:set-output render-atlas-base (spv:swizzle atlas-vertical :xy))
    (spv:set-output render-band-bounds
                    (spv:vec4 (spv:swizzle band-low :xy)
                              (spv:swizzle band-high :xy)))
    (spv:set-output render-band-counts
                    (spv:vec2 (spv:swizzle outline-horizontal :z)
                              (spv:swizzle atlas-vertical :z)))
    (spv:set-output render-color
                    (spv:vec4 color-input
                              (spv:swizzle position-alpha :z)))))

(spv:define-shader direct-widget-solid-vertex-specification
    (:stage :vertex
     :inputs ((position-opacity :vec3 :location 0)
              (color-input :vec3 :location 1))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (color-output :vec4 :location 0)))
  (let* ((local-x (spv:swizzle position-opacity :x))
         (local-y (spv:swizzle position-opacity :y))
         (alpha (spv:swizzle position-opacity :z)))
    (spv:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (spv:set-output color-output (spv:vec4 color-input alpha))))

(spv:define-shader direct-widget-analytic-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (local-coordinate :vec3 :location 1)
              (half-size-radius :vec3 :location 2)
              (color :vec3 :location 3))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-color :vec4 :location 2)))
  (let* ((local-x (spv:swizzle position :x))
         (local-y (spv:swizzle position :y))
         (alpha (spv:swizzle position :z)))
    (spv:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (spv:set-output render-coordinate (spv:swizzle local-coordinate :xy))
    (spv:set-output render-half-size-radius half-size-radius)
    (spv:set-output render-color (spv:vec4 (* color alpha) alpha))))

(spv:define-shader direct-widget-relief-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (local-coordinate :vec3 :location 1)
              (half-size-radius :vec3 :location 2)
              (color :vec3 :location 3)
              (relief :vec3 :location 4))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-color :vec4 :location 2)
               (render-height :float :location 3)))
  (let* ((local-x (spv:swizzle position :x))
         (local-y (spv:swizzle position :y))
         (alpha (spv:swizzle position :z)))
    (spv:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (spv:set-output render-coordinate (spv:swizzle local-coordinate :xy))
    (spv:set-output render-half-size-radius half-size-radius)
    (spv:set-output render-color (spv:vec4 (* color alpha) alpha))
    (spv:set-output render-height (spv:swizzle relief :x))))

(spv:define-shader direct-widget-gradient-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (local-coordinate :vec3 :location 1)
              (half-size-radius :vec3 :location 2)
              (paint-coordinate-kind :vec3 :location 3)
              (start-color :vec3 :location 4)
              (end-color :vec3 :location 5)
              (paint-alphas :vec3 :location 6))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-paint-coordinate-kind :vec3 :location 2)
               (render-start-color :vec3 :location 3)
               (render-end-color :vec3 :location 4)
               (render-paint-alphas :vec2 :location 5)))
  (let* ((local-x (spv:swizzle position :x))
         (local-y (spv:swizzle position :y)))
    (spv:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (spv:set-output render-coordinate (spv:swizzle local-coordinate :xy))
    (spv:set-output render-half-size-radius half-size-radius)
    (spv:set-output render-paint-coordinate-kind paint-coordinate-kind)
    (spv:set-output render-start-color start-color)
    (spv:set-output render-end-color end-color)
    (spv:set-output render-paint-alphas (spv:swizzle paint-alphas :xy))))

(spv:define-shader direct-widget-image-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (local-coordinate :vec3 :location 1)
              (half-size-radius :vec3 :location 2)
              (texture-coordinate-opacity :vec3 :location 3))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-texture-coordinate :vec2 :location 2)
               (render-opacity :float :location 3)))
  (let* ((local-x (spv:swizzle position :x))
         (local-y (spv:swizzle position :y)))
    (spv:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (spv:set-output render-coordinate (spv:swizzle local-coordinate :xy))
    (spv:set-output render-half-size-radius half-size-radius)
    (spv:set-output render-texture-coordinate
                    (spv:swizzle texture-coordinate-opacity :xy))
    (spv:set-output render-opacity
                    (spv:swizzle texture-coordinate-opacity :z))))

(defun clear-world-widget-text-resources (overlay)
  (dolist (table (list (world-widget-text-bind-groups overlay)
                       (direct-widget-image-bind-groups overlay)))
    (maphash (lambda (key group)
               (declare (ignore key))
               (luv:destroy group))
             table)
    (clrhash table))
  (maphash
   (lambda (surface state)
     (declare (ignore surface))
     (luv:destroy (direct-widget-frame-shape-bind-group state))
     (luv:destroy (direct-widget-frame-buffer state)))
   (direct-widget-frame-states overlay))
  (clrhash (direct-widget-frame-states overlay))
  (clrhash (world-widget-text-bind-groups overlay))
  (dolist (resource (direct-widget-resources overlay))
    (when resource (luv:destroy resource)))
  (clrhash (direct-widget-pipelines overlay))
  (setf (direct-widget-device overlay) nil
        (direct-widget-target-format overlay) nil
        (direct-widget-depth-stencil overlay) nil
        (direct-widget-shape-layout overlay) nil
        (direct-widget-image-layout overlay) nil
        (direct-widget-image-sampler overlay) nil
        (direct-widget-resources overlay) nil
        (world-widget-text-device overlay) nil
        (world-widget-text-layout overlay) nil
        (world-widget-text-pipeline overlay) nil
        (world-widget-text-fragment-module overlay) nil
        (world-widget-text-vertex-module overlay) nil
        (world-widget-text-target-format overlay) nil
        (world-widget-text-depth-stencil overlay) nil)
  overlay)

(defmethod release-raster-mirror-compositor :before
    ((overlay luvcraft-direct-widget-overlay))
  (clear-world-widget-text-resources overlay))

(defgeneric direct-widget-text-target-format
    (overlay session surface-texture)
  (:documentation "The actual color attachment format for direct text."))

(defmethod direct-widget-text-target-format
    ((overlay luvcraft-world-widget-overlay) session surface-texture)
  (declare (ignore overlay surface-texture))
  (luv:gpu-texture-format
   (luvcraft::luvcraft-session-color-texture session)))

(defmethod direct-widget-text-target-format
    ((overlay luvcraft-hud-widget-overlay) session surface-texture)
  (declare (ignore overlay session))
  (luv:gpu-texture-format surface-texture))

(defgeneric direct-widget-text-depth-stencil (overlay)
  (:documentation "The final pass depth declaration for direct text."))

(defmethod direct-widget-text-depth-stencil
    ((overlay luvcraft-world-widget-overlay))
  (declare (ignore overlay))
  '(:format :depth32-float
    :depth-write-enabled nil :depth-compare :less))

(defmethod direct-widget-text-depth-stencil
    ((overlay luvcraft-hud-widget-overlay))
  (declare (ignore overlay))
  nil)

(defun ensure-world-widget-text-pipeline (overlay target-format depth-stencil)
  (let ((device (mirror-device (widget-overlay-mirror overlay))))
    (unless (and (eq device (direct-widget-device overlay))
                 (eq target-format (direct-widget-target-format overlay))
                 (equal depth-stencil (direct-widget-depth-stencil overlay))
                 (gethash :text (direct-widget-pipelines overlay)))
      (clear-world-widget-text-resources overlay)
      (let ((created nil) (completed-p nil))
        (unwind-protect
             (labels
                 ((create (descriptor)
                    (let ((resource (luv:create device descriptor)))
                      (push resource created)
                      resource))
                  (make-pipeline
                      (name layout vertex-spec fragment-spec stride attributes)
                    (let* ((vertex
                             (create
                              (luv:make-shader-module-descriptor
                               :label (format nil "direct McCLIM ~A vertex" name)
                               :language :mathematical :code vertex-spec)))
                           (fragment
                             (create
                              (luv:make-shader-module-descriptor
                               :label (format nil "direct McCLIM ~A fragment" name)
                               :language :mathematical :code fragment-spec)))
                           (pipeline
                             (create
                              (luv:make-render-pipeline-descriptor
                               :label (format nil "direct McCLIM ~A" name)
                               :layout layout
                               :vertex
                               `(:module ,vertex
                                 ,@(when stride
                                     `(:buffers
                                       ((:array-stride ,stride
                                         :attributes ,attributes)))))
                               :fragment
                               `(:module ,fragment
                                 :targets
                                 ((:format ,target-format
                                   :blend :premultiplied-alpha)))
                               :depth-stencil depth-stencil
                               :primitive
                               (if stride
                                   '(:topology :triangle-list)
                                   '(:topology :triangle-strip))))))
                      pipeline)))
               (let* ((shape-layout
                        (create
                         (luv:make-bind-group-layout-descriptor
                          :label "direct McCLIM affine layout"
                          :entries '((:binding 2 :type :uniform-buffer)))))
                      (image-layout
                        (create
                         (luv:make-bind-group-layout-descriptor
                          :label "direct McCLIM image layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :sampler)
                                     (:binding 2 :type :uniform-buffer)))))
                      (text-layout
                        (create
                         (luv:make-bind-group-layout-descriptor
                          :label "direct McCLIM Slug layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :texture)
                                     (:binding 2 :type :uniform-buffer)))))
                      (sampler
                        (create
                         (luv:make-sampler-descriptor
                          :label "direct McCLIM image sampler"))))
                 (setf (direct-widget-shape-layout overlay) shape-layout
                       (direct-widget-image-layout overlay) image-layout
                       (world-widget-text-layout overlay) text-layout
                       (direct-widget-image-sampler overlay) sampler)
                 (flet ((install (key name layout vertex fragment stride attrs)
                          (setf (gethash key (direct-widget-pipelines overlay))
                                (make-pipeline name layout vertex fragment
                                               stride attrs))))
                   (install
                    :solid "solid" shape-layout
                    (direct-widget-solid-vertex-specification)
                    (spv:shader-specification-for :mcluv-solid :fragment)
                    24
                    '((:shader-location 0 :offset 0 :format :float32x3)
                      (:shader-location 1 :offset 12 :format :float32x3)))
                   (install
                    :analytic "analytic" shape-layout
                    (direct-widget-analytic-vertex-specification)
                    (luv.analytic:roundrect-fragment-specification)
                    48
                    '((:shader-location 0 :offset 0 :format :float32x3)
                      (:shader-location 1 :offset 12 :format :float32x3)
                      (:shader-location 2 :offset 24 :format :float32x3)
                      (:shader-location 3 :offset 36 :format :float32x3)))
                   (install
                    :relief "relief" shape-layout
                    (direct-widget-relief-vertex-specification)
                    (relief-roundrect-fragment-specification)
                    60
                    '((:shader-location 0 :offset 0 :format :float32x3)
                      (:shader-location 1 :offset 12 :format :float32x3)
                      (:shader-location 2 :offset 24 :format :float32x3)
                      (:shader-location 3 :offset 36 :format :float32x3)
                      (:shader-location 4 :offset 48 :format :float32x3)))
                   (install
                    :gradient "gradient" shape-layout
                    (direct-widget-gradient-vertex-specification)
                    (gradient-roundrect-fragment-specification)
                    84
                    '((:shader-location 0 :offset 0 :format :float32x3)
                      (:shader-location 1 :offset 12 :format :float32x3)
                      (:shader-location 2 :offset 24 :format :float32x3)
                      (:shader-location 3 :offset 36 :format :float32x3)
                      (:shader-location 4 :offset 48 :format :float32x3)
                      (:shader-location 5 :offset 60 :format :float32x3)
                      (:shader-location 6 :offset 72 :format :float32x3)))
                   (install
                    :image "image" image-layout
                    (direct-widget-image-vertex-specification)
                    (image-roundrect-fragment-specification)
                    48
                    '((:shader-location 0 :offset 0 :format :float32x3)
                      (:shader-location 1 :offset 12 :format :float32x3)
                      (:shader-location 2 :offset 24 :format :float32x3)
                      (:shader-location 3 :offset 36 :format :float32x3)))
                   (install
                    :text "Slug text" text-layout
                    (world-widget-slug-vertex-specification)
                    (luv.slug:slug-atlas-fragment-specification)
                    72
                    '((:shader-location 0 :offset 0 :format :float32x3)
                      (:shader-location 1 :offset 12 :format :float32x3)
                      (:shader-location 2 :offset 24 :format :float32x3)
                      (:shader-location 3 :offset 36 :format :float32x3)
                      (:shader-location 4 :offset 48 :format :float32x3)
                      (:shader-location 5 :offset 60 :format :float32x3)))
                   (install
                    :chassis "chassis" shape-layout
                    (lisp-machine-chassis-vertex-specification)
                    (lisp-machine-chassis-fragment-specification)
                    nil nil))
                 (setf (direct-widget-device overlay) device
                       (direct-widget-target-format overlay) target-format
                       (direct-widget-depth-stencil overlay) depth-stencil
                       (direct-widget-resources overlay) created
                       (world-widget-text-device overlay) device
                       (world-widget-text-target-format overlay) target-format
                       (world-widget-text-depth-stencil overlay) depth-stencil
                       (world-widget-text-pipeline overlay)
                       (gethash :text (direct-widget-pipelines overlay))
                       completed-p t)))
          (unless completed-p
            (dolist (resource created)
              (ignore-errors (luv:destroy resource))))))))
  overlay)

(defun ensure-direct-widget-frame-state (overlay surface-texture)
  (or (gethash surface-texture (direct-widget-frame-states overlay))
      (let ((buffer nil) (group nil) (completed-p nil))
        (unwind-protect
             (progn
               (setf buffer
                     (luv:create
                      (direct-widget-device overlay)
                      (luv:make-buffer-descriptor
                       :label "direct McCLIM affine state"
                       :size 64 :usage '(:uniform)))
                     group
                     (luv:create
                      (direct-widget-device overlay)
                      (luv:make-bind-group-descriptor
                       :label "direct McCLIM affine bindings"
                       :layout (direct-widget-shape-layout overlay)
                       :entries `((:binding 2 :resource ,buffer)))))
               (let ((state
                       (make-instance 'direct-widget-frame-state
                                      :buffer buffer :shape-bind-group group)))
                 (setf (gethash surface-texture
                                (direct-widget-frame-states overlay)) state
                       completed-p t)
                 state))
          (unless completed-p
            (when group (luv:destroy group))
            (when buffer (luv:destroy buffer)))))))

(defun ensure-direct-widget-image-bind-group
    (overlay paint surface-texture frame-state)
  (let ((key (list paint surface-texture)))
    (or (gethash key (direct-widget-image-bind-groups overlay))
        (setf (gethash key (direct-widget-image-bind-groups overlay))
              (luv:create
               (direct-widget-device overlay)
               (luv:make-bind-group-descriptor
                :label "direct McCLIM image bindings"
                :layout (direct-widget-image-layout overlay)
                :entries
                `((:binding 0 :resource ,(gpu-image-paint-view paint))
                  (:binding 1 :resource ,(direct-widget-image-sampler overlay))
                  (:binding 2
                   :resource ,(direct-widget-frame-buffer frame-state)))))))))

(defun ensure-world-widget-text-bind-group
    (overlay atlas surface-texture frame-state)
  (let ((key (list atlas surface-texture)))
    (or (gethash key (world-widget-text-bind-groups overlay))
        (setf (gethash key (world-widget-text-bind-groups overlay))
              (luv:create
               (direct-widget-device overlay)
               (luv:make-bind-group-descriptor
                :label "world McCLIM Slug bindings"
                :layout (world-widget-text-layout overlay)
                :entries
                `((:binding 0
                   :resource ,(luv.slug:slug-glyph-atlas-band-view atlas))
                  (:binding 1
                   :resource ,(luv.slug:slug-glyph-atlas-curve-view atlas))
                  (:binding 2
                   :resource ,(direct-widget-frame-buffer frame-state)))))))))

(spv:define-shader widget-relief-world-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (homogeneous-texture-coordinate :vec3 :location 1)
              (shade-padding :vec3 :location 2))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-texture-coordinate :vec2 :location 0)
               (render-shade :float :location 1)))
  (let* ()
    (spv:set-output
     clip-position
     (spv:vec4 position (spv:swizzle homogeneous-texture-coordinate :x)))
    (spv:set-output
     render-texture-coordinate
     (spv:swizzle homogeneous-texture-coordinate :yz))
    (spv:set-output render-shade
                    (spv:swizzle shade-padding :x))))

(spv:define-shader widget-relief-world-fragment-specification
    (:stage :fragment
     :inputs ((texture-coordinate :vec2 :location 0)
              (shade :float :location 1))
     :resources
     ((image :texture-2d :binding 0 :sample-transfer :identity)
      (texture-sampler :sampler :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((texel (spv:sample image texture-sampler texture-coordinate)))
    (spv:set-output
     color-output
     (spv:vec4 (* (spv:swizzle texel :rgb) shade)
               (spv:swizzle texel :a)))))

(defun clear-widget-overlay-relief-resources (overlay)
  (dolist (resource
            (list (widget-overlay-relief-pipeline overlay)
                  (widget-overlay-relief-fragment-module overlay)
                  (widget-overlay-relief-vertex-module overlay)))
    (when resource (luv:destroy resource)))
  (setf (widget-overlay-relief-layout overlay) nil
        (widget-overlay-relief-pipeline overlay) nil
        (widget-overlay-relief-fragment-module overlay) nil
        (widget-overlay-relief-vertex-module overlay) nil)
  overlay)

(defmethod release-raster-mirror-compositor :before
    ((overlay luvcraft-widget-overlay))
  (clear-widget-overlay-relief-resources overlay))

(defun ensure-widget-overlay-relief-pipeline (overlay format depth-format)
  (let ((layout (spinning-compositor-layout overlay)))
    (unless (and (eq layout (widget-overlay-relief-layout overlay))
                 (widget-overlay-relief-pipeline overlay))
      (clear-widget-overlay-relief-resources overlay)
      (let* ((device (spinning-compositor-device overlay))
             (vertex
               (luv:create
                device
                (luv:make-shader-module-descriptor
                 :label "McCLIM world relief vertex" :language :mathematical
                 :code (widget-relief-world-vertex-specification))))
             (fragment
               (luv:create
                device
                (luv:make-shader-module-descriptor
                 :label "McCLIM world relief fragment" :language :mathematical
                 :code (widget-relief-world-fragment-specification))))
             (pipeline
               (luv:create
                device
                (luv:make-render-pipeline-descriptor
                 :label "extruded McCLIM surface relief"
                 :layout layout
                 :vertex
                 `(:module ,vertex
                   :buffers
                   ((:array-stride 36
                     :attributes
                     ((:shader-location 0 :offset 0 :format :float32x3)
                      (:shader-location 1 :offset 12 :format :float32x3)
                      (:shader-location 2 :offset 24 :format :float32x3)))))
                 :fragment
                 `(:module ,fragment
                   :targets ((:format ,format)))
                 :depth-stencil
                 (when depth-format
                   `(:format ,depth-format
                     :depth-write-enabled t :depth-compare :less))
                 :primitive '(:topology :triangle-list)))))
        (setf (widget-overlay-relief-layout overlay) layout
              (widget-overlay-relief-vertex-module overlay) vertex
              (widget-overlay-relief-fragment-module overlay) fragment
              (widget-overlay-relief-pipeline overlay) pipeline))))
  overlay)

(defun ensure-widget-relief-frame-buffer (overlay frame-state byte-count)
  (when (> byte-count (spinning-frame-state-relief-capacity frame-state))
    (let* ((capacity (ash 1 (integer-length (max 1 (1- byte-count)))))
           (replacement
             (luv:create
              (spinning-compositor-device overlay)
              (luv:make-buffer-descriptor
               :label "extruded McCLIM relief vertices" :size capacity
               :usage '(:vertex :copy-dst)))))
      (alexandria:when-let
          ((old (spinning-frame-state-relief-buffer frame-state)))
        (luv:destroy old))
      (setf (spinning-frame-state-relief-buffer frame-state) replacement
            (spinning-frame-state-relief-capacity frame-state) capacity)))
  (spinning-frame-state-relief-buffer frame-state))

(defun place-widget-overlay-on-surface (overlay display session)
  "Put OVERLAY where DISPLAY's surface is this frame.

Asked at draw time, because the surface may be a phone in a moving hand;
for a wall the answer is the same every frame and costs a few vector ops."
  (multiple-value-bind (center right-axis up-axis normal-axis)
      (luvcraft:terminal-surface-panel-frame
       (luvcraft:terminal-display-surface display) session)
    (setf (widget-overlay-center overlay) center
          (widget-overlay-right-axis overlay) right-axis
          (widget-overlay-up-axis overlay) up-axis
          (widget-overlay-normal-axis overlay) normal-axis))
  overlay)

(defun world-device-clip-state (overlay session width height)
  "Return center, right, up, and normal clip vectors for OVERLAY's surface."
  (let* ((camera (luvcraft:luvcraft-session-camera session))
         (uniforms (luvcraft:camera-uniform-data camera width height))
         (camera-position (luvcraft:camera-position camera)))
    (flet ((lane-vector (offset)
             (vec:make-vec3
              (aref uniforms offset)
              (aref uniforms (+ offset 1))
              (aref uniforms (+ offset 2))))
           (difference (left right)
             (vec:make-vec3
              (- (vec:vec3-x left) (vec:vec3-x right))
              (- (vec:vec3-y left) (vec:vec3-y right))
              (- (vec:vec3-z left) (vec:vec3-z right)))))
      (let ((right (lane-vector 4))
            (up (lane-vector 8))
            (forward (lane-vector 12))
            (x-scale (aref uniforms 16))
            (y-scale (aref uniforms 17))
            (z-scale (aref uniforms 18))
            (z-offset (aref uniforms 19)))
        (labels ((clip-vector (vector point-p)
                   (let* ((relative
                            (if point-p
                                (difference vector camera-position)
                                vector))
                          (view-x (vec:vec3-dot relative right))
                          (view-y (vec:vec3-dot relative up))
                          (view-z (vec:vec3-dot relative forward)))
                     (list (* view-x x-scale)
                           (- (* view-y y-scale))
                           (+ (* view-z z-scale)
                              (if point-p z-offset 0.0))
                           view-z))))
          (make-array
           16 :element-type 'single-float
           :initial-contents
           (mapcar
            (lambda (value) (coerce value 'single-float))
            (append
              (clip-vector (widget-overlay-center overlay) t)
              (clip-vector (widget-overlay-right-axis overlay) nil)
              (clip-vector (widget-overlay-up-axis overlay) nil)
              (clip-vector (widget-overlay-normal-axis overlay) nil)))))))))

(defun widget-overlay-reliefs (overlay)
  (let ((sheet (mirror-sheet (widget-overlay-mirror overlay))))
    (loop for child in (gpu-sheet-paint-order sheet)
          for medium = (gpu-sheet-presentation-medium child)
          when (typep medium 'luv-raster-medium)
            append (coerce (raster-medium-reliefs medium) 'list))))

(defun surface-relief-perimeter (relief &optional (corner-segments 5))
  (let* ((left (surface-relief-x1 relief))
         (top (surface-relief-y1 relief))
         (right (surface-relief-x2 relief))
         (bottom (surface-relief-y2 relief))
         (radius
           (min (surface-relief-radius relief)
                (* 0.5 (- right left)) (* 0.5 (- bottom top))))
         points)
    (dolist (corner
              (list (list (- right radius) (+ top radius) (* -0.5 pi))
                    (list (- right radius) (- bottom radius) 0.0)
                    (list (+ left radius) (- bottom radius) (* 0.5 pi))
                    (list (+ left radius) (+ top radius) pi)))
      (destructuring-bind (center-x center-y start-angle) corner
        (dotimes (index corner-segments)
          (let ((angle
                  (+ start-angle
                     (* (* 0.5 pi) (/ index corner-segments)))))
            (push (list (+ center-x (* radius (cos angle)))
                        (+ center-y (* radius (sin angle))))
                  points)))))
    (nreverse points)))

(defun append-widget-relief-vertex
    (vertices state width height pixel-height x y shade)
  (let* ((u (/ x width))
         (v (/ y height))
         (local-x (- (* 2.0 u) 1.0))
         (local-y (- (* 2.0 v) 1.0)))
    (flet ((push-value (value)
             (vector-push-extend (coerce value 'single-float) vertices)))
      (let ((clip
              (loop for lane below 4
                    collect
                    (+ (aref state lane)
                       (* local-x (aref state (+ 4 lane)))
                       (* local-y (aref state (+ 8 lane)))
                       (* pixel-height (aref state (+ 12 lane)))))))
        (dolist (value (subseq clip 0 3)) (push-value value))
        (push-value (fourth clip)))
      (push-value u)
      (push-value v)
      (push-value shade)
      (push-value 0.0)
      (push-value 0.0))))

(defun widget-overlay-relief-vertices (overlay state width height)
  "Build the small world-space mesh derived from OVERLAY's relief commands."
  (let ((vertices
          (make-array 256 :element-type 'single-float
                          :adjustable t :fill-pointer 0))
        (pixel-world-scale
          (* (widget-overlay-height-scale overlay)
             (/ (* 2.0 (vec:vec3-length (widget-overlay-right-axis overlay)))
                width))))
    (dolist (relief (widget-overlay-reliefs overlay))
      ;; Negative ink height remains an analytical recess. Geometry drops to
      ;; the base plane until the compositor can cut a matching surface hole.
      (let* ((world-height
               (* pixel-world-scale (max 0.0 (surface-relief-height relief))))
             (points (surface-relief-perimeter relief))
             (center-x
               (* 0.5
                  (+ (surface-relief-x1 relief)
                     (surface-relief-x2 relief))))
             (center-y
               (* 0.5
                  (+ (surface-relief-y1 relief)
                     (surface-relief-y2 relief)))))
        (when (plusp world-height)
          (loop for tail on points
                for point = (first tail)
                for next = (or (second tail) (first points))
                do
                   ;; The top reuses the exact McCLIM texture, so text and
                   ;; other overpaint move with the physical control.
                   (append-widget-relief-vertex
                    vertices state width height world-height
                    center-x center-y 1.0)
                   (append-widget-relief-vertex
                    vertices state width height world-height
                    (first point) (second point) 1.0)
                   (append-widget-relief-vertex
                    vertices state width height world-height
                    (first next) (second next) 1.0)
                   ;; Two side-wall triangles, shaded independently of the
                   ;; analytical top face.
                   (dolist (vertex
                             (list (list point 0.0) (list next 0.0)
                                   (list next world-height)
                                   (list point 0.0) (list next world-height)
                                   (list point world-height)))
                     (append-widget-relief-vertex
                      vertices state width height (second vertex)
                      (first (first vertex)) (second (first vertex)) 0.56))))))
    vertices))

(defmethod present-raster-mirror-texture
    ((mirror luv-raster-mirror) context texture
     (overlay luvcraft-widget-overlay))
  (declare (ignore mirror context texture overlay))
  ;; Upload is complete.  Luvcraft will sample this texture in its next frame.
  nil)

(defun widget-overlay-logical-size (overlay)
  "Return OVERLAY's McCLIM surface size without requiring a backing texture."
  (multiple-value-list
   (gpu-mirror-logical-size (widget-overlay-mirror overlay))))

(luv:zdefun (prepare-direct-widget-overlay :zone :mcluv/prepare-overlay)
    (overlay session surface-texture state)
  "Publish STATE and return the destination frame's affine resources."
  (let ((target-format
          (direct-widget-text-target-format overlay session surface-texture))
        (depth-stencil (direct-widget-text-depth-stencil overlay)))
    (ensure-world-widget-text-pipeline overlay target-format depth-stencil)
    (let ((frame-state
            (ensure-direct-widget-frame-state overlay surface-texture)))
      (setf (widget-overlay-render-state overlay) state)
      (luv:write-buffer (direct-widget-frame-buffer frame-state) state)
      frame-state)))

(luv:zdefmethod (luvcraft:encode-luvcraft-overlay :zone :mcluv/encode-chassis)
    ((overlay luvcraft-widget-overlay) session pass surface-texture)
  (let ((mirror (widget-overlay-mirror overlay)))
    (when (gpu-mirror-prepared-commands mirror)
      (let* ((viewport-size
               (luv:canvas-extent (luvcraft::luvcraft-session-context session)))
             (state
               (world-device-clip-state
                overlay session (first viewport-size) (second viewport-size)))
             (frame-state
               (prepare-direct-widget-overlay
                overlay session surface-texture state)))
        ;; The physical housing is game geometry; only the CLIM screen itself
        ;; comes from the retained semantic stream.
        (luv:set-pipeline pass (gethash :chassis
                                        (direct-widget-pipelines overlay)))
        (luv:set-bind-group
         pass 0 (direct-widget-frame-shape-bind-group frame-state))
        (dotimes (layer 3)
          (luv:draw pass 4 1 (* layer 4))))))
  overlay)

(defun projected-screen-vertex (state width height u v)
  (let* ((center-x (- (* 2.0 u) 1.0))
         (center-y (- (* 2.0 v) 1.0))
         (clip-x (+ (aref state 0)
                    (* center-x (aref state 4))
                    (* center-y (aref state 8))))
         (clip-y (+ (aref state 1)
                    (* center-x (aref state 5))
                    (* center-y (aref state 9))))
         (clip-w (+ (aref state 3)
                    (* center-x (aref state 7))
                    (* center-y (aref state 11)))))
    (list (* width 0.5 (+ 1.0 (/ clip-x clip-w)))
          (* height 0.5 (+ 1.0 (/ clip-y clip-w)))
          clip-w u v)))

(defun set-world-widget-text-scissor
    (pass overlay mirror surface-texture clip)
  "Set a conservative framebuffer scissor for one pane-local CLIP."
  (destructuring-bind (surface-width surface-height &rest ignored)
      (luv:gpu-texture-size surface-texture)
    (declare (ignore ignored))
    (multiple-value-bind (logical-width logical-height)
        (gpu-mirror-logical-size mirror)
      (destructuring-bind (left top right bottom)
          (or clip (list 0 0 logical-width logical-height))
        (let* ((state (widget-overlay-render-state overlay))
               (corners
                 (mapcar
                  (lambda (point)
                    (projected-screen-vertex
                     state surface-width surface-height
                     (/ (first point) logical-width)
                     (/ (second point) logical-height)))
                  (list (list left top) (list right top)
                        (list left bottom) (list right bottom)))))
          ;; Crossing the camera plane makes the projected AABB unbounded.
          ;; The full target is conservative and this case is rare for a
          ;; focusable surface in front of the player.
          (if (some (lambda (corner) (not (plusp (third corner)))) corners)
              (progn
                (luv:set-scissor-rect
                 pass 0 0 surface-width surface-height)
                t)
              (let* ((x (max 0 (min surface-width
                                    (floor (reduce #'min corners :key #'first)))))
                     (y (max 0 (min surface-height
                                    (floor (reduce #'min corners :key #'second)))))
                     (right
                       (max x (min surface-width
                                   (ceiling
                                    (reduce #'max corners :key #'first)))))
                     (bottom
                       (max y (min surface-height
                                   (ceiling
                                    (reduce #'max corners :key #'second)))))
                     (width (- right x))
                     (height (- bottom y)))
                (when (and (plusp width) (plusp height))
                  (luv:set-scissor-rect pass x y width height)
                  t))))))))

(defmethod luvcraft:encode-luvcraft-overlay :before
    ((overlay luvcraft-direct-widget-overlay) session pass surface-texture)
  (declare (ignore session pass surface-texture))
  ;; Primary methods set this only when they actually draw their backing
  ;; layer.  A hidden inventory or fully retracted metabar must not replay the
  ;; previous visible frame's text.
  (setf (widget-overlay-render-state overlay) nil))

(luv:zdefmethod (luvcraft:encode-luvcraft-overlay
                 :zone :mcluv/encode-commands
                 :value
                 (length
                  (gpu-mirror-prepared-commands
                   (widget-overlay-mirror overlay))))
    :after
    ((overlay luvcraft-direct-widget-overlay) session pass surface-texture)
  "Replay OVERLAY's retained McCLIM commands in exact painter order."
  (let* ((mirror (widget-overlay-mirror overlay))
         (commands (gpu-mirror-prepared-commands mirror))
         (source-state (gpu-mirror-prepared-frame-state mirror)))
    (when (and commands source-state (widget-overlay-render-state overlay))
      (let* ((frame-state
               (ensure-direct-widget-frame-state overlay surface-texture))
             (active-clip (list :unset))
             (clip-visible-p t))
        (dolist (command commands)
          (let ((clip (gpu-frame-command-clip command)))
            (unless (equal clip active-clip)
              (setf active-clip clip
                    clip-visible-p
                    (set-world-widget-text-scissor
                     pass overlay mirror surface-texture clip))))
          (when clip-visible-p
            (multiple-value-bind
                  (pipeline buffer bind-group first-vertex vertex-count)
                (etypecase command
                  (gpu-solid-command
                   (values
                    (gethash :solid (direct-widget-pipelines overlay))
                    (gpu-frame-state-vertex-buffer source-state)
                    (direct-widget-frame-shape-bind-group frame-state)
                    (gpu-solid-command-first-vertex command)
                    (gpu-solid-command-vertex-count command)))
                  (gpu-analytic-command
                   (values
                    (gethash :analytic (direct-widget-pipelines overlay))
                    (gpu-frame-state-analytic-buffer source-state)
                    (direct-widget-frame-shape-bind-group frame-state)
                    (gpu-analytic-command-first-vertex command)
                    (gpu-analytic-command-vertex-count command)))
                  (gpu-relief-analytic-command
                   (values
                    (gethash :relief (direct-widget-pipelines overlay))
                    (gpu-frame-state-relief-buffer source-state)
                    (direct-widget-frame-shape-bind-group frame-state)
                    (gpu-relief-analytic-command-first-vertex command)
                    (gpu-relief-analytic-command-vertex-count command)))
                  (gpu-gradient-analytic-command
                   (values
                    (gethash :gradient (direct-widget-pipelines overlay))
                    (gpu-frame-state-gradient-buffer source-state)
                    (direct-widget-frame-shape-bind-group frame-state)
                    (gpu-gradient-analytic-command-first-vertex command)
                    (gpu-gradient-analytic-command-vertex-count command)))
                  (gpu-prepared-image-command
                   (values
                    (gethash :image (direct-widget-pipelines overlay))
                    (gpu-frame-state-image-buffer source-state)
                    (ensure-direct-widget-image-bind-group
                     overlay (gpu-prepared-image-command-paint command)
                     surface-texture frame-state)
                    (gpu-prepared-image-command-first-vertex command)
                    (gpu-prepared-image-command-vertex-count command)))
                  (gpu-prepared-text-command
                   (values
                    (gethash :text (direct-widget-pipelines overlay))
                    (gpu-frame-state-text-buffer source-state)
                    (ensure-world-widget-text-bind-group
                     overlay (gpu-prepared-text-command-atlas command)
                     surface-texture frame-state)
                    (gpu-prepared-text-command-first-vertex command)
                    (gpu-prepared-text-command-vertex-count command))))
              (luv:set-pipeline pass pipeline)
              (luv:set-bind-group pass 0 bind-group)
              (luv:set-vertex-buffer pass 0 buffer)
              (luv:draw pass vertex-count 1 first-vertex))))
        ;; Overlay encoding continues in the same scene pass.
        (destructuring-bind (width height &rest ignored)
            (luv:gpu-texture-size surface-texture)
          (declare (ignore ignored))
          (luv:set-scissor-rect pass 0 0 width height)))))
  overlay)

(defun barycentric-coordinates (x y a b c)
  (destructuring-bind (ax ay &rest ignored-a) a
    (declare (ignore ignored-a))
    (destructuring-bind (bx by &rest ignored-b) b
      (declare (ignore ignored-b))
      (destructuring-bind (cx cy &rest ignored-c) c
        (declare (ignore ignored-c))
        (let ((denominator
                (+ (* (- by cy) (- ax cx))
                   (* (- cx bx) (- ay cy)))))
          (unless (zerop denominator)
            (let* ((wa
                     (/ (+ (* (- by cy) (- x cx))
                           (* (- cx bx) (- y cy)))
                        denominator))
                   (wb
                     (/ (+ (* (- cy ay) (- x cx))
                           (* (- ax cx) (- y cy)))
                        denominator))
                   (wc (- 1.0 wa wb)))
              (when (and (>= wa 0.0) (>= wb 0.0) (>= wc 0.0))
                (list wa wb wc)))))))))

(defun perspective-texture-coordinate (weights vertices)
  (let ((normalizer 0.0)
        (u 0.0)
        (v 0.0))
    (loop for weight in weights
          for vertex in vertices
          for reciprocal-w = (/ 1.0 (third vertex))
          for corrected = (* weight reciprocal-w)
          do (incf normalizer corrected)
             (incf u (* corrected (fourth vertex)))
             (incf v (* corrected (fifth vertex))))
    (values (/ u normalizer) (/ v normalizer))))

(defun luvcraft-widget-texture-coordinate (overlay x y)
  "Project canvas X,Y through OVERLAY's last rendered screen quadrilateral."
  (let ((state (widget-overlay-render-state overlay)))
    (when (and state
               (loop for u in '(0.0 1.0 0.0 1.0)
                     for v in '(0.0 0.0 1.0 1.0)
                     always (plusp
                             (third
                              (projected-screen-vertex state 1 1 u v)))))
      ;; Project into the window's own coordinates rather than the drawable's.
      ;; A pointer event carries the position SDL reports, which is in logical
      ;; points; on a dense display the drawable is a multiple of that, and
      ;; hit-testing a point against a quad projected into pixels misses by
      ;; exactly the display's scale factor -- which is why no widget overlay
      ;; could be clicked at all on a Retina Mac.
      (let* ((canvas (luvcraft::luvcraft-session-canvas
                      (widget-overlay-session overlay)))
             (width (luv:canvas-width canvas))
             (height (luv:canvas-height canvas)))
        (let* ((top-left
                 (projected-screen-vertex state width height 0.0 0.0))
               (top-right
                 (projected-screen-vertex state width height 1.0 0.0))
               (bottom-left
                 (projected-screen-vertex state width height 0.0 1.0))
               (bottom-right
                 (projected-screen-vertex state width height 1.0 1.0))
               (first-triangle
                 (list top-left top-right bottom-left))
               (second-triangle
                 (list bottom-left top-right bottom-right)))
          (or (alexandria:when-let
                  ((weights
                     (barycentric-coordinates
                      x y top-left top-right bottom-left)))
                (multiple-value-list
                 (perspective-texture-coordinate
                  weights first-triangle)))
              (alexandria:when-let
                  ((weights
                     (barycentric-coordinates
                      x y bottom-left top-right bottom-right)))
                (multiple-value-list
                 (perspective-texture-coordinate
                  weights second-triangle)))))))))

(defun translated-widget-pointer-event (event x y)
  (etypecase event
    (luv:canvas-pointer-motion-event
     (make-instance
      (class-of event)
      :timestamp (luv:canvas-event-timestamp event) :x x :y y
      :delta-x 0.0 :delta-y 0.0))
    (luv:canvas-pointer-button-event
     (make-instance
      (class-of event)
      :timestamp (luv:canvas-event-timestamp event) :x x :y y
      :button (luv:canvas-pointer-event-button event)
      :clicks (luv:canvas-pointer-event-clicks event)))))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-widget-overlay) session canvas
     (event luv:canvas-pointer-event))
  (alexandria:when-let
      ((uv (luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (let* ((size (widget-overlay-logical-size overlay))
           (x (* (first uv) (first size)))
           (y (* (second uv) (second size))))
      (when (and (typep event 'luv:canvas-pointer-button-press-event)
                 (eq :left (luv:canvas-pointer-event-button event)))
        (luvcraft:focus-luvcraft-session session overlay))
      (luv:handle-canvas-event
       (widget-overlay-mirror overlay) canvas
       (translated-widget-pointer-event event x y))
      t)))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-widget-overlay) session canvas
     (event luv:canvas-pointer-event))
  (luvcraft:handle-luvcraft-overlay-event overlay session canvas event))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-widget-overlay) session canvas
     (event luv:canvas-event))
  (declare (ignore session))
  (luv:handle-canvas-event (widget-overlay-mirror overlay) canvas event)
  t)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-widget-overlay) session)
  (declare (ignore session))
  (destructuring-bind (width height)
      (luv:canvas-extent
       (luvcraft::luvcraft-session-context
        (widget-overlay-session overlay)))
    (when (luvcraft-widget-texture-coordinate
           overlay (/ width 2.0) (/ height 2.0))
      0.0)))

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay luvcraft-widget-overlay))
  (let ((frame (widget-overlay-frame overlay)))
    (unless (eq :disowned (frame-state frame))
      (destroy-frame frame)))
  overlay)

(defun add-scaled-vector (origin &rest vector-scales)
  (let ((x (vec:vec3-x origin))
        (y (vec:vec3-y origin))
        (z (vec:vec3-z origin)))
    (loop for (vector scale) on vector-scales by #'cddr
          do (incf x (* (vec:vec3-x vector) scale))
             (incf y (* (vec:vec3-y vector) scale))
             (incf z (* (vec:vec3-z vector) scale)))
    (vec:make-vec3 x y z)))

(defun embed-luvcraft-frame
    (session frame &key (distance 4.0) (width 1.8) (right-offset 2.8))
  "Embed enabled McCLIM FRAME in front of SESSION's current camera."
  (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
         (source-size (multiple-value-list (gpu-mirror-logical-size mirror)))
         (aspect (/ (first source-size) (second source-size)))
         (camera (luvcraft:luvcraft-session-camera session))
         (camera-position (luvcraft:camera-position camera)))
    (multiple-value-bind (right ignored-up forward)
        (luvcraft:camera-basis camera)
      (declare (ignore ignored-up))
      (let ((overlay
              (make-instance
               'luvcraft-world-widget-overlay
               :session session :frame frame :mirror mirror
               :center
               (add-scaled-vector camera-position
                                  forward distance right right-offset)
               :right-axis (vec:vec3-scale right (/ width 2.0))
               :up-axis
               (vec:make-vec3 0.0 (- (/ width aspect 2.0)) 0.0)
               :normal-axis (vec:vec3-scale forward -1.0))))
        (setf (mirror-compositor mirror) overlay)
        ;; The frame was enabled before it acquired its world compositor.
        ;; Repaint once so text is retained for the scene pass but omitted
        ;; from the backing texture.
        (repaint-gpu-mirror mirror)
        (luvcraft:add-luvcraft-overlay session overlay)
        overlay))))

(defun open-luvcraft-widget-lab
    (session &key (title "McCLIM gadget inside luvcraft")
                  (distance 4.0) (width 1.8) (right-offset 2.8))
  "Create a McCLIM terminal fixed in front of SESSION's current world camera."
  (embed-luvcraft-frame
   session
   (open-widget-lab
    :title title
    :target (luvcraft:luvcraft-session-canvas session)
    :context (luvcraft::luvcraft-session-context session)
    :device (luvcraft::luvcraft-session-device session))
   :distance distance :width width :right-offset right-offset))

(defun open-luvcraft-surveyor-map
    (session &key (title "surveyor map")
                  (distance 3.2) (width 3.2) (right-offset 0.0))
  "Create and embed a live-world McCLIM surveyor map for SESSION."
  (embed-luvcraft-frame
   session
   (open-surveyor-map
    session :title title
    :target (luvcraft:luvcraft-session-canvas session)
    :context (luvcraft::luvcraft-session-context session)
    :device (luvcraft::luvcraft-session-device session))
   :distance distance :width width :right-offset right-offset))

(defun close-luvcraft-widget-lab (overlay)
  "Remove and release an OPEN-LUVCRAFT-WIDGET-LAB overlay."
  (check-type overlay luvcraft-widget-overlay)
  (luvcraft:remove-luvcraft-overlay
   (widget-overlay-session overlay) overlay)
  nil)

(defun close-luvcraft-surveyor-map (overlay)
  "Remove and release an OPEN-LUVCRAFT-SURVEYOR-MAP overlay."
  (close-luvcraft-widget-lab overlay))
