;;; Application-neutral retained McCLIM commands composed directly into a
;;; caller-owned GPU render pass.  The implementation lives here so games can
;;; share semantic, target-resolution UI without depending on one another.

(in-package #:mcluv)

(defclass direct-gpu-mirror-compositor ()
  ((mirror :initarg :mirror :reader widget-overlay-mirror)
   (render-state :initform nil :accessor widget-overlay-render-state)
   (direct-device :initform nil :accessor direct-widget-device)
   (direct-target-format :initform nil :accessor direct-widget-target-format)
   (direct-depth-stencil :initform nil :accessor direct-widget-depth-stencil)
   (direct-shape-layout :initform nil :accessor direct-widget-shape-layout)
   (direct-image-layout :initform nil :accessor direct-widget-image-layout)
   (direct-image-sampler :initform nil :accessor direct-widget-image-sampler)
   (direct-pipelines :initform (make-hash-table :test #'eq)
                     :reader direct-widget-pipelines)
   (direct-pipeline-families :initform (make-hash-table :test #'eq)
                             :reader direct-mirror-pipeline-families)
   (direct-resources :initform nil :accessor direct-widget-resources)
   (direct-frame-states :initform (make-hash-table :test #'equal)
                        :reader direct-widget-frame-states)
   (direct-image-bind-groups :initform (make-hash-table :test #'equal)
                             :reader direct-widget-image-bind-groups)
   (direct-lattice-layout :initform nil
                          :accessor direct-widget-lattice-layout)
   (direct-lattice-bind-groups :initform (make-hash-table :test #'equal)
                               :reader direct-widget-lattice-bind-groups)
   (text-layout :initform nil :accessor direct-widget-text-layout)
   (text-bind-groups
    :initform (make-hash-table :test #'equal)
    :reader direct-widget-text-bind-groups)
   (solid-fragment-dependent
    :initform nil :accessor direct-mirror-solid-fragment-dependent))
  (:documentation
   "A retained McCLIM command stream replayed into a caller-owned GPU pass.

The caller supplies one 4x4 clip-space affine state and the actual destination
texture for each frame. Semantic solids, analytic shapes, gradients, images,
lattices, and Slug text are evaluated at that texture's native resolution;
there is no pane raster, upload, sampled copy, drawable acquisition, or queue
submission in this compositor. Per-drawable uniform resources are keyed by
LUV:CANVAS-FRAME-RESOURCE-KEY so multiple frames may remain in flight without
an unbounded cache of backend wrapper objects."))


(defclass direct-widget-frame-state ()
  ((buffer :initarg :buffer :reader direct-widget-frame-buffer)
   (shape-bind-group :initarg :shape-bind-group
                     :reader direct-widget-frame-shape-bind-group)
   (source-state :initarg :source-state
                 :reader direct-widget-frame-source-state)
   (prepared-revision :initform nil
                      :accessor direct-widget-frame-prepared-revision)))

(defstruct direct-mirror-pipeline-family
  pipeline resources source-revision solid-fragment-revision)

(define-condition direct-mirror-pipelines-not-prepared (error)
  ((families :initarg :families :reader direct-mirror-missing-families)
   (format :initarg :format :reader direct-mirror-missing-target-format)
   (depth-stencil :initarg :depth-stencil
                  :reader direct-mirror-missing-depth-stencil))
  (:report
   (lambda (condition stream)
     (format stream
             "Direct McCLIM pipeline~P ~{~S~^, ~} ~
must be prepared before the application render pass opens (format ~S, depth ~S)."
             (length (direct-mirror-missing-families condition))
             (direct-mirror-missing-families condition)
             (direct-mirror-missing-target-format condition)
             (direct-mirror-missing-depth-stencil condition)))))

(define-condition direct-mirror-release-error (error)
  ((failures :initarg :failures :reader direct-mirror-release-failures))
  (:report
   (lambda (condition stream)
     (let ((failures (direct-mirror-release-failures condition)))
       (format stream "~D direct McCLIM resource~:P failed to release"
               (length failures))
       (when failures
         (format stream "; first failure for ~S: ~A"
                 (caar failures) (cdar failures)))
       (write-char #\. stream)))))

(defstruct direct-widget-command-encode-context
  "Resources borrowed together for one direct-widget command replay."
  overlay destination-state source-state)


(defmethod gpu-command-rasterized-p
    ((compositor direct-gpu-mirror-compositor) command)
  (declare (ignore compositor command))
  nil)

;; Solid and Slug CPU vertices already contain RGB * authored alpha.  These
;; stages forward that premultiplied RGB exactly once; coverage is subsequently
;; applied to the complete RGBA value by the fragment stage.

(shader:define-shader direct-mirror-slug-vertex-specification
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
  (let* ((local-x (shader:swizzle position-alpha :x))
         (local-y (shader:swizzle position-alpha :y)))
    (shader:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (shader:set-output render-coordinate
                    (shader:swizzle outline-horizontal :xy))
    (shader:set-output render-atlas-base (shader:swizzle atlas-vertical :xy))
    (shader:set-output render-band-bounds
                    (shader:vec4 (shader:swizzle band-low :xy)
                              (shader:swizzle band-high :xy)))
    (shader:set-output render-band-counts
                    (shader:vec2 (shader:swizzle outline-horizontal :z)
                              (shader:swizzle atlas-vertical :z)))
    (shader:set-output render-color
                    (shader:vec4 color-input
                              (shader:swizzle position-alpha :z)))))

(shader:define-shader direct-widget-solid-vertex-specification
    (:stage :vertex
     :inputs ((position-opacity :vec3 :location 0)
              (color-input :vec3 :location 1))
     :resources
     ((state :uniform-block :set 0 :binding 2
             :members ((center :vec4) (right :vec4) (up :vec4))))
     :outputs ((clip-position :vec4 :built-in :position)
               (color-output :vec4 :location 0)))
  (let* ((local-x (shader:swizzle position-opacity :x))
         (local-y (shader:swizzle position-opacity :y))
         (alpha (shader:swizzle position-opacity :z)))
    (shader:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (shader:set-output color-output (shader:vec4 color-input alpha))))

(shader:define-shader direct-widget-analytic-vertex-specification
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
  (let* ((local-x (shader:swizzle position :x))
         (local-y (shader:swizzle position :y))
         (alpha (shader:swizzle position :z)))
    (shader:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (shader:set-output render-coordinate (shader:swizzle local-coordinate :xy))
    (shader:set-output render-half-size-radius half-size-radius)
    (shader:set-output render-color (shader:vec4 (* color alpha) alpha))))

(shader:define-shader direct-widget-relief-vertex-specification
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
  (let* ((local-x (shader:swizzle position :x))
         (local-y (shader:swizzle position :y))
         (alpha (shader:swizzle position :z)))
    (shader:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (shader:set-output render-coordinate (shader:swizzle local-coordinate :xy))
    (shader:set-output render-half-size-radius half-size-radius)
    (shader:set-output render-color (shader:vec4 (* color alpha) alpha))
    (shader:set-output render-height (shader:swizzle relief :x))))

(shader:define-shader direct-widget-gradient-vertex-specification
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
  (let* ((local-x (shader:swizzle position :x))
         (local-y (shader:swizzle position :y)))
    (shader:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (shader:set-output render-coordinate (shader:swizzle local-coordinate :xy))
    (shader:set-output render-half-size-radius half-size-radius)
    (shader:set-output render-paint-coordinate-kind paint-coordinate-kind)
    (shader:set-output render-start-color start-color)
    (shader:set-output render-end-color end-color)
    (shader:set-output render-paint-alphas (shader:swizzle paint-alphas :xy))))

(shader:define-shader direct-widget-image-vertex-specification
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
  (let* ((local-x (shader:swizzle position :x))
         (local-y (shader:swizzle position :y)))
    (shader:set-output clip-position
                    (+ center (* right local-x) (* up local-y)))
    (shader:set-output render-coordinate (shader:swizzle local-coordinate :xy))
    (shader:set-output render-half-size-radius half-size-radius)
    (shader:set-output render-texture-coordinate
                    (shader:swizzle texture-coordinate-opacity :xy))
    (shader:set-output render-opacity
                    (shader:swizzle texture-coordinate-opacity :z))))

(defun release-direct-gpu-mirror-resources (overlay)
  "Logically release every GPU resource retained by OVERLAY.

LUV:DESTROY invalidates each wrapper now and defers native teardown through the
backend submission frontier when an in-flight command buffer still owns it."
  (flet ((table-values (table)
           (let (values)
             (maphash (lambda (key value)
                        (declare (ignore key))
                        (push value values))
                      table)
             (clrhash table)
             values)))
    (let* ((bind-groups
             (mapcan #'table-values
                     (list (direct-widget-text-bind-groups overlay)
                           (direct-widget-image-bind-groups overlay)
                           (direct-widget-lattice-bind-groups overlay))))
           (frame-states
             (table-values (direct-widget-frame-states overlay)))
           (family-states
             (table-values (direct-mirror-pipeline-families overlay)))
           (resources
             (remove-duplicates
              (append bind-groups
                      (direct-widget-resources overlay)
                      (mapcan (lambda (family)
                                (copy-list
                                 (direct-mirror-pipeline-family-resources
                                  family)))
                              family-states))
              :test #'eq))
           (dependent (direct-mirror-solid-fragment-dependent overlay))
           (failures nil))
      ;; Detach every logical owner before invoking user-visible destruction.
      ;; One failing wrapper can therefore neither prevent later releases nor
      ;; remain reachable for a second destructive pass.
      (clrhash (direct-widget-pipelines overlay))
      (setf (widget-overlay-render-state overlay) nil
            (direct-widget-device overlay) nil
            (direct-widget-target-format overlay) nil
            (direct-widget-depth-stencil overlay) nil
            (direct-widget-shape-layout overlay) nil
            (direct-widget-image-layout overlay) nil
            (direct-widget-lattice-layout overlay) nil
            (direct-widget-image-sampler overlay) nil
            (direct-widget-resources overlay) nil
            (direct-widget-text-layout overlay) nil
            (direct-mirror-solid-fragment-dependent overlay) nil)
      (labels ((attempt (object thunk)
                 (handler-case
                     (funcall thunk)
                   (error (condition)
                     (push (cons object condition) failures)))))
        (dolist (state frame-states)
          (attempt
           (direct-widget-frame-source-state state)
           (lambda ()
             (release-gpu-frame-state
              (direct-widget-frame-source-state state))))
          (dolist (resource
                    (list (direct-widget-frame-shape-bind-group state)
                          (direct-widget-frame-buffer state)))
            (when resource
              (attempt resource (lambda () (luv:destroy resource))))))
        (dolist (resource resources)
          (when resource
            (attempt resource (lambda () (luv:destroy resource)))))
        (when dependent
          (attempt dependent
                   (lambda ()
                     (shader:release-shader-definition-dependent dependent)))))
      (when failures
        (error 'direct-mirror-release-error
               :failures (nreverse failures)))))
  overlay)

(defmethod release-mirror-compositor
    ((compositor direct-gpu-mirror-compositor))
  (release-direct-gpu-mirror-resources compositor)
  compositor)

(eval-when (:load-toplevel :execute)
  ;; Live images may retain the old raster-named :BEFORE adapter.  It would
  ;; release twice once the neutral primary below is installed.
  (let* ((generic (fdefinition 'release-raster-mirror-compositor))
         (method
           (find-method
            generic '(:before)
            (list (find-class 'direct-gpu-mirror-compositor)) nil)))
    (when method (remove-method generic method))))

(defmethod release-raster-mirror-compositor
    ((compositor direct-gpu-mirror-compositor))
  "Legacy adapter for callers compiled against the raster-era protocol name."
  (release-mirror-compositor compositor))


(defgeneric direct-gpu-mirror-depth-stencil (compositor)
  (:documentation
   "Return COMPOSITOR's depth declaration for every semantic pipeline."))


(defmethod direct-gpu-mirror-depth-stencil
    ((compositor direct-gpu-mirror-compositor))
  (declare (ignore compositor))
  nil)

(defgeneric set-direct-gpu-mirror-scissor
    (pass compositor mirror surface-texture clip))

(defun direct-mirror-projected-screen-vertex (state width height u v)
  "Project pane point U,V through STATE, retaining W,U,V for hit testing."
  (let* ((local-x (- (* 2.0 u) 1.0))
         (local-y (- (* 2.0 v) 1.0))
         (clip-x (+ (aref state 0)
                    (* local-x (aref state 4))
                    (* local-y (aref state 8))))
         (clip-y (+ (aref state 1)
                    (* local-x (aref state 5))
                    (* local-y (aref state 9))))
         (clip-w (+ (aref state 3)
                    (* local-x (aref state 7))
                    (* local-y (aref state 11)))))
    (if (zerop clip-w)
        (list 0.0 0.0 clip-w u v)
        (list (* width 0.5 (+ 1.0 (/ clip-x clip-w)))
              (* height 0.5 (+ 1.0 (/ clip-y clip-w)))
              clip-w u v))))

(defmethod set-direct-gpu-mirror-scissor
    (pass (compositor direct-gpu-mirror-compositor) mirror surface-texture clip)
  "Set a conservative target-pixel scissor for pane-local CLIP.

Clipping belongs to the shared affine compositor: both a world panel and a
HUD panel can rotate, scale, or project the same retained command stream."
  (destructuring-bind (surface-width surface-height &rest ignored)
      (luv:gpu-texture-size surface-texture)
    (declare (ignore ignored))
    (multiple-value-bind (logical-width logical-height)
        (gpu-mirror-logical-size mirror)
      (if (not (and (plusp logical-width) (plusp logical-height)))
          (progn
            (luv:set-scissor-rect pass 0 0 surface-width surface-height)
            t)
          (destructuring-bind (left top right bottom)
              (or clip (list 0 0 logical-width logical-height))
            (let* ((state (widget-overlay-render-state compositor))
                   (corners
                     (mapcar
                      (lambda (point)
                        (direct-mirror-projected-screen-vertex
                         state surface-width surface-height
                         (/ (first point) logical-width)
                         (/ (second point) logical-height)))
                      (list (list left top) (list right top)
                            (list left bottom) (list right bottom)))))
              ;; Crossing the camera plane makes the projected AABB
              ;; unbounded. The full target is the conservative answer.
              (if (some (lambda (corner)
                          (not (plusp (third corner))))
                        corners)
                  (progn
                    (luv:set-scissor-rect
                     pass 0 0 surface-width surface-height)
                    t)
                  (let* ((x
                           (max 0
                                (min surface-width
                                     (floor
                                      (reduce #'min corners :key #'first)))))
                         (y
                           (max 0
                                (min surface-height
                                     (floor
                                      (reduce #'min corners :key #'second)))))
                         (right
                           (max x
                                (min surface-width
                                     (ceiling
                                      (reduce #'max corners :key #'first)))))
                         (bottom
                           (max y
                                (min surface-height
                                     (ceiling
                                      (reduce #'max corners :key #'second)))))
                         (width (- right x))
                         (height (- bottom y)))
                    (when (and (plusp width) (plusp height))
                      (luv:set-scissor-rect pass x y width height)
                      t)))))))))


(defun ensure-direct-mirror-solid-fragment-dependent (compositor)
  (or (direct-mirror-solid-fragment-dependent compositor)
      (setf (direct-mirror-solid-fragment-dependent compositor)
            (shader:make-shader-definition-dependent
             (fdefinition 'shader:shader-specification-for)
             '(:mcluv-solid :fragment)))))

(defun direct-mirror-solid-fragment-revision (compositor)
  (multiple-value-bind (revision event)
      (shader:shader-definition-change-snapshot
       (ensure-direct-mirror-solid-fragment-dependent compositor))
    (declare (ignore event))
    revision))

(defun ensure-direct-mirror-shape-layout (compositor device)
  (or (direct-widget-shape-layout compositor)
      (let ((layout
              (luv:create
               device
               (luv:make-bind-group-layout-descriptor
                :label "direct McCLIM affine layout"
                :entries '((:binding 2 :type :uniform-buffer))))))
        (setf (direct-widget-shape-layout compositor) layout
              (direct-widget-resources compositor)
              (cons layout (direct-widget-resources compositor)))
        layout)))

(defun ensure-direct-mirror-image-layout (compositor device)
  (or (direct-widget-image-layout compositor)
      (let ((layout nil) (sampler nil) (completed-p nil))
        (unwind-protect
             (progn
               (setf layout
                     (luv:create
                      device
                      (luv:make-bind-group-layout-descriptor
                       :label "direct McCLIM image layout"
                       :entries '((:binding 0 :type :texture)
                                  (:binding 1 :type :sampler)
                                  (:binding 2 :type :uniform-buffer))))
                     sampler
                     (luv:create
                      device
                      (luv:make-sampler-descriptor
                       :label "direct McCLIM image sampler")))
               (setf (direct-widget-image-layout compositor) layout
                     (direct-widget-image-sampler compositor) sampler
                     (direct-widget-resources compositor)
                     (list* sampler layout (direct-widget-resources compositor))
                     completed-p t)
               layout)
          (unless completed-p
            (dolist (resource (remove nil (list sampler layout)))
              (ignore-errors (luv:destroy resource))))))))

(defun ensure-direct-mirror-lattice-layout (compositor device)
  (or (direct-widget-lattice-layout compositor)
      (let ((layout
              (luv:create
               device
               (luv:make-bind-group-layout-descriptor
                :label "direct McCLIM lattice layout"
                :entries '((:binding 0 :type :texture)
                           (:binding 2 :type :uniform-buffer))))))
        (setf (direct-widget-lattice-layout compositor) layout
              (direct-widget-resources compositor)
              (cons layout (direct-widget-resources compositor)))
        layout)))

(defun ensure-direct-mirror-text-layout (compositor device)
  (or (direct-widget-text-layout compositor)
      (let ((layout
              (luv:create
               device
               (luv:make-bind-group-layout-descriptor
                :label "direct McCLIM Slug layout"
                :entries '((:binding 0 :type :texture)
                           (:binding 1 :type :texture)
                           (:binding 2 :type :uniform-buffer))))))
        (setf (direct-widget-text-layout compositor) layout
              (direct-widget-resources compositor)
              (cons layout (direct-widget-resources compositor)))
        layout)))

(defun ensure-direct-mirror-family-layout (compositor device family)
  ;; Shape bindings also serve the application-specific chassis adapter and
  ;; are the common affine ABI for every direct mirror.
  (ensure-direct-mirror-shape-layout compositor device)
  (ecase family
    ((:solid :analytic :relief :gradient)
     (direct-widget-shape-layout compositor))
    (:image (ensure-direct-mirror-image-layout compositor device))
    (:lattice (ensure-direct-mirror-lattice-layout compositor device))
    (:text (ensure-direct-mirror-text-layout compositor device))))

(defun direct-mirror-family-specifications (compositor family)
  (ecase family
    (:solid
     (values
      "solid" (direct-widget-shape-layout compositor)
      (direct-widget-solid-vertex-specification)
      (shader:shader-specification-for :mcluv-solid :fragment)
      24
      '((:shader-location 0 :offset 0 :format :float32x3)
        (:shader-location 1 :offset 12 :format :float32x3))))
    (:analytic
     (values
      "analytic" (direct-widget-shape-layout compositor)
      (direct-widget-analytic-vertex-specification)
      (luv.analytic:roundrect-fragment-specification)
      48
      '((:shader-location 0 :offset 0 :format :float32x3)
        (:shader-location 1 :offset 12 :format :float32x3)
        (:shader-location 2 :offset 24 :format :float32x3)
        (:shader-location 3 :offset 36 :format :float32x3))))
    (:relief
     (values
      "relief" (direct-widget-shape-layout compositor)
      (direct-widget-relief-vertex-specification)
      (relief-roundrect-fragment-specification)
      60
      '((:shader-location 0 :offset 0 :format :float32x3)
        (:shader-location 1 :offset 12 :format :float32x3)
        (:shader-location 2 :offset 24 :format :float32x3)
        (:shader-location 3 :offset 36 :format :float32x3)
        (:shader-location 4 :offset 48 :format :float32x3))))
    (:gradient
     (values
      "gradient" (direct-widget-shape-layout compositor)
      (direct-widget-gradient-vertex-specification)
      (gradient-roundrect-fragment-specification)
      84
      '((:shader-location 0 :offset 0 :format :float32x3)
        (:shader-location 1 :offset 12 :format :float32x3)
        (:shader-location 2 :offset 24 :format :float32x3)
        (:shader-location 3 :offset 36 :format :float32x3)
        (:shader-location 4 :offset 48 :format :float32x3)
        (:shader-location 5 :offset 60 :format :float32x3)
        (:shader-location 6 :offset 72 :format :float32x3))))
    (:image
     (values
      "image" (direct-widget-image-layout compositor)
      (direct-widget-image-vertex-specification)
      (image-roundrect-fragment-specification)
      48
      '((:shader-location 0 :offset 0 :format :float32x3)
        (:shader-location 1 :offset 12 :format :float32x3)
        (:shader-location 2 :offset 24 :format :float32x3)
        (:shader-location 3 :offset 36 :format :float32x3))))
    (:lattice
     (values
      "lattice" (direct-widget-lattice-layout compositor)
      (direct-widget-analytic-vertex-specification)
      (luv.analytic:lattice-fragment-specification)
      48
      '((:shader-location 0 :offset 0 :format :float32x3)
        (:shader-location 1 :offset 12 :format :float32x3)
        (:shader-location 2 :offset 24 :format :float32x3)
        (:shader-location 3 :offset 36 :format :float32x3))))
    (:text
     (values
      "Slug text" (direct-widget-text-layout compositor)
      (direct-mirror-slug-vertex-specification)
      (luv.slug:slug-atlas-fragment-specification)
      72
      '((:shader-location 0 :offset 0 :format :float32x3)
        (:shader-location 1 :offset 12 :format :float32x3)
        (:shader-location 2 :offset 24 :format :float32x3)
        (:shader-location 3 :offset 36 :format :float32x3)
        (:shader-location 4 :offset 48 :format :float32x3)
        (:shader-location 5 :offset 60 :format :float32x3))))))

(defun build-direct-mirror-pipeline-family
    (compositor family target-format depth-stencil
     source-revision solid-fragment-revision)
  (multiple-value-bind (name layout vertex-spec fragment-spec stride attributes)
      (direct-mirror-family-specifications compositor family)
    (let ((created nil) (completed-p nil))
      (unwind-protect
           (labels ((create (descriptor)
                      (let ((resource
                              (luv:create
                               (direct-widget-device compositor) descriptor)))
                        (push resource created)
                        resource)))
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
                          :buffers
                          ((:array-stride ,stride :attributes ,attributes)))
                        :fragment
                        `(:module ,fragment
                          :targets
                          ((:format ,target-format
                            :blend :premultiplied-alpha)))
                        :depth-stencil depth-stencil
                        :primitive '(:topology :triangle-list)))))
               (setf completed-p t)
               (make-direct-mirror-pipeline-family
                :pipeline pipeline :resources created
                :source-revision source-revision
                :solid-fragment-revision solid-fragment-revision)))
        (unless completed-p
          (dolist (resource created)
            (ignore-errors (luv:destroy resource))))))))

(defun direct-mirror-pipeline-family-current-p
    (state family source-revision solid-fragment-revision)
  (and state
       (= source-revision
          (direct-mirror-pipeline-family-source-revision state))
       (or (not (eq family :solid))
           (= solid-fragment-revision
              (direct-mirror-pipeline-family-solid-fragment-revision state)))))

(defun release-direct-mirror-family-resources (families)
  "Exhaustively release detached pipeline FAMILY states."
  (let ((failures nil))
    (dolist (family families)
      (dolist (resource (direct-mirror-pipeline-family-resources family))
        (handler-case
            (luv:destroy resource)
          (error (condition)
            (push (cons resource condition) failures)))))
    (when failures
      (error 'direct-mirror-release-error
             :failures (nreverse failures)))))

(defun prepare-direct-gpu-mirror-pipelines
    (compositor target-format depth-stencil families)
  "Compile only FAMILIES, transactionally, before an application render pass."
  (let ((device (mirror-device (widget-overlay-mirror compositor))))
    (when families
      (unless (and (eq device (direct-widget-device compositor))
                   (eq target-format (direct-widget-target-format compositor))
                   (equal depth-stencil
                          (direct-widget-depth-stencil compositor)))
        (release-direct-gpu-mirror-resources compositor)
        (setf (direct-widget-device compositor) device
              (direct-widget-target-format compositor) target-format
              (direct-widget-depth-stencil compositor) depth-stencil))
      (dolist (family families)
        (ensure-direct-mirror-family-layout compositor device family))
      (let* ((source-revision (shader:shader-source-revision))
             (solid-fragment-revision
               (if (member :solid families)
                   (direct-mirror-solid-fragment-revision compositor)
                   0))
             (stale-families
               (remove-if
                (lambda (family)
                  (direct-mirror-pipeline-family-current-p
                   (gethash family
                            (direct-mirror-pipeline-families compositor))
                   family source-revision solid-fragment-revision))
                families)))
        (when stale-families
          (let ((candidates nil) (completed-p nil))
            (unwind-protect
                 (progn
                   (dolist (family stale-families)
                     (push
                      (cons family
                            (build-direct-mirror-pipeline-family
                             compositor family target-format depth-stencil
                             source-revision solid-fragment-revision))
                      candidates))
                   ;; Never publish a mixed cohort if source changed during a
                   ;; slow backend compilation.  The old families remain good.
                   (unless (= source-revision (shader:shader-source-revision))
                     (error "Direct McCLIM shader source changed while compiling."))
                   (when (member :solid stale-families)
                     (unless (= solid-fragment-revision
                                (direct-mirror-solid-fragment-revision compositor))
                       (error
                        "Direct McCLIM solid shader changed while compiling.")))
                   (let (retired)
                     (dolist (entry candidates)
                       (let* ((family (car entry))
                              (candidate (cdr entry))
                              (old
                                (gethash
                                 family
                                 (direct-mirror-pipeline-families compositor))))
                         (when old (push old retired))
                         (setf
                          (gethash family
                                   (direct-mirror-pipeline-families compositor))
                          candidate
                          (gethash family (direct-widget-pipelines compositor))
                          (direct-mirror-pipeline-family-pipeline candidate))))
                     (setf completed-p t)
                     (when retired
                       (release-direct-mirror-family-resources retired))))
              (unless completed-p
                (release-direct-mirror-family-resources
                 (mapcar #'cdr candidates)))))))))
  compositor)

(defun direct-gpu-mirror-missing-pipeline-families
    (compositor target-format depth-stencil families)
  (if (and (eq (mirror-device (widget-overlay-mirror compositor))
               (direct-widget-device compositor))
           (eq target-format (direct-widget-target-format compositor))
           (equal depth-stencil (direct-widget-depth-stencil compositor)))
      (remove-if (lambda (family)
                   (gethash family (direct-widget-pipelines compositor)))
                 families)
      families))

(defun require-direct-gpu-mirror-pipelines
    (compositor target-format depth-stencil families)
  "Assert that FAMILIES were prepared before the caller opened its pass."
  (let ((missing
          (direct-gpu-mirror-missing-pipeline-families
           compositor target-format depth-stencil families)))
    (when missing
      (error 'direct-mirror-pipelines-not-prepared
             :families missing :format target-format
             :depth-stencil depth-stencil)))
  compositor)

(defun ensure-direct-gpu-mirror-pipelines
    (compositor target-format depth-stencil)
  "Legacy encode adapter: verify preparation without compiling in an open pass."
  (alexandria:when-let ((revision
                         (gpu-mirror-prepared-revision
                          (widget-overlay-mirror compositor))))
    (require-direct-gpu-mirror-pipelines
     compositor target-format depth-stencil
     (gpu-prepared-frame-pipeline-families revision)))
  compositor)

(defun prepare-direct-gpu-mirror
    (compositor &key
                  (revision
                    (alexandria:when-let
                        ((mirror (widget-overlay-mirror compositor)))
                      (gpu-mirror-prepared-revision mirror)))
                  target-format
                  (depth-stencil
                    (direct-gpu-mirror-depth-stencil compositor)))
  "Prepare COMPOSITOR's current semantic families outside a render pass."
  (let ((mirror (widget-overlay-mirror compositor)))
    (when (and mirror revision)
      (let* ((context (mirror-context mirror))
           (target-format
             (or target-format
                 (and context (luv:canvas-format context)))))
        (when target-format
          (prepare-direct-gpu-mirror-pipelines
           compositor target-format depth-stencil
           (gpu-prepared-frame-pipeline-families revision))))))
  compositor)

(defmethod prepare-mirror-compositor-revision
    ((compositor direct-gpu-mirror-compositor) mirror revision)
  (declare (ignore mirror))
  (prepare-direct-gpu-mirror compositor :revision revision))

(defmethod prepare-mirror-compositor-target-revision
    ((compositor direct-gpu-mirror-compositor) mirror revision
     &key target-format
       (depth-stencil (direct-gpu-mirror-depth-stencil compositor)))
  (declare (ignore mirror))
  (prepare-direct-gpu-mirror
   compositor :revision revision :target-format target-format
              :depth-stencil depth-stencil))

(defmethod initialize-instance :after
    ((compositor direct-gpu-mirror-compositor) &key)
  ;; A compositor is normally attached after the mirror's first repaint.  This
  ;; closes that seam without waiting for an application render pass.
  (prepare-direct-gpu-mirror compositor))

(defun direct-gpu-mirror-frame-resource-key (overlay surface-texture)
  "Return the bounded backend frame key for SURFACE-TEXTURE.

Metal presents a stable drawable through a fresh Lisp texture wrapper each
time. Keying by that wrapper leaks one uniform buffer and several bind groups
per frame; the canvas context exposes the native drawable identity instead."
  (alexandria:if-let ((context
                        (mirror-context (widget-overlay-mirror overlay))))
    (luv:canvas-frame-resource-key context surface-texture)
    surface-texture))

(defun ensure-direct-widget-frame-state (overlay surface-texture)
  (let ((key
          (direct-gpu-mirror-frame-resource-key overlay surface-texture)))
    (or (gethash key (direct-widget-frame-states overlay))
        (let ((buffer nil) (group nil) (source-state nil) (completed-p nil))
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
                         :entries `((:binding 2 :resource ,buffer))))
                       source-state
                       (make-instance
                        'gpu-mirror-frame-state
                        :mirror (widget-overlay-mirror overlay)))
                 (let ((state
                         (make-instance
                          'direct-widget-frame-state
                          :buffer buffer :shape-bind-group group
                          :source-state source-state)))
                   (setf (gethash key (direct-widget-frame-states overlay))
                         state
                         completed-p t)
                   state))
            (unless completed-p
              (when source-state
                (ignore-errors (release-gpu-frame-state source-state)))
              (when group (luv:destroy group))
              (when buffer (luv:destroy buffer))))))))

(defun materialize-direct-widget-frame-revision
    (compositor frame-state revision)
  "Upload REVISION once into FRAME-STATE's backend-bounded source slot."
  (unless (eq revision (direct-widget-frame-prepared-revision frame-state))
    (upload-gpu-prepared-frame-revision
     (direct-widget-frame-source-state frame-state)
     (direct-widget-device compositor)
     revision)
    ;; Publish the pairing only after all six dense buffers contain REVISION.
    (setf (direct-widget-frame-prepared-revision frame-state) revision))
  (direct-widget-frame-source-state frame-state))

(defun ensure-direct-widget-image-bind-group
    (overlay paint frame-state)
  (let ((key (list paint frame-state)))
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

(defun ensure-direct-widget-lattice-bind-group
    (overlay paint frame-state)
  (let ((key (list paint frame-state)))
    (or (gethash key (direct-widget-lattice-bind-groups overlay))
        (setf (gethash key (direct-widget-lattice-bind-groups overlay))
              (luv:create
               (direct-widget-device overlay)
               (luv:make-bind-group-descriptor
                :label "direct McCLIM lattice bindings"
                :layout (direct-widget-lattice-layout overlay)
                :entries
                `((:binding 0 :resource ,(gpu-lattice-paint-view paint))
                  (:binding 2
                   :resource
                   ,(direct-widget-frame-buffer frame-state)))))))))

(defun ensure-direct-widget-text-bind-group
    (overlay atlas frame-state)
  (let ((key (list atlas frame-state)))
    (or (gethash key (direct-widget-text-bind-groups overlay))
        (setf (gethash key (direct-widget-text-bind-groups overlay))
              (luv:create
               (direct-widget-device overlay)
               (luv:make-bind-group-descriptor
                :label "direct McCLIM Slug bindings"
                :layout (direct-widget-text-layout overlay)
                :entries
                `((:binding 0
                   :resource ,(luv.slug:slug-glyph-atlas-band-view atlas))
                  (:binding 1
                   :resource ,(luv.slug:slug-glyph-atlas-curve-view atlas))
                  (:binding 2
                   :resource ,(direct-widget-frame-buffer frame-state)))))))))

(defmethod encode-gpu-command
    ((command gpu-solid-command) pass
     (context direct-widget-command-encode-context))
  (let ((overlay (direct-widget-command-encode-context-overlay context))
        (destination-state
          (direct-widget-command-encode-context-destination-state context))
        (source-state
          (direct-widget-command-encode-context-source-state context)))
    (encode-gpu-draw-range
     pass
     (gethash :solid (direct-widget-pipelines overlay))
     (direct-widget-frame-shape-bind-group destination-state)
     (gpu-frame-state-vertex-buffer source-state)
     (gpu-solid-command-first-vertex command)
     (gpu-solid-command-vertex-count command))))

(defmethod encode-gpu-command
    ((command gpu-analytic-command) pass
     (context direct-widget-command-encode-context))
  (let ((overlay (direct-widget-command-encode-context-overlay context))
        (destination-state
          (direct-widget-command-encode-context-destination-state context))
        (source-state
          (direct-widget-command-encode-context-source-state context)))
    (encode-gpu-draw-range
     pass
     (gethash :analytic (direct-widget-pipelines overlay))
     (direct-widget-frame-shape-bind-group destination-state)
     (gpu-frame-state-analytic-buffer source-state)
     (gpu-analytic-command-first-vertex command)
     (gpu-analytic-command-vertex-count command))))

(defmethod encode-gpu-command
    ((command gpu-relief-analytic-command) pass
     (context direct-widget-command-encode-context))
  (let ((overlay (direct-widget-command-encode-context-overlay context))
        (destination-state
          (direct-widget-command-encode-context-destination-state context))
        (source-state
          (direct-widget-command-encode-context-source-state context)))
    (encode-gpu-draw-range
     pass
     (gethash :relief (direct-widget-pipelines overlay))
     (direct-widget-frame-shape-bind-group destination-state)
     (gpu-frame-state-relief-buffer source-state)
     (gpu-relief-analytic-command-first-vertex command)
     (gpu-relief-analytic-command-vertex-count command))))

(defmethod encode-gpu-command
    ((command gpu-gradient-analytic-command) pass
     (context direct-widget-command-encode-context))
  (let ((overlay (direct-widget-command-encode-context-overlay context))
        (destination-state
          (direct-widget-command-encode-context-destination-state context))
        (source-state
          (direct-widget-command-encode-context-source-state context)))
    (encode-gpu-draw-range
     pass
     (gethash :gradient (direct-widget-pipelines overlay))
     (direct-widget-frame-shape-bind-group destination-state)
     (gpu-frame-state-gradient-buffer source-state)
     (gpu-gradient-analytic-command-first-vertex command)
     (gpu-gradient-analytic-command-vertex-count command))))

(defmethod encode-gpu-command
    ((command gpu-prepared-lattice-command) pass
     (context direct-widget-command-encode-context))
  (let* ((overlay
           (direct-widget-command-encode-context-overlay context))
         (destination-state
           (direct-widget-command-encode-context-destination-state context))
         (source-state
           (direct-widget-command-encode-context-source-state context)))
    (encode-gpu-draw-range
     pass
     (gethash :lattice (direct-widget-pipelines overlay))
     (ensure-direct-widget-lattice-bind-group
      overlay (gpu-prepared-lattice-command-paint command)
      destination-state)
     (gpu-frame-state-analytic-buffer source-state)
     (gpu-prepared-lattice-command-first-vertex command)
     (gpu-prepared-lattice-command-vertex-count command))))

(defmethod encode-gpu-command
    ((command gpu-prepared-image-command) pass
     (context direct-widget-command-encode-context))
  (let* ((overlay
           (direct-widget-command-encode-context-overlay context))
         (destination-state
           (direct-widget-command-encode-context-destination-state context))
         (source-state
           (direct-widget-command-encode-context-source-state context)))
    (encode-gpu-draw-range
     pass
     (gethash :image (direct-widget-pipelines overlay))
     (ensure-direct-widget-image-bind-group
      overlay (gpu-prepared-image-command-paint command)
      destination-state)
     (gpu-frame-state-image-buffer source-state)
     (gpu-prepared-image-command-first-vertex command)
     (gpu-prepared-image-command-vertex-count command))))

(defmethod encode-gpu-command
    ((command gpu-prepared-text-command) pass
     (context direct-widget-command-encode-context))
  (let* ((overlay
           (direct-widget-command-encode-context-overlay context))
         (destination-state
           (direct-widget-command-encode-context-destination-state context))
         (source-state
           (direct-widget-command-encode-context-source-state context)))
    (encode-gpu-draw-range
     pass
     (gethash :text (direct-widget-pipelines overlay))
     (ensure-direct-widget-text-bind-group
      overlay (gpu-prepared-text-command-atlas command)
      destination-state)
     (gpu-frame-state-text-buffer source-state)
     (gpu-prepared-text-command-first-vertex command)
     (gpu-prepared-text-command-vertex-count command))))


(luv:zdefun (encode-direct-gpu-mirror
             :zone :mcluv/encode-direct-mirror
             :value
             (length
              (or (gpu-mirror-prepared-commands
                   (widget-overlay-mirror compositor))
                  nil)))
    (compositor pass destination-texture state
     &key (frame-texture destination-texture))
  "Replay COMPOSITOR's textureless McCLIM mirror into PASS under affine STATE.

DESTINATION-TEXTURE describes the open pass for format and clipping.
FRAME-TEXTURE names the bounded in-flight presentation slot whose host-visible
buffers are safe to update; normally both arguments are the same texture."
  (let* ((mirror (widget-overlay-mirror compositor))
         (revision (gpu-mirror-prepared-revision mirror))
         (commands (and revision (gpu-prepared-frame-commands revision))))
    (when commands
      (let ((target-format (luv:gpu-texture-format destination-texture))
            (depth-stencil (direct-gpu-mirror-depth-stencil compositor)))
        ;; This is a guard only. Compilation belongs to mirror publication or
        ;; PREPARE-DIRECT-GPU-MIRROR, both outside the application's open pass.
        (require-direct-gpu-mirror-pipelines
         compositor target-format depth-stencil
         (gpu-prepared-frame-pipeline-families revision)))
      (let* ((frame-state
               (ensure-direct-widget-frame-state compositor frame-texture))
             (source-state
               (materialize-direct-widget-frame-revision
                compositor frame-state revision))
             (encode-context
               (make-direct-widget-command-encode-context
                :overlay compositor
                :destination-state frame-state
                :source-state source-state))
             (active-clip (list :unset))
             (clip-visible-p t))
        (declare (dynamic-extent encode-context))
        (setf (widget-overlay-render-state compositor) state)
        (luv:write-buffer (direct-widget-frame-buffer frame-state) state)
        (dolist (command commands)
          (let ((clip (gpu-command-clip command)))
            (unless (equal clip active-clip)
              (setf active-clip clip
                    clip-visible-p
                    (set-direct-gpu-mirror-scissor
                     pass compositor mirror destination-texture clip))))
          (when clip-visible-p
            (encode-gpu-command command pass encode-context)))
        (destructuring-bind (width height &rest ignored)
            (luv:gpu-texture-size destination-texture)
          (declare (ignore ignored))
          (luv:set-scissor-rect pass 0 0 width height)))))
  compositor)
