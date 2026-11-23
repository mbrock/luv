(in-package #:luft.render.shaders)

;;; Temporal companion mesh shader: skins current and previous bone palettes in
;;; Luft's Z-up world coordinates, applies Halton subpixel jitter, and writes
;;; exact per-vertex motion vectors so MetalFX / shader TAA reconstruct
;;; companions at full output resolution.
(define-shader-abstraction luft-companion-skin-row (palette joints weights axis)
  (flet ((term (lane)
           `(* (swizzle ,weights ,lane)
               (buffer-element ,palette (+ (* (uint (swizzle ,joints ,lane)) (uint 3.0))
                                           (uint ,axis))))))
    `(+ ,(term :x) (+ ,(term :y) (+ ,(term :z) ,(term :w))))))

(define-live-shader luft-companion-vertex-specification
    (:stage :vertex
     :inputs ((rest-position :vec3 :location 0)
              (rest-normal :vec3 :location 1)
              (albedo :vec3 :location 2)
              (joints :vec4 :location 3)
              (weights :vec4 :location 4)
              (origin-scale :vec4 :location 5)
              (heading :vec4 :location 6)
              (previous-origin-scale :vec4 :location 7)
              (previous-heading :vec4 :location 8))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-point :vec3 :location 0)
               (world-normal :vec3 :location 1)
               (surface-color :vec3 :location 2)
               (edge-light :float :location 3)
               (current-clip :vec4 :location 4)
               (previous-clip :vec4 :location 5))
     :resources ((camera-state :uniform-block :set 0 :binding 2
                               :members #.*scene-uniform-members*)
                 (palette :storage-buffer :set 0 :binding 3 :element :vec4)
                 (previous-palette :storage-buffer :set 0 :binding 4 :element :vec4)))
  (let* ((row-x (luft-companion-skin-row palette joints weights 0.0))
         (row-y (luft-companion-skin-row palette joints weights 1.0))
         (row-z (luft-companion-skin-row palette joints weights 2.0))
         (position (vec3 (+ (dot (swizzle row-x :xyz) rest-position) (swizzle row-x :w))
                         (+ (dot (swizzle row-y :xyz) rest-position) (swizzle row-y :w))
                         (+ (dot (swizzle row-z :xyz) rest-position) (swizzle row-z :w))))
         (normal (vec3 (dot (swizzle row-x :xyz) rest-normal)
                       (dot (swizzle row-y :xyz) rest-normal)
                       (dot (swizzle row-z :xyz) rest-normal)))
         (prev-row-x (luft-companion-skin-row previous-palette joints weights 0.0))
         (prev-row-y (luft-companion-skin-row previous-palette joints weights 1.0))
         (prev-row-z (luft-companion-skin-row previous-palette joints weights 2.0))
         (prev-position (vec3 (+ (dot (swizzle prev-row-x :xyz) rest-position) (swizzle prev-row-x :w))
                              (+ (dot (swizzle prev-row-y :xyz) rest-position) (swizzle prev-row-y :w))
                              (+ (dot (swizzle prev-row-z :xyz) rest-position) (swizzle prev-row-z :w))))
         (facing (vec3 (swizzle heading :x) (swizzle heading :y) 0.0))
         (sideways (vec3 (swizzle heading :y) (- (swizzle heading :x)) 0.0))
         (point (+ (swizzle origin-scale :xyz)
                   (* (swizzle origin-scale :w)
                      (+ (* sideways (swizzle position :x))
                         (+ (* facing (swizzle position :z))
                            (vec3 0.0 0.0 (swizzle position :y)))))))
         (world (assume-quantity point :quantity quantities:world-position :unit quantities:cell))
         (prev-facing (vec3 (swizzle previous-heading :x) (swizzle previous-heading :y) 0.0))
         (prev-sideways (vec3 (swizzle previous-heading :y) (- (swizzle previous-heading :x)) 0.0))
         (prev-point (+ (swizzle previous-origin-scale :xyz)
                        (* (swizzle previous-origin-scale :w)
                           (+ (* prev-sideways (swizzle prev-position :x))
                              (+ (* prev-facing (swizzle prev-position :z))
                                 (vec3 0.0 0.0 (swizzle prev-position :y)))))))
         (prev-world (assume-quantity prev-point :quantity quantities:world-position :unit quantities:cell))
         (divisor (swizzle (representation render-parameters) :z))
         (current (mesh-view-clip world camera-position camera-right camera-up
                                  camera-forward camera-projection divisor))
         (previous (mesh-view-clip prev-world previous-camera-position previous-camera-right
                                   previous-camera-up previous-camera-forward
                                   previous-camera-projection divisor))
         (jitter (representation (swizzle temporal-parameters :xy))))
    (set-output clip-position
                (vec4 (+ (swizzle current :x) (* (swizzle jitter :x) (swizzle current :w)))
                      (+ (swizzle current :y) (* (swizzle jitter :y) (swizzle current :w)))
                      (swizzle current :z) (swizzle current :w)))
    (set-output world-point point)
    (set-output world-normal
                (+ (* sideways (swizzle normal :x))
                   (+ (* facing (swizzle normal :z))
                      (vec3 0.0 0.0 (swizzle normal :y)))))
    (set-output surface-color albedo)
    (set-output edge-light (swizzle heading :z))
    (set-output current-clip current)
    (set-output previous-clip previous)))

(define-live-shader luft-companion-fragment-specification
    (:stage :fragment
     :inputs ((world-point :vec3 :location 0)
              (world-normal :vec3 :location 1)
              (surface-color :vec3 :location 2)
              (edge-light :float :location 3)
              (current-clip :vec4 :location 4)
              (previous-clip :vec4 :location 5))
     :outputs ((color-output :vec4 :location 0)
               (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :set 0 :binding 2
                               :members #.*scene-uniform-members*)))
  (let* ((normal (normalize world-normal))
         (view (normalize (- (representation (swizzle camera-position :xyz)) world-point)))
         (sun (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (sky-color (representation (swizzle sky-color-vector :xyz)))
         (ground-color (representation (swizzle ground-color-vector :xyz)))
         (wrapped (max 0.0 (/ (+ (dot normal sun) 0.35) 1.35)))
         (upness (swizzle normal :z))
         (ambient (+ (* sky-color (+ 0.30 (* 0.45 (+ 0.5 (* 0.5 upness)))))
                     (* ground-color (+ 0.12 (* 0.28 (- 0.5 (* 0.5 upness)))))))
         (rim (expt (- 1.0 (max 0.0 (dot normal view))) 3.0))
         (light (+ ambient (* sun-color (+ 0.12 (* wrapped 1.15))))))
    (set-output color-output
                (vec4 (+ (* surface-color light) (* surface-color (* edge-light rim))) 1.0))
    (set-output motion-output (mesh-temporal-motion previous-clip current-clip))))

(in-package #:luvcraft.luft.game)

;;; The old painters and shaders remain the authority. This adapter translates
;;; only ownership, frame uniforms and coordinates, not their visual language.
;;; #HN9MI5 #5JDNKF

(defclass presentation-camera (luvcraft:fly-camera) ()
  (:metaclass luv.arithmetic.records:quantity-class))

(defmethod luvcraft:camera-uniform-data :around ((camera presentation-camera) width height)
  (let ((data (call-next-method)))
    ;; Every retained primitive must use the terrain's depth projection.
    (setf (aref data 18) (/ 600.0 (- 600.0 0.1))
          (aref data 19) (/ (* -600.0 0.1) (- 600.0 0.1)))
    data))

(defclass companion-presentation ()
  ((environment :initarg :environment :reader presentation-environment)
   (layout :initarg :layout :reader presentation-layout)
   (frames :initform (luv:make-canvas-frame-resource-cache) :reader presentation-frames)
   (temporal-program :initform nil :accessor presentation-temporal-program)
   (temporal-bodies :initform (make-hash-table :test #'eq) :reader presentation-temporal-bodies)))

(defstruct temporal-companion-frame palette previous-palette instance binding camera-buffer)

(defstruct temporal-companion-state
  (frames (luv:make-canvas-frame-resource-cache))
  previous-palette
  (previous-instance (make-array 8 :element-type 'single-float :initial-element 0.0))
  (instance (make-array 16 :element-type 'single-float :initial-element 0.0))
  (last-frame -2))

(defun release-temporal-companion-frame (frame)
  (when (temporal-companion-frame-binding frame)
    (luv:destroy (temporal-companion-frame-binding frame)))
  (luv:destroy (temporal-companion-frame-palette frame))
  (luv:destroy (temporal-companion-frame-previous-palette frame))
  (luv:destroy (temporal-companion-frame-instance frame)))

(defun release-temporal-companion-state (state)
  (luv:destroy-canvas-frame-resource-cache
   (temporal-companion-state-frames state) #'release-temporal-companion-frame))

(defun ensure-temporal-companion-program (state device)
  (or (presentation-temporal-program state)
      (setf (presentation-temporal-program state)
            (render:make-drawing-program
             device :label "Luft temporal companion mesh"
             :vertex (luft.render.shaders::luft-companion-vertex-specification)
             :fragment (luft.render.shaders::luft-companion-fragment-specification)
             :vertex-buffers
             `((:array-stride ,(* 4 luvcraft.agent::+companion-vertex-floats+)
                :attributes ((:shader-location 0 :offset 0 :format :float32x3)
                             (:shader-location 1 :offset 12 :format :float32x3)
                             (:shader-location 2 :offset 24 :format :float32x3)
                             (:shader-location 3 :offset 36 :format :float32x4)
                             (:shader-location 4 :offset 52 :format :float32x4)))
               (:array-stride 64 :step-mode :instance
                :attributes ((:shader-location 5 :offset 0 :format :float32x4)
                             (:shader-location 6 :offset 16 :format :float32x4)
                             (:shader-location 7 :offset 32 :format :float32x4)
                             (:shader-location 8 :offset 48 :format :float32x4))))
             :targets (list (list :format luvcraft::+luvcraft-scene-color-format+)
                            (list :format :rg16-float))
             :sample-count 1
             :depth-stencil '(:format :depth32-float :depth-write-enabled t
                              :depth-compare :less)))))

(defun release-companion-frame (frame)
  (dolist (resource frame) (luv:destroy resource)))

(defun companion-presentation (viewer)
  (or (find-if (lambda (overlay) (typep overlay 'companion-presentation))
               (render:viewer-overlays viewer))
      (let* ((layout
               (luv:create
                (render:viewer-device viewer)
                (luv:make-bind-group-layout-descriptor
                 :label "original companion scene uniforms"
                 :entries '((:binding 2 :type :uniform-buffer)))))
             (environment (make-instance 'luvcraft:luvcraft-session
                                         :camera (make-instance 'presentation-camera)
                                         :sky-clock (world-sky-clock (viewer-world viewer))
                                         :sky-profile (world-sky-profile (viewer-world viewer)))))
        (render:add-viewer-overlay
         viewer (make-instance 'companion-presentation :layout layout
                                                      :environment environment)))))

(defmethod render:release-viewer-overlay ((state companion-presentation))
  (maphash (lambda (overlay body-state)
             (declare (ignore overlay))
             (release-temporal-companion-state body-state))
           (presentation-temporal-bodies state))
  (clrhash (presentation-temporal-bodies state))
  (when (presentation-temporal-program state)
    (render::release-program (presentation-temporal-program state))
    (setf (presentation-temporal-program state) nil))
  (luv:destroy-canvas-frame-resource-cache (presentation-frames state) #'release-companion-frame)
  (luv:destroy (presentation-layout state)))

(defmethod render:evict-viewer-overlay-frame-key ((state companion-presentation) key)
  (maphash (lambda (overlay body-state)
             (declare (ignore overlay))
             (luv:evict-canvas-frame-resource-key
              (temporal-companion-state-frames body-state) key #'release-temporal-companion-frame))
           (presentation-temporal-bodies state))
  (luv:evict-canvas-frame-resource-key (presentation-frames state) key #'release-companion-frame))

(defun presentation-position (viewer p)
  (let ((offset (game::luvcraft-world-source-offset (streaming-source viewer))))
    (vec3:make-vec3 (- (vec3:vec3-x p) offset) (vec3:vec3-z p)
                    (- (vec3:vec3-y p) offset))))

(defun host-position (viewer p)
  (let ((offset (game::luvcraft-world-source-offset (streaming-source viewer))))
    (vec3:make-vec3 (+ (vec3:vec3-x p) offset) (+ (vec3:vec3-z p) offset)
                    (vec3:vec3-y p))))

;;; Give the original safe-viewport framing solver a wall in its own axes.
;;; This is only a geometric view, not a second terminal or block world.
#-win32
(defclass terminal-framing-surface (luvcraft::terminal-surface)
  ((surface :initarg :surface :reader framing-surface)
   (viewer :initarg :viewer :reader framing-viewer)))

#-win32
(defmethod luvcraft::terminal-surface-physical-width ((surface terminal-framing-surface))
  (render::terminal-surface-width (framing-surface surface)))

#-win32
(defmethod luvcraft::terminal-surface-physical-height ((surface terminal-framing-surface))
  (render::terminal-surface-height (framing-surface surface)))

#-win32
(defmethod luvcraft::terminal-surface-lower-left-point
    ((surface terminal-framing-surface) &optional (offset 0.006))
  (presentation-position
   (framing-viewer surface)
   (render::terminal-surface-point
    (framing-surface surface) 0 0 (- offset render::*terminal-surface-offset*))))

;; Windows does not load the atelier's terminal overlay or its focus class.
#-win32
(defmethod render:viewer-focus-camera-pose
    ((state render::viewer-terminals) (viewer luvcraft-viewer))
  (let ((terminal (render::viewer-terminals-focus state)))
    (when terminal
      (let* ((surface (render::world-terminal-surface terminal))
             (framing (make-instance
                       'terminal-framing-surface :surface surface :viewer viewer
                       :face (cdr (assoc (render::terminal-surface-normal surface)
                                         '(((1 0 0) . :right) ((-1 0 0) . :left)
                                           ((0 1 0) . :front) ((0 -1 0) . :back))
                                         :test #'equal))
                       :block-width (render::terminal-surface-width surface)
                       :block-height (render::terminal-surface-height surface))))
        (destructuring-bind (width height) (luv:canvas-extent (render:viewer-context viewer))
          ;; The shared workbench owns the top status line, not an overlay.
          (let ((left 0) (top (if (luv.workbench:application-workbench viewer)
                                 mcluv::+status-bar-height+ 0))
                (right 0) (bottom 0))
            (dolist (overlay (render:viewer-overlays viewer))
              (multiple-value-bind (l u r b) (render::viewer-overlay-focus-insets overlay viewer)
                (setf left (max left l) top (max top u)
                      right (max right r) bottom (max bottom b))))
            (let ((pose (luvcraft::terminal-focus-camera-pose
                         framing width height left top right bottom)))
              (render:make-camera-pose
               :position (host-position viewer (luvcraft::camera-pose-position pose))
               :yaw (- (/ pi 2) (luvcraft::camera-pose-yaw pose))
               :pitch (luvcraft::camera-pose-pitch pose)
               :field-of-view (luvcraft::camera-pose-field-of-view pose)))))))))

(defmethod luvcraft:luvcraft-session-canvas ((viewer luvcraft-viewer))
  (render:viewer-canvas viewer))
(defmethod luvcraft:luvcraft-session-context ((viewer luvcraft-viewer))
  (render:viewer-context viewer))
(defmethod luvcraft:luvcraft-session-device ((viewer luvcraft-viewer))
  (render:viewer-device viewer))
(defmethod luvcraft:luvcraft-session-world ((viewer luvcraft-viewer))
  (world-block-world (viewer-world viewer)))
(defmethod luvcraft:luvcraft-session-overlays ((viewer luvcraft-viewer))
  (render:viewer-overlays viewer))
(defmethod luvcraft:luvcraft-session-modal-focus ((viewer luvcraft-viewer))
  (render:viewer-modal-focus viewer))

(defmethod luvcraft:luvcraft-session-camera ((viewer luvcraft-viewer))
  (let* ((camera (render:viewer-camera viewer))
         (old (luvcraft:luvcraft-session-camera
               (presentation-environment (companion-presentation viewer)))))
    (setf (luvcraft:camera-position old) (presentation-position viewer (render:camera-position camera))
          (luvcraft:camera-yaw old) (- (/ pi 2) (render::camera-yaw camera))
          (luvcraft:camera-pitch old) (render::camera-pitch camera)
          (luvcraft:camera-field-of-view old) (render:camera-field-of-view camera))
    old))

(defmethod luvcraft:add-luvcraft-overlay ((viewer luvcraft-viewer) overlay)
  (luv.application-agent:call-in-application-frame
   viewer (lambda () (render:add-viewer-overlay viewer overlay))))
(defmethod luvcraft:remove-luvcraft-overlay ((viewer luvcraft-viewer) overlay &key (release-p t))
  (luv.application-agent:call-in-application-frame
   viewer (lambda ()
            (if release-p (render:remove-viewer-overlay viewer overlay)
                (setf (render:viewer-overlays viewer)
                      (remove overlay (render:viewer-overlays viewer) :test #'eq))))))
(defmethod luvcraft:unfocus-luvcraft-session ((viewer luvcraft-viewer))
  (render:unfocus-viewer viewer))
(defmethod luvcraft:focus-luvcraft-session ((viewer luvcraft-viewer) focus)
  (render:focus-viewer viewer focus))
(defmethod luvcraft:request-luvcraft-session-checkpoint ((viewer luvcraft-viewer))
  (render:save-viewer-world viewer))

;;; Forward lifecycle messages for actual old overlays, not arbitrary host
;;; objects. In particular the host's no-op defaults remain genuinely neutral.
(defmacro bridge-companion-overlay (class)
  `(progn
     (defmethod render:viewer-overlay-stage ((overlay ,class))
       (luvcraft:luvcraft-overlay-stage overlay))
     (defmethod render:refresh-viewer-overlay ((overlay ,class) (viewer luvcraft-viewer))
       (luvcraft:refresh-luvcraft-overlay overlay viewer))
     (defmethod render:encode-viewer-overlay ((overlay ,class) (viewer luvcraft-viewer) pass surface)
       (luvcraft:encode-luvcraft-overlay overlay viewer pass surface))
     (defmethod render:handle-viewer-overlay-event ((overlay ,class) (viewer luvcraft-viewer) canvas event)
       (luvcraft:handle-luvcraft-overlay-event overlay viewer canvas event))
     (defmethod render:viewer-overlay-focus-insets ((overlay ,class) (viewer luvcraft-viewer))
       (luvcraft:luvcraft-overlay-focus-insets overlay viewer))
     (defmethod render:quiesce-viewer-overlay ((overlay ,class))
       (luvcraft::quiesce-luvcraft-overlay overlay))
     (defmethod render:release-viewer-overlay ((overlay ,class))
       (luvcraft:release-luvcraft-overlay overlay))
     (defmethod render:evict-viewer-overlay-frame-key ((overlay ,class) key)
       (luvcraft:evict-luvcraft-overlay-frame-key overlay key))))

(bridge-companion-overlay mcluv:luvcraft-widget-overlay)
(bridge-companion-overlay luvcraft.agent::sdf-agent-body-overlay)
(bridge-companion-overlay luvcraft.agent::companion-body-overlay)
(bridge-companion-overlay luvcraft.agent:embodied-agent)

(defmethod render:viewer-overlay-stage ((overlay luvcraft.agent::companion-body-overlay))
  (declare (ignore overlay))
  :temporal-scene)

(defun encode-temporal-companion-body (overlay viewer pass surface renderer-frame)
  (let* ((agent (luvcraft.agent::companion-body-agent overlay))
         (mesh (luvcraft.agent::companion-body-mesh overlay))
         (kind (luvcraft.agent::embodied-agent-save-kind agent)))
    (when (luvcraft.agent::companion-body-visible-p agent viewer)
      (let* ((presentation (companion-presentation viewer))
             (device (render:viewer-device viewer))
             (program (ensure-temporal-companion-program presentation device))
             (pose (luvcraft.agent::companion-kinematics agent))
             (palette (if (and pose (eq mesh (luvcraft.agent::companion-pose-mesh pose)))
                          (luvcraft.agent::companion-pose-palette pose)
                          (luvcraft.agent::sample-agent-palette
                           agent mesh (luvcraft.agent::companion-body-palette overlay))))
             (state (or (gethash overlay (presentation-temporal-bodies presentation))
                        (setf (gethash overlay (presentation-temporal-bodies presentation))
                              (make-temporal-companion-state
                               :previous-palette (make-array (length palette)
                                                             :element-type 'single-float
                                                             :initial-element 0.0)))))
             (frame-index (render::renderer-frame-index (render:viewer-renderer viewer)))
             (continuous-p (= (temporal-companion-state-last-frame state) (1- frame-index)))
             (prev-palette (if continuous-p
                               (temporal-companion-state-previous-palette state)
                               palette))
             (origin (host-position viewer (luvcraft.agent::embodied-agent-position agent)))
             (yaw (luvcraft.agent::embodied-agent-facing-yaw agent))
             (instance (temporal-companion-state-instance state))
             (prev-instance (temporal-companion-state-previous-instance state))
             (camera-buffer (render::renderer-frame-state-camera-buffer renderer-frame))
             (gpu-frame
               (luv:canvas-frame-resource
                (temporal-companion-state-frames state) (render:viewer-context viewer) surface
                (lambda (key target)
                  (declare (ignore key target))
                  (flet ((buffer (label floats usage)
                           (luv:create device (luv:make-buffer-descriptor
                                               :label label :size (* 4 floats) :usage usage))))
                    (make-temporal-companion-frame
                     :palette (buffer "Temporal companion palette" (length palette)
                                      '(:storage :copy-dst))
                     :previous-palette (buffer "Temporal companion previous palette" (length palette)
                                               '(:storage :copy-dst))
                     :instance (buffer "Temporal companion instance" 16
                                       '(:vertex :copy-dst))))))))
        (unless (eq camera-buffer (temporal-companion-frame-camera-buffer gpu-frame))
          (when (temporal-companion-frame-binding gpu-frame)
            (luv:destroy (temporal-companion-frame-binding gpu-frame)))
          (setf (temporal-companion-frame-binding gpu-frame)
                (render:make-program-binding
                 program device
                 :camera-state camera-buffer
                 :palette (temporal-companion-frame-palette gpu-frame)
                 :previous-palette (temporal-companion-frame-previous-palette gpu-frame))
                (temporal-companion-frame-camera-buffer gpu-frame) camera-buffer))
        (setf (aref instance 0) (coerce (vec3:vec3-x origin) 'single-float)
              (aref instance 1) (coerce (vec3:vec3-y origin) 'single-float)
              (aref instance 2) (coerce (vec3:vec3-z origin) 'single-float)
              (aref instance 3) (luvcraft.agent::companion-stature kind)
              (aref instance 4) (coerce (sin yaw) 'single-float)
              (aref instance 5) (coerce (cos yaw) 'single-float)
              (aref instance 6) (luvcraft.agent::companion-rim-light kind)
              (aref instance 7) 0.0)
        (if continuous-p
            (replace instance prev-instance :start1 8 :end1 16)
            (replace instance instance :start1 8 :end1 16 :start2 0 :end2 8))
        (luv:write-buffer (temporal-companion-frame-palette gpu-frame) palette)
        (luv:write-buffer (temporal-companion-frame-previous-palette gpu-frame) prev-palette)
        (luv:write-buffer (temporal-companion-frame-instance gpu-frame) instance)
        (luv:set-pipeline pass (render::program-pipeline program))
        (luv:set-vertex-buffer pass 0 (luvcraft.agent::companion-body-vertex-buffer overlay))
        (luv:set-vertex-buffer pass 1 (temporal-companion-frame-instance gpu-frame))
        (luv:set-bind-group pass 0 (temporal-companion-frame-binding gpu-frame))
        (luv:draw-indexed pass (luvcraft.agent::companion-body-indices overlay) :uint32
                          (length (luvcraft.agent::companion-mesh-indices mesh)))
        (replace (temporal-companion-state-previous-palette state) palette)
        (replace prev-instance instance :start2 0 :end2 8)
        (setf (temporal-companion-state-last-frame state) frame-index)))))

(defmethod render:encode-viewer-overlay
    ((overlay luvcraft.agent::companion-body-overlay) (viewer luvcraft-viewer) pass surface)
  (if render::*renderer-current-frame*
      (encode-temporal-companion-body overlay viewer pass surface render::*renderer-current-frame*)
      (luvcraft:encode-luvcraft-overlay overlay viewer pass surface)))

(defmethod render:release-viewer-overlay :before ((overlay luvcraft.agent::companion-body-overlay))
  (let* ((agent (luvcraft.agent::companion-body-agent overlay))
         (viewer (and agent (luvcraft.agent::gnome-session agent)))
         (presentation (and (typep viewer 'luvcraft-viewer)
                            (find-if (lambda (o) (typep o 'companion-presentation))
                                     (render:viewer-overlays viewer))))
         (state (and presentation (gethash overlay (presentation-temporal-bodies presentation)))))
    (when state
      (remhash overlay (presentation-temporal-bodies presentation))
      (release-temporal-companion-state state))))

(defparameter *companion-sdf-bodies-p* nil
  "Temporary default while world SDF characters cause corrupt flashing tiles.
Hands and triangle-based creatures use independent render paths. #O9H3CW")

(defmethod render:encode-viewer-overlay :around
    ((body luvcraft.agent::sdf-agent-body-overlay) (viewer luvcraft-viewer) pass surface)
  (when *companion-sdf-bodies-p*
    (call-next-method)))

(defmethod render:viewer-focus-score
    ((overlay mcluv:luvcraft-widget-overlay) (viewer luvcraft-viewer))
  (luvcraft:luvcraft-focus-score overlay viewer))
(defmethod render::viewer-world-panel-depth
    ((overlay mcluv:luvcraft-world-widget-overlay) (viewer luvcraft-viewer))
  (luvcraft:luvcraft-world-panel-depth overlay viewer))
(defmethod render:viewer-focus-entered
    ((overlay mcluv:luvcraft-widget-overlay) (viewer luvcraft-viewer))
  (luvcraft:luvcraft-focus-entered overlay viewer))
(defmethod render:viewer-focus-left
    ((overlay mcluv:luvcraft-widget-overlay) (viewer luvcraft-viewer))
  (luvcraft:luvcraft-focus-left overlay viewer))
(defmethod render:handle-viewer-focus-event
    ((overlay mcluv:luvcraft-widget-overlay) (viewer luvcraft-viewer) canvas event)
  (luvcraft:handle-luvcraft-focus-event overlay viewer canvas event))

(defmethod render:refresh-viewer-overlay :after
    ((overlay luvcraft.agent::sdf-agent-body-overlay) (viewer luvcraft-viewer))
  (luv:refresh-live-shader-pipeline (luvcraft.agent::gnome-body-pipeline overlay)))

(defmethod mcluv::luvcraft-widget-target-format
    ((overlay mcluv:luvcraft-hud-widget-overlay) (viewer luvcraft-viewer) surface)
  (declare (ignore overlay))
  (if surface (luv:gpu-texture-format surface) (luv:canvas-format (render:viewer-context viewer))))
(defmethod mcluv::luvcraft-widget-render-target
    ((overlay mcluv:luvcraft-hud-widget-overlay) (viewer luvcraft-viewer) surface)
  (declare (ignore overlay viewer)) surface)
(defmethod mcluv::luvcraft-widget-render-target
    ((overlay mcluv:luvcraft-world-widget-overlay) (viewer luvcraft-viewer) surface)
  (declare (ignore overlay surface))
  (render::renderer-panel-color-texture (render:viewer-renderer viewer)))

(defmethod luvcraft.agent::sdf-agent-pipeline-layout ((viewer luvcraft-viewer))
  (presentation-layout (companion-presentation viewer)))

(defmethod luvcraft.agent::sdf-agent-scene-bind-group ((viewer luvcraft-viewer) surface)
  (first (companion-frame viewer surface)))

(defmethod luvcraft.agent::sdf-agent-frame-uniform-buffer ((viewer luvcraft-viewer) surface)
  (second (companion-frame viewer surface)))

(defun companion-frame (viewer surface)
  "SURFACE's companion scene bind group and uniform buffer, freshly written."
  (let* ((state (companion-presentation viewer))
         (frame
           (luv:canvas-frame-resource
            (presentation-frames state) (render:viewer-context viewer) surface
            (lambda (key target)
              (declare (ignore key target))
              (let* ((device (render:viewer-device viewer))
                     (buffer (luv:create device (luv:make-buffer-descriptor
                                                 :label "companion frame" :size (* 4 76)
                                                 :usage '(:uniform :copy-dst))))
                     (group (luv:create device (luv:make-bind-group-descriptor
                                                :layout (presentation-layout state)
                                                :entries `((:binding 2 :resource ,buffer))))))
                (list group buffer)))))
         (extent (render::renderer-render-extent (render:viewer-renderer viewer)))
         (environment (presentation-environment state)))
    ;; Identical camera and sky packer to the original game.
    (luvcraft:luvcraft-session-camera viewer)
    (let ((data (luvcraft::frame-uniform-data environment (first extent) (second extent))))
      (luv:write-buffer (second frame) data))
    frame))

;;; The simulation remains Luft's. These objects are the original gnome/cat/
;;; Fantus presentation and conversation state, backed by the same provider.
(defclass luft-companion ()
  ((inhabitant :initarg :inhabitant :reader presented-inhabitant)))
(defclass luft-gnome (luft-companion luvcraft.agent:gnome) ())
(defclass luft-cat (luft-companion luvcraft.agent:cat) ())
(defclass luft-fantus (luft-companion luvcraft.agent:fantus) ())

;; Embed the two CLIM image arrays in the compiled game, including standalone builds.
(defparameter *companion-portraits*
  (list :fantus (clim:make-pattern
                 #.(clim:read-bitmap-file (asdf:system-relative-pathname
                                           :luvcraft "luvcraft/companions/portraits/fantus.png")
                                          :format :png) nil)
        :gnome (clim:make-pattern
                #.(clim:read-bitmap-file (asdf:system-relative-pathname
                                          :luvcraft "luvcraft/companions/portraits/gnome.png")
                                         :format :png) nil)))

(defmethod luvcraft.agent::dialogue-portrait ((agent luft-companion))
  (getf *companion-portraits* (inhabitant-kind (presented-inhabitant agent))))

(defmethod luvcraft.agent::dialogue-follow-label ((agent luft-companion))
  (let ((viewer (luvcraft.agent::gnome-session agent)))
    (if (equal (following-player (presented-inhabitant agent)) (player-id (viewer-local-player viewer)))
        "Stay here" "Follow me")))

(clim:define-command (com-toggle-companion-follow :command-table luvcraft-world) ((agent 'luft-companion))
  (let* ((viewer (render:viewer-command-viewer)) (inhabitant (presented-inhabitant agent))
         (id (player-id (viewer-local-player viewer))))
    (when (eq agent (render:viewer-modal-focus viewer))
      (request-companion-follow (viewer-world viewer) inhabitant (unless (equal id (following-player inhabitant)) id))
      (when (string= "" (luvcraft.agent::gnome-draft agent))
        (render:unfocus-viewer viewer)
        (when (and (render:viewer-canvas viewer) (typep (render:viewer-mode viewer) 'render:first-person-mode))
          (luv:set-canvas-relative-pointer-mode (render:viewer-canvas viewer) t)
          (setf (render:viewer-pointer-captured-p viewer) t))))))

(defmethod luvcraft.agent::choose-dialogue-follow ((agent luft-companion))
  (clim:execute-frame-command (luvcraft.agent::gnome-session agent) (list 'com-toggle-companion-follow agent)))

(defmethod luvcraft.agent::companion-ground-height ((viewer luvcraft-viewer) x z ceiling floor)
  (multiple-value-bind (face cx cy cz point)
      (terrain:raycast-face (render:viewer-source viewer)
                            (host-position viewer (vec3:make-vec3 x ceiling z))
                            (vec3:make-vec3 0 0 -1) :max-distance (- ceiling floor))
    (declare (ignore cx cy cz))
    (when (and face (= (luft:site-extent face) luft:+xy-face-extent+) (luft:site-positive-p face))
      (vec3:vec3-z point))))

(defun restore-companion-dialogue (agent inhabitant)
  "Retain saved conversation for focused reading without replaying subtitles."
  (let ((expired (- (get-internal-real-time)
                    (* (1+ luvcraft.agent::*dialogue-linger-seconds*)
                       internal-time-units-per-second))))
    (setf (luvcraft.agent::gnome-asked agent) (inhabitant-asked inhabitant)
          (luvcraft.agent::gnome-said agent) (inhabitant-said inhabitant)
          (luvcraft.agent::gnome-asked-at agent) expired
          (luvcraft.agent::gnome-said-at agent) expired))
  agent)

(defun ensure-inhabitant-presentation (viewer inhabitant)
  (or (inhabitant-presentation inhabitant)
      (let* ((p (presentation-position viewer (inhabitant-position inhabitant)))
             (character (inhabitant-character inhabitant))
             (yaw (atan (terrain:character-heading-x character)
                        (terrain:character-heading-y character)))
             (agent (make-instance (ecase (inhabitant-kind inhabitant)
                                     (:gnome 'luft-gnome) (:cat 'luft-cat) (:fantus 'luft-fantus))
                                   :inhabitant inhabitant :session viewer :position p
                                   :facing-yaw yaw
                                   :x (floor (vec3:vec3-x p)) :y (floor (vec3:vec3-y p))
                                   :z (floor (vec3:vec3-z p)))))
        (companion-presentation viewer)
        (setf (inhabitant-presentation inhabitant) agent)
        (restore-companion-dialogue agent inhabitant)
        (render:add-viewer-overlay viewer agent)
        (luvcraft.agent::ensure-embodied-agent-body agent)
        (when (inhabitant-provider inhabitant)
          (prepare-inhabitant-provider inhabitant (inhabitant-provider inhabitant)))
        agent)))

(defmethod prepare-inhabitant-provider ((inhabitant inhabitant) provider)
  (when (and (inhabitant-presentation inhabitant) (not (inhabitant-presentation-observer inhabitant)))
    (let ((observer (luvcraft.agent::make-gnome-observer (inhabitant-presentation inhabitant))))
      (setf (inhabitant-presentation-observer inhabitant) observer)
      (luv.application-agent:add-agent-observer provider observer))))

(defmethod luvcraft.agent::gnome-ask ((agent luft-companion) text)
  (setf (luvcraft.agent::gnome-asked agent) text
        (luvcraft.agent::gnome-asked-at agent) (get-internal-real-time))
  (let ((viewer (luvcraft.agent::gnome-session agent)))
    (tell-inhabitant viewer (presented-inhabitant agent) text
                     :asker (player-id (viewer-local-player viewer)))))

(defmethod render:advance-viewer-overlay ((agent luft-companion) (viewer luvcraft-viewer) seconds)
  (let* ((inhabitant (presented-inhabitant agent))
         (character (inhabitant-character inhabitant))
         (p (presentation-position viewer (inhabitant-position inhabitant)))
         ;; It faces whoever it attends to, else this view's player.
         (player (or (companion-attention-position (viewer-world viewer) inhabitant)
                     (terrain:body-position (terrain:character-body (render:viewer-player viewer))))))
    (setf (luvcraft.agent::embodied-agent-position agent) p
          (luvcraft.agent::embodied-agent-grounded-p agent)
          (terrain:body-grounded-p (terrain:character-body character))
          (luvcraft.agent::gnome-x agent) (floor (vec3:vec3-x p))
          (luvcraft.agent::gnome-y agent) (floor (vec3:vec3-y p))
          (luvcraft.agent::gnome-z agent) (floor (vec3:vec3-z p))
          (luvcraft.agent::embodied-agent-pending-agent-prompts agent)
          (and (eq :opening (inhabitant-activity inhabitant)) (list (inhabitant-asked inhabitant))))
    (unless (inhabitant-destination inhabitant)
      ;; Only turn toward the player when engaged (focused or mid-turn), so
      ;; idle companions keep their own heading instead of tracking the camera
      ;; like sprites when you walk around them.
      (when (and (or (eq agent (render:viewer-modal-focus viewer))
                     (not (eq :idle (inhabitant-activity inhabitant))))
                 (< (companion-distance (inhabitant-position inhabitant) player) 6))
        (let* ((delta-x (- (vec3:vec3-x player) (vec3:vec3-x (inhabitant-position inhabitant))))
               (delta-y (- (vec3:vec3-y player) (vec3:vec3-y (inhabitant-position inhabitant))))
               (yaw (atan delta-x delta-y))
               (old (luvcraft.agent::embodied-agent-facing-yaw agent))
               (turn (render::shortest-angle-difference yaw old))
               (step (* luvcraft.agent::*embodied-agent-turn-speed* seconds))
               (next (+ old (max (- step) (min step turn)))))
          (setf (luvcraft.agent::embodied-agent-facing-yaw agent) next
                (terrain:character-heading-x character) (sin next)
                (terrain:character-heading-y character) (cos next)))))
    (when (inhabitant-destination inhabitant)
      (setf (luvcraft.agent::embodied-agent-facing-yaw agent)
            (atan (terrain:character-heading-x character) (terrain:character-heading-y character))))))

(defmethod render:advance-viewer-overlay :after
    ((agent luft-companion) (viewer luvcraft-viewer) seconds)
  (luvcraft.agent::update-companion-gait agent seconds viewer))

(defmethod render:advance-viewer-game :before ((viewer luvcraft-viewer) seconds)
  (declare (ignore seconds))
  (let ((world (viewer-world viewer)))
    (initialize-world-agents world)
    (dolist (inhabitant (world-agents world)) (ensure-inhabitant-presentation viewer inhabitant))
    (present-waiting-proposal viewer)))

(defmethod render:viewer-focus-score ((agent luft-companion) (viewer luvcraft-viewer))
  (let ((camera (luvcraft:luvcraft-session-camera viewer)))
    (multiple-value-bind (right up forward) (luvcraft:camera-basis camera)
      (declare (ignore right up))
      (let ((distance (luvcraft.agent::embodied-agent-ray-distance
                       agent (luvcraft:camera-position camera) forward 8.0)))
        (when distance
          (multiple-value-bind (r u f) (render:camera-basis (render:viewer-camera viewer))
            (declare (ignore r u))
            (unless (terrain:raycast-face (render:viewer-source viewer)
                                          (render:camera-position (render:viewer-camera viewer)) f
                                          :max-distance distance)
              distance)))))))

(defmethod render:viewer-focus-entered ((agent luft-companion) (viewer luvcraft-viewer))
  (setf (viewer-conversation-agent viewer) (presented-inhabitant agent))
  (luvcraft:luvcraft-focus-entered agent viewer))
(defmethod render:viewer-focus-left ((agent luft-companion) (viewer luvcraft-viewer))
  (luvcraft:luvcraft-focus-left agent viewer))
(defmethod luvcraft.agent::conversation-sight-clear-p ((agent luft-companion) world from to)
  (declare (ignore world))
  ;; Cast outward from the companion so a camera embedded in a journaled
  ;; wall is rejected too. The native Luft scene owns those edits.
  (let* ((viewer (luvcraft.agent::gnome-session agent))
         (eye (host-position viewer from))
         (target (host-position viewer to))
         (direction (vec3:make-vec3 (- (vec3:vec3-x eye) (vec3:vec3-x target))
                                    (- (vec3:vec3-y eye) (vec3:vec3-y target))
                                    (- (vec3:vec3-z eye) (vec3:vec3-z target))))
         (distance (vec3:vec3-length direction)))
    (or (< distance 0.001)
        (null (terrain:raycast-face (render:viewer-source viewer) target direction
                                    :max-distance (+ distance 0.12))))))

(defmethod render:viewer-focus-camera-pose ((agent luft-companion) (viewer luvcraft-viewer))
  (let ((pose (luvcraft:luvcraft-focus-camera-pose agent viewer)))
    (render:make-camera-pose :position (host-position viewer (luvcraft::camera-pose-position pose))
                             :yaw (- (/ pi 2) (luvcraft::camera-pose-yaw pose))
                             :pitch (luvcraft::camera-pose-pitch pose)
                             :field-of-view (luvcraft::camera-pose-field-of-view pose))))
;; Focus is a text field: every letter types, approvals take Ctrl-Y/Ctrl-N.
(defmethod render:handle-viewer-focus-event ((agent luft-companion) (viewer luvcraft-viewer) canvas event)
  (luvcraft:handle-luvcraft-focus-event agent viewer canvas event))
