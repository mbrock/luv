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
   (frames :initform (luv:make-canvas-frame-resource-cache) :reader presentation-frames)))

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
                                         :sky-clock (viewer-sky-clock viewer)
                                         :sky-profile (viewer-sky-profile viewer))))
        (render:add-viewer-overlay
         viewer (make-instance 'companion-presentation :layout layout
                                                      :environment environment)))))

(defmethod render:release-viewer-overlay ((state companion-presentation))
  (luv:destroy-canvas-frame-resource-cache (presentation-frames state) #'release-companion-frame)
  (luv:destroy (presentation-layout state)))

(defmethod render:evict-viewer-overlay-frame-key ((state companion-presentation) key)
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
(defclass terminal-framing-surface (luvcraft::terminal-surface)
  ((surface :initarg :surface :reader framing-surface)
   (viewer :initarg :viewer :reader framing-viewer)))

(defmethod luvcraft::terminal-surface-physical-width ((surface terminal-framing-surface))
  (render::terminal-surface-width (framing-surface surface)))

(defmethod luvcraft::terminal-surface-physical-height ((surface terminal-framing-surface))
  (render::terminal-surface-height (framing-surface surface)))

(defmethod luvcraft::terminal-surface-lower-left-point
    ((surface terminal-framing-surface) &optional (offset 0.006))
  (presentation-position
   (framing-viewer surface)
   (render::terminal-surface-point
    (framing-surface surface) 0 0 (- offset render::*terminal-surface-offset*))))

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
  (luvcraft-viewer-world viewer))
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
(bridge-companion-overlay luvcraft.agent:embodied-agent)

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
    (first frame)))

;;; The simulation remains Luft's. These objects are the original gnome/cat/
;;; Fantus presentation and conversation state, backed by the same provider.
(defclass luft-companion ()
  ((inhabitant :initarg :inhabitant :reader presented-inhabitant)))
(defclass luft-gnome (luft-companion luvcraft.agent:gnome) ())
(defclass luft-cat (luft-companion luvcraft.agent:cat) ())
(defclass luft-fantus (luft-companion luvcraft.agent:fantus) ())

(defun ensure-inhabitant-presentation (viewer inhabitant)
  (or (inhabitant-presentation inhabitant)
      (let* ((p (presentation-position viewer (inhabitant-position inhabitant)))
             (agent (make-instance (ecase (inhabitant-kind inhabitant)
                                     (:gnome 'luft-gnome) (:cat 'luft-cat) (:fantus 'luft-fantus))
                                   :inhabitant inhabitant :session viewer :position p
                                   :x (floor (vec3:vec3-x p)) :y (floor (vec3:vec3-y p))
                                   :z (floor (vec3:vec3-z p)))))
        (companion-presentation viewer)
        (setf (inhabitant-presentation inhabitant) agent
              (luvcraft.agent::gnome-asked agent) (inhabitant-asked inhabitant)
              (luvcraft.agent::gnome-said agent) (inhabitant-said inhabitant)
              (luvcraft.agent::gnome-said-at agent) (get-internal-real-time))
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
  (tell-inhabitant (luvcraft.agent::gnome-session agent) (presented-inhabitant agent) text))

(defmethod render:advance-viewer-overlay ((agent luft-companion) (viewer luvcraft-viewer) seconds)
  (let* ((inhabitant (presented-inhabitant agent))
         (character (inhabitant-character inhabitant))
         (p (presentation-position viewer (inhabitant-position inhabitant)))
         (player (terrain:body-position (terrain:character-body (render:viewer-player viewer)))))
    (setf (luvcraft.agent::embodied-agent-position agent) p
          (luvcraft.agent::gnome-x agent) (floor (vec3:vec3-x p))
          (luvcraft.agent::gnome-y agent) (floor (vec3:vec3-y p))
          (luvcraft.agent::gnome-z agent) (floor (vec3:vec3-z p))
          (luvcraft.agent::embodied-agent-pending-agent-prompts agent)
          (and (inhabitant-opening inhabitant) (list (inhabitant-asked inhabitant))))
    (unless (inhabitant-journey inhabitant)
      (when (< (companion-distance (inhabitant-position inhabitant) player) 6)
        (let* ((delta-x (- (vec3:vec3-x player) (vec3:vec3-x (inhabitant-position inhabitant))))
               (delta-y (- (vec3:vec3-y player) (vec3:vec3-y (inhabitant-position inhabitant))))
               (yaw (atan delta-x delta-y))
               (old (luvcraft.agent::embodied-agent-facing-yaw agent))
               (turn (render::shortest-angle-difference yaw old))
               (step (* luvcraft.agent::*embodied-agent-turn-speed* seconds)))
          (setf (luvcraft.agent::embodied-agent-facing-yaw agent)
                (+ old (max (- step) (min step turn)))))))
    (when (inhabitant-journey inhabitant)
      (setf (luvcraft.agent::embodied-agent-facing-yaw agent)
            (atan (terrain:character-heading-x character) (terrain:character-heading-y character))))))

(defmethod render:advance-viewer-game :before ((viewer luvcraft-viewer) seconds)
  (declare (ignore seconds))
  (initialize-viewer-agents viewer)
  (dolist (inhabitant (viewer-agents viewer)) (ensure-inhabitant-presentation viewer inhabitant)))

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
(defmethod render:viewer-focus-camera-pose ((agent luft-companion) (viewer luvcraft-viewer))
  (let ((pose (luvcraft:luvcraft-focus-camera-pose agent viewer)))
    (render:make-camera-pose :position (host-position viewer (luvcraft::camera-pose-position pose))
                             :yaw (- (/ pi 2) (luvcraft::camera-pose-yaw pose))
                             :pitch (luvcraft::camera-pose-pitch pose)
                             :field-of-view (luvcraft::camera-pose-field-of-view pose))))
(defmethod render:handle-viewer-focus-event ((agent luft-companion) (viewer luvcraft-viewer) canvas event)
  (let ((approval (inhabitant-approval (presented-inhabitant agent))))
    (if (and approval (typep event 'luv:canvas-key-press-event)
             (member (luv:canvas-key-event-key-name event) '(:y :n :t)))
        (case (luv:canvas-key-event-key-name event)
          (:y (luvcraft.agent:approve-tool-approval approval))
          (:n (luvcraft.agent:deny-tool-approval approval "Player declined."))
          (:t (render:unfocus-viewer viewer)
              (clim:execute-frame-command viewer '(com-talk-to-companion))))
        (luvcraft:handle-luvcraft-focus-event agent viewer canvas event))))
