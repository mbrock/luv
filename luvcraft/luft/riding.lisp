(in-package #:luvcraft.luft.game)

;;; Original rides and bodies, with only the host/terrain seam translated.
(defmethod luvcraft:luvcraft-session-critters ((viewer luvcraft-viewer))
  (let ((population (make-instance 'luvcraft::critter-population)))
    (dolist (animal (viewer-animals viewer)) (luvcraft::add-critter population animal))
    population))

(defmethod luvcraft:luvcraft-session-movement-intent ((viewer luvcraft-viewer))
  (viewer-rider-intent viewer))

(defmethod luvcraft::carry-luvcraft-player ((viewer luvcraft-viewer) animal)
  (let ((player (render:viewer-player viewer)))
    (when player
      (let ((body (terrain:character-body player)))
        (setf (terrain:body-position body) (animal-luft-position animal)
              (terrain:body-velocity body) (vec3:make-vec3 0.0 0.0 0.0)
              (terrain:body-grounded-p body) t)))
    player))

(defmethod luvcraft::dismount-luvcraft-player ((viewer luvcraft-viewer) animal)
  (let* ((player (render:viewer-player viewer))
         (body (and player (terrain:character-body player)))
         (p (animal-luft-position animal))
         (solid (terrain::inspection-source-solid (render:viewer-source viewer))))
    (when body
      (loop for (dx dy) in luvcraft::*dismount-offsets*
            for x = (+ (vec3:vec3-x p) dx)
            for y = (+ (vec3:vec3-y p) dy)
            when (terrain::body-clear-at-p solid x y (vec3:vec3-z p)
                                           (terrain:body-height body) (terrain:body-radius body))
              do (setf (terrain:body-position body) (vec3:make-vec3 x y (vec3:vec3-z p)))
                 (return)))
    player))

(defmethod render:viewer-focus-carries-player-p ((ride luvcraft::critter-ride))
  (luvcraft::luvcraft-focus-carries-player-p ride))

(defmethod render:viewer-focus-entered ((ride luvcraft::critter-ride) (viewer luvcraft-viewer))
  (setf (viewer-ride-return-p viewer) nil)
  (luvcraft:clear-movement-intent (viewer-rider-intent viewer))
  (luvcraft::carry-luvcraft-player viewer (luvcraft::critter-ride-critter ride)))

(defmethod render:viewer-focus-left ((ride luvcraft::critter-ride) (viewer luvcraft-viewer))
  (setf (viewer-ride-return-p viewer) t)
  (luvcraft:clear-movement-intent (viewer-rider-intent viewer))
  (luvcraft::luvcraft-focus-left ride viewer))

(defmethod render:viewer-focus-return-camera-pose ((viewer luvcraft-viewer) origin)
  (when (and (viewer-ride-return-p viewer) (render:viewer-player viewer)
             (typep (render:viewer-mode viewer) 'render:first-person-mode))
    (let ((p (terrain:body-position (terrain:character-body (render:viewer-player viewer)))))
      (render:make-camera-pose
       :position (vec3:make-vec3 (vec3:vec3-x p) (vec3:vec3-y p)
                                 (+ (vec3:vec3-z p) render::*first-person-eye-height*))
       :yaw (render:camera-pose-yaw origin) :pitch (render:camera-pose-pitch origin)
       :field-of-view (render:camera-pose-field-of-view origin)))))

(defmethod render:handle-viewer-focus-event
    ((ride luvcraft::critter-ride) (viewer luvcraft-viewer) canvas event)
  (declare (ignore canvas))
  (when (typep event 'luv:canvas-key-event)
    (let ((direction (second (assoc (luv:canvas-key-event-key-name event)
                                    render::+viewer-movement-keys+))))
      (when direction
        (setf (luvcraft:movement-urging-p (viewer-rider-intent viewer) direction)
              (typep event 'luv:canvas-key-press-event)))))
  t)

(defmethod render:viewer-focus-camera-pose ((ride luvcraft::critter-ride) (viewer luvcraft-viewer))
  (let ((pose (luvcraft::luvcraft-focus-camera-pose ride viewer)))
    (render::make-camera-pose
     :position (host-position viewer (luvcraft::camera-pose-position pose))
     :yaw (- (/ pi 2) (luvcraft::camera-pose-yaw pose))
     :pitch (luvcraft::camera-pose-pitch pose)
     :field-of-view (luvcraft::camera-pose-field-of-view pose))))

;;; Every animal is a focus candidate, but the original ray/box intersection
;;; still decides which one the player sees, with Luft terrain as the occluder.
(defmethod render:viewer-focus-score ((ride luvcraft::critter-ride) (viewer luvcraft-viewer))
  (let* ((camera (render:viewer-camera viewer))
         (origin (render:camera-position camera)))
    (multiple-value-bind (right up forward) (render:camera-basis camera)
      (declare (ignore right up))
      (let ((distance (luvcraft:critter-ray-distance
                       (luvcraft::critter-ride-critter ride)
                       (presentation-position viewer origin)
                       (vec3:make-vec3 (vec3:vec3-x forward) (vec3:vec3-z forward)
                                       (vec3:vec3-y forward))
                       luvcraft::+luvcraft-target-reach+)))
        (when distance
          (multiple-value-bind (face x y z point)
              (terrain:raycast-face (render:viewer-source viewer) origin forward :max-distance distance)
            (declare (ignore x y z point))
            (unless face distance)))))))

(defun advance-viewer-ride (viewer seconds)
  (let ((focus (render:viewer-modal-focus viewer)))
    (when (typep focus 'luvcraft::critter-ride)
      ;; Match the original host: step the animal, then rein and carry.
      (luvcraft::advance-luvcraft-focus focus viewer seconds))))

(defun update-viewer-rides (viewer)
  (dolist (overlay (copy-list (render:viewer-overlays viewer)))
    (when (and (typep overlay 'luvcraft::critter-ride)
               (not (member (luvcraft::critter-ride-critter overlay) (viewer-animals viewer))))
      (render:remove-viewer-overlay viewer overlay)))
  (dolist (animal (viewer-animals viewer))
    (unless (find animal (render:viewer-overlays viewer)
                  :key (lambda (overlay)
                         (when (typep overlay 'luvcraft::critter-ride)
                           (luvcraft::critter-ride-critter overlay))))
      (render:add-viewer-overlay viewer (luvcraft::activate-luvcraft-critter animal viewer))))
  viewer)

(defmethod luv.knobs:knobs-for-host :around ((viewer luvcraft-viewer))
  (append (call-next-method)
          (mapcar #'luv.knobs:find-knob
                  '(luvcraft::ride-seat-lift luvcraft::ride-seat-offset luvcraft::ride-seat-pitch))))

;;; The same bounded SDF hands and palm poses as old Luvcraft.
(defmethod luvcraft::luvcraft-session-body ((viewer luvcraft-viewer)) (viewer-body viewer))
(defmethod luvcraft::player-body-pipeline-layout ((viewer luvcraft-viewer))
  (presentation-layout (companion-presentation viewer)))
(defmethod luvcraft::player-body-scene-bind-group ((viewer luvcraft-viewer) surface)
  (luvcraft.agent::sdf-agent-scene-bind-group viewer surface))

(bridge-companion-overlay luvcraft::player-body)

(defmethod render:encode-viewer-overlay :around
    ((body luvcraft::player-body) (viewer luvcraft-viewer) pass surface)
  (when (viewer-body-visible-p viewer)
    (call-next-method)))

(defmethod render:refresh-viewer-overlay ((body luvcraft::player-body) (viewer luvcraft-viewer))
  (luv:refresh-live-shader-pipeline (luvcraft::player-body-sdf-pipeline body)))

(defmethod render:advance-viewer-game :around ((viewer luvcraft-viewer) seconds)
  (multiple-value-prog1 (call-next-method)
    (advance-viewer-ride viewer seconds)
    (update-viewer-rides viewer)
    (let* ((body (viewer-body viewer))
           (player (render:viewer-player viewer))
           (environment (and player (render:viewer-device viewer)
                             (presentation-environment (companion-presentation viewer)))))
      (when environment
        (unless (luvcraft::player-body-sdf-pipeline body) (luvcraft::attach-player-body-sdf viewer))
        (when (and (viewer-ball-in-hand-p viewer)
                   (not (find-if (lambda (overlay) (typep overlay 'held-item-drawing))
                                 (render:viewer-overlays viewer))))
          (render:add-viewer-overlay viewer (make-instance 'held-item-drawing)))
        ;; A data-only Y-up player feeds the original bob/landing animation;
        ;; it never asks the old world's collision solver to move anything.
        (let* ((physical (terrain:character-body player))
               (v (terrain:body-velocity physical))
               (proxy (or (luvcraft:luvcraft-session-player environment)
                          (setf (slot-value environment 'luvcraft::player)
                                (make-instance 'luvcraft::block-world-player)))))
          (setf (luvcraft::player-velocity proxy)
                (vec3:make-vec3 (vec3:vec3-x v) (vec3:vec3-z v) (vec3:vec3-y v))
                (luvcraft::player-walk-speed proxy) (coerce (terrain:character-speed player) 'double-float)
                (luvcraft::player-grounded-p proxy) (terrain:body-grounded-p physical)
                (luvcraft::player-body-hand-item body)
                (when (viewer-ball-in-hand-p viewer) (viewer-ball-hand viewer)))
          (luvcraft::advance-player-body body environment seconds :place-view-p nil))))))

(defmethod render::place-viewer-at-player-eyes :after ((viewer luvcraft-viewer))
  (when (render:viewer-player viewer)
    (multiple-value-bind (sway lift) (luvcraft::player-body-view-offset (viewer-body viewer))
      (let* ((camera (render:viewer-camera viewer))
             (p (render:camera-position camera)) (yaw (render:camera-yaw camera)))
        (incf (vec3:vec3-x p) (* sway (sin yaw)))
        (incf (vec3:vec3-y p) (* sway (cos yaw)))
        (incf (vec3:vec3-z p) lift)))))
