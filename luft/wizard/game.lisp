(in-package #:luft.wizard)

;;; The game: one canvas, one renderer, one streamed isle, one walking
;;; character drawn as the renderer's robed figure, and an isometric camera
;;; that follows him.  A click on the top of a cell walks him there.  Q and E
;;; turn the view an eighth, the wheel zooms, R returns him to the path.
;;;
;;; Everything below is a client of the engine: the terrain streams and
;;; answers the ray and the route, the simulation steps the body, and the
;;; renderer turns the camera and the figure into a frame.

(defvar *wizard* nil "The running wizard game, if any.")

(defparameter *wizard-camera-distance* 40.0
  "How far back along its view the isometric camera stands from the wizard.")

(defparameter *wizard-camera-pitch* -0.5165006
  "The isometric camera's fixed downward pitch: the classic dimetric view.")

(defparameter *wizard-bevel-width* 1
  "The bevel the isle is meshed with, which its frame must agree with.")

(defclass wizard ()
  ((canvas :initarg :canvas :reader wizard-canvas)
   (device :initarg :device :reader wizard-device)
   (context :initarg :context :reader wizard-context)
   (renderer :initarg :renderer :reader wizard-renderer)
   (production-system :initarg :production-system
                      :reader wizard-production-system)
   (simulation :initarg :simulation :reader wizard-simulation)
   (character :initarg :character :accessor wizard-character)
   (camera :initarg :camera :reader wizard-camera)
   (isometric-height :initarg :isometric-height :initform 18.0
                     :accessor wizard-isometric-height)
   (hour :initarg :hour :initform 16.5 :accessor wizard-hour
         :documentation "The fixed time of day the isle is shown at.")
   (last-timestamp :initform nil :accessor wizard-last-timestamp)
   (surface-views :initform (make-hash-table :test #'equal)
                  :reader wizard-surface-views)
   (running-p :initform t :accessor wizard-running-p)
   (stop-controller :initarg :stop-controller :reader wizard-stop-controller))
  (:documentation "The isometric wizard game: an isle, a walker, a camera."))

(defun wizard-scene (wizard)
  (terrain:simulation-source (wizard-simulation wizard)))

(defun make-wizard-character (scene)
  "A walker standing at SCENE's world-source spawn."
  (multiple-value-bind (x y z yaw)
      (terrain:world-source-spawn (terrain:streaming-scene-source scene))
    (terrain:make-walking-character
     :position (make-vec3 (+ x 0.5) (+ y 0.5) (coerce z 'single-float))
     :heading-x (cos yaw) :heading-y (sin yaw))))

;;; The camera.

(defun follow-wizard (wizard &optional seconds)
  "Aim the camera at the wizard's chest, easing toward it over SECONDS."
  (let* ((camera (wizard-camera wizard))
         (feet (terrain:body-position
                (terrain:character-body (wizard-character wizard))))
         (forward (nth-value 2 (render:camera-basis camera)))
         (position (render:camera-position camera))
         (blend (if seconds (- 1.0 (exp (* -6.0 seconds))) 1.0)))
    (flet ((ease (current aim component)
             (+ current
                (* blend (- (- aim (* *wizard-camera-distance* component))
                            current)))))
      (setf (vec3-x position)
            (ease (vec3-x position) (vec3-x feet) (vec3-x forward))
            (vec3-y position)
            (ease (vec3-y position) (vec3-y feet) (vec3-y forward))
            (vec3-z position)
            (ease (vec3-z position) (+ (vec3-z feet) 1.45) (vec3-z forward)))))
  wizard)

(defun turn-wizard-view (wizard eighths)
  "Turn the isometric view by EIGHTHS of a circle, easing into it."
  (let ((camera (wizard-camera wizard)))
    (render:target-camera-yaw
     camera (+ (render:camera-target-yaw camera) (* eighths (/ pi 4))))))

(defun wizard-logical-extent (wizard)
  (let ((canvas (wizard-canvas wizard)))
    (list (canvas-width canvas) (canvas-height canvas))))

(defun wizard-pointer-ray (wizard x y)
  "The world ray through logical point X, Y of the wizard's window."
  (destructuring-bind (width height) (wizard-logical-extent wizard)
    (let ((render:*projection* :isometric)
          (render:*isometric-height* (wizard-isometric-height wizard)))
      (render:camera-pointer-ray (wizard-camera wizard) width height x y))))

(defun wizard-pointed-cell (wizard x y)
  "The foot cell under logical point X, Y, or NIL where nothing stands."
  (multiple-value-bind (origin direction) (wizard-pointer-ray wizard x y)
    (terrain:raycast-standing-cell (wizard-scene wizard) origin direction)))

(defun wizard-click (wizard x y)
  "Walk the wizard to the cell top under logical point X, Y.

Return the route, whose status says whether the terrain found a way, or
NIL when the point names nowhere to stand."
  (multiple-value-bind (cx cy cz) (wizard-pointed-cell wizard x y)
    (when cx
      (terrain:start-character-route (wizard-character wizard)
                                     (wizard-scene wizard) cx cy cz))))

(defun reset-wizard (wizard)
  "Return the wizard to his spawn on the path."
  (let ((simulation (wizard-simulation wizard))
        (spawn (make-wizard-character (wizard-scene wizard))))
    (terrain:remove-simulation-character simulation (wizard-character wizard))
    (setf (wizard-character wizard) spawn)
    (terrain:add-simulation-character simulation spawn)
    (follow-wizard wizard)
    (setf (render:renderer-history-valid-p (wizard-renderer wizard)) nil)
    wizard))

;;; The world's clock and its streaming.

(defun wizard-resident-p (wizard)
  "Whether the ground under the wizard is resident, so he may move."
  (let ((feet (terrain:body-position
               (terrain:character-body (wizard-character wizard)))))
    (not (eq :unknown-nonresident
             (terrain:streaming-scene-cell-state
              (wizard-scene wizard)
              (floor (vec3-x feet)) (floor (vec3-y feet))
              (max 0 (floor (vec3-z feet))))))))

(defun advance-wizard-streaming (wizard)
  "Publish finished chunks and demand those around the wizard."
  (let ((scene (wizard-scene wizard))
        (production-system (wizard-production-system wizard)))
    (render:drain-streaming-scene-production
     scene (wizard-renderer wizard) production-system)
    (when (null (terrain:streaming-scene-replacement scene))
      (let ((feet (terrain:body-position
                   (terrain:character-body (wizard-character wizard)))))
        (terrain:retarget-streaming-scene
         scene production-system *wizard-bevel-width*
         (vec3-x feet) (vec3-y feet))))))

(defun advance-wizard (wizard timestamp)
  "Step the world and the camera to TIMESTAMP, in seconds."
  (let* ((last (wizard-last-timestamp wizard))
         (dt (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0)))
    (setf (wizard-last-timestamp wizard) timestamp)
    (render:advance-camera-response (wizard-camera wizard) dt)
    (render:advance-sky-clock dt)
    ;; No one moves until the ground under him has been raised.
    (when (wizard-resident-p wizard)
      (terrain:advance-world-simulation (wizard-simulation wizard) dt))
    (follow-wizard wizard (and last dt))))

;;; The frame.

(defun wizard-surface-view (wizard surface)
  "The wizard's texture view of presentation slot SURFACE."
  (let* ((key (canvas-frame-resource-key (wizard-context wizard) surface))
         (views (wizard-surface-views wizard))
         (view (gethash key views)))
    (when (and view (not (eq surface (gpu-texture-view-texture view))))
      ;; A backend may hand back a fresh wrapper for a revisited slot.
      (destroy view)
      (setf view nil))
    (or view
        (setf (gethash key views)
              (create (wizard-device wizard)
                      (make-texture-view-descriptor :texture surface))))))

(defun release-wizard-surface-view (wizard surface)
  (let* ((key (canvas-frame-resource-key (wizard-context wizard) surface))
         (view (gethash key (wizard-surface-views wizard))))
    (when view (destroy view))
    (remhash key (wizard-surface-views wizard))))

(defun wizard-figure (wizard)
  "The wizard's pose as the renderer's figure lanes."
  (let* ((character (wizard-character wizard))
         (body (terrain:character-body character)))
    (render:figure-lanes
     (terrain:body-position body) (terrain:body-previous-position body)
     (terrain:character-gait character) (terrain:character-previous-gait character)
     (terrain:character-heading-x character) (terrain:character-heading-y character)
     (terrain:character-previous-heading-x character)
     (terrain:character-previous-heading-y character))))

(defun encode-wizard-frame (wizard encoder surface extent)
  "Encode the isle and the wizard at the game's hour and zoom."
  (let ((render:*isometric-height* (wizard-isometric-height wizard))
        (render:*sky-hour* (wizard-hour wizard)))
    (render:encode-renderer-view
     (wizard-renderer wizard) (wizard-context wizard) encoder surface
     (wizard-surface-view wizard surface) extent (wizard-camera wizard)
     :projection :isometric
     :figure (wizard-figure wizard)
     :bevel-width *wizard-bevel-width*
     :effect-time (or (wizard-last-timestamp wizard) 0.0))))

(defun render-wizard-frame (wizard)
  (when (wizard-running-p wizard)
    (advance-wizard-streaming wizard)
    (let* ((canvas (wizard-canvas wizard))
           (renderer (wizard-renderer wizard))
           (finish-timing (render:start-renderer-gpu-timing
                           renderer canvas (canvas-time-unadjusted canvas))))
      (present-canvas-frame
       (wizard-context wizard)
       (lambda (surface encoder presentation-time)
         (advance-wizard wizard presentation-time)
         (encode-wizard-frame wizard encoder surface
                              (canvas-extent (wizard-context wizard)))))
      (when finish-timing (funcall finish-timing))
      (render:pace-renderer-resolution renderer canvas))))

;;; Input.

(defmethod handle-canvas-event ((wizard wizard) canvas (event canvas-pointer-button-press-event))
  (declare (ignore canvas))
  (when (eq :left (canvas-pointer-event-button event))
    (wizard-click wizard (canvas-pointer-event-x event)
                  (canvas-pointer-event-y event)))
  nil)

(defmethod handle-canvas-event ((wizard wizard) canvas (event canvas-pointer-wheel-event))
  (declare (ignore canvas))
  (setf (wizard-isometric-height wizard)
        (max 8.0 (min 64.0 (* (wizard-isometric-height wizard)
                              (expt 1.1 (- (canvas-pointer-event-scroll-y event)))))))
  nil)

(defmethod handle-canvas-event ((wizard wizard) canvas (event canvas-key-press-event))
  (declare (ignore canvas))
  (unless (canvas-key-event-repeat-p event)
    (case (canvas-key-event-key-name event)
      (:q (turn-wizard-view wizard -1))
      (:e (turn-wizard-view wizard 1))
      (:r (reset-wizard wizard))))
  nil)

(defmethod handle-canvas-event ((wizard wizard) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas))
  (request-controlled-stop (wizard-stop-controller wizard)
                           (lambda () (%stop-wizard wizard))
                           :thread-name "wizard stop")
  :defer-canvas-close)

(defmethod handle-canvas-event ((wizard wizard) canvas event)
  (declare (ignore canvas event))
  nil)

;;; Screenshots through luv's shared offscreen capture.

(defmethod capture-canvas ((wizard wizard))
  (wizard-canvas wizard))

(defmethod advance-capture-frame ((wizard wizard) capture frame-index)
  (declare (ignore capture frame-index))
  (advance-wizard-streaming wizard))

(defmethod encode-capture-frame ((wizard wizard) capture encoder target extent)
  (declare (ignore capture))
  (encode-wizard-frame wizard encoder target extent))

(defmethod cleanup-capture ((wizard wizard) capture)
  (let ((target (capture-target capture))
        (canvas (wizard-canvas wizard)))
    (when (and target (eq :open (canvas-state canvas)))
      (request-canvas-frame
       canvas (lambda (timestamp)
                (declare (ignore timestamp))
                (release-wizard-surface-view wizard target)))))
  (values))

(defun capture-wizard-frame (pathname &optional (wizard *wizard*))
  "Render one wizard frame offscreen into the PNG at PATHNAME."
  (capture-application-screenshot wizard (merge-pathnames pathname)
                                  :label "wizard screenshot"))

;;; Lifecycle.

(defun start-wizard (&key (source (make-wizard-world-source))
                       (width 1100) (height 800) fullscreen-p
                       (provider *gpu-provider*) (frames-per-second 60))
  "Open the isometric wizard on SOURCE's isle and return the game."
  (when *wizard*
    (error "The wizard is already playing: ~A" *wizard*))
  (let ((canvas (make-sdl-canvas
                 :title "Wizard — click to walk · Q/E turn · wheel zoom · R return"
                 :width width :height height :visible-p nil
                 :fullscreen-p fullscreen-p :high-pixel-density-p t
                 :presentation-api (sdl-presentation-api-for provider)))
        (device nil)
        (renderer nil)
        (production-system nil)
        (wizard nil))
    (open-canvas canvas)
    (handler-bind ((serious-condition
                     (lambda (condition)
                       (declare (ignore condition))
                       (unless wizard
                         (when production-system
                           (ignore-errors
                            (production:stop-production-system production-system)))
                         (when renderer (ignore-errors (render:destroy-renderer renderer)))
                         (ignore-errors (close-canvas canvas))
                         (when device (ignore-errors (destroy device)))))))
      (setf device (request-gpu-device
                    provider (make-device-descriptor :label "wizard")))
      (let* ((context (make-canvas-context
                       canvas provider
                       ;; :COPY-SRC lets the offscreen capture read frames back.
                       (make-canvas-configuration
                        :device device :usage '(:render-attachment :copy-src))))
             (scene (terrain:make-world-source-streaming-scene
                     source :mesh-preparer render:+render-population-preparer+))
             (simulation (terrain:make-world-simulation scene))
             (character (make-wizard-character scene))
             (camera (make-instance 'render:fly-camera
                                    :position (make-vec3 0.0 0.0 0.0)
                                    :yaw 2.2455373 :pitch *wizard-camera-pitch*)))
        (setf renderer (render:make-renderer
                        device (canvas-format context) (canvas-extent context)
                        :lattice-factory nil)
              production-system (production:make-single-worker-production-system
                                 :name "wizard mesh producer"))
        (terrain:add-simulation-character simulation character)
        (setf wizard (make-instance 'wizard
                                    :canvas canvas :device device :context context
                                    :renderer renderer
                                    :production-system production-system
                                    :simulation simulation :character character
                                    :camera camera
                                    :stop-controller (make-canvas-stop-controller
                                                      canvas :name "wizard")))
        (follow-wizard wizard)
        (setf (canvas-event-handler canvas) wizard)
        (show-canvas canvas)
        (setf (canvas-clock canvas)
              (make-cadence-clock (lambda (native-canvas timestamp)
                                    (declare (ignore native-canvas timestamp))
                                    (render-wizard-frame wizard))
                                  :frames-per-second frames-per-second))
        (setf *wizard* wizard)))))

(defun %stop-wizard (wizard)
  "Release WIZARD in order: captures, the last frame, then its resources."
  (setf (wizard-running-p wizard) nil)
  (quiesce-application-captures wizard)
  (let ((canvas (wizard-canvas wizard)))
    (when (member (canvas-state canvas) '(:opening :open))
      (setf (canvas-clock canvas) (make-demand-clock))
      ;; A synchronous no-op after the clock changes is the frame boundary.
      (request-canvas-frame canvas (lambda (timestamp) (declare (ignore timestamp)))))
    (setf (canvas-event-handler canvas) nil)
    (production:stop-production-system (wizard-production-system wizard))
    (maphash (lambda (key view) (declare (ignore key)) (destroy view))
             (wizard-surface-views wizard))
    (clrhash (wizard-surface-views wizard))
    (render:destroy-renderer (wizard-renderer wizard))
    (when (member (canvas-state canvas) '(:opening :open))
      (close-canvas canvas))
    (destroy (wizard-device wizard)))
  (when (eq wizard *wizard*)
    (setf *wizard* nil))
  (values))

(defun stop-wizard (&optional (wizard *wizard*))
  "Close WIZARD's window and release its renderer, worker, and device, once."
  (when wizard
    (request-application-capture-shutdown wizard)
    (call-with-stop-controller (wizard-stop-controller wizard)
                               (lambda () (%stop-wizard wizard))))
  (values))
