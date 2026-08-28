(in-package #:mcluv)

;;; The source-update workflow and retained panel are shared.  This file is
;;; only Luvcraft's fixed HUD attachment, focus, and lifecycle policy.

(defclass luvcraft-source-update-overlay (luvcraft-hud-widget-overlay) ())

(defmethod source-update-systems-for ((session luvcraft:luvcraft-session))
  (declare (ignore session))
  '("luvcraft"))

(defmethod source-update-title-for ((session luvcraft:luvcraft-session))
  (declare (ignore session))
  "Luvcraft source update")

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-source-update-overlay))
  (declare (ignore overlay))
  :hud)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-source-update-overlay) session pass surface-texture)
  (declare (ignore pass))
  (let* ((frame (widget-overlay-frame overlay))
         (canvas (luvcraft:luvcraft-session-canvas session)))
    (refresh-source-update frame)
    (prepare-source-update frame)
    (prepare-direct-widget-overlay
     overlay session surface-texture
     (source-update-screen-state
      frame (list (luv:canvas-width canvas) (luv:canvas-height canvas)))))
  overlay)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-source-update-overlay) session)
  (declare (ignore overlay session))
  nil)

(defmethod luvcraft:luvcraft-focus-camera-pose
    ((overlay luvcraft-source-update-overlay) session)
  (declare (ignore session))
  (let ((camera
          (luvcraft:luvcraft-session-camera
           (widget-overlay-session overlay))))
    (luvcraft::make-camera-pose
     (luvcraft::copy-camera-position (luvcraft:camera-position camera))
     (luvcraft:camera-yaw camera) (luvcraft:camera-pitch camera)
     luvcraft::+luvcraft-camera-vertical-field-of-view+)))

(defmethod luvcraft:luvcraft-focus-entered
    ((overlay luvcraft-source-update-overlay) session)
  (setf (luvcraft:luvcraft-session-pointer-capture-suspended-p session) nil)
  overlay)

(defmethod luvcraft:quiesce-luvcraft-overlay
    ((overlay luvcraft-source-update-overlay))
  (quiesce-source-update-session
   (source-update-frame-session (widget-overlay-frame overlay))))

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay luvcraft-source-update-overlay))
  (destroy-source-update (widget-overlay-frame overlay))
  overlay)

(defun find-luvcraft-source-update (session)
  (find-if (lambda (overlay)
             (typep overlay 'luvcraft-source-update-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun close-luvcraft-source-update (overlay session)
  "Close SESSION's source panel when no source operation is active."
  (let ((source-session
          (source-update-frame-session (widget-overlay-frame overlay))))
    (unless (source-update-busy-p source-session)
      (when (eq overlay (luvcraft:luvcraft-session-modal-focus session))
        (luvcraft:unfocus-luvcraft-session session))
      (luvcraft:remove-luvcraft-overlay session overlay)))
  nil)

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-source-update-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (when (eq :dismiss
            (handle-source-update-key-event
             (widget-overlay-frame overlay) event))
    (close-luvcraft-source-update overlay session))
  t)

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-source-update-overlay) session canvas
     (event luv:canvas-event))
  (declare (ignore overlay session canvas event))
  t)

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-source-update-overlay) session canvas
     (event luv:canvas-pointer-button-press-event))
  (declare (ignore session canvas))
  (alexandria:when-let
      ((uv (luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (destructuring-bind (width height)
        (widget-overlay-logical-size overlay)
      (handle-source-update-pointer-press
       (widget-overlay-frame overlay)
       (* (first uv) width) (* (second uv) height)
       (luv:canvas-pointer-event-button event))))
  t)

(defun open-luvcraft-source-update (session)
  "Attach, focus, then start SESSION's shared source-update workflow."
  (or (find-luvcraft-source-update session)
      (let ((frame nil)
            (overlay nil)
            (transferred-p nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (setf frame
                     (make-embedded-source-update
                      session
                      (luvcraft:luvcraft-session-canvas session)
                      (luvcraft:luvcraft-session-context session)
                      (luvcraft:luvcraft-session-device session)))
               (let ((mirror (source-update-mirror frame)))
                 (setf overlay
                       (make-instance
                        'luvcraft-source-update-overlay
                        :session session :frame frame :mirror mirror)
                       (mirror-compositor mirror) overlay))
               (setf transferred-p t)
               (luvcraft:add-luvcraft-overlay session overlay)
               (luvcraft:focus-luvcraft-session session overlay)
               (start-source-update frame)
               (setf completed-p t)
               overlay)
          (unless completed-p
            (cond
              ((and overlay
                    (member overlay
                            (luvcraft:luvcraft-session-overlays session)
                            :test #'eq))
               (ignore-errors
                (luvcraft:remove-luvcraft-overlay session overlay)))
              ((and overlay (not transferred-p))
               (ignore-errors (luvcraft:release-luvcraft-overlay overlay)))
              ((and frame (not transferred-p))
               (ignore-errors (destroy-source-update frame)))))))))
