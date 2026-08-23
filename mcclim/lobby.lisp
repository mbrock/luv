;;; Luvcraft's placement and lifecycle adapter for the shared lobby HUD.

(in-package #:mcluv)

(eval-when (:load-toplevel :execute)
  ;; The former adapter opened the detailed panel from an :AROUND method on
  ;; START-LUVCRAFT-LOBBY.  Deleting a DEFMETHOD from source does not retract
  ;; it from a durable image, so remove that obsolete method explicitly.  No
  ;; current component owns this coordinate: starting the radio is deliberately
  ;; independent from showing its optional panel.
  (when (and (fboundp 'luvcraft:start-luvcraft-lobby)
             (find-class 'luvcraft:luvcraft-session nil))
    (alexandria:when-let
        ((method
           (find-method
            #'luvcraft:start-luvcraft-lobby '(:around)
            (list (find-class 'luvcraft:luvcraft-session)) nil)))
      (remove-method #'luvcraft:start-luvcraft-lobby method))))

(defclass luvcraft-lobby-hud-overlay (luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-lobby-hud-overlay))
  (declare (ignore overlay))
  :hud)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-lobby-hud-overlay) session pass surface-texture)
  (declare (ignore pass))
  (prepare-direct-widget-overlay
   overlay session surface-texture
   (luv.lobby.mcclim:lobby-hud-screen-state
    (widget-overlay-frame overlay)
    (multiple-value-list
     (luv:canvas-logical-size
      (luvcraft:luvcraft-session-canvas session)))))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-lobby-hud-overlay) session)
  (declare (ignore session))
  (luv.lobby.mcclim:refresh-lobby-hud (widget-overlay-frame overlay))
  overlay)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-lobby-hud-overlay) session)
  (declare (ignore overlay session))
  nil)

(defun luvcraft-lobby-hud-overlay (session)
  (find-if (lambda (overlay)
             (typep overlay 'luvcraft-lobby-hud-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun %open-luvcraft-lobby-hud (session)
  (or (luvcraft-lobby-hud-overlay session)
      (let ((frame nil)
            (overlay nil)
            (transferred-p nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (let ((client
                       (or (luvcraft:luvcraft-session-lobby-client session)
                           (luvcraft:start-luvcraft-lobby session))))
                 (setf frame
                       (luv.lobby.mcclim:make-embedded-lobby-hud
                        client
                        (luvcraft:luvcraft-session-canvas session)
                        (luvcraft:luvcraft-session-context session)
                        (luvcraft:luvcraft-session-device session)
                        :title "luvcraft lobby"
                        ;; The Luvcraft overlay is itself the shared compositor.
                        :install-compositor-p nil)))
               (let ((mirror (luv.lobby.mcclim:lobby-hud-mirror frame)))
                 (setf overlay
                       (make-instance 'luvcraft-lobby-hud-overlay
                                      :session session :frame frame :mirror mirror)
                       (mirror-compositor mirror) overlay)
                 (setf transferred-p t)
                 (luvcraft:add-luvcraft-overlay session overlay))
               (setf completed-p t)
               overlay)
          (unless completed-p
            (when (and overlay
                       (member overlay
                               (luvcraft:luvcraft-session-overlays session)
                               :test #'eq))
              (ignore-errors
               (luvcraft:remove-luvcraft-overlay
                session overlay)))
            (when (and frame (not transferred-p))
              (ignore-errors
               (luv.lobby.mcclim:destroy-lobby-hud frame))))))))

(defun %close-luvcraft-lobby-hud (session)
  (dolist (overlay (copy-list (luvcraft:luvcraft-session-overlays session)))
    (when (typep overlay 'luvcraft-lobby-hud-overlay)
      (luvcraft:remove-luvcraft-overlay session overlay)))
  nil)

(defun open-luvcraft-lobby-hud (session)
  "Open the detailed panel at a frame boundary, preserving the live radio."
  (luv:request-canvas-frame
   (luvcraft:luvcraft-session-canvas session)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%open-luvcraft-lobby-hud session))))

(defun close-luvcraft-lobby-hud (session)
  "Hide the detailed panel at a frame boundary, preserving its live radio."
  (luv:request-canvas-frame
   (luvcraft:luvcraft-session-canvas session)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (%close-luvcraft-lobby-hud session))))

(defun toggle-luvcraft-lobby-hud (session)
  "Toggle the detailed panel atomically at SESSION's next frame boundary."
  (luv:request-canvas-frame
   (luvcraft:luvcraft-session-canvas session)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (if (luvcraft-lobby-hud-overlay session)
         (%close-luvcraft-lobby-hud session)
         (%open-luvcraft-lobby-hud session))
     t)))

(defmethod luvcraft:stop-luvcraft-lobby :around
    ((session luvcraft:luvcraft-session))
  ;; Attempt both sides even if one release fails.  The HUD is released while
  ;; the device is live; a later START receives exactly one fresh instrument.
  (luv:with-release-report
    (luv:releasing :lobby-hud
      ;; A live session may also be stopped interactively, outside the complete
      ;; application teardown path.  Cross its canvas boundary when available;
      ;; adapter-only lifecycle fixtures deliberately have no native canvas.
      (if (slot-boundp session 'luvcraft::canvas)
          (close-luvcraft-lobby-hud session)
          (%close-luvcraft-lobby-hud session)))
    (luv:releasing :lobby-radio
      (call-next-method)))
  nil)
