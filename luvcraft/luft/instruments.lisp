;;; The old McCLIM instruments, hosted by Luft's native overlay/focus protocol.
(in-package #:luvcraft.luft.game)

(defmethod mcluv:instrument-canvas ((viewer luvcraft-viewer))
  (render::viewer-canvas viewer))
(defmethod mcluv:instrument-context ((viewer luvcraft-viewer))
  (render::viewer-context viewer))
(defmethod mcluv:instrument-device ((viewer luvcraft-viewer))
  (render::viewer-device viewer))
(defmethod mcluv:instrument-overlays ((viewer luvcraft-viewer))
  (render:viewer-overlays viewer))
(defmethod mcluv:instrument-focus ((viewer luvcraft-viewer))
  (render:viewer-modal-focus viewer))
(defmethod mcluv:focus-instrument ((viewer luvcraft-viewer) overlay)
  (render:focus-viewer viewer overlay))
(defmethod mcluv:unfocus-instrument ((viewer luvcraft-viewer))
  (render:unfocus-viewer viewer))
(defmethod mcluv:add-instrument ((viewer luvcraft-viewer) overlay)
  (render:add-viewer-overlay viewer overlay))
(defmethod mcluv:remove-instrument ((viewer luvcraft-viewer) overlay)
  (render:remove-viewer-overlay viewer overlay))
(defmethod mcluv:toggle-instrument-creative-mode ((viewer luvcraft-viewer))
  (setf (viewer-creative-p viewer) (not (viewer-creative-p viewer))))

(defparameter *status-diagnostics-p* nil
  "Include engine telemetry in the game status line when explicitly enabled.")

;; Ordinary play needs a short hint, not the engine's complete status dump.
(defmethod mcluv:status-bar-channels-for ((viewer luvcraft-viewer))
  (if *status-diagnostics-p*
      (let ((channels (call-next-method)))
        (list* (first channels) :player-hint (rest channels)))
      '(:application :player-hint :fps)))

(defmethod mcluv:status-bar-channel-label
    ((channel (eql :player-hint)) (viewer luvcraft-viewer))
  (declare (ignore channel viewer))
  nil)

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :player-hint)) (viewer luvcraft-viewer) bar)
  (declare (ignore channel bar))
  (unless (render:viewer-modal-focus viewer)
    (mcluv:palette-hint viewer)))

(defmethod render:viewer-focus-dismiss-key-p
    ((overlay mcluv:luvcraft-inventory-overlay) (viewer luvcraft-viewer) event)
  (declare (ignore viewer event))
  ;; In the picture-first browser Tab changes rows, and Escape cancels a
  ;; pending kit replacement before it closes. Its existing handler owns both.
  (not (mcluv::inventory-simple-p (mcluv:widget-overlay-frame overlay))))

;; Realize before the canvas starts: opening I never compiles a UI pipeline.
(defun attach-player-instruments (viewer)
  (mcluv:open-luvcraft-inventory viewer)
  (mcluv:unfocus-instrument viewer)
  (mcluv:open-luvcraft-hotbar viewer))
