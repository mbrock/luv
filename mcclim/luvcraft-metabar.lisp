;;; Luvcraft adapter for the shared retained-GPU metabar instrument.

(in-package #:mcluv)

;;; ---------------------------------------------------------------------
;;; Luvcraft's existing knob and action vocabulary.

(defmethod metabar-groups-for ((session luvcraft:luvcraft-session))
  (declare (ignore session))
  (luvcraft:knob-groups))

(defmethod metabar-controls-for
    ((session luvcraft:luvcraft-session) group)
  (declare (ignore session))
  (luvcraft:knobs-in-group group))

(defmethod metabar-actions-for ((session luvcraft:luvcraft-session))
  (declare (ignore session))
  luvcraft:*actions*)

(defmethod metabar-control-kind
    ((knob luvcraft:scalar-knob) (session luvcraft:luvcraft-session))
  (declare (ignore knob session))
  :scalar)

(defmethod metabar-control-kind
    ((knob luvcraft:switch-knob) (session luvcraft:luvcraft-session))
  (declare (ignore knob session))
  :switch)

(defmethod metabar-control-label
    ((knob luvcraft:knob) (session luvcraft:luvcraft-session))
  (declare (ignore session))
  (luvcraft:knob-label knob))

(defmethod metabar-control-value
    ((knob luvcraft:knob) (session luvcraft:luvcraft-session))
  (luvcraft:knob-value knob session))

(defmethod metabar-control-value-label
    ((knob luvcraft:knob) (session luvcraft:luvcraft-session) value)
  (declare (ignore value))
  (luvcraft:format-knob-value knob session))

(defmethod metabar-control-fraction
    ((knob luvcraft:knob) (session luvcraft:luvcraft-session) value)
  (declare (ignore value))
  (luvcraft:knob-fraction knob session))

(defmethod metabar-control-change-kind
    ((knob luvcraft:knob) (session luvcraft:luvcraft-session))
  (declare (ignore session))
  (and (luvcraft:shader-knob-p knob) :rebuild))

(defmethod metabar-control-update-policy
    ((knob luvcraft:scalar-knob) (session luvcraft:luvcraft-session))
  (declare (ignore session))
  ;; A shader source literal should rebuild once at release, not once per
  ;; pointer-motion frame.  Plain live values remain continuous.
  (if (luvcraft:shader-knob-p knob) :commit-on-release :continuous))

(defmethod perform-metabar-control-step
    ((knob luvcraft:knob) (session luvcraft:luvcraft-session)
     direction multiplier)
  (luvcraft:step-knob knob session direction multiplier))

(defmethod perform-metabar-control-set-fraction
    ((knob luvcraft:scalar-knob) (session luvcraft:luvcraft-session)
     fraction)
  (let* ((minimum (luvcraft:knob-minimum knob))
         (maximum (luvcraft:knob-maximum knob))
         (step (luvcraft:knob-step knob))
         (raw (+ minimum (* fraction (- maximum minimum))))
         (quantized (* step (round raw step))))
    (luvcraft:set-knob-value knob quantized session)))

(defmethod perform-metabar-control-toggle
    ((knob luvcraft:switch-knob) (session luvcraft:luvcraft-session))
  (luvcraft:toggle-knob knob session))

(defmethod metabar-action-label
    ((action luvcraft:action) (session luvcraft:luvcraft-session))
  (declare (ignore session))
  (luvcraft:action-label action))

(defmethod perform-metabar-action
    ((action luvcraft:action) (session luvcraft:luvcraft-session))
  (luvcraft:run-action action session))

;;; ---------------------------------------------------------------------
;;; Luvcraft overlay lifecycle.

(defclass luvcraft-metabar-overlay (luvcraft-hud-widget-overlay)
  ((slide :initform 0d0 :accessor metabar-slide
          :documentation "0 off the left edge, 1 fully open; eased.")
   (open-p :initform t :accessor metabar-open-p)
   (last-time :initform nil :accessor metabar-last-time)))

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-metabar-overlay))
  (declare (ignore overlay))
  :hud)

(defun luvcraft-metabar-frame (overlay)
  (widget-overlay-frame overlay))

(defun luvcraft-metabar-viewport-extent (session)
  (let ((canvas (luvcraft:luvcraft-session-canvas session)))
    (list (luv:canvas-width canvas) (luv:canvas-height canvas))))

(luv:zdefmethod (luvcraft:encode-luvcraft-overlay :zone :metabar/encode)
    ((overlay luvcraft-metabar-overlay) session pass surface-texture)
  (declare (ignore pass))
  (when (plusp (metabar-slide overlay))
    (let ((frame (luvcraft-metabar-frame overlay)))
      (prepare-direct-widget-overlay
       overlay session surface-texture
       (metabar-screen-state
        frame (luvcraft-metabar-viewport-extent session)
        (metabar-slide overlay)))))
  overlay)

(luv:zdefmethod (luvcraft:refresh-luvcraft-overlay :zone :metabar/refresh)
    ((overlay luvcraft-metabar-overlay) session)
  ;; Application operations and value observation occur at the session's
  ;; frame boundary, never in input dispatch or McCLIM repaint.
  (let ((frame (luvcraft-metabar-frame overlay)))
    (drain-metabar-operations frame)
    ;; Author and publish the retained semantic revision before a game render
    ;; pass opens.  Overlay encoding below only replays its prepared commands.
    (prepare-metabar frame))
  (let* ((now (or (luvcraft::luvcraft-session-last-frame-time session) 0d0))
         (last (metabar-last-time overlay))
         (seconds (if last (min 0.1d0 (max 0d0 (- now last))) 0d0))
         (target (if (metabar-open-p overlay) 1d0 0d0))
         (slide (metabar-slide overlay))
         (step (* 6d0 seconds)))
    (setf (metabar-last-time overlay) now
          (metabar-slide overlay)
          (cond ((> target slide) (min target (+ slide step)))
                ((< target slide) (max target (- slide step)))
                (t slide)))
    (when (and (not (metabar-open-p overlay))
               (zerop (metabar-slide overlay)))
      (luvcraft:remove-luvcraft-overlay session overlay)))
  overlay)

(defun close-luvcraft-metabar (overlay)
  "Slide OVERLAY away and restore world focus immediately."
  (check-type overlay luvcraft-metabar-overlay)
  (let ((session (widget-overlay-session overlay)))
    (setf (metabar-open-p overlay) nil)
    (when (eq overlay (luvcraft:luvcraft-session-modal-focus session))
      (luvcraft:unfocus-luvcraft-session session)))
  nil)

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-metabar-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore canvas))
  (let* ((frame (luvcraft-metabar-frame overlay))
         (uv (luvcraft-widget-texture-coordinate
              overlay
              (luv:canvas-pointer-event-x event)
              (luv:canvas-pointer-event-y event))))
    ;; A release clears a commit-style drag even if it landed outside.
    (when (typep event 'luv:canvas-pointer-button-release-event)
      (handle-metabar-pointer-event frame event nil nil))
    (cond
      (uv
       (when (and (typep event 'luv:canvas-pointer-button-press-event)
                  (eq :left (luv:canvas-pointer-event-button event)))
         (luvcraft:focus-luvcraft-session session overlay))
       (handle-metabar-pointer-event
        frame event
        (* (first uv) +metabar-width+)
        (* (second uv) (metabar-logical-height frame)))
       t)
      ((and (typep event 'luv:canvas-pointer-button-press-event)
            (eq overlay (luvcraft:luvcraft-session-modal-focus session)))
       ;; Let the same click reach the world after dismissing the drawer.
       (close-luvcraft-metabar overlay)
       nil)
      (t nil))))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-metabar-overlay) session canvas
     (event luv:canvas-pointer-event))
  (luvcraft:handle-luvcraft-overlay-event overlay session canvas event))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-metabar-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore session canvas))
  (when (eq :dismiss
            (handle-metabar-key-event (luvcraft-metabar-frame overlay) event))
    (close-luvcraft-metabar overlay))
  t)

(defun find-luvcraft-metabar (session)
  (find-if (lambda (overlay)
             (typep overlay 'luvcraft-metabar-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun open-luvcraft-metabar (session &key (title "luvcraft metabar"))
  "Create, attach, slide in, and focus SESSION's shared metabar."
  (let ((frame nil)
        (overlay nil)
        (transferred-p nil)
        (completed-p nil))
    (unwind-protect
         (progn
           (setf frame
                 (make-embedded-metabar
                  session
                  (luvcraft:luvcraft-session-canvas session)
                  (luvcraft:luvcraft-session-context session)
                  (luvcraft:luvcraft-session-device session)
                  :title title))
           (let ((mirror (metabar-mirror frame)))
             (setf overlay
                   (make-instance 'luvcraft-metabar-overlay
                                  :session session :frame frame :mirror mirror)
                   (mirror-compositor mirror) overlay))
           (setf transferred-p t)
           (luvcraft:add-luvcraft-overlay session overlay)
           (luvcraft:focus-luvcraft-session session overlay)
           (setf completed-p t)
           overlay)
      (unless completed-p
        (cond
          ((and overlay
                (member overlay (luvcraft:luvcraft-session-overlays session)
                        :test #'eq))
           (ignore-errors
            (luvcraft:remove-luvcraft-overlay session overlay)))
          ((and overlay (not transferred-p))
           (ignore-errors (luvcraft:release-luvcraft-overlay overlay)))
          ((and frame (not transferred-p))
           (ignore-errors (destroy-metabar frame))))))))

(defmethod luvcraft:toggle-luvcraft-metabar
    ((session luvcraft:luvcraft-session))
  (alexandria:if-let ((overlay (find-luvcraft-metabar session)))
    (if (metabar-open-p overlay)
        (close-luvcraft-metabar overlay)
        (progn
          (refresh-metabar-vocabulary (luvcraft-metabar-frame overlay))
          (setf (metabar-open-p overlay) t)
          (luvcraft:focus-luvcraft-session session overlay)))
    (open-luvcraft-metabar session))
  t)
