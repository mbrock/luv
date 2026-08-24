;;; LUFT adapter for the shared retained-GPU metabar instrument.

(in-package #:luft.render)

(defparameter +viewer-metabar-bevel-widths+ #(1 2 4))
(defconstant +viewer-metabar-minimum-speed+ 1.0)
(defconstant +viewer-metabar-maximum-speed+ 12.0)
(defconstant +viewer-metabar-speed-step+ 0.5)
(defconstant +viewer-metabar-minimum-sensitivity+ 0.0005)
(defconstant +viewer-metabar-maximum-sensitivity+ 0.0100)
(defconstant +viewer-metabar-sensitivity-step+ 0.0005)

;;; ---------------------------------------------------------------------
;;; The viewer's existing authored settings as an open metabar vocabulary.

(defmethod mcluv:metabar-groups-for ((viewer viewer))
  (declare (ignore viewer))
  '(:geometry :navigation))

(defmethod mcluv:metabar-controls-for ((viewer viewer) (group (eql :geometry)))
  (declare (ignore viewer group))
  '(:bevel-width :construction-lines))

(defmethod mcluv:metabar-controls-for
    ((viewer viewer) (group (eql :navigation)))
  (declare (ignore viewer group))
  '(:movement-speed :mouse-sensitivity))

(defmethod mcluv:metabar-actions-for ((viewer viewer))
  (declare (ignore viewer))
  '(:reset-view :quit))

(defmethod mcluv:metabar-control-kind
    ((control (eql :bevel-width)) (viewer viewer))
  (declare (ignore control viewer))
  :scalar)

(defmethod mcluv:metabar-control-kind
    ((control (eql :construction-lines)) (viewer viewer))
  (declare (ignore control viewer))
  :switch)

(defmethod mcluv:metabar-control-kind
    ((control (eql :movement-speed)) (viewer viewer))
  (declare (ignore control viewer))
  :scalar)

(defmethod mcluv:metabar-control-kind
    ((control (eql :mouse-sensitivity)) (viewer viewer))
  (declare (ignore control viewer))
  :scalar)

(defmethod mcluv:metabar-control-label
    ((control (eql :bevel-width)) (viewer viewer))
  (declare (ignore control viewer))
  "bevel width")

(defmethod mcluv:metabar-control-label
    ((control (eql :construction-lines)) (viewer viewer))
  (declare (ignore control viewer))
  "construction lines")

(defmethod mcluv:metabar-control-label
    ((control (eql :movement-speed)) (viewer viewer))
  (declare (ignore control viewer))
  "movement speed")

(defmethod mcluv:metabar-control-label
    ((control (eql :mouse-sensitivity)) (viewer viewer))
  (declare (ignore control viewer))
  "mouse sensitivity")

(defun viewer-metabar-movement-speed (viewer)
  (if (viewer-player viewer)
      (walking-player-speed (viewer-player viewer))
      (viewer-speed viewer)))

(defmethod mcluv:metabar-control-value
    ((control (eql :bevel-width)) (viewer viewer))
  (declare (ignore control))
  (viewer-bevel-width viewer))

(defmethod mcluv:metabar-control-value
    ((control (eql :construction-lines)) (viewer viewer))
  (declare (ignore control viewer))
  (plusp *wireframe*))

(defmethod mcluv:metabar-control-value
    ((control (eql :movement-speed)) (viewer viewer))
  (declare (ignore control))
  (viewer-metabar-movement-speed viewer))

(defmethod mcluv:metabar-control-value
    ((control (eql :mouse-sensitivity)) (viewer viewer))
  (declare (ignore control))
  (viewer-sensitivity viewer))

(defmethod mcluv:metabar-control-value-label
    ((control (eql :bevel-width)) (viewer viewer) value)
  (declare (ignore control viewer))
  (bevel-width-label value))

(defmethod mcluv:metabar-control-value-label
    ((control (eql :construction-lines)) (viewer viewer) value)
  (declare (ignore control viewer))
  (if value "on" "off"))

(defmethod mcluv:metabar-control-value-label
    ((control (eql :movement-speed)) (viewer viewer) value)
  (declare (ignore control viewer))
  (format nil "~,1F cells/s" value))

(defmethod mcluv:metabar-control-value-label
    ((control (eql :mouse-sensitivity)) (viewer viewer) value)
  (declare (ignore control viewer))
  (format nil "~,4F rad/px" value))

(defmethod mcluv:metabar-control-fraction
    ((control (eql :bevel-width)) (viewer viewer) value)
  (declare (ignore control viewer))
  (/ (or (position value +viewer-metabar-bevel-widths+) 0)
     (1- (length +viewer-metabar-bevel-widths+))))

(defmethod mcluv:metabar-control-fraction
    ((control (eql :construction-lines)) (viewer viewer) value)
  (declare (ignore control viewer))
  (if value 1.0 0.0))

(defmethod mcluv:metabar-control-fraction
    ((control (eql :movement-speed)) (viewer viewer) value)
  (declare (ignore control viewer))
  (/ (- value +viewer-metabar-minimum-speed+)
     (- +viewer-metabar-maximum-speed+
        +viewer-metabar-minimum-speed+)))

(defmethod mcluv:metabar-control-fraction
    ((control (eql :mouse-sensitivity)) (viewer viewer) value)
  (declare (ignore control viewer))
  (/ (- value +viewer-metabar-minimum-sensitivity+)
     (- +viewer-metabar-maximum-sensitivity+
        +viewer-metabar-minimum-sensitivity+)))

(defmethod mcluv:metabar-control-change-kind
    ((control (eql :bevel-width)) (viewer viewer))
  (declare (ignore control viewer))
  :rebuild)

(defmethod mcluv:metabar-control-update-policy
    ((control (eql :bevel-width)) (viewer viewer))
  (declare (ignore control viewer))
  :commit-on-release)

(defun set-viewer-construction-lines (viewer enabled-p)
  (setf *wireframe* (if enabled-p 1.0 0.0))
  (when (viewer-renderer viewer)
    (setf (renderer-history-valid-p (viewer-renderer viewer)) nil))
  (refresh-viewer-inspector viewer)
  enabled-p)

(defun set-viewer-metabar-bevel-width (viewer bevel-width)
  (unless (and (= bevel-width (viewer-bevel-width viewer))
               (null (viewer-bevel-profile viewer)))
    ;; This remains the viewer renderer's own coherent replacement boundary.
    ;; The metabar only schedules it there and never remeshes in pointer input.
    (refresh-viewer-renderer
     viewer :solid (viewer-source viewer) :bevel-width bevel-width
            :bevel-profile nil)
    (refresh-viewer-inspector viewer))
  bevel-width)

(defun set-viewer-metabar-movement-speed (viewer value)
  (let* ((clamped (max +viewer-metabar-minimum-speed+
                       (min +viewer-metabar-maximum-speed+ value)))
         (quantized (* +viewer-metabar-speed-step+
                       (round clamped +viewer-metabar-speed-step+))))
    (setf (viewer-speed viewer) quantized)
    (when (viewer-player viewer)
      (setf (walking-player-speed (viewer-player viewer)) quantized))
    quantized))

(defun set-viewer-metabar-sensitivity (viewer value)
  (let* ((clamped (max +viewer-metabar-minimum-sensitivity+
                       (min +viewer-metabar-maximum-sensitivity+ value)))
         (quantized (* +viewer-metabar-sensitivity-step+
                       (round clamped
                              +viewer-metabar-sensitivity-step+))))
    (setf (viewer-sensitivity viewer) quantized)))

(defmethod mcluv:perform-metabar-control-step
    ((control (eql :bevel-width)) (viewer viewer) direction multiplier)
  (declare (ignore control))
  (let* ((widths +viewer-metabar-bevel-widths+)
         (index (or (position (viewer-bevel-width viewer) widths) 0))
         (target (aref widths
                       (mod (+ index (* direction multiplier))
                            (length widths)))))
    (set-viewer-metabar-bevel-width viewer target)))

(defmethod mcluv:perform-metabar-control-step
    ((control (eql :construction-lines)) (viewer viewer)
     direction multiplier)
  (declare (ignore control multiplier))
  (set-viewer-construction-lines viewer (plusp direction)))

(defmethod mcluv:perform-metabar-control-step
    ((control (eql :movement-speed)) (viewer viewer) direction multiplier)
  (declare (ignore control))
  (set-viewer-metabar-movement-speed
   viewer (+ (viewer-metabar-movement-speed viewer)
             (* direction multiplier +viewer-metabar-speed-step+))))

(defmethod mcluv:perform-metabar-control-step
    ((control (eql :mouse-sensitivity)) (viewer viewer)
     direction multiplier)
  (declare (ignore control))
  (set-viewer-metabar-sensitivity
   viewer (+ (viewer-sensitivity viewer)
             (* direction multiplier +viewer-metabar-sensitivity-step+))))

(defmethod mcluv:perform-metabar-control-set-fraction
    ((control (eql :bevel-width)) (viewer viewer) fraction)
  (declare (ignore control))
  (let* ((widths +viewer-metabar-bevel-widths+)
         (index (round (* fraction (1- (length widths))))))
    (set-viewer-metabar-bevel-width viewer (aref widths index))))

(defmethod mcluv:perform-metabar-control-set-fraction
    ((control (eql :movement-speed)) (viewer viewer) fraction)
  (declare (ignore control))
  (set-viewer-metabar-movement-speed
   viewer (+ +viewer-metabar-minimum-speed+
             (* fraction (- +viewer-metabar-maximum-speed+
                            +viewer-metabar-minimum-speed+)))))

(defmethod mcluv:perform-metabar-control-set-fraction
    ((control (eql :mouse-sensitivity)) (viewer viewer) fraction)
  (declare (ignore control))
  (set-viewer-metabar-sensitivity
   viewer (+ +viewer-metabar-minimum-sensitivity+
             (* fraction (- +viewer-metabar-maximum-sensitivity+
                            +viewer-metabar-minimum-sensitivity+)))))

(defmethod mcluv:perform-metabar-control-toggle
    ((control (eql :construction-lines)) (viewer viewer))
  (declare (ignore control))
  (set-viewer-construction-lines viewer (not (plusp *wireframe*))))

(defmethod mcluv:metabar-action-label
    ((action (eql :reset-view)) (viewer viewer))
  (declare (ignore action viewer))
  "reset view")

(defmethod mcluv:metabar-action-label
    ((action (eql :quit)) (viewer viewer))
  (declare (ignore action viewer))
  "quit LUFT")

(defmethod mcluv:perform-metabar-action
    ((action (eql :reset-view)) (viewer viewer))
  (declare (ignore action))
  (reset-viewer-camera viewer))

(defmethod mcluv:perform-metabar-action
    ((action (eql :quit)) (viewer viewer))
  (declare (ignore action))
  (request-viewer-quit viewer))

;;; ---------------------------------------------------------------------
;;; Viewer instrument attachment.

(defclass viewer-metabar-instrument ()
  ((frame :initarg :frame :reader viewer-metabar-frame)
   (compositor :initarg :compositor :reader viewer-metabar-compositor)
   (slide :initform 0d0 :accessor viewer-metabar-slide)
   (open-p :initform t :accessor viewer-metabar-open-p)
   (last-time :initform
              (/ (get-internal-real-time)
                 (coerce internal-time-units-per-second 'double-float))
              :accessor viewer-metabar-last-time)))

(defun viewer-metabar-attachment (viewer)
  (find-if (lambda (instrument)
             (typep instrument 'viewer-metabar-instrument))
           (viewer-instruments viewer)))

(defmethod viewer-instrument-priority ((instrument viewer-metabar-instrument))
  (declare (ignore instrument))
  500)

(defmethod viewer-instrument-present-p
    ((instrument viewer-metabar-instrument) viewer)
  (declare (ignore instrument viewer))
  ;; The dispatcher refreshes only present attachments.  Keep a fully closed
  ;; drawer present for its final refresh, which detaches and releases it;
  ;; ENCODE and event methods below remain no-ops while it is closed.
  t)

(defmethod refresh-viewer-instrument
    ((instrument viewer-metabar-instrument) viewer)
  (let ((frame (viewer-metabar-frame instrument)))
    (mcluv:drain-metabar-operations frame)
    ;; McCLIM authors and publishes the retained semantic revision outside
    ;; the game's render pass.  ENCODE below is replay-only.
    (mcluv:prepare-metabar frame))
  (let* ((now (/ (get-internal-real-time)
                 (coerce internal-time-units-per-second 'double-float)))
         (last (viewer-metabar-last-time instrument))
         (seconds (min 0.1d0 (max 0d0 (- now last))))
         (target (if (viewer-metabar-open-p instrument) 1d0 0d0))
         (slide (viewer-metabar-slide instrument))
         (step (* 6d0 seconds)))
    (setf (viewer-metabar-last-time instrument) now
          (viewer-metabar-slide instrument)
          (cond ((> target slide) (min target (+ slide step)))
                ((< target slide) (max target (- slide step)))
                (t slide)))
    (when (and (not (viewer-metabar-open-p instrument))
               (zerop (viewer-metabar-slide instrument)))
      (remove-viewer-instrument viewer instrument)))
  instrument)

(defmethod encode-viewer-instrument
    ((instrument viewer-metabar-instrument)
     viewer pass surface-texture physical-extent)
  (declare (ignore physical-extent))
  (when (plusp (viewer-metabar-slide instrument))
    (let ((frame (viewer-metabar-frame instrument)))
      (mcluv:encode-direct-gpu-mirror
       (viewer-metabar-compositor instrument) pass surface-texture
       (mcluv:metabar-screen-state
        frame (viewer-logical-extent viewer)
        (viewer-metabar-slide instrument)))))
  instrument)

(defmethod release-viewer-instrument
    ((instrument viewer-metabar-instrument) viewer)
  (declare (ignore viewer))
  (mcluv:destroy-metabar (viewer-metabar-frame instrument)))

(defun close-viewer-metabar (viewer)
  "Begin sliding VIEWER's metabar away, if present."
  (alexandria:when-let ((instrument (viewer-metabar-attachment viewer)))
    (setf (viewer-metabar-open-p instrument) nil))
  nil)

(defun open-viewer-metabar (viewer &key (title "LUFT metabar"))
  "Attach VIEWER's shared metabar and release relative pointer capture."
  (alexandria:if-let ((instrument (viewer-metabar-attachment viewer)))
    (progn
      ;; Lisp development may have extended the open CLOS vocabulary while
      ;; this drawer was sliding out.  Reopening is its publication boundary.
      (mcluv:refresh-metabar-vocabulary
       (viewer-metabar-frame instrument))
      (setf (viewer-metabar-open-p instrument) t)
      instrument)
    (let ((frame nil)
          (transferred-p nil)
          (completed-p nil))
      (unwind-protect
           (progn
             (clear-viewer-controls viewer)
             (when (viewer-pointer-captured-p viewer)
               (set-canvas-relative-pointer-mode (viewer-canvas viewer) nil)
               (setf (viewer-pointer-captured-p viewer) nil))
             (setf frame
                   (mcluv:make-embedded-metabar
                    viewer
                    (viewer-canvas viewer)
                    (viewer-context viewer)
                    (viewer-device viewer)
                    :title title))
             (let* ((mirror (mcluv:metabar-mirror frame))
                    (compositor
                      (make-instance 'mcluv:direct-gpu-mirror-compositor
                                     :mirror mirror))
                    (instrument
                      (make-instance 'viewer-metabar-instrument
                                     :frame frame :compositor compositor)))
               (setf (mcluv:mirror-compositor mirror) compositor)
               (setf transferred-p t)
               (add-viewer-instrument viewer instrument)
               (setf completed-p t)
               instrument))
        (unless completed-p
          (when (and frame (not transferred-p))
            (mcluv:destroy-metabar frame)))))))

(defun toggle-viewer-metabar (viewer)
  (alexandria:if-let ((instrument (viewer-metabar-attachment viewer)))
    (if (viewer-metabar-open-p instrument)
        (close-viewer-metabar viewer)
        (open-viewer-metabar viewer))
    (open-viewer-metabar viewer))
  t)

(clim:define-command (com-toggle-metabar
                      :command-table luft-atelier
                      :name "Toggle Metabar"
                      :keystroke (:return))
    ()
  (toggle-viewer-metabar (viewer-command-viewer)))

(defmethod handle-viewer-instrument-event
    ((instrument viewer-metabar-instrument)
     viewer canvas (event canvas-key-press-event))
  (declare (ignore canvas))
  (when (viewer-metabar-open-p instrument)
    (when (eq :dismiss
              (mcluv:handle-metabar-key-event
               (viewer-metabar-frame instrument) event))
      (close-viewer-metabar viewer))
    t))

(defmethod handle-viewer-instrument-event
    ((instrument viewer-metabar-instrument)
     viewer canvas (event canvas-key-release-event))
  (declare (ignore viewer canvas event))
  (and (viewer-metabar-open-p instrument) t))

(defmethod handle-viewer-instrument-event
    ((instrument viewer-metabar-instrument)
     viewer canvas (event canvas-pointer-event))
  (declare (ignore canvas))
  (when (viewer-metabar-open-p instrument)
    (let ((frame (viewer-metabar-frame instrument)))
      (when (typep event 'canvas-pointer-motion-event)
        (setf (viewer-pointer-x viewer) (canvas-pointer-event-x event)
              (viewer-pointer-y viewer) (canvas-pointer-event-y event)))
      (multiple-value-bind (x y)
          (mcluv:metabar-local-coordinate
           frame
           (canvas-pointer-event-x event)
           (canvas-pointer-event-y event)
           (viewer-logical-extent viewer)
           (viewer-metabar-slide instrument))
        (when (typep event 'canvas-pointer-button-release-event)
          (mcluv:handle-metabar-pointer-event frame event nil nil))
        (cond
          (x (mcluv:handle-metabar-pointer-event frame event x y))
          ((typep event 'canvas-pointer-button-press-event)
           (close-viewer-metabar viewer)))))
    ;; Modal while open: an outside click dismisses without throwing a ball or
    ;; recapturing relative pointer mode in the same event.
    t))

(defmethod handle-viewer-instrument-event
    ((instrument viewer-metabar-instrument)
     viewer canvas (event canvas-window-focus-lost-event))
  (declare (ignore instrument canvas event))
  (close-viewer-metabar viewer)
  nil)
