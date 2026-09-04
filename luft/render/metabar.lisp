;;; LUFT's semantic vocabulary for the workbench-owned metabar pane.

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
  (unless (= bevel-width (viewer-bevel-width viewer))
    ;; This remains the viewer renderer's own coherent replacement boundary.
    ;; The metabar only schedules it there and never remeshes in pointer input.
    (refresh-viewer-renderer
     viewer :solid (viewer-source viewer) :bevel-width bevel-width)
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

(defun close-viewer-metabar (viewer)
  (when-let
      ((workbench (luv.workbench:application-workbench viewer)))
    (luv.workbench:close-workbench-metabar workbench)))

(defun open-viewer-metabar (viewer &key (title "LUFT metabar"))
  (declare (ignore title))
  (luv.workbench:open-workbench-metabar
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(defun toggle-viewer-metabar (viewer)
  (luv.workbench:toggle-workbench-metabar
   (or (luv.workbench:application-workbench viewer)
       (error "~S has no attached workbench." viewer))))

(clim:define-command (com-toggle-metabar
                      :command-table luft-atelier
                      :name "Toggle Metabar"
                      :keystroke (:return))
    ()
  (toggle-viewer-metabar (viewer-command-viewer)))
