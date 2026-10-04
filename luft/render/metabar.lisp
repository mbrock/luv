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
  '(:geometry :navigation :sky :lens :atmosphere))

(defmethod mcluv:metabar-controls-for ((viewer viewer) (group (eql :geometry)))
  (declare (ignore viewer group))
  '(:bevel-width :construction-lines))

(defmethod mcluv:metabar-controls-for
    ((viewer viewer) (group (eql :navigation)))
  (declare (ignore viewer group))
  '(:movement-speed :mouse-sensitivity :field-of-view))

(defmethod mcluv:metabar-controls-for ((viewer viewer) (group (eql :sky)))
  (declare (ignore viewer group))
  '(:time-of-day))

(defmethod mcluv:metabar-controls-for ((viewer viewer) (group (eql :lens)))
  (declare (ignore viewer group))
  '(:bloom-gain :bloom-threshold :shaft-gain :shaft-decay
    :vignette :paper-grain))

(defmethod mcluv:metabar-controls-for
    ((viewer viewer) (group (eql :atmosphere)))
  (declare (ignore viewer group))
  '(:haze-density :haze-height))

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
      (character-speed (viewer-player viewer))
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
      (setf (character-speed (viewer-player viewer)) quantized))
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

;;; ---------------------------------------------------------------------
;;; Live picture settings.
;;;
;;; The frame reads these places every frame, so a slider needs no
;;; rebuild.  One definition gives a place its label, range, step, and value
;;; text, which is all the metabar protocol asks of a scalar.

(defmacro define-viewer-metabar-setting
    (name &key label place minimum maximum step display)
  "Make the setf-able PLACE, evaluated with VIEWER bound, a metabar scalar.

DISPLAY is a function of the value returning its text."
  (let ((quantize (gensym "QUANTIZE")))
    `(progn
       (defmethod mcluv:metabar-control-kind
           ((control (eql ,name)) (viewer viewer))
         (declare (ignore control viewer))
         :scalar)
       (defmethod mcluv:metabar-control-label
           ((control (eql ,name)) (viewer viewer))
         (declare (ignore control viewer))
         ,label)
       (defmethod mcluv:metabar-control-value
           ((control (eql ,name)) (viewer viewer))
         (declare (ignore control) (ignorable viewer))
         ,place)
       (defmethod mcluv:metabar-control-value-label
           ((control (eql ,name)) (viewer viewer) value)
         (declare (ignore control viewer))
         (funcall ,display value))
       (defmethod mcluv:metabar-control-fraction
           ((control (eql ,name)) (viewer viewer) value)
         (declare (ignore control viewer))
         (/ (- value ,minimum) (- ,maximum ,minimum)))
       (flet ((,quantize (value)
                (* ,step (round (max ,minimum (min ,maximum value)) ,step))))
         (defmethod mcluv:perform-metabar-control-step
             ((control (eql ,name)) (viewer viewer) direction multiplier)
           (declare (ignore control) (ignorable viewer))
           (setf ,place
                 (coerce (,quantize (+ ,place (* direction multiplier ,step)))
                         'single-float)))
         (defmethod mcluv:perform-metabar-control-set-fraction
             ((control (eql ,name)) (viewer viewer) fraction)
           (declare (ignore control) (ignorable viewer))
           (setf ,place
                 (coerce (,quantize (+ ,minimum
                                       (* fraction (- ,maximum ,minimum))))
                         'single-float))))
       ',name)))

(defun metabar-clock-label (hour)
  (multiple-value-bind (hours fraction) (floor hour)
    (format nil "~2,'0D:~2,'0D" (mod hours 24) (floor (* fraction 60)))))

(defun metabar-number-label (control-string)
  (lambda (value) (format nil control-string value)))

(define-viewer-metabar-setting :time-of-day
  :label "time of day" :place *sky-hour*
  :minimum 0.0 :maximum 24.0 :step 0.25 :display #'metabar-clock-label)

(define-viewer-metabar-setting :field-of-view
  :label "field of view" :place (camera-field-of-view (viewer-camera viewer))
  :minimum 0.45 :maximum 1.75 :step 0.05
  :display (lambda (value) (format nil "~D°" (round (* value (/ 180 pi))))))

(define-viewer-metabar-setting :bloom-gain
  :label "bloom" :place *bloom-gain*
  :minimum 0.0 :maximum 1.0 :step 0.02 :display (metabar-number-label "~,2F"))

(define-viewer-metabar-setting :bloom-threshold
  :label "bloom threshold" :place *bloom-threshold*
  :minimum 0.2 :maximum 4.0 :step 0.1 :display (metabar-number-label "~,1F"))

(define-viewer-metabar-setting :shaft-gain
  :label "sun shafts" :place *shaft-gain*
  :minimum 0.0 :maximum 1.0 :step 0.02 :display (metabar-number-label "~,2F"))

(define-viewer-metabar-setting :shaft-decay
  :label "shaft reach" :place *shaft-decay*
  :minimum 0.8 :maximum 0.995 :step 0.005
  :display (metabar-number-label "~,3F"))

(define-viewer-metabar-setting :vignette
  :label "vignette" :place *vignette*
  :minimum 0.0 :maximum 1.0 :step 0.02 :display (metabar-number-label "~,2F"))

(define-viewer-metabar-setting :paper-grain
  :label "paper grain" :place *paper-grain*
  :minimum 0.0 :maximum 0.15 :step 0.005
  :display (metabar-number-label "~,3F"))

(define-viewer-metabar-setting :haze-density
  :label "haze" :place *haze-density*
  :minimum 0.0 :maximum 0.03 :step 0.001
  :display (metabar-number-label "~,3F"))

(define-viewer-metabar-setting :haze-height
  :label "haze falloff" :place *haze-height*
  :minimum 0.0 :maximum 0.08 :step 0.002
  :display (metabar-number-label "~,3F"))
