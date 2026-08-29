;;; The metabar is a shared, retained application instrument: a drawer of
;;; grouped live controls and actions which an application may attach to its
;;; own render and input lifecycle.  This file owns the semantic UI, not any
;;; particular game's state or overlay plumbing.

(in-package #:mcluv)

(defconstant +metabar-width+ 460)
(defconstant +metabar-row-height+ 74)
(defconstant +metabar-switch-height+ 46)
(defconstant +metabar-header-height+ 36)
(defconstant +metabar-action-height+ 44)
(defconstant +metabar-top-pad+ 8)
(defconstant +metabar-bottom-pad+ 8)
(defconstant +metabar-status-height+ 24)
(defconstant +metabar-pad+ 18)
(defconstant +metabar-track-left+ 58)
(defconstant +metabar-track-right+ 400)
(defconstant +metabar-viewport-margin+ 12)

(defun metabar-alpha-ink (red green blue alpha)
  (compose-in (make-rgb-color red green blue) (make-opacity alpha)))

;; Every background ink carries alpha into the direct compositor.  Its solid
;; and analytic shaders premultiply RGB before the ordinary OVER blend.
(defparameter *metabar-shadow-ink*
  (metabar-alpha-ink 0.0 0.0 0.0 0.34))
(defparameter *metabar-edge-ink*
  (metabar-alpha-ink 0.42 0.46 0.40 0.82))
(defparameter *metabar-panel-ink*
  (metabar-alpha-ink 0.070 0.073 0.066 0.90))
(defparameter *metabar-row-ink*
  (metabar-alpha-ink 0.16 0.16 0.15 0.68))
(defparameter *metabar-selected-row-ink*
  (metabar-alpha-ink 0.23 0.25 0.22 0.90))
(defparameter *metabar-header-ink*
  (metabar-alpha-ink 0.12 0.125 0.115 0.76))
(defparameter *metabar-track-ink*
  (metabar-alpha-ink 0.30 0.30 0.28 0.82))
(defparameter *metabar-fill-ink*
  (metabar-alpha-ink 0.58 0.78 0.54 0.96))
(defparameter *metabar-rebuild-ink*
  (metabar-alpha-ink 0.85 0.68 0.40 0.98))
(defparameter *metabar-knob-ink* (make-rgb-color 0.93 0.93 0.90))
(defparameter *metabar-text-ink* (make-rgb-color 0.91 0.89 0.82))
(defparameter *metabar-muted-ink* (make-rgb-color 0.60 0.60 0.55))
(defparameter *metabar-error-ink* (make-rgb-color 0.94 0.48 0.40))

;;; ---------------------------------------------------------------------
;;; Application protocol.

(defgeneric metabar-groups-for (owner)
  (:documentation
   "Return OWNER's ordered group identities.  Group identities remain owned
by the application; symbols are sufficient when they are the vocabulary."))

(defmethod metabar-groups-for (owner)
  (declare (ignore owner))
  nil)

(defgeneric metabar-group-label (owner group)
  (:documentation "Return GROUP's short label in OWNER's metabar."))

(defmethod metabar-group-label (owner (group symbol))
  (declare (ignore owner))
  (substitute #\Space #\- (string-downcase group)))

(defmethod metabar-group-label (owner (group string))
  (declare (ignore owner))
  group)

(defgeneric metabar-controls-for (owner group)
  (:documentation "Return OWNER's ordered controls in GROUP."))

(defmethod metabar-controls-for (owner group)
  (declare (ignore owner group))
  nil)

(defgeneric metabar-actions-for (owner)
  (:documentation "Return OWNER's ordered metabar actions."))

(defmethod metabar-actions-for (owner)
  (declare (ignore owner))
  nil)

(defgeneric metabar-control-kind (control owner)
  (:documentation "Return :SCALAR or :SWITCH for CONTROL in OWNER."))

(defgeneric metabar-control-label (control owner)
  (:documentation "Return CONTROL's short label in OWNER."))

(defgeneric metabar-control-value (control owner)
  (:documentation
   "Return CONTROL's cheap authored value.  This runs during owner refresh,
never inside repaint."))

(defgeneric metabar-control-value-label (control owner value)
  (:documentation "Format the already observed VALUE for CONTROL."))

(defmethod metabar-control-value-label (control owner value)
  (declare (ignore control owner))
  (princ-to-string value))

(defgeneric metabar-control-fraction (control owner value)
  (:documentation "Return scalar VALUE's clamped position from 0 to 1."))

(defgeneric metabar-control-change-kind (control owner)
  (:documentation
   "Return NIL for an immediate value, or a short semantic marker such as
:REBUILD when realizing CONTROL involves deferred derived work."))

(defmethod metabar-control-change-kind (control owner)
  (declare (ignore control owner))
  nil)

(defgeneric metabar-control-update-policy (control owner)
  (:documentation
   "Return :CONTINUOUS or :COMMIT-ON-RELEASE.  The latter previews a pointer
drag but applies only its last value after release, which keeps expensive
derived work out of motion-event bursts."))

(defmethod metabar-control-update-policy (control owner)
  (declare (ignore control owner))
  :continuous)

(defgeneric perform-metabar-control-step
    (control owner direction multiplier)
  (:documentation
   "At OWNER's frame boundary, move CONTROL by signed DIRECTION and MULTIPLIER."))

(defgeneric perform-metabar-control-set-fraction (control owner fraction)
  (:documentation
   "At OWNER's frame boundary, set scalar CONTROL from clamped FRACTION."))

(defgeneric perform-metabar-control-toggle (control owner)
  (:documentation "At OWNER's frame boundary, toggle switch CONTROL."))

(defgeneric metabar-action-label (action owner)
  (:documentation "Return ACTION's short button label in OWNER."))

(defgeneric perform-metabar-action (action owner)
  (:documentation "Invoke ACTION at OWNER's frame boundary."))

;;; ---------------------------------------------------------------------
;;; Cached vocabulary, rows, and operations.

(defstruct metabar-vocabulary
  groups
  control-groups
  actions)

(defstruct metabar-row
  kind
  control-kind
  subject
  label
  height
  count
  value
  value-label
  fraction
  change-kind
  update-policy)

(defstruct metabar-operation
  kind
  subject
  argument)

(defun capture-metabar-vocabulary (owner)
  "Capture OWNER's small semantic vocabulary outside repaint."
  (let ((groups (copy-list (metabar-groups-for owner))))
    (make-metabar-vocabulary
     :groups groups
     :control-groups
     (mapcar (lambda (group)
               (cons group (copy-list (metabar-controls-for owner group))))
             groups)
     :actions (copy-list (metabar-actions-for owner)))))

(defun metabar-vocabulary-controls (vocabulary group)
  (cdr (assoc group (metabar-vocabulary-control-groups vocabulary)
              :test #'eq)))

(defun metabar-control-row-height (control owner)
  (ecase (metabar-control-kind control owner)
    (:scalar +metabar-row-height+)
    (:switch +metabar-switch-height+)))

(defun metabar-natural-height-for (owner vocabulary)
  "Return the fixed logical height which fits VOCABULARY's largest group."
  (+ +metabar-top-pad+
     (* +metabar-header-height+
        (length (metabar-vocabulary-groups vocabulary)))
     (reduce
      #'max (metabar-vocabulary-groups vocabulary)
      :key
      (lambda (group)
        (reduce #'+ (metabar-vocabulary-controls vocabulary group)
                :key (lambda (control)
                       (metabar-control-row-height control owner))
                :initial-value 0))
      :initial-value 0)
     (* +metabar-action-height+
        (length (metabar-vocabulary-actions vocabulary)))
     +metabar-status-height+
     +metabar-bottom-pad+))

(defclass metabar-pane (transparent-gpu-application-pane) ())

(defvar *metabar-construction-height* 600
  "Logical height used while MAKE-EMBEDDED-METABAR realizes its layout.")

(defclass metabar-state ()
  ((owner :initarg :owner :initform nil :reader metabar-owner)
   (vocabulary :initarg :vocabulary :initform nil
               :accessor metabar-vocabulary)
   (logical-height :initarg :logical-height :initform nil
                   :accessor metabar-logical-height)
   (rows :initform nil :accessor metabar-rows)
   (selected :initform 0 :accessor metabar-selected)
   (open-group :initform nil :accessor metabar-open-group)
   (dragging :initform nil :accessor metabar-dragging)
   (preview-fractions :initform nil :accessor metabar-preview-fractions)
   (pending-operations :initform nil :accessor metabar-pending-operations)
   (diagnostic :initform nil :accessor metabar-diagnostic)
   (dirty-p :initform t :accessor metabar-dirty-p)))

(define-application-frame metabar
    (standard-application-frame metabar-state)
  ()
  (:menu-bar nil)
  (:panes
   (bar (make-pane 'metabar-pane
                   :background +transparent-ink+
                   :width +metabar-width+
                   :height *metabar-construction-height*
                   :min-width +metabar-width+
                   :min-height *metabar-construction-height*
                   :max-width +metabar-width+
                   :max-height *metabar-construction-height*)))
  (:layouts (default bar)))

(defun make-metabar-control-row (frame control)
  (let* ((owner (metabar-owner frame))
         (kind (metabar-control-kind control owner))
         (value (metabar-control-value control owner)))
    (make-metabar-row
     :kind :control
     :control-kind kind
     :subject control
     :label (metabar-control-label control owner)
     :height (ecase kind
               (:scalar +metabar-row-height+)
               (:switch +metabar-switch-height+))
     :value value
     :value-label (metabar-control-value-label control owner value)
     :fraction (metabar-control-fraction control owner value)
     :change-kind (metabar-control-change-kind control owner)
     :update-policy (metabar-control-update-policy control owner))))

(defun rebuild-metabar-rows (frame)
  "Rebuild FRAME's retained row vocabulary after a structural change."
  (let* ((owner (metabar-owner frame))
         (vocabulary (metabar-vocabulary frame))
         (open-group (metabar-open-group frame)))
    (setf (metabar-rows frame)
          (append
           (loop for group in (metabar-vocabulary-groups vocabulary)
                 collect
                 (make-metabar-row
                  :kind :group :subject group
                  :label (metabar-group-label owner group)
                  :height +metabar-header-height+
                  :count (length
                          (metabar-vocabulary-controls vocabulary group)))
                 when (eq group open-group)
                   append
                   (mapcar (lambda (control)
                             (make-metabar-control-row frame control))
                           (metabar-vocabulary-controls vocabulary group)))
           (mapcar
            (lambda (action)
              (make-metabar-row
               :kind :action :subject action
               :label (metabar-action-label action owner)
               :height +metabar-action-height+))
            (metabar-vocabulary-actions vocabulary))))
    (when (plusp (length (metabar-rows frame)))
      (setf (metabar-selected frame)
            (mod (metabar-selected frame) (length (metabar-rows frame)))))
    (setf (metabar-dirty-p frame) t))
  frame)

(defmethod initialize-instance :after ((frame metabar-state) &key)
  (unless (metabar-vocabulary frame)
    (setf (metabar-vocabulary frame)
          (capture-metabar-vocabulary (metabar-owner frame))))
  (unless (metabar-logical-height frame)
    (setf (metabar-logical-height frame)
          (metabar-natural-height-for
           (metabar-owner frame) (metabar-vocabulary frame))))
  (unless (metabar-open-group frame)
    (setf (metabar-open-group frame)
          (first (metabar-vocabulary-groups
                  (metabar-vocabulary frame)))))
  (rebuild-metabar-rows frame))

(defun refresh-metabar-vocabulary (frame)
  "Re-read FRAME's groups, controls, and actions outside repaint."
  (setf (metabar-vocabulary frame)
        (capture-metabar-vocabulary (metabar-owner frame)))
  (unless (member (metabar-open-group frame)
                  (metabar-vocabulary-groups (metabar-vocabulary frame))
                  :test #'eq)
    (setf (metabar-open-group frame)
          (first (metabar-vocabulary-groups
                  (metabar-vocabulary frame)))))
  (rebuild-metabar-rows frame))

(defun metabar-selected-row (frame)
  (nth (metabar-selected frame) (metabar-rows frame)))

(defun metabar-row-top (frame index)
  (+ +metabar-top-pad+
     (loop for row in (metabar-rows frame)
           repeat index
           sum (metabar-row-height row))))

(defun metabar-row-at (frame y)
  "Return the visible row index at local logical Y, or NIL."
  (let ((top +metabar-top-pad+))
    (loop for row in (metabar-rows frame)
          for index from 0
          for bottom = (+ top (metabar-row-height row))
          when (and (<= top y) (< y bottom))
            return index
          do (setf top bottom))))

(defun metabar-control-row (frame control)
  (find control (metabar-rows frame)
        :key (lambda (row)
               (and (eq :control (metabar-row-kind row))
                    (metabar-row-subject row)))
        :test #'eq))

(defun invalidate-metabar (frame)
  "Record a semantic revision.  Repainting waits for the owner's refresh boundary."
  (setf (metabar-dirty-p frame) t)
  frame)

(defun refresh-metabar-state (frame)
  "Observe visible control values outside repaint and invalidate on change."
  (let ((owner (metabar-owner frame))
        (changed-p nil))
    (dolist (row (metabar-rows frame))
      (when (eq :control (metabar-row-kind row))
        (let* ((control (metabar-row-subject row))
               (value (metabar-control-value control owner))
               (change-kind (metabar-control-change-kind control owner)))
          (unless (equalp value (metabar-row-value row))
            (setf (metabar-row-value row) value
                  (metabar-row-value-label row)
                  (metabar-control-value-label control owner value)
                  (metabar-row-fraction row)
                  (metabar-control-fraction control owner value)
                  changed-p t))
          (unless (eql change-kind (metabar-row-change-kind row))
            (setf (metabar-row-change-kind row) change-kind
                  changed-p t)))))
    (when changed-p (invalidate-metabar frame)))
  frame)

(defun metabar-preview-fraction (frame control)
  (cdr (assoc control (metabar-preview-fractions frame) :test #'eq)))

(defun (setf metabar-preview-fraction) (fraction frame control)
  (let ((entry (assoc control (metabar-preview-fractions frame) :test #'eq)))
    (if entry
        (setf (cdr entry) fraction)
        (push (cons control fraction) (metabar-preview-fractions frame))))
  (invalidate-metabar frame)
  fraction)

(defun clear-metabar-preview-fraction (frame control)
  (let ((previews (metabar-preview-fractions frame)))
    (when (assoc control previews :test #'eq)
      (setf (metabar-preview-fractions frame)
            (delete control previews :key #'car :test #'eq))
      ;; The committed application value may quantize back to its old value.
      ;; Removing the retained preview is nevertheless a visible revision.
      (invalidate-metabar frame)))
  frame)

(defun queued-metabar-operation (operations kind subject)
  (find-if (lambda (operation)
             (and (eq kind (metabar-operation-kind operation))
                  (eq subject (metabar-operation-subject operation))))
           operations))

(defun queue-metabar-operation (frame kind subject &optional argument)
  "Queue one small semantic operation without invoking application code."
  (let* ((operations (metabar-pending-operations frame))
         (existing (queued-metabar-operation operations kind subject)))
    (cond
      ((and existing (eq kind :set-fraction))
       (setf (metabar-operation-argument existing) argument))
      ((and existing (eq kind :step))
       (incf (metabar-operation-argument existing) argument)
       ;; Opposite nudges in one input burst cancel before an expensive owner
       ;; realization (for example shader rebuilding) can observe a no-op.
       (when (zerop (metabar-operation-argument existing))
         (setf (metabar-pending-operations frame)
               (delete existing operations :test #'eq))))
      (t
       (setf (metabar-pending-operations frame)
             (append operations
                     (list (make-metabar-operation
                            :kind kind :subject subject
                            :argument argument)))))))
  frame)

(defun perform-metabar-operation (frame operation)
  (let ((owner (metabar-owner frame))
        (subject (metabar-operation-subject operation))
        (argument (metabar-operation-argument operation)))
    (ecase (metabar-operation-kind operation)
      (:step
       (perform-metabar-control-step
        subject owner (if (minusp argument) -1 1) (abs argument)))
      (:set-fraction
       (perform-metabar-control-set-fraction subject owner argument))
      (:toggle
       (perform-metabar-control-toggle subject owner))
      (:action
       (perform-metabar-action subject owner)))))

(defun metabar-operation-held-p (frame operation)
  (and (eq :set-fraction (metabar-operation-kind operation))
       (eq (metabar-operation-subject operation)
           (metabar-dragging frame))
       (let ((row (metabar-control-row
                   frame (metabar-operation-subject operation))))
         (and row
              (eq :commit-on-release
                  (metabar-row-update-policy row))))))

(defun drain-metabar-operations (frame)
  "Apply queued edits at the owner boundary, retaining commit-style drags."
  (let ((pending (metabar-pending-operations frame))
        (retained nil)
        (diagnostic nil)
        (attempted-p nil))
    (setf (metabar-pending-operations frame) nil)
    (dolist (operation pending)
      (if (metabar-operation-held-p frame operation)
          (push operation retained)
          (progn
            (setf attempted-p t)
            (handler-case
                (unwind-protect
                    (perform-metabar-operation frame operation)
                  ;; A failed commit must not leave its optimistic preview
                  ;; displayed as though the owner accepted it.
                  (ignore-errors
                  (clear-metabar-preview-fraction
                   frame (metabar-operation-subject operation))))
              (error (condition)
                ;; A development instrument should leave the application alive
                ;; and put the failure where the human can see it.
                (setf diagnostic condition))))))
    (setf (metabar-pending-operations frame) (nreverse retained))
    (when (and attempted-p
               (not (eq diagnostic (metabar-diagnostic frame))))
      (setf (metabar-diagnostic frame) diagnostic)
      (invalidate-metabar frame))
    (refresh-metabar-state frame))
  frame)

(defun cancel-metabar-interaction (frame)
  "Discard an unfinished drag when FRAME's transient presentation closes."
  (when (or (metabar-dragging frame)
            (metabar-pending-operations frame)
            (metabar-preview-fractions frame))
    (setf (metabar-dragging frame) nil
          (metabar-pending-operations frame) nil
          (metabar-preview-fractions frame) nil)
    (invalidate-metabar frame))
  frame)

;;; ---------------------------------------------------------------------
;;; Painting: cached semantic rows only, direct GPU only.

(define-condition metabar-requires-direct-gpu (error)
  ((object :initarg :object :reader metabar-non-gpu-object))
  (:report
   (lambda (condition stream)
     (format stream "The metabar requires retained direct GPU media, not ~S."
             (metabar-non-gpu-object condition)))))

(define-condition metabar-direct-presentation-violation (error)
  ((reason :initarg :reason :reader metabar-presentation-violation-reason))
  (:report
   (lambda (condition stream)
     (format stream "The metabar violated its direct presentation contract: ~A."
             (metabar-presentation-violation-reason condition)))))

(defun ensure-metabar-gpu-medium (medium)
  (unless (typep medium 'luv-gpu-medium)
    (error 'metabar-requires-direct-gpu :object medium))
  medium)

(defun draw-metabar-selection-mark (medium top bottom)
  (draw-analytic-rounded-rectangle*
   medium 7 (+ top 4) 11 (- bottom 4) :radius 2
   :ink *metabar-fill-ink*))

(defun draw-metabar-group-row (medium row top selected-p open-p)
  (let ((bottom (+ top (metabar-row-height row))))
    (draw-analytic-rounded-rectangle*
     medium 7 (+ top 2) (- +metabar-width+ 8) (- bottom 2) :radius 6
     :ink (if selected-p *metabar-selected-row-ink* *metabar-header-ink*))
    (when selected-p (draw-metabar-selection-mark medium top bottom))
    (draw-text* medium (if open-p "▾" "▸")
                +metabar-pad+ (/ (+ top bottom) 2)
                :align-y :center :text-size 15
                :ink (if open-p *metabar-fill-ink* *metabar-muted-ink*))
    (draw-text* medium (metabar-row-label row)
                (+ +metabar-pad+ 22) (/ (+ top bottom) 2)
                :align-y :center :text-size 15 :text-face :bold
                :ink (if (or open-p selected-p)
                         *metabar-text-ink* *metabar-muted-ink*))
    (draw-text* medium (format nil "~D" (metabar-row-count row))
                (- +metabar-width+ +metabar-pad+) (/ (+ top bottom) 2)
                :align-x :right :align-y :center :text-size 13
                :ink *metabar-muted-ink*)))

(defun draw-metabar-control-title (medium row top selected-p)
  (draw-text* medium (metabar-row-label row)
              +metabar-pad+ (+ top 24)
              :align-y :center :text-size 17 :ink *metabar-text-ink*)
  (when (metabar-row-change-kind row)
    (draw-circle* medium
                  (+ +metabar-pad+ 10
                     (text-size medium (metabar-row-label row)
                                :text-style (make-text-style nil nil 17)))
                  (+ top 24) 4 :ink *metabar-rebuild-ink*))
  (draw-text* medium (metabar-row-value-label row)
              (- +metabar-width+ +metabar-pad+) (+ top 24)
              :align-x :right :align-y :center :text-size 17
              :text-face :bold
              :ink (if selected-p *metabar-fill-ink* *metabar-text-ink*)))

(defun draw-metabar-scalar-row (frame medium row top selected-p)
  (let* ((track-y (+ top 52))
         (control (metabar-row-subject row))
         (preview (metabar-preview-fraction frame control))
         (fraction (if preview preview (metabar-row-fraction row)))
         (knob-x (+ +metabar-track-left+
                    (* fraction (- +metabar-track-right+
                                   +metabar-track-left+)))))
    (draw-metabar-control-title medium row top selected-p)
    (draw-text* medium "−" (- +metabar-track-left+ 18) track-y
                :align-x :center :align-y :center :text-size 24
                :ink *metabar-muted-ink*)
    (draw-text* medium "+" (+ +metabar-track-right+ 18) track-y
                :align-x :center :align-y :center :text-size 24
                :ink *metabar-muted-ink*)
    (draw-analytic-rounded-rectangle*
     medium +metabar-track-left+ (- track-y 4)
     +metabar-track-right+ (+ track-y 4) :radius 4
     :ink *metabar-track-ink*)
    (when (> knob-x +metabar-track-left+)
      (draw-analytic-rounded-rectangle*
       medium +metabar-track-left+ (- track-y 4) knob-x (+ track-y 4)
       :radius 4 :ink *metabar-fill-ink*))
    (draw-circle* medium knob-x track-y 10 :ink *metabar-panel-ink*)
    (draw-circle* medium knob-x track-y 8 :ink *metabar-knob-ink*)))

(defun draw-metabar-switch-row (medium row top selected-p)
  (let* ((y (+ top (/ (metabar-row-height row) 2)))
         (right (- +metabar-width+ +metabar-pad+))
         (left (- right 48))
         (on-p (not (null (metabar-row-value row)))))
    (draw-text* medium (metabar-row-label row) +metabar-pad+ y
                :align-y :center :text-size 17 :ink *metabar-text-ink*)
    (draw-analytic-rounded-rectangle*
     medium left (- y 12) right (+ y 12) :radius 12
     :ink (if on-p *metabar-fill-ink* *metabar-track-ink*))
    (draw-circle* medium (if on-p (- right 12) (+ left 12)) y 8
                  :ink *metabar-knob-ink*)
    (when selected-p
      (draw-metabar-selection-mark
       medium top (+ top (metabar-row-height row))))))

(defun draw-metabar-control-row (frame medium row top selected-p)
  (let ((bottom (+ top (metabar-row-height row))))
    (draw-analytic-rounded-rectangle*
     medium 7 (+ top 2) (- +metabar-width+ 8) (- bottom 2) :radius 6
     :ink (if selected-p *metabar-selected-row-ink* *metabar-row-ink*))
    (when (and selected-p (eq :scalar (metabar-row-control-kind row)))
      (draw-metabar-selection-mark medium top bottom))
    (ecase (metabar-row-control-kind row)
      (:scalar (draw-metabar-scalar-row frame medium row top selected-p))
      (:switch (draw-metabar-switch-row medium row top selected-p)))))

(defun draw-metabar-action-row (medium row top selected-p)
  (let ((bottom (+ top (metabar-row-height row))))
    (draw-analytic-rounded-rectangle*
     medium 10 (+ top 4) (- +metabar-width+ 10) (- bottom 4) :radius 7
     :ink (if selected-p *metabar-fill-ink* *metabar-selected-row-ink*))
    (draw-text* medium (metabar-row-label row)
                (/ +metabar-width+ 2) (/ (+ top bottom) 2)
                :align-x :center :align-y :center :text-size 16
                :ink (if selected-p *metabar-panel-ink* *metabar-text-ink*))))

(defmethod handle-repaint ((pane metabar-pane) region)
  (declare (ignore region))
  (let ((frame (pane-frame pane)))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (ensure-metabar-gpu-medium medium)
        (draw-analytic-rounded-rectangle*
         medium (+ left 5) (+ top 7) right bottom :radius 16
         :ink *metabar-shadow-ink*)
        (draw-analytic-rounded-rectangle*
         medium left top right bottom :radius 15 :ink *metabar-edge-ink*)
        (draw-analytic-rounded-rectangle*
         medium (+ left 2) (+ top 2) (- right 2) (- bottom 2)
         :radius 13 :ink *metabar-panel-ink*)
        (let ((y +metabar-top-pad+))
          (loop for row in (metabar-rows frame)
                for index from 0
                for selected-p = (= index (metabar-selected frame))
                do
                   (ecase (metabar-row-kind row)
                     (:group
                      (draw-metabar-group-row
                       medium row y selected-p
                       (eq (metabar-row-subject row)
                           (metabar-open-group frame))))
                     (:control
                      (draw-metabar-control-row
                       frame medium row y selected-p))
                     (:action
                      (draw-metabar-action-row medium row y selected-p)))
                   (incf y (metabar-row-height row))))
        (let ((status-y (- (metabar-logical-height frame)
                           +metabar-bottom-pad+ 5)))
          (if (metabar-diagnostic frame)
              (draw-text* medium "change failed · inspect the metabar"
                          +metabar-pad+ status-y :align-y :bottom
                          :text-size 12 :ink *metabar-error-ink*)
              (draw-text* medium "↑↓ choose  ·  ←→ change  ·  space applies"
                          +metabar-pad+ status-y :align-y :bottom
                          :text-size 12 :ink *metabar-muted-ink*)))))))

(defun metabar-mirror (frame &key (errorp t))
  "Return FRAME's embedded direct-GPU mirror."
  (let* ((sheet (frame-top-level-sheet frame))
         (mirror (and sheet (sheet-direct-mirror sheet))))
    (cond ((typep mirror 'luv-gpu-mirror) mirror)
          (errorp (error 'metabar-requires-direct-gpu :object mirror))
          (t nil))))

(defun validate-metabar-direct-presentation (frame)
  "Assert FRAME has no raster backing, fallback, or prepared image command."
  (let* ((mirror (metabar-mirror frame))
         (sheet (mirror-sheet mirror)))
    (when (mirror-texture mirror)
      (error 'metabar-direct-presentation-violation
             :reason "the embedded mirror acquired a backing texture"))
    (dolist (painted-sheet (gpu-sheet-paint-order sheet))
      (let ((medium (gpu-sheet-presentation-medium painted-sheet)))
        (when (and (typep medium 'luv-gpu-medium)
                   (gpu-medium-fallback-report medium))
          (error 'metabar-direct-presentation-violation
                 :reason (format nil "~S used decomposed primitive fallbacks"
                                 painted-sheet)))))
    (when (find-if (lambda (command)
                     (typep command 'gpu-prepared-image-command))
                   (gpu-mirror-prepared-commands mirror))
      (error 'metabar-direct-presentation-violation
             :reason "a rasterized image command reached the prepared stream"))
    frame))

(defun repaint-metabar (frame)
  "Publish FRAME's cached semantic stream without drawable acquisition or wait."
  (alexandria:when-let ((mirror (metabar-mirror frame :errorp nil)))
    (unless (and (mirror-embedded-p mirror) (null (mirror-texture mirror)))
      (error 'metabar-requires-direct-gpu :object mirror))
    (repaint-gpu-mirror mirror)
    (validate-metabar-direct-presentation frame)
    (setf (metabar-dirty-p frame) nil))
  frame)

(defun prepare-metabar (frame)
  "Ensure FRAME's next direct composition sees its latest semantic revision."
  (let ((mirror (metabar-mirror frame)))
    (if (or (metabar-dirty-p frame)
            (null (gpu-mirror-prepared-commands mirror)))
        (repaint-metabar frame)
        (prepare-gpu-mirror-compositor mirror)))
  frame)

;;; ---------------------------------------------------------------------
;;; Logical placement and input.

(defun metabar-panel-scale (source-logical-extent viewport-logical-extent)
  (destructuring-bind (source-width source-height) source-logical-extent
    (declare (ignore source-width))
    (destructuring-bind (viewport-width viewport-height)
        viewport-logical-extent
      (declare (ignore viewport-width))
      (min 1.0
           (/ (max 1.0 (- viewport-height
                          (* 2 +metabar-viewport-margin+)))
              source-height)))))

(defun metabar-placement-state
    (source-logical-extent viewport-logical-extent slide)
  "Return the left-drawer affine state in destination logical pixels.

At scale one, one metabar coordinate is one logical viewport pixel.  A 2x
drawable therefore evaluates every analytic edge and glyph at 2x native
samples rather than enlarging a pane raster."
  (destructuring-bind (source-width source-height) source-logical-extent
    (destructuring-bind (viewport-width viewport-height)
        viewport-logical-extent
      (let* ((scale (metabar-panel-scale source-logical-extent
                                         viewport-logical-extent))
             (half-width (/ (* source-width scale) viewport-width))
             (half-height (/ (* source-height scale) viewport-height))
             (slide (max 0.0 (min 1.0 slide)))
             (eased (- 1.0 (expt (- 1.0 slide) 3)))
             (center-x (+ -1.0 (* half-width (- (* 2.0 eased) 1.0)))))
        (make-array
         12 :element-type 'single-float
         :initial-contents
         (mapcar (lambda (value) (coerce value 'single-float))
                 (list center-x 0.0 0.0 1.0
                       half-width 0.0 0.0 0.0
                       0.0 half-height 0.0 0.0)))))))

(defun metabar-screen-state (frame viewport-logical-extent slide)
  (metabar-placement-state
   (list +metabar-width+ (metabar-logical-height frame))
   viewport-logical-extent slide))

(defun metabar-local-coordinate
    (frame pointer-x pointer-y viewport-logical-extent slide)
  "Map a destination-logical pointer into FRAME, or return NIL."
  (let* ((source-extent (list +metabar-width+
                              (metabar-logical-height frame)))
         (scale (metabar-panel-scale source-extent viewport-logical-extent))
         (display-width (* +metabar-width+ scale))
         (display-height (* (metabar-logical-height frame) scale))
         (slide (max 0.0 (min 1.0 slide)))
         (eased (- 1.0 (expt (- 1.0 slide) 3)))
         (left (* display-width (- eased 1.0)))
         (top (* 0.5 (- (second viewport-logical-extent) display-height))))
    (when (and (<= left pointer-x (+ left display-width))
               (<= top pointer-y (+ top display-height)))
      (values (/ (- pointer-x left) scale)
              (/ (- pointer-y top) scale)))))

(defun select-metabar-row (frame index)
  (let ((count (length (metabar-rows frame))))
    (when (plusp count)
      (let ((selection (mod index count)))
        (unless (= selection (metabar-selected frame))
          (setf (metabar-selected frame) selection)
          (invalidate-metabar frame)))))
  frame)

(defun open-metabar-group (frame group)
  (when (and group (not (eq group (metabar-open-group frame))))
    (setf (metabar-open-group frame) group)
    (rebuild-metabar-rows frame))
  (let ((index (position group (metabar-rows frame)
                         :key (lambda (row)
                                (and (eq :group (metabar-row-kind row))
                                     (metabar-row-subject row)))
                         :test #'eq)))
    (when index (select-metabar-row frame index)))
  frame)

(defun nudge-metabar-selection (frame direction multiplier)
  (alexandria:when-let ((row (metabar-selected-row frame)))
    (case (metabar-row-kind row)
      (:control
       (queue-metabar-operation
        frame :step (metabar-row-subject row) (* direction multiplier)))
      (:group
       (open-metabar-group frame (metabar-row-subject row)))))
  frame)

(defun press-metabar-selection (frame)
  (alexandria:when-let ((row (metabar-selected-row frame)))
    (ecase (metabar-row-kind row)
      (:group (open-metabar-group frame (metabar-row-subject row)))
      (:control
       (when (eq :switch (metabar-row-control-kind row))
         (queue-metabar-operation frame :toggle (metabar-row-subject row))))
      (:action
       (queue-metabar-operation frame :action (metabar-row-subject row)))))
  frame)

(defun set-metabar-from-track (frame row fraction)
  (let* ((control (metabar-row-subject row))
         (fraction (max 0.0 (min 1.0 fraction))))
    (setf (metabar-preview-fraction frame control) fraction)
    (queue-metabar-operation frame :set-fraction control fraction))
  frame)

(defun handle-metabar-key-event (frame event)
  "Handle a portable key press without invoking OWNER; return :CONTINUE or :DISMISS."
  (check-type event luv:canvas-key-press-event)
  (when (luv:canvas-key-event-repeat-p event)
    (return-from handle-metabar-key-event :continue))
  (let ((key (luv:canvas-key-event-key-name event))
        (multiplier
          (if (member :shift (luv:canvas-key-event-modifiers event)) 10 1)))
    (case key
      (:escape :dismiss)
      ((:return :keypad-enter)
       (if (and (metabar-selected-row frame)
                (eq :group (metabar-row-kind (metabar-selected-row frame))))
           (progn (press-metabar-selection frame) :continue)
           :dismiss))
      ((:up :k)
       (select-metabar-row frame (1- (metabar-selected frame)))
       :continue)
      ((:down :j)
       (select-metabar-row frame (1+ (metabar-selected frame)))
       :continue)
      ((:left :h)
       (nudge-metabar-selection frame -1 multiplier)
       :continue)
      ((:right :l)
       (nudge-metabar-selection frame 1 multiplier)
       :continue)
      (:space
       (press-metabar-selection frame)
       :continue)
      (t :continue))))

(defun handle-metabar-pointer-event (frame event local-x local-y)
  "Handle one pointer EVENT already projected into FRAME logical coordinates."
  (let ((row-index (and local-y (metabar-row-at frame local-y))))
    (typecase event
      (luv:canvas-pointer-button-release-event
       (setf (metabar-dragging frame) nil)
       :continue)
      (luv:canvas-pointer-motion-event
       (alexandria:when-let* ((control (metabar-dragging frame))
                              (x local-x)
                              (row (metabar-control-row frame control)))
         (set-metabar-from-track
          frame row (/ (- x +metabar-track-left+)
                       (- +metabar-track-right+ +metabar-track-left+))))
       :continue)
      (luv:canvas-pointer-wheel-event
       (when row-index
         (let ((row (nth row-index (metabar-rows frame))))
           (when (eq :control (metabar-row-kind row))
             (select-metabar-row frame row-index)
             (let ((delta (luv:canvas-pointer-event-scroll-y event)))
               (unless (zerop delta)
                 (nudge-metabar-selection frame (if (plusp delta) 1 -1) 1))))))
       :continue)
      (luv:canvas-pointer-button-press-event
       (when (and row-index
                  (eq :left (luv:canvas-pointer-event-button event)))
         (select-metabar-row frame row-index)
         (let ((row (nth row-index (metabar-rows frame))))
           (case (metabar-row-kind row)
             (:group (open-metabar-group frame (metabar-row-subject row)))
             (:action
              (queue-metabar-operation
               frame :action (metabar-row-subject row)))
             (:control
              (if (eq :scalar (metabar-row-control-kind row))
                  (cond
                    ((< local-x +metabar-track-left+)
                     (nudge-metabar-selection frame -1 1))
                    ((> local-x +metabar-track-right+)
                     (nudge-metabar-selection frame 1 1))
                    (t
                     (setf (metabar-dragging frame)
                           (metabar-row-subject row))
                     (set-metabar-from-track
                      frame row (/ (- local-x +metabar-track-left+)
                                   (- +metabar-track-right+
                                      +metabar-track-left+)))))
                  (queue-metabar-operation
                   frame :toggle (metabar-row-subject row)))))))
       :continue)
      (t :continue))))

;;; ---------------------------------------------------------------------
;;; Embedded frame ownership.

(defun make-embedded-metabar
    (owner canvas context device &key (title "metabar"))
  "Create OWNER's direct-GPU metabar on its existing CANVAS and DEVICE."
  (let* ((vocabulary (capture-metabar-vocabulary owner))
         (height (metabar-natural-height-for owner vocabulary))
         (port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target* canvas)
                 (*embedded-mirror-context* context)
                 (*embedded-mirror-device* device)
                 (*metabar-construction-height* height))
             (make-application-frame
              'metabar :frame-manager manager :enable t
              :owner owner :vocabulary vocabulary :logical-height height))))
    (setf (frame-pretty-name frame) title)
    (make-gpu-frame-background-transparent frame)
    (handler-case
        (let ((mirror (metabar-mirror frame)))
          (unless (and (mirror-embedded-p mirror)
                       (null (mirror-texture mirror)))
            (error 'metabar-requires-direct-gpu :object mirror))
          (repaint-metabar frame)
          frame)
      (error (condition)
        (unless (eq :disowned (frame-state frame))
          (destroy-frame frame))
        (error condition)))))

(defun destroy-metabar (frame)
  "Release FRAME's mirror, retained buffers, and McCLIM ownership."
  (check-type frame metabar)
  (unless (eq :disowned (frame-state frame))
    (destroy-frame frame))
  nil)
