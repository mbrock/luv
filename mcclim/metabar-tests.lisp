(in-package #:mcluv.tests)

(defclass metabar-test-owner ()
  ((gain :initform 0.5 :accessor metabar-test-gain)
   (enabled-p :initform nil :accessor metabar-test-enabled-p)
   (actions :initform 0 :accessor metabar-test-actions)
   (fail-set-p :initform nil :accessor metabar-test-fail-set-p)))

(defmethod mcluv::metabar-groups-for ((owner metabar-test-owner))
  (declare (ignore owner))
  '(:tuning))

(defmethod mcluv::metabar-controls-for
    ((owner metabar-test-owner) (group (eql :tuning)))
  (declare (ignore owner group))
  '(:gain :enabled))

(defmethod mcluv::metabar-actions-for ((owner metabar-test-owner))
  (declare (ignore owner))
  '(:fire))

(defmethod mcluv::metabar-control-kind
    ((control (eql :gain)) (owner metabar-test-owner))
  (declare (ignore control owner))
  :scalar)

(defmethod mcluv::metabar-control-kind
    ((control (eql :enabled)) (owner metabar-test-owner))
  (declare (ignore control owner))
  :switch)

(defmethod mcluv::metabar-control-label
    ((control (eql :gain)) (owner metabar-test-owner))
  (declare (ignore control owner))
  "gain")

(defmethod mcluv::metabar-control-label
    ((control (eql :enabled)) (owner metabar-test-owner))
  (declare (ignore control owner))
  "enabled")

(defmethod mcluv::metabar-control-value
    ((control (eql :gain)) (owner metabar-test-owner))
  (declare (ignore control))
  (metabar-test-gain owner))

(defmethod mcluv::metabar-control-value
    ((control (eql :enabled)) (owner metabar-test-owner))
  (declare (ignore control))
  (metabar-test-enabled-p owner))

(defmethod mcluv::metabar-control-value-label
    ((control (eql :gain)) (owner metabar-test-owner) value)
  (declare (ignore control owner))
  (format nil "~,1F" value))

(defmethod mcluv::metabar-control-value-label
    ((control (eql :enabled)) (owner metabar-test-owner) value)
  (declare (ignore control owner))
  (if value "on" "off"))

(defmethod mcluv::metabar-control-fraction
    ((control (eql :gain)) (owner metabar-test-owner) value)
  (declare (ignore control owner))
  value)

(defmethod mcluv::metabar-control-fraction
    ((control (eql :enabled)) (owner metabar-test-owner) value)
  (declare (ignore control owner))
  (if value 1.0 0.0))

(defmethod mcluv::metabar-control-update-policy
    ((control (eql :gain)) (owner metabar-test-owner))
  (declare (ignore control owner))
  :commit-on-release)

(defmethod mcluv::perform-metabar-control-step
    ((control (eql :gain)) (owner metabar-test-owner)
     direction multiplier)
  (declare (ignore control))
  (setf (metabar-test-gain owner)
        (max 0.0 (min 1.0 (+ (metabar-test-gain owner)
                             (* direction multiplier 0.1))))))

(defmethod mcluv::perform-metabar-control-step
    ((control (eql :enabled)) (owner metabar-test-owner)
     direction multiplier)
  (declare (ignore control multiplier))
  (setf (metabar-test-enabled-p owner) (plusp direction)))

(defmethod mcluv::perform-metabar-control-set-fraction
    ((control (eql :gain)) (owner metabar-test-owner) fraction)
  (declare (ignore control))
  (when (metabar-test-fail-set-p owner)
    (error "scripted metabar commit failure"))
  (setf (metabar-test-gain owner) fraction))

(defmethod mcluv::perform-metabar-control-toggle
    ((control (eql :enabled)) (owner metabar-test-owner))
  (declare (ignore control))
  (setf (metabar-test-enabled-p owner)
        (not (metabar-test-enabled-p owner))))

(defmethod mcluv::metabar-action-label
    ((action (eql :fire)) (owner metabar-test-owner))
  (declare (ignore action owner))
  "fire")

(defmethod mcluv::perform-metabar-action
    ((action (eql :fire)) (owner metabar-test-owner))
  (declare (ignore action))
  (incf (metabar-test-actions owner)))

(defun make-metabar-test-frame (&optional (owner (make-instance 'metabar-test-owner)))
  (let* ((vocabulary (mcluv::capture-metabar-vocabulary owner))
         (height (mcluv::metabar-natural-height-for owner vocabulary)))
    (clim:make-application-frame
     'mcluv::metabar :owner owner :vocabulary vocabulary
     :logical-height height)))

(defun metabar-test-key (key &key modifiers)
  (make-instance 'luv:canvas-key-press-event
                 :timestamp 0 :key-name key :modifiers modifiers))

(defun metabar-test-pointer (class &key (x 0.0) (y 0.0) button)
  (apply #'make-instance class
         :timestamp 0 :x x :y y
         (when button (list :button button :clicks 1))))

(define-test metabar-is-an-open-owner-and-control-protocol
  (dolist (name '(mcluv::metabar-groups-for
                  mcluv::metabar-group-label
                  mcluv::metabar-controls-for
                  mcluv::metabar-actions-for
                  mcluv::metabar-control-kind
                  mcluv::metabar-control-value
                  mcluv::perform-metabar-control-step
                  mcluv::perform-metabar-control-set-fraction
                  mcluv::perform-metabar-control-toggle
                  mcluv::perform-metabar-action))
    (true (typep (fdefinition name) 'generic-function)))
  (let* ((owner (make-instance 'metabar-test-owner))
         (frame (make-metabar-test-frame owner)))
    (true (equal '(:tuning)
                 (mcluv::metabar-vocabulary-groups
                  (mcluv::metabar-vocabulary frame))))
    (true (equal '(:group :control :control :action)
                 (mapcar #'mcluv::metabar-row-kind
                         (mcluv::metabar-rows frame))))
    (true (= 240 (mcluv::metabar-logical-height frame)))))

(define-test static-metabar-prepares-live-shader-revisions
  (let ((frame (make-metabar-test-frame)))
    (setf (mcluv::metabar-dirty-p frame) nil)
    (multiple-value-bind (probe revision)
        (mount-static-direct-preparation-probe frame)
      (mcluv:prepare-metabar frame)
      (true (equal (list revision)
                   (current-revision-preparation-probe-revisions probe))))))

(define-test metabar-input-queues-and-frame-refresh-publishes-semantic-change
  (let* ((owner (make-instance 'metabar-test-owner))
         (frame (make-metabar-test-frame owner)))
    (mcluv::select-metabar-row frame 1)
    (true (eq :continue
              (mcluv::handle-metabar-key-event
               frame (metabar-test-key :right))))
    ;; Input has changed only retained UI state; the application is untouched.
    (true (= 0.5 (metabar-test-gain owner)))
    (true (= 1 (length (mcluv::metabar-pending-operations frame))))
    (mcluv::drain-metabar-operations frame)
    (true (< (abs (- 0.6 (metabar-test-gain owner))) 1.0e-6))
    (setf (mcluv::metabar-dirty-p frame) nil)
    (mcluv::refresh-metabar-state frame)
    (true (not (mcluv::metabar-dirty-p frame)))
    (setf (metabar-test-gain owner) 0.7)
    (mcluv::refresh-metabar-state frame)
    (true (mcluv::metabar-dirty-p frame))
    (mcluv::select-metabar-row frame 2)
    (mcluv::handle-metabar-key-event frame (metabar-test-key :space))
    (true (not (metabar-test-enabled-p owner)))
    (mcluv::drain-metabar-operations frame)
    (true (metabar-test-enabled-p owner))
    (mcluv::select-metabar-row frame 3)
    (mcluv::handle-metabar-key-event frame (metabar-test-key :space))
    (true (zerop (metabar-test-actions owner)))
    (mcluv::drain-metabar-operations frame)
    (true (= 1 (metabar-test-actions owner)))
    (mcluv::select-metabar-row frame 1)
    (mcluv::handle-metabar-key-event frame (metabar-test-key :right))
    (mcluv::handle-metabar-key-event frame (metabar-test-key :left))
    (true (null (mcluv::metabar-pending-operations frame)))))

(define-test commit-controls-preview-and-coalesce-until-pointer-release
  (let* ((owner (make-instance 'metabar-test-owner))
         (frame (make-metabar-test-frame owner))
         (row-top (mcluv::metabar-row-top frame 1))
         (track-y (+ row-top 52))
         (press (metabar-test-pointer
                 'luv:canvas-pointer-button-press-event
                 :button :left))
         (motion (metabar-test-pointer
                  'luv:canvas-pointer-motion-event))
         (release (metabar-test-pointer
                   'luv:canvas-pointer-button-release-event
                   :button :left)))
    (mcluv::handle-metabar-pointer-event frame press 330.0 track-y)
    (mcluv::handle-metabar-pointer-event frame motion 365.8 track-y)
    (true (= 1 (length (mcluv::metabar-pending-operations frame))))
    (true (mcluv::metabar-preview-fraction frame :gain))
    (mcluv::drain-metabar-operations frame)
    (true (= 0.5 (metabar-test-gain owner)))
    (true (= 1 (length (mcluv::metabar-pending-operations frame))))
    (mcluv::handle-metabar-pointer-event frame release nil nil)
    (mcluv::drain-metabar-operations frame)
    (true (< (abs (- 0.9 (metabar-test-gain owner))) 0.01))
    (true (null (mcluv::metabar-pending-operations frame)))
    (true (null (mcluv::metabar-preview-fraction frame :gain)))))

(define-test clearing-a-quantized-preview-is-a-semantic-revision
  (let ((frame (make-metabar-test-frame)))
    (setf (mcluv::metabar-preview-fraction frame :gain) 0.5
          (mcluv::metabar-dirty-p frame) nil)
    ;; An application may quantize a committed preview back to the value the
    ;; row already cached.  Removing that preview still changes the picture.
    (mcluv::clear-metabar-preview-fraction frame :gain)
    (true (mcluv::metabar-dirty-p frame))
    (true (null (mcluv::metabar-preview-fraction frame :gain)))))

(define-test a-failed-commit-clears-its-optimistic-preview
  (let* ((owner (make-instance 'metabar-test-owner))
         (frame (make-metabar-test-frame owner)))
    (setf (metabar-test-fail-set-p owner) t
          (mcluv::metabar-preview-fraction frame :gain) 0.9
          (mcluv::metabar-pending-operations frame)
          (list (mcluv::make-metabar-operation
                 :kind :set-fraction :subject :gain :argument 0.9)))
    (mcluv::drain-metabar-operations frame)
    (true (= 0.5 (metabar-test-gain owner)))
    (true (null (mcluv::metabar-preview-fraction frame :gain)))
    (true (search "scripted metabar commit failure"
                  (princ-to-string (mcluv::metabar-diagnostic frame))))))

(define-test metabar-layout-is-logical-and-native-destination-resolution
  (let* ((owner (make-instance 'metabar-test-owner))
         (frame (make-metabar-test-frame owner))
         (logical '(1344 840))
         (drawable '(2688 1680))
         (state (mcluv::metabar-screen-state frame logical 1.0))
         (half-width (aref state 4))
         (half-height (aref state 9)))
    (true (< (abs (- 460.0 (* half-width (first logical)))) 1.0e-4))
    (true (< (abs (- 240.0 (* half-height (second logical)))) 1.0e-4))
    ;; The same logical transform receives twice the native samples on a 2x
    ;; drawable; no intermediate panel raster is resized.
    (true (< (abs (- 920.0 (* half-width (first drawable)))) 1.0e-4))
    (true (< (abs (- 480.0 (* half-height (second drawable)))) 1.0e-4))
    (multiple-value-bind (x y)
        (mcluv::metabar-local-coordinate frame 230.0 420.0 logical 1.0)
      (true (< (abs (- 230.0 x)) 1.0e-4))
      (true (< (abs (- 120.0 y)) 1.0e-4)))))

(define-test metabar-panel-is-translucent-analytic-and-prepares-no-image
  (let ((medium (fresh-gpu-medium)))
    (setf (clim:medium-ink medium) mcluv::*metabar-panel-ink*)
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 0 0 460 240 15 t)
    (let ((vertices (mcluv::gpu-medium-analytic-vertices medium))
          (semantic-commands (mcluv::gpu-medium-commands medium)))
      (true (< (abs (- 0.90 (aref vertices 2))) 1.0e-5))
      (true (= 1 (length semantic-commands)))
      (true (typep (aref semantic-commands 0)
                   'mcluv::gpu-analytic-command))
      (true (null (mcluv:gpu-medium-fallback-report medium)))
      (let* ((sheet
               (make-instance 'mcluv::metabar-pane
                              :region
                              (clim:make-bounding-rectangle 0 0 460 240)))
             (mirror
               (make-instance 'mcluv:luv-gpu-mirror
                              :sheet sheet :target nil :embedded-p t)))
        (multiple-value-bind (prepared text-data)
            (mcluv::prepare-gpu-frame-commands mirror semantic-commands)
          (declare (ignore text-data))
          (true (= 1 (length prepared)))
          (true (typep (first prepared) 'mcluv::gpu-analytic-command))
          (true (null
                 (find-if
                  (lambda (command)
                    (typep command 'mcluv::gpu-prepared-image-command))
                  prepared))))))))
