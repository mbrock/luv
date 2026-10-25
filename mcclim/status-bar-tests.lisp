(in-package #:mcluv.tests)

(defclass status-bar-test-owner ()
  ((samples :initform 0 :accessor status-bar-test-samples)))

(defclass stable-status-bar-test-owner (status-bar-test-owner) ())

(defclass status-bar-summary-test-client () ())

(defclass status-bar-summary-test-owner (status-bar-test-owner)
  ((client :initform (make-instance 'status-bar-summary-test-client)
           :reader status-bar-summary-test-client)))

(defmethod luv.lobby:lobby-client-summary
    ((client status-bar-summary-test-client))
  (declare (ignore client))
  (values :online 7 nil 42))

(defmethod mcluv:status-bar-lobby-client
    ((owner status-bar-summary-test-owner))
  (status-bar-summary-test-client owner))

(defmethod mcluv:status-bar-channels-for
    ((owner stable-status-bar-test-owner))
  (declare (ignore owner))
  '(:game-field))

(defmethod mcluv:status-bar-application-name ((owner status-bar-test-owner))
  (declare (ignore owner))
  "test game")

(defmethod mcluv:status-bar-source-root ((owner status-bar-test-owner))
  (declare (ignore owner))
  nil)

(defmethod mcluv:status-bar-channels-for ((owner status-bar-test-owner))
  (append (call-next-method) '(:game-field)))

(defmethod mcluv:status-bar-channel-label
    ((channel (eql :game-field)) (owner status-bar-test-owner))
  (declare (ignore channel owner))
  "game")

(defmethod mcluv:status-bar-channel-value
    ((channel (eql :game-field)) (owner status-bar-test-owner) bar)
  (declare (ignore channel bar))
  (incf (status-bar-test-samples owner))
  "ready")

(defun make-status-bar-test-frame
    (&optional (owner (make-instance 'status-bar-test-owner))
               (logical-width 900))
  (clim:make-application-frame
   'mcluv:status-bar :owner owner
                      :logical-width logical-width :worktree nil))

(define-test status-bar-composes-base-and-game-defined-clos-channels
  (let* ((owner (make-instance 'status-bar-test-owner))
         (bar (make-status-bar-test-frame owner)))
    (setf (mcluv::status-bar-last-sample-ticks bar) 0)
    (mcluv:refresh-status-bar bar 900 :now 1)
    (true (equal '(:application :pid :fps :heap :lobby :worktree :game-field)
                 (mapcar #'mcluv:status-bar-field-channel
                         (mcluv:status-bar-visible-fields bar))))
    (true (string= "test game"
                   (mcluv:status-bar-field-value
                    (first (mcluv:status-bar-visible-fields bar)))))
    (true (string= "ready"
                   (mcluv:status-bar-field-value
                    (car (last (mcluv:status-bar-visible-fields bar))))))))

(define-test status-bar-sampling-and-semantic-repaint-are-throttled
  (let* ((owner (make-instance 'stable-status-bar-test-owner))
         (bar (make-status-bar-test-frame owner))
         (ticks internal-time-units-per-second))
    (setf (mcluv::status-bar-last-sample-ticks bar) 0)
    (mcluv:refresh-status-bar bar 900 :now ticks)
    (let ((revision (mcluv::status-bar-revision bar)))
      (true (= 1 (status-bar-test-samples owner)))
      (loop for frame from 1 to 100
            do (mcluv:refresh-status-bar
                bar 900 :now (+ ticks frame)))
      (true (= 1 (status-bar-test-samples owner)))
      (true (= revision (mcluv::status-bar-revision bar)))
      ;; A later sample runs once, but identical semantic fields do not dirty
      ;; or revise the retained stream.
      (mcluv:refresh-status-bar bar 900 :now (* 2 ticks))
      (true (= 2 (status-bar-test-samples owner)))
      (true (= revision (mcluv::status-bar-revision bar))))))

(define-test static-status-bar-prepares-live-shader-revisions
  (let ((bar (make-status-bar-test-frame)))
    (setf (mcluv::status-bar-dirty-p bar) nil)
    (multiple-value-bind (probe revision)
        (mount-static-direct-preparation-probe bar)
      (mcluv:prepare-status-bar bar)
      (true (equal (list revision)
                   (current-revision-preparation-probe-revisions probe))))))

(define-test status-bar-drops-trailing-fields-to-fit-a-narrow-window
  (let ((bar (make-status-bar-test-frame
              (make-instance 'stable-status-bar-test-owner) 180))
        (medium (fresh-gpu-medium)))
    (setf (mcluv:status-bar-visible-fields bar)
          (list (mcluv::make-status-bar-field
                 :channel :application :label nil :value "LUFT")
                (mcluv::make-status-bar-field
                 :channel :pid :label "pid" :value "123")
                (mcluv::make-status-bar-field
                 :channel :fps :label "fps" :value "60")))
    (let* ((first "LUFT")
           (first-two "LUFT  ·  pid 123")
           (pad (* 2 mcluv::+status-bar-horizontal-pad+))
           (first-width
             (mcluv::measured-status-bar-text-width medium first))
           (first-two-width
             (mcluv::measured-status-bar-text-width medium first-two)))
      (setf (mcluv:status-bar-logical-width bar)
            (+ pad (ceiling first-two-width)))
      (true (string= first-two
                     (mcluv::status-bar-display-string bar medium)))
      (true (<= (mcluv::measured-status-bar-text-width
                 medium (mcluv::status-bar-display-string bar medium))
                (- (mcluv:status-bar-logical-width bar) pad)))
      ;; Fitting is a stable prefix policy: narrower bars neither rescale text
      ;; nor skip ahead to a later, shorter field.
      (setf (mcluv:status-bar-logical-width bar)
            (+ pad (floor (1- first-two-width))))
      (true (string= first (mcluv::status-bar-display-string bar medium)))
      (setf (mcluv:status-bar-logical-width bar)
            (+ pad (max 0 (floor (1- first-width)))))
      (true (string= "" (mcluv::status-bar-display-string bar medium))))))

(define-test status-bar-fits-wide-glyphs-by-shaped-advance
  (let* ((bar (make-status-bar-test-frame
               (make-instance 'stable-status-bar-test-owner)))
         (medium (fresh-gpu-medium))
         (text "WWWWWWWWWWWW")
         (actual-width
           (mcluv::measured-status-bar-text-width medium text))
         (old-eight-pixel-estimate (* 8 (length text)))
         (available-width old-eight-pixel-estimate))
    (setf (mcluv:status-bar-visible-fields bar)
          (list (mcluv::make-status-bar-field
                 :channel :wide :label nil :value text))
          (mcluv:status-bar-logical-width bar)
          (+ available-width (* 2 mcluv::+status-bar-horizontal-pad+)))
    (true (<= old-eight-pixel-estimate available-width))
    (true (> actual-width available-width))
    (true (string= "" (mcluv::status-bar-display-string bar medium)))))

(define-test status-bar-samples-only-bounded-summary-data
  (let* ((owner (make-instance 'status-bar-summary-test-owner))
         (bar (make-status-bar-test-frame owner)))
    (true (string= "online 7"
                   (mcluv:status-bar-channel-value :lobby owner bar)))
    (let ((text
            (mcluv::bounded-status-bar-text
             (make-string 10000 :initial-element #\x))))
      (true (= mcluv::+status-bar-maximum-field-characters+ (length text)))
      (true (string= "..." text
                     :start2 (- (length text) 3))))
    (let* ((owned (copy-seq "ready"))
           (snapshot (mcluv::bounded-status-bar-text owned)))
      (setf (char owned 0) #\X)
      (true (string= "ready" snapshot)))))

(define-test status-bar-is-top-aligned-and-native-destination-resolution
  (let* ((bar (make-status-bar-test-frame))
         (logical '(900 600))
         (drawable '(1800 1200))
         (state (mcluv:status-bar-screen-state bar logical))
         (half-width (aref state 4))
         (half-height (aref state 9))
         (center-y (aref state 1)))
    (true (< (abs (- 900.0 (* half-width (first logical)))) 1.0e-4))
    (true (< (abs (- 28.0 (* half-height (second logical)))) 1.0e-4))
    (true (< (abs (- 1800.0 (* half-width (first drawable)))) 1.0e-4))
    (true (< (abs (- 56.0 (* half-height (second drawable)))) 1.0e-4))
    ;; The upper edge is exactly NDC -1; the bar does not reserve a margin.
    (true (< (abs (+ 1.0 (- center-y half-height))) 1.0e-6))))

(define-test status-bar-panel-is-opaque-analytic-and-never-an-image
  (let ((medium (fresh-gpu-medium)))
    (setf (clim:medium-ink medium) (mcluv::ui-ink :surface))
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 0 0 900 28 0 t)
    (let ((vertices (mcluv::gpu-medium-analytic-vertices medium))
          (commands (mcluv::gpu-medium-commands medium)))
      (true (< (abs (- 1.0 (aref vertices 2))) 1.0e-5))
      (true (= 1 (length commands)))
      (true (typep (aref commands 0) 'mcluv::gpu-analytic-command))
      (true (null (mcluv:gpu-medium-fallback-report medium)))
      (let* ((sheet
               (make-instance 'mcluv::status-bar-pane
                              :region
                              (clim:make-bounding-rectangle 0 0 900 28)))
             (mirror
               (make-instance 'mcluv:luv-gpu-mirror
                              :sheet sheet :target nil :embedded-p t)))
        (multiple-value-bind (prepared text-data)
            (mcluv::prepare-gpu-frame-commands mirror commands)
          (declare (ignore text-data))
          (true (= 1 (length prepared)))
          (true (null
                 (find-if
                  (lambda (command)
                    (typep command 'mcluv::gpu-prepared-image-command))
                  prepared))))))))

(defclass chip-status-bar-test-owner (stable-status-bar-test-owner) ())
(defmethod mcluv:status-bar-presentation ((owner chip-status-bar-test-owner))
  (declare (ignore owner))
  :chip)

(define-test game-status-is-matched-shadowed-monospace-without-a-clock-pill
  (dolist (channels '((:fps :daytime) (:fps) (:daytime) nil))
    (let* ((bar (make-status-bar-test-frame (make-instance 'chip-status-bar-test-owner)))
           (medium (fresh-gpu-medium))
           (pane (make-instance 'mcluv::status-bar-pane :frame bar
                                :region (clim:make-bounding-rectangle 0 0 900 64)))
           (font (mcluv::gpu-text-font-pathname (clim:make-text-style :fix :roman 16))))
      ;; A retained CPU command stream: no port, native window or device.
      (setf (slot-value pane 'clim:medium) medium
            (mcluv:status-bar-visible-fields bar)
            (loop for channel in channels
                  collect (mcluv::make-status-bar-field
                           :channel channel :value (if (eq channel :fps) "60" "06:45"))))
      (clim:with-output-recording-options (pane :record nil)
        (clim:handle-repaint pane clim:+everywhere+))
      (let ((commands (coerce (mcluv::gpu-medium-commands medium) 'list)))
        (true (= (* 2 (length channels)) (length commands)))
        (true (every (lambda (command) (typep command 'mcluv::gpu-text-command)) commands))
        (loop for (shadow foreground) on commands by #'cddr
              for fps-p = (string= "60 fps" (mcluv::gpu-text-command-string foreground))
              do (true (equal font (mcluv::gpu-text-command-font-pathname foreground)))
                 (true (equal font (mcluv::gpu-text-command-font-pathname shadow)))
                 (true (= 16 (mcluv::gpu-text-command-size foreground)
                          (mcluv::gpu-text-command-size shadow)))
                 (true (= 34 (mcluv::gpu-text-command-y foreground)))
                 (true (eq :baseline (mcluv::gpu-text-command-align-y foreground)))
                 (true (eq (if fps-p :left :right)
                           (mcluv::gpu-text-command-align-x foreground)))
                 (true (= (if fps-p 20 880) (mcluv::gpu-text-command-x foreground)))
                 (true (= (1+ (mcluv::gpu-text-command-x foreground))
                          (mcluv::gpu-text-command-x shadow)))
                 (true (= 35 (mcluv::gpu-text-command-y shadow)))
                 (true (equalp '(0 0 0 0.45) (mcluv::gpu-text-command-color shadow)))
                 (true (equalp '(1 1 1 0.92) (mcluv::gpu-text-command-color foreground))))))))

(define-test compact-status-keeps-its-margins-without-shrinking-type
  (let* ((bar (make-status-bar-test-frame (make-instance 'chip-status-bar-test-owner)))
         (medium (fresh-gpu-medium))
         (text "Daytime  11:00")
         (advance (mcluv::measured-status-bar-text-width medium text)))
    (setf (mcluv:status-bar-visible-fields bar)
          (list (mcluv::make-status-bar-field :channel :daytime :value text))
          (mcluv:status-bar-logical-width bar) (+ 64 (ceiling advance)))
    (true (= 64 (mcluv:status-bar-overlay-height bar)))
    (true (string= text (mcluv::status-bar-display-string bar medium)))
    (setf (mcluv:status-bar-logical-width bar) (+ 64 (floor (1- advance))))
    (true (string= "" (mcluv::status-bar-display-string bar medium)))))
