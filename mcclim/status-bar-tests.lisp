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

(deftest status-bar-composes-base-and-game-defined-clos-channels
  (let* ((owner (make-instance 'status-bar-test-owner))
         (bar (make-status-bar-test-frame owner)))
    (setf (mcluv::status-bar-last-sample-ticks bar) 0)
    (mcluv:refresh-status-bar bar 900 :now 1)
    (ok (equal '(:application :pid :fps :heap :lobby :worktree :game-field)
               (mapcar #'mcluv:status-bar-field-channel
                       (mcluv:status-bar-visible-fields bar))))
    (ok (string= "test game"
                 (mcluv:status-bar-field-value
                  (first (mcluv:status-bar-visible-fields bar)))))
    (ok (string= "ready"
                 (mcluv:status-bar-field-value
                  (car (last (mcluv:status-bar-visible-fields bar))))))))

(deftest status-bar-sampling-and-semantic-repaint-are-throttled
  (let* ((owner (make-instance 'stable-status-bar-test-owner))
         (bar (make-status-bar-test-frame owner))
         (ticks internal-time-units-per-second))
    (setf (mcluv::status-bar-last-sample-ticks bar) 0)
    (mcluv:refresh-status-bar bar 900 :now ticks)
    (let ((revision (mcluv::status-bar-revision bar)))
      (ok (= 1 (status-bar-test-samples owner)))
      (loop for frame from 1 to 100
            do (mcluv:refresh-status-bar
                bar 900 :now (+ ticks frame)))
      (ok (= 1 (status-bar-test-samples owner)))
      (ok (= revision (mcluv::status-bar-revision bar)))
      ;; A later sample runs once, but identical semantic fields do not dirty
      ;; or revise the retained stream.
      (mcluv:refresh-status-bar bar 900 :now (* 2 ticks))
      (ok (= 2 (status-bar-test-samples owner)))
      (ok (= revision (mcluv::status-bar-revision bar))))))

(deftest static-status-bar-prepares-live-shader-revisions
  (let ((bar (make-status-bar-test-frame)))
    (setf (mcluv::status-bar-dirty-p bar) nil)
    (multiple-value-bind (probe revision)
        (mount-static-direct-preparation-probe bar)
      (mcluv:prepare-status-bar bar)
      (ok (equal (list revision)
                 (current-revision-preparation-probe-revisions probe))))))

(deftest status-bar-drops-trailing-fields-to-fit-a-narrow-window
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
      (ok (string= first-two
                   (mcluv::status-bar-display-string bar medium)))
      (ok (<= (mcluv::measured-status-bar-text-width
               medium (mcluv::status-bar-display-string bar medium))
              (- (mcluv:status-bar-logical-width bar) pad)))
      ;; Fitting is a stable prefix policy: narrower bars neither rescale text
      ;; nor skip ahead to a later, shorter field.
      (setf (mcluv:status-bar-logical-width bar)
            (+ pad (floor (1- first-two-width))))
      (ok (string= first (mcluv::status-bar-display-string bar medium)))
      (setf (mcluv:status-bar-logical-width bar)
            (+ pad (max 0 (floor (1- first-width)))))
      (ok (string= "" (mcluv::status-bar-display-string bar medium))))))

(deftest status-bar-fits-wide-glyphs-by-shaped-advance
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
    (ok (<= old-eight-pixel-estimate available-width))
    (ok (> actual-width available-width))
    (ok (string= "" (mcluv::status-bar-display-string bar medium)))))

(deftest status-bar-samples-only-bounded-summary-data
  (let* ((owner (make-instance 'status-bar-summary-test-owner))
         (bar (make-status-bar-test-frame owner)))
    (ok (string= "online 7"
                 (mcluv:status-bar-channel-value :lobby owner bar)))
    (let ((text
            (mcluv::bounded-status-bar-text
             (make-string 10000 :initial-element #\x))))
      (ok (= mcluv::+status-bar-maximum-field-characters+ (length text)))
      (ok (string= "..." text
                   :start2 (- (length text) 3))))
    (let* ((owned (copy-seq "ready"))
           (snapshot (mcluv::bounded-status-bar-text owned)))
      (setf (char owned 0) #\X)
      (ok (string= "ready" snapshot)))))

(deftest status-bar-is-top-aligned-and-native-destination-resolution
  (let* ((bar (make-status-bar-test-frame))
         (logical '(900 600))
         (drawable '(1800 1200))
         (state (mcluv:status-bar-screen-state bar logical))
         (half-width (aref state 4))
         (half-height (aref state 9))
         (center-y (aref state 1)))
    (ok (< (abs (- 900.0 (* half-width (first logical)))) 1.0e-4))
    (ok (< (abs (- 28.0 (* half-height (second logical)))) 1.0e-4))
    (ok (< (abs (- 1800.0 (* half-width (first drawable)))) 1.0e-4))
    (ok (< (abs (- 56.0 (* half-height (second drawable)))) 1.0e-4))
    ;; The upper edge is exactly NDC -1; the bar does not reserve a margin.
    (ok (< (abs (+ 1.0 (- center-y half-height))) 1.0e-6))))

(deftest status-bar-panel-is-translucent-analytic-and-never-an-image
  (let ((medium (fresh-gpu-medium)))
    (setf (clim:medium-ink medium) mcluv::*status-bar-panel-ink*)
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 0 0 900 28 0 t)
    (let ((vertices (mcluv::gpu-medium-analytic-vertices medium))
          (commands (mcluv::gpu-medium-commands medium)))
      (ok (< (abs (- 0.72 (aref vertices 2))) 1.0e-5))
      (ok (= 1 (length commands)))
      (ok (typep (aref commands 0) 'mcluv::gpu-analytic-command))
      (ok (null (mcluv:gpu-medium-fallback-report medium)))
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
          (ok (= 1 (length prepared)))
          (ok (null
               (find-if
                (lambda (command)
                  (typep command 'mcluv::gpu-prepared-image-command))
                prepared))))))))
