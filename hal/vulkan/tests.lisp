(in-package #:luv.tests)

(deftest default-provider-prefers-the-native-apple-backend
  #+darwin
  (ok (typep luv:*gpu-provider* 'luv:metal-gpu-provider))
  #-darwin
  (ok (typep luv:*gpu-provider* 'luv:vulkan-gpu-provider)))

(deftest renderer-readbacks-use-compressed-png-output
  (let ((pixels (make-array (* 64 64 4)
                            :element-type '(unsigned-byte 8)
                            :initial-element 255)))
    (uiop:with-temporary-file
        (:pathname pathname :prefix "luv-readback-" :suffix ".png")
      (luv:write-rgba-png pathname pixels 64 64 :rgba8-unorm)
      (with-open-file (stream pathname :element-type '(unsigned-byte 8))
        (ok (< (file-length stream) 1024))))))

(deftest cadence-clock-wakes-before-deadlines-and-preserves-phase
  (let* ((timestamps nil)
         (clock
           (luv:make-cadence-clock
            (lambda (canvas timestamp)
              (declare (ignore canvas))
              (push timestamp timestamps))
            :frames-per-second 60)))
    (luv:service-canvas-clock clock nil 10d0)
    (ok (equal timestamps '(10d0)))
    ;; The millisecond SDL wait must wake before the fractional deadline.
    (ok (= 16 (luv:clock-wait-timeout clock 10d0)))
    ;; An ordinary late wake does not move the cadence's original phase.
    (luv:service-canvas-clock clock nil 10.017d0)
    (ok (= 16 (luv:clock-wait-timeout clock 10.017d0)))
    (ok (< (abs (- (luv::cadence-clock-next-frame-time clock)
                   (+ 10d0 (/ 2d0 60d0))))
           1d-12))
    ;; A long pause skips missed frames rather than replaying them.
    (luv:service-canvas-clock clock nil 10.1d0)
    (ok (= 3 (length timestamps)))
    (ok (> (luv::cadence-clock-next-frame-time clock) 10.1d0))))

(deftest lazy-clock-gives-one-time-to-a-whole-turn
  (let ((samples '(10d0 20d0 30d0))
        (calls 0))
    (let ((clock
            (luv:make-lazy-clock
             :source (lambda ()
                       (incf calls)
                       (pop samples)))))
      (ok (= 10d0 (luv:lazy-clock-now clock)))
      (ok (= 10d0 (luv:lazy-clock-now clock)))
      (ok (= 1 calls))
      (luv:call-with-lazy-clock-time
       clock 99d0
       (lambda () (ok (= 99d0 (luv:lazy-clock-now clock)))))
      (ok (= 10d0 (luv:lazy-clock-now clock)))
      (ok (= 20d0 (luv:lazy-clock-now-unadjusted clock)))
      (ok (= 10d0 (luv:lazy-clock-now clock)))
      (luv:clear-lazy-clock clock)
      (ok (= 30d0 (luv:lazy-clock-now clock)))
      (ok (= 3 calls)))))

(deftest cadence-presentation-time-is-the-following-display-beat
  (let* ((clock (luv:make-lazy-clock :source (lambda () 10d0)))
         (cadence
           (luv:make-cadence-clock
            (lambda (canvas timestamp)
              (declare (ignore canvas timestamp)))
            :frames-per-second 60))
         (canvas
           (luv:make-sdl-canvas :clock cadence :time clock)))
    (ok (< (abs (- (luv:canvas-presentation-time canvas)
                   (+ 10d0 (/ 1d0 60d0))))
           1d-12))))

(deftest presentation-clock-asks-once-and-leaves-pacing-to-the-frame
  (let* ((timestamps nil)
        (clock
          (luv:make-presentation-clock
           (lambda (canvas timestamp)
             (declare (ignore canvas))
             (push timestamp timestamps)))))
    (ok (= 0 (luv:clock-wait-timeout clock 4d0)))
    (ok (luv:service-canvas-clock clock nil 4d0))
    (ok (equal timestamps '(4d0)))))

(deftest canvas-loop-failure-preserves-the-actionable-condition
  (let* ((canvas (luv:make-sdl-canvas))
         (root-cause (make-condition 'simple-error
                                     :format-control "event dispatch failed"))
         (request (luv::make-sdl-canvas-request :function #'identity)))
    (setf (luv::sdl-canvas-startup-error canvas) root-cause
          (luv::sdl-canvas-requests canvas) (list request))
    (luv::fail-sdl-canvas-requests
     canvas (luv::sdl-canvas-terminal-error canvas))
    (ok (eq root-cause (luv::sdl-canvas-request-error request)))
    (ok (null (luv::sdl-canvas-requests canvas)))))

(deftest slug-formats-retain-the-exact-vulkan-abi-values
  (ok (= 81 (cffi:foreign-enum-value 'lvk::format :r16g16-uint)))
  (ok (= 97
         (cffi:foreign-enum-value
          'lvk::format :r16g16b16a16-sfloat))))

(deftest video-planes-retain-the-exact-vulkan-abi-values
  (ok (= 9 (cffi:foreign-enum-value 'lvk::format :r8-unorm)))
  (ok (= 16 (cffi:foreign-enum-value 'lvk::format :r8g8-unorm)))
  (ok (= #x10
         (cffi:foreign-bitfield-value 'lvk::image-aspect-flags '(:plane-0))))
  (ok (= #x20
         (cffi:foreign-bitfield-value 'lvk::image-aspect-flags '(:plane-1))))
  (ok (= #x20
         (cffi:foreign-bitfield-value 'lvk::queue-flags '(:video-decode)))))

(deftest premultiplied-alpha-retains-the-exact-vulkan-blend-factor
  (ok (= 7
         (cffi:foreign-enum-value
          'lvk::blend-factor :one-minus-src-alpha))))

(deftest sampled-texture-layouts-do-not-require-a-sampler
  (let* ((entries '((:binding 0 :type :texture)
                    (:binding 1 :type :texture)
                    (:binding 2 :type :uniform-buffer)))
         (descriptor
           (luv:make-bind-group-layout-descriptor :entries entries)))
    (ok (equal entries
               (luv::texture-sampler-uniform-layout-entries descriptor)))))

(deftest definitions-retain-abi-metadata-without-call-classes
  (let ((description (lvk:vulkan-function-description 'vk:create-instance)))
    (ok (equal (getf description :foreign-name) "vkCreateInstance"))
    (ok (eq (getf description :return-type) 'lvk::checked-result))
    (ok (equal (mapcar #'first (getf description :arguments))
               '(lvk::create-info lvk::allocator lvk::instance)))
    (ok (not (getf description :command-p)))
    (ok (null (find-class 'vk:create-instance nil)))))

(deftest vulkan-device-contract-includes-shader-int64
  (let ((description
          (lvk:vulkan-function-description
           'vk:get-physical-device-features)))
    (ok (equal (getf description :foreign-name)
               "vkGetPhysicalDeviceFeatures"))
    (ok (= (* 55 (cffi:foreign-type-size :uint32))
           (cffi:foreign-type-size
            '(:struct lvk::physical-device-features))))
    (ok (< (cffi:foreign-slot-offset
            '(:struct lvk::physical-device-features) 'lvk::shader-float64)
           (cffi:foreign-slot-offset
            '(:struct lvk::physical-device-features) 'lvk::shader-int64)))))

(deftest present-timing-abi-is-explicit-and-inspectable
  (let ((description
          (lvk:vulkan-function-description
           'vk:get-past-presentation-timing-ext)))
    (ok (equal (getf description :foreign-name)
               "vkGetPastPresentationTimingEXT"))
    (ok (equal (mapcar #'first (getf description :arguments))
               '(lvk::device lvk::past-presentation-timing-info
                 lvk::past-presentation-timing-properties))))
  (ok (= 16
         (cffi:foreign-type-size '(:struct lvk::present-stage-time))))
  (ok (= 32
         (cffi:foreign-type-size
          '(:struct lvk::swapchain-timing-properties))))
  (ok (= #x240
         (cffi:foreign-bitfield-value
          'lvk::swapchain-create-flags
          '(:present-id-2-khr :present-timing-ext)))))

(cffi:defcallback test-past-presentation-timing-device-procedure :int32
    ((device :pointer)
     (past-presentation-timing-info :pointer)
     (past-presentation-timing-properties :pointer))
  (declare (ignore device past-presentation-timing-info
                   past-presentation-timing-properties))
  0)

(deftest presentation-timing-commands-use-device-procedure-dispatch
  (let ((original (symbol-function 'lvk::device-procedure))
        (lookups nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'lvk::device-procedure)
                 (lambda (device name)
                   (push (list device name) lookups)
                   (cffi:callback test-past-presentation-timing-device-procedure)))
           (let ((device (cffi:make-pointer 42)))
             (ok (eq :success
                     (vk:get-past-presentation-timing-ext
                      device (cffi:null-pointer) (cffi:null-pointer))))
             (ok (equal `((,device "vkGetPastPresentationTimingEXT"))
                        lookups))))
      (setf (symbol-function 'lvk::device-procedure) original))))

(deftest presentation-timeline-correlates-predictions-with-display-results
  (let ((timeline
          (make-instance 'luv::vulkan-presentation-timeline
                         :stage :image-first-pixel-visible)))
    (setf (luv::vulkan-presentation-timeline-status timeline) :recording
          (luv::vulkan-presentation-timeline-time-domain timeline)
          :clock-monotonic
          (luv::vulkan-presentation-timeline-time-domain-id timeline) 7
          (luv::vulkan-presentation-timeline-refresh-duration timeline)
          16666667
          (luv::vulkan-presentation-timeline-refresh-interval timeline)
          16666667)
    (dotimes (index 3)
      (let ((present-id (1+ index)))
        (luv::note-vulkan-presentation-submission
         timeline present-id (+ 10d0 (/ index 60d0)) (+ 9.99d0 (/ index 60d0)))
        (ok (luv::note-vulkan-presentation-result
             timeline present-id (+ 1000000000 (* index 16666667))
             :clock-monotonic 7))))
    (let* ((snapshot (luv::snapshot-vulkan-presentation-timeline timeline))
           (observations
             (coerce (luv:presentation-timing-snapshot-observations snapshot)
                     'list))
           (intervals
             (luv::presentation-timing-interval-milliseconds observations))
           (drift
             (luv::presentation-timing-phase-errors-milliseconds
              observations)))
      (ok (eq :recording
              (luv:presentation-timing-snapshot-status snapshot)))
      (ok (equal '(1 2 3)
                 (mapcar #'luv:presentation-timing-observation-present-id
                         observations)))
      (ok (= 2 (length intervals)))
      (ok (every (lambda (value) (< (abs (- value 16.666667d0)) 1d-9))
                 intervals))
      (ok (< (abs (car (last drift))) 1d-3)))))

(deftest presentation-prediction-marches-over-native-display-beats
  (let ((timeline
          (make-instance 'luv::vulkan-presentation-timeline
                         :stage :image-first-pixel-visible
                         :absolute-time-p t)))
    (setf (luv::vulkan-presentation-timeline-status timeline) :recording
          (luv::vulkan-presentation-timeline-time-domain timeline)
          :present-stage-local-ext
          (luv::vulkan-presentation-timeline-time-domain-id timeline) 7
          (luv::vulkan-presentation-timeline-refresh-duration timeline)
          16666667)
    ;; Present 7 targeted 1.000 s but appeared one refresh later.  Native
    ;; scheduling catches up from the actual display beat, while the unrelated
    ;; host animation prediction remains untouched.
    (luv::note-vulkan-presentation-submission
     timeline 7 10d0 9.99d0 1000000000)
    (ok (luv::note-vulkan-presentation-result
         timeline 7 1016666667 :present-stage-local-ext 7))
    (multiple-value-bind (host target)
        (luv::predict-vulkan-presentation-target timeline 99d0)
      (ok (= host 99d0))
      (ok (= target 1033333334)))
    (multiple-value-bind (host target)
        (luv::predict-vulkan-presentation-target timeline 100d0)
      (ok (= host 100d0))
      (ok (= target 1033333334))
      (luv::note-vulkan-presentation-submission
       timeline 8 host 99.99d0 target))
    ;; Feedback for 7 may still be the newest result when frame 9 is queued.
    ;; The queued target, not a cross-clock lateness estimate, owns cadence.
    (multiple-value-bind (host target)
        (luv::predict-vulkan-presentation-target timeline 101d0)
      (ok (= host 101d0))
      (ok (= target 1050000001)))))

(deftest presentation-timeline-ring-retains-only-its-newest-minute
  (let ((timeline
          (make-instance 'luv::vulkan-presentation-timeline
                         :stage :image-first-pixel-out)))
    (loop for present-id from 1
          to (+ luv::+vulkan-presentation-timing-capacity+ 5)
          do (luv::note-vulkan-presentation-submission
              timeline present-id (float present-id 1d0)
              (float present-id 1d0)))
    (let ((observations
            (luv:presentation-timing-snapshot-observations
             (luv::snapshot-vulkan-presentation-timeline timeline))))
      (ok (= luv::+vulkan-presentation-timing-capacity+
             (length observations)))
      (ok (= 6
             (luv:presentation-timing-observation-present-id
              (aref observations 0))))
      (ok (= (+ luv::+vulkan-presentation-timing-capacity+ 5)
             (luv:presentation-timing-observation-present-id
              (aref observations (1- (length observations)))))))))

(deftest real-loader-calls-allocate-events-only-in-an-explicit-trace
  (ok (null (lvk:current-vulkan-trace)))
  (ok (plusp (length (lvk:enumerate-instance-extension-names))))
  (let (trace)
    (lvk:with-vulkan-trace (active-trace)
      (setf trace active-trace)
      (ok (plusp (length (lvk:enumerate-instance-extension-names)))))
    (ok (null (lvk:current-vulkan-trace)))
    (let ((events (lvk:vulkan-trace-events trace)))
      (ok (= (length events) 2))
      (dolist (event events)
        (ok (equal (lvk:vulkan-call-event-foreign-name event)
                   "vkEnumerateInstanceExtensionProperties"))
        (ok (eq (lvk:vulkan-call-event-status event) :returned))))))
