(in-package #:luv.tests)

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

(deftest slug-formats-retain-the-exact-vulkan-abi-values
  (ok (= 81 (cffi:foreign-enum-value 'lvk::format :r16g16-uint)))
  (ok (= 97
         (cffi:foreign-enum-value
          'lvk::format :r16g16b16a16-sfloat))))

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
