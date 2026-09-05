(in-package #:luft.render.tests)

(define-test fixed-exposure-omits-measurement
  (let ((control (render:make-fixed-exposure 1.25))
        (device (make-instance 'gpu-test-device))
        (encoder (make-instance 'gpu-test-encoder)))
    (true (= 1.25 (render:advance-exposure control)))
    (true (null (render:make-exposure-binding control device :image :sampler)))
    (render:encode-exposure control encoder nil 0)
    (render:release-exposure control)
    (render:release-exposure control)
    (true (null (test-gpu-resources device)))
    (true (null (test-gpu-commands encoder))))
  (dolist (invalid '(0 -1 :automatic))
    (fail (render:make-fixed-exposure invalid))))

(define-test automatic-exposure-rolls-back-every-construction-prefix
  (let* ((device (make-instance 'gpu-test-device))
         (control (render:make-automatic-exposure device))
         (allocations (test-gpu-attempts device)))
    (render:release-exposure control)
    (loop for failure from 1 to allocations do
      (let ((device (make-instance 'gpu-test-device :fail-at failure)))
        (fail (render:make-automatic-exposure device))
        (true (= (1- failure) (length (test-gpu-resources device))))
        (dolist (resource (test-gpu-resources device))
          (true (= 1 (test-gpu-release-attempts resource))))))))

(define-test automatic-exposure-retains-only-failed-release-handles
  (let* ((device (make-instance 'gpu-test-device))
         (control (render:make-automatic-exposure device))
         (failed (first (test-gpu-resources device))))
    (setf (test-gpu-fail-release-p failed) t)
    (fail (render:release-exposure control))
    (render:release-exposure control)
    (render:release-exposure control)
    (true (null (render::owned-gpu-resources control)))
    (dolist (resource (test-gpu-resources device))
      (true (= (if (eq resource failed) 2 1)
               (test-gpu-release-attempts resource))))))

(define-test automatic-exposure-keeps-inflight-measurements-and-consumes-in-order
  (let* ((device (make-instance 'gpu-test-device))
         (control (render:make-automatic-exposure device))
         (encoder (make-instance 'gpu-test-encoder))
         (entries (render::exposure-readbacks control))
         (oldest (render::exposure-readback-buffer (aref entries 0)))
         (newest (render::exposure-readback-buffer (aref entries 2))))
    (unwind-protect
         (progn
           (dotimes (frame 3) (render:encode-exposure control encoder :binding frame))
           (let ((commands (copy-list (test-gpu-commands encoder))))
             (render:encode-exposure control encoder :binding 3)
             (true (equal commands (test-gpu-commands encoder))))
           (setf (test-gpu-ready-p newest) t
                 (test-gpu-bytes newest)
                 (make-array render::+exposure-probe-byte-count+
                             :element-type '(unsigned-byte 8) :initial-element 255))
           (true (= 1.0 (render:advance-exposure control)))
           (setf (test-gpu-ready-p oldest) t
                 (test-gpu-bytes oldest)
                 (make-array render::+exposure-probe-byte-count+
                             :element-type '(unsigned-byte 8) :initial-element 0))
           (true (< (abs (- 1.036 (render:advance-exposure control))) 0.00001))
           (true (null (render::exposure-readback-frame (aref entries 0))))
           (true (= 1 (render::exposure-readback-frame (aref entries 1))))
           (true (= 2 (render::exposure-readback-frame (aref entries 2))))
           (render:encode-exposure control encoder :replacement-binding 3)
           (true (= 3 (render::exposure-readback-frame (aref entries 0)))))
      (render:release-exposure control))))

(define-test renderer-composes-fixed-exposure-through-resize
  (let* ((device (make-instance 'gpu-test-device))
         (factory (lambda (device)
                    (declare (ignore device))
                    (render:make-fixed-exposure 1.25)))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :exposure-factory factory)))
    (unwind-protect
         (progn
           (true (eq factory (render::renderer-exposure-factory renderer)))
           (render::replace-renderer-target-generation renderer '(800 600))
           (true (= 1.25 (render::renderer-exposure renderer)))
           (true (null (render::renderer-exposure-binding renderer)))
           (true (notany
                  (lambda (resource)
                    (search "exposure"
                            (or (luv::gpu-descriptor-label
                                 (test-gpu-descriptor resource)) "")))
                  (test-gpu-resources device))))
      (render:destroy-renderer renderer))))

(define-test renderer-resize-rebinds-exposure-without-replacing-its-queue
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (control (render::renderer-exposure-control renderer))
         (queue (render::exposure-readbacks control))
         (binding (render::renderer-exposure-binding renderer)))
    (unwind-protect
         (progn
           (setf (render::exposure-readback-frame (aref queue 0)) 7)
           (render::replace-renderer-target-generation renderer '(800 600))
           (true (eq control (render::renderer-exposure-control renderer)))
           (true (eq queue (render::exposure-readbacks control)))
           (true (= 7 (render::exposure-readback-frame (aref queue 0))))
           (true (not (eq binding (render::renderer-exposure-binding renderer))))
           (true (= 1 (test-gpu-release-attempts binding))))
      (render:destroy-renderer renderer))))
