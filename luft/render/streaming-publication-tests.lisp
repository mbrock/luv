(in-package #:luft.render.tests)

(defun call-with-streaming-test-result (function)
  "Compile a real one-cell region on the production worker; leave it unaccepted."
  (let ((builder (render::make-scene-builder)))
    (render::scene-builder-cell builder 10 10 2)
    (let* ((scene (render::make-streaming-scene
                   (render::finish-scene-builder builder)))
           (system (production:make-single-worker-production-system
                    :name "LUFT publication test")))
      (unwind-protect
           (progn
             (setf (gethash 0 (render::streaming-scene-loaded scene)) 1)
             (let* ((request (render::schedule-streaming-scene-cohort
                              scene system '(0) 1 0))
                    (result
                      (loop repeat 1000
                            for result = (production:receive-production-result-no-hang system)
                            when result return result
                            do (sleep 0.01)
                            finally (error "Streaming test worker did not finish."))))
               (when (production:production-result-condition result)
                 (error (production:production-result-condition result)))
               (funcall function scene request (production:production-result-value result))))
        (production:stop-production-system system)))))

(define-test streaming-accepts-only-complete-current-pending-results
  (call-with-streaming-test-result
   (lambda (scene request result)
     (let ((replacement (render::streaming-scene-replacement scene)))
       (true (= 1 (render::streaming-scene-pending-mesh-count scene)))
       (true (not (nth-value 2 (render::ready-streaming-scene-meshes scene))))
       (fail (render::accept-streaming-mesh-result
              scene request
              (render::%make-streaming-mesh-result
               nil (render::streaming-mesh-result-generation result))))
       (true (null (render::streaming-replacement-result replacement)))
       (incf (render::scene-content-revision scene))
       (true (not (render::accept-streaming-mesh-result scene request result)))
       (decf (render::scene-content-revision scene))
       (true (render::accept-streaming-mesh-result scene request result))
       (true (not (render::accept-streaming-mesh-result scene request result)))
       (true (= 0 (render::streaming-scene-pending-mesh-count scene)))
       (true (nth-value 2 (render::ready-streaming-scene-meshes scene)))))))

(define-test streaming-reset-detaches-old-requests-even-with-the-same-input-stamp
  (call-with-streaming-test-result
   (lambda (scene request result)
     (render::reset-streaming-scene-publication scene)
     (setf (gethash 0 (render::streaming-scene-loaded scene)) 1)
     (let ((new-request
             (make-instance 'render::streaming-mesh-request
                            :key :test
                            :snapshot (render::make-streaming-region-snapshot scene '(0) 1))))
       (setf (render::streaming-scene-replacement scene)
             (render::make-streaming-replacement '(0) nil new-request))
       (true (render::current-streaming-mesh-request-p scene request))
       (true (not (render::accept-streaming-mesh-result scene request result)))
       (true (null (render::streaming-replacement-result
                    (render::streaming-scene-replacement scene))))))))

(define-test streaming-publication-retries-gpu-failure-and-removes-without-a-worker
  (call-with-streaming-test-result
   (lambda (scene request result)
     (let* ((device (make-instance 'gpu-test-device))
            (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
            (replacement (render::streaming-scene-replacement scene))
            (old-light (render::streaming-scene-light-generation scene)))
       (unwind-protect
            (progn
              (true (render::accept-streaming-mesh-result scene request result))
              (true (null (render::streaming-scene-mesh-cache scene)))
              (setf (slot-value device 'fail-at) (1+ (test-gpu-attempts device)))
              (fail (render::publish-ready-streaming-scene scene renderer))
              (true (eq replacement (render::streaming-scene-replacement scene)))
              (true (null (render::renderer-slot-order renderer)))
              (true (eq old-light (render::streaming-scene-light-generation scene)))
              (true (null (render::streaming-scene-mesh-cache scene)))
              (setf (slot-value device 'fail-at) nil)
              (true (= 1 (render::publish-ready-streaming-scene scene renderer)))
              (true (equal '(0) (render::renderer-slot-order renderer)))
              (true (null (render::streaming-scene-replacement scene)))
              (true (eq (render::streaming-mesh-result-mesh-cache result)
                        (render::streaming-scene-mesh-cache scene)))
              (setf (render::streaming-scene-replacement scene)
                    (render::make-streaming-removal
                     '(0)
                     (render::make-scene-mesh-generation-value
                      scene :test-removal (render::streaming-scene-light-generation scene))))
              (true (nth-value 2 (render::ready-streaming-scene-meshes scene)))
              (true (= 0 (render::publish-ready-streaming-scene scene renderer)))
              (true (null (render::renderer-slot-order renderer)))
              (true (null (render::streaming-scene-replacement scene))))
         (render:destroy-renderer renderer))))))

(define-test streaming-retains-worker-failure-as-a-failed-replacement
  (call-with-streaming-test-result
   (lambda (scene request result)
     (declare (ignore result))
     (let* ((condition (make-condition 'simple-error :format-control "worker failed"))
            (failed (make-instance 'production:production-result
                                   :request request :condition condition
                                   :elapsed-seconds 0d0)))
       (fail (render::receive-streaming-mesh-result scene failed))
       (true (eq condition
                 (render::streaming-replacement-failure
                  (render::streaming-scene-replacement scene))))
       (true (zerop (render::streaming-scene-pending-mesh-count scene)))
       (true (not (render::receive-streaming-mesh-result scene failed)))
       (true (= 1 (length (render::streaming-scene-production-errors scene))))
       (render::reset-streaming-scene-publication scene)
       (true (null (render::streaming-scene-replacement scene)))))))
