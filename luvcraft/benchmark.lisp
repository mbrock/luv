;;; Repeatable demand-driven benchmarks of a fully resident luvcraft world.

(in-package #:luvcraft)

(defun benchmark-metal-device-name (device)
  #-darwin
  (declare (ignore device))
  #+darwin
  (luv.objective-c:objective-c-string
   (luv.metal:device-name (luv::metal-native-object device)))
  #-darwin
  (error "The Metal benchmark is only available on Darwin."))

(defun run-luvcraft-benchmark-frame (session &optional sample)
  (request-canvas-frame
   (luvcraft-session-canvas session)
   (lambda (timestamp)
     (render-luvcraft-frame session timestamp sample))))

(defun benchmark-luvcraft-frame-performance
    (&key (frame-count 120) (warmup-count 30)
      (width 960) (height 640)
      (world (make-empty-little-block-world :seed 121))
      (camera (make-instance 'fly-camera))
      csv-pathname (stream *standard-output*))
  "Measure a fixed, fully resident world through the real Metal frame path.

The hidden demand-clock canvas avoids the application's 60 Hz scheduler, but
CAMetalLayer may still pace drawable availability.  All desired chunk products
arrive before warmup, then FRAME-COUNT consecutive frames reuse the same world,
pipelines, attachments, and drawable pool.  Per-frame values measure CPU
orchestration and encoding.  Completion throughput includes final shared-event
drainage and is intentionally not labelled as GPU time."
  (check-type frame-count (integer 1))
  (check-type warmup-count (integer 0))
  (check-type width (integer 1))
  (check-type height (integer 1))
  (let ((session nil)
        (samples (make-array frame-count))
        (provider (make-instance 'metal-gpu-provider)))
    (unwind-protect
         (progn
           (setf session
                 (start-luvcraft
                  :title "luvcraft Metal frame benchmark"
                  :width width :height height
                  :visible-p nil :frames-per-second nil
                  :provider provider :world world :camera camera
                  :sky-clock
                  (make-instance 'sky-clock :pinned-day-fraction 0.5)))
           (let ((desired
                   (hash-table-count
                    (luvcraft-session-desired-chunks session))))
             (wait-for-luvcraft-products
              session :minimum desired :timeout 30d0)
             (dotimes (index warmup-count)
               (declare (ignore index))
               (run-luvcraft-benchmark-frame session))
             (let ((queue
                     (device-queue (luvcraft-session-device session))))
               (submitted-work-done queue)
               (let ((batch-start (get-internal-real-time)))
                 (dotimes (index frame-count)
                   (let ((sample (make-luvcraft-frame-sample)))
                     (run-luvcraft-benchmark-frame session sample)
                     (setf (aref samples index) sample)))
                 (let ((drain-start (get-internal-real-time)))
                   (submitted-work-done queue)
                   (let* ((finished (get-internal-real-time))
                          (units
                            (coerce internal-time-units-per-second
                                    'double-float))
                          (trace
                            (make-cpu-trace :label "representative frame"))
                          (benchmark
                            (make-luvcraft-frame-benchmark
                             :backend :metal
                             :device
                             (benchmark-metal-device-name
                              (luvcraft-session-device session))
                             :width width :height height
                             :warmup-count warmup-count :samples samples
                             :completion-seconds
                             (/ (- finished batch-start) units)
                             :drain-seconds
                             (/ (- finished drain-start) units)
                             :desired-chunk-count desired)))
                     ;; Warm the reusable zone buffer, then capture one extra
                     ;; frame after the measured batch so detailed zones and
                     ;; their first-use allocation cannot perturb its metrics.
                     (with-cpu-trace (trace)
                       (run-luvcraft-benchmark-frame session))
                     (with-cpu-trace (trace)
                       (run-luvcraft-benchmark-frame session))
                     (print-luvcraft-frame-benchmark benchmark stream)
                     (format stream "~%")
                     (print-cpu-trace trace stream)
                     (when csv-pathname
                       (write-luvcraft-frame-benchmark-csv
                        benchmark csv-pathname)
                       (format stream "  samples: ~A~%"
                               (namestring (truename csv-pathname))))
                     benchmark))))))
      (when session
        (stop-luvcraft session)))))
