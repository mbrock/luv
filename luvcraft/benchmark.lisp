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

(defun luvcraft-benchmark-streaming-settled-p (session)
  (let ((desired
          (hash-table-count (luvcraft-session-desired-chunks session))))
    (and (= desired
            (length (resident-world-chunks (luvcraft-session-world session)))
            (hash-table-count (luvcraft-session-chunk-products session)))
         (zerop
          (production-system-pending-count
           (luvcraft-session-production-system session)))
         (zerop
          (hash-table-count
           (luvcraft-session-outstanding-production session)))
         (zerop
          (hash-table-count
           (luvcraft-session-staged-chunk-products session))))))

(defun luvcraft-benchmark-desired-keys (session)
  (loop for key being the hash-keys
          of (luvcraft-session-desired-chunks session)
        collect key))

(defun begin-luvcraft-benchmark-scenario (scenario session)
  "Begin SCENARIO and return the desired keys immediately before it."
  (let ((before (luvcraft-benchmark-desired-keys session)))
    (case scenario
      (:steady)
      (:streaming
       (let* ((world (luvcraft-session-world session))
              (shape (voxel-space-chunk-shape (block-world-space world)))
              (player (luvcraft-session-player session)))
         (incf (player-x player) (chunk-shape-width shape))
         (sync-camera-to-player (luvcraft-session-camera session) player)))
      (otherwise
       (error "Unknown luvcraft benchmark scenario ~S." scenario)))
    before))

(defun count-luvcraft-benchmark-entering-chunks (before session)
  (count-if
   (lambda (key) (not (member key before :test #'equal)))
   (luvcraft-benchmark-desired-keys session)))

(defun benchmark-luvcraft-frame-performance
    (&key (frame-count 120) (warmup-count 30)
      (width 960) (height 640)
      (world (make-empty-little-block-world :seed 121))
      (camera (make-instance 'fly-camera))
      (scenario :steady) csv-pathname (stream *standard-output*))
  "Measure steady or streaming world frames through the real Metal path.

The streaming scenario moves one chunk in +X after warmup and records the
entire asynchronous load, mesh, and owner-publication transition.  #887PO7

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
  (check-type scenario (member :steady :streaming))
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
                  ;; WIDTH and HEIGHT are benchmark controls, not window points.
                  :high-pixel-density-p nil
                  :provider provider :world world :camera camera
                  :sky-clock
                  (make-instance 'sky-clock :pinned-day-fraction 0.5)))
           (let ((desired
                   (hash-table-count
                    (luvcraft-session-desired-chunks session))))
             (wait-for-luvcraft-products
              session :minimum desired :timeout 30d0)
             (loop repeat warmup-count
                   do (run-luvcraft-benchmark-frame session))
             (let ((queue
                     (device-queue (luvcraft-session-device session))))
               (submitted-work-done queue)
               (let ((before (begin-luvcraft-benchmark-scenario scenario session))
                     (entering 0)
                     (settled-frame nil)
                     (batch-start (get-internal-real-time)))
                 (dotimes (index frame-count)
                   (let ((sample (make-luvcraft-frame-sample)))
                     (run-luvcraft-benchmark-frame session sample)
                     (setf (aref samples index) sample)
                     (when (and (eq scenario :streaming) (zerop index))
                       (setf entering
                             (count-luvcraft-benchmark-entering-chunks
                              before session)))
                     (when (and (eq scenario :streaming)
                                (null settled-frame)
                                (luvcraft-benchmark-streaming-settled-p session))
                       (setf settled-frame index))))
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
                             :scenario scenario
                             :device
                             (benchmark-metal-device-name
                              (luvcraft-session-device session))
                             :width width :height height
                             :warmup-count warmup-count :samples samples
                             :completion-seconds
                             (/ (- finished batch-start) units)
                             :drain-seconds
                             (/ (- finished drain-start) units)
                             :desired-chunk-count desired
                             :entering-chunk-count entering
                             :settled-frame settled-frame)))
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
