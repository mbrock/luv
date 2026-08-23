;;; Small, explicit measurements of the actual luvcraft frame path.

(in-package #:luvcraft)

(luv.arithmetic.records:define-quantity-struct
    (luvcraft-frame-sample (:constructor make-luvcraft-frame-sample ()))
  (frame-seconds 0d0 :type double-float
                 :quantity (:quantity :frame-cpu-duration :unit :second))
  (simulation-seconds 0d0 :type double-float
                      :quantity (:quantity :simulation-duration :unit :second))
  (streaming-seconds 0d0 :type double-float
                     :quantity (:quantity :streaming-duration :unit :second))
  (presentation-seconds 0d0 :type double-float
                        :quantity (:quantity :presentation-duration :unit :second))
  (shader-refresh-seconds 0d0 :type double-float
                          :quantity (:quantity :shader-refresh-duration :unit :second))
  (mesh-publication-seconds 0d0 :type double-float
                            :quantity (:quantity :mesh-publication-duration :unit :second))
  (uniform-seconds 0d0 :type double-float
                   :quantity (:quantity :uniform-update-duration :unit :second))
  (shadow-encode-seconds 0d0 :type double-float
                         :quantity (:quantity :shadow-encode-duration :unit :second))
  (scene-encode-seconds 0d0 :type double-float
                        :quantity (:quantity :scene-encode-duration :unit :second))
  (surface-copy-encode-seconds 0d0 :type double-float
                               :quantity (:quantity :surface-copy-encode-duration
                                          :unit :second))
  (resident-chunk-count 0 :type fixnum)
  (pending-production-count 0 :type fixnum)
  (staged-chunk-count 0 :type fixnum)
  (chunk-count 0 :type fixnum)
  (draw-count 0 :type fixnum)
  (vertex-count 0 :type fixnum))

(defmacro with-luvcraft-frame-timing ((sample accessor zone) &body body)
  "Accumulate BODY into SAMPLE and expose it as nested CPU trace ZONE."
  (let ((record (gensym "SAMPLE"))
        (started (gensym "STARTED")))
    `(with-cpu-trace-zone (,zone)
       (let* ((,record ,sample)
              (,started (and ,record (get-internal-real-time))))
         ;; One body serves sampled and unsampled frames.  Besides making the
         ;; semantic guarantee obvious, this keeps a large render pass from
         ;; appearing twice in the compiler IR at every timing boundary.
         (unwind-protect
              (progn ,@body)
           (when ,record
             (incf (,accessor ,record)
                   (/ (- (get-internal-real-time) ,started)
                      (coerce internal-time-units-per-second
                              'double-float)))))))))

;;; Tracy watches the same frame path live, where the sample struct above
;;; watches a fixed batch of frames and prints the result.  The two are meant
;;; to answer different questions: the benchmark says whether a change moved
;;; the numbers, and Tracy says where a frame that felt wrong actually went.

(defparameter *luvcraft-tracy-plots*
  '("resident chunks" "pending production" "staged chunks"
    "drawable chunks" "draws" "vertices"
    "player chunk x" "player chunk z"
    "frame CPU ms" "60 Hz budget ms")
  "The per-frame counts luvcraft draws alongside its Tracy zones.")

(defvar *luvcraft-tracy-plots-described-p* nil
  "Whether the connected viewer has been told how to draw luvcraft's plots.")

(defun start-luvcraft-tracy ()
  "Start Tracy for luvcraft and answer whether the profiler is running.

Nothing is recorded until a viewer connects, so this is safe to leave on.
Call it before starting a session if you want the production workers to appear
under their own names: a thread can only introduce itself to a profiler that
is already running, and luvcraft's workers introduce themselves as they start."
  (start-tracy :application-name "luvcraft")
  (setf *luvcraft-tracy-plots-described-p* nil)
  *tracy*)

(defvar *luvcraft-tracy-capture-controller* nil
  "The shared subprocess owner for this Lisp application's captures.")

(defvar *luvcraft-tracy-capture-controller-lock*
  (sb-thread:make-mutex :name "Luvcraft Tracy capture controller"))

(defun make-luvcraft-tracy-capture-controller ()
  (luv.tracy.capture:make-tracy-capture-controller
   :application-name "luvcraft"
   :directory
   (merge-pathnames "build/tracy/"
                    (asdf:system-source-directory "luvcraft"))))

(defun ensure-luvcraft-tracy-capture-controller ()
  (sb-thread:with-mutex (*luvcraft-tracy-capture-controller-lock*)
    (when (or (null *luvcraft-tracy-capture-controller*)
              (luv.tracy.capture:tracy-capture-controller-released-p
               *luvcraft-tracy-capture-controller*))
      (setf *luvcraft-tracy-capture-controller*
            (make-luvcraft-tracy-capture-controller)))
    *luvcraft-tracy-capture-controller*))

(defun luvcraft-tracy-capture-active-p ()
  "Whether Luvcraft's capture is starting, recording, or freezing its trace."
  (let ((controller *luvcraft-tracy-capture-controller*))
    (and controller
         (luv.tracy.capture:tracy-capture-active-p controller))))

(defun start-luvcraft-tracy-capture ()
  "Publish an asynchronous capture start and return its reserved pathname."
  (luv.tracy.capture:start-tracy-capture
   (ensure-luvcraft-tracy-capture-controller)))

(defun stop-luvcraft-tracy-capture ()
  "Publish one graceful stop without waiting for trace finalization."
  (let ((controller *luvcraft-tracy-capture-controller*))
    (when controller
      (luv.tracy.capture:stop-tracy-capture controller))))

(defun toggle-luvcraft-tracy-capture ()
  "Atomically start or stop Luvcraft's one Tracy capture generation."
  (luv.tracy.capture:toggle-tracy-capture
   (ensure-luvcraft-tracy-capture-controller)))

(defun open-luvcraft-tracy-capture (&optional pathname)
  "Open PATHNAME or Luvcraft's last frozen trace without blocking the caller."
  (let ((controller *luvcraft-tracy-capture-controller*))
    (when controller
      (luv.tracy.capture:open-tracy-capture controller pathname))))

(defun reveal-luvcraft-tracy-capture (&optional pathname)
  "Reveal PATHNAME or Luvcraft's last frozen trace without blocking the caller."
  (let ((controller *luvcraft-tracy-capture-controller*))
    (when controller
      (luv.tracy.capture:reveal-tracy-capture controller pathname))))

(defun release-luvcraft-tracy-capture-controller ()
  "Terminally detach Luvcraft's current controller without waiting on it.

Detachment is atomic and precedes release, so a later game session obtains a
fresh controller even while the prior capture finishes on its owner thread."
  (let ((controller nil))
    (sb-thread:with-mutex (*luvcraft-tracy-capture-controller-lock*)
      (setf controller *luvcraft-tracy-capture-controller*
            *luvcraft-tracy-capture-controller* nil))
    (when controller
      (luv.tracy.capture:release-tracy-capture-controller controller)
      t)))

(defun describe-luvcraft-tracy-plots ()
  "Describe luvcraft's plots to a viewer, once per connection.

Plot configuration reaches only a viewer that is already listening, and a
capture may begin at any frame, so the description is re-sent on each fresh
connection rather than once at startup."
  (let ((connected (tracy-connected-p)))
    (cond ((and connected (not *luvcraft-tracy-plots-described-p*))
           ;; A capture launched from F9 initializes Tracy on its control
           ;; worker.  Name the actual rendering lane when it first observes
           ;; that viewer rather than leaving the control worker called main.
           (name-tracy-thread "canvas")
           (dolist (plot *luvcraft-tracy-plots*)
             (configure-tracy-plot plot :format :number :step t))
           (setf *luvcraft-tracy-plots-described-p* t))
          ((not connected)
           (setf *luvcraft-tracy-plots-described-p* nil)))))

(luv.arithmetic.records:define-quantity-struct luvcraft-frame-benchmark
  (backend :metal :type keyword)
  (scenario :steady :type keyword)
  (device "" :type string)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (warmup-count 0 :type fixnum)
  (samples #() :type vector)
  (completion-seconds 0d0 :type double-float
                      :quantity (:quantity :benchmark-completion-duration
                                 :unit :second))
  (drain-seconds 0d0 :type double-float
                 :quantity (:quantity :benchmark-drain-duration :unit :second))
  (desired-chunk-count 0 :type fixnum)
  (entering-chunk-count 0 :type fixnum)
  (settled-frame nil :type (or null fixnum)))

(defun luvcraft-frame-samples-metric-summary (samples reader)
  "Return median, p95, mean, and maximum milliseconds over SAMPLES."
  (let* ((values
           (map 'vector
                (lambda (sample) (* 1000d0 (funcall reader sample)))
                samples))
         (count (length values)))
    (when (zerop count)
      (error "Cannot summarize an empty luvcraft frame benchmark."))
    (let* ((sorted (sort (copy-seq values) #'<))
           (median-index (floor count 2))
           (p95-rank (ceiling (* 0.95d0 count)))
           (p95-index (min (1- count) (max 0 (1- p95-rank))))
           (median
             (if (oddp count)
                 (aref sorted median-index)
                 (/ (+ (aref sorted (1- median-index))
                       (aref sorted median-index))
                    2d0))))
      (values median
              (aref sorted p95-index)
              (/ (reduce #'+ values) count)
              (aref sorted (1- count))))))

(defun luvcraft-frame-metric-summary (benchmark reader)
  "Return the median, p95, mean, and maximum milliseconds for READER."
  (luvcraft-frame-samples-metric-summary
   (luvcraft-frame-benchmark-samples benchmark) reader))

(defun luvcraft-frame-benchmark-transition-samples (benchmark)
  "Return measured samples up to streaming settlement, or all when unsettled."
  (let* ((samples (luvcraft-frame-benchmark-samples benchmark))
         (settled (luvcraft-frame-benchmark-settled-frame benchmark)))
    (subseq samples 0 (if settled (min (length samples) (1+ settled))
                           (length samples)))))

(defparameter *luvcraft-frame-metrics*
  `(("frame CPU" . ,#'luvcraft-frame-sample-frame-seconds)
    ("simulation" . ,#'luvcraft-frame-sample-simulation-seconds)
    ("streaming" . ,#'luvcraft-frame-sample-streaming-seconds)
    ("present (inclusive)" .
     ,#'luvcraft-frame-sample-presentation-seconds)
    ("shader refresh" .
     ,#'luvcraft-frame-sample-shader-refresh-seconds)
    ("mesh publication" .
     ,#'luvcraft-frame-sample-mesh-publication-seconds)
    ("uniform update" . ,#'luvcraft-frame-sample-uniform-seconds)
    ("shadow encode" . ,#'luvcraft-frame-sample-shadow-encode-seconds)
    ("scene encode" . ,#'luvcraft-frame-sample-scene-encode-seconds)
    ("surface copy encode" .
     ,#'luvcraft-frame-sample-surface-copy-encode-seconds)))

(defun print-luvcraft-frame-benchmark
    (benchmark &optional (stream *standard-output*))
  "Print a bounded human-readable summary of BENCHMARK."
  (let* ((samples (luvcraft-frame-benchmark-samples benchmark))
         (count (length samples))
         (first (and (plusp count) (aref samples 0)))
         (completion (luvcraft-frame-benchmark-completion-seconds benchmark)))
    (format stream "luvcraft ~:(~A~) frame benchmark~%"
            (luvcraft-frame-benchmark-backend benchmark))
    (format stream "  device: ~A~%" (luvcraft-frame-benchmark-device benchmark))
    (format stream "  scenario: ~(~A~)~%"
            (luvcraft-frame-benchmark-scenario benchmark))
    (format stream "  frame: ~Dx~D, ~D warmup + ~D measured~%"
            (luvcraft-frame-benchmark-width benchmark)
            (luvcraft-frame-benchmark-height benchmark)
            (luvcraft-frame-benchmark-warmup-count benchmark)
            count)
    (when first
      (format stream "  first measured: ~D/~D chunks, ~D draws, ~:D vertices~%"
              (luvcraft-frame-sample-chunk-count first)
              (luvcraft-frame-benchmark-desired-chunk-count benchmark)
              (luvcraft-frame-sample-draw-count first)
              (luvcraft-frame-sample-vertex-count first)))
    (format stream "~%  CPU metric                 median      p95     mean      max~%")
    (dolist (metric *luvcraft-frame-metrics*)
      (multiple-value-bind (median p95 mean maximum)
          (luvcraft-frame-metric-summary benchmark (cdr metric))
        (format stream "  ~24A ~7,3F  ~7,3F  ~7,3F  ~7,3F ms~%"
                (car metric) median p95 mean maximum)))
    (when (eq :streaming (luvcraft-frame-benchmark-scenario benchmark))
      (let* ((transition
               (luvcraft-frame-benchmark-transition-samples benchmark))
             (transition-count (length transition))
             (settled (luvcraft-frame-benchmark-settled-frame benchmark)))
        (multiple-value-bind (median p95 mean maximum)
            (luvcraft-frame-samples-metric-summary
             transition #'luvcraft-frame-sample-frame-seconds)
          (format stream "~%  streaming transition: ~D entering chunks, ~D frames~%"
                  (luvcraft-frame-benchmark-entering-chunk-count benchmark)
                  transition-count)
          (format stream "    settled: ~:[not within measured batch~;frame ~:*~D~]~%"
                  settled)
          (format stream "    frame CPU: median ~,3F, p95 ~,3F, max ~,3F ms (~,1F frames/s mean)~%"
                  median p95 maximum (/ 1000d0 mean))
          (format stream "    60 Hz deadline misses: ~D/~D~%"
                  (count-if
                   (lambda (sample)
                     (> (luvcraft-frame-sample-frame-seconds sample)
                        (/ 1d0 60d0)))
                   transition)
                  transition-count))))
    (format stream "~%  completion-limited: ~,3F ms/frame (~,1F frames/s)~%"
            (* 1000d0 (/ completion count))
            (/ count completion))
    (format stream "  final queue drain: ~,3F ms~%"
            (* 1000d0
               (luvcraft-frame-benchmark-drain-seconds benchmark)))
    (format stream "  note: completion-limited time is submit-to-shared-event wall time, ~
not a GPU timestamp.~%")
    benchmark))

(defun write-luvcraft-frame-benchmark-csv (benchmark pathname)
  "Write every BENCHMARK sample as stable, comparison-friendly CSV."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (format stream "frame,frame_cpu_ms,simulation_ms,streaming_ms,present_ms,shader_refresh_ms,mesh_publication_ms,uniform_ms,shadow_encode_ms,scene_encode_ms,surface_copy_encode_ms,resident_chunks,pending_production,staged_chunks,chunks,draws,vertices~%")
    (loop for sample across (luvcraft-frame-benchmark-samples benchmark)
          for index from 0
          do (format stream
                     "~D,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~D,~D,~D,~D,~D,~D~%"
                     index
                     (* 1000d0 (luvcraft-frame-sample-frame-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-simulation-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-streaming-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-presentation-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-shader-refresh-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-mesh-publication-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-uniform-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-shadow-encode-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-scene-encode-seconds sample))
                     (* 1000d0
                        (luvcraft-frame-sample-surface-copy-encode-seconds
                         sample))
                     (luvcraft-frame-sample-resident-chunk-count sample)
                     (luvcraft-frame-sample-pending-production-count sample)
                     (luvcraft-frame-sample-staged-chunk-count sample)
                     (luvcraft-frame-sample-chunk-count sample)
                     (luvcraft-frame-sample-draw-count sample)
                     (luvcraft-frame-sample-vertex-count sample))))
  pathname)
