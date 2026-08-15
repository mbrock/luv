;;; Small, explicit measurements of the actual luvcraft frame path.

(in-package #:luv)

(defstruct (luvcraft-frame-sample
             (:constructor make-luvcraft-frame-sample ()))
  (frame-seconds 0d0 :type double-float)
  (simulation-seconds 0d0 :type double-float)
  (streaming-seconds 0d0 :type double-float)
  (presentation-seconds 0d0 :type double-float)
  (shader-refresh-seconds 0d0 :type double-float)
  (mesh-publication-seconds 0d0 :type double-float)
  (uniform-seconds 0d0 :type double-float)
  (shadow-encode-seconds 0d0 :type double-float)
  (scene-encode-seconds 0d0 :type double-float)
  (surface-copy-encode-seconds 0d0 :type double-float)
  (chunk-count 0 :type fixnum)
  (draw-count 0 :type fixnum)
  (vertex-count 0 :type fixnum))

(defmacro with-luvcraft-frame-timing ((sample accessor) &body body)
  "Accumulate BODY's real time into ACCESSOR when SAMPLE is non-NIL."
  (let ((record (gensym "SAMPLE"))
        (started (gensym "STARTED")))
    `(let ((,record ,sample))
       (if ,record
           (let ((,started (get-internal-real-time)))
             (unwind-protect
                  (progn ,@body)
               (incf (,accessor ,record)
                     (/ (- (get-internal-real-time) ,started)
                        (coerce internal-time-units-per-second
                                'double-float)))))
           (progn ,@body)))))

(defstruct luvcraft-frame-benchmark
  (backend :metal :type keyword)
  (device "" :type string)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (warmup-count 0 :type fixnum)
  (samples #() :type vector)
  (completion-seconds 0d0 :type double-float)
  (drain-seconds 0d0 :type double-float)
  (desired-chunk-count 0 :type fixnum))

(defun luvcraft-frame-metric-values (benchmark reader)
  (map 'vector
       (lambda (sample)
         (* 1000d0 (funcall reader sample)))
       (luvcraft-frame-benchmark-samples benchmark)))

(defun luvcraft-frame-metric-summary (benchmark reader)
  "Return the median, p95, mean, and maximum milliseconds for READER."
  (let* ((values (luvcraft-frame-metric-values benchmark reader))
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
    (format stream "  frame: ~Dx~D, ~D warmup + ~D measured~%"
            (luvcraft-frame-benchmark-width benchmark)
            (luvcraft-frame-benchmark-height benchmark)
            (luvcraft-frame-benchmark-warmup-count benchmark)
            count)
    (when first
      (format stream "  world: ~D/~D chunks, ~D draws, ~:D vertices per frame~%"
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
    (format stream "frame,frame_cpu_ms,simulation_ms,streaming_ms,present_ms,shader_refresh_ms,mesh_publication_ms,uniform_ms,shadow_encode_ms,scene_encode_ms,surface_copy_encode_ms,chunks,draws,vertices~%")
    (loop for sample across (luvcraft-frame-benchmark-samples benchmark)
          for index from 0
          do (format stream
                     "~D,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~,6F,~D,~D,~D~%"
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
                     (luvcraft-frame-sample-chunk-count sample)
                     (luvcraft-frame-sample-draw-count sample)
                     (luvcraft-frame-sample-vertex-count sample))))
  pathname)
