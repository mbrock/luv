;;;; Repeatable CPU benchmarks for LUFT's immutable materialization pipeline.

(defpackage #:luft.benchmark
  (:use #:cl)
  (:export #:run-mesher-benchmark))

(in-package #:luft.benchmark)

(defparameter +benchmark-phases+
  '(:normalize :surface :records-chain :records-dense :full-chain))

(defstruct (mesher-case
             (:constructor %make-mesher-case
                 (size pattern domain sites solid surface
                  chain-occupancy dense-occupancy)))
  (size 0 :type fixnum :read-only t)
  (pattern :solid :type keyword :read-only t)
  (domain nil :type luft:world-domain :read-only t)
  (sites #() :type (simple-array (unsigned-byte 64) (*)) :read-only t)
  (solid nil :type luft:chain :read-only t)
  (surface nil :type luft:chain :read-only t)
  (chain-occupancy nil :type function :read-only t)
  (dense-occupancy nil :type function :read-only t))

(defstruct mesher-sample
  (size 0 :type fixnum)
  (pattern :solid :type keyword)
  (phase :full-chain :type keyword)
  (index 0 :type fixnum)
  (iterations 1 :type fixnum)
  (cell-count 0 :type fixnum)
  (face-count 0 :type fixnum)
  (elapsed-seconds 0d0 :type double-float)
  (bytes-consed 0 :type integer)
  (gc-seconds 0d0 :type double-float)
  (garbage-collections 0 :type fixnum))

(defvar *mesher-benchmark-sink* nil)

(defun power-of-two-p (value)
  (and (plusp value) (zerop (logand value (1- value)))))

(defun terrain-height (x y size)
  (max 1
       (min size
            (round (+ (* 0.46 size)
                      (* 0.17 size (sin (/ (+ x (* 0.35 y)) 3.7)))
                      (* 0.11 size (cos (/ (- y (* 0.22 x)) 5.1))))))))

(defun architecture-cell-p (x y z size)
  (let* ((quarter (max 1 (floor size 4)))
         (three-quarters (min (1- size) (* 3 quarter)))
         (pillar-step (max 3 quarter))
         (wall-height (max 2 (floor size 2)))
         (pillar-height (max 3 (floor (* 3 size) 4))))
    (or (zerop z)
        (and (< z wall-height)
             (or (and (or (= x quarter) (= x three-quarters))
                      (<= quarter y three-quarters))
                 (and (or (= y quarter) (= y three-quarters))
                      (<= quarter x three-quarters))))
        (and (< z pillar-height)
             (zerop (mod x pillar-step))
             (zerop (mod y pillar-step)))
        (and (= z wall-height)
             (<= quarter x three-quarters)
             (<= quarter y three-quarters)
             (or (= x quarter) (= x three-quarters)
                 (= y quarter) (= y three-quarters))))))

(defun benchmark-cell-p (pattern x y z size)
  (ecase pattern
    (:solid t)
    (:terrain (< z (terrain-height x y size)))
    (:architecture (architecture-cell-p x y z size))
    (:checkerboard (evenp (+ x y z)))))

(defun make-case-sites (domain size pattern origin-x origin-y origin-z)
  (let ((buffer
          (make-array (max 16 (floor (* size size size) 2))
                      :element-type '(unsigned-byte 64)
                      :adjustable t :fill-pointer 0)))
    (dotimes (z size)
      (dotimes (y size)
        (dotimes (x size)
          (when (benchmark-cell-p pattern x y z size)
            (vector-push-extend
             (luft:make-site domain (+ origin-x x) (+ origin-y y)
                             (+ origin-z z) luft:+cell-extent+ 1)
             buffer)))))
    (let ((sites (make-array (length buffer)
                             :element-type '(unsigned-byte 64))))
      (replace sites buffer)
      sites)))

(defun make-solid-chain (domain sites)
  (let ((builder (luft:make-chain-builder
                  domain :initial-capacity (length sites))))
    (loop for site across sites
          do (luft:chain-builder-add-site builder site))
    (luft:finish-chain-builder builder)))

(defun make-dense-occupancy
    (sites origin-x origin-y origin-z size)
  (let* ((window-size (+ size 2))
         (window-x (1- origin-x))
         (window-y (1- origin-y))
         (window-z (1- origin-z))
         (plane (* window-size window-size))
         (bits (make-array (* plane window-size) :element-type 'bit
                           :initial-element 0)))
    (labels ((index (x y z)
               (+ (- x window-x)
                  (* window-size (- y window-y))
                  (* plane (- z window-z)))))
      (loop for site across sites
            do (setf (sbit bits (index (luft:site-x site)
                                       (luft:site-y site)
                                       (luft:site-z site)))
                     1))
      (lambda (x y z)
        (let ((local-x (- x window-x))
              (local-y (- y window-y))
              (local-z (- z window-z)))
          (if (and (<= 0 local-x) (< local-x window-size)
                   (<= 0 local-y) (< local-y window-size)
                   (<= 0 local-z) (< local-z window-size))
              (sbit bits (+ local-x (* window-size local-y)
                            (* plane local-z)))
              0))))))

(defun make-mesher-case (size pattern)
  (unless (and (typep size 'fixnum) (>= size 4) (power-of-two-p size))
    (error "LUFT benchmark sizes must be power-of-two fixnums >= 4, not ~S."
           size))
  (unless (member pattern '(:solid :terrain :architecture :checkerboard))
    (error "Unknown LUFT benchmark pattern ~S." pattern))
  (let* ((period (* 2 size))
         (bits (1- (integer-length period)))
         (domain (luft:make-world-domain :x-bits bits :y-bits bits))
         (origin-x (floor size 2))
         (origin-y (floor size 2))
         (origin-z 32)
         (sites (make-case-sites domain size pattern
                                 origin-x origin-y origin-z))
         (solid (make-solid-chain domain sites))
         (surface (luft:surface-chain solid))
         (chain-occupancy
           (lambda (x y z)
             (luft:chain-cell-occupancy-bit solid x y z)))
         (dense-occupancy
           (make-dense-occupancy sites origin-x origin-y origin-z size)))
    (%make-mesher-case size pattern domain sites solid surface
                       chain-occupancy dense-occupancy)))

(defun phase-zone (phase)
  (ecase phase
    (:normalize :luft/benchmark/normalize)
    (:surface :luft/benchmark/surface)
    (:records-chain :luft/benchmark/records-chain)
    (:records-dense :luft/benchmark/records-dense)
    (:full-chain :luft/benchmark/full-chain)))

(defun phase-work-count (case phase)
  (ecase phase
    ((:normalize :surface) (length (mesher-case-sites case)))
    ((:records-chain :records-dense :full-chain)
     (luft:chain-count (mesher-case-surface case)))))

(defun invoke-benchmark-phase (case phase)
  (let ((work-count (phase-work-count case phase)))
    (luv:with-cpu-trace-zone
        ((phase-zone phase) :tracy-value work-count)
      (ecase phase
        (:normalize
         (make-solid-chain (mesher-case-domain case)
                           (mesher-case-sites case)))
        (:surface
         (luft:surface-chain (mesher-case-solid case)))
        (:records-chain
         (luft.render:make-face-materialization-from-surface
          (mesher-case-surface case) (mesher-case-chain-occupancy case)))
        (:records-dense
         (luft.render:make-face-materialization-from-surface
          (mesher-case-surface case) (mesher-case-dense-occupancy case)))
        (:full-chain
         (luft.render:make-face-materialization
          (mesher-case-solid case)))))))

(defun materialization-words= (a b)
  (equalp (luft.render:face-materialization-words a)
          (luft.render:face-materialization-words b)))

(defun validate-case-occupancy (case)
  (let ((chain
          (invoke-benchmark-phase case :records-chain))
        (dense
          (invoke-benchmark-phase case :records-dense)))
    (unless (materialization-words= chain dense)
      (error "Chain and dense occupancy disagree for ~D^3 ~(~A~)."
             (mesher-case-size case) (mesher-case-pattern case))))
  case)

(defun measure-benchmark-phase
    (case phase sample-count warmup-count &optional (stream *standard-output*))
  (format stream "  ~20A " phase)
  (force-output stream)
  (dotimes (index warmup-count)
    (declare (ignorable index))
    (write-char #\w stream)
    (force-output stream)
    (setf *mesher-benchmark-sink* (invoke-benchmark-phase case phase)))
  (let ((iterations
          (loop with iterations = 1
                with target-seconds = 0.025d0
                do (let ((observation (luv:make-runtime-observation)))
                     (luv:with-runtime-observation (observation)
                       (dotimes (index iterations)
                         (declare (ignorable index))
                         (setf *mesher-benchmark-sink*
                               (invoke-benchmark-phase case phase))))
                     (when (or (>= (luv:runtime-observation-elapsed-seconds
                                    observation)
                                   target-seconds)
                               (>= iterations 1024))
                       (return iterations)))
                   (setf iterations (min 1024 (* 2 iterations))))))
    (format stream "x~D " iterations)
    (force-output stream)
    (sb-ext:gc :full t)
    (let ((samples (make-array sample-count)))
      (dotimes (index sample-count)
        (write-char #\. stream)
        (force-output stream)
        (let ((observation (luv:make-runtime-observation)))
          (luv:with-runtime-observation (observation)
            (dotimes (iteration iterations)
              (declare (ignorable iteration))
              (setf *mesher-benchmark-sink*
                    (invoke-benchmark-phase case phase))))
          (setf (aref samples index)
                (make-mesher-sample
                 :size (mesher-case-size case)
                 :pattern (mesher-case-pattern case)
                 :phase phase :index index
                 :iterations iterations
                 :cell-count (length (mesher-case-sites case))
                 :face-count (luft:chain-count (mesher-case-surface case))
                 :elapsed-seconds
                 (/ (luv:runtime-observation-elapsed-seconds observation)
                    iterations)
                 :bytes-consed
                 (round (/ (luv:runtime-observation-bytes-consed observation)
                           iterations))
                 :gc-seconds (/ (luv:runtime-observation-gc-seconds observation)
                                iterations)
                 :garbage-collections
                 (luv:runtime-observation-garbage-collections observation)))))
      (terpri stream)
      samples)))

(defun metric-summary (samples reader)
  (let* ((values (map 'vector reader samples))
         (count (length values))
         (sorted (sort (copy-seq values) #'<))
         (middle (floor count 2))
         (p95-index (min (1- count)
                         (max 0 (1- (ceiling (* 0.95d0 count))))))
         (median (if (oddp count)
                     (aref sorted middle)
                     (/ (+ (aref sorted (1- middle))
                           (aref sorted middle))
                        2d0))))
    (values median
            (aref sorted p95-index)
            (/ (reduce #'+ values) count)
            (aref sorted (1- count)))))

(defun print-phase-summary (case phase samples stream)
  (multiple-value-bind (median p95 mean maximum)
      (metric-summary samples #'mesher-sample-elapsed-seconds)
    (declare (ignore mean))
    (multiple-value-bind (median-bytes p95-bytes mean-bytes maximum-bytes)
        (metric-summary samples #'mesher-sample-bytes-consed)
      (declare (ignore p95-bytes mean-bytes maximum-bytes))
      (let ((work (phase-work-count case phase)))
        (format stream
                "    ~20A ~8,3F ms p50  ~8,3F ms p95  ~8,3F ms max  ~
~8,2F MiB  ~8,2F Mwork/s~%"
                phase (* 1000d0 median) (* 1000d0 p95)
                (* 1000d0 maximum) (/ median-bytes 1048576d0)
                (if (plusp median) (/ work median 1000000d0) 0d0))))))

(defun write-csv-header (stream)
  (format stream
          "size,pattern,phase,sample,iterations,cells,faces,record_bytes,elapsed_ms,allocated_bytes,gc_ms,batch_gc_count~%"))

(defun write-sample-csv (sample stream)
  (format stream "~D,~(~A~),~(~A~),~D,~D,~D,~D,~D,~,6F,~D,~,6F,~D~%"
          (mesher-sample-size sample)
          (mesher-sample-pattern sample)
          (mesher-sample-phase sample)
          (mesher-sample-index sample)
          (mesher-sample-iterations sample)
          (mesher-sample-cell-count sample)
          (mesher-sample-face-count sample)
          (* luft:+face-record-byte-size+ (mesher-sample-face-count sample))
          (* 1000d0 (mesher-sample-elapsed-seconds sample))
          (mesher-sample-bytes-consed sample)
          (* 1000d0 (mesher-sample-gc-seconds sample))
          (mesher-sample-garbage-collections sample)))

(defun print-representative-trace (case stream)
  (let ((trace
          (luv:make-cpu-trace
           :label (format nil "LUFT ~D^3 ~(~A~) full-chain"
                          (mesher-case-size case)
                          (mesher-case-pattern case)))))
    (luv:with-cpu-trace (trace)
      (setf *mesher-benchmark-sink*
            (invoke-benchmark-phase case :full-chain)))
    (luv:print-cpu-trace trace stream)))

(defun run-mesher-benchmark
    (&key (sizes '(8 16 32 64))
          (patterns '(:solid :terrain :architecture :checkerboard))
          (sample-count 15) (warmup-count 3)
          (csv-pathname #P"build/luft-mesher-benchmark.csv")
          (trace-p t) (stream *standard-output*))
  "Benchmark LUFT's CPU lowering across deterministic chunk-shaped solids.

The CSV contains every untraced runtime observation.  TRACE-P adds one
separate nested explanation after each case; it is never included in the
sample distribution."
  (check-type sample-count (integer 1 *))
  (check-type warmup-count (integer 0 *))
  (let ((csv-pathname (merge-pathnames csv-pathname))
        (all-samples nil))
    (ensure-directories-exist csv-pathname)
    (with-open-file (csv csv-pathname :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
      (write-csv-header csv)
      (dolist (size sizes)
        (dolist (pattern patterns)
          (format stream "~&LUFT CPU mesher: building ~D^3 ~(~A~)...~%"
                  size pattern)
          (force-output stream)
          (let ((case (make-mesher-case size pattern)))
            (format stream "  ~:D occupied cells, ~:D exposed faces, ~:D record bytes~%"
                    (length (mesher-case-sites case))
                    (luft:chain-count (mesher-case-surface case))
                    (* luft:+face-record-byte-size+
                       (luft:chain-count (mesher-case-surface case))))
            (format stream "  validating dense occupancy...")
            (force-output stream)
            (validate-case-occupancy case)
            (format stream " exact~%")
            (dolist (phase +benchmark-phases+)
              (let ((samples
                      (measure-benchmark-phase
                       case phase sample-count warmup-count stream)))
                (print-phase-summary case phase samples stream)
                (loop for sample across samples
                      do (write-sample-csv sample csv)
                         (push sample all-samples))))
            (force-output csv)
            (when trace-p
              (print-representative-trace case stream))))))
    (format stream "~&Wrote ~:D samples to ~A~%"
            (length all-samples) csv-pathname)
    (values (coerce (nreverse all-samples) 'vector) csv-pathname)))
