;;;; CPU experiments for full-height LUFT occupancy fibers.

(defpackage #:luft.z-fiber-benchmark
  (:use #:cl)
  (:export #:available-kernel-families
           #:run-z-fiber-benchmark))

(in-package #:luft.z-fiber-benchmark)

(defconstant +fiber-words+ 4)
(defconstant +direction-count+ 6)
(defconstant +plus-x+ 0)
(defconstant +minus-x+ 1)
(defconstant +plus-y+ 2)
(defconstant +minus-y+ 3)
(defconstant +plus-z+ 4)
(defconstant +minus-z+ 5)
(defconstant +u64-mask+ #xffffffffffffffff)
(defconstant +low-63-mask+ #x7fffffffffffffff)
(defconstant +low-62-mask+ #x3fffffffffffffff)

(deftype u64-vector () '(simple-array (unsigned-byte 64) (*)))

(defstruct (fiber-case
             (:constructor %make-fiber-case
                 (width pattern stride occupancy solid-count
                  total-runs maximum-runs output)))
  (width 0 :type fixnum :read-only t)
  (pattern :terrain :type keyword :read-only t)
  (stride 0 :type fixnum :read-only t)
  (occupancy #() :type u64-vector :read-only t)
  (solid-count 0 :type fixnum :read-only t)
  (total-runs 0 :type fixnum :read-only t)
  (maximum-runs 0 :type fixnum :read-only t)
  (output #() :type u64-vector :read-only t))

(defstruct fiber-sample
  (width 0 :type fixnum)
  (pattern :terrain :type keyword)
  (phase :surface-scalar :type keyword)
  (simd-family :scalar :type keyword)
  (index 0 :type fixnum)
  (iterations 1 :type fixnum)
  (fiber-count 0 :type fixnum)
  (solid-count 0 :type fixnum)
  (run-count 0 :type fixnum)
  (face-count 0 :type fixnum)
  (elapsed-seconds 0d0 :type double-float)
  (bytes-consed 0 :type integer)
  (gc-seconds 0d0 :type double-float)
  (garbage-collections 0 :type fixnum))

(defvar *z-fiber-benchmark-sink* 0)

(declaim (inline input-base output-base occupancy-word solid-bit-p))

(defun input-base (stride local-x local-y)
  (declare (type fixnum stride local-x local-y)
           (optimize (speed 3) (safety 0)))
  (* +fiber-words+ (+ local-x (* local-y stride))))

(defun output-base (width x y)
  (declare (type fixnum width x y)
           (optimize (speed 3) (safety 0)))
  (* +fiber-words+ +direction-count+ (+ x (* y width))))

(defun occupancy-word (occupancy base z)
  (declare (type u64-vector occupancy)
           (type fixnum base z)
           (optimize (speed 3) (safety 0)))
  (aref occupancy (+ base (ash z -6))))

(defun solid-bit-p (occupancy base z)
  (declare (type u64-vector occupancy)
           (type fixnum base z)
           (optimize (speed 3) (safety 0)))
  (and (<= 0 z) (< z luft:+top-z+)
       (logbitp (logand z 63) (occupancy-word occupancy base z))))

(defun terrain-height (x y)
  (max 8
       (min 230
            (round (+ 112d0
                      (* 31d0 (sin (/ (+ x (* 0.31d0 y)) 17d0)))
                      (* 19d0 (cos (/ (- y (* 0.23d0 x)) 29d0))))))))

(defun architecture-solid-p (x y z)
  (let ((floor-z (* 16 (floor z 16))))
    (or (< z 4)
        (= z floor-z)
        (and (< (mod x 12) 2) (< z (+ floor-z 12)))
        (and (< (mod y 14) 2) (< z (+ floor-z 9)))
        (and (< (mod (+ x y) 31) 2)
             (<= 32 z 191)))))

(defun cave-solid-p (x y z)
  (and (< z (terrain-height x y))
       (or (< z 8)
           (> (+ (sin (/ x 5d0))
                 (cos (/ y 7d0))
                 (sin (/ z 4d0)))
              -0.35d0))))

(defun pattern-solid-p (pattern x y z)
  (ecase pattern
    (:solid t)
    (:terrain (< z (terrain-height x y)))
    (:architecture (architecture-solid-p x y z))
    (:caves (cave-solid-p x y z))
    (:checkerboard (evenp (+ x y z)))))

(defun make-fiber-occupancy (width pattern)
  (let* ((stride (+ width 2))
         (occupancy
           (make-array (* stride stride +fiber-words+)
                       :element-type '(unsigned-byte 64)
                       :initial-element 0))
         (origin-x 128)
         (origin-y 128)
         (solid-count 0))
    (dotimes (local-y stride)
      (dotimes (local-x stride)
        (let ((base (input-base stride local-x local-y))
              (x (+ origin-x (1- local-x)))
              (y (+ origin-y (1- local-y))))
          (dotimes (word +fiber-words+)
            (let ((bits 0))
              (dotimes (bit 64)
                (let ((z (+ bit (* 64 word))))
                  (when (and (< z luft:+top-z+)
                             (pattern-solid-p pattern x y z))
                    (setf bits (logior bits (ash 1 bit)))
                    (when (and (<= 1 local-x width)
                               (<= 1 local-y width))
                      (incf solid-count)))))
              (setf (aref occupancy (+ base word)) bits))))))
    (values occupancy stride solid-count)))

(defun fiber-run-count-scan (occupancy base)
  (declare (type u64-vector occupancy)
           (type fixnum base)
           (optimize (speed 3) (safety 0)))
  (let ((runs 1)
        (previous (if (solid-bit-p occupancy base 0) 1 0)))
    (declare (type fixnum runs previous))
    (loop for z fixnum from 1 below luft:+top-z+
          for bit fixnum = (if (solid-bit-p occupancy base z) 1 0)
          unless (= bit previous)
            do (incf runs)
               (setf previous bit))
    runs))

(defun fiber-run-count-bits (occupancy base)
  (declare (type u64-vector occupancy)
           (type fixnum base)
           (optimize (speed 3) (safety 0)))
  (let ((transitions 0))
    (declare (type fixnum transitions))
    (dotimes (word +fiber-words+)
      (let* ((bits (aref occupancy (+ base word)))
             (internal-mask (if (= word 3) +low-62-mask+ +low-63-mask+)))
        (incf transitions
              (logcount (logand internal-mask
                                (logxor bits (ash bits -1)))))
        (when (< word 3)
          (unless (= (ldb (byte 1 63) bits)
                     (ldb (byte 1 0) (aref occupancy (+ base word 1))))
            (incf transitions)))))
    (1+ transitions)))

(defun occupancy-run-statistics (occupancy stride width)
  (let ((total 0) (maximum 0))
    (declare (type fixnum total maximum))
    (dotimes (y width)
      (dotimes (x width)
        (let ((runs (fiber-run-count-bits
                     occupancy (input-base stride (1+ x) (1+ y)))))
          (incf total runs)
          (setf maximum (max maximum runs)))))
    (values total maximum)))

(defun make-fiber-case (width pattern)
  (check-type width (integer 1 256))
  (unless (member pattern '(:solid :terrain :architecture :caves :checkerboard))
    (error "Unknown Z-fiber benchmark pattern ~S." pattern))
  (multiple-value-bind (occupancy stride solid-count)
      (make-fiber-occupancy width pattern)
    (multiple-value-bind (total-runs maximum-runs)
        (occupancy-run-statistics occupancy stride width)
      (%make-fiber-case
       width pattern stride occupancy solid-count total-runs maximum-runs
       (make-array (* width width +direction-count+ +fiber-words+)
                   :element-type '(unsigned-byte 64)
                   :initial-element 0)))))

(declaim (inline write-vertical-masks))

(defun write-vertical-masks (occupancy input-base output output-base)
  (declare (type u64-vector occupancy output)
           (type fixnum input-base output-base)
           (optimize (speed 3) (safety 0)))
  (dotimes (word +fiber-words+)
    (let* ((solid (aref occupancy (+ input-base word)))
           (previous (if (plusp word)
                         (aref occupancy (+ input-base word -1))
                         0))
           (next (if (< word (1- +fiber-words+))
                     (aref occupancy (+ input-base word 1))
                     0))
           (above (logior (ash solid -1)
                          (ash (logand next 1) 63)))
           (below (logand +u64-mask+
                          (logior (ash solid 1)
                                  (ldb (byte 1 63) previous)))))
      (setf (aref output (+ output-base (* +plus-z+ +fiber-words+) word))
            (logand solid (lognot above))
            (aref output (+ output-base (* +minus-z+ +fiber-words+) word))
            (logand solid (lognot below)))))
  output)

(defun surface-masks-scalar (case)
  (let ((width (fiber-case-width case))
        (stride (fiber-case-stride case))
        (occupancy (fiber-case-occupancy case))
        (output (fiber-case-output case)))
    (declare (type fixnum width stride)
             (type u64-vector occupancy output)
             (optimize (speed 3) (safety 0)))
    (dotimes (y width)
      (dotimes (x width)
        (let* ((input-base (input-base stride (1+ x) (1+ y)))
               (output-base (output-base width x y))
               (right (+ input-base +fiber-words+))
               (left (- input-base +fiber-words+))
               (front (+ input-base (* stride +fiber-words+)))
               (back (- input-base (* stride +fiber-words+))))
          (declare (type fixnum input-base output-base right left front back))
          (dotimes (word +fiber-words+)
            (let ((solid (aref occupancy (+ input-base word))))
              (setf
               (aref output (+ output-base (* +plus-x+ +fiber-words+) word))
               (logand solid (lognot (aref occupancy (+ right word))))
               (aref output (+ output-base (* +minus-x+ +fiber-words+) word))
               (logand solid (lognot (aref occupancy (+ left word))))
               (aref output (+ output-base (* +plus-y+ +fiber-words+) word))
               (logand solid (lognot (aref occupancy (+ front word))))
               (aref output (+ output-base (* +minus-y+ +fiber-words+) word))
               (logand solid (lognot (aref occupancy (+ back word)))))))
          (write-vertical-masks occupancy input-base output output-base))))
    output))

(defmacro define-simd-surface-kernel (name package lanes)
  (flet ((sym (name) (intern name package)))
    (let ((aref-wide (sym (format nil "U64.~D-AREF" lanes)))
          (and-wide (sym (format nil "U64.~D-AND" lanes)))
          (not-wide (sym (format nil "U64.~D-NOT" lanes))))
      `(defun ,name (case)
         (let ((width (fiber-case-width case))
               (stride (fiber-case-stride case))
               (occupancy (fiber-case-occupancy case))
               (output (fiber-case-output case)))
           (declare (type fixnum width stride)
                    (type u64-vector occupancy output)
                    (optimize (speed 3) (safety 0)))
           (dotimes (y width)
             (dotimes (x width)
               (let* ((input-base (input-base stride (1+ x) (1+ y)))
                      (output-base (output-base width x y))
                      (right (+ input-base +fiber-words+))
                      (left (- input-base +fiber-words+))
                      (front (+ input-base (* stride +fiber-words+)))
                      (back (- input-base (* stride +fiber-words+))))
                 (declare (type fixnum input-base output-base
                                        right left front back))
                 (loop for word fixnum from 0 below +fiber-words+ by ,lanes
                       for solid = (,aref-wide occupancy (+ input-base word))
                       do (setf
                           (,aref-wide output
                                       (+ output-base
                                          (* +plus-x+ +fiber-words+) word))
                           (,and-wide solid
                                      (,not-wide (,aref-wide occupancy
                                                            (+ right word))))
                           (,aref-wide output
                                       (+ output-base
                                          (* +minus-x+ +fiber-words+) word))
                           (,and-wide solid
                                      (,not-wide (,aref-wide occupancy
                                                            (+ left word))))
                           (,aref-wide output
                                       (+ output-base
                                          (* +plus-y+ +fiber-words+) word))
                           (,and-wide solid
                                      (,not-wide (,aref-wide occupancy
                                                            (+ front word))))
                           (,aref-wide output
                                       (+ output-base
                                          (* +minus-y+ +fiber-words+) word))
                           (,and-wide solid
                                      (,not-wide (,aref-wide occupancy
                                                            (+ back word))))))
                 (write-vertical-masks
                  occupancy input-base output output-base))))
           output)))))

#+x86-64
(define-simd-surface-kernel surface-masks-avx2 #:sb-simd-avx2 4)

#+x86-64
(define-simd-surface-kernel surface-masks-sse2 #:sb-simd-sse2 2)

#+arm64
(define-simd-surface-kernel surface-masks-neon #:sb-simd-neon 2)

(defun instruction-set-available-p (name)
  (sb-simd-internals:instruction-set-available-p
   (sb-simd-internals:find-instruction-set name)))

(defun available-kernel-families ()
  "Return the runnable Z-fiber kernels, fastest first."
  (append
   #+x86-64 (and (instruction-set-available-p :avx2) '(:avx2))
   #+x86-64 (and (instruction-set-available-p :sse2) '(:sse2))
   #+arm64 (and (instruction-set-available-p :neon) '(:neon))
   '(:scalar)))

(defun fastest-simd-family ()
  (find-if (lambda (family) (not (eq family :scalar)))
           (available-kernel-families)))

(defun surface-kernel (family)
  (ecase family
    (:scalar #'surface-masks-scalar)
    #+x86-64 (:avx2 #'surface-masks-avx2)
    #+x86-64 (:sse2 #'surface-masks-sse2)
    #+arm64 (:neon #'surface-masks-neon)))

(declaim (inline set-output-bit))

(defun set-output-bit (output base direction z)
  (declare (type u64-vector output)
           (type fixnum base direction z)
           (optimize (speed 3) (safety 0)))
  (let ((index (+ base (* direction +fiber-words+) (ash z -6))))
    (setf (aref output index)
          (logior (aref output index) (ash 1 (logand z 63))))))

(defun surface-masks-cell-scan (case)
  "Slow per-cell oracle for the bit-fiber surface masks."
  (let ((width (fiber-case-width case))
        (stride (fiber-case-stride case))
        (occupancy (fiber-case-occupancy case))
        (output (fiber-case-output case)))
    (declare (type fixnum width stride)
             (type u64-vector occupancy output)
             (optimize (speed 3) (safety 0)))
    (fill output 0)
    (dotimes (y width)
      (dotimes (x width)
        (let* ((input-base (input-base stride (1+ x) (1+ y)))
               (output-base (output-base width x y))
               (right (+ input-base +fiber-words+))
               (left (- input-base +fiber-words+))
               (front (+ input-base (* stride +fiber-words+)))
               (back (- input-base (* stride +fiber-words+))))
          (declare (type fixnum input-base output-base right left front back))
          (dotimes (z luft:+top-z+)
            (when (solid-bit-p occupancy input-base z)
              (unless (solid-bit-p occupancy right z)
                (set-output-bit output output-base +plus-x+ z))
              (unless (solid-bit-p occupancy left z)
                (set-output-bit output output-base +minus-x+ z))
              (unless (solid-bit-p occupancy front z)
                (set-output-bit output output-base +plus-y+ z))
              (unless (solid-bit-p occupancy back z)
                (set-output-bit output output-base +minus-y+ z))
              (unless (solid-bit-p occupancy input-base (1+ z))
                (set-output-bit output output-base +plus-z+ z))
              (unless (solid-bit-p occupancy input-base (1- z))
                (set-output-bit output output-base +minus-z+ z)))))))
    output))

(defun output-face-count (output)
  (declare (type u64-vector output)
           (optimize (speed 3) (safety 0)))
  (loop for word across output sum (logcount word)))

(defun copy-output (case)
  (let* ((source (fiber-case-output case))
         (copy (make-array (length source)
                           :element-type '(unsigned-byte 64))))
    (replace copy source)
    copy))

(defun validate-case (case)
  (let* ((cell-output
           (progn (surface-masks-cell-scan case) (copy-output case)))
         (scalar-output
           (progn (surface-masks-scalar case) (copy-output case)))
         (simd-families (remove :scalar (available-kernel-families))))
    (unless (equalp cell-output scalar-output)
      (error "Cell and scalar-fiber masks disagree for ~D-wide ~(~A~)."
             (fiber-case-width case) (fiber-case-pattern case)))
    (dolist (simd-family simd-families)
      (funcall (surface-kernel simd-family) case)
      (unless (equalp scalar-output (fiber-case-output case))
        (error "Scalar and ~A masks disagree for ~D-wide ~(~A~)."
               simd-family (fiber-case-width case) (fiber-case-pattern case))))
    (dotimes (y (fiber-case-width case))
      (dotimes (x (fiber-case-width case))
        (let ((base (input-base (fiber-case-stride case) (1+ x) (1+ y))))
          (unless (= (fiber-run-count-scan (fiber-case-occupancy case) base)
                     (fiber-run-count-bits (fiber-case-occupancy case) base))
            (error "Run counters disagree at (~D,~D) for ~(~A~)."
                   x y (fiber-case-pattern case))))))
    (surface-masks-scalar case)
    (values (output-face-count scalar-output) simd-families)))

(defparameter +benchmark-phases+
  '(:runs-scan :runs-bits :surface-cell :surface-scalar :surface-simd))

(defun phase-zone (phase)
  (ecase phase
    (:runs-scan :luft/z-fiber/runs-scan)
    (:runs-bits :luft/z-fiber/runs-bits)
    (:surface-cell :luft/z-fiber/surface-cell)
    (:surface-scalar :luft/z-fiber/surface-scalar)
    (:surface-simd :luft/z-fiber/surface-simd)))

(defun count-runs-with (case function)
  (let ((total 0)
        (width (fiber-case-width case))
        (stride (fiber-case-stride case))
        (occupancy (fiber-case-occupancy case)))
    (declare (type fixnum total width stride)
             (type u64-vector occupancy))
    (dotimes (y width total)
      (dotimes (x width)
        (incf total
              (funcall function occupancy
                       (input-base stride (1+ x) (1+ y))))))))

(defun invoke-phase (case phase simd-family)
  (luv:with-cpu-trace-zone
      ((phase-zone phase) :tracy-value (fiber-case-solid-count case))
    (setf *z-fiber-benchmark-sink*
          (ecase phase
            (:runs-scan (count-runs-with case #'fiber-run-count-scan))
            (:runs-bits (count-runs-with case #'fiber-run-count-bits))
            (:surface-cell
             (aref (surface-masks-cell-scan case) 0))
            (:surface-scalar
             (aref (surface-masks-scalar case) 0))
            (:surface-simd
             (aref (funcall (surface-kernel simd-family) case) 0))))))

(defun calibrate-iterations (case phase simd-family)
  (loop with iterations fixnum = 1
        with target-seconds = 0.025d0
        do (let ((observation (luv:make-runtime-observation)))
             (luv:with-runtime-observation (observation)
               (dotimes (index iterations)
                 (declare (ignore index))
                 (invoke-phase case phase simd-family)))
             (when (or
                    (>= (luv:runtime-observation-elapsed-seconds observation)
                        target-seconds)
                       (>= iterations 1048576))
               (return iterations)))
           (setf iterations (min 1048576 (* 2 iterations)))))

(defun measure-phase
    (case phase simd-family sample-count warmup-count stream face-count)
  (format stream "  ~18A " phase)
  (force-output stream)
  (dotimes (index warmup-count)
    (declare (ignore index))
    (write-char #\w stream)
    (force-output stream)
    (invoke-phase case phase simd-family))
  (let ((iterations (calibrate-iterations case phase simd-family)))
    (format stream " x~D " iterations)
    (force-output stream)
    (sb-ext:gc :full t)
    (let ((samples (make-array sample-count)))
      (dotimes (index sample-count)
        (write-char #\. stream)
        (force-output stream)
        (let ((observation (luv:make-runtime-observation)))
          (luv:with-runtime-observation (observation)
            (dotimes (iteration iterations)
              (declare (ignore iteration))
              (invoke-phase case phase simd-family)))
          (setf (aref samples index)
                (make-fiber-sample
                 :width (fiber-case-width case)
                 :pattern (fiber-case-pattern case)
                 :phase phase :simd-family simd-family
                 :index index :iterations iterations
                 :fiber-count (* (fiber-case-width case)
                                 (fiber-case-width case))
                 :solid-count (fiber-case-solid-count case)
                 :run-count (fiber-case-total-runs case)
                 :face-count face-count
                 :elapsed-seconds
                 (/ (luv:runtime-observation-elapsed-seconds observation)
                    iterations)
                 :bytes-consed
                 (round (/ (luv:runtime-observation-bytes-consed observation)
                           iterations))
                 :gc-seconds
                 (/ (luv:runtime-observation-gc-seconds observation)
                    iterations)
                 :garbage-collections
                 (luv:runtime-observation-garbage-collections observation)))))
      (terpri stream)
      samples)))

(defun percentile (values fraction)
  (let* ((sorted (sort (copy-seq values) #'<))
         (index (round (* fraction (1- (length sorted))))))
    (aref sorted index)))

(defun sample-elapsed-milliseconds (sample)
  (* 1000d0 (fiber-sample-elapsed-seconds sample)))

(defun print-phase-summary (case phase samples stream)
  (declare (ignore phase))
  (let ((milliseconds (map 'vector #'sample-elapsed-milliseconds samples))
        (bytes (map 'vector #'fiber-sample-bytes-consed samples)))
    (format stream
            "    p50 ~,4F ms  p95 ~,4F ms  ~,3F ns/fiber  ~,3F KiB~%"
            (percentile milliseconds 0.50d0)
            (percentile milliseconds 0.95d0)
            (* 1d6 (percentile milliseconds 0.50d0)
               (/ (* (fiber-case-width case) (fiber-case-width case))))
            (/ (percentile bytes 0.50d0) 1024d0))))

(defun write-csv-header (stream)
  (format stream
          "width,pattern,phase,simd_family,sample,iterations,fibers,cells,solid_cells,runs,faces,output_bytes,elapsed_ms,allocated_bytes,gc_ms,batch_gc_count~%"))

(defun write-sample-csv (sample stream)
  (format stream "~D,~(~A~),~(~A~),~(~A~),~D,~D,~D,~D,~D,~D,~D,~D,~,6F,~D,~,6F,~D~%"
          (fiber-sample-width sample)
          (fiber-sample-pattern sample)
          (fiber-sample-phase sample)
          (fiber-sample-simd-family sample)
          (fiber-sample-index sample)
          (fiber-sample-iterations sample)
          (fiber-sample-fiber-count sample)
          (* (fiber-sample-fiber-count sample) luft:+top-z+)
          (fiber-sample-solid-count sample)
          (fiber-sample-run-count sample)
          (fiber-sample-face-count sample)
          (* (fiber-sample-fiber-count sample)
             +direction-count+ +fiber-words+ 8)
          (sample-elapsed-milliseconds sample)
          (fiber-sample-bytes-consed sample)
          (* 1000d0 (fiber-sample-gc-seconds sample))
          (fiber-sample-garbage-collections sample)))

(defun run-z-fiber-benchmark
    (&key (widths '(16 32))
          (patterns '(:solid :terrain :architecture :caves :checkerboard))
          (sample-count 15) (warmup-count 3)
          (csv-pathname #P"build/luft-z-fiber-benchmark.csv")
          (stream *standard-output*))
  "Benchmark per-cell, scalar bit-fiber, and native SIMD surface extraction."
  (check-type sample-count (integer 1 *))
  (check-type warmup-count (integer 0 *))
  (let ((simd-family (fastest-simd-family))
        (csv-pathname (merge-pathnames csv-pathname))
        (all-samples nil))
    (unless simd-family
      (error "No native SIMD family is available; found ~S."
             (available-kernel-families)))
    (format stream "LUFT Z fibers: kernels ~{~(~A~)~^, ~}; benchmarking ~A.~%"
            (available-kernel-families) simd-family)
    (ensure-directories-exist csv-pathname)
    (with-open-file (csv csv-pathname :direction :output
                         :if-exists :supersede :if-does-not-exist :create)
      (write-csv-header csv)
      (dolist (width widths)
        (dolist (pattern patterns)
          (format stream "~&Building ~Dx~D full-height ~(~A~) fibers...~%"
                  width width pattern)
          (force-output stream)
          (let ((case (make-fiber-case width pattern)))
            (multiple-value-bind (face-count validated-family)
                (validate-case case)
              (declare (ignore validated-family))
              (format stream
                      "  ~:D solids, ~:D faces, ~:D runs (~,2F/fiber, max ~D); exact masks~%"
                      (fiber-case-solid-count case) face-count
                      (fiber-case-total-runs case)
                      (/ (fiber-case-total-runs case)
                         (* width width 1d0))
                      (fiber-case-maximum-runs case))
              (dolist (phase +benchmark-phases+)
                (let ((samples
                        (measure-phase case phase simd-family sample-count
                                       warmup-count stream face-count)))
                  (print-phase-summary case phase samples stream)
                  (loop for sample across samples
                        do (write-sample-csv sample csv)
                           (push sample all-samples))))
              (force-output csv))))))
    (format stream "~&Wrote ~:D samples to ~A~%"
            (length all-samples) csv-pathname)
    (values (coerce (nreverse all-samples) 'vector) csv-pathname)))
