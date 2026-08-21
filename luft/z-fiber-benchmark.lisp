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

(defstruct (air-workspace
             (:constructor %make-air-workspace
                 (reachable-a reachable-b cell-queue run-offsets
                  run-starts run-ends run-owners run-reached run-queue)))
  reachable-a reachable-b cell-queue run-offsets
  run-starts run-ends run-owners run-reached run-queue)

(deftype u64-vector () '(simple-array (unsigned-byte 64) (*)))

(defstruct (fiber-case
             (:constructor %make-fiber-case
                 (width pattern stride occupancy solid-count
                  total-runs maximum-runs output air-workspace)))
  (width 0 :type fixnum :read-only t)
  (pattern :terrain :type keyword :read-only t)
  (stride 0 :type fixnum :read-only t)
  (occupancy #() :type u64-vector :read-only t)
  (solid-count 0 :type fixnum :read-only t)
  (total-runs 0 :type fixnum :read-only t)
  (maximum-runs 0 :type fixnum :read-only t)
  (output #() :type u64-vector :read-only t)
  (air-workspace nil :type air-workspace :read-only t))

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
  (air-run-count 0 :type fixnum)
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

(defun make-air-workspace (occupancy stride)
  "Index every maximal empty Z interval in every haloed fiber."
  (let* ((fiber-count (* stride stride))
         (run-offsets (make-array (1+ fiber-count) :element-type 'fixnum))
         (starts (make-array 64 :element-type '(unsigned-byte 8)
                                :adjustable t :fill-pointer 0))
         (ends (make-array 64 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
         (owners (make-array 64 :element-type 'fixnum
                                :adjustable t :fill-pointer 0)))
    (dotimes (fiber fiber-count)
      (setf (aref run-offsets fiber) (length starts))
      (let ((base (* fiber +fiber-words+))
            (z 0))
        (loop while (< z luft:+top-z+)
              do (if (solid-bit-p occupancy base z)
                     (incf z)
                     (let ((start z))
                       (loop while (and (< z luft:+top-z+)
                                        (not (solid-bit-p occupancy base z)))
                             do (incf z))
                       (vector-push-extend start starts)
                       (vector-push-extend (1- z) ends)
                       (vector-push-extend fiber owners))))))
    (setf (aref run-offsets fiber-count) (length starts))
    (let ((run-count (length starts)))
      (%make-air-workspace
       (make-array (length occupancy) :element-type '(unsigned-byte 64)
                                      :initial-element 0)
       (make-array (length occupancy) :element-type '(unsigned-byte 64)
                                      :initial-element 0)
       (make-array (* fiber-count luft:+top-z+) :element-type 'fixnum)
       run-offsets
       (coerce starts '(simple-array (unsigned-byte 8) (*)))
       (coerce ends '(simple-array (unsigned-byte 8) (*)))
       (coerce owners '(simple-array fixnum (*)))
       (make-array run-count :element-type 'bit :initial-element 0)
       (make-array run-count :element-type 'fixnum)))))

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
                   :initial-element 0)
       (make-air-workspace occupancy stride)))))

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

;;; Camera-connected air is a different question from the complete boundary.
;;; The complete boundary includes sealed caves.  These kernels seed the air
;;; containing a camera above the center fiber, discover only that component,
;;; and then write the solid faces incident to it.  AIR-CELL is the obvious
;;; oracle, AIR-BITS propagates directly through the four-word fibers, and
;;; AIR-RUNS traverses maximal empty Z intervals joined by horizontal overlap.

(declaim (inline reachable-bit-p mark-reachable-bit))

(defun reachable-bit-p (reachable fiber z)
  (declare (type u64-vector reachable)
           (type fixnum fiber z)
           (optimize (speed 3) (safety 0)))
  (logbitp (logand z 63)
           (aref reachable (+ (* fiber +fiber-words+) (ash z -6)))))

(defun mark-reachable-bit (reachable fiber z)
  (declare (type u64-vector reachable)
           (type fixnum fiber z)
           (optimize (speed 3) (safety 0)))
  (let ((index (+ (* fiber +fiber-words+) (ash z -6))))
    (setf (aref reachable index)
          (logior (aref reachable index) (ash 1 (logand z 63))))))

(defun camera-air-seed (case)
  "Return the center fiber's top air cell, or NIL when the camera is obstructed."
  (let* ((stride (fiber-case-stride case))
         (center (floor stride 2))
         (fiber (+ center (* center stride)))
         (base (* fiber +fiber-words+))
         (z (1- luft:+top-z+)))
    (if (solid-bit-p (fiber-case-occupancy case) base z)
        (values nil nil)
        (values fiber z))))

(declaim (inline write-reachable-range))

(defun write-reachable-range (reachable fiber low high)
  (declare (type u64-vector reachable)
           (type fixnum fiber low high)
           (optimize (speed 3) (safety 0)))
  (let ((base (* fiber +fiber-words+))
        (first-word (ash low -6))
        (last-word (ash high -6)))
    (loop for word fixnum from first-word to last-word
          for word-low fixnum = (* word 64)
          for local-low fixnum = (max 0 (- low word-low))
          for local-high fixnum = (min 63 (- high word-low))
          for mask = (logand +u64-mask+
                             (- (ash 1 (1+ local-high))
                                (ash 1 local-low)))
          do (setf (aref reachable (+ base word))
                   (logior (aref reachable (+ base word)) mask))))
  reachable)

(defun write-camera-boundary (case reachable)
  "Write solid faces incident to REACHABLE air into CASE's output buffer."
  (let ((width (fiber-case-width case))
        (stride (fiber-case-stride case))
        (occupancy (fiber-case-occupancy case))
        (output (fiber-case-output case)))
    (declare (type fixnum width stride)
             (type u64-vector occupancy reachable output)
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
            (let* ((solid (aref occupancy (+ input-base word)))
                   (air (aref reachable (+ input-base word)))
                   (previous (if (plusp word)
                                 (aref reachable (+ input-base word -1))
                                 0))
                   (next (if (< word (1- +fiber-words+))
                             (aref reachable (+ input-base word 1))
                             0))
                   (air-above (logior (ash air -1)
                                      (ash (logand next 1) 63)))
                   (air-below (logand +u64-mask+
                                      (logior (ash air 1)
                                              (ldb (byte 1 63) previous)))))
              (setf
               (aref output (+ output-base (* +plus-x+ +fiber-words+) word))
               (logand solid (aref reachable (+ right word)))
               (aref output (+ output-base (* +minus-x+ +fiber-words+) word))
               (logand solid (aref reachable (+ left word)))
               (aref output (+ output-base (* +plus-y+ +fiber-words+) word))
               (logand solid (aref reachable (+ front word)))
               (aref output (+ output-base (* +minus-y+ +fiber-words+) word))
               (logand solid (aref reachable (+ back word)))
               (aref output (+ output-base (* +plus-z+ +fiber-words+) word))
               (logand solid air-above)
               (aref output (+ output-base (* +minus-z+ +fiber-words+) word))
               (logand solid air-below)))))))
    output))

(defun write-camera-boundary-cell-scan (case reachable)
  "Slow, direct oracle for the packed camera-boundary writer."
  (let ((width (fiber-case-width case))
        (stride (fiber-case-stride case))
        (occupancy (fiber-case-occupancy case))
        (output (fiber-case-output case)))
    (declare (type fixnum width stride)
             (type u64-vector occupancy reachable output)
             (optimize (speed 3) (safety 0)))
    (fill output 0)
    (dotimes (y width)
      (dotimes (x width)
        (let* ((input-base (input-base stride (1+ x) (1+ y)))
               (input-fiber (floor input-base +fiber-words+))
               (output-base (output-base width x y)))
          (declare (type fixnum input-base input-fiber output-base))
          (dotimes (z luft:+top-z+)
            (when (solid-bit-p occupancy input-base z)
              (when (reachable-bit-p reachable (1+ input-fiber) z)
                (set-output-bit output output-base +plus-x+ z))
              (when (reachable-bit-p reachable (1- input-fiber) z)
                (set-output-bit output output-base +minus-x+ z))
              (when (reachable-bit-p reachable (+ input-fiber stride) z)
                (set-output-bit output output-base +plus-y+ z))
              (when (reachable-bit-p reachable (- input-fiber stride) z)
                (set-output-bit output output-base +minus-y+ z))
              (when (and (< z (1- luft:+top-z+))
                         (reachable-bit-p reachable input-fiber (1+ z)))
                (set-output-bit output output-base +plus-z+ z))
              (when (and (plusp z)
                         (reachable-bit-p reachable input-fiber (1- z)))
                (set-output-bit output output-base +minus-z+ z)))))))
    output))

(defun camera-boundary-cell-flood (case)
  "Reference breadth-first search over individual empty cells."
  (let* ((stride (fiber-case-stride case))
         (fiber-count (* stride stride))
         (occupancy (fiber-case-occupancy case))
         (workspace (fiber-case-air-workspace case))
         (reachable (air-workspace-reachable-a workspace))
         (queue (air-workspace-cell-queue workspace))
         (head 0)
         (tail 0))
    (declare (type fixnum stride fiber-count head tail)
             (type u64-vector occupancy reachable)
             (type (simple-array fixnum (*)) queue)
             (optimize (speed 3) (safety 0)))
    (fill reachable 0)
    (labels ((visit (fiber z)
               (declare (type fixnum fiber z))
               (when (and (<= 0 fiber) (< fiber fiber-count)
                          (<= 0 z) (< z luft:+top-z+)
                          (not (solid-bit-p occupancy
                                            (* fiber +fiber-words+) z))
                          (not (reachable-bit-p reachable fiber z)))
                 (mark-reachable-bit reachable fiber z)
                 (setf (aref queue tail) (+ (* fiber luft:+top-z+) z))
                 (incf tail))))
      (multiple-value-bind (seed-fiber seed-z) (camera-air-seed case)
        (when seed-fiber
          (visit seed-fiber seed-z)))
      (loop while (< head tail)
            for packed fixnum = (aref queue head)
            do (incf head)
               (multiple-value-bind (fiber z)
                   (floor packed luft:+top-z+)
                 (declare (type fixnum fiber z))
                 (let ((x (mod fiber stride)))
                   (declare (type fixnum x))
                   (when (plusp x) (visit (1- fiber) z))
                   (when (< x (1- stride)) (visit (1+ fiber) z)))
                 (when (>= fiber stride) (visit (- fiber stride) z))
                 (when (< fiber (- fiber-count stride))
                   (visit (+ fiber stride) z))
                 (when (plusp z) (visit fiber (1- z)))
                 (when (< z (1- luft:+top-z+)) (visit fiber (1+ z))))))
    (write-camera-boundary-cell-scan case reachable)))

(defun camera-boundary-bit-waves (case)
  "Propagate reachable air through packed masks one cell per fixed-point wave."
  (let* ((stride (fiber-case-stride case))
         (fiber-count (* stride stride))
         (occupancy (fiber-case-occupancy case))
         (workspace (fiber-case-air-workspace case))
         (source (air-workspace-reachable-a workspace))
         (destination (air-workspace-reachable-b workspace)))
    (declare (type fixnum stride fiber-count)
             (type u64-vector occupancy source destination)
             (optimize (speed 3) (safety 0)))
    (fill source 0)
    (fill destination 0)
    (multiple-value-bind (seed-fiber seed-z) (camera-air-seed case)
      (when seed-fiber
        (mark-reachable-bit source seed-fiber seed-z)))
    (loop
      (let ((changed nil))
        (dotimes (fiber fiber-count)
          (let ((base (* fiber +fiber-words+))
                (x (mod fiber stride)))
            (declare (type fixnum base x))
            (dotimes (word +fiber-words+)
              (let* ((index (+ base word))
                     (current (aref source index))
                     (previous (if (plusp word)
                                   (aref source (1- index)) 0))
                     (next (if (< word (1- +fiber-words+))
                               (aref source (1+ index)) 0))
                     (neighbors
                       (logior
                        current
                        (logand +u64-mask+
                                (logior (ash current 1)
                                        (ldb (byte 1 63) previous)))
                        (logior (ash current -1)
                                (ash (logand next 1) 63))
                        (if (plusp x)
                            (aref source (- index +fiber-words+)) 0)
                        (if (< x (1- stride))
                            (aref source (+ index +fiber-words+)) 0)
                        (if (>= fiber stride)
                            (aref source (- index (* stride +fiber-words+))) 0)
                        (if (< fiber (- fiber-count stride))
                            (aref source (+ index (* stride +fiber-words+))) 0)))
                     (valid (if (= word 3) +low-63-mask+ +u64-mask+))
                     (new (logand valid neighbors
                                  (lognot (aref occupancy index)))))
                (setf (aref destination index) new)
                (unless (= new current)
                  (setf changed t))))))
        (unless changed
          (return (write-camera-boundary case source)))
        (rotatef source destination)))))

(defun camera-boundary-run-flood (case)
  "Flood maximal air intervals; horizontal overlap is the adjacency test."
  (let* ((stride (fiber-case-stride case))
         (workspace (fiber-case-air-workspace case))
         (offsets (air-workspace-run-offsets workspace))
         (starts (air-workspace-run-starts workspace))
         (ends (air-workspace-run-ends workspace))
         (owners (air-workspace-run-owners workspace))
         (reached (air-workspace-run-reached workspace))
         (queue (air-workspace-run-queue workspace))
         (reachable (air-workspace-reachable-a workspace))
         (head 0)
         (tail 0))
    (declare (type fixnum stride head tail)
             (type (simple-array fixnum (*)) offsets owners queue)
             (type (simple-array (unsigned-byte 8) (*)) starts ends)
             (type simple-bit-vector reached)
             (type u64-vector reachable)
             (optimize (speed 3) (safety 0)))
    (fill reached 0)
    (fill reachable 0)
    (labels ((admit (run)
               (declare (type fixnum run))
               (when (zerop (sbit reached run))
                 (setf (sbit reached run) 1
                       (aref queue tail) run)
                 (incf tail)))
             (visit-fiber (neighbor low high)
               (declare (type fixnum neighbor low high))
               (loop for run fixnum from (aref offsets neighbor)
                     below (aref offsets (1+ neighbor))
                     when (> (aref starts run) high)
                       do (loop-finish)
                     when (>= (aref ends run) low)
                       do (admit run))))
      (multiple-value-bind (seed-fiber seed-z) (camera-air-seed case)
        (when seed-fiber
          (loop for run fixnum from (aref offsets seed-fiber)
                below (aref offsets (1+ seed-fiber))
                when (<= (aref starts run) seed-z (aref ends run))
                  do (admit run)
                     (loop-finish))))
      (loop while (< head tail)
            for run fixnum = (aref queue head)
            for fiber fixnum = (aref owners run)
            for low fixnum = (aref starts run)
            for high fixnum = (aref ends run)
            do (incf head)
               (let ((x (mod fiber stride)))
                 (declare (type fixnum x))
                 (when (plusp x) (visit-fiber (1- fiber) low high))
                 (when (< x (1- stride))
                   (visit-fiber (1+ fiber) low high)))
               (when (>= fiber stride)
                 (visit-fiber (- fiber stride) low high))
               (when (< fiber (- (* stride stride) stride))
                 (visit-fiber (+ fiber stride) low high)))
      (dotimes (run (length starts))
        (when (= 1 (sbit reached run))
          (write-reachable-range reachable (aref owners run)
                                 (aref starts run) (aref ends run)))))
    (write-camera-boundary case reachable)))

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
         (simd-families (remove :scalar (available-kernel-families)))
         (camera-cell-output
           (progn (camera-boundary-cell-flood case) (copy-output case))))
    (unless (equalp cell-output scalar-output)
      (error "Cell and scalar-fiber masks disagree for ~D-wide ~(~A~)."
             (fiber-case-width case) (fiber-case-pattern case)))
    (dolist (simd-family simd-families)
      (funcall (surface-kernel simd-family) case)
      (unless (equalp scalar-output (fiber-case-output case))
        (error "Scalar and ~A masks disagree for ~D-wide ~(~A~)."
               simd-family (fiber-case-width case) (fiber-case-pattern case))))
    (camera-boundary-bit-waves case)
    (unless (equalp camera-cell-output (fiber-case-output case))
      (error "Cell and bit-wave camera boundaries disagree for ~D-wide ~(~A~)."
             (fiber-case-width case) (fiber-case-pattern case)))
    (camera-boundary-run-flood case)
    (unless (equalp camera-cell-output (fiber-case-output case))
      (error "Cell and air-run camera boundaries disagree for ~D-wide ~(~A~)."
             (fiber-case-width case) (fiber-case-pattern case)))
    (dotimes (y (fiber-case-width case))
      (dotimes (x (fiber-case-width case))
        (let ((base (input-base (fiber-case-stride case) (1+ x) (1+ y))))
          (unless (= (fiber-run-count-scan (fiber-case-occupancy case) base)
                     (fiber-run-count-bits (fiber-case-occupancy case) base))
            (error "Run counters disagree at (~D,~D) for ~(~A~)."
                   x y (fiber-case-pattern case))))))
    (surface-masks-scalar case)
    (values (output-face-count scalar-output)
            (output-face-count camera-cell-output)
            simd-families)))

(defparameter +benchmark-phases+
  '(:runs-scan :runs-bits :surface-cell :surface-scalar :surface-simd
    :air-cell :air-bits :air-runs))

(defun camera-air-phase-p (phase)
  (member phase '(:air-cell :air-bits :air-runs)))

(defun phase-zone (phase)
  (ecase phase
    (:runs-scan :luft/z-fiber/runs-scan)
    (:runs-bits :luft/z-fiber/runs-bits)
    (:surface-cell :luft/z-fiber/surface-cell)
    (:surface-scalar :luft/z-fiber/surface-scalar)
    (:surface-simd :luft/z-fiber/surface-simd)
    (:air-cell :luft/z-fiber/camera-air-cell)
    (:air-bits :luft/z-fiber/camera-air-bits)
    (:air-runs :luft/z-fiber/camera-air-runs)))

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
             (aref (funcall (surface-kernel simd-family) case) 0))
            (:air-cell (aref (camera-boundary-cell-flood case) 0))
            (:air-bits (aref (camera-boundary-bit-waves case) 0))
            (:air-runs (aref (camera-boundary-run-flood case) 0))))))

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
                 :air-run-count
                 (length (air-workspace-run-starts
                          (fiber-case-air-workspace case)))
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
          "width,pattern,phase,simd_family,sample,iterations,fibers,cells,solid_cells,runs,air_runs,faces,output_bytes,elapsed_ms,allocated_bytes,gc_ms,batch_gc_count~%"))

(defun write-sample-csv (sample stream)
  (format stream "~D,~(~A~),~(~A~),~(~A~),~D,~D,~D,~D,~D,~D,~D,~D,~D,~,6F,~D,~,6F,~D~%"
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
          (fiber-sample-air-run-count sample)
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
            (multiple-value-bind
                (face-count camera-face-count validated-families)
                (validate-case case)
              (declare (ignore validated-families))
              (format stream
                      "  ~:D solids, ~:D faces (~:D camera-air), ~:D runs (~,2F/fiber, max ~D), ~:D halo air runs; exact masks~%"
                      (fiber-case-solid-count case) face-count
                      camera-face-count
                      (fiber-case-total-runs case)
                      (/ (fiber-case-total-runs case)
                         (* width width 1d0))
                      (fiber-case-maximum-runs case)
                      (length (air-workspace-run-starts
                               (fiber-case-air-workspace case))))
              (dolist (phase +benchmark-phases+)
                (let* ((phase-face-count
                         (if (camera-air-phase-p phase)
                             camera-face-count face-count))
                       (samples
                         (measure-phase case phase simd-family sample-count
                                        warmup-count stream phase-face-count)))
                  (print-phase-summary case phase samples stream)
                  (loop for sample across samples
                        do (write-sample-csv sample csv)
                           (push sample all-samples))))
              (force-output csv))))))
    (format stream "~&Wrote ~:D samples to ~A~%"
            (length all-samples) csv-pathname)
    (values (coerce (nreverse all-samples) 'vector) csv-pathname)))
