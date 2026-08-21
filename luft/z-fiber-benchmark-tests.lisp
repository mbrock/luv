(defpackage #:luft.z-fiber-benchmark.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:z #:luft.z-fiber-benchmark)))

(in-package #:luft.z-fiber-benchmark.tests)

(deftest scalar-fibers-match-the-cell-oracle
  (dolist (pattern '(:solid :terrain :architecture :caves :checkerboard))
    (let* ((case (z::make-fiber-case 4 pattern))
           (cell
             (progn (z::surface-masks-cell-scan case) (z::copy-output case)))
           (scalar
             (progn (z::surface-masks-scalar case) (z::copy-output case))))
      (ok (equalp cell scalar) (format nil "~A surface masks" pattern))
      (dotimes (y 4)
        (dotimes (x 4)
          (let ((base (z::input-base
                       (z::fiber-case-stride case) (1+ x) (1+ y))))
            (ok (= (z::fiber-run-count-scan
                    (z::fiber-case-occupancy case) base)
                   (z::fiber-run-count-bits
                    (z::fiber-case-occupancy case) base))
                (format nil "~A runs at (~D,~D)" pattern x y))))))))

(deftest native-simd-matches-scalar-fibers
  (let ((families (remove :scalar (z:available-kernel-families))))
    (ok families "A native SIMD family is available")
    (dolist (family families)
      (dolist (pattern '(:solid :terrain :architecture :caves :checkerboard))
        (let* ((case (z::make-fiber-case 5 pattern))
               (scalar
                 (progn (z::surface-masks-scalar case) (z::copy-output case))))
          (funcall (z::surface-kernel family) case)
          (ok (equalp scalar (z::fiber-case-output case))
              (format nil "~A agrees for ~A" family pattern)))))))

(deftest camera-air-algorithms-find-the-same-boundary
  (dolist (pattern '(:solid :terrain :architecture :caves :checkerboard))
    (let* ((case (z::make-fiber-case 5 pattern))
           (cell
             (progn (z::camera-boundary-cell-flood case) (z::copy-output case)))
           (bits
             (progn (z::camera-boundary-bit-waves case) (z::copy-output case))))
      (z::camera-boundary-run-flood case)
      (ok (equalp cell bits)
          (format nil "~A cell and bit-wave camera boundary" pattern))
      (ok (equalp cell (z::fiber-case-output case))
          (format nil "~A cell and air-run camera boundary" pattern)))))

(deftest camera-air-excludes-the-ground-underside
  (let* ((width 4)
         (case (z::make-fiber-case width :terrain)))
    (z::surface-masks-scalar case)
    (let ((complete-faces (z::output-face-count (z::fiber-case-output case))))
      (z::camera-boundary-run-flood case)
      (ok (= (- complete-faces (* width width))
             (z::output-face-count (z::fiber-case-output case)))))))

(deftest full-height-solid-and-checkerboard-have-expected-shape
  (let ((solid (z::make-fiber-case 4 :solid))
        (checkerboard (z::make-fiber-case 4 :checkerboard)))
    (z::surface-masks-scalar solid)
    (ok (= 32 (z::output-face-count (z::fiber-case-output solid))))
    (ok (= 16 (z::fiber-case-total-runs solid)))
    (ok (= 1 (z::fiber-case-maximum-runs solid)))
    (ok (= (* 16 luft:+top-z+)
           (z::fiber-case-total-runs checkerboard)))
    (ok (= luft:+top-z+ (z::fiber-case-maximum-runs checkerboard)))
    (ok (loop for index from 3 below (length (z::fiber-case-occupancy solid))
              by z::+fiber-words+
              always (not (logbitp 63 (aref (z::fiber-case-occupancy solid)
                                            index))))
        "Bit 255 remains the air sentinel")))
