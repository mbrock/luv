(defpackage #:luft.z-fiber-benchmark.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip)
  (:local-nicknames (#:z #:luft.z-fiber-benchmark)))

(in-package #:luft.z-fiber-benchmark.tests)

(define-test scalar-fibers-match-the-cell-oracle
  (dolist (pattern '(:solid :terrain :architecture :caves :checkerboard))
    (let* ((case (z::make-fiber-case 4 pattern))
           (cell
             (progn (z::surface-masks-cell-scan case) (z::copy-output case)))
           (scalar
             (progn (z::surface-masks-scalar case) (z::copy-output case))))
      (true (equalp cell scalar) (format nil "~A surface masks" pattern))
      (dotimes (y 4)
        (dotimes (x 4)
          (let ((base (z::input-base
                       (z::fiber-case-stride case) (1+ x) (1+ y))))
            (true (= (z::fiber-run-count-scan
                      (z::fiber-case-occupancy case) base)
                     (z::fiber-run-count-bits
                      (z::fiber-case-occupancy case) base))
                  (format nil "~A runs at (~D,~D)" pattern x y))))))))

(define-test native-simd-matches-scalar-fibers
  (let ((families (remove :scalar (z:available-kernel-families))))
    (true families "A native SIMD family is available")
    (dolist (family families)
      (dolist (pattern '(:solid :terrain :architecture :caves :checkerboard))
        (let* ((case (z::make-fiber-case 5 pattern))
               (scalar
                 (progn (z::surface-masks-scalar case) (z::copy-output case))))
          (funcall (z::surface-kernel family) case)
          (true (equalp scalar (z::fiber-case-output case))
                (format nil "~A agrees for ~A" family pattern)))))))

(define-test camera-air-algorithms-find-the-same-boundary
  (dolist (pattern '(:solid :terrain :architecture :caves :checkerboard))
    (let* ((case (z::make-fiber-case 5 pattern))
           (cell
             (progn (z::camera-boundary-cell-flood case) (z::copy-output case)))
           (bits
             (progn (z::camera-boundary-bit-waves case) (z::copy-output case))))
      (z::camera-boundary-run-flood case)
      (true (equalp cell bits)
            (format nil "~A cell and bit-wave camera boundary" pattern))
      (true (equalp cell (z::fiber-case-output case))
            (format nil "~A cell and air-run camera boundary" pattern)))))

(define-test camera-air-excludes-the-ground-underside
  (let* ((width 4)
         (case (z::make-fiber-case width :terrain)))
    (z::surface-masks-scalar case)
    (let ((complete-faces (z::output-face-count (z::fiber-case-output case))))
      (z::camera-boundary-run-flood case)
      (true (= (- complete-faces (* width width))
               (z::output-face-count (z::fiber-case-output case)))))))

(define-test maintained-atmosphere-chain-matches-rebuilds
  (let* ((maintained (z::make-atmosphere-edit-case 6))
         (rebuilt (z::make-atmosphere-edit-case 6))
         (maintained-case (z::atmosphere-edit-case-case maintained))
         (rebuilt-case (z::atmosphere-edit-case-case rebuilt)))
    (dotimes (edit 32)
      (z::maintain-atmosphere-surface-edit maintained)
      (z::rebuild-atmosphere-after-edit rebuilt)
      (true (equalp (z::fiber-case-occupancy maintained-case)
                    (z::fiber-case-occupancy rebuilt-case))
            (format nil "occupancy after surface edit ~D" edit))
      (true (equalp (z::fiber-case-output maintained-case)
                    (z::fiber-case-output rebuilt-case))
            (format nil "atmosphere chain after surface edit ~D" edit))
      (true (= (z::fiber-case-solid-count maintained-case)
               (z::fiber-case-solid-count rebuilt-case))
            (format nil "solid count after surface edit ~D" edit))
      (let ((maintained-air (z::fiber-case-air-workspace maintained-case))
            (rebuilt-air (z::fiber-case-air-workspace rebuilt-case)))
        (true (equalp (z::air-workspace-run-offsets maintained-air)
                      (z::air-workspace-run-offsets rebuilt-air))
              (format nil "air-run offsets after surface edit ~D" edit))
        (true (equalp (z::air-workspace-run-starts maintained-air)
                      (z::air-workspace-run-starts rebuilt-air))
              (format nil "air-run starts after surface edit ~D" edit))
        (true (equalp (z::air-workspace-run-ends maintained-air)
                      (z::air-workspace-run-ends rebuilt-air))
              (format nil "air-run ends after surface edit ~D" edit))))))

(define-test full-height-solid-and-checkerboard-have-expected-shape
  (let ((solid (z::make-fiber-case 4 :solid))
        (checkerboard (z::make-fiber-case 4 :checkerboard)))
    (z::surface-masks-scalar solid)
    (true (= 32 (z::output-face-count (z::fiber-case-output solid))))
    (true (= 16 (z::fiber-case-total-runs solid)))
    (true (= 1 (z::fiber-case-maximum-runs solid)))
    (true (= (* 16 luft:+top-z+)
             (z::fiber-case-total-runs checkerboard)))
    (true (= luft:+top-z+ (z::fiber-case-maximum-runs checkerboard)))
    (true (loop for index from 3 below (length (z::fiber-case-occupancy solid))
                by z::+fiber-words+
                always (not (logbitp 63 (aref (z::fiber-case-occupancy solid)
                                              index))))
          "Bit 255 remains the air sentinel")))
