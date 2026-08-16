(in-package #:luv.tests)

(defun scalar-test-position-energy (buffer)
  "Reference sum of squared positions over a generated columnar buffer."
  (records:with-columnar-buffer-storage
      ((length row (xs x) (ys y)) buffer test-position-columns)
    (declare (ignore row)
             (optimize (speed 3) (safety 1)))
    (loop with sum of-type single-float = 0f0
          for index fixnum below length
          for x single-float = (aref xs index)
          for y single-float = (aref ys index)
          do (incf sum (+ (* x x) (* y y)))
          finally (return sum))))

#+x86-64
(defun sse-test-position-energy (buffer)
  (records:with-columnar-buffer-storage
      ((length row (xs x) (ys y)) buffer test-position-columns)
    (declare (ignore row)
             (optimize (speed 3) (safety 0) (debug 0)))
    (let ((wide-end (the sb-simd:index (* 4 (floor length 4))))
          (sum (sb-simd-sse:f32.4 0f0)))
      (loop for index of-type sb-simd:index from 0 below wide-end by 4
            for x = (sb-simd-sse:f32.4-aref xs index)
            for y = (sb-simd-sse:f32.4-aref ys index)
            do (setf sum
                     (sb-simd-sse:f32.4+
                      sum
                      (sb-simd-sse:f32.4+
                       (sb-simd-sse:f32.4* x x)
                       (sb-simd-sse:f32.4* y y)))))
      (loop with result of-type single-float =
              (sb-simd-sse:f32.4-horizontal+ sum)
            for tail fixnum from wide-end below length
            do (setf result
                     (sb-simd:f32+
                      result
                      (sb-simd:f32+
                       (sb-simd:f32* (aref xs tail) (aref xs tail))
                       (sb-simd:f32* (aref ys tail) (aref ys tail)))))
            finally (return result)))))

#+x86-64
(defun avx-test-position-energy (buffer)
  (records:with-columnar-buffer-storage
      ((length row (xs x) (ys y)) buffer test-position-columns)
    (declare (ignore row)
             (optimize (speed 3) (safety 0) (debug 0)))
    (let ((wide-end (the sb-simd:index (* 8 (floor length 8))))
          (sum (sb-simd-avx:f32.8 0f0)))
      (loop for index of-type sb-simd:index from 0 below wide-end by 8
            for x = (sb-simd-avx:f32.8-aref xs index)
            for y = (sb-simd-avx:f32.8-aref ys index)
            do (setf sum
                     (sb-simd-avx:f32.8+
                      sum
                      (sb-simd-avx:f32.8+
                       (sb-simd-avx:f32.8* x x)
                       (sb-simd-avx:f32.8* y y)))))
      (loop with result of-type single-float =
              (sb-simd-avx:f32.8-horizontal+ sum)
            for tail fixnum from wide-end below length
            do (setf result
                     (sb-simd:f32+
                      result
                      (sb-simd:f32+
                       (sb-simd:f32* (aref xs tail) (aref xs tail))
                       (sb-simd:f32* (aref ys tail) (aref ys tail)))))
            finally (return result)))))

#+arm64
(defun neon-test-position-energy (buffer)
  (records:with-columnar-buffer-storage
      ((length row (xs x) (ys y)) buffer test-position-columns)
    (declare (ignore row)
             (optimize (speed 3) (safety 0) (debug 0)))
    (let ((wide-end (the sb-simd:index (* 4 (floor length 4))))
          (sum (sb-simd-neon:f32.4 0f0)))
      (loop for index of-type sb-simd:index from 0 below wide-end by 4
            for x = (sb-simd-neon:f32.4-aref xs index)
            for y = (sb-simd-neon:f32.4-aref ys index)
            do (setf sum
                     (sb-simd-neon:f32.4+
                      sum
                      (sb-simd-neon:f32.4+
                       (sb-simd-neon:f32.4* x x)
                       (sb-simd-neon:f32.4* y y)))))
      (loop with result of-type single-float =
              (sb-simd-neon:f32.4-horizontal+ sum)
            for tail fixnum from wide-end below length
            do (setf result
                     (sb-simd:f32+
                      result
                      (sb-simd:f32+
                       (sb-simd:f32* (aref xs tail) (aref xs tail))
                       (sb-simd:f32* (aref ys tail) (aref ys tail)))))
            finally (return result)))))

(defun simd-test-position-energy (buffer)
  "Run the native-width squared-position oracle with a scalar tail. #VKLLPR"
  #+x86-64
  (sb-simd:instruction-set-case
    (:avx (avx-test-position-energy buffer))
    (:sse (sse-test-position-energy buffer)))
  #+arm64
  (sb-simd:instruction-set-case
    (:neon (neon-test-position-energy buffer)))
  #-(or x86-64 arm64)
  (scalar-test-position-energy buffer))

(deftest generated-columnar-lanes-feed-native-simd-kernels
  (let ((buffer (make-test-position-columns :capacity 19)))
    (dotimes (index 19)
      (test-position-columns-push
       buffer (float (1+ index) 0f0) (float (mod index 7) 0f0)))
    (ok (= (scalar-test-position-energy buffer)
           (simd-test-position-energy buffer)))))
