;;;; One-tile CPU experiment on the alpine ridge (#QDEDGB).
;;;; From the checkout: ./env sbcl --script scripts/stride-four-ridge.lisp

(require :asdf)
(pushnew (truename "./") asdf:*central-registry* :test #'equal)
(asdf:load-system "luvcraft/luft")

(in-package #:cl-user)

(load (merge-pathnames "stride-four-ridge-fixture.lisp" *load-truename*))

(defun ridge-children (source)
  (let ((children (make-array '(2 2))))
    (dotimes (dx 2)
      (dotimes (dy 2)
        (setf (aref children dx dy)
              (luft.terrain:build-world-source-coarse-tile
               source (+ 12 dx) (+ 16 dy) :cull-interior-caves-p nil))))
    children))

(defun exact-ridge (source)
  (luft.terrain:reduce-stride-two-children 6 8 (ridge-children source)))

(defun native-ridge (source)
  (luft.terrain:build-native-stride-four-tile source 6 8))

(defun clear-bore-rays (tile)
  ;; Trace each ray all the way from one tile face to the other.
  (let ((words (luft.terrain:coarse-voxel-tile-words tile)))
    (loop for y from 6 below 10 sum
      (loop for z from 19 below 21 count
        (loop for x below 32 never (logbitp z (aref words (+ y (* 32 x)))))))))

(defun ridge-mesh (tile)
  (multiple-value-bind (sites apps lights stars)
      (luft.terrain:mesh-coarse-voxel-tile tile (constantly nil) nil)
    (assert (= (* stars 4) (length sites) (length lights)))
    (assert (= (* stars 8) (length apps)))
    (loop for i below (length sites) by 4
          for x = (- (aref sites i) luft.terrain:+coarse-tile-bias+)
          for y = (- (aref sites (+ i 1)) luft.terrain:+coarse-tile-bias+)
          for packed = (aref sites (+ i 2))
          do (assert (and (<= 768 x 892) (<= 1024 y 1148) (zerop (mod x 4))
                          (zerop (mod y 4)) (zerop (mod (ldb (byte 16 0) packed) 4))
                          (= 2 (ldb (byte 3 16) packed)))))
    stars))

(defun ridge-section (tile name)
  (let ((words (luft.terrain:coarse-voxel-tile-words tile)))
    (with-open-file (out (merge-pathnames name #p"build/stride-four-ridge/")
                         :direction :output :if-exists :supersede)
      (format out "P2~%32 32~%1~%")
      (loop for z from 31 downto 0 do
        (dotimes (x 32)
          (format out "~D " (if (logbitp z (aref words (+ 8 (* 32 x)))) 0 1)))
        (terpri out)))))

(defun compare-ridge (source suffix carved-p)
  (let* ((exact (exact-ridge source))
         (native (native-ridge source))
         (delta (map 'list (lambda (a b) (* 4 (abs (- (integer-length a) (integer-length b)))))
                     (luft.terrain:coarse-voxel-tile-words exact)
                     (luft.terrain:coarse-voxel-tile-words native))))
    (dolist (entry (list (cons "exact" exact) (cons "native" native)))
      (let ((rays (clear-bore-rays (cdr entry))))
        (format t "~&~A ~A: ~D/8 clear, ~D stars~%" (car entry) suffix rays (ridge-mesh (cdr entry)))
        (assert (if carved-p (= rays 8) (< rays 8)))
        (when carved-p
          ;; The edited ridge retains its cap one coarse cell above the bore.
          (assert (= 88 (* 4 (integer-length
                              (aref (luft.terrain:coarse-voxel-tile-words (cdr entry))
                                    (+ 8 (* 32 11))))))))
        (ridge-section (cdr entry) (format nil "~A-~A.pgm" (car entry) suffix))))
    (format t "~&~A column-top difference in world cells: mean ~,2F, max ~D, identical ~D/1024~%"
            suffix (/ (reduce #'+ delta) 1024) (reduce #'max delta) (count 0 delta))
    (let ((col (position (reduce #'max delta) delta)))
      (format t "Largest difference at (~D,~D): exact top face ~D, native ~D~%"
              (+ 768 (* 4 (floor col 32))) (+ 1024 (* 4 (mod col 32)))
              (* 4 (integer-length (aref (luft.terrain:coarse-voxel-tile-words exact) col)))
              (* 4 (integer-length (aref (luft.terrain:coarse-voxel-tile-words native) col)))))))

(defun median-build-ms (thunk)
  (let ((times (loop repeat 7 for start = (get-internal-real-time)
                     do (funcall thunk)
                     collect (/ (* 1000.0d0 (- (get-internal-real-time) start))
                                internal-time-units-per-second))))
    (fourth (sort times #'<))))

(ensure-directories-exist #p"build/stride-four-ridge/")
(let ((source (ridge-source))
      (luvcraft.luft::*luvcraft-generation-threads* 1))
  (compare-ridge source "before" nil)
  (carve-ridge source)
  (compare-ridge source "after" t)
  (let ((children (ridge-children source)))
    (format t "~&Median fresh tile CPU wall ms, 7 builds: exact ~,3F, parent alone ~,3F, native ~,3F~%"
            (median-build-ms (lambda () (exact-ridge source)))
            (median-build-ms (lambda () (luft.terrain:reduce-stride-two-children 6 8 children)))
            (median-build-ms (lambda () (native-ridge source))))))
