(in-package #:luft)

(defvar *luft-test-count* 0)

(defmacro %check (form)
  `(progn
     (incf *luft-test-count*)
     (unless ,form (error "LUFT check failed: ~S" ',form))))

(defun %one-cell-chain ()
  (let* ((domain (make-world-domain :horizontal-bits 4))
         (builder (make-chain-builder domain)))
    (chain-builder-add-site builder (make-site domain 4 4 4 +cell-extent+ 1))
    (finish-chain-builder builder)))

(defun %test-one-cell-stars ()
  (let* ((chain (%one-cell-chain))
         (mesh
           (handler-bind
               ((missing-chunk
                  (lambda (condition)
                    (declare (ignore condition))
                    (invoke-restart 'treat-as-air))))
             (mesh-star-chunk chain (chunk-key-at 4 4)
                              :outside-domain-policy :air)))
         (words (surface-mesh-star-site-words mesh)))
    (%check (= 32 (length words)))
    (loop for offset from 3 below (length words) by 4
          for star = (aref words offset)
          do (%check (plusp star))
             (%check (zerop (logand star (1- star)))))
    (%check (= 44 (surface-mesh-triangle-count mesh)))))

(defun %test-word-parallel-star-selection ()
  "Compare dense selection with the small atlas-oriented occupancy oracle."
  (let* ((domain (make-world-domain :horizontal-bits 6))
         (builder (make-chain-builder domain))
         (occupied (make-hash-table :test #'equal)))
    (flet ((occupy (x y z)
             (let ((cell (list x y z)))
               (unless (gethash cell occupied)
                 (setf (gethash cell occupied) t)
                 (chain-builder-add-site
                  builder (make-site domain x y z +cell-extent+ 1))))))
      ;; Exercise every horizontal edge, both vertical extremes, and enough
      ;; deterministic interior configurations to cross all four fiber words.
      (dolist (cell '((0 0 0) (63 0 1) (0 63 253) (63 63 254)
                       (31 31 63) (31 31 64) (32 32 127) (32 32 128)))
        (apply #'occupy cell))
      (let ((state #x31415926))
        (dotimes (i 800)
          (declare (ignore i))
          (setf state (ldb (byte 32 0) (+ (* state 1664525) 1013904223)))
          (let ((x (ldb (byte 6 0) state))
                (y (ldb (byte 6 6) state))
                (z (mod (ash state -12) +top-z+)))
            (occupy x y z)))))
    (let* ((chain (finish-chain-builder builder))
           (mesh (mesh-star-chunk chain 0 :outside-domain-policy :air))
           (words (surface-mesh-star-site-words mesh))
           (expected (star-surface-sites occupied))
           (expected-count
             (loop for star being the hash-values of expected
                   count (and (plusp star) (/= star #xff)))))
      (%check (= expected-count (/ (length words) 4)))
      (loop for offset from 0 below (length words) by 4
            for coordinate = (list (aref words offset)
                                   (aref words (+ offset 1))
                                   (aref words (+ offset 2)))
            do (%check (= (aref words (+ offset 3))
                          (gethash coordinate expected 0))))
      #+x86-64
      (when (luft.avx512::available-p)
        (let* ((*star-selection-instruction-set-override* :avx512)
               (avx512-mesh
                 (mesh-star-chunk chain 0 :outside-domain-policy :air)))
          (%check
           (equalp words
                   (surface-mesh-star-site-words avx512-mesh))))))))

(defun %test-star-selection-instruction-kernel ()
  "Keep machine-specific spelling subordinate to the scalar Boolean law."
  (let ((below (make-array 20 :element-type '(unsigned-byte 64)))
        (above (make-array 20 :element-type '(unsigned-byte 64)))
        (scalar (make-array 4 :element-type '(unsigned-byte 64)))
        (second (make-array 4 :element-type '(unsigned-byte 64)))
        (native (make-array 8 :element-type '(unsigned-byte 64)))
        (state #x9e3779b97f4a7c15))
    (dotimes (index 20)
      (setf state (ldb (byte 64 0) (+ (* state 6364136223846793005) 1))
            (aref below index) state
            state (ldb (byte 64 0) (+ (* state 6364136223846793005) 1))
            (aref above index) state))
    (%star-active-words-scalar below above 0 4 8 12 scalar 0)
    (%star-active-words-scalar below above 4 8 12 16 second 0)
    (if (eq (star-selection-instruction-set) :avx512)
        (progn
          (%star-active-words-avx512 below above 0 4 8 12 native 0)
          (%check (equalp scalar (subseq native 0 4)))
          (%check (equalp second (subseq native 4 8))))
        (progn
          (funcall (%star-active-kernel)
                   below above 0 4 8 12 native 0)
          (%check (equalp scalar (subseq native 0 4)))))
    #+x86-64
    (when (luft.avx512::available-p)
      (fill native 0)
      (%star-active-words-avx512 below above 0 4 8 12 native 0)
      (%check (equalp scalar (subseq native 0 4)))
      (%check (equalp second (subseq native 4 8))))))

(defun %test-atlas ()
  (%check (= 256 (length *star-atlas-owned-triangles*)))
  (dotimes (star 256)
    (%check (equal (star-atlas-owned-triangles star)
                   (svref *star-atlas-owned-triangles* star))))
  (%check (null (star-atlas-owned-triangles 0)))
  (%check (null (star-atlas-owned-triangles #xff))))

(defun %test-cubical-addressing ()
  (dotimes (star 256)
    (multiple-value-bind (representative transformation complemented-p)
        (star-canonical-form star :reflections t)
      (declare (ignore complemented-p))
      (%check (= star (transform-star transformation representative))))))

(defun run-luft-tests (&key (stream *standard-output*))
  (setf *luft-test-count* 0)
  (%test-one-cell-stars)
  (%test-word-parallel-star-selection)
  (%test-star-selection-instruction-kernel)
  (%test-atlas)
  (%test-cubical-addressing)
  (format stream "LUFT: ~D star checks passed.~%" *luft-test-count*)
  (values t *luft-test-count*))
