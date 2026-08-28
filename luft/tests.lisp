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
    (%check
     (equalp words
             #(4 4 4 128 4 4 5 8 5 4 4 64 5 4 5 4
               4 5 4 32 4 5 5 2 5 5 4 16 5 5 5 1)))
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

(defun %same-triangle-p (first second)
  "Whether FIRST and SECOND are the same unoriented geometric triangle."
  (and (subsetp first second :test #'equal)
       (subsetp second first :test #'equal)))

(defun %triangle-stabilizer (star triangle transformations)
  "The cubical symmetries fixing STAR and TRIANGLE as a primitive."
  (loop for transformation in transformations
        when (and (= star (transform-star transformation star))
                  (%same-triangle-p
                   triangle
                   (first (transform-star-triangles
                           transformation (list triangle)))))
          collect transformation))

(defun %sample-fixed-by-transformations-p (sample transformations)
  (every (lambda (transformation)
           (= sample (%transform-star-sample transformation sample)))
         transformations))

(defun %stable-star-samples (star occupied-p stabilizer)
  "Samples of the requested occupancy fixed by all of STABILIZER."
  (loop for sample below 8
        when (and (eq occupied-p (logbitp sample star))
                  (%sample-fixed-by-transformations-p sample stabilizer))
          collect sample))

(defun %test-atlas-appearance-selector-hypothesis ()
  "Disprove one symmetry-stable occupied/empty selector per triangle.

A selector equivariant under cubical symmetry must be fixed by every symmetry
that fixes both its star and geometric triangle.  We exhaust all emitted
triangles and all 48 signed-axis symmetries."
  (let ((transformations (append (star-rotations) (star-reflections)))
        (triangle-count 0)
        (obstructions nil))
    (dotimes (star 256)
      (dolist (triangle (star-atlas-owned-triangles star))
        (incf triangle-count)
        (let* ((stabilizer
                 (%triangle-stabilizer star triangle transformations))
               (occupied (%stable-star-samples star t stabilizer))
               (empty (%stable-star-samples star nil stabilizer)))
          (unless (and occupied empty)
            (push (list star triangle occupied empty) obstructions)))))
    ;; Preserve the complete per-star triangle census while recording that the
    ;; proposed selector contract fails on 48 emitted triangle primitives.
    (%check (= 4200 triangle-count))
    (%check (= 48 (length obstructions)))
    ;; #x81's positive junction triangle is fixed by cyclic axis permutation.
    ;; Its two occupied samples are fixed, but all six empty samples move, so
    ;; no symmetry-equivariant single empty selector exists.  Since this holds
    ;; for every empty sample, adding an "outward" restriction cannot help.
    (let* ((star #x81)
           (triangle '((0 1 1) (1 1 0) (1 0 1)))
           (cycle '((0 1 0) (0 0 1) (1 0 0))))
      (%check (= star (transform-star cycle star)))
      (%check (%same-triangle-p
               triangle
               (first (transform-star-triangles cycle (list triangle)))))
      (%check (every (lambda (sample)
                       (or (logbitp sample star)
                           (/= sample (%transform-star-sample cycle sample))))
                     '(0 1 2 3 4 5 6 7)))
      (%check (find (list star triangle '(0 7) nil) obstructions
                    :test #'equal)))))

(defun %transform-sample-mask (transformation mask)
  (loop for sample below 8
        when (logbitp sample mask)
          sum (ash 1 (%transform-star-sample transformation sample))))

(defun %mean-masked-values (values mask)
  (let ((sum 0)
        (count 0))
    (dotimes (sample 8 (/ sum count))
      (when (logbitp sample mask)
        (incf sum (aref values sample))
        (incf count)))))

(defun %maximum-masked-values (values mask)
  (loop for sample below 8
        when (logbitp sample mask)
          maximize (aref values sample)))

(defun %transform-sample-values (transformation values)
  (let ((transformed (make-array 8)))
    (dotimes (sample 8 transformed)
      (setf (aref transformed (%transform-star-sample transformation sample))
            (aref values sample)))))

(defun %check-appearance-mask-equivariance
    (star transformed-star transformation source-function target-function)
  (multiple-value-bind (material light) (funcall source-function star)
    (multiple-value-bind (transformed-material transformed-light)
        (funcall target-function transformed-star)
      (%check (= (%transform-sample-mask transformation material)
                 transformed-material))
      (%check (= (%transform-sample-mask transformation light)
                 transformed-light))
      ;; Material is the equal-weight mean over the solid set; illumination is
      ;; the componentwise maximum over the air set.  Scalar lanes suffice to
      ;; prove each component because both policies act componentwise.
      (let* ((material-values #(3 5 11 17 23 29 37 41))
             (light-values #(1 8 2 7 3 6 4 5))
             (transformed-material-values
               (%transform-sample-values transformation material-values))
             (transformed-light-values
               (%transform-sample-values transformation light-values)))
        (%check (= (%mean-masked-values material-values material)
                   (%mean-masked-values transformed-material-values
                                        transformed-material)))
        (%check (= (%maximum-masked-values light-values light)
                   (%maximum-masked-values transformed-light-values
                                           transformed-light)))))))

(defun %test-atlas-appearance-sample-set-policy ()
  "Prove face/band/junction sets and commutative reductions over all 48 symmetries."
  (let ((transformations (append (star-rotations) (star-reflections))))
    (dotimes (star 256)
      (%check (= (length (star-atlas-owned-triangles star))
                 (length (star-atlas-owned-appearance-masks star))))
      (let ((parts (star-atlas-parts star)))
        (dolist (transformation transformations)
          (let ((transformed-star (transform-star transformation star)))
            (dolist (face (remove-if (lambda (part) (null (second part)))
                                     (getf parts :faces)))
              (let* ((pair (first face))
                     (transformed-pair
                       (%transform-face-pair transformation pair)))
                (%check-appearance-mask-equivariance
                 star transformed-star transformation
                 (lambda (source-star)
                   (%face-appearance-sample-masks source-star pair))
                 (lambda (target-star)
                   (%face-appearance-sample-masks
                    target-star transformed-pair)))))
            (dolist (band (remove-if (lambda (part) (null (second part)))
                                     (getf parts :bands)))
              (let* ((key (first band))
                     (transformed-key
                       (%transform-band-direction transformation key)))
                (%check-appearance-mask-equivariance
                 star transformed-star transformation
                 (lambda (source-star)
                   (%band-appearance-sample-masks source-star key))
                 (lambda (target-star)
                   (%band-appearance-sample-masks
                    target-star transformed-key)))))
            (when (getf parts :junction)
              (%check-appearance-mask-equivariance
               star transformed-star transformation
               #'%junction-appearance-sample-masks
               #'%junction-appearance-sample-masks))))))))

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
  (%test-atlas-appearance-selector-hypothesis)
  (%test-atlas-appearance-sample-set-policy)
  (%test-cubical-addressing)
  (format stream "LUFT: ~D star checks passed.~%" *luft-test-count*)
  (values t *luft-test-count*))
