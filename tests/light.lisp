(in-package #:luv/luvcraft/tests)

;;; The reference relight is exercised on small hand-built worlds whose
;;; correct fields can be reasoned out cell by cell: open columns, roofs
;;; and shafts, emitters and their falloff, and both vertical and lateral
;;; chunk seams.

(defparameter *test-glow-block*
  (make-instance 'luv::block-kind
                 :name :test-glow :face-tiles '(:all 3)
                 :light-emission 10 :surface-emission 1.0))

(defparameter *test-dim-glow-block*
  (make-instance 'luv::block-kind
                 :name :test-dim-glow :face-tiles '(:all 3)
                 :light-emission 6))

(defun make-open-sky-test-world (&rest chunk-keys)
  "A world whose absent +Y boundary is open sky, with all-air chunks."
  (let ((world (make-block-world
                :source (make-instance 'little-world-source :seed 1))))
    (dolist (key (or chunk-keys '((0 0 0))))
      (apply #'luv::ensure-world-chunk world key))
    world))

(defun sky-at (world x y z)
  (values (world-light-at world x y z)))

(defun blocklight-at (world x y z)
  (nth-value 1 (world-light-at world x y z)))

(deftest open-columns-see-full-sky
  (let ((world (make-open-sky-test-world)))
    (relight-block-world world)
    (ok (= (sky-at world 8 15 8) 15))
    (ok (= (sky-at world 8 0 8) 15))
    (ok (= (sky-at world 0 0 0) 15))
    (ok (= (sky-at world 15 7 15) 15))
    ;; Lateral terrain is generatable but not resident, so the result is
    ;; honest but provisional.
    (ok (eq (nth-value 2 (world-light-at world 8 8 8)) :provisional))))

(deftest roofs-block-light-and-shafts-transmit-it
  (let ((world (make-open-sky-test-world)))
    (dotimes (x 16)
      (dotimes (z 16)
        (unless (and (= x 8) (= z 8))
          (setf (world-block-at world x 15 z) luv::*stone-block*))))
    (relight-block-world world)
    ;; Stone roof cells admit no sky; the open shaft carries a full beam
    ;; to the floor, and lateral spread attenuates one level per step.
    (ok (= (sky-at world 2 15 2) 0))
    (ok (= (sky-at world 8 15 8) 15))
    (ok (= (sky-at world 8 0 8) 15))
    (ok (= (sky-at world 7 14 8) 14))
    (ok (= (sky-at world 2 14 2) 3))
    (ok (= (sky-at world 0 0 0) 0))))

(deftest emitters-fall-off-and-the-brighter-source-wins
  (let ((world (make-open-sky-test-world)))
    (setf (world-block-at world 8 8 8) *test-glow-block*)
    (setf (world-block-at world 12 8 8) *test-dim-glow-block*)
    (relight-block-world world)
    (ok (= (blocklight-at world 8 8 8) 10))
    (ok (= (blocklight-at world 8 9 8) 9))
    (ok (= (blocklight-at world 8 8 11) 7))
    (ok (= (blocklight-at world 8 8 2) 4))
    ;; Sixteen Manhattan steps out, the level is exhausted.
    (ok (= (blocklight-at world 0 8 0) 0))
    ;; Between the two emitters each cell keeps the brighter contribution.
    (ok (= (blocklight-at world 10 8 8) 8))
    (ok (= (blocklight-at world 11 8 8) 7))
    ;; Emission does not perturb the sky field.
    (ok (= (sky-at world 8 9 8) 15))))

(deftest walls-stop-blocklight
  (let ((world (make-open-sky-test-world)))
    (setf (world-block-at world 4 8 8) *test-glow-block*)
    ;; A full stone shell one step out along +X.
    (dotimes (y 16)
      (dotimes (z 16)
        (setf (world-block-at world 6 y z) luv::*stone-block*)))
    (relight-block-world world)
    (ok (= (blocklight-at world 5 8 8) 9))
    (ok (= (blocklight-at world 6 8 8) 0))
    (ok (= (blocklight-at world 7 8 8) 0))))

(deftest sky-crosses-a-vertical-chunk-seam
  (let ((world (make-open-sky-test-world '(0 0 0) '(0 1 0))))
    ;; Roof the upper chunk's top layer, world y = 31, with one shaft.
    (dotimes (x 16)
      (dotimes (z 16)
        (unless (and (= x 4) (= z 4))
          (setf (world-block-at world x 31 z) luv::*stone-block*))))
    (relight-block-world world)
    (ok (= (sky-at world 4 31 4) 15))
    ;; The beam crosses the seam at world y = 15/16 undiminished.
    (ok (= (sky-at world 4 16 4) 15))
    (ok (= (sky-at world 4 15 4) 15))
    (ok (= (sky-at world 4 0 4) 15))
    ;; Lateral falloff below the seam still measures from the shaft.
    (ok (= (sky-at world 0 5 0) 7))
    (ok (= (sky-at world 15 5 15) 0))))

(deftest sky-crosses-a-lateral-chunk-seam
  (let ((world (make-open-sky-test-world '(0 0 0) '(1 0 0))))
    ;; Roof the +X chunk completely; its light must arrive sideways from
    ;; the open chunk across the seam.
    (dotimes (x 16)
      (dotimes (z 16)
        (setf (world-block-at world (+ 16 x) 15 z) luv::*stone-block*)))
    (relight-block-world world)
    (ok (= (sky-at world 15 14 8) 15))
    (ok (= (sky-at world 16 14 8) 14))
    (ok (= (sky-at world 20 14 8) 10))
    (ok (= (sky-at world 31 14 8) 0))))

(deftest relighting-is-a-derived-domain-with-its-own-revisions
  (let ((world (make-open-sky-test-world)))
    (let* ((chunk (luv::world-chunk-at world 0 0 0))
           (content-revision (luv::block-chunk-revision chunk))
           (world-revision (block-world-revision world))
           (changed (relight-block-world world))
           (field (block-chunk-light-field chunk)))
      (ok (= (length changed) 1))
      (ok (= (chunk-light-field-revision field) 1))
      ;; Light publication does not impersonate an authored edit.
      (ok (= (luv::block-chunk-revision chunk) content-revision))
      (ok (= (block-world-revision world) world-revision))
      ;; A second solve over unchanged content publishes nothing.
      (ok (null (relight-block-world world)))
      (ok (= (chunk-light-field-revision field) 1))
      ;; A content edit then changes the field and only the light revision
      ;; and changed light boundaries advance.
      (let ((top-before
              (chunk-light-field-boundary-revision field 0 1 0))
            (bottom-before
              (chunk-light-field-boundary-revision field 0 -1 0)))
        (setf (world-block-at world 8 15 8) luv::*stone-block*)
        (ok (relight-block-world world))
        (ok (= (chunk-light-field-revision field) 2))
        (ok (= (chunk-light-field-boundary-revision field 0 1 0)
               (1+ top-before)))
        (ok (= (chunk-light-field-boundary-revision field 0 -1 0)
               (1+ bottom-before)))))))

(deftest absent-neighbors-are-never-silently-open-sky
  ;; A world with no source keeps every boundary :UNKNOWN, so nothing is
  ;; lit and the result says so instead of inventing daylight.
  (let ((world (make-block-world)))
    (luv::ensure-world-chunk world 0 0 0)
    (relight-block-world world)
    (multiple-value-bind (sky block state) (world-light-at world 8 8 8)
      (ok (= sky 0))
      (ok (= block 0))
      (ok (eq state :provisional)))))
