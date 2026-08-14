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

;;; The incremental relighter is judged against the reference solver: after
;;; its queues settle, every resident cell must be bit-identical to a
;;; from-scratch solve of the same world.

(defun light-matches-reference-p (world)
  (let ((reference (luv::solve-light-region
                    (luv::capture-light-region world))))
    (loop for chunk in (resident-world-chunks world)
          always
          (let* ((key (luv::block-chunk-key chunk))
                 (entry (gethash key (luv::light-region-entries reference)))
                 (field (block-chunk-light-field chunk)))
            (and field
                 (equalp (luv::light-region-entry-sky entry)
                         (chunk-light-field-sky-levels field))
                 (equalp (luv::light-region-entry-block entry)
                         (chunk-light-field-block-levels field)))))))

(deftest incremental-edits-converge-to-the-reference-field
  (let* ((world (make-block-world
                 :source (make-instance 'little-world-source :seed 1)))
         (state (luv::attach-lighting-state world)))
    ;; Arrival through the hook lights the fresh chunk incrementally.
    (luv::ensure-world-chunk world 0 0 0)
    (ok (luv::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    ;; Roofing one cell darkens its column; removing it restores the beam.
    (setf (world-block-at world 8 15 8) luv::*stone-block*)
    (ok (luv::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    (ok (= (sky-at world 8 14 8) 14))
    (setf (world-block-at world 8 15 8) nil)
    (ok (luv::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    (ok (= (sky-at world 8 0 8) 15))
    ;; An emitter appears and disappears.
    (setf (world-block-at world 4 4 4) *test-glow-block*)
    (ok (luv::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    (ok (= (blocklight-at world 4 5 4) 9))
    (setf (world-block-at world 4 4 4) nil)
    (ok (luv::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    (ok (= (blocklight-at world 4 5 4) 0))
    ;; A settled state publishes nothing further.
    (ok (null (luv::reconcile-lighting state)))
    (ok (plusp (luv::lighting-state-publications state)))
    (ok (plusp (luv::lighting-state-cells-visited state)))))

(deftest random-edits-and-residency-match-the-reference-solver
  (let* ((world (make-block-world
                 :source (make-instance 'little-world-source :seed 1)))
         (state (luv::attach-lighting-state world))
         (rng 987654321)
         (chunk-keys '((0 0 0) (1 0 0) (0 1 0))))
    (labels ((next-random (limit)
               (setf rng (mod (+ (* rng 1103515245) 12345) (expt 2 31)))
               (mod (floor rng 65536) limit))
             (random-block ()
               (case (next-random 8)
                 ((0 1 2) luv::*stone-block*)
                 (3 *test-glow-block*)
                 (t nil)))
             (random-resident-cell ()
               (let* ((keys (mapcar #'luv::block-chunk-key
                                    (resident-world-chunks world)))
                      (key (nth (next-random (length keys)) keys)))
                 (list (+ (* 16 (first key)) (next-random 16))
                       (+ (* 16 (second key)) (next-random 16))
                       (+ (* 16 (third key)) (next-random 16))))))
      (dolist (key chunk-keys)
        (apply #'luv::ensure-world-chunk world key))
      ;; Random terrain, then interleaved edit bursts and reconciles.
      (dotimes (index 300)
        (destructuring-bind (x y z) (random-resident-cell)
          (setf (world-block-at world x y z) (random-block))))
      (luv::reconcile-lighting state)
      (ok (light-matches-reference-p world))
      (dotimes (round 6)
        (dotimes (edit 10)
          (destructuring-bind (x y z) (random-resident-cell)
            (setf (world-block-at world x y z) (random-block))))
        (luv::reconcile-lighting state)
        (ok (light-matches-reference-p world)))
      ;; A departure relights the retained neighbors; a re-arrival with
      ;; fresh edits converges again.
      (luv::remove-world-chunk world 1 0 0)
      (luv::reconcile-lighting state)
      (ok (light-matches-reference-p world))
      (luv::ensure-world-chunk world 1 0 0)
      (dotimes (edit 12)
        (setf (world-block-at world
                              (+ 16 (next-random 16))
                              (next-random 16)
                              (next-random 16))
              (random-block)))
      (luv::reconcile-lighting state)
      (ok (light-matches-reference-p world)))))

(deftest same-key-replacement-removes-the-old-chunk-light
  (let* ((world (make-open-sky-test-world '(0 0 0) '(1 0 0)))
         (state (luv::attach-lighting-state world)))
    (setf (world-block-at world 16 8 8) *test-glow-block*)
    (luv::reconcile-lighting state)
    (ok (= (blocklight-at world 15 8 8) 9))
    ;; Streaming can replace a chunk at the same key before the next lighting
    ;; reconcile.  The departure still has to run, or retained neighbors keep
    ;; light propagated from the old incarnation.
    (luv::remove-world-chunk world 1 0 0)
    (luv::ensure-world-chunk world 1 0 0)
    (luv::reconcile-lighting state)
    (ok (light-matches-reference-p world))
    (ok (= (blocklight-at world 15 8 8) 0))))

(deftest meshes-carry-raw-corner-light-and-material-emission
  (let* ((world (make-open-sky-test-world))
         (state (luv::attach-lighting-state world))
         (mesher (make-instance 'exposed-face-mesher)))
    ;; A solid floor with a glowing block resting on it, under open sky.
    (dotimes (x 16)
      (dotimes (z 16)
        (setf (world-block-at world x 0 z) luv::*stone-block*)))
    (setf (world-block-at world 8 1 8) *test-glow-block*)
    (luv::reconcile-lighting state)
    (let* ((chunk (luv::world-chunk-at world 0 0 0))
           (mesh (mesh-block-chunk mesher world chunk))
           (vertices (block-mesh-vertices mesh))
           (stride luv::+block-mesh-floats-per-vertex+)
           (sky-readings nil)
           (block-readings nil)
           (emissive-vertices 0))
      (loop for base from 0 below (length vertices) by stride
            do (push (aref vertices (+ base 9)) sky-readings)
               (push (aref vertices (+ base 10)) block-readings)
               (when (plusp (aref vertices (+ base 11)))
                 (incf emissive-vertices)))
      ;; Floor tops under open sky read full skylight; the glow block's own
      ;; faces carry its surface emission, and its blocklight reaches the
      ;; floor around it.
      (ok (find 1.0 sky-readings))
      (ok (plusp (reduce #'max block-readings)))
      ;; Five exposed faces of the resting glow block, six vertices each.
      (ok (= emissive-vertices 30))
      ;; The immutable snapshot meshes bit-identically to the owner side.
      (let* ((snapshot (luv::make-block-mesh-snapshot
                        world chunk
                        (luv::chunk-mesh-dependency-stamp world chunk)))
             (snapshot-mesh (mesh-block-snapshot mesher snapshot)))
        (ok (equalp (block-mesh-vertices mesh)
                    (block-mesh-vertices snapshot-mesh)))))))

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
