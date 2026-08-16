(in-package #:luvcraft.tests)

;;; The reference relight is exercised on small hand-built worlds whose
;;; correct fields can be reasoned out cell by cell: open columns, roofs
;;; and shafts, emitters and their falloff, and both vertical and lateral
;;; chunk seams.

(defparameter *test-glow-block*
  (make-instance 'luvcraft::block-kind
                 :name :test-glow :face-tiles '(:all 3)
                 :light-emission 10 :surface-emission 1.0))

(defparameter *test-dim-glow-block*
  (make-instance 'luvcraft::block-kind
                 :name :test-dim-glow :face-tiles '(:all 3)
                 :light-emission 6))

(defvar *light-region-window-lookups* nil)

(defmethod locate-chunk-window-site :around
    ((region luvcraft::light-region) x y z)
  (when *light-region-window-lookups*
    (incf *light-region-window-lookups*))
  (call-next-method))

(defun make-open-sky-test-world (&rest chunk-keys)
  "A world whose absent +Y boundary is open sky, with all-air chunks."
  (let ((world (make-block-world
                :source (make-instance 'little-world-source :seed 1))))
    (dolist (key (or chunk-keys '((0 0 0))))
      (apply #'luvcraft::ensure-world-chunk world key))
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
          (setf (world-block-at world x 15 z) luvcraft::*stone-block*))))
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
        (setf (world-block-at world 6 y z) luvcraft::*stone-block*)))
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
          (setf (world-block-at world x 31 z) luvcraft::*stone-block*))))
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
        (setf (world-block-at world (+ 16 x) 15 z) luvcraft::*stone-block*)))
    (relight-block-world world)
    (ok (= (sky-at world 15 14 8) 15))
    (ok (= (sky-at world 16 14 8) 14))
    (ok (= (sky-at world 20 14 8) 10))
    (ok (= (sky-at world 31 14 8) 0))))

(deftest relighting-is-a-derived-domain-with-its-own-revisions
  (let ((world (make-open-sky-test-world)))
    (let* ((chunk (luvcraft::world-chunk-at world 0 0 0))
           (content-revision (luvcraft::block-chunk-revision chunk))
           (world-revision (block-world-revision world))
           (changed (relight-block-world world))
           (field (block-chunk-light-field chunk)))
      (ok (= (length changed) 1))
      (ok (= (chunk-light-field-revision field) 1))
      ;; Light publication does not impersonate an authored edit.
      (ok (= (luvcraft::block-chunk-revision chunk) content-revision))
      (ok (= (block-world-revision world) world-revision))
      ;; A second solve over unchanged content publishes nothing.
      (ok (null (relight-block-world world)))
      (ok (= (chunk-light-field-revision field) 1))
      ;; A content edit then changes the field and only the light revision
      ;; and changed light boundaries advance.
      (let ((top-before
              (chunk-light-field-boundary-revision field +voxel-positive-y+))
            (bottom-before
              (chunk-light-field-boundary-revision field +voxel-negative-y+)))
        (setf (world-block-at world 8 15 8) luvcraft::*stone-block*)
        (ok (relight-block-world world))
        (ok (= (chunk-light-field-revision field) 2))
        (ok (= (chunk-light-field-boundary-revision field +voxel-positive-y+)
               (1+ top-before)))
        (ok (= (chunk-light-field-boundary-revision field +voxel-negative-y+)
               (1+ bottom-before)))))))

;;; The incremental relighter is judged against the reference solver: after
;;; its queues settle, every resident cell must be bit-identical to a
;;; from-scratch solve of the same world.

(defun light-matches-reference-p (world)
  (let ((reference (luvcraft::solve-light-region
                    (luvcraft::capture-light-region world))))
    (loop for chunk in (resident-world-chunks world)
          always
          (let* ((key (chunk-domain-coordinate (block-chunk-domain chunk)))
                 (entry (gethash key (luvcraft::light-region-entries reference)))
                 (field (block-chunk-light-field chunk)))
            (and field
                 (equalp (luvcraft::light-region-entry-sky entry)
                         (chunk-light-field-sky-levels field))
                 (equalp (luvcraft::light-region-entry-block entry)
                         (chunk-light-field-block-levels field)))))))

(deftest frontier-light-is-inspectable-and-exactly-matches-the-oracle
  (let ((definition
          (luvcraft.frontier:frontier-program-definition-for
           'luvcraft::voxel-light-addition)))
    (ok (typep definition 'luvcraft.frontier:frontier-program-definition))
    (ok (eq :monotone-max-fixpoint
            (luvcraft.frontier:frontier-program-definition-family
             definition)))
    (ok (eq :brightest-first-buckets
            (luvcraft.frontier:frontier-program-definition-frontier-layout
             definition)))
    (ok (eq :voxel-face-relations
            (luvcraft.frontier:frontier-program-definition-neighborhood
             definition))))
  (let ((world
          (make-open-sky-test-world
           '(0 0 0) '(0 1 0) '(1 0 0))))
    ;; Give the comparison vertical and lateral crossings, occlusion, two
    ;; competing emitters, and a direct sky shaft in one small world.
    (dotimes (x 32)
      (dotimes (z 16)
        (unless (and (= x 4) (= z 4))
          (setf (world-block-at world x 15 z) luvcraft::*stone-block*))))
    (setf (world-block-at world 15 8 8) *test-glow-block*
          (world-block-at world 18 8 8) *test-dim-glow-block*)
    (let ((comparison (compare-voxel-light-solvers world)))
      (ok (voxel-light-solver-comparison-equal-p comparison))
      (ok (null
           (voxel-light-solver-comparison-mismatched-keys comparison)))
      (ok (= (voxel-light-solver-comparison-legacy-visits comparison)
             (voxel-light-solver-comparison-frontier-visits comparison)))
      (ok (plusp
           (voxel-light-solver-comparison-legacy-bytes-consed comparison)))
      (ok (plusp
           (voxel-light-solver-comparison-frontier-bytes-consed comparison)))
      (ok (>= (voxel-light-solver-comparison-legacy-gc-seconds comparison)
              0d0))
      (ok (>= (voxel-light-solver-comparison-frontier-gc-seconds comparison)
              0d0))
      (ok (= 2 (length
                (voxel-light-solver-comparison-frontier-executions
                 comparison))))
      (dolist (execution
               (voxel-light-solver-comparison-frontier-executions comparison))
        (ok (plusp
             (luvcraft.frontier:frontier-execution-visits execution)))
        (ok (= (* 6
                  (luvcraft.frontier:frontier-execution-visits execution))
               (luvcraft.frontier:frontier-execution-relations execution)))
        (ok (plusp
             (luvcraft.frontier:frontier-execution-crossings execution)))))))

(defun make-compiled-light-proof-world ()
  "Three chunks with vertical and lateral seams, occlusion, a shaft, emitters."
  (let ((world (make-open-sky-test-world '(0 0 0) '(0 1 0) '(1 0 0))))
    (dotimes (x 32)
      (dotimes (z 16)
        (unless (and (= x 4) (= z 4))
          (setf (world-block-at world x 15 z) luvcraft::*stone-block*))))
    (setf (world-block-at world 15 8 8) *test-glow-block*
          (world-block-at world 18 8 8) *test-dim-glow-block*)
    world))

(deftest compiled-light-kernel-is-inspectable-and-matches-both-oracles
  ;; The program states its law; the realization retains the checked
  ;; expressions, the emitted forms, and the compiled functions.
  (let* ((definition
           (luvcraft.frontier:frontier-program-definition-for
            'luvcraft::voxel-light-addition))
         (realization (luvcraft::compiled-light-realization :sky-light)))
    (ok (luvcraft.frontier:frontier-program-definition-transfer definition))
    (ok (equal '("LEVEL" "OPACITY")
               (mapcar (lambda (role)
                         (symbol-name
                          (luvcraft.frontier:frontier-field-role-name role)))
                       (luvcraft.frontier:frontier-program-definition-fields
                        definition))))
    (ok (luvcraft.frontier:frontier-realization-current-p realization))
    (ok (eq 'lambda
            (first (luvcraft.frontier:frontier-realization-drain-form
                    realization))))
    (ok (functionp
         (luvcraft.frontier:frontier-realization-drain-function realization)))
    (ok (functionp
         (luvcraft.frontier:frontier-realization-admit-function realization)))
    (ok (= 15 (luvcraft.frontier:frontier-realization-maximum-priority
               realization)))
    ;; The transfer law is checked in the bound field's own quantity: sky
    ;; light stays sky light, and attenuation steps enter through an explicit
    ;; AS-FIELD-QUANTITY boundary rather than by coincidence of encoding.
    (ok (eq :sky-propagation-level
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic.language:arithmetic-expression-quantity-specification
              (luvcraft.frontier:frontier-realization-transfer realization)))))
    (ok (eq :block-propagation-level
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic.language:arithmetic-expression-quantity-specification
              (luvcraft.frontier:frontier-realization-transfer
               (luvcraft::compiled-light-realization :block-light))))))
    ;; The emitted scalar loop contains no arithmetic dispatch: the law was
    ;; inlined as ordinary CL operators over declared lanes.
    (ok (not (labels ((mentions-p (tree)
                        (if (atom tree)
                            (member tree '(luv.arithmetic.lisp::lisp-add
                                           luv.arithmetic.lisp::lisp-subtract
                                           funcall apply))
                            (or (mentions-p (car tree))
                                (mentions-p (cdr tree))))))
               (mentions-p
                (luvcraft.frontier:frontier-realization-drain-form
                 realization))))))
  (let ((world (make-compiled-light-proof-world)))
    ;; Warm the realizations, then compare exactly against both oracles.
    (compare-voxel-light-solvers world :candidate :compiled)
    (let ((legacy (compare-voxel-light-solvers world :candidate :compiled))
          (frontier (compare-voxel-light-solvers world :candidate :frontier)))
      (ok (voxel-light-solver-comparison-equal-p legacy))
      (ok (= (voxel-light-solver-comparison-legacy-visits legacy)
             (voxel-light-solver-comparison-frontier-visits legacy)
             (voxel-light-solver-comparison-frontier-visits frontier)))
      (ok (eq :compiled (voxel-light-solver-comparison-candidate-solver legacy)))
      (ok (= 2 (length
                (voxel-light-solver-comparison-frontier-executions legacy))))
      ;; Both realizations of the same definition perform the same semantic
      ;; work: visits, relations, and crossings agree execution by execution.
      (loop for compiled in (voxel-light-solver-comparison-frontier-executions
                             legacy)
            for manual in (voxel-light-solver-comparison-frontier-executions
                           frontier)
            do (ok (= (luvcraft.frontier:frontier-execution-visits compiled)
                      (luvcraft.frontier:frontier-execution-visits manual)))
               (ok (= (luvcraft.frontier:frontier-execution-relations compiled)
                      (luvcraft.frontier:frontier-execution-relations manual)))
               (ok (= (luvcraft.frontier:frontier-execution-crossings compiled)
                      (luvcraft.frontier:frontier-execution-crossings manual)))))))

(deftest compiled-light-seeds-are-boundary-transfers
  ;; Open sky is a virtual source at full brightness related inward through
  ;; the program's own transfer law: straight down pays only opacity, a
  ;; lateral boundary pays one step more, and a fully opaque cell admits
  ;; nothing.  The realization exposes this as its RELATE entry.
  (let* ((world (make-open-sky-test-world '(0 0 0)))
         (sky (luvcraft::compiled-light-realization :sky-light)))
    (setf (world-block-at world 3 15 3) luvcraft::*stone-block*)
    (let* ((region (luvcraft::capture-light-region world))
           (frontier (luvcraft.frontier:make-realization-frontier sky))
           (execution
             (luvcraft.frontier:make-realization-execution sky region frontier))
           (entry (gethash (make-chunk-coordinate 0 0 0)
                           (luvcraft::light-region-entries region)))
           (domain (luvcraft::light-region-entry-domain entry))
           (levels (luvcraft::light-region-entry-sky entry)))
      (ok (functionp (luvcraft.frontier:frontier-realization-relate-function sky)))
      (flet ((relate (x y z direction)
               (luvcraft.frontier:relate-frontier-realization-site
                sky region frontier execution entry
                (chunk-domain-offset-components domain x y z) direction
                :direct-direction luvcraft.world:+voxel-negative-y+
                :level luvcraft::+maximum-light-level+)))
        (ok (relate 0 15 0 luvcraft.world:+voxel-negative-y+))
        (ok (= 15 (aref levels (chunk-domain-offset-components domain 0 15 0))))
        (ok (relate 15 15 0 luvcraft.world:+voxel-negative-x+))
        (ok (= 14 (aref levels (chunk-domain-offset-components domain 15 15 0))))
        (ok (not (relate 3 15 3 luvcraft.world:+voxel-negative-y+)))
        (ok (= 0 (aref levels (chunk-domain-offset-components domain 3 15 3))))
        ;; A second, dimmer relation into the same site is not an improvement.
        (ok (not (relate 0 15 0 luvcraft.world:+voxel-negative-x+))))
      (ok (= 2 (luvcraft.frontier:bucket-frontier-count frontier)))
      (ok (= 2 (luvcraft.frontier:frontier-execution-admissions execution))))))

(deftest compiled-light-drain-allocates-nothing-per-relation
  ;; A warmed drain over pre-grown frontier storage allocates only at chunk
  ;; crossings, where the window resolves a coordinate key (#L84JCX), never
  ;; per site or per relation.  The proof world exposes about 68,000
  ;; relations and 4,000 crossings; one cons per relation would exceed a
  ;; megabyte.
  (let* ((world (make-compiled-light-proof-world))
         (sky (luvcraft::compiled-light-realization :sky-light))
         (frontier (luvcraft.frontier:make-realization-frontier sky))
         (bytes nil)
         (relations 0)
         (crossings 0))
    (dotimes (round 3)
      (let* ((region (luvcraft::capture-light-region world))
             (execution
               (luvcraft.frontier:make-realization-execution sky region frontier)))
        (luvcraft::seed-compiled-sky-boundaries sky region frontier execution)
        (let ((observation (make-runtime-observation)))
          (with-runtime-observation (observation)
            (luvcraft.frontier:drain-frontier-realization
             sky region frontier execution
             :direct-direction luvcraft.world:+voxel-negative-y+))
          (setf bytes (runtime-observation-bytes-consed observation)
                relations (luvcraft.frontier:frontier-execution-relations
                           execution)
                crossings (luvcraft.frontier:frontier-execution-crossings
                           execution)))))
    (ok (> relations 60000))
    (ok (< bytes (+ (* 64 1024) (* 128 crossings))))))

(deftest compiled-light-can-be-selected-for-real-publication
  (let ((world (make-open-sky-test-world '(0 0 0) '(1 0 0))))
    (setf (world-block-at world 15 8 8) *test-glow-block*)
    (let ((*voxel-light-solver* :compiled))
      (ok (relight-block-world world)))
    (ok (light-matches-reference-p world))
    (ok (= 10 (blocklight-at world 15 8 8)))
    (ok (= 9 (blocklight-at world 16 8 8)))))

(deftest bucket-frontier-admission-does-not-construct-a-type-per-site
  (let ((frontier
          (luvcraft.frontier:make-bucket-frontier
           :maximum-priority 15 :initial-capacity 8192))
        (observation (make-runtime-observation)))
    ;; Warm CLOS accessor caches outside the measured extent.
    (luvcraft.frontier:bucket-frontier-push frontier nil 0 7)
    (luvcraft.frontier:bucket-frontier-pop frontier)
    (with-runtime-observation (observation)
      (dotimes (offset 8192)
        (luvcraft.frontier:bucket-frontier-push frontier nil offset 7))
      (dotimes (offset 8192)
        (declare (ignore offset))
        (luvcraft.frontier:bucket-frontier-pop frontier)))
    ;; The former dynamic TYPEP specifier allocated three conses per push.
    (ok (< (runtime-observation-bytes-consed observation) 4096))))

(deftest light-solvers-expose-symmetric-nested-timing-zones
  (let ((world (make-open-sky-test-world))
        (trace (make-cpu-trace :label "voxel light solvers")))
    (with-cpu-trace (trace)
      (compare-voxel-light-solvers world))
    (let ((names (mapcar #'cpu-trace-zone-name (cpu-trace-zones trace))))
      (ok (equal
           '(:lighting/compare
             :lighting/legacy
             :lighting/legacy/seed-sky
             :lighting/legacy/propagate-sky
             :lighting/legacy/drain-sites
             :lighting/legacy/seed-block
             :lighting/legacy/propagate-block
             :lighting/legacy/drain-sites
             :lighting/frontier
             :lighting/frontier/seed-sky
             :lighting/frontier/propagate-sky
             :lighting/frontier/drain-sites
             :lighting/frontier/seed-block
             :lighting/frontier/propagate-block
             :lighting/frontier/drain-sites)
           names)))))

(deftest frontier-light-can-be-selected-for-real-publication
  (let ((world (make-open-sky-test-world '(0 0 0) '(1 0 0))))
    (setf (world-block-at world 15 8 8) *test-glow-block*)
    (let ((*voxel-light-solver* :frontier))
      (ok (relight-block-world world)))
    (ok (light-matches-reference-p world))
    (ok (= 10 (blocklight-at world 15 8 8)))
    (ok (= 9 (blocklight-at world 16 8 8)))))

(defun check-incremental-edits-converge ()
  (let* ((world (make-block-world
                 :source (make-instance 'little-world-source :seed 1)))
         (state (luvcraft::attach-lighting-state world)))
    ;; Arrival through the hook lights the fresh chunk incrementally.
    (luvcraft::ensure-world-chunk world 0 0 0)
    (ok (luvcraft::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    ;; Roofing one cell darkens its column; removing it restores the beam.
    (setf (world-block-at world 8 15 8) luvcraft::*stone-block*)
    (ok (luvcraft::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    (ok (= (sky-at world 8 14 8) 14))
    (setf (world-block-at world 8 15 8) nil)
    (ok (luvcraft::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    (ok (= (sky-at world 8 0 8) 15))
    ;; An emitter appears and disappears.
    (setf (world-block-at world 4 4 4) *test-glow-block*)
    (ok (luvcraft::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    (ok (= (blocklight-at world 4 5 4) 9))
    (setf (world-block-at world 4 4 4) nil)
    (ok (luvcraft::reconcile-lighting state))
    (ok (light-matches-reference-p world))
    (ok (= (blocklight-at world 4 5 4) 0))
    ;; A settled state publishes nothing further.
    (ok (null (luvcraft::reconcile-lighting state)))
    (ok (plusp (luvcraft::lighting-state-publications state)))
    (ok (plusp (luvcraft::lighting-state-cells-visited state)))))

(deftest incremental-edits-converge-to-the-reference-field
  (check-incremental-edits-converge))

(deftest compiled-incremental-edits-converge-to-the-reference-field
  (let ((*voxel-light-solver* :compiled))
    (check-incremental-edits-converge)))

(deftest asynchronous-lighting-publishes-only-a-current-immutable-capture
  (let* ((world (make-open-sky-test-world '(0 0 0)))
         (chunk (luvcraft::world-chunk-at world 0 0 0))
         (state (luvcraft::attach-lighting-state world))
         (session (make-instance 'luvcraft-session
                                 :world world :lighting-state state))
         (stale-request
           (make-instance
            'luvcraft::block-light-production-request
            :key '(:light) :priority -1
            :dependency-stamp
            (luvcraft::block-world-light-dependency-stamp world)
            :region (luvcraft::capture-light-region world :immutable-p t))))
    ;; The request owns its dense input.  A later edit invalidates publication
    ;; without changing what the producer is currently solving.
    (setf (world-block-at world 1 1 1) *test-glow-block*)
    (let ((payload
            (luvcraft::perform-production-request stale-request)))
      (ok (null (luvcraft::publish-production-result
                 session stale-request payload)))
      (ok (null (block-chunk-light-field chunk)))
      (ok (luvcraft::lighting-state-dirty-p state)))
    (let* ((request
             (make-instance
              'luvcraft::block-light-production-request
              :key '(:light) :priority -1
              :solver :frontier
              :dependency-stamp
              (luvcraft::block-world-light-dependency-stamp world)
              :region (luvcraft::capture-light-region world :immutable-p t)))
           (payload (luvcraft::perform-production-request request)))
      (ok (eq :frontier
              (luvcraft::block-light-production-request-solver request)))
      (ok (luvcraft::publish-production-result session request payload))
      (ok (= (blocklight-at world 1 1 1)
             (block-light-emission *test-glow-block*)))
      (ok (light-matches-reference-p world)))))

(deftest settled-cell-edits-use-the-incremental-relighter
  (let* ((world (make-open-sky-test-world '(0 0 0)))
         (state (luvcraft::attach-lighting-state world))
         (session (make-instance 'luvcraft-session
                                 :world world :lighting-state state)))
    ;; Initial residency is a global concern.  Settle it before modeling the
    ;; ordinary player edit path in an already visible world.
    (ok (luvcraft::reconcile-lighting state))
    (setf (world-block-at world 8 15 8) luvcraft::*stone-block*)
    (ok (not (luvcraft::lighting-state-residency-dirty-p state)))
    (ok (luvcraft::schedule-luvcraft-lighting session))
    (ok (not (luvcraft::lighting-state-dirty-p state)))
    (ok (not (gethash '(:light)
                      (luvcraft-session-outstanding-production session))))
    (ok (= (sky-at world 8 14 8) 14))
    (ok (light-matches-reference-p world))))

(defun check-random-edits-and-residency ()
  (let* ((world (make-block-world
                 :source (make-instance 'little-world-source :seed 1)))
         (state (luvcraft::attach-lighting-state world))
         (rng 987654321)
         (chunk-keys '((0 0 0) (1 0 0) (0 1 0))))
    (labels ((next-random (limit)
               (setf rng (mod (+ (* rng 1103515245) 12345) (expt 2 31)))
               (mod (floor rng 65536) limit))
             (random-block ()
               (case (next-random 8)
                 ((0 1 2) luvcraft::*stone-block*)
                 (3 *test-glow-block*)
                 (t nil)))
             (random-resident-cell ()
               (let* ((keys (mapcar #'luvcraft::block-chunk-key
                                    (resident-world-chunks world)))
                      (key (nth (next-random (length keys)) keys)))
                 (list (+ (* 16 (first key)) (next-random 16))
                       (+ (* 16 (second key)) (next-random 16))
                       (+ (* 16 (third key)) (next-random 16))))))
      (dolist (key chunk-keys)
        (apply #'luvcraft::ensure-world-chunk world key))
      ;; Random terrain, then interleaved edit bursts and reconciles.
      (dotimes (index 300)
        (destructuring-bind (x y z) (random-resident-cell)
          (setf (world-block-at world x y z) (random-block))))
      (luvcraft::reconcile-lighting state)
      (ok (light-matches-reference-p world))
      (dotimes (round 6)
        (dotimes (edit 10)
          (destructuring-bind (x y z) (random-resident-cell)
            (setf (world-block-at world x y z) (random-block))))
        (luvcraft::reconcile-lighting state)
        (ok (light-matches-reference-p world)))
      ;; A departure relights the retained neighbors; a re-arrival with
      ;; fresh edits converges again.
      (luvcraft::remove-world-chunk world 1 0 0)
      (luvcraft::reconcile-lighting state)
      (ok (light-matches-reference-p world))
      (luvcraft::ensure-world-chunk world 1 0 0)
      (dotimes (edit 12)
        (setf (world-block-at world
                              (+ 16 (next-random 16))
                              (next-random 16)
                              (next-random 16))
              (random-block)))
      (luvcraft::reconcile-lighting state)
      (ok (light-matches-reference-p world)))))

(deftest random-edits-and-residency-match-the-reference-solver
  (check-random-edits-and-residency))

(deftest compiled-random-edits-and-residency-match-the-reference-solver
  ;; The compiled removal and addition programs must reproduce the reference
  ;; field across the same edit bursts, departure, and re-arrival. #K3WRD3
  (let ((*voxel-light-solver* :compiled))
    (check-random-edits-and-residency)))

(deftest compiled-light-removal-is-an-invalidation-program
  (let ((definition
          (luvcraft.frontier:frontier-program-definition-for
           'luvcraft::voxel-light-removal))
        (realization (luvcraft::compiled-light-removal-realization :sky-light)))
    (ok (eq :invalidation
            (luvcraft.frontier:frontier-program-definition-family definition)))
    (ok (luvcraft.frontier:frontier-program-definition-retain-admissions-p
         definition))
    (ok (functionp
         (luvcraft.frontier:frontier-realization-drain-function realization)))
    (ok (functionp
         (luvcraft.frontier:frontier-realization-admit-function realization))))
  ;; Roofing a lit column: the sky removal clears exactly the beam beneath
  ;; the roof (its dependents), hands the beam's lit lateral neighbours to
  ;; the addition frontier as survivors, and the addition program relights
  ;; the column from them to the reference field.
  (let* ((world (make-open-sky-test-world))
         (state (luvcraft::attach-lighting-state world))
         (*voxel-light-solver* :compiled))
    (luvcraft::reconcile-lighting state)
    (setf (world-block-at world 8 15 8) luvcraft::*stone-block*)
    (let* ((region (luvcraft::make-light-candidate world)))
      (multiple-value-bind (executions visits)
          (luvcraft::reconcile-compiled-lighting state region)
        (destructuring-bind (sky-removal block-removal sky-addition block-addition)
            executions
          (declare (ignore block-removal block-addition))
          ;; The edited cell plus the fifteen cells of beam beneath it.
          (ok (= 16 (luvcraft.frontier:frontier-site-buffer-length
                     (luvcraft.frontier:frontier-execution-admitted-sites
                      sky-removal))))
          (ok (= 16 (luvcraft.frontier:frontier-execution-visits sky-removal)))
          (ok (plusp (luvcraft.frontier:frontier-execution-emissions
                      sky-removal)))
          (ok (plusp (luvcraft.frontier:frontier-execution-visits
                      sky-addition)))
          (ok (plusp visits))))
      (luvcraft::publish-light-region region)
      (clrhash (luvcraft::lighting-state-dirty-cells state)))
    (ok (light-matches-reference-p world))
    (ok (= 14 (sky-at world 8 14 8)))))

(deftest same-key-replacement-removes-the-old-chunk-light
  (let* ((world (make-open-sky-test-world '(0 0 0) '(1 0 0)))
         (state (luvcraft::attach-lighting-state world)))
    (setf (world-block-at world 16 8 8) *test-glow-block*)
    (luvcraft::reconcile-lighting state)
    (ok (= (blocklight-at world 15 8 8) 9))
    ;; Streaming can replace a chunk at the same key before the next lighting
    ;; reconcile.  The departure still has to run, or retained neighbors keep
    ;; light propagated from the old incarnation.
    (luvcraft::remove-world-chunk world 1 0 0)
    (luvcraft::ensure-world-chunk world 1 0 0)
    (luvcraft::reconcile-lighting state)
    (ok (light-matches-reference-p world))
    (ok (= (blocklight-at world 15 8 8) 0))))

(deftest player-placeable-crystal-relights-across-chunk-boundaries
  (let* ((world (make-open-sky-test-world '(0 0 0) '(1 0 0)))
         (state (luvcraft::attach-lighting-state world))
         (left (luvcraft::world-chunk-at world 0 0 0))
         (right (luvcraft::world-chunk-at world 1 0 0)))
    (luvcraft::reconcile-lighting state)
    (let ((left-revision-before
            (chunk-light-field-revision (block-chunk-light-field left)))
          (right-revision-before
            (chunk-light-field-revision (block-chunk-light-field right))))
      (edit-block-at *crystal-block* world 16 8 8)
      (ok (luvcraft::reconcile-lighting state))
      (ok (light-matches-reference-p world))
      (ok (= (blocklight-at world 16 8 8)
             (block-light-emission *crystal-block*)))
      (ok (= (blocklight-at world 15 8 8)
             (1- (block-light-emission *crystal-block*))))
      (ok (> (chunk-light-field-revision (block-chunk-light-field left))
             left-revision-before))
      (ok (> (chunk-light-field-revision (block-chunk-light-field right))
             right-revision-before)))
    (let ((left-revision-before
            (chunk-light-field-revision (block-chunk-light-field left)))
          (right-revision-before
            (chunk-light-field-revision (block-chunk-light-field right))))
      (edit-block-at nil world 16 8 8)
      (ok (luvcraft::reconcile-lighting state))
      (ok (light-matches-reference-p world))
      (ok (= (blocklight-at world 16 8 8) 0))
      (ok (= (blocklight-at world 15 8 8) 0))
      (ok (> (chunk-light-field-revision (block-chunk-light-field left))
             left-revision-before))
      (ok (> (chunk-light-field-revision (block-chunk-light-field right))
             right-revision-before)))))

(deftest meshes-carry-raw-corner-light-and-material-emission
  (let* ((world (make-open-sky-test-world))
         (state (luvcraft::attach-lighting-state world))
         (mesher (make-instance 'exposed-face-mesher)))
    ;; A solid floor with a glowing block resting on it, under open sky.
    (dotimes (x 16)
      (dotimes (z 16)
        (setf (world-block-at world x 0 z) luvcraft::*stone-block*)))
    (setf (world-block-at world 8 1 8) *crystal-block*)
    (luvcraft::reconcile-lighting state)
    (let* ((chunk (luvcraft::world-chunk-at world 0 0 0))
           (mesh (mesh-block-chunk mesher world chunk))
           (vertices (block-mesh-vertices mesh))
           (stride luvcraft::+block-mesh-floats-per-vertex+)
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
      (let* ((snapshot (luvcraft::make-block-mesh-snapshot
                        world chunk
                        (luvcraft::chunk-mesh-dependency-stamp world chunk)))
             (snapshot-mesh (mesh-block-snapshot mesher snapshot)))
        (ok (equalp (block-mesh-vertices mesh)
                    (block-mesh-vertices snapshot-mesh)))))))

(deftest absent-neighbors-are-never-silently-open-sky
  ;; A world with no source keeps every boundary :UNKNOWN, so nothing is
  ;; lit and the result says so instead of inventing daylight.
  (let ((world (make-block-world)))
    (luvcraft::ensure-world-chunk world 0 0 0)
    (relight-block-world world)
    (multiple-value-bind (sky block state) (world-light-at world 8 8 8)
      (ok (= sky 0))
      (ok (= block 0))
      (ok (eq state :provisional)))))

(deftest packed-light-scheduling-preserves-the-fixed-point
  (let* ((world (make-little-block-world :chunk-radius 1 :seed 121))
         (lifo (luvcraft::capture-light-region world))
         (level (luvcraft::capture-light-region world)))
    (multiple-value-bind (lifo-result lifo-visits)
        (luvcraft::solve-light-region lifo :scheduling :lifo)
      (declare (ignore lifo-result))
      (multiple-value-bind (level-result level-visits)
          (luvcraft::solve-light-region level :scheduling :level)
        (declare (ignore level-result))
        (ok (< level-visits lifo-visits))
        (maphash
         (lambda (key lifo-entry)
           (let ((level-entry
                   (gethash key (luvcraft::light-region-entries level))))
             (ok level-entry)
             (ok (equalp (luvcraft::light-region-entry-sky lifo-entry)
                         (luvcraft::light-region-entry-sky level-entry)))
             (ok (equalp (luvcraft::light-region-entry-block lifo-entry)
                         (luvcraft::light-region-entry-block level-entry)))))
         (luvcraft::light-region-entries lifo))))))

(deftest light-hot-traversal-locates-only-at-domain-crossings
  (let ((world (make-block-world :chunk-width 3
                                 :chunk-height 3
                                 :chunk-depth 3)))
    (let* ((chunk (luvcraft::ensure-world-chunk world 0 0 0))
           (region (luvcraft::capture-light-region world))
           (key (chunk-domain-coordinate (block-chunk-domain chunk)))
           (entry (gethash key (luvcraft::light-region-entries region)))
           (domain (luvcraft::light-region-entry-domain entry))
           (levels (luvcraft::light-region-entry-block entry)))
      (labels ((visit (x y z)
                 (fill levels 0)
                 (let* ((local (make-local-coordinate x y z))
                        (offset (chunk-domain-offset domain local))
                        (queue (luvcraft::make-light-worklist))
                        (*light-region-window-lookups* 0))
                   (setf (aref levels offset) 1)
                   (luvcraft::light-worklist-push queue entry offset 1)
                   (values
                    (luvcraft::propagate-light-region
                     region #'luvcraft::light-region-entry-block
                     queue nil)
                    *light-region-window-lookups*))))
        (multiple-value-bind (visited lookups) (visit 1 1 1)
          (ok (= visited 1))
          (ok (zerop lookups)))
        (multiple-value-bind (visited lookups) (visit 0 1 1)
          (ok (= visited 1))
          (ok (= lookups 1)))))))

(deftest unlighting-locates-only-at-domain-crossings
  (let ((world (make-block-world :chunk-width 3
                                 :chunk-height 3
                                 :chunk-depth 3)))
    (let* ((chunk (luvcraft::ensure-world-chunk world 0 0 0))
           (region (luvcraft::capture-light-region world))
           (key (chunk-domain-coordinate (block-chunk-domain chunk)))
           (entry (gethash key (luvcraft::light-region-entries region))))
      (labels ((visit (x y z)
                 (let* ((local (make-local-coordinate x y z))
                        (offset
                          (chunk-domain-offset
                           (luvcraft::light-region-entry-domain entry) local))
                        (queue
                          (luvcraft::make-light-removal-queue
                           :block-light #'luvcraft::light-region-entry-block))
                        (sources (luvcraft::make-light-worklist))
                        (*light-region-window-lookups* 0))
                   (luvcraft::enqueue-light-removal queue entry offset 1)
                   (values
                    (luvcraft::unlight-light-region region queue sources)
                    *light-region-window-lookups*))))
        (multiple-value-bind (visited lookups) (visit 1 1 1)
          (ok (= visited 1))
          (ok (zerop lookups)))
        (multiple-value-bind (visited lookups) (visit 0 1 1)
          (ok (= visited 1))
          (ok (= lookups 1)))))))

(deftest light-boundary-change-comparison-uses-domain-faces
  (let ((world (make-block-world :chunk-width 3
                                 :chunk-height 4
                                 :chunk-depth 5)))
    (let* ((chunk (luvcraft::ensure-world-chunk world 0 0 0))
           (region (luvcraft::capture-light-region world))
           (key (chunk-domain-coordinate (block-chunk-domain chunk)))
           (domain (luvcraft::light-region-entry-domain
                    (gethash key (luvcraft::light-region-entries region))))
           (old (make-array (chunk-domain-cardinality domain)
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
           (new (copy-seq old)))
      (labels ((changed-directions ()
                 (loop for direction in luvcraft::*voxel-face-directions*
                       when (luvcraft::light-boundary-plane-changed-p
                             domain old new direction)
                         collect direction)))
        (setf (aref new
                    (chunk-domain-offset
                     domain (make-local-coordinate 1 1 1)))
              1)
        (ok (null (changed-directions)))
        (fill new 0)
        (setf (aref new 0) 1)
        (ok (equal (changed-directions)
                   (list +voxel-negative-x+
                         +voxel-negative-y+
                         +voxel-negative-z+)))))))

(deftest light-region-is-a-chunk-window-with-policy-free-availability
  (let ((world (make-block-world)))
    (luvcraft::ensure-world-chunk world 0 0 0)
    (let ((region (luvcraft::capture-light-region world)))
      (multiple-value-bind (entry offset availability)
          (locate-chunk-window-site region 0 0 0)
        (ok entry)
        (ok (zerop offset))
        (ok (eq availability :available)))
      (multiple-value-bind (entry offset availability)
          (locate-chunk-window-site region 16 0 0)
        (ok (null entry))
        (ok (null offset))
        (ok (eq availability :unavailable))))))
