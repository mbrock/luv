(in-package #:luvcraft.tests)

;;; The reference relight is exercised on small hand-built worlds whose
;;; correct fields can be reasoned out cell by cell: open columns, roofs
;;; and shafts, emitters and their falloff, and both vertical and lateral
;;; chunk seams.

(defparameter *test-glow-block*
  (make-instance 'luvcraft::block-kind
                 :name :test-glow :face-tiles '(:all :stone)
                 :light-emission 10 :surface-emission 1.0))

(defparameter *test-dim-glow-block*
  (make-instance 'luvcraft::block-kind
                 :name :test-dim-glow :face-tiles '(:all :stone)
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

(define-test open-columns-see-full-sky
  (let ((world (make-open-sky-test-world)))
    (relight-block-world world)
    (true (= (sky-at world 8 15 8) 15))
    (true (= (sky-at world 8 0 8) 15))
    (true (= (sky-at world 0 0 0) 15))
    (true (= (sky-at world 15 7 15) 15))
    ;; Lateral terrain is generatable but not resident, so the result is
    ;; honest but provisional.
    (true (eq (nth-value 2 (world-light-at world 8 8 8)) :provisional))))

(define-test roofs-block-light-and-shafts-transmit-it
  (let ((world (make-open-sky-test-world)))
    (dotimes (x 16)
      (dotimes (z 16)
        (unless (and (= x 8) (= z 8))
          (setf (world-block-at world x 15 z) luvcraft::*stone-block*))))
    (relight-block-world world)
    ;; Stone roof cells admit no sky; the open shaft carries a full beam
    ;; to the floor, and lateral spread attenuates one level per step.
    (true (= (sky-at world 2 15 2) 0))
    (true (= (sky-at world 8 15 8) 15))
    (true (= (sky-at world 8 0 8) 15))
    (true (= (sky-at world 7 14 8) 14))
    (true (= (sky-at world 2 14 2) 3))
    (true (= (sky-at world 0 0 0) 0))))

(define-test emitters-fall-off-and-the-brighter-source-wins
  (let ((world (make-open-sky-test-world)))
    (setf (world-block-at world 8 8 8) *test-glow-block*)
    (setf (world-block-at world 12 8 8) *test-dim-glow-block*)
    (relight-block-world world)
    (true (= (blocklight-at world 8 8 8) 10))
    (true (= (blocklight-at world 8 9 8) 9))
    (true (= (blocklight-at world 8 8 11) 7))
    (true (= (blocklight-at world 8 8 2) 4))
    ;; Sixteen Manhattan steps out, the level is exhausted.
    (true (= (blocklight-at world 0 8 0) 0))
    ;; Between the two emitters each cell keeps the brighter contribution.
    (true (= (blocklight-at world 10 8 8) 8))
    (true (= (blocklight-at world 11 8 8) 7))
    ;; Emission does not perturb the sky field.
    (true (= (sky-at world 8 9 8) 15))))

(define-test walls-stop-blocklight
  (let ((world (make-open-sky-test-world)))
    (setf (world-block-at world 4 8 8) *test-glow-block*)
    ;; A full stone shell one step out along +X.
    (dotimes (y 16)
      (dotimes (z 16)
        (setf (world-block-at world 6 y z) luvcraft::*stone-block*)))
    (relight-block-world world)
    (true (= (blocklight-at world 5 8 8) 9))
    (true (= (blocklight-at world 6 8 8) 0))
    (true (= (blocklight-at world 7 8 8) 0))))

(define-test sky-crosses-a-vertical-chunk-seam
  (let ((world (make-open-sky-test-world '(0 0 0) '(0 1 0))))
    ;; Roof the upper chunk's top layer, world y = 31, with one shaft.
    (dotimes (x 16)
      (dotimes (z 16)
        (unless (and (= x 4) (= z 4))
          (setf (world-block-at world x 31 z) luvcraft::*stone-block*))))
    (relight-block-world world)
    (true (= (sky-at world 4 31 4) 15))
    ;; The beam crosses the seam at world y = 15/16 undiminished.
    (true (= (sky-at world 4 16 4) 15))
    (true (= (sky-at world 4 15 4) 15))
    (true (= (sky-at world 4 0 4) 15))
    ;; Lateral falloff below the seam still measures from the shaft.
    (true (= (sky-at world 0 5 0) 7))
    (true (= (sky-at world 15 5 15) 0))))

(define-test sky-crosses-a-lateral-chunk-seam
  (let ((world (make-open-sky-test-world '(0 0 0) '(1 0 0))))
    ;; Roof the +X chunk completely; its light must arrive sideways from
    ;; the open chunk across the seam.
    (dotimes (x 16)
      (dotimes (z 16)
        (setf (world-block-at world (+ 16 x) 15 z) luvcraft::*stone-block*)))
    (relight-block-world world)
    (true (= (sky-at world 15 14 8) 15))
    (true (= (sky-at world 16 14 8) 14))
    (true (= (sky-at world 20 14 8) 10))
    (true (= (sky-at world 31 14 8) 0))))

(define-test relighting-is-a-derived-domain-with-its-own-revisions
  (let ((world (make-open-sky-test-world)))
    (let* ((chunk (luvcraft::world-chunk-at world 0 0 0))
           (content-revision (luvcraft::block-chunk-revision chunk))
           (world-revision (block-world-revision world))
           (changed (relight-block-world world))
           (field (block-chunk-light-field chunk)))
      (true (= (length changed) 1))
      (true (= (chunk-light-field-revision field) 1))
      ;; Light publication does not impersonate an authored edit.
      (true (= (luvcraft::block-chunk-revision chunk) content-revision))
      (true (= (block-world-revision world) world-revision))
      ;; A second solve over unchanged content publishes nothing.
      (true (null (relight-block-world world)))
      (true (= (chunk-light-field-revision field) 1))
      ;; A content edit then changes the field and only the light revision
      ;; and changed light boundaries advance.
      (let ((top-before
              (chunk-light-field-boundary-revision field +voxel-positive-y+))
            (bottom-before
              (chunk-light-field-boundary-revision field +voxel-negative-y+)))
        (setf (world-block-at world 8 15 8) luvcraft::*stone-block*)
        (true (relight-block-world world))
        (true (= (chunk-light-field-revision field) 2))
        (true (= (chunk-light-field-boundary-revision field +voxel-positive-y+)
                 (1+ top-before)))
        (true (= (chunk-light-field-boundary-revision field +voxel-negative-y+)
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

(define-test compiled-frontier-light-is-inspectable-and-matches-the-oracle
  (let ((definition
          (luvcraft.frontier:frontier-program-definition-for
           'luvcraft::voxel-light-addition)))
    (true (typep definition 'luvcraft.frontier:frontier-program-definition))
    (true (eq :monotone-max-fixpoint
              (luvcraft.frontier:frontier-program-definition-family
               definition)))
    (true (eq :brightest-first-buckets
              (luvcraft.frontier:frontier-program-definition-frontier-layout
               definition)))
    (true (eq :voxel-face-relations
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
    (let ((comparison
            (luvcraft.light-reference:compare-voxel-light-solvers world)))
      (true (luvcraft.light-reference:voxel-light-solver-comparison-equal-p
             comparison))
      (true (null
             (luvcraft.light-reference:voxel-light-solver-comparison-mismatched-keys
              comparison)))
      (true (= (luvcraft.light-reference:voxel-light-solver-comparison-legacy-visits
                comparison)
               (luvcraft.light-reference:voxel-light-solver-comparison-candidate-visits
                comparison)))
      (true (plusp
             (luvcraft.light-reference:voxel-light-solver-comparison-legacy-bytes-consed
              comparison)))
      (true (plusp
             (luvcraft.light-reference:voxel-light-solver-comparison-candidate-bytes-consed
              comparison)))
      (true (>= (luvcraft.light-reference:voxel-light-solver-comparison-legacy-gc-seconds
                 comparison)
                0d0))
      (true (>= (luvcraft.light-reference:voxel-light-solver-comparison-candidate-gc-seconds
                 comparison)
                0d0))
      (true (= 2 (length
                  (luvcraft.light-reference:voxel-light-solver-comparison-candidate-executions
                   comparison))))
      (dolist (execution
               (luvcraft.light-reference:voxel-light-solver-comparison-candidate-executions
                comparison))
        (true (plusp
               (luvcraft.frontier:frontier-execution-visits execution)))
        (true (= (* 6
                    (luvcraft.frontier:frontier-execution-visits execution))
                 (luvcraft.frontier:frontier-execution-relations execution)))
        (true (plusp
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

(define-test compiled-light-kernel-is-inspectable-and-matches-the-reference-oracle
  ;; The program states its law; the realization retains the checked
  ;; expressions, the emitted forms, and the compiled functions.
  (let* ((definition
           (luvcraft.frontier:frontier-program-definition-for
            'luvcraft::voxel-light-addition))
         (realization (luvcraft::compiled-light-realization :sky-light)))
    (true (luvcraft.frontier:frontier-program-definition-transfer definition))
    (true (equal '("LEVEL" "OPACITY")
                 (mapcar (lambda (role)
                           (symbol-name
                            (luvcraft.frontier:frontier-field-role-name role)))
                         (luvcraft.frontier:frontier-program-definition-fields
                          definition))))
    (true (luvcraft.frontier:frontier-realization-current-p realization))
    (true (eq 'lambda
              (first (luvcraft.frontier:frontier-realization-drain-form
                      realization))))
    (true (functionp
           (luvcraft.frontier:frontier-realization-drain-function realization)))
    (true (functionp
           (luvcraft.frontier:frontier-realization-admit-function realization)))
    (true (= 15 (luvcraft.frontier:frontier-realization-maximum-priority
                 realization)))
    ;; The transfer law is checked in the bound field's own quantity: sky
    ;; light stays sky light, and attenuation steps enter through an explicit
    ;; AS-FIELD-QUANTITY boundary rather than by coincidence of encoding.
    (true (eq :sky-propagation-level
              (luv.arithmetic:quantity-specification-name
               (luv.arithmetic.language:arithmetic-expression-quantity-specification
                (luvcraft.frontier:frontier-realization-transfer realization)))))
    (true (eq :block-propagation-level
              (luv.arithmetic:quantity-specification-name
               (luv.arithmetic.language:arithmetic-expression-quantity-specification
                (luvcraft.frontier:frontier-realization-transfer
                 (luvcraft::compiled-light-realization :block-light))))))
    ;; The emitted scalar loop contains no arithmetic dispatch: the law was
    ;; inlined as ordinary CL operators over declared lanes.
    (true (not (labels ((mentions-p (tree)
                          (if (atom tree)
                              (member tree '(luv.arithmetic.lisp::lisp-add
                                             luv.arithmetic.lisp::lisp-subtract
                                             funcall apply))
                              (or (mentions-p (car tree))
                                  (mentions-p (cdr tree))))))
                 (mentions-p
                  (luvcraft.frontier:frontier-realization-drain-form
                   realization))))))
  (let* ((world (make-compiled-light-proof-world))
         (comparison
           (luvcraft.light-reference:compare-voxel-light-solvers world)))
    (true (luvcraft.light-reference:voxel-light-solver-comparison-equal-p
           comparison))
    (true (= (luvcraft.light-reference:voxel-light-solver-comparison-legacy-visits
              comparison)
             (luvcraft.light-reference:voxel-light-solver-comparison-candidate-visits
              comparison)))
    (true (eq :compiled
              (luvcraft.light-reference:voxel-light-solver-comparison-candidate-solver
               comparison)))
    (true (= 2 (length
                (luvcraft.light-reference:voxel-light-solver-comparison-candidate-executions
                 comparison))))))

(define-test compiled-light-seeds-are-boundary-transfers
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
      (true (functionp (luvcraft.frontier:frontier-realization-relate-function sky)))
      (flet ((relate (x y z direction)
               (luvcraft.frontier:relate-frontier-realization-site
                sky region frontier execution entry
                (chunk-domain-offset-components domain x y z) direction
                :direct-direction luvcraft.world:+voxel-negative-y+
                :level luvcraft::+maximum-light-level+)))
        (true (relate 0 15 0 luvcraft.world:+voxel-negative-y+))
        (true (= 15 (aref levels (chunk-domain-offset-components domain 0 15 0))))
        (true (relate 15 15 0 luvcraft.world:+voxel-negative-x+))
        (true (= 14 (aref levels (chunk-domain-offset-components domain 15 15 0))))
        (true (not (relate 3 15 3 luvcraft.world:+voxel-negative-y+)))
        (true (= 0 (aref levels (chunk-domain-offset-components domain 3 15 3))))
        ;; A second, dimmer relation into the same site is not an improvement.
        (true (not (relate 0 15 0 luvcraft.world:+voxel-negative-x+))))
      (true (= 2 (luvcraft.frontier:bucket-frontier-count frontier)))
      (true (= 2 (luvcraft.frontier:frontier-execution-admissions execution))))))

(define-test compiled-light-drain-allocates-nothing-per-relation
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
    (true (> relations 60000))
    (true (< bytes (+ (* 64 1024) (* 128 crossings))))))

(define-test compiled-light-can-be-selected-for-real-publication
  (let ((world (make-open-sky-test-world '(0 0 0) '(1 0 0))))
    (setf (world-block-at world 15 8 8) *test-glow-block*)
    (let ((*voxel-light-solver* :compiled))
      (true (relight-block-world world)))
    (true (light-matches-reference-p world))
    (true (= 10 (blocklight-at world 15 8 8)))
    (true (= 9 (blocklight-at world 16 8 8)))))

(define-test compiled-frontier-light-is-the-production-default
  (true (eq :compiled *voxel-light-solver*))
  (let ((request
          (make-instance 'luvcraft::block-light-production-request
                         :key '(:light)
                         :region nil
                         :dependency-stamp nil)))
    (true (eq :compiled
              (luvcraft::block-light-production-request-solver request)))))

(define-test voxel-light-solver-dispatch-rejects-retired-and-unknown-names
  (let* ((world (make-open-sky-test-world))
         (region (luvcraft::capture-light-region world))
         (state (luvcraft::attach-lighting-state world))
         (candidate (luvcraft::make-light-candidate world)))
    (dolist (solver '(:legacy :frontier :misspelled))
      (fail (solve-light-region-using solver region) 'error)
      (fail
       (luvcraft::reconcile-light-region-using solver state candidate)
       'error))))

(define-test bucket-frontier-admission-does-not-construct-a-type-per-site
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
    (true (< (runtime-observation-bytes-consed observation) 4096))))

(define-test light-solvers-expose-symmetric-nested-timing-zones
  (let ((world (make-open-sky-test-world))
        (trace (make-cpu-trace :label "voxel light solvers")))
    (with-cpu-trace (trace)
      (luvcraft.light-reference:compare-voxel-light-solvers world))
    (let ((names (mapcar #'cpu-trace-zone-name (cpu-trace-zones trace))))
      (true (equal
             '(:lighting/compare
               :lighting/legacy
               :lighting/legacy/seed-sky
               :lighting/legacy/propagate-sky
               :lighting/legacy/drain-sites
               :lighting/legacy/seed-block
               :lighting/legacy/propagate-block
               :lighting/legacy/drain-sites
               :lighting/compiled
               :lighting/compiled/seed-sky
               :lighting/compiled/propagate-sky
               :lighting/compiled/drain-sites
               :lighting/compiled/seed-block
               :lighting/compiled/propagate-block
               :lighting/compiled/drain-sites)
             names)))))

(defun check-incremental-edits-converge ()
  (let* ((world (make-block-world
                 :source (make-instance 'little-world-source :seed 1)))
         (state (luvcraft::attach-lighting-state world)))
    ;; Arrival through the hook lights the fresh chunk incrementally.
    (luvcraft::ensure-world-chunk world 0 0 0)
    (true (luvcraft::reconcile-lighting state))
    (true (light-matches-reference-p world))
    ;; Roofing one cell darkens its column; removing it restores the beam.
    (setf (world-block-at world 8 15 8) luvcraft::*stone-block*)
    (true (luvcraft::reconcile-lighting state))
    (true (light-matches-reference-p world))
    (true (= (sky-at world 8 14 8) 14))
    (setf (world-block-at world 8 15 8) nil)
    (true (luvcraft::reconcile-lighting state))
    (true (light-matches-reference-p world))
    (true (= (sky-at world 8 0 8) 15))
    ;; An emitter appears and disappears.
    (setf (world-block-at world 4 4 4) *test-glow-block*)
    (true (luvcraft::reconcile-lighting state))
    (true (light-matches-reference-p world))
    (true (= (blocklight-at world 4 5 4) 9))
    (setf (world-block-at world 4 4 4) nil)
    (true (luvcraft::reconcile-lighting state))
    (true (light-matches-reference-p world))
    (true (= (blocklight-at world 4 5 4) 0))
    ;; A settled state publishes nothing further.
    (true (null (luvcraft::reconcile-lighting state)))
    (true (plusp (luvcraft::lighting-state-publications state)))
    (true (plusp (luvcraft::lighting-state-cells-visited state)))))

(define-test compiled-incremental-edits-converge-to-the-reference-field
  (check-incremental-edits-converge))

(define-test asynchronous-lighting-publishes-only-a-current-immutable-capture
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
      (true (null (luvcraft::publish-production-result
                   session stale-request payload)))
      (true (null (block-chunk-light-field chunk)))
      (true (luvcraft::lighting-state-dirty-p state)))
    (let* ((request
             (make-instance
              'luvcraft::block-light-production-request
              :key '(:light) :priority -1
              :solver :compiled
              :dependency-stamp
              (luvcraft::block-world-light-dependency-stamp world)
              :region (luvcraft::capture-light-region world :immutable-p t)))
           (payload (luvcraft::perform-production-request request)))
      (true (eq :compiled
                (luvcraft::block-light-production-request-solver request)))
      (true (luvcraft::publish-production-result session request payload))
      (true (= (blocklight-at world 1 1 1)
               (block-light-emission *test-glow-block*)))
      (true (light-matches-reference-p world)))))

(define-test settled-cell-edits-use-the-incremental-relighter
  (let* ((world (make-open-sky-test-world '(0 0 0)))
         (state (luvcraft::attach-lighting-state world))
         (session (make-instance 'luvcraft-session
                                 :world world :lighting-state state)))
    ;; Initial residency is a global concern.  Settle it before modeling the
    ;; ordinary player edit path in an already visible world.
    (true (luvcraft::reconcile-lighting state))
    (setf (world-block-at world 8 15 8) luvcraft::*stone-block*)
    (true (not (luvcraft::lighting-state-residency-dirty-p state)))
    (true (luvcraft::schedule-luvcraft-lighting session))
    (true (not (luvcraft::lighting-state-dirty-p state)))
    (true (not (gethash '(:light)
                        (luvcraft-session-outstanding-production session))))
    (true (= (sky-at world 8 14 8) 14))
    (true (light-matches-reference-p world))))

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
      (true (light-matches-reference-p world))
      (dotimes (round 6)
        (dotimes (edit 10)
          (destructuring-bind (x y z) (random-resident-cell)
            (setf (world-block-at world x y z) (random-block))))
        (luvcraft::reconcile-lighting state)
        (true (light-matches-reference-p world)))
      ;; A departure relights the retained neighbors; a re-arrival with
      ;; fresh edits converges again.
      (luvcraft::remove-world-chunk world 1 0 0)
      (luvcraft::reconcile-lighting state)
      (true (light-matches-reference-p world))
      (luvcraft::ensure-world-chunk world 1 0 0)
      (dotimes (edit 12)
        (setf (world-block-at world
                              (+ 16 (next-random 16))
                              (next-random 16)
                              (next-random 16))
              (random-block)))
      (luvcraft::reconcile-lighting state)
      (true (light-matches-reference-p world)))))

(define-test compiled-random-edits-and-residency-match-the-reference-solver
  ;; The compiled removal and addition programs must reproduce the reference
  ;; field across the same edit bursts, departure, and re-arrival. #K3WRD3
  (check-random-edits-and-residency))

(define-test compiled-light-removal-is-an-invalidation-program
  (let ((definition
          (luvcraft.frontier:frontier-program-definition-for
           'luvcraft::voxel-light-removal))
        (realization (luvcraft::compiled-light-removal-realization :sky-light)))
    (true (eq :invalidation
              (luvcraft.frontier:frontier-program-definition-family definition)))
    (true (luvcraft.frontier:frontier-program-definition-retain-admissions-p
           definition))
    (true (functionp
           (luvcraft.frontier:frontier-realization-drain-function realization)))
    (true (functionp
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
          (true (= 16 (luvcraft.frontier:frontier-site-buffer-length
                       (luvcraft.frontier:frontier-execution-admitted-sites
                        sky-removal))))
          (true (= 16 (luvcraft.frontier:frontier-execution-visits sky-removal)))
          (true (plusp (luvcraft.frontier:frontier-execution-emissions
                        sky-removal)))
          (true (plusp (luvcraft.frontier:frontier-execution-visits
                        sky-addition)))
          (true (plusp visits))))
      (luvcraft::publish-light-region region)
      (clrhash (luvcraft::lighting-state-dirty-cells state)))
    (true (light-matches-reference-p world))
    (true (= 14 (sky-at world 8 14 8)))))

(define-test same-key-replacement-removes-the-old-chunk-light
  (let* ((world (make-open-sky-test-world '(0 0 0) '(1 0 0)))
         (state (luvcraft::attach-lighting-state world)))
    (setf (world-block-at world 16 8 8) *test-glow-block*)
    (luvcraft::reconcile-lighting state)
    (true (= (blocklight-at world 15 8 8) 9))
    ;; Streaming can replace a chunk at the same key before the next lighting
    ;; reconcile.  The departure still has to run, or retained neighbors keep
    ;; light propagated from the old incarnation.
    (luvcraft::remove-world-chunk world 1 0 0)
    (luvcraft::ensure-world-chunk world 1 0 0)
    (luvcraft::reconcile-lighting state)
    (true (light-matches-reference-p world))
    (true (= (blocklight-at world 15 8 8) 0))))

(define-test player-placeable-crystal-relights-across-chunk-boundaries
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
      (true (luvcraft::reconcile-lighting state))
      (true (light-matches-reference-p world))
      (true (= (blocklight-at world 16 8 8)
               (block-light-emission *crystal-block*)))
      (true (= (blocklight-at world 15 8 8)
               (1- (block-light-emission *crystal-block*))))
      (true (> (chunk-light-field-revision (block-chunk-light-field left))
               left-revision-before))
      (true (> (chunk-light-field-revision (block-chunk-light-field right))
               right-revision-before)))
    (let ((left-revision-before
            (chunk-light-field-revision (block-chunk-light-field left)))
          (right-revision-before
            (chunk-light-field-revision (block-chunk-light-field right))))
      (edit-block-at nil world 16 8 8)
      (true (luvcraft::reconcile-lighting state))
      (true (light-matches-reference-p world))
      (true (= (blocklight-at world 16 8 8) 0))
      (true (= (blocklight-at world 15 8 8) 0))
      (true (> (chunk-light-field-revision (block-chunk-light-field left))
               left-revision-before))
      (true (> (chunk-light-field-revision (block-chunk-light-field right))
               right-revision-before)))))

(define-test meshes-carry-raw-corner-light-and-material-emission
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
      (true (find 1.0 sky-readings))
      (true (plusp (reduce #'max block-readings)))
      ;; Five exposed faces of the resting glow block, six vertices each.
      (true (= emissive-vertices 30))
      ;; The immutable snapshot meshes bit-identically to the owner side.
      (let* ((snapshot (luvcraft::make-block-mesh-snapshot
                        world chunk
                        (luvcraft::chunk-mesh-dependency-stamp world chunk)))
             (snapshot-mesh (mesh-block-snapshot mesher snapshot)))
        (true (equalp (block-mesh-vertices mesh)
                      (block-mesh-vertices snapshot-mesh)))))))

(define-test absent-neighbors-are-never-silently-open-sky
  ;; A world with no source keeps every boundary :UNKNOWN, so nothing is
  ;; lit and the result says so instead of inventing daylight.
  (let ((world (make-block-world)))
    (luvcraft::ensure-world-chunk world 0 0 0)
    (relight-block-world world)
    (multiple-value-bind (sky block state) (world-light-at world 8 8 8)
      (true (= sky 0))
      (true (= block 0))
      (true (eq state :provisional)))))

(define-test packed-light-scheduling-preserves-the-fixed-point
  (let* ((world (make-little-block-world :chunk-radius 1 :seed 121))
         (lifo (luvcraft::capture-light-region world))
         (level (luvcraft::capture-light-region world)))
    (multiple-value-bind (lifo-result lifo-visits)
        (luvcraft::solve-light-region lifo :scheduling :lifo)
      (declare (ignore lifo-result))
      (multiple-value-bind (level-result level-visits)
          (luvcraft::solve-light-region level :scheduling :level)
        (declare (ignore level-result))
        (true (< level-visits lifo-visits))
        (maphash
         (lambda (key lifo-entry)
           (let ((level-entry
                   (gethash key (luvcraft::light-region-entries level))))
             (true level-entry)
             (true (equalp (luvcraft::light-region-entry-sky lifo-entry)
                           (luvcraft::light-region-entry-sky level-entry)))
             (true (equalp (luvcraft::light-region-entry-block lifo-entry)
                           (luvcraft::light-region-entry-block level-entry)))))
         (luvcraft::light-region-entries lifo))))))

(define-test light-hot-traversal-locates-only-at-domain-crossings
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
          (true (= visited 1))
          (true (zerop lookups)))
        (multiple-value-bind (visited lookups) (visit 0 1 1)
          (true (= visited 1))
          (true (= lookups 1)))))))

(define-test light-boundary-change-comparison-uses-domain-faces
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
        (true (null (changed-directions)))
        (fill new 0)
        (setf (aref new 0) 1)
        (true (equal (changed-directions)
                     (list +voxel-negative-x+
                           +voxel-negative-y+
                           +voxel-negative-z+)))))))

(define-test light-region-is-a-chunk-window-with-policy-free-availability
  (let ((world (make-block-world)))
    (luvcraft::ensure-world-chunk world 0 0 0)
    (let ((region (luvcraft::capture-light-region world)))
      (multiple-value-bind (entry offset availability)
          (locate-chunk-window-site region 0 0 0)
        (true entry)
        (true (zerop offset))
        (true (eq availability :available)))
      (multiple-value-bind (entry offset availability)
          (locate-chunk-window-site region 16 0 0)
        (true (null entry))
        (true (null offset))
        (true (eq availability :unavailable))))))
