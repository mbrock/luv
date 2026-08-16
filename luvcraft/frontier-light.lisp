;;; Voxel light stated as a frontier program, beside the established oracle.

(in-package #:luvcraft)

(frontiers:define-frontier-program voxel-light-addition
  :family :monotone-max-fixpoint
  :frontier-layout :brightest-first-buckets
  :neighborhood :voxel-face-relations
  :materialization :sky-and-block-light-columns
  ;; The local law of voxel light.  LEVEL is the relaxed best-known field,
  ;; bound to the sky or block lane at realization; OPACITY is the entered
  ;; cell's attenuation.  A relation transfers the source level minus the
  ;; propagation loss, admits a strict improvement, and schedules it at its
  ;; own brightness.  Those three are the monotone family's meaning; only the
  ;; transfer needs stating.
  :fields ((level :relaxed t) opacity)
  :constants (direct-direction)
  :predicates ((direct (direction= direct-direction)))
  :transfer (- (level source)
               (as-field-quantity level
                 (luvcraft.arithmetic:light-propagation-loss
                  (opacity target) direct))))

;;; Field bindings close the program over the light region's storage: each
;;; role names the lanes it borrows from a materialization (a light region
;;; entry) and how one offset is read or written through them.

(defun light-level-field-binding (field-name)
  (let ((lane-reader
          (ecase field-name
            (:sky-light 'light-region-entry-sky)
            (:block-light 'light-region-entry-block))))
    (frontiers:make-frontier-field-binding
     'level
     :declaration (fields:field-definition-for field-name)
     :lanes `((levels (,lane-reader materialization)
                      :type (simple-array (unsigned-byte 8) (*))))
     :read '(aref levels offset)
     :write '(setf (aref levels offset) value))))

(defun light-opacity-field-binding ()
  (frontiers:make-frontier-field-binding
   'opacity
   :declaration (represented-block-slot-declaration 'light-opacity)
   :lanes '((losses (light-region-entry-opacity-lut materialization)
                    :type (simple-array (unsigned-byte 8) (*)))
            (indices (light-region-entry-indices materialization)
                     :type (simple-array (unsigned-byte 16) (*))))
   :read '(aref losses (aref indices offset))))

(defvar *compiled-light-realizations* (make-hash-table :test #'eq)
  "Realizations of VOXEL-LIGHT-ADDITION per relaxed field name.")

(defun compiled-light-realization (field-name)
  "Return the current realization of the light program over FIELD-NAME."
  (let ((realization (gethash field-name *compiled-light-realizations*)))
    (if (and realization
             (frontiers:frontier-realization-current-p realization))
        realization
        (setf (gethash field-name *compiled-light-realizations*)
              (frontiers:compile-frontier-program
               'voxel-light-addition
               :bindings (list (light-level-field-binding field-name)
                               (light-opacity-field-binding))
               :site-domain '(light-region-entry-domain materialization))))))

(defun clear-compiled-light-realizations ()
  (clrhash *compiled-light-realizations*))

(defmethod frontiers:note-frontier-program-redefinition
    ((name (eql 'voxel-light-addition)))
  (declare (ignore name))
  (clear-compiled-light-realizations))

(defun make-light-frontier (field-name)
  (frontiers:make-bucket-frontier
   :maximum-priority +maximum-light-level+
   :priority-meaning (fields:field-definition-for field-name)))

(defun seed-frontier-sky-boundaries (region)
  "Seed open boundaries into a new brightest-first sky frontier."
  (let ((frontier (make-light-frontier :sky-light)))
    (maphash
     (lambda (key entry)
       (seed-entry-open-boundaries
        region key entry
        (lambda (seed-entry offset level)
          (frontiers:bucket-frontier-push
           frontier seed-entry offset level))))
     (light-region-entries region))
    frontier))

(defun seed-frontier-emitters (region)
  "Seed emitting cells into a new brightest-first block-light frontier."
  (let ((frontier (make-light-frontier :block-light)))
    (maphash
     (lambda (key entry)
       (declare (ignore key))
       (let ((indices (light-region-entry-indices entry))
             (emission (light-region-entry-emission-lut entry))
             (levels (light-region-entry-block entry))
             (domain (light-region-entry-domain entry)))
         (do-chunk-domain-sites (offset local domain)
           (values local)
           (let ((level (aref emission (aref indices offset))))
             (when (plusp level)
               (setf (aref levels offset) level)
               (frontiers:bucket-frontier-push
                frontier entry offset level))))))
     (light-region-entries region))
    frontier))

(defmethod frontiers:execute-frontier-program
    ((program (eql 'voxel-light-addition)) (region light-region)
     &key field-reader frontier skylight-p &allow-other-keys)
  "Realize voxel-light addition as one closed relation-and-transfer loop."
  (declare (ignore program))
  (let ((execution
          (frontiers:make-frontier-execution
           'voxel-light-addition region frontier)))
    (with-cpu-trace-zone
        (:lighting/frontier/drain-sites
         :tracy-value (frontiers:frontier-execution-visits execution))
      (frontiers:do-voxel-frontier-relations
          (entry offset queued-level
           neighbor neighbor-offset direction destination crossing
           availability frontier region (light-region-entry-domain entry)
           *voxel-face-directions*
           :execution execution :result execution)
        (values queued-level destination crossing availability)
        (when neighbor
          (let ((level (aref (funcall field-reader entry) offset)))
            (when (plusp level)
              (let* ((opacity
                       (light-region-opacity neighbor neighbor-offset))
                     (loss
                       (if (and skylight-p
                                (eq direction +voxel-negative-y+))
                           opacity
                           (+ 1 opacity)))
                     (candidate (- level loss))
                     (levels (funcall field-reader neighbor)))
                (when (> candidate (aref levels neighbor-offset))
                  (setf (aref levels neighbor-offset) candidate)
                  (frontiers:admit-frontier-site
                   execution neighbor neighbor-offset candidate))))))))))

(defun solve-frontier-light-region (region)
  "Solve REGION from scratch with the greenfield frontier-light program.

Milestone one deliberately implements only the monotone addition fixpoint.
The returned execution objects retain the declared program, frontier layout,
and traversal counters for inspection in a live image. #DVUZ6H"
  (let* ((sky-frontier
           (with-cpu-trace-zone (:lighting/frontier/seed-sky)
             (seed-frontier-sky-boundaries region)))
         (sky-execution
           (with-cpu-trace-zone (:lighting/frontier/propagate-sky)
             (frontiers:execute-frontier-program
              'voxel-light-addition region
              :field-reader #'light-region-entry-sky
              :frontier sky-frontier
              :skylight-p t)))
         (block-frontier
           (with-cpu-trace-zone (:lighting/frontier/seed-block)
             (seed-frontier-emitters region)))
         (block-execution
           (with-cpu-trace-zone (:lighting/frontier/propagate-block)
             (frontiers:execute-frontier-program
              'voxel-light-addition region
              :field-reader #'light-region-entry-block
              :frontier block-frontier
              :skylight-p nil)))
         (executions (list sky-execution block-execution)))
    (values region
            (reduce #'+ executions
                    :key #'frontiers:frontier-execution-visits)
            executions)))

(defmethod solve-light-region-using
    ((solver (eql :frontier)) (region light-region) &key &allow-other-keys)
  (declare (ignore solver))
  (with-cpu-trace-zone (:lighting/frontier)
    (solve-frontier-light-region region)))

;;; The compiled solver.  Seeds and drains both go through the realization:
;;; a sky boundary is a virtual source at full brightness whose loss follows
;;; the same law, and an emitter is a value joined into the block field.

(defun seed-compiled-sky-boundaries (realization region frontier execution)
  (maphash
   (lambda (key entry)
     (dolist (direction *voxel-face-directions*)
       (when (and (not (light-region-neighbor-resident-p region key direction))
                  (eq (light-region-absent-boundary-semantics
                       region key direction)
                      :open-sky))
         (let ((downward-p (eq direction +voxel-positive-y+)))
           (map-entry-face-sites
            entry direction
            (lambda (offset local)
              (values local)
              (frontiers:admit-frontier-realization-site
               realization region frontier execution entry offset
               (- +maximum-light-level+
                  (luvcraft.arithmetic:light-propagation-loss
                   (light-region-opacity entry offset) downward-p))
               :direct-direction +voxel-negative-y+)))))))
   (light-region-entries region)))

(defun seed-compiled-emitters (realization region frontier execution)
  (maphash
   (lambda (key entry)
     (declare (ignore key))
     (let ((indices (light-region-entry-indices entry))
           (emission (light-region-entry-emission-lut entry))
           (domain (light-region-entry-domain entry)))
       (do-chunk-domain-sites (offset local domain)
         (values local)
         (let ((level (aref emission (aref indices offset))))
           (when (plusp level)
             (frontiers:admit-frontier-realization-site
              realization region frontier execution entry offset level
              :direct-direction nil))))))
   (light-region-entries region)))

(defun solve-compiled-light-region (region)
  "Solve REGION from scratch with the compiled voxel-light realization. #PJY6E1"
  (let* ((sky (compiled-light-realization :sky-light))
         (block (compiled-light-realization :block-light))
         (sky-frontier (frontiers:make-realization-frontier sky))
         (block-frontier (frontiers:make-realization-frontier block))
         (sky-execution
           (frontiers:make-realization-execution sky region sky-frontier))
         (block-execution
           (frontiers:make-realization-execution block region block-frontier)))
    (with-cpu-trace-zone (:lighting/compiled/seed-sky)
      (seed-compiled-sky-boundaries sky region sky-frontier sky-execution))
    (with-cpu-trace-zone (:lighting/compiled/propagate-sky)
      (with-cpu-trace-zone
          (:lighting/compiled/drain-sites
           :tracy-value (frontiers:frontier-execution-visits sky-execution))
        (frontiers:drain-frontier-realization
         sky region sky-frontier sky-execution
         :direct-direction +voxel-negative-y+)))
    (with-cpu-trace-zone (:lighting/compiled/seed-block)
      (seed-compiled-emitters block region block-frontier block-execution))
    (with-cpu-trace-zone (:lighting/compiled/propagate-block)
      (with-cpu-trace-zone
          (:lighting/compiled/drain-sites
           :tracy-value (frontiers:frontier-execution-visits block-execution))
        (frontiers:drain-frontier-realization
         block region block-frontier block-execution
         :direct-direction nil)))
    (values region
            (+ (frontiers:frontier-execution-visits sky-execution)
               (frontiers:frontier-execution-visits block-execution))
            (list sky-execution block-execution))))

(defmethod solve-light-region-using
    ((solver (eql :compiled)) (region light-region) &key &allow-other-keys)
  (declare (ignore solver))
  (with-cpu-trace-zone (:lighting/compiled)
    (solve-compiled-light-region region)))

(defstruct voxel-light-solver-comparison
  (equal-p nil :type boolean)
  (mismatched-keys nil :type list)
  (legacy-visits 0 :type (integer 0 #.most-positive-fixnum))
  (frontier-visits 0 :type (integer 0 #.most-positive-fixnum))
  (legacy-seconds 0d0 :type double-float)
  (frontier-seconds 0d0 :type double-float)
  (legacy-bytes-consed 0 :type integer)
  (frontier-bytes-consed 0 :type integer)
  (legacy-gc-seconds 0d0 :type double-float)
  (frontier-gc-seconds 0d0 :type double-float)
  (legacy-garbage-collections 0 :type fixnum)
  (frontier-garbage-collections 0 :type fixnum)
  (frontier-executions nil :type list)
  (candidate-solver :frontier :type keyword))

(defun light-regions-mismatched-keys (left right)
  (let ((mismatches nil))
    (maphash
     (lambda (key left-entry)
       (let ((right-entry (gethash key (light-region-entries right))))
         (unless (and right-entry
                      (equalp (light-region-entry-sky left-entry)
                              (light-region-entry-sky right-entry))
                      (equalp (light-region-entry-block left-entry)
                              (light-region-entry-block right-entry)))
           (push key mismatches))))
     (light-region-entries left))
    (maphash
     (lambda (key right-entry)
       (declare (ignore right-entry))
       (unless (gethash key (light-region-entries left))
         (pushnew key mismatches :test #'equalp)))
     (light-region-entries right))
    (nreverse mismatches)))

(defun compare-voxel-light-solvers (world &key (candidate :frontier))
  "Run the legacy program and CANDIDATE over equivalent captures of WORLD.

CANDIDATE is :FRONTIER (the handwritten frontier lowering) or :COMPILED (the
compiled realization).  The result reports exact per-chunk array equality,
visit counts, timings, and the retained frontier executions in the FRONTIER-
slots.  Neither candidate is published. #DVUZ6H"
  (with-cpu-trace-zone (:lighting/compare)
    (let ((legacy-region (capture-light-region world))
          (frontier-region (capture-light-region world))
          (legacy-visits 0)
          (frontier-visits 0)
          (legacy-seconds 0d0)
          (frontier-seconds 0d0)
          (legacy-observation (make-runtime-observation))
          (frontier-observation (make-runtime-observation))
          (executions nil))
      (with-runtime-observation (legacy-observation)
        (multiple-value-setq (legacy-region legacy-visits)
          (solve-light-region-using :legacy legacy-region)))
      (setf legacy-seconds
            (runtime-observation-elapsed-seconds legacy-observation))
      (tracy-plot "lighting legacy allocated bytes"
                  (runtime-observation-bytes-consed legacy-observation))
      (tracy-plot "lighting legacy GC ms"
                  (* 1000d0
                     (runtime-observation-gc-seconds legacy-observation)))
      (tracy-plot "lighting legacy collections"
                  (runtime-observation-garbage-collections
                   legacy-observation))
      (with-runtime-observation (frontier-observation)
        (multiple-value-setq (frontier-region frontier-visits executions)
          (solve-light-region-using candidate frontier-region)))
      (setf frontier-seconds
            (runtime-observation-elapsed-seconds frontier-observation))
      (tracy-plot "lighting frontier allocated bytes"
                  (runtime-observation-bytes-consed frontier-observation))
      (tracy-plot "lighting frontier GC ms"
                  (* 1000d0
                     (runtime-observation-gc-seconds frontier-observation)))
      (tracy-plot "lighting frontier collections"
                  (runtime-observation-garbage-collections
                   frontier-observation))
      (let ((mismatches
              (light-regions-mismatched-keys legacy-region frontier-region)))
        (make-voxel-light-solver-comparison
         :equal-p (null mismatches)
         :mismatched-keys mismatches
         :legacy-visits legacy-visits
         :frontier-visits frontier-visits
         :legacy-seconds legacy-seconds
         :frontier-seconds frontier-seconds
         :legacy-bytes-consed
         (runtime-observation-bytes-consed legacy-observation)
         :frontier-bytes-consed
         (runtime-observation-bytes-consed frontier-observation)
         :legacy-gc-seconds
         (runtime-observation-gc-seconds legacy-observation)
         :frontier-gc-seconds
         (runtime-observation-gc-seconds frontier-observation)
         :legacy-garbage-collections
         (runtime-observation-garbage-collections legacy-observation)
         :frontier-garbage-collections
         (runtime-observation-garbage-collections frontier-observation)
         :frontier-executions executions
         :candidate-solver candidate)))))
