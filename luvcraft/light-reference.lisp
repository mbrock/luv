;;; The deliberately simple voxel-light oracle and its differential harness.
;;; This file is loaded only by the explicit LUVCRAFT/LIGHT-REFERENCE system;
;;; no reference implementation participates in production solver dispatch.

(defpackage #:luvcraft.light-reference
  (:use #:cl #:luv)
  (:export #:compare-voxel-light-solvers
           #:voxel-light-solver-comparison
           #:voxel-light-solver-comparison-candidate-bytes-consed
           #:voxel-light-solver-comparison-candidate-executions
           #:voxel-light-solver-comparison-candidate-garbage-collections
           #:voxel-light-solver-comparison-candidate-gc-seconds
           #:voxel-light-solver-comparison-candidate-seconds
           #:voxel-light-solver-comparison-candidate-solver
           #:voxel-light-solver-comparison-candidate-visits
           #:voxel-light-solver-comparison-equal-p
           #:voxel-light-solver-comparison-legacy-bytes-consed
           #:voxel-light-solver-comparison-legacy-garbage-collections
           #:voxel-light-solver-comparison-legacy-gc-seconds
           #:voxel-light-solver-comparison-legacy-seconds
           #:voxel-light-solver-comparison-legacy-visits
           #:voxel-light-solver-comparison-mismatched-keys))

(in-package #:luvcraft)

(records:define-columnar-buffer light-worklist-bucket
  ;; ENTRY supplies the semantic chunk boundary, while OFFSET and LEVEL are
  ;; unboxed site data.  The generated buffer owns their shared extent.
  (entry nil :type (or null light-region-entry) :clear-on-remove t)
  (offset 0 :type (unsigned-byte 32))
  (level 0 :type (unsigned-byte 8)))

(defstruct (light-worklist (:constructor %make-light-worklist))
  (scheduling :lifo :type (member :lifo :level) :read-only t)
  (field-definition nil
                    :type (or null
                              luvcraft.world.fields:voxel-field-definition)
                    :read-only t)
  (buckets #() :type simple-vector :read-only t)
  (maximum-level -1 :type fixnum)
  (count 0 :type fixnum))

(defun make-light-worklist (&key (scheduling :lifo) field-definition)
  "Make the reference solver's packed LIFO or level-scheduled worklist.

This machinery belongs to the test-only voxel-light oracle.  Both modes
retain ENTRY plus dense OFFSET and LEVEL lanes, never a cons or coordinate
object per item. #QS1ERH #LDP5UR"
  (check-type scheduling (member :lifo :level))
  (check-type field-definition
              (or null luvcraft.world.fields:voxel-field-definition))
  (let* ((layout-definition
           (records:columnar-layout-definition-for 'light-worklist-bucket))
         (row-declaration
           (records:make-columnar-row-declaration
            layout-definition
            (and field-definition `((level . ,field-definition)))))
         (buckets (make-array (1+ +maximum-light-level+))))
    (dotimes (level (length buckets))
      (setf (aref buckets level)
            (make-light-worklist-bucket
             :capacity 256 :row-declaration row-declaration)))
    (%make-light-worklist
     :scheduling scheduling :field-definition field-definition
     :buckets buckets)))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((worklist light-worklist) field-name)
  (let ((definition (light-worklist-field-definition worklist)))
    (and definition
         (eq field-name
             (luvcraft.world.fields:voxel-field-definition-name definition))
         definition)))

(declaim (inline light-worklist-empty-p light-worklist-push))
(defun light-worklist-empty-p (worklist)
  (zerop (light-worklist-count worklist)))

(defun light-worklist-push (worklist entry offset level)
  "Push ENTRY, domain OFFSET, and propagation LEVEL onto WORKLIST."
  (check-type entry light-region-entry)
  (check-type offset (unsigned-byte 32))
  (check-type level (unsigned-byte 8))
  (let* ((bucket-level
           (ecase (light-worklist-scheduling worklist)
             (:lifo 0)
             (:level level)))
         (bucket (aref (light-worklist-buckets worklist) bucket-level)))
    (light-worklist-bucket-push bucket entry offset level)
    (incf (light-worklist-count worklist))
    (setf (light-worklist-maximum-level worklist)
          (max bucket-level (light-worklist-maximum-level worklist)))
    worklist))

(defun light-worklist-pop (worklist)
  "Pop (VALUES ENTRY OFFSET LEVEL PRESENT-P), clearing retained ENTRY."
  (when (light-worklist-empty-p worklist)
    (return-from light-worklist-pop (values nil nil nil nil)))
  (let* ((bucket-level (light-worklist-maximum-level worklist))
         (bucket (aref (light-worklist-buckets worklist) bucket-level)))
    (multiple-value-bind (entry offset level present-p)
        (light-worklist-bucket-pop bucket)
      (unless present-p
        (error "Lighting worklist count disagrees with bucket ~D." bucket-level))
      (decf (light-worklist-count worklist))
      (when (zerop (light-worklist-bucket-length bucket))
        (loop for candidate downfrom (1- bucket-level) to 0
              when (plusp
                    (light-worklist-bucket-length
                     (aref (light-worklist-buckets worklist) candidate)))
                do (setf (light-worklist-maximum-level worklist) candidate)
                   (return)
              finally (setf (light-worklist-maximum-level worklist) -1)))
      (values entry offset level t))))

(defun propagate-light-region (region field-reader queue skylight-p)
  "Run the reference FIELD's max-fixpoint propagation from packed QUEUE sites."
  (let ((visited 0))
    (with-cpu-trace-zone
        (:lighting/legacy/drain-sites :tracy-value visited)
      (loop until (light-worklist-empty-p queue)
            do (multiple-value-bind (entry offset queued-level present-p)
                   (light-worklist-pop queue)
                 (declare (ignore queued-level present-p))
                 (let* ((domain (light-region-entry-domain entry))
                        (local (chunk-domain-local-coordinate domain offset)))
                   (declare (dynamic-extent local))
                   (incf visited)
                   (let ((level (aref (funcall field-reader entry) offset)))
                     (when (plusp level)
                       (do-chunk-window-neighbors
                           (neighbor-offset destination crossing direction
                            materialization availability
                            region domain local *voxel-face-directions*)
                         (values destination)
                         (let ((neighbor
                                 (ecase availability
                                   (:local entry)
                                   (:available materialization)
                                   (:unavailable nil))))
                           (when neighbor
                             (let* ((opacity
                                      (light-region-opacity
                                       neighbor neighbor-offset))
                                    (loss
                                      (if (and skylight-p
                                               (eq direction
                                                   +voxel-negative-y+))
                                          opacity
                                          (+ 1 opacity)))
                                    (candidate (- level loss))
                                    (levels (funcall field-reader neighbor)))
                               (when (> candidate
                                        (aref levels neighbor-offset))
                                 (setf (aref levels neighbor-offset) candidate)
                                 (light-worklist-push
                                  queue neighbor neighbor-offset
                                  candidate))))))))))))
    visited))

(defun seed-reference-open-sky-at-offset (entry offset downward-p)
  (let* ((opacity (light-region-opacity entry offset))
         (level (- +maximum-light-level+
                   (if downward-p opacity (+ 1 opacity)))))
    (when (> level (aref (light-region-entry-sky entry) offset))
      (setf (aref (light-region-entry-sky entry) offset) level)
      t)))

(defun seed-reference-open-boundaries (region key entry enqueue)
  (dolist (direction *voxel-face-directions*)
    (when (and (not (light-region-neighbor-resident-p region key direction))
               (eq (light-region-absent-boundary-semantics
                    region key direction)
                   :open-sky))
      (map-entry-face-sites
       entry direction
       (lambda (offset local)
         (values local)
         (when (seed-reference-open-sky-at-offset
                entry offset (eq direction +voxel-positive-y+))
           (funcall enqueue entry offset
                    (aref (light-region-entry-sky entry) offset))))))))

(defun seed-reference-sky-boundaries (region &key (scheduling :level))
  (let ((queue
          (make-light-worklist
           :scheduling scheduling
           :field-definition (fields:field-definition-for :sky-light))))
    (maphash (lambda (key entry)
               (seed-reference-open-boundaries
                region key entry
                (lambda (seed-entry offset level)
                  (light-worklist-push
                   queue seed-entry offset level))))
             (light-region-entries region))
    queue))

(defun seed-reference-emitters (region &key (scheduling :level))
  (let ((queue
          (make-light-worklist
           :scheduling scheduling
           :field-definition (fields:field-definition-for :block-light))))
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
               (light-worklist-push queue entry offset level))))))
     (light-region-entries region))
    queue))

(defun solve-light-region (region &key (scheduling :level))
  "Solve REGION with the explicit test/reference voxel-light oracle."
  (with-cpu-trace-zone (:lighting/legacy)
    (let* ((sky-worklist
             (with-cpu-trace-zone (:lighting/legacy/seed-sky)
               (seed-reference-sky-boundaries
                region :scheduling scheduling)))
           (sky-visits
             (with-cpu-trace-zone (:lighting/legacy/propagate-sky)
               (propagate-light-region
                region #'light-region-entry-sky sky-worklist t)))
           (block-worklist
             (with-cpu-trace-zone (:lighting/legacy/seed-block)
               (seed-reference-emitters region :scheduling scheduling)))
           (block-visits
             (with-cpu-trace-zone (:lighting/legacy/propagate-block)
               (propagate-light-region
                region #'light-region-entry-block block-worklist nil))))
      (values region (+ sky-visits block-visits)))))

(in-package #:luvcraft.light-reference)

(defstruct voxel-light-solver-comparison
  (equal-p nil :type boolean)
  (mismatched-keys nil :type list)
  (legacy-visits 0 :type (integer 0 #.most-positive-fixnum))
  (candidate-visits 0 :type (integer 0 #.most-positive-fixnum))
  (legacy-seconds 0d0 :type double-float)
  (candidate-seconds 0d0 :type double-float)
  (legacy-bytes-consed 0 :type integer)
  (candidate-bytes-consed 0 :type integer)
  (legacy-gc-seconds 0d0 :type double-float)
  (candidate-gc-seconds 0d0 :type double-float)
  (legacy-garbage-collections 0 :type fixnum)
  (candidate-garbage-collections 0 :type fixnum)
  (candidate-executions nil :type list)
  (candidate-solver :compiled :type keyword))

(defun light-regions-mismatched-keys (left right)
  (let ((mismatches nil))
    (maphash
     (lambda (key left-entry)
       (let ((right-entry
               (gethash key (luvcraft::light-region-entries right))))
         (unless (and right-entry
                      (equalp (luvcraft::light-region-entry-sky left-entry)
                              (luvcraft::light-region-entry-sky right-entry))
                      (equalp (luvcraft::light-region-entry-block left-entry)
                              (luvcraft::light-region-entry-block right-entry)))
           (push key mismatches))))
     (luvcraft::light-region-entries left))
    (maphash
     (lambda (key right-entry)
       (declare (ignore right-entry))
       (unless (gethash key (luvcraft::light-region-entries left))
         (pushnew key mismatches :test #'equalp)))
     (luvcraft::light-region-entries right))
    (nreverse mismatches)))

(defun compare-voxel-light-solvers (world &key (candidate :compiled))
  "Compare the test-only legacy oracle with production CANDIDATE over WORLD.

Equivalent captures are solved without publication.  The result reports exact
per-chunk array equality, work and runtime observations, and the candidate's
retained frontier executions.  Unsupported candidate names signal through
the production solver protocol."
  (with-cpu-trace-zone (:lighting/compare)
    (let ((legacy-region (luvcraft::capture-light-region world))
          (candidate-region (luvcraft::capture-light-region world))
          (legacy-visits 0)
          (candidate-visits 0)
          (legacy-seconds 0d0)
          (candidate-seconds 0d0)
          (legacy-observation (make-runtime-observation))
          (candidate-observation (make-runtime-observation))
          (executions nil))
      (with-runtime-observation (legacy-observation)
        (multiple-value-setq (legacy-region legacy-visits)
          (luvcraft::solve-light-region legacy-region)))
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
      (with-runtime-observation (candidate-observation)
        (multiple-value-setq (candidate-region candidate-visits executions)
          (luvcraft:solve-light-region-using candidate candidate-region)))
      (setf candidate-seconds
            (runtime-observation-elapsed-seconds candidate-observation))
      (tracy-plot "lighting candidate allocated bytes"
                  (runtime-observation-bytes-consed candidate-observation))
      (tracy-plot "lighting candidate GC ms"
                  (* 1000d0
                     (runtime-observation-gc-seconds candidate-observation)))
      (tracy-plot "lighting candidate collections"
                  (runtime-observation-garbage-collections
                   candidate-observation))
      (let ((mismatches
              (light-regions-mismatched-keys
               legacy-region candidate-region)))
        (make-voxel-light-solver-comparison
         :equal-p (null mismatches)
         :mismatched-keys mismatches
         :legacy-visits legacy-visits
         :candidate-visits candidate-visits
         :legacy-seconds legacy-seconds
         :candidate-seconds candidate-seconds
         :legacy-bytes-consed
         (runtime-observation-bytes-consed legacy-observation)
         :candidate-bytes-consed
         (runtime-observation-bytes-consed candidate-observation)
         :legacy-gc-seconds
         (runtime-observation-gc-seconds legacy-observation)
         :candidate-gc-seconds
         (runtime-observation-gc-seconds candidate-observation)
         :legacy-garbage-collections
         (runtime-observation-garbage-collections legacy-observation)
         :candidate-garbage-collections
         (runtime-observation-garbage-collections candidate-observation)
         :candidate-executions executions
         :candidate-solver candidate)))))
