;;; Voxel light stated as a frontier program, beside the established oracle.

(in-package #:luvcraft)

(frontiers:define-frontier-program voxel-light-addition
  :family :monotone-max-fixpoint
  :frontier-layout :brightest-first-buckets
  :neighborhood :voxel-face-relations
  :materialization :sky-and-block-light-columns)

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
           'voxel-light-addition region frontier))
        (start (get-internal-real-time)))
    (unwind-protect
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
                      execution neighbor neighbor-offset candidate)))))))
      (setf (frontiers:frontier-execution-elapsed-seconds execution)
            (/ (- (get-internal-real-time) start)
               (coerce internal-time-units-per-second 'double-float))))))

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
  (frontier-executions nil :type list))

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

(defun compare-voxel-light-solvers (world)
  "Run legacy and frontier programs over equivalent captures of WORLD.

The result reports exact per-chunk array equality, visit counts, timings, and
the retained frontier executions.  Neither candidate is published. #DVUZ6H"
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
      (with-runtime-observation (frontier-observation)
        (multiple-value-setq (frontier-region frontier-visits executions)
          (solve-light-region-using :frontier frontier-region)))
      (setf frontier-seconds
            (runtime-observation-elapsed-seconds frontier-observation))
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
         :frontier-executions executions)))))
