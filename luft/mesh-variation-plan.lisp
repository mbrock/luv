(in-package #:luft)

;;; A transition plan is the small, owner-independent result of inspecting the
;;; complete witness closure.  Emission consumes it without rediscovering
;;; collapse neighborhoods or weakening the canonical owner boundary.
(defstruct (%bevel-transition-plan
             (:constructor %make-bevel-transition-plan
                 (repair-splits repair-source-filter candidate-splits
                  collapsed-triangle-count context-collapsed-triangle-count
                  unmatched-edge-count residual-edge-count
                  repair-point-count)))
  (repair-splits nil :type hash-table :read-only t)
  (repair-source-filter nil :read-only t)
  (candidate-splits nil :type hash-table :read-only t)
  (collapsed-triangle-count 0 :type fixnum :read-only t)
  (context-collapsed-triangle-count 0 :type fixnum :read-only t)
  (unmatched-edge-count 0 :type fixnum :read-only t)
  (residual-edge-count 0 :type fixnum :read-only t)
  (repair-point-count 0 :type fixnum :read-only t))

(defun %plan-variable-bevel-transitions
    (field owner-witnesses output-witnesses output-set realize-set
     live-triangle-counts-by-owner packing contract-t-junctions-p)
  "Discover and prove the complete local contraction plan for one cohort."
  ;; Only a three-distinct-point collinear collapse can create the long-edge /
  ;; two-short-edge mismatch.  Count that exact local edge neighborhood, not
  ;; every edge in the otherwise closed witness.
  (let ((all-candidate-splits (make-hash-table :test #'eql))
        (relevant-candidate-edges (make-hash-table :test #'eql))
        (candidate-splits (make-hash-table :test #'eql))
        (queried-edge-counts (make-hash-table :test #'eql))
        (repair-splits (make-hash-table :test #'eql))
        (queried-source-filter nil)
        (repair-source-filter nil)
        (collapsed-triangle-count 0)
        (context-collapsed-triangle-count 0))
    (%with-bevel-site-width-field (site-width) field
      (labels ((spatial-edge-key (lx ly lz rx ry rz)
                 (nth-value 0
                   (%pack-spatial-edge packing lx ly lz rx ry rz)))
               (packed-point-edge-key (left right)
                 (spatial-edge-key
                  (%global-mesh-point-x left)
                  (%global-mesh-point-y left)
                  (%global-mesh-point-z left)
                  (%global-mesh-point-x right)
                  (%global-mesh-point-y right)
                  (%global-mesh-point-z right)))
               (record-candidate (owner lx ly lz rx ry rz middle)
                 (let ((edge (spatial-edge-key lx ly lz rx ry rz)))
                   (pushnew middle (gethash edge all-candidate-splits) :test #'=)
                   (when (nth-value 1 (gethash owner output-set))
                     (setf (gethash edge relevant-candidate-edges) t))))
               (discover-transition
                   (owner kind ax ay az bx by bz cx cy cz)
                 (%with-transformed-bevel-triangle
                     (tax tay taz tbx tby tbz tcx tcy tcz)
                     site-width ax ay az bx by bz cx cy cz
                   (multiple-value-bind (nx ny nz)
                       (%triangle-cross-scalars
                        tax tay taz tbx tby tbz tcx tcy tcz)
                     (if (and (zerop nx) (zerop ny) (zerop nz))
                         (progn
                           (incf context-collapsed-triangle-count)
                           (when (nth-value 1 (gethash owner output-set))
                             (incf collapsed-triangle-count))
                           (unless
                               (or (and (= tax tbx) (= tay tby) (= taz tbz))
                                   (and (= tbx tcx) (= tby tcy) (= tbz tcz))
                                   (and (= tcx tax) (= tcy tay) (= tcz taz)))
                             (let* ((pa (%pack-global-mesh-point tax tay taz))
                                    (pb (%pack-global-mesh-point tbx tby tbz))
                                    (pc (%pack-global-mesh-point tcx tcy tcz))
                                    (ab
                                      (%global-mesh-point-distance-squared pa pb))
                                    (bc
                                      (%global-mesh-point-distance-squared pb pc))
                                    (ca
                                      (%global-mesh-point-distance-squared pc pa)))
                               ;; Match the reference reduction's later-edge
                               ;; tie preference, though a collinear interior
                               ;; point gives the long edge a strict maximum.
                               (if (> ab bc)
                                   (if (> ab ca)
                                       (record-candidate
                                        owner tax tay taz tbx tby tbz pc)
                                       (record-candidate
                                        owner tcx tcy tcz tax tay taz pb))
                                   (if (> bc ca)
                                       (record-candidate
                                        owner tbx tby tbz tcx tcy tcz pa)
                                       (record-candidate
                                        owner tcx tcy tcz tax tay taz pb))))))
                         (when (nth-value 1 (gethash owner realize-set))
                           (incf
                            (aref (gethash owner
                                           live-triangle-counts-by-owner)
                                  (ecase kind
                                    (:face 0)
                                    (:band 1)
                                    (:junction 2)))))))))
               (mark-candidate-edge (lx ly lz rx ry rz)
                 (let ((edge (spatial-edge-key lx ly lz rx ry rz)))
                   (when (nth-value 1
                            (gethash edge all-candidate-splits))
                     (setf (gethash edge relevant-candidate-edges) t))))
               (mark-output-candidate-edges
                   (ax ay az bx by bz cx cy cz)
                 (%with-transformed-bevel-triangle
                     (tax tay taz tbx tby tbz tcx tcy tcz)
                     site-width ax ay az bx by bz cx cy cz
                   (multiple-value-bind (nx ny nz)
                       (%triangle-cross-scalars
                        tax tay taz tbx tby tbz tcx tcy tcz)
                     (unless (and (zerop nx) (zerop ny) (zerop nz))
                       (mark-candidate-edge tax tay taz tbx tby tbz)
                       (mark-candidate-edge tbx tby tbz tcx tcy tcz)
                       (mark-candidate-edge tcx tcy tcz tax tay taz)))))
               (count-queried-edge (lx ly lz rx ry rz)
                 (let ((key (spatial-edge-key lx ly lz rx ry rz)))
                   (multiple-value-bind (count present-p)
                       (gethash key queried-edge-counts)
                     (when present-p
                       (setf (gethash key queried-edge-counts) (1+ count))))))
               (scan-queried-transition (ax ay az bx by bz cx cy cz)
                 (%with-transformed-bevel-triangle
                     (tax tay taz tbx tby tbz tcx tcy tcz)
                     site-width ax ay az bx by bz cx cy cz
                   (multiple-value-bind (nx ny nz)
                       (%triangle-cross-scalars
                        tax tay taz tbx tby tbz tcx tcy tcz)
                     (unless (and (zerop nx) (zerop ny) (zerop nz))
                       (count-queried-edge tax tay taz tbx tby tbz)
                       (count-queried-edge tbx tby tbz tcx tcy tcz)
                       (count-queried-edge tcx tcy tcz tax tay taz))))))
        (declare (inline spatial-edge-key packed-point-edge-key
                         record-candidate mark-candidate-edge
                         count-queried-edge))
        (dolist (owner-witness owner-witnesses)
          (let ((owner (car owner-witness))
                (witness (cdr owner-witness)))
            (%do-surface-mesh-triangle-scalars
                (witness kind stock ambient mask
                         ax ay az bx by bz cx cy cz)
              (declare (ignore stock ambient mask))
              (discover-transition
               owner kind ax ay az bx by bz cx cy cz))))
        ;; A guard-owned collapse can require a repair child in an output mesh.
        ;; Mark candidates whose surviving long edge occurs on an output-owned
        ;; source triangle as well as candidates created by output collapses.
        (dolist (owner-witness output-witnesses)
          (let ((witness (cdr owner-witness)))
            (%do-surface-mesh-triangle-scalars
                (witness kind stock ambient mask
                         ax ay az bx by bz cx cy cz)
              (declare (ignore kind stock ambient mask))
              (mark-output-candidate-edges
               ax ay az bx by bz cx cy cz))))
        (maphash
         (lambda (edge points)
           (when (nth-value 1 (gethash edge relevant-candidate-edges))
             (setf (gethash edge candidate-splits) points)))
         all-candidate-splits)
        (setf queried-edge-counts
              (make-hash-table
               :test #'eql
               :size (max 16 (* 3 (hash-table-count candidate-splits)))))
        (maphash
         (lambda (edge points)
           (multiple-value-bind (left right)
               (%spatial-edge-points packing edge)
             (let* ((left-packed
                      (%pack-global-mesh-point
                       (first left) (second left) (third left)))
                    (right-packed
                      (%pack-global-mesh-point
                       (first right) (second right) (third right)))
                    (points
                      (sort points #'<
                            :key (lambda (point)
                                   (%global-mesh-point-distance-squared
                                    left-packed point)))))
               (setf (gethash edge candidate-splits) points
                     (gethash edge queried-edge-counts) 0)
               (loop with previous = left-packed
                     for point in points
                     do (setf (gethash (packed-point-edge-key previous point)
                                       queried-edge-counts)
                              0
                              previous point)
                     finally
                        (setf (gethash
                               (packed-point-edge-key previous right-packed)
                               queried-edge-counts)
                              0)))))
         candidate-splits)
        (setf queried-source-filter
              (%make-source-anchor-filter packing queried-edge-counts))
        (when queried-source-filter
          (dolist (owner-witness owner-witnesses)
            (let ((witness (cdr owner-witness)))
              (%do-surface-mesh-triangle-scalars
                  (witness kind stock ambient mask
                           ax ay az bx by bz cx cy cz)
                (declare (ignore kind stock ambient mask))
                (when (%triangle-touches-source-anchor-filter-p
                       queried-source-filter ax ay az bx by bz cx cy cz)
                  (scan-queried-transition
                   ax ay az bx by bz cx cy cz))))))
        (let ((unmatched-edge-count
                (loop for count being the hash-values of queried-edge-counts
                      count (not (or (zerop count) (= count 2))))))
          (when contract-t-junctions-p
            (maphash
             (lambda (edge points)
               (when (= 1 (gethash edge queried-edge-counts 0))
                 (multiple-value-bind (left right)
                     (%spatial-edge-points packing edge)
                   (let ((left
                           (%pack-global-mesh-point
                            (first left) (second left) (third left)))
                         (right
                           (%pack-global-mesh-point
                            (first right) (second right) (third right))))
                     (when (loop with previous = left
                                 for point in points
                                 always (= 1 (gethash
                                              (packed-point-edge-key
                                               previous point)
                                              queried-edge-counts 0))
                                 do (setf previous point)
                                 finally
                                    (return
                                      (= 1 (gethash
                                            (packed-point-edge-key
                                             previous right)
                                            queried-edge-counts 0))))
                       (setf (gethash edge repair-splits) points))))))
             candidate-splits)
            (setf repair-source-filter
                  (%make-source-anchor-filter packing repair-splits))
            ;; Prove that the selected contractions account for the entire
            ;; mismatch before changing any triangles.  A new failure mode
            ;; must become explicit rather than rendering another hairline
            ;; crack.
            (maphash
             (lambda (edge points)
               (decf (gethash edge queried-edge-counts))
               (multiple-value-bind (left right)
                   (%spatial-edge-points packing edge)
                 (let ((left
                         (%pack-global-mesh-point
                          (first left) (second left) (third left)))
                       (right
                         (%pack-global-mesh-point
                          (first right) (second right) (third right))))
                   (loop with previous = left
                         for point in points
                         do (incf (gethash
                                   (packed-point-edge-key previous point)
                                   queried-edge-counts 0))
                            (setf previous point)
                         finally
                            (incf (gethash
                                   (packed-point-edge-key previous right)
                                   queried-edge-counts 0))))))
             repair-splits))
          (let ((residual-edge-count
                  (loop for count being the hash-values of queried-edge-counts
                        count (not (or (zerop count) (= count 2))))))
            (when (and contract-t-junctions-p
                       (plusp residual-edge-count))
              (error "Site-local bevel contraction left ~D of ~D unmatched edges after ~D repairs."
                     residual-edge-count unmatched-edge-count
                     (hash-table-count repair-splits)))
            (%make-bevel-transition-plan
             repair-splits
             repair-source-filter
             candidate-splits
             collapsed-triangle-count
             context-collapsed-triangle-count
             unmatched-edge-count
             residual-edge-count
             (loop for points being the hash-values of repair-splits
                   sum (length points)))))))))
