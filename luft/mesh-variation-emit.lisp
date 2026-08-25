(in-package #:luft)

(defun %emit-variable-bevel-transition-owners
    (field realized-witnesses output-set builder-by-owner
     live-triangle-counts-by-owner packing plan)
  "Emit one already-proved transition plan, preserving every source owner."
  (let ((repair-splits
          (%bevel-transition-plan-repair-splits plan))
        (repair-source-filter
          (%bevel-transition-plan-repair-source-filter plan))
        (repair-point-count
          (%bevel-transition-plan-repair-point-count plan)))
    (%with-bevel-site-width-field (site-width) field
      (labels ((spatial-edge-key (lx ly lz rx ry rz)
                 (nth-value 0
                   (%pack-spatial-edge packing lx ly lz rx ry rz)))
               (edge-splits (lx ly lz rx ry rz)
                 (let* ((edge (spatial-edge-key lx ly lz rx ry rz))
                        (points (gethash edge repair-splits)))
                   (when points
                     (let* ((left (%pack-global-mesh-point lx ly lz))
                            (right (%pack-global-mesh-point rx ry rz))
                            (ordered
                              (if (< left right) points (reverse points))))
                       (mapcar #'%global-mesh-point-list ordered)))))
               (edge-points (left right)
                 (append (list left)
                         (edge-splits
                          (first left) (second left) (third left)
                          (first right) (second right) (third right))
                         (list right)))
               (mark-visible-edge (table left right bit mask)
                 (when (logtest bit mask)
                   (loop for points on (edge-points left right)
                         while (rest points)
                         do (setf (gethash
                                   (%ordered-point-edge
                                    (first points) (second points))
                                   table)
                                  t))))
               (triangle-boundary-mask (visible a b c)
                 (logior (if (gethash (%ordered-point-edge b c) visible)
                             #b001
                             0)
                         (if (gethash (%ordered-point-edge c a) visible)
                             #b010
                             0)
                         (if (gethash (%ordered-point-edge a b) visible)
                             #b100
                             0)))
               (emit-transition
                   (builder kind stock ambient mask
                    ax ay az bx by bz cx cy cz
                    repair-neighborhood-p)
                 (%with-transformed-bevel-triangle
                     (tax tay taz tbx tby tbz tcx tcy tcz)
                     site-width ax ay az bx by bz cx cy cz
                   (multiple-value-bind (nx ny nz)
                       (%triangle-cross-scalars
                        tax tay taz tbx tby tbz tcx tcy tcz)
                     (unless (and (zerop nx) (zerop ny) (zerop nz))
                       (multiple-value-bind (onx ony onz)
                           (%triangle-cross-scalars
                            ax ay az bx by bz cx cy cz)
                         (unless
                             (plusp
                              (the fixnum
                                (+ (the fixnum (* nx onx))
                                   (the fixnum (* ny ony))
                                   (the fixnum (* nz onz)))))
                           (error "Site-local bevel folded ~S triangle ~S ~S ~S into ~S ~S ~S."
                                  kind
                                  (list ax ay az) (list bx by bz)
                                  (list cx cy cz)
                                  (list tax tay taz) (list tbx tby tbz)
                                  (list tcx tcy tcz))))
                       (let ((ab (when repair-neighborhood-p
                                   (edge-splits
                                    tax tay taz tbx tby tbz)))
                             (bc (when repair-neighborhood-p
                                   (edge-splits
                                    tbx tby tbz tcx tcy tcz)))
                             (ca (when repair-neighborhood-p
                                   (edge-splits
                                    tcx tcy tcz tax tay taz))))
                         (if (not (or ab bc ca))
                             (%emit-global-triangle-scalars
                              builder kind stock ambient mask nx ny nz
                              tax tay taz tbx tby tbz tcx tcy tcz)
                             (let* ((ta (list tax tay taz))
                                    (tb (list tbx tby tbz))
                                    (tc (list tcx tcy tcz))
                                    (cross (list nx ny nz))
                                    (loop
                                      (append (list ta) ab (list tb) bc
                                              (list tc) ca))
                                    (triangles
                                      (%triangulate-coplanar-loop loop cross))
                                    (visible
                                      (make-hash-table :test #'equal)))
                               (unless triangles
                                 (error "Could not contract site-local bevel T-junction around ~S."
                                        loop))
                               (mark-visible-edge visible ta tb #b100 mask)
                               (mark-visible-edge visible tb tc #b001 mask)
                               (mark-visible-edge visible tc ta #b010 mask)
                               (dolist (triangle triangles)
                                 (destructuring-bind (a b c) triangle
                                   (%emit-global-triangle
                                    builder kind stock ambient
                                    (triangle-boundary-mask visible a b c)
                                    (%primitive-plane-normal a b c)
                                    triangle)))))))))))
        (declare (inline spatial-edge-key edge-splits))
        (dolist (owner-witness realized-witnesses)
          (let* ((owner (car owner-witness))
                 (builder (gethash owner builder-by-owner))
                 (live-triangle-counts
                   (gethash owner live-triangle-counts-by-owner)))
            (%reserve-builder-triangle-capacities
             builder
             (+ (aref live-triangle-counts 0) repair-point-count)
             (+ (aref live-triangle-counts 1) repair-point-count)
             (+ (aref live-triangle-counts 2) repair-point-count))))
        (dolist (owner-witness realized-witnesses)
          (let* ((owner (car owner-witness))
                 (witness (cdr owner-witness))
                 (builder (gethash owner builder-by-owner)))
            (%do-surface-mesh-triangle-scalars
                (witness kind stock ambient mask
                         ax ay az bx by bz cx cy cz)
              (emit-transition
               builder kind stock ambient mask
               ax ay az bx by bz cx cy cz
               (and repair-source-filter
                    (%triangle-touches-source-anchor-filter-p
                     repair-source-filter
                     ax ay az bx by bz cx cy cz))))))
        (let ((realized
                (mapcar
                 (lambda (owner-witness)
                   (let ((owner (car owner-witness)))
                     (cons owner
                           (%finish-surface-mesh
                            (gethash owner builder-by-owner)))))
                 realized-witnesses)))
          (values
           (remove-if-not
            (lambda (entry)
              (nth-value 1 (gethash (car entry) output-set)))
            realized)
           (remove-if
            (lambda (entry)
              (nth-value 1 (gethash (car entry) output-set)))
            realized)))))))
