(in-package #:luft)

(defun %coplanar-group-loops (triangles)
  "Return oriented boundary loops, or NIL when the union is not simple."
  (let ((edges (make-hash-table :test #'equal)))
    (dolist (triangle triangles)
      (destructuring-bind (mask a b c) triangle
        (declare (ignore mask))
        (dolist (edge (list (list a b) (list b c) (list c a)))
          (let ((key (%ordered-point-edge (first edge) (second edge))))
            (if (gethash key edges)
                (remhash key edges)
                (setf (gethash key edges) edge))))))
    (let ((next (make-hash-table :test #'equal))
          (incoming (make-hash-table :test #'equal)))
      (loop for edge being the hash-values of edges do
        (destructuring-bind (start end) edge
          (when (gethash start next)
            (return-from %coplanar-group-loops nil))
          (setf (gethash start next) end)
          (incf (gethash end incoming 0))))
      (loop for start being the hash-keys of next
            unless (= 1 (gethash start incoming 0))
              do (return-from %coplanar-group-loops nil))
      (let ((loops nil))
        (loop while (plusp (hash-table-count next)) do
          (let* ((start (sort (loop for point being the hash-keys of next
                                    collect point)
                              #'%point-order<))
                 (start (first start))
                 (point start)
                 (loop nil))
            (loop do (push point loop)
                     (multiple-value-bind (following present-p)
                         (gethash point next)
                       (unless present-p
                         (return-from %coplanar-group-loops nil))
                       (remhash point next)
                       (setf point following))
                  until (equal point start))
            (push (nreverse loop) loops)))
        (nreverse loops)))))

(defun %point-in-oriented-triangle-p (point a b c normal)
  (and (not (member point (list a b c) :test #'equal))
       (>= (%point-dot (%point-cross a b point) normal) 0)
       (>= (%point-dot (%point-cross b c point) normal) 0)
       (>= (%point-dot (%point-cross c a point) normal) 0)))

(defun %triangulate-coplanar-loop (loop normal)
  "Ear-clip one positively oriented simple integer polygon."
  ;; Retain collinear boundary vertices: another coplanar attribute group or
  ;; differently oriented plane may meet there. Removing such a vertex would
  ;; preserve the continuous surface but introduce a topological T-junction.
  (let ((points loop)
        (triangles nil))
    (when (< (length points) 3)
      (return-from %triangulate-coplanar-loop nil))
    (loop while (> (length points) 3) do
      (let ((ear-index nil)
            (count (length points)))
        (dotimes (index count)
          (let ((a (nth (mod (1- index) count) points))
                (b (nth index points))
                (c (nth (mod (1+ index) count) points)))
            (when (and (plusp (%point-dot (%point-cross a b c) normal))
                       (notany (lambda (point)
                                 (%point-in-oriented-triangle-p
                                  point a b c normal))
                               points))
              (setf ear-index index)
              (return))))
        (unless ear-index
          (return-from %triangulate-coplanar-loop nil))
        (let* ((count (length points))
               (a (nth (mod (1- ear-index) count) points))
               (b (nth ear-index points))
               (c (nth (mod (1+ ear-index) count) points)))
          (push (list a b c) triangles)
          (setf points
                (loop for point in points
                      for index from 0
                      unless (= index ear-index) collect point)))))
    (push points triangles)
    (nreverse triangles)))

(defun %emit-global-triangle (builder kind stock ambient mask normal triangle)
  (destructuring-bind (a b c) triangle
    (let* ((minimums
             (loop for axis below 3
                   collect (min (nth axis a) (nth axis b) (nth axis c))))
           (base (mapcar (lambda (coordinate)
                           (floor coordinate +mesh-cell-size+))
                         minimums))
           (origin (mapcar (lambda (coordinate)
                             (* coordinate +mesh-cell-size+))
                           base))
           (scratch (surface-mesh-builder-vertex-scratch builder)))
      (%scratch-triangle
       scratch 0 (ecase kind (:face 0) (:band 1) (:junction 2)) mask
       (- (first a) (first origin))
       (- (second a) (second origin))
       (- (third a) (third origin))
       (- (first b) (first origin))
       (- (second b) (second origin))
       (- (third b) (third origin))
       (- (first c) (first origin))
       (- (second c) (second origin))
       (- (third c) (third origin))
       (first normal) (second normal) (third normal))
      (%emit-instance builder kind (first base) (second base) (third base)
                      stock ambient 3))))

(defun surface-mesh-with-triangle-ink (mesh)
  "Return MESH's exact triangles with every primitive edge marked visible.

The geometry, stock, ambient value, primitive class, and winding are retained.
Only the three construction-mask bits change.  This diagnostic realization
exposes connectivity that the ordinary semantic edge mask intentionally hides;
it must not be substituted for the production mesh outside topology captures."
  (check-type mesh surface-mesh)
  (let ((builder (%make-surface-mesh-builder
                  (surface-mesh-domain mesh) (surface-mesh-bevel-width mesh))))
    (setf (surface-mesh-builder-singular-star-count builder)
          (surface-mesh-singular-star-count mesh))
    (%map-surface-mesh-triangle-records
     (lambda (kind stock ambient mask normal a b c)
       (declare (ignore mask))
       (%emit-global-triangle builder kind stock ambient #b111 normal
                              (list a b c)))
     mesh)
    (%finish-surface-mesh builder)))

(defun surface-mesh-split-neighborhood (mesh split)
  "Return only the triangles incident to the three points in SPLIT.

SPLIT is (LEFT MIDDLE RIGHT), as reported in the :CANDIDATE-SPLITS bevel
diagnostic.  A triangle is retained when two or more of its vertices are
split points, so the result is the smallest actual mesh patch that contrasts
one long edge with its two short neighbours.  Every retained edge is marked
visible; no vertex or triangle geometry is otherwise changed.  This is the
executable closeup used by the mixed-bevel degeneracy atlas. #WSEK3C"
  (check-type mesh surface-mesh)
  (unless (and (listp split) (= 3 (length split)))
    (error "A mesh split neighborhood needs (LEFT MIDDLE RIGHT), not ~S."
           split))
  (let ((builder (%make-surface-mesh-builder
                  (surface-mesh-domain mesh) (surface-mesh-bevel-width mesh))))
    (%map-surface-mesh-triangle-records
     (lambda (kind stock ambient mask normal a b c)
       (declare (ignore mask))
       (when (>= (count-if (lambda (point)
                             (member point split :test #'equal))
                           (list a b c))
                 2)
         (%emit-global-triangle builder kind stock ambient #b111 normal
                                (list a b c))))
     mesh)
    (%finish-surface-mesh builder)))

(defun %coplanar-group-key< (left right)
  (flet ((numeric-key (key)
           (destructuring-bind (kind stock ambient normal plane) key
             (list (ecase kind (:face 0) (:band 1) (:junction 2))
                   stock ambient
                   (first normal) (second normal) (third normal) plane))))
    (loop for l in (numeric-key left)
          for r in (numeric-key right)
          when (/= l r) return (< l r)
          finally (return nil))))

(defun %coplanar-merged-surface-mesh (mesh)
  "Dissolve only interior edges between exactly coplanar equal-attribute faces.

The output has the same points, oriented planes, stocks, ambient values,
silhouette, and depth as MESH. Groups with a non-simple boundary retain their
original triangles, making the unmerged medial mesh a local rebuild oracle."
  (let ((groups (make-hash-table :test #'equal))
        (builder (%make-surface-mesh-builder
                  (surface-mesh-domain mesh) (surface-mesh-bevel-width mesh))))
    (%map-surface-mesh-triangle-records
     (lambda (kind stock ambient mask normal a b c)
       (let ((key (list kind stock ambient normal (%point-dot normal a))))
         (push (list mask a b c) (gethash key groups))))
     mesh)
    (dolist (key (sort (loop for key being the hash-keys of groups collect key)
                       #'%coplanar-group-key<))
      (let ((triangles (gethash key groups)))
        (destructuring-bind (kind stock ambient normal plane) key
               (declare (ignore plane))
               (let ((loops (%coplanar-group-loops triangles))
                     (merged nil)
                     (boundary (make-hash-table :test #'equal)))
                 (when loops
                   (dolist (loop loops)
                     (loop for point on loop
                           for a = (first point)
                           for b = (or (second point) (first loop))
                           do (setf (gethash (%ordered-point-edge a b) boundary)
                                    t)))
                   (setf merged
                         (loop for loop in loops
                               when (plusp
                                     (loop for point on loop
                                           for a = (first point)
                                           for b = (or (second point)
                                                       (first loop))
                                           sum (%point-dot
                                                (%point-cross '(0 0 0) a b)
                                                normal)))
                                 append (%triangulate-coplanar-loop loop normal)
                               else do (setf loops nil))))
                 (if (and loops merged)
                     (dolist (triangle merged)
                       (destructuring-bind (a b c) triangle
                         (let ((mask
                                 (logior
                                  (if (gethash (%ordered-point-edge b c)
                                               boundary) #b001 0)
                                  (if (gethash (%ordered-point-edge c a)
                                               boundary) #b010 0)
                                  (if (gethash (%ordered-point-edge a b)
                                               boundary) #b100 0))))
                           (%emit-global-triangle builder kind stock ambient
                                                  mask normal triangle))))
                     (dolist (triangle triangles)
                       (%emit-global-triangle builder kind stock ambient
                                              (first triangle) normal
                                              (rest triangle))))))))
    (setf (surface-mesh-builder-singular-star-count builder)
          (surface-mesh-singular-star-count mesh))
    (%finish-surface-mesh builder)))

(defun coplanar-compressed-surface-mesh (mesh)
  "Return MESH with only exact coplanar interior edges dissolved.

This is an explicit cold compression/diagnostic transform, never an alternate
mode of MESH-CHUNK or the variable-width topology producer.  MESH is not
modified; groups whose boundary is not simple retain their original
triangles."
  (check-type mesh surface-mesh)
  (%coplanar-merged-surface-mesh mesh))
