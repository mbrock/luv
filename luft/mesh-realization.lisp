(in-package #:luft)

;;; ---------------------------------------------------------------------------
;;; Exact coplanar compression

(defun %point-order< (left right)
  (loop for l in left
        for r in right
        when (/= l r) return (< l r)
        finally (return nil)))

(defun %ordered-point-edge (left right)
  (if (%point-order< left right)
      (list left right)
      (list right left)))

(defun %point-cross (a b c)
  (let ((ux (- (first b) (first a)))
        (uy (- (second b) (second a)))
        (uz (- (third b) (third a)))
        (vx (- (first c) (first a)))
        (vy (- (second c) (second a)))
        (vz (- (third c) (third a))))
    (list (- (* uy vz) (* uz vy))
          (- (* uz vx) (* ux vz))
          (- (* ux vy) (* uy vx)))))

(defun %point-dot (left right)
  (+ (* (first left) (first right))
     (* (second left) (second right))
     (* (third left) (third right))))

(defun %primitive-plane-normal (a b c)
  (let* ((cross (%point-cross a b c))
         (divisor (reduce #'gcd cross :key #'abs)))
    (unless (plusp divisor)
      (error "Degenerate triangle in coplanar compression: ~S ~S ~S."
             a b c))
    (mapcar (lambda (coordinate) (/ coordinate divisor)) cross)))

(defun %map-surface-mesh-triangle-records (function mesh)
  "Call FUNCTION with kind, stock, ambient, mask, normal, and three points."
  (let ((templates (surface-mesh-template-vertex-words mesh))
        (ranges (surface-mesh-template-ranges mesh)))
    (labels ((point (base vertex)
               (loop for axis below 3
                     collect (+ (* +mesh-cell-size+ (nth axis base))
                                (- (aref templates
                                         (+ (* vertex
                                               +mesh-template-vertex-word-count+)
                                            axis))
                                   +mesh-template-coordinate-bias+))))
             (visit (words kind)
               (loop for offset from 0 below (length words) by 4
                     for base = (list (aref words offset)
                                      (aref words (+ offset 1))
                                      (aref words (+ offset 2)))
                     for meta = (aref words (+ offset 3))
                     for template-id = (ldb (byte 16 0) meta)
                     for stock = (ldb (byte +mesh-instance-stock-bit-count+
                                            +mesh-instance-stock-shift+)
                                      meta)
                     for ambient = (ldb (byte 2
                                              +mesh-instance-ambient-occlusion-shift+)
                                        meta)
                     for start = (aref ranges (* 2 template-id))
                     for count = (aref ranges (1+ (* 2 template-id)))
                     do (loop for vertex from start below (+ start count) by 3
                              for attributes =
                                (aref templates
                                      (+ (* vertex
                                            +mesh-template-vertex-word-count+)
                                         3))
                              for a = (point base vertex)
                              for b = (point base (1+ vertex))
                              for c = (point base (+ vertex 2))
                              for normal = (%primitive-plane-normal a b c)
                              do (funcall function kind stock ambient
                                          (ldb (byte 3 10) attributes)
                                          normal a b c)))))
      (visit (surface-mesh-face-instance-words mesh) :face)
      (visit (surface-mesh-band-instance-words mesh) :band)
      (visit (surface-mesh-fan-instance-words mesh) :junction))))

;;; ---------------------------------------------------------------------------
;;; Sparse semantic attachment realization

(defstruct (surface-attachment-frame
             (:constructor %make-surface-attachment-frame
                 (origin normal tangent primitive-kinds stocks))
             (:copier nil))
  "One authored face-chart point resolved against the finished surface.

ORIGIN is in renderer world units.  NORMAL is the outward unit normal of the
hit primitive, or the deterministic unit bisector of its normal cone when the
chart point lies exactly on a crease.  TANGENT is the authored face's first
chart axis projected into that tangent plane.  The primitive and stock lists
retain the cold diagnostic provenance of every tied hit."
  (origin #() :type (simple-array single-float (3)) :read-only t)
  (normal #() :type (simple-array single-float (3)) :read-only t)
  (tangent #() :type (simple-array single-float (3)) :read-only t)
  (primitive-kinds nil :type list :read-only t)
  (stocks nil :type list :read-only t))

(defun %surface-frame-axis-vector (axis)
  (ecase axis
    (:x '(1.0d0 0.0d0 0.0d0))
    (:y '(0.0d0 1.0d0 0.0d0))
    (:z '(0.0d0 0.0d0 1.0d0))))

(defun %surface-frame-dot (left right)
  (+ (* (first left) (first right))
     (* (second left) (second right))
     (* (third left) (third right))))

(defun %surface-frame-scale (amount vector)
  (mapcar (lambda (component) (* amount component)) vector))

(defun %surface-frame+ (&rest vectors)
  (loop for axis below 3
        collect (loop for vector in vectors sum (nth axis vector))))

(defun %surface-frame-cross (left right)
  (list (- (* (second left) (third right))
           (* (third left) (second right)))
        (- (* (third left) (first right))
           (* (first left) (third right)))
        (- (* (first left) (second right))
           (* (second left) (first right)))))

(defun %surface-frame-unit (vector)
  (let ((length
          (sqrt (max 0.0d0 (%surface-frame-dot vector vector)))))
    (unless (> length 1.0d-12)
      (error "Cannot normalize the zero attachment-frame vector ~S." vector))
    (%surface-frame-scale (/ 1.0d0 length) vector)))

(defun %surface-frame-point-in-projected-triangle-p
    (point a b c u-axis v-axis)
  "Test POINT in ABC after projection into the authored face chart."
  (labels ((project (value)
             (values (%surface-frame-dot value u-axis)
                     (%surface-frame-dot value v-axis)))
           (edge (ax ay bx by px py)
             (- (* (- px ax) (- by ay))
                (* (- py ay) (- bx ax)))))
    (multiple-value-bind (px py) (project point)
      (multiple-value-bind (ax ay) (project a)
        (multiple-value-bind (bx by) (project b)
          (multiple-value-bind (cx cy) (project c)
            (let* ((ab (edge ax ay bx by px py))
                   (bc (edge bx by cx cy px py))
                   (ca (edge cx cy ax ay px py))
                   (epsilon 1.0d-8)
                   (negative-p (or (< ab (- epsilon))
                                   (< bc (- epsilon))
                                   (< ca (- epsilon))))
                   (positive-p (or (> ab epsilon) (> bc epsilon)
                                   (> ca epsilon))))
              (not (and negative-p positive-p)))))))))

(defun %surface-frame-nearest-projected-triangle-point
    (chart a b c u-axis v-axis authored-normal primitive-normal denominator)
  "Return the nearest chart-projected point of ABC and its squared distance.

When CHART projects inside ABC, retain the original authored-normal ray
intersection exactly.  Otherwise the nearest point of the closed projected
triangle lies on one of its three edges; lift that edge parameter back onto
the actual three-dimensional primitive."
  (if (%surface-frame-point-in-projected-triangle-p
       chart a b c u-axis v-axis)
      (let ((displacement
              (/ (- (%surface-frame-dot primitive-normal a)
                    (%surface-frame-dot primitive-normal chart))
                 denominator)))
        (values
         (%surface-frame+
          chart (%surface-frame-scale displacement authored-normal))
         0.0d0))
      (let ((chart-u (%surface-frame-dot chart u-axis))
            (chart-v (%surface-frame-dot chart v-axis))
            (best-point nil)
            (best-distance-squared nil))
        (labels ((visit-edge (start end)
                   (let* ((edge (%surface-frame+ end
                                                 (%surface-frame-scale
                                                  -1.0d0 start)))
                          (edge-u (%surface-frame-dot edge u-axis))
                          (edge-v (%surface-frame-dot edge v-axis))
                          (length-squared
                            (+ (* edge-u edge-u) (* edge-v edge-v)))
                          (start-u (%surface-frame-dot start u-axis))
                          (start-v (%surface-frame-dot start v-axis))
                          (parameter
                            (max 0.0d0
                                 (min 1.0d0
                                      (/ (+ (* (- chart-u start-u) edge-u)
                                            (* (- chart-v start-v) edge-v))
                                         length-squared))))
                          (point
                            (%surface-frame+
                             start (%surface-frame-scale parameter edge)))
                          (delta-u
                            (- (%surface-frame-dot point u-axis) chart-u))
                          (delta-v
                            (- (%surface-frame-dot point v-axis) chart-v))
                          (distance-squared
                            (+ (* delta-u delta-u) (* delta-v delta-v))))
                     (when (or (null best-distance-squared)
                               (< distance-squared best-distance-squared)
                               (and (= distance-squared
                                       best-distance-squared)
                                    (%point-order< point best-point)))
                       (setf best-point point
                             best-distance-squared distance-squared)))))
          ;; A facing triangle has a nonzero chart projection, so at least two
          ;; of these projected edges have positive length.  Skip the possible
          ;; zero-length edge defensively rather than dividing by zero.
          (dolist (edge (list (list a b) (list b c) (list c a)))
            (let* ((start (first edge))
                   (end (second edge))
                   (delta (%surface-frame+ end
                                           (%surface-frame-scale -1.0d0 start)))
                   (du (%surface-frame-dot delta u-axis))
                   (dv (%surface-frame-dot delta v-axis)))
              (when (> (+ (* du du) (* dv dv)) 1.0d-20)
                (visit-edge start end))))
          (unless best-point
            (error "Facing attachment triangle has no projected edge: ~S ~S ~S."
                   a b c))
          (values best-point best-distance-squared)))))

(defun %surface-frame-point-in-support-footprint-p
    (point center u-axis v-axis epsilon)
  "Test POINT in the authored face's inclusive one-cell footprint."
  (let* ((offset (%surface-frame+ point
                                  (%surface-frame-scale -1.0d0 center)))
         (half-cell (* 0.5d0 +mesh-cell-size+))
         (limit (+ half-cell epsilon))
         (u (%surface-frame-dot offset u-axis))
         (v (%surface-frame-dot offset v-axis)))
    (and (<= (- limit) u limit)
         (<= (- limit) v limit))))

(defun %surface-frame-point-distance-squared (left right)
  (loop for l in left
        for r in right
        for delta = (- l r)
        sum (* delta delta)))

(defun %surface-frame-candidate-relation
    (radius-squared displacement point
     best-radius-squared best-displacement best-point
     tie-epsilon radius-squared-tie-epsilon point-squared-tie-epsilon)
  "Classify one admissible candidate against the current geometric optimum."
  (cond
    ((or (null best-radius-squared)
         (< radius-squared
            (- best-radius-squared radius-squared-tie-epsilon)))
     :replace)
    ((<= (abs (- radius-squared best-radius-squared))
         radius-squared-tie-epsilon)
     (cond
       ((> displacement (+ best-displacement tie-epsilon)) :replace)
       ((<= (abs (- displacement best-displacement)) tie-epsilon)
        (cond
          ((<= (%surface-frame-point-distance-squared point best-point)
               point-squared-tie-epsilon)
           :tie)
          ((%point-order< point best-point) :replace)
          (t :ignore)))
       (t :ignore)))
    (t :ignore)))

(defun resolve-surface-attachment-frame
    (meshes face &key (u 0.0d0) (v 0.0d0))
  "Resolve FACE chart coordinates U/V against finished MESHES.

U and V are normalized logical-face coordinates in [-1,1].  The square chart
is compacted radially onto |U|+|V|<=1: points already inside that diamond are
unchanged, while logical corners map continuously onto the realized junction
domain instead of casting through empty space beyond a chamfered corner.

For every facing finished triangle, the resolver finds the closest point in
the authored face chart and lifts it onto that actual primitive.  Candidates
are ranked first by minimum tangential distance and then by outermost normal
displacement.  Thus any triangle under the old authored-normal ray has zero
tangential distance and produces the exact old result; only a genuine ray miss
moves tangentially onto the nearest finished face, band, transition triangle,
or junction fan.

The finished surface of the authored support cell cannot lie outward of its
cubical face or more than the mesh's maximum bevel width inward or tangentially
away from the mapped chart point.  It must also stay inside the exact one-cell
support-face footprint.  Enforcing that slab, radius, footprint, and outward
facing cone is an ownership condition, not a global nearest-hit heuristic:
parallel or neighboring surfaces elsewhere must never steal the attachment.

At a non-smooth point there is no unique differential normal.  Rather than
falling back to the cubical face, this function returns the normalized sum of
the distinct tied primitive normals: a deterministic bisector of the actual
surface normal cone."
  (unless (listp meshes) (setf meshes (list meshes)))
  (unless meshes
    (error "Attachment realization needs at least one finished surface mesh."))
  (unless (every (lambda (mesh) (typep mesh 'surface-mesh)) meshes)
    (error "Attachment realization received a non-mesh member in ~S." meshes))
  (unless (and (realp u) (<= -1 u 1) (realp v) (<= -1 v 1))
    (error "Attachment chart coordinates must lie in [-1,1], not (~S,~S)."
           u v))
  (let ((domain (surface-mesh-domain (first meshes))))
    (unless (every (lambda (mesh) (eq domain (surface-mesh-domain mesh)))
                   (rest meshes))
      (error "Attachment realization meshes do not share one world domain."))
    (%require-face domain face)
    (multiple-value-bind (u-name v-name) (face-tangent-axes face)
      (multiple-value-bind (normal-x normal-y normal-z)
          (face-oriented-normal face)
        (let* ((authored-normal
                 (mapcar #'coerce (list normal-x normal-y normal-z)
                         (make-list 3 :initial-element 'double-float)))
               (u-axis (%surface-frame-axis-vector u-name))
               (v-axis (%surface-frame-axis-vector v-name))
               (v-axis
                 (if (minusp
                      (%surface-frame-dot
                       (%surface-frame-cross u-axis v-axis) authored-normal))
                     (%surface-frame-scale -1.0d0 v-axis)
                     v-axis))
               (center
                 (loop for coordinate in
                       (list (site-x face) (site-y face) (site-z face))
                       for axis-number below 3
                       collect
                       (coerce
                        (+ (* +mesh-cell-size+ coordinate)
                           (if (logbitp axis-number (site-extent face))
                               (/ +mesh-cell-size+ 2)
                               0))
                        'double-float)))
               (chart-u (coerce u 'double-float))
               (chart-v (coerce v 'double-float))
               (chart-scale
                 (max 1.0d0 (+ (abs chart-u) (abs chart-v))))
               (chart
                 (%surface-frame+
                  center
                  (%surface-frame-scale
                   (* 0.5d0 +mesh-cell-size+ (/ chart-u chart-scale)) u-axis)
                  (%surface-frame-scale
                   (* 0.5d0 +mesh-cell-size+ (/ chart-v chart-scale)) v-axis)))
               (best-radius-squared nil)
               (best-displacement nil)
               (best-point nil)
               (hits nil)
               (maximum-inset
                 (reduce #'max meshes :key #'surface-mesh-bevel-width))
               (tie-epsilon (* 1.0d-7 (max 1 maximum-inset)))
               (point-squared-tie-epsilon (* tie-epsilon tie-epsilon))
               (radius-squared-tie-epsilon
                 (+ (* 2.0d0 maximum-inset tie-epsilon)
                    point-squared-tie-epsilon))
               (maximum-radius-squared
                 (+ (* maximum-inset maximum-inset)
                    radius-squared-tie-epsilon)))
          (dolist (mesh meshes)
            (%map-surface-mesh-triangle-records
             (lambda (kind stock ambient mask primitive-normal a b c)
               (declare (ignore ambient mask))
               (let* ((primitive-normal (mapcar (lambda (value)
                                                  (coerce value 'double-float))
                                                primitive-normal))
                      (denominator
                        (%surface-frame-dot primitive-normal authored-normal)))
                 (when (> denominator 1.0d-10)
                   (multiple-value-bind (point radius-squared)
                       (%surface-frame-nearest-projected-triangle-point
                        chart a b c u-axis v-axis authored-normal
                        primitive-normal denominator)
                     (let ((displacement
                             (%surface-frame-dot
                              (%surface-frame+
                               point (%surface-frame-scale -1.0d0 chart))
                              authored-normal)))
                       (when (and
                              (<= (- (+ maximum-inset tie-epsilon))
                                  displacement tie-epsilon)
                              (<= radius-squared maximum-radius-squared)
                              (%surface-frame-point-in-support-footprint-p
                               point center u-axis v-axis tie-epsilon))
                         (case
                             (%surface-frame-candidate-relation
                              radius-squared displacement point
                              best-radius-squared best-displacement best-point
                              tie-epsilon radius-squared-tie-epsilon
                              point-squared-tie-epsilon)
                           (:replace
                            (setf best-radius-squared radius-squared
                                  best-displacement displacement
                                  best-point point
                                  hits
                                  (list
                                   (list kind stock primitive-normal point))))
                           (:tie
                            (push (list kind stock primitive-normal point)
                                  hits)))))))))
             mesh))
          (unless hits
            (error "Face chart point (~S,~S) on ~S misses the finished surface."
                   u v face))
          (let* ((unit-normals
                   (remove-duplicates
                    (mapcar (lambda (hit)
                              (%surface-frame-unit (third hit)))
                            hits)
                    :test (lambda (left right)
                            (> (%surface-frame-dot left right)
                               (- 1.0d0 1.0d-10)))))
                 (normal
                   (%surface-frame-unit
                    (reduce #'%surface-frame+ unit-normals)))
                 (projected-u
                   (%surface-frame+
                    u-axis
                    (%surface-frame-scale
                     (- (%surface-frame-dot u-axis normal)) normal)))
                 (tangent
                   (if (> (%surface-frame-dot projected-u projected-u)
                          1.0d-12)
                       (%surface-frame-unit projected-u)
                       (%surface-frame-unit
                        (%surface-frame+
                         v-axis
                         (%surface-frame-scale
                          (- (%surface-frame-dot v-axis normal)) normal)))))
                 (point best-point))
            (%make-surface-attachment-frame
             (map '(simple-array single-float (3))
                  (lambda (coordinate)
                    (coerce (/ coordinate +mesh-cell-size+) 'single-float))
                  point)
             (map '(simple-array single-float (3))
                  (lambda (component) (coerce component 'single-float)) normal)
             (map '(simple-array single-float (3))
                  (lambda (component) (coerce component 'single-float)) tangent)
             (sort (remove-duplicates (mapcar #'first hits))
                   #'< :key (lambda (kind)
                              (ecase kind (:face 0) (:band 1) (:junction 2))))
             (sort (remove-duplicates (mapcar #'second hits)) #'<))))))))
