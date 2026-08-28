(in-package #:luft)

;;; Resolved geometry of one width-one occupancy star
;;;
;;; This file is the source of truth for width-one star geometry.  A caller
;;; supplies one of the 256 eight-cell occupancy stars and receives ordinary
;;; lists of local integer triangle positions, derived here from first
;;; principles: exposed faces inset one tick from their creases, bevel bands
;;; pairing occupancy transitions around each lattice edge, and junction
;;; cycles closed by the cap/strip/fan dichotomy at each lattice point.
;;; Materials, ambient occlusion, ownership witnesses, normals, and packed
;;; renderer attributes do not enter this view.
;;;
;;; The production mesher compiles its width-one tables independently for
;;; speed; the exhaustive differential in LUFT's tests pins those tables to
;;; the derivation below, star by star.  Only the owned-orientation slice at
;;; the end of this file still reads the production vocabulary, because row
;;; ownership -- which site emits which patch -- is a production concept
;;; rather than a geometric one.
;;;
;;; The code is written as a top-down description in the ubiquitous language
;;; of stars, faces, bands, junctions, triangles, and cubical transformations.
;;; Each function should say one clear thing at its present level of
;;; abstraction.  When saying it would require array layout, bit manipulation,
;;; matrix arithmetic, or another lower-level mechanism, introduce a plainly
;;; named operation for that idea and define it later in the file.  Continue
;;; this way until the remaining operations are small, unsurprising bedrock:
;;; slicing a row, taking a dot product, forming an occupancy mask, or decoding
;;; coordinates.  The result should read first as the geometric derivation and
;;; only afterward as its implementation.  Prefer another domain sentence over
;;; an optimized expression that makes a reader reconstruct the sentence.

(defun star-triangles (star)
  "Resolve STAR into its face, band, and junction triangle lists.

STAR is the usual eight-bit occupancy mask.  The returned coordinates are
local mesh ticks around the star at the origin; one voxel is
+MESH-CELL-SIZE+ ticks wide."
  (%check-star star)
  (list :faces (star-face-triangles star)
        :bands (star-band-triangles star)
        :junctions (star-junction-triangles star)))

(defun star-face-triangles (star)
  "Resolve the inset face patches owned by width-one occupancy STAR."
  (%check-star star)
  (%triangles-of-templates (%star-face-templates star)))

(defun star-band-triangles (star)
  "Resolve the bevel bands owned by width-one occupancy STAR."
  (%check-star star)
  (%triangles-of-templates (%star-band-templates star)))

(defun star-junction-triangles (star)
  "Derive the central junction triangles of width-one occupancy STAR.

The junction is wholly owned by its lattice point, so the owned and local
views coincide, and both are the first-principles derivation."
  (%check-star star)
  (loop for cycle in (%star-junction-cycles star)
        append (%star-junction-cycle-triangles cycle)))

(defun star-local-surface-triangles (star)
  "Derive the complete local surface patch implied by occupancy STAR.

Unlike STAR-TRIANGLES, this includes the face quadrants and band half-edges
owned by neighboring lattice points.  It enumerates the twelve face
quadrants and six band half-edges directly.  The central junction is
already wholly owned at this point."
  (%check-star star)
  (list :faces (%star-local-face-triangles star)
        :bands (%star-local-band-triangles star)
        :junctions (star-junction-triangles star)))

(defun %star-local-face-triangles (star)
  (loop for quadrant in (%star-face-quadrants)
        for normal-sign = (%star-face-quadrant-normal-sign star quadrant)
        unless (zerop normal-sign)
          append (%star-face-quadrant-triangles quadrant normal-sign)))

(defun %star-local-band-triangles (star)
  (loop for half-edge in (%star-band-half-edges)
        append (%star-band-half-edge-triangles star half-edge)))

;;; Face patches
;;;
;;; Each of the twelve face quadrants names one neighboring cell pair across
;;; one axis.  When exactly one of the two cells is occupied the pair exposes
;;; a face, and its width-one patch is the cell face inset one tick on every
;;; side.  The insets that a flat continuation would reclaim are band-kind
;;; collars and belong to the half-edges below.

(defun %star-face-quadrants ()
  "Name each face quadrant as (NORMAL-AXIS U-POSITIVE-P V-POSITIVE-P)."
  (loop for axis-number below 3
        append
        (loop for u-positive-p in '(nil t)
              append
              (loop for v-positive-p in '(nil t)
                    collect (list axis-number
                                  u-positive-p v-positive-p)))))

(defun %star-face-quadrant-normal-sign (star quadrant)
  "Return QUADRANT's outward normal sign, or zero when it is not exposed."
  (multiple-value-bind (low-sample high-sample)
      (%star-face-quadrant-samples quadrant)
    (let ((low-occupied-p (logbitp low-sample star))
          (high-occupied-p (logbitp high-sample star)))
      (cond ((and low-occupied-p (not high-occupied-p)) 1)
            ((and high-occupied-p (not low-occupied-p)) -1)
            (t 0)))))

(defun %star-face-quadrant-samples (quadrant)
  (destructuring-bind (axis-number u-positive-p v-positive-p) quadrant
    (let* ((u (svref +axis-u+ axis-number))
           (v (svref +axis-v+ axis-number))
           (low-sample
             (logior (if u-positive-p (ash 1 u) 0)
                     (if v-positive-p (ash 1 v) 0))))
      (values low-sample (logior low-sample (ash 1 axis-number))))))

(defun %star-face-quadrant-triangles
    (quadrant normal-sign)
  (let ((axis-number (first quadrant)))
    (destructuring-bind (p0 p1 p2 p3)
        (%star-face-quadrant-points quadrant)
      (list (%star-oriented-triangle p0 p1 p2 axis-number normal-sign)
            (%star-oriented-triangle p0 p2 p3 axis-number normal-sign)))))

(defun %star-face-quadrant-points (quadrant)
  (destructuring-bind (axis-number u-positive-p v-positive-p) quadrant
    (let* ((u (svref +axis-u+ axis-number))
           (v (svref +axis-v+ axis-number))
           (u-base (if u-positive-p 0 (- +mesh-cell-size+)))
           (v-base (if v-positive-p 0 (- +mesh-cell-size+)))
           (far (1- +mesh-cell-size+))
           (p0 (list 0 0 0)) (p1 (list 0 0 0))
           (p2 (list 0 0 0)) (p3 (list 0 0 0)))
      (setf (nth u p0) (+ u-base 1) (nth v p0) (+ v-base 1)
            (nth u p1) (+ u-base far) (nth v p1) (+ v-base 1)
            (nth u p2) (+ u-base far) (nth v p2) (+ v-base far)
            (nth u p3) (+ u-base 1) (nth v p3) (+ v-base far))
      (list p0 p1 p2 p3))))

(defun %star-oriented-triangle (a b c normal-axis normal-sign)
  (let* ((u (svref +axis-u+ normal-axis))
         (v (svref +axis-v+ normal-axis))
         (orientation
           (- (* (- (nth u b) (nth u a)) (- (nth v c) (nth v a)))
              (* (- (nth v b) (nth v a)) (- (nth u c) (nth u a))))))
    ;; AXIS-U and AXIS-V are cyclic for X and Z, but anticyclic for Y.
    (when (= normal-axis 1)
      (setf orientation (- orientation)))
    (if (= (signum orientation) normal-sign)
        (list a b c)
        (list a c b))))

;;; Bevel bands
;;;
;;; Each of the six half-edges names the lattice edge leaving the site in one
;;; signed axis direction.  The four cells around that edge form a cyclic
;;; quadrant run; each occupancy transition in the run is one exposed face
;;; meeting the edge.  Paired transitions with distinct face normals bound a
;;; crease and own one bevel band between their rails.  A flat pair --
;;; coplanar faces continuing across the edge -- owns the two one-tick
;;; collars that restore the insets its faces did not need.

(defun %star-band-half-edges ()
  "Name each band half-edge as (AXIS POSITIVE-P)."
  (loop for axis-number below 3
        append (list (list axis-number nil)
                     (list axis-number t))))

(defun %star-band-half-edge-triangles (star half-edge)
  (let ((state (%star-half-edge-state star half-edge)))
    (loop for group in (%star-paired-transitions state)
          append (%star-transition-pair-triangles half-edge state group))))

(defun %star-paired-transitions (state)
  "Pair the transition indices of one four-bit quadrant occupancy run.

The ordinary two transitions are one pair.  A checkerboard has four
transitions and forces a manifold pairing choice; bands pair around each
EMPTY quadrant, so two diagonally-touching solids meet in concave fillets
joined across their shared lattice edge rather than separating into two
convex chamfer sheets."
  (let ((transitions
          (loop for index below 4
                unless (eq (logbitp index state)
                           (logbitp (mod (1+ index) 4) state))
                  collect index)))
    (ecase (length transitions)
      (0 nil)
      (2 (list transitions))
      (4 (loop for index below 4
               unless (logbitp index state)
                 collect (list (mod (+ index 3) 4) index))))))

(defun %star-transition-pair-triangles (half-edge state group)
  (let ((left (%star-half-edge-transition half-edge state (first group)))
        (right (%star-half-edge-transition half-edge state (second group))))
    (if (= (getf left :normal-axis) (getf right :normal-axis))
        ;; Equal normals are one flat face continued across the lattice
        ;; edge; the pair owns its two collars instead of a bevel band,
        ;; listed in the scan order of the cells whose faces they extend.
        (loop for transition in (%star-scan-ordered-transitions
                                 half-edge (list left right))
              append (%star-flat-collar-triangles half-edge transition))
        (%star-crease-band-triangles half-edge left right))))

(defun %star-scan-ordered-transitions (half-edge transitions)
  (sort (copy-list transitions) #'<
        :key (lambda (transition)
               (%star-half-edge-sample
                half-edge (getf transition :occupied-quadrant)))))

(defun %star-half-edge-transition (half-edge state transition-index)
  "Resolve one occupancy transition into its face and rail directions.

STATE is the four-bit quadrant occupancy run of HALF-EDGE.  The face normal
points from the occupied quadrant toward the empty one; the rail offset
points from the lattice edge into the occupied quadrant along the face
plane."
  (destructuring-bind (axis-number positive-p) half-edge
    (declare (ignore positive-p))
    (let* ((u (svref +axis-u+ axis-number))
           (v (svref +axis-v+ axis-number))
           (next-index (mod (1+ transition-index) 4))
           (occupied-p (logbitp transition-index state))
           (occupied-index (if occupied-p transition-index next-index))
           (empty-index (if occupied-p next-index transition-index))
           (qu-occupied (svref +quadrant-u+ occupied-index))
           (qv-occupied (svref +quadrant-v+ occupied-index))
           (qu-empty (svref +quadrant-u+ empty-index))
           (qv-empty (svref +quadrant-v+ empty-index))
           (normal-axis (if (/= qu-occupied qu-empty) u v))
           (other-axis (if (= normal-axis u) v u)))
      (list :normal-axis normal-axis
            :normal-sign (if (= normal-axis u) qu-empty qv-empty)
            :other-axis other-axis
            :other-sign (if (= other-axis u) qu-occupied qv-occupied)
            :occupied-quadrant occupied-index))))

(defun %star-crease-band-triangles (half-edge left right)
  "One bevel band between the rails of two paired crease transitions."
  (destructuring-bind (axis-number positive-p) half-edge
    (let* ((low (+ (%star-half-edge-base positive-p) 1))
           (high (+ (%star-half-edge-base positive-p)
                    (1- +mesh-cell-size+)))
           (normal (%star-transition-pair-normal left right))
           (left-rail (%star-transition-rail left))
           (right-rail (%star-transition-rail right))
           (p0 (%star-rail-point left-rail axis-number low))
           (p1 (%star-rail-point right-rail axis-number low))
           (p2 (%star-rail-point right-rail axis-number high))
           (p3 (%star-rail-point left-rail axis-number high)))
      (list (%star-oriented-space-triangle p0 p1 p2 normal)
            (%star-oriented-space-triangle p0 p2 p3 normal)))))

(defun %star-flat-collar-triangles (half-edge transition)
  "One one-tick collar strip beside the lattice edge in a flat face plane.

The quad is laid out in the face's own frame with ascending coordinates,
exactly as face cells are, so its diagonal agrees with the face it extends."
  (destructuring-bind (axis-number positive-p) half-edge
    (let* ((low (+ (%star-half-edge-base positive-p) 1))
           (high (+ (%star-half-edge-base positive-p)
                    (1- +mesh-cell-size+)))
           (normal-axis (getf transition :normal-axis))
           (normal (%star-transition-normal transition))
           (cross-sign (getf transition :other-sign))
           (u (svref +axis-u+ normal-axis))
           (v (svref +axis-v+ normal-axis)))
      (flet ((span (axis)
               (if (= axis axis-number)
                   (list low high)
                   (list (min 0 cross-sign) (max 0 cross-sign)))))
        (destructuring-bind (u0 u1) (span u)
          (destructuring-bind (v0 v1) (span v)
            (let ((p0 (%star-plane-point u u0 v v0))
                  (p1 (%star-plane-point u u1 v v0))
                  (p2 (%star-plane-point u u1 v v1))
                  (p3 (%star-plane-point u u0 v v1)))
              (list (%star-oriented-space-triangle p0 p1 p2 normal)
                    (%star-oriented-space-triangle p0 p2 p3 normal)))))))))

(defun %star-plane-point (u-axis u-coordinate v-axis v-coordinate)
  (let ((point (list 0 0 0)))
    (setf (nth u-axis point) u-coordinate
          (nth v-axis point) v-coordinate)
    point))

(defun %star-half-edge-base (positive-p)
  (if positive-p 0 (- +mesh-cell-size+)))

(defun %star-transition-rail (transition)
  "The rail point one tick into the occupied quadrant, at edge coordinate 0."
  (let ((rail (list 0 0 0)))
    (setf (nth (getf transition :other-axis) rail)
          (getf transition :other-sign))
    rail))

(defun %star-transition-normal (transition)
  (let ((normal (list 0 0 0)))
    (setf (nth (getf transition :normal-axis) normal)
          (getf transition :normal-sign))
    normal))

(defun %star-transition-pair-normal (left right)
  (mapcar #'+ (%star-transition-normal left)
          (%star-transition-normal right)))

(defun %star-rail-point (rail axis-number coordinate)
  (let ((point (copy-list rail)))
    (setf (nth axis-number point) coordinate)
    point))

(defun %star-oriented-space-triangle (a b c normal)
  "Orient triangle A B C so its winding agrees with NORMAL."
  (if (minusp (%dot-product (%cross-product (%point-difference b a)
                                            (%point-difference c a))
                            normal))
      (list a c b)
      (list a b c)))

(defun %star-half-edge-state (star half-edge)
  (loop for quadrant below 4
        for sample = (%star-half-edge-sample half-edge quadrant)
        when (logbitp sample star)
          sum (ash 1 quadrant)))

(defun %star-half-edge-sample (half-edge quadrant)
  (destructuring-bind (axis-number positive-p) half-edge
    (let ((u (svref +axis-u+ axis-number))
          (v (svref +axis-v+ axis-number)))
      (logior (if positive-p (ash 1 axis-number) 0)
              (if (plusp (svref +quadrant-u+ quadrant)) (ash 1 u) 0)
              (if (plusp (svref +quadrant-v+ quadrant)) (ash 1 v) 0)))))

;;; The junction
;;;
;;; After every face patch and band is present, the open boundary edges next
;;; to the lattice point trace one or more cycles on the sphere of radius
;;; sqrt(2) around it.  A three-record cycle closes with its triangular cap.
;;; A planar cycle through the site fans flat from it, and a non-planar
;;; saddle cycle forces an interior apex whose only equidistant choice is
;;; the lattice point itself, so both fan from the site.  A planar cycle
;;; that misses the site needs no interior vertex at all and closes as a
;;; deterministic strip.

(defun %star-junction-cycles (star)
  "Trace the directed open boundary cycles around the central site."
  (%star-traced-cycles (%star-junction-records star)))

(defun %star-junction-cycle-triangles (cycle)
  (let ((points (mapcar #'first cycle)))
    (cond ((= 3 (length cycle))
           (%star-boundary-cap-triangles points))
          ((or (%points-plane-through-origin-p points)
               (not (%points-single-plane-p points)))
           (%star-centered-fan-triangles cycle))
          (t
           (%star-boundary-strip-triangles points)))))

(defun %star-boundary-cap-triangles (points)
  "Close a three-record cycle with its single triangle.

The observed loop follows the existing surface winding, so the cap reverses
it to pair every boundary edge with opposite winding."
  (destructuring-bind (a b c) points
    (list (list a c b))))

(defun %star-centered-fan-triangles (cycle)
  "Fan CYCLE from the lattice point at the origin."
  (loop for (left right) in cycle
        collect (list (list 0 0 0) right left)))

(defun %star-boundary-strip-triangles (points)
  "Triangulate a planar cycle without introducing interior geometry.

Repeatedly remove the end of the point run whose replacement diagonal is
shorter, so the remaining vertices stay a contiguous interval of the
boundary and the result is a deterministic local strip rather than a long
fan of spokes."
  (let ((triangles nil))
    (loop while (> (length points) 3) do
      (let ((first (first points))
            (second (second points))
            (last (car (last points)))
            (penultimate (car (last points 2))))
        (if (<= (%squared-distance second last)
                (%squared-distance first penultimate))
            (progn
              (push (list first last second) triangles)
              (setf points (rest points)))
            (progn
              (push (list first last penultimate) triangles)
              (setf points (butlast points))))))
    (destructuring-bind (a b c) points
      (push (list a c b) triangles))
    (nreverse triangles)))

(defun %star-junction-records (star)
  "The directed open boundary edges incident to the central lattice point.

Every face and band triangle edge is counted with its direction; an edge
traversed exactly once is open, directed as its surface traversed it.  The
central site owns the open edges whose endpoints stay within one tick of
it; the rest belong to neighboring lattice points.  Records sort in
descending point order so cycle tracing is deterministic."
  (let ((records
          (remove-if-not
           #'%star-junction-record-p
           (%once-traversed-edges
            (append (%star-local-face-triangles star)
                    (%star-local-band-triangles star))))))
    (sort records #'%star-record> :key #'%star-record-undirected-name)))

(defun %star-junction-record-p (record)
  (flet ((nearby-p (point)
           (every (lambda (coordinate) (<= -1 coordinate 1)) point)))
    (and (nearby-p (first record)) (nearby-p (second record)))))

(defun %once-traversed-edges (triangles)
  "Directed edges traversed exactly once across all TRIANGLES."
  (let ((net (make-hash-table :test #'equal)))
    (dolist (triangle triangles)
      (destructuring-bind (a b c) triangle
        (dolist (edge (list (list a b) (list b c) (list c a)))
          (destructuring-bind (from to) edge
            (if (%star-point< from to)
                (incf (gethash (list from to) net 0))
                (decf (gethash (list to from) net 0)))))))
    (let ((open nil))
      (maphash (lambda (edge count)
                 (destructuring-bind (low high) edge
                   (case count
                     (0)
                     (1 (push (list low high) open))
                     (-1 (push (list high low) open))
                     (t (error "Edge ~S is traversed ~D net times."
                               edge count)))))
               net)
      open)))

(defun %star-traced-cycles (records)
  "Order consistently directed RECORDS into loops, in record order."
  (let ((remaining (copy-list records))
        (cycles nil))
    (loop while remaining do
      (let* ((first-record (pop remaining))
             (cycle (list first-record))
             (stop (first first-record))
             (next (second first-record)))
        (loop until (equal next stop) do
          (let ((next-record
                  (find next remaining :key #'first :test #'equal)))
            (unless next-record
              (error "Open junction boundary stops at ~S." next))
            (setf remaining (remove next-record remaining :test #'eq)
                  cycle (cons next-record cycle)
                  next (second next-record))))
        (push (nreverse cycle) cycles)))
    (nreverse cycles)))

(defun %star-record-undirected-name (record)
  (destructuring-bind (left right) record
    (if (%star-point< left right)
        (list left right)
        (list right left))))

(defun %star-record> (left-name right-name)
  (loop for left-point in left-name
        for right-point in right-name
        unless (equal left-point right-point)
          do (return (%star-point< right-point left-point))
        finally (return nil)))

;;; Points and planes

(defun %points-plane-through-origin-p (points)
  "Whether POINTS all lie in one plane containing the origin."
  (let ((normal (%first-nonzero-cross points)))
    (and normal
         (every (lambda (point)
                  (zerop (%dot-product normal point)))
                points))))

(defun %points-single-plane-p (points)
  "Whether POINTS all lie in one (any) plane."
  (or (<= (length points) 3)
      (let* ((origin (first points))
             (offsets (mapcar (lambda (point)
                                (%point-difference point origin))
                              (rest points)))
             (normal (%first-nonzero-cross offsets)))
        (or (null normal)
            (every (lambda (offset)
                     (zerop (%dot-product normal offset)))
                   offsets)))))

(defun %first-nonzero-cross (points)
  (loop for (a . rest) on points
        do (loop for b in rest
                 for cross = (%cross-product a b)
                 unless (every #'zerop cross)
                   do (return-from %first-nonzero-cross cross)))
  nil)

(defun %cross-product (a b)
  (destructuring-bind (ax ay az) a
    (destructuring-bind (bx by bz) b
      (list (- (* ay bz) (* az by))
            (- (* az bx) (* ax bz))
            (- (* ax by) (* ay bx))))))

(defun %point-difference (a b)
  (mapcar #'- a b))

(defun %squared-distance (a b)
  (loop for left in a
        for right in b
        sum (expt (- left right) 2)))

(defun %unoriented-triangle-key (triangle)
  (sort (copy-list triangle) #'%star-point<))

(defun %star-point< (left right)
  (loop for left-coordinate in left
        for right-coordinate in right
        when (/= left-coordinate right-coordinate)
          do (return (< left-coordinate right-coordinate))
        finally (return nil)))

;;; Cubical symmetries
;;;
;;; A transformation is an ordinary three-by-three list matrix.  Its rows are
;;; signed coordinate axes, so it acts on the point lists above without a
;;; second geometry representation.  The 24 rotations have determinant +1;
;;; the 24 reflections have determinant -1.

(defun star-rotations ()
  "Return the 24 proper signed-axis transformations of a cubical star."
  (%star-transformations :proper))

(defun star-reflections ()
  "Return the 24 orientation-reversing transformations of a cubical star."
  (%star-transformations :reversing))

(defun transform-star (transformation star)
  "Apply signed-axis TRANSFORMATION to occupancy STAR."
  (%check-star star)
  (%star-of-samples
   (loop for sample in (%occupied-star-samples star)
         collect (%transform-star-sample transformation sample))))

(defun transform-star-triangles (transformation triangles)
  "Apply signed-axis TRANSFORMATION to ordinary list TRIANGLES.

Vertex order is retained.  Consequently a reflection reverses the geometric
orientation of each triangle; callers comparing outward-oriented surfaces may
reverse the last two vertices afterward."
  (loop for triangle in triangles
        collect (%transform-star-triangle transformation triangle)))

(defun star-orbit (star &key reflections complement)
  "Return the sorted occupancy orbit of STAR under cubical symmetry.

By default only proper rotations act.  REFLECTIONS includes the other half of
the full cube group; COMPLEMENT also identifies occupied and empty cells."
  (%check-star star)
  (%sorted-distinct-stars
   (%possibly-complemented-stars
    (%transformed-stars star (%star-orbit-transformations reflections))
    complement)))

(defun star-canonical-form (star &key reflections)
  "Return STAR's canonical representative and the transformation onto STAR.

The representative is the smallest occupancy mask in STAR's orbit, so equal
representatives identify equivalent stars.  The second value is a signed-axis
transformation carrying the representative back onto STAR:

  (TRANSFORM-STAR transformation representative) = STAR

By default only proper rotations act; REFLECTIONS canonicalizes under the
full cube group.  Among the transformations reaching the representative the
first proper rotation wins, so the answer is deterministic and a reversing
transformation appears only when STAR is a reflected chiral form."
  (%check-star star)
  (values-list
   (svref (svref *star-canonical-forms* (if reflections 1 0)) star)))

(defun %star-canonical-form (star reflections)
  (loop with representative = nil
        with witness = nil
        for transformation in (%star-orbit-transformations reflections)
        for image = (transform-star transformation star)
        when (or (null representative) (< image representative))
          do (setf representative image
                   witness transformation)
        finally
           (return
             (list representative
                   (%inverse-star-transformation witness)))))

(defun %star-canonical-form-table (reflections)
  (let ((table (make-array 256)))
    (dotimes (star 256 table)
      (setf (svref table star) (%star-canonical-form star reflections)))))

(defun %transform-star-sample (transformation sample)
  (%star-direction-index
   (%transform-star-point transformation (%star-sample-direction sample))))

(defun %transform-star-triangle (transformation triangle)
  (loop for point in triangle
        collect (%transform-star-point transformation point)))

(defun %inverse-star-transformation (transformation)
  "Invert a signed-axis transformation by transposing it."
  (apply #'mapcar #'list transformation))

(defun %star-orbit-transformations (include-reflections-p)
  (if include-reflections-p
      (append (star-rotations) (star-reflections))
      (star-rotations)))

(defun %transformed-stars (star transformations)
  (loop for transformation in transformations
        collect (transform-star transformation star)))

(defun %possibly-complemented-stars (stars include-complements-p)
  (if include-complements-p
      (loop for star in stars
            append (list star (%complement-star star)))
      stars))

(defun %sorted-distinct-stars (stars)
  (sort (remove-duplicates stars) #'<))

(defun %complement-star (star)
  (logxor #xff star))

(defun %star-transformations (orientation)
  (loop with determinant = (%orientation-determinant orientation)
        for permutation in (%axis-permutations)
        append
        (loop for signs in (%axis-sign-combinations)
              when (= determinant
                      (%signed-permutation-determinant permutation signs))
                collect (%signed-permutation-transformation
                         permutation signs))))

(defun %orientation-determinant (orientation)
  (ecase orientation
    (:proper 1)
    (:reversing -1)))

(defun %axis-permutations ()
  '((0 1 2) (0 2 1) (1 0 2) (1 2 0) (2 0 1) (2 1 0)))

(defun %axis-sign-combinations ()
  (loop for x-sign in '(-1 1)
        append
        (loop for y-sign in '(-1 1)
              append
              (loop for z-sign in '(-1 1)
                    collect (list x-sign y-sign z-sign)))))

(defun %signed-permutation-determinant (permutation signs)
  (* (%permutation-sign permutation)
     (reduce #'* signs)))

(defun %signed-permutation-transformation (permutation signs)
  (loop for source-axis in permutation
        for sign in signs
        collect (%signed-axis-row source-axis sign)))

(defun %signed-axis-row (source-axis sign)
  (loop for axis below 3
        collect (if (= axis source-axis) sign 0)))

(defun %permutation-sign (permutation)
  (if (oddp
       (loop for tail on permutation
             sum (count-if (lambda (later)
                             (> (first tail) later))
                           (rest tail))))
      -1
      1))

(defun %star-sample-direction (sample)
  (loop for axis below 3
        collect (if (logbitp axis sample) 1 -1)))

(defun %occupied-star-samples (star)
  (loop for sample below 8
        when (logbitp sample star)
          collect sample))

(defun %star-of-samples (samples)
  (loop for sample in samples
        sum (ash 1 sample)))

(defun %star-direction-index (direction)
  (loop for component in direction
        for axis from 0
        when (plusp component)
          sum (ash 1 axis)))

(defun %transform-star-point (transformation point)
  (loop for row in transformation
        collect (%dot-product row point)))

(defun %dot-product (left right)
  (loop for left-coordinate in left
        for right-coordinate in right
        sum (* left-coordinate right-coordinate)))

(defun %check-star (star)
  (check-type star (unsigned-byte 8))
  star)

;;; The production ownership slice
;;;
;;; Row ownership -- which lattice site's query row emits which face patch
;;; and band -- is a production division of labor, not a geometric fact, so
;;; the owned-orientation view reads the production vocabulary directly.
;;; The geometry inside those templates is pinned to the derivation above by
;;; the exhaustive differential in LUFT's tests.

(defun %star-face-templates (star)
  (let ((dimension (%star-query-dimension)))
    (%star-template-slice
     star
     (width-one-query-dimension-face-starts dimension)
     (width-one-query-dimension-face-templates dimension))))

(defun %star-band-templates (star)
  (let ((dimension (%star-query-dimension)))
    (%star-template-slice
     star
     (width-one-query-dimension-band-starts dimension)
     (width-one-query-dimension-band-templates dimension))))

(defun %star-junction-templates (star)
  (let ((dimension (%star-query-dimension)))
    (%star-template-slice
     star
     (width-one-query-dimension-fan-starts dimension)
     (width-one-query-dimension-fan-templates dimension))))

(defun %star-query-dimension ()
  (width-one-query-vocabulary-dimension
   *width-one-query-vocabulary*))

(defun %star-template-slice (star starts templates)
  "Return the template identifiers belonging to one STAR row."
  (loop for row from (aref starts star)
                  below (aref starts (1+ star))
        collect (aref templates row)))

(defun %triangles-of-templates (template-identifiers)
  (loop for template-id in template-identifiers
        append (%template-triangles template-id)))

(defun %template-triangles (template-id)
  (let ((positions (%template-vertex-positions template-id)))
    (unless (zerop (mod (length positions) 3))
      (error "Star template ~D has ~D vertices, not a triangle list."
             template-id (length positions)))
    (loop for remaining on positions by #'cdddr
          collect (list (first remaining)
                        (second remaining)
                        (third remaining)))))

(defun %template-vertex-positions (template-id)
  (let* ((vocabulary *width-one-query-vocabulary*)
         (ranges (width-one-query-vocabulary-ranges vocabulary))
         (words
           (svref
            (width-one-query-vocabulary-vertex-words-by-width vocabulary)
            1))
         (start (aref ranges (* 2 template-id)))
         (count (aref ranges (1+ (* 2 template-id)))))
    (loop for vertex from start below (+ start count)
          collect (%template-vertex-position words vertex))))

(defun %template-vertex-position (words vertex)
  (let ((word (* +mesh-template-vertex-word-count+ vertex)))
    (list (- (aref words word) +mesh-template-coordinate-bias+)
          (- (aref words (+ word 1)) +mesh-template-coordinate-bias+)
          (- (aref words (+ word 2)) +mesh-template-coordinate-bias+))))

;;; Built once at load time, after every operation above is available.

(defparameter *star-canonical-forms*
  (vector (%star-canonical-form-table nil)
          (%star-canonical-form-table t))
  "Canonical forms of all 256 stars, without and then with reflections.")
