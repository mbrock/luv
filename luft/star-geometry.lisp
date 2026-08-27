(in-package #:luft)

;;; Resolved geometry of one width-one occupancy star
;;;
;;; This is a deliberately plain view of the production query vocabulary.
;;; A caller supplies one of the 256 eight-cell occupancy stars and receives
;;; ordinary lists of local integer triangle positions.  Materials, ambient
;;; occlusion, ownership witnesses, normals, and packed renderer attributes do
;;; not enter this view.
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
  "Resolve the central junctions of width-one occupancy STAR."
  (%check-star star)
  (%triangles-of-templates (%star-junction-templates star)))

(defun star-local-surface-triangles (star)
  "Resolve the complete local surface patch implied by occupancy STAR.

Unlike STAR-TRIANGLES, this forgets which neighboring lattice point owns each
face and band.  It asks the production query about every proper cubical
orientation, brings each answer back into STAR's coordinates, and takes the
geometric union.  The central junction is already wholly owned at this point,
so its selected triangulation is retained rather than collecting equivalent
rotated triangulations of the same patch."
  (%check-star star)
  (list :faces (%star-owned-triangle-rotation-union
                star #'star-face-triangles)
        :bands (%star-owned-triangle-rotation-union
                star #'star-band-triangles)
        :junctions (star-junction-triangles star)))

(defun %star-owned-triangle-rotation-union (star owned-triangles)
  (%distinct-geometric-triangles
   (loop for rotation in (star-rotations)
         for rotated-star = (transform-star rotation star)
         append
         (transform-star-triangles
          (%inverse-star-transformation rotation)
          (funcall owned-triangles rotated-star)))))

(defun %distinct-geometric-triangles (triangles)
  "Keep one triangle for each distinct set of three vertex positions."
  (remove-duplicates triangles
                     :test #'equal
                     :key #'%unoriented-triangle-key))

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
