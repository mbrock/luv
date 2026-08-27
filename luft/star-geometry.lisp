(in-package #:luft)

;;; Resolved geometry of one width-one occupancy star
;;;
;;; This is a deliberately plain view of the production query vocabulary.
;;; A caller supplies one of the 256 eight-cell occupancy stars and receives
;;; ordinary lists of local integer triangle positions.  Materials, ambient
;;; occlusion, ownership witnesses, normals, and packed renderer attributes do
;;; not enter this view.

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

;;; Cubical symmetries
;;;
;;; A transformation is an ordinary three-by-three list matrix.  Its rows are
;;; signed coordinate axes, so it acts on the point lists above without a
;;; second geometry representation.  The 24 rotations have determinant +1;
;;; the 24 reflections have determinant -1.

(defun star-rotations ()
  "Return the 24 proper signed-axis transformations of a cubical star."
  (%star-transformations 1))

(defun star-reflections ()
  "Return the 24 orientation-reversing transformations of a cubical star."
  (%star-transformations -1))

(defun transform-star (transformation star)
  "Apply signed-axis TRANSFORMATION to occupancy STAR."
  (%check-star star)
  (loop with transformed = 0
        for sample below 8
        when (logbitp sample star)
          do (setf (ldb (byte 1
                              (%star-direction-index
                               (%transform-star-point
                                transformation
                                (%star-sample-direction sample))))
                           transformed)
                   1)
        finally (return transformed)))

(defun transform-star-triangles (transformation triangles)
  "Apply signed-axis TRANSFORMATION to ordinary list TRIANGLES.

Vertex order is retained.  Consequently a reflection reverses the geometric
orientation of each triangle; callers comparing outward-oriented surfaces may
reverse the last two vertices afterward."
  (loop for triangle in triangles
        collect (loop for point in triangle
                      collect (%transform-star-point transformation point))))

(defun star-orbit (star &key reflections complement)
  "Return the sorted occupancy orbit of STAR under cubical symmetry.

By default only proper rotations act.  REFLECTIONS includes the other half of
the full cube group; COMPLEMENT also identifies occupied and empty cells."
  (%check-star star)
  (sort
   (remove-duplicates
    (loop for transformation in
          (if reflections
              (append (star-rotations) (star-reflections))
              (star-rotations))
          for transformed = (transform-star transformation star)
          append (if complement
                     (list transformed (logxor #xff transformed))
                     (list transformed))))
   #'<))

(defun %star-transformations (determinant)
  (loop for permutation in '((0 1 2) (0 2 1) (1 0 2)
                             (1 2 0) (2 0 1) (2 1 0))
        append
        (loop for x-sign in '(-1 1)
              append
              (loop for y-sign in '(-1 1)
                    append
                    (loop for z-sign in '(-1 1)
                          for signs = (list x-sign y-sign z-sign)
                          when (= determinant
                                  (* (%permutation-sign permutation)
                                     x-sign y-sign z-sign))
                            collect
                            (loop for source-axis in permutation
                                  for sign in signs
                                  collect
                                  (loop for axis below 3
                                        collect (if (= axis source-axis)
                                                    sign
                                                    0))))))))

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

(defun %star-direction-index (direction)
  (loop for component in direction
        for axis from 0
        when (plusp component)
          sum (ash 1 axis)))

(defun %transform-star-point (transformation point)
  (loop for row in transformation
        collect (loop for coefficient in row
                      for coordinate in point
                      sum (* coefficient coordinate))))

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
