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
