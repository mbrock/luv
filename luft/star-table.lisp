(in-package #:luft)

;;; The star atlas as plain data
;;;
;;; star-atlas.lisp defines the whole width-one local patch of each of
;;; the 22 canonical star families, keyed by part: twelve face quadrants
;;; named by their cell sample pairs, six band half-edges named by signed
;;; axis directions, and the junction.  This file unfolds those families
;;; onto all 256 stars with nothing but signed-axis transformations --
;;; no geometric algorithms run here.
;;;
;;; Owned geometry is not symmetry-equivariant, so ownership is a filter
;;; applied after unfolding, stated entirely in local terms: a site owns
;;; the faces whose cell pair contains the all-positive cell (the site is
;;; that face's minimal corner), the bands on its positive half-edges
;;; (the negative half-edge of one site is the positive half-edge of its
;;; neighbor), and its junction.  Drawing every site's owned triangles
;;; therefore tiles the surface of any solid exactly once; the tests
;;; close that surface over every star block and pseudorandom solids.

(defun %star-atlas-family-table (atlas)
  (unless (eq (first atlas) :width-one-star-atlas)
    (error "*STAR-ATLAS* is not a star atlas."))
  (unless (= (getf (rest atlas) :cell-size) +mesh-cell-size+)
    (error "Star atlas cell size ~D does not match ~D."
           (getf (rest atlas) :cell-size) +mesh-cell-size+))
  (let ((table (make-hash-table)))
    (dolist (family (getf (rest atlas) :families) table)
      (destructuring-bind (&key star complement faces bands junction) family
        (unless (= star (star-canonical-form star :reflections t))
          (error "Star atlas family #x~2,'0X is not canonical." star))
        (unless (= complement
                   (star-canonical-form (%complement-star star)
                                        :reflections t))
          (error "Star atlas family #x~2,'0X names the wrong complement."
                 star))
        (setf (gethash star table)
              (list :faces faces :bands bands :junction junction))))))

;;; Unfolding
;;;
;;; STAR-CANONICAL-FORM already witnesses the transformation carrying each
;;; representative onto its star, so unfolding transforms the family's
;;; points and part names alike.  A reversing witness reverses triangle
;;; orientation, so the last two vertices swap back to face outward.

(defun %transformation-determinant (transformation)
  (destructuring-bind ((a b c) (d e f) (g h i)) transformation
    (- (+ (* a e i) (* b f g) (* c d h))
       (+ (* c e g) (* b d i) (* a f h)))))

(defun %transform-oriented-triangles (transformation triangles)
  (let ((triangles (transform-star-triangles transformation triangles))
        (reversing-p (minusp (%transformation-determinant
                              transformation))))
    (if reversing-p
        (loop for (a b c) in triangles collect (list a c b))
        triangles)))

(defun %transform-face-pair (transformation pair)
  (let ((images (loop for sample in pair
                      collect (%transform-star-sample
                               transformation sample))))
    (list (logand (first images) (second images))
          (logior (first images) (second images)))))

(defun %transform-band-direction (transformation key)
  (destructuring-bind (axis sign) key
    (let* ((direction (loop for axis-number below 3
                            collect (if (= axis-number axis) sign 0)))
           (image (%transform-star-point transformation direction))
           (image-axis (position-if-not #'zerop image)))
      (list image-axis (nth image-axis image)))))

(defun %unfold-star-parts (star)
  (multiple-value-bind (representative transformation)
      (star-canonical-form star :reflections t)
    (let ((family (or (gethash representative *star-atlas-families*)
                      (error "No atlas family for star #x~2,'0X." star))))
      (flet ((triangles (triangles)
               (%transform-oriented-triangles transformation triangles)))
        (list :faces
              (loop for (pair part) in (getf family :faces)
                    collect (list (%transform-face-pair
                                   transformation pair)
                                  (triangles part)))
              :bands
              (loop for (key part) in (getf family :bands)
                    collect (list (%transform-band-direction
                                   transformation key)
                                  (triangles part)))
              :junction (triangles (getf family :junction)))))))

(defun %star-owned-part-triangles (parts)
  (append (loop for (pair triangles) in (getf parts :faces)
                when (= (second pair) #b111)
                  append triangles)
          (loop for (key triangles) in (getf parts :bands)
                when (plusp (second key))
                  append triangles)
          (getf parts :junction)))

(defun %sample-list-mask (samples)
  (loop for sample in samples sum (ash 1 sample)))

(defun %face-appearance-sample-masks (star pair)
  "The direct occupied/empty samples on one ordinary face."
  (let ((pair-mask (%sample-list-mask pair)))
    (values (logand star pair-mask)
            (logand (logxor #xff star) pair-mask))))

(defun %band-appearance-sample-masks (star key)
  "The occupied/empty cells incident to the half-edge named by KEY."
  (destructuring-bind (axis sign) key
    (let ((incident
            (loop for sample below 8
                  when (eq (plusp sign) (logbitp axis sample))
                    sum (ash 1 sample))))
      (values (logand star incident)
              (logand (logxor #xff star) incident)))))

(defun %junction-appearance-sample-masks (star)
  "All occupied and empty cells incident to the lattice junction."
  (values star (logxor #xff star)))

(defun %repeat-appearance-masks (triangles material-mask light-mask)
  (loop repeat (length triangles)
        collect (list material-mask light-mask)))

(defun %star-owned-appearance-masks (star parts)
  "Parallel material/light sample masks for STAR's owned atlas triangles.

Faces retain their unique solid/air pair.  A band uses all four cells
incident to its lattice half-edge, and a junction uses all eight cells.  The
consumer reduces material tones by equal-weight arithmetic mean and light by
componentwise maximum.  Both reductions are commutative, so these set-valued
selectors contain no hidden canonical-frame tie breaker."
  (append
   (loop for (pair triangles) in (getf parts :faces)
         when (= (second pair) #b111)
           append (multiple-value-bind (material light)
                      (%face-appearance-sample-masks star pair)
                    (%repeat-appearance-masks triangles material light)))
   (loop for (key triangles) in (getf parts :bands)
         when (plusp (second key))
           append (multiple-value-bind (material light)
                      (%band-appearance-sample-masks star key)
                    (%repeat-appearance-masks triangles material light)))
   (multiple-value-bind (material light)
       (%junction-appearance-sample-masks star)
     (%repeat-appearance-masks (getf parts :junction) material light))))

(defun star-atlas-parts (star)
  "Return STAR's whole local patch unfolded from the atlas.

The result is a plist of :FACES, :BANDS, and :JUNCTION; faces and bands
are lists of (KEY TRIANGLES) with the atlas part keys transformed along
with the geometry."
  (%check-star star)
  (svref *star-atlas-parts* star))

(defun star-atlas-owned-triangles (star)
  "Return the atlas triangles a site with occupancy STAR draws itself.

Ownership is the local filter documented above: minimal-corner faces,
positive half-edge bands, and the junction.  Instantiating this list at
every lattice site tiles the width-one surface exactly once."
  (%check-star star)
  (svref *star-atlas-owned-triangles* star))

(defun star-atlas-owned-appearance-masks (star)
  "Return (MATERIAL-MASK LIGHT-MASK) parallel to STAR's owned triangles."
  (%check-star star)
  (svref *star-atlas-owned-appearance-masks* star))

;;; Meshing a solid
;;;
;;; The basic mesher is a table lookup: enumerate the lattice sites
;;; touching the solid, read each site's occupancy star, and translate
;;; the owned triangles into place.  This is also the shape of the
;;; instanced GPU path: one instance per site carrying (X Y Z STAR).

(defun star-surface-sites (occupied-cells)
  "Map each lattice site touching OCCUPIED-CELLS to its occupancy star.

OCCUPIED-CELLS is an EQUAL hash set of integer cell coordinates
\(X Y Z); the result maps site coordinates to eight-bit stars."
  (let ((stars (make-hash-table :test #'equal)))
    (maphash
     (lambda (cell presence)
       (declare (ignore presence))
       (dolist (dx '(0 1))
         (dolist (dy '(0 1))
           (dolist (dz '(0 1))
             (let ((site (list (+ (first cell) dx)
                               (+ (second cell) dy)
                               (+ (third cell) dz)))
                   (sample (logior (if (zerop dx) 1 0)
                                   (if (zerop dy) 2 0)
                                   (if (zerop dz) 4 0))))
               (setf (gethash site stars)
                     (logior (gethash site stars 0)
                             (ash 1 sample))))))))
     occupied-cells)
    stars))

(defun star-surface-triangles (occupied-cells)
  "Mesh the width-one surface of OCCUPIED-CELLS as a triangle soup.

Cells are unit lattice cells; the triangles are in global mesh ticks,
each drawn once by the site that owns it."
  (let ((triangles nil))
    (maphash
     (lambda (site star)
       (dolist (triangle (star-atlas-owned-triangles star))
         (push (loop for point in triangle
                     collect (mapcar
                              (lambda (coordinate site-coordinate)
                                (+ coordinate
                                   (* +mesh-cell-size+
                                      site-coordinate)))
                              point site))
               triangles)))
     (star-surface-sites occupied-cells))
    triangles))

;;; The loaded tables

(defparameter *star-atlas-families*
  (%star-atlas-family-table *star-atlas*)
  "Canonical family star to local patch parts, straight from the atlas.")

(defparameter *star-atlas-parts*
  (let ((table (make-array 256)))
    (dotimes (star 256 table)
      (setf (svref table star) (%unfold-star-parts star))))
  "Every star's local patch, unfolded from its canonical family.")

(defparameter *star-atlas-owned-triangles*
  (let ((table (make-array 256)))
    (dotimes (star 256 table)
      (setf (svref table star)
            (%star-owned-part-triangles (svref *star-atlas-parts* star)))))
  "Every star's owned triangles: what one site instance draws.")

(defparameter *star-atlas-owned-appearance-masks*
  (let ((table (make-array 256)))
    (dotimes (star 256 table)
      (setf (svref table star)
            (%star-owned-appearance-masks
             star (svref *star-atlas-parts* star)))))
  "Symmetry-stable material/light sample sets parallel to owned triangles.")
