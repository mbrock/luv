(in-package #:luft)

;;; The atlas is geometry truth.  This file contains only its cubical address
;;; transformations and small views used by the browser.

(defun %check-star (star)
  (check-type star (unsigned-byte 8))
  star)

(defun star-triangles (star)
  "Return STAR's atlas geometry grouped as faces, bands, and junctions."
  (let ((parts (star-atlas-parts star)))
    (list :faces (loop for (_ triangles) in (getf parts :faces)
                       append triangles)
          :bands (loop for (_ triangles) in (getf parts :bands)
                       append triangles)
          :junctions (getf parts :junction))))

(defun star-face-triangles (star)
  (getf (star-triangles star) :faces))

(defun star-band-triangles (star)
  (getf (star-triangles star) :bands))

(defun star-junction-triangles (star)
  (getf (star-triangles star) :junctions))

(defun star-local-surface-triangles (star)
  "The complete local atlas patch; ownership is deliberately not filtered."
  (star-triangles star))

(defun star-rotations ()
  "Return the 24 proper signed-axis transformations of a cube."
  (%star-transformations :proper))

(defun star-reflections ()
  "Return the 24 orientation-reversing signed-axis transformations."
  (%star-transformations :reversing))

(defun transform-star (transformation star)
  (let ((answer 0))
    (%check-star star)
    (dotimes (sample 8 answer)
      (when (logbitp sample star)
        (setf answer
              (logior answer
                      (ash 1 (%transform-star-sample transformation sample))))))))

(defun transform-star-triangles (transformation triangles)
  (loop for triangle in triangles
        collect (loop for point in triangle
                      collect (%transform-star-point transformation point))))

(defun star-orbit (star &key reflections complement)
  (%check-star star)
  (sort
   (remove-duplicates
    (loop for transformation in (%star-orbit-transformations reflections)
          for image = (transform-star transformation star)
          append (if complement (list image (%complement-star image))
                     (list image))))
   #'<))

(defun star-canonical-form (star &key reflections complement)
  "Return the least equivalent star and a transformation carrying it to STAR."
  (%check-star star)
  (values-list
   (svref (svref *star-canonical-forms*
                 (+ (if reflections 1 0) (if complement 2 0)))
          star)))

(defun %star-canonical-form (star reflections complement)
  (loop with representative
        with witness
        with complemented
        for transformation in (%star-orbit-transformations reflections)
        do (dolist (complement-p (if complement '(nil t) '(nil)))
             (let ((image (transform-star transformation
                                          (if complement-p
                                              (%complement-star star)
                                              star))))
               (when (or (null representative) (< image representative))
                 (setf representative image
                       witness transformation
                       complemented complement-p))))
        finally (return (list representative
                              (%inverse-star-transformation witness)
                              complemented))))

(defun %star-canonical-form-table (reflections complement)
  (let ((table (make-array 256)))
    (dotimes (star 256 table)
      (setf (svref table star)
            (%star-canonical-form star reflections complement)))))

(defun %transform-star-sample (transformation sample)
  (%star-direction-index
   (%transform-star-point transformation (%star-sample-direction sample))))

(defun %transform-star-point (transformation point)
  (loop for row in transformation
        collect (loop for a in row for b in point sum (* a b))))

(defun %inverse-star-transformation (transformation)
  (apply #'mapcar #'list transformation))

(defun %complement-star (star)
  (logxor #xff star))

(defun %star-orbit-transformations (include-reflections-p)
  (if include-reflections-p
      (append (star-rotations) (star-reflections))
      (star-rotations)))

(defun %star-transformations (orientation)
  (loop with determinant = (ecase orientation (:proper 1) (:reversing -1))
        for permutation in '((0 1 2) (0 2 1) (1 0 2)
                             (1 2 0) (2 0 1) (2 1 0))
        append
        (loop for sx in '(-1 1) append
          (loop for sy in '(-1 1) append
            (loop for sz in '(-1 1)
                  for signs = (list sx sy sz)
                  when (= determinant
                          (* (%permutation-sign permutation)
                             sx sy sz))
                    collect
                    (loop for source-axis in permutation
                          for sign in signs
                          collect
                          (loop for axis below 3
                                collect (if (= axis source-axis) sign 0))))))))

(defun %permutation-sign (permutation)
  (if (oddp (loop for tail on permutation
                  sum (count-if (lambda (later) (> (first tail) later))
                                (rest tail))))
      -1 1))

(defun %star-sample-direction (sample)
  (loop for axis below 3 collect (if (logbitp axis sample) 1 -1)))

(defun %star-direction-index (direction)
  (loop for component in direction
        for axis from 0
        when (plusp component) sum (ash 1 axis)))

(defparameter *star-canonical-forms*
  (vector (%star-canonical-form-table nil nil)
          (%star-canonical-form-table t nil)
          (%star-canonical-form-table nil t)
          (%star-canonical-form-table t t)))
