(in-package #:luft.render)

;;; Canonical fixture bodies: a coopered barrel and a framed crate, each an
;;; expanded triangle stream in one unit cell, Z up, corner at the origin.
;;; Each vertex is two Vec4 rows: local position and PART, then local normal
;;; and U.  For a stave U is its index plus the position across it, so the
;;; fragment can find seams and give each stave its own cut of the wood.
;;; Instances (LUFT.TERRAIN::PACK-FIXTURE-BODIES) place, turn, and tint them.

(defconstant +fixture-body-vertex-scalar-count+ 8)

(defconstant +fixture-part-stave+ 0)
(defconstant +fixture-part-hoop+ 1)
(defconstant +fixture-part-head+ 2)
(defconstant +fixture-part-chime+ 3)
(defconstant +fixture-part-frame+ 4)
(defconstant +fixture-part-panel+ 5)
(defconstant +fixture-part-brace+ 6)

(defparameter *barrel-stave-count* 14)
(defparameter *barrel-end-radius* 0.36)
(defparameter *barrel-belly-radius* 0.445)
(defparameter *barrel-height* 0.97)
(defparameter *barrel-hoops* '((0.06 0.15) (0.27 0.33) (0.64 0.70) (0.82 0.91))
  "Bands of iron, as heights in the barrel's unit height.")

(defun barrel-radius (z)
  "The stave radius at height Z: a parabolic belly between the two heads."
  (let ((s (- (* 2 (/ z *barrel-height*)) 1)))
    (+ *barrel-end-radius*
       (* (- *barrel-belly-radius* *barrel-end-radius*) (- 1 (* s s))))))

(defun barrel-radius-slope (z)
  (let ((s (- (* 2 (/ z *barrel-height*)) 1)))
    (* (- *barrel-belly-radius* *barrel-end-radius*) -2 s (/ 2 *barrel-height*))))

(defun %fixture-normalize (x y z)
  (let ((length (sqrt (+ (* x x) (* y y) (* z z)))))
    (list (/ x length) (/ y length) (/ z length))))

(defun make-fixture-body-builder ()
  (make-array 1024 :element-type 'single-float :adjustable t :fill-pointer 0))

(defun fixture-emit-vertex (data point part normal u)
  (dolist (value (list (first point) (second point) (third point) part
                       (first normal) (second normal) (third normal) u))
    (vector-push-extend (coerce value 'single-float) data)))

(defun fixture-emit-quad (data a b c d part normals us)
  "Emit quad A B C D (counter-clockwise seen from outside) as two triangles.
NORMALS and US give each corner's normal and U."
  (destructuring-bind (na nb nc nd) normals
    (destructuring-bind (ua ub uc ud) us
      (fixture-emit-vertex data a part na ua)
      (fixture-emit-vertex data b part nb ub)
      (fixture-emit-vertex data c part nc uc)
      (fixture-emit-vertex data a part na ua)
      (fixture-emit-vertex data c part nc uc)
      (fixture-emit-vertex data d part nd ud))))

(defun ring-point (radius angle z)
  (list (+ 0.5 (* radius (cos angle))) (+ 0.5 (* radius (sin angle))) z))

(defun emit-lathe-band (data part segments z0 z1 radius-function slope-function
                        &key (u-function (constantly 0)) (facet 0.0) (inward nil))
  "A surface of revolution between Z0 and Z1, SEGMENTS around.
FACET blends each normal toward its segment's middle, cutting flat staves."
  (dotimes (i segments)
    (let* ((a0 (* 2 pi (/ i segments)))
           (a1 (* 2 pi (/ (1+ i) segments)))
           (middle (/ (+ a0 a1) 2)))
      (flet ((normal (angle z)
               (let* ((angle (+ (* (- 1 facet) angle) (* facet middle)))
                      (slope (funcall slope-function z))
                      (n (%fixture-normalize (cos angle) (sin angle) (- slope))))
                 (if inward (mapcar #'- n) n))))
        (let ((a (ring-point (funcall radius-function z0) a0 z0))
              (b (ring-point (funcall radius-function z0) a1 z0))
              (c (ring-point (funcall radius-function z1) a1 z1))
              (d (ring-point (funcall radius-function z1) a0 z1))
              (normals (list (normal a0 z0) (normal a1 z0)
                             (normal a1 z1) (normal a0 z1)))
              (us (list (funcall u-function i 0) (funcall u-function i 1)
                        (funcall u-function i 1) (funcall u-function i 0))))
          (if inward
              (fixture-emit-quad data a d c b part
                                 (list (first normals) (fourth normals)
                                       (third normals) (second normals))
                                 (list (first us) (fourth us) (third us) (second us)))
              (fixture-emit-quad data a b c d part normals us)))))))

(defun emit-annulus (data part segments z inner outer up)
  "A flat ring at Z between radii INNER and OUTER facing up when UP."
  (let ((normal (if up '(0 0 1) '(0 0 -1))))
    (dotimes (i segments)
      (let* ((a0 (* 2 pi (/ i segments)))
             (a1 (* 2 pi (/ (1+ i) segments)))
             (a (ring-point outer a0 z)) (b (ring-point outer a1 z))
             (c (ring-point inner a1 z)) (d (ring-point inner a0 z))
             (normals (list normal normal normal normal))
             (us '(0 0 0 0)))
        (if up
            (fixture-emit-quad data a b c d part normals us)
            (fixture-emit-quad data a d c b part normals us))))))

(defun emit-barrel (data)
  (let* ((staves *barrel-stave-count*)
         (rings 10)
         (height *barrel-height*)
         (chime 0.035)
         (head-depth 0.045)
         (inner (- *barrel-end-radius* chime)))
    ;; Staves: each its own run of facets, two columns across.
    (dotimes (ring rings)
      (let ((z0 (* height (/ ring rings)))
            (z1 (* height (/ (1+ ring) rings))))
        (emit-lathe-band data +fixture-part-stave+ (* 2 staves) z0 z1
                         #'barrel-radius #'barrel-radius-slope
                         :facet 0.0
                         :u-function (lambda (i side)
                                       (+ (floor i 2) (* 0.5 (+ (mod i 2) side)))))))
    ;; Iron hoops stand proud of the staves, with lips top and bottom.
    (loop for (low high) in *barrel-hoops*
          for z0 = (* height low)
          for z1 = (* height high)
          for proud = 0.014
          do (emit-lathe-band data +fixture-part-hoop+ 40 z0 z1
                              (lambda (z) (+ proud (barrel-radius z)))
                              #'barrel-radius-slope)
             (emit-annulus data +fixture-part-hoop+ 40 z1
                           (barrel-radius z1) (+ proud (barrel-radius z1)) t)
             (emit-annulus data +fixture-part-hoop+ 40 z0
                           (barrel-radius z0) (+ proud (barrel-radius z0)) nil))
    ;; The chime: stave ends around a head set below them.
    (emit-annulus data +fixture-part-chime+ 40 height inner *barrel-end-radius* t)
    (emit-lathe-band data +fixture-part-chime+ 40 (- height head-depth) height
                     (constantly inner) (constantly 0) :inward t)
    (emit-annulus data +fixture-part-head+ 40 (- height head-depth) 0.0 inner t)
    (emit-annulus data +fixture-part-head+ 40 0.0 0.0 *barrel-end-radius* nil)))

(defun emit-box (data part low high)
  "An axis-aligned box from LOW to HIGH, each a list of three coordinates."
  (destructuring-bind ((x0 y0 z0) (x1 y1 z1)) (list low high)
    (flet ((face (a b c d normal)
             (fixture-emit-quad data a b c d part
                                (list normal normal normal normal) '(0 0 0 0))))
      (face (list x0 y0 z0) (list x0 y1 z0) (list x1 y1 z0) (list x1 y0 z0) '(0 0 -1))
      (face (list x0 y0 z1) (list x1 y0 z1) (list x1 y1 z1) (list x0 y1 z1) '(0 0 1))
      (face (list x0 y0 z0) (list x1 y0 z0) (list x1 y0 z1) (list x0 y0 z1) '(0 -1 0))
      (face (list x0 y1 z0) (list x0 y1 z1) (list x1 y1 z1) (list x1 y1 z0) '(0 1 0))
      (face (list x0 y0 z0) (list x0 y0 z1) (list x0 y1 z1) (list x0 y1 z0) '(-1 0 0))
      (face (list x1 y0 z0) (list x1 y1 z0) (list x1 y1 z1) (list x1 y0 z1) '(1 0 0)))))

(defun fixture-emit-oriented-quad (data part a b c d normal)
  "Emit quad A B C D facing NORMAL, whichever way round its corners run."
  (let* ((normal (mapcar (lambda (x) (coerce x 'single-float)) normal))
         (u (mapcar #'- b a))
         (v (mapcar #'- c a))
         (cross (list (- (* (second u) (third v)) (* (third u) (second v)))
                      (- (* (third u) (first v)) (* (first u) (third v)))
                      (- (* (first u) (second v)) (* (second u) (first v)))))
         (normals (list normal normal normal normal)))
    (if (minusp (reduce #'+ (mapcar #'* cross normal)))
        (fixture-emit-quad data a d c b part normals '(0 0 0 0))
        (fixture-emit-quad data a b c d part normals '(0 0 0 0)))))

(defun emit-side-brace (data axis side outer width)
  "A diagonal board on the side face across AXIS (0 X, 1 Y) at SIDE (0 or 1).
It runs corner to corner of the face's opening, standing OUTER out of it."
  (let* ((sign (if (zerop side) -1 1))
         (plane (if (zerop side) 0.06 0.94))
         (face (+ plane (* sign outer)))
         (half (/ width 2 (sqrt 2)))
         (low 0.16) (high 0.84)
         ;; In-face coordinates: S along the face, T up.
         (corners (list (list (- low half) (+ low half))
                        (list (+ low half) (- low half))
                        (list (+ high half) (- high half))
                        (list (- high half) (+ high half)))))
    (labels ((lift (corner depth)
               (if (zerop axis)
                   (list depth (first corner) (second corner))
                   (list (first corner) depth (second corner))))
             (in-face (s z)
               (let ((length (sqrt (+ (* s s) (* z z)))))
                 (if (zerop axis)
                     (list 0 (/ s length) (/ z length))
                     (list (/ s length) 0 (/ z length)))))
             (quad (a b c d normal)
               (fixture-emit-oriented-quad data +fixture-part-brace+ a b c d normal)))
      (destructuring-bind (o0 o1 o2 o3) (mapcar (lambda (c) (lift c face)) corners)
        (destructuring-bind (i0 i1 i2 i3) (mapcar (lambda (c) (lift c plane)) corners)
          (quad o0 o1 o2 o3 (if (zerop axis) (list sign 0 0) (list 0 sign 0)))
          (quad i0 i3 o3 o0 (in-face -1 1))
          (quad i1 i2 o2 o1 (in-face 1 -1))
          (quad i0 i1 o1 o0 (in-face -1 -1))
          (quad i2 i3 o3 o2 (in-face 1 1)))))))

(defun emit-crate (data)
  (let ((a 0.03) (b 0.16) (c 0.84) (d 0.97))
    ;; The boarded box itself, recessed within its frame.
    (emit-box data +fixture-part-panel+ '(0.06 0.06 0.06) '(0.94 0.94 0.94))
    ;; Corner posts run full height; rails run between them.
    (dolist (x (list (list a b) (list c d)))
      (dolist (y (list (list a b) (list c d)))
        (emit-box data +fixture-part-frame+
                  (list (first x) (first y) a) (list (second x) (second y) d))))
    (dolist (z (list (list a b) (list c d)))
      (dolist (y (list (list a b) (list c d)))
        (emit-box data +fixture-part-frame+
                  (list b (first y) (first z)) (list c (second y) (second z))))
      (dolist (x (list (list a b) (list c d)))
        (emit-box data +fixture-part-frame+
                  (list (first x) b (first z)) (list (second x) c (second z)))))
    (dotimes (axis 2)
      (dotimes (side 2)
        (emit-side-brace data axis side 0.018 0.11)))))

(defun %make-fixture-body-vertex-data ()
  "The barrel's vertices, then the crate's, and each body's vertex range."
  (let ((data (make-fixture-body-builder))
        (ranges (make-array (length luft.terrain::+material-bodies+)
                            :initial-element '(0 0))))
    (flet ((body (code emit)
             (let ((first (/ (fill-pointer data) +fixture-body-vertex-scalar-count+)))
               (funcall emit data)
               (setf (aref ranges code)
                     (list first (- (/ (fill-pointer data)
                                       +fixture-body-vertex-scalar-count+)
                                    first))))))
      (body (position :barrel luft.terrain::+material-bodies+) #'emit-barrel)
      (body (position :crate luft.terrain::+material-bodies+) #'emit-crate))
    (values (make-array (length data) :element-type 'single-float
                                      :initial-contents data)
            ranges)))
