;;;; Exact, owned Luft atlas geometry for the independent CPU probes.
;;;; Usage: ./env sbcl --script .../export.lisp > build/far-field-meshes.json
(defpackage #:luft (:use #:cl))
(in-package #:luft)
(defconstant +mesh-cell-size+ 8)
(let ((*standard-output* *error-output*))
  (load "luft/star-geometry.lisp")
  (load "luft/star-atlas.lisp")
  (load "luft/star-table.lisp"))

(defpackage #:far-field-export (:use #:cl))
(in-package #:far-field-export)

(defun cells (name)
  (let ((table (make-hash-table :test #'equal)))
    (flet ((put (x y z material)
             (setf (gethash (list x y z) table) material)))
      (ecase name
        (:ridge
         (loop for x below 6 do
           (loop for y below 2 do
             (loop for z below 2 do (put x y z 0)))))
        (:junction
         (dolist (p '((0 0 0) (1 0 0) (0 1 0) (0 0 1) (1 1 0)))
           (apply #'put (append p '(0)))))
        (:sheets
         (dolist (p '((0 -1 -1) (-1 0 -1) (0 0 0) (-1 -1 0)))
           (apply #'put (append p '(1)))))
        (:cave
         (loop for x from -3 to 3 do
           (loop for z below 5 do
             (unless (and (<= -1 x 1) (<= 1 z 3)) (put x 0 z 0))
             (put x 4 z 2))
           (loop for y below 5 do (put x y -1 1))))
        (:terrain
         (loop for x below 6 do
           (loop for y below 6
                 for height = (+ 1 (floor (+ x y) 3)) do
                   (loop for z below height do
                     (put x y z (if (= z (1- height)) 1 0))))))
        (:edited-cave
         (setf table (cells :cave))
         (put 0 0 2 3))))
    table))

(defun mesh (cells width)
  (let ((triangles nil) (materials nil))
    (maphash
     (lambda (site star)
       (loop for triangle in (luft::star-atlas-owned-triangles star)
             for masks in (luft::star-atlas-owned-appearance-masks star)
             for sample = (position 1 (loop for i below 8 collect (ldb (byte 1 i) (first masks))))
             for cell = (loop for s in site for axis from 0
                              collect (+ s (if (logbitp axis sample) 0 -1))) do
               (push (gethash cell cells) materials)
               (push (loop for point in triangle collect
                       (loop for c in point for s in site
                             for a = (luft::%flat-face-coordinate c)
                             collect (+ (* s 8) a (* width (- c a)))))
                     triangles)))
     (luft::star-surface-sites cells))
    (values (nreverse triangles) (nreverse materials))))

(defun json-array (value)
  (if (listp value)
      (progn
        (write-char #\[)
        (loop for tail on value do
          (json-array (first tail))
          (when (rest tail) (write-char #\,)))
        (write-char #\]))
      (princ value)))

(write-char #\{)
(loop for name in '(:ridge :junction :sheets :cave :terrain :edited-cave)
      for first = t then nil do
        (unless first (write-char #\,))
        (format t "~S:{\"triangles\":" (string-downcase name))
        (multiple-value-bind (triangles material)
            (mesh (cells name) (if (eq name :terrain) 1 2))
          (json-array triangles)
          (write-string ",\"material\":")
          (json-array material)
          (write-char #\})))
(write-line "}")
