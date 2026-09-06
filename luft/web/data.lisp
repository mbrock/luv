(in-package #:luft.web)

(defun array-form (sequence)
  `(array ,@(map 'list (lambda (item)
                        (if (or (listp item) (vectorp item))
                            (array-form item)
                            item))
                 sequence)))

(defun atlas-data ()
  "Export the production owned patches, in native XYZ mesh ticks.
No browser bevel algorithm: ownership, winding, and appearance selectors all
come from the same unfolded atlas that feeds the native renderer."
  (loop for star below 256
        collect (list (luft:star-atlas-owned-triangles star)
                      (luft:star-atlas-owned-appearance-masks star))))

(defun demo-cells ()
  "A finite editable highland with steps, concave corners, arches, and trees.
Coordinates use Luft's Z-up unit-cell convention; material zero is air."
  (let ((cells (make-hash-table :test #'equal)))
    (labels ((cell (x y z kind)
               (setf (gethash (list x y z) cells) kind))
             (box (x0 y0 z0 x1 y1 z1 kind)
               (loop for x from x0 below x1 do
                 (loop for y from y0 below y1 do
                   (loop for z from z0 below z1 do (cell x y z kind))))))
      (loop for x below 48 do
        (loop for y below 48
              for radius = (sqrt (+ (expt (- x 24) 2)
                                   (expt (- y 24) 2)))
              for height = (max 1 (floor (+ 4 (* 2 (sin (* x 0.17)))
                                             (* 2 (cos (* y 0.21)))
                                             (* 3 (max 0 (- 1 (/ radius 25)))))))
              do (box x y 0 (1+ x) (1+ y) height 1)))
      ;; A low stone terrace and stair leading through a double arch.
      (box 17 16 0 32 31 9 2)
      (loop for step below 6 do
        (box 21 (+ 10 step) 0 26 (+ 11 step) (+ 4 step) 2))
      (dolist (x '(18 24 30)) (box x 24 9 (+ x 2) 26 15 2))
      (box 18 24 15 32 26 17 2)
      (dolist (x '(20 26))
        (cell x 24 14 2) (cell (+ x 3) 24 14 2)
        (cell x 25 14 2) (cell (+ x 3) 25 14 2))
      ;; A small crystal on its own plinth and a deliberately notched wall.
      (box 27 18 9 30 21 10 2)
      (box 28 19 10 29 20 13 4)
      (cell 27 19 11 4) (cell 29 19 11 4)
      (box 18 28 9 30 30 11 2)
      (loop for x from 18 below 30 by 2 do (box x 28 11 (1+ x) 30 12 2))
      (dolist (position '((9 18) (12 32) (36 32) (37 14) (7 8)))
        (destructuring-bind (x y) position
          (let ((ground (loop for z below 16 when (gethash (list x y z) cells)
                               maximize (1+ z))))
            (box x y ground (1+ x) (1+ y) (+ ground 5) 3)
            (box (- x 2) (- y 2) (+ ground 3)
                 (+ x 3) (+ y 3) (+ ground 6) 5)
            (box (1- x) (1- y) (+ ground 6)
                 (+ x 2) (+ y 2) (+ ground 7) 5))))
      (loop for point being the hash-keys of cells using (hash-value kind)
            collect (append point (list kind))))))
