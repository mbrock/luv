(in-package #:luft.terrain)

;;; Fixture bodies.  A material kind may name a drawn body (a barrel, a crate)
;;; instead of the cubical star surface.  Its cells stay solid in the scene's
;;; fibers, so walking, editing, and light are unchanged; the mesher alone is
;;; shown those cells as air, so neighbours face them properly, and each such
;;; cell of an owner chunk becomes one body instance for the renderer.

(defconstant +fixture-body-scalar-count+ 16
  "Floats per body: corner/code, side/yaw, top/seed, and sampled voxel light.")

(defun material-body-table (placement-vocabulary)
  "A vector by placement offset of NIL or (CODE SIDE-TONE TOP-TONE)."
  (let* ((placements
           (domains:identity-vocabulary-members placement-vocabulary))
         (table (make-array (length placements) :initial-element nil))
         (any nil))
    (loop for placement across placements
          for offset from 0
          for kind = (material-placement-kind placement)
          for body = (material-kind-body kind)
          unless (eq body :none)
            do (multiple-value-bind (top side) (material-kind-oriented-tones kind)
                 (setf any t
                       (aref table offset)
                       (list (position body +material-bodies+) side top))))
    (and any table)))

(defun material-chunk-body-cells (table material-chunk)
  "The (CELL . ENTRY) of every body cell in MATERIAL-CHUNK, a cell table."
  (let ((cells nil))
    (when (and table material-chunk)
      (maphash (lambda (cell offset)
                 (let ((entry (and (< offset (length table)) (aref table offset))))
                   (when entry (push (cons cell entry) cells))))
               material-chunk))
    cells))

(defun fibers-without-cells (fibers cells)
  "FIBERS with each of the (CELL . ENTRY) CELLS cleared, or FIBERS itself."
  (if (null cells)
      fibers
      (let ((copy (luft:copy-chunk-fibers fibers)))
        (loop for (cell) in cells
              do (setf (luft:fibers-cell-bit copy (luft:site-x cell)
                                             (luft:site-y cell) (luft:site-z cell))
                       0))
        copy)))

(defun fixture-body-seed (x y z)
  "A stable unit hash of cell X, Y, Z."
  (let ((h (logand #xffffffff
                   (logxor (* x 73856093) (* y 19349663) (* z 83492791)))))
    (setf h (logand #xffffffff (* (logxor h (ash h -13)) 1274126177)))
    (/ (float (logand h #xffff)) 65536.0)))

(defun pack-fixture-bodies (cells)
  "Pack the (CELL . (CODE SIDE TOP)) CELLS into body rows sorted by code."
  (let* ((sorted (sort (copy-list cells)
                       (lambda (a b)
                         (or (< (second a) (second b))
                             (and (= (second a) (second b))
                                  (< (car a) (car b)))))))
         (data (make-array (* +fixture-body-scalar-count+ (length sorted))
                           :element-type 'single-float)))
    (loop for (cell code side top) in sorted
          for offset from 0 by +fixture-body-scalar-count+
          for x = (luft:site-x cell)
          for y = (luft:site-y cell)
          for z = (luft:site-z cell)
          for seed = (fixture-body-seed x y z)
          for yaw = (if (eq (nth code +material-bodies+) :barrel)
                        (* seed (/ (* 2 pi) 16))
                        (* (floor (* seed 4)) (/ pi 2)))
          do (replace data
                      (mapcar (lambda (value) (coerce value 'single-float))
                              (list x y z code
                                    (first side) (second side) (third side) yaw
                                    (first top) (second top) (third top) seed
                                    0 0 0 0))
                      :start1 offset))
    data))
