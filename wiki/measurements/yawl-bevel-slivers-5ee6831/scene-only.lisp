;;;; Disposable ablation: change scene bevels, never the shadow geometry.
(in-package #:luft.render)

(defparameter *bevel-probe-original-tables* (symbol-function 'star-patch-tables))

(defun bevel-probe-flat-tables ()
  (let* ((control (multiple-value-list (funcall *bevel-probe-original-tables* t)))
         (flat (multiple-value-list (funcall *bevel-probe-original-tables* nil)))
         (ranges (sixth flat)))
    (dotimes (star 256)
      (setf (aref (aref ranges star) 2) (aref (aref (sixth control) star) 2)
            (aref (aref ranges star) 3) (aref (aref (sixth control) star) 3)))
    (values (first flat) (second flat) (third flat)
            (fourth control) (fifth control) ranges)))

(when (uiop:getenv "BEVEL_FLAT")
  ;; Leave *TERRAIN-BEVELS-P* true: construction must retain the same merged
  ;; shadow renderer and its exact inward shadow surface.
  (setf (symbol-function 'star-patch-tables)
        (lambda (bevels-p)
          (assert bevels-p)
          (bevel-probe-flat-tables))))
