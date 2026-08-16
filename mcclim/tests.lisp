(defpackage #:mcluv.tests
  (:use #:cl #:rove))

(in-package #:mcluv.tests)

(defun fresh-gpu-medium ()
  (make-instance 'mcluv:luv-gpu-medium))

(deftest filled-rectangles-become-one-analytic-command
  (let ((medium (fresh-gpu-medium)))
    (clim:medium-draw-rectangle* medium 10 20 110 60 t)
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
               'mcluv::gpu-analytic-command))
    (ok (= 72 (length (mcluv::gpu-medium-analytic-vertices medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(deftest full-ellipses-are-analytic-while-arcs-retain-the-fallback
  (let ((medium (fresh-gpu-medium)))
    (clim:medium-draw-ellipse*
     medium 80 60 35 8 -5 20 0 (* 2 pi) t)
    (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
               'mcluv::gpu-analytic-command))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))
    (clim:medium-draw-ellipse*
     medium 80 60 35 8 -5 20 0 pi t)
    (ok (find :ellipse (mcluv:gpu-medium-fallback-report medium)
              :key (lambda (entry) (getf entry :primitive))))))

(deftest roundrect-command-carries-the-semantic-radius
  (let ((medium (fresh-gpu-medium)))
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 10 20 110 60 12 t)
    (let ((vertices (mcluv::gpu-medium-analytic-vertices medium)))
      ;; Each vertex is position, local coordinate, half-size/radius, color.
      (ok (= 50.0 (aref vertices 6)))
      (ok (= 20.0 (aref vertices 7)))
      (ok (= 12.0 (aref vertices 8))))
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(deftest roundrect-has-a-native-mcclim-output-record
  (ok (find-class 'mcluv::draw-analytic-rounded-rectangle-output-record nil))
  (ok (typep (fdefinition 'mcluv::medium-draw-analytic-rounded-rectangle*)
             'generic-function)))
