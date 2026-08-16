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

(deftest linear-gradient-coordinates-and-colors
  (let ((gradient
          (mcluv:make-linear-gradient
           10 20 110 20 clim:+black+ clim:+white+)))
    (ok (= 0.0 (mcluv::gradient-coordinate gradient 10 20)))
    (ok (= 0.5 (mcluv::gradient-coordinate gradient 60 20)))
    (ok (= 1.0 (mcluv::gradient-coordinate gradient 110 20)))
    (multiple-value-bind (red green blue alpha)
        (mcluv::color-rgba (mcluv::design-ink gradient 60 20))
      (ok (= 0.5 red))
      (ok (= 0.5 green))
      (ok (= 0.5 blue))
      (ok (= 1.0 alpha)))))

(deftest radial-gradient-coordinates
  (let ((gradient
          (mcluv:make-radial-gradient
           50 60 25 clim:+white+ clim:+black+)))
    (ok (= 0.0 (mcluv::gradient-coordinate gradient 50 60)))
    (ok (= 0.5 (mcluv::gradient-coordinate gradient 62.5 60)))
    (ok (= 1.0 (mcluv::gradient-coordinate gradient 50 85)))))

(deftest relief-design-is-a-semantic-height-bearing-ink
  (let* ((albedo (clim:make-rgb-color 0.2 0.4 0.7))
         (relief (mcluv:make-relief-design albedo 6.5))
         (transformed
           (clim:transform-region
            (clim:make-translation-transformation 10 20) relief)))
    (ok (eq albedo (mcluv::design-ink relief 12 14)))
    (ok (= 6.5 (mcluv:design-height relief)))
    (ok (= 0 (mcluv:design-height albedo)))
    (ok (= 6.5 (mcluv:design-height transformed)))
    (ok (eq albedo (mcluv:relief-albedo transformed)))))

(deftest relief-roundrect-is-one-dense-analytic-command
  (let ((medium (fresh-gpu-medium)))
    (setf (clim:medium-ink medium)
          (mcluv:make-relief-design
           (clim:make-rgb-color 0.2 0.4 0.7) 7.0))
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 10 20 110 60 12 t)
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
               'mcluv::gpu-relief-analytic-command))
    ;; Six vertices, each containing five packed float32x3 attributes.
    (ok (= 90 (length (mcluv::gpu-medium-relief-vertices medium))))
    (ok (= 7.0 (aref (mcluv::gpu-medium-relief-vertices medium) 12)))
    (ok (zerop (length (mcluv::gpu-medium-vertices medium))))
    (ok (zerop (length (mcluv::gpu-medium-analytic-vertices medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(deftest gradient-roundrect-is-one-dense-analytic-command
  (let* ((medium (fresh-gpu-medium))
         (gradient
           (mcluv:make-linear-gradient
            10 20 110 20 clim:+black+ clim:+white+)))
    (setf (clim:medium-ink medium) gradient)
    (mcluv::medium-draw-analytic-rounded-rectangle*
     medium 10 20 110 60 12 t)
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
               'mcluv::gpu-gradient-analytic-command))
    ;; Six vertices, each containing seven packed float32x3 attributes.
    (ok (= 126 (length (mcluv::gpu-medium-gradient-vertices medium))))
    (ok (zerop (length (mcluv::gpu-medium-vertices medium))))
    (ok (zerop (length (mcluv::gpu-medium-analytic-vertices medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(defun tiny-image-pattern ()
  (clim:make-pattern
   (make-array '(4 4) :element-type '(unsigned-byte 32)
                       :initial-element #xff3366cc)
   nil))

(deftest draw-pattern-is-one-cached-image-command
  (let ((medium (fresh-gpu-medium))
        (pattern (tiny-image-pattern)))
    (climi::medium-draw-pattern* medium pattern 10 20)
    (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
    (let ((command (aref (mcluv::gpu-medium-commands medium) 0)))
      (ok (typep command 'mcluv::gpu-image-command))
      (ok (eq pattern
              (mcluv::gpu-image-paint-source
               (mcluv::gpu-image-command-design command)))))
    (ok (= 72 (length (mcluv::gpu-medium-image-vertices medium))))
    (ok (null (mcluv:gpu-medium-fallback-report medium)))))

(deftest transformed-image-paint-keeps-source-and-affine-coordinates
  (let* ((pattern (tiny-image-pattern))
         (transformation (clim:make-transformation 2 0.5 -0.25 3 40 60))
         (paint (clim:transform-region transformation pattern)))
    (ok (eq pattern (mcluv::gpu-image-paint-source paint)))
    (multiple-value-bind (x y)
        (clim:transform-position transformation 2 3)
      (multiple-value-bind (u v)
          (mcluv::gpu-image-paint-coordinate paint x y)
        (ok (< (abs (- u 0.5)) 1.0e-6))
        (ok (< (abs (- v 0.75)) 1.0e-6))))))

(deftest polygons-use-native-gradient-and-image-paints
  (dolist (paint
            (list (mcluv:make-linear-gradient
                   0 0 100 0 clim:+black+ clim:+white+)
                  (tiny-image-pattern)))
    (let ((medium (fresh-gpu-medium)))
      (setf (clim:medium-ink medium) paint)
      (clim:medium-draw-polygon*
       medium #(10 10 90 20 70 80 20 70) t t)
      (ok (= 1 (length (mcluv::gpu-medium-commands medium))))
      (ok (typep (aref (mcluv::gpu-medium-commands medium) 0)
                 (if (typep paint 'mcluv:linear-gradient)
                     'mcluv::gpu-gradient-analytic-command
                     'mcluv::gpu-image-command))))))

(deftest commands-retain-mcclim-clipping-for-native-scissors
  (let ((medium (fresh-gpu-medium)))
    (setf (clim:medium-clipping-region medium)
          (clim:make-rectangle* 0.1 0.2 0.8 0.9))
    (clim:medium-draw-rectangle* medium 0 0 1 1 t)
    (let ((clip
            (mcluv::gpu-analytic-command-clip
             (aref (mcluv::gpu-medium-commands medium) 0))))
      (ok clip)
      (ok (equal '(0.1 0.2 0.8 0.9) clip)))))
