(defpackage #:luft.render.tests
  (:use #:cl #:rove #:luft.render)
  (:local-nicknames (#:vec3 #:luv.arithmetic.lisp.vec3)))

(in-package #:luft.render.tests)

(defun sky-pixel-p (pixels offset)
  ;; The clear colour is a pale blue: blue clearly above red.
  (> (aref pixels (+ offset 2)) (+ 30 (aref pixels offset))))

(defun count-pixels (pixels width height predicate &key (from-row 0)
                                                        (to-row height))
  (loop for y from from-row below to-row
        sum (loop for x below width
                  count (funcall predicate pixels (* 4 (+ x (* y width)))))))

(deftest demo-scene-terms-are-its-surface-in-whole-bricks
  (let* ((scene (make-demo-scene))
         (surface (scene-surface scene))
         (terms (scene-terms scene))
         (present (remove 0 terms)))
    (ok (zerop (mod (length terms) luft.render.shaders:+brick-size+)))
    (ok (= (scene-brick-count scene)
           (/ (length terms) luft.render.shaders:+brick-size+)))
    (ok (= (length present) (luft:chain-count surface)))
    (ok (every (lambda (term)
                 (= (luft:packed-term-coefficient term)
                    (luft:chain-coefficient
                     surface (luft:packed-term-site term))))
               present))
    (ok (every (lambda (term)
                 (= 2 (luft:site-spatial-dimension
                       (luft:packed-term-site term))))
               present))
    ;; The surface is closed: its boundary vanishes.
    (ok (zerop (luft:chain-count (luft:boundary-chain surface))))))

(deftest brick-spheres-enclose-their-faces
  (let* ((scene (make-demo-scene))
         (terms (scene-terms scene))
         (spheres (scene-bricks scene))
         (size luft.render.shaders:+brick-size+))
    (ok (= (length spheres) (* 4 (scene-brick-count scene))))
    (ok (loop for brick below (scene-brick-count scene)
              for center-x = (aref spheres (* 4 brick))
              for center-y = (aref spheres (+ 1 (* 4 brick)))
              for center-z = (aref spheres (+ 2 (* 4 brick)))
              for radius = (aref spheres (+ 3 (* 4 brick)))
              always
              (loop for index from (* brick size) below (* (1+ brick) size)
                    for term = (aref terms index)
                    always
                    (or (zerop term)
                        (flet ((reach (axis anchor center)
                                 (max (abs (- anchor center))
                                      (abs (- (if (luft:site-extends-p
                                                   (luft:packed-term-site term)
                                                   axis)
                                                  (1+ anchor)
                                                  anchor)
                                              center)))))
                          (let* ((site (luft:packed-term-site term))
                                 (dx (reach :x (luft:site-x site) center-x))
                                 (dy (reach :y (luft:site-y site) center-y))
                                 (dz (reach :z (luft:site-z site) center-z)))
                            (<= (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
                                (+ radius 1.0e-3))))))))))

#+darwin
(deftest the-demo-scene-renders-ground-under-sky
  (let* ((width 160)
         (height 100)
         (renderer (make-renderer :scene (make-demo-scene)
                                  :camera (make-fly-camera)
                                  :width width :height height)))
    (unwind-protect
         (progn
           (let* ((pixels (render-pixels renderer))
                  (sky-above (count-pixels pixels width height #'sky-pixel-p
                                           :to-row 10))
                  (ground-below (count-pixels
                                 pixels width height
                                 (lambda (pixels offset)
                                   (not (sky-pixel-p pixels offset)))
                                 :from-row 80)))
             (ok (= (* 4 width height) (length pixels)))
             (ok (> sky-above (* 0.9 10 width)))
             (ok (> ground-below (* 0.9 20 width))))
           ;; Turned straight up, every brick fails the frustum test and only
           ;; sky remains.
           (setf (camera-pitch (renderer-camera renderer)) 1.5)
           (let ((pixels (render-pixels renderer)))
             (ok (= (* width height)
                    (count-pixels pixels width height #'sky-pixel-p)))))
      (destroy-renderer renderer))))
