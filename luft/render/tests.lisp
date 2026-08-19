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

(deftest renderer-creation-steps-leave-traces-and-breadcrumbs
  (let ((trace (luv:make-cpu-trace :label "luft creation test"))
        (stream (make-string-output-stream)))
    (let ((luv:*log-stream* stream)
          (luv:*log-categories* '(:luft)))
      (ok (eq :created
              (luv:with-cpu-trace (trace)
                (luft.render::with-renderer-creation-step
                    (:luft/test-creation "test creation")
                  :created))))
      (let ((zones (luv:cpu-trace-zones trace)))
        (ok (= 1 (length zones)))
        (ok (eq :luft/test-creation
                (luv:cpu-trace-zone-name (first zones)))))
      (let ((log (get-output-stream-string stream)))
        (ok (search "begin test creation" log))
        (ok (search "complete test creation" log))
        (ok (not (search "interrupted test creation" log))))
      (handler-case
          (luft.render::with-renderer-creation-step
              (:luft/test-interruption "test interruption")
            (error "deliberate test interruption"))
        (error () nil))
      (let ((log (get-output-stream-string stream)))
        (ok (search "begin test interruption" log))
        (ok (search "interrupted test interruption" log))
        (ok (not (search "complete test interruption" log)))))))

(deftest demo-scene-sites-are-its-surface-in-whole-bricks
  (let* ((scene (make-demo-scene))
         (surface (scene-surface scene))
         (sites (scene-sites scene))
         (present (remove 0 sites)))
    (ok (zerop (mod (length sites) luft.render.shaders:+brick-size+)))
    (ok (= (scene-brick-count scene)
           (/ (length sites) luft.render.shaders:+brick-size+)))
    (ok (= (length present) (luft:chain-count surface)))
    (ok (every (lambda (site)
                 (luft:chain-site-p surface site))
               present))
    (ok (every (lambda (site)
                 (= 2 (luft:site-dimension site)))
               present))
    ;; The surface is closed: its boundary vanishes.
    (ok (zerop (luft:chain-count (luft:boundary-chain surface))))))

(deftest brick-spheres-enclose-their-faces
  (let* ((scene (make-demo-scene))
         (sites (scene-sites scene))
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
                    for site = (aref sites index)
                    always
                    (or (zerop site)
                        (flet ((reach (axis anchor center)
                                 (max (abs (- anchor center))
                                      (abs (- (if (luft:site-extends-p site axis)
                                                  (1+ anchor)
                                                  anchor)
                                              center)))))
                          (let* ((dx (reach :x (luft:site-x site) center-x))
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


(defun probe-scene ()
  "A floor with a block and an L-shaped stack: pure, mixed, and concave stars."
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (solid (luft:make-solid-chain domain)))
    (loop for x from 1 to 8
          do (loop for y from 1 to 8
                   do (setf (luft:solid-cell-p solid x y 0) t)))
    (setf (luft:solid-cell-p solid 4 4 1) t
          (luft:solid-cell-p solid 6 4 1) t
          (luft:solid-cell-p solid 6 5 1) t
          (luft:solid-cell-p solid 6 5 2) t)
    (make-scene domain :solid solid)))

#+darwin
(deftest shaped-surfaces-are-watertight-from-above
  ;; Straight down onto the floor, every pixel inside the floor is ground:
  ;; a crack between rounded faces would let the sky through.
  (let* ((width 200)
         (height 200)
         (*bevel-radius* 0.3)
         (renderer (make-renderer
                    :scene (probe-scene)
                    :camera (make-fly-camera
                             :position (vec3:make-vec3 5.0 5.0 9.0)
                             :yaw 0.0 :pitch -1.5
                             :field-of-view 0.75)
                    :width width :height height :style :bevel)))
    (unwind-protect
         (let ((pixels (render-pixels renderer)))
           (ok (zerop (count-pixels pixels width height #'sky-pixel-p
                                    :from-row 20 :to-row 180)))
           (setf (renderer-style renderer) :flat)
           (ok (zerop (count-pixels (render-pixels renderer) width height
                                    #'sky-pixel-p
                                    :from-row 20 :to-row 180)))
           (setf (renderer-style renderer) :chamfer)
           (ok (zerop (count-pixels (render-pixels renderer) width height
                                    #'sky-pixel-p
                                    :from-row 20 :to-row 180))))
      (destroy-renderer renderer))))
