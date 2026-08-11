(defpackage #:luv/examples/tests
  (:use #:cl #:rove #:luv))

(in-package #:luv/examples/tests)

(deftest little-world-is-deterministic-and-chunked
  (let ((first (make-little-block-world :seed 77))
        (second (make-little-block-world :seed 77)))
    (ok (= (length (resident-world-chunks first)) 9))
    (ok (typep (block-world-source first) 'little-world-source))
    (ok (= (little-world-source-seed (block-world-source first)) 77))
    (ok
     (loop for x from -16 below 32
           always
           (loop for z from -16 below 32
                 always
                 (loop for y below 16
                       always
                       (multiple-value-bind (first-block first-status)
                           (block-at first x y z)
                         (multiple-value-bind (second-block second-status)
                             (block-at second x y z)
                           (and (eq first-status :resident)
                                (eq second-status :resident)
                                (eq first-block second-block))))))))
    (multiple-value-bind (block status) (block-at first 32 0 0)
      (ok (null block))
      (ok (eq status :absent))))
  (let* ((source (make-instance 'little-world-source :seed 77))
         (world (make-block-world :source source)))
    (materialize-little-world-chunk source world 0 0)
    (let ((revision (block-world-revision world)))
      (materialize-little-world-chunk source world 0 0)
      (ok (= (block-world-revision world) revision)))))

(deftest meshing-and-editing-cross-a-chunk-boundary
  (let ((world (make-block-world :chunk-width 2
                                 :chunk-height 2
                                 :chunk-depth 2)))
    (ensure-world-chunk world 0 0 0)
    (ensure-world-chunk world 1 0 0)
    (setf (block-at world 1 0 0) luv::*stone-block*
          (block-at world 2 0 0) luv::*stone-block*)
    (let ((mesher (make-instance 'exposed-face-mesher)))
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 10))
      (let ((revision (block-world-revision world)))
        (setf (block-at world 2 0 0) nil)
        (ok (= (block-world-revision world) (1+ revision))))
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 6))
      (setf (block-at world 2 0 0) luv::*stone-block*)
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 10)))))

(deftest camera-edits-the-resident-lattice
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (camera (make-instance 'fly-camera
                                :x 0.5 :y 1.5 :z 1.5
                                :yaw (/ pi 2) :pitch 0.0))
         (demo (make-instance 'cube-world-demo
                              :world world
                              :camera camera
                              :selected-block luv::*dirt-block*)))
    (ensure-world-chunk world 0 0 0)
    ;; The second stone means placing after removing the first still has a
    ;; solid target beyond the empty adjacent site.
    (setf (block-at world 2 1 1) luv::*stone-block*
          (block-at world 3 1 1) luv::*stone-block*)
    (multiple-value-bind (coordinate status)
        (edit-cube-world-block demo :remove)
      (ok (eq status :edited))
      (ok (= (world-coordinate-x coordinate) 2))
      (ok (null (block-at world 2 1 1))))
    (multiple-value-bind (coordinate status)
        (edit-cube-world-block demo :place)
      (ok (eq status :edited))
      (ok (= (world-coordinate-x coordinate) 2))
      (ok (eq (block-at world 2 1 1) luv::*dirt-block*)))))
