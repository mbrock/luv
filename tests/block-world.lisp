(defpackage #:luv/examples/tests
  (:use #:cl #:rove #:luv))

(in-package #:luv/examples/tests)

(deftest little-world-is-deterministic-and-chunked
  (let ((first (make-little-block-world :seed 77))
        (second (make-little-block-world :seed 77)))
    (ok (= (length (resident-world-chunks first)) 81))
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
    (multiple-value-bind (block status) (block-at first 80 0 0)
      (ok (null block))
      (ok (eq status :absent))))
  (let* ((source (make-instance 'little-world-source :seed 77))
         (world (make-block-world :source source)))
    (materialize-little-world-chunk source world 0 0)
    (let ((revision (block-world-revision world)))
      (materialize-little-world-chunk source world 0 0)
      (ok (= (block-world-revision world) revision)))))

(deftest little-world-edits-survive-rematerialization
  (let* ((world (make-little-block-world :chunk-radius 0 :seed 31))
         (source (block-world-source world)))
    (ok (block-at world 1 1 1))
    (edit-block-at nil world 1 1 1)
    (multiple-value-bind (block present-p)
        (block-edit-at (little-world-source-edits source) 1 1 1)
      (ok present-p)
      (ok (null block)))
    (ok (= (block-edit-overlay-count (little-world-source-edits source)) 1))
    (rematerialize-little-world-chunk source world 0 0)
    (multiple-value-bind (block status) (block-at world 1 1 1)
      (ok (eq status :resident))
      (ok (null block)))
    ;; Explicit placement into generated air is an overlay value too.
    (edit-block-at luv::*stone-block* world 2 14 2)
    (rematerialize-little-world-chunk source world 0 0)
    (ok (eq (block-at world 2 14 2) luv::*stone-block*))
    (ok (= (block-edit-overlay-count (little-world-source-edits source)) 2))))

(deftest block-atlas-and-mesh-vertices-carry-material-readings
  (let ((atlas (make-block-texture-atlas)))
    (ok (equal (array-dimensions atlas) '(16 112)))
    (ok (subtypep (array-element-type atlas) '(unsigned-byte 32)))
    (ok (= (ldb (byte 8 24) (aref atlas 8 8)) 255))
    (ok (/= (aref atlas 8 8) (aref atlas 8 (+ 8 (* 3 16))))))
  (flet ((face (name)
           (find name luv::*block-faces* :key #'block-face-name)))
    (ok (= (block-face-tile luv::*grass-block* (face :top)) 0))
    (ok (= (block-face-tile luv::*grass-block* (face :front)) 1))
    (ok (= (block-face-tile luv::*grass-block* (face :bottom)) 2))
    (ok (= (block-face-tile luv::*wood-block* (face :top)) 5)))
  (let ((world (make-block-world :chunk-width 2
                                 :chunk-height 2
                                 :chunk-depth 2)))
    (ensure-world-chunk world 0 0 0)
    (setf (block-at world 0 0 0) luv::*stone-block*)
    (let ((mesh (mesh-block-world (make-instance 'exposed-face-mesher) world)))
      (ok (= (length (block-mesh-vertices mesh))
             (* 9 (block-mesh-vertex-count mesh)))))))

(deftest scalar-player-walks-collides-and-jumps
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (camera (make-instance 'fly-camera :x 1.5 :y 4.62 :z 1.5
                                            :yaw 0d0 :pitch 0d0))
         (player (make-instance 'block-world-player :x 1.5d0 :y 3d0
                                                    :z 1.5d0))
         (keys (make-hash-table :test #'eq)))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 4 do
      (loop for z below 4 do
        (setf (block-at world x 0 z) luv::*stone-block*)))
    ;; Gravity settles the body exactly on the block tops.
    (dotimes (step 240)
      (declare (ignorable step))
      (step-block-world-player player world camera keys (/ 1d0 120d0)))
    (ok (< (abs (- (player-y player) 1d0)) 1d-5))
    (ok (player-grounded-p player))
    (ok (< (abs (- (camera-y camera) 2.62d0)) 1d-5))
    ;; A held right input accelerates into, but not through, a two-block wall.
    (setf (block-at world 3 1 1) luv::*stone-block*
          (block-at world 3 2 1) luv::*stone-block*
          (gethash :d keys) t)
    (dotimes (step 120)
      (declare (ignorable step))
      (step-block-world-player player world camera keys (/ 1d0 120d0)))
    (ok (<= (player-x player) 2.700001d0))
    (ok (= (player-velocity-x player) 0d0))
    (remhash :d keys)
    ;; Jump is an edge request, not a second form of flying.
    (let ((ground-y (player-y player)))
      (step-block-world-player player world camera keys (/ 1d0 120d0)
                               :jump-p t)
      (ok (> (player-y player) ground-y))
      (ok (not (player-grounded-p player))))
    (dotimes (step 120)
      (declare (ignorable step))
      (step-block-world-player player world camera keys (/ 1d0 120d0)))
    (ok (< (abs (- (player-y player) 1d0)) 1d-5))
    (ok (player-grounded-p player))))

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

(deftest chunk-mesh-products-have-narrow-neighbor-dependencies
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (left (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (block-at world 3 1 1) luv::*stone-block*
          (block-at world 4 1 1) luv::*stone-block*)
    (ok (= (block-mesh-face-count (mesh-block-chunk mesher world left)) 5))
    (ok (= (block-mesh-face-count (mesh-block-chunk mesher world right)) 5))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      ;; This changes RIGHT, but not the boundary LEFT's mesh observes.
      (setf (block-at world 5 2 2) luv::*stone-block*)
      (ok (equal stamp (chunk-mesh-dependency-stamp world left)))
      ;; This touches RIGHT's -X boundary and must invalidate LEFT.
      (setf (block-at world 4 2 2) luv::*stone-block*)
      (ok (not (equal stamp (chunk-mesh-dependency-stamp world left)))))))

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
    (let ((occupied-demo
            (make-instance 'cube-world-demo
                           :world world :camera camera
                           :player (make-instance 'block-world-player
                                                  :x 2.5d0 :y 1d0 :z 1.5d0)
                           :selected-block luv::*dirt-block*)))
      (multiple-value-bind (coordinate status)
          (edit-cube-world-block occupied-demo :place)
        (ok (null coordinate))
        (ok (eq status :blocked))
        (ok (null (block-at world 2 1 1)))))
    (multiple-value-bind (coordinate status)
        (edit-cube-world-block demo :place)
      (ok (eq status :edited))
      (ok (= (world-coordinate-x coordinate) 2))
      (ok (eq (block-at world 2 1 1) luv::*dirt-block*)))))
