(defpackage #:luv/examples/tests
  (:use #:cl #:rove #:luv))

(in-package #:luv/examples/tests)

(defclass gated-production-request (luv::production-request)
  ((gate :initarg :gate :reader gated-production-request-gate)
   (value :initarg :value :reader gated-production-request-value)))

(defmethod luv::perform-production-request ((request gated-production-request))
  (sb-thread:wait-on-semaphore (gated-production-request-gate request))
  (gated-production-request-value request))

(defun production-system-active-request (system)
  (sb-thread:with-mutex ((luv::production-system-lock system))
    (luv::production-system-active-request system)))

(defun wait-until (predicate &key (timeout 2.0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop until (funcall predicate)
          when (>= (get-internal-real-time) deadline)
            do (return nil)
          do (sleep 0.001)
          finally (return t))))

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
                           (describe-block-allocatingly first x y z)
                         (multiple-value-bind (second-block second-status)
                             (describe-block-allocatingly second x y z)
                           (and (eq first-status :resident)
                                (eq second-status :resident)
                                (eq first-block second-block))))))))
    (multiple-value-bind (block status) (describe-block-allocatingly first 80 0 0)
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
    (ok (describe-block-allocatingly world 1 1 1))
    (edit-block-at nil world 1 1 1)
    (multiple-value-bind (block present-p)
        (block-edit-at (little-world-source-edits source) 1 1 1)
      (ok present-p)
      (ok (null block)))
    (ok (= (block-edit-overlay-count (little-world-source-edits source)) 1))
    (rematerialize-little-world-chunk source world 0 0)
    (multiple-value-bind (block status) (describe-block-allocatingly world 1 1 1)
      (ok (eq status :resident))
      (ok (null block)))
    ;; Explicit placement into generated air is an overlay value too.
    (edit-block-at luv::*stone-block* world 2 14 2)
    (rematerialize-little-world-chunk source world 0 0)
    (ok (eq (describe-block-allocatingly world 2 14 2) luv::*stone-block*))
    (ok (= (block-edit-overlay-count (little-world-source-edits source)) 2))))

(deftest little-world-residency-follows-a-bounded-window
  (let* ((world (make-little-block-world :chunk-radius 1 :seed 31))
         (source (block-world-source world)))
    (edit-block-at nil world 1 1 1)
    (multiple-value-bind (entering leaving)
        (center-little-world-residency source world 2 0 :radius 1)
      (ok (= (length entering) 6))
      (ok (= (length leaving) 6)))
    (ok (= (length (resident-world-chunks world)) 9))
    (multiple-value-bind (chunk present-p) (world-chunk-at world 0 0 0)
      (ok (null chunk))
      (ok (null present-p)))
    (center-little-world-residency source world 0 0 :radius 1)
    (multiple-value-bind (block status) (describe-block-allocatingly world 1 1 1)
      (ok (eq status :resident))
      (ok (null block)))
    (let ((revision (block-world-revision world)))
      (multiple-value-bind (entering leaving)
          (center-little-world-residency source world 0 0 :radius 1)
        (ok (null entering))
        (ok (null leaving)))
      (ok (= (block-world-revision world) revision)))))

(deftest block-atlas-and-mesh-vertices-carry-material-readings
  (let ((atlas (make-block-texture-atlas)))
    (ok (equal (array-dimensions atlas) '(16 144)))
    (ok (subtypep (array-element-type atlas) '(unsigned-byte 32)))
    (ok (= (ldb (byte 8 24) (aref atlas 8 8)) 255))
    (ok (/= (aref atlas 8 8) (aref atlas 8 (+ 8 (* 3 16))))))
  (flet ((face (name)
           (find name luv::*block-faces* :key #'block-face-name)))
    (ok (= (block-face-tile luv::*grass-block* (face :top)) 0))
    (ok (= (block-face-tile luv::*grass-block* (face :front)) 1))
    (ok (= (block-face-tile luv::*grass-block* (face :bottom)) 2))
    (ok (= (block-face-tile luv::*wood-block* (face :top)) 5))
    (ok (= (block-face-tile luv::*sand-block* (face :top)) 7))
    (ok (= (block-face-tile luv::*snow-block* (face :top)) 8))
    (ok (= (length (placeable-block-kinds)) 7)))
  (let ((world (make-block-world :chunk-width 2
                                 :chunk-height 2
                                 :chunk-depth 2)))
    (ensure-world-chunk world 0 0 0)
    (setf (describe-block-allocatingly world 0 0 0) luv::*stone-block*)
    (let ((mesh (mesh-block-world (make-instance 'exposed-face-mesher) world)))
      (ok (= (length (block-mesh-vertices mesh))
             (* 9 (block-mesh-vertex-count mesh)))))))

(deftest little-world-has-readable-biome-materials
  (let ((source (make-instance 'little-world-source :seed 121))
        (materials (make-hash-table :test #'eq)))
    (loop for x from -96 to 96 by 4 do
      (loop for z from -96 to 96 by 4
            for surface = (little-world-surface-height source x z 16)
            do (setf (gethash
                      (little-world-surface-material
                       source x z surface 16)
                      materials)
                     t)))
    (ok (gethash luv::*grass-block* materials))
    (ok (gethash luv::*sand-block* materials))
    (ok (gethash luv::*snow-block* materials))))

(deftest crosshair-and-numbered-materials-are-playable-state
  (let* ((vertices (luv::make-block-world-crosshair-vertices 960 640))
         (demo (make-instance 'cube-world-demo
                              :selected-block luv::*stone-block*)))
    (ok (= (length vertices)
           (* luv::+block-world-crosshair-vertex-count+ 6)))
    (ok (eq (select-cube-world-block demo 1) luv::*grass-block*))
    (ok (eq (cube-world-demo-selected-block demo) luv::*grass-block*))
    (ok (eq (select-cube-world-block demo 7) luv::*snow-block*))
    (ok (null (select-cube-world-block demo 8)))))

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
        (setf (describe-block-allocatingly world x 0 z) luv::*stone-block*)))
    ;; Gravity settles the body exactly on the block tops.
    (dotimes (step 240)
      (declare (ignorable step))
      (step-block-world-player player world camera keys (/ 1d0 120d0)))
    (ok (< (abs (- (player-y player) 1d0)) 1d-5))
    (ok (player-grounded-p player))
    (ok (< (abs (- (camera-y camera) 2.62d0)) 1d-5))
    ;; A held right input accelerates into, but not through, a two-block wall.
    (setf (describe-block-allocatingly world 3 1 1) luv::*stone-block*
          (describe-block-allocatingly world 3 2 1) luv::*stone-block*
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
    (setf (describe-block-allocatingly world 1 0 0) luv::*stone-block*
          (describe-block-allocatingly world 2 0 0) luv::*stone-block*)
    (let ((mesher (make-instance 'exposed-face-mesher)))
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 10))
      (let ((revision (block-world-revision world)))
        (setf (describe-block-allocatingly world 2 0 0) nil)
        (ok (= (block-world-revision world) (1+ revision))))
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 6))
      (setf (describe-block-allocatingly world 2 0 0) luv::*stone-block*)
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 10)))))

(deftest chunk-mesh-is-exactly-sized-and-preserves-the-public-emitter
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (describe-block-allocatingly world 0 0 0) luv::*stone-block*)
    (let* ((mesh (mesh-block-chunk mesher world chunk))
           (vertices (block-mesh-vertices mesh)))
      (ok (= (block-mesh-face-count mesh) 6))
      (ok (= (length vertices)
             (* (block-mesh-face-count mesh)
                luv::+block-mesh-floats-per-face+)))
      (ok (= (array-total-size vertices) (length vertices))))
    ;; Tools may still emit a single semantic face through the exported API;
    ;; the optimized neighborhood object remains an implementation detail.
    (let ((vertices
            (make-array luv::+block-mesh-floats-per-face+
                        :element-type 'single-float :fill-pointer 0)))
      (emit-block-face mesher world vertices luv::*stone-block*
                       (find :top luv::*block-faces*
                             :key #'block-face-name)
                       0 0 0)
      (ok (= (length vertices) luv::+block-mesh-floats-per-face+)))))

(deftest immutable-mesh-snapshot-is-bit-identical-to-owner-side-meshing
  (let* ((world (make-little-block-world :chunk-radius 1 :seed 121))
         (chunk (world-chunk-at world 0 0 0))
         (mesher (make-instance 'exposed-face-mesher))
         (stamp (chunk-mesh-dependency-stamp world chunk))
         (snapshot (make-block-mesh-snapshot world chunk stamp))
         (direct (mesh-block-chunk mesher world chunk))
         (copied (mesh-block-snapshot mesher snapshot)))
    (ok (equal stamp (block-mesh-snapshot-dependency-stamp snapshot)))
    (ok (= (block-mesh-face-count direct) (block-mesh-face-count copied)))
    (ok (= (block-mesh-vertex-count direct) (block-mesh-vertex-count copied)))
    (ok (equalp (block-mesh-vertices direct) (block-mesh-vertices copied)))
    (setf (describe-block-allocatingly world 0 0 0) nil)
    (ok (equalp (block-mesh-vertices copied)
                (block-mesh-vertices (mesh-block-snapshot mesher snapshot))))))

(deftest production-system-coalesces-desired-work-and-stops-cooperatively
  (let ((system (luv::make-single-worker-production-system
                 :name "luv production test")))
    (unwind-protect
         (let* ((first
                  (make-instance
                   'luv::block-chunk-load-request
                   :key '(:load (0 0 0)) :priority 4
                   :seed 1 :demand-token 1
                   :width 8 :height 8 :depth 8))
                (latest
                  (make-instance
                   'luv::block-chunk-load-request
                   :key '(:load (0 0 0)) :priority 0
                   :seed 2 :demand-token 2
                   :width 8 :height 8 :depth 8)))
           (luv::schedule-production-request system first)
           (luv::schedule-production-request system latest)
           (multiple-value-bind (result present-p)
               (sb-concurrency:receive-message
                (luv::production-system-result-mailbox system) :timeout 5.0)
             (ok present-p)
             (ok (null (luv::production-result-condition result)))
             (ok (<= (luv::production-system-pending-count system) 2))))
      (luv::stop-production-system system))
    (ok (not (sb-thread:thread-alive-p
              (luv::production-system-thread system))))))

(deftest production-system-keeps-one-result-behind-its-owner
  (let* ((system (luv::make-single-worker-production-system
                  :name "luv production backpressure test"))
         (first-gate (sb-thread:make-semaphore :count 0))
         (second-gate (sb-thread:make-semaphore :count 0))
         (first (make-instance 'gated-production-request
                               :key :first :gate first-gate :value :first))
         (second (make-instance 'gated-production-request
                                :key :second :gate second-gate :value :second)))
    (unwind-protect
         (progn
           (luv::schedule-production-request system first)
           (ok (wait-until
                (lambda () (eq (production-system-active-request system)
                               first))))
           ;; Scheduling while FIRST is active must remain desired work, not a
           ;; second queued wake which can run behind an unread first result.
           (luv::schedule-production-request system second)
           (sb-thread:signal-semaphore first-gate)
           (ok (wait-until
                (lambda ()
                  (and (= 1 (sb-concurrency:mailbox-count
                             (luv::production-system-result-mailbox system)))
                       (not (eq (production-system-active-request system)
                                first))))))
           (ok (null (production-system-active-request system)))
           (ok (= 1 (sb-concurrency:mailbox-count
                     (luv::production-system-result-mailbox system))))
           (ok (nth-value
                1 (gethash :second (luv::production-system-desired system))))
           (multiple-value-bind (result present-p)
               (luv::receive-production-result-no-hang system)
             (ok present-p)
             (ok (eq (luv::production-result-value result) :first)))
           (ok (wait-until
                (lambda () (eq (production-system-active-request system)
                               second))))
           (sb-thread:signal-semaphore second-gate)
           (multiple-value-bind (result present-p)
               (sb-concurrency:receive-message
                (luv::production-system-result-mailbox system) :timeout 2.0)
             (ok present-p)
             (ok (eq (luv::production-result-value result) :second))))
      (sb-thread:signal-semaphore first-gate)
      (sb-thread:signal-semaphore second-gate)
      (luv::stop-production-system system))))

(deftest prebuilt-world-remains-desired-for-asynchronous-meshing
  (let* ((world (make-block-world :chunk-width 8
                                  :chunk-height 8
                                  :chunk-depth 8))
         (first (ensure-world-chunk world -1 0 2))
         (second (ensure-world-chunk world 3 0 -4))
         (system (luv::make-single-worker-production-system
                  :name "luv static residency test"))
         (demo (make-instance 'cube-world-demo
                              :world world
                              :player (make-instance 'block-world-player
                                                     :x 0d0 :y 0d0 :z 0d0)
                              :production-system system)))
    (unwind-protect
         (progn
           (luv::maintain-cube-world-residency demo)
           (ok (gethash (luv::block-chunk-key first)
                        (cube-world-demo-desired-chunks demo)))
           (ok (gethash (luv::block-chunk-key second)
                        (cube-world-demo-desired-chunks demo)))
           (remove-world-chunk world -1 0 2)
           (luv::maintain-cube-world-residency demo)
           (ok (not (gethash (luv::block-chunk-key first)
                             (cube-world-demo-desired-chunks demo))))
           (ok (gethash (luv::block-chunk-key second)
                        (cube-world-demo-desired-chunks demo))))
      (luv::stop-production-system system))))

(deftest chunk-mesh-products-have-narrow-neighbor-dependencies
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (left (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (describe-block-allocatingly world 3 1 1) luv::*stone-block*
          (describe-block-allocatingly world 4 1 1) luv::*stone-block*)
    (ok (= (block-mesh-face-count (mesh-block-chunk mesher world left)) 5))
    (ok (= (block-mesh-face-count (mesh-block-chunk mesher world right)) 5))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      ;; This changes RIGHT, but not the boundary LEFT's mesh observes.
      (setf (describe-block-allocatingly world 5 2 2) luv::*stone-block*)
      (ok (equal stamp (chunk-mesh-dependency-stamp world left)))
      ;; This touches RIGHT's -X boundary and must invalidate LEFT.
      (setf (describe-block-allocatingly world 4 2 2) luv::*stone-block*)
      (ok (not (equal stamp (chunk-mesh-dependency-stamp world left)))))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      (remove-world-chunk world 0 0 0)
      (let ((replacement (ensure-world-chunk world 0 0 0)))
        (ok (not (equal stamp
                        (chunk-mesh-dependency-stamp world replacement))))))))

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
    (setf (describe-block-allocatingly world 2 1 1) luv::*stone-block*
          (describe-block-allocatingly world 3 1 1) luv::*stone-block*)
    (multiple-value-bind (coordinate status)
        (edit-cube-world-block demo :remove)
      (ok (eq status :edited))
      (ok (= (world-coordinate-x coordinate) 2))
      (ok (null (describe-block-allocatingly world 2 1 1))))
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
        (ok (null (describe-block-allocatingly world 2 1 1)))))
    (multiple-value-bind (coordinate status)
        (edit-cube-world-block demo :place)
      (ok (eq status :edited))
      (ok (= (world-coordinate-x coordinate) 2))
      (ok (eq (describe-block-allocatingly world 2 1 1) luv::*dirt-block*)))))
