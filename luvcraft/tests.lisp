(defpackage #:luv/luvcraft/tests
  (:use #:cl #:rove #:luv))

(in-package #:luv/luvcraft/tests)

(defclass recording-command-encoder (gpu-command-encoder)
  ((commands :initform nil :accessor recording-command-encoder-commands)))

(defmethod encode ((encoder recording-command-encoder) command)
  (push command (recording-command-encoder-commands encoder))
  encoder)

(defclass recording-chunk-window ()
  ((locations :initform nil :accessor recording-window-locations)))

(defmethod locate-chunk-window-site
    ((window recording-chunk-window) x y z)
  (push (list x y z) (recording-window-locations window))
  (values window 37 :available))

(deftest player-storage-publishes-quantities-without-wrapping-values
  (let* ((position (make-vec3 1d0 2d0 3d0))
         (velocity (make-vec3 4d0 5d0 6d0))
         (player (make-instance 'block-world-player
                                :position position :velocity velocity))
         (position-declaration
           (luv.arithmetic.records:record-slot-declaration
            'block-world-player 'luv::position))
         (velocity-declaration
           (luv.arithmetic.records:record-slot-declaration
            'block-world-player 'luv::velocity)))
    (ok (eq position (player-position player)))
    (ok (eq 'vec3
            (luv.arithmetic:declaration-representation-type
             position-declaration)))
    (ok (eq :world-position
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic:declaration-quantity-specification
              position-declaration))))
    (ok (eq :point
            (luv.arithmetic:quantity-specification-character
             (luv.arithmetic:declaration-quantity-specification
              position-declaration))))
    (ok (= 1
           (luv.arithmetic:quantity-specification-tensor-order
            (luv.arithmetic:declaration-quantity-specification
             velocity-declaration))))
    (ok (null
         (luv.arithmetic.records:record-slot-declaration
          'block-world-player 'luv::grounded-p)))
    (let ((predicted (luv::predict-player-position player 0.5d0)))
      (ok (equalp (make-vec3 3d0 4.5d0 6d0) predicted))
      (ok (eq position (player-position player)))
      (ok (eq velocity (player-velocity player))))
    (ok (compiled-function-p luv::*predict-player-position-function*))
    (ok (signals
         (luv.arithmetic.lisp:bind-lisp-arithmetic-realization
          luv::*predict-player-position-realization*
          (list velocity-declaration velocity-declaration
                luv::*player-frame-duration-declaration*)
          :actual-result-declaration position-declaration)
         'luv.arithmetic:declaration-compatibility-error))))

(deftest sky-frame-structure-publishes-quantities-without-changing-layout
  (let* ((sky (sky-frame-parameters
               (make-instance 'sky-clock)
               (make-default-sky-profile)))
         (direction (luv::sky-frame-parameters-sun-direction sky))
         (direction-declaration
           (luv.arithmetic.records:record-slot-declaration
            'luv::sky-frame-parameters 'luv::sun-direction))
         (fog-declaration
           (luv.arithmetic.records:record-slot-declaration
            'luv::sky-frame-parameters 'luv::fog-far)))
    (ok (typep sky 'luv::sky-frame-parameters))
    (ok (typep direction 'vec3))
    (ok (eq :world-direction
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic:declaration-quantity-specification
              direction-declaration))))
    (ok (eq 'single-float
            (luv.arithmetic:declaration-representation-type
             fog-declaration)))
    (ok (typep (luv::sky-frame-parameters-fog-far sky) 'single-float))))

(deftest semantic-owner-audit-exposes-camera-sky-material-and-timing-fields
  (dolist (claim
           '((fly-camera luv::yaw :camera-yaw)
             (fly-camera luv::sensitivity :look-sensitivity)
             (sky-clock luv::rate :sky-cycle-rate)
             (sky-clock luv::pinned-day-fraction :day-fraction)
             (luv::sky-keyframe luv::sun-color :linear-rgb)
             (luv::sky-keyframe luv::fog-far :view-distance)
             (block-kind luv::light-opacity :block-light-attenuation-step)
             (block-kind luv::surface-emission :material-emission)
             (luv::luvcraft-frame-sample luv::simulation-seconds
              :simulation-duration)
             (luv::luvcraft-frame-benchmark luv::drain-seconds
              :benchmark-drain-duration)
             (luv::production-result luv::elapsed-seconds
              :production-duration)
             (luv::luvcraft-lighting-state luv::last-latency-seconds
              :lighting-reconciliation-duration)
             (luvcraft-session luv::last-frame-time
              :monotonic-frame-time)
             (luvcraft-session luv::physics-accumulator
              :physics-accumulated-duration)))
    (destructuring-bind (record slot quantity) claim
      (let ((declaration
              (luv.arithmetic.records:record-slot-declaration record slot)))
        (ok declaration)
        (ok (eq quantity
                (luv.arithmetic:quantity-specification-name
                 (luv.arithmetic:declaration-quantity-specification
                  declaration))))))))

(deftest semantic-owner-audit-exposes-quantity-bearing-constants
  (dolist (claim
           '((luv::+player-physics-step+ :frame-duration double-float)
             (luv::+player-collision-epsilon+ :world-distance double-float)
             (luv::+luvcraft-shadow-half-extent+ :world-distance single-float)
             (luv::+luvcraft-shadow-depth-radius+ :world-distance single-float)
             (luv::+luvcraft-shadow-base-bias+ :shadow-depth single-float)
             (luv::+luvcraft-shadow-slope-bias+ :shadow-depth single-float)
             (luv::+luvcraft-shadow-minimum-filter-radius+
              :shadow-filter-radius single-float)
             (luv::+luvcraft-shadow-maximum-filter-radius+
              :shadow-filter-radius single-float)))
    (destructuring-bind (name quantity representation) claim
      (let ((declaration
              (luv.arithmetic:value-declaration-for name)))
        (ok declaration)
        (ok (eq representation
                (luv.arithmetic:declaration-representation-type
                 declaration)))
        (ok (eq quantity
                (luv.arithmetic:quantity-specification-name
                 (luv.arithmetic:declaration-quantity-specification
                  declaration))))))))

(deftest chunk-window-protocol-selects-representation-at-crossings
  (let* ((space (make-voxel-space
                 :chunk-shape
                 (make-chunk-shape :width 2 :height 2 :depth 2)))
         (domain (make-chunk-domain space (make-chunk-coordinate 0 0 0)))
         (window (make-instance 'recording-chunk-window)))
    ;; A local step remains pure domain arithmetic: the window is not asked.
    (multiple-value-bind (offset local crossing materialization availability)
        (continue-chunk-window-site
         window domain (make-local-coordinate 0 0 0) +voxel-positive-x+)
      (ok (= offset 1))
      (ok (= (local-coordinate-x local) 1))
      (ok (null crossing))
      (ok (null materialization))
      (ok (eq availability :local))
      (ok (null (recording-window-locations window))))
    ;; Crossing selects the aggregate once; a fifth window participates by
    ;; adding a method, with no type switch in the continuation operation.
    (multiple-value-bind (offset local crossing materialization availability)
        (continue-chunk-window-site
         window domain (make-local-coordinate 1 0 0) +voxel-positive-x+)
      (ok (= offset 37))
      (ok (= (local-coordinate-x local) 0))
      (ok (eq crossing +voxel-positive-x+))
      (ok (eq materialization window))
      (ok (eq availability :available))
      (ok (equal (recording-window-locations window) '((2 0 0)))))))

(deftest current-meshing-windows-share-location-availability
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (neighborhood (luv::make-block-mesh-neighborhood world chunk))
         (snapshot
           (make-block-mesh-snapshot
            world chunk (chunk-mesh-dependency-stamp world chunk))))
    (dolist (window (list world neighborhood snapshot))
      (multiple-value-bind (materialization offset availability)
          (locate-chunk-window-site window 0 0 0)
        (ok materialization)
        ;; Offsets belong to each representation: the live/neighborhood
        ;; chunks use local dense order, while the snapshot includes a halo.
        (ok (typep offset '(integer 0)))
        (ok (eq availability :available)))
      (multiple-value-bind (materialization offset availability)
          (locate-chunk-window-site window 20 0 0)
        (ok (null materialization))
        (ok (null offset))
        (ok (eq availability :unavailable))))))

(deftest voxel-light-fields-retain-distinct-quantity-definitions
  (let* ((sky (luv.world.fields:field-definition-for :sky-light))
         (block (luv.world.fields:field-definition-for :block-light))
         (world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0)))
    (relight-block-world world)
    (let* ((light (block-chunk-light-field chunk))
           (region (luv::capture-light-region world))
           (entry (nth-value 0 (locate-chunk-window-site region 0 0 0)))
           (snapshot
             (make-block-mesh-snapshot
              world chunk (chunk-mesh-dependency-stamp world chunk))))
      (ok (equal '(unsigned-byte 8)
                 (luv.arithmetic:declaration-representation-type sky)))
      (ok (equal (luv.arithmetic:declaration-representation-type sky)
                 (luv.arithmetic:declaration-representation-type block)))
      (ok (eq :sky-propagation-level
              (luv.arithmetic:quantity-specification-name
               (luv.arithmetic:declaration-quantity-specification sky))))
      (ok (eq :block-propagation-level
              (luv.arithmetic:quantity-specification-name
               (luv.arithmetic:declaration-quantity-specification block))))
      (ok (signals (luv.arithmetic:ensure-declarations-compatible sky block)
                   'luv.arithmetic:declaration-compatibility-error))
      (dolist (claim `((,light :sky-light)
                       (,light :block-light)
                       (,entry :block-content)
                       (,entry :sky-light)
                       (,entry :block-light)
                       (,snapshot :block-content)
                       (,snapshot :sky-light)
                       (,snapshot :block-light)))
        (destructuring-bind (materialization name) claim
          (ok (luv.world.fields:materialized-field-current-p
               materialization name)))))))

(defun shader-input-product-layout (specification)
  "Flatten location-ordered shader inputs into one test-side product layout."
  (let ((offset 0) (projections nil))
    (dolist (input
             (sort (copy-list
                    (luv.spir-v:shader-specification-inputs specification))
                   #'< :key #'luv.spir-v:shader-interface-location))
      (let* ((width
               (luv.spir-v:shader-type-component-count
                (luv.arithmetic:declaration-representation-type input)))
             (whole
               (luv.arithmetic:declaration-quantity-specification input))
             (layout (luv.arithmetic:declaration-quantity-layout input)))
        (when whole
          (push (luv.arithmetic:make-quantity-projection
                 (loop for position below width collect (+ offset position))
                 whole)
                projections))
        (when layout
          (dolist (projection
                   (luv.arithmetic:quantity-layout-projections layout))
            (push
             (luv.arithmetic:make-quantity-projection
              (mapcar (lambda (position) (+ offset position))
                      (luv.arithmetic:quantity-projection-positions projection))
              (luv.arithmetic:quantity-projection-specification projection))
             projections)))
        (incf offset width)))
    (luv.arithmetic:make-quantity-layout offset (nreverse projections))))

(deftest block-meshes-carry-a-repeated-product-matching-the-shader-contract
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (mesh (mesh-block-chunk (make-instance 'exposed-face-mesher)
                                 world chunk))
         (declaration (block-mesh-vertex-declaration mesh))
         (layout (luv.arithmetic:declaration-quantity-layout declaration))
         (element
           (luv.arithmetic:repeated-quantity-layout-element-layout layout))
         (shader-layout
           (shader-input-product-layout
            (luv.spir-v:shader-specification-for :block-surface :vertex))))
    (ok (eq declaration
            (luv.arithmetic:value-declaration-for :block-mesh-vertices)))
    (ok (typep (block-mesh-vertices mesh)
               (luv.arithmetic:declaration-representation-type declaration)))
    (ok (= 12 (luv.arithmetic:repeated-quantity-layout-stride layout)))
    (ok (luv.arithmetic:quantity-layout= element shader-layout))
    (ok (= (length (block-mesh-vertices mesh))
           (* 12 (block-mesh-vertex-count mesh))))
    (ok (signals
         (make-instance 'block-mesh
                        :vertices (make-array 11 :element-type 'single-float)
                        :vertex-count 1 :face-count 0)
         'error))))

(deftest light-removal-queues-own-the-meaning-of-unwrapped-levels
  (let* ((coordinate (make-world-coordinate 1 2 3))
         (sky
           (luv::make-light-removal-queue
            :sky-light #'luv::light-region-entry-sky :skylight-p t))
         (block
           (luv::make-light-removal-queue
            :block-light #'luv::light-region-entry-block))
         (coordinate-declaration
           (luv.arithmetic.records:record-slot-declaration
            'luv::light-removal 'luv::coordinate)))
    (luv::enqueue-light-removal sky coordinate 12)
    (ok (eq :world-position
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic:declaration-quantity-specification
              coordinate-declaration))))
    (ok (luv.world.fields:materialized-field-current-p sky :sky-light))
    (ok (luv.world.fields:materialized-field-current-p block :block-light))
    (ok (null
         (luv.world.fields:materialized-field-definition sky :block-light)))
    (ok (eq :sky-propagation-level
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic:declaration-quantity-specification
              (luv::light-removal-queue-field-definition sky)))))
    (ok (eq :block-propagation-level
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic:declaration-quantity-specification
              (luv::light-removal-queue-field-definition block)))))
    (let ((item (first (luv::light-removal-queue-items sky))))
      (ok (eq coordinate (luv::light-removal-coordinate item)))
      (ok (= 12 (luv::light-removal-level item))))
    (ok (signals (luv::enqueue-light-removal sky coordinate 16) 'error))))

(deftest cpu-trace-zones-are-nested-reusable-and-bounded
  (let ((trace (make-cpu-trace :label "test")))
    (with-cpu-trace (trace)
      (with-cpu-trace-zone (:outer)
        (with-cpu-trace-zone (:inner)
          (values))))
    (let* ((first-zones (cpu-trace-zones trace))
           (outer (first first-zones))
           (inner (second first-zones)))
      (ok (= 2 (length first-zones)))
      (ok (eq :outer (cpu-trace-zone-name outer)))
      (ok (eq :inner (cpu-trace-zone-name inner)))
      (ok (= -1 (cpu-trace-zone-parent-index outer)))
      (ok (= 0 (cpu-trace-zone-parent-index inner)))
      (ok (>= (cpu-trace-zone-seconds outer)
              (cpu-trace-zone-seconds inner)))
      (with-cpu-trace (trace)
        (with-cpu-trace-zone (:again)
          (values)))
      (let ((second-zones (cpu-trace-zones trace)))
        (ok (= 1 (length second-zones)))
        (ok (eq outer (first second-zones)))
        (ok (eq :again (cpu-trace-zone-name (first second-zones)))))
      (let ((text (with-output-to-string (stream)
                    (print-cpu-trace trace stream))))
        (ok (search "inclusive" text))
        (ok (search "again" text))))))

(deftest texture-preparation-is-a-backend-neutral-command
  (let ((encoder (make-instance 'recording-command-encoder)))
    (prepare-texture encoder :shadow-depth :texture-binding)
    (let ((command (first (recording-command-encoder-commands encoder))))
      (ok (typep command 'gpu-prepare-texture-command))
      (ok (eq :shadow-depth
              (luv::gpu-prepare-texture-command-texture command)))
      (ok (eq :texture-binding
              (luv::gpu-prepare-texture-command-usage command))))))

(deftest frame-performance-summary-is-comparison-friendly
  (let ((samples (make-array 4)))
    (dotimes (index 4)
      (let ((sample (luv::make-luvcraft-frame-sample)))
        (setf (luv::luvcraft-frame-sample-frame-seconds sample)
              (/ (1+ index) 1000d0)
              (aref samples index) sample)))
    (let ((benchmark
            (luv::make-luvcraft-frame-benchmark :samples samples)))
      (multiple-value-bind (median p95 mean maximum)
          (luv::luvcraft-frame-metric-summary
           benchmark #'luv::luvcraft-frame-sample-frame-seconds)
        (ok (= 2.5d0 median))
        (ok (= 4d0 p95))
        (ok (= 2.5d0 mean))
        (ok (= 4d0 maximum))))))

(defclass gated-production-request (luv::production-request)
  ((gate :initarg :gate :reader gated-production-request-gate)
   (value :initarg :value :reader gated-production-request-value)))

(defmethod luv::perform-production-request ((request gated-production-request))
  (sb-thread:wait-on-semaphore (gated-production-request-gate request))
  (gated-production-request-value request))

(defclass title-canvas ()
  ((title :initarg :title :accessor canvas-title)))

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
                           (world-block-at first x y z)
                         (multiple-value-bind (second-block second-status)
                             (world-block-at second x y z)
                           (and (eq first-status :resident)
                                (eq second-status :resident)
                                (eq first-block second-block))))))))
    (multiple-value-bind (block status) (world-block-at first 80 0 0)
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
    (ok (world-block-at world 1 1 1))
    (edit-block-at nil world 1 1 1)
    (multiple-value-bind (block present-p)
        (block-edit-at (little-world-source-edits source) 1 1 1)
      (ok present-p)
      (ok (null block)))
    (ok (= (block-edit-overlay-count (little-world-source-edits source)) 1))
    (rematerialize-little-world-chunk source world 0 0)
    (multiple-value-bind (block status) (world-block-at world 1 1 1)
      (ok (eq status :resident))
      (ok (null block)))
    ;; Explicit placement into generated air is an overlay value too.
    (edit-block-at luv::*stone-block* world 2 14 2)
    (rematerialize-little-world-chunk source world 0 0)
    (ok (eq (world-block-at world 2 14 2) luv::*stone-block*))
    (ok (= (block-edit-overlay-count (little-world-source-edits source)) 2))))

(deftest little-world-save-descriptions-round-trip-semantic-state
  (let* ((world (make-empty-little-block-world
                 :chunk-width 12 :chunk-height 20 :chunk-depth 10 :seed 913))
         (source (block-world-source world))
         (camera (make-instance 'fly-camera :yaw 1.25 :pitch -0.35))
         (player (make-instance 'block-world-player
                                :position
                                (make-vec3 -20.5d0 7.25d0 44.0d0))))
    (record-block-edit (little-world-source-edits source)
                       luv::*crystal-block* -19 8 44)
    (record-block-edit (little-world-source-edits source) nil 3 4 -5)
    (let ((description
            (make-luvcraft-save-description
             world :camera camera :player player
             :selected-block luv::*crystal-block*)))
      ;; Stable coordinate order makes saves readable and diffs meaningful.
      (ok (equal
           (mapcar (lambda (edit) (getf edit :at))
                   (getf (rest (getf (rest (getf (rest description) :world))
                                    :source))
                         :edits))
           '((-19 8 44) (3 4 -5))))
      (multiple-value-bind (restored resume)
          (restore-luvcraft-save-description description)
        (let* ((restored-space (block-world-space restored))
               (shape (voxel-space-chunk-shape restored-space))
               (restored-source (block-world-source restored)))
          (ok (= (chunk-shape-width shape) 12))
          (ok (= (chunk-shape-height shape) 20))
          (ok (= (chunk-shape-depth shape) 10))
          (ok (= (little-world-source-seed restored-source) 913))
          (ok (= (block-edit-overlay-count
                  (little-world-source-edits restored-source))
                 2))
          (ok (eq (block-edit-at (little-world-source-edits restored-source)
                                 -19 8 44)
                  luv::*crystal-block*))
          (multiple-value-bind (block present-p)
              (block-edit-at (little-world-source-edits restored-source)
                             3 4 -5)
            (ok present-p)
            (ok (null block)))
          (center-little-world-residency restored-source restored -2 4
                                         :radius 0)
          (multiple-value-bind (block status)
              (world-block-at restored -19 8 44)
            (ok (eq status :resident))
            (ok (eq block luv::*crystal-block*)))
          (center-little-world-residency restored-source restored 0 -1
                                         :radius 0)
          (multiple-value-bind (block status)
              (world-block-at restored 3 4 -5)
            (ok (eq status :resident))
            (ok (null block))))
        (multiple-value-bind (restored-camera restored-player selected-block)
            (restore-luvcraft-resume-save-description resume)
          (ok (= (camera-yaw restored-camera) 1.25))
          (ok (= (camera-pitch restored-camera) -0.35))
          (ok (= (player-x restored-player) -20.5d0))
          (ok (= (player-y restored-player) 7.25d0))
          (ok (= (player-z restored-player) 44.0d0))
          (ok (eq selected-block luv::*crystal-block*)))))))

(deftest camera-uniform-coerces-vec3-at-the-gpu-boundary
  (let* ((uniform
          (camera-uniform-data
           (make-instance 'fly-camera
                          :position (make-vec3 8d0 11d0 -6d0)
                          :yaw 1.25d0
                          :pitch -0.35d0)
           1280 720))
         (declaration
           (luv.arithmetic:value-declaration-for :camera-uniform-data)))
    (ok (typep uniform '(simple-array single-float (20))))
    (ok (typep uniform
               (luv.arithmetic:declaration-representation-type declaration)))
    (ok (= 20
           (luv.arithmetic:quantity-layout-extent
            (luv.arithmetic:declaration-quantity-layout declaration))))
    (ok (equalp (subseq uniform 0 4) #(8.0 11.0 -6.0 0.0)))))

(deftest frame-uniform-product-matches-the-live-shader-contract
  (let* ((session
           (make-instance 'luvcraft-session
                          :camera (make-instance 'fly-camera)))
         (data (luv::frame-uniform-data session 1280 720))
         (declaration
           (luv.arithmetic:value-declaration-for :frame-uniform-data))
         (host-layout
           (luv.arithmetic:declaration-quantity-layout declaration))
         (block (luv.spir-v:block-world-camera-uniform-block))
         (shader-layout (luv::frame-shader-uniform-product-layout block)))
    (ok (eq declaration
            (luv.arithmetic:value-declaration-for :frame-uniform-data)))
    (ok (typep data
               (luv.arithmetic:declaration-representation-type declaration)))
    (ok (= 72 (luv.arithmetic:quantity-layout-extent host-layout)))
    (ok (luv.arithmetic:quantity-layout= host-layout shader-layout))
    (ok (= 288 (luv::block-world-camera-uniform-size session)))
    ;; Four dense matrix rows are representation for the declared
    ;; :WORLD-TO-SHADOW map, not sixteen falsely homogeneous quantities.
    (loop for position from 56 below 72
          do (ok (null (luv.arithmetic:project-quantity-layout
                        host-layout (list position)))))))

(deftest world-save-validation-rejects-unknown-meaning
  (ok (signals
       (restore-luvcraft-save-description
        '(:luvcraft-world :format-version 99
          :world (:block-world) :resume nil))))
  (ok (signals
       (restore-block-save-description :block '(:name :missing-material))))
  (ok (signals
       (restore-world-source-save-description
        :little-world '(:source-version 99 :seed 1 :edits ())))))

(deftest asynchronous-world-checkpoints-flush-the-latest-description
  (uiop:with-temporary-file
      (:pathname pathname :prefix "luvcraft-checkpoint-" :suffix ".sexp")
    (let* ((first-world (make-empty-little-block-world :seed 101))
           (latest-world (make-empty-little-block-world :seed 202))
           (writer (make-world-checkpoint-writer pathname)))
      (request-world-checkpoint
       writer (make-luvcraft-save-description first-world))
      (request-world-checkpoint
       writer (make-luvcraft-save-description latest-world))
      (stop-world-checkpoint-writer writer)
      (multiple-value-bind (restored resume) (read-luvcraft-save pathname)
        (ok (null resume))
        (ok (= (little-world-source-seed (block-world-source restored))
               202))))))

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
    (multiple-value-bind (block status) (world-block-at world 1 1 1)
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
    (ok (equal (array-dimensions atlas) '(16 160)))
    (ok (subtypep (array-element-type atlas) '(unsigned-byte 32)))
    (ok (= (ldb (byte 8 24) (aref atlas 8 8)) 255))
    (ok (/= (aref atlas 8 8) (aref atlas 8 (+ 8 (* 3 16)))))
    (ok (/= (aref atlas 8 8) (aref atlas 8 (+ 8 (* 9 16))))))
  (flet ((face (name)
           (find name luv::*block-faces* :key #'block-face-name)))
    (ok (= (block-face-tile luv::*grass-block* (face :top)) 0))
    (ok (= (block-face-tile luv::*grass-block* (face :front)) 1))
    (ok (= (block-face-tile luv::*grass-block* (face :bottom)) 2))
    (ok (= (block-face-tile luv::*wood-block* (face :top)) 5))
    (ok (= (block-face-tile luv::*sand-block* (face :top)) 7))
    (ok (= (block-face-tile luv::*snow-block* (face :top)) 8))
    (ok (= (block-face-tile *crystal-block* (face :top)) 9))
    (ok (= (block-light-emission *crystal-block*) 12))
    (ok (= (block-surface-emission *crystal-block*) 1.2))
    (ok (= (length (placeable-block-kinds)) 8)))
  (let ((world (make-block-world :chunk-width 2
                                 :chunk-height 2
                                 :chunk-depth 2)))
    (ensure-world-chunk world 0 0 0)
    (setf (world-block-at world 0 0 0) luv::*stone-block*)
    (let ((mesh (mesh-block-world (make-instance 'exposed-face-mesher) world)))
      (ok (= (length (block-mesh-vertices mesh))
             (* 12 (block-mesh-vertex-count mesh)))))))

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
         (canvas (make-instance 'title-canvas :title "luvcraft test"))
         (session (make-instance 'luvcraft-session
                                 :canvas canvas
                                 :title-base "luvcraft test"
                                 :selected-block luv::*stone-block*)))
    (ok (= (length vertices)
           (* luv::+block-world-crosshair-vertex-count+ 6)))
    (ok (eq (select-luvcraft-block session 1) luv::*grass-block*))
    (ok (eq (luvcraft-session-selected-block session) luv::*grass-block*))
    (ok (eq (select-luvcraft-block session 7) luv::*snow-block*))
    (ok (eq (select-luvcraft-block session 8) *crystal-block*))
    (ok (search "1–8 select" (canvas-title canvas)))
    (ok (search "crystal" (canvas-title canvas)))
    (ok (null (select-luvcraft-block session 9)))
    (handle-canvas-event
     session canvas
     (make-instance 'canvas-key-press-event
                    :key-name :8 :character #\8))
    (ok (eq (luvcraft-session-selected-block session) *crystal-block*))))

(deftest gazetteer-names-semantic-gameplay-views
  (let* ((views (luvcraft-gazetteer-views))
         (names (mapcar #'luvcraft-gazetteer-view-name views)))
    (ok (equal names (remove-duplicates names :test #'eq)))
    (dolist (name '(:little-world-noon :little-world-dusk :shadow-forest
                    :glow-floor :crystal-seam :shadow-yard))
      (ok (find name names)))
    (let* ((view (find-luvcraft-gazetteer-view "crystal-seam"))
           (world
             (funcall (luv::luvcraft-gazetteer-view-world-factory view))))
      (ok (eq (world-block-at world 16 1 8) *crystal-block*))
      (ok (= (nth-value 1 (world-light-at world 16 1 8))
             (block-light-emission *crystal-block*)))
      (ok (= (nth-value 1 (world-light-at world 15 1 8))
             (1- (block-light-emission *crystal-block*)))))))

(deftest shadow-yard-gazetteer-has-raised-casters-over-receiver
  (let* ((view (find-luvcraft-gazetteer-view "shadow-yard"))
         (world (funcall (luv::luvcraft-gazetteer-view-world-factory view))))
    (ok (eq (world-block-at world 7 0 7) luv::*snow-block*))
    (ok (eq (world-block-at world 9 1 10) luv::*stone-block*))
    (ok (eq (world-block-at world 10 8 10) luv::*stone-block*))
    (ok (null (world-block-at world 9 9 10)))
    (ok (null (world-block-at world 8 1 4)))
    (ok (= (nth-value 0 (world-light-at world 7 1 7)) 15))))

(deftest shadow-projection-ignores-subtexel-camera-translation
  (let* ((clock (make-instance 'sky-clock :pinned-day-fraction 0.42))
         (sky (sky-frame-parameters clock (make-default-sky-profile)))
         (first-camera
           (make-instance 'fly-camera :position (make-vec3 0d0 0d0 0d0)))
         (nearby-camera
           (make-instance 'fly-camera
                          :position (make-vec3 0.01d0 0d0 0.01d0)))
         (farther-camera
           (make-instance 'fly-camera
                          :position (make-vec3 0.25d0 0d0 0.25d0)))
         (first-rows (luv::shadow-frame-rows first-camera sky))
         (nearby-rows (luv::shadow-frame-rows nearby-camera sky))
         (farther-rows (luv::shadow-frame-rows farther-camera sky)))
    ;; The first two rows locate the orthographic footprint.  Translation
    ;; smaller than one 0.0625-world-unit shadow texel cannot move it.
    (ok (equal (subseq first-rows 0 8) (subseq nearby-rows 0 8)))
    (ok (not (equal (subseq first-rows 0 8)
                    (subseq farther-rows 0 8))))))

(deftest shadow-projection-is-continuous-through-old-up-axis-threshold
  (let* ((camera (make-instance 'fly-camera))
         (profile (make-default-sky-profile))
         (before
           (luv::shadow-frame-rows
            camera
            (sky-frame-parameters
             (make-instance 'sky-clock :pinned-day-fraction 0.451)
             profile)))
         (after
           (luv::shadow-frame-rows
            camera
            (sky-frame-parameters
             (make-instance 'sky-clock :pinned-day-fraction 0.453)
             profile)))
         (right-dot
           (loop for index below 3
                 sum (* (nth index before) (nth index after)))))
    ;; Row X has length 1/extent.  Undo that scale before comparing the
    ;; neighboring orientations around the former abs(forward.y)=0.92 switch.
    (ok (> (* right-dot
              luv::+luvcraft-shadow-half-extent+
              luv::+luvcraft-shadow-half-extent+)
           0.99))))

(deftest temporal-frame-derivatives-expose-change-and-flicker
  (let ((first #(10 20 30 255 40 50 60 255))
        (second #(13 17 36 255 40 50 60 255))
        (third #(16 14 42 255 43 53 63 255)))
    (multiple-value-bind (difference mean maximum changed)
        (luv::temporal-derivative-rgba second first 10.0)
      (ok (equalp difference #(40 40 40 255 0 0 0 255)))
      (ok (< (abs (- mean (/ 2.0 255.0))) 1e-6))
      (ok (< (abs (- maximum (/ 4.0 255.0))) 1e-6))
      (ok (= changed 0.5)))
    (multiple-value-bind (difference mean maximum changed)
        (luv::temporal-derivative-rgba third second 10.0 first)
      (ok (equalp difference #(0 0 0 255 30 30 30 255)))
      (ok (< (abs (- mean (/ 1.5 255.0))) 1e-6))
      (ok (< (abs (- maximum (/ 3.0 255.0))) 1e-6))
      (ok (= changed 0.5)))))

(deftest scalar-player-walks-collides-and-jumps
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 1.5 4.62 1.5)
                                :yaw 0d0 :pitch 0d0))
         (player (make-instance 'block-world-player
                                :position (make-vec3 1.5d0 3d0 1.5d0)))
         (keys (make-hash-table :test #'eq)))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 4 do
      (loop for z below 4 do
        (setf (world-block-at world x 0 z) luv::*stone-block*)))
    ;; Gravity settles the body exactly on the block tops.
    (dotimes (step 240)
      (declare (ignorable step))
      (step-block-world-player player world camera keys (/ 1d0 120d0)))
    (ok (< (abs (- (player-y player) 1d0)) 1d-5))
    (ok (player-grounded-p player))
    (ok (< (abs (- (camera-y camera) 2.62d0)) 1d-5))
    ;; A held right input accelerates into, but not through, a two-block wall.
    (setf (world-block-at world 3 1 1) luv::*stone-block*
          (world-block-at world 3 2 1) luv::*stone-block*
          (gethash :d keys) t)
    (dotimes (step 120)
      (declare (ignorable step))
      (step-block-world-player player world camera keys (/ 1d0 120d0)))
    (ok (<= (player-x player) 2.700001d0))
    (ok (= (player-velocity-x player) 0d0))
    (ok (< (abs (- (player-y player) 1d0)) 1d-5))
    (ok (player-grounded-p player))
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

(deftest scalar-player-autojumps-a-clear-one-block-ledge
  (let* ((world (make-block-world :chunk-width 8
                                  :chunk-height 4
                                  :chunk-depth 4))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 1.5 2.62 1.5)
                                :yaw 0d0 :pitch 0d0))
         (player (make-instance 'block-world-player
                                :position (make-vec3 1.5d0 1d0 1.5d0)
                                :grounded-p t))
         (keys (make-hash-table :test #'eq))
         (highest-y (player-y player)))
    (ensure-world-chunk world 0 0 0)
    (loop for x below 8 do
      (loop for z below 4 do
        (setf (world-block-at world x 0 z) luv::*stone-block*)))
    (setf (world-block-at world 3 1 1) luv::*stone-block*
          (gethash :d keys) t)
    (dotimes (step 120)
      (declare (ignorable step))
      (step-block-world-player player world camera keys (/ 1d0 120d0))
      (setf highest-y (max highest-y (player-y player))))
    (ok (> highest-y 2d0))
    (ok (> (player-x player) 3.3d0))))

(deftest meshing-and-editing-cross-a-chunk-boundary
  (let ((world (make-block-world :chunk-width 2
                                 :chunk-height 2
                                 :chunk-depth 2)))
    (ensure-world-chunk world 0 0 0)
    (ensure-world-chunk world 1 0 0)
    (setf (world-block-at world 1 0 0) luv::*stone-block*
          (world-block-at world 2 0 0) luv::*stone-block*)
    (let ((mesher (make-instance 'exposed-face-mesher)))
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 10))
      (let ((revision (block-world-revision world)))
        (setf (world-block-at world 2 0 0) nil)
        (ok (= (block-world-revision world) (1+ revision))))
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 6))
      (setf (world-block-at world 2 0 0) luv::*stone-block*)
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 10)))))

(deftest chunk-mesh-is-exactly-sized-and-preserves-the-public-emitter
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (world-block-at world 0 0 0) luv::*stone-block*)
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
    (setf (world-block-at world 0 0 0) nil)
    (ok (equalp (block-mesh-vertices copied)
                (block-mesh-vertices (mesh-block-snapshot mesher snapshot))))))

(deftest production-system-coalesces-desired-work-and-stops-cooperatively
  (let ((system (luv::make-single-worker-production-system
                 :name "luv production test")))
    (unwind-protect
         (let* ((first
                  (make-instance
                   'luv::little-world-load-request
                   :key '(:load (0 0 0)) :priority 4
                   :seed 1 :demand-token 1
                   :width 8 :height 8 :depth 8))
                (latest
                  (make-instance
                   'luv::little-world-load-request
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
         (session (make-instance 'luvcraft-session
                              :world world
                              :player (make-instance 'block-world-player
                                                     :position
                                                     (make-vec3 0d0 0d0 0d0))
                              :production-system system)))
    (unwind-protect
         (progn
           (luv::maintain-luvcraft-residency session)
           (ok (gethash (luv::block-chunk-key first)
                        (luvcraft-session-desired-chunks session)))
           (ok (gethash (luv::block-chunk-key second)
                        (luvcraft-session-desired-chunks session)))
           (remove-world-chunk world -1 0 2)
           (luv::maintain-luvcraft-residency session)
           (ok (not (gethash (luv::block-chunk-key first)
                             (luvcraft-session-desired-chunks session))))
           (ok (gethash (luv::block-chunk-key second)
                        (luvcraft-session-desired-chunks session))))
      (luv::stop-production-system system))))

(deftest chunk-mesh-products-have-narrow-neighbor-dependencies
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (left (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (world-block-at world 3 1 1) luv::*stone-block*
          (world-block-at world 4 1 1) luv::*stone-block*)
    (ok (= (block-mesh-face-count (mesh-block-chunk mesher world left)) 5))
    (ok (= (block-mesh-face-count (mesh-block-chunk mesher world right)) 5))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      ;; This changes RIGHT, but not the boundary LEFT's mesh observes.
      (setf (world-block-at world 5 2 2) luv::*stone-block*)
      (ok (equal stamp (chunk-mesh-dependency-stamp world left)))
      ;; This touches RIGHT's -X boundary and must invalidate LEFT.
      (setf (world-block-at world 4 2 2) luv::*stone-block*)
      (ok (not (equal stamp (chunk-mesh-dependency-stamp world left)))))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      (remove-world-chunk world 0 0 0)
      (let ((replacement (ensure-world-chunk world 0 0 0)))
        (ok (not (equal stamp
                        (chunk-mesh-dependency-stamp world replacement))))))))

(defun test-luvcraft-chunk-product (chunk stamp)
  (make-instance
   'luv::luvcraft-chunk-product
   :coordinate (chunk-domain-coordinate (block-chunk-domain chunk))
   :dependency-stamp stamp
   :mesh nil :vertex-buffer nil))

(deftest boundary-mesh-replacements-publish-as-one-visible-cohort
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (left (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 0 0))
         (session (make-instance 'luv::luvcraft-session :world world))
         (left-key '(0 0 0))
         (right-key '(1 0 0)))
    (setf (gethash left-key (luv::luvcraft-session-desired-chunks session)) t
          (gethash right-key (luv::luvcraft-session-desired-chunks session)) t
          (world-block-at world 1 0 0) luv::*stone-block*
          (world-block-at world 2 0 0) luv::*stone-block*)
    (let ((old-left
            (test-luvcraft-chunk-product
             left (chunk-mesh-dependency-stamp world left)))
          (old-right
            (test-luvcraft-chunk-product
             right (chunk-mesh-dependency-stamp world right))))
      (setf (gethash left-key (luv::luvcraft-session-chunk-products session))
            old-left
            (gethash right-key (luv::luvcraft-session-chunk-products session))
            old-right)
      ;; Removing RIGHT's boundary block also exposes a face owned by LEFT.
      ;; One completed replacement must leave the whole old pair visible.
      (setf (world-block-at world 2 0 0) nil)
      (let ((new-right
              (test-luvcraft-chunk-product
               right (chunk-mesh-dependency-stamp world right))))
        (setf (gethash right-key
                       (luv::luvcraft-session-staged-chunk-products session))
              new-right)
        (ok (zerop (luv::publish-ready-luvcraft-meshes session)))
        (ok (eq old-left
                (gethash left-key
                         (luv::luvcraft-session-chunk-products session))))
        (ok (eq old-right
                (gethash right-key
                         (luv::luvcraft-session-chunk-products session))))
        (let ((new-left
                (test-luvcraft-chunk-product
                 left (chunk-mesh-dependency-stamp world left))))
          (setf (gethash left-key
                         (luv::luvcraft-session-staged-chunk-products session))
                new-left)
          (ok (= 2 (luv::publish-ready-luvcraft-meshes session)))
          (ok (eq new-left
                  (gethash left-key
                           (luv::luvcraft-session-chunk-products session))))
          (ok (eq new-right
                  (gethash right-key
                           (luv::luvcraft-session-chunk-products session)))))))))

(deftest camera-edits-the-resident-lattice
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (camera (make-instance 'fly-camera
                                :position (make-vec3 0.5 1.5 1.5)
                                :yaw (/ pi 2) :pitch 0.0))
         (session (make-instance 'luvcraft-session
                              :world world
                              :camera camera
                              :selected-block luv::*dirt-block*)))
    (ensure-world-chunk world 0 0 0)
    ;; The second stone means placing after removing the first still has a
    ;; solid target beyond the empty adjacent site.
    (setf (world-block-at world 2 1 1) luv::*stone-block*
          (world-block-at world 3 1 1) luv::*stone-block*)
    (multiple-value-bind (coordinate status)
        (edit-luvcraft-block session :remove)
      (ok (eq status :edited))
      (ok (= (world-coordinate-x coordinate) 2))
      (ok (null (world-block-at world 2 1 1))))
    (let ((occupied-session
            (make-instance 'luvcraft-session
                           :world world :camera camera
                           :player (make-instance 'block-world-player
                                                  :position
                                                  (make-vec3 2.5d0 1d0 1.5d0))
                           :selected-block luv::*dirt-block*)))
      (multiple-value-bind (coordinate status)
          (edit-luvcraft-block occupied-session :place)
        (ok (null coordinate))
        (ok (eq status :blocked))
        (ok (null (world-block-at world 2 1 1)))))
    (multiple-value-bind (coordinate status)
        (edit-luvcraft-block session :place)
      (ok (eq status :edited))
      (ok (= (world-coordinate-x coordinate) 2))
      (ok (eq (world-block-at world 2 1 1) luv::*dirt-block*)))))
