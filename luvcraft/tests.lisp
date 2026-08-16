(in-package #:luvcraft.tests)

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

(deftest block-smash-particles-form-a-bounded-textured-burst
  (let ((system (make-instance 'block-particle-system))
        (coordinate (make-world-coordinate 3 5 -2)))
    (smash-block-particles system luvcraft::*dirt-block* coordinate)
    (ok (= luvcraft::+block-particle-burst-size+
           (block-particle-count system)))
    (let ((vertices (luvcraft::block-particle-vertices system)))
      (ok (= (length vertices)
             (* (block-particle-count system)
                luvcraft::+block-particle-vertices-per-particle+
                luvcraft::+block-mesh-floats-per-vertex+)))
      (ok (typep vertices '(array single-float (*)))))
    (dotimes (index 20)
      (declare (ignorable index))
      (smash-block-particles system luvcraft::*stone-block* coordinate))
    (ok (= luvcraft::+maximum-block-particles+
           (block-particle-count system)))))

(deftest block-smash-particles-rise-fall-and-expire
  (let* ((system (make-instance 'block-particle-system))
         (coordinate (make-world-coordinate 0 0 0)))
    (smash-block-particles system luvcraft::*grass-block* coordinate)
    (let* ((particle (aref (block-particle-system-particles system) 0))
           (initial-y (luvcraft::block-particle-y particle))
           (initial-velocity-y
             (luvcraft::block-particle-velocity-y particle)))
      (luvcraft::advance-block-particles system 0.05)
      (ok (> (luvcraft::block-particle-y particle) initial-y))
      (ok (< (luvcraft::block-particle-velocity-y particle)
             initial-velocity-y)))
    (luvcraft::advance-block-particles system 1.0)
    (ok (zerop (block-particle-count system)))))

(deftest vec3-is-imported-from-its-arithmetic-representation-package
  (dolist (package-name '("LUVCRAFT.WORLD" "LUVCRAFT"))
    (multiple-value-bind (symbol status)
        (find-symbol "VEC3" package-name)
      (ok (eq symbol 'luv.arithmetic.lisp.vec3:vec3))
      (ok (eq status :internal))))
  (ok (eq (symbol-package 'vec3)
          (find-package "LUV.ARITHMETIC.LISP.VEC3"))))

(deftest player-storage-publishes-quantities-without-wrapping-values
  (let* ((position (make-vec3 1d0 2d0 3d0))
         (velocity (make-vec3 4d0 5d0 6d0))
         (player (make-instance 'block-world-player
                                :position position :velocity velocity))
         (position-declaration
           (luv.arithmetic.records:record-slot-declaration
            'block-world-player 'luvcraft::position))
         (velocity-declaration
           (luv.arithmetic.records:record-slot-declaration
            'block-world-player 'luvcraft::velocity)))
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
          'block-world-player 'luvcraft::grounded-p)))
    (let ((predicted (luvcraft::predict-player-position player 0.5d0)))
      (ok (equalp (make-vec3 3d0 4.5d0 6d0) predicted))
      (ok (eq position (player-position player)))
      (ok (eq velocity (player-velocity player))))
    (ok (compiled-function-p luvcraft::*predict-player-position-function*))
    (ok (signals
         (luv.arithmetic.lisp:bind-lisp-arithmetic-realization
          luvcraft::*predict-player-position-realization*
          (list velocity-declaration velocity-declaration
                luvcraft::*player-frame-duration-declaration*)
          :actual-result-declaration position-declaration)
         'luv.arithmetic:declaration-compatibility-error))))

(deftest sky-frame-structure-publishes-quantities-without-changing-layout
  (let* ((sky (sky-frame-parameters
               (make-instance 'sky-clock)
               (make-default-sky-profile)))
         (direction (luvcraft::sky-frame-parameters-sun-direction sky))
         (direction-declaration
           (luv.arithmetic.records:record-slot-declaration
            'luvcraft::sky-frame-parameters 'luvcraft::sun-direction))
         (fog-declaration
           (luv.arithmetic.records:record-slot-declaration
            'luvcraft::sky-frame-parameters 'luvcraft::fog-far)))
    (ok (typep sky 'luvcraft::sky-frame-parameters))
    (ok (typep direction 'vec3))
    (ok (eq :world-direction
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic:declaration-quantity-specification
              direction-declaration))))
    (ok (eq 'single-float
            (luv.arithmetic:declaration-representation-type
             fog-declaration)))
    (ok (typep (luvcraft::sky-frame-parameters-fog-far sky) 'single-float))))

(deftest production-fog-law-is-shared-by-shader-and-cpu
  (let* ((sky
           (luvcraft::%make-sky-frame-parameters
            :fog-near 20.0 :fog-far 100.0))
         (definition
           (luv.arithmetic.language:arithmetic-function-definition-for
            'luvcraft.arithmetic:fog-amount-at-view-distance))
         (vertex (luvcraft.shaders:block-world-vertex-specification))
         (calls
           (remove-if-not
            (lambda (expression)
              (and (typep expression
                          'luv.arithmetic.language:arithmetic-function-call)
                   (eq definition
                       (luv.arithmetic.language:arithmetic-function-call-definition
                        expression))))
            (luv.spir-v:shader-specification-expressions vertex))))
    (ok definition)
    (ok (= 1 (length calls)))
    (ok (= 0.0 (luvcraft::sky-fog-amount-at-distance sky 10.0)))
    (ok (= 0.25 (luvcraft::sky-fog-amount-at-distance sky 60.0)))
    (ok (= 1.0 (luvcraft::sky-fog-amount-at-distance sky 120.0)))
    (ok (compiled-function-p luvcraft::*sky-fog-amount-function*))
    (ok (signals
         (luv.arithmetic.lisp:bind-lisp-arithmetic-realization
          luvcraft::*sky-fog-amount-realization*
          (list
           luvcraft::*sky-fog-view-distance-declaration*
           luvcraft::*sky-fog-amount-declaration*
           (luv.arithmetic.records:record-slot-declaration
            'luvcraft::sky-frame-parameters 'luvcraft::fog-far))
          :actual-result-declaration luvcraft::*sky-fog-amount-declaration*)
         'luv.arithmetic:declaration-compatibility-error))))

(deftest semantic-owner-audit-exposes-camera-sky-material-and-timing-fields
  (dolist (claim
           '((fly-camera luvcraft::yaw :camera-yaw)
             (fly-camera luvcraft::sensitivity :look-sensitivity)
             (sky-clock luvcraft::rate :sky-cycle-rate)
             (sky-clock luvcraft::pinned-day-fraction :day-fraction)
             (luvcraft::sky-keyframe luvcraft::sun-color :linear-rgb)
             (luvcraft::sky-keyframe luvcraft::fog-far :view-distance)
             (block-kind luvcraft::light-opacity :block-light-attenuation-step)
             (block-kind luvcraft::surface-emission :material-emission)
             (luvcraft::luvcraft-frame-sample luvcraft::simulation-seconds
              :simulation-duration)
             (luvcraft::luvcraft-frame-benchmark luvcraft::drain-seconds
              :benchmark-drain-duration)
             (luvcraft::production-result luvcraft::elapsed-seconds
              :production-duration)
             (luvcraft::luvcraft-lighting-state luvcraft::last-latency-seconds
              :lighting-reconciliation-duration)
             (luvcraft-session luvcraft::last-frame-time
              :monotonic-frame-time)
             (luvcraft-session luvcraft::physics-accumulator
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
           '((luvcraft::+player-physics-step+ :frame-duration double-float)
             (luvcraft::+player-collision-epsilon+ :world-distance double-float)
             (luvcraft::+player-step-height+ :world-distance double-float)
             (luvcraft::+player-terminal-fall-speed+ :world-velocity double-float)
             (luvcraft::+luvcraft-camera-near-distance+ :view-distance single-float)
             (luvcraft::+luvcraft-camera-far-distance+ :view-distance single-float)
             (luvcraft::+luvcraft-camera-vertical-field-of-view+
              :camera-field-of-view single-float)
             (luvcraft::+luvcraft-target-reach+ :ray-distance double-float)
             (luvcraft::+luvcraft-maximum-frame-duration+
              :frame-duration double-float)
             (luvcraft::+luvcraft-shadow-half-extent+ :world-distance single-float)
             (luvcraft::+luvcraft-shadow-depth-radius+ :world-distance single-float)
             (luvcraft::+luvcraft-shadow-base-bias+ :shadow-depth single-float)
             (luvcraft::+luvcraft-shadow-slope-bias+ :shadow-depth single-float)
             (luvcraft::+luvcraft-shadow-minimum-filter-radius+
              :shadow-filter-radius single-float)
             (luvcraft::+luvcraft-shadow-maximum-filter-radius+
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

(deftest chunk-window-neighbor-iteration-retains-only-explicit-copies
  (let* ((space (make-voxel-space
                 :chunk-shape
                 (make-chunk-shape :width 3 :height 3 :depth 3)))
         (domain (make-chunk-domain space (make-chunk-coordinate 0 0 0)))
         (window (make-instance 'recording-chunk-window))
         (neighbors nil))
    (do-chunk-window-neighbors
        (offset destination crossing direction materialization availability
         window domain (make-local-coordinate 1 1 1)
         *voxel-face-directions*)
      (ok (eq availability :local))
      (push (list offset (copy-local-coordinate destination)) neighbors))
    (ok (= (length neighbors) 6))
    (dolist (expected (list (make-local-coordinate 0 1 1)
                            (make-local-coordinate 2 1 1)
                            (make-local-coordinate 1 0 1)
                            (make-local-coordinate 1 2 1)
                            (make-local-coordinate 1 1 0)
                            (make-local-coordinate 1 1 2)))
      (ok (find expected neighbors :key #'second :test #'equalp)))
    (ok (null (recording-window-locations window)))
    (setf neighbors nil)
    (do-chunk-window-neighbors
        (offset destination crossing direction materialization availability
         window domain (make-local-coordinate 0 1 1)
         *voxel-face-directions*)
      (when crossing (push crossing neighbors)))
    (ok (equal neighbors (list +voxel-negative-x+)))
    (ok (equal (recording-window-locations window) '((-1 1 1))))))

(deftest current-meshing-windows-share-location-availability
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (neighborhood (luvcraft::make-block-mesh-neighborhood world chunk))
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
  (let* ((sky (luvcraft.world.fields:field-definition-for :sky-light))
         (block (luvcraft.world.fields:field-definition-for :block-light))
         (world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0)))
    (relight-block-world world)
    (let* ((light (block-chunk-light-field chunk))
           (region (luvcraft::capture-light-region world))
           (entry (nth-value 0 (locate-chunk-window-site region 0 0 0)))
           (resident-representation
             (luvcraft.world.fields:materialized-field-representation
              light :sky-light))
           (captured-representation
             (luvcraft.world.fields:materialized-field-representation
              entry :sky-light))
           (block-properties
             (luvcraft::light-region-entry-block-properties entry))
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
      (ok (typep resident-representation 'luvcraft::voxel-light-columns))
      (ok (typep captured-representation 'luvcraft::voxel-light-columns))
      (ok (not (eq resident-representation captured-representation)))
      (ok (eq (block-chunk-domain chunk)
              (luvcraft.world.fields:field-representation-domain
               resident-representation)))
      (ok (eq (luvcraft::light-region-entry-domain entry)
              (luvcraft.world.fields:field-representation-domain
               captured-representation)))
      (ok (= (length (luvcraft::light-region-entry-opacity-lut entry))
             (luv.domains:domain-cardinality
              (luvcraft.world.fields:field-representation-domain
               block-properties))))
      (dolist (lane-and-quantity
                '((luvcraft::propagation-loss
                   :block-light-attenuation-step)
                  (luvcraft::emission-level
                   :block-light-emission-step)))
        (destructuring-bind (lane quantity) lane-and-quantity
          (ok (eq quantity
                  (luv.arithmetic:quantity-specification-name
                   (luv.arithmetic:declaration-quantity-specification
                    (luv.arithmetic.records:columnar-row-lane-declaration
                     (luvcraft::block-light-properties-row-declaration
                      block-properties)
                     lane)))))))
      (dolist (claim `((,light :sky-light)
                       (,light :block-light)
                       (,entry :block-content)
                       (,entry :sky-light)
                       (,entry :block-light)
                       (,snapshot :block-content)
                       (,snapshot :sky-light)
                       (,snapshot :block-light)))
        (destructuring-bind (materialization name) claim
          (ok (luvcraft.world.fields:materialized-field-current-p
               materialization name)))))))

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
           (luvcraft::shader-input-product-layout
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

(deftest screen-geometry-products-match-their-shader-contracts
  (dolist (claim
           `((:sky-vertices
              ,(luvcraft::make-block-world-sky-vertices)
              3
              ,(luvcraft.shaders:block-world-sky-vertex-specification))
             (:crosshair-vertices
              ,(luvcraft::make-block-world-crosshair-vertices 960 640)
              ,luvcraft::+block-world-crosshair-vertex-count+
              ,(luvcraft.shaders:block-world-crosshair-vertex-specification))))
    (destructuring-bind (name vertices count specification) claim
      (let* ((declaration (luv.arithmetic:value-declaration-for name))
             (layout
               (luv.arithmetic:declaration-quantity-layout declaration)))
        (ok (typep vertices
                   (luv.arithmetic:declaration-representation-type
                    declaration)))
        (ok (= (length vertices)
               (* count
                  (luv.arithmetic:repeated-quantity-layout-stride layout))))
        (ok
         (luv.arithmetic:quantity-layout=
          (luv.arithmetic:repeated-quantity-layout-element-layout layout)
          (luvcraft::shader-input-product-layout specification)))))))

(deftest world-text-model-carries-atlas-band-metadata
  (let* ((center (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 10.0))
         (serialized
           (luv.slug::make-slug-serialized-outline
            :horizontal-band-count 7 :vertical-band-count 5))
         (resource
           (luvcraft::make-world-text-glyph-resource
            :serialized serialized))
         (glyph
           (luvcraft::make-world-text-glyph
            :resource resource
            :origin-x 0.0 :origin-y 0.0
            :outline-min-x 0.0 :outline-min-y 0.0
            :outline-max-x 1.0 :outline-max-y 1.0))
         (locations (make-hash-table :test #'eq))
         (atlas (luvcraft::make-world-text-glyph-atlas :locations locations))
         (instances nil))
    (setf (gethash resource locations) '(17 29)
          instances
          (luvcraft::make-world-text-instances
           (list glyph) atlas center
           (luv.arithmetic.lisp.vec3:make-vec3 1.0 0.0 0.0)
           (luv.arithmetic.lisp.vec3:make-vec3 0.0 1.0 0.0)
           0.5 0.0 0.0 1.0 1.0))
    (ok (= 18 (length instances)))
    (ok (= 10.0 (aref instances 2)))
    (ok (< (abs (- 0.535 (aref instances 3))) 1e-6))
    (ok (< (abs (- 0.535 (aref instances 7))) 1e-6))
    (ok (= 7.0 (aref instances 11)))
    (ok (= 5.0 (aref instances 14)))
    (ok (= 17.0 (aref instances 15)))
    (ok (= 29.0 (aref instances 16)))))

(deftest light-removal-queues-own-the-meaning-of-unwrapped-levels
  (let* ((world (make-block-world))
         (chunk (luvcraft::ensure-world-chunk world 0 0 0))
         (region (luvcraft::capture-light-region world))
         (entry (gethash (chunk-domain-coordinate (block-chunk-domain chunk))
                         (luvcraft::light-region-entries region)))
         (domain (block-chunk-domain chunk))
         (local (make-local-coordinate 1 2 3))
         (offset (chunk-domain-offset domain local))
         (sky
           (luvcraft::make-light-removal-queue
            :sky-light #'luvcraft::light-region-entry-sky :skylight-p t))
         (block
           (luvcraft::make-light-removal-queue
            :block-light #'luvcraft::light-region-entry-block)))
    (luvcraft::enqueue-light-removal sky entry offset 12)
    (ok (eq domain (luvcraft::light-region-entry-domain entry)))
    (ok (luvcraft.world.fields:materialized-field-current-p sky :sky-light))
    (ok (luvcraft.world.fields:materialized-field-current-p block :block-light))
    (ok (null
         (luvcraft.world.fields:materialized-field-definition sky :block-light)))
    (ok (eq :sky-propagation-level
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic:declaration-quantity-specification
              (luvcraft::light-removal-queue-field-definition sky)))))
    (ok (eq :block-propagation-level
            (luv.arithmetic:quantity-specification-name
             (luv.arithmetic:declaration-quantity-specification
              (luvcraft::light-removal-queue-field-definition block)))))
    (ok (eq (luvcraft::light-removal-queue-field-definition sky)
            (luv.arithmetic.records:columnar-row-lane-declaration
             (luvcraft::light-worklist-bucket-row-declaration
              (aref
               (luvcraft::light-worklist-buckets
                (luvcraft::light-removal-queue-worklist sky))
               0))
             'luvcraft::level)))
    (ok (eq (luvcraft::light-removal-queue-field-definition block)
            (luv.arithmetic.records:columnar-row-lane-declaration
             (luvcraft::light-worklist-bucket-row-declaration
              (aref
               (luvcraft::light-worklist-buckets
                (luvcraft::light-removal-queue-worklist block))
               0))
             'luvcraft::level)))
    (multiple-value-bind (queued-entry queued-offset level present-p)
        (luvcraft::light-worklist-pop
         (luvcraft::light-removal-queue-worklist sky))
      (ok present-p)
      (ok (eq entry queued-entry))
      (ok (= offset queued-offset))
      (ok (= 12 level)))
    (ok (signals
         (luvcraft::enqueue-light-removal sky entry offset 16) 'error))))

(deftest packed-light-worklists-preserve-order-and-release-entries
  (let* ((world (make-block-world))
         (chunk (luvcraft::ensure-world-chunk world 0 0 0))
         (region (luvcraft::capture-light-region world))
         (entry (gethash (chunk-domain-coordinate (block-chunk-domain chunk))
                         (luvcraft::light-region-entries region)))
         (lifo (luvcraft::make-light-worklist :scheduling :lifo))
         (level (luvcraft::make-light-worklist :scheduling :level)))
    (luvcraft::light-worklist-push lifo entry 1 3)
    (luvcraft::light-worklist-push lifo entry 2 12)
    (let* ((bucket (aref (luvcraft::light-worklist-buckets lifo) 0))
           (entries (luvcraft::light-worklist-bucket-entry-lane bucket)))
      (multiple-value-bind (popped popped-offset popped-level present-p)
          (luvcraft::light-worklist-pop lifo)
        (ok present-p)
        (ok (eq entry popped))
        (ok (= 2 popped-offset))
        (ok (= 12 popped-level)))
      (multiple-value-bind (popped popped-offset popped-level present-p)
          (luvcraft::light-worklist-pop lifo)
        (ok present-p)
        (ok (eq entry popped))
        (ok (= 1 popped-offset))
        (ok (= 3 popped-level)))
      (ok (luvcraft::light-worklist-empty-p lifo))
      (ok (loop for index below (array-total-size entries)
                always (null (row-major-aref entries index))))
      (luvcraft::light-worklist-push lifo entry 4 5)
      (ok (eq entries
              (luvcraft::light-worklist-bucket-entry-lane
               (aref (luvcraft::light-worklist-buckets lifo) 0)))))
    (dolist (item '((1 3) (2 12) (3 12) (4 5)))
      (luvcraft::light-worklist-push level entry (first item) (second item)))
    (dolist (expected '((3 12) (2 12) (4 5) (1 3)))
      (multiple-value-bind (popped popped-offset popped-level present-p)
          (luvcraft::light-worklist-pop level)
        (ok present-p)
        (ok (eq entry popped))
        (ok (= (first expected) popped-offset))
        (ok (= (second expected) popped-level))))
    (ok (luvcraft::light-worklist-empty-p level))
    (loop for bucket across (luvcraft::light-worklist-buckets level)
          for entries = (luvcraft::light-worklist-bucket-entry-lane bucket)
          do (ok (loop for index below (array-total-size entries)
                       always (null (row-major-aref entries index)))))
    (multiple-value-bind (popped offset popped-level present-p)
        (luvcraft::light-worklist-pop level)
      (ok (null popped))
      (ok (null offset))
      (ok (null popped-level))
      (ok (null present-p)))))

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

(deftest tracy-source-locations-are-interned-per-zone
  ;; Tracy tells zones apart by the address of their source location, and
  ;; recompiling a file re-runs the LOAD-TIME-VALUE that asks for one.  Two
  ;; requests describing the same zone therefore have to answer with the same
  ;; pointer, or a recompile in the middle of a capture would split the zone.
  (let ((first (luv:tracy-source-location "test/zone" :file "tests.lisp"))
        (again (luv:tracy-source-location "test/zone" :file "tests.lisp"))
        (other (luv:tracy-source-location "test/other" :file "tests.lisp")))
    (ok (cffi:pointer-eq first again))
    (ok (not (cffi:pointer-eq first other)))
    (ok (string= "canvas/frame" (luv:tracy-zone-name :canvas/frame)))
    (ok (string= "already a name" (luv:tracy-zone-name "already a name")))))

(deftest streaming-trace-quiescence-requires-a-complete-publication-frontier
  (let ((quiet '(:center (0 0) :desired 81 :outstanding 0 :staged 0
                 :products 81 :lighting-dirty-p nil :errors 0)))
    (ok (luvcraft::luvcraft-streaming-trace-state-quiescent-p quiet))
    (ok (luvcraft::luvcraft-streaming-trace-state-quiescent-p quiet '(0 0)))
    (ok (not (luvcraft::luvcraft-streaming-trace-state-quiescent-p
              quiet '(1 0))))
    (dolist (busy '((:outstanding 1) (:staged 1) (:products 80)
                    (:lighting-dirty-p t) (:errors 1)))
      (let ((state (copy-list quiet)))
        (setf (getf state (first busy)) (second busy))
        (ok (not (luvcraft::luvcraft-streaming-trace-state-quiescent-p
                  state)))))))

(deftest tracy-zone-contexts-travel-as-a-single-word
  ;; The binding spells TracyCZoneCtx as a uint64 rather than as the structure
  ;; it is, which keeps zone entry and exit out of libffi but makes an ABI
  ;; claim: that `struct { uint32_t id; int32_t active; }' comes back in the
  ;; register a word would, id low and active high.  Check it on whatever
  ;; machine is running rather than trusting it on one nobody has tried.
  (if (not (luv:tracy-client-available-p))
      (ok t "This machine has no Tracy client to check the zone ABI against.")
      (let ((ours (not luv:*tracy*)))
        (unwind-protect
             (progn
               (luv:start-tracy :application-name "luv tests")
               (let* ((location (luv::tracy-source-location "test/abi"))
                      (outer (luv::%tracy-emit-zone-begin location 1))
                      (inner (luv::%tracy-emit-zone-begin location 1))
                      (active (if (luv:tracy-connected-p) 1 0)))
                 (luv::%tracy-emit-zone-end inner)
                 (luv::%tracy-emit-zone-end outer)
                 ;; A structure returned indirectly would leave the high half
                 ;; holding whatever the register happened to contain.
                 (ok (= active (ldb (byte 32 32) outer)))
                 (ok (= active (ldb (byte 32 32) inner)))))
          (when ours (luv:stop-tracy))))))

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
      (let ((sample (luvcraft::make-luvcraft-frame-sample)))
        (setf (luvcraft::luvcraft-frame-sample-frame-seconds sample)
              (/ (1+ index) 1000d0)
              (aref samples index) sample)))
    (let ((benchmark
            (luvcraft::make-luvcraft-frame-benchmark :samples samples)))
      (multiple-value-bind (median p95 mean maximum)
          (luvcraft::luvcraft-frame-metric-summary
           benchmark #'luvcraft::luvcraft-frame-sample-frame-seconds)
        (ok (= 2.5d0 median))
        (ok (= 4d0 p95))
        (ok (= 2.5d0 mean))
        (ok (= 4d0 maximum))))))

(deftest streaming-frame-summary-covers-only-the-transition
  (let ((samples (make-array 4)))
    (dotimes (index 4)
      (let ((sample (luvcraft::make-luvcraft-frame-sample)))
        (setf (luvcraft::luvcraft-frame-sample-frame-seconds sample)
              (/ (1+ index) 1000d0)
              (aref samples index) sample)))
    (let* ((benchmark
             (luvcraft::make-luvcraft-frame-benchmark
              :scenario :streaming :samples samples
              :completion-seconds 0.004d0
              :entering-chunk-count 9 :settled-frame 1))
           (transition
             (luvcraft::luvcraft-frame-benchmark-transition-samples benchmark))
           (text
             (with-output-to-string (stream)
               (luvcraft:print-luvcraft-frame-benchmark benchmark stream))))
      (ok (= 2 (length transition)))
      (ok (search "9 entering chunks, 2 frames" text))
      (ok (search "settled: frame 1" text)))))

(defclass gated-production-request (luvcraft::production-request)
  ((gate :initarg :gate :reader gated-production-request-gate)
   (value :initarg :value :reader gated-production-request-value)))

(defmethod luvcraft::perform-production-request ((request gated-production-request))
  (sb-thread:wait-on-semaphore (gated-production-request-gate request))
  (gated-production-request-value request))

(defclass title-canvas ()
  ((title :initarg :title :accessor canvas-title)))

(defun production-system-active-request (system)
  (sb-thread:with-mutex ((luvcraft::production-system-lock system))
    (luvcraft::production-system-active-request system)))

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
    (edit-block-at luvcraft::*stone-block* world 2 14 2)
    (rematerialize-little-world-chunk source world 0 0)
    (ok (eq (world-block-at world 2 14 2) luvcraft::*stone-block*))
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
                       luvcraft::*crystal-block* -19 8 44)
    (record-block-edit (little-world-source-edits source) nil 3 4 -5)
    (let ((description
            (make-luvcraft-save-description
             world :camera camera :player player
             :selected-block luvcraft::*crystal-block*)))
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
                  luvcraft::*crystal-block*))
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
            (ok (eq block luvcraft::*crystal-block*)))
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
          (ok (eq selected-block luvcraft::*crystal-block*)))))))

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
         (data (luvcraft::frame-uniform-data session 1280 720))
         (declaration
           (luv.arithmetic:value-declaration-for :frame-uniform-data))
         (host-layout
           (luv.arithmetic:declaration-quantity-layout declaration))
         (block (luvcraft.shaders:block-world-camera-uniform-block))
         (shader-layout (luvcraft::frame-shader-uniform-product-layout block)))
    (ok (eq declaration
            (luv.arithmetic:value-declaration-for :frame-uniform-data)))
    (ok (typep data
               (luv.arithmetic:declaration-representation-type declaration)))
    (ok (= 72 (luv.arithmetic:quantity-layout-extent host-layout)))
    (ok (luv.arithmetic:quantity-layout= host-layout shader-layout))
    (ok (= 288 (luvcraft::block-world-camera-uniform-size session)))
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
  (ok (eq :srgb-to-linear
          (texture-format-sample-transfer
           luvcraft::+block-atlas-texture-format+)))
  (ok (eq luvcraft::+block-atlas-texture-format+
          (luvcraft::ensure-block-atlas-sample-transfer
           luvcraft::+block-atlas-texture-format+)))
  (ok (signals
       (luvcraft::ensure-block-atlas-sample-transfer :rgba8-unorm)
       'error))
  (let ((atlas (make-block-texture-atlas)))
    (ok (equal (array-dimensions atlas) '(16 160)))
    (ok (subtypep (array-element-type atlas) '(unsigned-byte 32)))
    (ok (= (ldb (byte 8 24) (aref atlas 8 8)) 255))
    (ok (/= (aref atlas 8 8) (aref atlas 8 (+ 8 (* 3 16)))))
    (ok (/= (aref atlas 8 8) (aref atlas 8 (+ 8 (* 9 16))))))
  (flet ((face (name)
           (find name luvcraft::*block-faces* :key #'block-face-name)))
    (ok (= (block-face-tile luvcraft::*grass-block* (face :top)) 0))
    (ok (= (block-face-tile luvcraft::*grass-block* (face :front)) 1))
    (ok (= (block-face-tile luvcraft::*grass-block* (face :bottom)) 2))
    (ok (= (block-face-tile luvcraft::*wood-block* (face :top)) 5))
    (ok (= (block-face-tile luvcraft::*sand-block* (face :top)) 7))
    (ok (= (block-face-tile luvcraft::*snow-block* (face :top)) 8))
    (ok (= (block-face-tile *crystal-block* (face :top)) 9))
    (ok (= (block-light-emission *crystal-block*) 12))
    (ok (= (block-surface-emission *crystal-block*) 1.2))
    (ok (= (length (placeable-block-kinds)) 8)))
  (let ((world (make-block-world :chunk-width 2
                                 :chunk-height 2
                                 :chunk-depth 2)))
    (ensure-world-chunk world 0 0 0)
    (setf (world-block-at world 0 0 0) luvcraft::*stone-block*)
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
    (ok (gethash luvcraft::*grass-block* materials))
    (ok (gethash luvcraft::*sand-block* materials))
    (ok (gethash luvcraft::*snow-block* materials))))

(deftest crosshair-and-numbered-materials-are-playable-state
  (let* ((vertices (luvcraft::make-block-world-crosshair-vertices 960 640))
         (canvas (make-instance 'title-canvas :title "luvcraft test"))
         (session (make-instance 'luvcraft-session
                                 :canvas canvas
                                 :title-base "luvcraft test"
                                 :selected-block luvcraft::*stone-block*)))
    (ok (= (length vertices)
           (* luvcraft::+block-world-crosshair-vertex-count+ 6)))
    (ok (eq (select-luvcraft-block session 1) luvcraft::*grass-block*))
    (ok (eq (luvcraft-session-selected-block session) luvcraft::*grass-block*))
    (ok (eq (select-luvcraft-block session 7) luvcraft::*snow-block*))
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
             (funcall (luvcraft::luvcraft-gazetteer-view-world-factory view))))
      (ok (eq (world-block-at world 16 1 8) *crystal-block*))
      (ok (= (nth-value 1 (world-light-at world 16 1 8))
             (block-light-emission *crystal-block*)))
      (ok (= (nth-value 1 (world-light-at world 15 1 8))
             (1- (block-light-emission *crystal-block*)))))))

(deftest shadow-yard-gazetteer-has-raised-casters-over-receiver
  (let* ((view (find-luvcraft-gazetteer-view "shadow-yard"))
         (world (funcall (luvcraft::luvcraft-gazetteer-view-world-factory view))))
    (ok (eq (world-block-at world 7 0 7) luvcraft::*snow-block*))
    (ok (eq (world-block-at world 9 1 10) luvcraft::*stone-block*))
    (ok (eq (world-block-at world 10 8 10) luvcraft::*stone-block*))
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
         (first-rows (luvcraft::shadow-frame-rows first-camera sky))
         (nearby-rows (luvcraft::shadow-frame-rows nearby-camera sky))
         (farther-rows (luvcraft::shadow-frame-rows farther-camera sky)))
    ;; The first two rows locate the orthographic footprint.  Translation
    ;; smaller than one 0.0625-world-unit shadow texel cannot move it.
    (ok (equal (subseq first-rows 0 8) (subseq nearby-rows 0 8)))
    (ok (not (equal (subseq first-rows 0 8)
                    (subseq farther-rows 0 8))))))

(deftest shadow-projection-is-continuous-through-old-up-axis-threshold
  (let* ((camera (make-instance 'fly-camera))
         (profile (make-default-sky-profile))
         (before
           (luvcraft::shadow-frame-rows
            camera
            (sky-frame-parameters
             (make-instance 'sky-clock :pinned-day-fraction 0.451)
             profile)))
         (after
           (luvcraft::shadow-frame-rows
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
              luvcraft::+luvcraft-shadow-half-extent+
              luvcraft::+luvcraft-shadow-half-extent+)
           0.99))))

(deftest temporal-frame-derivatives-expose-change-and-flicker
  (let ((first #(10 20 30 255 40 50 60 255))
        (second #(13 17 36 255 40 50 60 255))
        (third #(16 14 42 255 43 53 63 255)))
    (multiple-value-bind (difference mean maximum changed)
        (luvcraft::temporal-derivative-rgba second first 10.0)
      (ok (equalp difference #(40 40 40 255 0 0 0 255)))
      (ok (< (abs (- mean (/ 2.0 255.0))) 1e-6))
      (ok (< (abs (- maximum (/ 4.0 255.0))) 1e-6))
      (ok (= changed 0.5)))
    (multiple-value-bind (difference mean maximum changed)
        (luvcraft::temporal-derivative-rgba third second 10.0 first)
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
        (setf (world-block-at world x 0 z) luvcraft::*stone-block*)))
    ;; Gravity settles the body exactly on the block tops.
    (dotimes (step 240)
      (declare (ignorable step))
      (step-block-world-player player world camera keys (/ 1d0 120d0)))
    (ok (< (abs (- (player-y player) 1d0)) 1d-5))
    (ok (player-grounded-p player))
    (ok (< (abs (- (camera-y camera) 2.62d0)) 1d-5))
    ;; A held right input accelerates into, but not through, a two-block wall.
    (setf (world-block-at world 3 1 1) luvcraft::*stone-block*
          (world-block-at world 3 2 1) luvcraft::*stone-block*
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
        (setf (world-block-at world x 0 z) luvcraft::*stone-block*)))
    (setf (world-block-at world 3 1 1) luvcraft::*stone-block*
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
    (setf (world-block-at world 1 0 0) luvcraft::*stone-block*
          (world-block-at world 2 0 0) luvcraft::*stone-block*)
    (let ((mesher (make-instance 'exposed-face-mesher)))
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 10))
      (let ((revision (block-world-revision world)))
        (setf (world-block-at world 2 0 0) nil)
        (ok (= (block-world-revision world) (1+ revision))))
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 6))
      (setf (world-block-at world 2 0 0) luvcraft::*stone-block*)
      (ok (= (block-mesh-face-count (mesh-block-world mesher world)) 10)))))

(deftest chunk-mesh-is-exactly-sized-and-preserves-the-public-emitter
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (chunk (ensure-world-chunk world 0 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (world-block-at world 0 0 0) luvcraft::*stone-block*)
    (let* ((mesh (mesh-block-chunk mesher world chunk))
           (vertices (block-mesh-vertices mesh)))
      (ok (= (block-mesh-face-count mesh) 6))
      (ok (= (length vertices)
             (* (block-mesh-face-count mesh)
                luvcraft::+block-mesh-floats-per-face+)))
      (ok (= (array-total-size vertices) (length vertices))))
    ;; Tools may still emit a single semantic face through the exported API;
    ;; the optimized neighborhood object remains an implementation detail.
    (let ((vertices
            (make-array luvcraft::+block-mesh-floats-per-face+
                        :element-type 'single-float :fill-pointer 0)))
      (emit-block-face mesher world vertices luvcraft::*stone-block*
                       (find :top luvcraft::*block-faces*
                             :key #'block-face-name)
                       0 0 0)
      (ok (= (length vertices) luvcraft::+block-mesh-floats-per-face+)))))

(deftest immutable-mesh-snapshot-is-bit-identical-to-owner-side-meshing
  (let* ((world (make-little-block-world :chunk-radius 1 :seed 121))
         (chunk (world-chunk-at world 0 0 0))
         (mesher (make-instance 'exposed-face-mesher))
         (stamp (chunk-mesh-dependency-stamp world chunk))
         (snapshot (make-block-mesh-snapshot world chunk stamp))
         (direct (mesh-block-chunk mesher world chunk))
         (copied (mesh-block-snapshot mesher snapshot)))
    (ok (equal stamp (block-mesh-snapshot-dependency-stamp snapshot)))
    (let ((halo (luvcraft::block-mesh-snapshot-halo-domain snapshot)))
      (ok (= (luv.domains:domain-cardinality halo)
             (length (luvcraft::block-mesh-snapshot-sample-indices snapshot))
             (length (luvcraft::block-mesh-snapshot-sky-samples snapshot))
             (length
              (luvcraft::block-mesh-snapshot-block-light-samples snapshot)))))
    (ok (eq (luvcraft.world.fields:field-definition-for :sky-light)
            (luvcraft::block-mesh-snapshot-sky-definition snapshot)))
    (ok (eq (luvcraft.world.fields:field-definition-for :block-light)
            (luvcraft::block-mesh-snapshot-block-light-definition snapshot)))
    (ok (= (block-mesh-face-count direct) (block-mesh-face-count copied)))
    (ok (= (block-mesh-vertex-count direct) (block-mesh-vertex-count copied)))
    (ok (equalp (block-mesh-vertices direct) (block-mesh-vertices copied)))
    (setf (world-block-at world 0 0 0) nil)
    (ok (equalp (block-mesh-vertices copied)
                (block-mesh-vertices (mesh-block-snapshot mesher snapshot))))))

(deftest production-system-coalesces-desired-work-and-stops-cooperatively
  (let ((system (luvcraft::make-single-worker-production-system
                 :name "luv production test")))
    (unwind-protect
         (let* ((first
                  (make-instance
                   'luvcraft::little-world-load-request
                   :key '(:load (0 0 0)) :priority 4
                   :seed 1 :demand-token 1
                   :width 8 :height 8 :depth 8))
                (latest
                  (make-instance
                   'luvcraft::little-world-load-request
                   :key '(:load (0 0 0)) :priority 0
                   :seed 2 :demand-token 2
                   :width 8 :height 8 :depth 8)))
           (luvcraft::schedule-production-request system first)
           (luvcraft::schedule-production-request system latest)
           (multiple-value-bind (result present-p)
               (sb-concurrency:receive-message
                (luvcraft::production-system-result-mailbox system) :timeout 5.0)
             (ok present-p)
             (ok (null (luvcraft::production-result-condition result)))
             (ok (<= (luvcraft::production-system-pending-count system) 2))))
      (luvcraft::stop-production-system system))
    (ok (not (sb-thread:thread-alive-p
              (luvcraft::production-system-thread system))))))

(deftest production-system-keeps-one-result-behind-its-owner
  (let* ((system (luvcraft::make-single-worker-production-system
                  :name "luv production backpressure test"))
         (first-gate (sb-thread:make-semaphore :count 0))
         (second-gate (sb-thread:make-semaphore :count 0))
         (first (make-instance 'gated-production-request
                               :key :first :gate first-gate :value :first))
         (second (make-instance 'gated-production-request
                                :key :second :gate second-gate :value :second)))
    (unwind-protect
         (progn
           (luvcraft::schedule-production-request system first)
           (ok (wait-until
                (lambda () (eq (production-system-active-request system)
                               first))))
           ;; Scheduling while FIRST is active must remain desired work, not a
           ;; second queued wake which can run behind an unread first result.
           (luvcraft::schedule-production-request system second)
           (sb-thread:signal-semaphore first-gate)
           (ok (wait-until
                (lambda ()
                  (and (= 1 (sb-concurrency:mailbox-count
                             (luvcraft::production-system-result-mailbox system)))
                       (not (eq (production-system-active-request system)
                                first))))))
           (ok (null (production-system-active-request system)))
           (ok (= 1 (sb-concurrency:mailbox-count
                     (luvcraft::production-system-result-mailbox system))))
           (ok (nth-value
                1 (gethash :second (luvcraft::production-system-desired system))))
           (multiple-value-bind (result present-p)
               (luvcraft::receive-production-result-no-hang system)
             (ok present-p)
             (ok (eq (luvcraft::production-result-value result) :first)))
           (ok (wait-until
                (lambda () (eq (production-system-active-request system)
                               second))))
           (sb-thread:signal-semaphore second-gate)
           (multiple-value-bind (result present-p)
               (sb-concurrency:receive-message
                (luvcraft::production-system-result-mailbox system) :timeout 2.0)
             (ok present-p)
             (ok (eq (luvcraft::production-result-value result) :second))))
      (sb-thread:signal-semaphore first-gate)
      (sb-thread:signal-semaphore second-gate)
      (luvcraft::stop-production-system system))))

(deftest prebuilt-world-remains-desired-for-asynchronous-meshing
  (let* ((world (make-block-world :chunk-width 8
                                  :chunk-height 8
                                  :chunk-depth 8))
         (first (ensure-world-chunk world -1 0 2))
         (second (ensure-world-chunk world 3 0 -4))
         (system (luvcraft::make-single-worker-production-system
                  :name "luv static residency test"))
         (session (make-instance 'luvcraft-session
                              :world world
                              :player (make-instance 'block-world-player
                                                     :position
                                                     (make-vec3 0d0 0d0 0d0))
                              :production-system system)))
    (unwind-protect
         (progn
           (luvcraft::maintain-luvcraft-residency session)
           (ok (gethash (luvcraft::block-chunk-key first)
                        (luvcraft-session-desired-chunks session)))
           (ok (gethash (luvcraft::block-chunk-key second)
                        (luvcraft-session-desired-chunks session)))
           (remove-world-chunk world -1 0 2)
           (luvcraft::maintain-luvcraft-residency session)
           (ok (not (gethash (luvcraft::block-chunk-key first)
                             (luvcraft-session-desired-chunks session))))
           (ok (gethash (luvcraft::block-chunk-key second)
                        (luvcraft-session-desired-chunks session))))
      (luvcraft::stop-production-system system))))

(deftest chunk-mesh-products-have-narrow-neighbor-dependencies
  (let* ((world (make-block-world :chunk-width 4
                                  :chunk-height 4
                                  :chunk-depth 4))
         (left (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 0 0))
         (mesher (make-instance 'exposed-face-mesher)))
    (setf (world-block-at world 3 1 1) luvcraft::*stone-block*
          (world-block-at world 4 1 1) luvcraft::*stone-block*)
    (ok (= (block-mesh-face-count (mesh-block-chunk mesher world left)) 5))
    (ok (= (block-mesh-face-count (mesh-block-chunk mesher world right)) 5))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      ;; This changes RIGHT, but not the boundary LEFT's mesh observes.
      (setf (world-block-at world 5 2 2) luvcraft::*stone-block*)
      (ok (equal stamp (chunk-mesh-dependency-stamp world left)))
      ;; This touches RIGHT's -X boundary and must invalidate LEFT.
      (setf (world-block-at world 4 2 2) luvcraft::*stone-block*)
      (ok (not (equal stamp (chunk-mesh-dependency-stamp world left)))))
    (let ((stamp (chunk-mesh-dependency-stamp world left)))
      (remove-world-chunk world 0 0 0)
      (let ((replacement (ensure-world-chunk world 0 0 0)))
        (ok (not (equal stamp
                        (chunk-mesh-dependency-stamp world replacement))))))))

(defun test-luvcraft-chunk-product (chunk stamp)
  (make-instance
   'luvcraft::luvcraft-chunk-product
   :coordinate (chunk-domain-coordinate (block-chunk-domain chunk))
   :dependency-stamp stamp
   :mesh nil :vertex-buffer nil))

(deftest boundary-mesh-replacements-publish-as-one-visible-cohort
  (let* ((world (make-block-world :chunk-width 2
                                  :chunk-height 2
                                  :chunk-depth 2))
         (left (ensure-world-chunk world 0 0 0))
         (right (ensure-world-chunk world 1 0 0))
         (session (make-instance 'luvcraft::luvcraft-session :world world))
         (left-key '(0 0 0))
         (right-key '(1 0 0)))
    (setf (gethash left-key (luvcraft::luvcraft-session-desired-chunks session)) t
          (gethash right-key (luvcraft::luvcraft-session-desired-chunks session)) t
          (world-block-at world 1 0 0) luvcraft::*stone-block*
          (world-block-at world 2 0 0) luvcraft::*stone-block*)
    (let ((old-left
            (test-luvcraft-chunk-product
             left (chunk-mesh-dependency-stamp world left)))
          (old-right
            (test-luvcraft-chunk-product
             right (chunk-mesh-dependency-stamp world right))))
      (setf (gethash left-key (luvcraft::luvcraft-session-chunk-products session))
            old-left
            (gethash right-key (luvcraft::luvcraft-session-chunk-products session))
            old-right)
      ;; Removing RIGHT's boundary block also exposes a face owned by LEFT.
      ;; One completed replacement must leave the whole old pair visible.
      (setf (world-block-at world 2 0 0) nil)
      (let ((new-right
              (test-luvcraft-chunk-product
               right (chunk-mesh-dependency-stamp world right))))
        (setf (gethash right-key
                       (luvcraft::luvcraft-session-staged-chunk-products session))
              new-right)
        (ok (zerop (luvcraft::publish-ready-luvcraft-meshes session)))
        (ok (eq old-left
                (gethash left-key
                         (luvcraft::luvcraft-session-chunk-products session))))
        (ok (eq old-right
                (gethash right-key
                         (luvcraft::luvcraft-session-chunk-products session))))
        (let ((new-left
                (test-luvcraft-chunk-product
                 left (chunk-mesh-dependency-stamp world left))))
          (setf (gethash left-key
                         (luvcraft::luvcraft-session-staged-chunk-products session))
                new-left)
          (ok (= 2 (luvcraft::publish-ready-luvcraft-meshes session)))
          (ok (eq new-left
                  (gethash left-key
                           (luvcraft::luvcraft-session-chunk-products session))))
          (ok (eq new-right
                  (gethash right-key
                           (luvcraft::luvcraft-session-chunk-products session)))))))))

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
                              :selected-block luvcraft::*dirt-block*)))
    (ensure-world-chunk world 0 0 0)
    ;; The second stone means placing after removing the first still has a
    ;; solid target beyond the empty adjacent site.
    (setf (world-block-at world 2 1 1) luvcraft::*stone-block*
          (world-block-at world 3 1 1) luvcraft::*stone-block*)
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
                           :selected-block luvcraft::*dirt-block*)))
      (multiple-value-bind (coordinate status)
          (edit-luvcraft-block occupied-session :place)
        (ok (null coordinate))
        (ok (eq status :blocked))
        (ok (null (world-block-at world 2 1 1)))))
    (multiple-value-bind (coordinate status)
        (edit-luvcraft-block session :place)
      (ok (eq status :edited))
      (ok (= (world-coordinate-x coordinate) 2))
      (ok (eq (world-block-at world 2 1 1) luvcraft::*dirt-block*)))))
