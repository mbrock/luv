(defpackage #:luft.render.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true))

(in-package #:luft.render.tests)

(define-test atlas-is-one-fixed-complete-table
  (let ((words (luft.render::star-meshlet-template-words)))
    (true (= (* 256 luft.render::+star-meshlet-record-count+ 4)
             (length words)))
    (dotimes (star 256)
      (let ((block (* star luft.render::+star-meshlet-record-count+ 4)))
        (true (= (length (luft:star-atlas-owned-triangles star))
                 (aref words block)))))))

(define-test terrain-shaders-are-direct-mesh-stages
  (let* ((mesh (luft.render.shaders:mesh-vertex-specification))
         (shadow (luft.render.shaders:shadow-vertex-specification))
         (fragment (luft.render.shaders:star-fragment-specification))
         (output (luv.shader:shader-specification-mesh-output mesh)))
    (true (eq :mesh (luv.shader:shader-specification-stage mesh)))
    (true (eq :mesh (luv.shader:shader-specification-stage shadow)))
    (true (= 75 (luv.shader:shader-mesh-output-max-vertices output)))
    (true (= 25 (luv.shader:shader-mesh-output-max-primitives output)))
    (true (luv.msl:compile-msl mesh))
    (true (luv.spir-v:compile-shader-specification mesh))
    (true (luv.msl:compile-msl shadow))
    (true (luv.spir-v:compile-shader-specification shadow))
    (true (luv.msl:compile-msl fragment))
    (true (luv.spir-v:compile-shader-specification fragment))
    (let ((scene-bindings
            (mapcar #'luv.shader:shader-resource-binding
                    (luv.shader:shader-specification-resources mesh)))
          (shadow-bindings
            (mapcar #'luv.shader:shader-resource-binding
                    (luv.shader:shader-specification-resources shadow))))
      (true (member 3 scene-bindings)
            "the scene mesh stage binds active-star appearance")
      (true (member 6 scene-bindings)
            "the scene mesh stage binds the material descriptor table")
      (true (not (member 3 shadow-bindings))
            "the shadow mesh stage remains geometry-only")
      (true (not (member 6 shadow-bindings))
            "the shadow mesh stage does not bind material descriptors"))))

(define-test one-cell-is-eight-workgroups-with-no-cpu-geometry
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (builder (luft:make-chain-builder domain)))
    (luft:chain-builder-add-site
     builder (luft:make-site domain 4 4 4 luft:+cell-extent+ 1))
    (let* ((chain (luft:finish-chain-builder builder))
           (key (luft:chunk-key-at 4 4))
           (mesh
             (handler-bind
                 ((luft:missing-chunk
                    (lambda (condition)
                      (declare (ignore condition))
                      (invoke-restart 'luft:treat-as-air))))
               (luft:mesh-star-chunk
                chain key
                :outside-domain-policy :air)))
           (population (luft.render::make-render-population (list mesh))))
      (true (= 8 (luft.render::render-population-mesh-workgroup-count
                  population)))
      (true (= 32 (length (luft:surface-mesh-star-site-words mesh))))
      (true (= 64 (length (luft:surface-mesh-appearance-codes mesh))))
      (loop for site-offset from 3 below 32 by 4
            for appearance-offset from 0 by 8
            for star = (aref (luft:surface-mesh-star-site-words mesh)
                             site-offset)
            do (dotimes (sample 8)
                 (true (= (if (logbitp sample star) 1 0)
                          (aref (luft:surface-mesh-appearance-codes mesh)
                                (+ appearance-offset sample)))))))))

(define-test material-repainting-is-an-appearance-only-product
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (builder (luft:make-chain-builder domain))
         (first (luft:make-site domain 4 4 4 luft:+cell-extent+ 1))
         (second (luft:make-site domain 5 4 4 luft:+cell-extent+ 1))
         (materials (make-hash-table :test #'eql))
         (vocabulary (luft.render::make-scene-material-vocabulary))
         (descriptors
           (luft.render::compile-terrain-material-descriptors vocabulary)))
    (luft:chain-builder-add-site builder first)
    (luft:chain-builder-add-site builder second)
    (let* ((chain (luft:finish-chain-builder builder))
           (mesh
             (handler-bind
                 ((luft:missing-chunk
                    (lambda (condition)
                      (declare (ignore condition))
                      (invoke-restart 'luft:treat-as-air))))
               (luft:mesh-star-chunk chain (luft:chunk-key-at 4 4)
                                     :outside-domain-policy :air)))
           (geometry (copy-seq (luft:surface-mesh-star-site-words mesh)))
           (triangles (luft:surface-mesh-triangle-count mesh)))
      ;; Dense code 1 is earth and code 3 is limestone (air remains code 0).
      (setf (gethash first materials) 0
            (gethash second materials) 2)
      (luft.render::compile-surface-mesh-appearance
       mesh materials descriptors)
      (let ((mixed (copy-seq (luft:surface-mesh-appearance-codes mesh))))
        (setf (gethash first materials) 1)
        (luft.render::compile-surface-mesh-appearance
         mesh materials descriptors)
        (true (not (equalp mixed
                           (luft:surface-mesh-appearance-codes mesh)))
              "material-only repaint replaces the sidecar")
        (true (equalp geometry (luft:surface-mesh-star-site-words mesh))
              "material-only repaint preserves every geometry byte")
        (true (= triangles (luft:surface-mesh-triangle-count mesh))
              "material-only repaint preserves triangle count")
        (true
         (loop for site-offset from 3 below (length geometry) by 4
               for appearance-offset from 0 by 8
               for star = (aref geometry site-offset)
               for codes = (subseq mixed appearance-offset
                                   (+ appearance-offset 8))
               thereis
               (and (find 1 codes) (find 3 codes)
                    (some
                     (lambda (masks)
                       (let ((mask (first masks)))
                         (and (> (logcount mask) 1)
                              (loop for sample below 8
                                    thereis
                                    (and (logbitp sample mask)
                                         (= 1 (aref codes sample))))
                              (loop for sample below 8
                                    thereis
                                    (and (logbitp sample mask)
                                         (= 3 (aref codes sample)))))))
                     (luft:star-atlas-owned-appearance-masks star))))
         "one emitted band or junction reduces both material samples")))))

(defun same-material-cell-table-p (left right)
  (and (= (hash-table-count left) (hash-table-count right))
       (loop for cell being the hash-keys of left using (hash-value offset)
             always (eql offset (gethash cell right)))))

(define-test authored-world-source-is-deterministic-and-seam-independent
  (let* ((domain (luft:make-world-domain :x-bits 8 :y-bits 8))
         (source (make-instance 'luft.render::authored-world-source
                                :domain domain :seed 121))
         (left-key (luft:chunk-key-at 0 0))
         (right-key (luft:chunk-key-at 64 0))
         (left-a
           (luft.render::materialize-authored-world-chunk
            source left-key 1))
         (left-b
           (luft.render::materialize-authored-world-chunk
            source left-key 2))
         (right
           (luft.render::materialize-authored-world-chunk
            source right-key 3)))
    (true (luft:chain=
           (luft.render::resident-cell-chunk-chain left-a)
           (luft.render::resident-cell-chunk-chain left-b))
          "incarnation does not enter deterministic source content")
    (true (same-material-cell-table-p
           (luft.render::resident-cell-chunk-material-cells left-a)
           (luft.render::resident-cell-chunk-material-cells left-b)))
    ;; The authored road crosses this exact chunk seam. Both independently
    ;; generated columns expose its continuous limestone surface.
    (loop for x in '(63 64)
          for resident in (list left-a right)
          for y = (round (luft.render::large-world-road-centre-y x))
          for z = (1- (luft.render::large-world-terrain-height source x y))
          for cell = (luft:make-site domain x y z luft:+cell-extent+ 1)
          do (true (= 1 (luft:chain-cell-occupancy-bit
                         (luft.render::resident-cell-chunk-chain resident)
                         x y z)))
             (true (= 2 (gethash
                         cell
                         (luft.render::resident-cell-chunk-material-cells
                          resident)))
                   "the cross-seam road is authored limestone"))
    (true (<= (abs
               (- (luft.render::large-world-river-centre-x 255)
                  (luft.render::large-world-river-centre-x 256)))
              2.0d0)
          "the authored river has no chunk-coordinate discontinuity")
    (true (luft.render::large-world-citadel-cell-p 1464 640 31)
          "the masonry destination spans its western chunk seam")))

(define-test authored-world-sparse-edits-survive-rematerialization
  (let* ((domain (luft:make-world-domain :x-bits 8 :y-bits 8))
         (source (make-instance 'luft.render::authored-world-source
                                :domain domain :seed 121))
         (key (luft:chunk-key-at 64 0))
         (x 70) (y (round (luft.render::large-world-road-centre-y x)))
         (z (1- (luft.render::large-world-terrain-height source x y)))
         (cell (luft:make-site domain x y z luft:+cell-extent+ 1)))
    (setf (gethash cell (luft.render::authored-world-source-edits source)) nil)
    (let ((resident
            (luft.render::materialize-authored-world-chunk
             source key 1
             :edits (luft.render::capture-authored-world-chunk-edits
                     source key))))
      (true (zerop (luft:chain-cell-occupancy-bit
                    (luft.render::resident-cell-chunk-chain resident) x y z))
            "an explicit-air edit overrides regenerated road content")
      (true (not (nth-value
                  1 (gethash
                     cell
                     (luft.render::resident-cell-chunk-material-cells
                      resident))))))))

(define-test authored-world-demand-rejects-stale-incarnations
  (let* ((scene (luft.render:make-authored-world-streaming-scene
                 :horizontal-bits 8 :residency-radius 0))
         (source (luft.render::streaming-scene-source scene))
         (key (luft:chunk-key-at 64 64))
         (request
           (make-instance
            'luft.render::authored-chunk-load-request
            :key '(:test-load) :priority 0 :scene scene :source source
            :chunk-key key :demand-token 7 :incarnation 12 :edits nil))
         (resident
           (luft.render::%make-resident-cell-chunk
            key 12 (luft:make-chain (luft:chain-domain
                                     (luft.render:scene-solid scene)))
            (make-hash-table :test #'eql))))
    (setf (luv.production:production-request-ticket request) 3
          (gethash key (luft.render::streaming-scene-load-outstanding scene)) 3
          (gethash key (luft.render::streaming-scene-desired scene)) 8)
    (true (not (luft.render::accept-authored-chunk-load-result
                scene request resident))
          "a result from the old demand token cannot install")
    (setf (gethash key (luft.render::streaming-scene-desired scene)) 7)
    (true (luft.render::accept-authored-chunk-load-result
           scene request resident)
          "the exact token, ticket, key, and incarnation install once")))

(define-test authored-world-settled-residency-does-not-rematerialize-gameplay
  (let* ((scene (luft.render:make-authored-world-streaming-scene
                 :horizontal-bits 8 :residency-radius 0))
         (domain (luft:chain-domain (luft.render:scene-solid scene)))
         (key (luft:chunk-key-at 64 64))
         (resident
           (luft.render::%make-resident-cell-chunk
            key 1 (luft:make-chain domain) (make-hash-table :test #'eql)))
         (revision (luft.render::scene-content-revision scene)))
    (setf (luft.render::streaming-scene-focus scene) (cons 1 1)
          (gethash key (luft.render::streaming-scene-desired scene)) 1
          (gethash key (luft.render::streaming-scene-store scene)) resident
          (gethash key (luft.render::streaming-scene-loaded scene)) 1)
    (true (not (luft.render::retarget-authored-world scene nil 1 64 64)))
    (true (= revision (luft.render::scene-content-revision scene))
          "a settled frame does not rebuild immutable gameplay values")))

(define-test authored-world-residency-is-bounded-and-absence-is-explicit
  (let* ((scene (luft.render:make-authored-world-streaming-scene
                 :horizontal-bits 8 :residency-radius 1))
         (domain (luft:chain-domain (luft.render:scene-solid scene)))
         (empty (luft:make-chain domain))
         (materials (make-hash-table :test #'eql))
         (keep (luft:chunk-key-at 64 64))
         (evict (luft:chunk-key-at 192 192)))
    (setf (gethash keep (luft.render::streaming-scene-desired scene)) 1
          (gethash keep (luft.render::streaming-scene-store scene))
          (luft.render::%make-resident-cell-chunk keep 1 empty materials)
          (gethash evict (luft.render::streaming-scene-store scene))
          (luft.render::%make-resident-cell-chunk evict 2 empty materials))
    (true (equal (list evict)
                 (luft.render::evict-undesired-authored-world-residents scene)))
    (true (= 1 (hash-table-count
                (luft.render::streaming-scene-store scene))))
    (true (eq :open-sky
              (luft.render::streaming-scene-cell-state scene 64 64 60)))
    (true (eq :unknown-nonresident
              (luft.render::streaming-scene-cell-state scene 0 0 60)))
    (true (eq :closed-boundary
              (luft.render::streaming-scene-cell-state scene -1 0 60)))))

(define-test authored-world-player-spawns-supported-with-a-traversable-window
  (let* ((scene (luft.render:make-authored-world-streaming-scene))
         (source (luft.render::streaming-scene-source scene))
         (player (luft.render::make-scene-walking-player scene))
         (position (luft.render:walking-player-position player))
         (x (floor (luv.arithmetic.lisp.vec3:vec3-x position)))
         (y (floor (luv.arithmetic.lisp.vec3:vec3-y position)))
         (z (floor (luv.arithmetic.lisp.vec3:vec3-z position)))
         (key (luft:chunk-key-at x y))
         (resident
           (luft.render::materialize-authored-world-chunk source key 1)))
    (true (= 0 (luft.render::streaming-scene-residency-radius scene))
          "ordinary play keeps the proved one-source render publication")
    (true (= 1 luft.render::+authored-world-gameplay-radius+)
          "collision reads a wider resident guard for seam crossing")
    (true (= (luft.render::large-world-terrain-height source x y) z)
          "the authored spawn derives its foot height from the source")
    (setf (luft.render:scene-solid scene)
          (luft.render::resident-cell-chunk-chain resident))
    (true (luft.render::walking-player-standable-cell-p scene x y z)
          "the player starts above rather than inside the road")
    (true (luft.render::walking-player-standable-cell-p scene (1+ x) y
                                                        (luft.render::large-world-terrain-height
                                                         source (1+ x) y))
          "the authored road has an immediately traversable neighboring cell")))

(define-test authored-world-gameplay-collision-crosses-the-render-seam
  (let* ((scene (luft.render:make-authored-world-streaming-scene
                 :horizontal-bits 8))
         (source (luft.render::streaming-scene-source scene))
         (left-key (luft:chunk-key-at 0 0))
         (right-key (luft:chunk-key-at 64 0))
         (left
           (luft.render::materialize-authored-world-chunk
            source left-key 1))
         (right
           (luft.render::materialize-authored-world-chunk
            source right-key 2)))
    (setf (gethash left-key (luft.render::streaming-scene-store scene)) left
          (gethash right-key (luft.render::streaming-scene-store scene)) right)
    (luft.render::rebuild-authored-world-resident-values
     scene (list left-key) (list left-key right-key))
    (loop for x in '(63 64)
          for y = (round (luft.render::large-world-road-centre-y x))
          for z = (luft.render::large-world-terrain-height source x y)
          do (true (luft.render::walking-player-standable-cell-p scene x y z)
                   "collision stays supported across the one-source render seam"))))

(define-test camera-yaw-follows-intent-smoothly
  (let ((camera (luft.render:make-fly-camera :yaw 0.0)))
    (luft.render::target-camera-yaw camera (/ pi 2))
    (true (zerop (luft.render:camera-yaw camera))
          "requesting a pose does not move the rendered camera immediately")
    (luft.render::advance-camera-response camera 0.1)
    (true (< 0.0 (luft.render:camera-yaw camera) (/ pi 2))
          "a frame advances only partway toward the requested pose")
    (let ((one-step (luft.render:camera-yaw camera)))
      (setf (luft.render:camera-yaw camera) 0.0)
      (luft.render::target-camera-yaw camera (/ pi 2))
      (luft.render::advance-camera-response camera 0.05)
      (luft.render::advance-camera-response camera 0.05)
      (true (< (abs (- one-step (luft.render:camera-yaw camera))) 1.0e-6)
            "the response is independent of frame subdivision"))))

(define-test camera-yaw-target-takes-the-shortest-turn
  (let ((camera (luft.render:make-fly-camera :yaw (- pi 0.1))))
    (luft.render::target-camera-yaw camera (+ (- pi) 0.1))
    (luft.render::advance-camera-response camera 0.05)
    (true (> (luft.render:camera-yaw camera) (- pi 0.1))
          "crossing the angle seam continues through the nearby orientation")
    (setf (luft.render:camera-yaw camera) 0.25)
    (true (= 0.25 (luft.render::camera-target-yaw camera))
          "an explicit pose assignment remains an immediate settled cut")))
