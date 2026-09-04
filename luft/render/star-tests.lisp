(defpackage #:luft.render.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:fail)
  (:import-from #:luv.arithmetic.lisp.vec3
                #:make-vec3 #:vec3-x #:vec3-y #:vec3-z)
  (:local-nicknames (#:render #:luft.render)
                    (#:shaders #:luft.render.shaders)
                    (#:quantities #:luft.render.quantities)
                    (#:math #:luv.arithmetic)
                    (#:records #:luv.arithmetic.records)
                    (#:arithmetic #:luv.arithmetic.lisp)
                    (#:production #:luv.production)))

(in-package #:luft.render.tests)

(define-test luft-status-is-semantic-only-with-no-pane-adapter
  (dolist (name '("VIEWER-STATUS-BAR"
                  "VIEWER-STATUS-BAR-ATTACHMENT"
                  "%OPEN-VIEWER-STATUS-BAR"
                  "OPEN-VIEWER-STATUS-BAR"))
    (true (null (find-symbol name '#:luft.render))))
  (true (find-method #'mcluv:status-bar-application-name
                     nil (list (find-class 'render:viewer)) nil))
  (true (find-method #'mcluv:status-bar-channels-for
                     nil (list (find-class 'render:viewer)) nil))
  (let ((viewer (allocate-instance (find-class 'render:viewer))))
    (true (= 12 (length (mcluv:status-bar-channels-for viewer))))
    (true (eq :mode (second (mcluv:status-bar-channels-for viewer))))
    (true (equal '(:coordinates :chunks :stream :bevel :view)
                 (last (mcluv:status-bar-channels-for viewer) 5))))
  (let ((position (make-vec3 130.25 63.75 17.0)))
    (true (string= "130.3,63.8,17.0"
                   (render::status-bar-position position)))
    (true (string= "2,0"
                   (render::status-bar-position-chunk position)))))

(define-test viewer-services-have-named-owners-with-no-instrument-protocol
  (let ((state (make-instance 'render::viewer-service-state)))
    (true (null (render:viewer-lobby-client state)))
    (true (null (render::viewer-tracy-capture-controller state)))
    (true (null (render::viewer-agent-service state)))
    (true (typep (render::viewer-service-lock state)
                 'sb-thread:mutex)))
  (dolist (name '("VIEWER-INSTRUMENTS"
                  "ADD-VIEWER-INSTRUMENT"
                  "REMOVE-VIEWER-INSTRUMENT"
                  "VIEWER-INSTRUMENT-PRIORITY"
                  "VIEWER-INSTRUMENT-PRESENT-P"
                  "REFRESH-VIEWER-INSTRUMENT"
                  "ENCODE-VIEWER-INSTRUMENT"
                  "HANDLE-VIEWER-INSTRUMENT-EVENT"
                  "QUIESCE-VIEWER-INSTRUMENT"
                  "RELEASE-VIEWER-INSTRUMENT"))
    (true (not (eq :external
                   (nth-value 1 (find-symbol name '#:luft.render))))))
  (let ((system (asdf:find-system "luft/render")))
    (true (null (find "luft/render/instruments"
                      (asdf:component-children system)
                      :key #'asdf:component-name
                      :test #'string=)))
    (true (null (find "luft/render/workbench-proof"
                      (asdf:component-children system)
                      :key #'asdf:component-name
                      :test #'string=)))))

(define-test atlas-is-one-fixed-complete-table
  (let ((words (render::star-meshlet-template-words)))
    (true (= (* 256 render::+star-meshlet-record-count+ 4)
             (length words)))
    (dotimes (star 256)
      (let ((block (* star render::+star-meshlet-record-count+ 4)))
        (true (= (length (luft:star-atlas-owned-triangles star))
                 (aref words block)))))))

(define-test terrain-shaders-are-direct-mesh-stages
  (let* ((mesh (shaders:mesh-vertex-specification))
         (shadow (shaders:shadow-vertex-specification))
         (fragment (shaders:star-fragment-specification))
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
           (population (render::make-render-population (list mesh))))
      (true (= 8 (render::render-population-mesh-workgroup-count
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
         (vocabulary (render::make-scene-material-vocabulary))
         (descriptors
           (render::compile-terrain-material-descriptors vocabulary)))
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
      (render::compile-surface-mesh-appearance
       mesh materials descriptors)
      (let ((mixed (copy-seq (luft:surface-mesh-appearance-codes mesh))))
        (setf (gethash first materials) 1)
        (render::compile-surface-mesh-appearance
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
         (source (make-instance 'render::authored-world-source
                                :domain domain :seed 121))
         (left-key (luft:chunk-key-at 0 0))
         (right-key (luft:chunk-key-at 64 0))
         (left-a (render::materialize-authored-world-chunk source left-key 1))
         (left-b (render::materialize-authored-world-chunk source left-key 2))
         (right (render::materialize-authored-world-chunk source right-key 3)))
    (true (luft:fibers=
           (render::resident-cell-chunk-fibers left-a)
           (render::resident-cell-chunk-fibers left-b))
          "incarnation does not enter deterministic source content")
    (true (same-material-cell-table-p
           (render::resident-cell-chunk-material-cells left-a)
           (render::resident-cell-chunk-material-cells left-b)))
    ;; The authored road crosses this exact chunk seam. Both independently
    ;; generated columns expose its continuous limestone surface.
    (loop for x in '(63 64)
          for resident in (list left-a right)
          for y = (round (render::large-world-road-centre-y x))
          for z = (1- (render::large-world-terrain-height source x y))
          for cell = (luft:make-site domain x y z luft:+cell-extent+ 1)
          do (true (= 1 (luft:fibers-cell-bit
                         (render::resident-cell-chunk-fibers resident)
                         x y z)))
             (true (= 2 (gethash
                         cell
                         (render::resident-cell-chunk-material-cells
                          resident)))
                   "the cross-seam road is authored limestone"))
    (true (<= (abs
               (- (render::large-world-river-centre-x 255)
                  (render::large-world-river-centre-x 256)))
              2.0d0)
          "the authored river has no chunk-coordinate discontinuity")
    (true (render::large-world-citadel-cell-p 1464 640 31)
          "the masonry destination spans its western chunk seam")))

(define-test authored-world-sparse-edits-survive-rematerialization
  (let* ((domain (luft:make-world-domain :x-bits 8 :y-bits 8))
         (source (make-instance 'render::authored-world-source
                                :domain domain :seed 121))
         (key (luft:chunk-key-at 64 0))
         (x 70) (y (round (render::large-world-road-centre-y x)))
         (z (1- (render::large-world-terrain-height source x y)))
         (cell (luft:make-site domain x y z luft:+cell-extent+ 1)))
    (setf (gethash cell (render::authored-world-source-edits source)) nil)
    (let ((resident
            (render::materialize-authored-world-chunk
             source key 1
             :edits (render::capture-authored-world-chunk-edits
                     source key))))
      (true (zerop (luft:fibers-cell-bit
                    (render::resident-cell-chunk-fibers resident) x y z))
            "an explicit-air edit overrides regenerated road content")
      (true (not (nth-value
                  1 (gethash
                     cell
                     (render::resident-cell-chunk-material-cells
                      resident))))))))

(define-test authored-world-demand-rejects-stale-incarnations
  (let* ((scene (render:make-authored-world-streaming-scene
                 :horizontal-bits 8 :residency-radius 0))
         (source (render::streaming-scene-source scene))
         (key (luft:chunk-key-at 64 64))
         (request
           (make-instance
            'render::authored-chunk-load-request
            :key '(:test-load) :priority 0 :scene scene :source source
            :chunk-key key :demand-token 7 :incarnation 12 :edits nil))
         (resident
           (render::%make-resident-cell-chunk
            key 12 (luft:make-chunk-fibers (render::scene-domain scene) key)
            (make-hash-table :test #'eql))))
    (setf (production:production-request-ticket request) 3
          (gethash key (render::streaming-scene-load-outstanding scene)) 3
          (gethash key (render::streaming-scene-desired scene)) 8)
    (true (not (render::accept-authored-chunk-load-result
                scene request resident))
          "a result from the old demand token cannot install")
    (setf (gethash key (render::streaming-scene-desired scene)) 7)
    (true (render::accept-authored-chunk-load-result
           scene request resident)
          "the exact token, ticket, key, and incarnation install once")))

(define-test authored-world-settled-residency-does-not-rematerialize-gameplay
  (let* ((scene (render:make-authored-world-streaming-scene
                 :horizontal-bits 8 :residency-radius 0))
         (domain (render::scene-domain scene))
         (key (luft:chunk-key-at 64 64))
         (resident
           (render::%make-resident-cell-chunk
            key 1 (luft:make-chunk-fibers domain key)
            (make-hash-table :test #'eql)))
         (revision (render::scene-content-revision scene)))
    (setf (render::streaming-scene-focus scene) (cons 1 1)
          (gethash key (render::streaming-scene-desired scene)) 1
          (gethash key (render::streaming-scene-store scene)) resident
          (gethash key (render::streaming-scene-loaded scene)) 1)
    (true (not (render::retarget-authored-world scene nil 1 64 64)))
    (true (= revision (render::scene-content-revision scene))
          "a settled frame does not rebuild immutable gameplay values")))

(define-test authored-world-residency-is-bounded-and-absence-is-explicit
  (let* ((scene (render:make-authored-world-streaming-scene
                 :horizontal-bits 8 :residency-radius 1))
         (domain (render::scene-domain scene))
         (materials (make-hash-table :test #'eql))
         (keep (luft:chunk-key-at 64 64))
         (evict (luft:chunk-key-at 192 192)))
    (setf (gethash keep (render::streaming-scene-desired scene)) 1
          (gethash keep (render::streaming-scene-store scene))
          (render::%make-resident-cell-chunk
           keep 1 (luft:make-chunk-fibers domain keep) materials)
          (gethash evict (render::streaming-scene-store scene))
          (render::%make-resident-cell-chunk
           evict 2 (luft:make-chunk-fibers domain evict) materials))
    (true (equal (list evict)
                 (render::evict-undesired-authored-world-residents scene)))
    (true (= 1 (hash-table-count
                (render::streaming-scene-store scene))))
    (true (eq :open-sky
              (render::streaming-scene-cell-state scene 64 64 60)))
    (true (eq :unknown-nonresident
              (render::streaming-scene-cell-state scene 0 0 60)))
    (true (eq :closed-boundary
              (render::streaming-scene-cell-state scene -1 0 60)))))

(define-test authored-world-player-spawns-supported-with-a-traversable-window
  (let* ((scene (render:make-authored-world-streaming-scene))
         (source (render::streaming-scene-source scene))
         (player (render::make-scene-walking-player scene))
         (position (render:walking-player-position player))
         (x (floor (vec3-x position)))
         (y (floor (vec3-y position)))
         (z (floor (vec3-z position)))
         (key (luft:chunk-key-at x y))
         (resident
           (render::materialize-authored-world-chunk source key 1)))
    (true (= 1 (render::streaming-scene-residency-radius scene))
          "ordinary play overlaps visible chunks across player seam crossings")
    (true (= 1 render::+authored-world-gameplay-radius+)
          "collision reads a wider resident guard for seam crossing")
    (true (= (render::large-world-terrain-height source x y) z)
          "the authored spawn derives its foot height from the source")
    (let ((store (luft:make-fiber-store (render::scene-domain scene))))
      (setf (luft:fiber-store-chunk store key)
            (render::resident-cell-chunk-fibers resident)
            (render:scene-solid scene) store))
    (true (render::walking-player-standable-cell-p scene x y z)
          "the player starts above rather than inside the road")
    (true (render::walking-player-standable-cell-p
           scene (1+ x) y
           (render::large-world-terrain-height source (1+ x) y))
          "the authored road has an immediately traversable neighboring cell")))

(define-test authored-world-player-waits-for-initial-collision-publication
  (let* ((scene (render:make-authored-world-streaming-scene))
         (source (render::streaming-scene-source scene))
         (player (render::make-scene-walking-player scene))
         (position (render:walking-player-position player))
         (x (vec3-x position))
         (y (vec3-y position))
         (z (vec3-z position))
         (key (luft:chunk-key-at (floor x) (floor y)))
         (camera (render::make-fly-camera)))
    ;; Demand completion is not collision publication. Even an accepted CPU
    ;; chunk must not start physics before the owner publishes SCENE-SOLID.
    (setf (gethash key (render::streaming-scene-store scene))
          (render::materialize-authored-world-chunk source key 1))
    (render::request-walking-player-jump player)
    (dotimes (i 10)
      (render::advance-walking-player player scene camera 1.0 0.0 0.1))
    (true (= x (vec3-x position)))
    (true (= y (vec3-y position)))
    (true (= z (vec3-z position)))
    (true (zerop (render::walking-player-vertical-velocity player)))
    (true (render::walking-player-jump-requested-p player))
    (true (equalp position (render::walking-player-previous-position player)))
    (render::rebuild-authored-world-resident-values scene (list key))
    (render::advance-walking-player player scene camera 0.0 0.0 0.1)
    (true (< z (vec3-z position) (+ z 1.0))
          "publication releases the queued jump with only this frame's time")
    (true (not (render::walking-player-jump-requested-p player)))
    (setf (luft:fiber-store-chunk (render:scene-solid scene) key)
          (luft:make-chunk-fibers (render::scene-domain scene) key))
    (let ((falling (render::make-scene-walking-player scene)))
      (render::advance-walking-player falling scene camera 0.0 0.0 0.1)
      (true (< (vec3-z (render:walking-player-position falling)) z)
            "published empty terrain is air, not a loading barrier"))))

(define-test authored-world-gameplay-collision-crosses-the-render-seam
  (let* ((scene (render:make-authored-world-streaming-scene
                 :horizontal-bits 8))
         (source (render::streaming-scene-source scene))
         (left-key (luft:chunk-key-at 0 0))
         (right-key (luft:chunk-key-at 64 0))
         (left (render::materialize-authored-world-chunk source left-key 1))
         (right (render::materialize-authored-world-chunk source right-key 2)))
    (setf (gethash left-key (render::streaming-scene-store scene)) left
          (gethash right-key (render::streaming-scene-store scene)) right)
    (render::rebuild-authored-world-resident-values
     scene (list left-key) (list left-key right-key))
    (loop for x in '(63 64)
          for y = (round (render::large-world-road-centre-y x))
          for z = (render::large-world-terrain-height source x y)
          do (true (render::walking-player-standable-cell-p scene x y z)
                   "collision stays supported across the one-source render seam"))))

(define-test camera-yaw-follows-intent-smoothly
  (let ((camera (render:make-fly-camera :yaw 0.0)))
    (render::target-camera-yaw camera (/ pi 2))
    (true (zerop (render:camera-yaw camera))
          "requesting a pose does not move the rendered camera immediately")
    (render::advance-camera-response camera 0.1)
    (true (< 0.0 (render:camera-yaw camera) (/ pi 2))
          "a frame advances only partway toward the requested pose")
    (let ((one-step (render:camera-yaw camera)))
      (setf (render:camera-yaw camera) 0.0)
      (render::target-camera-yaw camera (/ pi 2))
      (render::advance-camera-response camera 0.05)
      (render::advance-camera-response camera 0.05)
      (true (< (abs (- one-step (render:camera-yaw camera))) 1.0e-6)
            "the response is independent of frame subdivision"))))

(define-test camera-yaw-target-takes-the-shortest-turn
  (let ((camera (render:make-fly-camera :yaw (- pi 0.1))))
    (render::target-camera-yaw camera (+ (- pi) 0.1))
    (render::advance-camera-response camera 0.05)
    (true (> (render:camera-yaw camera) (- pi 0.1))
          "crossing the angle seam continues through the nearby orientation")
    (setf (render:camera-yaw camera) 0.25)
    (true (= 0.25 (render::camera-target-yaw camera))
          "an explicit pose assignment remains an immediate settled cut")))

(define-test material-edits-share-other-chunks-and-freeze-worker-input
  (let* ((scene (render:make-authored-world-streaming-scene
                 :horizontal-bits 8 :seed 121))
         (source (render::streaming-scene-source scene))
         (keys (list (luft:chunk-key-at 0 0) (luft:chunk-key-at 64 0))))
    (loop for key in keys for incarnation from 1
          do (setf (gethash key (render::streaming-scene-store scene))
                   (render::materialize-authored-world-chunk
                    source key incarnation)))
    (render::rebuild-authored-world-resident-values scene keys)
    (let* ((snapshot (render::snapshot-streaming-scene-input scene))
           (before (render::scene-material-cells snapshot))
           (other (gethash (second keys) (render::material-store-chunks before)))
           (domain (render::scene-domain scene))
           (cell (luft:make-site domain 20 20 0 luft:+cell-extent+ 1)))
      (true (nth-value 1 (render::material-cell-at before cell)))
      (multiple-value-bind (edit status key)
          (render::edit-streaming-scene-cell scene cell nil)
        (true edit)
        (true (eq :edited status))
        (true (= key (first keys)))
        (let ((after (render::scene-material-cells scene)))
          (true (not (nth-value 1 (render::material-cell-at after cell))))
          (true (nth-value 1 (render::material-cell-at before cell)))
          (true (eq other (gethash (second keys)
                                   (render::material-store-chunks after))))
          (true (= 1 (render::scene-cell-bit snapshot 20 20 0)))
          (true (zerop (render::scene-cell-bit scene 20 20 0)))))
      (render::edit-streaming-scene-cell
       scene cell render::*terrain-material-placement*)
      (multiple-value-bind (offset present-p)
          (render::material-cell-at (render::scene-material-cells scene) cell)
        (true present-p)
        (true (zerop offset) "offset zero is a real placement, not air")))))

(define-test walking-body-sweeps-ceilings-and-falls-after-floor-removal
  (let* ((domain (luft:make-world-domain :x-bits 8 :y-bits 8))
         (store (luft:make-fiber-store domain))
         (fibers (luft:make-chunk-fibers domain 0))
         (player (render::make-walking-player
                  :position (make-vec3 10.5 10.5 1.0))))
    (setf (luft:fiber-store-chunk store 0) fibers
          (luft:fibers-cell-bit fibers 10 10 0) 1
          (luft:fibers-cell-bit fibers 10 10 3) 1
          (render::walking-player-height player) 1.8
          (render::walking-player-grounded-p player) t)
    (render::request-walking-player-jump player)
    (dotimes (i 30)
      (render::advance-walking-player-vertical player store 0.016)
      (true (<= (+ (vec3-z (render:walking-player-position player)) 1.8)
                3.0001)))
    (true (render::walking-player-grounded-p player))
    (true (= 1.0 (vec3-z (render:walking-player-position player))))
    (luft:fiber-store-edit-cell store 10 10 0 0)
    (render::advance-walking-player-vertical player store 0.016)
    (true (not (render::walking-player-grounded-p player)))
    (true (< (vec3-z (render:walking-player-position player)) 1.0))))

(define-test first-person-camera-uses-the-body-and-centre-ray
  (let* ((viewer (allocate-instance (find-class 'render:viewer)))
         (player (render::make-walking-player
                  :position (make-vec3 12.5 19.5 8.0)))
         (camera (render::make-fly-camera :yaw 0.0 :pitch 0.0))
         (canvas (luv:make-sdl-canvas :width 800 :height 600))
         (render::*projection* :perspective))
    (setf (slot-value viewer 'render::player) player
          (slot-value viewer 'render::camera) camera
          (slot-value viewer 'render::canvas) canvas
          (slot-value viewer 'render::mode)
          (make-instance 'render::first-person-mode)
          (slot-value viewer 'render::pointer-captured-p) nil)
    (render::place-viewer-at-player-eyes viewer)
    (multiple-value-bind (origin direction) (render::viewer-pointer-ray viewer)
      (true (= 12.5 (vec3-x origin)))
      (true (< (abs (- (vec3-z origin) 9.62)) 0.0001))
      (true (= 1.0 (vec3-x direction)))
      (true (zerop (vec3-y direction)))
      (true (zerop (vec3-z direction))))))

(define-test chunk-material-lookups-preserve-light-propagation
  (let* ((domain (luft:make-world-domain :x-bits 8 :y-bits 8))
         (cells (make-hash-table :test #'eql))
         (opacity (make-array 1 :element-type '(unsigned-byte 8)
                               :initial-element 15))
         (cell (luft:make-site domain 64 12 5 luft:+cell-extent+ 1))
         (source (luft:make-site domain 63 12 5 luft:+cell-extent+ 1))
         (sources (vector (luft:make-voxel-light-source
                           source (luft:pack-voxel-light 8 4 2)))))
    (setf (gethash cell cells) 0)
    (let* ((store (render::material-store-from-table cells))
           (a (luft:solve-voxel-light domain cells opacity sources))
           (b (luft:solve-voxel-light
               domain (render::material-cell-reader store) opacity sources)))
      (loop for x from 61 to 66
            do (loop for y from 10 to 14
                     do (loop for z from 3 to 7
                              do (true (= (luft:voxel-light-at a x y z)
                                          (luft:voxel-light-at b x y z)))))))))

(define-test mesh-snapshots-retain-guard-chunk-materials
  (let* ((scene (render:make-authored-world-streaming-scene
                 :horizontal-bits 8 :seed 121))
         (source (render::streaming-scene-source scene))
         (keys (list (luft:chunk-key-at 0 0) (luft:chunk-key-at 64 0))))
    (loop for key in keys for incarnation from 1
          do (setf (gethash key (render::streaming-scene-store scene))
                   (render::materialize-authored-world-chunk
                    source key incarnation)))
    (render::rebuild-authored-world-resident-values scene (list (first keys)) keys)
    (let* ((cell (luft:make-site (render::scene-domain scene)
                                64 0 0 luft:+cell-extent+ 1))
           (snapshot (render::snapshot-streaming-scene-input scene)))
      (true (not (nth-value 1 (render::material-cell-at
                              (render::scene-material-cells scene) cell))))
      (true (nth-value 1 (render::material-cell-at
                         (render::scene-material-cells snapshot) cell)))
      (true (= 1 (render::scene-cell-bit snapshot 64 0 0))))))

(defun make-body-collision-fixture ()
  (let* ((domain (luft:make-world-domain :x-bits 8 :y-bits 8))
         (store (luft:make-fiber-store domain)))
    (setf (luft:fiber-store-chunk store 0) (luft:make-chunk-fibers domain 0))
    store))

(defun make-test-walking-body (x y z)
  (let ((player (render::make-walking-player :position (make-vec3 x y z))))
    (setf (render::walking-player-height player) 1.8)
    player))

(define-test walking-body-stops-at-wall-and-slides-without-climbing
  (let* ((store (make-body-collision-fixture))
         (player (make-test-walking-body 10.5 10.5 1.0))
         (position (render:walking-player-position player)))
    (loop for y from 8 to 16 do
      (loop for z from 1 to 5 do (luft:fiber-store-edit-cell store 12 y z 1)))
    (render::try-walking-player-axis player store :x 20.0)
    (true (< (abs (- (vec3-x position) 11.7)) 0.0001)
          "a long sweep stops a body radius before the wall")
    (true (= 1.0 (vec3-z position)) "horizontal movement never changes height")
    (render::try-walking-player-axis player store :y 2.0)
    (true (= 12.5 (vec3-y position)) "the touching body slides along the wall")
    (true (render::walking-player-clear-at-p store 11.7 12.5 1.0 1.8))
    (true (not (render::walking-player-clear-at-p store 11.9 12.5 1.0 1.8))
          "an empty centre column does not imply an empty body")
    (let ((cell (luft:make-site (luft:fiber-store-domain store)
                               11 12 1 luft:+cell-extent+ 1)))
      (setf (vec3-x position) 10.8)
      (true (render::walking-player-overlaps-cell-p player cell)
            "placement rejects blocks overlapping only the body's edge"))))

(define-test walking-body-sweeps-both-directions-on-every-axis
  (dolist (axis '(:x :y :z))
    (dolist (sign '(-1 1))
      (let* ((store (make-body-collision-fixture))
             (position (make-vec3 10.5 10.5 10.0))
             (x (if (eq axis :x) (if (plusp sign) 12 8) 10))
             (y (if (eq axis :y) (if (plusp sign) 12 8) 10))
             (z (if (eq axis :z) (if (plusp sign) 13 8) 10))
             (expected (if (eq axis :z) (if (plusp sign) 1.2 -1.0)
                           (* sign 1.2))))
        (luft:fiber-store-edit-cell store x y z 1)
        (multiple-value-bind (travel blocked-p)
            (render::sweep-walking-body-axis store position 1.8 0.3 axis (* sign 20.0))
          (true blocked-p)
          (true (< (abs (- travel expected)) 0.0001)))))))

(define-test walking-off-a-ledge-falls-instead-of-snapping-down
  (let* ((store (make-body-collision-fixture))
         (player (make-test-walking-body 10.5 10.5 4.0))
         (position (render:walking-player-position player)))
    (luft:fiber-store-edit-cell store 10 10 3 1)
    (luft:fiber-store-edit-cell store 11 10 0 1)
    (render::try-walking-player-axis player store :x 0.6)
    (render::advance-walking-player-vertical player store 0.1)
    (true (= 4.0 (vec3-z position)) "the trailing edge still has support")
    (render::try-walking-player-axis player store :x 0.4)
    (true (= 4.0 (vec3-z position)) "crossing the edge preserves foot height")
    (render::advance-walking-player-vertical player store 0.1)
    (true (< 3.8 (vec3-z position) 4.0) "gravity begins a continuous fall")
    (true (not (render::walking-player-grounded-p player)))
    (dotimes (i 60) (render::advance-walking-player-vertical player store (/ 1.0 120)))
    (true (= 1.0 (vec3-z position)))
    (true (render::walking-player-grounded-p player))))

(define-test walking-up-a-block-requires-a-physical-jump
  (let* ((store (make-body-collision-fixture))
         (player (make-test-walking-body 10.5 10.5 1.0))
         (position (render:walking-player-position player))
         (camera (render::make-fly-camera :yaw 0.0)))
    (loop for x from 8 to 20 do
      (luft:fiber-store-edit-cell store x 10 0 1)
      (when (>= x 11) (luft:fiber-store-edit-cell store x 10 1 1)))
    (dotimes (i 30) (render::advance-walking-player player store camera 1.0 0.0 (/ 1.0 60)))
    (true (< (abs (- (vec3-x position) 10.7)) 0.0001))
    (true (= 1.0 (vec3-z position)))
    (render::request-walking-player-jump player)
    (dotimes (i 50)
      (render::advance-walking-player player store camera 1.0 0.0 (/ 1.0 60))
      (true (render::walking-player-clear-at-p
             store (vec3-x position) (vec3-y position) (vec3-z position) 1.8)))
    (true (> (vec3-x position) 11.3))
    (true (= 2.0 (vec3-z position)))
    (true (render::walking-player-grounded-p player))))

(define-test walking-ballistics-check-storage-without-wrapping-values
  (math:define-unit 'walking-millisecond
    :reference :second :magnitude 1/1000 :quantity-kind :duration)
  (let* ((position (make-vec3 10.5 10.5 30.0))
         (player (render::make-walking-player :position position))
         (velocity
           (records:record-slot-declaration
            'render::walking-player 'render::vertical-velocity))
         (gravity (math:value-declaration-for
                   'render::+walking-player-gravity+))
         (duration render::*walking-duration-declaration*)
         (realization render::*walking-displacement-realization*)
         (result (math:declaration-quantity-specification
                  (arithmetic:lisp-arithmetic-realization-result-declaration
                   realization))))
    (true (eq position (render:walking-player-position player)))
    (true (not (eq position (render::walking-player-previous-position player))))
    (true (equalp position (render::walking-player-previous-position player)))
    (true (eq :difference (math:quantity-specification-character result)))
    (true (eq 'quantities:world-z-position
              (math:quantity-specification-name result)))
    (true (null (records:record-slot-declaration
                 'render::walking-player 'render::grounded-p)))
    (true (compiled-function-p render::*walking-displacement-function*))
    ;; Signed motion and both float representations retain the old equation.
    (dolist (v '(0.0 9.0 -7.0 9d0))
      (dolist (dt '(0.0 0.125 0.5 1d0))
        (true (= (+ (* v dt) (* 0.5 -24.0 dt dt))
                 (funcall render::*walking-displacement-function*
                          v -24.0 dt)))))
    (fail
     (arithmetic:bind-lisp-arithmetic-realization
      realization (list gravity velocity duration))
     'math:declaration-compatibility-error)
    (flet ((declaration (&rest options)
             (math:make-represented-value-declaration
              :representation-type 'real
              :quantity-specification
              (math:make-declared-quantity-specification options))))
      ;; Milliseconds require an explicit conversion, not a relabelled number.
      (fail
       (arithmetic:bind-lisp-arithmetic-realization
        realization
        (list velocity gravity
              (declaration :quantity 'quantities:elapsed-time
                           :unit 'walking-millisecond)))
       'math:declaration-compatibility-error)
      ;; The sweep consumes displacement, not an absolute foot height.
      (fail
       (arithmetic:bind-lisp-arithmetic-realization
        realization (list velocity gravity duration)
        :actual-result-declaration
        (declaration :quantity 'quantities:world-z-position
                     :unit 'quantities:cell :character :point))
       'math:declaration-compatibility-error))))

(define-test walking-gravity-agrees-across-render-frame-rates
  (let* ((store (make-body-collision-fixture))
         (camera (render::make-fly-camera))
         (heights
           (loop for rate in '(30 60 144) collect
             (let* ((player (make-test-walking-body 10.5 10.5 30.0))
                    (position (render:walking-player-position player)))
               (dotimes (i rate)
                 (render::advance-walking-player player store camera 0.0 0.0 (/ 1.0 rate)))
               (vec3-z position)))))
    (dolist (height heights)
      (true (< (abs (- height 18.0)) 0.001) "one second falls half g times t squared"))))

(define-test follow-camera-starts-inside-the-body-and-reserves-clearance
  (let* ((store (make-body-collision-fixture))
         (viewer (allocate-instance (find-class 'render:viewer)))
         (player (make-test-walking-body 10.5 10.5 1.0))
         (camera (render::make-fly-camera)))
    (loop for y from 8 to 13 do
      (loop for z from 0 to 6 do (luft:fiber-store-edit-cell store 11 y z 1)))
    (setf (slot-value viewer 'render::player) player
          (slot-value viewer 'render::camera) camera
          (slot-value viewer 'render::source) store
          (render::walking-player-heading-x player) 1.0
          (render::walking-player-heading-y player) 0.0
          (render::camera-position camera) (make-vec3 14.0 10.5 2.45))
    (render::constrain-viewer-follow-camera viewer)
    (true (< (vec3-x (render::camera-position camera)) 10.85)
          "look-ahead beyond the wall cannot put the camera through it")
    (true (> (vec3-x (render::camera-position camera)) 10.5))))

(define-test star-culling-sphere-encloses-the-complete-atlas
  ;; Culling assumes every possible emitted vertex lies within radius two
  ;; cells of its site, including concave and singular star templates.
  (let ((largest-square 0))
    (dotimes (star 256)
      (dolist (triangle (luft:star-atlas-owned-triangles star))
        (dolist (point triangle)
          (setf largest-square
                (max largest-square (reduce #'+ point :key (lambda (x) (* x x))))))))
    (true (< largest-square (* 4 luft:+mesh-cell-size+ luft:+mesh-cell-size+))
          "every atlas vertex is strictly inside the shader's culling sphere")))

(define-test streaming-star-reuse-matches-cold-compilation
  (let ((builder (render::make-scene-builder :horizontal-bits 8)))
    (dolist (xy '((10 10) (63 10) (64 10) (63 63) (64 64) (200 200)))
      (render::scene-builder-cell builder (first xy) (second xy) 2))
    (let* ((scene (render::make-streaming-scene
                   (render::finish-scene-builder builder)))
           (domain (render::scene-domain scene))
           (keys (mapcar (lambda (xy) (luft:chunk-key-at (first xy) (second xy)))
                         '((0 0) (64 0) (0 64) (64 64) (192 192)))))
      ;; Stable, explicitly resident empty guards exercise the same immutable
      ;; chunk identity contract as the authored-world loader.
      (dotimes (x 4)
        (dotimes (y 4)
          (let ((key (luft:chunk-key-at (* 64 x) (* 64 y))))
            (unless (gethash key (render::streaming-scene-store scene))
              (setf (gethash key (render::streaming-scene-store scene))
                    (luft:make-chunk-fibers domain key))))))
      (dolist (key keys)
        (setf (gethash key (render::streaming-scene-loaded scene)) 1))
      (labels ((compile-current ()
                 (production:perform-production-request
                  (make-instance
                   'render::streaming-mesh-request :key :test :priority 0
                   :snapshot (render::make-streaming-region-snapshot
                              scene keys 1))))
               (remember (result)
                 (setf (render::streaming-scene-mesh-cache scene)
                       (render::streaming-mesh-result-mesh-cache result)))
               (check-cold (warm)
                 (let ((saved (render::streaming-scene-mesh-cache scene)))
                   (unwind-protect
                        (progn
                          (setf (render::streaming-scene-mesh-cache scene) nil)
                          (let ((cold (compile-current)))
                            (loop for a in (render::streaming-mesh-result-meshes warm)
                                  for b in (render::streaming-mesh-result-meshes cold)
                                  for am = (render::prepared-render-mesh-mesh (cdr a))
                                  for bm = (render::prepared-render-mesh-mesh (cdr b))
                                  for ap = (render::prepared-render-mesh-population (cdr a))
                                  for bp = (render::prepared-render-mesh-population (cdr b))
                                  do (true (eql (car a) (car b)))
                                     (true (equalp (luft:surface-mesh-star-site-words am)
                                                  (luft:surface-mesh-star-site-words bm)))
                                     (true (equalp (luft:surface-mesh-appearance-codes am)
                                                  (luft:surface-mesh-appearance-codes bm)))
                                     (true (equalp (render::render-population-instance-words ap)
                                                  (render::render-population-instance-words bp)))
                                     (true (equalp (render::render-population-appearance-words ap)
                                                  (render::render-population-appearance-words bp))))))
                     (setf (render::streaming-scene-mesh-cache scene) saved)))))
        (let* ((first (compile-current))
               (far (cdr (assoc (car (last keys))
                                (render::streaming-mesh-result-meshes first))))
               (old-mesh (render::prepared-render-mesh-mesh far))
               (old-field (luft:surface-mesh-voxel-light old-mesh)))
          (remember first)
          ;; Interior, both sides of a seam, and an emissive edit must agree
          ;; byte-for-byte with a cold compile while distant products survive.
          (dolist (xy '((10 10) (63 10) (64 10) (63 63) (64 64)))
            (let* ((cell (luft:make-site domain (first xy) (second xy) 2
                                         luft:+cell-extent+ 1))
                   (material (render::scene-material-placement-at scene cell)))
              (dolist (placement (list nil material))
                (true (render::edit-streaming-scene-cell scene cell placement))
                (let* ((result (compile-current))
                       (new-far (cdr (assoc (car (last keys))
                                           (render::streaming-mesh-result-meshes result)))))
                  (true (eq (render::prepared-render-mesh-population far)
                            (render::prepared-render-mesh-population new-far)))
                  (true (not (eq old-mesh (render::prepared-render-mesh-mesh new-far))))
                  (true (eq old-field (luft:surface-mesh-voxel-light old-mesh)))
                  (true (not (eq old-field
                                 (luft:surface-mesh-voxel-light
                                  (render::prepared-render-mesh-mesh new-far)))))
                  (check-cold result)
                  (remember result)))))
          (let ((cell (luft:make-site domain 11 10 2 luft:+cell-extent+ 1)))
            (true (render::edit-streaming-scene-cell
                   scene cell render::*crystal-material-placement*))
            (let ((result (compile-current)))
              (check-cold result)
              (remember result)))
          ;; Repainting can replace a material chunk without changing fibers.
          ;; It must invalidate the old appearance even with identical geometry.
          (setf (render::scene-material-cells scene)
                (render::material-store-with-cell
                 (render::scene-material-cells scene)
                 (luft:make-site domain 10 10 2 luft:+cell-extent+ 1) 2))
          (check-cold (compile-current)))))))
