(defpackage #:luft.render.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:clim #:clim)
                    (#:climi #:clim-internals)
                    (#:luv #:luv)
                    (#:production #:luv.production)
                    (#:render #:luft.render)))

(in-package #:luft.render.tests)

(defun make-two-chunk-streaming-scene ()
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 7)))
    ;; These cells share a face across the X chunk boundary.
    (luft.render::scene-builder-cell builder 63 4 4)
    (luft.render::scene-builder-cell builder 64 4 4)
    (render:make-streaming-scene
     (luft.render::finish-scene-builder builder) :frames-per-load 1)))

(deftest a-streaming-mesh-request-owns-an-immutable-residency-snapshot
  (let* ((scene (make-two-chunk-streaming-scene))
         (left (luft:chunk-key-at 63 4))
         (right (luft:chunk-key-at 64 4)))
    (setf (gethash left (luft.render::streaming-scene-loaded scene)) t)
    (let* ((snapshot
             (luft.render::make-streaming-mesh-snapshot
              scene left luft:+mesh-bevel-width+))
           (request
             (make-instance 'luft.render::streaming-mesh-request
                            :key left :snapshot snapshot))
           (before
             (production:perform-production-request request)))
      (ok (luft.render::current-streaming-mesh-request-p scene request))
      (setf (gethash right (luft.render::streaming-scene-loaded scene)) t)
      (ok (not (luft.render::current-streaming-mesh-request-p scene request)))
      ;; The old request remains independently executable, while a current
      ;; oracle observes and closes the newly resident cross-chunk seam.
      (ok (equalp (luft:surface-mesh-face-instance-words before)
                  (luft:surface-mesh-face-instance-words
                   (production:perform-production-request request))))
      (ok (not (equalp
                (luft:surface-mesh-face-instance-words before)
                (luft:surface-mesh-face-instance-words
                 (render:mesh-streaming-chunk
                  scene left luft:+mesh-bevel-width+))))))))

(deftest a-streaming-residency-cohort-publishes-only-when-complete
  (let* ((scene (make-two-chunk-streaming-scene))
         (left (luft:chunk-key-at 63 4))
         (right (luft:chunk-key-at 64 4)))
    (setf (gethash left (luft.render::streaming-scene-loaded scene)) t
          (gethash right (luft.render::streaming-scene-loaded scene)) t
          (luft.render::streaming-scene-cohort scene) (list left right))
    (flet ((request (key ticket)
             (let ((request
                     (make-instance
                      'luft.render::streaming-mesh-request
                      :key key
                      :snapshot
                      (luft.render::make-streaming-mesh-snapshot
                       scene key luft:+mesh-bevel-width+))))
               (setf (production:production-request-ticket request) ticket
                     (gethash key
                              (luft.render::streaming-scene-outstanding scene))
                     ticket)
               request)))
      (let ((left-request (request left 1))
            (right-request (request right 2)))
        (ok (luft.render::accept-streaming-mesh-result
             scene left-request :left-mesh))
        (ok (null (luft.render::ready-streaming-scene-meshes scene)))
        (ok (luft.render::accept-streaming-mesh-result
             scene right-request :right-mesh))
        (ok (equal (list (cons left :left-mesh)
                         (cons right :right-mesh))
                   (luft.render::ready-streaming-scene-meshes scene)))))))

(defun key-event (class key-name &key character modifiers repeat-p)
  (make-instance class
                 :timestamp 0
                 :key-name key-name
                 :character character
                 :unshifted-character character
                 :modifiers modifiers
                 :repeat-p repeat-p))

(defun key-press (key-name &key character modifiers repeat-p)
  (key-event 'luv:canvas-key-press-event key-name
             :character character :modifiers modifiers :repeat-p repeat-p))

(defun key-release (key-name &key character modifiers)
  (key-event 'luv:canvas-key-release-event key-name
             :character character :modifiers modifiers))

(deftest the-viewer-is-the-mcclim-application
  (ok (string= "1/8" (luft.render::bevel-width-label 1)))
  (ok (string= "1/4" (luft.render::bevel-width-label 2)))
  (ok (string= "1/2" (luft.render::bevel-width-label 4)))
  (ok (= 2 (luft.render::next-bevel-width 1)))
  (ok (= 4 (luft.render::next-bevel-width 2)))
  (ok (= 1 (luft.render::next-bevel-width 4)))
  (let ((viewer (clim:make-application-frame 'render:viewer)))
    (ok (typep viewer 'clim:application-frame))
    (ok (null (climi::frame-process viewer)))
    (ok (not (luft.render::viewer-inspector-p viewer)))
    (ok (luft.render::viewer-inspector-p
         (clim:make-application-frame 'render:viewer :inspector-p t)))
    (ok (null (luft.render::viewer-key-command viewer (key-press :w))))
    (ok (equal '(luft.render::com-release-pointer)
               (luft.render::viewer-key-command
                viewer (key-press :escape))))
    (setf (luft.render::viewer-pointer-captured-p viewer) t)
    (ok (equal '(luft.render::com-start-moving :forward)
               (luft.render::viewer-key-command viewer (key-press :w))))
    (ok (equal '(luft.render::com-stop-moving :forward)
               (luft.render::viewer-key-command viewer (key-release :w))))
    (ok (equal '(luft.render::com-reset-view)
               (luft.render::viewer-key-command viewer (key-press :r))))
    (ok (equal '(luft.render::com-toggle-construction-lines)
               (luft.render::viewer-key-command viewer (key-press :c))))
    (ok (equal '(luft.render::com-toggle-bevel-width)
               (luft.render::viewer-key-command viewer (key-press :b))))
    (ok (equal '(luft.render::com-toggle-fullscreen)
               (luft.render::viewer-key-command viewer (key-press :f11))))
    (ok (equal '(luft.render::com-quit)
               (luft.render::viewer-key-command
                viewer (key-press :q :character #\q
                                     :modifiers '(:control)))))
    (clim:execute-frame-command
     viewer (luft.render::viewer-key-command viewer (key-press :w)))
    (ok (luft.render::viewer-control-active-p viewer :forward))
    (clim:execute-frame-command
     viewer (luft.render::viewer-key-command viewer (key-release :w)))
    (ok (not (luft.render::viewer-control-active-p viewer :forward)))))

(deftest orthographic-walk-moves-on-the-ground-without-zooming
  (let* ((viewer (clim:make-application-frame 'render:viewer))
         (camera (render:viewer-camera viewer))
         (before (render:camera-position camera))
         (render:*projection* :isometric)
         (render:*isometric-height* 18.0))
    (setf (luft.render::viewer-last-timestamp viewer) 1.0)
    (luft.render::set-viewer-control viewer :forward t)
    (luft.render::advance-viewer-camera viewer 1.1)
    (let ((after (render:camera-position camera)))
      (ok (or (/= (luv.arithmetic.lisp.vec3:vec3-x before)
                  (luv.arithmetic.lisp.vec3:vec3-x after))
              (/= (luv.arithmetic.lisp.vec3:vec3-y before)
                  (luv.arithmetic.lisp.vec3:vec3-y after))))
      (ok (= (luv.arithmetic.lisp.vec3:vec3-z before)
             (luv.arithmetic.lisp.vec3:vec3-z after)))
      (ok (= 18.0 render:*isometric-height*)))))

(deftest the-spike-scene-is-three-site-instance-streams
  (let* ((mesh (render:make-render-mesh
                (render:make-manifold-spike-scene)))
         (templates (luft:surface-mesh-template-vertex-words mesh)))
    (ok (plusp (luft:surface-mesh-face-triangle-count mesh)))
    (ok (plusp (luft:surface-mesh-band-triangle-count mesh)))
    (ok (plusp (luft:surface-mesh-fan-triangle-count mesh)))
    (ok (plusp (luft:surface-mesh-singular-star-count mesh)))
    (ok (plusp (luft:surface-mesh-face-instance-count mesh)))
    (ok (plusp (luft:surface-mesh-band-instance-count mesh)))
    (ok (plusp (luft:surface-mesh-fan-instance-count mesh)))
    (ok (plusp (luft:surface-mesh-template-count mesh)))
    (ok (zerop (mod (length templates)
                    luft:+mesh-template-vertex-word-count+)))))

(deftest the-walking-player-belongs-to-the-sanctuary
  (ok (luft.render::scene-player-p
       (render:make-mountain-sanctuary-scene)))
  (ok (not (luft.render::scene-player-p
            (render:make-manifold-spike-scene))))
  (ok (not (luft.render::scene-player-p
            (render:make-miter-study-scene)))))

(defun instance-signature (base-x base-y base-z packed vertices start count)
  (let ((signature
          (make-array (+ 5 (* count luft:+mesh-template-vertex-word-count+))
                      :element-type '(unsigned-byte 32))))
    (setf (aref signature 0) base-x
          (aref signature 1) base-y
          (aref signature 2) base-z
          (aref signature 3) (logand packed #xffff0000)
          (aref signature 4) count)
    (replace signature vertices :start1 5
                                :start2 (* start
                                           luft:+mesh-template-vertex-word-count+)
                                :end2 (* (+ start count)
                                         luft:+mesh-template-vertex-word-count+))
    signature))

(defun word-vector< (left right)
  (loop for a across left
        for b across right
        when (/= a b) return (< a b)
        finally (return (< (length left) (length right)))))

(defun mesh-instance-signatures (mesh)
  (let ((ranges (luft:surface-mesh-template-ranges mesh))
        (vertices (luft:surface-mesh-template-vertex-words mesh))
        (signatures nil))
    (dolist (words (list (luft:surface-mesh-face-instance-words mesh)
                         (luft:surface-mesh-band-instance-words mesh)
                         (luft:surface-mesh-fan-instance-words mesh)))
      (loop for offset from 0 below (length words) by 4
            for packed = (aref words (+ offset 3))
            for template-id = (ldb (byte 16 0) packed)
            for start = (aref ranges (* 2 template-id))
            for count = (aref ranges (1+ (* 2 template-id)))
            do (push (instance-signature
                      (aref words offset) (aref words (+ offset 1))
                      (aref words (+ offset 2)) packed vertices start count)
                     signatures)))
    signatures))

(defun population-instance-signatures (population)
  (let* ((words (luft.render::render-population-instance-words population))
         (vertices (luft.render::render-population-template-words population))
         (triangle-count
           (luft.render::render-population-triangle-instance-count population))
         (signatures nil))
    (loop for offset from 0 below (length words) by 4
          for instance-index from 0
          for packed = (aref words (+ offset 3))
          for template-id = (ldb (byte 16 0) packed)
          for count = (if (< instance-index triangle-count) 3 6)
          for start = (* template-id luft.render::+render-template-vertex-count+)
          do (push (instance-signature
                    (aref words offset) (aref words (+ offset 1))
                    (aref words (+ offset 2)) packed vertices start count)
                   signatures))
    signatures))

(deftest resident-meshes-form-one-exact-two-draw-population
  (let* ((miter (render:make-render-mesh (render:make-miter-study-scene)))
         (spike (render:make-render-mesh (render:make-manifold-spike-scene)))
         (meshes (list miter spike))
         (population (luft.render::make-render-population meshes))
         (source-signatures
           (mapcan #'mesh-instance-signatures meshes))
         (population-signatures
           (population-instance-signatures population))
         (triangle-count
           (luft.render::render-population-triangle-instance-count population))
         (quad-count
           (luft.render::render-population-quad-instance-count population)))
    (ok (equalp (sort source-signatures #'word-vector<)
                (sort population-signatures #'word-vector<)))
    (ok (= (+ triangle-count quad-count)
           (+ (luft:surface-mesh-face-instance-count miter)
              (luft:surface-mesh-band-instance-count miter)
              (luft:surface-mesh-fan-instance-count miter)
              (luft:surface-mesh-face-instance-count spike)
              (luft:surface-mesh-band-instance-count spike)
              (luft:surface-mesh-fan-instance-count spike))))
    (ok (<= (+ (if (plusp triangle-count) 1 0)
               (if (plusp quad-count) 1 0))
            2))))

(deftest canonical-templates-are-shared-between-resident-meshes
  (let* ((mesh (render:make-render-mesh (render:make-miter-study-scene)))
         (single (luft.render::make-render-population (list mesh)))
         (double (luft.render::make-render-population (list mesh mesh)))
         (stride (* luft.render::+render-template-vertex-count+
                    luft:+mesh-template-vertex-word-count+)))
    (ok (= (/ (length (luft.render::render-population-template-words single))
              stride)
           (/ (length (luft.render::render-population-template-words double))
              stride)))
    (ok (= (* 2 (length (luft.render::render-population-instance-words single)))
           (length (luft.render::render-population-instance-words double))))))

(deftest the-connected-miter-study-uses-the-site-stream-abi
  (dolist (bevel-width '(1 2 4))
    (let ((mesh (render:make-render-mesh
                 (render:make-miter-study-scene)
                 :bevel-width bevel-width)))
      (ok (= bevel-width (luft:surface-mesh-bevel-width mesh)))
      (if (= bevel-width 4)
          (progn
            (ok (zerop (luft:surface-mesh-face-triangle-count mesh)))
            (ok (zerop (luft:surface-mesh-band-triangle-count mesh))))
          (progn
            (ok (plusp (luft:surface-mesh-face-triangle-count mesh)))
            (ok (plusp (luft:surface-mesh-band-triangle-count mesh)))))
      (ok (plusp (luft:surface-mesh-fan-triangle-count mesh)))
      (ok (zerop (luft:surface-mesh-singular-star-count mesh)))
      (ok (luft::%mesh-closed-p mesh))
      (let ((lattice (luft.render::mesh-lattice-point-words mesh)))
        (ok (loop for offset from 3 below (length lattice) by 4
                  thereis (zerop (aref lattice offset))))
        (ok (loop for offset from 3 below (length lattice) by 4
                  thereis (= 1 (aref lattice offset))))
        (ok (loop for offset from 3 below (length lattice) by 4
                  thereis (= 2 (aref lattice offset))))
        (ok (loop for offset from 0 below (length lattice) by 4
                  always
                  (or (/= 2 (aref lattice (+ offset 3)))
                      (and
                       (zerop (mod (aref lattice offset)
                                   luft:+mesh-cell-size+))
                       (zerop (mod (aref lattice (+ offset 1))
                                   luft:+mesh-cell-size+))
                       (zerop (mod (aref lattice (+ offset 2))
                                   luft:+mesh-cell-size+))))))))))

(deftest terrain-chamfers-use-one-side-material
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-cell builder 4 4 4)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene)))
    (flet ((instance-stocks (words)
             (loop for offset from 3 below (length words) by 4
                   collect (ldb (byte 4 16) (aref words offset)))))
      ;; Every closure of an isolated terrain cell joins its top to a side,
      ;; so it resolves uniformly to soil (1), never grass (0).
      (dolist (words (list (luft:surface-mesh-band-instance-words mesh)
                           (luft:surface-mesh-fan-instance-words mesh)))
        (ok (plusp (length words)))
        (ok (every (lambda (stock) (= stock 1))
                   (instance-stocks words)))))))

(deftest flat-terrain-closures-continue-the-grass-material
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-box builder 4 5 4 5 4 4)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene)))
    (flet ((contains-grass-p (words)
             (loop for offset from 3 below (length words) by 4
                   thereis (zerop (ldb (byte 4 16) (aref words offset))))))
      (ok (contains-grass-p
           (luft:surface-mesh-band-instance-words mesh)))
      (ok (contains-grass-p
           (luft:surface-mesh-fan-instance-words mesh))))))

(deftest miter-study-chamfers-do-not-use-the-terrain-top-stock
  (let ((mesh (render:make-render-mesh (render:make-miter-study-scene))))
    (dolist (words (list (luft:surface-mesh-band-instance-words mesh)
                         (luft:surface-mesh-fan-instance-words mesh)))
      (ok (notany (lambda (stock) (zerop stock))
                  (loop for offset from 3 below (length words) by 4
                        collect (ldb (byte 4 16) (aref words offset))))))))

(deftest stone-terrain-chamfers-have-an-earth-set-reading
  (ok (= luft.render::+stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+stone-stock+))))
  (ok (= luft.render::+earth-set-stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+stone-stock+ luft.render::+grass-stock+))))
  (ok (= luft.render::+earth-set-stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+soil-stock+ luft.render::+stone-stock+))))
  (ok (= luft.render::+soil-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+grass-stock+ luft.render::+soil-stock+)))))

(deftest earth-set-readings-are-confined-to-stone-terrain-chamfers
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-box builder 4 6 4 6 2 2)
                  (luft.render::scene-builder-cell
                   builder 5 5 3 :architecture-p t)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene)))
    (flet ((stocks (words)
             (loop for offset from 3 below (length words) by 4
                   collect (ldb (byte 4 16) (aref words offset)))))
      (ok (notany (lambda (stock)
                    (= stock luft.render::+earth-set-stone-stock+))
                  (stocks (luft:surface-mesh-face-instance-words mesh))))
      (ok (some (lambda (stock)
                  (= stock luft.render::+earth-set-stone-stock+))
                (append
                 (stocks (luft:surface-mesh-band-instance-words mesh))
                 (stocks (luft:surface-mesh-fan-instance-words mesh))))))))

(deftest directional-star-ambient-occlusion-measures-the-outward-hemisphere
  (ok (= 0 (luft::%directional-star-ambient-occlusion #b00000000 '(0 0 1))))
  (ok (= 1 (luft::%directional-star-ambient-occlusion #b00010000 '(0 0 1))))
  (ok (= 3 (luft::%directional-star-ambient-occlusion #b11110000 '(0 0 1))))
  (ok (= 3 (luft::%directional-star-ambient-occlusion #b10001000 '(1 1 0)))))

(deftest topology-ao-is-confined-to-bevels-and-junctions
  (let ((mesh (render:make-render-mesh (render:make-miter-study-scene))))
    (flet ((levels (words)
             (loop for offset from 3 below (length words) by 4
                   collect (ldb (byte 2 20) (aref words offset)))))
      (ok (every #'zerop (levels (luft:surface-mesh-face-instance-words mesh))))
      (ok (some #'plusp
                (append (levels (luft:surface-mesh-band-instance-words mesh))
                        (levels (luft:surface-mesh-fan-instance-words mesh))))))))

(deftest mesh-and-presentation-shaders-lower-through-both-conventional-backends
  (let* ((vertex (luft.render.shaders:mesh-vertex-specification))
         (fragment (luft.render.shaders:mesh-fragment-specification))
         (lattice-vertex
           (luft.render.shaders:lattice-point-vertex-specification))
         (lattice-fragment
           (luft.render.shaders:lattice-point-fragment-specification))
         (player-vertex
           (luft.render.shaders:player-sdf-vertex-specification))
         (player-fragment
           (luft.render.shaders:player-sdf-fragment-specification))
         (present-vertex
           (luft.render.shaders:present-vertex-specification))
         (present-fragment
           (luft.render.shaders:present-fragment-specification))
         (vertex-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl vertex)))
         (fragment-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl fragment)))
         (present-fragment-msl
           (luv.msl:msl-document-source
            (luv.msl:compile-msl present-fragment))))
    (ok (search "[[vertex_id]]" vertex-msl))
    (ok (search "[[instance_id]]" vertex-msl))
    (ok (search "const device uint4* instances" vertex-msl))
    (ok (search "const device uint4* template_vertices" vertex-msl))
    (ok (search "barycentric" fragment-msl))
    (ok (search "motion_output" fragment-msl))
    (ok (search "depth2d<float> scene_depth" present-fragment-msl))
    (ok (search "[[instance_id]]"
                (luv.msl:msl-document-source
                 (luv.msl:compile-msl lattice-vertex))))
    (ok (luv.msl:compile-msl lattice-fragment))
    (ok (luv.spir-v:compile-shader-specification vertex))
    (ok (luv.spir-v:compile-shader-specification fragment))
    (ok (luv.spir-v:compile-shader-specification lattice-vertex))
    (ok (luv.spir-v:compile-shader-specification lattice-fragment))
    (ok (luv.msl:compile-msl player-vertex))
    (ok (luv.msl:compile-msl player-fragment))
    (ok (luv.spir-v:compile-shader-specification player-vertex))
    (ok (luv.spir-v:compile-shader-specification player-fragment))
    (ok (luv.spir-v:compile-shader-specification present-vertex))
    (ok (luv.spir-v:compile-shader-specification present-fragment))))

(deftest the-camera-block-packs-both-projections
  (let ((camera (render:make-fly-camera)))
    (flet ((lane (projection)
             (let ((render:*projection* projection))
               (let ((view
                       (luft.render::capture-frame-view
                        camera 1100 800 #(0.0 0.0))))
                 (luft.render::camera-uniform-data
                  view view #(0.5 0.5 0.001 0.001) 1.0 7.25)))))
      (let ((perspective (lane :perspective))
            (isometric (lane :isometric))
            (quarter
              (let ((render:*projection* :isometric))
                (let ((view
                        (luft.render::capture-frame-view
                         camera 1100 800 #(0.0 0.0))))
                  (luft.render::camera-uniform-data
                   view view #(0.5 0.5 0.001 0.001) 1.0 7.25 2)))))
        (ok (= 56 (length perspective)))
        (ok (typep perspective '(simple-array single-float (56))))
        (ok (= 1.0 (aref perspective 22)))
        (ok (= 0.0 (aref isometric 22)))
        (flet ((depth (data view-z)
                 (let ((clip (+ (* view-z (aref data 18)) (aref data 19))))
                   (if (zerop (aref data 22)) clip (/ clip view-z)))))
          (ok (< (abs (depth perspective 0.1)) 1d-4))
          (ok (< (abs (- (depth perspective 200.0) 1.0)) 1d-4))
          (ok (< (abs (depth isometric
                            luft.render::+orthographic-near+)) 1d-4))
          (ok (< (abs (- (depth isometric
                               luft.render::+orthographic-far+) 1.0))
                 1d-4)))
        (ok (= (aref perspective 20) 0.125))
        (ok (= (aref quarter 20) 0.25))
        (ok (= (aref perspective 21) render:*wireframe*))
        (ok (equalp #(0.5 0.5 0.001 0.001)
                    (subseq perspective 48 52)))
        (ok (equalp #(29.5 24.5 15.48 7.25)
                    (subseq perspective 52 56)))))))

(deftest a-pointer-ray-retains-the-semantic-boundary-site
  (let* ((domain (luft:make-world-domain :horizontal-bits 4))
         (builder (luft:make-chain-builder domain)))
    (luft:chain-builder-add-site
     builder (luft:make-site domain 4 4 4 luft:+cell-extent+ 1))
    (let* ((solid (luft:finish-chain-builder builder))
           (inspection
             (luft.render::raycast-site
              solid
              (luv.arithmetic.lisp.vec3:make-vec3 4.5 4.5 8.0)
              (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 -1.0)))
           (site (luft.render::site-inspection-site inspection))
           (cell (luft.render::site-inspection-cell inspection)))
      (ok inspection)
      (ok (= 3.0 (luft.render::site-inspection-distance inspection)))
      (ok (= luft:+xy-face-extent+ (luft:site-extent site)))
      (ok (luft:site-positive-p site))
      (ok (= 4 (luft:site-x site) (luft:site-x cell)))
      (ok (= 4 (luft:site-y site) (luft:site-y cell)))
      (ok (= 5 (luft:site-z site)))
      (ok (= 4 (luft:site-z cell)))
      (ok (= #x80 (render:site-inspection-star-mask inspection)))
      (ok (not (luft:star-singular-p
                (render:site-inspection-star-mask inspection)))))))
