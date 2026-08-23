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
  (ok (= 2 luft:+mesh-bevel-width+))
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

(deftest scene-builders-translate-authored-sites-at-the-boundary
  (let* ((builder (luft.render::make-scene-builder
                   :horizontal-bits 5 :origin-x 7 :origin-y 11))
         (scene (progn
                  (luft.render::scene-builder-cell builder 2 3 4)
                  (luft.render::finish-scene-builder builder)))
         (solid (luft.render::scene-solid scene)))
    (ok (= 1 (luft:chain-cell-occupancy-bit solid 9 14 4)))
    (ok (zerop (luft:chain-cell-occupancy-bit solid 2 3 4)))))

(deftest the-sanctuary-curtain-is-bedded-into-the-mountain
  (let* ((scene (render:make-mountain-sanctuary-scene))
         (solid (luft.render::scene-solid scene))
         (domain (luft:chain-domain solid)))
    (flet ((occupied-p (x y z)
             (= 1 (luft:chain-cell-occupancy-bit solid x y z)))
           (architecture-p (x y z)
             (eq :architecture
                 (luft.render::material-placement-role
                  (luft.render::scene-material-placement-at
                   scene (luft:make-site domain x y z luft:+cell-extent+ 1))))))
      ;; The front curtain and both round keeps have continuous stone shoes
      ;; where the procedural ridge can otherwise fall below their fixed base.
      (dolist (point '((20 45) (40 45) (15 41) (45 41)))
        (destructuring-bind (x y) point
          (incf x luft.render::+sanctuary-origin-x+)
          (incf y luft.render::+sanctuary-origin-y+)
          (ok (occupied-p x y 17))
          (ok (occupied-p x y 18))
          (ok (architecture-p x y 17))
          (ok (architecture-p x y 18))))
      ;; The stair arrives at a supported masonry threshold, while the gate
      ;; opening itself remains clear at the sanctuary floor.
      (let ((x (+ 30 luft.render::+sanctuary-origin-x+))
            (y (+ 45 luft.render::+sanctuary-origin-y+)))
        (ok (occupied-p x y 18))
        (ok (architecture-p x y 18))
        (ok (not (occupied-p x y 19))))
      ;; Terrain and inhabited architecture now continue well beyond the old
      ;; 64-cell diorama, including the remote back-ridge beacon.
      (ok (occupied-p (+ luft.render::*sanctuary-beacon-x*
                         luft.render::+sanctuary-origin-x+)
                      (+ luft.render::*sanctuary-beacon-y*
                         luft.render::+sanctuary-origin-y+)
                      20))
      (ok (architecture-p
           (+ luft.render::*sanctuary-beacon-x*
              luft.render::+sanctuary-origin-x+)
           (+ luft.render::*sanctuary-beacon-y*
              luft.render::+sanctuary-origin-y+)
           (+ 8
              (luft.render::mountain-sanctuary-terrain-height
               luft.render::*sanctuary-beacon-x*
               luft.render::*sanctuary-beacon-y*)))))))

(deftest scene-cells-store-vocabulary-closed-material-offsets
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-cell builder 2 2 2)
                  (luft.render::scene-builder-cell
                   builder 2 2 3 :architecture-p t)
                  (luft.render::finish-scene-builder builder)))
         (domain (luft:chain-domain (luft.render::scene-solid scene)))
         (earth-site (luft:make-site domain 2 2 2 luft:+cell-extent+ 1))
         (stone-site (luft:make-site domain 2 2 3 luft:+cell-extent+ 1)))
    (ok (every (lambda (offset) (typep offset '(unsigned-byte 16)))
               (loop for offset being the hash-values
                       of (luft.render::scene-material-cells scene)
                     collect offset)))
    (ok (eq luft.render::*terrain-material-placement*
            (luft.render::scene-material-placement-at scene earth-site)))
    (ok (eq luft.render::*sanctuary-material-placement*
            (luft.render::scene-material-placement-at scene stone-site)))
    (ok (eq luft.render::*sanctuary-material-frame*
            (luft.render::material-placement-frame
             (luft.render::scene-material-placement-at scene stone-site))))))

(deftest semantic-surface-assemblies-retain-the-legacy-render-oracle
  (ok (equal
       (subseq
        (loop for assembly across
                (luv.domains:identity-vocabulary-members
                 luft.render::*surface-assembly-vocabulary*)
              collect (luft.render::surface-assembly-name assembly))
        0 9)
       '(:grass :soil :subsoil :limestone :turf-set-limestone
         :soil-set-limestone :deep-set-limestone :turf-edge
         :foundation-limestone)))
  (ok (equal (loop for offset below 9 collect offset)
             (list luft.render::+grass-stock+ luft.render::+soil-stock+
                   luft.render::+subsoil-stock+ luft.render::+stone-stock+
                   luft.render::+turf-set-stone-stock+
                   luft.render::+soil-set-stone-stock+
                   luft.render::+deep-set-stone-stock+
                   luft.render::+turf-edge-stock+
                   luft.render::+foundation-stone-stock+))))

(deftest semantic-material-migration-is-byte-identical-to-the-stock-oracle
  (labels ((legacy-face-stock (scene face)
             (multiple-value-bind (cell axis side)
                 (luft.render::face-solid-cell
                  (luft.render::scene-solid scene) face)
               (let ((placement
                       (luft.render::scene-material-placement-at scene cell)))
                 (cond ((eq :architecture
                            (luft.render::material-placement-role placement))
                        (if (luft.render::scene-foundation-cell-p scene cell)
                            luft.render::+foundation-stone-stock+
                            luft.render::+stone-stock+))
                       ((not (eq axis :z)) luft.render::+soil-stock+)
                       ((eq side :backward) luft.render::+grass-stock+)
                       (t luft.render::+subsoil-stock+)))))
           (legacy-chamfer-stock (stocks)
             (flet ((stone-p (stock)
                      (member stock
                              (list luft.render::+stone-stock+
                                    luft.render::+foundation-stone-stock+))))
               (cond ((and (some #'stone-p stocks)
                           (member luft.render::+subsoil-stock+ stocks))
                      luft.render::+deep-set-stone-stock+)
                     ((and (some #'stone-p stocks)
                           (member luft.render::+soil-stock+ stocks))
                      luft.render::+soil-set-stone-stock+)
                     ((and (some #'stone-p stocks)
                           (member luft.render::+grass-stock+ stocks))
                      luft.render::+turf-set-stone-stock+)
                     ((every (lambda (stock) (= stock (first stocks)))
                             (rest stocks))
                      (first stocks))
                     ((some #'stone-p stocks) luft.render::+stone-stock+)
                     ((and (member luft.render::+grass-stock+ stocks)
                           (some (lambda (stock)
                                   (<= luft.render::+soil-stock+ stock
                                       luft.render::+subsoil-stock+))
                                 stocks))
                      luft.render::+turf-edge-stock+)
                     (t luft.render::+soil-stock+))))
           (same-mesh-p (left right)
             (every #'identity
                    (list
                     (equalp (luft:surface-mesh-template-vertex-words left)
                             (luft:surface-mesh-template-vertex-words right))
                     (equalp (luft:surface-mesh-template-ranges left)
                             (luft:surface-mesh-template-ranges right))
                     (equalp (luft:surface-mesh-face-instance-words left)
                             (luft:surface-mesh-face-instance-words right))
                     (equalp (luft:surface-mesh-band-instance-words left)
                             (luft:surface-mesh-band-instance-words right))
                     (equalp (luft:surface-mesh-fan-instance-words left)
                             (luft:surface-mesh-fan-instance-words right))))))
    (dolist (scene (list (render:make-mountain-sanctuary-scene
                          :beacon-placement
                          luft.render::*sanctuary-material-placement*)
                         (render:make-miter-study-scene)))
      (ok (same-mesh-p
           (render:make-render-mesh scene)
           (render:make-render-mesh
            scene :stock-function (lambda (face) (legacy-face-stock scene face))
                  :chamfer-stock-function #'legacy-chamfer-stock))))))

(deftest material-semantics-compile-once-before-dense-meshing
  (let ((luft.render::*material-placement-compilation-count* 0))
    (let* ((scene (render:make-mountain-sanctuary-scene))
           (compilations
             luft.render::*material-placement-compilation-count*)
           (placement-count
             (length
              (luv.domains:identity-vocabulary-members
               (luft.render::scene-material-vocabulary scene))))
           (program (luft.render::scene-material-program scene)))
      (ok (= placement-count compilations))
      (ok (typep
           (luft.render::material-program-placement-face-stocks program)
           '(simple-array (unsigned-byte 16) (*))))
      (ok (typep (luft.render::material-program-placement-flags program)
                 '(simple-array (unsigned-byte 8) (*))))
      (render:make-render-mesh scene)
      (ok (= compilations
             luft.render::*material-placement-compilation-count*)))))

(deftest compiled-placement-local-materials-match-the-semantic-oracle
  (labels ((same-mesh-p (left right)
             (every #'equalp
                    (list (luft:surface-mesh-template-vertex-words left)
                          (luft:surface-mesh-template-ranges left)
                          (luft:surface-mesh-face-instance-words left)
                          (luft:surface-mesh-band-instance-words left)
                          (luft:surface-mesh-fan-instance-words left))
                    (list (luft:surface-mesh-template-vertex-words right)
                          (luft:surface-mesh-template-ranges right)
                          (luft:surface-mesh-face-instance-words right)
                          (luft:surface-mesh-band-instance-words right)
                          (luft:surface-mesh-fan-instance-words right)))))
    (let ((scene (render:make-mountain-sanctuary-scene)))
      (ok
       (same-mesh-p
        (render:make-render-mesh scene)
        (render:make-render-mesh
         scene
         :stock-function
         (lambda (face)
           (luft.render::surface-assembly-offset
            (luft.render::face-reading-assembly
             (luft.render::scene-face-reading scene face))))
         :chamfer-stock-function
         (lambda (stocks)
           (luft.render::surface-assembly-offset
            (luft.render::closure-surface-assembly
             (mapcar #'luft.render::surface-assembly-at stocks))))))))))

(deftest compiled-contacts-retain-both-authored-placement-frames
  (let* ((earth-frame
           (make-instance 'luft.render::material-frame
                          :name :test-earth :origin '(4 4 2)
                          :axes '((1 0 0) (0 1 0) (0 0 1))))
         (stone-frame
           (make-instance 'luft.render::material-frame
                          :name :test-stone :origin '(5 5 3)
                          :axes '((0 1 0) (-1 0 0) (0 0 1))))
         (earth
           (make-instance 'luft.render::material-placement
                          :name :test-earth :kind luft.render::*earth-material*
                          :finish :living :frame earth-frame :role :terrain))
         (stone
           (make-instance
            'luft.render::material-placement
            :name :test-stone :kind luft.render::*limestone-material*
            :finish :dressed :frame stone-frame :role :architecture))
         (builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene
           (progn
             (luft.render::scene-builder-box
              builder 4 6 4 6 2 2 :material earth)
             (luft.render::scene-builder-cell
              builder 5 5 3 :material stone)
             (luft.render::finish-scene-builder builder)))
         (compiled (render:make-render-mesh scene))
         (semantic
           (render:make-render-mesh
            scene
            :stock-function
            (lambda (face)
              (luft.render::surface-assembly-offset
               (luft.render::face-reading-assembly
                (luft.render::scene-face-reading scene face))))
            :chamfer-stock-function
            (lambda (stocks)
              (luft.render::surface-assembly-offset
               (luft.render::closure-surface-assembly
                (mapcar #'luft.render::surface-assembly-at stocks)))))))
    (ok
     (every #'equalp
            (list (luft:surface-mesh-face-instance-words compiled)
                  (luft:surface-mesh-band-instance-words compiled)
                  (luft:surface-mesh-fan-instance-words compiled))
            (list (luft:surface-mesh-face-instance-words semantic)
                  (luft:surface-mesh-band-instance-words semantic)
                  (luft:surface-mesh-fan-instance-words semantic))))
    (ok
     (find-if
      (lambda (assembly)
        (and (eq :contact
                 (luft.render::surface-assembly-relation assembly))
             (eq stone-frame
                 (luft.render::surface-reading-frame
                  (luft.render::surface-assembly-primary assembly)))
             (eq earth-frame
                 (luft.render::surface-reading-frame
                  (luft.render::surface-assembly-secondary assembly)))))
      (luv.domains:identity-vocabulary-members
       luft.render::*surface-assembly-vocabulary*)))))

(deftest surface-assembly-descriptors-compile-semantic-material-data
  (let ((words (luft.render::surface-assembly-descriptor-words)))
    (ok (<= (* 9 luft.render::+surface-assembly-descriptor-row-count+ 4)
            (length words)))
    (ok (equalp #(0.18 0.31 0.105 7.0) (subseq words 0 4)))
    (let ((contact (* luft.render::+turf-set-stone-stock+
                      luft.render::+surface-assembly-descriptor-row-count+ 4)))
      (ok (equalp #(0.53 0.49 0.39 1.0)
                  (subseq words contact (+ contact 4))))
      (ok (equalp #(0.18 0.31 0.105 0.0)
                  (subseq words (+ contact 4) (+ contact 8)))))))

(deftest authored-placement-frames-compile-to-distinct-dense-assemblies
  (let* ((scene (render:make-mountain-sanctuary-scene))
         (domain (luft:chain-domain (luft.render::scene-solid scene)))
         (x (+ luft.render::+sanctuary-origin-x+
               luft.render::*sanctuary-beacon-x*))
         (y (+ luft.render::+sanctuary-origin-y+
               luft.render::*sanctuary-beacon-y*))
         (z (+ 8 (luft.render::mountain-sanctuary-terrain-height
                  luft.render::*sanctuary-beacon-x*
                  luft.render::*sanctuary-beacon-y*)))
         (cell (luft:make-site domain x y z luft:+cell-extent+ 1)))
    (ok (eq luft.render::*beacon-material-placement*
            (luft.render::scene-material-placement-at scene cell)))
    (render:make-render-mesh scene)
    (let* ((assembly
             (find luft.render::*beacon-material-frame*
                   (luv.domains:identity-vocabulary-members
                    luft.render::*surface-assembly-vocabulary*)
                   :key (lambda (candidate)
                          (luft.render::surface-reading-frame
                           (luft.render::surface-assembly-primary candidate)))))
           (offset (luft.render::surface-assembly-offset assembly))
           (row (* offset
                   luft.render::+surface-assembly-descriptor-row-count+ 4))
           (words (luft.render::surface-assembly-descriptor-words)))
      (ok assembly)
      (ok (equalp #(90.0 78.0 0.0 2.0)
                  (subseq words (+ row 12) (+ row 16))))
      (ok (equalp #(0.70710677 0.70710677 0.0 0.02)
                  (subseq words (+ row 16) (+ row 20)))))))

(deftest surface-assembly-ids-use-the-widened-instance-field
  (let* ((assembly-id #xabc)
         (mesh
           (render:make-render-mesh
            (render:make-miter-study-scene)
            :stock-function (lambda (face) (declare (ignore face)) assembly-id)
            :chamfer-stock-function
            (lambda (stocks) (declare (ignore stocks)) assembly-id))))
    (dolist (words (list (luft:surface-mesh-face-instance-words mesh)
                         (luft:surface-mesh-band-instance-words mesh)
                         (luft:surface-mesh-fan-instance-words mesh)))
      (ok (plusp (length words)))
      (ok (loop for offset from 3 below (length words) by 4
                always (= assembly-id
                          (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                               (aref words offset))))))
    (ok (handler-case
            (progn (luft.render::make-render-population (list mesh)) nil)
          (error () t)))))

(deftest player-gait-anchors-stance-feet-and-rises-over-support
  (let ((step-length 0.75)
        (leg-length 1.07737)
        (hip-height 1.01))
    (labels ((foot-sample (step-coordinate parity)
               (let* ((cycle (* 0.5 (- step-coordinate parity)))
                      (cycle-index (floor cycle))
                      (phase (- cycle cycle-index))
                      (swing-time
                        (min 1.0 (max 0.0 (* 2.0 (- phase 0.5)))))
                      (swing-weight
                        (* swing-time swing-time swing-time
                           (+ 10.0
                              (* swing-time
                                 (+ -15.0 (* 6.0 swing-time)))))))
                 (values
                  (* step-length
                     (+ parity (* 2.0 cycle-index) 0.5
                        (* 2.0 swing-weight)))
                  (* 0.19 (sin (* pi swing-time))))))
             (pelvis-height (step-coordinate)
               (let* ((phase (- step-coordinate (floor step-coordinate)))
                      (offset (* step-length (- 0.5 phase))))
                 (sqrt (- (* leg-length leg-length) (* offset offset))))))
      ;; Each alternating stance interval holds one foot at one exact world
      ;; coordinate while the root advances by a complete half-step.
      (multiple-value-bind (left-a left-a-lift) (foot-sample 0.1 0.0)
        (multiple-value-bind (left-b left-b-lift) (foot-sample 0.9 0.0)
          (ok (= left-a left-b (* step-length 0.5)))
          (ok (zerop left-a-lift))
          (ok (zerop left-b-lift))))
      (multiple-value-bind (right-a right-a-lift) (foot-sample 1.1 1.0)
        (multiple-value-bind (right-b right-b-lift) (foot-sample 1.9 1.0)
          (ok (= right-a right-b (* step-length 1.5)))
          (ok (zerop right-a-lift))
          (ok (zerop right-b-lift))))
      ;; The other foot clears the deck during transfer and lands at zero
      ;; height; fourteen half-steps span the bridge's 10.5-cell half-route.
      (multiple-value-bind (mid-swing mid-lift) (foot-sample 0.5 1.0)
        (declare (ignore mid-swing))
        (ok (> mid-lift 0.18)))
      (ok (= 10.5 (* 14 step-length)))
      ;; A fixed leg is shortest at double support and tallest over the
      ;; planted foot at mid-stance, giving the body its non-arbitrary bob.
      (ok (= (pelvis-height 0.0) (pelvis-height 1.0)))
      (ok (> (pelvis-height 0.5) (pelvis-height 0.0)))
      ;; The height equation now uses the same hip and ankle centres as the
      ;; rendered SDF.  Its stance-leg reach is constant at the endpoints and
      ;; over the support contact, rather than only looking approximately so.
      (let* ((half-step (* step-length 0.5))
             (contact-height (pelvis-height 0.0))
             (mid-height (pelvis-height 0.5))
             (stance-reach
               (sqrt (+ (* contact-height contact-height)
                        (* half-step half-step)))))
        (ok (< (abs (- contact-height hip-height)) 1e-4))
        (ok (< (abs (- stance-reach leg-length)) 1e-6))
        (ok (< (abs (- mid-height leg-length)) 1e-6))))))

(deftest player-motion-channels-preserve-key-poses-and-foot-rockers
  (labels ((ease (amount)
             (let ((time (min 1.0 (max 0.0 amount))))
               (* time time time
                  (+ 10.0 (* time (+ -15.0 (* 6.0 time)))))))
           (segment (phase beginning end beginning-value end-value)
             (+ beginning-value
                (* (- end-value beginning-value)
                   (ease (/ (- phase beginning) (- end beginning))))))
           (channel (phase contact down passing up next-contact)
             (cond ((< phase 0.16)
                    (segment phase 0.0 0.16 contact down))
                   ((< phase 0.50)
                    (segment phase 0.16 0.50 down passing))
                   ((< phase 0.72)
                    (segment phase 0.50 0.72 passing up))
                   (t
                    (segment phase 0.72 1.0 up next-contact))))
           (rocker (cycle-phase)
             (if (< cycle-phase 0.5)
                 (let ((stance-time (* cycle-phase 2.0)))
                   (cond ((< stance-time 0.18)
                          (segment stance-time 0.0 0.18 0.17 0.0))
                         ((< stance-time 0.72) 0.0)
                         (t (segment stance-time 0.72 1.0 0.0 -0.30))))
                 (let ((swing-time (* (- cycle-phase 0.5) 2.0)))
                   (cond ((< swing-time 0.32)
                          (segment swing-time 0.0 0.32 -0.30 0.10))
                         ((< swing-time 0.78) 0.10)
                         (t (segment swing-time 0.78 1.0 0.10 0.17)))))))
    ;; Authored values survive exactly at semantic pose boundaries.
    (ok (= 0.20 (channel 0.0 0.20 -0.10 0.30 0.40 0.50)))
    (ok (= -0.10 (channel 0.16 0.20 -0.10 0.30 0.40 0.50)))
    (ok (= 0.30 (channel 0.50 0.20 -0.10 0.30 0.40 0.50)))
    (ok (= 0.40 (channel 0.72 0.20 -0.10 0.30 0.40 0.50)))
    (ok (= 0.50 (channel 1.0 0.20 -0.10 0.30 0.40 0.50)))
    ;; The planted boot accepts weight from heel to flat, stays flat through
    ;; the ankle rocker, then rolls over its toe.  Swing dorsiflexion clears
    ;; the ground and returns continuously to the next heel contact.
    (ok (> (rocker 0.0) 0.16))
    (ok (zerop (rocker 0.15)))
    (ok (< (rocker 0.48) -0.25))
    (ok (< (abs (- (rocker 0.49999) (rocker 0.5))) 1e-4))
    (ok (> (rocker 0.70) 0.09))
    (ok (< (abs (- (rocker 0.99999) (rocker 0.0))) 1e-4))))

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

(deftest terrain-chamfers-distinguish-the-living-top-edge
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-cell builder 4 4 4)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene)))
    (flet ((instance-stocks (words)
             (loop for offset from 3 below (length words) by 4
                   collect (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                                (aref words offset)))))
      (let ((stocks
              (mapcan #'instance-stocks
                      (list (luft:surface-mesh-band-instance-words mesh)
                            (luft:surface-mesh-fan-instance-words mesh)))))
        (ok (plusp (length stocks)))
        (ok (member luft.render::+turf-edge-stock+ stocks))
        (ok (member luft.render::+soil-stock+ stocks))))))

(deftest flat-terrain-closures-retain-a-living-edge-reading
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-box builder 4 5 4 5 4 4)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene)))
    (flet ((contains-turf-edge-p (words)
             (loop for offset from 3 below (length words) by 4
                   thereis (= luft.render::+turf-edge-stock+
                              (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                                   (aref words offset))))))
      (ok (contains-turf-edge-p
           (luft:surface-mesh-band-instance-words mesh)))
      (ok (contains-turf-edge-p
           (luft:surface-mesh-fan-instance-words mesh))))))

(deftest miter-study-chamfers-do-not-use-the-terrain-top-stock
  (let ((mesh (render:make-render-mesh (render:make-miter-study-scene))))
    (dolist (words (list (luft:surface-mesh-band-instance-words mesh)
                         (luft:surface-mesh-fan-instance-words mesh)))
      (ok (notany (lambda (stock) (zerop stock))
                  (loop for offset from 3 below (length words) by 4
                        collect (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                                     (aref words offset))))))))

(deftest stone-terrain-chamfers-have-an-earth-set-reading
  (ok (= luft.render::+stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+stone-stock+))))
  (ok (= luft.render::+turf-set-stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+stone-stock+ luft.render::+grass-stock+))))
  (ok (= luft.render::+soil-set-stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+soil-stock+ luft.render::+stone-stock+))))
  (ok (= luft.render::+deep-set-stone-stock+
         (luft.render::scene-chamfer-stock
          (list luft.render::+stone-stock+ luft.render::+grass-stock+
                luft.render::+subsoil-stock+))))
  (ok (= luft.render::+turf-edge-stock+
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
                   collect (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                                (aref words offset)))))
      (ok (notany (lambda (stock)
                    (member stock
                            (list luft.render::+turf-set-stone-stock+
                                  luft.render::+soil-set-stone-stock+
                                  luft.render::+deep-set-stone-stock+)))
                  (stocks (luft:surface-mesh-face-instance-words mesh))))
      (ok (some (lambda (stock)
                  (member stock
                          (list luft.render::+turf-set-stone-stock+
                                luft.render::+soil-set-stone-stock+
                                luft.render::+deep-set-stone-stock+)))
                (append
                 (stocks (luft:surface-mesh-band-instance-words mesh))
                 (stocks (luft:surface-mesh-fan-instance-words mesh))))))))

(deftest terrain-borne-architecture-marks-only-its-lowest-face-course
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (progn
                  (luft.render::scene-builder-box builder 4 6 4 6 2 2)
                  (luft.render::scene-builder-box
                   builder 5 5 5 5 3 4 :architecture-p t)
                  (luft.render::finish-scene-builder builder)))
         (mesh (render:make-render-mesh scene))
         (face-stocks
           (loop with words = (luft:surface-mesh-face-instance-words mesh)
                 for offset from 3 below (length words) by 4
                 collect (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                              (aref words offset)))))
    (ok (member luft.render::+foundation-stone-stock+ face-stocks))
    (ok (member luft.render::+stone-stock+ face-stocks))
    (ok (notany (lambda (stock)
                  (<= luft.render::+turf-set-stone-stock+ stock
                      luft.render::+deep-set-stone-stock+))
                face-stocks))))

(deftest directional-star-ambient-occlusion-measures-the-outward-hemisphere
  (ok (= 0 (luft::%directional-star-ambient-occlusion #b00000000 '(0 0 1))))
  (ok (= 1 (luft::%directional-star-ambient-occlusion #b00010000 '(0 0 1))))
  (ok (= 3 (luft::%directional-star-ambient-occlusion #b11110000 '(0 0 1))))
  (ok (= 3 (luft::%directional-star-ambient-occlusion #b10001000 '(1 1 0)))))

(deftest topology-ao-is-confined-to-bevels-and-junctions
  (let ((mesh (render:make-render-mesh (render:make-miter-study-scene))))
    (flet ((levels (words)
             (loop for offset from 3 below (length words) by 4
                   collect (ldb (byte 2 28) (aref words offset)))))
      (ok (every #'zerop (levels (luft:surface-mesh-face-instance-words mesh))))
      (ok (some #'plusp
                (append (levels (luft:surface-mesh-band-instance-words mesh))
                        (levels (luft:surface-mesh-fan-instance-words mesh))))))))

(deftest mesh-and-presentation-shaders-lower-through-both-conventional-backends
  (let* ((vertex (luft.render.shaders:mesh-vertex-specification))
         (fragment (luft.render.shaders:mesh-fragment-specification))
         (shadow-vertex
           (luft.render.shaders:shadow-vertex-specification))
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
    (ok (search "const device float4* material_descriptors" fragment-msl))
    (ok (search "depth2d<float> shadow_map" fragment-msl))
    (ok (search "sampler shadow_sampler" fragment-msl))
    (ok (search "barycentric" fragment-msl))
    (ok (search "motion_output" fragment-msl))
    (ok (search "depth2d<float> scene_depth" present-fragment-msl))
    (ok (search "highlight_energy" present-fragment-msl))
    (ok (search "paper_grade" present-fragment-msl))
    (ok (search "[[instance_id]]"
                (luv.msl:msl-document-source
                 (luv.msl:compile-msl lattice-vertex))))
    (ok (luv.msl:compile-msl lattice-fragment))
    (ok (luv.spir-v:compile-shader-specification vertex))
    (ok (luv.spir-v:compile-shader-specification fragment))
    (ok (luv.msl:compile-msl shadow-vertex))
    (ok (luv.spir-v:compile-shader-specification shadow-vertex))
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
            (eighth
              (let ((render:*projection* :isometric))
                (let ((view
                        (luft.render::capture-frame-view
                         camera 1100 800 #(0.0 0.0))))
                  (luft.render::camera-uniform-data
                   view view #(0.5 0.5 0.001 0.001) 1.0 7.25 1)))))
        (ok (= 92 (length perspective)))
        (ok (typep perspective '(simple-array single-float (92))))
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
        (ok (= (aref perspective 20) 0.25))
        (ok (= (aref eighth 20) 0.125))
        (ok (= (aref perspective 21) render:*wireframe*))
        (ok (equalp #(0.5 0.5 0.001 0.001)
                    (subseq perspective 48 52)))
        (ok (equalp #(61.5 48.5 15.48 7.25)
                    (subseq perspective 52 56)))
        (ok (equalp (luft.render::light-sun-color luft.render:*light*)
                    (subseq perspective 60 64)))
        (ok (equalp (luft.render::light-sky-color luft.render:*light*)
                    (subseq perspective 64 68)))
        (ok (equalp (luft.render::light-ground-color luft.render:*light*)
                    (subseq perspective 68 72)))
        (ok (= (/ luft.render::+shadow-map-size+)
               (aref perspective 88)))
        (ok (= (luft.render::light-shadow-filter-radius luft.render:*light*)
               (aref perspective 91)))))))

(deftest the-light-frame-is-texel-stable-under-subtexel-camera-motion
  (let* ((light luft.render:*light*)
         (center (luv.arithmetic.lisp.vec3:make-vec3 31.0 47.0 13.0))
         (rows (luft.render::light-shadow-rows light center))
         (texel (/ (* 2.0 (luft.render::light-shadow-half-extent light))
                   luft.render::+shadow-map-size+))
         (nearby
           (luv.arithmetic.lisp.vec3:make-vec3
            (+ (luv.arithmetic.lisp.vec3:vec3-x center) (* texel 0.1))
            (luv.arithmetic.lisp.vec3:vec3-y center)
            (luv.arithmetic.lisp.vec3:vec3-z center)))
         (nearby-rows (luft.render::light-shadow-rows light nearby)))
    (ok (= 16 (length rows)))
    ;; Snapping is in the light plane: a tiny arbitrary world translation may
    ;; cross no light-space texel boundary, and therefore leaves X/Y rows exact.
    (ok (equalp (subseq rows 0 8) (subseq nearby-rows 0 8)))
    (ok (= 36 (length (luft.render::light-uniform-data light center))))))

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
