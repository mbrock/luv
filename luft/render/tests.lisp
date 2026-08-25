(defpackage #:luft.render.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:clim #:clim)
                    (#:climi #:clim-internals)
                    (#:luv #:luv)
                    (#:production #:luv.production)
                    (#:render #:luft.render)))

(in-package #:luft.render.tests)

(defclass flame-resource-probe-device (luv:gpu-device)
  ((events :initform nil :accessor flame-resource-probe-events)
   (fail-bind-group-p :initform nil
                      :accessor flame-resource-probe-fail-bind-group-p)))

(defclass flame-resource-probe ()
  ((kind :initarg :kind :reader flame-resource-probe-kind)
   (device :initarg :device :reader flame-resource-probe-device)
   (data :initform nil :accessor flame-resource-probe-data)))

(defmethod luv:create
    ((device flame-resource-probe-device) (descriptor luv::buffer-descriptor))
  (let ((resource
          (make-instance 'flame-resource-probe
                         :kind :buffer :device device)))
    (push (list :create-buffer (luv::buffer-descriptor-size descriptor))
          (flame-resource-probe-events device))
    resource))

(defmethod luv:create
    ((device flame-resource-probe-device)
     (descriptor luv::bind-group-descriptor))
  (when (flame-resource-probe-fail-bind-group-p device)
    (error "Injected flame bind-group construction failure."))
  (let ((resource
          (make-instance 'flame-resource-probe
                         :kind :bind-group :device device)))
    (push (list :create-bind-group
                (length (luv::bind-group-descriptor-entries descriptor)))
          (flame-resource-probe-events device))
    resource))

(defmethod luv:write-buffer
    ((resource flame-resource-probe) data &key (offset 0))
  (setf (flame-resource-probe-data resource) (copy-seq data))
  (push (list :write offset (length data))
        (flame-resource-probe-events
         (flame-resource-probe-device resource)))
  resource)

(defmethod luv:destroy ((resource flame-resource-probe))
  (push (list :destroy (flame-resource-probe-kind resource))
        (flame-resource-probe-events
         (flame-resource-probe-device resource)))
  (values))

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
             (luft.render::prepared-render-mesh-mesh
              (production:perform-production-request request))))
      (ok (luft.render::current-streaming-mesh-request-p scene request))
      (setf (gethash right (luft.render::streaming-scene-loaded scene)) t)
      (ok (not (luft.render::current-streaming-mesh-request-p scene request)))
      ;; The old request remains independently executable, while a current
      ;; oracle observes and closes the newly resident cross-chunk seam.
      (ok (equalp (luft:surface-mesh-face-instance-words before)
                  (luft:surface-mesh-face-instance-words
                   (luft.render::prepared-render-mesh-mesh
                    (production:perform-production-request request)))))
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

(deftest a-streaming-window-is-bounded-around-its-focus
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 8)))
    (dotimes (chunk-x 3)
      (dotimes (chunk-y 3)
        (luft.render::scene-builder-cell
         builder (* chunk-x luft:+chunk-size+)
         (* chunk-y luft:+chunk-size+) 0)))
    (let ((scene
            (render:make-streaming-scene
             (luft.render::finish-scene-builder builder)
             :residency-radius 1)))
      (ok (= 4 (length (luft.render::streaming-scene-keys-near scene 0 0))))
      (ok (= 9 (length (luft.render::streaming-scene-keys-near scene 1 1))))
      (ok (zerop (hash-table-count
                  (luft.render::streaming-scene-loaded scene)))))))

(deftest a-streaming-window-uses-detail-medial-and-merged-medial-rings
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 9)))
    (dotimes (chunk-x 7)
      (dotimes (chunk-y 7)
        (luft.render::scene-builder-cell
         builder (* chunk-x luft:+chunk-size+)
         (* chunk-y luft:+chunk-size+) 0)))
    (let* ((scene
             (render:make-streaming-scene
              (luft.render::finish-scene-builder builder)
              :residency-radius 3 :lod-radius 1 :merge-radius 2))
           (system
             (production:make-single-worker-production-system
              :name "LUFT LoD policy test")))
      (unwind-protect
           (progn
             (ok (luft.render::retarget-streaming-scene
                  scene system 2 192 192))
             (ok (= 49 (hash-table-count
                        (luft.render::streaming-scene-loaded scene))))
             (ok (= 9
                    (loop for width being the hash-values of
                          (luft.render::streaming-scene-loaded scene)
                          count (= width 2))))
             (ok (= 40
                    (loop for width being the hash-values of
                          (luft.render::streaming-scene-loaded scene)
                          count (= width 4))))
             (ok (= 24
                    (loop for merge-p being the hash-values of
                          (luft.render::streaming-scene-merged scene)
                          count merge-p)))
             (let* ((key (luft:chunk-key-at 192 192))
                    (snapshot
                      (luft.render::make-streaming-mesh-snapshot scene key 2))
                    (request
                      (make-instance 'luft.render::streaming-mesh-request
                                     :key key :snapshot snapshot)))
               (ok (luft.render::current-streaming-mesh-request-p
                    scene request))
               (setf (gethash key (luft.render::streaming-scene-loaded scene))
                     4)
               (ok (not (luft.render::current-streaming-mesh-request-p
                         scene request)))))
        (production:stop-production-system system)))))

(deftest a-planar-chunk-merges-a-four-by-four-slab-into-six-rectangles
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 6)))
    (loop for y from 4 below 8 do
      (loop for x from 4 below 8 do
        (luft.render::scene-builder-cell builder x y 1)))
    (let* ((scene (luft.render::finish-scene-builder builder))
           (key (luft:chunk-key-at 4 4))
           (chunks (make-hash-table :test #'eql)))
      (luft:map-chain-chunks
       (lambda (chunk-key chain)
         (setf (gethash chunk-key chunks) chain))
       (luft.render::scene-solid scene))
      (handler-bind
          ((luft:missing-chunk
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air)))
           (luft:outside-domain
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air))))
        (let ((mesh
                (luft:mesh-chunk
                 (gethash key chunks) key :planar-merge-p t)))
          (ok (= 6 (luft:surface-mesh-face-instance-count mesh)))
          (ok (= 12 (luft:surface-mesh-triangle-count mesh)))
          (ok (zerop (luft:surface-mesh-band-instance-count mesh)))
          (ok (zerop (luft:surface-mesh-fan-instance-count mesh)))
          (ok (luft::%mesh-closed-p mesh)))))))

(deftest a-planar-chunk-keeps-a-tall-cliff-in-one-rectangle
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 6)))
    (dotimes (z 64)
      (luft.render::scene-builder-cell builder 4 4 z))
    (let* ((scene (luft.render::finish-scene-builder builder))
           (key (luft:chunk-key-at 4 4))
           (chunk nil))
      (luft:map-chain-chunks
       (lambda (chunk-key chain)
         (when (= chunk-key key) (setf chunk chain)))
       (luft.render::scene-solid scene))
      (handler-bind
          ((luft:missing-chunk
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air)))
           (luft:outside-domain
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air))))
        (let ((mesh (luft:mesh-chunk chunk key :planar-merge-p t)))
          (ok (= 6 (luft:surface-mesh-face-instance-count mesh)))
          (ok (= 12 (luft:surface-mesh-triangle-count mesh)))
          (ok (find (+ luft:+mesh-template-coordinate-bias+ 512)
                    (luft:surface-mesh-template-vertex-words mesh)))
          (ok (luft::%mesh-closed-p mesh)))))))

(deftest coplanar-compression-keeps-the-medial-chunk-surface-exact
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 6)))
    (loop for x from 4 below 12 do
      (loop for y from 4 below 12 do
        (dotimes (z (+ 2 (floor (+ x y) 3)))
          (luft.render::scene-builder-cell builder x y z))))
    (let* ((scene (luft.render::finish-scene-builder builder))
           (key (luft:chunk-key-at 4 4))
           (chunk nil))
      (luft:map-chain-chunks
       (lambda (chunk-key chain)
         (when (= chunk-key key) (setf chunk chain)))
       (luft.render::scene-solid scene))
      (handler-bind
          ((luft:missing-chunk
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air)))
           (luft:outside-domain
             (lambda (condition)
               (declare (ignore condition))
               (invoke-restart 'luft:treat-as-air))))
        (let ((medial (luft:mesh-chunk chunk key :bevel-width 4))
              (merged (luft:mesh-chunk chunk key :bevel-width 4
                                                 :coplanar-merge-p t)))
          (ok (< (luft:surface-mesh-triangle-count merged)
                 (luft:surface-mesh-triangle-count medial)))
          (ok (luft::%mesh-closed-p merged))
          (ok (luft::%same-plane-areas-p
               (luft::%mesh-oriented-plane-areas medial)
               (luft::%mesh-oriented-plane-areas merged))))))))

(deftest a-pure-lod-shift-remeshes-only-chunks-whose-tier-changed
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 8)))
    (dotimes (chunk-x 4)
      (dotimes (chunk-y 4)
        (luft.render::scene-builder-cell
         builder (* chunk-x luft:+chunk-size+)
         (* chunk-y luft:+chunk-size+) 0)))
    (let* ((scene
             (render:make-streaming-scene
              (luft.render::finish-scene-builder builder)
              :residency-radius 3 :lod-radius 1))
           (system
             (production:make-single-worker-production-system
              :name "LUFT LoD shift test")))
      (unwind-protect
           (progn
             (ok (luft.render::retarget-streaming-scene
                  scene system 2 64 64))
             ;; Model completion of the initial cohort; tickets already in the
             ;; worker are harmless because the second scheduling supersedes
             ;; every changed key with a newer ticket.
             (setf (luft.render::streaming-scene-cohort scene) nil
                   (luft.render::streaming-scene-removals scene) nil)
             (clrhash (luft.render::streaming-scene-outstanding scene))
             (ok (luft.render::retarget-streaming-scene
                  scene system 2 128 64))
             (ok (= 6 (length
                       (luft.render::streaming-scene-cohort scene))))
             (ok (= 9
                    (loop for width being the hash-values of
                          (luft.render::streaming-scene-loaded scene)
                          count (= width 2))))
             (ok (= 7
                    (loop for width being the hash-values of
                          (luft.render::streaming-scene-loaded scene)
                          count (= width 4)))))
        (production:stop-production-system system)))))

(deftest highland-landscape-is-deterministic-and-regionally-varied
  (let* ((size 256)
         (heights
           (loop for x below size by 8 append
             (loop for y below size by 8
                   collect
                   (luft.render::highland-landscape-height
                    x y size :seed 121))))
         (again
           (loop for x below size by 8 append
             (loop for y below size by 8
                   collect
                   (luft.render::highland-landscape-height
                    x y size :seed 121)))))
    (ok (equal heights again))
    (ok (>= (- (reduce #'max heights) (reduce #'min heights)) 24))
    (ok (>= (length (remove-duplicates heights)) 20))
    (ok (not (equal heights
                    (loop for x below size by 8 append
                      (loop for y below size by 8
                            collect
                            (luft.render::highland-landscape-height
                             x y size :seed 913))))))))

(deftest highland-landscape-streams-by-default
  (let ((scene
          (render:make-highland-sanctuary-scene :horizontal-bits 6)))
    (ok (typep scene 'render:streaming-scene))
    (ok (= 3 (luft.render::streaming-scene-residency-radius scene)))
    (ok (= 1 (luft.render::streaming-scene-lod-radius scene)))
    (ok (= 2 (luft.render::streaming-scene-merge-radius scene)))
    (ok (= 1 (hash-table-count
              (luft.render::streaming-scene-store scene))))))

(deftest retargeting-replaces-one-complete-residency-window
  (let* ((scene (make-two-chunk-streaming-scene))
         (left (luft:chunk-key-at 63 4))
         (right (luft:chunk-key-at 64 4))
         (system
           (production:make-single-worker-production-system
            :name "LUFT retarget test")))
    (setf (luft.render::streaming-scene-residency-radius scene) 0
          (gethash left (luft.render::streaming-scene-loaded scene)) t)
    (unwind-protect
         (progn
           (ok (luft.render::retarget-streaming-scene
                scene system luft:+mesh-bevel-width+ 64 4))
           (ok (null (gethash left
                              (luft.render::streaming-scene-loaded scene))))
           (ok (gethash right (luft.render::streaming-scene-loaded scene)))
           (ok (equal (list right)
                      (luft.render::streaming-scene-cohort scene)))
           (ok (equal (list left)
                      (luft.render::streaming-scene-removals scene)))
           (ok (= 1 (hash-table-count
                     (luft.render::streaming-scene-outstanding scene)))))
      (production:stop-production-system system))))

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
    (ok (typep (render:viewer-mode viewer) 'render:isometric-walk-mode))
    (ok (equal '(luft.render::com-start-moving :forward)
               (luft.render::viewer-key-command viewer (key-press :w))))
    (setf (render:viewer-mode viewer) (make-instance 'render:orbit-mode))
    (ok (null (luft.render::viewer-key-command viewer (key-press :w))))
    (ok (equal '(luft.render::com-toggle-fullscreen)
               (luft.render::viewer-key-command
                viewer (key-press :f11))))
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
    (ok (equal '(luft.render::com-toggle-viewer-mode)
               (luft.render::viewer-key-command viewer (key-press :m))))
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

(deftest click-to-walk-routes-around-a-character-high-wall
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (player
           (render:make-walking-player
            :position (luv.arithmetic.lisp.vec3:make-vec3 1.5 2.5 1.0))))
    (luft.render::scene-builder-box builder 0 15 0 15 0 0)
    ;; The direct row is blocked.  The only short way around the north end
    ;; crosses Y=5, proving the click produced a route rather than a velocity.
    (loop for y from 0 to 4 do
      (loop for z from 1 to 4 do
        (luft.render::scene-builder-cell builder 3 y z)))
    (let* ((scene (luft.render::finish-scene-builder builder :player-p t))
           (route
             (render:start-walking-player-route player scene 5 2 1)))
      (ok (eq :running (render:walking-route-status route)))
      (ok (find 5 (render:walking-route-cells route)
                :key #'luft:site-y))
      (ok (> (length (render:walking-route-cells route)) 4))
      (ok (plusp (render:walking-route-visits route)))
      (loop repeat 1200
            while (eq :running (render:walking-route-status route))
            do (multiple-value-bind (forward right maximum-distance)
                   (luft.render::walking-player-route-control
                    player (render:make-fly-camera :yaw 0.0))
                 (luft.render::advance-walking-player
                  player scene (render:make-fly-camera :yaw 0.0)
                  (or forward 0.0) (or right 0.0) (/ 1.0 120.0)
                  :maximum-distance maximum-distance)
                 (luft.render::trim-walking-player-route player)))
      (ok (eq :arrived (render:walking-route-status route)))
      (ok (< (abs (- 5.5
                     (luv.arithmetic.lisp.vec3:vec3-x
                      (render:walking-player-position player))))
             0.12))
      (ok (< (abs (- 2.5
                     (luv.arithmetic.lisp.vec3:vec3-y
                      (render:walking-player-position player))))
             0.12)))))

(deftest click-to-walk-authors-straight-diagonal-waypoints
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (player
           (render:make-walking-player
            :position (luv.arithmetic.lisp.vec3:make-vec3 1.5 1.5 1.0))))
    (luft.render::scene-builder-box builder 0 15 0 15 0 0)
    (let* ((scene (luft.render::finish-scene-builder builder :player-p t))
           (route (render:start-walking-player-route player scene 6 6 1))
           (cells (render:walking-route-cells route)))
      (ok (= 5 (length cells)))
      (loop for cell in cells
            for coordinate from 2
            do (ok (= coordinate (luft:site-x cell) (luft:site-y cell)))))))

(deftest orthographic-walk-moves-on-the-ground-without-zooming
  (let* ((viewer (clim:make-application-frame 'render:viewer))
         (camera (render:viewer-camera viewer))
         (player (render:viewer-player viewer))
         (before-player-x
           (luv.arithmetic.lisp.vec3:vec3-x
            (render:walking-player-position player)))
         (before-player-y
           (luv.arithmetic.lisp.vec3:vec3-y
            (render:walking-player-position player)))
         (render:*projection* :isometric)
         (render:*isometric-height* 18.0))
    (luft.render::advance-viewer-camera viewer 1.0)
    (let ((before-camera-z
            (luv.arithmetic.lisp.vec3:vec3-z
             (render:camera-position camera))))
      (luft.render::set-viewer-control viewer :forward t)
      (luft.render::advance-viewer-camera viewer 1.1)
      (let ((after (render:walking-player-position player)))
        (ok (or (/= before-player-x
                    (luv.arithmetic.lisp.vec3:vec3-x after))
                (/= before-player-y
                    (luv.arithmetic.lisp.vec3:vec3-y after))))
        ;; A same-height walking step translates the following camera without
        ;; changing its orbit height.
        (ok (= before-camera-z
               (luv.arithmetic.lisp.vec3:vec3-z
                (render:camera-position camera))))
        (ok (= 18.0 render:*isometric-height*))))))

(deftest following-camera-ignores-small-relief-and-catches-large-jumps
  (let* ((camera (render:make-fly-camera :yaw 0.0 :pitch -0.5))
         (player (render:make-walking-player)))
    (luft.render::follow-walking-player camera player)
    (let* ((camera-position (render:camera-position camera))
           (settled-z (luv.arithmetic.lisp.vec3:vec3-z camera-position)))
      ;; A stair-sized vertical discrepancy belongs to the traveler, not the
      ;; composition, and remains inside the camera's quiet zone.
      (incf (luv.arithmetic.lisp.vec3:vec3-z
             (render:walking-player-position player))
            0.75)
      (luft.render::follow-walking-player camera player :seconds 0.1)
      (ok (= settled-z
             (luv.arithmetic.lisp.vec3:vec3-z camera-position)))
      ;; A fall or teleport is well outside that zone and closes most of its
      ;; error promptly instead of inheriting the gentle local response.
      (incf (luv.arithmetic.lisp.vec3:vec3-z
             (render:walking-player-position player))
            12.0)
      (let ((before-error
              (abs (- (- (+ (luv.arithmetic.lisp.vec3:vec3-z
                             (render:walking-player-position player))
                            1.45)
                         (* 18.0 (sin -0.5)))
                      (luv.arithmetic.lisp.vec3:vec3-z camera-position)))))
        (luft.render::follow-walking-player camera player :seconds 0.1)
        (let ((after-error
                (abs (- (- (+ (luv.arithmetic.lisp.vec3:vec3-z
                               (render:walking-player-position player))
                              1.45)
                           (* 18.0 (sin -0.5)))
                        (luv.arithmetic.lisp.vec3:vec3-z camera-position)))))
          (ok (< after-error (* 0.45 before-error))))))))

(deftest walking-player-climbs-one-step-but-not-a-character-high-wall
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (camera (render:make-fly-camera
                  :position
                  (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 0.0)
                  :yaw 0.0 :pitch -0.5))
         (player
           (render:make-walking-player
            :position (luv.arithmetic.lisp.vec3:make-vec3 2.5 2.5 1.0))))
    (luft.render::scene-builder-box builder 0 7 0 5 0 0)
    (luft.render::scene-builder-cell builder 3 2 1)
    (let ((scene (luft.render::finish-scene-builder builder :player-p t)))
      (luft.render::advance-walking-player player scene camera 1 0 0.2)
      (ok (= 2.0 (luv.arithmetic.lisp.vec3:vec3-z
                  (render:walking-player-position player))))
      (ok (> (luft.render::walking-player-gait player) 0.0))
      (loop repeat 60 do
        (luft.render::advance-walking-player
         player scene camera 0 0 (/ 1.0 60.0)))
      (let ((step-coordinate
              (/ (luft.render::walking-player-gait player) pi)))
        (ok (< (abs (- step-coordinate (round step-coordinate))) 1e-5)))
      ;; The next column is filled through the character's head.  Axis
      ;; separation leaves the player at the near edge instead of climbing or
      ;; teleporting to its remote roof.
      (loop for z from 1 to 4 do
        (luft.render::scene-builder-cell builder 4 2 z))
      (let* ((blocked-scene
               (luft.render::finish-scene-builder builder :player-p t))
             (before-x
               (luv.arithmetic.lisp.vec3:vec3-x
                (render:walking-player-position player))))
        (luft.render::advance-walking-player
         player blocked-scene camera 1 0 0.2)
        (ok (= before-x
               (luv.arithmetic.lisp.vec3:vec3-x
                (render:walking-player-position player))))))))

(deftest gameplay-treats-the-finite-world-domain-as-a-wall
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (camera
           (render:make-fly-camera
            :position (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 0.0)
            :yaw 0.0 :pitch -0.5))
         (player
           (render:make-walking-player
            :position
            (luv.arithmetic.lisp.vec3:make-vec3 15.5 8.5 1.0))))
    (luft.render::scene-builder-box builder 0 15 0 15 0 0)
    (let ((scene (luft.render::finish-scene-builder builder :player-p t)))
      (ok (= 1 (luft.render::collision-cell-occupancy-bit
                (luft.render::scene-solid scene) 16 8 1)))
      (let ((before
              (luv.arithmetic.lisp.vec3:vec3-x
               (render:walking-player-position player))))
        (luft.render::advance-walking-player player scene camera 1 0 0.2)
        (ok (= before
               (luv.arithmetic.lisp.vec3:vec3-x
                (render:walking-player-position player))))
        (setf (luft.render::walking-player-grounded-p player) nil)
        (ok (null (luft.render::try-walking-player-air-axis
                   player scene :x 1.0)))
        (ok (= before
               (luv.arithmetic.lisp.vec3:vec3-x
                (render:walking-player-position player))))))))

(deftest click-thrown-ball-has-motion-and-render-state
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (luft.render::finish-scene-builder builder :player-p t))
         (player (render:make-walking-player))
         (origin (luv.arithmetic.lisp.vec3:make-vec3 2.0 2.0 4.0))
         (direction (luv.arithmetic.lisp.vec3:make-vec3 1.0 0.0 0.0)))
    (luft.render::throw-walking-player-ball player origin direction)
    (ok (luvcraft:physics-body-alive-p
         (luft.render::walking-player-physics player)
         (luft.render::walking-player-ball-handle player)))
    (let ((before (luv.arithmetic.lisp.vec3:vec3-x
                   (luft.render::walking-player-ball-position player))))
      (luft.render::advance-walking-player-ball player scene 0.1)
      (ok (plusp (luvcraft:physics-world-step-count
                  (luft.render::walking-player-physics player))))
      (ok (> (luv.arithmetic.lisp.vec3:vec3-x
              (luft.render::walking-player-ball-position player)) before)))
    (multiple-value-bind (character previous direction-lane ball previous-ball)
        (luft.render::walking-player-render-lanes player)
      (declare (ignore character previous direction-lane previous-ball))
      (ok (= luft.render::+thrown-ball-radius+ (fourth ball))))))

(deftest a-thrown-ball-bounces-off-the-finite-world-domain
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene (luft.render::finish-scene-builder builder :player-p t))
         (player (render:make-walking-player))
         (origin (luv.arithmetic.lisp.vec3:make-vec3 14.0 8.0 4.0))
         (direction (luv.arithmetic.lisp.vec3:make-vec3 1.0 0.0 0.0)))
    (luft.render::throw-walking-player-ball player origin direction)
    (loop repeat 12 do
      (luft.render::advance-walking-player-ball player scene (/ 1.0 60.0)))
    (let* ((physics (luft.render::walking-player-physics player))
           (handle (luft.render::walking-player-ball-handle player))
           (x (luv.arithmetic.lisp.vec3:vec3-x
               (luft.render::walking-player-ball-position player))))
      (multiple-value-bind (vx vy vz)
          (luvcraft:physics-body-velocity physics handle)
        (declare (ignore vy vz))
        (ok (< x 16.0))
        (ok (minusp vx))))))

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

(deftest the-elevated-sanctuary-rim-is-an-authored-stone-battlement
  (let ((scene (render:make-mountain-sanctuary-scene)))
    (multiple-value-bind (west east present-p)
        (luft.render::mountain-sanctuary-terrain-x-bounds 47)
      (declare (ignore west))
      (ok present-p)
      (let* ((height
               (luft.render::mountain-sanctuary-terrain-height east 47))
             (x (+ luft.render::+sanctuary-origin-x+ east))
             (y (+ luft.render::+sanctuary-origin-y+ 47))
             (solid (luft.render::scene-solid scene))
             (wall-cell
               (luft:make-site (luft:chain-domain solid) x y height
                               luft:+cell-extent+ 1)))
        (ok (>= height luft.render::+sanctuary-plateau-height+))
        ;; Two continuous courses stop the one-step walker; this even
        ;; contour column also carries the alternating crenellation.
        (ok (= 1 (luft:chain-cell-occupancy-bit solid x y height)))
        (ok (= 1 (luft:chain-cell-occupancy-bit solid x y (1+ height))))
        (ok (= 1 (luft:chain-cell-occupancy-bit solid x y (+ height 2))))
        (ok (zerop (luft:chain-cell-occupancy-bit solid (1+ x) y height)))
        (ok (eq luft.render::*sanctuary-material-placement*
                (luft.render::scene-material-placement-at scene wall-cell)))))
    ;; The low southern shore remains open rather than walling in the route.
    (multiple-value-bind (west east present-p)
        (luft.render::mountain-sanctuary-terrain-x-bounds -15)
      (declare (ignore west))
      (ok present-p)
      (let ((height
              (luft.render::mountain-sanctuary-terrain-height east -15)))
        (ok (< height luft.render::+sanctuary-plateau-height+))
        (ok (zerop
             (luft:chain-cell-occupancy-bit
              (luft.render::scene-solid scene)
              (+ luft.render::+sanctuary-origin-x+ east)
              (+ luft.render::+sanctuary-origin-y+ -15)
              height)))))))

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

(deftest crystal-optics-compile-as-independent-render-and-light-facts
  (let* ((words (luft.render::surface-assembly-descriptor-words))
         (stride (* luft.render::+surface-assembly-descriptor-row-count+ 4))
         (crystal (* (luft.render::surface-assembly-offset
                      luft.render::*crystal-surface*)
                     stride))
         (flame (* (luft.render::surface-assembly-offset
                    luft.render::*torch-flame-surface*)
                   stride))
         (vocabulary (luft.render::make-scene-material-vocabulary))
         (opacities
           (luft.render::compile-material-light-opacity-table vocabulary))
         (crystal-placement
           (luv.domains:identity-vocabulary-offset
            vocabulary luft.render::*crystal-material-placement*)))
    ;; Crystal-only secondary/tertiary tone lanes are a compact optical ABI.
    (ok (equalp #(1.62 0.48 0.58 0.42)
                (subseq words (+ crystal 4) (+ crystal 8))))
    (ok (equalp #(18.0 0.30 0.88 0.14)
                (subseq words (+ crystal 8) (+ crystal 12))))
    ;; Descriptor Y.w is visual opacity and Z.w is visible HDR emission.
    (ok (< (abs (- 0.48 (aref words (+ crystal 23)))) 1.0e-6))
    (ok (< (abs (- 0.30 (aref words (+ crystal 27)))) 1.0e-6))
    (ok (= 1.0 (aref words (+ flame 23))))
    (ok (< (abs (- 1.8 (aref words (+ flame 27)))) 1.0e-6))
    ;; Propagation is a separate CPU lane: crystal transmits with entered
    ;; opacity one while ordinary authored solids remain fully blocking.
    (ok (= 1 (aref opacities crystal-placement)))
    (ok (= 15 (aref opacities 0)))
    (ok (= (luft:pack-voxel-light 3 11 15)
           (luft.render::material-kind-packed-light-emission
            luft.render::*crystal-material*)))))

(deftest voxel-light-shrine-retains-semantic-torches-and-colored-sources
  (let* ((scene (render:make-voxel-light-shrine-scene))
         (solid (render:scene-solid scene))
         (domain (luft:chain-domain solid))
         (field (render:scene-voxel-light scene))
         (crystal (luft:make-site domain 12 13 5 luft:+cell-extent+ 1))
         (backing (luft:make-site domain 12 13 4 luft:+cell-extent+ 1))
         (crystal-light (luft:voxel-light-at-site field crystal)))
    (ok (= 4 (length (render:scene-torches scene))))
    ;; Collision and static rendering retain the occupied union; the semantic
    ;; phase chains still classify the crystal and its opaque backing apart.
    (ok (luft:chain-site-p (render:scene-solid scene) crystal))
    (ok (luft:chain-site-p (luft.render::scene-translucent-solid scene)
                           crystal))
    (ok (not (luft:chain-site-p (luft.render::scene-opaque-solid scene)
                                crystal)))
    (ok (luft:chain-site-p (luft.render::scene-opaque-solid scene) backing))
    (ok (not (luft:chain-site-p
              (luft.render::scene-translucent-solid scene) backing)))
    (ok (>= (luft:voxel-light-red crystal-light) 3))
    (ok (>= (luft:voxel-light-green crystal-light) 11))
    (ok (= (luft:voxel-light-blue crystal-light) 15))
    (ok (plusp (luft:voxel-light-field-visits field)))
    (loop for attachment across (render:scene-torches scene)
          for support = (luft.render::torch-attachment-support-cell attachment)
          for source = (luft.render::torch-attachment-source-cell attachment)
          for light = (luft:voxel-light-at-site field source)
          do (ok (luft:chain-site-p solid support))
             (ok (not (luft:chain-site-p solid source)))
             ;; Its own warm source survives componentwise joins with other
             ;; torches and the cyan crystals; neighboring sources may only
             ;; raise lanes, never replace or add them arithmetically.
             (ok (= 15 (luft:voxel-light-red light)))
             (ok (>= (luft:voxel-light-green light) 9))
             (ok (>= (luft:voxel-light-blue light) 3)))))

(deftest torch-flames-pack-six-orientations-and-replay-their-volume-integral
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 4)))
    (luft.render::scene-builder-cell builder 7 7 7 :architecture-p t)
    (dolist (axis '(:x :y :z))
      (dolist (side '(:low :high))
        (luft.render::scene-builder-torch builder 7 7 7 axis side)))
    (let* ((scene (luft.render::finish-scene-builder builder))
           (words (luft.render::scene-torch-flame-instance-words scene))
           (owned (luft.render::%copy-torch-flame-instance-words words))
           (codes
             (sort
              (loop for offset from 3 below (length words)
                      by render:+torch-flame-instance-word-count+
                    collect (aref words offset))
              #'<))
           (top-offset
             (loop for offset from 0 below (length words)
                     by render:+torch-flame-instance-word-count+
                   when (= 5 (aref words (+ offset 3))) return offset))
           (top (subseq words top-offset
                        (+ top-offset render:+torch-flame-instance-word-count+)))
           (clock (render:torch-flame-effect-uniform-data 4.25 4.0)))
      (ok (= 6 (/ (length words)
                  render:+torch-flame-instance-word-count+)))
      (ok (equal '(0 1 2 3 4 5) codes))
      (ok (not (eq words owned)))
      (ok (equalp words owned))
      (setf (aref owned 0) 0)
      (ok (/= (aref words 0) (aref owned 0)))
      (ok (equalp #(4.25 4.0 0.0 0.0) clock))
      (let ((density
              (render:torch-flame-reference-density
               top 7.5 7.5 8.62 1.25)))
        (ok (plusp density))
        (ok (= density
               (render:torch-flame-reference-density
                top 7.5 7.5 8.62 1.25))))
      (multiple-value-bind (red green blue alpha)
          (render:torch-flame-reference-integrate-ray
           top 7.5 7.5 7.83 0.0 0.0 1.0 1.25)
        (ok (> red green blue 0.0))
        (ok (< 0.0 alpha 1.0))))))

(deftest renderer-flame-population-replacement-is-owned-and-transactional
  (let* ((device (make-instance 'flame-resource-probe-device))
         (old-buffer
           (make-instance 'flame-resource-probe
                          :kind :old-buffer :device device))
         (old-group
           (make-instance 'flame-resource-probe
                          :kind :old-bind-group :device device))
         (camera
           (make-instance 'flame-resource-probe :kind :camera :device device))
         (effect
           (make-instance 'flame-resource-probe :kind :effect :device device))
         (renderer
           (make-instance
            'luft.render::renderer
            :device device :camera-buffer camera
            :flame-layout :flame-layout :flame-effect-buffer effect
            :flame-instance-buffer old-buffer :flame-bind-group old-group))
         (source
           (make-array 8 :element-type '(unsigned-byte 32)
                         :initial-contents '(56 60 60 0 64 60 60 1))))
    (luft.render::renderer-set-flame-instance-words renderer source)
    (let ((installed-buffer
            (luft.render::renderer-flame-instance-buffer renderer))
          (installed-group
            (luft.render::renderer-flame-bind-group renderer)))
      (ok (= 2 (luft.render::renderer-flame-instance-count renderer)))
      (ok (equalp source
                  (luft.render::renderer-flame-instance-words renderer)))
      (ok (equalp source (flame-resource-probe-data installed-buffer)))
      (setf (aref source 0) 0)
      (ok (= 56 (aref (luft.render::renderer-flame-instance-words renderer) 0)))
      (ok (member '(:destroy :old-bind-group)
                  (flame-resource-probe-events device) :test #'equal))
      (ok (member '(:destroy :old-buffer)
                  (flame-resource-probe-events device) :test #'equal))
      ;; Failure after candidate-buffer creation retires only that candidate;
      ;; the last complete population and bind group remain published.
      (setf (flame-resource-probe-fail-bind-group-p device) t)
      (ok (handler-case
              (progn
                (luft.render::renderer-set-flame-instance-words
                 renderer #(60 56 60 2))
                nil)
            (error () t)))
      (ok (eq installed-buffer
              (luft.render::renderer-flame-instance-buffer renderer)))
      (ok (eq installed-group
              (luft.render::renderer-flame-bind-group renderer)))
      (ok (= 2 (luft.render::renderer-flame-instance-count renderer)))
      (ok (member '(:destroy :buffer)
                  (flame-resource-probe-events device) :test #'equal))
      (ok (handler-case
              (progn
                (luft.render::%copy-torch-flame-instance-words #(1 2 3))
                nil)
            (error () t))))))

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

(defun mesh-open-edges (mesh)
  (let ((records (luft::%mesh-geometric-edge-records mesh)))
    (loop for edge being the hash-keys of records using (hash-value record)
          when (= 1 (car record)) collect edge)))

(defun make-centred-crystal-bezel-test-scene ()
  "One crystal protruding from the centre of a three-by-three stone support."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 4)))
    (luft.render::scene-builder-box
     builder 4 6 4 6 4 4 :architecture-p t)
    (luft.render::scene-builder-cell
     builder 5 5 5
     :material luft.render::*crystal-material-placement*)
    (luft.render::finish-scene-builder builder)))

(defun direct-material-bevel-union (scene profile)
  "Build the mixed union directly, without the static render entry point."
  (let* ((chamfer-stock-function
           (luft.render::make-compiled-material-chamfer-stock-function
            (luft.render::scene-material-program scene)))
         (witness
           (luft.render::%make-scene-phase-mesh
            scene (render:scene-solid scene) 1 nil chamfer-stock-function)))
    (multiple-value-bind (stock-masks site-widths)
        (luft.render::compile-material-bevel-site-policy profile)
      (luft:vary-surface-mesh-bevel-widths-from-stock-masks
       witness stock-masks site-widths))))

(deftest static-crystal-meshes-are-the-direct-union-at-widths-one-two-and-four
  (let ((scene (make-centred-crystal-bezel-test-scene)))
    (dolist (width '(1 2 4))
      (let* ((profile
               (render:make-material-bevel-profile
                :terrain-width 2 :architecture-width 2
                :crystal-width 4 :contact-width width))
             (material-mesh (render:make-material-bevel-mesh scene profile))
             (material-oracle (direct-material-bevel-union scene profile))
             (uniform-mesh
               (render:make-render-mesh scene :bevel-width width))
             (uniform-oracle
               (luft.render::%make-scene-phase-mesh
                scene (render:scene-solid scene) width nil
                (luft.render::make-compiled-material-chamfer-stock-function
                 (luft.render::scene-material-program scene)))))
        (ok (luft::%same-surface-mesh-representation-p
             material-oracle material-mesh))
        (ok (luft::%same-surface-mesh-representation-p
             uniform-oracle uniform-mesh))
        (ok (luft::%mesh-closed-p material-mesh))
        (ok (luft::%mesh-nondegenerate-p material-mesh))))))

(defun make-gallery-support-crystal-test-scene (position)
  "The reduced gallery plinth with one crystal on an edge or corner."
  (let ((builder (luft.render::make-scene-builder :horizontal-bits 5)))
    (luft.render::scene-builder-box
     builder 14 19 6 9 4 4 :architecture-p t)
    (destructuring-bind (x y z)
        (ecase position
          (:edge '(16 9 5))
          (:corner '(19 9 5)))
      (luft.render::scene-builder-cell
       builder x y z
       :material luft.render::*crystal-material-placement*))
    (luft.render::finish-scene-builder builder)))

(deftest gallery-support-edge-and-corner-crystals-build-as-one-closed-union
  (dolist (position '(:edge :corner))
    (let* ((scene (make-gallery-support-crystal-test-scene position))
           (mesh
             (render:make-material-bevel-mesh
              scene
              (render:make-material-bevel-profile
               :terrain-width 2 :architecture-width 2
               :crystal-width 4 :contact-width 2)))
           (population (luft.render::make-render-population (list mesh))))
      (ok (null (luft:surface-mesh-companions mesh)))
      (ok (luft::%mesh-closed-p mesh))
      (ok (luft::%mesh-nondegenerate-p mesh))
      (ok (plusp
           (luft.render::render-population-opaque-triangle-instance-count
            population)))
      (ok (plusp
           (luft.render::render-population-translucent-triangle-instance-count
            population))))))

(deftest contiguous-crystal-row-has-one-original-perimeter-without-cell-teeth
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene
           (progn
             (luft.render::scene-builder-box
              builder 3 7 3 5 3 3 :architecture-p t)
             (loop for x from 4 to 6
                   do (luft.render::scene-builder-cell
                       builder x 4 4
                       :material luft.render::*crystal-material-placement*))
             (luft.render::finish-scene-builder builder)))
         (mesh
           (render:make-material-bevel-mesh
            scene
            (render:make-material-bevel-profile
             :terrain-width 2 :architecture-width 2
             :crystal-width 4 :contact-width 2)))
         (crystal-stock
           (luft.render::surface-assembly-offset
            luft.render::*crystal-surface*))
         (crystal-exterior
           (luft:select-surface-mesh-stocks
            mesh (lambda (stock) (= stock crystal-stock))))
         (edges (mesh-open-edges crystal-exterior))
         (points
           (sort
            (remove-duplicates (mapcan #'copy-list edges) :test #'equal)
            #'luft::%point-order<)))
    ;; Canonical-site subdivisions remain along the long sides, but all points
    ;; lie on one chamfered outer collar; no host tooth rises between crystals.
    (ok
     (equal
      '((32 34 34) (32 38 34) (34 32 34) (34 40 34)
        (38 32 34) (38 40 34) (40 32 34) (40 40 34)
        (42 32 34) (42 40 34) (46 32 34) (46 40 34)
        (48 32 34) (48 40 34) (50 32 34) (50 40 34)
        (54 32 34) (54 40 34) (56 34 34) (56 38 34))
      points))
    (ok (= 20 (length edges)))
    (ok (luft::%mesh-closed-p mesh))))

(deftest shrine-population-keeps-light-parallel-and-translucency-separate
  (let* ((scene (render:make-voxel-light-shrine-scene))
         (mesh (render:make-material-bevel-mesh
                scene (render:make-material-bevel-profile)))
         (population (luft.render::make-render-population (list mesh)))
         (instances
           (+ (luft.render::render-population-triangle-instance-count
               population)
              (luft.render::render-population-quad-instance-count
               population)))
         (companions (luft:surface-mesh-companions mesh))
         (torch-mesh (first companions)))
    (ok (= 1 (length companions)))
    (ok (luft::%mesh-closed-p mesh))
    (ok (luft::%mesh-nondegenerate-p mesh))
    (ok (eq (render:scene-voxel-light scene)
            (luft:surface-mesh-voxel-light mesh)))
    (ok (eq (render:scene-voxel-light scene)
            (luft:surface-mesh-voxel-light torch-mesh)))
    (ok (= (* 4 instances)
           (length (luft.render::render-population-instance-words population))))
    (ok (= (* 2 instances)
           (length (luft.render::render-population-light-words population))))
    (ok (plusp
         (luft.render::render-population-opaque-triangle-instance-count
          population)))
    (ok (plusp
         (luft.render::render-population-translucent-triangle-instance-count
          population)))
    ;; The exact socket and shaft retain 24 bronze triangles per attachment;
    ;; the old eight-triangle static flame cone is now a separate volume draw.
    (ok (= 96 (luft:surface-mesh-triangle-count torch-mesh)))
    (let ((body-stock
            (luft.render::surface-assembly-offset
             luft.render::*torch-body-surface*)))
      (dolist (stream (list (luft:surface-mesh-face-instance-words torch-mesh)
                            (luft:surface-mesh-band-instance-words torch-mesh)
                            (luft:surface-mesh-fan-instance-words torch-mesh)))
        (ok (loop for offset from 3 below (length stream) by 4
                  always (= body-stock
                            (ldb
                             (byte luft:+mesh-instance-stock-bit-count+ 16)
                             (aref stream offset)))))))
    (ok (some #'plusp
              (coerce (luft.render::render-population-light-words population)
                      'list)))))

(deftest streaming-phases-retain-both-sides-of-a-chunk-seam-contact
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 7))
         (scene
           (progn
             (luft.render::scene-builder-cell
              builder 63 8 4 :architecture-p t)
             (luft.render::scene-builder-cell
              builder 64 8 4
              :material luft.render::*crystal-material-placement*)
             (luft.render::finish-scene-builder builder)))
         (streaming (render:make-streaming-scene scene))
         (left (luft:chunk-key-at 63 8))
         (right (luft:chunk-key-at 64 8)))
    (setf (gethash left (luft.render::streaming-scene-loaded streaming)) 2
          (gethash right (luft.render::streaming-scene-loaded streaming)) 2)
    (let* ((left-mesh (render:mesh-streaming-chunk streaming left 2))
           (right-mesh (render:mesh-streaming-chunk streaming right 2))
           (right-crystal (first (luft:surface-mesh-companions right-mesh)))
           (translucent-cohort
             (append (luft:surface-mesh-companions left-mesh)
                     (luft:surface-mesh-companions right-mesh))))
      (flet ((interface-face-p (mesh)
               (let ((found nil))
                 (luft::%map-mesh-triangles
                  (lambda (kind a b c)
                    (when (and (eq kind :face)
                               (every (lambda (point) (= 512 (first point)))
                                      (list a b c)))
                      (setf found t)))
                  mesh)
                 found)))
        ;; The target chunk has no opaque cells, but its captured empty phase
        ;; chain remains distinct from an unknown residency boundary.
        (ok (null (gethash right
                           (luft.render::streaming-scene-opaque-store
                            streaming))))
        (ok (null (gethash left
                           (luft.render::streaming-scene-translucent-store
                            streaming))))
        (ok (interface-face-p left-mesh))
        (ok right-crystal)
        (ok (interface-face-p right-crystal))
        (ok (luft::%meshes-closed-p (list left-mesh right-mesh)))
        (ok (luft::%meshes-closed-p translucent-cohort))
        ;; A chunk's root can be deliberately open at an ownership seam; the
        ;; full crystal cell belongs to this target chunk and remains closed.
        (ok (luft::%mesh-closed-p right-crystal))))))

(deftest an-empty-streaming-neighborhood-materializes-one-empty-root
  (let* ((scene
           (luft.render::finish-scene-builder
            (luft.render::make-scene-builder :horizontal-bits 7)))
         (streaming (render:make-streaming-scene scene))
         (key (luft:chunk-key-at 64 64)))
    (setf (gethash key (luft.render::streaming-scene-loaded streaming)) 2)
    (let ((mesh (render:mesh-streaming-chunk streaming key 2)))
      (ok (zerop (luft:surface-mesh-triangle-count mesh)))
      (ok (null (luft:surface-mesh-companions mesh)))
      (ok (eq (render:scene-voxel-light scene)
              (luft:surface-mesh-voxel-light mesh))))))

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

(deftest material-bevel-profile-compiles-semantic-widths-once
  (let* ((profile (render:make-material-bevel-profile
                   :terrain-width 4 :architecture-width 1 :contact-width 2))
         (widths (render:compile-material-bevel-profile profile))
         (crystal-stock
           (luft.render::surface-assembly-offset
            luft.render::*crystal-surface*)))
    (ok (= 4 (aref widths luft.render::+grass-stock+)))
    (ok (= 4 (aref widths luft.render::+soil-stock+)))
    (ok (= 4 (aref widths luft.render::+turf-edge-stock+)))
    (ok (= 1 (aref widths luft.render::+stone-stock+)))
    (ok (= 1 (aref widths luft.render::+foundation-stone-stock+)))
    (ok (= 2 (aref widths luft.render::+turf-set-stone-stock+)))
    (ok (= 2 (aref widths luft.render::+soil-set-stone-stock+)))
    (ok (= 2 (aref widths luft.render::+deep-set-stone-stock+)))
    (ok (= 4 (aref widths crystal-stock)))
    (multiple-value-bind (stock-masks site-widths)
        (luft.render::compile-material-bevel-site-policy profile)
      (ok (= luft.render::+material-bevel-terrain-mask+
             (aref stock-masks luft.render::+grass-stock+)))
      (ok (= luft.render::+material-bevel-architecture-mask+
             (aref stock-masks luft.render::+stone-stock+)))
      (ok (= luft.render::+material-bevel-crystal-mask+
             (aref stock-masks crystal-stock)))
      (ok (= (logior luft.render::+material-bevel-terrain-mask+
                     luft.render::+material-bevel-architecture-mask+)
             (aref stock-masks luft.render::+turf-set-stone-stock+)))
      (ok (= 4 (aref site-widths
                     luft.render::+material-bevel-terrain-mask+)))
      (ok (= 1 (aref site-widths
                     luft.render::+material-bevel-architecture-mask+)))
      (ok (= 4 (aref site-widths
                     luft.render::+material-bevel-crystal-mask+)))
      (ok (= 2 (aref site-widths
                     (logior luft.render::+material-bevel-terrain-mask+
                             luft.render::+material-bevel-architecture-mask+)))))))

(deftest material-bevel-policy-builds-one-closed-site-local-surface
  (let* ((builder (luft.render::make-scene-builder :horizontal-bits 4))
         (scene
           (progn
             (luft.render::scene-builder-box builder 4 9 4 9 2 2)
             (luft.render::scene-builder-box
              builder 5 6 5 6 3 5 :architecture-p t)
             (luft.render::finish-scene-builder builder)))
         (profile (render:make-material-bevel-profile))
         (meshes (render:make-material-bevel-meshes scene profile)))
    (multiple-value-bind (mesh width-census)
        (render:make-material-bevel-mesh scene profile)
      (ok (= 4 (luft:surface-mesh-bevel-width mesh)))
      (ok (luft::%mesh-closed-p mesh))
      (ok (luft::%mesh-nondegenerate-p mesh))
      (ok (plusp (aref width-census 1)))
      (ok (plusp (aref width-census 2)))
      (ok (zerop (aref width-census 3)))
      (ok (plusp (aref width-census 4)))
      (ok (= 1 (length meshes)))
      (ok (= 0 (caar meshes)))
      (ok (luft::%mesh-closed-p (cdar meshes))))))

(defun mesh-triangle-quality (mesh)
  "Return minimum angle, maximum longest-edge/altitude ratio, and sliver count."
  (let ((minimum-angle 180.0d0)
        (maximum-aspect 0.0d0)
        (aspect-over-five 0))
    (labels ((distance-squared (left right)
               (loop for l in left
                     for r in right
                     sum (expt (- l r) 2)))
             (angle (left-squared right-squared opposite-squared)
               (* (/ 180.0d0 pi)
                  (acos
                   (max -1.0d0
                        (min 1.0d0
                             (/ (- (+ left-squared right-squared)
                                   opposite-squared)
                                (* 2.0d0
                                   (sqrt (* left-squared
                                            right-squared))))))))))
      (luft::%map-mesh-triangles
       (lambda (kind a b c)
         (declare (ignore kind))
         (let* ((ab2 (distance-squared a b))
                (bc2 (distance-squared b c))
                (ca2 (distance-squared c a))
                (cross
                  (luft::%cross (luft::%point- b a)
                                (luft::%point- c a)))
                (cross2
                  (loop for component in cross
                        sum (* component component)))
                (aspect
                  (/ (float (max ab2 bc2 ca2) 1.0d0)
                     (sqrt cross2))))
           (setf minimum-angle
                 (min minimum-angle
                      (angle ab2 ca2 bc2)
                      (angle ab2 bc2 ca2)
                      (angle bc2 ca2 ab2))
                 maximum-aspect (max maximum-aspect aspect))
           (when (> aspect 5.0d0)
             (incf aspect-over-five))))
       mesh))
    (values minimum-angle maximum-aspect aspect-over-five)))

(deftest material-bevel-transition-contracts-the-medial-t-junction
  (let* ((scene (render:make-material-bevel-transition-study-scene))
         (width-one (render:make-render-mesh scene :bevel-width 1)))
    (multiple-value-bind (mesh width-census diagnostics)
        (render:make-material-bevel-mesh
         scene (render:make-material-bevel-profile))
      (ok (plusp (aref width-census 1)))
      (ok (plusp (aref width-census 2)))
      (ok (plusp (aref width-census 4)))
      (ok (equalp #(0 11 5 0 7) width-census))
      (ok (= 31 (getf diagnostics :collapsed-triangle-count)))
      (ok (= 3 (getf diagnostics :unmatched-edge-count)))
      (ok (= 1 (getf diagnostics :repaired-edge-count)))
      (ok (zerop (getf diagnostics :residual-edge-count)))
      (ok (equal '(((48 34 26) (48 36 28) (48 38 30)))
                 (getf diagnostics :candidate-splits)))
      (ok (= 190 (luft:surface-mesh-triangle-count mesh)))
      (ok (luft::%mesh-closed-p mesh))
      (ok (luft::%mesh-nondegenerate-p mesh))
      ;; Contracting the medial T-junction may subdivide a neighbour, but it
      ;; must not make triangle quality worse than the width-one topology
      ;; witness from which the mixed surface was evaluated.
      (multiple-value-bind (minimum-angle maximum-aspect sliver-count)
          (mesh-triangle-quality mesh)
        (multiple-value-bind
              (witness-minimum-angle witness-maximum-aspect
               witness-sliver-count)
            (mesh-triangle-quality width-one)
          (ok (>= minimum-angle (- witness-minimum-angle 1.0d-9)))
          (ok (<= maximum-aspect (+ witness-maximum-aspect 1.0d-9)))
          (ok (<= sliver-count witness-sliver-count)))))))

(deftest compiled-material-site-field-matches-its-generic-repair-oracle
  (let* ((scene (render:make-material-bevel-transition-study-scene))
         ;; Compile the material vocabulary only after witness construction;
         ;; chamfer assembly can intern stocks while building that witness.
         (witness (render:make-render-mesh scene :bevel-width 1))
         (profile (render:make-material-bevel-profile)))
    (multiple-value-bind (stock-masks site-widths)
        (luft.render::compile-material-bevel-site-policy profile)
      (ok (luft::%paged-byte-stock-mask-policy-p
           (luft:surface-mesh-domain witness) stock-masks site-widths))
      (flet ((generic-width (x y z stocks)
               (declare (ignore x y z))
               (let ((site-mask 0))
                 (dolist (stock stocks)
                   (setf site-mask
                         (logior site-mask (aref stock-masks stock))))
                 (aref site-widths site-mask))))
        (dolist (contract-p '(nil t))
          (multiple-value-bind
                (generic generic-census generic-diagnostics)
              (luft:vary-surface-mesh-bevel-widths
               witness #'generic-width
               :contract-t-junctions-p contract-p)
            (multiple-value-bind
                  (compiled compiled-census compiled-diagnostics)
                (luft:vary-surface-mesh-bevel-widths-from-stock-masks
                 witness stock-masks site-widths
                 :contract-t-junctions-p contract-p)
              (ok (luft::%same-surface-mesh-representation-p
                   generic compiled))
              (ok (equalp generic-census compiled-census))
              (ok (equal generic-diagnostics compiled-diagnostics)))))))))

(deftest material-bevel-transition-can-exhibit-the-uncontracted-t-junction
  (multiple-value-bind (mesh width-census diagnostics)
      (render:make-material-bevel-mesh
       (render:make-material-bevel-transition-study-scene)
       (render:make-material-bevel-profile)
       :contract-t-junctions-p nil)
    (declare (ignore width-census))
    (ok (= 3 (getf diagnostics :unmatched-edge-count)))
    (ok (zerop (getf diagnostics :repaired-edge-count)))
    (ok (= 3 (getf diagnostics :residual-edge-count)))
    (ok (not (luft::%mesh-closed-p mesh)))
    ;; The diagnostic mesh omits zero-area triangles.  Its defect is solely
    ;; the long-edge/short-edge connectivity mismatch exposed by construction
    ;; ink, not a retained degenerate primitive.
    (ok (luft::%mesh-nondegenerate-p mesh))))

(deftest material-bevel-transition-isolates-the-exact-split-neighborhood
  (let ((scene (render:make-material-bevel-transition-study-scene))
        (profile (render:make-material-bevel-profile)))
    (flet ((neighborhood (contract-p)
             (multiple-value-bind (mesh width-census diagnostics)
                 (render:make-material-bevel-mesh
                  scene profile :contract-t-junctions-p contract-p)
               (declare (ignore width-census))
               (luft:surface-mesh-split-neighborhood
                mesh (first (getf diagnostics :candidate-splits))))))
      (let ((uncontracted (neighborhood nil))
            (contracted (neighborhood t)))
        (ok (= 3 (luft:surface-mesh-triangle-count uncontracted)))
        (ok (= 4 (luft:surface-mesh-triangle-count contracted)))
        (ok (luft::%mesh-nondegenerate-p uncontracted))
        (ok (luft::%mesh-nondegenerate-p contracted))
        (let ((inked (luft:surface-mesh-with-triangle-ink contracted)))
          (ok (= (luft:surface-mesh-triangle-count contracted)
                 (luft:surface-mesh-triangle-count inked)))
          (ok (luft::%same-plane-areas-p
               (luft::%mesh-oriented-plane-areas contracted)
               (luft::%mesh-oriented-plane-areas inked))))))))

(defun check-authored-stair-boundary (boundary)
  (multiple-value-bind (mesh width-census diagnostics)
      (render:make-material-bevel-mesh
       (render:make-mountain-sanctuary-scene :stair-boundary boundary)
       (render:make-material-bevel-profile))
    (declare (ignore diagnostics))
    (ok (plusp (aref width-census 1)))
    (ok (plusp (aref width-census 2)))
    (ok (plusp (aref width-census 4)))
    (ok (luft::%mesh-closed-p mesh))
    (ok (luft::%mesh-nondegenerate-p mesh))))

(deftest open-stair-remains-an-ordinary-closed-material-surface
  (check-authored-stair-boundary :open))

(deftest bordered-stair-remains-an-ordinary-closed-material-surface
  (check-authored-stair-boundary :border))

(deftest low-wall-stair-remains-an-ordinary-closed-material-surface
  (check-authored-stair-boundary :low-wall))

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
         (flame-vertex
           (luft.render.shaders:torch-flame-vertex-specification))
         (flame-fragment
           (luft.render.shaders:torch-flame-fragment-specification))
         (present-vertex
           (luft.render.shaders:present-vertex-specification))
         (present-fragment
           (luft.render.shaders:present-fragment-specification))
         (sky-fragment
           (luft.render.shaders:sky-fragment-specification))
         (sky-temporal-fragment
           (luft.render.shaders:sky-temporal-fragment-specification))
         (exposure-probe-fragment
           (luft.render.shaders:exposure-probe-fragment-specification))
         (vertex-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl vertex)))
         (fragment-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl fragment)))
         (flame-vertex-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl flame-vertex)))
         (flame-fragment-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl flame-fragment)))
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
    (ok (search "gemstone_radiance" fragment-msl))
    (ok (search "[[instance_id]]" flame-vertex-msl))
    (ok (search "flame_instances" flame-vertex-msl))
    (ok (search "flame_effect_parameters" flame-fragment-msl))
    ;; The expensive dielectric response must remain structured control flow,
    ;; not an eager select paid by every ordinary terrain fragment.
    (ok (search "if (abs((kernel_code - 8.0f)) < 0.5f)" fragment-msl))
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
    (ok (luv.spir-v:compile-shader-specification flame-vertex))
    (ok (luv.spir-v:compile-shader-specification flame-fragment))
    (ok (luv.msl:compile-msl sky-fragment))
    (ok (luv.msl:compile-msl sky-temporal-fragment))
    (ok (luv.msl:compile-msl exposure-probe-fragment))
    (ok (luv.spir-v:compile-shader-specification sky-fragment))
    (ok (luv.spir-v:compile-shader-specification sky-temporal-fragment))
    (ok (luv.spir-v:compile-shader-specification exposure-probe-fragment))
    (ok (luv.spir-v:compile-shader-specification present-vertex))
    (ok (luv.spir-v:compile-shader-specification present-fragment))))

(deftest exposure-probes-decode-and-adapt-with-moppes-asymmetric-rates
  (let* ((luminance 0.16d0)
         (encoded
           (round
            (* 255d0
               (/ (+ (log luminance) 9.21034d0) 11.98293d0))))
         (bytes
           (make-array render::+exposure-probe-byte-count+
                       :element-type '(unsigned-byte 8)
                       :initial-element 0)))
    (loop for index from 0 below (length bytes) by 4
          do (setf (aref bytes index) encoded))
    (ok (< (abs (- luminance
                   (render::exposure-probe-average-luminance bytes)))
           0.01d0))
    ;; Looking into more light closes down quickly; opening into darkness is
    ;; deliberately slower, matching Moppe's eye-adaptation architecture.
    (ok (< (abs (- 0.955f0
                   (render::adapted-exposure 1.0f0 0.32f0)))
           1.0e-6))
    (ok (< (abs (- 1.036f0
                   (render::adapted-exposure 1.0f0 0.08f0)))
           1.0e-6))
    (ok (< (render::adapted-exposure 1.9f0 1000.0f0) 1.9f0))
    (ok (> (render::adapted-exposure 0.55f0 0.00001f0) 0.55f0))))

(deftest the-camera-block-packs-both-projections
  (let ((camera (render:make-fly-camera))
        (player (render:make-walking-player)))
    (flet ((lane (projection)
             (let ((render:*projection* projection))
               (let ((view
                       (luft.render::capture-frame-view
                        camera 1100 800 #(0.0 0.0))))
                 (luft.render::camera-uniform-data
                  view view #(0.5 0.5 0.001 0.001) 1.0 player)))))
      (let ((perspective (lane :perspective))
            (isometric (lane :isometric))
            (eighth
              (let ((render:*projection* :isometric))
                (let ((view
                        (luft.render::capture-frame-view
                         camera 1100 800 #(0.0 0.0))))
                  (luft.render::camera-uniform-data
                   view view #(0.5 0.5 0.001 0.001) 1.0 player 1)))))
        (ok (= 108 (length perspective)))
        (ok (typep perspective '(simple-array single-float (108))))
        (ok (= 1.0 (aref perspective 22)))
        (ok (= 0.0 (aref isometric 22)))
        (flet ((depth (data view-z)
                 (let ((clip (+ (* view-z (aref data 18)) (aref data 19))))
                   (if (zerop (aref data 22)) clip (/ clip view-z)))))
          (ok (< (abs (depth perspective 0.1)) 1d-4))
          (ok (< (abs (- (depth perspective 600.0) 1.0)) 1d-4))
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
        (ok (equalp #(61.5 48.5 15.48 0.0)
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
               (aref perspective 91)))
        (ok (equalp #(61.5 48.5 15.48 0.0)
                    (subseq perspective 92 96)))
        (ok (equalp #(0.0 1.0 0.0 0.0)
                    (subseq perspective 96 100)))))))

(deftest an-off-centre-pointer-ray-inverts-the-rendered-projection
  (let* ((canvas (make-instance 'luv:sdl-canvas :width 1000 :height 800))
         (camera
           (render:make-fly-camera
            :position (luv.arithmetic.lisp.vec3:make-vec3 0.0 0.0 10.0)
            :yaw 0.0 :pitch 0.0))
         (viewer
           (clim:make-application-frame
            'render:viewer :canvas canvas :camera camera))
         (render:*projection* :isometric)
         (render:*isometric-height* 20.0))
    ;; At yaw and pitch zero camera UP is world +Z.  A pointer one quarter of
    ;; the viewport below centre must therefore start five cells below the
    ;; camera, not at the vertically mirrored point five cells above it.
    (setf (luft.render::viewer-pointer-x viewer) 500.0
          (luft.render::viewer-pointer-y viewer) 600.0)
    (multiple-value-bind (origin direction)
        (luft.render::viewer-pointer-ray viewer)
      (ok (< (abs (- 5.0
                     (luv.arithmetic.lisp.vec3:vec3-z origin)))
             1.0e-6))
      (ok (< (abs (- 1.0
                     (luv.arithmetic.lisp.vec3:vec3-x direction)))
             1.0e-6)))))

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

(deftest film-cleanup-cannot-resurrect-a-shutting-down-viewer
  (flet ((make-probe ()
           (clim:make-application-frame
            'render:viewer :canvas (make-instance 'luv:canvas))))
    (let* ((viewer (make-probe))
           (capture
             (make-instance 'luv:application-capture
                            :application viewer :kind :film)))
      (setf (luv:capture-client-state capture) '(:running-p t)
            (render::viewer-running-p viewer) nil)
      (luv:cleanup-capture viewer capture)
      (ok (render::viewer-running-p viewer)))
    (let* ((viewer (make-probe))
           (capture
             (make-instance 'luv:application-capture
                            :application viewer :kind :film)))
      (setf (luv:capture-client-state capture) '(:running-p t)
            (render::viewer-running-p viewer) nil)
      (luv:request-application-capture-shutdown viewer)
      (luv:cleanup-capture viewer capture)
      (ok (not (render::viewer-running-p viewer))))))
