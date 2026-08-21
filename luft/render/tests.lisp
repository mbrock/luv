(defpackage #:luft.render.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:clim #:clim)
                    (#:climi #:clim-internals)
                    (#:luv #:luv)
                    (#:render #:luft.render)))

(in-package #:luft.render.tests)

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
  (let ((viewer (clim:make-application-frame 'render:viewer)))
    (ok (typep viewer 'clim:application-frame))
    ;; No second CLIM process or native window: canvas delivery executes the
    ;; atelier's commands inline on the renderer owner thread.
    (ok (null (climi::frame-process viewer)))
    (ok (equal '(luft.render::com-start-moving :forward)
               (luft.render::viewer-key-command viewer (key-press :w))))
    (ok (equal '(luft.render::com-stop-moving :forward)
               (luft.render::viewer-key-command viewer (key-release :w))))
    (ok (equal '(luft.render::com-reset-view)
               (luft.render::viewer-key-command viewer (key-press :r))))
    (ok (equal '(luft.render::com-toggle-construction-lines)
               (luft.render::viewer-key-command viewer (key-press :c))))
    (ok (equal '(luft.render::com-toggle-fullscreen)
               (luft.render::viewer-key-command viewer (key-press :f11))))
    (ok (equal '(luft.render::com-quit)
               (luft.render::viewer-key-command
                viewer (key-press :q :character #\q
                                     :modifiers '(:control)))))
    (ok (null (luft.render::viewer-key-command viewer (key-press :f8))))
    (clim:execute-frame-command
     viewer (luft.render::viewer-key-command viewer (key-press :w)))
    (ok (luft.render::viewer-control-active-p viewer :forward))
    (clim:execute-frame-command
     viewer (luft.render::viewer-key-command viewer (key-release :w)))
    (ok (not (luft.render::viewer-control-active-p viewer :forward)))))

(deftest face-materialization-is-a-polarity-partition-of-the-surface
  (let* ((solid (render:make-demo-solid))
         (surface (luft:surface-chain solid))
         (materialization (render:make-face-materialization solid))
         (domain (render:face-materialization-domain materialization))
         (words (render:face-materialization-words materialization))
         (positive (render:face-materialization-positive-count materialization))
         (negative (render:face-materialization-negative-count materialization)))
    (ok (= (+ positive negative) (luft:chain-count surface)))
    (ok (= (length words)
           (* luft:+face-record-word-count+ (luft:chain-count surface))))
    (loop for index below (+ positive negative) do
      (multiple-value-bind (face shape stock construction-mask)
          (luft:load-face-record words index domain)
        (ok (eq (< index positive) (luft:site-positive-p face)))
        (ok (luft:shape-word-valid-p shape))
        (ok (<= 0 stock 3))
        (ok (typep construction-mask '(unsigned-byte 21)))))))

(deftest the-mountain-scene-keeps-one-small-paper-palette
  (let* ((scene (render:make-mountain-sanctuary-scene))
         (materialization (render:make-face-materialization scene))
         (domain (render:face-materialization-domain materialization))
         (words (render:face-materialization-words materialization))
         (count (+ (render:face-materialization-positive-count materialization)
                   (render:face-materialization-negative-count materialization)))
         (stocks (make-hash-table)))
    (dotimes (index count)
      (multiple-value-bind (face shape stock)
          (luft:load-face-record words index domain)
        (declare (ignore face shape))
        (setf (gethash stock stocks) t)))
    (ok (equal '(0 1 2 3)
               (sort (loop for stock being the hash-keys of stocks collect stock)
                     #'<)))))

(deftest the-miter-study-retains-its-star-family-and-wall-termination
  (let* ((scene (render:make-miter-study-scene))
         (solid (render:scene-solid scene))
         (domain (luft:chain-domain solid))
         (occupancy
           (lambda (x y z)
             (luft:chain-cell-occupancy-bit solid x y z))))
    (flet ((vertex-count (x y z)
             (nth-value
              7 (luft:classify-site-star
                 domain
                 (luft:make-site domain x y z luft:+vertex-extent+ 1)
                 occupancy))))
      ;; A solid four-cell floor under one, two, and three terrace cells.
      (ok (= 5 (vertex-count 4 3 2)))
      (ok (= 6 (vertex-count 5 3 2)))
      (ok (= 7 (vertex-count 9 5 2))))
    ;; The lower terrace ends while its neighbouring wall cells continue.
    (ok (= 1 (funcall occupancy 11 7 2)))
    (ok (= 0 (funcall occupancy 12 7 2)))
    (ok (= 1 (funcall occupancy 11 8 2)))
    (ok (= 1 (funcall occupancy 12 8 2)))))

(deftest face-and-atelier-shaders-lower-through-both-conventional-backends
  (let* ((vertex (luft.render.shaders:face-vertex-specification))
         (fragment (luft.render.shaders:face-fragment-specification))
         (present-vertex
           (luft.render.shaders:present-vertex-specification))
         (present-fragment
           (luft.render.shaders:present-fragment-specification))
         (inspector-vertex
           (luft.render.shaders:inspector-vertex-specification))
         (inspector-fragment
           (luft.render.shaders:inspector-fragment-specification))
         (msl-source
           (luv.msl:msl-document-source (luv.msl:compile-msl vertex)))
         (fragment-msl
           (luv.msl:msl-document-source (luv.msl:compile-msl fragment))))
    (ok (search "[[vertex_id]]" msl-source))
    (ok (search "[[instance_id]]" msl-source))
    (ok (search "const device uint4* faces" msl-source))
    (ok (search "camera_position" msl-source))
    (ok (search "motion_output" fragment-msl))
    (ok (search "construction_mask" fragment-msl))
    (ok (luv.msl:compile-msl inspector-vertex))
    (ok (luv.msl:compile-msl inspector-fragment))
    (ok (luv.spir-v:compile-shader-specification vertex))
    (ok (luv.spir-v:compile-shader-specification fragment))
    (ok (luv.spir-v:compile-shader-specification present-vertex))
    (ok (luv.spir-v:compile-shader-specification present-fragment))
    (ok (luv.spir-v:compile-shader-specification inspector-vertex))
    (ok (luv.spir-v:compile-shader-specification inspector-fragment))))

(deftest the-camera-block-packs-both-projections
  ;; Perspective and isometric share the three projection rows and differ
  ;; only in the homogeneous divisor, so the test that they agree on depth
  ;; is the test that the shared rows really are shared.
  (let ((camera (render:make-fly-camera)))
    (flet ((lane (projection)
             (let ((render:*projection* projection))
               (let ((view
                       (luft.render::capture-frame-view
                        camera 1100 800 #(0.0 0.0))))
                 (luft.render::camera-uniform-data
                  view view #(0.5 0.5 0.001 0.001) 1.0)))))
      (let ((perspective (lane :perspective))
            (isometric (lane :isometric))
            (near 0.1)
            (far 200.0))
        (ok (= 52 (length perspective)))
        (ok (typep perspective '(simple-array single-float (52))))
        (dolist (data (list perspective isometric))
          (ok (> (aref data 16) 0.0))
          (ok (> (aref data 17) 0.0))
          (ok (> (aref data 18) 0.0))
          (ok (< (aref data 19) 0.0)))
        (ok (= 1.0 (aref perspective 22)))
        (ok (= 0.0 (aref isometric 22)))
        (flet ((depth (data view-z)
                 (let ((clip (+ (* view-z (aref data 18)) (aref data 19))))
                   (if (zerop (aref data 22)) clip (/ clip view-z)))))
          (ok (< (abs (depth perspective near)) 1d-4))
          (ok (< (abs (- (depth perspective far) 1.0)) 1d-4))
          (ok (< (abs (depth isometric near)) 1d-4))
          (ok (< (abs (- (depth isometric far) 1.0)) 1d-4)))
        (ok (= (aref perspective 20) render:*chamfer-width*))
        (ok (= (aref perspective 21) render:*wireframe*))
        (ok (= 1.0 (aref perspective 23)))
        (ok (equalp #(0.5 0.5 0.001 0.001)
                    (subseq perspective 48 52)))))))

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
      (ok (luft:shape-word-valid-p
           (luft.render::site-inspection-shape-word inspection))))))

;;; A literal Lisp transcription of FACE-VERTEX-SPECIFICATION's position
;;; arithmetic, reading the same face record the GPU reads.  It exists so a
;;; disagreement between the shader and LUFT's CPU realization is a failing
;;; test rather than something you have to see in a screenshot.

(defun shader-realize-point (words record-index width i j)
  (let* ((base (* 4 record-index))
         (low (aref words base))
         (high (aref words (+ base 1)))
         (shape (aref words (+ base 2)))
         (extent (ldb (byte 3 0) low))
         (negative (ldb (byte 1 3) low))
         (x (float (ldb (byte 24 4) low) 1d0))
         (y (float (+ (ldb (byte 4 28) low) (* (ldb (byte 20 0) high) 16)) 1d0))
         (z (float (ldb (byte 8 20) high) 1d0))
         (u (if (= extent 6) #(0d0 1d0 0d0) #(1d0 0d0 0d0)))
         (v (if (= extent 3) #(0d0 1d0 0d0) #(0d0 0d0 1d0)))
         (canonical (cond ((= extent 3) #(0d0 0d0 1d0))
                          ((= extent 5) #(0d0 -1d0 0d0))
                          (t #(1d0 0d0 0d0))))
         (normal (if (= negative 1) (map 'vector #'- canonical) canonical))
         (w (float width 1d0))
         (i-boundary (or (= i 0) (= i 3)))
         (j-boundary (or (= j 0) (= j 3)))
         (point-kind (if i-boundary (if j-boundary 2 1) (if j-boundary 1 0)))
         (lambda-i (ecase i (0 0d0) (1 w) (2 (- 1d0 w)) (3 1d0)))
         (lambda-j (ecase j (0 0d0) (1 w) (2 (- 1d0 w)) (3 1d0)))
         (flat (vector (+ x (* (aref u 0) lambda-i) (* (aref v 0) lambda-j))
                       (+ y (* (aref u 1) lambda-i) (* (aref v 1) lambda-j))
                       (+ z (* (aref u 2) lambda-i) (* (aref v 2) lambda-j))))
         (corner-code
           (if (= i 0)
               (if (= j 0) (ldb (byte 6 8) shape) (ldb (byte 6 14) shape))
               (if (= j 0) (ldb (byte 6 20) shape) (ldb (byte 6 26) shape))))
         (direction (ldb (byte 5 0) corner-code))
         (corner-q (vector (float (- (mod direction 3) 1) 1d0)
                           (float (- (mod (floor direction 3) 3) 1) 1d0)
                           (float (- (floor direction 9) 1) 1d0)))
         (corner-reach (if (= 1 (ldb (byte 1 5) corner-code)) 2/3 1/2))
         (edge-code
           (if i-boundary
               (if (= i 0) (ldb (byte 2 0) shape) (ldb (byte 2 2) shape))
               (if (= j 0) (ldb (byte 2 4) shape) (ldb (byte 2 6) shape))))
         (tangent (if i-boundary u v))
         (side (if i-boundary (if (= i 0) -1d0 1d0) (if (= j 0) -1d0 1d0)))
         (outward (map 'vector (lambda (c) (* c side)) tangent))
         (normal-sign (if (= edge-code 1) -1d0 1d0))
         (edge-q (if (= edge-code 0)
                     #(0d0 0d0 0d0)
                     (map 'vector #'-
                          (map 'vector (lambda (c) (* c normal-sign)) normal)
                          outward)))
         (q (ecase point-kind (2 corner-q) (1 edge-q) (0 #(0d0 0d0 0d0))))
         (reach (ecase point-kind (2 corner-reach) (1 1/2) (0 0))))
    (map 'vector (lambda (f qq) (+ f (* qq w (float reach 1d0)))) flat q)))

(deftest the-shader-realizes-exactly-what-the-cpu-reference-realizes
  (let* ((width 0.20d0)
         (materialization (render:make-face-materialization
                           (render:make-gallery-solid)))
         (domain (render:face-materialization-domain materialization))
         (words (render:face-materialization-words materialization))
         (count (+ (render:face-materialization-positive-count materialization)
                   (render:face-materialization-negative-count materialization)))
         (worst 0d0))
    (ok (plusp count))
    (dotimes (record count)
      (multiple-value-bind (face shape) (luft:load-face-record words record domain)
        (dotimes (i 4)
          (dotimes (j 4)
            (let ((shaded (shader-realize-point words record width i j)))
              (multiple-value-bind (rx ry rz)
                  (luft:realize-face-point domain face shape width i j)
                (setf worst
                      (max worst
                           (abs (- (aref shaded 0) rx))
                           (abs (- (aref shaded 1) ry))
                           (abs (- (aref shaded 2) rz))))))))))
    (ok (zerop worst))))
