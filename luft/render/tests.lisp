(defpackage #:luft.render.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:render #:luft.render)))

(in-package #:luft.render.tests)

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
      (multiple-value-bind (face shape stock)
          (luft:load-face-record words index domain)
        (ok (eq (< index positive) (luft:site-positive-p face)))
        (ok (luft:shape-word-valid-p shape))
        (ok (<= 0 stock 3))))))

(deftest face-shaders-lower-through-both-conventional-backends
  (let* ((vertex (luft.render.shaders:face-vertex-specification))
         (fragment (luft.render.shaders:face-fragment-specification))
         (msl-source
           (luv.msl:msl-document-source (luv.msl:compile-msl vertex))))
    (ok (search "[[vertex_id]]" msl-source))
    (ok (search "[[instance_id]]" msl-source))
    (ok (search "const device uint4* faces" msl-source))
    (ok (search "camera_position" msl-source))
    (ok (luv.spir-v:compile-shader-specification vertex))
    (ok (luv.spir-v:compile-shader-specification fragment))))

(deftest fly-camera-packs-a-perspective-frame
  (let* ((camera (render:make-fly-camera))
         (data (luft.render::camera-uniform-data camera 1100 800)))
    (ok (= 20 (length data)))
    (ok (typep data '(simple-array single-float (20))))
    (ok (> (aref data 16) 0.0))
    (ok (> (aref data 17) 0.0))
    (ok (> (aref data 18) 1.0))
    (ok (< (aref data 19) 0.0))))
