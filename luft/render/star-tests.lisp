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
    (true (luv.spir-v:compile-shader-specification fragment))))

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
      (true (= 32 (length (luft:surface-mesh-star-site-words mesh)))))))

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
