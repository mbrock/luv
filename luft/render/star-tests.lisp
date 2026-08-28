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
