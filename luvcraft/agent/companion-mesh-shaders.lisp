(in-package #:luvcraft.shaders)

;;; Blender supplies the surface and the IK-solved skeleton, not a distance
;;; field. The mesh shares the companions' camera and sky uniforms.
(define-shader-method shader-specification-for
    companion-mesh-vertex-specification
    ((role (eql :companion-mesh)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (normal :vec3 :location 1)
              (albedo :vec3 :location 2)
              (bind-point :vec3 :location 3)
              (origin-scale :vec4 :location 4)
              (heading :vec4 :location 5))
     :outputs ((clip-position :vec4 :built-in :position)
               (world-point :vec3 :location 0)
               (world-normal :vec3 :location 1)
               (surface-color :vec3 :location 2)
               (fabric-point :vec3 :location 3)
               (edge-light :float :location 4))
     :resources ((frame-state :uniform-block :set 0 :binding 2
                             :members #.*frame-uniform-members*)))
  (let* ((facing (swizzle heading :xyz))
         (sideways (vec3 (swizzle facing :z) 0.0 (- (swizzle facing :x))))
         (point (+ (swizzle origin-scale :xyz)
                   (* (swizzle origin-scale :w)
                      (+ (* sideways (swizzle position :x))
                         (+ (vec3 0.0 (swizzle position :y) 0.0)
                            (* facing (swizzle position :z)))))))
         (relative (- point (representation (swizzle camera-vector :xyz))))
         (view-x (dot relative (representation (swizzle right-vector :xyz))))
         (view-y (dot relative (representation (swizzle up-vector :xyz))))
         (view-z (dot relative (representation (swizzle forward-vector :xyz)))))
    (set-output clip-position
                (vec4 (* view-x (representation (swizzle projection-vector :x)))
                      (- (* view-y (representation (swizzle projection-vector :y))))
                      (+ (* view-z (representation (swizzle projection-vector :z)))
                         (representation (swizzle projection-vector :w)))
                      view-z))
    (set-output world-point point)
    (set-output world-normal
                (+ (* sideways (swizzle normal :x))
                   (+ (vec3 0.0 (swizzle normal :y) 0.0)
                      (* facing (swizzle normal :z)))))
    (set-output surface-color albedo)
    (set-output fabric-point bind-point)
    (set-output edge-light (swizzle heading :w))))

(define-shader-method shader-specification-for
    companion-mesh-fragment-specification
    ((role (eql :companion-mesh)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((world-point :vec3 :location 0)
              (world-normal :vec3 :location 1)
              (surface-color :vec3 :location 2)
              (fabric-point :vec3 :location 3)
              (edge-light :float :location 4))
     :outputs ((color-output :vec4 :location 0))
     :resources ((frame-state :uniform-block :set 0 :binding 2
                             :members #.*frame-uniform-members*)))
  (let* ((normal (normalize world-normal))
         (view (normalize (- (representation (swizzle camera-vector :xyz)) world-point)))
         (sun (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (wrapped (max 0.0 (/ (+ (dot normal sun) 0.35) 1.35)))
         (sky (+ 0.55 (* 0.45 (swizzle normal :y))))
         (grain (- (lattice-noise (* fabric-point (vec3 85.0 85.0 85.0))) 0.5))
         (cloth (* surface-color (+ 1.0 (* 0.08 grain))))
         (rim (expt (- 1.0 (max 0.0 (dot normal view))) 3.0))
         (light (+ (* ambient sky) (* sun-color (+ 0.12 (* wrapped 1.15))))))
    (set-output color-output
                (vec4 (+ (* cloth light) (* surface-color (* edge-light rim))) 1.0))))
