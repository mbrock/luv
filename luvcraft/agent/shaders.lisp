;;; The first genuinely volumetric body for an embodied agent: one sphere SDF.
;;;
;;; A camera-facing square is only the conservative rasterization proxy.  Each
;;; fragment sends its world-space camera ray through the signed distance
;;; field and shades the point where sphere tracing converges.  The proxy's
;;; depth remains an approximation for now; the shape and its lighting are
;;; genuinely three-dimensional.

(in-package #:luvcraft.shaders)

(define-shader-method shader-specification-for
    gnome-sdf-vertex-specification
    ((role (eql :gnome-sdf)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((quad-corner :vec3 :location 0)
              (sphere-center-radius :vec4 :location 1))
     :outputs ((clip-position :vec4 :built-in :position)
               (proxy-world-position :vec3 :location 0)
               (sphere-output :vec4 :location 1))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((center (swizzle sphere-center-radius :xyz))
         (radius (swizzle sphere-center-radius :w))
         (camera (representation (swizzle camera-vector :xyz)))
         (right (representation (swizzle right-vector :xyz)))
         (up (representation (swizzle up-vector :xyz)))
         (forward (representation (swizzle forward-vector :xyz)))
         (corner-x (- (* (swizzle quad-corner :x) 2.0) 1.0))
         (corner-y (- (* (swizzle quad-corner :y) 2.0) 1.0))
         (world-position
           (+ center (+ (* right (* corner-x radius))
                        (* up (* corner-y radius)))))
         (relative (- world-position camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward))
         (x-scale (representation (swizzle projection-vector :x)))
         (y-scale (representation (swizzle projection-vector :y)))
         (z-scale (representation (swizzle projection-vector :z)))
         (z-offset (representation (swizzle projection-vector :w))))
    (set-output clip-position
                (vec4 (* view-x x-scale)
                      (- (* view-y y-scale))
                      (+ (* view-z z-scale) z-offset)
                      view-z))
    (set-output proxy-world-position world-position)
    (set-output sphere-output sphere-center-radius)))

(define-shader-method shader-specification-for
    gnome-sdf-fragment-specification
    ((role (eql :gnome-sdf)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((proxy-world-position :vec3 :location 0)
              (sphere-input :vec4 :location 1))
     :outputs ((color-output :vec4 :location 0))
     :resources
     ((frame-state :uniform-block :set 0 :binding 2
                   :members #.*frame-uniform-members*)))
  (let* ((camera (representation (swizzle camera-vector :xyz)))
         (center (swizzle sphere-input :xyz))
         (radius (swizzle sphere-input :w))
         (ray (normalize (- proxy-world-position camera)))
         ;; Forty deliberately boring sphere-tracing steps.  Once a ray has
         ;; converged it keeps the same distance, so the fixed fold is both a
         ;; real ray marcher and the smallest control-flow experiment.
         (travel
           (counted-fold (march 40.0 ray-distance 0.0)
             (let* ((point (+ camera (* ray ray-distance)))
                    (offset (- point center))
                    (distance (- (sqrt (dot offset offset)) radius)))
               (if (< (abs distance) 0.002)
                   ray-distance
                   (+ ray-distance (max distance 0.002))))))
         (point (+ camera (* ray travel)))
         (offset (- point center))
         (surface-distance (- (sqrt (dot offset offset)) radius))
         (coverage (if (< (abs surface-distance) 0.006) 1.0 0.0))
         (normal (normalize offset))
         (sun-direction (representation (swizzle sun-vector :xyz)))
         (sun-color (representation (swizzle sun-color-vector :xyz)))
         (ambient (representation (swizzle ambient-vector :xyz)))
         (diffuse (max 0.0 (dot normal sun-direction)))
         (view-facing (max 0.0 (dot normal (* ray -1.0))))
         (rim (expt (- 1.0 view-facing) 3.0))
         (albedo (vec3 0.10 0.42 0.92))
         (illumination
           (+ (* ambient 0.75)
              (* sun-color (+ 0.18 (* diffuse 1.35)))))
         (radiance
           (+ (* albedo illumination)
              (* (vec3 0.12 0.32 0.90) (* rim 0.35)))))
    ;; The scene target uses premultiplied alpha.  Misses leave no rectangular
    ;; trace of the conservative billboard.
    (set-output color-output (vec4 (* radiance coverage) coverage))))
