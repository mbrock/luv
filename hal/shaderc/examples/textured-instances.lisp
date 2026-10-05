;;; A small program in the shape a native renderer draws: no vertex buffers.
;;;
;;; The vertex stage pulls a unit quad's corners and each instance's placement
;;; from storage buffers by :VERTEX-INDEX and :INSTANCE-INDEX, and projects
;;; through rows in a uniform block.  The fragment stage samples an albedo
;;; texture through the standard linear sampler and compares a shadow map
;;; through the standard comparison sampler.  luv-shaderc reads this file in
;;; LUV.SHADER-USER, so the forms need no package prefixes.

(define-shader textured-instances-vertex
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv :vec2 :location 0)
               (shadow-coordinate :vec3 :location 1)
               (variant :uint :location 2 :interpolation :flat))
     :resources ((frame :uniform-block :binding 0
                  :members ((sun-direction :vec4)
                            (view-row-x :vec4)
                            (view-row-y :vec4)
                            (view-row-z :vec4)
                            (view-row-w :vec4)
                            (shadow-row-x :vec4)
                            (shadow-row-y :vec4)
                            (shadow-row-z :vec4)))
                 (corners :storage-buffer :binding 1 :element :vec4)
                 (instances :storage-buffer :binding 2 :element :vec4)))
  (let* ((corner (buffer-element corners vertex-index))
         (placement (buffer-element instances instance-index))
         (world (+ (* (swizzle corner :xyz) (swizzle placement :w))
                   (swizzle placement :xyz)))
         (point (vec4 world 1.0))
         (shadow (vec3 (dot shadow-row-x point)
                       (dot shadow-row-y point)
                       (dot shadow-row-z point))))
    (set-output clip-position
                (vec4 (dot view-row-x point)
                      (dot view-row-y point)
                      (dot view-row-z point)
                      (dot view-row-w point)))
    (set-output uv (swizzle corner :xy))
    (set-output shadow-coordinate
                (vec3 (+ (* (swizzle shadow :xy) 0.5) (vec2 0.5 0.5))
                      (swizzle shadow :z)))
    (set-output variant (mod instance-index (uint 4.0)))))

(define-shader textured-instances-fragment
    (:stage :fragment
     :inputs ((uv :vec2 :location 0)
              (shadow-coordinate :vec3 :location 1)
              (variant :uint :location 2 :interpolation :flat))
     :outputs ((color :vec4 :location 0))
     ;; A stage may declare a prefix of a uniform block; linking keeps the
     ;; longest view.
     :resources ((frame :uniform-block :binding 0
                  :members ((sun-direction :vec4)))
                 (albedo :texture-2d :binding 0)
                 (shadow-map :depth-texture-2d :binding 1)
                 (linear-clamp :sampler :binding 0)
                 (shadow-compare :sampler :binding 3)))
  (let* ((base (sample albedo linear-clamp uv))
         (lit (sample-compare shadow-map shadow-compare
                              (swizzle shadow-coordinate :xy)
                              (swizzle shadow-coordinate :z)))
         (tint (if (= variant (uint 0.0)) 1.0 0.85))
         (sun (clamp (swizzle sun-direction :w) 0.0 1.0))
         (shade (* (mix 0.35 1.0 (* lit sun)) tint)))
    (set-output color (vec4 (* (swizzle base :rgb) shade)
                            (swizzle base :a)))))

(define-shader-program textured-instances
  :vertex textured-instances-vertex
  :fragment textured-instances-fragment)
