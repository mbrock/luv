;;; A full-screen sky: three vertices cover the screen, and the fragment
;;; stage blends horizon into zenith and adds a soft sun.

(define-shader sky-vertex
    (:stage :vertex
     :inputs ((corner :uint :built-in :vertex-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv :vec2 :location 0)))
  (let* ((st (vec2 (float (* (uint 2) (logand corner (uint 1))))
                   (float (logand corner (uint 2))))))
    (set-output clip-position (vec4 (- (* st 2.0) (vec2 1.0 1.0)) 0.0 1.0))
    (set-output uv st)))

(define-shader sky-fragment
    (:stage :fragment
     :inputs ((uv :vec2 :location 0))
     :outputs ((color :vec4 :location 0))
     :resources ((sky :uniform-block :binding 0
                  :members ((horizon :vec4) (zenith :vec4) (sun :vec4)))))
  (let* ((height (clamp (- 1.0 (swizzle uv :y)) 0.0 1.0))
         (offset (- uv (swizzle sun :xy)))
         (base (mix (swizzle horizon :rgb) (swizzle zenith :rgb)
                    (smoothstep 0.0 0.8 height)))
         (glow (- 1.0 (smoothstep 0.0 (swizzle sun :z)
                                  (sqrt (dot offset offset))))))
    (set-output color (vec4 (+ base (* glow (vec3 1.0 0.85 0.6))) 1.0))))

(define-shader-program sky
  :vertex sky-vertex
  :fragment sky-fragment)
