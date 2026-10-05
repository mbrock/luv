;;; A compute program: one invocation per particle advances its position by
;;; its velocity and writes it back.  Positions are a read-write storage
;;; buffer, velocities read-only; both share the buffer binding numbers with
;;; the uniform block.

(define-shader particle-advance-compute
    (:stage :compute
     :workgroup-size (64 1 1)
     :inputs ((particle :uvec3 :built-in :global-invocation-id)
              (group-size :uvec3 :built-in :workgroup-size))
     :resources ((step-state :uniform-block :binding 0
                  :members ((timing :vec4)))
                 (positions :storage-buffer :binding 1 :element :vec4
                            :access :read-write)
                 (velocities :storage-buffer :binding 2 :element :vec4)))
  (let* ((index (swizzle particle :x))
         (count (uint (swizzle timing :y)))
         (position (buffer-element positions index))
         (velocity (buffer-element velocities index))
         (moved (+ (swizzle position :xyz)
                   (* (swizzle velocity :xyz) (swizzle timing :x)))))
    (when (< index count)
      (set-buffer-element positions index
                          (vec4 moved (+ (swizzle position :w)
                                         (float (swizzle group-size :x))))))))

(define-shader-program particle-advance
  :compute particle-advance-compute)
