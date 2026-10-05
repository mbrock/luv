;;; A compute program over structured particles.  Each particle is one
;;; storage-buffer element of a host-shareable structure (its C++ twin is
;;; written into the header with its size and every offset asserted); a
;;; camera-style uniform block carries a mat4; and the advance itself is a
;;; shader function from structure to structure, with signed cells, bit
;;; flags, and a boolean.

(define-shader-struct particle
  (position :vec4)
  (velocity :vec4)
  (orientation :mat4)
  (cell :ivec2)
  (age :float)
  (flags :uint))

(define-shader-function advance-particle (particle delta gravity)
  (let* ((velocity (+ (particle-velocity particle) (* gravity delta)))
         (position (+ (particle-position particle) (* velocity delta)))
         (alive (< (particle-age particle) 10.0)))
    (make-particle :position position
                   :velocity velocity
                   :orientation (particle-orientation particle)
                   :cell (ivec2 (floor (swizzle position :xz)))
                   :age (+ (particle-age particle) delta)
                   :flags (logior (particle-flags particle)
                                  (if alive (uint 0.0) (uint 1.0))))))

(define-shader particle-swarm-compute
    (:stage :compute
     :workgroup-size (64 1 1)
     :inputs ((thread :uvec3 :built-in :global-invocation-id))
     :resources ((swarm :uniform-block :binding 0
                  :members ((world :mat4) (timing :vec4)))
                 (particles :storage-buffer :binding 1 :element particle
                            :access :read-write)))
  (let* ((index (swizzle thread :x))
         (count (uint (swizzle timing :y)))
         (gravity (* world (vec4 0.0 -9.8 0.0 0.0))))
    (when (< index count)
      (set-buffer-element
       particles index
       (advance-particle (buffer-element particles index)
                         (swizzle timing :x) gravity)))))

(define-shader-program particle-swarm
  :compute particle-swarm-compute)
