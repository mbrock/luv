;;; GPU frustum culling: one invocation per instance tests its bounding
;;; sphere against six frustum planes and appends the instance's index to a
;;; list of visible instances, counted by an atomic in an indexed indirect
;;; draw's argument record.
;;;
;;; The record is five unsigned integers in Direct3D 12's
;;; D3D12_DRAW_INDEXED_ARGUMENTS order, which is also Metal's
;;; MTLDrawIndexedPrimitivesIndirectArguments: index count, instance count,
;;; first index, base vertex, first instance.  The host zeroes the instance
;;; count before the dispatch; invocation 0 writes the other four, and the
;;; draw's vertex stage reads VISIBLE[instance index] to find its instance.
;;;
;;; Planes are (normal, distance) with normals pointing into the frustum, so
;;; a sphere is outside when its centre lies more than its radius behind any
;;; plane: dot(normal, centre) + distance < -radius.

(define-shader instance-culling-compute
    (:stage :compute
     :workgroup-size (64 1 1)
     :inputs ((invocation :uvec3 :built-in :global-invocation-id))
     :resources ((frustum :uniform-block :binding 0
                  :members ((left :vec4) (right :vec4)
                            (bottom :vec4) (top :vec4)
                            (near :vec4) (far :vec4)
                            ;; x: instance count, y: indices per instance,
                            ;; z: first index, w: base vertex.
                            (draw :vec4)))
                 (instances :storage-buffer :binding 1 :element :vec4)
                 (visible :storage-buffer :binding 2 :element :uint
                          :access :read-write)
                 (arguments :storage-buffer :binding 3 :element :uint
                            :access :read-write)))
  (let* ((index (swizzle invocation :x))
         (zero (uint 0.0)))
    (when (= index zero)
      (set-buffer-element arguments zero (uint (swizzle draw :y)))
      (set-buffer-element arguments (uint 2.0) (uint (swizzle draw :z)))
      (set-buffer-element arguments (uint 3.0) (uint (swizzle draw :w)))
      (set-buffer-element arguments (uint 4.0) zero))
    (when (< index (uint (swizzle draw :x)))
      (let* ((sphere (buffer-element instances index))
             (centre (vec4 (swizzle sphere :xyz) 1.0))
             (nearest (min (dot left centre) (dot right centre)
                           (dot bottom centre) (dot top centre)
                           (dot near centre) (dot far centre))))
        (when (>= nearest (- (swizzle sphere :w)))
          (let* ((slot (atomic-add arguments (uint 1.0) (uint 1.0))))
            (set-buffer-element visible slot index)))))))

(define-shader-program instance-culling
  :compute instance-culling-compute)
