(in-package #:luvcraft.tests)

(shader:define-shader wgsl-position-probe
    (:stage :vertex
     :inputs ((position :vec4 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)))
  (shader:set-output clip-position position))

(define-test wgsl-is-a-structured-sibling-shader-target
  (let* ((specification (wgsl-position-probe))
         (first (wgsl:compile-wgsl specification))
         (second (wgsl:compile-wgsl specification))
         (source (wgsl:wgsl-document-source first)))
    (true (typep first 'wgsl:wgsl-document))
    (true (eq specification (wgsl:wgsl-document-specification first)))
    (true (string= source (wgsl:wgsl-document-source second)))
    (true (search "@vertex" source))
    (true (search "@builtin(position)" source))
    ;; The shared graph retains Vulkan's framebuffer-oriented clip Y. The
    ;; target ABI must undo that convention for WebGPU, as MSL does for Metal.
    (true (search "(stage_in.position * vec4<f32>(1.0f, -1.0f, 1.0f, 1.0f))"
                  source))))

(define-test integers-and-booleans-lower-to-wgsl
  ;; #CAI3RP
  (let ((source (wgsl:wgsl-document-source
                 (wgsl:compile-wgsl (integer-bits-fragment-probe)))))
    (true (search "@location(1) @interpolate(flat) cell: vec2<i32>," source))
    (true (search "vec2<i32>(4i, (-4i))" source))
    (true (search "let floored: vec2<i32> = (mod_remainder_" source))
    (true (search "select(vec2<i32>(0i), mod_divisor_" source))
    (true (search "bitcast<u32>(stage_in.uv.x)" source))
    ;; WGSL shifts by u32 counts, one per component.
    (true (search "(offset >> vec2<u32>(u32(2i)))" source))
    (true (search "select(stage_in.uv, vec2<f32>(bounded), near)" source))
    ;; && and || are scalar in WGSL; the probe's vector logic is in ALL.
    (true (search "(all(near) && (!(halved == 0i)))" source))
    (true (search "(any((!near))" source))))

(define-test matrices-lower-to-wgsl
  ;; #QEHEEE
  (let ((source
          (wgsl:wgsl-document-source
           (wgsl:compile-wgsl
            (shader:parse-shader-specification
             'wgsl-matrix-probe
             '(:stage :vertex
               :inputs ((position :vec4 :location 0))
               :outputs ((clip-position :vec4 :built-in :position))
               :resources ((frame :uniform-block :binding 0
                            :members ((view-projection :mat4) (tint :vec4)))))
             '((set-output clip-position
                (+ (* view-projection position)
                   (* position (shader:transpose view-projection))
                   (shader:column view-projection 3)))))))))
    (true (search "view_projection: mat4x4<f32>," source))
    (true (search "(frame.view_projection * stage_in.position)" source))
    (true (search "(stage_in.position * transpose(frame.view_projection))"
                  source))
    (true (search "frame.view_projection[3]" source))))

(define-test structure-values-lower-to-wgsl
  ;; #V16OXI
  (let ((source
          (wgsl:wgsl-document-source
           (wgsl:compile-wgsl
            (shader:parse-shader-specification
             'wgsl-struct-probe
             '(:stage :fragment
               :inputs ((uv :vec2 :location 0))
               :outputs ((color :vec4 :location 0)))
             '((let* ((segment (make-probe-segment
                                :start (vec4 uv 0.0 1.0)
                                :end (vec4 1.0 1.0 0.0 1.0))))
                 (set-output color (- (probe-segment-end segment)
                                      (probe-segment-start segment))))))))))
    (true (search "struct ProbeSegment {
  start: vec4<f32>,
  end: vec4<f32>,
}" source))
    (true (search "ProbeSegment(vec4<f32>(stage_in.uv, 0.0f, 1.0f)" source))
    (true (search "(segment.end - segment.start)" source))))

(define-test wgsl-fragments-take-built-ins-discard-and-nested-bindings
  (let* ((specification
           (shader:parse-shader-specification
            'wgsl-fragment-built-in-probe
            '(:stage :fragment
              :inputs ((pixel :vec4 :built-in :frag-coord)
                       (front :bool :built-in :front-facing))
              :outputs ((color :vec4 :location 0)
                        (depth :float :built-in :frag-depth)))
            '((let* ((shade (if front 1.0 0.5)))
                (when (< (swizzle pixel :x) 0.5)
                  (shader:discard))
                (let* ((shade (* shade 2.0)))
                  (set-output color (vec4 shade shade shade 1.0))
                  (set-output depth (swizzle pixel :z)))))))
         (source (wgsl:wgsl-document-source (wgsl:compile-wgsl specification))))
    (true (search "@builtin(position) pixel: vec4<f32>" source))
    (true (search "@builtin(front_facing) front: bool" source))
    (true (search "@builtin(frag_depth) depth: f32" source))
    (true (search "discard;" source))
    (true (search "let shade_2: f32 = (shade * 2.0f);" source))))

(define-test block-vertex-lowers-projective-map-to-wgsl
  (let ((source (wgsl:wgsl-document-source
                 (wgsl:compile-wgsl
                  (shaders:block-world-vertex-specification)))))
    (true (search "let shadow_projection: vec3<f32> =" source))
    (true (search "dot(frame_state.shadow_row_x, vec4<f32>(stage_in.world_position, 1.0f))"
                  source))
    (true (search "vec3<f32>(0.5f, 0.5f, 1.0f)" source))
    (true (search "vec3<f32>(0.5f, 0.5f, 0.0f)" source))
    (true (search "result.clip_position = (clip * vec4<f32>(1.0f, -1.0f, 1.0f, 1.0f));"
                  source))
    (true (search "result.shadow_uv_output = shadow_projection.xy;" source))))

(define-test wgsl-places-binding-families-and-refuses-what-it-lacks
  (flet ((place (declaration)
           ;; One bind group per binding family.
           (values (position (shader:shader-resource-family declaration)
                             '(:buffer :texture :storage-texture :sampler))
                   (shader:shader-resource-binding declaration))))
    (let ((source (wgsl:wgsl-document-source
                   (wgsl:compile-wgsl (texture-kinds-fragment-probe)
                                      :resource-binding #'place))))
      (true (search "@group(1) @binding(1) var cascades: texture_depth_2d_array;"
                    source))
      (true (search "@group(3) @binding(3) var shadow: sampler_comparison;"
                    source))
      (true (search "textureSampleLevel(layers, linear_clamp, stage_in.uv, layer, 2.0f)"
                    source))
      (true (search "textureSampleCompareLevel(cascades, shadow, stage_in.uv, layer, 0.5f)"
                    source)))
    ;; A WebGPU group numbers its bindings once.  Declarations that number
    ;; each family from zero need a caller to place them.
    (true (eq :wgsl-binding-collision
              (effect-failure-reason
               (lambda ()
                 (wgsl:compile-wgsl (texture-kinds-fragment-probe))))))
    ;; Standard WGSL has neither waves nor task and mesh stages.
    (true (eq :unsupported-wgsl-wave-operation
              (effect-failure-reason
               (lambda ()
                 (wgsl:compile-wgsl (workgroup-effects-probe)
                                    :resource-binding #'place)))))
    (true (eq :unsupported-wgsl-stage
              (effect-failure-reason
               (lambda ()
                 (wgsl:compile-wgsl (vulkan-task-probe))))))))
