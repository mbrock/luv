(in-package #:luvcraft.tests)

(spv:define-shader msl-unsigned-texel-fold-probe
    (:stage :fragment
     :resources ((band-data :uint-texture-2d :binding 0)
                 (curve-data :texture-2d :binding 1))
     :outputs ((color :vec4 :location 0)))
  (let* ((origin (spv:uvec2 (spv:uint 0.0) (spv:uint 0.0)))
         (header (spv:texel-load band-data origin))
         (count (spv:swizzle header :x))
         (offset (spv:swizzle header :y))
         (seed (spv:swizzle (spv:texel-load curve-data origin) :x))
         (total
           (spv:counted-fold (index count sum seed)
             (let* ((address (+ offset index))
                    (location
                      (spv:uvec2 (mod address (spv:uint 4096.0))
                                 (/ address (spv:uint 4096.0))))
                    (word
                      (spv:swizzle (spv:texel-load band-data location) :x)))
               (+ sum (float word))))))
    (spv:set-output color (spv:vec4 total total total 1.0))))

(spv:define-shader msl-vertex-index-probe
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((clip-position :vec4 :built-in :position)))
  (let* ((x (spv:float (mod vertex-index (spv:uint 2.0)))))
    (spv:set-output clip-position (spv:vec4 x 0.0 0.0 1.0))))

(spv:define-task-payload msl-task-mesh-payload
  (payload-site :uint64)
  (payload-position (:array :vec4 32)))

(spv:define-shader msl-task-probe
    (:stage :task
     :workgroup-size (32 1 1)
     :payload msl-task-mesh-payload
     :inputs ((lane :uint :built-in :local-invocation-index)
              (local-id :uvec3 :built-in :local-invocation-id)
              (group :uvec3 :built-in :workgroup-id)
              (group-count :uvec3 :built-in :num-workgroups)
              (threads :uvec3 :built-in :workgroup-size)))
  (let* ((three (spv:uint 3.0))
         (one (spv:uint 1.0))
         (position
           (if (= lane (spv:uint 0.0))
               (spv:vec4 -0.8 -0.8 0.0 1.0)
               (if (= lane one)
                   (spv:vec4 0.8 -0.8 0.0 1.0)
                   (spv:vec4 0.0 0.8 0.0 1.0)))))
    (when (= lane (spv:uint 0.0))
      (spv:set-payload payload-site (spv:uint64 three)))
    (spv:set-payload-element
     payload-position lane position)
    (spv:emit-mesh-workgroups (spv:uvec3 one one one))))

(spv:define-shader msl-mesh-probe
    (:stage :mesh
     :workgroup-size (32 1 1)
     :payload msl-task-mesh-payload
     :inputs ((lane :uint :built-in :local-invocation-index)
              (group :uvec3 :built-in :workgroup-id))
     :mesh-output
     (:topology :triangles
      :max-vertices 32
      :max-primitives 16
      :vertex ((position :vec4 :built-in :position)
               (uv :vec2 :location 0))
      :primitive ((primitive-color :vec4 :location 1))))
  (let* ((vertex-count (spv:uint payload-site))
         (primitive-count (spv:uint 1.0)))
    (spv:set-mesh-output-counts vertex-count primitive-count)
    (when (< lane vertex-count)
      (spv:set-mesh-vertex
       lane
       (position (spv:payload-element payload-position lane))
       (uv (spv:vec2 0.0 0.0))))
    (when (= lane (spv:uint 0.0))
      (spv:set-mesh-primitive
       (spv:uint 0.0)
       (spv:uvec3 (spv:uint 0.0)
                  (spv:uint 1.0)
                  (spv:uint 2.0))
       (primitive-color (spv:vec4 1.0 1.0 1.0 1.0))))))

(spv:define-shader msl-mesh-fragment-probe
    (:stage :fragment
     :outputs ((color :vec4 :location 0)))
  (spv:set-output color (spv:vec4 0.25 0.5 0.75 1.0)))

(defun msl-binding-named (name specification)
  (find name (spv:shader-specification-bindings specification)
        :key #'spv:shader-object-name
        :test (lambda (left right)
                (string-equal (symbol-name left) (symbol-name right)))))

(defun msl-named (name objects name-function)
  (find name objects :key name-function :test #'string=))

(deftest block-fragment-lowers-directly-to-structured-msl
  (let* ((specification (shaders:block-world-fragment-specification))
         (document (msl:compile-msl specification))
         (source (msl:msl-document-source document)))
    (ok (typep document 'msl:msl-document))
    (ok (eq specification (msl:msl-document-specification document)))
    (ok (= (length (msl:msl-document-declarations document)) 3))
    (ok (typep (msl:msl-document-entry-point document)
               'msl:msl-entry-point))
    (ok (search "fragment BlockWorldFragmentSpecificationOutput" source))
    (ok (search "BlockWorldFragmentSpecificationInput stage_in [[stage_in]]"
                source))
    (ok (search "texture2d<float> block_atlas [[texture(0)]]" source))
    (ok (search "sampler block_sampler [[sampler(1)]]" source))
    (ok (search "constant FrameState& frame_state [[buffer(2)]]" source))
    (ok (search "depth2d<float> shadow_map [[texture(3)]]" source))
    (ok (search "shadow_map.sample_compare" source))
    (ok (search "result.color_output = rgba;" source))))

(deftest exact-unsigned-texel-fold-lowers-directly-to-metal
  (let ((source
          (msl:msl-document-source
           (msl:compile-msl (msl-unsigned-texel-fold-probe)))))
    (ok (search "texture2d<uint> band_data [[texture(0)]]" source))
    (ok (search "band_data.read" source))
    (ok (search "for (uint fold_index_1 = 0u;" source))
    (ok (search "% uint(4096.0f)" source))
    (ok (search "/ uint(4096.0f)" source))
    (ok (search "float(" source))))

(deftest vertex-index-is-a-direct-metal-built-in-parameter
  (let ((source
          (msl:msl-document-source
           (msl:compile-msl (msl-vertex-index-probe)))))
    (ok (search "uint vertex_index [[vertex_id]]" source))
    (ok (search "vertex_index % uint(2.0f)" source))
    (ng (search "stage_in.vertex_index" source))
    (ng (search "[[stage_in]]" source))))

(deftest task-and-mesh-specifications-retain-their-workgroup-contracts
  (let* ((task (msl-task-probe))
         (mesh (msl-mesh-probe))
         (payload (spv:shader-specification-task-payload task))
         (mesh-output (spv:shader-specification-mesh-output mesh)))
    (ok (equal '(32 1 1) (spv:shader-specification-workgroup-size task)))
    (ok (eq payload (spv:shader-specification-task-payload mesh)))
    (ok (= 2 (length (spv:shader-task-payload-fields payload))))
    (ok (= 32
           (spv:shader-task-payload-field-element-count
            (second (spv:shader-task-payload-fields payload)))))
    (ok (eq :triangles (spv:shader-mesh-output-topology mesh-output)))
    (ok (= 32 (spv:shader-mesh-output-max-vertices mesh-output)))
    (ok (= 16 (spv:shader-mesh-output-max-primitives mesh-output)))
    (ok (typep (first (spv:shader-specification-statements mesh))
               'spv:shader-mesh-output-counts))))

(deftest task-and-mesh-effects-enforce-stage-and-collective-legality
  (flet ((failure-reason (name options body)
           (handler-case
               (progn
                 (spv:parse-shader-specification name options body)
                 nil)
             (spv:shader-language-error (condition)
               (spv:shader-language-error-reason condition)))))
    (ok (eq :mesh-output-counts-not-uniform
            (failure-reason
             'varying-mesh-counts
             '(:stage :mesh
               :workgroup-size (32 1 1)
               :inputs ((lane :uint :built-in :local-invocation-index))
               :mesh-output
               (:topology :triangles
                :max-vertices 32 :max-primitives 16
                :vertex ((position :vec4 :built-in :position))))
             '((let* ((one (spv:uint 1.0)))
                 (spv:set-mesh-output-counts lane one)
                 (spv:set-mesh-vertex
                  lane (position (spv:vec4 0.0 0.0 0.0 1.0)))
                 (spv:set-mesh-primitive
                  (spv:uint 0.0)
                  (spv:uvec3 (spv:uint 0.0)
                             (spv:uint 0.0)
                             (spv:uint 0.0))))))))
    (ok (eq :mesh-workgroups-emission-must-be-last
            (failure-reason
             'nonterminal-task-emission
             '(:stage :task
               :workgroup-size (1 1 1)
               :payload msl-task-mesh-payload
               :inputs ((lane :uint :built-in :local-invocation-index)))
             '((let* ((one (spv:uint 1.0)))
                 (spv:emit-mesh-workgroups (spv:uvec3 one one one))
                 (spv:set-payload payload-site (spv:uint64 one)))))))
    (ok (eq :invalid-statement-for-stage
            (failure-reason
             'task-vertex-write
             '(:stage :task
               :workgroup-size (1 1 1)
               :inputs ((lane :uint :built-in :local-invocation-index)))
             '((spv:set-mesh-vertex
                lane (position (spv:vec4 0.0 0.0 0.0 1.0)))))))
    (ok (eq :mesh-output-count-exceeds-limit
            (failure-reason
             'oversized-mesh-count
             '(:stage :mesh
               :workgroup-size (1 1 1)
               :inputs ((lane :uint :built-in :local-invocation-index))
               :mesh-output
               (:topology :triangles
                :max-vertices 32 :max-primitives 16
                :vertex ((position :vec4 :built-in :position))))
             '((let* ((vertices (spv:uint 33.0))
                      (primitives (spv:uint 1.0)))
                 (spv:set-mesh-output-counts vertices primitives)
                 (spv:set-mesh-vertex
                  lane (position (spv:vec4 0.0 0.0 0.0 1.0)))
                 (spv:set-mesh-primitive
                  (spv:uint 0.0)
                  (spv:uvec3 (spv:uint 0.0)
                             (spv:uint 0.0)
                             (spv:uint 0.0))))))))
    (ok (eq :payload-index-out-of-bounds
            (failure-reason
             'payload-overrun
             '(:stage :task
               :workgroup-size (1 1 1)
               :payload msl-task-mesh-payload
               :inputs ((lane :uint :built-in :local-invocation-index)))
             '((let* ((one (spv:uint 1.0)))
                 (spv:set-payload-element
                  payload-position (spv:uint 32.0)
                  (spv:vec4 0.0 0.0 0.0 1.0))
                 (spv:emit-mesh-workgroups
                  (spv:uvec3 one one one)))))))))

(deftest task-and-mesh-lower-to-metal-object-and-mesh-entry-points
  (let ((task-source
          (msl:msl-document-source (msl:compile-msl (msl-task-probe))))
        (mesh-source
          (msl:msl-document-source (msl:compile-msl (msl-mesh-probe)))))
    (ok (search "struct MslTaskMeshPayload" task-source))
    (ok (search "ulong payload_site;" task-source))
    (ok (search "payload.payload_site = ulong(three);" task-source))
    (ok (search "float4 payload_position[32];" task-source))
    (ok (search "[[object]] void msl_task_probe(" task-source))
    (ok (search "object_data MslTaskMeshPayload& payload [[payload]]"
                task-source))
    (ok (search "uint lane [[thread_index_in_threadgroup]]" task-source))
    (ok (search "uint3 local_id [[thread_position_in_threadgroup]]"
                task-source))
    (ok (search "uint3 group [[threadgroup_position_in_grid]]" task-source))
    (ok (search "uint3 group_count [[threadgroups_per_grid]]" task-source))
    (ok (search "uint3 threads [[threads_per_threadgroup]]" task-source))
    (ok (search "payload.payload_position[lane]" task-source))
    (ok (search "mesh_grid.set_threadgroups_per_grid" task-source))
    (ok (search "[[mesh]] void msl_mesh_probe(" mesh-source))
    (ok (search
         "metal::mesh<MslMeshProbeVertex, MslMeshProbePrimitive, 32, 16, metal::topology::triangle> mesh_out"
         mesh-source))
    (ok (search "object_data const MslTaskMeshPayload& payload [[payload]]"
                mesh-source))
    (ok (search "uint(payload.payload_site)" mesh-source))
    (ok (search "mesh_out.set_primitive_count(primitive_count);" mesh-source))
    (ok (search "mesh_out.set_vertex(lane, MslMeshProbeVertex{" mesh-source))
    (ok (search "mesh_out.set_index" mesh-source))
    (ok (search "mesh_out.set_primitive" mesh-source))))

(deftest slug-atlas-derivatives-and-band-selection-lower-to-metal
  (let ((fragment-source
          (msl:msl-document-source
           (msl:compile-msl
            (shaders:block-world-text-fragment-specification))))
        (vertex-source
          (msl:msl-document-source
           (msl:compile-msl
            (shaders:block-world-text-vertex-specification)))))
    (ok (search "dfdx(stage_in.render_coordinate)" fragment-source))
    (ok (search "dfdy(stage_in.render_coordinate)" fragment-source))
    (ok (search "stage_in.render_coordinate.y - stage_in.band_bounds.y"
                fragment-source))
    (ok (search "stage_in.render_coordinate.x - stage_in.band_bounds.x"
                fragment-source))
    (ok (search "band_base + horizontal_band" fragment-source))
    (ok (search "band_base + horizontal_band_count" fragment-source))
    (ok (search
         "result.render_band_bounds = float4(stage_in.band_low.xy, stage_in.band_high.xy)"
         vertex-source))))

(deftest block-vertex-lowers-projective-map-to-msl
  (let* ((specification (shaders:block-world-vertex-specification))
         (document (msl:compile-msl specification))
         (source (msl:msl-document-source document))
         (binding (msl-binding-named 'shadow-projection specification))
         (expression (spv:shader-binding-expression binding)))
    (ok (search "vertex BlockWorldVertexSpecificationOutput" source))
    (ok (search "float3 shadow_projection =" source))
    (ok (search "dot(frame_state.shadow_row_x, float4(stage_in.world_position, 1.0f))"
                source))
    (ok (search "float3(0.5f, 0.5f, 1.0f)" source))
    (ok (search "float3(0.5f, 0.5f, 0.0f)" source))
    ;; The shared camera graph uses Vulkan framebuffer orientation.  Metal's
    ;; target boundary owns the one required clip-space Y conversion.
    (ok (search
         "result.clip_position = float4((clip).x, -(clip).y, (clip).z, (clip).w);"
         source))
    (ok (search "result.shadow_depth_output = shadow_depth;" source))
    (ok (gethash expression
                 (msl:msl-document-expression-occurrences document)))))

(deftest msl-lowering-is-deterministic-and-does-not-perturb-spir-v
  (let* ((specification (shaders:block-world-fragment-specification))
         (spir-v-before (spv:assemble-shader-specification specification))
         (first (msl:msl-document-source (msl:compile-msl specification)))
         (second (msl:msl-document-source (msl:compile-msl specification)))
         (spir-v-after (spv:assemble-shader-specification specification)))
    (ok (string= first second))
    (ok (equalp spir-v-before spir-v-after))
    (ok (typep (spv:lower-shader-specification :spir-v specification)
               'spv:shader-lowering))))

(deftest msl-occurrences-retain-expression-provenance
  (let* ((specification (shaders:block-world-fragment-specification))
         (document (msl:compile-msl specification))
         (binding (msl-binding-named 'reflected specification))
         (expression (spv:shader-binding-expression binding))
         (occurrences
           (gethash expression
                    (msl:msl-document-expression-occurrences document))))
    (ok occurrences)
    (ok (every (lambda (occurrence)
                 (eq expression
                     (gethash occurrence
                              (msl:msl-document-occurrence-expression
                               document))))
               occurrences))
    (ok (some (lambda (occurrence)
                (search "albedo" (msl:msl-source-occurrence-text occurrence)))
              occurrences))))

(deftest generated-msl-explains-quantities-in-plain-language
  (let* ((specification (shaders:block-world-vertex-specification))
         (document (msl:compile-msl specification))
         (source (msl:msl-document-source document))
         (input (first (spv:shader-specification-inputs specification)))
         (input-structure (first (msl:msl-document-declarations document)))
         (field
           (msl-named "world_position"
                      (msl:msl-structure-fields input-structure)
                      #'msl:msl-field-name))
         (binding (msl-binding-named 'view-z specification))
         (output
           (first (spv:shader-specification-statements specification)))
         (statement
           (msl-named
            "view_z"
            (msl:msl-entry-point-statements
             (msl:msl-document-entry-point document))
            (lambda (statement)
              (and (typep statement 'msl:msl-variable-statement)
                   (msl:msl-variable-statement-name statement))))))
    (ok (search
         "World position is a point-valued vector in the lattice coordinate kind, measured in cell units, and dimensionless."
         source))
    (ok (search
         "The xy lanes hold texture uv as a point-valued vector"
         source))
    (ok (search "This numeric value has no quantity annotation." source))
    (ok (search
         "View distance is a difference-valued scalar in the lattice coordinate kind, measured in cell units"
         source))
    (ok (eq input (msl:msl-field-origin field)))
    (ok (eq binding (msl:msl-variable-statement-origin statement)))
    (ok (eq output
            (msl:msl-output-statement-origin
             (find-if (lambda (statement)
                        (typep statement 'msl:msl-output-statement))
                      (msl:msl-entry-point-statements
                       (msl:msl-document-entry-point document))))))))

(deftest texture-parameters-describe-their-sampled-quantity-layout
  (let* ((specification (shaders:block-world-fragment-specification))
         (document (msl:compile-msl specification))
         (source (msl:msl-document-source document))
         (resource
           (find "BLOCK-ATLAS"
                 (spv:shader-specification-resources specification)
                 :key (lambda (resource)
                        (symbol-name (spv:shader-object-name resource)))
                 :test #'string=))
         (parameter
           (msl-named
            "block_atlas"
            (msl:msl-entry-point-parameters
             (msl:msl-document-entry-point document))
            #'msl:msl-parameter-name)))
    (ok (search "The sampled xyz lanes hold linear rgb" source))
    (ok (search "The sampled w lane holds surface relief" source))
    (ok (eq resource (msl:msl-parameter-origin parameter)))))

(deftest target-context-precedes-operator-identity
  (ok (equal
       (mapcar #'symbol-name
               (closer-mop:generic-function-argument-precedence-order
                #'spv:lower-shader-call))
       '("CONTEXT" "OPERATOR" "EXPRESSION"))))

(deftest slug-proof-lowers-to-direct-metal-pixel-mathematics
  (let* ((document
           (msl:compile-msl (slug:slug-bezier-fragment-specification)))
         (source (msl:msl-document-source document)))
    (ok (search "fragment SlugBezierFragmentSpecificationOutput" source))
    (ok (search "sign(" source))
    (ok (search "sqrt(" source))
    (ok (search "result.color_output" source))))

(deftest analytic-roundrect-lowers-to-direct-metal-distance-mathematics
  (let* ((document
           (msl:compile-msl
            (luv.analytic:roundrect-fragment-specification)))
         (source (msl:msl-document-source document)))
    (ok (search "fragment RoundrectFragmentSpecificationOutput" source))
    (ok (search "dfdx(" source))
    (ok (search "dfdy(" source))
    (ok (search "sqrt(" source))
    (ok (search "result.color_output" source))))

(deftest slug-band-function-lexicals-stay-inside-their-metal-loops
  (let* ((source
           (msl:msl-document-source
            (msl:compile-msl
             (slug:slug-banded-fragment-specification))))
         (first-loop (search "for (uint fold_index_1" source))
         (first-local
           (search "_local_1_axis_a" source))
         (first-update
           (search "fold_state_1 = float2((fold_state_1.x" source)))
    (ok (search "texture2d<uint> band_data" source))
    (ok (search "texture2d<float> curve_data" source))
    (ok first-loop)
    (ok (search "for (uint fold_index_2" source))
    (ok (< first-loop first-local first-update))))

(deftest counted-fold-lowers-to-a-direct-metal-loop
  (let* ((specification
           (spv:parse-shader-specification
            'metal-fold-probe
            '(:stage :fragment
              :inputs ((count :float :location 0))
              :outputs ((result :float :location 0)))
            '((spv:set-output result
                              (spv:counted-fold
                                  (index count sum 0.0)
                                (+ sum index))))))
         (source
           (msl:msl-document-source (msl:compile-msl specification))))
    (ok (search "float fold_state_1 = 0.0f;" source))
    (ok (search "for (float fold_index_1 = 0.0f;" source))
    (ok (search "fold_state_1 = (fold_state_1 + fold_index_1);" source))
    (ok (search "result.result = fold_state_1;" source))))

(deftest shared-conditionals-lower-inside-direct-metal-folds
  (let* ((specification
           (spv:parse-shader-specification
            'metal-conditional-fold-probe
            '(:stage :fragment
              :inputs ((count :float :location 0)
                       (limit :float :location 1))
              :outputs ((result :float :location 0)))
            '((spv:set-output result
                              (spv:counted-fold
                                  (index count sum 0.0)
                                (if (< index limit)
                                    (+ sum index)
                                    sum))))))
         (source
           (msl:msl-document-source (msl:compile-msl specification))))
    (ok (search "fold_index_1 < stage_in.limit" source))
    (ok (search "? (fold_state_1 + fold_index_1) : fold_state_1" source))))

(deftest unsupported-msl-boundaries-retain-source-reasons
  (flet ((reason-for (specification)
           (handler-case
               (progn (msl:compile-msl specification) nil)
             (spv:shader-language-error (condition)
               (spv:shader-language-error-reason condition)))))
    (let ((compute
            (spv:parse-shader-specification
             'compute-probe
             '(:stage :compute :outputs ((value :float :location 0)))
             '((spv:set-output value 1.0))))
          (descriptor-set
            (spv:parse-shader-specification
             'descriptor-set-probe
             '(:stage :fragment
               :outputs ((value :float :location 0))
               :resources ((image :texture-2d :set 1 :binding 0)))
             '((spv:set-output value 1.0)))))
      (ok (eq :unsupported-msl-stage (reason-for compute)))
      (ok (eq :unsupported-msl-descriptor-set
              (reason-for descriptor-set))))))

(deftest storage-buffers-and-bit-fields-lower-to-device-pointers-and-shifts
  (let ((source
          (msl:msl-document-source
           (msl:compile-msl (storage-site-fragment-probe)))))
    (ok (search "const device ulong* sites [[buffer(1)]]" source))
    (ok (search "const device uint4* words [[buffer(2)]]" source))
    (ok (search "ulong term = sites[uint(3.0f)];" source))
    (ok (search "words[extent].x" source))
    ;; A 64-bit field masks with a 64-bit literal, a 32-bit one with a
    ;; 32-bit literal, and a field reaching the top bit needs no mask.
    (ok (search "((term >> 4ul) & 0xFFFFFFul)" source))
    (ok (search "((term >> 0ul) & 0xFul)" source))
    (ok (search "(word >> 16u)" source))
    (ok (search "uint whole = (word);" source))))
