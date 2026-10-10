;;;; Executable claims for the WGSL lowering.
;;;;
;;;; Text claims pin the dialect choices; when Dawn is at hand, every probe
;;;; is also compiled by it, as Chrome would.  Dawn is the `webgpu` npm
;;;; package under node, found as scripts/wgsl-validate.mjs describes: set
;;;; LUV_WEBGPU to a directory where `npm install webgpu` was run.

(defpackage #:luv.wgsl.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:is)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:wgsl #:luv.wgsl)))

(in-package #:luv.wgsl.tests)

(defparameter *validator*
  (merge-pathnames "scripts/wgsl-validate.mjs"
                   (asdf:system-source-directory "luv/wgsl")))

(defvar *dawn-command* :unknown
  "The command validating WGSL files with Dawn, NIL when node or Dawn is
missing, or :UNKNOWN before the first look.")

(defun dawn-command ()
  (when (eq *dawn-command* :unknown)
    (let ((command (list (or (uiop:getenv "LUV_NODE") "node")
                         (uiop:native-namestring *validator*))))
      (setf *dawn-command*
            (and (ignore-errors
                  (zerop (nth-value
                          2 (uiop:run-program (append command '("--probe"))
                                              :ignore-error-status t))))
                 command))))
  *dawn-command*)

(defun dawn-diagnostics (document)
  "Compile DOCUMENT with Dawn.  Return NIL on success, else Dawn's report.
Without Dawn, return NIL: the text claims still hold."
  (let ((command (dawn-command)))
    (when command
      (uiop:with-temporary-file (:pathname source :type "wgsl" :keep nil)
        (wgsl:write-wgsl document source)
        (multiple-value-bind (output error-output status)
            (uiop:run-program
             (append command (list (uiop:native-namestring source)))
             :output :string :error-output :string :ignore-error-status t)
          (unless (zerop status)
            (format nil "~A~A~%~A" output error-output
                    (wgsl:wgsl-document-source document))))))))

(defmacro compiles (document)
  `(is eq nil (dawn-diagnostics ,document)))

(defun source-of (specification &rest arguments)
  (let ((document (apply #'wgsl:compile-wgsl specification arguments)))
    (values (wgsl:wgsl-document-source document) document)))

(defun failure-reason (thunk)
  (handler-case (progn (funcall thunk) nil)
    (shader:shader-language-error (condition)
      (shader:shader-language-error-reason condition))))

(defun family-groups (declaration)
  "Buffers, textures, and samplers each in a bind group of their own."
  (values (ecase (shader:shader-resource-family declaration)
            (:buffer 0)
            (:texture 1)
            (:storage-texture 1)
            (:sampler 2))
          (+ (shader:shader-resource-binding declaration)
             (if (eq :storage-texture
                     (shader:shader-resource-family declaration))
                 16
                 0))))

(defun placed-source-of (specification &rest arguments)
  (apply #'source-of specification :resource-binding #'family-groups
         arguments))

(shader:define-shader wgsl-pulling-vertex-probe
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index)
              (instance-index :uint :built-in :instance-index))
     :outputs ((clip-position :vec4 :built-in :position)
               (uv :vec2 :location 1)
               (tile :uint :location 0))
     :resources ((frame :uniform-block :binding 0
                  :members ((offset :vec4) (scale :vec4)))
                 (corners :storage-buffer :binding 1 :element :vec4)
                 (heights :texture-2d :binding 0)
                 (linear-clamp :sampler :binding 0)))
  (let* ((corner (shader:buffer-element corners vertex-index))
         (height (shader:swizzle
                  (shader:sample heights linear-clamp
                                 (shader:swizzle corner :xy))
                  :r)))
    (shader:set-output clip-position
                       (+ (* corner (shader:swizzle scale :x))
                          (shader:vec4 0.0 height 0.0 0.0)
                          offset))
    (shader:set-output uv (shader:swizzle corner :zw))
    (shader:set-output tile instance-index)))

(shader:define-shader wgsl-shading-fragment-probe
    (:stage :fragment
     :inputs ((uv :vec2 :location 1)
              (shadow :vec3 :location 2)
              (tile :uint :location 0))
     :outputs ((color :vec4 :location 0)
               (glow :vec4 :location 1))
     :resources ((albedo :texture-2d :binding 0)
                 (shadow-map :depth-texture-2d :binding 1)
                 (nearest-clamp :sampler :binding 2)
                 (linear-clamp :sampler :binding 0)
                 (shadow-compare :sampler :binding 3)))
  (let* ((base (shader:sample albedo linear-clamp uv))
         (depth (shader:sample shadow-map nearest-clamp uv))
         (lit (shader:sample-compare shadow-map shadow-compare
                                     (shader:swizzle shadow :xy)
                                     (shader:swizzle shadow :z)))
         (ripple (shader:fract (* (sin (shader:swizzle uv :x)) 43758.5)))
         (bent (shader:mix (shader:swizzle base :rgb)
                           (shader:normalize (shader:swizzle base :gbr))
                           ripple))
         (edge (shader:smoothstep 0.1 0.9 (shader:swizzle uv :y)))
         (cut (shader:step 0.5 (shader:swizzle uv :x)))
         (slope (+ (abs (shader:derivative-x (shader:swizzle uv :x)))
                   (abs (shader:derivative-y (shader:swizzle uv :y)))))
         (sign (signum (- (shader:swizzle uv :x) 0.5)))
         (curve (expt (max (shader:swizzle uv :x) 0.001 slope) 2.2))
         (fog (exp (- (log (+ 1.0 (sqrt (min edge cut)))))))
         (wrap (floor (* (cos (shader:swizzle uv :y)) 4.0)))
         (spread (shader:swizzle ripple :xxx)))
    (when (> lit 0.5)
      (shader:set-output glow (shader:vec4 (* lit spread) (float tile))))
    (shader:set-output
     color
     (shader:vec4 (* bent (shader:clamp (* curve fog) 0.0 1.0))
                  (if (< sign 0.0)
                      (+ wrap (shader:swizzle depth :x))
                      (shader:dot (shader:swizzle base :rgb)
                                  (shader:vec3 0.2 0.7 0.1)))))))

(shader:define-shader wgsl-bit-field-probe
    (:stage :fragment
     :outputs ((color :vec4 :location 0))
     :resources ((words :storage-buffer :binding 2 :element :uvec4)
                 (band-data :uint-texture-2d :binding 0)
                 (depth :depth-texture-2d :binding 1)))
  (let* ((word (shader:swizzle (shader:buffer-element words (shader:uint 0.0))
                               :x))
         (header (shader:texel-load
                  band-data (shader:uvec2 (shader:uint 1.0) word)))
         (stored (shader:texel-load depth (shader:uvec2 word word)))
         (field (ldb (byte 24 4) word))
         (nibble (ldb (byte 4 0) word))
         (top (ldb (byte 16 16) word))
         (placed (ldb (byte 3 nibble) word))
         (count (shader:swizzle header :x))
         (total
           (shader:counted-fold (index count sum 0.0)
             (+ sum (float (mod (+ index top) (shader:uint 7.0)))))))
    (shader:set-output
     color
     (shader:vec4 (float field) (float (+ nibble placed))
                  total (shader:swizzle stored :x)))))

(shader:define-shader-function wgsl-reused-local (first second)
  (let* ((sum (+ first first)))
    (+ sum second)))

(shader:define-shader wgsl-inline-probe
    (:stage :fragment
     :inputs ((value :float :location 0))
     :outputs ((result :float :location 0)))
  (shader:set-output
   result
   (wgsl-reused-local (wgsl-reused-local value 1.0) 2.0)))

(shader:define-shader wgsl-early-fold-probe
    (:stage :fragment
     :inputs ((count :float :location 0)
              (limit :float :location 1))
     :outputs ((result :float :location 0)))
  (shader:set-output
   result
   (shader:counted-fold (index count sum 0.0 :until (> sum limit))
     (if (< index limit) (+ sum index) sum))))

(define-test vertex-pulling-lowers-to-wgsl-bindings-and-built-ins
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-pulling-vertex-probe))
    (true (search "@builtin(vertex_index) vertex_index: u32," source))
    (true (search "@builtin(instance_index) instance_index: u32," source))
    (true (search "struct Frame {
  offset: vec4<f32>,
  scale: vec4<f32>,
}
@group(0) @binding(0) var<uniform> frame: Frame;" source))
    (true (search "@group(0) @binding(1) var<storage, read> corners: array<vec4<f32>>;"
                  source))
    (true (search "@group(1) @binding(0) var heights: texture_2d<f32>;" source))
    (true (search "@group(2) @binding(0) var linear_clamp: sampler;" source))
    (true (search "let corner: vec4<f32> = corners[stage_in.vertex_index];"
                  source))
    (true (search "frame.offset" source))
    ;; Only fragments have implicit derivatives to choose a level by.
    (true (search "textureSampleLevel(heights, linear_clamp, corner.xy, 0.0f)"
                  source))
    ;; Integer varyings never interpolate, and WebGPU makes them say so.
    (true (search "@location(0) @interpolate(flat) tile: u32," source))
    (true (search "@location(1) uv: vec2<f32>," source))
    ;; Clip Y flips exactly as in MSL: the shared graph is Vulkan-oriented.
    (true (search "* vec4<f32>(1.0f, -1.0f, 1.0f, 1.0f));" source))
    (true (search "@vertex
fn wgsl_pulling_vertex_probe(stage_in: WgslPullingVertexProbeInput) -> WgslPullingVertexProbeOutput {"
                  source))
    (is string= "wgsl_pulling_vertex_probe"
        (wgsl:wgsl-document-entry-point-name document))
    (compiles document))
  ;; A group numbers its bindings once, whatever their kinds.  Declarations
  ;; that number each family from zero need a caller to place them.
  (is eq :wgsl-binding-collision
      (failure-reason
       (lambda () (wgsl:compile-wgsl (wgsl-pulling-vertex-probe))))))

(define-test fragment-operators-lower-to-wgsl-built-ins
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-shading-fragment-probe))
    ;; Fragment modules sample and differentiate wherever the source does.
    (true (search "diagnostic(off, derivative_uniformity);" source))
    (true (search "@group(1) @binding(1) var shadow_map: texture_depth_2d;"
                  source))
    (true (search "@group(2) @binding(3) var shadow_compare: sampler_comparison;"
                  source))
    (true (search "textureSample(albedo, linear_clamp, stage_in.uv)" source))
    ;; A depth texture samples one f32; the language's value is a vec4.
    (true (search "vec4<f32>(textureSample(shadow_map, nearest_clamp, stage_in.uv))"
                  source))
    (true (search "textureSampleCompareLevel(shadow_map, shadow_compare, stage_in.shadow.xy, stage_in.shadow.z)"
                  source))
    (true (search "fract(" source))
    (true (search "mix(base.rgb, normalize(base.gbr), ripple)" source))
    (true (search "smoothstep(0.1f, 0.9f, stage_in.uv.y)" source))
    (true (search "step(0.5f, stage_in.uv.x)" source))
    (true (search "dpdx(stage_in.uv.x)" source))
    (true (search "dpdy(stage_in.uv.y)" source))
    ;; A local may not take the name of a built-in the module calls.
    (true (search "let sign_: f32 = sign((stage_in.uv.x - 0.5f));" source))
    (true (search "pow(max(max(stage_in.uv.x, 0.001f), slope), 2.2f)" source))
    (true (search "exp((-log(" source))
    ;; A WGSL scalar has no components to swizzle.
    (true (search "let spread: vec3<f32> = vec3<f32>(ripple);" source))
    ;; WGSL has no conditional expression; select takes the false value first.
    (true (search "select(dot(base.rgb, vec3<f32>(0.2f, 0.7f, 0.1f)), (wrap + depth.x), (sign_ < 0.0f))"
                  source))
    (true (search "if ((lit > 0.5f)) {" source))
    ;; The fragment input repeats the vertex output's flat interpolation.
    (true (search "@location(0) @interpolate(flat) tile: u32," source))
    (true (search "@location(1) glow: vec4<f32>," source))
    (compiles document)))

(define-test bit-fields-texel-loads-and-folds-lower-to-integer-wgsl
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-bit-field-probe))
    (true (search "@group(0) @binding(2) var<storage, read> words: array<vec4<u32>>;"
                  source))
    (true (search "@group(1) @binding(0) var band_data: texture_2d<u32>;" source))
    (true (search "((word >> 4u) & 0xFFFFFFu)" source))
    (true (search "((word >> 0u) & 0xFu)" source))
    (true (search "(word >> 16u)" source))
    ;; A field may sit at a position known only when the shader runs.
    (true (search "((word >> nibble) & 0x7u)" source))
    (true (search "textureLoad(band_data, vec2<u32>(u32(1.0f), word), 0u)"
                  source))
    (true (search "vec4<f32>(textureLoad(depth, vec2<u32>(word, word), 0u))"
                  source))
    (true (search "for (var fold_index_1: u32 = 0u; fold_index_1 < count; fold_index_1 = fold_index_1 + 1u) {"
                  source))
    (true (search "% u32(7.0f))" source))
    ;; A stage without inputs takes no parameter: WGSL has no empty structure.
    (true (search "fn wgsl_bit_field_probe() -> WgslBitFieldProbeOutput {"
                  source))
    (compiles document)))

(define-test inline-functions-materialize-each-local-once
  (multiple-value-bind (source document) (source-of (wgsl-inline-probe))
    (let ((declaration "let wgsl_reused_local_1_local_1_sum: f32 ="))
      (true (search declaration source))
      (false (search declaration source
                     :start2 (1+ (search declaration source)))))
    ;; RESULT names the entry's output variable; a shader name escapes.
    (true (search "@location(0) result_: f32," source))
    (true (search "result.result_ = " source))
    (compiles document)))

(define-test early-exit-folds-lower-to-loops-with-breaks
  (multiple-value-bind (source document) (source-of (wgsl-early-fold-probe))
    (true (search "var fold_state_1: f32 = 0.0f;" source))
    (true (search "for (var fold_index_1: f32 = 0.0f;" source))
    (true (search "if ((fold_state_1 > stage_in.limit)) { break; }" source))
    (true (search "select(fold_state_1, (fold_state_1 + fold_index_1), (fold_index_1 < stage_in.limit))"
                  source))
    (compiles document)))

(define-test entry-point-names-and-occurrences-are-retained
  (let* ((specification (wgsl-shading-fragment-probe))
         (document (wgsl:compile-wgsl specification
                                      :entry-point-name "shading_fragment"
                                      :resource-binding #'family-groups))
         (binding (find "BENT" (shader:shader-specification-bindings
                                specification)
                        :key (lambda (binding)
                               (symbol-name (shader:shader-object-name
                                             binding)))
                        :test #'string=))
         (expression (shader:shader-binding-expression binding))
         (occurrences
           (gethash expression
                    (wgsl:wgsl-document-expression-occurrences document))))
    (true (search "fn shading_fragment(stage_in: WgslShadingFragmentProbeInput)"
                  (wgsl:wgsl-document-source document)))
    (is string= "shading_fragment"
        (wgsl:wgsl-document-entry-point-name document))
    (true occurrences)
    (true (every (lambda (occurrence)
                   (eq expression
                       (gethash occurrence
                                (wgsl:wgsl-document-occurrence-expression
                                 document))))
                 occurrences))
    (true (string= (wgsl:wgsl-document-source document)
                   (wgsl:wgsl-document-source
                    (wgsl:compile-wgsl specification
                                       :entry-point-name "shading_fragment"
                                       :resource-binding #'family-groups))))))

(define-test samplers-are-declared-as-the-whole-program-uses-them
  ;; A stage that never compares may still declare the comparison sampler,
  ;; and one bind group layout serves every stage.
  (let ((specification
          (shader:parse-shader-specification
           'wgsl-sampler-probe
           '(:stage :fragment
             :inputs ((uv :vec2 :location 0))
             :outputs ((color :vec4 :location 0))
             :resources ((albedo :texture-2d :binding 0)
                         (linear-clamp :sampler :binding 0)
                         (shadow-compare :sampler :binding 3)))
           '((shader:set-output color
              (shader:sample albedo linear-clamp uv))))))
    (true (search "var shadow_compare: sampler;"
                  (placed-source-of specification)))
    (multiple-value-bind (source document)
        (placed-source-of specification
                          :comparison-samplers '(:shadow-compare))
      (true (search "var shadow_compare: sampler_comparison;" source))
      (true (search "var linear_clamp: sampler;" source))
      (compiles document))
    ;; WGSL gives the two kinds of sampler different types.
    (is eq :wgsl-comparison-sampler-samples
        (failure-reason
         (lambda ()
           (placed-source-of specification
                             :comparison-samplers '(:linear-clamp)))))))

(define-test unsupported-wgsl-boundaries-retain-source-reasons
  (flet ((reason (options body)
           (failure-reason
            (lambda ()
              (placed-source-of
               (shader:parse-shader-specification 'wgsl-boundary-probe
                                                  options body))))))
    ;; Standard WGSL has no 64-bit integers.
    (is eq :unsupported-wgsl-64-bit-integer
        (reason '(:stage :fragment
                  :outputs ((value :float :location 0))
                  :resources ((sites :storage-buffer :binding 0
                               :element :uint64)))
                '((shader:set-output
                   value
                   (float (shader:uint
                           (ldb (byte 8 4)
                                (shader:buffer-element
                                 sites (shader:uint 0.0)))))))))
    (is eq :unsupported-wgsl-64-bit-integer
        (reason '(:stage :fragment
                  :inputs ((seed :float :location 0))
                  :outputs ((value :float :location 0)))
                '((shader:set-output
                   value
                   (float (shader:uint (ldb (byte 8 36)
                                            (shader:uint64 seed))))))))
    ;; Nor subgroup operations and their built-ins.
    (is eq :unsupported-wgsl-wave-operation
        (reason '(:stage :compute :workgroup-size (1 1 1)
                  :inputs ((cell :uvec3 :built-in :global-invocation-id))
                  :resources ((out :storage-buffer :binding 0
                               :element :float :access :read-write)))
                '((shader:set-buffer-element
                   out (shader:swizzle cell :x)
                   (shader:wave-active-sum
                    (float (shader:swizzle cell :x)))))))
    (is eq :unsupported-wgsl-wave-operation
        (reason '(:stage :compute :workgroup-size (1 1 1)
                  :inputs ((lane :uint :built-in :wave-lane-index))
                  :resources ((out :storage-buffer :binding 0
                               :element :uint :access :read-write)))
                '((shader:set-buffer-element out lane lane))))
    ;; Only fragments have neighbours to difference against.
    (is eq :unsupported-wgsl-derivative
        (reason '(:stage :compute :workgroup-size (1 1 1)
                  :inputs ((cell :uvec3 :built-in :global-invocation-id))
                  :resources ((out :storage-buffer :binding 0
                               :element :float :access :read-write)))
                '((shader:set-buffer-element
                   out (shader:swizzle cell :x)
                   (shader:derivative-x
                    (float (shader:swizzle cell :x)))))))
    ;; WebGPU gives a vertex stage no storage it could write.
    (is eq :unsupported-wgsl-vertex-stage-buffer-access
        (reason '(:stage :vertex
                  :inputs ((index :uint :built-in :vertex-index))
                  :outputs ((position :vec4 :built-in :position))
                  :resources ((corners :storage-buffer :binding 0
                               :element :vec4 :access :read-write)))
                '((shader:set-output
                   position (shader:buffer-element corners index)))))
    ;; WGSL filters f32 textures only.
    (is eq :unsupported-wgsl-texture-sampling
        (reason '(:stage :fragment
                  :inputs ((uv :vec2 :location 0))
                  :outputs ((value :float :location 0))
                  :resources ((marks :uint-texture-2d :binding 0)
                              (filter :sampler :binding 0)))
                '((shader:set-output
                   value
                   (float (shader:swizzle (shader:sample marks filter uv)
                                          :x))))))
    (is eq :sampler-both-samples-and-compares
        (reason '(:stage :fragment
                  :inputs ((uv :vec2 :location 0))
                  :outputs ((value :float :location 0))
                  :resources ((depth :depth-texture-2d :binding 0)
                              (any :sampler :binding 0)))
                '((shader:set-output
                   value
                   (+ (shader:sample-compare depth any uv 0.5)
                      (shader:swizzle (shader:sample depth any uv)
                                      :x))))))))

(shader:define-shader wgsl-compute-probe
    (:stage :compute
     :workgroup-size (8 8 1)
     :inputs ((cell :uvec3 :built-in :global-invocation-id)
              (local :uvec3 :built-in :local-invocation-id)
              (lane :uint :built-in :local-invocation-index)
              (group :uvec3 :built-in :workgroup-id)
              (groups :uvec3 :built-in :num-workgroups)
              (extent :uvec3 :built-in :workgroup-size))
     :resources ((grid :uniform-block :binding 0 :members ((size :vec4)))
                 (heights :storage-buffer :binding 1 :element :float
                          :access :read-write)
                 (seeds :storage-buffer :binding 2 :element :uvec2)))
  (let* ((width (shader:uint (shader:swizzle size :x)))
         (index (+ (* (shader:swizzle cell :y) width) (shader:swizzle cell :x)))
         (seed (shader:buffer-element seeds lane))
         (bump (float (+ (shader:swizzle seed :x) (shader:swizzle local :y)
                         (shader:swizzle group :x) (shader:swizzle groups :z)
                         (shader:swizzle extent :x)))))
    (when (< (shader:swizzle cell :x) width)
      (shader:set-buffer-element
       heights index (+ (shader:buffer-element heights index) bump)))))

(define-test compute-stages-lower-to-workgroups-and-storage-buffers
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-compute-probe))
    (true (search "@compute @workgroup_size(8, 8, 1)
fn wgsl_compute_probe(stage_in: WgslComputeProbeInput) {" source))
    (true (search "@builtin(global_invocation_id) cell: vec3<u32>," source))
    (true (search "@builtin(local_invocation_id) local: vec3<u32>," source))
    (true (search "@builtin(local_invocation_index) lane: u32," source))
    (true (search "@builtin(workgroup_id) group: vec3<u32>," source))
    (true (search "@builtin(num_workgroups) groups: vec3<u32>," source))
    (true (search "@group(0) @binding(1) var<storage, read_write> heights: array<f32>;"
                  source))
    (true (search "@group(0) @binding(2) var<storage, read> seeds: array<vec2<u32>>;"
                  source))
    ;; The group size is the @workgroup_size constant, not a built-in value.
    (false (search "extent" source))
    (true (search "vec3<u32>(8u, 8u, 1u).x" source))
    (true (search "heights[index] = (heights[index] + bump);" source))
    (false (search "result" source))
    (compiles document)))

;;; Integers, booleans, and bits.  #CAI3RP

(shader:define-shader wgsl-integer-bits-probe
    (:stage :compute
     :workgroup-size (64 1 1)
     :inputs ((thread :uvec3 :built-in :global-invocation-id))
     :resources ((cells :storage-buffer :binding 0 :element :ivec2
                        :access :read-write)
                 (words :storage-buffer :binding 1 :element :uint)))
  (let* ((index (shader:swizzle thread :x))
         (cell (shader:buffer-element cells index))
         (word (shader:buffer-element words index))
         (divisor (shader:ivec2 (shader:int 4) (shader:int -4)))
         (floored (mod cell divisor))
         (truncated (rem cell divisor))
         (near (< (abs cell) (shader:ivec2 (shader:int 3) (shader:int 3))))
         (far (not near))
         (both (and near far))
         (either (or near (shader:bvec2 floored)))
         (scalar (and (shader:any either) (not (shader:all both)) t))
         (picked (shader:select either floored truncated))
         (bits (shader:bit-cast :uint (float (shader:swizzle cell :x))))
         (packed (logxor (logior (ash word 3) (ash bits -2))
                         (lognot (logand word (shader:uint 255.0)))))
         (spread (shader:shift-right picked (shader:uint 1.0)))
         (signs (signum (- (min floored truncated) (max cell divisor)))))
    (when scalar
      (shader:set-buffer-element
       cells index
       (+ spread signs
          (shader:ivec2 (shader:int (shader:shift-left packed index))
                        (shader:clamp (shader:swizzle cell :y)
                               (shader:int -8) (shader:int 8))))))))

(define-test integers-and-booleans-lower-to-wgsl
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-integer-bits-probe))
    (true (search "var<storage, read_write> cells: array<vec2<i32>>;" source))
    (true (search "let divisor: vec2<i32> = vec2<i32>(4i, (-4i));" source))
    ;; MOD is floored (its sign follows the divisor), REM is the truncated %.
    (true (search "let floored: vec2<i32> = (((cell % divisor) + divisor) % divisor);"
                  source))
    (true (search "let truncated: vec2<i32> = (cell % divisor);" source))
    ;; && and || are scalar in WGSL; & and | combine boolean vectors.
    (true (search "let both: vec2<bool> = (near & far);" source))
    (true (search "let either: vec2<bool> = (near | vec2<bool>(floored));"
                  source))
    (true (search "let scalar: bool = ((any(either) && (!all(both))) && true);"
                  source))
    (true (search "select(truncated, floored, either)" source))
    (true (search "bitcast<u32>(f32(cell.x))" source))
    (true (search "((word << 3u) | (bits >> 2u))" source))
    (true (search "(~(word & u32(255.0f)))" source))
    ;; WGSL shifts by u32 counts, one per component.
    (true (search "(picked >> vec2<u32>(u32(1.0f)))" source))
    (true (search "sign((min(floored, truncated) - max(cell, divisor)))"
                  source))
    (true (search "clamp(cell.y, (-8i), 8i)" source))
    (compiles document)))

;;; Matrices.  #QEHEEE

(shader:define-shader wgsl-matrix-probe
    (:stage :compute
     :workgroup-size (1 1 1)
     :inputs ((thread :uvec3 :built-in :global-invocation-id))
     :resources ((frame :uniform-block :binding 0
                  :members ((a :mat4) (b :mat4) (v :vec4)))
                 (vectors :storage-buffer :binding 1 :element :vec4
                          :access :read-write)
                 (products :storage-buffer :binding 2 :element :mat4
                           :access :read-write)))
  (let* ((index (shader:swizzle thread :x))
         (basis (shader:mat2 (shader:swizzle v :xy) (shader:swizzle v :zw))))
    (shader:set-buffer-element vectors index (* a v))
    (shader:set-buffer-element vectors (+ index (shader:uint 1.0)) (* v a))
    (shader:set-buffer-element vectors (+ index (shader:uint 2.0))
                               (shader:column a 1))
    (shader:set-buffer-element
     vectors (+ index (shader:uint 3.0))
     (shader:vec4 (* (shader:transpose basis) (shader:swizzle v :xy))
                  (* (* 2.0 basis) (shader:swizzle v :zw))))
    (shader:set-buffer-element products index (* a b))))

(define-test matrices-lower-to-wgsl-column-major
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-matrix-probe))
    ;; WGSL's matrices are the language's: built from columns, indexed by
    ;; column, and multiplied as written.
    (true (search "a: mat4x4<f32>," source))
    (true (search "var<storage, read_write> products: array<mat4x4<f32>>;"
                  source))
    (true (search "mat2x2<f32>(frame.v.xy, frame.v.zw)" source))
    (true (search "vectors[index] = (frame.a * frame.v);" source))
    (true (search "= (frame.v * frame.a);" source))
    (true (search "= frame.a[1];" source))
    (true (search "(transpose(basis) * frame.v.xy)" source))
    (true (search "((2.0f * basis) * frame.v.zw)" source))
    (true (search "products[index] = (frame.a * frame.b);" source))
    (compiles document)))

;;; Structures.  #V16OXI

(shader:define-shader-struct wgsl-instance
  (transform :mat4)
  (tint :vec4)
  (cell :ivec2)
  (layer :uint)
  (weight :float))

(shader:define-shader wgsl-struct-probe
    (:stage :compute
     :workgroup-size (64 1 1)
     :inputs ((thread :uvec3 :built-in :global-invocation-id))
     :resources ((instances :storage-buffer :binding 0
                            :element wgsl-instance :access :read-write)
                 (sources :storage-buffer :binding 1 :element wgsl-instance)))
  (let* ((index (shader:swizzle thread :x))
         (source (shader:buffer-element sources index))
         (moved (make-wgsl-instance
                 :transform (* (wgsl-instance-transform source)
                               (wgsl-instance-transform source))
                 :tint (* (wgsl-instance-transform source)
                          (wgsl-instance-tint source))
                 :cell (+ (wgsl-instance-cell source)
                          (shader:ivec2 (shader:int 1) (shader:int -1)))
                 :layer (wgsl-instance-layer source)
                 :weight 0.5)))
    (shader:set-buffer-element instances index moved)))

(define-test structures-lower-to-wgsl-storage-buffers
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-struct-probe))
    (true (search "struct WgslInstance {
  transform: mat4x4<f32>,
  tint: vec4<f32>,
  cell: vec2<i32>,
  layer: u32,
  weight: f32,
}" source))
    (true (search "var<storage, read_write> instances: array<WgslInstance>;"
                  source))
    (true (search "var<storage, read> sources: array<WgslInstance>;" source))
    ;; A structure's constructor takes its fields in order.
    (true (search "WgslInstance((source.transform * source.transform), (source.transform * source.tint), "
                  source))
    (true (search "instances[index] = moved;" source))
    (compiles document)))

(define-test host-layouts-are-checked-against-wgsl-alignment
  ;; WGSL aligns a vec3 to sixteen bytes and rounds a structure's size up
  ;; to its alignment.  The language lays host structures out so that
  ;; neither rule moves anything, and the lowering checks that it did.
  (is equal '(96 16)
      (multiple-value-list
       (wgsl::wgsl-type-layout (shader:find-shader-type 'wgsl-instance))))
  (is equal '(12 16)
      (multiple-value-list (wgsl::wgsl-type-layout :vec3)))
  (is eq nil (wgsl::wgsl-type-layout :bool))
  (let ((struct (shader::make-shader-struct-type
                 'wgsl-misplaced '((weight :float) (tint :vec4))
                 '(define-shader-struct wgsl-misplaced))))
    ;; As if a host had packed the vec4 straight after the float.
    (setf (shader:shader-struct-field-offset
           (second (shader:shader-struct-type-fields struct)))
          4
          (shader:shader-struct-field-offset
           (first (shader:shader-struct-type-fields struct)))
          0
          (shader:shader-struct-type-size struct) 20)
    (is eq :wgsl-layout-differs-from-host
        (failure-reason
         (lambda () (wgsl::check-wgsl-struct-layout struct nil))))))

;;; Textures, fragment built-ins, and compute effects.

(shader:define-shader wgsl-texture-kinds-probe
    (:stage :fragment
     :inputs ((uv :vec2 :location 0)
              (pixel :vec4 :built-in :frag-coord)
              (front :bool :built-in :front-facing)
              (sample-number :uint :built-in :sample-index))
     :outputs ((color :vec4 :location 0)
               (depth :float :built-in :frag-depth))
     :resources ((albedo :texture-2d :binding 0)
                 (cascades :depth-texture-2d-array :binding 1)
                 (sky :texture-cube :binding 2)
                 (volume :texture-3d :binding 3)
                 (layers :texture-2d-array :binding 4)
                 (heights :depth-texture-2d :binding 5)
                 (marks :uint-texture-2d :binding 6)
                 (linear-clamp :sampler :binding 0)
                 (nearest-clamp :sampler :binding 2)
                 (shadow :sampler :binding 3)))
  (let* ((layer (shader:uint (shader:swizzle pixel :x)))
         (direction (shader:vec3 uv 1.0))
         (base (shader:sample albedo linear-clamp uv))
         (level (shader:sample-level layers linear-clamp uv layer 2.0))
         (biased (shader:sample-bias albedo linear-clamp uv 0.5))
         (graded (shader:sample-grad sky linear-clamp direction direction
                                     direction))
         (lit (shader:sample-compare cascades shadow uv layer 0.5))
         (lit-gather (shader:gather-compare cascades shadow uv layer 0.5))
         (reds (shader:gather albedo linear-clamp uv))
         (depths (shader:gather heights nearest-clamp uv))
         (coarse (shader:sample-level cascades nearest-clamp uv layer 1.0))
         (marked (shader:gather marks nearest-clamp uv))
         (fog (shader:sample-level volume linear-clamp direction 0.0))
         (texel (shader:texel-load volume (shader:uvec3 layer layer layer)
                                   (shader:uint 1.0)))
         (array-texel (shader:texel-load layers (shader:uvec2 layer layer)
                                         layer (shader:uint 0.0)))
         (size (shader:texture-size cascades))
         (sky-size (shader:texture-size sky (shader:uint 1.0))))
    (when (< (shader:swizzle base :a) 0.5)
      (shader:discard))
    (shader:set-output
     color
     (* (+ base level biased graded lit-gather reds depths coarse fog texel
           array-texel
           (shader:vec4 (float (shader:swizzle size :z))
                        (float (shader:swizzle sky-size :y))
                        (float sample-number)
                        (float (shader:swizzle marked :x))))
        lit (if front 1.0 0.5)))
    (shader:set-output depth (shader:swizzle pixel :z))))

(define-test texture-kinds-and-fragment-built-ins-lower-to-wgsl
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-texture-kinds-probe))
    (true (search "var cascades: texture_depth_2d_array;" source))
    (true (search "var sky: texture_cube<f32>;" source))
    (true (search "var volume: texture_3d<f32>;" source))
    (true (search "var layers: texture_2d_array<f32>;" source))
    (true (search "var marks: texture_2d<u32>;" source))
    ;; An array's layer follows the coordinate as an argument of its own.
    (true (search "textureSampleLevel(layers, linear_clamp, stage_in.uv, layer, 2.0f)"
                  source))
    (true (search "textureSampleBias(albedo, linear_clamp, stage_in.uv, 0.5f)"
                  source))
    (true (search "textureSampleGrad(sky, linear_clamp, direction, direction, direction)"
                  source))
    (true (search "textureSampleCompareLevel(cascades, shadow, stage_in.uv, layer, 0.5f)"
                  source))
    (true (search "textureGatherCompare(cascades, shadow, stage_in.uv, layer, 0.5f)"
                  source))
    ;; A colour gather names its channel; a depth texture has one.
    (true (search "textureGather(0, albedo, linear_clamp, stage_in.uv)" source))
    (true (search "textureGather(heights, nearest_clamp, stage_in.uv)" source))
    (true (search "let marked: vec4<u32> = textureGather(0, marks, nearest_clamp, stage_in.uv);"
                  source))
    ;; A depth texture's level is an integer.
    (true (search "vec4<f32>(textureSampleLevel(cascades, nearest_clamp, stage_in.uv, layer, u32(1.0f)))"
                  source))
    (true (search "textureLoad(volume, vec3<u32>(layer, layer, layer), u32(1.0f))"
                  source))
    (true (search "textureLoad(layers, vec2<u32>(layer, layer), layer, u32(0.0f))"
                  source))
    (true (search "let size: vec3<u32> = vec3<u32>(textureDimensions(cascades), textureNumLayers(cascades));"
                  source))
    (true (search "let sky_size: vec2<u32> = textureDimensions(sky, u32(1.0f));"
                  source))
    ;; The fragment's position is :FRAG-COORD as the language promises it.
    (true (search "@builtin(position) pixel: vec4<f32>," source))
    (true (search "@builtin(front_facing) front: bool," source))
    (true (search "@builtin(sample_index) sample_number: u32," source))
    (true (search "@builtin(frag_depth) depth: f32," source))
    (true (search "    discard;" source))
    (compiles document)))

(shader:define-shader wgsl-workgroup-probe
    (:stage :compute
     :workgroup-size (64 1 1)
     :inputs ((cell :uvec3 :built-in :global-invocation-id)
              (local :uint :built-in :local-invocation-index))
     :shared ((tile :vec4 64)
              (counts :uint 4))
     :resources ((counter :storage-buffer :binding 0 :element :uint
                          :access :read-write)
                 (values :storage-buffer :binding 1 :element :vec4
                         :access :read-write)
                 (image :read-write-texture-2d :binding 0 :format :rgba16f)
                 (mask :read-write-texture-2d :binding 1 :format :r32ui)
                 (heat :read-write-texture-2d :binding 2 :format :r32f)
                 (paint :read-write-texture-2d :binding 3 :format :rgba8)))
  (let* ((index (shader:swizzle cell :x))
         (zero (shader:uint 0.0))
         (one (shader:uint 1.0))
         (texel (shader:uvec2 index zero))
         (loaded (shader:texel-load paint texel))
         (masked (shader:texel-load mask texel))
         (size (shader:texture-size image)))
    (shader:set-shared-element tile local loaded)
    (when (= local zero)
      (shader:set-shared-element counts zero zero))
    (shader:workgroup-barrier)
    (shader:atomic-add counts zero one)
    (let* ((neighbour (shader:shared-element
                       tile (mod (+ local one) (shader:uint 64.0))))
           (tally (shader:shared-element counts zero))
           (slot (shader:atomic-add counter zero one))
           (low (shader:atomic-min counter one index))
           (swapped (shader:atomic-exchange counter one index))
           (compared (shader:atomic-compare-exchange counter one zero index))
           (seen (shader:buffer-element counter zero)))
      (shader:atomic-compare-exchange counter one zero index)
      (shader:storage-barrier)
      (shader:set-buffer-element counter one seen)
      (shader:set-buffer-element
       values slot
       (+ neighbour
          (shader:vec4 (float tally) (float low) (float swapped)
                       (float (+ compared (shader:swizzle masked :x)
                                 (shader:swizzle size :y))))))
      (shader:set-texel image texel neighbour)
      (shader:set-texel mask texel (shader:uvec4 slot zero zero zero))
      (shader:set-texel heat texel (shader:vec4 0.5 0.0 0.0 0.0)))))

(define-test workgroup-memory-atomics-and-storage-textures-lower-to-wgsl
  (multiple-value-bind (source document)
      (placed-source-of (wgsl-workgroup-probe))
    (true (search "var<workgroup> tile: array<vec4<f32>, 64>;" source))
    ;; A buffer or array any atomic touches is atomic throughout the module.
    (true (search "var<workgroup> counts: array<atomic<u32>, 4>;" source))
    (true (search "var<storage, read_write> counter: array<atomic<u32>>;"
                  source))
    (true (search "atomicStore(&counts[zero], zero);" source))
    (true (search "let tally: u32 = atomicLoad(&counts[zero]);" source))
    (true (search "let seen: u32 = atomicLoad(&counter[zero]);" source))
    (true (search "atomicStore(&counter[one], seen);" source))
    ;; An atomic statement keeps its effect and assigns its value to nothing.
    (true (search "_ = atomicAdd(&counts[zero], one);" source))
    (true (search "let slot: u32 = atomicAdd(&counter[zero], one);" source))
    (true (search "let low: u32 = atomicMin(&counter[one], index);" source))
    (true (search "atomicExchange(&counter[one], index)" source))
    ;; The weak compare-exchange is retried while it fails spuriously.
    (true (search "  loop {
    let exchange_3 = atomicCompareExchangeWeak(&counter[one], comparand_1, index);
    observed_2 = exchange_3.old_value;
    if (exchange_3.exchanged || (observed_2 != comparand_1)) { break; }
  }
  let compared: u32 = observed_2;" source))
    ;; Each storage texture takes the least access the module needs, in its
    ;; family's place after the sampled textures.
    (true (search "@group(1) @binding(16) var image: texture_storage_2d<rgba16float, write>;"
                  source))
    (true (search "@group(1) @binding(17) var mask: texture_storage_2d<r32uint, read_write>;"
                  source))
    (true (search "@group(1) @binding(18) var heat: texture_storage_2d<r32float, write>;"
                  source))
    (true (search "@group(1) @binding(19) var paint: texture_storage_2d<rgba8unorm, read>;"
                  source))
    (true (search "requires readonly_and_readwrite_storage_textures;" source))
    ;; A storage texel has the language's four channels whatever its format.
    (true (search "let masked: vec4<u32> = textureLoad(mask, texel);" source))
    (true (search "textureStore(heat, texel, vec4<f32>(0.5f, 0.0f, 0.0f, 0.0f));"
                  source))
    (true (search "let size: vec2<u32> = textureDimensions(image);" source))
    (true (search "tile[stage_in.local] = loaded;" source))
    (true (search "  workgroupBarrier();
  storageBarrier();
  textureBarrier();" source))
    (compiles document)))

(define-test storage-textures-take-the-access-their-program-needs
  (flet ((probe (format body)
           (shader:parse-shader-specification
            'wgsl-storage-probe
            `(:stage :compute :workgroup-size (1 1 1)
              :inputs ((cell :uvec3 :built-in :global-invocation-id))
              :resources ((image :read-write-texture-2d :binding 0
                           :format ,format)))
            body))
         (image (specification)
           (first (shader:shader-specification-resources specification))))
    (let ((store (probe :rgba16f
                        '((shader:set-texel image (shader:swizzle cell :xy)
                           (shader:vec4 1.0 0.0 0.0 1.0)))))
          (load-store
            (probe :rgba16f
                   '((shader:set-texel image (shader:swizzle cell :xy)
                      (shader:texel-load image (shader:swizzle cell :yx)))))))
      (multiple-value-bind (source document) (placed-source-of store)
        (true (search "texture_storage_2d<rgba16float, write>;" source))
        ;; Writing alone is WebGPU as first shipped; nothing is required.
        (false (search "requires" source))
        (compiles document))
      (is eq :write (wgsl:wgsl-storage-texture-access (image store) store))
      ;; WebGPU lets a module both load and store one-channel 32-bit texels
      ;; only.
      (is eq :unsupported-wgsl-read-write-storage-texture
          (failure-reason (lambda () (placed-source-of load-store))))
      (is eq :read-write
          (let ((specification
                  (probe :r32f
                         '((shader:set-texel
                            image (shader:swizzle cell :xy)
                            (shader:texel-load
                             image (shader:swizzle cell :yx)))))))
            (wgsl:wgsl-storage-texture-access (image specification)
                                              specification)))
      ;; A caller that knows the other stages chooses for all of them.
      (true (search "texture_storage_2d<rgba16float, read>;"
                    (placed-source-of
                     store :storage-texture-access (constantly :read)))))))

(define-test nested-bindings-sequence-after-effects-without-redeclaring
  (let* ((specification
           (shader:parse-shader-specification
            'wgsl-sequence-probe
            '(:stage :compute :workgroup-size (1 1 1)
              :inputs ((cell :uvec3 :built-in :global-invocation-id))
              :shared ((scratch :uint 4))
              :resources ((data :storage-buffer :binding 0 :element :uint
                           :access :read-write)))
            '((let* ((index (shader:swizzle cell :x)))
                (shader:set-buffer-element data index index)
                (let* ((index (+ index (shader:buffer-element data index)))
                       (data (+ index index)))
                  (shader:set-shared-element scratch (shader:uint 0) data))
                (shader:set-buffer-element
                 data index
                 (shader:shared-element scratch (shader:uint 0)))))))
         (source (source-of specification)))
    ;; The inner INDEX reads the store before it, under a fresh name, and
    ;; the inner DATA does not hide the buffer from the store after it.
    (true (< (search "data[index] = index;" source)
             (search "let index_2: u32 = (index + data[index]);" source)
             (search "let data_2: u32 = (index_2 + index_2);" source)
             (search "scratch[0u] = data_2;" source)
             (search "data[index] = scratch[0u];" source)))
    (compiles (wgsl:compile-wgsl specification))))

(define-test names-become-wgsl-identifiers
  (is string= "frame_state" (wgsl:wgsl-identifier 'frame-state))
  ;; Keywords and reserved words, from the specification's two lists.
  (is string= "default_" (wgsl:wgsl-identifier 'default))
  (is string= "target_" (wgsl:wgsl-identifier 'target))
  (is string= "filter_" (wgsl:wgsl-identifier 'filter))
  ;; Predeclared names the lowering itself writes.
  (is string= "mix_" (wgsl:wgsl-identifier 'mix))
  (is string= "vec3_" (wgsl:wgsl-identifier 'vec3))
  ;; Others stay free for shaders: a local may be called DISTANCE.
  (is string= "distance" (wgsl:wgsl-identifier 'distance))
  ;; No identifier begins with two underscores or is one alone.
  (is string= "v__hidden" (wgsl:wgsl-identifier '--hidden))
  (is string= "v_" (wgsl:wgsl-identifier '-))
  (is string= "_2d" (wgsl:wgsl-identifier '2d)))
