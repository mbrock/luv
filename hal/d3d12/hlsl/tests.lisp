;;;; Executable claims for the HLSL lowering.
;;;;
;;;; Text claims pin the dialect choices; when DXC is on PATH (the Nix
;;;; environments carry it) every probe is also compiled to DXIL with the
;;;; profile and entry point its document names.

(defpackage #:luv.hlsl.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:is)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:hlsl #:luv.hlsl)))

(in-package #:luv.hlsl.tests)

(defun dxc-program ()
  "The DXC executable to validate with, or NIL when none is installed."
  (let ((candidate (or (uiop:getenv "LUV_DXC") "dxc")))
    (and (ignore-errors
          (zerop (nth-value 2 (uiop:run-program (list candidate "--version")
                                                :ignore-error-status t))))
         candidate)))

(defun dxc-diagnostics (document)
  "Compile DOCUMENT with DXC.  Return NIL on success, else DXC's report.
Without DXC, return NIL: the text claims still hold."
  (let ((dxc (dxc-program)))
    (when dxc
      (uiop:with-temporary-file (:pathname source :type "hlsl" :keep nil)
        (hlsl:write-hlsl document source)
        (uiop:with-temporary-file (:pathname object :type "dxil" :keep nil)
          (multiple-value-bind (output error-output status)
              (uiop:run-program
               (list dxc "-HV" "2021" "-WX"
                     "-T" (hlsl:hlsl-document-profile document)
                     "-E" (hlsl:hlsl-entry-point-name
                           (hlsl:hlsl-document-entry-point document))
                     "-Fo" (uiop:native-namestring object)
                     (uiop:native-namestring source))
               :output :string :error-output :string
               :ignore-error-status t)
            (unless (zerop status)
              (format nil "~A~A~%~A" output error-output
                      (hlsl:hlsl-document-source document)))))))))

(defmacro compiles (document)
  `(is eq nil (dxc-diagnostics ,document)))

(defun source-of (specification &rest arguments)
  (let ((document (apply #'hlsl:compile-hlsl specification arguments)))
    (values (hlsl:hlsl-document-source document) document)))

(defun failure-reason (thunk)
  (handler-case (progn (funcall thunk) nil)
    (shader:shader-language-error (condition)
      (shader:shader-language-error-reason condition))))

(shader:define-shader hlsl-pulling-vertex-probe
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

(shader:define-shader hlsl-shading-fragment-probe
    (:stage :fragment
     :inputs ((uv :vec2 :location 1)
              (shadow :vec3 :location 2))
     :outputs ((color :vec4 :location 0)
               (glow :vec4 :location 1))
     :resources ((albedo :texture-2d :binding 0)
                 (shadow-map :depth-texture-2d :binding 1)
                 (linear-clamp :sampler :binding 0)
                 (shadow-compare :sampler :binding 3)))
  (let* ((base (shader:sample albedo linear-clamp uv))
         (depth (shader:sample shadow-map linear-clamp uv))
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
         (wrap (floor (* (cos (shader:swizzle uv :y)) 4.0))))
    (when (> lit 0.5)
      (shader:set-output glow (shader:vec4 lit lit lit 1.0)))
    (shader:set-output
     color
     (shader:vec4 (* bent (shader:clamp (* curve fog) 0.0 1.0))
                  (if (< sign 0.0)
                      (+ wrap (shader:swizzle depth :x))
                      (shader:dot (shader:swizzle base :rgb)
                                  (shader:vec3 0.2 0.7 0.1)))))))

(shader:define-shader hlsl-bit-field-probe
    (:stage :fragment
     :outputs ((color :vec4 :location 0))
     :resources ((sites :storage-buffer :binding 1 :element :uint64)
                 (words :storage-buffer :binding 2 :element :uvec4)
                 (band-data :uint-texture-2d :binding 0)
                 (depth :depth-texture-2d :binding 1)))
  (let* ((term (shader:buffer-element sites (shader:uint 3.0)))
         (word (shader:swizzle (shader:buffer-element words (shader:uint 0.0))
                               :x))
         (header (shader:texel-load
                  band-data (shader:uvec2 (shader:uint 1.0) word)))
         (stored (shader:texel-load depth (shader:uvec2 word word)))
         (field (ldb (byte 24 4) term))
         (nibble (ldb (byte 4 0) term))
         (top (ldb (byte 16 16) word))
         (count (shader:swizzle header :x))
         (total
           (shader:counted-fold (index count sum 0.0)
             (+ sum (float (mod (+ index top) (shader:uint 7.0)))))))
    (shader:set-output
     color
     (shader:vec4 (float (shader:uint field)) (float (shader:uint nibble))
                  total (shader:swizzle stored :x)))))

(shader:define-shader-function hlsl-reused-local (first second)
  (let* ((sum (+ first first)))
    (+ sum second)))

(shader:define-shader hlsl-inline-probe
    (:stage :fragment
     :inputs ((value :float :location 0))
     :outputs ((result :float :location 0)))
  (shader:set-output
   result
   (hlsl-reused-local (hlsl-reused-local value 1.0) 2.0)))

(shader:define-shader hlsl-early-fold-probe
    (:stage :fragment
     :inputs ((count :float :location 0)
              (limit :float :location 1))
     :outputs ((result :float :location 0)))
  (shader:set-output
   result
   (shader:counted-fold (index count sum 0.0 :until (> sum limit))
     (if (< index limit) (+ sum index) sum))))

(define-test vertex-pulling-lowers-to-direct3d-registers-and-system-values
  (multiple-value-bind (source document)
      (source-of (hlsl-pulling-vertex-probe))
    (true (search "uint vertex_index : SV_VertexID" source))
    (true (search "uint instance_index : SV_InstanceID" source))
    (true (search "cbuffer FrameBlock : register(b0) {" source))
    (true (search "  Frame frame;" source))
    (true (search "StructuredBuffer<float4> corners : register(t1, space0);"
                  source))
    (true (search "Texture2D<float4> heights : register(t0, space1);" source))
    (true (search "SamplerState linear_clamp : register(s0);" source))
    (true (search "float4 corner = corners[vertex_index];" source))
    (true (search "frame.offset" source))
    ;; Outside pixel shaders, SM 6.0 has no implicit-derivative Sample.
    (true (search "heights.SampleLevel(linear_clamp, corner.xy, 0.0f)" source))
    ;; The position comes first and locations follow in order, whatever the
    ;; declaration order, so the pixel shader can mirror the signature.
    (true (< (search "SV_Position" source) (search "LOCATION0" source)
             (search "LOCATION1" source)))
    ;; Integer varyings never interpolate.
    (true (search "nointerpolation uint tile : LOCATION0;" source))
    ;; Clip Y flips exactly as in MSL: the shared graph is Vulkan-oriented.
    (true (search "* float4(1.0f, -1.0f, 1.0f, 1.0f))" source))
    (true (string= "vs_6_0" (hlsl:hlsl-document-profile document)))
    (true (string= "hlsl_pulling_vertex_probe"
                   (hlsl:hlsl-entry-point-name
                    (hlsl:hlsl-document-entry-point document))))
    (compiles document)))

(define-test fragment-operators-lower-to-hlsl-intrinsics
  (multiple-value-bind (source document)
      (source-of (hlsl-shading-fragment-probe))
    (true (search "Texture2D<float> shadow_map : register(t1, space1);" source))
    (true (search "SamplerComparisonState shadow_compare : register(s3);"
                  source))
    (true (search "albedo.Sample(linear_clamp, stage_in.uv)" source))
    ;; A depth texture samples one float; the language's value is a vec4.
    (true (search "((float4)(shadow_map.Sample(linear_clamp, stage_in.uv)))"
                  source))
    (true (search "shadow_map.SampleCmpLevelZero(shadow_compare," source))
    (true (search "frac(" source))
    (true (search "lerp(base.rgb, normalize(base.gbr), ripple)" source))
    (true (search "smoothstep(0.1f, 0.9f, stage_in.uv.y)" source))
    (true (search "step(0.5f, stage_in.uv.x)" source))
    (true (search "ddx(stage_in.uv.x)" source))
    (true (search "ddy(stage_in.uv.y)" source))
    ;; HLSL's sign returns int; SIGNUM keeps its operand's type.
    (true (search "((float)(sign(" source))
    (true (search "pow(max(max(stage_in.uv.x, 0.001f), slope), 2.2f)" source))
    (true (search "exp((-log(" source))
    (true (search "dot(base.rgb, float3(0.2f, 0.7f, 0.1f))" source))
    (true (search "(sign_ < 0.0f) ?" source))
    (true (search "if (lit > 0.5f) {" source))
    (true (search "float4 glow : SV_Target1;" source))
    (true (search "float4 sv_position : SV_Position;" source))
    (true (string= "ps_6_0" (hlsl:hlsl-document-profile document)))
    (compiles document)))

(define-test bit-fields-texel-loads-and-folds-lower-to-integer-hlsl
  (multiple-value-bind (source document) (source-of (hlsl-bit-field-probe))
    (true (search "StructuredBuffer<uint64_t> sites : register(t1, space0);"
                  source))
    (true (search "StructuredBuffer<uint4> words : register(t2, space0);"
                  source))
    (true (search "Texture2D<uint4> band_data : register(t0, space1);" source))
    (true (search "uint64_t term = sites[((uint)(3.0f))];" source))
    (true (search "((term >> 4ull) & 0xFFFFFFull)" source))
    (true (search "((term >> 0ull) & 0xFull)" source))
    (true (search "(word >> 16u)" source))
    (true (search "band_data.Load(int3(int2(uint2(((uint)(1.0f)), word)), 0))"
                  source))
    (true (search "((float4)(depth.Load(" source))
    (true (search "for (uint fold_index_1 = 0u; fold_index_1 < count;" source))
    (true (search "% ((uint)(7.0f))" source))
    (compiles document)))

(define-test inline-functions-materialize-each-local-once
  (multiple-value-bind (source document) (source-of (hlsl-inline-probe))
    (let ((declaration "float hlsl_reused_local_1_local_1_sum ="))
      (true (search declaration source))
      (false (search declaration source
                     :start2 (1+ (search declaration source)))))
    ;; RESULT names the entry's output structure; a shader name escapes.
    (true (search "float result_ : SV_Target0;" source))
    (compiles document)))

(define-test early-exit-folds-lower-to-loops-with-breaks
  (multiple-value-bind (source document) (source-of (hlsl-early-fold-probe))
    (true (search "float fold_state_1 = 0.0f;" source))
    (true (search "for (float fold_index_1 = 0.0f;" source))
    (true (search "if (fold_state_1 > stage_in.limit) break;" source))
    (true (search "(fold_index_1 < stage_in.limit) ?" source))
    (compiles document)))

(define-test fragment-input-signature-mirrors-the-vertex-outputs
  (let* ((vertex (hlsl-pulling-vertex-probe))
         (fragment
           (shader:parse-shader-specification
            'mirror-probe
            '(:stage :fragment
              :inputs ((uv :vec2 :location 1))
              :outputs ((color :vec4 :location 0)))
            '((shader:set-output color (shader:vec4 uv 0.0 1.0)))))
         (source
           (source-of fragment
                      :interface (shader:shader-specification-outputs vertex))))
    (true (< (search "float4 sv_position : SV_Position;" source)
             (search "nointerpolation uint unused_location0 : LOCATION0;"
                     source)
             (search "float2 uv : LOCATION1;" source)))
    (compiles (hlsl:compile-hlsl
               fragment
               :interface (shader:shader-specification-outputs vertex)))))

(define-test entry-point-names-and-occurrences-are-retained
  (let* ((specification (hlsl-shading-fragment-probe))
         (document (hlsl:compile-hlsl specification
                                      :entry-point-name "shading_fragment"))
         (binding (find "BENT" (shader:shader-specification-bindings
                                specification)
                        :key (lambda (binding)
                               (symbol-name (shader:shader-object-name
                                             binding)))
                        :test #'string=))
         (expression (shader:shader-binding-expression binding))
         (occurrences
           (gethash expression
                    (hlsl:hlsl-document-expression-occurrences document))))
    (true (search (concatenate
                   'string
                   "HlslShadingFragmentProbeOutput shading_fragment("
                   "HlslShadingFragmentProbeInput stage_in)")
                  (hlsl:hlsl-document-source document)))
    (true occurrences)
    (true (every (lambda (occurrence)
                   (eq expression
                       (gethash occurrence
                                (hlsl:hlsl-document-occurrence-expression
                                 document))))
                 occurrences))
    (true (string= (hlsl:hlsl-document-source document)
                   (hlsl:hlsl-document-source
                    (hlsl:compile-hlsl specification
                                       :entry-point-name
                                       "shading_fragment"))))))

(define-test unsupported-hlsl-boundaries-retain-source-reasons
  (flet ((probe (options body)
           (shader:parse-shader-specification 'hlsl-boundary-probe
                                              options body)))
    ;; Direct3D has no system value for the dispatch's group count.
    (is eq :unsupported-hlsl-built-in
        (failure-reason
         (lambda ()
           (hlsl:compile-hlsl
            (probe '(:stage :compute :workgroup-size (1 1 1)
                     :inputs ((groups :uvec3 :built-in :num-workgroups))
                     :resources ((out :storage-buffer :binding 0
                                  :element :uint :access :read-write)))
                   '((shader:set-buffer-element
                      out (shader:swizzle groups :x)
                      (shader:swizzle groups :y))))))))
    (is eq :unsupported-hlsl-descriptor-set
        (failure-reason
         (lambda ()
           (hlsl:compile-hlsl
            (probe '(:stage :fragment
                     :outputs ((value :float :location 0))
                     :resources ((image :texture-2d :set 1 :binding 0)))
                   '((shader:set-output value 1.0)))))))
    ;; Metal shares one buffer index space, so the contract does too.
    (is eq :hlsl-binding-collision
        (failure-reason
         (lambda ()
           (hlsl:compile-hlsl
            (probe '(:stage :fragment
                     :outputs ((value :float :location 0))
                     :resources ((camera :uniform-block :binding 0
                                  :members ((position :vec4)))
                                 (data :storage-buffer :binding 0
                                  :element :vec4)))
                   '((shader:set-output value 1.0)))))))
    (is eq :sampler-both-samples-and-compares
        (failure-reason
         (lambda ()
           (hlsl:compile-hlsl
            (probe '(:stage :fragment
                     :inputs ((uv :vec2 :location 0))
                     :outputs ((value :float :location 0))
                     :resources ((depth :depth-texture-2d :binding 0)
                                 (any :sampler :binding 0)))
                   '((shader:set-output
                      value
                      (+ (shader:sample-compare depth any uv 0.5)
                         (shader:swizzle (shader:sample depth any uv)
                                         :x)))))))))))

(shader:define-shader hlsl-compute-probe
    (:stage :compute
     :workgroup-size (8 8 1)
     :inputs ((cell :uvec3 :built-in :global-invocation-id)
              (local :uvec3 :built-in :local-invocation-id)
              (lane :uint :built-in :local-invocation-index)
              (group :uvec3 :built-in :workgroup-id)
              (extent :uvec3 :built-in :workgroup-size))
     :resources ((grid :uniform-block :binding 0 :members ((size :vec4)))
                 (heights :storage-buffer :binding 1 :element :float
                          :access :read-write)
                 (seeds :storage-buffer :binding 2 :element :uvec2)))
  (let* ((width (shader:uint (shader:swizzle size :x)))
         (index (+ (* (shader:swizzle cell :y) width) (shader:swizzle cell :x)))
         (seed (shader:buffer-element seeds lane))
         (bump (float (+ (shader:swizzle seed :x) (shader:swizzle local :y)
                         (shader:swizzle group :x) (shader:swizzle extent :x)))))
    (when (< (shader:swizzle cell :x) width)
      (shader:set-buffer-element
       heights index (+ (shader:buffer-element heights index) bump)))))

(define-test compute-stages-lower-to-numthreads-and-unordered-access
  (multiple-value-bind (source document) (source-of (hlsl-compute-probe))
    (true (search "[numthreads(8, 8, 1)]" source))
    (true (search (concatenate
                   'string
                   "void hlsl_compute_probe(uint3 cell : SV_DispatchThreadID, "
                   "uint3 local : SV_GroupThreadID, uint lane : SV_GroupIndex, "
                   "uint3 group : SV_GroupID)")
                  source))
    (true (search "RWStructuredBuffer<float> heights : register(u1, space0);"
                  source))
    (true (search "StructuredBuffer<uint2> seeds : register(t2, space0);"
                  source))
    ;; The group size is the [numthreads] constant.
    (true (search "uint3(8u, 8u, 1u).x" source))
    (true (search "heights[index] = (heights[index] + bump);" source))
    (false (search "result" source))
    (true (string= "cs_6_0" (hlsl:hlsl-document-profile document)))
    (compiles document)))

(define-test buffer-stores-belong-to-compute-and-read-write-buffers
  (flet ((reason (options body)
           (failure-reason
            (lambda ()
              (shader:parse-shader-specification 'store-probe options
                                                 (list body))))))
    (is eq :read-only-storage-buffer
        (reason '(:stage :compute :workgroup-size (1 1 1)
                  :resources ((data :storage-buffer :binding 0
                               :element :float)))
                '(shader:set-buffer-element data (shader:uint 0.0) 1.0)))
    (is eq :buffer-element-type-mismatch
        (reason '(:stage :compute :workgroup-size (1 1 1)
                  :resources ((data :storage-buffer :binding 0
                               :element :vec4 :access :read-write)))
                '(shader:set-buffer-element data (shader:uint 0.0) 1.0)))
    (is eq :invalid-statement-for-stage
        (reason '(:stage :fragment
                  :outputs ((color :vec4 :location 0))
                  :resources ((data :storage-buffer :binding 0
                               :element :float :access :read-write)))
                '(shader:set-buffer-element data (shader:uint 0.0) 1.0)))
    (is eq :invalid-workgroup-size
        (reason '(:stage :compute
                  :resources ((data :storage-buffer :binding 0
                               :element :float :access :read-write)))
                '(shader:set-buffer-element data (shader:uint 0.0) 1.0)))
    (is eq :ordinary-outputs-on-workgroup-stage
        (reason '(:stage :compute :workgroup-size (1 1 1)
                  :outputs ((value :float :location 0)))
                '(shader:set-output value 1.0)))
    (is eq :invalid-storage-buffer-access
        (reason '(:stage :compute :workgroup-size (1 1 1)
                  :resources ((data :storage-buffer :binding 0
                               :element :float :access :write)))
                '(shader:set-buffer-element data (shader:uint 0.0) 1.0)))))

;;; Integers, booleans, and bits.  #CAI3RP

(shader:define-shader hlsl-integer-bits-probe
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

(define-test integers-and-booleans-lower-to-hlsl-2021
  (multiple-value-bind (source document) (source-of (hlsl-integer-bits-probe))
    (true (search "RWStructuredBuffer<int2> cells" source))
    (true (search "int2 divisor = int2(4, (-4));" source))
    ;; MOD is floored (its sign follows the divisor), REM is C's %.
    (true (search "int2 floored = (((cell % divisor) + divisor) % divisor);"
                  source))
    (true (search "int2 truncated = (cell % divisor);" source))
    ;; Vector logic is and()/or() in HLSL 2021; scalar logic stays &&/||.
    (true (search "bool2 both = and(near, far);" source))
    (true (search "bool2 either = or(near, bool2(floored));" source))
    (true (search "bool scalar = ((any(either) && (!all(both))) && true);"
                  source))
    (true (search "select(either, floored, truncated)" source))
    (true (search "asuint(((float)(cell.x)))" source))
    (true (search "((word << 3u) | (bits >> 2u))" source))
    (true (search "(~(word & ((uint)(255.0f))))" source))
    (true (search "((int2)(sign(" source))
    (compiles document)))
