;;; HLSL for textures, storage textures, and stage effects.
;;;
;;; Texture arrays, cubes, and volumes are Texture2DArray, TextureCube, and
;;; Texture3D in register space 1 beside the 2D textures; a storage texture
;;; is an RWTexture2D in u-register space 1.  Workgroup arrays are
;;; groupshared, barriers the ...WithGroupSync intrinsics, atomics the
;;; Interlocked functions, and wave operations shader model 6.0's Wave
;;; intrinsics -- nothing past 6.0.

(in-package #:luv.hlsl)

;;; Locals and plain statements.

(defun hlsl-local-name (context base &optional unique)
  "Claim BASE as a local name; when UNIQUE and BASE is taken, a fresh one."
  (let ((names (hlsl-context-local-names context)))
    (if (and unique (gethash base names))
        (loop for ordinal from 2
              for candidate = (format nil "~A_~D" base ordinal)
              unless (gethash candidate names)
                do (setf (gethash candidate names) t)
                   (return candidate))
        (progn (setf (gethash base names) t) base))))

(defun hlsl-temporary-name (context prefix)
  (hlsl-local-name
   context
   (format nil "~A_~D" prefix (incf (hlsl-context-temporary-counter context)))
   t))

(defclass hlsl-line-statement ()
  ((text :initarg :text :reader hlsl-line-statement-text)
   (origin :initarg :origin :initform nil :reader hlsl-line-statement-origin))
  (:documentation "One rendered statement line, such as an intrinsic call."))

(defmethod write-hlsl-statement ((statement hlsl-line-statement) stream)
  (write-hlsl-indent stream)
  (format stream "~A~%" (hlsl-line-statement-text statement)))

(defun hlsl-line (origin control &rest arguments)
  (make-instance 'hlsl-line-statement
                 :text (apply #'format nil control arguments)
                 :origin origin))

(defun push-hlsl-pending (context &rest statements)
  (setf (hlsl-context-pending-statements context)
        (append (hlsl-context-pending-statements context) statements)))

;;; Types, built-ins, and declarations.

(defun hlsl-storage-texture-type-name (type)
  (format nil "RWTexture2D<~A>"
          (ecase (shader:shader-type-storage-format type)
            ((:rgba32f :rgba16f) "float4")
            (:rgba8 "unorm float4")
            (:r32f "float")
            (:r32ui "uint"))))

(defun hlsl-expression-built-in-text (input)
  "The HLSL expression for a built-in that is no entry parameter, or NIL."
  (case (shader:shader-interface-built-in input)
    ;; One SV_Position serves the stage; its w is the clip w, where Vulkan
    ;; and Metal give the reciprocal the language promises.
    (:frag-coord
     "float4(stage_in.sv_position.xyz, 1.0f / stage_in.sv_position.w)")
    (:wave-lane-index "WaveGetLaneIndex()")
    (:wave-lane-count "WaveGetLaneCount()")))

(defun hlsl-built-in-parameter-p (input)
  "Whether INPUT is a built-in passed to the entry point by semantic."
  (and (shader:shader-interface-built-in input)
       (not (member (shader:shader-interface-built-in input)
                    '(:workgroup-size :frag-coord
                      :wave-lane-index :wave-lane-count)))))

(defclass hlsl-groupshared-declaration ()
  ((type :initarg :type :reader hlsl-groupshared-type)
   (name :initarg :name :reader hlsl-groupshared-name)
   (count :initarg :count :reader hlsl-groupshared-count)
   (origin :initarg :origin :reader hlsl-groupshared-origin)))

(defun hlsl-groupshared-declaration (array)
  (make-instance 'hlsl-groupshared-declaration
                 :type (hlsl-type-name (shader:shader-declaration-type array))
                 :name (hlsl-identifier (shader:shader-object-name array))
                 :count (shader:shader-shared-array-element-count array)
                 :origin array))

(defmethod write-hlsl-declaration
    ((declaration hlsl-groupshared-declaration) stream)
  (format stream "groupshared ~A ~A[~D];~%"
          (hlsl-groupshared-type declaration)
          (hlsl-groupshared-name declaration)
          (hlsl-groupshared-count declaration)))

;;; Workgroup memory.

(defun hlsl-element-text (context target index)
  (format nil "~A[~A]"
          (gethash target (hlsl-context-references context))
          (hlsl-text (lower-hlsl-expression context index))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-shared-element))
  (note-hlsl-occurrence
   context expression
   (hlsl-element-text context (shader:shader-shared-element-array expression)
                      (shader:shader-shared-element-index expression))))

;;; Textures.

(defun hlsl-texture-coordinate (texture-type coordinate layer)
  "Array textures take the layer as the coordinate's last float component."
  (if (shader:shader-texture-arrayed-p texture-type)
      (format nil "float3(~A, (float)(~A))" coordinate layer)
      coordinate))

(defun hlsl-sampling-operands (context expression)
  "Return the texture, sampler, coordinate (with any layer folded in), and
the remaining operand texts of a sampling EXPRESSION."
  (let* ((operands (shader:shader-call-operands expression))
         (type (shader:shader-expression-type (first operands)))
         (texts (lower-hlsl-operands context expression))
         (arrayed-p (shader:shader-texture-arrayed-p type)))
    (values (first texts) (second texts)
            (hlsl-texture-coordinate type (third texts)
                                     (and arrayed-p (fourth texts)))
            (nthcdr (if arrayed-p 4 3) texts))))

(defun check-hlsl-comparison-sampler (context expression)
  (let ((sampler (shader:shader-resource-target
                  (second (shader:shader-call-operands expression)))))
    (unless (and sampler
                 (member (shader:shader-resource-key sampler)
                         (hlsl-context-comparison-samplers context)))
      (error 'shader:shader-language-error
             :form (shader:shader-expression-source-form expression)
             :reason :hlsl-comparison-sampler-unknown
             :details (and sampler (shader:shader-object-name sampler))))))

(defun hlsl-sampled (expression text)
  (hlsl-widen-depth (first (shader:shader-call-operands expression)) text))

(define-hlsl-operator shader:sample (context expression)
  ;; Implicit-derivative Sample exists only in pixel shaders before SM 6.6.
  ;; Elsewhere sample the base level, as Metal does outside fragments.
  (multiple-value-bind (texture sampler coordinate)
      (hlsl-sampling-operands context expression)
    (note-hlsl-occurrence
     context expression
     (hlsl-sampled
      expression
      (if (eq :fragment (hlsl-context-stage context))
          (format nil "~A.Sample(~A, ~A)" texture sampler coordinate)
          (format nil "~A.SampleLevel(~A, ~A, 0.0f)"
                  texture sampler coordinate))))))

(define-hlsl-operator shader:sample-level (context expression)
  (multiple-value-bind (texture sampler coordinate rest)
      (hlsl-sampling-operands context expression)
    (note-hlsl-occurrence
     context expression
     (hlsl-sampled expression
                   (format nil "~A.SampleLevel(~A, ~A, ~A)"
                           texture sampler coordinate (first rest))))))

(define-hlsl-operator shader:sample-bias (context expression)
  (multiple-value-bind (texture sampler coordinate rest)
      (hlsl-sampling-operands context expression)
    (note-hlsl-occurrence
     context expression
     (hlsl-sampled expression
                   (format nil "~A.SampleBias(~A, ~A, ~A)"
                           texture sampler coordinate (first rest))))))

(define-hlsl-operator shader:sample-grad (context expression)
  (multiple-value-bind (texture sampler coordinate rest)
      (hlsl-sampling-operands context expression)
    (note-hlsl-occurrence
     context expression
     (hlsl-sampled expression
                   (format nil "~A.SampleGrad(~A, ~A, ~A, ~A)"
                           texture sampler coordinate
                           (first rest) (second rest))))))

(define-hlsl-operator shader:sample-compare (context expression)
  ;; Depth comparison reads the base level in every stage.  Shadow maps have
  ;; one level, so this is Metal's sample_compare without the gradient
  ;; requirement that would forbid it in vertex shaders and divergent code.
  (check-hlsl-comparison-sampler context expression)
  (multiple-value-bind (texture sampler coordinate rest)
      (hlsl-sampling-operands context expression)
    (note-hlsl-occurrence
     context expression
     (format nil "~A.SampleCmpLevelZero(~A, ~A, ~A)"
             texture sampler coordinate (first rest)))))

(define-hlsl-operator shader:gather (context expression)
  ;; A depth texture has one channel, whose Gather is the red gather.
  (multiple-value-bind (texture sampler coordinate)
      (hlsl-sampling-operands context expression)
    (note-hlsl-occurrence
     context expression
     (format nil "~A.~:[GatherRed~;Gather~](~A, ~A)"
             texture
             (hlsl-depth-texture-p (first (shader:shader-call-operands
                                           expression)))
             sampler coordinate))))

(define-hlsl-operator shader:gather-compare (context expression)
  (check-hlsl-comparison-sampler context expression)
  (multiple-value-bind (texture sampler coordinate rest)
      (hlsl-sampling-operands context expression)
    (note-hlsl-occurrence
     context expression
     (format nil "~A.GatherCmp(~A, ~A, ~A)"
             texture sampler coordinate (first rest)))))

(defun hlsl-widen-storage-texel (type text)
  "A one-channel storage texel read as the language's four channels."
  (if (= 1 (shader:storage-texture-format-channels
            (shader:shader-type-storage-format type)))
      (if (eq :uint (shader:shader-type-scalar-kind
                     (shader:find-shader-type
                      (shader:shader-type-sample-result-type type))))
          (format nil "uint4(~A, 0u, 0u, 1u)" text)
          (format nil "float4(~A, 0.0f, 0.0f, 1.0f)" text))
      text))

(define-hlsl-operator shader:texel-load (context expression)
  (let* ((operands (shader:shader-call-operands expression))
         (type (shader:shader-expression-type (first operands)))
         (texts (lower-hlsl-operands context expression)))
    (destructuring-bind (texture coordinate &rest rest) texts
      (note-hlsl-occurrence
       context expression
       (if (shader:shader-storage-texture-type-p type)
           (hlsl-widen-storage-texel
            type (format nil "~A[~A]" texture coordinate))
           (let ((level (or (if (shader:shader-texture-arrayed-p type)
                                (second rest)
                                (first rest))
                            "0")))
             (hlsl-widen-depth
              (first operands)
              (ecase (shader:shader-type-texture-dimension type)
                (:2d (format nil "~A.Load(int3(int2(~A), ~A))"
                             texture coordinate
                             (if (string= level "0")
                                 level
                                 (format nil "int(~A)" level))))
                (:2d-array
                 (format nil "~A.Load(int4(int2(~A), int(~A), int(~A)))"
                         texture coordinate (first rest) level))
                (:3d (format nil "~A.Load(int4(int3(~A), int(~A)))"
                             texture coordinate level))))))))))

(define-hlsl-operator shader:texture-size (context expression)
  (let* ((operands (shader:shader-call-operands expression))
         (type (shader:shader-expression-type (first operands)))
         (texts (lower-hlsl-operands context expression))
         (texture (first texts))
         (level (or (second texts) "0u"))
         (base (hlsl-temporary-name context "size"))
         (width (format nil "~A_width" base))
         (height (format nil "~A_height" base))
         (third (and (eq :uvec3 (shader:texture-size-type type))
                     (format nil "~A_~:[depth~;layers~]" base
                             (shader:shader-texture-arrayed-p type))))
         (levels (format nil "~A_levels" base)))
    (if (shader:shader-storage-texture-type-p type)
        (push-hlsl-pending
         context
         (hlsl-line expression "uint ~A, ~A;" width height)
         (hlsl-line expression "~A.GetDimensions(~A, ~A);"
                    texture width height))
        (push-hlsl-pending
         context
         (hlsl-line expression "uint ~A, ~A~@[, ~A~], ~A;"
                    width height third levels)
         (hlsl-line expression "~A.GetDimensions(~A, ~A, ~A~@[, ~A~], ~A);"
                    texture level width height third levels)))
    (note-hlsl-occurrence
     context expression
     (if third
         (format nil "uint3(~A, ~A, ~A)" width height third)
         (format nil "uint2(~A, ~A)" width height)))))

;;; Atomics and waves.

(defun hlsl-atomic-target-text (context expression)
  (hlsl-element-text context (shader:shader-atomic-call-target expression)
                     (first (shader:shader-call-operands expression))))

(defun lower-hlsl-atomic (context expression function)
  (let* ((destination (hlsl-atomic-target-text context expression))
         (operands (mapcar (lambda (operand)
                             (hlsl-text (lower-hlsl-expression context operand)))
                           (rest (shader:shader-call-operands expression))))
         (original (hlsl-temporary-name context "atomic")))
    (push-hlsl-pending
     context
     (hlsl-line expression "uint ~A;" original)
     (hlsl-line expression "~A(~A, ~{~A, ~}~A);"
                function destination operands original))
    (note-hlsl-occurrence context expression original)))

(macrolet ((atomics (&rest pairs)
             `(progn
                ,@(loop for (operator function) on pairs by #'cddr
                        collect `(defmethod shader:lower-shader-call
                                     ((operator (eql ',operator))
                                      (context hlsl-lowering-context)
                                      (expression shader:shader-atomic-call))
                                   (declare (ignore operator))
                                   (lower-hlsl-atomic context expression
                                                      ,function))))))
  (atomics shader:atomic-add "InterlockedAdd"
           shader:atomic-min "InterlockedMin"
           shader:atomic-max "InterlockedMax"
           shader:atomic-and "InterlockedAnd"
           shader:atomic-or "InterlockedOr"
           shader:atomic-xor "InterlockedXor"
           shader:atomic-exchange "InterlockedExchange"
           ;; The comparand precedes the value, as the language's does.
           shader:atomic-compare-exchange "InterlockedCompareExchange"))

(define-hlsl-operator shader:wave-active-sum (context expression)
  (lower-hlsl-function-call context expression "WaveActiveSum"))

(define-hlsl-operator shader:wave-prefix-sum (context expression)
  (lower-hlsl-function-call context expression "WavePrefixSum"))

(define-hlsl-operator shader:wave-ballot (context expression)
  (lower-hlsl-function-call context expression "WaveActiveBallot"))

;;; Statements.

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context) (statement shader:shader-block-statement))
  (nconc (loop for binding in (shader:shader-block-statement-bindings statement)
               nconc (lower-hlsl-local-binding context binding :unique t))
         (mapcan (lambda (child) (lower-hlsl-statement context child))
                 (shader:shader-block-statement-statements statement))))

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context) (statement shader:shader-discard))
  (declare (ignore context))
  (list (hlsl-line statement "discard;")))

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context) (statement shader:shader-barrier))
  (declare (ignore context))
  (list (hlsl-line statement
                   (ecase (shader:shader-barrier-memory statement)
                     (:workgroup "GroupMemoryBarrierWithGroupSync();")
                     (:storage "AllMemoryBarrierWithGroupSync();")))))

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context) (statement shader:shader-evaluation))
  ;; Every HLSL effect is already a pending statement; its value is unused.
  (lower-hlsl-expression context (shader:shader-evaluation-expression statement))
  (drain-hlsl-pending-statements context))

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context) (statement shader:shader-shared-store))
  (let* ((target (hlsl-element-text
                  context (shader:shader-shared-store-array statement)
                  (shader:shader-shared-store-index statement)))
         (value (hlsl-text
                 (lower-hlsl-expression
                  context (shader:shader-shared-store-value statement)))))
    (append (drain-hlsl-pending-statements context)
            (list (hlsl-line statement "~A = ~A;" target value)))))

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context) (statement shader:shader-texel-store))
  (let* ((texture (shader:shader-texel-store-texture statement))
         (type (shader:shader-declaration-type texture))
         (coordinate (hlsl-text
                      (lower-hlsl-expression
                       context (shader:shader-texel-store-coordinate statement))))
         (value (hlsl-text
                 (lower-hlsl-expression
                  context (shader:shader-texel-store-value statement)))))
    (append (drain-hlsl-pending-statements context)
            (list (hlsl-line statement "~A[~A] = ~:[~A~;(~A).x~];"
                             (gethash texture (hlsl-context-references context))
                             coordinate
                             (= 1 (shader:storage-texture-format-channels
                                   (shader:shader-type-storage-format type)))
                             value)))))
