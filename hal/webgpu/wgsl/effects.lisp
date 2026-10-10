;;; WGSL for buffers, textures, storage textures, and stage effects.
;;;
;;; Sampled textures are texture_2d, texture_2d_array, texture_cube, and
;;; texture_3d of f32 or u32, and the depth textures texture_depth_2d and
;;; texture_depth_2d_array, whose samples and loads are one f32 widened to
;;; the language's vec4.  A storage texture is a texture_storage_2d of its
;;; format with the least access the module needs.  Workgroup arrays are
;;; var<workgroup>, barriers workgroupBarrier and storageBarrier, and
;;; atomics the atomic built-ins on atomic<u32> elements: a buffer or
;;; workgroup array that any atomic touches is atomic throughout the
;;; module, so every access to it is an atomic load, store, or
;;; read-modify-write.

(in-package #:luv.wgsl)

;;; Buffers and workgroup memory.

(defun wgsl-element-text (context target index)
  (format nil "~A[~A]"
          (gethash target (wgsl-context-references context))
          (wgsl-text context index)))

(defun wgsl-element-read-text (context target index)
  (let ((element (wgsl-element-text context target index)))
    (if (wgsl-atomic-target-p context target)
        (format nil "atomicLoad(&~A)" element)
        element)))

(defun wgsl-element-store (context target index value)
  "The statements storing VALUE, an expression, to TARGET's element INDEX."
  (let ((element (wgsl-element-text context target index))
        (value (wgsl-text context value)))
    (append (drain-wgsl-pending-statements context)
            (list (if (wgsl-atomic-target-p context target)
                      (wgsl-line "atomicStore(&~A, ~A);" element value)
                      (wgsl-line "~A = ~A;" element value))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-buffer-element))
  (note-wgsl-occurrence
   context expression
   (wgsl-element-read-text context
                           (shader:shader-buffer-element-buffer expression)
                           (shader:shader-buffer-element-index expression))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-shared-element))
  (note-wgsl-occurrence
   context expression
   (wgsl-element-read-text context
                           (shader:shader-shared-element-array expression)
                           (shader:shader-shared-element-index expression))))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context) (statement shader:shader-buffer-store))
  (wgsl-element-store context
                      (shader:shader-buffer-store-buffer statement)
                      (shader:shader-buffer-store-index statement)
                      (shader:shader-buffer-store-value statement)))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context) (statement shader:shader-shared-store))
  (wgsl-element-store context
                      (shader:shader-shared-store-array statement)
                      (shader:shader-shared-store-index statement)
                      (shader:shader-shared-store-value statement)))

(defun write-wgsl-workgroup-variables (stream context)
  (dolist (array (shader:shader-specification-shared-arrays
                  (wgsl-context-specification context)))
    (format stream "var<workgroup> ~A: array<~A, ~D>;~%~%"
            (gethash array (wgsl-context-references context))
            (if (wgsl-atomic-target-p context array)
                "atomic<u32>"
                (wgsl-type-name (shader:shader-declaration-type array)))
            (shader:shader-shared-array-element-count array))))

;;; Texture types.

(defun wgsl-texture-type-name (type form)
  (case (shader:shader-type-name type)
    (:texture-2d "texture_2d<f32>")
    (:depth-texture-2d "texture_depth_2d")
    (:uint-texture-2d "texture_2d<u32>")
    (:texture-2d-array "texture_2d_array<f32>")
    (:depth-texture-2d-array "texture_depth_2d_array")
    (:texture-cube "texture_cube<f32>")
    (:texture-3d "texture_3d<f32>")
    (otherwise
     (wgsl-failure form :unsupported-wgsl-type
                   (shader:shader-type-name type)))))

(defparameter *wgsl-read-write-texel-formats* '(:r32f :r32ui)
  "The storage texture formats WebGPU lets one module both load and store.")

(defun wgsl-storage-texture-uses (specification)
  "Two lists: the storage textures SPECIFICATION loads texels from, and
those it stores texels to."
  (let ((loaded nil) (stored nil))
    (shader:map-shader-specification-expressions
     (lambda (expression)
       (when (and (typep expression 'shader:shader-call)
                  (eq 'shader:texel-load
                      (shader:shader-call-operator expression)))
         (let ((texture (shader:shader-resource-target
                         (first (shader:shader-call-operands expression)))))
           (when (and texture
                      (shader:shader-storage-texture-type-p
                       (shader:shader-declaration-type texture)))
             (pushnew texture loaded)))))
     specification)
    (labels ((walk (statements)
               (dolist (statement statements)
                 (when (typep statement 'shader:shader-texel-store)
                   (pushnew (shader:shader-texel-store-texture statement)
                            stored))
                 (walk (shader:shader-statement-children statement)))))
      (walk (shader:shader-specification-statements specification)))
    (values loaded stored)))

(defun wgsl-storage-texture-access (texture &rest specifications)
  "The access a module declares for the storage texture TEXTURE, given the
SPECIFICATIONS that share it by name: :WRITE when they only store to it (or
never touch its texels), :READ when they only load from it, and :READ-WRITE
when they do both, which WebGPU allows for one-channel 32-bit formats alone.
:READ and :READ-WRITE need the readonly_and_readwrite_storage_textures
language extension, which the lowering then requires."
  (let ((key (shader:shader-resource-key texture))
        (format (shader:shader-type-storage-format
                 (shader:shader-declaration-type texture)))
        (loaded-p nil) (stored-p nil))
    (dolist (specification specifications)
      (multiple-value-bind (loaded stored)
          (wgsl-storage-texture-uses specification)
        (when (find key loaded :key #'shader:shader-resource-key)
          (setf loaded-p t))
        (when (find key stored :key #'shader:shader-resource-key)
          (setf stored-p t))))
    (cond ((not loaded-p) :write)
          ((not stored-p) :read)
          ((member format *wgsl-read-write-texel-formats*) :read-write)
          (t
           (wgsl-failure (shader:shader-object-source-form texture)
                         :unsupported-wgsl-read-write-storage-texture
                         format)))))

(defun wgsl-module-texture-access (context texture)
  "The storage texture TEXTURE's access in the module CONTEXT lowers."
  (or (and (wgsl-context-storage-texture-access context)
           (funcall (wgsl-context-storage-texture-access context) texture))
      (wgsl-storage-texture-access
       texture (wgsl-context-specification context))))

(defun wgsl-storage-texture-type-name (context texture)
  (format nil "texture_storage_2d<~A, ~A>"
          (ecase (shader:shader-type-storage-format
                  (shader:shader-declaration-type texture))
            (:rgba32f "rgba32float")
            (:rgba16f "rgba16float")
            (:rgba8 "rgba8unorm")
            (:r32f "r32float")
            (:r32ui "r32uint"))
          (ecase (wgsl-module-texture-access context texture)
            (:read "read")
            (:write "write")
            (:read-write "read_write"))))

;;; Sampling.

(defun wgsl-sampling-operands (context expression)
  "The texture's type; the texture, sampler, and coordinate texts; the
layer text of an array texture, or NIL; and the operator's own operands."
  (let* ((type (shader:shader-expression-type
                (first (shader:shader-call-operands expression))))
         (texts (lower-wgsl-operands context expression))
         (arrayed-p (shader:shader-texture-arrayed-p type)))
    (values type (first texts) (second texts) (third texts)
            (and arrayed-p (fourth texts))
            (nthcdr (if arrayed-p 4 3) texts))))

(defun wgsl-widen-depth (type text)
  "A depth texture yields one f32; the language's sample is a vec4 with
the depth in every component, as MSL's float4(depth)."
  (if (shader:shader-type-image-depth-p type)
      (format nil "vec4<f32>(~A)" text)
      text))

(defun check-wgsl-sampler (context expression comparison-p)
  "Signal unless EXPRESSION's sampler is declared as its operator needs:
sampler_comparison to compare depth, sampler otherwise."
  (let ((sampler (shader:shader-resource-target
                  (second (shader:shader-call-operands expression)))))
    (unless (eq (not comparison-p)
                (not (and sampler
                          (member (shader:shader-resource-key sampler)
                                  (wgsl-context-comparison-samplers
                                   context)))))
      (wgsl-failure (shader:shader-expression-source-form expression)
                    (if comparison-p
                        :wgsl-comparison-sampler-unknown
                        :wgsl-comparison-sampler-samples)
                    (and sampler (shader:shader-object-name sampler))))))

(defun check-wgsl-filterable (expression type &key depth)
  "WGSL filters f32 textures only, and gives depth textures no biased or
gradient sampling (DEPTH is NIL for an operator that has none)."
  (when (or (eq :uint (shader:shader-type-scalar-kind
                       (shader:find-shader-type
                        (shader:shader-type-sample-result-type type))))
            (and (not depth) (shader:shader-type-image-depth-p type)))
    (wgsl-failure (shader:shader-expression-source-form expression)
                  :unsupported-wgsl-texture-sampling
                  (list (shader:shader-call-operator expression)
                        (shader:shader-type-name type)))))

(define-wgsl-operator shader:sample (context expression)
  ;; Implicit derivatives exist only in fragments.  Elsewhere sample the
  ;; base level, as Metal and Direct3D do.
  (check-wgsl-sampler context expression nil)
  (multiple-value-bind (type texture sampler coordinate layer)
      (wgsl-sampling-operands context expression)
    (check-wgsl-filterable expression type :depth t)
    (note-wgsl-occurrence
     context expression
     (wgsl-widen-depth
      type
      (if (eq :fragment (wgsl-context-stage context))
          (format nil "textureSample(~A, ~A, ~A~@[, ~A~])"
                  texture sampler coordinate layer)
          (format nil "textureSampleLevel(~A, ~A, ~A~@[, ~A~], ~A)"
                  texture sampler coordinate layer
                  (if (shader:shader-type-image-depth-p type) "0u" "0.0f")))))))

(define-wgsl-operator shader:sample-level (context expression)
  ;; A depth texture's level is an integer: its levels do not blend.
  (check-wgsl-sampler context expression nil)
  (multiple-value-bind (type texture sampler coordinate layer own)
      (wgsl-sampling-operands context expression)
    (check-wgsl-filterable expression type :depth t)
    (note-wgsl-occurrence
     context expression
     (wgsl-widen-depth
      type
      (format nil "textureSampleLevel(~A, ~A, ~A~@[, ~A~], ~A)"
              texture sampler coordinate layer
              (if (shader:shader-type-image-depth-p type)
                  (format nil "u32(~A)" (first own))
                  (first own)))))))

(define-wgsl-operator shader:sample-bias (context expression)
  (check-wgsl-sampler context expression nil)
  (multiple-value-bind (type texture sampler coordinate layer own)
      (wgsl-sampling-operands context expression)
    (check-wgsl-filterable expression type)
    (note-wgsl-occurrence
     context expression
     (format nil "textureSampleBias(~A, ~A, ~A~@[, ~A~], ~A)"
             texture sampler coordinate layer (first own)))))

(define-wgsl-operator shader:sample-grad (context expression)
  (check-wgsl-sampler context expression nil)
  (multiple-value-bind (type texture sampler coordinate layer own)
      (wgsl-sampling-operands context expression)
    (check-wgsl-filterable expression type)
    (note-wgsl-occurrence
     context expression
     (format nil "textureSampleGrad(~A, ~A, ~A~@[, ~A~], ~A, ~A)"
             texture sampler coordinate layer (first own) (second own)))))

(define-wgsl-operator shader:sample-compare (context expression)
  ;; Depth comparison reads the base level in every stage, as the HLSL
  ;; lowering's SampleCmpLevelZero: shadow maps have one level, and the
  ;; implicit-derivative form is a fragment's alone.
  (check-wgsl-sampler context expression t)
  (multiple-value-bind (type texture sampler coordinate layer own)
      (wgsl-sampling-operands context expression)
    (declare (ignore type))
    (note-wgsl-occurrence
     context expression
     (format nil "textureSampleCompareLevel(~A, ~A, ~A~@[, ~A~], ~A)"
             texture sampler coordinate layer (first own)))))

(define-wgsl-operator shader:gather (context expression)
  ;; A colour gather names its channel, the first; a depth texture has one.
  (check-wgsl-sampler context expression nil)
  (multiple-value-bind (type texture sampler coordinate layer)
      (wgsl-sampling-operands context expression)
    (note-wgsl-occurrence
     context expression
     (format nil "textureGather(~:[0, ~;~]~A, ~A, ~A~@[, ~A~])"
             (shader:shader-type-image-depth-p type)
             texture sampler coordinate layer))))

(define-wgsl-operator shader:gather-compare (context expression)
  (check-wgsl-sampler context expression t)
  (multiple-value-bind (type texture sampler coordinate layer own)
      (wgsl-sampling-operands context expression)
    (declare (ignore type))
    (note-wgsl-occurrence
     context expression
     (format nil "textureGatherCompare(~A, ~A, ~A~@[, ~A~], ~A)"
             texture sampler coordinate layer (first own)))))

;;; Texels and sizes.

(define-wgsl-operator shader:texel-load (context expression)
  ;; textureLoad(texture, coordinate[, layer], level); a storage texture is
  ;; one level as bound, and its texel already has the language's four
  ;; channels whatever its format.
  (let ((type (shader:shader-expression-type
               (first (shader:shader-call-operands expression)))))
    (destructuring-bind (texture coordinate &rest rest)
        (lower-wgsl-operands context expression)
      (note-wgsl-occurrence
       context expression
       (if (shader:shader-storage-texture-type-p type)
           (format nil "textureLoad(~A, ~A)" texture coordinate)
           (let ((arrayed-p (shader:shader-texture-arrayed-p type)))
             (wgsl-widen-depth
              type
              (format nil "textureLoad(~A, ~A~@[, ~A~], ~A)"
                      texture coordinate (and arrayed-p (first rest))
                      (or (if arrayed-p (second rest) (first rest))
                          "0u")))))))))

(define-wgsl-operator shader:texture-size (context expression)
  (let ((type (shader:shader-expression-type
               (first (shader:shader-call-operands expression)))))
    (destructuring-bind (texture &optional level)
        (lower-wgsl-operands context expression)
      (let ((dimensions (format nil "textureDimensions(~A~@[, ~A~])"
                                texture level)))
        (note-wgsl-occurrence
         context expression
         (if (shader:shader-texture-arrayed-p type)
             (format nil "vec3<u32>(~A, textureNumLayers(~A))"
                     dimensions texture)
             dimensions))))))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context) (statement shader:shader-texel-store))
  (let ((coordinate
          (wgsl-text context (shader:shader-texel-store-coordinate statement)))
        (value (wgsl-text context (shader:shader-texel-store-value statement))))
    (append (drain-wgsl-pending-statements context)
            (list (wgsl-line "textureStore(~A, ~A, ~A);"
                             (gethash (shader:shader-texel-store-texture
                                       statement)
                                      (wgsl-context-references context))
                             coordinate value)))))

;;; Atomics.  Each built-in takes a pointer to the atomic element and
;;; returns the element's previous value, as the language's operators do.

(defun wgsl-atomic-pointer (context expression)
  (format nil "&~A"
          (wgsl-element-text context
                             (shader:shader-atomic-call-target expression)
                             (first (shader:shader-call-operands expression)))))

(macrolet ((atomics (&rest pairs)
             `(progn
                ,@(loop for (operator function) on pairs by #'cddr
                        collect
                        `(defmethod shader:lower-shader-call
                             ((operator (eql ',operator))
                              (context wgsl-lowering-context)
                              (expression shader:shader-atomic-call))
                           (declare (ignore operator))
                           (note-wgsl-occurrence
                            context expression
                            (format nil "~A(~A, ~A)" ,function
                                    (wgsl-atomic-pointer context expression)
                                    (wgsl-text
                                     context
                                     (second (shader:shader-call-operands
                                              expression))))))))))
  (atomics shader:atomic-add "atomicAdd"
           shader:atomic-min "atomicMin"
           shader:atomic-max "atomicMax"
           shader:atomic-and "atomicAnd"
           shader:atomic-or "atomicOr"
           shader:atomic-xor "atomicXor"
           shader:atomic-exchange "atomicExchange"))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:atomic-compare-exchange))
     (context wgsl-lowering-context)
     (expression shader:shader-atomic-call))
  "WGSL's only compare-exchange is weak: atomicCompareExchangeWeak returns
a structure of the element's old value and whether it was exchanged, and may
fail spuriously, having exchanged nothing although the old value equals the
comparand.  Retry exactly those failures, so the result is the element's
previous value, equal to the comparand exactly when it was replaced, as in
HLSL and SPIR-V."
  (declare (ignore operator))
  (destructuring-bind (index comparand value)
      (shader:shader-call-operands expression)
    (declare (ignore index))
    (let ((pointer (wgsl-atomic-pointer context expression))
          (comparand (wgsl-text context comparand))
          (value (wgsl-text context value))
          (expected (wgsl-temporary-name context "comparand"))
          (observed (wgsl-temporary-name context "observed"))
          (exchange (wgsl-temporary-name context "exchange")))
      (push-wgsl-pending
       context
       (wgsl-line "let ~A: u32 = ~A;" expected comparand)
       (wgsl-line "var ~A: u32 = ~A;" observed expected)
       (wgsl-line "loop {")
       (wgsl-line "  let ~A = atomicCompareExchangeWeak(~A, ~A, ~A);"
                  exchange pointer expected value)
       (wgsl-line "  ~A = ~A.old_value;" observed exchange)
       (wgsl-line "  if (~A.exchanged || (~A != ~A)) { break; }"
                  exchange observed expected)
       (wgsl-line "}"))
      (note-wgsl-occurrence context expression observed))))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context) (statement shader:shader-evaluation))
  (let* ((expression (shader:shader-evaluation-expression statement))
         (value (wgsl-text context expression))
         (pending (drain-wgsl-pending-statements context)))
    ;; A compare-exchange's effect is its pending loop; another atomic is
    ;; the call itself, its value assigned to nothing.
    (if (eq 'shader:atomic-compare-exchange
            (shader:shader-call-operator expression))
        pending
        (append pending (list (wgsl-line "_ = ~A;" value))))))

;;; Barriers and waves.

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context) (statement shader:shader-barrier))
  (ecase (shader:shader-barrier-memory statement)
    (:workgroup (list (wgsl-line "workgroupBarrier();")))
    ;; STORAGE-BARRIER orders workgroup memory, storage buffers, and
    ;; storage textures.  textureBarrier belongs to the language extension
    ;; a module needs to read a storage texture at all.
    (:storage
     (list* (wgsl-line "workgroupBarrier();")
            (wgsl-line "storageBarrier();")
            (and (wgsl-module-requirements context)
                 (list (wgsl-line "textureBarrier();")))))))

(macrolet ((waves (&rest operators)
             `(progn
                ,@(loop for operator in operators
                        collect
                        `(define-wgsl-operator ,operator (context expression)
                           (declare (ignore context))
                           (wgsl-failure
                            (shader:shader-expression-source-form expression)
                            :unsupported-wgsl-wave-operation ',operator))))))
  ;; Standard WGSL has no subgroup operations.
  (waves shader:wave-active-sum shader:wave-prefix-sum shader:wave-ballot))
