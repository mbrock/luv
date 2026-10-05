;;; SPIR-V for textures, storage textures, and stage effects.
;;;
;;; The vocabulary below extends the assembler with exactly what these
;;; features emit: image sampling with explicit operands, gathers, reads,
;;; writes and size queries; OpKill; barriers; atomics; and subgroup
;;; arithmetic.  Lowering asks for capabilities and a module version as it
;;; uses them, so a module that uses none of this is unchanged.

(in-package #:luv.spir-v)

;;; Instructions.

(define-instruction image-sample-implicit-lod-with
    (sampled-image coordinate image-operands &rest operands)
  (:opcode 87) (:result :typed)
  (:operands :id :id (:enum image-operands) :id))
(define-instruction image-sample-dref-explicit-lod
    (sampled-image coordinate depth-reference image-operands &rest operands)
  (:opcode 90) (:result :typed)
  (:operands :id :id :id (:enum image-operands) :id))
(define-instruction image-fetch-with
    (image coordinate image-operands &rest operands)
  (:opcode 95) (:result :typed)
  (:operands :id :id (:enum image-operands) :id))
(define-instruction image-gather (sampled-image coordinate component)
  (:opcode 96) (:result :typed) (:operands :id :id :id))
(define-instruction image-dref-gather (sampled-image coordinate depth-reference)
  (:opcode 97) (:result :typed) (:operands :id :id :id))
(define-instruction image-read (image coordinate)
  (:opcode 98) (:result :typed) (:operands :id :id))
(define-instruction image-query-size-lod (image level-of-detail)
  (:opcode 103) (:result :typed) (:operands :id :id))
(define-instruction image-query-size (image)
  (:opcode 104) (:result :typed) (:operands :id))
(define-instruction control-barrier (execution memory semantics)
  (:opcode 224) (:operands :id :id :id))
(define-instruction atomic-load (pointer memory semantics)
  (:opcode 227) (:result :typed) (:operands :id :id :id))
(define-instruction atomic-store (pointer memory semantics value)
  (:opcode 228) (:operands :id :id :id :id))
(define-instruction atomic-compare-exchange
    (pointer memory equal unequal value comparator)
  (:opcode 230) (:result :typed) (:operands :id :id :id :id :id :id))

(defmacro define-atomic-instructions (&body definitions)
  `(progn
     ,@(loop for (name opcode) in definitions
             collect `(define-instruction ,name (pointer memory semantics value)
                        (:opcode ,opcode) (:result :typed)
                        (:operands :id :id :id :id)))))

(define-atomic-instructions
  (atomic-exchange 229)
  (atomic-i-add 234)
  (atomic-u-min 237)
  (atomic-u-max 239)
  (atomic-and 240)
  (atomic-or 241)
  (atomic-xor 242))

(define-instruction kill () (:opcode 252))
(define-instruction group-non-uniform-ballot (execution predicate)
  (:opcode 339) (:result :typed) (:operands :id :id))
(define-instruction group-non-uniform-i-add (execution operation value)
  (:opcode 349) (:result :typed)
  (:operands :id (:enum group-operation) :id))
(define-instruction group-non-uniform-f-add (execution operation value)
  (:opcode 350) (:result :typed)
  (:operands :id (:enum group-operation) :id))

(extend-enumeration capability
  (sample-rate-shading 35)
  (image-query 50)
  (group-non-uniform 61)
  (group-non-uniform-arithmetic 63)
  (group-non-uniform-ballot 64))
(extend-enumeration execution-mode (depth-replacing 12))
(extend-enumeration built-in
  (frag-coord 15)
  (front-facing 17)
  (sample-id 18)
  (frag-depth 22)
  (subgroup-size 36)
  (subgroup-local-invocation-id 41))
(extend-enumeration dim (3d 2) (cube 3))
(extend-enumeration image-format
  (rgba32f 1) (rgba16f 2) (r32f 3) (r32ui 33))
(extend-enumeration image-operands (bias #x1) (grad #x4))
(extend-enumeration storage-class (workgroup 4))
(define-enumeration group-operation
  (reduce 0) (inclusive-scan 1) (exclusive-scan 2))

;;; Scopes and memory semantics are constant operands.
(defparameter *scope-device* 1)
(defparameter *scope-workgroup* 2)
(defparameter *scope-subgroup* 3)
(defparameter *semantics-acquire-release* #x8)
(defparameter *semantics-uniform-memory* #x40)
(defparameter *semantics-workgroup-memory* #x100)
(defparameter *semantics-image-memory* #x800)

;;; Requirements.

(defun require-spir-v-capability (context capability)
  (unless (member capability (context-required-capabilities context))
    (setf (context-required-capabilities context)
          (append (context-required-capabilities context)
                  (list capability)))))

(defun require-spir-v-version (context version)
  (setf (context-minimum-version context)
        (max version (context-minimum-version context))))

(defun require-subgroups (context &optional capability)
  ;; Subgroup operations are core in SPIR-V 1.3 (Vulkan 1.1).
  (require-spir-v-version context #x00010300)
  (require-spir-v-capability context 'group-non-uniform)
  (when capability
    (require-spir-v-capability context capability)))

(defun spir-v-built-in-name (context built-in)
  "The BuiltIn enumerant for the language's BUILT-IN, noting what it needs."
  (case built-in
    (:sample-index
     (require-spir-v-capability context 'sample-rate-shading)
     'sample-id)
    (:wave-lane-index (require-subgroups context) 'subgroup-local-invocation-id)
    (:wave-lane-count (require-subgroups context) 'subgroup-size)
    (otherwise built-in)))

(defun shader-effect-execution-modes (specification main-id)
  (when (find :frag-depth (shader-specification-outputs specification)
              :key #'shader-interface-built-in)
    (list (make-instance 'spir-v-execution-mode
                         :function main-id :name 'depth-replacing))))

;;; Types and variables.

(defun shader-image-type-form (context id type)
  "The OpTypeImage form of a sampled or storage texture TYPE."
  (let* ((storage-p (shader-storage-texture-type-p type))
         (dimension (shader-type-texture-dimension type)))
    (list id 'type-image
          (ensure-shader-type-id
           context
           (ecase (shader-type-scalar-kind
                   (find-shader-type (shader-type-sample-result-type type)))
             (:float :float)
             (:uint :uint)))
          (ecase dimension
            ((:2d :2d-array) '2d)
            (:cube 'cube)
            (:3d '3d))
          (if (shader-type-image-depth-p type) 1 0)
          (if (eq dimension :2d-array) 1 0)
          0
          (if storage-p 2 1)
          (if storage-p
              (ecase (shader-type-storage-format type)
                (:rgba32f 'rgba32f)
                (:rgba16f 'rgba16f)
                (:rgba8 'rgba8)
                (:r32f 'r32f)
                (:r32ui 'r32ui))
              'unknown))))

(defun register-shared-array (context array)
  "Declare ARRAY as one Workgroup-storage variable of its element array."
  (let* ((name (shader-object-name array))
         (array-type-id
           (ensure-array-type-id context (shader-declaration-type array)
                                 (shader-shared-array-element-count array)))
         (pointer-id
           (ensure-pointer-to-type-id
            context 'workgroup array-type-id
            (format nil "~A-WORKGROUP-POINTER" name)))
         (variable-id (reserve-shader-id context name)))
    (append-context-form 'variable-declarations context
                         (list variable-id 'variable pointer-id 'workgroup))
    (setf (gethash array (context-shared-array-variables context))
          variable-id)))

(defun shader-element-pointer (context target index expression)
  "An access chain to element INDEX (an expression) of a storage buffer or
workgroup array TARGET."
  (let ((pointer (fresh-shader-id
                  context
                  (format nil "~A-ELEMENT-POINTER" (shader-object-name target))))
        (index-id (lower-shader-expression context index)))
    (emit-shader-instruction
     context expression
     (etypecase target
       (shader-storage-buffer
        (list pointer 'access-chain
              (ensure-pointer-type-id
               context 'storage-buffer
               (shader-storage-buffer-element-type target))
              (gethash target (context-variable-ids context))
              (ensure-shader-uint-constant context 0)
              index-id))
       (shader-shared-array
        (list pointer 'access-chain
              (ensure-pointer-type-id
               context 'workgroup (shader-declaration-type target))
              (gethash target (context-shared-array-variables context))
              index-id))))
    pointer))

(defmethod lower-shader-expression-value
    (context (expression shader-shared-element))
  (let ((array (shader-shared-element-array expression)))
    (emit-value-instruction
     context expression (shader-declaration-type array) 'load
     (list (shader-element-pointer
            context array (shader-shared-element-index expression)
            expression)))))

(defmethod shader-expression-provenance-name
    ((expression shader-shared-element))
  (shader-object-name (shader-shared-element-array expression)))

;;; Textures.

(defun spir-v-texture-coordinate (context expression texture-type coordinate
                                  layer &key texel)
  "COORDINATE's value id, with an array's LAYER appended: as a float for
sampling, or as an unsigned integer when TEXEL."
  (let ((coordinate-id (lower-shader-expression context coordinate)))
    (if (and layer (shader-texture-arrayed-p texture-type))
        (let ((layer-id (lower-shader-expression context layer)))
          (if texel
              (emit-value-instruction
               context expression :uvec3 'composite-construct
               (list coordinate-id layer-id))
              (emit-value-instruction
               context expression :vec3 'composite-construct
               (list coordinate-id
                     (emit-value-instruction
                      context expression :float 'convert-u-to-f
                      (list layer-id))))))
        coordinate-id)))

(defun spir-v-sampled-image (context expression)
  "Lower the call's operands in order, then combine its texture and sampler.
Return the sampled image, the coordinate (an array's layer folded in), and
the ids of the remaining operands."
  (destructuring-bind (texture sampler coordinate &rest rest)
      (shader-call-operands expression)
    (let* ((type (shader-expression-type texture))
           (arrayed-p (shader-texture-arrayed-p type))
           (texture-id (lower-shader-expression context texture))
           (sampler-id (lower-shader-expression context sampler))
           (coordinate-id
             (spir-v-texture-coordinate context expression type coordinate
                                        (and arrayed-p (first rest))))
           (rest-ids (mapcar (lambda (operand)
                               (lower-shader-expression context operand))
                             (if arrayed-p (rest rest) rest)))
           (sampled-id (fresh-shader-id context
                                        (expression-result-name expression))))
      (emit-shader-instruction
       context expression
       (list sampled-id 'sampled-image
             (ensure-sampled-image-type-id context type)
             texture-id sampler-id))
      (values sampled-id coordinate-id rest-ids))))

(defun lower-spir-v-sample (context expression instruction &rest image-operands)
  "Sample through INSTRUCTION; IMAGE-OPERANDS names its operand mask, whose
operand ids follow from the call's own operands."
  (multiple-value-bind (sampled coordinate rest)
      (spir-v-sampled-image context expression)
    (emit-value-instruction
     context expression (shader-expression-type expression) instruction
     (list* sampled coordinate (append image-operands rest)))))

(defmethod lower-shader-call ((operator (eql 'sample)) context expression)
  (lower-spir-v-sample context expression 'image-sample-implicit-lod))

(defmethod lower-shader-call ((operator (eql 'sample-level)) context expression)
  (lower-spir-v-sample context expression 'image-sample-explicit-lod 'lod))

(defmethod lower-shader-call ((operator (eql 'sample-bias)) context expression)
  (lower-spir-v-sample context expression 'image-sample-implicit-lod-with 'bias))

(defmethod lower-shader-call ((operator (eql 'sample-grad)) context expression)
  (lower-spir-v-sample context expression 'image-sample-explicit-lod 'grad))

(defmethod lower-shader-call
    ((operator (eql 'sample-compare)) context expression)
  (lower-spir-v-sample context expression 'image-sample-dref-implicit-lod))

(defmethod lower-shader-call ((operator (eql 'gather)) context expression)
  (multiple-value-bind (sampled coordinate) (spir-v-sampled-image context expression)
    (emit-value-instruction
     context expression (shader-expression-type expression) 'image-gather
     (list sampled coordinate (ensure-shader-uint-constant context 0)))))

(defmethod lower-shader-call ((operator (eql 'gather-compare)) context expression)
  (lower-spir-v-sample context expression 'image-dref-gather))

(defmethod lower-shader-call ((operator (eql 'texel-load)) context expression)
  (destructuring-bind (texture coordinate &rest rest)
      (shader-call-operands expression)
    (let* ((type (shader-expression-type texture))
           (arrayed-p (shader-texture-arrayed-p type))
           (texture-id (lower-shader-expression context texture))
           (coordinate-id
             (spir-v-texture-coordinate context expression type coordinate
                                        (and arrayed-p (first rest))
                                        :texel t))
           (level (if arrayed-p (second rest) (first rest))))
      (cond ((shader-storage-texture-type-p type)
             (emit-value-instruction
              context expression (shader-expression-type expression)
              'image-read (list texture-id coordinate-id)))
            (level
             (emit-value-instruction
              context expression (shader-expression-type expression)
              'image-fetch-with
              (list texture-id coordinate-id 'lod
                    (lower-shader-expression context level))))
            (t
             (emit-value-instruction
              context expression (shader-expression-type expression)
              'image-fetch (list texture-id coordinate-id)))))))

(defmethod lower-shader-call ((operator (eql 'texture-size)) context expression)
  (require-spir-v-capability context 'image-query)
  (destructuring-bind (texture &optional level) (shader-call-operands expression)
    (let ((texture-id (lower-shader-expression context texture)))
      (if (shader-storage-texture-type-p (shader-expression-type texture))
          (emit-value-instruction
           context expression (shader-expression-type expression)
           'image-query-size (list texture-id))
          (emit-value-instruction
           context expression (shader-expression-type expression)
           'image-query-size-lod
           (list texture-id
                 (if level
                     (lower-shader-expression context level)
                     (ensure-shader-uint-constant context 0))))))))

;;; Atomics and waves.

(defun spir-v-atomic-scope (context target)
  (ensure-shader-uint-constant
   context (etypecase target
             (shader-storage-buffer *scope-device*)
             (shader-shared-array *scope-workgroup*))))

(defun lower-spir-v-atomic (context expression instruction)
  (let* ((target (shader-atomic-call-target expression))
         (operands (shader-call-operands expression))
         (pointer (shader-element-pointer context target (first operands)
                                          expression))
         (relaxed (ensure-shader-uint-constant context 0))
         (scope (spir-v-atomic-scope context target)))
    (if (eq instruction 'atomic-compare-exchange)
        (destructuring-bind (comparand value) (rest operands)
          (emit-value-instruction
           context expression :uint 'atomic-compare-exchange
           (list pointer scope relaxed relaxed
                 (lower-shader-expression context value)
                 (lower-shader-expression context comparand))))
        (emit-value-instruction
         context expression :uint instruction
         (list pointer scope relaxed
               (lower-shader-expression context (second operands)))))))

(macrolet ((atomics (&rest pairs)
             `(progn
                ,@(loop for (operator instruction) on pairs by #'cddr
                        collect `(defmethod lower-shader-call
                                     ((operator (eql ',operator)) context
                                      expression)
                                   (lower-spir-v-atomic context expression
                                                        ',instruction))))))
  ;; This package shadows the shared words for its instructions.
  (atomics atomic-add atomic-i-add
           atomic-min atomic-u-min
           atomic-max atomic-u-max
           luv.shader:atomic-and atomic-and
           luv.shader:atomic-or atomic-or
           luv.shader:atomic-xor atomic-xor
           luv.shader:atomic-exchange atomic-exchange
           luv.shader:atomic-compare-exchange atomic-compare-exchange))

(defun lower-spir-v-wave-sum (context expression operation)
  (require-subgroups context 'group-non-uniform-arithmetic)
  (let ((operand (first (shader-call-operands expression))))
    (emit-value-instruction
     context expression (shader-expression-type expression)
     (if (shader-float-type-p (shader-expression-type operand))
         'group-non-uniform-f-add
         'group-non-uniform-i-add)
     (list (ensure-shader-uint-constant context *scope-subgroup*)
           operation
           (lower-shader-expression context operand)))))

(defmethod lower-shader-call ((operator (eql 'wave-active-sum)) context expression)
  (lower-spir-v-wave-sum context expression 'reduce))

(defmethod lower-shader-call ((operator (eql 'wave-prefix-sum)) context expression)
  (lower-spir-v-wave-sum context expression 'exclusive-scan))

(defmethod lower-shader-call ((operator (eql 'wave-ballot)) context expression)
  (require-subgroups context 'group-non-uniform-ballot)
  (emit-value-instruction
   context expression :uvec4 'group-non-uniform-ballot
   (list (ensure-shader-uint-constant context *scope-subgroup*)
         (lower-shader-expression
          context (first (shader-call-operands expression))))))

;;; Statements.

(defmethod lower-shader-statement (context (statement shader-block-statement))
  ;; The bindings are computed here, in order, after every earlier effect.
  (dolist (binding (shader-block-statement-bindings statement))
    (let ((expression (shader-binding-expression binding)))
      (when (shader-expression-materialized-p expression)
        (lower-shader-expression context expression))))
  (dolist (child (shader-block-statement-statements statement))
    (lower-shader-statement context child)))

(defmethod lower-shader-statement (context (statement shader-discard))
  ;; OpKill ends its block; anything after it is unreachable but must still
  ;; stand in a block of its own.
  (emit-shader-instruction context nil '(kill))
  (begin-shader-basic-block context (fresh-shader-id context 'after-discard)))

(defmethod lower-shader-statement (context (statement shader-barrier))
  (multiple-value-bind (memory semantics)
      (ecase (shader-barrier-memory statement)
        (:workgroup
         (values *scope-workgroup*
                 (logior *semantics-acquire-release*
                         *semantics-workgroup-memory*)))
        (:storage
         (values *scope-device*
                 (logior *semantics-acquire-release*
                         *semantics-uniform-memory*
                         *semantics-workgroup-memory*
                         *semantics-image-memory*))))
    (emit-shader-instruction
     context nil
     (list 'control-barrier
           (ensure-shader-uint-constant context *scope-workgroup*)
           (ensure-shader-uint-constant context memory)
           (ensure-shader-uint-constant context semantics)))))

(defmethod lower-shader-statement (context (statement shader-evaluation))
  (lower-shader-expression context (shader-evaluation-expression statement)))

(defmethod lower-shader-statement (context (statement shader-shared-store))
  (let* ((expression (shader-shared-store-value statement))
         (pointer (shader-element-pointer
                   context (shader-shared-store-array statement)
                   (shader-shared-store-index statement) expression)))
    (emit-shader-instruction
     context expression
     (list 'store pointer (lower-shader-expression context expression)))))

(defmethod lower-shader-statement (context (statement shader-texel-store))
  (let* ((texture (shader-texel-store-texture statement))
         (value (shader-texel-store-value statement))
         (image (emit-value-instruction
                 context value (shader-declaration-type texture) 'load
                 (list (gethash texture (context-variable-ids context))))))
    (emit-shader-instruction
     context value
     (list 'image-write image
           (lower-shader-expression
            context (shader-texel-store-coordinate statement))
           (lower-shader-expression context value)))))
