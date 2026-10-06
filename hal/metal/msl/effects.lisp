;;; MSL for textures, storage textures, and stage effects.
;;;
;;; Texture arrays, cubes, and volumes are texture2d_array, depth2d_array,
;;; texturecube, and texture3d; a storage texture is a read_write texture2d
;;; at texture index 16 + its binding, past the sampled textures.  Workgroup
;;; arrays are threadgroup variables of the kernel, barriers
;;; threadgroup_barrier, waves simdgroups.  A buffer or workgroup array that
;;; any atomic touches is atomic_uint, so every access to it in the stage is
;;; a relaxed atomic load, store, or read-modify-write.

(in-package #:luv.msl)

;;; Locals and plain statements.

(defun msl-local-name (context base &optional unique)
  "Claim BASE as a local name; when UNIQUE and BASE is taken, a fresh one."
  (let ((names (msl-context-local-names context)))
    (if (and unique (gethash base names))
        (loop for ordinal from 2
              for candidate = (format nil "~A_~D" base ordinal)
              unless (gethash candidate names)
                do (setf (gethash candidate names) t)
                   (return candidate))
        (progn (setf (gethash base names) t) base))))

(defun msl-temporary-name (context prefix)
  (msl-local-name
   context
   (format nil "~A_~D" prefix (incf (msl-context-temporary-counter context)))
   t))

(defclass msl-line-statement ()
  ((text :initarg :text :reader msl-line-statement-text)
   (origin :initarg :origin :initform nil :reader msl-line-statement-origin))
  (:documentation "One rendered statement line, such as a barrier."))

(defmethod write-msl-statement ((statement msl-line-statement) stream)
  (write-msl-indent stream)
  (format stream "~A~%" (msl-line-statement-text statement)))

(defun msl-line (origin control &rest arguments)
  (make-instance 'msl-line-statement
                 :text (apply #'format nil control arguments)
                 :origin origin))

(defun push-msl-pending (context &rest statements)
  (setf (msl-context-pending-statements context)
        (append (msl-context-pending-statements context) statements)))

;;; Types, built-ins, and declarations.

(defun msl-storage-texture-type-name (type)
  (format nil "texture2d<~A, access::read_write>"
          (if (eq :uint (shader:shader-type-scalar-kind
                         (shader:find-shader-type
                          (shader:shader-type-sample-result-type type))))
              "uint"
              "float")))

(defun msl-stage-built-in-attribute (stage direction built-in)
  "The attribute of a fragment or wave built-in, or NIL."
  (case direction
    (:input
     (case built-in
       (:frag-coord (and (eq stage :fragment) "[[position]]"))
       (:front-facing (and (eq stage :fragment) "[[front_facing]]"))
       (:sample-index (and (eq stage :fragment) "[[sample_id]]"))
       (:wave-lane-index "[[thread_index_in_simdgroup]]")
       (:wave-lane-count "[[threads_per_simdgroup]]")))
    (:output
     (case built-in
       (:frag-depth (and (eq stage :fragment) "[[depth(any)]]"))))))

(defun msl-resource-parameters (context specification)
  (mapcar (lambda (resource)
            (msl-resource-parameter resource
                                    (msl-context-atomic-targets context)))
          (shader:shader-specification-resources specification)))

(defun msl-atomic-target-p (context target)
  (member target (msl-context-atomic-targets context)))

(defun msl-atomic-load-text (context target element)
  (if (msl-atomic-target-p context target)
      (format nil "atomic_load_explicit(&~A, memory_order_relaxed)" element)
      element))

(defun msl-threadgroup-declarations (context specification)
  "A kernel's workgroup arrays, declared first in its body."
  (loop for array in (shader:shader-specification-shared-arrays specification)
        collect (msl-line array "threadgroup ~A ~A[~D];"
                          (if (msl-atomic-target-p context array)
                              "atomic_uint"
                              (msl-type-name
                               (shader:shader-declaration-type array)))
                          (gethash array (msl-context-references context))
                          (shader:shader-shared-array-element-count array))))

;;; Workgroup memory.

(defun msl-element-text (context target index)
  (format nil "~A[~A]"
          (gethash target (msl-context-references context))
          (msl-occurrence-text (lower-msl-expression context index))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-shared-element))
  (let ((array (shader:shader-shared-element-array expression)))
    (note-msl-occurrence
     context expression
     (msl-atomic-load-text
      context array
      (msl-element-text context array
                        (shader:shader-shared-element-index expression))))))

;;; Textures.

(defun msl-texture-operand-texts (context expression)
  "Return the texture's type, the operand texts, and whether it is arrayed."
  (let* ((operands (shader:shader-call-operands expression))
         (type (shader:shader-expression-type (first operands))))
    (values type
            (mapcar #'msl-occurrence-text
                    (lower-msl-operands context expression))
            (shader:shader-texture-arrayed-p type))))

(defun msl-sampling-call (context expression method &optional suffix-function)
  "TEXTURE.METHOD(SAMPLER, COORDINATE[, LAYER], ...): SUFFIX-FUNCTION makes
the trailing arguments from the operator's own operand texts."
  (multiple-value-bind (type texts arrayed-p)
      (msl-texture-operand-texts context expression)
    (destructuring-bind (texture sampler coordinate &rest rest) texts
      (let* ((layer (and arrayed-p (first rest)))
             (own (if arrayed-p (rest rest) rest))
             (call (format nil "~A.~A(~A, ~A~@[, ~A~]~{, ~A~})"
                           texture method sampler coordinate layer
                           (if suffix-function
                               (funcall suffix-function type own)
                               own))))
        call))))

(defun msl-sampled (expression text)
  "Depth textures sample one float; the language's sample is a vec4."
  (if (shader:shader-type-image-depth-p
       (shader:shader-expression-type
        (first (shader:shader-call-operands expression))))
      (format nil "float4(~A)" text)
      text))

(defmacro define-msl-operator (operator (context expression) &body body)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (,context msl-lowering-context)
        (,expression shader:shader-call))
     (declare (ignore operator))
     ,@body))

(define-msl-operator shader:sample (context expression)
  (note-msl-occurrence
   context expression
   (msl-sampled expression (msl-sampling-call context expression "sample"))))

(define-msl-operator shader:sample-level (context expression)
  (note-msl-occurrence
   context expression
   (msl-sampled expression
                (msl-sampling-call
                 context expression "sample"
                 (lambda (type own)
                   (declare (ignore type))
                   (list (format nil "metal::level(~A)" (first own))))))))

(define-msl-operator shader:sample-bias (context expression)
  (note-msl-occurrence
   context expression
   (msl-sampled expression
                (msl-sampling-call
                 context expression "sample"
                 (lambda (type own)
                   (declare (ignore type))
                   (list (format nil "metal::bias(~A)" (first own))))))))

(define-msl-operator shader:sample-grad (context expression)
  (note-msl-occurrence
   context expression
   (msl-sampled expression
                (msl-sampling-call
                 context expression "sample"
                 (lambda (type own)
                   (list (format nil "~A(~A, ~A)"
                                 (ecase (shader:shader-type-texture-dimension
                                         type)
                                   ((:2d :2d-array) "metal::gradient2d")
                                   (:cube "metal::gradientcube")
                                   (:3d "metal::gradient3d"))
                                 (first own) (second own))))))))

(define-msl-operator shader:sample-compare (context expression)
  (note-msl-occurrence
   context expression (msl-sampling-call context expression "sample_compare")))

(define-msl-operator shader:gather (context expression)
  ;; Colour gathers name their channel; a cube's gather takes no offset,
  ;; and a depth texture has one channel to gather.  Metal's helper names
  ;; are qualified, so a shader local such as LEVEL cannot shadow them.
  (note-msl-occurrence
   context expression
   (msl-sampling-call
    context expression "gather"
    (lambda (type own)
      (declare (ignore own))
      (cond ((shader:shader-type-image-depth-p type) nil)
            ((eq :cube (shader:shader-type-texture-dimension type))
             (list "metal::component::x"))
            (t (list "int2(0)" "metal::component::x")))))))

(define-msl-operator shader:gather-compare (context expression)
  (note-msl-occurrence
   context expression (msl-sampling-call context expression "gather_compare")))

(define-msl-operator shader:texel-load (context expression)
  ;; read(coordinate[, layer][, level]); storage textures have no level.
  ;; A depth texture reads a scalar, which widens to the language's vec4
  ;; texel as HLSL's Load does.
  (multiple-value-bind (type texts) (msl-texture-operand-texts context expression)
    (destructuring-bind (texture &rest arguments) texts
      (note-msl-occurrence
       context expression
       (format nil (if (shader:shader-type-image-depth-p type)
                       "float4(~A.read(~{~A~^, ~}))"
                       "~A.read(~{~A~^, ~})")
               texture arguments)))))

(define-msl-operator shader:texture-size (context expression)
  (multiple-value-bind (type texts) (msl-texture-operand-texts context expression)
    (destructuring-bind (texture &optional level) texts
      (let ((level (if (shader:shader-storage-texture-type-p type)
                       ""
                       (or level "0u"))))
        (note-msl-occurrence
         context expression
         (ecase (shader:shader-type-texture-dimension type)
           ((:2d :cube)
            (format nil "uint2(~A.get_width(~A), ~A.get_height(~A))"
                    texture level texture level))
           (:2d-array
            (format nil "uint3(~A.get_width(~A), ~A.get_height(~A), ~
                         ~A.get_array_size())"
                    texture level texture level texture))
           (:3d
            (format nil "uint3(~A.get_width(~A), ~A.get_height(~A), ~
                         ~A.get_depth(~A))"
                    texture level texture level texture level))))))))

;;; Atomics and waves.

(defun lower-msl-atomic (context expression function)
  (let* ((operands (shader:shader-call-operands expression))
         (destination (msl-element-text
                       context (shader:shader-atomic-call-target expression)
                       (first operands)))
         (value (msl-occurrence-text
                 (lower-msl-expression context (second operands)))))
    (note-msl-occurrence
     context expression
     (format nil "~A(&~A, ~A, memory_order_relaxed)"
             function destination value))))

(macrolet ((atomics (&rest pairs)
             `(progn
                ,@(loop for (operator function) on pairs by #'cddr
                        collect `(defmethod shader:lower-shader-call
                                     ((operator (eql ',operator))
                                      (context msl-lowering-context)
                                      (expression shader:shader-atomic-call))
                                   (declare (ignore operator))
                                   (lower-msl-atomic context expression
                                                     ,function))))))
  (atomics shader:atomic-add "atomic_fetch_add_explicit"
           shader:atomic-min "atomic_fetch_min_explicit"
           shader:atomic-max "atomic_fetch_max_explicit"
           shader:atomic-and "atomic_fetch_and_explicit"
           shader:atomic-or "atomic_fetch_or_explicit"
           shader:atomic-xor "atomic_fetch_xor_explicit"
           shader:atomic-exchange "atomic_exchange_explicit"))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:atomic-compare-exchange))
     (context msl-lowering-context)
     (expression shader:shader-atomic-call))
  ;; Metal's only compare-exchange is weak: it may fail spuriously, leaving
  ;; the comparand as the observed value.  Retry exactly those failures, so
  ;; the result is the element's previous value, as in HLSL and SPIR-V.
  (declare (ignore operator))
  (destructuring-bind (index comparand value) (shader:shader-call-operands
                                               expression)
    (let* ((destination (msl-element-text
                         context (shader:shader-atomic-call-target expression)
                         index))
           (comparand (msl-occurrence-text
                       (lower-msl-expression context comparand)))
           (value (msl-occurrence-text (lower-msl-expression context value)))
           (expected (msl-temporary-name context "comparand"))
           (observed (msl-temporary-name context "observed")))
      (push-msl-pending
       context
       (msl-line expression "uint ~A = ~A;" expected comparand)
       (msl-line expression "uint ~A = ~A;" observed expected)
       (msl-line expression
                 "while (!atomic_compare_exchange_weak_explicit(&~A, &~A, ~A, ~
                  memory_order_relaxed, memory_order_relaxed) && ~A == ~A) {}"
                 destination observed value observed expected))
      (note-msl-occurrence context expression observed))))

(define-msl-operator shader:wave-active-sum (context expression)
  (lower-msl-function-call context expression "simd_sum"))

(define-msl-operator shader:wave-prefix-sum (context expression)
  (lower-msl-function-call context expression "simd_prefix_exclusive_sum"))

(define-msl-operator shader:wave-ballot (context expression)
  ;; A simdgroup has at most 64 lanes, whose vote is one 64-bit mask.
  (let ((vote (msl-temporary-name context "ballot"))
        (predicate (msl-occurrence-text
                    (lower-msl-expression
                     context (first (shader:shader-call-operands expression))))))
    (push-msl-pending
     context
     (msl-line expression "ulong ~A = static_cast<ulong>(simd_ballot(~A));"
               vote predicate))
    (note-msl-occurrence
     context expression
     (format nil "uint4(uint(~A), uint(~A >> 32), 0u, 0u)" vote vote))))

;;; Statements.

(defun lower-msl-block-binding (context binding)
  (let* ((expression (shader:shader-binding-expression binding))
         (name (msl-local-name
                context (msl-identifier (shader:shader-object-name binding)) t))
         (value (lower-msl-expression context expression))
         (pending (drain-msl-pending-statements context)))
    (setf (gethash binding (msl-context-references context)) name)
    (append pending
            (list (make-instance
                   'msl-variable-statement
                   :type (msl-type-name (shader:shader-expression-type
                                         expression))
                   :name name :value value :origin binding)))))

(defmethod lower-msl-statement
    ((context msl-lowering-context) (statement shader:shader-block-statement))
  (nconc (loop for binding in (shader:shader-block-statement-bindings statement)
               nconc (lower-msl-block-binding context binding))
         (mapcan (lambda (child) (lower-msl-statement context child))
                 (shader:shader-block-statement-statements statement))))

(defmethod lower-msl-statement
    ((context msl-lowering-context) (statement shader:shader-discard))
  (declare (ignore context))
  (list (msl-line statement "discard_fragment();")))

(defmethod lower-msl-statement
    ((context msl-lowering-context) (statement shader:shader-barrier))
  (declare (ignore context))
  (list (msl-line statement "threadgroup_barrier(~A);"
                  (ecase (shader:shader-barrier-memory statement)
                    (:workgroup "mem_flags::mem_threadgroup")
                    (:storage
                     (concatenate 'string
                                  "mem_flags::mem_device"
                                  " | mem_flags::mem_threadgroup"
                                  " | mem_flags::mem_texture"))))))

(defmethod lower-msl-statement
    ((context msl-lowering-context) (statement shader:shader-evaluation))
  (let* ((expression (shader:shader-evaluation-expression statement))
         (value (lower-msl-expression context expression))
         (pending (drain-msl-pending-statements context)))
    ;; A compare-exchange's effect is its pending loop; other atomics are
    ;; the call itself.
    (if (eq 'shader:atomic-compare-exchange
            (shader:shader-call-operator expression))
        pending
        (append pending
                (list (msl-line statement "~A;"
                                (msl-occurrence-text value)))))))

(defmethod lower-msl-statement
    ((context msl-lowering-context) (statement shader:shader-shared-store))
  (let* ((array (shader:shader-shared-store-array statement))
         (target (msl-element-text context array
                                   (shader:shader-shared-store-index statement)))
         (value (msl-occurrence-text
                 (lower-msl-expression
                  context (shader:shader-shared-store-value statement)))))
    (append (drain-msl-pending-statements context)
            (list (if (msl-atomic-target-p context array)
                      (msl-line statement
                                "atomic_store_explicit(&~A, ~A, ~
                                 memory_order_relaxed);"
                                target value)
                      (msl-line statement "~A = ~A;" target value))))))

(defmethod lower-msl-statement
    ((context msl-lowering-context) (statement shader:shader-texel-store))
  (let ((coordinate (msl-occurrence-text
                     (lower-msl-expression
                      context (shader:shader-texel-store-coordinate statement))))
        (value (msl-occurrence-text
                (lower-msl-expression
                 context (shader:shader-texel-store-value statement)))))
    (append (drain-msl-pending-statements context)
            (list (msl-line statement "~A.write(~A, ~A);"
                            (gethash (shader:shader-texel-store-texture statement)
                                     (msl-context-references context))
                            value coordinate)))))
