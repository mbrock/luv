;;; Stage effects: sequencing, fragment and compute built-ins, discard,
;;; workgroup memory, barriers, atomics, and wave operations.
;;;
;;; The expression language is pure, so an effect needs a place in time.  A
;;; stage body is a sequence of statements, and a statement-level LET* binds
;;; values at its own position in that sequence: after every statement
;;; before it.  Effects whose results matter -- atomics -- are allowed only
;;; as the whole value of such a binding (or as a statement of their own),
;;; so every backend evaluates each one exactly once, where it stands.
;;; #X4ZZ92

(in-package #:luv.shader)

;;; Sequenced bindings.

(defvar *shader-effect-binding-form* nil
  "The value form of the stage-body LET* binding being parsed.  An effect
expression, such as an atomic, is legal only as exactly this form.")

(defun parse-shader-statement-bindings (raw-bindings environment)
  "Parse a stage body's LET* bindings in order.
Return the bindings and the environment that sees them."
  (let ((bindings nil)
        (lexical-environment environment))
    (unless (listp raw-bindings)
      (error 'shader-language-error
             :form raw-bindings :reason :invalid-binding))
    (dolist (raw-binding raw-bindings)
      (unless (and (consp raw-binding) (= (length raw-binding) 2)
                   (symbolp (first raw-binding)))
        (error 'shader-language-error
               :form raw-binding :reason :invalid-binding))
      (let* ((name (first raw-binding))
             (expression
               (let ((*shader-effect-binding-form* (second raw-binding)))
                 (parse-shader-expression (second raw-binding)
                                          lexical-environment)))
             (binding
               (make-instance 'shader-binding
                              :name name :expression expression
                              :source-form raw-binding)))
        (setf (shader-expression-name expression) name)
        (push binding bindings)
        (push (cons name binding) lexical-environment)))
    (values (nreverse bindings) lexical-environment)))

(defmethod parse-shader-statement
    ((operator (eql 'let*)) stage form environment context)
  (declare (ignore operator))
  (unless (and (>= (length form) 2) (listp (second form)))
    (error 'shader-language-error
           :form form :reason :invalid-statement-binding-form))
  (multiple-value-bind (bindings lexical-environment)
      (parse-shader-statement-bindings (second form) environment)
    (make-instance
     'shader-block-statement
     :bindings bindings
     :statements (mapcar (lambda (statement)
                           (parse-shader-statement-form
                            statement stage lexical-environment context))
                         (cddr form))
     :source-form form)))

(defun require-effect-binding (form)
  "Signal unless FORM is the whole value of a stage-body LET* binding."
  (unless (eq form *shader-effect-binding-form*)
    (error 'shader-language-error
           :form form :reason :effect-requires-statement-binding
           :details (first form))))

;;; Built-ins.  The vertex position and the workgroup built-ins are older;
;;; these are checked here for their stage, direction, and type.

(defparameter *stage-built-ins*
  '((:frag-coord :input (:fragment) :vec4)
    (:front-facing :input (:fragment) :bool)
    (:sample-index :input (:fragment) :uint)
    (:frag-depth :output (:fragment) :float)
    (:wave-lane-index :input (:fragment :compute) :uint)
    (:wave-lane-count :input (:fragment :compute) :uint))
  "(BUILT-IN DIRECTION STAGES TYPE) for the fragment and wave built-ins.

:FRAG-COORD is the fragment's framebuffer position: x and y in pixels from
the top-left corner, at pixel centres (0.5, 0.5 for the first), z its depth,
and w the reciprocal of its clip w -- Vulkan's FragCoord, Metal's
[[position]], and Direct3D's SV_Position with w inverted.  :FRONT-FACING is
true for a front face under the pipeline's winding.  :SAMPLE-INDEX runs the
stage once per sample.  :FRAG-DEPTH replaces the fragment's depth.")

(defun stage-built-in-type (stage direction built-in)
  "The type BUILT-IN has as a DIRECTION of STAGE, or NIL."
  (let ((entry (assoc built-in *stage-built-ins*)))
    (and entry
         (eq direction (second entry))
         (member stage (third entry))
         (fourth entry))))

(defun validate-stage-built-ins (stage inputs outputs)
  (loop for (declarations direction) in `((,inputs :input) (,outputs :output))
        do (let ((seen nil))
             (dolist (declaration declarations)
               (let* ((built-in (shader-interface-built-in declaration))
                      (entry (assoc built-in *stage-built-ins*)))
                 (when entry
                   (let ((type (stage-built-in-type stage direction built-in)))
                     (unless type
                       (error 'shader-language-error
                              :form (shader-object-source-form declaration)
                              :reason :built-in-not-in-stage
                              :details (list built-in direction stage)))
                     (unless (shader-type=
                              type (shader-declaration-type declaration))
                       (error 'shader-language-error
                              :form (shader-object-source-form declaration)
                              :reason :built-in-type
                              :details (list built-in type)))
                     (when (member built-in seen)
                       (error 'shader-language-error
                              :form (shader-object-source-form declaration)
                              :reason :duplicate-built-in
                              :details built-in))
                     (push built-in seen))))))))

;;; Fragment effects.

(define-shader-operator discard
  "End the fragment invocation without writing any output (a statement).")

(defmethod parse-shader-statement
    ((operator (eql 'discard)) stage form environment context)
  (declare (ignore operator environment context))
  (unless (eq stage :fragment)
    (error 'shader-language-error
           :form form :reason :invalid-statement-for-stage
           :details (list 'discard stage)))
  (unless (= 1 (length form))
    (error 'shader-language-error :form form :reason :discard-arity))
  (make-instance 'shader-discard :source-form form))

;;; Workgroup arrays.

(defparameter *shared-memory-limit* 16384
  "Bytes of workgroup memory one compute stage may declare: Vulkan's
guaranteed minimum, below Direct3D's and Metal's 32 KiB.")

(defun shader-shared-array-byte-size (array)
  "A conservative byte size: vec3 elements count as 16 bytes, as in MSL."
  (let ((type (shader-declaration-type array)))
    (* (shader-shared-array-element-count array)
       (if (= 3 (shader-type-component-count type))
           4
           (shader-type-component-count type))
       (floor (shader-type-bit-width type) 8))))

(defun parse-shared-array-declarations (stage forms options)
  (when (and forms (not (eq stage :compute)))
    (error 'shader-language-error
           :form options :reason :shared-memory-outside-compute
           :details stage))
  (let ((arrays
          (loop with names = nil
                for form in forms
                collect
                (progn
                  (unless (and (consp form) (= 3 (length form))
                               (symbolp (first form)))
                    (error 'shader-language-error
                           :form form :reason :invalid-shared-array))
                  (destructuring-bind (name type count) form
                    (let ((element-type (find-shader-type type form)))
                      (unless (and (shader-type-component-count element-type)
                                   (member (shader-type-scalar-kind
                                            element-type)
                                           '(:float :uint))
                                   (= 32 (shader-type-bit-width element-type)))
                        (error 'shader-language-error
                               :form form :reason :invalid-shared-array-element
                               :details type))
                      (unless (typep count '(integer 1 *))
                        (error 'shader-language-error
                               :form form :reason :invalid-shared-array-length
                               :details count))
                      (when (find name names :test #'shader-symbol=)
                        (error 'shader-language-error
                               :form form :reason :duplicate-shared-array
                               :details name))
                      (push name names)
                      (make-instance 'shader-shared-array
                                     :name name :type element-type
                                     :element-count count
                                     :source-form form)))))))
    (let ((bytes (reduce #'+ arrays :key #'shader-shared-array-byte-size)))
      (when (> bytes *shared-memory-limit*)
        (error 'shader-language-error
               :form options :reason :shared-memory-exceeds-limit
               :details (list bytes *shared-memory-limit*))))
    arrays))

(defclass shader-shared-element (shader-expression)
  ((array
    :initarg :array
    :reader shader-shared-element-array)
   (index
    :initarg :index
    :reader shader-shared-element-index))
  (:documentation "A read of one workgroup array element."))

(defmethod shader-expression-children ((expression shader-shared-element))
  (list (shader-shared-element-index expression)))

(defmethod shader-expression-form ((expression shader-shared-element))
  (shader-expression-source-form expression))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-shared-element))
  (declare (ignore expression))
  nil)

(defmethod lang:arithmetic-expression-quantity-checked-p
    ((expression shader-shared-element))
  (declare (ignore expression))
  nil)

(define-shader-operator shared-element
  "Read one element of a compute stage's workgroup array.")

(define-shader-operator set-shared-element
  "Store one element of a compute stage's workgroup array (a statement).")

(defun shader-shared-array-named (name environment form)
  (let ((array (shader-environment-value name environment form)))
    (unless (typep array 'shader-shared-array)
      (error 'shader-language-error
             :form form :reason :not-shared-array :details name))
    array))

(defun parse-shared-array-index (array index form)
  (unless (shader-uint-type-p (shader-expression-type index))
    (error 'shader-language-error
           :form form :reason :shared-array-index-type
           :details (shader-type-name (shader-expression-type index))))
  (multiple-value-bind (constant constant-p) (shader-constant-uint-value index)
    (when (and constant-p
               (>= constant (shader-shared-array-element-count array)))
      (error 'shader-language-error
             :form form :reason :shared-array-index-out-of-bounds
             :details (list constant
                            (shader-shared-array-element-count array)))))
  index)

(defmethod parse-shader-operator-call
    ((operator (eql 'shared-element)) form environment)
  (declare (ignore operator))
  (unless (= (length form) 3)
    (error 'shader-language-error :form form :reason :shared-element-arity))
  (let* ((array (shader-shared-array-named (second form) environment form))
         (index (parse-shared-array-index
                 array (parse-shader-expression (third form) environment)
                 form)))
    (make-instance 'shader-shared-element
                   :array array :index index
                   :type (shader-declaration-type array)
                   :quantity-specification nil :quantity-layout nil
                   :source-form form)))

(defmethod parse-shader-statement
    ((operator (eql 'set-shared-element)) (stage (eql :compute))
     form environment context)
  (declare (ignore operator context))
  (unless (= (length form) 4)
    (error 'shader-language-error
           :form form :reason :set-shared-element-arity))
  (let* ((array (shader-shared-array-named (second form) environment form))
         (index (parse-shared-array-index
                 array (parse-shader-expression (third form) environment)
                 form))
         (value (parse-shader-expression (fourth form) environment)))
    (unless (shader-type= (shader-declaration-type array)
                          (shader-expression-type value))
      (error 'shader-language-error
             :form form :reason :shared-element-type-mismatch
             :details (list (shader-type-name
                             (shader-declaration-type array))
                            (shader-type-name
                             (shader-expression-type value)))))
    (make-instance 'shader-shared-store
                   :array array :index index :value value
                   :source-form form)))

;;; Barriers.

(define-shader-operator workgroup-barrier
  "Wait for the whole workgroup, ordering its workgroup memory (a statement).")

(define-shader-operator storage-barrier
  "Wait for the whole workgroup, ordering workgroup memory and storage
buffers and textures as well (a statement).")

(defun parse-barrier-statement (memory stage form)
  (unless (eq stage :compute)
    (error 'shader-language-error
           :form form :reason :invalid-statement-for-stage
           :details (list (first form) stage)))
  (unless (= 1 (length form))
    (error 'shader-language-error :form form :reason :barrier-arity))
  (make-instance 'shader-barrier :memory memory :source-form form))

(defmethod parse-shader-statement
    ((operator (eql 'workgroup-barrier)) stage form environment context)
  (declare (ignore operator environment context))
  (parse-barrier-statement :workgroup stage form))

(defmethod parse-shader-statement
    ((operator (eql 'storage-barrier)) stage form environment context)
  (declare (ignore operator environment context))
  (parse-barrier-statement :storage stage form))

;;; Atomics.  Each reads one unsigned element, combines it with a value,
;;; stores the result, and returns the element as it was -- relaxed: no
;;; ordering of other memory, which the barriers provide.

(defclass shader-atomic-call (shader-call)
  ((target
    :initarg :target
    :reader shader-atomic-call-target
    :documentation "The read-write storage buffer or workgroup array."))
  (:documentation
   "An atomic read-modify-write.  Its operands are the element index, then
for ATOMIC-COMPARE-EXCHANGE the comparand, then the value."))

(defmethod shader-expression-form ((expression shader-atomic-call))
  (shader-expression-source-form expression))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *atomic-operators*
    '(atomic-add atomic-min atomic-max atomic-and atomic-or atomic-xor
      atomic-exchange atomic-compare-exchange)
    "The atomic read-modify-write operators, all on 32-bit unsigned elements."))

(define-shader-operator atomic-add
  "Atomically add to an unsigned element; return its previous value.")
(define-shader-operator atomic-min
  "Atomically lower an unsigned element to a minimum; return its previous value.")
(define-shader-operator atomic-max
  "Atomically raise an unsigned element to a maximum; return its previous value.")
(define-shader-operator atomic-and
  "Atomically AND bits into an unsigned element; return its previous value.")
(define-shader-operator atomic-or
  "Atomically OR bits into an unsigned element; return its previous value.")
(define-shader-operator atomic-xor
  "Atomically XOR bits into an unsigned element; return its previous value.")
(define-shader-operator atomic-exchange
  "Atomically replace an unsigned element; return its previous value.")
(define-shader-operator atomic-compare-exchange
  "Atomically replace an unsigned element that equals a comparand; return
its previous value, which equals the comparand exactly when it was replaced.")

(defun shader-atomic-target (name environment form)
  (let ((target (shader-environment-value name environment form)))
    (typecase target
      (shader-storage-buffer
       (unless (shader-storage-buffer-writable-p target)
         (error 'shader-language-error
                :form form :reason :read-only-storage-buffer :details name))
       (unless (shader-uint-type-p
                (shader-storage-buffer-element-type target))
         (error 'shader-language-error
                :form form :reason :atomic-element-type
                :details (shader-type-name
                          (shader-storage-buffer-element-type target)))))
      (shader-shared-array
       (unless (shader-uint-type-p (shader-declaration-type target))
         (error 'shader-language-error
                :form form :reason :atomic-element-type
                :details (shader-type-name
                          (shader-declaration-type target)))))
      (t
       (error 'shader-language-error
              :form form :reason :not-atomic-target :details name)))
    target))

(defun parse-shader-atomic-call (operator form environment)
  (require-effect-binding form)
  (let ((arity (if (eq operator 'atomic-compare-exchange) 5 4)))
    (unless (= arity (length form))
      (error 'shader-language-error
             :form form :reason :atomic-arity
             :details (list operator (1- arity)))))
  (let* ((target (shader-atomic-target (second form) environment form))
         (operands (mapcar (lambda (operand)
                             (parse-shader-expression operand environment))
                           (cddr form))))
    (dolist (operand operands)
      (unless (shader-uint-type-p (shader-expression-type operand))
        (error 'shader-language-error
               :form form :reason :atomic-operand-type
               :details (shader-type-name (shader-expression-type operand)))))
    (when (typep target 'shader-shared-array)
      (parse-shared-array-index target (first operands) form))
    (make-instance 'shader-atomic-call
                   :operator operator :operands operands :target target
                   :type (find-shader-type :uint)
                   :quantity-specification nil :quantity-layout nil
                   :source-form form)))

(macrolet ((define-atomic-parsers ()
             `(progn
                ,@(loop for operator in *atomic-operators*
                        collect
                        `(defmethod parse-shader-operator-call
                             ((operator (eql ',operator)) form environment)
                           (parse-shader-atomic-call
                            operator form environment))))))
  (define-atomic-parsers))

(defun shader-effect-form-p (form)
  (and (consp form) (member (first form) *atomic-operators*)))

(defun parse-shader-evaluation (stage form environment)
  (unless (eq stage :compute)
    (error 'shader-language-error
           :form form :reason :invalid-statement-for-stage
           :details (list (first form) stage)))
  (make-instance 'shader-evaluation
                 :expression (let ((*shader-effect-binding-form* form))
                               (parse-shader-expression form environment))
                 :source-form form))

(macrolet ((define-atomic-statements ()
             `(progn
                ,@(loop for operator in *atomic-operators*
                        collect
                        `(defmethod parse-shader-statement
                             ((operator (eql ',operator)) stage form
                              environment context)
                           (declare (ignore context))
                           (parse-shader-evaluation
                            stage form environment))))))
  (define-atomic-statements))

(defun shader-specification-atomic-targets (specification)
  "The buffers and workgroup arrays SPECIFICATION accesses atomically.
MSL must type these as atomic_uint and access them only atomically."
  (let ((targets nil))
    (map-shader-specification-expressions
     (lambda (expression)
       (when (typep expression 'shader-atomic-call)
         (pushnew (shader-atomic-call-target expression) targets)))
     specification)
    (nreverse targets)))

;;; Wave operations: the invocations of one SIMD group (a wave, simdgroup,
;;; or subgroup) combining values without memory.  Their result depends on
;;; which lanes are active, so they belong in uniform control flow.

(define-shader-operator wave-active-sum
  "The sum of a scalar over the active lanes of the invocation's wave.")
(define-shader-operator wave-prefix-sum
  "The sum of a scalar over the wave's active lanes below this one.")
(define-shader-operator wave-ballot
  "A uvec4 bit mask of the wave's active lanes whose boolean is true.")

(defun infer-wave-sum-type (operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= 1 (length types))
          (eql 1 (shader-type-component-count (first types)))
          (member (shader-type-scalar-kind (first types)) '(:float :uint))
          (= 32 (shader-type-bit-width (first types)))))
   operands source-form :invalid-wave-sum)
  (shader-expression-type (first operands)))

(defmethod infer-shader-call-type
    ((operator (eql 'wave-active-sum)) operands source-form)
  (infer-wave-sum-type operands source-form))

(defmethod infer-shader-call-type
    ((operator (eql 'wave-prefix-sum)) operands source-form)
  (infer-wave-sum-type operands source-form))

(defmethod infer-shader-call-type
    ((operator (eql 'wave-ballot)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= 1 (length types)) (shader-type= (first types) :bool)))
   operands source-form :invalid-wave-ballot)
  (find-shader-type :uvec4))

(defun infer-wave-sum-quantity (operands source-form)
  (let ((operand (first operands)))
    (when (shader-expression-quantity-checked-p operand)
      (require-semantic-operands operands source-form '(0))
      (with-shader-quantity-errors (source-form :invalid-quantity-operation)
        (let ((specification
                (shader-expression-quantity-specification operand)))
          (math:derive-quantity-specification
           '+ specification specification))))))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'wave-active-sum)) operands source-form)
  (infer-wave-sum-quantity operands source-form))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'wave-prefix-sum)) operands source-form)
  (infer-wave-sum-quantity operands source-form))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'wave-ballot)) operands source-form)
  (declare (ignore operands source-form))
  nil)

(defparameter *wave-operators* '(wave-active-sum wave-prefix-sum wave-ballot))

;;; Uniformity: an atomic's previous value differs per invocation however
;;; uniform its operands, and a wave's sums and ballots differ per wave.

(defmethod shader-expression-uniformity ((expression shader-atomic-call))
  (declare (ignore expression))
  :invocation)

(defmethod shader-expression-uniformity ((expression shader-call))
  (if (member (shader-call-operator expression) *wave-operators*)
      :invocation
      (call-next-method)))

;;; Whole-stage checks.

(defvar *shader-uniformity-cache* nil
  "When bound to a hash table, uniformities already derived: expression
graphs share subexpressions, so an unremembered walk can be exponential.")

(defmethod shader-expression-uniformity :around ((expression shader-expression))
  (let ((cache *shader-uniformity-cache*))
    (if cache
        (or (gethash expression cache)
            (setf (gethash expression cache) (call-next-method)))
        (call-next-method))))

(defun map-shader-statements (function statements)
  "Call FUNCTION on every statement in STATEMENTS, depth first, with a
second argument: whether some enclosing condition varies per invocation."
  (labels ((walk (statements divergent-p)
             (dolist (statement statements)
               (funcall function statement divergent-p)
               (walk (shader-statement-children statement)
                     (or divergent-p
                         (and (typep statement 'shader-conditional-statement)
                              (not (shader-expression-workgroup-uniform-p
                                    (shader-conditional-statement-condition
                                     statement)))))))))
    (walk statements nil)))

(defun validate-stage-effects (stage bindings statements)
  "Check stage-wide rules of effects: barriers only in uniform control flow,
wave operations only where waves exist, and biased sampling only in
fragments."
  (when (statement-tree-occurrences statements 'shader-barrier)
    (let ((*shader-uniformity-cache* (make-hash-table :test #'eq)))
      (map-shader-statements
       (lambda (statement divergent-p)
         (when (and (typep statement 'shader-barrier) divergent-p)
           (error 'shader-language-error
                  :form (shader-statement-source-form statement)
                  :reason :barrier-in-divergent-control-flow)))
       statements)))
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((visit (expression)
               (unless (gethash expression seen)
                 (setf (gethash expression seen) t)
                 (when (and (typep expression 'shader-call)
                            (member (shader-call-operator expression)
                                    *wave-operators*)
                            (not (member stage '(:fragment :compute))))
                   (error 'shader-language-error
                          :form (shader-expression-source-form expression)
                          :reason :wave-operation-outside-wave-stage
                          :details stage))
                 ;; Only fragments have the implicit derivatives a bias
                 ;; adjusts.
                 (when (and (typep expression 'shader-call)
                            (eq 'sample-bias (shader-call-operator expression))
                            (not (eq stage :fragment)))
                   (error 'shader-language-error
                          :form (shader-expression-source-form expression)
                          :reason :sample-bias-outside-fragment
                          :details stage))
                 (when (typep expression 'shader-reference)
                   (let ((target (shader-reference-target expression)))
                     (when (typep target 'shader-binding)
                       (visit (shader-binding-expression target)))))
                 (mapc #'visit (shader-expression-children expression)))))
      (dolist (binding bindings)
        (visit (shader-binding-expression binding)))
      (dolist (statement statements)
        (mapc #'visit (shader-statement-expressions statement))))))
