;;; Typed mathematical shader expressions and their deterministic SPIR-V lowering.
;;;
;;; The instruction language in hal/vulkan/spir-v/instructions.lisp remains
;;; deliberately literal.  This
;;; layer is the pleasant source language: declarations, bindings, and every
;;; expression are CLOS objects which retain their source forms.  Lowering keeps
;;; an object-to-object correspondence between those expressions and the CLOS
;;; instruction occurrences in the resulting basic block.
;;;
;;; The language itself is a small compiled subset of Common Lisp plus a
;;; vector library.  Operators are named by ordinary symbols — CL's own where
;;; CL has the word, this package's where it does not — and each operator's
;;; parsing, typing, and lowering are EQL-specialized methods, so growing the
;;; language means defining methods, never editing a dispatch table.

(in-package #:luv.spir-v)

(define-condition shader-language-error (error)
  ((form
    :initarg :form
    :initform nil
    :reader shader-language-error-form)
   (reason
    :initarg :reason
    :reader shader-language-error-reason)
   (details
    :initarg :details
    :initform nil
    :reader shader-language-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Cannot understand shader expression ~S: ~A~@[ (~S)~]."
             (shader-language-error-form condition)
             (shader-language-error-reason condition)
             (shader-language-error-details condition)))))

(defmacro with-shader-quantity-errors ((source-form reason) &body body)
  "Translate backend-neutral quantity failures into shader source errors."
  `(handler-case
       (progn ,@body)
     (math:undefined-unit (condition)
       (error 'shader-language-error
              :form ,source-form :reason :undefined-unit
              :details (math:undefined-unit-name condition)))
     (math:quantity-operation-error (condition)
       (error 'shader-language-error
              :form ,source-form :reason ,reason
              :details (math:quantity-operation-error-reason condition)))))

(defclass shader-type ()
  ((name
    :initarg :name
    :reader shader-type-name)
   (component-count
    :initarg :component-count
    :initform nil
    :reader shader-type-component-count)
   (scalar-kind
    :initarg :scalar-kind
    :initform nil
    :reader shader-type-scalar-kind)
   (bit-width
    :initarg :bit-width
    :initform nil
    :reader shader-type-bit-width)
   (opaque-kind
    :initarg :opaque-kind
    :initform nil
    :reader shader-type-opaque-kind)
   (sample-result-type
    :initarg :sample-result-type
    :initform nil
    :reader shader-type-sample-result-type)
   (image-depth-p
    :initarg :image-depth-p
    :initform nil
    :reader shader-type-image-depth-p)))

(defmethod print-object ((type shader-type) stream)
  (print-unreadable-object (type stream :type t)
    (prin1 (shader-type-name type) stream)))

(defparameter *shader-types* (make-hash-table :test #'eq))

(defun register-shader-type
    (name &key component-count scalar-kind bit-width opaque-kind
               sample-result-type image-depth-p)
  (setf (gethash name *shader-types*)
        (make-instance 'shader-type
                       :name name
                       :component-count component-count
                       :scalar-kind scalar-kind
                       :bit-width bit-width
                       :opaque-kind opaque-kind
                       :sample-result-type sample-result-type
                       :image-depth-p image-depth-p)))

(register-shader-type :float :component-count 1 :scalar-kind :float
                      :bit-width 32)
(register-shader-type :bool)
(register-shader-type :vec2 :component-count 2 :scalar-kind :float :bit-width 32)
(register-shader-type :vec3 :component-count 3 :scalar-kind :float :bit-width 32)
(register-shader-type :vec4 :component-count 4 :scalar-kind :float :bit-width 32)
(register-shader-type :uint :component-count 1 :scalar-kind :uint :bit-width 32)
(register-shader-type :uint64 :component-count 1 :scalar-kind :uint :bit-width 64)
(register-shader-type :uvec2 :component-count 2 :scalar-kind :uint :bit-width 32)
(register-shader-type :uvec3 :component-count 3 :scalar-kind :uint :bit-width 32)
(register-shader-type :uvec4 :component-count 4 :scalar-kind :uint :bit-width 32)
(register-shader-type :texture-2d
                      :opaque-kind :texture-2d
                      :sample-result-type :vec4)
(register-shader-type :depth-texture-2d
                      :opaque-kind :texture-2d
                      :sample-result-type :vec4
                      :image-depth-p t)
(register-shader-type :uint-texture-2d
                      :opaque-kind :texture-2d
                      :sample-result-type :uvec4)
(register-shader-type :sampler :opaque-kind :sampler)
(register-shader-type :uniform-block :opaque-kind :uniform-block)
(register-shader-type :storage-buffer :opaque-kind :storage-buffer)

(defun find-shader-type (designator &optional source-form)
  (or (and (typep designator 'shader-type) designator)
      (and (symbolp designator)
           (loop for type being the hash-values of *shader-types*
                 when (string-equal (symbol-name designator)
                                    (symbol-name (shader-type-name type)))
                   return type))
      (error 'shader-language-error
             :form source-form :reason :unknown-type :details designator)))

(defun shader-type= (left right)
  (eq (find-shader-type left) (find-shader-type right)))

(defun shader-float-type-p (type)
  (let ((type (find-shader-type type)))
    (and (eq (shader-type-scalar-kind type) :float)
         (= (shader-type-component-count type) 1))))

(defun shader-uint-type-p (type)
  (shader-type= type :uint))

(defun shader-unsigned-type-p (type)
  (let ((type (find-shader-type type)))
    (and (eq (shader-type-scalar-kind type) :uint)
         (= (shader-type-component-count type) 1))))

(defun shader-vector-type-p (type)
  (let ((count (shader-type-component-count (find-shader-type type))))
    (and count (> count 1))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; An intermediate live version implemented these compatibility accessors
  ;; as ordinary functions.  Restore their original generic-function shape
  ;; before defining delegating methods over the common arithmetic slots.
  (dolist (name '(shader-object-name shader-object-source-form
                  shader-binding-expression
                  shader-expression-quantity-specification
                  shader-expression-quantity-layout
                  shader-expression-source-form shader-expression-name
                  (setf shader-expression-name)
                  shader-literal-value shader-reference-target
                  shader-call-operator shader-call-operands
                  shader-call-parameters shader-quantity-boundary-operand
                  shader-unit-conversion-operand
                  shader-unit-conversion-factor))
    (when (and (fboundp name)
               (not (typep (fdefinition name) 'generic-function)))
      (fmakunbound name))))

(defclass shader-named-object (lang:arithmetic-named-object) ())

(defgeneric shader-object-name (object))

(defmethod shader-object-name ((object shader-named-object))
  (lang:arithmetic-object-name object))

(defmethod shader-object-name ((object lang:arithmetic-function-source))
  (lang:arithmetic-object-name object))

(defgeneric shader-object-source-form (object))

(defmethod shader-object-source-form ((object shader-named-object))
  (lang:arithmetic-object-source-form object))

(defmethod shader-object-source-form
    ((object lang:arithmetic-function-source))
  (lang:arithmetic-object-source-form object))

(defclass shader-variable-declaration (shader-named-object)
  ((type
    :initarg :type
    :reader shader-declaration-type)
   (quantity-specification
    :initarg :quantity-specification
    :initform nil
    :reader shader-declaration-quantity-specification)
   (quantity-layout
    :initarg :quantity-layout
    :initform nil
    :reader shader-declaration-quantity-layout))
  (:documentation
   "A represented shader value with optional backend-neutral semantic meaning."))

(defmethod math:declaration-representation-type
    ((declaration shader-variable-declaration))
  (shader-declaration-type declaration))

(defmethod math:declaration-quantity-specification
    ((declaration shader-variable-declaration))
  (shader-declaration-quantity-specification declaration))

(defmethod math:declaration-quantity-layout
    ((declaration shader-variable-declaration))
  (shader-declaration-quantity-layout declaration))

(defmethod math:declaration-source-form
    ((declaration shader-variable-declaration))
  (shader-object-source-form declaration))

(defclass shader-interface-variable (shader-variable-declaration)
  ((direction
    :initarg :direction
    :reader shader-interface-direction)
   (location
    :initarg :location
    :initform nil
    :reader shader-interface-location)
   (built-in
    :initarg :built-in
    :initform nil
    :reader shader-interface-built-in)))

(defclass shader-resource (shader-variable-declaration)
  ((descriptor-set
    :initarg :descriptor-set
    :initform 0
    :reader shader-resource-descriptor-set)
   (binding
    :initarg :binding
    :reader shader-resource-binding)
   (sample-quantity-specification
    :initarg :sample-quantity-specification
    :initform nil
    :reader shader-resource-sample-quantity-specification)
   (sample-quantity-layout
    :initarg :sample-quantity-layout
    :initform nil
    :reader shader-resource-sample-quantity-layout)
   (sample-transfer
    :initarg :sample-transfer
    :initform nil
    :reader shader-resource-sample-transfer))
  (:documentation
   "A descriptor resource and, for textures, the represented meaning and
colour transfer of the value returned by sampling."))

(defclass shader-uniform-block (shader-resource)
  ((members
    :initarg :members
    :initform nil
    :accessor shader-uniform-block-members))
  (:documentation
   "One descriptor-backed uniform block with ordered, inspectable members."))

(defclass shader-uniform-member (shader-variable-declaration)
  ((block
    :initarg :block
    :reader shader-uniform-member-block)
   (index
    :initarg :index
    :reader shader-uniform-member-index)
   (offset
    :initarg :offset
    :reader shader-uniform-member-offset))
  (:documentation
   "A named value inside a SHADER-UNIFORM-BLOCK, not a separate resource."))

(defclass shader-storage-buffer (shader-resource)
  ((element-type
    :initarg :element-type
    :reader shader-storage-buffer-element-type))
  (:documentation
   "One descriptor-backed read-only array of uniformly typed elements.

The array has no declared length: a shader indexes it with BUFFER-ELEMENT
and the host decides how many elements it uploads.  #HFX2LI"))

(defun shader-storage-buffer-element-stride (buffer)
  "Return the byte distance between consecutive elements of BUFFER."
  (let ((type (shader-storage-buffer-element-type buffer)))
    (* (shader-type-component-count type)
       (floor (shader-type-bit-width type) 8))))

(defclass shader-task-payload (shader-named-object)
  ((fields
    :initarg :fields
    :initform nil
    :accessor shader-task-payload-fields))
  (:documentation
   "The named ABI shared by one task shader and its mesh shader consumers."))

(defclass shader-task-payload-field (shader-variable-declaration)
  ((payload
    :initarg :payload
    :reader shader-task-payload-field-payload)
   (index
    :initarg :index
    :reader shader-task-payload-field-index)
   (element-count
    :initarg :element-count
    :initform nil
    :reader shader-task-payload-field-element-count))
  (:documentation
   "One scalar/vector field or fixed array in a task payload ABI."))

(defgeneric task-payload-definition-for (name)
  (:documentation "Return the task-payload ABI named by NAME, or NIL."))

(defmethod task-payload-definition-for (name)
  (declare (ignore name))
  nil)

(defun parse-task-payload-field-type (form)
  (let ((designator (second form)))
    (if (and (consp designator)
             (shader-symbol= (first designator) :array)
             (= (length designator) 3))
        (destructuring-bind (array element-type element-count) designator
          (declare (ignore array))
          (unless (typep element-count '(integer 1 *))
            (error 'shader-language-error
                   :form form :reason :invalid-payload-array-size
                   :details element-count))
          (values (find-shader-type element-type form) element-count))
        (values (find-shader-type designator form) nil))))

(defun make-task-payload-definition (name field-forms source-form)
  (unless (and (listp field-forms) field-forms)
    (error 'shader-language-error
           :form source-form :reason :empty-task-payload))
  (let ((payload
          (make-instance 'shader-task-payload
                         :name name :source-form source-form)))
    (setf (shader-task-payload-fields payload)
          (loop with names = nil
                for field-form in field-forms
                for index from 0
                collect
                (destructuring-bind
                    (field-name field-type
                     &key quantity dimension unit affine-p character components)
                    field-form
                  (declare (ignore field-type))
                  (when (find field-name names :test #'shader-symbol=)
                    (error 'shader-language-error
                           :form field-form :reason :duplicate-payload-field
                           :details field-name))
                  (push field-name names)
                  (multiple-value-bind (type element-count)
                      (parse-task-payload-field-type field-form)
                    (let ((specification
                            (parse-declaration-quantity-specification
                             quantity dimension unit
                             (declared-character affine-p character)
                             type field-form)))
                      (make-instance
                       'shader-task-payload-field
                       :name field-name :type type
                       :quantity-specification specification
                       :quantity-layout
                       (parse-declaration-quantity-layout
                        components type field-form specification)
                       :payload payload :index index
                       :element-count element-count
                       :source-form field-form))))))
    payload))

(defmacro define-task-payload (name &body fields)
  "Define a durable, inspectable task-to-mesh payload ABI."
  (let* ((package (or (symbol-package name) *package*))
         (variable
           (intern (format nil "*~A-TASK-PAYLOAD*" (symbol-name name))
                   package)))
    `(progn
       (defparameter ,variable
         (make-task-payload-definition
          ',name ',fields '(define-task-payload ,name ,@fields)))
       (defmethod task-payload-definition-for ((payload-name (eql ',name)))
         (declare (ignore payload-name))
         ,variable)
       ',name)))

(defclass shader-map-definition (shader-named-object)
  ((domain-type
    :initarg :domain-type
    :reader shader-map-domain-type)
   (domain-quantity-specification
    :initarg :domain-quantity-specification
    :reader shader-map-domain-quantity-specification))
  (:documentation
   "An inspectable semantic map whose dense representation is supplied at use."))

(defclass shader-projective-map-definition (shader-map-definition)
  ((homogeneous-type
    :initarg :homogeneous-type
    :reader shader-projective-map-homogeneous-type)
   (sample-type
    :initarg :sample-type
    :reader shader-projective-map-sample-type)
   (sample-quantity-layout
    :initarg :sample-quantity-layout
    :reader shader-projective-map-sample-quantity-layout)
   (coordinate-scale
    :initarg :coordinate-scale
    :reader shader-projective-map-coordinate-scale)
   (coordinate-offset
    :initarg :coordinate-offset
    :reader shader-projective-map-coordinate-offset))
  (:documentation
   "A four-row homogeneous map with a separately checked sampling projection."))

(defgeneric shader-map-definition-for (name)
  (:documentation "Return the shader semantic map named by NAME, or NIL."))

(defmethod shader-map-definition-for (name)
  (declare (ignore name))
  nil)

(defun shader-uniform-block-byte-size (block)
  "The host byte size implied by BLOCK's shader-visible vec4-lane layout.

Hosts allocating a backing buffer should derive their size here rather than
repeating the lane arithmetic as a literal."
  (let ((members (shader-uniform-block-members block)))
    (if members
        (+ (shader-uniform-member-offset (car (last members))) 16)
        0)))

(defclass shader-binding
    (shader-named-object lang:arithmetic-binding)
  ())

(defclass shader-function-parameter-binding
    (shader-binding lang:arithmetic-function-parameter-binding)
  ()
  (:documentation
   "A lexical alias for one already parsed shader-function argument."))

(defgeneric shader-binding-expression (binding))

(defmethod shader-binding-expression ((binding shader-binding))
  (lang:arithmetic-binding-expression binding))

(defclass shader-expression (lang:arithmetic-expression)
  ((type
    :initarg :type
    :reader shader-expression-type)))

(defgeneric shader-expression-quantity-specification (expression))

(defmethod shader-expression-quantity-specification
    ((expression shader-expression))
  (lang:arithmetic-expression-quantity-specification expression))

(defgeneric shader-expression-quantity-layout (expression))

(defmethod shader-expression-quantity-layout ((expression shader-expression))
  (lang:arithmetic-expression-quantity-layout expression))

(defgeneric shader-expression-source-form (expression))

(defmethod shader-expression-source-form ((expression shader-expression))
  (lang:arithmetic-expression-source-form expression))

(defgeneric shader-expression-name (expression))

(defmethod shader-expression-name ((expression shader-expression))
  (lang:arithmetic-expression-name expression))

(defgeneric (setf shader-expression-name) (name expression))

(defmethod (setf shader-expression-name) (name (expression shader-expression))
  (setf (lang:arithmetic-expression-name expression) name))

(defgeneric shader-expression-quantity-checked-p (expression)
  (:documentation
   "Whether semantic quantity checking is active at EXPRESSION.

Annotations enter checked arithmetic; an explicit REPRESENTATION boundary
leaves it again while retaining the semantic operand in the expression graph."))

(defgeneric shader-expression-materialized-p (expression)
  (:documentation
   "Whether EXPRESSION directly denotes one lowerable represented value."))

(defmethod shader-expression-materialized-p ((expression shader-expression))
  (declare (ignore expression))
  t)

(defclass shader-literal
    (shader-expression lang:arithmetic-literal)
  ())

(defgeneric shader-literal-value (literal))

(defmethod shader-literal-value ((literal shader-literal))
  (lang:arithmetic-literal-value literal))

(defclass shader-reference
    (shader-expression lang:arithmetic-reference)
  ())

(defgeneric shader-reference-target (reference))

(defmethod shader-reference-target ((reference shader-reference))
  (lang:arithmetic-reference-target reference))

(defclass shader-call
    (shader-expression lang:arithmetic-call)
  ())

(defgeneric shader-call-operator (call))

(defmethod shader-call-operator ((call shader-call))
  (lang:arithmetic-call-operator call))

(defgeneric shader-call-operands (call))

(defmethod shader-call-operands ((call shader-call))
  (lang:arithmetic-call-operands call))

(defgeneric shader-call-parameters (call))

(defmethod shader-call-parameters ((call shader-call))
  (lang:arithmetic-call-parameters call))

(defclass shader-function-definition (lang:arithmetic-function-source)
  ()
  (:documentation
   "Reusable shader source parsed into the typed graph at each call site."))

(defgeneric shader-function-parameters (definition))

(defmethod shader-function-parameters
    ((definition lang:arithmetic-function-source))
  (lang:arithmetic-function-parameter-names definition))

(defgeneric shader-function-body (definition))

(defmethod shader-function-body
    ((definition lang:arithmetic-function-source))
  (lang:arithmetic-function-body definition))

(defclass shader-function-call
    (shader-expression lang:arithmetic-function-call)
  ()
  (:documentation
   "An inspectable typed call whose body is inlined during backend lowering."))

(defclass shader-conditional
    (shader-expression lang:arithmetic-conditional)
  ()
  (:documentation "A typed value conditional from the shared language."))

(defclass shader-counted-fold
    (shader-expression lang:arithmetic-counted-fold)
  ()
  (:documentation
   "A typed bounded fold lowered as structured target control flow."))

(defgeneric shader-function-call-definition (call))

(defmethod shader-function-call-definition ((call shader-function-call))
  (lang:arithmetic-function-call-definition call))

(defgeneric shader-function-call-arguments (call))

(defmethod shader-function-call-arguments ((call shader-function-call))
  (lang:arithmetic-function-call-arguments call))

(defgeneric shader-function-call-bindings (call))

(defmethod shader-function-call-bindings ((call shader-function-call))
  (lang:arithmetic-function-call-bindings call))

(defgeneric shader-function-call-result (call))

(defmethod shader-function-call-result ((call shader-function-call))
  (lang:arithmetic-function-call-result call))

(defclass shader-map-application (shader-expression)
  ((definition
    :initarg :definition
    :reader shader-map-application-definition)
   (point
    :initarg :point
    :reader shader-map-application-point)
   (rows
    :initarg :rows
    :reader shader-map-application-rows))
  (:documentation
   "The homogeneous result of applying a represented semantic map."))

(defclass shader-map-projection (shader-expression)
  ((application
    :initarg :application
    :reader shader-map-projection-application))
  (:documentation
   "A virtual sampling product projected from a homogeneous map application."))

(defclass shader-quantity-boundary
    (shader-expression lang:arithmetic-quantity-boundary)
  ())

(defgeneric shader-quantity-boundary-operand (expression))

(defmethod shader-quantity-boundary-operand
    ((expression shader-quantity-boundary))
  (lang:arithmetic-quantity-boundary-operand expression))

(defclass shader-interpretation (shader-quantity-boundary) ()
  (:documentation
   "A checked semantic name for a represented value, with no codegen effect."))

(defun shader-interpretation-operand (expression)
  (shader-quantity-boundary-operand expression))

(defclass shader-quantity-construction (shader-quantity-boundary) ()
  (:documentation
   "An explicitly meaningful value constructed from semantically raw source."))

(defun shader-quantity-construction-operand (expression)
  (shader-quantity-boundary-operand expression))

(defclass shader-quantity-assumption (shader-quantity-boundary) ()
  (:documentation
   "An explicit assumption that an opaque represented value has a meaning."))

(defun shader-quantity-assumption-operand (expression)
  (shader-quantity-boundary-operand expression))

(defclass shader-representation (shader-quantity-boundary) ()
  (:documentation
   "An explicit exposure of a semantic value's raw machine representation."))

(defun shader-representation-operand (expression)
  (shader-quantity-boundary-operand expression))

(defclass shader-unit-conversion
    (shader-expression lang:arithmetic-unit-conversion)
  ()
  (:documentation
   "An explicit linear unit conversion that may emit numerical scaling."))

(defgeneric shader-unit-conversion-operand (expression))

(defmethod shader-unit-conversion-operand
    ((expression shader-unit-conversion))
  (lang:arithmetic-unit-conversion-operand expression))

(defgeneric shader-unit-conversion-factor (expression))

(defmethod shader-unit-conversion-factor
    ((expression shader-unit-conversion))
  (lang:arithmetic-unit-conversion-factor expression))

(defmethod lang:arithmetic-reference-target-name
    ((target shader-named-object))
  (shader-object-name target))

(defmethod lang:arithmetic-reference-target-quantity-checked-p
    ((target shader-variable-declaration))
  (or (shader-declaration-quantity-specification target)
      (shader-declaration-quantity-layout target)))

(defmethod lang:arithmetic-reference-target-quantity-specification
    ((target shader-variable-declaration))
  (shader-declaration-quantity-specification target))

(defmethod lang:arithmetic-reference-target-quantity-layout
    ((target shader-variable-declaration))
  (shader-declaration-quantity-layout target))

(defmethod lang:arithmetic-expression-quantity-checked-p
    ((expression shader-map-application))
  (declare (ignore expression))
  t)

(defmethod lang:arithmetic-expression-quantity-checked-p
    ((expression shader-map-projection))
  (declare (ignore expression))
  t)

(defmethod lang:arithmetic-expression-quantity-checked-p
    ((expression shader-representation))
  (declare (ignore expression))
  nil)

(defmethod shader-expression-quantity-checked-p ((expression shader-literal))
  (lang:arithmetic-expression-quantity-checked-p expression))

(defmethod shader-expression-quantity-checked-p ((expression shader-reference))
  (lang:arithmetic-expression-quantity-checked-p expression))

(defmethod shader-expression-quantity-checked-p ((expression shader-call))
  (lang:arithmetic-expression-quantity-checked-p expression))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-function-call))
  (shader-expression-quantity-checked-p
   (shader-function-call-result expression)))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-conditional))
  (lang:arithmetic-expression-quantity-checked-p expression))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-counted-fold))
  (shader-expression-quantity-checked-p
   (lang:arithmetic-counted-fold-initial expression)))

(defmethod lang:arithmetic-expression-quantity-checked-p
    ((expression shader-function-call))
  (shader-expression-quantity-checked-p
   (shader-function-call-result expression)))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-map-application))
  (lang:arithmetic-expression-quantity-checked-p expression))


(defmethod shader-expression-materialized-p
    ((expression shader-map-projection))
  ;; Exact fields lower directly from cached components; no intermediate vec3
  ;; exists solely for compiler convenience.
  (declare (ignore expression))
  nil)

(defmethod shader-expression-quantity-checked-p
    ((expression shader-map-projection))
  (lang:arithmetic-expression-quantity-checked-p expression))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-quantity-boundary))
  (lang:arithmetic-expression-quantity-checked-p expression))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-representation))
  ;; This boundary is deliberately the one way for checked source to enter an
  ;; opaque representation-level calculation.  Its operand remains visible in
  ;; the graph, but arithmetic above the node is raw until meaning is assumed
  ;; again at another explicit boundary.
  (lang:arithmetic-expression-quantity-checked-p expression))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-unit-conversion))
  (lang:arithmetic-expression-quantity-checked-p expression))

(defclass shader-statement ()
  ((source-form
    :initarg :source-form
    :reader shader-statement-source-form))
  (:documentation
   "One ordered shader effect or structured group of shader effects."))

(defclass shader-output-assignment (shader-statement)
  ((output
    :initarg :output
    :reader shader-assignment-output)
   (value
    :initarg :value
    :reader shader-assignment-value)))

(defmethod shader-assignment-source-form
    ((assignment shader-output-assignment))
  (shader-statement-source-form assignment))

(defclass shader-conditional-statement (shader-statement)
  ((condition
    :initarg :condition
    :reader shader-conditional-statement-condition)
   (statements
    :initarg :statements
    :reader shader-conditional-statement-statements)))

(defclass shader-mesh-output-counts (shader-statement)
  ((vertex-count
    :initarg :vertex-count
    :reader shader-mesh-output-vertex-count)
   (primitive-count
    :initarg :primitive-count
    :reader shader-mesh-output-primitive-count)))

(defclass shader-mesh-vertex-store (shader-statement)
  ((index
    :initarg :index
    :reader shader-mesh-vertex-store-index)
   (values
    :initarg :values
    :reader shader-mesh-vertex-store-values)))

(defclass shader-mesh-primitive-store (shader-statement)
  ((index
    :initarg :index
    :reader shader-mesh-primitive-store-index)
   (indices
    :initarg :indices
    :reader shader-mesh-primitive-store-indices)
   (values
    :initarg :values
    :reader shader-mesh-primitive-store-values)))

(defclass shader-task-payload-store (shader-statement)
  ((field
    :initarg :field
    :reader shader-task-payload-store-field)
   (index
    :initarg :index
    :initform nil
    :reader shader-task-payload-store-index)
   (value
    :initarg :value
    :reader shader-task-payload-store-value)))

(defclass shader-emit-mesh-workgroups (shader-statement)
  ((workgroups
    :initarg :workgroups
    :reader shader-emit-mesh-workgroups-counts)))

(defclass shader-mesh-output ()
  ((topology
    :initarg :topology
    :reader shader-mesh-output-topology)
   (max-vertices
    :initarg :max-vertices
    :reader shader-mesh-output-max-vertices)
   (max-primitives
    :initarg :max-primitives
    :reader shader-mesh-output-max-primitives)
   (vertex-outputs
    :initarg :vertex-outputs
    :reader shader-mesh-output-vertex-outputs)
   (primitive-outputs
    :initarg :primitive-outputs
    :reader shader-mesh-output-primitive-outputs)
   (source-form
    :initarg :source-form
    :reader shader-mesh-output-source-form)))

(defclass shader-payload-element (shader-expression)
  ((field
    :initarg :field
    :reader shader-payload-element-field)
   (index
    :initarg :index
    :reader shader-payload-element-index)))

(defclass shader-buffer-element (shader-expression)
  ((buffer
    :initarg :buffer
    :reader shader-buffer-element-buffer)
   (index
    :initarg :index
    :reader shader-buffer-element-index)))

(defclass shader-bit-field-call (shader-call)
  ((size
    :initarg :size
    :reader shader-bit-field-size)
   (position
    :initarg :position
    :reader shader-bit-field-position))
  (:documentation
   "An LDB call whose byte specifier is part of the operator, not an operand."))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-payload-element))
  (or (shader-declaration-quantity-specification
       (shader-payload-element-field expression))
      (shader-declaration-quantity-layout
       (shader-payload-element-field expression))))

(defmethod shader-expression-quantity-checked-p
    ((expression shader-buffer-element))
  ;; Buffer elements are raw representations until a shader interprets them.
  (declare (ignore expression))
  nil)

(defmethod lang:arithmetic-expression-quantity-checked-p
    ((expression shader-payload-element))
  (shader-expression-quantity-checked-p expression))

(defmethod lang:arithmetic-expression-quantity-checked-p
    ((expression shader-buffer-element))
  (shader-expression-quantity-checked-p expression))

(defclass shader-specification (shader-named-object)
  ((stage
    :initarg :stage
    :reader shader-specification-stage)
   (inputs
    :initarg :inputs
    :initform nil
    :reader shader-specification-inputs)
   (outputs
    :initarg :outputs
    :initform nil
    :reader shader-specification-outputs)
   (resources
    :initarg :resources
    :initform nil
    :reader shader-specification-resources)
   (workgroup-size
    :initarg :workgroup-size
    :initform nil
    :reader shader-specification-workgroup-size)
   (task-payload
    :initarg :task-payload
    :initform nil
    :reader shader-specification-task-payload)
   (mesh-output
    :initarg :mesh-output
    :initform nil
    :reader shader-specification-mesh-output)
   (bindings
    :initarg :bindings
    :initform nil
    :reader shader-specification-bindings)
   (statements
    :initarg :statements
    :initform nil
    :reader shader-specification-statements)))

(defgeneric shader-expression-form (expression)
  (:documentation "Reconstruct the compact mathematical form of EXPRESSION."))

(defmethod shader-expression-form ((expression shader-literal))
  (lang:arithmetic-expression-form expression))

(defmethod shader-expression-form ((expression shader-reference))
  (lang:arithmetic-expression-form expression))

(defmethod shader-expression-form ((expression shader-call))
  (lang:arithmetic-expression-form expression))

(defmethod shader-expression-form ((expression shader-function-call))
  (shader-expression-source-form expression))

(defmethod shader-expression-form ((expression shader-conditional))
  (lang:arithmetic-expression-form expression))

(defmethod shader-expression-form ((expression shader-counted-fold))
  (shader-expression-source-form expression))

(defmethod shader-expression-form ((expression shader-map-application))
  (lang:arithmetic-expression-form expression))

(defmethod shader-expression-form ((expression shader-map-projection))
  (lang:arithmetic-expression-form expression))

(defmethod shader-expression-form ((expression shader-quantity-boundary))
  (lang:arithmetic-expression-form expression))

(defmethod shader-expression-form ((expression shader-unit-conversion))
  (lang:arithmetic-expression-form expression))

(defmethod shader-expression-form ((expression shader-payload-element))
  (shader-expression-source-form expression))

(defmethod shader-expression-form ((expression shader-buffer-element))
  (shader-expression-source-form expression))

(defmethod shader-expression-form ((expression shader-bit-field-call))
  (shader-expression-source-form expression))

(defmethod lang:arithmetic-expression-form
    ((expression shader-map-application))
  (shader-expression-source-form expression))

(defmethod lang:arithmetic-expression-form
    ((expression shader-map-projection))
  (shader-expression-source-form expression))

(defmethod print-object ((expression shader-expression) stream)
  (print-unreadable-object (expression stream :type t)
    (prin1 (shader-expression-form expression) stream)
    (format stream " : ~A"
            (shader-type-name (shader-expression-type expression)))))

(defgeneric shader-expression-children (expression)
  (:documentation "The immediate subexpressions EXPRESSION computes from."))

(defmethod shader-expression-children ((expression shader-expression))
  (lang:arithmetic-expression-children expression))

(defmethod shader-expression-children ((expression shader-call))
  (lang:arithmetic-expression-children expression))

(defmethod shader-expression-children ((expression shader-function-call))
  (append (shader-function-call-arguments expression)
          (list (shader-function-call-result expression))))

(defmethod shader-expression-children ((expression shader-map-application))
  (lang:arithmetic-expression-children expression))

(defmethod shader-expression-children ((expression shader-map-projection))
  (lang:arithmetic-expression-children expression))

(defmethod shader-expression-children ((expression shader-quantity-boundary))
  (lang:arithmetic-expression-children expression))

(defmethod shader-expression-children ((expression shader-unit-conversion))
  (lang:arithmetic-expression-children expression))

(defmethod shader-expression-children ((expression shader-payload-element))
  (list (shader-payload-element-index expression)))

(defmethod shader-expression-children ((expression shader-buffer-element))
  (list (shader-buffer-element-index expression)))

(defmethod lang:arithmetic-expression-children
    ((expression shader-map-application))
  (cons (shader-map-application-point expression)
        (shader-map-application-rows expression)))

(defmethod lang:arithmetic-expression-children
    ((expression shader-map-projection))
  (list (shader-map-projection-application expression)))

(defgeneric shader-statement-expressions (statement)
  (:documentation
   "Return the expressions directly or recursively owned by STATEMENT."))

(defmethod shader-statement-expressions ((statement shader-output-assignment))
  (list (shader-assignment-value statement)))

(defmethod shader-statement-expressions
    ((statement shader-conditional-statement))
  (cons (shader-conditional-statement-condition statement)
        (mapcan #'shader-statement-expressions
                (shader-conditional-statement-statements statement))))

(defmethod shader-statement-expressions
    ((statement shader-mesh-output-counts))
  (list (shader-mesh-output-vertex-count statement)
        (shader-mesh-output-primitive-count statement)))

(defmethod shader-statement-expressions
    ((statement shader-mesh-vertex-store))
  (cons (shader-mesh-vertex-store-index statement)
        (mapcar #'cdr (shader-mesh-vertex-store-values statement))))

(defmethod shader-statement-expressions
    ((statement shader-mesh-primitive-store))
  (list* (shader-mesh-primitive-store-index statement)
         (shader-mesh-primitive-store-indices statement)
         (mapcar #'cdr (shader-mesh-primitive-store-values statement))))

(defmethod shader-statement-expressions
    ((statement shader-task-payload-store))
  (remove nil (list (shader-task-payload-store-index statement)
                    (shader-task-payload-store-value statement))))

(defmethod shader-statement-expressions
    ((statement shader-emit-mesh-workgroups))
  (list (shader-emit-mesh-workgroups-counts statement)))

(defun shader-specification-expressions (specification)
  "Return the expression graph in source order, without duplicate objects."
  (let ((seen (make-hash-table :test #'eq))
        (expressions nil))
    (labels ((visit (expression)
               (unless (gethash expression seen)
                 (setf (gethash expression seen) t)
                 (push expression expressions)
                 (mapc #'visit (shader-expression-children expression)))))
      (dolist (binding (shader-specification-bindings specification))
        (visit (shader-binding-expression binding)))
      (dolist (statement (shader-specification-statements specification))
        (mapc #'visit (shader-statement-expressions statement))))
    (nreverse expressions)))

(defgeneric shader-expression-uniformity (expression)
  (:documentation
   "Return :WORKGROUP or :INVOCATION for the expression's finest variation."))

(defun combine-shader-uniformities (&rest uniformities)
  (if (member :invocation uniformities) :invocation :workgroup))

(defmethod shader-expression-uniformity ((expression shader-literal))
  (declare (ignore expression))
  :workgroup)

(defmethod shader-expression-uniformity ((expression shader-reference))
  (let ((target (shader-reference-target expression)))
    (typecase target
      (shader-binding
       (shader-expression-uniformity (shader-binding-expression target)))
      (shader-task-payload-field :workgroup)
      (shader-uniform-member :workgroup)
      (shader-resource :workgroup)
      (shader-interface-variable
       (if (member (shader-interface-built-in target)
                   '(:workgroup-id :num-workgroups :workgroup-size))
           :workgroup
           :invocation))
      (t :invocation))))

(defmethod shader-expression-uniformity ((expression shader-expression))
  (apply #'combine-shader-uniformities
         (mapcar #'shader-expression-uniformity
                 (shader-expression-children expression))))

(defun shader-expression-workgroup-uniform-p (expression)
  (eq :workgroup (shader-expression-uniformity expression)))

(defun shader-symbol= (left right)
  (and (symbolp left) (symbolp right)
       (string-equal (symbol-name left) (symbol-name right))))

(defun find-shader-environment-value (name environment)
  (loop for (candidate . value) in environment
        when (shader-symbol= name candidate)
          return value))

(defun shader-environment-value (name environment source-form)
  (or (find-shader-environment-value name environment)
      (error 'shader-language-error
             :form source-form :reason :unknown-name :details name)))

(defun make-shader-reference (name environment source-form)
  (let ((target (shader-environment-value name environment source-form)))
    (when (and (typep target 'shader-task-payload-field)
               (shader-task-payload-field-element-count target))
      (error 'shader-language-error
             :form source-form :reason :payload-array-requires-element
             :details name))
    (when (typep target 'shader-storage-buffer)
      (error 'shader-language-error
             :form source-form :reason :storage-buffer-requires-element
             :details name))
    (make-instance 'shader-reference
                   :target target
                   :type (etypecase target
                           (shader-variable-declaration
                            (shader-declaration-type target))
                           (shader-binding
                            (shader-expression-type
                             (shader-binding-expression target))))
                   :quantity-specification
                   (etypecase target
                     (shader-variable-declaration
                      (shader-declaration-quantity-specification target))
                     (shader-binding
                      (shader-expression-quantity-specification
                       (shader-binding-expression target))))
                   :quantity-layout
                   (etypecase target
                     (shader-variable-declaration
                      (shader-declaration-quantity-layout target))
                     (shader-binding
                      (shader-expression-quantity-layout
                       (shader-binding-expression target))))
                   :source-form source-form)))

(defun shader-numeric-type-p (type)
  (not (null (shader-type-component-count type))))

(defun require-shader-types (predicate operands source-form reason)
  (unless (funcall predicate (mapcar #'shader-expression-type operands))
    (error 'shader-language-error
           :form source-form :reason reason
           :details (mapcar (lambda (operand)
                              (shader-type-name
                               (shader-expression-type operand)))
                            operands))))

(defgeneric infer-shader-call-type (operator operands source-form)
  (:documentation
   "Check OPERANDS against OPERATOR's contract and return the call's type."))

(defgeneric infer-shader-call-quantity-specification
    (operator operands source-form)
  (:documentation
   "Derive semantic meaning for one shader call once annotations enter it."))

(defgeneric infer-shader-call-quantity-layout (operator operands source-form)
  (:documentation
   "Derive a packed semantic layout for one shader call, or return NIL."))

(defmethod infer-shader-call-quantity-layout
    (operator operands source-form)
  (declare (ignore operator operands source-form))
  nil)

(defmethod infer-shader-call-quantity-specification
    (operator operands source-form)
  "Derive semantics totally once any operand descends from an annotation.

An entirely unannotated graph remains valid legacy shader source.  Once an
annotation enters a calculation, however, a missing operand specification or
an operator without a backend-neutral rule is a source error rather than a
silent loss of meaning."
  (handler-case
      (lang:infer-arithmetic-call-quantity-specification
       operator operands source-form)
    (lang:arithmetic-language-error (condition)
      (error 'shader-language-error
             :form (lang:arithmetic-language-error-form condition)
             :reason (lang:arithmetic-language-error-reason condition)
             :details (lang:arithmetic-language-error-details condition)))))

(defun require-semantic-operands (operands source-form indices)
  (loop for index in indices
        for operand = (nth index operands)
        for specification = (and operand
                                 (shader-expression-quantity-specification
                                  operand))
        unless specification
          collect (and operand (shader-expression-form operand)) into missing
        finally
           (when missing
             (error 'shader-language-error
                    :form source-form
                    :reason :missing-quantity-specification
                    :details missing))))

(defun require-dimensionless-coordinate (specification source-form)
  (unless (and (= (math:quantity-specification-tensor-order specification) 1)
               (math:dimensionless-p
                (math:quantity-specification-dimension specification))
               (math:unitless-p
                (math:quantity-specification-unit specification)))
    (error 'shader-language-error
           :form source-form :reason :invalid-sample-coordinate-quantity
           :details specification)))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'sample)) operands source-form)
  (declare (ignore operator))
  (let ((coordinate (third operands)))
    (when (shader-expression-quantity-checked-p coordinate)
      (require-semantic-operands operands source-form '(2))
      (require-dimensionless-coordinate
       (shader-expression-quantity-specification coordinate) source-form)))
  (let ((texture (shader-reference-target (first operands))))
    (shader-resource-sample-quantity-specification texture)))

(defmethod infer-shader-call-quantity-layout
    ((operator (eql 'sample)) operands source-form)
  (declare (ignore operator source-form))
  (let ((texture (shader-reference-target (first operands))))
    (shader-resource-sample-quantity-layout texture)))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'texel-load)) operands source-form)
  (declare (ignore operator source-form))
  (let ((texture (shader-reference-target (first operands))))
    (shader-resource-sample-quantity-specification texture)))

(defmethod infer-shader-call-quantity-layout
    ((operator (eql 'texel-load)) operands source-form)
  (declare (ignore operator source-form))
  (let ((texture (shader-reference-target (first operands))))
    (shader-resource-sample-quantity-layout texture)))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'uint)) operands source-form)
  (declare (ignore operator operands source-form))
  nil)

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'uint64)) operands source-form)
  (declare (ignore operator operands source-form))
  nil)

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'float)) operands source-form)
  (declare (ignore operator operands source-form))
  nil)

(defmethod infer-shader-call-quantity-layout
    ((operator (eql 'uint)) operands source-form)
  (declare (ignore operator operands source-form))
  nil)

(defmethod infer-shader-call-quantity-layout
    ((operator (eql 'uint64)) operands source-form)
  (declare (ignore operator operands source-form))
  nil)

(defmethod infer-shader-call-quantity-layout
    ((operator (eql 'float)) operands source-form)
  (declare (ignore operator operands source-form))
  nil)

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'sample-compare)) operands source-form)
  (declare (ignore operator))
  (let ((coordinate (third operands))
        (reference (fourth operands)))
    (when (or (shader-expression-quantity-checked-p coordinate)
              (shader-expression-quantity-checked-p reference))
      (require-semantic-operands operands source-form '(2 3))
      (require-dimensionless-coordinate
       (shader-expression-quantity-specification coordinate) source-form)
      (unless (zerop (math:quantity-specification-tensor-order
                      (shader-expression-quantity-specification reference)))
        (error 'shader-language-error
               :form source-form :reason :invalid-depth-reference-quantity
               :details
               (shader-expression-quantity-specification reference)))
      (math:make-quantity-specification nil))))

(defun infer-vector-constructor-quantity-specification
    (operator operands source-form)
  (declare (ignore operator))
  (when (some #'shader-expression-quantity-checked-p operands)
    (require-semantic-operands
     operands source-form (loop for index below (length operands) collect index))
    (let* ((specifications
             (mapcar #'shader-expression-quantity-specification operands))
           (first (first specifications))
           (name (math:quantity-specification-name first)))
      (dolist (specification (rest specifications))
        (unless (and (math:dimension=
                      (math:quantity-specification-dimension specification)
                      (math:quantity-specification-dimension first))
                     (math:unit-expression=
                      (math:quantity-specification-unit specification)
                      (math:quantity-specification-unit first))
                     (eq (math:quantity-specification-character specification)
                         (math:quantity-specification-character first)))
          (error 'shader-language-error
                 :form source-form
                 :reason :invalid-quantity-operation
                 :details :incompatible-vector-constituents))
        (unless (eq (math:quantity-specification-name specification) name)
          (setf name nil)))
      (math:make-quantity-specification
       name
       :dimension (math:quantity-specification-dimension first)
       :unit (math:quantity-specification-unit first)
       :tensor-order 1
       :character (math:quantity-specification-character first)))))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'vec2)) operands source-form)
  (infer-vector-constructor-quantity-specification
   operator operands source-form))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'vec3)) operands source-form)
  (infer-vector-constructor-quantity-specification
   operator operands source-form))

(defmethod infer-shader-call-quantity-specification
    ((operator (eql 'vec4)) operands source-form)
  (infer-vector-constructor-quantity-specification
   operator operands source-form))

(defmethod infer-shader-call-type (operator operands source-form)
  (declare (ignore operands))
  (error 'shader-language-error
         :form source-form :reason :unknown-operator :details operator))

(defun require-numeric-operands (operator operands source-form)
  (unless operands
    (error 'shader-language-error
           :form source-form :reason :missing-operands :details operator))
  (require-shader-types
   (lambda (types) (every #'shader-numeric-type-p types))
   operands source-form :non-numeric-arithmetic))

(defun infer-uniform-arithmetic-type (operator operands source-form)
  "The shared + and - contract: every operand carries one common type."
  (require-numeric-operands operator operands source-form)
  (let ((types (mapcar #'shader-expression-type operands)))
    (unless (every (lambda (type) (shader-type= type (first types)))
                   (rest types))
      (error 'shader-language-error
             :form source-form :reason :incompatible-arithmetic-types
             :details (mapcar #'shader-type-name types)))
    (first types)))

(defmethod infer-shader-call-type ((operator (eql '+)) operands source-form)
  (infer-uniform-arithmetic-type operator operands source-form))

(defmethod infer-shader-call-type ((operator (eql '-)) operands source-form)
  (infer-uniform-arithmetic-type operator operands source-form))

(defmethod infer-shader-call-type ((operator (eql '*)) operands source-form)
  (require-numeric-operands operator operands source-form)
  (let* ((types (mapcar #'shader-expression-type operands))
         (vectors (remove-if-not #'shader-vector-type-p types)))
    (cond ((every (lambda (type) (shader-type= type (first types)))
                  (rest types))
           (first types))
          ((and vectors
                (eq :float
                    (shader-type-scalar-kind (first vectors)))
                (every (lambda (type)
                         (or (shader-float-type-p type)
                             (shader-type= type (first vectors))))
                       types))
           (first vectors))
          (t
           (error 'shader-language-error
                  :form source-form :reason :incompatible-product-types
                  :details (mapcar #'shader-type-name types))))))

(defmethod infer-shader-call-type ((operator (eql '/)) operands source-form)
  (require-numeric-operands operator operands source-form)
  (let ((types (mapcar #'shader-expression-type operands)))
    (unless (= (length types) 2)
      (error 'shader-language-error
             :form source-form :reason :division-arity))
    (cond ((shader-type= (first types) (second types)) (first types))
          ((and (shader-vector-type-p (first types))
                (eq :float
                    (shader-type-scalar-kind (first types)))
                (shader-float-type-p (second types)))
           (first types))
          (t
           (error 'shader-language-error
                  :form source-form :reason :incompatible-division-types
                  :details (mapcar #'shader-type-name types))))))

(defun infer-scalar-comparison-type (operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= 2 (length types))
          (= 1 (shader-type-component-count (first types)))
          (member (shader-type-scalar-kind (first types)) '(:float :uint))
          (shader-type= (first types) (second types))))
   operands source-form :invalid-scalar-comparison)
  (find-shader-type :bool))

(defmacro define-scalar-comparison-type (operator)
  `(defmethod infer-shader-call-type
       ((operator (eql ',operator)) operands source-form)
     (declare (ignore operator))
     (infer-scalar-comparison-type operands source-form)))

(define-scalar-comparison-type <)
(define-scalar-comparison-type <=)
(define-scalar-comparison-type >)
(define-scalar-comparison-type >=)
(define-scalar-comparison-type =)

(defun swizzle-components (designator source-form)
  (let* ((name (string-downcase (symbol-name designator)))
         (indices
           (loop for character across name
                 collect (or (position character "xyzw")
                             (position character "rgba")
                             (error 'shader-language-error
                                    :form source-form
                                    :reason :invalid-swizzle
                                    :details designator)))))
    (unless (<= 1 (length indices) 4)
      (error 'shader-language-error
             :form source-form :reason :invalid-swizzle-length
             :details designator))
    indices))

(defun vector-type-for-width (width source-form &optional (scalar-kind :float))
  (find-shader-type
   (ecase scalar-kind
     (:float (ecase width (1 :float) (2 :vec2) (3 :vec3) (4 :vec4)))
     (:uint (ecase width (1 :uint) (2 :uvec2) (3 :uvec3) (4 :uvec4))))
   source-form))

(defun vector-constructor-width (operands source-form)
  (loop for operand in operands
        for count = (shader-type-component-count
                     (shader-expression-type operand))
        unless count
          do (error 'shader-language-error
                    :form source-form :reason :opaque-vector-constituent)
        sum count))

(defmethod infer-shader-call-type ((operator (eql 'dot)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 2)
          (shader-vector-type-p (first types))
          (eq :float (shader-type-scalar-kind (first types)))
          (shader-type= (first types) (second types))))
   operands source-form :invalid-dot-product)
  (find-shader-type :float))

(defmethod infer-shader-call-type ((operator (eql 'sample)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 3)
          (eq (shader-type-opaque-kind (first types)) :texture-2d)
          (shader-type-sample-result-type (first types))
          (eq (shader-type-opaque-kind (second types)) :sampler)
          (shader-type= (third types) :vec2)))
   operands source-form :invalid-texture-sample)
  (find-shader-type
   (shader-type-sample-result-type
    (shader-expression-type (first operands)))))

(defmethod infer-shader-call-type
    ((operator (eql 'texel-load)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 2)
          (eq (shader-type-opaque-kind (first types)) :texture-2d)
          (shader-type-sample-result-type (first types))
          (shader-type= (second types) :uvec2)))
   operands source-form :invalid-texel-load)
  (find-shader-type
   (shader-type-sample-result-type
    (shader-expression-type (first operands)))))

(defmethod infer-shader-call-type ((operator (eql 'uint)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 1)
          (= 1 (shader-type-component-count (first types)))
          (member (shader-type-scalar-kind (first types)) '(:float :uint))))
   operands source-form :invalid-uint-conversion)
  (find-shader-type :uint))

(defmethod infer-shader-call-type
    ((operator (eql 'uint64)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 1)
          (= 1 (shader-type-component-count (first types)))
          (member (shader-type-scalar-kind (first types)) '(:float :uint))))
   operands source-form :invalid-uint64-conversion)
  (find-shader-type :uint64))

(defmethod infer-shader-call-type ((operator (eql 'float)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 1)
          (= 1 (shader-type-component-count (first types)))
          (member (shader-type-scalar-kind (first types)) '(:float :uint))))
   operands source-form :invalid-float-conversion)
  (find-shader-type :float))

(defmethod infer-shader-call-type ((operator (eql 'mod)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 2)
          (every #'shader-unsigned-type-p types)
          (shader-type= (first types) (second types))))
   operands source-form :invalid-unsigned-remainder)
  (shader-expression-type (first operands)))

(defmethod infer-shader-call-type
    ((operator (eql 'sample-compare)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 4)
          (eq (shader-type-opaque-kind (first types)) :texture-2d)
          (shader-type-image-depth-p (first types))
          (eq (shader-type-opaque-kind (second types)) :sampler)
          (shader-type= (third types) :vec2)
          (shader-type= (fourth types) :float)))
   operands source-form :invalid-depth-comparison-sample)
  (find-shader-type :float))

(defmethod infer-shader-call-type ((operator (eql 'mix)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 3)
          (shader-type= (first types) (second types))
          (eq :float (shader-type-scalar-kind (first types)))
          (shader-float-type-p (third types))))
   operands source-form :invalid-mix)
  (shader-expression-type (first operands)))

;;; The extended-math family shares one componentwise contract: a fixed or
;;; open operand count, every operand carrying the same scalar or vector
;;; type, and a result of that type.  Each operator states its accepted
;;; signature explicitly instead of promising every GLSL overload.

(defun infer-uniform-extended-type (operator operands source-form minimum maximum)
  (unless (and (<= minimum (length operands))
               (or (null maximum) (<= (length operands) maximum)))
    (error 'shader-language-error
           :form source-form :reason :wrong-operand-count
           :details (list operator (length operands))))
  (let ((type (infer-uniform-arithmetic-type
               operator operands source-form)))
    (unless (eq :float (shader-type-scalar-kind type))
      (error 'shader-language-error
             :form source-form :reason :invalid-extended-math-type
             :details (shader-type-name type)))
    type))

(defmethod infer-shader-call-type ((operator (eql 'min)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 2 nil))

(defmethod infer-shader-call-type ((operator (eql 'max)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 2 nil))

(defmethod infer-shader-call-type ((operator (eql 'abs)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 1 1))

(defmethod infer-shader-call-type
    ((operator (eql 'signum)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 1 1))

(defmethod infer-shader-call-type ((operator (eql 'sqrt)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 1 1))

(macrolet
    ((define-unary-extended-type (&rest operators)
       `(progn
          ,@(mapcar
             (lambda (operator)
               `(defmethod infer-shader-call-type
                    ((operator (eql ',operator)) operands source-form)
                  (infer-uniform-extended-type
                   operator operands source-form 1 1)))
             operators))))
  (define-unary-extended-type floor fract sin cos exp log))

(defmethod infer-shader-call-type
    ((operator (eql 'derivative-x)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 1 1))

(defmethod infer-shader-call-type
    ((operator (eql 'derivative-y)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 1 1))

(defmethod infer-shader-call-type ((operator (eql 'expt)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 2 2))

(defmethod infer-shader-call-type ((operator (eql 'clamp)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 3 3))

(defmethod infer-shader-call-type
    ((operator (eql 'smoothstep)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 3 3))

(defmethod infer-shader-call-type ((operator (eql 'step)) operands source-form)
  (infer-uniform-extended-type operator operands source-form 2 2))

(defmethod infer-shader-call-type
    ((operator (eql 'normalize)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 1)
          (shader-vector-type-p (first types))
          (eq :float (shader-type-scalar-kind (first types)))))
   operands source-form :invalid-normalize)
  (shader-expression-type (first operands)))

(defun infer-vector-constructor-type (type-name width operands source-form)
  (unless (= width (vector-constructor-width operands source-form))
    (error 'shader-language-error
           :form source-form :reason :invalid-vector-width :details width))
  (find-shader-type type-name))

(defmethod infer-shader-call-type ((operator (eql 'vec2)) operands source-form)
  (infer-vector-constructor-type :vec2 2 operands source-form))

(defmethod infer-shader-call-type ((operator (eql 'vec3)) operands source-form)
  (infer-vector-constructor-type :vec3 3 operands source-form))

(defmethod infer-shader-call-type ((operator (eql 'vec4)) operands source-form)
  (infer-vector-constructor-type :vec4 4 operands source-form))

(defmethod infer-shader-call-type ((operator (eql 'uvec2)) operands source-form)
  (let ((type (infer-vector-constructor-type :uvec2 2 operands source-form)))
    (unless (every (lambda (operand)
                     (eq :uint (shader-type-scalar-kind
                                (shader-expression-type operand))))
                   operands)
      (error 'shader-language-error
             :form source-form :reason :invalid-unsigned-vector-constituent))
    type))

(defmethod infer-shader-call-type ((operator (eql 'uvec3)) operands source-form)
  (let ((type (infer-vector-constructor-type :uvec3 3 operands source-form)))
    (unless (every (lambda (operand)
                     (eq :uint (shader-type-scalar-kind
                                (shader-expression-type operand))))
                   operands)
      (error 'shader-language-error
             :form source-form :reason :invalid-unsigned-vector-constituent))
    type))

(defmethod infer-shader-call-type ((operator (eql 'uvec4)) operands source-form)
  (let ((type (infer-vector-constructor-type :uvec4 4 operands source-form)))
    (unless (every (lambda (operand)
                     (eq :uint (shader-type-scalar-kind
                                (shader-expression-type operand))))
                   operands)
      (error 'shader-language-error
             :form source-form :reason :invalid-unsigned-vector-constituent))
    type))

;;; Operators are named by ordinary symbols, treating the shader language as
;;; a small compiled subset of Common Lisp plus a vector library.  Where CL
;;; already has the word with the right meaning the operator IS that symbol:
;;; + is CL:+, exactly as SBCL's own compiler keys IR knowledge off standard
;;; names it never funcalls.  Where CL is silent (DOT, MIX, SWIZZLE, ...) the
;;; operator is an exported symbol of this package.  A SHADER-CALL stores the
;;; symbol, never a resolved behavior object, so redefining an operator's
;;; methods reaches every existing specification on its next compile.

(defvar *shader-operator-documentation* (make-hash-table :test #'eq))

(defmethod documentation ((name symbol) (type (eql 'shader-operator)))
  (gethash name *shader-operator-documentation*))

(defmethod (setf documentation)
    (new-value (name symbol) (type (eql 'shader-operator)))
  (if new-value
      (setf (gethash name *shader-operator-documentation*) new-value)
      (progn (remhash name *shader-operator-documentation*) nil)))

(defgeneric shader-operator-p (operator)
  (:documentation
   "Whether OPERATOR names an operator of the shader language.

Membership is an open set of EQL methods: DEFINE-SHADER-OPERATOR admits a
name, and the operator's behavior arrives as further EQL methods on
PARSE-SHADER-OPERATOR-CALL, INFER-SHADER-CALL-TYPE, and LOWER-SHADER-CALL."))

(defmethod shader-operator-p (operator)
  (declare (ignore operator))
  nil)

(defmacro define-shader-operator (name &optional documentation)
  "Admit NAME into the shader operator vocabulary, with its documentation.

Documentation lives under the SHADER-OPERATOR doc-type, so shader meaning
never collides with a standard symbol's function documentation:
\(DOCUMENTATION 'CL:+ 'SHADER-OPERATOR) answers for shaders alone."
  `(progn
     (defmethod shader-operator-p ((operator (eql ',name)))
       t)
     ,@(when documentation
         `((setf (documentation ',name 'shader-operator) ,documentation)))
     ',name))

(define-shader-operator +
  "Componentwise addition of uniformly typed scalar or vector values.")
(define-shader-operator -
  "Componentwise subtraction, or negation when given a single operand.")
(define-shader-operator *
  "Chained multiplication of scalars, matching vectors, and vector-scalar pairs.")
(define-shader-operator /
  "Division of two matching values, or a vector scaled by a scalar's reciprocal.")
(define-shader-operator dot
  "The scalar inner product of two vectors of the same width.")
(define-shader-operator sample
  "Sample a two-dimensional texture through a sampler at a UV coordinate.")
(define-shader-operator sample-compare
  "Compare a depth reference through a comparison sampler at a UV coordinate.")
(define-shader-operator texel-load
  "Load one exact two-dimensional texel at an unsigned integer coordinate.")
(define-shader-operator payload-element
  "Read one indexed element of the task payload shared with a mesh shader.")
(define-shader-operator buffer-element
  "Read one indexed element of a storage buffer resource.")
(define-shader-operator ldb
  "Extract the unsigned bit field (BYTE SIZE POSITION) of one unsigned scalar.")
(define-shader-operator derivative-x
  "Return the horizontal screen-space derivative of a fragment value.")
(define-shader-operator derivative-y
  "Return the vertical screen-space derivative of a fragment value.")
(define-shader-operator uint
  "Convert one scalar float or unsigned value to a 32-bit unsigned integer.")
(define-shader-operator uint64
  "Convert one scalar float or unsigned value to a 64-bit unsigned integer.")
(define-shader-operator float
  "Convert one scalar float or unsigned value to a 32-bit float.")
(define-shader-operator mod
  "Return the unsigned remainder of two scalar integer values.")
(define-shader-operator mix
  "Linear interpolation from one value toward another by a scalar amount.")
(define-shader-operator vec2
  "Construct a two-component vector from scalars and vectors of total width 2.")
(define-shader-operator vec3
  "Construct a three-component vector from scalars and vectors of total width 3.")
(define-shader-operator vec4
  "Construct a four-component vector from scalars and vectors of total width 4.")
(define-shader-operator uvec2
  "Construct a two-component unsigned vector from unsigned constituents.")
(define-shader-operator uvec3
  "Construct a three-component unsigned vector from unsigned constituents.")
(define-shader-operator uvec4
  "Construct a four-component unsigned vector from unsigned constituents.")
(define-shader-operator swizzle
  "Select and reorder vector components by a designator such as :XYZ or :RGB.")
(define-shader-operator min
  "The componentwise minimum of two or more uniformly typed values.")
(define-shader-operator max
  "The componentwise maximum of two or more uniformly typed values.")
(define-shader-operator abs
  "The componentwise absolute value of one scalar or vector.")
(define-shader-operator signum
  "The componentwise sign: negative one, zero, or positive one.")
(define-shader-operator sqrt
  "The componentwise square root of one scalar or vector.")
(define-shader-operator expt
  "Raise a value to a power, componentwise over one uniform type.")
;;; The transcendental and lattice family: what procedural image mathematics
;;; needs to build hashes, value noise, and periodic shaping without a table.
(define-shader-operator floor
  "The componentwise greatest integer not above one scalar or vector.")
(define-shader-operator fract
  "The componentwise fractional part: the value less its floor.")
(define-shader-operator sin
  "The componentwise sine of one scalar or vector of radians.")
(define-shader-operator cos
  "The componentwise cosine of one scalar or vector of radians.")
(define-shader-operator exp
  "The componentwise natural exponential of one scalar or vector.")
(define-shader-operator log
  "The componentwise natural logarithm of one scalar or vector.")
(define-shader-operator < "Test two scalar floats for ordered less-than.")
(define-shader-operator <= "Test two scalar floats for ordered less-or-equal.")
(define-shader-operator > "Test two scalar floats for ordered greater-than.")
(define-shader-operator >= "Test two scalar floats for ordered greater-or-equal.")
(define-shader-operator = "Test two scalar floats for ordered equality.")
(define-shader-operator clamp
  "Constrain a value between uniformly typed lower and upper bounds.")
(define-shader-operator smoothstep
  "Hermite interpolation from zero to one across an edge pair, then a value.")
(define-shader-operator step
  "Return zero below an edge and one at or above it, componentwise.")
(define-shader-operator normalize
  "Scale one vector to unit length.")
(define-shader-operator quantity
  "Construct an explicitly meaningful scalar or vector literal.")
(define-shader-operator assume-quantity
  "State an explicit semantic assumption about an otherwise raw value.")
(define-shader-operator interpret
  "Assign a checked semantic quantity specification without emitting code.")
(define-shader-operator representation
  "Expose a semantic value's raw representation without emitting code.")
(define-shader-operator project-point
  "Apply a named projective map to a semantic affine point.")
(define-shader-operator project-sample
  "Project a homogeneous map application into its declared sampling product.")
(define-shader-operator convert-unit
  "Explicitly express a semantic quantity in another compatible unit.")

;;; Shader functions are typed source composition.  Authors write an ordinary
;;; expression body, including lexical LET*, and every call is parsed against
;;; its actual arguments into an inspectable SHADER-FUNCTION-CALL.  Backends
;;; inline the resulting expression graph; no shader author constructs forms.

(defvar *shader-function-documentation* (make-hash-table :test #'eq))

(defmethod documentation ((name symbol) (type (eql 'shader-function)))
  (gethash name *shader-function-documentation*))

(defmethod (setf documentation)
    (new-value (name symbol) (type (eql 'shader-function)))
  (if new-value
      (setf (gethash name *shader-function-documentation*) new-value)
      (progn (remhash name *shader-function-documentation*) nil)))

(defgeneric shader-function-definition-for (name)
  (:documentation "Return the live typed shader function named by NAME, or NIL."))

(defmethod shader-function-definition-for (name)
  (declare (ignore name))
  nil)

(defun make-shader-function-definition (name parameters body)
  (unless (and (symbolp name)
               (listp parameters)
               (every #'symbolp parameters)
               (= (length parameters)
                  (length (remove-duplicates parameters :test #'eq))))
    (error 'shader-language-error
           :form (list* 'define-shader-function name parameters body)
           :reason :invalid-shader-function-parameters
           :details parameters))
  (unless (= (length body) 1)
    (error 'shader-language-error
           :form (list* 'define-shader-function name parameters body)
           :reason :expected-single-shader-function-body))
  (make-instance 'shader-function-definition
                 :name name
                 :parameter-forms parameters
                 :parameter-names parameters
                 :body body
                 :source-form
                 (list* 'define-shader-function name parameters body)))

(defmacro define-shader-function (name parameters &body body)
  "Define a reusable typed shader expression with ordinary source syntax.

The body is parsed at each call site, so argument types and quantity meanings
flow through the same operator protocol as handwritten shader expressions.
LET* is lexical inside the function.  The definition macro records source; its
body does not execute as Lisp and does not return generated S-expressions.
#RO74NL"
  (let* ((function-name (gensym "FUNCTION-NAME"))
         (documentation (and (stringp (first body)) (first body)))
         (forms (if documentation (rest body) body)))
    `(progn
       (forget-shader-abstraction ',name)
       (defmethod shader-function-definition-for
           ((,function-name (eql ',name)))
         (declare (ignore ,function-name))
         (load-time-value
          (make-shader-function-definition ',name ',parameters ',forms)))
       ,@(when documentation
           `((setf (documentation ',name 'shader-function)
                   ,documentation)))
       (note-shader-source-redefinition ',name)
       ',name)))

;;; Abstractions are source vocabulary, not core operators.  They rewrite into
;;; ordinary shader forms before parsing.  Keep them for genuinely syntactic
;;; generation, such as unrolling host-known data; ordinary reusable
;;; calculations belong in DEFINE-SHADER-FUNCTION above.

(defvar *shader-abstraction-documentation* (make-hash-table :test #'eq))

(defvar *shader-source-revision* 0)
(defvar *shader-source-revision-lock*
  (sb-thread:make-mutex :name "luv shader source revision"))

(defun shader-source-revision ()
  "Return the revision of reusable shader functions and source abstractions."
  (sb-thread:with-mutex (*shader-source-revision-lock*)
    *shader-source-revision*))

(defun shader-abstraction-revision ()
  "Compatibility name for SHADER-SOURCE-REVISION."
  (shader-source-revision))

(defun note-shader-source-redefinition (name)
  "Record that reusable shader source named by NAME may have changed."
  (declare (ignore name))
  (sb-thread:with-mutex (*shader-source-revision-lock*)
    (incf *shader-source-revision*)))

(defmethod lang:note-arithmetic-function-redefinition ((name symbol))
  "Prefer newly shared source over any shader-only source of the same name."
  (when (fboundp 'forget-shader-function)
    (forget-shader-function name))
  (note-shader-source-redefinition name))

(defmethod documentation ((name symbol) (type (eql 'shader-abstraction)))
  (gethash name *shader-abstraction-documentation*))

(defmethod (setf documentation)
    (new-value (name symbol) (type (eql 'shader-abstraction)))
  (if new-value
      (setf (gethash name *shader-abstraction-documentation*) new-value)
      (progn (remhash name *shader-abstraction-documentation*) nil)))

(defgeneric shader-abstraction-p (operator)
  (:documentation
   "Whether OPERATOR names source vocabulary expanded before shader parsing."))

(defmethod shader-abstraction-p (operator)
  (declare (ignore operator))
  nil)

(defgeneric expand-shader-abstraction-call (operator form)
  (:documentation
   "Expand one source abstraction call into core shader-language forms."))

(defmethod expand-shader-abstraction-call (operator form)
  (declare (ignore operator))
  (error 'shader-language-error
         :form form :reason :unknown-abstraction :details (first form)))

(defun remove-shader-eql-methods (generic-function-name name)
  (when (fboundp generic-function-name)
    (let ((generic-function (fdefinition generic-function-name)))
      (when (typep generic-function 'generic-function)
        (dolist (method
                 (copy-list
                  (closer-mop:generic-function-methods generic-function)))
          (let ((specializer
                  (first (closer-mop:method-specializers method))))
            (when (and (typep specializer 'closer-mop:eql-specializer)
                       (eq name
                           (closer-mop:eql-specializer-object specializer)))
              (remove-method generic-function method))))))))

(defun forget-shader-abstraction (name)
  "Remove an old source-rewriter definition when NAME becomes a function."
  (remove-shader-eql-methods 'shader-abstraction-p name)
  (remove-shader-eql-methods 'expand-shader-abstraction-call name)
  (remhash name *shader-abstraction-documentation*)
  name)

(defun forget-shader-function (name)
  "Remove an old typed-function definition when NAME becomes an abstraction."
  (remove-shader-eql-methods 'shader-function-definition-for name)
  (remhash name *shader-function-documentation*)
  name)

(defmacro define-shader-abstraction (name lambda-list &body body)
  "Define NAME as a source-level shader abstraction.

The expansion body receives the destructured call operands and returns a raw
shader source form made from core operators or other abstractions."
  (let* ((form (gensym "FORM"))
         (operator (gensym "OPERATOR"))
         (documentation (and (stringp (first body)) (first body)))
         (forms (if documentation (rest body) body)))
    `(progn
       (forget-shader-function ',name)
       (defmethod shader-abstraction-p ((,operator (eql ',name)))
         t)
       (defmethod expand-shader-abstraction-call
           ((,operator (eql ',name)) ,form)
         (destructuring-bind (,operator ,@lambda-list) ,form
           (declare (ignore ,operator))
           ,@forms))
       ,@(when documentation
           `((setf (documentation ',name 'shader-abstraction)
                   ,documentation)))
       (note-shader-source-redefinition ',name)
       ',name)))

(defun expand-shader-source-form (form)
  "Expand all shader abstraction calls nested inside FORM."
  (cond ((atom form) form)
        ((and (symbolp (first form)) (shader-abstraction-p (first form)))
         (expand-shader-source-form
          (expand-shader-abstraction-call (first form) form)))
        (t (mapcar #'expand-shader-source-form form))))

(define-shader-abstraction shadow-depth-test
    (depth-texture sampler coordinate receiver-depth bias)
  "One receiver-versus-depth-map comparison, filterable after comparison."
  `(sample-compare ,depth-texture ,sampler ,coordinate
                   (- ,receiver-depth ,bias)))

(define-shader-abstraction shadow-visibility
    (depth-texture sampler coordinate receiver-depth receiver-depth-gradient
     texel-size bias radius)
  "Weighted disk PCF with receiver-plane depth correction at every tap."
  (flet ((tap (x y weight)
           (let ((offset
                   `(interpret
                     (* ,texel-size
                        (vec2 (* ,radius ,x) (* ,radius ,y)))
                     :quantity :shadow-uv :character :difference)))
             `(* ,weight
                 (shadow-depth-test
                  ,depth-texture ,sampler
                  (+ ,coordinate ,offset)
                  (+ ,receiver-depth
                     (interpret
                      (dot ,receiver-depth-gradient ,offset)
                      :quantity :shadow-depth :character :difference))
                  ,bias)))))
    `(/ (+
         ,(tap 0.0 0.0 4.0)
         ;; An inner, heavier ring gives the footprint a Gaussian-like core.
         ,@(loop for (x y) in '((0.45 0.0) (0.3182 0.3182)
                                (0.0 0.45) (-0.3182 0.3182)
                                (-0.45 0.0) (-0.3182 -0.3182)
                                (0.0 -0.45) (0.3182 -0.3182))
                 collect (tap x y 2.0))
         ;; Rotate the outer ring by half a sector so no square sample border
         ;; is reinforced along the light-space axes.
         ,@(loop for (x y) in '((0.9239 0.3827) (0.3827 0.9239)
                                (-0.3827 0.9239) (-0.9239 0.3827)
                                (-0.9239 -0.3827) (-0.3827 -0.9239)
                                (0.3827 -0.9239) (0.9239 -0.3827))
                 collect (tap x y 1.0)))
        28.0)))

(defgeneric parse-shader-operator-call (operator form environment)
  (:documentation
   "Parse one (OPERATOR . ARGUMENTS) form into a typed SHADER-CALL.

The default method parses every argument as an expression and asks
INFER-SHADER-CALL-TYPE for the result type; an operator with special
syntax, such as SWIZZLE's component designator, replaces parsing wholesale."))

(defmethod parse-shader-operator-call (operator form environment)
  (let ((operands (mapcar (lambda (operand)
                            (parse-shader-expression operand environment))
                          (rest form))))
    (make-instance 'shader-call
                   :operator operator
                   :operands operands
                   :type (infer-shader-call-type operator operands form)
                   :quantity-specification
                   (infer-shader-call-quantity-specification
                    operator operands form)
                   :quantity-layout
                   (infer-shader-call-quantity-layout
                    operator operands form)
                   :source-form form)))

(defmethod parse-shader-operator-call ((operator (eql 'swizzle)) form environment)
  (unless (= (length (rest form)) 2)
    (error 'shader-language-error :form form :reason :swizzle-arity))
  (let* ((operand (parse-shader-expression (second form) environment))
         (designator (third form))
         (indices (swizzle-components designator form))
         (input-width
           (shader-type-component-count (shader-expression-type operand))))
    (unless (and input-width (every (lambda (index) (< index input-width))
                                    indices))
      (error 'shader-language-error
             :form form :reason :swizzle-out-of-range
             :details designator))
    (let* ((specification
             (shader-expression-quantity-specification operand))
           (layout (shader-expression-quantity-layout operand))
           (projected
             (or (and layout
                      (math:project-quantity-layout layout indices))
                 (and specification
                      (with-shader-quantity-errors
                          (form :invalid-quantity-projection)
                        (math:project-quantity-specification
                         specification indices input-width))))))
      (when (and (shader-expression-quantity-checked-p operand)
                 (null projected))
        (error 'shader-language-error
               :form form :reason :undeclared-quantity-projection
               :details designator))
      (make-instance 'shader-call
                     :operator 'swizzle
                     :operands (list operand)
                     :parameters (list designator)
                     :type (vector-type-for-width
                            (length indices) form
                            (shader-type-scalar-kind
                             (shader-expression-type operand)))
                     :quantity-specification projected
                     :source-form form))))

(defmethod parse-shader-operator-call
    ((operator (eql 'payload-element)) form environment)
  (declare (ignore operator))
  (unless (= (length form) 3)
    (error 'shader-language-error
           :form form :reason :payload-element-arity))
  (let* ((field-name (second form))
         (field (shader-environment-value field-name environment form))
         (index (parse-shader-expression (third form) environment)))
    (unless (and (typep field 'shader-task-payload-field)
                 (shader-task-payload-field-element-count field))
      (error 'shader-language-error
             :form form :reason :not-payload-array :details field-name))
    (unless (shader-uint-type-p (shader-expression-type index))
      (error 'shader-language-error
             :form form :reason :payload-index-type
             :details (shader-type-name (shader-expression-type index))))
    (multiple-value-bind (constant-index constant-p)
        (shader-constant-uint-value index)
      (when (and constant-p
                 (>= constant-index
                     (shader-task-payload-field-element-count field)))
        (error 'shader-language-error
               :form form :reason :payload-index-out-of-bounds
               :details
               (list constant-index
                     (shader-task-payload-field-element-count field)))))
    (make-instance 'shader-payload-element
                   :field field :index index
                   :type (shader-declaration-type field)
                   :quantity-specification
                   (shader-declaration-quantity-specification field)
                   :quantity-layout (shader-declaration-quantity-layout field)
                   :source-form form)))

(defmethod parse-shader-operator-call
    ((operator (eql 'buffer-element)) form environment)
  (declare (ignore operator))
  (unless (= (length form) 3)
    (error 'shader-language-error
           :form form :reason :buffer-element-arity))
  (let* ((buffer-name (second form))
         (buffer (shader-environment-value buffer-name environment form))
         (index (parse-shader-expression (third form) environment)))
    (unless (typep buffer 'shader-storage-buffer)
      (error 'shader-language-error
             :form form :reason :not-storage-buffer :details buffer-name))
    (unless (shader-uint-type-p (shader-expression-type index))
      (error 'shader-language-error
             :form form :reason :buffer-index-type
             :details (shader-type-name (shader-expression-type index))))
    (make-instance 'shader-buffer-element
                   :buffer buffer :index index
                   :type (shader-storage-buffer-element-type buffer)
                   :quantity-specification nil
                   :quantity-layout nil
                   :source-form form)))

(defun shader-constant-integer-value (form)
  "Return FORM's integer value when it is an integer literal or a constant
symbol naming one, else NIL."
  (cond ((integerp form) form)
        ((and (symbolp form) (constantp form) (boundp form)
              (integerp (symbol-value form)))
         (symbol-value form))
        (t nil)))

(defmethod parse-shader-operator-call
    ((operator (eql 'ldb)) form environment)
  "Parse (LDB (BYTE SIZE POSITION) VALUE).

SIZE is a positive integer.  POSITION is an integer, or a 32-bit unsigned
expression for a field whose place is only known at run time; then the field
must still fit the operand's width, which is checked by the caller's data
rather than the language."
  (unless (= (length form) 3)
    (error 'shader-language-error :form form :reason :ldb-arity))
  (let* ((specifier (second form))
         (size (and (consp specifier) (= (length specifier) 3)
                    (shader-symbol= (first specifier) 'byte)
                    (shader-constant-integer-value (second specifier))))
         (position (and size
                        (shader-constant-integer-value (third specifier))))
         (position-expression
           (and size (null position)
                (parse-shader-expression (third specifier) environment)))
         (value (parse-shader-expression (third form) environment))
         (type (shader-expression-type value)))
    (unless (and size (plusp size)
                 (or (and position (>= position 0))
                     position-expression))
      (error 'shader-language-error
             :form form :reason :invalid-byte-specifier :details specifier))
    (unless (shader-unsigned-type-p type)
      (error 'shader-language-error
             :form form :reason :invalid-bit-field-operand
             :details (shader-type-name type)))
    (when (and position-expression
               (not (shader-uint-type-p
                     (shader-expression-type position-expression))))
      (error 'shader-language-error
             :form form :reason :byte-position-type
             :details (shader-type-name
                       (shader-expression-type position-expression))))
    (unless (<= (+ size (or position 0)) (shader-type-bit-width type))
      (error 'shader-language-error
             :form form :reason :byte-specifier-exceeds-width
             :details (list specifier (shader-type-bit-width type))))
    (make-instance 'shader-bit-field-call
                   :operator operator
                   :operands (if position-expression
                                 (list value position-expression)
                                 (list value))
                   :size size :position position
                   :type type
                   :quantity-specification nil
                   :quantity-layout nil
                   :source-form form)))

(defvar *shader-function-call-counter* 0)
(defvar *shader-function-call-stack* nil)

(defun shader-function-binding-name
    (definition call-index role name &optional ordinal)
  (let ((function-name (symbol-name (shader-object-name definition)))
        (binding-name (symbol-name name)))
    (make-symbol
     (if ordinal
         (format nil "~A-~D-~A-~D-~A"
                 function-name call-index role ordinal binding-name)
         (format nil "~A-~D-~A-~A"
                 function-name call-index role binding-name)))))

(defun parse-shader-expression-body
    (body environment &key binding-name-function)
  "Parse one expression body, optionally beginning with lexical LET*."
  (unless (= (length body) 1)
    (error 'shader-language-error
           :form body :reason :expected-single-shader-function-body))
  (let ((form (first body)))
    (if (and (consp form) (eq (first form) 'let*))
        (destructuring-bind (operator raw-bindings &rest results) form
          (declare (ignore operator))
          (unless (= (length results) 1)
            (error 'shader-language-error
                   :form form :reason :expected-single-shader-function-result))
          (let ((bindings nil)
                (lexical-environment environment))
            (dolist (raw-binding raw-bindings)
              (unless (and (consp raw-binding) (= (length raw-binding) 2)
                           (symbolp (first raw-binding)))
                (error 'shader-language-error
                       :form raw-binding :reason :invalid-binding))
              (let* ((source-name (first raw-binding))
                     (name (if binding-name-function
                               (funcall binding-name-function source-name)
                               source-name))
                     (expression
                       (parse-shader-expression
                        (second raw-binding) lexical-environment))
                     (binding
                       (make-instance 'shader-binding
                                      :name name :expression expression
                                      :source-form raw-binding)))
                (setf (shader-expression-name expression) name)
                (push binding bindings)
                (push (cons source-name binding) lexical-environment)))
            (values (nreverse bindings)
                    (parse-shader-expression
                     (first results) lexical-environment))))
        (values nil (parse-shader-expression form environment)))))

(defun parse-shader-function-call (definition form environment)
  (let* ((name (shader-object-name definition))
         (parameters (shader-function-parameters definition))
         (argument-forms (rest form)))
    (unless (= (length parameters) (length argument-forms))
      (error 'shader-language-error
             :form form :reason :shader-function-arity
             :details (list :expected (length parameters)
                            :actual (length argument-forms))))
    (when (member name *shader-function-call-stack* :test #'eq)
      (error 'shader-language-error
             :form form :reason :recursive-shader-function
             :details (reverse (cons name *shader-function-call-stack*))))
    (let* ((arguments
             (mapcar (lambda (argument)
                       (parse-shader-expression argument environment))
                     argument-forms))
           (call-index (incf *shader-function-call-counter*))
           (parameter-bindings
             (loop for parameter in parameters
                   for argument in arguments
                   collect
                   (make-instance
                    'shader-function-parameter-binding
                    :name (shader-function-binding-name
                           definition call-index "PARAMETER" parameter)
                    :expression argument
                    :source-form (list parameter argument))))
           (function-environment
             (loop for parameter in parameters
                   for binding in parameter-bindings
                   collect (cons parameter binding))))
      (let ((*shader-function-call-stack*
              (cons name *shader-function-call-stack*))
            (local-index 0))
        (multiple-value-bind (body-bindings result)
            (parse-shader-expression-body
             (shader-function-body definition) function-environment
             :binding-name-function
             (lambda (local-name)
               (shader-function-binding-name
                definition call-index "LOCAL" local-name
                (incf local-index))))
          (make-instance
           'shader-function-call
           :definition definition :arguments arguments
           :bindings (append parameter-bindings body-bindings)
           :result result
           :type (shader-expression-type result)
           :quantity-specification
           (shader-expression-quantity-specification result)
           :quantity-layout (shader-expression-quantity-layout result)
           :source-form form))))))

(defun parse-shader-counted-fold (form environment)
  (multiple-value-bind (index-name count-form state-name initial-form
                        update-form until-form valid-p)
      (lang:counted-fold-form-parts form)
    (unless valid-p
      (error 'shader-language-error
             :form form :reason :invalid-counted-fold))
    (unless (and (symbolp index-name) (symbolp state-name)
                 (not (eq index-name state-name)))
      (error 'shader-language-error
             :form form :reason :invalid-counted-fold-bindings))
    (let* ((count (parse-shader-expression count-form environment))
           (initial (parse-shader-expression initial-form environment))
           (count-type (shader-expression-type count))
           (index-binding
             (make-instance 'shader-binding
                            :name index-name
                            :expression
                            (make-instance
                             'shader-literal
                             :value (if (shader-uint-type-p count-type) 0 0.0)
                             :type count-type
                             :quantity-specification
                             (and (shader-float-type-p count-type)
                                  (math:make-quantity-specification nil))
                             :source-form (list index-name 0))
                            :source-form (list index-name 0)))
           (state-binding
             (make-instance 'shader-binding
                            :name state-name :expression initial
                            :source-form (list state-name initial-form)))
           (fold-environment
             (list* (cons index-name index-binding)
                    (cons state-name state-binding)
                    environment)))
      (multiple-value-bind (update-bindings update)
          (parse-shader-expression-body (list update-form) fold-environment)
        (unless (or (shader-float-type-p count-type)
                    (shader-uint-type-p count-type))
          (error 'shader-language-error
                 :form count-form :reason :counted-fold-count-type))
        (unless (and (shader-type=
                      (shader-expression-type initial)
                      (shader-expression-type update))
                     (lang:arithmetic-state-compatible-p initial update))
          (error 'shader-language-error
                 :form form :reason :counted-fold-state-mismatch))
        (make-instance
         'shader-counted-fold
         :count count :initial initial
         :index-binding index-binding :state-binding state-binding
         :bindings update-bindings :update update
         :until (parse-shader-counted-fold-until until-form fold-environment)
         :type (shader-expression-type initial)
         :quantity-specification
         (shader-expression-quantity-specification initial)
         :quantity-layout (shader-expression-quantity-layout initial)
         :source-form form)))))

(defun parse-shader-counted-fold-until (form environment)
  "Parse a COUNTED-FOLD's :UNTIL test in the fold's ENVIRONMENT.

The test is evaluated at the head of every iteration, seeing the index and
the carried state, and must be a straight-line boolean: it lowers into the
loop header, so it may not itself fold."
  (when form
    (let ((until (parse-shader-expression form environment)))
      (unless (shader-type= (shader-expression-type until) :bool)
        (error 'shader-language-error
               :form form :reason :counted-fold-until-type
               :details (shader-type-name (shader-expression-type until))))
      until)))

(defun parse-shader-conditional (form environment)
  (unless (= 4 (length form))
    (error 'shader-language-error :form form :reason :conditional-arity))
  (let ((condition (parse-shader-expression (second form) environment))
        (consequent (parse-shader-expression (third form) environment))
        (alternative (parse-shader-expression (fourth form) environment)))
    (unless (shader-type= (shader-expression-type condition) :bool)
      (error 'shader-language-error
             :form (second form) :reason :conditional-condition-type
             :details (shader-type-name (shader-expression-type condition))))
    (unless (and (shader-type= (shader-expression-type consequent)
                               (shader-expression-type alternative))
                 (lang:arithmetic-state-compatible-p consequent alternative))
      (error 'shader-language-error
             :form form :reason :conditional-branch-mismatch
             :details (list (shader-expression-form consequent)
                            (shader-expression-form alternative))))
    (make-instance
     'shader-conditional
     :condition condition :consequent consequent :alternative alternative
     :type (shader-expression-type consequent)
     :quantity-specification
     (shader-expression-quantity-specification consequent)
     :quantity-layout (shader-expression-quantity-layout consequent)
     :source-form form)))

(defun parse-shader-call (form environment)
  (let* ((operator (first form))
         (function (and (symbolp operator)
                        (or (shader-function-definition-for operator)
                            (lang:arithmetic-function-definition-for
                             operator)))))
    (cond ((eq operator 'counted-fold)
           (parse-shader-counted-fold form environment))
          ((eq operator 'if)
           (parse-shader-conditional form environment))
          ((shader-operator-p operator)
           (parse-shader-operator-call operator form environment))
          (function
           (parse-shader-function-call function form environment))
          (t
           (error 'shader-language-error
                  :form form :reason :unknown-operator :details operator)))))

;;; A live named value -- a knob, in the application's word -- may stand in
;;; shader source as a symbol.  It folds to a literal at parse time, and the
;;; parse records what was folded, so a live pipeline can tell when the
;;; source it was built from has quietly moved.

(defgeneric shader-source-value (name)
  (:documentation
   "The live value NAME stands for in shader source, or NIL when NAME names
none.  Returns (VALUES VALUE DECLARATION FOUND-P): VALUE is a real, and
DECLARATION, when given, is a represented-value declaration whose quantity
the literal takes on."))

(defmethod shader-source-value ((name t))
  (declare (ignore name))
  (values nil nil nil))

(defvar *shader-source-value-references* nil
  "When bound to a cons cell, its car collects (NAME . VALUE) for every live
named value folded while parsing, so the parser's caller can remember what
its artifact depends on.")

(defun shader-source-value-literal (form)
  "FORM as a folded literal when it names a live shader source value."
  (multiple-value-bind (value declaration found-p) (shader-source-value form)
    (when found-p
      (when *shader-source-value-references*
        (push (cons form value) (car *shader-source-value-references*)))
      (make-instance 'shader-literal
                     :value (coerce value 'single-float)
                     :type (find-shader-type :float)
                     :quantity-specification
                     (or (and declaration
                              (math:declaration-quantity-specification
                               declaration))
                         (math:make-quantity-specification nil))
                     :source-form form))))

(defun shader-source-value-references-current-p (references)
  "Whether every (NAME . VALUE) in REFERENCES still names that value."
  (every (lambda (reference)
           (multiple-value-bind (value declaration found-p)
               (shader-source-value (car reference))
             (declare (ignore declaration))
             (and found-p (eql value (cdr reference)))))
         references))

(defun parse-shader-expression (form environment)
  (cond ((realp form)
         (make-instance 'shader-literal
                        :value (coerce form 'single-float)
                        :type (find-shader-type :float)
                        :quantity-specification
                        (math:make-quantity-specification nil)
                        :source-form form))
        ((symbolp form)
         (cond ((find-shader-environment-value form environment)
                (make-shader-reference form environment form))
               ((shader-source-value-literal form))
               ((and (constantp form) (boundp form)
                     (realp (symbol-value form)))
                (make-instance 'shader-literal
                               :value (coerce (symbol-value form) 'single-float)
                               :type (find-shader-type :float)
                               :quantity-specification
                               (math:make-quantity-specification nil)
                               :source-form form))
               (t
                (make-shader-reference form environment form))))
        ((consp form) (parse-shader-call form environment))
        (t
         (error 'shader-language-error
                :form form :reason :unsupported-expression))))

(defun shader-type-tensor-order (type source-form)
  (let ((count (shader-type-component-count type)))
    (unless count
      (error 'shader-language-error
             :form source-form
             :reason :quantity-on-opaque-type
             :details (shader-type-name type)))
    (if (= count 1) 0 1)))

(defun declared-character (affine-p character)
  "Merge the historical :AFFINE-P and the general :CHARACTER source keywords
into one designator for PARSE-DECLARATION-QUANTITY-SPECIFICATION."
  (or character (and affine-p t)))

(defun declared-character-options (character)
  "Translate a source character designator into constructor options.

NIL leaves the character to the named definition; T is the historical
:AFFINE-P spelling of a point; a keyword names the character directly."
  (cond ((null character) nil)
        ((eq character t) (list :character :point))
        (t (list :character character))))

(defun parse-declaration-quantity-specification
    (quantity dimension unit character type source-form)
  (when (or quantity dimension unit character)
    (with-shader-quantity-errors
        (source-form :invalid-quantity-declaration)
      (math:make-declared-quantity-specification
       (append
        (and quantity (list :quantity quantity))
        (and dimension (list :dimension dimension))
        (and unit (list :unit unit))
        (list :tensor-order (shader-type-tensor-order type source-form))
        (declared-character-options character))))))

(defun parse-declaration-quantity-layout
    (components type source-form &optional whole)
  (when components
    (let ((extent (shader-type-component-count type)))
      (unless (and extent (> extent 1))
        (error 'shader-language-error
               :form source-form :reason :quantity-components-on-non-vector
               :details (shader-type-name type)))
      (let ((occupied nil)
            (projections nil))
        (dolist (component-form components)
          (destructuring-bind
              (selector &key quantity
                        (dimension nil dimension-supplied-p)
                        (unit nil unit-supplied-p)
                        (affine-p nil affine-supplied-p)
                        (character nil character-supplied-p))
              component-form
            (unless quantity
              (error 'shader-language-error
                     :form component-form
                     :reason :missing-component-quantity))
            (let ((positions (swizzle-components selector component-form)))
              (unless (every (lambda (position) (< position extent)) positions)
                (error 'shader-language-error
                       :form component-form :reason :swizzle-out-of-range
                       :details selector))
              (let ((overlap (intersection positions occupied)))
                (when overlap
                  (error 'shader-language-error
                         :form component-form
                         :reason :overlapping-quantity-components
                         :details overlap)))
              (setf occupied (append positions occupied))
              (let ((projection-type
                      (vector-type-for-width
                       (length positions) component-form
                       (shader-type-scalar-kind type))))
                (push
                 (math:make-quantity-projection
                  positions
                  (parse-declaration-quantity-specification
                   quantity
                   (if dimension-supplied-p
                       dimension
                       (and whole
                            (math:quantity-specification-dimension whole)))
                   (if unit-supplied-p
                       unit
                       (and whole
                            (math:quantity-specification-unit whole)))
                   (cond (character-supplied-p character)
                         (affine-supplied-p (and affine-p :point))
                         (whole (math:quantity-specification-character whole))
                         (t nil))
                   projection-type component-form))
                 projections)))))
        (math:make-quantity-layout extent (nreverse projections))))))

(defun make-projective-shader-map-definition
    (name &key domain-type domain-quantity domain-dimension domain-unit
               domain-affine-p sample-type sample-components
               coordinate-scale coordinate-offset source-form)
  "Construct a checked projective map definition from declarative semantics."
  (let* ((domain-type (find-shader-type domain-type source-form))
         (homogeneous-type (find-shader-type :vec4 source-form))
         (sample-type (find-shader-type sample-type source-form))
         (domain-component-count
           (shader-type-component-count domain-type))
         (component-count (shader-type-component-count sample-type)))
    (unless (and domain-component-count
                 component-count
                 (= domain-component-count 3)
                 (= component-count 3)
                 (listp coordinate-scale)
                 (listp coordinate-offset)
                 (= (length coordinate-scale) component-count)
                 (= (length coordinate-offset) component-count)
                 (every #'realp coordinate-scale)
                 (every #'realp coordinate-offset))
      (error 'shader-language-error
             :form source-form :reason :invalid-projective-map-shape))
    (let ((domain
            (parse-declaration-quantity-specification
             domain-quantity domain-dimension domain-unit domain-affine-p
             domain-type source-form))
          (sample-layout
            (parse-declaration-quantity-layout
             sample-components sample-type source-form)))
      (unless (and domain
                   (math:quantity-specification-affine-p domain)
                   sample-layout)
        (error 'shader-language-error
               :form source-form :reason :invalid-projective-map-semantics))
      (make-instance
       'shader-projective-map-definition
       :name name
       :source-form source-form
       :domain-type domain-type
       :domain-quantity-specification domain
       :homogeneous-type homogeneous-type
       :sample-type sample-type
       :sample-quantity-layout sample-layout
       :coordinate-scale coordinate-scale
       :coordinate-offset coordinate-offset))))

(defmacro define-projective-shader-map
    (name &key domain-type domain-quantity domain-dimension domain-unit
               domain-affine-p sample-type sample-components
               coordinate-scale coordinate-offset)
  "Define an inspectable projective map behind an EQL-specialized protocol."
  (let ((storage
          (intern (format nil "*~A-SHADER-MAP*" (symbol-name name))
                  *package*))
        (source
          `(define-projective-shader-map ,name
             :domain-type ,domain-type
             :domain-quantity ,domain-quantity
             :domain-dimension ,domain-dimension
             :domain-unit ,domain-unit
             :domain-affine-p ,domain-affine-p
             :sample-type ,sample-type
             :sample-components ,sample-components
             :coordinate-scale ,coordinate-scale
             :coordinate-offset ,coordinate-offset)))
    `(progn
       (defparameter ,storage
         (make-projective-shader-map-definition
          ',name
          :domain-type ',domain-type
          :domain-quantity ',domain-quantity
          :domain-dimension ',domain-dimension
          :domain-unit ',domain-unit
          :domain-affine-p ',domain-affine-p
          :sample-type ',sample-type
          :sample-components ',sample-components
          :coordinate-scale ',coordinate-scale
          :coordinate-offset ',coordinate-offset
          :source-form ',source))
       (defmethod shader-map-definition-for ((map-name (eql ',name)))
         (declare (ignore map-name))
         ,storage)
       ',name)))

(defmethod parse-shader-operator-call
    ((operator (eql 'project-point)) form environment)
  (declare (ignore operator))
  (destructuring-bind (name map-name point-form &rest row-forms) form
    (declare (ignore name))
    (unless (symbolp map-name)
      (error 'shader-language-error
             :form form :reason :invalid-shader-map-name
             :details map-name))
    (let ((definition (shader-map-definition-for map-name)))
      (unless definition
        (error 'shader-language-error
               :form form :reason :undefined-shader-map :details map-name))
      (unless (= (length row-forms) 4)
        (error 'shader-language-error
               :form form :reason :projective-map-row-count
               :details (length row-forms)))
      (let* ((point (parse-shader-expression point-form environment))
             (rows
               (mapcar (lambda (row)
                         (parse-shader-expression row environment))
                       row-forms))
             (point-quantity
               (shader-expression-quantity-specification point)))
        (unless (and (shader-type=
                      (shader-expression-type point)
                      (shader-map-domain-type definition))
                     (shader-expression-quantity-checked-p point)
                     point-quantity
                     (math:quantity-specification=
                      point-quantity
                      (shader-map-domain-quantity-specification definition)))
          (error 'shader-language-error
                 :form form :reason :projective-map-domain-mismatch
                 :details (shader-expression-form point)))
        (unless (every
                 (lambda (row)
                   (and (shader-type= (shader-expression-type row) :vec4)
                        (not (shader-expression-quantity-checked-p row))))
                 rows)
          (error 'shader-language-error
                 :form form :reason :invalid-projective-map-rows
                 :details (mapcar #'shader-expression-form rows)))
        (make-instance
         'shader-map-application
         :definition definition
         :point point
         :rows rows
         :type (shader-projective-map-homogeneous-type definition)
         :source-form form)))))

(defmethod parse-shader-operator-call
    ((operator (eql 'project-sample)) form environment)
  (declare (ignore operator))
  (destructuring-bind (name application-form) form
    (declare (ignore name))
    (let* ((operand (parse-shader-expression application-form environment))
           (application (shader-map-application-for-projection operand)))
      (unless application
        (error 'shader-language-error
               :form form :reason :sampling-projection-requires-map-application
               :details (shader-expression-form operand)))
      (let ((definition (shader-map-application-definition application)))
        (unless (typep definition 'shader-projective-map-definition)
          (error 'shader-language-error
                 :form form :reason :unsupported-sampling-projection
                 :details (class-name (class-of definition))))
        (make-instance
         'shader-map-projection
         :application application
         :type (shader-projective-map-sample-type definition)
         :quantity-layout
         (shader-projective-map-sample-quantity-layout definition)
         :source-form form)))))

(defmethod parse-shader-operator-call
    ((operator (eql 'interpret)) form environment)
  (declare (ignore operator))
  (destructuring-bind
      (name operand-form &key quantity dimension unit affine-p character) form
    (declare (ignore name))
    (unless (or quantity dimension unit affine-p character)
      (error 'shader-language-error
             :form form :reason :missing-quantity-interpretation))
    (let* ((operand (parse-shader-expression operand-form environment))
           (type (shader-expression-type operand))
           (interpretation
             (parse-declaration-quantity-specification
              quantity dimension unit (declared-character affine-p character)
              type form)))
      (with-shader-quantity-errors
          (form :invalid-quantity-interpretation)
        (math:interpret-quantity-specification
         (and (shader-expression-quantity-checked-p operand)
              (shader-expression-quantity-specification operand))
         interpretation))
      (make-instance 'shader-interpretation
                     :operand operand
                     :type type
                     :quantity-specification interpretation
                     :source-form form))))

(defmethod parse-shader-operator-call
    ((operator (eql 'convert-unit)) form environment)
  (declare (ignore operator))
  (destructuring-bind
      (name operand-form &key (unit nil unit-supplied-p)) form
    (declare (ignore name))
    (unless unit-supplied-p
      (error 'shader-language-error
             :form form :reason :missing-target-unit))
    (let* ((operand (parse-shader-expression operand-form environment))
           (source (and (shader-expression-quantity-checked-p operand)
                        (shader-expression-quantity-specification operand))))
      (unless source
        (error 'shader-language-error
               :form form :reason :unit-conversion-requires-quantity))
      (with-shader-quantity-errors (form :invalid-unit-conversion)
        (multiple-value-bind (target factor)
            (math:convert-quantity-specification-unit source unit)
          (make-instance
           'shader-unit-conversion
           :operand operand
           :factor factor
           :type (shader-expression-type operand)
           :quantity-specification target
           :source-form form))))))

(defun shader-constant-expression-p (expression)
  (typecase expression
    (shader-literal t)
    (shader-call
     (and (member (shader-call-operator expression) '(vec2 vec3 vec4))
          (every #'shader-constant-expression-p
                 (shader-call-operands expression))))
    (t nil)))

(defun shader-constant-uint-value (expression)
  "Return a compile-time unsigned value and true, or NIL and false."
  (typecase expression
    (shader-reference
     (let ((target (shader-reference-target expression)))
       (if (typep target 'shader-binding)
           (shader-constant-uint-value (shader-binding-expression target))
           (values nil nil))))
    (shader-call
     (if (and (eq 'uint (shader-call-operator expression))
              (= 1 (length (shader-call-operands expression)))
              (typep (first (shader-call-operands expression))
                     'shader-literal))
         (values (truncate
                  (shader-literal-value
                   (first (shader-call-operands expression))))
                 t)
         (values nil nil)))
    (t (values nil nil))))

(defun parse-raw-quantity-boundary
    (class form environment &key constant-only-p)
  (destructuring-bind
      (name operand-form &key quantity dimension unit affine-p character) form
    (declare (ignore name))
    (unless (or quantity dimension unit affine-p character)
      (error 'shader-language-error
             :form form :reason :missing-quantity-interpretation))
    (let* ((operand (parse-shader-expression operand-form environment))
           (type (shader-expression-type operand)))
      (when (or (shader-expression-quantity-checked-p operand)
                (shader-expression-quantity-layout operand))
        (error 'shader-language-error
               :form form :reason :quantity-already-has-semantics
               :details (shader-expression-form operand)))
      (when (and constant-only-p
                 (not (shader-constant-expression-p operand)))
        (error 'shader-language-error
               :form form :reason :quantity-requires-literal-construction
               :details (shader-expression-form operand)))
      (make-instance
       class
       :operand operand
       :type type
       :quantity-specification
       (parse-declaration-quantity-specification
        quantity dimension unit (declared-character affine-p character)
        type form)
       :source-form form))))

(defmethod parse-shader-operator-call
    ((operator (eql 'quantity)) form environment)
  (declare (ignore operator))
  (parse-raw-quantity-boundary
   'shader-quantity-construction form environment :constant-only-p t))

(defmethod parse-shader-operator-call
    ((operator (eql 'assume-quantity)) form environment)
  (declare (ignore operator))
  (parse-raw-quantity-boundary
   'shader-quantity-assumption form environment))

(defmethod parse-shader-operator-call
    ((operator (eql 'representation)) form environment)
  (declare (ignore operator))
  (unless (= (length form) 2)
    (error 'shader-language-error
           :form form :reason :representation-arity))
  (let ((operand (parse-shader-expression (second form) environment)))
    (unless (shader-expression-quantity-checked-p operand)
      (error 'shader-language-error
             :form form :reason :representation-requires-quantity
             :details (shader-expression-form operand)))
    (make-instance 'shader-representation
                   :operand operand
                   :type (shader-expression-type operand)
                   :source-form form)))

(defun parse-interface-declaration (form direction)
  (destructuring-bind
      (name type &key location built-in quantity dimension unit affine-p
                       character components)
      form
    (unless (or (and (typep location '(integer 0 *)) (null built-in))
                (and (null location) built-in))
      (error 'shader-language-error
             :form form :reason :invalid-interface-decoration
             :details (list :location location :built-in built-in)))
    (let* ((resolved-type (find-shader-type type form))
           (specification
             (parse-declaration-quantity-specification
              quantity dimension unit (declared-character affine-p character)
              resolved-type form)))
      (make-instance 'shader-interface-variable
                     :name name
                     :type resolved-type
                     :quantity-specification specification
                     :quantity-layout
                     (parse-declaration-quantity-layout
                      components resolved-type form specification)
                     :direction direction
                     :location location
                     :built-in built-in
                     :source-form form))))

(defun parse-resource-declaration (form)
  (destructuring-bind
      (name type &key (set 0) binding members element
                       sample-quantity sample-dimension sample-unit
                       sample-affine-p sample-character sample-components
                       sample-transfer)
      form
    (unless (and (typep set '(integer 0 *))
                 (typep binding '(integer 0 *)))
      (error 'shader-language-error
             :form form :reason :invalid-resource-location
             :details (list set binding)))
    (when (and element (not (shader-symbol= type :storage-buffer)))
      (error 'shader-language-error
             :form form :reason :element-on-non-storage-buffer))
    (cond
      ((shader-symbol= type :storage-buffer)
       (let ((element-type (and element (find-shader-type element form))))
         (unless (and element-type
                      (shader-type-component-count element-type)
                      (member (shader-type-component-count element-type)
                              '(1 2 4)))
           (error 'shader-language-error
                  :form form :reason :invalid-storage-buffer-element
                  :details element))
         (when members
           (error 'shader-language-error
                  :form form :reason :members-on-opaque-resource))
         (make-instance 'shader-storage-buffer
                        :name name
                        :type (find-shader-type :storage-buffer form)
                        :element-type element-type
                        :descriptor-set set :binding binding
                        :source-form form)))
      ((shader-symbol= type :uniform-block)
       (let ((block
               (make-instance 'shader-uniform-block
                              :name name
                              :type (find-shader-type :uniform-block form)
                              :descriptor-set set :binding binding
                              :source-form form)))
         (unless (and (listp members) members)
           (error 'shader-language-error
                  :form form :reason :empty-uniform-block))
         (setf (shader-uniform-block-members block)
               (loop for member-form in members
                     for index from 0
                     collect
                     (destructuring-bind
                         (member-name member-type
                          &key quantity dimension unit affine-p character
                            components)
                         member-form
                       (let ((resolved-type
                               (find-shader-type member-type member-form)))
                         ;; This intentionally models the renderer's current
                         ;; camera ABI: an aggregate of aligned vec4 lanes.
                         ;; Do not imply general std140 packing until the
                         ;; language owns that calculation explicitly.
                         (unless (eq resolved-type (find-shader-type :vec4))
                           (error 'shader-language-error
                                  :form member-form
                                  :reason :unsupported-uniform-member-type
                                  :details member-type))
                         (let ((specification
                                 (parse-declaration-quantity-specification
                                  quantity dimension unit
                                  (declared-character affine-p character)
                                  resolved-type member-form)))
                           (make-instance
                            'shader-uniform-member
                            :name member-name :type resolved-type
                            :quantity-specification specification
                            :quantity-layout
                            (parse-declaration-quantity-layout
                             components resolved-type member-form
                             specification)
                            :block block :index index :offset (* index 16)
                            :source-form member-form))))))
         block))
      (t
       (let* ((resolved-type (find-shader-type type form))
              (sample-type
                (and (shader-type-sample-result-type resolved-type)
                     (find-shader-type
                      (shader-type-sample-result-type resolved-type)
                      form)))
              (sample-specification
                (and sample-type
                     (parse-declaration-quantity-specification
                      sample-quantity sample-dimension sample-unit
                      (declared-character sample-affine-p sample-character)
                      sample-type form))))
         (unless (and (shader-type-opaque-kind resolved-type)
                      (not (eq (shader-type-opaque-kind resolved-type)
                               :uniform-block)))
           (error 'shader-language-error
                  :form form :reason :non-resource-type :details type))
         (when members
           (error 'shader-language-error
                  :form form :reason :members-on-opaque-resource))
         (unless (member sample-transfer '(nil :identity :srgb-to-linear))
           (error 'shader-language-error
                  :form form :reason :invalid-sample-transfer
                  :details sample-transfer))
         (when (and (or sample-quantity sample-dimension sample-unit
                        sample-affine-p sample-character sample-components
                        sample-transfer)
                    (null sample-type))
           (error 'shader-language-error
                  :form form :reason :sample-semantics-on-non-texture))
         (make-instance 'shader-resource
                        :name name :type resolved-type
                        :sample-quantity-specification sample-specification
                        :sample-quantity-layout
                        (and sample-type
                             (parse-declaration-quantity-layout
                              sample-components sample-type form
                              sample-specification))
                        :sample-transfer sample-transfer
                        :descriptor-set set :binding binding
                        :source-form form))))))

(defun parse-output-assignment (form environment outputs)
  (unless (and (consp form) (eq (first form) 'set-output)
               (= (length form) 3))
    (error 'shader-language-error
           :form form :reason :expected-output-assignment))
  (let* ((output-name (second form))
         (output (shader-environment-value output-name
                                           (mapcar (lambda (item)
                                                     (cons (shader-object-name item)
                                                           item))
                                                   outputs)
                                           form))
         (value (parse-shader-expression (third form) environment)))
    (unless (shader-type= (shader-declaration-type output)
                          (shader-expression-type value))
      (error 'shader-language-error
             :form form :reason :output-type-mismatch
             :details (list (shader-type-name (shader-declaration-type output))
                            (shader-type-name (shader-expression-type value)))))
    (let ((expected
            (shader-declaration-quantity-specification output))
          (actual
            (shader-expression-quantity-specification value))
          (expected-layout
            (shader-declaration-quantity-layout output))
          (actual-layout
            (shader-expression-quantity-layout value)))
      (when (and expected
                 (or (null actual)
                     (not (math:quantity-specification= expected actual))))
        (error 'shader-language-error
               :form form :reason :output-quantity-mismatch
               :details (list expected actual)))
      (when (and expected-layout
                 (or (null actual-layout)
                     (not (math:quantity-layout=
                           expected-layout actual-layout))))
        (error 'shader-language-error
               :form form :reason :output-quantity-layout-mismatch
               :details (list expected-layout actual-layout))))
    (make-instance 'shader-output-assignment
                   :output output :value value :source-form form)))

(defclass shader-parsing-context ()
  ((stage
    :initarg :stage
    :reader shader-parsing-context-stage)
   (outputs
    :initarg :outputs
    :initform nil
    :reader shader-parsing-context-outputs)
   (task-payload
    :initarg :task-payload
    :initform nil
    :reader shader-parsing-context-task-payload)
   (mesh-output
    :initarg :mesh-output
    :initform nil
    :reader shader-parsing-context-mesh-output)))

(defun check-shader-store-value
    (declaration value form type-reason quantity-reason layout-reason)
  (unless (shader-type= (shader-declaration-type declaration)
                        (shader-expression-type value))
    (error 'shader-language-error
           :form form :reason type-reason
           :details (list (shader-type-name (shader-declaration-type declaration))
                          (shader-type-name (shader-expression-type value)))))
  (let ((expected (shader-declaration-quantity-specification declaration))
        (actual (shader-expression-quantity-specification value))
        (expected-layout (shader-declaration-quantity-layout declaration))
        (actual-layout (shader-expression-quantity-layout value)))
    (when (and expected
               (or (null actual)
                   (not (math:quantity-specification= expected actual))))
      (error 'shader-language-error
             :form form :reason quantity-reason
             :details (list expected actual)))
    (when (and expected-layout
               (or (null actual-layout)
                   (not (math:quantity-layout=
                         expected-layout actual-layout))))
      (error 'shader-language-error
             :form form :reason layout-reason
             :details (list expected-layout actual-layout))))
  value)

(defgeneric parse-shader-statement
    (operator stage form environment context)
  (:documentation
   "Parse one stage effect, dispatching on its source operator and stage."))

(defmethod parse-shader-statement
    (operator stage form environment context)
  (declare (ignore environment context))
  (error 'shader-language-error
         :form form :reason :invalid-statement-for-stage
         :details (list operator stage)))

(defmethod parse-shader-statement
    ((operator (eql 'set-output)) stage form environment context)
  (declare (ignore operator))
  (unless (member stage '(:vertex :fragment :compute))
    (error 'shader-language-error
           :form form :reason :invalid-statement-for-stage
           :details (list 'set-output stage)))
  (parse-output-assignment
   form environment (shader-parsing-context-outputs context)))

(defun parse-complete-mesh-output-values
    (forms declarations environment source-form)
  (let ((seen nil)
        (parsed nil))
    (dolist (form forms)
      (unless (and (consp form) (= (length form) 2) (symbolp (first form)))
        (error 'shader-language-error
               :form form :reason :invalid-mesh-output-value))
      (let* ((name (first form))
             (declaration
               (find name declarations :key #'shader-object-name
                                       :test #'shader-symbol=)))
        (unless declaration
          (error 'shader-language-error
                 :form form :reason :unknown-mesh-output :details name))
        (when (member declaration seen)
          (error 'shader-language-error
                 :form form :reason :duplicate-mesh-output :details name))
        (let ((value (parse-shader-expression (second form) environment)))
          (check-shader-store-value
           declaration value form
           :mesh-output-type-mismatch
           :mesh-output-quantity-mismatch
           :mesh-output-quantity-layout-mismatch)
          (push declaration seen)
          (push (cons declaration value) parsed))))
    (let ((missing (set-difference declarations seen)))
      (when missing
        (error 'shader-language-error
               :form source-form :reason :missing-mesh-output-values
               :details (mapcar #'shader-object-name missing))))
    (loop for declaration in declarations
          collect (assoc declaration parsed))))

(defmethod parse-shader-statement
    ((operator (eql 'set-mesh-output-counts)) (stage (eql :mesh))
     form environment context)
  (declare (ignore operator context))
  (unless (= (length form) 3)
    (error 'shader-language-error
           :form form :reason :mesh-output-counts-arity))
  (let ((vertex-count (parse-shader-expression (second form) environment))
        (primitive-count (parse-shader-expression (third form) environment)))
    (unless (and (shader-uint-type-p (shader-expression-type vertex-count))
                 (shader-uint-type-p (shader-expression-type primitive-count)))
      (error 'shader-language-error
             :form form :reason :mesh-output-count-type))
    (make-instance 'shader-mesh-output-counts
                   :vertex-count vertex-count :primitive-count primitive-count
                   :source-form form)))

(defmethod parse-shader-statement
    ((operator (eql 'set-mesh-vertex)) (stage (eql :mesh))
     form environment context)
  (declare (ignore operator))
  (unless (>= (length form) 3)
    (error 'shader-language-error
           :form form :reason :mesh-vertex-arity))
  (let* ((index (parse-shader-expression (second form) environment))
         (mesh-output (shader-parsing-context-mesh-output context)))
    (unless (shader-uint-type-p (shader-expression-type index))
      (error 'shader-language-error
             :form form :reason :mesh-output-index-type))
    (make-instance
     'shader-mesh-vertex-store
     :index index
     :values
     (parse-complete-mesh-output-values
      (cddr form) (shader-mesh-output-vertex-outputs mesh-output)
      environment form)
     :source-form form)))

(defun mesh-topology-index-type (topology)
  (ecase topology
    (:points :uint)
    (:lines :uvec2)
    (:triangles :uvec3)))

(defmethod parse-shader-statement
    ((operator (eql 'set-mesh-primitive)) (stage (eql :mesh))
     form environment context)
  (declare (ignore operator))
  (unless (>= (length form) 3)
    (error 'shader-language-error
           :form form :reason :mesh-primitive-arity))
  (let* ((index (parse-shader-expression (second form) environment))
         (indices (parse-shader-expression (third form) environment))
         (mesh-output (shader-parsing-context-mesh-output context))
         (expected-index-type
           (mesh-topology-index-type
            (shader-mesh-output-topology mesh-output))))
    (unless (shader-uint-type-p (shader-expression-type index))
      (error 'shader-language-error
             :form form :reason :mesh-output-index-type))
    (unless (shader-type= expected-index-type
                          (shader-expression-type indices))
      (error 'shader-language-error
             :form form :reason :mesh-primitive-indices-type
             :details (list expected-index-type
                            (shader-type-name
                             (shader-expression-type indices)))))
    (make-instance
     'shader-mesh-primitive-store
     :index index :indices indices
     :values
     (parse-complete-mesh-output-values
      (cdddr form) (shader-mesh-output-primitive-outputs mesh-output)
      environment form)
     :source-form form)))

(defun shader-payload-field-named (name payload source-form)
  (or (and payload
           (find name (shader-task-payload-fields payload)
                      :key #'shader-object-name :test #'shader-symbol=))
      (error 'shader-language-error
             :form source-form :reason :unknown-payload-field :details name)))

(defmethod parse-shader-statement
    ((operator (eql 'set-payload)) (stage (eql :task))
     form environment context)
  (declare (ignore operator))
  (unless (= (length form) 3)
    (error 'shader-language-error
           :form form :reason :set-payload-arity))
  (let* ((field
           (shader-payload-field-named
            (second form) (shader-parsing-context-task-payload context) form))
         (value (parse-shader-expression (third form) environment)))
    (when (shader-task-payload-field-element-count field)
      (error 'shader-language-error
             :form form :reason :payload-array-requires-element
             :details (shader-object-name field)))
    (check-shader-store-value
     field value form
     :payload-type-mismatch :payload-quantity-mismatch
     :payload-quantity-layout-mismatch)
    (make-instance 'shader-task-payload-store
                   :field field :value value :source-form form)))

(defmethod parse-shader-statement
    ((operator (eql 'set-payload-element)) (stage (eql :task))
     form environment context)
  (declare (ignore operator))
  (unless (= (length form) 4)
    (error 'shader-language-error
           :form form :reason :set-payload-element-arity))
  (let* ((field
           (shader-payload-field-named
            (second form) (shader-parsing-context-task-payload context) form))
         (index (parse-shader-expression (third form) environment))
         (value (parse-shader-expression (fourth form) environment)))
    (unless (shader-task-payload-field-element-count field)
      (error 'shader-language-error
             :form form :reason :not-payload-array
             :details (shader-object-name field)))
    (unless (shader-uint-type-p (shader-expression-type index))
      (error 'shader-language-error
             :form form :reason :payload-index-type))
    (multiple-value-bind (constant-index constant-p)
        (shader-constant-uint-value index)
      (when (and constant-p
                 (>= constant-index
                     (shader-task-payload-field-element-count field)))
        (error 'shader-language-error
               :form form :reason :payload-index-out-of-bounds
               :details
               (list constant-index
                     (shader-task-payload-field-element-count field)))))
    (check-shader-store-value
     field value form
     :payload-type-mismatch :payload-quantity-mismatch
     :payload-quantity-layout-mismatch)
    (make-instance 'shader-task-payload-store
                   :field field :index index :value value :source-form form)))

(defmethod parse-shader-statement
    ((operator (eql 'emit-mesh-workgroups)) (stage (eql :task))
     form environment context)
  (declare (ignore operator context))
  (unless (= (length form) 2)
    (error 'shader-language-error
           :form form :reason :emit-mesh-workgroups-arity))
  (let ((workgroups (parse-shader-expression (second form) environment)))
    (unless (shader-type= :uvec3 (shader-expression-type workgroups))
      (error 'shader-language-error
             :form form :reason :mesh-workgroups-type
             :details (shader-type-name
                       (shader-expression-type workgroups))))
    (make-instance 'shader-emit-mesh-workgroups
                   :workgroups workgroups :source-form form)))

(defmethod parse-shader-statement
    ((operator (eql 'when)) stage form environment context)
  (declare (ignore operator))
  (unless (>= (length form) 3)
    (error 'shader-language-error
           :form form :reason :shader-when-arity))
  (let ((condition (parse-shader-expression (second form) environment)))
    (unless (shader-type= :bool (shader-expression-type condition))
      (error 'shader-language-error
             :form form :reason :shader-when-condition-type))
    (make-instance
     'shader-conditional-statement
     :condition condition
     :statements
     (mapcar (lambda (statement)
               (parse-shader-statement-form
                statement stage environment context))
             (cddr form))
     :source-form form)))

(defun parse-shader-statement-form (form stage environment context)
  (unless (and (consp form) (symbolp (first form)))
    (error 'shader-language-error
           :form form :reason :invalid-shader-statement))
  (parse-shader-statement (first form) stage form environment context))

(defun parse-shader-body (body environment context)
  (unless (= (length body) 1)
    (error 'shader-language-error
           :form body :reason :expected-single-shader-body))
  (let ((form (first body)))
    ;; LET* is CL:LET* by identity: the language is a compiled subset of CL,
    ;; and its binding form is the standard symbol, not a look-alike.
    (if (and (consp form) (eq (first form) 'let*))
        (destructuring-bind (operator raw-bindings &rest statements) form
          (declare (ignore operator))
          (let ((bindings nil)
                (lexical-environment environment))
            (dolist (raw-binding raw-bindings)
              (unless (and (consp raw-binding) (= (length raw-binding) 2)
                           (symbolp (first raw-binding)))
                (error 'shader-language-error
                       :form raw-binding :reason :invalid-binding))
              (let* ((name (first raw-binding))
                     (expression
                       (parse-shader-expression (second raw-binding)
                                                lexical-environment))
                     (binding
                       (make-instance 'shader-binding
                                      :name name :expression expression
                                      :source-form raw-binding)))
                (setf (shader-expression-name expression) name)
                (push binding bindings)
                (push (cons name binding) lexical-environment)))
            (values (nreverse bindings)
                    (mapcar (lambda (statement)
                              (parse-shader-statement-form
                               statement
                               (shader-parsing-context-stage context)
                               lexical-environment context))
                            statements))))
        (values nil
                (list
                 (parse-shader-statement-form
                  form (shader-parsing-context-stage context)
                  environment context))))))

(defun collect-shader-bindings (bindings statements)
  "Hoist inline-function lexical bindings in dependency order.

The bindings remain typed objects owned by each SHADER-FUNCTION-CALL.  A flat
entry-block order lets both SPIR-V and direct MSL lowering name their values
without turning function definitions back into source-form substitution."
  (let ((seen (make-hash-table :test #'eq))
        (ordered nil))
    (labels ((add-binding (binding)
               (unless (gethash binding seen)
                 (visit (shader-binding-expression binding))
                 (setf (gethash binding seen) t)
                 (push binding ordered)))
             (visit (expression)
               (typecase expression
                 (shader-counted-fold
                  ;; COUNT and INITIAL belong to the preheader.  UPDATE owns
                  ;; its lexical work inside the loop body.
                  (visit (lang:arithmetic-counted-fold-count expression))
                  (visit (lang:arithmetic-counted-fold-initial expression)))
                 (shader-function-call
                  (mapc #'visit (shader-function-call-arguments expression))
                  (dolist (binding (shader-function-call-bindings expression))
                    (if (typep binding 'shader-function-parameter-binding)
                        (visit (shader-binding-expression binding))
                        (add-binding binding)))
                  (visit (shader-function-call-result expression)))
                 (t
                  (mapc #'visit (shader-expression-children expression))))))
      (dolist (binding bindings)
        (add-binding binding))
      (dolist (statement statements)
        (mapc #'visit (shader-statement-expressions statement))))
    (nreverse ordered)))

(defun parse-workgroup-size (form options)
  (unless (and (listp form) (= (length form) 3)
               (every (lambda (value) (typep value '(integer 1 *))) form))
    (error 'shader-language-error
           :form options :reason :invalid-workgroup-size :details form))
  form)

(defun parse-mesh-output-declaration (form)
  (unless (listp form)
    (error 'shader-language-error
           :form form :reason :invalid-mesh-output))
  (let ((topology (getf form :topology))
        (max-vertices (getf form :max-vertices))
        (max-primitives (getf form :max-primitives)))
    (unless (member topology '(:points :lines :triangles))
      (error 'shader-language-error
             :form form :reason :invalid-mesh-topology :details topology))
    (unless (and (typep max-vertices '(integer 1 *))
                 (typep max-primitives '(integer 1 *)))
      (error 'shader-language-error
             :form form :reason :invalid-mesh-output-limits
             :details (list max-vertices max-primitives)))
    (let ((vertex-outputs
            (mapcar (lambda (declaration)
                      (parse-interface-declaration declaration :output))
                    (getf form :vertex)))
          (primitive-outputs
            (mapcar (lambda (declaration)
                      (parse-interface-declaration declaration :output))
                    (getf form :primitive))))
      (unless vertex-outputs
        (error 'shader-language-error
               :form form :reason :empty-mesh-vertex-output))
      (unless (= 1 (count :position vertex-outputs
                          :key #'shader-interface-built-in))
        (error 'shader-language-error
               :form form :reason :mesh-position-output-count))
      (make-instance 'shader-mesh-output
                     :topology topology
                     :max-vertices max-vertices
                     :max-primitives max-primitives
                     :vertex-outputs vertex-outputs
                     :primitive-outputs primitive-outputs
                     :source-form form))))

(defun workgroup-built-in-type (built-in)
  (case built-in
    (:local-invocation-index :uint)
    ((:local-invocation-id :workgroup-id :num-workgroups :workgroup-size)
     :uvec3)
    (otherwise nil)))

(defun validate-workgroup-inputs (inputs options)
  (let ((seen nil))
    (dolist (input inputs)
      (let* ((built-in (shader-interface-built-in input))
             (expected (workgroup-built-in-type built-in)))
        (unless expected
          (error 'shader-language-error
                 :form (shader-object-source-form input)
                 :reason :invalid-workgroup-input :details built-in))
        (unless (shader-type= expected (shader-declaration-type input))
          (error 'shader-language-error
                 :form (shader-object-source-form input)
                 :reason :workgroup-built-in-type
                 :details (list built-in expected)))
        (when (member built-in seen)
          (error 'shader-language-error
                 :form options :reason :duplicate-workgroup-built-in
                 :details built-in))
        (push built-in seen)))
    (unless (member :local-invocation-index seen)
      (error 'shader-language-error
             :form options :reason :missing-local-invocation-index))))

(defun statement-tree-occurrences (statements class)
  (loop for statement in statements
        append
        (append (when (typep statement class) (list statement))
                (when (typep statement 'shader-conditional-statement)
                  (statement-tree-occurrences
                   (shader-conditional-statement-statements statement)
                   class)))))

(defun validate-stage-statements
    (stage statements source-form &optional mesh-output)
  (ecase stage
    ((:vertex :fragment :compute) nil)
    (:mesh
     (let ((counts
             (statement-tree-occurrences
              statements 'shader-mesh-output-counts))
           (stores
             (append
              (statement-tree-occurrences statements 'shader-mesh-vertex-store)
              (statement-tree-occurrences
               statements 'shader-mesh-primitive-store))))
       (unless (= (length counts) 1)
         (error 'shader-language-error
                :form source-form :reason :mesh-output-counts-count
                :details (length counts)))
       (unless (eq (first statements) (first counts))
         (error 'shader-language-error
                :form (shader-statement-source-form (first counts))
                :reason :mesh-output-counts-must-be-first))
       (unless (every #'shader-expression-workgroup-uniform-p
                      (shader-statement-expressions (first counts)))
         (error 'shader-language-error
                :form (shader-statement-source-form (first counts))
                :reason :mesh-output-counts-not-uniform))
       (loop for expression
               in (shader-statement-expressions (first counts))
             for limit in (list (shader-mesh-output-max-vertices mesh-output)
                                (shader-mesh-output-max-primitives mesh-output))
             do (multiple-value-bind (constant constant-p)
                    (shader-constant-uint-value expression)
                  (when (and constant-p (> constant limit))
                    (error 'shader-language-error
                           :form (shader-statement-source-form (first counts))
                           :reason :mesh-output-count-exceeds-limit
                           :details (list constant limit)))))
       (unless stores
         (error 'shader-language-error
                :form source-form :reason :empty-mesh-output-body))))
    (:task
     (let ((emissions
             (statement-tree-occurrences
              statements 'shader-emit-mesh-workgroups)))
       (unless (= (length emissions) 1)
         (error 'shader-language-error
                :form source-form :reason :mesh-workgroups-emission-count
                :details (length emissions)))
       (unless (eq (car (last statements)) (first emissions))
         (error 'shader-language-error
                :form (shader-statement-source-form (first emissions))
                :reason :mesh-workgroups-emission-must-be-last))
       (unless (shader-expression-workgroup-uniform-p
                (shader-emit-mesh-workgroups-counts (first emissions)))
         (error 'shader-language-error
                :form (shader-statement-source-form (first emissions))
                :reason :mesh-workgroups-not-uniform))))))

(defun parse-shader-specification (name options body)
  (let* ((stage (getf options :stage))
         (expanded-body (mapcar #'expand-shader-source-form body))
         (inputs (mapcar (lambda (form)
                           (parse-interface-declaration form :input))
                         (getf options :inputs)))
         (outputs (mapcar (lambda (form)
                            (parse-interface-declaration form :output))
                          (getf options :outputs)))
         (resources (mapcar #'parse-resource-declaration
                            (getf options :resources)))
         (workgroup-size
           (and (member stage '(:task :mesh))
                (parse-workgroup-size (getf options :workgroup-size) options)))
         (payload-name (getf options :payload))
         (payload
           (and payload-name
                (or (task-payload-definition-for payload-name)
                    (error 'shader-language-error
                           :form options :reason :undefined-task-payload
                           :details payload-name))))
         (mesh-output
           (and (eq stage :mesh)
                (parse-mesh-output-declaration (getf options :mesh-output))))
         (environment-items
           (append inputs
                   (loop for resource in resources
                         if (typep resource 'shader-uniform-block)
                           append (shader-uniform-block-members resource)
                         else collect resource)
                   (and payload (shader-task-payload-fields payload))))
         (environment
           (mapcar (lambda (item) (cons (shader-object-name item) item))
                   environment-items)))
    (unless (member stage '(:vertex :fragment :compute :task :mesh))
      (error 'shader-language-error
             :form options :reason :invalid-stage :details stage))
    (when (member stage '(:task :mesh))
      (validate-workgroup-inputs inputs options)
      (when outputs
        (error 'shader-language-error
               :form options :reason :ordinary-outputs-on-workgroup-stage)))
    (when (and (eq stage :task) (getf options :mesh-output))
      (error 'shader-language-error
             :form options :reason :mesh-output-on-task-stage))
    (when (and (not (member stage '(:task :mesh)))
               (or payload-name (getf options :mesh-output)))
      (error 'shader-language-error
             :form options :reason :workgroup-contract-on-ordinary-stage))
    (let ((*shader-function-call-counter* 0)
          (*shader-function-call-stack* nil))
      (let ((context
              (make-instance 'shader-parsing-context
                             :stage stage :outputs outputs
                             :task-payload payload :mesh-output mesh-output)))
        (multiple-value-bind (bindings statements)
            (parse-shader-body expanded-body environment context)
          (validate-stage-statements
           stage statements (list* 'define-shader name options body)
           mesh-output)
          (make-instance
           'shader-specification
           :name name :stage stage
           :inputs inputs :outputs outputs :resources resources
           :workgroup-size workgroup-size
           :task-payload payload :mesh-output mesh-output
           :bindings (collect-shader-bindings bindings statements)
           :statements statements
           :source-form (list* 'define-shader name options body)))))))

(defmacro define-shader (name options &body body)
  "Define NAME as a function returning a durable, inspectable shader graph."
  (let* ((package (or (symbol-package name) *package*))
         (variable (intern (format nil "*~A*" (symbol-name name)) package)))
    `(progn
       (defparameter ,variable
         (parse-shader-specification ',name ',options ',body))
       (defun ,name () ,variable))))

(defmacro define-live-shader (name options &body body)
  "Define NAME as a function that reparses its shader source on every call.

Where DEFINE-SHADER parses once at load, this parses each time it is asked,
so live named values (SHADER-SOURCE-VALUE) folded into the body are read
afresh -- and noted in *SHADER-SOURCE-VALUE-REFERENCES* -- by every pipeline
build, exactly as a DEFINE-SHADER-METHOD's body is.  Parsing at load still
happens once, to fail early on a broken source."
  `(progn
     (parse-shader-specification ',name ',options ',body)
     (defun ,name ()
       (parse-shader-specification ',name ',options ',body))))

;;; Live definitions ---------------------------------------------------------

(defgeneric shader-specification-for (role stage)
  (:documentation
   "Return the current durable shader specification for ROLE and STAGE."))

(defmacro define-shader-method
    (generic-function name specialized-lambda-list options &body body)
  "Define a shader-producing method with ordinary DEFMETHOD identity.

Calling the method reparses its small source form so changes to source-level
abstractions participate in live rebuilding.  Method replacement remains the
role/stage identity watched by the MOP; abstraction revisions are tracked
separately by live artifacts."
  `(defmethod ,generic-function ,specialized-lambda-list
     (parse-shader-specification ',name ',options ',body)))

(defclass shader-definition-dependent ()
  ((generic-function
    :initarg :generic-function
    :reader shader-definition-dependent-generic-function)
   (arguments
    :initarg :arguments
    :reader shader-definition-dependent-arguments)
   (lock
    :initform (sb-thread:make-mutex :name "luv shader definition dependent")
    :reader shader-definition-dependent-lock)
   (revision
    :initform 0
    :accessor shader-definition-dependent-revision)
   (attempted-revision
    :initform 0
    :accessor shader-definition-dependent-attempted-revision)
   (last-event
    :initform nil
    :accessor shader-definition-dependent-last-event)
   (subscribed-p
    :initform nil
    :accessor shader-definition-dependent-subscribed-p))
  (:documentation
   "A narrow MOP subscription for one generic-function argument tuple.

UPDATE-DEPENDENT only records a monotonically increasing revision.  Consumers
perform compilation, GPU work, and calls back into the generic function after
the method mutation has completed and outside the generic function's lock."))

(defun shader-method-specializer-accepts-p (specializer argument)
  (if (typep specializer 'closer-mop:eql-specializer)
      (eql argument (closer-mop:eql-specializer-object specializer))
      (typep argument specializer)))

(defun shader-method-accepts-arguments-p (method arguments)
  (let ((specializers (closer-mop:method-specializers method)))
    (and (= (length specializers) (length arguments))
         (every #'shader-method-specializer-accepts-p
                specializers arguments))))

(defmethod closer-mop:update-dependent
    ((generic-function standard-generic-function)
     (dependent shader-definition-dependent)
     &rest event)
  (declare (ignore generic-function))
  ;; SBCL/Closer-MOP may also announce generic-function reinitialization with
  ;; no event arguments.  ADD-METHOD and REMOVE-METHOD contain the affected
  ;; method and are sufficient; replacement naturally coalesces to one pending
  ;; revision before the next consumer turn.
  (when (and (= (length event) 2)
             (member (first event)
                     '(add-method remove-method))
             (shader-method-accepts-arguments-p
              (second event)
              (shader-definition-dependent-arguments dependent)))
    (sb-thread:with-mutex ((shader-definition-dependent-lock dependent))
      (incf (shader-definition-dependent-revision dependent))
      (setf (shader-definition-dependent-last-event dependent) event)))
  nil)

(defun make-shader-definition-dependent (generic-function arguments)
  "Subscribe a revision source to GENERIC-FUNCTION changes for ARGUMENTS."
  (check-type generic-function standard-generic-function)
  (let ((dependent
          (make-instance 'shader-definition-dependent
                         :generic-function generic-function
                         :arguments (copy-list arguments))))
    (closer-mop:add-dependent generic-function dependent)
    (setf (shader-definition-dependent-subscribed-p dependent) t)
    dependent))

(defun shader-definition-change-pending-p (dependent)
  (sb-thread:with-mutex ((shader-definition-dependent-lock dependent))
    (> (shader-definition-dependent-revision dependent)
       (shader-definition-dependent-attempted-revision dependent))))

(defun shader-definition-change-snapshot (dependent)
  "Return the current definition revision and its most recent MOP event."
  (sb-thread:with-mutex ((shader-definition-dependent-lock dependent))
    (values (shader-definition-dependent-revision dependent)
            (copy-list (shader-definition-dependent-last-event dependent)))))

(defun acknowledge-shader-definition-change (dependent revision)
  "Record that the consumer finished attempting REVISION.

A newer concurrent notification remains pending."
  (sb-thread:with-mutex ((shader-definition-dependent-lock dependent))
    (setf (shader-definition-dependent-attempted-revision dependent)
          (max (shader-definition-dependent-attempted-revision dependent)
               (min revision
                    (shader-definition-dependent-revision dependent)))))
  dependent)

(defun release-shader-definition-dependent (dependent)
  "Remove DEPENDENT from its generic function.  This operation is idempotent."
  (when (shader-definition-dependent-subscribed-p dependent)
    (closer-mop:remove-dependent
     (shader-definition-dependent-generic-function dependent)
     dependent)
    (setf (shader-definition-dependent-subscribed-p dependent) nil))
  nil)

;;; Lowering -----------------------------------------------------------------

(defclass shader-lowering ()
  ((specification
    :initarg :specification
    :reader shader-lowering-specification)
   (module
    :initarg :module
    :reader shader-lowering-module)
   (expression-instructions
    :initarg :expression-instructions
    :reader shader-lowering-expression-instructions)
   (instruction-expressions
    :initarg :instruction-expressions
    :reader shader-lowering-instruction-expressions)
   (diagnostics
    :initarg :diagnostics
    :initform nil
   :reader shader-lowering-diagnostics)))

(defgeneric lower-shader-specification (target specification)
  (:documentation
   "Lower SPECIFICATION for TARGET without changing its source graph.

TARGET participates in ordinary CLOS dispatch so each backend can own its
structured product and source provenance.  #JDLQPN"))

(defclass shader-lowering-context ()
  ((type-ids :initform (make-hash-table :test #'eq) :accessor context-type-ids)
   (pointer-ids :initform (make-hash-table :test #'equal)
                :accessor context-pointer-ids)
   (constant-ids :initform (make-hash-table :test #'equal)
                 :accessor context-constant-ids)
   (variable-ids :initform (make-hash-table :test #'eq)
                 :accessor context-variable-ids)
   (direct-values :initform (make-hash-table :test #'eq)
                  :accessor context-direct-values)
   (array-type-ids :initform (make-hash-table :test #'equal)
                   :accessor context-array-type-ids)
   (uniform-struct-ids :initform (make-hash-table :test #'eq)
                       :accessor context-uniform-struct-ids)
   (stage :initform nil :accessor context-stage)
   (task-payload-variable :initform nil
                          :accessor context-task-payload-variable)
   (mesh-primitive-indices-variable
    :initform nil :accessor context-mesh-primitive-indices-variable)
   (loaded-values :initform (make-hash-table :test #'eq)
                  :accessor context-loaded-values)
   (loaded-blocks :initform (make-hash-table :test #'eq)
                  :accessor context-loaded-blocks)
   (loaded-instructions :initform (make-hash-table :test #'eq)
                        :accessor context-loaded-instructions)
   (constant-instructions :initform (make-hash-table :test #'equal)
                          :accessor context-constant-instructions)
   (expression-values :initform (make-hash-table :test #'eq)
                      :accessor context-expression-values)
   (map-component-values :initform (make-hash-table :test #'eq)
                         :accessor context-map-component-values)
   (expression-instructions :initform (make-hash-table :test #'eq)
                            :reader context-expression-instructions)
   (instruction-expressions :initform (make-hash-table :test #'eq)
                            :reader context-instruction-expressions)
   (claimed-ids :initform (make-hash-table :test #'eq)
                :accessor context-claimed-ids)
   (name-counts :initform (make-hash-table :test #'equal)
                :accessor context-name-counts)
   (extended-instruction-imports
    :initform nil :accessor context-extended-instruction-imports)
   (type-declarations :initform nil :accessor context-type-declarations)
   (constant-declarations :initform nil :accessor context-constant-declarations)
   (variable-declarations :initform nil :accessor context-variable-declarations)
   (annotations :initform nil :accessor context-annotations)
   (interfaces :initform nil :accessor context-interfaces)
   (fold-values :initform (make-hash-table :test #'eq)
                :accessor context-fold-values)
   (basic-blocks :initform nil :accessor context-basic-blocks)
   (current-block :initform nil :accessor context-current-block)
   (instructions :initform nil :accessor context-instructions)))

(defun begin-shader-basic-block (context label)
  (let ((block (make-instance 'spir-v-basic-block :label label)))
    (setf (context-basic-blocks context)
          (nconc (context-basic-blocks context) (list block))
          (context-current-block context) block)
    block))

(defun shader-id-string (name)
  (let ((text (string-upcase (string name))))
    (with-output-to-string (stream)
      (write-char #\% stream)
      (loop for character across text
            do (write-char (if (or (alphanumericp character)
                                   (char= character #\-))
                               character
                               #\-)
                           stream)))))

(defun shader-id (name)
  (intern (shader-id-string name) (find-package '#:luv.spir-v)))

(defun reserve-shader-id (context name)
  (let ((id (shader-id name)))
    (if (gethash id (context-claimed-ids context))
        (fresh-shader-id context name)
        (progn
          (setf (gethash id (context-claimed-ids context)) t)
          id))))

(defun fresh-shader-id (context name)
  (let* ((base (shader-id-string name))
         (count (gethash base (context-name-counts context) 0)))
    (loop
      for next = (1+ count) then (1+ next)
      for candidate-name = (if (= next 1) base
                               (format nil "~A-~D" base next))
      for candidate = (intern candidate-name (find-package '#:luv.spir-v))
      unless (gethash candidate (context-claimed-ids context))
        do (setf (gethash base (context-name-counts context)) next
                 (gethash candidate (context-claimed-ids context)) t)
           (cl:return candidate))))

(defun append-context-form (slot context form)
  (setf (slot-value context slot)
        (nconc (slot-value context slot) (list form)))
  form)

(defun ensure-shader-type-id (context type)
  (let ((type (find-shader-type type)))
    (or (gethash type (context-type-ids context))
        (let* ((kind (shader-type-opaque-kind type))
               (id (reserve-shader-id context (shader-type-name type))))
          (setf (gethash type (context-type-ids context)) id)
          (append-context-form
           'type-declarations context
           (cond ((shader-type= type :bool)
                  (list id 'type-bool))
                 ((eq kind :texture-2d)
                  (list id 'type-image
                        (ensure-shader-type-id
                         context
                         (ecase
                             (shader-type-scalar-kind
                              (find-shader-type
                               (shader-type-sample-result-type type)))
                           (:float :float)
                           (:uint :uint)))
                        '2d
                        (if (shader-type-image-depth-p type) 1 0)
                        0 0 1 'unknown))
                 ((eq kind :sampler) (list id 'type-sampler))
                 ((= (shader-type-component-count type) 1)
                  (ecase (shader-type-scalar-kind type)
                    (:float (list id 'type-float
                                  (shader-type-bit-width type)))
                    (:uint (list id 'type-int
                                 (shader-type-bit-width type) 0))))
                 (t
                  (list id 'type-vector
                        (ensure-shader-type-id
                         context
                         (ecase (shader-type-scalar-kind type)
                           (:float :float)
                           (:uint :uint)))
                        (shader-type-component-count type)))))
          id))))

(defun ensure-void-type-id (context)
  (let ((id (shader-id "VOID")))
    (unless (gethash id (context-claimed-ids context))
      (reserve-shader-id context "VOID")
      (append-context-form 'type-declarations context
                           (list id 'type-void)))
    id))

(defun ensure-bool-type-id (context)
  (let ((type (find-shader-type :bool)))
    (or (gethash type (context-type-ids context))
        (gethash :bool (context-type-ids context))
      (let ((id (reserve-shader-id context "BOOL")))
        (setf (gethash :bool (context-type-ids context)) id
              (gethash type (context-type-ids context)) id)
        (append-context-form 'type-declarations context
                             (list id 'type-bool))
        id))))

(defun ensure-pointer-type-id (context storage-class value-type)
  (let* ((value-id (ensure-shader-type-id context value-type))
         (key (list storage-class value-id)))
    (or (gethash key (context-pointer-ids context))
        (let ((id (reserve-shader-id
                   context
                   (format nil "~A-~A-POINTER"
                           storage-class
                           (shader-type-name (find-shader-type value-type))))))
          (setf (gethash key (context-pointer-ids context)) id)
          (append-context-form 'type-declarations context
                               (list id 'type-pointer storage-class value-id))
          id))))

(defun ensure-pointer-to-type-id (context storage-class value-id name)
  (let ((key (list storage-class value-id)))
    (or (gethash key (context-pointer-ids context))
        (let ((id (reserve-shader-id context name)))
          (setf (gethash key (context-pointer-ids context)) id)
          (append-context-form 'type-declarations context
                               (list id 'type-pointer storage-class value-id))
          id))))

(defun ensure-array-type-id (context element-type element-count)
  (let* ((element-type (find-shader-type element-type))
         (key (list element-type element-count)))
    (or (gethash key (context-array-type-ids context))
        (let* ((element-id (ensure-shader-type-id context element-type))
               (length-id
                 (reserve-shader-id
                  context
                  (format nil "ARRAY-LENGTH-~D-~A"
                          element-count (shader-type-name element-type))))
               (id (reserve-shader-id
                    context
                    (format nil "~A-ARRAY-~D"
                            (shader-type-name element-type) element-count))))
          (setf (gethash key (context-array-type-ids context)) id)
          ;; Array lengths are type operands, so keep their constants directly
          ;; beside the derived type instead of in the later value-constant
          ;; section.
          (append-context-form
           'type-declarations context
           (list length-id 'constant
                 (ensure-shader-type-id context :uint) element-count))
          (append-context-form
           'type-declarations context
           (list id 'type-array element-id length-id))
          id))))

(defun ensure-uniform-block-type-id (context block)
  (or (gethash block (context-uniform-struct-ids context))
      (let ((id (reserve-shader-id
                 context
                 (format nil "~A-BLOCK" (shader-object-name block)))))
        (setf (gethash block (context-uniform-struct-ids context)) id)
        (append-context-form
         'type-declarations context
         (list* id 'type-struct
                (mapcar (lambda (member)
                          (ensure-shader-type-id
                           context (shader-declaration-type member)))
                        (shader-uniform-block-members block))))
        (append-context-form 'annotations context
                             (list 'decorate id 'block))
        (dolist (member (shader-uniform-block-members block))
          (append-context-form
           'annotations context
           (list 'member-decorate id
                 (shader-uniform-member-index member)
                 'offset (shader-uniform-member-offset member))))
        id)))

(defun ensure-uniform-block-pointer-type-id (context block)
  (let* ((struct-id (ensure-uniform-block-type-id context block))
         (key (list 'uniform struct-id)))
    (or (gethash key (context-pointer-ids context))
        (let ((id (reserve-shader-id
                   context
                   (format nil "~A-POINTER" (shader-object-name block)))))
          (setf (gethash key (context-pointer-ids context)) id)
          (append-context-form 'type-declarations context
                               (list id 'type-pointer 'uniform struct-id))
          id))))

(defun ensure-storage-buffer-type-id (context buffer)
  "Return the id of BUFFER's block struct: one runtime array of elements."
  (or (gethash buffer (context-uniform-struct-ids context))
      (let* ((name (shader-object-name buffer))
             (element-id (ensure-shader-type-id
                          context (shader-storage-buffer-element-type buffer)))
             (array-id (reserve-shader-id
                        context (format nil "~A-RUNTIME-ARRAY" name)))
             (id (reserve-shader-id context (format nil "~A-BLOCK" name))))
        (setf (gethash buffer (context-uniform-struct-ids context)) id)
        (append-context-form 'type-declarations context
                             (list array-id 'type-runtime-array element-id))
        (append-context-form
         'annotations context
         (list 'decorate array-id 'array-stride
               (shader-storage-buffer-element-stride buffer)))
        (append-context-form 'type-declarations context
                             (list id 'type-struct array-id))
        (append-context-form 'annotations context
                             (list 'decorate id 'block))
        (append-context-form 'annotations context
                             (list 'member-decorate id 0 'offset 0))
        (append-context-form 'annotations context
                             (list 'member-decorate id 0 'non-writable))
        id)))

(defun ensure-storage-buffer-pointer-type-id (context buffer)
  (let* ((struct-id (ensure-storage-buffer-type-id context buffer))
         (key (list 'storage-buffer struct-id)))
    (or (gethash key (context-pointer-ids context))
        (let ((id (reserve-shader-id
                   context
                   (format nil "~A-POINTER" (shader-object-name buffer)))))
          (setf (gethash key (context-pointer-ids context)) id)
          (append-context-form 'type-declarations context
                               (list id 'type-pointer 'storage-buffer struct-id))
          id))))

(defun shader-constant-name (value)
  (format nil "FLOAT-~A" value))

(defun ensure-shader-constant (context value &optional expression)
  (let* ((value (coerce value 'single-float))
         (key (list :float value)))
    (multiple-value-bind (id found-p)
        (gethash key (context-constant-ids context))
      (if found-p
          (progn
            (when expression
              (associate-shader-instruction
               context expression
               (gethash key (context-constant-instructions context))))
            id)
          (let* ((id (reserve-shader-id context
                                        (shader-constant-name value)))
                 (instruction
                   (parse-instruction
                    (list id 'constant
                          (ensure-shader-type-id context :float) value))))
            (setf (gethash key (context-constant-ids context)) id
                  (gethash key (context-constant-instructions context))
                  instruction)
            (append-context-form
             'constant-declarations context instruction)
            (when expression
              (associate-shader-instruction context expression instruction))
            id)))))

(defun ensure-shader-uint-constant (context value)
  "Return an internal unsigned constant used for structural addressing."
  (let ((key (list :uint value)))
    (or (gethash key (context-constant-ids context))
        (let ((type-id (ensure-shader-type-id context :uint))
              (id (reserve-shader-id context
                                     (format nil "UINT-~D" value))))
          (setf (gethash key (context-constant-ids context)) id)
          (append-context-form 'constant-declarations context
                               (list id 'constant type-id value))
          id))))

(defun ensure-sampled-image-type-id (context texture-type)
  (let* ((texture-type (find-shader-type texture-type))
         (key (list :sampled-image texture-type))
         (table (context-pointer-ids context)))
    (or (gethash key table)
        (let ((id (reserve-shader-id
                   context
                   (format nil "~A-SAMPLED-IMAGE"
                           (shader-type-name texture-type)))))
          (setf (gethash key table) id)
          (append-context-form
           'type-declarations context
           (list id 'type-sampled-image
                 (ensure-shader-type-id context texture-type)))
          id))))

(defun ensure-glsl-extended-import (context)
  "Return the module's single GLSL.std.450 import id, requesting it once.

Modules whose expressions use no extended mathematics never acquire one."
  (let ((import (first (context-extended-instruction-imports context))))
    (if import
        (spir-v-extended-instruction-import-result-id import)
        (let ((id (reserve-shader-id context "GLSL-STD-450")))
          (setf (context-extended-instruction-imports context)
                (list (make-instance 'spir-v-extended-instruction-import
                                     :result-id id)))
          id))))

(defun register-shader-variable (context declaration)
  (let* ((direction
           (etypecase declaration
             (shader-interface-variable
              (ecase (shader-interface-direction declaration)
                (:input 'input)
                (:output 'output)))
             (shader-uniform-block 'uniform)
             (shader-storage-buffer 'storage-buffer)
             (shader-resource 'uniform-constant)))
         (type (shader-declaration-type declaration))
         (pointer-id
           (typecase declaration
             (shader-uniform-block
              (ensure-uniform-block-pointer-type-id context declaration))
             (shader-storage-buffer
              (ensure-storage-buffer-pointer-type-id context declaration))
             (t (ensure-pointer-type-id context direction type))))
         (variable-id (reserve-shader-id context
                                         (shader-object-name declaration))))
    (setf (gethash declaration (context-variable-ids context)) variable-id)
    (append-context-form 'variable-declarations context
                         (list variable-id 'variable pointer-id direction))
    (etypecase declaration
      (shader-interface-variable
       (append-context-form
        'annotations context
        (if (shader-interface-built-in declaration)
            (list 'decorate variable-id 'built-in
                  (list 'enum 'built-in
                        (shader-interface-built-in declaration)))
            (list 'decorate variable-id 'location
                  (shader-interface-location declaration))))
       (setf (context-interfaces context)
             (nconc (context-interfaces context) (list variable-id))))
      (shader-resource
       (append-context-form
        'annotations context
        (list 'decorate variable-id 'descriptor-set
              (shader-resource-descriptor-set declaration)))
       (append-context-form
        'annotations context
        (list 'decorate variable-id 'binding
              (shader-resource-binding declaration)))
       ;; SPIR-V 1.4 modules list every global the entry point touches.
       (when (member (context-stage context) '(:task :mesh))
         (setf (context-interfaces context)
               (nconc (context-interfaces context) (list variable-id))))))
    variable-id))

(defun register-workgroup-size-value (context declaration workgroup-size)
  (let* ((type-id (ensure-shader-type-id context :uvec3))
         (value-id (reserve-shader-id context
                                      (shader-object-name declaration))))
    (append-context-form
     'constant-declarations context
     (list* value-id 'constant-composite type-id
            (mapcar (lambda (extent)
                      (ensure-shader-uint-constant context extent))
                    workgroup-size)))
    (append-context-form
     'annotations context
     (list 'decorate value-id 'built-in '(enum built-in workgroup-size)))
    (setf (gethash declaration (context-direct-values context)) value-id)
    value-id))

(defun ensure-task-payload-type-id (context payload)
  (or (gethash payload (context-uniform-struct-ids context))
      (let ((id (reserve-shader-id
                 context
                 (format nil "~A-PAYLOAD-TYPE"
                         (shader-object-name payload)))))
        (setf (gethash payload (context-uniform-struct-ids context)) id)
        (append-context-form
         'type-declarations context
         (list* id 'type-struct
                (mapcar
                 (lambda (field)
                   (let ((element-count
                           (shader-task-payload-field-element-count field)))
                     (if element-count
                         (ensure-array-type-id
                          context (shader-declaration-type field) element-count)
                         (ensure-shader-type-id
                          context (shader-declaration-type field)))))
                 (shader-task-payload-fields payload))))
        id)))

(defun register-task-payload (context payload)
  (let* ((type-id (ensure-task-payload-type-id context payload))
         (pointer-id
           (ensure-pointer-to-type-id
            context 'task-payload-workgroup-ext type-id
            (format nil "~A-PAYLOAD-POINTER" (shader-object-name payload))))
         (variable-id
           (reserve-shader-id
            context (format nil "~A-PAYLOAD" (shader-object-name payload)))))
    (append-context-form
     'variable-declarations context
     (list variable-id 'variable pointer-id 'task-payload-workgroup-ext))
    (setf (context-task-payload-variable context) variable-id
          (context-interfaces context)
          (nconc (context-interfaces context) (list variable-id)))
    variable-id))

(defun mesh-output-array-size (mesh-output per-primitive-p)
  (if per-primitive-p
      (shader-mesh-output-max-primitives mesh-output)
      (shader-mesh-output-max-vertices mesh-output)))

(defun register-mesh-output-variable
    (context declaration mesh-output per-primitive-p)
  (let* ((element-type (shader-declaration-type declaration))
         (array-type-id
           (ensure-array-type-id
            context element-type
            (mesh-output-array-size mesh-output per-primitive-p)))
         (pointer-id
           (ensure-pointer-to-type-id
            context 'output array-type-id
            (format nil "~A-OUTPUT-ARRAY-POINTER"
                    (shader-object-name declaration))))
         (variable-id
           (reserve-shader-id context (shader-object-name declaration))))
    (setf (gethash declaration (context-variable-ids context)) variable-id)
    (append-context-form 'variable-declarations context
                         (list variable-id 'variable pointer-id 'output))
    (append-context-form
     'annotations context
     (if (shader-interface-built-in declaration)
         (list 'decorate variable-id 'built-in
               (list 'enum 'built-in
                     (shader-interface-built-in declaration)))
         (list 'decorate variable-id 'location
               (shader-interface-location declaration))))
    (when per-primitive-p
      (append-context-form 'annotations context
                           (list 'decorate variable-id 'per-primitive-ext)))
    (setf (context-interfaces context)
          (nconc (context-interfaces context) (list variable-id)))
    variable-id))

(defun register-mesh-outputs (context mesh-output)
  (dolist (declaration (shader-mesh-output-vertex-outputs mesh-output))
    (register-mesh-output-variable context declaration mesh-output nil))
  (dolist (declaration (shader-mesh-output-primitive-outputs mesh-output))
    (register-mesh-output-variable context declaration mesh-output t))
  (let* ((topology (shader-mesh-output-topology mesh-output))
         (index-type (mesh-topology-index-type topology))
         (array-type-id
           (ensure-array-type-id
            context index-type
            (shader-mesh-output-max-primitives mesh-output)))
         (pointer-id
           (ensure-pointer-to-type-id
            context 'output array-type-id "PRIMITIVE-INDICES-POINTER"))
         (variable-id (reserve-shader-id context "PRIMITIVE-INDICES")))
    (append-context-form 'variable-declarations context
                         (list variable-id 'variable pointer-id 'output))
    (append-context-form
     'annotations context
     (list 'decorate variable-id 'built-in
           (list 'enum 'built-in
                 (ecase topology
                   (:points 'primitive-point-indices-ext)
                   (:lines 'primitive-line-indices-ext)
                   (:triangles 'primitive-triangle-indices-ext)))))
    (setf (context-mesh-primitive-indices-variable context) variable-id
          (context-interfaces context)
          (nconc (context-interfaces context) (list variable-id)))))

(defun associate-shader-instruction (context expression instruction)
  (let ((forward (context-expression-instructions context))
        (reverse (context-instruction-expressions context)))
    (unless (member instruction (gethash expression forward) :test #'eq)
      (setf (gethash expression forward)
            (nconc (gethash expression forward) (list instruction))))
    (unless (member expression (gethash instruction reverse) :test #'eq)
      (setf (gethash instruction reverse)
            (nconc (gethash instruction reverse) (list expression)))))
  instruction)

(defun emit-shader-instruction (context expression form)
  (let ((instruction (parse-instruction form)))
    (setf (context-instructions context)
          (nconc (context-instructions context) (list instruction)))
    (let ((block (context-current-block context)))
      (unless block
        (error 'shader-language-error
               :form form :reason :instruction-outside-basic-block))
      (setf (spir-v-basic-block-instructions block)
            (nconc (spir-v-basic-block-instructions block)
                   (list instruction))))
    (when expression
      (associate-shader-instruction context expression instruction))
    instruction))

(defun alias-shader-expression (context expression source-expression)
  (dolist (instruction
           (gethash source-expression (context-expression-instructions context)))
    (associate-shader-instruction context expression instruction)))

(defgeneric shader-operator-result-name (operator)
  (:documentation
   "The noun naming OPERATOR's SSA results in lowered provenance."))

(defmethod shader-operator-result-name ((operator symbol))
  operator)

(defmethod shader-operator-result-name ((operator (eql '+))) 'sum)
(defmethod shader-operator-result-name ((operator (eql '-))) 'difference)
(defmethod shader-operator-result-name ((operator (eql '*))) 'product)
(defmethod shader-operator-result-name ((operator (eql '/))) 'quotient)

(defgeneric shader-expression-provenance-name (expression)
  (:documentation "The default noun naming EXPRESSION's lowered results."))

(defmethod shader-expression-provenance-name ((expression shader-literal))
  'literal)

(defmethod shader-expression-provenance-name ((expression shader-reference))
  (shader-object-name (shader-reference-target expression)))

(defmethod shader-expression-provenance-name ((expression shader-call))
  (shader-operator-result-name (shader-call-operator expression)))

(defmethod shader-expression-provenance-name
    ((expression shader-function-call))
  (shader-object-name (shader-function-call-definition expression)))

(defmethod shader-expression-provenance-name
    ((expression shader-conditional))
  (declare (ignore expression))
  'conditional)

(defmethod shader-expression-provenance-name
    ((expression shader-map-application))
  (declare (ignore expression))
  'homogeneous-point)

(defmethod shader-expression-provenance-name
    ((expression shader-map-projection))
  (declare (ignore expression))
  'projected-sample)

(defmethod shader-expression-provenance-name
    ((expression shader-payload-element))
  (shader-object-name (shader-payload-element-field expression)))

(defmethod shader-expression-provenance-name
    ((expression shader-buffer-element))
  (shader-object-name (shader-buffer-element-buffer expression)))

(defmethod shader-expression-provenance-name
    ((expression shader-interpretation))
  (declare (ignore expression))
  'interpretation)

(defmethod shader-expression-provenance-name
    ((expression shader-quantity-construction))
  (declare (ignore expression))
  'quantity)

(defmethod shader-expression-provenance-name
    ((expression shader-quantity-assumption))
  (declare (ignore expression))
  'assumption)

(defmethod shader-expression-provenance-name
    ((expression shader-representation))
  (declare (ignore expression))
  'representation)

(defun expression-result-name (expression)
  (or (shader-expression-name expression)
      (shader-expression-provenance-name expression)))

(defun emit-value-instruction (context expression type instruction operands)
  ;; Types own their canonical names.  Claim the result type before deriving a
  ;; value name: a first-use constructor such as (VEC3 0 0 0) otherwise lets
  ;; both its type and its value independently choose %VEC3.
  (let ((type-id (ensure-shader-type-id context type)))
    (let ((result
            (fresh-shader-id context (expression-result-name expression))))
      (emit-shader-instruction
       context expression
       (list* result instruction type-id operands))
      result)))

(defun lower-shader-reference (context expression)
  (let ((target (shader-reference-target expression)))
    (multiple-value-bind (direct direct-p)
        (gethash target (context-direct-values context))
      (if direct-p
          direct
          (etypecase target
            (shader-binding
             (multiple-value-bind (fold-value fold-value-p)
                 (gethash target (context-fold-values context))
               (if fold-value-p
                   fold-value
                   (let* ((source (shader-binding-expression target))
                          (value (lower-shader-expression context source)))
                     (alias-shader-expression context expression source)
                     value))))
            (shader-task-payload-field
             (let* ((type (shader-declaration-type target))
                    (pointer
                      (fresh-shader-id
                       context
                       (format nil "~A-POINTER" (shader-object-name target))))
                    (value
                      (fresh-shader-id context (shader-object-name target))))
               (emit-shader-instruction
                context expression
                (list pointer 'access-chain
                      (ensure-pointer-type-id
                       context 'task-payload-workgroup-ext type)
                      (context-task-payload-variable context)
                      (ensure-shader-uint-constant
                       context (shader-task-payload-field-index target))))
               (emit-shader-instruction
                context expression
                (list value 'load (ensure-shader-type-id context type) pointer))
               value))
            (shader-uniform-member
             (let* ((block (shader-uniform-member-block target))
                    (type (shader-declaration-type target))
                    (pointer
                      (fresh-shader-id context
                                       (format nil "~A-POINTER"
                                               (shader-object-name target))))
                    (value
                      (fresh-shader-id context (shader-object-name target))))
               (emit-shader-instruction
                context expression
                (list pointer 'access-chain
                      (ensure-pointer-type-id context 'uniform type)
                      (gethash block (context-variable-ids context))
                      (ensure-shader-uint-constant
                       context (shader-uniform-member-index target))))
               (emit-shader-instruction
                context expression
                (list value 'load (ensure-shader-type-id context type) pointer))
               value))
            (shader-variable-declaration
             (multiple-value-bind (value found-p)
                 (gethash target (context-loaded-values context))
               (if (and found-p
                        (eq (gethash target (context-loaded-blocks context))
                            (context-current-block context)))
                   (progn
                     (associate-shader-instruction
                      context expression
                      (gethash target (context-loaded-instructions context)))
                     value)
                   (let* ((type (shader-declaration-type target))
                          (value
                            (emit-value-instruction
                             context expression type 'load
                             (list (gethash
                                    target (context-variable-ids context)))))
                          (instruction
                            (car (last (context-instructions context)))))
                     (setf (gethash target (context-loaded-values context)) value
                           (gethash target (context-loaded-blocks context))
                           (context-current-block context)
                           (gethash target
                                    (context-loaded-instructions context))
                           instruction)
                     value)))))))))

(defgeneric binary-arithmetic-instruction (operator left-type right-type)
  (:documentation
   "The SPIR-V instruction computing one binary step of OPERATOR."))

(defmethod binary-arithmetic-instruction ((operator (eql '+)) left-type right-type)
  (declare (ignore right-type))
  (ecase (shader-type-scalar-kind left-type)
    (:float 'f-add)
    (:uint 'i-add)))

(defmethod binary-arithmetic-instruction ((operator (eql '-)) left-type right-type)
  (declare (ignore right-type))
  (ecase (shader-type-scalar-kind left-type)
    (:float 'f-sub)
    (:uint 'i-sub)))

(defmethod binary-arithmetic-instruction ((operator (eql '/)) left-type right-type)
  (declare (ignore right-type))
  (ecase (shader-type-scalar-kind left-type)
    (:float 'f-div)
    (:uint 'u-div)))

(defmethod binary-arithmetic-instruction ((operator (eql '*)) left-type right-type)
  (if (or (and (shader-vector-type-p left-type)
               (shader-float-type-p right-type))
          (and (shader-float-type-p left-type)
               (shader-vector-type-p right-type)))
      'vector-times-scalar
      (ecase (shader-type-scalar-kind left-type)
        (:float 'f-mul)
        (:uint 'i-mul))))

(defun emit-binary-arithmetic
    (context expression operator result-type left-id left-type right-id right-type)
  (let ((instruction
          (binary-arithmetic-instruction operator left-type right-type)))
    (when (and (eq instruction 'vector-times-scalar)
               (shader-float-type-p left-type))
      (rotatef left-id right-id))
    (emit-value-instruction context expression result-type instruction
                            (list left-id right-id))))

(defun emit-extended-instruction (context expression type instruction-name operands)
  "Emit one GLSL.std.450 operation, keyed by its enumerated instruction name."
  (emit-value-instruction
   context expression type 'ext-inst
   (list* (ensure-glsl-extended-import context)
          (list 'enum 'glsl-std-450 instruction-name)
          operands)))

(defun lower-extended-call (context expression instruction-name)
  "Lower EXPRESSION as one extended instruction over its lowered operands."
  (emit-extended-instruction
   context expression (shader-expression-type expression) instruction-name
   (mapcar (lambda (operand) (lower-shader-expression context operand))
           (shader-call-operands expression))))

(defun lower-chained-extended-call (context expression instruction-name)
  "Fold EXPRESSION's operands left to right through a binary extended step."
  (let* ((operands (shader-call-operands expression))
         (value (lower-shader-expression context (first operands))))
    (dolist (operand (rest operands) value)
      (setf value
            (emit-extended-instruction
             context expression (shader-expression-type expression)
             instruction-name
             (list value (lower-shader-expression context operand)))))))

(defgeneric lower-shader-call (operator context expression)
  (:argument-precedence-order context operator expression)
  (:documentation
   "Lower EXPRESSION into CONTEXT's target product and return its value.

Target context deliberately precedes operator identity in method selection:
an operator implemented only for one backend must not capture another
backend's context before its source-located unsupported-operation method."))

(defmethod lower-shader-call (operator context expression)
  (declare (ignore context))
  (error 'shader-language-error
         :form (shader-expression-source-form expression)
         :reason :unknown-operator :details operator))

(defun lower-chained-arithmetic (context expression)
  "Fold EXPRESSION's operands left to right through its binary operator."
  (let* ((operator (shader-call-operator expression))
         (operands (shader-call-operands expression))
         (first (first operands))
         (value (lower-shader-expression context first))
         (value-type (shader-expression-type first)))
    (dolist (operand (rest operands) value)
      (let ((operand-value (lower-shader-expression context operand))
            (operand-type (shader-expression-type operand)))
        (setf value
              (emit-binary-arithmetic
               context expression operator
               (cond ((shader-vector-type-p value-type) value-type)
                     ((shader-vector-type-p operand-type) operand-type)
                     (t (shader-expression-type expression)))
               value value-type operand-value operand-type)
              value-type
              (cond ((shader-vector-type-p value-type) value-type)
                    ((shader-vector-type-p operand-type) operand-type)
                    (t (shader-expression-type expression))))))))

(defmethod lower-shader-call ((operator (eql '+)) context expression)
  (lower-chained-arithmetic context expression))

(defmethod lower-shader-call ((operator (eql '*)) context expression)
  (lower-chained-arithmetic context expression))

(defmethod lower-shader-call ((operator (eql '-)) context expression)
  (let ((operands (shader-call-operands expression)))
    (if (= (length operands) 1)
        (if (eq :float (shader-type-scalar-kind
                        (find-shader-type (shader-expression-type expression))))
            (emit-value-instruction
             context expression (shader-expression-type expression) 'f-negate
             (list (lower-shader-expression context (first operands))))
            (error 'shader-language-error
                   :form (shader-expression-source-form expression)
                   :reason :unsigned-negation))
        (lower-chained-arithmetic context expression))))

(defmethod lower-shader-call ((operator (eql 'mod)) context expression)
  (destructuring-bind (left right) (shader-call-operands expression)
    (emit-value-instruction
     context expression (shader-expression-type expression) 'u-mod
     (list (lower-shader-expression context left)
           (lower-shader-expression context right)))))

(defmethod lower-shader-call ((operator (eql 'ldb)) context expression)
  ;; Left-align the field, then right-align it: two logical shifts by 32-bit
  ;; amounts extract any field of a 32- or 64-bit value without bit-field
  ;; instructions, which Vulkan restricts to 32-bit operands, and without
  ;; wide mask constants.  A run-time position subtracts itself from the
  ;; constant left shift.
  (let* ((type (shader-expression-type expression))
         (width (shader-type-bit-width type))
         (size (shader-bit-field-size expression))
         (position (shader-bit-field-position expression))
         (operands (shader-call-operands expression))
         (value (lower-shader-expression context (first operands)))
         (left-shift
           (cond (position
                  (and (< (+ size position) width)
                       (ensure-shader-uint-constant
                        context (- width size position))))
                 (t
                  (emit-value-instruction
                   context expression :uint 'i-sub
                   (list (ensure-shader-uint-constant context (- width size))
                         (lower-shader-expression
                          context (second operands)))))))
         (aligned
           (if left-shift
               (emit-value-instruction
                context expression type 'shift-left-logical
                (list value left-shift))
               value)))
    (if (= size width)
        (progn
          (alias-shader-expression context expression (first operands))
          aligned)
        (emit-value-instruction
         context expression type 'shift-right-logical
         (list aligned
               (ensure-shader-uint-constant context (- width size)))))))

(defmethod lower-shader-call ((operator (eql '/)) context expression)
  (let ((operands (shader-call-operands expression)))
    (if (and (shader-vector-type-p
              (shader-expression-type (first operands)))
             (shader-float-type-p
              (shader-expression-type (second operands))))
        (let* ((vector (first operands))
               (scalar (second operands))
               (vector-id (lower-shader-expression context vector))
               (scalar-id (lower-shader-expression context scalar))
               (float-type (find-shader-type :float))
               (reciprocal-id (fresh-shader-id context 'reciprocal)))
          (emit-shader-instruction
           context expression
           (list reciprocal-id 'f-div
                 (ensure-shader-type-id context float-type)
                 (ensure-shader-constant context 1.0) scalar-id))
          (emit-binary-arithmetic
           context expression '* (shader-expression-type vector)
           vector-id (shader-expression-type vector)
           reciprocal-id float-type))
        (lower-chained-arithmetic context expression))))

(defun comparison-instruction (operator operand-type)
  (ecase (shader-type-scalar-kind operand-type)
    (:float
     (ecase operator
       (< 'f-ord-less-than)
       (<= 'f-ord-less-than-equal)
       (> 'f-ord-greater-than)
       (>= 'f-ord-greater-than-equal)
       (= 'f-ord-equal)))
    (:uint
     (ecase operator
       (< 'u-less-than)
       (<= 'u-less-than-equal)
       (> 'u-greater-than)
       (>= 'u-greater-than-equal)
       (= 'i-equal)))))

(defmacro define-comparison-lowering (operator)
  `(defmethod lower-shader-call
       ((operator (eql ',operator)) context expression)
     (emit-value-instruction
      context expression :bool
      (comparison-instruction
       operator (shader-expression-type
                 (first (shader-call-operands expression))))
      (mapcar (lambda (operand)
                (lower-shader-expression context operand))
              (shader-call-operands expression)))))

(define-comparison-lowering <)
(define-comparison-lowering <=)
(define-comparison-lowering >)
(define-comparison-lowering >=)
(define-comparison-lowering =)

(defmethod lower-shader-call ((operator (eql 'mix)) context expression)
  (destructuring-bind (from to amount) (shader-call-operands expression)
    (let* ((from-id (lower-shader-expression context from))
           (to-id (lower-shader-expression context to))
           (amount-id (lower-shader-expression context amount))
           (one-id (ensure-shader-constant context 1.0))
           (float-type (find-shader-type :float))
           (value-type (shader-expression-type expression))
           (inverse
             (emit-binary-arithmetic context expression '- float-type
                                     one-id float-type amount-id float-type))
           ;; Emit the target contribution first to retain the arithmetic
           ;; ordering of luvcraft's original pointful shader.
           (to-part
             (emit-binary-arithmetic context expression '* value-type
                                     to-id value-type amount-id float-type))
           (from-part
             (emit-binary-arithmetic context expression '* value-type
                                     from-id value-type inverse float-type)))
      (emit-binary-arithmetic context expression '+ value-type
                              to-part value-type from-part value-type))))

(defmethod lower-shader-call ((operator (eql 'dot)) context expression)
  (emit-value-instruction
   context expression :float 'dot
   (mapcar (lambda (operand) (lower-shader-expression context operand))
           (shader-call-operands expression))))

(defgeneric shader-map-application-for-projection (expression)
  (:documentation
   "Return the virtual map application denoted by EXPRESSION, or NIL."))

(defmethod shader-map-application-for-projection (expression)
  (declare (ignore expression))
  nil)

(defmethod shader-map-application-for-projection
    ((expression shader-map-application))
  expression)

(defgeneric shader-map-application-from-target (target)
  (:documentation
   "Return a virtual map application carried by reference TARGET, or NIL."))

(defmethod shader-map-application-from-target (target)
  (declare (ignore target))
  nil)

(defmethod shader-map-application-from-target ((target shader-binding))
  (shader-map-application-for-projection
   (shader-binding-expression target)))

(defmethod shader-map-application-for-projection
    ((expression shader-reference))
  (shader-map-application-from-target
   (shader-reference-target expression)))

(defgeneric lower-shader-map-homogeneous-components
    (definition context application &optional origin)
  (:documentation
   "Lower APPLICATION once and return its four homogeneous components."))

(defmethod lower-shader-map-homogeneous-components
    ((definition shader-projective-map-definition) context application
     &optional (origin application))
  (or (gethash application (context-map-component-values context))
      (let* ((point (shader-map-application-point application))
             (point-value (lower-shader-expression context point))
             (homogeneous
               (emit-value-instruction
                context origin :vec4 'composite-construct
                (list point-value (ensure-shader-constant context 1.0))))
             (clip
               (mapcar
                (lambda (row)
                  (emit-value-instruction
                   context origin :float 'dot
                   (list (lower-shader-expression context row) homogeneous)))
                (shader-map-application-rows application))))
        (setf (gethash application (context-map-component-values context))
              clip))))

(defgeneric lower-shader-map-sample-components
    (definition context projection)
  (:documentation
   "Project one homogeneous application into represented sample components."))

(defmethod lower-shader-map-sample-components
    ((definition shader-projective-map-definition) context projection)
  (or (gethash projection (context-map-component-values context))
      (let* ((application (shader-map-projection-application projection))
             (float-type (find-shader-type :float))
             (clip
               (lower-shader-map-homogeneous-components
                definition context application projection))
             (w (fourth clip))
             (normalized
               (loop for component in (subseq clip 0 3)
                     collect
                     (emit-binary-arithmetic
                      context projection '/ :float
                      component float-type w float-type)))
             (result
               (loop for component in normalized
                     for scale in
                       (shader-projective-map-coordinate-scale definition)
                     for offset in
                       (shader-projective-map-coordinate-offset definition)
                     collect
                     (let ((scaled
                             (if (= scale 1)
                                 component
                                 (emit-binary-arithmetic
                                  context projection '* :float
                                  component float-type
                                  (ensure-shader-constant context scale)
                                  float-type))))
                       (if (zerop offset)
                           scaled
                           (emit-binary-arithmetic
                            context projection '+ :float
                            scaled float-type
                            (ensure-shader-constant context offset)
                            float-type))))))
        (setf (gethash projection (context-map-component-values context))
              result))))

(defun lower-shader-map-projection
    (context expression projection indices)
  (let ((components
          (lower-shader-map-sample-components
           (shader-map-application-definition
            (shader-map-projection-application projection))
           context projection)))
    (alias-shader-expression context expression projection)
    (if (= (length indices) 1)
        (nth (first indices) components)
        (emit-value-instruction
         context expression (shader-expression-type expression)
         'composite-construct
         (mapcar (lambda (index) (nth index components)) indices)))))

(defgeneric shader-map-projection-for-swizzle (expression)
  (:documentation
   "Return the virtual sampling projection denoted by EXPRESSION, or NIL."))

(defmethod shader-map-projection-for-swizzle (expression)
  (declare (ignore expression))
  nil)

(defmethod shader-map-projection-for-swizzle
    ((expression shader-map-projection))
  expression)

(defgeneric shader-map-projection-from-target (target))

(defmethod shader-map-projection-from-target (target)
  (declare (ignore target))
  nil)

(defmethod shader-map-projection-from-target ((target shader-binding))
  (shader-map-projection-for-swizzle (shader-binding-expression target)))

(defmethod shader-map-projection-for-swizzle
    ((expression shader-reference))
  (shader-map-projection-from-target (shader-reference-target expression)))

(defmethod lower-shader-call ((operator (eql 'swizzle)) context expression)
  (let* ((operand (first (shader-call-operands expression)))
         (indices (swizzle-components
                   (first (shader-call-parameters expression))
                   (shader-expression-source-form expression)))
         (map-projection
           (shader-map-projection-for-swizzle operand)))
    (if map-projection
        (lower-shader-map-projection
         context expression map-projection indices)
        (let ((value (lower-shader-expression context operand)))
          (if (= (length indices) 1)
              (emit-value-instruction context expression
                                      (shader-expression-type expression)
                                      'composite-extract
                                      (list value (first indices)))
              (emit-value-instruction context expression
                                      (shader-expression-type expression)
                                      'vector-shuffle
                                      (list* value value indices)))))))

(defun lower-vector-constructor (context expression)
  (emit-value-instruction
   context expression (shader-expression-type expression)
   'composite-construct
   (mapcar (lambda (operand) (lower-shader-expression context operand))
           (shader-call-operands expression))))

(defmethod lower-shader-call ((operator (eql 'vec2)) context expression)
  (lower-vector-constructor context expression))

(defmethod lower-shader-call ((operator (eql 'vec3)) context expression)
  (lower-vector-constructor context expression))

(defmethod lower-shader-call ((operator (eql 'vec4)) context expression)
  (lower-vector-constructor context expression))

(defmethod lower-shader-call ((operator (eql 'uvec2)) context expression)
  (lower-vector-constructor context expression))

(defmethod lower-shader-call ((operator (eql 'uvec3)) context expression)
  (lower-vector-constructor context expression))

(defmethod lower-shader-call ((operator (eql 'uvec4)) context expression)
  (lower-vector-constructor context expression))

(defmethod lower-shader-call ((operator (eql 'uint)) context expression)
  (let* ((operand (first (shader-call-operands expression)))
         (type (shader-expression-type operand))
         (value (lower-shader-expression context operand)))
    (cond ((shader-uint-type-p type)
           (alias-shader-expression context expression operand)
           value)
          ((shader-unsigned-type-p type)
           (emit-value-instruction context expression :uint 'u-convert
                                   (list value)))
          (t
           (emit-value-instruction context expression :uint 'convert-f-to-u
                                   (list value))))))

(defmethod lower-shader-call ((operator (eql 'uint64)) context expression)
  (let* ((operand (first (shader-call-operands expression)))
         (type (shader-expression-type operand))
         (value (lower-shader-expression context operand)))
    (cond ((shader-type= type :uint64)
           (alias-shader-expression context expression operand)
           value)
          ((shader-unsigned-type-p type)
           (emit-value-instruction context expression :uint64 'u-convert
                                   (list value)))
          (t
           (emit-value-instruction context expression :uint64 'convert-f-to-u
                                   (list value))))))

(defmethod lower-shader-call ((operator (eql 'float)) context expression)
  (let* ((operand (first (shader-call-operands expression)))
         (value (lower-shader-expression context operand)))
    (if (shader-float-type-p (shader-expression-type operand))
        (progn (alias-shader-expression context expression operand) value)
        (emit-value-instruction context expression :float 'convert-u-to-f
                                (list value)))))

(defmethod lower-shader-call ((operator (eql 'min)) context expression)
  (lower-chained-extended-call context expression 'f-min))

(defmethod lower-shader-call ((operator (eql 'max)) context expression)
  (lower-chained-extended-call context expression 'f-max))

(defmethod lower-shader-call ((operator (eql 'abs)) context expression)
  (lower-extended-call context expression 'f-abs))

(defmethod lower-shader-call ((operator (eql 'signum)) context expression)
  (lower-extended-call context expression 'f-sign))

(defmethod lower-shader-call ((operator (eql 'sqrt)) context expression)
  (lower-extended-call context expression 'sqrt))

(macrolet
    ((define-unary-extended-lowering (&rest pairs)
       `(progn
          ,@(mapcar
             (lambda (pair)
               (destructuring-bind (operator instruction) pair
                 `(defmethod lower-shader-call
                      ((operator (eql ',operator)) context expression)
                    (lower-extended-call context expression ',instruction))))
             pairs))))
  (define-unary-extended-lowering
      (floor floor) (fract fract) (sin sin) (cos cos) (exp exp) (log log)))

(defmethod lower-shader-call
    ((operator (eql 'derivative-x)) context expression)
  (emit-value-instruction
   context expression (shader-expression-type expression) 'd-pdx
   (list (lower-shader-expression
          context (first (shader-call-operands expression))))))

(defmethod lower-shader-call
    ((operator (eql 'derivative-y)) context expression)
  (emit-value-instruction
   context expression (shader-expression-type expression) 'd-pdy
   (list (lower-shader-expression
          context (first (shader-call-operands expression))))))

(defmethod lower-shader-call ((operator (eql 'expt)) context expression)
  (lower-extended-call context expression 'pow))

(defmethod lower-shader-call ((operator (eql 'clamp)) context expression)
  (lower-extended-call context expression 'f-clamp))

(defmethod lower-shader-call ((operator (eql 'smoothstep)) context expression)
  (lower-extended-call context expression 'smooth-step))

(defmethod lower-shader-call ((operator (eql 'step)) context expression)
  (lower-extended-call context expression 'step))

(defmethod lower-shader-call ((operator (eql 'normalize)) context expression)
  (lower-extended-call context expression 'normalize))

(defmethod lower-shader-call ((operator (eql 'sample)) context expression)
  (destructuring-bind (texture sampler coordinate)
      (shader-call-operands expression)
    (let* ((texture-id (lower-shader-expression context texture))
           (sampler-id (lower-shader-expression context sampler))
           (coordinate-id (lower-shader-expression context coordinate))
           (texture-type (shader-expression-type texture))
           (sampled-id
             (fresh-shader-id context
                              (expression-result-name expression))))
      (emit-shader-instruction
       context expression
       (list sampled-id 'sampled-image
             (ensure-sampled-image-type-id context texture-type)
             texture-id sampler-id))
      (emit-value-instruction
       context expression (shader-expression-type expression)
       'image-sample-implicit-lod
       (list sampled-id coordinate-id)))))

(defmethod lower-shader-call ((operator (eql 'texel-load)) context expression)
  (destructuring-bind (texture coordinate) (shader-call-operands expression)
    (emit-value-instruction
     context expression (shader-expression-type expression) 'image-fetch
     (list (lower-shader-expression context texture)
           (lower-shader-expression context coordinate)))))

(defmethod lower-shader-call
    ((operator (eql 'sample-compare)) context expression)
  (destructuring-bind (texture sampler coordinate depth-reference)
      (shader-call-operands expression)
    (let* ((texture-id (lower-shader-expression context texture))
           (sampler-id (lower-shader-expression context sampler))
           (coordinate-id (lower-shader-expression context coordinate))
           (depth-reference-id
             (lower-shader-expression context depth-reference))
           (texture-type (shader-expression-type texture))
           (sampled-id
             (fresh-shader-id context
                              (expression-result-name expression))))
      (emit-shader-instruction
       context expression
       (list sampled-id 'sampled-image
             (ensure-sampled-image-type-id context texture-type)
             texture-id sampler-id))
      (emit-value-instruction
       context expression (shader-expression-type expression)
       'image-sample-dref-implicit-lod
       (list sampled-id coordinate-id depth-reference-id)))))

(defgeneric lower-shader-expression-value (context expression)
  (:documentation "Lower EXPRESSION into instructions and return its value id."))

(defmethod lower-shader-expression-value (context (expression shader-literal))
  (ensure-shader-constant context
                          (shader-literal-value expression)
                          expression))

(defmethod lower-shader-expression-value (context (expression shader-reference))
  (lower-shader-reference context expression))

(defmethod lower-shader-expression-value
    (context (expression shader-payload-element))
  (let* ((field (shader-payload-element-field expression))
         (type (shader-declaration-type field))
         (pointer (fresh-shader-id
                   context
                   (format nil "~A-ELEMENT-POINTER"
                           (shader-object-name field))))
         (index (lower-shader-expression
                 context (shader-payload-element-index expression))))
    (emit-shader-instruction
     context expression
     (list pointer 'access-chain
           (ensure-pointer-type-id context 'task-payload-workgroup-ext type)
           (context-task-payload-variable context)
           (ensure-shader-uint-constant
            context (shader-task-payload-field-index field))
           index))
    (emit-value-instruction context expression type 'load (list pointer))))

(defmethod lower-shader-expression-value
    (context (expression shader-buffer-element))
  (let* ((buffer (shader-buffer-element-buffer expression))
         (type (shader-storage-buffer-element-type buffer))
         (pointer (fresh-shader-id
                   context
                   (format nil "~A-ELEMENT-POINTER"
                           (shader-object-name buffer))))
         (index (lower-shader-expression
                 context (shader-buffer-element-index expression))))
    (emit-shader-instruction
     context expression
     (list pointer 'access-chain
           (ensure-pointer-type-id context 'storage-buffer type)
           (gethash buffer (context-variable-ids context))
           (ensure-shader-uint-constant context 0)
           index))
    (emit-value-instruction context expression type 'load (list pointer))))

(defmethod lower-shader-expression-value (context (expression shader-call))
  (lower-shader-call (shader-call-operator expression) context expression))

(defmethod lower-shader-expression-value
    (context (expression shader-function-call))
  (let* ((result (shader-function-call-result expression))
         (value (lower-shader-expression context result)))
    (alias-shader-expression context expression result)
    value))

(defmethod lower-shader-expression-value
    (context (expression shader-conditional))
  (emit-value-instruction
   context expression (shader-expression-type expression) 'select
   (list
    (lower-shader-expression
     context (lang:arithmetic-conditional-condition expression))
    (lower-shader-expression
     context (lang:arithmetic-conditional-consequent expression))
    (lower-shader-expression
     context (lang:arithmetic-conditional-alternative expression)))))

(defmethod lower-shader-expression-value
    (context (expression shader-counted-fold))
  (let* ((count-expression
           (lang:arithmetic-counted-fold-count expression))
         (count-type (shader-expression-type count-expression))
         (unsigned-p (shader-uint-type-p count-type))
         (preheader
           (spir-v-basic-block-label (context-current-block context)))
         (count
           (lower-shader-expression
            context count-expression))
         (initial
           (lower-shader-expression
            context (lang:arithmetic-counted-fold-initial expression)))
         (state-type (shader-expression-type expression))
         (header-label (fresh-shader-id context 'fold-header))
         (body-label (fresh-shader-id context 'fold-body))
         (continue-label (fresh-shader-id context 'fold-continue))
         (merge-label (fresh-shader-id context 'fold-merge))
         (index-id (fresh-shader-id context 'fold-index))
         (state-id (fresh-shader-id context 'fold-state))
         (next-index-id (fresh-shader-id context 'fold-next-index))
         (zero (if unsigned-p
                   (ensure-shader-uint-constant context 0)
                   (ensure-shader-constant context 0.0)))
         (one (if unsigned-p
                  (ensure-shader-uint-constant context 1)
                  (ensure-shader-constant context 1.0))))
    (emit-shader-instruction context expression (list 'branch header-label))
    (let ((header (begin-shader-basic-block context header-label)))
      ;; The index and state phis are prepended to the header once the back
      ;; edge is known, so an :UNTIL test lowered here may already refer to
      ;; them through the fold values.
      (setf (gethash (lang:arithmetic-counted-fold-index-binding expression)
                     (context-fold-values context))
            index-id
            (gethash (lang:arithmetic-counted-fold-state-binding expression)
                     (context-fold-values context))
            state-id)
      (let ((condition-id (fresh-shader-id context 'fold-condition))
            (until (lang:arithmetic-counted-fold-until expression)))
        (emit-shader-instruction
         context expression
         (list condition-id (if unsigned-p 'u-less-than 'f-ord-less-than)
               (ensure-bool-type-id context) index-id count))
        (when until
          (let ((until-id (lower-shader-expression context until))
                (continue-id (fresh-shader-id context 'fold-continue-p))
                (guarded-id (fresh-shader-id context 'fold-guarded-condition)))
            (emit-shader-instruction
             context expression
             (list continue-id 'logical-not
                   (ensure-bool-type-id context) until-id))
            (emit-shader-instruction
             context expression
             (list guarded-id 'logical-and
                   (ensure-bool-type-id context) condition-id continue-id))
            (setf condition-id guarded-id)))
        (emit-shader-instruction
         context expression
         (list 'loop-merge merge-label continue-label 'none))
        (emit-shader-instruction
         context expression
         (list 'branch-conditional condition-id body-label merge-label)))
      (begin-shader-basic-block context body-label)
      (let ((next-state
              (lower-shader-expression
               context (lang:arithmetic-counted-fold-update expression))))
        (emit-shader-instruction context expression
                                 (list 'branch continue-label))
        (begin-shader-basic-block context continue-label)
        (emit-shader-instruction
         context expression
         (list next-index-id (if unsigned-p 'i-add 'f-add)
               (ensure-shader-type-id context count-type) index-id one))
        (emit-shader-instruction context expression (list 'branch header-label))
        (let ((index-phi
                (parse-instruction
                 (list index-id 'phi
                       (ensure-shader-type-id context count-type)
                       zero preheader next-index-id continue-label)))
              (state-phi
                (parse-instruction
                 (list state-id 'phi
                       (ensure-shader-type-id context state-type)
                       initial preheader next-state continue-label))))
          (setf (spir-v-basic-block-instructions header)
                (list* index-phi state-phi
                       (spir-v-basic-block-instructions header))
                (context-instructions context)
                (nconc (context-instructions context)
                       (list index-phi state-phi)))
          (associate-shader-instruction context expression state-phi)))
      (remhash (lang:arithmetic-counted-fold-index-binding expression)
               (context-fold-values context))
      (remhash (lang:arithmetic-counted-fold-state-binding expression)
               (context-fold-values context))
      (begin-shader-basic-block context merge-label)
      state-id)))

(defmethod lower-shader-expression-value
    (context (expression shader-map-application))
  (emit-value-instruction
   context expression (shader-expression-type expression) 'composite-construct
   (lower-shader-map-homogeneous-components
    (shader-map-application-definition expression) context expression)))

(defmethod lower-shader-expression-value
    (context (expression shader-map-projection))
  (declare (ignore context))
  (error 'shader-language-error
         :form (shader-expression-source-form expression)
         :reason :sampling-projection-requires-field-selection))

(defmethod lower-shader-expression-value
    (context (expression shader-quantity-boundary))
  (let* ((operand (shader-quantity-boundary-operand expression))
         (value (lower-shader-expression context operand)))
    (alias-shader-expression context expression operand)
    value))

(defmethod lower-shader-expression-value
    (context (expression shader-unit-conversion))
  (let* ((operand (shader-unit-conversion-operand expression))
         (operand-value (lower-shader-expression context operand))
         (factor (shader-unit-conversion-factor expression)))
    (if (= factor 1)
        (progn
          (alias-shader-expression context expression operand)
          operand-value)
        (emit-binary-arithmetic
         context expression '* (shader-expression-type expression)
         operand-value (shader-expression-type operand)
         (ensure-shader-constant context factor) (find-shader-type :float)))))

(defun lower-shader-expression (context expression)
  (or (gethash expression (context-expression-values context))
      (setf (gethash expression (context-expression-values context))
            (lower-shader-expression-value context expression))))

(defgeneric lower-shader-statement (context statement)
  (:documentation "Lower one semantic shader effect into SPIR-V control/data flow."))

(defmethod lower-shader-statement
    (context (statement shader-output-assignment))
  (let* ((expression (shader-assignment-value statement))
         (value (lower-shader-expression context expression))
         (output-id
           (gethash (shader-assignment-output statement)
                    (context-variable-ids context))))
    (emit-shader-instruction context expression (list 'store output-id value))))

(defmethod lower-shader-statement
    (context (statement shader-conditional-statement))
  (let ((condition
          (lower-shader-expression
           context (shader-conditional-statement-condition statement)))
        (body-label (fresh-shader-id context 'conditional-body))
        (merge-label (fresh-shader-id context 'conditional-merge)))
    (emit-shader-instruction
     context (shader-conditional-statement-condition statement)
     (list 'selection-merge merge-label 'none))
    (emit-shader-instruction
     context (shader-conditional-statement-condition statement)
     (list 'branch-conditional condition body-label merge-label))
    (begin-shader-basic-block context body-label)
    (dolist (child (shader-conditional-statement-statements statement))
      (lower-shader-statement context child))
    (emit-shader-instruction context nil (list 'branch merge-label))
    (begin-shader-basic-block context merge-label)))

(defmethod lower-shader-statement
    (context (statement shader-mesh-output-counts))
  (let ((vertex-expression
          (shader-mesh-output-vertex-count statement)))
    (emit-shader-instruction
     context vertex-expression
     (list 'set-mesh-outputs-ext
           (lower-shader-expression context vertex-expression)
           (lower-shader-expression
            context (shader-mesh-output-primitive-count statement))))))

(defun lower-shader-array-store
    (context declaration index expression storage-class)
  (let ((pointer
          (fresh-shader-id
           context
           (format nil "~A-ELEMENT-POINTER"
                   (shader-object-name declaration)))))
    (emit-shader-instruction
     context expression
     (list pointer 'access-chain
           (ensure-pointer-type-id
            context storage-class (shader-declaration-type declaration))
           (gethash declaration (context-variable-ids context))
           index))
    (emit-shader-instruction
     context expression
     (list 'store pointer (lower-shader-expression context expression)))))

(defmethod lower-shader-statement
    (context (statement shader-mesh-vertex-store))
  (let ((index
          (lower-shader-expression
           context (shader-mesh-vertex-store-index statement))))
    (dolist (pair (shader-mesh-vertex-store-values statement))
      (lower-shader-array-store context (car pair) index (cdr pair) 'output))))

(defmethod lower-shader-statement
    (context (statement shader-mesh-primitive-store))
  (let* ((index-expression (shader-mesh-primitive-store-index statement))
         (index (lower-shader-expression context index-expression))
         (indices-expression (shader-mesh-primitive-store-indices statement))
         (indices-pointer (fresh-shader-id context 'primitive-indices-pointer)))
    (emit-shader-instruction
     context indices-expression
     (list indices-pointer 'access-chain
           (ensure-pointer-type-id
            context 'output (shader-expression-type indices-expression))
           (context-mesh-primitive-indices-variable context)
           index))
    (emit-shader-instruction
     context indices-expression
     (list 'store indices-pointer
           (lower-shader-expression context indices-expression)))
    (dolist (pair (shader-mesh-primitive-store-values statement))
      (lower-shader-array-store context (car pair) index (cdr pair) 'output))))

(defmethod lower-shader-statement
    (context (statement shader-task-payload-store))
  (let* ((field (shader-task-payload-store-field statement))
         (expression (shader-task-payload-store-value statement))
         (pointer
           (fresh-shader-id
            context
            (format nil "~A-POINTER" (shader-object-name field))))
         (indices
           (append
            (list (ensure-shader-uint-constant
                   context (shader-task-payload-field-index field)))
            (when (shader-task-payload-store-index statement)
              (list
               (lower-shader-expression
                context (shader-task-payload-store-index statement)))))))
    (emit-shader-instruction
     context expression
     (list* pointer 'access-chain
            (ensure-pointer-type-id
             context 'task-payload-workgroup-ext
             (shader-declaration-type field))
            (context-task-payload-variable context)
            indices))
    (emit-shader-instruction
     context expression
     (list 'store pointer (lower-shader-expression context expression)))))

(defmethod lower-shader-statement
    (context (statement shader-emit-mesh-workgroups))
  (let* ((expression (shader-emit-mesh-workgroups-counts statement))
         (counts (lower-shader-expression context expression))
         (type (ensure-shader-type-id context :uint))
         (components
           (loop for component below 3
                 collect
                 (let ((id (fresh-shader-id context 'mesh-group-count)))
                   (emit-shader-instruction
                    context expression
                    (list id 'composite-extract type counts component))
                   id))))
    (emit-shader-instruction
     context expression
     (append (list 'emit-mesh-tasks-ext) components
             (when (context-task-payload-variable context)
               (list (context-task-payload-variable context)))))))

(defun shader-entry-execution-model (stage)
  (ecase stage
    (:vertex 'vertex)
    (:fragment 'fragment)
    (:compute 'gl-compute)
    (:task 'task-ext)
    (:mesh 'mesh-ext)))

(defun mesh-topology-execution-mode (topology)
  (ecase topology
    (:points 'output-points)
    (:lines 'output-lines-ext)
    (:triangles 'output-triangles-ext)))

(defun shader-execution-modes (specification main-id)
  (let ((stage (shader-specification-stage specification)))
    (case stage
      (:fragment
       (list (make-instance 'spir-v-execution-mode
                            :function main-id :name 'origin-upper-left)))
      ((:task :mesh)
       (append
        (list
         (make-instance
          'spir-v-execution-mode
          :function main-id :name 'local-size
          :literals (shader-specification-workgroup-size specification)))
        (when (eq stage :mesh)
          (let ((mesh-output
                  (shader-specification-mesh-output specification)))
            (list
             (make-instance
              'spir-v-execution-mode
              :function main-id
              :name (mesh-topology-execution-mode
                     (shader-mesh-output-topology mesh-output)))
             (make-instance
              'spir-v-execution-mode
              :function main-id :name 'output-vertices
              :literals (list
                         (shader-mesh-output-max-vertices mesh-output)))
             (make-instance
              'spir-v-execution-mode
              :function main-id :name 'output-primitives-ext
              :literals (list
                         (shader-mesh-output-max-primitives mesh-output)))))))))))

(defun compile-shader-specification (specification)
  "Lower SPECIFICATION and retain bidirectional expression/instruction links."
  (check-type specification shader-specification)
  (let* ((context (make-instance 'shader-lowering-context))
         (void-id (ensure-void-type-id context))
         (main-id (reserve-shader-id context "MAIN"))
         (storage-buffers
           (remove-if-not (lambda (resource)
                            (typep resource 'shader-storage-buffer))
                          (shader-specification-resources specification)))
         (entry-id (reserve-shader-id context "ENTRY"))
         (function-type-id (reserve-shader-id context "FUNCTION-TYPE")))
    (setf (context-stage context) (shader-specification-stage specification))
    (begin-shader-basic-block context entry-id)
    (append-context-form 'type-declarations context
                         (list function-type-id 'type-function void-id))
    (dolist (declaration (shader-specification-inputs specification))
      (if (eq :workgroup-size (shader-interface-built-in declaration))
          (register-workgroup-size-value
           context declaration
           (shader-specification-workgroup-size specification))
          (register-shader-variable context declaration)))
    (dolist (declaration
             (append (shader-specification-outputs specification)
                     (shader-specification-resources specification)))
      (register-shader-variable context declaration))
    (when (shader-specification-task-payload specification)
      (register-task-payload
       context (shader-specification-task-payload specification)))
    (when (shader-specification-mesh-output specification)
      (register-mesh-outputs
       context (shader-specification-mesh-output specification)))
    ;; LET* is part of the language contract, not merely pretty syntax.  Emit
    ;; binding computations in source order so the resulting basic block reads
    ;; alongside the specification and retains ordinary Lisp evaluation order.
    (dolist (binding (shader-specification-bindings specification))
      (let ((expression (shader-binding-expression binding)))
        (when (shader-expression-materialized-p expression)
          (lower-shader-expression context expression))))
    (dolist (statement (shader-specification-statements specification))
      (lower-shader-statement context statement))
    (unless (eq :task (shader-specification-stage specification))
      (emit-shader-instruction context nil '(return)))
    (let* ((module
             (make-instance
              'spir-v-module
              :version
              (if (member (shader-specification-stage specification)
                          '(:task :mesh))
                  #x00010400
                  #x00010000)
              :capabilities
              (append
               '(shader)
               (when (gethash (find-shader-type :uint64)
                              (context-type-ids context))
                 '(int64))
               (when (member (shader-specification-stage specification)
                             '(:task :mesh))
                 '(mesh-shading-ext)))
              :extensions
              (append
               (when (member (shader-specification-stage specification)
                             '(:task :mesh))
                 '("SPV_EXT_mesh_shader"))
               ;; The StorageBuffer storage class is core from SPIR-V 1.3;
               ;; the 1.0 modules of ordinary stages must ask for it.
               (when (and storage-buffers
                          (not (member (shader-specification-stage
                                        specification)
                                       '(:task :mesh))))
                 '("SPV_KHR_storage_buffer_storage_class")))
              :extended-instruction-imports
              (context-extended-instruction-imports context)
              :entry-points
              (list (make-instance
                     'spir-v-entry-point
                     :execution-model
                     (shader-entry-execution-model
                      (shader-specification-stage specification))
                     :function main-id
                     :interfaces (context-interfaces context)))
              :execution-modes (shader-execution-modes specification main-id)
              :annotations (context-annotations context)
              :global-declarations
              (append (context-type-declarations context)
                      (context-constant-declarations context)
                      (context-variable-declarations context))
              :function-definitions
              (list
               (make-instance
                'spir-v-function-definition
                :result-id main-id :return-type void-id
                :function-type function-type-id
                :basic-blocks
                (context-basic-blocks context)))))
           (lowering
             (make-instance
              'shader-lowering
              :specification specification
              :module module
              :expression-instructions
              (context-expression-instructions context)
              :instruction-expressions
              (context-instruction-expressions context))))
      lowering)))

(defmethod lower-shader-specification
    ((target (eql :spir-v)) (specification shader-specification))
  (declare (ignore target))
  (compile-shader-specification specification))

(defun shader-module (specification)
  (shader-lowering-module (compile-shader-specification specification)))

(defun assemble-shader-specification (specification)
  (assemble-spir-v-module (shader-module specification)))
