;;; SPIR-V lowering for luv's typed shader graph.
;;;
;;; The source language and its generic lowering protocols live in LUV.SHADER;
;;; this sibling owns only the SPIR-V construction context and result product.

(in-package #:luv.spir-v)

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
   (bool-vector-type-ids :initform (make-hash-table :test #'eql)
                         :accessor context-bool-vector-type-ids)
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

(defun ensure-bool-vector-type-id (context count)
  "The OpTypeVector of COUNT booleans: a vector select's condition."
  (or (gethash count (context-bool-vector-type-ids context))
      (let ((id (reserve-shader-id context (format nil "BVEC~D" count))))
        (setf (gethash count (context-bool-vector-type-ids context)) id)
        (append-context-form
         'type-declarations context
         (list id 'type-vector (ensure-bool-type-id context) count))
        id)))

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
       (when (shader-interface-interpolation declaration)
         (append-context-form
          'annotations context
          (list 'decorate variable-id
                (shader-interface-interpolation declaration))))
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
    (when (shader-interface-interpolation declaration)
      (append-context-form
       'annotations context
       (list 'decorate variable-id
             (shader-interface-interpolation declaration))))
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

(defmethod lower-shader-call
    ((operator (eql 'luv.shader:dot)) context expression)
  (emit-value-instruction
   context expression :float 'dot
   (mapcar (lambda (operand) (lower-shader-expression context operand))
           (shader-call-operands expression))))

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
  (let* ((type (shader-expression-type expression))
         (count (shader-type-component-count (find-shader-type type)))
         (condition
           (lower-shader-expression
            context (lang:arithmetic-conditional-condition expression)))
         (consequent
           (lower-shader-expression
            context (lang:arithmetic-conditional-consequent expression)))
         (alternative
           (lower-shader-expression
            context (lang:arithmetic-conditional-alternative expression))))
    ;; OpSelect took one boolean per component until SPIR-V 1.4 relaxed it,
    ;; and a stage's module version is not this expression's business, so a
    ;; scalar condition choosing between vectors is splatted rather than
    ;; leaned on: the wider form is valid at every version.
    (when (and count (> count 1))
      (let ((splat (fresh-shader-id context "CONDITION-VECTOR")))
        (emit-shader-instruction
         context nil
         (list* splat 'composite-construct
                (ensure-bool-vector-type-id context count)
                (make-list count :initial-element condition)))
        (setf condition splat)))
    (emit-value-instruction
     context expression type 'select
     (list condition consequent alternative))))

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
