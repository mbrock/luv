(defpackage #:luv/spir-v/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:spv #:luv.spir-v))
  ;; Shader operators are identified by symbol, so specification bodies
  ;; written here must use the shader language's own words.
  (:import-from #:luv.spir-v
                #:dot #:sample #:mix #:vec2 #:vec3 #:vec4 #:swizzle
                #:clamp #:smoothstep #:normalize
                #:set-output))

(in-package #:luv/spir-v/tests)

(defun binding-named (name specification)
  (find name (spv:shader-specification-bindings specification)
        :key #'spv:shader-object-name
        :test (lambda (left right)
                (string-equal (symbol-name left) (symbol-name right)))))

(defun form-names (form)
  (cond ((symbolp form) (string-downcase (symbol-name form)))
        ((consp form) (mapcar #'form-names form))
        (t form)))

(defgeneric shader-method-probe (role stage))

(spv:define-shader-method shader-method-probe shader-method-probe
    ((role (eql :probe)) (stage (eql :fragment)))
    (:stage :fragment
     :outputs ((color :vec4 :location 0)))
  (set-output color (vec4 0.1 0.2 0.3 1.0)))

(deftest shader-method-redefinition-is-observable-and-coalesced
  (let* ((generic-function (fdefinition 'shader-method-probe))
         (dependent
           (spv:make-shader-definition-dependent
            generic-function '(:probe :fragment))))
    (unwind-protect
         (progn
           (ok (not (spv:shader-definition-change-pending-p dependent)))
           (eval
            '(spv:define-shader-method
                 shader-method-probe shader-method-probe
                 ((role (eql :probe)) (stage (eql :fragment)))
                 (:stage :fragment
                  :outputs ((color :vec4 :location 0)))
               (set-output color (vec4 0.8 0.4 0.2 1.0))))
           (ok (= 1 (length (closer-mop:generic-function-methods
                             generic-function))))
           (ok (spv:shader-definition-change-pending-p dependent))
           (multiple-value-bind (revision event)
               (spv:shader-definition-change-snapshot dependent)
             ;; Replacement emits REMOVE-METHOD and ADD-METHOD on the pinned
             ;; SBCL/Closer-MOP stack.  Consumers see their coalesced revision.
             (ok (>= revision 2))
             (ok (eq (first event) 'add-method))
             (ok (equal
                  (form-names
                   (spv:shader-expression-form
                    (spv:shader-assignment-value
                     (first
                      (spv:shader-specification-statements
                       (shader-method-probe :probe :fragment))))))
                  '("vec4" 0.8 0.4 0.2 1.0)))
             (spv:acknowledge-shader-definition-change dependent revision)
             (ok (not (spv:shader-definition-change-pending-p dependent)))))
      (spv:release-shader-definition-dependent dependent))))

(deftest shader-source-is-a-typed-clos-graph
  (let* ((specification (spv:block-world-fragment-specification))
         (sun (binding-named 'sun specification))
         (directional-light
           (binding-named 'directional-light specification))
         (light (binding-named 'light specification))
         (lit (binding-named 'lit specification))
         (fogged (binding-named 'fogged specification)))
    (ok (typep specification 'spv:shader-specification))
    (ok (eq (spv:shader-specification-stage specification) :fragment))
    (ok (= (length (spv:shader-specification-inputs specification)) 3))
    (ok (= (length (spv:shader-specification-resources specification)) 2))
    (ok (typep (spv:shader-binding-expression sun) 'spv:shader-call))
    (ok (spv:shader-type=
         (spv:shader-expression-type (spv:shader-binding-expression sun))
         :vec3))
    (ok (equal
         (form-names
          (spv:shader-expression-form
           (spv:shader-binding-expression directional-light)))
         '("mix" "shade-light" "sun-light" "hemisphere")))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression light))
         :vec3))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression lit)))
               '("*" "albedo" "light")))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression fogged)))
               '("mix" "sky" "lit" "fog")))
    (ok (> (length (spv:shader-specification-expressions specification))
           (length (spv:shader-specification-bindings specification))))))

(deftest block-vertex-source-is-a-typed-clos-graph-with-an-explicit-abi
  (let* ((specification (spv:block-world-vertex-specification))
         (resource (first (spv:shader-specification-resources specification)))
         (clip-position
           (find 'clip-position
                 (spv:shader-specification-outputs specification)
                 :key #'spv:shader-object-name
                 :test (lambda (left right)
                         (string-equal (symbol-name left)
                                       (symbol-name right)))))
         (fog-factor (binding-named 'fog-factor specification)))
    (ok (typep specification 'spv:shader-specification))
    (ok (eq (spv:shader-specification-stage specification) :vertex))
    (ok (= (length (spv:shader-specification-inputs specification)) 3))
    (ok (= (length (spv:shader-specification-outputs specification)) 4))
    (ok (eq (spv:shader-interface-built-in clip-position) :position))
    (ok (typep resource 'spv:shader-uniform-block))
    (ok (= (spv:shader-resource-binding resource) 2))
    (ok (equal (mapcar (lambda (member)
                         (string-downcase
                          (symbol-name (spv:shader-object-name member))))
                       (spv:shader-uniform-block-members resource))
               '("camera-vector" "right-vector" "up-vector" "forward-vector"
                 "projection-vector" "fog-vector")))
    (ok (equal (mapcar #'spv:shader-uniform-member-offset
                       (spv:shader-uniform-block-members resource))
               '(0 16 32 48 64 80)))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression fog-factor)))
               '("clamp" ("-" 1.0 "fog-distance-squared") 0.0 1.0)))))

(deftest block-vertex-uniform-members-retain-access-chain-provenance
  (let* ((lowering (spv:block-world-vertex-lowering))
         (specification (spv:shader-lowering-specification lowering))
         (camera (binding-named 'camera specification))
         (reference
           (first (spv:shader-call-operands
                   (spv:shader-binding-expression camera))))
         (instructions
           (gethash reference
                    (spv:shader-lowering-expression-instructions lowering)))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    (ok (find "ACCESS-CHAIN" names :test #'string=))
    (ok (find "LOAD" names :test #'string=))))

(deftest uniform-blocks-do-not-pretend-to-implement-general-packing
  (ok (signals
       (spv::parse-shader-specification
        'test-uniform-layout
        '(:stage :vertex
          :resources ((state :uniform-block :binding 0
                       :members ((unsupported :float))))
          :outputs ((position :vec4 :built-in :position)))
        '((set-output position (vec4 0.0 0.0 0.0 1.0))))
       'spv:shader-language-error)))

(deftest lowering-retains-expression-to-ssa-provenance
  (let* ((lowering (spv:block-world-fragment-lowering))
         (specification (spv:shader-lowering-specification lowering))
         (lit-expression
           (spv:shader-binding-expression (binding-named 'lit specification)))
         (instructions
           (gethash lit-expression
                    (spv:shader-lowering-expression-instructions lowering))))
    (ok (typep (spv:shader-lowering-module lowering) 'spv:spir-v-module))
    (ok instructions)
    (ok (every (lambda (instruction) (typep instruction 'spv:instruction))
               instructions))
    (ok (find "F-MUL" instructions
              :key (lambda (instruction)
                     (symbol-name (spv:instruction-name instruction)))
              :test #'string=))
    (ok (some (lambda (instruction)
                (member lit-expression
                        (gethash instruction
                                 (spv:shader-lowering-instruction-expressions
                                  lowering))
                        :test #'eq))
              instructions))))

(deftest constants-and-reused-loads-retain-occurrence-provenance
  (let* ((specification
           (spv:parse-shader-specification
            'reuse-input
            '(:stage :fragment
              :inputs ((value :float :location 0))
              :outputs ((color :float :location 0)))
            '((let* ((twice (+ value value)))
                (set-output color twice)))))
         (lowering (spv:compile-shader-specification specification))
         (call (spv:shader-binding-expression
                (binding-named 'twice specification)))
         (references (spv:shader-call-operands call))
         (left-instructions
           (gethash (first references)
                    (spv:shader-lowering-expression-instructions lowering)))
         (right-instructions
           (gethash (second references)
                    (spv:shader-lowering-expression-instructions lowering)))
         (block-specification (spv:block-world-fragment-specification))
         (sun (binding-named 'sun block-specification))
         (block-lowering (spv:block-world-fragment-lowering))
         (literal (first (spv:shader-call-operands
                          (spv:shader-binding-expression sun))))
         (constant-instructions
           (gethash literal
                    (spv:shader-lowering-expression-instructions
                     block-lowering))))
    (ok (= (length left-instructions) 1))
    (ok (eq (first left-instructions) (first right-instructions)))
    (ok (= (length constant-instructions) 1))
    (ok (string-equal
         (symbol-name
          (spv:instruction-name (first constant-instructions)))
         "constant"))))

(deftest vector-scalar-division-lowers-through-a-reciprocal
  (let* ((specification
           (spv:parse-shader-specification
            'vector-division
            '(:stage :fragment
              :inputs ((value :vec3 :location 0)
                       (scale :float :location 1))
              :outputs ((color :vec3 :location 0)))
            '((let* ((quotient (/ value scale)))
                (set-output color quotient)))))
         (instructions
           (spv:lower-spir-v
            (spv:shader-lowering-module
             (spv:compile-shader-specification specification))))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    (ok (find "F-DIV" names :test #'string=))
    (ok (find "VECTOR-TIMES-SCALAR" names :test #'string=))
    (ok (> (length (spv:assemble-shader-specification specification)) 5))))

(deftest shader-lowering-is-deterministic-and-assemblable
  (flet ((forms ()
           (mapcar #'spv:instruction-form
                   (spv:lower-spir-v
                    (spv:block-world-fragment-module)))))
    (ok (equal (forms) (forms)))
    (let ((words (spv:block-world-fragment-shader)))
      (ok (> (length words) 5))
      (ok (= (aref words 0) #x07230203)))
    (let ((vertex (spv:block-world-crosshair-vertex-shader))
          (fragment (spv:block-world-crosshair-fragment-shader)))
      (ok (> (length vertex) 5))
      (ok (> (length fragment) 5))
      (ok (= (aref vertex 0) #x07230203))
      (ok (= (aref fragment 0) #x07230203)))
    (let ((vertex (spv:block-world-vertex-shader)))
      (ok (> (length vertex) 5))
      (ok (= (aref vertex 0) #x07230203)))))

(deftest extended-math-lowers-through-one-shared-import-in-layout-order
  (let* ((specification
           (spv:parse-shader-specification
            'extended-math
            '(:stage :fragment
              :inputs ((direction :vec3 :location 0)
                       (level :float :location 1))
              :outputs ((color :vec4 :location 0)))
            '((let* ((unit (normalize direction))
                     (glow (smoothstep 0.9 1.0 level))
                     (lit (max 0.0 (dot unit (vec3 0.0 1.0 0.0))))
                     (shaped (expt (clamp (+ glow lit) 0.0 1.0) 2.2))
                     (softened (sqrt (abs shaped)))
                     (rgb (* unit softened)))
                (set-output color (vec4 rgb (min 1.0 softened)))))))
         (module (spv:shader-lowering-module
                  (spv:compile-shader-specification specification)))
         (instructions (spv:lower-spir-v module))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    ;; Every extended operator in one module shares a single import, which
    ;; sits in SPIR-V logical layout between capability and memory model.
    (ok (= 1 (length
              (spv:spir-v-module-extended-instruction-imports module))))
    (ok (= 1 (count "EXT-INST-IMPORT" names :test #'string=)))
    (ok (< (position "CAPABILITY" names :test #'string=)
           (position "EXT-INST-IMPORT" names :test #'string=)
           (position "MEMORY-MODEL" names :test #'string=)))
    (ok (= 8 (count "EXT-INST" names :test #'string=)))
    (ok (> (length (spv:assemble-shader-specification specification)) 5))
    ;; Deterministic lowering, and no import where no extended math occurs.
    (flet ((forms ()
             (mapcar #'spv:instruction-form
                     (spv:lower-spir-v
                      (spv:shader-lowering-module
                       (spv:compile-shader-specification specification))))))
      (ok (equal (forms) (forms))))
    (ok (null (spv:spir-v-module-extended-instruction-imports
               (spv:block-world-fragment-module))))))

(deftest extended-math-signatures-are-explicit-contracts
  (flet ((failure-reason (body)
           (handler-case
               (progn
                 (spv:parse-shader-specification
                  'bad-extended-math
                  '(:stage :fragment
                    :inputs ((value :vec3 :location 0)
                             (scale :float :location 1))
                    :outputs ((color :vec3 :location 0)))
                  body)
                 nil)
             (spv:shader-language-error (condition)
               (spv:shader-language-error-reason condition)))))
    ;; Vector values with scalar bounds are not silently splatted.
    (ok (eq (failure-reason '((set-output color (clamp value 0.0 1.0))))
            :incompatible-arithmetic-types))
    (ok (eq (failure-reason '((let* ((unit (normalize scale)))
                                (set-output color (* value unit)))))
            :invalid-normalize))
    (ok (eq (failure-reason '((set-output color (min value))))
            :wrong-operand-count))
    (ok (eq (failure-reason '((set-output color (expt value))))
            :wrong-operand-count))))

(deftest extended-operations-retain-expression-provenance
  (let* ((specification
           (spv:parse-shader-specification
            'clamped-level
            '(:stage :fragment
              :inputs ((level :float :location 0))
              :outputs ((color :float :location 0)))
            '((let* ((held (clamp level 0.0 1.0)))
                (set-output color held)))))
         (lowering (spv:compile-shader-specification specification))
         (call (spv:shader-binding-expression
                (binding-named 'held specification)))
         (instructions
           (gethash call
                    (spv:shader-lowering-expression-instructions lowering))))
    (ok instructions)
    (ok (find "EXT-INST" instructions
              :key (lambda (instruction)
                     (symbol-name (spv:instruction-name instruction)))
              :test #'string=))
    (ok (some (lambda (instruction)
                (member call
                        (gethash instruction
                                 (spv:shader-lowering-instruction-expressions
                                  lowering))
                        :test #'eq))
              instructions))))

(deftest shader-diagnostics-name-the-source-failure
  (let ((unknown-reason
          (handler-case
              (progn
                (spv:parse-shader-specification
                 'bad-shader
                 '(:stage :fragment
                   :outputs ((color :vec4 :location 0)))
                 '((set-output color missing-name)))
                nil)
            (spv:shader-language-error (condition)
              (spv:shader-language-error-reason condition))))
        (type-reason
          (handler-case
              (progn
                (spv:parse-shader-specification
                 'bad-shader
                 '(:stage :fragment
                   :inputs ((scalar :float :location 0))
                   :outputs ((color :vec4 :location 0)))
                 '((set-output color scalar)))
                nil)
            (spv:shader-language-error (condition)
              (spv:shader-language-error-reason condition)))))
    (ok (eq unknown-reason :unknown-name))
    (ok (eq type-reason :output-type-mismatch))))
