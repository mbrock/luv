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
(defgeneric shader-abstraction-method-probe (role stage))

(spv:define-shader-method shader-method-probe shader-method-probe
    ((role (eql :probe)) (stage (eql :fragment)))
    (:stage :fragment
     :outputs ((color :vec4 :location 0)))
  (set-output color (vec4 0.1 0.2 0.3 1.0)))

(spv:define-shader-method
    shader-abstraction-method-probe shader-abstraction-method-probe
    ((role (eql :abstraction-probe)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((receiver :float :location 0)
              (depth :float :location 1)
              (bias :float :location 2))
     :outputs ((visibility :float :location 0)))
  (let* ((visible (test-shadow-rewrite receiver depth bias)))
    (set-output visibility visible)))

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
         (sun-direction (binding-named 'sun-direction specification))
         (sun-visibility (binding-named 'sun-visibility specification))
         (direct-shadow (binding-named 'direct-shadow specification))
         (sky-light (binding-named 'sky-light specification))
         (reflected (binding-named 'reflected specification))
         (radiance (binding-named 'radiance specification))
         (fogged (binding-named 'fogged specification)))
    (ok (typep specification 'spv:shader-specification))
    (ok (eq (spv:shader-specification-stage specification) :fragment))
    (ok (= (length (spv:shader-specification-inputs specification)) 6))
    (ok (= (length (spv:shader-specification-resources specification)) 5))
    (ok (typep (spv:shader-binding-expression sun-direction)
               'spv:shader-call))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression sun-direction))
         :vec3))
    (ok (equal
         (form-names
          (spv:shader-expression-form
           (spv:shader-binding-expression sun-visibility)))
         '("smoothstep" 0.9 1.0 "sky-input")))
    (ok (equal
         (form-names
          (spv:shader-expression-form
           (spv:shader-binding-expression direct-shadow)))
         '("mix" 1.0 "shadow-sample" "shadow-in-bounds")))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression sky-light))
         :vec3))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression reflected)))
               '("*" "albedo"
                 ("+" "sky-light" "sun-light" "local-light"))))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression radiance)))
               '("+" "reflected" ("*" "albedo" "emission-input"))))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression fogged)))
               '("mix" "radiance" "fog-color" "fog-input")))
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
         (fog-progress (binding-named 'fog-progress specification))
         (fog-amount (binding-named 'fog-amount specification)))
    (ok (typep specification 'spv:shader-specification))
    (ok (eq (spv:shader-specification-stage specification) :vertex))
    (ok (= (length (spv:shader-specification-inputs specification)) 4))
    (ok (= (length (spv:shader-specification-outputs specification)) 7))
    (ok (eq (spv:shader-interface-built-in clip-position) :position))
    (ok (typep resource 'spv:shader-uniform-block))
    (ok (= (spv:shader-resource-binding resource) 2))
    (ok (equal (mapcar (lambda (member)
                         (string-downcase
                          (symbol-name (spv:shader-object-name member))))
                       (spv:shader-uniform-block-members resource))
               '("camera-vector" "right-vector" "up-vector" "forward-vector"
                 "projection-vector" "fog-vector"
                 "sun-vector" "sun-color-vector" "zenith-vector"
                 "horizon-vector" "ambient-vector" "fog-color-vector"
                 "shadow-control-vector" "shadow-filter-vector"
                 "shadow-row-x" "shadow-row-y"
                 "shadow-row-z" "shadow-row-w")))
    (ok (equal (mapcar #'spv:shader-uniform-member-offset
                       (spv:shader-uniform-block-members resource))
               '(0 16 32 48 64 80 96 112 128 144 160 176
                 192 208 224 240 256 272)))
    (ok (= (spv:shader-uniform-block-byte-size resource) 288))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression fog-progress)))
               '("clamp" ("/" ("-" "view-z" "fog-near") "fog-span") 0.0 1.0)))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression fog-amount)))
               '("*" "fog-progress" "fog-progress")))))

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

(deftest block-shadow-vertex-is-a-light-space-depth-shader
  (let* ((specification (spv:block-world-shadow-vertex-specification))
         (resource (first (spv:shader-specification-resources specification)))
         (clip-x (binding-named 'clip-x specification))
         (clip-position
           (first (spv:shader-specification-statements specification))))
    (ok (eq (spv:shader-specification-stage specification) :vertex))
    (ok (= (length (spv:shader-specification-inputs specification)) 1))
    (ok (= (length (spv:shader-specification-outputs specification)) 1))
    (ok (typep resource 'spv:shader-uniform-block))
    (ok (= (spv:shader-resource-binding resource) 2))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression clip-x))
         :float))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression clip-x)))
               '("dot" "shadow-row-x" "world")))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-assignment-value clip-position))
         :vec4))
    (ok (> (length (spv:block-world-shadow-vertex-shader)) 5))))

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
         (reflected-expression
           (spv:shader-binding-expression
            (binding-named 'reflected specification)))
         (instructions
           (gethash reflected-expression
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
                (member reflected-expression
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
         (torch-color (binding-named 'torch-color block-specification))
         (block-lowering
           (spv:compile-shader-specification block-specification))
         (literal (first (spv:shader-call-operands
                          (spv:shader-binding-expression torch-color))))
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

(deftest depth-texture-sampling-feeds-ordinary-float-math
  (let* ((specification
           (spv:parse-shader-specification
            'depth-sample
            '(:stage :fragment
              :inputs ((uv :vec2 :location 0)
                       (receiver-depth :float :location 1))
              :outputs ((visibility :float :location 0))
              :resources ((shadow-map :depth-texture-2d :binding 0)
                          (shadow-sampler :sampler :binding 1)))
            '((let* ((stored-depth (sample shadow-map shadow-sampler uv))
                     (depth-lane (swizzle stored-depth :x))
                     (visible (spv:step receiver-depth depth-lane)))
                (set-output visibility visible)))))
         (stored-depth (binding-named 'stored-depth specification))
         (depth-lane (binding-named 'depth-lane specification))
         (visible (binding-named 'visible specification))
         (module (spv:shader-lowering-module
                  (spv:compile-shader-specification specification)))
         (instructions (spv:lower-spir-v module))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression stored-depth))
         :vec4))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression depth-lane))
         :float))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression visible))
         :float))
    (ok (find "IMAGE-SAMPLE-IMPLICIT-LOD" names :test #'string=))
    (ok (find "EXT-INST" names :test #'string=))
    (ok (> (length (spv:assemble-shader-specification specification)) 5))))

(deftest shadow-visibility-is-a-source-abstraction-over-core-math
  (ok (spv:shader-abstraction-p 'spv:shadow-visibility))
  (ok (not (spv:shader-operator-p 'spv:shadow-visibility)))
  (let* ((specification
           (spv:parse-shader-specification
            'shadow-visibility-probe
            '(:stage :fragment
              :inputs ((uv :vec2 :location 0)
                       (receiver-depth :float :location 1)
                       (texel-size :vec2 :location 2)
                       (bias :float :location 3)
                       (radius :float :location 4))
              :outputs ((visibility :float :location 0))
              :resources ((shadow-map :depth-texture-2d :binding 0)
                          (shadow-sampler :sampler :binding 1)))
            '((let* ((visible (spv:shadow-visibility
                               shadow-map shadow-sampler uv
                               receiver-depth texel-size bias radius)))
                (set-output visibility visible)))))
         (visible (binding-named 'visible specification))
         (expression (spv:shader-binding-expression visible))
         (module (spv:shader-lowering-module
                  (spv:compile-shader-specification specification)))
         (instructions (spv:lower-spir-v module))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    (ok (typep expression 'spv:shader-call))
    (ok (eq (spv:shader-call-operator expression) '/))
    (ok (= 17 (count "IMAGE-SAMPLE-IMPLICIT-LOD" names :test #'string=)))
    (ok (find "EXT-INST" names :test #'string=))
    (ok (> (length (spv:assemble-shader-specification specification)) 5))))

(deftest shader-abstraction-redefinition-affects-fresh-parses
  (labels ((install-subtraction ()
             (eval
              '(spv:define-shader-abstraction test-shadow-rewrite
                   (receiver depth bias)
                 `(spv:step (- ,receiver ,bias) ,depth))))
           (install-addition ()
             (eval
              '(spv:define-shader-abstraction test-shadow-rewrite
                   (receiver depth bias)
                 `(spv:step (+ ,receiver ,bias) ,depth))))
           (install-bad-expansion ()
             (eval
              '(spv:define-shader-abstraction test-shadow-rewrite
                   (receiver depth bias)
                 `(spv:step ,receiver ,depth ,bias))))
           (parse-probe ()
             (spv:parse-shader-specification
              'shadow-rewrite-probe
              '(:stage :fragment
                :inputs ((receiver :float :location 0)
                         (depth :float :location 1)
                         (bias :float :location 2))
                :outputs ((visibility :float :location 0)))
              '((let* ((visible (test-shadow-rewrite receiver depth bias)))
                  (set-output visibility visible)))))
           (visible-form (specification)
             (spv:shader-expression-form
              (spv:shader-binding-expression
               (binding-named 'visible specification)))))
    (unwind-protect
           (progn
           (install-subtraction)
           (let* ((subtracted (parse-probe))
                  (subtracted-form (visible-form subtracted))
                  (method-subtracted-form
                    (visible-form
                     (shader-abstraction-method-probe
                      :abstraction-probe :fragment)))
                  (revision (spv:shader-abstraction-revision)))
             (install-addition)
             (let ((added (parse-probe))
                   (method-added
                     (shader-abstraction-method-probe
                      :abstraction-probe :fragment)))
               (ok (> (spv:shader-abstraction-revision) revision))
               (ok (equal (form-names subtracted-form)
                          '("step" ("-" "receiver" "bias") "depth")))
               (ok (equal (form-names (visible-form added))
                          '("step" ("+" "receiver" "bias") "depth")))
               (ok (equal (form-names method-subtracted-form)
                          '("step" ("-" "receiver" "bias") "depth")))
               (ok (equal (form-names (visible-form method-added))
                          '("step" ("+" "receiver" "bias") "depth")))
               (install-bad-expansion)
               (ok (signals (parse-probe) 'spv:shader-language-error))
               (ok (equal (form-names subtracted-form)
                          '("step" ("-" "receiver" "bias") "depth"))))))
      (install-subtraction))))

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
      (ok (= (aref vertex 0) #x07230203)))
    (let ((vertex (spv:block-world-sky-vertex-shader))
          (fragment (spv:block-world-sky-fragment-shader)))
      (ok (> (length vertex) 5))
      (ok (> (length fragment) 5))
      (ok (= (aref vertex 0) #x07230203))
      (ok (= (aref fragment 0) #x07230203)))
    (let ((vertex (spv:block-world-shadow-vertex-shader)))
      (ok (> (length vertex) 5))
      (ok (= (aref vertex 0) #x07230203)))))

(deftest every-scene-stage-declares-the-same-frame-uniform-block
  ;; Identical member order and offsets at binding 2 are the ABI contract
  ;; which lets one buffer feed the vertex and fragment halves of both the
  ;; block material and the sky.
  (flet ((frame-block (specification)
           (find-if (lambda (resource)
                      (typep resource 'spv:shader-uniform-block))
                    (spv:shader-specification-resources specification)))
         (member-layout (block)
           (mapcar (lambda (member)
                     (list (string-downcase
                            (symbol-name (spv:shader-object-name member)))
                           (spv:shader-uniform-member-offset member)))
                   (spv:shader-uniform-block-members block))))
    (let* ((specifications
             (list (spv:block-world-vertex-specification)
                   (spv:block-world-fragment-specification)
                   (spv:block-world-sky-vertex-specification)
                   (spv:block-world-sky-fragment-specification)
                   (spv:block-world-shadow-vertex-specification)))
           (blocks (mapcar #'frame-block specifications))
           (reference (member-layout (first blocks))))
      (ok (every (lambda (block) (typep block 'spv:shader-uniform-block))
                 blocks))
      (ok (every (lambda (block)
                   (= (spv:shader-resource-binding block) 2))
                 blocks))
      (ok (every (lambda (block)
                   (equal (member-layout block) reference))
                 (rest blocks)))
      (ok (every (lambda (block)
                   (= (spv:shader-uniform-block-byte-size block) 288))
                 blocks)))))

(deftest the-sky-material-is-image-mathematics-over-environment-lanes
  (let* ((vertex (spv:block-world-sky-vertex-specification))
         (fragment (spv:block-world-sky-fragment-specification))
         (fragment-module (spv:block-world-sky-fragment-module))
         (ray (binding-named 'ray vertex))
         (unit (binding-named 'unit fragment))
         (disc (binding-named 'disc fragment)))
    (ok (eq (spv:shader-specification-stage vertex) :vertex))
    (ok (eq (spv:shader-specification-stage fragment) :fragment))
    (ok (spv:shader-type=
         (spv:shader-expression-type (spv:shader-binding-expression ray))
         :vec3))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression unit)))
               '("normalize" "ray-input")))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression disc)))
               '("smoothstep" "disc-outer" "disc-inner" "alignment")))
    ;; All the fragment's extended mathematics shares one import.
    (ok (= 1 (length (spv:spir-v-module-extended-instruction-imports
                      fragment-module))))))

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
               (spv:block-world-crosshair-fragment-module))))))

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
