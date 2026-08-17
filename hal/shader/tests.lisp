(defpackage #:luvcraft.tests
  (:use #:cl #:rove #:luv #:luvcraft #:luvcraft.world)
  (:local-nicknames (#:spv #:luv.spir-v)
                    (#:shaders #:luvcraft.shaders)
                    (#:analytic #:luv.analytic)
                    (#:slug #:luv.slug)
                    (#:math #:luv.arithmetic)
                    (#:lang #:luv.arithmetic.language)
                    (#:msl #:luv.msl)
                    #+darwin
                    (#:objc #:luv.objective-c)
                    #+darwin
                    (#:metal #:luv.metal))
  (:import-from #:luv.arithmetic.lisp.vec3
                #:make-vec3
                #:vec3
                #:vec3-component
                #:vec3-cross
                #:vec3-dot
                #:vec3-length
                #:vec3-list
                #:vec3-normalize
                #:vec3-scale
                #:vec3-x
                #:vec3-y
                #:vec3-z)
  ;; Shader operators are identified by symbol, so specification bodies
  ;; written here must use the shader language's own words.
  (:import-from #:luv.spir-v
                #:dot #:sample #:sample-compare #:texel-load #:mix
                #:uint #:uvec2
                #:vec2 #:vec4 #:swizzle
                #:clamp #:smoothstep #:normalize
                #:quantity #:assume-quantity #:interpret #:representation
                #:convert-unit #:project-point #:project-sample #:counted-fold
                #:set-output))

(in-package #:luvcraft.tests)

(math:define-quantity :test-position :kind :dimensionless
  :components (:test-position-x :test-position-y :test-position-z))

(deftest portable-operator-symbols-retain-shader-identity
  (ok (eq 'spv:dot 'math:dot))
  (ok (eq 'spv:clamp 'math:clamp))
  (ok (eq 'spv:mix 'math:mix))
  (ok (eq 'spv:smoothstep 'math:smoothstep))
  (ok (eq 'spv:step 'math:step))
  (ok (eq 'spv:normalize 'math:normalize))
  (ok (eq 'spv:quantity 'lang:quantity))
  (ok (eq 'spv:assume-quantity 'lang:assume-quantity))
  (ok (eq 'spv:interpret 'lang:interpret))
  (ok (eq 'spv:representation 'lang:representation))
  (ok (eq 'spv:convert-unit 'lang:convert-unit)))

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

(defconstant +shader-function-test-offset+ 0.25)

(spv:define-shader-function typed-shader-function-probe (value scale)
  "A lexical typed-function probe written without source-form construction."
  (let* ((shifted (+ value +shader-function-test-offset+))
         (scaled (* shifted scale)))
    scaled))

(lang:define-arithmetic-function shared-shader-function-probe ((value))
  (let* ((shifted (+ value 0.25)))
    (* shifted shifted)))

(spv:define-shader unsigned-texel-fold-probe
    (:stage :fragment
     :resources ((band-data :uint-texture-2d :binding 0)
                 (curve-data :texture-2d :binding 1))
     :outputs ((color :vec4 :location 0)))
  (let* ((origin (uvec2 (uint 0.0) (uint 0.0)))
         (header (texel-load band-data origin))
         (count (swizzle header :x))
         (offset (swizzle header :y))
         (seed (swizzle (texel-load curve-data origin) :x))
         (total
           (counted-fold (index count sum seed)
             (let* ((address (+ offset index))
                    (location
                      (uvec2 (mod address (uint 4096.0))
                             (/ address (uint 4096.0))))
                    (word
                      (swizzle (texel-load band-data location) :x)))
               (+ sum (float word))))))
    (set-output color (vec4 total total total 1.0))))

(spv:define-task-payload vulkan-task-mesh-payload
  (payload-site :uint64)
  (payload-position (:array :vec4 32)))

(spv:define-shader vulkan-task-probe
    (:stage :task
     :workgroup-size (32 1 1)
     :payload vulkan-task-mesh-payload
     :inputs ((lane :uint :built-in :local-invocation-index)
              (local-id :uvec3 :built-in :local-invocation-id)
              (group :uvec3 :built-in :workgroup-id)
              (group-count :uvec3 :built-in :num-workgroups)
              (threads :uvec3 :built-in :workgroup-size)))
  (let* ((three (spv:uint 3.0))
         (one (spv:uint 1.0)))
    (when (= lane (spv:uint 0.0))
      (spv:set-payload payload-site (spv:uint64 three)))
    (spv:set-payload-element
     payload-position lane (spv:vec4 0.0 0.0 0.0 1.0))
    (spv:emit-mesh-workgroups (spv:uvec3 one one one))))

(spv:define-shader vulkan-mesh-probe
    (:stage :mesh
     :workgroup-size (32 1 1)
     :payload vulkan-task-mesh-payload
     :inputs ((lane :uint :built-in :local-invocation-index)
              (group :uvec3 :built-in :workgroup-id))
     :mesh-output
     (:topology :triangles
      :max-vertices 32
      :max-primitives 16
      :vertex ((position :vec4 :built-in :position)
               (uv :vec2 :location 0))
      :primitive ((primitive-color :vec4 :location 1))))
  (let* ((vertex-count (spv:uint payload-site))
         (primitive-count (spv:uint 1.0)))
    (spv:set-mesh-output-counts vertex-count primitive-count)
    (when (< lane vertex-count)
      (spv:set-mesh-vertex
       lane
       (position (spv:payload-element payload-position lane))
       (uv (spv:vec2 0.0 0.0))))
    (when (= lane (spv:uint 0.0))
      (spv:set-mesh-primitive
       (spv:uint 0.0)
       (spv:uvec3 (spv:uint 0.0)
                  (spv:uint 1.0)
                  (spv:uint 2.0))
       (primitive-color (spv:vec4 1.0 1.0 1.0 1.0))))))

(lang:define-arithmetic-function shared-fold-probe ((count))
  (counted-fold (index count sum 0.0)
    (+ sum index)))

(lang:define-arithmetic-function shared-conditional-fold-probe
    ((count) (limit))
  (counted-fold (index count sum 0.0)
    (if (< index limit) (+ sum index) sum)))

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

(deftest shader-functions-are-typed-calls-with-lexical-bindings
  (let* ((definition
           (spv:shader-function-definition-for 'typed-shader-function-probe))
         (specification
           (spv:parse-shader-specification
            'typed-shader-function-specification
            '(:stage :fragment
              :inputs ((value :float :location 0)
                       (scale :float :location 1))
              :outputs ((result :float :location 0)))
            '((set-output result
                          (typed-shader-function-probe value scale)))))
         (call
           (spv:shader-assignment-value
            (first (spv:shader-specification-statements specification))))
         (lowering (spv:compile-shader-specification specification)))
    (ok (typep definition 'spv:shader-function-definition))
    (ok (equal '(value scale) (spv:shader-function-parameters definition)))
    (ok (search "without source-form construction"
                (documentation 'typed-shader-function-probe
                               'spv:shader-function)))
    (ok (typep call 'spv:shader-function-call))
    (ok (eq definition (spv:shader-function-call-definition call)))
    (ok (= 2 (length (spv:shader-function-call-arguments call))))
    ;; Two parameter aliases and two lexical LET* bindings remain typed
    ;; objects.  Only computed locals need entry-block declarations.
    (ok (= 4 (length (spv:shader-function-call-bindings call))))
    (ok (= 2 (length (spv:shader-specification-bindings specification))))
    (ok (equal '(typed-shader-function-probe value scale)
               (spv:shader-expression-form call)))
    (ok (gethash call
                 (spv:shader-lowering-expression-instructions lowering)))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification specification) 0)))))

(deftest shader-function-redefinition-affects-only-fresh-parses
  (labels ((install-addition ()
             (eval
              '(spv:define-shader-function redefinable-shader-function
                   (left right)
                 (+ left right))))
           (install-subtraction ()
             (eval
              '(spv:define-shader-function redefinable-shader-function
                   (left right)
                 (- left right))))
           (parse-probe ()
             (spv:parse-shader-specification
              'redefinable-shader-function-specification
              '(:stage :fragment
                :inputs ((left :float :location 0)
                         (right :float :location 1))
                :outputs ((result :float :location 0)))
              '((set-output result
                            (redefinable-shader-function left right)))))
           (result-operator (specification)
             (let ((call
                     (spv:shader-assignment-value
                      (first
                       (spv:shader-specification-statements specification)))))
               (spv:shader-call-operator
                (spv:shader-function-call-result call)))))
    (install-addition)
    (let ((addition (parse-probe))
          (revision (spv:shader-source-revision)))
      (install-subtraction)
      (let ((subtraction (parse-probe)))
        (ok (> (spv:shader-source-revision) revision))
        (ok (eq '+ (result-operator addition)))
        (ok (eq '- (result-operator subtraction)))
        (ok (signals
             (spv:parse-shader-specification
              'bad-shader-function-arity
              '(:stage :fragment
                :inputs ((left :float :location 0))
                :outputs ((result :float :location 0)))
              '((set-output result (redefinable-shader-function left))))
             'spv:shader-language-error))))))

(deftest shader-source-name-can-migrate-from-rewriter-to-typed-function
  (eval
   '(spv:define-shader-abstraction source-kind-migration-probe (value)
      `(+ ,value 1.0)))
  (ok (spv:shader-abstraction-p 'source-kind-migration-probe))
  (eval
   '(spv:define-shader-function source-kind-migration-probe (value)
      (+ value 1.0)))
  (ok (not (spv:shader-abstraction-p 'source-kind-migration-probe)))
  (ok (spv:shader-function-definition-for 'source-kind-migration-probe))
  (let* ((specification
           (spv:parse-shader-specification
            'source-kind-migration-specification
            '(:stage :fragment
              :inputs ((value :float :location 0))
              :outputs ((result :float :location 0)))
            '((set-output result (source-kind-migration-probe value)))))
         (call
           (spv:shader-assignment-value
            (first (spv:shader-specification-statements specification)))))
    (ok (typep call 'spv:shader-function-call))))

(deftest shader-source-is-a-typed-clos-graph
  (let* ((specification (shaders:block-world-fragment-specification))
         (sun-direction (binding-named 'sun-direction specification))
         (sun-visibility (binding-named 'sun-visibility specification))
         (direct-shadow (binding-named 'direct-shadow specification))
         (sky-light (binding-named 'sky-light specification))
         (reflected (binding-named 'reflected specification))
         (radiance (binding-named 'radiance specification))
         (fogged (binding-named 'fogged specification)))
    (ok (typep specification 'spv:shader-specification))
    (ok (eq (spv:shader-specification-stage specification) :fragment))
    (ok (= (length (spv:shader-specification-inputs specification)) 8))
    (ok (= (length (spv:shader-specification-resources specification)) 6))
    (ok (typep (spv:shader-binding-expression sun-direction)
               'spv:shader-call))
    (ok (typep (spv:shader-binding-expression sun-direction)
               'lang:arithmetic-call))
    (ok (equal
         (spv:shader-expression-form
          (spv:shader-binding-expression sun-direction))
         (lang:arithmetic-expression-form
          (spv:shader-binding-expression sun-direction))))
    (ok (eq :world-direction
            (math:quantity-specification-name
             (spv:shader-expression-quantity-specification
              (spv:shader-binding-expression sun-direction)))))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression sun-direction))
         :vec3))
    (ok (equal
         (form-names
          (spv:shader-expression-form
           (spv:shader-binding-expression sun-visibility)))
         '("smoothstep"
           ("quantity" 0.9 "quantity" "sky-light-level" "unit" "one")
           ("quantity" 1.0 "quantity" "sky-light-level" "unit" "one")
           "sky-input")))
    ;; The map's answer is taken only where the surface faces the light; a
    ;; surface turned away is lit by nothing, so it is also shadowed by
    ;; nothing.  #0604PY
    (ok (equal
         (form-names
          (spv:shader-expression-form
           (spv:shader-binding-expression
            (binding-named 'sampled-shadow specification))))
         '("mix" 1.0 "shadow-sample" "shadow-in-bounds")))
    (ok (equal
         (form-names
          (spv:shader-expression-form
           (spv:shader-binding-expression direct-shadow)))
         '("mix" 1.0 "sampled-shadow" "shadow-relevance")))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-binding-expression sky-light))
         :vec3))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression reflected)))
               '("interpret"
                 ("*" "albedo"
                  ("+" "sky-light" "sun-light" "local-light"))
                 "quantity" "linear-rgb" "unit" "one")))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression radiance)))
               '("+" "reflected" "specular"
                 ("interpret" ("*" "albedo" "emission-input")
                  "quantity" "linear-rgb" "unit" "one"))))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression fogged)))
               '("mix" "radiance" "fog-color" "fog-amount")))
    (dolist (binding (list sky-light reflected radiance fogged))
      (ok (eq :absolute
              (math:quantity-specification-character
               (spv:shader-expression-quantity-specification
                (spv:shader-binding-expression binding))))))
    (ok (> (length (spv:shader-specification-expressions specification))
           (length (spv:shader-specification-bindings specification))))))

(deftest block-vertex-source-is-a-typed-clos-graph-with-an-explicit-abi
  (let* ((specification (shaders:block-world-vertex-specification))
         (resource (first (spv:shader-specification-resources specification)))
         (clip-position
           (find 'clip-position
                 (spv:shader-specification-outputs specification)
                 :key #'spv:shader-object-name
                 :test (lambda (left right)
                         (string-equal (symbol-name left)
                                       (symbol-name right)))))
         (fog-amount (binding-named 'fog-amount specification))
         (relative (binding-named 'relative specification))
         (view-z (binding-named 'view-z specification))
         (shadow-projection
           (binding-named 'shadow-projection specification)))
    (ok (typep specification 'spv:shader-specification))
    (ok (eq (spv:shader-specification-stage specification) :vertex))
    (ok (= (length (spv:shader-specification-inputs specification)) 5))
    (ok (= (length (spv:shader-specification-outputs specification)) 9))
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
    (let ((fog-call (spv:shader-binding-expression fog-amount)))
      (ok (typep fog-call 'spv:shader-function-call))
      (ok (eq
           (lang:arithmetic-function-definition-for
            'luvcraft.arithmetic:fog-amount-at-view-distance)
           (spv:shader-function-call-definition fog-call)))
      (ok (equal
           (form-names (spv:shader-expression-form fog-call))
           '("fog-amount-at-view-distance"
             "view-z" "fog-near" "fog-far"))))
    (let ((relative-quantity
            (spv:shader-expression-quantity-specification
             (spv:shader-binding-expression relative)))
          (view-z-quantity
            (spv:shader-expression-quantity-specification
             (spv:shader-binding-expression view-z))))
      (ok (eq :world-position
              (math:quantity-specification-name relative-quantity)))
      (ok (not (math:quantity-specification-affine-p relative-quantity)))
      (ok (math:unit-expression=
           :cell (math:quantity-specification-unit relative-quantity)))
      (ok (eq :view-distance
              (math:quantity-specification-name view-z-quantity)))
      (ok (math:unit-expression=
           :cell (math:quantity-specification-unit view-z-quantity))))
    (ok (typep (spv:shader-binding-expression shadow-projection)
               'spv:shader-map-projection))))

(deftest projective-maps-are-semantic-objects-with-packed-products
  (let* ((specification (shaders:block-world-vertex-specification))
         (projection-binding
           (binding-named 'shadow-projection specification))
         (projection (spv:shader-binding-expression projection-binding))
         (application (spv:shader-map-projection-application projection))
         (definition (spv:shader-map-definition-for :world-to-light))
         (domain
           (spv:shader-map-domain-quantity-specification definition))
         (layout
           (spv:shader-projective-map-sample-quantity-layout definition))
         (uv (math:project-quantity-layout layout '(0 1)))
         (depth (math:project-quantity-layout layout '(2))))
    (ok (typep definition 'spv:shader-projective-map-definition))
    (ok (eq definition (spv:shader-map-application-definition application)))
    (ok (eq application
            (spv:shader-map-projection-application projection)))
    (ok (spv:shader-type= :vec3 (spv:shader-map-domain-type definition)))
    (ok (spv:shader-type=
         :vec4 (spv:shader-projective-map-homogeneous-type definition)))
    (ok (spv:shader-type=
         :vec3 (spv:shader-projective-map-sample-type definition)))
    (ok (eq :world-position
            (math:quantity-specification-name domain)))
    (ok (math:quantity-specification-affine-p domain))
    (ok (math:unit-expression=
         :cell (math:quantity-specification-unit domain)))
    (ok (eq :shadow-uv (math:quantity-specification-name uv)))
    (ok (math:quantity-specification-affine-p uv))
    (ok (eq :shadow-depth (math:quantity-specification-name depth)))
    (ok (math:quantity-specification-affine-p depth))
    (ok (math:quantity-layout=
         layout (spv:shader-expression-quantity-layout projection)))
    (ok (null (spv:shader-expression-quantity-layout application)))
    (ok (equal '(1/2 1/2 1)
               (spv:shader-projective-map-coordinate-scale definition)))
    (ok (equal '(1/2 1/2 0)
               (spv:shader-projective-map-coordinate-offset definition)))
    (ok (= 4 (length (spv:shader-map-application-rows application))))
    (ok (every (lambda (row)
                 (not (spv:shader-expression-quantity-checked-p row)))
               (spv:shader-map-application-rows application)))
    (ok (spv:shader-expression-materialized-p application))
    (ok (not (spv:shader-expression-materialized-p projection)))
    (labels ((contains-representation-p (expression)
               (or (typep expression 'spv:shader-representation)
                   (some #'contains-representation-p
                         (spv:shader-expression-children expression)))))
      (ok (not (contains-representation-p application))))))

(deftest projective-map-applications-reject-undefined-or-wrong-semantics
  (flet ((reason-for (point-declaration form &optional annotated-row-p)
           (handler-case
               (progn
                 (spv:parse-shader-specification
                  'invalid-projective-map-probe
                  `(:stage :vertex
                    :inputs
                    (,point-declaration
                     (row-x :vec4 :location 1
                            ,@(when annotated-row-p
                                '(:quantity :linear-rgba :unit :one)))
                     (row-y :vec4 :location 2)
                     (row-z :vec4 :location 3)
                     (row-w :vec4 :location 4))
                    :outputs
                    ((result :vec2 :location 0
                             :quantity :shadow-uv :unit :one :affine-p t)))
                  `((set-output result (swizzle ,form :xy))))
                 nil)
             (spv:shader-language-error (condition)
               (spv:shader-language-error-reason condition)))))
    (let ((world
            '(position :vec3 :location 0
              :quantity :world-position :unit :cell :affine-p t))
          (raw '(position :vec3 :location 0))
          (direction
            '(position :vec3 :location 0
              :quantity :world-direction :unit :one)))
      (ok (eq :undefined-shader-map
              (reason-for
               world
               '(project-point :missing-map position
                 row-x row-y row-z row-w))))
      (ok (eq :projective-map-domain-mismatch
              (reason-for
               raw
               '(project-point :world-to-light position
                 row-x row-y row-z row-w))))
      (ok (eq :projective-map-domain-mismatch
              (reason-for
               direction
               '(project-point :world-to-light position
                 row-x row-y row-z row-w))))
      (ok (eq :projective-map-row-count
              (reason-for
               world
               '(project-point :world-to-light position
                 row-x row-y row-z))))
      (ok (eq :invalid-projective-map-rows
              (reason-for
               world
               '(project-point :world-to-light position
                 row-x row-y row-z row-w)
               t)))
      (ok (eq :sampling-projection-requires-map-application
              (handler-case
                  (progn
                    (spv:parse-shader-specification
                     'invalid-sampling-projection-probe
                     '(:stage :vertex
                       :inputs ((raw-clip :vec4 :location 0))
                       :outputs
                       ((result :vec2 :location 0
                                :quantity :shadow-uv :unit :one)))
                     '((set-output
                        result (swizzle (project-sample raw-clip) :xy))))
                    nil)
                (spv:shader-language-error (condition)
                  (spv:shader-language-error-reason condition))))))))

(deftest block-vertex-uniform-members-retain-access-chain-provenance
  (let* ((lowering (shaders:block-world-vertex-lowering))
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
  (let* ((specification (shaders:block-world-shadow-vertex-specification))
         (resource (first (spv:shader-specification-resources specification)))
         (clip (binding-named 'clip specification))
         (clip-position
           (first (spv:shader-specification-statements specification))))
    (ok (eq (spv:shader-specification-stage specification) :vertex))
    (ok (= (length (spv:shader-specification-inputs specification)) 1))
    (ok (= (length (spv:shader-specification-outputs specification)) 1))
    (ok (typep resource 'spv:shader-uniform-block))
    (ok (= (spv:shader-resource-binding resource) 2))
    (let ((application (spv:shader-binding-expression clip)))
      (ok (typep application 'spv:shader-map-application))
      (ok (eq (spv:shader-map-definition-for :world-to-light)
              (spv:shader-map-application-definition application)))
      (ok (spv:shader-type=
           (spv:shader-expression-type application) :vec4)))
    (ok (spv:shader-type=
         (spv:shader-expression-type
          (spv:shader-assignment-value clip-position))
         :vec4))
    (ok (> (length (shaders:block-world-shadow-vertex-shader)) 5))))

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
  (let* ((lowering (shaders:block-world-fragment-lowering))
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
         (block-specification (shaders:block-world-fragment-specification))
         (torch-color (binding-named 'torch-color block-specification))
         (block-lowering
           (spv:compile-shader-specification block-specification))
         (literal
           (first
            (spv:shader-call-operands
             (spv:shader-quantity-construction-operand
              (spv:shader-binding-expression torch-color)))))
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

(deftest a-first-use-vector-constructor-cannot-claim-its-type-id
  (let ((specification
          (spv:parse-shader-specification
           'first-use-vector-constructor
           '(:stage :fragment
             :outputs ((color :vec4 :location 0)))
           '((let* ((rgb (spv:vec3 0.0 0.0 0.0)))
               (set-output color (vec4 rgb 1.0)))))))
    ;; VEC3 has no interface declaration to predeclare its type.  Assembly is
    ;; therefore the direct proof that the type and its first value received
    ;; distinct result IDs.
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification specification) 0)))))

(deftest canonical-shader-id-reservations-never-reuse-a-claimed-id
  (let* ((context (make-instance 'spv::shader-lowering-context))
         (first (spv::reserve-shader-id context 'probe))
         (second (spv::reserve-shader-id context 'probe)))
    (ok (not (eq first second)))
    (ok (string= "%PROBE" (symbol-name first)))
    (ok (string= "%PROBE-2" (symbol-name second)))))

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

(deftest shader-arithmetic-carries-backend-neutral-quantity-specifications
  (flet ((parse-depth-probe (annotated-p)
           (spv:parse-shader-specification
            'semantic-depth-probe
            (if annotated-p
                '(:stage :fragment
                  :inputs
                  ((receiver-depth :float :location 0
                                   :quantity :shadow-depth)
                   (bias :float :location 1
                         :quantity :shadow-depth :character :difference))
                  :outputs
                  ((biased-depth :float :location 0
                                 :quantity :shadow-depth)))
                '(:stage :fragment
                  :inputs ((receiver-depth :float :location 0)
                           (bias :float :location 1))
                  :outputs ((biased-depth :float :location 0))))
            '((let* ((biased (- receiver-depth bias)))
                (set-output biased-depth biased))))))
    (let* ((annotated (parse-depth-probe t))
           (plain (parse-depth-probe nil))
           (receiver (first (spv:shader-specification-inputs annotated)))
           (biased (binding-named 'biased annotated))
           (receiver-specification
             (spv:shader-declaration-quantity-specification receiver))
           (biased-specification
             (spv:shader-expression-quantity-specification
              (spv:shader-binding-expression biased))))
      (ok (eq :shadow-depth
              (math:quantity-specification-name receiver-specification)))
      (ok (math:quantity-specification-affine-p receiver-specification))
      (ok (math:quantity-specification-affine-p biased-specification))
      ;; Semantic checking is a source concern; it does not perturb the SPIR-V
      ;; representation or the deterministic lowering of valid arithmetic.
      (ok (equalp (spv:assemble-shader-specification annotated)
                  (spv:assemble-shader-specification plain))))))

(deftest shader-arithmetic-rejects-dimensionally-or-affinely-invalid-forms
  (labels ((reason-for (options body)
             (handler-case
                 (progn
                   (spv:parse-shader-specification
                    'invalid-semantic-probe options body)
                   nil)
               (spv:shader-language-error (condition)
                 (list (spv:shader-language-error-reason condition)
                       (spv:shader-language-error-details condition))))))
    (ok (equal
         '(:invalid-quantity-operation :cannot-add-points)
         (reason-for
          '(:stage :fragment
            :inputs ((left :float :location 0
                           :quantity :shadow-depth :affine-p t)
                     (right :float :location 1
                            :quantity :shadow-depth :affine-p t))
            :outputs ((result :float :location 0)))
          '((let* ((sum (+ left right)))
              (set-output result sum))))))
    (ok (equal
         '(:invalid-quantity-operation :different-quantity-spaces)
         (reason-for
          '(:stage :fragment
            :inputs ((depth :float :location 0
                            :quantity :shadow-depth)
                     (distance :float :location 1
                               :quantity :distance
                               :dimension :length))
            :outputs ((result :float :location 0)))
          '((let* ((sum (+ depth distance)))
              (set-output result sum))))))))

(deftest annotated-shader-arithmetic-is-total-and-unit-exact
  (labels ((reason-for (inputs expression)
             (handler-case
                 (progn
                   (spv:parse-shader-specification
                    'semantic-totality-probe
                    `(:stage :fragment
                      :inputs ,inputs
                      :outputs ((result :float :location 0)))
                    `((let* ((value ,expression))
                        (set-output result value))))
                   nil)
               (spv:shader-language-error (condition)
                 (list (spv:shader-language-error-reason condition)
                       (spv:shader-language-error-details condition))))))
    (let* ((specification
             (spv:parse-shader-specification
              'semantic-max-probe
              '(:stage :fragment
                :inputs ((left :float :location 0
                                :quantity :distance
                                :dimension :length :unit :metre)
                         (right :float :location 1
                                 :quantity :distance
                                 :dimension :length :unit :metre))
                :outputs ((result :float :location 0)))
              '((let* ((value (max left right)))
                  (set-output result value)))))
           (value (binding-named 'value specification)))
      (ok (math:unit-expression=
           :metre
           (math:quantity-specification-unit
            (spv:shader-expression-quantity-specification
             (spv:shader-binding-expression value))))))
    (ok (equal
         '(:invalid-quantity-operation :different-units)
         (reason-for
          '((left :float :location 0 :quantity :distance
                  :dimension :length :unit :metre)
            (right :float :location 1 :quantity :distance
                   :dimension :length :unit :kilometre))
          '(max left right))))
    (ok (equal
         '(:invalid-quantity-operation :unknown-operator)
         (reason-for
          '((value :float :location 0 :quantity :distance
                   :dimension :length :unit :metre))
          '(abs value))))
    (ok (equal
         '(:missing-quantity-specification (RIGHT))
         (reason-for
          '((left :float :location 0 :quantity :distance
                  :dimension :length :unit :metre)
            (right :float :location 1))
          '(+ left right))))))

(deftest packed-gpu-vectors-project-as-semantic-products
  (let* ((specification
           (spv:parse-shader-specification
            'semantic-product-probe
            '(:stage :fragment
              :inputs
              ((packed :vec3 :location 0
                       :components
                       ((:xy :quantity :sample-position :affine-p t)
                        (:z :quantity :sample-value))))
              :outputs
              ((position-output :vec2 :location 0
                                :quantity :sample-position :affine-p t)))
            '((let* ((position (swizzle packed :xy))
                     (sample-value (swizzle packed :z)))
                (set-output position-output position)))))
         (packed (first (spv:shader-specification-inputs specification)))
         (layout (spv:shader-declaration-quantity-layout packed))
         (position (binding-named 'position specification))
         (sample-value (binding-named 'sample-value specification)))
    (ok (null (spv:shader-declaration-quantity-specification packed)))
    (ok (= 3 (math:quantity-layout-extent layout)))
    (ok (eq :sample-position
            (math:quantity-specification-name
             (spv:shader-expression-quantity-specification
              (spv:shader-binding-expression position)))))
    (ok (eq :sample-value
            (math:quantity-specification-name
             (spv:shader-expression-quantity-specification
              (spv:shader-binding-expression sample-value))))))
  (labels ((reason-for (expression)
             (handler-case
                 (progn
                   (spv:parse-shader-specification
                    'invalid-semantic-product-probe
                    '(:stage :fragment
                      :inputs
                      ((packed :vec3 :location 0
                               :components
                               ((:xy :quantity :sample-position)
                                (:z :quantity :sample-value))))
                      :outputs ((result :vec3 :location 0)))
                    `((set-output result ,expression)))
                   nil)
               (spv:shader-language-error (condition)
                 (spv:shader-language-error-reason condition)))))
    (ok (eq :undeclared-quantity-projection
            (reason-for '(swizzle packed :yz))))
    (ok (eq :missing-quantity-specification
            (reason-for '(+ packed packed)))))
  (let* ((specification
           (spv:parse-shader-specification
            'homogeneous-component-probe
            '(:stage :fragment
              :inputs ((position :vec3 :location 0
                                 :quantity :test-position))
              :outputs ((x-output :float :location 0
                                  :quantity :test-position-x)))
            '((set-output x-output (swizzle position :x)))))
         (x (spv:shader-assignment-value
             (first (spv:shader-specification-statements specification)))))
    (ok (eq :test-position-x
            (math:quantity-specification-name
             (spv:shader-expression-quantity-specification x))))))

(deftest texture-samples-can-publish-semantic-channel-layouts
  (let* ((specification
           (spv:parse-shader-specification
            'semantic-sample-probe
            '(:stage :fragment
              :inputs ((uv :vec2 :location 0))
              :outputs ((rgb-output :vec3 :location 0
                                    :quantity :linear-rgb))
              :resources
              ((image :texture-2d :binding 0
                      :sample-transfer :srgb-to-linear
                      :sample-components
                      ((:rgb :quantity :linear-rgb)
                       (:a :quantity :opacity)))
               (sampler :sampler :binding 1)))
            '((let* ((texel (sample image sampler uv))
                     (rgb (swizzle texel :rgb))
                     (alpha (swizzle texel :a)))
                (set-output rgb-output rgb)))))
         (image (first (spv:shader-specification-resources specification)))
         (texel (binding-named 'texel specification))
         (rgb (binding-named 'rgb specification))
         (alpha (binding-named 'alpha specification)))
    (ok (eq :srgb-to-linear
            (spv:shader-resource-sample-transfer image)))
    (ok (math:quantity-layout=
         (spv:shader-resource-sample-quantity-layout image)
         (spv:shader-expression-quantity-layout
          (spv:shader-binding-expression texel))))
    (ok (eq :linear-rgb
            (math:quantity-specification-name
             (spv:shader-expression-quantity-specification
              (spv:shader-binding-expression rgb)))))
    (ok (eq :opacity
            (math:quantity-specification-name
             (spv:shader-expression-quantity-specification
              (spv:shader-binding-expression alpha)))))))

(deftest sample-transfer-metadata-is-texture-only
  (flet ((reason-for (resource)
           (handler-case
               (progn
                 (spv:parse-shader-specification
                  'invalid-sample-transfer-probe
                  `(:stage :fragment
                    :outputs ((result :float :location 0))
                    :resources (,resource))
                  '((set-output result 1.0)))
                 nil)
             (spv:shader-language-error (condition)
               (spv:shader-language-error-reason condition)))))
    (ok (eq :invalid-sample-transfer
            (reason-for
             '(image :texture-2d :binding 0
                     :sample-transfer :mystery-transfer))))
    (ok (eq :sample-semantics-on-non-texture
            (reason-for
             '(sampler :sampler :binding 0
                       :sample-transfer :srgb-to-linear))))))

(deftest semantic-boundaries-are-distinct-checked-and-have-no-codegen-effect
  (flet ((probe (annotated-p)
           (spv:parse-shader-specification
            'semantic-boundary-probe
            '(:stage :fragment
              :inputs ((left :float :location 0)
                       (right :float :location 1))
              :outputs ((result :float :location 0)))
            (if annotated-p
                '((let* ((typed-left
                           (assume-quantity left :quantity :left-factor))
                          (typed-right
                           (assume-quantity right :quantity :right-factor))
                          (product (* typed-left typed-right))
                          (sum (interpret product
                                          :quantity :combined-factor))
                          (represented (representation sum))
                          (recovered
                            (assume-quantity represented
                                             :quantity :combined-factor)))
                    (set-output result recovered)))
                '((let* ((sum (* left right)))
                    (set-output result sum)))))))
    (let* ((annotated (probe t))
           (plain (probe nil))
           (typed-left (binding-named 'typed-left annotated))
           (sum (binding-named 'sum annotated))
           (represented (binding-named 'represented annotated))
           (recovered (binding-named 'recovered annotated)))
      (ok (typep (spv:shader-binding-expression typed-left)
                 'spv:shader-quantity-assumption))
      (ok (typep (spv:shader-binding-expression sum)
                 'spv:shader-interpretation))
      (ok (eq :combined-factor
              (math:quantity-specification-name
               (spv:shader-expression-quantity-specification
                (spv:shader-binding-expression sum)))))
      (ok (typep (spv:shader-binding-expression represented)
                 'spv:shader-representation))
      (ok (not (spv:shader-expression-quantity-checked-p
                (spv:shader-binding-expression represented))))
      (ok (null (spv:shader-expression-quantity-specification
                 (spv:shader-binding-expression represented))))
      (ok (typep (spv:shader-binding-expression recovered)
                 'spv:shader-quantity-assumption))
      (ok (equalp (spv:assemble-shader-specification annotated)
                  (spv:assemble-shader-specification plain)))))
  (flet ((probe (constructed-p)
           (spv:parse-shader-specification
            'quantity-construction-probe
            '(:stage :fragment
              :outputs ((result :float :location 0)))
            `((set-output result
                          ,(if constructed-p
                               '(quantity 1.0 :quantity :threshold)
                               1.0))))))
    (let* ((constructed (probe t))
           (plain (probe nil))
           (expression
             (spv:shader-assignment-value
              (first (spv:shader-specification-statements constructed)))))
      (ok (typep expression 'spv:shader-quantity-construction))
      (ok (equalp (spv:assemble-shader-specification constructed)
                  (spv:assemble-shader-specification plain)))))
  (flet ((reason-for (form)
           (handler-case
               (progn
                 (spv:parse-shader-specification
                  'invalid-semantic-boundary-probe
                  '(:stage :fragment
                    :inputs ((value :float :location 0))
                    :outputs ((result :float :location 0)))
                  `((set-output result ,form)))
                 nil)
             (spv:shader-language-error (condition)
               (spv:shader-language-error-reason condition)))))
    ;; INTERPRET can name checked arithmetic, but cannot smuggle meaning onto
    ;; a raw input.  ASSUME-QUANTITY is the deliberately loud boundary for it.
    (ok (eq :representation-requires-quantity
            (reason-for '(representation value))))
    (ok (eq :invalid-quantity-interpretation
            (reason-for
             '(interpret value :quantity :distance
                         :dimension :length :unit :metre))))
    (ok (eq :invalid-quantity-interpretation
            (reason-for
             '(interpret
               (assume-quantity value :quantity :distance
                                :dimension :length :unit :metre)
               :quantity :distance
               :dimension :length :unit :kilometre))))
    (ok (eq :invalid-quantity-interpretation
            (reason-for
             '(interpret
               (assume-quantity value :quantity :height
                                :dimension :length :unit :metre)
               :quantity :width
               :dimension :length :unit :metre))))))

(deftest production-vertex-interfaces-carry-quantities-end-to-end
  (let* ((specification (shaders:block-world-vertex-specification))
         (inputs (spv:shader-specification-inputs specification))
         (outputs (spv:shader-specification-outputs specification))
         (position (first inputs))
         (uv-shade (second inputs))
         (normal (third inputs))
         (light (fourth inputs))
         (fog (fourth outputs))
         (shadow-uv (sixth outputs))
         (shadow-depth (seventh outputs))
         (position-quantity
           (spv:shader-declaration-quantity-specification position)))
    (ok (spv:shader-type=
         :vec3 (math:declaration-representation-type position)))
    (ok (eq position-quantity
            (math:declaration-quantity-specification position)))
    (ok (math:declaration-quantity-checked-p position))
    (ok (eq :world-position
            (math:quantity-specification-name position-quantity)))
    (ok (math:quantity-specification-affine-p position-quantity))
    (ok (math:unit-expression=
         :cell (math:quantity-specification-unit position-quantity)))
    (ok (spv:shader-declaration-quantity-layout uv-shade))
    (ok (eq :world-direction
            (math:quantity-specification-name
             (spv:shader-declaration-quantity-specification normal))))
    (ok (spv:shader-declaration-quantity-layout light))
    (ok (eq :fog-amount
            (math:quantity-specification-name
             (spv:shader-declaration-quantity-specification fog))))
    (ok (eq :shadow-uv
            (math:quantity-specification-name
             (spv:shader-declaration-quantity-specification shadow-uv))))
    (ok (eq :shadow-depth
            (math:quantity-specification-name
             (spv:shader-declaration-quantity-specification shadow-depth))))
    (ok (signals
         (spv:parse-shader-specification
          'invalid-production-unit-mix
          '(:stage :vertex
            :inputs
            ((position :vec3 :location 0
                       :quantity :world-position :unit :cell :affine-p t)
             (direction :vec3 :location 1
                        :quantity :world-direction :unit :one))
            :outputs ((result :vec3 :location 0)))
         '((set-output result (+ position direction))))
         'spv:shader-language-error))))

(deftest explicit-unit-conversion-preserves-meaning-and-scales-values
  (let* ((specification
           (spv:parse-shader-specification
            'unit-conversion-probe
            '(:stage :fragment
              :outputs ((result :float :location 0)))
            '((let* ((opacity
                       (quantity 50.0 :quantity :opacity :unit :percent))
                     (fraction (convert-unit opacity :unit :one)))
                (set-output result fraction)))))
         (binding (binding-named 'fraction specification))
         (expression (spv:shader-binding-expression binding))
         (quantity
           (spv:shader-expression-quantity-specification expression))
         (instructions
           (spv:lower-spir-v
            (spv:shader-lowering-module
             (spv:compile-shader-specification specification))))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    (ok (typep expression 'spv:shader-unit-conversion))
    (ok (= 1/100 (spv:shader-unit-conversion-factor expression)))
    (ok (eq :opacity (math:quantity-specification-name quantity)))
    (ok (math:unitless-p (math:quantity-specification-unit quantity)))
    (ok (find "F-MUL" names :test #'string=))
    (ok (> (length (spv:assemble-shader-specification specification)) 5)))
  (flet ((reason-for (form)
           (handler-case
               (progn
                 (spv:parse-shader-specification
                  'invalid-unit-conversion-probe
                  '(:stage :fragment
                    :inputs ((value :float :location 0))
                    :outputs ((result :float :location 0)))
                  `((set-output result ,form)))
                 nil)
             (spv:shader-language-error (condition)
               (spv:shader-language-error-reason condition)))))
    (ok (eq :unit-conversion-requires-quantity
            (reason-for '(convert-unit value :unit :metre))))
    (ok (eq :invalid-quantity-declaration
            (reason-for
             '(quantity 1.0 :quantity :opacity :unit :radian))))
    (ok (eq :invalid-quantity-declaration
            (reason-for
             '(quantity 1.0 :quantity :unregistered-distance
                            :unit :metre))))
    (ok (eq :invalid-unit-conversion
            (reason-for
             '(convert-unit
               (quantity 1.0 :quantity :duration :unit :second)
               :unit :metre))))
    (ok (eq :undefined-unit
            (reason-for
             '(convert-unit
               (quantity 1.0 :quantity :distance :unit :metre)
               :unit :furlong))))))

(deftest production-shadow-material-carries-semantic-quantities
  (let ((specification (shaders:block-world-fragment-specification)))
    (flet ((quantity (name)
             (spv:shader-expression-quantity-specification
              (spv:shader-binding-expression
               (binding-named name specification)))))
      (let ((coordinate (quantity 'shadow-coordinate))
            (receiver-depth (quantity 'receiver-depth))
            (bias (quantity 'shadow-bias))
            (world-span (quantity 'shadow-world-span))
            (gradient (quantity 'shadow-depth-gradient))
            (blocker-separation (quantity 'shadow-blocker-separation))
            (filter-radius (quantity 'shadow-filter-radius))
            (rgba (quantity 'rgba)))
        (ok (eq :shadow-uv
                (math:quantity-specification-name coordinate)))
        (ok (eq :normalized-coordinate
                (math:quantity-specification-kind coordinate)))
        (ok (math:quantity-specification-affine-p coordinate))
        (ok (eq :shadow-depth
                (math:quantity-specification-name receiver-depth)))
        (ok (math:quantity-specification-affine-p receiver-depth))
        (ok (not (math:quantity-specification-affine-p bias)))
        (ok (math:unit-expression=
             :cell (math:quantity-specification-unit world-span)))
        (ok (eq :shadow-depth-gradient
                (math:quantity-specification-name gradient)))
        (ok (eq :shadow-depth
                (math:quantity-specification-name blocker-separation)))
        (ok (math:quantity-specification-absolute-p blocker-separation))
        (ok (eq :shadow-filter-radius
                (math:quantity-specification-name filter-radius)))
        (ok (eq :sample-count
                (math:quantity-specification-kind filter-radius)))
        (ok (eq :linear-rgba
                (math:quantity-specification-name rgba)))
        (ok (eq :relative-color-signal
                (math:quantity-specification-kind rgba)))))))

(deftest production-quantity-vocabulary-supplies-character-defaults
  (flet ((specification (name &optional (unit :one))
           (math:make-quantity-specification name :unit unit)))
    (dolist (name '(:world-position :texture-uv :shadow-uv :shadow-depth
                    :clip-coordinate))
      (ok (math:quantity-specification-affine-p
           (specification name (if (eq name :world-position) :cell :one)))))
    (dolist (name '(:world-distance :linear-rgb :linear-rgba :opacity
                    :ambient-occlusion :fog-amount :day-factor
                    :sky-light-level :block-light-level :material-emission
                    :shadow-filter-radius :view-distance))
      (let ((quantity
              (specification name
                             (if (member name '(:world-distance :view-distance))
                                 :cell
                                 :one))))
        (ok (math:quantity-specification-absolute-p quantity))
        (ok (math:quantity-specification-non-negative-p quantity))))))

(deftest production-points-and-amounts-reject-invalid-arithmetic
  (labels ((failure-for (form)
             (handler-case
                 (progn
                   (spv:parse-shader-specification
                    'invalid-production-quantity-operation
                    '(:stage :fragment
                      :outputs ((result :float :location 0)))
                    `((set-output result ,form)))
                   nil)
               (spv:shader-language-error (condition) condition))))
    (let* ((negation
             '(- (quantity 1.0 :quantity :opacity :unit :one)))
           (point-addition
             '(+ (quantity 0.0 :quantity :world-x-position :unit :cell)
                 (quantity 1.0 :quantity :world-x-position :unit :cell)))
           (negation-error (failure-for negation))
           (point-error (failure-for point-addition)))
      (ok (eq :invalid-quantity-operation
              (spv:shader-language-error-reason negation-error)))
      (ok (eq :cannot-negate-amount
              (spv::shader-language-error-details negation-error)))
      (ok (equal negation (spv::shader-language-error-form negation-error)))
      (ok (eq :invalid-quantity-operation
              (spv:shader-language-error-reason point-error)))
      (ok (eq :cannot-add-points
              (spv::shader-language-error-details point-error)))
      (ok (equal point-addition
                 (spv::shader-language-error-form point-error))))))

(deftest production-crosshair-composes-linear-rgb-with-opacity
  (let* ((specification (shaders:block-world-crosshair-fragment-specification))
         (ink (first (spv:shader-specification-inputs specification)))
         (rgba (binding-named 'rgba specification))
         (ink-quantity
           (spv:shader-declaration-quantity-specification ink))
         (opaque-quantity
           (spv:shader-expression-quantity-specification
            (find-if
             (lambda (expression)
               (let ((quantity
                       (spv:shader-expression-quantity-specification
                        expression)))
                 (and quantity
                      (eq :opacity
                          (math:quantity-specification-name quantity)))))
             (spv:shader-specification-expressions specification))))
         (rgba-quantity
           (spv:shader-expression-quantity-specification
            (spv:shader-binding-expression rgba))))
    (ok (eq :linear-rgb
            (math:quantity-specification-name ink-quantity)))
    (ok (eq :opacity
            (math:quantity-specification-name opaque-quantity)))
    (ok (eq :linear-rgba
            (math:quantity-specification-name rgba-quantity)))
    (ok (math:quantity-specification-non-negative-p rgba-quantity))))

(deftest shadow-visibility-is-a-source-abstraction-over-core-math
  (ok (spv:shader-abstraction-p 'spv:shadow-visibility))
  (ok (not (spv:shader-operator-p 'spv:shadow-visibility)))
  (let* ((specification
           (spv:parse-shader-specification
            'shadow-visibility-probe
            '(:stage :fragment
              :inputs
              ((uv :vec2 :location 0
                   :quantity :shadow-uv)
               (receiver-depth :float :location 1
                               :quantity :shadow-depth)
               (receiver-depth-gradient :vec2 :location 2
                                        :quantity :shadow-depth-gradient)
               (texel-size :vec2 :location 3
                           :quantity :shadow-uv :character :difference)
               (bias :float :location 4
                     :quantity :shadow-depth :character :difference)
               (radius :float :location 5
                       :quantity :shadow-filter-radius))
              :outputs ((visibility :float :location 0))
              :resources ((shadow-map :depth-texture-2d :binding 0)
                          (shadow-sampler :sampler :binding 1)))
            '((let* ((coordinate uv)
                     (depth receiver-depth)
                     (gradient receiver-depth-gradient)
                     (texel texel-size)
                     (depth-bias bias)
                     (filter-radius radius)
                     (visible (spv:shadow-visibility
                               shadow-map shadow-sampler coordinate
                               depth gradient texel depth-bias filter-radius)))
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
    (ok (= 17 (count "IMAGE-SAMPLE-DREF-IMPLICIT-LOD" names :test #'string=)))
    (ok (= 17 (count "DOT" names :test #'string=)))
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
                    (shaders:block-world-fragment-module)))))
    (ok (equal (forms) (forms)))
    (let ((words (shaders:block-world-fragment-shader)))
      (ok (> (length words) 5))
      (ok (= (aref words 0) #x07230203)))
    (let ((vertex (shaders:block-world-crosshair-vertex-shader))
          (fragment (shaders:block-world-crosshair-fragment-shader)))
      (ok (> (length vertex) 5))
      (ok (> (length fragment) 5))
      (ok (= (aref vertex 0) #x07230203))
      (ok (= (aref fragment 0) #x07230203)))
    (let ((vertex (shaders:block-world-vertex-shader)))
      (ok (> (length vertex) 5))
      (ok (= (aref vertex 0) #x07230203)))
    (let ((vertex (shaders:block-world-sky-vertex-shader))
          (fragment (shaders:block-world-sky-fragment-shader)))
      (ok (> (length vertex) 5))
      (ok (> (length fragment) 5))
      (ok (= (aref vertex 0) #x07230203))
      (ok (= (aref fragment 0) #x07230203)))
    (let ((vertex (shaders:block-world-shadow-vertex-shader)))
      (ok (> (length vertex) 5))
      (ok (= (aref vertex 0) #x07230203)))
    (let ((vertex
            (spv:assemble-shader-specification
             (shaders:block-world-text-vertex-specification)))
          (fragment
            (spv:assemble-shader-specification
             (shaders:block-world-text-fragment-specification))))
      (ok (> (length vertex) 5))
      (ok (> (length fragment) 5))
      (ok (= (aref vertex 0) #x07230203))
      (ok (= (aref fragment 0) #x07230203)))))

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
             (list (shaders:block-world-vertex-specification)
                   (shaders:block-world-fragment-specification)
                   (shaders:block-world-sky-vertex-specification)
                   (shaders:block-world-sky-fragment-specification)
                   (shaders:block-world-shadow-vertex-specification)
                   (shaders:block-world-text-vertex-specification)))
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
  (let* ((vertex (shaders:block-world-sky-vertex-specification))
         (fragment (shaders:block-world-sky-fragment-specification))
         (fragment-module (shaders:block-world-sky-fragment-module))
         (ray (binding-named 'ray vertex))
         (direction (binding-named 'direction fragment))
         (cloud-density (binding-named 'cloud-density fragment))
         (disc (binding-named 'disc fragment)))
    (ok (eq (spv:shader-specification-stage vertex) :vertex))
    (ok (eq (spv:shader-specification-stage fragment) :fragment))
    (ok (spv:shader-type=
         (spv:shader-expression-type (spv:shader-binding-expression ray))
         :vec3))
    ;; The sky drops out of the checked-quantity world once, deliberately,
    ;; and is ordinary image mathematics over a unit view ray thereafter.
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression direction)))
               '("normalize" ("representation" "ray-input"))))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression cloud-density)))
               '("*" "deck-mask"
                 ("smoothstep" "cloud-edge"
                  ("+" "cloud-edge" 0.11) "cloud-field"))))
    (ok (equal (form-names
                (spv:shader-expression-form
                 (spv:shader-binding-expression disc)))
               '("smoothstep" ("-" 1.0 "disc-limb")
                 ("-" 1.0 ("*" 0.56 "disc-limb")) "alignment")))
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
                     (lit (max 0.0 (dot unit (spv:vec3 0.0 1.0 0.0))))
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
               (shaders:block-world-crosshair-fragment-module))))))

(deftest slug-root-eligibility-is-the-eight-class-table
  (let ((expected '((0 0) (1 0) (1 1) (1 0)
                    (0 1) (1 1) (0 1) (0 0))))
    (loop for code below 8
          for pair in expected
          for y1 = (if (logbitp 0 code) 1.0 -1.0)
          for y2 = (if (logbitp 1 code) 1.0 -1.0)
          for y3 = (if (logbitp 2 code) 1.0 -1.0)
          do (ok (equal pair
                        (multiple-value-list
                         (slug:slug-root-eligibility y1 y2 y3))))))
  ;; Zero is deliberately in the non-positive class.
  (ok (equal '(0 0)
             (multiple-value-list
              (slug:slug-root-eligibility 0.0 0.0 0.0)))))

(defun slug-test-point (x y)
  (slug:make-slug-point :x x :y y))

(defun slug-test-line (x1 y1 x2 y2)
  (slug:make-slug-line (slug-test-point x1 y1)
                       (slug-test-point x2 y2)))

(defun slug-test-square (left bottom right top &key clockwise-p)
  (let ((points
          (if clockwise-p
              (list (slug-test-point left bottom)
                    (slug-test-point left top)
                    (slug-test-point right top)
                    (slug-test-point right bottom))
              (list (slug-test-point left bottom)
                    (slug-test-point right bottom)
                    (slug-test-point right top)
                    (slug-test-point left top)))))
    (loop for start in points
          for end in (append (rest points) (list (first points)))
          collect (slug:make-slug-line start end))))

(defun slug-test-curve-min (curve axis)
  (let ((reader (ecase axis
                  (:x #'slug:slug-point-x)
                  (:y #'slug:slug-point-y))))
    (apply #'min
           (mapcar reader
                   (list (slug:slug-quadratic-start curve)
                         (slug:slug-quadratic-control curve)
                         (slug:slug-quadratic-end curve))))))

(defun slug-test-curve-max (curve axis)
  (let ((reader (ecase axis
                  (:x #'slug:slug-point-x)
                  (:y #'slug:slug-point-y))))
    (apply #'max
           (mapcar reader
                   (list (slug:slug-quadratic-start curve)
                         (slug:slug-quadratic-control curve)
                         (slug:slug-quadratic-end curve))))))

(defun slug-test-curve-axis-parallel-p (curve axis)
  (= (slug-test-curve-min curve axis)
     (slug-test-curve-max curve axis)))

(deftest arbitrary-quadratic-contours-pack-into-sorted-slug-bands
  (let* ((outer (slug-test-square 0 0 4 4))
         (inner (slug-test-square 1 1 3 3 :clockwise-p t))
         (outline (slug:make-slug-outline :contours (list outer inner)))
         (packed
           (slug:pack-slug-outline
            outline :horizontal-band-count 2 :vertical-band-count 2))
         (curves (slug:slug-packed-outline-curves packed)))
    (ok (eq :counterclockwise (slug:slug-contour-orientation outer)))
    (ok (eq :clockwise (slug:slug-contour-orientation inner)))
    (ok (= 8 (length curves)))
    (ok (= 0 (slug:slug-packed-outline-min-x packed)))
    (ok (= 4 (slug:slug-packed-outline-max-y packed)))
    (ok (every
         (lambda (band)
           (let ((indices (slug:slug-band-curve-indices band)))
             (and
              ;; Horizontal ray bands omit horizontal curves.
              (every (lambda (index)
                       (let ((curve (aref curves index)))
                         (not (= (slug:slug-point-y
                                  (slug:slug-quadratic-start curve))
                                 (slug:slug-point-y
                                  (slug:slug-quadratic-control curve))
                                 (slug:slug-point-y
                                  (slug:slug-quadratic-end curve))))))
                     indices)
              ;; Descending maximum x supports the shader's early exit.
              (apply #'>=
                     (mapcar
                      (lambda (index)
                        (slug-test-curve-max (aref curves index) :x))
                      indices)))))
         (slug:slug-packed-outline-horizontal-bands packed)))
    (ok (every
         (lambda (band)
           (let ((descending (slug:slug-band-curve-indices band))
                 (ascending
                   (slug:slug-band-ascending-curve-indices band)))
             (and
              (every (lambda (index)
                       (not (slug-test-curve-axis-parallel-p
                             (aref curves index) :x)))
                     descending)
              (apply #'>=
                     (mapcar (lambda (index)
                               (slug-test-curve-max
                                (aref curves index) :y))
                             descending))
              (apply #'<=
                     (mapcar (lambda (index)
                               (slug-test-curve-min
                                (aref curves index) :y))
                             ascending)))))
         (slug:slug-packed-outline-vertical-bands packed)))
    (let ((line (first outer)))
      (ok (equalp (slug:slug-quadratic-control line)
                  (slug:slug-quadratic-end line))))))

(deftest malformed-slug-contours-report-the-broken-junction
  (let* ((first (slug-test-line 0 0 1 0))
         (second (slug-test-line 2 0 0 0))
         (outline (slug:make-slug-outline
                   :contours (list (list first second)))))
    (handler-case
        (progn
          (slug:slug-outline-curves outline)
          (ok nil))
      (slug:slug-outline-error (condition)
        (ok (eq :disconnected-contour
                (slug:slug-outline-error-reason condition)))
        (ok (equal '(0 0)
                   (slug:slug-outline-error-details condition)))))))

(deftest slug-texture-serialization-repacks-the-actual-half-values
  (let* ((contour (slug-test-square 0 0 1/3 1))
         (serialized
           (slug:serialize-slug-outline
            (slug:make-slug-outline :contours (list contour))
            :horizontal-band-count 2 :vertical-band-count 2))
         (packed (slug:slug-serialized-outline-packed-outline serialized))
         (curve-words
           (slug:slug-serialized-outline-curve-half-words serialized))
         (band-words
           (slug:slug-serialized-outline-band-uint16-words serialized))
         (curve-upload
           (slug:slug-serialized-outline-curve-upload-data serialized))
         (band-upload
           (slug:slug-serialized-outline-band-upload-data serialized)))
    (ok (= 4096 (slug:slug-serialized-outline-curve-width serialized)))
    (ok (= 4096 (slug:slug-serialized-outline-band-width serialized)))
    (ok (= 5 (slug:slug-serialized-outline-curve-texel-count serialized)))
    (ok (= 20 (length curve-words)))
    ;; The first curve's p3 is the following curve texel's p1.
    (ok (equalp (subseq curve-words 2 4) (subseq curve-words 4 6)))
    (ok (= 12 (slug:slug-serialized-outline-band-texel-count serialized)))
    (ok (equal '(4096 1)
               (slug:slug-serialized-outline-curve-texture-size serialized)))
    (ok (equal '(4096 1)
               (slug:slug-serialized-outline-band-texture-size serialized)))
    (ok (nth-value 0
          (subtypep (array-element-type curve-upload) '(unsigned-byte 64))))
    (ok (nth-value 0
          (subtypep (array-element-type band-upload) '(unsigned-byte 32))))
    (ok (= (row-major-aref curve-upload 0)
           (loop for index below 4
                 sum (ash (aref curve-words index) (* index 16)))))
    (ok (= #x00040002 (row-major-aref band-upload 0)))
    (ok (= 2 (aref band-words 0)))
    (ok (= 4 (aref band-words 1)))
    (ok (equalp #(1 0 3 0) (subseq band-words 8 12)))
    (ok (not (= 1/3 (slug:slug-packed-outline-max-x packed))))
    (ok (< (abs (- 1/3 (slug:slug-packed-outline-max-x packed)))
           1/1000))))

(deftest slug-serialization-defaults-to-spatial-bands
  (let ((serialized
          (slug:serialize-slug-outline
           (slug:make-slug-outline
            :contours (list (slug-test-square 0 0 1 1))))))
    (ok (= 4 (slug:slug-serialized-outline-horizontal-band-count serialized)))
    (ok (= 4 (slug:slug-serialized-outline-vertical-band-count serialized)))
    (ok (> (slug:slug-serialized-outline-band-texel-count serialized) 8))))

(deftest zpb-ttf-glyphs-enter-slug-before-software-rasterization
  (zpb-ttf:with-font-loader
      (font-loader (cl-dejavu:font-pathname "DejaVuSans.ttf"))
    (let* ((glyph (slug:load-slug-glyph #\O font-loader))
           (outline (slug:slug-glyph-outline glyph))
           (contours (slug:slug-outline-contours outline))
           (packed (slug:pack-slug-outline outline))
           (orientations
             (mapcar #'slug:slug-contour-orientation contours)))
      (ok (char= #\O (slug:slug-glyph-character glyph)))
      (ok (= 2048 (slug:slug-glyph-units-per-em glyph)))
      (ok (= 1612 (slug:slug-glyph-advance-width glyph)))
      (ok (= 2 (length contours)))
      (ok (= 16 (length (slug:slug-packed-outline-curves packed))))
      (ok (member :clockwise orientations))
      (ok (member :counterclockwise orientations))
      (let* ((normalized (slug:normalize-slug-glyph-outline glyph))
             (normalized-packed (slug:pack-slug-outline normalized)))
        (ok (= (slug:slug-packed-outline-min-x normalized-packed)
               (/ (slug:slug-packed-outline-min-x packed) 2048)))
        (ok (= (slug:slug-packed-outline-max-x normalized-packed)
               (/ (slug:slug-packed-outline-max-x packed) 2048)))
        (ok (= (slug:slug-packed-outline-min-y normalized-packed)
               (/ (slug:slug-packed-outline-min-y packed) 2048)))
        (ok (= (slug:slug-packed-outline-max-y normalized-packed)
               (/ (slug:slug-packed-outline-max-y packed) 2048))))
      (ok (every (lambda (contour)
                   (every (lambda (curve)
                            (not (equalp
                                  (slug:slug-quadratic-control curve)
                                  (slug:slug-quadratic-end curve))))
                          contour))
                 contours)))))

(deftest shaders-consume-shared-arithmetic-functions-directly
  (let* ((before (spv:shader-source-revision))
         (specification
           (spv:parse-shader-specification
            'shared-function-fragment
            '(:stage :fragment
              :inputs ((value :float :location 0))
              :outputs ((result :float :location 0)))
            '((set-output result
                          (shared-shader-function-probe value)))))
         (call
           (spv:shader-assignment-value
            (first (spv:shader-specification-statements specification)))))
    (ok (typep call 'spv:shader-function-call))
    (ok (typep call 'lang:arithmetic-function-call))
    (ok (typep (spv:shader-function-call-definition call)
               'lang:arithmetic-function-definition))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification specification) 0)))
    (lang:note-arithmetic-function-redefinition
     'shared-shader-function-probe)
    (ok (> (spv:shader-source-revision) before))))

(deftest shared-source-retires-an-older-shader-only-definition
  (eval '(spv:define-shader-function shared-source-migration-probe (value)
           (+ value 1.0)))
  (ok (spv:shader-function-definition-for
       'shared-source-migration-probe))
  (eval '(lang:define-arithmetic-function
             shared-source-migration-probe ((value))
           (+ value 2.0)))
  (ok (null (spv:shader-function-definition-for
             'shared-source-migration-probe)))
  (ok (lang:arithmetic-function-definition-for
       'shared-source-migration-probe)))

(deftest task-and-mesh-lower-to-validated-vulkan-shaped-spir-v
  (let* ((task-specification (vulkan-task-probe))
         (mesh-specification (vulkan-mesh-probe))
         (task-lowering
           (spv:lower-shader-specification :spir-v task-specification))
         (mesh-lowering
           (spv:lower-shader-specification :spir-v mesh-specification))
         (task-module (spv:shader-lowering-module task-lowering))
         (mesh-module (spv:shader-lowering-module mesh-lowering))
         (task-instructions (spv:lower-spir-v task-module))
         (mesh-instructions (spv:lower-spir-v mesh-module))
         (task-names (mapcar #'spv:instruction-name task-instructions))
         (mesh-names (mapcar #'spv:instruction-name mesh-instructions))
         (task-forms
           (write-to-string (mapcar #'spv:instruction-form task-instructions)))
         (mesh-forms
           (write-to-string (mapcar #'spv:instruction-form mesh-instructions)))
         (payload-expression
           (find-if (lambda (expression)
                      (typep expression 'spv:shader-payload-element))
                    (spv:shader-specification-expressions
                     mesh-specification))))
    (dolist (module (list task-module mesh-module))
      (ok (= #x00010400 (spv:spir-v-module-version module)))
      (ok (member 'spv::mesh-shading-ext
                  (spv:spir-v-module-capabilities module)))
      (ok (member 'spv::int64
                  (spv:spir-v-module-capabilities module)))
      (ok (equal '("SPV_EXT_mesh_shader")
                 (spv:spir-v-module-extensions module))))
    (ok (eq 'spv::task-ext
            (spv:spir-v-entry-point-execution-model
             (first (spv:spir-v-module-entry-points task-module)))))
    (ok (eq 'spv::mesh-ext
            (spv:spir-v-entry-point-execution-model
             (first (spv:spir-v-module-entry-points mesh-module)))))
    (ok (equal '(spv::local-size)
               (mapcar #'spv:spir-v-execution-mode-name
                       (spv:spir-v-module-execution-modes task-module))))
    (ok (equal '(spv::local-size spv::output-triangles-ext
                 spv::output-vertices spv::output-primitives-ext)
               (mapcar #'spv:spir-v-execution-mode-name
                       (spv:spir-v-module-execution-modes mesh-module))))
    (dolist (name '(spv::selection-merge spv::emit-mesh-tasks-ext))
      (ok (find name task-names)))
    (dolist (name '(spv::set-mesh-outputs-ext spv::selection-merge
                    spv::access-chain spv::store spv::u-convert))
      (ok (find name mesh-names)))
    (ng (find 'spv::return task-names))
    (ok (find 'spv::return mesh-names))
    (ok (search "TASK-PAYLOAD-WORKGROUP-EXT" task-forms))
    (ok (search "TASK-PAYLOAD-WORKGROUP-EXT" mesh-forms))
    (ok (search "PER-PRIMITIVE-EXT" mesh-forms))
    (ok (search "PRIMITIVE-TRIANGLE-INDICES-EXT" mesh-forms))
    (ok payload-expression)
    (ok (gethash payload-expression
                 (spv:shader-lowering-expression-instructions
                  mesh-lowering)))
    (dolist (specification (list task-specification mesh-specification))
      (let ((words (spv:assemble-shader-specification specification)))
        (ok (= #x07230203 (aref words 0)))
        (ok (= #x00010400 (aref words 1)))))))

(deftest shared-counted-fold-lowers-to-structured-spir-v
  (let* ((specification
           (spv:parse-shader-specification
            'fold-fragment
            '(:stage :fragment
              :inputs ((count :float :location 0))
              :outputs ((result :float :location 0)))
            '((set-output result (shared-fold-probe count)))))
         (names
           (mapcar #'spv:instruction-name
                   (spv:lower-spir-v
                    (spv:shader-module specification)))))
    (ok (find "PHI" names :key #'symbol-name :test #'string=))
    (ok (find "LOOP-MERGE" names :key #'symbol-name :test #'string=))
    (ok (find "BRANCH-CONDITIONAL" names
              :key #'symbol-name :test #'string=))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification specification) 0)))))

(deftest exact-unsigned-texel-loads-flow-through-shared-folds
  (let* ((specification (unsigned-texel-fold-probe))
         (module
           (spv:shader-lowering-module
            (spv:lower-shader-specification :spir-v specification)))
         (names
           (loop for function in (spv:spir-v-module-function-definitions module)
                 append
                 (loop for block in (spv:spir-v-function-basic-blocks function)
                       append (mapcar #'spv:instruction-name
                                      (spv:spir-v-basic-block-instructions
                                       block))))))
    (ok (spv:shader-type=
         :uvec4
         (spv:shader-expression-type
          (spv:shader-binding-expression
           (binding-named 'header specification)))))
    (dolist (name '(image-fetch u-mod u-div i-add u-less-than convert-u-to-f))
      (ok (find name names :test #'string-equal)))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification specification) 0)))))

(deftest slug-atlas-uses-fragment-derivatives-and-selected-bands
  (let* ((specification (slug:slug-atlas-fragment-specification))
         (module
           (spv:shader-lowering-module
            (spv:lower-shader-specification :spir-v specification)))
         (names
           (loop for function in (spv:spir-v-module-function-definitions module)
                 append
                 (loop for block in (spv:spir-v-function-basic-blocks function)
                       append (mapcar #'spv:instruction-name
                                      (spv:spir-v-basic-block-instructions
                                       block))))))
    (dolist (name '(d-pdx d-pdy select image-fetch))
      (ok (find name names :test #'string-equal)))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification specification) 0)))))

(deftest shared-conditionals-lower-inside-structured-spir-v-folds
  (let* ((specification
           (spv:parse-shader-specification
            'conditional-fold-fragment
            '(:stage :fragment
              :inputs ((count :float :location 0)
                       (limit :float :location 1))
              :outputs ((result :float :location 0)))
            '((set-output result
                          (shared-conditional-fold-probe count limit)))))
         (names
           (mapcar #'spv:instruction-name
                   (spv:lower-spir-v
                    (spv:shader-module specification)))))
    (ok (find "F-ORD-LESS-THAN" names
              :key #'symbol-name :test #'string=))
    (ok (find "SELECT" names :key #'symbol-name :test #'string=))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification specification) 0)))))

(deftest analytic-roundrect-distance-covers-the-fixed-shape-family
  (flet ((near (left right)
           (< (abs (- left right)) 1.0e-5)))
    ;; A two-by-one roundrect is one unit inside at its centre and exactly on
    ;; its straight right edge.
    (ok (near -1.0
              (analytic:roundrect-signed-distance 0.0 0.0 2.0 1.0 0.25)))
    (ok (near 0.0
              (analytic:roundrect-signed-distance 2.0 0.0 2.0 1.0 0.25)))
    ;; Radius equal to both half-extents is the ordinary circle distance.
    (ok (near 0.0
              (analytic:roundrect-signed-distance 0.6 0.8 1.0 1.0 1.0)))
    (ok (near 1.0
              (analytic:roundrect-signed-distance 2.0 0.0 1.0 1.0 1.0)))
    ;; Excessive and negative radii are normalized at the semantic boundary.
    (ok (near 0.0
              (analytic:roundrect-signed-distance 1.0 0.0 1.0 0.5 8.0)))
    (ok (near 0.0
              (analytic:roundrect-signed-distance 1.0 0.0 1.0 0.5 -1.0)))))

(deftest analytic-roundrect-proof-shares-distance-and-derivative-coverage
  (let* ((vertex (analytic:roundrect-vertex-specification))
         (fragment (analytic:roundrect-fragment-specification))
         (coverage (binding-named 'coverage fragment))
         (forms
           (write-to-string
            (mapcar #'spv:instruction-form
                    (spv:lower-spir-v
                     (spv:shader-module fragment))))))
    (ok (eq :vertex (spv:shader-specification-stage vertex)))
    (ok (eq :fragment (spv:shader-specification-stage fragment)))
    (ok (= 4 (length (spv:shader-specification-inputs vertex))))
    (ok (= 3 (length (spv:shader-specification-inputs fragment))))
    (ok (typep (spv:shader-binding-expression coverage)
               'spv:shader-function-call))
    (ok (eq 'analytic:roundrect-coverage
            (spv:shader-object-name
             (spv:shader-function-call-definition
              (spv:shader-binding-expression coverage)))))
    (ok (lang:arithmetic-function-definition-for
         'analytic:roundrect-signed-distance))
    (ok (search "D-PDX" forms))
    (ok (search "D-PDY" forms))
    (ok (search "SQRT" forms))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification vertex) 0)))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification fragment) 0)))))

(deftest slug-proof-is-a-pixel-shader-over-quadratic-roots
  (let* ((vertex (slug:slug-bezier-vertex-specification))
         (fragment (slug:slug-bezier-fragment-specification))
         (fragment-value
           (spv:shader-assignment-value
            (first (spv:shader-specification-statements fragment))))
         (lowering (spv:compile-shader-specification fragment))
         (forms
           (mapcar #'spv:instruction-form
                   (spv:lower-spir-v
                    (spv:shader-lowering-module lowering))))
         (printed (write-to-string forms)))
    (ok (eq :vertex (spv:shader-specification-stage vertex)))
    (ok (eq :fragment (spv:shader-specification-stage fragment)))
    (ok (= 3 (length (spv:shader-specification-inputs vertex))))
    (ok (= 2 (length (spv:shader-specification-inputs fragment))))
    (ok (typep fragment-value 'spv:shader-function-call))
    (ok (eq 'slug:slug-quadratic-outline
            (spv:shader-object-name
             (spv:shader-function-call-definition fragment-value))))
    (ok (not (spv:shader-abstraction-p 'slug:slug-quadratic-outline)))
    (ok (spv:shader-function-definition-for
         'slug:slug-quadratic-outline))
    (ok (some (lambda (expression)
                (typep expression 'spv:shader-function-call))
              (spv:shader-specification-expressions fragment)))
    (ok (search "F-SIGN" printed))
    (ok (search "SQRT" printed))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification vertex) 0)))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification fragment) 0)))))

(deftest slug-bands-are-two-data-driven-structured-traversals
  (let* ((specification (slug:slug-banded-fragment-specification))
         (instructions
           (spv:lower-spir-v (spv:shader-module specification)))
         (names (mapcar #'spv:instruction-name instructions)))
    (ok (= 2 (count "LOOP-MERGE" names
                    :key #'symbol-name :test #'string=)))
    (ok (>= (count "IMAGE-FETCH" names
                   :key #'symbol-name :test #'string=)
            6))
    (ok (find "U-MOD" names :key #'symbol-name :test #'string=))
    (ok (find "U-DIV" names :key #'symbol-name :test #'string=))
    (ok (= #x07230203
           (aref (spv:assemble-shader-specification specification) 0)))))

(deftest harfbuzz-shaping-selects-ligatures-and-preserves-clusters
  (let* ((font (cl-dejavu:font-pathname "DejaVuSans.ttf"))
         (shaped (slug:shape-slug-text "office" font))
         (glyphs (slug:slug-shaped-text-glyphs shaped)))
    (ok (= 2048 (slug:slug-shaped-text-units-per-em shaped)))
    ;; o, ffi, c, e: the three source characters at byte cluster 1 become one
    ;; glyph selected by HarfBuzz rather than three cmap lookups.
    (ok (= 4 (length glyphs)))
    (ok (equal '(0 1 4 5)
               (loop for glyph across glyphs
                     collect (slug:slug-shaped-glyph-cluster glyph))))
    (ok (= (slug:slug-shaped-text-x-advance shaped)
           (loop for glyph across glyphs
                 sum (slug:slug-shaped-glyph-x-advance glyph))))))

(deftest extended-math-signatures-are-explicit-contracts
  (flet ((failure-reason (body)
           (handler-case
               (progn
                 (spv:parse-shader-specification
                  'bad-extended-math
                  '(:stage :fragment
                    :inputs ((value :vec3 :location 0)
                             (scale :float :location 1)
                             (word :uint :location 2)
                             (uvalue :uvec2 :location 3))
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
            :wrong-operand-count))
    ;; Unsigned traversal values do not leak into float-only operations.
    (ok (eq (failure-reason '((set-output color (* uvalue scale))))
            :incompatible-product-types))
    (ok (eq (failure-reason '((set-output color (dot uvalue uvalue))))
            :invalid-dot-product))
    (ok (eq (failure-reason '((set-output color (mix word word scale))))
            :invalid-mix))
    (ok (eq (failure-reason '((set-output color (normalize uvalue))))
            :invalid-normalize))
    (ok (eq (failure-reason '((set-output color (min word word))))
            :invalid-extended-math-type))))

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

;;; Storage buffers and bit fields: the path a packed 64-bit site takes from a
;;; host array into a shader.

(spv:define-shader storage-site-fragment-probe
    (:stage :fragment
     :resources ((sites :storage-buffer :binding 1 :element :uint64)
                 (words :storage-buffer :binding 2 :element :uvec4))
     :outputs ((color :vec4 :location 0)))
  (let* ((term (spv:buffer-element sites (uint 3.0)))
         (extent (uint (ldb (byte 4 0) term)))
         (x (uint (ldb (byte 24 4) term)))
         (z (uint (ldb (byte 8 52) term)))
         (word (swizzle (spv:buffer-element words extent) :x))
         (high (ldb (byte 16 16) word))
         (whole (ldb (byte 32 0) word))
         (shade (/ (float (+ x z high whole)) 255.0)))
    (set-output color (vec4 shade shade shade 1.0))))

(defun storage-probe-error-reason (resources body)
  (handler-case
      (progn
        (spv:parse-shader-specification
         'storage-probe
         `(:stage :fragment
           :resources ,resources
           :outputs ((color :vec4 :location 0)))
         body)
        nil)
    (spv:shader-language-error (condition)
      (spv:shader-language-error-reason condition))))

(deftest storage-buffers-declare-typed-elements-and-index-them
  (let* ((specification (storage-site-fragment-probe))
         (sites (find 'sites (spv:shader-specification-resources specification)
                      :key #'spv:shader-object-name :test #'string-equal))
         (words (find 'words (spv:shader-specification-resources specification)
                      :key #'spv:shader-object-name :test #'string-equal))
         (term (binding-named 'term specification)))
    (ok (typep sites 'spv:shader-storage-buffer))
    (ok (eq (spv:find-shader-type :uint64)
            (spv:shader-storage-buffer-element-type sites)))
    (ok (= 8 (spv:shader-storage-buffer-element-stride sites)))
    (ok (= 16 (spv:shader-storage-buffer-element-stride words)))
    (ok (typep (spv:shader-binding-expression term) 'spv:shader-buffer-element))
    (ok (eq (spv:find-shader-type :uint64)
            (spv:shader-expression-type (spv:shader-binding-expression term))))
    (ok (typep (spv:shader-binding-expression (binding-named 'extent specification))
               'spv:shader-call))))

(deftest storage-buffers-and-bit-fields-reject-ill-typed-source
  (ok (eq :invalid-storage-buffer-element
          (storage-probe-error-reason
           '((sites :storage-buffer :binding 1))
           '((set-output color (vec4 1.0 1.0 1.0 1.0))))))
  (ok (eq :invalid-storage-buffer-element
          (storage-probe-error-reason
           '((sites :storage-buffer :binding 1 :element :uvec3))
           '((set-output color (vec4 1.0 1.0 1.0 1.0))))))
  (ok (eq :element-on-non-storage-buffer
          (storage-probe-error-reason
           '((sites :texture-2d :binding 1 :element :uint))
           '((set-output color (vec4 1.0 1.0 1.0 1.0))))))
  (ok (eq :storage-buffer-requires-element
          (storage-probe-error-reason
           '((sites :storage-buffer :binding 1 :element :vec4))
           '((set-output color sites)))))
  (ok (eq :buffer-index-type
          (storage-probe-error-reason
           '((sites :storage-buffer :binding 1 :element :vec4))
           '((set-output color (spv:buffer-element sites 1.0))))))
  (ok (eq :byte-specifier-exceeds-width
          (storage-probe-error-reason
           '((sites :storage-buffer :binding 1 :element :uint))
           '((let* ((word (spv:buffer-element sites (uint 0.0)))
                    (field (float (ldb (byte 8 28) word))))
               (set-output color (vec4 field field field 1.0)))))))
  (ok (eq :invalid-byte-specifier
          (storage-probe-error-reason
           '((sites :storage-buffer :binding 1 :element :uint))
           '((let* ((word (spv:buffer-element sites (uint 0.0)))
                    (field (float (ldb (byte 0 4) word))))
               (set-output color (vec4 field field field 1.0)))))))
  (ok (eq :invalid-bit-field-operand
          (storage-probe-error-reason
           '((sites :storage-buffer :binding 1 :element :float))
           '((let* ((word (spv:buffer-element sites (uint 0.0)))
                    (field (ldb (byte 8 4) word)))
               (set-output color (vec4 field field field 1.0))))))))

(deftest storage-buffers-lower-to-storage-class-runtime-arrays
  (let* ((specification (storage-site-fragment-probe))
         (module (spv:shader-module specification))
         (instructions (spv:lower-spir-v module))
         (names (mapcar #'spv:instruction-name instructions))
         (forms (write-to-string (mapcar #'spv:instruction-form instructions)))
         (words (spv:assemble-shader-specification specification)))
    (ok (equal '("SPV_KHR_storage_buffer_storage_class")
               (spv:spir-v-module-extensions module)))
    (ok (member 'spv::int64 (spv:spir-v-module-capabilities module)))
    (dolist (name '(spv::type-runtime-array spv::access-chain spv::load
                    spv::shift-left-logical spv::shift-right-logical))
      (ok (find name names)))
    ;; Bit fields never lower to bit-field instructions, which Vulkan limits
    ;; to 32-bit operands, nor to 64-bit mask constants.
    (ng (find 'spv::bitwise-and names))
    (ok (search "STORAGE-BUFFER" forms))
    (ok (search "ARRAY-STRIDE 8" forms))
    (ok (search "ARRAY-STRIDE 16" forms))
    (ok (search "NON-WRITABLE" forms))
    (ok (= #x07230203 (aref words 0)))))
