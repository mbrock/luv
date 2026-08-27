(defpackage #:luvcraft.tests
  (:use #:cl #:luv #:luvcraft #:luvcraft.world)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip)
  (:local-nicknames (#:shader #:luv.shader)
                    (#:spv #:luv.spir-v)
                    (#:shaders #:luvcraft.shaders)
                    (#:analytic #:luv.analytic)
                    (#:slug #:luv.slug)
                    (#:math #:luv.arithmetic)
                    (#:lang #:luv.arithmetic.language)
                    (#:msl #:luv.msl)
                    (#:wgsl #:luv.wgsl)
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
  (:import-from #:luv.shader
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

(define-test portable-operator-symbols-retain-shader-identity
  (true (eq 'shader:dot 'math:dot))
  (true (eq 'shader:clamp 'math:clamp))
  (true (eq 'shader:mix 'math:mix))
  (true (eq 'shader:smoothstep 'math:smoothstep))
  (true (eq 'shader:step 'math:step))
  (true (eq 'shader:normalize 'math:normalize))
  (true (eq 'shader:quantity 'lang:quantity))
  (true (eq 'shader:assume-quantity 'lang:assume-quantity))
  (true (eq 'shader:interpret 'lang:interpret))
  (true (eq 'shader:representation 'lang:representation))
  (true (eq 'shader:convert-unit 'lang:convert-unit)))

(define-test shared-shader-vocabulary-has-neutral-symbol-homes
  (dolist (symbol (list 'shader:shader-specification
                        'shader:shader-source-revision
                        'shader:lower-shader-call
                        'shader:lower-shader-specification))
    (true (eq (find-package '#:luv.shader) (symbol-package symbol)))
    (multiple-value-bind (found status)
        (find-symbol (symbol-name symbol) '#:luv.spir-v)
      (true (eq symbol found))
      (true (eq :inherited status))))
  (true (eq (find-package '#:luv.spir-v)
            (symbol-package 'spv:shader-lowering)))
  (true (null (find-symbol "SHADER-LOWERING" '#:luv.shader)))
  (multiple-value-bind (instruction-dot status)
      (find-symbol "DOT" '#:luv.spir-v)
    (true (eq :internal status))
    (true (eq (find-package '#:luv.spir-v)
              (symbol-package instruction-dot)))
    (true (not (eq instruction-dot 'shader:dot)))
    (true (typep (find-class instruction-dot)
                 'spv:instruction-class)))
  (true (null (find-class 'shader:dot nil)))
  (let ((foreign-instruction-names nil)
        (spir-v-package (find-package '#:luv.spir-v)))
    (do-symbols (symbol spir-v-package)
      (let ((class (find-class symbol nil)))
        (when (and (typep class 'spv:instruction-class)
                   (not (eq spir-v-package (symbol-package symbol))))
          (push symbol foreign-instruction-names))))
    (true (null foreign-instruction-names))))

(defun binding-named (name specification)
  (find name (shader:shader-specification-bindings specification)
        :key #'shader:shader-object-name
        :test (lambda (left right)
                (string-equal (symbol-name left) (symbol-name right)))))

(defun form-names (form)
  (cond ((symbolp form) (string-downcase (symbol-name form)))
        ((consp form) (mapcar #'form-names form))
        (t form)))

(defgeneric shader-method-probe (role stage))
(defgeneric shader-abstraction-method-probe (role stage))

(defconstant +shader-function-test-offset+ 0.25)

(shader:define-shader-function typed-shader-function-probe (value scale)
  "A lexical typed-function probe written without source-form construction."
  (let* ((shifted (+ value +shader-function-test-offset+))
         (scaled (* shifted scale)))
    scaled))

(lang:define-arithmetic-function shared-shader-function-probe ((value))
  (let* ((shifted (+ value 0.25)))
    (* shifted shifted)))

(shader:define-shader unsigned-texel-fold-probe
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

(shader:define-task-payload vulkan-task-mesh-payload
  (payload-site :uint64)
  (payload-position (:array :vec4 32)))

(shader:define-shader vulkan-task-probe
    (:stage :task
     :workgroup-size (32 1 1)
     :payload vulkan-task-mesh-payload
     :inputs ((lane :uint :built-in :local-invocation-index)
              (local-id :uvec3 :built-in :local-invocation-id)
              (group :uvec3 :built-in :workgroup-id)
              (group-count :uvec3 :built-in :num-workgroups)
              (threads :uvec3 :built-in :workgroup-size)))
  (let* ((three (shader:uint 3.0))
         (one (shader:uint 1.0)))
    (when (= lane (shader:uint 0.0))
      (shader:set-payload payload-site (shader:uint64 three)))
    (shader:set-payload-element
     payload-position lane (shader:vec4 0.0 0.0 0.0 1.0))
    (shader:emit-mesh-workgroups (shader:uvec3 one one one))))

(shader:define-shader vulkan-mesh-probe
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
  (let* ((vertex-count (shader:uint payload-site))
         (primitive-count (shader:uint 1.0)))
    (shader:set-mesh-output-counts vertex-count primitive-count)
    (when (< lane vertex-count)
      (shader:set-mesh-vertex
       lane
       (position (shader:payload-element payload-position lane))
       (uv (shader:vec2 0.0 0.0))))
    (when (= lane (shader:uint 0.0))
      (shader:set-mesh-primitive
       (shader:uint 0.0)
       (shader:uvec3 (shader:uint 0.0)
                  (shader:uint 1.0)
                  (shader:uint 2.0))
       (primitive-color (shader:vec4 1.0 1.0 1.0 1.0))))))

(lang:define-arithmetic-function shared-fold-probe ((count))
  (counted-fold (index count sum 0.0)
    (+ sum index)))

(lang:define-arithmetic-function shared-conditional-fold-probe
    ((count) (limit))
  (counted-fold (index count sum 0.0)
    (if (< index limit) (+ sum index) sum)))

(shader:define-shader-method shader-method-probe shader-method-probe
    ((role (eql :probe)) (stage (eql :fragment)))
    (:stage :fragment
     :outputs ((color :vec4 :location 0)))
  (set-output color (vec4 0.1 0.2 0.3 1.0)))

(shader:define-shader-method
    shader-abstraction-method-probe shader-abstraction-method-probe
    ((role (eql :abstraction-probe)) (stage (eql :fragment)))
    (:stage :fragment
     :inputs ((receiver :float :location 0)
              (depth :float :location 1)
              (bias :float :location 2))
     :outputs ((visibility :float :location 0)))
  (let* ((visible (test-shadow-rewrite receiver depth bias)))
    (set-output visibility visible)))

(define-test shader-method-redefinition-is-observable-and-coalesced
  (let* ((generic-function (fdefinition 'shader-method-probe))
         (dependent
           (shader:make-shader-definition-dependent
            generic-function '(:probe :fragment))))
    (unwind-protect
         (progn
           (true (not (shader:shader-definition-change-pending-p dependent)))
           (eval
            '(shader:define-shader-method
                 shader-method-probe shader-method-probe
                 ((role (eql :probe)) (stage (eql :fragment)))
                 (:stage :fragment
                  :outputs ((color :vec4 :location 0)))
               (set-output color (vec4 0.8 0.4 0.2 1.0))))
           (true (= 1 (length (closer-mop:generic-function-methods
                               generic-function))))
           (true (shader:shader-definition-change-pending-p dependent))
           (multiple-value-bind (revision event)
               (shader:shader-definition-change-snapshot dependent)
             ;; Replacement emits REMOVE-METHOD and ADD-METHOD on the pinned
             ;; SBCL/Closer-MOP stack.  Consumers see their coalesced revision.
             (true (>= revision 2))
             (true (eq (first event) 'add-method))
             (true (equal
                    (form-names
                     (shader:shader-expression-form
                      (shader:shader-assignment-value
                       (first
                        (shader:shader-specification-statements
                         (shader-method-probe :probe :fragment))))))
                    '("vec4" 0.8 0.4 0.2 1.0)))
             (shader:acknowledge-shader-definition-change dependent revision)
             (true (not (shader:shader-definition-change-pending-p dependent)))))
      (shader:release-shader-definition-dependent dependent))))

(define-test shader-functions-are-typed-calls-with-lexical-bindings
  (let* ((definition
           (shader:shader-function-definition-for 'typed-shader-function-probe))
         (specification
           (shader:parse-shader-specification
            'typed-shader-function-specification
            '(:stage :fragment
              :inputs ((value :float :location 0)
                       (scale :float :location 1))
              :outputs ((result :float :location 0)))
            '((set-output result
                          (typed-shader-function-probe value scale)))))
         (call
           (shader:shader-assignment-value
            (first (shader:shader-specification-statements specification))))
         (lowering (spv:compile-shader-specification specification)))
    (true (typep definition 'shader:shader-function-definition))
    (true (equal '(value scale) (shader:shader-function-parameters definition)))
    (true (search "without source-form construction"
                  (documentation 'typed-shader-function-probe
                                 'shader:shader-function)))
    (true (typep call 'shader:shader-function-call))
    (true (eq definition (shader:shader-function-call-definition call)))
    (true (= 2 (length (shader:shader-function-call-arguments call))))
    ;; Two parameter aliases and two lexical LET* bindings remain typed
    ;; objects.  Only computed locals need entry-block declarations.
    (true (= 4 (length (shader:shader-function-call-bindings call))))
    (true (= 2 (length (shader:shader-specification-bindings specification))))
    (true (equal '(typed-shader-function-probe value scale)
                 (shader:shader-expression-form call)))
    (true (gethash call
                   (spv:shader-lowering-expression-instructions lowering)))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))))

(define-test shader-function-redefinition-affects-only-fresh-parses
  (labels ((install-addition ()
             (eval
              '(shader:define-shader-function redefinable-shader-function
                   (left right)
                 (+ left right))))
           (install-subtraction ()
             (eval
              '(shader:define-shader-function redefinable-shader-function
                   (left right)
                 (- left right))))
           (parse-probe ()
             (shader:parse-shader-specification
              'redefinable-shader-function-specification
              '(:stage :fragment
                :inputs ((left :float :location 0)
                         (right :float :location 1))
                :outputs ((result :float :location 0)))
              '((set-output result
                            (redefinable-shader-function left right)))))
           (result-operator (specification)
             (let ((call
                     (shader:shader-assignment-value
                      (first
                       (shader:shader-specification-statements specification)))))
               (shader:shader-call-operator
                (shader:shader-function-call-result call)))))
    (install-addition)
    (let ((addition (parse-probe))
          (revision (shader:shader-source-revision)))
      (install-subtraction)
      (let ((subtraction (parse-probe)))
        (true (> (shader:shader-source-revision) revision))
        (true (eq '+ (result-operator addition)))
        (true (eq '- (result-operator subtraction)))
        (fail
         (shader:parse-shader-specification
          'bad-shader-function-arity
          '(:stage :fragment
            :inputs ((left :float :location 0))
            :outputs ((result :float :location 0)))
          '((set-output result (redefinable-shader-function left))))
         'shader:shader-language-error)))))

(define-test top-level-shader-redefinition-advances-the-live-source-revision
  (flet ((definition (macro name red)
           `(,macro ,name
                (:stage :fragment
                 :outputs ((color :vec4 :location 0)))
              (set-output color (vec4 ,red 0.0 0.0 1.0)))))
    (dolist (case '((shader:define-shader
                     revision-static-stage-probe)
                    (shader:define-live-shader
                     revision-live-stage-probe)))
      (destructuring-bind (macro name) case
        (let ((before (shader:shader-source-revision)))
          (eval (definition macro name 0.25))
          (let ((first (shader:shader-source-revision)))
            (true (> first before))
            (eval (definition macro name 0.75))
            (true (> (shader:shader-source-revision) first))))))))

(define-test shader-source-name-can-migrate-from-rewriter-to-typed-function
  (eval
   '(shader:define-shader-abstraction source-kind-migration-probe (value)
      `(+ ,value 1.0)))
  (true (shader:shader-abstraction-p 'source-kind-migration-probe))
  (eval
   '(shader:define-shader-function source-kind-migration-probe (value)
      (+ value 1.0)))
  (true (not (shader:shader-abstraction-p 'source-kind-migration-probe)))
  (true (shader:shader-function-definition-for 'source-kind-migration-probe))
  (let* ((specification
           (shader:parse-shader-specification
            'source-kind-migration-specification
            '(:stage :fragment
              :inputs ((value :float :location 0))
              :outputs ((result :float :location 0)))
            '((set-output result (source-kind-migration-probe value)))))
         (call
           (shader:shader-assignment-value
            (first (shader:shader-specification-statements specification)))))
    (true (typep call 'shader:shader-function-call))))

(define-test shader-source-is-a-typed-clos-graph
  (let* ((specification (shaders:block-world-fragment-specification))
         (sun-direction (binding-named 'sun-direction specification))
         (sun-visibility (binding-named 'sun-visibility specification))
         (direct-shadow (binding-named 'direct-shadow specification))
         (sky-light (binding-named 'sky-light specification))
         (reflected (binding-named 'reflected specification))
         (radiance (binding-named 'radiance specification))
         (fogged (binding-named 'fogged specification)))
    (true (typep specification 'shader:shader-specification))
    (true (eq (shader:shader-specification-stage specification) :fragment))
    (true (= (length (shader:shader-specification-inputs specification)) 9))
    (true (= (length (shader:shader-specification-resources specification)) 7))
    (true (typep (shader:shader-binding-expression sun-direction)
                 'shader:shader-call))
    (true (typep (shader:shader-binding-expression sun-direction)
                 'lang:arithmetic-call))
    (true (equal
           (shader:shader-expression-form
            (shader:shader-binding-expression sun-direction))
           (lang:arithmetic-expression-form
            (shader:shader-binding-expression sun-direction))))
    (true (eq :world-direction
              (math:quantity-specification-name
               (shader:shader-expression-quantity-specification
                (shader:shader-binding-expression sun-direction)))))
    (true (shader:shader-type=
           (shader:shader-expression-type
            (shader:shader-binding-expression sun-direction))
           :vec3))
    (true (equal
           (form-names
            (shader:shader-expression-form
             (shader:shader-binding-expression sun-visibility)))
           '("smoothstep"
             ("quantity" 0.9 "quantity" "sky-light-level" "unit" "one")
             ("quantity" 1.0 "quantity" "sky-light-level" "unit" "one")
             "sky-input")))
    ;; The map's answer is taken only where the surface faces the light; a
    ;; surface turned away is lit by nothing, so it is also shadowed by
    ;; nothing.  #0604PY
    (true (equal
           (form-names
            (shader:shader-expression-form
             (shader:shader-binding-expression
              (binding-named 'sampled-shadow specification))))
           '("mix" 1.0 "shadow-sample" "shadow-in-bounds")))
    (true (equal
           (form-names
            (shader:shader-expression-form
             (shader:shader-binding-expression direct-shadow)))
           '("mix" 1.0 "sampled-shadow" "shadow-relevance")))
    (true (shader:shader-type=
           (shader:shader-expression-type
            (shader:shader-binding-expression sky-light))
           :vec3))
    (true (equal (form-names
                  (shader:shader-expression-form
                   (shader:shader-binding-expression reflected)))
                 '("interpret"
                   ("*" "albedo"
                    ("+" "sky-light" "sun-light" "local-light"))
                   "quantity" "linear-rgb" "unit" "one")))
    (true (equal (form-names
                  (shader:shader-expression-form
                   (shader:shader-binding-expression radiance)))
                 '("+" "reflected" "specular"
                   ("interpret" ("*" "albedo" "emission-input")
                    "quantity" "linear-rgb" "unit" "one"))))
    (true (equal (form-names
                  (shader:shader-expression-form
                   (shader:shader-binding-expression fogged)))
                 '("mix" "radiance" "fog-color" "fog-amount")))
    (dolist (binding (list sky-light reflected radiance fogged))
      (true (eq :absolute
                (math:quantity-specification-character
                 (shader:shader-expression-quantity-specification
                  (shader:shader-binding-expression binding))))))
    (true (> (length (shader:shader-specification-expressions specification))
             (length (shader:shader-specification-bindings specification))))))

(define-test block-vertex-source-is-a-typed-clos-graph-with-an-explicit-abi
  (let* ((specification (shaders:block-world-vertex-specification))
         (resource (first (shader:shader-specification-resources specification)))
         (clip-position
           (find 'clip-position
                 (shader:shader-specification-outputs specification)
                 :key #'shader:shader-object-name
                 :test (lambda (left right)
                         (string-equal (symbol-name left)
                                       (symbol-name right)))))
         (fog-amount (binding-named 'fog-amount specification))
         (relative (binding-named 'relative specification))
         (view-z (binding-named 'view-z specification))
         (shadow-projection
           (binding-named 'shadow-projection specification)))
    (true (typep specification 'shader:shader-specification))
    (true (eq (shader:shader-specification-stage specification) :vertex))
    (true (= (length (shader:shader-specification-inputs specification)) 5))
    (true (= (length (shader:shader-specification-outputs specification)) 10))
    (true (eq (shader:shader-interface-built-in clip-position) :position))
    (true (typep resource 'shader:shader-uniform-block))
    (true (= (shader:shader-resource-binding resource) 2))
    (true (equal (mapcar (lambda (member)
                           (string-downcase
                            (symbol-name (shader:shader-object-name member))))
                         (shader:shader-uniform-block-members resource))
                 '("camera-vector" "right-vector" "up-vector" "forward-vector"
                   "projection-vector" "fog-vector"
                   "sun-vector" "sun-color-vector" "zenith-vector"
                   "horizon-vector" "ambient-vector" "fog-color-vector"
                   "shadow-control-vector" "shadow-filter-vector"
                   "atlas-vector"
                   "shadow-row-x" "shadow-row-y"
                   "shadow-row-z" "shadow-row-w")))
    (true (equal (mapcar #'shader:shader-uniform-member-offset
                         (shader:shader-uniform-block-members resource))
                 '(0 16 32 48 64 80 96 112 128 144 160 176
                   192 208 224 240 256 272 288)))
    (true (= (shader:shader-uniform-block-byte-size resource) 304))
    (let ((fog-call (shader:shader-binding-expression fog-amount)))
      (true (typep fog-call 'shader:shader-function-call))
      (true (eq
             (lang:arithmetic-function-definition-for
              'luvcraft.arithmetic:fog-amount-at-view-distance)
             (shader:shader-function-call-definition fog-call)))
      (true (equal
             (form-names (shader:shader-expression-form fog-call))
             '("fog-amount-at-view-distance"
               "view-z" "fog-near" "fog-far"))))
    (let ((relative-quantity
            (shader:shader-expression-quantity-specification
             (shader:shader-binding-expression relative)))
          (view-z-quantity
            (shader:shader-expression-quantity-specification
             (shader:shader-binding-expression view-z))))
      (true (eq :world-position
                (math:quantity-specification-name relative-quantity)))
      (true (not (math:quantity-specification-affine-p relative-quantity)))
      (true (math:unit-expression=
             :cell (math:quantity-specification-unit relative-quantity)))
      (true (eq :view-distance
                (math:quantity-specification-name view-z-quantity)))
      (true (math:unit-expression=
             :cell (math:quantity-specification-unit view-z-quantity))))
    (true (typep (shader:shader-binding-expression shadow-projection)
                 'shader:shader-map-projection))))

(define-test projective-maps-are-semantic-objects-with-packed-products
  (let* ((specification (shaders:block-world-vertex-specification))
         (projection-binding
           (binding-named 'shadow-projection specification))
         (projection (shader:shader-binding-expression projection-binding))
         (application (shader:shader-map-projection-application projection))
         (definition (shader:shader-map-definition-for :world-to-light))
         (domain
           (shader:shader-map-domain-quantity-specification definition))
         (layout
           (shader:shader-projective-map-sample-quantity-layout definition))
         (uv (math:project-quantity-layout layout '(0 1)))
         (depth (math:project-quantity-layout layout '(2))))
    (true (typep definition 'shader:shader-projective-map-definition))
    (true (eq definition (shader:shader-map-application-definition application)))
    (true (eq application
              (shader:shader-map-projection-application projection)))
    (true (shader:shader-type= :vec3 (shader:shader-map-domain-type definition)))
    (true (shader:shader-type=
           :vec4 (shader:shader-projective-map-homogeneous-type definition)))
    (true (shader:shader-type=
           :vec3 (shader:shader-projective-map-sample-type definition)))
    (true (eq :world-position
              (math:quantity-specification-name domain)))
    (true (math:quantity-specification-affine-p domain))
    (true (math:unit-expression=
           :cell (math:quantity-specification-unit domain)))
    (true (eq :shadow-uv (math:quantity-specification-name uv)))
    (true (math:quantity-specification-affine-p uv))
    (true (eq :shadow-depth (math:quantity-specification-name depth)))
    (true (math:quantity-specification-affine-p depth))
    (true (math:quantity-layout=
           layout (shader:shader-expression-quantity-layout projection)))
    (true (null (shader:shader-expression-quantity-layout application)))
    (true (equal '(1/2 1/2 1)
                 (shader:shader-projective-map-coordinate-scale definition)))
    (true (equal '(1/2 1/2 0)
                 (shader:shader-projective-map-coordinate-offset definition)))
    (true (= 4 (length (shader:shader-map-application-rows application))))
    (true (every (lambda (row)
                   (not (shader:shader-expression-quantity-checked-p row)))
                 (shader:shader-map-application-rows application)))
    (true (shader:shader-expression-materialized-p application))
    (true (not (shader:shader-expression-materialized-p projection)))
    (labels ((contains-representation-p (expression)
               (or (typep expression 'shader:shader-representation)
                   (some #'contains-representation-p
                         (shader:shader-expression-children expression)))))
      (true (not (contains-representation-p application))))))

(define-test projective-map-applications-reject-undefined-or-wrong-semantics
  (flet ((reason-for (point-declaration form &optional annotated-row-p)
           (handler-case
               (progn
                 (shader:parse-shader-specification
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
             (shader:shader-language-error (condition)
               (shader:shader-language-error-reason condition)))))
    (let ((world
            '(position :vec3 :location 0
              :quantity :world-position :unit :cell :affine-p t))
          (raw '(position :vec3 :location 0))
          (direction
            '(position :vec3 :location 0
              :quantity :world-direction :unit :one)))
      (true (eq :undefined-shader-map
                (reason-for
                 world
                 '(project-point :missing-map position
                   row-x row-y row-z row-w))))
      (true (eq :projective-map-domain-mismatch
                (reason-for
                 raw
                 '(project-point :world-to-light position
                   row-x row-y row-z row-w))))
      (true (eq :projective-map-domain-mismatch
                (reason-for
                 direction
                 '(project-point :world-to-light position
                   row-x row-y row-z row-w))))
      (true (eq :projective-map-row-count
                (reason-for
                 world
                 '(project-point :world-to-light position
                   row-x row-y row-z))))
      (true (eq :invalid-projective-map-rows
                (reason-for
                 world
                 '(project-point :world-to-light position
                   row-x row-y row-z row-w)
                 t)))
      (true (eq :sampling-projection-requires-map-application
                (handler-case
                    (progn
                      (shader:parse-shader-specification
                       'invalid-sampling-projection-probe
                       '(:stage :vertex
                         :inputs ((raw-clip :vec4 :location 0))
                         :outputs
                         ((result :vec2 :location 0
                                  :quantity :shadow-uv :unit :one)))
                       '((set-output
                          result (swizzle (project-sample raw-clip) :xy))))
                      nil)
                  (shader:shader-language-error (condition)
                    (shader:shader-language-error-reason condition))))))))

(define-test block-vertex-uniform-members-retain-access-chain-provenance
  (let* ((lowering (shaders:block-world-vertex-lowering))
         (specification (spv:shader-lowering-specification lowering))
         (camera (binding-named 'camera specification))
         (reference
           (first (shader:shader-call-operands
                   (shader:shader-binding-expression camera))))
         (instructions
           (gethash reference
                    (spv:shader-lowering-expression-instructions lowering)))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    (true (find "ACCESS-CHAIN" names :test #'string=))
    (true (find "LOAD" names :test #'string=))))

(define-test block-shadow-vertex-is-a-light-space-depth-shader
  (let* ((specification (shaders:block-world-shadow-vertex-specification))
         (resource (first (shader:shader-specification-resources specification)))
         (clip (binding-named 'clip specification))
         (clip-position
           (first (shader:shader-specification-statements specification))))
    (true (eq (shader:shader-specification-stage specification) :vertex))
    (true (= (length (shader:shader-specification-inputs specification)) 1))
    (true (= (length (shader:shader-specification-outputs specification)) 1))
    (true (typep resource 'shader:shader-uniform-block))
    (true (= (shader:shader-resource-binding resource) 2))
    (let ((application (shader:shader-binding-expression clip)))
      (true (typep application 'shader:shader-map-application))
      (true (eq (shader:shader-map-definition-for :world-to-light)
                (shader:shader-map-application-definition application)))
      (true (shader:shader-type=
             (shader:shader-expression-type application) :vec4)))
    (true (shader:shader-type=
           (shader:shader-expression-type
            (shader:shader-assignment-value clip-position))
           :vec4))
    (true (> (length (shaders:block-world-shadow-vertex-shader)) 5))))

(define-test uniform-blocks-do-not-pretend-to-implement-general-packing
  (fail
   (shader:parse-shader-specification
    'test-uniform-layout
    '(:stage :vertex
      :resources ((state :uniform-block :binding 0
                   :members ((unsupported :float))))
      :outputs ((position :vec4 :built-in :position)))
    '((set-output position (vec4 0.0 0.0 0.0 1.0))))
   'shader:shader-language-error))

(define-test lowering-retains-expression-to-ssa-provenance
  (let* ((lowering (shaders:block-world-fragment-lowering))
         (specification (spv:shader-lowering-specification lowering))
         (reflected-expression
           (shader:shader-binding-expression
            (binding-named 'reflected specification)))
         (instructions
           (gethash reflected-expression
                    (spv:shader-lowering-expression-instructions lowering))))
    (true (typep (spv:shader-lowering-module lowering) 'spv:spir-v-module))
    (true instructions)
    (true (every (lambda (instruction) (typep instruction 'spv:instruction))
                 instructions))
    (true (find "F-MUL" instructions
                :key (lambda (instruction)
                       (symbol-name (spv:instruction-name instruction)))
                :test #'string=))
    (true (some (lambda (instruction)
                  (member reflected-expression
                          (gethash instruction
                                   (spv:shader-lowering-instruction-expressions
                                    lowering))
                          :test #'eq))
                instructions))))

(define-test constants-and-reused-loads-retain-occurrence-provenance
  (let* ((specification
           (shader:parse-shader-specification
            'reuse-input
            '(:stage :fragment
              :inputs ((value :float :location 0))
              :outputs ((color :float :location 0)))
            '((let* ((twice (+ value value)))
                (set-output color twice)))))
         (lowering (spv:compile-shader-specification specification))
         (call (shader:shader-binding-expression
                (binding-named 'twice specification)))
         (references (shader:shader-call-operands call))
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
            (shader:shader-call-operands
             (shader:shader-quantity-construction-operand
              (shader:shader-binding-expression torch-color)))))
         (constant-instructions
           (gethash literal
                    (spv:shader-lowering-expression-instructions
                     block-lowering))))
    (true (= (length left-instructions) 1))
    (true (eq (first left-instructions) (first right-instructions)))
    (true (= (length constant-instructions) 1))
    (true (string-equal
           (symbol-name
            (spv:instruction-name (first constant-instructions)))
           "constant"))))

(define-test vector-scalar-division-lowers-through-a-reciprocal
  (let* ((specification
           (shader:parse-shader-specification
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
    (true (find "F-DIV" names :test #'string=))
    (true (find "VECTOR-TIMES-SCALAR" names :test #'string=))
    (true (> (length (spv:assemble-shader-specification specification)) 5))))

(define-test a-first-use-vector-constructor-cannot-claim-its-type-id
  (let ((specification
          (shader:parse-shader-specification
           'first-use-vector-constructor
           '(:stage :fragment
             :outputs ((color :vec4 :location 0)))
           '((let* ((rgb (shader:vec3 0.0 0.0 0.0)))
               (set-output color (vec4 rgb 1.0)))))))
    ;; VEC3 has no interface declaration to predeclare its type.  Assembly is
    ;; therefore the direct proof that the type and its first value received
    ;; distinct result IDs.
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))))

(define-test canonical-shader-id-reservations-never-reuse-a-claimed-id
  (let* ((context (make-instance 'spv::shader-lowering-context))
         (first (spv::reserve-shader-id context 'probe))
         (second (spv::reserve-shader-id context 'probe)))
    (true (not (eq first second)))
    (true (string= "%PROBE" (symbol-name first)))
    (true (string= "%PROBE-2" (symbol-name second)))))

(define-test depth-texture-sampling-feeds-ordinary-float-math
  (let* ((specification
           (shader:parse-shader-specification
            'depth-sample
            '(:stage :fragment
              :inputs ((uv :vec2 :location 0)
                       (receiver-depth :float :location 1))
              :outputs ((visibility :float :location 0))
              :resources ((shadow-map :depth-texture-2d :binding 0)
                          (shadow-sampler :sampler :binding 1)))
            '((let* ((stored-depth (sample shadow-map shadow-sampler uv))
                     (depth-lane (swizzle stored-depth :x))
                     (visible (shader:step receiver-depth depth-lane)))
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
    (true (shader:shader-type=
           (shader:shader-expression-type
            (shader:shader-binding-expression stored-depth))
           :vec4))
    (true (shader:shader-type=
           (shader:shader-expression-type
            (shader:shader-binding-expression depth-lane))
           :float))
    (true (shader:shader-type=
           (shader:shader-expression-type
            (shader:shader-binding-expression visible))
           :float))
    (true (find "IMAGE-SAMPLE-IMPLICIT-LOD" names :test #'string=))
    (true (find "EXT-INST" names :test #'string=))
    (true (> (length (spv:assemble-shader-specification specification)) 5))))

(define-test shader-arithmetic-carries-backend-neutral-quantity-specifications
  (flet ((parse-depth-probe (annotated-p)
           (shader:parse-shader-specification
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
           (receiver (first (shader:shader-specification-inputs annotated)))
           (biased (binding-named 'biased annotated))
           (receiver-specification
             (shader:shader-declaration-quantity-specification receiver))
           (biased-specification
             (shader:shader-expression-quantity-specification
              (shader:shader-binding-expression biased))))
      (true (eq :shadow-depth
                (math:quantity-specification-name receiver-specification)))
      (true (math:quantity-specification-affine-p receiver-specification))
      (true (math:quantity-specification-affine-p biased-specification))
      ;; Semantic checking is a source concern; it does not perturb the SPIR-V
      ;; representation or the deterministic lowering of valid arithmetic.
      (true (equalp (spv:assemble-shader-specification annotated)
                    (spv:assemble-shader-specification plain))))))

(define-test shader-arithmetic-rejects-dimensionally-or-affinely-invalid-forms
  (labels ((reason-for (options body)
             (handler-case
                 (progn
                   (shader:parse-shader-specification
                    'invalid-semantic-probe options body)
                   nil)
               (shader:shader-language-error (condition)
                 (list (shader:shader-language-error-reason condition)
                       (shader:shader-language-error-details condition))))))
    (true (equal
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
    (true (equal
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

(define-test annotated-shader-arithmetic-is-total-and-unit-exact
  (labels ((reason-for (inputs expression)
             (handler-case
                 (progn
                   (shader:parse-shader-specification
                    'semantic-totality-probe
                    `(:stage :fragment
                      :inputs ,inputs
                      :outputs ((result :float :location 0)))
                    `((let* ((value ,expression))
                        (set-output result value))))
                   nil)
               (shader:shader-language-error (condition)
                 (list (shader:shader-language-error-reason condition)
                       (shader:shader-language-error-details condition))))))
    (let* ((specification
             (shader:parse-shader-specification
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
      (true (math:unit-expression=
             :metre
             (math:quantity-specification-unit
              (shader:shader-expression-quantity-specification
               (shader:shader-binding-expression value))))))
    (true (equal
           '(:invalid-quantity-operation :different-units)
           (reason-for
            '((left :float :location 0 :quantity :distance
                    :dimension :length :unit :metre)
              (right :float :location 1 :quantity :distance
                     :dimension :length :unit :kilometre))
            '(max left right))))
    (true (equal
           '(:invalid-quantity-operation :unknown-operator)
           (reason-for
            '((value :float :location 0 :quantity :distance
                     :dimension :length :unit :metre))
            '(abs value))))
    (true (equal
           '(:missing-quantity-specification (RIGHT))
           (reason-for
            '((left :float :location 0 :quantity :distance
                    :dimension :length :unit :metre)
              (right :float :location 1))
            '(+ left right))))))

(define-test packed-gpu-vectors-project-as-semantic-products
  (let* ((specification
           (shader:parse-shader-specification
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
         (packed (first (shader:shader-specification-inputs specification)))
         (layout (shader:shader-declaration-quantity-layout packed))
         (position (binding-named 'position specification))
         (sample-value (binding-named 'sample-value specification)))
    (true (null (shader:shader-declaration-quantity-specification packed)))
    (true (= 3 (math:quantity-layout-extent layout)))
    (true (eq :sample-position
              (math:quantity-specification-name
               (shader:shader-expression-quantity-specification
                (shader:shader-binding-expression position)))))
    (true (eq :sample-value
              (math:quantity-specification-name
               (shader:shader-expression-quantity-specification
                (shader:shader-binding-expression sample-value))))))
  (labels ((reason-for (expression)
             (handler-case
                 (progn
                   (shader:parse-shader-specification
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
               (shader:shader-language-error (condition)
                 (shader:shader-language-error-reason condition)))))
    (true (eq :undeclared-quantity-projection
              (reason-for '(swizzle packed :yz))))
    (true (eq :missing-quantity-specification
              (reason-for '(+ packed packed)))))
  (let* ((specification
           (shader:parse-shader-specification
            'homogeneous-component-probe
            '(:stage :fragment
              :inputs ((position :vec3 :location 0
                                 :quantity :test-position))
              :outputs ((x-output :float :location 0
                                  :quantity :test-position-x)))
            '((set-output x-output (swizzle position :x)))))
         (x (shader:shader-assignment-value
             (first (shader:shader-specification-statements specification)))))
    (true (eq :test-position-x
              (math:quantity-specification-name
               (shader:shader-expression-quantity-specification x))))))

(define-test texture-samples-can-publish-semantic-channel-layouts
  (let* ((specification
           (shader:parse-shader-specification
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
         (image (first (shader:shader-specification-resources specification)))
         (texel (binding-named 'texel specification))
         (rgb (binding-named 'rgb specification))
         (alpha (binding-named 'alpha specification)))
    (true (eq :srgb-to-linear
              (shader:shader-resource-sample-transfer image)))
    (true (math:quantity-layout=
           (shader:shader-resource-sample-quantity-layout image)
           (shader:shader-expression-quantity-layout
            (shader:shader-binding-expression texel))))
    (true (eq :linear-rgb
              (math:quantity-specification-name
               (shader:shader-expression-quantity-specification
                (shader:shader-binding-expression rgb)))))
    (true (eq :opacity
              (math:quantity-specification-name
               (shader:shader-expression-quantity-specification
                (shader:shader-binding-expression alpha)))))))

(define-test sample-transfer-metadata-is-texture-only
  (flet ((reason-for (resource)
           (handler-case
               (progn
                 (shader:parse-shader-specification
                  'invalid-sample-transfer-probe
                  `(:stage :fragment
                    :outputs ((result :float :location 0))
                    :resources (,resource))
                  '((set-output result 1.0)))
                 nil)
             (shader:shader-language-error (condition)
               (shader:shader-language-error-reason condition)))))
    (true (eq :invalid-sample-transfer
              (reason-for
               '(image :texture-2d :binding 0
                       :sample-transfer :mystery-transfer))))
    (true (eq :sample-semantics-on-non-texture
              (reason-for
               '(sampler :sampler :binding 0
                         :sample-transfer :srgb-to-linear))))))

(define-test semantic-boundaries-are-distinct-checked-and-have-no-codegen-effect
  (flet ((probe (annotated-p)
           (shader:parse-shader-specification
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
      (true (typep (shader:shader-binding-expression typed-left)
                   'shader:shader-quantity-assumption))
      (true (typep (shader:shader-binding-expression sum)
                   'shader:shader-interpretation))
      (true (eq :combined-factor
                (math:quantity-specification-name
                 (shader:shader-expression-quantity-specification
                  (shader:shader-binding-expression sum)))))
      (true (typep (shader:shader-binding-expression represented)
                   'shader:shader-representation))
      (true (not (shader:shader-expression-quantity-checked-p
                  (shader:shader-binding-expression represented))))
      (true (null (shader:shader-expression-quantity-specification
                   (shader:shader-binding-expression represented))))
      (true (typep (shader:shader-binding-expression recovered)
                   'shader:shader-quantity-assumption))
      (true (equalp (spv:assemble-shader-specification annotated)
                    (spv:assemble-shader-specification plain)))))
  (flet ((probe (constructed-p)
           (shader:parse-shader-specification
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
             (shader:shader-assignment-value
              (first (shader:shader-specification-statements constructed)))))
      (true (typep expression 'shader:shader-quantity-construction))
      (true (equalp (spv:assemble-shader-specification constructed)
                    (spv:assemble-shader-specification plain)))))
  (flet ((reason-for (form)
           (handler-case
               (progn
                 (shader:parse-shader-specification
                  'invalid-semantic-boundary-probe
                  '(:stage :fragment
                    :inputs ((value :float :location 0))
                    :outputs ((result :float :location 0)))
                  `((set-output result ,form)))
                 nil)
             (shader:shader-language-error (condition)
               (shader:shader-language-error-reason condition)))))
    ;; INTERPRET can name checked arithmetic, but cannot smuggle meaning onto
    ;; a raw input.  ASSUME-QUANTITY is the deliberately loud boundary for it.
    (true (eq :representation-requires-quantity
              (reason-for '(representation value))))
    (true (eq :invalid-quantity-interpretation
              (reason-for
               '(interpret value :quantity :distance
                           :dimension :length :unit :metre))))
    (true (eq :invalid-quantity-interpretation
              (reason-for
               '(interpret
                 (assume-quantity value :quantity :distance
                                  :dimension :length :unit :metre)
                 :quantity :distance
                 :dimension :length :unit :kilometre))))
    (true (eq :invalid-quantity-interpretation
              (reason-for
               '(interpret
                 (assume-quantity value :quantity :height
                                  :dimension :length :unit :metre)
                 :quantity :width
                 :dimension :length :unit :metre))))))

(define-test production-vertex-interfaces-carry-quantities-end-to-end
  (let* ((specification (shaders:block-world-vertex-specification))
         (inputs (shader:shader-specification-inputs specification))
         (outputs (shader:shader-specification-outputs specification))
         (position (first inputs))
         (uv-shade (second inputs))
         (normal (third inputs))
         (light (fourth inputs))
         (fog (fourth outputs))
         (shadow-uv (sixth outputs))
         (shadow-depth (seventh outputs))
         (tile-offset (tenth outputs))
         (position-quantity
           (shader:shader-declaration-quantity-specification position)))
    (true (shader:shader-type=
           :vec3 (math:declaration-representation-type position)))
    (true (eq position-quantity
              (math:declaration-quantity-specification position)))
    (true (math:declaration-quantity-checked-p position))
    (true (eq :world-position
              (math:quantity-specification-name position-quantity)))
    (true (math:quantity-specification-affine-p position-quantity))
    (true (math:unit-expression=
           :cell (math:quantity-specification-unit position-quantity)))
    (true (shader:shader-declaration-quantity-layout uv-shade))
    (true (eq :world-direction
              (math:quantity-specification-name
               (shader:shader-declaration-quantity-specification normal))))
    (true (shader:shader-declaration-quantity-layout light))
    (true (eq :fog-amount
              (math:quantity-specification-name
               (shader:shader-declaration-quantity-specification fog))))
    (true (eq :shadow-uv
              (math:quantity-specification-name
               (shader:shader-declaration-quantity-specification shadow-uv))))
    (true (eq :shadow-depth
              (math:quantity-specification-name
               (shader:shader-declaration-quantity-specification shadow-depth))))
    (true (eq :flat (shader:shader-interface-interpolation tile-offset)))
    (true (eq :atlas-tile-offset
              (math:quantity-specification-name
               (shader:shader-declaration-quantity-specification tile-offset))))
    (fail
     (shader:parse-shader-specification
      'invalid-production-unit-mix
      '(:stage :vertex
        :inputs
        ((position :vec3 :location 0
                   :quantity :world-position :unit :cell :affine-p t)
         (direction :vec3 :location 1
                    :quantity :world-direction :unit :one))
        :outputs ((result :vec3 :location 0)))
     '((set-output result (+ position direction))))
     'shader:shader-language-error)))

(define-test explicit-unit-conversion-preserves-meaning-and-scales-values
  (let* ((specification
           (shader:parse-shader-specification
            'unit-conversion-probe
            '(:stage :fragment
              :outputs ((result :float :location 0)))
            '((let* ((opacity
                       (quantity 50.0 :quantity :opacity :unit :percent))
                     (fraction (convert-unit opacity :unit :one)))
                (set-output result fraction)))))
         (binding (binding-named 'fraction specification))
         (expression (shader:shader-binding-expression binding))
         (quantity
           (shader:shader-expression-quantity-specification expression))
         (instructions
           (spv:lower-spir-v
            (spv:shader-lowering-module
             (spv:compile-shader-specification specification))))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    (true (typep expression 'shader:shader-unit-conversion))
    (true (= 1/100 (shader:shader-unit-conversion-factor expression)))
    (true (eq :opacity (math:quantity-specification-name quantity)))
    (true (math:unitless-p (math:quantity-specification-unit quantity)))
    (true (find "F-MUL" names :test #'string=))
    (true (> (length (spv:assemble-shader-specification specification)) 5)))
  (flet ((reason-for (form)
           (handler-case
               (progn
                 (shader:parse-shader-specification
                  'invalid-unit-conversion-probe
                  '(:stage :fragment
                    :inputs ((value :float :location 0))
                    :outputs ((result :float :location 0)))
                  `((set-output result ,form)))
                 nil)
             (shader:shader-language-error (condition)
               (shader:shader-language-error-reason condition)))))
    (true (eq :unit-conversion-requires-quantity
              (reason-for '(convert-unit value :unit :metre))))
    (true (eq :invalid-quantity-declaration
              (reason-for
               '(quantity 1.0 :quantity :opacity :unit :radian))))
    (true (eq :invalid-quantity-declaration
              (reason-for
               '(quantity 1.0 :quantity :unregistered-distance
                              :unit :metre))))
    (true (eq :invalid-unit-conversion
              (reason-for
               '(convert-unit
                 (quantity 1.0 :quantity :duration :unit :second)
                 :unit :metre))))
    (true (eq :undefined-unit
              (reason-for
               '(convert-unit
                 (quantity 1.0 :quantity :distance :unit :metre)
                 :unit :furlong))))))

(define-test production-shadow-material-carries-semantic-quantities
  (let ((specification (shaders:block-world-fragment-specification)))
    (flet ((quantity (name)
             (shader:shader-expression-quantity-specification
              (shader:shader-binding-expression
               (binding-named name specification)))))
      (let ((coordinate (quantity 'shadow-coordinate))
            (receiver-depth (quantity 'receiver-depth))
            (bias (quantity 'shadow-bias))
            (world-span (quantity 'shadow-world-span))
            (gradient (quantity 'shadow-depth-gradient))
            (blocker-separation (quantity 'shadow-blocker-separation))
            (filter-radius (quantity 'shadow-filter-radius))
            (rgba (quantity 'rgba)))
        (true (eq :shadow-uv
                  (math:quantity-specification-name coordinate)))
        (true (eq :normalized-coordinate
                  (math:quantity-specification-kind coordinate)))
        (true (math:quantity-specification-affine-p coordinate))
        (true (eq :shadow-depth
                  (math:quantity-specification-name receiver-depth)))
        (true (math:quantity-specification-affine-p receiver-depth))
        (true (not (math:quantity-specification-affine-p bias)))
        (true (math:unit-expression=
               :cell (math:quantity-specification-unit world-span)))
        (true (eq :shadow-depth-gradient
                  (math:quantity-specification-name gradient)))
        (true (eq :shadow-depth
                  (math:quantity-specification-name blocker-separation)))
        (true (math:quantity-specification-absolute-p blocker-separation))
        (true (eq :shadow-filter-radius
                  (math:quantity-specification-name filter-radius)))
        (true (eq :sample-count
                  (math:quantity-specification-kind filter-radius)))
        (true (eq :linear-rgba
                  (math:quantity-specification-name rgba)))
        (true (eq :relative-color-signal
                  (math:quantity-specification-kind rgba)))))))

(define-test production-quantity-vocabulary-supplies-character-defaults
  (flet ((specification (name &optional (unit :one))
           (math:make-quantity-specification name :unit unit)))
    (dolist (name '(:world-position :texture-uv :shadow-uv :shadow-depth
                    :clip-coordinate))
      (true (math:quantity-specification-affine-p
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
        (true (math:quantity-specification-absolute-p quantity))
        (true (math:quantity-specification-non-negative-p quantity))))))

(define-test production-points-and-amounts-reject-invalid-arithmetic
  (labels ((failure-for (form)
             (handler-case
                 (progn
                   (shader:parse-shader-specification
                    'invalid-production-quantity-operation
                    '(:stage :fragment
                      :outputs ((result :float :location 0)))
                    `((set-output result ,form)))
                   nil)
               (shader:shader-language-error (condition) condition))))
    (let* ((negation
             '(- (quantity 1.0 :quantity :opacity :unit :one)))
           (point-addition
             '(+ (quantity 0.0 :quantity :world-x-position :unit :cell)
                 (quantity 1.0 :quantity :world-x-position :unit :cell)))
           (negation-error (failure-for negation))
           (point-error (failure-for point-addition)))
      (true (eq :invalid-quantity-operation
                (shader:shader-language-error-reason negation-error)))
      (true (eq :cannot-negate-amount
                (shader:shader-language-error-details negation-error)))
      (true (equal negation (shader:shader-language-error-form negation-error)))
      (true (eq :invalid-quantity-operation
                (shader:shader-language-error-reason point-error)))
      (true (eq :cannot-add-points
                (shader:shader-language-error-details point-error)))
      (true (equal point-addition
                   (shader:shader-language-error-form point-error))))))

(define-test production-crosshair-composes-linear-rgb-with-opacity
  (let* ((specification (shaders:block-world-crosshair-fragment-specification))
         (ink (first (shader:shader-specification-inputs specification)))
         (rgba (binding-named 'rgba specification))
         (ink-quantity
           (shader:shader-declaration-quantity-specification ink))
         (opaque-quantity
           (shader:shader-expression-quantity-specification
            (find-if
             (lambda (expression)
               (let ((quantity
                       (shader:shader-expression-quantity-specification
                        expression)))
                 (and quantity
                      (eq :opacity
                          (math:quantity-specification-name quantity)))))
             (shader:shader-specification-expressions specification))))
         (rgba-quantity
           (shader:shader-expression-quantity-specification
            (shader:shader-binding-expression rgba))))
    (true (eq :linear-rgb
              (math:quantity-specification-name ink-quantity)))
    (true (eq :opacity
              (math:quantity-specification-name opaque-quantity)))
    (true (eq :linear-rgba
              (math:quantity-specification-name rgba-quantity)))
    (true (math:quantity-specification-non-negative-p rgba-quantity))))

(define-test shadow-visibility-is-a-source-abstraction-over-core-math
  (true (shader:shader-abstraction-p 'shader:shadow-visibility))
  (true (not (shader:shader-operator-p 'shader:shadow-visibility)))
  (let* ((specification
           (shader:parse-shader-specification
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
                     (visible (shader:shadow-visibility
                               shadow-map shadow-sampler coordinate
                               depth gradient texel depth-bias filter-radius)))
                (set-output visibility visible)))))
         (visible (binding-named 'visible specification))
         (expression (shader:shader-binding-expression visible))
         (module (spv:shader-lowering-module
                  (spv:compile-shader-specification specification)))
         (instructions (spv:lower-spir-v module))
         (names (mapcar (lambda (instruction)
                          (symbol-name (spv:instruction-name instruction)))
                        instructions)))
    (true (typep expression 'shader:shader-call))
    (true (eq (shader:shader-call-operator expression) '/))
    (true (= 17 (count "IMAGE-SAMPLE-DREF-IMPLICIT-LOD" names :test #'string=)))
    (true (= 17 (count "DOT" names :test #'string=)))
    (true (> (length (spv:assemble-shader-specification specification)) 5))))

(define-test shader-abstraction-redefinition-affects-fresh-parses
  (labels ((install-subtraction ()
             (eval
              '(shader:define-shader-abstraction test-shadow-rewrite
                   (receiver depth bias)
                 `(shader:step (- ,receiver ,bias) ,depth))))
           (install-addition ()
             (eval
              '(shader:define-shader-abstraction test-shadow-rewrite
                   (receiver depth bias)
                 `(shader:step (+ ,receiver ,bias) ,depth))))
           (install-bad-expansion ()
             (eval
              '(shader:define-shader-abstraction test-shadow-rewrite
                   (receiver depth bias)
                 `(shader:step ,receiver ,depth ,bias))))
           (parse-probe ()
             (shader:parse-shader-specification
              'shadow-rewrite-probe
              '(:stage :fragment
                :inputs ((receiver :float :location 0)
                         (depth :float :location 1)
                         (bias :float :location 2))
                :outputs ((visibility :float :location 0)))
              '((let* ((visible (test-shadow-rewrite receiver depth bias)))
                  (set-output visibility visible)))))
           (visible-form (specification)
             (shader:shader-expression-form
              (shader:shader-binding-expression
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
                  (revision (shader:shader-source-revision)))
             (install-addition)
             (let ((added (parse-probe))
                   (method-added
                     (shader-abstraction-method-probe
                      :abstraction-probe :fragment)))
               (true (> (shader:shader-source-revision) revision))
               (true (equal (form-names subtracted-form)
                            '("step" ("-" "receiver" "bias") "depth")))
               (true (equal (form-names (visible-form added))
                            '("step" ("+" "receiver" "bias") "depth")))
               (true (equal (form-names method-subtracted-form)
                            '("step" ("-" "receiver" "bias") "depth")))
               (true (equal (form-names (visible-form method-added))
                            '("step" ("+" "receiver" "bias") "depth")))
               (install-bad-expansion)
               (fail (parse-probe) 'shader:shader-language-error)
               (true (equal (form-names subtracted-form)
                            '("step" ("-" "receiver" "bias") "depth"))))))
      (install-subtraction))))

(define-test shader-lowering-is-deterministic-and-assemblable
  (flet ((forms ()
           (mapcar #'spv:instruction-form
                   (spv:lower-spir-v
                    (shaders:block-world-fragment-module)))))
    (true (equal (forms) (forms)))
    (let ((words (shaders:block-world-fragment-shader)))
      (true (> (length words) 5))
      (true (= (aref words 0) #x07230203)))
    (let ((vertex
            (spv:assemble-shader-specification
             (shaders:block-world-crosshair-vertex-specification)))
          (fragment (shaders:block-world-crosshair-fragment-shader)))
      (true (> (length vertex) 5))
      (true (> (length fragment) 5))
      (true (= (aref vertex 0) #x07230203))
      (true (= (aref fragment 0) #x07230203)))
    (let ((vertex (shaders:block-world-vertex-shader)))
      (true (> (length vertex) 5))
      (true (= (aref vertex 0) #x07230203)))
    (let ((vertex (shaders:block-world-sky-vertex-shader))
          (fragment (shaders:block-world-sky-fragment-shader)))
      (true (> (length vertex) 5))
      (true (> (length fragment) 5))
      (true (= (aref vertex 0) #x07230203))
      (true (= (aref fragment 0) #x07230203)))
    (let ((vertex (shaders:block-world-shadow-vertex-shader)))
      (true (> (length vertex) 5))
      (true (= (aref vertex 0) #x07230203)))
    (let ((vertex
            (spv:assemble-shader-specification
             (shaders:block-world-text-vertex-specification)))
          (fragment
            (spv:assemble-shader-specification
             (shaders:block-world-text-fragment-specification))))
      (true (> (length vertex) 5))
      (true (> (length fragment) 5))
      (true (= (aref vertex 0) #x07230203))
      (true (= (aref fragment 0) #x07230203)))))

(define-test every-scene-stage-declares-the-same-frame-uniform-block
  ;; Identical member order and offsets at binding 2 are the ABI contract
  ;; which lets one buffer feed the vertex and fragment halves of both the
  ;; block material and the sky.
  (flet ((frame-block (specification)
           (find-if (lambda (resource)
                      (typep resource 'shader:shader-uniform-block))
                    (shader:shader-specification-resources specification)))
         (member-layout (block)
           (mapcar (lambda (member)
                     (list (string-downcase
                            (symbol-name (shader:shader-object-name member)))
                           (shader:shader-uniform-member-offset member)))
                   (shader:shader-uniform-block-members block))))
    (let* ((specifications
             (list (shaders:block-world-vertex-specification)
                   (shaders:block-world-fragment-specification)
                   (shaders:block-world-sky-vertex-specification)
                   (shaders:block-world-sky-fragment-specification)
                   (shaders:block-world-shadow-vertex-specification)
                   (shaders:block-world-text-vertex-specification)))
           (blocks (mapcar #'frame-block specifications))
           (reference (member-layout (first blocks))))
      (true (every (lambda (block) (typep block 'shader:shader-uniform-block))
                   blocks))
      (true (every (lambda (block)
                     (= (shader:shader-resource-binding block) 2))
                   blocks))
      (true (every (lambda (block)
                     (equal (member-layout block) reference))
                   (rest blocks)))
      (true (every (lambda (block)
                     (= (shader:shader-uniform-block-byte-size block) 304))
                   blocks)))))

(define-test the-sky-material-is-image-mathematics-over-environment-lanes
  (let* ((vertex (shaders:block-world-sky-vertex-specification))
         (fragment (shaders:block-world-sky-fragment-specification))
         (fragment-module (shaders:block-world-sky-fragment-module))
         (ray (binding-named 'ray vertex))
         (direction (binding-named 'direction fragment))
         (cloud-density (binding-named 'cloud-density fragment))
         (disc (binding-named 'disc fragment)))
    (true (eq (shader:shader-specification-stage vertex) :vertex))
    (true (eq (shader:shader-specification-stage fragment) :fragment))
    (true (shader:shader-type=
           (shader:shader-expression-type (shader:shader-binding-expression ray))
           :vec3))
    ;; The sky drops out of the checked-quantity world once, deliberately,
    ;; and is ordinary image mathematics over a unit view ray thereafter.
    (true (equal (form-names
                  (shader:shader-expression-form
                   (shader:shader-binding-expression direction)))
                 '("normalize" ("representation" "ray-input"))))
    ;; A deck is a coverage threshold over one noise field, and the width of
    ;; that threshold is the only thing that softens toward the horizon.
    (true (equal (form-names
                  (shader:shader-expression-form
                   (shader:shader-binding-expression cloud-density)))
                 '("*" "deck-mask"
                   ("smoothstep" "coverage"
                    ("+" "coverage" "softness") "cloud-field"))))
    ;; The deck's own shadow is the same field sampled along the deck toward
    ;; the sun, against the same threshold: a lit face and a dark underside
    ;; from one extra tap.
    (true (equal (form-names
                  (shader:shader-expression-form
                   (shader:shader-binding-expression
                    (binding-named 'shadow-density fragment))))
                 '("smoothstep" "coverage" ("+" "coverage" "softness")
                   "shadow-field")))
    (true (equal (form-names
                  (shader:shader-expression-form
                   (shader:shader-binding-expression disc)))
                 '("smoothstep" ("-" 1.0 "disc-limb")
                   ("-" 1.0 ("*" 0.56 "disc-limb")) "alignment")))
    ;; All the fragment's extended mathematics shares one import.
    (true (= 1 (length (spv:spir-v-module-extended-instruction-imports
                        fragment-module))))))

(define-test the-sky-and-the-block-surface-agree-on-what-distance-looks-like
  ;; Below the horizon the sky stands in for terrain too far off to be
  ;; resident, so the two stages have to arrive at the same colour or the
  ;; edge of the resident world draws itself as a line.  They agree by
  ;; calling the same function on the same lanes rather than by two
  ;; expressions kept in step by hand.
  (let* ((sky (shaders:block-world-sky-fragment-specification))
         (surface (shaders:block-world-fragment-specification))
         (aerial (binding-named 'aerial sky))
         (fog-color (binding-named 'fog-color surface)))
    (flet ((operator (expression)
             (first (form-names (shader:shader-expression-form expression)))))
      (true (string= "aerial-perspective-color"
                     (operator (shader:shader-binding-expression aerial))))
      (true (string= "assume-quantity"
                     (operator (shader:shader-binding-expression fog-color))))
      (true (equal (form-names
                    (shader:shader-expression-form
                     (shader:shader-binding-expression fog-color)))
                   '("assume-quantity"
                     ("aerial-perspective-color"
                      ("representation" ("swizzle" "fog-color-vector" "xyz"))
                      "look-direction" ("representation" "sun-direction")
                      "low-sun" ("representation" "day-factor"))
                     "quantity" "linear-rgb" "unit" "one")))
      ;; And the fog it feeds is still an absolute colour, mixed by the same
      ;; amount the vertex stage measured.
      (true (equal (form-names
                    (shader:shader-expression-form
                     (shader:shader-binding-expression
                      (binding-named 'fogged surface))))
                   '("mix" "radiance" "fog-color" "fog-amount")))
      (true (eq :absolute
                (math:quantity-specification-character
                 (shader:shader-expression-quantity-specification
                  (shader:shader-binding-expression fog-color))))))))

(define-test extended-math-lowers-through-one-shared-import-in-layout-order
  (let* ((specification
           (shader:parse-shader-specification
            'extended-math
            '(:stage :fragment
              :inputs ((direction :vec3 :location 0)
                       (level :float :location 1))
              :outputs ((color :vec4 :location 0)))
            '((let* ((unit (normalize direction))
                     (glow (smoothstep 0.9 1.0 level))
                     (lit (max 0.0 (dot unit (shader:vec3 0.0 1.0 0.0))))
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
    (true (= 1 (length
                (spv:spir-v-module-extended-instruction-imports module))))
    (true (= 1 (count "EXT-INST-IMPORT" names :test #'string=)))
    (true (< (position "CAPABILITY" names :test #'string=)
             (position "EXT-INST-IMPORT" names :test #'string=)
             (position "MEMORY-MODEL" names :test #'string=)))
    (true (= 8 (count "EXT-INST" names :test #'string=)))
    (true (> (length (spv:assemble-shader-specification specification)) 5))
    ;; Deterministic lowering, and no import where no extended math occurs.
    (flet ((forms ()
             (mapcar #'spv:instruction-form
                     (spv:lower-spir-v
                      (spv:shader-lowering-module
                       (spv:compile-shader-specification specification))))))
      (true (equal (forms) (forms))))
    (true (null (spv:spir-v-module-extended-instruction-imports
                 (shaders:block-world-crosshair-fragment-module))))))

(define-test slug-root-eligibility-is-the-eight-class-table
  (let ((expected '((0 0) (1 0) (1 1) (1 0)
                    (0 1) (1 1) (0 1) (0 0))))
    (loop for code below 8
          for pair in expected
          for y1 = (if (logbitp 0 code) 1.0 -1.0)
          for y2 = (if (logbitp 1 code) 1.0 -1.0)
          for y3 = (if (logbitp 2 code) 1.0 -1.0)
          do (true (equal pair
                          (multiple-value-list
                           (slug:slug-root-eligibility y1 y2 y3))))))
  ;; Zero is deliberately in the non-positive class.
  (true (equal '(0 0)
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

(define-test arbitrary-quadratic-contours-pack-into-sorted-slug-bands
  (let* ((outer (slug-test-square 0 0 4 4))
         (inner (slug-test-square 1 1 3 3 :clockwise-p t))
         (outline (slug:make-slug-outline :contours (list outer inner)))
         (packed
           (slug:pack-slug-outline
            outline :horizontal-band-count 2 :vertical-band-count 2))
         (curves (slug:slug-packed-outline-curves packed)))
    (true (eq :counterclockwise (slug:slug-contour-orientation outer)))
    (true (eq :clockwise (slug:slug-contour-orientation inner)))
    (true (= 8 (length curves)))
    (true (= 0 (slug:slug-packed-outline-min-x packed)))
    (true (= 4 (slug:slug-packed-outline-max-y packed)))
    (true (every
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
    (true (every
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
      (true (equalp (slug:slug-quadratic-control line)
                    (slug:slug-quadratic-end line))))))

(define-test malformed-slug-contours-report-the-broken-junction
  (let* ((first (slug-test-line 0 0 1 0))
         (second (slug-test-line 2 0 0 0))
         (outline (slug:make-slug-outline
                   :contours (list (list first second)))))
    (handler-case
        (progn
          (slug:slug-outline-curves outline)
          (true nil))
      (slug:slug-outline-error (condition)
        (true (eq :disconnected-contour
                  (slug:slug-outline-error-reason condition)))
        (true (equal '(0 0)
                     (slug:slug-outline-error-details condition)))))))

(define-test slug-texture-serialization-repacks-the-actual-half-values
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
    (true (= 4096 (slug:slug-serialized-outline-curve-width serialized)))
    (true (= 4096 (slug:slug-serialized-outline-band-width serialized)))
    (true (= 5 (slug:slug-serialized-outline-curve-texel-count serialized)))
    (true (= 20 (length curve-words)))
    ;; The first curve's p3 is the following curve texel's p1.
    (true (equalp (subseq curve-words 2 4) (subseq curve-words 4 6)))
    ;; Four headers, then the two vertical sides once (both horizontal bands
    ;; hold exactly them, so the second points at the first's list) and the
    ;; two horizontal sides once, likewise shared.
    (true (= 8 (slug:slug-serialized-outline-band-texel-count serialized)))
    (true (equal '(4096 1)
                 (slug:slug-serialized-outline-curve-texture-size serialized)))
    (true (equal '(4096 1)
                 (slug:slug-serialized-outline-band-texture-size serialized)))
    (true (nth-value 0
            (subtypep (array-element-type curve-upload) '(unsigned-byte 64))))
    (true (nth-value 0
            (subtypep (array-element-type band-upload) '(unsigned-byte 32))))
    (true (= (row-major-aref curve-upload 0)
             (loop for index below 4
                   sum (ash (aref curve-words index) (* index 16)))))
    (true (= #x00040002 (row-major-aref band-upload 0)))
    (true (equalp #(2 4 2 4 2 6 2 6) (subseq band-words 0 8)))
    (true (equalp #(1 0 3 0) (subseq band-words 8 12)))
    (true (equalp #(2 0 0 0) (subseq band-words 12 16)))
    (let ((slug:*slug-share-band-lists* nil))
      (true (= 12 (slug:slug-serialized-outline-band-texel-count
                   (slug:serialize-slug-outline
                    (slug:make-slug-outline :contours (list contour))
                    :horizontal-band-count 2 :vertical-band-count 2)))))
    (true (not (= 1/3 (slug:slug-packed-outline-max-x packed))))
    (true (< (abs (- 1/3 (slug:slug-packed-outline-max-x packed)))
             1/1000))))

(define-test slug-serialization-chooses-band-counts-by-load
  ;; A square's two vertical sides fall in every horizontal band however
  ;; many there are, so one band per axis is as good as any and cheapest.
  (let ((serialized
          (slug:serialize-slug-outline
           (slug:make-slug-outline
            :contours (list (slug-test-square 0 0 1 1))))))
    (true (= 1 (slug:slug-serialized-outline-horizontal-band-count serialized)))
    (true (= 1 (slug:slug-serialized-outline-vertical-band-count serialized)))
    (true (= 6 (slug:slug-serialized-outline-band-texel-count serialized))))
  ;; Two squares stacked with a gap: two horizontal bands each hold one
  ;; square's sides (a load of two) where one band would hold four.
  (let* ((lower (slug-test-square 0 0 1 1))
         (upper (slug-test-square 0 2 1 3))
         (outline (slug:make-slug-outline :contours (list lower upper)))
         (packed (slug:pack-slug-outline outline))
         (curves (slug:slug-packed-outline-curves packed)))
    (true (= 2 (slug:choose-slug-band-count curves :y 0 3)))
    (true (= 2 (length (slug:slug-packed-outline-horizontal-bands packed))))
    (true (every (lambda (band)
                   (= 2 (length (slug:slug-band-curve-indices band))))
                 (slug:slug-packed-outline-horizontal-bands packed)))
    (let ((slug:*slug-maximum-band-count* 1))
      (true (= 1 (slug:choose-slug-band-count curves :y 0 3))))))

(define-test zpb-ttf-glyphs-enter-slug-before-software-rasterization
  (zpb-ttf:with-font-loader
      (font-loader (cl-dejavu:font-pathname "DejaVuSans.ttf"))
    (let* ((glyph (slug:load-slug-glyph #\O font-loader))
           (outline (slug:slug-glyph-outline glyph))
           (contours (slug:slug-outline-contours outline))
           (packed (slug:pack-slug-outline outline))
           (orientations
             (mapcar #'slug:slug-contour-orientation contours)))
      (true (char= #\O (slug:slug-glyph-character glyph)))
      (true (= 2048 (slug:slug-glyph-units-per-em glyph)))
      (true (= 1612 (slug:slug-glyph-advance-width glyph)))
      (true (= 2 (length contours)))
      (true (= 16 (length (slug:slug-packed-outline-curves packed))))
      (true (member :clockwise orientations))
      (true (member :counterclockwise orientations))
      (let* ((normalized (slug:normalize-slug-glyph-outline glyph))
             (normalized-packed (slug:pack-slug-outline normalized)))
        (true (= (slug:slug-packed-outline-min-x normalized-packed)
                 (/ (slug:slug-packed-outline-min-x packed) 2048)))
        (true (= (slug:slug-packed-outline-max-x normalized-packed)
                 (/ (slug:slug-packed-outline-max-x packed) 2048)))
        (true (= (slug:slug-packed-outline-min-y normalized-packed)
                 (/ (slug:slug-packed-outline-min-y packed) 2048)))
        (true (= (slug:slug-packed-outline-max-y normalized-packed)
                 (/ (slug:slug-packed-outline-max-y packed) 2048))))
      (true (every (lambda (contour)
                     (every (lambda (curve)
                              (not (equalp
                                    (slug:slug-quadratic-control curve)
                                    (slug:slug-quadratic-end curve))))
                            contour))
                   contours)))))

(define-test shaders-consume-shared-arithmetic-functions-directly
  (let* ((before (shader:shader-source-revision))
         (specification
           (shader:parse-shader-specification
            'shared-function-fragment
            '(:stage :fragment
              :inputs ((value :float :location 0))
              :outputs ((result :float :location 0)))
            '((set-output result
                          (shared-shader-function-probe value)))))
         (call
           (shader:shader-assignment-value
            (first (shader:shader-specification-statements specification)))))
    (true (typep call 'shader:shader-function-call))
    (true (typep call 'lang:arithmetic-function-call))
    (true (typep (shader:shader-function-call-definition call)
                 'lang:arithmetic-function-definition))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))
    (lang:note-arithmetic-function-redefinition
     'shared-shader-function-probe)
    (true (> (shader:shader-source-revision) before))))

(define-test shared-source-retires-an-older-shader-only-definition
  (eval '(shader:define-shader-function shared-source-migration-probe (value)
           (+ value 1.0)))
  (true (shader:shader-function-definition-for
         'shared-source-migration-probe))
  (eval '(lang:define-arithmetic-function
             shared-source-migration-probe ((value))
           (+ value 2.0)))
  (true (null (shader:shader-function-definition-for
               'shared-source-migration-probe)))
  (true (lang:arithmetic-function-definition-for
         'shared-source-migration-probe)))

(define-test task-and-mesh-lower-to-validated-vulkan-shaped-spir-v
  (let* ((task-specification (vulkan-task-probe))
         (mesh-specification (vulkan-mesh-probe))
         (task-lowering
           (shader:lower-shader-specification :spir-v task-specification))
         (mesh-lowering
           (shader:lower-shader-specification :spir-v mesh-specification))
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
                      (typep expression 'shader:shader-payload-element))
                    (shader:shader-specification-expressions
                     mesh-specification))))
    (dolist (module (list task-module mesh-module))
      (true (= #x00010400 (spv:spir-v-module-version module)))
      (true (member 'spv::mesh-shading-ext
                    (spv:spir-v-module-capabilities module)))
      (true (member 'spv::int64
                    (spv:spir-v-module-capabilities module)))
      (true (equal '("SPV_EXT_mesh_shader")
                   (spv:spir-v-module-extensions module))))
    (true (eq 'spv::task-ext
              (spv:spir-v-entry-point-execution-model
               (first (spv:spir-v-module-entry-points task-module)))))
    (true (eq 'spv::mesh-ext
              (spv:spir-v-entry-point-execution-model
               (first (spv:spir-v-module-entry-points mesh-module)))))
    (true (equal '(spv::local-size)
                 (mapcar #'spv:spir-v-execution-mode-name
                         (spv:spir-v-module-execution-modes task-module))))
    (true (equal '(spv::local-size spv::output-triangles-ext
                   spv::output-vertices spv::output-primitives-ext)
                 (mapcar #'spv:spir-v-execution-mode-name
                         (spv:spir-v-module-execution-modes mesh-module))))
    (dolist (name '(spv::selection-merge spv::emit-mesh-tasks-ext))
      (true (find name task-names)))
    (dolist (name '(spv::set-mesh-outputs-ext spv::selection-merge
                    spv::access-chain spv::store spv::u-convert))
      (true (find name mesh-names)))
    (false (find 'spv::return task-names))
    (true (find 'spv::return mesh-names))
    (true (search "TASK-PAYLOAD-WORKGROUP-EXT" task-forms))
    (true (search "TASK-PAYLOAD-WORKGROUP-EXT" mesh-forms))
    (true (search "PER-PRIMITIVE-EXT" mesh-forms))
    (true (search "PRIMITIVE-TRIANGLE-INDICES-EXT" mesh-forms))
    (true payload-expression)
    (true (gethash payload-expression
                   (spv:shader-lowering-expression-instructions
                    mesh-lowering)))
    (dolist (specification (list task-specification mesh-specification))
      (let ((words (spv:assemble-shader-specification specification)))
        (true (= #x07230203 (aref words 0)))
        (true (= #x00010400 (aref words 1)))))))

(define-test shared-counted-fold-lowers-to-structured-spir-v
  (let* ((specification
           (shader:parse-shader-specification
            'fold-fragment
            '(:stage :fragment
              :inputs ((count :float :location 0))
              :outputs ((result :float :location 0)))
            '((set-output result (shared-fold-probe count)))))
         (names
           (mapcar #'spv:instruction-name
                   (spv:lower-spir-v
                    (spv:shader-module specification)))))
    (true (find "PHI" names :key #'symbol-name :test #'string=))
    (true (find "LOOP-MERGE" names :key #'symbol-name :test #'string=))
    (true (find "BRANCH-CONDITIONAL" names
                :key #'symbol-name :test #'string=))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))))

(define-test counted-fold-until-guards-the-spir-v-loop-header
  (let* ((specification
           (shader:parse-shader-specification
            'until-fold-fragment
            '(:stage :fragment
              :inputs ((count :float :location 0))
              :outputs ((result :float :location 0)))
            '((set-output result
                          (counted-fold
                              (index count sum 0.0 :until (> sum 10.0))
                            (+ sum index))))))
         (names
           (mapcar #'spv:instruction-name
                   (spv:lower-spir-v
                    (spv:shader-module specification)))))
    (true (find "LOGICAL-NOT" names :key #'symbol-name :test #'string=))
    (true (find "LOGICAL-AND" names :key #'symbol-name :test #'string=))
    (true (find "LOOP-MERGE" names :key #'symbol-name :test #'string=))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))
    ;; The test must be a boolean.
    (fail
     (shader:parse-shader-specification
      'bad-until-fold-fragment
      '(:stage :fragment
        :inputs ((count :float :location 0))
        :outputs ((result :float :location 0)))
      '((set-output result
                    (counted-fold (index count sum 0.0 :until sum)
                      (+ sum index)))))
     'shader:shader-language-error)))

(define-test exact-unsigned-texel-loads-flow-through-shared-folds
  (let* ((specification (unsigned-texel-fold-probe))
         (module
           (spv:shader-lowering-module
            (shader:lower-shader-specification :spir-v specification)))
         (names
           (loop for function in (spv:spir-v-module-function-definitions module)
                 append
                 (loop for block in (spv:spir-v-function-basic-blocks function)
                       append (mapcar #'spv:instruction-name
                                      (spv:spir-v-basic-block-instructions
                                       block))))))
    (true (shader:shader-type=
           :uvec4
           (shader:shader-expression-type
            (shader:shader-binding-expression
             (binding-named 'header specification)))))
    (dolist (name '(image-fetch u-mod u-div i-add u-less-than convert-u-to-f))
      (true (find name names :test #'string-equal)))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))))

(define-test slug-atlas-uses-fragment-derivatives-and-selected-bands
  (let* ((specification (slug:slug-atlas-fragment-specification))
         (module
           (spv:shader-lowering-module
            (shader:lower-shader-specification :spir-v specification)))
         (names
           (loop for function in (spv:spir-v-module-function-definitions module)
                 append
                 (loop for block in (spv:spir-v-function-basic-blocks function)
                       append (mapcar #'spv:instruction-name
                                      (spv:spir-v-basic-block-instructions
                                       block))))))
    (dolist (name '(d-pdx d-pdy select image-fetch))
      (true (find name names :test #'string-equal)))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))))

(define-test shared-conditionals-lower-inside-structured-spir-v-folds
  (let* ((specification
           (shader:parse-shader-specification
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
    (true (find "F-ORD-LESS-THAN" names
                :key #'symbol-name :test #'string=))
    (true (find "SELECT" names :key #'symbol-name :test #'string=))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))))

(define-test analytic-roundrect-distance-covers-the-fixed-shape-family
  (flet ((near (left right)
           (< (abs (- left right)) 1.0e-5)))
    ;; A two-by-one roundrect is one unit inside at its centre and exactly on
    ;; its straight right edge.
    (true (near -1.0
                (analytic:roundrect-signed-distance 0.0 0.0 2.0 1.0 0.25)))
    (true (near 0.0
                (analytic:roundrect-signed-distance 2.0 0.0 2.0 1.0 0.25)))
    ;; Radius equal to both half-extents is the ordinary circle distance.
    (true (near 0.0
                (analytic:roundrect-signed-distance 0.6 0.8 1.0 1.0 1.0)))
    (true (near 1.0
                (analytic:roundrect-signed-distance 2.0 0.0 1.0 1.0 1.0)))
    ;; Excessive and negative radii are normalized at the semantic boundary.
    (true (near 0.0
                (analytic:roundrect-signed-distance 1.0 0.0 1.0 0.5 8.0)))
    (true (near 0.0
                (analytic:roundrect-signed-distance 1.0 0.0 1.0 0.5 -1.0)))))

(define-test analytic-roundrect-proof-shares-distance-and-derivative-coverage
  (let* ((vertex (analytic:roundrect-vertex-specification))
         (fragment (analytic:roundrect-fragment-specification))
         (coverage (binding-named 'coverage fragment))
         (forms
           (write-to-string
            (mapcar #'spv:instruction-form
                    (spv:lower-spir-v
                     (spv:shader-module fragment))))))
    (true (eq :vertex (shader:shader-specification-stage vertex)))
    (true (eq :fragment (shader:shader-specification-stage fragment)))
    (true (= 4 (length (shader:shader-specification-inputs vertex))))
    (true (= 3 (length (shader:shader-specification-inputs fragment))))
    (true (typep (shader:shader-binding-expression coverage)
                 'shader:shader-function-call))
    (true (eq 'analytic:roundrect-coverage
              (shader:shader-object-name
               (shader:shader-function-call-definition
                (shader:shader-binding-expression coverage)))))
    (true (lang:arithmetic-function-definition-for
           'analytic:roundrect-signed-distance))
    (true (search "D-PDX" forms))
    (true (search "D-PDY" forms))
    (true (search "SQRT" forms))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification vertex) 0)))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification fragment) 0)))))

(define-test slug-proof-is-a-pixel-shader-over-quadratic-roots
  (let* ((vertex (slug:slug-bezier-vertex-specification))
         (fragment (slug:slug-bezier-fragment-specification))
         (fragment-value
           (shader:shader-assignment-value
            (first (shader:shader-specification-statements fragment))))
         (lowering (spv:compile-shader-specification fragment))
         (forms
           (mapcar #'spv:instruction-form
                   (spv:lower-spir-v
                    (spv:shader-lowering-module lowering))))
         (printed (write-to-string forms)))
    (true (eq :vertex (shader:shader-specification-stage vertex)))
    (true (eq :fragment (shader:shader-specification-stage fragment)))
    (true (= 3 (length (shader:shader-specification-inputs vertex))))
    (true (= 2 (length (shader:shader-specification-inputs fragment))))
    (true (typep fragment-value 'shader:shader-function-call))
    (true (eq 'slug:slug-quadratic-outline
              (shader:shader-object-name
               (shader:shader-function-call-definition fragment-value))))
    (true (not (shader:shader-abstraction-p 'slug:slug-quadratic-outline)))
    (true (shader:shader-function-definition-for
           'slug:slug-quadratic-outline))
    (true (some (lambda (expression)
                  (typep expression 'shader:shader-function-call))
                (shader:shader-specification-expressions fragment)))
    (true (search "F-SIGN" printed))
    (true (search "SQRT" printed))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification vertex) 0)))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification fragment) 0)))))

(define-test slug-bands-are-two-data-driven-structured-traversals
  (let* ((specification (slug:slug-banded-fragment-specification))
         (instructions
           (spv:lower-spir-v (spv:shader-module specification)))
         (names (mapcar #'spv:instruction-name instructions)))
    (true (= 2 (count "LOOP-MERGE" names
                      :key #'symbol-name :test #'string=)))
    (true (>= (count "IMAGE-FETCH" names
                     :key #'symbol-name :test #'string=)
              6))
    (true (find "U-MOD" names :key #'symbol-name :test #'string=))
    (true (find "U-DIV" names :key #'symbol-name :test #'string=))
    (true (= #x07230203
             (aref (spv:assemble-shader-specification specification) 0)))))

(define-test harfbuzz-shaping-selects-ligatures-and-preserves-clusters
  (let* ((font (cl-dejavu:font-pathname "DejaVuSans.ttf"))
         (shaped (slug:shape-slug-text "office" font))
         (glyphs (slug:slug-shaped-text-glyphs shaped)))
    (true (= 2048 (slug:slug-shaped-text-units-per-em shaped)))
    ;; o, ffi, c, e: the three source characters at byte cluster 1 become one
    ;; glyph selected by HarfBuzz rather than three cmap lookups.
    (true (= 4 (length glyphs)))
    (true (equal '(0 1 4 5)
                 (loop for glyph across glyphs
                       collect (slug:slug-shaped-glyph-cluster glyph))))
    (true (= (slug:slug-shaped-text-x-advance shaped)
             (loop for glyph across glyphs
                   sum (slug:slug-shaped-glyph-x-advance glyph))))))

(define-test extended-math-signatures-are-explicit-contracts
  (flet ((failure-reason (body)
           (handler-case
               (progn
                 (shader:parse-shader-specification
                  'bad-extended-math
                  '(:stage :fragment
                    :inputs ((value :vec3 :location 0)
                             (scale :float :location 1)
                             (word :uint :location 2)
                             (uvalue :uvec2 :location 3))
                    :outputs ((color :vec3 :location 0)))
                  body)
                 nil)
             (shader:shader-language-error (condition)
               (shader:shader-language-error-reason condition)))))
    ;; Vector values with scalar bounds are not silently splatted.
    (true (eq (failure-reason '((set-output color (clamp value 0.0 1.0))))
              :incompatible-arithmetic-types))
    (true (eq (failure-reason '((let* ((unit (normalize scale)))
                                  (set-output color (* value unit)))))
              :invalid-normalize))
    (true (eq (failure-reason '((set-output color (min value))))
              :wrong-operand-count))
    (true (eq (failure-reason '((set-output color (expt value))))
              :wrong-operand-count))
    ;; Unsigned traversal values do not leak into float-only operations.
    (true (eq (failure-reason '((set-output color (* uvalue scale))))
              :incompatible-product-types))
    (true (eq (failure-reason '((set-output color (dot uvalue uvalue))))
              :invalid-dot-product))
    (true (eq (failure-reason '((set-output color (mix word word scale))))
              :invalid-mix))
    (true (eq (failure-reason '((set-output color (normalize uvalue))))
              :invalid-normalize))
    (true (eq (failure-reason '((set-output color (min word word))))
              :invalid-extended-math-type))))

(define-test extended-operations-retain-expression-provenance
  (let* ((specification
           (shader:parse-shader-specification
            'clamped-level
            '(:stage :fragment
              :inputs ((level :float :location 0))
              :outputs ((color :float :location 0)))
            '((let* ((held (clamp level 0.0 1.0)))
                (set-output color held)))))
         (lowering (spv:compile-shader-specification specification))
         (call (shader:shader-binding-expression
                (binding-named 'held specification)))
         (instructions
           (gethash call
                    (spv:shader-lowering-expression-instructions lowering))))
    (true instructions)
    (true (find "EXT-INST" instructions
                :key (lambda (instruction)
                       (symbol-name (spv:instruction-name instruction)))
                :test #'string=))
    (true (some (lambda (instruction)
                  (member call
                          (gethash instruction
                                   (spv:shader-lowering-instruction-expressions
                                    lowering))
                          :test #'eq))
                instructions))))

(define-test shader-diagnostics-name-the-source-failure
  (let ((unknown-reason
          (handler-case
              (progn
                (shader:parse-shader-specification
                 'bad-shader
                 '(:stage :fragment
                   :outputs ((color :vec4 :location 0)))
                 '((set-output color missing-name)))
                nil)
            (shader:shader-language-error (condition)
              (shader:shader-language-error-reason condition))))
        (type-reason
          (handler-case
              (progn
                (shader:parse-shader-specification
                 'bad-shader
                 '(:stage :fragment
                   :inputs ((scalar :float :location 0))
                   :outputs ((color :vec4 :location 0)))
                 '((set-output color scalar)))
                nil)
            (shader:shader-language-error (condition)
              (shader:shader-language-error-reason condition)))))
    (true (eq unknown-reason :unknown-name))
    (true (eq type-reason :output-type-mismatch))))

;;; Storage buffers and bit fields: the path a packed 64-bit site takes from a
;;; host array into a shader.

(shader:define-shader storage-site-fragment-probe
    (:stage :fragment
     :resources ((sites :storage-buffer :binding 1 :element :uint64)
                 (words :storage-buffer :binding 2 :element :uvec4))
     :outputs ((color :vec4 :location 0)))
  (let* ((term (shader:buffer-element sites (uint 3.0)))
         (extent (uint (ldb (byte 4 0) term)))
         (x (uint (ldb (byte 24 4) term)))
         (z (uint (ldb (byte 8 52) term)))
         (word (swizzle (shader:buffer-element words extent) :x))
         (high (ldb (byte 16 16) word))
         (whole (ldb (byte 32 0) word))
         (shade (/ (float (+ x z high whole)) 255.0)))
    (set-output color (vec4 shade shade shade 1.0))))

(defun storage-probe-error-reason (resources body)
  (handler-case
      (progn
        (shader:parse-shader-specification
         'storage-probe
         `(:stage :fragment
           :resources ,resources
           :outputs ((color :vec4 :location 0)))
         body)
        nil)
    (shader:shader-language-error (condition)
      (shader:shader-language-error-reason condition))))

(define-test storage-buffers-declare-typed-elements-and-index-them
  (let* ((specification (storage-site-fragment-probe))
         (sites (find 'sites (shader:shader-specification-resources specification)
                      :key #'shader:shader-object-name :test #'string-equal))
         (words (find 'words (shader:shader-specification-resources specification)
                      :key #'shader:shader-object-name :test #'string-equal))
         (term (binding-named 'term specification)))
    (true (typep sites 'shader:shader-storage-buffer))
    (true (eq (shader:find-shader-type :uint64)
              (shader:shader-storage-buffer-element-type sites)))
    (true (= 8 (shader:shader-storage-buffer-element-stride sites)))
    (true (= 16 (shader:shader-storage-buffer-element-stride words)))
    (true (typep (shader:shader-binding-expression term) 'shader:shader-buffer-element))
    (true (eq (shader:find-shader-type :uint64)
              (shader:shader-expression-type (shader:shader-binding-expression term))))
    (true (typep (shader:shader-binding-expression (binding-named 'extent specification))
                 'shader:shader-call))))

(define-test storage-buffers-and-bit-fields-reject-ill-typed-source
  (true (eq :invalid-storage-buffer-element
            (storage-probe-error-reason
             '((sites :storage-buffer :binding 1))
             '((set-output color (vec4 1.0 1.0 1.0 1.0))))))
  (true (eq :invalid-storage-buffer-element
            (storage-probe-error-reason
             '((sites :storage-buffer :binding 1 :element :uvec3))
             '((set-output color (vec4 1.0 1.0 1.0 1.0))))))
  (true (eq :element-on-non-storage-buffer
            (storage-probe-error-reason
             '((sites :texture-2d :binding 1 :element :uint))
             '((set-output color (vec4 1.0 1.0 1.0 1.0))))))
  (true (eq :storage-buffer-requires-element
            (storage-probe-error-reason
             '((sites :storage-buffer :binding 1 :element :vec4))
             '((set-output color sites)))))
  (true (eq :buffer-index-type
            (storage-probe-error-reason
             '((sites :storage-buffer :binding 1 :element :vec4))
             '((set-output color (shader:buffer-element sites 1.0))))))
  (true (eq :byte-specifier-exceeds-width
            (storage-probe-error-reason
             '((sites :storage-buffer :binding 1 :element :uint))
             '((let* ((word (shader:buffer-element sites (uint 0.0)))
                      (field (float (ldb (byte 8 28) word))))
                 (set-output color (vec4 field field field 1.0)))))))
  (true (eq :invalid-byte-specifier
            (storage-probe-error-reason
             '((sites :storage-buffer :binding 1 :element :uint))
             '((let* ((word (shader:buffer-element sites (uint 0.0)))
                      (field (float (ldb (byte 0 4) word))))
                 (set-output color (vec4 field field field 1.0)))))))
  (true (eq :invalid-bit-field-operand
            (storage-probe-error-reason
             '((sites :storage-buffer :binding 1 :element :float))
             '((let* ((word (shader:buffer-element sites (uint 0.0)))
                      (field (ldb (byte 8 4) word)))
                 (set-output color (vec4 field field field 1.0))))))))

(define-test storage-buffers-lower-to-storage-class-runtime-arrays
  (let* ((specification (storage-site-fragment-probe))
         (module (spv:shader-module specification))
         (instructions (spv:lower-spir-v module))
         (names (mapcar #'spv:instruction-name instructions))
         (forms (write-to-string (mapcar #'spv:instruction-form instructions)))
         (words (spv:assemble-shader-specification specification)))
    (true (equal '("SPV_KHR_storage_buffer_storage_class")
                 (spv:spir-v-module-extensions module)))
    (true (member 'spv::int64 (spv:spir-v-module-capabilities module)))
    (dolist (name '(spv::type-runtime-array spv::access-chain spv::load
                    spv::shift-left-logical spv::shift-right-logical))
      (true (find name names)))
    ;; Bit fields never lower to bit-field instructions, which Vulkan limits
    ;; to 32-bit operands, nor to 64-bit mask constants.
    (false (find 'spv::bitwise-and names))
    (true (search "STORAGE-BUFFER" forms))
    (true (search "ARRAY-STRIDE 8" forms))
    (true (search "ARRAY-STRIDE 16" forms))
    (true (search "NON-WRITABLE" forms))
    (true (= #x07230203 (aref words 0)))))
