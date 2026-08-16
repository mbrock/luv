(defpackage #:luv/msl/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:msl #:luv.msl)
                    (#:spv #:luv.spir-v)
                    (#:slug #:luv.slug)))

(in-package #:luv/msl/tests)

(defun binding-named (name specification)
  (find name (spv:shader-specification-bindings specification)
        :key #'spv:shader-object-name
        :test (lambda (left right)
                (string-equal (symbol-name left) (symbol-name right)))))

(defun msl-named (name objects name-function)
  (find name objects :key name-function :test #'string=))

(deftest block-fragment-lowers-directly-to-structured-msl
  (let* ((specification (spv:block-world-fragment-specification))
         (document (msl:compile-msl specification))
         (source (msl:msl-document-source document)))
    (ok (typep document 'msl:msl-document))
    (ok (eq specification (msl:msl-document-specification document)))
    (ok (= (length (msl:msl-document-declarations document)) 3))
    (ok (typep (msl:msl-document-entry-point document)
               'msl:msl-entry-point))
    (ok (search "fragment BlockWorldFragmentSpecificationOutput" source))
    (ok (search "BlockWorldFragmentSpecificationInput stage_in [[stage_in]]"
                source))
    (ok (search "texture2d<float> block_atlas [[texture(0)]]" source))
    (ok (search "sampler block_sampler [[sampler(1)]]" source))
    (ok (search "constant FrameState& frame_state [[buffer(2)]]" source))
    (ok (search "depth2d<float> shadow_map [[texture(3)]]" source))
    (ok (search "shadow_map.sample_compare" source))
    (ok (search "result.color_output = rgba;" source))))

(deftest block-vertex-lowers-projective-map-to-msl
  (let* ((specification (spv:block-world-vertex-specification))
         (document (msl:compile-msl specification))
         (source (msl:msl-document-source document))
         (binding (binding-named 'shadow-projection specification))
         (expression (spv:shader-binding-expression binding)))
    (ok (search "vertex BlockWorldVertexSpecificationOutput" source))
    (ok (search "float3 shadow_projection =" source))
    (ok (search "dot(frame_state.shadow_row_x, float4(stage_in.world_position, 1.0f))"
                source))
    (ok (search "float3(0.5f, 0.5f, 1.0f)" source))
    (ok (search "float3(0.5f, 0.5f, 0.0f)" source))
    ;; The shared camera graph uses Vulkan framebuffer orientation.  Metal's
    ;; target boundary owns the one required clip-space Y conversion.
    (ok (search
         "result.clip_position = float4((clip).x, -(clip).y, (clip).z, (clip).w);"
         source))
    (ok (search "result.shadow_depth_output = shadow_depth;" source))
    (ok (gethash expression
                 (msl:msl-document-expression-occurrences document)))))

(deftest msl-lowering-is-deterministic-and-does-not-perturb-spir-v
  (let* ((specification (spv:block-world-fragment-specification))
         (spir-v-before (spv:assemble-shader-specification specification))
         (first (msl:msl-document-source (msl:compile-msl specification)))
         (second (msl:msl-document-source (msl:compile-msl specification)))
         (spir-v-after (spv:assemble-shader-specification specification)))
    (ok (string= first second))
    (ok (equalp spir-v-before spir-v-after))
    (ok (typep (spv:lower-shader-specification :spir-v specification)
               'spv:shader-lowering))))

(deftest msl-occurrences-retain-expression-provenance
  (let* ((specification (spv:block-world-fragment-specification))
         (document (msl:compile-msl specification))
         (binding (binding-named 'reflected specification))
         (expression (spv:shader-binding-expression binding))
         (occurrences
           (gethash expression
                    (msl:msl-document-expression-occurrences document))))
    (ok occurrences)
    (ok (every (lambda (occurrence)
                 (eq expression
                     (gethash occurrence
                              (msl:msl-document-occurrence-expression
                               document))))
               occurrences))
    (ok (some (lambda (occurrence)
                (search "albedo" (msl:msl-source-occurrence-text occurrence)))
              occurrences))))

(deftest generated-msl-explains-quantities-in-plain-language
  (let* ((specification (spv:block-world-vertex-specification))
         (document (msl:compile-msl specification))
         (source (msl:msl-document-source document))
         (input (first (spv:shader-specification-inputs specification)))
         (input-structure (first (msl:msl-document-declarations document)))
         (field
           (msl-named "world_position"
                      (msl:msl-structure-fields input-structure)
                      #'msl:msl-field-name))
         (binding (binding-named 'view-z specification))
         (output
           (first (spv:shader-specification-statements specification)))
         (statement
           (msl-named
            "view_z"
            (msl:msl-entry-point-statements
             (msl:msl-document-entry-point document))
            (lambda (statement)
              (and (typep statement 'msl:msl-variable-statement)
                   (msl:msl-variable-statement-name statement))))))
    (ok (search
         "World position is a point-valued vector in the lattice coordinate kind, measured in cell units, and dimensionless."
         source))
    (ok (search
         "The xy lanes hold texture uv as a point-valued vector"
         source))
    (ok (search "This numeric value has no quantity annotation." source))
    (ok (search
         "View distance is a difference-valued scalar in the lattice coordinate kind, measured in cell units"
         source))
    (ok (eq input (msl:msl-field-origin field)))
    (ok (eq binding (msl:msl-variable-statement-origin statement)))
    (ok (eq output
            (msl:msl-output-statement-origin
             (find-if (lambda (statement)
                        (typep statement 'msl:msl-output-statement))
                      (msl:msl-entry-point-statements
                       (msl:msl-document-entry-point document))))))))

(deftest texture-parameters-describe-their-sampled-quantity-layout
  (let* ((specification (spv:block-world-fragment-specification))
         (document (msl:compile-msl specification))
         (source (msl:msl-document-source document))
         (resource
           (find "BLOCK-ATLAS"
                 (spv:shader-specification-resources specification)
                 :key (lambda (resource)
                        (symbol-name (spv:shader-object-name resource)))
                 :test #'string=))
         (parameter
           (msl-named
            "block_atlas"
            (msl:msl-entry-point-parameters
             (msl:msl-document-entry-point document))
            #'msl:msl-parameter-name)))
    (ok (search "The sampled xyz lanes hold linear rgb" source))
    (ok (search "The sampled w lane holds opacity" source))
    (ok (eq resource (msl:msl-parameter-origin parameter)))))

(deftest target-context-precedes-operator-identity
  (ok (equal
       (mapcar #'symbol-name
               (closer-mop:generic-function-argument-precedence-order
                #'spv:lower-shader-call))
       '("CONTEXT" "OPERATOR" "EXPRESSION"))))

(deftest slug-proof-lowers-to-direct-metal-pixel-mathematics
  (let* ((document
           (msl:compile-msl (slug:slug-bezier-fragment-specification)))
         (source (msl:msl-document-source document)))
    (ok (search "fragment SlugBezierFragmentSpecificationOutput" source))
    (ok (search "sign(" source))
    (ok (search "sqrt(" source))
    (ok (search "result.color_output" source))))

(deftest counted-fold-lowers-to-a-direct-metal-loop
  (let* ((specification
           (spv:parse-shader-specification
            'metal-fold-probe
            '(:stage :fragment
              :inputs ((count :float :location 0))
              :outputs ((result :float :location 0)))
            '((spv:set-output result
                              (spv:counted-fold
                                  (index count sum 0.0)
                                (+ sum index))))))
         (source
           (msl:msl-document-source (msl:compile-msl specification))))
    (ok (search "float fold_state_1 = 0.0f;" source))
    (ok (search "for (float fold_index_1 = 0.0f;" source))
    (ok (search "fold_state_1 = (fold_state_1 + fold_index_1);" source))
    (ok (search "result.result = fold_state_1;" source))))

(deftest unsupported-msl-boundaries-retain-source-reasons
  (flet ((reason-for (specification)
           (handler-case
               (progn (msl:compile-msl specification) nil)
             (spv:shader-language-error (condition)
               (spv:shader-language-error-reason condition)))))
    (let ((compute
            (spv:parse-shader-specification
             'compute-probe
             '(:stage :compute :outputs ((value :float :location 0)))
             '((spv:set-output value 1.0))))
          (descriptor-set
            (spv:parse-shader-specification
             'descriptor-set-probe
             '(:stage :fragment
               :outputs ((value :float :location 0))
               :resources ((image :texture-2d :set 1 :binding 0)))
             '((spv:set-output value 1.0)))))
      (ok (eq :unsupported-msl-stage (reason-for compute)))
      (ok (eq :unsupported-msl-descriptor-set
              (reason-for descriptor-set))))))
