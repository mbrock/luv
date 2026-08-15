(defpackage #:luv/msl/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:msl #:luv.msl)
                    (#:spv #:luv.spir-v)))

(in-package #:luv/msl/tests)

(defun binding-named (name specification)
  (find name (spv:shader-specification-bindings specification)
        :key #'spv:shader-object-name
        :test (lambda (left right)
                (string-equal (symbol-name left) (symbol-name right)))))

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

(deftest target-context-precedes-operator-identity
  (ok (equal
       (mapcar #'symbol-name
               (closer-mop:generic-function-argument-precedence-order
                #'spv:lower-shader-call))
       '("CONTEXT" "OPERATOR" "EXPRESSION"))))

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
