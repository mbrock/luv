(defpackage #:luv/spir-v/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:spv #:luv.spir-v)))

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

(deftest shader-source-is-a-typed-clos-graph
  (let* ((specification (spv:block-world-fragment-specification))
         (sun (binding-named 'sun specification))
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
    (ok (find "VECTOR-TIMES-SCALAR" instructions
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

(deftest shader-lowering-is-deterministic-and-assemblable
  (flet ((forms ()
           (mapcar #'spv:instruction-form
                   (spv:lower-spir-v
                    (spv:block-world-fragment-module)))))
    (ok (equal (forms) (forms)))
    (let ((words (spv:block-world-fragment-shader)))
      (ok (> (length words) 5))
      (ok (= (aref words 0) #x07230203)))))

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
