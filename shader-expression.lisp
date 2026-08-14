;;; Typed mathematical shader expressions and their deterministic SPIR-V lowering.
;;;
;;; The instruction language in SPIR-V.LISP remains deliberately literal.  This
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

(defclass shader-type ()
  ((name
    :initarg :name
    :reader shader-type-name)
   (component-count
    :initarg :component-count
    :initform nil
    :reader shader-type-component-count)
   (opaque-kind
    :initarg :opaque-kind
    :initform nil
    :reader shader-type-opaque-kind)))

(defmethod print-object ((type shader-type) stream)
  (print-unreadable-object (type stream :type t)
    (prin1 (shader-type-name type) stream)))

(defparameter *shader-types* (make-hash-table :test #'eq))

(defun register-shader-type (name &key component-count opaque-kind)
  (setf (gethash name *shader-types*)
        (make-instance 'shader-type
                       :name name
                       :component-count component-count
                       :opaque-kind opaque-kind)))

(register-shader-type :float :component-count 1)
(register-shader-type :vec2 :component-count 2)
(register-shader-type :vec3 :component-count 3)
(register-shader-type :vec4 :component-count 4)
(register-shader-type :texture-2d :opaque-kind :texture-2d)
(register-shader-type :sampler :opaque-kind :sampler)
(register-shader-type :uniform-block :opaque-kind :uniform-block)

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
  (shader-type= type :float))

(defun shader-vector-type-p (type)
  (let ((count (shader-type-component-count (find-shader-type type))))
    (and count (> count 1))))

(defclass shader-named-object ()
  ((name
    :initarg :name
    :reader shader-object-name)
   (source-form
    :initarg :source-form
    :initform nil
    :reader shader-object-source-form)))

(defclass shader-variable-declaration (shader-named-object)
  ((type
    :initarg :type
    :reader shader-declaration-type)))

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
    :reader shader-resource-binding)))

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

(defclass shader-binding (shader-named-object)
  ((expression
    :initarg :expression
    :reader shader-binding-expression)))

(defclass shader-expression ()
  ((type
    :initarg :type
    :reader shader-expression-type)
   (source-form
    :initarg :source-form
    :reader shader-expression-source-form)
   (name
    :initarg :name
    :initform nil
    :accessor shader-expression-name)))

(defclass shader-literal (shader-expression)
  ((value
    :initarg :value
    :reader shader-literal-value)))

(defclass shader-reference (shader-expression)
  ((target
    :initarg :target
    :reader shader-reference-target)))

(defclass shader-call (shader-expression)
  ((operator
    :initarg :operator
    :reader shader-call-operator)
   (operands
    :initarg :operands
    :reader shader-call-operands)
   (parameters
    :initarg :parameters
    :initform nil
    :reader shader-call-parameters)))

(defclass shader-output-assignment ()
  ((output
    :initarg :output
    :reader shader-assignment-output)
   (value
    :initarg :value
    :reader shader-assignment-value)
   (source-form
    :initarg :source-form
    :reader shader-assignment-source-form)))

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
  (shader-literal-value expression))

(defmethod shader-expression-form ((expression shader-reference))
  (shader-object-name (shader-reference-target expression)))

(defmethod shader-expression-form ((expression shader-call))
  ;; The operator is the symbol the author wrote, so the form rebuilds by
  ;; simple reassembly; trailing parameters cover special syntax like
  ;; SWIZZLE's component designator.
  (append (cons (shader-call-operator expression)
                (mapcar #'shader-expression-form
                        (shader-call-operands expression)))
          (shader-call-parameters expression)))

(defmethod print-object ((expression shader-expression) stream)
  (print-unreadable-object (expression stream :type t)
    (prin1 (shader-expression-form expression) stream)
    (format stream " : ~A"
            (shader-type-name (shader-expression-type expression)))))

(defgeneric shader-expression-children (expression)
  (:documentation "The immediate subexpressions EXPRESSION computes from."))

(defmethod shader-expression-children ((expression shader-expression))
  nil)

(defmethod shader-expression-children ((expression shader-call))
  (shader-call-operands expression))

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
        (visit (shader-assignment-value statement))))
    (nreverse expressions)))

(defun shader-symbol= (left right)
  (and (symbolp left) (symbolp right)
       (string-equal (symbol-name left) (symbol-name right))))

(defun shader-environment-value (name environment source-form)
  (or (loop for (candidate . value) in environment
            when (shader-symbol= name candidate)
              return value)
      (error 'shader-language-error
             :form source-form :reason :unknown-name :details name)))

(defun make-shader-reference (name environment source-form)
  (let ((target (shader-environment-value name environment source-form)))
    (make-instance 'shader-reference
                   :target target
                   :type (etypecase target
                           (shader-variable-declaration
                            (shader-declaration-type target))
                           (shader-binding
                            (shader-expression-type
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
    (cond ((null vectors) (find-shader-type :float))
          ((every (lambda (type)
                    (or (shader-float-type-p type)
                        (shader-type= type (first vectors))))
                  types)
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
                (shader-float-type-p (second types)))
           (first types))
          (t
           (error 'shader-language-error
                  :form source-form :reason :incompatible-division-types
                  :details (mapcar #'shader-type-name types))))))

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

(defun vector-type-for-width (width source-form)
  (find-shader-type
   (ecase width (1 :float) (2 :vec2) (3 :vec3) (4 :vec4))
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
          (shader-type= (first types) (second types))))
   operands source-form :invalid-dot-product)
  (find-shader-type :float))

(defmethod infer-shader-call-type ((operator (eql 'sample)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 3)
          (eq (shader-type-opaque-kind (first types)) :texture-2d)
          (eq (shader-type-opaque-kind (second types)) :sampler)
          (shader-type= (third types) :vec2)))
   operands source-form :invalid-texture-sample)
  (find-shader-type :vec4))

(defmethod infer-shader-call-type ((operator (eql 'mix)) operands source-form)
  (require-shader-types
   (lambda (types)
     (and (= (length types) 3)
          (shader-type= (first types) (second types))
          (shader-numeric-type-p (first types))
          (shader-float-type-p (third types))))
   operands source-form :invalid-mix)
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
(define-shader-operator mix
  "Linear interpolation from one value toward another by a scalar amount.")
(define-shader-operator vec2
  "Construct a two-component vector from scalars and vectors of total width 2.")
(define-shader-operator vec3
  "Construct a three-component vector from scalars and vectors of total width 3.")
(define-shader-operator vec4
  "Construct a four-component vector from scalars and vectors of total width 4.")
(define-shader-operator swizzle
  "Select and reorder vector components by a designator such as :XYZ or :RGB.")

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
    (make-instance 'shader-call
                   :operator 'swizzle
                   :operands (list operand)
                   :parameters (list designator)
                   :type (vector-type-for-width (length indices) form)
                   :source-form form)))

(defun parse-shader-call (form environment)
  (let ((operator (first form)))
    (unless (shader-operator-p operator)
      (error 'shader-language-error
             :form form :reason :unknown-operator :details operator))
    (parse-shader-operator-call operator form environment)))

(defun parse-shader-expression (form environment)
  (cond ((realp form)
         (make-instance 'shader-literal
                        :value (coerce form 'single-float)
                        :type (find-shader-type :float)
                        :source-form form))
        ((symbolp form) (make-shader-reference form environment form))
        ((consp form) (parse-shader-call form environment))
        (t
         (error 'shader-language-error
                :form form :reason :unsupported-expression))))

(defun parse-interface-declaration (form direction)
  (destructuring-bind (name type &key location built-in) form
    (unless (or (and (typep location '(integer 0 *)) (null built-in))
                (and (null location) built-in))
      (error 'shader-language-error
             :form form :reason :invalid-interface-decoration
             :details (list :location location :built-in built-in)))
    (make-instance 'shader-interface-variable
                   :name name
                   :type (find-shader-type type form)
                   :direction direction
                   :location location
                   :built-in built-in
                   :source-form form)))

(defun parse-resource-declaration (form)
  (destructuring-bind (name type &key (set 0) binding members) form
    (unless (and (typep set '(integer 0 *))
                 (typep binding '(integer 0 *)))
      (error 'shader-language-error
             :form form :reason :invalid-resource-location
             :details (list set binding)))
    (if (shader-symbol= type :uniform-block)
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
                      (destructuring-bind (member-name member-type)
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
                          (make-instance
                           'shader-uniform-member
                           :name member-name :type resolved-type
                           :block block :index index :offset (* index 16)
                           :source-form member-form)))))
          block)
        (let ((resolved-type (find-shader-type type form)))
          (unless (and (shader-type-opaque-kind resolved-type)
                       (not (eq (shader-type-opaque-kind resolved-type)
                                :uniform-block)))
        (error 'shader-language-error
               :form form :reason :non-resource-type :details type))
          (when members
            (error 'shader-language-error
                   :form form :reason :members-on-opaque-resource))
          (make-instance 'shader-resource
                         :name name :type resolved-type
                         :descriptor-set set :binding binding
                         :source-form form)))))

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
    (make-instance 'shader-output-assignment
                   :output output :value value :source-form form)))

(defun parse-shader-body (body environment outputs)
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
                              (parse-output-assignment
                               statement lexical-environment outputs))
                            statements))))
        (values nil (list (parse-output-assignment form environment outputs))))))

(defun parse-shader-specification (name options body)
  (let* ((stage (getf options :stage))
         (inputs (mapcar (lambda (form)
                           (parse-interface-declaration form :input))
                         (getf options :inputs)))
         (outputs (mapcar (lambda (form)
                            (parse-interface-declaration form :output))
                          (getf options :outputs)))
         (resources (mapcar #'parse-resource-declaration
                            (getf options :resources)))
         (environment-items
           (append inputs
                   (loop for resource in resources
                         if (typep resource 'shader-uniform-block)
                           append (shader-uniform-block-members resource)
                         else collect resource)))
         (environment
           (mapcar (lambda (item) (cons (shader-object-name item) item))
                   environment-items)))
    (unless (member stage '(:vertex :fragment :compute))
      (error 'shader-language-error
             :form options :reason :invalid-stage :details stage))
    (multiple-value-bind (bindings statements)
        (parse-shader-body body environment outputs)
      (make-instance 'shader-specification
                     :name name :stage stage
                     :inputs inputs :outputs outputs :resources resources
                     :bindings bindings :statements statements
                     :source-form (list* 'define-shader name options body)))))

(defmacro define-shader (name options &body body)
  "Define NAME as a function returning a durable, inspectable shader graph."
  (let* ((package (or (symbol-package name) *package*))
         (variable (intern (format nil "*~A*" (symbol-name name)) package)))
    `(progn
       (defparameter ,variable
         (parse-shader-specification ',name ',options ',body))
       (defun ,name () ,variable))))

;;; Live definitions ---------------------------------------------------------

(defgeneric shader-specification-for (role stage)
  (:documentation
   "Return the current durable shader specification for ROLE and STAGE."))

(defmacro define-shader-method
    (generic-function name specialized-lambda-list options &body body)
  "Define a shader-producing method with ordinary DEFMETHOD identity.

The parsed graph is constructed once when the method definition is loaded or
evaluated.  Calling the generic function is consequently pure and cheap, while
re-evaluating an identical qualifier/specializer coordinate replaces the old
method and lets the MOP announce that definitional change."
  `(defmethod ,generic-function ,specialized-lambda-list
     (load-time-value
      (parse-shader-specification ',name ',options ',body)
      t)))

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

(defclass shader-lowering-context ()
  ((type-ids :initform (make-hash-table :test #'eq) :accessor context-type-ids)
   (pointer-ids :initform (make-hash-table :test #'equal)
                :accessor context-pointer-ids)
   (constant-ids :initform (make-hash-table :test #'equal)
                 :accessor context-constant-ids)
   (variable-ids :initform (make-hash-table :test #'eq)
                 :accessor context-variable-ids)
   (uniform-struct-ids :initform (make-hash-table :test #'eq)
                       :accessor context-uniform-struct-ids)
   (loaded-values :initform (make-hash-table :test #'eq)
                  :accessor context-loaded-values)
   (loaded-instructions :initform (make-hash-table :test #'eq)
                        :accessor context-loaded-instructions)
   (constant-instructions :initform (make-hash-table :test #'equal)
                          :accessor context-constant-instructions)
   (expression-values :initform (make-hash-table :test #'eq)
                      :accessor context-expression-values)
   (expression-instructions :initform (make-hash-table :test #'eq)
                            :reader context-expression-instructions)
   (instruction-expressions :initform (make-hash-table :test #'eq)
                            :reader context-instruction-expressions)
   (claimed-ids :initform (make-hash-table :test #'eq)
                :accessor context-claimed-ids)
   (name-counts :initform (make-hash-table :test #'equal)
                :accessor context-name-counts)
   (type-declarations :initform nil :accessor context-type-declarations)
   (constant-declarations :initform nil :accessor context-constant-declarations)
   (variable-declarations :initform nil :accessor context-variable-declarations)
   (annotations :initform nil :accessor context-annotations)
   (interfaces :initform nil :accessor context-interfaces)
   (instructions :initform nil :accessor context-instructions)))

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
    (setf (gethash id (context-claimed-ids context)) t)
    id))

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
           (cond ((eq kind :texture-2d)
                  (list id 'type-image
                        (ensure-shader-type-id context :float)
                        '2d 0 0 0 1 'unknown))
                 ((eq kind :sampler) (list id 'type-sampler))
                 ((= (shader-type-component-count type) 1)
                  (list id 'type-float 32))
                 (t
                  (list id 'type-vector
                        (ensure-shader-type-id context :float)
                        (shader-type-component-count type)))))
          id))))

(defun ensure-void-type-id (context)
  (let ((id (shader-id "VOID")))
    (unless (gethash id (context-claimed-ids context))
      (reserve-shader-id context "VOID")
      (append-context-form 'type-declarations context
                           (list id 'type-void)))
    id))

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
        (let ((type-id (or (gethash :uint (context-type-ids context))
                           (let ((id (reserve-shader-id context "UINT")))
                             (setf (gethash :uint (context-type-ids context)) id)
                             (append-context-form 'type-declarations context
                                                  (list id 'type-int 32 0))
                             id)))
              (id (reserve-shader-id context
                                     (format nil "UINT-~D" value))))
          (setf (gethash key (context-constant-ids context)) id)
          (append-context-form 'constant-declarations context
                               (list id 'constant type-id value))
          id))))

(defun ensure-sampled-image-type-id (context)
  (let* ((key :sampled-image)
         (table (context-pointer-ids context)))
    (or (gethash key table)
        (let ((id (reserve-shader-id context "SAMPLED-IMAGE")))
          (setf (gethash key table) id)
          (append-context-form
           'type-declarations context
           (list id 'type-sampled-image
                 (ensure-shader-type-id context :texture-2d)))
          id))))

(defun register-shader-variable (context declaration)
  (let* ((direction
           (etypecase declaration
             (shader-interface-variable
              (ecase (shader-interface-direction declaration)
                (:input 'input)
                (:output 'output)))
             (shader-uniform-block 'uniform)
             (shader-resource 'uniform-constant)))
         (type (shader-declaration-type declaration))
         (pointer-id
           (if (typep declaration 'shader-uniform-block)
               (ensure-uniform-block-pointer-type-id context declaration)
               (ensure-pointer-type-id context direction type)))
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
              (shader-resource-binding declaration)))))
    variable-id))

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

(defun expression-result-name (expression)
  (or (shader-expression-name expression)
      (shader-expression-provenance-name expression)))

(defun emit-value-instruction (context expression type instruction operands)
  (let ((result (fresh-shader-id context (expression-result-name expression))))
    (emit-shader-instruction
     context expression
     (list* result instruction (ensure-shader-type-id context type) operands))
    result))

(defun lower-shader-reference (context expression)
  (let ((target (shader-reference-target expression)))
    (etypecase target
      (shader-binding
       (let* ((source (shader-binding-expression target))
              (value (lower-shader-expression context source)))
         (alias-shader-expression context expression source)
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
         (if found-p
             (progn
               (associate-shader-instruction
                context expression
                (gethash target (context-loaded-instructions context)))
               value)
             (let* ((type (shader-declaration-type target))
                    (value
                      (emit-value-instruction
                       context expression type 'load
                       (list (gethash target
                                      (context-variable-ids context)))))
                    (instruction (car (last (context-instructions context)))))
               (setf (gethash target (context-loaded-values context)) value
                     (gethash target (context-loaded-instructions context))
                     instruction)
               value)))))))

(defgeneric binary-arithmetic-instruction (operator left-type right-type)
  (:documentation
   "The SPIR-V instruction computing one binary step of OPERATOR."))

(defmethod binary-arithmetic-instruction ((operator (eql '+)) left-type right-type)
  (declare (ignore left-type right-type))
  'f-add)

(defmethod binary-arithmetic-instruction ((operator (eql '-)) left-type right-type)
  (declare (ignore left-type right-type))
  'f-sub)

(defmethod binary-arithmetic-instruction ((operator (eql '/)) left-type right-type)
  (declare (ignore left-type right-type))
  'f-div)

(defmethod binary-arithmetic-instruction ((operator (eql '*)) left-type right-type)
  (if (or (and (shader-vector-type-p left-type)
               (shader-float-type-p right-type))
          (and (shader-float-type-p left-type)
               (shader-vector-type-p right-type)))
      'vector-times-scalar
      'f-mul))

(defun emit-binary-arithmetic
    (context expression operator result-type left-id left-type right-id right-type)
  (let ((instruction
          (binary-arithmetic-instruction operator left-type right-type)))
    (when (and (eq instruction 'vector-times-scalar)
               (shader-float-type-p left-type))
      (rotatef left-id right-id))
    (emit-value-instruction context expression result-type instruction
                            (list left-id right-id))))

(defgeneric lower-shader-call (operator context expression)
  (:documentation
   "Emit EXPRESSION's instructions into CONTEXT and return its value id."))

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
                     (t (find-shader-type :float)))
               value value-type operand-value operand-type)
              value-type
              (cond ((shader-vector-type-p value-type) value-type)
                    ((shader-vector-type-p operand-type) operand-type)
                    (t (find-shader-type :float))))))))

(defmethod lower-shader-call ((operator (eql '+)) context expression)
  (lower-chained-arithmetic context expression))

(defmethod lower-shader-call ((operator (eql '*)) context expression)
  (lower-chained-arithmetic context expression))

(defmethod lower-shader-call ((operator (eql '-)) context expression)
  (let ((operands (shader-call-operands expression)))
    (if (= (length operands) 1)
        (emit-value-instruction
         context expression (shader-expression-type expression) 'f-negate
         (list (lower-shader-expression context (first operands))))
        (lower-chained-arithmetic context expression))))

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

(defmethod lower-shader-call ((operator (eql 'swizzle)) context expression)
  (let* ((operand (first (shader-call-operands expression)))
         (value (lower-shader-expression context operand))
         (indices (swizzle-components
                   (first (shader-call-parameters expression))
                   (shader-expression-source-form expression))))
    (if (= (length indices) 1)
        (emit-value-instruction context expression
                                (shader-expression-type expression)
                                'composite-extract
                                (list value (first indices)))
        (emit-value-instruction context expression
                                (shader-expression-type expression)
                                'vector-shuffle
                                (list* value value indices)))))

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

(defmethod lower-shader-call ((operator (eql 'sample)) context expression)
  (destructuring-bind (texture sampler coordinate)
      (shader-call-operands expression)
    (let* ((texture-id (lower-shader-expression context texture))
           (sampler-id (lower-shader-expression context sampler))
           (coordinate-id (lower-shader-expression context coordinate))
           (sampled-id
             (fresh-shader-id context
                              (expression-result-name expression))))
      (emit-shader-instruction
       context expression
       (list sampled-id 'sampled-image
             (ensure-sampled-image-type-id context)
             texture-id sampler-id))
      (emit-value-instruction
       context expression :vec4 'image-sample-implicit-lod
       (list sampled-id coordinate-id)))))

(defgeneric lower-shader-expression-value (context expression)
  (:documentation "Lower EXPRESSION into instructions and return its value id."))

(defmethod lower-shader-expression-value (context (expression shader-literal))
  (ensure-shader-constant context
                          (shader-literal-value expression)
                          expression))

(defmethod lower-shader-expression-value (context (expression shader-reference))
  (lower-shader-reference context expression))

(defmethod lower-shader-expression-value (context (expression shader-call))
  (lower-shader-call (shader-call-operator expression) context expression))

(defun lower-shader-expression (context expression)
  (or (gethash expression (context-expression-values context))
      (setf (gethash expression (context-expression-values context))
            (lower-shader-expression-value context expression))))

(defun shader-entry-execution-model (stage)
  (ecase stage
    (:vertex 'vertex)
    (:fragment 'fragment)
    (:compute 'gl-compute)))

(defun compile-shader-specification (specification)
  "Lower SPECIFICATION and retain bidirectional expression/instruction links."
  (check-type specification shader-specification)
  (let* ((context (make-instance 'shader-lowering-context))
         (void-id (ensure-void-type-id context))
         (main-id (reserve-shader-id context "MAIN"))
         (entry-id (reserve-shader-id context "ENTRY"))
         (function-type-id (reserve-shader-id context "FUNCTION-TYPE")))
    (append-context-form 'type-declarations context
                         (list function-type-id 'type-function void-id))
    (dolist (declaration
             (append (shader-specification-inputs specification)
                     (shader-specification-outputs specification)
                     (shader-specification-resources specification)))
      (register-shader-variable context declaration))
    ;; LET* is part of the language contract, not merely pretty syntax.  Emit
    ;; binding computations in source order so the resulting basic block reads
    ;; alongside the specification and retains ordinary Lisp evaluation order.
    (dolist (binding (shader-specification-bindings specification))
      (lower-shader-expression context (shader-binding-expression binding)))
    (dolist (statement (shader-specification-statements specification))
      (let* ((value-expression (shader-assignment-value statement))
             (value (lower-shader-expression context value-expression))
             (output-id
               (gethash (shader-assignment-output statement)
                        (context-variable-ids context))))
        (emit-shader-instruction context value-expression
                                 (list 'store output-id value))))
    (emit-shader-instruction context nil '(return))
    (let* ((module
             (make-instance
              'spir-v-module
              :entry-points
              (list (make-instance
                     'spir-v-entry-point
                     :execution-model
                     (shader-entry-execution-model
                      (shader-specification-stage specification))
                     :function main-id
                     :interfaces (context-interfaces context)))
              :execution-modes
              (when (eq (shader-specification-stage specification) :fragment)
                (list (make-instance 'spir-v-execution-mode
                                     :function main-id
                                     :name 'origin-upper-left)))
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
                (list (make-instance 'spir-v-basic-block
                                     :label entry-id
                                     :instructions
                                     (context-instructions context)))))))
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

(defun shader-module (specification)
  (shader-lowering-module (compile-shader-specification specification)))

(defun assemble-shader-specification (specification)
  (assemble-spir-v-module (shader-module specification)))
