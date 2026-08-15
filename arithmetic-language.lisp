;;; Backend-neutral compiled arithmetic definitions and expression graphs.
;;;
;;; This is the shared source/semantic frontend.  It deliberately does not
;;; choose single versus double floats, a vector representation, an execution
;;; strategy, or a lowering backend.  A realization consumes these inspectable
;;; objects after quantity checking and chooses those machine details itself.

(in-package #:luv.arithmetic.language)

(define-condition arithmetic-language-error (error)
  ((form
    :initarg :form
    :initform nil
    :reader arithmetic-language-error-form)
   (reason
    :initarg :reason
    :reader arithmetic-language-error-reason)
   (details
    :initarg :details
    :initform nil
    :reader arithmetic-language-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Cannot understand arithmetic expression ~S: ~A~@[ (~S)~]."
             (arithmetic-language-error-form condition)
             (arithmetic-language-error-reason condition)
             (arithmetic-language-error-details condition)))))

(defmacro with-arithmetic-quantity-errors ((source-form reason) &body body)
  `(handler-case
       (progn ,@body)
     (math:undefined-unit (condition)
       (error 'arithmetic-language-error
              :form ,source-form :reason :undefined-unit
              :details (math:undefined-unit-name condition)))
     (math:quantity-operation-error (condition)
       (error 'arithmetic-language-error
              :form ,source-form :reason ,reason
              :details (math:quantity-operation-error-reason condition)))))

(defclass arithmetic-named-object ()
  ((name
    :initarg :name
    :reader arithmetic-object-name)
   (source-form
    :initarg :source-form
    :initform nil
    :reader arithmetic-object-source-form)))

(defclass arithmetic-expression ()
  ((quantity-specification
    :initarg :quantity-specification
    :initform nil
    :reader arithmetic-expression-quantity-specification)
   (quantity-layout
    :initarg :quantity-layout
    :initform nil
    :reader arithmetic-expression-quantity-layout)
   (source-form
    :initarg :source-form
    :reader arithmetic-expression-source-form)
   (name
    :initarg :name
    :initform nil
    :accessor arithmetic-expression-name))
  (:documentation
   "A source expression with semantic meaning but no chosen machine form."))

(defclass arithmetic-parameter (arithmetic-named-object)
  ((quantity-specification
    :initarg :quantity-specification
    :initform nil
    :reader arithmetic-parameter-quantity-specification)
   (quantity-layout
    :initarg :quantity-layout
    :initform nil
    :reader arithmetic-parameter-quantity-layout)))

(defclass arithmetic-binding (arithmetic-named-object)
  ((expression
    :initarg :expression
    :reader arithmetic-binding-expression)))

(defclass arithmetic-literal (arithmetic-expression)
  ((value
    :initarg :value
    :reader arithmetic-literal-value)))

(defclass arithmetic-reference (arithmetic-expression)
  ((target
    :initarg :target
    :reader arithmetic-reference-target)))

(defclass arithmetic-call (arithmetic-expression)
  ((operator
    :initarg :operator
    :reader arithmetic-call-operator)
   (operands
    :initarg :operands
    :reader arithmetic-call-operands)
   (parameters
    :initarg :parameters
    :initform nil
    :reader arithmetic-call-parameters)))

(defclass arithmetic-quantity-boundary (arithmetic-expression)
  ((operand
    :initarg :operand
    :reader arithmetic-quantity-boundary-operand)))

(defclass arithmetic-quantity-construction (arithmetic-quantity-boundary) ())
(defclass arithmetic-quantity-assumption (arithmetic-quantity-boundary) ())
(defclass arithmetic-interpretation (arithmetic-quantity-boundary) ())
(defclass arithmetic-representation (arithmetic-quantity-boundary) ())

(defclass arithmetic-unit-conversion (arithmetic-expression)
  ((operand
    :initarg :operand
    :reader arithmetic-unit-conversion-operand)
   (factor
    :initarg :factor
    :reader arithmetic-unit-conversion-factor)))

(defgeneric arithmetic-reference-target-name (target)
  (:documentation "Return the source name denoted by reference TARGET."))

(defmethod arithmetic-reference-target-name ((target arithmetic-named-object))
  (arithmetic-object-name target))

(defgeneric arithmetic-reference-target-quantity-checked-p (target)
  (:documentation "Whether semantic checking is active at reference TARGET."))

(defgeneric arithmetic-reference-target-quantity-specification (target)
  (:documentation "Return TARGET's homogeneous quantity specification, or NIL."))

(defgeneric arithmetic-reference-target-quantity-layout (target)
  (:documentation "Return TARGET's packed quantity layout, or NIL."))

(defmethod arithmetic-reference-target-quantity-checked-p
    ((target arithmetic-parameter))
  (or (arithmetic-parameter-quantity-specification target)
      (arithmetic-parameter-quantity-layout target)))

(defmethod arithmetic-reference-target-quantity-specification
    ((target arithmetic-parameter))
  (arithmetic-parameter-quantity-specification target))

(defmethod arithmetic-reference-target-quantity-layout
    ((target arithmetic-parameter))
  (arithmetic-parameter-quantity-layout target))

(defmethod arithmetic-reference-target-quantity-checked-p
    ((target arithmetic-binding))
  (arithmetic-expression-quantity-checked-p
   (arithmetic-binding-expression target)))

(defmethod arithmetic-reference-target-quantity-specification
    ((target arithmetic-binding))
  (arithmetic-expression-quantity-specification
   (arithmetic-binding-expression target)))

(defmethod arithmetic-reference-target-quantity-layout
    ((target arithmetic-binding))
  (arithmetic-expression-quantity-layout
   (arithmetic-binding-expression target)))

(defgeneric arithmetic-expression-quantity-checked-p (expression)
  (:documentation
   "Whether semantic quantity checking is active at EXPRESSION."))

(defmethod arithmetic-expression-quantity-checked-p
    ((expression arithmetic-literal))
  nil)

(defmethod arithmetic-expression-quantity-checked-p
    ((expression arithmetic-reference))
  (arithmetic-reference-target-quantity-checked-p
   (arithmetic-reference-target expression)))

(defmethod arithmetic-expression-quantity-checked-p
    ((expression arithmetic-call))
  (or (arithmetic-expression-quantity-specification expression)
      (arithmetic-expression-quantity-layout expression)
      (some #'arithmetic-expression-quantity-checked-p
            (arithmetic-call-operands expression))))

(defmethod arithmetic-expression-quantity-checked-p
    ((expression arithmetic-quantity-boundary))
  (declare (ignore expression))
  t)

(defmethod arithmetic-expression-quantity-checked-p
    ((expression arithmetic-representation))
  (declare (ignore expression))
  nil)

(defmethod arithmetic-expression-quantity-checked-p
    ((expression arithmetic-unit-conversion))
  (declare (ignore expression))
  t)

(defgeneric arithmetic-expression-form (expression)
  (:documentation "Reconstruct the compact source form of EXPRESSION."))

(defmethod arithmetic-expression-form ((expression arithmetic-literal))
  (arithmetic-literal-value expression))

(defmethod arithmetic-expression-form ((expression arithmetic-reference))
  (arithmetic-reference-target-name
   (arithmetic-reference-target expression)))

(defmethod arithmetic-expression-form ((expression arithmetic-call))
  (append (cons (arithmetic-call-operator expression)
                (mapcar #'arithmetic-expression-form
                        (arithmetic-call-operands expression)))
          (arithmetic-call-parameters expression)))

(defmethod arithmetic-expression-form
    ((expression arithmetic-quantity-boundary))
  (arithmetic-expression-source-form expression))

(defmethod arithmetic-expression-form ((expression arithmetic-unit-conversion))
  (arithmetic-expression-source-form expression))

(defmethod print-object ((expression arithmetic-expression) stream)
  (print-unreadable-object (expression stream :type t)
    (prin1 (arithmetic-expression-form expression) stream)))

(defgeneric arithmetic-expression-children (expression)
  (:documentation "Return EXPRESSION's immediate source operands."))

(defmethod arithmetic-expression-children ((expression arithmetic-expression))
  nil)

(defmethod arithmetic-expression-children ((expression arithmetic-call))
  (arithmetic-call-operands expression))

(defmethod arithmetic-expression-children
    ((expression arithmetic-quantity-boundary))
  (list (arithmetic-quantity-boundary-operand expression)))

(defmethod arithmetic-expression-children
    ((expression arithmetic-unit-conversion))
  (list (arithmetic-unit-conversion-operand expression)))

(defclass arithmetic-function-definition (arithmetic-named-object)
  ((parameters
    :initarg :parameters
    :reader arithmetic-function-parameters)
   (bindings
    :initarg :bindings
    :initform nil
    :reader arithmetic-function-bindings)
   (result
    :initarg :result
    :reader arithmetic-function-result))
  (:documentation
   "A checked arithmetic source definition awaiting an execution backend."))

(defun arithmetic-function-expressions (definition)
  "Return DEFINITION's expression graph in source order without duplicates."
  (let ((seen (make-hash-table :test #'eq))
        (expressions nil))
    (labels ((visit (expression)
               (unless (gethash expression seen)
                 (setf (gethash expression seen) t)
                 (push expression expressions)
                 (mapc #'visit (arithmetic-expression-children expression)))))
      (dolist (binding (arithmetic-function-bindings definition))
        (visit (arithmetic-binding-expression binding)))
      (visit (arithmetic-function-result definition)))
    (nreverse expressions)))

(defvar *arithmetic-operator-documentation* (make-hash-table :test #'eq))

(defmethod documentation ((name symbol) (type (eql 'arithmetic-operator)))
  (gethash name *arithmetic-operator-documentation*))

(defmethod (setf documentation)
    (new-value (name symbol) (type (eql 'arithmetic-operator)))
  (if new-value
      (setf (gethash name *arithmetic-operator-documentation*) new-value)
      (progn (remhash name *arithmetic-operator-documentation*) nil)))

(defgeneric arithmetic-operator-p (operator)
  (:documentation "Whether OPERATOR belongs to the compiled arithmetic language."))

(defmethod arithmetic-operator-p (operator)
  (declare (ignore operator))
  nil)

(defmacro define-arithmetic-operator (name &optional documentation)
  "Admit NAME to the open arithmetic vocabulary through an EQL method."
  `(progn
     (defmethod arithmetic-operator-p ((operator (eql ',name)))
       (declare (ignore operator))
       t)
     ,@(when documentation
         `((setf (documentation ',name 'arithmetic-operator) ,documentation)))
     ',name))

(define-arithmetic-operator + "Addition over compatible quantities.")
(define-arithmetic-operator - "Subtraction or unary negation.")
(define-arithmetic-operator * "Multiplication and scalar scaling.")
(define-arithmetic-operator / "Division of two represented quantities.")
(define-arithmetic-operator dot "The inner product of two vectors.")
(define-arithmetic-operator min "The minimum of compatible quantities.")
(define-arithmetic-operator max "The maximum of compatible quantities.")
(define-arithmetic-operator clamp "Constrain a quantity between compatible bounds.")
(define-arithmetic-operator mix "Interpolate compatible quantities by a scalar amount.")
(define-arithmetic-operator smoothstep "Produce dimensionless progress across compatible edges.")
(define-arithmetic-operator step "Compare compatible quantities and produce dimensionless values.")
(define-arithmetic-operator normalize "Normalize a dimensionless vector.")
(define-arithmetic-operator expt "Raise a dimensionless value to a dimensionless power.")
(define-arithmetic-operator quantity "Construct a meaningful literal.")
(define-arithmetic-operator assume-quantity "State external meaning for a raw value.")
(define-arithmetic-operator interpret "Name a compatible derived quantity.")
(define-arithmetic-operator representation "Expose a quantity's raw representation.")
(define-arithmetic-operator convert-unit "Convert a quantity to a compatible unit.")

(defun arithmetic-environment-value (name environment source-form)
  (or (cdr (assoc name environment :test #'eq))
      (error 'arithmetic-language-error
             :form source-form :reason :unknown-name :details name)))

(defun make-arithmetic-reference (name environment source-form)
  (let ((target (arithmetic-environment-value name environment source-form)))
    (make-instance
     'arithmetic-reference
     :target target
     :quantity-specification
     (arithmetic-reference-target-quantity-specification target)
     :quantity-layout
     (arithmetic-reference-target-quantity-layout target)
     :source-form source-form)))

(defun parse-source-quantity-specification
    (options source-form &key (default-tensor-order 0))
  (destructuring-bind
      (&key (quantity nil quantity-supplied-p)
            (dimension nil dimension-supplied-p)
            (unit nil unit-supplied-p)
            (tensor-order default-tensor-order tensor-order-supplied-p)
            (affine-p nil affine-p-supplied-p))
      options
    (when (or quantity-supplied-p dimension-supplied-p unit-supplied-p
              tensor-order-supplied-p affine-p-supplied-p)
      (with-arithmetic-quantity-errors
          (source-form :invalid-quantity-declaration)
        (apply
         #'math:make-quantity-specification quantity
         (append
          (and dimension-supplied-p (list :dimension dimension))
          (list :unit unit :tensor-order tensor-order :affine-p affine-p)))))))

(defun infer-arithmetic-call-quantity-specification
    (operator operands source-form)
  (when (some #'arithmetic-expression-quantity-checked-p operands)
    (let ((specifications
            (mapcar #'arithmetic-expression-quantity-specification operands)))
      (unless (every #'identity specifications)
        (error 'arithmetic-language-error
               :form source-form :reason :missing-quantity-specification
               :details
               (loop for operand in operands
                     for specification in specifications
                     unless specification
                       collect (arithmetic-expression-form operand))))
      (with-arithmetic-quantity-errors
          (source-form :invalid-quantity-operation)
        (apply #'math:derive-quantity-specification operator specifications)))))

(defgeneric parse-arithmetic-operator-call (operator form environment)
  (:documentation "Parse one arithmetic call into an inspectable expression."))

(defmethod parse-arithmetic-operator-call (operator form environment)
  (let ((operands
          (mapcar (lambda (operand)
                    (parse-arithmetic-expression operand environment))
                  (rest form))))
    (make-instance
     'arithmetic-call
     :operator operator
     :operands operands
     :quantity-specification
     (infer-arithmetic-call-quantity-specification operator operands form)
     :source-form form)))

(defgeneric arithmetic-constant-expression-p (expression)
  (:documentation "Whether EXPRESSION is a literal construction source."))

(defmethod arithmetic-constant-expression-p (expression)
  (declare (ignore expression))
  nil)

(defmethod arithmetic-constant-expression-p ((expression arithmetic-literal))
  t)

(defun parse-raw-quantity-boundary
    (class form environment &key constant-only-p)
  (destructuring-bind (name operand-form &rest options) form
    (declare (ignore name))
    (let* ((operand (parse-arithmetic-expression operand-form environment))
           (specification
             (parse-source-quantity-specification options form)))
      (unless specification
        (error 'arithmetic-language-error
               :form form :reason :missing-quantity-interpretation))
      (when (or (arithmetic-expression-quantity-checked-p operand)
                (arithmetic-expression-quantity-layout operand))
        (error 'arithmetic-language-error
               :form form :reason :quantity-already-has-semantics
               :details (arithmetic-expression-form operand)))
      (when (and constant-only-p
                 (not (arithmetic-constant-expression-p operand)))
        (error 'arithmetic-language-error
               :form form :reason :quantity-requires-literal-construction
               :details (arithmetic-expression-form operand)))
      (make-instance class
                     :operand operand
                     :quantity-specification specification
                     :source-form form))))

(defmethod parse-arithmetic-operator-call
    ((operator (eql 'quantity)) form environment)
  (declare (ignore operator))
  (parse-raw-quantity-boundary
   'arithmetic-quantity-construction form environment :constant-only-p t))

(defmethod parse-arithmetic-operator-call
    ((operator (eql 'assume-quantity)) form environment)
  (declare (ignore operator))
  (parse-raw-quantity-boundary
   'arithmetic-quantity-assumption form environment))

(defmethod parse-arithmetic-operator-call
    ((operator (eql 'interpret)) form environment)
  (declare (ignore operator))
  (destructuring-bind (name operand-form &rest options) form
    (declare (ignore name))
    (let* ((operand (parse-arithmetic-expression operand-form environment))
           (interpretation
             (parse-source-quantity-specification options form)))
      (unless interpretation
        (error 'arithmetic-language-error
               :form form :reason :missing-quantity-interpretation))
      (with-arithmetic-quantity-errors
          (form :invalid-quantity-interpretation)
        (math:interpret-quantity-specification
         (and (arithmetic-expression-quantity-checked-p operand)
              (arithmetic-expression-quantity-specification operand))
         interpretation))
      (make-instance 'arithmetic-interpretation
                     :operand operand
                     :quantity-specification interpretation
                     :source-form form))))

(defmethod parse-arithmetic-operator-call
    ((operator (eql 'representation)) form environment)
  (declare (ignore operator))
  (unless (= (length form) 2)
    (error 'arithmetic-language-error
           :form form :reason :representation-arity))
  (let ((operand (parse-arithmetic-expression (second form) environment)))
    (unless (arithmetic-expression-quantity-checked-p operand)
      (error 'arithmetic-language-error
             :form form :reason :representation-requires-quantity
             :details (arithmetic-expression-form operand)))
    (make-instance 'arithmetic-representation
                   :operand operand :source-form form)))

(defmethod parse-arithmetic-operator-call
    ((operator (eql 'convert-unit)) form environment)
  (declare (ignore operator))
  (destructuring-bind (name operand-form &key (unit nil unit-supplied-p)) form
    (declare (ignore name))
    (unless unit-supplied-p
      (error 'arithmetic-language-error
             :form form :reason :missing-target-unit))
    (let* ((operand (parse-arithmetic-expression operand-form environment))
           (source
             (and (arithmetic-expression-quantity-checked-p operand)
                  (arithmetic-expression-quantity-specification operand))))
      (unless source
        (error 'arithmetic-language-error
               :form form :reason :unit-conversion-requires-quantity))
      (with-arithmetic-quantity-errors (form :invalid-unit-conversion)
        (multiple-value-bind (target factor)
            (math:convert-quantity-specification-unit source unit)
          (make-instance 'arithmetic-unit-conversion
                         :operand operand
                         :factor factor
                         :quantity-specification target
                         :source-form form))))))

(defun parse-arithmetic-call (form environment)
  (let ((operator (first form)))
    (unless (arithmetic-operator-p operator)
      (error 'arithmetic-language-error
             :form form :reason :unknown-operator :details operator))
    (parse-arithmetic-operator-call operator form environment)))

(defun parse-arithmetic-expression (form environment)
  (cond ((realp form)
         (make-instance 'arithmetic-literal
                        :value form
                        :quantity-specification
                        (math:make-quantity-specification nil)
                        :source-form form))
        ((symbolp form)
         (make-arithmetic-reference form environment form))
        ((consp form)
         (parse-arithmetic-call form environment))
        (t
         (error 'arithmetic-language-error
                :form form :reason :unsupported-expression))))

(defun parse-arithmetic-parameter (form)
  (destructuring-bind (name &rest options) form
    (unless (symbolp name)
      (error 'arithmetic-language-error
             :form form :reason :invalid-parameter-name :details name))
    (make-instance
     'arithmetic-parameter
     :name name
     :quantity-specification
     (parse-source-quantity-specification options form)
     :source-form form)))

(defun parse-arithmetic-body (body environment)
  (unless (= (length body) 1)
    (error 'arithmetic-language-error
           :form body :reason :expected-single-arithmetic-body))
  (let ((form (first body)))
    (if (and (consp form) (eq (first form) 'let*))
        (destructuring-bind (operator raw-bindings &rest results) form
          (declare (ignore operator))
          (unless (= (length results) 1)
            (error 'arithmetic-language-error
                   :form form :reason :expected-single-result))
          (let ((bindings nil)
                (lexical-environment environment))
            (dolist (raw-binding raw-bindings)
              (unless (and (consp raw-binding) (= (length raw-binding) 2)
                           (symbolp (first raw-binding)))
                (error 'arithmetic-language-error
                       :form raw-binding :reason :invalid-binding))
              (let* ((name (first raw-binding))
                     (expression
                       (parse-arithmetic-expression
                        (second raw-binding) lexical-environment))
                     (binding
                       (make-instance 'arithmetic-binding
                                      :name name :expression expression
                                      :source-form raw-binding)))
                (setf (arithmetic-expression-name expression) name)
                (push binding bindings)
                (push (cons name binding) lexical-environment)))
            (values (nreverse bindings)
                    (parse-arithmetic-expression
                     (first results) lexical-environment))))
        (values nil (parse-arithmetic-expression form environment)))))

(defun parse-arithmetic-function-definition (name parameter-forms body)
  "Parse one backend-neutral arithmetic function definition."
  (let* ((parameters (mapcar #'parse-arithmetic-parameter parameter-forms))
         (environment
           (mapcar (lambda (parameter)
                     (cons (arithmetic-object-name parameter) parameter))
                   parameters)))
    (multiple-value-bind (bindings result)
        (parse-arithmetic-body body environment)
      (make-instance
       'arithmetic-function-definition
       :name name
       :parameters parameters
       :bindings bindings
       :result result
       :source-form
       (list* 'define-arithmetic-function name parameter-forms body)))))

(defgeneric arithmetic-function-definition-for (name)
  (:documentation "Return the live arithmetic definition named by NAME, or NIL."))

(defmethod arithmetic-function-definition-for (name)
  (declare (ignore name))
  nil)

(defmacro define-arithmetic-function (name parameters &body body)
  "Define one inspectable arithmetic function through an EQL method."
  (let ((function-name (gensym "FUNCTION-NAME")))
    `(defmethod arithmetic-function-definition-for
         ((,function-name (eql ',name)))
       (declare (ignore ,function-name))
       (load-time-value
        (parse-arithmetic-function-definition
         ',name ',parameters ',body)))))
