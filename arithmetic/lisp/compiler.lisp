;;; Common Lisp realization of backend-neutral compiled arithmetic.
;;;
;;; Semantic checking has already happened in LUV.ARITHMETIC.LANGUAGE.  This
;;; file erases quantity boundaries into ordinary numerical code and compiles
;;; the resulting lambda.  Runtime values are numbers and vectors, never
;;; quantity wrappers.

(in-package #:luv.arithmetic.lisp)

(define-condition lisp-arithmetic-error (error)
  ((definition
    :initarg :definition
    :initform nil
    :reader lisp-arithmetic-error-definition)
   (expression
    :initarg :expression
    :initform nil
    :reader lisp-arithmetic-error-expression)
   (reason
    :initarg :reason
    :reader lisp-arithmetic-error-reason)
   (details
    :initarg :details
    :initform nil
    :reader lisp-arithmetic-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Cannot realize arithmetic~@[ function ~S~]~@[ expression ~S~] in Lisp: ~A~@[ (~S)~]."
             (let ((definition
                     (lisp-arithmetic-error-definition condition)))
               (and definition (lang:arithmetic-object-name definition)))
             (let ((expression
                     (lisp-arithmetic-error-expression condition)))
               (and expression (lang:arithmetic-expression-form expression)))
             (lisp-arithmetic-error-reason condition)
             (lisp-arithmetic-error-details condition)))))

(defun lisp-vector-length (left right)
  (unless (= (length left) (length right))
    (error 'lisp-arithmetic-error
           :reason :different-vector-lengths
           :details (list (length left) (length right))))
  (length left))

(defun map-lisp-vector (function vector)
  (let* ((length (length vector))
         (result (make-array length)))
    (dotimes (index length result)
      (setf (aref result index)
            (funcall function (aref vector index))))))

(defun map-lisp-vectors (function left right)
  (let* ((length (lisp-vector-length left right))
         (result (make-array length)))
    (dotimes (index length result)
      (setf (aref result index)
            (funcall function
                     (aref left index)
                     (aref right index))))))

(defun lisp-binary-operation (function left right)
  (cond ((and (numberp left) (numberp right))
         (funcall function left right))
        ((and (vectorp left) (vectorp right))
         (map-lisp-vectors function left right))
        ((and (vectorp left) (numberp right))
         (map-lisp-vector
          (lambda (component)
            (funcall function component right))
          left))
        ((and (numberp left) (vectorp right))
         (map-lisp-vector
          (lambda (component)
            (funcall function left component))
          right))
        (t
         (error 'lisp-arithmetic-error
                :reason :unsupported-runtime-representations
                :details (list (type-of left) (type-of right))))))

(defun reduce-lisp-operation (function operands identity)
  (if operands
      (reduce (lambda (left right)
                (lisp-binary-operation function left right))
              (rest operands) :initial-value (first operands))
      identity))

(defun lisp-add (&rest operands)
  (reduce-lisp-operation #'+ operands 0))

(defun lisp-subtract (&rest operands)
  (unless operands
    (error 'lisp-arithmetic-error :reason :subtraction-requires-operands))
  (if (rest operands)
      (reduce-lisp-operation #'- operands nil)
      (lisp-binary-operation #'- 0 (first operands))))

(defun lisp-multiply (&rest operands)
  (reduce-lisp-operation #'* operands 1))

(defun lisp-divide (&rest operands)
  (unless operands
    (error 'lisp-arithmetic-error :reason :division-requires-operands))
  (if (rest operands)
      (reduce-lisp-operation #'/ operands nil)
      (lisp-binary-operation #'/ 1 (first operands))))

(defun lisp-min (&rest operands)
  (unless operands
    (error 'lisp-arithmetic-error :reason :minimum-requires-operands))
  (reduce-lisp-operation #'min operands nil))

(defun lisp-max (&rest operands)
  (unless operands
    (error 'lisp-arithmetic-error :reason :maximum-requires-operands))
  (reduce-lisp-operation #'max operands nil))

(defun lisp-expt (base exponent)
  (lisp-binary-operation #'expt base exponent))

(defun lisp-dot (left right)
  (unless (and (vectorp left) (vectorp right))
    (error 'lisp-arithmetic-error
           :reason :dot-requires-vectors
           :details (list (type-of left) (type-of right))))
  (let ((length (lisp-vector-length left right))
        (result 0))
    (dotimes (index length result)
      (incf result (* (aref left index) (aref right index))))))

(defun lisp-clamp (value lower upper)
  (lisp-binary-operation
   #'min (lisp-binary-operation #'max value lower) upper))

(defun lisp-mix (from to amount)
  (lisp-add from
            (lisp-multiply
             (lisp-subtract to from)
             amount)))

(defun zero-like-number (number)
  (if (floatp number) (float 0 number) 0))

(defun one-like-number (number)
  (if (floatp number) (float 1 number) 1))

(defun lisp-step (edge value)
  (lisp-binary-operation
   (lambda (edge-component value-component)
     (if (< value-component edge-component)
         (zero-like-number value-component)
         (one-like-number value-component)))
   edge value))

(defun lisp-smoothstep (lower upper value)
  (let ((progress
          (lisp-clamp
           (lisp-divide
            (lisp-subtract value lower)
            (lisp-subtract upper lower))
           0 1)))
    (lisp-multiply progress progress
                   (lisp-subtract 3 (lisp-multiply 2 progress)))))

(defun lisp-normalize (vector)
  (unless (vectorp vector)
    (error 'lisp-arithmetic-error
           :reason :normalize-requires-vector
           :details (type-of vector)))
  (let ((magnitude (sqrt (lisp-dot vector vector))))
    (lisp-divide vector magnitude)))

(defgeneric lisp-arithmetic-operator-function (operator)
  (:documentation
   "Return the ordinary Lisp function name implementing OPERATOR."))

(defmethod lisp-arithmetic-operator-function (operator)
  (error 'lisp-arithmetic-error
         :reason :unsupported-operator :details operator))

(defmacro define-lisp-arithmetic-operator (operator function)
  `(defmethod lisp-arithmetic-operator-function
       ((operator (eql ',operator)))
     (declare (ignore operator))
     ',function))

(define-lisp-arithmetic-operator + lisp-add)
(define-lisp-arithmetic-operator - lisp-subtract)
(define-lisp-arithmetic-operator * lisp-multiply)
(define-lisp-arithmetic-operator / lisp-divide)
(define-lisp-arithmetic-operator dot lisp-dot)
(define-lisp-arithmetic-operator min lisp-min)
(define-lisp-arithmetic-operator max lisp-max)
(define-lisp-arithmetic-operator clamp lisp-clamp)
(define-lisp-arithmetic-operator mix lisp-mix)
(define-lisp-arithmetic-operator smoothstep lisp-smoothstep)
(define-lisp-arithmetic-operator step lisp-step)
(define-lisp-arithmetic-operator normalize lisp-normalize)
(define-lisp-arithmetic-operator expt lisp-expt)

(defun lisp-environment-value (target environment expression)
  (or (cdr (assoc target environment :test #'eq))
      (error 'lisp-arithmetic-error
             :expression expression
             :reason :unbound-expression-target
             :details (lang:arithmetic-reference-target-name target))))

(defgeneric lower-lisp-arithmetic-expression (expression environment)
  (:documentation
   "Lower checked EXPRESSION to ordinary Lisp using target-to-name ENVIRONMENT."))

(defmethod lower-lisp-arithmetic-expression
    ((expression lang:arithmetic-literal) environment)
  (declare (ignore environment))
  (lang:arithmetic-literal-value expression))

(defmethod lower-lisp-arithmetic-expression
    ((expression lang:arithmetic-reference) environment)
  (lisp-environment-value
   (lang:arithmetic-reference-target expression) environment expression))

(defmethod lower-lisp-arithmetic-expression
    ((expression lang:arithmetic-call) environment)
  (when (lang:arithmetic-call-parameters expression)
    (error 'lisp-arithmetic-error
           :expression expression
           :reason :unsupported-call-parameters
           :details (lang:arithmetic-call-parameters expression)))
  (cons
   (lisp-arithmetic-operator-function
    (lang:arithmetic-call-operator expression))
   (mapcar (lambda (operand)
             (lower-lisp-arithmetic-expression operand environment))
           (lang:arithmetic-call-operands expression))))

(defmethod lower-lisp-arithmetic-expression
    ((expression lang:arithmetic-quantity-boundary) environment)
  ;; Construction, assumption, interpretation, and representation are all
  ;; compile-time semantic boundaries.  Their runtime representation is the
  ;; operand itself.
  (lower-lisp-arithmetic-expression
   (lang:arithmetic-quantity-boundary-operand expression) environment))

(defmethod lower-lisp-arithmetic-expression
    ((expression lang:arithmetic-unit-conversion) environment)
  (let ((operand
          (lower-lisp-arithmetic-expression
           (lang:arithmetic-unit-conversion-operand expression) environment))
        (factor (lang:arithmetic-unit-conversion-factor expression)))
    (if (= factor 1)
        operand
        (list 'lisp-multiply factor operand))))

(defun arithmetic-definition-for-lisp (name)
  (or (lang:arithmetic-function-definition-for name)
      (error 'lisp-arithmetic-error
             :reason :undefined-arithmetic-function :details name)))

(defgeneric lower-arithmetic-function (definition)
  (:documentation
   "Lower a checked arithmetic definition or its symbolic name to a lambda."))

(defmethod lower-arithmetic-function ((name symbol))
  (lower-arithmetic-function (arithmetic-definition-for-lisp name)))

(defmethod lower-arithmetic-function
    ((definition lang:arithmetic-function-definition))
  (let ((environment nil)
        (lambda-parameters nil)
        (lambda-bindings nil))
    (dolist (parameter (lang:arithmetic-function-parameters definition))
      (let ((name (gensym
                   (format nil "~A-"
                           (lang:arithmetic-object-name parameter)))))
        (push name lambda-parameters)
        (push (cons parameter name) environment)))
    (setf lambda-parameters (nreverse lambda-parameters))
    (dolist (binding (lang:arithmetic-function-bindings definition))
      (let* ((value
               (lower-lisp-arithmetic-expression
                (lang:arithmetic-binding-expression binding) environment))
             (name
               (gensym
                (format nil "~A-" (lang:arithmetic-object-name binding)))))
        (push (list name value) lambda-bindings)
        (push (cons binding name) environment)))
    (let ((result
            (lower-lisp-arithmetic-expression
             (lang:arithmetic-function-result definition) environment)))
      `(lambda ,lambda-parameters
         ,(if lambda-bindings
              `(let* ,(nreverse lambda-bindings) ,result)
              result)))))

(defmacro define-lisp-arithmetic-function (name parameters &body body)
  "Define checked arithmetic NAME as both semantic source and a Lisp function."
  (let* ((definition
           (lang:parse-arithmetic-function-definition name parameters body))
         (lambda-expression (lower-arithmetic-function definition)))
    `(progn
       (lang:define-arithmetic-function ,name ,parameters ,@body)
       (defun ,name ,(second lambda-expression)
         ,@(cddr lambda-expression))
       ',name)))

(defgeneric compile-arithmetic-function (definition)
  (:documentation
   "Compile a checked arithmetic definition or symbolic name to a function."))

(defmethod compile-arithmetic-function ((name symbol))
  (compile-arithmetic-function (arithmetic-definition-for-lisp name)))

(defmethod compile-arithmetic-function
    ((definition lang:arithmetic-function-definition))
  (compile nil (lower-arithmetic-function definition)))
