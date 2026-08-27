(in-package #:luv.tests)

(lang:define-arithmetic-function fog-shape
    ((view-distance :quantity :distance :unit :metre)
     (fog-near :quantity :distance :unit :metre)
     (fog-far :quantity :distance :unit :metre))
  (let* ((fog-span (- fog-far fog-near))
         (fog-progress
           (clamp (/ (- view-distance fog-near) fog-span)
                  (quantity 0.0 :unit :one)
                  (quantity 1.0 :unit :one))))
    (interpret (* fog-progress fog-progress)
               :quantity :proportion :unit :one)))

(lang:define-arithmetic-function square-offset ((value))
  (let* ((shifted (+ value 1.0)))
    (* shifted shifted)))

(lang:define-arithmetic-function composed-arithmetic ((value))
  (+ (square-offset value) 2.0))

(lang:define-arithmetic-function triangular-number ((count))
  (counted-fold (index count sum 0)
    (+ sum index)))

(lang:define-arithmetic-function bounded-triangular-number ((count) (limit))
  (counted-fold (index count sum 0)
    (if (< index limit) (+ sum index) sum)))

(lang:define-arithmetic-function folded-remainders ((count))
  (counted-fold (index count sum 0)
    (let* ((next (+ index 1))
           (remainder (mod next 3)))
      (+ sum remainder))))

(define-test arithmetic-functions-retain-checked-source-graphs
  (let* ((definition (lang:arithmetic-function-definition-for 'fog-shape))
         (bindings (lang:arithmetic-function-bindings definition))
         (result (lang:arithmetic-function-result definition))
         (specification
           (lang:arithmetic-expression-quantity-specification result)))
    (true (typep definition 'lang:arithmetic-function-definition))
    (true (= 3 (length (lang:arithmetic-function-parameters definition))))
    (true (equal '(fog-span fog-progress)
                 (mapcar #'lang:arithmetic-object-name bindings)))
    (true (eq :proportion
              (math:quantity-specification-name specification)))
    (true (math:unitless-p
           (math:quantity-specification-unit specification)))
    (true (equal '(interpret (* fog-progress fog-progress)
                             :quantity :proportion :unit :one)
                 (lang:arithmetic-expression-form result)))
    (true (> (length (lang:arithmetic-function-expressions definition))
             (length bindings)))
    (true (eq definition
              (lang:arithmetic-function-definition-for 'fog-shape)))))

(define-test reusable-functions-are-part-of-the-common-expression-graph
  (let* ((definition
           (lang:arithmetic-function-definition-for 'composed-arithmetic))
         (call
           (first (lang:arithmetic-call-operands
                   (lang:arithmetic-function-result definition)))))
    (true (typep call 'lang:arithmetic-function-call))
    (true (eq 'square-offset
              (lang:arithmetic-object-name
               (lang:arithmetic-function-call-definition call))))
    (true (= 1 (length (lang:arithmetic-function-call-arguments call))))
    (true (= 2 (length (lang:arithmetic-function-call-bindings call))))
    (true (typep (first (lang:arithmetic-function-call-bindings call))
                 'lang:arithmetic-function-parameter-binding))
    (true (equal '(square-offset value)
                 (lang:arithmetic-expression-form call)))))

(define-test common-functions-reject-bad-applications
  (fail
   (lang:parse-arithmetic-function-definition
    'bad-arity '((value)) '((square-offset value value)))
   'lang:arithmetic-language-error)
  (true (eq :arithmetic-function-arity
            (handler-case
                (progn
                  (lang:parse-arithmetic-function-definition
                   'bad-arity '((value)) '((square-offset value value)))
                  nil)
              (lang:arithmetic-language-error (condition)
                (lang:arithmetic-language-error-reason condition)))))
  (lang:define-arithmetic-function recursion-probe ((value))
    (+ value 1.0))
  (true (eq :recursive-arithmetic-function
            (handler-case
                (progn
                  (lang:parse-arithmetic-function-definition
                   'recursion-probe '((value)) '((recursion-probe value)))
                  nil)
              (lang:arithmetic-language-error (condition)
                (lang:arithmetic-language-error-reason condition))))))

(define-test counted-fold-is-shared-inspectable-control-flow
  (let ((fold
          (lang:arithmetic-function-result
           (lang:arithmetic-function-definition-for 'triangular-number))))
    (true (typep fold 'lang:arithmetic-counted-fold))
    (true (eq 'index
              (lang:arithmetic-object-name
               (lang:arithmetic-counted-fold-index-binding fold))))
    (true (eq 'sum
              (lang:arithmetic-object-name
               (lang:arithmetic-counted-fold-state-binding fold))))
    (true (equal '(counted-fold (index count sum 0) (+ sum index))
                 (lang:arithmetic-expression-form fold)))))

(define-test counted-fold-updates-own-shared-lexical-bindings
  (let* ((fold
           (lang:arithmetic-function-result
            (lang:arithmetic-function-definition-for 'folded-remainders)))
         (bindings (lang:arithmetic-counted-fold-bindings fold)))
    (true (equal '(next remainder)
                 (mapcar #'lang:arithmetic-object-name bindings)))
    (true (eq 'mod
              (lang:arithmetic-call-operator
               (lang:arithmetic-binding-expression (second bindings)))))))

(define-test conditionals-and-comparisons-are-shared-expression-nodes
  (let* ((fold
           (lang:arithmetic-function-result
            (lang:arithmetic-function-definition-for
             'bounded-triangular-number)))
         (conditional (lang:arithmetic-counted-fold-update fold)))
    (true (typep conditional 'lang:arithmetic-conditional))
    (true (equal '(if (< index limit) (+ sum index) sum)
                 (lang:arithmetic-expression-form conditional)))
    (true (typep (lang:arithmetic-conditional-condition conditional)
                 'lang:arithmetic-call))))

(define-test arithmetic-parameters-implement-the-common-declaration-protocol
  (let* ((definition (lang:arithmetic-function-definition-for 'fog-shape))
         (parameter (first (lang:arithmetic-function-parameters definition))))
    (true (null (math:declaration-representation-type parameter)))
    (true (eq :distance
              (math:quantity-specification-name
               (math:declaration-quantity-specification parameter))))
    (true (math:declaration-quantity-checked-p parameter))))

(define-test arithmetic-functions-reject-semantic-mistakes-before-a-backend
  (fail
   (lang:parse-arithmetic-function-definition
    'bad-addition
    '((distance :quantity :distance :unit :metre)
      (height :quantity :height :unit :metre))
    '((+ distance height)))
   'lang:arithmetic-language-error)
  (true (eq :different-quantity-spaces
            (handler-case
                (progn
                  (lang:parse-arithmetic-function-definition
                   'bad-addition
                   '((distance :quantity :distance :unit :metre)
                     (height :quantity :height :unit :metre))
                   '((+ distance height)))
                  nil)
              (lang:arithmetic-language-error (condition)
                (lang:arithmetic-language-error-details condition))))))

(define-test unit-conversion-is-an-inspectable-common-expression
  (let* ((definition
           (lang:parse-arithmetic-function-definition
            'kilometres-to-metres
            '((distance :quantity :distance :unit :kilometre))
            '((convert-unit distance :unit :metre))))
         (result (lang:arithmetic-function-result definition)))
    (true (typep result 'lang:arithmetic-unit-conversion))
    (true (= 1000 (lang:arithmetic-unit-conversion-factor result)))
    (true (math:unit-expression=
           :metre
           (math:quantity-specification-unit
            (lang:arithmetic-expression-quantity-specification result))))))

(define-test common-literals-preserve-source-representation
  (let* ((definition
           (lang:parse-arithmetic-function-definition
            'double-literal nil '(1.0d0)))
         (result (lang:arithmetic-function-result definition)))
    (true (typep result 'lang:arithmetic-literal))
    (true (typep (lang:arithmetic-literal-value result) 'double-float))
    (true (eql 1.0d0 (lang:arithmetic-expression-form result)))))
