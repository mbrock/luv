(defpackage #:luv/arithmetic/language/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:lang #:luv.arithmetic.language)
                    (#:math #:luv.arithmetic))
  (:import-from #:luv.arithmetic #:clamp)
  (:import-from #:luv.arithmetic.language
                #:quantity #:interpret #:convert-unit))

(in-package #:luv/arithmetic/language/tests)

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

(deftest arithmetic-functions-retain-checked-source-graphs
  (let* ((definition (lang:arithmetic-function-definition-for 'fog-shape))
         (bindings (lang:arithmetic-function-bindings definition))
         (result (lang:arithmetic-function-result definition))
         (specification
           (lang:arithmetic-expression-quantity-specification result)))
    (ok (typep definition 'lang:arithmetic-function-definition))
    (ok (= 3 (length (lang:arithmetic-function-parameters definition))))
    (ok (equal '(fog-span fog-progress)
               (mapcar #'lang:arithmetic-object-name bindings)))
    (ok (eq :proportion
            (math:quantity-specification-name specification)))
    (ok (math:unitless-p
         (math:quantity-specification-unit specification)))
    (ok (equal '(interpret (* fog-progress fog-progress)
                           :quantity :proportion :unit :one)
               (lang:arithmetic-expression-form result)))
    (ok (> (length (lang:arithmetic-function-expressions definition))
           (length bindings)))
    (ok (eq definition
            (lang:arithmetic-function-definition-for 'fog-shape)))))

(deftest arithmetic-functions-reject-semantic-mistakes-before-a-backend
  (ok (signals
       (lang:parse-arithmetic-function-definition
        'bad-addition
        '((distance :quantity :distance :unit :metre)
          (height :quantity :height :unit :metre))
        '((+ distance height)))
       'lang:arithmetic-language-error))
  (ok (eq :different-quantity-spaces
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

(deftest unit-conversion-is-an-inspectable-common-expression
  (let* ((definition
           (lang:parse-arithmetic-function-definition
            'kilometres-to-metres
            '((distance :quantity :distance :unit :kilometre))
            '((convert-unit distance :unit :metre))))
         (result (lang:arithmetic-function-result definition)))
    (ok (typep result 'lang:arithmetic-unit-conversion))
    (ok (= 1000 (lang:arithmetic-unit-conversion-factor result)))
    (ok (math:unit-expression=
         :metre
         (math:quantity-specification-unit
          (lang:arithmetic-expression-quantity-specification result))))))

(deftest common-literals-preserve-source-representation
  (let* ((definition
           (lang:parse-arithmetic-function-definition
            'double-literal nil '(1.0d0)))
         (result (lang:arithmetic-function-result definition)))
    (ok (typep result 'lang:arithmetic-literal))
    (ok (typep (lang:arithmetic-literal-value result) 'double-float))
    (ok (eql 1.0d0 (lang:arithmetic-expression-form result)))))
