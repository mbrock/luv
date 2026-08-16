(defpackage #:luv/arithmetic/lisp/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:lang #:luv.arithmetic.language)
                    (#:lisp #:luv.arithmetic.lisp)
                    (#:math #:luv.arithmetic))
  (:import-from #:luv.arithmetic #:dot #:clamp)
  (:import-from #:luv.arithmetic.language
                #:quantity #:interpret #:convert-unit #:counted-fold))

(in-package #:luv/arithmetic/lisp/tests)

(lisp:define-lisp-arithmetic-function cpu-fog-shape
    ((view-distance :quantity :distance :unit :metre)
     (fog-near :quantity :distance :unit :metre)
     (fog-far :quantity :distance :unit :metre))
  (let* ((fog-span (- fog-far fog-near))
         (fog-progress
           (clamp (/ (- view-distance fog-near) fog-span)
                  (quantity 0.0d0 :unit :one)
                  (quantity 1.0d0 :unit :one))))
    (interpret (* fog-progress fog-progress)
               :quantity :proportion :unit :one)))

(lang:define-arithmetic-function add-vectors
    ((left :unit :one :tensor-order 1)
     (right :unit :one :tensor-order 1))
  (+ left right))

(lang:define-arithmetic-function vector-inner-product
    ((left :unit :one :tensor-order 1)
     (right :unit :one :tensor-order 1))
  (dot left right))

(lang:define-arithmetic-function move-distance-point
    ((point :quantity :distance :unit :metre :affine-p t)
     (offset :quantity :distance :unit :metre))
  (+ point offset))

(lang:define-arithmetic-function kilometres-to-metres
    ((distance :quantity :distance :unit :kilometre))
  (convert-unit distance :unit :metre))

(lisp:define-lisp-arithmetic-function offset-square ((value))
  (let* ((shifted (+ value 1.0d0)))
    (* shifted shifted)))

(lisp:define-lisp-arithmetic-function twice-offset-square ((value))
  (+ (offset-square value) (offset-square value)))

(lisp:define-lisp-arithmetic-function cpu-triangular-number ((count))
  (counted-fold (index count sum 0)
    (+ sum index)))

(defun tree-contains-any-object-p (tree objects)
  (cond ((atom tree) (member tree objects :test #'eql))
        ((consp tree)
         (or (tree-contains-any-object-p (car tree) objects)
             (tree-contains-any-object-p (cdr tree) objects)))))

(deftest checked-definitions-compile-to-ordinary-lisp-functions
  (let* ((lambda-expression
           (lisp:lower-arithmetic-function 'cpu-fog-shape))
         (function (lisp:compile-arithmetic-function 'cpu-fog-shape))
         (result (cpu-fog-shape 5.0d0 0.0d0 10.0d0)))
    (ok (compiled-function-p #'cpu-fog-shape))
    (ok (compiled-function-p function))
    (ok (typep result 'double-float))
    (ok (< (abs (- result 0.25d0)) 1.0d-12))
    (ok (not
         (tree-contains-any-object-p
          lambda-expression
          (list 'lang:quantity 'lang:interpret 'lang:convert-unit))))))

(deftest shared-function-calls-lower-and-execute-as-ordinary-lisp
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'twice-offset-square)))
    (ok (= 18.0d0 (twice-offset-square 2.0d0)))
    (ok (= 18.0d0
           (funcall (lisp:compile-arithmetic-function
                     'twice-offset-square)
                    2.0d0)))
    (ok (tree-contains-any-object-p lambda-expression '(let*)))
    (ok (not (tree-contains-any-object-p
              lambda-expression '(offset-square))))))

(deftest counted-fold-lowers-to-an-ordinary-lisp-loop
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'cpu-triangular-number)))
    (ok (= 10 (cpu-triangular-number 5)))
    (ok (= 45 (funcall (lisp:compile-arithmetic-function
                        'cpu-triangular-number)
                       10)))
    (ok (tree-contains-any-object-p lambda-expression '(dotimes)))))

(deftest ordinary-vectors-execute-without-semantic-wrappers
  (let ((add (lisp:compile-arithmetic-function 'add-vectors))
        (dot (lisp:compile-arithmetic-function 'vector-inner-product))
        (left #(1.0d0 2.0d0 3.0d0))
        (right #(4.0d0 5.0d0 6.0d0)))
    (ok (equalp #(5.0d0 7.0d0 9.0d0)
                (funcall add left right)))
    (ok (= 32.0d0 (funcall dot left right)))))

(deftest lisp-realizations-bind-storage-contracts-once
  (let* ((realization
           (lisp:make-lisp-arithmetic-realization
            'add-vectors
            :parameter-representation-types '(vector vector)
            :result-representation-type 'vector))
         (expected
           (lisp:lisp-arithmetic-realization-parameter-declarations
            realization))
         (function
           (lisp:bind-lisp-arithmetic-realization realization expected)))
    (ok (compiled-function-p function))
    (ok (equalp #(4d0 6d0)
                (funcall function #(1d0 2d0) #(3d0 4d0))))
    (ok (signals
         (lisp:bind-lisp-arithmetic-realization
          realization
          (list
           (math:make-represented-value-declaration
            :representation-type 'real
            :quantity-specification
            (math:declaration-quantity-specification (first expected))
            :source-form '(bad-scalar-slot))
           (second expected)))
         'math:declaration-compatibility-error))))

(deftest affine-arithmetic-is-checked-once-and-executes-numerically
  (let* ((definition
           (lang:arithmetic-function-definition-for 'move-distance-point))
         (result (lang:arithmetic-function-result definition))
         (specification
           (lang:arithmetic-expression-quantity-specification result))
         (function (lisp:compile-arithmetic-function definition)))
    (ok (math:quantity-specification-affine-p specification))
    (ok (= 12.5d0 (funcall function 10.0d0 2.5d0))))
  (ok (signals
       (lang:parse-arithmetic-function-definition
        'add-points
        '((left :quantity :distance :unit :metre :affine-p t)
          (right :quantity :distance :unit :metre :affine-p t))
        '((+ left right)))
       'lang:arithmetic-language-error)))

(deftest explicit-unit-conversion-becomes-an-exact-numeric-scale
  (let* ((lambda-expression
           (lisp:lower-arithmetic-function 'kilometres-to-metres))
         (function
           (lisp:compile-arithmetic-function 'kilometres-to-metres)))
    (ok (= 1500.0d0 (funcall function 1.5d0)))
    (ok (tree-contains-any-object-p lambda-expression '(1000)))
    (ok (not (tree-contains-any-object-p
              lambda-expression (list 'lang:convert-unit))))))

(deftest dimensional-errors-prevent-lisp-compilation
  (ok (signals
       (lisp:compile-arithmetic-function
        (lang:parse-arithmetic-function-definition
         'bad-addition
         '((distance :quantity :distance :unit :metre)
           (height :quantity :height :unit :metre))
         '((+ distance height))))
       'lang:arithmetic-language-error)))
