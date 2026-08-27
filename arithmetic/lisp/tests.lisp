(in-package #:luv.tests)

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

(lisp:define-lisp-arithmetic-function absolute-square-root ((value))
  (sqrt (abs value)))

(lisp:define-lisp-arithmetic-function cpu-triangular-number ((count))
  (counted-fold (index count sum 0)
    (+ sum index)))

(lisp:define-lisp-arithmetic-function cpu-bounded-triangular-number
    ((count) (limit))
  (counted-fold (index count sum 0)
    (if (< index limit) (+ sum index) sum)))

(lisp:define-lisp-arithmetic-function cpu-capped-triangular-number
    ((count) (cap))
  (counted-fold (index count sum 0 :until (> sum cap))
    (+ sum index)))

(lisp:define-lisp-arithmetic-function cpu-folded-remainders ((count))
  (counted-fold (index count sum 0)
    (let* ((next (+ index 1))
           (remainder (mod next 3)))
      (+ sum remainder))))

(defun tree-contains-any-object-p (tree objects)
  (cond ((atom tree) (member tree objects :test #'eql))
        ((consp tree)
         (or (tree-contains-any-object-p (car tree) objects)
             (tree-contains-any-object-p (cdr tree) objects)))))

(define-test checked-definitions-compile-to-ordinary-lisp-functions
  (let* ((lambda-expression
           (lisp:lower-arithmetic-function 'cpu-fog-shape))
         (function (lisp:compile-arithmetic-function 'cpu-fog-shape))
         (result (cpu-fog-shape 5.0d0 0.0d0 10.0d0)))
    (true (compiled-function-p #'cpu-fog-shape))
    (true (compiled-function-p function))
    (true (typep result 'double-float))
    (true (< (abs (- result 0.25d0)) 1.0d-12))
    (true (not
           (tree-contains-any-object-p
            lambda-expression
            (list 'lang:quantity 'lang:interpret 'lang:convert-unit))))))

(define-test shared-function-calls-lower-and-execute-as-ordinary-lisp
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'twice-offset-square)))
    (true (= 18.0d0 (twice-offset-square 2.0d0)))
    (true (= 18.0d0
             (funcall (lisp:compile-arithmetic-function
                       'twice-offset-square)
                      2.0d0)))
    (true (tree-contains-any-object-p lambda-expression '(let*)))
    (true (not (tree-contains-any-object-p
                lambda-expression '(offset-square))))))

(define-test counted-fold-lowers-to-an-ordinary-lisp-loop
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'cpu-triangular-number)))
    (true (= 10 (cpu-triangular-number 5)))
    (true (= 45 (funcall (lisp:compile-arithmetic-function
                          'cpu-triangular-number)
                         10)))
    (true (tree-contains-any-object-p lambda-expression '(dotimes)))))

(define-test counted-fold-until-leaves-the-ordinary-loop-early
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'cpu-capped-triangular-number)))
    ;; 0+1+2+3+4 = 10 passes the cap of 8 after index 4; index 5 is never
    ;; added, so the fold stops with 10 rather than running on to 45.
    (true (= 10 (cpu-capped-triangular-number 10 8)))
    (true (= 45 (cpu-capped-triangular-number 10 100)))
    (true (tree-contains-any-object-p lambda-expression '(return)))))

(define-test counted-fold-lexicals-lower-inside-the-ordinary-loop
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'cpu-folded-remainders)))
    (true (= 6 (cpu-folded-remainders 5)))
    (true (tree-contains-any-object-p lambda-expression '(let*)))
    (true (tree-contains-any-object-p lambda-expression '(mod)))))

(define-test shared-conditionals-retain-ordinary-lisp-branching
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'cpu-bounded-triangular-number)))
    (true (= 10 (cpu-bounded-triangular-number 10 5)))
    (true (tree-contains-any-object-p lambda-expression '(if)))
    (true (tree-contains-any-object-p lambda-expression '(<)))))

(define-test ordinary-vectors-execute-without-semantic-wrappers
  (let ((add (lisp:compile-arithmetic-function 'add-vectors))
        (dot (lisp:compile-arithmetic-function 'vector-inner-product))
        (left #(1.0d0 2.0d0 3.0d0))
        (right #(4.0d0 5.0d0 6.0d0)))
    (true (equalp #(5.0d0 7.0d0 9.0d0)
                  (funcall add left right)))
    (true (= 32.0d0 (funcall dot left right)))))

(define-test raw-unary-operators-execute-componentwise
  (true (= 2.0d0 (absolute-square-root -4.0d0)))
  (true (equalp #(2.0d0 3.0d0)
                (absolute-square-root #(-4.0d0 9.0d0)))))

(define-test vec3-is-owned-and-realized-by-the-lisp-arithmetic-backend
  (let* ((vector (vec:make-vec3 3d0 4d0 0d0))
         (add (lisp:compile-arithmetic-function 'add-vectors))
         (dot (lisp:compile-arithmetic-function 'vector-inner-product)))
    (true (= (vec:vec3-length vector) 5d0))
    (true (= 6d0
             (vec:vec3-dot vector (vec:make-vec3 2d0 0d0 1d0))))
    (true (equalp (vec:make-vec3 6d0 8d0 0d0)
                  (vec:vec3-scale vector 2d0)))
    (true (equalp (vec:make-vec3 0d0 0d0 1d0)
                  (vec:vec3-cross (vec:make-vec3 1d0 0d0 0d0)
                                  (vec:make-vec3 0d0 1d0 0d0))))
    (let ((normalized (vec:vec3-normalize vector)))
      (true (< (abs (- (vec:vec3-x normalized) 0.6d0)) 1d-12))
      (true (< (abs (- (vec:vec3-y normalized) 0.8d0)) 1d-12))
      (true (zerop (vec:vec3-z normalized))))
    (setf (vec:vec3-component vector :z) 5d0)
    (true (equal '(3d0 4d0 5d0) (vec:vec3-list vector)))
    (true (equalp (vec:make-vec3 4d0 6d0 8d0)
                  (funcall add vector (vec:make-vec3 1d0 2d0 3d0))))
    (true (= 14d0
             (funcall dot (vec:make-vec3 1d0 2d0 3d0)
                          (vec:make-vec3 1d0 2d0 3d0))))))

(define-test lisp-realizations-bind-storage-contracts-once
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
    (true (compiled-function-p function))
    (true (equalp #(4d0 6d0)
                  (funcall function #(1d0 2d0) #(3d0 4d0))))
    (fail
     (lisp:bind-lisp-arithmetic-realization
      realization
      (list
       (math:make-represented-value-declaration
        :representation-type 'real
        :quantity-specification
        (math:declaration-quantity-specification (first expected))
        :source-form '(bad-scalar-slot))
       (second expected)))
     'math:declaration-compatibility-error)))

(define-test affine-arithmetic-is-checked-once-and-executes-numerically
  (let* ((definition
           (lang:arithmetic-function-definition-for 'move-distance-point))
         (result (lang:arithmetic-function-result definition))
         (specification
           (lang:arithmetic-expression-quantity-specification result))
         (function (lisp:compile-arithmetic-function definition)))
    (true (math:quantity-specification-affine-p specification))
    (true (= 12.5d0 (funcall function 10.0d0 2.5d0))))
  (fail
   (lang:parse-arithmetic-function-definition
    'add-points
    '((left :quantity :distance :unit :metre :affine-p t)
      (right :quantity :distance :unit :metre :affine-p t))
    '((+ left right)))
   'lang:arithmetic-language-error))

(define-test explicit-unit-conversion-becomes-an-exact-numeric-scale
  (let* ((lambda-expression
           (lisp:lower-arithmetic-function 'kilometres-to-metres))
         (function
           (lisp:compile-arithmetic-function 'kilometres-to-metres)))
    (true (= 1500.0d0 (funcall function 1.5d0)))
    (true (tree-contains-any-object-p lambda-expression '(1000)))
    (true (not (tree-contains-any-object-p
                lambda-expression (list 'lang:convert-unit))))))

(define-test dimensional-errors-prevent-lisp-compilation
  (fail
   (lisp:compile-arithmetic-function
    (lang:parse-arithmetic-function-definition
     'bad-addition
     '((distance :quantity :distance :unit :metre)
       (height :quantity :height :unit :metre))
     '((+ distance height))))
   'lang:arithmetic-language-error))
