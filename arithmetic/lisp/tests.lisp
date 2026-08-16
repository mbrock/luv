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

(lisp:define-lisp-arithmetic-function cpu-triangular-number ((count))
  (counted-fold (index count sum 0)
    (+ sum index)))

(lisp:define-lisp-arithmetic-function cpu-bounded-triangular-number
    ((count) (limit))
  (counted-fold (index count sum 0)
    (if (< index limit) (+ sum index) sum)))

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

(deftest counted-fold-lexicals-lower-inside-the-ordinary-loop
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'cpu-folded-remainders)))
    (ok (= 6 (cpu-folded-remainders 5)))
    (ok (tree-contains-any-object-p lambda-expression '(let*)))
    (ok (tree-contains-any-object-p lambda-expression '(mod)))))

(deftest shared-conditionals-retain-ordinary-lisp-branching
  (let ((lambda-expression
          (lisp:lower-arithmetic-function 'cpu-bounded-triangular-number)))
    (ok (= 10 (cpu-bounded-triangular-number 10 5)))
    (ok (tree-contains-any-object-p lambda-expression '(if)))
    (ok (tree-contains-any-object-p lambda-expression '(<)))))

(deftest ordinary-vectors-execute-without-semantic-wrappers
  (let ((add (lisp:compile-arithmetic-function 'add-vectors))
        (dot (lisp:compile-arithmetic-function 'vector-inner-product))
        (left #(1.0d0 2.0d0 3.0d0))
        (right #(4.0d0 5.0d0 6.0d0)))
    (ok (equalp #(5.0d0 7.0d0 9.0d0)
                (funcall add left right)))
    (ok (= 32.0d0 (funcall dot left right)))))

(deftest vec3-is-owned-and-realized-by-the-lisp-arithmetic-backend
  (let* ((vector (vec:make-vec3 3d0 4d0 0d0))
         (add (lisp:compile-arithmetic-function 'add-vectors))
         (dot (lisp:compile-arithmetic-function 'vector-inner-product)))
    (ok (= (vec:vec3-length vector) 5d0))
    (ok (= 6d0
           (vec:vec3-dot vector (vec:make-vec3 2d0 0d0 1d0))))
    (ok (equalp (vec:make-vec3 6d0 8d0 0d0)
                (vec:vec3-scale vector 2d0)))
    (ok (equalp (vec:make-vec3 0d0 0d0 1d0)
                (vec:vec3-cross (vec:make-vec3 1d0 0d0 0d0)
                                (vec:make-vec3 0d0 1d0 0d0))))
    (let ((normalized (vec:vec3-normalize vector)))
      (ok (< (abs (- (vec:vec3-x normalized) 0.6d0)) 1d-12))
      (ok (< (abs (- (vec:vec3-y normalized) 0.8d0)) 1d-12))
      (ok (zerop (vec:vec3-z normalized))))
    (setf (vec:vec3-component vector :z) 5d0)
    (ok (equal '(3d0 4d0 5d0) (vec:vec3-list vector)))
    (ok (equalp (vec:make-vec3 4d0 6d0 8d0)
                (funcall add vector (vec:make-vec3 1d0 2d0 3d0))))
    (ok (= 14d0
           (funcall dot (vec:make-vec3 1d0 2d0 3d0)
                        (vec:make-vec3 1d0 2d0 3d0))))
    (ok (null (find-package "LUVCRAFT.WORLD")))))

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
