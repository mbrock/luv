(defpackage #:luv/arithmetic/tests
  (:use #:cl #:rove)
  (:local-nicknames (#:math #:luv.arithmetic)))

(in-package #:luv/arithmetic/tests)

(deftest dimensions-form-a-canonical-symbolic-product
  (let* ((length (math:make-dimension :length))
         (duration (math:make-dimension :duration))
         (velocity (math:divide-dimensions length duration))
         (acceleration (math:divide-dimensions velocity duration)))
    (ok (math:dimension=
         velocity (math:make-dimension '((:length 1) (:duration -1)))))
    (ok (math:dimension=
         acceleration
         (math:make-dimension '((:duration -2) (:length 1)))))
    (ok (math:dimensionless-p
         (math:divide-dimensions length length)))
    (ok (math:dimension=
         (math:exponentiate-dimension acceleration 1/2)
         (math:make-dimension '((:length 1/2) (:duration -1)))))))

(deftest named-quantity-spaces-govern-addition
  (let ((airspeed
          (math:make-quantity-specification
           :airspeed :dimension '((:length 1) (:duration -1))))
        (climb-rate
          (math:make-quantity-specification
           :climb-rate :dimension '((:length 1) (:duration -1)))))
    (ok (signals (math:derive-quantity-specification '+ airspeed climb-rate)
                 'math:quantity-operation-error))
    (ok (eq :different-quantity-spaces
            (handler-case
                (progn
                  (math:derive-quantity-specification '+ airspeed climb-rate)
                  nil)
              (math:quantity-operation-error (condition)
                (math:quantity-operation-error-reason condition)))))))

(deftest affine-points-and-differences-have-asymmetric-arithmetic
  (let* ((point
           (math:make-quantity-specification
            :shadow-depth :affine-p t))
         (difference
           (math:make-quantity-specification :shadow-depth))
         (moved (math:derive-quantity-specification '- point difference))
         (separation (math:derive-quantity-specification '- point point)))
    (ok (math:quantity-specification-affine-p moved))
    (ok (not (math:quantity-specification-affine-p separation)))
    (ok (signals (math:derive-quantity-specification '+ point point)
                 'math:quantity-operation-error))
    (ok (signals (math:derive-quantity-specification '- difference point)
                 'math:quantity-operation-error))))

(deftest dimensionless-scalar-scaling-preserves-semantic-identity
  (let* ((bias (math:make-quantity-specification :shadow-depth))
         (number (math:make-quantity-specification nil))
         (scaled (math:derive-quantity-specification '* number bias)))
    (ok (math:quantity-specification= scaled bias))))

(deftest exact-units-compose-without-implicit-conversion
  (let* ((metres
           (math:make-quantity-specification
            :distance :dimension :length :unit :metre))
         (kilometres
           (math:make-quantity-specification
            :distance :dimension :length :unit :kilometre))
         (seconds
           (math:make-quantity-specification
            :duration :dimension :duration :unit :second))
         (speed (math:derive-quantity-specification '/ metres seconds)))
    (ok (signals (math:derive-quantity-specification '+ metres kilometres)
                 'math:quantity-operation-error))
    (ok (eq :different-units
            (handler-case
                (progn
                  (math:derive-quantity-specification '+ metres kilometres)
                  nil)
              (math:quantity-operation-error (condition)
                (math:quantity-operation-error-reason condition)))))
    (ok (math:unit-expression=
         (math:quantity-specification-unit speed)
         '((:metre 1) (:second -1))))))

(deftest extrema-require-exactly-compatible-quantities
  (let ((left
          (math:make-quantity-specification
           :distance :dimension :length :unit :metre))
        (right
          (math:make-quantity-specification
           :distance :dimension :length :unit :metre))
        (other-unit
          (math:make-quantity-specification
           :distance :dimension :length :unit :kilometre)))
    (ok (math:quantity-specification=
         left (math:derive-quantity-specification 'max left right)))
    (ok (signals
         (math:derive-quantity-specification 'max left other-unit)
         'math:quantity-operation-error))))

(math:define-quantity-components :position (:position-x :position-y))

(deftest semantic-layouts-distinguish-vectors-from-packed-tuples
  (let* ((uv
           (math:make-quantity-specification
            :texture-uv :tensor-order 1 :affine-p t))
         (occlusion
           (math:make-quantity-specification :ambient-occlusion))
         (layout
           (math:make-quantity-layout
            3
            (list (math:make-quantity-projection '(0 1) uv)
                  (math:make-quantity-projection '(2) occlusion))))
         (reordered-layout
           (math:make-quantity-layout
            3
            (list (math:make-quantity-projection '(2) occlusion)
                  (math:make-quantity-projection '(0 1) uv))))
         (position
           (math:make-quantity-specification
            :position :dimension :length :unit :metre
            :tensor-order 1 :affine-p t))
         (position-x
           (math:project-quantity-specification position '(0) 2)))
    (ok (eq uv (math:project-quantity-layout layout '(0 1))))
    (ok (eq occlusion (math:project-quantity-layout layout '(2))))
    (ok (math:quantity-layout= layout reordered-layout))
    (ok (null (math:project-quantity-layout layout '(1 2))))
    (ok (eq :position-x
            (math:quantity-specification-name position-x)))
    (ok (math:unit-expression=
         :metre (math:quantity-specification-unit position-x)))
    (ok (math:quantity-specification-affine-p position-x))
    (ok (zerop (math:quantity-specification-tensor-order position-x)))
    (ok (signals
         (math:project-quantity-specification
          (math:make-quantity-specification
           :unnamed-axes :tensor-order 1)
          '(0) 2)
         'math:quantity-operation-error))))

(deftest interpretation-requires-derived-semantics
  (let ((target
          (math:make-quantity-specification
           :distance :dimension :length :unit :metre)))
    (ok (signals (math:interpret-quantity-specification nil target)
                 'math:quantity-operation-error))))
