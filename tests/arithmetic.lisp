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
