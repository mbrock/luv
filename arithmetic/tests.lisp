(defpackage #:luv.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip)
  (:local-nicknames (#:lang #:luv.arithmetic.language)
                    (#:lisp #:luv.arithmetic.lisp)
                    (#:vec #:luv.arithmetic.lisp.vec3)
                    (#:lvk #:luv.vulkan)
                    (#:math #:luv.arithmetic)
                    #+darwin
                    (#:metal #:luv.metal)
                    #+darwin
                    (#:objc #:luv.objective-c)
                    (#:records #:luv.arithmetic.records)
                    (#:vk #:luv.vk))
  (:import-from #:luv.arithmetic #:dot #:clamp)
  (:import-from #:luv.arithmetic.language
                #:quantity #:interpret #:convert-unit #:counted-fold))

(in-package #:luv.tests)

(math:define-quantity :position :kind :length
  :components (:position-x :position-y))

(math:define-quantity-constant +declared-test-distance+ 3.5d0
  :type double-float
  :quantity (:quantity :distance :unit :metre))

(define-test quantity-constants-publish-meaning-without-wrapping-values
  (let ((declaration
          (math:value-declaration-for '+declared-test-distance+)))
    (true (typep +declared-test-distance+ 'double-float))
    (true (= +declared-test-distance+ 3.5d0))
    (true (eq 'double-float
              (math:declaration-representation-type declaration)))
    (true (eq :distance
              (math:quantity-specification-name
               (math:declaration-quantity-specification declaration))))
    (true (math:unit-expression=
           :metre
           (math:quantity-specification-unit
            (math:declaration-quantity-specification declaration))))
    (true (null (math:value-declaration-for '+not-a-declared-value+)))))

(define-test dimensions-form-a-canonical-symbolic-product
  (let* ((length (math:make-dimension :length))
         (duration (math:make-dimension :duration))
         (velocity (math:divide-dimensions length duration))
         (acceleration (math:divide-dimensions velocity duration)))
    (true (math:dimension=
           velocity (math:make-dimension '((:length 1) (:duration -1)))))
    (true (math:dimension=
           acceleration
           (math:make-dimension '((:duration -2) (:length 1)))))
    (true (math:dimensionless-p
           (math:divide-dimensions length length)))
    (true (math:dimension=
           (math:exponentiate-dimension acceleration 1/2)
           (math:make-dimension '((:length 1/2) (:duration -1)))))))

(define-test named-quantity-spaces-govern-addition
  (let ((airspeed
          (math:make-quantity-specification
           :airspeed :dimension '((:length 1) (:duration -1))))
        (climb-rate
          (math:make-quantity-specification
           :climb-rate :dimension '((:length 1) (:duration -1)))))
    (fail (math:derive-quantity-specification '+ airspeed climb-rate)
          'math:quantity-operation-error)
    (true (eq :different-quantity-spaces
              (handler-case
                  (progn
                    (math:derive-quantity-specification '+ airspeed climb-rate)
                    nil)
                (math:quantity-operation-error (condition)
                  (math:quantity-operation-error-reason condition)))))))

(define-test affine-points-and-differences-have-asymmetric-arithmetic
  (let* ((point
           (math:make-quantity-specification
            :shadow-depth :affine-p t))
         (difference
           (math:make-quantity-specification
            :shadow-depth :character :difference))
         (moved (math:derive-quantity-specification '- point difference))
         (separation (math:derive-quantity-specification '- point point)))
    (true (math:quantity-specification-affine-p moved))
    (true (not (math:quantity-specification-affine-p separation)))
    (fail (math:derive-quantity-specification '+ point point)
          'math:quantity-operation-error)
    (fail (math:derive-quantity-specification '- difference point)
          'math:quantity-operation-error)))

(define-test dimensionless-scalar-scaling-preserves-semantic-identity
  (let* ((bias (math:make-quantity-specification
                :shadow-depth :character :difference))
         (number (math:make-quantity-specification nil))
         (scaled (math:derive-quantity-specification '* number bias)))
    (true (math:quantity-specification= scaled bias))))

(define-test exact-units-compose-without-implicit-conversion
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
    (fail (math:derive-quantity-specification '+ metres kilometres)
          'math:quantity-operation-error)
    (true (eq :different-units
              (handler-case
                  (progn
                    (math:derive-quantity-specification '+ metres kilometres)
                    nil)
                (math:quantity-operation-error (condition)
                  (math:quantity-operation-error-reason condition)))))
    (true (math:unit-expression=
           (math:quantity-specification-unit speed)
           '((:metre 1) (:second -1))))))

(define-test semantic-units-own-dimensions-bases-and-scales
  (let* ((metres
           (math:make-quantity-specification :distance :unit :metre))
         (kilometres
           (math:make-quantity-specification :distance :unit :kilometre))
         (percent-proportion
           (math:make-quantity-specification :proportion :unit :percent)))
    (true (math:dimension=
           :length (math:quantity-specification-dimension metres)))
    (true (math:dimensionless-p
           (math:quantity-specification-dimension percent-proportion)))
    (true (= 1000
             (math:unit-conversion-factor :kilometre :metre)))
    (true (= 1/100
             (math:unit-conversion-factor :percent :one)))
    (true (= 1/1000
             (math:unit-conversion-factor :per-mille :one)))
    (true (= 1/1000000
             (math:unit-conversion-factor :parts-per-million :one)))
    (true (math:dimension=
           '((:duration -1))
           (math:quantity-specification-dimension
            (math:make-quantity-specification :frequency :unit :hertz))))
    (multiple-value-bind (converted factor)
        (math:convert-quantity-specification-unit kilometres :metre)
      (true (= factor 1000))
      (true (eq :distance
                (math:quantity-specification-name converted)))
      (true (math:unit-expression=
             :metre (math:quantity-specification-unit converted))))
    (multiple-value-bind (converted factor)
        (math:convert-quantity-specification-unit percent-proportion :one)
      (true (= factor 1/100))
      (true (eq :proportion
                (math:quantity-specification-name converted)))
      (true (math:unitless-p
             (math:quantity-specification-unit converted))))
    (fail
     (math:make-quantity-specification
      :duration :dimension :duration :unit :metre)
     'math:quantity-operation-error)
    (fail
     (math:make-quantity-specification
      :unitless :dimension nil :unit :metre)
     'math:quantity-operation-error)
    (fail (math:make-unit-expression :mystery-unit)
          'math:undefined-unit)))

(define-test units-are-admissible-only-for-their-semantic-quantity-kinds
  (let ((angle
          (math:make-quantity-specification :angle :unit :radian))
        (proportion
          (math:make-quantity-specification :proportion :unit :percent)))
    (true (eq :angular-measure
              (math:quantity-specification-kind angle)))
    (true (eq :proportion
              (math:quantity-specification-kind proportion)))
    (multiple-value-bind (converted factor)
        (math:convert-quantity-specification-unit angle :one)
      (true (= 1 factor))
      (true (eq :angular-measure
                (math:quantity-specification-kind converted))))
    (fail
     (math:make-quantity-specification :proportion :unit :radian)
     'math:quantity-operation-error)
    (fail
     (math:make-quantity-specification :angle :unit :steradian)
     'math:quantity-operation-error)
    (fail
     (math:make-quantity-specification
      :proportion :unit '((:radian 1) (:second -1)))
     'math:quantity-operation-error)
    (fail
     (math:make-quantity-specification
      :proportion :dimension :length)
     'math:quantity-operation-error)
    (true (eq :undefined-quantity-definition
              (handler-case
                  (progn
                    (math:make-quantity-specification
                     :unregistered-distance :unit :metre)
                    nil)
                (math:quantity-operation-error (condition)
                  (math:quantity-operation-error-reason condition)))))))

(define-test extrema-require-exactly-compatible-quantities
  (let ((left
          (math:make-quantity-specification
           :distance :dimension :length :unit :metre))
        (right
          (math:make-quantity-specification
           :distance :dimension :length :unit :metre))
        (other-unit
          (math:make-quantity-specification
           :distance :dimension :length :unit :kilometre)))
    (true (math:quantity-specification=
           left (math:derive-quantity-specification 'max left right)))
    (fail
     (math:derive-quantity-specification 'max left other-unit)
     'math:quantity-operation-error)))

(define-test portable-numerical-operators-own-backend-neutral-semantics
  (let* ((depth
           (math:make-quantity-specification
            :distance :dimension :length :unit :metre))
         (amount (math:make-quantity-specification nil))
         (direction
           (math:make-quantity-specification nil :tensor-order 1)))
    (true (math:quantity-specification=
           depth
           (math:derive-quantity-specification
            'math:clamp depth depth depth)))
    (true (math:quantity-specification=
           depth
           (math:derive-quantity-specification
            'math:mix depth depth amount)))
    (true (math:dimensionless-quantity-specification-p
           (math:derive-quantity-specification
            'math:step depth depth)
           0))
    (true (math:dimensionless-quantity-specification-p
           (math:derive-quantity-specification
            'math:smoothstep depth depth depth)
           0))
    (true (eq direction
              (math:derive-quantity-specification
               'math:normalize direction)))
    (true (math:dimensionless-quantity-specification-p
           (math:derive-quantity-specification 'expt amount amount)
           0))))

(define-test quantity-definitions-own-their-homogeneous-components
  (let ((position (math:quantity-definition-for :position))
        (position-x (math:quantity-definition-for :position-x)))
    (true (equal '(:position-x :position-y)
                 (math:quantity-definition-components position)))
    (true (equal (math:quantity-definition-components position)
                 (math:quantity-component-names :position)))
    (true (eq :length (math:quantity-definition-kind position-x)))))

(define-test semantic-layouts-distinguish-vectors-from-packed-tuples
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
    (true (eq uv (math:project-quantity-layout layout '(0 1))))
    (true (eq occlusion (math:project-quantity-layout layout '(2))))
    (true (math:quantity-layout= layout reordered-layout))
    (true (null (math:project-quantity-layout layout '(1 2))))
    (true (eq :position-x
              (math:quantity-specification-name position-x)))
    (true (math:unit-expression=
           :metre (math:quantity-specification-unit position-x)))
    (true (math:quantity-specification-affine-p position-x))
    (true (zerop (math:quantity-specification-tensor-order position-x)))
    (fail
     (math:project-quantity-specification
      (math:make-quantity-specification
       :unnamed-axes :tensor-order 1)
      '(0) 2)
     'math:quantity-operation-error)))

(define-test repeated-layouts-name-strided-products-without-element-objects
  (let* ((position
           (math:make-quantity-specification
            :position :dimension :length :unit :metre
            :tensor-order 1 :affine-p t))
         (element
           (math:make-quantity-layout
            3 (list (math:make-quantity-projection '(0 1 2) position))))
         (packed (math:make-repeated-quantity-layout element))
         (padded (math:make-repeated-quantity-layout element :stride 4))
         (same
           (math:make-repeated-quantity-layout
            (math:make-quantity-layout
             3 (list (math:make-quantity-projection '(0 1 2) position))))))
    (true (= 3 (math:repeated-quantity-layout-stride packed)))
    (true (= 4 (math:quantity-layout-extent padded)))
    (true (eq element
              (math:repeated-quantity-layout-element-layout packed)))
    (true (eq position (math:project-quantity-layout packed '(0 1 2))))
    (true (math:quantity-layout= packed same))
    (true (not (math:quantity-layout= packed element)))
    (true (not (math:quantity-layout= packed padded)))
    (fail (math:make-repeated-quantity-layout element :stride 2)
          'error)))

(define-test source-declarations-share-one-quantity-option-parser
  (let ((point
          (math:make-declared-quantity-specification
           '(:quantity :position :unit :metre
             :tensor-order 1 :character :point)))
        (anonymous-vector
          (math:make-declared-quantity-specification
           '(:unit :one) :default-tensor-order 1)))
    (true (null (math:make-declared-quantity-specification nil)))
    (true (eq :position (math:quantity-specification-name point)))
    (true (eq :point (math:quantity-specification-character point)))
    (true (= 1 (math:quantity-specification-tensor-order point)))
    (true (null (math:quantity-specification-name anonymous-vector)))
    (true (= 1
             (math:quantity-specification-tensor-order anonymous-vector)))))

(define-test represented-value-declarations-keep-meaning-beside-representation
  (let* ((specification
           (math:make-declared-quantity-specification
            '(:quantity :position :unit :metre
              :tensor-order 1 :character :point)))
         (declaration
           (math:make-represented-value-declaration
            :representation-type 'vec3
            :quantity-specification specification
            :source-form '(position :type vec3
                                    :quantity (:position :unit :metre)))))
    (true (eq 'vec3 (math:declaration-representation-type declaration)))
    (true (eq specification
              (math:declaration-quantity-specification declaration)))
    (true (null (math:declaration-quantity-layout declaration)))
    (true (math:declaration-quantity-checked-p declaration))
    (true (equal '(position :type vec3
                            :quantity (:position :unit :metre))
                 (math:declaration-source-form declaration)))))

(define-test represented-value-compatibility-checks-meaning-and-representation
  (let* ((distance
           (math:make-declared-quantity-specification
            '(:quantity :distance :unit :metre)))
         (height
           (math:make-declared-quantity-specification
            '(:quantity :height :unit :metre)))
         (expected
           (math:make-represented-value-declaration
            :representation-type 'real
            :quantity-specification distance
            :source-form '(parameter distance)))
         (actual
           (math:make-represented-value-declaration
            :representation-type 'double-float
            :quantity-specification distance
            :source-form '(slot distance)))
         (wrong-meaning
           (math:make-represented-value-declaration
            :representation-type 'double-float
            :quantity-specification height
            :source-form '(slot height)))
         (wrong-representation
           (math:make-represented-value-declaration
            :representation-type 'string
            :quantity-specification distance
            :source-form '(slot label))))
    (true (eq actual
              (math:ensure-declarations-compatible actual expected)))
    (fail (math:ensure-declarations-compatible
           wrong-meaning expected)
          'math:declaration-compatibility-error)
    (fail (math:ensure-declarations-compatible
           wrong-representation expected)
          'math:declaration-compatibility-error)))

(define-test interpretation-requires-derived-semantics
  (let ((target
          (math:make-quantity-specification
           :distance :dimension :length :unit :metre)))
    (fail (math:interpret-quantity-specification nil target)
          'math:quantity-operation-error)))

;;; The three-way affine character: point, absolute, difference.  These are
;;; the executable claims of the V3 operation table in wiki figure #LNRY72.

(math:define-quantity :light-level :kind :proportion :non-negative-p t)
(math:define-quantity :altitude :kind :length :character :point)

(defun operation-reason (operator &rest specifications)
  (handler-case
      (progn (apply #'math:derive-quantity-specification operator specifications)
             nil)
    (math:quantity-operation-error (condition)
      (math:quantity-operation-error-reason condition))))

(define-test non-negative-definitions-default-to-absolute-character
  (let ((level (math:make-quantity-specification :light-level :unit :one))
        (altitude (math:make-quantity-specification :altitude :unit :metre))
        (plain (math:make-quantity-specification :distance :unit :metre)))
    (true (eq :absolute (math:quantity-specification-character level)))
    (true (math:quantity-specification-absolute-p level))
    (true (math:quantity-specification-non-negative-p level))
    (true (eq :point (math:quantity-specification-character altitude)))
    (true (math:quantity-specification-affine-p altitude))
    (true (eq :difference (math:quantity-specification-character plain)))
    (true (math:quantity-specification-difference-p plain))
    ;; The historical :affine-p spelling still means point, and an explicit
    ;; :affine-p nil on a declared point yields a difference of that quantity.
    (true (eq :point
              (math:quantity-specification-character
               (math:make-quantity-specification :distance :unit :metre
                                                 :affine-p t))))
    (true (eq :difference
              (math:quantity-specification-character
               (math:make-quantity-specification :altitude :unit :metre
                                                 :affine-p nil))))
    (fail (math:make-quantity-specification :distance :unit :metre
                                            :affine-p t
                                            :character :absolute)
          'error)))

(define-test absolute-and-difference-addition-follows-the-cone-rules
  (let* ((level (math:make-quantity-specification :light-level :unit :one))
         (delta (math:make-quantity-specification :light-level :unit :one
                                                  :character :difference))
         (sum (math:derive-quantity-specification '+ level level))
         (shifted (math:derive-quantity-specification '+ level delta))
         (gap (math:derive-quantity-specification '- level level))
         (reduced (math:derive-quantity-specification '- level delta))
         (signed (math:derive-quantity-specification '- delta level)))
    (true (eq :absolute (math:quantity-specification-character sum)))
    (true (eq :absolute (math:quantity-specification-character shifted)))
    (true (eq :difference (math:quantity-specification-character gap)))
    (true (eq :absolute (math:quantity-specification-character reduced)))
    (true (eq :difference (math:quantity-specification-character signed)))
    ;; A derived difference is anonymous-in-character but named, so it no
    ;; longer promises non-negativity.
    (true (not (math:quantity-specification-non-negative-p gap)))))

(define-test points-mix-with-amounts-and-differences-asymmetrically
  (let* ((altitude (math:make-quantity-specification :altitude :unit :metre))
         (height (math:make-quantity-specification :height :unit :metre
                                                   :character :absolute))
         (climb (math:make-quantity-specification :altitude :unit :metre
                                                  :character :difference))
         (raised (math:derive-quantity-specification '+ altitude climb))
         (lowered (math:derive-quantity-specification '- altitude climb))
         (span (math:derive-quantity-specification '- altitude altitude)))
    (true (eq :point (math:quantity-specification-character raised)))
    (true (eq :point (math:quantity-specification-character lowered)))
    (true (eq :difference (math:quantity-specification-character span)))
    (true (eq :cannot-add-points (operation-reason '+ altitude altitude)))
    (true (eq :cannot-subtract-point-from-amount
              (operation-reason '- climb altitude)))
    (true (eq :cannot-scale-affine-point (operation-reason '* altitude height)))
    (true (eq :cannot-negate-point (operation-reason '- altitude)))))

(define-test products-keep-absoluteness-only-when-every-factor-has-it
  (let* ((level (math:make-quantity-specification :light-level :unit :one))
         (delta (math:make-quantity-specification :light-level :unit :one
                                                  :character :difference))
         (metres (math:make-quantity-specification :distance :unit :metre
                                                   :character :absolute))
         (number (math:make-quantity-specification nil))
         (product (math:derive-quantity-specification '* level metres))
         (ratio (math:derive-quantity-specification '/ level level))
         (mixed (math:derive-quantity-specification '* level delta))
         (rate (math:derive-quantity-specification '/ delta metres))
         (scaled (math:derive-quantity-specification '* number level))
         (scaled-delta (math:derive-quantity-specification '* number delta)))
    (true (eq :absolute (math:quantity-specification-character product)))
    (true (eq :absolute (math:quantity-specification-character ratio)))
    (true (eq :difference (math:quantity-specification-character mixed)))
    (true (eq :difference (math:quantity-specification-character rate)))
    ;; A bare number is a scale factor and preserves the other character.
    (true (eq :absolute (math:quantity-specification-character scaled)))
    (true (eq :difference (math:quantity-specification-character scaled-delta)))))

(define-test negating-an-amount-is-never-an-amount
  (let ((level (math:make-quantity-specification :light-level :unit :one))
        (delta (math:make-quantity-specification :light-level :unit :one
                                                 :character :difference)))
    (true (eq :cannot-negate-amount (operation-reason '- level)))
    (true (eq :difference
              (math:quantity-specification-character
               (math:derive-quantity-specification '- delta))))))

(define-test interpretation-may-promote-a-difference-to-an-absolute
  (let* ((level (math:make-quantity-specification :light-level :unit :one))
         (gap (math:derive-quantity-specification '- level level))
         (altitude (math:make-quantity-specification :altitude :unit :metre))
         (metres (math:make-quantity-specification :distance :unit :metre)))
    (true (eq :absolute
              (math:quantity-specification-character
               (math:interpret-quantity-specification gap level))))
    ;; No other character crossing is an interpretation.
    (fail (math:interpret-quantity-specification level gap)
          'math:quantity-operation-error)
    (fail (math:interpret-quantity-specification metres altitude)
          'math:quantity-operation-error)
    (fail (math:interpret-quantity-specification altitude metres)
          'math:quantity-operation-error)))
