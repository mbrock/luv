(in-package #:luv.tests)

(defclass quantity-base-record ()
  ((position
    :initarg :position
    :type vector
    :quantity (:quantity :distance :unit :metre :tensor-order 1)
    :accessor quantity-record-position))
  (:metaclass records:quantity-class))

(defclass quantity-derived-record (quantity-base-record) ()
  (:metaclass records:quantity-class))

(defclass quantity-redefined-record ()
  ((reading
    :type double-float
    :quantity (:quantity :distance :unit :metre)))
  (:metaclass records:quantity-class))

(records:define-quantity-struct
    (quantity-structure-record
     (:constructor make-quantity-structure-record (direction count)))
  (direction #() :type vector
             :quantity (:unit :one :tensor-order 1))
  (count 0 :type fixnum))

(deftest clos-slots-retain-effective-quantity-declarations
  (let* ((direct
           (first (closer-mop:class-direct-slots
                   (find-class 'quantity-base-record))))
         (base
           (records:record-slot-declaration
            'quantity-base-record 'position))
         (inherited
           (records:record-slot-declaration
            'quantity-derived-record 'position)))
    (ok (eq 'vector (math:declaration-representation-type direct)))
    (ok (eq 'vector (math:declaration-representation-type base)))
    (ok (math:quantity-specification=
         (math:declaration-quantity-specification base)
         (math:declaration-quantity-specification inherited)))
    (ok (eq :distance
            (math:quantity-specification-name
             (math:declaration-quantity-specification inherited))))))

(deftest clos-slot-access-remains-ordinary-raw-access
  (let* ((value #(1.0d0 2.0d0 3.0d0))
         (record (make-instance 'quantity-base-record :position value)))
    (ok (eq value (quantity-record-position record)))
    (setf (quantity-record-position record) #(4.0d0 5.0d0 6.0d0))
    (ok (equalp #(4.0d0 5.0d0 6.0d0)
                (quantity-record-position record)))))

(deftest clos-slot-redefinition-replaces-inspectable-quantity-meaning
  (unwind-protect
       (progn
         (eval
          '(defclass quantity-redefined-record ()
             ((reading
               :type double-float
               :quantity (:quantity :height :unit :metre)))
             (:metaclass records:quantity-class)))
         (ok (eq :height
                 (math:quantity-specification-name
                  (math:declaration-quantity-specification
                   (records:record-slot-declaration
                    'quantity-redefined-record 'reading))))))
    (eval
     '(defclass quantity-redefined-record ()
        ((reading
          :type double-float
          :quantity (:quantity :distance :unit :metre)))
        (:metaclass records:quantity-class)))))

(deftest conflicting-inherited-quantity-declarations-are-rejected
  (ok (signals
       (progn
         (eval
          '(defclass quantity-conflicting-record (quantity-base-record)
             ((position
               :type vector
               :quantity (:quantity :height :unit :metre :tensor-order 1)))
             (:metaclass records:quantity-class)))
         ;; CLOS may defer effective-slot computation until finalization.
         (records:record-slot-declaration
          'quantity-conflicting-record 'position))
       'records:quantity-slot-conflict)))

(deftest defstruct-retains-layout-and-publishes-the-same-protocol
  (let* ((direction #(1.0 0.0 0.0))
         (record (make-quantity-structure-record direction 3))
         (declaration
           (records:record-slot-declaration
            'quantity-structure-record 'direction)))
    (ok (eq direction (quantity-structure-record-direction record)))
    (ok (= 3 (quantity-structure-record-count record)))
    (ok (eq 'vector (math:declaration-representation-type declaration)))
    (ok (= 1
           (math:quantity-specification-tensor-order
            (math:declaration-quantity-specification declaration))))
    (ok (null
         (records:record-slot-declaration
          'quantity-structure-record 'count)))))
