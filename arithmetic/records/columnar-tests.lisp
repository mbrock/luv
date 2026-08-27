(in-package #:luv.tests)

(records:define-columnar-buffer test-columnar-buffer
  (object nil :type (or null cons) :clear-on-remove t)
  (distance 0f0 :type single-float))

(records:define-columnar-buffer
    (test-position-columns
     :quantities (((x y)
                   (:quantity :position :unit :metre :tensor-order 1))))
  (x 0f0 :type single-float)
  (y 0f0 :type single-float))

(defclass test-finite-domain ()
  ((cardinality :initarg :cardinality :reader test-domain-cardinality)))

(defmethod luv.domains:domain-cardinality ((domain test-finite-domain))
  (test-domain-cardinality domain))

(records:define-columnar-materialization test-columnar-materialization
  (sample 0 :type (unsigned-byte 16))
  (weight 0f0 :type single-float))

(define-test columnar-buffers-grow-pop-reset-and-iterate-raw-lanes
  (let* ((distance
           (math:make-represented-value-declaration
            :representation-type 'single-float
            :quantity-specification
            (math:make-declared-quantity-specification
             '(:quantity :distance :unit :metre))))
         (buffer
           (make-test-columnar-buffer
            :capacity 1 :declarations `((distance . ,distance))))
         (first-lane (test-columnar-buffer-object-lane buffer)))
    (test-columnar-buffer-push buffer (list :first) 1.0f0)
    (test-columnar-buffer-push buffer (list :second) 2.0f0)
    (true (= 2 (test-columnar-buffer-length buffer)))
    (true (>= (test-columnar-buffer-capacity buffer) 2))
    (true (= (test-columnar-buffer-capacity buffer)
             (length (test-columnar-buffer-object-lane buffer))
             (length (test-columnar-buffer-distance-lane buffer))))
    (true (not (eq first-lane (test-columnar-buffer-object-lane buffer))))
    (true (eq distance
              (records:columnar-row-lane-declaration
               (test-columnar-buffer-row-declaration buffer) 'distance)))
    (records:with-columnar-buffer-storage
        ((length row (objects object) (distances distance))
         buffer test-columnar-buffer)
      (true (= 2 length))
      (true (eq row (test-columnar-buffer-row-declaration buffer)))
      (true (eq objects (test-columnar-buffer-object-lane buffer)))
      (true (eq distances (test-columnar-buffer-distance-lane buffer))))
    (let ((readings nil))
      (records:do-columnar-buffer-rows
          ((object value) buffer test-columnar-buffer)
        (push (list (first object) value) readings))
      (true (equal '((:second 2.0f0) (:first 1.0f0)) readings)))
    (multiple-value-bind (object value present-p)
        (test-columnar-buffer-pop buffer)
      (true present-p)
      (true (equal '(:second) object))
      (true (= 2.0f0 value)))
    (true (null (aref (test-columnar-buffer-object-lane buffer) 1)))
    (test-columnar-buffer-push buffer (list :again) 3.0f0)
    (test-columnar-buffer-reset buffer)
    (true (zerop (test-columnar-buffer-length buffer)))
    (true (loop for object across (test-columnar-buffer-object-lane buffer)
                always (null object)))))

(define-test columnar-bindings-reject-incompatible-representations
  (let ((wrong
          (math:make-represented-value-declaration
           :representation-type 'string
           :quantity-specification
           (math:make-declared-quantity-specification
            '(:quantity :distance :unit :metre)))))
    (fail
     (make-test-columnar-buffer
      :declarations `((distance . ,wrong)))
     'records:columnar-declaration-error)))

(define-test split-columnar-lanes-retain-one-vector-quantity
  (let* ((buffer (make-test-position-columns))
         (row (test-position-columns-row-declaration buffer))
         (layout (records:columnar-row-declaration-quantity-layout row))
         (position (math:project-quantity-layout layout '(0 1))))
    (true position)
    (true (eq :position (math:quantity-specification-name position)))
    (true (= 1 (math:quantity-specification-tensor-order position)))
    (test-position-columns-push buffer 3.0f0 4.0f0)
    (records:with-columnar-buffer-row
        ((x y) buffer 0 test-position-columns)
      (true (= 3.0f0 x))
      (true (= 4.0f0 y)))))

(define-test fixed-columnar-materializations-retain-domain-layout-and-storage
  (let* ((domain (make-instance 'test-finite-domain :cardinality 3))
         (distance
           (math:make-represented-value-declaration
            :representation-type 'single-float
            :quantity-specification
            (math:make-declared-quantity-specification
             '(:quantity :distance :unit :metre))))
         (materialization
           (make-test-columnar-materialization
            domain :declarations `((weight . ,distance)))))
    (records:with-columnar-materialization-storage
        ((borrowed-domain extent row (samples sample) (weights weight))
         materialization test-columnar-materialization)
      (true (eq domain borrowed-domain))
      (true (= 3 extent (length samples) (length weights)))
      (true (eq distance
                (records:columnar-row-lane-declaration row 'weight)))
      (setf (aref samples 1) 9
            (aref weights 1) 2.5f0)
      (true (= 9 (aref samples 1)))
      (true (= 2.5f0 (aref weights 1))))))
