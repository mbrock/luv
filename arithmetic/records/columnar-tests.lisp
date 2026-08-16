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

(deftest columnar-buffers-grow-pop-reset-and-iterate-raw-lanes
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
    (ok (= 2 (test-columnar-buffer-length buffer)))
    (ok (>= (test-columnar-buffer-capacity buffer) 2))
    (ok (= (test-columnar-buffer-capacity buffer)
           (length (test-columnar-buffer-object-lane buffer))
           (length (test-columnar-buffer-distance-lane buffer))))
    (ok (not (eq first-lane (test-columnar-buffer-object-lane buffer))))
    (ok (eq distance
            (records:columnar-row-lane-declaration
             (test-columnar-buffer-row-declaration buffer) 'distance)))
    (let ((readings nil))
      (records:do-columnar-buffer-rows
          ((object value) buffer test-columnar-buffer)
        (push (list (first object) value) readings))
      (ok (equal '((:second 2.0f0) (:first 1.0f0)) readings)))
    (multiple-value-bind (object value present-p)
        (test-columnar-buffer-pop buffer)
      (ok present-p)
      (ok (equal '(:second) object))
      (ok (= 2.0f0 value)))
    (ok (null (aref (test-columnar-buffer-object-lane buffer) 1)))
    (test-columnar-buffer-push buffer (list :again) 3.0f0)
    (test-columnar-buffer-reset buffer)
    (ok (zerop (test-columnar-buffer-length buffer)))
    (ok (loop for object across (test-columnar-buffer-object-lane buffer)
              always (null object)))))

(deftest columnar-bindings-reject-incompatible-representations
  (let ((wrong
          (math:make-represented-value-declaration
           :representation-type 'string
           :quantity-specification
           (math:make-declared-quantity-specification
            '(:quantity :distance :unit :metre)))))
    (ok (signals
         (make-test-columnar-buffer
          :declarations `((distance . ,wrong)))
         'records:columnar-declaration-error))))

(deftest split-columnar-lanes-retain-one-vector-quantity
  (let* ((buffer (make-test-position-columns))
         (row (test-position-columns-row-declaration buffer))
         (layout (records:columnar-row-declaration-quantity-layout row))
         (position (math:project-quantity-layout layout '(0 1))))
    (ok position)
    (ok (eq :position (math:quantity-specification-name position)))
    (ok (= 1 (math:quantity-specification-tensor-order position)))
    (test-position-columns-push buffer 3.0f0 4.0f0)
    (records:with-columnar-buffer-row
        ((x y) buffer 0 test-position-columns)
      (ok (= 3.0f0 x))
      (ok (= 4.0f0 y)))))
