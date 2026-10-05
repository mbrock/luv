;;; Plain-language explanations of semantic shader quantities.
;;;
;;; Every textual lowering may comment a declaration, binding, or resource
;;; with what its value means.  The sentences depend only on the shared
;;; quantity specifications and layouts, so they live beside the language
;;; rather than inside one backend.

(in-package #:luv.shader)

(defun semantic-words (name)
  (substitute #\Space #\- (string-downcase (symbol-name name))))

(defun semantic-factor-description (factor)
  (let ((name (semantic-words (car factor)))
        (power (cdr factor)))
    (case power
      (1 name)
      (2 (format nil "~A squared" name))
      (3 (format nil "~A cubed" name))
      (otherwise (format nil "~A to the ~A power" name power)))))

(defun semantic-factor-product-description (factors)
  (format nil "~{~A~^ times ~}" (mapcar #'semantic-factor-description factors)))

(defun semantic-tensor-description (order)
  (case order
    (0 "scalar")
    (1 "vector")
    (otherwise (format nil "tensor of order ~D" order))))

(defun semantic-character-description (specification)
  (case (math:quantity-specification-character specification)
    (:point "point-valued")
    (:absolute
     (if (math:quantity-specification-non-negative-p specification)
         "non-negative absolute"
         "absolute"))
    (:difference "difference-valued")))

(defun semantic-quantity-predicate (specification)
  (let* ((kind (math:quantity-specification-kind specification))
         (unit-factors
           (math:unit-expression-factors
            (math:quantity-specification-unit specification)))
         (dimension-factors
           (math:dimension-factors
            (math:quantity-specification-dimension specification))))
    (with-output-to-string (stream)
      (format stream "a ~A ~A"
              (semantic-character-description specification)
              (semantic-tensor-description
               (math:quantity-specification-tensor-order specification)))
      (when kind
        (format stream " in the ~A kind" (semantic-words kind)))
      (cond
        ((and (null unit-factors) (null dimension-factors))
         (write-string ", unitless and dimensionless" stream))
        (t
         (if unit-factors
             (format stream ", measured in ~A units"
                     (semantic-factor-product-description unit-factors))
             (write-string ", unitless" stream))
         (if dimension-factors
             (format stream ", with ~A dimension"
                     (semantic-factor-product-description dimension-factors))
             (write-string ", and dimensionless" stream)))))))

(defun semantic-capitalize-sentence (text)
  (if (plusp (length text))
      (concatenate 'string
                   (string (char-upcase (char text 0)))
                   (subseq text 1))
      text))

(defun semantic-quantity-sentence (specification)
  (let ((name (math:quantity-specification-name specification)))
    (format nil "~A is ~A."
            (if name
                (semantic-capitalize-sentence (semantic-words name))
                "This value")
            (semantic-quantity-predicate specification))))

(defun semantic-lane-name (positions)
  (coerce (mapcar (lambda (position) (char "xyzw" position)) positions)
          'string))

(defun semantic-layout-sentences (layout &key sampled-p)
  (let ((occupied nil)
        (sentences nil))
    (dolist (projection (math:quantity-layout-projections layout))
      (let* ((positions (math:quantity-projection-positions projection))
             (specification
               (math:quantity-projection-specification projection))
             (name (math:quantity-specification-name specification))
             (lanes (semantic-lane-name positions)))
        (setf occupied (nconc (copy-list positions) occupied))
        (push
         (format nil "The ~A~A ~A ~A ~A~A."
                 (if sampled-p "sampled " "")
                 lanes
                 (if (= (length positions) 1) "lane" "lanes")
                 (if (= (length positions) 1) "holds" "hold")
                 (if name (semantic-words name) "an unnamed quantity")
                 (format nil " as ~A" (semantic-quantity-predicate specification)))
         sentences)))
    (let ((uncovered
            (loop for position below (math:quantity-layout-extent layout)
                  unless (member position occupied)
                    collect position)))
      (when uncovered
        (push
         (format nil "The ~A~A ~A ~A no quantity annotation."
                 (if sampled-p "sampled " "")
                 (semantic-lane-name uncovered)
                 (if (= (length uncovered) 1) "lane" "lanes")
                 (if (= (length uncovered) 1) "has" "have"))
         sentences)))
    (nreverse sentences)))

(defgeneric shader-origin-quantity-specification (origin)
  (:documentation "Return the homogeneous quantity carried by ORIGIN."))

(defmethod shader-origin-quantity-specification ((origin t))
  nil)

(defmethod shader-origin-quantity-specification
    ((origin shader-variable-declaration))
  (shader-declaration-quantity-specification origin))

(defmethod shader-origin-quantity-specification ((origin shader-binding))
  (shader-expression-quantity-specification
   (shader-binding-expression origin)))

(defmethod shader-origin-quantity-specification
    ((origin shader-output-assignment))
  (shader-expression-quantity-specification
   (shader-assignment-value origin)))

(defmethod shader-origin-quantity-specification ((origin shader-resource))
  (shader-resource-sample-quantity-specification origin))

(defgeneric shader-origin-quantity-layout (origin)
  (:documentation "Return the component quantity layout carried by ORIGIN."))

(defmethod shader-origin-quantity-layout ((origin t))
  nil)

(defmethod shader-origin-quantity-layout
    ((origin shader-variable-declaration))
  (shader-declaration-quantity-layout origin))

(defmethod shader-origin-quantity-layout ((origin shader-binding))
  (shader-expression-quantity-layout
   (shader-binding-expression origin)))

(defmethod shader-origin-quantity-layout
    ((origin shader-output-assignment))
  (shader-expression-quantity-layout
   (shader-assignment-value origin)))

(defmethod shader-origin-quantity-layout ((origin shader-resource))
  (shader-resource-sample-quantity-layout origin))

(defun shader-semantic-sentences (origin &key sampled-p unannotated-p)
  "Return English sentences saying what ORIGIN's value means.
ORIGIN is a declaration, binding, output assignment, or resource.  SAMPLED-P
describes a texture's sampled lanes; UNANNOTATED-P admits a raw value."
  (let ((specification (shader-origin-quantity-specification origin))
        (layout (shader-origin-quantity-layout origin)))
    (cond
      (specification (list (semantic-quantity-sentence specification)))
      (layout (semantic-layout-sentences layout :sampled-p sampled-p))
      (unannotated-p (list "This numeric value has no quantity annotation.")))))
