;;; Direct Metal Shading Language lowering for luv's mathematical shaders.
;;;
;;; The source shader graph stays unchanged.  This sibling compiler produces a
;;; small structured document and records every rendered expression occurrence,
;;; rather than routing Metal through SPIR-V or accumulating one opaque string.

(in-package #:luv.msl)

(defclass msl-target ()
  ((language-version
    :initarg :language-version
    :initform "4.0"
    :reader msl-target-language-version))
  (:documentation
   "The Metal language policy selected for one direct shader lowering."))

(defparameter *metal-4-target*
  (make-instance 'msl-target :language-version "4.0"))

(defclass msl-source-occurrence ()
  ((expression
    :initarg :expression
    :reader msl-source-occurrence-expression)
   (text
    :initarg :text
    :reader msl-source-occurrence-text))
  (:documentation
   "One rendered occurrence retaining its originating shader expression."))

(defclass msl-field ()
  ((type :initarg :type :reader msl-field-type)
   (name :initarg :name :reader msl-field-name)
   (attribute :initarg :attribute :reader msl-field-attribute)
   (origin :initarg :origin :reader msl-field-origin)
   (array-length
    :initarg :array-length
    :initform nil
    :reader msl-field-array-length))
  (:documentation
   "One rendered structure field retaining its shader declaration for
#YA4KDP."))

(defclass msl-structure-declaration ()
  ((name :initarg :name :reader msl-structure-name)
   (fields :initarg :fields :reader msl-structure-fields)))

(defclass msl-parameter ()
  ((type :initarg :type :reader msl-parameter-type)
   (name :initarg :name :reader msl-parameter-name)
   (attribute :initarg :attribute :reader msl-parameter-attribute)
   (origin :initarg :origin :initform nil :reader msl-parameter-origin))
  (:documentation
   "One entry-point parameter and the resource, if any, which produced it."))

(defclass msl-variable-statement ()
  ((type :initarg :type :reader msl-variable-statement-type)
   (name :initarg :name :reader msl-variable-statement-name)
   (value :initarg :value :reader msl-variable-statement-value)
   (origin :initarg :origin :reader msl-variable-statement-origin))
  (:documentation
   "One local declaration retaining its semantic shader binding."))

(defclass msl-output-statement ()
  ((field :initarg :field :reader msl-output-statement-field)
   (value :initarg :value :reader msl-output-statement-value)
   (origin :initarg :origin :reader msl-output-statement-origin))
  (:documentation
   "One output assignment retaining its semantic shader assignment."))

(defclass msl-if-statement ()
  ((condition :initarg :condition :reader msl-if-statement-condition)
   (statements :initarg :statements :reader msl-if-statement-statements)
   (origin :initarg :origin :reader msl-if-statement-origin)))

(defclass msl-mesh-output-counts-statement ()
  ((lane :initarg :lane :reader msl-mesh-output-counts-lane)
   (vertex-count
    :initarg :vertex-count
    :reader msl-mesh-output-counts-vertex-count)
   (primitive-count
    :initarg :primitive-count
    :reader msl-mesh-output-counts-primitive-count)
   (origin :initarg :origin :reader msl-mesh-output-counts-origin)))

(defclass msl-mesh-vertex-statement ()
  ((index :initarg :index :reader msl-mesh-vertex-index)
   (vertex-type :initarg :vertex-type :reader msl-mesh-vertex-type)
   (values :initarg :values :reader msl-mesh-vertex-values)
   (origin :initarg :origin :reader msl-mesh-vertex-origin)))

(defclass msl-mesh-primitive-statement ()
  ((index :initarg :index :reader msl-mesh-primitive-index)
   (indices :initarg :indices :reader msl-mesh-primitive-indices)
   (topology :initarg :topology :reader msl-mesh-primitive-topology)
   (primitive-type
    :initarg :primitive-type
    :initform nil
    :reader msl-mesh-primitive-type)
   (values
    :initarg :values
    :initform nil
    :reader msl-mesh-primitive-values)
   (origin :initarg :origin :reader msl-mesh-primitive-origin)))

(defclass msl-task-payload-store-statement ()
  ((field :initarg :field :reader msl-task-payload-store-field)
   (index
    :initarg :index
    :initform nil
    :reader msl-task-payload-store-index)
   (value :initarg :value :reader msl-task-payload-store-value)
   (origin :initarg :origin :reader msl-task-payload-store-origin)))

(defclass msl-emit-mesh-workgroups-statement ()
  ((lane :initarg :lane :reader msl-emit-mesh-workgroups-lane)
   (workgroups
    :initarg :workgroups
    :reader msl-emit-mesh-workgroups-counts)
   (origin :initarg :origin :reader msl-emit-mesh-workgroups-origin)))

(defclass msl-counted-fold-statement ()
  ((type :initarg :type :reader msl-counted-fold-statement-type)
   (state-name
    :initarg :state-name :reader msl-counted-fold-statement-state-name)
   (initial :initarg :initial :reader msl-counted-fold-statement-initial)
   (index-name
    :initarg :index-name :reader msl-counted-fold-statement-index-name)
   (index-type
    :initarg :index-type :reader msl-counted-fold-statement-index-type)
   (count :initarg :count :reader msl-counted-fold-statement-count)
   (bindings
    :initarg :bindings :initform nil
    :reader msl-counted-fold-statement-bindings)
   (update :initarg :update :reader msl-counted-fold-statement-update)
   (until-bindings
    :initarg :until-bindings :initform nil
    :reader msl-counted-fold-statement-until-bindings)
   (until :initarg :until :initform nil
          :reader msl-counted-fold-statement-until)
   (origin :initarg :origin :reader msl-counted-fold-statement-origin)))

(defclass msl-entry-point ()
  ((stage :initarg :stage :reader msl-entry-point-stage)
   (return-type :initarg :return-type :reader msl-entry-point-return-type)
   (name :initarg :name :reader msl-entry-point-name)
   (parameters :initarg :parameters :reader msl-entry-point-parameters)
   (statements :initarg :statements :reader msl-entry-point-statements)))

(defclass msl-document ()
  ((target :initarg :target :reader msl-document-target)
   (specification :initarg :specification :reader msl-document-specification)
   (declarations :initarg :declarations :reader msl-document-declarations)
   (entry-point :initarg :entry-point :reader msl-document-entry-point)
   (source :initarg :source :accessor msl-document-source)
   (expression-occurrences
    :initarg :expression-occurrences
    :reader msl-document-expression-occurrences)
   (occurrence-expression
    :initarg :occurrence-expression
    :reader msl-document-occurrence-expression)))

(defclass msl-lowering-context ()
  ((target :initarg :target :reader msl-context-target)
   (specification :initarg :specification :reader msl-context-specification)
   (references
    :initform (make-hash-table :test #'eq)
    :reader msl-context-references)
   (expression-occurrences
    :initform (make-hash-table :test #'eq)
    :reader msl-context-expression-occurrences)
   (occurrence-expression
    :initform (make-hash-table :test #'eq)
    :reader msl-context-occurrence-expression)
   (pending-statements
    :initform nil :accessor msl-context-pending-statements)
   (fold-counter :initform 0 :accessor msl-context-fold-counter)))

(defun drain-msl-pending-statements (context)
  (prog1 (msl-context-pending-statements context)
    (setf (msl-context-pending-statements context) nil)))

(defun msl-identifier (name)
  (let ((text (string-downcase (string name))))
    (with-output-to-string (stream)
      (loop for character across text
            for firstp = t then nil
            for emitted = (if (or (alphanumericp character)
                                  (char= character #\_))
                              character
                              #\_)
            do (when (and firstp (digit-char-p emitted))
                 (write-char #\_ stream))
               (write-char emitted stream)))))

(defun msl-type-name (type &optional source-form)
  (case (shader:shader-type-name (shader:find-shader-type type source-form))
    (:bool "bool")
    (:float "float")
    (:uint "uint")
    (:uint64 "ulong")
    (:vec2 "float2")
    (:vec3 "float3")
    (:vec4 "float4")
    (:uvec2 "uint2")
    (:uvec3 "uint3")
    (:uvec4 "uint4")
    (:texture-2d "texture2d<float>")
    (:depth-texture-2d "depth2d<float>")
    (:uint-texture-2d "texture2d<uint>")
    (:sampler "sampler")
    (otherwise
     (error 'shader:shader-language-error
            :form source-form :reason :unsupported-msl-type
            :details (shader:shader-type-name type)))))

(defun msl-structure-name-for (name &optional suffix)
  (let ((capitalize-next-p t))
    (with-output-to-string (stream)
      (loop for character across (string-downcase (string name))
            if (alphanumericp character)
              do (write-char (if capitalize-next-p
                                 (char-upcase character)
                                 character)
                             stream)
                 (setf capitalize-next-p nil)
            else
              do (setf capitalize-next-p t))
      (when suffix
        (write-string suffix stream)))))

(defun msl-float-literal (value)
  (let* ((raw (string-downcase
               (write-to-string (coerce value 'single-float))))
         (normalized
           (map 'string
                (lambda (character)
                  (if (find character "sfdl" :test #'char=)
                      #\e
                      character))
                raw)))
    (format nil "~A~Af"
            normalized
            (if (or (find #\. normalized) (find #\e normalized)) "" ".0"))))

(defun msl-semantic-words (name)
  (substitute #\Space #\- (string-downcase (symbol-name name))))

(defun msl-factor-description (factor)
  (let ((name (msl-semantic-words (car factor)))
        (power (cdr factor)))
    (case power
      (1 name)
      (2 (format nil "~A squared" name))
      (3 (format nil "~A cubed" name))
      (otherwise (format nil "~A to the ~A power" name power)))))

(defun msl-factor-product-description (factors)
  (format nil "~{~A~^ times ~}" (mapcar #'msl-factor-description factors)))

(defun msl-tensor-description (order)
  (case order
    (0 "scalar")
    (1 "vector")
    (otherwise (format nil "tensor of order ~D" order))))

(defun msl-character-description (specification)
  (case (math:quantity-specification-character specification)
    (:point "point-valued")
    (:absolute
     (if (math:quantity-specification-non-negative-p specification)
         "non-negative absolute"
         "absolute"))
    (:difference "difference-valued")))

(defun msl-quantity-predicate (specification)
  (let* ((kind (math:quantity-specification-kind specification))
         (unit-factors
           (math:unit-expression-factors
            (math:quantity-specification-unit specification)))
         (dimension-factors
           (math:dimension-factors
            (math:quantity-specification-dimension specification))))
    (with-output-to-string (stream)
      (format stream "a ~A ~A"
              (msl-character-description specification)
              (msl-tensor-description
               (math:quantity-specification-tensor-order specification)))
      (when kind
        (format stream " in the ~A kind" (msl-semantic-words kind)))
      (cond
        ((and (null unit-factors) (null dimension-factors))
         (write-string ", unitless and dimensionless" stream))
        (t
         (if unit-factors
             (format stream ", measured in ~A units"
                     (msl-factor-product-description unit-factors))
             (write-string ", unitless" stream))
         (if dimension-factors
             (format stream ", with ~A dimension"
                     (msl-factor-product-description dimension-factors))
             (write-string ", and dimensionless" stream)))))))

(defun msl-capitalize-sentence (text)
  (if (plusp (length text))
      (concatenate 'string
                   (string (char-upcase (char text 0)))
                   (subseq text 1))
      text))

(defun msl-quantity-sentence (specification)
  (let ((name (math:quantity-specification-name specification)))
    (format nil "~A is ~A."
            (if name
                (msl-capitalize-sentence (msl-semantic-words name))
                "This value")
            (msl-quantity-predicate specification))))

(defun msl-lane-name (positions)
  (coerce (mapcar (lambda (position) (char "xyzw" position)) positions)
          'string))

(defun msl-layout-sentences (layout &key sampled-p)
  (let ((occupied nil)
        (sentences nil))
    (dolist (projection (math:quantity-layout-projections layout))
      (let* ((positions (math:quantity-projection-positions projection))
             (specification
               (math:quantity-projection-specification projection))
             (name (math:quantity-specification-name specification))
             (lanes (msl-lane-name positions)))
        (setf occupied (nconc (copy-list positions) occupied))
        (push
         (format nil "The ~A~A ~A ~A ~A~A."
                 (if sampled-p "sampled " "")
                 lanes
                 (if (= (length positions) 1) "lane" "lanes")
                 (if (= (length positions) 1) "holds" "hold")
                 (if name (msl-semantic-words name) "an unnamed quantity")
                 (format nil " as ~A" (msl-quantity-predicate specification)))
         sentences)))
    (let ((uncovered
            (loop for position below (math:quantity-layout-extent layout)
                  unless (member position occupied)
                    collect position)))
      (when uncovered
        (push
         (format nil "The ~A~A ~A ~A no quantity annotation."
                 (if sampled-p "sampled " "")
                 (msl-lane-name uncovered)
                 (if (= (length uncovered) 1) "lane" "lanes")
                 (if (= (length uncovered) 1) "has" "have"))
         sentences)))
    (nreverse sentences)))

(defgeneric msl-origin-quantity-specification (origin)
  (:documentation "Return the homogeneous quantity carried by ORIGIN."))

(defmethod msl-origin-quantity-specification ((origin t))
  nil)

(defmethod msl-origin-quantity-specification
    ((origin shader:shader-variable-declaration))
  (shader:shader-declaration-quantity-specification origin))

(defmethod msl-origin-quantity-specification ((origin shader:shader-binding))
  (shader:shader-expression-quantity-specification
   (shader:shader-binding-expression origin)))

(defmethod msl-origin-quantity-specification
    ((origin shader:shader-output-assignment))
  (shader:shader-expression-quantity-specification
   (shader:shader-assignment-value origin)))

(defmethod msl-origin-quantity-specification ((origin shader:shader-resource))
  (shader:shader-resource-sample-quantity-specification origin))

(defgeneric msl-origin-quantity-layout (origin)
  (:documentation "Return the component quantity layout carried by ORIGIN."))

(defmethod msl-origin-quantity-layout ((origin t))
  nil)

(defmethod msl-origin-quantity-layout
    ((origin shader:shader-variable-declaration))
  (shader:shader-declaration-quantity-layout origin))

(defmethod msl-origin-quantity-layout ((origin shader:shader-binding))
  (shader:shader-expression-quantity-layout
   (shader:shader-binding-expression origin)))

(defmethod msl-origin-quantity-layout
    ((origin shader:shader-output-assignment))
  (shader:shader-expression-quantity-layout
   (shader:shader-assignment-value origin)))

(defmethod msl-origin-quantity-layout ((origin shader:shader-resource))
  (shader:shader-resource-sample-quantity-layout origin))

(defun msl-semantic-sentences (origin &key sampled-p unannotated-p)
  (let ((specification (msl-origin-quantity-specification origin))
        (layout (msl-origin-quantity-layout origin)))
    (cond
      (specification (list (msl-quantity-sentence specification)))
      (layout (msl-layout-sentences layout :sampled-p sampled-p))
      (unannotated-p (list "This numeric value has no quantity annotation.")))))

(defun write-msl-semantic-comments
    (origin stream indentation &key sampled-p unannotated-p)
  (dolist (sentence
           (msl-semantic-sentences
            origin :sampled-p sampled-p :unannotated-p unannotated-p))
    (format stream "~A// ~A~%" indentation sentence)))

(defun note-msl-occurrence (context expression text)
  (let ((occurrence
          (make-instance 'msl-source-occurrence
                         :expression expression :text text)))
    (push occurrence
          (gethash expression (msl-context-expression-occurrences context)))
    (setf (gethash occurrence (msl-context-occurrence-expression context))
          expression)
    occurrence))

(defun msl-occurrence-text (occurrence)
  (msl-source-occurrence-text occurrence))

(defgeneric lower-msl-expression (context expression)
  (:documentation
   "Render one shader EXPRESSION and retain a source occurrence for it."))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-literal))
  (note-msl-occurrence
   context expression (msl-float-literal (shader:shader-literal-value expression))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-reference))
  (let* ((target (shader:shader-reference-target expression))
         (text (gethash target (msl-context-references context))))
    (cond (text
           (note-msl-occurrence context expression text))
          ((typep target 'shader:shader-function-parameter-binding)
           (let ((argument
                   (lower-msl-expression
                    context (shader:shader-binding-expression target))))
             (note-msl-occurrence
              context expression (msl-occurrence-text argument))))
          (t
           (error 'shader:shader-language-error
                  :form (shader:shader-expression-source-form expression)
                  :reason :unsupported-msl-reference
                  :details (shader:shader-object-name target))))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-payload-element))
  (let ((index
          (lower-msl-expression
           context (shader:shader-payload-element-index expression))))
    (note-msl-occurrence
     context expression
     (format nil "payload.~A[~A]"
             (msl-identifier
              (shader:shader-object-name
               (shader:shader-payload-element-field expression)))
             (msl-occurrence-text index)))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-buffer-element))
  (let ((index
          (lower-msl-expression
           context (shader:shader-buffer-element-index expression))))
    (note-msl-occurrence
     context expression
     (format nil "~A[~A]"
             (msl-identifier
              (shader:shader-object-name
               (shader:shader-buffer-element-buffer expression)))
             (msl-occurrence-text index)))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-call))
  (shader:lower-shader-call (shader:shader-call-operator expression)
                         context expression))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-function-call))
  (let ((outer-statements (drain-msl-pending-statements context))
        (local-statements nil)
        (saved-references nil))
    (unwind-protect
         (progn
           (dolist (binding (shader:shader-function-call-bindings expression))
             (unless (or (typep binding
                                'shader:shader-function-parameter-binding)
                         (nth-value 1
                           (gethash binding
                                    (msl-context-references context))))
               (multiple-value-bind (old-reference old-reference-p)
                   (gethash binding (msl-context-references context))
                 (push (list binding old-reference old-reference-p)
                       saved-references))
               (let* ((binding-expression
                        (shader:shader-binding-expression binding))
                      (name
                        (msl-identifier (shader:shader-object-name binding)))
                      (value
                        (lower-msl-expression context binding-expression)))
                 (setf local-statements
                       (nconc local-statements
                              (drain-msl-pending-statements context))
                       (gethash binding (msl-context-references context)) name)
                 (setf local-statements
                       (nconc
                        local-statements
                        (list
                         (make-instance
                          'msl-variable-statement
                          :type (msl-type-name
                                 (shader:shader-expression-type
                                  binding-expression))
                          :name name :value value :origin binding)))))))
           (let ((result
                   (lower-msl-expression
                    context (shader:shader-function-call-result expression))))
             (setf local-statements
                   (nconc local-statements
                          (drain-msl-pending-statements context))
                   (msl-context-pending-statements context)
                   (nconc outer-statements local-statements))
             (note-msl-occurrence
              context expression (msl-occurrence-text result))))
      (dolist (saved saved-references)
        (destructuring-bind (binding old-reference old-reference-p) saved
          (if old-reference-p
              (setf (gethash binding (msl-context-references context))
                    old-reference)
              (remhash binding (msl-context-references context))))))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-conditional))
  (let ((condition
          (lower-msl-expression
           context (lang:arithmetic-conditional-condition expression)))
        (consequent
          (lower-msl-expression
           context (lang:arithmetic-conditional-consequent expression)))
        (alternative
          (lower-msl-expression
           context (lang:arithmetic-conditional-alternative expression))))
    (note-msl-occurrence
     context expression
     (format nil "(~A ? ~A : ~A)"
             (msl-occurrence-text condition)
             (msl-occurrence-text consequent)
             (msl-occurrence-text alternative)))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-counted-fold))
  (let* ((ordinal (incf (msl-context-fold-counter context)))
         (state-name (format nil "fold_state_~D" ordinal))
         (index-name (format nil "fold_index_~D" ordinal))
         (count
           (lower-msl-expression
            context (lang:arithmetic-counted-fold-count expression)))
         (initial
           (lower-msl-expression
            context (lang:arithmetic-counted-fold-initial expression)))
         (index-binding
           (lang:arithmetic-counted-fold-index-binding expression))
         (state-binding
           (lang:arithmetic-counted-fold-state-binding expression)))
    (multiple-value-bind (old-index old-index-p)
        (gethash index-binding (msl-context-references context))
      (multiple-value-bind (old-state old-state-p)
          (gethash state-binding (msl-context-references context))
        (setf (gethash index-binding (msl-context-references context))
              index-name
              (gethash state-binding (msl-context-references context))
              state-name)
        (let* ((preheader-statements
                 (drain-msl-pending-statements context))
               (until-expression
                 (lang:arithmetic-counted-fold-until expression))
               (until
                 (and until-expression
                      (lower-msl-expression context until-expression)))
               (until-statements
                 (and until (drain-msl-pending-statements context)))
               (local-statements nil))
          (dolist (binding
                   (lang:arithmetic-counted-fold-bindings expression))
            (let* ((binding-expression
                     (shader:shader-binding-expression binding))
                   (name (msl-identifier (shader:shader-object-name binding)))
                   (value (lower-msl-expression context binding-expression)))
              (setf local-statements
                    (nconc local-statements
                           (drain-msl-pending-statements context)))
              (setf (gethash binding (msl-context-references context)) name)
              (setf local-statements
                    (nconc
                     local-statements
                     (list
                      (make-instance
                       'msl-variable-statement
                       :type (msl-type-name
                              (shader:shader-expression-type binding-expression))
                       :name name :value value :origin binding))))))
          (let ((update
                  (lower-msl-expression
                   context (lang:arithmetic-counted-fold-update expression))))
            (setf local-statements
                  (nconc local-statements
                         (drain-msl-pending-statements context)))
            (setf (msl-context-pending-statements context)
                  (nconc preheader-statements
                         (list
                          (make-instance
                           'msl-counted-fold-statement
                           :type (msl-type-name
                                  (shader:shader-expression-type expression))
                           :state-name state-name :initial initial
                           :index-name index-name
                           :index-type
                           (msl-type-name
                            (shader:shader-expression-type
                             (lang:arithmetic-counted-fold-count expression)))
                           :count count
                           :bindings local-statements
                           :update update
                           :until-bindings until-statements
                           :until until
                           :origin expression))))
            (dolist (binding
                     (lang:arithmetic-counted-fold-bindings expression))
              (remhash binding (msl-context-references context)))
            (if old-index-p
                (setf (gethash index-binding
                               (msl-context-references context))
                      old-index)
                (remhash index-binding (msl-context-references context)))
            (if old-state-p
                (setf (gethash state-binding
                               (msl-context-references context))
                      old-state)
                (remhash state-binding (msl-context-references context)))))))
    (note-msl-occurrence context expression state-name)))

(defgeneric lower-msl-shader-map-application
    (definition context application)
  (:documentation "Render one semantic shader-map application for MSL."))

(defmethod lower-msl-shader-map-application
    (definition (context msl-lowering-context)
     (application shader:shader-map-application))
  (declare (ignore context))
  (error 'shader:shader-language-error
         :form (shader:shader-expression-source-form application)
         :reason :unsupported-msl-shader-map
         :details (class-name (class-of definition))))

(defmethod lower-msl-shader-map-application
    ((definition shader:shader-projective-map-definition)
     (context msl-lowering-context)
     (application shader:shader-map-application))
  (declare (ignore definition))
  (let* ((point
           (msl-occurrence-text
            (lower-msl-expression
             context (shader:shader-map-application-point application))))
           (rows
             (mapcar (lambda (row)
                       (msl-occurrence-text
                        (lower-msl-expression context row)))
                     (shader:shader-map-application-rows application)))
           (homogeneous (format nil "float4(~A, 1.0f)" point))
           (clip-components
             (mapcar (lambda (row)
                       (format nil "dot(~A, ~A)" row homogeneous))
                     rows)))
    (note-msl-occurrence
     context application
     (format nil "float4(~{~A~^, ~})" clip-components))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-map-projection))
  (let* ((application (shader:shader-map-projection-application expression))
         (definition (shader:shader-map-application-definition application))
         (point
           (msl-occurrence-text
            (lower-msl-expression
             context (shader:shader-map-application-point application))))
         (rows
           (mapcar (lambda (row)
                     (msl-occurrence-text
                      (lower-msl-expression context row)))
                   (shader:shader-map-application-rows application)))
         (homogeneous (format nil "float4(~A, 1.0f)" point))
         (clip-components
           (mapcar (lambda (row)
                     (format nil "dot(~A, ~A)" row homogeneous))
                   rows))
           (normalized
             (format nil "(float3(~{~A~^, ~}) / ~A)"
                     (subseq clip-components 0 3)
                     (fourth clip-components)))
           (scale
             (format nil "float3(~{~A~^, ~})"
                     (mapcar #'msl-float-literal
                             (shader:shader-projective-map-coordinate-scale
                              definition))))
           (offset
             (format nil "float3(~{~A~^, ~})"
                     (mapcar #'msl-float-literal
                             (shader:shader-projective-map-coordinate-offset
                              definition)))))
    (note-msl-occurrence
     context expression
     (format nil "((~A * ~A) + ~A)" normalized scale offset))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-map-application))
  (lower-msl-shader-map-application
   (shader:shader-map-application-definition expression) context expression))

(defun lower-msl-quantity-boundary (context expression operand)
  (let ((lowered (lower-msl-expression context operand)))
    (note-msl-occurrence context expression (msl-occurrence-text lowered))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-interpretation))
  (lower-msl-quantity-boundary
   context expression (shader:shader-interpretation-operand expression)))

(defmethod lower-msl-expression
    ((context msl-lowering-context)
     (expression shader:shader-quantity-construction))
  (lower-msl-quantity-boundary
   context expression (shader:shader-quantity-construction-operand expression)))

(defmethod lower-msl-expression
    ((context msl-lowering-context)
     (expression shader:shader-quantity-assumption))
  (lower-msl-quantity-boundary
   context expression (shader:shader-quantity-assumption-operand expression)))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-representation))
  (lower-msl-quantity-boundary
   context expression (shader:shader-representation-operand expression)))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-unit-conversion))
  (let* ((operand
           (lower-msl-expression
            context (shader:shader-unit-conversion-operand expression)))
         (factor (shader:shader-unit-conversion-factor expression))
         (text
           (if (= factor 1)
               (msl-occurrence-text operand)
               (format nil "(~A * ~A)"
                       (msl-occurrence-text operand)
                       (msl-float-literal factor)))))
    (note-msl-occurrence context expression text)))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression shader:shader-expression))
  (declare (ignore context))
  (error 'shader:shader-language-error
         :form (shader:shader-expression-source-form expression)
         :reason :unsupported-msl-expression
         :details (class-name (class-of expression))))

(defun lower-msl-operands (context expression)
  (mapcar (lambda (operand)
            (lower-msl-expression context operand))
          (shader:shader-call-operands expression)))

(defun lower-msl-infix-call (context expression operator)
  (let ((operands (mapcar #'msl-occurrence-text
                          (lower-msl-operands context expression))))
    (note-msl-occurrence
     context expression
     (cond
       ((and (string= operator "-") (= (length operands) 1))
        (format nil "(-~A)" (first operands)))
       ((= (length operands) 1)
        (format nil "(~A)" (first operands)))
       (t
        (reduce (lambda (left right)
                  (format nil "(~A ~A ~A)" left operator right))
                (rest operands) :initial-value (first operands)))))))

(defun lower-msl-function-call (context expression name)
  (let ((operands (mapcar #'msl-occurrence-text
                          (lower-msl-operands context expression))))
    (note-msl-occurrence
     context expression (format nil "~A(~{~A~^, ~})" name operands))))

(defun lower-msl-chained-function-call (context expression name)
  (let ((operands (mapcar #'msl-occurrence-text
                          (lower-msl-operands context expression))))
    (note-msl-occurrence
     context expression
     (reduce (lambda (left right)
               (format nil "~A(~A, ~A)" name left right))
             (rest operands) :initial-value (first operands)))))

(defmethod shader:lower-shader-call
    (operator (context msl-lowering-context) expression)
  (error 'shader:shader-language-error
         :form (shader:shader-expression-source-form expression)
         :reason :unsupported-msl-operator :details operator))

(defmacro define-msl-infix-operator (operator text)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (context msl-lowering-context)
        (expression shader:shader-call))
     (declare (ignore operator))
     (lower-msl-infix-call context expression ,text)))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:ldb))
     (context msl-lowering-context)
     (expression shader:shader-bit-field-call))
  "Lower LDB to a logical shift and mask in the operand's own width."
  (declare (ignore operator))
  (let* ((operands (shader:shader-call-operands expression))
         (value (msl-occurrence-text
                 (lower-msl-expression context (first operands))))
         (size (shader:shader-bit-field-size expression))
         (position (shader:shader-bit-field-position expression))
         (width (shader:shader-type-bit-width
                 (shader:shader-expression-type expression)))
         (suffix (if (= width 64) "ul" "u"))
         (shift (cond (position (format nil "~D~A" position suffix))
                      ((= width 64)
                       (format nil "ulong(~A)"
                               (msl-occurrence-text
                                (lower-msl-expression
                                 context (second operands)))))
                      (t (msl-occurrence-text
                          (lower-msl-expression
                           context (second operands)))))))
    (note-msl-occurrence
     context expression
     (cond ((and position (zerop position) (= size width))
            (format nil "(~A)" value))
           ((and position (= (+ size position) width))
            (format nil "(~A >> ~A)" value shift))
           (t
            (format nil "((~A >> ~A) & 0x~X~A)"
                    value shift (1- (ash 1 size)) suffix))))))

(define-msl-infix-operator + "+")
(define-msl-infix-operator - "-")
(define-msl-infix-operator * "*")
(define-msl-infix-operator / "/")
(define-msl-infix-operator mod "%")
(define-msl-infix-operator < "<")
(define-msl-infix-operator <= "<=")
(define-msl-infix-operator > ">")
(define-msl-infix-operator >= ">=")
(define-msl-infix-operator = "==")

(defmacro define-msl-function-operator (operator name)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (context msl-lowering-context)
        (expression shader:shader-call))
     (declare (ignore operator))
     (lower-msl-function-call context expression ,name)))

(define-msl-function-operator shader:dot "dot")
(define-msl-function-operator shader:mix "mix")
(define-msl-function-operator abs "abs")
(define-msl-function-operator signum "sign")
(define-msl-function-operator sqrt "sqrt")
(define-msl-function-operator shader:derivative-x "dfdx")
(define-msl-function-operator shader:derivative-y "dfdy")
(define-msl-function-operator expt "pow")
(define-msl-function-operator shader:clamp "clamp")
(define-msl-function-operator shader:smoothstep "smoothstep")
(define-msl-function-operator shader:step "step")
(define-msl-function-operator shader:normalize "normalize")
(define-msl-function-operator floor "floor")
(define-msl-function-operator shader:fract "fract")
(define-msl-function-operator sin "sin")
(define-msl-function-operator cos "cos")
(define-msl-function-operator exp "exp")
(define-msl-function-operator log "log")

(defmacro define-msl-chained-function-operator (operator name)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (context msl-lowering-context)
        (expression shader:shader-call))
     (declare (ignore operator))
     (lower-msl-chained-function-call context expression ,name)))

(define-msl-chained-function-operator min "min")
(define-msl-chained-function-operator max "max")

(defun lower-msl-vector-constructor (context expression)
  (let ((operands (mapcar #'msl-occurrence-text
                          (lower-msl-operands context expression))))
    (note-msl-occurrence
     context expression
     (format nil "~A(~{~A~^, ~})"
             (msl-type-name (shader:shader-expression-type expression)
                            (shader:shader-expression-source-form expression))
             operands))))

(defmacro define-msl-vector-constructor (operator)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (context msl-lowering-context)
        (expression shader:shader-call))
     (declare (ignore operator))
     (lower-msl-vector-constructor context expression)))

(define-msl-vector-constructor shader:vec2)
(define-msl-vector-constructor shader:vec3)
(define-msl-vector-constructor shader:vec4)
(define-msl-vector-constructor shader:uvec2)
(define-msl-vector-constructor shader:uvec3)
(define-msl-vector-constructor shader:uvec4)

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:uint))
     (context msl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (lower-msl-function-call context expression "uint"))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:uint64))
     (context msl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (lower-msl-function-call context expression "ulong"))

(defmethod shader:lower-shader-call
    ((operator (eql 'float))
     (context msl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (lower-msl-function-call context expression "float"))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:swizzle))
     (context msl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (let* ((operand
           (lower-msl-expression
            context (first (shader:shader-call-operands expression))))
         (components
           (string-downcase
            (string (first (shader:shader-call-parameters expression))))))
    (note-msl-occurrence
     context expression
     (format nil "~A.~A" (msl-occurrence-text operand) components))))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:sample))
     (context msl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (destructuring-bind (texture sampler coordinate)
      (shader:shader-call-operands expression)
    (let* ((lowered
             (mapcar (lambda (operand)
                       (lower-msl-expression context operand))
                     (list texture sampler coordinate)))
           (sample
             (format nil "~A.sample(~A, ~A)"
                     (msl-occurrence-text (first lowered))
                     (msl-occurrence-text (second lowered))
                     (msl-occurrence-text (third lowered))))
           (text
             (if (shader:shader-type-image-depth-p
                  (shader:shader-expression-type texture))
                 (format nil "float4(~A)" sample)
                 sample)))
      (note-msl-occurrence context expression text))))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:sample-compare))
     (context msl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (let ((operands (mapcar #'msl-occurrence-text
                          (lower-msl-operands context expression))))
    (note-msl-occurrence
     context expression
     (destructuring-bind (texture sampler coordinate depth-reference) operands
       (format nil "~A.sample_compare(~A, ~A, ~A)"
               texture sampler coordinate depth-reference)))))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:texel-load))
     (context msl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (destructuring-bind (texture coordinate)
      (mapcar #'msl-occurrence-text
              (lower-msl-operands context expression))
    (note-msl-occurrence
     context expression (format nil "~A.read(~A)" texture coordinate))))

(defun msl-interface-attribute (stage declaration)
  (let ((direction (shader:shader-interface-direction declaration))
        (location (shader:shader-interface-location declaration))
        (built-in (shader:shader-interface-built-in declaration))
        (interpolation (shader:shader-interface-interpolation declaration))
        (source-form (shader:shader-object-source-form declaration)))
    (cond
      ((and (eq direction :output) (eq built-in :position))
       "[[position]]")
      ((and (eq stage :vertex) (eq direction :input)
            (eq built-in :vertex-index))
       "[[vertex_id]]")
      ((and (member stage '(:task :mesh)) (eq direction :input))
       (case built-in
         (:local-invocation-index "[[thread_index_in_threadgroup]]")
         (:local-invocation-id "[[thread_position_in_threadgroup]]")
         (:workgroup-id "[[threadgroup_position_in_grid]]")
         (:num-workgroups "[[threadgroups_per_grid]]")
         (:workgroup-size "[[threads_per_threadgroup]]")
         (otherwise
          (error 'shader:shader-language-error
                 :form source-form :reason :unsupported-msl-built-in
                 :details (list direction built-in)))))
      (built-in
       (error 'shader:shader-language-error
              :form source-form :reason :unsupported-msl-built-in
              :details (list direction built-in)))
      ((and (eq stage :vertex) (eq direction :input))
       (format nil "[[attribute(~D)]]" location))
      ((and (eq stage :fragment) (eq direction :output))
       (format nil "[[color(~D)]]" location))
      (t
       (format nil "[[user(locn~D)~@[, ~A~]]]"
               location
               (case interpolation
                 (:flat "flat")
                 (otherwise nil)))))))

(defun msl-interface-structure (name suffix stage declarations)
  (make-instance
   'msl-structure-declaration
   :name (msl-structure-name-for name suffix)
   :fields
   (mapcar
    (lambda (declaration)
      (make-instance
       'msl-field
       :type (msl-type-name (shader:shader-declaration-type declaration)
                            (shader:shader-object-source-form declaration))
       :name (msl-identifier (shader:shader-object-name declaration))
       :attribute (msl-interface-attribute stage declaration)
       :origin declaration))
    declarations)))

(defun msl-built-in-input-parameter (stage declaration)
  (make-instance
   'msl-parameter
   :type (msl-type-name (shader:shader-declaration-type declaration)
                        (shader:shader-object-source-form declaration))
   :name (msl-identifier (shader:shader-object-name declaration))
   :attribute (msl-interface-attribute stage declaration)
   :origin declaration))

(defun msl-uniform-structure (resource)
  (make-instance
   'msl-structure-declaration
   :name (msl-structure-name-for (shader:shader-object-name resource))
   :fields
   (mapcar
    (lambda (member)
      (make-instance
       'msl-field
       :type (msl-type-name (shader:shader-declaration-type member)
                            (shader:shader-object-source-form member))
       :name (msl-identifier (shader:shader-object-name member))
       :attribute nil :origin member))
    (shader:shader-uniform-block-members resource))))

(defun msl-task-payload-structure (payload)
  (make-instance
   'msl-structure-declaration
   :name (msl-structure-name-for (shader:shader-object-name payload))
   :fields
   (mapcar
    (lambda (field)
      (make-instance
       'msl-field
       :type (msl-type-name (shader:shader-declaration-type field)
                            (shader:shader-object-source-form field))
       :name (msl-identifier (shader:shader-object-name field))
       :attribute nil :origin field
       :array-length (shader:shader-task-payload-field-element-count field)))
    (shader:shader-task-payload-fields payload))))

(defun msl-resource-parameter (resource)
  (unless (zerop (shader:shader-resource-descriptor-set resource))
    (error 'shader:shader-language-error
           :form (shader:shader-object-source-form resource)
           :reason :unsupported-msl-descriptor-set
           :details (shader:shader-resource-descriptor-set resource)))
  (let* ((name (msl-identifier (shader:shader-object-name resource)))
         (binding (shader:shader-resource-binding resource))
         (type (shader:shader-declaration-type resource))
         (kind (shader:shader-type-opaque-kind type)))
    (case kind
      (:uniform-block
       (make-instance
        'msl-parameter
        :type (format nil "constant ~A&"
                      (msl-structure-name-for
                       (shader:shader-object-name resource)))
        :name name :attribute (format nil "[[buffer(~D)]]" binding)
        :origin resource))
      (:storage-buffer
       (make-instance
        'msl-parameter
        :type (format nil "const device ~A*"
                      (msl-type-name
                       (shader:shader-storage-buffer-element-type resource)
                       (shader:shader-object-source-form resource)))
        :name name :attribute (format nil "[[buffer(~D)]]" binding)
        :origin resource))
      (:texture-2d
       (make-instance
        'msl-parameter
        :type (msl-type-name type (shader:shader-object-source-form resource))
        :name name :attribute (format nil "[[texture(~D)]]" binding)
        :origin resource))
      (:sampler
       (make-instance
        'msl-parameter
        :type "sampler" :name name
        :attribute (format nil "[[sampler(~D)]]" binding)
        :origin resource))
      (otherwise
       (error 'shader:shader-language-error
              :form (shader:shader-object-source-form resource)
              :reason :unsupported-msl-resource :details kind)))))

(defun register-msl-declaration-references
    (context specification input-parameter-name)
  (dolist (input (shader:shader-specification-inputs specification))
    (setf (gethash input (msl-context-references context))
          (if (shader:shader-interface-built-in input)
              (msl-identifier (shader:shader-object-name input))
              (format nil "~A.~A" input-parameter-name
                      (msl-identifier (shader:shader-object-name input))))))
  (dolist (resource (shader:shader-specification-resources specification))
    (let ((resource-name (msl-identifier (shader:shader-object-name resource))))
      (setf (gethash resource (msl-context-references context)) resource-name)
      (when (typep resource 'shader:shader-uniform-block)
        (dolist (member (shader:shader-uniform-block-members resource))
          (setf (gethash member (msl-context-references context))
                (format nil "~A.~A" resource-name
                        (msl-identifier (shader:shader-object-name member))))))))
  (let ((payload (shader:shader-specification-task-payload specification)))
    (when payload
      (dolist (field (shader:shader-task-payload-fields payload))
        (unless (shader:shader-task-payload-field-element-count field)
          (setf (gethash field (msl-context-references context))
                (format nil "payload.~A"
                        (msl-identifier (shader:shader-object-name field))))))))
  context)

(defgeneric write-msl-declaration (declaration stream))

(defmethod write-msl-declaration
    ((declaration msl-structure-declaration) stream)
  (format stream "struct ~A {~%" (msl-structure-name declaration))
  (dolist (field (msl-structure-fields declaration))
    (write-msl-semantic-comments
     (msl-field-origin field) stream "  " :unannotated-p t)
    (format stream "  ~A ~A~A~@[ ~A~];~%"
            (msl-field-type field)
            (msl-field-name field)
            (if (msl-field-array-length field)
                (format nil "[~D]" (msl-field-array-length field))
                "")
            (msl-field-attribute field)))
  (format stream "};~%"))

(defgeneric write-msl-statement (statement stream))

(defvar *msl-statement-indentation* 1)

(defun write-msl-indent (stream &optional (extra 0))
  (loop repeat (+ *msl-statement-indentation* extra)
        do (write-string "  " stream)))

(defun msl-position-adjusted-text (declaration value)
  (if (eq :position (shader:shader-interface-built-in declaration))
      ;; The shared camera graph intentionally retains Vulkan's
      ;; framebuffer-oriented clip Y. Metal owns the target conversion.
      (format nil "float4((~A).x, -(~A).y, (~A).z, (~A).w)"
              value value value value)
      value))

(defun msl-control-condition-text (condition)
  (if (and (plusp (length condition))
           (char= #\( (char condition 0))
           (char= #\) (char condition (1- (length condition)))))
      condition
      (format nil "(~A)" condition)))

(defmethod write-msl-statement ((statement msl-variable-statement) stream)
  (write-msl-indent stream)
  (write-msl-semantic-comments
   (msl-variable-statement-origin statement) stream ""
   :unannotated-p t)
  (write-msl-indent stream)
  (format stream "~A ~A = ~A;~%"
          (msl-variable-statement-type statement)
          (msl-variable-statement-name statement)
          (msl-occurrence-text (msl-variable-statement-value statement))))

(defmethod write-msl-statement ((statement msl-output-statement) stream)
  (let* ((origin (msl-output-statement-origin statement))
         (output (shader:shader-assignment-output origin))
         (value (msl-occurrence-text
                 (msl-output-statement-value statement)))
         (text (msl-position-adjusted-text output value)))
    (write-msl-indent stream)
    (format stream "result.~A = ~A;~%"
            (msl-output-statement-field statement) text)))

(defmethod write-msl-statement ((statement msl-if-statement) stream)
  (write-msl-indent stream)
  (format stream "if ~A {~%"
          (msl-control-condition-text
           (msl-occurrence-text (msl-if-statement-condition statement))))
  (let ((*msl-statement-indentation* (1+ *msl-statement-indentation*)))
    (dolist (child (msl-if-statement-statements statement))
      (write-msl-statement child stream)))
  (write-msl-indent stream)
  (format stream "}~%"))

(defmethod write-msl-statement
    ((statement msl-mesh-output-counts-statement) stream)
  (write-msl-indent stream)
  (format stream "// publishes ~A vertices and ~A primitives~%"
          (msl-occurrence-text
           (msl-mesh-output-counts-vertex-count statement))
          (msl-occurrence-text
           (msl-mesh-output-counts-primitive-count statement)))
  (write-msl-indent stream)
  (format stream "if (~A == 0u)~%"
          (msl-mesh-output-counts-lane statement))
  (write-msl-indent stream 1)
  (format stream "mesh_out.set_primitive_count(~A);~%"
          (msl-occurrence-text
           (msl-mesh-output-counts-primitive-count statement))))

(defmethod write-msl-statement
    ((statement msl-mesh-vertex-statement) stream)
  (write-msl-indent stream)
  (format stream "mesh_out.set_vertex(~A, ~A{~{~A~^, ~}});~%"
          (msl-occurrence-text (msl-mesh-vertex-index statement))
          (msl-mesh-vertex-type statement)
          (loop for (declaration . occurrence)
                  in (msl-mesh-vertex-values statement)
                collect
                (msl-position-adjusted-text
                 declaration (msl-occurrence-text occurrence)))))

(defmethod write-msl-statement
    ((statement msl-mesh-primitive-statement) stream)
  (let* ((primitive-index
           (msl-occurrence-text (msl-mesh-primitive-index statement)))
         (indices (msl-occurrence-text (msl-mesh-primitive-indices statement)))
         (components
           (ecase (msl-mesh-primitive-topology statement)
             (:points '(nil))
             (:lines '("x" "y"))
             (:triangles '("x" "y" "z"))))
         (width (length components)))
    (loop for component in components
          for offset from 0
          do (write-msl-indent stream)
             (format stream "mesh_out.set_index((~A * ~Du) + ~Du, ~A~A);~%"
                     primitive-index width offset indices
                     (if component (format nil ".~A" component) "")))
    (when (msl-mesh-primitive-type statement)
      (write-msl-indent stream)
      (format stream "mesh_out.set_primitive(~A, ~A{~{~A~^, ~}});~%"
              primitive-index
              (msl-mesh-primitive-type statement)
              (mapcar (lambda (pair)
                        (msl-occurrence-text (cdr pair)))
                      (msl-mesh-primitive-values statement))))))

(defmethod write-msl-statement
    ((statement msl-task-payload-store-statement) stream)
  (write-msl-indent stream)
  (format stream "payload.~A~A = ~A;~%"
          (msl-task-payload-store-field statement)
          (if (msl-task-payload-store-index statement)
              (format nil "[~A]"
                      (msl-occurrence-text
                       (msl-task-payload-store-index statement)))
              "")
          (msl-occurrence-text (msl-task-payload-store-value statement))))

(defmethod write-msl-statement
    ((statement msl-emit-mesh-workgroups-statement) stream)
  (write-msl-indent stream)
  (format stream "if (~A == 0u)~%"
          (msl-emit-mesh-workgroups-lane statement))
  (write-msl-indent stream 1)
  (format stream "mesh_grid.set_threadgroups_per_grid(~A);~%"
          (msl-occurrence-text
           (msl-emit-mesh-workgroups-counts statement))))

(defmethod write-msl-statement
    ((statement msl-counted-fold-statement) stream)
  (write-msl-indent stream)
  (format stream "~A ~A = ~A;~%"
          (msl-counted-fold-statement-type statement)
          (msl-counted-fold-statement-state-name statement)
          (msl-occurrence-text
           (msl-counted-fold-statement-initial statement)))
  (let* ((index-type (msl-counted-fold-statement-index-type statement))
         (unsigned-p (string= index-type "uint")))
    (write-msl-indent stream)
    (format stream "for (~A ~A = ~A; ~A < ~A; ~A += ~A) {~%"
            index-type
            (msl-counted-fold-statement-index-name statement)
            (if unsigned-p "0u" "0.0f")
            (msl-counted-fold-statement-index-name statement)
            (msl-occurrence-text (msl-counted-fold-statement-count statement))
            (msl-counted-fold-statement-index-name statement)
            (if unsigned-p "1u" "1.0f")))
  (let ((until (msl-counted-fold-statement-until statement)))
    (when until
      (let ((*msl-statement-indentation* (1+ *msl-statement-indentation*)))
        (dolist (binding (msl-counted-fold-statement-until-bindings statement))
          (write-msl-statement binding stream)))
      (write-msl-indent stream 1)
      (format stream "if ~A break;~%"
              (msl-control-condition-text (msl-occurrence-text until)))))
  (dolist (binding (msl-counted-fold-statement-bindings statement))
    (write-msl-indent stream 1)
    (format stream "~A ~A = ~A;~%"
            (msl-variable-statement-type binding)
            (msl-variable-statement-name binding)
            (msl-occurrence-text (msl-variable-statement-value binding))))
  (write-msl-indent stream 1)
  (format stream "~A = ~A;~%"
          (msl-counted-fold-statement-state-name statement)
          (msl-occurrence-text
           (msl-counted-fold-statement-update statement)))
  (write-msl-indent stream)
  (format stream "}~%"))

(defun msl-stage-qualifier (stage)
  (ecase stage
    (:vertex "vertex")
    (:fragment "fragment")
    (:compute "kernel")
    (:task "[[object]]")
    (:mesh "[[mesh]]")))

(defun write-msl-entry-point (entry-point stream)
  (format stream "~A ~A ~A("
          (msl-stage-qualifier (msl-entry-point-stage entry-point))
          (msl-entry-point-return-type entry-point)
          (msl-entry-point-name entry-point))
  (loop for parameter in (msl-entry-point-parameters entry-point)
        for firstp = t then nil
        do (unless firstp
             (format stream ",~%"))
           (when (msl-parameter-origin parameter)
             (write-msl-semantic-comments
              (msl-parameter-origin parameter) stream "    "
              :sampled-p t))
           (unless firstp
             (write-string "    " stream))
           (format stream "~A ~A~@[ ~A~]"
                   (msl-parameter-type parameter)
                   (msl-parameter-name parameter)
                   (msl-parameter-attribute parameter)))
  (let ((void-p (string= "void" (msl-entry-point-return-type entry-point))))
    (format stream ") {~%")
    (unless void-p
      (format stream "  ~A result = {};~%"
              (msl-entry-point-return-type entry-point)))
    (let ((*msl-statement-indentation* 1))
      (dolist (statement (msl-entry-point-statements entry-point))
        (write-msl-statement statement stream)))
    (unless void-p
      (format stream "  return result;~%"))
    (format stream "}~%")))

(defun render-msl-document (document)
  (with-output-to-string (stream)
    (format stream "#include <metal_stdlib>~%~%using namespace metal;~%")
    (dolist (declaration (msl-document-declarations document))
      (terpri stream)
      (write-msl-declaration declaration stream))
    (terpri stream)
    (write-msl-entry-point (msl-document-entry-point document) stream)))

(defun msl-local-invocation-index-name (specification)
  (msl-identifier
   (shader:shader-object-name
    (find :local-invocation-index
          (shader:shader-specification-inputs specification)
          :key #'shader:shader-interface-built-in))))

(defgeneric lower-msl-statement (context statement)
  (:documentation "Lower one semantic shader statement to structured MSL."))

(defun lower-msl-expression-with-pending (context expression)
  (let ((value (lower-msl-expression context expression)))
    (values value (drain-msl-pending-statements context))))

(defmethod lower-msl-statement
    ((context msl-lowering-context) (statement shader:shader-output-assignment))
  (multiple-value-bind (value pending)
      (lower-msl-expression-with-pending
       context (shader:shader-assignment-value statement))
    (append
     pending
     (list
      (make-instance
       'msl-output-statement
       :field (msl-identifier
               (shader:shader-object-name
                (shader:shader-assignment-output statement)))
       :value value :origin statement)))))

(defmethod lower-msl-statement
    ((context msl-lowering-context)
     (statement shader:shader-conditional-statement))
  (multiple-value-bind (condition pending)
      (lower-msl-expression-with-pending
       context (shader:shader-conditional-statement-condition statement))
    (append
     pending
     (list
      (make-instance
       'msl-if-statement
       :condition condition
       :statements
       (mapcan (lambda (child) (lower-msl-statement context child))
               (shader:shader-conditional-statement-statements statement))
       :origin statement)))))

(defmethod lower-msl-statement
    ((context msl-lowering-context)
     (statement shader:shader-mesh-output-counts))
  (let ((vertex-count
          (lower-msl-expression
           context (shader:shader-mesh-output-vertex-count statement)))
        (primitive-count
          (lower-msl-expression
           context (shader:shader-mesh-output-primitive-count statement))))
    (append
     (drain-msl-pending-statements context)
     (list
      (make-instance
       'msl-mesh-output-counts-statement
       :lane (msl-local-invocation-index-name
              (msl-context-specification context))
       :vertex-count vertex-count :primitive-count primitive-count
       :origin statement)))))

(defun lower-msl-declaration-values (context values)
  (let ((pending nil)
        (lowered nil))
    (dolist (pair values)
      (let ((value (lower-msl-expression context (cdr pair))))
        (setf pending
              (nconc pending (drain-msl-pending-statements context)))
        (push (cons (car pair) value) lowered)))
    (values (nreverse lowered) pending)))

(defmethod lower-msl-statement
    ((context msl-lowering-context) (statement shader:shader-mesh-vertex-store))
  (let ((index
          (lower-msl-expression
           context (shader:shader-mesh-vertex-store-index statement))))
    (let ((pending (drain-msl-pending-statements context)))
      (multiple-value-bind (values value-pending)
          (lower-msl-declaration-values
           context (shader:shader-mesh-vertex-store-values statement))
        (append
         pending value-pending
         (list
          (make-instance
           'msl-mesh-vertex-statement
           :index index
           :vertex-type
           (msl-structure-name-for
            (shader:shader-object-name (msl-context-specification context))
            "Vertex")
           :values values :origin statement)))))))

(defmethod lower-msl-statement
    ((context msl-lowering-context)
     (statement shader:shader-mesh-primitive-store))
  (let ((index
          (lower-msl-expression
           context (shader:shader-mesh-primitive-store-index statement)))
        (indices
          (lower-msl-expression
           context (shader:shader-mesh-primitive-store-indices statement))))
    (let ((pending (drain-msl-pending-statements context)))
      (multiple-value-bind (values value-pending)
          (lower-msl-declaration-values
           context (shader:shader-mesh-primitive-store-values statement))
        (let* ((specification (msl-context-specification context))
               (mesh-output (shader:shader-specification-mesh-output specification)))
          (append
           pending value-pending
           (list
            (make-instance
             'msl-mesh-primitive-statement
             :index index :indices indices
             :topology (shader:shader-mesh-output-topology mesh-output)
             :primitive-type
             (and values
                  (msl-structure-name-for
                   (shader:shader-object-name specification) "Primitive"))
             :values values :origin statement))))))))

(defmethod lower-msl-statement
    ((context msl-lowering-context)
     (statement shader:shader-task-payload-store))
  (let ((index
          (and (shader:shader-task-payload-store-index statement)
               (lower-msl-expression
                context (shader:shader-task-payload-store-index statement))))
        (value
          (lower-msl-expression
           context (shader:shader-task-payload-store-value statement))))
    (append
     (drain-msl-pending-statements context)
     (list
      (make-instance
       'msl-task-payload-store-statement
       :field (msl-identifier
               (shader:shader-object-name
                (shader:shader-task-payload-store-field statement)))
       :index index :value value :origin statement)))))

(defmethod lower-msl-statement
    ((context msl-lowering-context)
     (statement shader:shader-emit-mesh-workgroups))
  (multiple-value-bind (workgroups pending)
      (lower-msl-expression-with-pending
       context (shader:shader-emit-mesh-workgroups-counts statement))
    (append
     pending
     (list
      (make-instance
       'msl-emit-mesh-workgroups-statement
       :lane (msl-local-invocation-index-name
              (msl-context-specification context))
       :workgroups workgroups :origin statement)))))

(defun lower-msl-bindings (context specification)
  (let ((statements nil))
    (dolist (binding (shader:shader-specification-bindings specification))
      (let* ((expression (shader:shader-binding-expression binding))
             (name (msl-identifier (shader:shader-object-name binding)))
             (value (lower-msl-expression context expression)))
        (setf statements
              (nconc statements (drain-msl-pending-statements context)))
        (setf (gethash binding (msl-context-references context)) name)
        (setf statements
              (nconc
               statements
               (list
                (make-instance
                 'msl-variable-statement
                 :type (msl-type-name
                        (shader:shader-expression-type expression)
                        (shader:shader-expression-source-form expression))
                 :name name :value value :origin binding))))))
    statements))

(defun lower-msl-statements (context specification)
  (mapcan (lambda (statement) (lower-msl-statement context statement))
          (shader:shader-specification-statements specification)))

(defun msl-uniform-structures (specification)
  (loop for resource in (shader:shader-specification-resources specification)
        when (typep resource 'shader:shader-uniform-block)
          collect (msl-uniform-structure resource)))

(defun finish-msl-document
    (target specification context declarations entry-point)
  (maphash (lambda (expression occurrences)
             (setf (gethash expression
                            (msl-context-expression-occurrences context))
                   (nreverse occurrences)))
           (msl-context-expression-occurrences context))
  (let ((document
          (make-instance
           'msl-document
           :target target :specification specification
           :declarations declarations :entry-point entry-point :source ""
           :expression-occurrences
           (msl-context-expression-occurrences context)
           :occurrence-expression
           (msl-context-occurrence-expression context))))
    (setf (msl-document-source document) (render-msl-document document))
    document))

(defun lower-traditional-msl-specification
    (target specification context)
  (let* ((stage (shader:shader-specification-stage specification))
         (base-name (shader:shader-object-name specification))
         (ordinary-inputs
           (remove-if #'shader:shader-interface-built-in
                      (shader:shader-specification-inputs specification)))
         (built-in-inputs
           (remove-if-not #'shader:shader-interface-built-in
                          (shader:shader-specification-inputs specification)))
         (input-structure
           (when ordinary-inputs
             (msl-interface-structure base-name "Input" stage ordinary-inputs)))
         (output-structure
           (msl-interface-structure
            base-name "Output" stage
            (shader:shader-specification-outputs specification)))
         (input-parameter-name "stage_in"))
    (unless (member stage '(:vertex :fragment))
      (error 'shader:shader-language-error
             :form (shader:shader-object-source-form specification)
             :reason :unsupported-msl-stage :details stage))
    (register-msl-declaration-references
     context specification input-parameter-name)
    (let ((entry-point
            (make-instance
             'msl-entry-point
             :stage stage :return-type (msl-structure-name output-structure)
             :name (msl-identifier base-name)
             :parameters
             (append
              (when input-structure
                (list
                 (make-instance
                  'msl-parameter :type (msl-structure-name input-structure)
                  :name input-parameter-name :attribute "[[stage_in]]")))
              (mapcar (lambda (input)
                        (msl-built-in-input-parameter stage input))
                      built-in-inputs)
              (mapcar #'msl-resource-parameter
                      (shader:shader-specification-resources specification)))
             :statements
             (nconc (lower-msl-bindings context specification)
                    (lower-msl-statements context specification)))))
      (finish-msl-document
       target specification context
       (append (remove nil (list input-structure output-structure))
               (msl-uniform-structures specification))
       entry-point))))

(defun msl-workgroup-parameters (stage specification)
  (mapcar (lambda (input) (msl-built-in-input-parameter stage input))
          (shader:shader-specification-inputs specification)))

(defun msl-payload-parameter (stage payload)
  (when payload
    (make-instance
     'msl-parameter
     :type (format nil "object_data ~A~A&"
                   (if (eq stage :mesh) "const " "")
                   (msl-structure-name-for (shader:shader-object-name payload)))
     :name "payload" :attribute "[[payload]]")))

(defun lower-task-msl-specification (target specification context)
  (let* ((payload (shader:shader-specification-task-payload specification))
         (payload-structure (and payload (msl-task-payload-structure payload))))
    (register-msl-declaration-references context specification "stage_in")
    (finish-msl-document
     target specification context
     (append (remove nil (list payload-structure))
             (msl-uniform-structures specification))
     (make-instance
      'msl-entry-point
      :stage :task :return-type "void"
      :name (msl-identifier (shader:shader-object-name specification))
      :parameters
      (append
       (remove nil
               (list (msl-payload-parameter :task payload)
                     (make-instance
                      'msl-parameter
                      :type "metal::mesh_grid_properties"
                      :name "mesh_grid" :attribute nil)))
       (msl-workgroup-parameters :task specification)
       (mapcar #'msl-resource-parameter
               (shader:shader-specification-resources specification)))
      :statements
      (nconc (lower-msl-bindings context specification)
             (lower-msl-statements context specification))))))

(defun msl-mesh-topology-name (topology)
  (ecase topology
    (:points "point")
    (:lines "line")
    (:triangles "triangle")))

(defun lower-mesh-msl-specification (target specification context)
  (let* ((base-name (shader:shader-object-name specification))
         (mesh-output (shader:shader-specification-mesh-output specification))
         (vertex-structure
           (msl-interface-structure
            base-name "Vertex" :mesh
            (shader:shader-mesh-output-vertex-outputs mesh-output)))
         (primitive-outputs
           (shader:shader-mesh-output-primitive-outputs mesh-output))
         (primitive-structure
           (and primitive-outputs
                (msl-interface-structure
                 base-name "Primitive" :mesh primitive-outputs)))
         (payload (shader:shader-specification-task-payload specification))
         (payload-structure (and payload (msl-task-payload-structure payload)))
         (mesh-type
           (format nil "metal::mesh<~A, ~A, ~D, ~D, metal::topology::~A>"
                   (msl-structure-name vertex-structure)
                   (if primitive-structure
                       (msl-structure-name primitive-structure)
                       "void")
                   (shader:shader-mesh-output-max-vertices mesh-output)
                   (shader:shader-mesh-output-max-primitives mesh-output)
                   (msl-mesh-topology-name
                    (shader:shader-mesh-output-topology mesh-output)))))
    (register-msl-declaration-references context specification "stage_in")
    (finish-msl-document
     target specification context
     (append (remove nil
                     (list vertex-structure primitive-structure
                           payload-structure))
             (msl-uniform-structures specification))
     (make-instance
      'msl-entry-point
      :stage :mesh :return-type "void"
      :name (msl-identifier base-name)
      :parameters
      (append
       (list (make-instance 'msl-parameter
                            :type mesh-type :name "mesh_out" :attribute nil))
       (remove nil (list (msl-payload-parameter :mesh payload)))
       (msl-workgroup-parameters :mesh specification)
       (mapcar #'msl-resource-parameter
               (shader:shader-specification-resources specification)))
      :statements
      (nconc (lower-msl-bindings context specification)
             (lower-msl-statements context specification))))))

(defmethod shader:lower-shader-specification
    ((target msl-target) (specification shader:shader-specification))
  "Lower the shared shader graph directly to a structured MSL document."
  (let ((context
          (make-instance 'msl-lowering-context
                         :target target :specification specification)))
    (case (shader:shader-specification-stage specification)
      ((:vertex :fragment)
       (lower-traditional-msl-specification target specification context))
      (:task (lower-task-msl-specification target specification context))
      (:mesh (lower-mesh-msl-specification target specification context))
      (otherwise
       (error 'shader:shader-language-error
              :form (shader:shader-object-source-form specification)
              :reason :unsupported-msl-stage
              :details (shader:shader-specification-stage specification))))))

(defun compile-msl (specification &optional (target *metal-4-target*))
  (shader:lower-shader-specification target specification))

(defun write-msl (document pathname)
  "Write DOCUMENT's deterministic source to PATHNAME and return PATHNAME."
  (check-type document msl-document)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (msl-document-source document) stream))
  pathname)
