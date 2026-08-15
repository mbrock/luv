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
   (attribute :initarg :attribute :reader msl-field-attribute)))

(defclass msl-structure-declaration ()
  ((name :initarg :name :reader msl-structure-name)
   (fields :initarg :fields :reader msl-structure-fields)))

(defclass msl-parameter ()
  ((type :initarg :type :reader msl-parameter-type)
   (name :initarg :name :reader msl-parameter-name)
   (attribute :initarg :attribute :reader msl-parameter-attribute)))

(defclass msl-variable-statement ()
  ((type :initarg :type :reader msl-variable-statement-type)
   (name :initarg :name :reader msl-variable-statement-name)
   (value :initarg :value :reader msl-variable-statement-value)))

(defclass msl-output-statement ()
  ((field :initarg :field :reader msl-output-statement-field)
   (value :initarg :value :reader msl-output-statement-value)))

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
    :reader msl-context-occurrence-expression)))

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
  (case (spv:shader-type-name (spv:find-shader-type type source-form))
    (:float "float")
    (:vec2 "float2")
    (:vec3 "float3")
    (:vec4 "float4")
    (:texture-2d "texture2d<float>")
    (:depth-texture-2d "depth2d<float>")
    (:sampler "sampler")
    (otherwise
     (error 'spv:shader-language-error
            :form source-form :reason :unsupported-msl-type
            :details (spv:shader-type-name type)))))

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
    ((context msl-lowering-context) (expression spv:shader-literal))
  (note-msl-occurrence
   context expression (msl-float-literal (spv:shader-literal-value expression))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression spv:shader-reference))
  (let* ((target (spv:shader-reference-target expression))
         (text (gethash target (msl-context-references context))))
    (unless text
      (error 'spv:shader-language-error
             :form (spv:shader-expression-source-form expression)
             :reason :unsupported-msl-reference
             :details (spv:shader-object-name target)))
    (note-msl-occurrence context expression text)))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression spv:shader-call))
  (spv:lower-shader-call (spv:shader-call-operator expression)
                         context expression))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression spv:shader-map-application))
  (declare (ignore context))
  (error 'spv:shader-language-error
         :form (spv:shader-expression-source-form expression)
         :reason :unsupported-msl-projective-map))

(defun lower-msl-quantity-boundary (context expression operand)
  (let ((lowered (lower-msl-expression context operand)))
    (note-msl-occurrence context expression (msl-occurrence-text lowered))))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression spv:shader-interpretation))
  (lower-msl-quantity-boundary
   context expression (spv:shader-interpretation-operand expression)))

(defmethod lower-msl-expression
    ((context msl-lowering-context)
     (expression spv:shader-quantity-construction))
  (lower-msl-quantity-boundary
   context expression (spv:shader-quantity-construction-operand expression)))

(defmethod lower-msl-expression
    ((context msl-lowering-context)
     (expression spv:shader-quantity-assumption))
  (lower-msl-quantity-boundary
   context expression (spv:shader-quantity-assumption-operand expression)))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression spv:shader-representation))
  (lower-msl-quantity-boundary
   context expression (spv:shader-representation-operand expression)))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression spv:shader-unit-conversion))
  (let* ((operand
           (lower-msl-expression
            context (spv:shader-unit-conversion-operand expression)))
         (factor (spv:shader-unit-conversion-factor expression))
         (text
           (if (= factor 1)
               (msl-occurrence-text operand)
               (format nil "(~A * ~A)"
                       (msl-occurrence-text operand)
                       (msl-float-literal factor)))))
    (note-msl-occurrence context expression text)))

(defmethod lower-msl-expression
    ((context msl-lowering-context) (expression spv:shader-expression))
  (declare (ignore context))
  (error 'spv:shader-language-error
         :form (spv:shader-expression-source-form expression)
         :reason :unsupported-msl-expression
         :details (class-name (class-of expression))))

(defun lower-msl-operands (context expression)
  (mapcar (lambda (operand)
            (lower-msl-expression context operand))
          (spv:shader-call-operands expression)))

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

(defmethod spv:lower-shader-call
    (operator (context msl-lowering-context) expression)
  (error 'spv:shader-language-error
         :form (spv:shader-expression-source-form expression)
         :reason :unsupported-msl-operator :details operator))

(defmacro define-msl-infix-operator (operator text)
  `(defmethod spv:lower-shader-call
       ((operator (eql ',operator))
        (context msl-lowering-context)
        (expression spv:shader-call))
     (declare (ignore operator))
     (lower-msl-infix-call context expression ,text)))

(define-msl-infix-operator + "+")
(define-msl-infix-operator - "-")
(define-msl-infix-operator * "*")
(define-msl-infix-operator / "/")

(defmacro define-msl-function-operator (operator name)
  `(defmethod spv:lower-shader-call
       ((operator (eql ',operator))
        (context msl-lowering-context)
        (expression spv:shader-call))
     (declare (ignore operator))
     (lower-msl-function-call context expression ,name)))

(define-msl-function-operator spv:dot "dot")
(define-msl-function-operator spv:mix "mix")
(define-msl-function-operator abs "abs")
(define-msl-function-operator sqrt "sqrt")
(define-msl-function-operator expt "pow")
(define-msl-function-operator spv:clamp "clamp")
(define-msl-function-operator spv:smoothstep "smoothstep")
(define-msl-function-operator spv:step "step")
(define-msl-function-operator spv:normalize "normalize")

(defmacro define-msl-chained-function-operator (operator name)
  `(defmethod spv:lower-shader-call
       ((operator (eql ',operator))
        (context msl-lowering-context)
        (expression spv:shader-call))
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
             (msl-type-name (spv:shader-expression-type expression)
                            (spv:shader-expression-source-form expression))
             operands))))

(defmacro define-msl-vector-constructor (operator)
  `(defmethod spv:lower-shader-call
       ((operator (eql ',operator))
        (context msl-lowering-context)
        (expression spv:shader-call))
     (declare (ignore operator))
     (lower-msl-vector-constructor context expression)))

(define-msl-vector-constructor spv:vec2)
(define-msl-vector-constructor spv:vec3)
(define-msl-vector-constructor spv:vec4)

(defmethod spv:lower-shader-call
    ((operator (eql 'spv:swizzle))
     (context msl-lowering-context)
     (expression spv:shader-call))
  (declare (ignore operator))
  (let* ((operand
           (lower-msl-expression
            context (first (spv:shader-call-operands expression))))
         (components
           (string-downcase
            (string (first (spv:shader-call-parameters expression))))))
    (note-msl-occurrence
     context expression
     (format nil "~A.~A" (msl-occurrence-text operand) components))))

(defmethod spv:lower-shader-call
    ((operator (eql 'spv:sample))
     (context msl-lowering-context)
     (expression spv:shader-call))
  (declare (ignore operator))
  (destructuring-bind (texture sampler coordinate)
      (spv:shader-call-operands expression)
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
             (if (spv:shader-type-image-depth-p
                  (spv:shader-expression-type texture))
                 (format nil "float4(~A)" sample)
                 sample)))
      (note-msl-occurrence context expression text))))

(defmethod spv:lower-shader-call
    ((operator (eql 'spv:sample-compare))
     (context msl-lowering-context)
     (expression spv:shader-call))
  (declare (ignore operator))
  (let ((operands (mapcar #'msl-occurrence-text
                          (lower-msl-operands context expression))))
    (note-msl-occurrence
     context expression
     (destructuring-bind (texture sampler coordinate depth-reference) operands
       (format nil "~A.sample_compare(~A, ~A, ~A)"
               texture sampler coordinate depth-reference)))))

(defun msl-interface-attribute (stage declaration)
  (let ((direction (spv:shader-interface-direction declaration))
        (location (spv:shader-interface-location declaration))
        (built-in (spv:shader-interface-built-in declaration))
        (source-form (spv:shader-object-source-form declaration)))
    (cond
      ((and (eq direction :output) (eq built-in :position))
       "[[position]]")
      (built-in
       (error 'spv:shader-language-error
              :form source-form :reason :unsupported-msl-built-in
              :details (list direction built-in)))
      ((and (eq stage :vertex) (eq direction :input))
       (format nil "[[attribute(~D)]]" location))
      ((and (eq stage :fragment) (eq direction :output))
       (format nil "[[color(~D)]]" location))
      (t
       (format nil "[[user(locn~D)]]" location)))))

(defun msl-interface-structure (name suffix stage declarations)
  (make-instance
   'msl-structure-declaration
   :name (msl-structure-name-for name suffix)
   :fields
   (mapcar
    (lambda (declaration)
      (make-instance
       'msl-field
       :type (msl-type-name (spv:shader-declaration-type declaration)
                            (spv:shader-object-source-form declaration))
       :name (msl-identifier (spv:shader-object-name declaration))
       :attribute (msl-interface-attribute stage declaration)))
    declarations)))

(defun msl-uniform-structure (resource)
  (make-instance
   'msl-structure-declaration
   :name (msl-structure-name-for (spv:shader-object-name resource))
   :fields
   (mapcar
    (lambda (member)
      (make-instance
       'msl-field
       :type (msl-type-name (spv:shader-declaration-type member)
                            (spv:shader-object-source-form member))
       :name (msl-identifier (spv:shader-object-name member))
       :attribute nil))
    (spv:shader-uniform-block-members resource))))

(defun msl-resource-parameter (resource)
  (unless (zerop (spv:shader-resource-descriptor-set resource))
    (error 'spv:shader-language-error
           :form (spv:shader-object-source-form resource)
           :reason :unsupported-msl-descriptor-set
           :details (spv:shader-resource-descriptor-set resource)))
  (let* ((name (msl-identifier (spv:shader-object-name resource)))
         (binding (spv:shader-resource-binding resource))
         (type (spv:shader-declaration-type resource))
         (kind (spv:shader-type-opaque-kind type)))
    (case kind
      (:uniform-block
       (make-instance
        'msl-parameter
        :type (format nil "constant ~A&"
                      (msl-structure-name-for
                       (spv:shader-object-name resource)))
        :name name :attribute (format nil "[[buffer(~D)]]" binding)))
      (:texture-2d
       (make-instance
        'msl-parameter
        :type (msl-type-name type (spv:shader-object-source-form resource))
        :name name :attribute (format nil "[[texture(~D)]]" binding)))
      (:sampler
       (make-instance
        'msl-parameter
        :type "sampler" :name name
        :attribute (format nil "[[sampler(~D)]]" binding)))
      (otherwise
       (error 'spv:shader-language-error
              :form (spv:shader-object-source-form resource)
              :reason :unsupported-msl-resource :details kind)))))

(defun register-msl-declaration-references
    (context specification input-parameter-name)
  (dolist (input (spv:shader-specification-inputs specification))
    (setf (gethash input (msl-context-references context))
          (format nil "~A.~A" input-parameter-name
                  (msl-identifier (spv:shader-object-name input)))))
  (dolist (resource (spv:shader-specification-resources specification))
    (let ((resource-name (msl-identifier (spv:shader-object-name resource))))
      (setf (gethash resource (msl-context-references context)) resource-name)
      (when (typep resource 'spv:shader-uniform-block)
        (dolist (member (spv:shader-uniform-block-members resource))
          (setf (gethash member (msl-context-references context))
                (format nil "~A.~A" resource-name
                        (msl-identifier (spv:shader-object-name member))))))))
  context)

(defgeneric write-msl-declaration (declaration stream))

(defmethod write-msl-declaration
    ((declaration msl-structure-declaration) stream)
  (format stream "struct ~A {~%" (msl-structure-name declaration))
  (dolist (field (msl-structure-fields declaration))
    (format stream "  ~A ~A~@[ ~A~];~%"
            (msl-field-type field)
            (msl-field-name field)
            (msl-field-attribute field)))
  (format stream "};~%"))

(defgeneric write-msl-statement (statement stream))

(defmethod write-msl-statement ((statement msl-variable-statement) stream)
  (format stream "  ~A ~A = ~A;~%"
          (msl-variable-statement-type statement)
          (msl-variable-statement-name statement)
          (msl-occurrence-text (msl-variable-statement-value statement))))

(defmethod write-msl-statement ((statement msl-output-statement) stream)
  (format stream "  result.~A = ~A;~%"
          (msl-output-statement-field statement)
          (msl-occurrence-text (msl-output-statement-value statement))))

(defun msl-stage-qualifier (stage)
  (ecase stage
    (:vertex "vertex")
    (:fragment "fragment")
    (:compute "kernel")))

(defun write-msl-entry-point (entry-point stream)
  (format stream "~A ~A ~A("
          (msl-stage-qualifier (msl-entry-point-stage entry-point))
          (msl-entry-point-return-type entry-point)
          (msl-entry-point-name entry-point))
  (loop for parameter in (msl-entry-point-parameters entry-point)
        for firstp = t then nil
        do (unless firstp
             (format stream ",~%    "))
           (format stream "~A ~A~@[ ~A~]"
                   (msl-parameter-type parameter)
                   (msl-parameter-name parameter)
                   (msl-parameter-attribute parameter)))
  (format stream ") {~%  ~A result = {};~%"
          (msl-entry-point-return-type entry-point))
  (dolist (statement (msl-entry-point-statements entry-point))
    (write-msl-statement statement stream))
  (format stream "  return result;~%}~%"))

(defun render-msl-document (document)
  (with-output-to-string (stream)
    (format stream "#include <metal_stdlib>~%~%using namespace metal;~%")
    (dolist (declaration (msl-document-declarations document))
      (terpri stream)
      (write-msl-declaration declaration stream))
    (terpri stream)
    (write-msl-entry-point (msl-document-entry-point document) stream)))

(defmethod spv:lower-shader-specification
    ((target msl-target) (specification spv:shader-specification))
  "Lower the shared shader graph directly to a structured MSL document.

This is the sibling target proof described by #58IDSR."
  (let* ((context
           (make-instance 'msl-lowering-context
                          :target target :specification specification))
         (stage (spv:shader-specification-stage specification))
         (base-name (spv:shader-object-name specification))
         (input-structure
           (msl-interface-structure
            base-name "Input" stage
            (spv:shader-specification-inputs specification)))
         (output-structure
           (msl-interface-structure
            base-name "Output" stage
            (spv:shader-specification-outputs specification)))
         (uniform-structures
           (loop for resource in (spv:shader-specification-resources specification)
                 when (typep resource 'spv:shader-uniform-block)
                   collect (msl-uniform-structure resource)))
         (input-parameter-name "stage_in")
         (parameters
           (cons
            (make-instance
             'msl-parameter
             :type (msl-structure-name input-structure)
             :name input-parameter-name :attribute "[[stage_in]]")
            (mapcar #'msl-resource-parameter
                    (spv:shader-specification-resources specification))))
         (statements nil))
    (unless (member stage '(:vertex :fragment))
      (error 'spv:shader-language-error
             :form (spv:shader-object-source-form specification)
             :reason :unsupported-msl-stage :details stage))
    (register-msl-declaration-references
     context specification input-parameter-name)
    (dolist (binding (spv:shader-specification-bindings specification))
      (let ((expression (spv:shader-binding-expression binding)))
        (unless (spv:shader-expression-materialized-p expression)
          (error 'spv:shader-language-error
                 :form (spv:shader-expression-source-form expression)
                 :reason :unsupported-msl-virtual-binding
                 :details (spv:shader-object-name binding)))
        (let* ((name (msl-identifier (spv:shader-object-name binding)))
               (value (lower-msl-expression context expression)))
          (setf (gethash binding (msl-context-references context)) name)
          (push (make-instance
                 'msl-variable-statement
                 :type (msl-type-name
                        (spv:shader-expression-type expression)
                        (spv:shader-expression-source-form expression))
                 :name name :value value)
                statements))))
    (dolist (statement (spv:shader-specification-statements specification))
      (push (make-instance
             'msl-output-statement
             :field (msl-identifier
                     (spv:shader-object-name
                      (spv:shader-assignment-output statement)))
             :value (lower-msl-expression
                     context (spv:shader-assignment-value statement)))
            statements))
    (maphash (lambda (expression occurrences)
               (setf (gethash expression
                              (msl-context-expression-occurrences context))
                     (nreverse occurrences)))
             (msl-context-expression-occurrences context))
    (let* ((entry-point
             (make-instance
              'msl-entry-point
              :stage stage
              :return-type (msl-structure-name output-structure)
              :name (msl-identifier base-name)
              :parameters parameters
              :statements (nreverse statements)))
           (document
             (make-instance
              'msl-document
              :target target :specification specification
              :declarations
              (append (list input-structure output-structure)
                      uniform-structures)
              :entry-point entry-point
              :source ""
              :expression-occurrences
              (msl-context-expression-occurrences context)
              :occurrence-expression
              (msl-context-occurrence-expression context))))
      (setf (msl-document-source document) (render-msl-document document))
      document)))

(defun compile-msl (specification &optional (target *metal-4-target*))
  (spv:lower-shader-specification target specification))

(defun write-msl (document pathname)
  "Write DOCUMENT's deterministic source to PATHNAME and return PATHNAME."
  (check-type document msl-document)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (msl-document-source document) stream))
  pathname)
