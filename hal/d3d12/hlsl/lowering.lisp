;;; Direct HLSL lowering for luv's mathematical shaders.
;;;
;;; A sibling of the MSL lowering: the source graph stays unchanged, the
;;; product is a small structured document, and every rendered expression
;;; occurrence keeps the shader expression it came from.  The dialect is the
;;; one DXC compiles to DXIL for shader model 6.0, so the output runs on any
;;; Direct3D 12 device up to the Xbox Series' 6.4 without newer features.
;;;
;;; Resources follow the Metal/Direct3D binding families (see
;;; SHADER-RESOURCE-FAMILY): a uniform block is a cbuffer at bN; a read-only
;;; storage buffer is a StructuredBuffer at tN in space0, a read-write one an
;;; RWStructuredBuffer at uN; a texture is a Texture2D at tN in space1, so
;;; buffer and texture numbers never collide; and a sampler is sN, a
;;; SamplerComparisonState when the program compares depth through it.
;;; Vertex, fragment, and compute stages lower; task and mesh stages do not,
;;; since shader model 6.4 has no mesh shaders.  #VH2TIP

(in-package #:luv.hlsl)

(defclass hlsl-target ()
  ((shader-model
    :initarg :shader-model
    :initform "6.0"
    :reader hlsl-target-shader-model
    :documentation "The DXC profile suffix, such as \"6.0\" for vs_6_0."))
  (:documentation
   "The HLSL dialect and shader model selected for one lowering."))

(defparameter *shader-model-6-target*
  (make-instance 'hlsl-target :shader-model "6.0"))

(defun hlsl-profile (stage &optional (target *shader-model-6-target*))
  "The DXC -T profile for STAGE under TARGET, such as \"ps_6_0\"."
  (format nil "~A_~A"
          (ecase stage
            (:vertex "vs")
            (:fragment "ps")
            (:compute "cs"))
          (substitute #\_ #\. (hlsl-target-shader-model target))))

(defclass hlsl-source-occurrence ()
  ((expression
    :initarg :expression
    :reader hlsl-source-occurrence-expression)
   (text
    :initarg :text
    :reader hlsl-source-occurrence-text))
  (:documentation
   "One rendered occurrence retaining its originating shader expression."))

(defclass hlsl-field ()
  ((type :initarg :type :reader hlsl-field-type)
   (name :initarg :name :reader hlsl-field-name)
   (semantic :initarg :semantic :initform nil :reader hlsl-field-semantic)
   (interpolation
    :initarg :interpolation
    :initform nil
    :reader hlsl-field-interpolation)
   (origin :initarg :origin :initform nil :reader hlsl-field-origin))
  (:documentation
   "One structure field and the declaration, if any, that produced it."))

(defclass hlsl-structure-declaration ()
  ((name :initarg :name :reader hlsl-structure-name)
   (fields :initarg :fields :reader hlsl-structure-fields)))

(defclass hlsl-resource-declaration ()
  ((type :initarg :type :reader hlsl-resource-type)
   (name :initarg :name :reader hlsl-resource-name)
   (register :initarg :register :reader hlsl-resource-register)
   (origin :initarg :origin :reader hlsl-resource-origin))
  (:documentation "One global resource bound by register."))

(defclass hlsl-constant-buffer-declaration (hlsl-resource-declaration)
  ((structure
    :initarg :structure
    :reader hlsl-constant-buffer-structure))
  (:documentation
   "A cbuffer holding one structure-typed member, so a uniform block reads
as BLOCK.MEMBER exactly as it does in MSL."))

(defclass hlsl-struct-constructor-declaration ()
  ((struct :initarg :struct :reader hlsl-struct-constructor-struct))
  (:documentation
   "A function building one structure value from its fields: HLSL has
initializer lists only in declarations, not in expressions."))

(defclass hlsl-parameter ()
  ((type :initarg :type :reader hlsl-parameter-type)
   (name :initarg :name :reader hlsl-parameter-name)
   (semantic :initarg :semantic :initform nil :reader hlsl-parameter-semantic)
   (origin :initarg :origin :initform nil :reader hlsl-parameter-origin)))

(defclass hlsl-variable-statement ()
  ((type :initarg :type :reader hlsl-variable-statement-type)
   (name :initarg :name :reader hlsl-variable-statement-name)
   (value :initarg :value :reader hlsl-variable-statement-value)
   (origin :initarg :origin :reader hlsl-variable-statement-origin)))

(defclass hlsl-output-statement ()
  ((field :initarg :field :reader hlsl-output-statement-field)
   (value :initarg :value :reader hlsl-output-statement-value)
   (origin :initarg :origin :reader hlsl-output-statement-origin)))

(defclass hlsl-if-statement ()
  ((condition :initarg :condition :reader hlsl-if-statement-condition)
   (statements :initarg :statements :reader hlsl-if-statement-statements)
   (origin :initarg :origin :reader hlsl-if-statement-origin)))

(defclass hlsl-counted-fold-statement ()
  ((type :initarg :type :reader hlsl-counted-fold-statement-type)
   (state-name
    :initarg :state-name :reader hlsl-counted-fold-statement-state-name)
   (initial :initarg :initial :reader hlsl-counted-fold-statement-initial)
   (index-name
    :initarg :index-name :reader hlsl-counted-fold-statement-index-name)
   (index-type
    :initarg :index-type :reader hlsl-counted-fold-statement-index-type)
   (count :initarg :count :reader hlsl-counted-fold-statement-count)
   (bindings
    :initarg :bindings :initform nil
    :reader hlsl-counted-fold-statement-bindings)
   (update :initarg :update :reader hlsl-counted-fold-statement-update)
   (until-bindings
    :initarg :until-bindings :initform nil
    :reader hlsl-counted-fold-statement-until-bindings)
   (until :initarg :until :initform nil
          :reader hlsl-counted-fold-statement-until)
   (origin :initarg :origin :reader hlsl-counted-fold-statement-origin)))

(defclass hlsl-buffer-store-statement ()
  ((buffer :initarg :buffer :reader hlsl-buffer-store-buffer)
   (index :initarg :index :reader hlsl-buffer-store-index)
   (value :initarg :value :reader hlsl-buffer-store-value)
   (origin :initarg :origin :reader hlsl-buffer-store-origin)))

(defclass hlsl-entry-point ()
  ((stage :initarg :stage :reader hlsl-entry-point-stage)
   (workgroup-size
    :initarg :workgroup-size :initform nil
    :reader hlsl-entry-point-workgroup-size)
   (return-type :initarg :return-type :reader hlsl-entry-point-return-type)
   (name :initarg :name :reader hlsl-entry-point-name)
   (parameters :initarg :parameters :reader hlsl-entry-point-parameters)
   (statements :initarg :statements :reader hlsl-entry-point-statements)))

(defclass hlsl-document ()
  ((target :initarg :target :reader hlsl-document-target)
   (specification :initarg :specification :reader hlsl-document-specification)
   (declarations :initarg :declarations :reader hlsl-document-declarations)
   (entry-point :initarg :entry-point :reader hlsl-document-entry-point)
   (source :initarg :source :accessor hlsl-document-source)
   (expression-occurrences
    :initarg :expression-occurrences
    :reader hlsl-document-expression-occurrences)
   (occurrence-expression
    :initarg :occurrence-expression
    :reader hlsl-document-occurrence-expression)))

(defun hlsl-document-profile (document)
  "The DXC -T profile that compiles DOCUMENT."
  (hlsl-profile (hlsl-entry-point-stage (hlsl-document-entry-point document))
                (hlsl-document-target document)))

(defclass hlsl-lowering-context ()
  ((target :initarg :target :reader hlsl-context-target)
   (specification :initarg :specification :reader hlsl-context-specification)
   (comparison-samplers
    :initarg :comparison-samplers
    :initform nil
    :reader hlsl-context-comparison-samplers)
   (references
    :initform (make-hash-table :test #'eq)
    :reader hlsl-context-references)
   (expression-occurrences
    :initform (make-hash-table :test #'eq)
    :reader hlsl-context-expression-occurrences)
   (occurrence-expression
    :initform (make-hash-table :test #'eq)
    :reader hlsl-context-occurrence-expression)
   (function-call-results
    :initform (make-hash-table :test #'eq)
    :reader hlsl-context-function-call-results)
   (pending-statements
    :initform nil :accessor hlsl-context-pending-statements)
   (fold-counter :initform 0 :accessor hlsl-context-fold-counter)))

(defun hlsl-context-stage (context)
  (shader:shader-specification-stage (hlsl-context-specification context)))

(defun drain-hlsl-pending-statements (context)
  (prog1 (hlsl-context-pending-statements context)
    (setf (hlsl-context-pending-statements context) nil)))

;;; Names.

(defparameter *hlsl-reserved-words*
  '(;; Keywords and storage classes.
    "appendstructuredbuffer" "asm" "auto" "bool" "break" "buffer" "case"
    "cbuffer" "centroid" "class" "column_major" "compile" "const"
    "continue" "default" "discard" "do" "double" "dword" "else" "export"
    "extern" "false" "float" "for" "globallycoherent" "groupshared" "half"
    "if" "in" "indices" "inline" "inout" "int" "interface" "line" "lineadj"
    "linear" "matrix" "namespace" "nointerpolation" "noperspective" "out"
    "packoffset" "pass" "payload" "point" "precise" "primitives" "register"
    "return" "row_major" "sample" "sampler" "shared" "snorm" "static"
    "string" "struct" "switch" "tbuffer" "technique" "template" "texture"
    "this" "triangle" "triangleadj" "true" "typedef" "uint" "uniform"
    "unorm" "vector" "vertices" "void" "volatile" "while" "signed"
    "unsigned"
    ;; Intrinsics this lowering calls, which a local of the same name would
    ;; shadow.
    "abs" "clamp" "cos" "ddx" "ddy" "dot" "exp" "floor" "frac" "lerp" "log"
    "max" "min" "normalize" "pow" "sign" "sin" "smoothstep" "sqrt" "step"
    "all" "and" "any" "asfloat" "asint" "asuint" "mul" "or" "select"
    "transpose"
    ;; Names this lowering itself declares.
    "result" "stage_in")
  "Words a shader name may spell but generated HLSL cannot declare.")

(defun hlsl-identifier (name)
  "Return NAME as a lower-case HLSL identifier, escaping reserved words."
  (let* ((text (string-downcase (string name)))
         (identifier
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
    (if (member identifier *hlsl-reserved-words* :test #'string=)
        (concatenate 'string identifier "_")
        identifier)))

(defun hlsl-structure-name-for (name &optional suffix)
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

(defun hlsl-type-name (type &optional source-form)
  (when (shader:shader-struct-type-p (shader:find-shader-type type source-form))
    (return-from hlsl-type-name
      (hlsl-structure-name-for
       (shader:shader-type-name (shader:find-shader-type type)))))
  (case (shader:shader-type-name (shader:find-shader-type type source-form))
    (:bool "bool")
    (:float "float")
    (:uint "uint")
    (:uint64 "uint64_t")
    (:vec2 "float2")
    (:vec3 "float3")
    (:vec4 "float4")
    (:uvec2 "uint2")
    (:uvec3 "uint3")
    (:uvec4 "uint4")
    (:int "int")
    (:ivec2 "int2")
    (:ivec3 "int3")
    (:ivec4 "int4")
    (:bvec2 "bool2")
    (:bvec3 "bool3")
    (:bvec4 "bool4")
    (:mat2 "float2x2")
    (:mat3 "float3x3")
    (:mat4 "float4x4")
    (:texture-2d "Texture2D<float4>")
    (:depth-texture-2d "Texture2D<float>")
    (:uint-texture-2d "Texture2D<uint4>")
    (otherwise
     (error 'shader:shader-language-error
            :form source-form :reason :unsupported-hlsl-type
            :details (shader:shader-type-name type)))))

(defun hlsl-field-type-name (type &optional source-form)
  "TYPE as a field of a cbuffer or structured-buffer structure.  A language
matrix is stored as its HLSL transpose (see the matrix operators), so its
columns, consecutive in memory, are the rows of a row_major HLSL matrix."
  (if (shader:shader-matrix-type-p type)
      (format nil "row_major ~A" (hlsl-type-name type source-form))
      (hlsl-type-name type source-form)))

(defun hlsl-float-literal (value)
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

(defun write-hlsl-semantic-comments
    (origin stream indentation &key sampled-p unannotated-p)
  (dolist (sentence
           (shader:shader-semantic-sentences
            origin :sampled-p sampled-p :unannotated-p unannotated-p))
    (format stream "~A// ~A~%" indentation sentence)))

;;; Expressions.

(defun note-hlsl-occurrence (context expression text)
  (let ((occurrence
          (make-instance 'hlsl-source-occurrence
                         :expression expression :text text)))
    (push occurrence
          (gethash expression (hlsl-context-expression-occurrences context)))
    (setf (gethash occurrence (hlsl-context-occurrence-expression context))
          expression)
    occurrence))

(defun hlsl-text (occurrence)
  (hlsl-source-occurrence-text occurrence))

(defgeneric lower-hlsl-expression (context expression)
  (:documentation
   "Render one shader EXPRESSION and retain a source occurrence for it."))

(defun hlsl-scalar-literal (type value)
  "VALUE as an HLSL literal of scalar TYPE."
  (ecase (shader:shader-type-scalar-kind (shader:find-shader-type type))
    (:float (hlsl-float-literal value))
    (:uint (format nil "~Du" value))
    (:int (cond ((= value (- (expt 2 31))) "(-2147483647 - 1)")
                ((minusp value) (format nil "(~D)" value))
                (t (format nil "~D" value))))
    (:bool (if value "true" "false"))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-literal))
  (note-hlsl-occurrence
   context expression
   (hlsl-scalar-literal (shader:shader-expression-type expression)
                        (shader:shader-literal-value expression))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-reference))
  (let* ((target (shader:shader-reference-target expression))
         (text (gethash target (hlsl-context-references context))))
    (cond (text
           (note-hlsl-occurrence context expression text))
          ((typep target 'shader:shader-function-parameter-binding)
           (let ((argument
                   (lower-hlsl-expression
                    context (shader:shader-binding-expression target))))
             (note-hlsl-occurrence context expression (hlsl-text argument))))
          (t
           (error 'shader:shader-language-error
                  :form (shader:shader-expression-source-form expression)
                  :reason :unsupported-hlsl-reference
                  :details (shader:shader-object-name target))))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-buffer-element))
  (let ((index
          (lower-hlsl-expression
           context (shader:shader-buffer-element-index expression))))
    (note-hlsl-occurrence
     context expression
     (format nil "~A[~A]"
             (hlsl-identifier
              (shader:shader-object-name
               (shader:shader-buffer-element-buffer expression)))
             (hlsl-text index)))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-call))
  (shader:lower-shader-call (shader:shader-call-operator expression)
                            context expression))

(defun hlsl-struct-constructor-name (struct)
  ;; Field and local identifiers are lower case, so this cannot collide.
  (format nil "construct_~A" (hlsl-type-name struct)))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context)
     (expression shader:shader-struct-construction))
  ;; #V16OXI
  (note-hlsl-occurrence
   context expression
   (format nil "~A(~{~A~^, ~})"
           (hlsl-struct-constructor-name (shader:shader-expression-type expression))
           (mapcar (lambda (value)
                     (hlsl-text (lower-hlsl-expression context value)))
                   (shader:shader-struct-construction-values expression)))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context)
     (expression shader:shader-struct-field-read))
  (note-hlsl-occurrence
   context expression
   (format nil "~A.~A"
           (hlsl-text (lower-hlsl-expression
                       context
                       (shader:shader-struct-field-read-operand expression)))
           (hlsl-identifier
            (shader:shader-object-name
             (shader:shader-struct-field-read-field expression))))))

(defun hlsl-struct-declarations (specification)
  "Each structure SPECIFICATION uses, contained ones first, with its
constructor function.  Matrix fields are row_major, as in buffers, so a
structured buffer of them has the language's layout."
  (loop for struct in (shader:shader-specification-struct-types specification)
        collect (make-instance
                 'hlsl-structure-declaration
                 :name (hlsl-type-name struct)
                 :fields
                 (mapcar (lambda (field)
                           (make-instance
                            'hlsl-field
                            :type (hlsl-field-type-name
                                   (shader:shader-struct-field-type field))
                            :name (hlsl-identifier
                                   (shader:shader-object-name field))))
                         (shader:shader-struct-type-fields struct)))
        collect (make-instance 'hlsl-struct-constructor-declaration
                               :struct struct)))

(defun check-hlsl-structure-names (declarations)
  "A structure the author defined must not share a generated one's name."
  (let ((seen nil))
    (dolist (declaration declarations declarations)
      (let ((name (typecase declaration
                    (hlsl-structure-declaration
                     (hlsl-structure-name declaration))
                    (hlsl-constant-buffer-declaration
                     (hlsl-resource-type declaration)))))
        (when name
          (when (member name seen :test #'string=)
            (error 'shader:shader-language-error
                   :reason :hlsl-structure-name-collision :details name))
          (push name seen))))))

(defun lower-hlsl-local-binding (context binding)
  "Lower BINDING to a local declaration, returning the statements it needs."
  (let* ((expression (shader:shader-binding-expression binding))
         (name (hlsl-identifier (shader:shader-object-name binding)))
         (value (lower-hlsl-expression context expression))
         (statements (drain-hlsl-pending-statements context)))
    (setf (gethash binding (hlsl-context-references context)) name)
    (append statements
            (list (make-instance
                   'hlsl-variable-statement
                   :type (hlsl-type-name
                          (shader:shader-expression-type expression)
                          (shader:shader-expression-source-form expression))
                   :name name :value value :origin binding)))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-function-call))
  (multiple-value-bind (cached-result cached-p)
      (gethash expression (hlsl-context-function-call-results context))
    (if cached-p
        (note-hlsl-occurrence context expression cached-result)
        (let ((outer-statements (drain-hlsl-pending-statements context))
              (local-statements nil)
              (saved-references nil)
              (references (hlsl-context-references context)))
          (unwind-protect
               (progn
                 (dolist (binding
                          (shader:shader-function-call-bindings expression))
                   (unless (or (typep binding
                                      'shader:shader-function-parameter-binding)
                               (nth-value 1 (gethash binding references)))
                     (push (list binding nil nil) saved-references)
                     (setf local-statements
                           (nconc local-statements
                                  (lower-hlsl-local-binding context binding)))))
                 (let* ((result
                          (lower-hlsl-expression
                           context
                           (shader:shader-function-call-result expression)))
                        (result-text (hlsl-text result)))
                   (setf local-statements
                         (nconc local-statements
                                (drain-hlsl-pending-statements context))
                         (hlsl-context-pending-statements context)
                         (nconc outer-statements local-statements)
                         (gethash expression
                                  (hlsl-context-function-call-results context))
                         result-text)
                   (note-hlsl-occurrence context expression result-text)))
            (dolist (saved saved-references)
              (remhash (first saved) references)))))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-conditional))
  ;; The condition is always a scalar bool, so ?: selects whole values and
  ;; means the same under HLSL 2018 and 2021 (where it also short-circuits).
  (let ((condition
          (lower-hlsl-expression
           context (lang:arithmetic-conditional-condition expression)))
        (consequent
          (lower-hlsl-expression
           context (lang:arithmetic-conditional-consequent expression)))
        (alternative
          (lower-hlsl-expression
           context (lang:arithmetic-conditional-alternative expression))))
    (note-hlsl-occurrence
     context expression
     (format nil "(~A ? ~A : ~A)"
             (hlsl-text condition)
             (hlsl-text consequent)
             (hlsl-text alternative)))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-counted-fold))
  (let* ((ordinal (incf (hlsl-context-fold-counter context)))
         (state-name (format nil "fold_state_~D" ordinal))
         (index-name (format nil "fold_index_~D" ordinal))
         (references (hlsl-context-references context))
         (count
           (lower-hlsl-expression
            context (lang:arithmetic-counted-fold-count expression)))
         (initial
           (lower-hlsl-expression
            context (lang:arithmetic-counted-fold-initial expression)))
         (index-binding
           (lang:arithmetic-counted-fold-index-binding expression))
         (state-binding
           (lang:arithmetic-counted-fold-state-binding expression)))
    (multiple-value-bind (old-index old-index-p)
        (gethash index-binding references)
      (multiple-value-bind (old-state old-state-p)
          (gethash state-binding references)
        (setf (gethash index-binding references) index-name
              (gethash state-binding references) state-name)
        (let* ((preheader-statements (drain-hlsl-pending-statements context))
               (until-expression
                 (lang:arithmetic-counted-fold-until expression))
               (until
                 (and until-expression
                      (lower-hlsl-expression context until-expression)))
               (until-statements
                 (and until (drain-hlsl-pending-statements context)))
               (local-statements
                 (loop for binding
                         in (lang:arithmetic-counted-fold-bindings expression)
                       nconc (lower-hlsl-local-binding context binding)))
               (update
                 (lower-hlsl-expression
                  context (lang:arithmetic-counted-fold-update expression))))
          (setf local-statements
                (nconc local-statements
                       (drain-hlsl-pending-statements context)))
          (setf (hlsl-context-pending-statements context)
                (nconc preheader-statements
                       (list
                        (make-instance
                         'hlsl-counted-fold-statement
                         :type (hlsl-type-name
                                (shader:shader-expression-type expression))
                         :state-name state-name :initial initial
                         :index-name index-name
                         :index-type
                         (hlsl-type-name
                          (shader:shader-expression-type
                           (lang:arithmetic-counted-fold-count expression)))
                         :count count
                         :bindings local-statements
                         :update update
                         :until-bindings until-statements
                         :until until
                         :origin expression))))
          (dolist (binding (lang:arithmetic-counted-fold-bindings expression))
            (remhash binding references))
          (if old-index-p
              (setf (gethash index-binding references) old-index)
              (remhash index-binding references))
          (if old-state-p
              (setf (gethash state-binding references) old-state)
              (remhash state-binding references)))))
    (note-hlsl-occurrence context expression state-name)))

(defun hlsl-projective-clip-components (context application)
  (let* ((point
           (hlsl-text
            (lower-hlsl-expression
             context (shader:shader-map-application-point application))))
         (homogeneous (format nil "float4(~A, 1.0f)" point)))
    (mapcar (lambda (row)
              (format nil "dot(~A, ~A)"
                      (hlsl-text (lower-hlsl-expression context row))
                      homogeneous))
            (shader:shader-map-application-rows application))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-map-application))
  (let ((definition (shader:shader-map-application-definition expression)))
    (unless (typep definition 'shader:shader-projective-map-definition)
      (error 'shader:shader-language-error
             :form (shader:shader-expression-source-form expression)
             :reason :unsupported-hlsl-shader-map
             :details (class-name (class-of definition))))
    (note-hlsl-occurrence
     context expression
     (format nil "float4(~{~A~^, ~})"
             (hlsl-projective-clip-components context expression)))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-map-projection))
  (let* ((application (shader:shader-map-projection-application expression))
         (definition (shader:shader-map-application-definition application))
         (clip (hlsl-projective-clip-components context application))
         (normalized
           (format nil "(float3(~{~A~^, ~}) / ~A)"
                   (subseq clip 0 3) (fourth clip)))
         (scale
           (format nil "float3(~{~A~^, ~})"
                   (mapcar #'hlsl-float-literal
                           (shader:shader-projective-map-coordinate-scale
                            definition))))
         (offset
           (format nil "float3(~{~A~^, ~})"
                   (mapcar #'hlsl-float-literal
                           (shader:shader-projective-map-coordinate-offset
                            definition)))))
    (note-hlsl-occurrence
     context expression
     (format nil "((~A * ~A) + ~A)" normalized scale offset))))

(defun lower-hlsl-quantity-boundary (context expression operand)
  (note-hlsl-occurrence
   context expression (hlsl-text (lower-hlsl-expression context operand))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context)
     (expression shader:shader-quantity-boundary))
  ;; Interpretation, construction, assumption, and representation are
  ;; semantic boundaries without runtime work.
  (lower-hlsl-quantity-boundary
   context expression (shader:shader-quantity-boundary-operand expression)))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-unit-conversion))
  (let* ((operand
           (lower-hlsl-expression
            context (shader:shader-unit-conversion-operand expression)))
         (factor (shader:shader-unit-conversion-factor expression)))
    (note-hlsl-occurrence
     context expression
     (if (= factor 1)
         (hlsl-text operand)
         (format nil "(~A * ~A)" (hlsl-text operand)
                 (hlsl-float-literal factor))))))

(defmethod lower-hlsl-expression
    ((context hlsl-lowering-context) (expression shader:shader-expression))
  (declare (ignore context))
  (error 'shader:shader-language-error
         :form (shader:shader-expression-source-form expression)
         :reason :unsupported-hlsl-expression
         :details (class-name (class-of expression))))

;;; Operators.

(defun lower-hlsl-operands (context expression)
  (mapcar (lambda (operand) (hlsl-text (lower-hlsl-expression context operand)))
          (shader:shader-call-operands expression)))

(defun lower-hlsl-infix-call (context expression operator)
  (let ((operands (lower-hlsl-operands context expression)))
    (note-hlsl-occurrence
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

(defun lower-hlsl-function-call (context expression name)
  (note-hlsl-occurrence
   context expression
   (format nil "~A(~{~A~^, ~})" name (lower-hlsl-operands context expression))))

(defun lower-hlsl-cast (context expression type-name)
  (note-hlsl-occurrence
   context expression
   (format nil "((~A)(~A))"
           type-name (first (lower-hlsl-operands context expression)))))

(defmethod shader:lower-shader-call
    (operator (context hlsl-lowering-context) expression)
  (error 'shader:shader-language-error
         :form (shader:shader-expression-source-form expression)
         :reason :unsupported-hlsl-operator :details operator))

(defmacro define-hlsl-operator (operator (context expression) &body body)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (,context hlsl-lowering-context)
        (,expression shader:shader-call))
     (declare (ignore operator))
     ,@body))

(macrolet ((infix (&rest pairs)
             `(progn
                ,@(loop for (operator text) on pairs by #'cddr
                        collect `(define-hlsl-operator ,operator
                                     (context expression)
                                   (lower-hlsl-infix-call
                                    context expression ,text)))))
           (functions (&rest pairs)
             `(progn
                ,@(loop for (operator name) on pairs by #'cddr
                        collect `(define-hlsl-operator ,operator
                                     (context expression)
                                   (lower-hlsl-function-call
                                    context expression ,name))))))
  ;; REM is C's truncated %; MOD is % only for unsigned values (below).
  (infix + "+" - "-" / "/" rem "%"
         < "<" <= "<=" > ">" >= ">=" = "==" /= "!="
         logand "&" logior "|" logxor "^")
  (functions shader:dot "dot"
             shader:mix "lerp"
             abs "abs"
             shader:any "any"
             shader:all "all"
             sqrt "sqrt"
             shader:derivative-x "ddx"
             shader:derivative-y "ddy"
             expt "pow"
             shader:clamp "clamp"
             shader:smoothstep "smoothstep"
             shader:step "step"
             shader:normalize "normalize"
             floor "floor"
             shader:fract "frac"
             sin "sin"
             cos "cos"
             exp "exp"
             log "log"
             shader:vec2 "float2"
             shader:vec3 "float3"
             shader:vec4 "float4"
             shader:uvec2 "uint2"
             shader:uvec3 "uint3"
             shader:uvec4 "uint4"
             shader:ivec2 "int2"
             shader:ivec3 "int3"
             shader:ivec4 "int4"
             shader:bvec2 "bool2"
             shader:bvec3 "bool3"
             shader:bvec4 "bool4"))

(define-hlsl-operator mod (context expression)
  ;; Unsigned MOD is %; signed MOD is floored, its sign following the
  ;; divisor, as CL:MOD.
  (if (eq :int (shader:shader-type-scalar-kind
                (shader:shader-expression-type expression)))
      (destructuring-bind (left right) (lower-hlsl-operands context expression)
        (note-hlsl-occurrence
         context expression
         (format nil "(((~A % ~A) + ~A) % ~A)" left right right right)))
      (lower-hlsl-infix-call context expression "%")))

;;; HLSL 2021 keeps && and || for scalars, where they short-circuit, and
;;; spells the componentwise vector forms and() and or().  #CAI3RP
(macrolet ((logical (operator infix function)
             `(define-hlsl-operator ,operator (context expression)
                (if (shader:shader-vector-type-p
                     (shader:shader-expression-type expression))
                    (let ((operands (lower-hlsl-operands context expression)))
                      (note-hlsl-occurrence
                       context expression
                       (reduce (lambda (left right)
                                 (format nil "~A(~A, ~A)" ,function left right))
                               (rest operands)
                               :initial-value (first operands))))
                    (lower-hlsl-infix-call context expression ,infix)))))
  (logical and "&&" "and")
  (logical or "||" "or"))

(define-hlsl-operator not (context expression)
  (note-hlsl-occurrence
   context expression
   (format nil "(!~A)" (first (lower-hlsl-operands context expression)))))

(define-hlsl-operator lognot (context expression)
  (note-hlsl-occurrence
   context expression
   (format nil "(~~~A)" (first (lower-hlsl-operands context expression)))))

(define-hlsl-operator shader:select (context expression)
  ;; HLSL 2021's select(c, t, f) is the source order; ?: is scalar-only.
  (lower-hlsl-function-call context expression "select"))

(define-hlsl-operator ash (context expression)
  (let ((count (first (shader:shader-call-parameters expression)))
        (value (first (lower-hlsl-operands context expression))))
    (note-hlsl-occurrence
     context expression
     (if (zerop count)
         (format nil "(~A)" value)
         (format nil "(~A ~A ~Du)" value (if (plusp count) "<<" ">>")
                 (abs count))))))

(define-hlsl-operator shader:shift-left (context expression)
  (lower-hlsl-infix-call context expression "<<"))

(define-hlsl-operator shader:shift-right (context expression)
  ;; Arithmetic for signed values, logical for unsigned ones.
  (lower-hlsl-infix-call context expression ">>"))

(define-hlsl-operator shader:bit-cast (context expression)
  (note-hlsl-occurrence
   context expression
   (format nil "~A(~A)"
           (ecase (shader:shader-type-scalar-kind
                   (shader:shader-expression-type expression))
             (:float "asfloat")
             (:uint "asuint")
             (:int "asint"))
           (first (lower-hlsl-operands context expression)))))

(define-hlsl-operator shader:int (context expression)
  (lower-hlsl-cast context expression "int"))

;;; Matrices.  HLSL indexes a matrix by rows and builds one from rows, while
;;; the language means columns, so a language matrix M is represented by the
;;; HLSL matrix M^T (as SPIRV-Cross does): (MAT4 C0 C1 C2 C3) is
;;; float4x4(C0, C1, C2, C3), (COLUMN M I) is M[I], and the products swap
;;; their operands into mul: M*v is mul(v, M), v*M is mul(M, v), and A*B is
;;; mul(B, A).  Buffers declare the matrices row_major, so the columns are
;;; consecutive sixteen-byte rows exactly as in Metal and on the host.
;;; #QEHEEE

(define-hlsl-operator shader:mat2 (context expression)
  (lower-hlsl-function-call context expression "float2x2"))

(define-hlsl-operator shader:mat3 (context expression)
  (lower-hlsl-function-call context expression "float3x3"))

(define-hlsl-operator shader:mat4 (context expression)
  (lower-hlsl-function-call context expression "float4x4"))

(define-hlsl-operator shader:transpose (context expression)
  (lower-hlsl-function-call context expression "transpose"))

(define-hlsl-operator shader:column (context expression)
  (note-hlsl-occurrence
   context expression
   (format nil "~A[~D]" (first (lower-hlsl-operands context expression))
           (first (shader:shader-call-parameters expression)))))

(define-hlsl-operator * (context expression)
  (let ((operands (shader:shader-call-operands expression)))
    (if (notany (lambda (operand)
                  (shader:shader-matrix-type-p
                   (shader:shader-expression-type operand)))
                operands)
        (lower-hlsl-infix-call context expression "*")
        (let* ((texts (lower-hlsl-operands context expression))
               (text (first texts))
               (type (shader:shader-expression-type (first operands))))
          (loop for operand in (rest operands)
                for operand-text in (rest texts)
                for operand-type = (shader:shader-expression-type operand)
                do (setf text
                         (if (and (or (shader:shader-matrix-type-p type)
                                      (shader:shader-matrix-type-p
                                       operand-type))
                                  (not (shader:shader-float-type-p type))
                                  (not (shader:shader-float-type-p
                                        operand-type)))
                             (format nil "mul(~A, ~A)" operand-text text)
                             (format nil "(~A * ~A)" text operand-text))
                         type (if (or (shader:shader-matrix-type-p type)
                                      (shader:shader-matrix-type-p
                                       operand-type))
                                  (shader:shader-product-type
                                   type operand-type)
                                  (if (shader:shader-vector-type-p type)
                                      type
                                      operand-type))))
          (note-hlsl-occurrence context expression text)))))

(define-hlsl-operator signum (context expression)
  ;; HLSL's sign returns int; the language's SIGNUM keeps the operand type.
  (note-hlsl-occurrence
   context expression
   (format nil "((~A)(sign(~A)))"
           (hlsl-type-name (shader:shader-expression-type expression))
           (first (lower-hlsl-operands context expression)))))

(define-hlsl-operator shader:uint (context expression)
  (lower-hlsl-cast context expression "uint"))

(define-hlsl-operator shader:uint64 (context expression)
  (lower-hlsl-cast context expression "uint64_t"))

(define-hlsl-operator float (context expression)
  (lower-hlsl-cast context expression "float"))

(macrolet ((chained (&rest operators)
             `(progn
                ,@(loop for operator in operators
                        collect
                        `(define-hlsl-operator ,operator (context expression)
                           (let ((operands
                                   (lower-hlsl-operands context expression)))
                             (note-hlsl-occurrence
                              context expression
                              (reduce (lambda (left right)
                                        (format nil "~(~A~)(~A, ~A)"
                                                ',operator left right))
                                      (rest operands)
                                      :initial-value (first operands)))))))))
  (chained min max))

(define-hlsl-operator shader:swizzle (context expression)
  (note-hlsl-occurrence
   context expression
   (format nil "~A.~(~A~)"
           (first (lower-hlsl-operands context expression))
           (first (shader:shader-call-parameters expression)))))

(defun hlsl-depth-texture-p (expression)
  (shader:shader-type-image-depth-p (shader:shader-expression-type expression)))

(defun hlsl-widen-depth (texture-expression text)
  "A depth Texture2D<float> yields one float; the language's sample is a
vec4, which MSL builds as float4(depth).  A scalar cast splats the same way."
  (if (hlsl-depth-texture-p texture-expression)
      (format nil "((float4)(~A))" text)
      text))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:sample))
     (context hlsl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  ;; Implicit-derivative Sample exists only in pixel shaders before SM 6.6.
  ;; Elsewhere sample the base level, as Metal does outside fragments.
  (destructuring-bind (texture sampler coordinate)
      (lower-hlsl-operands context expression)
    (note-hlsl-occurrence
     context expression
     (hlsl-widen-depth
      (first (shader:shader-call-operands expression))
      (if (eq :fragment (hlsl-context-stage context))
          (format nil "~A.Sample(~A, ~A)" texture sampler coordinate)
          (format nil "~A.SampleLevel(~A, ~A, 0.0f)"
                  texture sampler coordinate))))))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:sample-compare))
     (context hlsl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  ;; Depth comparison reads the base level in every stage.  Shadow maps have
  ;; one level, so this is Metal's sample_compare without the gradient
  ;; requirement that would forbid it in vertex shaders and divergent code.
  (let ((sampler (shader:shader-resource-target
                  (second (shader:shader-call-operands expression)))))
    (unless (and sampler
                 (member (shader:shader-resource-key sampler)
                         (hlsl-context-comparison-samplers context)))
      (error 'shader:shader-language-error
             :form (shader:shader-expression-source-form expression)
             :reason :hlsl-comparison-sampler-unknown
             :details (and sampler (shader:shader-object-name sampler)))))
  (destructuring-bind (texture sampler coordinate reference)
      (lower-hlsl-operands context expression)
    (note-hlsl-occurrence
     context expression
     (format nil "~A.SampleCmpLevelZero(~A, ~A, ~A)"
             texture sampler coordinate reference))))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:texel-load))
     (context hlsl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (destructuring-bind (texture coordinate)
      (lower-hlsl-operands context expression)
    (note-hlsl-occurrence
     context expression
     (hlsl-widen-depth
      (first (shader:shader-call-operands expression))
      (format nil "~A.Load(int3(int2(~A), 0))" texture coordinate)))))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:ldb))
     (context hlsl-lowering-context)
     (expression shader:shader-bit-field-call))
  "Lower LDB to a logical shift and mask in the operand's own width."
  (declare (ignore operator))
  (let* ((operands (shader:shader-call-operands expression))
         (value (hlsl-text (lower-hlsl-expression context (first operands))))
         (size (shader:shader-bit-field-size expression))
         (position (shader:shader-bit-field-position expression))
         (width (shader:shader-type-bit-width
                 (shader:shader-expression-type expression)))
         (suffix (if (= width 64) "ull" "u"))
         (shift
           (cond (position (format nil "~D~A" position suffix))
                 (t
                  (let ((text (hlsl-text
                               (lower-hlsl-expression
                                context (second operands)))))
                    (if (= width 64)
                        (format nil "((uint64_t)(~A))" text)
                        text))))))
    (note-hlsl-occurrence
     context expression
     (cond ((and position (zerop position) (= size width))
            (format nil "(~A)" value))
           ((and position (= (+ size position) width))
            (format nil "(~A >> ~A)" value shift))
           (t
            (format nil "((~A >> ~A) & 0x~X~A)"
                    value shift (1- (ash 1 size)) suffix))))))

;;; Interface structures.

(defun hlsl-flat-p (declaration)
  "Integers never interpolate; Direct3D requires them to say so."
  (or (eq :flat (shader:shader-interface-interpolation declaration))
      (member (shader:shader-type-scalar-kind
               (shader:shader-declaration-type declaration))
              '(:uint :int))))

(defun hlsl-interface-semantic (stage declaration)
  (let ((direction (shader:shader-interface-direction declaration))
        (location (shader:shader-interface-location declaration))
        (built-in (shader:shader-interface-built-in declaration)))
    (cond
      ((and (eq stage :compute)
            (member built-in '(:global-invocation-id :local-invocation-id
                               :local-invocation-index :workgroup-id)))
       (ecase built-in
         (:global-invocation-id "SV_DispatchThreadID")
         (:local-invocation-id "SV_GroupThreadID")
         (:local-invocation-index "SV_GroupIndex")
         (:workgroup-id "SV_GroupID")))
      ((and (eq direction :output) (eq built-in :position)) "SV_Position")
      ((and (eq stage :vertex) (eq direction :input)
            (eq built-in :vertex-index))
       "SV_VertexID")
      ((and (eq stage :vertex) (eq direction :input)
            (eq built-in :instance-index))
       "SV_InstanceID")
      (built-in
       (error 'shader:shader-language-error
              :form (shader:shader-object-source-form declaration)
              :reason :unsupported-hlsl-built-in
              :details (list stage direction built-in)))
      ((and (eq stage :fragment) (eq direction :output))
       (format nil "SV_Target~D" location))
      (t (format nil "LOCATION~D" location)))))

(defun hlsl-interface-field (stage declaration &optional name)
  (make-instance
   'hlsl-field
   :type (hlsl-type-name (shader:shader-declaration-type declaration)
                         (shader:shader-object-source-form declaration))
   :name (or name (hlsl-identifier (shader:shader-object-name declaration)))
   :semantic (hlsl-interface-semantic stage declaration)
   :interpolation (and (not (eq :fragment stage))
                       (eq :output (shader:shader-interface-direction
                                    declaration))
                       (hlsl-flat-p declaration)
                       "nointerpolation")
   :origin declaration))

(defun hlsl-position-first (declarations)
  "Order inter-stage declarations as the signature both stages share:
SV_Position, then user locations ascending."
  (stable-sort (copy-list declarations) #'<
               :key (lambda (declaration)
                      (if (shader:shader-interface-built-in declaration)
                          -1
                          (shader:shader-interface-location declaration)))))

(defun hlsl-fragment-input-fields (specification interface)
  "The pixel shader's input structure.  It mirrors the vertex output
signature (INTERFACE, when known) element for element, because Direct3D
links the two stages by register layout as well as by semantic."
  (let* ((inputs (remove-if #'shader:shader-interface-built-in
                            (shader:shader-specification-inputs
                             specification)))
         (sources
           (if interface
               (remove-if #'shader:shader-interface-built-in interface)
               inputs)))
    (cons (make-instance 'hlsl-field :type "float4" :name "sv_position"
                                     :semantic "SV_Position")
          (loop for source in (hlsl-position-first sources)
                for location = (shader:shader-interface-location source)
                for input = (find location inputs
                                  :key #'shader:shader-interface-location)
                collect
                (make-instance
                 'hlsl-field
                 :type (hlsl-type-name
                        (shader:shader-declaration-type source)
                        (shader:shader-object-source-form source))
                 :name (if input
                           (hlsl-identifier (shader:shader-object-name input))
                           (format nil "unused_location~D" location))
                 :semantic (format nil "LOCATION~D" location)
                 :interpolation (and (hlsl-flat-p source) "nointerpolation")
                 :origin (or input source))))))

;;; Resources.

(defun hlsl-resource-declaration (context resource)
  (unless (zerop (shader:shader-resource-descriptor-set resource))
    (error 'shader:shader-language-error
           :form (shader:shader-object-source-form resource)
           :reason :unsupported-hlsl-descriptor-set
           :details (shader:shader-resource-descriptor-set resource)))
  (let* ((name (hlsl-identifier (shader:shader-object-name resource)))
         (binding (shader:shader-resource-binding resource))
         (type (shader:shader-declaration-type resource))
         (form (shader:shader-object-source-form resource)))
    (ecase (shader:shader-type-opaque-kind type)
      (:uniform-block
       (make-instance
        'hlsl-constant-buffer-declaration
        :type (hlsl-structure-name-for (shader:shader-object-name resource))
        :name name :register (format nil "b~D" binding) :origin resource
        :structure
        (make-instance
         'hlsl-structure-declaration
         :name (hlsl-structure-name-for (shader:shader-object-name resource))
         :fields
         (mapcar (lambda (member)
                   (make-instance
                    'hlsl-field
                    :type (hlsl-field-type-name
                           (shader:shader-declaration-type member)
                           (shader:shader-object-source-form member))
                    :name (hlsl-identifier (shader:shader-object-name member))
                    :origin member))
                 (shader:shader-uniform-block-members resource)))))
      (:storage-buffer
       ;; A read-write buffer is an unordered access view, register uN.
       (let ((writable-p (shader:shader-storage-buffer-writable-p resource)))
         (make-instance
          'hlsl-resource-declaration
          :type (format nil "~:[~;RW~]StructuredBuffer<~A>"
                        writable-p
                        (hlsl-field-type-name
                         (shader:shader-storage-buffer-element-type resource)
                         form))
          :name name
          :register (format nil "~:[t~;u~]~D, space0" writable-p binding)
          :origin resource)))
      (:texture-2d
       (make-instance
        'hlsl-resource-declaration
        :type (hlsl-type-name type form)
        :name name :register (format nil "t~D, space1" binding)
        :origin resource))
      (:sampler
       (make-instance
        'hlsl-resource-declaration
        :type (if (member (shader:shader-resource-key resource)
                          (hlsl-context-comparison-samplers context))
                  "SamplerComparisonState"
                  "SamplerState")
        :name name :register (format nil "s~D" binding)
        :origin resource)))))

(defun register-hlsl-references (context specification)
  (let ((references (hlsl-context-references context)))
    (dolist (input (shader:shader-specification-inputs specification))
      (setf (gethash input references)
            (cond
              ;; HLSL has no system value for the group size: it is the
              ;; [numthreads] constant itself.
              ((eq :workgroup-size (shader:shader-interface-built-in input))
               (format nil "uint3(~{~Du~^, ~})"
                       (shader:shader-specification-workgroup-size
                        specification)))
              ((shader:shader-interface-built-in input)
               (hlsl-identifier (shader:shader-object-name input)))
              (t
               (format nil "stage_in.~A"
                       (hlsl-identifier
                        (shader:shader-object-name input)))))))
    (dolist (resource (shader:shader-specification-resources specification))
      (let ((name (hlsl-identifier (shader:shader-object-name resource))))
        (setf (gethash resource references) name)
        (when (typep resource 'shader:shader-uniform-block)
          (dolist (member (shader:shader-uniform-block-members resource))
            (setf (gethash member references)
                  (format nil "~A.~A" name
                          (hlsl-identifier
                           (shader:shader-object-name member))))))))))

(defun check-hlsl-binding-collisions (specification)
  "Uniform blocks and storage buffers share Metal's buffer index space, so
the contract numbers them together although HLSL registers would not clash."
  (let ((collision
          (first (shader:shader-family-binding-collisions specification))))
    (when collision
      (destructuring-bind (first second) collision
        (error 'shader:shader-language-error
               :form (shader:shader-object-source-form second)
               :reason :hlsl-binding-collision
               :details (list (shader:shader-resource-family second)
                              (shader:shader-resource-binding second)
                              (shader:shader-object-name first)
                              (shader:shader-object-name second)))))))

;;; Statements.

(defgeneric lower-hlsl-statement (context statement))

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context) (statement shader:shader-output-assignment))
  (let* ((value (lower-hlsl-expression
                 context (shader:shader-assignment-value statement)))
         (pending (drain-hlsl-pending-statements context)))
    (append pending
            (list (make-instance
                   'hlsl-output-statement
                   :field (hlsl-identifier
                           (shader:shader-object-name
                            (shader:shader-assignment-output statement)))
                   :value value :origin statement)))))

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context)
     (statement shader:shader-conditional-statement))
  (let* ((condition
           (lower-hlsl-expression
            context (shader:shader-conditional-statement-condition statement)))
         (pending (drain-hlsl-pending-statements context)))
    (append pending
            (list (make-instance
                   'hlsl-if-statement
                   :condition condition
                   :statements
                   (mapcan (lambda (child)
                             (lower-hlsl-statement context child))
                           (shader:shader-conditional-statement-statements
                            statement))
                   :origin statement)))))

(defmethod lower-hlsl-statement
    ((context hlsl-lowering-context) (statement shader:shader-buffer-store))
  (let* ((index (lower-hlsl-expression
                 context (shader:shader-buffer-store-index statement)))
         (value (lower-hlsl-expression
                 context (shader:shader-buffer-store-value statement)))
         (pending (drain-hlsl-pending-statements context)))
    (append pending
            (list (make-instance
                   'hlsl-buffer-store-statement
                   :buffer (hlsl-identifier
                            (shader:shader-object-name
                             (shader:shader-buffer-store-buffer statement)))
                   :index index :value value :origin statement)))))

(defmethod lower-hlsl-statement ((context hlsl-lowering-context) statement)
  (error 'shader:shader-language-error
         :form (shader:shader-statement-source-form statement)
         :reason :unsupported-hlsl-statement
         :details (class-name (class-of statement))))

;;; Rendering.

(defvar *hlsl-indentation* 1)

(defun write-hlsl-indent (stream &optional (extra 0))
  (loop repeat (+ *hlsl-indentation* extra) do (write-string "  " stream)))

(defun hlsl-condition-text (condition)
  (if (and (plusp (length condition))
           (char= #\( (char condition 0))
           (char= #\) (char condition (1- (length condition)))))
      condition
      (format nil "(~A)" condition)))

(defun hlsl-position-adjusted-text (declaration value)
  (if (eq :position (shader:shader-interface-built-in declaration))
      ;; The shared camera graph keeps Vulkan's framebuffer-oriented clip Y.
      ;; Direct3D, like Metal, points clip Y up; negate Y as MSL does.
      (format nil "(~A * float4(1.0f, -1.0f, 1.0f, 1.0f))" value)
      value))

(defgeneric write-hlsl-statement (statement stream))

(defmethod write-hlsl-statement ((statement hlsl-variable-statement) stream)
  (write-hlsl-indent stream)
  (write-hlsl-semantic-comments
   (hlsl-variable-statement-origin statement) stream ""
   :unannotated-p t)
  (write-hlsl-indent stream)
  (format stream "~A ~A = ~A;~%"
          (hlsl-variable-statement-type statement)
          (hlsl-variable-statement-name statement)
          (hlsl-text (hlsl-variable-statement-value statement))))

(defmethod write-hlsl-statement ((statement hlsl-output-statement) stream)
  (write-hlsl-indent stream)
  (format stream "result.~A = ~A;~%"
          (hlsl-output-statement-field statement)
          (hlsl-position-adjusted-text
           (shader:shader-assignment-output
            (hlsl-output-statement-origin statement))
           (hlsl-text (hlsl-output-statement-value statement)))))

(defmethod write-hlsl-statement ((statement hlsl-buffer-store-statement) stream)
  (write-hlsl-indent stream)
  (format stream "~A[~A] = ~A;~%"
          (hlsl-buffer-store-buffer statement)
          (hlsl-text (hlsl-buffer-store-index statement))
          (hlsl-text (hlsl-buffer-store-value statement))))

(defmethod write-hlsl-statement ((statement hlsl-if-statement) stream)
  (write-hlsl-indent stream)
  (format stream "if ~A {~%"
          (hlsl-condition-text
           (hlsl-text (hlsl-if-statement-condition statement))))
  (let ((*hlsl-indentation* (1+ *hlsl-indentation*)))
    (dolist (child (hlsl-if-statement-statements statement))
      (write-hlsl-statement child stream)))
  (write-hlsl-indent stream)
  (format stream "}~%"))

(defmethod write-hlsl-statement
    ((statement hlsl-counted-fold-statement) stream)
  (let* ((index-type (hlsl-counted-fold-statement-index-type statement))
         (unsigned-p (string= index-type "uint"))
         (index (hlsl-counted-fold-statement-index-name statement)))
    (write-hlsl-indent stream)
    (format stream "~A ~A = ~A;~%"
            (hlsl-counted-fold-statement-type statement)
            (hlsl-counted-fold-statement-state-name statement)
            (hlsl-text (hlsl-counted-fold-statement-initial statement)))
    (write-hlsl-indent stream)
    (format stream "for (~A ~A = ~A; ~A < ~A; ~A += ~A) {~%"
            index-type index (if unsigned-p "0u" "0.0f")
            index (hlsl-text (hlsl-counted-fold-statement-count statement))
            index (if unsigned-p "1u" "1.0f")))
  (let ((*hlsl-indentation* (1+ *hlsl-indentation*)))
    (let ((until (hlsl-counted-fold-statement-until statement)))
      (when until
        (dolist (binding (hlsl-counted-fold-statement-until-bindings statement))
          (write-hlsl-statement binding stream))
        (write-hlsl-indent stream)
        (format stream "if ~A break;~%"
                (hlsl-condition-text (hlsl-text until)))))
    (dolist (binding (hlsl-counted-fold-statement-bindings statement))
      (write-hlsl-statement binding stream))
    (write-hlsl-indent stream)
    (format stream "~A = ~A;~%"
            (hlsl-counted-fold-statement-state-name statement)
            (hlsl-text (hlsl-counted-fold-statement-update statement))))
  (write-hlsl-indent stream)
  (format stream "}~%"))

(defgeneric write-hlsl-declaration (declaration stream))

(defmethod write-hlsl-declaration
    ((declaration hlsl-structure-declaration) stream)
  (format stream "struct ~A {~%" (hlsl-structure-name declaration))
  (dolist (field (hlsl-structure-fields declaration))
    (when (hlsl-field-origin field)
      (write-hlsl-semantic-comments
       (hlsl-field-origin field) stream "  " :unannotated-p t))
    (format stream "  ~@[~A ~]~A ~A~@[ : ~A~];~%"
            (hlsl-field-interpolation field)
            (hlsl-field-type field)
            (hlsl-field-name field)
            (hlsl-field-semantic field)))
  (format stream "};~%"))

(defmethod write-hlsl-declaration
    ((declaration hlsl-constant-buffer-declaration) stream)
  (write-hlsl-declaration (hlsl-constant-buffer-structure declaration) stream)
  (terpri stream)
  (format stream "cbuffer ~ABlock : register(~A) {~%  ~A ~A;~%};~%"
          (hlsl-resource-type declaration)
          (hlsl-resource-register declaration)
          (hlsl-resource-type declaration)
          (hlsl-resource-name declaration)))

(defmethod write-hlsl-declaration
    ((declaration hlsl-struct-constructor-declaration) stream)
  (let* ((struct (hlsl-struct-constructor-struct declaration))
         (fields (shader:shader-struct-type-fields struct))
         (names (mapcar (lambda (field)
                          (hlsl-identifier (shader:shader-object-name field)))
                        fields)))
    (format stream "~A ~A(~{~A~^, ~}) {~%"
            (hlsl-type-name struct) (hlsl-struct-constructor-name struct)
            (loop for field in fields
                  for name in names
                  collect (format nil "~A ~A"
                                  (hlsl-type-name
                                   (shader:shader-struct-field-type field))
                                  name)))
    (format stream "  ~A constructed_;~%" (hlsl-type-name struct))
    (dolist (name names)
      (format stream "  constructed_.~A = ~:*~A;~%" name))
    (format stream "  return constructed_;~%}~%")))

(defmethod write-hlsl-declaration
    ((declaration hlsl-resource-declaration) stream)
  (write-hlsl-semantic-comments
   (hlsl-resource-origin declaration) stream "" :sampled-p t)
  (format stream "~A ~A : register(~A);~%"
          (hlsl-resource-type declaration)
          (hlsl-resource-name declaration)
          (hlsl-resource-register declaration)))

(defun write-hlsl-entry-point (entry-point stream)
  (when (hlsl-entry-point-workgroup-size entry-point)
    (format stream "[numthreads(~{~D~^, ~})]~%"
            (hlsl-entry-point-workgroup-size entry-point)))
  (format stream "~A ~A(~{~A~^, ~}) {~%"
          (hlsl-entry-point-return-type entry-point)
          (hlsl-entry-point-name entry-point)
          (mapcar (lambda (parameter)
                    (format nil "~A ~A~@[ : ~A~]"
                            (hlsl-parameter-type parameter)
                            (hlsl-parameter-name parameter)
                            (hlsl-parameter-semantic parameter)))
                  (hlsl-entry-point-parameters entry-point)))
  (let ((void-p (string= "void" (hlsl-entry-point-return-type entry-point))))
    (unless void-p
      (format stream "  ~A result = (~:*~A)0;~%"
              (hlsl-entry-point-return-type entry-point)))
    (let ((*hlsl-indentation* 1))
      (dolist (statement (hlsl-entry-point-statements entry-point))
        (write-hlsl-statement statement stream)))
    (unless void-p
      (format stream "  return result;~%"))
    (format stream "}~%")))

(defun render-hlsl-document (document)
  (with-output-to-string (stream)
    (format stream "// HLSL for DXC, profile ~A, entry point ~A.~%"
            (hlsl-document-profile document)
            (hlsl-entry-point-name (hlsl-document-entry-point document)))
    (dolist (declaration (hlsl-document-declarations document))
      (terpri stream)
      (write-hlsl-declaration declaration stream))
    (terpri stream)
    (write-hlsl-entry-point (hlsl-document-entry-point document) stream)))

;;; Specifications.

(defun lower-hlsl-bindings (context specification)
  (loop for binding in (shader:shader-specification-bindings specification)
        nconc (lower-hlsl-local-binding context binding)))

(defun lower-hlsl-traditional-specification
    (context entry-point-name interface)
  (let* ((specification (hlsl-context-specification context))
         (stage (shader:shader-specification-stage specification))
         (base-name (shader:shader-object-name specification))
         (ordinary-inputs
           (remove-if #'shader:shader-interface-built-in
                      (shader:shader-specification-inputs specification)))
         (built-in-inputs
           (remove-if-not #'shader:shader-interface-built-in
                          (shader:shader-specification-inputs specification)))
         (input-structure
           (cond
             ((eq stage :fragment)
              (make-instance
               'hlsl-structure-declaration
               :name (hlsl-structure-name-for base-name "Input")
               :fields (hlsl-fragment-input-fields specification interface)))
             (ordinary-inputs
              (make-instance
               'hlsl-structure-declaration
               :name (hlsl-structure-name-for base-name "Input")
               :fields (mapcar (lambda (input)
                                 (hlsl-interface-field stage input))
                               (hlsl-position-first ordinary-inputs))))))
         (output-structure
           (make-instance
            'hlsl-structure-declaration
            :name (hlsl-structure-name-for base-name "Output")
            :fields (mapcar (lambda (output)
                              (hlsl-interface-field stage output))
                            (hlsl-position-first
                             (shader:shader-specification-outputs
                              specification)))))
         (resources
           (mapcar (lambda (resource)
                     (hlsl-resource-declaration context resource))
                   (shader:shader-specification-resources specification))))
    (register-hlsl-references context specification)
    (let ((entry-point
            (make-instance
             'hlsl-entry-point
             :stage stage
             :return-type (hlsl-structure-name output-structure)
             :name (or entry-point-name (hlsl-identifier base-name))
             :parameters
             (append
              (when input-structure
                (list (make-instance
                       'hlsl-parameter
                       :type (hlsl-structure-name input-structure)
                       :name "stage_in")))
              (mapcar (lambda (input)
                        (make-instance
                         'hlsl-parameter
                         :type (hlsl-type-name
                                (shader:shader-declaration-type input))
                         :name (hlsl-identifier
                                (shader:shader-object-name input))
                         :semantic (hlsl-interface-semantic stage input)
                         :origin input))
                      built-in-inputs))
             :statements
             (nconc (lower-hlsl-bindings context specification)
                    (mapcan (lambda (statement)
                              (lower-hlsl-statement context statement))
                            (shader:shader-specification-statements
                             specification))))))
      (values (append (remove nil (list input-structure output-structure))
                      resources)
              entry-point))))

(defun lower-hlsl-compute-specification (context entry-point-name)
  (let* ((specification (hlsl-context-specification context))
         (resources
           (mapcar (lambda (resource)
                     (hlsl-resource-declaration context resource))
                   (shader:shader-specification-resources specification))))
    (register-hlsl-references context specification)
    (values
     resources
     (make-instance
      'hlsl-entry-point
      :stage :compute :return-type "void"
      :workgroup-size (shader:shader-specification-workgroup-size
                       specification)
      :name (or entry-point-name
                (hlsl-identifier (shader:shader-object-name specification)))
      :parameters
      (loop for input in (shader:shader-specification-inputs specification)
            unless (eq :workgroup-size (shader:shader-interface-built-in input))
              collect (make-instance
                       'hlsl-parameter
                       :type (hlsl-type-name
                              (shader:shader-declaration-type input))
                       :name (hlsl-identifier (shader:shader-object-name input))
                       :semantic (hlsl-interface-semantic :compute input)
                       :origin input))
      :statements
      (nconc (lower-hlsl-bindings context specification)
             (mapcan (lambda (statement)
                       (lower-hlsl-statement context statement))
                     (shader:shader-specification-statements
                      specification)))))))

(defmethod shader:lower-shader-specification
    ((target hlsl-target) (specification shader:shader-specification))
  "Lower the shared shader graph to an HLSL document named after it."
  (compile-hlsl specification :target target))

(defun compile-hlsl
    (specification &key (target *shader-model-6-target*) entry-point-name
                        (comparison-samplers nil comparison-samplers-p)
                        interface)
  "Lower SPECIFICATION to a structured HLSL document.

ENTRY-POINT-NAME names the entry function (default: after the
specification).  COMPARISON-SAMPLERS lists the keys of samplers declared as
SamplerComparisonState; by default, those SPECIFICATION itself compares
with.  INTERFACE, for a fragment stage, is the vertex stage's output
declarations, so the pixel input signature mirrors it exactly."
  (let ((stage (shader:shader-specification-stage specification)))
    (unless (member stage '(:vertex :fragment :compute))
      (error 'shader:shader-language-error
             :form (shader:shader-object-source-form specification)
             :reason :unsupported-hlsl-stage :details stage)))
  (check-hlsl-binding-collisions specification)
  (let ((context
          (make-instance
           'hlsl-lowering-context
           :target target :specification specification
           :comparison-samplers
           (if comparison-samplers-p
               comparison-samplers
               (shader:shader-comparison-samplers specification)))))
    (multiple-value-bind (declarations entry-point)
        (if (eq :compute (shader:shader-specification-stage specification))
            (lower-hlsl-compute-specification context entry-point-name)
            (lower-hlsl-traditional-specification
             context entry-point-name interface))
      (setf declarations
            (check-hlsl-structure-names
             (append (hlsl-struct-declarations specification) declarations)))
      (maphash (lambda (expression occurrences)
                 (setf (gethash expression
                                (hlsl-context-expression-occurrences context))
                       (nreverse occurrences)))
               (hlsl-context-expression-occurrences context))
      (let ((document
              (make-instance
               'hlsl-document
               :target target :specification specification
               :declarations declarations :entry-point entry-point
               :source ""
               :expression-occurrences
               (hlsl-context-expression-occurrences context)
               :occurrence-expression
               (hlsl-context-occurrence-expression context))))
        (setf (hlsl-document-source document) (render-hlsl-document document))
        document))))

(defun write-hlsl (document pathname)
  "Write DOCUMENT's deterministic source to PATHNAME and return PATHNAME."
  (check-type document hlsl-document)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (hlsl-document-source document) stream))
  pathname)
