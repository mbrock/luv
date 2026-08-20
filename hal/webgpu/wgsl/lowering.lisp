;;; Direct WebGPU Shading Language lowering for luv's mathematical shaders.
;;;
;;; The shared graph remains the semantic source.  This sibling target owns
;;; WGSL spelling, entry-point/resource ABI, pipeline overrides, structured
;;; statements, and source occurrences.  The first target surface is
;;; deliberately conventional WebGPU: vertex and fragment stages, without
;;; pretending that task or mesh stages exist in standard WGSL.

(in-package #:luv.wgsl)

(defclass wgsl-target ()
  ((overrides
    :initarg :overrides
    :initform nil
    :reader wgsl-target-overrides))
  (:documentation
   "The WebGPU target policy for one lowering.

OVERRIDES is an ordered list of shader source-value symbols which should stay
pipeline-overridable instead of becoming their already checked literal
defaults.  Other source values retain the native folded-literal semantics."))

(defclass wgsl-source-occurrence ()
  ((expression :initarg :expression :reader wgsl-source-occurrence-expression)
   (text :initarg :text :reader wgsl-source-occurrence-text)))

(defclass wgsl-override ()
  ((name :initarg :name :reader wgsl-override-name)
   (identifier :initarg :identifier :reader wgsl-override-identifier)
   (type :initarg :type :reader wgsl-override-type)
   (default :initarg :default :reader wgsl-override-default))
  (:documentation
   "One scalar WGSL override retained with its Lisp source identity."))

(defclass wgsl-variable-statement ()
  ((type :initarg :type :reader wgsl-variable-statement-type)
   (name :initarg :name :reader wgsl-variable-statement-name)
   (value :initarg :value :reader wgsl-variable-statement-value)))

(defclass wgsl-output-statement ()
  ((declaration :initarg :declaration
                :reader wgsl-output-statement-declaration)
   (field :initarg :field :reader wgsl-output-statement-field)
   (value :initarg :value :reader wgsl-output-statement-value)))

(defclass wgsl-if-statement ()
  ((condition :initarg :condition :reader wgsl-if-statement-condition)
   (statements :initarg :statements :reader wgsl-if-statement-statements)))

(defclass wgsl-counted-fold-statement ()
  ((type :initarg :type :reader wgsl-counted-fold-statement-type)
   (state-name :initarg :state-name
               :reader wgsl-counted-fold-statement-state-name)
   (initial :initarg :initial :reader wgsl-counted-fold-statement-initial)
   (index-name :initarg :index-name
               :reader wgsl-counted-fold-statement-index-name)
   (index-type :initarg :index-type
               :reader wgsl-counted-fold-statement-index-type)
   (count :initarg :count :reader wgsl-counted-fold-statement-count)
   (bindings :initarg :bindings :initform nil
             :reader wgsl-counted-fold-statement-bindings)
   (update :initarg :update :reader wgsl-counted-fold-statement-update)
   (until-bindings :initarg :until-bindings :initform nil
                   :reader wgsl-counted-fold-statement-until-bindings)
   (until :initarg :until :initform nil
          :reader wgsl-counted-fold-statement-until)))

(defclass wgsl-document ()
  ((target :initarg :target :reader wgsl-document-target)
   (specification :initarg :specification :reader wgsl-document-specification)
   (source :initarg :source :reader wgsl-document-source)
   (overrides :initarg :overrides :reader wgsl-document-overrides)
   (expression-occurrences
    :initarg :expression-occurrences
    :reader wgsl-document-expression-occurrences)
   (occurrence-expression
    :initarg :occurrence-expression
    :reader wgsl-document-occurrence-expression)))

(defclass wgsl-lowering-context ()
  ((target :initarg :target :reader wgsl-context-target)
   (specification :initarg :specification :reader wgsl-context-specification)
   (references :initform (make-hash-table :test #'eq)
               :reader wgsl-context-references)
   (expression-occurrences :initform (make-hash-table :test #'eq)
                           :reader wgsl-context-expression-occurrences)
   (occurrence-expression :initform (make-hash-table :test #'eq)
                          :reader wgsl-context-occurrence-expression)
   (function-call-results :initform (make-hash-table :test #'eq)
                          :reader wgsl-context-function-call-results)
   (pending-statements :initform nil :accessor wgsl-context-pending-statements)
   (fold-counter :initform 0 :accessor wgsl-context-fold-counter)
   (encountered-overrides :initform (make-hash-table :test #'eq)
                          :reader wgsl-context-encountered-overrides)))

(defun wgsl-identifier (name)
  "Spell NAME as a deterministic non-reserved WGSL identifier."
  (let ((text (string-downcase (string name))))
    (with-output-to-string (stream)
      (when (or (zerop (length text))
                (digit-char-p (char text 0)))
        (write-char #\_ stream))
      (loop for character across text
            do (write-char (if (or (alphanumericp character)
                                   (char= character #\_))
                               character
                               #\_)
                           stream)))))

(defun wgsl-structure-name (name suffix)
  (let ((capitalize-next-p t))
    (with-output-to-string (stream)
      (loop for character across (string-downcase (string name))
            if (alphanumericp character)
              do (write-char (if capitalize-next-p
                                 (char-upcase character)
                                 character)
                             stream)
                 (setf capitalize-next-p nil)
            else do (setf capitalize-next-p t))
      (write-string suffix stream))))

(defun wgsl-type-name (type &optional source-form)
  (case (shader:shader-type-name (shader:find-shader-type type source-form))
    (:bool "bool")
    (:float "f32")
    (:uint "u32")
    (:vec2 "vec2<f32>")
    (:vec3 "vec3<f32>")
    (:vec4 "vec4<f32>")
    (:uvec2 "vec2<u32>")
    (:uvec3 "vec3<u32>")
    (:uvec4 "vec4<u32>")
    (otherwise
     (error 'shader:shader-language-error
            :form source-form :reason :unsupported-wgsl-type
            :details (shader:shader-type-name type)))))

(defun wgsl-float-literal (value)
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

(defun note-wgsl-occurrence (context expression text)
  (let ((occurrence
          (make-instance 'wgsl-source-occurrence
                         :expression expression :text text)))
    (push occurrence
          (gethash expression (wgsl-context-expression-occurrences context)))
    (setf (gethash occurrence (wgsl-context-occurrence-expression context))
          expression)
    occurrence))

(defun wgsl-occurrence-text (occurrence)
  (wgsl-source-occurrence-text occurrence))

(defun drain-wgsl-pending-statements (context)
  (prog1 (wgsl-context-pending-statements context)
    (setf (wgsl-context-pending-statements context) nil)))

(defun wgsl-override-name-p (context name)
  (member name (wgsl-target-overrides (wgsl-context-target context))
          :test #'eq))

(defun ensure-wgsl-override (context expression)
  (let ((name (shader:shader-expression-source-form expression)))
    (or (gethash name (wgsl-context-encountered-overrides context))
        (let ((type (shader:shader-expression-type expression)))
          (unless (= 1 (shader:shader-type-component-count type))
            (error 'shader:shader-language-error
                   :form name :reason :non-scalar-wgsl-override
                   :details (shader:shader-type-name type)))
          (setf (gethash name (wgsl-context-encountered-overrides context))
                (make-instance
                 'wgsl-override
                 :name name
                 :identifier (format nil "knob_~A" (wgsl-identifier name))
                 :type (wgsl-type-name type name)
                 :default (shader:shader-literal-value expression)))))))

(defgeneric lower-wgsl-expression (context expression))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-literal))
  (let ((source (shader:shader-expression-source-form expression)))
    (note-wgsl-occurrence
     context expression
     (if (and (symbolp source) (wgsl-override-name-p context source))
         (wgsl-override-identifier (ensure-wgsl-override context expression))
         (wgsl-float-literal (shader:shader-literal-value expression))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-reference))
  (let* ((target (shader:shader-reference-target expression))
         (text (gethash target (wgsl-context-references context))))
    (cond (text (note-wgsl-occurrence context expression text))
          ((typep target 'shader:shader-function-parameter-binding)
           (let ((argument
                   (lower-wgsl-expression
                    context (shader:shader-binding-expression target))))
             (note-wgsl-occurrence
              context expression (wgsl-occurrence-text argument))))
          (t
           (error 'shader:shader-language-error
                  :form (shader:shader-expression-source-form expression)
                  :reason :unsupported-wgsl-reference
                  :details (shader:shader-object-name target))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-call))
  (shader:lower-shader-call
   (shader:shader-call-operator expression) context expression))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-function-call))
  (multiple-value-bind (cached-result cached-p)
      (gethash expression (wgsl-context-function-call-results context))
    (if cached-p
        (note-wgsl-occurrence context expression cached-result)
        (let ((outer-statements (drain-wgsl-pending-statements context))
              (local-statements nil)
              (saved-references nil))
          (unwind-protect
               (progn
                 (dolist (binding
                          (shader:shader-function-call-bindings expression))
                   (unless (or
                            (typep binding
                                   'shader:shader-function-parameter-binding)
                            (nth-value
                             1 (gethash binding
                                        (wgsl-context-references context))))
                     (multiple-value-bind (old-reference old-reference-p)
                         (gethash binding (wgsl-context-references context))
                       (push (list binding old-reference old-reference-p)
                             saved-references))
                     (let* ((binding-expression
                              (shader:shader-binding-expression binding))
                            (name (wgsl-identifier
                                   (shader:shader-object-name binding)))
                            (value (lower-wgsl-expression
                                    context binding-expression)))
                       (setf local-statements
                             (nconc local-statements
                                    (drain-wgsl-pending-statements context))
                             (gethash binding (wgsl-context-references context))
                             name)
                       (setf local-statements
                             (nconc
                              local-statements
                              (list
                               (make-instance
                                'wgsl-variable-statement
                                :type (wgsl-type-name
                                       (shader:shader-expression-type
                                        binding-expression))
                                :name name :value value)))))))
                 (let* ((result
                          (lower-wgsl-expression
                           context
                           (shader:shader-function-call-result expression)))
                        (result-text (wgsl-occurrence-text result)))
                   (setf local-statements
                         (nconc local-statements
                                (drain-wgsl-pending-statements context))
                         (wgsl-context-pending-statements context)
                         (nconc outer-statements local-statements)
                         (gethash expression
                                  (wgsl-context-function-call-results context))
                         result-text)
                   (note-wgsl-occurrence context expression result-text)))
            (dolist (saved saved-references)
              (destructuring-bind (binding old-reference old-reference-p) saved
                (if old-reference-p
                    (setf (gethash binding (wgsl-context-references context))
                          old-reference)
                    (remhash binding (wgsl-context-references context))))))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-conditional))
  (let ((condition
          (lower-wgsl-expression
           context (lang:arithmetic-conditional-condition expression)))
        (consequent
          (lower-wgsl-expression
           context (lang:arithmetic-conditional-consequent expression)))
        (alternative
          (lower-wgsl-expression
           context (lang:arithmetic-conditional-alternative expression))))
    ;; WGSL has no conditional expression. SELECT is valid here because shader
    ;; expressions are pure; ordered effects live in shader statements.
    (note-wgsl-occurrence
     context expression
     (format nil "select(~A, ~A, ~A)"
             (wgsl-occurrence-text alternative)
             (wgsl-occurrence-text consequent)
             (wgsl-occurrence-text condition)))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-counted-fold))
  (let* ((ordinal (incf (wgsl-context-fold-counter context)))
         (state-name (format nil "fold_state_~D" ordinal))
         (index-name (format nil "fold_index_~D" ordinal))
         (count
           (lower-wgsl-expression
            context (lang:arithmetic-counted-fold-count expression)))
         (initial
           (lower-wgsl-expression
            context (lang:arithmetic-counted-fold-initial expression)))
         (index-binding
           (lang:arithmetic-counted-fold-index-binding expression))
         (state-binding
           (lang:arithmetic-counted-fold-state-binding expression)))
    (multiple-value-bind (old-index old-index-p)
        (gethash index-binding (wgsl-context-references context))
      (multiple-value-bind (old-state old-state-p)
          (gethash state-binding (wgsl-context-references context))
        (setf (gethash index-binding (wgsl-context-references context))
              index-name
              (gethash state-binding (wgsl-context-references context))
              state-name)
        (let* ((preheader-statements (drain-wgsl-pending-statements context))
               (until-expression
                 (lang:arithmetic-counted-fold-until expression))
               (until
                 (and until-expression
                      (lower-wgsl-expression context until-expression)))
               (until-statements
                 (and until (drain-wgsl-pending-statements context)))
               (local-statements nil))
          (dolist (binding (lang:arithmetic-counted-fold-bindings expression))
            (let* ((binding-expression
                     (shader:shader-binding-expression binding))
                   (name (wgsl-identifier (shader:shader-object-name binding)))
                   (value (lower-wgsl-expression context binding-expression)))
              (setf local-statements
                    (nconc local-statements
                           (drain-wgsl-pending-statements context))
                    (gethash binding (wgsl-context-references context)) name)
              (setf local-statements
                    (nconc
                     local-statements
                     (list
                      (make-instance
                       'wgsl-variable-statement
                       :type (wgsl-type-name
                              (shader:shader-expression-type
                               binding-expression))
                       :name name :value value))))))
          (let ((update
                  (lower-wgsl-expression
                   context (lang:arithmetic-counted-fold-update expression))))
            (setf local-statements
                  (nconc local-statements
                         (drain-wgsl-pending-statements context))
                  (wgsl-context-pending-statements context)
                  (nconc
                   preheader-statements
                   (list
                    (make-instance
                     'wgsl-counted-fold-statement
                     :type (wgsl-type-name
                            (shader:shader-expression-type expression))
                     :state-name state-name :initial initial
                     :index-name index-name
                     :index-type
                     (wgsl-type-name
                      (shader:shader-expression-type
                       (lang:arithmetic-counted-fold-count expression)))
                     :count count :bindings local-statements :update update
                     :until-bindings until-statements :until until))))
            (dolist (binding
                     (lang:arithmetic-counted-fold-bindings expression))
              (remhash binding (wgsl-context-references context)))
            (if old-index-p
                (setf (gethash index-binding (wgsl-context-references context))
                      old-index)
                (remhash index-binding (wgsl-context-references context)))
            (if old-state-p
                (setf (gethash state-binding (wgsl-context-references context))
                      old-state)
                (remhash state-binding (wgsl-context-references context)))))))
    (note-wgsl-occurrence context expression state-name)))

(defun lower-wgsl-quantity-boundary (context expression operand)
  (let ((lowered (lower-wgsl-expression context operand)))
    (note-wgsl-occurrence context expression (wgsl-occurrence-text lowered))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-interpretation))
  (lower-wgsl-quantity-boundary
   context expression (shader:shader-interpretation-operand expression)))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context)
     (expression shader:shader-quantity-construction))
  (lower-wgsl-quantity-boundary
   context expression (shader:shader-quantity-construction-operand expression)))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context)
     (expression shader:shader-quantity-assumption))
  (lower-wgsl-quantity-boundary
   context expression (shader:shader-quantity-assumption-operand expression)))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-representation))
  (lower-wgsl-quantity-boundary
   context expression (shader:shader-representation-operand expression)))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-unit-conversion))
  (let* ((operand
           (lower-wgsl-expression
            context (shader:shader-unit-conversion-operand expression)))
         (factor (shader:shader-unit-conversion-factor expression)))
    (note-wgsl-occurrence
     context expression
     (if (= factor 1)
         (wgsl-occurrence-text operand)
         (format nil "(~A * ~A)"
                 (wgsl-occurrence-text operand)
                 (wgsl-float-literal factor))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-expression))
  (declare (ignore context))
  (error 'shader:shader-language-error
         :form (shader:shader-expression-source-form expression)
         :reason :unsupported-wgsl-expression
         :details (class-name (class-of expression))))

(defun lower-wgsl-operands (context expression)
  (mapcar (lambda (operand) (lower-wgsl-expression context operand))
          (shader:shader-call-operands expression)))

(defun lower-wgsl-infix-call (context expression operator)
  (let ((operands (mapcar #'wgsl-occurrence-text
                          (lower-wgsl-operands context expression))))
    (note-wgsl-occurrence
     context expression
     (cond ((and (string= operator "-") (= (length operands) 1))
            (format nil "(-~A)" (first operands)))
           ((= (length operands) 1) (format nil "(~A)" (first operands)))
           (t
            (reduce (lambda (left right)
                      (format nil "(~A ~A ~A)" left operator right))
                    (rest operands) :initial-value (first operands)))))))

(defun lower-wgsl-function-call (context expression name)
  (let ((operands (mapcar #'wgsl-occurrence-text
                          (lower-wgsl-operands context expression))))
    (note-wgsl-occurrence
     context expression (format nil "~A(~{~A~^, ~})" name operands))))

(defun lower-wgsl-chained-function-call (context expression name)
  (let ((operands (mapcar #'wgsl-occurrence-text
                          (lower-wgsl-operands context expression))))
    (note-wgsl-occurrence
     context expression
     (reduce (lambda (left right) (format nil "~A(~A, ~A)" name left right))
             (rest operands) :initial-value (first operands)))))

(defmethod shader:lower-shader-call
    (operator (context wgsl-lowering-context) expression)
  (error 'shader:shader-language-error
         :form (shader:shader-expression-source-form expression)
         :reason :unsupported-wgsl-operator :details operator))

(defmacro define-wgsl-infix-operator (operator text)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (context wgsl-lowering-context)
        (expression shader:shader-call))
     (declare (ignore operator))
     (lower-wgsl-infix-call context expression ,text)))

(define-wgsl-infix-operator + "+")
(define-wgsl-infix-operator - "-")
(define-wgsl-infix-operator * "*")
(define-wgsl-infix-operator / "/")
(define-wgsl-infix-operator mod "%")
(define-wgsl-infix-operator < "<")
(define-wgsl-infix-operator <= "<=")
(define-wgsl-infix-operator > ">")
(define-wgsl-infix-operator >= ">=")
(define-wgsl-infix-operator = "==")

(defmacro define-wgsl-function-operator (operator name)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (context wgsl-lowering-context)
        (expression shader:shader-call))
     (declare (ignore operator))
     (lower-wgsl-function-call context expression ,name)))

(define-wgsl-function-operator shader:dot "dot")
(define-wgsl-function-operator shader:mix "mix")
(define-wgsl-function-operator abs "abs")
(define-wgsl-function-operator signum "sign")
(define-wgsl-function-operator sqrt "sqrt")
(define-wgsl-function-operator shader:derivative-x "dpdx")
(define-wgsl-function-operator shader:derivative-y "dpdy")
(define-wgsl-function-operator expt "pow")
(define-wgsl-function-operator shader:clamp "clamp")
(define-wgsl-function-operator shader:smoothstep "smoothstep")
(define-wgsl-function-operator shader:step "step")
(define-wgsl-function-operator shader:normalize "normalize")
(define-wgsl-function-operator floor "floor")
(define-wgsl-function-operator shader:fract "fract")
(define-wgsl-function-operator sin "sin")
(define-wgsl-function-operator cos "cos")
(define-wgsl-function-operator exp "exp")
(define-wgsl-function-operator log "log")

(defmacro define-wgsl-chained-function-operator (operator name)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (context wgsl-lowering-context)
        (expression shader:shader-call))
     (declare (ignore operator))
     (lower-wgsl-chained-function-call context expression ,name)))

(define-wgsl-chained-function-operator min "min")
(define-wgsl-chained-function-operator max "max")

(defun lower-wgsl-vector-constructor (context expression)
  (let ((operands (mapcar #'wgsl-occurrence-text
                          (lower-wgsl-operands context expression))))
    (note-wgsl-occurrence
     context expression
     (format nil "~A(~{~A~^, ~})"
             (wgsl-type-name (shader:shader-expression-type expression)
                             (shader:shader-expression-source-form expression))
             operands))))

(defmacro define-wgsl-vector-constructor (operator)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (context wgsl-lowering-context)
        (expression shader:shader-call))
     (declare (ignore operator))
     (lower-wgsl-vector-constructor context expression)))

(define-wgsl-vector-constructor shader:vec2)
(define-wgsl-vector-constructor shader:vec3)
(define-wgsl-vector-constructor shader:vec4)
(define-wgsl-vector-constructor shader:uvec2)
(define-wgsl-vector-constructor shader:uvec3)
(define-wgsl-vector-constructor shader:uvec4)

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:uint))
     (context wgsl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (lower-wgsl-function-call context expression "u32"))

(defmethod shader:lower-shader-call
    ((operator (eql 'float))
     (context wgsl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (lower-wgsl-function-call context expression "f32"))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:swizzle))
     (context wgsl-lowering-context)
     (expression shader:shader-call))
  (declare (ignore operator))
  (let* ((operand
           (lower-wgsl-expression
            context (first (shader:shader-call-operands expression))))
         (components
           (string-downcase
            (string (first (shader:shader-call-parameters expression))))))
    (note-wgsl-occurrence
     context expression
     (format nil "~A.~A" (wgsl-occurrence-text operand) components))))

(defun wgsl-interface-attribute (declaration)
  (let ((location (shader:shader-interface-location declaration))
        (built-in (shader:shader-interface-built-in declaration))
        (interpolation (shader:shader-interface-interpolation declaration))
        (source-form (shader:shader-object-source-form declaration)))
    (cond (built-in
           (format nil "@builtin(~A)"
                   (case built-in
                     (:position "position")
                     (:vertex-index "vertex_index")
                     (otherwise
                      (error 'shader:shader-language-error
                             :form source-form
                             :reason :unsupported-wgsl-built-in
                             :details built-in)))))
          (location
           (format nil "@location(~D)~@[ @interpolate(~A)~]"
                   location (case interpolation (:flat "flat"))))
          (t
           (error 'shader:shader-language-error
                  :form source-form :reason :undecorated-wgsl-interface)))))

(defun write-wgsl-interface-structure
    (stream specification suffix declarations)
  (let ((name (wgsl-structure-name
               (shader:shader-object-name specification) suffix)))
    (format stream "struct ~A {~%" name)
    (dolist (declaration declarations)
      (format stream "  ~A ~A: ~A,~%"
              (wgsl-interface-attribute declaration)
              (wgsl-identifier (shader:shader-object-name declaration))
              (wgsl-type-name
               (shader:shader-declaration-type declaration)
               (shader:shader-object-source-form declaration))))
    (format stream "}~%~%")
    name))

(defun write-wgsl-uniform-block (stream resource)
  (let ((structure-name
          (wgsl-structure-name (shader:shader-object-name resource) ""))
        (resource-name (wgsl-identifier (shader:shader-object-name resource))))
    (format stream "struct ~A {~%" structure-name)
    (dolist (member (shader:shader-uniform-block-members resource))
      (format stream "  ~A: ~A,~%"
              (wgsl-identifier (shader:shader-object-name member))
              (wgsl-type-name
               (shader:shader-declaration-type member)
               (shader:shader-object-source-form member))))
    (format stream "}~%")
    (format stream "@group(~D) @binding(~D) var<uniform> ~A: ~A;~%~%"
            (shader:shader-resource-descriptor-set resource)
            (shader:shader-resource-binding resource)
            resource-name structure-name)))

(defun register-wgsl-references (context specification)
  (dolist (input (shader:shader-specification-inputs specification))
    (setf (gethash input (wgsl-context-references context))
          (format nil "stage_in.~A"
                  (wgsl-identifier (shader:shader-object-name input)))))
  (dolist (resource (shader:shader-specification-resources specification))
    (unless (typep resource 'shader:shader-uniform-block)
      (error 'shader:shader-language-error
             :form (shader:shader-object-source-form resource)
             :reason :unsupported-wgsl-resource
             :details (shader:shader-type-opaque-kind
                       (shader:shader-declaration-type resource))))
    (let ((resource-name
            (wgsl-identifier (shader:shader-object-name resource))))
      (setf (gethash resource (wgsl-context-references context)) resource-name)
      (dolist (member (shader:shader-uniform-block-members resource))
        (setf (gethash member (wgsl-context-references context))
              (format nil "~A.~A" resource-name
                      (wgsl-identifier
                       (shader:shader-object-name member)))))))
  context)

(defgeneric lower-wgsl-statement (context statement))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context)
     (statement shader:shader-output-assignment))
  (let ((value
          (lower-wgsl-expression
           context (shader:shader-assignment-value statement))))
    (append
     (drain-wgsl-pending-statements context)
     (list
      (make-instance
       'wgsl-output-statement
       :declaration (shader:shader-assignment-output statement)
       :field (wgsl-identifier
               (shader:shader-object-name
                (shader:shader-assignment-output statement)))
       :value value)))))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context)
     (statement shader:shader-conditional-statement))
  (let ((condition
          (lower-wgsl-expression
           context (shader:shader-conditional-statement-condition statement))))
    (append
     (drain-wgsl-pending-statements context)
     (list
      (make-instance
       'wgsl-if-statement :condition condition
       :statements
       (mapcan (lambda (child) (lower-wgsl-statement context child))
               (shader:shader-conditional-statement-statements statement)))))))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context) (statement shader:shader-statement))
  (declare (ignore context))
  (error 'shader:shader-language-error
         :form (shader:shader-statement-source-form statement)
         :reason :unsupported-wgsl-statement
         :details (class-name (class-of statement))))

(defun wgsl-index-zero (type)
  (if (string= type "u32") "0u" "0.0f"))

(defun wgsl-index-one (type)
  (if (string= type "u32") "1u" "1.0f"))

(defvar *wgsl-statement-indentation* 1)

(defun write-wgsl-indent (stream &optional (extra 0))
  (loop repeat (+ *wgsl-statement-indentation* extra)
        do (write-string "  " stream)))

(defgeneric write-wgsl-statement-form (statement stream))

(defmethod write-wgsl-statement-form
    ((statement wgsl-variable-statement) stream)
  (write-wgsl-indent stream)
  (format stream "let ~A: ~A = ~A;~%"
          (wgsl-variable-statement-name statement)
          (wgsl-variable-statement-type statement)
          (wgsl-occurrence-text (wgsl-variable-statement-value statement))))

(defmethod write-wgsl-statement-form
    ((statement wgsl-output-statement) stream)
  (let* ((declaration (wgsl-output-statement-declaration statement))
         (value (wgsl-occurrence-text
                 (wgsl-output-statement-value statement)))
         (text
           (if (eq :position (shader:shader-interface-built-in declaration))
               ;; The shared camera graph intentionally retains Vulkan's
               ;; framebuffer-oriented clip Y. WebGPU owns this conversion.
               (format nil "vec4<f32>((~A).x, -(~A).y, (~A).z, (~A).w)"
                       value value value value)
               value)))
    (write-wgsl-indent stream)
    (format stream "result.~A = ~A;~%"
            (wgsl-output-statement-field statement) text)))

(defmethod write-wgsl-statement-form ((statement wgsl-if-statement) stream)
  (write-wgsl-indent stream)
  (format stream "if (~A) {~%"
          (wgsl-occurrence-text (wgsl-if-statement-condition statement)))
  (let ((*wgsl-statement-indentation* (1+ *wgsl-statement-indentation*)))
    (dolist (child (wgsl-if-statement-statements statement))
      (write-wgsl-statement-form child stream)))
  (write-wgsl-indent stream)
  (format stream "}~%"))

(defmethod write-wgsl-statement-form
    ((statement wgsl-counted-fold-statement) stream)
  (write-wgsl-indent stream)
  (format stream "var ~A: ~A = ~A;~%"
          (wgsl-counted-fold-statement-state-name statement)
          (wgsl-counted-fold-statement-type statement)
          (wgsl-occurrence-text
           (wgsl-counted-fold-statement-initial statement)))
  (write-wgsl-indent stream)
  (format stream
          "for (var ~A: ~A = ~A; ~A < ~A; ~A = ~A + ~A) {~%"
          (wgsl-counted-fold-statement-index-name statement)
          (wgsl-counted-fold-statement-index-type statement)
          (wgsl-index-zero (wgsl-counted-fold-statement-index-type statement))
          (wgsl-counted-fold-statement-index-name statement)
          (wgsl-occurrence-text (wgsl-counted-fold-statement-count statement))
          (wgsl-counted-fold-statement-index-name statement)
          (wgsl-counted-fold-statement-index-name statement)
          (wgsl-index-one (wgsl-counted-fold-statement-index-type statement)))
  (let ((*wgsl-statement-indentation* (1+ *wgsl-statement-indentation*)))
    (dolist (binding (wgsl-counted-fold-statement-until-bindings statement))
      (write-wgsl-statement-form binding stream))
    (when (wgsl-counted-fold-statement-until statement)
      (write-wgsl-indent stream)
      (format stream "if (~A) { break; }~%"
              (wgsl-occurrence-text
               (wgsl-counted-fold-statement-until statement))))
    (dolist (binding (wgsl-counted-fold-statement-bindings statement))
      (write-wgsl-statement-form binding stream))
    (write-wgsl-indent stream)
    (format stream "~A = ~A;~%"
            (wgsl-counted-fold-statement-state-name statement)
            (wgsl-occurrence-text
             (wgsl-counted-fold-statement-update statement))))
  (write-wgsl-indent stream)
  (format stream "}~%"))

(defun encountered-wgsl-overrides (context)
  (loop for name in (wgsl-target-overrides (wgsl-context-target context))
        for override = (gethash name
                                (wgsl-context-encountered-overrides context))
        when override collect override))

(defun render-wgsl-document (context statements)
  (let* ((specification (wgsl-context-specification context))
         (stage (shader:shader-specification-stage specification))
         (inputs (shader:shader-specification-inputs specification))
         (outputs (shader:shader-specification-outputs specification)))
    (unless (member stage '(:vertex :fragment))
      (error 'shader:shader-language-error
             :form (shader:shader-object-source-form specification)
             :reason :unsupported-wgsl-stage :details stage))
    (with-output-to-string (stream)
      (dolist (override (encountered-wgsl-overrides context))
        (format stream "override ~A: ~A = ~A;~%"
                (wgsl-override-identifier override)
                (wgsl-override-type override)
                (wgsl-float-literal (wgsl-override-default override))))
      (when (encountered-wgsl-overrides context) (terpri stream))
      (dolist (resource (shader:shader-specification-resources specification))
        (write-wgsl-uniform-block stream resource))
      (let ((input-name
              (write-wgsl-interface-structure
               stream specification "Input" inputs))
            (output-name
              (write-wgsl-interface-structure
               stream specification "Output" outputs)))
        (format stream "@~(~A~)~%fn ~A(stage_in: ~A) -> ~A {~%"
                stage
                (wgsl-identifier (shader:shader-object-name specification))
                input-name output-name)
        (format stream "  var result: ~A;~%" output-name)
        (let ((*wgsl-statement-indentation* 1))
          (dolist (statement statements)
            (write-wgsl-statement-form statement stream)))
        (format stream "  return result;~%}~%")))))

(defmethod shader:lower-shader-specification
    ((target wgsl-target) (specification shader:shader-specification))
  "Lower the shared shader graph directly to deterministic WGSL."
  (let ((context (make-instance 'wgsl-lowering-context
                                :target target
                                :specification specification)))
    (unless (member (shader:shader-specification-stage specification)
                    '(:vertex :fragment))
      (error 'shader:shader-language-error
             :form (shader:shader-object-source-form specification)
             :reason :unsupported-wgsl-stage
             :details (shader:shader-specification-stage specification)))
    (register-wgsl-references context specification)
    (let ((statements nil))
      (dolist (binding (shader:shader-specification-bindings specification))
        (let* ((expression (shader:shader-binding-expression binding))
               (name (wgsl-identifier (shader:shader-object-name binding)))
               (value (lower-wgsl-expression context expression)))
          (setf statements
                (nconc statements (drain-wgsl-pending-statements context))
                (gethash binding (wgsl-context-references context)) name)
          (setf statements
                (nconc statements
                       (list
                        (make-instance
                         'wgsl-variable-statement
                         :type (wgsl-type-name
                                (shader:shader-expression-type expression))
                         :name name :value value))))))
      (dolist (statement (shader:shader-specification-statements specification))
        (setf statements
              (nconc statements (lower-wgsl-statement context statement))))
      (maphash (lambda (expression occurrences)
                 (setf (gethash expression
                                (wgsl-context-expression-occurrences context))
                       (nreverse occurrences)))
               (wgsl-context-expression-occurrences context))
      (make-instance
       'wgsl-document
       :target target :specification specification
       :source (render-wgsl-document context statements)
       :overrides (encountered-wgsl-overrides context)
       :expression-occurrences (wgsl-context-expression-occurrences context)
       :occurrence-expression (wgsl-context-occurrence-expression context)))))

(defun compile-wgsl (specification &key overrides)
  (shader:lower-shader-specification
   (make-instance 'wgsl-target :overrides overrides) specification))

(defun write-wgsl (document pathname)
  (check-type document wgsl-document)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (wgsl-document-source document) stream))
  pathname)
