;;; Direct WebGPU Shading Language lowering for luv's mathematical shaders.
;;;
;;; A sibling of the MSL and HLSL lowerings: the shared graph remains the
;;; semantic source, the product is one module of text, and every rendered
;;; expression occurrence keeps the shader expression it came from.  The
;;; dialect is standard WGSL as Chrome's Tint reads it: no enable
;;; extension, and one language extension, required only by a module that
;;; reads a storage texture.  This target also owns pipeline overrides: a
;;; named source value may stay an override constant instead of folding to
;;; its literal.
;;;
;;; A uniform block is a var<uniform> of one structure; a storage buffer a
;;; var<storage, read> or var<storage, read_write> of a runtime-sized
;;; array; a texture, storage texture, or sampler a handle variable.  Each
;;; sits at the @group and @binding its declaration names, or where the
;;; caller's RESOURCE-BINDING function places it.  Vertex, fragment, and
;;; compute stages lower.  Task and mesh stages, 64-bit integers, and wave
;;; operations do not: standard WGSL has none of them.
;;;
;;; WGSL converts nothing implicitly, and the language's own typing is as
;;; strict: operands of one componentwise operation already share a type,
;;; except where * and / scale a vector by a scalar and MIX blends by one,
;;; which WGSL accepts as written.

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

(defclass wgsl-line-statement ()
  ((text :initarg :text :reader wgsl-line-statement-text))
  (:documentation "One rendered statement line, such as a store or a call."))

(defclass wgsl-document ()
  ((target :initarg :target :reader wgsl-document-target)
   (specification :initarg :specification :reader wgsl-document-specification)
   (entry-point-name :initarg :entry-point-name
                     :reader wgsl-document-entry-point-name)
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
   (comparison-samplers
    :initarg :comparison-samplers :initform nil
    :reader wgsl-context-comparison-samplers
    :documentation "The keys of samplers declared sampler_comparison.")
   (resource-binding
    :initarg :resource-binding :initform nil
    :reader wgsl-context-resource-binding
    :documentation "NIL, or a function of a resource declaration returning
its group and binding in place of the declared ones.")
   (storage-texture-access
    :initarg :storage-texture-access :initform nil
    :reader wgsl-context-storage-texture-access
    :documentation "NIL, or a function of a storage texture declaration
returning :READ, :WRITE, or :READ-WRITE in place of the module's own use.")
   (atomic-targets
    :initarg :atomic-targets :initform nil
    :reader wgsl-context-atomic-targets
    :documentation "Buffers and workgroup arrays some atomic touches: their
elements are atomic<u32>, so every access to them is atomic.")
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
   (declared-names
    :initform (make-hash-table :test #'equal)
    :reader wgsl-context-declared-names
    :documentation "Every name the module or its entry function declares,
so a later local neither redeclares one nor hides a resource.")
   (temporary-counter :initform 0 :accessor wgsl-context-temporary-counter)
   (encountered-overrides :initform (make-hash-table :test #'eq)
                          :reader wgsl-context-encountered-overrides)))

(defun wgsl-context-stage (context)
  (shader:shader-specification-stage (wgsl-context-specification context)))

(defun wgsl-failure (form reason &optional details)
  (error 'shader:shader-language-error
         :form form :reason reason :details details))

;;; Names.

(defparameter *wgsl-keywords*
  '("alias" "break" "case" "const" "const_assert" "continue" "continuing"
    "default" "diagnostic" "discard" "else" "enable" "false" "fn" "for" "if"
    "let" "loop" "override" "requires" "return" "struct" "switch" "true"
    "var" "while")
  "WGSL's keywords, from the specification's keyword summary.")

(defparameter *wgsl-reserved-words*
  '("NULL" "Self" "abstract" "active" "alignas" "alignof" "as" "asm"
    "asm_fragment" "async" "attribute" "auto" "await" "become" "cast"
    "catch" "class" "co_await" "co_return" "co_yield" "coherent"
    "column_major" "common" "compile" "compile_fragment" "concept"
    "const_cast" "consteval" "constexpr" "constinit" "crate" "debugger"
    "decltype" "delete" "demote" "demote_to_helper" "do" "dynamic_cast"
    "enum" "explicit" "export" "extends" "extern" "external" "fallthrough"
    "filter" "final" "finally" "friend" "from" "fxgroup" "get" "goto"
    "groupshared" "highp" "impl" "implements" "import" "inline"
    "instanceof" "interface" "layout" "lowp" "macro" "macro_rules" "match"
    "mediump" "meta" "mod" "module" "move" "mut" "mutable" "namespace"
    "new" "nil" "noexcept" "noinline" "nointerpolation" "non_coherent"
    "noncoherent" "noperspective" "null" "nullptr" "of" "operator"
    "package" "packoffset" "partition" "pass" "patch" "pixelfragment"
    "precise" "precision" "premerge" "priv" "protected" "pub" "public"
    "readonly" "ref" "regardless" "register" "reinterpret_cast" "require"
    "resource" "restrict" "self" "set" "shared" "sizeof" "smooth" "snorm"
    "static" "static_assert" "static_cast" "std" "subroutine" "super"
    "target" "template" "this" "thread_local" "throw" "trait" "try" "type"
    "typedef" "typeid" "typename" "typeof" "union" "unless" "unorm"
    "unsafe" "unsized" "use" "using" "varying" "virtual" "volatile" "wgsl"
    "where" "with" "writeonly" "yield")
  "WGSL's reserved words, from the specification: no module may spell one.")

(defparameter *wgsl-predeclared-names*
  '(;; Types and type generators this lowering writes.
    "bool" "f32" "i32" "u32" "vec2" "vec3" "vec4" "mat2x2" "mat3x3"
    "mat4x4" "array" "atomic" "sampler" "sampler_comparison" "texture_2d"
    "texture_2d_array" "texture_3d" "texture_cube" "texture_depth_2d"
    "texture_depth_2d_array" "texture_storage_2d"
    ;; Built-in functions this lowering calls.  The others are spelled in
    ;; camelCase, which no shader name becomes.
    "abs" "all" "any" "bitcast" "clamp" "cos" "dot" "dpdx" "dpdy" "exp"
    "floor" "fract" "log" "max" "min" "mix" "normalize" "pow" "select"
    "sign" "sin" "smoothstep" "sqrt" "step" "transpose")
  "Predeclared names a declaration of the same name would hide from the
code this lowering writes after it.")

(defparameter *wgsl-own-names* '("result" "stage_in")
  "Names every entry function declares for itself.")

(defun wgsl-undeclarable-name-p (identifier)
  (flet ((among (words) (member identifier words :test #'string=)))
    (or (among *wgsl-keywords*) (among *wgsl-reserved-words*)
        (among *wgsl-predeclared-names*) (among *wgsl-own-names*))))

(defun wgsl-name-spelling (name)
  "NAME in lower case with an underscore for every other character."
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

(defun wgsl-identifier (name)
  "Spell NAME as a deterministic WGSL identifier the module may declare.
A word WGSL keeps for itself, or a predeclared name this lowering uses,
gains a trailing underscore; an identifier may not begin with two."
  (let ((identifier (wgsl-name-spelling name)))
    (cond ((wgsl-undeclarable-name-p identifier)
           (concatenate 'string identifier "_"))
          ((or (string= identifier "_")
               (and (< 1 (length identifier))
                    (string= "__" identifier :end2 2)))
           (concatenate 'string "v" identifier))
          (t identifier))))

(defun wgsl-structure-name (name suffix)
  "NAME in CamelCase with SUFFIX, as structures are named.  One reserved
word is spelled that way: a structure named SELF is Self_."
  (let* ((capitalize-next-p t)
         (structure-name
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
    (if (wgsl-undeclarable-name-p structure-name)
        (concatenate 'string structure-name "_")
        structure-name)))

(defun declare-wgsl-name (context base)
  "Claim a name nothing in the module or its entry function has declared:
BASE, or BASE with the first free ordinal.  WGSL forbids redeclaring a name
in one scope, and a local that repeated a resource's name would hide it."
  (let ((names (wgsl-context-declared-names context)))
    (if (gethash base names)
        (loop for ordinal from 2
              for candidate = (format nil "~A_~D" base ordinal)
              unless (gethash candidate names)
                do (setf (gethash candidate names) t)
                   (return candidate))
        (progn (setf (gethash base names) t) base))))

(defun wgsl-temporary-name (context prefix)
  (declare-wgsl-name
   context
   (format nil "~A_~D" prefix
           (incf (wgsl-context-temporary-counter context)))))

(defun declare-wgsl-module-name (context name form)
  "Claim NAME for a module-scope declaration, which nothing may share."
  (when (gethash name (wgsl-context-declared-names context))
    (wgsl-failure form :wgsl-module-name-collision name))
  (setf (gethash name (wgsl-context-declared-names context)) t)
  name)

;;; Types and literals.

(defun wgsl-type-name (type &optional source-form)
  "TYPE as the WGSL type of a value: a scalar, vector, matrix, or structure."
  (let ((type (shader:find-shader-type type source-form)))
    (when (shader:shader-struct-type-p type)
      (return-from wgsl-type-name
        (wgsl-structure-name (shader:shader-type-name type) "")))
    (case (shader:shader-type-name type)
      (:bool "bool")
      (:float "f32")
      (:uint "u32")
      (:vec2 "vec2<f32>")
      (:vec3 "vec3<f32>")
      (:vec4 "vec4<f32>")
      (:uvec2 "vec2<u32>")
      (:uvec3 "vec3<u32>")
      (:uvec4 "vec4<u32>")
      (:int "i32")
      (:ivec2 "vec2<i32>")
      (:ivec3 "vec3<i32>")
      (:ivec4 "vec4<i32>")
      (:bvec2 "vec2<bool>")
      (:bvec3 "vec3<bool>")
      (:bvec4 "vec4<bool>")
      (:mat2 "mat2x2<f32>")
      (:mat3 "mat3x3<f32>")
      (:mat4 "mat4x4<f32>")
      (:uint64
       (wgsl-failure source-form :unsupported-wgsl-64-bit-integer
                     (shader:shader-type-name type)))
      (otherwise
       (wgsl-failure source-form :unsupported-wgsl-type
                     (shader:shader-type-name type))))))

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

(defun wgsl-scalar-literal (type value)
  "VALUE as a WGSL literal of scalar TYPE."
  (ecase (shader:shader-type-scalar-kind (shader:find-shader-type type))
    (:float (wgsl-float-literal value))
    (:uint (format nil "~Du" value))
    (:int (cond ((= value (- (expt 2 31))) "i32(-2147483648)")
                ((minusp value) (format nil "(~Di)" value))
                (t (format nil "~Di" value))))
    (:bool (if value "true" "false"))))

;;; Host layouts.  WGSL lays structures out by its own alignment rules; a
;;; buffer the host fills must come out exactly as the language laid it.

(defun wgsl-type-layout (type)
  "The byte size and alignment WGSL gives a host-shareable value of TYPE,
or NIL for a type WGSL cannot place in a buffer."
  (let ((type (shader:find-shader-type type)))
    (cond
      ((shader:shader-struct-type-p type)
       (let ((offset 0) (alignment 1))
         (dolist (field (shader:shader-struct-type-fields type))
           (multiple-value-bind (size field-alignment)
               (wgsl-type-layout (shader:shader-struct-field-type field))
             (unless size
               (return-from wgsl-type-layout nil))
             (setf offset (+ (* field-alignment
                                (ceiling offset field-alignment))
                             size)
                   alignment (max alignment field-alignment))))
         (values (* alignment (ceiling offset alignment)) alignment)))
      ((shader:shader-matrix-type-p type)
       (ecase (shader:shader-type-column-count type)
         (2 (values 16 8))
         (3 (values 48 16))
         (4 (values 64 16))))
      ((and (member (shader:shader-type-scalar-kind type) '(:float :uint :int))
            (eql 32 (shader:shader-type-bit-width type)))
       (ecase (shader:shader-type-component-count type)
         (1 (values 4 4))
         (2 (values 8 8))
         (3 (values 12 16))
         (4 (values 16 16)))))))

(defun check-wgsl-struct-layout (struct form)
  "Signal unless WGSL places every field of STRUCT where the host does."
  (let ((offset 0))
    (flet ((differ (&rest details)
             (wgsl-failure form :wgsl-layout-differs-from-host
                           (cons (shader:shader-type-name struct) details))))
      (dolist (field (shader:shader-struct-type-fields struct))
        (let ((field-type (shader:shader-struct-field-type field)))
          (multiple-value-bind (size alignment) (wgsl-type-layout field-type)
            (unless size
              (differ (shader:shader-object-name field)))
            (setf offset (* alignment (ceiling offset alignment)))
            (unless (eql offset (shader:shader-struct-field-offset field))
              (differ (shader:shader-object-name field)
                      :wgsl-offset offset
                      :host-offset (shader:shader-struct-field-offset field)))
            (when (shader:shader-struct-type-p field-type)
              (check-wgsl-struct-layout
               (shader:find-shader-type field-type) form))
            (incf offset size))))
      (let ((size (wgsl-type-layout struct)))
        (unless (eql size (shader:shader-struct-type-size struct))
          (differ :wgsl-size size
                  :host-size (shader:shader-struct-type-size struct)))))))

(defun check-wgsl-resource-layout (resource)
  "Signal unless RESOURCE's buffer has in WGSL the layout its host writes:
a uniform block's member offsets, a storage buffer's element stride."
  (let ((form (shader:shader-object-source-form resource)))
    (typecase resource
      (shader:shader-uniform-block
       (let ((offset 0))
         (dolist (member (shader:shader-uniform-block-members resource))
           (multiple-value-bind (size alignment)
               (wgsl-type-layout (shader:shader-declaration-type member))
             (setf offset (* alignment (ceiling offset alignment)))
             (unless (eql offset (shader:shader-uniform-member-offset member))
               (wgsl-failure
                form :wgsl-layout-differs-from-host
                (list (shader:shader-object-name member)
                      :wgsl-offset offset
                      :host-offset
                      (shader:shader-uniform-member-offset member))))
             (incf offset size)))))
      (shader:shader-storage-buffer
       (let ((element (shader:find-shader-type
                       (shader:shader-storage-buffer-element-type resource))))
         (when (shader:shader-struct-type-p element)
           (check-wgsl-struct-layout element form))
         (multiple-value-bind (size alignment) (wgsl-type-layout element)
           (let ((stride (and size (* alignment (ceiling size alignment)))))
             (unless (eql stride
                          (shader:shader-storage-buffer-element-stride
                           resource))
               (wgsl-failure
                form :wgsl-layout-differs-from-host
                (list (shader:shader-object-name resource)
                      :wgsl-stride stride
                      :host-stride
                      (shader:shader-storage-buffer-element-stride
                       resource)))))))))))

;;; Occurrences and pending statements.

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

(defun wgsl-line (control &rest arguments)
  (make-instance 'wgsl-line-statement
                 :text (apply #'format nil control arguments)))

(defun push-wgsl-pending (context &rest statements)
  (setf (wgsl-context-pending-statements context)
        (append (wgsl-context-pending-statements context) statements)))

;;; Overrides.

(defun wgsl-override-name-p (context name)
  (member name (wgsl-target-overrides (wgsl-context-target context))
          :test #'eq))

(defun wgsl-override-identifier-for (name)
  ;; The prefix already keeps the name clear of every word WGSL reserves.
  (format nil "knob_~A" (wgsl-name-spelling name)))

(defun ensure-wgsl-override (context expression)
  (let ((name (shader:shader-expression-source-form expression)))
    (or (gethash name (wgsl-context-encountered-overrides context))
        (let ((type (shader:shader-expression-type expression)))
          (unless (= 1 (shader:shader-type-component-count type))
            (wgsl-failure name :non-scalar-wgsl-override
                          (shader:shader-type-name type)))
          (setf (gethash name (wgsl-context-encountered-overrides context))
                (make-instance
                 'wgsl-override
                 :name name
                 :identifier (wgsl-override-identifier-for name)
                 :type (wgsl-type-name type name)
                 :default (shader:shader-literal-value expression)))))))

(defun encountered-wgsl-overrides (context)
  (loop for name in (wgsl-target-overrides (wgsl-context-target context))
        for override = (gethash name
                                (wgsl-context-encountered-overrides context))
        when override collect override))

;;; Expressions.

(defgeneric lower-wgsl-expression (context expression)
  (:documentation
   "Render one shader EXPRESSION and retain a source occurrence for it."))

(defun wgsl-text (context expression)
  "EXPRESSION lowered in CONTEXT, as text."
  (wgsl-occurrence-text (lower-wgsl-expression context expression)))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-literal))
  (let ((source (shader:shader-expression-source-form expression)))
    (note-wgsl-occurrence
     context expression
     (if (and (symbolp source) (wgsl-override-name-p context source))
         (wgsl-override-identifier (ensure-wgsl-override context expression))
         (wgsl-scalar-literal (shader:shader-expression-type expression)
                              (shader:shader-literal-value expression))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-reference))
  (let* ((target (shader:shader-reference-target expression))
         (text (gethash target (wgsl-context-references context))))
    (cond (text (note-wgsl-occurrence context expression text))
          ((typep target 'shader:shader-function-parameter-binding)
           (note-wgsl-occurrence
            context expression
            (wgsl-text context (shader:shader-binding-expression target))))
          (t
           (wgsl-failure (shader:shader-expression-source-form expression)
                         :unsupported-wgsl-reference
                         (shader:shader-object-name target))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-call))
  (shader:lower-shader-call
   (shader:shader-call-operator expression) context expression))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context)
     (expression shader:shader-struct-construction))
  ;; #V16OXI
  (note-wgsl-occurrence
   context expression
   (format nil "~A(~{~A~^, ~})"
           (wgsl-type-name (shader:shader-expression-type expression))
           (mapcar (lambda (value) (wgsl-text context value))
                   (shader:shader-struct-construction-values expression)))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context)
     (expression shader:shader-struct-field-read))
  (note-wgsl-occurrence
   context expression
   (format nil "~A.~A"
           (wgsl-text context
                      (shader:shader-struct-field-read-operand expression))
           (wgsl-identifier
            (shader:shader-object-name
             (shader:shader-struct-field-read-field expression))))))

(defun lower-wgsl-local-binding (context binding)
  "Lower BINDING to a let declaration under a name of its own, returning
the statements it needs.  A texture or sampler cannot be a WGSL value, so a
binding of one stands for the resource it names."
  (let* ((expression (shader:shader-binding-expression binding))
         (type (shader:shader-expression-type expression))
         (value (lower-wgsl-expression context expression))
         (statements (drain-wgsl-pending-statements context)))
    (if (shader:shader-type-opaque-kind type)
        (progn
          (setf (gethash binding (wgsl-context-references context))
                (wgsl-occurrence-text value))
          statements)
        (let ((name (declare-wgsl-name
                     context
                     (wgsl-identifier (shader:shader-object-name binding)))))
          (setf (gethash binding (wgsl-context-references context)) name)
          (append statements
                  (list (make-instance
                         'wgsl-variable-statement
                         :type (wgsl-type-name
                                type
                                (shader:shader-expression-source-form
                                 expression))
                         :name name :value value)))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-function-call))
  (multiple-value-bind (cached-result cached-p)
      (gethash expression (wgsl-context-function-call-results context))
    (if cached-p
        (note-wgsl-occurrence context expression cached-result)
        (let ((outer-statements (drain-wgsl-pending-statements context))
              (local-statements nil)
              (local-bindings nil)
              (references (wgsl-context-references context)))
          (unwind-protect
               (progn
                 (dolist (binding
                          (shader:shader-function-call-bindings expression))
                   (unless (or (typep binding
                                      'shader:shader-function-parameter-binding)
                               (nth-value 1 (gethash binding references)))
                     (push binding local-bindings)
                     (setf local-statements
                           (nconc local-statements
                                  (lower-wgsl-local-binding context binding)))))
                 (let ((result-text
                         (wgsl-text
                          context
                          (shader:shader-function-call-result expression))))
                   (setf local-statements
                         (nconc local-statements
                                (drain-wgsl-pending-statements context))
                         (wgsl-context-pending-statements context)
                         (nconc outer-statements local-statements)
                         (gethash expression
                                  (wgsl-context-function-call-results context))
                         result-text)
                   (note-wgsl-occurrence context expression result-text)))
            (dolist (binding local-bindings)
              (remhash binding references)))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-conditional))
  ;; WGSL has no conditional expression.  SELECT is valid here because the
  ;; language's conditional chooses between scalars or vectors, never
  ;; structures or matrices, and its arms are pure.
  (let ((condition
          (wgsl-text context
                     (lang:arithmetic-conditional-condition expression)))
        (consequent
          (wgsl-text context
                     (lang:arithmetic-conditional-consequent expression)))
        (alternative
          (wgsl-text context
                     (lang:arithmetic-conditional-alternative expression))))
    (note-wgsl-occurrence
     context expression
     (format nil "select(~A, ~A, ~A)" alternative consequent condition))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-counted-fold))
  (let* ((ordinal (incf (wgsl-context-fold-counter context)))
         (state-name (declare-wgsl-name
                      context (format nil "fold_state_~D" ordinal)))
         (index-name (declare-wgsl-name
                      context (format nil "fold_index_~D" ordinal)))
         (references (wgsl-context-references context))
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
        (gethash index-binding references)
      (multiple-value-bind (old-state old-state-p)
          (gethash state-binding references)
        (setf (gethash index-binding references) index-name
              (gethash state-binding references) state-name)
        (let* ((preheader-statements (drain-wgsl-pending-statements context))
               (until-expression
                 (lang:arithmetic-counted-fold-until expression))
               (until
                 (and until-expression
                      (lower-wgsl-expression context until-expression)))
               (until-statements
                 (and until (drain-wgsl-pending-statements context)))
               (local-statements
                 (loop for binding
                         in (lang:arithmetic-counted-fold-bindings expression)
                       nconc (lower-wgsl-local-binding context binding)))
               (update
                 (lower-wgsl-expression
                  context (lang:arithmetic-counted-fold-update expression))))
          (setf local-statements
                (nconc local-statements
                       (drain-wgsl-pending-statements context)))
          (setf (wgsl-context-pending-statements context)
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
          (dolist (binding (lang:arithmetic-counted-fold-bindings expression))
            (remhash binding references))
          (if old-index-p
              (setf (gethash index-binding references) old-index)
              (remhash index-binding references))
          (if old-state-p
              (setf (gethash state-binding references) old-state)
              (remhash state-binding references)))))
    (note-wgsl-occurrence context expression state-name)))

(defun wgsl-projective-clip-components (context application)
  (let ((homogeneous
          (format nil "vec4<f32>(~A, 1.0f)"
                  (wgsl-text
                   context
                   (shader:shader-map-application-point application)))))
    (mapcar (lambda (row)
              (format nil "dot(~A, ~A)" (wgsl-text context row) homogeneous))
            (shader:shader-map-application-rows application))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-map-application))
  (let ((definition (shader:shader-map-application-definition expression)))
    (unless (typep definition 'shader:shader-projective-map-definition)
      (wgsl-failure (shader:shader-expression-source-form expression)
                    :unsupported-wgsl-shader-map
                    (class-name (class-of definition))))
    (note-wgsl-occurrence
     context expression
     (format nil "vec4<f32>(~{~A~^, ~})"
             (wgsl-projective-clip-components context expression)))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-map-projection))
  (let* ((application (shader:shader-map-projection-application expression))
         (definition (shader:shader-map-application-definition application))
         (clip (wgsl-projective-clip-components context application)))
    (flet ((triple (values)
             (format nil "vec3<f32>(~{~A~^, ~})"
                     (mapcar #'wgsl-float-literal values))))
      (note-wgsl-occurrence
       context expression
       (format nil "(((vec3<f32>(~{~A~^, ~}) / ~A) * ~A) + ~A)"
               (subseq clip 0 3) (fourth clip)
               (triple (shader:shader-projective-map-coordinate-scale
                        definition))
               (triple (shader:shader-projective-map-coordinate-offset
                        definition)))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context)
     (expression shader:shader-quantity-boundary))
  ;; Interpretation, construction, assumption, and representation are
  ;; semantic boundaries without runtime work.
  (note-wgsl-occurrence
   context expression
   (wgsl-text context (shader:shader-quantity-boundary-operand expression))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-unit-conversion))
  (let ((operand
          (wgsl-text context
                     (shader:shader-unit-conversion-operand expression)))
        (factor (shader:shader-unit-conversion-factor expression)))
    (note-wgsl-occurrence
     context expression
     (if (= factor 1)
         operand
         (format nil "(~A * ~A)" operand (wgsl-float-literal factor))))))

(defmethod lower-wgsl-expression
    ((context wgsl-lowering-context) (expression shader:shader-expression))
  (declare (ignore context))
  (wgsl-failure (shader:shader-expression-source-form expression)
                :unsupported-wgsl-expression
                (class-name (class-of expression))))

;;; Operators.

(defun lower-wgsl-operands (context expression)
  "The texts of EXPRESSION's operands, lowered in order."
  (mapcar (lambda (operand) (wgsl-text context operand))
          (shader:shader-call-operands expression)))

(defun lower-wgsl-infix-call (context expression operator)
  (let ((operands (lower-wgsl-operands context expression)))
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
  (note-wgsl-occurrence
   context expression
   (format nil "~A(~{~A~^, ~})" name (lower-wgsl-operands context expression))))

(defmethod shader:lower-shader-call
    (operator (context wgsl-lowering-context) expression)
  (wgsl-failure (shader:shader-expression-source-form expression)
                :unsupported-wgsl-operator operator))

(defmacro define-wgsl-operator (operator (context expression) &body body)
  `(defmethod shader:lower-shader-call
       ((operator (eql ',operator))
        (,context wgsl-lowering-context)
        (,expression shader:shader-call))
     (declare (ignore operator))
     ,@body))

(macrolet ((infix (&rest pairs)
             `(progn
                ,@(loop for (operator text) on pairs by #'cddr
                        collect `(define-wgsl-operator ,operator
                                     (context expression)
                                   (lower-wgsl-infix-call
                                    context expression ,text)))))
           (functions (&rest pairs)
             `(progn
                ,@(loop for (operator name) on pairs by #'cddr
                        collect `(define-wgsl-operator ,operator
                                     (context expression)
                                   (lower-wgsl-function-call
                                    context expression ,name))))))
  ;; WGSL's matrices are column-major with the language's products, so *
  ;; is * for them too.  REM is the truncated %; MOD is % only for unsigned
  ;; values (below).  #QEHEEE
  (infix + "+" - "-" * "*" / "/" rem "%"
         < "<" <= "<=" > ">" >= ">=" = "==" /= "!="
         logand "&" logior "|" logxor "^")
  (functions shader:dot "dot"
             shader:mix "mix"
             abs "abs"
             shader:any "any"
             shader:all "all"
             signum "sign"
             sqrt "sqrt"
             expt "pow"
             shader:clamp "clamp"
             shader:smoothstep "smoothstep"
             shader:step "step"
             shader:normalize "normalize"
             floor "floor"
             shader:fract "fract"
             sin "sin"
             cos "cos"
             exp "exp"
             log "log"
             shader:transpose "transpose"
             shader:uint "u32"
             shader:int "i32"
             float "f32"))

(define-wgsl-operator mod (context expression)
  ;; Unsigned MOD is %; signed MOD is floored, its sign following the
  ;; divisor, as CL:MOD.
  (if (eq :int (shader:shader-type-scalar-kind
                (shader:shader-expression-type expression)))
      (destructuring-bind (left right) (lower-wgsl-operands context expression)
        (note-wgsl-occurrence
         context expression
         (format nil "(((~A % ~A) + ~A) % ~A)" left right right right)))
      (lower-wgsl-infix-call context expression "%")))

;;; WGSL's && and || are scalar; & and | combine boolean vectors.
(macrolet ((logical (operator scalar vector)
             `(define-wgsl-operator ,operator (context expression)
                (lower-wgsl-infix-call
                 context expression
                 (if (shader:shader-vector-type-p
                      (shader:shader-expression-type expression))
                     ,vector
                     ,scalar)))))
  (logical and "&&" "&")
  (logical or "||" "|"))

(macrolet ((prefix (operator text)
             `(define-wgsl-operator ,operator (context expression)
                (note-wgsl-occurrence
                 context expression
                 (format nil "(~A~A)" ,text
                         (first (lower-wgsl-operands context expression)))))))
  (prefix not "!")
  (prefix lognot "~"))

(define-wgsl-operator shader:select (context expression)
  ;; WGSL's select(f, t, c) is c ? t : f, the reverse of the source order.
  (destructuring-bind (condition consequent alternative)
      (lower-wgsl-operands context expression)
    (note-wgsl-occurrence
     context expression
     (format nil "select(~A, ~A, ~A)" alternative consequent condition))))

(macrolet ((chained (operator name)
             `(define-wgsl-operator ,operator (context expression)
                (let ((operands (lower-wgsl-operands context expression)))
                  (note-wgsl-occurrence
                   context expression
                   (reduce (lambda (left right)
                             (format nil "~A(~A, ~A)" ,name left right))
                           (rest operands)
                           :initial-value (first operands)))))))
  (chained min "min")
  (chained max "max"))

(defun wgsl-shift-count (value-type count-type count)
  "WGSL shifts by u32 counts, one per value component."
  (let ((width (shader:shader-type-component-count value-type)))
    (cond ((and (= width 1) (eq :uint (shader:shader-type-scalar-kind
                                       count-type)))
           count)
          ((= width 1) (format nil "u32(~A)" count))
          ((and (eq :uint (shader:shader-type-scalar-kind count-type))
                (= width (shader:shader-type-component-count count-type)))
           count)
          ((= 1 (shader:shader-type-component-count count-type))
           (format nil "vec~D<u32>(~A)" width
                   (if (eq :uint (shader:shader-type-scalar-kind count-type))
                       count
                       (format nil "u32(~A)" count))))
          (t (format nil "vec~D<u32>(~A)" width count)))))

(define-wgsl-operator ash (context expression)
  (let ((count (first (shader:shader-call-parameters expression)))
        (type (shader:shader-expression-type expression))
        (value (first (lower-wgsl-operands context expression))))
    (note-wgsl-occurrence
     context expression
     (if (zerop count)
         (format nil "(~A)" value)
         (format nil "(~A ~A ~A)" value (if (plusp count) "<<" ">>")
                 (wgsl-shift-count type (shader:find-shader-type :uint)
                                   (format nil "~Du" (abs count))))))))

;;; >> is arithmetic for signed values and logical for unsigned ones.
(macrolet ((shift (operator text)
             `(define-wgsl-operator ,operator (context expression)
                (destructuring-bind (value count)
                    (lower-wgsl-operands context expression)
                  (note-wgsl-occurrence
                   context expression
                   (format nil "(~A ~A ~A)" value ,text
                           (wgsl-shift-count
                            (shader:shader-expression-type expression)
                            (shader:shader-expression-type
                             (second (shader:shader-call-operands expression)))
                            count)))))))
  (shift shader:shift-left "<<")
  (shift shader:shift-right ">>"))

(define-wgsl-operator shader:bit-cast (context expression)
  (note-wgsl-occurrence
   context expression
   (format nil "bitcast<~A>(~A)"
           (wgsl-type-name (shader:shader-expression-type expression))
           (first (lower-wgsl-operands context expression)))))

(define-wgsl-operator shader:uint64 (context expression)
  (declare (ignore context))
  (wgsl-failure (shader:shader-expression-source-form expression)
                :unsupported-wgsl-64-bit-integer 'shader:uint64))

(macrolet ((derivative (operator name)
             `(define-wgsl-operator ,operator (context expression)
                ;; Only fragments have neighbours to difference against.
                (unless (eq :fragment (wgsl-context-stage context))
                  (wgsl-failure (shader:shader-expression-source-form
                                 expression)
                                :unsupported-wgsl-derivative
                                (wgsl-context-stage context)))
                (lower-wgsl-function-call context expression ,name))))
  (derivative shader:derivative-x "dpdx")
  (derivative shader:derivative-y "dpdy"))

;;; A constructor assembles constituents or converts one vector of another
;;; scalar kind; WGSL spells both as the type applied to its arguments.
(macrolet ((constructors (&rest operators)
             `(progn
                ,@(loop for operator in operators
                        collect
                        `(define-wgsl-operator ,operator (context expression)
                           (lower-wgsl-function-call
                            context expression
                            (wgsl-type-name
                             (shader:shader-expression-type expression)
                             (shader:shader-expression-source-form
                              expression))))))))
  (constructors shader:vec2 shader:vec3 shader:vec4
                shader:uvec2 shader:uvec3 shader:uvec4
                shader:ivec2 shader:ivec3 shader:ivec4
                shader:bvec2 shader:bvec3 shader:bvec4
                shader:mat2 shader:mat3 shader:mat4))

(define-wgsl-operator shader:column (context expression)
  (note-wgsl-occurrence
   context expression
   (format nil "~A[~D]" (first (lower-wgsl-operands context expression))
           (first (shader:shader-call-parameters expression)))))

(defun wgsl-swizzle-components (designator)
  "DESIGNATOR's component letters.  WGSL takes xyzw or rgba, never both in
one swizzle, so a mixed designator is respelled in xyzw."
  (let ((letters (string-downcase (string designator))))
    (if (or (every (lambda (letter) (find letter "xyzw")) letters)
            (every (lambda (letter) (find letter "rgba")) letters))
        letters
        (map 'string
             (lambda (letter)
               (char "xyzw" (or (position letter "xyzw")
                                (position letter "rgba"))))
             letters))))

(define-wgsl-operator shader:swizzle (context expression)
  (let* ((operand (first (shader:shader-call-operands expression)))
         (text (first (lower-wgsl-operands context expression)))
         (components (wgsl-swizzle-components
                      (first (shader:shader-call-parameters expression)))))
    (note-wgsl-occurrence
     context expression
     (cond
       ;; A WGSL scalar has no components: its swizzle is itself, or itself
       ;; in every component of a vector.
       ((shader:shader-vector-type-p (shader:shader-expression-type operand))
        (format nil "~A.~A" text components))
       ((= 1 (length components)) text)
       (t (format nil "~A(~A)"
                  (wgsl-type-name (shader:shader-expression-type expression))
                  text))))))

(defmethod shader:lower-shader-call
    ((operator (eql 'shader:ldb))
     (context wgsl-lowering-context)
     (expression shader:shader-bit-field-call))
  "Lower LDB to a logical shift and mask of a 32-bit operand."
  (declare (ignore operator))
  (unless (= 32 (shader:shader-type-bit-width
                 (shader:shader-expression-type expression)))
    (wgsl-failure (shader:shader-expression-source-form expression)
                  :unsupported-wgsl-64-bit-integer 'shader:ldb))
  (let* ((operands (lower-wgsl-operands context expression))
         (value (first operands))
         (size (shader:shader-bit-field-size expression))
         (position (shader:shader-bit-field-position expression))
         (shift (if position (format nil "~Du" position) (second operands))))
    (note-wgsl-occurrence
     context expression
     (cond ((and position (zerop position) (= size 32))
            (format nil "(~A)" value))
           ((and position (= (+ size position) 32))
            (format nil "(~A >> ~A)" value shift))
           (t
            (format nil "((~A >> ~A) & 0x~Xu)"
                    value shift (1- (ash 1 size))))))))

;;; Interface structures.

(defun wgsl-flat-p (declaration)
  "Integers never interpolate, and WebGPU requires them to say so on both
sides of the rasterizer."
  (or (eq :flat (shader:shader-interface-interpolation declaration))
      (member (shader:shader-type-scalar-kind
               (shader:shader-declaration-type declaration))
              '(:uint :int))))

(defun wgsl-built-in-name (stage declaration)
  "The @builtin value of DECLARATION in STAGE.  A fragment's position is the
language's :FRAG-COORD as it stands: pixel centres, depth in z, and the
reciprocal of clip w in w."
  (let ((direction (shader:shader-interface-direction declaration))
        (built-in (shader:shader-interface-built-in declaration)))
    (or (case built-in
          (:position
           (and (or (and (eq stage :vertex) (eq direction :output))
                    (and (eq stage :fragment) (eq direction :input)))
                "position"))
          (:frag-coord
           (and (eq stage :fragment) (eq direction :input) "position"))
          (:vertex-index
           (and (eq stage :vertex) (eq direction :input) "vertex_index"))
          (:instance-index
           (and (eq stage :vertex) (eq direction :input) "instance_index"))
          (:front-facing
           (and (eq stage :fragment) (eq direction :input) "front_facing"))
          (:sample-index
           (and (eq stage :fragment) (eq direction :input) "sample_index"))
          (:frag-depth
           (and (eq stage :fragment) (eq direction :output) "frag_depth"))
          (:global-invocation-id
           (and (eq stage :compute) "global_invocation_id"))
          (:local-invocation-id
           (and (eq stage :compute) "local_invocation_id"))
          (:local-invocation-index
           (and (eq stage :compute) "local_invocation_index"))
          (:workgroup-id (and (eq stage :compute) "workgroup_id"))
          (:num-workgroups (and (eq stage :compute) "num_workgroups"))
          ((:wave-lane-index :wave-lane-count)
           (wgsl-failure (shader:shader-object-source-form declaration)
                         :unsupported-wgsl-wave-operation built-in)))
        (wgsl-failure (shader:shader-object-source-form declaration)
                      :unsupported-wgsl-built-in
                      (list stage direction built-in)))))

(defun wgsl-interface-attribute (stage declaration)
  (let ((location (shader:shader-interface-location declaration))
        (direction (shader:shader-interface-direction declaration)))
    (cond ((shader:shader-interface-built-in declaration)
           (format nil "@builtin(~A)" (wgsl-built-in-name stage declaration)))
          (location
           (format nil "@location(~D)~:[~; @interpolate(flat)~]"
                   location
                   (and (wgsl-flat-p declaration)
                        (or (and (eq stage :vertex) (eq direction :output))
                            (and (eq stage :fragment)
                                 (eq direction :input))))))
          (t
           (wgsl-failure (shader:shader-object-source-form declaration)
                         :undecorated-wgsl-interface)))))

(defun wgsl-structure-member-p (declaration)
  "Whether DECLARATION is a member of the entry point's input structure.
WGSL has no built-in value for the workgroup size: it is the
@workgroup_size constant itself."
  (not (eq :workgroup-size (shader:shader-interface-built-in declaration))))

(defun write-wgsl-interface-structure (stream context name declarations)
  (let ((stage (wgsl-context-stage context)))
    (format stream "struct ~A {~%" name)
    (dolist (declaration declarations)
      (format stream "  ~A ~A: ~A,~%"
              (wgsl-interface-attribute stage declaration)
              (wgsl-identifier (shader:shader-object-name declaration))
              (wgsl-type-name
               (shader:shader-declaration-type declaration)
               (shader:shader-object-source-form declaration))))
    (format stream "}~%~%")))

;;; Structures and resources.

(defun write-wgsl-struct-declarations (stream specification)
  "Each structure SPECIFICATION uses, contained ones first."
  (dolist (struct (shader:shader-specification-struct-types specification))
    (format stream "struct ~A {~%" (wgsl-type-name struct))
    (dolist (field (shader:shader-struct-type-fields struct))
      (format stream "  ~A: ~A,~%"
              (wgsl-identifier (shader:shader-object-name field))
              (wgsl-type-name (shader:shader-struct-field-type field))))
    (format stream "}~%~%")))

(defun wgsl-resource-location (context resource)
  "RESOURCE's group and binding."
  (if (wgsl-context-resource-binding context)
      (funcall (wgsl-context-resource-binding context) resource)
      (values (shader:shader-resource-descriptor-set resource)
              (shader:shader-resource-binding resource))))

(defun check-wgsl-binding-collisions (context specification)
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (resource (shader:shader-specification-resources specification))
      (let* ((location (multiple-value-list
                        (wgsl-resource-location context resource)))
             (other (gethash location seen)))
        (when other
          (wgsl-failure (shader:shader-object-source-form resource)
                        :wgsl-binding-collision
                        (list :group (first location)
                              :binding (second location)
                              (shader:shader-object-name other)
                              (shader:shader-object-name resource))))
        (setf (gethash location seen) resource)))))

(defun wgsl-atomic-target-p (context target)
  (member target (wgsl-context-atomic-targets context)))

(defun wgsl-resource-variable (context resource)
  "RESOURCE's address space and access in angle brackets (or NIL for a
handle), and its type."
  (let ((type (shader:shader-declaration-type resource))
        (form (shader:shader-object-source-form resource)))
    (ecase (shader:shader-type-opaque-kind type)
      (:uniform-block
       (values "<uniform>"
               (wgsl-structure-name (shader:shader-object-name resource) "")))
      (:storage-buffer
       ;; WebGPU gives a vertex stage no storage it could write.
       (when (and (shader:shader-storage-buffer-writable-p resource)
                  (eq :vertex (wgsl-context-stage context)))
         (wgsl-failure form :unsupported-wgsl-vertex-stage-buffer-access
                       (list (shader:shader-object-name resource)
                             :read-write)))
       (values (if (shader:shader-storage-buffer-writable-p resource)
                   "<storage, read_write>"
                   "<storage, read>")
               (format nil "array<~A>"
                       (if (wgsl-atomic-target-p context resource)
                           "atomic<u32>"
                           (wgsl-type-name
                            (shader:shader-storage-buffer-element-type
                             resource)
                            form)))))
      (:texture (values nil (wgsl-texture-type-name type form)))
      (:storage-texture
       (values nil (wgsl-storage-texture-type-name context resource)))
      (:sampler
       (values nil (if (member (shader:shader-resource-key resource)
                               (wgsl-context-comparison-samplers context))
                       "sampler_comparison"
                       "sampler"))))))

(defun write-wgsl-resource (stream context resource)
  (check-wgsl-resource-layout resource)
  (when (typep resource 'shader:shader-uniform-block)
    (format stream "struct ~A {~%"
            (wgsl-structure-name (shader:shader-object-name resource) ""))
    (dolist (member (shader:shader-uniform-block-members resource))
      (format stream "  ~A: ~A,~%"
              (wgsl-identifier (shader:shader-object-name member))
              (wgsl-type-name
               (shader:shader-declaration-type member)
               (shader:shader-object-source-form member))))
    (format stream "}~%"))
  (multiple-value-bind (group binding) (wgsl-resource-location context resource)
    (multiple-value-bind (space type) (wgsl-resource-variable context resource)
      (format stream "@group(~D) @binding(~D) var~@[~A~] ~A: ~A;~%~%"
              group binding space
              (gethash resource (wgsl-context-references context))
              type))))

(defun register-wgsl-references (context specification)
  "Name every input, resource, and workgroup array for the entry function."
  (let ((references (wgsl-context-references context)))
    (dolist (name (wgsl-target-overrides (wgsl-context-target context)))
      (setf (gethash (wgsl-override-identifier-for name)
                     (wgsl-context-declared-names context))
            t))
    (dolist (input (shader:shader-specification-inputs specification))
      (setf (gethash input references)
            (if (wgsl-structure-member-p input)
                (format nil "stage_in.~A"
                        (wgsl-identifier (shader:shader-object-name input)))
                (format nil "vec3<u32>(~{~Du~^, ~})"
                        (shader:shader-specification-workgroup-size
                         specification)))))
    (dolist (declaration
             (append (shader:shader-specification-resources specification)
                     (shader:shader-specification-shared-arrays
                      specification)))
      (let ((name (declare-wgsl-module-name
                   context
                   (wgsl-identifier (shader:shader-object-name declaration))
                   (shader:shader-object-source-form declaration))))
        (setf (gethash declaration references) name)
        (when (typep declaration 'shader:shader-uniform-block)
          (dolist (member (shader:shader-uniform-block-members declaration))
            (setf (gethash member references)
                  (format nil "~A.~A" name
                          (wgsl-identifier
                           (shader:shader-object-name member))))))))
    context))

(defun check-wgsl-structure-names (specification input-name output-name)
  "A structure the author defined must not share a generated one's name."
  (let ((seen nil))
    (dolist (name
             (append
              (mapcar #'wgsl-type-name
                      (shader:shader-specification-struct-types specification))
              (loop for resource
                      in (shader:shader-specification-resources specification)
                    when (typep resource 'shader:shader-uniform-block)
                      collect (wgsl-structure-name
                               (shader:shader-object-name resource) ""))
              (remove nil (list input-name output-name))))
      (when (member name seen :test #'string=)
        (wgsl-failure (shader:shader-object-source-form specification)
                      :wgsl-structure-name-collision name))
      (push name seen))))

;;; Statements.

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
    ((context wgsl-lowering-context) (statement shader:shader-block-statement))
  (nconc
   (loop for binding in (shader:shader-block-statement-bindings statement)
         nconc (lower-wgsl-local-binding context binding))
   (mapcan (lambda (child) (lower-wgsl-statement context child))
           (shader:shader-block-statement-statements statement))))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context) (statement shader:shader-discard))
  (declare (ignore context))
  (list (wgsl-line "discard;")))

(defmethod lower-wgsl-statement
    ((context wgsl-lowering-context) (statement shader:shader-statement))
  (declare (ignore context))
  (wgsl-failure (shader:shader-statement-source-form statement)
                :unsupported-wgsl-statement
                (class-name (class-of statement))))

;;; Rendering.

(defvar *wgsl-statement-indentation* 1)

(defun write-wgsl-indent (stream &optional (extra 0))
  (loop repeat (+ *wgsl-statement-indentation* extra)
        do (write-string "  " stream)))

(defgeneric write-wgsl-statement-form (statement stream))

(defmethod write-wgsl-statement-form ((statement wgsl-line-statement) stream)
  (write-wgsl-indent stream)
  (format stream "~A~%" (wgsl-line-statement-text statement)))

(defmethod write-wgsl-statement-form
    ((statement wgsl-variable-statement) stream)
  (write-wgsl-indent stream)
  (format stream "let ~A: ~A = ~A;~%"
          (wgsl-variable-statement-name statement)
          (wgsl-variable-statement-type statement)
          (wgsl-occurrence-text (wgsl-variable-statement-value statement))))

(defmethod write-wgsl-statement-form
    ((statement wgsl-output-statement) stream)
  (let ((declaration (wgsl-output-statement-declaration statement))
        (value (wgsl-occurrence-text
                (wgsl-output-statement-value statement))))
    (write-wgsl-indent stream)
    (format stream "result.~A = ~A;~%"
            (wgsl-output-statement-field statement)
            (if (eq :position (shader:shader-interface-built-in declaration))
                ;; The shared camera graph keeps Vulkan's framebuffer-oriented
                ;; clip Y.  WebGPU, like Metal and Direct3D, points clip Y
                ;; up; negate Y as their lowerings do.
                (format nil "(~A * vec4<f32>(1.0f, -1.0f, 1.0f, 1.0f))" value)
                value))))

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
  (let* ((index (wgsl-counted-fold-statement-index-name statement))
         (index-type (wgsl-counted-fold-statement-index-type statement))
         (unsigned-p (string= index-type "u32")))
    (write-wgsl-indent stream)
    (format stream "var ~A: ~A = ~A;~%"
            (wgsl-counted-fold-statement-state-name statement)
            (wgsl-counted-fold-statement-type statement)
            (wgsl-occurrence-text
             (wgsl-counted-fold-statement-initial statement)))
    (write-wgsl-indent stream)
    (format stream "for (var ~A: ~A = ~A; ~A < ~A; ~A = ~A + ~A) {~%"
            index index-type (if unsigned-p "0u" "0.0f")
            index
            (wgsl-occurrence-text (wgsl-counted-fold-statement-count statement))
            index index (if unsigned-p "1u" "1.0f")))
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

(defun wgsl-module-requirements (context)
  "The language extensions the module must name in a requires directive.
Reading a storage texture, alone or beside writing it, is one."
  (when (some (lambda (resource)
                (and (shader:shader-storage-texture-type-p
                      (shader:shader-declaration-type resource))
                     (not (eq :write (wgsl-module-texture-access
                                      context resource)))))
              (shader:shader-specification-resources
               (wgsl-context-specification context)))
    '("readonly_and_readwrite_storage_textures")))

(defun render-wgsl-document (context entry-point-name statements)
  (let* ((specification (wgsl-context-specification context))
         (stage (shader:shader-specification-stage specification))
         (base-name (shader:shader-object-name specification))
         (inputs (remove-if-not
                  #'wgsl-structure-member-p
                  (shader:shader-specification-inputs specification)))
         (outputs (shader:shader-specification-outputs specification))
         (input-name (and inputs (wgsl-structure-name base-name "Input")))
         (output-name (and outputs (wgsl-structure-name base-name "Output"))))
    (check-wgsl-structure-names specification input-name output-name)
    (with-output-to-string (stream)
      (format stream "// WGSL for WebGPU, ~(~A~) stage, entry point ~A.~%"
              stage entry-point-name)
      ;; This language's lowered code samples and differentiates wherever
      ;; its source does, as every other target allows.
      (when (eq stage :fragment)
        (format stream "diagnostic(off, derivative_uniformity);~%"))
      (dolist (requirement (wgsl-module-requirements context))
        (format stream "requires ~A;~%" requirement))
      (terpri stream)
      (dolist (override (encountered-wgsl-overrides context))
        (format stream "override ~A: ~A = ~A;~%"
                (wgsl-override-identifier override)
                (wgsl-override-type override)
                (wgsl-float-literal (wgsl-override-default override))))
      (when (encountered-wgsl-overrides context) (terpri stream))
      (write-wgsl-struct-declarations stream specification)
      (dolist (resource (shader:shader-specification-resources specification))
        (write-wgsl-resource stream context resource))
      (write-wgsl-workgroup-variables stream context)
      (when input-name
        (write-wgsl-interface-structure stream context input-name inputs))
      (when output-name
        (write-wgsl-interface-structure stream context output-name outputs))
      (format stream "@~(~A~)~@[ @workgroup_size(~{~D~^, ~})~]~%"
              stage
              (and (eq stage :compute)
                   (shader:shader-specification-workgroup-size specification)))
      (format stream "fn ~A(~@[stage_in: ~A~])~@[ -> ~A~] {~%"
              entry-point-name input-name output-name)
      (when output-name
        (format stream "  var result: ~A;~%" output-name))
      (let ((*wgsl-statement-indentation* 1))
        (dolist (statement statements)
          (write-wgsl-statement-form statement stream)))
      (when output-name
        (format stream "  return result;~%"))
      (format stream "}~%"))))

;;; Specifications.

(defun lower-wgsl-specification
    (target specification
     &key entry-point-name resource-binding storage-texture-access
          (comparison-samplers nil comparison-samplers-p))
  (let ((stage (shader:shader-specification-stage specification)))
    (unless (member stage '(:vertex :fragment :compute))
      (wgsl-failure (shader:shader-object-source-form specification)
                    :unsupported-wgsl-stage stage)))
  (let* ((context
           (make-instance
            'wgsl-lowering-context
            :target target :specification specification
            :resource-binding resource-binding
            :storage-texture-access storage-texture-access
            :comparison-samplers
            (if comparison-samplers-p
                comparison-samplers
                (shader:shader-comparison-samplers specification))
            :atomic-targets
            (shader:shader-specification-atomic-targets specification)))
         (entry-point-name
           (or entry-point-name
               (wgsl-identifier (shader:shader-object-name specification)))))
    (check-wgsl-binding-collisions context specification)
    (register-wgsl-references context specification)
    (declare-wgsl-module-name
     context entry-point-name (shader:shader-object-source-form specification))
    (let ((statements
            (nconc
             (loop for binding
                     in (shader:shader-specification-bindings specification)
                   nconc (lower-wgsl-local-binding context binding))
             (mapcan (lambda (statement)
                       (lower-wgsl-statement context statement))
                     (shader:shader-specification-statements
                      specification)))))
      (maphash (lambda (expression occurrences)
                 (setf (gethash expression
                                (wgsl-context-expression-occurrences context))
                       (nreverse occurrences)))
               (wgsl-context-expression-occurrences context))
      (make-instance
       'wgsl-document
       :target target :specification specification
       :entry-point-name entry-point-name
       :source (render-wgsl-document context entry-point-name statements)
       :overrides (encountered-wgsl-overrides context)
       :expression-occurrences (wgsl-context-expression-occurrences context)
       :occurrence-expression (wgsl-context-occurrence-expression context)))))

(defmethod shader:lower-shader-specification
    ((target wgsl-target) (specification shader:shader-specification))
  "Lower the shared shader graph directly to deterministic WGSL."
  (lower-wgsl-specification target specification))

(defun compile-wgsl
    (specification &key overrides entry-point-name resource-binding
                        storage-texture-access
                        (comparison-samplers nil comparison-samplers-p))
  "Lower SPECIFICATION to a WGSL document.

OVERRIDES lists the source values kept as pipeline overrides.
ENTRY-POINT-NAME names the entry function (default: after the
specification).  RESOURCE-BINDING is a function of each resource declaration
returning its group and binding (default: its declared set and binding).
COMPARISON-SAMPLERS lists the keys of samplers declared sampler_comparison;
by default, those SPECIFICATION itself compares with, though a program whose
other stage compares through a sampler must say so for every stage.
STORAGE-TEXTURE-ACCESS is a function of each storage texture declaration
returning :READ, :WRITE, or :READ-WRITE; by default, what SPECIFICATION
itself does with the texture (see WGSL-STORAGE-TEXTURE-ACCESS)."
  (apply #'lower-wgsl-specification
         (make-instance 'wgsl-target :overrides overrides) specification
         :entry-point-name entry-point-name
         :resource-binding resource-binding
         :storage-texture-access storage-texture-access
         (and comparison-samplers-p
              (list :comparison-samplers comparison-samplers))))

(defun write-wgsl (document pathname)
  "Write DOCUMENT's deterministic source to PATHNAME and return PATHNAME."
  (check-type document wgsl-document)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (wgsl-document-source document) stream))
  pathname)
