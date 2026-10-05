;;; luv-shaderc: shader programs compiled ahead of time for a native renderer.
;;;
;;; A source file holds ordinary DEFINE-SHADER, DEFINE-SHADER-FUNCTION, and
;;; DEFINE-SHADER-PROGRAM forms.  Each program it defines is linked once
;;; (SHADER:LINK-SHADER-PROGRAM), checked against the binding contract the
;;; renderer's hardware layer expects, lowered to one MSL and one HLSL
;;; document per stage, and described twice for the host: as a JSON manifest
;;; and as a C++ header of uniform structures and a resource table.  #1I6G0R

(in-package #:luv.shaderc)

(defparameter *targets* '(:msl :hlsl)
  "The shading languages written when a caller names none.")

(define-condition shaderc-error (error)
  ((message :initarg :message :reader shaderc-error-message))
  (:report (lambda (condition stream)
             (write-string (shaderc-error-message condition) stream))))

(defun shaderc-fail (control &rest arguments)
  (error 'shaderc-error :message (apply #'format nil control arguments)))

(defvar *compilation-context* nil
  "What is being compiled, innermost first, for error reports.")

(defmacro with-compilation-context ((control &rest arguments) &body body)
  `(let ((*compilation-context*
           (cons (format nil ,control ,@arguments) *compilation-context*)))
     ,@body))

;;; Names.  Shader names are Lisp symbols; generated C++ needs identifiers.

(defparameter *cpp-reserved-words*
  '("alignas" "alignof" "and" "and_eq" "asm" "auto" "bitand" "bitor" "bool"
    "break" "case" "catch" "char" "char8_t" "char16_t" "char32_t" "class"
    "compl" "concept" "const" "consteval" "constexpr" "constinit"
    "const_cast" "continue" "co_await" "co_return" "co_yield" "decltype"
    "default" "delete" "do" "double" "dynamic_cast" "else" "enum" "explicit"
    "export" "extern" "false" "float" "for" "friend" "goto" "if" "inline"
    "int" "long" "mutable" "namespace" "new" "noexcept" "not" "not_eq"
    "nullptr" "operator" "or" "or_eq" "private" "protected" "public"
    "register" "reinterpret_cast" "requires" "return" "short" "signed"
    "sizeof" "static" "static_assert" "static_cast" "struct" "switch"
    "template" "this" "thread_local" "throw" "true" "try" "typedef"
    "typeid" "typename" "union" "unsigned" "using" "virtual" "void"
    "volatile" "wchar_t" "while" "xor" "xor_eq"))

(defparameter *nhal-type-names*
  '("Program" "Resource" "ResourceKind" "StageMask")
  "Names the generated header sees from moppe::nhal, which a uniform
structure must not shadow.")

(defun snake-identifier (name)
  "NAME (a symbol or string) as a snake_case C++ identifier."
  (let ((identifier
          (with-output-to-string (stream)
            (loop for character across (string-downcase (string name))
                  for firstp = t then nil
                  for emitted = (if (alphanumericp character) character #\_)
                  do (when (and firstp (digit-char-p emitted))
                       (write-char #\_ stream))
                     (write-char emitted stream)))))
    (if (member identifier *cpp-reserved-words* :test #'string=)
        (concatenate 'string identifier "_")
        identifier)))

(defun camel-identifier (name)
  "NAME as a CamelCase C++ type name."
  (let* ((capitalize-next-p t)
         (identifier
           (with-output-to-string (stream)
             (loop for character across (string-downcase (string name))
                   if (alphanumericp character)
                     do (write-char (if capitalize-next-p
                                        (char-upcase character)
                                        character)
                                    stream)
                        (setf capitalize-next-p nil)
                   else do (setf capitalize-next-p t)))))
    (cond ((zerop (length identifier)) "Block")
          ((digit-char-p (char identifier 0))
           (concatenate 'string "Block" identifier))
          ((member identifier *nhal-type-names* :test #'string=)
           (concatenate 'string identifier "Block"))
          (t identifier))))

(defun stage-name (stage)
  (string-downcase (symbol-name stage)))

(defun entry-point-name (program-name stage)
  "The stable entry function of PROGRAM-NAME's STAGE in every language."
  (format nil "~A_~A" program-name (stage-name stage)))

;;; Loading source files.

(defun load-note-position (note)
  "The \"line L, column C\" SBCL's LOAD notes for a failing form, or NIL."
  (let* ((start (search "line " note))
         (end (and start (position #\Newline note :start start))))
    (and start (string-right-trim '(#\Space) (subseq note start end)))))

(defun load-shader-source (pathname)
  "Load PATHNAME in LUV.SHADER-USER; return the programs it defines in order.
The file may change package with IN-PACKAGE; LOAD restores ours after it.
An error names the failing top-level form's position in its context."
  (let ((shader:*shader-programs* nil)
        (*package* (find-package '#:luv.shader-user))
        (*readtable* (copy-readtable nil))
        (notes (make-string-output-stream)))
    (handler-bind ((style-warning #'muffle-warning)
                   (error
                     (lambda (condition)
                       (declare (ignore condition))
                       (let ((position (load-note-position
                                        (get-output-stream-string notes))))
                         (when position
                           (push (format nil "the form at ~A" position)
                                 *compilation-context*))))))
      ;; SBCL's LOAD writes where a failing form starts to *ERROR-OUTPUT*;
      ;; keep that for the context instead of interleaving it.
      (let ((*error-output* notes))
        (load pathname)))
    shader:*shader-programs*))

;;; The binding contract (moppe's docs/nhal.md).

(defparameter *binding-limit* 16
  "Buffer and texture binding numbers run from 0 below this limit.")

(defparameter *standard-samplers*
  '((0 :sampler "linear filtering, clamp to edge")
    (1 :sampler "linear filtering, repeat")
    (2 :sampler "nearest, clamp to edge")
    (3 :comparison-sampler "linear comparison (less-equal), clamp to edge"))
  "The fixed sampler set a renderer provides, by binding number.")

(defun resource-description (resource)
  (format nil "~(~A~) ~A at binding ~D"
          (shader:shader-program-resource-kind resource)
          (snake-identifier (shader:shader-program-resource-name resource))
          (shader:shader-program-resource-binding resource)))

(defun check-binding-contract (linkage)
  (dolist (resource (shader:shader-program-linkage-resources linkage))
    (let ((binding (shader:shader-program-resource-binding resource))
          (kind (shader:shader-program-resource-kind resource)))
      (if (eq :sampler (shader:shader-program-resource-family resource))
          (let ((standard (assoc binding *standard-samplers*)))
            (unless standard
              (shaderc-fail "~A: samplers are the standard set ~{~D~^, ~}."
                            (resource-description resource)
                            (mapcar #'first *standard-samplers*)))
            (unless (eq kind (second standard))
              (shaderc-fail "~A: sampler ~D is ~A, so it ~:[cannot~;must~] ~
                             be used with SAMPLE-COMPARE."
                            (resource-description resource) binding
                            (third standard)
                            (eq :comparison-sampler (second standard)))))
          (unless (< binding *binding-limit*)
            (shaderc-fail "~A: ~(~A~) bindings run from 0 to ~D."
                          (resource-description resource)
                          (shader:shader-program-resource-family resource)
                          (1- *binding-limit*))))))
  (let ((vertex (shader:shader-program-linkage-specification linkage :vertex)))
    (when vertex
      (dolist (input (shader:shader-specification-inputs vertex))
        (unless (shader:shader-interface-built-in input)
          (shaderc-fail "Vertex input ~S has a location, but programs have ~
                         no vertex buffers: pull it from a storage buffer ~
                         with :VERTEX-INDEX or :INSTANCE-INDEX."
                        (shader:shader-object-source-form input)))))))

;;; Compilation.

(defclass compiled-stage ()
  ((stage :initarg :stage :reader compiled-stage-stage)
   (specification
    :initarg :specification :reader compiled-stage-specification)
   (entry-point :initarg :entry-point :reader compiled-stage-entry-point)
   (msl :initarg :msl :initform nil :reader compiled-stage-msl)
   (hlsl :initarg :hlsl :initform nil :reader compiled-stage-hlsl)))

(defclass compiled-program ()
  ((name :initarg :name :reader compiled-program-name
         :documentation "The snake_case name of files, namespace, and entries.")
   (program :initarg :program :reader compiled-program-program)
   (linkage :initarg :linkage :reader compiled-program-linkage)
   (stages :initarg :stages :reader compiled-program-stages)))

(defun compile-shader-program (program &key (targets *targets*))
  "Link PROGRAM (a SHADER-PROGRAM or its name), check the binding contract,
and lower every stage to each of TARGETS."
  (let* ((program (if (typep program 'shader:shader-program)
                      program
                      (shader:find-shader-program program)))
         (name (snake-identifier (shader:shader-object-name program))))
    (with-compilation-context ("program ~(~A~)" (shader:shader-object-name program))
      (let* ((linkage (shader:link-shader-program program))
             (vertex (shader:shader-program-linkage-specification
                      linkage :vertex)))
        (check-binding-contract linkage)
        (make-instance
         'compiled-program
         :name name :program program :linkage linkage
         :stages
         (loop for (stage . specification)
                 in (shader:shader-program-linkage-specifications linkage)
               for entry = (entry-point-name name stage)
               collect
               (with-compilation-context ("~(~A~) stage ~(~A~)"
                                          stage
                                          (shader:shader-object-name
                                           specification))
                 (make-instance
                  'compiled-stage
                  :stage stage :specification specification :entry-point entry
                  :msl (and (member :msl targets)
                            (msl:compile-msl specification
                                             :entry-point-name entry))
                  :hlsl (and (member :hlsl targets)
                             (hlsl:compile-hlsl
                              specification
                              :entry-point-name entry
                              :comparison-samplers
                              (shader:shader-program-linkage-comparison-samplers
                               linkage)
                              :interface
                              (and (eq stage :fragment) vertex
                                   (shader:shader-specification-outputs
                                    vertex))))))))))))

;;; JSON, written by hand: the manifest is small and its shape is fixed.

(defun write-json-string (string stream)
  (write-char #\" stream)
  (loop for character across string
        do (case character
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Tab (write-string "\\t" stream))
             (otherwise
              (if (< (char-code character) 32)
                  (format stream "\\u~4,'0X" (char-code character))
                  (write-char character stream)))))
  (write-char #\" stream))

(defun write-json (value stream &optional (indent 0))
  "Write VALUE: strings, integers, :TRUE, :FALSE, :NULL, (:ARRAY . ITEMS),
and (:OBJECT (KEY . VALUE) ...)."
  (flet ((newline (depth)
           (terpri stream)
           (loop repeat depth do (write-string "  " stream))))
    (cond
      ((stringp value) (write-json-string value stream))
      ((integerp value) (format stream "~D" value))
      ((eq value :true) (write-string "true" stream))
      ((eq value :false) (write-string "false" stream))
      ((eq value :null) (write-string "null" stream))
      ((and (consp value) (eq (first value) :array))
       (if (rest value)
           (progn
             (write-char #\[ stream)
             (loop for (item . more) on (rest value)
                   do (newline (1+ indent))
                      (write-json item stream (1+ indent))
                      (when more (write-char #\, stream)))
             (newline indent)
             (write-char #\] stream))
           (write-string "[]" stream)))
      ((and (consp value) (eq (first value) :object))
       (write-char #\{ stream)
       (loop for ((key . item) . more) on (rest value)
             do (newline (1+ indent))
                (write-json-string key stream)
                (write-string ": " stream)
                (write-json item stream (1+ indent))
                (when more (write-char #\, stream)))
       (newline indent)
       (write-char #\} stream))
      (t (error "Cannot write ~S as JSON." value)))))

(defun json-type-name (type)
  (if (shader:shader-struct-type-p type)
      (snake-identifier (shader:shader-type-name type))
      (string-downcase (symbol-name (shader:shader-type-name type)))))

(defun program-struct-types (linkage)
  "The structures the host lays out: storage-buffer elements, each after
the structures it contains.  #V16OXI"
  (let ((types nil))
    (labels ((note (type)
               (when (and (shader:shader-struct-type-p type)
                          (not (member type types)))
                 (dolist (field (shader:shader-struct-type-fields type))
                   (note (shader:shader-struct-field-type field)))
                 (unless (member type types)
                   (push type types)))))
      (dolist (resource (shader:shader-program-linkage-resources linkage))
        (let ((declaration
                (shader:shader-program-resource-declaration resource)))
          (when (typep declaration 'shader:shader-storage-buffer)
            (note (shader:find-shader-type
                   (shader:shader-storage-buffer-element-type
                    declaration)))))))
    (nreverse types)))

(defun struct-json (struct)
  `(:object
    ("name" . ,(json-type-name struct))
    ("cpp" . ,(camel-identifier (shader:shader-type-name struct)))
    ("size" . ,(shader:shader-struct-type-size struct))
    ("alignment" . ,(shader:shader-struct-type-alignment struct))
    ("fields"
     . (:array
        ,@(mapcar (lambda (field)
                    `(:object
                      ("name" . ,(snake-identifier
                                  (shader:shader-object-name field)))
                      ("type" . ,(json-type-name
                                  (shader:shader-struct-field-type field)))
                      ("offset" . ,(shader:shader-struct-field-offset field))))
                  (shader:shader-struct-type-fields struct))))))

(defun stage-file-name (compiled stage extension)
  (format nil "~A.~A.~A" (compiled-program-name compiled) (stage-name stage)
          extension))

(defun resource-json (resource)
  (let* ((declaration (shader:shader-program-resource-declaration resource))
         (binding (shader:shader-program-resource-binding resource))
         (kind (shader:shader-program-resource-kind resource)))
    `(:object
      ("name" . ,(snake-identifier (shader:shader-program-resource-name
                                    resource)))
      ("kind" . ,(substitute #\_ #\- (string-downcase (symbol-name kind))))
      ("family" . ,(string-downcase
                    (symbol-name
                     (shader:shader-program-resource-family resource))))
      ("binding" . ,binding)
      ("stages" . (:array ,@(mapcar #'stage-name
                                    (shader:shader-program-resource-stages
                                     resource))))
      ("size" . ,(if (typep declaration 'shader:shader-uniform-block)
                     (shader:shader-uniform-block-byte-size declaration)
                     0))
      ,@(typecase declaration
          (shader:shader-uniform-block
           `(("struct" . ,(camel-identifier
                           (shader:shader-object-name declaration)))
             ("members"
              . (:array
                 ,@(mapcar
                    (lambda (member)
                      `(:object
                        ("name" . ,(snake-identifier
                                    (shader:shader-object-name member)))
                        ("type" . ,(json-type-name
                                    (shader:shader-declaration-type member)))
                        ("offset" . ,(shader:shader-uniform-member-offset
                                      member))))
                    (shader:shader-uniform-block-members declaration))))))
          (shader:shader-storage-buffer
           `(("element" . ,(json-type-name
                            (shader:shader-storage-buffer-element-type
                             declaration)))
             ,@(when (shader:shader-struct-type-p
                      (shader:shader-storage-buffer-element-type declaration))
                 `(("struct" . ,(camel-identifier
                                 (shader:shader-type-name
                                  (shader:shader-storage-buffer-element-type
                                   declaration))))))
             ("stride" . ,(shader:shader-storage-buffer-element-stride
                           declaration)))))
      ("msl" . ,(format nil "[[~A(~D)]]"
                        (ecase (shader:shader-program-resource-family resource)
                          (:buffer "buffer")
                          (:texture "texture")
                          (:sampler "sampler"))
                        binding))
      ("hlsl" . ,(ecase kind
                   (:uniform-block (format nil "b~D" binding))
                   (:storage-buffer (format nil "t~D, space0" binding))
                   (:read-write-storage-buffer
                    (format nil "u~D, space0" binding))
                   ((:texture-2d :depth-texture-2d :uint-texture-2d)
                    (format nil "t~D, space1" binding))
                   ((:sampler :comparison-sampler)
                    (format nil "s~D" binding)))))))

(defun program-json (compiled)
  "The reflection manifest of COMPILED as a JSON string."
  (let* ((linkage (compiled-program-linkage compiled))
         (program (compiled-program-program compiled))
         (source (shader:shader-program-source-pathname program)))
    (with-output-to-string (stream)
      (write-json
       `(:object
         ("generator" . "luv-shaderc")
         ("name" . ,(compiled-program-name compiled))
         ("source" . ,(if source (file-namestring source) :null))
         ("stages"
          . (:array
             ,@(mapcar
                (lambda (compiled-stage)
                  (let ((stage (compiled-stage-stage compiled-stage)))
                    `(:object
                      ("stage" . ,(stage-name stage))
                      ("shader" . ,(string-downcase
                                    (symbol-name
                                     (shader:shader-object-name
                                      (compiled-stage-specification
                                       compiled-stage)))))
                      ("entry" . ,(compiled-stage-entry-point compiled-stage))
                      ,@(when (compiled-stage-msl compiled-stage)
                          `(("msl" . ,(stage-file-name compiled stage
                                                       "metal"))))
                      ,@(when (compiled-stage-hlsl compiled-stage)
                          `(("hlsl" . ,(stage-file-name compiled stage "hlsl"))
                            ("hlsl_profile"
                             . ,(hlsl:hlsl-profile stage)))))))
                (compiled-program-stages compiled))))
         ("resources"
          . (:array ,@(mapcar #'resource-json
                              (shader:shader-program-linkage-resources
                               linkage))))
         ("fragment_outputs"
          . (:array
             ,@(mapcar (lambda (output)
                         `(:object
                           ("name" . ,(snake-identifier
                                       (shader:shader-object-name output)))
                           ("location" . ,(shader:shader-interface-location
                                           output))
                           ("type" . ,(json-type-name
                                       (shader:shader-declaration-type
                                        output)))))
                       (shader:shader-program-linkage-color-outputs
                        linkage))))
         ("color_outputs"
          . ,(length (shader:shader-program-linkage-color-outputs linkage)))
         ,@(let ((structs (program-struct-types linkage)))
             (when structs
               `(("structs" . (:array ,@(mapcar #'struct-json structs))))))
         ,@(let ((compute (shader:shader-program-linkage-specification
                           linkage :compute)))
             (when compute
               `(("workgroup_size"
                  . (:array ,@(shader:shader-specification-workgroup-size
                               compute)))))))
       stream)
      (terpri stream))))

;;; The C++ header.

(defun stage-mask-text (stages)
  (format nil "~{stage_~A~^ | ~}" (mapcar #'stage-name stages)))

(defun cpp-field-type (type)
  "The C++ type holding one host-shareable value of TYPE."
  (let ((type (shader:find-shader-type type)))
    (cond ((shader:shader-struct-type-p type)
           (camel-identifier (shader:shader-type-name type)))
          ((shader:shader-matrix-type-p type)
           (format nil "std::array<float, ~D>"
                   (floor (shader:shader-type-byte-size type) 4)))
          (t
           (let ((scalar (ecase (shader:shader-type-scalar-kind type)
                           (:float "float")
                           (:uint "std::uint32_t")
                           (:int "std::int32_t")))
                 (count (shader:shader-type-component-count type)))
             (if (= count 1)
                 scalar
                 (format nil "std::array<~A, ~D>" scalar count)))))))

(defun write-struct-header (struct stream)
  "STRUCT as a C++ structure whose size and every offset are asserted to
be the shaders' (see SHADER:DEFINE-SHADER-STRUCT).  #V16OXI"
  (let ((type (camel-identifier (shader:shader-type-name struct))))
    (format stream "  struct ~A {~%" type)
    (dolist (field (shader:shader-struct-type-fields struct))
      (format stream "    ~A ~A;~%"
              (cpp-field-type (shader:shader-struct-field-type field))
              (snake-identifier (shader:shader-object-name field))))
    (format stream "  };~%  static_assert(sizeof(~A) == ~D);~%"
            type (shader:shader-struct-type-size struct))
    (dolist (field (shader:shader-struct-type-fields struct))
      (format stream "  static_assert(offsetof(~A, ~A) == ~D);~%"
              type (snake-identifier (shader:shader-object-name field))
              (shader:shader-struct-field-offset field)))
    (terpri stream)))

(defun program-header (compiled)
  "The C++ reflection header of COMPILED as a string."
  (let* ((linkage (compiled-program-linkage compiled))
         (resources (shader:shader-program-linkage-resources linkage))
         (source (shader:shader-program-source-pathname
                  (compiled-program-program compiled)))
         (blocks (remove-if-not
                  (lambda (resource)
                    (typep (shader:shader-program-resource-declaration
                            resource)
                           'shader:shader-uniform-block))
                  resources))
         (structs (program-struct-types linkage))
         (names (append
                 (mapcar (lambda (struct)
                           (camel-identifier (shader:shader-type-name struct)))
                         structs)
                 (mapcar (lambda (resource)
                           (camel-identifier
                            (shader:shader-object-name
                             (shader:shader-program-resource-declaration
                              resource))))
                         blocks))))
    (let ((duplicate (find-if (lambda (name)
                                (< 1 (count name names :test #'string=)))
                              names)))
      (when duplicate
        (shaderc-fail "Two C++ structures would be named ~A: rename the ~
                       uniform block or the shader structure."
                      duplicate)))
    (with-output-to-string (stream)
      (format stream "// Generated by luv-shaderc~@[ from ~A~]; do not edit.~%"
              (and source (file-namestring source)))
      (format stream "#pragma once~%#include <moppe/nhal/reflection.hh>~%")
      (format stream "#include <array>~%")
      (when structs
        (format stream "#include <cstddef>~%#include <cstdint>~%"))
      (terpri stream)
      (format stream "namespace moppe::nhal::shaders::~A {~%"
              (compiled-program-name compiled))
      (dolist (struct structs)
        (write-struct-header struct stream))
      (dolist (resource blocks)
        (let* ((declaration (shader:shader-program-resource-declaration resource))
               (type (camel-identifier (shader:shader-object-name declaration))))
          (format stream "  struct ~A {~%" type)
          (dolist (member (shader:shader-uniform-block-members declaration))
            ;; A vec4 lane is four floats; a mat4 is its four columns'
            ;; lanes, column-major: element (row r, column c) is [4c + r].
            (format stream "    std::array<float, ~D> ~A;~%"
                    (floor (shader:shader-type-byte-size
                            (shader:shader-declaration-type member))
                           4)
                    (snake-identifier (shader:shader-object-name member))))
          (format stream "  };~%  static_assert(sizeof(~A) == ~D);~%~%"
                  type (shader:shader-uniform-block-byte-size declaration))))
      (if resources
          (progn
            (format stream "  inline constexpr Resource resources[] = {~%")
            (dolist (resource resources)
              (let ((declaration
                      (shader:shader-program-resource-declaration resource)))
                (format stream "    {\"~A\", ResourceKind::~A, ~D,~%     ~A, ~A},~%"
                        (snake-identifier (shader:shader-object-name
                                           declaration))
                        (substitute #\_ #\-
                                    (string-downcase
                                     (symbol-name
                                      (shader:shader-program-resource-kind
                                       resource))))
                        (shader:shader-program-resource-binding resource)
                        (stage-mask-text
                         (shader:shader-program-resource-stages resource))
                        (if (typep declaration 'shader:shader-uniform-block)
                            (format nil "sizeof(~A)"
                                    (camel-identifier
                                     (shader:shader-object-name declaration)))
                            "0"))))
            (format stream "  };~%"))
          (format stream "  inline constexpr std::span<const Resource> ~
                          resources {};~%"))
      (flet ((entry (stage)
               (let ((compiled-stage
                       (find stage (compiled-program-stages compiled)
                             :key #'compiled-stage-stage)))
                 (if compiled-stage
                     (format nil "\"~A\"" (compiled-stage-entry-point
                                           compiled-stage))
                     "nullptr"))))
        (format stream "  inline constexpr Program program {~%")
        (format stream "    .name = \"~A\",~%" (compiled-program-name compiled))
        (format stream "    .vertex_entry = ~A,~%" (entry :vertex))
        (format stream "    .fragment_entry = ~A,~%" (entry :fragment))
        (format stream "    .compute_entry = ~A,~%" (entry :compute))
        (format stream "    .resources = resources,~%")
        (format stream "    .color_outputs = ~D,~%"
                (length (shader:shader-program-linkage-color-outputs linkage)))
        (format stream "  };~%")
        (let ((compute (shader:shader-program-linkage-specification
                        linkage :compute)))
          (when compute
            ;; Program has no field for it yet, so the size stands beside.
            (format stream "  inline constexpr std::array<std::uint32_t, 3> ~
                            workgroup_size {~{~D~^, ~}};~%"
                    (shader:shader-specification-workgroup-size compute))))
        (format stream "}~%")))))

;;; Writing.

(defun write-text-file (pathname text)
  (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create
                                   :external-format :utf-8)
    (write-string text stream))
  pathname)

(defun write-compiled-program (compiled directory)
  "Write COMPILED's stage documents, manifest, and header into DIRECTORY.
Return the written pathnames."
  (let ((directory (uiop:ensure-directory-pathname directory))
        (written nil))
    (ensure-directories-exist directory)
    (flet ((output (name text)
             (push (write-text-file (merge-pathnames name directory) text)
                   written)))
      (dolist (compiled-stage (compiled-program-stages compiled))
        (let ((stage (compiled-stage-stage compiled-stage)))
          (when (compiled-stage-msl compiled-stage)
            (output (stage-file-name compiled stage "metal")
                    (msl:msl-document-source
                     (compiled-stage-msl compiled-stage))))
          (when (compiled-stage-hlsl compiled-stage)
            (output (stage-file-name compiled stage "hlsl")
                    (hlsl:hlsl-document-source
                     (compiled-stage-hlsl compiled-stage))))))
      (output (format nil "~A.json" (compiled-program-name compiled))
              (program-json compiled))
      (output (format nil "~A.hh" (compiled-program-name compiled))
              (program-header compiled)))
    (nreverse written)))

(defun compile-shader-files (pathnames &key directory (targets *targets*))
  "Compile every program defined in PATHNAMES, writing into DIRECTORY when
given.  Return the compiled programs and, second, the written pathnames."
  (let ((compiled nil) (written nil) (names (make-hash-table :test #'equal)))
    (dolist (pathname pathnames)
      (let ((pathname (or (probe-file pathname)
                          (shaderc-fail "No such shader source file: ~A"
                                        pathname))))
        (with-compilation-context ("~A" (file-namestring pathname))
          (let ((programs (load-shader-source pathname)))
            (unless programs
              (shaderc-fail "~A defines no DEFINE-SHADER-PROGRAM."
                            (file-namestring pathname)))
            (dolist (program programs)
              (let* ((program (compile-shader-program program
                                                      :targets targets))
                     (name (compiled-program-name program))
                     (previous (gethash name names)))
                (when previous
                  (shaderc-fail "Program ~A is defined in both ~A and ~A."
                                name previous (file-namestring pathname)))
                (setf (gethash name names) (file-namestring pathname))
                (push program compiled)
                (when directory
                  (setf written
                        (append written
                                (write-compiled-program program
                                                        directory))))))))))
    (values (nreverse compiled) written)))
