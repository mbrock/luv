;;; A deliberately small SPIR-V assembler for shaders luv can execute today.
;;;
;;; Result IDs lead their instruction, which makes the data read naturally:
;;;
;;;   (%float type-float 32)
;;;   (%x load %float %pointer)
;;;   (return)
;;;
;;; The instruction and enumerant tables are explicit subsets of the SPIR-V
;;; grammar.  Growing this file follows actual shader needs rather than
;;; importing the entire registry into the Lisp image.

(in-package #:luv.spir-v)

(define-condition spir-v-error (error)
  ((form
    :initarg :form
    :initform nil
    :reader spir-v-error-form)
   (reason
    :initarg :reason
    :reader spir-v-error-reason)
   (details
    :initarg :details
    :initform nil
    :reader spir-v-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Cannot assemble SPIR-V form ~S: ~A~@[ (~S)~]."
             (spir-v-error-form condition)
             (spir-v-error-reason condition)
             (spir-v-error-details condition)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct instruction
    opcode
    result
    operands)

  (defvar *instructions* nil)
  (defvar *enumerants* nil)
  ;; These variables used to hold alists; make source re-evaluation migrate a
  ;; durable image instead of requiring a restart.
  (unless (hash-table-p *instructions*)
    (setf *instructions* (make-hash-table :test #'equal)))
  (unless (hash-table-p *enumerants*)
    (setf *enumerants* (make-hash-table :test #'equal)))

  (defun registry-key (name)
    (string-upcase (symbol-name name)))

  (defun instruction-operand-specs (name lambda-list types)
    (let ((rest-position (position '&rest lambda-list)))
      (cond
        (rest-position
         (unless (and (= (+ rest-position 2) (length lambda-list))
                      (= (1+ rest-position) (length types)))
           (error "Bad operands for SPIR-V instruction ~S." name))
         (append (subseq types 0 rest-position)
                 (list (list :rest (elt types rest-position)))))
        (t
         (unless (= (length lambda-list) (length types))
           (error "Bad operands for SPIR-V instruction ~S." name))
         types))))

  (defun register-instruction (name lambda-list opcode result types)
    (unless (typep opcode '(unsigned-byte 16))
      (error "Bad opcode for SPIR-V instruction ~S: ~S." name opcode))
    (unless (member result '(:none :id :typed))
      (error "Bad result convention for SPIR-V instruction ~S: ~S."
             name result))
    (setf (gethash (registry-key name) *instructions*)
          (make-instruction
           :opcode opcode
           :result result
           :operands
           (instruction-operand-specs name lambda-list types))))

  (defun register-enumeration (name entries)
    (let ((values (make-hash-table :test #'equal)))
      (dolist (entry entries)
        (destructuring-bind (enumerant value) entry
          (setf (gethash (registry-key enumerant) values) value)))
      (setf (gethash (registry-key name) *enumerants*) values))))

(defmacro define-instruction (name lambda-list &rest options)
  "Define and register one instruction in luv's SPIR-V backend vocabulary."
  (let ((opcode nil)
        (result :none)
        (types nil))
    (dolist (option options)
      (case (first option)
        (:opcode (setf opcode (second option)))
        (:result (setf result (second option)))
        (:operands (setf types (rest option)))
        (otherwise (error "Unknown DEFINE-INSTRUCTION option ~S." option))))
    (unless opcode
      (error "SPIR-V instruction ~S has no opcode." name))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (register-instruction ',name ',lambda-list ,opcode ,result ',types))))

(defmacro define-enumeration (name &body entries)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-enumeration ',name ',entries)))

(defmacro define-typed-unary-instructions (&body definitions)
  `(progn
     ,@(loop for (name opcode) in definitions
             collect `(define-instruction ,name (value)
                        (:opcode ,opcode)
                        (:result :typed)
                        (:operands :id)))))

(defmacro define-typed-binary-instructions (&body definitions)
  `(progn
     ,@(loop for (name opcode) in definitions
             collect `(define-instruction ,name (left right)
                        (:opcode ,opcode)
                        (:result :typed)
                        (:operands :id :id)))))

;;; Like SBCL's backend instruction files, this is executable treaty text:
;;; definitions register themselves, and repetitive instruction families get
;;; a local definer instead of being expanded by hand.

(define-instruction capability (capability)
  (:opcode 17)
  (:operands (:enum capability)))

(define-instruction memory-model (addressing-model memory-model)
  (:opcode 14)
  (:operands (:enum addressing-model) (:enum memory-model)))

(define-instruction entry-point
    (execution-model entry-point name &rest interface)
  (:opcode 15)
  (:operands (:enum execution-model) :id :string :id))

(define-instruction execution-mode (entry-point mode &rest literals)
  (:opcode 16)
  (:operands :id (:enum execution-mode) :literal))

(define-instruction decorate (target decoration &rest literals)
  (:opcode 71)
  (:operands :id (:enum decoration) :literal))

(define-instruction type-void () (:opcode 19) (:result :id))
(define-instruction type-int (width signedness)
  (:opcode 21) (:result :id) (:operands :literal :literal))
(define-instruction type-float (width)
  (:opcode 22) (:result :id) (:operands :literal))
(define-instruction type-vector (component count)
  (:opcode 23) (:result :id) (:operands :id :literal))
(define-instruction type-image
    (sampled-type dim depth arrayed multisampled sampled format)
  (:opcode 25)
  (:result :id)
  (:operands :id (:enum dim) :literal :literal :literal :literal
             (:enum image-format)))
(define-instruction type-pointer (storage-class type)
  (:opcode 32) (:result :id) (:operands (:enum storage-class) :id))
(define-instruction type-function (return-type &rest parameter-types)
  (:opcode 33) (:result :id) (:operands :id :id))

(define-instruction constant (value)
  (:opcode 43) (:result :typed) (:operands :literal))
(define-instruction variable (storage-class)
  (:opcode 59) (:result :typed) (:operands (:enum storage-class)))
(define-typed-unary-instructions
  (load 61)
  (convert-u-to-f 112))
(define-instruction vector-shuffle (left right &rest components)
  (:opcode 79) (:result :typed) (:operands :id :id :literal))
(define-instruction composite-construct (&rest constituents)
  (:opcode 80) (:result :typed) (:operands :id))
(define-instruction composite-extract (composite &rest indices)
  (:opcode 81) (:result :typed) (:operands :id :literal))
(define-instruction image-write (image coordinate texel)
  (:opcode 99) (:operands :id :id :id))
(define-typed-binary-instructions
  (f-add 129)
  (f-mul 133)
  (f-div 136))
(define-instruction function (control function-type)
  (:opcode 54)
  (:result :typed)
  (:operands (:enum function-control) :id))
(define-instruction function-end () (:opcode 56))
(define-instruction label () (:opcode 248) (:result :id))
(define-instruction return () (:opcode 253))

(define-enumeration capability (shader 1))
(define-enumeration addressing-model (logical 0))
(define-enumeration memory-model (glsl-450 1))
(define-enumeration execution-model (gl-compute 5))
(define-enumeration execution-mode (local-size 17))
(define-enumeration decoration
  (built-in 11)
  (binding 33)
  (descriptor-set 34))
(define-enumeration built-in (global-invocation-id 28))
(define-enumeration dim (2d 1))
(define-enumeration image-format (rgba8 4))
(define-enumeration storage-class
  (uniform-constant 0)
  (input 1)
  (function 7))
(define-enumeration function-control (none 0))

(defun same-name-p (left right)
  (and (symbolp left) (symbolp right)
       (string-equal (symbol-name left) (symbol-name right))))

(defun instruction-for (name)
  (and (symbolp name)
       (gethash (registry-key name) *instructions*)))

(defun enumerant-value (kind name form)
  (let ((kind-values
          (and (symbolp kind)
               (gethash (registry-key kind) *enumerants*))))
    (multiple-value-bind (value found-p)
        (and kind-values (symbolp name)
             (gethash (registry-key name) kind-values))
      (if found-p
          value
          (error 'spir-v-error :form form :reason :unknown-enumerant
                 :details (list kind name))))))

(defun split-instruction-form (form)
  (unless (and (consp form) (symbolp (first form)))
    (error 'spir-v-error :form form :reason :malformed-instruction))
  (cond ((instruction-for (first form))
         (values nil (first form) (rest form)))
        ((and (rest form) (instruction-for (second form)))
         (values (first form) (second form) (cddr form)))
        (t
         (error 'spir-v-error :form form :reason :unknown-instruction))))

(defun collect-result-ids (forms)
  (let ((ids (make-hash-table :test #'eq))
        (next-id 1))
    (dolist (form forms)
      (multiple-value-bind (result-name operation operands)
          (split-instruction-form form)
        (declare (ignore operands))
        (let ((instruction (instruction-for operation)))
          (when (and result-name
                     (eq :none (instruction-result instruction)))
            (error 'spir-v-error :form form :reason :unexpected-result))
          (when (and (null result-name)
                     (not (eq :none (instruction-result instruction))))
            (error 'spir-v-error :form form :reason :missing-result))
          (when result-name
            (when (gethash result-name ids)
              (error 'spir-v-error :form form :reason :duplicate-result
                     :details result-name))
            (setf (gethash result-name ids) next-id)
            (incf next-id)))))
    (values ids next-id)))

(defun id-word (name ids form)
  (or (and (symbolp name) (gethash name ids))
      (error 'spir-v-error :form form :reason :unknown-id :details name)))

(defun single-float-word (value)
  (cffi:with-foreign-object (storage :float)
    (setf (cffi:mem-ref storage :float) (coerce value 'single-float))
    (cffi:mem-ref storage :uint32)))

(defun literal-word (value form)
  (cond ((typep value '(unsigned-byte 32)) value)
        ((and (integerp value) (<= (- (ash 1 31)) value -1))
         (ldb (byte 32 0) value))
        ((realp value) (single-float-word value))
        ((and (consp value) (same-name-p (first value) 'float)
              (= 2 (length value)) (realp (second value)))
         (single-float-word (second value)))
        ((and (consp value) (same-name-p (first value) 'enum)
              (= 3 (length value)))
         (enumerant-value (second value) (third value) form))
        (t
         (error 'spir-v-error :form form :reason :invalid-literal
                :details value))))

(defun string-words (string form)
  (unless (stringp string)
    (error 'spir-v-error :form form :reason :expected-string
           :details string))
  (let* ((octets
           (append
            (loop for character across string
                  for code = (char-code character)
                  unless (<= code #xff)
                    do (error 'spir-v-error :form form
                              :reason :non-octet-string :details string)
                  collect code)
            '(0)))
         (word-count (ceiling (length octets) 4)))
    (loop for word-index below word-count
          collect
          (loop for byte-index below 4
                for index = (+ (* word-index 4) byte-index)
                when (< index (length octets))
                  sum (ash (nth index octets) (* byte-index 8))))))

(defun encode-one-operand (spec value ids form)
  (cond ((eq spec :id) (list (id-word value ids form)))
        ((eq spec :literal) (list (literal-word value form)))
        ((eq spec :string) (string-words value form))
        ((and (consp spec) (eq :enum (first spec)))
         (list (enumerant-value (second spec) value form)))
        (t
         (error 'spir-v-error :form form :reason :invalid-operand-spec
                :details spec))))

(defun encode-operands (specs values ids form)
  (let ((words nil))
    (loop while specs
          for spec = (pop specs)
          do (if (and (consp spec) (eq :rest (first spec)))
                 (progn
                   (dolist (value values)
                     (setf words
                           (nconc words
                                  (encode-one-operand
                                   (second spec) value ids form))))
                   (setf values nil specs nil))
                 (progn
                   (unless values
                     (error 'spir-v-error :form form
                            :reason :missing-operand :details spec))
                   (setf words
                         (nconc words
                                (encode-one-operand
                                 spec (pop values) ids form))))))
    (when values
      (error 'spir-v-error :form form :reason :extra-operands
             :details values))
    words))

(defun encode-instruction (form ids)
  (multiple-value-bind (result-name operation source-operands)
      (split-instruction-form form)
    (let* ((instruction (instruction-for operation))
           (result-style (instruction-result instruction))
           (operands source-operands)
           (prefix
             (ecase result-style
               (:none nil)
               (:id (list (id-word result-name ids form)))
               (:typed
                (unless operands
                  (error 'spir-v-error :form form
                         :reason :missing-result-type))
                (list (id-word (pop operands) ids form)
                      (id-word result-name ids form)))))
           (operand-words
             (encode-operands (copy-list (instruction-operands instruction))
                              operands ids form))
           (words (nconc prefix operand-words))
           (word-count (1+ (length words))))
      (cons (logior (ash word-count 16) (instruction-opcode instruction))
            words))))

(defun assemble (forms &key (version #x00010000) (generator 0))
  "Assemble FORMS into a vector of SPIR-V 32-bit words.

This assembler intentionally recognizes only the vocabulary declared in
*INSTRUCTIONS* and *ENUMERANTS*.  Result symbols may be referenced before their
definitions."
  (multiple-value-bind (ids bound) (collect-result-ids forms)
    (let ((words (make-array 64 :element-type '(unsigned-byte 32)
                                :adjustable t :fill-pointer 0)))
      (dolist (word (list #x07230203 version generator bound 0))
        (vector-push-extend word words))
      (dolist (form forms)
        (dolist (word (encode-instruction form ids))
          (vector-push-extend word words)))
      words)))

(defun write-spir-v (words pathname)
  "Write SPIR-V WORDS to PATHNAME in its defined little-endian byte order."
  (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (loop for word across words
          do (dotimes (byte 4)
               (write-byte (ldb (byte 8 (* byte 8)) word) stream))))
  pathname)

(defun gradient-compute-shader (&key (width 640) (height 480))
  "Assemble a compute shader which writes an XY color gradient to RGBA8."
  (unless (and (integerp width) (plusp width)
               (integerp height) (plusp height))
    (error 'spir-v-error :form nil :reason :invalid-image-size
           :details (list width height)))
  (assemble
   `((capability shader)
     (memory-model logical glsl-450)
     (entry-point gl-compute %main "main" %global-id)
     (execution-mode %main local-size 8 8 1)
     (decorate %global-id built-in (enum built-in global-invocation-id))
     (decorate %output-image descriptor-set 0)
     (decorate %output-image binding 0)

     (%void type-void)
     (%uint type-int 32 0)
     (%float type-float 32)
     (%uvec2 type-vector %uint 2)
     (%uvec3 type-vector %uint 3)
     (%vec4 type-vector %float 4)
     (%storage-image type-image %float 2d 0 0 0 2 rgba8)
     (%input-uvec3-pointer type-pointer input %uvec3)
     (%image-pointer type-pointer uniform-constant %storage-image)
     (%function-type type-function %void)

     (%inverse-width constant %float ,(/ 1.0 width))
     (%inverse-height constant %float ,(/ 1.0 height))
     (%blue constant %float 0.25)
     (%one constant %float 1.0)
     (%global-id variable %input-uvec3-pointer input)
     (%output-image variable %image-pointer uniform-constant)

     (%main function %void none %function-type)
     (%entry label)
     (%global-value load %uvec3 %global-id)
     (%coordinate vector-shuffle %uvec2 %global-value %global-value 0 1)
     (%x composite-extract %uint %coordinate 0)
     (%y composite-extract %uint %coordinate 1)
     (%xf convert-u-to-f %float %x)
     (%yf convert-u-to-f %float %y)
     (%red f-mul %float %xf %inverse-width)
     (%green f-mul %float %yf %inverse-height)
     (%color composite-construct %vec4 %red %green %blue %one)
     (%image load %storage-image %output-image)
     (image-write %image %coordinate %color)
     (return)
     (function-end))))
