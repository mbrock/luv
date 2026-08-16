;;; A deliberately small SPIR-V assembler for shaders luv can execute today.
;;;
;;; Result IDs lead their instruction, which makes the data read naturally:
;;;
;;;   (%float type-float 32)
;;;   (%x load %float %pointer)
;;;   (return)
;;;
;;; The instruction classes and enumerant table are explicit subsets of the
;;; SPIR-V grammar.  Growing this file follows actual shader needs rather than
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
  ;; One-time migration from the short-lived structure-definition registry.
  ;; No user-held occurrences existed yet: the old instances were definitions
  ;; in *INSTRUCTIONS*, so retiring that class binding loses no shader IR.
  (let ((old-class (find-class 'instruction nil)))
    (when (and old-class
               (not (typep old-class 'closer-mop:standard-class)))
      (setf (find-class 'instruction) nil)
      (when (boundp '*instructions*)
        (makunbound '*instructions*))
      (dolist (name '(make-instruction copy-instruction instruction-p
                      instruction-opcode instruction-result
                      instruction-operands encode-instruction))
        (when (fboundp name)
          (fmakunbound name))))))

(defclass instruction-class (closer-mop:standard-class)
  ((opcode
    :initform nil
    :accessor instruction-class-opcode)
   (result-convention
    :initform :none
    :accessor instruction-class-result-convention)
   (lambda-list
    :initform nil
    :accessor instruction-class-lambda-list)
   (operand-specs
    :initform nil
    :accessor instruction-class-operand-specs)))

(defmethod closer-mop:validate-superclass
    ((class instruction-class) (superclass closer-mop:standard-class))
  (declare (ignore class superclass))
  t)

(defclass instruction ()
  ((result-id
    :initarg :result-id
    :initform nil
    :reader instruction-result-id)
   (result-type
    :initarg :result-type
    :initform nil
    :reader instruction-result-type))
  (:metaclass instruction-class))

(defclass typed-unary-instruction (instruction)
  ((value :initarg :value))
  (:metaclass instruction-class))

(defclass typed-binary-arithmetic-instruction (instruction)
  ((left :initarg :left)
   (right :initarg :right))
  (:metaclass instruction-class))

(defgeneric instruction-name (instruction-or-class))
(defgeneric instruction-opcode (instruction-or-class))
(defgeneric instruction-result-convention (instruction-or-class))

(defmethod instruction-name ((class instruction-class))
  (class-name class))

(defmethod instruction-name ((instruction instruction))
  (instruction-name (class-of instruction)))

(defmethod instruction-opcode ((class instruction-class))
  (instruction-class-opcode class))

(defmethod instruction-opcode ((instruction instruction))
  (instruction-opcode (class-of instruction)))

(defmethod instruction-result-convention ((class instruction-class))
  (instruction-class-result-convention class))

(defmethod instruction-result-convention ((instruction instruction))
  (instruction-result-convention (class-of instruction)))

(eval-when (:compile-toplevel :load-toplevel :execute)
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

  (defun instruction-operand-names (lambda-list)
    (remove '&rest lambda-list))

  (defun configure-instruction-class
      (class name lambda-list opcode result types)
    (unless (typep opcode '(unsigned-byte 16))
      (error "Bad opcode for SPIR-V instruction ~S: ~S." name opcode))
    (unless (member result '(:none :id :typed))
      (error "Bad result convention for SPIR-V instruction ~S: ~S."
             name result))
    (setf (instruction-class-opcode class) opcode
          (instruction-class-result-convention class) result
          (instruction-class-lambda-list class) lambda-list
          (instruction-class-operand-specs class)
          (instruction-operand-specs name lambda-list types))
    class))

(defmacro define-instruction (name lambda-list &rest options)
  "Define one instruction class in luv's SPIR-V backend vocabulary."
  (let ((opcode nil)
        (result :none)
        (types nil)
        (superclasses '(instruction))
        (inherited-operands-p nil))
    (dolist (option options)
      (case (first option)
        (:opcode (setf opcode (second option)))
        (:result (setf result (second option)))
        (:operands (setf types (rest option)))
        (:superclasses (setf superclasses (rest option)))
        (:inherited-operands (setf inherited-operands-p t))
        (otherwise (error "Unknown DEFINE-INSTRUCTION option ~S." option))))
    (unless opcode
      (error "SPIR-V instruction ~S has no opcode." name))
    (let ((slots
            (unless inherited-operands-p
              (loop for operand in (instruction-operand-names lambda-list)
                    collect
                    `(,operand
                      :initarg ,(intern (symbol-name operand) :keyword))))))
      `(progn
         (defclass ,name ,superclasses
           ,slots
           (:metaclass instruction-class))
         (eval-when (:load-toplevel :execute)
           (configure-instruction-class
            (find-class ',name) ',name ',lambda-list
            ,opcode ,result ',types))
         ',name))))

(defmacro define-enumeration (name &body entries)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (register-enumeration ',name ',entries)))

(defmacro define-typed-unary-instructions (&body definitions)
  `(progn
     ,@(loop for (name opcode) in definitions
             collect `(define-instruction ,name (value)
                        (:opcode ,opcode)
                        (:result :typed)
                        (:operands :id)
                        (:superclasses typed-unary-instruction)
                        (:inherited-operands)))))

(defmacro define-typed-binary-instructions (&body definitions)
  `(progn
     ,@(loop for (name opcode) in definitions
             collect `(define-instruction ,name (left right)
                        (:opcode ,opcode)
                        (:result :typed)
                        (:operands :id :id)
                        (:superclasses
                         typed-binary-arithmetic-instruction)
                        (:inherited-operands)))))

;;; Like SBCL's backend instruction files, this is executable treaty text:
;;; definitions create their own IR classes, and repetitive instruction
;;; families get a local definer instead of being expanded by hand.

(define-instruction capability (capability)
  (:opcode 17)
  (:operands (:enum capability)))

(define-instruction extension (name)
  (:opcode 10)
  (:operands :string))

(define-instruction ext-inst-import (name)
  (:opcode 11)
  (:result :id)
  (:operands :string))

;; The extended instruction number is a literal, so lowering may write the
;; readable (enum glsl-std-450 f-clamp) form rather than a bare integer.
(define-instruction ext-inst (set instruction &rest operands)
  (:opcode 12)
  (:result :typed)
  (:operands :id :literal :id))

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

(define-instruction member-decorate
    (target member decoration &rest literals)
  (:opcode 72)
  (:operands :id :literal (:enum decoration) :literal))

(define-instruction type-void () (:opcode 19) (:result :id))
(define-instruction type-bool () (:opcode 20) (:result :id))
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
(define-instruction type-sampler () (:opcode 26) (:result :id))
(define-instruction type-sampled-image (image-type)
  (:opcode 27) (:result :id) (:operands :id))
(define-instruction type-struct (&rest member-types)
  (:opcode 30) (:result :id) (:operands :id))
(define-instruction type-pointer (storage-class type)
  (:opcode 32) (:result :id) (:operands (:enum storage-class) :id))
(define-instruction type-function (return-type &rest parameter-types)
  (:opcode 33) (:result :id) (:operands :id :id))

(define-instruction constant (value)
  (:opcode 43) (:result :typed) (:operands :literal))
(define-instruction variable (storage-class)
  (:opcode 59) (:result :typed) (:operands (:enum storage-class)))
(define-instruction store (pointer object)
  (:opcode 62) (:operands :id :id))
(define-instruction access-chain (base &rest indexes)
  (:opcode 65) (:result :typed) (:operands :id :id))
(define-typed-unary-instructions
  (load 61)
  (convert-f-to-u 109)
  (convert-u-to-f 112)
  (f-negate 127))
(define-instruction vector-shuffle (left right &rest components)
  (:opcode 79) (:result :typed) (:operands :id :id :literal))
(define-instruction composite-construct (&rest constituents)
  (:opcode 80) (:result :typed) (:operands :id))
(define-instruction composite-extract (composite &rest indices)
  (:opcode 81) (:result :typed) (:operands :id :literal))
(define-instruction sampled-image (image sampler)
  (:opcode 86) (:result :typed) (:operands :id :id))
(define-instruction image-sample-implicit-lod (sampled-image coordinate)
  (:opcode 87) (:result :typed) (:operands :id :id))
(define-instruction image-sample-explicit-lod
    (sampled-image coordinate image-operands &rest operands)
  (:opcode 88) (:result :typed)
  (:operands :id :id (:enum image-operands) :id))
(define-instruction image-sample-dref-implicit-lod
    (sampled-image coordinate depth-reference)
  (:opcode 89) (:result :typed) (:operands :id :id :id))
(define-instruction image-fetch (image coordinate)
  (:opcode 95) (:result :typed) (:operands :id :id))
(define-instruction image-write (image coordinate texel)
  (:opcode 99) (:operands :id :id :id))
(define-instruction select (condition true-object false-object)
  (:opcode 169) (:result :typed) (:operands :id :id :id))
(define-typed-binary-instructions
  (i-add 128)
  (f-add 129)
  (i-sub 130)
  (f-sub 131)
  (i-mul 132)
  (f-mul 133)
  (u-div 134)
  (f-div 136)
  (u-mod 137)
  (dot 148)
  (shift-right-logical 194)
  (bitwise-and 199))
(define-typed-binary-instructions
  (i-equal 170)
  (u-greater-than 172)
  (u-greater-than-equal 174)
  (u-less-than 176)
  (u-less-than-equal 178)
  (f-ord-equal 180)
  (f-ord-less-than 184)
  (f-ord-greater-than 186)
  (f-ord-less-than-equal 188)
  (f-ord-greater-than-equal 190))
(define-instruction vector-times-scalar (vector scalar)
  (:opcode 142)
  (:result :typed)
  (:operands :id :id))
(define-instruction function (control function-type)
  (:opcode 54)
  (:result :typed)
  (:operands (:enum function-control) :id))
(define-instruction function-end () (:opcode 56))
(define-instruction phi (&rest pairs)
  (:opcode 245) (:result :typed) (:operands :id))
(define-instruction loop-merge (merge-block continue-target loop-control)
  (:opcode 246)
  (:operands :id :id (:enum loop-control)))
(define-instruction label () (:opcode 248) (:result :id))
(define-instruction branch (target)
  (:opcode 249) (:operands :id))
(define-instruction branch-conditional (condition true-label false-label)
  (:opcode 250) (:operands :id :id :id))
(define-instruction return () (:opcode 253))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *enumerants* nil)
  (unless (hash-table-p *enumerants*)
    (setf *enumerants* (make-hash-table :test #'equal)))

  (defun register-enumeration (name entries)
    (let ((values (make-hash-table :test #'equal)))
      (dolist (entry entries)
        (destructuring-bind (enumerant value) entry
          (setf (gethash (registry-key enumerant) values) value)))
      (setf (gethash (registry-key name) *enumerants*) values))))

(define-enumeration capability (shader 1))
(define-enumeration addressing-model (logical 0))
(define-enumeration memory-model (glsl-450 1))
(define-enumeration execution-model
  (vertex 0)
  (fragment 4)
  (gl-compute 5))
(define-enumeration execution-mode
  (origin-upper-left 7)
  (local-size 17))
(define-enumeration loop-control (none 0))
(define-enumeration decoration
  (block 2)
  (built-in 11)
  (location 30)
  (binding 33)
  (descriptor-set 34)
  (offset 35))
(define-enumeration built-in
  (position 0)
  (global-invocation-id 28)
  (vertex-index 42))
(define-enumeration dim (2d 1))
(define-enumeration image-format
  (unknown 0)
  (rgba8 4))
(define-enumeration image-operands (lod #x2))
;; GLSL.std.450 extended instruction numbers, spelled with this assembler's
;; word conventions.  Entries follow actual shader needs, like the opcodes.
(define-enumeration glsl-std-450
  (f-abs 4)
  (f-sign 6)
  (pow 26)
  (sqrt 31)
  (f-min 37)
  (f-max 40)
  (f-clamp 43)
  (step 48)
  (smooth-step 49)
  (normalize 69))
(define-enumeration storage-class
  (uniform-constant 0)
  (input 1)
  (uniform 2)
  (output 3)
  (function 7))
(define-enumeration function-control (none 0))

(defun same-name-p (left right)
  (and (symbolp left) (symbolp right)
       (string-equal (symbol-name left) (symbol-name right))))

(defun instruction-class-p (class)
  (typep class 'instruction-class))

(defun instruction-class-for (name)
  (when (symbolp name)
    (let* ((direct (find-class name nil))
           (local-symbol
             (find-symbol (symbol-name name) (find-package '#:luv.spir-v)))
           (local (and local-symbol (find-class local-symbol nil))))
      (cond ((instruction-class-p direct) direct)
            ((instruction-class-p local) local)))))

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
  (cond ((instruction-class-for (first form))
         (values nil (instruction-class-for (first form)) (rest form)))
        ((and (rest form) (instruction-class-for (second form)))
         (values (first form)
                 (instruction-class-for (second form))
                 (cddr form)))
        (t
         (error 'spir-v-error :form form :reason :unknown-instruction))))

(defun instruction-instance-initargs (class values form)
  (let* ((lambda-list (instruction-class-lambda-list class))
         (rest-position (position '&rest lambda-list))
         (required
           (if rest-position
               (subseq lambda-list 0 rest-position)
               lambda-list))
         (remaining (copy-list values))
         (initargs nil))
    (dolist (name required)
      (unless remaining
        (error 'spir-v-error :form form :reason :missing-operand
               :details name))
      (setf initargs
            (nconc initargs
                   (list (intern (symbol-name name) :keyword)
                         (pop remaining)))))
    (if rest-position
        (let ((name (elt lambda-list (1+ rest-position))))
          (setf initargs
                (nconc initargs
                       (list (intern (symbol-name name) :keyword)
                             remaining))))
        (when remaining
          (error 'spir-v-error :form form :reason :extra-operands
                 :details remaining)))
    initargs))

(defun parse-instruction (form)
  "Parse one surface FORM into an inspectable instruction occurrence."
  (multiple-value-bind (result-id class operands)
      (split-instruction-form form)
    (let ((result-convention (instruction-result-convention class))
          (result-type nil))
      (when (and result-id (eq result-convention :none))
        (error 'spir-v-error :form form :reason :unexpected-result))
      (when (and (null result-id) (not (eq result-convention :none)))
        (error 'spir-v-error :form form :reason :missing-result))
      (when (eq result-convention :typed)
        (unless operands
          (error 'spir-v-error :form form :reason :missing-result-type))
        (setf result-type (pop operands)))
      (apply #'make-instance class
             :result-id result-id
             :result-type result-type
             (instruction-instance-initargs class operands form)))))

(defun parse-module (forms)
  "Parse surface FORMS into a vector of instruction occurrences."
  (map 'vector
       (lambda (form)
         (if (typep form 'instruction) form (parse-instruction form)))
       forms))

(defun instruction-operand-values (instruction)
  (let* ((class (class-of instruction))
         (lambda-list (instruction-class-lambda-list class))
         (values nil))
    (loop while lambda-list
          for name = (pop lambda-list)
          do (if (eq name '&rest)
                 (cl:return
                   (nconc (nreverse values)
                          (copy-list
                           (slot-value instruction (pop lambda-list)))))
                 (push (slot-value instruction name) values))
          finally (cl:return (nreverse values)))))

(defgeneric instruction-form (instruction))

(defmethod instruction-form ((instruction instruction))
  "Return the canonical s-expression surface form for INSTRUCTION."
  (let ((name (instruction-name instruction))
        (operands (instruction-operand-values instruction)))
    (ecase (instruction-result-convention instruction)
      (:none (list* name operands))
      (:id (list* (instruction-result-id instruction) name operands))
      (:typed
       (list* (instruction-result-id instruction)
              name
              (instruction-result-type instruction)
              operands)))))

(defmethod print-object ((instruction instruction) stream)
  (print-unreadable-object (instruction stream :type t :identity t)
    (handler-case
        (prin1 (instruction-form instruction) stream)
      (unbound-slot ()
        (write-string "uninitialized" stream)))))

(defun collect-result-ids (instructions)
  (let ((ids (make-hash-table :test #'eq))
        (next-id 1))
    (map nil
         (lambda (instruction)
           (let ((result-id (instruction-result-id instruction)))
             (when result-id
               (when (gethash result-id ids)
                 (error 'spir-v-error
                        :form (instruction-form instruction)
                        :reason :duplicate-result
                        :details result-id))
               (setf (gethash result-id ids) next-id)
               (incf next-id))))
         instructions)
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

(defgeneric encode-instruction (instruction ids))

(defmethod encode-instruction ((instruction instruction) ids)
  (let* ((class (class-of instruction))
         (form (instruction-form instruction))
         (result-style (instruction-result-convention instruction))
         (operands (instruction-operand-values instruction))
         (prefix
           (ecase result-style
             (:none nil)
             (:id
              (list (id-word (instruction-result-id instruction)
                             ids form)))
             (:typed
              (list (id-word (instruction-result-type instruction)
                             ids form)
                    (id-word (instruction-result-id instruction)
                             ids form)))))
         (operand-words
           (encode-operands
            (copy-list (instruction-class-operand-specs class))
            operands ids form))
         (words (nconc prefix operand-words))
         (word-count (1+ (length words))))
    (cons (logior (ash word-count 16) (instruction-opcode instruction))
          words)))

(defun assemble (forms &key (version #x00010000) (generator 0))
  "Assemble surface FORMS or instruction occurrences into SPIR-V words.

This assembler intentionally recognizes only the vocabulary declared in
instruction classes and *ENUMERANTS*. Result symbols may be referenced before
their definitions."
  (let ((instructions (parse-module forms)))
    (multiple-value-bind (ids bound) (collect-result-ids instructions)
      (let ((words (make-array 64 :element-type '(unsigned-byte 32)
                                  :adjustable t :fill-pointer 0)))
        (dolist (word (list #x07230203 version generator bound 0))
          (vector-push-extend word words))
        (map nil
             (lambda (instruction)
               (dolist (word (encode-instruction instruction ids))
                 (vector-push-extend word words)))
             instructions)
        words))))

(defun write-spir-v (words pathname)
  "Write SPIR-V WORDS to PATHNAME in its defined little-endian byte order."
  (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede
                                   :element-type '(unsigned-byte 8))
    (loop for word across words
          do (dotimes (byte 4)
               (write-byte (ldb (byte 8 (* byte 8)) word) stream))))
  pathname)
