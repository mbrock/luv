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

(defstruct instruction
  opcode
  result
  operands)

(defparameter *instructions*
  `((capability . ,(make-instruction
                    :opcode 17 :result :none
                    :operands '((:enum capability))))
    (memory-model . ,(make-instruction
                      :opcode 14 :result :none
                      :operands '((:enum addressing-model)
                                  (:enum memory-model))))
    (entry-point . ,(make-instruction
                     :opcode 15 :result :none
                     :operands '((:enum execution-model) :id :string
                                 (:rest :id))))
    (execution-mode . ,(make-instruction
                        :opcode 16 :result :none
                        :operands '(:id (:enum execution-mode)
                                    (:rest :literal))))
    (decorate . ,(make-instruction
                  :opcode 71 :result :none
                  :operands '(:id (:enum decoration) (:rest :literal))))
    (type-void . ,(make-instruction :opcode 19 :result :id))
    (type-int . ,(make-instruction
                  :opcode 21 :result :id
                  :operands '(:literal :literal)))
    (type-float . ,(make-instruction
                    :opcode 22 :result :id
                    :operands '(:literal)))
    (type-vector . ,(make-instruction
                     :opcode 23 :result :id
                     :operands '(:id :literal)))
    (type-image . ,(make-instruction
                    :opcode 25 :result :id
                    :operands '(:id (:enum dim) :literal :literal :literal
                                :literal (:enum image-format))))
    (type-pointer . ,(make-instruction
                      :opcode 32 :result :id
                      :operands '((:enum storage-class) :id)))
    (type-function . ,(make-instruction
                       :opcode 33 :result :id
                       :operands '(:id (:rest :id))))
    (constant . ,(make-instruction
                  :opcode 43 :result :typed
                  :operands '(:literal)))
    (variable . ,(make-instruction
                  :opcode 59 :result :typed
                  :operands '((:enum storage-class))))
    (load . ,(make-instruction
              :opcode 61 :result :typed :operands '(:id)))
    (vector-shuffle . ,(make-instruction
                        :opcode 79 :result :typed
                        :operands '(:id :id (:rest :literal))))
    (composite-construct . ,(make-instruction
                             :opcode 80 :result :typed
                             :operands '((:rest :id))))
    (composite-extract . ,(make-instruction
                           :opcode 81 :result :typed
                           :operands '(:id (:rest :literal))))
    (image-write . ,(make-instruction
                     :opcode 99 :result :none
                     :operands '(:id :id :id)))
    (convert-u-to-f . ,(make-instruction
                        :opcode 112 :result :typed :operands '(:id)))
    (f-mul . ,(make-instruction
               :opcode 133 :result :typed :operands '(:id :id)))
    (f-add . ,(make-instruction
               :opcode 129 :result :typed :operands '(:id :id)))
    (f-div . ,(make-instruction
               :opcode 136 :result :typed :operands '(:id :id)))
    (function . ,(make-instruction
                  :opcode 54 :result :typed
                  :operands '((:enum function-control) :id)))
    (function-end . ,(make-instruction :opcode 56 :result :none))
    (label . ,(make-instruction :opcode 248 :result :id))
    (return . ,(make-instruction :opcode 253 :result :none))))

(defparameter *enumerants*
  '((capability
     (shader . 1))
    (addressing-model
     (logical . 0))
    (memory-model
     (glsl-450 . 1))
    (execution-model
     (gl-compute . 5))
    (execution-mode
     (local-size . 17))
    (decoration
     (built-in . 11)
     (binding . 33)
     (descriptor-set . 34))
    (built-in
     (global-invocation-id . 28))
    (dim
     (2d . 1))
    (image-format
     (rgba8 . 4))
    (storage-class
     (uniform-constant . 0)
     (input . 1)
     (function . 7))
    (function-control
     (none . 0))))

(defun same-name-p (left right)
  (and (symbolp left) (symbolp right)
       (string-equal (symbol-name left) (symbol-name right))))

(defun named-value (name alist)
  (let ((pair (find name alist :key #'car :test #'same-name-p)))
    (values (cdr pair) (not (null pair)))))

(defun instruction-for (name)
  (named-value name *instructions*))

(defun enumerant-value (kind name form)
  (let ((kind-values (named-value kind *enumerants*)))
    (multiple-value-bind (value found-p) (named-value name kind-values)
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
