;;;; The TL wire codec.
;;;;
;;;; TL is a tagged binary format: every boxed value begins with a 32-bit
;;;; little-endian constructor id, and the id tells you how to read the rest.
;;;; That is a vocabulary whose members grow one at a time and each carry
;;;; their own behaviour, so each constructor is a class here, and reading a
;;;; boxed value is a table lookup from id to class followed by ordinary
;;;; generic dispatch.  Adding a constructor is one DEFINE-TL-OBJECT form and
;;;; touches nothing else.
;;;;
;;;; The id-to-name table itself stays a hash table on purpose: the numbering
;;;; belongs to Telegram's schema, not to us, and a lookup table is the
;;;; honest shape for a vocabulary someone else numbers.

(in-package #:telegram.tl)

(defconstant +vector-constructor+ #x1CB5C415
  "The constructor id of TL's `vector'.")

(defconstant +bool-true-constructor+ #x997275B5)
(defconstant +bool-false-constructor+ #xBC799737)

;;;; Conditions

(define-condition tl-error (error)
  ()
  (:documentation "Something was wrong with a TL byte stream."))

(define-condition short-tl-data (tl-error)
  ((needed :initarg :needed :reader short-tl-data-needed)
   (available :initarg :available :reader short-tl-data-available))
  (:report (lambda (condition stream)
             (format stream "TL stream ended early: needed ~D byte~:P, ~D left."
                     (short-tl-data-needed condition)
                     (short-tl-data-available condition))))
  (:documentation "A read ran past the end of the available bytes."))

(define-condition trailing-tl-data (tl-error)
  ((remaining :initarg :remaining :reader trailing-tl-data-remaining))
  (:report (lambda (condition stream)
             (format stream "~D unread byte~:P after a complete TL value."
                     (trailing-tl-data-remaining condition))))
  (:documentation
   "A value decoded successfully but did not consume its whole message.  For
a protocol this exact, that means we read the wrong thing."))

(define-condition unexpected-tl-constructor (tl-error)
  ((id :initarg :id :reader unexpected-tl-constructor-id)
   (expected :initarg :expected :initform nil
             :reader unexpected-tl-constructor-expected))
  (:report (lambda (condition stream)
             (format stream "Unexpected TL constructor #x~8,'0X~@[, wanted ~
~{#x~8,'0X~^ or ~}~]."
                     (unexpected-tl-constructor-id condition)
                     (unexpected-tl-constructor-expected condition))))
  (:documentation "A boxed value was not of the constructor the caller wanted."))

(define-condition unknown-tl-constructor (tl-error)
  ((id :initarg :id :reader unexpected-tl-constructor-id))
  (:report (lambda (condition stream)
             (format stream "No TL constructor is defined for #x~8,'0X."
                     (unexpected-tl-constructor-id condition))))
  (:documentation
   "A constructor id arrived that this image has no definition for.  During
schema work this is information, not a defect: it names exactly what to
define next."))

;;;; Readers

(defstruct (tl-reader (:constructor %make-tl-reader (octets position end)))
  "A cursor over a byte vector.  Dense data, no object identity: readers are
made and dropped per message."
  (octets (octets:make-octets 0) :type octets:octets)
  (position 0 :type fixnum)
  (end 0 :type fixnum))

(defun make-tl-reader (sequence &key (start 0) (end (length sequence)))
  "A reader over SEQUENCE between START and END."
  (%make-tl-reader (octets:to-octets sequence) start end))

(declaim (inline tl-reader-remaining tl-reader-exhausted-p))
(defun tl-reader-remaining (reader)
  "How many bytes READER has not consumed."
  (- (tl-reader-end reader) (tl-reader-position reader)))

(defun tl-reader-exhausted-p (reader)
  "Has READER consumed everything it was given?"
  (zerop (tl-reader-remaining reader)))

(defun expect-tl-end (reader)
  "Signal TRAILING-TL-DATA unless READER is exhausted.  Called at the end of
every top-level decode, because in MTProto a surplus byte is a misread
message rather than a harmless extra."
  (unless (tl-reader-exhausted-p reader)
    (error 'trailing-tl-data :remaining (tl-reader-remaining reader)))
  reader)

(defun take-octets (reader count)
  "Consume and return COUNT raw bytes."
  (let ((available (tl-reader-remaining reader)))
    (when (> count available)
      (error 'short-tl-data :needed count :available available)))
  (let* ((start (tl-reader-position reader))
         (result (subseq (tl-reader-octets reader) start (+ start count))))
    (setf (tl-reader-position reader) (+ start count))
    result))

(defun take-unsigned (reader size)
  "Consume SIZE bytes as a little-endian unsigned integer."
  (let ((available (tl-reader-remaining reader)))
    (when (> size available)
      (error 'short-tl-data :needed size :available available)))
  (let ((octets (tl-reader-octets reader))
        (start (tl-reader-position reader))
        (value 0))
    (loop for index from (1- size) downto 0
          do (setf value (logior (ash value 8) (aref octets (+ start index)))))
    (setf (tl-reader-position reader) (+ start size))
    value))

(defun unsigned-signed (value bits)
  "Reinterpret an unsigned VALUE of BITS bits as two's-complement signed."
  (if (logbitp (1- bits) value) (- value (ash 1 bits)) value))

(defun read-tl-int (reader)
  "A TL `int': 32 bits, signed, little-endian."
  (unsigned-signed (take-unsigned reader 4) 32))

(defun read-tl-long (reader)
  "A TL `long' read as unsigned.  Fingerprints and message ids want this face."
  (take-unsigned reader 8))

(defun read-tl-signed-long (reader)
  "A TL `long' read as signed.  Salts and session ids want this face."
  (unsigned-signed (take-unsigned reader 8) 64))

(defun read-tl-double (reader)
  "A TL `double': IEEE 754 binary64, little-endian."
  (let ((bits (take-unsigned reader 8)))
    (sb-kernel:make-double-float (unsigned-signed (ldb (byte 32 32) bits) 32)
                                 (ldb (byte 32 0) bits))))

(defun read-tl-int128 (reader)
  "A TL `int128': sixteen opaque bytes, kept as bytes."
  (take-octets reader 16))

(defun read-tl-int256 (reader)
  "A TL `int256': thirty-two opaque bytes, kept as bytes."
  (take-octets reader 32))

(defun tl-padding-length (length)
  "How many zero bytes follow a length-prefixed field whose prefix and
payload together occupy LENGTH bytes."
  (mod (- 4 (mod length 4)) 4))

(defun read-tl-bytes (reader)
  "A TL `bytes' or `string' payload: a short or long length prefix, the
bytes, then zero padding up to a four-byte boundary."
  (let ((first (take-unsigned reader 1)))
    (multiple-value-bind (length prefix)
        (if (< first 254)
            (values first 1)
            (values (take-unsigned reader 3) 4))
      (prog1 (take-octets reader length)
        (take-octets reader (tl-padding-length (+ prefix length)))))))

(defun read-tl-string (reader)
  "A TL `string', decoded as UTF-8."
  (octets:octets-string (read-tl-bytes reader)))

(defun read-tl-bool (reader)
  "A boxed TL `Bool'."
  (let ((id (take-unsigned reader 4)))
    (cond ((= id +bool-true-constructor+) t)
          ((= id +bool-false-constructor+) nil)
          (t (error 'unexpected-tl-constructor
                    :id id
                    :expected (list +bool-true-constructor+
                                    +bool-false-constructor+))))))

(defun read-tl-vector (reader element-reader)
  "A boxed TL `vector', each element read by ELEMENT-READER."
  (let ((id (take-unsigned reader 4)))
    (unless (= id +vector-constructor+)
      (error 'unexpected-tl-constructor :id id
                                        :expected (list +vector-constructor+))))
  (let ((count (take-unsigned reader 4)))
    (loop repeat count collect (funcall element-reader reader))))

(defun read-tl-raw (reader &optional count)
  "COUNT bytes verbatim, or everything left when COUNT is omitted.  This is
how a payload that another layer will interpret -- an RPC result, a container
member -- is carried without being parsed here."
  (take-octets reader (or count (tl-reader-remaining reader))))

;;;; Writers

(defstruct (tl-writer (:constructor make-tl-writer ()))
  "An append-only byte buffer."
  (buffer (make-array 64 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0)))

(defun tl-writer-length (writer)
  "How many bytes WRITER holds."
  (fill-pointer (tl-writer-buffer writer)))

(defun tl-writer-octets (writer)
  "The bytes WRITER has accumulated, as a fresh dense vector."
  (octets:to-octets (copy-seq (tl-writer-buffer writer))))

(defmacro with-tl-writer ((variable) &body body)
  "Bind VARIABLE to a fresh writer, run BODY, and return the bytes written.
Every encoder in this system is spelled with this macro."
  `(let ((,variable (make-tl-writer)))
     ,@body
     (tl-writer-octets ,variable)))

(defun write-tl-raw (writer sequence)
  "Append SEQUENCE verbatim."
  (let ((buffer (tl-writer-buffer writer)))
    (map nil (lambda (byte) (vector-push-extend byte buffer)) sequence))
  writer)

(defun write-unsigned (writer value size)
  "Append VALUE as SIZE little-endian bytes."
  (let ((buffer (tl-writer-buffer writer)))
    (dotimes (index size writer)
      (vector-push-extend (ldb (byte 8 (* 8 index)) value) buffer))))

(defun write-tl-constructor (writer id)
  "A 32-bit constructor id, which is unsigned and so does not fit `int'."
  (check-type id (unsigned-byte 32))
  (write-unsigned writer id 4))

(defun write-tl-int (writer value)
  "A TL `int'."
  (check-type value (signed-byte 32))
  (write-unsigned writer (ldb (byte 32 0) value) 4))

(defun write-tl-long (writer value)
  "A TL `long', given unsigned."
  (check-type value (unsigned-byte 64))
  (write-unsigned writer value 8))

(defun write-tl-signed-long (writer value)
  "A TL `long', given signed."
  (check-type value (signed-byte 64))
  (write-unsigned writer (ldb (byte 64 0) value) 8))

(defun write-tl-double (writer value)
  "A TL `double'."
  (let ((double (float value 1d0)))
    (write-unsigned writer
                    (logior (ash (ldb (byte 32 0)
                                      (sb-kernel:double-float-high-bits double))
                                 32)
                            (sb-kernel:double-float-low-bits double))
                    8)))

(defun write-tl-int128 (writer value)
  "A TL `int128'."
  (assert (= 16 (length value)) (value) "int128 must be 16 bytes.")
  (write-tl-raw writer value))

(defun write-tl-int256 (writer value)
  "A TL `int256'."
  (assert (= 32 (length value)) (value) "int256 must be 32 bytes.")
  (write-tl-raw writer value))

(defun write-tl-bytes (writer sequence)
  "A TL `bytes' payload, with its length prefix and zero padding."
  (let* ((bytes (octets:to-octets sequence))
         (length (length bytes))
         (prefix (if (< length 254) 1 4)))
    (if (< length 254)
        (write-unsigned writer length 1)
        (progn (write-unsigned writer 254 1)
               (write-unsigned writer length 3)))
    (write-tl-raw writer bytes)
    (dotimes (index (tl-padding-length (+ prefix length)))
      (write-unsigned writer 0 1))
    writer))

(defun write-tl-string (writer string)
  "A TL `string', encoded as UTF-8."
  (write-tl-bytes writer (octets:string-octets string)))

(defun write-tl-bool (writer value)
  "A boxed TL `Bool'."
  (write-unsigned writer
                  (if value +bool-true-constructor+ +bool-false-constructor+)
                  4))

(defun write-tl-vector (writer items element-writer)
  "A boxed TL `vector', each element written by ELEMENT-WRITER."
  (write-unsigned writer +vector-constructor+ 4)
  (write-unsigned writer (length items) 4)
  (dolist (item items writer)
    (funcall element-writer writer item)))

;;;; The typed slot vocabulary
;;;;
;;;; A TL type name is a symbol -- INT, BYTES, (VECTOR LONG) -- and reading
;;;; or writing one is a generic function specialized by EQL on that symbol.
;;;; The compound forms pass the whole specification alongside the head, so
;;;; (VECTOR (VECTOR LONG)) needs no special case.

(defun tl-specification-kind (specification)
  "The symbol that decides how SPECIFICATION is read and written."
  (if (consp specification) (first specification) specification))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun canonical-tl-specification (specification)
    "Re-intern the type names in SPECIFICATION here.

A schema is written in whatever package owns its constructors, so INT128
read there is not the INT128 these methods are specialized on.  Canonicalizing
at definition time lets a schema name types unqualified and still reach this
vocabulary -- and keeps the vocabulary genuinely extensible, since a new type
is a method on a symbol rather than an entry in a table."
    (cond ((consp specification)
           (mapcar #'canonical-tl-specification specification))
          ((symbolp specification)
           (intern (symbol-name specification) '#:telegram.tl))
          (t specification))))

(defgeneric read-tl-value (kind specification reader)
  (:documentation
   "Read one value of SPECIFICATION from READER.  KIND is the head symbol of
SPECIFICATION and is what methods specialize on."))

(defgeneric write-tl-value (kind specification writer value)
  (:documentation
   "Write VALUE as SPECIFICATION into WRITER."))

(defun read-tl (specification reader)
  "Read one value of SPECIFICATION."
  (read-tl-value (tl-specification-kind specification) specification reader))

(defun write-tl (specification writer value)
  "Write VALUE as SPECIFICATION."
  (write-tl-value (tl-specification-kind specification) specification
                  writer value))

(macrolet ((define-primitive (kind reader-function writer-function)
             `(progn
                (defmethod read-tl-value ((kind (eql ',kind)) specification
                                          reader)
                  (declare (ignore specification))
                  (,reader-function reader))
                (defmethod write-tl-value ((kind (eql ',kind)) specification
                                           writer value)
                  (declare (ignore specification))
                  (,writer-function writer value)))))
  (define-primitive int read-tl-int write-tl-int)
  (define-primitive long read-tl-long write-tl-long)
  (define-primitive signed-long read-tl-signed-long write-tl-signed-long)
  (define-primitive int128 read-tl-int128 write-tl-int128)
  (define-primitive int256 read-tl-int256 write-tl-int256)
  (define-primitive double read-tl-double write-tl-double)
  (define-primitive bytes read-tl-bytes write-tl-bytes)
  (define-primitive string read-tl-string write-tl-string)
  (define-primitive bool read-tl-bool write-tl-bool))

(defmethod read-tl-value ((kind (eql 'raw)) specification reader)
  (declare (ignore specification))
  (read-tl-raw reader))

(defmethod write-tl-value ((kind (eql 'raw)) specification writer value)
  (declare (ignore specification))
  (write-tl-raw writer value))

(defmethod read-tl-value ((kind (eql 'vector)) specification reader)
  (let ((element (second specification)))
    (read-tl-vector reader (lambda (reader) (read-tl element reader)))))

(defmethod write-tl-value ((kind (eql 'vector)) specification writer value)
  (let ((element (second specification)))
    (write-tl-vector writer value
                     (lambda (writer item) (write-tl element writer item)))))

;;;; TL objects

(defclass tl-object ()
  ()
  (:documentation
   "A decoded TL constructor.  Subclasses are generated by DEFINE-TL-OBJECT
and know their own constructor id, slot layout, and codec."))

(defvar *tl-constructors* (make-hash-table)
  "Constructor id to constructor name.  Telegram owns this numbering, so it
is a table rather than a protocol.")

(defun tl-constructor-name (id)
  "The symbol naming the constructor with ID, or NIL."
  (values (gethash id *tl-constructors*)))

(defun (setf tl-constructor-name) (name id)
  (setf (gethash id *tl-constructors*) name))

(defgeneric tl-constructor-id (designator)
  (:documentation
   "The 32-bit constructor id of a TL object or of the symbol naming one."))

(defgeneric tl-object-slots (designator)
  (:documentation
   "A list of (SLOT-NAME TYPE-SPECIFICATION ACCESSOR) describing how the
constructor is laid out on the wire.  Printing, decoding, and encoding all
read this one description."))

(defmethod tl-object-slots ((designator t))
  (declare (ignore designator))
  '())

(defgeneric decode-tl-body (name reader)
  (:documentation
   "Read the body of the constructor NAME -- everything after its id -- from
READER and return the object."))

(defgeneric encode-tl (object writer)
  (:documentation
   "Write OBJECT, constructor id first, into WRITER."))

(defun encode-tl-octets (object)
  "OBJECT encoded as a standalone byte vector."
  (with-tl-writer (writer) (encode-tl object writer)))

(defun decode-tl-object (reader)
  "Read one boxed TL object from READER."
  (let* ((id (take-unsigned reader 4))
         (name (tl-constructor-name id)))
    (unless name
      (error 'unknown-tl-constructor :id id))
    (decode-tl-body name reader)))

(defun decode-tl-octets (sequence &key (expect-end t))
  "Decode SEQUENCE as one boxed TL object.  By default the whole sequence
must be consumed."
  (let* ((reader (make-tl-reader sequence))
         (object (decode-tl-object reader)))
    (when expect-end (expect-tl-end reader))
    object))

(defmethod read-tl-value ((kind (eql 'object)) specification reader)
  (declare (ignore specification))
  (decode-tl-object reader))

(defmethod write-tl-value ((kind (eql 'object)) specification writer value)
  (declare (ignore specification))
  (encode-tl value writer))

(defmethod print-object ((object tl-object) stream)
  (print-unreadable-object (object stream :type t)
    (loop for (name specification accessor) in (tl-object-slots object)
          do (format stream " ~(~A~)=~A" name
                     (format-tl-value specification (funcall accessor object))))))

(defun format-tl-value (specification value)
  "A short readable rendering of VALUE for the printer.  Opaque byte fields
print as hex because that is how every MTProto reference quotes them."
  (case (tl-specification-kind specification)
    ((int128 int256 bytes raw)
     (if (> (length value) 16)
         (format nil "~A…[~D]" (octets:octets-hex value :end 8) (length value))
         (octets:octets-hex value)))
    ((vector) (format nil "[~D]" (length value)))
    (t (princ-to-string value))))

(defun slot-accessor-name (constructor slot)
  "The accessor CONSTRUCTOR-SLOT, interned where CONSTRUCTOR lives."
  (intern (concatenate 'string (symbol-name constructor) "-" (symbol-name slot))
          (symbol-package constructor)))

(defmacro define-tl-object (name (id &key (superclasses '(tl-object))
                                          (decode t) (encode t)
                                          documentation)
                            &body slots)
  "Define the TL constructor NAME with constructor id ID.

Each entry of SLOTS is (SLOT-NAME TYPE-SPECIFICATION &key documentation),
where TYPE-SPECIFICATION is one of the symbols READ-TL-VALUE dispatches on.
The form expands to an ordinary DEFCLASS plus ordinary methods, so a
constructor whose layout the specification cannot express -- MSG-CONTAINER,
say -- can pass :DECODE NIL or :ENCODE NIL and get a hand-written method
instead of a special case in the macro."
  (let* ((slot-descriptions
           (loop for (slot-name specification . options) in slots
                 collect (list slot-name
                               (canonical-tl-specification specification)
                               (slot-accessor-name name slot-name)
                               (getf options :documentation))))
         (reader (gensym "READER"))
         (writer (gensym "WRITER"))
         (object (gensym "OBJECT")))
    `(progn
       ;; A constructor and its accessors are the interface: exporting them
       ;; here keeps a schema from being shadowed by a parallel export list
       ;; that has to be edited in step with it.
       (export '(,name ,@(mapcar #'third slot-descriptions))
               ,(package-name (symbol-package name)))
       (defclass ,name ,superclasses
         ,(loop for (slot-name specification accessor slot-documentation)
                  in slot-descriptions
                collect `(,slot-name
                          :initarg ,(intern (symbol-name slot-name) :keyword)
                          :accessor ,accessor
                          ,@(when slot-documentation
                              `(:documentation ,slot-documentation))
                          ,@(when (eq (tl-specification-kind specification)
                                      'vector)
                              '(:initform '()))))
         ,@(when documentation `((:documentation ,documentation))))
       (defmethod tl-constructor-id ((,object ,name)) ,id)
       (defmethod tl-constructor-id ((,object (eql ',name))) ,id)
       (setf (tl-constructor-name ,id) ',name)
       ,(let ((layout `',(loop for (slot-name specification accessor)
                                 in slot-descriptions
                               collect (list slot-name specification accessor))))
          `(progn
             (defmethod tl-object-slots ((,object ,name)) ,layout)
             (defmethod tl-object-slots ((,object (eql ',name))) ,layout)))
       ,@(when decode
           `((defmethod decode-tl-body ((,object (eql ',name)) ,reader)
               (make-instance ',name
                              ,@(loop for (slot-name specification)
                                        in slot-descriptions
                                      append
                                      `(,(intern (symbol-name slot-name)
                                                 :keyword)
                                        (read-tl ',specification ,reader)))))))
       ,@(when encode
           `((defmethod encode-tl ((,object ,name) ,writer)
               (write-unsigned ,writer ,id 4)
               ,@(loop for (slot-name specification accessor)
                         in slot-descriptions
                       collect `(write-tl ',specification ,writer
                                          (,accessor ,object)))
               ,writer)))
       ',name)))
