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

;;;; The schema as data

(defstruct (tl-field (:constructor make-tl-field
                         (name keyword specification condition)))
  "One field of a TL constructor.  CONDITION is (FLAGS-KEYWORD . BIT) when the
field is optional, and NIL when it is always present."
  (name "" :type string)
  (keyword :field :type keyword)
  (specification nil)
  (condition nil))

(defstruct (tl-definition (:constructor make-tl-definition
                              (name keyword id function-p result-specification
                               result-name source fields)))
  "One line of a TL schema, in the form the codec reads."
  (name "" :type string)
  (keyword :constructor :type keyword)
  (id 0 :type (unsigned-byte 32))
  (function-p nil)
  (result-specification nil)
  (result-name "" :type string)
  (source "" :type string)
  (fields #() :type simple-vector))

(defmethod print-object ((definition tl-definition) stream)
  (print-unreadable-object (definition stream :type t)
    (format stream "~A#~8,'0x~:[~; (function)~]"
            (tl-definition-name definition) (tl-definition-id definition)
            (tl-definition-function-p definition))))

(defvar *tl-definitions-by-id* (make-hash-table)
  "Constructor id to definition.")

(defvar *tl-definitions-by-keyword* (make-hash-table)
  "Constructor keyword to definition.")

(defun find-tl-definition (designator &key (errorp t))
  "The definition DESIGNATOR names: a keyword, a constructor id, a TL name
string, or a record."
  (or (etypecase designator
        (tl-definition designator)
        (keyword (gethash designator *tl-definitions-by-keyword*))
        (integer (gethash designator *tl-definitions-by-id*))
        (string (gethash (tl-keyword designator) *tl-definitions-by-keyword*)))
      (when errorp
        (error 'unknown-tl-name
               :detail (format nil "~S names no TL constructor" designator)))))

(defun tl-definition-field (definition keyword &key (errorp t))
  "The field of DEFINITION called KEYWORD, and its index."
  (let ((index (position keyword (tl-definition-fields definition)
                         :key #'tl-field-keyword)))
    (cond (index (values (aref (tl-definition-fields definition) index) index))
          (errorp (error 'unknown-tl-name
                         :detail (format nil "~A has no field ~S"
                                         (tl-definition-name definition)
                                         keyword)))
          (t nil))))

(defun map-tl-definitions (function)
  "Call FUNCTION on every loaded definition."
  (maphash (lambda (id definition)
             (declare (ignore id))
             (funcall function definition))
           *tl-definitions-by-id*))

(defun find-tl-definitions (substring &key functions types)
  "Every definition whose TL name contains SUBSTRING, for finding one's way
around a schema of this size from the listener."
  (let ((found '()))
    (map-tl-definitions
     (lambda (definition)
       (when (and (search substring (tl-definition-name definition)
                          :test #'char-equal)
                  (or (not functions) (tl-definition-function-p definition))
                  (or (not types) (not (tl-definition-function-p definition))))
         (push definition found))))
    (sort found #'string< :key #'tl-definition-name)))

;;;; Records
;;;;
;;;; A decoded message.  Dense inside -- the values are a simple vector
;;;; parallel to the definition's fields -- and addressed by keyword outside.

(defstruct (tl-record (:constructor %make-tl-record (definition values))
                      (:copier nil))
  "One decoded TL value: which constructor it is, and what its fields hold."
  (definition nil :type tl-definition)
  (values #() :type simple-vector))

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

(defun read-bare-tl-vector (reader element-reader)
  "A bare TL `vector<t>': a count and that many elements, with no constructor
of its own.  Returns a simple vector -- an empty one is a real value, which
matters because an absent optional field is NIL."
  (let ((count (take-unsigned reader 4)))
    (let ((result (make-array count)))
      (dotimes (index count result)
        (setf (aref result index) (funcall element-reader reader))))))

(defun read-tl-vector (reader element-reader)
  "A boxed TL `Vector<T>', each element read by ELEMENT-READER."
  (let ((id (take-unsigned reader 4)))
    (unless (= id +vector-constructor+)
      (error 'unexpected-tl-constructor :id id
                                        :expected (list +vector-constructor+))))
  (read-bare-tl-vector reader element-reader))

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

(defun write-bare-tl-vector (writer items element-writer)
  "A bare TL `vector<t>'.  ITEMS may be any sequence."
  (write-unsigned writer (length items) 4)
  (map nil (lambda (item) (funcall element-writer writer item)) items)
  writer)

(defun write-tl-vector (writer items element-writer)
  "A boxed TL `Vector<T>', each element written by ELEMENT-WRITER."
  (write-unsigned writer +vector-constructor+ 4)
  (write-bare-tl-vector writer items element-writer))

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

(defmethod read-tl-value ((kind (eql 'bare-vector)) specification reader)
  (let ((element (second specification)))
    (read-bare-tl-vector reader (lambda (reader) (read-tl element reader)))))

(defmethod write-tl-value ((kind (eql 'bare-vector)) specification writer value)
  (let ((element (second specification)))
    (write-bare-tl-vector writer value
                          (lambda (writer item) (write-tl element writer item)))))

(defmethod read-tl-value ((kind (eql 'flags)) specification reader)
  (declare (ignore specification))
  (read-tl-int reader))

(defmethod write-tl-value ((kind (eql 'flags)) specification writer value)
  (declare (ignore specification))
  (write-tl-int writer value))

;;;; FLAG is the pseudo-type of a conditional field declared `true': it
;;;; occupies a bit in the flags word and no bytes at all.  The methods exist
;;;; so that the vocabulary is total, but DEFINE-TL-OBJECT never calls them --
;;;; it reads and writes the bit directly.

(defmethod read-tl-value ((kind (eql 'flag)) specification reader)
  (declare (ignore specification reader))
  t)

(defmethod write-tl-value ((kind (eql 'flag)) specification writer value)
  (declare (ignore specification value))
  writer)
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
   "A list of (SLOT-NAME SPECIFICATION ACCESSOR CONDITION) describing how the
constructor is laid out on the wire.  Printing, decoding, and encoding all
read this one description.")
  (:method ((designator t)) '()))

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
  "Read one boxed TL value from READER.

The class registry is consulted first and the schema table second: a handful
of MTProto constructors are classes because methods attach to them, and
everything Telegram publishes is a record because nothing does."
  (let* ((id (take-unsigned reader 4))
         (name (tl-constructor-name id)))
    (cond (name (decode-tl-body name reader))
          ((find-tl-definition id :errorp nil)
           (decode-tl-record (find-tl-definition id) reader))
          (t (error 'unknown-tl-constructor :id id)))))

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
  "Write a boxed value: an MTProto object, a schema record, or bytes someone
already encoded -- the last of which is what lets invokeWithLayer wrap a query
it did not build."
  (declare (ignore specification))
  (if (typep value '(or tl-object tl-record))
      (encode-tl value writer)
      (write-tl-raw writer value)))

;;;; Printing

(defun format-tl-value (specification value)
  "A short rendering of VALUE for the printer.  Deliberately shallow: these
objects nest deeply enough that a full print is unreadable."
  (case (tl-specification-kind specification)
    ((int128 int256 bytes raw)
     (if (> (length value) 12)
         (format nil "~A…~D" (octets:octets-hex value :end 6) (length value))
         (octets:octets-hex value)))
    ((vector bare-vector) (format nil "[~D]" (length value)))
    ((object) (cond ((typep value 'tl-object)
                     (format nil "#<~(~A~)>" (type-of value)))
                    ((tl-record-p value)
                     (format nil "#<~A>"
                             (tl-definition-name (tl-record-definition value))))
                    (t (format nil "~D bytes" (length value)))))
    ((string) (if (> (length value) 24)
                  (format nil "~S…" (subseq value 0 24))
                  (format nil "~S" value)))
    ((flags) (format nil "#b~B" value))
    (t (princ-to-string value))))

(defmethod print-object ((object tl-object) stream)
  (print-unreadable-object (object stream :type t)
    (loop for (name specification accessor) in (tl-object-slots object)
          when (and (slot-boundp object name) (slot-value object name))
            do (format stream " ~(~A~)=~A" name
                       (format-tl-value specification
                                        (funcall accessor object))))))

;;;; The definer

(defun slot-accessor-name (constructor slot)
  "The accessor CONSTRUCTOR-SLOT, interned where CONSTRUCTOR lives."
  (intern (concatenate 'string (symbol-name constructor) "-" (symbol-name slot))
          (symbol-package constructor)))

(defstruct (tl-slot (:constructor make-tl-slot
                        (name specification accessor condition documentation)))
  "One slot of a constructor, as DEFINE-TL-OBJECT understands it."
  name specification accessor condition documentation)

(defun tl-slot-flag-p (slot)
  "Does this slot occupy a bit rather than any bytes?"
  (eq 'flag (tl-specification-kind (tl-slot-specification slot))))

(defun tl-slot-flags-p (slot)
  "Is this slot the flags word itself?"
  (eq 'flags (tl-specification-kind (tl-slot-specification slot))))

(defun tl-slot-initform (slot)
  "The initform a slot gets, or NIL for none.

An optional slot defaults to NIL, which is also how absence is spelled; a
flags word defaults to zero because it is recomputed on the way out; a vector
defaults to empty."
  (cond ((tl-slot-condition slot) '(nil))
        ((tl-slot-flags-p slot) '(0))
        ((tl-slot-flag-p slot) '(nil))
        ((member (tl-specification-kind (tl-slot-specification slot))
                 '(vector bare-vector))
         '(#()))
        (t nil)))

(defmacro define-tl-object (name (id &key (superclasses '(tl-object))
                                          (decode t) (encode t)
                                          documentation)
                            &body slots)
  "Define the TL constructor NAME with constructor id ID.

Each entry of SLOTS is

    (SLOT-NAME SPECIFICATION &key flag documentation)

where SPECIFICATION is one of the symbols READ-TL-VALUE dispatches on and
FLAG, when given, is (FLAGS-SLOT . BIT): the slot is present on the wire only
when that bit is set in that flags word.  A slot whose specification is FLAG
occupies the bit and nothing else, which is what TL's conditional `true'
means.  The flags word itself is recomputed when encoding, from which
optional slots hold a value, so it never has to be maintained by hand.

The form expands to an ordinary DEFCLASS plus ordinary methods, so a
constructor whose layout this cannot express -- MSG-CONTAINER, say -- passes
:DECODE NIL or :ENCODE NIL and gets a hand-written method instead of a
special case in the macro."
  (let* ((slot-objects
           (loop for (slot-name specification . options) in slots
                 collect (make-tl-slot slot-name
                                       (canonical-tl-specification specification)
                                       (slot-accessor-name name slot-name)
                                       (getf options :flag)
                                       (getf options :documentation))))
         (layout (loop for slot in slot-objects
                       collect (list (tl-slot-name slot)
                                     (tl-slot-specification slot)
                                     (tl-slot-accessor slot)
                                     (tl-slot-condition slot))))
         (object (gensym "OBJECT")))
    `(progn
       ;; A constructor and its accessors are the interface: exporting them
       ;; here keeps a schema from being shadowed by a parallel export list
       ;; that has to be edited in step with it.
       (export '(,name ,@(mapcar #'tl-slot-accessor slot-objects))
               ,(package-name (symbol-package name)))
       (defclass ,name ,superclasses
         ,(loop for slot in slot-objects
                collect `(,(tl-slot-name slot)
                          :initarg ,(intern (symbol-name (tl-slot-name slot))
                                            :keyword)
                          :accessor ,(tl-slot-accessor slot)
                          ,@(let ((initform (tl-slot-initform slot)))
                              (when initform `(:initform ',(first initform))))
                          ,@(when (tl-slot-documentation slot)
                              `(:documentation ,(tl-slot-documentation slot)))))
         ,@(when documentation `((:documentation ,documentation))))
       (defmethod tl-constructor-id ((,object ,name)) ,id)
       (defmethod tl-constructor-id ((,object (eql ',name))) ,id)
       (setf (tl-constructor-name ,id) ',name)
       (defmethod tl-object-slots ((,object ,name)) ',layout)
       (defmethod tl-object-slots ((,object (eql ',name))) ',layout)
       ,@(when decode (list (expand-tl-decoder name slot-objects)))
       ,@(when encode (list (expand-tl-encoder name id slot-objects)))
       ',name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun tl-flag-variables (slot-objects)
    "A variable name for each flags word, keyed by slot name.  The variables
are gensyms so that a schema field called READER or WRITER cannot capture
anything."
    (loop for slot in slot-objects
          when (tl-slot-flags-p slot)
            collect (cons (tl-slot-name slot)
                          (gensym (symbol-name (tl-slot-name slot))))))

  (defun tl-flag-variable (variables slot)
    "The variable holding the flags word that SLOT's condition names."
    (let ((condition (tl-slot-condition slot)))
      (when condition
        (or (cdr (assoc (car condition) variables))
            (error "~S is conditional on ~S, which is not a flags field."
                   (tl-slot-name slot) (car condition))))))

  (defun tl-slot-decode-form (slot variables reader)
    "How one slot is read: a bit out of its flags word, a guarded read, or an
unconditional read."
    (let ((flags (tl-flag-variable variables slot)))
      (cond ((and flags (tl-slot-flag-p slot))
             `(logbitp ,(cdr (tl-slot-condition slot)) ,flags))
            (flags
             `(when (logbitp ,(cdr (tl-slot-condition slot)) ,flags)
                (read-tl ',(tl-slot-specification slot) ,reader)))
            (t `(read-tl ',(tl-slot-specification slot) ,reader)))))

  (defun expand-tl-decoder (name slot-objects)
    "A decoder that reads the slots in wire order.

LET* is doing real work here: the flags word has to be bound before the
fields it guards are read, and TL puts it first for exactly that reason."
    (let* ((reader (gensym "READER"))
           (designator (gensym "NAME"))
           (variables (tl-flag-variables slot-objects))
           (pairs (loop for slot in slot-objects
                        collect (cons slot
                                      (if (tl-slot-flags-p slot)
                                          (cdr (assoc (tl-slot-name slot)
                                                      variables))
                                          (gensym (symbol-name
                                                   (tl-slot-name slot))))))))
      `(defmethod decode-tl-body ((,designator (eql ',name)) ,reader)
         (declare (ignore ,designator) (ignorable ,reader))
         (let* ,(loop for (slot . variable) in pairs
                      collect (list variable
                                    (tl-slot-decode-form slot variables reader)))
           (make-instance ',name
                          ,@(loop for (slot . variable) in pairs
                                  append (list (intern (symbol-name
                                                        (tl-slot-name slot))
                                                       :keyword)
                                               variable)))))))

  (defun expand-tl-encoder (name id slot-objects)
    (let* ((writer (gensym "WRITER"))
           (object (gensym "OBJECT"))
           (variables (tl-flag-variables slot-objects)))
      `(defmethod encode-tl ((,object ,name) ,writer)
         (let ,(loop for (slot-name . variable) in variables
                     collect
                     `(,variable
                       (logior
                        ,@(loop for slot in slot-objects
                                for condition = (tl-slot-condition slot)
                                when (and condition
                                          (eq slot-name (car condition)))
                                  collect `(if (,(tl-slot-accessor slot)
                                                ,object)
                                               ,(ash 1 (cdr condition))
                                               0)))))
           (declare (ignorable ,@(mapcar #'cdr variables)))
           (write-tl-constructor ,writer ,id)
           ,@(loop for slot in slot-objects
                   for flags = (tl-flag-variable variables slot)
                   collect
                   (cond ((tl-slot-flags-p slot)
                          `(write-tl-int ,writer
                                         ,(cdr (assoc (tl-slot-name slot)
                                                      variables))))
                         ((tl-slot-flag-p slot) nil)
                         (flags
                          `(when (,(tl-slot-accessor slot) ,object)
                             (write-tl ',(tl-slot-specification slot) ,writer
                                       (,(tl-slot-accessor slot) ,object))))
                         (t `(write-tl ',(tl-slot-specification slot) ,writer
                                       (,(tl-slot-accessor slot) ,object)))))
           ,writer)))))

;;;; Schema records

(define-condition tl-schema-error (tl-error)
  ((detail :initarg :detail :reader tl-schema-error-detail)
   (line :initarg :line :initform nil :reader tl-schema-error-line))
  (:report (lambda (condition stream)
             (format stream "~@[Line ~D: ~]~A"
                     (tl-schema-error-line condition)
                     (tl-schema-error-detail condition))))
  (:documentation "The schema text said something this reader cannot parse."))

(define-condition unknown-tl-name (tl-schema-error)
  ()
  (:documentation "No constructor in the loaded schema goes by that name."))

;;;; Names
;;;;
;;;; TL is camelCase with dotted namespaces; Lisp is kebab-case.  The dot
;;;; survives, because keeping it makes the TL name recoverable by eye:
;;;; messages.sendMessage becomes :MESSAGES.SEND-MESSAGE.  Names are keywords
;;;; rather than symbols in a package of their own, which is what keeps the
;;;; schema from costing ten thousand exported symbols.

(defun tl-lisp-name-string (tl-name)
  "TL-NAME rewritten in Lisp's spelling."
  (with-output-to-string (out)
    (loop with length = (length tl-name)
          for index from 0 below length
          for character = (char tl-name index)
          for previous = (and (plusp index) (char tl-name (1- index)))
          for next = (and (< (1+ index) length) (char tl-name (1+ index)))
          do (cond ((char= character #\_) (write-char #\- out))
                   ((and (upper-case-p character)
                         previous
                         (or (lower-case-p previous) (digit-char-p previous)
                             (and (upper-case-p previous) next
                                  (lower-case-p next))))
                    (write-char #\- out)
                    (write-char (char-upcase character) out))
                   (t (write-char (char-upcase character) out))))))

(defun tl-keyword (tl-name)
  "The keyword naming TL-NAME."
  (intern (tl-lisp-name-string tl-name) :keyword))

(defun tl-name (record)
  "Which constructor RECORD is, as a keyword."
  (tl-definition-keyword (tl-record-definition record)))

(defun tl-record-id (record)
  (tl-definition-id (tl-record-definition record)))

(defun tl-value (record field &key (errorp t))
  "The value of FIELD in RECORD.  An optional field that did not arrive is
NIL, which is also how it is spelled going out."
  (multiple-value-bind (definition index)
      (tl-definition-field (tl-record-definition record) field :errorp errorp)
    (when definition
      (aref (tl-record-values record) index))))

(defun (setf tl-value) (value record field)
  (multiple-value-bind (definition index)
      (tl-definition-field (tl-record-definition record) field)
    (declare (ignore definition))
    (setf (aref (tl-record-values record) index) value)))

(defun tl-values (record)
  "RECORD's fields as a plist, omitting the ones that are absent."
  (loop for field across (tl-definition-fields (tl-record-definition record))
        for value across (tl-record-values record)
        unless (or (null value) (eq :flags (tl-field-keyword field)))
          append (list (tl-field-keyword field) value)))

(defun make-tl (designator &rest fields)
  "Build a TL value.

  (make-tl :messages.send-message
           :peer (make-tl :input-peer-self)
           :message \"hello\" :random-id 1)

Fields not given are NIL, which for an optional field means absent and for a
required one will be caught when it is encoded.  The flags word is computed
on the way out and never has to be given."
  (let* ((definition (find-tl-definition designator))
         (values (make-array (length (tl-definition-fields definition))
                             :initial-element nil))
         (record (%make-tl-record definition values)))
    (loop for (field value) on fields by #'cddr
          do (setf (tl-value record field) value))
    record))

(defmethod print-object ((record tl-record) stream)
  (print-unreadable-object (record stream)
    (format stream "~A" (tl-definition-name (tl-record-definition record)))
    (loop for field across (tl-definition-fields (tl-record-definition record))
          for value across (tl-record-values record)
          unless (or (null value) (eq :flags (tl-field-keyword field)))
            do (format stream " ~(~A~)=~A" (tl-field-keyword field)
                       (format-tl-value (tl-field-specification field) value)))))

(defun describe-tl (designator &optional (stream *standard-output*))
  "Print what the schema says about DESIGNATOR."
  (let ((definition (find-tl-definition designator)))
    (format stream "~&~A#~8,'0x~@[ (function returning ~A)~]~%  ~A~%"
            (tl-definition-name definition) (tl-definition-id definition)
            (and (tl-definition-function-p definition)
                 (tl-definition-result-name definition))
            (tl-definition-source definition))
    (loop for field across (tl-definition-fields definition)
          do (format stream "  ~(~24A~) ~A~@[  [optional: ~(~A~) bit ~D]~]~%"
                     (tl-field-keyword field)
                     (tl-field-specification field)
                     (car (tl-field-condition field))
                     (cdr (tl-field-condition field))))
    definition))

;;;; The record codec

(defun decode-tl-record (definition reader)
  "Read DEFINITION's fields from READER.

The flags word is read before the fields it guards -- TL puts it first for
exactly that reason -- so one left-to-right pass is enough."
  (let* ((fields (tl-definition-fields definition))
         (values (make-array (length fields) :initial-element nil))
         (flags '()))
    (loop for index from 0 below (length fields)
          for field = (aref fields index)
          for specification = (tl-field-specification field)
          for condition = (tl-field-condition field)
          do (setf (aref values index)
                   (cond ((null condition)
                          (let ((value (read-tl specification reader)))
                            (when (eq 'flags (tl-specification-kind
                                              specification))
                              (push (cons (tl-field-keyword field) value) flags))
                            value))
                         ((eq 'flag (tl-specification-kind specification))
                          (logbitp (cdr condition)
                                   (tl-condition-flags flags condition
                                                       definition)))
                         ((logbitp (cdr condition)
                                   (tl-condition-flags flags condition
                                                       definition))
                          (read-tl specification reader)))))
    (%make-tl-record definition values)))

(defun tl-condition-flags (flags condition definition)
  (or (cdr (assoc (car condition) flags))
      (error 'tl-schema-error
             :detail (format nil "~A reads ~(~A~) before it is set"
                             (tl-definition-name definition)
                             (car condition)))))

(defmethod encode-tl ((record tl-record) writer)
  (let* ((definition (tl-record-definition record))
         (fields (tl-definition-fields definition))
         (values (tl-record-values record)))
    (write-tl-constructor writer (tl-definition-id definition))
    (loop for index from 0 below (length fields)
          for field = (aref fields index)
          for specification = (tl-field-specification field)
          for condition = (tl-field-condition field)
          do (cond ((eq 'flags (tl-specification-kind specification))
                    (write-tl-int writer (compute-tl-flags record
                                                          (tl-field-keyword
                                                           field))))
                   ((eq 'flag (tl-specification-kind specification)))
                   ((null condition)
                    (write-tl specification writer (aref values index)))
                   ((aref values index)
                    (write-tl specification writer (aref values index)))))
    writer))

(defun compute-tl-flags (record flags-keyword)
  "The flags word named FLAGS-KEYWORD, recomputed from which optional fields
RECORD actually holds.  Nothing has to keep it in step by hand."
  (let ((definition (tl-record-definition record)))
    (loop with value = 0
          for field across (tl-definition-fields definition)
          for index from 0
          for condition = (tl-field-condition field)
          do (when (and condition (eq flags-keyword (car condition))
                        (aref (tl-record-values record) index))
               (setf value (logior value (ash 1 (cdr condition)))))
          finally (return value))))

