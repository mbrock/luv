;;;; The MQTT wire primitives (MQTT 5, section 1.5): octet vectors, a
;;;; reader and a writer over them, and the five data representations every
;;;; packet is built from.

(in-package #:mqtt)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(simple-array octet (*)))

(defun make-octets (length &key (initial-element 0))
  (make-array length :element-type 'octet :initial-element initial-element))

(defun to-octets (sequence)
  "SEQUENCE as a fresh simple octet vector.  A string is UTF-8 encoded."
  (etypecase sequence
    (string (sb-ext:string-to-octets sequence :external-format :utf-8))
    (octets (copy-seq sequence))
    (sequence (coerce sequence 'octets))))

(defun octets-string (octets &key (start 0) end)
  "OCTETS decoded as UTF-8."
  (sb-ext:octets-to-string octets :external-format :utf-8 :start start :end end))

(defun concatenate-octets (&rest sequences)
  (apply #'concatenate 'octets sequences))

;;;; Conditions

(define-condition mqtt-error (error) ()
  (:documentation "Anything the MQTT layers can signal."))

(define-condition malformed-packet (mqtt-error)
  ((detail :initarg :detail :initform nil :reader malformed-packet-detail))
  (:report (lambda (condition stream)
             (format stream "Malformed MQTT packet~@[: ~A~]."
                     (malformed-packet-detail condition))))
  (:documentation "Bytes that do not parse as the packet they claim to be."))

(define-condition protocol-error (mqtt-error)
  ((detail :initarg :detail :initform nil :reader protocol-error-detail))
  (:report (lambda (condition stream)
             (format stream "MQTT protocol error~@[: ~A~]."
                     (protocol-error-detail condition))))
  (:documentation "A well-formed packet that is wrong for the session's state."))

(defun malformed (control &rest arguments)
  (error 'malformed-packet :detail (apply #'format nil control arguments)))

;;;; Reader

(defstruct (wire-reader (:constructor %make-wire-reader))
  (octets (make-octets 0) :type octets)
  (position 0 :type fixnum)
  (end 0 :type fixnum))

(defun make-wire-reader (octets &key (start 0) (end (length octets)))
  (%make-wire-reader :octets (if (typep octets 'octets) octets (to-octets octets))
                     :position start :end end))

(defun wire-reader-remaining (reader)
  (- (wire-reader-end reader) (wire-reader-position reader)))

(defun wire-reader-exhausted-p (reader)
  (zerop (wire-reader-remaining reader)))

(defun read-octet (reader)
  (when (wire-reader-exhausted-p reader)
    (malformed "read past the end of the packet"))
  (prog1 (aref (wire-reader-octets reader) (wire-reader-position reader))
    (incf (wire-reader-position reader))))

(defun read-octet-vector (reader length)
  "The next LENGTH raw octets."
  (when (< (wire-reader-remaining reader) length)
    (malformed "~D octets wanted, ~D left" length (wire-reader-remaining reader)))
  (let ((start (wire-reader-position reader)))
    (incf (wire-reader-position reader) length)
    (subseq (wire-reader-octets reader) start (+ start length))))

(defun read-two-byte-integer (reader)
  (logior (ash (read-octet reader) 8) (read-octet reader)))

(defun read-four-byte-integer (reader)
  (logior (ash (read-two-byte-integer reader) 16) (read-two-byte-integer reader)))

(defun read-variable-byte-integer (reader)
  "Section 1.5.5: seven bits per octet, low group first, high bit continues."
  (loop with value = 0
        for multiplier = 1 then (* multiplier 128)
        for octet = (read-octet reader)
        do (incf value (* (logand octet #x7f) multiplier))
           (when (> multiplier (* 128 128 128))
             (malformed "variable byte integer longer than four octets"))
        while (logbitp 7 octet)
        finally (return value)))

(defun read-binary-data (reader)
  "Section 1.5.6: two-byte length, then that many octets."
  (read-octet-vector reader (read-two-byte-integer reader)))

(defun read-utf8-string (reader)
  "Section 1.5.4: a length-prefixed UTF-8 string."
  (let ((octets (read-binary-data reader)))
    (handler-case (octets-string octets)
      (error () (malformed "string is not well-formed UTF-8")))))

(defun read-utf8-string-pair (reader)
  (cons (read-utf8-string reader) (read-utf8-string reader)))

;;;; Writer

(defstruct (wire-writer (:constructor %make-wire-writer))
  (buffer (make-array 64 :element-type 'octet :adjustable t :fill-pointer 0)))

(defun make-wire-writer () (%make-wire-writer))

(defun wire-writer-octets (writer)
  "Everything written so far, as a simple octet vector."
  (coerce (wire-writer-buffer writer) 'octets))

(defun wire-writer-length (writer)
  (fill-pointer (wire-writer-buffer writer)))

(defmacro with-wire-writer ((writer) &body body)
  "Run BODY with WRITER bound to a fresh writer and return its octets."
  `(let ((,writer (make-wire-writer)))
     ,@body
     (wire-writer-octets ,writer)))

(defun write-octet (writer octet)
  (vector-push-extend octet (wire-writer-buffer writer))
  octet)

(defun write-octet-vector (writer octets)
  (loop for octet across octets do (write-octet writer octet))
  octets)

(defun write-two-byte-integer (writer value)
  (unless (typep value '(unsigned-byte 16))
    (malformed "~A does not fit in two bytes" value))
  (write-octet writer (ldb (byte 8 8) value))
  (write-octet writer (ldb (byte 8 0) value))
  value)

(defun write-four-byte-integer (writer value)
  (unless (typep value '(unsigned-byte 32))
    (malformed "~A does not fit in four bytes" value))
  (write-two-byte-integer writer (ldb (byte 16 16) value))
  (write-two-byte-integer writer (ldb (byte 16 0) value))
  value)

(defconstant +max-variable-byte-integer+ 268435455)

(defun write-variable-byte-integer (writer value)
  (unless (<= 0 value +max-variable-byte-integer+)
    (malformed "~A does not fit in a variable byte integer" value))
  (loop do (multiple-value-bind (quotient remainder) (floor value 128)
             (setf value quotient)
             (write-octet writer (if (plusp quotient)
                                     (logior remainder #x80)
                                     remainder)))
        while (plusp value))
  value)

(defun write-binary-data (writer octets)
  (let ((octets (to-octets octets)))
    (write-two-byte-integer writer (length octets))
    (write-octet-vector writer octets)))

(defun write-utf8-string (writer string)
  (write-binary-data writer (to-octets string))
  string)

(defun write-utf8-string-pair (writer pair)
  (write-utf8-string writer (car pair))
  (write-utf8-string writer (cdr pair))
  pair)
