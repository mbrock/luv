;;;; Octets, the conditions of a strict codec, and the limits it checks.
;;;;
;;;; Integers are big-endian.  Reals are IEEE doubles and must be finite.
;;;; Strings are UTF-8 behind a two-octet length.  A reader never reads past
;;;; its end; it signals MALFORMED-MESSAGE instead of an array error.

(in-package #:luvcraft.wire)

(deftype octet () '(unsigned-byte 8))
(deftype octets () '(simple-array octet (*)))

(defun make-octets (length)
  (make-array length :element-type 'octet :initial-element 0))

(defconstant +protocol-version+ 2
  "2: a welcome's placements carry their provenance.")
(defconstant +world-tag-length+ 16
  "A world tag is the 16-octet MD5 of recipe and vocabulary (#RELI34).")
(defconstant +header-length+ 35
  "Magic 4, version 2, type 1, world tag 16, session 8, body length 4.")
(defconstant +max-datagram-length+ 60000
  "A larger unreliable message travels on the stream instead.")
(defconstant +default-port+ 47475
  "The host's TCP listener and UDP socket share this port.")
(defconstant +default-announce-port+ 47474
  "Hosts broadcast beacons to this UDP port.")

(sb-ext:defglobal +magic+
    (coerce (map 'vector #'char-code "LUVW") '(simple-array (unsigned-byte 8) (*)))
  "The four octets every frame starts with.")

;;;; Conditions

(define-condition wire-error (error) ()
  (:documentation "Anything the wire codec signals."))

(define-condition malformed-message (wire-error)
  ((detail :initarg :detail :initform nil :reader malformed-message-detail))
  (:report (lambda (condition stream)
             (format stream "Malformed Luvcraft wire message~@[: ~A~]."
                     (malformed-message-detail condition))))
  (:documentation "Octets that are not exactly what an encoder would write."))

(define-condition unsupported-version (malformed-message)
  ((version :initarg :version :reader unsupported-version-version))
  (:report (lambda (condition stream)
             (format stream "The peer speaks Luvcraft wire version ~D; ~
                             this image speaks ~D."
                     (unsupported-version-version condition)
                     +protocol-version+)))
  (:documentation "A well-framed message from another protocol version."))

(define-condition unencodable-value (wire-error)
  ((detail :initarg :detail :initform nil :reader unencodable-value-detail))
  (:report (lambda (condition stream)
             (format stream "Cannot encode for the Luvcraft wire~@[: ~A~]."
                     (unencodable-value-detail condition))))
  (:documentation "A value that no strict decoder would accept back."))

(defvar *encoding* nil
  "True while encoding, so a failed check is the sender's fault.")

(defun refuse (control &rest arguments)
  "Signal the condition for a value that fails a check: UNENCODABLE-VALUE
while encoding, MALFORMED-MESSAGE while decoding."
  (error (if *encoding* 'unencodable-value 'malformed-message)
         :detail (apply #'format nil control arguments)))

;;;; Limits

(defstruct (wire-limits (:copier copy-wire-limits))
  "What a decoder accepts beyond what the types say.  An endpoint adopts the
vocabulary size of the welcome it sends or receives."
  (vocabulary-size 65536 :type (integer 0 #.(expt 2 32)))
  (domain-low '(-16777216 -16777216 -16777216) :type list)
  (domain-high '(16777216 16777216 16777216) :type list)
  (max-speed 1024d0 :type real)
  (max-frame-length (* 32 1024 1024) :type (integer 64)))

(defvar *default-wire-limits* (make-wire-limits))

(defvar *limits* *default-wire-limits*
  "The limits in force for the current encode or decode.")

;;;; Reader

(defstruct (wire-reader (:constructor make-wire-reader (octets position end)))
  (octets (make-octets 0) :type octets)
  (position 0 :type fixnum)
  (end 0 :type fixnum))

(defun reader-remaining (reader)
  (- (wire-reader-end reader) (wire-reader-position reader)))

(defun read-octet-vector (reader length)
  (when (> length (reader-remaining reader))
    (refuse "~D octets wanted, ~D left" length (reader-remaining reader)))
  (let ((start (wire-reader-position reader)))
    (incf (wire-reader-position reader) length)
    (subseq (wire-reader-octets reader) start (+ start length))))

(defun read-unsigned (reader octets)
  (when (> octets (reader-remaining reader))
    (refuse "~D octets wanted, ~D left" octets (reader-remaining reader)))
  (let ((vector (wire-reader-octets reader))
        (start (wire-reader-position reader))
        (value 0))
    (dotimes (index octets)
      (setf value (logior (ash value 8) (aref vector (+ start index)))))
    (incf (wire-reader-position reader) octets)
    value))

(defun read-signed (reader octets)
  (let ((value (read-unsigned reader octets))
        (bits (* 8 octets)))
    (if (logbitp (1- bits) value) (- value (ash 1 bits)) value)))

(defun read-finite (reader)
  (let ((bits (read-unsigned reader 8)))
    (when (= (ldb (byte 11 52) bits) #x7ff)
      (refuse "a non-finite number"))
    (let ((high (ldb (byte 32 32) bits)))
      (sb-kernel:make-double-float
       (if (logbitp 31 high) (- high (ash 1 32)) high)
       (ldb (byte 32 0) bits)))))

(defun read-string (reader)
  (let ((octets (read-octet-vector reader (read-unsigned reader 2))))
    (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
      (error () (refuse "a string that is not well-formed UTF-8")))))

;;;; Writer

(defun make-wire-writer ()
  (make-array 64 :element-type 'octet :adjustable t :fill-pointer 0))

(defmacro with-wire-writer ((writer) &body body)
  `(let ((,writer (make-wire-writer)))
     ,@body
     (coerce ,writer 'octets)))

(defun write-octet-vector (writer octets)
  (loop for octet across octets do (vector-push-extend octet writer)))

(defun write-unsigned (writer octets value)
  (unless (typep value `(integer 0 (,(ash 1 (* 8 octets)))))
    (refuse "~S is not an unsigned ~D-bit integer" value (* 8 octets)))
  (loop for shift from (* 8 (1- octets)) downto 0 by 8
        do (vector-push-extend (ldb (byte 8 shift) value) writer)))

(defun write-signed (writer octets value)
  (let ((bits (* 8 octets)))
    (unless (typep value `(signed-byte ,bits))
      (refuse "~S is not a signed ~D-bit integer" value bits))
    (write-unsigned writer octets (ldb (byte bits 0) value))))

(defun finite (value)
  "VALUE as a finite double, or a refusal."
  (let ((double (and (realp value)
                     (not (and (floatp value)
                               (or (sb-ext:float-infinity-p value)
                                   (sb-ext:float-nan-p value))))
                     (handler-case (coerce value 'double-float)
                       (error () nil)))))
    (or double (refuse "~S is not a finite real" value))))

(defun write-finite (writer value)
  (let ((double (finite value)))
    (write-unsigned writer 8
                    (logior (ash (ldb (byte 32 0)
                                      (sb-kernel:double-float-high-bits double))
                                 32)
                            (sb-kernel:double-float-low-bits double)))))

(defun write-string-field (writer string)
  (unless (stringp string) (refuse "~S is not a string" string))
  (let ((octets (sb-ext:string-to-octets string :external-format :utf-8)))
    (write-unsigned writer 2 (length octets))
    (write-octet-vector writer octets)))
