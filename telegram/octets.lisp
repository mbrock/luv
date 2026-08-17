;;;; Octet vectors, and the two impure inputs a wire protocol cannot avoid.
;;;;
;;;; MTProto is a byte protocol, so the substrate here is a plain
;;;; SIMPLE-ARRAY of (UNSIGNED-BYTE 8): dense, specialized, and without any
;;;; object identity of its own.  The CLOS in this file is deliberately
;;;; confined to the boundary -- ENTROPY and CLOCK -- because those are the
;;;; two places where a test wants to substitute a recording for the world.

(in-package #:telegram.octets)

(deftype octet ()
  "One byte on the wire."
  '(unsigned-byte 8))

(deftype octets (&optional (length '*))
  "A dense byte vector.  Every wire-level value in this client is one of these."
  `(simple-array (unsigned-byte 8) (,length)))

(declaim (inline octetsp))
(defun octetsp (object)
  "Is OBJECT an octet vector of the exact representation this client uses?"
  (typep object 'octets))

(defun make-octets (length &key (initial-element 0))
  "A fresh octet vector of LENGTH bytes."
  (make-array length :element-type '(unsigned-byte 8)
                     :initial-element initial-element))

(defun to-octets (sequence)
  "Coerce SEQUENCE -- a list, string of char-codes, or any vector of bytes --
into the canonical octet-vector representation, without copying when it
already is one."
  (if (octetsp sequence)
      sequence
      (coerce sequence '(simple-array (unsigned-byte 8) (*)))))

(defun concatenate-octets (&rest sequences)
  "The concatenation of SEQUENCES as one octet vector."
  (let* ((length (reduce #'+ sequences :key #'length))
         (result (make-octets length))
         (position 0))
    (dolist (sequence sequences result)
      (replace result sequence :start1 position)
      (incf position (length sequence)))))

(defun octets= (left right)
  "Are LEFT and RIGHT the same bytes?  Not constant-time; MTProto's hash
comparisons are over values an attacker already knows."
  (and (= (length left) (length right))
       (not (mismatch left right))))

(defun octets-xor (left right)
  "The bytewise exclusive or of two equally long byte sequences."
  (assert (= (length left) (length right)) (left right)
          "XOR operands differ in length: ~D and ~D."
          (length left) (length right))
  (let ((result (make-octets (length left))))
    (dotimes (index (length left) result)
      (setf (aref result index) (logxor (elt left index) (elt right index))))))

(defparameter +hex-digits+ "0123456789abcdef")

(defun octets-hex (sequence &key (start 0) (end (length sequence)))
  "SEQUENCE printed as lowercase hexadecimal, the form every MTProto
reference dumps its test vectors in."
  (let ((result (make-string (* 2 (- end start)))))
    (loop for index from start below end
          for out from 0 by 2
          for byte = (elt sequence index)
          do (setf (char result out) (char +hex-digits+ (ash byte -4))
                   (char result (1+ out)) (char +hex-digits+ (logand byte 15))))
    result))

(defun hex-octets (string)
  "Parse STRING as hexadecimal, ignoring whitespace so that a vector copied
out of a specification can stay laid out the way it was published."
  (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
        (high nil))
    (loop for character across string
          for weight = (digit-char-p character 16)
          do (cond ((member character '(#\Space #\Tab #\Newline #\Return #\_)))
                   ((null weight)
                    (error "~S is not a hexadecimal digit." character))
                   ((null high) (setf high weight))
                   (t (vector-push-extend (logior (ash high 4) weight) result)
                      (setf high nil))))
    (when high
      (error "Hexadecimal string has an odd number of digits."))
    (to-octets result)))

(defun string-octets (string)
  "STRING encoded as UTF-8, which is what TL means by `string'."
  (to-octets (sb-ext:string-to-octets string :external-format :utf-8)))

(defun octets-string (sequence &key (start 0) (end (length sequence)))
  "SEQUENCE decoded as UTF-8."
  (sb-ext:octets-to-string (to-octets (subseq sequence start end))
                           :external-format :utf-8))

(defun octets-integer (sequence &key (start 0) (end (length sequence))
                                     (endian :big))
  "The unsigned integer that SEQUENCE spells in ENDIAN order."
  (ecase endian
    (:big (loop with value = 0
                for index from start below end
                do (setf value (logior (ash value 8) (elt sequence index)))
                finally (return value)))
    (:little (loop with value = 0
                   for index from (1- end) downto start
                   do (setf value (logior (ash value 8) (elt sequence index)))
                   finally (return value)))))

(defun integer-octets (integer &key length (endian :big))
  "INTEGER as bytes in ENDIAN order, left-padded to LENGTH when given.  The
padded form is how MTProto transmits a 2048-bit key whose leading byte
happens to be zero."
  (check-type integer (integer 0))
  (let* ((natural-length (max 1 (ceiling (integer-length integer) 8)))
         (length (or length natural-length)))
    (when (< length natural-length)
      (error "~D does not fit in ~D byte~:P." integer length))
    (let ((result (make-octets length)))
      (ecase endian
        (:big (loop for index from (1- length) downto 0
                    for shift from 0 by 8
                    do (setf (aref result index) (ldb (byte 8 shift) integer))))
        (:little (loop for index from 0 below length
                       for shift from 0 by 8
                       do (setf (aref result index)
                                (ldb (byte 8 shift) integer)))))
      result)))

(defparameter +base64-alphabet+
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "The standard base64 alphabet.  PEM uses no other.")

(defun base64-octets (string)
  "Decode STRING as base64, ignoring line breaks and whitespace.  This exists
so that a PEM public key can be a literal in the source rather than a file
and a dependency."
  (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
        (accumulator 0)
        (bits 0))
    (loop for character across string
          for weight = (position character +base64-alphabet+)
          do (cond ((char= character #\=) (loop-finish))
                   ((null weight)
                    (unless (member character
                                    '(#\Space #\Tab #\Newline #\Return))
                      (error "~S is not a base64 character." character)))
                   (t (setf accumulator (logior (ash accumulator 6) weight))
                      (incf bits 6)
                      (when (>= bits 8)
                        (decf bits 8)
                        (vector-push-extend
                         (ldb (byte 8 bits) accumulator) result)))))
    (to-octets result)))

;;;; Entropy
;;;;
;;;; The auth-key exchange consumes a lot of randomness: a nonce, a new
;;;; nonce, RSA padding, and a 2048-bit Diffie-Hellman secret.  Making the
;;;; source an object rather than a call to a global generator is what lets
;;;; the whole handshake replay byte-for-byte in a test.

(defclass entropy ()
  ()
  (:documentation
   "A source of unpredictable bytes.  Subclasses decide how unpredictable."))

(defgeneric random-octets (entropy count)
  (:documentation
   "COUNT fresh bytes from ENTROPY, as an octet vector."))

(defclass system-entropy (entropy)
  ((path :initarg :path :initform "/dev/urandom" :reader system-entropy-path))
  (:documentation
   "The operating system's cryptographic random source."))

(defmethod random-octets ((entropy system-entropy) count)
  (let ((result (make-octets count)))
    (with-open-file (stream (system-entropy-path entropy)
                            :element-type '(unsigned-byte 8))
      (let ((read (read-sequence result stream)))
        (unless (= read count)
          (error "~A yielded ~D of ~D requested byte~:P."
                 (system-entropy-path entropy) read count))))
    result))

(define-condition entropy-exhausted (error)
  ((entropy :initarg :entropy :reader entropy-exhausted-entropy)
   (requested :initarg :requested :reader entropy-exhausted-requested))
  (:report (lambda (condition stream)
             (format stream "Recorded entropy ran out with ~D byte~:P still ~
requested."
                     (entropy-exhausted-requested condition))))
  (:documentation
   "A REPLAYING-ENTROPY was asked for more bytes than were recorded."))

(defclass replaying-entropy (entropy)
  ((source :initarg :source :reader replaying-entropy-source
           :documentation "The recorded bytes, consumed front to back.")
   (position :initarg :position :initform 0
             :accessor replaying-entropy-position))
  (:documentation
   "Entropy that replays a fixed recording.  Handing one of these to the auth
exchange turns a handshake into a deterministic function of its inputs, which
is how the published sample handshake becomes an executable claim."))

(defmethod random-octets ((entropy replaying-entropy) count)
  (let* ((source (replaying-entropy-source entropy))
         (start (replaying-entropy-position entropy))
         (end (+ start count)))
    (when (> end (length source))
      (error 'entropy-exhausted :entropy entropy
                                :requested (- end (length source))))
    (setf (replaying-entropy-position entropy) end)
    (to-octets (subseq source start end))))

(defvar *entropy* (make-instance 'system-entropy)
  "The entropy source used when a caller does not name one.")

;;;; Clocks
;;;;
;;;; MTProto message ids encode the time, and the server rejects ones that
;;;; drift too far, so the client tracks an offset against server time.  The
;;;; clock is an object for the same reason entropy is.

(defclass clock ()
  ()
  (:documentation "A reading of wall-clock time in the Unix epoch."))

(defgeneric clock-unix-nanoseconds (clock)
  (:documentation
   "Nanoseconds since the Unix epoch, as an integer."))

(defgeneric clock-unix-time (clock)
  (:method ((clock clock))
    (floor (clock-unix-nanoseconds clock) 1000000000))
  (:documentation
   "Whole seconds since the Unix epoch."))

(defclass system-clock (clock)
  ()
  (:documentation "The host's real-time clock."))

(defconstant +unix-epoch-universal-time+
  (encode-universal-time 0 0 0 1 1 1970 0)
  "The Common Lisp universal time at which the Unix epoch begins.")

(defmethod clock-unix-nanoseconds ((clock system-clock))
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (+ (* seconds 1000000000) (* microseconds 1000))))

(defclass frozen-clock (clock)
  ((nanoseconds :initarg :nanoseconds :accessor frozen-clock-nanoseconds
                :documentation "The instant this clock keeps reporting."))
  (:documentation
   "A clock stopped at one instant, so that message ids in a test are values
rather than observations.  Advance it by writing FROZEN-CLOCK-NANOSECONDS."))

(defmethod clock-unix-nanoseconds ((clock frozen-clock))
  (frozen-clock-nanoseconds clock))

(defvar *clock* (make-instance 'system-clock)
  "The clock used when a caller does not name one.")
