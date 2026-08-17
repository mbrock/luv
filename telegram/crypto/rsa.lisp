;;;; Telegram's server RSA public keys, and the two ways it pads for them.
;;;;
;;;; MTProto uses raw textbook RSA with a padding scheme of its own, and it
;;;; has had two.  The original wraps a SHA-1 of the payload and random filler
;;;; into a 255-byte block; the current one -- "RSA_PAD" -- reverses the
;;;; padded data, appends a SHA-256 keyed by a temporary AES key, encrypts
;;;; that under IGE, and retries with an incremented key until the result is
;;;; numerically below the modulus.
;;;;
;;;; Those are two behaviours of the same kind of thing, chosen by which key
;;;; the server offered, so they are two classes and one generic function
;;;; rather than a flag and a branch.

(in-package #:telegram.crypto)

(define-condition crypto-error (error)
  ()
  (:documentation "Something went wrong below the protocol layer."))

(define-condition unknown-public-key (crypto-error)
  ((fingerprints :initarg :fingerprints
                 :reader unknown-public-key-fingerprints))
  (:report (lambda (condition stream)
             (format stream "The server offered no public key we hold: ~
~{#x~16,'0X~^, ~}."
                     (unknown-public-key-fingerprints condition))))
  (:documentation
   "None of the fingerprints in a resPQ names a key this client bundles."))

(defclass public-key ()
  ((modulus :initarg :modulus :reader public-key-modulus)
   (exponent :initarg :exponent :reader public-key-exponent)
   (fingerprint :reader public-key-fingerprint
                :documentation
                "The low 64 bits, little-endian, of the SHA-1 of the key's TL
serialization.  This is the name the server calls the key by."))
  (:documentation
   "One of Telegram's server RSA public keys.  Subclasses differ only in how
they pad."))

(defclass legacy-public-key (public-key)
  ()
  (:documentation
   "A key used with the original SHA-1 padding, for payloads up to 235 bytes."))

(defclass padded-public-key (public-key)
  ()
  (:documentation
   "A key used with RSA_PAD, the current scheme, for payloads up to 144 bytes."))

(defmethod initialize-instance :after ((key public-key) &key)
  (setf (slot-value key 'fingerprint)
        (let ((digest (sha-1 (tl:with-tl-writer (writer)
                               (tl:write-tl-bytes
                                writer
                                (octets:integer-octets
                                 (public-key-modulus key)))
                               (tl:write-tl-bytes
                                writer
                                (octets:integer-octets
                                 (public-key-exponent key)))))))
          (octets:octets-integer digest :start 12 :end 20 :endian :little))))

(defmethod print-object ((key public-key) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~D-bit #x~16,'0X"
            (integer-length (public-key-modulus key))
            (public-key-fingerprint key))))

(defun make-public-key (modulus exponent &key (mode :padded))
  "A public key from MODULUS and EXPONENT, either of which may be given as an
integer or as big-endian bytes.  MODE is :PADDED or :LEGACY."
  (flet ((as-integer (value)
           (if (integerp value) value (octets:octets-integer value))))
    (make-instance (ecase mode
                     (:padded 'padded-public-key)
                     (:legacy 'legacy-public-key))
                   :modulus (as-integer modulus)
                   :exponent (as-integer exponent))))

(defun select-public-key (keys fingerprints)
  "The first key in KEYS named by FINGERPRINTS, preferring the server's own
ordering.  Signals UNKNOWN-PUBLIC-KEY when we hold none of them."
  (or (some (lambda (fingerprint)
              (find fingerprint keys :key #'public-key-fingerprint))
            fingerprints)
      (error 'unknown-public-key :fingerprints (coerce fingerprints 'list))))

;;;; PEM and DER
;;;;
;;;; Just enough ASN.1 to read an RSAPublicKey, so that a bundled key can be
;;;; a readable literal in the source instead of an opaque pair of bignums.

(defun read-der-element (octets position)
  "Read one DER tag-length-value at POSITION.  Returns the tag, the bounds of
the contents, and the position just past the element."
  (let* ((tag (aref octets position))
         (length-byte (aref octets (1+ position)))
         (position (+ position 2))
         (length length-byte))
    (when (logbitp 7 length-byte)
      (let ((count (logand length-byte #x7F)))
        (setf length (octets:octets-integer octets :start position
                                                   :end (+ position count))
              position (+ position count))))
    (values tag position (+ position length) (+ position length))))

(defun parse-rsa-public-key-der (octets &key (start 0))
  "Parse a DER RSAPublicKey, or a SubjectPublicKeyInfo wrapping one, and
return the modulus and exponent."
  (multiple-value-bind (tag contents end) (read-der-element octets start)
    (declare (ignore end))
    (assert (= tag #x30) () "Expected a DER SEQUENCE, got tag #x~2,'0X." tag)
    (multiple-value-bind (first-tag first-contents first-end first-next)
        (read-der-element octets contents)
      (declare (ignore first-contents first-end))
      (cond ((= first-tag #x02)
             ;; RSAPublicKey ::= SEQUENCE { modulus INTEGER, exponent INTEGER }
             (multiple-value-bind (tag modulus-start modulus-end next)
                 (read-der-element octets contents)
               (declare (ignore tag))
               (multiple-value-bind (tag exponent-start exponent-end)
                   (read-der-element octets next)
                 (declare (ignore tag))
                 (values (octets:octets-integer octets :start modulus-start
                                                       :end modulus-end)
                         (octets:octets-integer octets :start exponent-start
                                                       :end exponent-end)))))
            ((= first-tag #x30)
             ;; SubjectPublicKeyInfo: skip the algorithm identifier, then
             ;; parse the RSAPublicKey inside the BIT STRING.
             (multiple-value-bind (tag bits-start) (read-der-element octets
                                                                     first-next)
               (assert (= tag #x03) () "Expected a DER BIT STRING.")
               ;; The first content byte counts unused trailing bits.
               (parse-rsa-public-key-der octets :start (1+ bits-start))))
            (t (error "Unrecognized DER public key structure."))))))

(defun pem-bodies (pem)
  "The base64 bodies of every PEM block in PEM, in order."
  (let ((bodies '())
        (current nil))
    (with-input-from-string (stream pem)
      (loop for line = (read-line stream nil nil)
            while line
            do (let ((line (string-trim '(#\Space #\Tab #\Return) line)))
                 (cond ((and (>= (length line) 5)
                             (string= "-----" line :end2 5)
                             (search "BEGIN" line))
                        (setf current (make-string-output-stream)))
                       ((and (>= (length line) 5)
                             (string= "-----" line :end2 5)
                             (search "END" line))
                        (when current
                          (push (get-output-stream-string current) bodies)
                          (setf current nil)))
                       (current (write-string line current))))))
    (nreverse bodies)))

(defun public-keys-from-pem (pem &key (mode :padded))
  "Every RSA public key in the PEM text."
  (loop for body in (pem-bodies pem)
        collect (multiple-value-bind (modulus exponent)
                    (parse-rsa-public-key-der (octets:base64-octets body))
                  (make-public-key modulus exponent :mode mode))))

(defun public-key-from-pem (pem &key (mode :padded))
  "The first RSA public key in the PEM text."
  (or (first (public-keys-from-pem pem :mode mode))
      (error "No public key found in the given PEM text.")))

;;;; Encryption

(defconstant +rsa-block-length+ 256
  "Telegram's server keys are all 2048-bit.")

(defconstant +legacy-payload-limit+ 235)
(defconstant +padded-payload-limit+ 144)
(defconstant +padded-data-length+ 192
  "RSA_PAD pads the payload out to this length before reversing it.")

(defgeneric rsa-required-random-length (key)
  (:documentation
   "How many random bytes RSA-ENCRYPT will consume for KEY.  The caller
supplies them so that a handshake can be replayed exactly."))

(defmethod rsa-required-random-length ((key legacy-public-key))
  (+ +legacy-payload-limit+ 20))

(defmethod rsa-required-random-length ((key padded-public-key))
  (+ +padded-data-length+ 32))

(defgeneric rsa-encrypt (data key random)
  (:documentation
   "Encrypt DATA under KEY, consuming RANDOM for whatever padding the scheme
needs, and return the 256-byte block to put on the wire."))

(defun rsa-encrypt-block (block key)
  "Raw modular exponentiation of BLOCK under KEY."
  (octets:integer-octets (expt-mod (octets:octets-integer block)
                                   (public-key-exponent key)
                                   (public-key-modulus key))
                         :length +rsa-block-length+))

(defmethod rsa-encrypt (data (key legacy-public-key) random)
  (assert (<= (length data) +legacy-payload-limit+) (data)
          "The legacy RSA scheme carries at most ~D bytes, got ~D."
          +legacy-payload-limit+ (length data))
  (let ((buffer (octets:to-octets
                 (subseq random 0 (1- +rsa-block-length+)))))
    (replace buffer (sha-1 data))
    (replace buffer data :start1 20)
    (rsa-encrypt-block buffer key)))

(defun increment-big-endian (octets)
  "OCTETS as a big-endian integer plus one, wrapping, at the same width."
  (let ((result (copy-seq octets)))
    (loop for index from (1- (length result)) downto 0
          do (setf (aref result index) (mod (1+ (aref result index)) 256))
          while (zerop (aref result index)))
    result))

(defun reverse-octets (octets)
  (octets:to-octets (reverse octets)))

(defmethod rsa-encrypt (data (key padded-public-key) random)
  (assert (<= (length data) +padded-payload-limit+) (data)
          "RSA_PAD carries at most ~D bytes, got ~D."
          +padded-payload-limit+ (length data))
  (let* ((data (octets:to-octets data))
         (padded (octets:concatenate-octets
                  data (subseq random 0 (- +padded-data-length+ (length data)))))
         (reversed (reverse-octets padded))
         (zero-iv (octets:make-octets 32)))
    (loop for temporary-key = (octets:to-octets
                               (subseq random +padded-data-length+
                                       (+ +padded-data-length+ 32)))
            then (increment-big-endian temporary-key)
          for encrypted = (ige-encrypt
                           (octets:concatenate-octets
                            reversed
                            (sha-256 (octets:concatenate-octets
                                      temporary-key padded)))
                           temporary-key zero-iv)
          for candidate = (octets:concatenate-octets
                           (octets:octets-xor temporary-key
                                              (sha-256 encrypted))
                           encrypted)
          when (< (octets:octets-integer candidate) (public-key-modulus key))
            return (rsa-encrypt-block candidate key))))
