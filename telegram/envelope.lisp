;;;; Authorization keys and the two MTProto message envelopes.
;;;;
;;;; Before an authorization key exists, messages travel unencrypted with an
;;;; auth_key_id of zero.  Afterwards every message is an encrypted packet
;;;; whose plaintext carries the salt, session, id, sequence number, and body,
;;;; padded and sealed with a key derived per message and per direction.
;;;;
;;;; The direction matters: client and server derive from different halves of
;;;; the authorization key, so the same key never encrypts two messages the
;;;; same way.  That is a closed two-valued enumeration owned by the
;;;; specification, so it stays a keyword and a CASE.

(in-package #:telegram)

(define-condition mtproto-error (error)
  ()
  (:documentation "Any failure inside MTProto itself."))

(define-condition mtproto-protocol-error (mtproto-error)
  ((detail :initarg :detail :initform nil :reader mtproto-protocol-error-detail))
  (:report (lambda (condition stream)
             (format stream "MTProto protocol violation~@[: ~A~]."
                     (mtproto-protocol-error-detail condition))))
  (:documentation "The peer sent something the protocol does not allow."))

(define-condition auth-key-id-mismatch (mtproto-protocol-error)
  ()
  (:documentation
   "A packet was sealed under a different authorization key than ours."))

(define-condition message-key-mismatch (mtproto-protocol-error)
  ()
  (:documentation
   "A packet decrypted, but its message key does not match the plaintext.
This is the integrity check failing, so the plaintext is not trustworthy."))

(defclass auth-key ()
  ((data :initarg :data :reader auth-key-data
         :documentation "The 256-byte shared secret itself.")
   (aux-hash :reader auth-key-aux-hash
             :documentation "The first eight bytes of its SHA-1.")
   (id :reader auth-key-id
       :documentation "The last eight bytes of its SHA-1; the key's public name."))
  (:documentation
   "A 2048-bit MTProto authorization key together with the two short hashes
the protocol derives from it."))

(defmethod initialize-instance :after ((key auth-key) &key)
  (let ((data (octets:to-octets (auth-key-data key))))
    (assert (= 256 (length data)) (data)
            "An authorization key is 256 bytes, got ~D." (length data))
    (let ((digest (crypto:sha-1 data)))
      (setf (slot-value key 'data) data
            (slot-value key 'aux-hash) (octets:to-octets (subseq digest 0 8))
            (slot-value key 'id) (octets:to-octets (subseq digest 12 20))))))

(defmethod print-object ((key auth-key) stream)
  (print-unreadable-object (key stream :type t)
    (format stream "~A" (octets:octets-hex (auth-key-id key)))))

(defun make-auth-key (data)
  "The authorization key whose shared secret is the 256 bytes DATA."
  (make-instance 'auth-key :data data))

(defun auth-key-id-integer (key)
  "The key's id as the 64-bit little-endian integer the wire carries."
  (octets:octets-integer (auth-key-id key) :endian :little))

(defun new-nonce-hash (key new-nonce number)
  "The new_nonce_hash NUMBER for KEY, which the server returns to prove it
derived the same authorization key we did."
  (assert (= 32 (length new-nonce)) (new-nonce) "new_nonce is 32 bytes.")
  (check-type number (integer 1 3))
  (octets:to-octets
   (subseq (crypto:sha-1 (octets:concatenate-octets
                          new-nonce (list number) (auth-key-aux-hash key)))
           4 20)))

(defun sender-offset (sender)
  "Which half of the authorization key SENDER derives its keys from."
  (ecase sender
    (:client 0)
    (:server 8)))

(defun message-key (key plaintext sender)
  "The 16-byte msg_key for PLAINTEXT sent by SENDER under KEY."
  (let ((offset (sender-offset sender)))
    (octets:to-octets
     (subseq (crypto:sha-256
              (octets:concatenate-octets
               (subseq (auth-key-data key) (+ 88 offset) (+ 120 offset))
               plaintext))
             8 24))))

(defun derive-aes-key-iv (key message-key sender)
  "The AES key and IV for a message with MESSAGE-KEY from SENDER."
  (assert (= 16 (length message-key)) (message-key) "msg_key is 16 bytes.")
  (let* ((offset (sender-offset sender))
         (data (auth-key-data key))
         (a (crypto:sha-256 (octets:concatenate-octets
                             message-key
                             (subseq data offset (+ offset 36)))))
         (b (crypto:sha-256 (octets:concatenate-octets
                             (subseq data (+ 40 offset) (+ 76 offset))
                             message-key))))
    (values (octets:concatenate-octets (subseq a 0 8) (subseq b 8 24)
                                       (subseq a 24 32))
            (octets:concatenate-octets (subseq b 0 8) (subseq a 8 24)
                                       (subseq b 24 32)))))

(defun encrypt-padded (plaintext key sender)
  "Seal an already-padded PLAINTEXT: auth_key_id, msg_key, and the IGE
ciphertext, which is what actually goes into a transport frame."
  (assert (and (plusp (length plaintext))
               (zerop (mod (length plaintext) 16)))
          (plaintext)
          "An MTProto plaintext must be a nonempty multiple of 16 bytes, got ~D."
          (length plaintext))
  (let ((key-hash (message-key key plaintext sender)))
    (multiple-value-bind (aes-key aes-iv) (derive-aes-key-iv key key-hash sender)
      (octets:concatenate-octets (auth-key-id key) key-hash
                                 (crypto:ige-encrypt plaintext aes-key aes-iv)))))

(defun decrypt-padded (payload key sender)
  "Open a sealed PAYLOAD, checking both the key id and the integrity hash."
  (when (< (length payload) 24)
    (error 'mtproto-protocol-error :detail "encrypted payload is too short"))
  (let ((payload (octets:to-octets payload)))
    (unless (octets:octets= (subseq payload 0 8) (auth-key-id key))
      (error 'auth-key-id-mismatch))
    (let ((key-hash (octets:to-octets (subseq payload 8 24)))
          (ciphertext (octets:to-octets (subseq payload 24))))
      (unless (and (plusp (length ciphertext))
                   (zerop (mod (length ciphertext) 16)))
        (error 'mtproto-protocol-error
               :detail "ciphertext is not a multiple of 16 bytes"))
      (multiple-value-bind (aes-key aes-iv)
          (derive-aes-key-iv key key-hash sender)
        (let ((plaintext (crypto:ige-decrypt ciphertext aes-key aes-iv)))
          (unless (octets:octets= key-hash (message-key key plaintext sender))
            (error 'message-key-mismatch))
          plaintext)))))

;;;; The plain envelope

(defclass plain-message ()
  ((message-id :initarg :message-id :reader plain-message-message-id)
   (body :initarg :body :reader plain-message-body))
  (:documentation
   "An unencrypted MTProto message: the envelope used only until an
authorization key exists."))

(defmethod print-object ((message plain-message) stream)
  (print-unreadable-object (message stream :type t)
    (format stream "~D ~D bytes" (plain-message-message-id message)
            (length (plain-message-body message)))))

(defun make-plain-message (message-id body)
  (make-instance 'plain-message :message-id message-id :body body))

(defun encode-plain-message (message)
  "MESSAGE as the bytes of one transport frame."
  (tl:with-tl-writer (writer)
    (tl:write-tl-long writer 0)
    (tl:write-tl-long writer (plain-message-message-id message))
    (tl:write-tl-int writer (length (plain-message-body message)))
    (tl:write-tl-raw writer (plain-message-body message))))

(defun decode-plain-message (payload)
  "Decode a plain message from one transport frame."
  (let ((reader (tl:make-tl-reader payload)))
    (let ((auth-key-id (tl:read-tl-long reader)))
      (unless (zerop auth-key-id)
        (error 'mtproto-protocol-error
               :detail "expected a plain message, got an encrypted one")))
    (let* ((message-id (tl:read-tl-long reader))
           (length (tl:read-tl-int reader))
           (body (tl:read-tl-raw reader length)))
      (tl:expect-tl-end reader)
      (make-plain-message message-id body))))

;;;; The encrypted envelope

(defconstant +minimum-padding+ 12)
(defconstant +maximum-padding+ 1024)
(defconstant +encrypted-header-length+ 32
  "Salt, session id, message id, sequence number, and body length.")

(defclass encrypted-packet ()
  ((salt :initarg :salt :reader encrypted-packet-salt)
   (session-id :initarg :session-id :reader encrypted-packet-session-id)
   (message-id :initarg :message-id :reader encrypted-packet-message-id)
   (sequence-number :initarg :sequence-number
                    :reader encrypted-packet-sequence-number)
   (body :initarg :body :reader encrypted-packet-body)
   (padding :initarg :padding :initform nil
            :reader encrypted-packet-padding))
  (:documentation
   "The plaintext layout inside an encrypted MTProto message."))

(defmethod print-object ((packet encrypted-packet) stream)
  (print-unreadable-object (packet stream :type t)
    (format stream "~D seq ~D ~D bytes"
            (encrypted-packet-message-id packet)
            (encrypted-packet-sequence-number packet)
            (length (encrypted-packet-body packet)))))

(defun default-padding-length (body-length)
  "How much padding a body of BODY-LENGTH gets when the caller does not
choose.  Between 12 and 1024 bytes, and enough to reach a 16-byte boundary."
  (let ((plaintext (+ +encrypted-header-length+ body-length)))
    (+ 16 (- 16 (mod plaintext 16)))))

(defun encode-encrypted-packet-plaintext (packet)
  "The padded plaintext of PACKET, before encryption."
  (let ((body (octets:to-octets (encrypted-packet-body packet)))
        (padding (encrypted-packet-padding packet)))
    (assert (zerop (mod (length body) 4)) (packet)
            "An MTProto message body must be a whole number of words, got ~D."
            (length body))
    (assert (and (<= +minimum-padding+ (length padding) +maximum-padding+)
                 (zerop (mod (+ +encrypted-header-length+ (length body)
                                (length padding))
                             16)))
            (packet) "Padding of ~D bytes does not seal a ~D-byte body."
            (length padding) (length body))
    (tl:with-tl-writer (writer)
      (tl:write-tl-signed-long writer (encrypted-packet-salt packet))
      (tl:write-tl-signed-long writer (encrypted-packet-session-id packet))
      (tl:write-tl-long writer (encrypted-packet-message-id packet))
      (tl:write-tl-int writer (encrypted-packet-sequence-number packet))
      (tl:write-tl-int writer (length body))
      (tl:write-tl-raw writer body)
      (tl:write-tl-raw writer padding))))

(defun decode-encrypted-packet-plaintext (plaintext &key session-id)
  "Read the packet out of a decrypted PLAINTEXT, checking SESSION-ID when one
is given."
  (let* ((reader (tl:make-tl-reader plaintext))
         (salt (tl:read-tl-signed-long reader))
         (packet-session-id (tl:read-tl-signed-long reader))
         (message-id (tl:read-tl-long reader))
         (sequence-number (tl:read-tl-int reader))
         (body-length (tl:read-tl-int reader)))
    (unless (and (<= 0 body-length (tl:tl-reader-remaining reader))
                 (zerop (mod body-length 4)))
      (error 'mtproto-protocol-error :detail "implausible message body length"))
    (when (and session-id (/= session-id packet-session-id))
      (error 'mtproto-protocol-error :detail "session id mismatch"))
    (let* ((body (tl:read-tl-raw reader body-length))
           (padding (tl:read-tl-raw reader)))
      (unless (<= +minimum-padding+ (length padding) +maximum-padding+)
        (error 'mtproto-protocol-error :detail "implausible padding length"))
      (make-instance 'encrypted-packet
                     :salt salt :session-id packet-session-id
                     :message-id message-id
                     :sequence-number sequence-number
                     :body body :padding padding))))

(defun encode-encrypted-packet (packet key &key (sender :client)
                                                (entropy octets:*entropy*))
  "Seal PACKET under KEY.  When the packet carries no padding of its own, it
is drawn from ENTROPY."
  (let ((packet (if (encrypted-packet-padding packet)
                    packet
                    (make-instance
                     'encrypted-packet
                     :salt (encrypted-packet-salt packet)
                     :session-id (encrypted-packet-session-id packet)
                     :message-id (encrypted-packet-message-id packet)
                     :sequence-number (encrypted-packet-sequence-number packet)
                     :body (encrypted-packet-body packet)
                     :padding (octets:random-octets
                               entropy
                               (default-padding-length
                                (length (encrypted-packet-body packet))))))))
    (values (encrypt-padded (encode-encrypted-packet-plaintext packet)
                            key sender)
            packet)))

(defun decode-encrypted-packet (payload key &key (sender :server) session-id)
  "Open PAYLOAD under KEY and return the packet inside."
  (decode-encrypted-packet-plaintext (decrypt-padded payload key sender)
                                     :session-id session-id))

;;;; Message identifiers
;;;;
;;;; A msg_id is the time, as seconds in the high 32 bits and a fraction in
;;;; the low 32, with the bottom two bits reserved to say what kind of message
;;;; it is.  Client messages use zero there, and must strictly increase.

(defconstant +message-id-fraction-scale+ 4294967296)
(defconstant +nanoseconds-per-second+ 1000000000)

(defun next-message-id (last-message-id nanoseconds)
  "The next client message id at NANOSECONDS since the epoch, strictly after
LAST-MESSAGE-ID when one is given."
  (let* ((seconds (floor nanoseconds +nanoseconds-per-second+))
         (remainder (mod nanoseconds +nanoseconds-per-second+))
         (fraction (max 4 (floor (* remainder +message-id-fraction-scale+)
                                 +nanoseconds-per-second+)))
         (candidate (+ (* seconds +message-id-fraction-scale+) fraction))
         (aligned (- candidate (mod candidate 4))))
    (if last-message-id
        (max aligned (+ last-message-id 4))
        aligned)))
