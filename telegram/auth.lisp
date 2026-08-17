;;;; The authorization-key exchange.
;;;;
;;;; Four messages, each of which is a different constructor, and each of
;;;; which decides what the client sends next.  So the exchange is a mutable
;;;; object and the step is a generic function specialized on the server's
;;;; response class: HANDLE-AUTH-RESPONSE returns the next payload to send, or
;;;; NIL when the key exists.  There is no phase CASE anywhere; the phase slot
;;;; exists to reject a response that arrives out of order, not to choose the
;;;; code that handles it.
;;;;
;;;; Nothing here reads the clock or the random device directly.  Both come
;;;; from objects on the exchange, which is what makes Telegram's published
;;;; sample handshake reproducible byte for byte in the test suite.

(in-package #:telegram)

(define-condition nonce-mismatch (mtproto-protocol-error)
  ((expected :initarg :expected :reader nonce-mismatch-expected)
   (actual :initarg :actual :reader nonce-mismatch-actual))
  (:report (lambda (condition stream)
             (format stream "Nonce mismatch: expected ~A, got ~A."
                     (octets:octets-hex (nonce-mismatch-expected condition))
                     (octets:octets-hex (nonce-mismatch-actual condition)))))
  (:documentation
   "A handshake response did not echo the nonce we sent, so it is not an
answer to our exchange."))

(define-condition dh-generation-failed (mtproto-protocol-error)
  ((outcome :initarg :outcome :reader dh-generation-failed-outcome))
  (:report (lambda (condition stream)
             (format stream "Diffie-Hellman generation ~(~A~)."
                     (dh-generation-failed-outcome condition))))
  (:documentation
   "The server did not accept the shared key.  :RETRY asks for another
attempt with a fresh new_nonce; :FAIL is terminal."))

(defclass auth-key-material ()
  ((key :initarg :key :reader auth-key-material-key)
   (server-salt :initarg :server-salt :reader auth-key-material-server-salt)
   (time-offset :initarg :time-offset :reader auth-key-material-time-offset)
   (dc-id :initarg :dc-id :initform nil :reader auth-key-material-dc-id))
  (:documentation
   "Everything a completed exchange produces: the key itself, the initial
server salt, and how far our clock is from the server's."))

(defmethod print-object ((material auth-key-material) stream)
  (print-unreadable-object (material stream :type t)
    (format stream "~A salt ~D offset ~Ds"
            (octets:octets-hex (auth-key-id (auth-key-material-key material)))
            (auth-key-material-server-salt material)
            (auth-key-material-time-offset material))))

(defclass auth-exchange ()
  ((phase :initform :idle :accessor auth-exchange-phase
          :documentation
          "Which response is expected next.  Used to reject stray messages,
never to dispatch.")
   (public-keys :initarg :public-keys :initform nil
                :accessor auth-exchange-public-keys)
   (entropy :initarg :entropy :initform octets:*entropy*
            :reader auth-exchange-entropy)
   (clock :initarg :clock :initform octets:*clock*
          :reader auth-exchange-clock)
   (dc-id :initarg :dc-id :initform 0 :reader auth-exchange-dc-id)
   (test :initarg :test :initform nil :reader auth-exchange-test-p)
   (media-only :initarg :media-only :initform nil
               :reader auth-exchange-media-only-p)
   (dc-in-inner-data :initarg :dc-in-inner-data :initform t
                     :reader auth-exchange-dc-in-inner-data-p
                     :documentation
                     "Whether to seal p_q_inner_data_dc rather than the older
p_q_inner_data.  Current servers expect the former; the published sample
handshake predates it.")
   (nonce :initform nil :accessor auth-exchange-nonce)
   (server-nonce :initform nil :accessor auth-exchange-server-nonce)
   (new-nonce :initform nil :accessor auth-exchange-new-nonce)
   (public-key :initform nil :accessor auth-exchange-public-key)
   (temporary-key :initform nil :accessor auth-exchange-temporary-key)
   (temporary-iv :initform nil :accessor auth-exchange-temporary-iv)
   (key :initform nil :accessor auth-exchange-key)
   (server-salt :initform nil :accessor auth-exchange-server-salt)
   (time-offset :initform nil :accessor auth-exchange-time-offset))
  (:documentation
   "One run of the authorization-key exchange."))

(defmethod print-object ((exchange auth-exchange) stream)
  (print-unreadable-object (exchange stream :type t)
    (format stream "~(~A~)" (auth-exchange-phase exchange))))

(defun make-auth-exchange (&key (public-keys (telegram-public-keys))
                                (entropy octets:*entropy*)
                                (clock octets:*clock*)
                                (dc-id 0) test media-only
                                (dc-in-inner-data t))
  "An exchange that will accept any of PUBLIC-KEYS, drawing randomness from
ENTROPY and time from CLOCK."
  (make-instance 'auth-exchange :public-keys public-keys :entropy entropy
                                :clock clock :dc-id dc-id
                                :test test :media-only media-only
                                :dc-in-inner-data dc-in-inner-data))

(defconstant +test-data-center-offset+ 10000
  "What a test data centre adds to its own number inside p_q_inner_data_dc.")

(defun auth-exchange-inner-data-dc (exchange)
  "The dc field of p_q_inner_data_dc, which carries more than the number: a
test data centre adds ten thousand, and a media-only connection negates.  A
server that disagrees with what it is told here refuses the transport
outright, with -444 and no message."
  (let ((value (+ (auth-exchange-dc-id exchange)
                  (if (auth-exchange-test-p exchange)
                      +test-data-center-offset+
                      0))))
    (if (auth-exchange-media-only-p exchange) (- value) value)))

(defun auth-exchange-complete-p (exchange)
  "Has EXCHANGE produced a key?"
  (eq :complete (auth-exchange-phase exchange)))

(defun auth-exchange-result (exchange)
  "The AUTH-KEY-MATERIAL a completed EXCHANGE produced."
  (unless (auth-exchange-complete-p exchange)
    (error 'mtproto-protocol-error :detail "the exchange has not completed"))
  (make-instance 'auth-key-material
                 :key (auth-exchange-key exchange)
                 :server-salt (auth-exchange-server-salt exchange)
                 :time-offset (auth-exchange-time-offset exchange)
                 :dc-id (auth-exchange-dc-id exchange)))

(defun expect-auth-phase (exchange phase)
  (unless (eq phase (auth-exchange-phase exchange))
    (error 'mtproto-protocol-error
           :detail (format nil "response arrived in phase ~(~A~), not ~(~A~)"
                           (auth-exchange-phase exchange) phase))))

(defun verify-nonce (expected actual)
  (unless (octets:octets= expected actual)
    (error 'nonce-mismatch :expected expected :actual actual))
  t)

(defun begin-auth-exchange (exchange &key nonce)
  "Start EXCHANGE and return the req_pq_multi payload to send.  A NONCE may be
supplied to replay a recorded handshake; otherwise one is drawn from the
exchange's entropy."
  (expect-auth-phase exchange :idle)
  (let ((nonce (octets:to-octets
                (or nonce (octets:random-octets
                           (auth-exchange-entropy exchange) 16)))))
    (assert (= 16 (length nonce)) (nonce) "A client nonce is 16 bytes.")
    (setf (auth-exchange-nonce exchange) nonce
          (auth-exchange-phase exchange) :awaiting-res-pq)
    (tl:encode-tl-octets (make-instance 'req-pq-multi :nonce nonce))))

(defgeneric handle-auth-response (exchange response)
  (:documentation
   "Advance EXCHANGE by one server response.  Returns the next payload to
send, or NIL when the exchange is finished."))

(defmethod handle-auth-response ((exchange auth-exchange) (response tl:tl-object))
  (error 'mtproto-protocol-error
         :detail (format nil "~S is not a handshake response"
                         (type-of response))))

(defun advance-auth-exchange (exchange body)
  "Decode BODY -- the body of one plain message -- and advance EXCHANGE."
  (handle-auth-response exchange (tl:decode-tl-octets body)))

;;;; Step one: resPQ
;;;;
;;;; Factor the semiprime, seal it together with a fresh new_nonce under one
;;;; of the server's public keys, and ask for Diffie-Hellman parameters.

(defmethod handle-auth-response ((exchange auth-exchange) (response res-pq))
  (expect-auth-phase exchange :awaiting-res-pq)
  (verify-nonce (auth-exchange-nonce exchange) (res-pq-nonce response))
  (let* ((entropy (auth-exchange-entropy exchange))
         (public-key (crypto:select-public-key
                      (auth-exchange-public-keys exchange)
                      (res-pq-server-public-key-fingerprints response)))
         (new-nonce (octets:random-octets entropy 32)))
    (multiple-value-bind (p q) (crypto:factor-pq (res-pq-pq response))
      (let* ((p-bytes (octets:integer-octets p))
             (q-bytes (octets:integer-octets q))
             (inner (tl:encode-tl-octets
                     (if (auth-exchange-dc-in-inner-data-p exchange)
                         (make-instance 'p-q-inner-data-dc
                                        :pq (res-pq-pq response)
                                        :p p-bytes :q q-bytes
                                        :nonce (auth-exchange-nonce exchange)
                                        :server-nonce (res-pq-server-nonce
                                                       response)
                                        :new-nonce new-nonce
                                        :dc (auth-exchange-inner-data-dc
                                             exchange))
                         (make-instance 'p-q-inner-data
                                        :pq (res-pq-pq response)
                                        :p p-bytes :q q-bytes
                                        :nonce (auth-exchange-nonce exchange)
                                        :server-nonce (res-pq-server-nonce
                                                       response)
                                        :new-nonce new-nonce))))
             (encrypted (crypto:rsa-encrypt
                         inner public-key
                         (octets:random-octets
                          entropy
                          (crypto:rsa-required-random-length public-key)))))
        (setf (auth-exchange-server-nonce exchange) (res-pq-server-nonce response)
              (auth-exchange-new-nonce exchange) new-nonce
              (auth-exchange-public-key exchange) public-key
              (auth-exchange-phase exchange) :awaiting-server-dh-params)
        (tl:encode-tl-octets
         (make-instance 'req-dh-params
                        :nonce (auth-exchange-nonce exchange)
                        :server-nonce (res-pq-server-nonce response)
                        :p p-bytes :q q-bytes
                        :public-key-fingerprint (crypto:public-key-fingerprint
                                                 public-key)
                        :encrypted-data encrypted))))))

;;;; The handshake's own little cipher
;;;;
;;;; Between resPQ and dh_gen the two sides share only the nonces, so the
;;;; Diffie-Hellman parameters travel under a key derived from them by SHA-1,
;;;; with a SHA-1 of the plaintext standing in for an authentication tag.

(defun temporary-aes-key-iv (server-nonce new-nonce)
  "The AES key and IV the handshake derives from its nonces."
  (let ((a (crypto:sha-1 (octets:concatenate-octets new-nonce server-nonce)))
        (b (crypto:sha-1 (octets:concatenate-octets server-nonce new-nonce)))
        (c (crypto:sha-1 (octets:concatenate-octets new-nonce new-nonce))))
    (values (octets:concatenate-octets a (subseq b 0 12))
            (octets:concatenate-octets (subseq b 12 20) c (subseq new-nonce 0 4)))))

(defun encrypt-data-with-hash (data key iv padding)
  "SHA-1, data, and padding, encrypted under IGE."
  (let ((padding-length (mod (- 16 (mod (+ 20 (length data)) 16)) 16)))
    (assert (>= (length padding) padding-length) (padding)
            "Need ~D padding byte~:P, got ~D." padding-length (length padding))
    (crypto:ige-encrypt (octets:concatenate-octets
                         (crypto:sha-1 data) data
                         (subseq padding 0 padding-length))
                        key iv)))

(defun decrypt-data-with-hash (ciphertext key iv)
  "Open an IGE block whose plaintext is a SHA-1 followed by data and up to
fifteen bytes of padding.  The padding length is not transmitted, so it is
recovered by trying each possibility against the hash."
  (let* ((plaintext (crypto:ige-decrypt ciphertext key iv))
         (digest (subseq plaintext 0 20))
         (rest (subseq plaintext 20)))
    (or (loop for padding from 0 to 15
              when (and (<= padding (length rest))
                        (let ((data (subseq rest 0 (- (length rest) padding))))
                          (and (octets:octets= digest (crypto:sha-1 data))
                               (octets:to-octets data))))
                return it)
        (error 'mtproto-protocol-error
               :detail "handshake payload failed its hash check"))))

(defun exchange-server-salt (new-nonce server-nonce)
  "The initial server salt, which both sides derive rather than transmit."
  (let ((bytes (octets:octets-xor (subseq new-nonce 0 8)
                                  (subseq server-nonce 0 8))))
    (tl:read-tl-signed-long (tl:make-tl-reader bytes))))

(defconstant +dh-safety-margin+ (ash 1 (- 2048 64))
  "Public values must sit this far inside the group, so that the shared
secret cannot be brute-forced from a small exponent.")

(defun validate-dh-parameters (dh-prime g g-a g-b)
  "Check the Diffie-Hellman values the specification requires a client to
check before deriving a key from them."
  (flet ((in-range (value low high name)
           (unless (< low value high)
             (error 'mtproto-protocol-error
                    :detail (format nil "~A is outside its safe range" name)))))
    (in-range g 1 (1- dh-prime) "g")
    (in-range g-a 1 (1- dh-prime) "g_a")
    (in-range g-b 1 (1- dh-prime) "g_b")
    (in-range g-a +dh-safety-margin+ (- dh-prime +dh-safety-margin+) "g_a")
    (in-range g-b +dh-safety-margin+ (- dh-prime +dh-safety-margin+) "g_b")
    t))

;;;; Step two: server_DH_params

(defmethod handle-auth-response ((exchange auth-exchange)
                                 (response server-dh-params-fail))
  (expect-auth-phase exchange :awaiting-server-dh-params)
  (verify-nonce (auth-exchange-nonce exchange)
                (server-dh-params-fail-nonce response))
  (verify-nonce (auth-exchange-server-nonce exchange)
                (server-dh-params-fail-server-nonce response))
  (verify-nonce (octets:to-octets
                 (subseq (crypto:sha-1 (auth-exchange-new-nonce exchange)) 4 20))
                (server-dh-params-fail-new-nonce-hash response))
  (error 'dh-generation-failed :outcome :params-fail))

(defmethod handle-auth-response ((exchange auth-exchange)
                                 (response server-dh-params-ok))
  (expect-auth-phase exchange :awaiting-server-dh-params)
  (verify-nonce (auth-exchange-nonce exchange)
                (server-dh-params-ok-nonce response))
  (verify-nonce (auth-exchange-server-nonce exchange)
                (server-dh-params-ok-server-nonce response))
  (let ((server-nonce (auth-exchange-server-nonce exchange))
        (new-nonce (auth-exchange-new-nonce exchange)))
    (multiple-value-bind (temporary-key temporary-iv)
        (temporary-aes-key-iv server-nonce new-nonce)
      (let ((inner (tl:decode-tl-octets
                    (decrypt-data-with-hash
                     (server-dh-params-ok-encrypted-answer response)
                     temporary-key temporary-iv))))
        (check-type inner server-dh-inner-data)
        (verify-nonce (auth-exchange-nonce exchange)
                      (server-dh-inner-data-nonce inner))
        (verify-nonce server-nonce (server-dh-inner-data-server-nonce inner))
        (setf (auth-exchange-temporary-key exchange) temporary-key
              (auth-exchange-temporary-iv exchange) temporary-iv)
        (build-client-dh-request exchange inner)))))

(defun build-client-dh-request (exchange inner)
  "Choose a secret exponent, derive the authorization key, and encrypt our
public value for the server."
  (let* ((entropy (auth-exchange-entropy exchange))
         (secret-bytes (octets:random-octets entropy 256))
         (dh-prime (octets:octets-integer (server-dh-inner-data-dh-prime inner)))
         (g (server-dh-inner-data-g inner))
         (g-a (octets:octets-integer (server-dh-inner-data-g-a inner)))
         (secret (octets:octets-integer secret-bytes))
         (g-b (crypto:expt-mod g secret dh-prime))
         (shared (crypto:expt-mod g-a secret dh-prime)))
    (validate-dh-parameters dh-prime g g-a g-b)
    (let* ((key (make-auth-key (octets:integer-octets shared :length 256)))
           (client-inner (tl:encode-tl-octets
                          (make-instance 'client-dh-inner-data
                                         :nonce (auth-exchange-nonce exchange)
                                         :server-nonce (auth-exchange-server-nonce
                                                        exchange)
                                         :retry-id 0
                                         :g-b (octets:integer-octets g-b))))
           (encrypted (encrypt-data-with-hash
                       client-inner
                       (auth-exchange-temporary-key exchange)
                       (auth-exchange-temporary-iv exchange)
                       (octets:random-octets entropy 16))))
      (setf (auth-exchange-key exchange) key
            (auth-exchange-server-salt exchange)
            (exchange-server-salt (auth-exchange-new-nonce exchange)
                                  (auth-exchange-server-nonce exchange))
            (auth-exchange-time-offset exchange)
            (- (server-dh-inner-data-server-time inner)
               (octets:clock-unix-time (auth-exchange-clock exchange)))
            (auth-exchange-phase exchange) :awaiting-dh-gen)
      (tl:encode-tl-octets
       (make-instance 'set-client-dh-params
                      :nonce (auth-exchange-nonce exchange)
                      :server-nonce (auth-exchange-server-nonce exchange)
                      :encrypted-data encrypted)))))

;;;; Step three: dh_gen
;;;;
;;;; All three answers carry the same shape, differing in which new_nonce_hash
;;;; they contain, so the hash number is what tells the three apart.

(defun verify-dh-generation (exchange nonce server-nonce hash number)
  (expect-auth-phase exchange :awaiting-dh-gen)
  (verify-nonce (auth-exchange-nonce exchange) nonce)
  (verify-nonce (auth-exchange-server-nonce exchange) server-nonce)
  (verify-nonce (new-nonce-hash (auth-exchange-key exchange)
                                (auth-exchange-new-nonce exchange)
                                number)
                hash))

(defmethod handle-auth-response ((exchange auth-exchange) (response dh-gen-ok))
  (verify-dh-generation exchange
                        (dh-gen-ok-nonce response)
                        (dh-gen-ok-server-nonce response)
                        (dh-gen-ok-new-nonce-hash1 response)
                        1)
  (setf (auth-exchange-phase exchange) :complete)
  nil)

(defmethod handle-auth-response ((exchange auth-exchange) (response dh-gen-retry))
  (verify-dh-generation exchange
                        (dh-gen-retry-nonce response)
                        (dh-gen-retry-server-nonce response)
                        (dh-gen-retry-new-nonce-hash2 response)
                        2)
  (error 'dh-generation-failed :outcome :retry))

(defmethod handle-auth-response ((exchange auth-exchange) (response dh-gen-fail))
  (verify-dh-generation exchange
                        (dh-gen-fail-nonce response)
                        (dh-gen-fail-server-nonce response)
                        (dh-gen-fail-new-nonce-hash3 response)
                        3)
  (error 'dh-generation-failed :outcome :fail))
