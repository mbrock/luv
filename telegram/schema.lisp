;;;; The MTProto schema.
;;;;
;;;; These are the constructors of mtproto.tl -- the handshake and the service
;;;; messages -- as opposed to the far larger api.tl that describes Telegram
;;;; itself.  They are written out by hand because there are about thirty of
;;;; them, they change roughly never, and having them as source makes the
;;;; handshake readable.  The Telegram API schema is a code-generation problem
;;;; and belongs to a later round.

(in-package #:telegram)

;;;; The authorization-key exchange

(tl:define-tl-object req-pq-multi (#xBE7E8EF1
                                   :documentation
                                   "Open the handshake with a client nonce.")
  (nonce int128))

(tl:define-tl-object res-pq (#x05162463
                             :documentation
                             "The server's answer: its own nonce, a semiprime
to factor, and the fingerprints of the public keys it will accept.")
  (nonce int128)
  (server-nonce int128)
  (pq bytes)
  (server-public-key-fingerprints (vector long)))

(tl:define-tl-object p-q-inner-data (#x83C95AEC
                                     :documentation
                                     "The RSA-sealed proof of work.")
  (pq bytes)
  (p bytes)
  (q bytes)
  (nonce int128)
  (server-nonce int128)
  (new-nonce int256))

(tl:define-tl-object p-q-inner-data-dc (#xA9F55F95
                                        :documentation
                                        "The proof of work, naming the data
centre the key is being created for.  Current servers expect this variant.")
  (pq bytes)
  (p bytes)
  (q bytes)
  (nonce int128)
  (server-nonce int128)
  (new-nonce int256)
  (dc int))

(tl:define-tl-object req-dh-params (#xD712E4BE
                                    :documentation
                                    "Hand back the factors and the sealed
inner data, and ask for Diffie-Hellman parameters.")
  (nonce int128)
  (server-nonce int128)
  (p bytes)
  (q bytes)
  (public-key-fingerprint long)
  (encrypted-data bytes))

(tl:define-tl-object server-dh-params-fail (#x79CB045D
                                            :documentation
                                            "The server refused the proof of
work.")
  (nonce int128)
  (server-nonce int128)
  (new-nonce-hash int128))

(tl:define-tl-object server-dh-params-ok (#xD0E8075C
                                          :documentation
                                          "The Diffie-Hellman parameters,
encrypted under a key derived from the two nonces.")
  (nonce int128)
  (server-nonce int128)
  (encrypted-answer bytes))

(tl:define-tl-object server-dh-inner-data (#xB5890DBA
                                           :documentation
                                           "What server_DH_params_ok carries
once opened: the group, the server's public value, and the server's clock.")
  (nonce int128)
  (server-nonce int128)
  (g int)
  (dh-prime bytes)
  (g-a bytes)
  (server-time int))

(tl:define-tl-object client-dh-inner-data (#x6643B654
                                           :documentation
                                           "Our half of the exchange.")
  (nonce int128)
  (server-nonce int128)
  (retry-id long)
  (g-b bytes))

(tl:define-tl-object set-client-dh-params (#xF5045F1F
                                           :documentation
                                           "Send our public value and ask the
server to confirm the shared key.")
  (nonce int128)
  (server-nonce int128)
  (encrypted-data bytes))

(tl:define-tl-object dh-gen-ok (#x3BCBF734
                                :documentation
                                "The server derived the same key.")
  (nonce int128)
  (server-nonce int128)
  (new-nonce-hash1 int128))

(tl:define-tl-object dh-gen-retry (#x46DC1FB9
                                   :documentation
                                   "The server wants the exchange retried.")
  (nonce int128)
  (server-nonce int128)
  (new-nonce-hash2 int128))

(tl:define-tl-object dh-gen-fail (#xA69DAE02
                                  :documentation
                                  "The server derived a different key.")
  (nonce int128)
  (server-nonce int128)
  (new-nonce-hash3 int128))

;;;; Service messages

(tl:define-tl-object ping (#x7ABE77EC :documentation "Keep the session warm.")
  (ping-id long))

(tl:define-tl-object pong (#x347773C5 :documentation "The answer to a ping.")
  (message-id long)
  (ping-id long))

(tl:define-tl-object msgs-ack (#x62D6B459
                               :documentation
                               "Acknowledge content messages by id.")
  (message-ids (vector long)))

(tl:define-tl-object new-session-created (#x9EC20908
                                          :documentation
                                          "The server started a new session
and is telling us the salt to use.")
  (first-message-id long)
  (unique-id long)
  (server-salt signed-long))

(tl:define-tl-object bad-server-salt (#xEDAB447B
                                      :documentation
                                      "A message was sealed with a salt the
server no longer accepts; here is the current one.")
  (bad-message-id long)
  (bad-message-sequence-number int)
  (error-code int)
  (new-server-salt signed-long))

(tl:define-tl-object bad-msg-notification (#xA7EFF811
                                           :documentation
                                           "A message was rejected for a
reason other than its salt -- usually a message id too far from server time.")
  (bad-message-id long)
  (bad-message-sequence-number int)
  (error-code int))

(tl:define-tl-object rpc-result (#xF35C6D01
                                 :documentation
                                 "The answer to one request.  The result
itself stays raw: only the layer that made the request knows how to read it.")
  (request-message-id long)
  (result raw))

(tl:define-tl-object rpc-error (#x2144CA19
                                :documentation
                                "Telegram's error envelope, as it appears
inside an rpc_result.")
  (code int)
  (message string))

(tl:define-tl-object gzip-packed (#x3072CFA1
                                  :documentation
                                  "A deflate-compressed payload.  Recognized
here, but not yet inflated.")
  (packed-data bytes))

(tl:define-tl-object msg-detailed-info (#x276D3EC6)
  (message-id long)
  (answer-message-id long)
  (byte-count int)
  (status int))

(tl:define-tl-object msg-new-detailed-info (#x809DB6DF)
  (answer-message-id long)
  (byte-count int)
  (status int))

(tl:define-tl-object msgs-state-req (#xDA69FB52)
  (message-ids (vector long)))

(tl:define-tl-object future-salt (#x0949D9DC)
  (valid-since int)
  (valid-until int)
  (salt signed-long))

(tl:define-tl-object destroy-session-ok (#xE22045FC)
  (session-id long))

(tl:define-tl-object destroy-session-none (#x62D350C9)
  (session-id long))

(tl:define-tl-object http-wait (#x9299359F)
  (max-delay int)
  (wait-after int)
  (max-wait int))

;;;; Containers
;;;;
;;;; A container's members are messages, not TL values: each has its own id,
;;;; sequence number, and length-delimited body.  That layout is outside what
;;;; the slot vocabulary describes, so the codec is written out.

(defclass mtproto-message ()
  ((message-id :initarg :message-id :reader mtproto-message-message-id)
   (sequence-number :initarg :sequence-number
                    :reader mtproto-message-sequence-number)
   (body :initarg :body :reader mtproto-message-body))
  (:documentation
   "One message inside a session: an id, a sequence number, and an
undecoded body."))

(defmethod print-object ((message mtproto-message) stream)
  (print-unreadable-object (message stream :type t)
    (format stream "~D seq ~D ~D bytes"
            (mtproto-message-message-id message)
            (mtproto-message-sequence-number message)
            (length (mtproto-message-body message)))))

(tl:define-tl-object msg-container (#x73F1F8DC
                                    :decode nil :encode nil
                                    :documentation
                                    "Several messages delivered as one.")
  (messages vector))

(defmethod tl:decode-tl-body ((name (eql 'msg-container)) reader)
  (let ((count (tl:read-tl-int reader)))
    (when (minusp count)
      (error 'mtproto-protocol-error :detail "negative container length"))
    (make-instance
     'msg-container
     :messages
     (loop repeat count
           for message-id = (tl:read-tl-long reader)
           for sequence-number = (tl:read-tl-int reader)
           for length = (tl:read-tl-int reader)
           do (unless (<= 0 length (tl:tl-reader-remaining reader))
                (error 'mtproto-protocol-error
                       :detail "implausible container member length"))
           collect (make-instance 'mtproto-message
                                  :message-id message-id
                                  :sequence-number sequence-number
                                  :body (tl:read-tl-raw reader length))))))

(defmethod tl:encode-tl ((container msg-container) writer)
  (tl:write-tl-constructor writer (tl:tl-constructor-id container))
  (tl:write-tl-int writer (length (msg-container-messages container)))
  (dolist (message (msg-container-messages container) writer)
    (tl:write-tl-long writer (mtproto-message-message-id message))
    (tl:write-tl-int writer (mtproto-message-sequence-number message))
    (tl:write-tl-int writer (length (mtproto-message-body message)))
    (tl:write-tl-raw writer (mtproto-message-body message))))

;;;; future_salts carries a bare vector of bare future_salt values, which the
;;;; slot vocabulary cannot spell either.

(tl:define-tl-object future-salts (#xAE500895 :decode nil :encode nil)
  (request-message-id long)
  (now int)
  (salts vector))

(defmethod tl:decode-tl-body ((name (eql 'future-salts)) reader)
  (let* ((request-message-id (tl:read-tl-long reader))
         (now (tl:read-tl-int reader))
         (count (tl:read-tl-int reader)))
    (make-instance 'future-salts
                   :request-message-id request-message-id
                   :now now
                   :salts (loop repeat count
                                collect (tl:decode-tl-body 'future-salt
                                                           reader)))))
