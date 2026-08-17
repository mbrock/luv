;;;; Packages for the Telegram/MTProto client.
;;;;
;;;; The layering is deliberately one-directional:
;;;;
;;;;   telegram.octets   dense byte vectors, hex/base64/UTF-8, entropy sources
;;;;   telegram.tl       the TL wire codec and its object vocabulary
;;;;   telegram.crypto   SHA, AES-IGE, modular arithmetic, RSA public keys
;;;;   telegram          MTProto proper: transports, envelopes, auth, sessions
;;;;   telegram.net      the one place that owns a socket
;;;;
;;;; Everything up to and including TELEGRAM is sans-IO: no sockets, no clock
;;;; reads, no randomness that was not handed in.  That is what makes the
;;;; handshake reproducible against recorded byte fixtures.

(defpackage #:telegram.octets
  (:use #:cl)
  (:documentation
   "Octet vectors and the two things a protocol needs from the outside world
before it can be pure: bytes of entropy and a reading of the clock.")
  (:export #:octet
           #:octets
           #:octetsp
           #:make-octets
           #:to-octets
           #:concatenate-octets
           #:octets=
           #:octets-xor
           #:octets-hex
           #:hex-octets
           #:string-octets
           #:octets-string
           #:octets-integer
           #:integer-octets
           #:base64-octets
           #:inflate
           #:decompress
           #:inflate-error
           #:entropy
           #:random-octets
           #:system-entropy
           #:replaying-entropy
           #:replaying-entropy-source
           #:replaying-entropy-position
           #:entropy-exhausted
           #:*entropy*
           #:clock
           #:clock-unix-time
           #:clock-unix-nanoseconds
           #:system-clock
           #:frozen-clock
           #:frozen-clock-nanoseconds
           #:*clock*))

(defpackage #:telegram.tl
  (:use #:cl)
  (:local-nicknames (#:octets #:telegram.octets))
  (:documentation
   "The TL wire codec: primitive readers and writers, and a small object
vocabulary in which each constructor is a class that knows its own id and
how to read and write itself.")
  (:export #:tl-error
           #:tl-error-datum
           #:short-tl-data
           #:trailing-tl-data
           #:unexpected-tl-constructor
           #:unknown-tl-constructor
           #:unexpected-tl-constructor-id
           #:tl-reader
           #:make-tl-reader
           #:tl-reader-octets
           #:tl-reader-position
           #:tl-reader-end
           #:tl-reader-remaining
           #:tl-reader-exhausted-p
           #:expect-tl-end
           #:tl-writer
           #:make-tl-writer
           #:tl-writer-octets
           #:tl-writer-length
           #:with-tl-writer
           #:read-tl-int
           #:read-tl-long
           #:read-tl-signed-long
           #:read-tl-int128
           #:read-tl-int256
           #:read-tl-double
           #:read-tl-bytes
           #:read-tl-string
           #:read-tl-bool
           #:read-tl-vector
           #:read-bare-tl-vector
           #:read-tl-raw
           #:write-tl-int
           #:write-tl-long
           #:write-tl-signed-long
           #:write-tl-int128
           #:write-tl-int256
           #:write-tl-double
           #:write-tl-bytes
           #:write-tl-string
           #:write-tl-bool
           #:write-tl-vector
           #:write-bare-tl-vector
           #:write-tl-raw
           #:write-tl-constructor
           #:tl-object
           #:tl-object-slots
           #:tl-constructor-id
           #:tl-constructor-name
           #:tl-constructor-class
           #:define-tl-object
           #:encode-tl
           #:encode-tl-octets
           #:decode-tl-body
           #:decode-tl-object
           #:decode-tl-octets
           #:read-tl-value
           #:write-tl-value
           #:read-tl
           #:write-tl
           #:+vector-constructor+
           ;; the schema as data, and the records it decodes into
           #:define-tl-schema
           #:install-tl-schema
           #:read-tl-schema-file
           #:parse-tl-schema
           #:tl-schema-error
           #:unknown-tl-name
           #:tl-lisp-name-string
           #:tl-keyword
           #:tl-definition
           #:tl-definition-name
           #:tl-definition-keyword
           #:tl-definition-id
           #:tl-definition-fields
           #:tl-definition-function-p
           #:tl-definition-result-specification
           #:tl-definition-result-name
           #:tl-definition-source
           #:tl-definition-field
           #:tl-field
           #:tl-field-name
           #:tl-field-keyword
           #:tl-field-specification
           #:tl-field-condition
           #:find-tl-definition
           #:find-tl-definitions
           #:map-tl-definitions
           #:describe-tl
           #:tl-record
           #:tl-record-p
           #:tl-record-definition
           #:make-tl
           #:tl-name
           #:tl-record-id
           #:tl-value
           #:tl-values
           #:decode-tl-record))

(defpackage #:telegram.crypto
  (:use #:cl)
  (:local-nicknames (#:octets #:telegram.octets)
                    (#:tl #:telegram.tl))
  (:documentation
   "The cryptography MTProto asks for, owned rather than borrowed: SHA-1 and
SHA-256, AES-256 in ECB and IGE, modular exponentiation and semiprime
factorization on Lisp bignums, and RSA public keys with both of Telegram's
padding schemes.")
  (:export #:sha-1
           #:sha-256
           #:aes-key-schedule
           #:aes-encrypt-block
           #:aes-decrypt-block
           #:ige-encrypt
           #:ige-decrypt
           #:expt-mod
           #:factor-pq
           #:public-key
           #:legacy-public-key
           #:padded-public-key
           #:make-public-key
           #:public-key-modulus
           #:public-key-exponent
           #:public-key-fingerprint
           #:public-key-from-pem
           #:public-keys-from-pem
           #:select-public-key
           #:rsa-encrypt
           #:rsa-required-random-length
           #:crypto-error
           #:unknown-public-key))

(defpackage #:telegram
  (:use #:cl)
  (:local-nicknames (#:octets #:telegram.octets)
                    (#:tl #:telegram.tl)
                    (#:crypto #:telegram.crypto))
  (:documentation
   "MTProto 2.0: transport framing, the plain and encrypted message envelopes,
the authorization-key exchange, and the post-authorization session.  Nothing
here touches a socket; a driver feeds it bytes and writes back what it
produces.")
  (:export ;; transports
           #:mtproto-transport
           #:abridged-transport
           #:intermediate-transport
           #:transport-client-prefix
           #:encode-transport-frame
           #:make-frame-decoder
           #:frame-decoder
           #:feed-transport
           #:quick-ack
           #:quick-ack-token
           ;; authorization keys and envelopes
           #:auth-key
           #:make-auth-key
           #:auth-key-data
           #:auth-key-aux-hash
           #:auth-key-id
           #:auth-key-id-integer
           #:new-nonce-hash
           #:message-key
           #:derive-aes-key-iv
           #:encrypt-padded
           #:decrypt-padded
           #:plain-message
           #:make-plain-message
           #:plain-message-message-id
           #:plain-message-body
           #:encode-plain-message
           #:decode-plain-message
           #:encrypted-packet
           #:encrypted-packet-salt
           #:encrypted-packet-session-id
           #:encrypted-packet-message-id
           #:encrypted-packet-sequence-number
           #:encrypted-packet-body
           #:encrypted-packet-padding
           #:encode-encrypted-packet
           #:decode-encrypted-packet
           #:next-message-id
           ;; The MTProto schema constructors and their accessors export
           ;; themselves from DEFINE-TL-OBJECT; see telegram/schema.lisp.
           #:mtproto-message
           #:mtproto-message-message-id
           #:mtproto-message-sequence-number
           #:mtproto-message-body
           #:unwrap-gzip
           ;; bundled server keys
           #:+telegram-production-key-pem+
           #:+telegram-sample-key-pem+
           #:telegram-public-keys
           #:public-keys-from-pem-text
           ;; the authorization-key exchange
           #:auth-exchange
           #:make-auth-exchange
           #:auth-exchange-phase
           #:auth-exchange-nonce
           #:auth-exchange-server-nonce
           #:auth-exchange-new-nonce
           #:auth-exchange-key
           #:auth-exchange-server-salt
           #:auth-exchange-time-offset
           #:auth-exchange-complete-p
           #:auth-exchange-public-keys
           #:auth-exchange-test-p
           #:auth-exchange-media-only-p
           #:auth-exchange-inner-data-dc
           #:begin-auth-exchange
           #:handle-auth-response
           #:advance-auth-exchange
           #:auth-exchange-result
           #:auth-key-material
           #:auth-key-material-key
           #:auth-key-material-server-salt
           #:auth-key-material-time-offset
           #:auth-key-material-dc-id
           ;; sessions
           #:mtproto-session
           #:make-mtproto-session
           #:session-key
           #:session-server-salt
           #:session-id
           #:session-time-offset
           #:session-last-message-id
           #:session-sent-content-messages
           #:session-pending-requests
           #:session-initialized-p
           #:session-outbox
           #:session-events
           #:drain-session-outbox
           #:session-send
           #:session-send-request
           #:session-send-ping
           #:session-receive-packet
           #:handle-session-message
           #:pending-request
           #:pending-request-message-id
           #:pending-request-name
           #:pending-request-body
           #:pending-request-result
           #:pending-request-error
           #:pending-request-done-p
           ;; conditions
           #:mtproto-error
           #:mtproto-protocol-error
           #:nonce-mismatch
           #:auth-key-id-mismatch
           #:message-key-mismatch
           #:dh-generation-failed
           #:remote-rpc-error
           #:remote-rpc-error-code
           #:remote-rpc-error-message))

(defpackage #:telegram.net
  (:use #:cl)
  (:local-nicknames (#:octets #:telegram.octets)
                    (#:tl #:telegram.tl)
                    (#:crypto #:telegram.crypto)
                    (#:mt #:telegram))
  (:documentation
   "The socket-owning edge of the client: a connection that pumps a transport
over TCP and drives the sans-IO state machines behind it.")
  (:export #:*data-centers*
           #:data-center
           #:data-center-id
           #:data-center-host
           #:data-center-port
           #:find-data-center
           #:mtproto-connection
           #:connection-host
           #:connection-port
           #:connection-transport
           #:connection-session
           #:connection-dc-id
           #:connection-test-p
           #:connection-read-timeout
           #:connection-timeout
           #:connection-timeout-seconds
           #:data-center-test-p
           #:open-mtproto-connection
           #:close-mtproto-connection
           #:with-mtproto-connection
           #:send-plain-payload
           #:read-transport-frame
           #:create-auth-key
           #:establish-session
           #:flush-session
           #:pump-connection
           #:connection-ping
           #:connection-invoke
           #:connection-closed
           #:transport-error
           #:transport-error-code))
