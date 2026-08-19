;;;; Packages for the MQTT client.
;;;;
;;;;   mqtt       the wire codec, the packet vocabulary, and the session
;;;;   mqtt.net   the one place that owns a socket
;;;;
;;;; MQTT is sans-IO: a session is fed octets and drained of octets, and
;;;; nothing in it reads a clock or a socket.

(defpackage #:mqtt
  (:use #:cl)
  (:documentation
   "An MQTT 5 client core.  Packets are classes that encode and decode
themselves; a session turns inbound packets into events and requests into
outbound packets, and leaves the socket to somebody else.")
  (:export
   ;; wire
   #:octet #:octets #:make-octets #:to-octets #:octets-string
   #:concatenate-octets
   #:mqtt-error #:malformed-packet #:malformed-packet-detail
   #:protocol-error #:protocol-error-detail
   #:wire-reader #:make-wire-reader #:wire-reader-remaining
   #:wire-reader-exhausted-p
   #:read-octet #:read-octet-vector #:read-two-byte-integer
   #:read-four-byte-integer #:read-variable-byte-integer #:read-binary-data
   #:read-utf8-string #:read-utf8-string-pair
   #:wire-writer #:make-wire-writer #:wire-writer-octets #:with-wire-writer
   #:write-octet #:write-octet-vector #:write-two-byte-integer
   #:write-four-byte-integer #:write-variable-byte-integer #:write-binary-data
   #:write-utf8-string #:write-utf8-string-pair
   ;; properties and reason codes
   #:read-properties #:write-properties #:property #:user-properties
   #:reason-code #:reason-code-name #:reason-code-error-p
   ;; packets
   #:packet #:packet-properties #:packet-property #:packet-type-code
   #:packet-flags #:encode-packet-body #:decode-packet-body
   #:identified-packet #:packet-id
   #:reasoned-packet #:packet-reason-code #:packet-reason
   #:will #:will-topic #:will-payload #:will-qos #:will-retain-p #:will-properties
   #:connect-packet #:connect-client-id #:connect-clean-start-p
   #:connect-keep-alive #:connect-username #:connect-password #:connect-will
   #:connack-packet #:connack-session-present-p
   #:publish-packet #:publish-topic #:publish-payload #:publish-payload-string
   #:publish-qos #:publish-retain-p #:publish-dup-p
   #:acknowledgement-packet #:puback-packet #:pubrec-packet #:pubrel-packet
   #:pubcomp-packet
   #:subscription #:subscription-topic-filter #:subscription-qos
   #:subscription-no-local-p #:subscription-retain-as-published-p
   #:subscription-retain-handling
   #:subscribe-packet #:subscribe-subscriptions
   #:reason-list-packet #:packet-reason-codes #:suback-packet #:unsuback-packet
   #:unsubscribe-packet #:unsubscribe-topic-filters
   #:pingreq-packet #:pingresp-packet
   #:reason-only-packet #:disconnect-packet #:auth-packet
   #:encode-packet #:decode-packet
   #:packet-decoder #:make-packet-decoder #:feed-decoder
   ;; session
   #:pending-request #:pending-request-packet #:pending-request-response
   #:pending-request-done-p #:pending-request-id #:pending-request-reason-codes
   #:pending-request-failed-p
   #:mqtt-session #:make-mqtt-session #:session-client-id #:session-keep-alive
   #:session-clean-start-p #:session-username #:session-password #:session-will
   #:session-connect-properties #:session-state #:session-connack
   #:session-connected-p #:session-server-property
   #:drain-session-outbox #:drain-session-events
   #:session-begin #:session-publish #:session-subscribe #:session-unsubscribe
   #:session-ping #:session-disconnect #:session-receive #:handle-packet))

(defpackage #:mqtt.net
  (:use #:cl)
  (:documentation "A TCP connection driving an MQTT session.")
  (:export #:connection-closed #:connection-timeout #:connection-refused
           #:connection-refused-connack #:request-refused #:request-refused-pending
           #:mqtt-connection #:connection-host #:connection-port
           #:connection-session #:connection-read-timeout
           #:open-mqtt-connection #:close-mqtt-connection #:with-mqtt-connection
           #:connection-flush #:pump-connection #:pump-connection-until
           #:connection-await
           #:publish #:subscribe #:unsubscribe #:ping #:next-message
           #:*lobby-host* #:*lobby-port* #:*lobby-service-name*
           #:lobby-host #:discover-lobby-host #:tailnet-magic-dns-suffix
           #:lobby-available-p #:open-lobby-connection
           #:*lobby-store-prefix* #:lobby-put #:lobby-get #:lobby-value
           #:lobby-delete #:lobby-keys))
