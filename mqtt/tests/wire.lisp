;;;; Claims about the wire primitives and the packet codec.

(in-package #:mqtt.tests)

(define-test variable-byte-integers-match-section-1-5-5
  (flet ((vbi (value)
           (unhex (mqtt:with-wire-writer (writer)
                    (mqtt:write-variable-byte-integer writer value))))
         (unvbi (string)
           (mqtt:read-variable-byte-integer (mqtt:make-wire-reader (hex string)))))
    (true (equal "00" (vbi 0)))
    (true (equal "7f" (vbi 127)))
    (true (equal "8001" (vbi 128)))
    (true (equal "ff7f" (vbi 16383)))
    (true (equal "808001" (vbi 16384)))
    (true (equal "ffffff7f" (vbi 268435455)))
    (true (= 321 (unvbi "c102")))
    (true (= 268435455 (unvbi "ffffff7f")))
    (fail (unvbi "ffffffff7f") 'mqtt:malformed-packet)
    (fail (mqtt:with-wire-writer (writer)
            (mqtt:write-variable-byte-integer writer 268435456))
          'mqtt:malformed-packet)))

(define-test strings-and-integers
  (true (equal "00044d515454"
               (unhex (mqtt:with-wire-writer (writer)
                        (mqtt:write-utf8-string writer "MQTT")))))
  (true (equal "MQTT" (mqtt:read-utf8-string (mqtt:make-wire-reader (hex "00044d515454")))))
  (true (equal "0102" (unhex (mqtt:with-wire-writer (writer)
                               (mqtt:write-two-byte-integer writer #x0102)))))
  (true (equal "01020304" (unhex (mqtt:with-wire-writer (writer)
                                   (mqtt:write-four-byte-integer writer #x01020304)))))
  (true (= #x01020304 (mqtt:read-four-byte-integer (mqtt:make-wire-reader (hex "01020304")))))
  (group (context "reading past the end is malformed, not an array error")
    (fail (mqtt:read-two-byte-integer (mqtt:make-wire-reader (hex "01")))
          'mqtt:malformed-packet)))

(define-test properties-round-trip
  (let ((properties '((:session-expiry-interval . 3600)
                      (:user-property . ("k" . "v"))
                      (:user-property . ("k" . "w"))
                      (:receive-maximum . 20)
                      (:correlation-data . #(1 2 3)))))
    (let* ((octets (mqtt:with-wire-writer (writer)
                     (mqtt:write-properties writer properties)))
           (back (mqtt:read-properties (mqtt:make-wire-reader octets))))
      (true (= 3600 (mqtt:property back :session-expiry-interval)))
      (true (= 20 (mqtt:property back :receive-maximum)))
      (true (equalp #(1 2 3) (mqtt:property back :correlation-data)))
      (true (equal '(("k" . "v") ("k" . "w")) (mqtt:user-properties back)))
      (true (eq :none (mqtt:property back :reason-string :none)))))
  (group (context "an empty property list is one zero octet")
    (true (equal "00" (unhex (mqtt:with-wire-writer (writer)
                               (mqtt:write-properties writer '()))))))
  (group (context "an unknown identifier is malformed")
    (fail (mqtt:read-properties (mqtt:make-wire-reader (hex "02 7f 00")))
          'mqtt:malformed-packet)))

(define-test connect-encodes-as-section-3-1
  (group (context "the smallest CONNECT: no id, clean start, 60 second keep alive")
    (true (equal "100d00044d5154540502003c000000"
                 (encoded (make-instance 'mqtt:connect-packet)))))
  (group (context "credentials, a will, and properties all round trip")
    (let* ((packet (make-instance
                    'mqtt:connect-packet
                    :client-id "luv" :clean-start nil :keep-alive 30
                    :username "mikael" :password (mqtt:to-octets "secret")
                    :properties '((:session-expiry-interval . 120))
                    :will (make-instance 'mqtt:will :topic "luv/presence"
                                                    :payload "gone" :qos 1 :retain t
                                                    :properties '((:will-delay-interval . 5)))))
           (back (round-trip packet)))
      (true (equal "luv" (mqtt:connect-client-id back)))
      (true (not (mqtt:connect-clean-start-p back)))
      (true (= 30 (mqtt:connect-keep-alive back)))
      (true (equal "mikael" (mqtt:connect-username back)))
      (true (equal "secret" (mqtt:octets-string (mqtt:connect-password back))))
      (true (= 120 (mqtt:packet-property back :session-expiry-interval)))
      (let ((will (mqtt:connect-will back)))
        (true (equal "luv/presence" (mqtt:will-topic will)))
        (true (equal "gone" (mqtt:octets-string (mqtt:will-payload will))))
        (true (= 1 (mqtt:will-qos will)))
        (true (mqtt:will-retain-p will))
        (true (= 5 (mqtt:property (mqtt:will-properties will) :will-delay-interval)))))))

(define-test connack-decodes
  (let ((packet (decoded "20 03 00 00 00")))
    (true (typep packet 'mqtt:connack-packet))
    (true (not (mqtt:connack-session-present-p packet)))
    (true (eq :success (mqtt:packet-reason packet))))
  (let ((packet (decoded "20 09 01 87 06 1f 00 03 6e 6f 70")))
    (true (mqtt:connack-session-present-p packet))
    (true (eq :not-authorized (mqtt:packet-reason packet)))
    (true (mqtt:reason-code-error-p (mqtt:packet-reason-code packet)))
    (true (equal "nop" (mqtt:packet-property packet :reason-string)))))

(define-test publish-encodes-as-section-3-3
  (group (context "QoS 0 has no packet id")
    (true (equal "30080003612f62006869"
                 (encoded (make-instance 'mqtt:publish-packet :topic "a/b" :payload "hi")))))
  (group (context "QoS 1 with retain and dup carries its id after the topic")
    (true (equal "3b0a0003612f62002a006869"
                 (encoded (make-instance 'mqtt:publish-packet :topic "a/b" :payload "hi"
                                                              :qos 1 :retain t :dup t
                                                              :packet-id 42)))))
  (group (context "and QoS above 0 without an id is refused")
    (fail (encoded (make-instance 'mqtt:publish-packet :topic "a" :qos 1))
          'mqtt:malformed-packet))
  (let ((packet (decoded "3b0a0003612f62002a006869")))
    (true (equal "a/b" (mqtt:publish-topic packet)))
    (true (equal "hi" (mqtt:publish-payload-string packet)))
    (true (= 1 (mqtt:publish-qos packet)))
    (true (= 42 (mqtt:packet-id packet)))
    (true (mqtt:publish-retain-p packet))
    (true (mqtt:publish-dup-p packet)))
  (group (context "QoS 3 is malformed")
    (fail (decoded "36 05 0001 61 00 00") 'mqtt:malformed-packet))
  (group (context "an empty payload is fine")
    (true (equalp #() (mqtt:publish-payload (decoded "30 04 0001 61 00"))))))

(define-test acknowledgements-use-the-short-forms
  (group (context "success with no properties is just the packet id")
    (true (equal "40020001" (encoded (make-instance 'mqtt:puback-packet :packet-id 1))))
    (true (equal "50020001" (encoded (make-instance 'mqtt:pubrec-packet :packet-id 1))))
    (true (equal "62020001" (encoded (make-instance 'mqtt:pubrel-packet :packet-id 1))))
    (true (equal "70020001" (encoded (make-instance 'mqtt:pubcomp-packet :packet-id 1)))))
  (group (context "a reason code adds one octet")
    (true (equal "4003000110"
                 (encoded (make-instance 'mqtt:puback-packet :packet-id 1
                                                             :reason-code #x10)))))
  (group (context "and each short form decodes")
    (true (eq :success (mqtt:packet-reason (decoded "40020001"))))
    (true (eq :no-matching-subscribers (mqtt:packet-reason (decoded "4003000110"))))
    (true (equal "x" (mqtt:packet-property (decoded "4008 0001 10 04 1f 0001 78")
                                           :reason-string)))))

(define-test subscribe-encodes-as-section-3-8
  (true (equal "82090001000003612f6201"
               (encoded (make-instance 'mqtt:subscribe-packet
                                       :packet-id 1
                                       :subscriptions (list (mqtt:subscription '("a/b" :qos 1)))))))
  (group (context "options pack no-local, retain-as-published, retain-handling")
    (let ((back (round-trip (make-instance
                             'mqtt:subscribe-packet :packet-id 7
                             :subscriptions (list (mqtt:subscription
                                                   '("x/#" :qos 2 :no-local t
                                                     :retain-as-published t
                                                     :retain-handling 2))
                                                  (mqtt:subscription "y"))))))
      (true (= 7 (mqtt:packet-id back)))
      (destructuring-bind (first second) (mqtt:subscribe-subscriptions back)
        (true (equal "x/#" (mqtt:subscription-topic-filter first)))
        (true (= 2 (mqtt:subscription-qos first)))
        (true (mqtt:subscription-no-local-p first))
        (true (mqtt:subscription-retain-as-published-p first))
        (true (= 2 (mqtt:subscription-retain-handling first)))
        (true (equal "y" (mqtt:subscription-topic-filter second)))
        (true (= 0 (mqtt:subscription-qos second))))))
  (group (context "SUBACK lists one reason code per filter")
    (let ((packet (decoded "90 06 0007 00 00 02 8f")))
      (true (= 7 (mqtt:packet-id packet)))
      (true (equal '(0 2 #x8f) (mqtt:packet-reason-codes packet)))))
  (group (context "UNSUBSCRIBE and UNSUBACK")
    (true (equal "a20900010000016100017a"
                 (encoded (make-instance 'mqtt:unsubscribe-packet :packet-id 1
                                                                  :topic-filters '("a" "z")))))
    (true (equal '("a" "z") (mqtt:unsubscribe-topic-filters (decoded "a20900010000016100017a"))))
    (true (equal '(0 #x11) (mqtt:packet-reason-codes (decoded "b0 05 0001 00 00 11"))))))

(define-test short-packets
  (true (equal "c000" (encoded (make-instance 'mqtt:pingreq-packet))))
  (true (equal "d000" (encoded (make-instance 'mqtt:pingresp-packet))))
  (true (typep (decoded "d000") 'mqtt:pingresp-packet))
  (true (equal "e000" (encoded (make-instance 'mqtt:disconnect-packet))))
  (true (equal "e0018e" (encoded (make-instance 'mqtt:disconnect-packet
                                                :reason-code (mqtt:reason-code :session-taken-over)))))
  (true (eq :session-taken-over (mqtt:packet-reason (decoded "e0018e"))))
  (true (eq :success (mqtt:packet-reason (decoded "e000"))))
  (true (typep (decoded "f0 01 18") 'mqtt:auth-packet))
  (group (context "trailing octets are malformed")
    (fail (decoded "c00100") 'mqtt:malformed-packet))
  (group (context "an unknown type is malformed")
    (fail (decoded "0000") 'mqtt:malformed-packet)))

(define-test the-decoder-cuts-packets-out-of-a-stream
  (let ((decoder (mqtt:make-packet-decoder))
        (stream (hex "d000 30080003612f62006869 c000")))
    (group (context "fed one octet at a time, each packet appears exactly when complete")
      (let ((packets '()))
        (loop for octet across stream
              do (setf packets (append packets (mqtt:feed-decoder decoder (vector octet)))))
        (true (= 3 (length packets)))
        (true (typep (first packets) 'mqtt:pingresp-packet))
        (true (equal "hi" (mqtt:publish-payload-string (second packets))))
        (true (typep (third packets) 'mqtt:pingreq-packet))))
    (group (context "and fed all at once, all three come back in order")
      (let ((packets (mqtt:feed-decoder (mqtt:make-packet-decoder) stream)))
        (true (= 3 (length packets)))
        (true (typep (third packets) 'mqtt:pingreq-packet))))
    (group (context "a partial remaining length is not-yet, not malformed")
      (let ((decoder (mqtt:make-packet-decoder)))
        (true (null (mqtt:feed-decoder decoder (hex "30 80"))))
        (true (null (mqtt:feed-decoder decoder (hex "80"))))))))
