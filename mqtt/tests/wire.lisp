;;;; Claims about the wire primitives and the packet codec.

(in-package #:mqtt.tests)

(deftest variable-byte-integers-match-section-1-5-5
  (flet ((vbi (value)
           (unhex (mqtt:with-wire-writer (writer)
                    (mqtt:write-variable-byte-integer writer value))))
         (unvbi (string)
           (mqtt:read-variable-byte-integer (mqtt:make-wire-reader (hex string)))))
    (ok (equal "00" (vbi 0)))
    (ok (equal "7f" (vbi 127)))
    (ok (equal "8001" (vbi 128)))
    (ok (equal "ff7f" (vbi 16383)))
    (ok (equal "808001" (vbi 16384)))
    (ok (equal "ffffff7f" (vbi 268435455)))
    (ok (= 321 (unvbi "c102")))
    (ok (= 268435455 (unvbi "ffffff7f")))
    (ok (signals (unvbi "ffffffff7f") 'mqtt:malformed-packet))
    (ok (signals (mqtt:with-wire-writer (writer)
                   (mqtt:write-variable-byte-integer writer 268435456))
                 'mqtt:malformed-packet))))

(deftest strings-and-integers
  (ok (equal "00044d515454"
             (unhex (mqtt:with-wire-writer (writer)
                      (mqtt:write-utf8-string writer "MQTT")))))
  (ok (equal "MQTT" (mqtt:read-utf8-string (mqtt:make-wire-reader (hex "00044d515454")))))
  (ok (equal "0102" (unhex (mqtt:with-wire-writer (writer)
                             (mqtt:write-two-byte-integer writer #x0102)))))
  (ok (equal "01020304" (unhex (mqtt:with-wire-writer (writer)
                                 (mqtt:write-four-byte-integer writer #x01020304)))))
  (ok (= #x01020304 (mqtt:read-four-byte-integer (mqtt:make-wire-reader (hex "01020304")))))
  (testing "reading past the end is malformed, not an array error"
    (ok (signals (mqtt:read-two-byte-integer (mqtt:make-wire-reader (hex "01")))
                 'mqtt:malformed-packet))))

(deftest properties-round-trip
  (let ((properties '((:session-expiry-interval . 3600)
                      (:user-property . ("k" . "v"))
                      (:user-property . ("k" . "w"))
                      (:receive-maximum . 20)
                      (:correlation-data . #(1 2 3)))))
    (let* ((octets (mqtt:with-wire-writer (writer)
                     (mqtt:write-properties writer properties)))
           (back (mqtt:read-properties (mqtt:make-wire-reader octets))))
      (ok (= 3600 (mqtt:property back :session-expiry-interval)))
      (ok (= 20 (mqtt:property back :receive-maximum)))
      (ok (equalp #(1 2 3) (mqtt:property back :correlation-data)))
      (ok (equal '(("k" . "v") ("k" . "w")) (mqtt:user-properties back)))
      (ok (eq :none (mqtt:property back :reason-string :none)))))
  (testing "an empty property list is one zero octet"
    (ok (equal "00" (unhex (mqtt:with-wire-writer (writer)
                             (mqtt:write-properties writer '()))))))
  (testing "an unknown identifier is malformed"
    (ok (signals (mqtt:read-properties (mqtt:make-wire-reader (hex "02 7f 00")))
                 'mqtt:malformed-packet))))

(deftest connect-encodes-as-section-3-1
  (testing "the smallest CONNECT: no id, clean start, 60 second keep alive"
    (ok (equal "100d00044d5154540502003c000000"
               (encoded (make-instance 'mqtt:connect-packet)))))
  (testing "credentials, a will, and properties all round trip"
    (let* ((packet (make-instance
                    'mqtt:connect-packet
                    :client-id "luv" :clean-start nil :keep-alive 30
                    :username "mikael" :password (mqtt:to-octets "secret")
                    :properties '((:session-expiry-interval . 120))
                    :will (make-instance 'mqtt:will :topic "luv/presence"
                                                    :payload "gone" :qos 1 :retain t
                                                    :properties '((:will-delay-interval . 5)))))
           (back (round-trip packet)))
      (ok (equal "luv" (mqtt:connect-client-id back)))
      (ok (not (mqtt:connect-clean-start-p back)))
      (ok (= 30 (mqtt:connect-keep-alive back)))
      (ok (equal "mikael" (mqtt:connect-username back)))
      (ok (equal "secret" (mqtt:octets-string (mqtt:connect-password back))))
      (ok (= 120 (mqtt:packet-property back :session-expiry-interval)))
      (let ((will (mqtt:connect-will back)))
        (ok (equal "luv/presence" (mqtt:will-topic will)))
        (ok (equal "gone" (mqtt:octets-string (mqtt:will-payload will))))
        (ok (= 1 (mqtt:will-qos will)))
        (ok (mqtt:will-retain-p will))
        (ok (= 5 (mqtt:property (mqtt:will-properties will) :will-delay-interval)))))))

(deftest connack-decodes
  (let ((packet (decoded "20 03 00 00 00")))
    (ok (typep packet 'mqtt:connack-packet))
    (ok (not (mqtt:connack-session-present-p packet)))
    (ok (eq :success (mqtt:packet-reason packet))))
  (let ((packet (decoded "20 09 01 87 06 1f 00 03 6e 6f 70")))
    (ok (mqtt:connack-session-present-p packet))
    (ok (eq :not-authorized (mqtt:packet-reason packet)))
    (ok (mqtt:reason-code-error-p (mqtt:packet-reason-code packet)))
    (ok (equal "nop" (mqtt:packet-property packet :reason-string)))))

(deftest publish-encodes-as-section-3-3
  (testing "QoS 0 has no packet id"
    (ok (equal "30080003612f62006869"
               (encoded (make-instance 'mqtt:publish-packet :topic "a/b" :payload "hi")))))
  (testing "QoS 1 with retain and dup carries its id after the topic"
    (ok (equal "3b0a0003612f62002a006869"
               (encoded (make-instance 'mqtt:publish-packet :topic "a/b" :payload "hi"
                                                            :qos 1 :retain t :dup t
                                                            :packet-id 42)))))
  (testing "and QoS above 0 without an id is refused"
    (ok (signals (encoded (make-instance 'mqtt:publish-packet :topic "a" :qos 1))
                 'mqtt:malformed-packet)))
  (let ((packet (decoded "3b0a0003612f62002a006869")))
    (ok (equal "a/b" (mqtt:publish-topic packet)))
    (ok (equal "hi" (mqtt:publish-payload-string packet)))
    (ok (= 1 (mqtt:publish-qos packet)))
    (ok (= 42 (mqtt:packet-id packet)))
    (ok (mqtt:publish-retain-p packet))
    (ok (mqtt:publish-dup-p packet)))
  (testing "QoS 3 is malformed"
    (ok (signals (decoded "36 05 0001 61 00 00") 'mqtt:malformed-packet)))
  (testing "an empty payload is fine"
    (ok (equalp #() (mqtt:publish-payload (decoded "30 04 0001 61 00"))))))

(deftest acknowledgements-use-the-short-forms
  (testing "success with no properties is just the packet id"
    (ok (equal "40020001" (encoded (make-instance 'mqtt:puback-packet :packet-id 1))))
    (ok (equal "50020001" (encoded (make-instance 'mqtt:pubrec-packet :packet-id 1))))
    (ok (equal "62020001" (encoded (make-instance 'mqtt:pubrel-packet :packet-id 1))))
    (ok (equal "70020001" (encoded (make-instance 'mqtt:pubcomp-packet :packet-id 1)))))
  (testing "a reason code adds one octet"
    (ok (equal "4003000110"
               (encoded (make-instance 'mqtt:puback-packet :packet-id 1
                                                           :reason-code #x10)))))
  (testing "and each short form decodes"
    (ok (eq :success (mqtt:packet-reason (decoded "40020001"))))
    (ok (eq :no-matching-subscribers (mqtt:packet-reason (decoded "4003000110"))))
    (ok (equal "x" (mqtt:packet-property (decoded "4008 0001 10 04 1f 0001 78")
                                         :reason-string)))))

(deftest subscribe-encodes-as-section-3-8
  (ok (equal "82090001000003612f6201"
             (encoded (make-instance 'mqtt:subscribe-packet
                                     :packet-id 1
                                     :subscriptions (list (mqtt:subscription '("a/b" :qos 1)))))))
  (testing "options pack no-local, retain-as-published, retain-handling"
    (let ((back (round-trip (make-instance
                             'mqtt:subscribe-packet :packet-id 7
                             :subscriptions (list (mqtt:subscription
                                                   '("x/#" :qos 2 :no-local t
                                                     :retain-as-published t
                                                     :retain-handling 2))
                                                  (mqtt:subscription "y"))))))
      (ok (= 7 (mqtt:packet-id back)))
      (destructuring-bind (first second) (mqtt:subscribe-subscriptions back)
        (ok (equal "x/#" (mqtt:subscription-topic-filter first)))
        (ok (= 2 (mqtt:subscription-qos first)))
        (ok (mqtt:subscription-no-local-p first))
        (ok (mqtt:subscription-retain-as-published-p first))
        (ok (= 2 (mqtt:subscription-retain-handling first)))
        (ok (equal "y" (mqtt:subscription-topic-filter second)))
        (ok (= 0 (mqtt:subscription-qos second))))))
  (testing "SUBACK lists one reason code per filter"
    (let ((packet (decoded "90 06 0007 00 00 02 8f")))
      (ok (= 7 (mqtt:packet-id packet)))
      (ok (equal '(0 2 #x8f) (mqtt:packet-reason-codes packet)))))
  (testing "UNSUBSCRIBE and UNSUBACK"
    (ok (equal "a20900010000016100017a"
               (encoded (make-instance 'mqtt:unsubscribe-packet :packet-id 1
                                                                :topic-filters '("a" "z")))))
    (ok (equal '("a" "z") (mqtt:unsubscribe-topic-filters (decoded "a20900010000016100017a"))))
    (ok (equal '(0 #x11) (mqtt:packet-reason-codes (decoded "b0 05 0001 00 00 11"))))))

(deftest short-packets
  (ok (equal "c000" (encoded (make-instance 'mqtt:pingreq-packet))))
  (ok (equal "d000" (encoded (make-instance 'mqtt:pingresp-packet))))
  (ok (typep (decoded "d000") 'mqtt:pingresp-packet))
  (ok (equal "e000" (encoded (make-instance 'mqtt:disconnect-packet))))
  (ok (equal "e0018e" (encoded (make-instance 'mqtt:disconnect-packet
                                              :reason-code (mqtt:reason-code :session-taken-over)))))
  (ok (eq :session-taken-over (mqtt:packet-reason (decoded "e0018e"))))
  (ok (eq :success (mqtt:packet-reason (decoded "e000"))))
  (ok (typep (decoded "f0 01 18") 'mqtt:auth-packet))
  (testing "trailing octets are malformed"
    (ok (signals (decoded "c00100") 'mqtt:malformed-packet)))
  (testing "an unknown type is malformed"
    (ok (signals (decoded "0000") 'mqtt:malformed-packet))))

(deftest the-decoder-cuts-packets-out-of-a-stream
  (let ((decoder (mqtt:make-packet-decoder))
        (stream (hex "d000 30080003612f62006869 c000")))
    (testing "fed one octet at a time, each packet appears exactly when complete"
      (let ((packets '()))
        (loop for octet across stream
              do (setf packets (append packets (mqtt:feed-decoder decoder (vector octet)))))
        (ok (= 3 (length packets)))
        (ok (typep (first packets) 'mqtt:pingresp-packet))
        (ok (equal "hi" (mqtt:publish-payload-string (second packets))))
        (ok (typep (third packets) 'mqtt:pingreq-packet))))
    (testing "and fed all at once, all three come back in order"
      (let ((packets (mqtt:feed-decoder (mqtt:make-packet-decoder) stream)))
        (ok (= 3 (length packets)))
        (ok (typep (third packets) 'mqtt:pingreq-packet))))
    (testing "a partial remaining length is not-yet, not malformed"
      (let ((decoder (mqtt:make-packet-decoder)))
        (ok (null (mqtt:feed-decoder decoder (hex "30 80"))))
        (ok (null (mqtt:feed-decoder decoder (hex "80"))))))))
