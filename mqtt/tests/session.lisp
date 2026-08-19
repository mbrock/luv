;;;; Claims about the session: what goes out and what is reported.

(in-package #:mqtt.tests)

(defun connected-session (&rest initargs)
  "A session that has said CONNECT and heard a plain CONNACK."
  (let ((session (apply #'mqtt:make-mqtt-session :client-id "test" initargs)))
    (mqtt:session-begin session)
    (mqtt:drain-session-outbox session)
    (mqtt:session-receive session (hex "20 03 00 00 00"))
    (mqtt:drain-session-events session)
    session))

(defun outbox-hex (session)
  (mapcar #'unhex (mqtt:drain-session-outbox session)))

(defun event-heads (session)
  (mapcar #'first (mqtt:drain-session-events session)))

(deftest connecting
  (let ((session (mqtt:make-mqtt-session :client-id "luv" :keep-alive 30)))
    (ok (eq :new (mqtt:session-state session)))
    (mqtt:session-begin session)
    (ok (eq :connecting (mqtt:session-state session)))
    (testing "CONNECT goes out"
      (let ((out (outbox-hex session)))
        (ok (= 1 (length out)))
        (ok (typep (decoded (first out)) 'mqtt:connect-packet))))
    (testing "the CONNACK's assigned id and keep alive win"
      (mqtt:session-receive session (hex "20 0c 00 00 09 12 0003 616263 13 000a"))
      (ok (mqtt:session-connected-p session))
      (ok (equal "abc" (mqtt:session-client-id session)))
      (ok (= 10 (mqtt:session-keep-alive session)))
      (ok (equal '(:connected) (event-heads session)))
      (ok (= 10 (mqtt:session-server-property session :server-keep-alive))))
    (testing "a second CONNECT is a protocol error"
      (ok (signals (mqtt:session-begin session) 'mqtt:protocol-error))))
  (testing "a refusal ends the session"
    (let ((session (mqtt:make-mqtt-session)))
      (mqtt:session-begin session)
      (mqtt:session-receive session (hex "20 03 00 87 00"))
      (ok (eq :disconnected (mqtt:session-state session)))
      (ok (equal '(:refused) (event-heads session)))
      (ok (signals (mqtt:session-publish session "a" "b") 'mqtt:protocol-error)))))

(deftest publishing-at-each-qos
  (let ((session (connected-session)))
    (testing "QoS 0 goes out and nothing is pending"
      (ok (null (mqtt:session-publish session "a/b" "hi")))
      (ok (equal '("30080003612f62006869") (outbox-hex session))))
    (testing "QoS 1 takes id 1 and settles on PUBACK"
      (let ((pending (mqtt:session-publish session "a/b" "hi" :qos 1)))
        (ok (= 1 (mqtt:pending-request-id pending)))
        (ok (equal '("320a0003612f620001006869") (outbox-hex session)))
        (mqtt:session-receive session (hex "40 02 0001"))
        (ok (mqtt:pending-request-done-p pending))
        (ok (not (mqtt:pending-request-failed-p pending)))
        (ok (equal '(0) (mqtt:pending-request-reason-codes pending)))
        (ok (equal '(:published) (event-heads session)))))
    (testing "QoS 2 takes id 2, releases on PUBREC, settles on PUBCOMP"
      (let ((pending (mqtt:session-publish session "a/b" "hi" :qos 2)))
        (ok (= 2 (mqtt:pending-request-id pending)))
        (outbox-hex session)
        (mqtt:session-receive session (hex "50 02 0002"))
        (ok (not (mqtt:pending-request-done-p pending)))
        (ok (equal '("62020002") (outbox-hex session)))
        (mqtt:session-receive session (hex "70 02 0002"))
        (ok (mqtt:pending-request-done-p pending))
        (ok (equal '(:published) (event-heads session)))))
    (testing "a failing PUBREC settles the request as failed"
      (let ((pending (mqtt:session-publish session "a/b" "hi" :qos 2)))
        (outbox-hex session)
        (mqtt:session-receive session (hex "50 03 0003 87"))
        (ok (mqtt:pending-request-done-p pending))
        (ok (mqtt:pending-request-failed-p pending))
        (ok (null (outbox-hex session)))))
    (testing "an acknowledgement for nothing is a protocol error"
      (ok (signals (mqtt:session-receive session (hex "40 02 0009")) 'mqtt:protocol-error)))))

(deftest receiving-at-each-qos
  (let ((session (connected-session)))
    (testing "QoS 0 is an event and nothing goes back"
      (mqtt:session-receive session (hex "30080003612f62006869"))
      (let ((events (mqtt:drain-session-events session)))
        (ok (equal '(:message) (mapcar #'first events)))
        (ok (equal "hi" (mqtt:publish-payload-string (second (first events))))))
      (ok (null (outbox-hex session))))
    (testing "QoS 1 is an event and a PUBACK"
      (mqtt:session-receive session (hex "320a0003612f620005006869"))
      (ok (equal '(:message) (event-heads session)))
      (ok (equal '("40020005") (outbox-hex session))))
    (testing "QoS 2 is an event, a PUBREC, then a PUBCOMP on release"
      (mqtt:session-receive session (hex "340a0003612f620006006869"))
      (ok (equal '(:message) (event-heads session)))
      (ok (equal '("50020006") (outbox-hex session)))
      (testing "and a duplicate before the release is acknowledged, not delivered"
        (mqtt:session-receive session (hex "3c0a0003612f620006006869"))
        (ok (null (event-heads session)))
        (ok (equal '("50020006") (outbox-hex session))))
      (mqtt:session-receive session (hex "62 02 0006"))
      (ok (equal '("70020006") (outbox-hex session)))
      (ok (null (event-heads session))))))

(deftest subscribing
  (let ((session (connected-session)))
    (let ((pending (mqtt:session-subscribe session "a/b" '("c/#" :qos 2))))
      (ok (equal '("820f0001000003612f62000003632f2302") (outbox-hex session)))
      (mqtt:session-receive session (hex "90 06 0001 00 00 02 00"))
      (ok (mqtt:pending-request-done-p pending))
      (ok (equal '(0 2 0) (mqtt:pending-request-reason-codes pending)))
      (ok (equal '(:subscribed) (event-heads session))))
    (testing "a refused filter marks the request failed"
      (let ((pending (mqtt:session-subscribe session "$SYS/#")))
        (outbox-hex session)
        (mqtt:session-receive session (hex "90 04 0002 00 87"))
        (ok (mqtt:pending-request-failed-p pending))
        (ok (equal (quote (:subscribed)) (event-heads session)))))
    (testing "unsubscribing"
      (let ((pending (mqtt:session-unsubscribe session "a/b")))
        (ok (equal '("a2080003000003612f62") (outbox-hex session)))
        (mqtt:session-receive session (hex "b0 04 0003 00 00"))
        (ok (mqtt:pending-request-done-p pending))
        (ok (equal '(:unsubscribed) (event-heads session)))))))

(deftest pings-and-goodbyes
  (let ((session (connected-session)))
    (mqtt:session-ping session)
    (ok (equal '("c000") (outbox-hex session)))
    (mqtt:session-receive session (hex "d000"))
    (ok (equal '(:pong) (event-heads session)))
    (testing "the server can hang up"
      (mqtt:session-receive session (hex "e0 01 8e"))
      (ok (eq :disconnected (mqtt:session-state session)))
      (let ((events (mqtt:drain-session-events session)))
        (ok (equal '(:disconnected) (mapcar #'first events)))
        (ok (eq :session-taken-over (mqtt:packet-reason (second (first events))))))))
  (testing "or we can"
    (let ((session (connected-session)))
      (mqtt:session-disconnect session :reason :disconnect-with-will-message)
      (ok (equal '("e00104") (outbox-hex session)))
      (ok (eq :disconnected (mqtt:session-state session)))))
  (testing "a client-only packet from the server is a protocol error"
    (let ((session (connected-session)))
      (ok (signals (mqtt:session-receive session (hex "c000")) 'mqtt:protocol-error)))))

(deftest packet-ids-skip-those-in-flight
  (let ((session (connected-session)))
    (setf (slot-value session 'mqtt::next-packet-id) 65535)
    (let ((first (mqtt:session-publish session "a" "b" :qos 1))
          (second (mqtt:session-publish session "a" "b" :qos 1)))
      (ok (= 65535 (mqtt:pending-request-id first)))
      (ok (= 1 (mqtt:pending-request-id second)))
      (mqtt:session-receive session (hex "40 02 0001"))
      (setf (slot-value session 'mqtt::next-packet-id) 65535)
      (testing "65535 is still busy, so the next id is 1 again"
        (ok (= 1 (mqtt:pending-request-id (mqtt:session-publish session "a" "b" :qos 1))))))))
