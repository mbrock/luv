;;;; Claims about the encrypted envelope and the session that runs over it.

(in-package #:telegram.tests)

(defun test-key ()
  (mt:make-auth-key (counting-octets 256)))

(defun test-material (&key (server-salt 123) (time-offset 0))
  (make-instance 'mt:auth-key-material
                 :key (test-key) :server-salt server-salt
                 :time-offset time-offset))

(defun test-session (&key (session-id 456) (server-salt 123)
                          (unix-seconds 1693436740) (padding-byte #x33))
  (mt:make-mtproto-session (test-material :server-salt server-salt)
                           :session-id session-id
                           :entropy (constant-entropy padding-byte)
                           :clock (frozen unix-seconds)))

(defun server-packet (session body &key (message-id (ash 1693436741 32))
                                        (sequence-number 1))
  "Seal BODY as though the server had sent it to SESSION."
  (mt:encode-encrypted-packet
   (make-instance 'mt:encrypted-packet
                  :salt (mt:session-server-salt session)
                  :session-id (mt:session-id session)
                  :message-id message-id
                  :sequence-number sequence-number
                  :body body)
   (mt:session-key session)
   :sender :server
   :entropy (constant-entropy #x77)))

(defun outbox-packets (session)
  "Everything the session queued, decoded as the packets it sealed."
  (mapcar (lambda (payload)
            (mt:decode-encrypted-packet payload (mt:session-key session)
                                        :sender :client
                                        :session-id (mt:session-id session)))
          (mt:drain-session-outbox session)))

(deftest encrypted-packets-round-trip
  (let* ((key (test-key))
         (packet (make-instance 'mt:encrypted-packet
                                :salt 11 :session-id 22 :message-id 33
                                :sequence-number 1 :body (ascii "pong")))
         (sealed (mt:encode-encrypted-packet packet key :sender :client
                                                        :entropy (constant-entropy #x55)))
         (opened (mt:decode-encrypted-packet sealed key :sender :client
                                                        :session-id 22)))
    (ok (= 11 (mt:encrypted-packet-salt opened)))
    (ok (= 22 (mt:encrypted-packet-session-id opened)))
    (ok (= 33 (mt:encrypted-packet-message-id opened)))
    (ok (= 1 (mt:encrypted-packet-sequence-number opened)))
    (ok (equalp (ascii "pong") (mt:encrypted-packet-body opened)))
    (testing "padding reaches a 16-byte boundary and stays inside the legal range"
      (ok (= 28 (length (mt:encrypted-packet-padding opened))))
      (ok (zerop (mod (+ 32 4 28) 16))))
    (testing "and a packet addressed to another session is refused"
      (signals (mt:decode-encrypted-packet sealed key :sender :client
                                                      :session-id 99)
               'mt:mtproto-protocol-error))))

(deftest message-ids-are-monotone-and-aligned
  (let ((first (mt:next-message-id nil 1693436740123456789)))
    (ok (zerop (mod first 4)))
    (ok (= 1693436740 (ash first -32)))
    (testing "and never repeat even when the clock does not move"
      (let ((second (mt:next-message-id first 1693436740123456789))
            (third (mt:next-message-id nil 1693436740123456789)))
        (ok (> second first))
        (ok (= third first))))))

(deftest sequence-numbers-follow-the-content-policy
  (let ((session (test-session)))
    (testing "content messages are odd and advance the counter; service ones are not"
      (multiple-value-bind (bytes packet) (mt:session-send session (ascii "aaaa")
                                                          :content-p t)
        (declare (ignore bytes))
        (ok (= 1 (mt:encrypted-packet-sequence-number packet))))
      (multiple-value-bind (bytes packet) (mt:session-send session (ascii "bbbb"))
        (declare (ignore bytes))
        (ok (= 2 (mt:encrypted-packet-sequence-number packet))))
      (multiple-value-bind (bytes packet) (mt:session-send session (ascii "cccc")
                                                          :content-p t)
        (declare (ignore bytes))
        (ok (= 3 (mt:encrypted-packet-sequence-number packet))))
      (ok (= 2 (mt:session-sent-content-messages session))))))

(deftest rpc-results-resolve-their-request
  (let* ((session (test-session))
         (result (tl:encode-tl-octets (make-instance 'mt:pong :message-id 7
                                                              :ping-id 9))))
    (multiple-value-bind (bytes request)
        (mt:session-send-request session (ascii "aaaa") :name "updates.getState")
      (declare (ignore bytes))
      (ok (not (mt:pending-request-done-p request)))
      (let ((object (mt:session-receive-packet
                     session
                     (server-packet session
                                    (tl:encode-tl-octets
                                     (make-instance
                                      'mt:rpc-result
                                      :request-message-id
                                      (mt:pending-request-message-id request)
                                      :result result))))))
        (ok (typep object 'mt:rpc-result))
        (ok (mt:pending-request-done-p request))
        (ok (equalp result (mt:pending-request-result request)))
        (ok (null (mt:pending-request-error request)))
        (ok (zerop (hash-table-count (mt:session-pending-requests session)))))
      (testing "and the content message is acknowledged"
        (let ((packets (outbox-packets session)))
          (ok (= 1 (length packets)))
          (let ((ack (tl:decode-tl-octets
                      (mt:encrypted-packet-body (first packets)))))
            (ok (typep ack 'mt:msgs-ack))
            (ok (equalp (vector (ash 1693436741 32))
                        (mt:msgs-ack-message-ids ack)))
            (testing "as a service message, so it is even and not acknowledged back"
              (ok (evenp (mt:encrypted-packet-sequence-number
                          (first packets)))))))))))

(deftest rpc-errors-are-attached-to-their-request
  (let ((session (test-session)))
    (multiple-value-bind (bytes request)
        (mt:session-send-request session (ascii "aaaa") :name "help.getConfig")
      (declare (ignore bytes))
      (mt:session-receive-packet
       session
       (server-packet session
                      (tl:encode-tl-octets
                       (make-instance 'mt:rpc-result
                                      :request-message-id
                                      (mt:pending-request-message-id request)
                                      :result
                                      (tl:encode-tl-octets
                                       (make-instance 'mt:rpc-error
                                                      :code 303
                                                      :message "USER_MIGRATE_4"))))))
      (ok (mt:pending-request-done-p request))
      (let ((failure (mt:pending-request-error request)))
        (ok (typep failure 'mt:rpc-error))
        (ok (= 303 (mt:rpc-error-code failure)))
        (ok (equal "USER_MIGRATE_4" (mt:rpc-error-message failure)))))))

(deftest containers-are-unwrapped-into-their-members
  (let* ((session (test-session))
         (members (list (make-instance 'mt:mtproto-message
                                       :message-id (ash 1693436742 32)
                                       :sequence-number 1
                                       :body (tl:encode-tl-octets
                                              (make-instance 'mt:pong
                                                             :message-id 1
                                                             :ping-id 42)))
                        (make-instance 'mt:mtproto-message
                                       :message-id (ash 1693436743 32)
                                       :sequence-number 0
                                       :body (tl:encode-tl-octets
                                              (make-instance
                                               'mt:new-session-created
                                               :first-message-id 1
                                               :unique-id 2
                                               :server-salt 999))))))
    (mt:session-receive-packet
     session
     (server-packet session
                    (tl:encode-tl-octets
                     (make-instance 'mt:msg-container :messages members))
                    :sequence-number 0))
    (testing "each member was handled"
      (ok (find :pong (mt:session-events session) :key #'first))
      (ok (= 999 (mt:session-server-salt session))))
    (testing "and only the member with an odd sequence number is acknowledged"
      (let* ((packets (outbox-packets session))
             (ack (tl:decode-tl-octets
                   (mt:encrypted-packet-body (first packets)))))
        (ok (= 1 (length packets)))
        (ok (equalp (vector (ash 1693436742 32))
                    (mt:msgs-ack-message-ids ack)))))))

(deftest a-bad-salt-is-adopted-and-the-request-resent
  (let ((session (test-session)))
    (multiple-value-bind (bytes request)
        (mt:session-send-request session (ascii "aaaa") :name "help.getConfig")
      (declare (ignore bytes))
      (let ((message-id (mt:pending-request-message-id request)))
        (mt:session-receive-packet
         session
         (server-packet session
                        (tl:encode-tl-octets
                         (make-instance 'mt:bad-server-salt
                                        :bad-message-id message-id
                                        :bad-message-sequence-number 1
                                        :error-code 48
                                        :new-server-salt 4242))
                        :sequence-number 0))
        (ok (= 4242 (mt:session-server-salt session)))
        (testing "the resend keeps the message id and takes the new salt"
          (let ((packets (outbox-packets session)))
            (ok (= 1 (length packets)))
            (ok (= message-id (mt:encrypted-packet-message-id (first packets))))
            (ok (= 4242 (mt:encrypted-packet-salt (first packets))))
            (ok (= 1 (mt:encrypted-packet-sequence-number (first packets))))))
        (testing "and the request is still outstanding"
          (ok (not (mt:pending-request-done-p request))))))))

(deftest unknown-service-messages-are-logged-rather-than-fatal
  (let ((session (test-session)))
    (mt:session-receive-packet
     session
     (server-packet session
                    (tl:encode-tl-octets
                     (make-instance 'mt:msg-new-detailed-info
                                    :answer-message-id 5 :byte-count 6
                                    :status 7))
                    :sequence-number 0))
    (ok (find :unhandled (mt:session-events session) :key #'first))))

;;;; The data-centre table

(deftest data-centres-are-addressable
  (ok (typep (net:find-data-center 2) 'net:data-center))
  (ok (= 2 (net:data-center-id (net:find-data-center 2))))
  (ok (string/= (net:data-center-host (net:find-data-center 2))
                (net:data-center-host (net:find-data-center 2 :test t)))))
