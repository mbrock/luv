;;;; A client session (section 4): the state between CONNECT and DISCONNECT,
;;;; without a socket.
;;;;
;;;; The session is driven from outside.  Callers put requests in with
;;;; SESSION-PUBLISH, SESSION-SUBSCRIBE and friends, feed it the octets that
;;;; arrived with SESSION-RECEIVE, and take out what it wants sent with
;;;; DRAIN-SESSION-OUTBOX and what happened with DRAIN-SESSION-EVENTS.
;;;; Every inbound packet is handled by one method of HANDLE-PACKET.

(in-package #:mqtt)

;;;; Pending requests

(defclass pending-request ()
  ((packet :initarg :packet :reader pending-request-packet
           :documentation "What we sent.")
   (response :initform nil :accessor pending-request-response
             :documentation "The packet that settled it, once one has.")
   (done :initform nil :accessor pending-request-done-p))
  (:documentation "A request awaiting the server's answer, keyed by packet id."))

(defmethod print-object ((pending pending-request) stream)
  (print-unreadable-object (pending stream :type t)
    (format stream "~A~:[~; done~]" (pending-request-packet pending)
            (pending-request-done-p pending))))

(defun pending-request-id (pending)
  (packet-id (pending-request-packet pending)))

(defun pending-request-reason-codes (pending)
  "The reason codes the answer carried: one per filter for subscriptions,
one for a publication.  NIL until answered."
  (let ((response (pending-request-response pending)))
    (etypecase response
      (null nil)
      (reason-list-packet (packet-reason-codes response))
      (reasoned-packet (list (packet-reason-code response))))))

(defun pending-request-failed-p (pending)
  "True once the answer has arrived and any of it is a failure."
  (and (pending-request-done-p pending)
       (some #'reason-code-error-p (pending-request-reason-codes pending))))

(defun settle-pending-request (pending response)
  (setf (pending-request-response pending) response
        (pending-request-done-p pending) t)
  pending)

;;;; The session

(defclass mqtt-session ()
  ((client-id :initarg :client-id :initform "" :accessor session-client-id
              :documentation "Empty asks the server to assign one.")
   (keep-alive :initarg :keep-alive :initform 60 :accessor session-keep-alive
               :documentation "Seconds between packets before we must ping.
The server may lower it in CONNACK.")
   (clean-start :initarg :clean-start :initform t :accessor session-clean-start-p)
   (username :initarg :username :initform nil :accessor session-username)
   (password :initarg :password :initform nil :accessor session-password)
   (will :initarg :will :initform nil :accessor session-will)
   (connect-properties :initarg :properties :initform '()
                       :accessor session-connect-properties)
   (state :initform :new :accessor session-state
          :documentation "One of :new :connecting :connected :disconnected.")
   (connack :initform nil :accessor session-connack
            :documentation "The server's answer to our CONNECT.")
   (decoder :initform (make-packet-decoder) :reader session-decoder)
   (outbox :initform '() :accessor session-outbox
           :documentation "Encoded packets not yet handed to the socket, newest first.")
   (events :initform '() :accessor session-events
           :documentation "What happened that the owner has not yet seen, newest first.")
   (next-packet-id :initform 1 :accessor session-next-packet-id)
   (pending :initform (make-hash-table) :reader session-pending
            :documentation "Packet id -> PENDING-REQUEST.")
   (inbound-qos2 :initform (make-hash-table) :reader session-inbound-qos2
                 :documentation "Packet ids of QoS 2 publications delivered but not yet released."))
  (:documentation "One MQTT 5 client session, sans socket."))

(defmethod print-object ((session mqtt-session) stream)
  (print-unreadable-object (session stream :type t)
    (format stream "~S ~(~A~)" (session-client-id session) (session-state session))))

(defun make-mqtt-session (&rest initargs &key client-id keep-alive clean-start
                                              username password will properties)
  "A fresh session that has not yet said CONNECT."
  (declare (ignore client-id keep-alive clean-start username password will properties))
  (apply #'make-instance 'mqtt-session initargs))

(defun session-connected-p (session)
  (eq :connected (session-state session)))

(defun session-server-property (session name &optional default)
  "A property the server sent in CONNACK, such as :maximum-qos."
  (let ((connack (session-connack session)))
    (if connack (packet-property connack name default) default)))

;;;; Outbox and events

(defun queue-packet (session packet)
  "Encode PACKET for sending."
  (push (encode-packet packet) (session-outbox session))
  packet)

(defun drain-session-outbox (session)
  "Everything queued for sending, oldest first; the queue is emptied."
  (prog1 (reverse (session-outbox session))
    (setf (session-outbox session) '())))

(defun add-event (session &rest event)
  (push event (session-events session))
  event)

(defun drain-session-events (session)
  "Everything that happened since the last drain, oldest first.  An event
is a list headed by one of :connected :refused :message :published
:subscribed :unsubscribed :pong :disconnected :auth."
  (prog1 (reverse (session-events session))
    (setf (session-events session) '())))

;;;; Packet identifiers (section 2.2.1)

(defun allocate-packet-id (session)
  "A packet id no request is using."
  (let ((pending (session-pending session)))
    (loop repeat 65535
          for id = (session-next-packet-id session)
          do (setf (session-next-packet-id session) (if (= id 65535) 1 (1+ id)))
             (unless (gethash id pending)
               (return id))
          finally (error 'protocol-error :detail "all 65535 packet ids are in flight"))))

(defun register-pending (session packet)
  "Send PACKET, which needs a fresh id, and remember it until answered."
  (let ((pending (make-instance 'pending-request :packet packet)))
    (setf (packet-id packet) (allocate-packet-id session)
          (gethash (packet-id packet) (session-pending session)) pending)
    (queue-packet session packet)
    pending))

(defun take-pending (session id type)
  "The pending request answered by a TYPE packet with ID, removed."
  (let ((pending (gethash id (session-pending session))))
    (unless (and pending (typep (pending-request-packet pending) type))
      (error 'protocol-error
             :detail (format nil "no ~(~A~) is waiting on packet id ~D" type id)))
    (remhash id (session-pending session))
    pending))

;;;; Requests

(defun require-state (session &rest states)
  (unless (member (session-state session) states)
    (error 'protocol-error
           :detail (format nil "session is ~(~A~), not ~{~(~A~)~^ or ~}"
                           (session-state session) states))))

(defun session-begin (session)
  "Queue the CONNECT.  Returns the session."
  (require-state session :new)
  (setf (session-state session) :connecting)
  (queue-packet session
                (make-instance 'connect-packet
                               :client-id (session-client-id session)
                               :clean-start (session-clean-start-p session)
                               :keep-alive (session-keep-alive session)
                               :username (session-username session)
                               :password (session-password session)
                               :will (session-will session)
                               :properties (session-connect-properties session)))
  session)

(defun session-publish (session topic payload &key (qos 0) retain properties)
  "Publish PAYLOAD (a string or octets) to TOPIC.  At QoS 0 there is nothing
to wait for and NIL is returned; otherwise the PENDING-REQUEST that the
acknowledgement will settle."
  (require-state session :connected)
  (let ((packet (make-instance 'publish-packet :topic topic :payload payload
                                               :qos qos :retain retain
                                               :properties properties)))
    (if (zerop qos)
        (progn (queue-packet session packet) nil)
        (register-pending session packet))))

(defun session-subscribe (session &rest subscriptions)
  "Subscribe to SUBSCRIPTIONS, each a filter string, a (FILTER . options)
list, or a SUBSCRIPTION.  Returns the pending request."
  (require-state session :connected)
  (register-pending session
                    (make-instance 'subscribe-packet
                                   :subscriptions (mapcar #'subscription subscriptions))))

(defun session-unsubscribe (session &rest topic-filters)
  "Unsubscribe from TOPIC-FILTERS.  Returns the pending request."
  (require-state session :connected)
  (register-pending session
                    (make-instance 'unsubscribe-packet :topic-filters topic-filters)))

(defun session-ping (session)
  "Queue a PINGREQ; a :pong event answers it."
  (require-state session :connected)
  (queue-packet session (make-instance 'pingreq-packet))
  session)

(defun session-disconnect (session &key (reason :success) properties)
  "Queue a DISCONNECT and consider the session over."
  (require-state session :connected :connecting)
  (queue-packet session (make-instance 'disconnect-packet
                                       :reason-code (reason-code reason)
                                       :properties properties))
  (setf (session-state session) :disconnected)
  session)

;;;; Receiving

(defun session-receive (session octets)
  "Feed OCTETS from the wire; handle every packet they complete.  Returns
the packets handled, oldest first."
  (let ((packets (feed-decoder (session-decoder session) octets)))
    (dolist (packet packets packets)
      (handle-packet session packet))))

(defgeneric handle-packet (session packet)
  (:documentation "React to an inbound PACKET: update state, queue replies,
record events."))

(defmethod handle-packet ((session mqtt-session) (packet packet))
  ;; The server-to-client half of the protocol is closed and listed below;
  ;; anything else here is a server bug and says so.
  (error 'protocol-error
         :detail (format nil "the server sent a ~A" (type-of packet))))

(defmethod handle-packet ((session mqtt-session) (packet connack-packet))
  (require-state session :connecting)
  (setf (session-connack session) packet)
  (cond ((reason-code-error-p (packet-reason-code packet))
         (setf (session-state session) :disconnected)
         (add-event session :refused packet))
        (t
         (setf (session-state session) :connected)
         (let ((assigned (packet-property packet :assigned-client-identifier))
               (keep-alive (packet-property packet :server-keep-alive)))
           (when assigned (setf (session-client-id session) assigned))
           (when keep-alive (setf (session-keep-alive session) keep-alive)))
         (add-event session :connected packet))))

(defmethod handle-packet ((session mqtt-session) (packet publish-packet))
  (require-state session :connected)
  (let ((id (packet-id packet)))
    (ecase (publish-qos packet)
      (0 (add-event session :message packet))
      (1 (add-event session :message packet)
         (queue-packet session (make-instance 'puback-packet :packet-id id)))
      (2 ;; Deliver on arrival, and remember the id until the release so a
         ;; resent duplicate is acknowledged again but not delivered twice.
         (unless (gethash id (session-inbound-qos2 session))
           (setf (gethash id (session-inbound-qos2 session)) t)
           (add-event session :message packet))
         (queue-packet session (make-instance 'pubrec-packet :packet-id id))))))

(defmethod handle-packet ((session mqtt-session) (packet pubrel-packet))
  (remhash (packet-id packet) (session-inbound-qos2 session))
  (queue-packet session (make-instance 'pubcomp-packet :packet-id (packet-id packet))))

(defmethod handle-packet ((session mqtt-session) (packet puback-packet))
  (let ((pending (take-pending session (packet-id packet) 'publish-packet)))
    (add-event session :published (settle-pending-request pending packet))))

(defmethod handle-packet ((session mqtt-session) (packet pubrec-packet))
  (let* ((id (packet-id packet))
         (pending (gethash id (session-pending session))))
    (unless (and pending (typep (pending-request-packet pending) 'publish-packet))
      (error 'protocol-error
             :detail (format nil "PUBREC for unknown packet id ~D" id)))
    (cond ((reason-code-error-p (packet-reason-code packet))
           (remhash id (session-pending session))
           (add-event session :published (settle-pending-request pending packet)))
          (t
           ;; Not settled yet: the id stays in flight until PUBCOMP.
           (queue-packet session (make-instance 'pubrel-packet :packet-id id))))))

(defmethod handle-packet ((session mqtt-session) (packet pubcomp-packet))
  (let ((pending (take-pending session (packet-id packet) 'publish-packet)))
    (add-event session :published (settle-pending-request pending packet))))

(defmethod handle-packet ((session mqtt-session) (packet suback-packet))
  (let ((pending (take-pending session (packet-id packet) 'subscribe-packet)))
    (add-event session :subscribed (settle-pending-request pending packet))))

(defmethod handle-packet ((session mqtt-session) (packet unsuback-packet))
  (let ((pending (take-pending session (packet-id packet) 'unsubscribe-packet)))
    (add-event session :unsubscribed (settle-pending-request pending packet))))

(defmethod handle-packet ((session mqtt-session) (packet pingresp-packet))
  (add-event session :pong packet))

(defmethod handle-packet ((session mqtt-session) (packet disconnect-packet))
  (setf (session-state session) :disconnected)
  (add-event session :disconnected packet))

(defmethod handle-packet ((session mqtt-session) (packet auth-packet))
  ;; Enhanced authentication is not implemented; the owner sees it happen.
  (add-event session :auth packet))
