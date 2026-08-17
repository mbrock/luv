;;;; The socket-owning edge.
;;;;
;;;; Everything below this file is a function of its inputs.  This is the one
;;;; place that connects, blocks, reads a clock, and draws real randomness --
;;;; kept small on purpose, because every line here is a line the test suite
;;;; cannot replay.

(in-package #:telegram.net)

(define-condition connection-closed (mt:mtproto-error)
  ((host :initarg :host :initform nil :reader connection-closed-host))
  (:report (lambda (condition stream)
             (format stream "The connection to ~A closed."
                     (or (connection-closed-host condition) "Telegram"))))
  (:documentation "The peer closed the socket."))

(define-condition connection-timeout (mt:mtproto-error)
  ((seconds :initarg :seconds :reader connection-timeout-seconds))
  (:report (lambda (condition stream)
             (format stream "Telegram sent nothing for ~D second~:P."
                     (connection-timeout-seconds condition))))
  (:documentation "The peer went quiet for longer than we agreed to wait."))

(define-condition transport-error (mt:mtproto-error)
  ((code :initarg :code :reader transport-error-code))
  (:report (lambda (condition stream)
             (format stream "Telegram's transport rejected us with ~D."
                     (transport-error-code condition))))
  (:documentation
   "A four-byte negative frame, which is how the transport layer reports a
refusal before any MTProto message exists.  -404 usually means the
authorization key is unknown to this data centre."))

;;;; Data centres

(defclass data-center ()
  ((id :initarg :id :reader data-center-id)
   (host :initarg :host :reader data-center-host)
   (port :initarg :port :initform 443 :reader data-center-port)
   (test :initarg :test :initform nil :reader data-center-test-p))
  (:documentation "One of Telegram's front doors."))

(defmethod print-object ((center data-center) stream)
  (print-unreadable-object (center stream :type t)
    (format stream "~:[~;test ~]dc~D ~A:~D" (data-center-test-p center)
            (data-center-id center) (data-center-host center)
            (data-center-port center))))

(defparameter *data-centers*
  (list (make-instance 'data-center :id 1 :host "149.154.175.53")
        (make-instance 'data-center :id 2 :host "149.154.167.51")
        (make-instance 'data-center :id 3 :host "149.154.175.100")
        (make-instance 'data-center :id 4 :host "149.154.167.91")
        (make-instance 'data-center :id 5 :host "91.108.56.130")
        (make-instance 'data-center :id 1 :host "149.154.175.10" :test t)
        (make-instance 'data-center :id 2 :host "149.154.167.40" :test t)
        (make-instance 'data-center :id 3 :host "149.154.175.117" :test t))
  "Telegram's published entry points.  A real client learns the current list
from help.getConfig and stores it; these are the addresses that bootstrap
that.")

(defun find-data-center (id &key test)
  "The entry point for data centre ID."
  (or (find-if (lambda (center)
                 (and (= id (data-center-id center))
                      (eq (not test) (not (data-center-test-p center)))))
               *data-centers*)
      (error "No ~:[production~;test~] data centre ~D is known." test id)))

;;;; Connections

(defclass mtproto-connection ()
  ((host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (socket :initarg :socket :reader connection-socket)
   (dc-id :initarg :dc-id :initform 0 :reader connection-dc-id)
   (test :initarg :test :initform nil :reader connection-test-p)
   (transport :initarg :transport :reader connection-transport)
   (decoder :reader connection-decoder)
   (frames :initform '() :accessor connection-frames
           :documentation "Complete frames read but not yet consumed.")
   (last-message-id :initform nil :accessor connection-last-message-id
                    :documentation "For plain messages, before a session exists.")
   (session :initform nil :accessor connection-session)
   (timeout :initarg :timeout :initform 30 :accessor connection-read-timeout
            :documentation "How long to wait for the peer, in seconds.")
   (entropy :initarg :entropy :reader connection-entropy)
   (clock :initarg :clock :reader connection-clock))
  (:documentation
   "A TCP connection to one Telegram data centre, with the framing and, once
one exists, the session that runs over it."))

(defmethod print-object ((connection mtproto-connection) stream)
  (print-unreadable-object (connection stream :type t)
    (format stream "~A:~D~@[ dc~D~]" (connection-host connection)
            (connection-port connection)
            (let ((id (connection-dc-id connection)))
              (unless (zerop id) id)))))

(defmethod initialize-instance :after ((connection mtproto-connection) &key)
  (setf (slot-value connection 'decoder)
        (mt:make-frame-decoder (connection-transport connection))))

(defun host-address (host)
  "HOST as an IPv4 address vector, without a DNS round trip when it is
already numeric."
  (let ((parts (loop with start = 0
                     for position = (position #\. host :start start)
                     collect (parse-integer host :start start :end position
                                                 :junk-allowed t)
                     while position
                     do (setf start (1+ position)))))
    (if (and (= 4 (length parts)) (every (lambda (part) (typep part '(unsigned-byte 8)))
                                         parts))
        (coerce parts 'vector)
        (sb-bsd-sockets:host-ent-address
         (sb-bsd-sockets:get-host-by-name host)))))

(defun open-mtproto-connection (&key host (port 443) dc-id test
                                     (transport (make-instance
                                                 'mt:abridged-transport))
                                     (timeout 30)
                                     (entropy octets:*entropy*)
                                     (clock octets:*clock*))
  "Connect to a Telegram data centre and announce the transport.  Give either
HOST or DC-ID."
  (let* ((center (when (and dc-id (not host)) (find-data-center dc-id :test test)))
         (host (or host (data-center-host center)))
         (port (if center (data-center-port center) port))
         (dc-id (or dc-id 0))
         (socket (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream :protocol :tcp)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (ignore-errors (sb-bsd-sockets:socket-close socket)))))
      (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
      (sb-bsd-sockets:socket-connect socket (host-address host) port)
      (let ((connection (make-instance 'mtproto-connection
                                       :host host :port port :socket socket
                                       :dc-id dc-id :test test
                                       :transport transport
                                       :timeout timeout
                                       :entropy entropy :clock clock)))
        (write-octets connection (mt:transport-client-prefix transport))
        connection))))

(defun close-mtproto-connection (connection)
  "Close CONNECTION's socket."
  (ignore-errors (sb-bsd-sockets:socket-close (connection-socket connection)))
  connection)

(defmacro with-mtproto-connection ((variable &rest options) &body body)
  "Open a connection, run BODY, and close it however BODY leaves."
  `(let ((,variable (open-mtproto-connection ,@options)))
     (unwind-protect (progn ,@body)
       (close-mtproto-connection ,variable))))

(defun write-octets (connection octets)
  "Write OCTETS to the socket in full."
  (let ((buffer (octets:to-octets octets))
        (socket (connection-socket connection)))
    (loop with sent = 0
          while (< sent (length buffer))
          do (let ((count (sb-bsd-sockets:socket-send
                           socket (subseq buffer sent) (- (length buffer) sent))))
               (unless (and count (plusp count))
                 (error 'connection-closed :host (connection-host connection)))
               (incf sent count)))
    octets))

(defconstant +read-chunk-length+ 8192)

(defun read-transport-frame (connection)
  "Block until one complete frame arrives, and return it.  Quick
acknowledgements are returned as they come; the callers that do not care
about them skip them."
  (loop
    (when (connection-frames connection)
      (return (pop (connection-frames connection))))
    (let* ((socket (connection-socket connection))
           (timeout (connection-read-timeout connection))
           (buffer (octets:make-octets +read-chunk-length+)))
      ;; SBCL's sockets have no receive-timeout option, so the wait is
      ;; explicit: block on the descriptor, then take whatever arrived.
      (unless (or (null timeout)
                  (sb-sys:wait-until-fd-usable
                   (sb-bsd-sockets:socket-file-descriptor socket)
                   :input timeout))
        (error 'connection-timeout :seconds timeout))
      (let ((count (nth-value 1 (sb-bsd-sockets:socket-receive
                                 socket buffer nil))))
        (when (or (null count) (zerop count))
          (error 'connection-closed :host (connection-host connection)))
        (setf (connection-frames connection)
              (mt:feed-transport (connection-decoder connection)
                                 (subseq buffer 0 count)))))))

(defun read-payload-frame (connection)
  "The next frame that is a payload, skipping quick acknowledgements and
turning a bare transport error code into a condition."
  (loop for frame = (read-transport-frame connection)
        unless (typep frame 'mt:quick-ack)
          do (when (= 4 (length frame))
               (error 'transport-error
                      :code (tl:read-tl-int (tl:make-tl-reader frame))))
             (return frame)))

;;;; The handshake

(defun send-plain-payload (connection payload)
  "Wrap PAYLOAD in a plain message and a transport frame, and write it."
  (let ((message-id (mt:next-message-id
                     (connection-last-message-id connection)
                     (octets:clock-unix-nanoseconds (connection-clock connection)))))
    (setf (connection-last-message-id connection) message-id)
    (write-octets connection
                  (mt:encode-transport-frame
                   (connection-transport connection)
                   (mt:encode-plain-message
                    (mt:make-plain-message message-id payload))))))

(defun create-auth-key (connection &key (public-keys (mt:telegram-public-keys))
                                        nonce (dc-in-inner-data t))
  "Run the authorization-key exchange over CONNECTION and return the
AUTH-KEY-MATERIAL it produces."
  (let ((exchange (mt:make-auth-exchange
                   :public-keys public-keys
                   :entropy (connection-entropy connection)
                   :clock (connection-clock connection)
                   :dc-id (connection-dc-id connection)
                   :test (connection-test-p connection)
                   :dc-in-inner-data dc-in-inner-data)))
    (loop for payload = (mt:begin-auth-exchange exchange :nonce nonce)
            then (mt:advance-auth-exchange
                  exchange
                  (mt:plain-message-body
                   (mt:decode-plain-message (read-payload-frame connection))))
          while payload
          do (send-plain-payload connection payload))
    (mt:auth-exchange-result exchange)))

;;;; Sessions over a connection

(defun establish-session (connection material &key session-id)
  "Attach a session over MATERIAL to CONNECTION."
  (setf (connection-session connection)
        (mt:make-mtproto-session material
                                 :session-id session-id
                                 :entropy (connection-entropy connection)
                                 :clock (connection-clock connection))))

(defun flush-session (connection)
  "Write everything the session has queued."
  (dolist (packet (mt:drain-session-outbox (connection-session connection)))
    (write-octets connection
                  (mt:encode-transport-frame (connection-transport connection)
                                             packet))))

(defun pump-connection (connection)
  "Read one packet, let the session handle it, and write whatever that
produced.  Returns the decoded object."
  (let ((object (mt:session-receive-packet (connection-session connection)
                                           (read-payload-frame connection))))
    (flush-session connection)
    object))

(defun connection-invoke (connection body &key name (limit 32))
  "Send BODY as a request and pump the connection until its answer arrives.
Returns the raw result bytes, or signals the server's rpc_error."
  (let ((session (connection-session connection)))
    (multiple-value-bind (encrypted request)
        (mt:session-send-request session body :name name)
      (write-octets connection
                    (mt:encode-transport-frame (connection-transport connection)
                                               encrypted))
      (loop repeat limit
            until (mt:pending-request-done-p request)
            do (pump-connection connection))
      (unless (mt:pending-request-done-p request)
        (error 'mt:mtproto-protocol-error
               :detail (format nil "no answer to ~A after ~D packets"
                               (or name "request") limit)))
      (let ((failure (mt:pending-request-error request)))
        (when (typep failure 'mt:rpc-error)
          (error 'mt:remote-rpc-error :code (mt:rpc-error-code failure)
                                      :message (mt:rpc-error-message failure))))
      (values (mt:pending-request-result request) request))))

(defun connection-ping (connection &key (ping-id 1) (limit 8))
  "Ping the server and wait for the pong.  The shortest end-to-end proof that
an authorization key works.

The pong is looked for in the session's event log rather than in what
PUMP-CONNECTION returns, because the server is free to deliver it inside a
container alongside other messages."
  (let ((session (connection-session connection)))
    (write-octets connection
                  (mt:encode-transport-frame
                   (connection-transport connection)
                   (mt:session-send-ping session ping-id)))
    (flet ((ponged-p ()
             (find-if (lambda (event)
                        (and (eq :pong (first event))
                             (eql ping-id (second event))))
                      (mt:session-events session))))
      (loop repeat limit
            do (pump-connection connection)
               (let ((event (ponged-p)))
                 (when event (return event)))
            finally (error 'mt:mtproto-protocol-error
                           :detail "no pong arrived")))))
