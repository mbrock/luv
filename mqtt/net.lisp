;;;; The socket-owning edge.
;;;;
;;;; Everything in MQTT is a function of its inputs.  This is the one place
;;;; that connects, blocks, and reads a clock -- kept small on purpose,
;;;; because every line here is a line the test suite cannot replay.

(in-package #:mqtt.net)

(define-condition connection-closed (mqtt:mqtt-error)
  ((host :initarg :host :initform nil :reader connection-closed-host))
  (:report (lambda (condition stream)
             (format stream "The MQTT connection to ~A closed."
                     (or (connection-closed-host condition) "the broker"))))
  (:documentation "The peer closed the socket."))

(define-condition connection-timeout (mqtt:mqtt-error)
  ((seconds :initarg :seconds :reader connection-timeout-seconds))
  (:report (lambda (condition stream)
             (format stream "The broker sent nothing for ~D second~:P."
                     (connection-timeout-seconds condition))))
  (:documentation "The peer went quiet for longer than we agreed to wait."))

(define-condition connection-refused (mqtt:mqtt-error)
  ((connack :initarg :connack :reader connection-refused-connack))
  (:report (lambda (condition stream)
             (let ((connack (connection-refused-connack condition)))
               (format stream "The broker refused the connection: ~(~A~)~@[ (~A)~]."
                       (mqtt:packet-reason connack)
                       (mqtt:packet-property connack :reason-string)))))
  (:documentation "The CONNACK carried a failure reason code."))

(define-condition request-refused (mqtt:mqtt-error)
  ((pending :initarg :pending :reader request-refused-pending))
  (:report (lambda (condition stream)
             (let ((pending (request-refused-pending condition)))
               (format stream "The broker refused ~A: ~{~(~A~)~^, ~}."
                       (mqtt:pending-request-packet pending)
                       (mapcar #'mqtt:reason-code-name
                               (mqtt:pending-request-reason-codes pending))))))
  (:documentation "A subscribe, unsubscribe, or publish was answered with a
failure reason code."))

;;;; Connections

(defclass mqtt-connection ()
  ((host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (socket :initarg :socket :reader connection-socket)
   (session :initarg :session :reader connection-session)
   (timeout :initarg :timeout :initform 30 :accessor connection-read-timeout
            :documentation "How long one pump waits for the broker, in seconds.")
   (last-sent :initform (get-internal-real-time) :accessor connection-last-sent)
   (inbox :initform '() :accessor connection-inbox
          :documentation "Publications received but not yet taken, oldest first."))
  (:documentation "A TCP connection driving one MQTT session."))

(defmethod print-object ((connection mqtt-connection) stream)
  (print-unreadable-object (connection stream :type t)
    (format stream "~A:~D ~(~A~)" (connection-host connection)
            (connection-port connection)
            (mqtt:session-state (connection-session connection)))))

(defun host-address (host)
  "HOST as an address vector, without a DNS round trip when it is already
numeric.  IPv4 only; the tailnet gives every service one."
  (let ((parts (loop with start = 0
                     for position = (position #\. host :start start)
                     collect (parse-integer host :start start :end position
                                                 :junk-allowed t)
                     while position
                     do (setf start (1+ position)))))
    (if (and (= 4 (length parts))
             (every (lambda (part) (typep part '(unsigned-byte 8))) parts))
        (coerce parts 'vector)
        (sb-bsd-sockets:host-ent-address
         (sb-bsd-sockets:get-host-by-name host)))))

(defun open-mqtt-connection (&rest session-initargs
                             &key host (port 1883) (timeout 30)
                                  client-id keep-alive clean-start username
                                  password will properties)
  "Connect to the broker at HOST:PORT, send CONNECT, and wait for the CONNACK.
The remaining keywords describe the session; see MAKE-MQTT-SESSION.
Signals CONNECTION-REFUSED if the broker says no."
  (declare (ignore client-id keep-alive clean-start username password will
                   properties))
  (let ((session (apply #'mqtt:make-mqtt-session
                        (loop for (key value) on session-initargs by #'cddr
                              unless (member key '(:host :port :timeout))
                                append (list key value))))
        (socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (ignore-errors (sb-bsd-sockets:socket-close socket)))))
      (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
      (handler-case
          (if timeout
              (sb-ext:with-timeout timeout
                (sb-bsd-sockets:socket-connect socket (host-address host) port))
              (sb-bsd-sockets:socket-connect socket (host-address host) port))
        (sb-ext:timeout ()
          (error 'connection-timeout :seconds timeout)))
      (let ((connection (make-instance 'mqtt-connection
                                       :host host :port port :socket socket
                                       :session session :timeout timeout)))
        (mqtt:session-begin session)
        (connection-flush connection)
        (loop until (member (mqtt:session-state session) '(:connected :disconnected))
              do (pump-connection connection))
        (unless (mqtt:session-connected-p session)
          (error 'connection-refused :connack (mqtt:session-connack session)))
        connection))))

(defun close-mqtt-connection (connection &key (reason :success))
  "Say DISCONNECT if the session is still up, then close the socket."
  (let ((session (connection-session connection)))
    (ignore-errors
     (when (mqtt:session-connected-p session)
       (mqtt:session-disconnect session :reason reason)
       (connection-flush connection)))
    (ignore-errors (sb-bsd-sockets:socket-close (connection-socket connection)))
    connection))

(defmacro with-mqtt-connection ((variable &rest options) &body body)
  "Open a connection, run BODY, and close it however BODY leaves."
  `(let ((,variable (open-mqtt-connection ,@options)))
     (unwind-protect (progn ,@body)
       (close-mqtt-connection ,variable))))

;;;; Moving octets

(defun write-octets (connection octets)
  "Write OCTETS to the socket in full."
  (let ((socket (connection-socket connection)))
    (loop with sent = 0
          while (< sent (length octets))
          do (let ((count (sb-bsd-sockets:socket-send
                           socket (subseq octets sent) (- (length octets) sent))))
               (unless (and count (plusp count))
                 (error 'connection-closed :host (connection-host connection)))
               (incf sent count)))
    (setf (connection-last-sent connection) (get-internal-real-time))
    octets))

(defun connection-flush (connection)
  "Write everything the session has queued."
  (dolist (octets (mqtt:drain-session-outbox (connection-session connection)))
    (write-octets connection octets))
  connection)

(defconstant +read-chunk-length+ 8192)

(defun seconds-since-last-send (connection)
  (/ (- (get-internal-real-time) (connection-last-sent connection))
     internal-time-units-per-second))

(defun ping-if-due (connection)
  "Keep the session alive: ping once most of the keep-alive interval has
passed without our sending anything (section 3.1.2.10)."
  (let* ((session (connection-session connection))
         (keep-alive (mqtt:session-keep-alive session)))
    (when (and (plusp keep-alive)
               (mqtt:session-connected-p session)
               (>= (seconds-since-last-send connection) (* 3/4 keep-alive)))
      (mqtt:session-ping session)
      (connection-flush connection))))

(defun pump-connection (connection &key (timeout (connection-read-timeout connection)))
  "Wait up to TIMEOUT seconds for octets, hand them to the session, write
whatever that produced, and return the events it recorded, oldest first.
Publications also land in the connection's inbox for NEXT-MESSAGE."
  (let* ((session (connection-session connection))
         (socket (connection-socket connection))
         (keep-alive (mqtt:session-keep-alive session))
         ;; Wake in time to ping, whatever the caller's patience.
         (wait (if (and timeout (plusp keep-alive))
                   (min timeout (max 1/10 (- (* 3/4 keep-alive)
                                             (seconds-since-last-send connection))))
                   timeout))
         (buffer (mqtt:make-octets +read-chunk-length+)))
    (ping-if-due connection)
    ;; SBCL's sockets have no receive-timeout option, so the wait is explicit:
    ;; block on the descriptor, then take whatever arrived.
    (cond ((or (null wait)
               (sb-sys:wait-until-fd-usable
                (sb-bsd-sockets:socket-file-descriptor socket) :input wait))
           (let ((count (nth-value 1 (sb-bsd-sockets:socket-receive socket buffer nil))))
             (when (or (null count) (zerop count))
               (error 'connection-closed :host (connection-host connection)))
             (mqtt:session-receive session (subseq buffer 0 count))
             (connection-flush connection)
             (let ((events (mqtt:drain-session-events session)))
               (dolist (event events)
                 (when (eq :message (first event))
                   (setf (connection-inbox connection)
                         (append (connection-inbox connection) (list (second event))))))
               events)))
          ((< wait timeout)
           ;; Woke early for the keep-alive; not a timeout yet.
           (ping-if-due connection)
           '())
          (t (error 'connection-timeout :seconds timeout)))))

(defun pump-connection-until (connection predicate &key (timeout (connection-read-timeout connection)))
  "Pump until PREDICATE (called with no arguments) is true, giving the whole
TIMEOUT to the wait.  Returns the events seen, oldest first."
  (let ((deadline (+ (get-internal-real-time)
                     (if timeout (* timeout internal-time-units-per-second) 0)))
        (events '()))
    (loop until (funcall predicate)
          do (let ((left (if timeout
                             (/ (- deadline (get-internal-real-time))
                                internal-time-units-per-second)
                             nil)))
               (when (and left (<= left 0))
                 (error 'connection-timeout :seconds timeout))
               (setf events (append events (pump-connection connection :timeout left)))))
    events))

(defun connection-await (connection pending &key (timeout (connection-read-timeout connection)))
  "Pump until PENDING is answered.  Returns it; signals REQUEST-REFUSED if
the answer was a failure."
  (pump-connection-until connection
                         (lambda () (mqtt:pending-request-done-p pending))
                         :timeout timeout)
  (when (mqtt:pending-request-failed-p pending)
    (error 'request-refused :pending pending))
  pending)

;;;; The client verbs

(defun publish (connection topic payload &key (qos 0) retain properties (wait t))
  "Publish PAYLOAD to TOPIC.  At QoS 1 or 2, WAIT (the default) blocks until
the broker acknowledges.  Returns the pending request, or NIL at QoS 0."
  (let ((pending (mqtt:session-publish (connection-session connection) topic payload
                                       :qos qos :retain retain :properties properties)))
    (connection-flush connection)
    (if (and pending wait)
        (connection-await connection pending)
        pending)))

(defun subscribe (connection &rest subscriptions)
  "Subscribe and wait for the SUBACK.  Returns the granted reason codes."
  (let ((pending (apply #'mqtt:session-subscribe (connection-session connection)
                        subscriptions)))
    (connection-flush connection)
    (mqtt:pending-request-reason-codes (connection-await connection pending))))

(defun unsubscribe (connection &rest topic-filters)
  "Unsubscribe and wait for the UNSUBACK.  Returns the reason codes."
  (let ((pending (apply #'mqtt:session-unsubscribe (connection-session connection)
                        topic-filters)))
    (connection-flush connection)
    (mqtt:pending-request-reason-codes (connection-await connection pending))))

(defun ping (connection)
  "Ping the broker and wait for the pong.  The shortest proof the connection
is alive."
  (let ((session (connection-session connection))
        (ponged nil))
    (mqtt:session-ping session)
    (connection-flush connection)
    (loop until ponged
          do (dolist (event (pump-connection connection))
               (when (eq :pong (first event)) (setf ponged t))))
    t))

(defun next-message (connection &key (timeout (connection-read-timeout connection)))
  "The next publication, pumping until one arrives or TIMEOUT passes."
  (pump-connection-until connection
                         (lambda () (connection-inbox connection))
                         :timeout timeout)
  (pop (connection-inbox connection)))

;;;; The lobby

(defparameter *lobby-host* nil
  "The lobby broker's host, when set by hand or by LUV_LOBBY_HOST.  NIL means
discover it on the local tailnet; see LOBBY-HOST.")

(defparameter *lobby-port* 1883)

(defparameter *lobby-service-name* "luv-lobby"
  "The Tailscale Service name the lobby is published under (lobby/README.md).
Its MagicDNS name is this name under the tailnet's MagicDNS suffix.")

(defvar *discovered-lobby-host* nil
  "The host DISCOVER-LOBBY-HOST last found, so the tailscale CLI is asked once.")

(defun tailnet-magic-dns-suffix ()
  "This machine's tailnet MagicDNS suffix (\"example.ts.net\"), asked of the
local tailscale CLI, or NIL when there is no tailscale here or it is down.
Runs tailscale status --json and looks for the MagicDNSSuffix string; no JSON
reader needed for one key."
  (ignore-errors
   (let* ((output (sb-ext:with-timeout 3
                    (with-output-to-string (out)
                      (uiop:run-program '("tailscale" "status" "--json")
                                        :output out :error-output nil
                                        :input nil :ignore-error-status t))))
          (key "\"MagicDNSSuffix\"")
          (at (search key output)))
     (when at
       ;; After the key comes a colon, then the quoted value.
       (let* ((colon (position #\: output :start (+ at (length key))))
              (open (and colon (position #\" output :start colon)))
              (close (and open (position #\" output :start (1+ open)))))
         (when (and open close (< (1+ open) close))
           (subseq output (1+ open) close)))))))

(defun discover-lobby-host ()
  "Find the lobby on the local tailnet: the service name under this tailnet's
MagicDNS suffix, or the bare service name (MagicDNS search domains usually
resolve it) when the suffix cannot be learned.  Cached in
*DISCOVERED-LOBBY-HOST*; set that to NIL to look again."
  (or *discovered-lobby-host*
      (let ((suffix (tailnet-magic-dns-suffix)))
        ;; Cache a discovery, not a transient failure.  The bare Service name
        ;; remains a useful MagicDNS fallback, but the next call should ask
        ;; Tailscale again when it was all we could produce.
        (if suffix
            (setf *discovered-lobby-host*
                  (format nil "~A.~A" *lobby-service-name* suffix))
            *lobby-service-name*))))

(defun lobby-host ()
  "The host to reach the lobby at: *LOBBY-HOST* if set, else LUV_LOBBY_HOST,
else whatever the tailnet says (DISCOVER-LOBBY-HOST)."
  (or *lobby-host*
      (let ((env (uiop:getenv "LUV_LOBBY_HOST")))
        (and env (plusp (length env)) env))
      (discover-lobby-host)))

(defun lobby-available-p ()
  "True when the lobby's name resolves from here.  Cheap and offline-safe; it
says nothing about whether the broker answers."
  (ignore-errors (and (host-address (lobby-host)) t)))

(defun open-lobby-connection (&rest session-initargs &key &allow-other-keys)
  "Connect to the luv lobby.  Tailscale is the account system, so no
username or password is needed; the broker knows who this node is."
  (apply #'open-mqtt-connection :host (lobby-host) :port *lobby-port*
         session-initargs))

;;;; The lobby as a value store
;;;;
;;;; A retained message is a value the broker keeps for whoever subscribes
;;;; next, which makes the lobby a small key-value store that follows the
;;;; tailnet around instead of any one machine's environment variables.  The
;;;; store is the topics under luv/store/; a key is the rest of the topic.
;;;; Anyone on the tailnet who can reach the lobby can read it, which is the
;;;; intended audience for things like API keys shared among my machines.

(defparameter *lobby-store-prefix* "luv/store/")

(defvar *lobby-client-counter* 0)

(defun lobby-store-client-id ()
  (format nil "store-~A-~D-~D" (or (ignore-errors (machine-instance)) "luv")
          (sb-posix:getpid) (incf *lobby-client-counter*)))

(defun store-topic (key)
  (concatenate 'string *lobby-store-prefix* key))

(defun lobby-put (key value)
  "Store VALUE (a string or octet vector) under KEY in the lobby: a retained
QoS 1 publish on luv/store/KEY, acknowledged before we return.  Returns VALUE."
  (with-mqtt-connection (c :host (lobby-host) :port *lobby-port*
                           :client-id (lobby-store-client-id))
    (publish c (store-topic key) value :qos 1 :retain t))
  value)

(defun lobby-delete (key)
  "Forget KEY: a retained empty payload clears the broker's retained message."
  (with-mqtt-connection (c :host (lobby-host) :port *lobby-port*
                           :client-id (lobby-store-client-id))
    (publish c (store-topic key) "" :qos 1 :retain t))
  key)

(defun collect-retained (connection topic-filter)
  "Subscribe to TOPIC-FILTER and return the retained publications under it.
A ping after the subscribe is the end marker: the broker answers packets in
order, so once the pong is back every retained message has arrived."
  (subscribe connection (list topic-filter :qos 1))
  (ping connection)
  (loop for message = (and (connection-inbox connection) (pop (connection-inbox connection)))
        while message collect message))

(defun lobby-get (key &key (default nil))
  "The value stored under KEY as a string, or DEFAULT when there is none.
Signals the connection errors if the lobby cannot be reached, so a caller can
tell absent from unreachable."
  (with-mqtt-connection (c :host (lobby-host) :port *lobby-port*
                           :client-id (lobby-store-client-id))
    (let ((message (first (collect-retained c (store-topic key)))))
      (if (and message (plusp (length (mqtt:publish-payload message))))
          (mqtt:publish-payload-string message)
          default))))

(defun lobby-value (key)
  "LOBBY-GET that answers NIL rather than erroring when the lobby is not
there at all -- the shape for optional fallbacks like an API key."
  (and (lobby-available-p)
       (ignore-errors (lobby-get key))))

(defun lobby-keys ()
  "Every key in the store with its value, as an alist (KEY . VALUE)."
  (with-mqtt-connection (c :host (lobby-host) :port *lobby-port*
                           :client-id (lobby-store-client-id))
    (loop for message in (collect-retained c (concatenate 'string *lobby-store-prefix* "#"))
          for topic = (mqtt:publish-topic message)
          when (plusp (length (mqtt:publish-payload message)))
            collect (cons (subseq topic (length *lobby-store-prefix*))
                          (mqtt:publish-payload-string message)))))
