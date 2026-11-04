;;;; Endpoints: one worker owns the transport, the game drains a mailbox.
;;;;
;;;; An endpoint is a host (listening for guests) or a guest (joined to one
;;;; host).  Its worker thread is the only code that touches a socket or a
;;;; loopback queue.  The game encodes on its own thread -- so an unencodable
;;;; value is its own error -- hands the octets to the worker, and drains
;;;; decoded messages and lifecycle notices at a frame boundary, as the lobby
;;;; radio does (lobby/client.lisp).
;;;;
;;;; Unreliable messages go by datagram while both ends hear each other's
;;;; probes, and by the stream otherwise.  A host learns a guest's datagram
;;;; address from the guest's first probe, so no port is ever announced.

(in-package #:luvcraft.wire.net)

(defconstant +probe-interval+ 1/2)
(defconstant +datagram-freshness+ 3/2
  "Datagrams heard within this many seconds keep the datagram path open.")

(defun now ()
  (/ (get-internal-real-time) internal-time-units-per-second))

;;;; Mailbox items

(defstruct arrival
  "A decoded message from PEER, which came by CHANNEL, :STREAM or :DATAGRAM."
  peer channel message)

(defstruct notice
  "A lifecycle event.  KIND is :CONNECTED, :DISCONNECTED, or :REFUSED; a
refusal's DETAIL is the condition or a keyword such as :FOREIGN-WORLD."
  kind peer detail)

;;;; Peers

(defstruct (peer (:constructor %make-peer))
  "One connection as its endpoint sees it.  Only the worker writes it."
  (id 0)
  endpoint
  (session 0)
  address
  port
  connection
  (decoder (make-frame-decoder))
  datagram-address
  (last-datagram nil)
  (heard nil)
  (last-probe 0)
  (welcomed nil)
  (open-p t))

(defmethod print-object ((peer peer) stream)
  (print-unreadable-object (peer stream :type t)
    (format stream "~D ~A~@[:~A~]~:[ closed~;~]" (peer-id peer)
            (peer-address peer) (peer-port peer) (peer-open-p peer))))

(defun datagrams-fresh-p (peer)
  (let ((last (peer-last-datagram peer)))
    (and last (< (- (now) last) +datagram-freshness+))))

(defun peer-datagram-p (peer)
  "True when unreliable messages to PEER currently travel by datagram."
  (and (peer-open-p peer) (peer-datagram-address peer) (peer-heard peer)
       (datagrams-fresh-p peer)))

;;;; Transports

(defclass transport () ()
  (:documentation "What a worker drives: sockets, or a loopback network."))

(defgeneric transport-open (transport endpoint)
  (:documentation "Listen or connect.  Return the local port, if any."))
(defgeneric transport-poll (transport endpoint timeout)
  (:documentation "Wait at most TIMEOUT seconds for input or a wake, and feed
whatever arrived to the endpoint."))
(defgeneric transport-wake (transport)
  (:documentation "Make a waiting TRANSPORT-POLL return.  Any thread."))
(defgeneric transport-send-stream (transport peer octets))
(defgeneric transport-send-datagram (transport endpoint address octets)
  (:documentation "Send OCTETS to ADDRESS; a lost datagram is not an error."))
(defgeneric transport-close-peer (transport peer)
  (:documentation "Flush what is queued for PEER, then close it."))
(defgeneric transport-close (transport endpoint))
(defgeneric transport-tick (transport endpoint now)
  (:method (transport endpoint now)
    (declare (ignore transport endpoint now))))

;;;; Endpoints

(defclass endpoint ()
  ((role :initarg :role :reader endpoint-role)
   (name :initarg :name :reader endpoint-name)
   (transport :initarg :transport :reader endpoint-transport)
   (target :initarg :target :initform nil :reader endpoint-target)
   (datagrams :initarg :datagrams :initform t :reader endpoint-datagrams)
   (world-tag :initarg :world-tag :accessor endpoint-world-tag
              :documentation "Stamped on what this endpoint sends and checked
on what it receives.  A guest adopts its welcome's tag.")
   (limits :initarg :limits :accessor endpoint-limits
           :documentation "Decoding limits.  A guest adopts the vocabulary
size of its welcome, a host that of the welcome it sends.")
   (mailbox :initform (sb-concurrency:make-mailbox :name "wire arrivals")
            :reader endpoint-mailbox)
   (queue :initform (sb-concurrency:make-mailbox :name "wire commands")
          :reader endpoint-queue)
   (peers :initform '() :accessor %endpoint-peers)
   (next-peer-id :initform 0 :accessor endpoint-next-peer-id)
   (random :initform (make-random-state t) :reader endpoint-random)
   (state :initform :starting :accessor endpoint-state)
   (last-error :initform nil :accessor endpoint-last-error)
   (port :initform nil :accessor endpoint-port)
   (ready :initform (sb-thread:make-semaphore :name "wire endpoint ready")
          :reader endpoint-ready)
   (stopping :initform nil :accessor endpoint-stopping)
   (thread :initform nil :accessor endpoint-thread))
  (:documentation "A host or guest whose worker owns every socket."))

(defmethod print-object ((endpoint endpoint) stream)
  (print-unreadable-object (endpoint stream :type t)
    (format stream "~(~A~) ~A ~(~A~)~@[ port ~D~]" (endpoint-role endpoint)
            (endpoint-name endpoint) (endpoint-state endpoint)
            (endpoint-port endpoint))))

(defun endpoint-peers (endpoint)
  "The open peers: every guest of a host, or a guest's host."
  (copy-list (%endpoint-peers endpoint)))

(defun endpoint-running-p (endpoint)
  (let ((thread (endpoint-thread endpoint)))
    (and thread (sb-thread:thread-alive-p thread))))

(defun post (endpoint item)
  (sb-concurrency:send-message (endpoint-mailbox endpoint) item))

(defun drain-mailbox (endpoint)
  "Every arrival and notice posted since the last drain, oldest first.
Never blocks and never touches the transport."
  (sb-concurrency:receive-pending-messages (endpoint-mailbox endpoint)))

(defun next-mailbox-item (endpoint &key (timeout 1))
  "The oldest mailbox item, waiting at most TIMEOUT seconds; NIL if none."
  (values (sb-concurrency:receive-message (endpoint-mailbox endpoint)
                                          :timeout timeout)))

(defun command (endpoint &rest command)
  (sb-concurrency:send-message (endpoint-queue endpoint) command)
  (transport-wake (endpoint-transport endpoint)))

;;;; The game's side

(defun send-message (endpoint message &optional peer)
  "Send MESSAGE to PEER, or to every open peer.  It is stamped with the
endpoint's world tag and each peer's session and encoded here, so an
unencodable value signals on the caller; the worker does the I/O.
Reliable messages take the stream; unreliable ones take a datagram when the
path is open and the stream otherwise."
  (let ((peers (if peer (list peer) (%endpoint-peers endpoint)))
        (limits (endpoint-limits endpoint)))
    (when (welcome-message-p message)
      (let ((adopted (copy-wire-limits limits)))
        (setf (wire-limits-vocabulary-size adopted)
              (length (welcome-message-vocabulary message))
              (endpoint-limits endpoint) adopted
              limits adopted)))
    (dolist (peer peers peers)
      (command endpoint :send peer
               (encode-message (stamp-message
                                message
                                :world-tag (endpoint-world-tag endpoint)
                                :session (peer-session peer))
                               :limits limits)
               (message-channel message)))))

(defun close-peer (endpoint peer)
  "Flush what is queued for PEER and close its connection."
  (command endpoint :close peer)
  peer)

(defun start-endpoint (endpoint wait)
  (setf (endpoint-thread endpoint)
        (sb-thread:make-thread (lambda () (run-endpoint endpoint))
                               :name (format nil "wire ~(~A~) ~A"
                                             (endpoint-role endpoint)
                                             (endpoint-name endpoint))))
  (when wait
    (unless (sb-thread:wait-on-semaphore (endpoint-ready endpoint) :timeout wait)
      (stop-endpoint endpoint)
      (error "The wire ~(~A~) ~A did not open within ~A seconds."
             (endpoint-role endpoint) (endpoint-name endpoint) wait))
    (when (eq (endpoint-state endpoint) :failed)
      (let ((condition (endpoint-last-error endpoint)))
        (join-worker endpoint 1)
        (error condition))))
  endpoint)

(defun stop-endpoint (endpoint &key (timeout 2))
  "Stop the worker: it sends what is queued, closes every connection, and
exits.  Idempotent."
  (setf (endpoint-stopping endpoint) t)
  (command endpoint :stop)
  (join-worker endpoint timeout)
  endpoint)

(defun join-worker (endpoint timeout)
  (let ((thread (endpoint-thread endpoint)))
    (when (and thread (not (eq thread sb-thread:*current-thread*)))
      (sb-thread:join-thread thread :timeout timeout :default nil))))

;;;; The worker's side

(defun run-endpoint (endpoint)
  (let ((transport (endpoint-transport endpoint)))
    (unwind-protect
         (progn
           (handler-case
               (setf (endpoint-port endpoint)
                     (transport-open transport endpoint)
                     (endpoint-state endpoint) :open)
             (error (condition)
               (setf (endpoint-last-error endpoint) condition
                     (endpoint-state endpoint) :failed)))
           (sb-thread:signal-semaphore (endpoint-ready endpoint))
           (when (eq (endpoint-state endpoint) :open)
             (loop until (endpoint-stopping endpoint)
                   do (transport-poll transport endpoint (next-timeout endpoint))
                      (run-commands endpoint)
                      (run-timers endpoint))
             ;; A bye queued just before the stop still goes out.
             (run-commands endpoint)))
      (dolist (peer (%endpoint-peers endpoint))
        (ignore-errors (transport-close-peer transport peer))
        (setf (peer-open-p peer) nil))
      (setf (%endpoint-peers endpoint) '())
      (ignore-errors (transport-close transport endpoint))
      (unless (eq (endpoint-state endpoint) :failed)
        (setf (endpoint-state endpoint) :stopped)))))

(defun next-timeout (endpoint)
  (declare (ignore endpoint))
  1/10)

(defun run-commands (endpoint)
  (loop for command = (sb-concurrency:receive-message-no-hang
                       (endpoint-queue endpoint))
        while command do (execute-command endpoint command)))

(defgeneric execute-command (endpoint command)
  (:documentation "Run one queued COMMAND on the worker."))

(defmethod execute-command (endpoint command)
  (destructuring-bind (kind &rest arguments) command
    (ecase kind
      (:stop)
      (:send (destructuring-bind (peer octets channel) arguments
               (when (peer-open-p peer)
                 (send-octets endpoint peer octets channel))))
      (:close (let ((peer (first arguments)))
                (when (peer-open-p peer)
                  (transport-close-peer (endpoint-transport endpoint) peer)
                  (peer-closed endpoint peer :closed)))))))

(defun send-octets (endpoint peer octets channel)
  (let ((transport (endpoint-transport endpoint)))
    (handler-case
        (if (and (eq channel :unreliable) (peer-datagram-p peer)
                 (<= (length octets) +max-datagram-length+))
            (transport-send-datagram transport endpoint
                                     (peer-datagram-address peer) octets)
            (transport-send-stream transport peer octets))
      (error (condition)
        (peer-closed endpoint peer condition)))))

(defun send-probe (endpoint peer)
  (setf (peer-last-probe peer) (now))
  (ignore-errors
   (transport-send-datagram
    (endpoint-transport endpoint) endpoint (peer-datagram-address peer)
    (encode-message (make-probe-message :heard (datagrams-fresh-p peer)
                                        :world-tag (endpoint-world-tag endpoint)
                                        :session (peer-session peer))))))

(defun run-timers (endpoint)
  (let ((now (now)))
    (when (endpoint-datagrams endpoint)
      (dolist (peer (%endpoint-peers endpoint))
        (when (and (peer-datagram-address peer) (peer-welcomed peer)
                   (>= (- now (peer-last-probe peer)) +probe-interval+))
          (send-probe endpoint peer))))
    (transport-tick (endpoint-transport endpoint) endpoint now)))

;;;; What the transport reports

(defun make-peer (endpoint &rest initargs)
  (apply #'%make-peer :endpoint endpoint
                      :id (incf (endpoint-next-peer-id endpoint))
                      initargs))

(defun new-session (endpoint)
  (1+ (random (1- (ash 1 64)) (endpoint-random endpoint))))

(defun peer-opened (endpoint peer)
  (setf (%endpoint-peers endpoint)
        (append (%endpoint-peers endpoint) (list peer)))
  (post endpoint (make-notice :kind :connected :peer peer)))

(defun peer-closed (endpoint peer reason)
  (when (peer-open-p peer)
    (setf (peer-open-p peer) nil
          (%endpoint-peers endpoint) (remove peer (%endpoint-peers endpoint)))
    (ignore-errors (transport-close-peer (endpoint-transport endpoint) peer))
    (post endpoint (make-notice :kind :disconnected :peer peer :detail reason))))

(defun stream-received (endpoint peer octets &key (end (length octets)))
  "OCTETS arrived on PEER's stream.  A malformed frame closes the stream,
which is out of step from then on."
  (handler-case
      (dolist (message (feed-frame-decoder (peer-decoder peer) octets
                                           :end end
                                           :limits (endpoint-limits endpoint)))
        (accept-message endpoint peer message :stream))
    (wire-error (condition)
      (post endpoint (make-notice :kind :refused :peer peer :detail condition))
      (peer-closed endpoint peer condition))))

(defun datagram-received (endpoint octets address)
  "A datagram arrived from ADDRESS.  It is matched to its peer by session;
anything else is refused and dropped."
  (handler-case
      (let* ((message (decode-message octets :limits (endpoint-limits endpoint)
                                             :channel :datagram))
             (session (message-session message))
             (peer (find-if (lambda (peer)
                              (and (peer-welcomed peer) (plusp session)
                                   (= session (peer-session peer))))
                            (%endpoint-peers endpoint))))
        (cond
          ((null peer)
           (post endpoint (make-notice :kind :refused :detail :unknown-session)))
          ((not (equalp (message-world-tag message) (endpoint-world-tag endpoint)))
           (post endpoint (make-notice :kind :refused :peer peer
                                       :detail :foreign-world)))
          (t
           (when (eq (endpoint-role endpoint) :host)
             (setf (peer-datagram-address peer) address))
           (let ((fresh (datagrams-fresh-p peer)))
             (setf (peer-last-datagram peer) (now))
             (if (probe-message-p message)
                 (progn
                   (setf (peer-heard peer) (probe-message-heard message))
                   ;; Answer at once while either side has not yet heard the
                   ;; other, so the path opens in one round trip.
                   (when (or (not fresh) (not (probe-message-heard message)))
                     (send-probe endpoint peer)))
                 (post endpoint (make-arrival :peer peer :channel :datagram
                                              :message message)))))))
    (wire-error (condition)
      (post endpoint (make-notice :kind :refused :detail condition)))))

(defun accept-message (endpoint peer message channel)
  "Check MESSAGE's session and world against PEER and post it."
  (let ((session (message-session message)))
    (flet ((refuse (detail)
             (post endpoint (make-notice :kind :refused :peer peer :detail detail))
             (return-from accept-message nil)))
      (ecase (endpoint-role endpoint)
        (:host
         (if (hello-message-p message)
             (unless (zerop session) (refuse :session))
             (unless (= session (peer-session peer)) (refuse :session))))
        (:guest
         (cond ((welcome-message-p message)
                (when (zerop session) (refuse :session))
                (let ((limits (copy-wire-limits (endpoint-limits endpoint))))
                  (setf (wire-limits-vocabulary-size limits)
                        (length (welcome-message-vocabulary message))
                        (endpoint-limits endpoint) limits
                        (endpoint-world-tag endpoint) (message-world-tag message)
                        (peer-session peer) session
                        (peer-welcomed peer) t)))
               ((bye-message-p message))
               ((not (peer-welcomed peer)) (refuse :before-welcome))
               ((/= session (peer-session peer)) (refuse :session)))))
      (unless (or (hello-message-p message) (welcome-message-p message)
                  (bye-message-p message)
                  (equalp (message-world-tag message) (endpoint-world-tag endpoint)))
        (refuse :foreign-world))
      (post endpoint (make-arrival :peer peer :channel channel :message message))
      (when (and (welcome-message-p message) (endpoint-datagrams endpoint)
                 (peer-datagram-address peer))
        (send-probe endpoint peer)))))
