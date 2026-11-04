;;;; The loopback transport, and starting endpoints.
;;;;
;;;; A loopback network lets endpoints in one image find each other by name
;;;; and exchange the same encoded octets that sockets would carry, through
;;;; their workers' command queues.  Datagrams can be switched off to
;;;; exercise the stream fallback.

(in-package #:luvcraft.wire.net)

(defstruct (loopback-network (:constructor make-loopback-network
                                 (&key (datagrams t))))
  "Named hosts in one image.  When DATAGRAMS is NIL every datagram is lost."
  (lock (sb-thread:make-mutex :name "loopback network"))
  (hosts (make-hash-table :test #'equal))
  (datagrams t))

(defun loopback-hosts (network)
  "The names of the hosts on NETWORK."
  (sb-thread:with-mutex ((loopback-network-lock network))
    (sort (loop for name being the hash-keys of (loopback-network-hosts network)
                collect name)
          #'string<)))

(defclass loopback-transport (transport)
  ((network :initarg :network :reader loopback-transport-network)))

(defmethod transport-open ((transport loopback-transport) endpoint)
  (let ((network (loopback-transport-network transport))
        (name (endpoint-name endpoint)))
    (ecase (endpoint-role endpoint)
      (:host
       (sb-thread:with-mutex ((loopback-network-lock network))
         (when (gethash name (loopback-network-hosts network))
           (error "A loopback host named ~A already exists." name))
         (setf (gethash name (loopback-network-hosts network)) endpoint))
       nil)
      (:guest
       (let* ((target (endpoint-target endpoint))
              (host (sb-thread:with-mutex ((loopback-network-lock network))
                      (gethash target (loopback-network-hosts network)))))
         (unless (and host (eq (endpoint-state host) :open))
           (error "No loopback host named ~A." target))
         (let ((ours (make-peer endpoint :address target :datagram-address host))
               (theirs (make-peer host :address name :session (new-session host)
                                       :welcomed t)))
           (setf (peer-connection ours) theirs
                 (peer-connection theirs) ours)
           (command host :loopback-open theirs)
           (peer-opened endpoint ours)
           nil))))))

(defmethod transport-poll ((transport loopback-transport) endpoint timeout)
  (let ((command (sb-concurrency:receive-message (endpoint-queue endpoint)
                                                 :timeout timeout)))
    (when command (execute-command endpoint command))))

(defmethod transport-wake ((transport loopback-transport)))

(defmethod execute-command :around (endpoint command)
  (case (first command)
    (:loopback-open (peer-opened endpoint (second command)))
    (:loopback-stream (let ((peer (second command)))
                        (when (peer-open-p peer)
                          (stream-received endpoint peer (third command)))))
    (:loopback-datagram (destructuring-bind (octets from) (rest command)
                          (datagram-received endpoint octets from)))
    (:loopback-close (peer-closed endpoint (second command) :end-of-stream))
    (t (call-next-method))))

(defmethod transport-send-stream ((transport loopback-transport) peer octets)
  (let ((theirs (peer-connection peer)))
    (command (peer-endpoint theirs) :loopback-stream theirs octets)))

(defmethod transport-send-datagram ((transport loopback-transport) endpoint
                                    address octets)
  (when (and (loopback-network-datagrams (loopback-transport-network transport))
             (endpoint-datagrams endpoint)
             (endpoint-running-p address))
    (command address :loopback-datagram octets endpoint)))

(defmethod transport-close-peer ((transport loopback-transport) peer)
  (let ((theirs (peer-connection peer)))
    (when (and theirs (peer-open-p theirs))
      (command (peer-endpoint theirs) :loopback-close theirs))))

(defmethod transport-close ((transport loopback-transport) endpoint)
  (when (eq (endpoint-role endpoint) :host)
    (let ((network (loopback-transport-network transport)))
      (sb-thread:with-mutex ((loopback-network-lock network))
        (when (eq endpoint (gethash (endpoint-name endpoint)
                                    (loopback-network-hosts network)))
          (remhash (endpoint-name endpoint) (loopback-network-hosts network)))))))

;;;; Starting endpoints

(defun default-name ()
  (or (uiop:getenv "LUV_PLAYER_NAME") (uiop:getenv "USER")
      (ignore-errors (machine-instance)) "someone"))

(defun start-host (&key (name (default-name)) (port +default-port+)
                        (world-tag (zero-world-tag))
                        (limits *default-wire-limits*)
                        announce (announce-port +default-announce-port+)
                        (broadcast-addresses '(#(255 255 255 255)))
                        loopback (wait 5))
  "Start a host and return it once it listens.  PORT 0 picks a free port;
ENDPOINT-PORT tells which.  ANNOUNCE, a world name, broadcasts a beacon to
ANNOUNCE-PORT every half second.  LOOPBACK, a loopback network, hosts there
under NAME instead of on sockets."
  (start-endpoint
   (make-instance 'endpoint
                  :role :host :name name :world-tag (world-tag world-tag)
                  :limits limits
                  :transport
                  (if loopback
                      (make-instance 'loopback-transport :network loopback)
                      (make-instance 'socket-transport
                                     :port port :announce announce
                                     :announce-port announce-port
                                     :broadcast-addresses broadcast-addresses)))
   wait))

(defun start-guest (target &key (name (default-name))
                                (limits *default-wire-limits*)
                                (datagrams t) loopback (wait 5))
  "Join the host at TARGET, \"host[:port]\" (or a host's name on the
LOOPBACK network), and return the guest once connected.  The guest should
then send a HELLO-MESSAGE.  DATAGRAMS NIL keeps every message on the
stream."
  (start-endpoint
   (make-instance 'endpoint
                  :role :guest :name name :target target
                  :world-tag (zero-world-tag) :limits limits
                  :datagrams datagrams
                  :transport (if loopback
                                 (make-instance 'loopback-transport
                                                :network loopback)
                                 (make-instance 'socket-transport)))
   wait))
