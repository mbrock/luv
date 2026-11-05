;;;; The socket transport and LAN discovery.
;;;;
;;;; A host listens on one TCP port and binds one UDP socket to the same
;;;; port number; while announcing, it broadcasts a beacon every half second,
;;;; as moppe's companion_link does.  A guest connects to host[:port] and
;;;; keeps an ephemeral UDP socket.  Every socket is non-blocking and waited
;;;; on with poll(2) beside a wake pipe, all on the endpoint's worker.

(in-package #:luvcraft.wire.net)

;;;; poll(2)

(sb-alien:define-alien-type nil
    (sb-alien:struct pollfd
                     (fd sb-alien:int)
                     (events sb-alien:short)
                     (revents sb-alien:short)))

(defconstant +pollin+ 1)
(defconstant +pollout+ 4)

(defun poll-fds (requests timeout)
  "REQUESTS is a list of (FD . EVENTS).  Wait at most TIMEOUT seconds and
return the list of ready (FD . REVENTS)."
  (let* ((count (length requests))
         (fds (sb-alien:make-alien (sb-alien:struct pollfd) (max count 1))))
    (unwind-protect
         (progn
           (loop for (fd . events) in requests
                 for index from 0
                 do (let ((entry (sb-alien:deref fds index)))
                      (setf (sb-alien:slot entry 'fd) fd
                            (sb-alien:slot entry 'events) events
                            (sb-alien:slot entry 'revents) 0)))
           (let ((result (sb-alien:alien-funcall
                          (sb-alien:extern-alien
                           "poll" (function sb-alien:int
                                            (* (sb-alien:struct pollfd))
                                            sb-alien:unsigned-long sb-alien:int))
                          fds count (max 0 (round (* 1000 timeout))))))
             (when (plusp result)
               (loop for index below count
                     for entry = (sb-alien:deref fds index)
                     for revents = (sb-alien:slot entry 'revents)
                     unless (zerop revents)
                       collect (cons (sb-alien:slot entry 'fd) revents)))))
      (sb-alien:free-alien fds))))

;;;; Addresses

(defun parse-address (host)
  "HOST as an IPv4 address vector, resolving a name when it is not numeric."
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

(defun parse-target (target)
  "TARGET, \"host[:port]\", as an address vector and a port."
  (let ((colon (position #\: target :from-end t)))
    (values (parse-address (subseq target 0 colon))
            (if colon
                (parse-integer target :start (1+ colon))
                +default-port+))))

(defun dotted (address)
  (format nil "~{~D~^.~}" (coerce address 'list)))

;;;; Sockets

(defun udp-socket (port &key broadcast)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :datagram :protocol :udp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (when broadcast (setf (sb-bsd-sockets:sockopt-broadcast socket) t))
    (sb-bsd-sockets:socket-bind socket #(0 0 0 0) port)
    (setf (sb-bsd-sockets:non-blocking-mode socket) t)
    socket))

(defun socket-fd (socket) (sb-bsd-sockets:socket-file-descriptor socket))

(defstruct wake-pipe
  (read-fd 0) (write-fd 0))

(defun open-wake-pipe ()
  (multiple-value-bind (read write) (sb-posix:pipe)
    (dolist (fd (list read write))
      (sb-posix:fcntl fd sb-posix:f-setfl
                      (logior sb-posix:o-nonblock
                              (sb-posix:fcntl fd sb-posix:f-getfl))))
    (make-wake-pipe :read-fd read :write-fd write)))

(defun drain-wake-pipe (pipe)
  (let ((buffer (make-array 64 :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (buffer)
      (loop while (eql 64 (sb-unix:unix-read (wake-pipe-read-fd pipe)
                                             (sb-sys:vector-sap buffer) 64))))))

(defclass socket-transport (transport)
  ((port :initarg :port :initform +default-port+ :reader socket-transport-port)
   (announce :initarg :announce :initform nil :reader socket-transport-announce
             :documentation "A beacon (world name) to broadcast, or NIL.")
   (announce-port :initarg :announce-port :initform +default-announce-port+
                  :reader socket-transport-announce-port)
   (broadcast-addresses :initarg :broadcast-addresses
                        :initform '(#(255 255 255 255))
                        :reader socket-transport-broadcast-addresses)
   (connect-timeout :initarg :connect-timeout :initform 3
                    :reader socket-transport-connect-timeout)
   (listener :initform nil :accessor socket-transport-listener)
   (udp :initform nil :accessor socket-transport-udp)
   (pipe :initform (open-wake-pipe) :reader socket-transport-pipe)
   (buffer :initform (make-array 65536 :element-type '(unsigned-byte 8))
           :reader socket-transport-buffer)
   (last-beacon :initform 0 :accessor socket-transport-last-beacon)
   (beacon-failure :initform nil :accessor socket-transport-beacon-failure
                   :documentation "Why the last beacon could not be sent, or NIL.")))

(defstruct socket-connection
  socket
  (outgoing (make-array 0 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0)))

(defmethod transport-open ((transport socket-transport) endpoint)
  (ecase (endpoint-role endpoint)
    (:host
     (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                    :type :stream :protocol :tcp)))
       (setf (socket-transport-listener transport) listener)
       (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
       (sb-bsd-sockets:socket-bind listener #(0 0 0 0)
                                   (socket-transport-port transport))
       (sb-bsd-sockets:socket-listen listener 8)
       (setf (sb-bsd-sockets:non-blocking-mode listener) t)
       (let ((port (nth-value 1 (sb-bsd-sockets:socket-name listener))))
         (setf (socket-transport-udp transport) (udp-socket port :broadcast t))
         port)))
    (:guest
     (multiple-value-bind (address port) (parse-target (endpoint-target endpoint))
       (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                    :type :stream :protocol :tcp)))
         (handler-bind ((error (lambda (condition)
                                 (declare (ignore condition))
                                 (ignore-errors
                                  (sb-bsd-sockets:socket-close socket)))))
           (setf (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
           (handler-case
               (sb-ext:with-timeout (socket-transport-connect-timeout transport)
                 (sb-bsd-sockets:socket-connect socket address port))
             (sb-ext:timeout ()
               (error "No answer from ~A within ~D seconds."
                      (endpoint-target endpoint)
                      (socket-transport-connect-timeout transport))))
           (setf (sb-bsd-sockets:non-blocking-mode socket) t)
           (when (endpoint-datagrams endpoint)
             (setf (socket-transport-udp transport) (udp-socket 0)))
           (peer-opened endpoint
                        (make-peer endpoint
                                   :address (dotted address) :port port
                                   :connection (make-socket-connection
                                                :socket socket)
                                   :datagram-address (cons address port)))
           (nth-value 1 (sb-bsd-sockets:socket-name socket))))))))

(defmethod transport-wake ((transport socket-transport))
  (let ((octet (make-array 1 :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (octet)
      (sb-unix:unix-write (wake-pipe-write-fd (socket-transport-pipe transport))
                          (sb-sys:vector-sap octet) 0 1))))

(defun peer-socket (peer)
  (socket-connection-socket (peer-connection peer)))

(defmethod transport-poll ((transport socket-transport) endpoint timeout)
  (let* ((pipe (socket-transport-pipe transport))
         (listener (socket-transport-listener transport))
         (udp (socket-transport-udp transport))
         (peers (%endpoint-peers endpoint))
         (requests
           (append
            (list (cons (wake-pipe-read-fd pipe) +pollin+))
            (when listener (list (cons (socket-fd listener) +pollin+)))
            (when udp (list (cons (socket-fd udp) +pollin+)))
            (loop for peer in peers
                  collect (cons (socket-fd (peer-socket peer))
                                (if (plusp (length (socket-connection-outgoing
                                                    (peer-connection peer))))
                                    (logior +pollin+ +pollout+)
                                    +pollin+)))))
         (ready (poll-fds requests timeout)))
    (loop for (fd . revents) in ready
          do (cond
               ((= fd (wake-pipe-read-fd pipe)) (drain-wake-pipe pipe))
               ((and listener (= fd (socket-fd listener)))
                (accept-guests transport endpoint))
               ((and udp (= fd (socket-fd udp)))
                (receive-datagrams transport endpoint))
               (t
                (let ((peer (find fd peers
                                  :key (lambda (peer)
                                         (let ((socket (peer-socket peer)))
                                           (and socket (socket-fd socket)))))))
                  (when (and peer (peer-open-p peer)
                             (logtest revents +pollout+))
                    (flush-connection endpoint peer))
                  ;; Input, a hang-up, or an error: the read reports which.
                  (when (and peer (peer-open-p peer)
                             (logtest revents (lognot +pollout+)))
                    (receive-stream transport endpoint peer))))))))

(defun accept-guests (transport endpoint)
  (loop for socket = (handler-case
                         (sb-bsd-sockets:socket-accept
                          (socket-transport-listener transport))
                       (sb-bsd-sockets:interrupted-error () nil))
        while socket
        do (setf (sb-bsd-sockets:non-blocking-mode socket) t
                 (sb-bsd-sockets:sockopt-tcp-nodelay socket) t)
           (multiple-value-bind (address port) (sb-bsd-sockets:socket-peername socket)
             (peer-opened endpoint
                          (make-peer endpoint
                                     :address (dotted address) :port port
                                     :session (new-session endpoint)
                                     :welcomed t
                                     :connection (make-socket-connection
                                                  :socket socket))))))

(defun receive-stream (transport endpoint peer)
  (let ((buffer (socket-transport-buffer transport)))
    (handler-case
        (let ((length (nth-value 1 (sb-bsd-sockets:socket-receive
                                    (peer-socket peer) buffer nil))))
          (cond ((null length))
                ((zerop length) (peer-closed endpoint peer :end-of-stream))
                (t (stream-received endpoint peer buffer :end length))))
      (sb-bsd-sockets:socket-error (condition)
        (peer-closed endpoint peer condition)))))

(defun receive-datagrams (transport endpoint)
  (let ((buffer (socket-transport-buffer transport)))
    (loop
      (multiple-value-bind (vector length address port)
          (handler-case (sb-bsd-sockets:socket-receive
                         (socket-transport-udp transport) buffer nil)
            (sb-bsd-sockets:socket-error () nil))
        (declare (ignore vector))
        (unless length (return))
        (datagram-received endpoint (subseq buffer 0 length)
                           (cons (copy-seq address) port))))))

(defun flush-connection (endpoint peer)
  "Write as much of PEER's outgoing octets as the socket takes now."
  (let ((outgoing (socket-connection-outgoing (peer-connection peer))))
    (handler-case
        (loop while (plusp (length outgoing))
              do (let ((sent (sb-bsd-sockets:socket-send
                              (peer-socket peer)
                              (subseq outgoing 0 (min 65536 (length outgoing)))
                              nil)))
                   (unless (and sent (plusp sent)) (return))
                   (replace outgoing outgoing :start2 sent)
                   (decf (fill-pointer outgoing) sent)))
      (sb-bsd-sockets:socket-error (condition)
        (peer-closed endpoint peer condition)))))

(defmethod transport-send-stream ((transport socket-transport) peer octets)
  (let ((outgoing (socket-connection-outgoing (peer-connection peer))))
    (loop for octet across octets do (vector-push-extend octet outgoing))
    (flush-connection (peer-endpoint peer) peer)))

(defmethod transport-send-datagram ((transport socket-transport) endpoint address octets)
  (declare (ignore endpoint))
  (let ((udp (socket-transport-udp transport)))
    (when udp
      (handler-case
          (sb-bsd-sockets:socket-send udp octets (length octets)
                                      :address (list (car address) (cdr address)))
        (sb-bsd-sockets:socket-error () nil)))))

(defmethod transport-close-peer ((transport socket-transport) peer)
  (let ((connection (peer-connection peer)))
    (when (socket-connection-socket connection)
      ;; Give queued octets, such as a bye, a quarter second to leave.
      (loop with deadline = (+ (now) 1/4)
            while (and (plusp (length (socket-connection-outgoing connection)))
                       (< (now) deadline))
            do (ignore-errors
                (poll-fds (list (cons (socket-fd (peer-socket peer)) +pollout+))
                          1/20))
               (let ((outgoing (socket-connection-outgoing connection)))
                 (handler-case
                     (let ((sent (sb-bsd-sockets:socket-send
                                  (peer-socket peer) (coerce outgoing
                                                             '(simple-array (unsigned-byte 8) (*)))
                                  nil)))
                       (when (and sent (plusp sent))
                         (replace outgoing outgoing :start2 sent)
                         (decf (fill-pointer outgoing) sent)))
                   (error () (setf (fill-pointer outgoing) 0)))))
      (ignore-errors (sb-bsd-sockets:socket-close (peer-socket peer)))
      (setf (socket-connection-socket connection) nil))))

(defmethod transport-close ((transport socket-transport) endpoint)
  (declare (ignore endpoint))
  (dolist (socket (list (socket-transport-listener transport)
                        (socket-transport-udp transport)))
    (when socket (ignore-errors (sb-bsd-sockets:socket-close socket))))
  (let ((pipe (socket-transport-pipe transport)))
    (ignore-errors (sb-posix:close (wake-pipe-read-fd pipe)))
    (ignore-errors (sb-posix:close (wake-pipe-write-fd pipe)))))

(defmethod transport-tick ((transport socket-transport) endpoint now)
  (let ((world (socket-transport-announce transport)))
    (when (and world (>= (- now (socket-transport-last-beacon transport)) 1/2))
      (setf (socket-transport-last-beacon transport) now)
      (let ((octets (encode-message
                     (make-beacon-message :name (endpoint-name endpoint)
                                          :port (endpoint-port endpoint)
                                          :world world
                                          :world-tag (endpoint-world-tag endpoint)))))
        ;; A broadcast can fail for a while -- macOS answers EHOSTUNREACH
        ;; while an interface comes or goes, or where local network access
        ;; is denied.  Say so once, and try again on the next beacon.
        (let ((failure
                (handler-case
                    (dolist (address (socket-transport-broadcast-addresses transport))
                      (sb-bsd-sockets:socket-send
                       (socket-transport-udp transport) octets (length octets)
                       :address (list address
                                      (socket-transport-announce-port transport))))
                  (sb-bsd-sockets:socket-error (condition) condition))))
          (when (and failure (not (socket-transport-beacon-failure transport)))
            (post endpoint (make-notice :kind :unannounced :detail failure)))
          (setf (socket-transport-beacon-failure transport) failure))))))

;;;; Discovery

(defstruct discovered-host
  "A host heard on the LAN."
  address port name world world-tag heard)

(defun discovered-host-target (host)
  "HOST as a \"host:port\" target for START-GUEST."
  (format nil "~A:~D" (discovered-host-address host) (discovered-host-port host)))

(defclass discovery ()
  ((port :initarg :port :reader discovery-port)
   (lock :initform (sb-thread:make-mutex :name "wire discovery")
         :reader discovery-lock)
   (hosts :initform (make-hash-table :test #'equal) :reader discovery-hosts)
   (stopping :initform nil :accessor discovery-stopping)
   (thread :initform nil :accessor discovery-thread))
  (:documentation "A worker listening for host beacons."))

(defun start-discovery (&key (port +default-announce-port+))
  "Listen for host beacons on PORT until STOP-DISCOVERY.  The socket is bound
before this returns, so no beacon sent afterwards is missed."
  (let* ((discovery (make-instance 'discovery :port port))
         (socket (udp-socket port)))
    (setf (discovery-thread discovery)
          (sb-thread:make-thread (lambda () (run-discovery discovery socket))
                                 :name "wire discovery"))
    discovery))

(defun run-discovery (discovery socket)
  (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (loop until (discovery-stopping discovery)
               do (when (poll-fds (list (cons (socket-fd socket) +pollin+)) 1/10)
                    (loop
                      (multiple-value-bind (vector length address)
                          (handler-case (sb-bsd-sockets:socket-receive socket buffer nil)
                            (sb-bsd-sockets:socket-error () nil))
                        (declare (ignore vector))
                        (unless length (return))
                        (let ((beacon (ignore-errors
                                       (decode-message (subseq buffer 0 length)
                                                       :channel :datagram))))
                          (when (beacon-message-p beacon)
                            (let ((host (make-discovered-host
                                         :address (dotted address)
                                         :port (beacon-message-port beacon)
                                         :name (beacon-message-name beacon)
                                         :world (beacon-message-world beacon)
                                         :world-tag (message-world-tag beacon)
                                         :heard (now))))
                              (sb-thread:with-mutex ((discovery-lock discovery))
                                (setf (gethash (discovered-host-target host)
                                               (discovery-hosts discovery))
                                      host)))))))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun discovered-hosts (discovery &key (within 2))
  "The hosts heard in the last WITHIN seconds, by name."
  (let ((now (now)))
    (sb-thread:with-mutex ((discovery-lock discovery))
      (sort (loop for host being the hash-values of (discovery-hosts discovery)
                  when (< (- now (discovered-host-heard host)) within)
                    collect host)
            #'string< :key #'discovered-host-name))))

(defun stop-discovery (discovery)
  (setf (discovery-stopping discovery) t)
  (sb-thread:join-thread (discovery-thread discovery) :default nil :timeout 1)
  discovery)

(defun find-hosts (&key (seconds 1) (port +default-announce-port+))
  "Listen for SECONDS and return the hosts heard."
  (let ((discovery (start-discovery :port port)))
    (unwind-protect (progn (sleep seconds) (discovered-hosts discovery))
      (stop-discovery discovery))))
