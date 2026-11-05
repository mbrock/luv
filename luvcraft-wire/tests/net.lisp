;;;; Claims about endpoints: two of them in this image find each other and
;;;; trade reliable and unreliable messages, over a loopback network and
;;;; over real TCP and UDP sockets.

(in-package #:luvcraft.wire.tests)

(defun await (endpoint test &key (timeout 3))
  "The first mailbox item of ENDPOINT satisfying TEST within TIMEOUT
seconds, discarding the items before it; NIL if none comes."
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        for left = (/ (- deadline (get-internal-real-time))
                      internal-time-units-per-second)
        while (plusp left)
        do (let ((item (next-mailbox-item endpoint :timeout left)))
             (when (and item (funcall test item)) (return item)))))

(defun arrival-of (type)
  (lambda (item) (and (arrival-p item) (typep (arrival-message item) type))))

(defun notice-of (kind &optional detail)
  (lambda (item) (and (notice-p item) (eq kind (notice-kind item))
                      (or (null detail) (eql detail (notice-detail item))))))

(defun wait-until (predicate &key (timeout 3))
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        when (funcall predicate) return t
        while (< (get-internal-real-time) deadline)
        do (sleep 1/100)))

(defmacro with-endpoints ((&rest bindings) &body body)
  "Bind each endpoint and stop them all however BODY leaves."
  `(let ,(mapcar (lambda (binding) (list (first binding) nil)) bindings)
     (unwind-protect
          (progn ,@(mapcar (lambda (binding) `(setf ,@binding)) bindings)
                 ,@body)
       (dolist (endpoint (list ,@(mapcar #'first bindings)))
         (when endpoint (stop-endpoint endpoint))))))

(defun join (host guest)
  "Run the join handshake (#2SGDU2) and prove the reliable stream both ways.
Return the host's peer for GUEST and the guest's peer for HOST."
  (let* ((connected (await host (notice-of :connected)))
         (host-peer (and connected (notice-peer connected)))
         (guest-peer (first (endpoint-peers guest))))
    (true host-peer)
    (true guest-peer)
    (send-message guest (make-hello-message :identity "ida" :name "Ida"))
    (let ((hello (await host (arrival-of 'hello-message))))
      (true hello)
      (true (eq host-peer (arrival-peer hello)))
      (true (eq :stream (arrival-channel hello)))
      (true (zerop (message-session (arrival-message hello))))
      (true (equal "Ida" (hello-message-name (arrival-message hello)))))
    (send-message host (sample 'welcome-message) host-peer)
    (let ((welcome (await guest (arrival-of 'welcome-message))))
      (true welcome)
      (group (context "the guest adopts the host's world, session, and vocabulary")
        (true (equalp (endpoint-world-tag host) (endpoint-world-tag guest)))
        (true (= (peer-session host-peer) (peer-session guest-peer)))
        (true (plusp (peer-session guest-peer)))
        (true (= 4 (wire-limits-vocabulary-size (endpoint-limits guest))))
        (true (equalp (welcome-message-edits (sample 'welcome-message))
                      (welcome-message-edits (arrival-message welcome))))))
    (send-message guest (make-chat-message :speaker "Ida" :text "hej"))
    (let ((chat (await host (arrival-of 'chat-message))))
      (true (and chat (equal "hej" (chat-message-text (arrival-message chat))))))
    (send-message host (sample 'events-message) host-peer)
    (let ((events (await guest (arrival-of 'events-message))))
      (true (and events
                 (equalp (events-message-events (sample 'events-message))
                         (events-message-events (arrival-message events))))))
    (values host-peer guest-peer)))

(defun trade-snapshots (host guest host-peer guest-peer channel)
  "Send a pose from GUEST and an entity snapshot from HOST, and check both
arrive by CHANNEL."
  (when (eq channel :datagram)
    (group (context "the datagram path opens on both ends")
      (true (wait-until (lambda () (and (peer-datagram-p host-peer)
                                        (peer-datagram-p guest-peer)))))))
  (send-message guest (sample 'pose-message))
  (let ((pose (await host (arrival-of 'pose-message))))
    (true pose)
    (when pose
      (true (eq channel (arrival-channel pose)))
      (true (eq host-peer (arrival-peer pose)))
      (true (equalp (pose-message-pose (sample 'pose-message))
                    (pose-message-pose (arrival-message pose))))))
  (send-message host (sample 'entity-snapshot-message) host-peer)
  (let ((snapshot (await guest (arrival-of 'entity-snapshot-message))))
    (true snapshot)
    (when snapshot
      (true (eq channel (arrival-channel snapshot)))
      (true (equalp (entity-snapshot-message-snapshot
                     (sample 'entity-snapshot-message))
                    (entity-snapshot-message-snapshot
                     (arrival-message snapshot)))))))

(defun part (host guest host-peer guest-peer)
  "The guest says bye and closes; the host hears both."
  (send-message guest (make-bye-message :reason :leaving) guest-peer)
  (close-peer guest guest-peer)
  (true (await host (arrival-of 'bye-message)))
  (true (await host (notice-of :disconnected)))
  (true (wait-until (lambda () (not (member host-peer (endpoint-peers host)))))))

(define-test loopback-endpoints-find-each-other-by-name
  (let ((network (make-loopback-network)))
    (with-endpoints ((host (start-host :name "Alice" :world-tag (tag 3)
                                       :loopback network))
                     (guest (start-guest "Alice" :name "Bob" :loopback network)))
      (true (equal '("Alice") (loopback-hosts network)))
      (multiple-value-bind (host-peer guest-peer) (join host guest)
        (trade-snapshots host guest host-peer guest-peer :datagram)
        (part host guest host-peer guest-peer)))
    (group (context "a stopped host leaves the network")
      (true (null (loopback-hosts network))))
    (group (context "joining a name nobody hosts fails on the caller")
      (fail (start-guest "Nobody" :loopback network)))))

(define-test loopback-snapshots-fall-back-to-the-stream
  (let ((network (make-loopback-network :datagrams nil)))
    (with-endpoints ((host (start-host :name "Alice" :world-tag (tag 3)
                                       :loopback network))
                     (guest (start-guest "Alice" :name "Bob" :loopback network)))
      (multiple-value-bind (host-peer guest-peer) (join host guest)
        (sleep 0.6)
        (false (peer-datagram-p host-peer))
        (false (peer-datagram-p guest-peer))
        (trade-snapshots host guest host-peer guest-peer :stream)))))

(defun free-udp-port ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :datagram :protocol :udp)))
    (unwind-protect
         (progn (sb-bsd-sockets:socket-bind socket #(0 0 0 0) 0)
                (nth-value 1 (sb-bsd-sockets:socket-name socket)))
      (sb-bsd-sockets:socket-close socket))))

(defun broadcast-undeliverable ()
  "NIL when a datagram broadcast to 255.255.255.255 comes back to this
machine, else why not.

A host firewall may drop inbound broadcasts to ports it has not opened --
NixOS's default one does -- and then no beacon can be heard here at all.
macOS may refuse the send itself with EHOSTUNREACH, for instance where the
process may not use the local network."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :datagram :protocol :udp))
        (sender (make-instance 'sb-bsd-sockets:inet-socket
                               :type :datagram :protocol :udp))
        (probe (map '(vector (unsigned-byte 8)) #'char-code "luvcraft-wire probe"))
        (buffer (make-array 64 :element-type '(unsigned-byte 8))))
    (unwind-protect
         (progn
           (sb-bsd-sockets:socket-bind listener #(0 0 0 0) 0)
           (setf (sb-bsd-sockets:non-blocking-mode listener) t
                 (sb-bsd-sockets:sockopt-broadcast sender) t)
           (handler-case
               (progn
                 (sb-bsd-sockets:socket-send
                  sender probe (length probe)
                  :address (list #(255 255 255 255)
                                 (nth-value 1 (sb-bsd-sockets:socket-name listener))))
                 (unless (wait-until
                          (lambda ()
                            (let ((length (nth-value 1 (sb-bsd-sockets:socket-receive
                                                        listener buffer nil))))
                              (and length (= length (length probe)))))
                          :timeout 1)
                   "this machine drops its own UDP broadcasts, probably in a host firewall; open the announce port to discover hosts here"))
             (sb-bsd-sockets:socket-error (condition)
               (format nil "this machine refuses to send a UDP broadcast: ~A" condition))))
      (sb-bsd-sockets:socket-close listener)
      (sb-bsd-sockets:socket-close sender))))

(define-test socket-endpoints-find-each-other-by-broadcast-and-by-name
  (let ((reason (broadcast-undeliverable)))
    (if (null reason)
        (find-endpoints-by-broadcast-and-by-name)
        (progn
          (format t "~&Skipping LAN discovery: ~A.~%" reason)
          (parachute:skip reason
            (true (find-endpoints-by-broadcast-and-by-name)))))))

(defun find-endpoints-by-broadcast-and-by-name ()
  (let* ((announce-port (free-udp-port))
         (discovery (start-discovery :port announce-port)))
    (unwind-protect
         (with-endpoints ((host (start-host :name "Mikael" :port 0
                                            :world-tag (tag 5)
                                            :announce "Little world"
                                            :announce-port announce-port)))
           (let ((heard nil))
             (group (context "the host is heard by broadcast")
               (true (wait-until
                      (lambda ()
                        (setf heard (find (endpoint-port host)
                                          (discovered-hosts discovery)
                                          :key #'discovered-host-port)))))
               (when heard
                 (true (equal "Mikael" (discovered-host-name heard)))
                 (true (equal "Little world" (discovered-host-world heard)))
                 (true (equalp (tag 5) (discovered-host-world-tag heard)))))
             (when heard
               (with-endpoints ((first-guest (start-guest
                                              (discovered-host-target heard)
                                              :name "Ida")))
                 (group (context "a guest joined by its beacon uses datagrams")
                   (multiple-value-bind (host-peer guest-peer)
                       (join host first-guest)
                     (trade-snapshots host first-guest host-peer guest-peer
                                      :datagram)
                     (with-endpoints ((second-guest
                                       (start-guest (format nil "localhost:~D"
                                                            (endpoint-port host))
                                                    :name "Nils"
                                                    :datagrams nil)))
                       (group (context "a guest joined by name without UDP uses the stream")
                         (multiple-value-bind (host-peer-2 guest-peer-2)
                             (join host second-guest)
                           (trade-snapshots host second-guest host-peer-2
                                            guest-peer-2 :stream)
                           (group (context "one send reaches every guest by its own path")
                             (send-message host (sample 'pose-message))
                             (let ((one (await first-guest (arrival-of 'pose-message)))
                                   (two (await second-guest (arrival-of 'pose-message))))
                               (true (and one (eq :datagram (arrival-channel one))))
                               (true (and two (eq :stream (arrival-channel two))))))
                           (part host second-guest host-peer-2 guest-peer-2))))
                     (part host first-guest host-peer guest-peer)))))))
      (stop-discovery discovery))))

(define-test socket-endpoints-refuse-what-is-not-their-world
  (with-endpoints ((host (start-host :name "Mikael" :port 0 :world-tag (tag 5)))
                   (guest nil))
    (group (context "a stream that is not the protocol is refused and closed")
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :stream :protocol :tcp)))
        (unwind-protect
             (progn
               (sb-bsd-sockets:socket-connect socket #(127 0 0 1) (endpoint-port host))
               (true (await host (notice-of :connected)))
               (let ((request (map '(vector (unsigned-byte 8)) #'char-code
                                   "GET / HTTP/1.1")))
                 (sb-bsd-sockets:socket-send socket request (length request)))
               (let ((refusal (await host (notice-of :refused))))
                 (true (and refusal (typep (notice-detail refusal)
                                           'malformed-message))))
               (true (await host (notice-of :disconnected))))
          (sb-bsd-sockets:socket-close socket))))
    (group (context "a stray datagram is refused")
      (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                   :type :datagram :protocol :udp))
            (junk (make-array 8 :element-type '(unsigned-byte 8)
                                :initial-element 7)))
        (unwind-protect
             (sb-bsd-sockets:socket-send socket junk 8
                                         :address (list #(127 0 0 1)
                                                        (endpoint-port host)))
          (sb-bsd-sockets:socket-close socket))
        (let ((refusal (await host (notice-of :refused))))
          (true (and refusal (null (notice-peer refusal)))))))
    (group (context "a welcomed guest speaking for another world is refused")
      (setf guest (start-guest (format nil "127.0.0.1:~D" (endpoint-port host))))
      (multiple-value-bind (host-peer guest-peer) (join host guest)
        (declare (ignore host-peer))
        (setf (endpoint-world-tag guest) (tag 99))
        (send-message guest (make-chat-message :text "from elsewhere") guest-peer)
        (true (await host (notice-of :refused :foreign-world)))))
    (group (context "joining a port nobody listens on fails on the caller")
      (let ((port (endpoint-port host)))
        (stop-endpoint host)
        (fail (start-guest (format nil "127.0.0.1:~D" port)))))))
