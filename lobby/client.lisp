;;; A reusable application lobby radio.
;;;
;;; One worker owns the transport connection.  Applications and their render
;;; threads see only immutable semantic snapshots and retained values copied
;;; under a small mutex; they never perform socket I/O.

(in-package #:luv.lobby)

(defparameter +lobby-presence-prefix+ "luv/presence/")
(defparameter +lobby-presence-filter+ "luv/presence/+")
(defparameter +lobby-store-prefix+ "luv/store/")
(defparameter +lobby-store-filter+ "luv/store/+")
(defparameter +lobby-offline-payload+ (string #\Null))

(defvar *lobby-client-counter* 0)
(defvar *lobby-client-counter-lock*
  (sb-thread:make-mutex :name "lobby client identities"))

(defclass lobby-peer ()
  ((id :initarg :id :reader lobby-peer-id)
   (name :initarg :name :reader lobby-peer-name))
  (:documentation "One application instance heard on the current connection."))

(defstruct (lobby-snapshot
            (:constructor make-lobby-snapshot
                (&key status peers last-error revision)))
  "An immutable, connection-scoped view suitable for a frame boundary."
  status
  peers
  last-error
  revision)

(defclass lobby-transport () ()
  (:documentation "The socket-owning edge used by one LOBBY-CLIENT worker."))

(defgeneric open-lobby-transport (transport client)
  (:documentation "Open and return CLIENT's worker-owned connection."))

(defgeneric subscribe-lobby-transport (transport connection client)
  (:documentation "Subscribe CONNECTION to CLIENT's shared lobby topics."))

(defgeneric publish-lobby-transport (transport connection client payload)
  (:documentation "Publish CLIENT's retained presence PAYLOAD."))

(defgeneric next-lobby-publication (transport connection client)
  (:documentation
   "Return TOPIC and PAYLOAD for one publication, or NIL on a bounded idle poll."))

(defgeneric close-lobby-transport (transport connection client)
  (:documentation "Close CONNECTION without allowing an error to escape cleanup."))

(defclass mqtt-lobby-transport (lobby-transport)
  ((connect-timeout :initarg :connect-timeout :initform 1/4
                    :reader mqtt-lobby-connect-timeout)
   (poll-timeout :initarg :poll-timeout :initform 1/4
                 :reader mqtt-lobby-poll-timeout)
   (keep-alive :initarg :keep-alive :initform 20
               :reader mqtt-lobby-keep-alive))
  (:documentation
   "The MQTT lobby transport.  Every blocking operation is owned by the worker
and bounded tightly enough for cooperative application shutdown."))

(defclass lobby-client ()
  ((id :initarg :id :reader lobby-client-id)
   (name :initarg :name :reader lobby-client-name)
   (client-id-prefix :initarg :client-id-prefix
                     :reader lobby-client-id-prefix)
   (transport :initarg :transport :reader lobby-client-transport)
   (lock :initform (sb-thread:make-mutex :name "lobby semantic state")
         :reader lobby-client-lock)
   (wake :initform (sb-thread:make-waitqueue :name "lobby worker wake")
         :reader lobby-client-wake)
   (status :initform :stopped :accessor lobby-client-status)
   (last-error :initform nil :accessor lobby-client-last-error)
   (peers :initform (make-hash-table :test #'equal)
          :reader lobby-client-peers)
   (values :initform (make-hash-table :test #'equal)
           :reader lobby-client-values)
   (revision :initform 0 :accessor lobby-client-revision)
   (connection :initform nil :accessor lobby-client-connection)
   (stopping-p :initform nil :accessor lobby-client-stopping-p)
   (thread :initform nil :accessor lobby-client-thread))
  (:documentation
   "A restartable radio whose worker publishes connection-scoped snapshots."))

(define-condition lobby-worker-stop-timeout (error)
  ((client :initarg :client :reader lobby-worker-stop-timeout-client)
   (seconds :initarg :seconds :reader lobby-worker-stop-timeout-seconds))
  (:report
   (lambda (condition stream)
     (format stream "Lobby worker for ~A did not stop within ~,2F seconds."
             (lobby-client-name
              (lobby-worker-stop-timeout-client condition))
             (lobby-worker-stop-timeout-seconds condition)))))

(defmethod print-object ((client lobby-client) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (format stream "~A ~(~A~)" (lobby-client-name client)
            (lobby-client-status client))))

(defun lobby-topic-prefix-p (prefix topic)
  (and (<= (length prefix) (length topic))
       (string= prefix topic :end2 (length prefix))))

(defun lobby-safe-id (text)
  (string-downcase
   (with-output-to-string (stream)
     (loop for character across text
           do (write-char
               (if (alphanumericp character) character #\-)
               stream)))))

(defun default-lobby-participant-name ()
  (or (let ((name (uiop:getenv "LUV_PLAYER_NAME")))
        (and name (plusp (length name)) name))
      (let ((name (uiop:getenv "USER")))
        (and name (plusp (length name)) (string-capitalize name)))
      (ignore-errors (machine-instance))
      "someone"))

(defun next-lobby-client-number ()
  (sb-thread:with-mutex (*lobby-client-counter-lock*)
    (incf *lobby-client-counter*)))

(defun make-lobby-client
    (&key (name (default-lobby-participant-name))
          (client-id-prefix "luv")
          (transport (make-instance 'mqtt-lobby-transport)))
  "Make a stopped lobby client.  START-LOBBY-CLIENT owns all transport I/O."
  (let* ((prefix (lobby-safe-id client-id-prefix))
         (machine (lobby-safe-id (or (ignore-errors (machine-instance))
                                     "machine")))
         (id (format nil "~A-~A-~D-~D" prefix machine
                     (sb-posix:getpid) (next-lobby-client-number))))
    (make-instance 'lobby-client
                   :name name :id id :client-id-prefix prefix
                   :transport transport)))

(defun lobby-client-topic (client)
  (concatenate 'string +lobby-presence-prefix+ (lobby-client-id client)))

(defun %clear-lobby-cache (client)
  (clrhash (lobby-client-peers client))
  (clrhash (lobby-client-values client)))

(defun %publish-lobby-status
    (client status &key condition clear-cache-p)
  "Publish STATUS while CLIENT-LOCK is held."
  (let* ((detail (and condition (princ-to-string condition)))
         (changed-p
           (or (not (eq status (lobby-client-status client)))
               (not (equal detail (lobby-client-last-error client)))
               (and clear-cache-p
                    (or (plusp (hash-table-count (lobby-client-peers client)))
                        (plusp (hash-table-count
                                (lobby-client-values client))))))))
    (setf (lobby-client-status client) status
          (lobby-client-last-error client) detail)
    (when clear-cache-p (%clear-lobby-cache client))
    (when changed-p (incf (lobby-client-revision client))))
  status)

(defun publish-lobby-status
    (client status &key condition clear-cache-p)
  (sb-thread:with-mutex ((lobby-client-lock client))
    (%publish-lobby-status client status
                           :condition condition
                           :clear-cache-p clear-cache-p)))

(defun lobby-client-running-p (client)
  (and client
       (sb-thread:with-mutex ((lobby-client-lock client))
         (let ((thread (lobby-client-thread client)))
           (and thread (sb-thread:thread-alive-p thread)
                (not (lobby-client-stopping-p client)))))))

(defun lobby-client-stop-requested-p (client)
  (sb-thread:with-mutex ((lobby-client-lock client))
    (lobby-client-stopping-p client)))

(defun lobby-client-snapshot (client)
  "Copy CLIENT's small semantic state atomically; never touches its transport."
  (if client
      (sb-thread:with-mutex ((lobby-client-lock client))
        (make-lobby-snapshot
         :status (lobby-client-status client)
         :peers
         (sort
          (loop for peer being the hash-values of (lobby-client-peers client)
                collect peer)
          #'string-lessp :key #'lobby-peer-name)
         :last-error (lobby-client-last-error client)
         :revision (lobby-client-revision client)))
      (make-lobby-snapshot :status :stopped :peers nil
                           :last-error nil :revision 0)))

(defgeneric lobby-client-summary (client)
  (:documentation
   "Return CLIENT's STATUS, PEER-COUNT, LAST-ERROR, and REVISION as values.

This is the constant-work frame-boundary view: it neither copies nor sorts the
peer collection and never touches the transport.  Detailed tools which need
peer identities continue to use LOBBY-CLIENT-SNAPSHOT."))

(defmethod lobby-client-summary ((client lobby-client))
  (sb-thread:with-mutex ((lobby-client-lock client))
    (values (lobby-client-status client)
            (hash-table-count (lobby-client-peers client))
            (lobby-client-last-error client)
            (lobby-client-revision client))))

(defmethod lobby-client-summary ((client null))
  (declare (ignore client))
  (values :stopped 0 nil 0))

(defun lobby-client-value (client key)
  "Return the current connection's retained value for KEY, or NIL."
  (when client
    (sb-thread:with-mutex ((lobby-client-lock client))
      (gethash key (lobby-client-values client)))))

(defgeneric receive-lobby-publication (client topic payload)
  (:documentation
   "Publish one transport TOPIC/PAYLOAD into CLIENT's semantic local state."))

(defmethod receive-lobby-publication ((client lobby-client) topic payload)
  (cond
    ((lobby-topic-prefix-p +lobby-presence-prefix+ topic)
     (let ((id (subseq topic (length +lobby-presence-prefix+))))
       (unless (string= id (lobby-client-id client))
         (sb-thread:with-mutex ((lobby-client-lock client))
           (let* ((peers (lobby-client-peers client))
                  (old (gethash id peers))
                  (tombstone-p
                    (or (zerop (length payload))
                        (string= payload +lobby-offline-payload+))))
             (cond
               ((and tombstone-p old)
                (remhash id peers)
                (incf (lobby-client-revision client)))
               ((and (not tombstone-p)
                     (or (null old)
                         (not (string= payload (lobby-peer-name old)))))
                (setf (gethash id peers)
                      (make-instance 'lobby-peer :id id :name payload))
                (incf (lobby-client-revision client)))))))))
    ((lobby-topic-prefix-p +lobby-store-prefix+ topic)
     (let ((key (subseq topic (length +lobby-store-prefix+))))
       (sb-thread:with-mutex ((lobby-client-lock client))
         (let* ((values (lobby-client-values client))
                (old (gethash key values))
                (tombstone-p (zerop (length payload))))
           (cond
             ((and tombstone-p old)
              (remhash key values)
              (incf (lobby-client-revision client)))
             ((and (not tombstone-p) (not (equal old payload)))
              (setf (gethash key values) payload)
              (incf (lobby-client-revision client)))))))))
  client)

(defmethod open-lobby-transport
    ((transport mqtt-lobby-transport) (client lobby-client))
  (let ((topic (lobby-client-topic client)))
    (mqtt.net:open-lobby-connection
     :client-id (lobby-client-id client)
     :keep-alive (mqtt-lobby-keep-alive transport)
     :timeout (mqtt-lobby-connect-timeout transport)
     ;; The broker rejects a zero-byte Will even though MQTT permits it.  A
     ;; NUL is the retained radio tombstone; graceful shutdown uses empty.
     :will (make-instance 'mqtt:will :topic topic
                           :payload +lobby-offline-payload+
                           :qos 1 :retain t))))

(defmethod subscribe-lobby-transport
    ((transport mqtt-lobby-transport) connection (client lobby-client))
  (declare (ignore transport client))
  (mqtt.net:subscribe connection
                      (list +lobby-presence-filter+ :qos 1)
                      (list +lobby-store-filter+ :qos 1)))

(defmethod publish-lobby-transport
    ((transport mqtt-lobby-transport) connection
     (client lobby-client) payload)
  (declare (ignore transport))
  (mqtt.net:publish connection (lobby-client-topic client) payload
                    :qos 1 :retain t))

(defmethod next-lobby-publication
    ((transport mqtt-lobby-transport) connection (client lobby-client))
  (declare (ignore client))
  (handler-case
      (let ((message
              (mqtt.net:next-message
               connection :timeout (mqtt-lobby-poll-timeout transport))))
        (when message
          (values (mqtt:publish-topic message)
                  (mqtt:publish-payload-string message))))
    (mqtt.net:connection-timeout () nil)))

(defmethod close-lobby-transport
    ((transport mqtt-lobby-transport) connection (client lobby-client))
  (declare (ignore transport client))
  (mqtt.net:close-mqtt-connection connection))

(defun wait-before-lobby-retry (client seconds)
  (sb-thread:with-mutex ((lobby-client-lock client))
    (unless (lobby-client-stopping-p client)
      (sb-thread:condition-wait
       (lobby-client-wake client) (lobby-client-lock client)
       :timeout seconds))))

(defun call-with-lobby-radio-connection (client)
  (let ((connection nil)
        (transport (lobby-client-transport client)))
    (unwind-protect
         (progn
           (setf connection (open-lobby-transport transport client))
           (sb-thread:with-mutex ((lobby-client-lock client))
             (setf (lobby-client-connection client) connection))
           (subscribe-lobby-transport transport connection client)
           (publish-lobby-transport
            transport connection client (lobby-client-name client))
           (publish-lobby-status client :online)
           (loop until (lobby-client-stop-requested-p client)
                 do (multiple-value-bind (topic payload)
                        (next-lobby-publication transport connection client)
                      (when topic
                        (receive-lobby-publication client topic payload)))))
      ;; Only the worker ever speaks on the connection, including graceful
      ;; presence deletion and close.  STOP merely publishes a local flag.
      (when connection
        (ignore-errors
         (publish-lobby-transport transport connection client ""))
        (sb-thread:with-mutex ((lobby-client-lock client))
          (when (eq connection (lobby-client-connection client))
            (setf (lobby-client-connection client) nil)))
        (ignore-errors
         (close-lobby-transport transport connection client))))))

(defun run-lobby-client (client)
  (unwind-protect
       (loop with delay = 1/4
             until (lobby-client-stop-requested-p client)
             do (publish-lobby-status
                 client :connecting :clear-cache-p t)
                (handler-case
                    (progn
                      (call-with-lobby-radio-connection client)
                      (setf delay 1/4))
                  (error (condition)
                    (unless (lobby-client-stop-requested-p client)
                      (publish-lobby-status
                       client :offline :condition condition
                       :clear-cache-p t)
                      (wait-before-lobby-retry client delay)
                      (setf delay (min 5 (* 2 delay)))))))
    (sb-thread:with-mutex ((lobby-client-lock client))
      (%publish-lobby-status client :stopped :clear-cache-p t)
      (when (eq (lobby-client-thread client) sb-thread:*current-thread*)
        (setf (lobby-client-thread client) nil))))
  client)

(defun start-lobby-client (client)
  "Start CLIENT once.  A stopped client may be restarted with fresh state."
  (check-type client lobby-client)
  (sb-thread:with-mutex ((lobby-client-lock client))
    (let ((thread (lobby-client-thread client)))
      (cond
        ((and thread (sb-thread:thread-alive-p thread))
         (when (lobby-client-stopping-p client)
           (error "Lobby client ~A is still stopping."
                  (lobby-client-name client))))
        (t
         (setf (lobby-client-thread client) nil
               (lobby-client-stopping-p client) nil)
         (%publish-lobby-status client :starting :clear-cache-p t)
         (setf (lobby-client-thread client)
               (sb-thread:make-thread
                (lambda () (run-lobby-client client))
                :name (format nil "~A lobby radio"
                              (lobby-client-id-prefix client))))))))
  client)

(defun stop-lobby-client (client &key (timeout 3/4))
  "Request cooperative stop and wait at most TIMEOUT seconds.

No transport operation occurs on the caller.  Repeated stops are idempotent."
  (when client
    (let ((thread nil))
      (sb-thread:with-mutex ((lobby-client-lock client))
        (setf thread (lobby-client-thread client))
        (when thread
          (setf (lobby-client-stopping-p client) t)
          (%publish-lobby-status client :stopping :clear-cache-p t)
          (sb-thread:condition-broadcast (lobby-client-wake client))))
      (when (and thread (not (eq thread sb-thread:*current-thread*)))
        (multiple-value-bind (value state)
            (sb-thread:join-thread thread :timeout timeout :default :timeout)
          (declare (ignore value))
          (when (eq state :timeout)
            (error 'lobby-worker-stop-timeout
                   :client client :seconds timeout))))
      ;; RUN-LOBBY-CLIENT normally clears this itself.  Clearing a dead thread
      ;; here also recovers cleanly if an implementation returns before its
      ;; unwind cleanup becomes visible.
      (sb-thread:with-mutex ((lobby-client-lock client))
        (when (and (lobby-client-thread client)
                   (not (sb-thread:thread-alive-p
                         (lobby-client-thread client))))
          (setf (lobby-client-thread client) nil)))))
  client)
