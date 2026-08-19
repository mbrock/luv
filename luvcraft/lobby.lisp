;;; The lobby radio owned by a playing luvcraft session.
;;;
;;; One worker owns the MQTT socket.  It turns network changes into small,
;;; locked semantic snapshots; the canvas thread never performs network I/O.

(in-package #:luvcraft)

(defparameter +lobby-presence-prefix+ "luv/presence/")
(defparameter +lobby-presence-filter+ "luv/presence/+")
(defparameter +lobby-offline-payload+ (string #\Null))
(defparameter +lobby-store-prefix+ "luv/store/")
(defparameter +lobby-store-filter+ "luv/store/+")

(defclass lobby-peer ()
  ((id :initarg :id :reader lobby-peer-id)
   (name :initarg :name :reader lobby-peer-name))
  (:documentation "One game instance currently heard on the lobby radio."))

(defclass lobby-client ()
  ((id :initarg :id :reader lobby-client-id)
   (name :initarg :name :reader lobby-client-name)
   (lock :initform (sb-thread:make-mutex :name "luvcraft lobby state")
         :reader lobby-client-lock)
   (wake :initform (sb-thread:make-waitqueue :name "luvcraft lobby wake")
         :reader lobby-client-wake)
   (status :initform :starting :accessor lobby-client-status)
   (last-error :initform nil :accessor lobby-client-last-error)
   (peers :initform (make-hash-table :test #'equal) :reader lobby-client-peers)
   (values :initform (make-hash-table :test #'equal) :reader lobby-client-values)
   (revision :initform 0 :accessor lobby-client-revision)
   (connection :initform nil :accessor lobby-client-connection)
   (stopping-p :initform nil :accessor lobby-client-stopping-p)
   (thread :initform nil :accessor lobby-client-thread))
  (:documentation
   "A forgiving tailnet radio: one worker connection, presence, and cached
retained values.  Network failure changes STATUS and causes a later retry."))

(defmethod print-object ((client lobby-client) stream)
  (print-unreadable-object (client stream :type t :identity t)
    (format stream "~A ~(~A~)" (lobby-client-name client)
            (lobby-client-status client))))

(defun lobby-topic-prefix-p (prefix topic)
  (and (<= (length prefix) (length topic))
       (string= prefix topic :end2 (length prefix))))

(defun lobby-safe-id (text)
  (string-downcase
   (with-output-to-string (out)
     (loop for character across text
           do (write-char (if (alphanumericp character) character #\-) out)))))

(defun default-lobby-player-name ()
  (or (let ((name (uiop:getenv "LUV_PLAYER_NAME")))
        (and name (plusp (length name)) name))
      (let ((name (uiop:getenv "USER")))
        (and name (plusp (length name)) (string-capitalize name)))
      (ignore-errors (machine-instance))
      "someone"))

(defun make-lobby-client (&key (name (default-lobby-player-name)))
  (make-instance 'lobby-client
                 :name name
                 :id (format nil "~A-~D" (lobby-safe-id (machine-instance))
                             (sb-posix:getpid))))

(defun lobby-client-topic (client)
  (concatenate 'string +lobby-presence-prefix+ (lobby-client-id client)))

(defun set-lobby-client-status (client status &optional condition)
  (let ((detail (and condition (princ-to-string condition))))
    (sb-thread:with-mutex ((lobby-client-lock client))
      (let ((changed-p
              (or (not (eq status (lobby-client-status client)))
                  (not (equal detail (lobby-client-last-error client)))
                  (and (member status '(:offline :stopped))
                       (plusp (hash-table-count
                               (lobby-client-peers client)))))))
        (setf (lobby-client-status client) status
              (lobby-client-last-error client) detail)
        ;; A green dot is only evidence from the current connection.  A fresh
        ;; subscription reconstructs it from retained presence after recovery.
        (when (member status '(:offline :stopped))
          (clrhash (lobby-client-peers client)))
        (when changed-p
          (incf (lobby-client-revision client))))))
  status)

(defun lobby-client-stop-requested-p (client)
  (sb-thread:with-mutex ((lobby-client-lock client))
    (lobby-client-stopping-p client)))

(defun lobby-client-snapshot (client)
  "STATUS, other PEERS by name, last error text, and revision, atomically."
  (sb-thread:with-mutex ((lobby-client-lock client))
    (values (lobby-client-status client)
            (sort (loop for peer being the hash-values of (lobby-client-peers client)
                        collect (lobby-peer-name peer))
                  #'string-lessp)
            (lobby-client-last-error client)
            (lobby-client-revision client))))

(defun lobby-client-value (client key)
  "The last retained lobby store value heard for KEY, or NIL."
  (when client
    (sb-thread:with-mutex ((lobby-client-lock client))
      (gethash key (lobby-client-values client)))))

(defgeneric receive-lobby-publication (client topic payload)
  (:documentation
   "Publish one MQTT TOPIC/PAYLOAD into CLIENT's semantic local state."))

(defmethod receive-lobby-publication ((client lobby-client) topic payload)
  (cond
    ((lobby-topic-prefix-p +lobby-presence-prefix+ topic)
     (let ((id (subseq topic (length +lobby-presence-prefix+))))
       (unless (string= id (lobby-client-id client))
         (sb-thread:with-mutex ((lobby-client-lock client))
           (if (or (zerop (length payload))
                   (string= payload +lobby-offline-payload+))
               (remhash id (lobby-client-peers client))
               (setf (gethash id (lobby-client-peers client))
                     (make-instance 'lobby-peer :id id :name payload)))
           (incf (lobby-client-revision client))))))
    ((lobby-topic-prefix-p +lobby-store-prefix+ topic)
     (let ((key (subseq topic (length +lobby-store-prefix+))))
       (sb-thread:with-mutex ((lobby-client-lock client))
         (if (zerop (length payload))
             (remhash key (lobby-client-values client))
             (setf (gethash key (lobby-client-values client)) payload))
         (incf (lobby-client-revision client))))))
  client)

(defun receive-lobby-message (client message)
  (receive-lobby-publication
   client (mqtt:publish-topic message) (mqtt:publish-payload-string message)))

(defun wait-before-lobby-retry (client seconds)
  (sb-thread:with-mutex ((lobby-client-lock client))
    (unless (lobby-client-stopping-p client)
      (sb-thread:condition-wait (lobby-client-wake client)
                                (lobby-client-lock client)
                                :timeout seconds))))

(defun call-with-lobby-radio-connection (client)
  (let ((connection nil)
        (topic (lobby-client-topic client)))
    (unwind-protect
         (progn
           (setf connection
                 (mqtt.net:open-lobby-connection
                  :client-id (format nil "luvcraft-~A" (lobby-client-id client))
                  :keep-alive 20 :timeout 1
                  ;; The lobby broker rejects a zero-byte Will even though
                  ;; MQTT permits it.  A NUL is our retained radio tombstone;
                  ;; graceful shutdown still uses MQTT's empty deletion.
                  :will (make-instance 'mqtt:will :topic topic
                                       :payload +lobby-offline-payload+
                                       :qos 1 :retain t)))
           (sb-thread:with-mutex ((lobby-client-lock client))
             (setf (lobby-client-connection client) connection))
           (mqtt.net:subscribe connection (list +lobby-presence-filter+ :qos 1)
                               (list +lobby-store-filter+ :qos 1))
           (mqtt.net:publish connection topic (lobby-client-name client)
                             :qos 1 :retain t)
           (set-lobby-client-status client :online)
           (loop until (lobby-client-stop-requested-p client)
                 do (handler-case
                        (alexandria:when-let
                            ((message (mqtt.net:next-message connection :timeout 1)))
                          (receive-lobby-message client message))
                      (mqtt.net:connection-timeout ()))))
      ;; Any deliberate MQTT disconnect suppresses the Will, so erase our
      ;; retained presence first whenever the socket is still usable.
      (when connection
        (ignore-errors
         (mqtt.net:publish connection topic "" :qos 1 :retain t))
        (sb-thread:with-mutex ((lobby-client-lock client))
          (setf (lobby-client-connection client) nil))
        (mqtt.net:close-mqtt-connection connection)))))

(defun run-lobby-client (client)
  (loop with delay = 1
        until (lobby-client-stop-requested-p client)
        do (set-lobby-client-status client :connecting)
           (handler-case
               (progn
                 (call-with-lobby-radio-connection client)
                 (setf delay 1))
             (error (condition)
               (unless (lobby-client-stop-requested-p client)
                 (set-lobby-client-status client :offline condition)
                 (wait-before-lobby-retry client delay)
                 (setf delay (min 10 (* 2 delay)))))))
  (set-lobby-client-status client :stopped)
  client)

(defun start-lobby-client (client)
  (unless (lobby-client-thread client)
    (setf (lobby-client-thread client)
          (sb-thread:make-thread (lambda () (run-lobby-client client))
                                 :name "luvcraft lobby radio")))
  client)

(defun stop-lobby-client (client &key (timeout 6))
  (when client
    (sb-thread:with-mutex ((lobby-client-lock client))
      (setf (lobby-client-stopping-p client) t)
      (sb-thread:condition-notify (lobby-client-wake client)))
    (alexandria:when-let ((thread (lobby-client-thread client)))
      (unless (eq thread sb-thread:*current-thread*)
        (multiple-value-bind (value state)
            (sb-thread:join-thread thread :timeout timeout :default :timeout)
          (declare (ignore value))
          (when (eq state :timeout)
            (error "Lobby worker did not stop within ~D seconds." timeout))))
      (setf (lobby-client-thread client) nil)))
  client)

(defmethod start-luvcraft-lobby ((session luvcraft-session))
  (or (luvcraft-session-lobby-client session)
      (setf (luvcraft-session-lobby-client session)
            (start-lobby-client (make-lobby-client)))))

(defmethod stop-luvcraft-lobby ((session luvcraft-session))
  (alexandria:when-let ((client (luvcraft-session-lobby-client session)))
    (stop-lobby-client client)
    (setf (luvcraft-session-lobby-client session) nil))
  nil)

;;; Telegram owns the credential policy; luvcraft contributes its radio cache
;;; as one late fallback without teaching the Telegram system about MQTT.

(defun lobby-telegram-credential (name)
  (and *session*
       (member name '("TELEGRAM_API_ID" "TELEGRAM_API_HASH"
                      "TDLIB_API_ID" "TDLIB_API_HASH")
               :test #'string=)
       (lobby-client-value (luvcraft-session-lobby-client *session*) name)))

(pushnew 'lobby-telegram-credential telegram.client:*credential-fallbacks*)
