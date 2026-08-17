;;;; The post-authorization session.
;;;;
;;;; Once a key exists, everything travels in encrypted packets carrying a
;;;; salt, a session id, a message id, and a sequence number.  The session
;;;; owns that bookkeeping: the salt the server last told us to use, the
;;;; monotone message id, the odd/even sequence-number policy that decides
;;;; which messages get acknowledged, and the table of requests still waiting
;;;; for an answer.
;;;;
;;;; Incoming service messages are handled by a generic function specialized
;;;; on the message class, so teaching the session about a new one is a
;;;; DEFMETHOD.  Acknowledgement is a :BEFORE method rather than a line in
;;;; each handler, because whether a message needs acknowledging depends only
;;;; on its sequence number and not at all on what it says.

(in-package #:telegram)

(define-condition remote-rpc-error (mtproto-error)
  ((code :initarg :code :reader remote-rpc-error-code)
   (message :initarg :message :reader remote-rpc-error-message))
  (:report (lambda (condition stream)
             (format stream "Telegram returned ~D ~A."
                     (remote-rpc-error-code condition)
                     (remote-rpc-error-message condition))))
  (:documentation "An rpc_error the server returned for one of our requests."))

(defclass pending-request ()
  ((message-id :initarg :message-id :reader pending-request-message-id)
   (name :initarg :name :initform nil :reader pending-request-name
         :documentation "A label for logs and inspection; never sent.")
   (body :initarg :body :reader pending-request-body)
   (sequence-number :initarg :sequence-number
                    :reader pending-request-sequence-number)
   (result :initform nil :accessor pending-request-result
           :documentation "The raw TL bytes the server answered with.")
   (error :initform nil :accessor pending-request-error)
   (done :initform nil :accessor pending-request-done-p))
  (:documentation
   "A request that has been sent and not yet answered.  Kept because the
server may ask us to resend it under a corrected salt."))

(defmethod print-object ((request pending-request) stream)
  (print-unreadable-object (request stream :type t)
    (format stream "~@[~A ~]~D~:[~; done~]"
            (pending-request-name request)
            (pending-request-message-id request)
            (pending-request-done-p request))))

(defclass mtproto-session ()
  ((key :initarg :key :reader session-key)
   (server-salt :initarg :server-salt :accessor session-server-salt)
   (id :initarg :id :reader session-id)
   (time-offset :initarg :time-offset :initform 0 :accessor session-time-offset)
   (last-message-id :initarg :last-message-id :initform nil
                    :accessor session-last-message-id)
   (sent-content-messages :initform 0 :accessor session-sent-content-messages)
   (pending-requests :initform (make-hash-table)
                     :reader session-pending-requests)
   (outbox :initform '() :accessor session-outbox
           :documentation "Packets the session produced and a driver must write.")
   (acknowledgements :initform '() :accessor session-acknowledgements)
   (events :initform '() :accessor session-events
           :documentation "A newest-first log of what the session has seen.")
   (initialized :initform nil :accessor session-initialized-p
                :documentation
                "Whether initConnection has been sent on this session.  The
Telegram API layer requires it once per session and never again, and the
session is the thing that knows how long `once' lasts.")
   (entropy :initarg :entropy :initform octets:*entropy* :reader session-entropy)
   (clock :initarg :clock :initform octets:*clock* :reader session-clock))
  (:documentation
   "An MTProto session: one authorization key, one session id, and the state
that keeps a conversation over them coherent."))

(defmethod print-object ((session mtproto-session) stream)
  (print-unreadable-object (session stream :type t)
    (format stream "~A ~D pending"
            (octets:octets-hex (auth-key-id (session-key session)))
            (hash-table-count (session-pending-requests session)))))

(defun make-mtproto-session (material &key session-id
                                           (entropy octets:*entropy*)
                                           (clock octets:*clock*))
  "A session over the authorization key MATERIAL produced.  A random session
id is drawn from ENTROPY unless one is given -- reusing a stored one is how a
client resumes without the server starting a new session."
  (make-instance 'mtproto-session
                 :key (auth-key-material-key material)
                 :server-salt (auth-key-material-server-salt material)
                 :time-offset (auth-key-material-time-offset material)
                 :id (or session-id
                         (tl:read-tl-signed-long
                          (tl:make-tl-reader (octets:random-octets entropy 8))))
                 :entropy entropy
                 :clock clock))

(defun note-session-event (session event)
  "Record EVENT in the session log."
  (push event (session-events session))
  event)

(defun session-now-nanoseconds (session)
  "The session's idea of server time, in nanoseconds."
  (max 0 (+ (octets:clock-unix-nanoseconds (session-clock session))
            (* (session-time-offset session) +nanoseconds-per-second+))))

(defun session-next-sequence-number (session content-p)
  "The sequence number for the next outgoing message.  Only content messages
advance the counter, and only they get an odd number, which is precisely how
the peer knows what to acknowledge."
  (let ((sent (session-sent-content-messages session)))
    (if content-p (1+ (* 2 sent)) (* 2 sent))))

(defun session-send (session body &key content-p name padding)
  "Seal BODY as one outgoing packet and return the bytes to write, the packet
itself, and, for a content message, its PENDING-REQUEST."
  (let* ((message-id (next-message-id (session-last-message-id session)
                                      (session-now-nanoseconds session)))
         (packet (make-instance 'encrypted-packet
                                :salt (session-server-salt session)
                                :session-id (session-id session)
                                :message-id message-id
                                :sequence-number
                                (session-next-sequence-number session content-p)
                                :body (octets:to-octets body)
                                :padding padding)))
    (multiple-value-bind (encrypted sealed)
        (encode-encrypted-packet packet (session-key session)
                                 :sender :client
                                 :entropy (session-entropy session))
      (setf (session-last-message-id session) message-id)
      (when content-p
        (incf (session-sent-content-messages session)))
      (let ((request (when content-p
                       (let ((request (make-instance
                                       'pending-request
                                       :message-id message-id
                                       :name name
                                       :body (encrypted-packet-body sealed)
                                       :sequence-number
                                       (encrypted-packet-sequence-number sealed))))
                         (setf (gethash message-id
                                        (session-pending-requests session))
                               request)))))
        (values encrypted sealed request)))))

(defun session-send-request (session body &key name)
  "Send BODY as a content message.  Returns the bytes to write and the
PENDING-REQUEST that will hold the answer."
  (multiple-value-bind (encrypted packet request)
      (session-send session body :content-p t :name name)
    (declare (ignore packet))
    (values encrypted request)))

(defun session-send-ping (session ping-id)
  "Send a ping, which is a service message and so is never acknowledged or
resent."
  (session-send session (tl:encode-tl-octets
                         (make-instance 'ping :ping-id ping-id))))

(defun enqueue-session-packet (session encrypted)
  "Queue ENCRYPTED for the driver to write."
  (setf (session-outbox session)
        (append (session-outbox session) (list encrypted)))
  encrypted)

(defun drain-session-outbox (session)
  "Take everything the session has queued to send."
  (prog1 (session-outbox session)
    (setf (session-outbox session) '())))

;;;; Receiving

(defun session-receive-packet (session payload)
  "Open one encrypted PAYLOAD and let the session act on what is inside.
Returns the decoded top-level object.  Anything the session decided to send
in response -- an acknowledgement, a resend -- is waiting in the outbox."
  (let* ((packet (decode-encrypted-packet payload (session-key session)
                                          :sender :server
                                          :session-id (session-id session)))
         (message (make-instance 'mtproto-message
                                 :message-id (encrypted-packet-message-id packet)
                                 :sequence-number
                                 (encrypted-packet-sequence-number packet)
                                 :body (encrypted-packet-body packet)))
         (object (tl:decode-tl-octets (encrypted-packet-body packet))))
    (handle-session-message session message object)
    (flush-session-acknowledgements session)
    object))

(defun flush-session-acknowledgements (session)
  "Send one msgs_ack for everything received since the last flush."
  (let ((ids (remove-duplicates (nreverse (session-acknowledgements session)))))
    (setf (session-acknowledgements session) '())
    (when ids
      (enqueue-session-packet
       session
       (session-send session
                     (tl:encode-tl-octets
                      (make-instance 'msgs-ack
                                     :message-ids (coerce ids 'vector))))))))

(defgeneric handle-session-message (session message object)
  (:documentation
   "Act on OBJECT, which arrived as MESSAGE.  Specialize on OBJECT's class to
teach the session about another service message."))

(defmethod handle-session-message :before ((session mtproto-session)
                                           (message mtproto-message)
                                           (object t))
  "Queue an acknowledgement for content messages.  Whether a message needs
acknowledging is decided by its sequence number alone, so it composes with
every handler instead of appearing in each of them."
  (when (oddp (mtproto-message-sequence-number message))
    (push (mtproto-message-message-id message)
          (session-acknowledgements session))))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object t))
  "The fallback, specialized on T rather than on TL-OBJECT: a message may
also arrive as a schema record, and a session that cannot be handed one has
no business being connected to Telegram."
  (note-session-event session (list :unhandled object)))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object tl:tl-record))
  "Anything from the API schema, which in practice means updates: Telegram
pushes them unasked, alongside the answers to requests.  Logged by name so
the event log stays readable, and left for a caller that wants them."
  (note-session-event session (list :update (tl:tl-name object) object)))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object msg-container))
  (map nil
       (lambda (member)
         (handle-session-message session member
                                 (tl:decode-tl-octets
                                  (mtproto-message-body member))))
       (msg-container-messages object)))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object gzip-packed))
  "Inflate and dispatch again.  Compression is not a message kind."
  (handle-session-message
   session message
   (tl:decode-tl-octets
    (octets:decompress (gzip-packed-packed-data object)))))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object pong))
  (note-session-event session (list :pong (pong-ping-id object))))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object msgs-ack))
  (note-session-event session (list :acknowledged (msgs-ack-message-ids object))))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object new-session-created))
  (setf (session-server-salt session)
        (new-session-created-server-salt object))
  (note-session-event session (list :new-session
                                    (new-session-created-unique-id object))))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object bad-server-salt))
  (setf (session-server-salt session) (bad-server-salt-new-server-salt object))
  (note-session-event session (list :bad-server-salt
                                    (bad-server-salt-new-server-salt object)))
  (resend-pending-request session (bad-server-salt-bad-message-id object)))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object bad-msg-notification))
  (note-session-event session (list :bad-message
                                    (bad-msg-notification-error-code object)))
  (let ((request (gethash (bad-msg-notification-bad-message-id object)
                          (session-pending-requests session))))
    (when request
      (setf (pending-request-error request) object
            (pending-request-done-p request) t)
      (remhash (bad-msg-notification-bad-message-id object)
               (session-pending-requests session)))))

(defmethod handle-session-message ((session mtproto-session)
                                   (message mtproto-message)
                                   (object rpc-result))
  (let* ((message-id (rpc-result-request-message-id object))
         (result (unwrap-gzip (rpc-result-result object)))
         (request (gethash message-id (session-pending-requests session))))
    (remhash message-id (session-pending-requests session))
    (note-session-event session (list :rpc-result message-id))
    (when request
      (setf (pending-request-result request) result
            (pending-request-error request) (result-rpc-error result)
            (pending-request-done-p request) t))
    request))

(defun result-rpc-error (result)
  "The RPC-ERROR inside RESULT, if that is what it is."
  (when (>= (length result) 4)
    (let ((id (octets:octets-integer result :end 4 :endian :little)))
      (when (= id (tl:tl-constructor-id 'rpc-error))
        (tl:decode-tl-octets result)))))

(defun resend-pending-request (session message-id)
  "Re-send a request the server rejected, under the salt it just gave us.
The message id is kept, because from the server's point of view this is the
same request arriving again."
  (let ((request (gethash message-id (session-pending-requests session))))
    (when request
      (let ((packet (make-instance
                     'encrypted-packet
                     :salt (session-server-salt session)
                     :session-id (session-id session)
                     :message-id message-id
                     :sequence-number (pending-request-sequence-number request)
                     :body (pending-request-body request))))
        (enqueue-session-packet
         session
         (encode-encrypted-packet packet (session-key session)
                                  :sender :client
                                  :entropy (session-entropy session)))))))
