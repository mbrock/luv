;;;; MQTT 5 control packets (sections 2 and 3): each packet type is a class
;;;; that knows how to write and read its own body, and a decoder that turns
;;;; a stream of octets into a stream of packets.

(in-package #:mqtt)

;;;; Properties (section 2.2.2)
;;;;
;;;; A property list is an alist of (keyword . value).  It is an alist rather
;;;; than a plist because user properties and subscription identifiers may
;;;; repeat, and their order is meaningful.  The table below is the
;;;; specification's own, and closed: extending it means the spec changed.

(defparameter *property-table*
  '((#x01 :payload-format-indicator :byte)
    (#x02 :message-expiry-interval :four-byte-integer)
    (#x03 :content-type :utf8-string)
    (#x08 :response-topic :utf8-string)
    (#x09 :correlation-data :binary-data)
    (#x0b :subscription-identifier :variable-byte-integer)
    (#x11 :session-expiry-interval :four-byte-integer)
    (#x12 :assigned-client-identifier :utf8-string)
    (#x13 :server-keep-alive :two-byte-integer)
    (#x15 :authentication-method :utf8-string)
    (#x16 :authentication-data :binary-data)
    (#x17 :request-problem-information :byte)
    (#x18 :will-delay-interval :four-byte-integer)
    (#x19 :request-response-information :byte)
    (#x1a :response-information :utf8-string)
    (#x1c :server-reference :utf8-string)
    (#x1f :reason-string :utf8-string)
    (#x21 :receive-maximum :two-byte-integer)
    (#x22 :topic-alias-maximum :two-byte-integer)
    (#x23 :topic-alias :two-byte-integer)
    (#x24 :maximum-qos :byte)
    (#x25 :retain-available :byte)
    (#x26 :user-property :utf8-string-pair)
    (#x27 :maximum-packet-size :four-byte-integer)
    (#x28 :wildcard-subscription-available :byte)
    (#x29 :subscription-identifier-available :byte)
    (#x2a :shared-subscription-available :byte))
  "Section 2.2.2.2: identifier, name, and wire type of every property.")

(defun property-entry (designator)
  (or (find designator *property-table*
            :key (if (integerp designator) #'first #'second))
      (malformed "unknown property ~S" designator)))

(defun read-property-value (type reader)
  (ecase type
    (:byte (read-octet reader))
    (:two-byte-integer (read-two-byte-integer reader))
    (:four-byte-integer (read-four-byte-integer reader))
    (:variable-byte-integer (read-variable-byte-integer reader))
    (:utf8-string (read-utf8-string reader))
    (:binary-data (read-binary-data reader))
    (:utf8-string-pair (read-utf8-string-pair reader))))

(defun write-property-value (type writer value)
  (ecase type
    (:byte (write-octet writer value))
    (:two-byte-integer (write-two-byte-integer writer value))
    (:four-byte-integer (write-four-byte-integer writer value))
    (:variable-byte-integer (write-variable-byte-integer writer value))
    (:utf8-string (write-utf8-string writer value))
    (:binary-data (write-binary-data writer value))
    (:utf8-string-pair (write-utf8-string-pair writer value))))

(defun read-properties (reader)
  "A length-prefixed property list, as an alist."
  (let* ((length (read-variable-byte-integer reader))
         (end (+ (wire-reader-position reader) length)))
    (when (> end (wire-reader-end reader))
      (malformed "properties run past the packet"))
    (loop while (< (wire-reader-position reader) end)
          collect (destructuring-bind (id name type)
                      (property-entry (read-variable-byte-integer reader))
                    (declare (ignore id))
                    (cons name (read-property-value type reader))))))

(defun write-properties (writer properties)
  "PROPERTIES as a length-prefixed property list."
  (let ((body (with-wire-writer (inner)
                (loop for (name . value) in properties
                      do (destructuring-bind (id name type) (property-entry name)
                           (declare (ignore name))
                           (write-variable-byte-integer inner id)
                           (write-property-value type inner value))))))
    (write-variable-byte-integer writer (length body))
    (write-octet-vector writer body)))

(defun property (properties name &optional default)
  "The first NAME property in PROPERTIES, or DEFAULT."
  (let ((cell (assoc name properties)))
    (if cell (cdr cell) default)))

(defun user-properties (properties)
  "The user properties in PROPERTIES, as an alist of strings."
  (loop for (name . value) in properties
        when (eq name :user-property) collect value))

;;;; Reason codes (section 2.4)

(defparameter *reason-code-table*
  '((#x00 :success)                     ; also normal-disconnection, granted-qos-0
    (#x01 :granted-qos-1)
    (#x02 :granted-qos-2)
    (#x04 :disconnect-with-will-message)
    (#x10 :no-matching-subscribers)
    (#x11 :no-subscription-existed)
    (#x18 :continue-authentication)
    (#x19 :re-authenticate)
    (#x80 :unspecified-error)
    (#x81 :malformed-packet)
    (#x82 :protocol-error)
    (#x83 :implementation-specific-error)
    (#x84 :unsupported-protocol-version)
    (#x85 :client-identifier-not-valid)
    (#x86 :bad-user-name-or-password)
    (#x87 :not-authorized)
    (#x88 :server-unavailable)
    (#x89 :server-busy)
    (#x8a :banned)
    (#x8b :server-shutting-down)
    (#x8c :bad-authentication-method)
    (#x8d :keep-alive-timeout)
    (#x8e :session-taken-over)
    (#x8f :topic-filter-invalid)
    (#x90 :topic-name-invalid)
    (#x91 :packet-identifier-in-use)
    (#x92 :packet-identifier-not-found)
    (#x93 :receive-maximum-exceeded)
    (#x94 :topic-alias-invalid)
    (#x95 :packet-too-large)
    (#x96 :message-rate-too-high)
    (#x97 :quota-exceeded)
    (#x98 :administrative-action)
    (#x99 :payload-format-invalid)
    (#x9a :retain-not-supported)
    (#x9b :qos-not-supported)
    (#x9c :use-another-server)
    (#x9d :server-moved)
    (#x9e :shared-subscriptions-not-supported)
    (#x9f :connection-rate-exceeded)
    (#xa0 :maximum-connect-time)
    (#xa1 :subscription-identifiers-not-supported)
    (#xa2 :wildcard-subscriptions-not-supported)))

(defun reason-code-name (code)
  "The specification's name for reason CODE, or the code itself."
  (or (second (assoc code *reason-code-table*)) code))

(defun reason-code (designator)
  "DESIGNATOR as an integer reason code; a keyword is looked up."
  (if (integerp designator)
      designator
      (or (first (find designator *reason-code-table* :key #'second))
          (error "Unknown reason code ~S." designator))))

(defun reason-code-error-p (code)
  "Codes from #x80 up are failures (section 2.4)."
  (>= code #x80))

;;;; Packet classes

(defclass packet ()
  ((properties :initarg :properties :initform '() :accessor packet-properties))
  (:documentation "An MQTT control packet."))

(defgeneric packet-type-code (packet)
  (:documentation "The four-bit control packet type (section 2.1.2)."))

(defgeneric packet-flags (packet)
  (:documentation "The four flag bits of the fixed header (section 2.1.3).")
  (:method ((packet packet)) 0))

(defgeneric encode-packet-body (packet writer)
  (:documentation "Write PACKET's variable header and payload to WRITER."))

(defgeneric decode-packet-body (packet flags reader)
  (:documentation
   "Fill the fresh PACKET from READER, which holds exactly its variable header
and payload; FLAGS are the fixed header's low nibble.  Returns PACKET."))

(defmethod decode-packet-body :around ((packet packet) flags reader)
  (call-next-method)
  (unless (wire-reader-exhausted-p reader)
    (malformed "~D trailing octets after ~A" (wire-reader-remaining reader)
               (type-of packet)))
  packet)

(defun packet-property (packet name &optional default)
  (property (packet-properties packet) name default))

(defclass identified-packet (packet)
  ((packet-id :initarg :packet-id :initform nil :accessor packet-id))
  (:documentation "A packet that carries a packet identifier (section 2.2.1)."))

(defclass reasoned-packet (packet)
  ((reason-code :initarg :reason-code :initform 0 :accessor packet-reason-code))
  (:documentation "A packet with a single reason code."))

(defmethod packet-reason ((packet reasoned-packet))
  (reason-code-name (packet-reason-code packet)))

;;; CONNECT (3.1)

(defclass will ()
  ((topic :initarg :topic :reader will-topic)
   (payload :initarg :payload :initform (make-octets 0) :reader will-payload)
   (qos :initarg :qos :initform 0 :reader will-qos)
   (retain :initarg :retain :initform nil :reader will-retain-p)
   (properties :initarg :properties :initform '() :reader will-properties))
  (:documentation "The message the server publishes if the client vanishes."))

(defclass connect-packet (packet)
  ((client-id :initarg :client-id :initform "" :accessor connect-client-id)
   (clean-start :initarg :clean-start :initform t :accessor connect-clean-start-p)
   (keep-alive :initarg :keep-alive :initform 60 :accessor connect-keep-alive
               :documentation "Seconds; 0 turns the mechanism off.")
   (username :initarg :username :initform nil :accessor connect-username)
   (password :initarg :password :initform nil :accessor connect-password)
   (will :initarg :will :initform nil :accessor connect-will)))

(defmethod packet-type-code ((packet connect-packet)) 1)

(defmethod encode-packet-body ((packet connect-packet) writer)
  (let ((will (connect-will packet))
        (username (connect-username packet))
        (password (connect-password packet)))
    (write-utf8-string writer "MQTT")
    (write-octet writer 5)
    (write-octet writer (logior (if username #x80 0)
                                (if password #x40 0)
                                (if (and will (will-retain-p will)) #x20 0)
                                (if will (ash (will-qos will) 3) 0)
                                (if will #x04 0)
                                (if (connect-clean-start-p packet) #x02 0)))
    (write-two-byte-integer writer (connect-keep-alive packet))
    (write-properties writer (packet-properties packet))
    (write-utf8-string writer (connect-client-id packet))
    (when will
      (write-properties writer (will-properties will))
      (write-utf8-string writer (will-topic will))
      (write-binary-data writer (will-payload will)))
    (when username (write-utf8-string writer username))
    (when password (write-binary-data writer password))))

(defmethod decode-packet-body ((packet connect-packet) flags reader)
  (declare (ignore flags))
  (unless (string= "MQTT" (read-utf8-string reader))
    (malformed "not an MQTT CONNECT"))
  (unless (= 5 (read-octet reader))
    (malformed "protocol level other than 5"))
  (let ((connect-flags (read-octet reader)))
    (setf (connect-clean-start-p packet) (logbitp 1 connect-flags)
          (connect-keep-alive packet) (read-two-byte-integer reader)
          (packet-properties packet) (read-properties reader)
          (connect-client-id packet) (read-utf8-string reader))
    (when (logbitp 2 connect-flags)
      (let* ((properties (read-properties reader))
             (topic (read-utf8-string reader))
             (payload (read-binary-data reader)))
        (setf (connect-will packet)
              (make-instance 'will :topic topic :payload payload
                                   :qos (ldb (byte 2 3) connect-flags)
                                   :retain (logbitp 5 connect-flags)
                                   :properties properties))))
    (when (logbitp 7 connect-flags)
      (setf (connect-username packet) (read-utf8-string reader)))
    (when (logbitp 6 connect-flags)
      (setf (connect-password packet) (read-binary-data reader)))))

;;; CONNACK (3.2)

(defclass connack-packet (reasoned-packet)
  ((session-present :initarg :session-present :initform nil
                    :accessor connack-session-present-p)))

(defmethod packet-type-code ((packet connack-packet)) 2)

(defmethod encode-packet-body ((packet connack-packet) writer)
  (write-octet writer (if (connack-session-present-p packet) 1 0))
  (write-octet writer (packet-reason-code packet))
  (write-properties writer (packet-properties packet)))

(defmethod decode-packet-body ((packet connack-packet) flags reader)
  (declare (ignore flags))
  (setf (connack-session-present-p packet) (logbitp 0 (read-octet reader))
        (packet-reason-code packet) (read-octet reader)
        (packet-properties packet) (read-properties reader)))

;;; PUBLISH (3.3)

(defclass publish-packet (identified-packet)
  ((topic :initarg :topic :accessor publish-topic)
   (payload :initarg :payload :initform (make-octets 0) :accessor publish-payload)
   (qos :initarg :qos :initform 0 :accessor publish-qos)
   (retain :initarg :retain :initform nil :accessor publish-retain-p)
   (dup :initarg :dup :initform nil :accessor publish-dup-p)))

(defmethod print-object ((packet publish-packet) stream)
  (print-unreadable-object (packet stream :type t)
    (format stream "~S qos ~D~@[ #~D~]~:[~; retain~]"
            (publish-topic packet) (publish-qos packet)
            (packet-id packet) (publish-retain-p packet))))

(defmethod packet-type-code ((packet publish-packet)) 3)

(defmethod packet-flags ((packet publish-packet))
  (logior (if (publish-dup-p packet) #x08 0)
          (ash (publish-qos packet) 1)
          (if (publish-retain-p packet) #x01 0)))

(defmethod encode-packet-body ((packet publish-packet) writer)
  (write-utf8-string writer (publish-topic packet))
  (when (plusp (publish-qos packet))
    (write-two-byte-integer writer (or (packet-id packet)
                                       (malformed "PUBLISH at QoS ~D without a packet id"
                                                  (publish-qos packet)))))
  (write-properties writer (packet-properties packet))
  (write-octet-vector writer (to-octets (publish-payload packet))))

(defmethod decode-packet-body ((packet publish-packet) flags reader)
  (let ((qos (ldb (byte 2 1) flags)))
    (when (= qos 3) (malformed "PUBLISH with QoS 3"))
    (setf (publish-dup-p packet) (logbitp 3 flags)
          (publish-qos packet) qos
          (publish-retain-p packet) (logbitp 0 flags)
          (publish-topic packet) (read-utf8-string reader))
    (when (plusp qos)
      (setf (packet-id packet) (read-two-byte-integer reader)))
    (setf (packet-properties packet) (read-properties reader)
          (publish-payload packet) (read-octet-vector reader (wire-reader-remaining reader)))))

(defun publish-payload-string (packet)
  "The payload of PACKET decoded as UTF-8."
  (octets-string (publish-payload packet)))

;;; PUBACK, PUBREC, PUBREL, PUBCOMP (3.4 - 3.7): all the same shape.

(defclass acknowledgement-packet (identified-packet reasoned-packet) ()
  (:documentation "A packet id plus a reason code, with the short encodings
of sections 3.4.2.1 and 3.4.2.2."))

(defmethod print-object ((packet acknowledgement-packet) stream)
  (print-unreadable-object (packet stream :type t)
    (format stream "#~D ~A" (packet-id packet) (packet-reason packet))))

(defmethod encode-packet-body ((packet acknowledgement-packet) writer)
  (write-two-byte-integer writer (packet-id packet))
  ;; The reason code and properties may be omitted when there is nothing to
  ;; say; readers on the other side treat absence as success.
  (unless (and (zerop (packet-reason-code packet))
               (null (packet-properties packet)))
    (write-octet writer (packet-reason-code packet))
    (unless (null (packet-properties packet))
      (write-properties writer (packet-properties packet)))))

(defmethod decode-packet-body ((packet acknowledgement-packet) flags reader)
  (declare (ignore flags))
  (setf (packet-id packet) (read-two-byte-integer reader))
  (unless (wire-reader-exhausted-p reader)
    (setf (packet-reason-code packet) (read-octet reader))
    (unless (wire-reader-exhausted-p reader)
      (setf (packet-properties packet) (read-properties reader)))))

(defclass puback-packet (acknowledgement-packet) ())
(defclass pubrec-packet (acknowledgement-packet) ())
(defclass pubrel-packet (acknowledgement-packet) ())
(defclass pubcomp-packet (acknowledgement-packet) ())

(defmethod packet-type-code ((packet puback-packet)) 4)
(defmethod packet-type-code ((packet pubrec-packet)) 5)
(defmethod packet-type-code ((packet pubrel-packet)) 6)
(defmethod packet-type-code ((packet pubcomp-packet)) 7)

(defmethod packet-flags ((packet pubrel-packet)) 2)

;;; SUBSCRIBE (3.8)

(defclass subscription ()
  ((topic-filter :initarg :topic-filter :reader subscription-topic-filter)
   (qos :initarg :qos :initform 0 :reader subscription-qos)
   (no-local :initarg :no-local :initform nil :reader subscription-no-local-p)
   (retain-as-published :initarg :retain-as-published :initform nil
                        :reader subscription-retain-as-published-p)
   (retain-handling :initarg :retain-handling :initform 0
                    :reader subscription-retain-handling
                    :documentation "0 send retained, 1 only if new, 2 never."))
  (:documentation "One topic filter and its options (section 3.8.3.1)."))

(defmethod print-object ((subscription subscription) stream)
  (print-unreadable-object (subscription stream :type t)
    (format stream "~S qos ~D" (subscription-topic-filter subscription)
            (subscription-qos subscription))))

(defun subscription (designator)
  "DESIGNATOR as a subscription: a string is a filter at QoS 0, a list is
(FILTER &rest options), and a subscription is itself."
  (etypecase designator
    (subscription designator)
    (string (make-instance 'subscription :topic-filter designator))
    (cons (apply #'make-instance 'subscription :topic-filter (first designator)
                 (rest designator)))))

(defun subscription-options (subscription)
  (logior (subscription-qos subscription)
          (if (subscription-no-local-p subscription) #x04 0)
          (if (subscription-retain-as-published-p subscription) #x08 0)
          (ash (subscription-retain-handling subscription) 4)))

(defclass subscribe-packet (identified-packet)
  ((subscriptions :initarg :subscriptions :initform '()
                  :accessor subscribe-subscriptions)))

(defmethod packet-type-code ((packet subscribe-packet)) 8)
(defmethod packet-flags ((packet subscribe-packet)) 2)

(defmethod encode-packet-body ((packet subscribe-packet) writer)
  (write-two-byte-integer writer (packet-id packet))
  (write-properties writer (packet-properties packet))
  (when (null (subscribe-subscriptions packet))
    (malformed "SUBSCRIBE without a topic filter"))
  (dolist (subscription (subscribe-subscriptions packet))
    (write-utf8-string writer (subscription-topic-filter subscription))
    (write-octet writer (subscription-options subscription))))

(defmethod decode-packet-body ((packet subscribe-packet) flags reader)
  (declare (ignore flags))
  (setf (packet-id packet) (read-two-byte-integer reader)
        (packet-properties packet) (read-properties reader)
        (subscribe-subscriptions packet)
        (loop until (wire-reader-exhausted-p reader)
              collect (let ((filter (read-utf8-string reader))
                            (options (read-octet reader)))
                        (make-instance 'subscription
                                       :topic-filter filter
                                       :qos (ldb (byte 2 0) options)
                                       :no-local (logbitp 2 options)
                                       :retain-as-published (logbitp 3 options)
                                       :retain-handling (ldb (byte 2 4) options))))))

;;; SUBACK and UNSUBACK (3.9, 3.11): a packet id and one reason code per filter.

(defclass reason-list-packet (identified-packet)
  ((reason-codes :initarg :reason-codes :initform '() :accessor packet-reason-codes)))

(defmethod print-object ((packet reason-list-packet) stream)
  (print-unreadable-object (packet stream :type t)
    (format stream "#~D ~{~A~^ ~}" (packet-id packet)
            (mapcar #'reason-code-name (packet-reason-codes packet)))))

(defmethod encode-packet-body ((packet reason-list-packet) writer)
  (write-two-byte-integer writer (packet-id packet))
  (write-properties writer (packet-properties packet))
  (dolist (code (packet-reason-codes packet))
    (write-octet writer code)))

(defmethod decode-packet-body ((packet reason-list-packet) flags reader)
  (declare (ignore flags))
  (setf (packet-id packet) (read-two-byte-integer reader)
        (packet-properties packet) (read-properties reader)
        (packet-reason-codes packet)
        (loop until (wire-reader-exhausted-p reader) collect (read-octet reader))))

(defclass suback-packet (reason-list-packet) ())
(defclass unsuback-packet (reason-list-packet) ())

(defmethod packet-type-code ((packet suback-packet)) 9)
(defmethod packet-type-code ((packet unsuback-packet)) 11)

;;; UNSUBSCRIBE (3.10)

(defclass unsubscribe-packet (identified-packet)
  ((topic-filters :initarg :topic-filters :initform '()
                  :accessor unsubscribe-topic-filters)))

(defmethod packet-type-code ((packet unsubscribe-packet)) 10)
(defmethod packet-flags ((packet unsubscribe-packet)) 2)

(defmethod encode-packet-body ((packet unsubscribe-packet) writer)
  (write-two-byte-integer writer (packet-id packet))
  (write-properties writer (packet-properties packet))
  (when (null (unsubscribe-topic-filters packet))
    (malformed "UNSUBSCRIBE without a topic filter"))
  (dolist (filter (unsubscribe-topic-filters packet))
    (write-utf8-string writer filter)))

(defmethod decode-packet-body ((packet unsubscribe-packet) flags reader)
  (declare (ignore flags))
  (setf (packet-id packet) (read-two-byte-integer reader)
        (packet-properties packet) (read-properties reader)
        (unsubscribe-topic-filters packet)
        (loop until (wire-reader-exhausted-p reader) collect (read-utf8-string reader))))

;;; PINGREQ, PINGRESP (3.12, 3.13)

(defclass pingreq-packet (packet) ())
(defclass pingresp-packet (packet) ())

(defmethod packet-type-code ((packet pingreq-packet)) 12)
(defmethod packet-type-code ((packet pingresp-packet)) 13)

(defmethod encode-packet-body ((packet pingreq-packet) writer)
  (declare (ignore writer)))
(defmethod encode-packet-body ((packet pingresp-packet) writer)
  (declare (ignore writer)))
(defmethod decode-packet-body ((packet pingreq-packet) flags reader)
  (declare (ignore flags reader)))
(defmethod decode-packet-body ((packet pingresp-packet) flags reader)
  (declare (ignore flags reader)))

;;; DISCONNECT and AUTH (3.14, 3.15): a reason code, both parts optional.

(defclass reason-only-packet (reasoned-packet) ())

(defmethod print-object ((packet reason-only-packet) stream)
  (print-unreadable-object (packet stream :type t)
    (format stream "~A" (packet-reason packet))))

(defmethod encode-packet-body ((packet reason-only-packet) writer)
  (unless (and (zerop (packet-reason-code packet))
               (null (packet-properties packet)))
    (write-octet writer (packet-reason-code packet))
    (unless (null (packet-properties packet))
      (write-properties writer (packet-properties packet)))))

(defmethod decode-packet-body ((packet reason-only-packet) flags reader)
  (declare (ignore flags))
  (unless (wire-reader-exhausted-p reader)
    (setf (packet-reason-code packet) (read-octet reader))
    (unless (wire-reader-exhausted-p reader)
      (setf (packet-properties packet) (read-properties reader)))))

(defclass disconnect-packet (reason-only-packet) ())
(defclass auth-packet (reason-only-packet) ())

(defmethod packet-type-code ((packet disconnect-packet)) 14)
(defmethod packet-type-code ((packet auth-packet)) 15)

;;;; Whole packets

(defun packet-class-for-type (code)
  "Section 2.1.2's table.  It is the specification's closed enumeration, so
it is a table and not a protocol."
  (case code
    (1 'connect-packet) (2 'connack-packet) (3 'publish-packet)
    (4 'puback-packet) (5 'pubrec-packet) (6 'pubrel-packet) (7 'pubcomp-packet)
    (8 'subscribe-packet) (9 'suback-packet) (10 'unsubscribe-packet)
    (11 'unsuback-packet) (12 'pingreq-packet) (13 'pingresp-packet)
    (14 'disconnect-packet) (15 'auth-packet)
    (t (malformed "control packet type ~D" code))))

(defun encode-packet (packet)
  "PACKET as octets: fixed header, then body."
  (let ((body (with-wire-writer (writer) (encode-packet-body packet writer))))
    (with-wire-writer (writer)
      (write-octet writer (logior (ash (packet-type-code packet) 4)
                                  (packet-flags packet)))
      (write-variable-byte-integer writer (length body))
      (write-octet-vector writer body))))

(defun decode-packet (octets &key (start 0) (end (length octets)))
  "The one packet in OCTETS between START and END."
  (multiple-value-bind (packet next) (decode-packet-prefix octets start end)
    (unless packet (malformed "incomplete packet"))
    (unless (= next end)
      (malformed "~D octets after the packet" (- end next)))
    packet))

(defun decode-packet-prefix (octets start end)
  "If OCTETS holds a whole packet starting at START, return it and the
position after it; otherwise return NIL."
  (let ((octets (if (typep octets 'octets) octets (to-octets octets))))
    (when (< (- end start) 2)
      (return-from decode-packet-prefix nil))
    (let ((first (aref octets start))
          (length 0)
          (multiplier 1)
          (position (1+ start)))
      ;; The remaining length may be cut short by a chunk boundary, which is
      ;; not malformed, only not-yet.
      (loop
        (when (>= position end)
          (return-from decode-packet-prefix nil))
        (let ((octet (aref octets position)))
          (incf length (* (logand octet #x7f) multiplier))
          (incf position)
          (unless (logbitp 7 octet) (return))
          (when (> (- position start) 4)
            (malformed "remaining length longer than four octets"))
          (setf multiplier (* multiplier 128))))
      (when (< (- end position) length)
        (return-from decode-packet-prefix nil))
      (let ((packet (make-instance (packet-class-for-type (ldb (byte 4 4) first)))))
        (decode-packet-body packet (ldb (byte 4 0) first)
                            (make-wire-reader octets :start position
                                                     :end (+ position length)))
        (values packet (+ position length))))))

;;;; The stream decoder

(defclass packet-decoder ()
  ((buffer :initform (make-array 0 :element-type 'octet :adjustable t :fill-pointer 0)
           :accessor decoder-buffer))
  (:documentation "Accumulates octets until whole packets can be cut from them."))

(defun make-packet-decoder () (make-instance 'packet-decoder))

(defun feed-decoder (decoder octets)
  "Add OCTETS to DECODER and return every packet now complete, in order."
  (let ((buffer (decoder-buffer decoder)))
    (loop for octet across octets do (vector-push-extend octet buffer))
    (let ((simple (coerce buffer 'octets))
          (position 0)
          (packets '()))
      (loop
        (multiple-value-bind (packet next)
            (decode-packet-prefix simple position (length simple))
          (unless packet (return))
          (push packet packets)
          (setf position next)))
      (when (plusp position)
        (replace buffer buffer :start2 position)
        (setf (fill-pointer buffer) (- (length simple) position)))
      (nreverse packets))))
