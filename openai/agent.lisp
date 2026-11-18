(in-package #:openai)

(define-condition openai-error (error)
  ((detail :initarg :detail :reader openai-error-detail))
  (:report (lambda (condition stream)
             (format stream "OpenAI: ~A" (openai-error-detail condition)))))

(define-condition agent-closed (openai-error) ()
  (:report (lambda (condition stream)
             (format stream "The OpenAI agent connection closed~@[ (~A)~]."
                     (openai-error-detail condition)))))

(define-condition agent-failed (openai-error) ()
  (:report (lambda (condition stream)
             (format stream "The OpenAI response failed: ~A"
                     (openai-error-detail condition)))))

(define-condition connection-interrupted (openai-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The connection was interrupted. Try talking to me again." stream))))

(defparameter *response-idle-timeout* 120
  "Seconds to wait for another provider event. Player decisions and tools have no deadline.")

(defparameter *reconnect-delays* '(0.25 1)
  "Delays before retrying one interrupted provider request.")

(define-condition missing-api-key (openai-error) ()
  (:default-initargs :detail "OPENAI_API_KEY is not set and no fallback supplied one")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "No OpenAI API key is available.")))
  (:documentation
   "No API key was available when an agent was opened.  The RETRY restart asks
the environment and registered fallbacks again; USE-VALUE supplies one for
this connection."))

(defclass tool ()
  ((name :initarg :name :reader tool-name)
   (description :initarg :description :initform "" :reader tool-description)
   (parameters :initarg :parameters :initform '(("type" . "object"))
               :reader tool-parameters))
  (:documentation
   "A client function exposed to an OpenAI agent.  PARAMETERS is a JSON Schema
object represented as a CL-JSON alist.  Specialize CALL-TOOL for behavior."))

(defgeneric call-tool (tool arguments agent)
  (:documentation "Run TOOL with decoded JSON ARGUMENTS for AGENT."))

(defclass tool-output-image ()
  ((octets :initarg :octets :reader tool-output-image-octets)
   (media-type :initarg :media-type :initform "image/png"
               :reader tool-output-image-media-type)
   (detail :initarg :detail :initform "low" :reader tool-output-image-detail))
  (:documentation "One encoded image carried back as part of a function result."))

(defun make-tool-output-image (octets &key (media-type "image/png") (detail "low"))
  "Make an image result from encoded OCTETS such as the bytes of a PNG."
  (check-type octets (vector (unsigned-byte 8)))
  (make-instance 'tool-output-image
                 :octets octets :media-type media-type :detail detail))

(defclass tool-output ()
  ((text :initarg :text :initform "" :reader tool-output-text)
   (images :initarg :images :initform '() :reader tool-output-images))
  (:documentation
   "Text plus encoded images returned by a client function to the model."))

(defun make-tool-output (&key (text "") images)
  "Make a multimodal function result whose TEXT and IMAGES travel together."
  (make-instance 'tool-output :text text :images images))

(defclass agent-response ()
  ((text :initarg :text :reader agent-response-text)
   (reasoning :initarg :reasoning :reader agent-response-reasoning)
   (id :initarg :id :reader agent-response-id)
   (usage :initarg :usage :reader agent-response-usage))
  (:documentation "The accumulated result of one AGENT-TURN."))

(defclass agent ()
  ((model :initarg :model :reader agent-model)
   (instructions :initarg :instructions :initform "" :reader agent-instructions)
   (tools :initarg :tools :initform '() :reader agent-tools)
   (reasoning-effort :initarg :reasoning-effort :initform nil
                     :reader agent-reasoning-effort)
   (reasoning-summary :initarg :reasoning-summary :initform nil
                      :reader agent-reasoning-summary)
   ;; "fast" asks for the faster, dearer processing tier where the model
   ;; offers one; NIL leaves the request on the account's default tier.
   (service-tier :initarg :service-tier :initform nil
                 :reader agent-service-tier)
   (socket :initarg :socket :initform nil :accessor agent-socket)
   (url :initarg :url :initform nil :reader agent-url)
   (api-key :initarg :api-key :initform nil :reader agent-api-key)
   (connected-at :initform nil :accessor agent-connected-at)
   (events :initform '() :accessor agent-events)
   (event-lock :initform (sb-thread:make-mutex :name "OpenAI agent events")
               :reader agent-event-lock)
   (event-ready :initform (sb-thread:make-waitqueue :name "OpenAI agent event ready")
                :reader agent-event-ready)
   (turn-lock :initform (sb-thread:make-mutex :name "OpenAI agent turn")
              :reader agent-turn-lock)
   (created-p :initform nil :accessor agent-created-p)
   (closed-p :initform nil :accessor agent-closed-p)
   ;; A completed tool round awaiting delivery, including its exact local results.
   (pending-round :initform nil :accessor agent-pending-round)
   (response-id :initform nil :accessor agent-response-id))
  (:documentation
   "One persistent Responses conversation over a WebSocket connection.  Turns
are serialized; the websocket reader only queues events, so tools run in the
caller's thread.  The conversation outlives its connection: RESPONSE-ID names
the last cleanly finished turn, and a turn that finds the connection gone or
old opens a fresh one and continues from there."))

(defgeneric handle-agent-event (agent event)
  (:documentation
   "Observe one decoded Responses event in the thread executing AGENT-TURN.
The default method does nothing; applications may add methods on agent
subclasses for live text, reasoning, tool, or lifecycle presentation."))

(defmethod handle-agent-event ((agent agent) event)
  (declare (ignore agent event))
  nil)

(defvar *api-key-fallbacks* '()
  "Functions of no arguments tried in order for an API key when OPENAI_API_KEY
is unset; the first non-empty string wins.  Applications push providers here
(luvcraft asks the tailnet lobby).")

(defvar *api-key-environment-provider*
  (lambda () (uiop:getenv "OPENAI_API_KEY"))
  "Function used to read OPENAI_API_KEY.  Kept indirect so recovery behavior
can be tested without depending on or mutating the host process environment.")

(defun default-api-key ()
  "OPENAI_API_KEY from the environment, else the first *API-KEY-FALLBACKS*
answer."
  (let ((env (funcall *api-key-environment-provider*)))
    (if (and env (plusp (length env)))
        env
        (loop for fallback in *api-key-fallbacks*
              for key = (ignore-errors (funcall fallback))
              when (and (stringp key) (plusp (length key)))
                return key))))

(defun ensure-api-key (key)
  "Return a non-empty API key, offering semantic recovery when none exists."
  (loop
    (when (and (stringp key) (plusp (length key)))
      (return key))
    (restart-case (error 'missing-api-key)
      (retry ()
        :report "Ask OPENAI_API_KEY and the registered fallbacks again."
        (setf key (default-api-key)))
      (use-value (value)
        :report "Supply an API key for this connection."
        (setf key value)))))

(defun make-agent (&key model instructions tools reasoning-effort reasoning-summary
                     service-tier
                     (url "wss://api.openai.com/v1/responses")
                     (api-key (default-api-key))
                     (class 'agent) initargs)
  "Open one OpenAI Responses WebSocket connection.

MODEL is normally a GPT-5.6 model name.  TOOLS contains TOOL instances.
The server protocol is experimental; URL is injectable so tests and local
proxies can speak the same small protocol.  CLASS names the AGENT subclass
to make, with INITARGS for its own slots, so an application can specialize
HANDLE-AGENT-EVENT and CALL-TOOL on its own agent."
  (setf api-key (ensure-api-key api-key))
  (unless model
    (error 'openai-error :detail ":MODEL is required"))
  (let ((agent (apply #'make-instance class
                      :model model :instructions (or instructions "")
                      :tools tools :reasoning-effort reasoning-effort
                      :reasoning-summary reasoning-summary :service-tier service-tier
                      :url url :api-key api-key initargs))
        (delays *reconnect-delays*))
    (loop
      (handler-case (return (connect-agent agent))
        (connection-interrupted (condition)
          (unless delays (error condition))
          (sleep (pop delays)))))))

(defparameter *connection-lifetime* (* 55 60)
  "Age in seconds at which the next response.create opens a fresh connection.")

(defun connect-agent (agent)
  "Open a fresh WebSocket for AGENT, replacing any previous one.

Events from a replaced socket are dropped, so a late close or error from the
old connection cannot end a turn on the new one."
  (let ((socket (websocket-driver:make-client
                 (agent-url agent)
                 :additional-headers
                 `(("authorization" . ,(format nil "Bearer ~A" (agent-api-key agent)))
                   ("content-type" . "application/json")))))
    (flet ((enqueue (event)
             (enqueue-agent-event agent event socket)))
      (websocket-driver:on :message socket
                           (lambda (message)
                             (handler-case (enqueue (decode-json message))
                               (error (condition)
                                 (enqueue (list :transport-error condition))))))
      (websocket-driver:on :error socket
                           (lambda (detail) (enqueue (list :transport-error detail))))
      (websocket-driver:on :close socket
                           (lambda (&key code reason)
                             (enqueue (list :closed code reason)))))
    (let ((old (agent-socket agent)))
      (sb-thread:with-mutex ((agent-event-lock agent))
        (when (agent-closed-p agent) (error 'agent-closed :detail "already closed"))
        (setf (agent-socket agent) socket
              (agent-events agent) '()))
      (when (and old (eq :open (websocket-driver:ready-state old)))
        (ignore-errors
         (websocket-driver:send old "" :type :close :code 1000))))
    (handler-case (sb-sys:with-deadline (:seconds 20)
                    (websocket-driver:start-connection socket))
      ((or sb-sys:deadline-timeout stream-error usocket:socket-error usocket:ns-error) (condition)
        (error 'connection-interrupted :detail condition)))
    (when (agent-closed-p agent)
      (ignore-errors (websocket-driver:send socket "" :type :close :code 1000))
      (error 'agent-closed :detail "already closed"))
    (setf (agent-connected-at agent) (get-universal-time))
    agent))

(defgeneric ensure-agent-connection (agent)
  (:documentation "Prepare AGENT's connection for the next response.create."))

(defmethod ensure-agent-connection ((agent agent))
  "Each request follows a terminal response or starts on a fresh socket.
Queued events between requests, including a close during a tool, are stale."
  (when (agent-closed-p agent)
    (error 'agent-closed :detail "already closed"))
  (sb-thread:with-mutex ((agent-event-lock agent))
    (when (some (lambda (event) (or (member (first event) '(:closed :transport-error))
                                    (equal (json-value event :type) "error")))
                (agent-events agent))
      (setf (agent-connected-at agent) nil))
    (setf (agent-events agent) '()))
  (let ((socket (agent-socket agent))
        (connected-at (agent-connected-at agent)))
    (unless (and socket
                 (eq :open (websocket-driver:ready-state socket))
                 connected-at
                 (< (- (get-universal-time) connected-at) *connection-lifetime*))
      (connect-agent agent)))
  agent)

(defun close-agent (agent)
  "Begin AGENT's normal WebSocket close handshake.

WEBSOCKET-DRIVER forcibly destroys its reader thread in CLOSE-CONNECTION;
doing that while SBCL is in SSL I/O can corrupt the image.  A close frame lets
the reader receive the peer's reply and finish in its own thread instead."
  (let ((socket (sb-thread:with-mutex ((agent-event-lock agent))
                  (unless (agent-closed-p agent)
                    (setf (agent-closed-p agent) t)
                    (sb-thread:condition-broadcast (agent-event-ready agent))
                    (agent-socket agent)))))
    (when (and socket (eq :open (websocket-driver:ready-state socket)))
      (websocket-driver:send socket "" :type :close :code 1000)))
  agent)

(defun enqueue-agent-event (agent event &optional socket)
  (sb-thread:with-mutex ((agent-event-lock agent))
    (when (or (null socket) (eq socket (agent-socket agent)))
      (setf (agent-events agent) (nconc (agent-events agent) (list event)))
      (sb-thread:condition-notify (agent-event-ready agent))))
  event)

(defun next-agent-event (agent)
  (sb-thread:with-mutex ((agent-event-lock agent))
    (loop
      (when (agent-closed-p agent) (error 'agent-closed :detail "already closed"))
      (when (agent-events agent) (return (pop (agent-events agent))))
      (unless (sb-thread:condition-wait (agent-event-ready agent) (agent-event-lock agent)
                                        :timeout *response-idle-timeout*)
        (error 'connection-interrupted :detail "Timed out waiting for a response event.")))))

(defun json-key-name (key)
  (remove-if-not #'alphanumericp (string key)))

(defun json-value (object key)
  "The decoder maps JSON underscores to Lisp hyphens (and can leave doubled
hyphens), so compare keys in their punctuation-free spelling."
  (cdr (find key object :key #'car
                 :test (lambda (wanted actual)
                         (string-equal (json-key-name wanted)
                                       (json-key-name actual))))))

(defun json-object (&rest pairs)
  (loop for (key value) on pairs by #'cddr
        when value collect (cons key value)))

(defun json-string (value)
  (cl-json:encode-json-to-string value))

(defclass json-false () ()
  (:documentation "CL-JSON spells NIL as null; this is the value that spells false."))

(defmethod cl-json:encode-json ((object json-false) &optional (stream cl-json:*json-output*))
  (declare (ignore object))
  (write-string "false" stream)
  nil)

(defparameter +json-false+ (make-instance 'json-false)
  "JSON false, for schema fields such as additionalProperties.")

(defun decode-json (string)
  (cl-json:decode-json-from-string string))

(defun encoded-tools (tools)
  (mapcar (lambda (tool)
            (json-object "type" "function"
                         "name" (tool-name tool)
                         "description" (tool-description tool)
                         "parameters" (tool-parameters tool)
                         "strict" t))
          tools))

(defun response-create (agent input &optional (previous (agent-response-id agent)))
  "A response.create event for INPUT, continuing from PREVIOUS, by default the
agent's last finished turn: the server keeps the conversation under the
previous response id, so each event carries only what is new."
  (append
   (list (cons "type" "response.create")
         (cons "model" (agent-model agent))
         (cons "instructions" (agent-instructions agent))
         (cons "input" input))
   (if previous (list (cons "previous_response_id" previous)) '())
   (list
         (cons "tools" (encoded-tools (agent-tools agent)))
         (cons "tool_choice" "auto")
         (cons "parallel_tool_calls" t)
         (cons "stream" t))
   (when (agent-reasoning-effort agent)
     (list (cons "include" #("reasoning.encrypted_content"))))
   (if (agent-service-tier agent)
       (list (cons "service_tier" (agent-service-tier agent)))
       '())
   (let ((reasoning (json-object "effort" (agent-reasoning-effort agent)
                                 "summary" (agent-reasoning-summary agent))))
     (if reasoning (list (cons "reasoning" reasoning)) '()))))

(defun user-input (text)
  (list
   (list (cons "role" "user")
         (cons "content"
               (list
                (list (cons "type" "input_text")
                      (cons "text" text)))))))

(defgeneric send-json (agent object)
  (:documentation "Send OBJECT as one JSON message on AGENT's connection."))

(defmethod send-json :before ((agent agent) object)
  (declare (ignore object))
  ;; A local tool can wait for its player longer than the socket lives.
  (ensure-agent-connection agent))

(defmethod send-json ((agent agent) object)
  (when (agent-closed-p agent)
    (error 'agent-closed :detail "already closed"))
  (let ((message (json-string object)))
    (handler-case (sb-sys:with-deadline (:seconds 20)
                    (websocket-driver:send (agent-socket agent) message))
      (error (condition) (error 'connection-interrupted :detail condition)))
    ;; The driver can silently drop a write to a closing socket.
    (unless (eq :open (websocket-driver:ready-state (agent-socket agent)))
      (error 'connection-interrupted :detail "Socket closed during send."))))

(defun tool-for-call (agent name)
  (find name (agent-tools agent) :key #'tool-name :test #'string=))

(defgeneric encoded-tool-output (output)
  (:documentation "Return OUTPUT in the Responses function-call-output shape."))

(defmethod encoded-tool-output ((output string))
  output)

(defmethod encoded-tool-output ((output t))
  (json-string output))

(defun tool-output-image-data-url (image)
  (format nil "data:~A;base64,~A"
          (tool-output-image-media-type image)
          (cl-base64:usb8-array-to-base64-string
           (tool-output-image-octets image))))

(defmethod encoded-tool-output ((output tool-output))
  (append
   (unless (string= (tool-output-text output) "")
     (list (json-object "type" "input_text"
                        "text" (tool-output-text output))))
   (mapcar (lambda (image)
             (json-object "type" "input_image"
                          "image_url" (tool-output-image-data-url image)
                          "detail" (tool-output-image-detail image)))
           (tool-output-images output))))

(defun tool-result (agent item)
  (let* ((call-id (json-value item :call-id))
         (name (json-value item :name))
         (arguments (or (json-value item :arguments) "{}"))
         (tool (tool-for-call agent name)))
    (unless tool
      (error 'agent-failed :detail (format nil "unknown tool ~S" name)))
    (handler-case
        (let ((result (call-tool tool (decode-json arguments) agent)))
          (list (cons "type" "function_call_output")
                (cons "call_id" call-id)
                (cons "output" (encoded-tool-output result))))
      (error (condition)
        (list (cons "type" "function_call_output")
              (cons "call_id" call-id)
              (cons "output" (json-string `(("error" . ,(princ-to-string condition))))))))))

(defun output-item-text (item)
  (with-output-to-string (stream)
    (dolist (part (json-value item :content))
      (when (string= (json-value part :type) "output_text")
        (write-string (json-value part :text) stream)))))

(defstruct response-round
  ;; INPUT is incremental under PARENT. REPLAY holds the current turn back to
  ;; BASE in case the server no longer recognizes an intermediate response.
  parent input base replay)

(defun receive-response (agent)
  "Read one generation to completion before dispatching any local tool."
  (let ((text "") (reasoning "") (items '()) (text-items (make-hash-table :test #'equal)))
    (loop for event = (next-agent-event agent) do
      (when (member (first event) '(:closed :transport-error))
        (error 'connection-interrupted :detail (rest event)))
      (let ((type (json-value event :type)))
        (cond
          ((or (member type '("error" "response.failed" "response.incomplete") :test #'equal)
               (and (equal type "response.done")
                    (member (json-value (json-value event :response) :status)
                            '("failed" "cancelled" "incomplete") :test #'equal)))
           (let* ((response (json-value event :response))
                  (detail (or (json-value event :error) (json-value response :error) response event)))
             (error (if (member (json-value detail :code)
                                '("websocket_connection_limit_reached" "websocket_connection_timeout"
                                  "websocket_timeout" "request_timeout") :test #'equal)
                        'connection-interrupted 'agent-failed)
                    :detail detail)))
          ((equal type "response.output_text.delta")
           (setf (gethash (json-value event :item-id) text-items) t
                 text (append-string text (json-value event :delta))))
          ((member type '("response.reasoning_summary_text.delta" "response.reasoning_text.delta")
                   :test #'equal)
           (setf reasoning (append-string reasoning (json-value event :delta))))
          ((equal type "response.output_item.done")
           (push (json-value event :item) items)))
        (handle-agent-event agent event)
        (when (member type '("response.completed" "response.done") :test #'equal)
          (let* ((response (json-value event :response))
                 (output (or (json-value response :output) (nreverse items))))
            (unless (json-value response :id)
              (error 'agent-failed :detail "Completed response has no id."))
            (loop for item in output
                  when (and (equal "message" (json-value item :type))
                            (not (gethash (json-value item :id) text-items)))
                    do (setf text (append-string text (output-item-text item))))
            (return (values response output text reasoning))))))))

(defun request-round (agent round)
  "Retry only the current response.create. No tools run inside a retry."
  (let ((delays *reconnect-delays*))
    (loop
      (when (agent-closed-p agent) (error 'agent-closed :detail "already closed"))
      (handler-case
          (progn
            (handle-agent-event agent (json-object "type" "client.response.start"))
            (send-json agent (response-create agent (response-round-input round)
                                              (response-round-parent round)))
            (setf (agent-created-p agent) t)
            (return (receive-response agent)))
        (connection-interrupted (condition)
          (setf (agent-connected-at agent) nil)
          (when (agent-closed-p agent) (error 'agent-closed :detail "already closed"))
          (unless delays (error condition))
          (handle-agent-event agent (json-object "type" "client.response.retry"))
          (sleep (pop delays)))
        (agent-failed (condition)
          (unless (and (response-round-parent round)
                       (consp (openai-error-detail condition))
                       (equal "previous_response_not_found"
                              (json-value (openai-error-detail condition) :code)))
            (error condition))
          ;; Keep this turn's executed calls paired with their results. If
          ;; its base is also gone (e.g. an old save), drop that pointer too.
          (setf (response-round-parent round)
                (unless (equal (response-round-parent round) (response-round-base round))
                  (response-round-base round))
                (response-round-input round) (response-round-replay round))
          (handle-agent-event agent (json-object "type" "client.response.retry")))))))

(defun replay-output-item (item)
  "Recover input items from a completed round without relying on remote item IDs."
  (let ((type (json-value item :type)))
    (cond
      ((equal type "function_call")
       (json-object "type" type "call_id" (json-value item :call-id)
                    "name" (json-value item :name) "arguments" (json-value item :arguments)))
      ((equal type "message")
       (json-object "role" "assistant" "content" (output-item-text item)))
      ((and (equal type "reasoning") (json-value item :encrypted-content))
       (json-object "type" type "summary" #()
                    "encrypted_content" (json-value item :encrypted-content))))))

(defun agent-turn (agent text)
  "Run each completed batch of tools once, retaining its results through reconnects."
  (sb-thread:with-mutex ((agent-turn-lock agent))
    (let* ((user (user-input text))
           (pending (agent-pending-round agent))
           (round (if pending (copy-response-round pending)
                      (make-response-round :base (agent-response-id agent)
                                           :parent (agent-response-id agent))))
           (text-output "") (reasoning-output ""))
      (setf (response-round-input round) (append (response-round-input round) user)
            (response-round-replay round) (append (response-round-replay round) user))
      (loop
        (multiple-value-bind (response output words reasoning) (request-round agent round)
          (let ((calls (remove-if-not (lambda (item) (equal "function_call" (json-value item :type))) output))
                (id (json-value response :id)))
            (setf text-output (append-string text-output words)
                  reasoning-output (append-string reasoning-output reasoning))
            (unless calls
              (setf (agent-response-id agent) id (agent-pending-round agent) nil)
              (return (make-instance 'agent-response :text text-output :reasoning reasoning-output
                                     :id id :usage (json-value response :usage))))
            (when (agent-closed-p agent) (error 'agent-closed :detail "already closed"))
            (let* ((results (mapcar (lambda (call) (tool-result agent call)) calls))
                   (replay (append (response-round-replay round)
                                   (remove nil (mapcar #'replay-output-item output)) results)))
              (setf round (make-response-round :parent id :input results
                                               :base (response-round-base round) :replay replay)
                    (agent-pending-round agent) round))))))))

(defun append-string (string suffix)
  (if suffix
      (concatenate 'string string suffix)
      string))
