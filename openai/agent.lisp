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
   (socket :initarg :socket :reader agent-socket)
   (events :initform '() :accessor agent-events)
   (event-lock :initform (sb-thread:make-mutex :name "OpenAI agent events")
               :reader agent-event-lock)
   (event-ready :initform (sb-thread:make-waitqueue :name "OpenAI agent event ready")
                :reader agent-event-ready)
   (turn-lock :initform (sb-thread:make-mutex :name "OpenAI agent turn")
              :reader agent-turn-lock)
   (created-p :initform nil :accessor agent-created-p)
   (closed-p :initform nil :accessor agent-closed-p)
   (response-id :initform nil :accessor agent-response-id))
  (:documentation
   "One persistent Responses WebSocket connection.  Turns are serialized;
the websocket reader only queues events, so tools run in the caller's thread."))

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

(defun default-api-key ()
  "OPENAI_API_KEY from the environment, else the first *API-KEY-FALLBACKS*
answer."
  (let ((env (uiop:getenv "OPENAI_API_KEY")))
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
  (let* ((socket (websocket-driver:make-client
                  url :additional-headers
                  `(("authorization" . ,(format nil "Bearer ~A" api-key))
                    ("content-type" . "application/json"))))
         (agent (apply #'make-instance class
                       :model model :instructions (or instructions "")
                       :tools tools :reasoning-effort reasoning-effort
                       :reasoning-summary reasoning-summary :socket socket
                       initargs)))
    (websocket-driver:on :message socket
                         (lambda (message)
                           (handler-case
                               (enqueue-agent-event agent (decode-json message))
                             (error (condition)
                               (enqueue-agent-event agent
                                                    (list :transport-error condition))))))
    (websocket-driver:on :error socket
                         (lambda (detail) (enqueue-agent-event agent
                                                                 (list :transport-error detail))))
    (websocket-driver:on :close socket
                         (lambda (&key code reason)
                           (enqueue-agent-event agent (list :closed code reason))))
    (websocket-driver:start-connection socket)
    agent))

(defun close-agent (agent)
  "Begin AGENT's normal WebSocket close handshake.

WEBSOCKET-DRIVER forcibly destroys its reader thread in CLOSE-CONNECTION;
doing that while SBCL is in SSL I/O can corrupt the image.  A close frame lets
the reader receive the peer's reply and finish in its own thread instead."
  (unless (agent-closed-p agent)
    (setf (agent-closed-p agent) t)
    (websocket-driver:send (agent-socket agent) "" :type :close :code 1000))
  agent)

(defun enqueue-agent-event (agent event)
  (sb-thread:with-mutex ((agent-event-lock agent))
    (setf (agent-events agent) (nconc (agent-events agent) (list event)))
    (sb-thread:condition-notify (agent-event-ready agent)))
  event)

(defun next-agent-event (agent)
  (sb-thread:with-mutex ((agent-event-lock agent))
    (loop while (null (agent-events agent))
          do (sb-thread:condition-wait (agent-event-ready agent)
                                       (agent-event-lock agent)))
    (pop (agent-events agent))))

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

(defun response-create (agent input)
  "A response.create event for INPUT, continuing from the agent's last
response when it has one: the server keeps the conversation under the
previous response id, so each event carries only what is new."
  (append
   (list (cons "type" "response.create")
         (cons "model" (agent-model agent))
         (cons "instructions" (agent-instructions agent))
         (cons "input" input))
   (let ((previous (agent-response-id agent)))
     (if previous (list (cons "previous_response_id" previous)) '()))
   (list
         (cons "tools" (encoded-tools (agent-tools agent)))
         (cons "tool_choice" "auto")
         (cons "parallel_tool_calls" t)
         (cons "stream" t))
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

(defun send-json (agent object)
  (when (agent-closed-p agent)
    (error 'agent-closed :detail "already closed"))
  (websocket-driver:send (agent-socket agent) (json-string object)))

(defun tool-for-call (agent name)
  (find name (agent-tools agent) :key #'tool-name :test #'string=))

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
                (cons "output" (if (stringp result) result (json-string result)))))
      (error (condition)
        (list (cons "type" "function_call_output")
              (cons "call_id" call-id)
              (cons "output" (json-string `(("error" . ,(princ-to-string condition))))))))))

(defun output-item-text (item)
  (with-output-to-string (stream)
    (dolist (part (json-value item :content))
      (when (string= (json-value part :type) "output_text")
        (write-string (json-value part :text) stream)))))

(defun agent-turn (agent text)
  "Send TEXT and synchronously run one complete agent turn.

Text and reasoning deltas reach HANDLE-AGENT-EVENT immediately in this
calling thread.  Function calls run their TOOL here too, as their items
complete; when the response completes with calls outstanding, a further
response.create carries every function_call_output under the previous
response id, and the loop continues until a response completes with none."
  (sb-thread:with-mutex ((agent-turn-lock agent))
    (let ((text-output "") (reasoning-output "") (usage nil) (done nil)
          (items-with-text-deltas (make-hash-table :test #'equal))
          (outputs '()))
      (send-json agent (response-create agent (user-input text)))
      (setf (agent-created-p agent) t)
      (loop until done
            for event = (next-agent-event agent)
            do (cond
                 ((and (consp event) (eq (first event) :transport-error))
                  (error 'agent-failed :detail (second event)))
                 ((and (consp event) (eq (first event) :closed))
                  (setf (agent-closed-p agent) t)
                  (error 'agent-closed :detail (third event)))
                 (t
                  (handle-agent-event agent event)
                  (let ((type (json-value event :type)))
                    (cond
                     ((string= type "response.output_text.delta")
                      (setf (gethash (json-value event :item-id)
                                     items-with-text-deltas)
                            t)
                      (setf text-output
                            (append-string text-output (json-value event :delta))))
                     ((or (string= type "response.reasoning_summary_text.delta")
                          (string= type "response.reasoning_text.delta"))
                      (setf reasoning-output
                            (append-string reasoning-output (json-value event :delta))))
                     ((and (string= type "response.output_item.done")
                           (let ((item (json-value event :item)))
                             (and item (string= (json-value item :type) "function_call"))))
                      (push (tool-result agent (json-value event :item)) outputs))
                     ((and (string= type "response.output_item.done")
                           (let ((item (json-value event :item)))
                             (and item (string= (json-value item :type) "message"))))
                      (setf text-output
                            (let ((item (json-value event :item)))
                              (if (gethash (json-value item :id)
                                           items-with-text-deltas)
                                  text-output
                                  (append-string text-output
                                                 (output-item-text item))))))
                     ((member type '("response.completed" "response.done") :test #'string=)
                      (let ((response (json-value event :response)))
                        (when response
                          (let ((id (json-value response :id)))
                            (when id (setf (agent-response-id agent) id)))
                          (setf usage (json-value response :usage))))
                      (if outputs
                          (progn
                            (send-json agent (response-create agent (reverse outputs)))
                            (setf outputs '()))
                          (setf done t)))
                     ((string= type "response.failed")
                      (error 'agent-failed :detail (json-value event :response)))
                     ((string= type "error")
                      (error 'agent-failed :detail (or (json-value event :error) event))))))))
      (make-instance 'agent-response :text text-output :reasoning reasoning-output
                     :id (agent-response-id agent) :usage usage))))

(defun append-string (string suffix)
  (if suffix
      (concatenate 'string string suffix)
      string))
