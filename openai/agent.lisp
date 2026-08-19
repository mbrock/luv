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

(defun make-agent (&key model instructions tools reasoning-effort reasoning-summary
                     (url "wss://api.openai.com/v1/responses")
                     (api-key (uiop:getenv "OPENAI_API_KEY")))
  "Open one OpenAI Responses WebSocket connection.

MODEL is normally a GPT-5.6 model name.  TOOLS contains TOOL instances.
The server protocol is experimental; URL is injectable so tests and local
proxies can speak the same small protocol."
  (unless (and api-key (plusp (length api-key)))
    (error 'openai-error :detail "OPENAI_API_KEY is not set"))
  (unless model
    (error 'openai-error :detail ":MODEL is required"))
  (let* ((socket (websocket-driver:make-client
                  url :additional-headers
                  `(("authorization" . ,(format nil "Bearer ~A" api-key))
                    ("content-type" . "application/json"))))
         (agent (make-instance 'agent :model model :instructions (or instructions "")
                               :tools tools :reasoning-effort reasoning-effort
                               :reasoning-summary reasoning-summary :socket socket)))
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
  "Close AGENT's WebSocket.  It is safe to call more than once."
  (unless (agent-closed-p agent)
    (setf (agent-closed-p agent) t)
    (websocket-driver:close-connection (agent-socket agent)))
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
  (append
   (list (cons "type" "response.create")
         (cons "model" (agent-model agent))
         (cons "instructions" (agent-instructions agent))
         (cons "input" input)
         (cons "tools" (encoded-tools (agent-tools agent)))
         (cons "tool_choice" "auto")
         (cons "parallel_tool_calls" t)
         (cons "stream" t)
         (cons "store" '(:false)))
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
calling thread.  Function calls run their TOOL here too, then continue with a
response.append carrying its function_call_output."
  (sb-thread:with-mutex ((agent-turn-lock agent))
    (let ((text-output "") (reasoning-output "") (usage nil) (done nil)
          (tool-append-p nil))
      (send-json agent
                 (if (agent-created-p agent)
                     (list (cons "type" "response.append")
                           (cons "input" (user-input text)))
                     (prog1 (response-create agent (user-input text))
                       (setf (agent-created-p agent) t))))
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
                      (setf text-output
                            (append-string text-output (json-value event :delta))))
                     ((or (string= type "response.reasoning_summary_text.delta")
                          (string= type "response.reasoning_text.delta"))
                      (setf reasoning-output
                            (append-string reasoning-output (json-value event :delta))))
                     ((and (string= type "response.output_item.done")
                           (let ((item (json-value event :item)))
                             (and item (string= (json-value item :type) "function_call"))))
                      (setf tool-append-p t)
                      (send-json agent
                                 (list (cons "type" "response.append")
                                       (cons "input"
                                             (list (tool-result agent (json-value event :item)))))))
                     ((and (string= type "response.output_item.done")
                           (let ((item (json-value event :item)))
                             (and item (string= (json-value item :type) "message"))))
                      (setf text-output
                            (append-string text-output
                                           (output-item-text (json-value event :item)))))
                     ((member type '("response.completed" "response.done") :test #'string=)
                      (let ((response (json-value event :response)))
                        (when response
                          (let ((id (json-value response :id)))
                            (when id (setf (agent-response-id agent) id)))
                          (setf usage (json-value response :usage))))
                      (if tool-append-p
                          (setf tool-append-p nil)
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
