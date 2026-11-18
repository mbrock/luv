(in-package #:openai.tests)

(define-test response-create-shape
  (let ((agent (make-instance 'openai:agent
                              :model "gpt-5.6-terra"
                              :instructions "Be useful."
                              :tools '()
                              :socket nil)))
    (let ((request (openai::response-create agent (openai::user-input "hello"))))
      (true (string= "response.create" (cdr (assoc "type" request :test #'string=))))
      (true (string= "gpt-5.6-terra" (cdr (assoc "model" request :test #'string=))))
      (true (search "input_text" (openai::json-string request)))
      (true (null (assoc "previous_response_id" request :test #'string=)))
      (setf (openai:agent-response-id agent) "resp_1")
      (true (string= "resp_1"
                     (cdr (assoc "previous_response_id"
                                 (openai::response-create agent (openai::user-input "again"))
                                 :test #'string=)))))))

(define-test response-create-carries-tier-and-effort-only-when-asked
  (flet ((request (&rest initargs)
           (openai::response-create
            (apply #'make-instance 'openai:agent :model "gpt-6.1-sol"
                   :tools '() :socket nil initargs)
            (openai::user-input "hello"))))
    (let ((plain (request))
          (fast (request :service-tier "fast" :reasoning-effort "medium")))
      (true (null (assoc "service_tier" plain :test #'string=)))
      (true (null (assoc "reasoning" plain :test #'string=)))
      (true (string= "fast" (cdr (assoc "service_tier" fast :test #'string=))))
      (true (search "\"effort\":\"medium\"" (openai::json-string fast))))))

(defclass echo-tool (openai:tool) ())

(defmethod openai:call-tool ((tool echo-tool) arguments agent)
  (declare (ignore tool agent))
  arguments)

(define-test tool-result-shape
  (let* ((tool (make-instance 'echo-tool :name "echo"))
         (agent (make-instance 'openai:agent :model "gpt-5.6-terra"
                               :tools (list tool) :socket nil))
         (output (openai::tool-result
                  agent '((:call-id . "call_1") (:name . "echo")
                          (:arguments . "{\"x\":1}")))))
    (true (string= "function_call_output"
                   (cdr (assoc "type" output :test #'string=))))
    (true (string= "call_1" (cdr (assoc "call_id" output :test #'string=))))))

(defclass picture-tool (openai:tool) ())

(defmethod openai:call-tool ((tool picture-tool) arguments agent)
  (declare (ignore tool arguments agent))
  (openai:make-tool-output
   :text "a tiny picture"
   :images
   (list (openai:make-tool-output-image
          (make-array 4 :element-type '(unsigned-byte 8)
                        :initial-contents '(137 80 78 71))))))

(define-test multimodal-tool-results-keep-text-and-image-together
  (let* ((tool (make-instance 'picture-tool :name "picture"))
         (agent (make-instance 'openai:agent :model "gpt-5.6-terra"
                               :tools (list tool) :socket nil))
         (item (openai::tool-result
                agent '((:call-id . "call_2") (:name . "picture")
                        (:arguments . "{}"))))
         (content (cdr (assoc "output" item :test #'string=))))
    (true (= 2 (length content)))
    (true (string= "input_text"
                   (cdr (assoc "type" (first content) :test #'string=))))
    (true (string= "input_image"
                   (cdr (assoc "type" (second content) :test #'string=))))
    (true (search "data:image/png;base64,iVBORw=="
                  (cdr (assoc "image_url" (second content) :test #'string=))))))

(define-test missing-api-key-is-restartable
  (true (string= "from-restart"
                 (handler-bind
                     ((openai:missing-api-key
                        (lambda (condition)
                          (declare (ignore condition))
                          (invoke-restart 'use-value "from-restart"))))
                   (openai::ensure-api-key nil))))
  (let* ((calls 0)
         (openai::*api-key-environment-provider* (constantly nil))
         (openai:*api-key-fallbacks*
           (list (lambda ()
                   (when (> (incf calls) 1) "from-retry")))))
    (true (string= "from-retry"
                   (handler-bind
                       ((openai:missing-api-key
                          (lambda (condition)
                            (declare (ignore condition))
                            (invoke-restart 'openai:retry))))
                     (openai::ensure-api-key (openai:default-api-key)))))))

;;; A scripted connection: each request is answered with the next canned
;;; response from SCRIPT, so a turn's bookkeeping runs without a network.

(defclass scripted-agent (openai:agent)
  ((script :initarg :script :accessor scripted-agent-script)
   (requests :initform '() :accessor scripted-agent-requests)))

(defmethod openai::ensure-agent-connection ((agent scripted-agent))
  agent)

(defmethod openai::send-json ((agent scripted-agent) object)
  (push object (scripted-agent-requests agent))
  (dolist (event (pop (scripted-agent-script agent)))
    (openai::enqueue-agent-event agent event)))

(defun scripted-completion (id)
  `((:type . "response.completed") (:response (:id . ,id))))

(defun scripted-call (call-id)
  `((:type . "response.output_item.done")
    (:item (:type . "function_call") (:call-id . ,call-id)
           (:name . "echo") (:arguments . "{}"))))

(defun request-previous (request)
  (cdr (assoc "previous_response_id" request :test #'string=)))

(defclass counting-tool (echo-tool)
  ((count :initform 0 :accessor tool-count)))

(defmethod openai:call-tool :before ((tool counting-tool) arguments agent)
  (declare (ignore arguments agent))
  (incf (tool-count tool)))

(defun scripted-error (code)
  (openai::json-object "type" "error" "error" (openai::json-object "code" code)))

(define-test reconnect-resumes-after-the-tool-only-once
  (let* ((openai::*reconnect-delays* '(0 0))
         (tool (make-instance 'counting-tool :name "echo"))
         (agent (make-instance 'scripted-agent :model "test" :tools (list tool)
                               :script (list
                                        ;; An incomplete generation cannot start a tool.
                                        (list (scripted-call "abandoned") '(:closed 1006 "gone"))
                                        (list (scripted-call "approved") (scripted-completion "resp_tool"))
                                        (list (scripted-error "websocket_connection_limit_reached"))
                                        (list (scripted-error "previous_response_not_found"))
                                        (list (scripted-completion "resp_ok"))))))
    (setf (openai:agent-response-id agent) "base")
    (openai:agent-turn agent "build")
    (destructuring-bind (replay retry continuation regenerated initial) (scripted-agent-requests agent)
      (true (equal initial regenerated))
      (true (equal retry continuation))
      (true (equal "resp_tool" (request-previous retry)))
      (true (equal "base" (request-previous replay)))
      (let ((input (openai::json-value replay :input)))
        (true (= 3 (length input)))
        (true (equal "function_call" (openai::json-value (second input) :type)))
        (true (equal "function_call_output" (openai::json-value (third input) :type)))
        (true (every (lambda (item) (equal "approved" (openai::json-value item :call-id))) (rest input)))))
    (true (= 1 (tool-count tool)))
    (true (equal "resp_ok" (openai:agent-response-id agent)))))

(define-test failed-completion-does-not-dispatch-its-tools
  (let* ((tool (make-instance 'counting-tool :name "echo"))
         (agent (make-instance 'scripted-agent :model "test" :tools (list tool)
                               :script (list (list (scripted-call "a")
                                                   '((:type . "response.done")
                                                     (:response (:status . "incomplete"))))))))
    (fail (openai:agent-turn agent "build") 'openai:agent-failed)
    (true (zerop (tool-count tool)))))

(define-test silent-connection-has-a-bound-and-keeps-settled-tools
  (let* ((openai::*response-idle-timeout* 0.02)
         (openai::*reconnect-delays* '(0))
         (tool (make-instance 'counting-tool :name "echo"))
         (agent (make-instance 'scripted-agent :model "test" :tools (list tool)
                               :script (list (list (scripted-call "a") (scripted-completion "r"))
                                             nil nil))))
    (fail (openai:agent-turn agent "build") 'openai::connection-interrupted)
    (true (= 3 (length (scripted-agent-requests agent))))
    (setf (scripted-agent-script agent) (list (list (scripted-completion "done"))))
    (openai:agent-turn agent "are you back?")
    (let* ((request (first (scripted-agent-requests agent)))
           (input (openai::json-value request :input)))
      (true (equal "r" (request-previous request)))
      (true (equal "a" (openai::json-value (first input) :call-id)))
      (true (equal "user" (openai::json-value (second input) :role))))
    (true (= 1 (tool-count tool)))))

(define-test explicit-close-wakes-reader-and-old-callback-is-ignored
  (let ((agent (make-instance 'scripted-agent :model "test" :script nil)))
    (openai::enqueue-agent-event agent '(:closed 1006 "stale") :old-socket)
    (false (openai::agent-events agent))
    (let ((reader (sb-thread:make-thread
                   (lambda ()
                     (handler-case (openai::next-agent-event agent)
                       (openai:agent-closed () :closed))))))
      (openai:close-agent agent)
      (true (eq :closed (sb-thread:join-thread reader :timeout 2 :default :timeout))))
    (fail (openai:agent-turn agent "hello") 'openai:agent-closed)
    (false (scripted-agent-requests agent))))

(define-test missing-conversation-starts-afresh
  ;; A response id saved with the world outlives the server's memory of it.
  (let ((agent (make-instance 'scripted-agent
                              :model "gpt-5.6-terra" :socket nil
                              :script
                              (list (list '((:type . "error")
                                            (:error
                                             (:type . "invalid_request_error")
                                             (:code . "previous_response_not_found"))))
                                    (list (scripted-completion "resp_9"))))))
    (setf (openai:agent-response-id agent) "resp_gone")
    (openai:agent-turn agent "hello again")
    (destructuring-bind (fresh stale) (scripted-agent-requests agent)
      (true (string= "resp_gone" (request-previous stale)))
      (false (request-previous fresh)))
    (true (string= "resp_9" (openai:agent-response-id agent)))
    ;; Any other failure still ends the turn and keeps the conversation.
    (setf (scripted-agent-script agent)
          (list (list '((:type . "error")
                        (:error (:type . "server_error") (:code . "overloaded"))))))
    (fail (openai:agent-turn agent "again") 'openai:agent-failed)
    (true (string= "resp_9" (openai:agent-response-id agent)))))
