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

(define-test failed-turn-keeps-the-conversation
  (let ((agent (make-instance 'scripted-agent
                              :model "gpt-5.6-terra" :socket nil
                              :tools (list (make-instance 'echo-tool :name "echo"))
                              :script '())))
    ;; A finished turn becomes the conversation.
    (setf (scripted-agent-script agent)
          (list (list (scripted-completion "resp_1"))))
    (openai:agent-turn agent "hello")
    (true (string= "resp_1" (openai:agent-response-id agent)))
    ;; A turn that calls a tool and then loses its connection must not leave
    ;; the conversation at a response whose call has no output.
    (setf (scripted-agent-script agent)
          (list (list (scripted-call "call_1") (scripted-completion "resp_2"))
                (list '(:closed 1006 "gone"))))
    (fail (openai:agent-turn agent "build something") 'openai:agent-closed)
    (true (string= "resp_2" (request-previous (first (scripted-agent-requests agent)))))
    (true (string= "resp_1" (openai:agent-response-id agent)))
    ;; The next turn continues from the last finished one.
    (setf (scripted-agent-script agent)
          (list (list (scripted-completion "resp_3"))))
    (openai:agent-turn agent "what did I just ask?")
    (true (string= "resp_1" (request-previous (first (scripted-agent-requests agent)))))
    (true (string= "resp_3" (openai:agent-response-id agent)))))

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
