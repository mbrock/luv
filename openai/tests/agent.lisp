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
