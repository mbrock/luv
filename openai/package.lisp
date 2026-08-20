;;;; A deliberately narrow client for the experimental Responses WebSocket
;;;; transport used by Codex.  The OpenAI API itself owns its JSON vocabulary;
;;;; this package owns the durable agent and the local tool boundary.

(defpackage #:openai
  (:use #:cl)
  (:documentation
   "A persistent OpenAI Responses WebSocket agent.  One AGENT has one
connection and serializes its turns; call AGENT-TURN from a dedicated thread
when a program wants one agent thread per connection.")
  (:export
   #:openai-error #:openai-error-detail #:missing-api-key #:retry
   #:agent-closed #:agent-failed
   #:agent #:make-agent #:default-api-key #:*api-key-fallbacks* #:close-agent #:agent-model #:agent-response-id
   #:agent-turn #:agent-response #:agent-response-text #:agent-response-reasoning
   #:agent-response-id #:agent-response-usage
   #:tool #:tool-name #:tool-description #:tool-parameters #:call-tool
   #:tool-output #:make-tool-output #:tool-output-text #:tool-output-images
   #:tool-output-image #:make-tool-output-image #:tool-output-image-octets
   #:tool-output-image-media-type #:tool-output-image-detail
   #:handle-agent-event #:+json-false+))
