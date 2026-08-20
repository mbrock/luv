(in-package #:asdf-user)

(defsystem "openai"
  :description "A small persistent OpenAI Responses WebSocket agent client."
  :long-description
  "One OpenAI agent owns one persistent Responses WebSocket connection.
The public protocol is intentionally small: streaming text and reasoning,
and client function tools expressed as CLOS objects."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("cl-base64" "cl-json" "websocket-driver-client")
  :serial t
  :components ((:module "openai"
                :serial t
                :components ((:file "package")
                             (:file "agent"))))
  :in-order-to ((test-op (test-op "openai/test"))))

(defsystem "openai/test"
  :description "Tests for the OpenAI Responses WebSocket client."
  :depends-on ("openai" "rove")
  :serial t
  :components ((:module "openai/tests"
                :serial t
                :components ((:file "package")
                             (:file "agent"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:openai.tests))
               (error "OpenAI tests failed"))))
