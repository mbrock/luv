(defpackage #:luv.application-agent.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:agent #:luv.application-agent)
                    (#:openai #:openai)))

(in-package #:luv.application-agent.tests)

(declaim (declaration deftest))
