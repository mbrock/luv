(asdf:defsystem "sly-client"
  :description "The short-lived command client for managed Luv Lisp images."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ((:require "sb-bsd-sockets")
               (:require "sb-posix")
               "luv/parinfer"
               "cl-json")
  :serial t
  :components ((:file "scripts/sly-client"))
  :build-operation "program-op"
  :build-pathname "build/sly-client"
  :entry-point "sly-client:entry-point")

(asdf:defsystem "sly-client/test"
  :description "Lifecycle-selection tests for the managed Sly client."
  :depends-on ("sly-client" "luv/test-support")
  :serial t
  :components ((:file "scripts/sly-client-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:sly-client.tests)))
