(asdf:defsystem "sly-client"
  :description "The short-lived command client for managed Luv Lisp images."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ((:require "sb-bsd-sockets")
               (:require "sb-posix")
               "cl-json")
  :serial t
  :components ((:file "parinfer/implementation")
               (:file "scripts/sly-client"))
  :build-operation "program-op"
  :build-pathname "build/sly-client"
  :entry-point "sly-client:entry-point")
