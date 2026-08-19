(in-package #:asdf-user)

(defsystem "mqtt"
  :description "A sans-IO MQTT 5 client core: wire codec, packets, sessions."
  :long-description
  "Everything in this system is a function of its inputs: no sockets and no
clock reads.  A session is fed the octets that arrived and drained of the
octets to send; what happens in between is what the tests check.  The socket
lives in MQTT/NET."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components
  ((:module "mqtt"
    :serial t
    :components
    ((:file "package")
     (:file "wire")
     (:file "packets")
     (:file "session"))))
  :in-order-to ((test-op (test-op "mqtt/test"))))

(defsystem "mqtt/net"
  :description "A TCP connection that drives an MQTT session."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mqtt" (:require #:sb-bsd-sockets))
  :components ((:module "mqtt"
                :components ((:file "net")))))

(defsystem "mqtt/test"
  :description "Executable claims for the MQTT core, against the specification's own examples."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mqtt" "rove")
  :serial t
  :components
  ((:module "mqtt/tests"
    :serial t
    :components ((:file "package")
                 (:file "wire")
                 (:file "session"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:mqtt.tests))
               (error "mqtt tests failed"))))
