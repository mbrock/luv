(in-package #:asdf-user)

(defsystem "telegram"
  :description "A sans-IO MTProto core: TL, crypto, framing, auth, sessions."
  :long-description
  "Everything in this system is a function of its inputs: no sockets, no
clock reads, and no randomness that was not handed in.  That is what lets
Telegram's published sample handshake run as an ordinary test.  The socket
lives in TELEGRAM/NET."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components
  ((:module "telegram"
    :serial t
    :components
    ((:file "package")
     (:file "octets")
     (:file "tl")
     (:module "crypto"
      :serial t
      :components ((:file "sha")
                   (:file "aes")
                   (:file "bignum")
                   (:file "rsa")))
     (:file "transport")
     (:file "envelope")
     (:file "schema")
     (:file "keys")
     (:file "auth")
     (:file "session"))))
  :in-order-to ((test-op (test-op "telegram/test"))))

(defsystem "telegram/net"
  :description "A TCP connection that drives the MTProto core."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram" (:require #:sb-bsd-sockets))
  :components ((:module "telegram"
                :components ((:file "net")))))

(defsystem "telegram/test"
  :description "Executable claims for the MTProto core, against published vectors."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram" "telegram/net" "rove")
  :serial t
  :components
  ((:module "telegram/tests"
    :serial t
    :components ((:file "package")
                 (:file "primitives")
                 (:file "handshake")
                 (:file "session"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:telegram.tests))
               (error "telegram tests failed"))))

(defsystem "telegram/live"
  :description "An opt-in check that talks to a real Telegram data centre."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram/net")
  :components ((:module "telegram" :components ((:file "live")))))
