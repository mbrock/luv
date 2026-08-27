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
     (:file "inflate")
     (:file "tl")
     (:file "tl-schema")
     (:module "crypto"
      :serial t
      :components ((:file "sha")
                   (:file "aes")
                   (:file "bignum")
                   (:file "rsa")
                   (:file "srp")))
     (:file "transport")
     (:file "envelope")
     (:file "schema")
     (:file "keys")
     (:file "auth")
     (:file "session"))))
  :in-order-to ((test-op (test-op "telegram/test"))))

(defsystem "telegram/api"
  :description "Telegram's published TL schema, generated into classes."
  :long-description
  "One form reads schema/api.tl at compile time and expands into a class for
every constructor, function, and abstract type in it.  Kept as its own system
because the resulting fasl takes a few seconds to load, and the MTProto core
below it does not need any of it."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram")
  :components ((:module "telegram"
                :serial t
                :components ((:static-file "schema/api.tl")
                             (:file "api")))))

(defsystem "telegram/client"
  :description "Calling Telegram: application identity, layer negotiation, RPC."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram/api" "telegram/net" (:require #:sb-posix))
  :components ((:module "telegram"
                :serial t
                :components ((:file "client")
                             (:file "login")))))

(defsystem "telegram/net"
  :description "A TCP connection that drives the MTProto core."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram" (:require #:sb-bsd-sockets))
  :components ((:module "telegram"
                :components ((:file "net")))))

(defsystem "telegram/chat"
  :description "Peers, histories, photos, and an update cursor over INVOKE."
  :long-description
  "The application layer a chat client needs and the protocol does not
provide: peers that remember their access hashes, a history per peer sorted
by message id, and the pts/qts/date cursor that turns updates.getDifference
into news."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram/client")
  :components ((:module "telegram" :components ((:file "chat")))))

(defsystem "telegram/test"
  :description "Executable claims for the MTProto core, against published vectors."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram" "telegram/api" "telegram/net" "telegram/client"
               "luv/test-support")
  :serial t
  :components
  ((:module "telegram/tests"
    :serial t
    :components ((:file "package")
                 (:file "primitives")
                 (:file "handshake")
                 (:file "session")
                 (:file "schema")
                 (:file "login"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:telegram.tests)))

(defsystem "telegram/live"
  :description "An opt-in check that talks to a real Telegram data centre."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("telegram/net" "telegram/client")
  :components ((:module "telegram" :components ((:file "live")))))
