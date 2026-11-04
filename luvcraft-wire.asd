(in-package #:asdf-user)

(defsystem "luvcraft-wire"
  :description "The Luvcraft wire protocol: a sans-IO codec over octet vectors."
  :long-description
  "Messages for joining, events, edits, players, chat, chunks, and the
unreliable pose and entity snapshots, as typed structures that encode and
decode themselves strictly.  No sockets, no clock, and no game packages;
the endpoints live in LUVCRAFT-WIRE/NET.  See wiki figure A8LXLK."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components
  ((:module "luvcraft-wire"
    :serial t
    :components
    ((:file "package")
     (:file "octets")
     (:file "codec"))))
  :in-order-to ((test-op (test-op "luvcraft-wire/test"))))

(defsystem "luvcraft-wire/net"
  :description "Wire endpoints: one worker owns the TCP and UDP sockets, or a loopback network."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft-wire" "uiop" (:require #:sb-bsd-sockets)
               (:require #:sb-posix) (:require #:sb-concurrency))
  :serial t
  :components ((:module "luvcraft-wire"
                :serial t
                :components ((:file "endpoint")
                             (:file "sockets")
                             (:file "loopback")))))

(defsystem "luvcraft-wire/test"
  :description "Executable claims for the Luvcraft wire: codec and endpoints."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft-wire/net" "luv/test-support")
  :serial t
  :components ((:module "luvcraft-wire/tests"
                :serial t
                :components ((:file "package")
                             (:file "codec")
                             (:file "net"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luvcraft.wire.tests)))
