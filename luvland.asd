(in-package #:asdf-user)

(defsystem "luvland"
  :description "A Wayland atelier whose only world is windows."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "luv/wayland")
  :serial t
  :components ((:module "luvland"
                :serial t
                :components ((:file "package")
                             (:file "keys")
                             (:file "luvland"))))
  :in-order-to ((test-op (test-op "luvland/test"))))

(defsystem "luvland/test"
  :description "Luvland's zero-copy chain: a real Vulkan client's dmabufs, imported."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvland" "luv/test-support")
  :components ((:file "luvland/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luvland.tests)))
