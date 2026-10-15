(in-package #:asdf-user)

(defsystem "luvland"
  :description "A Wayland atelier whose only world is windows."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "luv/wayland" "luv/workbench")
  :serial t
  :components ((:module "luvland"
                :serial t
                :components ((:file "package")
                             (:file "keys")
                             (:file "luvland")
                             (:file "commands"))))
  :in-order-to ((test-op (test-op "luvland/test"))))

(defsystem "luvland/program"
  :description "The standalone Luvland executable."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvland")
  :components ((:file "luvland/main"))
  :build-operation "program-op"
  :build-pathname "build/luvland"
  :entry-point "luvland:main")

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
