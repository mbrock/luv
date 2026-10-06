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
                             (:file "luvland")))))
