(in-package #:asdf-user)

(defsystem "luft"
  :description "The executable LUFT foundation: packed cubical sites, chains,
the strict-minority chamfer classifier, and the face-record ABI."
  :version "0.1.0"
  :author "Mikael Brockman"
  :components ((:file "luft/foundation"))
  :in-order-to ((test-op (test-op "luft/test"))))

(defsystem "luft/test"
  :description "The foundation's own executable invariants."
  :version "0.1.0"
  :author "Mikael Brockman"
  :depends-on ("luft")
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luft '#:run-tests)))

(defsystem "luft/render"
  :description "A greenfield renderer drawing materialized face records:
one 16-byte record per exposed face, realized on the GPU from the face site,
its shape word, and the chamfer width alone."
  :version "0.1.0"
  :author "Mikael Brockman"
  :depends-on ("luft" "luv")
  :serial t
  :components ((:module "render"
                :pathname "luft/render"
                :serial t
                :components ((:file "package")
                             (:file "shaders")
                             (:file "render")))))

(defsystem "luft/program"
  :description "The standalone LUFT atelier executable."
  :version "0.1.0"
  :author "Mikael Brockman"
  :depends-on ("luft/render")
  :serial t
  :components ((:file "luft/render/main"))
  :build-operation "program-op"
  :build-pathname "build/luft"
  :entry-point "luft.render:main")
