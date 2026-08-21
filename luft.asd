(in-package #:asdf-user)

(defsystem "luft"
  :description "Canonical cubical topology and compact face realization."
  :version "0.0.1"
  :author "Mikael Brockman"
  :components ((:file "luft/luft"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luft '#:run-luft-tests)))

(defsystem "luft/render"
  :description "A greenfield atelier drawing surface chains of packed sites."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft" "luv")
  :serial t
  :components ((:module "render"
                :pathname "luft/render"
                :serial t
                :components ((:file "package")
                             (:file "shaders")
                             (:file "render")
                             (:file "studio"))))
  :in-order-to ((test-op (test-op "luft/render/test"))))

(defsystem "luft/program"
  :description "The standalone LUFT atelier executable."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/render")
  :serial t
  :components ((:file "luft/render/main"))
  :build-operation "program-op"
  :build-pathname "build/luft"
  :entry-point "luft.render:main")

(defsystem "luft/render/test"
  :description "Executable claims for the indexed-instanced renderer."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft/render" "rove")
  :components ((:file "luft/render/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luft.render.tests))
               (error "LUFT render tests failed"))))
