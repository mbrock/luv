(in-package #:asdf-user)

(defsystem "luft"
  :description "Packed sites in a small cubical world."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components ((:file "luft/package")
               (:file "luft/luft")
               (:file "luft/chain"))
  :in-order-to ((test-op (test-op "luft/test"))))

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
                             (:file "vertex-shaders")
                             (:file "field")
                             (:file "material")
                             (:file "lattice")
                             (:file "render")
                             (:file "studio")
                             (:file "film")
                             (:file "architecture"))))
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
  :description "Executable claims for the packed-site renderer."
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

(defsystem "luft/test"
  :description "Executable incidence laws for packed LUFT sites."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luft" "rove")
  :components ((:file "luft/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luft.tests))
               (error "LUFT tests failed"))))
