(asdf:defsystem #:luvcraft
  :description "The interactive luv block world as a standalone program."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/luvcraft)
  :components ((:module "luvcraft"
                :components ((:file "main"))))
  :build-operation "program-op"
  :build-pathname "build/luvcraft"
  :entry-point "luvcraft:main")
