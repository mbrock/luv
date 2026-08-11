(asdf:defsystem #:luvcraft
  :description "The interactive luv block world as a standalone program."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/examples)
  :components ((:file "luvcraft"))
  :build-operation "program-op"
  :build-pathname "luvcraft"
  :entry-point "luvcraft:main")
