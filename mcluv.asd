(asdf:defsystem #:mcluv
  :description "The McCLIM Listener running as a luv program."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/mcclim/listener)
  :components ((:file "mcluv"))
  :build-operation "program-op"
  :build-pathname "mcluv"
  :entry-point "mcluv:main")
