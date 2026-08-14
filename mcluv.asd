(asdf:defsystem #:mcluv
  :description "The McCLIM Listener running as a luv program."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/mcclim/listener
               #:luv/mcclim/shader-lab)
  :components ((:module "mcclim"
                :components ((:file "main"))))
  :build-operation "program-op"
  :build-pathname "build/mcluv"
  :entry-point "mcluv:main")
