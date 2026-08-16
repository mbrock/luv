(in-package #:asdf-user)

(defsystem "mcluv/backend"
  :description "An experimental McCLIM backend presented through luv canvases."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "mcclim-render" "cl-dejavu")
  :serial t
  :components ((:module "mcclim"
                :serial t
                :components ((:file "package")
                             (:file "port")
                             (:file "mirror")
                             (:file "gpu")
                             (:file "widget-lab")
                             (:file "compositor")))))

(defsystem "mcluv/shader-lab"
  :description "A McCLIM presentation browser for luvcraft's live shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/backend" "luvcraft")
  :components ((:file "mcclim/shader-lab")))

(defsystem "mcluv/luvcraft"
  :description "McCLIM gadget textures embedded in a live luvcraft session."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/backend" "luvcraft")
  :components ((:file "mcclim/luvcraft")))

(defsystem "mcluv/listener"
  :description "The McCLIM Listener running on the mcluv backend."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/backend" "clim-listener")
  :components ((:file "mcclim/listener")))

(defsystem "mcluv"
  :description "The McCLIM Listener and shader lab as a standalone luv program."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("mcluv/listener" "mcluv/shader-lab")
  :components ((:file "mcclim/main"))
  :build-operation "program-op"
  :build-pathname "build/mcluv"
  :entry-point "mcluv:main")
