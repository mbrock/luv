(asdf:defsystem #:luv
  :description "An experimental Common Lisp atelier for Vulkan."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas
               #:luv/spir-v))

(asdf:defsystem #:luv/spir-v
  :description "A small s-expression SPIR-V assembler for luv shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:cffi
               #:closer-mop)
  :serial t
  :components ((:file "spir-v-package")
               (:file "spir-v")
               (:file "shader")))

(asdf:defsystem #:luv/gpu
  :description "The WebGPU-shaped luv API and Vulkan backend."
  :version "0.0.1"
  :author "Mikael Brockman"
  #+sbcl
  :around-compile
  #+sbcl
  (lambda (thunk)
    (with-compilation-unit (:override t
                            :policy '(optimize (debug 3)))
      (funcall thunk)))
  :depends-on (#:cffi
               #+darwin
               #:float-features)
  :serial t
  :components ((:file "package")
               (:file "gpu")
               (:file "vulkan")
               (:file "gpu-vulkan")))

(asdf:defsystem #:luv/canvas
  :description "SDL canvas presentation for the WebGPU-shaped luv API."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/gpu
               #:luv/spir-v
               #:sdl3
               #+darwin
               #:trivial-main-thread)
  :serial t
  :components ((:file "canvas")
               (:file "canvas-sdl")
               #+darwin
               (:file "canvas-cocoa")
               (:file "canvas-vulkan")
               (:file "demo")))

(asdf:defsystem #:luv/mcclim
  :description "An experimental McCLIM backend presented by luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas
               #:mcclim-render)
  :serial t
  :components ((:file "mcclim-package")
               (:file "mcclim-port")
               (:file "mcclim-mirror")
               (:file "mcclim-lab")
               (:file "mcclim-widget-lab")))

(asdf:defsystem #:luv/mcclim/listener
  :description "The McCLIM Listener running on luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/mcclim
               #:clim-listener)
  :components ((:file "mcclim-listener")))
