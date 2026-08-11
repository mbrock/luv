(asdf:defsystem #:luv
  :description "An experimental Common Lisp atelier for Vulkan."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/examples)
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/tests))))

(asdf:defsystem #:luv/packages
  :description "Package definitions for luv's public and internal names."
  :version "0.0.1"
  :author "Mikael Brockman"
  :components ((:file "package")))

(asdf:defsystem #:luv/world
  :description "Finite chunk domains and the resident block-world model."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/packages)
  :components ((:module "world"
                :components ((:file "block-world"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/tests))))

(asdf:defsystem #:luv/invocation
  :description "A small protocol for reifying API calls as invocations."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/packages
               #:cffi
               #:closer-mop)
  :components ((:file "invocation")))

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

(asdf:defsystem #:luv/gpu/api
  :description "The backend-neutral, WebGPU-shaped luv GPU API."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/packages)
  :components ((:module "gpu"
                :components ((:file "api")))))

(asdf:defsystem #:luv/vulkan/fundament
  :description "Binding machinery for luv's owned Vulkan vocabulary."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/invocation
               #:cffi)
  :components ((:module "vulkan"
                :components ((:file "fundament")))))

(asdf:defsystem #:luv/vulkan/defs
  :description "The current hand-owned Vulkan ABI declarations."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/vulkan/fundament)
  :components ((:module "vulkan"
                :components ((:file "defs")))))

(asdf:defsystem #:luv/vulkan
  :description "Lisp-shaped helpers built on luv's Vulkan binding."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/vulkan/defs)
  :components ((:module "vulkan"
                :components ((:file "high")))))

(asdf:defsystem #:luv/gpu/vulkan
  :description "Vulkan implementation of the luv GPU API."
  :version "0.0.1"
  :author "Mikael Brockman"
  #+sbcl
  :around-compile
  #+sbcl
  (lambda (thunk)
    (with-compilation-unit (:override t
                            :policy '(optimize (debug 3)))
      (funcall thunk)))
  :depends-on (#:luv/gpu/api
               #:luv/vulkan
               #:cffi
               #+darwin
               #:float-features)
  :components ((:module "gpu"
                :components ((:file "vulkan")))))

(asdf:defsystem #:luv/gpu
  :description "The luv GPU API with its default Vulkan backend."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/gpu/vulkan))

(asdf:defsystem #:luv/canvas/api
  :description "Portable canvas, event, and presentation protocols."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/gpu/api)
  :components ((:module "canvas"
                :components ((:file "api")))))

(asdf:defsystem #:luv/canvas/sdl
  :description "SDL realization of luv's native canvas protocol."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas/api
               #:sdl3
               #+darwin
               #:trivial-main-thread)
  :serial t
  :components ((:module "canvas"
                :components ((:file "sdl")
                             #+darwin
                             (:file "cocoa")))))

(asdf:defsystem #:luv/canvas/vulkan
  :description "Vulkan presentation contexts for luv canvases."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas/sdl
               #:luv/gpu/vulkan)
  :components ((:module "canvas"
                :components ((:file "vulkan")))))

(asdf:defsystem #:luv/canvas
  :description "SDL canvas presentation for the luv GPU API."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas/vulkan))

(asdf:defsystem #:luv/examples
  :description "Interactive demos and exploratory applications built on luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/world
               #:luv/canvas
               #:luv/spir-v
               #:uiop)
  :serial t
  :components ((:module "examples"
                :components ((:file "demo")
                             (:file "png")
                             (:file "block-world")))))

(asdf:defsystem #:luv/tests
  :description "Executable claims about luv's renderer-independent models."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/world
               #:rove)
  :components ((:module "tests"
                :components ((:file "world"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call '#:rove '#:find-suite '#:luv/tests))
               (error "luv tests failed"))))

(asdf:defsystem #:luv/tools
  :description "One-shot command-line tools for luv development."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/examples
               #:uiop)
  :build-operation "program-op"
  :build-pathname "build/luv"
  :entry-point "luv.tools:main"
  :serial t
  :components ((:module "tools"
                :components ((:file "package")
                             (:file "runner")
                             (:file "block-world")))))

(asdf:defsystem #:luv/mcclim
  :description "An experimental McCLIM backend presented by luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas
               #:luv/spir-v
               #:mcclim-render)
  :serial t
  :components ((:file "mcclim-package")
               (:file "mcclim-port")
               (:file "mcclim-mirror")
               (:file "mcclim-lab")
               (:file "mcclim-widget-lab")
               (:file "mcclim-compositor")))

(asdf:defsystem #:luv/mcclim/listener
  :description "The McCLIM Listener running on luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/mcclim
               #:clim-listener)
  :components ((:file "mcclim-listener")))
