;;;; The luv systems.  The wiki tooling lives in the sibling luv-wiki.asd
;;;; because luv/wiki names it in :defsystem-depends-on, which ASDF cannot
;;;; resolve to a system defined in the very file being loaded.

(asdf:load-asd (merge-pathnames "luv-wiki.asd" (or *load-truename* *default-pathname-defaults*)))

(asdf:defsystem #:luv
  :description "An experimental Common Lisp atelier for Vulkan."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/examples
               #:luv/luvcraft)
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/arithmetic/tests)
                              (asdf:test-op #:luv/arithmetic/language/tests)
                              (asdf:test-op #:luv/arithmetic/lisp/tests)
                              (asdf:test-op #:luv/tests)
                              (asdf:test-op #:luv/spir-v/tests)
                              (asdf:test-op #:luv/luvcraft/tests)
                              (asdf:test-op #:luv-wiki/tests))))

(asdf:defsystem #:luv/packages
  :description "Package definitions for luv's public and internal names."
  :version "0.0.1"
  :author "Mikael Brockman"
  :components ((:file "package")))

(asdf:defsystem #:luv/arithmetic
  :description "Semantic specifications and dimensional algebra for compiled arithmetic."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components ((:file "arithmetic-package")
               (:file "arithmetic"))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/arithmetic/tests))))

(asdf:defsystem #:luv/arithmetic/tests
  :description "Executable claims for backend-neutral semantic arithmetic."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic
               #:rove)
  :components ((:module "tests"
                :components ((:file "arithmetic"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/arithmetic/tests))
               (error "luv arithmetic tests failed"))))

(asdf:defsystem #:luv/arithmetic/language
  :description "Backend-neutral compiled arithmetic definitions and expressions."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic)
  :serial t
  :components ((:file "arithmetic-language-package")
               (:file "arithmetic-language"))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:luv/arithmetic/language/tests))))

(asdf:defsystem #:luv/arithmetic/language/tests
  :description "Executable claims for the backend-neutral arithmetic frontend."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic/language
               #:rove)
  :components ((:module "tests"
                :components ((:file "arithmetic-language"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite
                       '#:luv/arithmetic/language/tests))
               (error "luv arithmetic language tests failed"))))

(asdf:defsystem #:luv/arithmetic/lisp
  :description "Common Lisp realization of checked arithmetic definitions."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic/language)
  :serial t
  :components ((:file "arithmetic-lisp-package")
               (:file "arithmetic-lisp"))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:luv/arithmetic/lisp/tests))))

(asdf:defsystem #:luv/arithmetic/lisp/tests
  :description "Executable claims for the Common Lisp arithmetic realization."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic/lisp
               #:rove)
  :components ((:module "tests"
                :components ((:file "arithmetic-lisp"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/arithmetic/lisp/tests))
               (error "luv Common Lisp arithmetic tests failed"))))

(asdf:defsystem #:luv/world
  :description "Finite chunk domains and the resident block-world model."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/packages)
  :components ((:module "luvcraft"
                :components ((:file "world"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/tests))))

(asdf:defsystem #:luv/parinfer
  :description "The connection-free indentation and parenthesis checker."
  :version "0.0.1"
  :author "Mikael Brockman"
  :components ((:file "parinfer")))

(asdf:defsystem #:luv/wiki
  :description "The luv workshop wiki.  Loading it reads every page into
Lisp objects; (asdf:make :luv/wiki) renders the static site into build/wiki/."
  :version "0.0.1"
  :author "Mikael Brockman"
  :defsystem-depends-on (#:luv-wiki)
  :depends-on (#:luv-wiki)
  :build-operation "luv.wiki:render-op"
  :components ((:module "wiki"
                :default-component-class "luv.wiki:org-file"
                :components ((:file "index")
                             (:file "block-world")
                             (:file "box3d-architecture")
                             (:file "commands-and-usage")
                             (:file "completion-frontier")
                             (:file "domains-and-bundles")
                             (:file "field-notes-measures")
                             (:file "field-notes-mp-units")
                             (:file "frame-slots")
                             (:file "luv-vulkan-hal")
                             (:file "mathematical-shaders")
                             (:file "moppe-legacy")
                             (:file "objective-c-and-metal")
                             (:file "physics-and-simd")
                             (:file "quantities-and-measurement")
                             (:file "resource-lifetimes")
                             (:file "sb-simd")
                             (:file "sky-and-light")
                             (:file "webgpu-shape")
                             (:file "wiki-site")
                             (:static-file "style.css")
                             (:static-file "images/dexp.png")))))

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
  :depends-on (#:luv/arithmetic/language
               #:cffi
               #:closer-mop)
  :serial t
  :components ((:file "spir-v-package")
               (:file "spir-v")
               (:file "shader")
               (:file "shader-expression")))

(asdf:defsystem #:luv/spir-v/tests
  :description "Tests for luv's mathematical shader language and lowering."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/luvcraft/shaders
               #:rove)
  :components ((:module "tests"
                :components ((:file "shader"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/spir-v/tests))
               (error "luv shader tests failed"))))

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

(asdf:defsystem #:luv/luvcraft/quantities
  :description "Backend-neutral semantic quantities for the luvcraft domain."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic)
  :serial t
  :components ((:module "luvcraft"
                :components ((:file "quantities-package")
                             (:file "quantities")))))

(asdf:defsystem #:luv/luvcraft/shaders
  :description "The luvcraft block-world materials as mathematical shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/luvcraft/quantities
               #:luv/spir-v)
  :components ((:module "luvcraft"
                :components ((:file "shaders")))))

(asdf:defsystem #:luv/luvcraft
  :description "Luvcraft: the interactive block world built on luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/world
               #:luv/canvas
               #:luv/luvcraft/quantities
               #:luv/luvcraft/shaders
               #:sb-concurrency
               #:uiop)
  :serial t
  :components ((:module "luvcraft"
                :components ((:file "production")
                             (:file "png")
                             (:file "blocks")
                             (:file "terrain")
                             (:file "light")
                             (:file "mesher")
                             (:file "simulation")
                             (:file "sky")
                             (:file "live-pipeline")
                             (:file "app")
                             (:file "streaming")
                             (:file "render")
                             (:file "capture")
                             (:file "gazetteer"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/luvcraft/tests))))

(asdf:defsystem #:luv/examples
  :description "Interactive demos and exploratory applications built on luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas
               #:luv/spir-v)
  :components ((:module "examples"
                :components ((:file "demo")))))

(asdf:defsystem #:luv/tests
  :description "Executable claims about luv's renderer-independent models."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/world
               #:luv/parinfer
               #:rove)
  :components ((:module "tests"
                :components ((:file "world")
                             (:file "parinfer"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call '#:rove '#:find-suite '#:luv/tests))
               (error "luv tests failed"))))

(asdf:defsystem #:luv/luvcraft/tests
  :description "Tests for the visible block-world slice above the core model."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/luvcraft
               #:rove)
  :components ((:module "tests"
                :components ((:file "block-world")
                             (:file "light"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/luvcraft/tests))
               (error "luvcraft tests failed"))))

(asdf:defsystem #:luv/tools
  :description "One-shot command-line tools for luv development."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/luvcraft
               #:uiop)
  :build-operation "program-op"
  :build-pathname "build/luv"
  :entry-point "luv.tools:main"
  :serial t
  :components ((:module "tools"
                :components ((:file "package")
                             (:file "runner")
                             (:file "block-world")
                             (:file "gazetteer")))))

(asdf:defsystem #:luv/mcclim
  :description "An experimental McCLIM backend presented by luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas
               #:luv/spir-v
               #:mcclim-render)
  :serial t
  :components ((:module "mcclim"
                :components ((:file "package")
                             (:file "port")
                             (:file "mirror")
                             (:file "widget-lab")
                             (:file "compositor")))))

(asdf:defsystem #:luv/mcclim/shader-lab
  :description "A McCLIM presentation browser for luvcraft's live shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/mcclim
               #:luv/luvcraft)
  :components ((:module "mcclim"
                :components ((:file "shader-lab")))))

(asdf:defsystem #:luv/mcclim/listener
  :description "The McCLIM Listener running on luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/mcclim
               #:clim-listener)
  :components ((:module "mcclim"
                :components ((:file "listener")))))
