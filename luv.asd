;;;; The luv systems.  The wiki tooling lives in the sibling luv-wiki.asd
;;;; because luv/wiki names it in :defsystem-depends-on, which ASDF cannot
;;;; resolve to a system defined in the very file being loaded.

(unless (asdf:registered-system "luv-wiki")
  (asdf:load-asd (merge-pathnames "luv-wiki.asd" (or *load-truename* *default-pathname-defaults*))))

(asdf:defsystem #:luv
  :description "An experimental Common Lisp atelier for Vulkan."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/examples
               #:luv/msl
               #:luv/luvcraft)
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/arithmetic/tests)
                              (asdf:test-op #:luv/arithmetic/records/tests)
                              (asdf:test-op #:luv/arithmetic/language/tests)
                              (asdf:test-op #:luv/arithmetic/lisp/tests)
                              (asdf:test-op #:luv/objective-c/tests)
                              (asdf:test-op #:luv/metal/tests)
                              (asdf:test-op #:luv/vulkan/tests)
                              (asdf:test-op #:luv/tests)
                              (asdf:test-op #:luv/spir-v/tests)
                              (asdf:test-op #:luv/msl/tests)
                              (asdf:test-op #:luv/luvcraft/tests)
                              (asdf:test-op #:luv-wiki/tests))))

(asdf:defsystem #:luv/packages
  :description "Package definitions for luv's public and internal names."
  :version "0.0.1"
  :author "Mikael Brockman"
  :components ((:file "package")))

(asdf:defsystem #:luv/trace
  :description "Low-overhead nested CPU zones for live luv measurements."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/packages)
  :components ((:module "hal"
                :components ((:file "trace")))))

(asdf:defsystem #:luv/arithmetic
  :description "Semantic specifications and dimensional algebra for compiled arithmetic."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components ((:module "arithmetic"
                :serial t
                :components ((:file "package")
                             (:file "semantics")
                             (:file "declarations"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/arithmetic/tests))))

(asdf:defsystem #:luv/arithmetic/records
  :description "Quantity declarations on Common Lisp classes and structures."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic
               #:closer-mop)
  :serial t
  :components ((:module "arithmetic"
                :components
                ((:module "records"
                  :serial t
                  :components ((:file "package")
                               (:file "records"))))))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:luv/arithmetic/records/tests))))

(asdf:defsystem #:luv/arithmetic/records/tests
  :description "Executable claims for quantity-bearing Lisp records."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic/records
               #:rove)
  :components ((:module "arithmetic"
                :components
                ((:module "records"
                  :components ((:file "tests"))))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite
                       '#:luv/arithmetic/records/tests))
               (error "luv arithmetic record tests failed"))))

(asdf:defsystem #:luv/arithmetic/tests
  :description "Executable claims for backend-neutral semantic arithmetic."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic
               #:rove)
  :components ((:module "arithmetic"
                :components ((:file "tests"))))
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
  :components ((:module "arithmetic"
                :components
                ((:module "language"
                  :serial t
                  :components ((:file "package")
                               (:file "frontend"))))))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:luv/arithmetic/language/tests))))

(asdf:defsystem #:luv/arithmetic/language/tests
  :description "Executable claims for the backend-neutral arithmetic frontend."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic/language
               #:rove)
  :components ((:module "arithmetic"
                :components
                ((:module "language"
                  :components ((:file "tests"))))))
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
  :components ((:module "arithmetic"
                :components
                ((:module "lisp"
                  :serial t
                  :components ((:file "package")
                               (:file "compiler"))))))
  :in-order-to ((asdf:test-op
                 (asdf:test-op #:luv/arithmetic/lisp/tests))))

(asdf:defsystem #:luv/arithmetic/lisp/tests
  :description "Executable claims for the Common Lisp arithmetic realization."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic/lisp
               #:rove)
  :components ((:module "arithmetic"
                :components
                ((:module "lisp"
                  :components ((:file "tests"))))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/arithmetic/lisp/tests))
               (error "luv Common Lisp arithmetic tests failed"))))

(asdf:defsystem #:luv/arithmetic/lisp/vec3
  :description "Luv VEC3 realization of checked Common Lisp arithmetic."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic/lisp
               #:luv/world)
  :components ((:module "arithmetic"
                :components
                ((:module "lisp"
                  :components ((:file "vec3")))))))

(asdf:defsystem #:luv/world/quantities
  :description "Semantic quantities for cells, lattice space, and its metric."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic)
  :serial t
  :components ((:module "luvcraft"
                :components ((:file "world-quantities-package")
                             (:file "world-quantities")))))

(asdf:defsystem #:luv/world/fields
  :description "Inspectable field definitions for distributed voxel facts."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic)
  :serial t
  :components ((:module "luvcraft"
                :components ((:file "world-fields-package")
                             (:file "world-fields")))))

(asdf:defsystem #:luv/world
  :description "Finite chunk domains and the resident block-world model."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/packages
               #:luv/arithmetic/records
               #:luv/world/quantities
               #:luv/world/fields)
  :components ((:module "luvcraft"
                :components ((:file "world"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/tests))))

(asdf:defsystem #:luv/parinfer
  :description "The connection-free indentation and parenthesis checker."
  :version "0.0.1"
  :author "Mikael Brockman"
  :components ((:module "parinfer"
                :components ((:file "implementation")))))

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
                             (:file "domains-and-bundles")
                             (:file "field-notes-measures")
                             (:file "field-notes-mp-units")
                             (:file "gpu-architecture")
                             (:file "frame-performance")
                             (:file "mathematical-shaders")
                             (:file "slug-bezier")
                             (:file "metal-backend")
                             (:file "moppe-legacy")
                             (:file "objective-c-and-metal")
                             (:file "physics-and-simd")
                             (:file "quantities-and-measurement")
                             (:file "sb-simd")
                             (:file "sky-and-light")
                             (:file "voxel-fields-and-windows")
                             (:file "wiki-site")
                             (:static-file "site.js")
                             (:static-file "images/dexp.png")
                             (:static-file "images/slug-bezier-proof.png")))))

(asdf:defsystem #:luv/objective-c
  :description "A declared Objective-C foreign object system with opt-in tracing."
  :version "0.0.1"
  :author "Mikael Brockman"
  :if-feature :darwin
  :depends-on (#:cffi
               #:cffi-libffi)
  :serial t
  :components ((:module "objective-c"
                :components ((:file "package")
                             (:static-file "exception-bridge.m")
                             (:file "exception-bridge")
                             (:file "runtime")
                             (:file "foundation")))))

(asdf:defsystem #:luv/metal
  :description "The native Metal vocabulary declared through luv's Objective-C system."
  :version "0.0.1"
  :author "Mikael Brockman"
  :if-feature :darwin
  :depends-on (#:luv/objective-c)
  :serial t
  :components ((:module "hal"
                :components
                ((:module "metal"
                  :components ((:file "probe")
                               (:file "ffi")))))))

(asdf:defsystem #:luv/metal/probe
  :description "The smallest native Metal object proof through luv's Objective-C system."
  :version "0.0.1"
  :author "Mikael Brockman"
  :if-feature :darwin
  :depends-on (#:luv/metal))

(asdf:defsystem #:luv/objective-c/tests
  :description "Executable claims for the Objective-C foreign object system."
  :version "0.0.1"
  :author "Mikael Brockman"
  :if-feature :darwin
  :depends-on (#:luv/metal/probe
               #:rove)
  :components ((:module "objective-c"
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/objective-c/tests))
               (error "luv Objective-C tests failed"))))

(asdf:defsystem #:luv/spir-v
  :description "A small s-expression SPIR-V assembler for luv shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic/language
               #:cffi
               #:closer-mop)
  :serial t
  :components
  ((:module "hal"
    :components
    ((:module "vulkan"
      :components
      ((:module "spir-v"
        :components ((:file "package")
                     (:file "instructions")
                     (:file "module")))))
     (:module "shader"
      :components ((:file "language")))))))

(asdf:defsystem #:luv/spir-v/tests
  :description "Tests for luv's mathematical shader language and lowering."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/luvcraft/shaders
               #:luv/slug
               #:cl-dejavu
               #:rove)
  :components ((:module "hal"
                :components
                ((:module "shader"
                  :components ((:file "tests"))))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/spir-v/tests))
               (error "luv shader tests failed"))))

(asdf:defsystem #:luv/slug
  :description "Slug quadratic outline preprocessing and mathematical shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/spir-v
               #:luv/arithmetic/lisp
               #:zpb-ttf)
  :serial t
  :components ((:module "hal"
                :components
                ((:module "shader"
                  :serial t
                  :components ((:file "slug-package")
                               (:file "slug-outline")
                               (:file "slug-truetype")
                               (:file "slug")))))))

(asdf:defsystem #:luv/msl
  :description "Direct Metal Shading Language lowering for mathematical shaders."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/spir-v)
  :serial t
  :components ((:module "hal"
                :components
                ((:module "metal"
                  :components
                  ((:module "msl"
                    :components ((:file "package")
                                 (:file "lowering"))))))))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/msl/tests))))

(asdf:defsystem #:luv/msl/tests
  :description "Executable claims for direct mathematical-shader MSL lowering."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/msl
               #:luv/luvcraft/shaders
               #:luv/slug
               #:rove)
  :components ((:module "hal"
                :components
                ((:module "metal"
                  :components
                  ((:module "msl"
                    :components ((:file "tests"))))))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/msl/tests))
               (error "luv MSL tests failed"))))

(asdf:defsystem #:luv/gpu/api
  :description "The backend-neutral, WebGPU-shaped luv GPU API."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/trace)
  :components ((:module "hal"
                :components ((:file "gpu")))))

(asdf:defsystem #:luv/vulkan/fundament
  :description "Direct bindings and opt-in tracing for luv's Vulkan vocabulary."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/packages
               #:cffi)
  :components ((:module "hal"
                :components
                ((:module "vulkan"
                  :components ((:file "ffi")))))))

(asdf:defsystem #:luv/vulkan/defs
  :description "The current hand-owned Vulkan ABI declarations."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/vulkan/fundament)
  :components ((:module "hal"
                :components
                ((:module "vulkan"
                  :components ((:file "abi")))))))

(asdf:defsystem #:luv/vulkan
  :description "Lisp-shaped helpers built on luv's Vulkan binding."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/vulkan/defs)
  :components ((:module "hal"
                :components
                ((:module "vulkan"
                  :components ((:file "native")))))))

(asdf:defsystem #:luv/vulkan/tests
  :description "Executable claims for direct Vulkan calls and opt-in tracing."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/vulkan
               #:rove)
  :components ((:module "hal"
                :components
                ((:module "vulkan"
                  :components ((:file "tests"))))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/vulkan/tests))
               (error "luv Vulkan tests failed"))))

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
               #:luv/spir-v
               #:cffi
               (:feature :darwin #:float-features))
  :components ((:module "hal"
                :components
                ((:module "vulkan"
                  :components ((:file "gpu")))))))

(asdf:defsystem #:luv/gpu/metal
  :description "Metal 4 implementation of luv's first GPU vocabulary."
  :version "0.0.1"
  :author "Mikael Brockman"
  :if-feature :darwin
  :depends-on (#:luv/gpu/api
               #:luv/metal
               #:luv/msl)
  :components ((:module "hal"
                :components
                ((:module "metal"
                  :components ((:file "gpu")))))))

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
  :components ((:module "hal"
                :components ((:file "canvas")))))

(asdf:defsystem #:luv/canvas/sdl
  :description "SDL realization of luv's native canvas protocol."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas/api
               #:sdl3
               (:feature :darwin #:trivial-main-thread))
  :serial t
  :components ((:module "hal"
                :components
                ((:module "sdl"
                  :components ((:file "canvas")
                               (:file "cocoa" :if-feature :darwin)))))))

(asdf:defsystem #:luv/canvas/vulkan
  :description "Vulkan presentation contexts for luv canvases."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas/sdl
               #:luv/gpu/vulkan)
  :components ((:module "hal"
                :components
                ((:module "vulkan"
                  :components ((:file "canvas")))))))

(asdf:defsystem #:luv/canvas/metal
  :description "Metal 4 presentation contexts for SDL canvases."
  :version "0.0.1"
  :author "Mikael Brockman"
  :if-feature :darwin
  :depends-on (#:luv/canvas/sdl
               #:luv/gpu/metal)
  :components ((:module "hal"
                :components
                ((:module "metal"
                  :components ((:file "canvas")))))))

(asdf:defsystem #:luv/canvas
  :description "SDL canvas presentation for the luv GPU API."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas/vulkan
               #:luv/canvas/metal))

(asdf:defsystem #:luv/metal/tests
  :description "Executable claims for the SDL and Metal 4 presentation seam."
  :version "0.0.1"
  :author "Mikael Brockman"
  :if-feature :darwin
  :depends-on (#:luv/canvas/metal
               #:luv/luvcraft
               #:rove)
  :components ((:module "hal"
                :components
                ((:module "metal"
                  :components ((:file "tests"))))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call
                      '#:rove '#:run-suite
                      (uiop:symbol-call
                       '#:rove '#:find-suite '#:luv/metal/tests))
               (error "luv Metal tests failed"))))

(asdf:defsystem #:luv/luvcraft/quantities
  :description "Backend-neutral semantic quantities for the luvcraft domain."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/arithmetic
               #:luv/world/quantities)
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
               #:luv/arithmetic/records
               #:luv/arithmetic/lisp/vec3
               #:luv/luvcraft/quantities
               #:luv/luvcraft/shaders
               #:sb-concurrency
               #:uiop)
  :serial t
  :components ((:module "luvcraft"
                :components ((:file "fields")
                             (:file "production")
                             (:file "png")
                             (:file "blocks")
                             (:file "terrain")
                             (:file "light")
                             (:file "mesher")
                             (:file "simulation")
                             (:file "persistence")
                             (:file "sky")
                             (:file "frame-performance")
                             (:file "live-pipeline")
                             (:file "app")
                             (:file "streaming")
                             (:file "render")
                             (:file "capture")
                             (:file "benchmark")
                             (:file "gazetteer"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:luv/luvcraft/tests))))

(asdf:defsystem #:luv/examples
  :description "Interactive demos and exploratory applications built on luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas
               #:luv/spir-v
               #:luv/slug)
  :components ((:module "hal"
                :components ((:file "examples")))))

(asdf:defsystem #:luv/tests
  :description "Executable claims about luv's renderer-independent models."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/world
               #:luv/parinfer
               #:rove)
  :serial t
  :components ((:module "luvcraft"
                :components ((:file "world-tests")))
               (:module "parinfer"
                :components ((:file "tests"))))
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
  :serial t
  :components ((:module "luvcraft"
                :components ((:file "tests")
                             (:file "light-tests"))))
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
  :components ((:module "luvcraft"
                :components
                ((:module "tools"
                  :serial t
                  :components ((:file "package")
                               (:file "runner")
                               (:file "block-world")
                               (:file "gazetteer")))))))

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
