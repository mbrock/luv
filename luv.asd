(in-package #:asdf-user)

(defsystem "luv/domains"
  :description "Minimal shared protocols for finite domains."
  :version "0.0.1"
  :author "Mikael Brockman"
  :serial t
  :components ((:file "domains/package")
               (:file "domains/domains")))

(defsystem "luv/arithmetic"
  :description "Semantic quantities and their inspectable Lisp and shader arithmetic."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("closer-mop" "luv/domains")
  :serial t
  :components
  ((:module "arithmetic"
    :serial t
    :components
    ((:file "package")
     (:file "semantics")
     (:file "declarations")
     (:module "records"
      :serial t
      :components ((:file "package")
                   (:file "records")
                   (:file "columnar")))
     (:module "language"
      :serial t
      :components ((:file "package")
                   (:file "frontend")))
     (:module "lisp"
      :serial t
      :components ((:file "package")
                   (:file "compiler")
                   (:file "vec3-package")
                   (:file "vec3")))))))

(defsystem "luv/shader"
  :description "The backend-neutral typed shader language and lowering protocols."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("closer-mop" "luv/arithmetic")
  :serial t
  :components ((:file "hal/shader/package")
               (:file "hal/shader/language")))

(defsystem "luv/spir-v"
  :description "Literal SPIR-V modules and lowering for luv's shader language."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("cffi" "closer-mop" "luv/shader")
  :serial t
  :components ((:file "hal/vulkan/spir-v/package")
               (:file "hal/vulkan/spir-v/instructions")
               (:file "hal/vulkan/spir-v/module")
               (:file "hal/vulkan/spir-v/lowering")))

(defsystem "luv/msl"
  :description "Structured Metal Shading Language lowering for luv's shader graph."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/shader")
  :serial t
  :components ((:file "hal/metal/msl/package")
               (:file "hal/metal/msl/lowering")))

(defsystem "luv/wgsl"
  :description "Structured WebGPU Shading Language lowering for luv's shader graph."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/shader")
  :serial t
  :components ((:file "hal/webgpu/wgsl/package")
               (:file "hal/webgpu/wgsl/lowering")))

(defsystem "luv/objective-c"
  :description "A declared Objective-C foreign object system with opt-in tracing."
  :version "0.0.1"
  :author "Mikael Brockman"
  :if-feature :darwin
  :depends-on ("cffi" "cffi-libffi")
  :serial t
  :components
  ((:module "objective-c"
    :serial t
    :components ((:file "package")
                 (:static-file "exception-bridge.m")
                 (:file "exception-bridge")
                 (:file "runtime")
                 (:file "foundation")))))

(defsystem "luv/ghostty"
  :description "A small experimental CFFI binding to libghostty-vt."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("cffi" "cffi-libffi")
  :serial t
  :components
  ((:module "ghostty"
    :serial t
    :components ((:static-file "README.md")
                 (:file "package")
                 (:file "ffi")
                 (:file "terminal")
                 (:file "screen")
                 (:file "key"))))
  :in-order-to ((test-op (test-op "luv/ghostty/test"))))

(defsystem "luv/ghostty/test"
  :description "Proof-of-concept integration tests for libghostty-vt."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/ghostty" "rove")
  :components ((:file "ghostty/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luv.ghostty.tests)
                                       :style :luv)
               (error "luv/ghostty tests failed"))))

(defsystem "luv/mupdf"
  :description "A narrow CFFI binding to MuPDF: pages as pixels or as text."
  :long-description
  "MuPDF's objects are opaque handles with accessor functions, so this binding
declares almost no C layouts -- only the matrix and the rectangle it passes by
value.  Structured text comes back through MuPDF's own XML serializer rather
than by walking its records, which keeps the whole system free of groveled
offsets and of headers."
  :version "0.0.1"
  :author "Mikael Brockman"
  ;; cffi-libffi is what lets a struct be passed by value.  MuPDF's page
  ;; rendering takes its transform that way and there is no variant that does
  ;; not, so the two layouts this binding declares have to be callable.
  :depends-on ("cffi" "cffi-libffi" "uiop" "alexandria")
  :serial t
  :components ((:module "mupdf"
                :serial t
                :components ((:file "package")
                             (:file "ffi")
                             (:file "document")))))

(defsystem "luv/libav"
  :description "A CFFI binding to FFmpeg's libav*, groveled against its headers."
  :version "0.0.1"
  :author "Mikael Brockman"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "uiop")
  :serial t
  :components
  ((:module "libav"
    :serial t
    :components ((:static-file "README.md")
                 (:static-file "test-pattern.mp4")
                 (:static-file "test-tone.mp4")
                 (:file "package")
                 (:cffi-grovel-file "abi")
                 (:file "ffi")
                 (:file "frame")
                 (:file "decode")
                 (:file "audio"))))
  :in-order-to ((test-op (test-op "luv/libav/test"))))

(defsystem "luv/libav/test"
  :description "Executable claims for FFmpeg's version agreement and AVFrame layout."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/libav" "rove")
  :components ((:file "libav/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luv.libav.tests)
                                       :style :luv)
               (error "luv/libav tests failed"))))

(defsystem "luv/terminal"
  :description "Owned terminal devices for luv's libghostty-vt terminal."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/ghostty" "cffi" "sb-concurrency" "sb-posix" "uiop")
  :serial t
  :components
  ((:module "terminal"
    :serial t
    :components ((:file "package")
                 (:file "pty"))))
  :in-order-to ((test-op (test-op "luv/terminal/test"))))

(defsystem "luv/terminal/canvas"
  :description "Portable canvas key events projected into a terminal device."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "luv/terminal")
  :components ((:file "terminal/canvas")))

(defsystem "luv/terminal/test"
  :description "Executable PTY ownership and terminal-driving claims."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/terminal/canvas" "rove" "uiop")
  :components ((:file "terminal/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luv.terminal.tests)
                                       :style :luv)
               (error "luv/terminal tests failed"))))

(defsystem "luv/parinfer"
  :description "The connection-free indentation and parenthesis checker."
  :version "0.0.1"
  :author "Mikael Brockman"
  :components ((:file "parinfer/implementation")))

(defsystem "luv"
  :description "An experimental Common Lisp GPU and native-canvas atelier."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/arithmetic"
               "luv/shader"
               "luv/spir-v"
               "luv/msl"
               "luv/wgsl"
               (:feature :darwin "luv/objective-c")
               "cffi"
               "closer-mop"
               "ieee-floats"
               "sdl3"
               "zpng"
               "zpb-ttf"
               #+sbcl (:require #:sb-posix)
               (:feature :darwin "float-features")
               (:feature :darwin "trivial-main-thread"))
  :serial t
  :components
  ((:module "hal"
    :serial t
    :components
    ((:file "package")
     (:file "log")
     (:file "tracy")
     (:file "trace")
     (:file "gpu")
     (:file "canvas")
     (:file "png")
     (:file "video")
     (:file "capture-specification")
     (:module "vulkan-core"
      :pathname "vulkan"
      :serial t
      :components
      ((:file "ffi")
       (:file "abi")
       (:file "native")))
     (:module "shader"
      :serial t
      :components ((:file "analytic-package")
                   (:file "analytic")
                   (:file "slug-package")
                   (:file "slug-outline")
                   (:file "slug-serialization")
                   (:file "slug-harfbuzz")
                   (:file "slug-truetype")
                   (:file "slug-cache")
                   (:file "slug")
                   (:file "lattice")))
     (:module "sdl"
      :serial t
      :components ((:file "canvas")
                   (:file "audio")
                   (:file "cocoa" :if-feature :darwin)))
     (:module "vulkan-backend"
      :pathname "vulkan"
      :serial t
      #+sbcl
      :around-compile
      #+sbcl
      (lambda (thunk)
        (with-compilation-unit (:override t
                                :policy '(optimize (debug 3)))
          (funcall thunk)))
      :components ((:file "validation")
                   (:file "gpu")
                   (:file "canvas")))
     (:module "metal"
      :if-feature :darwin
      :serial t
      :components
      ((:file "probe")
       (:file "ffi")
       (:file "iosurface")
       (:file "gpu")
       (:file "canvas")))
     (:file "examples"))))
  :in-order-to ((test-op (test-op "luv/test"))))

(defsystem "luv/test"
  :description "Executable claims for luv's arithmetic, native bindings, and HAL."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "luv/parinfer" "rove"
               #+sbcl (:require #:sb-simd))
  :serial t
  :components
  ((:file "arithmetic/tests")
   (:file "domains/tests")
   (:file "arithmetic/records/tests")
   (:file "arithmetic/records/columnar-tests")
   (:file "arithmetic/records/columnar-simd-tests" :if-feature :sbcl)
   (:file "arithmetic/language/tests")
   (:file "arithmetic/lisp/tests")
   (:file "objective-c/tests" :if-feature :darwin)
   (:file "hal/gpu-tests")
   (:file "hal/capture-specification-tests")
   (:file "hal/vulkan/tests")
   (:file "parinfer/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luv.tests)
                                       :style :luv)
               (error "luv tests failed"))))

(defsystem "luv/showcase"
  :description "Reproducible wiki screenshots and films rendered from named capture recipes."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luvcraft/agent" "luft/render")
  :build-operation "luv:capture-op"
  :serial t
  :components ((:module "captures"
                :serial t
                :components ((:file "package")
                             (:file "construction")
                             (:file "pre-noon")
                             (:file "day-cycle")
                             (:file "gallery")
                             (:file "gameplay-actions")
                             (:file "characters-instruments")
                             (:file "terminal-cinema")
                             (:file "reference-plates")
                             (:file "luft-portraits")))))

(defsystem "luv/mcclim"
  :description "An experimental McCLIM backend presented through luv canvases."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "mcclim-render" "cl-dejavu")
  :serial t
  :components ((:module "mcclim"
                :serial t
                :components ((:file "package")
                             (:file "paint")
                             (:file "port")
                             (:file "mirror")
                             (:file "gpu")
                             (:file "widget-lab")
                             (:file "compositor"))))
  :in-order-to ((test-op (test-op "luv/mcclim/test"))))

(defsystem "luv/mcclim/test"
  :description "Executable claims for luv's direct McCLIM GPU backend."
  :version "0.0.1"
  :depends-on ("luv/mcclim" "rove")
  :components ((:file "mcclim/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:mcluv.tests)
                                       :style :luv)
               (error "luv/mcclim tests failed"))))

(defsystem "luv/mcclim/gallery"
  :description "A screenshot gallery and primitive-fallback audit for McCLIM."
  :version "0.0.1"
  :depends-on ("luv/mcclim" "clim-examples")
  :components ((:file "mcclim/gallery")))

(defsystem "luv/mcclim/roundrect-benchmark"
  :description "A Tracy A/B of native and decomposed McCLIM roundrects."
  :version "0.0.1"
  :depends-on ("luv/mcclim")
  :components ((:file "mcclim/roundrect-benchmark")))

(defsystem "luv/mcclim/paint-benchmark"
  :description "A Tracy comparison of solid, gradient, and image GUI paints."
  :version "0.0.1"
  :depends-on ("luv/mcclim")
  :components ((:file "mcclim/paint-benchmark")))
