(in-package #:asdf-user)

(defsystem "luv/build"
  :description "Retained CLOS model and ASDF executor for Luv build operations."
  :version "0.0.1"
  :depends-on ("uiop")
  :serial t
  :components ((:file "luv/build/package")
               (:file "luv/build/model")
               (:file "luv/build/asdf"))
  :in-order-to ((test-op (test-op "luv/build/test"))))

(defsystem "luv/build/cli"
  :description "Console presentation and disposable-process policy for Luv builds."
  :version "0.0.1"
  :depends-on ("luv/build" (:require "sb-concurrency") (:require "sb-posix"))
  :components ((:file "luv/build/cli")))

(defsystem "luv/build/test"
  :description "Executable retained-model and ASDF-executor claims."
  :version "0.0.1"
  :depends-on ("luv/build" "luv/test-support")
  :components ((:file "luv/build/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.build.tests)))

(defsystem "luv/test-support"
  :description "The repository's Parachute test runner."
  :version "0.0.1"
  :depends-on ("parachute")
  :components ((:file "luv/test-support")))

(defsystem "luv/lobby"
  :description "A restartable application lobby radio with semantic snapshots."
  :version "0.0.1"
  :author "Mikael Brockman"
  ;; This optional system owns the MQTT dependency; the core LUV system does
  ;; not depend on it.
  :depends-on ("mqtt/net" "uiop")
  :serial t
  :components ((:file "lobby/client-package")
               (:file "lobby/client"))
  :in-order-to ((test-op (test-op "luv/lobby/test"))))

(defsystem "luv/lobby/test"
  :description "Executable lifecycle and snapshot claims for the lobby radio."
  :version "0.0.1"
  :depends-on ("luv/lobby" "luv/test-support")
  :components ((:file "lobby/client-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.lobby.tests)))

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
  :depends-on ("luv/ghostty" "luv/test-support")
  :components ((:file "ghostty/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.ghostty.tests)))

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
  :depends-on ("luv/libav" "luv/test-support")
  :components ((:file "libav/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.libav.tests)))

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
  :depends-on ("luv/terminal/canvas" "luv/test-support" "uiop")
  :components ((:file "terminal/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.terminal.tests)))

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
     (:file "application-lifecycle")
     (:file "frame-resources")
     (:file "live-artifact")
     (:file "png")
     (:file "video")
     (:file "application-capture")
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

(defsystem "luv/tracy-capture"
  :description "Concurrent application-neutral ownership of Tracy captures."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv")
  :serial t
  :components ((:file "hal/tracy-capture-package")
               (:file "hal/tracy-capture"))
  :in-order-to ((test-op (test-op "luv/tracy-capture/test"))))

(defsystem "luv/tracy-capture/test"
  :description "Executable concurrency and subprocess claims for Tracy capture."
  :version "0.0.1"
  :depends-on ("luv/tracy-capture" "luv/test-support")
  :components ((:file "hal/tracy-capture-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.tracy.capture.tests)))

(defsystem "luv/production"
  :description "A bounded latest-value owner/worker production boundary."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "sb-concurrency")
  :serial t
  :components ((:file "production/package")
               (:file "production/production")))

(defsystem "luv/test"
  :description "Executable claims for luv's arithmetic, native bindings, and HAL."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv" "luv/parinfer" "luv/test-support"
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
   (:file "hal/live-artifact-tests")
   (:file "hal/application-lifecycle-tests")
   (:file "hal/application-capture-tests")
   (:file "hal/capture-specification-tests")
   (:file "hal/vulkan/tests")
   (:file "parinfer/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.tests)))

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
  :depends-on ("luv" "luv/build" "luv/lobby" "mcclim-render" "cl-dejavu")
  :serial t
  :components ((:module "mcclim"
                :serial t
                :components ((:file "package")
                             (:file "input")
                             (:file "paint")
                             (:file "port")
                             (:file "mirror")
                             (:file "gpu")
                             (:file "command-menu")
                             (:file "source-update")
                             (:file "metabar")
                             (:file "application-status-bar")
                             (:file "widget-lab")
                             (:file "workbench-backend-proof")
                             (:file "compositor")
                             (:file "direct-compositor"))))
  :in-order-to ((test-op (test-op "luv/mcclim/test"))))

(defsystem "luv/mcclim/test"
  :description "Executable claims for luv's direct McCLIM GPU backend."
  :version "0.0.1"
  :depends-on ("luv/mcclim" "luv/test-support")
  :serial t
  :components ((:file "mcclim/tests")
               (:file "mcclim/command-menu-tests")
               (:file "mcclim/source-update-tests")
               (:file "mcclim/metabar-tests")
               (:file "mcclim/status-bar-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:mcluv.tests)))

(defsystem "luv/workbench"
  :description "The Luv-owned screen-space application shell substrate."
  :version "0.0.1"
  :depends-on ("luv/mcclim")
  :serial t
  :components ((:file "mcclim/workbench-package")
               (:file "mcclim/workbench"))
  :in-order-to ((test-op (test-op "luv/workbench/test"))))

(defsystem "luv/workbench/test"
  :description "Executable workbench layer, routing, and lifecycle claims."
  :version "0.0.1"
  :depends-on ("luv/workbench" "luv/test-support")
  :components ((:file "mcclim/workbench-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.workbench.tests)))

(defsystem "luv/application-agent"
  :description "Application-neutral asynchronous language-agent harness."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("openai" "mcclim" "uiop" "sb-concurrency")
  :serial t
  :components ((:file "application-agent/package")
               (:file "application-agent/presentations")
               (:file "application-agent/transcript")
               (:file "application-agent/harness")
               (:file "application-agent/command-tool"))
  :in-order-to ((test-op (test-op "luv/application-agent/test"))))

(defsystem "luv/application-agent/test"
  :description "Executable lifecycle, transcript, and command-tool claims."
  :version "0.0.1"
  :depends-on ("luv/application-agent" "luv/test-support")
  :serial t
  :components ((:file "application-agent/tests-package")
               (:file "application-agent/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.application-agent.tests)))

(defsystem "luv/lobby/mcclim"
  :description "A textureless retained-GPU HUD for the shared lobby radio."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/lobby" "luv/mcclim")
  :serial t
  :components ((:file "lobby/mcclim-package")
               (:file "lobby/mcclim"))
  :in-order-to ((test-op (test-op "luv/lobby/mcclim/test"))))

(defsystem "luv/lobby/mcclim/test"
  :description "GPU-media and native-resolution claims for the lobby HUD."
  :version "0.0.1"
  :depends-on ("luv/lobby/mcclim" "luv/test-support")
  :components ((:file "lobby/mcclim-tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:luv.test-support '#:test-package
                               '#:luv.lobby.mcclim.tests)))

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
