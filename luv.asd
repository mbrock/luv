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
                                                         '#:luv.ghostty.tests))
               (error "luv/ghostty tests failed"))))

(defsystem "luv/terminal"
  :description "Owned terminal devices for luv's libghostty-vt terminal."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("luv/ghostty" "sb-concurrency" "sb-posix" "uiop")
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
                                                         '#:luv.terminal.tests))
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
               (:feature :darwin "luv/objective-c")
               "cffi"
               "closer-mop"
               "ieee-floats"
               "sdl3"
               "zpng"
               "zpb-ttf"
               (:feature :darwin "float-features")
               (:feature :darwin "trivial-main-thread"))
  :serial t
  :components
  ((:module "hal"
    :serial t
    :components
    ((:file "package")
     (:file "tracy")
     (:file "trace")
     (:file "gpu")
     (:file "canvas")
     (:file "png")
     (:module "vulkan-core"
      :pathname "vulkan"
      :serial t
      :components
      ((:file "ffi")
       (:file "abi")
       (:file "native")
       (:module "spir-v"
        :serial t
        :components ((:file "package")
                     (:file "instructions")
                     (:file "module")))))
     (:module "shader"
      :serial t
      :components ((:file "language")
                   (:file "analytic-package")
                   (:file "analytic")
                   (:file "slug-package")
                   (:file "slug-outline")
                   (:file "slug-serialization")
                   (:file "slug-harfbuzz")
                   (:file "slug-truetype")
                   (:file "slug-cache")
                   (:file "slug")))
     (:module "msl"
      :pathname "metal/msl"
      :serial t
      :components ((:file "package")
                   (:file "lowering")))
     (:module "sdl"
      :serial t
      :components ((:file "canvas")
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
      :components ((:file "gpu")
                   (:file "canvas")))
     (:module "metal"
      :if-feature :darwin
      :serial t
      :components
      ((:file "probe")
       (:file "ffi")
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
   (:file "arithmetic/records/tests")
   (:file "arithmetic/records/columnar-tests")
   (:file "arithmetic/records/columnar-simd-tests" :if-feature :sbcl)
   (:file "arithmetic/language/tests")
   (:file "arithmetic/lisp/tests")
   (:file "objective-c/tests" :if-feature :darwin)
   (:file "hal/vulkan/tests")
   (:file "parinfer/tests"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:luv.tests))
               (error "luv tests failed"))))
