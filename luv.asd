(in-package #:asdf-user)

(defsystem "luv/arithmetic"
  :description "Semantic quantities and their inspectable Lisp and shader arithmetic."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("closer-mop")
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
                   (:file "records")))
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
                   (:file "slug-package")
                   (:file "slug-outline")
                   (:file "slug-serialization")
                   (:file "slug-harfbuzz")
                   (:file "slug-truetype")
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
  :depends-on ("luv" "luv/parinfer" "rove")
  :serial t
  :components
  ((:file "arithmetic/tests")
   (:file "arithmetic/records/tests")
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
