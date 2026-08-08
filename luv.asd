(asdf:defsystem #:luv
  :description "An experimental Common Lisp atelier for Vulkan."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/canvas))

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
