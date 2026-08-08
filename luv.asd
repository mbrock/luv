(asdf:defsystem #:luv
  :description "An experimental Common Lisp atelier for Vulkan."
  :version "0.0.1"
  :author "Mikael Brockman"
  #+sbcl
  :around-compile
  #+sbcl
  (lambda (thunk)
    ;; ASDF already establishes a compilation unit, so use OVERRIDE to make
    ;; this nested project policy effective. DEBUG 3 retains source forms in
    ;; compiled FASLs as well as richer debugger metadata.
    (with-compilation-unit (:override t
                            :policy '(optimize (debug 3)))
      (funcall thunk)))
  :depends-on (#:luv/canvas
               #:sdl3
               #:vk
               #+darwin
               #:float-features
               #+darwin
               #:trivial-main-thread)
  :serial t
  :components ((:file "luv")
               (:file "yellow")))

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
  :components ((:file "canvas")))

(asdf:defsystem #:luv/headless
  :description "Headless Vulkan-only entry points for luv."
  :version "0.0.1"
  :author "Mikael Brockman"
  #+sbcl
  :around-compile
  #+sbcl
  (lambda (thunk)
    (with-compilation-unit (:override t
                            :policy '(optimize (debug 3)))
      (funcall thunk)))
  :depends-on (#:vk)
  :serial t
  :components ((:file "package")
               (:file "headless")))
