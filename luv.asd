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
  :depends-on (#:sdl3
               #:vk)
  :serial t
  :components ((:file "luv")
               (:file "yellow")))
