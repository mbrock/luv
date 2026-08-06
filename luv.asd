(asdf:defsystem #:luv
  :description "An experimental Common Lisp atelier for Vulkan."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:sdl3
               #:vk)
  :serial t
  :components ((:file "luv")
               (:file "yellow")))

(asdf:defsystem #:luv/mcp
  :description "A shared-image cl-mcp server for hacking on luv from SLY and Codex."
  :depends-on (#:cl-mcp)
  :serial t
  :components ((:file "mcp")))
