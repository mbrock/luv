(unless (asdf:registered-system "slynk")
  (let ((directory (uiop:getenv "LUV_SLYNK_DIR")))
    (unless directory
      (error "LUV_SLYNK_DIR is not set; build luvcraft through the luv development environment."))
    (asdf:load-asd
     (merge-pathnames #P"slynk.asd"
                      (uiop:ensure-directory-pathname directory)))))

(asdf:defsystem #:luvcraft
  :description "The interactive luv block world as a standalone program."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on (#:luv/luvcraft
               #:sb-posix
               #:slynk)
  :components ((:module "luvcraft"
                :components ((:file "main"))))
  :build-operation "program-op"
  :build-pathname "build/luvcraft"
  :entry-point "luvcraft:main")
