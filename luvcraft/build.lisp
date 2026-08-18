;;;; Build the luvcraft executable from a fresh SBCL in the luv development shell.

(require :asdf)

(let ((project-root
        (truename
         (merge-pathnames #P"../"
                          (uiop:pathname-directory-pathname *load-truename*)))))
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"luvcraft.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"mcluv.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"telegram.asd" project-root))
  (let ((slynk-root (uiop:getenv "LUV_SLYNK_DIR")))
    (unless slynk-root
      (error "LUV_SLYNK_DIR is not set; build luvcraft through ./scripts/dev."))
    (asdf:load-asd
     (merge-pathnames #P"slynk.asd"
                      (uiop:ensure-directory-pathname slynk-root)))))

(asdf:make :luvcraft/program)
