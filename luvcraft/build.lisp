;;;; Build the luvcraft executable from a fresh SBCL in the luv development shell.

(require :asdf)

(let ((project-root
        (truename
         (merge-pathnames #P"../"
                          (uiop:pathname-directory-pathname *load-truename*)))))
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"luvcraft.asd" project-root)))

(asdf:load-system :luvcraft)
(asdf:make :luvcraft)
