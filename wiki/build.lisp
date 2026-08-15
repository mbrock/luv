;;;; Build the ./wiki executable from a fresh SBCL in the luv development shell.

(require :asdf)

(let ((project-root
        (truename
         (merge-pathnames #P"../"
                          (uiop:pathname-directory-pathname *load-truename*)))))
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root)))

(asdf:load-system :luv-wiki/cli)
(uiop:symbol-call '#:luv.wiki.cli '#:capture-asdf-configuration)
(asdf:make :luv-wiki/cli)
