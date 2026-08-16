;;;; Lisp entry point run by the Nix-backed scripts/luv launcher.

(require :asdf)

(defun launcher-project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(asdf:load-asd (merge-pathnames #P"luv.asd" (launcher-project-root)))
(asdf:load-asd (merge-pathnames #P"luvcraft.asd" (launcher-project-root)))
(asdf:load-system :luvcraft/tools)
(funcall (find-symbol "MAIN" "LUVCRAFT.TOOLS"))
