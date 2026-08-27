;;;; Quiet, dependency-light checks for the non-graphical LUFT system.

(require :asdf)

(defparameter *project-root*
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))

(let ((*default-pathname-defaults* *project-root*)
      (*compile-verbose* nil)
      (*compile-print* nil)
      (*load-verbose* nil)
      (*load-print* nil))
  (handler-bind ((warning #'muffle-warning)
                 (sb-ext:compiler-note #'muffle-warning))
    (asdf:load-asd (truename "luft.asd"))
    (asdf:test-system :luft/test)))
