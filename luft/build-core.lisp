;;;; Compile and load the non-graphical LUFT topology and mesher.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (require :sb-concurrency)
  (require :sb-posix))

(defparameter *project-root*
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))

(load (merge-pathnames #P"luvcraft/build-progress.lisp" *project-root*))
(luv-build:start *project-root* :system :luft)

(handler-case
    (progn
      (asdf:load-system :luft)
      (luv-build:finish :done))
  (luv-build:deadline-exceeded ()
    (luv-build:finish :deadline)
    (sb-ext:exit :code 1 :abort t))
  (error (condition)
    (luv-build:failed (princ-to-string condition))
    (luv-build:finish :error)
    (sb-ext:exit :code 1 :abort t)))
