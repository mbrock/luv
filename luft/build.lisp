;;;; Build the standalone LUFT atelier with narrated ASDF progress.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  (require :sb-concurrency)
  (require :sb-posix))

(defparameter *project-root*
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))

(load (merge-pathnames #P"luvcraft/build-progress.lisp" *project-root*))

(luv-build:start *project-root* :system :luft/program)

(uiop:register-image-dump-hook
 (lambda () (luv-build:finish :done)))

(handler-case (progn
                (asdf:make :luft/program)
                ;; A current PROGRAM-OP returns instead of dumping an image,
                ;; so its dump hook never gets the chance to close the report.
                (luv-build:finish :done))
  (luv-build:deadline-exceeded ()
    (luv-build:finish :deadline)
    (sb-ext:exit :code 1 :abort t))
  (error (condition)
    (luv-build:failed (princ-to-string condition))
    (luv-build:finish :error)
    (sb-ext:exit :code 1 :abort t)))
