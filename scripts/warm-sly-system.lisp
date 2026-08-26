;;;; Compile the checkout-owned part of the managed Sly image without putting
;;;; it in the reusable dependency core.

(require :asdf)
(require :sb-concurrency)
(require :sb-posix)

(defparameter *project-root*
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))
(defparameter *system* (or (uiop:getenv "LUV_SLY_SYSTEM") "luv-workbench"))

(asdf:initialize-source-registry
 `(:source-registry
   (:directory ,(namestring *project-root*))
   :inherit-configuration))
(load (merge-pathnames #P"luvcraft/build-progress.lisp" *project-root*))
(luv-build:start *project-root* :system *system* :invocation "make Sly cache")

(handler-case
    (progn
      (asdf:load-system *system*)
      (luv-build:finish :done))
  (luv-build:deadline-exceeded ()
    (luv-build:finish :deadline)
    (sb-ext:exit :code 1 :abort t))
  (error (condition)
    (luv-build:failed (princ-to-string condition))
    (luv-build:finish :error)
    (sb-ext:exit :code 1 :abort t)))
