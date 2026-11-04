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

;;; Everything this compiles is the checkout's own, so a full WARNING fails the
;;; build.  ASDF's compilation unit signals undefined variables when it
;;; closes, after the per-file logs, so they are collected around it.
(handler-case
    (let ((warnings '()))
      (handler-bind ((warning
                       (lambda (condition)
                         (unless (typep condition 'style-warning)
                           (push condition warnings)))))
        (asdf:load-system :luft))
      (when warnings
        (error "Compiled with ~D full WARNING~:P:~{~%  ~A~}"
               (length warnings) (reverse warnings)))
      (luv-build:finish :done))
  (luv-build:deadline-exceeded ()
    (luv-build:finish :deadline)
    (sb-ext:exit :code 1 :abort t))
  (error (condition)
    (luv-build:failed (princ-to-string condition))
    (luv-build:finish :error)
    (sb-ext:exit :code 1 :abort t)))
