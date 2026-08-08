;;;; Build the mcluv executable from a fresh SBCL in the luv development shell.

(require :asdf)

(load
 (merge-pathnames
  #P"setup.lisp"
  (let ((configured-home (uiop:getenv "QUICKLISP_HOME")))
    (if configured-home
        (pathname (format nil "~A/" configured-home))
        (merge-pathnames #P"quicklisp/" (user-homedir-pathname))))))

(let ((project-root (uiop:pathname-directory-pathname *load-truename*)))
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"mcluv.asd" project-root)))

(ql:quickload :mcluv :silent t)
(asdf:make :mcluv)
