;;;; Standalone Slynk server bootstrap for ./sly.

(require :asdf)

(defun getenv-or (name default)
  (or (uiop:getenv name) default))

(defun required-directory (name)
  (let ((value (uiop:getenv name)))
    (unless value
      (error "~A is not set. Enter `nix develop` so the luv shell can provide it."
             name))
    (uiop:ensure-directory-pathname value)))

(let* ((project-root
         (uiop:pathname-directory-pathname *load-truename*))
       (slynk-root (required-directory "LUV_SLYNK_DIR"))
       (port (parse-integer (getenv-or "LUV_SLYNK_PORT" "4005"))))
  (load
   (merge-pathnames
    #P"setup.lisp"
    (let ((configured-home (uiop:getenv "QUICKLISP_HOME")))
      (if configured-home
          (pathname (format nil "~A/" configured-home))
          (merge-pathnames #P"quicklisp/" (user-homedir-pathname))))))
  (uiop:symbol-call :ql :quickload '(:sdl3) :silent t)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(namestring slynk-root))
     :inherit-configuration))
  (asdf:load-asd (merge-pathnames #P"slynk.asd" slynk-root))
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root))
  (asdf:load-system :slynk)
  (asdf:load-system :luv)
  (format t "~&Starting luv Slynk on 127.0.0.1:~D.~%" port)
  (funcall (find-symbol "CREATE-SERVER" "SLYNK")
           :interface "127.0.0.1"
           :port port
           :dont-close t)
  (format t "~&Luv Slynk is ready on 127.0.0.1:~D.~%" port)
  (loop (sleep 3600)))
