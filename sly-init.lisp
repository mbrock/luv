;;;; Loaded by the project-local SLY implementation before Slynk starts.

(load
 (merge-pathnames
  #P"setup.lisp"
  (let ((configured-home (sb-ext:posix-getenv "QUICKLISP_HOME")))
    (if configured-home
        (pathname (format nil "~A/" configured-home))
        (merge-pathnames #P"quicklisp/" (user-homedir-pathname))))))
(asdf:load-asd (merge-pathnames #P"luv.asd" *load-truename*))
(ql:quickload :luv :silent t)

(defvar cl-user::*luv-slynk-port* nil)

(unless cl-user::*luv-slynk-port*
  (let ((port (parse-integer (or (uiop:getenv "LUV_SLYNK_PORT") "4005"))))
    ;; This file loads before SLY has sent SBCL its Slynk bootstrap form. Wait
    ;; for that form to finish, then open a durable listener for short-lived
    ;; ./sly clients alongside Emacs's own one-shot connection.
    (setf cl-user::*luv-slynk-port* :starting)
    (sb-thread:make-thread
     (lambda ()
       (loop for package = (find-package "SLYNK")
             for create-server =
               (and package (find-symbol "CREATE-SERVER" package))
             when (and create-server (fboundp create-server))
               do (handler-case
                      (setf cl-user::*luv-slynk-port*
                            (funcall create-server
                                     :port port :dont-close t))
                    (error (condition)
                      (setf cl-user::*luv-slynk-port* nil)
                      (format *error-output*
                              "Could not start luv Slynk listener: ~A~%"
                              condition)))
                  (return)
             do (sleep 0.05)))
     :name "luv Slynk listener bootstrap")))
