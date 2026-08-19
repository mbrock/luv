;;;; Loaded by the project-local SLY implementation before Slynk starts.

(require :asdf)

(defparameter cl-user::*luv-project-root*
  (uiop:pathname-directory-pathname *load-truename*))

(asdf:load-asd (merge-pathnames #P"luv.asd" *load-truename*))
(asdf:load-asd (merge-pathnames #P"luvcraft.asd" *load-truename*))
(asdf:load-asd (merge-pathnames #P"telegram.asd" *load-truename*))
(asdf:load-asd (merge-pathnames #P"openai.asd" *load-truename*))
(asdf:load-asd (merge-pathnames #P"luv-wiki.asd" *load-truename*))
(asdf:load-system :luv)
(asdf:load-system :luvcraft)
(asdf:load-system :luvcraft/agent)
(asdf:load-system :luv-wiki)
(asdf:load-asd (merge-pathnames #P"luft.asd" *load-truename*))
(asdf:load-system :luft/render)

(defun register-luv-readtables ()
  "Tell Slynk which packages read under a named readtable, so C-c C-c and
./sly eval read them as their files do.  IN-READTABLE does this itself when
Slynk is loaded first; here Slynk arrives after the project systems."
  (let ((alist (find-symbol "*READTABLE-ALIST*" "SLYNK")))
    (dolist (entry (list (cons "LUV.WIKI.STYLE"
                               (named-readtables:find-readtable 'luv.css:syntax))))
      (setf (symbol-value alist)
            (cons entry (remove (car entry) (symbol-value alist)
                                :key #'car :test #'string=))))))

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
                    (register-luv-readtables)
                    (error (condition)
                      (setf cl-user::*luv-slynk-port* nil)
                      (format *error-output*
                              "Could not start luv Slynk listener: ~A~%"
                              condition)))
                  (return)
             do (sleep 0.05)))
     :name "luv Slynk listener bootstrap")))
