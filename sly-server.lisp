;;;; Standalone Slynk server bootstrap for ./sly.

(require :asdf)

(defparameter cl-user::*luv-project-root* nil)

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
  (setf cl-user::*luv-project-root* project-root)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(namestring slynk-root))
     :inherit-configuration))
  (asdf:load-asd (merge-pathnames #P"slynk.asd" slynk-root))
  ;; Slynk first, so the wiki's IN-READTABLE forms can register their
  ;; readtables while the durable image is assembled.
  (asdf:load-system :slynk)
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"luvcraft.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"luv-wiki.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"luft.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"mcluv.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"telegram.asd" project-root))
  (asdf:load-system :luv)
  (asdf:load-system :luvcraft)
  ;; The McCLIM presentation layer -- hotbar, inventory, metabar, the film
  ;; browser -- is part of the game whichever way it is started, so the
  ;; durable image carries it just as the shipped executable does.  So is the
  ;; command layer above it, which is where the keyboard now lives: without it
  ;; the game window would take mouse and movement and answer no verb at all.
  (asdf:load-system :luvcraft/clim)
  (asdf:load-system :luv-wiki)
  (asdf:load-system :luft/render)
  (format t "~&Starting luv Slynk on 127.0.0.1:~D.~%" port)
  (funcall (find-symbol "CREATE-SERVER" "SLYNK")
           :interface "127.0.0.1"
           :port port
           :dont-close t)
  (format t "~&Luv Slynk is ready on 127.0.0.1:~D.~%" port)
  (loop (sleep 3600)))
