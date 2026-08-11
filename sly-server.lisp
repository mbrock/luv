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

(defun set-process-environment (name value)
  (unless
      (zerop
       (sb-alien:alien-funcall
        (sb-alien:extern-alien
         "setenv"
         (function sb-alien:int
                   sb-alien:c-string sb-alien:c-string sb-alien:int))
        name value 1))
    (error "Could not set ~A in the process environment." name)))

(defun maybe-use-offscreen-sdl-driver ()
  (unless (or (uiop:getenv "SDL_VIDEODRIVER")
              (uiop:getenv "DISPLAY")
              (uiop:getenv "WAYLAND_DISPLAY"))
    (set-process-environment "SDL_VIDEODRIVER" "offscreen")
    (format t "~&No display found; using SDL_VIDEODRIVER=offscreen.~%")))

(defun maybe-use-lavapipe-vulkan-driver ()
  (let ((icd (uiop:getenv "LUV_LAVAPIPE_ICD")))
    (when (and icd
               (probe-file icd)
               (not (uiop:getenv "VK_DRIVER_FILES"))
               (string= (uiop:getenv "SDL_VIDEODRIVER") "offscreen"))
      (set-process-environment "VK_DRIVER_FILES" icd)
      (format t "~&Using lavapipe Vulkan ICD: ~A~%" icd))))

(let* ((project-root
         (uiop:pathname-directory-pathname *load-truename*))
       (slynk-root (required-directory "LUV_SLYNK_DIR"))
       (port (parse-integer (getenv-or "LUV_SLYNK_PORT" "4005"))))
  (maybe-use-offscreen-sdl-driver)
  (maybe-use-lavapipe-vulkan-driver)
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
