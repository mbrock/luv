;;;; Capture hidden block-world frames from a fresh SBCL in the luv shell.

(require :asdf)

(defun script-project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(defun parse-positive-integer (string name)
  (let ((value (parse-integer string :junk-allowed nil)))
    (unless (plusp value)
      (error "~A must be positive, got ~D." name value))
    value))

(defun call-luv (name &rest arguments)
  (apply (symbol-function (find-symbol name "LUV")) arguments))

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
    (format *error-output*
            "No display found; using SDL_VIDEODRIVER=offscreen.~%")))

(defun maybe-use-lavapipe-vulkan-driver ()
  (let ((icd (uiop:getenv "LUV_LAVAPIPE_ICD")))
    (when (and icd
               (probe-file icd)
               (not (uiop:getenv "VK_DRIVER_FILES"))
               (string= (uiop:getenv "SDL_VIDEODRIVER") "offscreen"))
      (set-process-environment "VK_DRIVER_FILES" icd)
      (format *error-output* "Using lavapipe Vulkan ICD: ~A~%" icd))))

(let* ((project-root (script-project-root))
       (arguments (uiop:command-line-arguments))
       (target (or (first arguments) "/tmp/luv-block-world.png"))
       (count (and (second arguments)
                   (parse-positive-integer (second arguments) "count"))))
  (maybe-use-offscreen-sdl-driver)
  (maybe-use-lavapipe-vulkan-driver)
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root))
  (asdf:load-system :luv/examples)
  (if count
      (dolist (pathname
                (call-luv "CAPTURE-HIDDEN-CUBE-WORLD-FRAMES"
                          (pathname target) :count count))
        (format t "~A~%" pathname))
      (format t "~A~%"
              (call-luv "CAPTURE-HIDDEN-CUBE-WORLD-SCREENSHOT"
                        (pathname target)))))
