(in-package #:luvcraft)

(defun executable-directory ()
  (uiop:pathname-directory-pathname (truename sb-ext:*runtime-pathname*)))

(defun slynk-endpoint-pathname ()
  (let ((override (uiop:getenv "LUVCRAFT_SLYNK_ENDPOINT")))
    (if override
        (pathname override)
        (merge-pathnames #P"luvcraft.slynk" (executable-directory)))))

(defun requested-slynk-port ()
  (let ((value (uiop:getenv "LUVCRAFT_SLYNK_PORT")))
    (if value
        (let ((port (parse-integer value :junk-allowed t)))
          (unless (and port
                       (<= 0 port 65535)
                       (string= value (format nil "~D" port)))
            (error "LUVCRAFT_SLYNK_PORT must be an integer from 0 through 65535, not ~S."
                   value))
          port)
        0)))

(defun endpoint-owned-p (pathname pid port)
  (with-open-file (stream pathname :if-does-not-exist nil)
    (and stream
         (let ((*read-eval* nil))
           (and (eql pid (read stream nil nil))
                (eql port (read stream nil nil)))))))

(defun publish-slynk-endpoint (pathname pid port)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~D ~D~%" pid port)))

(defun call-with-slynk (function)
  ;; The Windows download is a player, without a development listener or
  ;; writes beside a potentially read-only installed executable.
  #+win32
  (return-from call-with-slynk (funcall function))
  #-win32
  (setf slynk:*log-output* (make-broadcast-stream))
  #-win32
  (let* ((pid (sb-posix:getpid))
         (port (slynk:create-server
                :interface "127.0.0.1"
                :port (requested-slynk-port)
                :dont-close t))
         (endpoint (slynk-endpoint-pathname)))
    (unwind-protect
         (progn
           (publish-slynk-endpoint endpoint pid port)
           (format t "Luvcraft Slynk is ready on 127.0.0.1:~D (pid ~D).~%"
                   port pid)
           (format t "Attach with ./sly --luvcraft COMMAND ...~%")
           (finish-output)
           (funcall function))
      (when (endpoint-owned-p endpoint pid port)
        (ignore-errors (delete-file endpoint))))))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: luvcraft [--metal | --vulkan] [--tracy] [--fullscreen] [--world FILE]~%")
  (format stream "       luvcraft [--help | --game-smoke-test PNG | --smoke-test PNG | --vulkan-smoke-test PNG | --metal-smoke-test PNG]~%")
  (format stream "~%")
  (format stream "With no arguments, resume the world using Metal 4 on macOS and Vulkan elsewhere.~%")
  (format stream "The game uses Luft terrain, initially without bevels; Enter opens the metabar toggle.~%")
  (format stream "--metal and --vulkan explicitly select an interactive backend.~%")
  (format stream "--tracy exposes live frame and worker zones to Tracy 0.14.0.~%")
  (format stream "--fullscreen opens the game on the whole display instead of in a window.~%")
  (format stream "--world loads or creates the named persistent world.~%")
  (format stream "--game-smoke-test PNG opens a temporary game, captures it, and checks save/reload.~%")
  (format stream "--smoke-test, --vulkan-smoke-test, and --metal-smoke-test run the game smoke test on the selected backend.~%"))

(defun make-metal-provider ()
  #+darwin
  (make-instance 'luv:metal-gpu-provider)
  #-darwin
  (error "The Metal backend is only available on Darwin."))

(defun make-vulkan-provider ()
  (make-instance 'luv:vulkan-gpu-provider))

(defun run-interactive (&key provider tracy-p fullscreen-p
                             (world-pathname
                               (default-luvcraft-world-pathname)))
  "Run luvcraft until its native window closes."
  (when tracy-p (start-luvcraft-tracy))
  (let* ((viewer (luvcraft.luft.game:play
                  :provider (or provider luv:*gpu-provider*)
                  :fullscreen-p fullscreen-p :pathname world-pathname))
         (canvas (luft.render:viewer-canvas viewer))
         (controller (luft.render::viewer-stop-controller viewer)))
    (unwind-protect
         (progn
           ;; Native close tears down beside SDL and closes the canvas last.
           ;; Keep the executable alive through that final close.
           (start-macos-updates canvas)
           (loop until (eq :closed (luv:canvas-state canvas))
                 do (when *macos-update-quit-requested*
                      (setf *macos-update-quit-requested* nil)
                      (luft.render:stop-viewer viewer))
                    (sleep 0.05))
           (let ((failure (luv::sdl-canvas-startup-error canvas)))
             (when failure (error failure)))
           ;; Report worker teardown failures in the executable too.
           (unless (eq :running (stop-controller-state controller))
             (wait-for-controlled-stop controller)))
      (luft.render:stop-viewer viewer))
    (finish-macos-updates)))

(defun parse-interactive-options (arguments)
  (let ((provider nil)
        (tracy-p nil)
        (fullscreen-p nil)
        (world-pathname (default-luvcraft-world-pathname)))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--metal")
                (setf provider (make-metal-provider)))
               ((string= argument "--vulkan")
                (setf provider (make-vulkan-provider)))
               ((string= argument "--tracy")
                (setf tracy-p t))
               ((string= argument "--fullscreen")
                (setf fullscreen-p t))
               ((string= argument "--world")
                (unless arguments
                  (error "--world requires a pathname."))
                (setf world-pathname (pathname (pop arguments))))
               (t (return-from parse-interactive-options
                    (values nil nil nil nil nil)))))
    (values provider world-pathname tracy-p fullscreen-p t)))

(defun run-game-smoke-test (pathname &optional provider)
  "Exercise the shipped game's startup, lazy assets, frame loop, and save."
  ;; These meshes are normally loaded only when a companion appears. A bare
  ;; terrain frame would not detect a package that accidentally omitted them.
  (dolist (kind '(:cat :gnome :fantus))
    (luvcraft.agent::load-companion-mesh kind))
  (uiop:with-temporary-file (:pathname world :type "world")
    (delete-file world)
    (dotimes (pass 2)
      (let* ((viewer (luvcraft.luft.game:play
                      :provider (or provider luv:*gpu-provider*)
                      :pathname world :width 640 :height 480))
             (canvas (luft.render:viewer-canvas viewer))
             (scene (luft.render:viewer-source viewer))
             (deadline (+ (get-internal-real-time)
                          (* 120 internal-time-units-per-second))))
        (unwind-protect
             (progn
               (loop
                 (when (or (luv::sdl-canvas-startup-error canvas)
                           (luv:canvas-failures canvas)
                           (luft.terrain:streaming-scene-production-errors scene))
                   (error "Game smoke failed: ~S" (luv:canvas-health canvas)))
                 (when (and (> (luv::sdl-canvas-frame-count canvas) 5)
                            (luv::call-on-sdl-canvas-thread
                             canvas
                             (lambda ()
                               ;; A running HUD alone does not prove that the
                               ;; terrain underneath the player is usable.
                               (and (plusp (hash-table-count
                                            (luft.terrain:streaming-scene-loaded scene)))
                                    (null (luft.terrain:streaming-scene-replacement scene))
                                    (luft.render::viewer-player-ground-resident-p viewer)))))
                   (return))
                 (when (> (get-internal-real-time) deadline)
                   (error "Game smoke timed out: ~S" (luv:canvas-health canvas)))
                 (sleep 0.1))
               (when (= pass 1)
                 (luft.render:capture-viewer-frame pathname viewer)))
          (luft.render:stop-viewer viewer))
        (unless (probe-file world)
          (error "Game smoke did not save its temporary world.")))))
  (format t "Game startup, companion assets, frames, and save/reload passed: ~A~%"
          (truename pathname)))

(defun run-smoke-test (pathname &optional provider)
  (format t "Rendering ~A~%" pathname)
  (run-game-smoke-test pathname provider))

(defun dispatch (arguments)
  (cond
    ((and (= (length arguments) 1)
          (member (first arguments) '("--help" "-h") :test #'string=))
     (usage))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--game-smoke-test"))
     (run-game-smoke-test (pathname (second arguments))))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--smoke-test"))
     (run-smoke-test (pathname (second arguments))))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--metal-smoke-test"))
     (run-smoke-test
      (pathname (second arguments))
      (make-metal-provider)))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--vulkan-smoke-test"))
     (run-smoke-test
      (pathname (second arguments))
      (make-vulkan-provider)))
    (t
     (multiple-value-bind
           (provider world-pathname tracy-p fullscreen-p interactive-p)
         (parse-interactive-options arguments)
       (if interactive-p
           (run-interactive :provider provider :tracy-p tracy-p
                            :fullscreen-p fullscreen-p
                            :world-pathname world-pathname)
           (progn
             (usage *error-output*)
             (error "Invalid luvcraft arguments: ~{~A~^ ~}" arguments)))))))

(defun main ()
  (handler-case
      (call-with-slynk
       (lambda ()
         (luv:call-with-sdl-main-thread
          (lambda ()
            (dispatch (uiop:command-line-arguments))))
         (finish-output *standard-output*)
         (finish-output *error-output*)))
    (error (condition)
      (format *error-output* "luvcraft: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
