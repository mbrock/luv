;;;; The standalone Luvland executable.

(in-package #:luvland)

(defun run-standalone-luvland ()
  "Serve Wayland clients until Luvland's window is closed."
  (let ((luvland (start-luvland)))
    (unwind-protect
         (loop while (luvland-running-p luvland) do (sleep 0.05))
      (stop-luvland luvland))))

(defun main ()
  "Run Luvland on the SDL main-thread host."
  (luv:call-with-sdl-main-thread #'run-standalone-luvland))
