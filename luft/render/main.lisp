;;;; Standalone LUFT atelier entry point.

(in-package #:luft.render)

(defun run-standalone-viewer ()
  (let ((viewer (start-viewer)))
    (unwind-protect
         (loop while (viewer-running-p viewer)
               do (sleep 0.05))
      (stop-viewer viewer))))

(defun main ()
  "Open the atelier and keep the process alive until its canvas closes."
  (luv:call-with-sdl-main-thread #'run-standalone-viewer))
