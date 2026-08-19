;;;; Standalone LUFT atelier entry point.

(in-package #:luft.render)

(defun main ()
  "Open the atelier and keep the process alive until its canvas closes."
  (let ((viewer (start-viewer)))
    (unwind-protect
         (loop while (viewer-running-p viewer)
               do (sleep 0.05))
      (stop-viewer viewer))))
