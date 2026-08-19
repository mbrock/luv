;;;; Standalone LUFT atelier entry point.

(in-package #:luft.render)

(defun main ()
  "Open the atelier and keep the process alive until its canvas closes."
  (multiple-value-bind (mode style pipeline-styles effects technique)
      (standalone-render-options)
    (log-event :luft "standalone render mode ~(~A~) by ~(~A~) technique"
               mode technique)
    (let ((viewer (start-viewer :technique technique
                                :style style
                                :pipeline-styles pipeline-styles
                                :effects effects)))
      (unwind-protect
           (loop while (viewer-running-p viewer)
                 do (sleep 0.05))
        (stop-viewer viewer)))))
