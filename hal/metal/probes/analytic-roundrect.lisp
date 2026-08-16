;;;; Cold-process render proof for the analytic roundrect family shader.

(require :asdf)

(asdf:load-asd (truename (merge-pathnames #P"../../../luv.asd" *load-truename*)))
(asdf:load-system :luv)

(let ((pathname
        (or (first (uiop:command-line-arguments))
            "build/analytic-roundrect-proof.png")))
  (handler-case
      (multiple-value-bind (pixels width height format)
          (luv:render-metal-analytic-roundrect-proof
           (make-instance 'luv:metal-gpu-provider))
        (ensure-directories-exist pathname)
        (luv:write-rgba-png pathname pixels width height format)
        (format t "~A ~Dx~D ~S~%" (truename pathname) width height format)
        (finish-output))
    (error (condition)
      (format *error-output* "Analytic roundrect proof failed: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
