;;;; Cold-process GPU render proof for a real TrueType glyph outline.

(require :asdf)

(asdf:load-asd (truename (merge-pathnames #P"../../../luv.asd" *load-truename*)))
(asdf:load-system :luv)
(asdf:load-system :cl-dejavu)

(let ((pathname
        (or (first (uiop:command-line-arguments))
            "build/slug-glyph-proof.png")))
  (handler-case
      (zpb-ttf:with-font-loader
          (font-loader (cl-dejavu:font-pathname "DejaVuSans.ttf"))
        (multiple-value-bind (pixels width height format)
            (luv:render-metal-slug-glyph
             (make-instance 'luv:metal-gpu-provider) #\O font-loader)
          (ensure-directories-exist pathname)
          (luv:write-rgba-png pathname pixels width height format)
          (format t "~A ~Dx~D ~S DejaVuSans O~%"
                  (truename pathname) width height format)
          (finish-output)))
    (error (condition)
      (format *error-output* "Slug glyph proof failed: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
