;;;; Cold-process HarfBuzz shaping and multi-glyph Slug render proof.

(require :asdf)

(asdf:load-asd (truename (merge-pathnames #P"../../../luv.asd" *load-truename*)))
(asdf:load-system :luv)
(asdf:load-system :cl-dejavu)

(let ((pathname
        (or (first (uiop:command-line-arguments))
            "build/slug-text-proof.png"))
      (text "office affinity"))
  (handler-case
      (let ((font (cl-dejavu:font-pathname "DejaVuSans.ttf")))
        (multiple-value-bind (pixels width height format shaped)
            (luv:render-metal-slug-text
             (make-instance 'luv:metal-gpu-provider) text font)
          (ensure-directories-exist pathname)
          (luv:write-rgba-png pathname pixels width height format)
          (format t "~A ~Dx~D ~S ~S -> ~D glyphs~%"
                  (truename pathname) width height format text
                  (length (luv.slug:slug-shaped-text-glyphs shaped)))
          (finish-output)))
    (error (condition)
      (format *error-output* "Slug text proof failed: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
