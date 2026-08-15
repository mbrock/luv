;;;; Cold-process acceptance probe for the SDL/Cocoa Metal 4 clear.

(require :asdf)

(asdf:load-asd (truename (merge-pathnames #P"../luv.asd" *load-truename*)))

(asdf:load-system :luv/canvas/metal)

(handler-case
    (progn
      (format t "~S~%"
              (luv:call-with-sdl-main-thread
               #'luv:probe-sdl-metal-clear))
      (finish-output))
  (error (condition)
    (format *error-output* "Metal clear probe failed: ~A~%" condition)
    (finish-output *error-output*)
    (uiop:quit 1)))
