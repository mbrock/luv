(in-package #:luft.render)

(defun main ()
  "Run the standalone atelier on the SDL/Cocoa main-thread host."
  (call-with-sdl-main-thread #'run-standalone-viewer))
