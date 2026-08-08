(defpackage #:mcluv
  (:use #:cl)
  (:export #:main))

(in-package #:mcluv)

(defun main ()
  "Run the McCLIM Listener on luv until its frame exits."
  (multiple-value-bind (process frame)
      (luv.mcclim:open-listener)
    (declare (ignore frame))
    (clim-sys:join-process process))
  (values))
