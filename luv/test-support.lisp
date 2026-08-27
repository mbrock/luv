(in-package #:cl-user)

(defpackage #:luv.test-support
  (:use #:cl)
  (:export #:format-seconds
           #:test-package))

(in-package #:luv.test-support)

(defparameter *quiet-seconds* 0.05)

(defun format-seconds (seconds)
  "Format a test duration like the build progress display."
  (cond ((< seconds *quiet-seconds*) "0s")
        ((< seconds 10) (format nil "~,1Fs" seconds))
        ((< seconds 600) (format nil "~Ds" (round seconds)))
        (t (multiple-value-bind (minutes secs) (floor (round seconds) 60)
             (format nil "~D:~2,'0D" minutes secs)))))

(defun test-package (package)
  "Run PACKAGE's Parachute tests, signalling when any test fails."
  (let ((report (parachute:test package :report 'parachute:summary)))
    (unless (eq :passed (parachute:status report))
      (error "Tests in ~A failed" package))
    report))
