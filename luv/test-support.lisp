(in-package #:cl-user)

(defpackage #:luv.test-support
  (:use #:cl)
  (:export #:format-seconds
           #:suite-failed
           #:suite-failed-package
           #:suite-failed-report
           #:test-package))

(in-package #:luv.test-support)

(defparameter *quiet-seconds* 0.05)

(define-condition suite-failed (error)
  ((package :initarg :package :reader suite-failed-package)
   (report :initarg :report :reader suite-failed-report))
  (:report (lambda (condition stream)
             (format stream "Tests in ~A failed"
                     (suite-failed-package condition)))))

(defun format-seconds (seconds)
  "Format a test duration like the build progress display."
  (cond ((< seconds *quiet-seconds*) "0s")
        ((< seconds 10) (format nil "~,1Fs" seconds))
        ((< seconds 600) (format nil "~Ds" (round seconds)))
        (t (multiple-value-bind (minutes secs) (floor (round seconds) 60)
             (format nil "~D:~2,'0D" minutes secs)))))

(defun test-package (package)
  "Run PACKAGE's Parachute tests, signalling when any test fails."
  (unless (parachute:package-tests package)
    (error "No Parachute tests are registered in ~A" package))
  (let ((report (parachute:test package :report 'parachute:summary)))
    (unless (eq :passed (parachute:status report))
      (error 'suite-failed :package package :report report))
    report))
