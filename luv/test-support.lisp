(in-package #:cl-user)

(defpackage #:luv.test-support
  (:use #:cl)
  (:export #:format-seconds
           #:call-with-test-shard
           #:shardable-test-package-p
           #:suite-failed
           #:suite-failed-package
           #:suite-failed-report
           #:test-root-count
           #:test-package))

(in-package #:luv.test-support)

(defparameter *quiet-seconds* 0.05)
(defvar *test-shard* nil)

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

(defun test-roots (package)
  (remove-if #'parachute:parent
             (parachute:package-tests (find-package package))))

(defun test-root-count (package)
  (length (test-roots package)))

(defun shardable-test-package-p (package)
  "Whether PACKAGE is a flat set of independent Parachute roots."
  (let ((tests (parachute:package-tests (find-package package))))
    (and tests
         (every (lambda (test)
                  (and (null (parachute:parent test))
                       (null (rest (parachute:dependencies test)))))
                tests))))

(defun call-with-test-shard (package index count thunk)
  "Run THUNK with PACKAGE restricted to one stable root-test partition."
  (let ((*test-shard* (list (find-package package) index count)))
    (funcall thunk)))

(defun selected-package-tests (package)
  (if *test-shard*
      (destructuring-bind (shard-package index count) *test-shard*
        (unless (eq package shard-package)
          (error "Expected sharded tests from ~A, not ~A"
                 (package-name shard-package) (package-name package)))
        (loop for test in (sort (test-roots package) #'string<
                                :key #'parachute:name)
              for position from 0
              when (= index (mod position count))
                collect test))
      package))

(defun test-package (package)
  "Run PACKAGE's Parachute tests, signalling when any test fails."
  (let ((package (find-package package)))
    (unless (parachute:package-tests package)
      (error "No Parachute tests are registered in ~A" package))
    (let ((report (parachute:test (selected-package-tests package)
                                  :report 'parachute:summary)))
      (unless (eq :passed (parachute:status report))
        (error 'suite-failed :package package :report report))
      report)))
