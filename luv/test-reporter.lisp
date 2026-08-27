(in-package #:cl-user)

(defpackage #:luv.test-reporter
  (:use #:cl)
  (:import-from #:rove/reporter
                #:reporter-stream)
  (:import-from #:rove/core/result
                #:passed-tests
                #:failed-tests
                #:pending-tests)
  (:import-from #:rove/reporter/registry
                #:add-reporter)
  (:import-from #:rove/utils/reporter
                #:format-failure-tests)
  (:export #:luv-reporter
           #:format-seconds
           #:register-luv-reporter))
(in-package #:luv.test-reporter)

(defparameter *quiet-seconds* 0.05)

(defun elapsed-seconds (since)
  (/ (float (- (get-internal-real-time) since) 1.0)
     internal-time-units-per-second))

(defun format-seconds (seconds)
  "Format a test duration like the build progress display."
  (cond ((< seconds *quiet-seconds*) "0s")
        ((< seconds 10) (format nil "~,1Fs" seconds))
        ((< seconds 600) (format nil "~Ds" (round seconds)))
        (t (multiple-value-bind (minutes secs) (floor (round seconds) 60)
             (format nil "~D:~2,'0D" minutes secs)))))

(defclass luv-reporter (rove/reporter:reporter)
  ((started-at :accessor reporter-started-at)
   (test-stack :initform nil :accessor reporter-test-stack)
   (test-timings :initform nil :accessor reporter-test-timings)))

(defmethod rove/core/stats:initialize :after ((reporter luv-reporter))
  (setf (reporter-started-at reporter) (get-internal-real-time)
        (reporter-test-stack reporter) nil
        (reporter-test-timings reporter) nil))

(defmethod rove/core/stats:test-begin
    ((reporter luv-reporter) description &optional count)
  (declare (ignore count))
  (push (cons description (get-internal-real-time))
        (reporter-test-stack reporter)))

(defmethod rove/core/stats:test-finish ((reporter luv-reporter) description)
  (declare (ignore description))
  (let ((timing (pop (reporter-test-stack reporter))))
    ;; Nested TESTING blocks are useful detail within a failure, but their
    ;; inclusive durations would duplicate the enclosing DEFTEST here.
    (when (null (reporter-test-stack reporter))
      (push (cons (car timing) (elapsed-seconds (cdr timing)))
            (reporter-test-timings reporter)))))

(defmethod rove/core/stats:summarize ((reporter luv-reporter))
  (let ((passed (passed-tests reporter))
        (failed (failed-tests reporter))
        (pending (pending-tests reporter))
        (seconds (elapsed-seconds (reporter-started-at reporter)))
        (slowest (first (sort (copy-list (reporter-test-timings reporter))
                              #'> :key #'cdr))))
    (if (and (null failed) (null pending))
        (format (reporter-stream reporter) "~D test~:P passed in ~A.~%"
                (length passed) (format-seconds seconds))
        (progn
          (format-failure-tests (reporter-stream reporter)
                                passed failed pending)
          (format (reporter-stream reporter) "Tests finished in ~A.~%"
                  (format-seconds seconds))))
    (when (and slowest (> (cdr slowest) *quiet-seconds*))
      (format (reporter-stream reporter) ";; Slowest was ~A at ~A.~%"
              (car slowest) (format-seconds (cdr slowest))))))

(defun register-luv-reporter ()
  ;; The repository historically named both :LUV and :SPEC explicitly in its
  ;; ASDF hooks.  During the aggregate test run they should share the same
  ;; failure-focused output; callers outside it retain Rove's stock registry.
  (add-reporter :luv 'luv-reporter)
  (add-reporter :spec 'luv-reporter))
