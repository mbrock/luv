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
           #:register-luv-reporter))
(in-package #:luv.test-reporter)

(defclass luv-reporter (rove/reporter:reporter) ())

(defmethod rove/core/stats:summarize ((reporter luv-reporter))
  (let ((passed (passed-tests reporter))
        (failed (failed-tests reporter))
        (pending (pending-tests reporter)))
    (if (and (null failed) (null pending))
        (format (reporter-stream reporter) "~D test~:P passed.~%"
                (length passed))
        (format-failure-tests (reporter-stream reporter)
                              passed failed pending))))

(defun register-luv-reporter ()
  ;; The repository historically named both :LUV and :SPEC explicitly in its
  ;; ASDF hooks.  During the aggregate test run they should share the same
  ;; failure-focused output; callers outside it retain Rove's stock registry.
  (add-reporter :luv 'luv-reporter)
  (add-reporter :spec 'luv-reporter))
