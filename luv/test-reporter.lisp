(in-package #:cl-user)

(defpackage #:luv.test-reporter
  (:use #:cl)
  (:import-from #:rove/reporter
                #:reporter-stream)
  (:import-from #:rove/reporter/dot
                #:dot-reporter)
  (:import-from #:rove/reporter/registry
                #:add-reporter)
  (:export #:luv-reporter
           #:register-luv-reporter))
(in-package #:luv.test-reporter)

(defclass luv-reporter (dot-reporter) ())

(defmethod rove/core/stats:suite-begin ((reporter luv-reporter) suite-name)
  (format (reporter-stream reporter) "  ~A~%" suite-name))

(defmethod rove/core/stats:test-begin ((reporter luv-reporter) description &optional count)
  (declare (ignore count))
  (when description
    (format (reporter-stream reporter) "    ~A " description)))

(defmethod rove/core/stats:test-finish ((reporter luv-reporter) description)
  (declare (ignore description))
  (fresh-line (reporter-stream reporter))
  (call-next-method))

(defun register-luv-reporter ()
  (add-reporter :luv 'luv-reporter))
