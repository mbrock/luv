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
           #:test-package
           #:xcrun-command
           #:metal-compiler-unavailable
           #:with-metal-compiler))

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

;;; Apple's offline Metal compiler

(defun xcrun-command (&rest arguments)
  "Apple's xcrun with ARGUMENTS.  Nix shells put xcbuild's xcrun first and
point DEVELOPER_DIR at a bare SDK; Metal's compiler lives in Xcode's."
  (if (probe-file "/usr/bin/xcrun")
      (list* "env" "-u" "DEVELOPER_DIR" "-u" "SDKROOT" "/usr/bin/xcrun"
             arguments)
      (list* "xcrun" arguments)))

(defvar *metal-compiler-unavailable* :unprobed)

(defun probe-metal-compiler ()
  "Compile an empty Metal 4 kernel; NIL on success, else why not."
  (uiop:with-temporary-file (:pathname source :type "metal" :keep nil
                             :stream stream :direction :output)
    (write-line "kernel void luv_probe() {}" stream)
    :close-stream
    (uiop:with-temporary-file (:pathname air :type "air" :keep nil)
      (multiple-value-bind (output error-output status)
          (ignore-errors
           (uiop:run-program
            (xcrun-command "-sdk" "macosx" "metal" "-std=metal4.0" "-c"
                           (uiop:native-namestring source)
                           "-o" (uiop:native-namestring air))
            :output :string :error-output :string :ignore-error-status t))
        (cond ((null status) "xcrun is not available")
              ((zerop status) nil)
              (t (let ((report (string-trim '(#\Space #\Tab #\Newline)
                                            (concatenate 'string
                                                         error-output output))))
                   ;; xcrun repeats the tool's own "error: " prefix.
                   (loop while (uiop:string-prefix-p "error: " report)
                         do (setf report (subseq report 7)))
                   (if (string= report "")
                       (format nil "xcrun metal exited with status ~D" status)
                       (subseq report 0 (position #\Newline report))))))))))

(defun metal-compiler-unavailable ()
  "NIL when Apple's offline Metal compiler compiles, else why it cannot.
Probes once per image, and says so once when tests will skip."
  (when (eq *metal-compiler-unavailable* :unprobed)
    (setf *metal-compiler-unavailable* (probe-metal-compiler))
    (when *metal-compiler-unavailable*
      (format t "~&Skipping offline Metal compilation: ~A.~%"
              *metal-compiler-unavailable*)))
  *metal-compiler-unavailable*)

(defmacro with-metal-compiler (&body body)
  "Run BODY's checks with Apple's offline Metal compiler, or record one
skipped result that gives the reason it is unavailable."
  ;; PARACHUTE:SKIP reports its forced checks as passed; this result is
  ;; counted under Skipped.
  (let ((reason (gensym "REASON")))
    `(let ((,reason (metal-compiler-unavailable)))
       (if ,reason
           (parachute:eval-in-context
            parachute:*context*
            (make-instance 'parachute:result
                           :expression 'with-metal-compiler
                           :status :skipped :description ,reason))
           (progn ,@body)))))

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
