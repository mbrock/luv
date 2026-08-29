;;;; Executable claims for the retained build model.

(defpackage #:luv.build.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:is)
  (:local-nicknames (#:build #:luv.build)))

(in-package #:luv.build.tests)

(define-condition retained-build-test-error (error) ())

(define-test retained-run-publishes-actions-and-rich-terminal-diagnostics
  (let* ((root (asdf:system-source-directory "luv"))
         (run (build:make-build-run root :system :fixture))
         (component
           (make-instance 'asdf:cl-source-file
                          :name "fixture"
                          :parent (make-instance 'asdf:system
                                                 :name "fixture-system"))))
    (handler-case
        (build:call-with-build-run
         run
         (lambda ()
           (build::call-with-build-action
            run :compile component
            (lambda () (error 'retained-build-test-error)))))
      (retained-build-test-error () nil))
    (let* ((snapshot (build:run-snapshot run))
           (action (first (build:run-snapshot-actions snapshot)))
           (diagnostic (first (build:run-snapshot-diagnostics snapshot))))
      (is equal "fixture-system/fixture"
          (build:build-action-snapshot-label action))
      (is eq :failed (build:build-action-snapshot-state action))
      (is eq :error (build:build-diagnostic-snapshot-severity diagnostic))
      (is eq 'retained-build-test-error
          (build:build-diagnostic-snapshot-condition-type diagnostic))
      (is = (build:build-action-snapshot-id action)
          (build:build-diagnostic-snapshot-action-id
           (build:run-snapshot-terminal-diagnostic snapshot)))
      (true (build:build-diagnostic-snapshot-backtrace diagnostic)))))

(define-test retained-terminal-failure-can-be-awaited
  (let ((run (build:make-build-run
              (asdf:system-source-directory "luv") :system :fixture)))
    (handler-case
        (build:call-with-build-run
         run (lambda () (error 'retained-build-test-error)))
      (retained-build-test-error () nil))
    (let ((snapshot (build:await-run run :timeout 0.1)))
      (is eq :failed (build:run-snapshot-state snapshot))
      (true (build:run-snapshot-terminal-diagnostic snapshot))
      (false (null (build:run-snapshot-diagnostics snapshot))))))

(define-test asdf-executor-completes-a-supplied-system-plan
  (let ((run (build:make-build-run
              (asdf:system-source-directory "luv") :system "luv/build")))
    (build:execute-run (make-instance 'build:asdf-build-executor) run)
    (let ((snapshot (build:run-snapshot run)))
      (is eq :succeeded (build:run-snapshot-state snapshot))
      (true (build:run-snapshot-plan-summary snapshot)))))

(define-test cancellation-is-cooperative-and-retained
  (let ((run (build:make-build-run
              (asdf:system-source-directory "luv") :system "luv/build")))
    (build:request-run-cancellation run)
    (build:execute-run
     (make-instance 'build:asdf-build-executor :signal-errors-p nil) run)
    (is eq :cancelled (build:run-state run))
    (true (build:run-snapshot-terminal-diagnostic
           (build:run-snapshot run)))))
