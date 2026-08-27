(in-package #:luv.tests)

(define-test parinfer-treats-escaped-symbol-syntax-as-reader-syntax
  (dolist (source '("(defparameter |foo(bar| 1)\n"
                    "(defparameter foo\\(bar 1)\n"
                    "(defparameter |foo\\|bar(baz| 1)\n"))
    (let ((report (sly-client.parinfer:analyze-indent-mode source)))
      (true (sly-client.parinfer:indent-mode-report-source-balanced-p report))
      (true (string= source
                     (sly-client.parinfer:indent-mode-report-candidate report)))
      (true (string= source (sly-client.parinfer:apply-indent-mode source))))))

(define-test parinfer-does-not-certify-an-unterminated-bar-symbol
  (let* ((source "(defparameter |foo(bar 1)\n")
         (report (sly-client.parinfer:analyze-indent-mode source)))
    (true (not (sly-client.parinfer:indent-mode-report-source-balanced-p report)))
    (true (not (sly-client.parinfer:indent-mode-report-candidate-balanced-p
                report)))
    (true (string= source (sly-client.parinfer:apply-indent-mode source)))))

(define-test parinfer-still-repairs-an-ordinary-missing-close
  (let* ((source "(defun probe ()\n  (+ 1 2)\n")
         (candidate (sly-client.parinfer:apply-indent-mode source))
         (report (sly-client.parinfer:analyze-indent-mode candidate)))
    (true (not (string= source candidate)))
    (true (sly-client.parinfer:indent-mode-report-source-balanced-p report))))
