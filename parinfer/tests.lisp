(in-package #:luv/tests)

(deftest parinfer-treats-escaped-symbol-syntax-as-reader-syntax
  (dolist (source '("(defparameter |foo(bar| 1)\n"
                    "(defparameter foo\\(bar 1)\n"
                    "(defparameter |foo\\|bar(baz| 1)\n"))
    (let ((report (sly-client/parinfer:analyze-indent-mode source)))
      (ok (sly-client/parinfer:indent-mode-report-source-balanced-p report))
      (ok (string= source
                   (sly-client/parinfer:indent-mode-report-candidate report)))
      (ok (string= source (sly-client/parinfer:apply-indent-mode source))))))

(deftest parinfer-does-not-certify-an-unterminated-bar-symbol
  (let* ((source "(defparameter |foo(bar 1)\n")
         (report (sly-client/parinfer:analyze-indent-mode source)))
    (ok (not (sly-client/parinfer:indent-mode-report-source-balanced-p report)))
    (ok (not (sly-client/parinfer:indent-mode-report-candidate-balanced-p
              report)))
    (ok (string= source (sly-client/parinfer:apply-indent-mode source)))))

(deftest parinfer-still-repairs-an-ordinary-missing-close
  (let* ((source "(defun probe ()\n  (+ 1 2)\n")
         (candidate (sly-client/parinfer:apply-indent-mode source))
         (report (sly-client/parinfer:analyze-indent-mode candidate)))
    (ok (not (string= source candidate)))
    (ok (sly-client/parinfer:indent-mode-report-source-balanced-p report))))
