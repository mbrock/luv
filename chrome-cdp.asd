(in-package #:asdf-user)

(defsystem "chrome-cdp"
  :description "A live Common Lisp workbench for the Chrome DevTools Protocol."
  :long-description
  "A persistent Chrome debugging connection with inspectable browser, target,
and session objects.  It discovers Chrome through DevToolsActivePort, carries
arbitrary CDP commands, evaluates JavaScript, and retains unsolicited events
for interactive debugging."
  :version "0.0.1"
  :author "Mikael Brockman"
  :depends-on ("cl-json" "uiop" "websocket-driver-client")
  :serial t
  :components ((:module "chrome-cdp"
                :serial t
                :components ((:file "package")
                             (:file "client")
                             (:file "workbench"))))
  :in-order-to ((test-op (test-op "chrome-cdp/test"))))

(defsystem "chrome-cdp/test"
  :description "Tests for the Chrome DevTools Protocol workbench."
  :depends-on ("chrome-cdp" "rove")
  :serial t
  :components ((:module "chrome-cdp/tests"
                :serial t
                :components ((:file "package")
                             (:file "client"))))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call '#:rove '#:run-suite
                                       (uiop:symbol-call '#:rove '#:find-suite
                                                         '#:chrome-cdp.tests))
               (error "Chrome CDP tests failed"))))
