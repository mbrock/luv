(in-package #:chrome-cdp.tests)

(defun fake-browser ()
  (make-instance 'chrome-cdp:browser :url "ws://test" :default-timeout 0.1))

(define-test active-port-discovery
  (let ((path (merge-pathnames
               (format nil "chrome-cdp-active-port-~A" (gensym))
               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (stream path :direction :output
                                        :if-exists :error
                                        :if-does-not-exist :create)
             (write-line "9222" stream)
             (write-line "/devtools/browser/example" stream))
           (true (string= "ws://127.0.0.1:9222/devtools/browser/example"
                          (chrome-cdp:chrome-debugging-websocket-url
                           :active-port-file path))))
      (when (probe-file path) (delete-file path)))))

(define-test top-level-responses-are-routed-by-id
  (let ((browser (fake-browser)))
    (chrome-cdp::receive-message
     browser "{\"id\":7,\"result\":{\"answer\":42}}")
    (let ((reply (chrome-cdp::wait-for-response browser 7 "Probe.answer" 0.1)))
      (true (= 42 (chrome-cdp::json-value
                   (chrome-cdp::json-value reply "result") "answer"))))))

(define-test target-session-responses-and-events-are-separated
  (let ((browser (fake-browser)))
    (chrome-cdp::receive-message
     browser
     "{\"method\":\"Target.receivedMessageFromTarget\",\"params\":{\"sessionId\":\"s1\",\"message\":\"{\\\"id\\\":9,\\\"result\\\":{\\\"value\\\":3}}\"}}")
    (let ((reply (chrome-cdp::wait-for-response
                  browser '("s1" 9) "Runtime.evaluate" 0.1)))
      (true (= 3 (chrome-cdp::json-value
                  (chrome-cdp::json-value reply "result") "value"))))
    (chrome-cdp::receive-message
     browser
     "{\"method\":\"Target.receivedMessageFromTarget\",\"params\":{\"sessionId\":\"s1\",\"message\":\"{\\\"method\\\":\\\"Page.loadEventFired\\\",\\\"params\\\":{\\\"timestamp\\\":12}}\"}}")
    (let ((event (chrome-cdp:next-event browser :timeout 0.1)))
      (true (string= "Page.loadEventFired" (chrome-cdp:event-method event)))
      (true (string= "s1" (chrome-cdp:event-session-id event)))
      (true (= 12 (chrome-cdp::json-value
                   (chrome-cdp:event-params event) "timestamp"))))))

(define-test remote-errors-retain-the-cdp-evidence
  (handler-case
      (progn
        (chrome-cdp::response-result
         '((:error (:code . -32601) (:message . "Method not found")
                   (:data . "Probe.nope")))
         "Probe.nope")
        (false t "Expected a CDP-REMOTE-ERROR."))
    (chrome-cdp:cdp-remote-error (condition)
      (true (= -32601 (chrome-cdp:cdp-error-code condition)))
      (true (string= "Probe.nope" (chrome-cdp:cdp-error-method condition)))
      (true (string= "Probe.nope" (chrome-cdp:cdp-error-data condition))))))

(define-test target-queries-are-case-insensitive-substrings
  (let* ((browser (fake-browser))
         (target (chrome-cdp::target-from-info
                  browser
                  '((:target-id . "t1") (:type . "page")
                    (:title . "LUFT WebGPU Workbench")
                    (:url . "https://example.test/luft")))))
    (true (chrome-cdp::target-matches-p target "page" "webgpu" "/LUFT" nil))
    (true (not (chrome-cdp::target-matches-p target "worker" nil nil nil)))))

(define-test javascript-exceptions-prefer-the-remote-description
  (true (string= "ReferenceError: nope is not defined"
                 (chrome-cdp::javascript-exception-detail
                  '((:text . "Uncaught")
                    (:exception (:description
                                 . "ReferenceError: nope is not defined")))))))
