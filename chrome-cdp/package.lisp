;;;; Chrome owns the CDP vocabulary.  This package owns durable, inspectable
;;;; local handles for a browser connection, its targets, and attached sessions.

(defpackage #:chrome-cdp
  (:use #:cl)
  (:documentation
   "A live workbench for Chrome's remote debugging protocol.

CONNECT-CHROME discovers a debugging-enabled Chrome through its
DevToolsActivePort file.  BROWSER-CALL and SESSION-CALL expose the complete
CDP without mirroring its externally owned method vocabulary in Lisp.
Targets and sessions are inspectable CLOS objects suitable for a durable SLY
image; protocol payloads remain ordinary CL-JSON alists.")
  (:export
   ;; Conditions.
   #:cdp-error #:cdp-error-detail
   #:chrome-not-debuggable #:chrome-active-port-file
   #:browser-closed #:browser-closed-browser
   #:cdp-timeout #:cdp-timeout-method #:cdp-timeout-seconds
   #:cdp-remote-error #:cdp-error-method #:cdp-error-code #:cdp-error-data
   #:javascript-error #:javascript-error-details #:javascript-error-expression
   #:target-not-found #:target-query

   ;; Connection and discovery.
   #:browser #:browser-url #:browser-open-p #:browser-default-timeout
   #:default-chrome-active-port-file #:chrome-debugging-websocket-url
   #:connect-chrome #:close-browser

   ;; Targets and attached sessions.
   #:target #:target-browser #:target-id #:target-type #:target-title #:target-url
   #:session #:session-browser #:session-target #:session-id
   #:browser-targets #:find-target #:find-page #:create-page #:close-page
   #:attach-target #:detach-session

   ;; Protocol and JavaScript.
   #:browser-call #:session-call #:evaluate-javascript #:navigate
   #:+json-false+

   ;; Asynchronous protocol events.
   #:event #:event-method #:event-params #:event-session-id
   #:next-event #:next-session-event #:drain-events

   ;; Durable-image conveniences.
   #:*chrome* #:ensure-chrome #:close-chrome #:chrome-pages
   #:chrome-page #:chrome-session))
