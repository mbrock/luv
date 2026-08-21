# Chrome CDP workbench

`chrome-cdp` gives the durable luv Lisp image one persistent connection to a
Chrome remote-debugging session. Chrome remains the owner of the DevTools
Protocol vocabulary; the Lisp side supplies inspectable browser, target,
session, and event objects around arbitrary CDP calls.

Chrome writes its rendezvous address to `DevToolsActivePort` when remote
debugging is enabled. On macOS the default is the regular Google Chrome
profile; set `CHROME_DEVTOOLS_ACTIVE_PORT_FILE` or pass `:active-port-file`
when using another profile.

```lisp
(asdf:load-system "chrome-cdp")

;; Retained in CHROME-CDP:*CHROME* for this Lisp image. Chrome may ask once
;; whether to allow the connection.
(chrome-cdp:ensure-chrome)

(chrome-cdp:chrome-pages)

(defparameter *page*
  (chrome-cdp:chrome-session :title "LUFT WebGPU"))

(chrome-cdp:evaluate-javascript *page* "document.title")
(chrome-cdp:evaluate-javascript *page* "Promise.resolve(42)")
```

The complete externally owned protocol remains available without a mirrored
Lisp API:

```lisp
(chrome-cdp:session-call *page* "Runtime.enable")

(chrome-cdp:evaluate-javascript
 *page* "console.log('hello from the durable Lisp image')")

(chrome-cdp:next-session-event
 *page* :method "Runtime.consoleAPICalled")

(chrome-cdp:session-call
 *page* "Performance.getMetrics")
```

Use `browser-call` for browser-level methods and `session-call` for methods in
an attached page, worker, or other target. Both accept ordinary CL-JSON alists
as parameters. `evaluate-javascript` returns the by-value result first and the
complete `Runtime.RemoteObject` as a second value.

Keep a browser connection alive when doing several operations: reconnecting is
both needless work and may repeat Chrome's approval dialog. Detach a page with
`detach-session`; `close-chrome` begins a normal WebSocket close handshake and
forgets the workbench connection.
