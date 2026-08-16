(defpackage #:luv.terminal
  (:use #:cl)
  (:local-nicknames (#:ghostty #:luv.ghostty))
  (:export #:pty-device
           #:open-pty-device
           #:pty-device-terminal
           #:pty-device-state
           #:pty-device-exit-code
           #:pty-device-condition
           #:send-pty-device-bytes
           #:send-pty-device-text
           #:send-pty-device-key
           #:send-pty-device-canvas-key-event
           #:resize-pty-device
           #:call-with-pty-device-terminal
           #:wait-for-pty-device
           #:close-pty-device))
