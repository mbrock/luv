(defpackage #:luv.ghostty
  (:nicknames #:ghostty)
  (:use #:cl)
  (:documentation "Explicitly owned Common Lisp access to libghostty-vt.")
  (:export #:load-libghostty-vt
           #:libghostty-vt-loaded-p
           #:ghostty-error
           #:ghostty-error-operation
           #:ghostty-error-result
           #:terminal
           #:make-terminal
           #:terminal-open-p
           #:close-terminal
           #:write-terminal
           #:write-terminal-bytes
           #:resize-terminal
           #:terminal-size
           #:set-terminal-response-function
           #:terminal-callback-condition
           #:terminal-text
           #:with-terminal
           #:key-encoder
           #:make-key-encoder
           #:key-encoder-open-p
           #:close-key-encoder
           #:encode-key-event
           #:with-key-encoder))
