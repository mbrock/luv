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
           #:terminal-text
           #:with-terminal))
