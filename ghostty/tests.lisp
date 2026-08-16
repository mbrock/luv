(defpackage #:luv.ghostty.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:ghostty #:luv.ghostty)))

(in-package #:luv.ghostty.tests)

(deftest vt-input-round-trips-through-plain-formatter
  (ghostty:with-terminal (terminal :columns 24 :rows 4)
    (ghostty:write-terminal
     terminal
     (format nil "Hello, ~C[1;32mGhostty~C[0m!~C~CSecond line"
             (code-char 27) (code-char 27) #\Return #\Newline))
    (ok (equal (format nil "Hello, Ghostty!~%Second line")
               (ghostty:terminal-text terminal)))))

(deftest terminal-ownership-is-explicit-and-idempotent
  (let ((terminal (ghostty:make-terminal)))
    (ok (ghostty:terminal-open-p terminal))
    (ghostty:close-terminal terminal)
    (ok (not (ghostty:terminal-open-p terminal)))
    (ok (eq terminal (ghostty:close-terminal terminal)))
    (ok (signals (ghostty:write-terminal terminal "closed") 'error))))
