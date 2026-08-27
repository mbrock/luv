(defpackage #:luv.ghostty.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:false #:fail #:group #:skip)
  (:local-nicknames (#:ghostty #:luv.ghostty)))

(in-package #:luv.ghostty.tests)

(define-test vt-input-round-trips-through-plain-formatter
  (ghostty:with-terminal (terminal :columns 24 :rows 4)
    (ghostty:write-terminal
     terminal
     (format nil "Hello, ~C[1;32mGhostty~C[0m!~C~CSecond line"
             (code-char 27) (code-char 27) #\Return #\Newline))
    (true (equal (format nil "Hello, Ghostty!~%Second line")
                 (ghostty:terminal-text terminal)))))

(define-test terminal-ownership-is-explicit-and-idempotent
  (let ((terminal (ghostty:make-terminal)))
    (true (ghostty:terminal-open-p terminal))
    (ghostty:close-terminal terminal)
    (true (not (ghostty:terminal-open-p terminal)))
    (true (eq terminal (ghostty:close-terminal terminal)))
    (fail (ghostty:write-terminal terminal "closed") 'error)))

(define-test key-encoding-follows-live-terminal-modes
  (ghostty:with-terminal (terminal)
    (ghostty:with-key-encoder (encoder)
      (true (equal '(27 91 65)
                   (coerce
                    (ghostty:encode-key-event
                     encoder terminal :press :arrow-up)
                    'list)))
      (ghostty:write-terminal terminal (format nil "~C[?1h" (code-char 27)))
      (true (equal '(27 79 65)
                   (coerce
                    (ghostty:encode-key-event
                     encoder terminal :press :arrow-up)
                    'list)))
      (true (equal '(1)
                   (coerce
                    (ghostty:encode-key-event
                     encoder terminal :press :a
                     :modifiers '(:control)
                     :text "a"
                     :unshifted-codepoint (char-code #\a))
                    'list))))))
