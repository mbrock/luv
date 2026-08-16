(defpackage #:luv.terminal.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:ghostty #:luv.ghostty)
                    (#:terminal #:luv.terminal)))

(in-package #:luv.terminal.tests)

(defmacro with-pty-test ((device terminal &rest open-arguments) &body body)
  `(ghostty:with-terminal (,terminal :columns 40 :rows 6)
     (let ((,device nil))
       (unwind-protect
            (progn
              (setf ,device
                    (terminal:open-pty-device ,terminal ,@open-arguments))
              ,@body)
         (when ,device (terminal:close-pty-device ,device))))))

(defun pty-terminal-text (device)
  (terminal:call-with-pty-device-terminal device #'ghostty:terminal-text))

(defun send-canvas-key (device key &optional character)
  (terminal:send-pty-device-canvas-key-event
   device
   (make-instance
    'luv:canvas-key-press-event :timestamp 0 :key-name key
    :character character :unshifted-character character)))

(defun wait-for-terminal-text (device needle &key (timeout 1.0))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop for text = (pty-terminal-text device)
          when (search needle text) do (return text)
          when (>= (get-internal-real-time) deadline) do (return nil)
          do (sleep 0.01))))

(deftest pty-child-output-and-input-drive-one-terminal
  (with-pty-test
      (device ghostty-terminal
       :program "/bin/sh"
       :arguments
       (list "-c"
             "printf 'term:%s\\r\\nready\\r\\n' \"$TERM\"; IFS= read -r line; printf 'got:%s\\r\\n' \"$line\""))
    (terminal:send-pty-device-text device (format nil "hello~%"))
    (ok (eq :exited (terminal:wait-for-pty-device device :timeout 3.0)))
    (let ((text (pty-terminal-text device)))
      (ok (search "ready" text))
      (ok (search "term:xterm-256color" text))
      (ok (search "got:hello" text)))
    (ok (null (terminal:pty-device-condition device)))))

(deftest pty-echo-reaches-the-terminal-before-enter
  (with-pty-test
      (device ghostty-terminal
       :program "/bin/sh"
       :arguments (list "-c" "IFS= read -r line"))
    (terminal:send-pty-device-text device "visible-before-enter")
    (ok (search "visible-before-enter"
                (wait-for-terminal-text device "visible-before-enter")))
    (ok (eq :running (terminal:pty-device-state device)))))

(deftest ghostty-query-responses-return-through-the-pty
  (let* ((python "python3")
         (script
           (format nil
                   "import os, termios, tty~%old = termios.tcgetattr(0)~%tty.setraw(0)~%os.write(1, b'\\x1b[6n')~%reply = b''~%while not reply.endswith(b'R'):~%    reply += os.read(0, 1)~%termios.tcsetattr(0, termios.TCSANOW, old)~%os.write(1, b'query-ok:' + reply.hex().encode() + b'\\r\\n')~%")))
    (with-pty-test
        (device ghostty-terminal
         :program python :arguments (list "-c" script))
      (ok (eq :exited (terminal:wait-for-pty-device device :timeout 3.0)))
      (ok (search "query-ok:1b5b313b3152" (pty-terminal-text device)))
      (ok (null (terminal:pty-device-condition device))))))

(deftest portable-canvas-keys-reach-the-pty-child
  (with-pty-test
      (device ghostty-terminal
       :program "/bin/sh"
       :arguments
       (list "-c"
             "printf 'key-ready\r\n'; IFS= read -r line; printf 'key:%s\r\n' \"$line\""))
    (send-canvas-key device :h #\h)
    (send-canvas-key device :i #\i)
    (send-canvas-key device :return #\Return)
    (ok (eq :exited (terminal:wait-for-pty-device device :timeout 3.0)))
    (let ((text (pty-terminal-text device)))
      (ok (search "key-ready" text))
      (ok (search "key:hi" text)))
    (ok (null (terminal:pty-device-condition device)))))

(deftest resize-coordinates-the-kernel-pty-and-ghostty-grid
  (with-pty-test
      (device ghostty-terminal
       :program "/bin/sh"
       :arguments (list "-c" "IFS= read -r line; stty size"))
    (terminal:resize-pty-device device 42 9
                                :cell-width-pixels 8
                                :cell-height-pixels 16)
    (terminal:send-pty-device-text device (format nil "size~%"))
    (ok (eq :exited (terminal:wait-for-pty-device device :timeout 3.0)))
    (multiple-value-bind (columns rows)
        (terminal:call-with-pty-device-terminal
         device #'ghostty:terminal-size)
      (ok (= columns 42))
      (ok (= rows 9)))
    (ok (search "9 42" (pty-terminal-text device)))
    (ok (null (terminal:pty-device-condition device)))))

(deftest closing-a-running-device-releases-only-its-pty
  (with-pty-test
      (device ghostty-terminal
       :program "/bin/sh"
       :arguments (list "-c" "printf 'waiting\\r\\n'; sleep 30"))
    (ok (eq device (terminal:close-pty-device device)))
    (ok (eq :closed (terminal:pty-device-state device)))
    (ok (not (sb-thread:thread-alive-p
              (luv.terminal::pty-device-thread device))))
    ;; The device owns the child and descriptor, not the semantic terminal.
    (ok (ghostty:terminal-open-p ghostty-terminal))
    (ok (eq ghostty-terminal
            (ghostty:write-terminal ghostty-terminal "still owned outside")))
    (ok (eq device (terminal:close-pty-device device)))))
