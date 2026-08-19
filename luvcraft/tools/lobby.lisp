;;;; luv lobby: the tailnet value store from the shell.
;;;;
;;;; `luv lobby put OPENAI_API_KEY` reads the value from stdin so secrets stay
;;;; out of shell history; `luv lobby get KEY` prints it for $(...) use.

(in-package #:luvcraft.tools)

(defun read-stdin-value ()
  "Everything on stdin, minus one trailing newline."
  (let ((text (with-output-to-string (out)
                (loop for line = (read-line *standard-input* nil)
                      for first = t then nil
                      while line
                      do (unless first (terpri out))
                         (write-string line out)))))
    text))

(defun command-lobby (arguments)
  (let ((verb (first arguments))
        (rest (rest arguments)))
    (flet ((one-key ()
             (unless (= 1 (length rest))
               (command-line-error "lobby ~A expects exactly one KEY." verb))
             (first rest)))
      (cond
        ((null verb)
         (command-line-error "lobby expects get|put|rm|ls|host."))
        ((string= verb "host")
         (format t "~A:~D~%" (mqtt.net:lobby-host) mqtt.net:*lobby-port*))
        ((string= verb "ls")
         (loop for (key . value) in (sort (mqtt.net:lobby-keys) #'string< :key #'car)
               do (format t "~A~30T~D byte~:P~%" key (length value))))
        ((string= verb "get")
         (let ((value (mqtt.net:lobby-get (one-key) :default :absent)))
           (when (eq value :absent)
             (format *error-output* "luv: no value stored under ~A.~%" (first rest))
             (uiop:quit 1))
           (write-string value)
           (terpri)))
        ((string= verb "put")
         (unless (<= 1 (length rest) 2)
           (command-line-error "lobby put expects KEY [VALUE]; without VALUE it reads stdin."))
         (let ((value (if (second rest) (second rest) (read-stdin-value))))
           (when (zerop (length value))
             (command-line-error "Refusing to store an empty value; use lobby rm to delete."))
           (mqtt.net:lobby-put (first rest) value)
           (format t "stored ~A (~D byte~:P) at ~A~%" (first rest) (length value)
                   (mqtt.net:lobby-host))))
        ((string= verb "rm")
         (mqtt.net:lobby-delete (one-key))
         (format t "deleted ~A~%" (first rest)))
        (t (command-line-error "Unknown lobby verb ~A; try get|put|rm|ls|host." verb))))))
