(in-package #:luvcraft.tools)

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: luv COMMAND [ARGS...]~%")
  (format stream "~%")
  (format stream "Commands:~%")
  (format stream "  block-world TARGET [--backend metal|vulkan] [--count N]~%")
  (format stream "              [--width N] [--height N] [--yaw-step R]~%")
  (format stream "  gazetteer TARGET-DIR [--backend metal|vulkan] [--view NAME]~%")
  (format stream "             [--width N] [--height N]~%")
  (format stream "             [--count N] [--forward-step R] [--yaw-step R]~%")
  (format stream "             [--day-start R] [--day-step R] [--difference-scale R]~%")
  (format stream "             [--shadow-only 1]~%")
  (format stream "  eval FORM [--package PACKAGE]~%")
  (format stream "  web [--host HOST] [--port PORT]~%")
  (format stream "  lobby get KEY | put KEY [VALUE] | rm KEY | ls | host~%")
  (format stream "        (the tailnet lobby's value store; put without VALUE reads stdin)~%")
  (format stream "  help~%"))

(define-condition command-line-error (error)
  ((message
    :initarg :message
    :reader command-line-error-message))
  (:report (lambda (condition stream)
             (format stream "~A" (command-line-error-message condition)))))

(defun command-line-error (control &rest arguments)
  (error 'command-line-error
         :message (apply #'format nil control arguments)))

(defun parse-integer-option (string name)
  (handler-case
      (let ((value (parse-integer string :junk-allowed nil)))
        (unless (plusp value)
          (command-line-error "~A must be positive, got ~D." name value))
        value)
    (parse-error ()
      (command-line-error "~A must be an integer, got ~S." name string))))

(defun parse-real-option (string name)
  (let ((*read-eval* nil))
    (handler-case
        (multiple-value-bind (value position)
            (read-from-string string)
          (unless (every (lambda (character)
                           (find character '(#\Space #\Tab #\Newline #\Return)))
                         (subseq string position))
            (command-line-error "~A must be a real number, got ~S." name string))
          (unless (realp value)
            (command-line-error "~A must be a real number, got ~S." name string))
          value)
      (error ()
        (command-line-error "~A must be a real number, got ~S." name string)))))

(defun parse-backend-option (string name)
  (cond
    ((string-equal string "vulkan") :vulkan)
    #+darwin
    ((string-equal string "metal") :metal)
    (t
     (command-line-error
      "~A must be ~:[vulkan~;metal or vulkan~], got ~S."
      name #+darwin t #-darwin nil string))))

(defun make-backend-provider (backend)
  (case backend
    ((nil) luv:*gpu-provider*)
    (:vulkan (make-instance 'luv:vulkan-gpu-provider))
    #+darwin
    (:metal (make-instance 'luv:metal-gpu-provider))
    (otherwise
     (command-line-error "Unsupported GPU backend ~S." backend))))

(defun option-value (arguments option)
  (or (first arguments)
      (command-line-error "~A requires a value." option)))

(defun parse-keyword-options (arguments specs)
  (let ((positionals '())
        (options '()))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--")
                (dolist (positional arguments)
                  (push positional positionals))
                (setf arguments nil))
               ((uiop:string-prefix-p "--" argument)
                (let* ((spec (assoc argument specs :test #'string=))
                       (key (or (second spec)
                                (command-line-error "Unknown option ~A." argument)))
                       (parser (or (third spec)
                                   (command-line-error "Unknown option ~A." argument)))
                       (value (option-value arguments argument)))
                  (pop arguments)
                  (setf (getf options key)
                        (funcall parser value argument))))
               (t
                (push argument positionals))))
    (values (nreverse positionals) options)))

(defun print-values (values)
  (dolist (value values)
    (format t "~S~%" value))
  (values))

(defun command-eval (arguments)
  (multiple-value-bind (positionals options)
      (parse-keyword-options
       arguments
       `(("--package" :package ,(lambda (value option)
                                  (declare (ignore option))
                                  value))))
    (unless (= 1 (length positionals))
      (command-line-error "eval expects exactly one FORM."))
    (let* ((package-name (or (getf options :package) "LUVCRAFT"))
           (package (or (find-package package-name)
                        (command-line-error "No package named ~A." package-name))))
      (let ((*package* package))
        (print-values (multiple-value-list
                       (eval (read-from-string (first positionals)))))))))

(defun dispatch-command (command arguments)
  (cond
    ((or (string= command "help")
         (string= command "--help")
         (string= command "-h"))
     (usage))
    ((string= command "block-world")
     (command-block-world arguments))
    ((string= command "gazetteer")
     (command-gazetteer arguments))
    ((string= command "eval")
     (command-eval arguments))
    ((string= command "lobby")
     (command-lobby arguments))
    (t
     (command-line-error "Unknown command ~A." command))))

(defun dispatch-main (arguments)
  (handler-case
      (if arguments
          (dispatch-command (first arguments) (rest arguments))
          (progn
            (usage *error-output*)
            (uiop:quit 2)))
    (command-line-error (condition)
      (format *error-output* "luv: ~A~%~%" condition)
      (usage *error-output*)
      (uiop:quit 2))))

(defun main (&optional (arguments (uiop:command-line-arguments)))
  (handler-case
      (progn
        (luv:call-with-sdl-main-thread
         (lambda ()
           (dispatch-main arguments)))
        (finish-output *standard-output*)
        (finish-output *error-output*))
    (error (condition)
      (format *error-output* "luv: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
