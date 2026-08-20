;;;; List and render the source-defined wiki media recipes.

(require :asdf)

(defun capture-project-root ()
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))

(format t "captures: loading recipe systems...~%")
(finish-output)
(dolist (system '("luv.asd" "luvcraft.asd" "luft.asd"
                  "mqtt.asd" "openai.asd" "telegram.asd"))
  (asdf:load-asd (merge-pathnames system (capture-project-root))))
(asdf:load-system :luv/showcase)
(format t "captures: recipes ready.~%")
(finish-output)

(defun capture-usage (&optional (stream *standard-output*))
  (format stream "Usage: scripts/captures list~%")
  (format stream "       scripts/captures render [--output DIRECTORY] [NAME...]~%"))

(defun parse-capture-render-arguments (arguments)
  (let ((directory
          (merge-pathnames #P"build/wiki/media/" (capture-project-root)))
        (names '()))
    (loop while arguments
          for argument = (pop arguments)
          do (cond ((string= argument "--output")
                    (unless arguments
                      (error "--output requires a directory."))
                    (setf directory (pathname (pop arguments))))
                   ((uiop:string-prefix-p "--" argument)
                    (error "Unknown option ~A." argument))
                   (t (push argument names))))
    (values directory (nreverse names))))

(defun list-captures ()
  (dolist (specification (luv:capture-specifications))
    (format t "~A~24T#~A  ~(~A~)/~A~%  ~A~%"
            (luv:capture-specification-name specification)
            (luv:capture-specification-figure-id specification)
            (luv:capture-specification-kind specification)
            (luv:capture-specification-extension specification)
            (luv:capture-specification-description specification))))

(defun render-captures (arguments)
  (multiple-value-bind (directory names)
      (parse-capture-render-arguments arguments)
    (luv:call-with-sdl-main-thread
     (lambda ()
       (luv:render-capture-set directory :names names)))))

(let ((arguments (uiop:command-line-arguments)))
  (cond ((equal arguments '("list"))
         (list-captures))
        ((and arguments (string= (first arguments) "render"))
         (render-captures (rest arguments)))
        (t
         (capture-usage *error-output*)
         (uiop:quit 2))))
