(require :asdf)

(defun project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(let ((*default-pathname-defaults* (project-root)))
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-asd (truename "luv.asd"))
    (asdf:load-asd (truename "luvcraft.asd"))
    (asdf:load-asd (truename "telegram.asd"))
    (asdf:load-asd (truename "mqtt.asd"))
    (asdf:load-asd (truename "openai.asd"))
    (asdf:load-asd (truename "luv-wiki.asd"))
    (asdf:load-asd (truename "luv-wiki-site.asd"))
    (asdf:load-asd (truename "luft.asd"))
    (asdf:load-system :rove)
    (load (merge-pathnames #P"luv/test-reporter.lisp"))
    (uiop:symbol-call :luv.test-reporter :register-luv-reporter)
    (setf (symbol-value (uiop:find-symbol* :*default-reporter* :rove)) :luv)
    (uiop:symbol-call :rove :use-reporter :luv)
    (dolist (system '(:luv :luv/ghostty :luv/libav :luvcraft :luvcraft/agent
                      :mqtt :openai
                      :luv-wiki :luft :luft/render))
      (format t "~&~A~%" (string-downcase (asdf:component-name (asdf:find-system system))))
      (asdf:test-system system)
      (terpri))))
