(require :asdf)

(defun project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(let ((*default-pathname-defaults* (project-root))
      (*compile-verbose* nil)
      (*compile-print* nil)
      (*load-verbose* nil)
      (*load-print* nil))
  (handler-bind ((warning #'muffle-warning)
                 (sb-ext:compiler-note #'muffle-warning))
    (asdf:load-asd (truename "luv.asd"))
    (asdf:load-asd (truename "luvcraft.asd"))
    (asdf:load-asd (truename "telegram.asd"))
    (asdf:load-asd (truename "mqtt.asd"))
    (asdf:load-asd (truename "openai.asd"))
    (asdf:load-asd (truename "chrome-cdp.asd"))
    (asdf:load-asd (truename "luv-wiki.asd"))
    (asdf:load-asd (truename "luv-wiki-site.asd"))
    (asdf:load-asd (truename "luft.asd"))
    (asdf:load-asd (truename "sly-client.asd"))
    (asdf:load-system :rove)
    (load (merge-pathnames #P"luv/test-reporter.lisp"))
    (uiop:symbol-call :luv.test-reporter :register-luv-reporter)
    (setf (symbol-value (uiop:find-symbol* :*default-reporter* :rove)) :luv)
    (uiop:symbol-call :rove :use-reporter :luv)
    (let ((systems '(:luv :luv/lobby :luv/tracy-capture
                     :luv/mcclim :luv/lobby/mcclim :luv/application-agent
                     :luv/ghostty :luv/libav :luvcraft :luvcraft/agent
                     :mqtt :openai :chrome-cdp
                     :sly-client/test
                     :luv-wiki :luft :luft/render
                     :luft/z-fiber-benchmark)))
      (unless (uiop:getenv "LUV_GHOSTTY_LIBRARY")
        (setf systems (remove :luv/ghostty systems))
        (format t "~&luv/ghostty (libghostty-vt unavailable; skipped)~%"))
      (dolist (system systems)
        (format t "~&~A~%"
                (string-downcase (asdf:component-name (asdf:find-system system))))
        (asdf:test-system system)))))
