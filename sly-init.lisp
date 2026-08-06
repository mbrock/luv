;;;; Loaded by the project-local SLY implementation before Slynk starts.

(load
 (merge-pathnames
  #P"setup.lisp"
  (let ((configured-home (sb-ext:posix-getenv "QUICKLISP_HOME")))
    (if configured-home
        (pathname (format nil "~A/" configured-home))
        (merge-pathnames #P"quicklisp/" (user-homedir-pathname))))))
(asdf:load-asd (merge-pathnames #P"luv.asd" *load-truename*))
(ql:quickload :luv/mcp :silent t)

(let ((configured-port (uiop:getenv "LUV_MCP_PORT")))
  (luv/mcp:start
   :port (if configured-port
             (parse-integer configured-port)
             luv/mcp:*default-port*)))
