;;;; Loaded by the project-local SLY implementation before Slynk starts.

(load #P"/home/mbrock/quicklisp/setup.lisp")
(asdf:load-asd (merge-pathnames #P"luv.asd" *load-truename*))
(ql:quickload :luv/mcp :silent t)

(let ((configured-port (uiop:getenv "LUV_MCP_PORT")))
  (luv/mcp:start
   :port (if configured-port
             (parse-integer configured-port)
             luv/mcp:*default-port*)))
