(defpackage #:luv/mcp
  (:use #:cl)
  (:export #:*default-port*
           #:start
           #:status
           #:stop))

(in-package #:luv/mcp)

(defparameter *default-port* 12345
  "The localhost port used by luv's shared-image MCP server and Codex bridge.")

(defun project-root ()
  (uiop:ensure-directory-pathname
   (asdf:system-source-directory :luv)))

(defun start (&key (port *default-port*) (stream *standard-output*))
  "Start cl-mcp in this Lisp image, sharing all state with SLY.

The TCP listener runs on a background thread. Worker isolation is explicitly
disabled, so REPL evaluation and code introspection happen in this process."
  (setf cl-mcp:*project-root* (project-root))
  (multiple-value-bind (thread actual-port)
      (cl-mcp:start-tcp-server-thread
       :host "127.0.0.1"
       :port port
       :accept-once nil
       :worker-pool nil)
    (declare (ignore thread))
    (unless actual-port
      (error "cl-mcp did not open its TCP listener."))
    (format stream
            "Shared-image MCP listening on tcp://127.0.0.1:~D (worker pool disabled).~%"
            actual-port)
    actual-port))

(defun status ()
  "Describe the shared MCP listener and confirm whether isolation is disabled."
  (list :running (cl-mcp:tcp-server-running-p)
        :port cl-mcp:*tcp-server-port*
        :worker-pool cl-mcp:*use-worker-pool*
        :project-root cl-mcp:*project-root*))

(defun stop ()
  "Stop the shared MCP listener without stopping the SLY Lisp image."
  (cl-mcp:stop-tcp-server-thread))
