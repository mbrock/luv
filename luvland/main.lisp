;;;; The standalone Luvland executable.
;;;;
;;;; This is how Luvland runs on a console: started from a virtual terminal's
;;;; own login, where SDL's KMSDRM driver owns the display, rather than inside
;;;; the swash-managed image, which has no terminal to own.  Its Slynk endpoint
;;;; lets ./sly --luvland reach the running compositor from elsewhere.

(in-package #:luvland)

(defun executable-directory ()
  (uiop:pathname-directory-pathname (truename sb-ext:*runtime-pathname*)))

(defun slynk-endpoint-pathname ()
  (let ((override (uiop:getenv "LUVLAND_SLYNK_ENDPOINT")))
    (if override
        (pathname override)
        (merge-pathnames #P"luvland.slynk" (executable-directory)))))

(defun publish-slynk-endpoint (pathname pid port)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname :direction :output
                                   :if-exists :supersede :if-does-not-exist :create)
    (format stream "~D ~D~%" pid port)))

(defun endpoint-owned-p (pathname pid port)
  (with-open-file (stream pathname :if-does-not-exist nil)
    (and stream
         (let ((*read-eval* nil))
           (and (eql pid (read stream nil nil))
                (eql port (read stream nil nil)))))))

(defun call-with-slynk (function)
  "Serve Slynk on loopback for the extent of FUNCTION."
  (setf slynk:*log-output* (make-broadcast-stream))
  (let* ((pid (sb-posix:getpid))
         (port (slynk:create-server :interface "127.0.0.1" :port 0 :dont-close t))
         (endpoint (slynk-endpoint-pathname)))
    (unwind-protect
         (progn
           (publish-slynk-endpoint endpoint pid port)
           (format t "Luvland Slynk is ready on 127.0.0.1:~D (pid ~D).~%" port pid)
           (format t "Attach with ./sly --luvland COMMAND ...~%")
           (finish-output)
           (funcall function))
      (when (endpoint-owned-p endpoint pid port)
        (ignore-errors (delete-file endpoint))))))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: luvland [--fullscreen] [--no-terminal] [--no-init]~%~
On a virtual terminal Luvland owns the display; C-t q quits.~%"))

(defun run-standalone-luvland (&key fullscreen-p (terminal-p t) (init-file-p t))
  "Serve Wayland clients until Luvland stops."
  (let ((luvland (start-luvland :fullscreen-p fullscreen-p
                                :spawn (and terminal-p *terminal*)
                                :init-file-p init-file-p)))
    (unwind-protect
         (loop while (luvland-running-p luvland) do (sleep 0.05))
      (stop-luvland luvland))))

(defun main ()
  "Run Luvland on the SDL main-thread host, with Slynk beside it."
  (let ((arguments (rest sb-ext:*posix-argv*)))
    (when (intersection arguments '("--help" "-h") :test #'string=)
      (usage)
      (uiop:quit 0))
    (let ((unknown (set-difference arguments '("--fullscreen" "--no-terminal" "--no-init")
                                   :test #'string=)))
      (when unknown
        (format *error-output* "luvland: unknown option ~A~%" (first unknown))
        (usage *error-output*)
        (uiop:quit 2)))
    (call-with-slynk
     (lambda ()
       (luv:call-with-sdl-main-thread
        (lambda ()
          (run-standalone-luvland
           :fullscreen-p (member "--fullscreen" arguments :test #'string=)
           :terminal-p (not (member "--no-terminal" arguments :test #'string=))
           :init-file-p (not (member "--no-init" arguments :test #'string=)))))))))
