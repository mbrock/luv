(defpackage #:luvcraft
  (:use #:cl)
  (:export #:*session*
           #:main))

(in-package #:luvcraft)

(defvar *session* nil
  "The session owned by the running standalone luvcraft, or NIL.")

(defun executable-directory ()
  (uiop:pathname-directory-pathname (truename sb-ext:*runtime-pathname*)))

(defun slynk-endpoint-pathname ()
  (merge-pathnames #P"luvcraft.slynk" (executable-directory)))

(defun requested-slynk-port ()
  (let ((value (uiop:getenv "LUVCRAFT_SLYNK_PORT")))
    (if value
        (let ((port (parse-integer value :junk-allowed t)))
          (unless (and port
                       (<= 0 port 65535)
                       (string= value (format nil "~D" port)))
            (error "LUVCRAFT_SLYNK_PORT must be an integer from 0 through 65535, not ~S."
                   value))
          port)
        0)))

(defun endpoint-owned-p (pathname pid port)
  (with-open-file (stream pathname :if-does-not-exist nil)
    (and stream
         (let ((*read-eval* nil))
           (and (eql pid (read stream nil nil))
                (eql port (read stream nil nil)))))))

(defun publish-slynk-endpoint (pathname pid port)
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream "~D ~D~%" pid port)))

(defun call-with-slynk (function)
  (setf slynk:*log-output* (make-broadcast-stream))
  (let* ((pid (sb-posix:getpid))
         (port (slynk:create-server
                :interface "127.0.0.1"
                :port (requested-slynk-port)
                :dont-close t))
         (endpoint (slynk-endpoint-pathname)))
    (unwind-protect
         (progn
           (publish-slynk-endpoint endpoint pid port)
           (format t "Luvcraft Slynk is ready on 127.0.0.1:~D (pid ~D).~%"
                   port pid)
           (format t "Attach with ./sly --luvcraft COMMAND ...~%")
           (finish-output)
           (funcall function))
      (when (endpoint-owned-p endpoint pid port)
        (ignore-errors (delete-file endpoint))))))

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: luvcraft [--help | --metal | --smoke-test PNG | --metal-smoke-test PNG | --metal-benchmark [FRAMES [CSV]]]~%")
  (format stream "~%")
  (format stream "With no arguments, open the interactive block world.~%")
  (format stream "--metal opens the interactive world with the Metal 4 backend.~%")
  (format stream "--smoke-test renders one hidden Vulkan frame and exits.~%")
  (format stream "--metal-smoke-test renders one hidden Metal 4 frame and exits.~%")
  (format stream "--metal-benchmark measures a fixed, fully resident Metal world.~%"))

(defun run-interactive (&optional provider)
  "Run luvcraft until its native window closes."
  (let ((session nil))
    (unwind-protect
         (progn
           (setf session
                 (luv:start-luvcraft
                  :provider (or provider luv:*gpu-provider*)
                  :title "luvcraft — walk, jump, mine, and build")
                 *session* session)
           ;; A native close request ends SDL's event loop.  Wait for complete
           ;; native teardown before releasing the session-owned GPU resources.
           (loop until (eq :closed
                           (luv:canvas-state
                            (luv:luvcraft-session-canvas session)))
                 do (sleep 0.05)))
      (when session
        (luv:stop-luvcraft session))
      (setf *session* nil))))

(defun run-smoke-test (pathname &optional provider)
  (format t "Rendering ~A~%" pathname)
  (luv:capture-hidden-luvcraft-screenshot
   pathname :provider (or provider luv:*gpu-provider*))
  (format t "Wrote ~A~%" (truename pathname)))

(defun parse-frame-count (argument)
  (let ((count (parse-integer argument :junk-allowed t)))
    (unless (and count (plusp count)
                 (string= argument (format nil "~D" count)))
      (error "Frame count must be a positive integer, not ~S." argument))
    count))

(defun run-metal-benchmark (&optional frame-count pathname)
  (luv:benchmark-luvcraft-frame-performance
   :frame-count (if frame-count (parse-frame-count frame-count) 120)
   :csv-pathname (or (and pathname (pathname pathname))
                     #P"build/luvcraft-metal-benchmark.csv")))

(defun dispatch (arguments)
  (cond
    ((null arguments)
     (run-interactive))
    ((and (= (length arguments) 1)
          (string= (first arguments) "--metal"))
     (run-interactive (make-instance 'luv:metal-gpu-provider)))
    ((and (= (length arguments) 1)
          (member (first arguments) '("--help" "-h") :test #'string=))
     (usage))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--smoke-test"))
     (run-smoke-test (pathname (second arguments))))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--metal-smoke-test"))
     (run-smoke-test
      (pathname (second arguments))
      (make-instance 'luv:metal-gpu-provider)))
    ((and (<= 1 (length arguments) 3)
          (string= (first arguments) "--metal-benchmark"))
     (run-metal-benchmark (second arguments) (third arguments)))
    (t
     (usage *error-output*)
     (error "Invalid luvcraft arguments: ~{~A~^ ~}" arguments))))

(defun main ()
  (handler-case
      (call-with-slynk
       (lambda ()
         (luv:call-with-sdl-main-thread
          (lambda ()
            (dispatch (uiop:command-line-arguments))))
         (finish-output *standard-output*)
         (finish-output *error-output*)))
    (error (condition)
      (format *error-output* "luvcraft: ~A~%" condition)
      (finish-output *error-output*)
      (uiop:quit 1))))
