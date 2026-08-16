(in-package #:luvcraft)

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
  (format stream "Usage: luvcraft [--metal | --vulkan] [--tracy] [--world FILE]~%")
  (format stream "       luvcraft [--help | --smoke-test PNG | --vulkan-smoke-test PNG | --metal-smoke-test PNG | --metal-text-closeup PNG | --metal-benchmark [FRAMES [CSV [SCENARIO]]]]~%")
  (format stream "~%")
  (format stream "With no arguments, resume the world using Metal 4 on macOS and Vulkan elsewhere.~%")
  (format stream "--metal and --vulkan explicitly select an interactive backend.~%")
  (format stream "--tracy exposes live frame and worker zones to Tracy 0.13.1.~%")
  (format stream "--world loads or creates the named persistent world.~%")
  (format stream "--smoke-test renders one hidden frame with the platform default and exits.~%")
  (format stream "--vulkan-smoke-test renders one hidden Vulkan frame and exits.~%")
  (format stream "--metal-smoke-test renders one hidden Metal 4 frame and exits.~%")
  (format stream "--metal-text-closeup renders the enlarged Slug world-text proof.~%")
  (format stream "--metal-benchmark measures steady or streaming Metal frames.~%"))

(defun make-metal-provider ()
  #+darwin
  (make-instance 'luv:metal-gpu-provider)
  #-darwin
  (error "The Metal backend is only available on Darwin."))

(defun make-vulkan-provider ()
  (make-instance 'luv:vulkan-gpu-provider))

(defun run-interactive (&key provider tracy-p
                             (world-pathname
                               (default-luvcraft-world-pathname)))
  "Run luvcraft until its native window closes."
  ;; Start before START-LUVCRAFT creates the producer.  Tracy thread names are
  ;; emitted by the threads themselves as they begin, so attaching later would
  ;; leave an otherwise useful worker lane anonymous.
  (play :provider (or provider luv:*gpu-provider*)
        :tracy-p tracy-p
        :world-pathname world-pathname)
  (unwind-protect
       (let ((session *session*))
         ;; A native close request ends SDL's event loop.  Wait for complete
         ;; native teardown before releasing the session-owned GPU resources.
         (loop until (eq :closed
                         (luv:canvas-state
                          (luvcraft-session-canvas session)))
               do (sleep 0.05))
         (let ((failure
                 (luv::sdl-canvas-startup-error
                  (luvcraft-session-canvas session))))
           (when failure (error failure))))
    (stop-playing)))

(defun parse-interactive-options (arguments)
  (let ((provider nil)
        (tracy-p nil)
        (world-pathname (default-luvcraft-world-pathname)))
    (loop while arguments
          for argument = (pop arguments)
          do (cond
               ((string= argument "--metal")
                (setf provider (make-metal-provider)))
               ((string= argument "--vulkan")
                (setf provider (make-vulkan-provider)))
               ((string= argument "--tracy")
                (setf tracy-p t))
               ((string= argument "--world")
                (unless arguments
                  (error "--world requires a pathname."))
                (setf world-pathname (pathname (pop arguments))))
               (t (return-from parse-interactive-options
                    (values nil nil nil nil)))))
    (values provider world-pathname tracy-p t)))

(defun run-smoke-test (pathname &optional provider)
  (format t "Rendering ~A~%" pathname)
  (capture-hidden-luvcraft-screenshot
   pathname :provider (or provider luv:*gpu-provider*))
  (format t "Wrote ~A~%" (truename pathname)))

(defun run-metal-text-closeup (pathname)
  (format t "Rendering Slug text close-up ~A~%" pathname)
  (capture-hidden-luvcraft-text-closeup
   pathname :provider (make-metal-provider))
  (format t "Wrote ~A~%" (truename pathname)))

(defun parse-frame-count (argument)
  (let ((count (parse-integer argument :junk-allowed t)))
    (unless (and count (plusp count)
                 (string= argument (format nil "~D" count)))
      (error "Frame count must be a positive integer, not ~S." argument))
    count))

(defun parse-benchmark-scenario (argument)
  (cond ((or (null argument) (string= argument "steady")) :steady)
        ((string= argument "streaming") :streaming)
        (t (error "Benchmark scenario must be steady or streaming, not ~S."
                  argument))))

(defun run-metal-benchmark (&optional frame-count pathname scenario)
  (benchmark-luvcraft-frame-performance
   :frame-count (if frame-count (parse-frame-count frame-count) 120)
   :scenario (parse-benchmark-scenario scenario)
   :csv-pathname (or (and pathname (pathname pathname))
                     #P"build/luvcraft-metal-benchmark.csv")))

(defun dispatch (arguments)
  (cond
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
      (make-metal-provider)))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--vulkan-smoke-test"))
     (run-smoke-test
      (pathname (second arguments))
      (make-vulkan-provider)))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--metal-text-closeup"))
     (run-metal-text-closeup (pathname (second arguments))))
    ((and (<= 1 (length arguments) 4)
          (string= (first arguments) "--metal-benchmark"))
     (run-metal-benchmark
      (second arguments) (third arguments) (fourth arguments)))
    (t
     (multiple-value-bind (provider world-pathname tracy-p interactive-p)
         (parse-interactive-options arguments)
       (if interactive-p
           (run-interactive :provider provider :tracy-p tracy-p
                            :world-pathname world-pathname)
           (progn
             (usage *error-output*)
             (error "Invalid luvcraft arguments: ~{~A~^ ~}" arguments)))))))

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
