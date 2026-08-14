(in-package #:luv.mcclim)

(defun open-listener (&key (server-path '(:luv))
                           (width 790)
                           (height 550)
                           (package :clim-user)
                           (debugger t)
                           exit-function)
  "Run the McCLIM Listener on a luv port in its own process.

Return the Listener process and application frame.  The frame top level owns
its McCLIM event queue; luv's SDL canvas thread only translates and enqueues
native input.  The menu bar is disabled until luv's Cocoa SDL host can own
multiple native canvases for McCLIM popup menu frames.  EXIT-FUNCTION, when
provided, runs after the frame and its native canvas have been torn down."
  (let* ((port (find-port :server-path server-path))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame nil)
         (startup-error nil)
         (startup-completion (sb-thread:make-semaphore :count 0)))
    (labels ((run ()
               (let ((*package* (find-package package)))
                 (handler-case
                     (progn
                       ;; On Cocoa, realizing a mirror hands the process main
                       ;; thread to SDL's durable event loop.  Construct the
                       ;; frame here so OPEN-LISTENER's caller is not stranded
                       ;; before McCLIM can enable and show the hidden canvas.
                       (setf frame
                             (make-application-frame
                              'clim-listener::listener
                              :frame-manager manager
                              :width width
                              :height height
                              :menu-bar nil))
                       (sb-thread:signal-semaphore startup-completion)
                       (unwind-protect
                            (if debugger
                                (clim-debugger:with-debugger ()
                                  (run-frame-top-level frame))
                                (run-frame-top-level frame))
                         (when (frame-manager frame)
                           (disown-frame manager frame))
                         (when exit-function
                           (funcall exit-function))))
                   (error (condition)
                     (unless frame
                       (setf startup-error condition)
                       (sb-thread:signal-semaphore startup-completion))
                     (error condition))))))
      (let ((process
              (clim-sys:make-process
               #'run :name "McCLIM Listener on luv")))
        (sb-thread:wait-on-semaphore startup-completion)
        (when startup-error
          (error startup-error))
        (values process frame)))))
