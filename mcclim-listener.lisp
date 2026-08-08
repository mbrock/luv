(in-package #:luv.mcclim)

(defun open-listener (&key (server-path '(:luv))
                           (width 790)
                           (height 550)
                           (package :clim-user)
                           (debugger t))
  "Run the McCLIM Listener on a luv port in its own process.

Return the Listener process and application frame.  The frame top level owns
its McCLIM event queue; luv's SDL canvas thread only translates and enqueues
native input.  The menu bar is disabled until luv's Cocoa SDL host can own
multiple native canvases for McCLIM popup menu frames."
  (let* ((port (find-port :server-path server-path))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'standard-frame-manager :port port)))
         (frame
           (make-application-frame
            'clim-listener::listener
            :frame-manager manager
            :width width
            :height height
            :menu-bar nil)))
    (labels ((run ()
               (let ((*package* (find-package package)))
                 (unwind-protect
                      (if debugger
                          (clim-debugger:with-debugger ()
                            (run-frame-top-level frame))
                          (run-frame-top-level frame))
                   (when (frame-manager frame)
                     (disown-frame manager frame))))))
      (values (clim-sys:make-process
               #'run :name "McCLIM Listener on luv")
              frame))))
