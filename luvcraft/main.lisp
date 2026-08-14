(defpackage #:luvcraft
  (:use #:cl)
  (:export #:main))

(in-package #:luvcraft)

(defun usage (&optional (stream *standard-output*))
  (format stream "Usage: luvcraft [--help | --smoke-test PNG]~%")
  (format stream "~%")
  (format stream "With no arguments, open the interactive block world.~%")
  (format stream "--smoke-test renders one hidden Vulkan frame and exits.~%"))

(defun run-interactive ()
  "Run luvcraft until its native window closes."
  (let ((demo nil))
    (unwind-protect
         (progn
           (setf demo
                 (luv:start-cube-world-demo
                  :title "luvcraft — walk, jump, mine, and build"))
           ;; A native close request ends SDL's event loop.  Wait for complete
           ;; native teardown before releasing the demo-owned GPU resources.
           (loop until (eq :closed
                           (luv:canvas-state
                            (luv:cube-world-demo-canvas demo)))
                 do (sleep 0.05)))
      (when demo
        (luv:stop-cube-world-demo demo)))))

(defun run-smoke-test (pathname)
  (format t "Rendering ~A~%" pathname)
  (luv:capture-hidden-cube-world-screenshot pathname)
  (format t "Wrote ~A~%" (truename pathname)))

(defun dispatch (arguments)
  (cond
    ((null arguments)
     (run-interactive))
    ((and (= (length arguments) 1)
          (member (first arguments) '("--help" "-h") :test #'string=))
     (usage))
    ((and (= (length arguments) 2)
          (string= (first arguments) "--smoke-test"))
     (run-smoke-test (pathname (second arguments))))
    (t
     (usage *error-output*)
     (error "Invalid luvcraft arguments: ~{~A~^ ~}" arguments))))

(defun run-program-thread (arguments)
  "Run application work off the process main thread.

On macOS the SDL/Cocoa event loop is dispatched onto the process main thread.
Keeping startup on this worker lets OPEN-CANVAS finish while that loop runs."
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (progn
           (dispatch arguments)
           (finish-output *standard-output*)
           (finish-output *error-output*)
           (sb-ext:exit :code 0 :abort t))
       (error (condition)
         (format *error-output* "luvcraft: ~A~%" condition)
         (finish-output *error-output*)
         (sb-ext:exit :code 1 :abort t))))
   :name "luvcraft application")
  ;; Cocoa takes over this thread through TRIVIAL-MAIN-THREAD and does not
  ;; return it to an ordinary JOIN-THREAD reliably.  Application teardown and
  ;; process exit therefore belong to the worker above, while the entry thread
  ;; simply remains available as the native UI thread.
  (loop (sleep 3600)))

(defun main ()
  (run-program-thread (uiop:command-line-arguments)))
