;;;; Cold-process acceptance probe for the SDL/Cocoa Metal 4 clear.

(require :asdf)

(asdf:load-asd (truename (merge-pathnames #P"../luv.asd" *load-truename*)))

(asdf:load-system :luv/canvas/metal)

(sb-thread:make-thread
 (lambda ()
   (handler-case
       (progn
         (format t "~S~%" (luv:probe-sdl-metal-clear))
         (finish-output)
         (sb-ext:exit :code 0 :abort t))
     (error (condition)
       (format *error-output* "Metal clear probe failed: ~A~%" condition)
       (finish-output *error-output*)
       (sb-ext:exit :code 1 :abort t))))
 :name "luv Metal clear probe")

;; SDL owns the Cocoa event loop on this thread.  The worker exits the process
;; after presentation and complete native teardown.
(loop (sleep 3600))
