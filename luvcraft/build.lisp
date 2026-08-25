;;;; Build the luvcraft executable from a fresh SBCL in the luv development shell.
;;;;
;;;; SBCL's --script silences *compile-verbose* and *load-verbose*, which is
;;;; how a build that spends minutes compiling Lisp ends up printing only the
;;;; C toolchain's chatter.  build-progress.lisp turns that inside out: the
;;;; compiler's own output goes to one log file per source file, and a display
;;;; thread narrates the build on the console.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  ;; These contribs are found through ASDF, which grumbles about duplicate
  ;; entries in SBCL's own contrib directory on the way.
  (require :sb-concurrency)
  (require :sb-posix))

(defparameter *project-root*
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))

(let ((slynk-root (uiop:getenv "LUV_SLYNK_DIR")))
  (unless slynk-root
    (error "LUV_SLYNK_DIR is not set; build luvcraft through ./scripts/dev."))
  (asdf:load-asd
   (merge-pathnames #P"slynk.asd"
                    (uiop:ensure-directory-pathname slynk-root))))

(load (merge-pathnames #P"luvcraft/build-progress.lisp" *project-root*))

(luv-build:start *project-root* :system :luvcraft/program)

;;; C-c means stop, not "report a bug": exit quietly on SIGINT while leaving
;;; every other unhandled condition to SBCL's usual disabled-debugger
;;; backtrace, which is what a real compiler failure needs.
(let ((default-hook sb-ext:*invoke-debugger-hook*))
  (setf sb-ext:*invoke-debugger-hook*
        (lambda (condition hook)
          (if (typep condition 'sb-sys:interactive-interrupt)
              (progn
                (luv-build:finish :interrupted)
                (sb-ext:exit :code 130 :abort t))
              (funcall default-hook condition hook)))))

;;; PROGRAM-OP ends in SAVE-LISP-AND-DIE, which refuses to dump an image while
;;; other threads are alive, so the display thread is stopped on the way out.
(uiop:register-image-dump-hook
 (lambda () (luv-build:finish :done)))

(handler-case (asdf:make :luvcraft/program)
  (luv-build:deadline-exceeded ()
    (luv-build:finish :deadline)
    (sb-ext:exit :code 1 :abort t))
  (error (condition)
    ;; The compiler's account of the failure is already in a log; the summary
    ;; says which one.  This has to go through the display thread: the build's
    ;; own output is pointed at build/logs/ by now.
    (luv-build:failed (princ-to-string condition))
    (luv-build:finish :error)
    (sb-ext:exit :code 1 :abort t)))
