;;;; Build the luvcraft executable from a fresh SBCL in the luv development shell.

;;; SBCL's --script silences *compile-verbose* and *load-verbose*, which is how
;;; a build that spends minutes compiling Lisp ends up printing only the C
;;; toolchain's chatter.  Rather than turning those back on -- they would name
;;; every fasl of every dependency, in absolute paths -- the perform methods
;;; below narrate this project's own files and name each system as it loads.
(setf *compile-verbose* nil
      *compile-print* nil
      *load-verbose* nil
      *load-print* nil)

(defvar *compiled-file-count* 0)
(defvar *build-start-time* (get-internal-real-time))

(defun elapsed-seconds (since)
  (/ (float (- (get-internal-real-time) since) 1.0)
     internal-time-units-per-second))

;;; C-c during a build means stop, not "report a bug": exit quietly on
;;; SIGINT while leaving every other unhandled condition to SBCL's usual
;;; disabled-debugger backtrace, which is what a real compiler failure needs.
(let ((default-hook sb-ext:*invoke-debugger-hook*))
  (setf sb-ext:*invoke-debugger-hook*
        (lambda (condition hook)
          (if (typep condition 'sb-sys:interactive-interrupt)
              (progn
                (format *error-output* "~&;; interrupted after ~D file~:P, ~,1Fs~%"
                        *compiled-file-count* (elapsed-seconds *build-start-time*))
                (finish-output *error-output*)
                (sb-ext:exit :code 130 :abort t))
              (funcall default-hook condition hook)))))

(require :asdf)

(defparameter *project-root*
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))

(defun local-component-p (component)
  "True when COMPONENT's source lives inside this checkout."
  (let ((path (ignore-errors (asdf:component-pathname component))))
    (and path
         (uiop:subpathp path *project-root*)
         t)))

(defun component-label (component)
  "The component's source path, relative to the checkout."
  (let ((path (asdf:component-pathname component)))
    (or (ignore-errors (uiop:enough-pathname path *project-root*)) path)))

(defmethod asdf:perform :before ((op asdf:prepare-op) (system asdf:system))
  ;; Loading cached fasls is silent and slow enough to look like a hang, so
  ;; name each system as it goes by -- one line per system, not per fasl.
  ;; PREPARE-OP rather than LOAD-OP: the latter is performed after a system's
  ;; own files, which would print each heading below the work it announces.
  (format *error-output* "~&;; ~A~%" (asdf:component-name system))
  (finish-output *error-output*))

(defmethod asdf:perform :around ((op asdf:compile-op) (component asdf:cl-source-file))
  (if (local-component-p component)
      (let ((start (get-internal-real-time)))
        ;; Print the name *before* compiling, so a file that hangs names itself.
        (format *error-output* "~&;;   [~3D] ~A" (incf *compiled-file-count*)
                (component-label component))
        (finish-output *error-output*)
        (multiple-value-prog1 (call-next-method)
          (format *error-output* "  ~,1Fs~%" (elapsed-seconds start))
          (finish-output *error-output*)))
      (call-next-method)))

(asdf:load-asd (merge-pathnames #P"luv.asd" *project-root*))
(asdf:load-asd (merge-pathnames #P"luvcraft.asd" *project-root*))
(asdf:load-asd (merge-pathnames #P"mcluv.asd" *project-root*))
(asdf:load-asd (merge-pathnames #P"telegram.asd" *project-root*))

(let ((slynk-root (uiop:getenv "LUV_SLYNK_DIR")))
  (unless slynk-root
    (error "LUV_SLYNK_DIR is not set; build luvcraft through ./scripts/dev."))
  (asdf:load-asd
   (merge-pathnames #P"slynk.asd"
                    (uiop:ensure-directory-pathname slynk-root))))

;;; PROGRAM-OP ends in SAVE-LISP-AND-DIE, so the summary has to be printed on
;;; the way out rather than after ASDF:MAKE returns.
(uiop:register-image-dump-hook
 (lambda ()
   (format *error-output* "~&;; ~D file~:P compiled in ~,1Fs~%"
           *compiled-file-count* (elapsed-seconds *build-start-time*))
   (finish-output *error-output*)))

(asdf:make :luvcraft/program)
