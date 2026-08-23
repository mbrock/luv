;;;; Standalone Slynk server bootstrap for ./sly.

(require :asdf)
(handler-bind ((warning #'muffle-warning))
  ;; build-progress.lisp uses these SBCL contrib packages at read time, just
  ;; like the standalone luvcraft and LUFT build bootstraps do.
  (require :sb-concurrency)
  (require :sb-posix))

(defparameter cl-user::*luv-project-root* nil)
(defparameter cl-user::*luv-slynk-port* nil)

(defun getenv-or (name default)
  (or (uiop:getenv name) default))

(defun required-directory (name)
  (let ((value (uiop:getenv name)))
    (unless value
      (error "~A is not set. Enter `nix develop` so the luv shell can provide it."
             name))
    (uiop:ensure-directory-pathname value)))

(let* ((project-root
         (uiop:pathname-directory-pathname *load-truename*))
       (slynk-root (required-directory "LUV_SLYNK_DIR"))
       (port (parse-integer (getenv-or "LUV_SLYNK_PORT" "4005"))))
  (setf cl-user::*luv-project-root* project-root
        cl-user::*luv-slynk-port* port)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(namestring slynk-root))
     :inherit-configuration))
  (asdf:load-asd (merge-pathnames #P"slynk.asd" slynk-root))
  ;; Slynk first, so the wiki's IN-READTABLE forms can register their
  ;; readtables while the durable image is assembled.
  (asdf:load-system :slynk)
  (asdf:load-asd (merge-pathnames #P"luv.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"luvcraft.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"luv-wiki.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"luft.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"telegram.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"mqtt.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"openai.asd" project-root))
  (asdf:load-asd (merge-pathnames #P"chrome-cdp.asd" project-root))
  ;; This is a separate top-level form from the LUV-BUILD uses below.  LOAD
  ;; reads and evaluates source one form at a time, so a genuinely fresh Lisp
  ;; now has both the package and its functions before reading that next form.
  (load (merge-pathnames #P"luvcraft/build-progress.lisp" project-root)))

(let ((project-root cl-user::*luv-project-root*)
      (port cl-user::*luv-slynk-port*)
      (systems '(:luv :luvcraft :luvcraft/agent :luvcraft/birthday
                 :luv-wiki :luft/render)))
    ;; The server's outer log is relayed by ./sly while this boot runs, so keep
    ;; narration there. Per-file compiler/toolchain chatter still gets the
    ;; build progress module's focused logs.
    (luv-build:start project-root :system systems :invocation "sly boot"
                     :redirect-output-p nil)
    (handler-case
        (progn
          (dolist (system systems)
            (asdf:load-system system))
          (luv-build:finish :done))
      (luv-build:deadline-exceeded ()
        (luv-build:finish :deadline)
        (sb-ext:exit :code 1 :abort t))
      (error (condition)
        (luv-build:failed (princ-to-string condition))
        (luv-build:finish :error)
        (error condition)))
  (format t "~&Starting luv Slynk on 127.0.0.1:~D.~%" port)
  (funcall (find-symbol "CREATE-SERVER" "SLYNK")
           :interface "127.0.0.1"
           :port port
           :dont-close t)
  (format t "~&Luv Slynk is ready on 127.0.0.1:~D.~%" port)
  (loop (sleep 3600)))
