;;;; Standalone Slynk server bootstrap for ./sly.

(require :asdf)
(require :sb-concurrency)
(require :sb-posix)

(defparameter cl-user::*luv-project-root* nil)

(defun getenv-or (name default)
  (or (uiop:getenv name) default))

(defun required-directory (name)
  (let ((value (uiop:getenv name)))
    (unless value
      (error "~A is not set. Enter `nix develop` so the luv shell can provide it."
             name))
    (uiop:ensure-directory-pathname value)))

(defun emit-swash-event (session event message &rest fields)
  (let ((swash (or (uiop:getenv "LUV_SWASH")
                   (error "LUV_SWASH is not set"))))
    (let ((process
            (uiop:launch-program
             (append (list swash "emit" session
                           "--event" event "--message" message)
                     (loop for field in fields append (list "--field" field)))
             :input nil :output nil :error-output *error-output*)))
      (unless (zerop (uiop:wait-process process))
        (error "Could not publish Swash event ~A for ~A" event session)))))

(let* ((project-root
         (uiop:pathname-directory-pathname *load-truename*))
       (slynk-root (required-directory "LUV_SLYNK_DIR"))
       (requested-port (parse-integer (getenv-or "LUV_SLYNK_PORT" "0")))
       (session (or (uiop:getenv "SWASH_SESSION")
                    (error "sly-server.lisp must run inside a Swash session")))
       (name (getenv-or "LUV_NAME" session)))
  (setf cl-user::*luv-project-root* project-root)
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
  (asdf:load-asd (merge-pathnames #P"sly-client.asd" project-root))
  (load (merge-pathnames #P"scripts/sly-asdf-status.lisp" project-root))
  (load (merge-pathnames #P"luvcraft/build-progress.lisp" project-root))
  (let ((systems '(:luv :luvcraft :luvcraft/agent :luvcraft/birthday
                   :luv-wiki :luft/render)))
    ;; The server's outer log is relayed by ./sly while this boot runs, so keep
    ;; narration there. Per-file compiler/toolchain chatter still gets the
    ;; build progress module's focused logs.
    (labels ((build-call (name &rest arguments)
               (apply (symbol-function (find-symbol name "LUV-BUILD"))
                      arguments))
             (deadline-exceeded-p (condition)
               (typep condition
                      (find-symbol "DEADLINE-EXCEEDED" "LUV-BUILD"))))
      (build-call "START" project-root :system systems :invocation "sly boot"
                  :redirect-output-p nil)
      (handler-case
          (progn
            (dolist (system systems)
              (asdf:load-system system))
            (build-call "FINISH" :done))
        (error (condition)
          (if (deadline-exceeded-p condition)
              (progn
                (build-call "FINISH" :deadline)
                (sb-ext:exit :code 1 :abort t))
              (progn
                (build-call "FAILED" (princ-to-string condition))
                (build-call "FINISH" :error)
                (error condition)))))))
  (format t "~&Starting luv Slynk on a kernel-assigned loopback port.~%")
  (force-output)
  (let ((port
          (funcall (find-symbol "CREATE-SERVER" "SLYNK")
                   :interface "127.0.0.1"
                   :port requested-port
                   :dont-close t)))
    (emit-swash-event
     session "slynk-ready" (format nil "Lisp ~A is ready" name)
     "LUV_KIND=LISP"
     (format nil "LUV_ROOT=~A" (namestring project-root))
     (format nil "LUV_NAME=~A" name)
     (format nil "LUV_SLYNK_PORT=~D" port)
     (format nil "LUV_SLYNK_PID=~D" (sb-posix:getpid)))
    (format t "~&Luv Slynk is ready on 127.0.0.1:~D (Swash ~A).~%"
            port session)
    (force-output)
    (loop (sleep 3600))))
