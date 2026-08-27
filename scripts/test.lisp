;;;; Worker half of scripts/test.py.  One invocation either prepares every
;;;; suite serially or runs a shard while emitting machine-readable boundaries.

(require :asdf)

(defparameter *project-root*
  (truename
   (merge-pathnames #P"../"
                    (uiop:pathname-directory-pathname *load-truename*))))

(defparameter *asd-files*
  '("luv.asd" "luvcraft.asd" "telegram.asd" "mqtt.asd" "openai.asd"
    "chrome-cdp.asd" "luv-wiki.asd" "luft.asd" "sly-client.asd"))

(defun load-project-definitions ()
  (dolist (file *asd-files*)
    (asdf:load-asd (truename file))))

(defun shader-validation-p (name)
  (string= name "shader-validate"))

(defun load-target (name)
  (if (shader-validation-p name)
      (progn
        (asdf:load-system :luvcraft/agent)
        (load (merge-pathnames #P"scripts/shader-validation.lisp"
                               *project-root*)))
      (asdf:load-system name)))

(defun run-target (name)
  (if (shader-validation-p name)
      (progn
        (load-target name)
        (uiop:symbol-call
         :luv.shader-validation :validate-production-shaders))
      (asdf:test-system name)))

(defun prepare-targets (names)
  ;; ASDF writes shared FASLs here, before any other SBCL exists.  Workers can
  ;; consequently load and test in parallel without racing cache publication.
  (dolist (name names)
    (load-target name))
  (format t "~&;; Prepared ~D test suite~:P.~%" (length names)))

(defun run-worker (names)
  (let ((failed nil))
    (dolist (name names)
      (format t "~&@@LUV-TEST@@ START ~A~%" name)
      (finish-output)
      (let ((start (get-internal-real-time))
            (status "passed"))
        (let ((*error-output* *standard-output*)
              (*trace-output* *standard-output*))
          (handler-case (run-target name)
            (error (condition)
              (setf status "failed"
                    failed t)
              (uiop:print-condition-backtrace
               condition :stream *standard-output* :count 30))))
        (format t "~&@@LUV-TEST@@ END ~A ~A ~,3F~%"
                name status
                (/ (float (- (get-internal-real-time) start) 1.0)
                   internal-time-units-per-second))
        (finish-output)))
    (when failed
      (uiop:quit 1))))

(let ((*default-pathname-defaults* *project-root*)
      (*compile-verbose* nil)
      (*compile-print* nil)
      (*load-verbose* nil)
      (*load-print* nil)
      (arguments (uiop:command-line-arguments)))
  (handler-bind ((warning #'muffle-warning)
                 (sb-ext:compiler-note #'muffle-warning))
    (load-project-definitions)
    (cond ((and arguments (string= (first arguments) "--prepare"))
           (prepare-targets (rest arguments)))
          ((and arguments (string= (first arguments) "--worker"))
           (run-worker (rest arguments)))
          (t
           (error "Expected --prepare or --worker followed by test systems.")))))
