(require :asdf)

(defun project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(let ((*default-pathname-defaults* (project-root))
      (*compile-verbose* nil)
      (*compile-print* nil)
      (*load-verbose* nil)
      (*load-print* nil))
  (handler-bind ((warning #'muffle-warning)
                 (sb-ext:compiler-note #'muffle-warning))
    (asdf:load-asd (truename "luv.asd"))
    (asdf:load-asd (truename "luvcraft.asd"))
    (asdf:load-asd (truename "telegram.asd"))
    (asdf:load-asd (truename "mqtt.asd"))
    (asdf:load-asd (truename "openai.asd"))
    (asdf:load-asd (truename "chrome-cdp.asd"))
    (asdf:load-asd (truename "luv-wiki.asd"))
    (asdf:load-asd (truename "luft.asd"))
    (asdf:load-asd (truename "sly-client.asd"))
    (asdf:load-system :luv/test-support)
    (let ((systems '(:luv :luv/tracy-capture :luv/application-agent
                     :luv/ghostty :luv/libav :luvcraft :luvcraft/agent
                     :mqtt :openai :chrome-cdp
                     :sly-client/test
                     :luv-wiki :luft :luft/render
                     :luft/z-fiber-benchmark)))
      (unless (uiop:getenv "LUV_GHOSTTY_LIBRARY")
        (setf systems (remove :luv/ghostty systems))
        (format t "~&luv/ghostty (libghostty-vt unavailable; skipped)~%"))
      (let* ((start (get-internal-real-time))
             (count (length systems))
             (stage-count (1+ count))
             (counter-width
               (+ 2 (* 2 (length (princ-to-string stage-count)))))
             (timings nil))
        (loop for system in systems
              for index from 1
              for name =
                (string-downcase
                 (asdf:component-name (asdf:find-system system)))
              for system-start = (get-internal-real-time)
              do (format t "~&~5,1,,,'0Fs~V@A  ~A~%"
                         (/ (float (- system-start start) 1.0)
                            internal-time-units-per-second)
                         counter-width
                         (format nil "~D/~D" index stage-count) name)
                 (finish-output)
                 (asdf:test-system system)
                 (push
                  (cons name
                        (/ (float (- (get-internal-real-time) system-start) 1.0)
                           internal-time-units-per-second))
                  timings))
        (let ((validation-start (get-internal-real-time)))
          (format t "~&~5,1,,,'0Fs~V@A  shader-validate~%"
                  (/ (float (- validation-start start) 1.0)
                     internal-time-units-per-second)
                  counter-width (format nil "~D/~D" stage-count stage-count))
          (finish-output)
          (load (merge-pathnames #P"scripts/shader-validation.lisp"))
          (uiop:symbol-call
           :luv.shader-validation :validate-production-shaders)
          (push
           (cons "shader-validate"
                 (/ (float (- (get-internal-real-time) validation-start) 1.0)
                    internal-time-units-per-second))
           timings))
        (let ((seconds
                (/ (float (- (get-internal-real-time) start) 1.0)
                   internal-time-units-per-second))
              (slowest (first (sort timings #'> :key #'cdr))))
          (format t "~&~%;; Tested ~D system~:P and validated shaders in ~A.~%"
                  count
                  (uiop:symbol-call
                   :luv.test-support :format-seconds seconds))
          (when slowest
            (format t ";; Slowest was ~A at ~A.~%"
                    (car slowest)
                    (uiop:symbol-call
                     :luv.test-support :format-seconds (cdr slowest)))))))))
