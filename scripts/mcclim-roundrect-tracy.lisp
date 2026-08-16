;;;; Run the hidden McCLIM rounded-rectangle Tracy workload.

(require :asdf)

(defun project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(asdf:load-asd (merge-pathnames #P"luv.asd" (project-root)))
(asdf:load-asd (merge-pathnames #P"mcluv.asd" (project-root)))
(asdf:load-system :mcluv/roundrect-benchmark)

(destructuring-bind (ready-path &optional (shape-count "256") (repetitions "30"))
    (uiop:command-line-arguments)
  (setf luv:*gpu-provider* (make-instance 'luv:metal-gpu-provider))
  (luv:start-tracy :application-name "McCLIM roundrect A/B")
  (luv:call-with-sdl-main-thread
   (lambda ()
     (mcluv:run-roundrect-tracy-benchmark
      :shape-count (parse-integer shape-count)
      :repetitions (parse-integer repetitions)
      :ready-function
      (lambda ()
        (with-open-file (stream ready-path :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
          (write-line "ready" stream)))))))
  ;; Leave enough time for the capture client to drain the final events before
  ;; process teardown disconnects it.
  (sleep 0.5)
