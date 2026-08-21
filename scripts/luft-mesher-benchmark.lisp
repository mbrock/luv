;;;; Run the deterministic LUFT CPU materialization benchmark.

(require :asdf)

(defun project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(asdf:load-asd (merge-pathnames #P"luv.asd" (project-root)))
(asdf:load-asd (merge-pathnames #P"luft.asd" (project-root)))
(asdf:load-system :luft/benchmark)

(destructuring-bind (&optional
                       (csv "build/luft-mesher-benchmark.csv")
                       (sizes "8,16,32,64")
                       (patterns "solid,terrain,architecture,checkerboard")
                       (samples "15")
                       (warmups "3"))
    (uiop:command-line-arguments)
  (luft.benchmark:run-mesher-benchmark
   :csv-pathname csv
   :sizes (mapcar #'parse-integer (uiop:split-string sizes :separator ","))
   :patterns
   (mapcar (lambda (name) (intern (string-upcase name) :keyword))
           (uiop:split-string patterns :separator ","))
   :sample-count (parse-integer samples)
   :warmup-count (parse-integer warmups)))
