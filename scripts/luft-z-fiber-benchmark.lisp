;;;; Run the deterministic LUFT Z-fiber CPU benchmark.

(require :asdf)

(defun project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(asdf:load-asd (merge-pathnames #P"luv.asd" (project-root)))
(asdf:load-asd (merge-pathnames #P"luft.asd" (project-root)))
(asdf:load-system :luft/z-fiber-benchmark)

(destructuring-bind (&optional
                       (csv "build/luft-z-fiber-benchmark.csv")
                       (widths "16,32")
                       (patterns "solid,terrain,architecture,caves,checkerboard")
                       (samples "15")
                       (warmups "3"))
    (uiop:command-line-arguments)
  (luft.z-fiber-benchmark:run-z-fiber-benchmark
   :csv-pathname csv
   :widths (mapcar #'parse-integer
                   (uiop:split-string widths :separator ","))
   :patterns
   (mapcar (lambda (name) (intern (string-upcase name) :keyword))
           (uiop:split-string patterns :separator ","))
   :sample-count (parse-integer samples)
   :warmup-count (parse-integer warmups)))
