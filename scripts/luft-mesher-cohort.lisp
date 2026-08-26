;;;; Cold, warm, and one-edit cohort benchmarks of the LUFT mesh query.

(require :asdf)

(defun project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(asdf:load-asd (merge-pathnames #P"luv.asd" (project-root)))
(asdf:load-asd (merge-pathnames #P"luft.asd" (project-root)))
(asdf:load-system :luft/mesh-query-profile)

(destructuring-bind (&optional
                       (output "build/luft-mesher-cohort.txt")
                       (warm-iterations "5"))
    (uiop:command-line-arguments)
  (luft.mesh-query-profile:run-mesh-cohort-benchmark
   :output output
   :warm-iterations (parse-integer warm-iterations)))
