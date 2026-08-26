;;;; Profile the production LUFT mesh query on one representative chunk.

(require :asdf)

(defun project-root ()
  (merge-pathnames #P"../"
                   (uiop:pathname-directory-pathname *load-truename*)))

(asdf:load-asd (merge-pathnames #P"luv.asd" (project-root)))
(asdf:load-asd (merge-pathnames #P"luft.asd" (project-root)))
(asdf:load-system :luft/mesh-query-profile)

(destructuring-bind (&optional
                       (directory "build/luft-mesher-profile")
                       (profile-seconds "2")
                       (sample-interval "0.0005")
                       (timing-seconds "0.25"))
    (uiop:command-line-arguments)
  (luft.mesh-query-profile:run-mesh-query-profile
   :output-directory directory
   :profile-seconds (coerce (read-from-string profile-seconds) 'double-float)
   :sample-interval (coerce (read-from-string sample-interval) 'double-float)
   :timing-seconds (coerce (read-from-string timing-seconds) 'double-float)))
