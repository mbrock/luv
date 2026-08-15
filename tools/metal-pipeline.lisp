;;;; Cold-process acceptance probe for the luvcraft Metal 4 render pipeline.

(require :asdf)

(asdf:load-asd (truename (merge-pathnames #P"../luv.asd" *load-truename*)))
(asdf:load-system :luv/gpu/metal)
(asdf:load-system :luv/luvcraft/shaders)

(handler-case
    (progn
      (format
       t "~S~%"
       (luv:probe-metal-render-pipeline
        (luv.spir-v:block-world-vertex-specification)
        (luv.spir-v:block-world-fragment-specification)
        '((:array-stride 48
           :attributes
           ((:shader-location 0 :offset 0 :format :float32x3)
            (:shader-location 1 :offset 12 :format :float32x3)
            (:shader-location 2 :offset 24 :format :float32x3)
            (:shader-location 3 :offset 36 :format :float32x3))))))
      (finish-output)
      (sb-ext:exit :code 0))
  (error (condition)
    (format *error-output* "Metal pipeline probe failed: ~A~%" condition)
    (finish-output *error-output*)
    (sb-ext:exit :code 1)))
