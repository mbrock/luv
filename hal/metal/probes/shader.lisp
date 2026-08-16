;;;; Cold-process acceptance probe for device-owned Metal 4 MSL compilation.

(require :asdf)

(asdf:load-asd (truename (merge-pathnames #P"../../../luv.asd" *load-truename*)))
(asdf:load-asd (truename (merge-pathnames #P"../../../luvcraft.asd" *load-truename*)))
(asdf:load-system :luvcraft)

(handler-case
    (progn
      (format t "~S~%"
              (luv:probe-metal-shader-library
               (luvcraft.shaders:block-world-fragment-specification)))
      (finish-output)
      (sb-ext:exit :code 0))
  (error (condition)
    (format *error-output* "Metal shader probe failed: ~A~%" condition)
    (finish-output *error-output*)
    (sb-ext:exit :code 1)))
