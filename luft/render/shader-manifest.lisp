(in-package #:luft.render.shaders)

;;; Production module manifest ----------------------------------------------

(defparameter *production-shader-specifications*
  '(mesh-vertex-specification
    star-fragment-specification
    shadow-vertex-specification
    player-sdf-vertex-specification
    player-sdf-fragment-specification
    lattice-point-vertex-specification
    lattice-point-fragment-specification
    present-vertex-specification
    present-fragment-specification
    sky-fragment-specification
    sky-temporal-fragment-specification
    exposure-probe-fragment-specification
    temporal-resolve-fragment-specification
    torch-body-vertex-specification
    torch-body-fragment-specification
    torch-body-shadow-vertex-specification
    torch-flame-vertex-specification
    torch-flame-fragment-specification
    torch-flame-composite-copy-fragment-specification)
  "Every production LUFT shader specification, in stable emission order.")

(defun write-production-spir-v (&optional (directory #p"build/"))
  "Assemble every production LUFT shader into DIRECTORY as luft-*.spv.

This is the byte-identity oracle for shader-source migrations: emit before,
emit after, and compare hashes.  Returns the written pathnames."
  (loop for name in *production-shader-specifications*
        for specification = (funcall name)
        for path = (merge-pathnames
                    (format nil "luft-~(~a~).spv" name) directory)
        do (luv.spir-v:write-spir-v
            (luv.spir-v:assemble-shader-specification specification)
            path)
        collect path))
