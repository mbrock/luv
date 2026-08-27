(in-package #:cl-user)

(defpackage #:luv.shader-validation
  (:use #:cl)
  (:export #:validate-production-shaders))
(in-package #:luv.shader-validation)

(defun production-shader-modules ()
  "Return the checked-in application's named SPIR-V module words."
  (flet ((assemble (specification)
           (luv.spir-v:assemble-shader-specification specification)))
    (list
     (cons "block-world.vert.spv"
           (luvcraft.shaders:block-world-vertex-shader))
     (cons "block-world.frag.spv"
           (luvcraft.shaders:block-world-fragment-shader))
     (cons "block-world-crosshair.vert.spv"
           (assemble
            (luvcraft.shaders:block-world-crosshair-vertex-specification)))
     (cons "block-world-crosshair.frag.spv"
           (luvcraft.shaders:block-world-crosshair-fragment-shader))
     (cons "block-world-sky.vert.spv"
           (luvcraft.shaders:block-world-sky-vertex-shader))
     (cons "block-world-sky.frag.spv"
           (luvcraft.shaders:block-world-sky-fragment-shader))
     (cons "block-world-shadow.vert.spv"
           (luvcraft.shaders:block-world-shadow-vertex-shader))
     (cons "block-world-text.vert.spv"
           (assemble
            (luvcraft.shaders:block-world-text-vertex-specification)))
     (cons "block-world-text.frag.spv"
           (assemble
            (luvcraft.shaders:block-world-text-fragment-specification)))
     (cons "terminal-cell.vert.spv"
           (assemble
            (luv.shader:shader-specification-for :terminal-cell :vertex)))
     (cons "terminal-cell.frag.spv"
           (assemble
            (luv.shader:shader-specification-for :terminal-cell :fragment)))
     (cons "video-screen.vert.spv"
           (assemble
            (luv.shader:shader-specification-for :video-screen :vertex)))
     (cons "video-screen.frag.spv"
           (assemble
            (luv.shader:shader-specification-for :video-screen :fragment)))
     (cons "terminal-screen.vert.spv"
           (assemble
            (luv.shader:shader-specification-for :terminal-screen :vertex)))
     (cons "terminal-screen.frag.spv"
           (assemble
            (luv.shader:shader-specification-for :terminal-screen :fragment)))
     (cons "terminal-faceplate.frag.spv"
           (assemble
            (luv.shader:shader-specification-for :terminal-faceplate :fragment)))
     (cons "analytic-roundrect.vert.spv"
           (assemble (luv.analytic:roundrect-vertex-specification)))
     (cons "analytic-roundrect.frag.spv"
           (assemble (luv.analytic:roundrect-fragment-specification)))
     (cons "slug-bezier.vert.spv"
           (assemble (luv.slug:slug-bezier-vertex-specification)))
     (cons "slug-bezier.frag.spv"
           (assemble (luv.slug:slug-bezier-fragment-specification)))
     (cons "mcluv-gradient.vert.spv"
           (assemble (mcluv::gradient-roundrect-vertex-specification)))
     (cons "mcluv-gradient.frag.spv"
           (assemble (mcluv::gradient-roundrect-fragment-specification)))
     (cons "mcluv-relief.vert.spv"
           (assemble (mcluv::relief-roundrect-vertex-specification)))
     (cons "mcluv-relief.frag.spv"
           (assemble (mcluv::relief-roundrect-fragment-specification)))
     (cons "mcluv-world-relief.vert.spv"
           (assemble (mcluv::direct-widget-relief-vertex-specification)))
     (cons "mcluv-world-relief.frag.spv"
           (assemble (mcluv::relief-roundrect-fragment-specification)))
     (cons "mcluv-image.vert.spv"
           (assemble (mcluv::image-roundrect-vertex-specification)))
     (cons "mcluv-image.frag.spv"
           (assemble (mcluv::image-roundrect-fragment-specification)))
     (cons "mcluv-compositor.vert.spv"
           (assemble (mcluv::spinning-texture-vertex-specification)))
     (cons "mcluv-compositor.frag.spv"
           (assemble (mcluv::spinning-texture-fragment-specification)))
     (cons "mcluv-chassis.vert.spv"
           (assemble (mcluv::lisp-machine-chassis-vertex-specification)))
     (cons "mcluv-chassis.frag.spv"
           (assemble (mcluv::lisp-machine-chassis-fragment-specification)))
     (cons "gnome-sdf.vert.spv"
           (assemble (luv.shader:shader-specification-for :gnome-sdf :vertex)))
     (cons "gnome-sdf.frag.spv"
           (assemble (luv.shader:shader-specification-for :gnome-sdf :fragment)))
     (cons "cat-sdf.vert.spv"
           (assemble (luv.shader:shader-specification-for :cat-sdf :vertex)))
     (cons "cat-sdf.frag.spv"
           (assemble (luv.shader:shader-specification-for :cat-sdf :fragment)))
     (cons "player-body-sdf.vert.spv"
           (assemble
            (luv.shader:shader-specification-for :player-body-sdf :vertex)))
     (cons "player-body-sdf.frag.spv"
           (assemble
            (luv.shader:shader-specification-for :player-body-sdf :fragment))))))

(defun validate-production-shaders (&optional (directory #p"build/"))
  "Write and externally validate every production SPIR-V module."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (ensure-directories-exist directory)
    (dolist (module (production-shader-modules))
      (let ((pathname (merge-pathnames (car module) directory)))
        (luv.spir-v:write-spir-v (cdr module) pathname)
        (uiop:run-program
         (list "spirv-val" "--target-env" "vulkan1.0"
               (uiop:native-namestring pathname))
         :output *standard-output* :error-output *error-output*))))
  (format t "shader-validate: all SPIR-V shaders valid.~%")
  (values))
