;;;; Emit the Metal shaders exercised by make msl-validate.

(require :asdf)
(asdf:load-asd (truename "luv.asd"))
(asdf:load-asd (truename "luvcraft.asd"))
(asdf:load-system :luvcraft)

(ensure-directories-exist #p"build/")
(dolist (shader
         (list
          (cons "block-world.vert"
                (luvcraft.shaders:block-world-vertex-specification))
          (cons "block-world.frag"
                (luvcraft.shaders:block-world-fragment-specification))
          (cons "block-world-text.vert"
                (luvcraft.shaders:block-world-text-vertex-specification))
          (cons "block-world-text.frag"
                (luvcraft.shaders:block-world-text-fragment-specification))
          (cons "terminal-cell.vert"
                (luv.shader:shader-specification-for :terminal-cell :vertex))
          (cons "terminal-cell.frag"
                (luv.shader:shader-specification-for :terminal-cell :fragment))
          (cons "terminal-screen.vert"
                (luv.shader:shader-specification-for :terminal-screen :vertex))
          (cons "terminal-screen.frag"
                (luv.shader:shader-specification-for :terminal-screen :fragment))
          (cons "terminal-faceplate.frag"
                (luv.shader:shader-specification-for :terminal-faceplate :fragment))
          (cons "analytic-roundrect.vert"
                (luv.analytic:roundrect-vertex-specification))
          (cons "analytic-roundrect.frag"
                (luv.analytic:roundrect-fragment-specification))
          (cons "slug-bezier.vert"
                (luv.slug:slug-bezier-vertex-specification))
          (cons "slug-bezier.frag"
                (luv.slug:slug-bezier-fragment-specification))
          (cons "mcluv-gradient.vert"
                (mcluv::gradient-roundrect-vertex-specification))
          (cons "mcluv-gradient.frag"
                (mcluv::gradient-roundrect-fragment-specification))
          (cons "mcluv-relief.vert"
                (mcluv::relief-roundrect-vertex-specification))
          (cons "mcluv-relief.frag"
                (mcluv::relief-roundrect-fragment-specification))
          (cons "mcluv-world-relief.vert"
                (mcluv::direct-widget-relief-vertex-specification))
          (cons "mcluv-world-relief.frag"
                (mcluv::relief-roundrect-fragment-specification))
          (cons "mcluv-image.vert"
                (mcluv::image-roundrect-vertex-specification))
          (cons "mcluv-image.frag"
                (mcluv::image-roundrect-fragment-specification))
          (cons "mcluv-compositor.vert"
                (mcluv::spinning-texture-vertex-specification))
          (cons "mcluv-compositor.frag"
                (mcluv::spinning-texture-fragment-specification))
          (cons "mcluv-chassis.vert"
                (mcluv::lisp-machine-chassis-vertex-specification))
          (cons "mcluv-chassis.frag"
                (mcluv::lisp-machine-chassis-fragment-specification))))
  (luv.msl:write-msl
   (luv.msl:compile-msl (cdr shader))
   (merge-pathnames (make-pathname :name (car shader) :type "metal")
                    #p"build/")))
