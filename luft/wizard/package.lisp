(defpackage #:luft.wizard
  (:use #:cl #:luv)
  (:import-from #:luv.arithmetic.lisp.vec3
                #:make-vec3 #:vec3-x #:vec3-y #:vec3-z)
  (:local-nicknames (#:terrain #:luft.terrain)
                    (#:render #:luft.render)
                    (#:domains #:luv.domains)
                    (#:production #:luv.production))
  (:documentation
   "The isometric wizard: a small game on the Luft engine alone.  It brings
its own palette, world source, character, isometric camera, and
click-to-walk, and uses the terrain, the simulation, and the renderer
through their protocols -- no atelier, no Luft world (#BJRZW2).")
  (:export #:*wizard*
           #:wizard
           #:make-wizard-palette
           #:wizard-world-source
           #:make-wizard-world-source
           #:wizard-ground-height
           #:wizard-path-centre-x
           #:start-wizard
           #:stop-wizard
           #:wizard-scene
           #:wizard-character
           #:wizard-camera
           #:wizard-pointer-ray
           #:wizard-click
           #:wizard-resident-p
           #:capture-wizard-frame))
