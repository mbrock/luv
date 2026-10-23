(defpackage #:luft.world
  (:use #:cl #:luv #:luft.terrain)
  (:import-from #:luv.arithmetic.lisp.vec3
                #:vec3 #:make-vec3
                #:vec3-x #:vec3-y #:vec3-z #:vec3-component
                #:vec3-cross #:vec3-dot #:vec3-length
                #:vec3-normalize #:vec3-scale)
  (:import-from #:alexandria #:when-let)
  (:local-nicknames (#:domains #:luv.domains)
                    (#:production #:luv.production)
                    (#:quantities #:luft.quantities)
                    (#:math #:luv.arithmetic))
  (:documentation
   "The Luft atelier's world as terrain content: its palette, its landscapes
and alpine large world, and its authored fixture scenes.  Another game on
the terrain brings its own.")
  (:export
           #:*alpine-beach-level*
           #:*alpine-snow-line*
           #:*alpine-tree-line*
           #:*beacon-material-placement*
           #:*crystal-material*
           #:*crystal-material-placement*
           #:*earth-material*
           #:*gallery*
           #:*gallery-columns*
           #:*gallery-pitch*
           #:*highland-rock-material*
           #:*highland-rock-material-placement*
           #:*large-world-relief-height*
           #:*large-world-scenic-spawns*
           #:*large-world-spawn-clearing*
           #:*large-world-spawn-summit-distance*
           #:*large-world-spawn-view-distance*
           #:*limestone-material*
           #:*sanctuary-beacon-x*
           #:*sanctuary-beacon-y*
           #:*sanctuary-material-placement*
           #:*sand-material*
           #:*sand-material-placement*
           #:*slate-material*
           #:*slate-material-placement*
           #:*snow-material*
           #:*snow-material-placement*
           #:*terminal-material*
           #:*terminal-material-placement*
           #:*terrain-material-placement*
           #:*torch-flame-material*
           #:+large-world-horizontal-bits+
           #:+large-world-seed+
           #:+large-world-spawn-x+
           #:+sanctuary-origin-x+
           #:+sanctuary-origin-y+
           #:+sanctuary-plateau-height+
           #:alpine-landscape-relief
           #:authored-world-source
           #:authored-world-source-seed
           #:build-highland-lookout
           #:gallery-plot-origin
           #:gallery-plot-report
           #:highland-landscape-height
           #:interpolate-landscape-reading
           #:landscape-hash-reading
           #:landscape-ramp
           #:landscape-value-noise
           #:large-world-base-placement
           #:large-world-citadel-cell-p
           #:large-world-column-placement
           #:large-world-river-centre-x
           #:large-world-road-centre-y
           #:large-world-road-height
           #:large-world-scenic-spawn
           #:large-world-spawn-view
           #:large-world-summit-visible-p
           #:large-world-surface-slope
           #:large-world-terrain-height
           #:make-authored-world-streaming-scene
           #:make-bevel-limit-study-scene
           #:make-demo-solid
           #:make-gallery-solid
           #:make-highland-sanctuary-scene
           #:make-manifold-spike-scene
           #:make-miter-study-scene
           #:make-mountain-sanctuary-scene
           #:make-scene-material-vocabulary
           #:make-traveler-study-scene
           #:make-voxel-light-shrine-scene
           #:make-world-scene-builder
           #:mountain-sanctuary-terrain-height
           #:mountain-sanctuary-terrain-x-bounds
           #:scene-builder-mountain-border-wall
           #:scene-builder-torch
           #:smooth-landscape-reading))
