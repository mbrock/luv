(defpackage #:luft.render.shaders
  (:use #:cl #:luv.spir-v)
  (:shadowing-import-from #:luv.spir-v #:step)
  (:documentation
   "Task, mesh, and fragment stages that draw a surface chain of packed sites.")
  (:export #:+brick-size+
           #:+frame-binding+
           #:+sites-binding+
           #:+bricks-binding+
           #:+cells-binding+
           #:*frame-uniform-members*
           #:surface-brick-payload
           #:surface-task-shader
           #:surface-mesh-shader
           #:bevel-mesh-shader
           #:chamfer-mesh-shader
           #:chamfer-fragment-shader
           #:paper-fragment-shader
           #:sky-mesh-shader
           #:sky-fragment-shader
           #:lens-fragment-shader
           #:+scene-binding+
           #:+sampler-binding+
           #:+lens-frame-binding+
           #:surface-fragment-shader
           #:frame-uniform-block))

(defpackage #:luft.render
  (:use #:cl #:luv)
  (:local-nicknames (#:shaders #:luft.render.shaders)
                    (#:vec3 #:luv.arithmetic.lisp.vec3))
  (:documentation
   "A greenfield atelier renderer: a small block world as a boundary chain
of packed LUFT sites, drawn by task and mesh shaders.")
  (:export #:scene
           #:make-scene
           #:make-demo-scene
           #:scene-domain
           #:scene-solid
           #:scene-surface
           #:scene-sites
           #:scene-bricks
           #:scene-brick-count
           #:scene-cell-bits
           #:refresh-scene
           #:fly-camera
           #:make-fly-camera
           #:camera-position
           #:camera-yaw
           #:camera-pitch
           #:camera-field-of-view
           #:camera-basis
           #:frame-uniform-data
           #:renderer
           #:make-renderer
           #:renderer-device
           #:renderer-scene
           #:renderer-camera
           #:renderer-extent
           #:renderer-style
           #:*bevel-radius*
           #:*chamfer-width*
           #:*arris-softness*
           #:*sun-direction*
           #:*sun-color*
           #:*sheen-strength*
           #:*fill-direction*
           #:*fill-strength*
           #:*ambient-light*
           #:*ground-color*
           #:*occlusion-strength*
           #:*shadow-strength*
           #:*top-color*
           #:*side-color*
           #:*bottom-color*
           #:*exposure*
           #:*sky-color*
           #:*draw-sky*
           #:*focus-distance*
           #:*aperture*
           #:*fog-distance*
           #:destroy-renderer
           #:upload-scene
           #:encode-frame
           #:render-pixels
           #:render-to-png
           #:downsample-pixels
           #:capture-demo-png
           #:viewer
           #:start-viewer
           #:stop-viewer
           #:viewer-renderer
           #:*viewer*))
