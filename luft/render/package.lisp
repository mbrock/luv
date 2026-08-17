(defpackage #:luft.render.shaders
  (:use #:cl #:luv.spir-v)
  (:shadowing-import-from #:luv.spir-v #:step)
  (:documentation
   "Task, mesh, and fragment stages that draw a surface chain of packed sites.")
  (:export #:+brick-size+
           #:+frame-binding+
           #:+terms-binding+
           #:+bricks-binding+
           #:+cells-binding+
           #:*frame-uniform-members*
           #:surface-brick-payload
           #:surface-task-shader
           #:surface-mesh-shader
           #:bevel-mesh-shader
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
           #:scene-terms
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
           #:destroy-renderer
           #:upload-scene
           #:encode-frame
           #:render-pixels
           #:render-to-png
           #:capture-demo-png
           #:viewer
           #:start-viewer
           #:stop-viewer
           #:viewer-renderer
           #:*viewer*))
