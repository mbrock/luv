(defpackage #:luft.render.shaders
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step)
  (:export #:lattice-point-fragment-specification
           #:lattice-point-vertex-specification
           #:player-sdf-fragment-specification
           #:player-sdf-vertex-specification
           #:mesh-fragment-specification
           #:mesh-vertex-specification
           #:shadow-vertex-specification
           #:present-fragment-specification
           #:present-vertex-specification))

(defpackage #:luft.render
  (:use #:cl #:luv)
  (:local-nicknames (#:domains #:luv.domains)
                    (#:shaders #:luft.render.shaders)
                    (#:production #:luv.production)
                    (#:vec3 #:luv.arithmetic.lisp.vec3))
  (:export #:scene
           #:scene-solid
           #:light
           #:*light*
           #:make-manifold-spike-scene
           #:make-mountain-sanctuary-scene
           #:make-miter-study-scene
           #:make-traveler-study-scene
           #:make-demo-solid
           #:make-gallery-solid
           #:gallery-plot-report
           #:gallery-plot-origin
           #:*gallery*
           #:make-render-mesh
           #:renderer
           #:renderer-set-mesh #:renderer-set-meshes
           #:renderer-remove-mesh #:renderer-clear-meshes
           #:streaming-scene #:make-streaming-scene
           #:advance-streaming-scene #:mesh-streaming-chunk
           #:make-highland-sanctuary-scene
           #:fly-camera
           #:make-fly-camera
           #:camera-position
           #:walking-player
           #:make-walking-player
           #:walking-player-position
           #:viewer-player
           ;; Retained across the live refoundation package reload.
           #:inspection-camera
           #:camera-x
           #:camera-y
           #:camera-z
           #:camera-yaw
           #:camera-pitch
           #:viewer-camera
           #:viewer-inspection
           #:site-inspection
           #:site-inspection-site
           #:site-inspection-cell
           #:site-inspection-point
           #:site-inspection-distance
           #:site-inspection-star-mask
           #:site-inspection-stock
           #:raycast-site
           #:reset-viewer-camera
           #:start-viewer
           #:stop-viewer
           #:refresh-viewer-renderer
           #:capture-viewer-frame
           #:*wireframe*
           #:*inspection-ink-p*
           #:*inspection-reach*
           #:*projection*
           #:*isometric-height*
           #:*character-time*
           #:viewer
           #:viewer-renderer
           #:viewer-running-p
           #:*viewer*
           #:main
           #:run-standalone-viewer))
