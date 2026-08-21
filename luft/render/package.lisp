(defpackage #:luft.render.shaders
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step)
  (:export #:face-fragment-specification
           #:face-vertex-specification
           #:inspector-fragment-specification
           #:inspector-vertex-specification
           #:present-fragment-specification
           #:present-vertex-specification))

(defpackage #:luft.render
  (:use #:cl #:luv)
  (:local-nicknames (#:shaders #:luft.render.shaders)
                    (#:vec3 #:luv.arithmetic.lisp.vec3))
  (:export #:face-materialization
           #:scene
           #:scene-solid
           #:make-mountain-sanctuary-scene
           #:face-materialization-domain
           #:face-materialization-negative-count
           #:face-materialization-positive-count
           #:face-materialization-words
           #:make-demo-solid
           #:make-gallery-solid
           #:gallery-plot-report
           #:gallery-plot-origin
           #:*gallery*
           #:make-face-materialization
           #:make-face-materialization-from-surface
           #:renderer
           #:renderer-materialization
           #:fly-camera
           #:make-fly-camera
           #:camera-position
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
           #:site-inspection-shape-word
           #:site-inspection-stock
           #:raycast-site
           #:reset-viewer-camera
           #:start-viewer
           #:stop-viewer
           #:refresh-viewer-renderer
           #:capture-viewer-frame
           #:*chamfer-width*
           #:*wireframe*
           #:*inspection-ink-p*
           #:*inspection-reach*
           #:*projection*
           #:*isometric-height*
           #:viewer
           #:viewer-renderer
           #:viewer-running-p
           #:*viewer*
           #:main
           #:run-standalone-viewer))
