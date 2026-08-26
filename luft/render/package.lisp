(defpackage #:luft.render.shaders
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step)
  (:export #:lattice-point-fragment-specification
           #:lattice-point-vertex-specification
           #:player-sdf-fragment-specification
           #:player-sdf-vertex-specification
           #:torch-flame-fragment-specification
           #:torch-flame-vertex-specification
           #:torch-body-vertex-specification
           #:torch-body-shadow-vertex-specification
           #:mesh-fragment-specification
           #:mesh-vertex-specification
           #:shadow-vertex-specification
           #:sky-fragment-specification
           #:sky-temporal-fragment-specification
           #:temporal-resolve-fragment-specification
           #:exposure-probe-fragment-specification
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
           #:scene-authored-voxel-light
           #:scene-voxel-light
           #:scene-voxel-light-propagation-p
           #:scene-torches
           #:scene-mesh-generation
           #:scene-mesh-generation-light-generation
           #:scene-mesh-generation-request-stamp
           #:scene-mesh-generation-result-stamp
           #:+torch-flame-instance-row-count+
           #:+torch-flame-instance-scalar-count+
           #:+torch-flame-sample-count+
           #:pack-torch-flame-frame
           #:validate-torch-flame-frame
           #:unpack-torch-flame-frame
           #:pack-torch-body-frame-flags
           #:unpack-torch-body-frame-flags
           #:+torch-body-vertex-row-count+
           #:+torch-body-vertex-scalar-count+
           #:torch-body-vertex-data
           #:torch-body-vertex-count
           #:torch-body-reference-vertex
           #:torch-flame-effect-uniform-data
           #:torch-flame-reference-signed-distance
           #:torch-flame-reference-density
           #:torch-flame-reference-integrate-ray
           #:*flame-time*
           #:light
           #:*light*
           #:make-bevel-limit-study-scene
           #:make-manifold-spike-scene
           #:make-mountain-sanctuary-scene
           #:make-material-bevel-transition-study-scene
           #:make-miter-study-scene
           #:make-traveler-study-scene
           #:make-voxel-light-shrine-scene
           #:make-demo-solid
           #:make-gallery-solid
           #:gallery-plot-report
           #:gallery-plot-origin
           #:*gallery*
           #:make-render-mesh
           #:make-whole-domain-diagnostic-mesh
           #:scene-builder-torch
           #:material-bevel-profile #:make-material-bevel-profile
           #:material-bevel-width #:compile-material-bevel-profile
           #:make-material-bevel-mesh
           #:make-uncontracted-material-bevel-diagnostic-mesh
           #:make-material-bevel-meshes
           #:renderer
           #:renderer-set-mesh #:renderer-set-meshes
           #:renderer-remove-mesh #:renderer-clear-meshes
           #:streaming-scene #:make-streaming-scene
           #:streaming-scene-light-generation
           #:mesh-streaming-chunk
           #:make-highland-sanctuary-scene
           #:fly-camera
           #:make-fly-camera
           #:camera-position
           #:walking-player
           #:make-walking-player
           #:walking-player-position
           #:walking-player-route
           #:walking-route
           #:walking-route-start
           #:walking-route-destination
           #:walking-route-cells
           #:walking-route-status
           #:walking-route-detail
           #:walking-route-visits
           #:start-walking-player-route
           #:viewer-player
           ;; Retained across the live refoundation package reload.
           #:inspection-camera
           #:camera-x
           #:camera-y
           #:camera-z
           #:camera-yaw
           #:camera-pitch
           #:viewer-camera
           #:viewer-mode
           #:isometric-walk-mode
           #:orbit-mode
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
           #:refresh-viewer-shaders
           #:viewer-live-artifact
           #:capture-viewer-frame
           #:film-viewer
           #:*wireframe*
           #:*inspection-ink-p*
           #:*inspection-reach*
           #:*projection*
           #:*isometric-height*
           #:*character-time*
           #:viewer
           #:viewer-renderer
           #:viewer-shader-diagnostic
           #:viewer-running-p
           ;; Ordered application instruments.  Each shared tool owns its UI
           ;; implementation; LUFT supplies only attachment, frame-boundary,
           ;; final-pass, input, and release policy through this protocol.
           #:viewer-instruments
           #:add-viewer-instrument
           #:remove-viewer-instrument
           #:viewer-instrument-priority
           #:viewer-instrument-present-p
           #:refresh-viewer-instrument
           #:encode-viewer-instrument
           #:handle-viewer-instrument-event
           #:release-viewer-instrument
           #:open-viewer-command-menu
           #:close-viewer-command-menu
           #:toggle-viewer-command-menu
           #:open-viewer-metabar
           #:close-viewer-metabar
           #:toggle-viewer-metabar
           #:open-viewer-lobby
           #:close-viewer-lobby
           #:toggle-viewer-lobby
           #:viewer-lobby-client
           #:viewer-status-bar
           #:open-viewer-status-bar
           #:start-viewer-tracy-capture
           #:stop-viewer-tracy-capture
           #:toggle-viewer-tracy-capture
           #:open-viewer-tracy-capture
           #:reveal-viewer-tracy-capture
           #:viewer-agent
           #:make-viewer-agent
           #:ask-viewer-agent
           #:release-viewer-agent
           #:viewer-agent-report
           #:*viewer*
           #:main
           #:run-standalone-viewer))
