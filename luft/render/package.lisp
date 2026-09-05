(defpackage #:luft.render.quantities
  (:use #:cl)
  (:import-from #:luv.arithmetic
                #:define-quantity-kind
                #:define-unit
                #:define-quantity)
  (:export #:spatial-coordinate
           #:unit-direction
           #:orientation-vector
           #:normalized-coordinate
           #:relative-color-signal
           #:control-signal
           #:sample-count
           #:cell
           #:world-position
           #:world-x-position
           #:world-y-position
           #:world-z-position
           #:world-velocity
           #:world-acceleration
           #:world-direction
           #:world-x-direction
           #:world-y-direction
           #:world-z-direction
           #:world-orientation
           #:world-x-orientation
           #:world-y-orientation
           #:world-z-orientation
           #:horizontal-direction
           #:horizontal-x-direction
           #:horizontal-y-direction
           #:world-distance
           #:spatial-scale
           #:gait-phase
           #:texture-coordinate
           #:texture-u-coordinate
           #:texture-v-coordinate
           #:temporal-jitter
           #:temporal-x-jitter
           #:temporal-y-jitter
           #:texel-extent
           #:texel-width
           #:texel-height
           #:shadow-coordinate
           #:shadow-u-coordinate
           #:shadow-v-coordinate
           #:shadow-depth-coordinate
           #:shadow-bias
           #:shadow-filter-radius
           #:bevel-proportion
           #:construction-line-strength
           #:inspection-ink-strength
           #:elapsed-time
           #:scene-radiance
           #:scene-red-radiance
           #:scene-green-radiance
           #:scene-blue-radiance
           #:scene-luminance
           #:exposure
           #:presented-color
           #:presented-red-color
           #:presented-green-color
           #:presented-blue-color)
  (:documentation
   "Collision-safe semantic quantity vocabulary for the LUFT renderer."))

(defpackage #:luft.render.shaders
  (:use #:cl #:luv.shader)
  (:shadowing-import-from #:luv.shader #:step)
  (:local-nicknames (#:quantities #:luft.render.quantities)
                    (#:math #:luv.arithmetic))
  (:export #:write-production-spir-v
           #:*production-shader-specifications*
           #:lattice-point-fragment-specification
           #:lattice-point-vertex-specification
           #:player-sdf-fragment-specification
           #:player-sdf-vertex-specification
           #:torch-flame-fragment-specification
           #:torch-flame-vertex-specification
           #:torch-body-vertex-specification
           #:torch-body-shadow-vertex-specification
           #:torch-body-fragment-specification
           #:star-fragment-specification
           #:terrain-mesh-specification
           #:terrain-shadow-mesh-specification
           #:sky-fragment-specification
           #:sky-temporal-fragment-specification
           #:temporal-resolve-fragment-specification
           #:exposure-probe-fragment-specification
           #:present-fragment-specification
           #:present-vertex-specification))

(defpackage #:luft.render
  (:use #:cl #:luv)
  ;; Import the everyday vocabulary; keep prefixes for semantic quantities
  ;; and neighboring subsystems. These are the original symbols, not wrappers.
  (:import-from #:luv.arithmetic.lisp.vec3
                #:vec3 #:make-vec3
                #:vec3-x #:vec3-y #:vec3-z #:vec3-component
                #:vec3-cross #:vec3-dot #:vec3-length
                #:vec3-normalize #:vec3-scale)
  (:import-from #:alexandria #:when-let)
  (:import-from #:luv.arithmetic #:define-quantity-constant)
  (:import-from #:luv.arithmetic.lisp
                #:define-lisp-arithmetic-function
                #:make-lisp-arithmetic-realization
                #:bind-lisp-arithmetic-realization)
  (:import-from #:luv.arithmetic.language #:interpret)
  (:import-from #:luv.arithmetic.records #:quantity-class #:record-slot-declaration)
  (:local-nicknames (#:domains #:luv.domains)
                    (#:shaders #:luft.render.shaders)
                    (#:production #:luv.production)
                    (#:quantities #:luft.render.quantities)
                    (#:math #:luv.arithmetic))
  (:export #:drawing-program
           #:make-drawing-program
           #:program-inputs
           #:make-program-binding
           #:encode-program
           #:release-program
           #:terrain-drawing #:make-terrain-drawing #:make-terrain-binding #:encode-terrain
           #:sun-shadow #:make-sun-shadow #:sun-shadow-texture #:sun-shadow-view #:sun-shadow-sampler
           #:make-lattice-drawing
           #:image-finishing #:make-image-finishing #:finishing-sampler
           #:make-composite-binding #:make-presentation-binding #:encode-composite #:encode-presentation
           #:reconstruction #:make-reconstruction #:reconstruction-kind
           #:make-reconstruction-binding #:encode-reconstruction-resolve
           #:scene
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
           #:exposure-control
           #:make-fixed-exposure #:make-automatic-exposure
           #:exposure-value #:advance-exposure
           #:make-exposure-binding #:encode-exposure #:release-exposure
           #:scene-drawing #:make-sky-drawing #:make-player-drawing
           #:make-scene-drawing-binding #:encode-scene-drawing
           #:release-scene-drawing
           #:torch-drawing #:make-framed-torch-drawing
           #:make-torch-frame-buffer #:upload-torch-frame
           #:make-torch-body-binding #:make-torch-flame-binding
           #:encode-torch-bodies #:encode-torch-flames #:release-torch-drawing
           #:renderer #:make-renderer #:destroy-renderer
           #:renderer-set-mesh #:renderer-set-meshes
           #:renderer-remove-mesh #:renderer-clear-meshes
           #:streaming-scene #:make-streaming-scene
           #:make-authored-world-streaming-scene
           #:streaming-scene-light-generation
           #:scene-edit
           #:scene-edit-cell
           #:scene-edit-old-placement
           #:scene-edit-new-placement
           #:scene-edit-content-revision
           #:edit-streaming-scene-cell
           #:mesh-streaming-chunk
           #:make-highland-sanctuary-scene
           #:fly-camera
           #:make-fly-camera
           #:camera-position
           #:physical-body
           #:body-position
           #:body-previous-position
           #:body-velocity
           #:body-height
           #:body-radius
           #:body-gravity
           #:body-grounded-p
           #:walking-character
           #:make-walking-character
           #:character-body
           #:character-controller
           #:character-speed
           #:character-ground-acceleration
           #:character-air-acceleration
           #:character-jump-speed
           #:movement-controller
           #:movement-intent
           #:controller-desire
           #:set-character-movement
           #:request-character-jump
           #:world-simulation
           #:make-world-simulation
           #:simulation-source
           #:simulation-bodies
           #:simulation-characters
           #:simulation-clock
           #:add-simulation-body
           #:add-simulation-character
           #:remove-simulation-character
           #:advance-world-simulation
           #:walking-route
           #:walking-route-start
           #:walking-route-destination
           #:walking-route-cells
           #:walking-route-status
           #:walking-route-detail
           #:walking-route-visits
           #:start-character-route
           #:cancel-character-route
           #:viewer-player
           #:viewer-simulation
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
           #:world-edit-mode
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
           #:open-viewer-command-menu
           #:close-viewer-command-menu
           #:toggle-viewer-command-menu
           #:open-viewer-source-update
           #:close-viewer-source-update
           #:open-viewer-metabar
           #:close-viewer-metabar
           #:toggle-viewer-metabar
           #:open-viewer-lobby
           #:close-viewer-lobby
           #:toggle-viewer-lobby
           #:viewer-lobby-client
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
