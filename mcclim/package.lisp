;;; McCLIM names the logical display connection a port and the relationship
;;; between a sheet and a presentation target a mirror.  Luv's canvas is one
;;; possible presentation target; a texture displayed on a 3D surface will be
;;; another.  Keep those concepts separate even in this first native-window
;;; implementation.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Keep source reloads honest when experimental public names are replaced.
  (let ((package (find-package '#:mcluv)))
    (when package
      (let ((symbol (find-symbol "LUV-MEDIUM" package)))
        (when symbol
          (unexport symbol package))))))

(defpackage #:mcluv
  (:use #:clim-lisp #:clim #:clime #:climb)
  (:local-nicknames (#:luv #:luv)
                    (#:spv #:luv.spir-v)
                    (#:vec #:luv.arithmetic.lisp.vec3))
  (:export #:luv-port
           #:luv-raster-port
           #:luv-gpu-port
           #:luv-frame-manager
           #:luv-graft
           #:luv-mirror
           #:luv-raster-mirror
           #:luv-gpu-mirror
           #:luv-raster-medium
           #:luv-gpu-medium
           #:luv-pointer
           #:mirror-sheet
           #:mirror-target
           #:mirror-context
           #:mirror-embedded-p
           #:mirror-texture
           #:mirror-compositor
           #:port-mirrors
           #:present-mirror
           #:repaint-gpu-mirror
           ;; Mounting a McCLIM pane inside the game: the overlay base class,
           ;; the specials that make a frame share luvcraft's one canvas
           ;; instead of asking for a second, and the compositor a HUD panel
           ;; draws itself with.
           #:luvcraft-widget-overlay
           #:luvcraft-direct-widget-overlay
           #:luvcraft-world-widget-overlay
           #:luvcraft-hud-widget-overlay
           #:widget-overlay-session
           #:widget-overlay-frame
           #:widget-overlay-mirror
           #:widget-overlay-render-state
           #:widget-overlay-logical-size
           #:prepare-direct-widget-overlay
           #:luvcraft-widget-texture-coordinate
           #:*embedded-mirror-target*
           #:*embedded-mirror-context*
           #:*embedded-mirror-device*
           #:ensure-spinning-compositor-resources
           #:ensure-spinning-compositor-frame-state
           #:spinning-compositor-pipeline
           #:spinning-frame-state-buffer
           #:spinning-frame-state-bind-group
           #:widget-lab
           #:widget-lab-click-count
           #:widget-lab-toggle-value
           #:open-widget-lab
           #:close-widget-lab
           #:shader-lab
           #:shader-lab-lowering
           #:shader-lab-selection
           #:shader-lab-definitions
           #:shader-lab-specifications
           #:shader-lab-materials
           #:shader-lab-process
           #:shader-lab-last-health-report
           #:shader-lab-health
           #:shader-lab-health-report
           #:shader-lab-health-report-status
           #:shader-lab-health-report-frame-state
           #:shader-lab-health-report-process-alive-p
           #:shader-lab-health-report-mirror-count
           #:shader-lab-health-report-canvas-state
           #:shader-lab-health-report-latency
           #:shader-lab-health-report-problems
           #:shader-lab-health-report-backtrace
           #:refresh-shader-lab
           #:capture-shader-lab-screenshot
           #:capture-default-shader-lab-screenshot
           #:capture-gpu-mirror-screenshot
           #:draw-analytic-rounded-rectangle*
           #:draw-lattice*
           #:linear-gradient
           #:radial-gradient
           #:make-linear-gradient
           #:make-radial-gradient
           #:relief-design
           #:make-relief-design
           #:relief-albedo
           #:relief-height
           #:design-height
           #:clear-gpu-medium-fallback-statistics
           #:gpu-medium-fallback-report
           #:capture-mcclim-gallery
           #:*mcclim-gallery-scenes*
           #:run-roundrect-tracy-benchmark
           #:run-paint-tracy-benchmark
           #:open-shader-lab
           #:close-shader-lab
           #:spinning-texture-compositor
           #:enable-spinning-mirror
           #:disable-spinning-mirror
           #:open-spinning-widget-lab
           #:luvcraft-widget-overlay
           #:luvcraft-hotbar-overlay
           #:luvcraft-inventory-overlay
           #:terminal-film-browser
           #:open-terminal-film-browser
           #:open-luvcraft-hotbar
           #:close-luvcraft-hotbar
           #:open-luvcraft-inventory
           #:close-luvcraft-inventory
           #:open-luvcraft-widget-lab
           #:close-luvcraft-widget-lab
           #:surveyor-map
           #:surveyor-map-mode
           #:surveyor-map-snapshot
           #:open-surveyor-map
           #:close-surveyor-map
           #:open-luvcraft-surveyor-map
           #:close-luvcraft-surveyor-map))

(in-package #:mcluv)
