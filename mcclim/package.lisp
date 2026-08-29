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
                    (#:build #:luv.build)
                    (#:shader #:luv.shader)
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
           #:direct-gpu-mirror-compositor
           #:encode-direct-gpu-mirror
           #:evict-direct-gpu-mirror-frame-key
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
           #:prepare-gpu-mirror-compositor
           #:dispatch-embedded-mirror-event
           #:present-gpu-mirror-sheet
           #:call-with-gpu-mirror-sheet-repaint
           #:transparent-gpu-application-pane
           #:transparent-gpu-top-level-sheet-pane
           #:make-gpu-frame-background-transparent
           ;; Shared retained-GPU M-x instrument.
           #:command-menu
           #:command-menu-owner-frame
           #:command-menu-command-tables
           #:command-menu-entries
           #:command-menu-results
           #:command-menu-query
           #:command-menu-selected
           #:command-menu-entry
           #:command-menu-entry-label
           #:command-menu-entry-command-name
           #:command-menu-entry-table
           #:command-menu-tables-for
           #:command-menu-entries-for-tables
           #:matching-command-menu-entries
           #:refresh-command-menu-entries
           #:command-menu-visible-results
           #:command-menu-selected-command
           #:command-menu-mirror
           #:repaint-command-menu
           #:prepare-command-menu
           #:command-menu-screen-state
           #:command-menu-local-coordinate
           #:handle-command-menu-key-event
           #:handle-command-menu-pointer-press
           #:execute-command-menu-command
           #:make-embedded-command-menu
           #:destroy-command-menu
           ;; Shared reviewed Git fast-forward and live ASDF load panel.
           #:source-update
           #:source-update-snapshot
           #:source-update-snapshot-state
           #:source-update-snapshot-heading
           #:source-update-snapshot-lines
           #:source-update-snapshot-footer
           #:source-update-frame-session
           #:source-update-root-for
           #:source-update-systems-for
           #:source-update-title-for
           #:source-update-build-run
           #:make-source-update-session
           #:start-source-update
           #:current-source-update-snapshot
           #:request-source-update-fetch
           #:request-source-update-apply
           #:request-source-update-retry
           #:source-update-busy-p
           #:wait-source-update-session
           #:quiesce-source-update-session
           #:refresh-source-update
           #:prepare-source-update
           #:source-update-mirror
           #:source-update-screen-state
           #:source-update-local-coordinate
           #:handle-source-update-key-event
           #:handle-source-update-pointer-press
           #:make-embedded-source-update
           #:destroy-source-update
           #:luvcraft-source-update-overlay
           #:find-luvcraft-source-update
           #:open-luvcraft-source-update
           #:close-luvcraft-source-update
           ;; Shared retained-GPU metabar instrument and its open semantic
           ;; application protocol.
           #:metabar
           #:metabar-owner
           #:metabar-logical-height
           #:metabar-diagnostic
           #:metabar-groups-for
           #:metabar-group-label
           #:metabar-controls-for
           #:metabar-actions-for
           #:metabar-control-kind
           #:metabar-control-label
           #:metabar-control-value
           #:metabar-control-value-label
           #:metabar-control-fraction
           #:metabar-control-change-kind
           #:metabar-control-update-policy
           #:perform-metabar-control-step
           #:perform-metabar-control-set-fraction
           #:perform-metabar-control-toggle
           #:metabar-action-label
           #:perform-metabar-action
           #:refresh-metabar-vocabulary
           #:refresh-metabar-state
           #:drain-metabar-operations
           #:prepare-metabar
           #:repaint-metabar
           #:metabar-mirror
           #:metabar-screen-state
           #:metabar-local-coordinate
           #:handle-metabar-key-event
           #:handle-metabar-pointer-event
           #:make-embedded-metabar
           #:destroy-metabar
           #:validate-metabar-direct-presentation
           #:metabar-requires-direct-gpu
           #:metabar-direct-presentation-violation
           ;; The always-available application status line.  Base channels
           ;; are shared while applications extend the ordered vocabulary by
           ;; specializing the ordinary CLOS protocol below.
           #:status-bar
           #:status-bar-owner
           #:status-bar-logical-width
           #:status-bar-visible-fields
           #:status-bar-field
           #:status-bar-field-channel
           #:status-bar-field-label
           #:status-bar-field-value
           #:status-bar-channels-for
           #:status-bar-channel-label
           #:status-bar-channel-value
           #:status-bar-application-name
           #:status-bar-lobby-client
           #:status-bar-source-root
           #:status-bar-worktree-description
           #:refresh-status-bar
           #:prepare-status-bar
           #:repaint-status-bar
           #:status-bar-mirror
           #:status-bar-screen-state
           #:resize-status-bar
           #:make-embedded-status-bar
           #:destroy-status-bar
           #:validate-status-bar-direct-presentation
           #:status-bar-requires-direct-gpu
           #:status-bar-direct-presentation-violation
           ;; Thin Luvcraft application adapters for shared global HUDs.
           #:luvcraft-status-bar-overlay
           #:find-luvcraft-status-bar
           #:open-luvcraft-status-bar
           #:luvcraft-lobby-hud-overlay
           #:open-luvcraft-lobby-hud
           #:close-luvcraft-lobby-hud
           #:toggle-luvcraft-lobby-hud
           ;; Portable luv key events as ordinary McCLIM gestures and
           ;; commands. Applications keep their command tables and dispatch
           ;; policy; this package owns only the event-vocabulary bridge.
           #:canvas-key-event-matches-gesture-p
           #:canvas-key-event-command
           #:execute-canvas-key-event-command
           #:format-gesture
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
           #:workbench-backend-proof
           #:workbench-backend-proof-mirror
           #:workbench-backend-proof-log
           #:make-embedded-workbench-backend-proof
           #:destroy-workbench-backend-proof
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
