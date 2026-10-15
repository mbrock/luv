(defpackage #:luvland
  (:use #:cl)
  (:local-nicknames (#:wl #:luv.wayland)
                    (#:shader #:luv.shader))
  (:documentation "Luvland: a Wayland atelier whose only world is windows.

Client surfaces are textured quads in a perspective space, laid out as a
strip the camera travels along, as niri lays out columns.  The protocol lives
in LUV.WAYLAND; this package supplies placement, picking, and focus.")
  (:export #:*luvland*
           #:start-luvland
           #:stop-luvland
           #:luvland-canvas
           #:luvland-server
           #:spawn-client
           #:focus-window
           #:luvland-windows
           #:capture-luvland-screenshot
           ;; Ratpoison's vocabulary, for the init file and the eval prompt.
           #:kbd
           #:define-key
           #:lookup-key
           #:make-sparse-keymap
           #:*top-map*
           #:*root-map*
           #:*prefix-key*
           #:set-prefix-key
           #:define-luvland-command
           #:run-command
           #:message
           #:*terminal*
           #:*window-margin*
           #:*border-width*
           #:*corner-radius*
           #:*shadow-blur*
           #:*shadow-alpha*
           #:*focused-border-color*
           #:*border-color*
           #:*overview-zoom*
           #:*cursor-size*
           #:*cursor-color*
           #:main))
