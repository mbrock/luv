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
           #:capture-luvland-screenshot))
