(defpackage #:luv.wayland
  (:use #:cl)
  (:documentation "A Wayland server over libwayland-server whose every interface
is implemented in Lisp.

The protocol is read from the vendored XML in wayland/protocols/.  One Lisp
dispatcher receives every request on every resource, decodes its arguments
from that description, and calls HANDLE-REQUEST on the resource's Lisp object.
The server owns one thread; all libwayland calls happen there.  It knows
nothing about worlds or GPUs: a host reads committed surface snapshots and
supplies placement, picking, and focus.")
  (:export
   ;; Protocol descriptions.
   #:find-interface
   #:interface-name
   #:interface-version
   #:interface-requests
   #:interface-events
   #:message-name
   #:message-since
   #:message-arguments
   #:argument-name
   #:argument-type
   ;; Server lifecycle.
   #:load-libwayland-server
   #:server
   #:start-server
   #:stop-server
   #:server-running-p
   #:server-socket-name
   #:server-clients
   #:server-surfaces
   #:server-toplevels
   #:call-in-server
   #:server-log
   #:*server*
   ;; Resources.
   #:resource
   #:resource-id
   #:resource-client
   #:resource-version
   #:resource-interface
   #:resource-live-p
   #:handle-request
   #:post-event
   #:client
   #:client-pid
   ;; Surfaces and their contents.
   #:surface
   #:surface-snapshot
   #:surface-role
   #:snapshot
   #:snapshot-width
   #:snapshot-height
   #:snapshot-pixels
   #:snapshot-serial
   #:snapshot-format
   #:toplevel
   #:toplevel-surface
   #:toplevel-title
   #:toplevel-app-id
   #:toplevel-size
   #:toplevel-mapped-p
   #:configure-toplevel
   #:close-toplevel
   #:send-frame-callbacks
   ;; Input.
   #:keyboard-focus
   #:pointer-focus
   #:send-key
   #:send-pointer-motion
   #:send-pointer-button
   #:send-pointer-axis
   #:focus-keyboard
   #:focus-pointer))
