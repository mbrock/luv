(defpackage #:luv.dbus
  (:use #:cl)
  (:documentation "A D-Bus client written in Lisp over a Unix socket.

The wire format is marshalled here rather than through libdbus, so every
message is a Lisp value one can inspect.  A connection owns two threads: one
reads messages and hands replies to the callers waiting for them; the other
runs handlers for incoming method calls and signals, which may themselves
call methods synchronously.

Values: integers for the integer types and handles, T or NIL for booleans,
double-floats, strings for strings, object paths, and signatures; lists for
arrays and structs, except that an array of bytes is an octet vector; a
dictionary is a list of (key . value) conses.  A variant decodes to its value
alone.  To send one, wrap the value with MAKE-VARIANT, or pass a string,
integer, double-float, octet vector, or T, whose signatures are guessed.")
  (:export
   ;; Values.
   #:variant
   #:make-variant
   #:variant-signature
   #:variant-value
   #:parse-signature
   #:marshal-values
   #:unmarshal-values
   ;; Messages.
   #:message
   #:message-type
   #:message-serial
   #:message-path
   #:message-interface
   #:message-member
   #:message-error-name
   #:message-reply-serial
   #:message-destination
   #:message-sender
   #:message-signature
   #:message-body
   #:encode-message
   #:decode-message
   ;; Connections.
   #:bus
   #:open-bus
   #:close-bus
   #:bus-open-p
   #:bus-unique-name
   #:session-bus-address
   #:system-bus-address
   #:call
   #:emit-signal
   #:reply
   #:reply-error
   #:add-handler
   #:remove-handler
   #:add-match
   #:request-name
   #:get-property
   #:get-all-properties
   #:dbus-error
   #:dbus-error-name
   #:dbus-error-message))
