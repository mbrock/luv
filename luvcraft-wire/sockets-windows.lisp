;;;; The socket transport is POSIX: poll(2) beside a wake pipe.  On Windows
;;;; only the loopback network works for now, and opening sockets says so.

(in-package #:luvcraft.wire.net)

(defclass socket-transport (transport) ()
  (:default-initargs :port +default-port+))

(defmethod initialize-instance :after
    ((transport socket-transport) &key port announce announce-port
                                       broadcast-addresses connect-timeout)
  (declare (ignore port announce announce-port broadcast-addresses connect-timeout)))

(defmethod transport-open ((transport socket-transport) endpoint)
  (declare (ignore endpoint))
  (error "LAN play needs the POSIX socket transport, which Windows lacks; ~
only the loopback network works here."))
