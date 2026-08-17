;;;; An opt-in check against a real Telegram data centre.
;;;;
;;;; This is deliberately not part of the test suite: it needs the network,
;;;; it talks to someone else's production servers, and it creates a real
;;;; authorization key.  Load the TELEGRAM/LIVE system and call it by hand.

(in-package #:telegram.net)

(export '(live-handshake-check))

(defun live-handshake-check (&key (dc-id 2) test (ping-id 1)
                                  (transport (make-instance
                                              'mt:abridged-transport))
                                  (stream *standard-output*))
  "Connect to a Telegram data centre, create an authorization key, open a
session over it, and ping.  Returns the AUTH-KEY-MATERIAL, which is a real
credential: it identifies this client to that data centre until it is
discarded.

  (telegram.net:live-handshake-check :dc-id 2)"
  (with-mtproto-connection (connection :dc-id dc-id :test test
                                       :transport transport)
    (format stream "~&connected to ~A~%" connection)
    (let ((material (create-auth-key connection)))
      (format stream "~&authorization key ~A~%~
                        server salt ~D, clock offset ~Ds~%"
              (octets:octets-hex
               (mt:auth-key-id (mt:auth-key-material-key material)))
              (mt:auth-key-material-server-salt material)
              (mt:auth-key-material-time-offset material))
      (establish-session connection material)
      (let ((pong (connection-ping connection :ping-id ping-id)))
        (format stream "~&pong ~S~%" pong))
      material)))
