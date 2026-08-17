;;;; Opt-in checks against a real Telegram data centre.
;;;;
;;;; Deliberately not part of the test suite: they need the network, they
;;;; talk to someone else's production servers, and the first one creates a
;;;; real authorization key.  Load the TELEGRAM/LIVE system and call them by
;;;; hand.

(defpackage #:telegram.live
  (:use #:cl)
  (:local-nicknames (#:octets #:telegram.octets)
                    (#:tl #:telegram.tl)
                    (#:mt #:telegram)
                    (#:net #:telegram.net)
                    (#:client #:telegram.client))
  (:documentation
   "Checks that talk to Telegram itself.  LIVE-HANDSHAKE-CHECK needs nothing
but a network; LIVE-API-CHECK needs a TELEGRAM_API_ID, and reports what the
server said either way.")
  (:export #:live-handshake-check
           #:live-api-check))

(in-package #:telegram.live)

(defun live-handshake-check (&key (dc-id 2) test (ping-id 1)
                                  (transport (make-instance
                                              'mt:abridged-transport))
                                  (stream *standard-output*))
  "Connect to a Telegram data centre, create an authorization key, open a
session over it, and ping.  Returns the AUTH-KEY-MATERIAL, which is a real
credential: it identifies this client to that data centre until it is
discarded.

  (telegram.live:live-handshake-check :dc-id 2)"
  (net:with-mtproto-connection (connection :dc-id dc-id :test test
                                           :transport transport)
    (format stream "~&connected to ~A~%" connection)
    (let ((material (net:create-auth-key connection)))
      (format stream "~&authorization key ~A~%~
                        server salt ~D, clock offset ~Ds~%"
              (octets:octets-hex
               (mt:auth-key-id (mt:auth-key-material-key material)))
              (mt:auth-key-material-server-salt material)
              (mt:auth-key-material-time-offset material))
      (net:establish-session connection material)
      (format stream "~&pong ~S~%" (net:connection-ping connection
                                                        :ping-id ping-id))
      material)))

(defun live-api-check (&key (dc-id 2) test
                            (application (client:application-from-environment))
                            (stream *standard-output*))
  "Create a key, then call help.getConfig and help.getNearestDc over it.

This is the check that exercises everything at once: the handshake, the
session, initConnection and invokeWithLayer, the generated schema, and gzip,
since a Config is large enough that Telegram compresses it.

Needs TELEGRAM_API_ID in the environment.  Telegram issues those per
developer at https://my.telegram.org/apps; the authorization key itself needs
no credentials, which is why the handshake check does not ask for any."
  (let ((client:*application* application))
    (net:with-mtproto-connection (connection :dc-id dc-id :test test)
      (format stream "~&connected to ~A as ~A~%" connection application)
      (net:establish-session connection (net:create-auth-key connection))
      (let ((config (client:invoke connection :help.get-config)))
        (format stream "~&~A: this is dc~D, ~D data centre~:P known~%"
                (tl:tl-name config)
                (tl:tl-value config :this-dc)
                (length (tl:tl-value config :dc-options)))
        (loop for option across (tl:tl-value config :dc-options)
              repeat 4
              do (format stream "~&    dc~D ~A:~D~@[ ipv6~]~@[ media~]~%"
                         (tl:tl-value option :id)
                         (tl:tl-value option :ip-address)
                         (tl:tl-value option :port)
                         (tl:tl-value option :ipv6)
                         (tl:tl-value option :media-only))))
      (let ((nearest (client:invoke connection :help.get-nearest-dc)))
        (format stream "~&~A: country ~S, nearest dc~D~%"
                (tl:tl-name nearest)
                (tl:tl-value nearest :country)
                (tl:tl-value nearest :nearest-dc))
        nearest))))
