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
           #:live-api-check
           #:live-login-check))

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

(defun test-code-reader (dc-id)
  "The code Telegram's test servers send: the data centre number, repeated to
whatever length the sentCode says.  Reading the length out of the response
rather than assuming it is what makes this work across layers."
  (lambda (sent-code)
    (let* ((type (tl:tl-value sent-code :type))
           (length (or (tl:tl-value type :length :errorp nil) 5)))
      (with-output-to-string (out)
        (dotimes (index length)
          (format out "~D" dc-id))))))

(defun live-login-check (&key phone-number (dc-id 2) test
                              (application (client:application-from-environment))
                              (session-file "/tmp/telegram-live-login.session")
                              (read-code (if test
                                             (test-code-reader dc-id)
                                             #'client:default-code-reader))
                              (stream *standard-output*))
  "Log in for real, then look around: whoami, the updates cursor, and the
first page of dialogs.

Needs a phone number and the code Telegram sends to it, so it cannot run
unattended.  The session goes to a scratch file rather than the usual one, so
running this does not disturb an existing login.

  (telegram.live:live-login-check :phone-number \"+15551234567\")

On the test network (:TEST T) the code reader assumes Telegram's documented
fixed code -- the data centre number repeated.  As of this writing the test
servers no longer honour that, so :TEST T needs a code you can actually see."
  (let ((client:*application* application))
    (multiple-value-bind (connection user)
        (client:log-in :phone-number phone-number
                       :dc-id dc-id :test test
                       :session-file session-file
                       :read-code read-code
                       :stream stream)
      (unwind-protect
           (progn
             (format stream "~&user: ~A~%" (client:user-label user))
             (let ((state (client:invoke connection :updates.get-state)))
               (format stream "~&~A: pts ~D, date ~D, seq ~D~%"
                       (tl:tl-name state)
                       (tl:tl-value state :pts) (tl:tl-value state :date)
                       (tl:tl-value state :seq)))
             (let ((dialogs (client:invoke connection :messages.get-dialogs
                                           :offset-date 0
                                           :offset-id 0
                                           :offset-peer (tl:make-tl
                                                         :input-peer-empty)
                                           :limit 10 :hash 0)))
               (format stream "~&~A: ~D dialog~:P, ~D user~:P~%"
                       (tl:tl-name dialogs)
                       (length (tl:tl-value dialogs :dialogs))
                       (length (tl:tl-value dialogs :users))))
             user)
        (net:close-mtproto-connection connection)))))
