(in-package #:luvcraft.clim.tests)

(deftest lobby-presence-is-a-small-semantic-snapshot
  (let ((client (make-instance 'luvcraft:lobby-client
                               :id "self" :name "Mikael")))
    (luvcraft:receive-lobby-publication client "luv/presence/self" "Mikael")
    (luvcraft:receive-lobby-publication client "luv/presence/game-2" "Daniel")
    (multiple-value-bind (status peers error revision)
        (luvcraft:lobby-client-snapshot client)
      (ok (eq :starting status))
      (ok (equal '("Daniel") peers))
      (ok (null error))
      (ok (plusp revision)))
    (luvcraft:receive-lobby-publication client "luv/presence/game-2" "")
    (ok (null (nth-value 1 (luvcraft:lobby-client-snapshot client))))
    (luvcraft:receive-lobby-publication client "luv/presence/game-2" "Daniel")
    (luvcraft::set-lobby-client-status client :offline)
    (ok (null (nth-value 1 (luvcraft:lobby-client-snapshot client))))))

(deftest lobby-presence-will-tombstone
  (let ((client (make-instance 'luvcraft:lobby-client
                               :id "self" :name "Mikael")))
    (luvcraft:receive-lobby-publication client "luv/presence/game-2" "Daniel")
    (luvcraft:receive-lobby-publication
     client "luv/presence/game-2" luvcraft::+lobby-offline-payload+)
    (ok (null (nth-value 1 (luvcraft:lobby-client-snapshot client))))))

(deftest lobby-retained-values-are-read-without-network-io
  (let ((client (make-instance 'luvcraft:lobby-client
                               :id "self" :name "Mikael")))
    (luvcraft:receive-lobby-publication
     client "luv/store/OPENAI_API_KEY" "secret")
    (ok (string= "secret" (luvcraft:lobby-client-value client "OPENAI_API_KEY")))
    (luvcraft:receive-lobby-publication client "luv/store/OPENAI_API_KEY" "")
    (ok (null (luvcraft:lobby-client-value client "OPENAI_API_KEY")))
    (luvcraft:receive-lobby-publication client "luv/store/TELEGRAM_API_ID" "12345")
    (luvcraft:receive-lobby-publication client "luv/store/TELEGRAM_API_HASH"
                                       "deadbeef")
    (ok (string= "12345"
                 (luvcraft:lobby-client-value client "TELEGRAM_API_ID")))
    (ok (string= "deadbeef"
                 (luvcraft:lobby-client-value client "TELEGRAM_API_HASH")))))

(deftest telegram-credentials-resolve-from-the-playing-session-cache
  (let* ((client (make-instance 'luvcraft:lobby-client
                                :id "self" :name "Mikael"))
         (session (make-instance 'luvcraft:luvcraft-session))
         (luvcraft:*session* session))
    (setf (luvcraft:luvcraft-session-lobby-client session) client)
    (luvcraft:receive-lobby-publication client "luv/store/TELEGRAM_API_ID" "12345")
    (ok (string= "12345"
                 (luvcraft::lobby-telegram-credential "TELEGRAM_API_ID")))
    (ok (null (luvcraft::lobby-telegram-credential "SOMETHING_ELSE")))))

(deftest telegram-console-recovers-when-radio-credentials-arrive-late
  (let ((console (make-instance 'mcluv::telegram-console))
        (telegram.client:*credential-files* nil)
        (telegram.client:*credential-fallbacks*
          (list (lambda (name)
                  (cond ((string= name "TELEGRAM_API_ID") "12345")
                        ((string= name "TELEGRAM_API_HASH") "deadbeef"))))))
    (setf (mcluv::console-login-stage console) :api-id)
    (ok (mcluv::adopt-late-console-credentials console))
    (ok (null (mcluv::console-login-stage console)))))
