;;; Luvcraft ownership of the shared application lobby radio.

(in-package #:luvcraft)

(defmethod start-luvcraft-lobby ((session luvcraft-session))
  (let ((client
          (or (luvcraft-session-lobby-client session)
              (setf (luvcraft-session-lobby-client session)
                    (luv.lobby:make-lobby-client
                     :client-id-prefix "luvcraft")))))
    ;; START-LOBBY-CLIENT is idempotent and also restarts a stopped client.
    (luv.lobby:start-lobby-client client)))

(defmethod stop-luvcraft-lobby ((session luvcraft-session))
  (let ((client (luvcraft-session-lobby-client session)))
    (when client
      ;; Retain the reference if bounded cooperative shutdown fails so an
      ;; interactive retry can still reach the worker.
      (luv.lobby:stop-lobby-client client)
      (setf (luvcraft-session-lobby-client session) nil)))
  nil)
