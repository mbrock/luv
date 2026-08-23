;;; Credential consumers own their policy.  The shared lobby only supplies a
;;; connection-scoped retained-value cache and never learns what its keys mean.

(in-package #:luvcraft)

(defun lobby-telegram-credential (name)
  (and *session*
       (member name '("TELEGRAM_API_ID" "TELEGRAM_API_HASH"
                      "TDLIB_API_ID" "TDLIB_API_HASH")
               :test #'string=)
       (luv.lobby:lobby-client-value
        (luvcraft-session-lobby-client *session*) name)))

(pushnew 'lobby-telegram-credential telegram.client:*credential-fallbacks*)
