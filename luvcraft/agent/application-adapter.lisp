(in-package #:luvcraft.agent)

;;; Luvcraft contributes only its application boundary and world vocabulary to
;;; the shared harness.  Bodies, construction, observation, approval, and
;;; presence remain game features in this package.

(defclass world-agent (luv.application-agent:application-agent)
  ((presence :initform nil :accessor world-agent-presence
             :documentation "The embodied Luvcraft body, or NIL.")))

(defun world-agent-session (agent)
  (luv.application-agent:application-agent-application agent))

(defun world-agent-turns (agent)
  (luv.application-agent:application-agent-turns agent))

(defun world-agent-current-turn (agent)
  (luv.application-agent:application-agent-current-turn agent))

(defun world-agent-handles (agent)
  (luv.application-agent:application-agent-handles agent))

(defvar *agent* nil
  "The most recently made Luvcraft WORLD-AGENT for REPL convenience.")

(defparameter *default-agent-model* "gpt-5.6")

(defparameter *default-agent-tools*
  '(com-where-am-i com-move-to com-block-at com-place-block-at
    com-propose-block-box com-describe-handle com-eval))

(defparameter *default-instructions*
  "You are an agent standing inside Luvcraft, a small block world running
inside a live Common Lisp image. Your tools are the game's own commands.
Coordinates are integer block cells: x and z are horizontal, y is up.
Results may mention #ABCD handles; pass one to a command accepting a handle
to inspect the same object again. Keep answers short; act rather than narrate.")

(defun lobby-openai-api-key ()
  ;; This is a retained cache read, not MQTT I/O, so construction never stalls
  ;; the canvas waiting for the lobby.
  (and luvcraft:*session*
       (luv.lobby:lobby-client-value
        (luvcraft:luvcraft-session-lobby-client luvcraft:*session*)
        "OPENAI_API_KEY")))

(pushnew 'lobby-openai-api-key openai:*api-key-fallbacks*)

(defun make-world-agent (&key (session luvcraft:*session*)
                           (model *default-agent-model*)
                           (commands *default-agent-tools*)
                           (instructions *default-instructions*)
                           (reasoning-summary "auto")
                           reasoning-effort
                           (api-key (openai:default-api-key)))
  "Open Luvcraft's thin WORLD-AGENT specialization for SESSION.

Connection setup performs network I/O; call this from SLY or a setup worker,
never from the canvas thread.  Once open, ASK itself always returns at once."
  (unless session
    (error "No Luvcraft session: use ./sly play or pass :SESSION."))
  (setf *agent*
        (luv.application-agent:make-application-agent
         :class 'world-agent :application session
         :model model :commands commands :instructions instructions
         :reasoning-summary reasoning-summary
         :reasoning-effort reasoning-effort :api-key api-key)))

(defun ask (text &key (agent (or *agent*
                                 (error "No Luvcraft agent is open."))))
  "Start a Luvcraft agent turn and return its transcript record immediately."
  (luv.application-agent:ask text :agent agent))

(defun ask-and-wait (text &key (agent (or *agent*
                                          (error "No Luvcraft agent is open.")))
                            (timeout 120))
  (luv.application-agent:ask-and-wait
   text :agent agent :timeout timeout :print-p t))

(defmethod luv.application-agent:application-command-frame
    ((session luvcraft:luvcraft-session))
  (luvcraft.clim::luvcraft-session-frame session))

(defmethod luv.application-agent:call-in-application-frame
    ((session luvcraft:luvcraft-session) function)
  (luv:request-canvas-frame
   (luvcraft:luvcraft-session-canvas session)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (funcall function))))

;;; The only domain-specific presentation type in the former harness core.

(defun placeable-block-kind-names ()
  (loop for kind in luvcraft::*block-kinds*
        when (luvcraft::block-kind-placeable-p kind)
          collect (luvcraft:block-kind-name kind)))

(define-presentation-method accept
    ((type luvcraft:block-kind) stream (view textual-view) &key)
  (let* ((string (read-token stream))
         (kind
           (luvcraft:block-kind-named
            (intern (string-upcase string) :keyword) nil)))
    (or kind
        (simple-parse-error
         "~S is not a block kind; try one of ~{~(~A~)~^, ~}."
         string (placeable-block-kind-names)))))

(define-presentation-method present
    (object (type luvcraft:block-kind) stream (view textual-view) &key)
  (format stream "~(~A~)" (luvcraft:block-kind-name object)))

(defmethod luv.application-agent:presentation-type-json-schema
    ((name (eql 'luvcraft:block-kind)) &rest parameters)
  (declare (ignore parameters))
  `(("type" . "string")
    ("enum" . ,(mapcar (lambda (name)
                          (string-downcase (symbol-name name)))
                        (placeable-block-kind-names)))))
