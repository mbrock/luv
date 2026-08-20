(in-package #:luvcraft.agent)

;;; The agent itself: a provider connection with a session, a transcript, and
;;; a handle table.  Turns run on their own thread (#T6PEVY) so that asking
;;; from ./sly returns at once with the TURN to watch, and so that the canvas
;;; thread is only ever borrowed for one command at a time.

(defclass world-agent (openai:agent)
  ((session :initarg :session :reader world-agent-session)
   (turns :initform '() :accessor world-agent-turns
          :documentation "Every turn so far, newest first.")
   (current-turn :initform nil :accessor world-agent-current-turn)
   (handles :initform (make-handle-table) :reader world-agent-handles)
   (presence :initform nil :accessor world-agent-presence
             :documentation "The agent's EMBODIED-AGENT body in the world, or NIL.")
   (observers :initform '() :accessor world-agent-observers
              :documentation
              "Functions of (AGENT EVENT-KIND OBJECT), told when the transcript changes.")
   (lock :initform (sb-thread:make-mutex :name "world agent") :reader world-agent-lock))
  (:documentation "A language model with a place in a luvcraft session."))

(defvar *agent* nil
  "The most recently made WORLD-AGENT, for a ./sly session's convenience.")

(defvar *current-agent* nil
  "The agent whose command is running, bound around EXECUTE-FRAME-COMMAND so a
command such as SAY can reach the agent's presence.")

(defparameter *default-agent-model* "gpt-5.6")

(defparameter *default-agent-tools*
  '(com-where-am-i com-block-at com-place-block-at com-describe-handle com-eval)
  "The commands an agent is handed by default.")

(defparameter *default-instructions*
  "You are an agent standing inside luvcraft, a small block world running
inside a live Common Lisp image.  Your tools are the game's own commands.
Coordinates are integer block cells: x and z are horizontal, y is up.
Results may mention #ABCD handles; pass a handle back to describe-handle to
read more about that thing.  Keep answers short; act rather than narrate.")

;;; When the image was started without OPENAI_API_KEY, ask the tailnet lobby's
;;; value store for it (mqtt/net.lisp): `./scripts/luv lobby put OPENAI_API_KEY`.
(defun lobby-openai-api-key ()
  ;; The lobby worker owns the socket.  Agent creation only reads the retained
  ;; value it has already heard, so a missing radio can never stall the canvas.
  (and luvcraft:*session*
       (luvcraft:lobby-client-value
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
  "Open an agent for SESSION with tools for COMMANDS.

API-KEY defaults to OPENAI_API_KEY, then registered semantic fallbacks such as
the playing session's lobby radio."
  (unless session
    (error "No session: start one with ./sly play, or pass :SESSION."))
  (let ((agent (openai:make-agent
                :class 'world-agent :initargs (list :session session)
                :model model :instructions instructions
                :reasoning-summary reasoning-summary
                :reasoning-effort reasoning-effort
                :api-key api-key
                :tools (mapcar #'make-command-tool commands))))
    (setf *agent* agent)))

;;; ---------------------------------------------------------------------
;;; Telling the surfaces

(defun notify-agent-observers (agent kind object)
  (dolist (observer (world-agent-observers agent))
    (handler-case (funcall observer agent kind object)
      (error (condition)
        (warn "Agent observer ~A failed: ~A" observer condition)))))

(defun add-agent-observer (agent function)
  (pushnew function (world-agent-observers agent))
  function)

(defun remove-agent-observer (agent function)
  (setf (world-agent-observers agent)
        (remove function (world-agent-observers agent))))

(defun note-tool-call (agent call)
  (let ((turn (world-agent-current-turn agent)))
    (when turn
      (sb-thread:with-mutex ((world-agent-lock agent))
        (push call (turn-calls turn))
        (setf (turn-status turn) :working))))
  (notify-agent-observers agent :call-started call))

(defun note-tool-call-finished (agent call)
  (notify-agent-observers agent :call-finished call))

;;; ---------------------------------------------------------------------
;;; Events from the provider, in the turn's thread

(defmethod openai:handle-agent-event ((agent world-agent) event)
  (let ((turn (world-agent-current-turn agent)))
    (when (and turn (listp event) (not (keywordp (first event))))
      (let ((type (openai::json-value event :type)))
        (cond
          ((or (equal type "response.reasoning_summary_text.delta")
               (equal type "response.reasoning_text.delta"))
           (setf (turn-thought turn)
                 (concatenate 'string (turn-thought turn)
                              (openai::json-value event :delta)))
           (notify-agent-observers agent :thought turn))
          ((equal type "response.reasoning_summary_part.added")
           ;; A new summary paragraph; keep them apart in the bubble.
           (unless (string= (turn-thought turn) "")
             (setf (turn-thought turn)
                   (concatenate 'string (turn-thought turn)
                                (string #\Newline) (string #\Newline)))))
          ((equal type "response.output_text.delta")
           (setf (turn-text turn)
                 (concatenate 'string (turn-text turn)
                              (openai::json-value event :delta)))
           (notify-agent-observers agent :text turn)))))))

;;; ---------------------------------------------------------------------
;;; Asking

(defun run-turn (agent turn)
  "Run TURN to completion in the calling thread."
  (setf (world-agent-current-turn agent) turn)
  (notify-agent-observers agent :turn-started turn)
  (unwind-protect
       (handler-case
           (let ((*handles* (world-agent-handles agent)))
             (setf (turn-response turn)
                   (openai:agent-turn agent (turn-prompt turn)))
             ;; The final message may arrive whole rather than as deltas.
             (let ((text (openai:agent-response-text (turn-response turn))))
               (when (and text (> (length text) (length (turn-text turn))))
                 (setf (turn-text turn) text)))
             (setf (turn-status turn) :done))
         (error (condition)
           (setf (turn-error turn) condition
                 (turn-status turn) :failed)))
    (setf (turn-finished turn) (get-internal-real-time))
    (notify-agent-observers agent :turn-finished turn))
  turn)

(defun ask (text &key (agent (or *agent* (make-world-agent))))
  "Ask AGENT TEXT on a thread of its own; return the TURN at once.

Watch it with PRINT-TRANSCRIPT, or on the HUD.  ASK-AND-WAIT blocks instead."
  (let ((turn (make-instance 'turn :prompt text)))
    (sb-thread:with-mutex ((world-agent-lock agent))
      (push turn (world-agent-turns agent)))
    (setf (turn-thread turn)
          (sb-thread:make-thread (lambda () (run-turn agent turn))
                                 :name "world agent turn"))
    turn))

(defun ask-and-wait (text &key (agent (or *agent* (make-world-agent)))
                            (timeout 120))
  "ASK and wait up to TIMEOUT seconds, then print and return the turn."
  (let ((turn (ask text :agent agent)))
    (sb-thread:join-thread (turn-thread turn) :default nil :timeout timeout)
    (print-transcript turn)
    turn))
