(in-package #:luvcraft.clim)

;;; The world command table holds the verbs a player has while nothing is
;;; focused.  Keeping it separate from the frame's own table is what will let
;;; a focused shell or phone contribute a table that does not inherit it: the
;;; layering that is currently an ordered ladder of guards in
;;; HANDLE-CANVAS-EVENT becomes table inheritance, where a key cannot leak
;;; into a surface that never asked for it.

(define-command-table luvcraft-world)

(define-application-frame luvcraft-frame ()
  ((session :initarg :session :reader luvcraft-frame-session))
  ;; :INHERIT-MENU is what carries keystrokes across inheritance -- a command
  ;; table inherits its parents' commands by default but not their accelerators,
  ;; so without this the world's keys are defined and unreachable.
  (:command-table (luvcraft-frame :inherit-from (luvcraft-world)
                                  :inherit-menu t))
  (:menu-bar nil))

(defun make-luvcraft-frame (session)
  "Return an application frame holding SESSION's command vocabulary.

Deliberately built without a frame manager: MAKE-APPLICATION-FRAME only
adopts a frame when one is supplied, so this frame has no panes, no mirror,
and no second native canvas -- which luv's Cocoa host would refuse anyway.
It exists for its command tables and for being *APPLICATION-FRAME* while a
command runs."
  (make-application-frame 'luvcraft-frame :session session))

(defun luvcraft-command-session ()
  "The session the running command belongs to."
  (luvcraft-frame-session *application-frame*))

;;; Keystrokes.
;;;
;;; CLIM writes a keystroke as a gesture specification -- (#\i), (:tab :shift),
;;; (#\d :control) -- while luv delivers a portable CANVAS-KEY-EVENT carrying a
;;; key name, the layout characters, and a logical modifier list.  Matching the
;;; two is the whole bridge; everything else is ordinary CLIM.

(defparameter +luvcraft-lock-modifiers+ '(:caps-lock :num-lock)
  "Modifiers a keystroke never names, and so never has to match.

A player with caps lock on is still playing the game.")

(defun canvas-key-event-gesture-modifiers (event)
  "EVENT's modifiers, less the ones a keystroke specification ignores."
  (set-difference (luv:canvas-key-event-modifiers event)
                  +luvcraft-lock-modifiers+))

(defun canvas-key-event-matches-gesture-p (event gesture)
  "Whether luv key EVENT is the keystroke named by CLIM's GESTURE.

A character in GESTURE is matched against the event's unshifted character, so
a binding is a place on the keyboard rather than a piece of text: I is I
whether or not caps lock is down, and Shift-I remains a different gesture
because its modifier set differs."
  (let* ((specification (if (listp gesture) gesture (list gesture)))
         (key (first specification))
         (modifiers (rest specification)))
    (and (etypecase key
           (character
            (eql key (luv:canvas-key-event-unshifted-character event)))
           (symbol
            (eq key (luv:canvas-key-event-key-name event))))
         (null (set-exclusive-or
                modifiers (canvas-key-event-gesture-modifiers event))))))

(defun canvas-key-event-command (frame event)
  "Return the command FRAME's tables bind to key EVENT, or NIL for none.

LOOKUP-KEYSTROKE-COMMAND-ITEM answers with the gesture itself when nothing
matches, and consults COMMAND-ENABLED against *APPLICATION-FRAME*, so both
are established here rather than left to the caller."
  (let* ((*application-frame* frame)
         (command
           (lookup-keystroke-command-item
            event (frame-command-table frame)
            :test #'canvas-key-event-matches-gesture-p)))
    (and (consp command) command)))

(defun execute-canvas-key-event-command (frame event)
  "Run whatever command FRAME binds to key EVENT; return true when one ran.

EXECUTE-FRAME-COMMAND applies the command on this very thread as long as the
frame has no process of its own, so a command is exactly as privileged as the
HANDLE-CANVAS-EVENT method that reached it."
  (alexandria:when-let ((command (canvas-key-event-command frame event)))
    (execute-frame-command frame command)
    t))
