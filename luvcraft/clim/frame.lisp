(in-package #:luvcraft.clim)

;;; Input arrives in layers, and the layers are tables.
;;;
;;; LUVCRAFT-WINDOW holds the keys that belong to the window rather than to
;;; anything inside it -- fullscreen, and the one gesture that leaves whatever
;;; has taken the keyboard.  It is consulted first and nothing can shadow it,
;;; because a surface that could swallow the key which escapes it is a trap.
;;;
;;; LUVCRAFT-WORLD holds the verbs a player has while nothing is focused.  It
;;; is consulted only then; a focused shell or phone is offered its own table
;;; instead, and by default has none.  That is the whole of the layering which
;;; used to be an ordered ladder of guards in HANDLE-CANVAS-EVENT, where every
;;; new global key was one more chance for a letter to leak into a surface
;;; someone was typing into.

(define-command-table luvcraft-window)

(define-command-table luvcraft-world)

(define-application-frame luvcraft-frame ()
  ((session :initarg :session :reader luvcraft-frame-session))
  ;; :INHERIT-MENU is what carries keystrokes across inheritance -- a command
  ;; table inherits its parents' commands by default but not their accelerators,
  ;; so without this the world's keys are defined and unreachable.
  (:command-table (luvcraft-frame :inherit-from (luvcraft-window luvcraft-world)
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

(defvar *luvcraft-frames*
  (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Each live session's application frame, held only as long as the session is.

The frame belongs to this layer rather than to the game, so it is found here
rather than stored in a session slot the core would never otherwise mention.")

(defun luvcraft-session-frame (session)
  "Return SESSION's application frame, making it on first use."
  (or (gethash session *luvcraft-frames*)
      (setf (gethash session *luvcraft-frames*)
            (make-luvcraft-frame session))))

(defgeneric luvcraft-focus-command-table (focus)
  (:documentation
   "Return the command table a focused FOCUS offers, or NIL for none.

A focus with no table takes every key that the window layer did not claim,
which is what a shell in raw mode wants: its letters are text, not verbs.  A
focus which does want verbs answers with a table, and inheritance -- rather
than the order of tests in a dispatch function -- decides what it also gets."))

(defmethod luvcraft-focus-command-table (focus)
  (declare (ignore focus))
  nil)

(defun luvcraft-key-command (session event)
  "The command SESSION's current input layer binds to key EVENT, or NIL.

The window layer answers first and cannot be shadowed.  After it, a focused
object is asked through its own table and an unfocused player through the
world's, so a key reaches exactly one layer."
  (let ((frame (luvcraft-session-frame session))
        (focus (luvcraft:luvcraft-session-modal-focus session)))
    (or (canvas-key-event-command frame event
                                  :command-table 'luvcraft-window)
        (if focus
            (alexandria:when-let
                ((table (luvcraft-focus-command-table focus)))
              (canvas-key-event-command frame event :command-table table))
            (canvas-key-event-command frame event
                                      :command-table 'luvcraft-world)))))

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

(defun canvas-key-event-command
    (frame event &key (command-table (frame-command-table frame)))
  "Return the command COMMAND-TABLE binds to key EVENT, or NIL for none.

LOOKUP-KEYSTROKE-COMMAND-ITEM answers with the gesture itself when nothing
matches, and consults COMMAND-ENABLED against *APPLICATION-FRAME*, so both
are established here rather than left to the caller."
  (let* ((*application-frame* frame)
         (command
           (lookup-keystroke-command-item
            event command-table
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
