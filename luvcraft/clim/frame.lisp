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
(define-command-table luvcraft-window-release)

;;; Movement is its own table because more than one thing moves.  A player on
;;; their own feet reaches it by inheritance from the world; a rider reaches
;;; the very same table as their focus's, which is what makes steering an
;;; animal and walking one vocabulary instead of two parallel ones.

(define-command-table luvcraft-movement)
(define-command-table luvcraft-movement-release)

(define-command-table luvcraft-world
  :inherit-from (luvcraft-movement)
  :inherit-menu t)

;;; A focused wall's own layer.  It exists so that a shell, which is entitled
;;; to every plain key, can still be told to become something else.

(define-command-table luvcraft-terminal)

(define-command-table luvcraft-world-release
  :inherit-from (luvcraft-movement-release)
  :inherit-menu t)

(define-application-frame luvcraft-frame ()
  ((session :initarg :session :reader luvcraft-frame-session))
  ;; The frame's table is the whole vocabulary, inheriting every layer,
  ;; because LOOKUP-KEYSTROKE-COMMAND-ITEM asks COMMAND-ENABLED whether the
  ;; command it found is present *here* -- a command reachable only from a
  ;; layer table would be looked up and then silently refused.  The layer
  ;; tables above are what dispatch actually searches; this one is what makes
  ;; the commands it finds legal to run.
  ;;
  ;; :INHERIT-MENU is what carries keystrokes across inheritance -- a command
  ;; table inherits its parents' commands by default but not their accelerators.
  (:command-table (luvcraft-frame
                   :inherit-from (luvcraft-window luvcraft-window-release
                                  luvcraft-world luvcraft-world-release
                                  luvcraft-terminal)
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

(defgeneric luvcraft-key-event-tables (event)
  (:documentation
   "Return the window and world command tables EVENT is looked up in.

A key going down and the same key coming up are different gestures wanting
different commands -- start walking and stop walking -- and CLIM's keystroke
accelerators only ever fire on the way down.  Splitting the tables by event
class is how a release gets a vocabulary of its own without the lookup having
to ask what kind of event it is holding.")
  (:method ((event luv:canvas-key-press-event))
    (values 'luvcraft-window 'luvcraft-world))
  (:method ((event luv:canvas-key-release-event))
    (values 'luvcraft-window-release 'luvcraft-world-release)))

(defgeneric luvcraft-focus-command-table (focus event)
  (:documentation
   "Return the command table focused FOCUS offers for EVENT, or NIL for none.

A focus with no table takes every key that the window layer did not claim,
which is what a shell in raw mode wants: its letters are text, not verbs.  A
focus which does want verbs answers with a table, and inheritance -- rather
than the order of tests in a dispatch function -- decides what it also gets."))

(defmethod luvcraft-focus-command-table (focus event)
  (declare (ignore focus event))
  nil)

(defun luvcraft-key-command (session event)
  "The command SESSION's current input layer binds to key EVENT, or NIL.

The window layer answers first and cannot be shadowed.  After it, a focused
object is asked through its own table and an unfocused player through the
world's, so a key reaches exactly one layer."
  (let ((frame (luvcraft-session-frame session))
        (focus (luvcraft:luvcraft-session-modal-focus session)))
    (multiple-value-bind (window world) (luvcraft-key-event-tables event)
      (or (canvas-key-event-command frame event :command-table window)
          (if focus
              (alexandria:when-let
                  ((table (luvcraft-focus-command-table focus event)))
                (canvas-key-event-command frame event :command-table table))
              (canvas-key-event-command frame event
                                        :command-table world))))))
