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
because its modifier set differs.

A gesture may name :ANY in place of its modifiers, meaning the key itself
whatever else is held with it.  Movement is like that -- shift makes a player
sprint rather than making W into some other key -- and a gesture that had to
enumerate its modifiers could not say so."
  (let* ((specification (if (listp gesture) gesture (list gesture)))
         (key (first specification))
         (modifiers (rest specification)))
    (and (etypecase key
           (character
            (eql key (luv:canvas-key-event-unshifted-character event)))
           (symbol
            (eq key (luv:canvas-key-event-key-name event))))
         (or (member :any modifiers)
             (null (set-exclusive-or
                    modifiers
                    (canvas-key-event-gesture-modifiers event)))))))

;;; Naming a keystroke.
;;;
;;; What a control or a legend prints beside a verb, so that nothing has to
;;; write a key down twice.

(defun format-gesture-key (key)
  "Name KEY the way a legend should print it."
  (etypecase key
    (character (string-upcase (string key)))
    (symbol
     (case key
       (:return "RET")
       (:escape "Esc")
       (:space "Space")
       (:tab "Tab")
       (:up "↑") (:down "↓") (:left "←") (:right "→")
       (:shift-left "Shift") (:shift-right "Shift")
       (t (string-capitalize (symbol-name key)))))))

(defun format-gesture (gesture)
  "Name a whole keystroke, modifiers and all.

:ANY is not printed: a key bound whatever else is held is just that key, and
saying so would be noise on every movement row."
  (let* ((specification (if (listp gesture) gesture (list gesture)))
         (key (first specification))
         (modifiers (remove :any (rest specification))))
    (format nil "~{~A~}~A"
            (mapcar (lambda (modifier)
                      ;; Words rather than the pretty modifier glyphs: the game
                      ;; draws its own text with its own font, and U+2318 and
                      ;; friends are simply absent from it, so a key printed
                      ;; that way loses its modifier entirely.
                      (case modifier
                        (:shift "Shift-")
                        (:control "Ctrl-")
                        (:meta "Alt-")
                        (:super "Cmd-")
                        (t (format nil "~A-" modifier))))
                    modifiers)
            (format-gesture-key key))))

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
