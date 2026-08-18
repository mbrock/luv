(in-package #:luvcraft.clim)

;;; The verbs.  Each one is the body that HANDLE-CANVAS-EVENT used to run
;;; inline behind an EQ test on a key name, now carrying its own name, its own
;;; human label, and its own keystroke as data in a table.

(define-command (com-toggle-inventory :command-table luvcraft-world
                                      :name "Toggle Inventory"
                                      :keystroke (#\i))
    ()
  (luvcraft:toggle-luvcraft-inventory (luvcraft-command-session)))

(define-command (com-toggle-phone :command-table luvcraft-world
                                  :name "Toggle Phone"
                                  :keystroke (#\f))
    ()
  (luvcraft:toggle-luvcraft-phone (luvcraft-command-session)))

(define-command (com-toggle-ball :command-table luvcraft-world
                                 :name "Toggle Ball"
                                 :keystroke (#\b))
    ()
  (luvcraft:toggle-luvcraft-ball (luvcraft-command-session)))

(define-command (com-toggle-metabar :command-table luvcraft-world
                                    :name "Toggle Metabar"
                                    :keystroke (:return))
    ()
  (luvcraft:toggle-luvcraft-metabar (luvcraft-command-session)))

(define-command (com-toggle-focus :command-table luvcraft-world
                                  :name "Toggle Focus"
                                  :keystroke (:tab))
    ()
  (luvcraft:toggle-luvcraft-session-focus (luvcraft-command-session)))

;;; The window layer.  Fullscreen belongs to the window rather than to
;;; anything inside it, and Shift-Tab is the gesture that gets the keyboard
;;; back from whatever has taken it -- including a shell reading raw keys,
;;; which is exactly the surface that must not be able to swallow it.

(define-command (com-toggle-fullscreen :command-table luvcraft-window
                                       :name "Toggle Fullscreen"
                                       :keystroke (:f11))
    ()
  (let ((canvas (luvcraft:luvcraft-session-canvas (luvcraft-command-session))))
    (luv:set-canvas-fullscreen canvas (not (luv:canvas-fullscreen-p canvas)))))

(define-command (com-leave-focus :command-table luvcraft-window
                                 :name "Leave Focus"
                                 :keystroke (:tab :shift))
    ()
  (luvcraft:unfocus-luvcraft-session (luvcraft-command-session)))

;;; Named but unbound: escape now shows the keymap, which releases the pointer
;;; on its way.  This stays reachable by name, from a listener or a script.
(define-command (com-release-pointer :command-table luvcraft-world
                                     :name "Release Pointer")
    ()
  (let ((session (luvcraft-command-session)))
    (when (luvcraft:luvcraft-session-pointer-captured-p session)
      (luv:set-canvas-relative-pointer-mode
       (luvcraft:luvcraft-session-canvas session) nil)
      (setf (luvcraft:luvcraft-session-pointer-captured-p session) nil))))

(define-command (com-select-quickbar-slot :command-table luvcraft-world
                                          :name "Select Quickbar Slot")
    ((slot 'integer :prompt "quickbar slot"))
  ;; The quickbar is how many slots the number row reaches, while the block a
  ;; slot names is looked up in the whole inventory: a slot past the end of the
  ;; bar selects nothing rather than something further down the list.
  (let ((session (luvcraft-command-session)))
    (when (<= 1 slot (length (luvcraft:block-inventory-quickbar-blocks
                              (luvcraft:luvcraft-session-inventory session))))
      (luvcraft:select-luvcraft-block session slot))))

;;; The number row selects a quickbar slot.  Nine commands would say the same
;;; thing nine times; a keystroke item of type :FUNCTION instead builds the
;;; command object for the digit that was pressed, which is the ordinary CLIM
;;; way to bind a family of keys to one verb with an argument.
(loop for slot from 1 to 9
      do (add-keystroke-to-command-table
          'luvcraft-world (list (digit-char slot)) :function
          (let ((slot slot))
            (lambda (gesture numeric-argument)
              (declare (ignore gesture numeric-argument))
              (list 'com-select-quickbar-slot slot)))
          :documentation (format nil "Select quickbar slot ~D" slot)
          :errorp nil))

;;; Movement.
;;;
;;; A direction key is a start and a stop rather than a state someone reads out
;;; of a key table later, so what the game holds is the urge itself: a rider on
;;; an animal and a player on their own feet run the same commands and set the
;;; same intent, and a gamepad or a recorded demo could run them too.

(define-command (com-start-walking :command-table luvcraft-movement
                                   :name "Start Walking")
    ((direction 'keyword :prompt "direction"))
  (setf (luvcraft:movement-urging-p
         (luvcraft:luvcraft-session-movement-intent (luvcraft-command-session))
         direction)
        t))

(define-command (com-stop-walking :command-table luvcraft-movement-release
                                  :name "Stop Walking")
    ((direction 'keyword :prompt "direction"))
  (setf (luvcraft:movement-urging-p
         (luvcraft:luvcraft-session-movement-intent (luvcraft-command-session))
         direction)
        nil))

(define-command (com-jump :command-table luvcraft-movement
                          :name "Jump"
                          :keystroke (:space :any))
    ()
  (setf (luvcraft:movement-intent-jump-requested-p
         (luvcraft:luvcraft-session-movement-intent (luvcraft-command-session)))
        t))

(defparameter *walk-keys*
  '((:w :forward) (:up :forward)
    (:s :backward) (:down :backward)
    (:a :left) (:left :left)
    (:d :right) (:right :right)
    (:shift-left :sprint) (:shift-right :sprint))
  "Which key urges which direction, arrows alongside the letters.

Bound with :ANY because a movement key means the same thing however it is
modified: Shift-W is a player sprinting forward, not a player pressing some
other key.")

(defun add-walk-keystrokes ()
  "Bind every walking key to its start on the way down and its stop on the up."
  (loop for (key direction) in *walk-keys*
        do (flet ((walk-item (command)
                    (let ((direction direction))
                      (lambda (gesture numeric-argument)
                        (declare (ignore gesture numeric-argument))
                        (list command direction)))))
             (add-keystroke-to-command-table
              'luvcraft-movement (list key :any) :function
              (walk-item 'com-start-walking) :errorp nil)
             (add-keystroke-to-command-table
              'luvcraft-movement-release (list key :any) :function
              (walk-item 'com-stop-walking) :errorp nil))))

(add-walk-keystrokes)

;;; A rider's focus offers exactly the movement table, so steering an animal
;;; and walking are the same commands reaching the same intent.  Everything
;;; else a rider presses still falls to the ride itself, which wants none of it.

(defmethod luvcraft-focus-command-table
    ((focus luvcraft:critter-ride) (event luv:canvas-key-press-event))
  (declare (ignore focus event))
  'luvcraft-movement)

(defmethod luvcraft-focus-command-table
    ((focus luvcraft:critter-ride) (event luv:canvas-key-release-event))
  (declare (ignore focus event))
  'luvcraft-movement-release)

;;; At a terminal.
;;;
;;; A wall in shell mode gives every key to the PTY, which is what makes it a
;;; shell and not a menu; so the keys that change what the wall *is* have to be
;;; ones no shell wants.  The command modifier is that: a terminal never sees
;;; it, and on this platform it is already how one switches between things.

(define-command (com-set-terminal-mode :command-table luvcraft-terminal
                                       :name "Set Wall Mode")
    ((mode 'keyword :prompt "wall mode"))
  (let* ((session (luvcraft-command-session))
         (focus (luvcraft:luvcraft-session-modal-focus session)))
    (when (typep focus 'luvcraft:terminal-display)
      (luvcraft:change-terminal-display-mode focus session mode))))

(defun terminal-mode-gesture (mode)
  "The keystroke that chooses MODE, by its place in the wall's own mode list."
  (alexandria:when-let
      ((index (position mode mcluv::*terminal-display-modes*)))
    (when (< index 9)
      (list (digit-char (1+ index)) :super))))

(defun add-terminal-mode-keystrokes ()
  "Bind each wall mode to the command modifier and its slot number."
  (dolist (mode mcluv::*terminal-display-modes*)
    (alexandria:when-let ((gesture (terminal-mode-gesture mode)))
      (add-keystroke-to-command-table
       'luvcraft-terminal gesture :function
       (let ((mode mode))
         (lambda (gesture numeric-argument)
           (declare (ignore gesture numeric-argument))
           (list 'com-set-terminal-mode mode)))
       :errorp nil))))

(add-terminal-mode-keystrokes)

(defmethod luvcraft-focus-command-table
    ((focus luvcraft:terminal-display) (event luv:canvas-key-press-event))
  (declare (ignore focus event))
  'luvcraft-terminal)

;;; The wall's own chooser asks what key reaches a mode rather than printing a
;;; number and hoping.  Specializing on SYMBOL beats the core's T default.

(defmethod luvcraft:luvcraft-key-hint ((mode symbol))
  (alexandria:when-let ((gesture (terminal-mode-gesture mode)))
    (format-gesture gesture)))
