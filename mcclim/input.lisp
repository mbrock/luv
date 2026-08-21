(in-package #:mcluv)

;;; A luv canvas reports portable physical keys, layout characters, and
;;; logical modifiers. McCLIM command tables call the same thing a gesture.
;;; Keeping this bridge beside the backend lets every canvas application be a
;;; CLIM application without borrowing another application's command package.

(defparameter +canvas-lock-modifiers+ '(:caps-lock :num-lock)
  "Modifiers a McCLIM keystroke never names and therefore need not match.")

(defun canvas-key-event-gesture-modifiers (event)
  "Return EVENT's logical modifiers, less locks ignored by gestures."
  (set-difference (luv:canvas-key-event-modifiers event)
                  +canvas-lock-modifiers+))

(defun canvas-key-event-matches-gesture-p (event gesture)
  "Whether portable luv key EVENT is the McCLIM GESTURE.

A character gesture matches EVENT's unshifted character, making it a place on
the current keyboard layout. A symbol gesture matches the physical key name.
The modifier :ANY means the key matches however it is modified."
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

(defun format-gesture-key (key)
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
  "Name GESTURE for a command legend without duplicating its binding."
  (let* ((specification (if (listp gesture) gesture (list gesture)))
         (key (first specification))
         (modifiers (remove :any (rest specification))))
    (format nil "~{~A~}~A"
            (mapcar (lambda (modifier)
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
  "Return the command COMMAND-TABLE binds to key EVENT, or NIL.

McCLIM answers with the gesture itself on a miss and checks command enablement
against *APPLICATION-FRAME*, so both details are established here."
  (let* ((*application-frame* frame)
         (command
           (lookup-keystroke-command-item
            event command-table :test #'canvas-key-event-matches-gesture-p)))
    (and (consp command) command)))

(defun execute-canvas-key-event-command (frame event)
  "Run FRAME's command for EVENT inline; return true when one ran."
  (let ((command (canvas-key-event-command frame event)))
    (when command
      (execute-frame-command frame command)
      t)))
