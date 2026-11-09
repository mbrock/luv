;;;; Luvland from the sofa: a gamepad drives the shell, and games keep the pad.
;;;;
;;;; Games read the pad themselves, so Luvland takes only the Guide button
;;;; while a client has focus.  Guide opens the play menu, a workbench prompt
;;;; whose candidates are the games, the open windows, and a few window
;;;; commands.  While the shell has the input, Luvland plays the pad as keys:
;;;; the D-pad is the arrows, A is Return, B is Escape.  Clients lose the
;;;; keyboard meanwhile, so an SDL game ignores the pad until the menu closes.

(in-package #:luvland)

(defvar *games* '()
  "The games the play menu offers, as (name command-line), in menu order.")

(defun define-game (name command-line)
  "Offer NAME in the play menu.  Choosing it focuses a window whose title
contains NAME, or else runs COMMAND-LINE through the shell as a client."
  (setf *games* (append (remove name *games* :key #'first :test #'string-equal)
                        (list (list name command-line))))
  name)

(defparameter *gamepad-shell-keys*
  '((:dpad-up . :up) (:dpad-down . :down)
    (:dpad-left . :left) (:dpad-right . :right)
    (:south . :return) (:east . :escape))
  "The keys the pad plays while the shell has the input.")

(defparameter *gamepad-menu-legend*
  "A choose · B back · LB/RB windows · Y overview · X close")

(defun shell-has-input-p (luvland)
  (let ((workbench (luvland-workbench luvland)))
    (and workbench (luv.workbench:workbench-input-suspended-p workbench))))

(defun play-key (luvland key-name pressed-p)
  "Press or release KEY-NAME as if on a keyboard, through the workbench."
  (let ((canvas (luvland-canvas luvland)))
    (luv:handle-canvas-event
     (luv:canvas-event-handler canvas) canvas
     (make-instance (if pressed-p 'luv:canvas-key-press-event 'luv:canvas-key-release-event)
                    :timestamp 0 :key-name key-name))))

(defun window-for-game (luvland name)
  (find-if (lambda (window) (search name (window-title window) :test #'char-equal))
           (luvland-windows luvland)))

(defun play (luvland name command-line)
  (let ((window (window-for-game luvland name)))
    (if window
        (focus-window luvland (window-toplevel window))
        (progn
          (message luvland "Starting ~A…" name)
          (spawn-client luvland "/bin/sh" "-c" (format nil "exec ~A" command-line))))))

(defun menu-entries (luvland)
  "The play menu as (label . thunk): games, then windows, then commands."
  (append
   (loop for (name command-line) in *games*
         collect (let ((name name) (command-line command-line))
                   (cons (if (window-for-game luvland name) (format nil "~A (running)" name) name)
                         (lambda () (play luvland name command-line)))))
   (loop for window in (sort (copy-list (luvland-windows luvland)) #'< :key #'window-number)
         collect (let ((toplevel (window-toplevel window)))
                   (cons (format nil "window ~D: ~A" (window-number window) (window-title window))
                         (lambda () (focus-window luvland toplevel)))))
   (list (cons "overview" (lambda () (toggle-overview luvland)))
         (cons "terminal" (lambda () (run-command-line luvland "terminal"))))
   ;; Whatever the bar offers a click, the pad reaches here.
   (bar-menu-entries luvland)
   (when (luvland-focus luvland)
     (list (cons "close window" (lambda () (run-command-line luvland "delete")))))))

(defun open-play-menu (luvland)
  (let ((entries (menu-entries luvland)))
    (luv.workbench:show-workbench-message (luvland-workbench luvland)
                                         *gamepad-menu-legend* :seconds 8)
    (luv.workbench:open-workbench-prompt
     (luvland-workbench luvland)
     :label "play:"
     :select-first-p t
     :completer (lambda (text)
                  (loop for (label) in entries
                        when (search (string-trim " " text) label :test #'char-equal)
                          collect label))
     :function (lambda (label)
                 (clear-message luvland)
                 (let ((entry (assoc label entries :test #'string=)))
                   (when entry (funcall (cdr entry))))))))

(define-luvland-command play ()
  (open-play-menu luvland))

(defun gamepad-menu-button (luvland button)
  "BUTTON pressed while the shell has the input; true if Luvland used it."
  (case button
    (:left-shoulder (focus-neighbor luvland -1 :wrap t) t)
    (:right-shoulder (focus-neighbor luvland 1 :wrap t) t)
    (:north (toggle-overview luvland) t)
    (:west (run-command-line luvland "delete") t)))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas
                                    (event luv:canvas-gamepad-button-press-event))
  (declare (ignore canvas))
  (let ((button (luv:canvas-gamepad-event-button event)))
    (cond
      ((shell-has-input-p luvland)
       (cond ((eq button :guide) (play-key luvland :escape t) (play-key luvland :escape nil))
             ((gamepad-menu-button luvland button))
             ((assoc button *gamepad-shell-keys*)
              (play-key luvland (cdr (assoc button *gamepad-shell-keys*)) t))))
      ;; With a client focused, the pad is the client's but for Guide.
      ((or (eq button :guide)
           (and (null (luvland-focus luvland)) (member button '(:south :start))))
       (open-play-menu luvland)))))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas
                                    (event luv:canvas-gamepad-button-release-event))
  (declare (ignore canvas))
  (let ((key (cdr (assoc (luv:canvas-gamepad-event-button event) *gamepad-shell-keys*))))
    (when (and key (shell-has-input-p luvland))
      (play-key luvland key nil))))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas
                                    (event luv:canvas-gamepad-added-event))
  (declare (ignore canvas))
  (push (cons (luv:canvas-gamepad-event-gamepad event) (luv:canvas-gamepad-event-name event))
        (luvland-gamepads luvland))
  (message luvland "~A connected: the ~:[Guide button~;A button~] opens the play menu."
           (or (luv:canvas-gamepad-event-name event) "A gamepad")
           (null (luvland-focus luvland))))

(defmethod luv:handle-canvas-event ((luvland luvland) canvas
                                    (event luv:canvas-gamepad-removed-event))
  (declare (ignore canvas))
  (setf (luvland-gamepads luvland)
        (remove (luv:canvas-gamepad-event-gamepad event) (luvland-gamepads luvland) :key #'car))
  (message luvland "Gamepad disconnected."))
