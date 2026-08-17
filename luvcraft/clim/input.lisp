(in-package #:luvcraft.clim)

;;; Keyboard dispatch.
;;;
;;; This is the method that used to live in LUVCRAFT/RENDER.LISP as a ladder of
;;; WHEN ... RETURN-FROM guards, one per global key, each of which had to be
;;; placed above or below the focus dispatch by hand.  What is left after the
;;; verbs became commands is the part that was never really about verbs: a
;;; keystroke lookup, the focus's claim on everything else, and the held-key
;;; state the player controller reads every physics step.
;;;
;;; It runs on the canvas thread, and so does every command it executes:
;;; EXECUTE-FRAME-COMMAND applies a command directly while the frame has no
;;; process of its own.  That is what makes it safe for a command to touch the
;;; session and the world at all.

(defmethod luv:handle-canvas-event
    ((session luvcraft:luvcraft-session) canvas
     (event luv:canvas-key-press-event))
  ;; An auto-repeat is one keystroke still held down, not another one: a verb
  ;; fires once, while a movement key wants every repeat it can get.
  (unless (luv:canvas-key-event-repeat-p event)
    (alexandria:when-let ((command (luvcraft-key-command session event)))
      (execute-frame-command (luvcraft-session-frame session) command)
      ;; The release matching a focus-changing Tab is swallowed below, so a
      ;; shell that has just been focused does not read a stray Tab-up.
      (when (eq :tab (luv:canvas-key-event-key-name event))
        (setf (luvcraft:luvcraft-session-focus-toggle-tab-down-p session) t))
      (return-from luv:handle-canvas-event nil)))
  (when (luvcraft:dispatch-luvcraft-focus-event session canvas event)
    (return-from luv:handle-canvas-event nil))
  (let ((key (luv:canvas-key-event-key-name event)))
    (setf (gethash key (luvcraft:luvcraft-session-pressed-keys session)) t)
    (when (and (eq key :space) (not (luv:canvas-key-event-repeat-p event)))
      (setf (luvcraft:luvcraft-session-jump-requested-p session) t)))
  nil)

(defmethod luv:handle-canvas-event
    ((session luvcraft:luvcraft-session) canvas
     (event luv:canvas-key-release-event))
  (when (and (eq :tab (luv:canvas-key-event-key-name event))
             (luvcraft:luvcraft-session-focus-toggle-tab-down-p session))
    (setf (luvcraft:luvcraft-session-focus-toggle-tab-down-p session) nil)
    (return-from luv:handle-canvas-event nil))
  (when (luvcraft:dispatch-luvcraft-focus-event session canvas event)
    (return-from luv:handle-canvas-event nil))
  (remhash (luv:canvas-key-event-key-name event)
           (luvcraft:luvcraft-session-pressed-keys session))
  nil)
