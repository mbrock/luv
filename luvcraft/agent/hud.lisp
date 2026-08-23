(in-package #:luvcraft.agent)

;;; The cassette HUD: the current turn drawn as cards in the corner (#9K823O,
;;; #14ZP6S).
;;;
;;; A McCLIM frame composited over the view, the way the hotbar and the legend
;;; are.  Every frame the overlay reads the turn and repaints when anything
;;; it shows has changed -- a call's status, its elapsed tenths of a second, a
;;; new line of thought -- so a running call's clock ticks and a finished one
;;; flips in place.  The drawing itself is presentation methods under the
;;; cassette view, one per kind of record, so a wall can draw the same cards
;;; later from the same objects.

(defparameter *agent-hud-width* 640)
(defparameter *agent-hud-height* 560)
(defparameter *agent-hud-margin* 16)
(defparameter *agent-hud-columns* 62
  "Characters per wrapped line at the body text size.")

(defparameter *hud-panel-ink* (make-rgb-color 0.09 0.09 0.085))
(defparameter *hud-edge-ink* (make-rgb-color 0.36 0.36 0.33))
(defparameter *hud-text-ink* (make-rgb-color 0.91 0.89 0.82))
(defparameter *hud-muted-ink* (make-rgb-color 0.58 0.58 0.53))
(defparameter *hud-thought-ink* (make-rgb-color 0.55 0.80 0.82))
(defparameter *hud-card-ink* (make-rgb-color 0.15 0.15 0.14))
(defparameter *hud-output-ink* (make-rgb-color 0.70 0.72 0.70))
(defparameter *hud-error-ink* (make-rgb-color 0.90 0.45 0.40))
(defparameter *hud-running-ink* (make-rgb-color 0.90 0.72 0.30))
(defparameter *hud-ok-ink* (make-rgb-color 0.55 0.78 0.50))

;;; ---------------------------------------------------------------------
;;; Tool identity: one colour per command, the way the stylesheet had one
;;; template pair per tool name.

(defgeneric command-ink (command)
  (:documentation "The colour that identifies COMMAND's cassettes.")
  (:method ((command t)) (make-rgb-color 0.35 0.72 0.70)))

(defmethod command-ink ((command (eql 'com-place-block-at)))
  (make-rgb-color 0.92 0.55 0.28))
(defmethod command-ink ((command (eql 'com-block-at)))
  (make-rgb-color 0.40 0.78 0.52))
(defmethod command-ink ((command (eql 'com-where-am-i)))
  (make-rgb-color 0.90 0.72 0.30))
(defmethod command-ink ((command (eql 'com-eval)))
  (make-rgb-color 0.68 0.85 0.35))
(defmethod command-ink ((command (eql 'com-describe-handle)))
  (make-rgb-color 0.70 0.55 0.90))

(defun tool-call-ink (call)
  (command-ink (command-tool-command (tool-call-tool call))))

;;; ---------------------------------------------------------------------
;;; Wrapping

(defun wrap-words (text columns)
  "TEXT as lines of at most COLUMNS characters, broken on spaces."
  (let ((lines '()) (current (make-string-output-stream)) (length 0))
    (flet ((flush ()
             (let ((line (get-output-stream-string current)))
               (when (plusp (length line)) (push line lines)))
             (setf length 0)))
      (dolist (paragraph (uiop:split-string text :separator '(#\Newline)))
        (dolist (word (remove "" (uiop:split-string paragraph :separator '(#\Space))
                              :test #'string=))
          (loop while (> (length word) columns)
                do (flush)
                   (push (subseq word 0 columns) lines)
                   (setf word (subseq word columns)))
          (when (and (plusp length) (> (+ length 1 (length word)) columns))
            (flush))
          (when (plusp length)
            (write-char #\Space current)
            (incf length))
          (write-string word current)
          (incf length (length word)))
        (flush)))
    (nreverse lines)))

;;; ---------------------------------------------------------------------
;;; The cassette view.
;;;
;;; Each presentation method draws from the stream's cursor row downward and
;;; leaves the cursor below what it drew, so a sequence of PRESENT calls
;;; stacks cards the way a stylesheet stacks templates.
;;;
;;; They are invoked with FUNCALL-PRESENTATION-GENERIC-FUNCTION rather than
;;; PRESENT: drawing inside WITH-OUTPUT-AS-PRESENTATION reaches the GPU
;;; medium as a replayed output record, and nothing shows.  Until the HUD
;;; wants clickable presentations, the methods are called as plain drawing.

(defparameter *cassette-line-height* 21)
(defparameter *cassette-text-size* 15)
(defparameter *cassette-output-lines* 4)

(defun cassette-left (stream)
  (declare (ignore stream))
  *agent-hud-margin*)

(defun cassette-right (stream)
  (- (bounding-rectangle-width stream) *agent-hud-margin*))

(defun cassette-top (stream)
  (nth-value 1 (stream-cursor-position stream)))

(defun advance-cassette (stream bottom)
  (setf (stream-cursor-position stream) (values (cassette-left stream) bottom)))

(defun draw-cassette-lines (stream lines top &key (ink *hud-output-ink*)
                                                (indent 0) (size *cassette-text-size*)
                                                (face :roman))
  "Draw LINES from TOP, one per line height; return the bottom."
  (let ((y top))
    (dolist (line lines)
      (draw-text* stream line (+ (cassette-left stream) indent) (+ y 11)
                  :align-y :center :text-size size :text-face face :ink ink)
      (incf y *cassette-line-height*))
    y))

(defun status-spine (call)
  "The spine glyph and its ink for CALL's status."
  (ecase (tool-call-status call)
    (:running (values "~" *hud-running-ink*))
    (:ok (values "+" (tool-call-ink call)))
    (:error (values "!" *hud-error-ink*))))

(defun tool-call-window-lines (call &optional (columns (- *agent-hud-columns* 2)))
  "The lines a cassette's window shows for CALL: the output, folded to
*CASSETTE-OUTPUT-LINES* with a count of what is left."
  (let ((lines (wrap-words
                (if (string= (tool-call-output call) "")
                    (if (eq :running (tool-call-status call)) "running" "")
                    (tool-call-output call))
                columns)))
    (if (> (length lines) *cassette-output-lines*)
        (append (subseq lines 0 (1- *cassette-output-lines*))
                (list (format nil "... ~D more lines"
                              (- (length lines) (1- *cassette-output-lines*)))))
        lines)))

(defparameter *cassette-header-height* 28)

(defun tool-call-cassette-height (call &optional (columns (- *agent-hud-columns* 2)))
  "How tall CALL's cassette is, header and window."
  (let ((lines (tool-call-window-lines call columns)))
    (+ *cassette-header-height* (* *cassette-line-height* (length lines))
       (if lines 6 0))))

(define-presentation-method present
    (call (type tool-call) stream (view cassette-view) &key)
  (let* ((left (cassette-left stream))
         (right (cassette-right stream))
         (top (cassette-top stream))
         (header-height *cassette-header-height*)
         (output-lines (tool-call-window-lines call))
         (bottom (+ top (tool-call-cassette-height call))))
    ;; The card.
    (draw-rectangle* stream left top right bottom :ink *hud-card-ink*)
    ;; The spine.
    (multiple-value-bind (glyph spine-ink) (status-spine call)
      (draw-rectangle* stream left top (+ left 26) (+ top header-height) :ink spine-ink)
      (draw-text* stream glyph (+ left 13) (+ top 14)
                  :align-x :center :align-y :center :text-size 16 :text-face :bold
                  :ink *hud-panel-ink*))
    ;; Name, arguments, clock.
    (draw-text* stream (tool-call-name call) (+ left 34) (+ top 14)
                :align-y :center :text-size 16 :text-face :bold
                :ink (tool-call-ink call))
    (let ((name-width (text-size stream (tool-call-name call)
                                 :text-style (make-text-style nil :bold 16)))
          (clock (format nil "~,1Fs" (tool-call-elapsed-seconds call))))
      (draw-text* stream clock (- right 8) (+ top 14)
                  :align-x :right :align-y :center :text-size 13 :ink *hud-muted-ink*)
      (let* ((arguments (tool-call-arguments-text call))
             (available (- right 8 50 (+ left 34 name-width 10)))
             (arguments-lines (wrap-words arguments
                                          (max 8 (floor available 8)))))
        (when arguments-lines
          (draw-text* stream (first arguments-lines) (+ left 34 name-width 10) (+ top 14)
                      :align-y :center :text-size 13 :ink *hud-muted-ink*))))
    ;; The window.
    (draw-cassette-lines stream output-lines (+ top header-height 2)
                         :indent 12
                         :ink (if (eq :error (tool-call-status call))
                                  *hud-error-ink*
                                  *hud-output-ink*))
    (advance-cassette stream (+ bottom 6))))

(define-presentation-method present
    (turn (type turn) stream (view cassette-view) &key)
  ;; Prompt.
  (let ((top (cassette-top stream)))
    (advance-cassette
     stream
     (draw-cassette-lines stream (wrap-words (format nil "> ~A" (turn-prompt turn))
                                             *agent-hud-columns*)
                          top :ink *hud-text-ink* :face :bold)))
  ;; Thought bubble, as text for now.
  (unless (string= (turn-thought turn) "")
    (let* ((lines (wrap-words (turn-thought turn) *agent-hud-columns*))
           (lines (if (> (length lines) 5) (last lines 5) lines)))
      (advance-cassette
       stream
       (+ 4 (draw-cassette-lines stream lines (+ 2 (cassette-top stream))
                                 :ink *hud-thought-ink* :face :italic)))))
  ;; Calls, oldest first.
  (dolist (call (turn-calls-in-order turn))
    (funcall-presentation-generic-function present call 'tool-call stream view))
  ;; What it said.
  (unless (string= (turn-text turn) "")
    (advance-cassette
     stream
     (draw-cassette-lines stream (wrap-words (turn-text turn) *agent-hud-columns*)
                          (+ 2 (cassette-top stream)) :ink *hud-text-ink*)))
  (when (turn-error turn)
    (advance-cassette
     stream
     (draw-cassette-lines stream (wrap-words (format nil "failed: ~A" (turn-error turn))
                                             *agent-hud-columns*)
                          (+ 2 (cassette-top stream)) :ink *hud-error-ink*))))

;;; ---------------------------------------------------------------------
;;; The frame and the overlay

(defclass agent-hud-pane (application-pane) ())

(define-application-frame luvcraft-agent-hud ()
  ((agent :initarg :agent :reader agent-hud-agent)
   (visible-state :initform nil :accessor agent-hud-visible-state))
  (:menu-bar nil)
  (:panes (sheet (make-pane 'agent-hud-pane)))
  (:layouts
   (default
    (horizontally (:width *agent-hud-width* :height *agent-hud-height*) sheet))))

(defun agent-hud-turn (frame)
  (world-agent-current-turn (agent-hud-agent frame)))

(defun agent-hud-state (frame)
  "Everything the HUD shows, as a key: repaint when it changes."
  (let ((turn (agent-hud-turn frame)))
    (and turn
         (list (turn-status turn) (length (turn-thought turn)) (length (turn-text turn))
               (and (turn-error turn) t)
               (mapcar (lambda (call)
                         (list (tool-call-status call)
                               (round (tool-call-elapsed-seconds call) 0.1)
                               (length (tool-call-output call))))
                       (turn-calls turn))))))

(defmethod handle-repaint ((pane agent-hud-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (agent (agent-hud-agent frame))
         (turn (agent-hud-turn frame)))
    (with-bounding-rectangle* (left top right bottom) pane
      (progn
        (draw-rectangle* pane left top right bottom :ink *hud-panel-ink*)
        (draw-rectangle* pane left top right bottom :filled nil
                         :line-thickness 2 :ink *hud-edge-ink*)
        (draw-text* pane "agent" *agent-hud-margin* 26
                    :align-y :center :text-size 20 :text-face :bold :ink *hud-text-ink*)
        (draw-text* pane (format nil "~A~@[ · ~(~A~)~]"
                                 (openai:agent-model agent)
                                 (and turn (turn-status turn)))
                    (- right *agent-hud-margin*) 26
                    :align-x :right :align-y :center :text-size 14 :ink *hud-muted-ink*)
        (setf (stream-cursor-position pane) (values *agent-hud-margin* 50))
        (if turn
            (funcall-presentation-generic-function
             present turn 'turn pane +cassette-view+)
            (draw-text* pane "waiting to be asked" *agent-hud-margin* 60
                        :align-y :center :text-size 13 :text-face :italic
                        :ink *hud-muted-ink*))))))

(defun repaint-agent-hud (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (check-type mirror mcluv:luv-gpu-mirror)
    (mcluv:repaint-gpu-mirror mirror))
  (setf (agent-hud-visible-state frame) (agent-hud-state frame))
  frame)

(defclass luvcraft-agent-hud-overlay (mcluv:luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage ((overlay luvcraft-agent-hud-overlay))
  (declare (ignore overlay))
  :hud)

(defun agent-hud-top-margin (session)
  "Return the HUD's ordinary margin below application-level top surfaces."
  (multiple-value-bind (left top right bottom)
      (luvcraft:luvcraft-session-focus-insets session)
    (declare (ignore left right bottom))
    (+ 20.0 top)))

(defun agent-hud-screen-state (overlay)
  "Top right corner, at its own size.

One pane pixel is one screen pixel unless the viewport cannot hold it:
shrinking the pane shrinks its type, and small type is what reads as soft."
  (let* ((session (mcluv:widget-overlay-session overlay))
         (source-size (mcluv:widget-overlay-logical-size overlay))
         (viewport-size
           (multiple-value-list
            (luv:canvas-logical-size
             (luvcraft:luvcraft-session-canvas
              session))))
         (source-width (first source-size))
         (source-height (second source-size))
         (viewport-width (first viewport-size))
         (viewport-height (second viewport-size))
         (top-margin (agent-hud-top-margin session))
         (scale (min 1.0
                     (/ (- viewport-width 40.0) source-width)
                     (/ (- viewport-height 76.0 top-margin) source-height)))
         (half-width (/ (* source-width scale) viewport-width))
         (half-height (/ (* source-height scale) viewport-height))
         (margin-x (/ 20.0 viewport-width))
         (margin-y (/ top-margin viewport-height))
         (center-x (- 1.0 margin-x half-width))
         (center-y (+ -1.0 margin-y half-height)))
    (make-array
     12 :element-type 'single-float
     :initial-contents
     (mapcar (lambda (value) (coerce value 'single-float))
             (list center-x center-y 0.0 1.0
                   half-width 0.0 0.0 0.0
                   0.0 half-height 0.0 0.0)))))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-agent-hud-overlay) session pass surface-texture)
  (declare (ignore pass))
  (mcluv:prepare-direct-widget-overlay
   overlay session surface-texture (agent-hud-screen-state overlay))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-agent-hud-overlay) session)
  (declare (ignore session))
  (let ((frame (mcluv:widget-overlay-frame overlay)))
    (unless (equal (agent-hud-state frame) (agent-hud-visible-state frame))
      (repaint-agent-hud frame)))
  overlay)

;;; A HUD, not a thing in the world: never targeted, never focused.

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-agent-hud-overlay) session)
  (declare (ignore overlay session))
  nil)

;;; ---------------------------------------------------------------------
;;; Opening and closing

(defun find-agent-hud (session)
  (find-if (lambda (overlay) (typep overlay 'luvcraft-agent-hud-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun open-agent-hud (&key (agent (or *agent* (make-world-agent)))
                         (session (world-agent-session agent)))
  "Attach the cassette HUD for AGENT to its session."
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'mcluv:luv-frame-manager :port port)))
         (frame
           (let ((mcluv:*embedded-mirror-target*
                   (luvcraft:luvcraft-session-canvas session))
                 (mcluv:*embedded-mirror-context*
                   (luvcraft:luvcraft-session-context session))
                 (mcluv:*embedded-mirror-device*
                   (luvcraft:luvcraft-session-device session)))
             (make-application-frame
              'luvcraft-agent-hud :frame-manager manager :enable t
              :agent agent))))
    (setf (frame-pretty-name frame) "luvcraft agent")
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance 'luvcraft-agent-hud-overlay
                            :session session :frame frame :mirror mirror)))
      (setf (mcluv:mirror-compositor mirror) overlay)
      (luvcraft:add-luvcraft-overlay session overlay)
      (repaint-agent-hud frame)
      overlay)))

(defun close-agent-hud (&key (session (and *agent* (world-agent-session *agent*))))
  "Put the agent HUD away."
  (alexandria:when-let ((overlay (and session (find-agent-hud session))))
    (luvcraft:remove-luvcraft-overlay session overlay)
    t))

(defun toggle-agent-hud (&key (agent (or *agent* (make-world-agent))))
  (if (find-agent-hud (world-agent-session agent))
      (close-agent-hud :session (world-agent-session agent))
      (open-agent-hud :agent agent)))
