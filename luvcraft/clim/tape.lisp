(in-package #:luvcraft.clim)

;;; The tape's question: a small panel that asks for a YouTube code.
;;;
;;; Focusing a tape (luvcraft/tape.lisp) opens this in the middle of the view,
;;; the way the legend opens: a McCLIM frame drawn into a GPU mirror and
;;; composited as a HUD overlay.  Since the focus offers no command table, every
;;; key the window layer does not claim comes here: letters and digits go into
;;; the draft, RET answers, Esc and Tab put the question away unanswered.

(defparameter *tape-prompt-width* 560)
(defparameter *tape-prompt-height* 178)
(defparameter *tape-prompt-margin* 26)

(defparameter *tape-prompt-panel-ink* (make-rgb-color 0.11 0.11 0.105))
(defparameter *tape-prompt-field-ink* (make-rgb-color 0.06 0.06 0.058))
(defparameter *tape-prompt-text-ink* (make-rgb-color 0.91 0.89 0.82))
(defparameter *tape-prompt-muted-ink* (make-rgb-color 0.60 0.60 0.55))
(defparameter *tape-prompt-edge-ink* (make-rgb-color 0.42 0.42 0.38))
(defparameter *tape-prompt-accent-ink* (make-rgb-color 0.85 0.62 0.30))
(defparameter *tape-prompt-warn-ink* (make-rgb-color 0.85 0.40 0.32))

(defclass tape-prompt-pane (application-pane) ())

(define-application-frame luvcraft-tape-prompt ()
  ((session :initarg :session :reader tape-prompt-session)
   (x :initarg :x :reader tape-prompt-x)
   (y :initarg :y :reader tape-prompt-y)
   (z :initarg :z :reader tape-prompt-z)
   (draft :initform "" :accessor tape-prompt-draft)
   (complaint :initform nil :accessor tape-prompt-complaint
              :documentation "What was wrong with the last answer, or NIL."))
  (:menu-bar nil)
  (:panes (sheet (make-pane 'tape-prompt-pane)))
  (:layouts
   (default
    (horizontally (:width *tape-prompt-width* :height *tape-prompt-height*)
      sheet))))

(defmethod handle-repaint ((pane tape-prompt-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (draft (tape-prompt-draft frame))
         (margin *tape-prompt-margin*)
         (right (- *tape-prompt-width* margin))
         (field-top 74)
         (field-bottom 116))
    (with-bounding-rectangle* (left top right-edge bottom) pane
      (progn
        (draw-rectangle* pane left top right-edge bottom
                         :ink *tape-prompt-panel-ink*)
        (draw-rectangle* pane left top right-edge bottom :filled nil
                         :line-thickness 2 :ink *tape-prompt-edge-ink*)
        (draw-text* pane "tape" margin 34
                    :align-y :center :text-size 22 :text-face :bold
                    :ink *tape-prompt-text-ink*)
        (draw-text* pane "Esc leaves it blank" right 34
                    :align-x :right :align-y :center :text-size 14
                    :ink *tape-prompt-muted-ink*)
        (draw-text* pane "which YouTube video?  a code or a link" margin 58
                    :align-y :center :text-size 14
                    :ink *tape-prompt-muted-ink*)
        ;; The field.
        (draw-rectangle* pane margin field-top right field-bottom
                         :ink *tape-prompt-field-ink*)
        (draw-rectangle* pane margin field-top right field-bottom
                         :filled nil :line-thickness 1
                         :ink *tape-prompt-accent-ink*)
        (let* ((text-x (+ margin 12))
               (text-y (/ (+ field-top field-bottom) 2))
               (width (text-size pane draft :text-style (make-text-style nil nil 18))))
          (draw-text* pane draft text-x text-y
                      :align-y :center :text-size 18
                      :ink *tape-prompt-text-ink*)
          ;; The caret, a bar after the draft.
          (draw-rectangle* pane (+ text-x width 2) (- text-y 11)
                           (+ text-x width 4) (+ text-y 11)
                           :ink *tape-prompt-accent-ink*))
        (draw-text* pane
                    (or (tape-prompt-complaint frame)
                        "RET fetches it; the reel becomes a film when it lands")
                    margin (- *tape-prompt-height* 30)
                    :align-y :center :text-size 14
                    :ink (if (tape-prompt-complaint frame)
                             *tape-prompt-warn-ink*
                             *tape-prompt-muted-ink*))))))

(defun repaint-tape-prompt (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (check-type mirror mcluv:luv-gpu-mirror)
    (mcluv:repaint-gpu-mirror mirror))
  frame)

;;; ---------------------------------------------------------------------
;;; The overlay: the panel in the middle of the screen, as the legend is.

(defparameter *tape-prompt-scale* 1.7
  "How much larger than its own pixels the panel is shown: a short question
in the middle of the view wants to be read from where the player stands.")

(defun tape-prompt-screen-state (overlay)
  "Place the panel, scaled, in the middle of the viewport."
  (let* ((source-size (mcluv:widget-overlay-logical-size overlay))
         (viewport-size
           (luv:canvas-extent
            (luvcraft:luvcraft-session-context
             (mcluv:widget-overlay-session overlay))))
         (source-width (first source-size))
         (source-height (second source-size))
         (viewport-width (first viewport-size))
         (viewport-height (second viewport-size))
         (scale (min *tape-prompt-scale*
                     (/ (- viewport-width 48.0) source-width)
                     (/ (- viewport-height 48.0) source-height)))
         (half-width (/ (* source-width scale) viewport-width))
         (half-height (/ (* source-height scale) viewport-height)))
    (make-array
     12 :element-type 'single-float
     :initial-contents
     (mapcar (lambda (value) (coerce value 'single-float))
             (list 0.0 0.0 0.0 1.0
                   half-width 0.0 0.0 0.0
                   0.0 half-height 0.0 0.0)))))

(defclass luvcraft-tape-prompt-overlay (mcluv:luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage ((overlay luvcraft-tape-prompt-overlay))
  (declare (ignore overlay))
  :hud)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-tape-prompt-overlay) session pass surface-texture)
  (declare (ignore pass))
  (mcluv:prepare-direct-widget-overlay
   overlay session surface-texture (tape-prompt-screen-state overlay))
  overlay)

;;; The question is asked by the tape, not by the crosshair: nothing targets
;;; the panel itself, and the camera does no more than lean in a little.

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-tape-prompt-overlay) session)
  (declare (ignore overlay session))
  nil)

(defun close-tape-prompt (overlay session)
  (when (eq overlay (luvcraft:luvcraft-session-modal-focus session))
    (luvcraft:unfocus-luvcraft-session session))
  (luvcraft:remove-luvcraft-overlay session overlay)
  nil)

(defun answer-tape-prompt (overlay session)
  "Take the draft as an answer: start the download and put the panel away,
or say what is wrong with it and keep asking."
  (let* ((frame (mcluv:widget-overlay-frame overlay))
         (draft (tape-prompt-draft frame))
         (video-id (luvcraft:youtube-video-id draft)))
    (cond ((zerop (length (string-trim " " draft)))
           (close-tape-prompt overlay session))
          ((null video-id)
           (setf (tape-prompt-complaint frame)
                 "that is not a YouTube code or link I recognise")
           (repaint-tape-prompt frame))
          (t
           (luvcraft:begin-tape-download
            session (tape-prompt-x frame) (tape-prompt-y frame)
            (tape-prompt-z frame) video-id)
           (close-tape-prompt overlay session)))))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-tape-prompt-overlay) session canvas
     (event luv:canvas-key-press-event))
  (let* ((frame (mcluv:widget-overlay-frame overlay))
         (key (luv:canvas-key-event-key-name event))
         (character (luv:canvas-key-event-character event))
         (paste-p (and (eq key :v)
                       (intersection '(:super :control)
                                     (luv:canvas-key-event-modifiers event)))))
    (case key
      ((:escape :tab)
       (close-tape-prompt overlay session))
      ((:return :keypad-enter)
       (answer-tape-prompt overlay session))
      ((:v)
       ;; Cmd-V or Ctrl-V pastes: a link is what a player has on the
       ;; clipboard, and typing eleven random letters is nobody's idea of fun.
       (if paste-p
           (alexandria:when-let ((text (luv:canvas-clipboard-text canvas)))
             (setf (tape-prompt-draft frame)
                   (concatenate 'string (tape-prompt-draft frame)
                                (string-trim
                                 '(#\Space #\Tab #\Return #\Newline)
                                 (subseq text 0 (position #\Newline text))))
                   (tape-prompt-complaint frame) nil)
             (repaint-tape-prompt frame))
           (when (and character (graphic-char-p character))
             (setf (tape-prompt-draft frame)
                   (concatenate 'string (tape-prompt-draft frame)
                                (string character))
                   (tape-prompt-complaint frame) nil)
             (repaint-tape-prompt frame))))
      (:backspace
       (let ((draft (tape-prompt-draft frame)))
         (when (plusp (length draft))
           (setf (tape-prompt-draft frame)
                 (subseq draft 0 (1- (length draft)))
                 (tape-prompt-complaint frame) nil)
           (repaint-tape-prompt frame))))
      (t
       (when (and character (graphic-char-p character)
                  (< (length (tape-prompt-draft frame)) 120))
         (setf (tape-prompt-draft frame)
               (concatenate 'string (tape-prompt-draft frame)
                            (string character))
               (tape-prompt-complaint frame) nil)
         (repaint-tape-prompt frame))))
    t))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-tape-prompt-overlay) session canvas
     (event luv:canvas-event))
  (declare (ignore overlay session canvas event))
  t)

;;; ---------------------------------------------------------------------
;;; Opening.

(defmethod luvcraft:open-tape-prompt
    ((session luvcraft:luvcraft-session) x y z)
  "Create, attach, and focus the panel asking which video the tape at X,Y,Z
should fetch."
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
              'luvcraft-tape-prompt :frame-manager manager :enable t
              :session session :x x :y y :z z))))
    (setf (frame-pretty-name frame) "tape")
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance 'luvcraft-tape-prompt-overlay
                            :session session :frame frame :mirror mirror)))
      (setf (mcluv:mirror-compositor mirror) overlay)
      (luvcraft:add-luvcraft-overlay session overlay)
      (repaint-tape-prompt frame)
      overlay)))
