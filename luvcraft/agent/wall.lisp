(in-package #:luvcraft.agent)

;;; The agent's wall: the transcript on a terminal wall, drawn the way the
;;; phone and the shell walls are drawn -- Ghostty cells as Slug glyph
;;; geometry, sharp at any distance (#GJUPMQ) -- rather than through a McCLIM
;;; mirror.
;;;
;;; The wall is a TERMINAL-DISPLAY with no PTY.  What it shows is VT bytes the
;;; agent writes into its Ghostty terminal: the prompt the player types at it,
;;; the thought as it streams, and each tool call as an ANSI cassette (#9K823O)
;;; once it has finished.  The terminal does the scrolling and the wrapping;
;;; we do the colours.
;;;
;;; Two threads meet here and one thing crosses between them, the way the
;;; Telegram wall already does it: the turn's thread formats text and posts it
;;; to an outbox; the canvas thread drains the outbox into the terminal at the
;;; frame boundary, where it also reads the terminal to draw it.  Nothing
;;; writes the terminal from anywhere else.

(defclass agent-terminal-display (luvcraft::terminal-display)
  ((agent :initarg :agent :accessor agent-wall-agent)
   (outbox :initform (sb-concurrency:make-mailbox :name "agent wall outbox")
           :reader agent-wall-outbox)
   (draft :initform "" :accessor agent-wall-draft
          :documentation "What the player has typed at the wall so far.")
   (line-open-p :initform nil :accessor agent-wall-line-open-p
                :documentation "Whether streamed text has left the cursor mid-line.")
   (printed :initform (make-hash-table :test #'eq) :reader agent-wall-printed
            :documentation "Per turn, (THOUGHT-LENGTH . TEXT-LENGTH) already written.")
   (observer :initform nil :accessor agent-wall-observer))
  (:documentation "A terminal wall showing one agent's transcript."))

;;; ---------------------------------------------------------------------
;;; Writing

(defun crlf (text)
  "TEXT with every newline made a carriage return and line feed.

There is no tty discipline between us and Ghostty to do it."
  (with-output-to-string (out)
    (loop for char across text
          do (when (char= char #\Newline) (write-char #\Return out))
             (write-char char out))))

(defun agent-wall-write (display text)
  "Queue TEXT for DISPLAY's terminal; the frame boundary writes it."
  (sb-concurrency:send-message (agent-wall-outbox display) (crlf text))
  display)

(defun agent-wall-close-line (display)
  (when (agent-wall-line-open-p display)
    (agent-wall-write display (string #\Newline))
    (setf (agent-wall-line-open-p display) nil)))

(defmethod luvcraft:refresh-luvcraft-overlay :before
    ((display agent-terminal-display) session)
  (declare (ignore session))
  (let ((terminal (luvcraft::terminal-display-terminal display))
        (wrote-p nil))
    (loop for (text received-p) = (multiple-value-list
                                   (sb-concurrency:receive-message-no-hang
                                    (agent-wall-outbox display)))
          while received-p
          do (ghostty:write-terminal terminal text)
             (setf wrote-p t))
    (when wrote-p
      (setf (luvcraft::terminal-display-dirty-p display) t))))

;;; ---------------------------------------------------------------------
;;; ANSI: the cassette as escape sequences.

(defclass ansi-view (textual-view) ()
  (:documentation "Text with SGR colour, for a terminal wall."))

(defparameter +ansi-view+ (make-instance 'ansi-view))

(defvar *ansi-columns* 80
  "The width of the terminal an ANSI presentation is being written for.")

(defun sgr-foreground (color)
  (multiple-value-bind (r g b) (color-rgb color)
    (format nil "~C[38;2;~D;~D;~Dm" #\Escape
            (round (* 255 r)) (round (* 255 g)) (round (* 255 b)))))

(defun sgr-background (color)
  (multiple-value-bind (r g b) (color-rgb color)
    (format nil "~C[48;2;~D;~D;~Dm" #\Escape
            (round (* 255 r)) (round (* 255 g)) (round (* 255 b)))))

(defun sgr-reset () (format nil "~C[0m" #\Escape))
(defun sgr-bold () (format nil "~C[1m" #\Escape))
(defun sgr-dim () (format nil "~C[2m" #\Escape))
(defun sgr-italic () (format nil "~C[3m" #\Escape))

(define-presentation-method present
    (call (type tool-call) stream (view ansi-view) &key)
  (multiple-value-bind (glyph spine-ink) (status-spine call)
    (let* ((ink (tool-call-ink call))
           (name (tool-call-name call))
           (arguments (tool-call-arguments-text call))
           (clock (format nil "~,1Fs" (tool-call-elapsed-seconds call)))
           (head (format nil " ~A ~A ~A" glyph name arguments))
           (pad (max 1 (- *ansi-columns* (length head) (length clock) 1))))
      ;; Header: spine, name, arguments, clock at the right edge.
      (format stream "~A~A ~A ~A~A~A~A~A ~A~A~A~A~A~A~A~%"
              (sgr-background spine-ink) (sgr-foreground *hud-panel-ink*) glyph
              (sgr-reset) (sgr-bold) (sgr-foreground ink) name (sgr-reset)
              (sgr-dim) arguments (sgr-reset)
              (make-string pad :initial-element #\Space)
              (sgr-dim) clock (sgr-reset))
      ;; Window: a few lines of output, indented under the spine.
      (let* ((text (tool-call-output call))
             (lines (if (string= text "") '() (wrap-words text (- *ansi-columns* 5))))
             (shown (if (> (length lines) *cassette-output-lines*)
                        (append (subseq lines 0 (1- *cassette-output-lines*))
                                (list (format nil "... ~D more lines"
                                              (- (length lines)
                                                 (1- *cassette-output-lines*)))))
                        lines)))
        (dolist (line shown)
          (format stream "   ~A~A~A~%"
                  (if (eq :error (tool-call-status call))
                      (sgr-foreground *hud-error-ink*)
                      (sgr-foreground *hud-output-ink*))
                  line (sgr-reset)))))))

;;; ---------------------------------------------------------------------
;;; Following a turn

(defun agent-wall-columns (display)
  (values (ghostty:terminal-size (luvcraft::terminal-display-terminal display))))

(defun agent-wall-note-turn-started (display turn)
  (agent-wall-close-line display)
  (setf (gethash turn (agent-wall-printed display)) (cons 0 0))
  ;; Whatever prompt line was waiting -- typed here or not -- is replaced by
  ;; the turn's own first line.
  (agent-wall-write display
                    (format nil "~C~C[2K~A> ~A~A~%" #\Return #\Escape
                            (sgr-bold) (turn-prompt turn) (sgr-reset))))

(defun agent-wall-note-streamed (display turn kind)
  "Write whatever of TURN's thought or text (KIND) has not been written."
  (let* ((printed (or (gethash turn (agent-wall-printed display))
                      (setf (gethash turn (agent-wall-printed display)) (cons 0 0))))
         (whole (ecase kind (:thought (turn-thought turn)) (:text (turn-text turn))))
         (done (ecase kind (:thought (car printed)) (:text (cdr printed)))))
    (when (> (length whole) done)
      (let ((fresh (subseq whole done)))
        (unless (agent-wall-line-open-p display)
          (agent-wall-write display
                            (ecase kind
                              (:thought (format nil "~A~A  " (sgr-italic)
                                                (sgr-foreground *hud-thought-ink*)))
                              (:text (format nil "~A" (sgr-foreground *hud-text-ink*))))))
        (agent-wall-write display fresh)
        (setf (agent-wall-line-open-p display) t)
        (ecase kind
          (:thought (setf (car printed) (length whole)))
          (:text (setf (cdr printed) (length whole))))))))

(defun agent-wall-note-call-started (display call)
  (agent-wall-close-line display)
  (agent-wall-write display
                    (format nil "~A ~~ ~A ~A~A~%" (sgr-dim) (tool-call-name call)
                            (tool-call-arguments-text call) (sgr-reset))))

(defun agent-wall-note-call-finished (display call)
  (agent-wall-close-line display)
  (let ((*ansi-columns* (agent-wall-columns display)))
    (agent-wall-write display (present-to-string call 'tool-call :view +ansi-view+))))

(defun agent-wall-note-turn-finished (display turn)
  (agent-wall-close-line display)
  (when (turn-error turn)
    (agent-wall-write display (format nil "~Afailed: ~A~A~%" (sgr-foreground *hud-error-ink*)
                                      (turn-error turn) (sgr-reset))))
  (agent-wall-write display (format nil "~%> "))
  (remhash turn (agent-wall-printed display)))

(defun make-agent-wall-observer (display)
  (lambda (agent kind object)
    (declare (ignore agent))
    (case kind
      (:turn-started (agent-wall-note-turn-started display object))
      (:thought (agent-wall-note-streamed display object :thought))
      (:text (agent-wall-note-streamed display object :text))
      (:call-started (agent-wall-note-call-started display object))
      (:call-finished (agent-wall-note-call-finished display object))
      (:turn-finished (agent-wall-note-turn-finished display object)))))

;;; ---------------------------------------------------------------------
;;; Typing at the wall

(defmethod luvcraft:handle-luvcraft-focus-event
    ((display agent-terminal-display) session canvas (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (let ((name (luv:canvas-key-event-key-name event))
        (char (luv:canvas-key-event-character event)))
    (cond
      ((eq name :return)
       (let ((prompt (string-trim " " (agent-wall-draft display))))
         (setf (agent-wall-draft display) "")
         (unless (string= prompt "")
           (ask prompt :agent (agent-wall-agent display)))))
      ((eq name :backspace)
       (let ((draft (agent-wall-draft display)))
         (when (plusp (length draft))
           (setf (agent-wall-draft display) (subseq draft 0 (1- (length draft))))
           (agent-wall-write display (format nil "~C ~C" #\Backspace #\Backspace)))))
      ((and char (graphic-char-p char)
            (not (intersection '(:control :meta :super) (luv:canvas-key-event-modifiers event))))
       (setf (agent-wall-draft display)
             (concatenate 'string (agent-wall-draft display) (string char)))
       (agent-wall-write display (string char))))
    t))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((display agent-terminal-display) session canvas (event luv:canvas-key-release-event))
  (declare (ignore session canvas event))
  t)

;;; ---------------------------------------------------------------------
;;; Opening one

(defun attach-agent-wall (display agent)
  "Give DISPLAY its AGENT: an observer in, a prompt out, no PTY at all."
  (setf (agent-wall-agent display) agent
        (agent-wall-observer display) (make-agent-wall-observer display))
  (add-agent-observer agent (agent-wall-observer display))
  (agent-wall-write display
                    (format nil "~A~Aluvcraft agent~A ~A~A~A~%~%> "
                            (sgr-bold) (sgr-foreground *hud-thought-ink*) (sgr-reset)
                            (sgr-dim) (openai:agent-model agent) (sgr-reset)))
  display)

(defmethod luvcraft:release-luvcraft-overlay :after ((display agent-terminal-display))
  (when (and (slot-boundp display 'agent) (agent-wall-observer display))
    (remove-agent-observer (agent-wall-agent display) (agent-wall-observer display))))

(defun open-agent-wall (&key (agent (or *agent* (make-world-agent)))
                          (session (world-agent-session agent))
                          hit)
  "Open AGENT's transcript on the terminal wall the player is looking at.

HIT may name another terminal block; by default it is the one under the
crosshair, as when activating a wall."
  (let ((hit (or hit (luvcraft::luvcraft-session-target session))))
    (unless hit
      (error "Look at a terminal block to open the agent's wall on it."))
    (let ((display
            (luvcraft::open-activated-wall-display
             session hit luvcraft::*terminal-block*
             (lambda (display) (attach-agent-wall display agent))
             :class 'agent-terminal-display)))
      (unless display
        (error "That is not a terminal wall."))
      display)))

(define-command (com-open-agent-wall :command-table luvcraft.clim::luvcraft-world
                                     :name "Open Agent Wall")
    ()
  "Put the agent's transcript on the terminal wall under the crosshair."
  (open-agent-wall :session (luvcraft.clim:luvcraft-command-session)))
