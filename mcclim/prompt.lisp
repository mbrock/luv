;;; A typed prompt line and a message window: the two halves of StumpWM's
;;; input bar, as workbench instruments.
;;;
;;; The prompt reads one line with completion and hands it to a continuation
;;; once its modal surface has closed; it never blocks a thread waiting for
;;; input.  Messages are passive text that time out.  Both are retained
;;; analytic GPU media drawn by the host's own final pass, like M-x.

(in-package #:mcluv)

(defconstant +prompt-width+ 720)
(defconstant +prompt-margin+ 14)
(defconstant +prompt-field-height+ 46)
(defconstant +prompt-row-height+ 28)
(defconstant +prompt-row-limit+ 8)
(defconstant +prompt-text-limit+ 400)
(defconstant +prompt-top+ 44
  "Where the prompt panel sits below the status line.")

(defconstant +message-margin+ 14)
(defconstant +message-line-height+ 22)
(defconstant +message-min-width+ 200)
(defconstant +message-max-width+ 900)
(defconstant +message-line-limit+ 24)
(defconstant +message-history-limit+ 64)

(defparameter *prompt-edge-ink* (command-menu-alpha-ink 0.38 0.44 0.40 0.88))
(defparameter *prompt-panel-ink* (command-menu-alpha-ink 0.040 0.044 0.042 0.94))
(defparameter *prompt-label-ink* (make-rgb-color 0.58 0.78 0.54))
(defparameter *prompt-text-ink* (make-rgb-color 0.93 0.92 0.87))
(defparameter *prompt-muted-ink* (make-rgb-color 0.60 0.62 0.57))
(defparameter *prompt-row-ink* (command-menu-alpha-ink 0.11 0.12 0.112 0.64))
(defparameter *prompt-selected-row-ink*
  (command-menu-alpha-ink 0.55 0.75 0.53 0.94))
(defparameter *prompt-selected-text-ink* (make-rgb-color 0.055 0.065 0.055))
(defparameter *message-panel-ink* (command-menu-alpha-ink 0.030 0.034 0.032 0.90))
(defparameter *message-error-ink* (make-rgb-color 0.96 0.55 0.48))

(defparameter *prompt-text-style* (make-text-style :fix nil 16))
(defparameter *message-text-style* (make-text-style :fix nil 15))

(defclass prompt-pane (transparent-gpu-application-pane) ())
(defclass message-pane (transparent-gpu-application-pane) ())

;;; ---------------------------------------------------------------------
;;; Prompt state.  Slots are prefixed: every instrument's state mixes into
;;; one workbench frame.

(defclass prompt-state ()
  ((prompt-label :initform "" :accessor prompt-label)
   (prompt-text :initform "" :accessor prompt-text)
   (prompt-completer :initform nil :accessor prompt-completer
                     :documentation "A function of the text returning candidate strings.")
   (prompt-candidates :initform nil :accessor prompt-candidates)
   (prompt-selected :initform nil :accessor prompt-selected
                    :documentation "The index of the chosen candidate, or NIL.")
   (prompt-continuation :initform nil :accessor prompt-continuation)
   (prompt-history :initform (make-hash-table :test #'equal)
                   :reader prompt-histories
                   :documentation "Earlier submissions by label, newest first.")
   (prompt-history-position :initform nil :accessor prompt-history-position)
   (prompt-dirty-p :initform t :accessor prompt-dirty-p)))

(defun prompt-history (frame)
  (gethash (prompt-label frame) (prompt-histories frame)))

(defun prompt-height (frame)
  (+ (* 2 +prompt-margin+) +prompt-field-height+
     (let ((rows (min +prompt-row-limit+ (length (prompt-candidates frame)))))
       (if (plusp rows) (+ 8 (* rows +prompt-row-height+)) 0))))

(defun prompt-placement (frame width height)
  "The prompt's LEFT, TOP, WIDTH, and HEIGHT in a WIDTH by HEIGHT shell."
  (declare (ignore height))
  (let ((pane-width (min +prompt-width+ (max 1 (- width 32)))))
    (values (max 0 (floor (- width pane-width) 2)) +prompt-top+
            pane-width (prompt-height frame))))

(defun update-prompt-candidates (frame)
  (let ((completer (prompt-completer frame)))
    (setf (prompt-candidates frame)
          (and completer
               (handler-case (funcall completer (prompt-text frame))
                 (error () nil)))
          (prompt-selected frame) nil
          (prompt-dirty-p frame) t)))

(defun open-prompt (frame &key (label "") (initial "") completer continuation)
  "Begin reading a line into FRAME's prompt.  CONTINUATION is called with the
submitted text after the prompt has closed."
  (setf (prompt-label frame) label
        (prompt-text frame) initial
        (prompt-completer frame) completer
        (prompt-continuation frame) continuation
        (prompt-history-position frame) nil)
  (update-prompt-candidates frame)
  frame)

(defun set-prompt-text (frame text)
  (setf (prompt-text frame) (subseq text 0 (min (length text) +prompt-text-limit+)))
  (update-prompt-candidates frame))

(defun prompt-common-prefix (strings)
  (if (null strings)
      ""
      (reduce (lambda (a b) (subseq a 0 (or (mismatch a b :test #'char-equal)
                                            (min (length a) (length b)))))
              strings)))

(defun complete-prompt (frame)
  "Tab: extend the text to the candidates' common prefix, or cycle them."
  (let* ((candidates (prompt-candidates frame))
         (prefix (prompt-common-prefix candidates)))
    (cond
      ((null candidates))
      ((null (rest candidates))
       (set-prompt-text frame (first candidates)))
      ((> (length prefix) (length (prompt-text frame)))
       (set-prompt-text frame prefix))
      (t
       (setf (prompt-selected frame)
             (mod (1+ (or (prompt-selected frame) -1)) (length candidates))
             (prompt-dirty-p frame) t)))))

(defun move-prompt-selection (frame delta)
  (let ((candidates (prompt-candidates frame)))
    (if candidates
        (setf (prompt-selected frame)
              (mod (+ (or (prompt-selected frame) (if (plusp delta) -1 0)) delta)
                   (length candidates))
              (prompt-dirty-p frame) t)
        ;; With nothing to choose from, the arrows walk this label's history.
        (let* ((history (prompt-history frame))
               (position (prompt-history-position frame))
               (next (if (minusp delta)
                         (min (1- (length history)) (if position (1+ position) 0))
                         (and position (plusp position) (1- position)))))
          (when history
            (setf (prompt-history-position frame) next)
            (set-prompt-text frame (if next (nth next history) "")))))))

(defun prompt-submission (frame)
  (let ((selected (prompt-selected frame)))
    (if selected
        (nth selected (prompt-candidates frame))
        (prompt-text frame))))

(defun remember-prompt-submission (frame text)
  (unless (alexandria:emptyp text)
    (push text (gethash (prompt-label frame) (prompt-histories frame)))))

(defun handle-prompt-key-event (frame event)
  "Edit FRAME's prompt for one key press.  Return :CONTINUE, :DISMISS, or
:SUBMIT and the text.  Held keys repeat, unlike M-x, so Backspace can be held."
  (check-type event luv:canvas-key-press-event)
  (let ((key (luv:canvas-key-event-key-name event))
        (character (luv:canvas-key-event-character event))
        (unshifted (luv:canvas-key-event-unshifted-character event))
        (modifiers (luv:canvas-key-event-modifiers event)))
    (cond
      ((or (eq key :escape)
           (and (member :control modifiers) (eql unshifted #\g)))
       (values :dismiss nil))
      ((member key '(:return :keypad-enter))
       (let ((text (prompt-submission frame)))
         (remember-prompt-submission frame text)
         (values :submit text)))
      ((eq key :tab) (complete-prompt frame) (values :continue nil))
      ((eq key :up) (move-prompt-selection frame -1) (values :continue nil))
      ((eq key :down) (move-prompt-selection frame 1) (values :continue nil))
      ((eq key :backspace)
       (let ((text (prompt-text frame)))
         (when (plusp (length text))
           (set-prompt-text frame (subseq text 0 (1- (length text))))))
       (values :continue nil))
      ((and (member :control modifiers) (eql unshifted #\u))
       (set-prompt-text frame "")
       (values :continue nil))
      ((and character (graphic-char-p character)
            (null (intersection '(:control :meta :super) modifiers)))
       (set-prompt-text frame (concatenate 'string (prompt-text frame)
                                           (string character)))
       (values :continue nil))
      (t (values :continue nil)))))

(defun draw-prompt-panel (pane left top right bottom ink)
  (draw-analytic-rounded-rectangle* pane (+ left 6) (+ top 8) (- right 1) (- bottom 1)
                                    :radius 14 :ink *command-menu-shadow-ink*)
  (draw-analytic-rounded-rectangle* pane left top right bottom
                                    :radius 14 :ink *prompt-edge-ink*)
  (draw-analytic-rounded-rectangle* pane (+ left 2) (+ top 2) (- right 2) (- bottom 2)
                                    :radius 12 :ink ink))

(defmethod handle-repaint ((pane prompt-pane) region)
  (declare (ignore region))
  (let ((frame (pane-frame pane)))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (ensure-command-menu-gpu-medium medium))
      (let* ((width (- right left))
             (height (- bottom top))
             (field-y (+ +prompt-margin+ (/ +prompt-field-height+ 2)))
             (label (prompt-label frame))
             (label-width (if (alexandria:emptyp label)
                              0
                              (+ 12 (text-size pane label :text-style *prompt-text-style*))))
             (text (prompt-text frame))
             (text-x (+ +prompt-margin+ 12 label-width))
             (text-width (text-size pane text :text-style *prompt-text-style*)))
        (draw-prompt-panel pane 0 0 width height *prompt-panel-ink*)
        (draw-text* pane label (+ +prompt-margin+ 12) field-y
                    :align-y :center :text-style *prompt-text-style*
                    :ink *prompt-label-ink*)
        (draw-text* pane text text-x field-y
                    :align-y :center :text-style *prompt-text-style*
                    :ink *prompt-text-ink*)
        (draw-rectangle* pane (+ text-x text-width 1) (- field-y 10)
                         (+ text-x text-width 3) (+ field-y 10)
                         :ink *prompt-label-ink*)
        (let* ((candidates (prompt-candidates frame))
               (selected (prompt-selected frame))
               (count (length candidates))
               (start (if selected
                          (min (max 0 (- selected (1- +prompt-row-limit+)))
                               (max 0 (- count +prompt-row-limit+)))
                          0)))
          (loop for candidate in (subseq candidates start
                                         (min count (+ start +prompt-row-limit+)))
                for index from start
                for y from (+ +prompt-margin+ +prompt-field-height+ 8)
                  by +prompt-row-height+
                for chosen = (eql index selected)
                do (draw-analytic-rounded-rectangle*
                    pane +prompt-margin+ (+ y 1) (- width +prompt-margin+)
                    (- (+ y +prompt-row-height+) 1)
                    :radius 6
                    :ink (if chosen *prompt-selected-row-ink* *prompt-row-ink*))
                   (draw-text* pane candidate (+ +prompt-margin+ 10)
                               (+ y (/ +prompt-row-height+ 2))
                               :align-y :center :text-style *prompt-text-style*
                               :ink (if chosen
                                        *prompt-selected-text-ink*
                                        *prompt-text-ink*))))))))

(defun prepare-prompt (frame)
  "Ensure FRAME's prompt has a complete retained revision for this frame."
  (let ((mirror (command-menu-mirror frame)))
    (if (or (prompt-dirty-p frame)
            (null (gpu-mirror-prepared-commands mirror)))
        (progn (repaint-gpu-mirror mirror)
               (setf (prompt-dirty-p frame) nil))
        (prepare-gpu-mirror-compositor mirror)))
  frame)

;;; ---------------------------------------------------------------------
;;; Messages.

(defclass message-state ()
  ((message-lines :initform nil :accessor message-lines)
   (message-error-p :initform nil :accessor message-error-p)
   (message-deadline :initform nil :accessor message-deadline
                     :documentation "Internal real time at which it hides, or NIL.")
   (message-width :initform +message-min-width+ :accessor message-width)
   (message-history :initform nil :accessor message-history)))

(defun message-height (frame)
  (+ (* 2 +message-margin+)
     (* +message-line-height+ (max 1 (length (message-lines frame))))))

(defun message-placement (frame width height)
  "Top right, below the status line, as StumpWM's message window."
  (declare (ignore height))
  (let ((pane-width (min (message-width frame) (max 1 (- width 32)))))
    (values (max 0 (- width pane-width 16)) +prompt-top+
            pane-width (message-height frame))))

(defun set-message (frame pane lines &key error-p (seconds 5))
  "Show LINES, a string or list of strings, in FRAME's message PANE.  With
SECONDS NIL it stays until replaced or cleared."
  (let* ((lines (if (stringp lines)
                    (uiop:split-string lines :separator '(#\Newline))
                    lines))
         (lines (subseq lines 0 (min (length lines) +message-line-limit+))))
    (setf (message-lines frame) lines
          (message-error-p frame) error-p
          (message-deadline frame)
          (and seconds (+ (get-internal-real-time)
                          (round (* seconds internal-time-units-per-second))))
          (message-width frame)
          (max +message-min-width+
               (min +message-max-width+
                    (+ (* 2 +message-margin+) 16
                       (reduce #'max lines
                               :key (lambda (line)
                                      (text-size pane line
                                                 :text-style *message-text-style*))
                               :initial-value 0)))))
    (push lines (message-history frame))
    (when (> (length (message-history frame)) +message-history-limit+)
      (setf (message-history frame)
            (subseq (message-history frame) 0 +message-history-limit+)))
    lines))

(defun message-expired-p (frame)
  (let ((deadline (message-deadline frame)))
    (and deadline (>= (get-internal-real-time) deadline))))

(defmethod handle-repaint ((pane message-pane) region)
  (declare (ignore region))
  (let ((frame (pane-frame pane)))
    (when (message-lines frame)
      (with-sheet-medium (medium pane)
        (ensure-command-menu-gpu-medium medium))
      (with-bounding-rectangle* (left top right bottom) pane
        (draw-prompt-panel pane 0 0 (- right left) (- bottom top) *message-panel-ink*)
        (loop for line in (message-lines frame)
              for y from (+ +message-margin+ (/ +message-line-height+ 2))
                by +message-line-height+
              do (draw-text* pane line (+ +message-margin+ 8) y
                             :align-y :center :text-style *message-text-style*
                             :ink (if (message-error-p frame)
                                      *message-error-ink*
                                      *prompt-text-ink*)))))))
