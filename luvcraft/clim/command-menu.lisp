(in-package #:luvcraft.clim)

;;; M-x: the game's command vocabulary as an executable, searchable menu.
;;;
;;; The keymap legend and this menu read the same CLIM command tables for two
;;; different questions.  Escape asks "what do my keys do?"; M-x asks "what
;;; can I do?".  A command remains one definition whether it is reached by a
;;; key, this finder, or some later physical tool or register.

(defparameter *command-menu-tables*
  '(luvcraft-movement luvcraft-world luvcraft-terminal luvcraft-window)
  "The semantic input layers whose directly owned commands M-x offers.")

(defun command-menu-command-without-arguments-p (name)
  "Whether NAME is a CLIM command M-x can execute without prompting yet."
  (alexandria:when-let
      ((parsers (gethash name climi::*command-parser-table*)))
    (null (climi::required-args parsers))))

(defun luvcraft-command-menu-entries ()
  "Return the currently defined argument-free commands as (LABEL . NAME).

Parameterized commands will belong here once the menu can ask for their
arguments.  Until then they are omitted rather than displayed as dead ends."
  (let ((entries nil))
    (dolist (table *command-menu-tables*)
      (map-over-command-table-commands
       (lambda (name)
         (alexandria:when-let
             ((label (command-line-name-for-command name table :errorp nil)))
           (when (command-menu-command-without-arguments-p name)
             (pushnew (cons label name) entries :key #'cdr :test #'eq))))
       table :inherited nil))
    (sort entries #'string-lessp :key #'car)))

(defun matching-command-menu-entries (entries query)
  "Return ENTRIES whose labels contain every whitespace-separated query word."
  (let ((words (remove-if #'alexandria:emptyp
                          (uiop:split-string query
                                             :separator '(#\Space #\Tab)))))
    (if (null words)
        entries
        (remove-if-not
         (lambda (entry)
           (every (lambda (word) (search word (car entry) :test #'char-equal))
                  words))
         entries))))

(defparameter *command-menu-width* 620)
(defparameter *command-menu-height* 420)
(defparameter *command-menu-margin* 26)
(defparameter *command-menu-row-height* 34)
(defparameter *command-menu-result-limit* 8)
(defparameter *command-menu-field-top* 54)
(defparameter *command-menu-field-bottom* 98)
(defparameter *command-menu-results-top* 116)

(defclass command-menu-pane (application-pane) ())

(define-application-frame luvcraft-command-menu ()
  ((session :initarg :session :reader command-menu-session)
   (entries :initform (luvcraft-command-menu-entries)
            :accessor command-menu-entries)
   (query :initform "" :accessor command-menu-query)
   (selected :initform 0 :accessor command-menu-selected))
  (:menu-bar nil)
  (:panes (sheet (make-pane 'command-menu-pane)))
  (:layouts
   (default
    (horizontally (:width *command-menu-width* :height *command-menu-height*)
      sheet))))

(defun command-menu-results (frame)
  (matching-command-menu-entries (command-menu-entries frame)
                                 (command-menu-query frame)))

(defun command-menu-visible-results (frame)
  "Return the visible result window and its zero-based start index."
  (let* ((results (command-menu-results frame))
         (count (length results))
         (selected (if (plusp count)
                       (mod (command-menu-selected frame) count)
                       0))
         (start (min (max 0 (- selected (1- *command-menu-result-limit*)))
                     (max 0 (- count *command-menu-result-limit*)))))
    (values (subseq results start
                    (min count (+ start *command-menu-result-limit*)))
            start)))

(defun draw-command-menu-row (pane entry top selected-p)
  (let ((left *command-menu-margin*)
        (right (- *command-menu-width* *command-menu-margin*)))
    (draw-rectangle* pane left (+ top 1) right
                     (+ top *command-menu-row-height* -1)
                     :ink (if selected-p
                              *legend-key-ink*
                              *legend-row-ink*))
    (draw-text* pane (car entry) (+ left 12)
                (+ top (/ *command-menu-row-height* 2))
                :align-y :center :text-size 17
                :ink (if selected-p
                         *legend-panel-ink*
                         *legend-text-ink*))))

(defmethod handle-repaint ((pane command-menu-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (query (command-menu-query frame))
         (results (command-menu-results frame))
         (selected (command-menu-selected frame))
         (margin *command-menu-margin*)
         (right (- *command-menu-width* margin)))
    (with-bounding-rectangle* (left top right-edge bottom) pane
      (with-sheet-medium (medium pane)
        (when (typep medium 'mcluv:luv-raster-medium)
          (mcluv::clear-raster-medium-reliefs medium))
        (draw-rectangle* pane left top right-edge bottom :ink *legend-panel-ink*)
        (draw-rectangle* pane left top right-edge bottom :filled nil
                         :line-thickness 2 :ink *legend-edge-ink*)
        (draw-text* pane "M-x" margin 30
                    :align-y :center :text-size 22 :text-face :bold
                    :ink *legend-text-ink*)
        (draw-text* pane "Esc cancels" right 30
                    :align-x :right :align-y :center :text-size 14
                    :ink *legend-muted-ink*)
        (draw-rectangle* pane margin *command-menu-field-top*
                         right *command-menu-field-bottom*
                         :ink (make-rgb-color 0.06 0.06 0.058))
        (draw-rectangle* pane margin *command-menu-field-top*
                         right *command-menu-field-bottom*
                         :filled nil :line-thickness 1 :ink *legend-key-ink*)
        (let* ((prompt "M-x ")
               (text (concatenate 'string prompt query))
               (text-x (+ margin 12))
               (text-y (/ (+ *command-menu-field-top*
                              *command-menu-field-bottom*) 2))
               (width (text-size pane text
                                 :text-style (make-text-style nil nil 18))))
          (draw-text* pane text text-x text-y :align-y :center :text-size 18
                      :ink *legend-text-ink*)
          (draw-rectangle* pane (+ text-x width 2) (- text-y 11)
                           (+ text-x width 4) (+ text-y 11)
                           :ink *legend-key-ink*))
        (multiple-value-bind (visible start)
            (command-menu-visible-results frame)
          (loop for entry in visible
                for index from start
                for y from *command-menu-results-top*
                      by *command-menu-row-height*
                do (draw-command-menu-row pane entry y (= index selected))))
        (when (null results)
          (draw-text* pane "No matching command" margin
                      (+ *command-menu-results-top* 20)
                      :align-y :center :text-size 16 :ink *legend-muted-ink*))
        (draw-text* pane
                    (format nil "~D command~:P  ·  ↑↓ choose  ·  RET runs"
                            (length results))
                    margin (- *command-menu-height* 24)
                    :align-y :center :text-size 14 :ink *legend-muted-ink*)))))

(defun repaint-command-menu (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'mcluv:luv-gpu-mirror)
        (mcluv:repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mcluv:mirror-sheet mirror) +everywhere+)
          (mcluv:present-mirror mirror))))
  frame)

(defclass luvcraft-command-menu-overlay (mcluv:luvcraft-hud-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage
    ((overlay luvcraft-command-menu-overlay))
  (declare (ignore overlay))
  :hud)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-command-menu-overlay) session pass surface-texture)
  (declare (ignore pass))
  (mcluv:prepare-direct-widget-overlay
   overlay session surface-texture (legend-screen-state overlay))
  overlay)

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-command-menu-overlay) session)
  (declare (ignore overlay session))
  nil)

(defmethod luvcraft:luvcraft-focus-camera-pose
    ((overlay luvcraft-command-menu-overlay) session)
  (declare (ignore session))
  (let ((camera (luvcraft:luvcraft-session-camera
                 (mcluv:widget-overlay-session overlay))))
    (luvcraft::make-camera-pose
     (luvcraft::copy-camera-position (luvcraft:camera-position camera))
     (luvcraft:camera-yaw camera) (luvcraft:camera-pitch camera)
     luvcraft::+luvcraft-camera-vertical-field-of-view+)))

(defmethod luvcraft:luvcraft-focus-entered
    ((overlay luvcraft-command-menu-overlay) session)
  (setf (luvcraft:luvcraft-session-pointer-capture-suspended-p session) nil)
  overlay)

(defun find-luvcraft-command-menu (session)
  (find-if (lambda (overlay)
             (typep overlay 'luvcraft-command-menu-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun close-luvcraft-command-menu (overlay session)
  "Cancel and remove the M-x menu."
  (when (eq overlay (luvcraft:luvcraft-session-modal-focus session))
    (luvcraft:unfocus-luvcraft-session session))
  (luvcraft:remove-luvcraft-overlay session overlay)
  nil)

(defun run-command-menu-selection (overlay session)
  (let* ((frame (mcluv:widget-overlay-frame overlay))
         (results (command-menu-results frame))
         (entry (and results
                     (nth (mod (command-menu-selected frame)
                               (length results))
                          results))))
    (when entry
      ;; The menu is gone before the command runs, so a command which opens a
      ;; modal tool can take focus normally.
      (close-luvcraft-command-menu overlay session)
      (execute-frame-command (luvcraft-session-frame session)
                             (list (cdr entry)))
      t)))

(defun reset-command-menu-selection (frame)
  (setf (command-menu-selected frame) 0)
  (repaint-command-menu frame))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-command-menu-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (let* ((frame (mcluv:widget-overlay-frame overlay))
         (key (luv:canvas-key-event-key-name event))
         (character (luv:canvas-key-event-character event)))
    (case key
      (:escape (close-luvcraft-command-menu overlay session))
      ((:return :keypad-enter) (run-command-menu-selection overlay session))
      (:up
       (let ((count (length (command-menu-results frame))))
         (when (plusp count)
           (setf (command-menu-selected frame)
                 (mod (1- (command-menu-selected frame)) count))
           (repaint-command-menu frame))))
      (:down
       (let ((count (length (command-menu-results frame))))
         (when (plusp count)
           (setf (command-menu-selected frame)
                 (mod (1+ (command-menu-selected frame)) count))
           (repaint-command-menu frame))))
      (:backspace
       (let ((query (command-menu-query frame)))
         (when (plusp (length query))
           (setf (command-menu-query frame) (subseq query 0 (1- (length query))))
           (reset-command-menu-selection frame))))
      (t
       (when (and character (graphic-char-p character)
                  (null (intersection '(:control :meta :super)
                                      (luv:canvas-key-event-modifiers event)))
                  (< (length (command-menu-query frame)) 80))
         (setf (command-menu-query frame)
               (concatenate 'string (command-menu-query frame)
                            (string character)))
         (reset-command-menu-selection frame))))
    t))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-command-menu-overlay) session canvas
     (event luv:canvas-event))
  (declare (ignore overlay session canvas event))
  t)

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-command-menu-overlay) session canvas
     (event luv:canvas-pointer-button-press-event))
  (declare (ignore canvas))
  (alexandria:when-let
      ((uv (mcluv:luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (when (eq :left (luv:canvas-pointer-event-button event))
      (let* ((frame (mcluv:widget-overlay-frame overlay))
             (y (* (second uv) *command-menu-height*))
             (row (floor (- y *command-menu-results-top*)
                         *command-menu-row-height*)))
        (multiple-value-bind (visible start)
            (command-menu-visible-results frame)
          (when (and (<= 0 row) (< row (length visible)))
            (setf (command-menu-selected frame) (+ start row))
            (repaint-command-menu frame)
            (run-command-menu-selection overlay session)))))
    t))

(defun open-luvcraft-command-menu (session &key (title "luvcraft M-x"))
  "Create, attach, and focus SESSION's searchable command menu."
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
              'luvcraft-command-menu :frame-manager manager :enable t
              :session session))))
    (setf (frame-pretty-name frame) title)
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance 'luvcraft-command-menu-overlay
                            :session session :frame frame :mirror mirror)))
      (setf (mcluv:mirror-compositor mirror) overlay)
      (luvcraft:add-luvcraft-overlay session overlay)
      (repaint-command-menu frame)
      (luvcraft:focus-luvcraft-session session overlay)
      overlay)))

(defun toggle-luvcraft-command-menu (session)
  (alexandria:if-let ((overlay (find-luvcraft-command-menu session)))
    (close-luvcraft-command-menu overlay session)
    (open-luvcraft-command-menu session))
  t)

(define-command (com-execute-command :command-table luvcraft-world
                                     :name "Execute Command"
                                     :keystroke (#\x :meta))
    ()
  (toggle-luvcraft-command-menu (luvcraft-command-session)))
