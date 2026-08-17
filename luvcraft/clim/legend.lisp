(in-package #:luvcraft.clim)

;;; The keymap legend: what the keys do, read off the tables that decide it.
;;;
;;; Nothing here is a written-down list of bindings.  The rows are gathered by
;;; walking the same command tables dispatch walks, so rebinding a key or
;;; adding a command changes what the game says about itself, and a legend can
;;; never quietly drift out of date with the thing it describes.
;;;
;;; Escape slides it up.  Escape again puts it away -- and since taking focus
;;; hands the pointer back, escape also does what escape has always done here,
;;; which is get the player their cursor.

;;; ---------------------------------------------------------------------
;;; Reading a table.

(defun keystroke-item-command (item gesture)
  "Return the command a keystroke ITEM stands for, or NIL.

A :FUNCTION item builds its command from the gesture that reached it -- which
is how one entry serves nine quickbar slots or four walking directions -- so
asking what it means is asking it."
  (case (command-menu-item-type item)
    (:command (command-menu-item-value item))
    (:function (ignore-errors
                (funcall (command-menu-item-value item) gesture 1)))
    (t nil)))

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
                      (case modifier
                        (:shift "⇧") (:control "^") (:meta "⌥") (:super "⌘")
                        (t (format nil "~A-" modifier))))
                    modifiers)
            (format-gesture-key key))))

(defgeneric luvcraft-command-legend-label (name arguments table)
  (:documentation
   "What a legend should call the command NAME applied to ARGUMENTS.

Rows are merged by this label, so it decides how coarse the legend is: one
line per argument where the argument is the point, one line for the whole
family where it is not.  Walking forward and sprinting are different lines
because they are different things to do; the nine quickbar slots are one line
because nobody needs to be told about each of them separately.")
  (:method (name arguments table)
    (declare (ignore arguments))
    (string-downcase
     (or (command-line-name-for-command name table :errorp nil)
         (substitute #\Space #\- (symbol-name name))))))

(defmethod luvcraft-command-legend-label
    ((name (eql 'com-start-walking)) arguments table)
  (declare (ignore table))
  (if (eq :sprint (first arguments))
      "sprint"
      (format nil "walk ~(~A~)" (first arguments))))

(defmethod luvcraft-command-legend-label
    ((name (eql 'com-start-looking)) arguments table)
  (declare (ignore arguments table))
  "look")

(defmethod luvcraft-command-legend-label
    ((name (eql 'com-select-quickbar-slot)) arguments table)
  (declare (ignore arguments table))
  "select block")

(defun command-table-legend-rows (table)
  "Return TABLE's keystrokes as (LABEL . KEYS) rows, one row per label.

Keys are merged rather than listed once each, because a cheatsheet wants to
say that the quickbar is 1-9 rather than saying `select block' nine times."
  (let ((rows nil))
    (map-over-command-table-keystrokes
     (lambda (menu-name gesture item)
       (declare (ignore menu-name))
       (alexandria:when-let*
           ((command (keystroke-item-command item gesture))
            (name (command-name command))
            (label (luvcraft-command-legend-label
                    name (command-arguments command) table)))
         (let ((row (assoc label rows :test #'string=)))
           (if row
               (pushnew (format-gesture gesture) (cdr row) :test #'string=)
               (push (cons label (list (format-gesture gesture))) rows)))))
     table :inherited nil)
    (mapcar (lambda (row) (cons (car row) (reverse (cdr row))))
            (nreverse rows))))

(defparameter *legend-sections*
  '(("Moving" luvcraft-movement)
    ("In the world" luvcraft-world)
    ("Any time" luvcraft-window))
  "Which tables the legend shows, in the order a player meets them.

Each is read without inheritance, so a command appears under the layer that
actually owns it rather than once per table that inherits it.")

(defun luvcraft-legend-sections ()
  "Return (TITLE . ROWS) for every section that has anything to say."
  (loop for (title table) in *legend-sections*
        for rows = (command-table-legend-rows table)
        when rows collect (cons title rows)))

;;; ---------------------------------------------------------------------
;;; The panel.

(defparameter *legend-width* 620)
(defparameter *legend-margin* 26)
(defparameter *legend-row-height* 30)
(defparameter *legend-section-gap* 22)
(defparameter *legend-header-height* 38)
(defparameter *legend-title-height* 52)
(defparameter *legend-keys-right* 300
  "Where the key column ends; labels begin a gutter to its right.

Wide enough for the longest run of keys one command answers to, which is the
whole number row.")

(defparameter *legend-panel-ink* (make-rgb-color 0.11 0.11 0.105))
(defparameter *legend-row-ink* (make-rgb-color 0.155 0.155 0.147))
(defparameter *legend-key-ink* (make-rgb-color 0.58 0.78 0.54))
(defparameter *legend-text-ink* (make-rgb-color 0.91 0.89 0.82))
(defparameter *legend-muted-ink* (make-rgb-color 0.60 0.60 0.55))
(defparameter *legend-edge-ink* (make-rgb-color 0.42 0.42 0.38))

(defun legend-height (&optional (sections (luvcraft-legend-sections)))
  (+ *legend-title-height*
     (loop for (nil . rows) in sections
           sum (+ *legend-header-height*
                  (* *legend-row-height* (length rows))
                  *legend-section-gap*))
     *legend-margin*))

(defclass legend-pane (application-pane) ())

(define-application-frame luvcraft-legend ()
  ((session :initarg :session :reader legend-session)
   ;; Gathered when the frame is made, so the very first paint already has
   ;; them: a pane realized by ENABLE-FRAME draws before anyone can fill a slot
   ;; in afterwards.
   (sections :initform (luvcraft-legend-sections) :accessor legend-sections))
  (:menu-bar nil)
  (:panes (sheet (make-pane 'legend-pane)))
  (:layouts
   (default
    (horizontally (:width *legend-width* :height (legend-height))
      sheet))))

(defun draw-legend-row (pane row top)
  (destructuring-bind (label . keys) row
    (draw-rectangle* pane *legend-margin* (+ top 1)
                     (- *legend-width* *legend-margin*)
                     (+ top *legend-row-height* -1)
                     :ink *legend-row-ink*)
    (draw-text* pane (format nil "~{~A~^  ~}" keys)
                *legend-keys-right* (+ top (/ *legend-row-height* 2))
                :align-x :right :align-y :center
                :text-size 16 :text-face :bold :ink *legend-key-ink*)
    (draw-text* pane label
                (+ *legend-keys-right* 24) (+ top (/ *legend-row-height* 2))
                :align-y :center :text-size 16 :ink *legend-text-ink*)))

(defmethod handle-repaint ((pane legend-pane) region)
  (declare (ignore region))
  (let ((sections (legend-sections (pane-frame pane))))
    (with-bounding-rectangle* (left top right bottom) pane
      (with-sheet-medium (medium pane)
        (when (typep medium 'mcluv:luv-raster-medium)
          (mcluv::clear-raster-medium-reliefs medium))
        (draw-rectangle* pane left top right bottom :ink *legend-panel-ink*)
        (draw-rectangle* pane left top right bottom :filled nil
                         :line-thickness 2 :ink *legend-edge-ink*)
        (draw-text* pane "keys" *legend-margin* 34
                    :align-y :center :text-size 22 :text-face :bold
                    :ink *legend-text-ink*)
        (draw-text* pane "Esc closes" (- *legend-width* *legend-margin*) 34
                    :align-x :right :align-y :center :text-size 14
                    :ink *legend-muted-ink*)
        (let ((y *legend-title-height*))
          (dolist (section sections)
            (draw-text* pane (car section) *legend-margin* (+ y 14)
                        :align-y :center :text-size 14 :text-face :bold
                        :ink *legend-muted-ink*)
            (incf y *legend-header-height*)
            (dolist (row (cdr section))
              (draw-legend-row pane row y)
              (incf y *legend-row-height*))
            (incf y *legend-section-gap*)))))))

(defun repaint-legend (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'mcluv:luv-gpu-mirror)
        (mcluv:repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mcluv:mirror-sheet mirror) +everywhere+)
          (mcluv:present-mirror mirror))))
  frame)

;;; ---------------------------------------------------------------------
;;; The overlay: a panel in the middle of the screen.

(defclass luvcraft-legend-overlay (mcluv:luvcraft-widget-overlay) ())

(defmethod luvcraft:luvcraft-overlay-stage ((overlay luvcraft-legend-overlay))
  (declare (ignore overlay))
  :hud)

(defun legend-screen-state (overlay)
  "Place the panel at its own size in the middle of the viewport."
  (let* ((source-size
           (luv:gpu-texture-size
            (mcluv:mirror-texture (mcluv:widget-overlay-mirror overlay))))
         (viewport-size
           (luv:canvas-extent
            (luvcraft:luvcraft-session-context
             (mcluv:widget-overlay-session overlay))))
         (source-width (first source-size))
         (source-height (second source-size))
         (viewport-width (first viewport-size))
         (viewport-height (second viewport-size))
         (scale (min 1.0
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

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-legend-overlay) session pass surface-texture)
  (declare (ignore session))
  (let* ((mirror (mcluv:widget-overlay-mirror overlay))
         (source (mcluv:mirror-texture mirror)))
    (when source
      (mcluv:ensure-spinning-compositor-resources
       overlay (mcluv:mirror-context mirror) source
       :target-format (luv:gpu-texture-format surface-texture))
      (let* ((state (legend-screen-state overlay))
             (frame-state
               (mcluv:ensure-spinning-compositor-frame-state
                overlay surface-texture)))
        (setf (mcluv:widget-overlay-render-state overlay) state)
        (luv:write-buffer (mcluv:spinning-frame-state-buffer frame-state) state)
        (luv:set-pipeline pass (mcluv:spinning-compositor-pipeline overlay))
        (luv:set-bind-group
         pass 0 (mcluv:spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

;;; A legend is a tool rather than a thing in the world: the camera stays put,
;;; and the crosshair never targets it.

(defmethod luvcraft:luvcraft-focus-score
    ((overlay luvcraft-legend-overlay) session)
  (declare (ignore overlay session))
  nil)

(defmethod luvcraft:luvcraft-focus-camera-pose
    ((overlay luvcraft-legend-overlay) session)
  "Stay exactly where the player was standing.

A legend is a tool, not a thing in the world: nothing is framed, nothing is
approached, and the view a player looks back at afterwards is the one they
were already looking at."
  (declare (ignore session))
  (let ((camera (luvcraft:luvcraft-session-camera
                 (mcluv:widget-overlay-session overlay))))
    (luvcraft::make-camera-pose
     (luvcraft::copy-camera-position (luvcraft:camera-position camera))
     (luvcraft:camera-yaw camera) (luvcraft:camera-pitch camera)
     luvcraft::+luvcraft-camera-vertical-field-of-view+)))

(defmethod luvcraft:luvcraft-focus-entered
    ((overlay luvcraft-legend-overlay) session)
  ;; Focusing released mouse look, and closing this panel should not silently
  ;; take it back: escape has always been how a player gets their cursor, and
  ;; a legend is what you open when you have lost track of the controls.
  (setf (luvcraft:luvcraft-session-pointer-capture-suspended-p session) nil)
  overlay)

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-legend-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (when (member (luv:canvas-key-event-key-name event) '(:escape :return))
    (close-luvcraft-legend overlay session))
  t)

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-legend-overlay) session canvas (event luv:canvas-event))
  (declare (ignore overlay session canvas event))
  t)

;;; ---------------------------------------------------------------------
;;; Opening and closing.

(defun find-luvcraft-legend (session)
  (find-if (lambda (overlay) (typep overlay 'luvcraft-legend-overlay))
           (luvcraft:luvcraft-session-overlays session)))

(defun open-luvcraft-legend (session &key (title "luvcraft keys"))
  "Create, attach, and focus SESSION's keymap legend."
  (let* ((port (find-port :server-path '(:luv)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'mcluv:luv-frame-manager :port port)))
         (sections (luvcraft-legend-sections))
         (frame
           (let ((mcluv:*embedded-mirror-target*
                   (luvcraft:luvcraft-session-canvas session))
                 (mcluv:*embedded-mirror-context*
                   (luvcraft:luvcraft-session-context session))
                 (mcluv:*embedded-mirror-device*
                   (luvcraft:luvcraft-session-device session)))
             (make-application-frame
              'luvcraft-legend :frame-manager manager :enable t
              :session session))))
    (setf (frame-pretty-name frame) title
          (legend-sections frame) sections)
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance 'luvcraft-legend-overlay
                            :session session :frame frame :mirror mirror)))
      (setf (mcluv:mirror-compositor mirror) overlay)
      (luvcraft:add-luvcraft-overlay session overlay)
      (repaint-legend frame)
      (luvcraft:focus-luvcraft-session session overlay)
      overlay)))

(defun close-luvcraft-legend (overlay session)
  "Put the legend away."
  (when (eq overlay (luvcraft:luvcraft-session-modal-focus session))
    (luvcraft:unfocus-luvcraft-session session))
  (luvcraft:remove-luvcraft-overlay session overlay)
  nil)

(defun toggle-luvcraft-legend (session)
  "Show SESSION's keymap legend, or put it away if it is already up."
  (alexandria:if-let ((overlay (find-luvcraft-legend session)))
    (close-luvcraft-legend overlay session)
    (open-luvcraft-legend session))
  t)

;;; Escape shows the keys.  It keeps its old meaning too: focusing the legend
;;; releases mouse look, and the legend refuses to take it back on the way out.

(define-command (com-show-keymap :command-table luvcraft-world
                                 :name "Show Keys"
                                 :keystroke (:escape))
    ()
  (toggle-luvcraft-legend (luvcraft-command-session)))
