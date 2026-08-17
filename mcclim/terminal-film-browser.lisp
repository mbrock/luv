;;; A McCLIM file browser presented directly on a focused terminal wall.

(in-package #:mcluv)

(defparameter *terminal-film-extensions*
  '("mp4" "m4v" "mov" "mkv" "webm" "avi" "mpg" "mpeg" "ts")
  "File suffixes offered as playable films by the wall browser.")

(defconstant +terminal-film-browser-width+ 720)
(defconstant +terminal-film-browser-height+ 440)
(defconstant +terminal-film-browser-header-height+ 54)
(defconstant +terminal-film-browser-footer-height+ 38)
(defconstant +terminal-film-browser-row-height+ 29)
(defconstant +terminal-film-browser-page-size+ 12)
(defconstant +terminal-film-browser-path-limit+ 42)

(defstruct terminal-film-entry
  pathname
  kind
  label)

(defun terminal-film-pathname-p (pathname)
  (member (string-downcase (or (pathname-type pathname) ""))
          *terminal-film-extensions* :test #'string=))

(defun terminal-film-directory-name (pathname)
  (let* ((parts (pathname-directory pathname))
         (name (car (last parts))))
    (if (stringp name) name (namestring pathname))))

(defun terminal-film-browser-entries (directory)
  "Return parent, directory, and playable-film entries inside DIRECTORY."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (parent (uiop:pathname-parent-directory-pathname directory))
         (directories
           (sort (copy-list (uiop:subdirectories directory))
                 #'string-lessp :key #'namestring))
         (films
           (sort (remove-if-not #'terminal-film-pathname-p
                                (uiop:directory-files directory))
                 #'string-lessp :key #'file-namestring)))
    (append
     (unless (equal directory parent)
       (list (make-terminal-film-entry
              :pathname parent :kind :directory :label "[..] parent")))
     (mapcar
      (lambda (pathname)
        (make-terminal-film-entry
         :pathname pathname :kind :directory
         :label (format nil "[DIR]  ~A/"
                        (terminal-film-directory-name pathname))))
      directories)
     (mapcar
      (lambda (pathname)
        (make-terminal-film-entry
         :pathname pathname :kind :film
         :label (format nil "[FILM] ~A" (file-namestring pathname))))
      films))))

(defclass terminal-film-browser-pane (application-pane) ())

(defun terminal-film-browser-path-label (pathname)
  (let ((label (namestring pathname)))
    (if (<= (length label) +terminal-film-browser-path-limit+)
        label
        (concatenate 'string
                     "..."
                     (subseq label
                             (- (length label)
                                (- +terminal-film-browser-path-limit+ 3)))))))

(define-application-frame terminal-film-browser ()
  ((display :initarg :display :reader terminal-film-browser-display)
   (directory :initarg :directory :accessor terminal-film-browser-directory)
   (entries :initform nil :accessor terminal-film-browser-frame-entries)
   (visible-entries :initform nil
                    :accessor terminal-film-browser-visible-entries)
   (offset :initform 0 :accessor terminal-film-browser-offset)
   (message :initform "Choose a directory or film."
            :accessor terminal-film-browser-message))
  (:menu-bar nil)
  (:panes
   (browser (make-pane 'terminal-film-browser-pane)))
  (:layouts
   (default
    (horizontally (:width +terminal-film-browser-width+
                   :height +terminal-film-browser-height+)
      browser))))

(defun terminal-film-browser-page (frame)
  (let* ((entries (terminal-film-browser-frame-entries frame))
         (offset (terminal-film-browser-offset frame))
         (previous-p (plusp offset))
         (available (- +terminal-film-browser-page-size+
                       (if previous-p 1 0)))
         (more-p (> (length entries) (+ offset available)))
         (content-count (- available (if more-p 1 0))))
    (append
     (when previous-p
       (list (make-terminal-film-entry :kind :previous
                                       :label "[PAGE] previous")))
     (subseq entries offset (min (length entries) (+ offset content-count)))
     (when more-p
       (list (make-terminal-film-entry :kind :next :label "[PAGE] next"))))))

(defun terminal-film-entry-color (entry)
  (ecase (terminal-film-entry-kind entry)
    (:directory (make-rgb-color 0.17 0.31 0.42))
    (:film (make-rgb-color 0.36 0.20 0.45))
    ((:previous :next) (make-rgb-color 0.20 0.24 0.27))))

(defmethod handle-repaint ((pane terminal-film-browser-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (entries (terminal-film-browser-visible-entries frame)))
    (with-bounding-rectangle* (left top right bottom) pane
      (draw-rectangle* pane left top right bottom
                       :ink (make-rgb-color 0.018 0.024 0.032))
      (draw-rectangle* pane left top right (+ top 46)
                       :ink (make-linear-gradient
                             0 top 0 (+ top 46)
                             (make-rgb-color 0.16 0.20 0.24)
                             (make-rgb-color 0.07 0.09 0.12)))
      (draw-text* pane "FILM BROWSER" (+ left 16) (+ top 17)
                  :align-y :center :text-size 14 :ink +white+)
      (draw-text* pane
                  (terminal-film-browser-path-label
                   (terminal-film-browser-directory frame))
                  (+ left 180) (+ top 17) :align-y :center :text-size 12
                  :ink (make-rgb-color 0.82 0.86 0.88))
      (loop for entry in entries
            for index from 0
            for row-top = (+ top +terminal-film-browser-header-height+
                             (* index +terminal-film-browser-row-height+))
            for row-bottom = (+ row-top (- +terminal-film-browser-row-height+ 3))
            do (draw-analytic-rounded-rectangle*
                (sheet-medium pane)
                (+ left 10) row-top (- right 10) row-bottom
                :radius 5 :ink (terminal-film-entry-color entry))
               (draw-text* pane (terminal-film-entry-label entry)
                           (+ left 22) (/ (+ row-top row-bottom) 2)
                           :align-y :center :text-size 13 :ink +white+))
      (draw-text* pane (terminal-film-browser-message frame)
                  (+ left 14) (- bottom 15)
                  :align-y :center :text-size 11
                  :ink (make-rgb-color 0.76 0.80 0.82)))))

(defun repaint-terminal-film-browser (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mirror-sheet mirror) +everywhere+)
          (present-mirror mirror))))
  frame)

(defun refresh-terminal-film-browser (frame &key (reset-offset-p nil))
  "Refresh FRAME's bounded directory snapshot without opening any files."
  (when reset-offset-p
    (setf (terminal-film-browser-offset frame) 0))
  (handler-case
      (setf (terminal-film-browser-frame-entries frame)
            (terminal-film-browser-entries
             (terminal-film-browser-directory frame))
            (terminal-film-browser-message frame)
            "Choose a directory or film.")
    (error (condition)
      (setf (terminal-film-browser-frame-entries frame) nil
            (terminal-film-browser-message frame)
            (format nil "Cannot read this directory: ~A" condition))))
  (setf (terminal-film-browser-visible-entries frame)
        (terminal-film-browser-page frame))
  (repaint-terminal-film-browser frame))

(defun choose-terminal-film-browser-entry (frame entry)
  "Enter ENTRY's directory, change page, or play its film on the wall."
  (when entry
    (ecase (terminal-film-entry-kind entry)
      (:previous
       (setf (terminal-film-browser-offset frame)
             (max 0 (- (terminal-film-browser-offset frame)
                       (1- +terminal-film-browser-page-size+))))
       (refresh-terminal-film-browser frame))
      (:next
       (incf (terminal-film-browser-offset frame)
             (1- +terminal-film-browser-page-size+))
       (refresh-terminal-film-browser frame))
      (:directory
       (setf (terminal-film-browser-directory frame)
             (uiop:ensure-directory-pathname
              (terminal-film-entry-pathname entry)))
       (refresh-terminal-film-browser frame :reset-offset-p t))
      (:film
       (handler-case
           (progn
             (luvcraft:play-terminal-display-film
              (terminal-film-browser-display frame)
              (terminal-film-entry-pathname entry))
             (setf (terminal-film-browser-message frame)
                   (format nil "Playing ~A"
                           (file-namestring
                            (terminal-film-entry-pathname entry)))))
         (error (condition)
           (setf (terminal-film-browser-message frame)
                 (format nil "Could not play film: ~A" condition))
           (repaint-terminal-film-browser frame)))))))

(defun terminal-film-browser-entry-at (frame texture-y)
  (let ((index (floor (- texture-y +terminal-film-browser-header-height+)
                      +terminal-film-browser-row-height+)))
    (when (<= 0 index)
      (nth index (terminal-film-browser-visible-entries frame)))))

(defclass terminal-film-browser-overlay (luvcraft-widget-overlay)
  ((display :initarg :display :reader terminal-film-browser-overlay-display)))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay terminal-film-browser-overlay) session pass surface-texture)
  "Draw the browser as one flat wall texture, without widget-lab chassis relief."
  (let* ((mirror (widget-overlay-mirror overlay))
         (source (mirror-texture mirror)))
    (when source
      (ensure-spinning-compositor-resources
       overlay (mirror-context mirror) source
       :depth-format :depth32-float
       :target-format
       (luv:gpu-texture-format
        (luvcraft::luvcraft-session-color-texture session)))
      (let* ((viewport-size
               (luv:canvas-extent (luvcraft::luvcraft-session-context session)))
             (state
               (world-device-clip-state
                overlay session (first viewport-size) (second viewport-size)))
             (frame-state
               (ensure-spinning-compositor-frame-state overlay surface-texture)))
        (setf (widget-overlay-render-state overlay) state)
        (luv:write-buffer (spinning-frame-state-buffer frame-state) state)
        (luv:set-pipeline pass (spinning-compositor-pipeline overlay))
        (luv:set-bind-group pass 0
                            (spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay terminal-film-browser-overlay) session canvas
     (event luv:canvas-pointer-event))
  (declare (ignore session canvas))
  (alexandria:when-let
      ((uv (luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (when (and (typep event 'luv:canvas-pointer-button-press-event)
               (eq :left (luv:canvas-pointer-event-button event)))
      (let* ((frame (widget-overlay-frame overlay))
             (height
               (second
                (luv:gpu-texture-size
                 (mirror-texture (widget-overlay-mirror overlay)))))
             (entry (terminal-film-browser-entry-at
                     frame (* (second uv) height))))
        (when entry
          (choose-terminal-film-browser-entry frame entry))))
    t))

(defun terminal-film-browser-world-frame (display)
  "Return center, half-right, texture-down, and outward axes for DISPLAY."
  (let* ((surface (luvcraft:terminal-display-surface display))
         (frame (luvcraft::terminal-face-frame
                 (luvcraft:terminal-surface-face surface)))
         (right (luvcraft::voxel-direction-vec3
                 (luvcraft::terminal-face-frame-right frame)))
         (up (luvcraft::voxel-direction-vec3
              (luvcraft::terminal-face-frame-up frame)))
         (outward (luvcraft::voxel-direction-vec3
                   (luvcraft::terminal-face-frame-outward frame)))
         (width (luvcraft::terminal-surface-physical-width surface))
         (height (luvcraft::terminal-surface-physical-height surface))
         (lower-left (luvcraft::terminal-surface-lower-left-point surface 0.010))
         (center
           (luvcraft::terminal-offset-point
            lower-left right (/ width 2.0) up (/ height 2.0))))
    (values center
            (vec:vec3-scale right (/ width 2.0))
            (vec:vec3-scale up (- (/ height 2.0)))
            outward)))

(defun open-terminal-film-browser (display)
  "Make one embedded McCLIM browser owned by DISPLAY's authored wall."
  (let* ((session (luvcraft::terminal-display-session display))
         (port (find-port :server-path '(:luv)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (frame
           (let ((*embedded-mirror-target*
                   (luvcraft:luvcraft-session-canvas session))
                 (*embedded-mirror-context*
                   (luvcraft::luvcraft-session-context session))
                 (*embedded-mirror-device*
                   (luvcraft::luvcraft-session-device session)))
             (make-application-frame
              'terminal-film-browser :frame-manager manager :enable t
              :display display :directory (user-homedir-pathname)))))
    (setf (frame-pretty-name frame) "terminal wall film browser")
    (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
      (multiple-value-bind (center right-axis up-axis normal-axis)
          (terminal-film-browser-world-frame display)
        (let ((overlay
                (make-instance
                 'terminal-film-browser-overlay
                 :session session :frame frame :mirror mirror :display display
                 :center center :right-axis right-axis :up-axis up-axis
                 :normal-axis normal-axis :height-scale 0.0)))
          (setf (mirror-compositor mirror) overlay
                (luvcraft:terminal-display-mode-overlay display) overlay)
          (refresh-terminal-film-browser frame :reset-offset-p t)
          overlay)))))

(defmethod luvcraft:change-terminal-display-mode :after
    ((display luvcraft:terminal-display)
     (session luvcraft:luvcraft-session) (mode (eql :film)))
  (declare (ignore session mode))
  (unless (luvcraft:terminal-display-mode-overlay display)
    (open-terminal-film-browser display)))
