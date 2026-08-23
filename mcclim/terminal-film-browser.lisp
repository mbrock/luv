;;; A McCLIM file browser presented directly on a focused terminal wall.

(in-package #:mcluv)

(defparameter *terminal-film-extensions*
  '("mp4" "m4v" "mov" "mkv" "webm" "avi" "mpg" "mpeg" "ts")
  "File suffixes offered as playable films by the wall browser.")

(defconstant +terminal-film-browser-width+ 720)
(defconstant +terminal-film-browser-height+ 440)
(defconstant +terminal-film-browser-header-height+ 70
  "Where the first row starts.  The painter and the hit-test both measure
from here, so a row is always where the click says it is.")
(defconstant +terminal-film-browser-footer-height+ 38)
(defconstant +terminal-film-browser-row-height+ 27)
(defconstant +terminal-film-browser-page-size+ 12)
(defconstant +terminal-film-browser-path-limit+ 52)

;; The same stone the communicator is cased in.  Two things mounted on the
;; same wall that behave alike should look alike, and a light surround gives
;; the faceplate's own reflection somewhere to land instead of blowing out
;; across a dark screen.
(defparameter *film-browser-bezel-ink* (make-rgb-color 0.55 0.51 0.43))
(defparameter *film-browser-bezel-light* (make-rgb-color 0.73 0.69 0.59))
(defparameter *film-browser-bezel-dark* (make-rgb-color 0.26 0.24 0.20))
(defparameter *film-browser-screen-ink* (make-rgb-color 0.075 0.075 0.075))
(defparameter *film-browser-row-ink* (make-rgb-color 0.125 0.125 0.125))
(defparameter *film-browser-text-ink* (make-rgb-color 0.92 0.93 0.94))
(defparameter *film-browser-muted-ink* (make-rgb-color 0.50 0.53 0.57))

(defstruct terminal-film-entry
  pathname
  kind
  label
  (size nil))

(defun terminal-film-size (pathname)
  "PATHNAME's length in bytes, or NIL if it will not say."
  (ignore-errors
   (with-open-file (stream pathname :element-type '(unsigned-byte 8))
     (file-length stream))))

(defun terminal-film-size-label (size)
  (cond ((null size) "")
        ((< size 1048576) (format nil "~D KB" (round size 1024)))
        ((< size 1073741824) (format nil "~,1F MB" (/ size 1048576.0)))
        (t (format nil "~,1F GB" (/ size 1073741824.0)))))

(defparameter *terminal-film-entry-marks*
  '((:directory . "▸") (:film . "▶") (:previous . "↑") (:next . "↓"))
  "A mark per row kind.  It replaces the bracketed word the label used to
carry: the shape says what the row is, and the name gets the whole line.")

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
              :pathname parent :kind :directory :label "..")))
     (mapcar
      (lambda (pathname)
        (make-terminal-film-entry
         :pathname pathname :kind :directory
         :label (format nil "~A/" (terminal-film-directory-name pathname))))
      directories)
     (mapcar
      (lambda (pathname)
        (make-terminal-film-entry
         :pathname pathname :kind :film
         :label (file-namestring pathname)
         :size (terminal-film-size pathname)))
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
       (list (make-terminal-film-entry :kind :previous :label "previous page")))
     (subseq entries offset (min (length entries) (+ offset content-count)))
     (when more-p
       (list (make-terminal-film-entry :kind :next :label "next page"))))))

(defun terminal-film-entry-color (entry)
  "The mark's colour.  A row is dark and the ink is what carries the kind:
twelve saturated bars fight each other and the filename loses."
  (ecase (terminal-film-entry-kind entry)
    (:directory (make-rgb-color 0.44 0.68 0.92))
    (:film (make-rgb-color 0.78 0.56 0.95))
    ((:previous :next) (make-rgb-color 0.55 0.58 0.62))))

(defmethod handle-repaint ((pane terminal-film-browser-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (entries (terminal-film-browser-visible-entries frame))
         (all (terminal-film-browser-frame-entries frame))
         (medium (sheet-medium pane)))
    (with-bounding-rectangle* (left top right bottom) pane
      ;; The same chassis the communicator wears, because they are the same
      ;; wall showing two different things.
      (draw-rectangle* pane left top right bottom
                       :ink *film-browser-bezel-ink*)
      (draw-rectangle* pane (+ left 3) (+ top 3) (- right 3) (- bottom 3)
                       :filled nil :line-thickness 3
                       :ink *film-browser-bezel-light*)
      (draw-rectangle* pane (+ left 6) (+ top 6) (- right 6) (- bottom 6)
                       :filled nil :line-thickness 2
                       :ink *film-browser-bezel-dark*)
      (draw-analytic-rounded-rectangle*
       medium (+ left 12) (+ top 12) (- right 12) (- bottom 12) :radius 6
       :ink *film-browser-screen-ink*)
      ;; Header
      (draw-text* pane "Films" (+ left 24) (+ top 30)
                  :align-y :center :text-size 19 :ink *film-browser-text-ink*)
      (draw-text* pane
                  (terminal-film-browser-path-label
                   (terminal-film-browser-directory frame))
                  (+ left 24) (+ top 52) :align-y :center :text-size 12
                  :ink *film-browser-muted-ink*)
      (draw-text* pane (format nil "~D item~:P" (length all))
                  (- right 24) (+ top 30)
                  :align-x :right :align-y :center :text-size 12
                  :ink *film-browser-muted-ink*)
      (draw-line* pane (+ left 18) (+ top 64) (- right 18) (+ top 64)
                  :ink (make-rgb-color 0.16 0.17 0.19))
      (loop for entry in entries
            for index from 0
            for row-top = (+ top +terminal-film-browser-header-height+
                             (* index +terminal-film-browser-row-height+))
            for row-bottom = (+ row-top (- +terminal-film-browser-row-height+ 2))
            do (when (oddp index)
                 (draw-analytic-rounded-rectangle*
                  medium (+ left 16) row-top (- right 16) row-bottom
                  :radius 4 :ink *film-browser-row-ink*))
               (draw-text* pane
                           (or (cdr (assoc (terminal-film-entry-kind entry)
                                           *terminal-film-entry-marks*))
                               "·")
                           (+ left 30) (/ (+ row-top row-bottom) 2.0)
                           :align-x :center :align-y :center :text-size 14
                           :ink (terminal-film-entry-color entry))
               (draw-text* pane (terminal-film-entry-label entry)
                           (+ left 48) (/ (+ row-top row-bottom) 2.0)
                           :align-y :center :text-size 13
                           :ink *film-browser-text-ink*)
               (alexandria:when-let ((size (terminal-film-entry-size entry)))
                 (draw-text* pane (terminal-film-size-label size)
                             (- right 30) (/ (+ row-top row-bottom) 2.0)
                             :align-x :right :align-y :center :text-size 11
                             :ink *film-browser-muted-ink*)))
      ;; Footer
      (draw-line* pane (+ left 18) (- bottom 40) (- right 18) (- bottom 40)
                  :ink (make-rgb-color 0.16 0.17 0.19))
      (draw-text* pane (terminal-film-browser-message frame)
                  (+ left 24) (- bottom 25)
                  :align-y :center :text-size 12
                  :ink *film-browser-muted-ink*))))

(defun repaint-terminal-film-browser (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (check-type mirror luv-gpu-mirror)
    (repaint-gpu-mirror mirror))
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

(defclass terminal-film-browser-overlay (luvcraft-world-widget-overlay)
  ((display :initarg :display :reader terminal-film-browser-overlay-display)))

(defun displace-terminal-mode-overlay (display wanted-type)
  "Return DISPLAY's mode child if it is already WANTED-TYPE, else drop it.

Every mode installs a different child and only one can be mounted, so
changing mode has to take the previous one down.  It is released rather than
merely forgotten: the child being replaced may own a thread or a socket, and
a Telegram console left running behind a film is a connection nobody closes."
  (let ((overlay (luvcraft:terminal-display-mode-overlay display)))
    (cond ((null overlay) nil)
          ((typep overlay wanted-type) overlay)
          (t
           (setf (luvcraft:terminal-display-mode-overlay display) nil)
           (ignore-errors (luvcraft:release-luvcraft-overlay overlay))
           nil))))

(luv:zdefmethod (luvcraft:encode-luvcraft-overlay :zone :film-browser/encode)
    ((overlay terminal-film-browser-overlay) session pass surface-texture)
  "Draw the browser's semantic command stream directly on its wall."
  (declare (ignore pass))
  (place-widget-overlay-on-surface
   overlay (terminal-film-browser-overlay-display overlay) session)
  (let ((viewport-size
          (luv:canvas-extent (luvcraft::luvcraft-session-context session))))
    (prepare-direct-widget-overlay
     overlay session surface-texture
     (world-device-clip-state
      overlay session (first viewport-size) (second viewport-size))))
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
             (height (second (widget-overlay-logical-size overlay)))
             (entry (terminal-film-browser-entry-at
                     frame (* (second uv) height))))
        (when entry
          (choose-terminal-film-browser-entry frame entry))))
    t))

(defun open-terminal-film-browser (display)
  "Make one embedded McCLIM browser owned by DISPLAY's authored wall."
  (let* ((session (luvcraft::terminal-display-session display))
         (port (find-port :server-path '(:luv-gpu)))
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
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (overlay
             (make-instance
              'terminal-film-browser-overlay
              :session session :frame frame :mirror mirror :display display
              :height-scale 0.0)))
      (place-widget-overlay-on-surface overlay display session)
      (setf (mirror-compositor mirror) overlay
            (luvcraft:terminal-display-mode-overlay display) overlay)
      (refresh-terminal-film-browser frame :reset-offset-p t)
      overlay)))

(defmethod luvcraft:change-terminal-display-mode :after
    ((display luvcraft:terminal-display)
     (session luvcraft:luvcraft-session) (mode (eql :film)))
  (declare (ignore session mode))
  ;; A detached display still has useful logical mode behavior (and is how
  ;; the core geometry is tested), but it has no canvas on which to mount a
  ;; browser overlay.
  (when (luvcraft::terminal-display-session display)
    (unless (displace-terminal-mode-overlay
             display 'terminal-film-browser-overlay)
      (open-terminal-film-browser display))))
