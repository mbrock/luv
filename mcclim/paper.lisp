;;; A sheet of PDF paper, standing in the luvcraft world.
;;;
;;; Not a picture of a page.  MuPDF gives back the typeset lines of a page --
;;; each with its box, its size, and the font it was set in -- and this draws
;;; them as text, so the glyphs go through the same Slug rasterizer the rest
;;; of luv's type does and stay sharp at whatever distance the player happens
;;; to be standing.  A rendered pixmap would have been fewer moving parts and
;;; would look like a photograph of paper taped to a wall.
;;;
;;; The sheet itself is analytic: one rounded rectangle, lit top to bottom,
;;; from the same primitive the inventory and the hotbar use.  The texture is
;;; cut to the page's own proportions, so that rectangle is the whole quad --
;;; the thing in the world is the sheet, with no frame, desk, or backdrop
;;; around it to decide what to put in.

(in-package #:mcluv)

(defparameter *paper-texture-width* 500
  "How many texture pixels wide a sheet is drawn.")

(defparameter *paper-texture-height* 680
  "How many tall.  OPEN-LUVCRAFT-PAPER rebinds this to the first page's own
proportions before making the frame, so the quad hung in the world is the
sheet itself -- there is no surround, and so nothing to decide what to put
in it.")

(defparameter *paper-margin* 0
  "How much texture is left around the sheet.  Zero, because the texture is
cut to the page: the sheet is the whole quad and its edge is the page edge.")

(defparameter *paper-document-pathname*
  (merge-pathnames "build/tool-being.pdf"
                   (asdf:system-source-directory "luv"))
  "The document a sheet shows when nobody said which.")

(defparameter *paper-text-ink* (make-rgb-color 0.10 0.09 0.08))
(defparameter *paper-muted-ink* (make-rgb-color 0.52 0.50 0.46))

(defclass paper-pane (application-pane) ())

(define-application-frame luvcraft-paper ()
  ((document :initarg :document :accessor paper-document)
   (page :initform 0 :accessor paper-page)
   ;; The page's own size in points and its lines, refetched when the page
   ;; turns.  Extraction is a few milliseconds and the page turns rarely, so
   ;; there is nothing to be gained by doing it anywhere but here.
   (page-width :initform 1.0 :accessor paper-page-width)
   (page-height :initform 1.0 :accessor paper-page-height)
   (runs :initform '() :accessor paper-runs)
   (painted :initform nil :accessor paper-painted))
  (:menu-bar nil)
  (:panes
   (paper (make-pane 'paper-pane
                     :background +transparent-ink+
                     :default-text-style (make-text-style :serif nil :normal))))
  (:layouts
   (default
    (horizontally (:width *paper-texture-width* :height *paper-texture-height*) paper))))

(defun load-paper-page (frame)
  "Measure the current page and pull its lines out of the document."
  (let ((document (paper-document frame))
        (number (paper-page frame)))
    (multiple-value-bind (width height) (luv.mupdf:page-size document number)
      (setf (paper-page-width frame) (max 1.0 width)
            (paper-page-height frame) (max 1.0 height)))
    (setf (paper-runs frame)
          (handler-case (luv.mupdf:page-text-runs document number)
            (error () '())))
    frame))

(defun paper-sheet-geometry (frame)
  "Where the sheet sits in the texture, and how many texture pixels a point is.

The page keeps its own proportions, so a page whose shape does not match the
texture's leaves margins rather than being stretched to fit.  The texture is
shaped like a page to keep those margins small."
  (let* ((available-height (- *paper-texture-height* (* 2 *paper-margin*)))
         (available-width (- *paper-texture-width* (* 2 *paper-margin*)))
         (scale (min (/ available-height (paper-page-height frame))
                     (/ available-width (paper-page-width frame))))
         (width (* scale (paper-page-width frame)))
         (height (* scale (paper-page-height frame))))
    (values (/ (- *paper-texture-width* width) 2.0)
            (/ (- *paper-texture-height* height) 2.0)
            width height scale)))

(defun paper-run-fitted-size (pane run scale)
  "The size to set RUN at so it occupies the box the document gave it.

The PDF was typeset in its own font and this is not that font: at the same
nominal size the substitute sets wider or narrower, and a line that was
justified to the measure spills off the page.  The document already says how
wide each line ended up, so the size is condensed until the line fits it
again.  That keeps the page's shape -- its margins, its ragged edge, where
the lines break -- which is what makes it read as this document rather than
as its words."
  (let* ((nominal (* scale (luv.mupdf:text-run-size run)))
         (target (* scale (luv.mupdf:text-run-width run)))
         (string (luv.mupdf:text-run-string run)))
    (if (or (<= nominal 0.0) (<= target 0.0) (zerop (length string)))
        nominal
        (let ((measured (text-size pane string
                                   :text-style (make-text-style :serif nil
                                                                nominal))))
          (if (<= measured target)
              nominal
              (max 1.0 (* nominal (/ target measured))))))))

(defmethod handle-repaint ((pane paper-pane) region)
  (declare (ignore region))
  (let ((frame (pane-frame pane)))
    (with-sheet-medium (medium pane)
      (when (typep medium 'luv-raster-medium)
        (clear-raster-medium-reliefs medium))
      (with-bounding-rectangle* (left top right bottom) pane
        ;; The whole texture is paper.  The texture is shaped like a page, so
        ;; the little the page's own proportions leave over is margin -- and
        ;; margin on a sheet of paper is more paper, not a backdrop.
        (draw-analytic-rounded-rectangle*
         medium left top right bottom :radius 4
         :ink (make-linear-gradient
               0 top 0 bottom
               (make-rgb-color 0.985 0.980 0.960)
               (make-rgb-color 0.900 0.895 0.870))))
      (multiple-value-bind (sheet-left sheet-top width height scale)
          (paper-sheet-geometry frame)
        (declare (ignore width height))
        (dolist (run (paper-runs frame))
          (let ((size (paper-run-fitted-size pane run scale)))
            (when (>= size 3.0)
              (draw-text* pane (luv.mupdf:text-run-string run)
                          (+ sheet-left (* scale (luv.mupdf:text-run-x run)))
                          (+ sheet-top (* scale (luv.mupdf:text-run-y run)))
                          :align-y :top
                          :text-size size
                          :ink *paper-text-ink*))))
        (draw-text* pane
                    (format nil "~A  ·  ~D / ~D"
                            (file-namestring
                             (luv.mupdf:document-pathname (paper-document frame)))
                            (1+ (paper-page frame))
                            (luv.mupdf:document-page-count (paper-document frame)))
                    (/ *paper-texture-width* 2.0) (- *paper-texture-height* 9)
                    :align-x :center :align-y :center :text-size 10
                    :ink *paper-muted-ink*))
      (setf (paper-painted frame) (paper-paint-state frame)))))

(defun paper-paint-state (frame)
  (list (paper-page frame) (length (paper-runs frame))))

(defun repaint-paper (frame)
  (let ((mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (if (typep mirror 'luv-gpu-mirror)
        (repaint-gpu-mirror mirror)
        (progn
          (repaint-sheet (mirror-sheet mirror) +everywhere+)
          (present-mirror mirror))))
  frame)

(defun turn-paper-page (frame delta)
  "Move DELTA pages, staying inside the document."
  (let* ((count (luv.mupdf:document-page-count (paper-document frame)))
         (wanted (max 0 (min (1- count) (+ (paper-page frame) delta)))))
    (unless (= wanted (paper-page frame))
      (setf (paper-page frame) wanted)
      (load-paper-page frame)
      t)))

;;;; The overlay

(defclass luvcraft-paper-overlay (luvcraft-widget-overlay) ())

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-paper-overlay) session pass surface-texture)
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
             (state (world-device-clip-state
                     overlay session (first viewport-size)
                     (second viewport-size)))
             (frame-state
               (ensure-spinning-compositor-frame-state overlay surface-texture)))
        (setf (widget-overlay-render-state overlay) state)
        (luv:write-buffer (spinning-frame-state-buffer frame-state) state)
        (luv:set-pipeline pass (spinning-compositor-pipeline overlay))
        (luv:set-bind-group pass 0
                            (spinning-frame-state-bind-group frame-state))
        (luv:draw pass 4))))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-paper-overlay) session)
  (declare (ignore session))
  (let ((frame (widget-overlay-frame overlay)))
    (unless (equal (paper-paint-state frame) (paper-painted frame))
      (repaint-paper frame)))
  overlay)

(defmethod luvcraft:handle-luvcraft-focus-event
    ((overlay luvcraft-paper-overlay) session canvas
     (event luv:canvas-key-press-event))
  (declare (ignore canvas))
  (let ((frame (widget-overlay-frame overlay)))
    (case (luv:canvas-key-event-key-name event)
      (:tab nil)
      (:escape (luvcraft:unfocus-luvcraft-session session) t)
      ((:right :down :page-down :space) (turn-paper-page frame 1) t)
      ((:left :up :page-up) (turn-paper-page frame -1) t)
      (:home (turn-paper-page frame most-negative-fixnum) t)
      (:end (turn-paper-page frame most-positive-fixnum) t)
      (t t))))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-paper-overlay) session canvas
     (event luv:canvas-pointer-wheel-event))
  "A wheel over the sheet turns pages, one notch at a time."
  (declare (ignore session canvas))
  (when (luvcraft-widget-texture-coordinate
         overlay
         (luv:canvas-pointer-event-x event)
         (luv:canvas-pointer-event-y event))
    (let ((scroll (luv:canvas-pointer-event-scroll-y event)))
      (unless (zerop scroll)
        (turn-paper-page (widget-overlay-frame overlay)
                         (if (plusp scroll) -1 1))))
    t))

(defmethod luvcraft:handle-luvcraft-overlay-event
    ((overlay luvcraft-paper-overlay) session canvas
     (event luv:canvas-pointer-button-press-event))
  "Clicking the right half goes forward, the left half back."
  (declare (ignore session canvas))
  (alexandria:when-let
      ((uv (luvcraft-widget-texture-coordinate
            overlay
            (luv:canvas-pointer-event-x event)
            (luv:canvas-pointer-event-y event))))
    (when (eq :left (luv:canvas-pointer-event-button event))
      (turn-paper-page (widget-overlay-frame overlay)
                       (if (< (first uv) 0.5) -1 1)))
    t))

(defmethod luvcraft:release-luvcraft-overlay
    ((overlay luvcraft-paper-overlay))
  (let ((frame (widget-overlay-frame overlay)))
    (ignore-errors (luv.mupdf:close-document (paper-document frame))))
  (call-next-method))

;;;; Mounting it

(defun open-luvcraft-paper (session &key (pathname *paper-document-pathname*)
                                         (distance 1.9) (width 1.9)
                                         (right-offset 0.0))
  "Hang PATHNAME in front of SESSION's camera as a sheet standing in the world.

A sheet of paper is not a screen bolted to a wall, so this does not go through
the terminal's display modes: it is its own object, placed where the player is
looking and focused with TAB like anything else."
  (let* ((port (find-port :server-path '(:luv)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (document (luv.mupdf:open-document pathname))
         (frame
           (let ((*embedded-mirror-target*
                   (luvcraft:luvcraft-session-canvas session))
                 (*embedded-mirror-context*
                   (luvcraft::luvcraft-session-context session))
                 (*embedded-mirror-device*
                   (luvcraft::luvcraft-session-device session)))
             (make-application-frame
              'luvcraft-paper :frame-manager manager :enable t
              :document document))))
    (setf (frame-pretty-name frame) "paper")
    (load-paper-page frame)
    (let* ((mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
           (source-size (luv:gpu-texture-size (mirror-texture mirror)))
           (aspect (/ (first source-size) (second source-size)))
           (camera (luvcraft:luvcraft-session-camera session))
           (camera-position (luvcraft:camera-position camera)))
      (multiple-value-bind (right ignored-up forward)
          (luvcraft:camera-basis camera)
        (declare (ignore ignored-up))
        (let ((overlay
                (make-instance
                 'luvcraft-paper-overlay
                 :session session :frame frame :mirror mirror
                 :center (add-scaled-vector camera-position
                                            forward distance
                                            right right-offset)
                 :right-axis (vec:vec3-scale right (/ width 2.0))
                 :up-axis (vec:make-vec3 0.0 (- (/ width aspect 2.0)) 0.0)
                 :normal-axis (vec:vec3-scale forward -1.0)
                 :height-scale 0.0)))
          (setf (mirror-compositor mirror) overlay)
          (luvcraft:add-luvcraft-overlay session overlay)
          (repaint-paper frame)
          overlay)))))

(defun close-luvcraft-paper (overlay)
  "Take a sheet down and close the document behind it."
  (check-type overlay luvcraft-paper-overlay)
  (luvcraft:remove-luvcraft-overlay (widget-overlay-session overlay) overlay)
  nil)
