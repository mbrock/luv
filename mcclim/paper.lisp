;;; A sheet of PDF paper, standing in the luvcraft world.
;;;
;;; The page is split the way #S8LIJP says to split it: the sheet is a raster
;;; and the type is geometry.
;;;
;;; The sheet is one analytic rounded rectangle drawn into the mirror texture,
;;; which is exactly what that path is good at -- a smooth fill with no detail
;;; finer than a texel.  The texture is cut to the page's own proportions, so
;;; that rectangle is the whole quad and there is no surround to decide about.
;;;
;;; The text is not in the texture at all.  MuPDF gives back each typeset line
;;; with its baseline, its size, and the font it was set in; those are shaped
;;; through Slug and emitted as world-space glyph instances on the sheet's own
;;; plane, the same way a terminal wall emits its screen.  Slug evaluates the
;;; outlines in the fragment shader at whatever resolution the pixel has, so
;;; the page is as sharp far away as it is up close -- which is the whole
;;; point, and which drawing the text into the texture could not do at any
;;; texture size (#I3G0S7).

(in-package #:mcluv)

(defparameter *paper-texture-width* 1200
  "How many texture pixels wide a sheet is drawn.

Sized for reading distance, which is the only distance at which a page of ten
point body text is legible at all -- on paper too.  At 500 the body landed on
ten pixels a line and broke up; much past this it is being minified instead,
and the mirror texture has no mipmaps, so that aliases rather than softens.")

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

(defparameter *paper-ink-lift* 0.004
  "How far off the sheet the type stands, in world units.

Ink on paper is not coplanar with it.  Nor can it be here: glyphs at exactly
the sheet's depth lose the depth test against the sheet and come out hollow.")

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

(defun paper-sheet-geometry (frame texture-width texture-height)
  "Where the sheet sits in the texture, and how many texture pixels a point is.

Measured from the texture the pane actually has rather than from the
parameter it was asked for: the first page decides the shape, later pages of
the same document need not be the same shape, and a page that is not gets
centred with paper around it rather than stretched."
  (let* ((available-height (- texture-height (* 2 *paper-margin*)))
         (available-width (- texture-width (* 2 *paper-margin*)))
         (scale (min (/ available-height (paper-page-height frame))
                     (/ available-width (paper-page-width frame))))
         (width (* scale (paper-page-width frame)))
         (height (* scale (paper-page-height frame))))
    (values (/ (- texture-width width) 2.0)
            (/ (- texture-height height) 2.0)
            width height scale)))

(defmethod handle-repaint ((pane paper-pane) region)
  "Paint the sheet, and only the sheet.  The type is world geometry."
  (declare (ignore region))
  (let ((frame (pane-frame pane)))
    (with-sheet-medium (medium pane)
      (when (typep medium 'luv-raster-medium)
        (clear-raster-medium-reliefs medium))
      (with-bounding-rectangle* (left top right bottom) pane
        (draw-analytic-rounded-rectangle*
         medium left top right bottom :radius 4
         :ink (make-linear-gradient
               0 top 0 bottom
               (make-rgb-color 0.985 0.980 0.960)
               (make-rgb-color 0.900 0.895 0.870))))
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


;;;; Setting the page as geometry
;;;;
;;;; One text run holds every line of the page.  The whole page shares one
;;;; glyph atlas and one instanced draw, which is why the lines are shaped
;;;; separately but their instance records are concatenated: the atlas is a
;;;; property of the glyph set, and a set assembled from the whole page is one
;;;; binding rather than one per line.

(defparameter *paper-faces*
  '((:italic . "DejaVuSerifCondensed-Italic.ttf")
    (:bold . "DejaVuSerifCondensed-Bold.ttf")
    (:regular . "DejaVuSerifCondensed.ttf"))
  "The faces a page is set in.

Condensed on purpose.  The documents this reads are set in Palatino-like
faces and the ordinary DejaVu serif is about a third wider, which is what
forced the old raster path to squeeze every line to make it fit.  A condensed
face lands close enough that the lines simply sit where the document put
them.")

(defun paper-face-pathname (font-name)
  "The face to set a run in, from what the document says it used."
  (let ((name (or font-name "")))
    (cl-dejavu:font-pathname
     (cdr (assoc (cond ((search "Italic" name) :italic)
                       ((search "Oblique" name) :italic)
                       ((search "Bold" name) :bold)
                       (t :regular))
                 *paper-faces*)))))

(defun paper-plane (overlay)
  "The sheet's top-left corner and the unit vectors that span it.

Returns the corner, a unit vector along the page's own left-to-right, a unit
vector along its top-to-bottom, and how many world units a PDF point is."
  (let* ((center (widget-overlay-center overlay))
         (right (widget-overlay-right-axis overlay))
         (up (widget-overlay-up-axis overlay))
         (frame (widget-overlay-frame overlay))
         (half-width (vec:vec3-length right))
         (right-unit (vec:vec3-scale right (/ 1.0 (max 1e-6 half-width))))
         (down-unit (vec:vec3-scale up (/ 1.0 (max 1e-6 (vec:vec3-length up)))))
         ;; UP-AXIS points down the texture, so the top-left corner is one
         ;; half-axis back along both.  The type also stands a fraction of a
         ;; centimetre off the sheet: glyphs exactly coplanar with the paper
         ;; lose the depth test against it and come out hollow, which is the
         ;; same reason a terminal wall lifts its glyph run off its blocks.
         (corner (add-scaled-vector center
                                    right -1.0
                                    up -1.0
                                    (widget-overlay-normal-axis overlay)
                                    *paper-ink-lift*))
         (points-per-world (/ (* 2.0 half-width)
                              (max 1.0 (paper-page-width frame)))))
    (values corner right-unit down-unit points-per-world)))

(defun paper-run-instances (overlay run atlas glyphs shaped font-loader)
  "Instance records placing one shaped line on the sheet at its own baseline."
  (multiple-value-bind (corner right-unit down-unit scale) (paper-plane overlay)
    (multiple-value-bind (min-x min-y max-x max-y)
        (luv.slug:slug-text-extents glyphs shaped font-loader)
      (let* ((em (* scale (luv.mupdf:text-run-size run)))
             (text-up (vec:vec3-scale down-unit -1.0))
             ;; Where the pen starts, in world space.
             (pen (add-scaled-vector
                   corner
                   right-unit (* scale (luv.mupdf:text-run-baseline-x run))
                   down-unit (* scale (luv.mupdf:text-run-baseline-y run))))
             ;; MAKE-WORLD-TEXT-INSTANCES centres a run on its own inked
             ;; extents, so the centre that puts the pen where it belongs is
             ;; the pen displaced by half of those extents.
             (center (add-scaled-vector
                      pen
                      right-unit (* em (/ (+ min-x max-x) 2.0))
                      text-up (* em (/ (+ min-y max-y) 2.0)))))
        (luvcraft::make-world-text-instances
         glyphs atlas center right-unit text-up em min-x min-y max-x max-y
         :ink '(0.09 0.08 0.07))))))

(defun paper-shaped-lines (overlay)
  "Shape every line of the current page and place it on the sheet.

Returns the concatenated instance data, every glyph placement, and the atlas
they share, or NIL when the page has no drawable text."
  (let* ((frame (widget-overlay-frame overlay))
         (cache (paper-glyph-cache overlay))
         (shapings '()))
    ;; Shape first, so the atlas is packed once for the whole page.
    (dolist (run (paper-runs frame))
      (let ((string (luv.mupdf:text-run-string run)))
        (when (plusp (length (string-trim " " string)))
          (let* ((font (paper-face-pathname (luv.mupdf:text-run-font run)))
                 (shaped (luv.slug:cached-slug-shaped-text cache font string)))
            (zpb-ttf:with-font-loader (loader font)
              (let ((glyphs (luv.slug:make-slug-glyph-placements
                             shaped loader cache font)))
                (when glyphs
                  (push (list run shaped glyphs font) shapings))))))))
    (setf shapings (nreverse shapings))
    (when shapings
      (let* ((all-glyphs (loop for (nil nil glyphs nil) in shapings
                               append glyphs))
             (atlas (luv.slug:slug-glyph-atlas-for cache all-glyphs))
             (records
               (loop for (run shaped glyphs font) in shapings
                     collect (zpb-ttf:with-font-loader (loader font)
                               (paper-run-instances overlay run atlas glyphs
                                                    shaped loader))))
             (total (reduce #'+ records :key #'length))
             (data (make-array total :element-type 'single-float))
             (cursor 0))
        (dolist (record records)
          (replace data record :start1 cursor)
          (incf cursor (length record)))
        (values data all-glyphs atlas)))))

;;;; The overlay

(defclass luvcraft-paper-overlay (luvcraft-widget-overlay)
  ((glyph-cache :initform nil :accessor paper-glyph-cache)
   (text-run :initform nil :accessor paper-text-run)
   (text-generation :initform nil :accessor paper-text-generation)
   (frame-bind-groups :initform (make-hash-table :test #'eq)
                      :reader paper-frame-bind-groups)))

(defun clear-paper-frame-bind-groups (overlay)
  (maphash (lambda (frame group)
             (declare (ignore frame))
             (ignore-errors (luv:destroy group)))
           (paper-frame-bind-groups overlay))
  (clrhash (paper-frame-bind-groups overlay)))

(defun ensure-paper-text-run (overlay session)
  "Build or republish the page's glyph run when the page has changed.

Called at a frame boundary rather than from inside the pass: this creates
pipelines and buffers, and the pass is no place to do that."
  (let ((frame (widget-overlay-frame overlay)))
    (unless (equal (paper-paint-state frame) (paper-text-generation overlay))
      (setf (paper-text-generation overlay) (paper-paint-state frame))
      (let ((device (luvcraft::luvcraft-session-device session)))
        (unless (paper-glyph-cache overlay)
          (setf (paper-glyph-cache overlay)
                (luv.slug:make-slug-glyph-cache device)))
        (multiple-value-bind (data glyphs atlas) (paper-shaped-lines overlay)
          (cond
            ((null data) nil)
            ((paper-text-run overlay)
             (multiple-value-bind (run atlas-changed-p)
                 (luvcraft::replace-world-text-run-instances
                  (paper-text-run overlay) device "page" glyphs atlas data)
               (declare (ignore run))
               ;; A new atlas is a new binding; the old groups name the old
               ;; textures and have to go.
               (when atlas-changed-p
                 (clear-paper-frame-bind-groups overlay))))
            (t
             ;; The scene pass draws into the session's colour texture, not
             ;; into the swapchain surface; a pipeline built for the surface
             ;; format is rejected as an incompatible render pass.
             (setf (paper-text-run overlay)
                   (luvcraft::make-world-text-run-from-instances
                    device
                    (luv:gpu-texture-format
                     (luvcraft::luvcraft-session-color-texture session))
                    "page" (paper-face-pathname nil) nil glyphs atlas
                    (widget-overlay-center overlay) 1.0 data
                    :label "PDF page Slug text")))))))
    (paper-text-run overlay)))

(defun paper-frame-bind-group (overlay run frame device)
  (or (gethash frame (paper-frame-bind-groups overlay))
      (setf (gethash frame (paper-frame-bind-groups overlay))
            (aref (luvcraft::make-world-text-frame-bind-groups
                   run device (luvcraft::luvcraft-frame-uniform-buffer frame))
                  0))))


(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay luvcraft-paper-overlay) session pass surface-texture)
  "Draw the sheet, then set the page on it."
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
        (luv:draw pass 4))
      ;; The type is a second draw in the same pass, in world space, sharing
      ;; the scene's own frame uniform rather than the compositor's.
      (alexandria:when-let ((run (paper-text-run overlay)))
        (let* ((device (luvcraft::luvcraft-session-device session))
               (frame (luvcraft::luvcraft-frame-state session surface-texture))
               (glyphs (luvcraft::world-text-run-glyphs run)))
          (when (plusp (length glyphs))
            (luv:set-pipeline pass (luvcraft::world-text-run-native-pipeline run))
            (luv:set-vertex-buffer
             pass 0 (luvcraft::world-text-run-vertex-buffer run))
            (luv:set-vertex-buffer
             pass 1 (luvcraft::world-text-run-instance-buffer run))
            (luv:set-bind-group
             pass 0 (paper-frame-bind-group overlay run frame device))
            (luv:draw pass 6 (length glyphs)))))))
  overlay)

(defmethod luvcraft:refresh-luvcraft-overlay
    ((overlay luvcraft-paper-overlay) session)
  (let ((frame (widget-overlay-frame overlay)))
    (unless (equal (paper-paint-state frame) (paper-painted frame))
      (repaint-paper frame))
    (ensure-paper-text-run overlay session))
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
  (clear-paper-frame-bind-groups overlay)
  (alexandria:when-let ((run (paper-text-run overlay)))
    (setf (paper-text-run overlay) nil)
    (ignore-errors (luvcraft::release-world-text-run run)))
  (let ((frame (widget-overlay-frame overlay)))
    (ignore-errors (luv.mupdf:close-document (paper-document frame))))
  (call-next-method))

;;;; Mounting it

(defun open-luvcraft-paper (session &key (pathname *paper-document-pathname*)
                                         (distance 1.05) (width 1.25)
                                         (right-offset 0.0))
  "Hang PATHNAME in front of SESSION's camera as a sheet standing in the world.

A sheet of paper is not a screen bolted to a wall, so this does not go through
the terminal's display modes: it is its own object, placed where the player is
looking and focused with TAB like anything else."
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'luv-frame-manager :port port)))
         (document (luv.mupdf:open-document pathname))
         ;; The texture is cut to the first page's proportions, so the quad
         ;; hung in the world is the sheet and there is no surround to fill.
         ;; The size is passed to the frame rather than bound around it: the
         ;; layout is consulted somewhere the binding does not reach, and a
         ;; page-shaped parameter that quietly failed to apply is how the
         ;; sheet came out landscape.
         (texture-height
           (multiple-value-bind (page-width page-height)
               (luv.mupdf:page-size document 0)
             (max 64 (round (* *paper-texture-width*
                               (/ page-height (max 1.0 page-width)))))))
         (frame
           (let ((*embedded-mirror-target*
                   (luvcraft:luvcraft-session-canvas session))
                 (*embedded-mirror-context*
                   (luvcraft::luvcraft-session-context session))
                 (*embedded-mirror-device*
                   (luvcraft::luvcraft-session-device session))
                 (*paper-texture-height* texture-height))
             (make-application-frame
              'luvcraft-paper :frame-manager manager :enable t
              :width *paper-texture-width* :height texture-height
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
