;;; A Ghostty terminal projected across a rectangle of authored world blocks.

(in-package #:luvcraft)

(defclass terminal-grid-domain ()
  ((columns :initarg :columns :reader terminal-grid-domain-columns)
   (rows :initarg :rows :reader terminal-grid-domain-rows)))

(defmethod domains:domain-cardinality ((domain terminal-grid-domain))
  (* (terminal-grid-domain-columns domain)
     (terminal-grid-domain-rows domain)))

(defun terminal-grid-offset (domain column row)
  "Return the row-major offset for COLUMN,ROW in DOMAIN."
  (let ((columns (terminal-grid-domain-columns domain))
        (rows (terminal-grid-domain-rows domain)))
    (unless (and (<= 0 column) (< column columns)
                 (<= 0 row) (< row rows))
      (error "Terminal cell (~D,~D) is outside ~Dx~D."
             column row columns rows))
    (+ column (* row columns))))

(defun terminal-grid-coordinate (domain offset)
  "Return COLUMN and ROW for row-major OFFSET in DOMAIN."
  (unless (< -1 offset (domains:domain-cardinality domain))
    (error "Terminal offset ~D is outside a ~D-cell grid."
           offset (domains:domain-cardinality domain)))
  (multiple-value-bind (row column)
      (floor offset (terminal-grid-domain-columns domain))
    (values column row)))

(defstruct (terminal-grid-presentation
             (:constructor %make-terminal-grid-presentation
                 (&key domain characters screen)))
  domain
  characters
  ;; The styled Ghostty snapshot behind CHARACTERS, or NIL for plain text.
  screen)

(defstruct terminal-glyph-occurrence
  glyph
  column
  row
  ;; Packed #xRRGGBB foreground and whether the bold face was used.
  foreground
  bold-p)

(defparameter *luvcraft-fonts-directory*
  (asdf:system-relative-pathname "luvcraft/core" "fonts/")
  "The checkout's bundled fonts, captured while the system is loaded.")

(defun luvcraft-font-pathname (name)
  (merge-pathnames name *luvcraft-fonts-directory*))

(defparameter *terminal-display-font-pathname*
  (luvcraft-font-pathname "MonaspaceNeon-Regular.ttf"))

(defparameter *terminal-display-bold-font-pathname*
  (luvcraft-font-pathname "MonaspaceNeon-Bold.ttf"))

(defparameter *terminal-display-default-foreground* #xF4EFE1
  "Packed sRGB #xRRGGBB used for cells without an explicit foreground.")

;;; A terminal cell's colour arrives as a display colour: the byte a shell
;;; would have written to a monitor, whose white means "as bright as this
;;; screen goes".  The scene it is drawn into measures radiance instead, and
;;; a lit tube in a daylit room is emphatically brighter than its own diffuse
;;; surroundings.  These gains are that conversion, and they are also what
;;; carries lit glyphs over the presentation bloom threshold, so the glow
;;; around bright text is the ordinary lens chain seeing an ordinary bright
;;; thing rather than a halo painted onto the terminal.

(defparameter *terminal-ink-emission* 2.3
  "Radiance per unit of glyph ink, in scene units where diffuse white is one.")

(defparameter *terminal-background-emission* 1.15
  "Radiance per unit of cell-background colour, held below the ink's own.")

(defparameter *terminal-font-scale* 1.0
  "The multiplier on the font size that fits a display's grid to its face;
the default a new display takes.")

;;; The frame records screen-right, screen-up, and outward in voxel axes.  EQL
;;; methods keep the six closed orientations inspectable without making block
;;; materials or terminal surfaces branch on their names.

(defstruct (terminal-face-frame
             (:constructor make-terminal-face-frame (right up outward)))
  right
  up
  outward)

(defgeneric terminal-face-frame (face))

(defmethod terminal-face-frame ((face (eql :front)))
  (make-terminal-face-frame
   +voxel-negative-x+ +voxel-positive-y+ +voxel-positive-z+))

(defmethod terminal-face-frame ((face (eql :back)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-positive-y+ +voxel-negative-z+))

(defmethod terminal-face-frame ((face (eql :right)))
  (make-terminal-face-frame
   +voxel-positive-z+ +voxel-positive-y+ +voxel-positive-x+))

(defmethod terminal-face-frame ((face (eql :left)))
  (make-terminal-face-frame
   +voxel-negative-z+ +voxel-positive-y+ +voxel-negative-x+))

(defmethod terminal-face-frame ((face (eql :top)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-positive-z+ +voxel-positive-y+))

(defmethod terminal-face-frame ((face (eql :bottom)))
  (make-terminal-face-frame
   +voxel-positive-x+ +voxel-negative-z+ +voxel-negative-y+))

(defclass terminal-surface ()
  ((world :initarg :world :reader terminal-surface-world)
   (material :initarg :material :reader terminal-surface-material)
   (absent-neighbor-policy
    :initarg :absent-neighbor-policy :initform :air
    :reader terminal-surface-absent-neighbor-policy)
   (face :initarg :face :reader terminal-surface-face)
   ;; The block at screen-space column zero, row zero (lower left).
   (origin :initarg :origin :reader terminal-surface-origin)
   (block-width :initarg :block-width :reader terminal-surface-width)
   (block-height :initarg :block-height :reader terminal-surface-height)
   ;; These keys are the exact authored chunk materializations whose block
   ;; content can change membership or exposure.  Owner keys are the subset
   ;; whose visible native meshes carry the display face itself.
   (dependency-keys :initform nil :accessor terminal-surface-dependency-keys)
   (owner-chunk-keys :initform nil
                     :accessor terminal-surface-owner-chunk-keys)
   (dependency-stamp :initform nil
                     :accessor terminal-surface-retained-dependency-stamp)
   (observed-world-revision
    :initform -1 :accessor terminal-surface-observed-world-revision)
   (state :initform :current :accessor terminal-surface-state)
   (reconciliations :initform 0
                    :accessor terminal-surface-reconciliations)))

;;; A terminal display draws onto *some* rectangle.  The wall's rectangle is
;;; a run of blocks (TERMINAL-SURFACE); a phone's is a small plane in the
;;; hand.  Everything downstream -- glyph placement, cell backgrounds, the
;;; screen panel and faceplate -- asks the surface only these questions.

(defgeneric terminal-surface-axes (surface)
  (:documentation
   "Return SURFACE's unit RIGHT, UP, and OUTWARD vectors as three vec3s, in
the coordinate space the surface's display is drawn in."))

(defgeneric terminal-surface-lower-left-point (surface &optional offset)
  (:documentation
   "Return the point OFFSET cell-depths outside SURFACE's lower left corner,
as seen from its outward side."))

(defgeneric terminal-surface-physical-width (surface)
  (:documentation "SURFACE's width along its right axis, in cells."))

(defgeneric terminal-surface-physical-height (surface)
  (:documentation "SURFACE's height along its up axis, in cells."))

(defgeneric terminal-surface-current-p (surface &optional session)
  (:documentation
   "True when SURFACE's last coherently published derived state is current."))

(defgeneric terminal-surface-focus-score (surface session)
  (:documentation
   "A non-negative score when the display on SURFACE can be entered by TAB,
or NIL.  Lower is nearer."))

(defgeneric terminal-surface-focus-camera-pose (surface session)
  (:documentation
   "The camera pose from which SURFACE's display is best read, or NIL to
leave the camera where it is with only the narrowed field of view."))

(defgeneric terminal-surface-panel-frame (surface session &optional offset)
  (:documentation
   "Where a panel mounted flat on SURFACE sits in the world this frame.

Returns the panel's centre point, its half-width vector along the surface's
right, its half-height vector pointing *down* the texture, and the outward
unit normal -- all in world space, OFFSET cell-depths off the face.  A wall's
answer never changes; a surface carried in the hand answers afresh each
frame, which is why this takes the SESSION and is asked at draw time rather
than once at mounting.")
  (:method (surface session &optional (offset 0.010))
    (declare (ignore session))
    ;; The general surface draws in world space already, so its own axes and
    ;; corner are the answer.
    (multiple-value-bind (right up outward) (terminal-surface-axes surface)
      (let* ((width (terminal-surface-physical-width surface))
             (height (terminal-surface-physical-height surface))
             (lower-left (terminal-surface-lower-left-point surface offset))
             (center (terminal-offset-point
                      lower-left right (/ width 2.0) up (/ height 2.0))))
        (values center
                (vec3-scale right (/ width 2.0))
                (vec3-scale up (- (/ height 2.0)))
                outward)))))

(defmethod terminal-surface-axes ((surface terminal-surface))
  (let ((frame (terminal-face-frame (terminal-surface-face surface))))
    (values (voxel-direction-vec3 (terminal-face-frame-right frame))
            (voxel-direction-vec3 (terminal-face-frame-up frame))
            (voxel-direction-vec3 (terminal-face-frame-outward frame)))))

(defmethod terminal-surface-focus-camera-pose
    ((surface terminal-surface) (session luvcraft-session))
  (destructuring-bind (width height)
      (canvas-extent (luvcraft-session-context session))
    (multiple-value-call
        (lambda (left top right bottom)
          (terminal-focus-camera-pose
           surface width height left top right bottom))
      (luvcraft-session-focus-insets session))))

(defmethod terminal-surface-focus-score
    ((surface terminal-surface) (session luvcraft-session))
  (multiple-value-bind (hit status) (luvcraft-session-target session)
    (declare (ignore status))
    (when hit
      (let ((coordinate (block-ray-hit-coordinate hit)))
        (when (loop for row below (terminal-surface-height surface)
                    thereis
                    (loop for column below (terminal-surface-width surface)
                          thereis
                          (equalp
                           coordinate
                           (terminal-surface-coordinate surface column row))))
          (block-ray-hit-distance hit))))))

(defvar *terminal-display-counter* 0)

(defun next-terminal-display-name ()
  (format nil "wall-~D" (incf *terminal-display-counter*)))

(defclass terminal-display ()
  ((session :initarg :session :initform nil :reader terminal-display-session)
   (surface :initarg :surface :reader terminal-display-surface)
   (mode :initform :shell :accessor terminal-display-mode)
   ;; LUVCRAFT/MCCLIM presentations install their browser here.  The terminal
   ;; remains the focus owner and delegates drawing and events to this child
   ;; only while its mode calls for it.
   (mode-overlay :initform nil :accessor terminal-display-mode-overlay)
   (film-screen :initform nil :accessor terminal-display-film-screen)
   ;; The wall's name, which its shell learns as LUVCRAFT_PARENT_SCREEN, and
   ;; the portal showing a child luvcraft that asked for that name.
   (name :initarg :name :initform (next-terminal-display-name)
         :reader terminal-display-name)
   (portal :initform nil :accessor terminal-display-portal)
   (terminal :initarg :terminal :initform nil :reader terminal-display-terminal)
   (device :initarg :device :initform nil :accessor terminal-display-device)
   (presentation :initarg :presentation :initform nil
                 :accessor terminal-display-presentation)
   (glyph-cache :initarg :glyph-cache :initform nil
                :reader terminal-display-glyph-cache)
   ;; Keyed by (CHARACTER . BOLD-P).
   (glyphs-by-character :initform (make-hash-table :test #'equal)
                        :reader terminal-display-glyphs-by-character)
   (glyph-run :initarg :glyph-run :initform nil
              :accessor terminal-display-glyph-run)
   (font-pathname :initarg :font-pathname :initform nil
                  :reader terminal-display-font-pathname)
   (bold-font-pathname :initarg :bold-font-pathname :initform nil
                       :reader terminal-display-bold-font-pathname)
   (default-foreground :initarg :default-foreground
                       :initform *terminal-display-default-foreground*
                       :reader terminal-display-default-foreground)
   (cell-run :initarg :cell-run :initform nil
             :accessor terminal-display-cell-run)
   (screen-run :initarg :screen-run :initform nil
               :accessor terminal-display-screen-run)
   (faceplate-run :initarg :faceplate-run :initform nil
                  :accessor terminal-display-faceplate-run)
   (margin :initarg :margin :initform 0.12
           :reader terminal-display-margin)
   (font-scale :initarg :font-scale :initform *terminal-font-scale*
               :accessor terminal-display-font-scale)
   (dirty-p :initform nil :accessor terminal-display-dirty-p)
   (refresh-count :initform 0 :accessor terminal-display-refresh-count)
   (frame-bind-groups :initform (make-hash-table :test #'eq)
                      :reader terminal-display-frame-bind-groups)))

(defgeneric change-terminal-display-mode (display session mode)
  (:documentation
   "Select DISPLAY's focused wall MODE.

The built-in modes are the EQL-specialized symbols :SHELL and :FILM.
LUVCRAFT/MCCLIM adds an :AFTER method which supplies the film browser, while
the display continues to own focus and movie lifetime."))

(defmethod change-terminal-display-mode
    ((display terminal-display) (session luvcraft-session) (mode (eql :shell)))
  (stop-terminal-display-film display session)
  (close-terminal-display-portal display)
  (setf (terminal-display-mode display) mode)
  display)

;;; A child luvcraft on the wall.  The shell on this wall carries the portal
;;; server's socket in its environment; a luvcraft started there connects and
;;; asks for this wall by name, and its picture takes the wall over until it
;;; goes away (Ctrl-C in the shell still reaches the shell, so that is how).

(defun open-terminal-display-portal (display mirror)
  "Put the ready child MIRROR on DISPLAY's wall and switch it to :PORTAL mode."
  (let ((session (terminal-display-session display))
        (surface (terminal-display-surface display)))
    (unless session
      (error "Terminal display ~S is not attached to a session." display))
    (close-terminal-display-portal display)
    (stop-terminal-display-film display session)
    (let ((portal
            (open-luvcraft-portal
             session
             :mirror mirror
             :attach-p nil
             :rectangle (lambda (aspect)
                          (terminal-portal-panel display aspect))
             :on-stop (lambda (portal)
                        (declare (ignore portal))
                        (close-terminal-display-portal display)))))
      (setf (terminal-display-portal display) portal
            (terminal-display-mode display) :portal)
      portal)))

(defun terminal-portal-panel (display aspect)
  "The portal panel for DISPLAY: the whole face, and inside it the picture
of ASPECT fitted with the same margin the text grid keeps, so the wall's
proportions hold when a shell becomes a portal.  Returns the panel's origin,
right edge, and up edge, then the picture rectangle in panel UV."
  (let* ((surface (terminal-display-surface display))
         (surface-width (terminal-surface-physical-width surface))
         (surface-height (terminal-surface-physical-height surface))
         (margin (terminal-display-margin display))
         (available-width (- surface-width (* 2 margin)))
         (available-height (- surface-height (* 2 margin)))
         (available-aspect (/ available-width available-height))
         (width (if (> aspect available-aspect)
                    available-width
                    (* available-height aspect)))
         (height (if (> aspect available-aspect)
                     (/ available-width aspect)
                     available-height))
         (left (/ (- surface-width width) 2.0))
         (bottom (/ (- surface-height height) 2.0)))
    (multiple-value-bind (origin right-edge up-edge)
        (terminal-film-rectangle surface (/ surface-width surface-height))
      (values origin right-edge up-edge
              ;; Panel V runs down the picture (the quad flips it), so the
              ;; bottom margin is the top of the rectangle in UV.
              (list (/ left surface-width)
                    (/ bottom surface-height)
                    (/ (+ left width) surface-width)
                    (/ (+ bottom height) surface-height))))))

(defun close-terminal-display-portal (display)
  "Take the child off DISPLAY's wall, if one is there, and go back to the shell."
  (alexandria:when-let ((portal (terminal-display-portal display))
                        (session (terminal-display-session display)))
    (setf (terminal-display-portal display) nil)
    (when (eq :portal (terminal-display-mode display))
      (setf (terminal-display-mode display) :shell))
    (forget-luvcraft-portal session portal)
    (close-luvcraft-portal session portal)
    (setf (terminal-display-dirty-p display) t))
  display)

(defmethod change-terminal-display-mode
    ((display terminal-display) (session luvcraft-session) (mode (eql :film)))
  ;; Selecting Film again while a movie is running returns to its browser.
  (stop-terminal-display-film display session)
  (setf (terminal-display-mode display) mode)
  display)

(defun terminal-display-delegate-overlay (display)
  "DISPLAY's mode child, when the mode is one that presents through it.

:SHELL drives a PTY and owns its own drawing and keys.  Every other mode
installs a presentation overlay and works through it -- except while a film is
actually playing, when the wall is a screen and not a control."
  (unless (or (eq :shell (terminal-display-mode display))
              (terminal-display-film-screen display))
    (terminal-display-mode-overlay display)))

(defun split-terminal-lines (text)
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (string-right-trim '(#\Return) (subseq text start end))
        while end
        do (setf start (1+ end))))

(defun make-terminal-grid-presentation (domain text)
  "Copy formatted terminal TEXT into one exact dense viewport DOMAIN."
  (let* ((columns (terminal-grid-domain-columns domain))
         (rows (terminal-grid-domain-rows domain))
         (characters
           (make-array (domains:domain-cardinality domain)
                       :element-type 'character :initial-element #\Space)))
    (loop for line in (split-terminal-lines text)
          for row below rows
          do (loop for character across line
                   for column below columns
                   do (setf (aref characters
                                  (terminal-grid-offset domain column row))
                            character)))
    (%make-terminal-grid-presentation
     :domain domain :characters characters)))

(defun make-terminal-screen-presentation (domain screen)
  "Copy a styled Ghostty SCREEN snapshot into one exact viewport DOMAIN."
  (let* ((columns (terminal-grid-domain-columns domain))
         (rows (terminal-grid-domain-rows domain))
         (characters
           (make-array (domains:domain-cardinality domain)
                       :element-type 'character :initial-element #\Space)))
    (dotimes (row (min rows (ghostty:terminal-screen-rows screen)))
      (dotimes (column (min columns (ghostty:terminal-screen-columns screen)))
        (setf (aref characters (terminal-grid-offset domain column row))
              (ghostty:terminal-screen-character screen column row))))
    (%make-terminal-grid-presentation
     :domain domain :characters characters :screen screen)))

(defun terminal-presentation-cell-style (presentation column row)
  "Return foreground #xRRGGBB, background #xRRGGBB or NIL, and bold-p."
  (let ((screen (terminal-grid-presentation-screen presentation)))
    (if (and screen
             (< column (ghostty:terminal-screen-columns screen))
             (< row (ghostty:terminal-screen-rows screen)))
        (multiple-value-bind (foreground background)
            (ghostty:terminal-screen-cell-colors screen column row)
          (values foreground background
                 (ghostty:terminal-screen-bold-p screen column row)))
        (values *terminal-display-default-foreground* nil nil))))

(defun terminal-grid-character (presentation column row)
  (aref (terminal-grid-presentation-characters presentation)
        (terminal-grid-offset
         (terminal-grid-presentation-domain presentation) column row)))

(defun terminal-fixture-framed-line (content)
  (format nil "│ ~76A │" content))

(defun terminal-display-fixture ()
  "Return deterministic fake PTY output for the world display proof."
  (let ((escape (code-char 27))
        (return (code-char 13)))
    (with-output-to-string (stream)
      (format stream "~C[2J~C[H" escape escape)
      (dolist (line
               (list
                "┌──────────────────────────────────────────────────────────────────────────────┐"
                (terminal-fixture-framed-line
                 "luvcraft :: ghostty terminal block material")
                "├──────────────────────────────────────────────────────────────────────────────┤"
                (terminal-fixture-framed-line
                 "one 80 x 24 terminal projected across one linked 8 x 5 block surface")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line "$ uname -a")
                (terminal-fixture-framed-line
                 "Darwin little-world arm64 26.0.0")
                (terminal-fixture-framed-line "$ ./scripts/wiki marks next")
                (terminal-fixture-framed-line
                 "NTQ5KY  Bridge Ghostty render-state rows into the world display")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line
                 "Ghostty owns the grid.  Luv owns the voxels.  Slug owns the curves.")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line "terminal surface")
                (terminal-fixture-framed-line "  ├─ material   :terminal")
                (terminal-fixture-framed-line "  ├─ face       :back")
                (terminal-fixture-framed-line "  ├─ blocks     8 x 5")
                (terminal-fixture-framed-line "  └─ cells      80 x 24")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line
                 "the font grid belongs to the whole face rectangle, not to each block")
                (terminal-fixture-framed-line "")
                (terminal-fixture-framed-line "$ echo 'hello from the magic voxel tv'")
                (terminal-fixture-framed-line "hello from the magic voxel tv")
                (terminal-fixture-framed-line "$ █")
                "└──────────────────────────────────────────────────────────────────────────────┘"))
        (write-string line stream)
        (write-char return stream)
        (terpri stream)))))

(defun terminal-offset-point (origin &rest vector-scales)
  (let ((x (vec3-x origin))
        (y (vec3-y origin))
        (z (vec3-z origin)))
    (loop for (vector scale) on vector-scales by #'cddr
          do (incf x (* (vec3-x vector) scale))
             (incf y (* (vec3-y vector) scale))
             (incf z (* (vec3-z vector) scale)))
    (make-vec3 x y z)))

(defun voxel-direction-vec3 (direction)
  (make-vec3 (voxel-direction-dx direction)
             (voxel-direction-dy direction)
             (voxel-direction-dz direction)))

(defun terminal-coordinate-step (coordinate direction &optional (amount 1))
  (make-world-coordinate
   (+ (world-coordinate-x coordinate)
      (* amount (voxel-direction-dx direction)))
   (+ (world-coordinate-y coordinate)
      (* amount (voxel-direction-dy direction)))
   (+ (world-coordinate-z coordinate)
      (* amount (voxel-direction-dz direction)))))

(defun terminal-absent-neighbor-solid-p (policy coordinate)
  (ecase policy
    (:air nil)
    (:solid t)
    (:error
     (error "Terminal surface reached absent terrain at ~S." coordinate))))

(defun terminal-face-exposed-p
    (world material coordinate outward absent-neighbor-policy)
  (multiple-value-bind (block availability)
      (sample-block-at world
                       (world-coordinate-x coordinate)
                       (world-coordinate-y coordinate)
                       (world-coordinate-z coordinate))
    (and (eq availability :resident)
         (eq block material)
         (let ((neighbor (terminal-coordinate-step coordinate outward)))
           (multiple-value-bind (outside outside-availability)
               (sample-block-at world
                                (world-coordinate-x neighbor)
                                (world-coordinate-y neighbor)
                                (world-coordinate-z neighbor))
             (ecase outside-availability
               (:resident (not (block-solid-p outside)))
               (:absent
                (not (terminal-absent-neighbor-solid-p
                      absent-neighbor-policy neighbor)))))))))

;;; Discovery is a discover-once frontier program (#K3PCP3): starting from
;;; one exposed terminal face, admit coplanar neighbours which are the same
;;; material and exposed, mark each once, and retain the admitted component.
;;; The compiled realization walks chunk offsets through the world's chunk
;;; window; no coordinate objects, hash keys, or conses appear per site.

(frontiers:define-frontier-program terminal-surface-discovery
  :family :discover-once
  :frontier-layout :lifo-stack
  :neighborhood (:voxel-relations coplanar-directions)
  :fields ((visited :memo t) terminal exposed)
  :constants (material outward absent-neighbor-policy coplanar-directions memo)
  :admission (and (terminal target) (exposed target))
  :retain-admissions t)

(defun terminal-discovery-visited-lane (memo chunk)
  "The per-chunk visited bits owned by one discovery's MEMO table."
  (or (gethash chunk memo)
      (setf (gethash chunk memo)
            (make-array (chunk-domain-cardinality (block-chunk-domain chunk))
                        :element-type 'bit :initial-element 0))))

(declaim (inline terminal-site-exposed-p))
(defun terminal-site-exposed-p (window chunk offset outward policy)
  "Whether the OUTWARD neighbour of CHUNK/OFFSET leaves that face visible."
  (let ((domain (block-chunk-domain chunk)))
    (multiple-value-bind (local-x local-y local-z)
        (chunk-domain-local-components domain offset)
      (multiple-value-bind (world-x world-y world-z)
          (chunk-domain-world-components domain local-x local-y local-z)
        (multiple-value-bind (neighbor neighbor-offset availability)
            (locate-chunk-window-site
             window
             (+ world-x (voxel-direction-dx outward))
             (+ world-y (voxel-direction-dy outward))
             (+ world-z (voxel-direction-dz outward)))
          (ecase availability
            (:available
             (not (block-solid-p
                   (block-content-at-offset
                    (block-chunk-content neighbor) neighbor-offset))))
            (:unavailable
             (ecase policy
               (:air t)
               (:solid nil)
               (:error
                (error "Terminal surface reached absent terrain at ~S."
                       (make-world-coordinate
                        (+ world-x (voxel-direction-dx outward))
                        (+ world-y (voxel-direction-dy outward))
                        (+ world-z (voxel-direction-dz outward)))))))))))))

(defun terminal-discovery-bindings ()
  (list
   (frontiers:make-frontier-field-binding
    'visited
    :lanes '((bits (terminal-discovery-visited-lane memo materialization)
                   :type simple-bit-vector))
    :read '(= 1 (sbit bits offset))
    :write '(setf (sbit bits offset) 1))
   (frontiers:make-frontier-field-binding
    'terminal
    :lanes '((content (block-chunk-content materialization)))
    :read '(eq (block-content-at-offset content offset) material))
   ;; Exposure probes the outward neighbour through the window; it is only
   ;; worth asking once the site is terminal material, so it reads lazily.
   (frontiers:make-frontier-field-binding
    'exposed
    :read '(terminal-site-exposed-p
            window materialization offset outward absent-neighbor-policy)
    :lazy t)))

(defvar *terminal-discovery-realization* nil)

(defun terminal-discovery-realization ()
  (if (and *terminal-discovery-realization*
           (frontiers:frontier-realization-current-p
            *terminal-discovery-realization*))
      *terminal-discovery-realization*
      (setf *terminal-discovery-realization*
            (frontiers:compile-frontier-program
             'terminal-surface-discovery
             :bindings (terminal-discovery-bindings)
             :site-domain '(block-chunk-domain materialization)))))

(defmethod frontiers:note-frontier-program-redefinition
    ((name (eql 'terminal-surface-discovery)))
  (declare (ignore name))
  (setf *terminal-discovery-realization* nil))

(defun discover-terminal-component (world x y z face material absent-neighbor-policy)
  "Discover the coplanar exposed component of MATERIAL containing X,Y,Z.

Return the retained execution, or NIL and a status when the seed itself is
not an exposed terminal face."
  (let* ((frame (terminal-face-frame face))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame))
         (directions (list right (opposite-voxel-direction right)
                           up (opposite-voxel-direction up)))
         (realization (terminal-discovery-realization))
         (frontier (frontiers:make-realization-frontier realization
                                                        :initial-capacity 64))
         (execution (frontiers:make-realization-execution
                     realization world frontier))
         (memo (make-hash-table :test #'eq)))
    (multiple-value-bind (chunk offset availability)
        (locate-chunk-window-site world x y z)
      (unless (and (eq availability :available)
                   (eq (block-content-at-offset (block-chunk-content chunk) offset)
                       material))
        (return-from discover-terminal-component (values nil :not-terminal)))
      (unless (terminal-site-exposed-p world chunk offset outward
                                       absent-neighbor-policy)
        (return-from discover-terminal-component (values nil :covered)))
      (flet ((constants ()
               (list :material material :outward outward
                     :absent-neighbor-policy absent-neighbor-policy
                     :coplanar-directions directions :memo memo)))
        (apply #'frontiers:admit-frontier-realization-site
               realization world frontier execution chunk offset nil
               (constants))
        (apply #'frontiers:drain-frontier-realization
               realization world frontier execution (constants))))
    (values execution :component)))

(defun map-execution-admitted-sites (function execution)
  "Call FUNCTION with the world X, Y, Z of every retained admitted site."
  (let* ((sites (frontiers:frontier-execution-admitted-sites execution))
         (chunks (frontiers:frontier-site-buffer-materialization-lane sites))
         (offsets (frontiers:frontier-site-buffer-offset-lane sites)))
    (dotimes (index (frontiers:frontier-site-buffer-length sites))
      (let* ((chunk (aref chunks index))
             (domain (block-chunk-domain chunk)))
        (multiple-value-bind (local-x local-y local-z)
            (chunk-domain-local-components domain (aref offsets index))
          (multiple-value-bind (x y z)
              (chunk-domain-world-components domain local-x local-y local-z)
            (funcall function x y z)))))))

(defun find-terminal-surface (world x y z face
                              &key (material *terminal-block*)
                                   (absent-neighbor-policy :air))
  "Discover the maximal exposed rectangular terminal surface at X,Y,Z,FACE.

Return the surface and :RECTANGLE.  A seed which is not terminal, is covered,
or belongs to a non-rectangular coplanar component instead returns NIL and a
descriptive status.  Discovery runs the compiled TERMINAL-SURFACE-DISCOVERY
program; this function only folds the retained component into a rectangle."
  (let* ((frame (terminal-face-frame face))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame)))
    (multiple-value-bind (execution status)
        (discover-terminal-component
         world x y z face material absent-neighbor-policy)
      (unless execution
        (return-from find-terminal-surface (values nil status)))
      (let ((minimum-u most-positive-fixnum) (maximum-u most-negative-fixnum)
            (minimum-v most-positive-fixnum) (maximum-v most-negative-fixnum)
            (count 0)
            (origin-x 0) (origin-y 0) (origin-z 0))
        (flet ((projection (x y z direction)
                 (+ (* x (voxel-direction-dx direction))
                    (* y (voxel-direction-dy direction))
                    (* z (voxel-direction-dz direction)))))
          (map-execution-admitted-sites
           (lambda (x y z)
             (let ((u (projection x y z right))
                   (v (projection x y z up)))
               (incf count)
               (setf minimum-u (min minimum-u u) maximum-u (max maximum-u u)
                     minimum-v (min minimum-v v) maximum-v (max maximum-v v))))
           execution)
          (map-execution-admitted-sites
           (lambda (x y z)
             (when (and (= minimum-u (projection x y z right))
                        (= minimum-v (projection x y z up)))
               (setf origin-x x origin-y y origin-z z)))
           execution))
        (let ((width (1+ (- maximum-u minimum-u)))
              (height (1+ (- maximum-v minimum-v))))
          (unless (= count (* width height))
            (return-from find-terminal-surface
              (values nil :non-rectangular)))
          (let ((surface
                  (make-instance
                   'terminal-surface
                   :world world :material material
                   :absent-neighbor-policy absent-neighbor-policy
                   :face face
                   :origin (make-world-coordinate origin-x origin-y origin-z)
                   :block-width width :block-height height)))
            (initialize-terminal-surface-dependencies surface)
            (values surface :rectangle)))))))

(defun terminal-surface-coordinate (surface column row)
  (let* ((frame (terminal-face-frame (terminal-surface-face surface)))
         (across
           (terminal-coordinate-step
            (terminal-surface-origin surface)
            (terminal-face-frame-right frame) column)))
    (terminal-coordinate-step
     across (terminal-face-frame-up frame) row)))

(defun terminal-surface-shape-current-p (surface)
  "Check SURFACE membership and maximality against authored world content."
  (let* ((world (terminal-surface-world surface))
         (material (terminal-surface-material surface))
         (absent-neighbor-policy
           (terminal-surface-absent-neighbor-policy surface))
         (frame (terminal-face-frame (terminal-surface-face surface)))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame))
         (width (terminal-surface-width surface))
         (height (terminal-surface-height surface)))
    (and
     (loop for row below height always
       (loop for column below width
             always (terminal-face-exposed-p
                     world material
                     (terminal-surface-coordinate surface column row)
                     outward absent-neighbor-policy)))
     ;; A newly adjacent exposed terminal block means this retained rectangle
     ;; is no longer maximal; its renderer must be rediscovered/rebuilt.
     (loop for row below height always
       (and (not (terminal-face-exposed-p
                  world material
                  (terminal-coordinate-step
                   (terminal-surface-coordinate surface 0 row) right -1)
                  outward absent-neighbor-policy))
            (not (terminal-face-exposed-p
                  world material
                  (terminal-coordinate-step
                   (terminal-surface-coordinate surface (1- width) row) right)
                  outward absent-neighbor-policy))))
     (loop for column below width always
       (and (not (terminal-face-exposed-p
                  world material
                  (terminal-coordinate-step
                   (terminal-surface-coordinate surface column 0) up -1)
                  outward absent-neighbor-policy))
            (not (terminal-face-exposed-p
                  world material
                  (terminal-coordinate-step
                   (terminal-surface-coordinate surface column (1- height)) up)
                  outward absent-neighbor-policy)))))))

(defun terminal-surface-dependency-coordinates (surface)
  "Return the cold set of cells whose content determines SURFACE validity."
  (let* ((frame (terminal-face-frame (terminal-surface-face surface)))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame))
         (width (terminal-surface-width surface))
         (height (terminal-surface-height surface))
         (coordinates nil))
    (labels ((observe (coordinate)
               (push coordinate coordinates)
               (push (terminal-coordinate-step coordinate outward)
                     coordinates)))
      (dotimes (row height)
        (dotimes (column width)
          (observe (terminal-surface-coordinate surface column row)))
        (observe
         (terminal-coordinate-step
          (terminal-surface-coordinate surface 0 row) right -1))
        (observe
         (terminal-coordinate-step
          (terminal-surface-coordinate surface (1- width) row) right)))
      (dotimes (column width)
        (observe
         (terminal-coordinate-step
          (terminal-surface-coordinate surface column 0) up -1))
        (observe
         (terminal-coordinate-step
          (terminal-surface-coordinate surface column (1- height)) up))))
    coordinates))

(defun terminal-coordinate-chunk-key (space coordinate)
  (multiple-value-bind (chunk-x chunk-y chunk-z)
      (voxel-space-decompose-components
       space
       (world-coordinate-x coordinate)
       (world-coordinate-y coordinate)
       (world-coordinate-z coordinate))
    (chunk-key chunk-x chunk-y chunk-z)))

(defun terminal-sort-chunk-keys (keys)
  (sort (remove-duplicates keys :test #'equal)
        (lambda (left right)
          (loop for a in left
                for b in right
                when (/= a b) return (< a b)
                finally (return nil)))))

(defun terminal-surface-dependency-stamp (surface)
  "Name the authored chunk materializations which justify SURFACE."
  (let ((world (terminal-surface-world surface)))
    (loop for key in (terminal-surface-dependency-keys surface)
          collect
          (multiple-value-bind (chunk present-p)
              (apply #'world-chunk-at world key)
            (if present-p
                (list (copy-list key)
                      (block-chunk-incarnation chunk)
                      (block-chunk-revision chunk))
                (list (copy-list key) nil))))))

(defun initialize-terminal-surface-dependencies (surface)
  (let* ((world (terminal-surface-world surface))
         (space (block-world-space world))
         (owner-keys
           (loop for row below (terminal-surface-height surface)
                 append
                 (loop for column below (terminal-surface-width surface)
                       collect
                       (terminal-coordinate-chunk-key
                        space
                        (terminal-surface-coordinate
                         surface column row)))))
         (dependency-keys
           (mapcar (lambda (coordinate)
                     (terminal-coordinate-chunk-key space coordinate))
                   (terminal-surface-dependency-coordinates surface))))
    (setf (terminal-surface-owner-chunk-keys surface)
          (terminal-sort-chunk-keys owner-keys)
          (terminal-surface-dependency-keys surface)
          (terminal-sort-chunk-keys dependency-keys)
          (terminal-surface-retained-dependency-stamp surface)
          (terminal-surface-dependency-stamp surface)
          (terminal-surface-observed-world-revision surface)
          (block-world-revision world)
          (terminal-surface-state surface) :current)
    surface))

(defun terminal-surface-visible-mesh-current-p (surface session)
  "Whether each native mesh carrying SURFACE is a current visible product."
  (let ((products (luvcraft-session-chunk-products session)))
    (loop for key in (terminal-surface-owner-chunk-keys surface)
          for product = (gethash key products)
          always (and product
                      (current-luvcraft-chunk-product-p
                       session key product)))))

(defun reconcile-terminal-surface (surface &optional session)
  "Reconcile derived SURFACE state after its native mesh publication boundary.

An authored edit does not immediately alter the retained display.  When the
surface's visible chunk products are still stale, the old geometry and glyph
projection remain one last-known-good cohort."
  (let* ((world (terminal-surface-world surface))
         (world-revision (block-world-revision world)))
    (unless (= world-revision
               (terminal-surface-observed-world-revision surface))
      (let ((stamp (terminal-surface-dependency-stamp surface)))
        (cond
          ((equal stamp
                  (terminal-surface-retained-dependency-stamp surface))
           ;; The edit was outside the chunks this surface observes.
           (setf (terminal-surface-observed-world-revision surface)
                 world-revision))
          ((and session
                (not (terminal-surface-visible-mesh-current-p
                      surface session)))
           ;; Keep both halves of the old visible cohort until native mesh
           ;; publication catches up.  Revisit on the next frame.
           nil)
          (t
           (setf (terminal-surface-retained-dependency-stamp surface) stamp
                 (terminal-surface-observed-world-revision surface)
                 world-revision
                 (terminal-surface-state surface)
                 (if (terminal-surface-shape-current-p surface)
                     :current
                     :invalid))
           (incf (terminal-surface-reconciliations surface))))))
    (terminal-surface-state surface)))

(defmethod terminal-surface-current-p
    ((surface terminal-surface) &optional session)
  (eq :current (reconcile-terminal-surface surface session)))

(defun terminal-surface-axis-extent (surface direction)
  (let ((extent
          (voxel-space-cell-extent
           (block-world-space (terminal-surface-world surface)))))
    (+ (* (abs (voxel-direction-dx direction)) (vec3-x extent))
       (* (abs (voxel-direction-dy direction)) (vec3-y extent))
       (* (abs (voxel-direction-dz direction)) (vec3-z extent)))))

(defmethod terminal-surface-physical-width ((surface terminal-surface))
  (let ((frame (terminal-face-frame (terminal-surface-face surface))))
    (* (terminal-surface-width surface)
       (terminal-surface-axis-extent
        surface (terminal-face-frame-right frame)))))

(defmethod terminal-surface-physical-height ((surface terminal-surface))
  (let ((frame (terminal-face-frame (terminal-surface-face surface))))
    (* (terminal-surface-height surface)
       (terminal-surface-axis-extent
        surface (terminal-face-frame-up frame)))))

(defun terminal-film-rectangle (surface film-aspect)
  "Fit FILM-ASPECT inside SURFACE and return its lower-left and edge vectors."
  (let* ((frame (terminal-face-frame (terminal-surface-face surface)))
         (right (voxel-direction-vec3 (terminal-face-frame-right frame)))
         (up (voxel-direction-vec3 (terminal-face-frame-up frame)))
         (surface-width (terminal-surface-physical-width surface))
         (surface-height (terminal-surface-physical-height surface))
         (surface-aspect (/ surface-width surface-height))
         (width (if (> film-aspect surface-aspect)
                    surface-width
                    (* surface-height film-aspect)))
         (height (if (> film-aspect surface-aspect)
                     (/ surface-width film-aspect)
                     surface-height))
         (left-margin (/ (- surface-width width) 2.0))
         (bottom-margin (/ (- surface-height height) 2.0))
         (origin
           (terminal-offset-point
            (terminal-surface-lower-left-point surface 0.009)
            right left-margin up bottom-margin)))
    (values origin (vec3-scale right width) (vec3-scale up height))))

(defun stop-terminal-display-film (display session)
  "Stop and release DISPLAY's owned movie, if any."
  (alexandria:when-let ((screen (terminal-display-film-screen display)))
    (when (eq screen (luvcraft-session-video-screen session))
      (setf (luvcraft-session-video-screen session) nil))
    (setf (terminal-display-film-screen display) nil)
    (release-video-screen screen))
  display)

(defun play-terminal-display-film (display pathname &key (hardware :required))
  "Play PATHNAME on DISPLAY's authored wall using the session's video backend.

HARDWARE is the decode policy MAKE-VIDEO-SCREEN takes.  It defaults to
:REQUIRED, which is right for authored films whose codec is known, and wrong
for a film that arrived from somewhere: whatever a stranger's phone recorded
is not necessarily something this device can decode in hardware, and :AUTO
lets it fall back to software rather than refusing to play."
  (let* ((session (terminal-display-session display))
         (surface (terminal-display-surface display)))
    (unless session
      (error "Terminal display ~S is not attached to a session." display))
    (stop-terminal-display-film display session)
    (let ((screen
            (make-video-screen
             (luvcraft-session-device session)
             (luvcraft-session-camera session)
             pathname +luvcraft-scene-color-format+
             :hardware hardware
             :rectangle
             (lambda (aspect)
               (terminal-film-rectangle surface aspect)))))
      (setf (terminal-display-film-screen display) screen
            (luvcraft-session-video-screen session) screen
            (terminal-display-mode display) :film)
      screen)))

(defmethod terminal-surface-lower-left-point
    ((surface terminal-surface) &optional (offset 0.006))
  "Return the metric world point OFFSET cell-depths outside the lower left."
  (let* ((origin (terminal-surface-origin surface))
         (space (block-world-space (terminal-surface-world surface)))
         (cell-origin (world-coordinate-cell-origin space origin))
         (extent (voxel-space-cell-extent space))
         (frame (terminal-face-frame (terminal-surface-face surface)))
         (right (terminal-face-frame-right frame))
         (up (terminal-face-frame-up frame))
         (outward (terminal-face-frame-outward frame)))
    (labels ((component (point-reader direction-reader)
               (let ((base (funcall point-reader cell-origin))
                     (cell-size (funcall point-reader extent))
                     (normal (funcall direction-reader outward))
                     (horizontal (funcall direction-reader right))
                     (vertical (funcall direction-reader up)))
                 (+ base
                    (* cell-size
                       (+ (cond ((plusp normal) 1.0)
                                ((minusp normal) 0.0)
                                ((minusp horizontal) 1.0)
                                ((minusp vertical) 1.0)
                                (t 0.0))
                          (* normal offset)))))))
      (make-vec3
       (component #'vec3-x #'voxel-direction-dx)
       (component #'vec3-y #'voxel-direction-dy)
       (component #'vec3-z #'voxel-direction-dz)))))

(defun place-terminal-block-rectangle
    (world origin-x origin-y origin-z face width height
     &key (material *terminal-block*))
  "Place a WIDTH by HEIGHT terminal-material rectangle in resident WORLD.

ORIGIN is the lower-left block as seen from FACE's outward side."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (let* ((origin (make-world-coordinate origin-x origin-y origin-z))
         (frame (terminal-face-frame face)))
    (with-world-change-transaction (world)
      (dotimes (row height)
        (dotimes (column width)
          (let ((coordinate
                  (terminal-coordinate-step
                   (terminal-coordinate-step
                    origin (terminal-face-frame-right frame) column)
                   (terminal-face-frame-up frame) row)))
            (edit-block-at material world
                           (world-coordinate-x coordinate)
                           (world-coordinate-y coordinate)
                           (world-coordinate-z coordinate)))))))
  world)

(defun fit-terminal-grid-in-surface
    (domain surface font-advance font-height margin font-scale)
  "Fit DOMAIN uniformly inside SURFACE and return scale, left, bottom, width,
and height in world units.  FONT-SCALE is an explicit multiplier on contain."
  (check-type margin (real 0))
  (check-type font-scale (real (0)))
  (let* ((surface-width (terminal-surface-physical-width surface))
         (surface-height (terminal-surface-physical-height surface))
         (available-width (- surface-width (* 2 margin)))
         (available-height (- surface-height (* 2 margin)))
         (columns (terminal-grid-domain-columns domain))
         (rows (terminal-grid-domain-rows domain)))
    (unless (and (plusp available-width) (plusp available-height))
      (error "Margin ~S leaves no room on a ~,3Fx~,3F terminal surface."
             margin surface-width surface-height))
    (let* ((scale
             (* font-scale
                (min (/ available-width (* columns font-advance))
                     (/ available-height (* rows font-height)))))
           (grid-width (* columns font-advance scale))
           (grid-height (* rows font-height scale)))
      (values scale
              (/ (- surface-width grid-width) 2.0)
              (/ (- surface-height grid-height) 2.0)
              grid-width grid-height))))

(defstruct (terminal-face (:constructor make-terminal-face (pathname loader)))
  "One font file and its open loader."
  pathname
  loader)

(defun terminal-display-character-glyphs
    (character bold-p glyphs-by-character glyph-cache face)
  (let ((key (cons character bold-p)))
    (multiple-value-bind (cached-glyphs present-p)
        (gethash key glyphs-by-character)
      (if present-p
          cached-glyphs
          (setf (gethash key glyphs-by-character)
                (luv.slug:make-slug-glyph-placements
                 (luv.slug:cached-slug-shaped-text
                  glyph-cache (terminal-face-pathname face) (string character))
                 (terminal-face-loader face) glyph-cache
                 (terminal-face-pathname face)))))))

(defun terminal-display-glyph-occurrences
    (presentation glyphs-by-character glyph-cache regular bold)
  (let ((occurrences nil)
        (domain (terminal-grid-presentation-domain presentation)))
    (dotimes (row (terminal-grid-domain-rows domain))
      (dotimes (column (terminal-grid-domain-columns domain))
        (let ((character
                (terminal-grid-character presentation column row)))
          (unless (char= character #\Space)
            (multiple-value-bind (foreground background bold-p)
                (terminal-presentation-cell-style presentation column row)
              (declare (ignore background))
              (dolist (glyph
                        (terminal-display-character-glyphs
                         character bold-p glyphs-by-character glyph-cache
                         (if bold-p bold regular)))
                (push (make-terminal-glyph-occurrence
                       :glyph glyph :column column :row row
                       :foreground foreground :bold-p bold-p)
                      occurrences)))))))
    (nreverse occurrences)))

(defun srgb-byte-to-linear (byte)
  (let ((value (/ byte 255.0)))
    (if (<= value 0.04045)
        (/ value 12.92)
        (expt (/ (+ value 0.055) 1.055) 2.4))))

(defun packed-color-linear-components (packed)
  "Return linear R G B floats for one packed sRGB #xRRGGBB."
  (values (srgb-byte-to-linear (ldb (byte 8 16) packed))
          (srgb-byte-to-linear (ldb (byte 8 8) packed))
          (srgb-byte-to-linear (ldb (byte 8 0) packed))))

(defun scaled-color-linear-components (packed emission)
  "Return one packed sRGB display colour as EMISSION units of scene radiance."
  (multiple-value-bind (red green blue) (packed-color-linear-components packed)
    (values (* red emission) (* green emission) (* blue emission))))

(defun terminal-font-metrics (font-loader)
  "Return the em-normalized line height, advance, and descender."
  (let* ((ascender (zpb-ttf:ascender font-loader))
         (descender (zpb-ttf:descender font-loader))
         (units-per-em (zpb-ttf:units/em font-loader)))
    (values (/ (- ascender descender) units-per-em)
            (/ (zpb-ttf:advance-width
                (zpb-ttf:find-glyph #\M font-loader))
               units-per-em)
            (/ descender units-per-em))))

(defun make-terminal-display-glyph-instances
    (occurrences atlas domain surface font-loader margin font-scale)
  "Place glyphs in one font grid fitted across the entire block SURFACE."
  (multiple-value-bind (font-height font-advance descender-ratio)
      (terminal-font-metrics font-loader)
    (multiple-value-bind (right up) (terminal-surface-axes surface)
     (let* ((origin (terminal-surface-lower-left-point surface))
           ;; The vertex stage dilates each quad by pixels; this is only
           ;; the static em padding, normally zero.
           (padding luv.slug:*slug-static-padding*)
           (data (make-array (* 24 (length occurrences))
                             :element-type 'single-float)))
      (multiple-value-bind (scale left bottom grid-width grid-height)
          (fit-terminal-grid-in-surface
           domain surface font-advance font-height margin font-scale)
        (declare (ignore grid-width))
        (labels ((difference (end start)
                   (make-vec3 (- (vec3-x end) (vec3-x start))
                              (- (vec3-y end) (vec3-y start))
                              (- (vec3-z end) (vec3-z start))))
                 (write-values (offset values)
                   (loop for value in values
                         for index from offset
                         do (setf (aref data index)
                                  (coerce value 'single-float)))))
          (loop for occurrence in occurrences
                for base from 0 by 24
                for glyph = (terminal-glyph-occurrence-glyph occurrence)
                for column = (terminal-glyph-occurrence-column occurrence)
                for row = (terminal-glyph-occurrence-row occurrence)
                for baseline =
                  (+ bottom grid-height
                     (- (* (1+ row) font-height scale))
                     (* (- descender-ratio) scale))
                for cell-x = (+ left (* column font-advance scale))
                for outline-left =
                  (- (luv.slug:slug-glyph-placement-outline-min-x glyph) padding)
                for outline-bottom =
                  (- (luv.slug:slug-glyph-placement-outline-min-y glyph) padding)
                for outline-right =
                  (+ (luv.slug:slug-glyph-placement-outline-max-x glyph) padding)
                for outline-top =
                  (+ (luv.slug:slug-glyph-placement-outline-max-y glyph) padding)
                for glyph-origin =
                  (terminal-offset-point
                   origin
                   right
                   (+ cell-x
                      (* (luv.slug:slug-glyph-placement-origin-x glyph) scale))
                   up
                   (+ baseline
                      (* (luv.slug:slug-glyph-placement-origin-y glyph) scale)))
                for quad-origin =
                  (terminal-offset-point
                   glyph-origin right (* outline-left scale)
                   up (* outline-bottom scale))
                for right-edge =
                  (terminal-offset-point
                   glyph-origin right (* outline-right scale)
                   up (* outline-bottom scale))
                for top-edge =
                  (terminal-offset-point
                   glyph-origin right (* outline-left scale)
                   up (* outline-top scale))
                for resource = (luv.slug:slug-glyph-placement-resource glyph)
                for serialized =
                  (luv.slug:slug-device-glyph-serialized resource)
                for atlas-location =
                  (gethash resource (luv.slug:slug-glyph-atlas-locations atlas))
                do (multiple-value-bind (red green blue)
                       (scaled-color-linear-components
                        (terminal-glyph-occurrence-foreground occurrence)
                        *terminal-ink-emission*)
                     ;; The three spare Z lanes carry the linear ink colour.
                     (write-values
                      base
                      (list
                       (vec3-x quad-origin) (vec3-y quad-origin)
                       (vec3-z quad-origin)
                       (vec3-x (difference right-edge quad-origin))
                       (vec3-y (difference right-edge quad-origin))
                       (vec3-z (difference right-edge quad-origin))
                       (vec3-x (difference top-edge quad-origin))
                       (vec3-y (difference top-edge quad-origin))
                       (vec3-z (difference top-edge quad-origin))
                       outline-left outline-bottom
                       (luv.slug:slug-serialized-outline-horizontal-band-count
                        serialized)
                       outline-right outline-top
                       (luv.slug:slug-serialized-outline-vertical-band-count
                        serialized)
                       (first atlas-location) (second atlas-location) red
                       (luv.slug:slug-glyph-placement-outline-min-x glyph)
                       (luv.slug:slug-glyph-placement-outline-min-y glyph) green
                       (luv.slug:slug-glyph-placement-outline-max-x glyph)
                       (luv.slug:slug-glyph-placement-outline-max-y glyph)
                       blue))))
          data))))))

(defun make-terminal-display-cell-instances
    (presentation surface font-loader margin font-scale)
  "Build one background record per same-coloured run of cells in a row.

Each record is origin, right edge, up edge, and linear RGB: 12 floats.  The
edges span the exact painted rectangle; the shader pads the quad itself and
resolves the analytic edge, so adjacent runs meet without seams."
  (multiple-value-bind (font-height font-advance) (terminal-font-metrics font-loader)
    (multiple-value-bind (right up) (terminal-surface-axes surface)
      (let* ((domain (terminal-grid-presentation-domain presentation))
             ;; Sit just below the glyph plane so ink always wins the depth test.
             (origin (terminal-surface-lower-left-point surface 0.004))
             (values nil)
             (count 0))
        (multiple-value-bind (scale left bottom grid-width grid-height)
            (fit-terminal-grid-in-surface
             domain surface font-advance font-height margin font-scale)
          (declare (ignore grid-width))
          (let ((cell-width (* font-advance scale))
                (cell-height (* font-height scale)))
            (flet ((emit (row start end background)
                     (let ((corner
                             (terminal-offset-point
                              origin
                              right (+ left (* start cell-width))
                              up (+ bottom grid-height
                                    (- (* (1+ row) cell-height)))))
                           (width (* (- end start) cell-width)))
                       (multiple-value-bind (red green blue)
                           (scaled-color-linear-components
                            background *terminal-background-emission*)
                         (incf count)
                         (push (list (vec3-x corner) (vec3-y corner) (vec3-z corner)
                                     (* (vec3-x right) width)
                                     (* (vec3-y right) width)
                                     (* (vec3-z right) width)
                                     (* (vec3-x up) cell-height)
                                     (* (vec3-y up) cell-height)
                                     (* (vec3-z up) cell-height)
                                     red green blue)
                               values)))))
              (dotimes (row (terminal-grid-domain-rows domain))
                (let ((run-start nil) (run-color nil))
                  (dotimes (column (terminal-grid-domain-columns domain))
                    (multiple-value-bind (foreground background)
                        (terminal-presentation-cell-style presentation column row)
                      (declare (ignore foreground))
                      (unless (eql background run-color)
                        (when run-color (emit row run-start column run-color))
                        (setf run-start column run-color background))))
                  (when run-color
                    (emit row run-start (terminal-grid-domain-columns domain)
                          run-color)))))))
        (let ((data (make-array (* 12 count) :element-type 'single-float))
              (index 0))
          (dolist (record (nreverse values))
            (dolist (value record)
              (setf (aref data index) (coerce value 'single-float))
              (incf index)))
          data)))))

(defun make-terminal-display-screen-instances (surface &optional (offset 0.003))
  "Build the one screen-panel record spanning the whole display surface.

The record shares the cell run's layout: origin, right edge, up edge, then
the outward face normal in the lane the cell run uses for ink.  OFFSET names
which of the terminal's coplanar rectangles this is: the default sits just
below the cell backgrounds, so both they and the glyphs win the depth test
against it while it hides the block tiles behind the screen, and the
faceplate's larger offset sits in front of the glyphs instead."
  (multiple-value-bind (right up outward) (terminal-surface-axes surface)
    (let* ((origin (terminal-surface-lower-left-point surface offset))
           (width (terminal-surface-physical-width surface))
           (height (terminal-surface-physical-height surface))
           (data (make-array 12 :element-type 'single-float))
           (values (list (vec3-x origin) (vec3-y origin) (vec3-z origin)
                         (* (vec3-x right) width)
                         (* (vec3-y right) width)
                         (* (vec3-z right) width)
                         (* (vec3-x up) height)
                         (* (vec3-y up) height)
                         (* (vec3-z up) height)
                         (vec3-x outward) (vec3-y outward) (vec3-z outward))))
      (loop for value in values
            for index from 0
            do (setf (aref data index) (coerce value 'single-float)))
      data)))

(defun make-terminal-display-glyph-population
    (presentation surface glyph-cache glyphs-by-character
     font-pathname bold-font-pathname margin font-scale)
  "Return glyphs, atlas, glyph instances, and background cell instances."
  (zpb-ttf:with-font-loader (font-loader font-pathname)
    (zpb-ttf:with-font-loader (bold-loader (or bold-font-pathname font-pathname))
      (let ((regular (make-terminal-face font-pathname font-loader))
            (bold (make-terminal-face (or bold-font-pathname font-pathname)
                                      bold-loader)))
        ;; Keep ordinary shell interaction on one stable atlas.  Unicode
        ;; output grows this retained character set only when a genuinely new
        ;; character first appears, rather than manufacturing an atlas for
        ;; every screen.
        (loop for code from 32 to 126
              do (terminal-display-character-glyphs
                  (code-char code) nil glyphs-by-character glyph-cache regular)
                 (terminal-display-character-glyphs
                  (code-char code) t glyphs-by-character glyph-cache bold))
        (let* ((occurrences
                 (terminal-display-glyph-occurrences
                  presentation glyphs-by-character glyph-cache regular bold))
               (glyphs (mapcar #'terminal-glyph-occurrence-glyph occurrences))
               (atlas-keys
                 (sort (loop for key being the hash-keys of glyphs-by-character
                             collect key)
                       (lambda (a b)
                         (or (< (char-code (car a)) (char-code (car b)))
                             (and (char= (car a) (car b))
                                  (not (cdr a)) (cdr b))))))
               (atlas-glyphs
                 (loop for key in atlas-keys
                       append (copy-list (gethash key glyphs-by-character))))
               (atlas (luv.slug:slug-glyph-atlas-for glyph-cache atlas-glyphs))
               (instances
                 (make-terminal-display-glyph-instances
                  occurrences atlas
                  (terminal-grid-presentation-domain presentation)
                  surface font-loader margin font-scale))
               (cell-instances
                 (make-terminal-display-cell-instances
                  presentation surface font-loader margin font-scale)))
          (values glyphs atlas instances cell-instances))))))

(defclass terminal-cell-run ()
  ((pipeline :initarg :pipeline :reader terminal-cell-run-pipeline)
   (vertex-buffer :initarg :vertex-buffer :reader terminal-cell-run-vertex-buffer)
   (instance-buffer :initarg :instance-buffer
                    :accessor terminal-cell-run-instance-buffer)
   (count :initarg :count :initform 0 :accessor terminal-cell-run-count))
  (:documentation
   "Solid background quads behind terminal cells, sharing the glyph run's
frame bind group so both draw against the same camera uniform."))

(defun make-terminal-cell-run (session glyph-run instances
                               &key (role :terminal-cell)
                                    (vertex-role role)
                                    (label "terminal cell backgrounds"))
  "Build an analytic world-rectangle run of ROLE over 48-byte instances.

The default ROLE draws cell backgrounds; :TERMINAL-SCREEN draws the one
screen panel from MAKE-TERMINAL-DISPLAY-SCREEN-INSTANCES with the same
vertex layout, and :TERMINAL-FACEPLATE draws the glass over the finished
picture from that same rectangle, which is why VERTEX-ROLE is separable:
two materials can share one placement stage without sharing a name."
  (let* ((device (luvcraft-session-device session))
         (vertex-data (make-world-text-quad-vertices))
         (vertex-buffer nil)
         (instance-buffer nil)
         (pipeline nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf vertex-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "terminal cell quads"
                          :size (* 4 (length vertex-data))
                          :usage '(:vertex :copy-dst)))
                 instance-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "terminal cell instances"
                          :size (max 4 (* 4 (length instances)))
                          :usage '(:vertex :copy-dst)))
                 pipeline
                 (make-live-shader-pipeline
                  :role role
                  :vertex-role vertex-role
                  :label label
                  :device device
                  :layout (world-text-run-layout glyph-run)
                  :vertex-buffers
                  '((:array-stride 12
                     :attributes
                     ((:shader-location 0 :offset 0 :format :float32x3)))
                    (:array-stride 48 :step-mode :instance
                     :attributes
                     ((:shader-location 1 :offset 0 :format :float32x3)
                      (:shader-location 2 :offset 12 :format :float32x3)
                      (:shader-location 3 :offset 24 :format :float32x3)
                      (:shader-location 4 :offset 36 :format :float32x3))))
                  :target-format +luvcraft-scene-color-format+
                  :target-blend :premultiplied-alpha
                  :primitive '(:topology :triangle-list)
                  :depth-stencil
                  '(:format :depth32-float
                    :depth-write-enabled nil
                    :depth-compare :less)))
           (write-buffer vertex-buffer vertex-data)
           (when (plusp (length instances))
             (write-buffer instance-buffer instances))
           (setf completed-p t)
           (make-instance 'terminal-cell-run
                          :pipeline pipeline :vertex-buffer vertex-buffer
                          :instance-buffer instance-buffer
                          :count (floor (length instances) 12)))
      (unless completed-p
        (when pipeline (ignore-errors (release-live-shader-pipeline pipeline)))
        (when instance-buffer (ignore-errors (destroy instance-buffer)))
        (when vertex-buffer (ignore-errors (destroy vertex-buffer)))))))

(defun replace-terminal-cell-run-instances (run device instances)
  (let ((buffer (create device
                        (make-buffer-descriptor
                         :label "terminal cell instances"
                         :size (max 4 (* 4 (length instances)))
                         :usage '(:vertex :copy-dst)))))
    (when (plusp (length instances))
      (write-buffer buffer instances))
    (let ((old (terminal-cell-run-instance-buffer run)))
      (setf (terminal-cell-run-instance-buffer run) buffer
            (terminal-cell-run-count run) (floor (length instances) 12))
      (destroy old))
    run))

(defun release-terminal-cell-run (run)
  (release-live-shader-pipeline (terminal-cell-run-pipeline run))
  (destroy (terminal-cell-run-instance-buffer run))
  (destroy (terminal-cell-run-vertex-buffer run))
  (values))

(defun make-terminal-display-glyph-run
    (session presentation surface glyph-cache glyphs-by-character
     font-pathname bold-font-pathname margin font-scale)
  "Return the glyph run and its companion background cell run."
  (multiple-value-bind (glyphs atlas instances cell-instances)
      (make-terminal-display-glyph-population
       presentation surface glyph-cache glyphs-by-character
       font-pathname bold-font-pathname margin font-scale)
    (let* ((lower-left (terminal-surface-lower-left-point surface))
           (center
             (multiple-value-bind (right up) (terminal-surface-axes surface)
               (terminal-offset-point
                lower-left
                right (/ (terminal-surface-physical-width surface) 2.0)
                up (/ (terminal-surface-physical-height surface) 2.0)))))
      (let ((glyph-run
              (make-world-text-run-from-instances
               (luvcraft-session-device session)
               +luvcraft-scene-color-format+
               "terminal display" font-pathname nil glyphs atlas center 1.0
               instances
               :label "terminal surface Slug glyphs")))
        (values glyph-run
                (make-terminal-cell-run session glyph-run cell-instances))))))

(defun clear-terminal-display-frame-bind-groups (display)
  (maphash (lambda (frame group)
             (declare (ignore frame))
             (destroy group))
           (terminal-display-frame-bind-groups display))
  (clrhash (terminal-display-frame-bind-groups display))
  display)

(defmethod luvcraft-overlay-live-shader-pipelines ((display terminal-display))
  (list* (world-text-run-pipeline (terminal-display-glyph-run display))
         (append
          (loop for run in (list (terminal-display-cell-run display)
                                 (terminal-display-screen-run display)
                                 (terminal-display-faceplate-run display))
                when run collect (terminal-cell-run-pipeline run))
          (alexandria:when-let ((portal (terminal-display-portal display)))
            (luvcraft-overlay-live-shader-pipelines portal)))))

(zdefmethod (refresh-luvcraft-overlay :zone :terminal/refresh)
    ((display terminal-display) (session luvcraft-session))
  (alexandria:when-let ((overlay (terminal-display-delegate-overlay display)))
    (refresh-luvcraft-overlay overlay session))
  ;; The wall's shaders are as live as the block world's: a redefined
  ;; :terminal-screen or :terminal-cell method rebuilds here, at the frame
  ;; boundary, keeping the last good pipeline on failure.
  (dolist (pipeline (luvcraft-overlay-live-shader-pipelines display))
    (refresh-live-shader-pipeline pipeline))
  (when (terminal-display-dirty-p display)
    ;; Clearing before the snapshot preserves an output notification which
    ;; races after this point; that later notification requests another frame
    ;; publication instead of being lost.
    (setf (terminal-display-dirty-p display) nil)
    (let ((completed-p nil))
      (unwind-protect
           (let* ((device (terminal-display-device display))
                  (screen
                    (if device
                        (termdev:call-with-pty-device-terminal
                         device #'ghostty:terminal-screen)
                        (ghostty:terminal-screen
                         (terminal-display-terminal display))))
                  (old-presentation (terminal-display-presentation display))
                  (presentation
                    (progn
                      ;; Unstyled cells take the wall's own ink colour.
                      (setf (ghostty:terminal-screen-default-foreground screen)
                            (terminal-display-default-foreground display))
                      (make-terminal-screen-presentation
                       (terminal-grid-presentation-domain old-presentation)
                       screen)))
                  (text (ghostty:terminal-screen-text screen)))
             (multiple-value-bind (glyphs atlas instances cell-instances)
                 (make-terminal-display-glyph-population
                  presentation (terminal-display-surface display)
                  (terminal-display-glyph-cache display)
                  (terminal-display-glyphs-by-character display)
                  (terminal-display-font-pathname display)
                  (terminal-display-bold-font-pathname display)
                  (terminal-display-margin display)
                  (terminal-display-font-scale display))
               (multiple-value-bind (run atlas-changed-p)
                   (replace-world-text-run-instances
                    (terminal-display-glyph-run display)
                    (luvcraft-session-device session)
                    text glyphs atlas instances)
                 (declare (ignore run))
                 (when (terminal-display-cell-run display)
                   (replace-terminal-cell-run-instances
                    (terminal-display-cell-run display)
                    (luvcraft-session-device session)
                    cell-instances))
                 (when atlas-changed-p
                   (clear-terminal-display-frame-bind-groups display))
                 (setf (terminal-display-presentation display) presentation)
                 (incf (terminal-display-refresh-count display))
                 (setf completed-p t))))
        (unless completed-p
          (setf (terminal-display-dirty-p display) t)))))
  display)

(defgeneric terminal-display-frame-uniform-buffer (display frame)
  (:documentation
   "The frame uniform buffer DISPLAY's runs draw against in FRAME.

A wall draws in world coordinates against the session's own frame uniform.
A display whose surface lives in some other space -- a phone in the hand --
supplies a uniform whose camera is expressed in that space instead."))

(defmethod terminal-display-frame-uniform-buffer
    ((display terminal-display) frame)
  (luvcraft-frame-uniform-buffer frame))

(defun terminal-display-frame-bind-group (display frame)
  (or (gethash frame (terminal-display-frame-bind-groups display))
      (let ((group
              (aref
               (make-world-text-frame-bind-groups
                (terminal-display-glyph-run display)
                (luvcraft-session-device (terminal-display-session display))
                (terminal-display-frame-uniform-buffer display frame))
               0)))
        (setf (gethash frame
                       (terminal-display-frame-bind-groups display))
              group)
        group)))

(zdefmethod (encode-luvcraft-overlay :zone :terminal/encode)
    ((display terminal-display) session pass surface-texture)
  (when (terminal-surface-current-p
         (terminal-display-surface display) session)
    (let ((frame (luvcraft-frame-state session surface-texture))
          (faceplate-run (terminal-display-faceplate-run display)))
      (case (terminal-display-mode display)
        (:shell
         (let ((glyph-run (terminal-display-glyph-run display))
               (cell-run (terminal-display-cell-run display))
               (screen-run (terminal-display-screen-run display)))
           (when (and screen-run (plusp (terminal-cell-run-count screen-run)))
             (set-pipeline pass (live-shader-pipeline-native-pipeline
                                 (terminal-cell-run-pipeline screen-run)))
             (set-vertex-buffer pass 0
                                (terminal-cell-run-vertex-buffer screen-run))
             (set-vertex-buffer pass 1
                                (terminal-cell-run-instance-buffer screen-run))
             (set-bind-group pass 0
                             (terminal-display-frame-bind-group display frame))
             (draw pass 6 (terminal-cell-run-count screen-run)))
           (when (and cell-run (plusp (terminal-cell-run-count cell-run)))
             (set-pipeline pass (live-shader-pipeline-native-pipeline
                                 (terminal-cell-run-pipeline cell-run)))
             (set-vertex-buffer pass 0
                                (terminal-cell-run-vertex-buffer cell-run))
             (set-vertex-buffer pass 1
                                (terminal-cell-run-instance-buffer cell-run))
             (set-bind-group pass 0
                             (terminal-display-frame-bind-group display frame))
             (draw pass 6 (terminal-cell-run-count cell-run)))
           (when (and glyph-run
                      (plusp (length (world-text-run-glyphs glyph-run))))
             (set-pipeline pass (world-text-run-native-pipeline glyph-run))
             (set-vertex-buffer pass 0 (world-text-run-vertex-buffer glyph-run))
             (set-vertex-buffer pass 1
                                (world-text-run-instance-buffer glyph-run))
             (set-bind-group pass 0
                             (terminal-display-frame-bind-group display frame))
             (draw pass 6 (length (world-text-run-glyphs glyph-run))))))
        (:portal
         ;; A child game's picture, under this wall's glass like the text.
         (alexandria:when-let ((portal (terminal-display-portal display)))
           (encode-luvcraft-portal-picture portal session pass surface-texture)))
        (t
         ;; Film, Telegram, and other presentation-layer modes all draw
         ;; through the mode's own overlay.
         (alexandria:when-let
             ((overlay (terminal-display-delegate-overlay display)))
           (encode-luvcraft-overlay overlay session pass surface-texture))))
      ;; The glass goes on last over shell, browser, or movie: raster,
      ;; corners, and reflections belong in front of the finished picture.
      (when (and faceplate-run (plusp (terminal-cell-run-count faceplate-run)))
        (set-pipeline pass (live-shader-pipeline-native-pipeline
                            (terminal-cell-run-pipeline faceplate-run)))
        (set-vertex-buffer pass 0
                           (terminal-cell-run-vertex-buffer faceplate-run))
        (set-vertex-buffer pass 1
                           (terminal-cell-run-instance-buffer faceplate-run))
        (set-bind-group pass 0
                        (terminal-display-frame-bind-group display frame))
        (draw pass 6 (terminal-cell-run-count faceplate-run)))))
  display)

(defmethod release-luvcraft-overlay ((display terminal-display))
  (alexandria:when-let ((session (terminal-display-session display)))
    (stop-terminal-display-film display session)
    (close-terminal-display-portal display)
    #+darwin
    (unregister-luvcraft-portal-screen session (terminal-display-name display)))
  (alexandria:when-let ((overlay (terminal-display-mode-overlay display)))
    (setf (terminal-display-mode-overlay display) nil)
    (release-luvcraft-overlay overlay))
  (when (terminal-display-device display)
    (termdev:close-pty-device (terminal-display-device display))
    (setf (terminal-display-device display) nil))
  (clear-terminal-display-frame-bind-groups display)
  (when (terminal-display-faceplate-run display)
    (release-terminal-cell-run (terminal-display-faceplate-run display))
    (setf (terminal-display-faceplate-run display) nil))
  (when (terminal-display-screen-run display)
    (release-terminal-cell-run (terminal-display-screen-run display))
    (setf (terminal-display-screen-run display) nil))
  (when (terminal-display-cell-run display)
    (release-terminal-cell-run (terminal-display-cell-run display))
    (setf (terminal-display-cell-run display) nil))
  (release-world-text-run (terminal-display-glyph-run display))
  (luv.slug:release-slug-glyph-cache
   (terminal-display-glyph-cache display))
  (ghostty:close-terminal (terminal-display-terminal display))
  display)

(defmethod handle-luvcraft-focus-event
    ((display terminal-display) session canvas (event canvas-key-event))
  ;; Keys still reach the shell under a portal: that is how the child gets
  ;; its Ctrl-C.
  (if (member (terminal-display-mode display) '(:shell :portal))
      (let ((device (terminal-display-device display)))
        (when device
          (termdev:send-pty-device-canvas-key-event device event))
        t)
      (let ((overlay (terminal-display-delegate-overlay display)))
        (if overlay
            (handle-luvcraft-focus-event overlay session canvas event)
            t))))

(defmethod handle-luvcraft-focus-event
    ((display terminal-display) session canvas (event canvas-event))
  (or (alexandria:when-let
          ((overlay (terminal-display-delegate-overlay display)))
        (handle-luvcraft-overlay-event overlay session canvas event))
      (handle-luvcraft-focus-control-event display session canvas event)))

(defmethod luvcraft-focus-score ((display terminal-display) session)
  (terminal-surface-focus-score (terminal-display-surface display) session))

;;; A screen framed exactly square-on stops being a thing in the world and
;;; becomes a pasted screenshot: nothing in the picture reports that the
;;; terminal has a plane, a bezel with depth, or a room around it.  A few
;;; degrees of azimuth and elevation give the frame back its perspective and
;;; let the faceplate's reflections and rim curvature actually appear, while
;;; staying far short of the angle at which a glyph's stem starts to matter.

(defparameter *terminal-focus-azimuth* 9.0
  "Degrees the focused view stands to the side of the screen's own normal.")

(defparameter *terminal-focus-elevation* 4.5
  "Degrees the focused view stands above the screen's own centre.")

;;; ---------------------------------------------------------------------
;;; The terminal's knobs.
;;;
;;; The emissions and the font scale are baked into each display's glyph
;;; instances, so a display must repopulate to show a change: that is the
;;; TERMINAL-REALIZATION, and the knobs carrying it are TERMINAL-KNOBs.

(defun terminal-displays (session)
  "Every terminal display among SESSION's overlays."
  (remove-if-not (lambda (overlay) (typep overlay 'terminal-display))
                 (luvcraft-session-overlays session)))

(defun mark-terminal-displays-dirty (session)
  "Ask every terminal display in SESSION to rebuild its glyphs next frame."
  (dolist (display (terminal-displays session))
    (setf (terminal-display-dirty-p display) t))
  session)

(defclass terminal-realization ()
  ()
  (:documentation
   "The value is baked into terminal glyph instances; every terminal display
must repopulate."))

(defmethod realize-knob progn ((knob terminal-realization) session)
  (mark-terminal-displays-dirty session))

(defclass terminal-knob (terminal-realization scalar-knob) ())

(defun terminal-font-scale (session)
  "The font scale SESSION's displays use: the first display's, or the
default a new one would take."
  (let ((display (first (terminal-displays session))))
    (if display
        (terminal-display-font-scale display)
        *terminal-font-scale*)))

(defun (setf terminal-font-scale) (scale session)
  (setf *terminal-font-scale* scale)
  (dolist (display (terminal-displays session))
    (setf (terminal-display-font-scale display) scale))
  scale)

(define-knob terminal-ink-emission
    (:label "terminal ink" :group :terminal :class 'terminal-knob
     :quantity (:quantity :emission-gain :unit :one)
     :unit-label "×" :minimum 0.2 :maximum 6.0 :step 0.1)
    *terminal-ink-emission*)
(define-knob terminal-background-emission
    (:label "terminal background" :group :terminal :class 'terminal-knob
     :quantity (:quantity :emission-gain :unit :one)
     :unit-label "×" :minimum 0.0 :maximum 3.0 :step 0.05)
    *terminal-background-emission*)
(define-knob terminal-font-scale
    (:label "terminal font" :group :terminal :class 'terminal-knob
     :quantity (:quantity :font-scale :unit :one)
     :unit-label "×" :minimum 0.3 :maximum 2.0 :step 0.05)
    (terminal-font-scale session))
(define-knob terminal-focus-azimuth
    (:label "focus azimuth" :group :terminal
     :quantity (:quantity :angle :unit :degree)
     :minimum -30.0 :maximum 30.0 :step 0.5)
    *terminal-focus-azimuth*)
(define-knob terminal-focus-elevation
    (:label "focus elevation" :group :terminal
     :quantity (:quantity :angle :unit :degree)
     :minimum -20.0 :maximum 20.0 :step 0.5)
    *terminal-focus-elevation*)

(defun terminal-focus-eye-direction (right up outward azimuth elevation)
  "The unit direction from a screen's centre toward a viewer standing at
AZIMUTH and ELEVATION degrees off that screen's outward normal."
  (let* ((radian (coerce (/ pi 180.0) 'single-float))
         (side (* azimuth radian))
         (rise (* elevation radian)))
    (vec3-normalize
     (terminal-offset-point
      (make-vec3 0.0 0.0 0.0)
      outward (* (cos rise) (cos side))
      right (* (cos rise) (sin side))
      up (sin rise)))))

(defun terminal-focus-camera-pose
    (surface viewport-width viewport-height
     &optional (left-inset 0.0) (top-inset 0.0)
               (right-inset 0.0) (bottom-inset 0.0))
  "Return a camera pose which contains SURFACE inside the safe view.

Framing is solved rather than approximated.  For an eye at distance D from
the surface centre along EYE-DIRECTION, a corner at offset O from that centre
lands at DOT(O, camera-right) * focal / (aspect * (D - DOT(O, EYE-DIRECTION)))
across and at the matching quotient down, because the camera's own right and
up axes are perpendicular to its forward axis and so drop out of the depth.
Inverting that for each capacity gives the smallest D at which one corner is
just inside the safe view; the pose takes the largest of the four.  With the
focus angles at zero this is exactly the head-on width-and-height solution it
generalizes."
  (let* ((width (coerce viewport-width 'single-float))
         (height (coerce viewport-height 'single-float))
         (margin (* 0.06 (min width height)))
         (left-capacity (- 1.0 (/ (* 2.0 (+ left-inset margin)) width)))
         (right-capacity (- 1.0 (/ (* 2.0 (+ right-inset margin)) width)))
         (top-capacity (- 1.0 (/ (* 2.0 (+ top-inset margin)) height)))
         (bottom-capacity
           (- 1.0 (/ (* 2.0 (+ bottom-inset margin)) height)))
         (horizontal-capacity (min left-capacity right-capacity))
         (vertical-capacity (+ top-capacity bottom-capacity)))
    (unless (and (plusp horizontal-capacity) (plusp vertical-capacity))
      (error "Focus insets leave no safe viewport inside ~Dx~D."
             viewport-width viewport-height))
    (let* ((frame (terminal-face-frame (terminal-surface-face surface)))
           (right
             (voxel-direction-vec3 (terminal-face-frame-right frame)))
           (up (voxel-direction-vec3 (terminal-face-frame-up frame)))
           (outward
             (voxel-direction-vec3 (terminal-face-frame-outward frame)))
           (half-width (/ (terminal-surface-physical-width surface) 2.0))
           (half-height (/ (terminal-surface-physical-height surface) 2.0))
           (lower-left (terminal-surface-lower-left-point surface))
           (center
             (terminal-offset-point
              lower-left right half-width up half-height))
           (field-of-view +luvcraft-camera-focused-vertical-field-of-view+)
           (focal (/ (tan (/ field-of-view 2.0))))
           (aspect (/ width height))
           (eye-direction
             (terminal-focus-eye-direction
              right up outward
              *terminal-focus-azimuth* *terminal-focus-elevation*))
           (forward (vec3-scale eye-direction -1.0))
           (pitch (asin (max -1.0 (min 1.0 (vec3-y forward)))))
           (yaw
             (if (> (abs (cos pitch)) 1e-5)
                 (atan (vec3-x forward) (vec3-z forward))
                 (atan (- (vec3-z right)) (vec3-x right))))
           ;; The camera carries no roll, so its own basis follows from the
           ;; pose it is about to be given rather than from the surface.
           (camera-right (make-vec3 (cos yaw) 0.0 (- (sin yaw))))
           (camera-up (make-vec3 (- (* (sin pitch) (sin yaw)))
                                 (cos pitch)
                                 (- (* (sin pitch) (cos yaw)))))
           ;; Move the eye down the surface just enough to place the terminal
           ;; in the center of the unobscured view rather than the full frame.
           (vertical-shift
             (* half-height
                (/ (- top-capacity bottom-capacity) vertical-capacity)))
           (upward-capacity (max top-capacity 1e-3))
           (downward-capacity (max bottom-capacity 1e-3))
           (distance
             (max 1.5
                  (loop for column in (list (- half-width) half-width)
                        maximize
                        (loop for row in (list (- half-height) half-height)
                              maximize
                              (let* ((offset
                                       (terminal-offset-point
                                        (make-vec3 0.0 0.0 0.0)
                                        right column up row))
                                     (across
                                       (abs (vec3-dot offset camera-right)))
                                     (raised
                                       (+ (vec3-dot offset camera-up)
                                          vertical-shift))
                                     (across-need
                                       (/ (* across focal)
                                          (* aspect horizontal-capacity)))
                                     (raised-need
                                       (if (plusp raised)
                                           (/ (* raised focal) upward-capacity)
                                           (/ (* (- raised) focal)
                                              downward-capacity))))
                                (+ (vec3-dot offset eye-direction)
                                   (max across-need raised-need)))))))
           (position
             (terminal-offset-point
              center eye-direction distance camera-up (- vertical-shift))))
      (make-camera-pose position yaw pitch field-of-view))))

(defmethod luvcraft-focus-camera-pose
    ((display terminal-display) (session luvcraft-session))
  (terminal-surface-focus-camera-pose
   (terminal-display-surface display) session))

(defun block-ray-hit-face (hit)
  "Return the exposed block face through which HIT entered its block."
  (let ((coordinate (block-ray-hit-coordinate hit))
        (adjacent (block-ray-hit-adjacent-coordinate hit)))
    (when adjacent
      (find-if
       (lambda (face)
         (let ((neighbor (block-face-neighbor face)))
           (and (= (+ (world-coordinate-x coordinate)
                      (voxel-direction-dx neighbor))
                   (world-coordinate-x adjacent))
                (= (+ (world-coordinate-y coordinate)
                      (voxel-direction-dy neighbor))
                   (world-coordinate-y adjacent))
                (= (+ (world-coordinate-z coordinate)
                      (voxel-direction-dz neighbor))
                   (world-coordinate-z adjacent)))))
       *block-faces*))))

(defmethod activate-luvcraft-target
    ((block block-kind) (session luvcraft-session) hit)
  (activate-wall-material (block-kind-name block) session hit))

(defgeneric activate-wall-material (name session hit)
  (:documentation
   "Create and return the wall interaction for the material named NAME, or NIL.

The material vocabulary is open here the same way it is in the atlas: one
EQL method per material which answers activation, so a new display-block
material adds a method rather than growing a CASE.")
  (:method (name session hit)
    (declare (ignore name session hit))
    nil))

(defun open-activated-wall-display (session hit material attach)
  "Open the terminal display for the MATERIAL wall HIT names and ATTACH it.

ATTACH receives the fresh display and gives the wall its process; if it
fails, the display is taken down rather than left attached to nothing."
  (let* ((coordinate (block-ray-hit-coordinate hit))
         (face (block-ray-hit-face hit)))
    (when face
      (let ((display nil)
            (completed-p nil))
        (unwind-protect
             (progn
               (setf display
                     (open-terminal-display
                      session
                      (world-coordinate-x coordinate)
                      (world-coordinate-y coordinate)
                      (world-coordinate-z coordinate)
                      (block-face-name face)
                      :fixture ""
                      :material material))
               (funcall attach display)
               (setf completed-p t)
               display)
          (unless completed-p
            (when display
              (remove-luvcraft-overlay session display))))))))

(defmethod activate-wall-material
    ((name (eql :terminal)) (session luvcraft-session) hit)
  (open-activated-wall-display
   session hit *terminal-block* #'attach-terminal-display-shell))

(defun attach-terminal-display-shell (display)
  "Attach an interactive login-free bash in the checkout to DISPLAY.

The shell learns where this game's portal server listens and which wall it is
on, so a luvcraft started in it appears right here."
  (let ((environment
          (cons
           "BASH_SILENCE_DEPRECATION_WARNING=1"
           (delete-if
            (lambda (entry)
              (uiop:string-prefix-p "BASH_SILENCE_DEPRECATION_WARNING=" entry))
            (copy-list (sb-ext:posix-environ)))))
        (session (terminal-display-session display)))
    #+darwin
    (when session
      (register-luvcraft-portal-screen
       session (terminal-display-name display)
       (lambda (mirror) (open-terminal-display-portal display mirror)))
      (setf environment
            (luvcraft-portal-environment
             session (terminal-display-name display) environment)))
    (attach-terminal-display-pty
     display
     ;; The Nix dev shell supplies Bash through PATH; NixOS deliberately has
     ;; no /bin/bash.  OPEN-PTY-DEVICE resolves a bare program name against
     ;; the child environment before launching it.
     :program "bash"
     :directory (uiop:getcwd)
     :environment environment)))

(defun attach-terminal-display-pty (display &rest open-arguments)
  "Attach one owned PTY device to DISPLAY's existing Ghostty terminal."
  (check-type display terminal-display)
  (when (terminal-display-device display)
    (error "Terminal display ~S already has an input device." display))
  (let ((arguments (copy-list open-arguments)))
    (remf arguments :on-output)
    (setf (terminal-display-device display)
          (apply #'termdev:open-pty-device
                 (terminal-display-terminal display)
                 :on-output
                 (lambda (device bytes)
                   (declare (ignore device bytes))
                   (setf (terminal-display-dirty-p display) t))
                 arguments)))
  display)

(defun terminal-grid-size-for-surface
    (surface font-pathname margin rows-per-block)
  "Choose columns and rows so the font grid fills SURFACE at ROWS-PER-BLOCK.

The row count follows the wall's block height; the column count is whatever
that cell height affords across the wall's width at the font's own aspect."
  (terminal-grid-columns-for-rows
   surface font-pathname margin
   (max 1 (round (* rows-per-block (terminal-surface-height surface))))))

(defun terminal-grid-columns-for-rows (surface font-pathname margin rows)
  "Return the columns ROWS of FONT afford across SURFACE, and ROWS."
  (zpb-ttf:with-font-loader (font-loader font-pathname)
    (multiple-value-bind (font-height font-advance)
        (terminal-font-metrics font-loader)
      (let* ((available-height
               (- (terminal-surface-physical-height surface) (* 2 margin)))
             (available-width
               (- (terminal-surface-physical-width surface) (* 2 margin)))
             (cell-height (/ available-height rows))
             (cell-width (* cell-height (/ font-advance font-height)))
             (columns (max 1 (floor available-width cell-width))))
        (values columns rows)))))

(defun open-terminal-display
    (session x y z face
     &key columns rows
          (rows-per-block 6)
          (fixture (terminal-display-fixture))
          (material *terminal-block*)
          (margin 0.12)
          (font-scale 1.0)
          (font-pathname *terminal-display-font-pathname*)
          (bold-font-pathname *terminal-display-bold-font-pathname*)
          (default-foreground *terminal-display-default-foreground*))
  "Attach a deterministic Ghostty terminal to an authored block rectangle.

X,Y,Z names any block in the desired exposed FACE.  The maximal coplanar
terminal-material component must be rectangular.  Its native block meshes
remain the display body; this overlay contributes only one fitted Slug grid.
When COLUMNS or ROWS is omitted the grid is sized to the wall itself at
ROWS-PER-BLOCK rows per block height."
  (multiple-value-bind (surface status)
      (find-terminal-surface
       (luvcraft-session-world session) x y z face :material material)
    (unless surface
      (error "Cannot open terminal display at (~D ~D ~D) ~S: ~S."
             x y z face status))
    (multiple-value-bind (fitted-columns fitted-rows)
        (terminal-grid-size-for-surface
         surface font-pathname margin rows-per-block)
      (setf columns (or columns fitted-columns)
            rows (or rows fitted-rows)))
    (make-terminal-display
     session surface columns rows
     :fixture fixture :margin margin :font-scale font-scale
     :font-pathname font-pathname :bold-font-pathname bold-font-pathname
     :default-foreground default-foreground)))

(defun make-terminal-display
    (session surface columns rows
     &key (class 'terminal-display)
          (fixture (terminal-display-fixture))
          (margin 0.12)
          (font-scale 1.0)
          (font-pathname *terminal-display-font-pathname*)
          (bold-font-pathname *terminal-display-bold-font-pathname*)
          (default-foreground *terminal-display-default-foreground*)
          (screen-role :terminal-screen)
          (faceplate-role :terminal-faceplate)
          (add-p t))
  "Build a COLUMNS by ROWS Ghostty terminal display of CLASS on SURFACE.

SURFACE is anything answering the terminal-surface protocol: a wall of
blocks or a phone screen.  SCREEN-ROLE and FACEPLATE-ROLE name the shader
materials of the panel behind the text and the glass in front of it; the
defaults are the wall's tube.  The display is added to SESSION's overlays
unless ADD-P is false."
    (let ((terminal nil)
          (glyph-cache nil)
          (glyph-run nil)
          (cell-run nil)
          (screen-run nil)
          (faceplate-run nil)
          (display nil)
          (completed-p nil))
      (unwind-protect
           (progn
             (setf terminal
                   (ghostty:make-terminal :columns columns :rows rows))
             (ghostty:write-terminal terminal fixture)
             (let* ((domain
                      (make-instance 'terminal-grid-domain
                                     :columns columns :rows rows))
                    (screen (ghostty:terminal-screen terminal))
                    (presentation
                      (progn
                        (setf (ghostty:terminal-screen-default-foreground screen)
                              default-foreground)
                        (make-terminal-screen-presentation domain screen)))
                    (glyphs-by-character (make-hash-table :test #'equal)))
               (setf glyph-cache
                     (luv.slug:make-slug-glyph-cache
                      (luvcraft-session-device session)))
               (multiple-value-setq (glyph-run cell-run)
                 (make-terminal-display-glyph-run
                  session presentation surface glyph-cache
                  glyphs-by-character font-pathname bold-font-pathname
                  margin font-scale))
               (setf screen-run
                     (make-terminal-cell-run
                      session glyph-run
                      (make-terminal-display-screen-instances surface)
                      :role screen-role
                      :vertex-role :terminal-screen
                      :label "terminal screen panel"))
               (setf faceplate-run
                     (make-terminal-cell-run
                      session glyph-run
                      (make-terminal-display-screen-instances surface 0.008)
                      :role faceplate-role
                      :vertex-role :terminal-screen
                      :label "terminal faceplate glass"))
               (setf display
                     (make-instance
                      class
                      :session session :surface surface :terminal terminal
                      :presentation presentation :glyph-cache glyph-cache
                      :glyph-run glyph-run :cell-run cell-run
                      :screen-run screen-run :faceplate-run faceplate-run
                      :font-pathname font-pathname
                      :bold-font-pathname bold-font-pathname
                      :default-foreground default-foreground
                      :margin margin :font-scale font-scale))
               (setf (slot-value display 'glyphs-by-character)
                     glyphs-by-character))
             (when add-p
               (add-luvcraft-overlay session display))
             (setf completed-p t)
             display)
        (unless completed-p
          (when faceplate-run
            (ignore-errors (release-terminal-cell-run faceplate-run)))
          (when screen-run
            (ignore-errors (release-terminal-cell-run screen-run)))
          (when cell-run
            (ignore-errors (release-terminal-cell-run cell-run)))
          (when glyph-run
            (ignore-errors (release-world-text-run glyph-run)))
          (when glyph-cache
            (ignore-errors (luv.slug:release-slug-glyph-cache glyph-cache)))
          (when terminal
            (ignore-errors (ghostty:close-terminal terminal)))))))
