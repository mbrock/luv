(in-package #:luft.render)

;;; Terminal walls: a shell on a rectangle of terminal cells.
;;;
;;; Build terminal cells side by side, look at their exposed face, and press
;;; Tab.  The coplanar terminal cells around the one you look at become one
;;; screen, a bash runs under a PTY, and Ghostty's screen is drawn there in
;;; Slug glyphs by the renderer's world-text component.  While the wall has
;;; the keyboard every key goes to the shell; Shift-Tab gives it back.
;;;
;;; This is Luvcraft's terminal wall (luvcraft/terminal-wall.lisp) carried
;;; over without its films, portals, phone, and focus camera.  The grid,
;;; colours, and glyph layout follow it closely so the two read alike.

(defparameter *terminal-font-pathname*
  (asdf:system-relative-pathname "luft/render"
                                 "fonts/MonaspaceNeon-Regular.ttf"))

(defparameter *terminal-bold-font-pathname*
  (asdf:system-relative-pathname "luft/render"
                                 "fonts/MonaspaceNeon-Bold.ttf"))

(defparameter *terminal-default-foreground* #xF4EFE1
  "Packed sRGB for cells without an explicit foreground.")

;;; A shell's colours are display colours; the scene measures radiance.  These
;;; gains make lit text a little brighter than sunlit ground, which is also
;;; what lets a bright glyph reach the presentation glow.

(defparameter *terminal-ink-emission* 1.6
  "Scene radiance per unit of glyph ink.")

(defparameter *terminal-background-emission* 0.7
  "Scene radiance per unit of an explicit cell background.")

(defparameter *terminal-screen-color* '(0.010 0.013 0.016 1.0)
  "Linear RGBA of the screen behind the grid: dark glass over the slab.")

(defparameter *terminal-rows-per-cell* 5
  "Text rows per cell of wall height.")

(defparameter *terminal-margin* 0.10
  "Cells of bare screen kept around the text grid.")

(defparameter *terminal-surface-offset* 0.012
  "How far in front of the wall face the screen layers float, in cells.")

;;; Surfaces.

(defstruct (terminal-surface (:constructor %make-terminal-surface))
  "A rectangle of terminal cells sharing one exposed face direction."
  cells           ; the (x y z) cells, for checking the wall still stands
  normal          ; outward unit normal, a list
  origin          ; lower-left corner of the screen, a vec3
  right           ; unit right, as seen from outside, a vec3
  up              ; unit up, a vec3
  width           ; cells along RIGHT
  height)         ; cells along UP

(defun terminal-cell-p (scene x y z)
  "Whether X,Y,Z is a solid terminal cell of SCENE."
  (and (= 1 (inspection-cell-bit (inspection-source-solid scene) x y z))
       (eq *terminal-material-placement*
           (scene-material-placement-at
            scene (luft:make-site (scene-domain scene) x y z
                                  luft:+cell-extent+ 1)))))

(defun terminal-face-exposed-p (scene x y z normal)
  (destructuring-bind (dx dy dz) normal
    (= 0 (inspection-cell-bit (inspection-source-solid scene)
                              (+ x dx) (+ y dy) (+ z dz)))))

(defun find-terminal-surface (scene x y z normal &key (limit 4096))
  "Return the coplanar terminal rectangle containing X,Y,Z facing NORMAL.

Only walls carry screens: NORMAL must be horizontal.  The component is every
terminal cell reachable through in-plane neighbours whose NORMAL face is open
to air; its bounding rectangle becomes the screen."
  (destructuring-bind (dx dy dz) normal
    (unless (and (zerop dz) (= 1 (+ (abs dx) (abs dy))))
      (return-from find-terminal-surface nil))
    (unless (and (terminal-cell-p scene x y z)
                 (terminal-face-exposed-p scene x y z normal))
      (return-from find-terminal-surface nil))
    (let ((seen (make-hash-table :test #'equal))
          (queue (list (list x y z)))
          (cells nil)
          ;; In-plane steps: along the wall horizontally, and vertically.
          (steps (if (zerop dx)
                     '((1 0 0) (-1 0 0) (0 0 1) (0 0 -1))
                     '((0 1 0) (0 -1 0) (0 0 1) (0 0 -1)))))
      (setf (gethash (list x y z) seen) t)
      (loop while (and queue (< (length cells) limit))
            do (let ((cell (pop queue)))
                 (push cell cells)
                 (dolist (step steps)
                   (let ((next (mapcar #'+ cell step)))
                     (unless (gethash next seen)
                       (setf (gethash next seen) t)
                       (when (and (apply #'terminal-cell-p scene next)
                                  (terminal-face-exposed-p
                                   scene (first next) (second next) (third next)
                                   normal))
                         (setf queue (append queue (list next)))))))))
      (make-terminal-surface-from-cells cells normal))))

(defun make-terminal-surface-from-cells (cells normal)
  "The screen rectangle bounding CELLS on their NORMAL side."
  (destructuring-bind (dx dy dz) normal
    (declare (ignore dz))
    ;; Seen from outside, looking along -NORMAL with Z up, right is
    ;; (-NORMAL) x Z.
    (let* ((right (make-vec3 (- dy) dx 0))
           (up (make-vec3 0 0 1))
           (axis (if (zerop dx) 1 0))
           (sign (if (zerop dx) dy dx))
           (right-axis (if (zerop dx) 0 1))
           (right-sign (if (zerop dx) (- dy) dx)))
      (flet ((right-low (cell)
               ;; The right coordinate of the cell's low edge.
               (if (plusp right-sign)
                   (nth right-axis cell)
                   (- (- (nth right-axis cell)) 1))))
        (let* ((r-min (reduce #'min cells :key #'right-low))
               (r-max (1+ (reduce #'max cells :key #'right-low)))
               (z-min (reduce #'min cells :key #'third))
               (z-max (1+ (reduce #'max cells :key #'third)))
               (plane (+ (nth axis (first cells))
                         (if (plusp sign) 1 0)
                         (* sign *terminal-surface-offset*)))
               (origin
                 (let ((point (list 0.0 0.0 (float z-min))))
                   (setf (nth axis point) (float plane))
                   ;; A right coordinate T lies at axis coordinate SIGN * T.
                   (setf (nth right-axis point) (float (* right-sign r-min)))
                   (apply #'make-vec3 point))))
          (%make-terminal-surface
           :cells cells :normal normal :origin origin
           :right right :up up
           :width (- r-max r-min) :height (- z-max z-min)))))))

(defun terminal-surface-standing-p (scene surface)
  "Whether every cell of SURFACE is still an exposed terminal cell."
  (every (lambda (cell)
           (and (apply #'terminal-cell-p scene cell)
                (terminal-face-exposed-p
                 scene (first cell) (second cell) (third cell)
                 (terminal-surface-normal surface))))
         (terminal-surface-cells surface)))

(defun terminal-surface-point (surface across along &optional (lift 0.0))
  "The world point ACROSS right and ALONG up from SURFACE's origin."
  (let ((origin (terminal-surface-origin surface))
        (right (terminal-surface-right surface))
        (up (terminal-surface-up surface))
        (normal (terminal-surface-normal surface)))
    (make-vec3 (+ (vec3-x origin) (* across (vec3-x right))
                  (* along (vec3-x up)) (* lift (first normal)))
               (+ (vec3-y origin) (* across (vec3-y right))
                  (* along (vec3-y up)) (* lift (second normal)))
               (+ (vec3-z origin) (* across (vec3-z right))
                  (* along (vec3-z up)) (* lift (third normal))))))

;;; Fonts and colour.

(defun terminal-font-metrics (font-loader)
  "Return the em-normalized line height, advance, and descender."
  (let ((ascender (zpb-ttf:ascender font-loader))
        (descender (zpb-ttf:descender font-loader))
        (units-per-em (zpb-ttf:units/em font-loader)))
    (values (/ (- ascender descender) units-per-em)
            (/ (zpb-ttf:advance-width (zpb-ttf:find-glyph #\M font-loader))
               units-per-em)
            (/ descender units-per-em))))

(defun srgb-byte-linear (byte)
  (let ((value (/ byte 255.0)))
    (if (<= value 0.04045)
        (/ value 12.92)
        (expt (/ (+ value 0.055) 1.055) 2.4))))

(defun packed-color-radiance (packed emission)
  "One packed sRGB #xRRGGBB display colour as EMISSION units of radiance."
  (list (* emission (srgb-byte-linear (ldb (byte 8 16) packed)))
        (* emission (srgb-byte-linear (ldb (byte 8 8) packed)))
        (* emission (srgb-byte-linear (ldb (byte 8 0) packed)))))

(defun terminal-grid-size (surface font-loader)
  "Columns and rows: *TERMINAL-ROWS-PER-CELL* rows a cell of wall height,
and as many columns as that cell height affords across the wall."
  (multiple-value-bind (font-height font-advance) (terminal-font-metrics font-loader)
    (let* ((available-width (- (terminal-surface-width surface)
                               (* 2 *terminal-margin*)))
           (available-height (- (terminal-surface-height surface)
                                (* 2 *terminal-margin*)))
           (rows (max 1 (round (* *terminal-rows-per-cell*
                                  (terminal-surface-height surface)))))
           (cell-height (/ available-height rows))
           (cell-width (* cell-height (/ font-advance font-height))))
      (values (max 1 (floor available-width cell-width)) rows))))

(defun terminal-grid-placement (surface columns rows font-loader)
  "Return the em scale and the grid's left and bottom offsets on SURFACE."
  (multiple-value-bind (font-height font-advance) (terminal-font-metrics font-loader)
    (let* ((available-width (- (terminal-surface-width surface)
                               (* 2 *terminal-margin*)))
           (available-height (- (terminal-surface-height surface)
                                (* 2 *terminal-margin*)))
           (scale (min (/ available-width (* columns font-advance))
                       (/ available-height (* rows font-height)))))
      (values scale
              (/ (- (terminal-surface-width surface)
                    (* columns font-advance scale))
                 2.0)
              (/ (- (terminal-surface-height surface)
                    (* rows font-height scale))
                 2.0)))))

;;; The live terminal.

(defclass world-terminal ()
  ((surface :initarg :surface :reader world-terminal-surface)
   (columns :initarg :columns :reader world-terminal-columns)
   (rows :initarg :rows :reader world-terminal-rows)
   (terminal :initarg :terminal :reader world-terminal-terminal)
   (device :initform nil :accessor world-terminal-device)
   (glyph-cache :initarg :glyph-cache :reader world-terminal-glyph-cache)
   ;; Keyed by (CHARACTER . BOLD-P).
   (glyphs :initform (make-hash-table :test #'equal)
           :reader world-terminal-glyphs)
   (batch :initform nil :accessor world-terminal-batch)
   (dirty-p :initform t :accessor world-terminal-dirty-p)))

(defun terminal-character-glyphs (terminal character bold-p loader pathname)
  "CHARACTER's Slug glyph placements in the regular or bold face, cached."
  (let ((key (cons character bold-p))
        (table (world-terminal-glyphs terminal)))
    (multiple-value-bind (glyphs present-p) (gethash key table)
      (if present-p
          glyphs
          (setf (gethash key table)
                (luv.slug:make-slug-glyph-placements
                 (luv.slug:cached-slug-shaped-text
                  (world-terminal-glyph-cache terminal) pathname
                  (string character))
                 loader (world-terminal-glyph-cache terminal) pathname))))))

(defun open-world-terminal (device surface)
  "Start a bash on SURFACE: a Ghostty terminal fitted to it and a PTY."
  (let* ((columns nil) (rows nil))
    (zpb-ttf:with-font-loader (loader *terminal-font-pathname*)
      (multiple-value-setq (columns rows) (terminal-grid-size surface loader)))
    (let* ((ghostty (luv.ghostty:make-terminal :columns columns :rows rows))
           (terminal (make-instance
                      'world-terminal
                      :surface surface :columns columns :rows rows
                      :terminal ghostty
                      :glyph-cache (luv.slug:make-slug-glyph-cache device))))
      (setf (world-terminal-device terminal)
            (luv.terminal:open-pty-device
             ghostty
             :program (or (uiop:getenv "LUV_BASH") "bash")
             :directory (uiop:getcwd)
             :environment
             (cons "BASH_SILENCE_DEPRECATION_WARNING=1"
                   (remove-if (lambda (entry)
                                (uiop:string-prefix-p
                                 "BASH_SILENCE_DEPRECATION_WARNING=" entry))
                              (sb-ext:posix-environ)))
             :on-output (lambda (device bytes)
                          (declare (ignore device bytes))
                          (setf (world-terminal-dirty-p terminal) t))))
      terminal)))

(defun close-world-terminal (terminal)
  (when (world-terminal-device terminal)
    (ignore-errors (luv.terminal:close-pty-device (world-terminal-device terminal)))
    (setf (world-terminal-device terminal) nil))
  (when (world-terminal-batch terminal)
    (release-world-text-batch (world-terminal-batch terminal))
    (setf (world-terminal-batch terminal) nil))
  (values))

(defun terminal-screen-snapshot (terminal)
  "Ghostty's styled screen, taken under the PTY owner's lock."
  (let ((screen
          (if (world-terminal-device terminal)
              (luv.terminal:call-with-pty-device-terminal
               (world-terminal-device terminal) #'luv.ghostty:terminal-screen)
              (luv.ghostty:terminal-screen (world-terminal-terminal terminal)))))
    (setf (luv.ghostty:terminal-screen-default-foreground screen)
          *terminal-default-foreground*)
    screen))

(defun push-floats (vector &rest values)
  (dolist (value values vector)
    (vector-push-extend (coerce value 'single-float) vector)))

(defun push-panel-record (records origin right-edge up-edge color)
  (push-floats records (vec3-x origin) (vec3-y origin) (vec3-z origin) 0.0)
  (push-floats records (vec3-x right-edge) (vec3-y right-edge)
               (vec3-z right-edge) 0.0)
  (push-floats records (vec3-x up-edge) (vec3-y up-edge) (vec3-z up-edge) 0.0)
  (apply #'push-floats records color))

(defun scaled-vec3 (vector scale)
  (make-vec3 (* scale (vec3-x vector)) (* scale (vec3-y vector))
             (* scale (vec3-z vector))))

(defun terminal-records (terminal screen)
  "Return panel records, glyph records, and the atlas for SCREEN."
  (let* ((surface (world-terminal-surface terminal))
         (columns (world-terminal-columns terminal))
         (rows (world-terminal-rows terminal))
         (right (terminal-surface-right surface))
         (up (terminal-surface-up surface))
         (panels (make-array 0 :element-type 'single-float
                               :adjustable t :fill-pointer 0))
         (glyph-data (make-array 0 :element-type 'single-float
                                   :adjustable t :fill-pointer 0)))
    (zpb-ttf:with-font-loader (regular *terminal-font-pathname*)
      (zpb-ttf:with-font-loader (bold *terminal-bold-font-pathname*)
        ;; One stable atlas for ordinary shells: printable ASCII in both
        ;; weights up front, other characters joining as they first appear.
        (loop for code from 32 to 126
              do (terminal-character-glyphs terminal (code-char code) nil
                                            regular *terminal-font-pathname*)
                 (terminal-character-glyphs terminal (code-char code) t
                                            bold *terminal-bold-font-pathname*))
        (multiple-value-bind (font-height font-advance descender)
            (terminal-font-metrics regular)
          (multiple-value-bind (scale left bottom)
              (terminal-grid-placement surface columns rows regular)
            (let ((cell-width (* font-advance scale))
                  (cell-height (* font-height scale))
                  (occurrences nil))
              ;; The screen, then per-row runs of explicit backgrounds.
              (push-panel-record
               panels (terminal-surface-point surface 0 0)
               (scaled-vec3 right (terminal-surface-width surface))
               (scaled-vec3 up (terminal-surface-height surface))
               *terminal-screen-color*)
              (flet ((row-bottom (row)
                       (+ bottom (* (- rows row 1) cell-height))))
                (dotimes (row (min rows (luv.ghostty:terminal-screen-rows screen)))
                  (let ((run-start nil) (run-color nil)
                        (row-columns
                          (min columns (luv.ghostty:terminal-screen-columns screen))))
                    (flet ((emit (end)
                             (when run-color
                               (push-panel-record
                                panels
                                (terminal-surface-point
                                 surface (+ left (* run-start cell-width))
                                 (row-bottom row) 0.002)
                                (scaled-vec3 right (* (- end run-start) cell-width))
                                (scaled-vec3 up cell-height)
                                (append (packed-color-radiance
                                         run-color *terminal-background-emission*)
                                        '(1.0))))))
                      (dotimes (column row-columns)
                        (multiple-value-bind (foreground background)
                            (luv.ghostty:terminal-screen-cell-colors
                             screen column row)
                          (unless (eql background run-color)
                            (emit column)
                            (setf run-start column run-color background))
                          (let ((character (luv.ghostty:terminal-screen-character
                                            screen column row)))
                            (unless (char= character #\Space)
                              (let ((bold-p (luv.ghostty:terminal-screen-bold-p
                                             screen column row)))
                                (dolist (glyph (terminal-character-glyphs
                                                terminal character bold-p
                                                (if bold-p bold regular)
                                                (if bold-p
                                                    *terminal-bold-font-pathname*
                                                    *terminal-font-pathname*)))
                                  (push (list glyph column row
                                              (or foreground
                                                  *terminal-default-foreground*))
                                        occurrences)))))))
                      (emit row-columns))))
                ;; A soft block cursor where the shell is writing.
                (when (luv.ghostty:terminal-screen-cursor-visible-p screen)
                  (let ((column (luv.ghostty:terminal-screen-cursor-column screen))
                        (row (luv.ghostty:terminal-screen-cursor-row screen)))
                    (when (and (< -1 column columns) (< -1 row rows))
                      (push-panel-record
                       panels
                       (terminal-surface-point
                        surface (+ left (* column cell-width)) (row-bottom row)
                        0.003)
                       (scaled-vec3 right cell-width)
                       (scaled-vec3 up cell-height)
                       '(0.55 0.70 0.62 0.55)))))
                (let* ((atlas-glyphs
                         (loop for glyphs being the hash-values
                                 of (world-terminal-glyphs terminal)
                               append (copy-list glyphs)))
                       (atlas (luv.slug:slug-glyph-atlas-for
                               (world-terminal-glyph-cache terminal)
                               atlas-glyphs)))
                  (dolist (occurrence (nreverse occurrences))
                    (destructuring-bind (glyph column row foreground) occurrence
                      (let* ((resource (luv.slug:slug-glyph-placement-resource glyph))
                             (serialized (luv.slug:slug-device-glyph-serialized
                                          resource))
                             (location (gethash resource
                                                (luv.slug:slug-glyph-atlas-locations
                                                 atlas)))
                             (min-x (luv.slug:slug-glyph-placement-outline-min-x glyph))
                             (min-y (luv.slug:slug-glyph-placement-outline-min-y glyph))
                             (max-x (luv.slug:slug-glyph-placement-outline-max-x glyph))
                             (max-y (luv.slug:slug-glyph-placement-outline-max-y glyph))
                             (baseline (+ (row-bottom row) (* (- descender) scale)
                                          (* (luv.slug:slug-glyph-placement-origin-y
                                              glyph)
                                             scale)))
                             (pen (+ left (* column cell-width)
                                     (* (luv.slug:slug-glyph-placement-origin-x glyph)
                                        scale)))
                             (quad-origin
                               (terminal-surface-point
                                surface (+ pen (* min-x scale))
                                (+ baseline (* min-y scale)) 0.004))
                             (right-edge (scaled-vec3 right (* (- max-x min-x) scale)))
                             (up-edge (scaled-vec3 up (* (- max-y min-y) scale)))
                             (ink (packed-color-radiance
                                   foreground *terminal-ink-emission*)))
                        (push-floats glyph-data
                                     (vec3-x quad-origin) (vec3-y quad-origin)
                                     (vec3-z quad-origin) min-x
                                     (vec3-x right-edge) (vec3-y right-edge)
                                     (vec3-z right-edge) min-y
                                     (vec3-x up-edge) (vec3-y up-edge)
                                     (vec3-z up-edge) max-x
                                     max-y
                                     (luv.slug:slug-serialized-outline-horizontal-band-count
                                      serialized)
                                     (luv.slug:slug-serialized-outline-vertical-band-count
                                      serialized)
                                     (first location)
                                     (second location) min-x min-y max-x
                                     max-y (first ink) (second ink) (third ink)))))
                  (values (coerce panels '(simple-array single-float (*)))
                          (coerce glyph-data '(simple-array single-float (*)))
                          atlas))))))))))

(defun refresh-world-terminal (terminal device)
  "Rebuild TERMINAL's batch from its shell's current screen, if it changed."
  (when (world-terminal-dirty-p terminal)
    ;; Clear first: output arriving during the rebuild marks it dirty again.
    (setf (world-terminal-dirty-p terminal) nil)
    (multiple-value-bind (panels glyphs atlas)
        (terminal-records terminal (terminal-screen-snapshot terminal))
      (let ((old (world-terminal-batch terminal)))
        (setf (world-terminal-batch terminal)
              (make-world-text-batch device panels glyphs atlas))
        (when old (release-world-text-batch old)))))
  terminal)

;;; The viewer's terminals and the keyboard.

(defvar *viewer-terminals* (make-hash-table :test #'eq :weakness :key)
  "Each viewer's world terminals and the one holding its keyboard, if any.")

(defstruct (viewer-terminals (:constructor make-viewer-terminals ()))
  (terminals nil)
  (focus nil))

(defun viewer-terminals (viewer)
  (or (gethash viewer *viewer-terminals*)
      (setf (gethash viewer *viewer-terminals*) (make-viewer-terminals))))

(defun same-terminal-surface-p (a b)
  (and (equal (terminal-surface-normal a) (terminal-surface-normal b))
       (null (set-exclusive-or (terminal-surface-cells a)
                               (terminal-surface-cells b) :test #'equal))))

(defparameter *terminal-focus-reach* 16.0
  "How far away, in cells, a terminal wall can be taken with Tab.  A screen
is read from across a room, so this is longer than the reach for editing.")

(defun viewer-terminal-at-inspection (viewer)
  "The terminal surface under VIEWER's crosshair, or NIL."
  (let ((inspection
          (multiple-value-bind (origin direction) (viewer-pointer-ray viewer)
            (handler-case
                (raycast-site (viewer-source viewer) origin direction
                              :max-distance *terminal-focus-reach*)
              (luft:outside-domain () nil))))
        (scene (viewer-source viewer)))
    (when (and inspection (typep scene 'scene))
      (let ((cell (site-inspection-cell inspection)))
        (multiple-value-bind (dx dy dz)
            (luft:face-oriented-normal (site-inspection-site inspection))
          (find-terminal-surface scene (luft:site-x cell) (luft:site-y cell)
                                 (luft:site-z cell) (list dx dy dz)))))))

(defun focus-viewer-terminal (viewer)
  "Give VIEWER's keyboard to the terminal it looks at, opening one there.
Return the terminal, or NIL when the crosshair is not on a terminal wall."
  (let ((surface (viewer-terminal-at-inspection viewer))
        (state (viewer-terminals viewer)))
    (when surface
      (let ((terminal
              (or (find-if (lambda (terminal)
                             (same-terminal-surface-p
                              surface (world-terminal-surface terminal)))
                           (viewer-terminals-terminals state))
                  (let ((terminal (open-world-terminal
                                   (renderer-device (viewer-renderer viewer))
                                   surface)))
                    (push terminal (viewer-terminals-terminals state))
                    terminal))))
        (clear-viewer-controls viewer)
        (setf (viewer-terminals-focus state) terminal)))))

(defun unfocus-viewer-terminal (viewer)
  (setf (viewer-terminals-focus (viewer-terminals viewer)) nil))

(defmethod advance-viewer-world-text ((viewer viewer))
  "Retire fallen walls, refresh changed screens, and publish the batches."
  (let ((state (viewer-terminals viewer))
        (scene (viewer-source viewer))
        (renderer (viewer-renderer viewer)))
    (when renderer
      (dolist (terminal (viewer-terminals-terminals state))
        (unless (and (typep scene 'scene)
                     (terminal-surface-standing-p
                      scene (world-terminal-surface terminal)))
          (when (eq terminal (viewer-terminals-focus state))
            (setf (viewer-terminals-focus state) nil))
          (setf (viewer-terminals-terminals state)
                (remove terminal (viewer-terminals-terminals state)))
          ;; Unpublish before releasing what the renderer would draw.
          (publish-renderer-world-text
           renderer (remove nil (mapcar #'world-terminal-batch
                                        (viewer-terminals-terminals state))))
          (close-world-terminal terminal)))
      (dolist (terminal (viewer-terminals-terminals state))
        (refresh-world-terminal terminal (renderer-device renderer)))
      (publish-renderer-world-text
       renderer (remove nil (mapcar #'world-terminal-batch
                                    (viewer-terminals-terminals state)))))))

(defun terminal-release-gesture-p (event)
  (and (eq :tab (canvas-key-event-key-name event))
       (member :shift (canvas-key-event-modifiers event))))

(defmethod handle-canvas-event :around
    ((viewer viewer) canvas (event canvas-key-event))
  (let* ((state (viewer-terminals viewer))
         (focus (viewer-terminals-focus state)))
    (cond
      ;; A focused wall takes every key but the one that gives it back.
      (focus
       (cond ((terminal-release-gesture-p event)
              (when (typep event 'canvas-key-press-event)
                (unfocus-viewer-terminal viewer)))
             ((world-terminal-device focus)
              (luv.terminal:send-pty-device-canvas-key-event
               (world-terminal-device focus) event)))
       nil)
      ;; Tab on a terminal wall takes the keyboard there.
      ((and (typep event 'canvas-key-press-event)
            (eq :tab (canvas-key-event-key-name event))
            (null (canvas-key-event-modifiers event))
            (not (canvas-key-event-repeat-p event))
            (typep (viewer-mode viewer) 'first-person-mode)
            (focus-viewer-terminal viewer))
       nil)
      (t (call-next-method)))))
