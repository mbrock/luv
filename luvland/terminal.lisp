;;;; Luvland's own terminal: a shell on a PTY driving a Ghostty screen, drawn
;;;; by Luvland itself into a window on the strip (#E0U5JD).
;;;;
;;;; A TERMINAL stands where a window's Wayland toplevel would, so focus,
;;;; window numbers, the overview, and the commands treat it as one more
;;;; window.  Each frame its screen, when it changed, is drawn into the
;;;; window's texture as the games draw a terminal wall: Ghostty's styled
;;;; snapshot read cell by cell, runs of cell backgrounds as flat panels, and
;;;; each character as Slug glyphs from a shared atlas.  Here the panels and
;;;; glyphs are laid out in the window's own pixels and drawn with the
;;;; workbench's screen-space solid and Slug shaders, so the window pipeline
;;;; then shows the texture like any client's.

(in-package #:luvland)

(defparameter *terminal-font*
  (asdf:system-relative-pathname "luvland" "fonts/MonaspaceNeon-Regular.ttf"))

(defparameter *terminal-bold-font*
  (asdf:system-relative-pathname "luvland" "fonts/MonaspaceNeon-Bold.ttf"))

(defparameter *terminal-font-size* 15
  "The terminal's em in logical pixels.")

(defparameter *terminal-padding* '(18 14)
  "Logical pixels kept clear around the grid, horizontally and vertically;
the grid is centred in what remains, as foot's pad=18x14 center was.")

(defparameter *terminal-foreground* #xF4EFE1
  "Packed sRGB for text without a colour of its own, as on the games' walls.")

(defparameter *terminal-background* #x16181D
  "Packed sRGB behind cells without a colour of its own.")

(defparameter *terminal-shell* nil
  "The program a terminal runs, with its arguments, or NIL for $SHELL.")

(defclass terminal ()
  ((ghostty :initarg :ghostty :reader terminal-ghostty)
   (device :initarg :device :reader terminal-device
           :documentation "The LUV.TERMINAL:PTY-DEVICE whose output drives GHOSTTY.")
   (title :initarg :title :reader terminal-title)
   (dirty-p :initform t :accessor terminal-dirty-p
            :documentation "Set by the PTY owner on output: the texture is stale.")
   (size :initform nil :accessor terminal-size
         :documentation "The window's (width height) in pixels.")
   (grid :initform nil :accessor terminal-grid
         :documentation "The layout for SIZE, a TERMINAL-GRID.")
   (texture :initform nil :accessor terminal-texture)
   (view :initform nil :accessor terminal-view)
   (buffers :initform '() :accessor terminal-buffers
            :documentation "The vertex buffers the texture was last drawn from."))
  (:documentation "A shell Luvland runs and draws itself, standing in for a
toplevel in its window."))

(defmethod print-object ((terminal terminal) stream)
  (print-unreadable-object (terminal stream :type t)
    (format stream "~A ~(~A~)" (terminal-title terminal)
            (luv.terminal:pty-device-state (terminal-device terminal)))))

(defun terminal-p (object)
  (typep object 'terminal))

;;; The status line and the window list name a window by its toplevel's title.
(defmethod wl:toplevel-title ((terminal terminal))
  (terminal-title terminal))

(defmethod wl:toplevel-app-id ((terminal terminal))
  "luvland-terminal")

;;; The grid.

(defstruct terminal-grid
  columns rows
  ;; Pixels: the em, one cell, the grid's top left, and the baseline in a cell.
  font-size cell-width cell-height left top baseline)

(defun fit-terminal-grid (width height scale)
  "The grid for a WIDTH by HEIGHT pixel window at SCALE pixels per logical
pixel: whole-pixel cells of the terminal font, centred within the padding."
  (let* ((loader (luv.slug:slug-font-loader *terminal-font*))
         (em (zpb-ttf:units/em loader))
         (size (* *terminal-font-size* scale))
         (ascent (* size (/ (zpb-ttf:ascender loader) em)))
         (descent (* size (/ (- (zpb-ttf:descender loader)) em)))
         (cell-width (max 1 (round (* size (/ (zpb-ttf:advance-width
                                               (zpb-ttf:find-glyph #\M loader))
                                              em)))))
         (cell-height (max 1 (ceiling (+ ascent descent))))
         (columns (max 1 (floor (- width (* 2 scale (first *terminal-padding*)))
                                cell-width)))
         (rows (max 1 (floor (- height (* 2 scale (second *terminal-padding*)))
                             cell-height))))
    (make-terminal-grid
     :columns columns :rows rows :font-size size
     :cell-width cell-width :cell-height cell-height
     :left (max 0 (floor (- width (* columns cell-width)) 2))
     :top (max 0 (floor (- height (* rows cell-height)) 2))
     :baseline (round (+ ascent (/ (- cell-height ascent descent) 2))))))

;;; Opening and closing.

(defun start-terminal (&key (columns 80) (rows 24) environment
                            (program *terminal-shell*))
  "A terminal running PROGRAM, a list of a program and its arguments or NIL
for the user's shell, on a COLUMNS by ROWS PTY in the home directory."
  (let* ((ghostty (luv.ghostty:make-terminal :columns columns :rows rows))
         (terminal nil)
         (device (handler-bind ((error (lambda (condition)
                                         (declare (ignore condition))
                                         (luv.ghostty:close-terminal ghostty))))
                   (apply #'luv.terminal:open-pty-device ghostty
                          :directory (user-homedir-pathname)
                          :environment environment
                          :on-output (lambda (device bytes)
                                       (declare (ignore device bytes))
                                       (when terminal
                                         (setf (terminal-dirty-p terminal) t)))
                          (when program
                            (list :program (first program)
                                  :arguments (rest program)))))))
    (setf terminal (make-instance
                    'terminal
                    :ghostty ghostty :device device
                    :title (file-namestring
                            (or (first program) (uiop:getenv "SHELL") "sh"))))))

(defun open-terminal (luvland &optional (terminal *terminal*))
  "Open TERMINAL in LUVLAND from any thread: :BUILT-IN for a shell in
Luvland's own terminal, or a list of a program and its arguments to run as a
Wayland client.  Programs the shell starts are clients of LUVLAND too.  A
terminal that cannot start becomes a message, not a failed Luvland."
  (if (listp terminal)
      (apply #'spawn-client luvland terminal)
      (let* ((grid (apply #'fit-terminal-grid
                          (append (fitted-window-size luvland)
                                  (list (display-scale luvland)))))
             (terminal (handler-case
                           (start-terminal :columns (terminal-grid-columns grid)
                                           :rows (terminal-grid-rows grid)
                                           :environment (client-environment luvland))
                         (error (condition)
                           (error-message luvland "terminal: ~A" condition)
                           (return-from open-terminal nil)))))
        ;; Windows are the canvas thread's: the next sync gives it one.
        (luv:request-canvas-frame (luvland-canvas luvland)
                                  (lambda (timestamp)
                                    (declare (ignore timestamp))
                                    (setf (luvland-terminals luvland)
                                          (append (luvland-terminals luvland)
                                                  (list terminal)))))
        terminal)))

(defun terminal-live-p (terminal)
  (member (luv.terminal:pty-device-state (terminal-device terminal))
          '(:starting :running)))

(defun close-terminal (luvland terminal)
  "Hang up TERMINAL's shell and release it; its window goes at the next sync."
  (setf (luvland-terminals luvland) (remove terminal (luvland-terminals luvland)))
  ;; Free Ghostty only once the PTY owner that writes into it has stopped.
  (when (ignore-errors (luv.terminal:close-pty-device (terminal-device terminal)))
    (luv.ghostty:close-terminal (terminal-ghostty terminal)))
  (retire luvland (shiftf (terminal-view terminal) nil))
  (retire luvland (shiftf (terminal-texture terminal) nil))
  (mapc (lambda (buffer) (retire luvland buffer))
        (shiftf (terminal-buffers terminal) '())))

(defun live-terminals (luvland)
  "LUVLAND's terminals, after closing those whose shells have exited."
  (dolist (terminal (luvland-terminals luvland))
    (unless (terminal-live-p terminal)
      (close-terminal luvland terminal)))
  (luvland-terminals luvland))

;;; Input.

(defun send-terminal-key (terminal event)
  "Send canvas key EVENT to TERMINAL's shell, encoded in its current modes."
  (when (terminal-live-p terminal)
    ;; The shell may exit between the check and the send.
    (ignore-errors
     (luv.terminal:send-pty-device-canvas-key-event (terminal-device terminal) event))))

(defun send-terminal-prefix-key (terminal key-name key)
  "Send KEY, the prefix key, pressed as KEY-NAME, to TERMINAL, as C-t t
does to a client."
  (let ((character (first key)))
    (when key-name
      (send-terminal-key terminal
                         (make-instance 'luv:canvas-key-press-event
                                        :timestamp 0 :key-name key-name
                                        :modifiers (rest key)
                                        :character (and (characterp character) character)
                                        :unshifted-character (and (characterp character)
                                                                  character))))))

;;; Fitting the window.  Runs on the canvas thread from SYNC-WINDOWS.

(defun sync-terminal-window (luvland window)
  "Give WINDOW's terminal a texture of the fitted window size, resizing its
grid and PTY when the size changed, and show it."
  (let* ((terminal (window-toplevel window))
         (size (fitted-window-size luvland)))
    (unless (equal size (terminal-size terminal))
      (destructuring-bind (width height) size
        (let* ((device (luvland-device luvland))
               (grid (fit-terminal-grid width height (display-scale luvland)))
               (old (terminal-grid terminal))
               (texture (luv:create device (luv:make-texture-descriptor
                                            :label "Luvland terminal"
                                            :size size :dimensions :2d
                                            :format (luvland-texture-format luvland)
                                            :usage '(:render-attachment
                                                     :texture-binding)))))
          (retire luvland (terminal-view terminal))
          (retire luvland (terminal-texture terminal))
          (setf (terminal-texture terminal) texture
                (terminal-view terminal)
                (luv:create device (luv:make-texture-view-descriptor :texture texture))
                (terminal-size terminal) size
                (terminal-grid terminal) grid
                (terminal-dirty-p terminal) t)
          (unless (and old
                       (= (terminal-grid-columns old) (terminal-grid-columns grid))
                       (= (terminal-grid-rows old) (terminal-grid-rows grid)))
            (when (terminal-live-p terminal)
              (ignore-errors
               (luv.terminal:resize-pty-device
                (terminal-device terminal)
                (terminal-grid-columns grid) (terminal-grid-rows grid)
                :cell-width-pixels (terminal-grid-cell-width grid)
                :cell-height-pixels (terminal-grid-cell-height grid)))))))
      (show window (terminal-texture terminal) (terminal-view terminal)
            (first size) (second size)))))

;;; Drawing.  One renderer per Luvland: the two pipelines, their atlas
;;; layout, and the glyphs of every character any terminal has shown.

(defstruct terminal-renderer
  solid-pipeline glyph-pipeline layout cache
  ;; Slug glyph placements by (character . bold-p), and the atlas holding
  ;; every one of them.
  (glyphs (make-hash-table :test 'equal))
  (atlas nil)
  (bind-groups (make-hash-table :test 'eq)))

(defun make-luvland-terminal-renderer (luvland)
  (let* ((device (luvland-device luvland))
         (format (luvland-texture-format luvland))
         (layout (luv:create device (luv:make-bind-group-layout-descriptor
                                     :label "Luvland terminal glyph atlas"
                                     :entries '((:binding 0 :type :texture)
                                                (:binding 1 :type :texture))))))
    (labels ((module (label role stage)
               ;; Kept with Luvland's own modules, which live as long as it.
               (let ((module (luv:create device (luv:make-shader-module-descriptor
                                                 :label label :language :mathematical
                                                 :code (shader:shader-specification-for
                                                        role stage)))))
                 (push module (luvland-modules luvland))
                 module))
             (pipeline (label role stride formats)
               (luv:create
                device
                (luv:make-render-pipeline-descriptor
                 :label label
                 ;; The panels bind the atlas too; their shader ignores it.
                 :layout layout
                 :vertex `(:module ,(module label role :vertex)
                           :buffers ((:array-stride ,stride
                                      :attributes
                                      ,(loop for format in formats
                                             for location from 0
                                             collect `(:shader-location ,location
                                                       :offset ,(* 12 location)
                                                       :format ,format)))))
                 :fragment `(:module ,(module label role :fragment)
                             :targets ((:format ,format :blend :premultiplied-alpha)))
                 :primitive '(:topology :triangle-list)))))
      (make-terminal-renderer
       :layout layout
       :cache (luv.slug:make-slug-glyph-cache device)
       :solid-pipeline (pipeline "Luvland terminal panels" :mcluv-solid 24
                                 '(:float32x3 :float32x3))
       :glyph-pipeline (pipeline "Luvland terminal glyphs" :mcluv-slug 72
                                 (make-list 6 :initial-element :float32x3))))))

(defun luvland-terminal-renderer* (luvland)
  (or (luvland-terminal-renderer luvland)
      (let ((renderer (make-luvland-terminal-renderer luvland)))
        ;; One stable atlas for ordinary shells, as on the games' walls:
        ;; printable ASCII in both weights up front.
        (loop for code from 33 to 126
              do (character-glyphs renderer (code-char code) nil)
                 (character-glyphs renderer (code-char code) t))
        (setf (luvland-terminal-renderer luvland) renderer))))

(defun character-glyphs (renderer character bold-p)
  "CHARACTER's Slug glyph placements in the regular or bold face, cached; a
new one retires the atlas, which is rebuilt to hold it."
  (let ((key (cons character bold-p))
        (table (terminal-renderer-glyphs renderer)))
    (multiple-value-bind (glyphs present-p) (gethash key table)
      (if present-p
          glyphs
          (let* ((font (if bold-p *terminal-bold-font* *terminal-font*))
                 (cache (terminal-renderer-cache renderer)))
            (setf (terminal-renderer-atlas renderer) nil
                  (gethash key table)
                  (luv.slug:make-slug-glyph-placements
                   (luv.slug:cached-slug-shaped-text cache font (string character))
                   (luv.slug:slug-font-loader font) cache font)))))))

(defun renderer-atlas (renderer)
  (or (terminal-renderer-atlas renderer)
      (setf (terminal-renderer-atlas renderer)
            (luv.slug:slug-glyph-atlas-for
             (terminal-renderer-cache renderer)
             (loop for glyphs being the hash-values of (terminal-renderer-glyphs renderer)
                   append (copy-list glyphs))))))

(defun atlas-bind-group (luvland renderer atlas)
  (or (gethash atlas (terminal-renderer-bind-groups renderer))
      (setf (gethash atlas (terminal-renderer-bind-groups renderer))
            (luv:create (luvland-device luvland)
                        (luv:make-bind-group-descriptor
                         :label "Luvland terminal glyph atlas"
                         :layout (terminal-renderer-layout renderer)
                         :entries `((:binding 0 :resource
                                     ,(luv.slug:slug-glyph-atlas-band-view atlas))
                                    (:binding 1 :resource
                                     ,(luv.slug:slug-glyph-atlas-curve-view atlas))))))))

(defun terminal-screen-snapshot (terminal)
  "Ghostty's styled screen, taken under the PTY owner's lock, in Luvland's
default colours."
  (let ((screen (luv.terminal:call-with-pty-device-terminal
                 (terminal-device terminal) #'luv.ghostty:terminal-screen)))
    (setf (luv.ghostty:terminal-screen-default-foreground screen) *terminal-foreground*
          (luv.ghostty:terminal-screen-default-background screen) *terminal-background*)
    screen))

(defun packed-color (packed srgb-target-p)
  "Packed sRGB #xRRGGBB as (r g b 1), linear when an sRGB target encodes."
  (flet ((channel (position)
           (let ((value (/ (ldb (byte 8 position) packed) 255.0)))
             (cond ((not srgb-target-p) value)
                   ((<= value 0.04045) (/ value 12.92))
                   (t (expt (/ (+ value 0.055) 1.055) 2.4))))))
    (list (channel 16) (channel 8) (channel 0) 1.0)))

(defun screen-vertices (renderer grid screen width height srgb-target-p)
  "Panel and glyph vertices drawing SCREEN on GRID in a WIDTH by HEIGHT
target, and the atlas the glyphs address."
  (let* ((columns (min (terminal-grid-columns grid) (luv.ghostty:terminal-screen-columns screen)))
         (rows (min (terminal-grid-rows grid) (luv.ghostty:terminal-screen-rows screen)))
         (cell-width (terminal-grid-cell-width grid))
         (cell-height (terminal-grid-cell-height grid))
         (size (terminal-grid-font-size grid))
         (padding (max (luv.slug:slug-dilation-em size) (/ 2.0 size)))
         (panels (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0))
         (glyph-data (make-array 0 :element-type 'single-float :adjustable t :fill-pointer 0)))
    (flet ((bold-p (column row) (luv.ghostty:terminal-screen-bold-p screen column row))
           (cell-x (column) (+ (terminal-grid-left grid) (* column cell-width)))
           (cell-y (row) (+ (terminal-grid-top grid) (* row cell-height)))
           (push-vertex (data &rest values)
             (dolist (value values)
               (vector-push-extend (float value 1.0) data))))
      ;; Every character is placed before the atlas is taken: a new one
      ;; changes it.
      (dotimes (row rows)
        (dotimes (column columns)
          (character-glyphs renderer (luv.ghostty:terminal-screen-character screen column row)
                            (bold-p column row))))
      (let ((atlas (renderer-atlas renderer)))
        (flet ((panel (column-start column-end row packed)
                 (let ((left (- (* 2 (/ (cell-x column-start) width)) 1))
                       (right (- (* 2 (/ (cell-x column-end) width)) 1))
                       (top (- (* 2 (/ (cell-y row) height)) 1))
                       (bottom (- (* 2 (/ (cell-y (1+ row)) height)) 1))
                       (color (butlast (packed-color packed srgb-target-p))))
                   (loop for (x y) in `((,left ,top) (,right ,top) (,right ,bottom)
                                        (,left ,top) (,right ,bottom) (,left ,bottom))
                         do (apply #'push-vertex panels x y 1.0 color))))
               (glyph (glyph column row color)
                 (let* ((resource (luv.slug:slug-glyph-placement-resource glyph))
                        (serialized (luv.slug:slug-device-glyph-serialized resource))
                        (location (gethash resource (luv.slug:slug-glyph-atlas-locations atlas)))
                        (min-x (luv.slug:slug-glyph-placement-outline-min-x glyph))
                        (min-y (luv.slug:slug-glyph-placement-outline-min-y glyph))
                        (max-x (luv.slug:slug-glyph-placement-outline-max-x glyph))
                        (max-y (luv.slug:slug-glyph-placement-outline-max-y glyph))
                        (pen-x (+ (cell-x column)
                                  (* size (luv.slug:slug-glyph-placement-origin-x glyph))))
                        (pen-y (- (+ (cell-y row) (terminal-grid-baseline grid))
                                  (* size (luv.slug:slug-glyph-placement-origin-y glyph)))))
                   ;; The quad grows by the Slug filter's reach on each side.
                   (flet ((vertex (outline-x outline-y)
                            (mcluv::append-gpu-text-vertex
                             glyph-data width height
                             (+ pen-x (* size outline-x)) (- pen-y (* size outline-y))
                             1.0 outline-x outline-y
                             (luv.slug:slug-serialized-outline-horizontal-band-count serialized)
                             (first location) (second location)
                             (luv.slug:slug-serialized-outline-vertical-band-count serialized)
                             min-x min-y max-x max-y color)))
                     (let ((left (- min-x padding)) (right (+ max-x padding))
                           (bottom (- min-y padding)) (top (+ max-y padding)))
                       (vertex left bottom) (vertex right bottom) (vertex right top)
                       (vertex left bottom) (vertex right top) (vertex left top))))))
          (dotimes (row rows)
            ;; Runs of one explicit background become one panel.
            (let ((run-start 0) (run-color nil))
              (dotimes (column columns)
                (multiple-value-bind (foreground background)
                    (luv.ghostty:terminal-screen-cell-colors screen column row)
                  (unless (eql background run-color)
                    (when run-color (panel run-start column row run-color))
                    (setf run-start column run-color background))
                  (let ((character (luv.ghostty:terminal-screen-character screen column row)))
                    (unless (char= character #\Space)
                      (let ((color (packed-color foreground srgb-target-p)))
                        (dolist (placement (character-glyphs renderer character
                                                             (bold-p column row)))
                          (glyph placement column row color)))))))
              (when run-color (panel run-start columns row run-color))))
          (values panels glyph-data atlas))))))

(defun vertex-buffer (luvland data)
  (let ((buffer (luv:create (luvland-device luvland)
                            (luv:make-buffer-descriptor
                             :label "Luvland terminal vertices"
                             :size (max 16 (* 4 (length data)))
                             :usage '(:vertex :copy-dst)))))
    (when (plusp (length data))
      (luv:write-buffer buffer (coerce data '(simple-array single-float (*)))))
    buffer))

(defun encode-terminal (luvland terminal encoder)
  "Draw TERMINAL's current screen into its texture."
  ;; Clear first: output arriving while this draws marks it stale again.
  (setf (terminal-dirty-p terminal) nil)
  (let* ((renderer (luvland-terminal-renderer* luvland))
         (grid (terminal-grid terminal))
         (screen (terminal-screen-snapshot terminal))
         (srgb-p (eq (luvland-texture-format luvland) :bgra8-unorm-srgb)))
    ;; A resize reaches Ghostty through the PTY owner; until it has, draw
    ;; what there is and come back next frame.
    (unless (and (= (luv.ghostty:terminal-screen-columns screen) (terminal-grid-columns grid))
                 (= (luv.ghostty:terminal-screen-rows screen) (terminal-grid-rows grid)))
      (setf (terminal-dirty-p terminal) t))
    (destructuring-bind (width height) (terminal-size terminal)
      (multiple-value-bind (panels glyphs atlas)
          (screen-vertices renderer grid screen width height srgb-p)
        (let ((panel-buffer (vertex-buffer luvland panels))
              (glyph-buffer (vertex-buffer luvland glyphs))
              (bind-group (atlas-bind-group luvland renderer atlas))
              (pass (luv:begin-render-pass
                     encoder
                     (luv:make-render-pass-descriptor
                      :label "Luvland terminal"
                      :color-attachments
                      `((:view ,(terminal-view terminal)
                         :load-op :clear :store-op :store
                         :clear-value ,(coerce (packed-color *terminal-background* srgb-p)
                                               'vector)))))))
          (mapc (lambda (buffer) (retire luvland buffer)) (terminal-buffers terminal))
          (setf (terminal-buffers terminal) (list panel-buffer glyph-buffer))
          (loop for (pipeline buffer count) in `((,(terminal-renderer-solid-pipeline renderer)
                                                  ,panel-buffer ,(floor (length panels) 6))
                                                 (,(terminal-renderer-glyph-pipeline renderer)
                                                  ,glyph-buffer ,(floor (length glyphs) 18)))
                when (plusp count)
                  do (luv:set-pipeline pass pipeline)
                     (luv:set-bind-group pass 0 bind-group)
                     (luv:set-vertex-buffer pass 0 buffer)
                     (luv:draw pass count))
          (luv:end-pass pass)
          (luv:prepare-texture encoder (terminal-texture terminal) :texture-binding))))))

(defun encode-terminals (luvland encoder)
  "Redraw every terminal whose screen changed, before the windows sample them."
  (dolist (terminal (luvland-terminals luvland))
    (when (and (terminal-dirty-p terminal) (terminal-texture terminal))
      (encode-terminal luvland terminal encoder))))
