;;;; Styled screen snapshots through libghostty-vt's render-state API.
;;;;
;;;; TERMINAL-TEXT gives plain trimmed text.  TERMINAL-SCREEN gives one dense
;;;; per-cell snapshot: the base character, resolved foreground/background
;;;; colours, bold/inverse flags, the default palette colours, and the cursor.

(in-package #:luv.ghostty)

;;; Render-state C ABI.  Enum values follow include/ghostty/vt/render.h.

(defconstant +render-state-data-cols+ 1)
(defconstant +render-state-data-rows+ 2)
(defconstant +render-state-data-row-iterator+ 4)
(defconstant +render-state-data-color-background+ 5)
(defconstant +render-state-data-color-foreground+ 6)
(defconstant +render-state-data-cursor-visible+ 11)
(defconstant +render-state-data-cursor-viewport-has-value+ 14)
(defconstant +render-state-data-cursor-viewport-x+ 15)
(defconstant +render-state-data-cursor-viewport-y+ 16)

(defconstant +render-state-row-data-cells+ 3)

(defconstant +render-state-row-cells-data-style+ 2)
(defconstant +render-state-row-cells-data-graphemes-len+ 3)
(defconstant +render-state-row-cells-data-graphemes-buf+ 4)
(defconstant +render-state-row-cells-data-bg-color+ 5)
(defconstant +render-state-row-cells-data-fg-color+ 6)

(cffi:defcstruct color-rgb
  (r :uint8)
  (g :uint8)
  (b :uint8))

(cffi:defcunion style-color-value
  (palette :uint8)
  (rgb (:struct color-rgb))
  (padding :uint64))

(cffi:defcstruct style-color
  (tag :int)
  (value (:union style-color-value)))

(cffi:defcstruct style
  (size :size)
  (fg-color (:struct style-color))
  (bg-color (:struct style-color))
  (underline-color (:struct style-color))
  (bold :bool)
  (italic :bool)
  (faint :bool)
  (blink :bool)
  (inverse :bool)
  (invisible :bool)
  (strikethrough :bool)
  (overline :bool)
  (underline :int))

(cffi:defcfun ("ghostty_render_state_new" %render-state-new) ghostty-result
  (allocator :pointer)
  (state :pointer))

(cffi:defcfun ("ghostty_render_state_free" %render-state-free) :void
  (state :pointer))

(cffi:defcfun ("ghostty_render_state_update" %render-state-update)
    ghostty-result
  (state :pointer)
  (terminal :pointer))

(cffi:defcfun ("ghostty_render_state_get" %render-state-get) ghostty-result
  (state :pointer)
  (data :int)
  (output :pointer))

(cffi:defcfun ("ghostty_render_state_row_iterator_new"
               %render-state-row-iterator-new) ghostty-result
  (allocator :pointer)
  (iterator :pointer))

(cffi:defcfun ("ghostty_render_state_row_iterator_free"
               %render-state-row-iterator-free) :void
  (iterator :pointer))

(cffi:defcfun ("ghostty_render_state_row_iterator_next"
               %render-state-row-iterator-next) :bool
  (iterator :pointer))

(cffi:defcfun ("ghostty_render_state_row_get" %render-state-row-get)
    ghostty-result
  (iterator :pointer)
  (data :int)
  (output :pointer))

(cffi:defcfun ("ghostty_render_state_row_cells_new"
               %render-state-row-cells-new) ghostty-result
  (allocator :pointer)
  (cells :pointer))

(cffi:defcfun ("ghostty_render_state_row_cells_free"
               %render-state-row-cells-free) :void
  (cells :pointer))

(cffi:defcfun ("ghostty_render_state_row_cells_next"
               %render-state-row-cells-next) :bool
  (cells :pointer))

(cffi:defcfun ("ghostty_render_state_row_cells_get"
               %render-state-row-cells-get) ghostty-result
  (cells :pointer)
  (data :int)
  (output :pointer))

;;; The snapshot.

(defstruct (terminal-screen (:constructor %make-terminal-screen))
  "One dense styled snapshot of a terminal viewport.

CHARACTERS holds the base character of each cell in row-major order, #\\Space
where the cell is empty.  FOREGROUND and BACKGROUND hold packed #xRRGGBB
colours per cell, or -1 where the cell has no explicit colour and the
DEFAULT-FOREGROUND / DEFAULT-BACKGROUND applies.  FLAGS holds bit 0 = bold,
bit 1 = inverse, bit 2 = faint, bit 3 = invisible."
  (columns 0 :type fixnum)
  (rows 0 :type fixnum)
  (characters "" :type simple-string)
  (foreground nil :type (simple-array fixnum (*)))
  (background nil :type (simple-array fixnum (*)))
  (flags nil :type (simple-array (unsigned-byte 8) (*)))
  (default-foreground #xFFFFFF :type fixnum)
  (default-background #x000000 :type fixnum)
  (cursor-visible-p nil)
  (cursor-column 0 :type fixnum)
  (cursor-row 0 :type fixnum))

(defconstant +screen-flag-bold+ 1)
(defconstant +screen-flag-inverse+ 2)
(defconstant +screen-flag-faint+ 4)
(defconstant +screen-flag-invisible+ 8)

(declaim (inline terminal-screen-offset))
(defun terminal-screen-offset (screen column row)
  (+ column (* row (terminal-screen-columns screen))))

(defun terminal-screen-character (screen column row)
  (schar (terminal-screen-characters screen)
         (terminal-screen-offset screen column row)))

(defun terminal-screen-bold-p (screen column row)
  (logtest +screen-flag-bold+
           (aref (terminal-screen-flags screen)
                 (terminal-screen-offset screen column row))))

(defun terminal-screen-inverse-p (screen column row)
  (logtest +screen-flag-inverse+
           (aref (terminal-screen-flags screen)
                 (terminal-screen-offset screen column row))))

(defun terminal-screen-cell-colors (screen column row)
  "Return the effective foreground and background #xRRGGBB of one cell.

Inverse video and the cursor cell swap the two; the second value is NIL when
the cell paints no background of its own."
  (let* ((offset (terminal-screen-offset screen column row))
         (flags (aref (terminal-screen-flags screen) offset))
         (fg (aref (terminal-screen-foreground screen) offset))
         (bg (aref (terminal-screen-background screen) offset))
         (fg-set-p (>= fg 0))
         (bg-set-p (>= bg 0))
         (fg (if fg-set-p fg (terminal-screen-default-foreground screen)))
         (bg (if bg-set-p bg (terminal-screen-default-background screen)))
         (cursor-p (and (terminal-screen-cursor-visible-p screen)
                        (= column (terminal-screen-cursor-column screen))
                        (= row (terminal-screen-cursor-row screen)))))
    (cond
      ((and (logtest +screen-flag-inverse+ flags) (not cursor-p))
       (values bg fg))
      (cursor-p
       ;; Draw the cursor as a block of the current foreground colour.
       (values (if (logtest +screen-flag-inverse+ flags) fg bg) fg))
      (t (values fg (and bg-set-p bg))))))

(defun terminal-screen-text (screen)
  "The snapshot's rows as one newline-separated, right-trimmed string."
  (with-output-to-string (stream)
    (dotimes (row (terminal-screen-rows screen))
      (unless (zerop row) (terpri stream))
      (write-string
       (string-right-trim
        " "
        (subseq (terminal-screen-characters screen)
                (terminal-screen-offset screen 0 row)
                (terminal-screen-offset screen 0 (1+ row))))
       stream))))

(defun read-color-rgb (pointer)
  (logior (ash (cffi:foreign-slot-value pointer '(:struct color-rgb) 'r) 16)
          (ash (cffi:foreign-slot-value pointer '(:struct color-rgb) 'g) 8)
          (cffi:foreign-slot-value pointer '(:struct color-rgb) 'b)))

(defun render-state-color (state data)
  (cffi:with-foreign-object (color '(:struct color-rgb))
    (check-result (%render-state-get state data color) 'terminal-screen)
    (read-color-rgb color)))

(defun render-state-bool (state data)
  (cffi:with-foreign-object (value :bool)
    (setf (cffi:mem-ref value :bool) nil)
    (check-result (%render-state-get state data value) 'terminal-screen)
    (cffi:mem-ref value :bool)))

(defun render-state-uint16 (state data)
  (cffi:with-foreign-object (value :uint16)
    (setf (cffi:mem-ref value :uint16) 0)
    (check-result (%render-state-get state data value) 'terminal-screen)
    (cffi:mem-ref value :uint16)))

(defun cell-optional-color (cells data)
  "A resolved cell colour, or -1 when the cell has none."
  (cffi:with-foreign-object (color '(:struct color-rgb))
    (let ((result (%render-state-row-cells-get cells data color)))
      (case result
        (:success (read-color-rgb color))
        (:invalid-value -1)
        (t (check-result result 'terminal-screen) -1)))))

(defun read-screen-cells (cells screen row style codepoints)
  "Copy one row of CELLS into SCREEN's dense lanes."
  (let ((columns (terminal-screen-columns screen))
        (characters (terminal-screen-characters screen))
        (foreground (terminal-screen-foreground screen))
        (background (terminal-screen-background screen))
        (flags (terminal-screen-flags screen)))
    (loop for column from 0
          while (and (< column columns) (%render-state-row-cells-next cells))
          do (let ((offset (terminal-screen-offset screen column row)))
               (cffi:with-foreign-object (count :uint32)
                 (setf (cffi:mem-ref count :uint32) 0)
                 (check-result
                  (%render-state-row-cells-get
                   cells +render-state-row-cells-data-graphemes-len+ count)
                  'graphemes-len)
                 (let ((count (cffi:mem-ref count :uint32)))
                   (when (plusp count)
                     ;; Only the base codepoint is placed on the wall grid.
                     ;; Combining marks stay in Ghostty's model for now.
                     (check-result
                      (%render-state-row-cells-get
                       cells +render-state-row-cells-data-graphemes-buf+
                       codepoints)
                      'graphemes-buf)
                     (let ((code (cffi:mem-aref codepoints :uint32 0)))
                       (when (and (plusp code) (< code char-code-limit))
                         (setf (schar characters offset) (code-char code)))))))
               (setf (aref foreground offset)
                     (cell-optional-color
                      cells +render-state-row-cells-data-fg-color+)
                     (aref background offset)
                     (cell-optional-color
                      cells +render-state-row-cells-data-bg-color+))
               (setf (cffi:foreign-slot-value style '(:struct style) 'size)
                     (cffi:foreign-type-size '(:struct style)))
               (check-result
                (%render-state-row-cells-get
                 cells +render-state-row-cells-data-style+ style)
                'style)
               (flet ((flag (slot bit)
                        (if (cffi:foreign-slot-value style '(:struct style) slot)
                            bit 0)))
                 (setf (aref flags offset)
                       (logior (flag 'bold +screen-flag-bold+)
                               (flag 'inverse +screen-flag-inverse+)
                               (flag 'faint +screen-flag-faint+)
                               (flag 'invisible +screen-flag-invisible+))))))))

(defun terminal-screen (terminal)
  "Snapshot TERMINAL's active viewport as a styled TERMINAL-SCREEN."
  (ensure-terminal-open terminal)
  (let ((state (cffi:null-pointer))
        (iterator (cffi:null-pointer))
        (cells (cffi:null-pointer)))
    (unwind-protect
         (cffi:with-foreign-objects ((output :pointer)
                                     (cells-output :pointer)
                                     (style '(:struct style))
                                     (codepoints :uint32 64))
           (setf (cffi:mem-ref output :pointer) (cffi:null-pointer))
           (check-result (%render-state-new (cffi:null-pointer) output)
                         'state-new)
           (setf state (cffi:mem-ref output :pointer))
           (check-result (%render-state-update state (terminal-pointer terminal))
                         'state-update)
           (setf (cffi:mem-ref output :pointer) (cffi:null-pointer))
           (check-result (%render-state-row-iterator-new (cffi:null-pointer) output)
                         'iterator-new)
           (setf iterator (cffi:mem-ref output :pointer))
           (setf (cffi:mem-ref output :pointer) (cffi:null-pointer))
           (check-result (%render-state-row-cells-new (cffi:null-pointer) output)
                         'cells-new)
           (setf cells (cffi:mem-ref output :pointer))
           (let* ((columns (render-state-uint16 state +render-state-data-cols+))
                  (rows (render-state-uint16 state +render-state-data-rows+))
                  (count (* columns rows))
                  (screen
                    (%make-terminal-screen
                     :columns columns :rows rows
                     :characters (make-string count :initial-element #\Space)
                     :foreground (make-array count :element-type 'fixnum
                                                   :initial-element -1)
                     :background (make-array count :element-type 'fixnum
                                                   :initial-element -1)
                     :flags (make-array count :element-type '(unsigned-byte 8)
                                              :initial-element 0)
                     :default-foreground
                     (render-state-color
                      state +render-state-data-color-foreground+)
                     :default-background
                     (render-state-color
                      state +render-state-data-color-background+))))
             (when (and (render-state-bool
                         state +render-state-data-cursor-visible+)
                        (render-state-bool
                         state +render-state-data-cursor-viewport-has-value+))
               (setf (terminal-screen-cursor-visible-p screen) t
                     (terminal-screen-cursor-column screen)
                     (render-state-uint16
                      state +render-state-data-cursor-viewport-x+)
                     (terminal-screen-cursor-row screen)
                     (render-state-uint16
                      state +render-state-data-cursor-viewport-y+)))
             ;; These out-parameters point at the pre-allocated handles.
             (setf (cffi:mem-ref output :pointer) iterator)
             (check-result
              (%render-state-get
               state +render-state-data-row-iterator+ output)
              'row-iterator)
             (loop for row from 0
                   while (and (< row rows)
                              (%render-state-row-iterator-next iterator))
                   do (setf (cffi:mem-ref cells-output :pointer) cells)
                      (check-result
                       (%render-state-row-get
                        iterator +render-state-row-data-cells+ cells-output)
                       'row-cells)
                      (read-screen-cells cells screen row style codepoints))
             screen))
      (unless (cffi:null-pointer-p cells) (%render-state-row-cells-free cells))
      (unless (cffi:null-pointer-p iterator)
        (%render-state-row-iterator-free iterator))
      (unless (cffi:null-pointer-p state) (%render-state-free state)))))
