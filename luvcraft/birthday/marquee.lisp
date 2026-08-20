;;; The marquee: a terminal wall wearing the birthday's name in lights.
;;;
;;; A rectangle of terminal blocks mounts over the gazebo's entrance, and a
;;; BIRTHDAY-MARQUEE-DISPLAY -- an ordinary Ghostty terminal wall -- shows
;;; big block letters scrolling across it, figlet-fashion, in rainbow ink
;;; with chase lights above and below.  No pty is involved: the marquee
;;; writes ANSI frames straight into its own Ghostty emulator and marks
;;; itself dirty, and the wall's stock refresh path re-snapshots the screen
;;; and republishes the glyphs, exactly as it would for a shell's output.
;;; The wall's ink is emissive above the lens's bright-pass threshold, so
;;; the letters glow into the dusk on their own.

(in-package #:luvcraft.birthday)

;;; ---------------------------------------------------------------------
;;; The letterforms
;;;
;;; Five-by-seven bitmaps for the characters a birthday banner needs; each
;;; pixel is drawn two cells wide so the letters read square on a terminal
;;; grid.  Anything the font does not know renders as blank space, which
;;; for a marquee is the right kind of failure.

(defparameter *marquee-font*
  '((#\A " XXX "
         "X   X"
         "X   X"
         "XXXXX"
         "X   X"
         "X   X"
         "X   X")
    (#\L "X    "
         "X    "
         "X    "
         "X    "
         "X    "
         "X    "
         "XXXXX")
    (#\E "XXXXX"
         "X    "
         "X    "
         "XXXX "
         "X    "
         "X    "
         "XXXXX")
    (#\X "X   X"
         "X   X"
         " X X "
         "  X  "
         " X X "
         "X   X"
         "X   X")
    (#\4 "   X "
         "  XX "
         " X X "
         "X  X "
         "XXXXX"
         "   X "
         "   X ")
    (#\Space "   "
             "   "
             "   "
             "   "
             "   "
             "   "
             "   "))
  "Rows of X-and-space pixels, seven per glyph.")

(defconstant +marquee-glyph-rows+ 7)

(defun marquee-banner-rows (text)
  "Render TEXT through the marquee font into seven strings of pixels."
  (let ((rows (make-array +marquee-glyph-rows+ :initial-element "")))
    (loop for character across (string-upcase text)
          for bitmap = (rest (assoc character *marquee-font*))
          when bitmap
            do (dotimes (row +marquee-glyph-rows+)
                 (setf (aref rows row)
                       (concatenate 'string (aref rows row)
                                    (nth row bitmap) " "))))
    ;; A dozen blank columns end the cycle, so the sign goes briefly dark
    ;; between one pass of the name and the next instead of chasing its
    ;; own tail around the wrap.
    (let ((gap (make-string 12 :initial-element #\Space)))
      (loop for row below +marquee-glyph-rows+
            do (setf (aref rows row)
                     (concatenate 'string (aref rows row) gap))))
    (coerce rows 'list)))

;;; ---------------------------------------------------------------------
;;; The frames
;;;
;;; A frame is one full ANSI repaint: clear, home, then every row.  The
;;; banner scrolls right to left with wraparound; ink hue drifts with the
;;; scroll so the rainbow rides the letters; the light strips above and
;;; below chase the other way.  Colour changes are emitted only at column
;;; boundaries where they change, which keeps a frame to a few kilobytes.

(defun marquee-hue-color (hue)
  "HUE in turns to (values red green blue), bright and saturated."
  (flet ((lobe (offset)
           (let ((angle (* 2 pi (- hue offset))))
             (round (* 255 (min 1.0 (max 0.0 (+ 0.55 (* 0.65 (cos angle))))))))))
    (values (lobe 0.0) (lobe 1/3) (lobe 2/3))))

(defun write-marquee-ink (stream red green blue)
  (format stream "~C[38;2;~D;~D;~Dm" (code-char 27) red green blue))

(defun write-marquee-lights-row (stream columns phase)
  "One row of chase lights: every fourth cell lit, walking with PHASE."
  (let ((last-ink nil))
    (dotimes (column columns)
      (if (zerop (mod (+ column phase) 4))
          (multiple-value-bind (red green blue)
              (marquee-hue-color (/ (mod (+ column (* 3 phase)) 24) 24.0))
            (let ((ink (list red green blue)))
              (unless (equal ink last-ink)
                (write-marquee-ink stream red green blue)
                (setf last-ink ink)))
            (write-char #\FULL_BLOCK stream))
          (write-char #\Space stream)))))

(defun marquee-frame-string (banner-rows banner-width columns rows phase)
  "The whole ANSI frame for the marquee at scroll PHASE.

Every row is cursor-addressed rather than newline-separated: a newline
after the last row of an exactly-filled screen scrolls it, which is how
the sign's top row of lights once quietly walked off the wall."
  (let ((escape (code-char 27))
        (top-gap (max 0 (floor (- rows +marquee-glyph-rows+ 2) 2))))
    (with-output-to-string (stream)
      (format stream "~C[2J" escape)
      (flet ((at-row (row)
               (format stream "~C[~D;1H" escape row)))
        ;; The upper light strip, the letters centred between the strips,
        ;; and the lower strip chasing the other way.
        (at-row 1)
        (write-marquee-lights-row stream columns phase)
        (loop for pixels in banner-rows
              for row from (+ 2 top-gap)
              do (at-row row)
                 (let ((last-ink nil))
                   (dotimes (column columns)
                     ;; Two screen cells per font pixel, scrolling leftward.
                     (let* ((pixel-column (mod (+ (floor column 2) phase)
                                               banner-width))
                            (on (and (< pixel-column (length pixels))
                                     (char= (char pixels pixel-column) #\X))))
                       (if on
                           (multiple-value-bind (red green blue)
                               (marquee-hue-color
                                (/ (mod (- pixel-column phase) 36) 36.0))
                             (let ((ink (list red green blue)))
                               (unless (equal ink last-ink)
                                 (write-marquee-ink stream red green blue)
                                 (setf last-ink ink)))
                             (write-char #\FULL_BLOCK stream))
                           (write-char #\Space stream))))))
        (at-row rows)
        (write-marquee-lights-row stream columns (- phase))
        (format stream "~C[0m" escape)))))

;;; ---------------------------------------------------------------------
;;; The display

(defparameter *marquee-tick-seconds* 0.09
  "How often the marquee advances one pixel column: about eleven a second,
the pace of a sign worth reading rather than a strobe.")

(defclass birthday-marquee-display (luvcraft::terminal-display)
  ((text :initform "ALEX 4" :accessor marquee-text)
   (banner-rows :initform nil :accessor marquee-banner-cache)
   (banner-width :initform 0 :accessor marquee-banner-width)
   (phase :initform 0 :accessor marquee-phase)
   (clock :initform nil :accessor marquee-clock)))

(defun (setf marquee-banner) (text display)
  (let ((rows (marquee-banner-rows text)))
    (setf (marquee-text display) text
          (marquee-banner-cache display) rows
          (marquee-banner-width display) (length (first rows))))
  text)

(defmethod luvcraft::refresh-luvcraft-overlay :before
    ((display birthday-marquee-display) (session luvcraft:luvcraft-session))
  ;; The wall clock, not the session clock: the sign keeps its own beat the
  ;; way the tape orb does, clamped so a stall does not fast-forward it.
  (let* ((now (/ (get-internal-real-time)
                 (float internal-time-units-per-second)))
         (then (or (marquee-clock display) now)))
    (when (or (null (marquee-clock display))
              (>= (- now then) *marquee-tick-seconds*))
      (setf (marquee-clock display) now)
      (incf (marquee-phase display))
      (let ((domain (luvcraft::terminal-grid-presentation-domain
                     (luvcraft::terminal-display-presentation display))))
        (ghostty:write-terminal
         (luvcraft::terminal-display-terminal display)
         (marquee-frame-string
          (marquee-banner-cache display)
          (marquee-banner-width display)
          (luvcraft::terminal-grid-domain-columns domain)
          (luvcraft::terminal-grid-domain-rows domain)
          (marquee-phase display))))
      (setf (luvcraft::terminal-display-dirty-p display) t))))

;;; ---------------------------------------------------------------------
;;; Mounting it

(defun add-birthday-marquee (session &key (text "ALEX 4")
                                          (origin '(-4 15 -9))
                                          (width 9) (height 3)
                                          (columns 72) (rows 11))
  "Mount the marquee wall and start its animation; returns the display.

Places a WIDTH by HEIGHT rectangle of terminal blocks at ORIGIN facing
-z (the :back face), discovers it as a terminal surface, and dresses it
in a marquee display showing TEXT.  Placing the same blocks again on a
reopened world is a no-op edit, so mounting is idempotent."
  (remove-birthday-marquee session)
  (destructuring-bind (x y z) origin
    (let ((world (luvcraft:luvcraft-session-world session)))
      (luvcraft:place-terminal-block-rectangle world x y z :back width height)
      (multiple-value-bind (surface status)
          (luvcraft:find-terminal-surface world x y z :back)
        (unless surface
          (error "The marquee wall did not become a surface: ~S." status))
        (let ((display (luvcraft::make-terminal-display
                        session surface columns rows
                        :class 'birthday-marquee-display
                        :fixture (format nil "~C[2J" (code-char 27)))))
          (setf (marquee-banner display) text)
          display)))))

(defun remove-birthday-marquee (session)
  "Take the marquee display down, leaving the wall blocks standing."
  (dolist (overlay (luvcraft::luvcraft-session-overlays session))
    (when (typep overlay 'birthday-marquee-display)
      (luvcraft:remove-luvcraft-overlay session overlay))))
