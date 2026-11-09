;;;; Luvland's bar: windows on the left, the machine on the right.
;;;;
;;;; The workbench's shared status line belongs to the games; Luvland draws
;;;; its own in the same pane through MCLUV:PAINT-STATUS-BAR, taller, so it
;;;; reads from a sofa at 1920 by 1080.  The left holds a chip per window,
;;;; the focused one filled, then the focused window's title.  The right
;;;; holds tray icons (tray.lisp), the gamepads, sound, network, battery,
;;;; and the clock, each drawn from state sampled elsewhere (indicators.lisp)
;;;; so a frame never waits on the machine.
;;;;
;;;; Painting lays the bar out into regions, which the pointer and the play
;;;; menu both use: whatever a click can do, the gamepad can reach too.  Tray
;;;; icons are client pixmaps, which the shell's stream does not admit, so
;;;; the bar leaves their slots empty and Luvland draws them over it.

(in-package #:luvland)

(defparameter *bar-text-size* 22)
(defparameter *bar-small-text-size* 18)
(defparameter *bar-chip-name-length* 14
  "The longest name a window's chip shows, in characters.")
(defparameter *bar-tray-slot* 30
  "A tray icon's square, in logical pixels.")

(defmethod mcluv:status-bar-height-for ((luvland luvland))
  *bar-height*)

;;; The sample: an immutable plist the shared bar compares with EQUALP, so
;;; the pane repaints only when something it shows has changed.

(defun app-id-name (app-id)
  "The name APP-ID gives its window, or NIL when it names none.
org.gnome.Nautilus is Nautilus and foot is foot, but Chromium makes up an id
for each web app, such as chrome-127.0.0.1__-Default, that names nothing a
person would recognise; its window's title does."
  (flet ((word-p (part)
           (and (plusp (length part))
                (alpha-char-p (char part 0))
                (every (lambda (char) (or (alphanumericp char) (find char "-_"))) part))))
    (let ((parts (uiop:split-string (or app-id "") :separator ".")))
      (when (and (every #'word-p parts)
                 (not (uiop:string-prefix-p "chrome-" app-id))
                 (not (uiop:string-prefix-p "crx_" app-id)))
        (car (last parts))))))

(defun window-chip-name (window)
  (let* ((toplevel (window-toplevel window))
         (name (cond ((terminal-p toplevel) "terminal")
                     ((app-id-name (wl:toplevel-app-id toplevel)))
                     (t (window-title window)))))
    (if (> (length name) *bar-chip-name-length*)
        (concatenate 'string (subseq name 0 (1- *bar-chip-name-length*)) "…")
        (copy-seq name))))

(defun clock-strings (&optional (time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day)
      (decode-universal-time time)
    (declare (ignore second year))
    (values (format nil "~2,'0D:~2,'0D" hour minute)
            (format nil "~A ~D ~A" (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                    date (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
                                           "Aug" "Sep" "Oct" "Nov" "Dec"))))))

(defun bar-sample (luvland)
  (let ((focus (luvland-focus luvland))
        (indicators (luvland-indicators luvland))
        (tray (luvland-tray luvland)))
    (multiple-value-bind (clock date) (clock-strings)
      (list :windows (loop for window in (luvland-windows luvland)
                           collect (list (window-number window) (window-chip-name window)
                                         (eq (window-toplevel window) focus)))
            :title (let ((window (focused-window luvland)))
                     (and window (copy-seq (window-title window))))
            :clock clock :date date
            :network (getf indicators :network)
            :volume (getf indicators :volume)
            :battery (find-if-not (lambda (battery) (getf battery :device-p))
                                  (getf indicators :batteries))
            :gamepads (length (luvland-gamepads luvland))
            :gamepad-battery (find-if (lambda (battery) (getf battery :device-p))
                                      (getf indicators :batteries))
            :tray (and tray
                       (loop for item in (tray-items tray)
                             unless (equal (tray-item-status item) "Passive")
                               collect (list (tray-item-key item) (tray-item-name item)
                                             (and (tray-item-pixmap item) t)
                                             (equal (tray-item-status item)
                                                    "NeedsAttention"))))))))

(defmethod mcluv:sample-status-bar ((luvland luvland) bar)
  (declare (ignore bar))
  (bar-sample luvland))

;;; Layout.  A region is a plist: :KIND, :X1, :X2, and what acting on it
;;; needs, such as :NUMBER for a window or :KEY for a tray item.

(defun bar-style (&key (size *bar-text-size*) bold-p)
  (clim:make-text-style :sans-serif (if bold-p :bold :roman) size))

(defun text-width (medium text style)
  (nth-value 0 (clim:text-size medium text :text-style style)))

(defun fit-text (medium text style width)
  "TEXT, shortened with an ellipsis until it fits in WIDTH, or NIL."
  (cond ((<= (text-width medium text style) width) text)
        ((< width (text-width medium "…" style)) nil)
        (t (let ((low 0) (high (length text)))
             ;; The longest prefix that fits with its ellipsis.
             (loop while (< low high)
                   do (let ((middle (ceiling (+ low high) 2)))
                        (if (<= (text-width medium (concatenate 'string (subseq text 0 middle) "…")
                                            style)
                                width)
                            (setf low middle)
                            (setf high (1- middle)))))
             (concatenate 'string (string-right-trim " " (subseq text 0 low)) "…")))))

(defun indicator-label (kind sample)
  "The text beside KIND's icon in SAMPLE, or NIL when KIND has nothing to show."
  (ecase kind
    (:network (let ((network (getf sample :network)))
                (and network
                     (case (getf network :kind)
                       (:offline "offline")
                       (t (or (getf network :name) ""))))))
    (:volume (let ((volume (getf sample :volume)))
               (and volume (if (getf volume :muted-p)
                               "muted"
                               (format nil "~D%" (getf volume :level))))))
    (:battery (let ((battery (getf sample :battery)))
                (and battery (format nil "~D%" (getf battery :level)))))
    (:gamepad (let ((count (getf sample :gamepads))
                    (battery (getf sample :gamepad-battery)))
                (and (plusp count)
                     (format nil "~:[~;~:*~D%~]~:[~; ×~D~]"
                             (and battery (getf battery :level))
                             (> count 1) count))))))

(defparameter *indicator-icon-widths*
  '((:network . 22) (:volume . 24) (:battery . 26) (:gamepad . 28)))

(defun layout-bar (sample medium width)
  "The regions of a WIDTH-wide bar showing SAMPLE, left to right."
  (let* ((style (bar-style))
         (bold (bar-style :bold-p t))
         (small (bar-style :size *bar-small-text-size*))
         (regions '())
         (right (- width 16)))
    ;; The right side, from the edge inwards: the clock, then indicators.
    (let* ((clock (getf sample :clock))
           (date (getf sample :date))
           (clock-width (text-width medium clock bold))
           (date-width (text-width medium date small)))
      (push (list :kind :clock :x1 (- right clock-width date-width 10) :x2 right
                  :clock clock :date date)
            regions)
      (decf right (+ clock-width date-width 10 22)))
    (dolist (kind '(:battery :network :volume :gamepad))
      (let ((label (indicator-label kind sample)))
        (when label
          (let* ((label (if (eq kind :network)
                            (or (fit-text medium label style 220) "")
                            label))
                 (icon (cdr (assoc kind *indicator-icon-widths*)))
                 (span (+ icon (if (string= label "") 0 (+ 8 (text-width medium label style))))))
            (push (list :kind kind :x1 (- right span) :x2 right :label label) regions)
            (decf right (+ span 22))))))
    (loop for (key name pixmap-p attention-p) in (reverse (getf sample :tray))
          do (push (list :kind :tray :x1 (- right *bar-tray-slot*) :x2 right
                         :key key :name name :pixmap-p pixmap-p :attention-p attention-p)
                   regions)
             (decf right (+ *bar-tray-slot* 8)))
    ;; The left side: window chips, then the focused window's title.
    (let ((left 10))
      (loop for (number name focused-p) in (getf sample :windows)
            for label = (format nil "~D ~A" number name)
            for chip-width = (+ 24 (text-width medium label (if focused-p bold style)))
            while (< (+ left chip-width) (- right 40))
            do (push (list :kind :window :x1 left :x2 (+ left chip-width)
                           :number number :label label :focused-p focused-p)
                     regions)
               (incf left (+ chip-width 6)))
      (let* ((title (or (getf sample :title)
                        (and (null (getf sample :windows))
                             "No windows · A or Guide opens the play menu")))
             (fitted (and title (fit-text medium title style (- right left 40)))))
        (when fitted
          (push (list :kind :title :x1 (+ left 10) :x2 right :label fitted
                      :hint-p (null (getf sample :windows)))
                regions))))
    (sort regions #'< :key (lambda (region) (getf region :x1)))))

;;; Painting.  Icons are built from roundrects and circles, the primitives
;;; the shell draws exactly; arcs would decompose into polygons.

(defun ink (role) (mcluv::ui-ink role))

(defun box (medium x1 y1 x2 y2 role &key (radius 0))
  (mcluv:draw-analytic-rounded-rectangle* medium x1 y1 x2 y2 :radius radius :ink (ink role)))

(defun label (medium text x y &key (role :text) (size *bar-text-size*) bold-p (align :left))
  (clim:draw-text* medium text x y :align-x align :align-y :center
                                   :text-style (bar-style :size size :bold-p bold-p)
                                   :ink (ink role)))

(defun draw-network-icon (medium x cy network)
  (case (getf network :kind)
    (:wired
     ;; Two machines joined below a third.
     (box medium (+ x 7) (- cy 9) (+ x 15) (- cy 3) :text :radius 1)
     (box medium (+ x 10) (- cy 3) (+ x 12) cy :text)
     (box medium (+ x 3) cy (+ x 19) (+ cy 2) :text)
     (box medium (+ x 3) cy (+ x 5) (+ cy 4) :text)
     (box medium (+ x 17) cy (+ x 19) (+ cy 4) :text)
     (box medium x (+ cy 3) (+ x 8) (+ cy 9) :text :radius 1)
     (box medium (+ x 14) (+ cy 3) (+ x 22) (+ cy 9) :text :radius 1))
    (t
     ;; Four bars, as many lit as the signal deserves.
     (let* ((strength (or (getf network :strength)
                          (if (eq (getf network :kind) :offline) 0 100)))
            (lit (cond ((eq (getf network :kind) :offline) 0)
                       ((>= strength 75) 4) ((>= strength 50) 3)
                       ((>= strength 25) 2) (t 1))))
       (dotimes (i 4)
         (box medium (+ x (* i 6)) (- (+ cy 9) (+ 6 (* 4 i))) (+ x (* i 6) 4) (+ cy 9)
              (if (< i lit) :text :track) :radius 1))))))

(defun draw-volume-icon (medium x cy volume)
  (let ((role (if (getf volume :muted-p) :danger :text))
        (level (getf volume :level)))
    (box medium x (- cy 4) (+ x 6) (+ cy 4) role :radius 1)
    (box medium (+ x 6) (- cy 8) (+ x 11) (+ cy 8) role :radius 2)
    (unless (getf volume :muted-p)
      (loop for i from 0 below 3
            for height in '(5 9 13)
            do (box medium (+ x 14 (* i 4)) (- cy (/ height 2)) (+ x 16 (* i 4)) (+ cy (/ height 2))
                    (if (> level (* i 33)) :text :track) :radius 1)))))

(defun draw-battery-icon (medium x cy battery)
  (let ((level (getf battery :level)))
    (box medium x (- cy 7) (+ x 24) (+ cy 7) :text :radius 3)
    (box medium (+ x 2) (- cy 5) (+ x 22) (+ cy 5) :surface :radius 2)
    (box medium (+ x 24) (- cy 3) (+ x 26) (+ cy 3) :text :radius 1)
    (when (plusp level)
      (box medium (+ x 3) (- cy 4) (+ x 3 (* 18 (/ (min level 100) 100))) (+ cy 4)
           (cond ((getf battery :charging-p) :accent) ((< level 15) :danger) (t :text))
           :radius 1))))

(defun draw-gamepad-icon (medium x cy)
  (box medium x (- cy 8) (+ x 28) (+ cy 8) :text :radius 8)
  (box medium (+ x 5) (- cy 1) (+ x 12) (+ cy 1) :surface)
  (box medium (+ x 7.5) (- cy 3.5) (+ x 9.5) (+ cy 3.5) :surface)
  (clim:draw-circle* medium (+ x 19) (+ cy 1) 1.8 :ink (ink :surface))
  (clim:draw-circle* medium (+ x 22.5) (- cy 2) 1.8 :ink (ink :surface)))

(defun paint-region (medium region sample cy)
  (let ((x1 (getf region :x1)) (x2 (getf region :x2)))
    (ecase (getf region :kind)
      (:window
       (let ((focused-p (getf region :focused-p)))
         (box medium x1 (- cy 16) x2 (+ cy 16) (if focused-p :forest :inset) :radius 8)
         (label medium (getf region :label) (+ x1 12) cy
                :role (if focused-p :on-accent :text) :bold-p focused-p)))
      (:title
       (label medium (getf region :label) x1 cy
              :role (if (getf region :hint-p) :muted :text)))
      (:clock
       (label medium (getf region :date) x1 cy :role :muted :size *bar-small-text-size*)
       (label medium (getf region :clock) x2 cy :bold-p t :align :right))
      ((:network :volume :battery :gamepad)
       (let ((kind (getf region :kind)))
         (ecase kind
           (:network (draw-network-icon medium x1 cy (getf sample :network)))
           (:volume (draw-volume-icon medium x1 cy (getf sample :volume)))
           (:battery (draw-battery-icon medium x1 cy (getf sample :battery)))
           (:gamepad (draw-gamepad-icon medium x1 cy)))
         (when (plusp (length (getf region :label)))
           (label medium (getf region :label)
                  (+ x1 (cdr (assoc kind *indicator-icon-widths*)) 8) cy
                  :role (if (and (eq kind :network)
                                 (eq (getf (getf sample :network) :kind) :offline))
                            :muted :text)))))
      (:tray
       (unless (getf region :pixmap-p)
         ;; No pixmap to draw: the item's initial on a tile.
         (box medium x1 (- cy 15) x2 (+ cy 15) :inset :radius 7)
         (label medium (string-upcase (subseq (or (getf region :name) "?") 0
                                              (min 1 (length (getf region :name)))))
                (/ (+ x1 x2) 2) cy :bold-p t :align :center))
       (when (getf region :attention-p)
         (clim:draw-circle* medium (- x2 3) (- cy 12) 4 :ink (ink :selection)))))))

(defmethod mcluv:paint-status-bar ((luvland luvland) bar pane medium)
  (declare (ignore pane))
  (let* ((width (mcluv:status-bar-logical-width bar))
         (sample (mcluv:status-bar-visible-fields bar))
         (cy (/ *bar-height* 2))
         ;; The pane paints once as it is made, before the first sample.
         (regions (and sample (layout-bar sample medium width))))
    (box medium 0 0 width *bar-height* :surface)
    (box medium 0 (1- *bar-height*) width *bar-height* :border)
    (dolist (region regions)
      (paint-region medium region sample cy))
    ;; The pointer, the play menu, and the tray's pixmaps read the layout.
    (setf (luvland-bar-regions luvland) regions)))

;;; Acting on the bar.

(defun tray-item-by-key (luvland key)
  (let ((tray (luvland-tray luvland)))
    (and tray (find key (tray-items tray) :key #'tray-item-key :test #'string=))))

(defun open-tray-menu (luvland item)
  "Offer ITEM's menu at the workbench prompt, as the play menu is offered."
  (let ((tray (luvland-tray luvland))
        (entries (tray-item-menu item)))
    (if (null entries)
        (activate-tray-item tray item)
        (luv.workbench:open-workbench-prompt
         (luvland-workbench luvland)
         :label (format nil "~A:" (tray-item-name item))
         :select-first-p t
         :completer (lambda (text)
                      (loop for (label) in entries
                            when (search (string-trim " " text) label :test #'char-equal)
                              collect label))
         :function (lambda (label)
                     (let ((entry (assoc label entries :test #'string=)))
                       (when entry (click-tray-menu-entry tray item (cdr entry)))))))))

(defun act-on-region (luvland region &key secondary-p)
  "Do what clicking REGION does; true if it does anything."
  (case (getf region :kind)
    (:window
     (let ((window (window-by-number luvland (getf region :number))))
       (when window
         (focus-window luvland (window-toplevel window))
         (when (overview-p luvland) (toggle-overview luvland))
         t)))
    (:volume (change-volume luvland (if secondary-p :up :mute)) t)
    (:gamepad (open-play-menu luvland) t)
    (:tray
     (let ((item (tray-item-by-key luvland (getf region :key))))
       (when item
         (if (or secondary-p (tray-item-item-is-menu-p item))
             (open-tray-menu luvland item)
             (activate-tray-item (luvland-tray luvland) item))
         t)))))

(defun bar-region-at (luvland x y)
  (when (and (luvland-workbench luvland) (< y *bar-height*))
    (find-if (lambda (region) (<= (getf region :x1) x (getf region :x2)))
             (luvland-bar-regions luvland))))

(defun bar-menu-entries (luvland)
  "The bar's clickable things beyond its windows, as play menu entries."
  (let ((tray (luvland-tray luvland)))
    (append
     (let ((volume (getf (luvland-indicators luvland) :volume)))
       (when volume
         (list (cons (if (getf volume :muted-p) "sound: unmute" "sound: mute")
                     (lambda () (change-volume luvland :mute)))
               (cons "sound: louder" (lambda () (change-volume luvland :up)))
               (cons "sound: quieter" (lambda () (change-volume luvland :down))))))
     (when tray
       (loop for item in (tray-items tray)
             for name = (tray-item-name item)
             unless (tray-item-item-is-menu-p item)
               collect (let ((item item))
                         (cons (format nil "tray: ~A" name)
                               (lambda () (activate-tray-item tray item))))
             append (loop for (label . id) in (tray-item-menu item)
                          collect (let ((item item) (id id))
                                    (cons (format nil "tray: ~A › ~A" name label)
                                          (lambda () (click-tray-menu-entry tray item id))))))))))

;;; Tray pixmaps, drawn by Luvland over the bar's empty slots.  Each is a
;;; WINDOW record used only for its texture, view, and size, so it shares
;;; the windows' bindings and frame states.

(defun pixmap-words (width height octets)
  "Big-endian ARGB32 OCTETS as premultiplied BGRA words, rows of WIDTH."
  (let ((words (make-array (list height width) :element-type '(unsigned-byte 32))))
    (dotimes (y height words)
      (dotimes (x width)
        (let* ((i (* 4 (+ x (* y width))))
               (alpha (aref octets i)))
          (flet ((channel (offset) (round (* (aref octets (+ i offset)) alpha) 255)))
            (setf (aref words y x)
                  (logior (ash alpha 24) (ash (channel 1) 16) (ash (channel 2) 8)
                          (channel 3)))))))))

(defun tray-icon (luvland item)
  "ITEM's icon as a WINDOW with a texture, made now if its pixmap changed."
  (let* ((icons (luvland-tray-icons luvland))
         (key (tray-item-key item))
         (pixmap (tray-item-pixmap item))
         (entry (gethash key icons)))
    (if (and entry (eq (car entry) pixmap))
        (cdr entry)
        (destructuring-bind (width height octets) pixmap
          (let* ((device (luvland-device luvland))
                 (texture (luv:create device (luv:make-texture-descriptor
                                              :label "Luvland tray icon"
                                              :size (list width height) :dimensions :2d
                                              :format (luvland-texture-format luvland)
                                              :usage '(:copy-dst :texture-binding))))
                 (icon (make-window nil)))
            (luv:write-texture (luv:device-queue device) (luv:make-texture-copy :texture texture)
                               (pixmap-words width height octets)
                               (luv:make-texture-data-layout :bytes-per-row (* 4 width)
                                                             :rows-per-image height)
                               (list width height))
            (show icon texture (luv:create device (luv:make-texture-view-descriptor
                                                   :texture texture))
                  width height)
            (when entry (forget-tray-icon luvland key))
            (setf (gethash key icons) (cons pixmap icon))
            icon)))))

(defun forget-tray-icon (luvland key)
  (let ((entry (gethash key (luvland-tray-icons luvland))))
    (when entry
      (remhash key (luvland-tray-icons luvland))
      (let ((icon (cdr entry)))
        (maphash (lambda (target state)
                   (declare (ignore target))
                   (let ((bindings (gethash icon state)))
                     (when bindings
                       (remhash icon state)
                       (dolist (slot '(:frame-group :window-group :frame-buffer :window-buffer))
                         (retire luvland (getf bindings slot))))))
                 (luvland-frame-states luvland))
        (retire luvland (window-view icon))
        (retire luvland (window-texture icon))))))

(defun encode-tray-icons (luvland pass state extent)
  "Draw each tray item's pixmap into its slot on the bar."
  (let ((tray (luvland-tray luvland))
        (scale (display-scale luvland)))
    (loop for key being the hash-keys of (luvland-tray-icons luvland)
          unless (and tray (tray-item-by-key luvland key))
            do (forget-tray-icon luvland key))
    (when tray
      (destructuring-bind (width height) extent
        (dolist (region (luvland-bar-regions luvland))
          (when (and (eq (getf region :kind) :tray) (getf region :pixmap-p))
            (let ((item (tray-item-by-key luvland (getf region :key))))
              (when (and item (tray-item-pixmap item))
                (let* ((icon (tray-icon luvland item))
                       (size (round (* (- *bar-tray-slot* 4) scale)))
                       (left (round (* (+ (getf region :x1) 2) scale)))
                       (top (round (- (* (/ *bar-height* 2) scale) (/ size 2))))
                       (matrix (mat4 (/ (* 2 size) width) 0 0 0
                                     0 (/ (* 2 size) height) 0 0
                                     0 0 1 0
                                     (- (/ (* 2 left) width) 1) (- (/ (* 2 top) height) 1) 0 1))
                       (entry (window-bindings luvland state icon)))
                  (luv:write-buffer (getf entry :window-buffer)
                                    (placement-uniform matrix icon :tint '(1 1 1 1) :extend 0 :radius 0
                                                                   :border-color '(0 0 0)))
                  (luv:set-pipeline pass (luvland-icon-pipeline luvland))
                  (luv:set-bind-group pass 0 (getf entry :window-group))
                  (luv:draw pass 4))))))))))
