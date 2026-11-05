;;; A shared retained-GPU lobby instrument.
;;;
;;; HANDLE-REPAINT reads only the frame's immutable semantic snapshot.  The
;;; application refresh seam copies lobby state before paint, then the direct
;;; compositor evaluates analytic media and Slug text in the destination's
;;; native pixels with ordinary premultiplied-alpha composition.

(in-package #:luv.lobby.mcclim)

(defconstant +lobby-hud-width+ 320)
(defconstant +lobby-hud-height+ 140
  "The card's height without a join list.")
(defconstant +lobby-hud-pane-height+ 196
  "The pane's height: room for a join list.  The card is only as tall as
what it shows, and sits at the pane's bottom.")
(defconstant +lobby-hud-margin+ 16)
(defconstant +lobby-join-rows+ 3)
(defconstant +lobby-join-top+ 128
  "The baseline of the join list's first row, from the card's top.")
(defconstant +lobby-join-row-height+ 22)

;;; A join list.  An application whose sessions can be joined offers its
;;; entries through LOBBY-JOIN-ENTRIES; the HUD copies them at the frame
;;; boundary, as it copies the radio's snapshot, and a click on a row asks
;;; the application to LOBBY-JOIN it.

(defstruct (lobby-join-entry (:constructor make-lobby-join-entry (&key title detail target)))
  "One session to join: what it is called, a short DETAIL such as its
players, and the TARGET to dial."
  (title "" :read-only t)
  (detail "" :read-only t)
  (target "" :read-only t))

(defgeneric lobby-join-entries (owner)
  (:documentation "The sessions OWNER can join, as LOBBY-JOIN-ENTRY values,
and true as a second value when OWNER keeps a join list at all.")
  (:method (owner)
    (declare (ignore owner))
    (values nil nil)))

(defgeneric lobby-join (owner entry)
  (:documentation "Join ENTRY's session, as the player at OWNER chose.")
  (:method (owner entry)
    (declare (ignore owner entry))
    nil))

(defgeneric lobby-hud-owner (frame)
  (:documentation "The application whose join list FRAME shows, or NIL.")
  (:method (frame)
    (declare (ignore frame))
    nil))

(defclass lobby-hud-pane (mcluv:transparent-gpu-application-pane) ())

(mcluv::define-ui-themed-pane lobby-hud-pane)

(defclass lobby-hud-state ()
  ((client :initarg :client :initform nil :reader lobby-hud-client)
   (visible-snapshot
    :initarg :visible-snapshot :initform nil
    :accessor lobby-hud-visible-snapshot)
   (visible-entries :initform nil :accessor lobby-hud-visible-entries
                    :documentation "The join list paint reads, copied at a frame boundary.")
   (join-list-p :initform nil :accessor lobby-hud-join-list-p
                :documentation "Whether the owner keeps a join list.")
   (painted-revision :initform nil :accessor lobby-hud-painted-revision)
   (compositor :initform nil :accessor lobby-hud-compositor)))

(define-application-frame lobby-hud
    (standard-application-frame lobby-hud-state)
  ()
  (:menu-bar nil)
  ;; The pane background is part of McCLIM's retained stream.  Leaving it at
  ;; the implementation default paints an opaque white rectangle behind the
  ;; rounded card even though the card itself is analytic.
  (:panes (sheet (make-pane 'lobby-hud-pane
                            :background +transparent-ink+
                            :width +lobby-hud-width+
                            :height +lobby-hud-pane-height+
                            :min-width +lobby-hud-width+
                            :min-height +lobby-hud-pane-height+
                            :max-width +lobby-hud-width+
                            :max-height +lobby-hud-pane-height+)))
  ;; This single authored surface is its own layout.  An intermediate rack
  ;; pane would contribute the implementation's opaque background.
  (:layouts (default sheet)))

(defun lobby-status-ink (status)
  (ecase status
    (:online (mcluv:ui-ink :accent))
    ((:starting :connecting) (mcluv:ui-ink :selection))
    (:offline (mcluv:ui-ink :danger))
    ((:stopping :stopped) (mcluv:ui-ink :muted))))

(defun lobby-short-error (error)
  (when error
    (if (> (length error) 42)
        (concatenate 'string (subseq error 0 39) "...")
        error)))

(defun lobby-hud-card-height (frame)
  "How tall FRAME's card is: taller by the rows of its join list."
  (if (lobby-hud-join-list-p frame)
      (+ +lobby-join-top+ 18
         (* +lobby-join-row-height+
            (1- (max 1 (min +lobby-join-rows+ (length (lobby-hud-visible-entries frame)))))))
      +lobby-hud-height+))

(defun lobby-short-text (text length)
  (if (> (length text) length)
      (concatenate 'string (subseq text 0 (- length 1)) "…")
      text))

(defmethod handle-repaint ((pane lobby-hud-pane) region)
  (declare (ignore region))
  ;; This is deliberately the only state read by paint.  CLIENT's lock and
  ;; worker are outside the McCLIM repaint transaction.
  (let* ((frame (pane-frame pane))
         (snapshot (lobby-hud-visible-snapshot frame))
         (status (if snapshot
                     (luv.lobby:lobby-snapshot-status snapshot)
                     :stopped))
         (peers (and snapshot (luv.lobby:lobby-snapshot-peers snapshot)))
         (error
           (and snapshot (luv.lobby:lobby-snapshot-last-error snapshot)))
         (join-list-p (lobby-hud-join-list-p frame))
         (entries (lobby-hud-visible-entries frame))
         (height (lobby-hud-card-height frame))
         ;; The card sits at the bottom of its pane.
         (top (- +lobby-hud-pane-height+ height)))
    (with-sheet-medium (medium pane)
      (mcluv:draw-analytic-rounded-rectangle*
       medium 4 (+ top 5) (- +lobby-hud-width+ 2) (+ top height -1)
       :radius 14 :ink (mcluv:ui-ink :shadow))
      (mcluv:draw-ui-surface
       medium 1 (+ top 1) (- +lobby-hud-width+ 3) (+ top height -5))
      (draw-circle* pane 20 (+ top 22) 5 :ink (lobby-status-ink status))
      (draw-text* pane "lobby" 34 (+ top 22)
                  :align-y :center :text-size 14 :text-face :bold
                  :ink (mcluv:ui-ink :text))
      (draw-text* pane (string-downcase (symbol-name status))
                  (- +lobby-hud-width+ 18) (+ top 22)
                  :align-x :right :align-y :center :text-size 11
                  :ink (mcluv:ui-ink :muted))
      (cond
        (peers
         (let ((rows (if join-list-p 2 4)))
           (loop for peer in peers
                 for y from (+ top 51) by 21
                 for row from 1 to rows
                 do (draw-circle* pane 21 y 4
                                  :ink (mcluv:ui-ink :accent))
                    (draw-text* pane (if (and (= row rows) (> (length peers) rows))
                                         (format nil "~A and ~D more"
                                                 (luv.lobby:lobby-peer-name peer)
                                                 (- (length peers) rows))
                                         (luv.lobby:lobby-peer-name peer))
                                35 y
                                :align-y :center :text-size 13
                                :ink (mcluv:ui-ink :text)))))
        ((and error (eq status :offline))
         (draw-text* pane (lobby-short-error error) 17 (+ top 52)
                     :align-y :center :text-size 11
                     :ink (mcluv:ui-ink :muted)))
        (t
         (draw-text* pane "quiet" 17 (+ top 52)
                     :align-y :center :text-size 11
                     :ink (mcluv:ui-ink :muted))))
      (when join-list-p
        (mcluv:draw-analytic-rounded-rectangle*
         medium 17 (+ top 89) (- +lobby-hud-width+ 18) (+ top 90)
         :radius 0.5 :ink (mcluv:ui-ink :muted))
        (draw-text* pane "join" 17 (+ top 106)
                    :align-y :center :text-size 12 :text-face :bold
                    :ink (mcluv:ui-ink :text))
        (draw-text* pane (if entries "click a world" "")
                    (- +lobby-hud-width+ 18) (+ top 106)
                    :align-x :right :align-y :center :text-size 11
                    :ink (mcluv:ui-ink :muted))
        (if entries
            (loop for entry in entries
                  for y from (+ top +lobby-join-top+) by +lobby-join-row-height+
                  repeat +lobby-join-rows+
                  do (draw-circle* pane 21 y 4 :ink (mcluv:ui-ink :selection))
                     (draw-text* pane (lobby-short-text (lobby-join-entry-title entry) 22)
                                 35 y :align-y :center :text-size 13
                                 :ink (mcluv:ui-ink :text))
                     (draw-text* pane (lobby-short-text (lobby-join-entry-detail entry) 24)
                                 (- +lobby-hud-width+ 18) y
                                 :align-x :right :align-y :center :text-size 11
                                 :ink (mcluv:ui-ink :muted)))
            (draw-text* pane "no open worlds" 17 (+ top +lobby-join-top+)
                        :align-y :center :text-size 11
                        :ink (mcluv:ui-ink :muted)))))))

(defun lobby-hud-entry-at (frame y)
  "The join entry whose row is at Y in FRAME's pane, or NIL."
  (let* ((top (- +lobby-hud-pane-height+ (lobby-hud-card-height frame)))
         (row (floor (- y top (- +lobby-join-top+ (/ +lobby-join-row-height+ 2)))
                     +lobby-join-row-height+)))
    (and (lobby-hud-join-list-p frame)
         (< -1 row +lobby-join-rows+)
         (nth row (lobby-hud-visible-entries frame)))))

(defmethod handle-event ((pane lobby-hud-pane) (event pointer-button-press-event))
  (let* ((frame (pane-frame pane))
         (entry (lobby-hud-entry-at frame (pointer-event-y event))))
    (when entry
      (lobby-join (lobby-hud-owner frame) entry))))

(defun lobby-hud-mirror (frame &key (errorp t))
  (let* ((sheet (frame-top-level-sheet frame))
         (mirror (and sheet (mcluv::sheet-direct-mirror sheet))))
    (cond
      ((typep mirror 'mcluv:luv-gpu-mirror) mirror)
      (errorp (error "Lobby HUD requires an embedded direct-GPU mirror, got ~S."
                     mirror))
      (t nil))))

(defun validate-lobby-hud-direct-presentation (frame)
  (let* ((mirror (lobby-hud-mirror frame))
         (sheet (mcluv:mirror-sheet mirror)))
    (when (mcluv:mirror-texture mirror)
      (error "Lobby HUD unexpectedly acquired a raster backing texture."))
    (dolist (painted-sheet (mcluv::gpu-sheet-paint-order sheet))
      (let ((medium (mcluv::gpu-sheet-presentation-medium painted-sheet)))
        (when (and (typep medium 'mcluv:luv-gpu-medium)
                   (mcluv:gpu-medium-fallback-report medium))
          (error "Lobby HUD used decomposed primitive fallbacks: ~S"
                 (mcluv:gpu-medium-fallback-report medium)))))
    (when (find-if
           (lambda (command)
             (typep command 'mcluv::gpu-prepared-image-command))
           (mcluv::gpu-mirror-prepared-commands mirror))
      (error "Lobby HUD prepared an image/raster command.")))
  frame)

(defun repaint-lobby-hud (frame)
  (let ((mirror (lobby-hud-mirror frame)))
    (unless (mcluv:mirror-embedded-p mirror)
      (error "Lobby HUD mirror is not embedded in its application canvas."))
    (mcluv:repaint-gpu-mirror mirror)
    (validate-lobby-hud-direct-presentation frame)
    (setf (lobby-hud-painted-revision frame)
          (let ((snapshot (lobby-hud-visible-snapshot frame)))
            (if snapshot
                (luv.lobby:lobby-snapshot-revision snapshot)
                -1))))
  frame)

(defun refresh-lobby-hud (frame)
  "Copy the current semantic snapshot and repaint only when its revision moved."
  (let* ((client (lobby-hud-client frame))
         (snapshot (and client (luv.lobby:lobby-client-snapshot client)))
         (revision
           (if snapshot (luv.lobby:lobby-snapshot-revision snapshot) -1)))
    (multiple-value-bind (entries join-list-p) (lobby-join-entries (lobby-hud-owner frame))
      (if (or (mcluv:ui-theme-changed-p frame)
              (not (eql revision (lobby-hud-painted-revision frame)))
              (not (equalp entries (lobby-hud-visible-entries frame)))
              (not (eq join-list-p (lobby-hud-join-list-p frame)))
              (null (mcluv::gpu-mirror-prepared-commands
                     (lobby-hud-mirror frame))))
          (progn
            ;; Publish the complete value before McCLIM begins its repaint.
            (setf (lobby-hud-visible-snapshot frame) snapshot
                  (lobby-hud-visible-entries frame) entries
                  (lobby-hud-join-list-p frame) (and join-list-p t))
            (repaint-lobby-hud frame))
          (mcluv:prepare-gpu-mirror-compositor
           (lobby-hud-mirror frame)))))
  frame)

(defun lobby-hud-screen-state
    (frame viewport-logical-extent &key (margin +lobby-hud-margin+))
  "Place FRAME in the lower-left using destination logical coordinates.

The affine state is expressed against the logical viewport.  A high-density
drawable therefore supplies its native samples directly to analytic edges and
Slug glyphs; no panel-sized texture is enlarged."
  (declare (ignore frame))
  (destructuring-bind (viewport-width viewport-height)
      viewport-logical-extent
    (let* ((scale
             (min 1.0
                  (/ (max 1.0 (- viewport-width (* 2 margin)))
                     +lobby-hud-width+)
                  (/ (max 1.0 (- viewport-height (* 2 margin)))
                     +lobby-hud-pane-height+)))
           (half-width (/ (* +lobby-hud-width+ scale) viewport-width))
           (half-height (/ (* +lobby-hud-pane-height+ scale) viewport-height))
           (center-x (+ -1.0 (/ (* 2.0 margin) viewport-width) half-width))
           (center-y (+ -1.0 (/ (* 2.0 margin) viewport-height) half-height)))
      (make-array
       12 :element-type 'single-float
       :initial-contents
       (mapcar (lambda (value) (coerce value 'single-float))
               (list center-x center-y 0.0 1.0
                     half-width 0.0 0.0 0.0
                     0.0 half-height 0.0 0.0))))))

(defun make-embedded-lobby-hud
    (client canvas context device
     &key (title "lobby") (install-compositor-p t))
  "Create CLIENT's textureless retained HUD on an application's GPU owner."
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager
           (or (first (clim-internals::frame-managers port))
               (make-instance 'mcluv:luv-frame-manager :port port)))
         (snapshot (luv.lobby:lobby-client-snapshot client))
         (frame
           (let ((mcluv:*embedded-mirror-target* canvas)
                 (mcluv:*embedded-mirror-context* context)
                 (mcluv:*embedded-mirror-device* device))
             (make-application-frame
              'lobby-hud :frame-manager manager :enable t
              :client client :visible-snapshot snapshot))))
    (setf (frame-pretty-name frame) title)
    (mcluv:make-gpu-frame-background-transparent frame)
    (handler-case
        (let* ((mirror (lobby-hud-mirror frame))
               (compositor
                 (and install-compositor-p
                      (make-instance 'mcluv:direct-gpu-mirror-compositor
                                     :mirror mirror))))
          (unless (and (mcluv:mirror-embedded-p mirror)
                       (null (mcluv:mirror-texture mirror)))
            (error "Lobby HUD did not receive a textureless embedded mirror."))
          (when compositor
            (setf (mcluv:mirror-compositor mirror) compositor
                  (lobby-hud-compositor frame) compositor))
          (repaint-lobby-hud frame)
          frame)
      (error (condition)
        (unless (eq :disowned (frame-state frame))
          (destroy-frame frame))
        (error condition)))))

(defun destroy-lobby-hud (frame)
  (when (and frame (not (eq :disowned (frame-state frame))))
    (destroy-frame frame))
  nil)
