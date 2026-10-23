;;;; Luvcraft on Luft as a game of its own: a host frame with Luvcraft's
;;;; title, keys, hotbar, and save, over the Luft renderer and shared play.
;;;;
;;;; The host brings walking, flight, first person, the workbench, and
;;;; streaming; this file brings only what is Luvcraft's (#J3WZZS).

(defpackage #:luvcraft.luft.game
  (:use #:cl)
  (:local-nicknames (#:render #:luft.render)
                    (#:terrain #:luft.terrain)
                    (#:game #:luvcraft.luft)
                    (#:vec3 #:luv.arithmetic.lisp.vec3))
  (:documentation "The Luvcraft on Luft host: its frame, keys, hotbar, and saves.")
  (:export #:play
           #:luvcraft-viewer
           #:luvcraft-viewer-hotbar
           #:*starting-hotbar*))

(in-package #:luvcraft.luft.game)

(defparameter *starting-hotbar*
  '(:grass :dirt :planks :bricks :red-wool :yellow-wool :blue-wool
    :crystal :flowers :stone)
  "The blocks on the number row when a game starts, as Luvcraft's own
quickbar begins: things to build a house with, colours, and a lamp.")

(defparameter *title*
  "Luvcraft — click to play · WASD walk · Space jump, twice to fly · left mine · right or E place · C pick · 1–0 blocks · F1 keys · Esc release")

;;; Luvcraft's in-world keys inherit the viewer's walking and tools; the
;;; atelier's own instruments (Tab terminal, B bevel, C construction lines,
;;; R reset, edit mode) are not part of the game.

(clim:define-command-table luvcraft-world :inherit-from (render:luft-play))
(clim:define-command-table luvcraft-world-release
  :inherit-from (render:luft-play-release))

(clim:define-application-frame luvcraft-viewer (render:game-host)
  ((luvcraft-world :initarg :luvcraft-world :reader luvcraft-viewer-world
                   :documentation "Luvcraft's own world: its source and edits as read.")
   (resume :initarg :resume :initform nil :reader luvcraft-viewer-resume)
   (inhabitants :initarg :inhabitants :initform nil
                :reader luvcraft-viewer-inhabitants)
   (animals :initform nil :accessor viewer-animals)
   (animal-previous-figures :initform (make-hash-table :test #'eq)
                            :reader viewer-animal-previous-figures)
   (animals-initialized-p :initform nil :accessor viewer-animals-initialized-p)
   (hotbar :initarg :hotbar :accessor luvcraft-viewer-hotbar)
   (inventory :accessor viewer-inventory)
   (creative-p :initform nil :accessor viewer-creative-p)
   (inventory-page :initform nil :accessor viewer-inventory-page)
   (held-block :initform nil :accessor viewer-held-block))
  (:command-table (luvcraft-viewer
                   :inherit-from (render:luft-window render:luft-window-release
                                  luvcraft-world luvcraft-world-release)
                   :inherit-menu t))
  (:menu-bar nil))

(defmethod initialize-instance :after ((viewer luvcraft-viewer) &key)
  ;; After the viewer's own method, which chose the vocabulary's first member.
  (let* ((resume (luvcraft-viewer-resume viewer))
         (inventory (restore-inventory resume))
         (selected (getf resume :selected-block)))
    (setf (viewer-inventory viewer) inventory
          (viewer-creative-p viewer) (getf resume :creative-p)
          (viewer-held-block viewer)
          (and selected
               (find selected (luvcraft:block-inventory-blocks inventory)
                     :key #'luvcraft::block-save-description :test #'equal)))
    (when (getf resume :quickbar)
      (setf (luvcraft-viewer-hotbar viewer)
            (mapcar (lambda (name)
                      (game:block-placement
                       (find name (luvcraft:block-inventory-blocks inventory)
                             :key #'luvcraft:block-kind-name)))
                    (getf resume :quickbar))))
    (setf (render:viewer-edit-material viewer)
          (or (and selected
                   (ignore-errors
                    (game:block-placement
                     (luvcraft::restore-block-value selected))))
              (first (luvcraft-viewer-hotbar viewer))))
    (put-on-hotbar viewer (render:viewer-edit-material viewer))))

(defun restore-inventory (resume)
  (let ((inventory (luvcraft:make-block-inventory
                    :blocks (remove-duplicates
                             (append (mapcar #'luvcraft:block-kind-named *starting-hotbar*)
                                     (luvcraft:placeable-block-kinds))
                             :from-end t)
                    :quantity 0)))
    (if (member :inventory resume)
        (setf (luvcraft:block-inventory-entries inventory)
              (loop for (description quantity) in (getf resume :inventory)
                    do (check-type quantity (integer 0))
                    collect (make-instance 'luvcraft:block-inventory-entry
                                           :block (luvcraft::restore-block-value description)
                                           :quantity quantity)))
        (dolist (description (getf resume :carried))
          (luvcraft:add-block-to-inventory
           inventory (luvcraft::restore-block-value description))))
    inventory))

(defun inventory-resume (viewer)
  (let ((resume (copy-list (luvcraft-viewer-resume viewer))))
    (setf (getf resume :inventory)
          (loop for entry in (luvcraft:block-inventory-entries (viewer-inventory viewer))
                collect (list (luvcraft::block-save-description
                               (luvcraft:block-inventory-entry-block entry))
                              (luvcraft:block-inventory-entry-quantity entry)))
          (getf resume :carried)
          (mapcar #'luvcraft::block-save-description
                  (luvcraft:block-inventory-carried-blocks (viewer-inventory viewer)))
          (getf resume :creative-p) (viewer-creative-p viewer)
          (getf resume :selected-block)
          (luvcraft::block-save-description
           (selected-block viewer (render:viewer-edit-material viewer)))
          (getf resume :quickbar)
          (mapcar #'terrain:material-placement-name (luvcraft-viewer-hotbar viewer)))
    resume))

(defun selected-block (viewer placement)
  (let ((held (viewer-held-block viewer)))
    (if (and held (eq placement (game:block-placement held))) held
        (or (game::luvcraft-block-for-placement placement)
            (find placement (luvcraft:block-inventory-blocks (viewer-inventory viewer))
                  :key #'game:block-placement)))))

(defmethod render:apply-viewer-world-edit :around
    ((viewer luvcraft-viewer) cell placement)
  ;; Commit inventory changes only after the engine accepts the edit.  Undo
  ;; and redo pass through this same boundary, so they conserve stock too.
  (let* ((inventory (viewer-inventory viewer))
         (block (and placement (selected-block viewer placement)))
         (entry (and block (luvcraft:block-inventory-entry-for inventory block)))
         (finite-p (or (not (viewer-creative-p viewer))
                       (and block (luvcraft:block-kind-carried-p block))))
         (old (terrain:scene-material-placement-at (render:viewer-source viewer) cell))
         (coordinate (game:site-luvcraft-cell (streaming-source viewer) cell))
         (overlay (luvcraft:little-world-source-edits
                   (luvcraft:block-world-source (luvcraft-viewer-world viewer))))
         (original (gethash coordinate (luvcraft::block-edit-overlay-entries overlay)))
         (old-block (and old (or (and original
                                       (eq old (game:block-placement original)) original)
                                 (game::luvcraft-block-for-placement old)))))
    (when (and block finite-p
               (or (null entry) (zerop (luvcraft:block-inventory-entry-quantity entry))))
      (setf (render:viewer-last-edit-status viewer) :empty)
      (return-from render:apply-viewer-world-edit (values nil :empty)))
    (multiple-value-bind (edit status) (call-next-method)
      (when edit
        (when (and block finite-p)
          (luvcraft:remove-block-from-inventory inventory block))
        (when old-block (luvcraft:add-block-to-inventory inventory old-block))
        ;; Keep per-instance kinds in Luvcraft's overlay: Luft's palette is
        ;; deliberately only a visual identity (all films share one reel).
        (apply #'luvcraft::record-block-edit overlay block coordinate)
        (when (and old-block (null placement))
          (setf (viewer-held-block viewer) old-block)
          (put-on-hotbar viewer old)))
      (values edit status))))

(defun put-on-hotbar (viewer placement)
  "Select PLACEMENT, replacing the selected slot when it is not on the bar."
  (let ((block (selected-block viewer placement)))
    (when block (luvcraft:add-block-to-inventory (viewer-inventory viewer) block 0)))
  (let* ((hotbar (luvcraft-viewer-hotbar viewer))
         (selected (position (render:viewer-edit-material viewer) hotbar)))
    (unless (member placement hotbar)
      (setf (nth (or selected 0) hotbar) placement)))
  (setf (render:viewer-edit-material viewer) placement))

(defmethod render:viewer-hotbar ((viewer luvcraft-viewer))
  (luvcraft-viewer-hotbar viewer))

(defmethod render:viewer-pick-material ((viewer luvcraft-viewer) placement)
  (let* ((inspection (render::viewer-inspection viewer))
         (coordinate (and inspection
                          (game:site-luvcraft-cell
                           (streaming-source viewer)
                           (render::site-inspection-cell inspection))))
         (overlay (luvcraft:little-world-source-edits
                   (luvcraft:block-world-source (luvcraft-viewer-world viewer)))))
    (setf (viewer-held-block viewer)
          (gethash coordinate (luvcraft::block-edit-overlay-entries overlay))))
  (put-on-hotbar viewer placement)
  (setf (render:viewer-last-edit-status viewer) :selected))

(defmethod render:viewer-key-event-tables
    ((viewer luvcraft-viewer) (event luv:canvas-key-press-event))
  (values 'render:luft-window 'luvcraft-world))

(defmethod render:viewer-key-event-tables
    ((viewer luvcraft-viewer) (event luv:canvas-key-release-event))
  (values 'render:luft-window-release 'luvcraft-world-release))

(clim:define-command (com-place-block :command-table luvcraft-world
                                      :name "Place Block" :keystroke (:e))
    ()
  (render:viewer-edit-at-crosshair (render:viewer-command-viewer) :build))

(clim:define-command (com-mine-block :command-table luvcraft-world
                                     :name "Mine Block" :keystroke (:x))
    ()
  (render:viewer-edit-at-crosshair (render:viewer-command-viewer) :smash))

(clim:define-command (com-pick-block :command-table luvcraft-world
                                     :name "Pick Block" :keystroke (:c))
    ()
  (render:viewer-edit-at-crosshair (render:viewer-command-viewer) :pick))

(clim:define-command (com-select-hotbar-slot :command-table luvcraft-world
                                             :name "Select Hotbar Slot")
    ((slot 'integer))
  (let* ((viewer (render:viewer-command-viewer))
         (page (viewer-inventory-page viewer)))
    (if page
        (let ((entry (nth (+ (* page 10) slot)
                          (luvcraft:block-inventory-entries (viewer-inventory viewer)))))
          (when entry
            (setf (viewer-held-block viewer) (luvcraft:block-inventory-entry-block entry)
                  (viewer-inventory-page viewer) nil)
            (put-on-hotbar viewer (game:block-placement (viewer-held-block viewer)))))
        (progn
          (setf (viewer-held-block viewer) nil)
          (render:select-viewer-material viewer slot)))))

(clim:define-command (com-inventory :command-table luvcraft-world
                                   :name "Inventory" :keystroke (:i)) ()
  (let ((viewer (render:viewer-command-viewer)))
    (setf (viewer-inventory-page viewer)
          (if (viewer-inventory-page viewer) nil 0))))

(clim:define-command (com-next-inventory-page :command-table luvcraft-world
                                             :name "Next Inventory Page"
                                             :keystroke (:n)) ()
  (let ((viewer (render:viewer-command-viewer)))
    (setf (viewer-inventory-page viewer)
          (mod (1+ (or (viewer-inventory-page viewer) -1))
               (max 1 (ceiling (length (luvcraft:block-inventory-entries
                                       (viewer-inventory viewer))) 10))))))

(clim:define-command (com-creative :command-table luvcraft-world
                                 :name "Creative Mode" :keystroke (:f6)) ()
  (let ((viewer (render:viewer-command-viewer)))
    (setf (viewer-creative-p viewer) (not (viewer-creative-p viewer)))))

(defmethod mcluv:palette-items-for ((viewer luvcraft-viewer))
  (if (viewer-inventory-page viewer)
      (nthcdr (* 10 (viewer-inventory-page viewer))
              (luvcraft:block-inventory-entries (viewer-inventory viewer)))
      (render:viewer-hotbar viewer)))

(defmethod mcluv:palette-selected-item ((viewer luvcraft-viewer))
  (if (viewer-inventory-page viewer)
      (luvcraft:block-inventory-entry-for (viewer-inventory viewer)
                                         (selected-block viewer (render:viewer-edit-material viewer)))
      (render:viewer-edit-material viewer)))

(defmethod mcluv:palette-item-label
    ((viewer luvcraft-viewer) (placement terrain:material-placement))
  (let ((block (selected-block viewer placement)))
    (if (typep block 'luvcraft::film-block-kind) (luvcraft::film-label block)
        (call-next-method))))

(defmethod mcluv:palette-item-label
    ((viewer luvcraft-viewer) (entry luvcraft:block-inventory-entry))
  (let ((block (luvcraft:block-inventory-entry-block entry)))
    (if (typep block 'luvcraft::film-block-kind) (luvcraft::film-label block)
        (mcluv:palette-item-label viewer (game:block-placement block)))))

(defmethod mcluv:palette-item-tones
    ((viewer luvcraft-viewer) (entry luvcraft:block-inventory-entry))
  (mcluv:palette-item-tones viewer
                          (game:block-placement (luvcraft:block-inventory-entry-block entry))))

(defmethod mcluv:palette-item-luminous-p
    ((viewer luvcraft-viewer) (entry luvcraft:block-inventory-entry))
  (mcluv:palette-item-luminous-p viewer
                               (game:block-placement (luvcraft:block-inventory-entry-block entry))))

(defmethod mcluv:palette-item-key ((viewer luvcraft-viewer) item index)
  (let* ((block (if (typep item 'luvcraft:block-inventory-entry)
                    (luvcraft:block-inventory-entry-block item)
                    (selected-block viewer item)))
         (entry (luvcraft:block-inventory-entry-for (viewer-inventory viewer) block)))
    (format nil "~D · ~A" (mod (1+ index) 10)
            (if (and (viewer-creative-p viewer)
                     (not (luvcraft:block-kind-carried-p block))) "∞"
                (if entry (luvcraft:block-inventory-entry-quantity entry) 0)))))

(loop for key in '(:1 :2 :3 :4 :5 :6 :7 :8 :9 :0) for slot from 0
      do (let ((slot slot))
           (clim:add-keystroke-to-command-table
            'luvcraft-world (list key) :function
            (lambda (gesture numeric-argument)
              (declare (ignore gesture numeric-argument))
              (list 'com-select-hotbar-slot slot))
            :errorp nil)))

;;; The workbench and its instruments.

(defmethod luv.workbench:workbench-application-name ((viewer luvcraft-viewer))
  "Luvcraft")

(defmethod mcluv:status-bar-application-name ((viewer luvcraft-viewer))
  "Luvcraft")

(defmethod mcluv:command-menu-tables-for ((viewer luvcraft-viewer))
  '(render:luft-window luvcraft-world))

(defmethod mcluv:keymap-legend-sections-for ((viewer luvcraft-viewer))
  `(("Building" . luvcraft-world)
    ("Moving" . render:luft-play)
    ("Pointer" . (("look around" "Mouse")
                  ("mine the block you face" "Left click")
                  ("place against it" "Right click")
                  ("pick its block" "Middle click")
                  ("choose a block" "Wheel")))
    ("Any time" . render:luft-window)))

(defmethod mcluv:keymap-legend-command-label
    ((viewer luvcraft-viewer) (name (eql 'com-select-hotbar-slot)) arguments table)
  (declare (ignore arguments table))
  "choose a block")

(defmethod mcluv:palette-hint ((viewer luvcraft-viewer))
  (cond ((viewer-inventory-page viewer)
         (format nil "Inventory ~D · N next"
                 (1+ (viewer-inventory-page viewer))))
        ((eq (render:viewer-last-edit-status viewer) :empty) "No blocks left · mine to collect")
        (t (format nil "~A · I inventory · F6 creative"
                   (if (viewer-creative-p viewer) "Creative" "Survival")))))

;;; Saves: Luvcraft's own format, so Luvcraft reads what this game wrote.

(defun streaming-source (viewer)
  (terrain:streaming-scene-source (render:viewer-source viewer)))

(defmethod render:viewer-world-description ((viewer luvcraft-viewer))
  (let ((player (render:viewer-player viewer))
        (camera (render:viewer-camera viewer)))
    (game:luvcraft-save-description
     (streaming-source viewer) (luvcraft-viewer-world viewer)
     :resume (inventory-resume viewer)
     :inhabitants (viewer-inhabitant-descriptions viewer)
     :position (and player
                    (let ((position (render:body-position
                                     (render:character-body player))))
                      (list (vec3:vec3-x position) (vec3:vec3-y position)
                            (vec3:vec3-z position))))
     :yaw (render:camera-yaw camera)
     :pitch (render:camera-pitch camera)
     :selected-block (selected-block viewer (render:viewer-edit-material viewer))
     :selected (render:viewer-edit-material viewer))))

(defmethod render:write-viewer-world-description
    ((viewer luvcraft-viewer) description pathname)
  (luvcraft::write-luvcraft-save-description description pathname))

(defun play (&rest viewer-arguments
             &key (pathname (luvcraft::default-luvcraft-world-pathname)
                            pathname-p)
               seed (relief :alpine)
               (world-height luvcraft::*little-world-height*)
             &allow-other-keys)
  "Play Luvcraft on Luft and return its viewer.

The world is the Luvcraft save at PATHNAME when it exists and no SEED is
given, else a fresh little world of SEED, RELIEF, and WORLD-HEIGHT.  Edits
and the player's pose are written back to PATHNAME in Luvcraft's own save
format as the game plays and when it stops; a fresh SEED writes only to a
PATHNAME given explicitly, never over the player's save.  Other keys go to
LUFT.RENDER:START-GAME-HOST."
  (let ((loaded-p (and pathname (null seed) (probe-file pathname))))
    (multiple-value-bind (world resume inhabitants)
        (if loaded-p
            (luvcraft:read-luvcraft-save pathname)
            (luvcraft:make-empty-little-block-world
             :seed (or seed 1) :relief relief :chunk-height world-height))
      (let ((source (game:luvcraft-world-luft-source world)))
        (multiple-value-bind (position yaw pitch)
            (and resume (game:luvcraft-resume-luft-pose source resume))
          (apply #'render:start-game-host
                 :solid (game:make-luvcraft-streaming-scene
                         source :mesh-preparer render:+render-population-preparer+)
                 :title *title*
                 :world-pathname (and (or loaded-p pathname-p
                                          (and pathname (null seed)))
                                      pathname)
                 :saved-world (and position
                                   (list :player position :yaw yaw :pitch pitch))
                 :frame-class 'luvcraft-viewer
                 :frame-initargs
                 (list :luvcraft-world world :resume resume
                       :inhabitants inhabitants
                       :hotbar (mapcar (lambda (name)
                                         (game:block-placement
                                          (luvcraft:block-kind-named name)))
                                       *starting-hotbar*))
                 (alexandria:remove-from-plist
                  viewer-arguments :pathname :seed :relief :world-height)))))))
