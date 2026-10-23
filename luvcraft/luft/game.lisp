;;;; Luvcraft on Luft as a game of its own: a viewer frame with Luvcraft's
;;;; title, keys, hotbar, and save, over the Luft renderer and studio.
;;;;
;;;; The viewer brings walking, flight, first person, the workbench, and
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

(clim:define-application-frame luvcraft-viewer (render:viewer)
  ((luvcraft-world :initarg :luvcraft-world :reader luvcraft-viewer-world
                   :documentation "Luvcraft's own world: its source and edits as read.")
   (resume :initarg :resume :initform nil :reader luvcraft-viewer-resume)
   (inhabitants :initarg :inhabitants :initform nil
                :reader luvcraft-viewer-inhabitants)
   (hotbar :initarg :hotbar :accessor luvcraft-viewer-hotbar))
  (:command-table (luvcraft-viewer
                   :inherit-from (render:luft-window render:luft-window-release
                                  luvcraft-world luvcraft-world-release)
                   :inherit-menu t))
  (:panes
   (inspector :application
              :display-function 'luft.render::display-site-inspector
              :scroll-bars nil))
  (:layouts (default (clim:horizontally (:width 372 :height 306) inspector)))
  (:menu-bar nil))

(defmethod initialize-instance :after ((viewer luvcraft-viewer) &key)
  ;; After the viewer's own method, which chose the vocabulary's first member.
  (let ((selected (getf (luvcraft-viewer-resume viewer) :selected-block)))
    (setf (render:viewer-edit-material viewer)
          (or (and selected
                   (ignore-errors
                    (game:block-placement
                     (luvcraft::restore-block-value selected))))
              (first (luvcraft-viewer-hotbar viewer))))
    (put-on-hotbar viewer (render:viewer-edit-material viewer))))

(defun put-on-hotbar (viewer placement)
  "Select PLACEMENT, putting it in the selected slot when it is not carried."
  (let* ((hotbar (luvcraft-viewer-hotbar viewer))
         (selected (position (render:viewer-edit-material viewer) hotbar)))
    (unless (member placement hotbar)
      (setf (nth (or selected 0) hotbar) placement)))
  (setf (render:viewer-edit-material viewer) placement))

(defmethod render:viewer-hotbar ((viewer luvcraft-viewer))
  (luvcraft-viewer-hotbar viewer))

(defmethod render:viewer-pick-material ((viewer luvcraft-viewer) placement)
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
  (render:select-viewer-material (render:viewer-command-viewer) slot))

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
  (if (and (typep (render:viewer-mode viewer) 'render:first-person-mode)
           (not (render:viewer-pointer-captured-p viewer)))
      "Click to play  ·  F1 or H for keys"
      (call-next-method)))

;;; Saves: Luvcraft's own format, so Luvcraft reads what this game wrote.

(defun streaming-source (viewer)
  (terrain:streaming-scene-source (render:viewer-source viewer)))

(defmethod render:viewer-world-description ((viewer luvcraft-viewer))
  (let ((player (render:viewer-player viewer))
        (camera (render:viewer-camera viewer)))
    (game:luvcraft-save-description
     (streaming-source viewer) (luvcraft-viewer-world viewer)
     :resume (luvcraft-viewer-resume viewer)
     :inhabitants (luvcraft-viewer-inhabitants viewer)
     :position (and player
                    (let ((position (render:body-position
                                     (render:character-body player))))
                      (list (vec3:vec3-x position) (vec3:vec3-y position)
                            (vec3:vec3-z position))))
     :yaw (render:camera-yaw camera)
     :pitch (render:camera-pitch camera)
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
LUFT.RENDER:START-VIEWER."
  (let ((loaded-p (and pathname (null seed) (probe-file pathname))))
    (multiple-value-bind (world resume inhabitants)
        (if loaded-p
            (luvcraft:read-luvcraft-save pathname)
            (luvcraft:make-empty-little-block-world
             :seed (or seed 1) :relief relief :chunk-height world-height))
      (let ((source (game:luvcraft-world-luft-source world)))
        (multiple-value-bind (position yaw pitch)
            (and resume (game:luvcraft-resume-luft-pose source resume))
          (apply #'render:start-viewer
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
