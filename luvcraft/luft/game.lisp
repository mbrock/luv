;;;; Luvcraft on Luft as a game of its own: a host frame with Luvcraft's
;;;; title, keys, hotbar, and save, over the Luft renderer and shared play.
;;;;
;;;; The host brings walking, flight, first person, the workbench, and
;;;; streaming; this file brings only what is Luvcraft's (#J3WZZS).

(in-package #:luvcraft.luft.game)

(defparameter *title*
  "Luvcraft — click to play · WASD walk · Space jump, twice to fly · left mine · right or E place · C pick · 1–0 blocks · F1 keys · Esc release")

;;; Luvcraft's in-world keys inherit the viewer's walking and tools; the
;;; atelier's own instruments (Tab terminal, B bevel, C construction lines,
;;; R reset, edit mode) are not part of the game.

(clim:define-command-table luvcraft-world
  :inherit-from (render:luft-play) :inherit-menu t)
(clim:define-command-table luvcraft-window
  :inherit-from (render:luft-window) :inherit-menu t)
(clim:define-command-table luvcraft-world-release
  :inherit-from (render:luft-play-release) :inherit-menu t)

;;; A view of one world through its local player's eyes (#32DUBY). The host
;;; owns the camera and controls; this frame adds overlays and particles, and
;;; refers to its world and local player for everything they own.
(clim:define-application-frame luvcraft-viewer (render:game-host)
  ((world :initarg :world :reader viewer-world)
   (local-player :initarg :local-player :reader viewer-local-player)
   (particles :initform (make-instance 'luvcraft::block-particle-system)
              :reader viewer-particles)
   (sky-light :initform (make-luvcraft-light) :reader viewer-sky-light)
   (body :initform (make-instance 'luvcraft::player-body) :reader viewer-body
         :documentation "The first-person hands, bob, and held item that this view draws.")
   (conversation-agent :initform nil :accessor viewer-conversation-agent
                       :documentation "The companion this view addresses with T, Y, and N."))
  (:command-table (luvcraft-viewer
                   :inherit-from (luvcraft-window render:luft-window-release
                                  luvcraft-world luvcraft-world-release)
                   :inherit-menu t))
  (:menu-bar nil))

(defmethod initialize-instance :around
    ((viewer luvcraft-viewer) &rest initargs
     &key world local-player source simulation player luvcraft-world resume inhabitants
       (hotbar (starting-hotbar-placements)) sky-origin-offset
     &allow-other-keys)
  ;; A viewer always views a world. Without one, LUVCRAFT-WORLD, RESUME,
  ;; INHABITANTS, and HOTBAR describe a fresh world and its only player.
  (let* ((world (or world
                    (make-luvcraft-world
                     :block-world luvcraft-world :inhabitants inhabitants
                     :sky-origin-offset sky-origin-offset
                     :simulation (or simulation
                                     (terrain:make-world-simulation
                                      (or source (error "A Luvcraft viewer needs a scene.")))))))
         (local-player (or local-player (make-luvcraft-player :resume resume :hotbar hotbar))))
    (when player
      (setf (player-character local-player) player))
    (add-world-player world local-player)
    (apply #'call-next-method viewer :world world :local-player local-player
           :simulation (world-simulation world) initargs)))

(defmethod initialize-instance :after ((viewer luvcraft-viewer) &key)
  ;; After the host's own method, which installed the player's character.
  (setf (world-editor (viewer-world viewer))
        (lambda (cell placement) (render:apply-viewer-world-edit viewer cell placement)))
  (when (render:viewer-canvas viewer)
    (render:add-viewer-overlay viewer (make-instance 'target-marker))
    (attach-player-instruments viewer)
    (restore-player-kit (viewer-local-player viewer)))
  (install-luvcraft-motor (render:viewer-player viewer)))

(defmethod (setf render:viewer-player) :after (character (viewer luvcraft-viewer))
  (install-luvcraft-motor character)
  (setf (player-character (viewer-local-player viewer)) character))

;;; The host's selection and hotbar are the local player's.

(defmethod render:viewer-edit-material ((viewer luvcraft-viewer))
  (player-placement (viewer-local-player viewer)))

(defmethod (setf render:viewer-edit-material) (placement (viewer luvcraft-viewer))
  (setf (player-placement (viewer-local-player viewer)) placement))

(defmethod render:viewer-hotbar ((viewer luvcraft-viewer))
  (player-hotbar (viewer-local-player viewer)))

(defmethod luvcraft:player-inventory ((viewer luvcraft-viewer))
  (luvcraft:player-inventory (viewer-local-player viewer)))

(defmethod luvcraft:player-selected-block ((viewer luvcraft-viewer))
  (luvcraft:player-selected-block (viewer-local-player viewer)))

(defmethod (setf luvcraft:player-selected-block) (block (viewer luvcraft-viewer))
  (setf (luvcraft:player-selected-block (viewer-local-player viewer)) block))

(defmethod luvcraft:player-creative-p ((viewer luvcraft-viewer))
  (luvcraft:player-creative-p (viewer-local-player viewer)))

(defmethod luvcraft:update-player-instruments ((viewer luvcraft-viewer))
  (luvcraft:update-player-instruments (viewer-local-player viewer)))

(defun toggle-creative-mode (viewer)
  (let ((player (viewer-local-player viewer)))
    (setf (luvcraft:player-creative-p player) (not (luvcraft:player-creative-p player)))))

;;; The world's time and edits, as this view sees them.

(defmethod render::advance-viewer-sky ((viewer luvcraft-viewer) seconds)
  (advance-world-sky (viewer-world viewer) seconds))

(defmethod render:advance-viewer-game ((viewer luvcraft-viewer) seconds)
  ;; The host has stepped the world's characters; the rest of it follows.
  (advance-world-populations (viewer-world viewer) seconds)
  (luvcraft::advance-block-particles (viewer-particles viewer) seconds)
  (advance-viewer-companions viewer seconds))

(defmethod render:viewer-figures ((viewer luvcraft-viewer))
  (append (call-next-method) (animal-figure-list viewer)
          (ball-figures viewer) (particle-figures viewer)))

(defmethod render:viewer-world-actor ((viewer luvcraft-viewer))
  ;; Undo is per player: each takes back only its own edits.
  (world-edit-actor (viewer-local-player viewer)))

(defmethod render:apply-viewer-world-edit :around
    ((viewer luvcraft-viewer) cell placement)
  ;; The local player edits the world; the host's own edit, which schedules
  ;; meshing, is the scene edit.
  (multiple-value-bind (edit status mined)
      (player-edit-world-cell (viewer-world viewer) (viewer-local-player viewer) cell placement
                       :scene-edit (lambda () (call-next-method)))
    (setf (render:viewer-last-edit-status viewer) status)
    (when mined
      (luvcraft::smash-block-particles
       (viewer-particles viewer) mined
       (apply #'luvcraft.world:make-world-coordinate
              (game:site-luvcraft-cell (streaming-source viewer) cell))))
    (values edit status)))

(defmethod luv:handle-canvas-event
    ((viewer luvcraft-viewer) canvas (event luv:canvas-pointer-wheel-event))
  (declare (ignore canvas))
  ;; Placement is a visual identity: several carried films share it. Cycle
  ;; the real blocks, as the old game does, rather than losing reel identity.
  (when (render::viewer-pointer-captured-p viewer)
    (luvcraft:cycle-luvcraft-block
     viewer (- (signum (round (luv:canvas-pointer-event-scroll-y event))))))
  nil)

(defmethod render:viewer-pick-material ((viewer luvcraft-viewer) placement)
  (let ((inspection (render::viewer-inspection viewer))
        (player (viewer-local-player viewer)))
    (setf (player-held-block player)
          (and inspection
               (world-authored-block (viewer-world viewer)
                                     (render::site-inspection-cell inspection))))
    (put-on-hotbar player placement))
  (setf (render:viewer-last-edit-status viewer) :selected))

(defmethod render:viewer-key-event-tables
    ((viewer luvcraft-viewer) (event luv:canvas-key-press-event))
  (values 'luvcraft-window 'luvcraft-world))

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
  (luvcraft:select-luvcraft-block (render:viewer-command-viewer) (1+ slot)))

(clim:define-command (com-inventory :command-table luvcraft-window
                                   :name "Inventory" :keystroke (:i)) ()
  (mcluv:toggle-player-inventory (render:viewer-command-viewer)))

(clim:define-command (com-creative :command-table luvcraft-window
                                 :name "Creative Mode" :keystroke (:f6)) ()
  (toggle-creative-mode (render:viewer-command-viewer)))

(defmethod mcluv:palette-visible-p ((viewer luvcraft-viewer))
  (declare (ignore viewer))
  nil)

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
  '(render:luft-window luvcraft-window render:luft-play luvcraft-world))

(defmethod mcluv:keymap-legend-sections-for ((viewer luvcraft-viewer))
  `(("Building" . luvcraft-world)
    ("Moving" . render:luft-play)
    ("Pointer" . (("look around" "Mouse")
                  ("mine the block you face" "Left click")
                  ("place against it" "Right click")
                  ("pick its block" "Middle click")
                  ("choose a block" "Wheel")))
    ("Inventory" . luvcraft-window)
    ("Any time" . render:luft-window)))

(clim:define-command (com-use-terminal :command-table luvcraft-world
                                      :name "Use Terminal" :keystroke (:tab)) ()
  (render::viewer-use-at-crosshair (render:viewer-command-viewer)))

(defmethod mcluv:keymap-legend-command-label
    ((viewer luvcraft-viewer) (name (eql 'com-select-hotbar-slot)) arguments table)
  (declare (ignore arguments table))
  "choose a block")

;;; Saves: the world's, with this view's player and look.

(defun streaming-source (viewer)
  (world-source (viewer-world viewer)))

(defmethod render:viewer-world-description ((viewer luvcraft-viewer))
  (let ((camera (render:viewer-camera viewer)))
    (luvcraft-world-save-description
     (viewer-world viewer) (viewer-local-player viewer)
     :yaw (render:camera-yaw camera) :pitch (render:camera-pitch camera))))

(defmethod render:write-viewer-world-description
    ((viewer luvcraft-viewer) description pathname)
  (luvcraft::write-luvcraft-save-description description pathname))

(defun play (&rest viewer-arguments
             &key (pathname (luvcraft::default-luvcraft-world-pathname)
                            pathname-p)
               seed (relief :alpine)
               (world-height luvcraft::*little-world-height*)
               (terrain-bevels-p nil)
             &allow-other-keys)
  "Play Luvcraft on Luft and return its viewer.

The world is the Luvcraft save at PATHNAME when it exists and no SEED is
given, else a fresh little world of SEED, RELIEF, and WORLD-HEIGHT.  Edits
and the player's pose are written back to PATHNAME in Luvcraft's own save
format as the game plays and when it stops; a fresh SEED writes only to a
PATHNAME given explicitly, never over the player's save. Terrain starts with
plain cube faces; TERRAIN-BEVELS-P opts in, and the metabar switches it live.
The resident window spans seven 64-cell chunks per side; the streaming
metabar can adjust its radius without changing the world's collision rules.
Other keys go to LUFT.RENDER:START-GAME-HOST."
  (let ((loaded-p (and pathname (null seed) (probe-file pathname))))
    (multiple-value-bind (block-world resume inhabitants)
        (if loaded-p
            (luvcraft:read-luvcraft-save pathname)
            (luvcraft:make-empty-little-block-world
             :seed (or seed 1) :relief relief :chunk-height world-height))
      (let* ((source (game:luvcraft-world-luft-source block-world))
             (world (make-luvcraft-world
                     :block-world block-world :inhabitants inhabitants
                     :scene (game:make-luvcraft-streaming-scene
                             source :residency-radius 3
                             :mesh-preparer render:+render-population-preparer+))))
        (multiple-value-bind (position yaw pitch flying-p)
            (and resume (game:luvcraft-resume-luft-pose source resume))
          (apply #'render:start-game-host
                 :solid (world-scene world)
                 :title *title*
                 :terrain-bevels-p terrain-bevels-p
                 :sky-factory 'make-luvcraft-sky-drawing
                 :terrain-factory 'make-luvcraft-terrain-drawing
                 :torch-factory 'make-luvcraft-torch-drawing
                 :finishing-factory 'make-luvcraft-image-finishing
                 :exposure-factory 'make-luvcraft-exposure
                 :figure-factory 'make-creature-figure-drawing
                 :world-pathname (and (or loaded-p pathname-p
                                          (and pathname (null seed)))
                                      pathname)
                 :saved-world (and position
                                   (list :player position :yaw yaw :pitch pitch
                                         :flying-p flying-p))
                 :frame-class 'luvcraft-viewer
                 :frame-initargs
                 (list :world world :local-player (make-luvcraft-player :resume resume))
                 (alexandria:remove-from-plist
                  viewer-arguments :pathname :seed :relief :world-height
                  :terrain-bevels-p)))))))
