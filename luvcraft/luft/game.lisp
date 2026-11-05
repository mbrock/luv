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
                       :documentation "The companion this view addresses with T, Y, and N.")
   (previous-animal-figures :initform (make-hash-table :test #'eq)
                            :reader viewer-previous-animal-figures
                            :documentation "Each turtle's boxes as this view last drew them, for motion.")
   (previous-ball-centers :initform (make-hash-table) :reader viewer-previous-ball-centers
                          :documentation "Each ball's centre as this view last drew it, for motion.")
   (particle-sequence :initform nil :accessor viewer-particle-sequence
                      :documentation "The journal sequence this view's particles are derived up to,
from the head when it opened, or NIL for the watermark."))
  (:command-table (luvcraft-viewer
                   :inherit-from (luvcraft-window render:luft-window-release
                                  luvcraft-world luvcraft-world-release)
                   :inherit-menu t))
  (:menu-bar nil))

(defmethod initialize-instance :around
    ((viewer luvcraft-viewer) &rest initargs
     &key world local-player source simulation player luvcraft-world resume record inhabitants
       (hotbar (starting-hotbar-placements)) sky-origin-offset
     &allow-other-keys)
  ;; A viewer always views a world. Without one, LUVCRAFT-WORLD, RECORD (or
  ;; format 1's RESUME), INHABITANTS, and HOTBAR describe a fresh world and
  ;; its only player.
  (let* ((world (or world
                    (make-luvcraft-world
                     :block-world luvcraft-world :inhabitants inhabitants
                     :sky-origin-offset sky-origin-offset
                     :simulation (or simulation
                                     (terrain:make-world-simulation
                                      (or source (error "A Luvcraft viewer needs a scene.")))))))
         (local-player (or local-player
                           (make-luvcraft-player :record (or record (resume-player-record resume))
                                                 :hotbar hotbar))))
    (when player
      (setf (player-character local-player) player))
    (add-world-player world local-player)
    (apply #'call-next-method viewer :world world :local-player local-player
           :simulation (world-simulation world) initargs)))

(defmethod initialize-instance :after ((viewer luvcraft-viewer) &key)
  ;; After the host's own method, which installed the player's character.
  (setf (world-editor (viewer-world viewer))
        (lambda (cell placement) (render:apply-viewer-world-edit viewer cell placement)))
  (pushnew viewer (world-views (viewer-world viewer)))
  ;; Particles burst for what happens from now on: a reloaded journal's
  ;; tail is history, not news.
  (let ((scene (world-scene (viewer-world viewer))))
    (when (typep scene 'terrain:streaming-scene)
      (setf (viewer-particle-sequence viewer)
            (terrain:world-journal-head (terrain:streaming-scene-journal scene)))))
  (when (render:viewer-canvas viewer)
    (render:add-viewer-overlay viewer (make-instance 'target-marker))
    (attach-player-instruments viewer)
    (restore-player-kit (viewer-local-player viewer)))
  (install-luvcraft-motor (render:viewer-player viewer)))

(defmethod (setf render:viewer-player) :after (character (viewer luvcraft-viewer))
  (install-luvcraft-motor character)
  (setf (player-character (viewer-local-player viewer)) character))

(defmethod render:apply-viewer-input-frame ((viewer luvcraft-viewer) frame)
  (steer-luvcraft-player (viewer-local-player viewer) frame))

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
  (let ((session (world-session (viewer-world viewer)))
        (camera (render:viewer-camera viewer)))
    (when session
      (unless (session-viewer session)
        (setf (session-viewer session) viewer))
      (advance-world-session session seconds :yaw (render:camera-yaw camera)
                                             :pitch (render:camera-pitch camera))))
  (advertise-viewer-session viewer)
  (remember-viewer-poses viewer)
  (advance-world-populations (viewer-world viewer) seconds)
  (derive-viewer-particles viewer)
  (luvcraft::advance-block-particles (viewer-particles viewer) seconds)
  (advance-viewer-companions viewer seconds))

(defmethod render:viewer-figures ((viewer luvcraft-viewer))
  (append (call-next-method) (remote-player-figures viewer)
          (animal-figure-list viewer)
          (ball-figures viewer) (particle-figures viewer)))

(defmethod render:viewer-world-actor ((viewer luvcraft-viewer))
  ;; Undo is per player: each takes back only its own edits.
  (world-edit-actor (viewer-local-player viewer)))

(defmethod render:apply-viewer-world-edit :around
    ((viewer luvcraft-viewer) cell placement)
  ;; The local player edits the world; the host's own edit, which schedules
  ;; meshing, is the scene edit.
  ;; Particles follow from the journal's confirmed events, whoever made them.
  (let* ((player (or *world-edit-player* (viewer-local-player viewer)))
         (cause render::*viewer-world-edit-cause*)
         ;; Taken before the edit: mining puts what it mined in hand.
         (block (and placement (eq cause :edit) (selected-block player placement))))
    (multiple-value-bind (edit status)
        (player-edit-world-cell (viewer-world viewer) player cell placement
                                :scene-edit (lambda (block) (call-next-method viewer cell block)))
      (setf (render:viewer-last-edit-status viewer) status)
      (let ((tape (render:viewer-input-tape viewer)))
        ;; The local player's tape replays its edits as soak.lisp's actions do.
        (when (and tape (null *world-edit-player*) (or edit (eq status :requested)))
          (terrain:record-input-action
           tape (if (eq cause :edit)
                    (list :edit (luft:site-x cell) (luft:site-y cell) (luft:site-z cell)
                          (luvcraft::block-save-description block))
                    (list cause)))))
      (values edit status))))

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

(defun start-luvcraft-view (world local-player viewer-arguments &key world-pathname saved-world
                                                                     terrain-bevels-p)
  "Open a view of WORLD through LOCAL-PLAYER's eyes and return it."
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
         :world-pathname world-pathname
         :saved-world saved-world
         :frame-class 'luvcraft-viewer
         :frame-initargs (list :world world :local-player local-player)
         (alexandria:remove-from-plist
          viewer-arguments :pathname :seed :relief :world-height :terrain-bevels-p
          :host :join :port :player-id :name)))

(defun play (&rest viewer-arguments
             &key (pathname (luvcraft::default-luvcraft-world-pathname)
                            pathname-p)
               seed (relief :alpine)
               (world-height luvcraft::*little-world-height*)
               (terrain-bevels-p nil)
               host join (port wire:+default-port+)
               (player-id (default-player-identity)) (name (default-player-name))
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

HOST true opens the world to guests on PORT (TCP and UDP 47475 by default)
and announces it on the LAN.  JOIN, \"host[:port]\", plays the world that
host serves instead: nothing is read from or written to this machine's
saves.  Either way the local player is PLAYER-ID, called NAME.  Other keys
go to LUFT.RENDER:START-GAME-HOST."
  (when join
    (return-from play
      (multiple-value-bind (world player session pose)
          (join-luvcraft-world join :identity player-id :name name
                                    :scene-initargs
                                    (list :residency-radius 3
                                          :mesh-preparer render:+render-population-preparer+))
        (closing-session-unless-viewed
         session
         (lambda ()
           (start-luvcraft-view
            world player viewer-arguments
            :terrain-bevels-p terrain-bevels-p
            :saved-world (list :player (coerce (wire:pose-position pose) 'list)
                               :yaw (coerce (wire:pose-yaw pose) 'single-float)
                               :pitch (coerce (wire:pose-pitch pose) 'single-float)
                               :flying-p (wire:pose-flying pose))))))))
  (let ((loaded-p (and pathname (null seed) (probe-file pathname))))
    (multiple-value-bind (block-world resume inhabitants journal)
        (if loaded-p
            (luvcraft:read-luvcraft-save pathname)
            (luvcraft:make-empty-little-block-world
             :seed (or seed 1) :relief relief :chunk-height world-height))
      (multiple-value-bind (record others)
          (adopt-saved-players (getf journal :players) resume player-id)
        (let* ((source (game:luvcraft-world-luft-source block-world :journal journal))
               (world (make-luvcraft-world
                       :block-world block-world :inhabitants inhabitants :player-records others
                       :scene (game:make-luvcraft-streaming-scene
                               source :residency-radius 3
                               :mesh-preparer render:+render-population-preparer+)))
               (player (make-luvcraft-player :record record :id player-id :name name)))
          (multiple-value-bind (position yaw pitch flying-p)
              (and record (game:luvcraft-resume-luft-pose source record))
            (closing-session-unless-viewed
             (and host (host-luvcraft-world world player :port port :name name
                                                         :announce (format nil "~A's world" name)))
             (lambda ()
               (start-luvcraft-view
                world player viewer-arguments
                :terrain-bevels-p terrain-bevels-p
                :world-pathname (and (or loaded-p pathname-p
                                         (and pathname (null seed)))
                                     pathname)
                :saved-world (and position
                                  (list :player position :yaw yaw :pitch pitch
                                        :flying-p flying-p)))))))))))

(defun closing-session-unless-viewed (session start-view)
  "Call START-VIEW and return its viewer.  If it does not return, as when the
canvas cannot open, close SESSION, so its ports and worker do not outlive
a game that never started."
  (let ((viewer nil))
    (unwind-protect (setf viewer (funcall start-view))
      (when (and session (null viewer))
        (close-world-session session)))))

(defmethod render::quiesce-viewer-services :after ((viewer luvcraft-viewer))
  ;; The view has let go of its companions' conversations (agents.lisp).  The
  ;; world goes with its last view: its companions retire, and guests are
  ;; told goodbye.
  (forget-view-chores viewer)
  (let ((world (viewer-world viewer)))
    (setf (world-views world) (remove viewer (world-views world)))
    (unless (world-views world)
      (shutdown-luvcraft-world world))))
