;;;; The game is a world, its players, and their views (#32DUBY).
;;;;
;;;; A LUVCRAFT-WORLD runs without a window: Luvcraft's block world and its
;;;; streaming scene, the simulation of every character in it, its turtles,
;;;; balls, springs, and companions, its sky, and a table of its players.  A
;;;; LUVCRAFT-PLAYER is one person in it: a character, an inventory and hotbar,
;;;; and what they hold.  The LUVCRAFT-VIEWER (game.lisp) is one view of a
;;;; world through its local player's eyes; its game-host protocols forward
;;;; here, so two worlds can run in one image.
(in-package #:luvcraft.luft.game)

(defparameter *starting-hotbar*
  '(:grass :dirt :planks :bricks :red-wool :yellow-wool :blue-wool
    :crystal :flowers :stone)
  "The blocks on the number row when a game starts, as Luvcraft's own
quickbar begins: things to build a house with, colours, and a lamp.")

(defun starting-hotbar-placements ()
  (mapcar (lambda (name) (game:block-placement (luvcraft:block-kind-named name)))
          *starting-hotbar*))

;;; A player.

(defclass luvcraft-player ()
  ((id :initarg :id :reader player-id
       :documentation "A stable identity, the key of the world's players table.")
   (name :initarg :name :reader player-name)
   (character :initarg :character :initform nil :accessor player-character
              :documentation "The walking character whose body the world simulates.")
   (resume :initarg :resume :initform nil :reader player-resume
           :documentation "The player's saved record, with fields this game does not read.")
   (inventory :initarg :inventory :reader luvcraft:player-inventory)
   (hotbar :initarg :hotbar :accessor player-hotbar
           :documentation "The placements on the number row, published from the inventory.")
   (placement :initarg :placement :initform nil :accessor player-placement
              :documentation "The material placement in hand.")
   (held-block :initarg :held-block :initform nil :accessor player-held-block
               :documentation "The exact block in hand when several share its placement, as films do.")
   (creative-p :initarg :creative-p :initform nil :accessor luvcraft:player-creative-p)
   (rider-intent :initform (luvcraft:make-movement-intent) :reader player-rider-intent
                 :documentation "Riding steers the original game's intent, not the walking controller.")
   (ball-hand :initform (make-instance 'luvcraft::ball) :reader player-ball-hand)
   (ball-in-hand-p :initform nil :accessor player-ball-in-hand-p))
  (:documentation "One person in a Luvcraft world: their character, stock, and hands."))

(defmethod print-object ((player luvcraft-player) stream)
  (print-unreadable-object (player stream :type t :identity t)
    (format stream "~S" (player-id player))))

(defun restore-inventory (resume)
  (let ((inventory (luvcraft:make-block-inventory
                    :blocks (remove-duplicates
                             (append (mapcar #'luvcraft:block-kind-named *starting-hotbar*)
                                     (luvcraft:placeable-block-kinds))
                             :from-end t))))
    ;; Materials are unlimited, as in the original game; only carried blocks
    ;; keep counts, so stock saved while every material was finite is dropped.
    (if (member :inventory resume)
        (setf (luvcraft:block-inventory-entries inventory)
              (loop for (description quantity) in (getf resume :inventory)
                    for block = (luvcraft::restore-block-value description)
                    for count = (and (luvcraft:block-kind-carried-p block) (or quantity 0))
                    do (check-type count (or null (integer 0)))
                    collect (make-instance 'luvcraft:block-inventory-entry
                                           :block block :quantity count)))
        (dolist (description (getf resume :carried))
          (luvcraft:add-block-to-inventory
           inventory (luvcraft::restore-block-value description))))
    inventory))

(defun make-luvcraft-player (&key (id "local") (name id) resume
                               (hotbar (starting-hotbar-placements)) character)
  "A player restored from RESUME, Luvcraft's saved player record, or a new one
whose number row starts as HOTBAR."
  (let* ((inventory (restore-inventory resume))
         (selected (getf resume :selected-block))
         (player (make-instance
                  'luvcraft-player
                  :id id :name name :resume resume :character character
                  :inventory inventory :hotbar hotbar
                  :creative-p (getf resume :creative-p)
                  :held-block
                  (and selected
                       (find selected (luvcraft:block-inventory-blocks inventory)
                             :key #'luvcraft::block-save-description :test #'equal)))))
    (setf (player-placement player)
          (or (and selected
                   (ignore-errors
                    (game:block-placement (luvcraft::restore-block-value selected))))
              (first hotbar)))
    (put-on-hotbar player (player-placement player))
    (restore-player-kit player)
    player))

(defun restore-player-kit (player)
  "Give PLAYER its saved bar, inventory mode, kit, and selected block.
Preferences choose a new player's kit; an actual world resume keeps its own
bar, mode, stock, and selected per-instance block, so a view that applies
preferences restores the kit again after them."
  (let ((resume (player-resume player))
        (inventory (luvcraft:player-inventory player)))
    (when (getf resume :quickbar)
      (luvcraft:arrange-luvcraft-quickbar player (getf resume :quickbar)))
    (when (member :simple-inventory-p resume)
      (setf (luvcraft::block-inventory-simple-p inventory)
            (getf resume :simple-inventory-p)
            (luvcraft::block-inventory-kit inventory)
            (mapcar #'luvcraft::restore-block-value (getf resume :building-kit))))
    (let ((selected (getf resume :selected-block)))
      (when selected
        (setf (luvcraft:player-selected-block player)
              (luvcraft::restore-block-value selected))))
    (luvcraft:update-player-instruments player)
    player))

(defun selected-block (player placement)
  "The block PLAYER means by PLACEMENT: the one in hand when it shares it."
  (let ((held (player-held-block player)))
    (if (and held (eq placement (game:block-placement held))) held
        (or (game::luvcraft-block-for-placement placement)
            (find placement (luvcraft:block-inventory-blocks (luvcraft:player-inventory player))
                  :key #'game:block-placement)))))

(defun put-on-hotbar (player placement)
  "Select PLACEMENT, replacing the selected slot when it is not on the bar."
  (let ((block (selected-block player placement)))
    (when block
      (luvcraft:add-block-to-inventory (luvcraft:player-inventory player) block 0)
      (luvcraft:put-luvcraft-block-on-quickbar player block))))

(defmethod luvcraft:player-selected-block ((player luvcraft-player))
  (selected-block player (player-placement player)))

(defmethod (setf luvcraft:player-selected-block) (block (player luvcraft-player))
  (setf (player-held-block player) block
        (player-placement player) (game:block-placement block))
  block)

(defmethod luvcraft:update-player-instruments ((player luvcraft-player))
  (setf (player-hotbar player)
        (mapcar #'game:block-placement
                (luvcraft:block-inventory-quickbar-blocks (luvcraft:player-inventory player)))))

(defun player-save-record (player)
  "PLAYER's record as Luvcraft's save resume, keeping the fields it did not read."
  (let ((resume (copy-list (player-resume player)))
        (inventory (luvcraft:player-inventory player)))
    (setf (getf resume :inventory)
          (loop for entry in (luvcraft:block-inventory-entries inventory)
                collect (list (luvcraft::block-save-description
                               (luvcraft:block-inventory-entry-block entry))
                              (luvcraft:block-inventory-entry-quantity entry)))
          (getf resume :carried)
          (mapcar #'luvcraft::block-save-description
                  (luvcraft:block-inventory-carried-blocks inventory))
          (getf resume :creative-p) (luvcraft:player-creative-p player)
          (getf resume :simple-inventory-p) (luvcraft::block-inventory-simple-p inventory)
          (getf resume :building-kit)
          (mapcar #'luvcraft::block-save-description
                  (luvcraft::block-inventory-kit inventory))
          (getf resume :selected-block)
          (luvcraft::block-save-description (luvcraft:player-selected-block player))
          (getf resume :quickbar)
          (mapcar #'terrain:material-placement-name (player-hotbar player)))
    resume))

;;; A world.

;;; The class shares its name with the LUVCRAFT-WORLD command table, which
;;; lives in CLIM's separate command-table namespace.
(defclass luvcraft-world ()
  ((block-world :initarg :block-world :initform nil :reader world-block-world
                :documentation "Luvcraft's own world: its recipe and block edits as Luvcraft reads them.")
   (simulation :initarg :simulation :reader world-simulation
               :documentation "The streaming scene's simulation of every character in it.")
   (players :initform nil :accessor world-players
            :documentation "The players table, in the order they joined; ids are stable keys.")
   (editor :initform nil :accessor world-editor
           :documentation "A view's edit function of CELL and PLACEMENT, or NIL to edit the scene.
A view's edit also schedules meshing and makes feedback, so a world's own
actors, such as companions, edit through it when one is open.")
   ;; The sky.
   (sky-clock :initform (make-instance 'luvcraft::sky-clock) :reader world-sky-clock)
   (sky-profile :initform (luvcraft::make-default-sky-profile) :reader world-sky-profile)
   (sky-elapsed :initform 0.0 :accessor world-sky-elapsed)
   (sky-origin-offset :initarg :sky-origin-offset :reader world-sky-origin-offset)
   (sun-orbit-tilt :initform 0.44 :accessor world-sun-orbit-tilt)
   ;; Inhabitants, as saved and as they live.
   (saved-inhabitants :initarg :inhabitants :initform nil :accessor world-saved-inhabitants)
   (animals :initform nil :accessor world-animals)
   (animal-count :initform 6 :accessor world-animal-count)
   (animal-previous-figures :initform (make-hash-table :test #'eq)
                            :reader world-animal-previous-figures)
   (animals-initialized-p :initform nil :accessor world-animals-initialized-p)
   (agents :initform nil :accessor world-agents)
   (agents-initialized-p :initform nil :accessor world-agents-initialized-p)
   ;; Balls, and the springs that throw them.
   (physics :initform nil :accessor world-physics)
   (physics-clock :initform 0d0 :accessor world-physics-clock)
   (springs :initform nil :accessor world-springs)
   (springs-initialized-p :initform nil :accessor world-springs-initialized-p)
   (ball-previous-centers :initform (make-hash-table) :reader world-ball-previous-centers))
  (:documentation "A Luvcraft world and everything that lives in it, without a window."))

(defun make-luvcraft-world (&key block-world scene
                              (simulation (terrain:make-world-simulation scene))
                              inhabitants sky-origin-offset)
  "A world playing BLOCK-WORLD on SCENE, with its saved INHABITANTS."
  (make-instance 'luvcraft-world
                 :block-world block-world :simulation simulation :inhabitants inhabitants
                 :sky-origin-offset
                 (or sky-origin-offset
                     (let ((scene (terrain:simulation-source simulation)))
                       (if (and (typep scene 'terrain:streaming-scene)
                                (typep (terrain:streaming-scene-source scene)
                                       'game:luvcraft-world-source))
                           (game:luvcraft-world-source-offset
                            (terrain:streaming-scene-source scene))
                           0.0)))))

(defun world-scene (world)
  (terrain:simulation-source (world-simulation world)))

(defun world-source (world)
  (terrain:streaming-scene-source (world-scene world)))

(defun cell-resident-p (world x y)
  "Whether the chunk under X, Y is resident in WORLD: elsewhere is unknown, not air."
  (nth-value 1 (gethash (luft:chunk-key-at x y)
                        (terrain:streaming-scene-store (world-scene world)))))

(defun find-world-player (world id)
  (find id (world-players world) :key #'player-id :test #'equal))

(defun world-owner (world)
  "The player WORLD's companions serve: the first to join, its host."
  (first (world-players world)))

(defun add-world-player (world player)
  "Admit PLAYER under its id and return it; its character joins WORLD's simulation."
  (let ((present (find-world-player world (player-id player))))
    (when (and present (not (eq present player)))
      (error "~S already has a player ~S." world (player-id player))))
  (unless (find-world-player world (player-id player))
    (setf (world-players world) (append (world-players world) (list player))))
  (when (player-character player)
    ;; Every player walks with Luvcraft's own motor, with or without a view.
    (install-luvcraft-motor (player-character player))
    (terrain:add-simulation-character (world-simulation world) (player-character player)))
  player)

(defun remove-world-player (world player)
  "Let PLAYER leave WORLD, taking its character out of the simulation."
  (setf (world-players world) (remove player (world-players world)))
  (when (player-character player)
    (terrain:remove-simulation-character (world-simulation world) (player-character player)))
  player)

(defun player-ground-resident-p (world player)
  "Whether PLAYER's character stands on a resident chunk of WORLD's scene."
  (let ((character (player-character player)))
    (and character
         (let ((p (terrain:body-position (terrain:character-body character))))
           (cell-resident-p world (floor (vec3:vec3-x p)) (floor (vec3:vec3-y p)))))))

;;; Time.

(defun advance-world-sky (world seconds)
  (luvcraft::advance-sky-clock (world-sky-clock world) seconds)
  (incf (world-sky-elapsed world) seconds))

(defun advance-world-populations (world seconds)
  "Advance everything living in WORLD but its characters' simulation: turtles,
balls and springs, then companions.  A game host steps the simulation itself,
under its own residency and focus rules, before this."
  (advance-world-animals world seconds)
  (advance-world-balls world seconds)
  (advance-world-companions world seconds))

(defun advance-luvcraft-world (world seconds)
  "Advance a headless WORLD by SECONDS in a view's order: sky, characters, then populations."
  (advance-world-sky world seconds)
  (terrain:advance-world-simulation (world-simulation world) seconds)
  (advance-world-populations world seconds)
  world)

;;; Edits.

;;; Luvcraft's own overlay of authored blocks, kept beside Luft's edits so a
;;; film keeps its identity: Luft's palette is deliberately only a visual
;;; identity, and all films share one reel.  These two are its only readers
;;; and writers here, so the overlay can become a view of the journal.

(defun world-authored-block (world cell)
  "The block a player or companion last authored at CELL, or NIL."
  (values (gethash (game:site-luvcraft-cell (world-source world) cell)
                   (luvcraft::block-edit-overlay-entries
                    (luvcraft:little-world-source-edits
                     (luvcraft:block-world-source (world-block-world world)))))))

(defun record-world-authored-block (world cell block)
  "Record BLOCK, or NIL for air, as authored at CELL."
  (apply #'luvcraft::record-block-edit
         (luvcraft:little-world-source-edits
          (luvcraft:block-world-source (world-block-world world)))
         block (game:site-luvcraft-cell (world-source world) cell)))

(defvar *world-edit-actor* nil
  "The journal actor of the edit in progress when it is not the editing
player's own, such as a companion building for its player.")

(defun world-edit-actor (player)
  "The principal whose journal event an edit by or for PLAYER is."
  (or *world-edit-actor* (player-id player)))

(defun apply-world-scene-edit (world player cell placement)
  "Sequence PLAYER's edit in WORLD's scene, as a headless world does: no view meshes it."
  (if (and placement
           (some (lambda (body) (terrain:body-overlaps-cell-p body cell))
                 (terrain:simulation-bodies (world-simulation world))))
      (values nil :occupied)
      (terrain:edit-world-cell (world-scene world) cell placement
                               :actor (world-edit-actor player))))

(defun player-edit-world-cell (world player cell placement
                               &key (scene-edit (lambda ()
                                                  (apply-world-scene-edit
                                                   world player cell placement))))
  "Change CELL to PLACEMENT, or mine it when PLACEMENT is NIL, as PLAYER.

Return the journal event, its status (:EDITED, or :QUEUED while a mesh
cohort is in flight), and the block mined, if any.  As in the original
game, materials are unlimited and only carried blocks, such as films, are
picked up and spent; PLAYER's stock changes only after SCENE-EDIT, a function
of no arguments returning an edit and status, accepts the change.  A headless
world edits its scene directly; a view passes its host's edit, which also
schedules meshing.  Undo and redo pass through this same boundary, so they
conserve stock too."
  (when (and placement (companion-occupies-cell-p world cell))
    (return-from player-edit-world-cell (values nil :occupied nil)))
  (let* ((inventory (luvcraft:player-inventory player))
         (block (and placement (selected-block player placement)))
         (entry (and block (luvcraft:block-inventory-entry-for inventory block)))
         (finite-p (and block (luvcraft:block-kind-carried-p block)))
         (old (terrain:scene-material-placement-at (world-scene world) cell))
         (original (world-authored-block world cell))
         (old-block (and old (or (and original
                                       (eq old (game:block-placement original)) original)
                                 (game::luvcraft-block-for-placement old)))))
    (when (and finite-p
               (or (null entry) (zerop (luvcraft:block-inventory-entry-quantity entry))))
      (return-from player-edit-world-cell (values nil :empty nil)))
    (multiple-value-bind (edit status) (funcall scene-edit)
      (when (and edit (world-physics world))
        (luvcraft::wake-physics-bodies-near
         (world-physics world) (- (luft:site-x cell) 1024) (luft:site-z cell)
         (- (luft:site-y cell) 1024) 2.0))
      ;; The overlay below is not committed yet. Discover springs from that
      ;; accepted-edit truth next tick, retaining unchanged throw counts.
      (setf (world-springs-initialized-p world) nil)
      (when edit
        (when finite-p
          (luvcraft:remove-block-from-inventory inventory block))
        (when (and old-block (luvcraft:block-kind-carried-p old-block))
          (luvcraft:add-block-to-inventory inventory old-block))
        (record-world-authored-block world cell block)
        (when (and old-block (null placement) (luvcraft:block-kind-carried-p old-block))
          (setf (player-held-block player) old-block)
          (put-on-hotbar player old)))
      (values edit status (and edit (null placement) old-block)))))

;;; Saves: format 2, a vocabulary table and columns (#Y3S82T); format 1 still
;;; loads.  A save holds one player's record.

(defun luvcraft-world-save-description (world player &key (yaw 0.0) (pitch 0.0))
  "WORLD's save with PLAYER as its player, looking along YAW and PITCH."
  (let ((character (player-character player)))
    (game:luvcraft-save-description
     (world-source world) (world-block-world world)
     :resume (player-save-record player)
     :inhabitants (world-inhabitant-descriptions world)
     :position (and character
                    (let ((position (terrain:body-position (terrain:character-body character))))
                      (list (vec3:vec3-x position) (vec3:vec3-y position)
                            (vec3:vec3-z position))))
     :yaw yaw :pitch pitch
     :flying-p (and character (terrain:character-flying-p character))
     :selected-block (luvcraft:player-selected-block player)
     :selected (player-placement player))))

(defun save-luvcraft-world (world player pathname &rest look)
  "Write WORLD's save with PLAYER as its player to PATHNAME, atomically."
  (luvcraft::write-luvcraft-save-description
   (apply #'luvcraft-world-save-description world player look) pathname)
  pathname)
