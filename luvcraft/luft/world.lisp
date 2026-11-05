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
   (name :initarg :name :accessor player-name)
   (character :initarg :character :initform nil :accessor player-character
              :documentation "The walking character whose body the world simulates.")
   (record :initarg :record :initform nil :accessor player-saved-record
           :documentation "The player's record in the players table (#Y3S82T), with
fields this game does not read.  A remote player's is the latest it sent.")
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
   (ball-in-hand-p :initform nil :accessor player-ball-in-hand-p)
   (track :initarg :track :initform nil :accessor player-track
          :documentation "A remote player's snapshot track (#VM5TN6); NIL for one simulated here."))
  (:documentation "One person in a Luvcraft world: their character, stock, and hands."))

(defmethod print-object ((player luvcraft-player) stream)
  (print-unreadable-object (player stream :type t :identity t)
    (format stream "~S" (player-id player))))

;;; A player's record (#Y3S82T) is a resume: a plist of pose fields in
;;; Luvcraft's coordinates (:PLAYER-POSITION, :LOOK, :FLYING), :CREATIVE-P,
;;; the ordered :INVENTORY of (BLOCK QUANTITY) whose first ten entries are
;;; the hotbar, :SIMPLE-INVENTORY-P and its :BUILDING-KIT, and the
;;; :SELECTED-BLOCK in hand, all as block identities.  The save writes the
;;; blocks as vocabulary offsets; format 1's resume names them symbolically.

(defun resume-player-record (resume)
  "Format 1's symbolic RESUME as a player record of block identities."
  (flet ((restore (description) (luvcraft::restore-block-value description)))
    (loop for (key value) on resume by #'cddr
          collect key
          collect (case key
                    (:inventory (loop for (description quantity) in value
                                      collect (list (restore description) quantity)))
                    ((:carried :building-kit) (mapcar #'restore value))
                    (:selected-block (and value (restore value)))
                    (t value)))))

(defun restore-inventory (record)
  (let ((inventory (luvcraft:make-block-inventory
                    :blocks (remove-duplicates
                             (append (mapcar #'luvcraft:block-kind-named *starting-hotbar*)
                                     (luvcraft:placeable-block-kinds))
                             :from-end t))))
    ;; Materials are unlimited, as in the original game; only carried blocks
    ;; keep counts, so stock saved while every material was finite is dropped.
    (if (getf record :inventory)
        (setf (luvcraft:block-inventory-entries inventory)
              (loop for (block quantity) in (getf record :inventory)
                    for count = (and (luvcraft:block-kind-carried-p block) (or quantity 0))
                    do (check-type count (or null (integer 0)))
                    collect (make-instance 'luvcraft:block-inventory-entry
                                           :block block :quantity count)))
        (dolist (block (getf record :carried))
          (luvcraft:add-block-to-inventory inventory block)))
    inventory))

(defun make-luvcraft-player (&key (id "local") (name id) resume
                               (record (resume-player-record resume))
                               (hotbar (starting-hotbar-placements)) character)
  "A player restored from RECORD, its record in the players table, or from
RESUME, format 1's symbolic one; or a new one whose number row starts as HOTBAR."
  (let* ((inventory (restore-inventory record))
         (selected (getf record :selected-block))
         (player (make-instance
                  'luvcraft-player
                  :id id :name name :record record :character character
                  :inventory inventory :hotbar hotbar
                  :creative-p (getf record :creative-p)
                  :held-block
                  (and selected
                       ;; Restored separately, an entity may be a twin of its stock's.
                       (find (luvcraft::block-save-description selected)
                             (luvcraft:block-inventory-blocks inventory)
                             :key #'luvcraft::block-save-description :test #'equal)))))
    (setf (player-placement player)
          (or (and selected (ignore-errors (game:block-placement selected)))
              (first hotbar)))
    (put-on-hotbar player (player-placement player))
    (restore-player-kit player)
    player))

(defun restore-player-kit (player)
  "Give PLAYER its saved bar, inventory mode, kit, and selected block.
Preferences choose a new player's kit; an actual world resume keeps its own
bar, mode, stock, and selected per-instance block, so a view that applies
preferences restores the kit again after them."
  (let ((record (player-saved-record player))
        (inventory (luvcraft:player-inventory player)))
    (when (getf record :quickbar)
      ;; Format 1 named its bar; a record's inventory is in bar order.
      (luvcraft:arrange-luvcraft-quickbar player (getf record :quickbar)))
    (when (member :simple-inventory-p record)
      (setf (luvcraft::block-inventory-simple-p inventory) (getf record :simple-inventory-p)
            (luvcraft::block-inventory-kit inventory) (getf record :building-kit)))
    (let ((selected (getf record :selected-block)))
      (when selected
        (setf (luvcraft:player-selected-block player) selected)))
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

(defun player-stock-record (player)
  "PLAYER's record with its stock, mode, and hands as they are now, keeping
the fields it did not read; its pose fields are as last recorded."
  (let ((record (loop for (key value) on (player-saved-record player) by #'cddr
                      ;; Format 1's derived fields: the inventory is the bar.
                      unless (member key '(:carried :quickbar))
                        collect key and collect value))
        (inventory (luvcraft:player-inventory player)))
    (setf (getf record :id) (player-id player)
          (getf record :name) (player-name player)
          (getf record :creative-p) (and (luvcraft:player-creative-p player) t)
          (getf record :inventory)
          (loop for entry in (luvcraft:block-inventory-entries inventory)
                collect (list (luvcraft:block-inventory-entry-block entry)
                              (luvcraft:block-inventory-entry-quantity entry)))
          (getf record :simple-inventory-p) (luvcraft::block-inventory-simple-p inventory)
          (getf record :building-kit) (copy-list (luvcraft::block-inventory-kit inventory))
          (getf record :selected-block) (luvcraft:player-selected-block player))
    record))

(defun record-with-pose (record source position yaw pitch flying-p)
  "RECORD with its pose fields for feet at Luft POSITION in SOURCE."
  (let ((record (copy-list record)))
    (loop for (key value) on (game::luft-pose-resume-fields source position yaw pitch flying-p)
            by #'cddr
          do (setf (getf record key) value))
    record))

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
   (absent-records :initarg :player-records :initform nil :accessor world-absent-records
                   :documentation "The saved records of players not here now (#Y3S82T),
in the players table's order: whoever joins again under one of their ids gets it back.")
   (views :initform nil :accessor world-views
          :documentation "The views open on this world; when the last closes, the world shuts down.")
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
   (animals-initialized-p :initform nil :accessor world-animals-initialized-p)
   (agents :initform nil :accessor world-agents)
   (agents-initialized-p :initform nil :accessor world-agents-initialized-p)
   ;; Balls, and the springs that throw them.
   (physics :initform nil :accessor world-physics)
   (physics-clock :initform 0d0 :accessor world-physics-clock)
   (springs :initform nil :accessor world-springs)
   (springs-initialized-p :initform nil :accessor world-springs-initialized-p)
   (living :initform nil :accessor world-living
           :documentation "How its turtles, balls, rides, and sky are shared (living-world.lisp), or NIL.")
   (authored-sequence :initform nil :accessor world-authored-sequence
                      :documentation "The journal sequence Luvcraft's overlay of authored blocks is current at.")
   (session :initform nil :accessor world-session
            :documentation "The host or guest session joining this world to others (session.lisp), or NIL."))
  (:documentation "A Luvcraft world and everything that lives in it, without a window."))

(defun make-luvcraft-world (&key block-world scene
                              (simulation (terrain:make-world-simulation scene))
                              inhabitants player-records sky-origin-offset)
  "A world playing BLOCK-WORLD on SCENE, with its saved INHABITANTS and the
PLAYER-RECORDS of players who are not here yet."
  (make-instance 'luvcraft-world
                 :block-world block-world :simulation simulation :inhabitants inhabitants
                 :player-records player-records
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

(defun ground-resident-p (world x y)
  "Whether WORLD's collision knows column X, Y.  More chunks are resident for
meshing than collide, so whatever falls waits where this is false (#I6O4J6)."
  (terrain:scene-collision-resident-p (world-scene world) x y))

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

(defun steer-luvcraft-player (player frame)
  "Steer PLAYER by one terrain:INPUT-FRAME: its character walks, looks, jumps,
and flies by it, and its rider intent reins whatever it rides by the same axes."
  (let ((intent (player-rider-intent player)))
    (flet ((urge (positive negative value)
             (setf (luvcraft:movement-urging-p intent positive) (plusp value)
                   (luvcraft:movement-urging-p intent negative) (minusp value))))
      (urge :forward :backward (terrain:input-frame-forward frame))
      (urge :right :left (terrain:input-frame-right frame))))
  (when (player-character player)
    (terrain:apply-input-frame (player-character player) frame))
  player)

(defun player-ground-resident-p (world player)
  "Whether PLAYER's character stands where WORLD's scene has collision."
  (let ((character (player-character player)))
    (and character
         (let ((p (terrain:body-position (terrain:character-body character))))
           (ground-resident-p world (floor (vec3:vec3-x p)) (floor (vec3:vec3-y p)))))))

;;; Time.

(defun advance-world-sky (world seconds)
  (luvcraft::advance-sky-clock (world-sky-clock world) seconds)
  (incf (world-sky-elapsed world) seconds))

(defun advance-world-populations (world seconds)
  "Advance everything living in WORLD but its characters' simulation: turtles,
balls and springs, then companions.  A game host steps the simulation itself,
under its own residency and focus rules, before this."
  ;; Events a view sequenced, or a peer's, reach Luvcraft's overlay first.
  (sync-world-authored-blocks world)
  (if (replica-world-p world)
      ;; A guest draws its host's turtles, balls, and sky (#EA88ON).
      (advance-world-replicas world seconds)
      (progn (advance-world-animals world seconds)
             (advance-world-balls world seconds)))
  (advance-world-companions world seconds))

(defun advance-luvcraft-world (world seconds &key before-tick)
  "Advance a headless WORLD by SECONDS in a view's order: sky, characters, then
populations.  BEFORE-TICK, a function of the tick number, steers its players,
as by STEER-LUVCRAFT-PLAYER with a tape's or a bot's next frame."
  (advance-world-sky world seconds)
  (terrain:advance-world-simulation (world-simulation world) seconds
                                    :before-tick before-tick)
  (when (world-session world)
    (advance-world-session (world-session world) seconds))
  (advance-world-populations world seconds)
  world)

;;; Edits.

;;; The world journal is the authority for authored blocks (#3F0SCZ): its
;;; values are block identities, so a film keeps its entity.  Luvcraft's own
;;; overlay, which its block world, balls, and saves read, is a view of the
;;; journal brought up to date from the events after its watermark.

(defun world-authored-block (world cell)
  "The block identity last authored at CELL, or NIL."
  (multiple-value-bind (value present-p)
      (gethash cell (terrain:world-source-edits (world-source world)))
    (and present-p (values (game::journal-value-block value)))))

(defun sync-world-authored-blocks (world)
  "Bring Luvcraft's overlay of WORLD's authored blocks up to its journal's head."
  (let ((source (world-source world)))
    (when (typep source 'terrain:world-source)
      (sync-world-authored-blocks-from world source))))

(defun sync-world-authored-blocks-from (world source)
  (let* ((journal (terrain:world-source-journal source))
         (events (terrain:world-journal-events journal))
         (from (or (world-authored-sequence world)
                   (terrain:world-journal-watermark journal)))
         (overlay (luvcraft:little-world-source-edits
                   (luvcraft:block-world-source (world-block-world world)))))
    (loop for index from (- from (terrain:world-journal-watermark journal))
            below (length events)
          for event = (aref events index)
          do (apply #'luvcraft::record-block-edit overlay
                    (values (game::journal-value-block (terrain:world-event-after event)))
                    (game:site-luvcraft-cell source (terrain:world-event-site event))))
    (setf (world-authored-sequence world) (terrain:world-journal-head journal))))

(defvar *world-edit-actor* nil
  "The journal actor of the edit in progress when it is not the editing
player's own, such as a companion building for its player.")

(defvar *world-edit-player* nil
  "The player whose stock the edit in progress spends when it is not the
view's own, such as a guest's companion building on the host.")

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
                               &key (scene-edit (lambda (block)
                                                  (apply-world-scene-edit
                                                   world player cell block))))
  "Change CELL to PLACEMENT, or mine it when PLACEMENT is NIL, as PLAYER.

PLACEMENT may also be the exact block identity to put there, as undo and
redo give it; otherwise it names the block PLAYER holds or has for it.

Return the journal event, its status (:EDITED, or :QUEUED while a mesh
cohort is in flight), and the block mined, if any.  As in the original
game, materials are unlimited and only carried blocks, such as films, are
picked up and spent; PLAYER's stock changes only after SCENE-EDIT, a function
of the block identity returning an edit and status, accepts the change.  A headless
world edits its scene directly; a view passes its host's edit, which also
schedules meshing.  Undo and redo pass through this same boundary, so they
conserve stock too."
  (when (and placement (companion-occupies-cell-p world cell))
    (return-from player-edit-world-cell (values nil :occupied nil)))
  (let* ((identity-p (typep placement '(or luvcraft:block-kind luvcraft:block-entity)))
         (block (if identity-p
                    placement
                    (and placement (selected-block player placement))))
         (placement (if identity-p (game:block-placement placement) placement))
         (inventory (luvcraft:player-inventory player))
         (entry (and block (luvcraft:block-inventory-entry-for inventory block)))
         (finite-p (and block (luvcraft:block-kind-carried-p block)))
         (old (terrain:scene-material-placement-at (world-scene world) cell))
         (original (world-authored-block world cell))
         (old-block (and old (or (and original
                                       (eq old (game:block-placement original)) original)
                                 (game::luvcraft-block-for-placement old)))))
    (when (or (and placement (null block))
              (and finite-p
                   (or (null entry) (zerop (luvcraft:block-inventory-entry-quantity entry)))))
      (return-from player-edit-world-cell (values nil :empty nil)))
    (multiple-value-bind (edit status)
        ;; A guest's world asks its host's sequencer instead (#8EZK9F).
        (if (world-session world)
            (session-edit-world-cell (world-session world) player cell block scene-edit)
            (funcall scene-edit block))
      (when (and edit (world-physics world))
        (luvcraft::wake-physics-bodies-near
         (world-physics world) (- (luft:site-x cell) 1024) (luft:site-z cell)
         (- (luft:site-y cell) 1024) 2.0))
      ;; Discover springs from the accepted edit next tick, retaining
      ;; unchanged throw counts.
      (sync-world-authored-blocks world)
      (setf (world-springs-initialized-p world) nil)
      (when edit
        (when finite-p
          (luvcraft:remove-block-from-inventory inventory block))
        (when (and old-block (luvcraft:block-kind-carried-p old-block))
          (luvcraft:add-block-to-inventory inventory old-block))
        (when (and old-block (null placement) (luvcraft:block-kind-carried-p old-block))
          (setf (player-held-block player) old-block)
          (put-on-hotbar player old)))
      (values edit status (and edit (null placement) old-block)))))

;;; Saves: format 2, a vocabulary table and columns, with a players table
;;; (#Y3S82T); format 1 still loads, its resume becoming its player's record.

(defun absent-player-record (world id)
  (find id (world-absent-records world) :key (lambda (record) (getf record :id)) :test #'equal))

(defun take-absent-player-record (world id)
  "The saved record of the player ID, which is no longer absent once taken."
  (let ((record (absent-player-record world id)))
    (when record
      (setf (world-absent-records world) (remove record (world-absent-records world))))
    record))

(defun keep-absent-player-record (world record)
  "Remember RECORD for when its player joins again."
  (setf (world-absent-records world)
        (append (remove (getf record :id) (world-absent-records world)
                        :key (lambda (other) (getf other :id)) :test #'equal)
                (list record)))
  record)

(defun adopt-saved-players (records resume id)
  "The record the local player ID takes from a save's players table RECORDS,
or from its format-1 RESUME, and the records of everyone else.  The local
player takes its own record, else the first, the world's owner, as a
format-1 save had one player."
  (if records
      (let ((own (or (find id records :key (lambda (record) (getf record :id)) :test #'equal)
                     (first records))))
        (values own (remove own records)))
      (values (resume-player-record resume) nil)))

(defun player-table-record (world player &key yaw pitch)
  "PLAYER's record in WORLD's players table, posed where it is now: YAW and
PITCH are its look, when a view knows it.  A remote player's stock is its
own, so its record is the one it last sent (player-table.lisp)."
  (let ((character (player-character player)))
    (cond ((player-track player) (remote-player-table-record world player))
          (character
           (let ((position (terrain:body-position (terrain:character-body character))))
             (record-with-pose (player-stock-record player) (world-source world)
                               (list (vec3:vec3-x position) (vec3:vec3-y position)
                                     (vec3:vec3-z position))
                               (or yaw (atan (terrain:character-heading-y character)
                                             (terrain:character-heading-x character)))
                               (or pitch 0.0)
                               (terrain:character-flying-p character))))
          (t (player-stock-record player)))))

(defun world-player-records (world player &key yaw pitch)
  "WORLD's players table, PLAYER, who saves it, first: everyone here, then
everyone who has been."
  (append (list (player-table-record world player :yaw yaw :pitch pitch))
          (loop for other in (world-players world)
                unless (eq other player)
                  collect (player-table-record world other))
          (remove-if (lambda (record) (find-world-player world (getf record :id)))
                     (world-absent-records world))))

(defun luvcraft-world-save-description (world player &key (yaw 0.0) (pitch 0.0))
  "WORLD's save with PLAYER, looking along YAW and PITCH, first in its players table."
  (game:luvcraft-save-description
   (world-source world) (world-block-world world)
   :players (world-player-records world player :yaw yaw :pitch pitch)
   :inhabitants (world-inhabitant-descriptions world)))

(defun save-luvcraft-world (world player pathname &rest look)
  "Write WORLD's save with PLAYER as its player to PATHNAME, atomically."
  (luvcraft::write-luvcraft-save-description
   (apply #'luvcraft-world-save-description world player look) pathname)
  pathname)
