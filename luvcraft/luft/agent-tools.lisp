(in-package #:luvcraft.luft.game)

(defun companion-command-context ()
  (let ((agent luv.application-agent:*current-agent*))
    (unless (typep agent 'inhabitant-agent) (error "This tool needs an embodied companion."))
    (let ((inhabitant (agent-inhabitant agent)))
      (when (inhabitant-retired-p inhabitant) (error "The game has closed."))
      (let ((application (luv.application-agent:application-agent-application agent)))
        ;; A headless host's companion runs its tools in the world itself.
        (if (typep application 'luvcraft-world)
            (values application inhabitant nil)
            (values (viewer-world application) inhabitant application))))))

(defmethod luv.application-agent:command-tool-runs-on-canvas-p ((command (eql 'com-eval)))
  nil)

(clim:define-command (com-eval :command-table luvcraft-world :name "Eval")
    ((form 'string))
  "Evaluate a Common Lisp form in the live game image. LUFT.RENDER:*VIEWER* is the game view. Schedule frame-owned changes with LUV.APPLICATION-AGENT:CALL-IN-APPLICATION-FRAME."
  (multiple-value-bind (world inhabitant viewer) (companion-command-context)
    (declare (ignore world))
    (unless (gnome-live-lisp-p viewer inhabitant)
      (error "Live Lisp is a solo gnome tool."))
    (luvcraft.agent:com-eval form)))

(defun material-from-name (name)
  (unless (string-equal name "air")
    (game:block-placement (luvcraft:block-kind-named (intern (string-upcase name) :keyword)))))

(defun cell-name (world cell)
  (let ((placement (terrain:scene-material-placement-at (world-scene world) cell)))
    (if placement (string-downcase (symbol-name (terrain:material-placement-name placement))) "air")))

(defun validate-agent-reach (world inhabitant cell)
  (unless (cell-resident-p world (luft:site-x cell) (luft:site-y cell))
    (error "Cell is not resident; walk closer to the player."))
  (let* ((p (inhabitant-position inhabitant))
         (eye (vec3:make-vec3 (vec3:vec3-x p) (vec3:vec3-y p) (+ 1 (vec3:vec3-z p))))
         (dx (- (+ 0.5 (luft:site-x cell)) (vec3:vec3-x eye)))
         (dy (- (+ 0.5 (luft:site-y cell)) (vec3:vec3-y eye)))
         (dz (- (+ 0.5 (luft:site-z cell)) (vec3:vec3-z eye)))
         (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
    (when (> distance 6) (error "Cell is out of reach; walk closer first."))
    (when (> distance 0.01)
      (multiple-value-bind (face x y z point hit-distance)
          (terrain:raycast-face (world-scene world) eye
                                (vec3:make-vec3 (/ dx distance) (/ dy distance) (/ dz distance))
                                :max-distance distance)
        (declare (ignore point))
        (when (and face (< hit-distance (- distance 0.01))
                   (not (and (= x (luft:site-x cell)) (= y (luft:site-y cell))
                             (= z (luft:site-z cell)))))
          (error "Another solid cell blocks the companion's reach.")))))
  t)

(defun companion-edit (world inhabitant cell placement)
  "Edit CELL as INHABITANT, from its owner's stock: through WORLD's open view, else directly.
The journal names INHABITANT's stable id as the actor, so its edits undo on
their own, and keep their author across saves."
  (let ((*world-edit-actor* (inhabitant-actor inhabitant))
        (*world-edit-player* (companion-owner world inhabitant)))
    (if (world-editor world)
        (funcall (world-editor world) cell placement)
        (player-edit-world-cell world *world-edit-player* cell placement))))

(defun edit-agent-cell (world inhabitant cell placement)
  (validate-agent-reach world inhabitant cell)
  ;; Do not let the player's per-instance held block leak into a tool edit.
  (let* ((player (companion-owner world inhabitant))
         (held (player-held-block player)))
    (unwind-protect
         (progn
           (setf (player-held-block player) nil)
           (multiple-value-bind (edit status) (companion-edit world inhabitant cell placement)
             (declare (ignore edit))
             (values (format nil "Cell (~D ~D ~D): ~A" (luft:site-x cell) (luft:site-y cell)
                             (luft:site-z cell) status)
                     status)))
      (setf (player-held-block player) held))))

(clim:define-command (com-look-around :command-table luvcraft-world :name "Look Around") ()
  "Report your feet, player, inventory, walking status and nearby standing surfaces. Z is up."
  (multiple-value-bind (world inhabitant) (companion-command-context)
    (let* ((p (inhabitant-position inhabitant))
           (owner (companion-owner world inhabitant))
           ;; The player is whoever spoke last; the stock is its summoner's.
           (player (or (companion-attention-position world inhabitant)
                       (terrain:body-position (terrain:character-body (player-character owner))))))
      (format nil "~(~A~) feet (~,2F ~,2F ~,2F); player (~,2F ~,2F ~,2F). Z up.~%~
                   Creative: ~A. Inventory: ~{~A~^, ~}.~%Nearby foot cells: ~{~A~^; ~}"
              (inhabitant-kind inhabitant) (vec3:vec3-x p) (vec3:vec3-y p) (vec3:vec3-z p)
              (vec3:vec3-x player) (vec3:vec3-y player) (vec3:vec3-z player)
              (luvcraft:player-creative-p owner)
              ;; A NIL quantity is unlimited supply, as in creative mode.
              (loop for entry in (luvcraft:block-inventory-entries (luvcraft:player-inventory owner))
                    for quantity = (luvcraft:block-inventory-entry-quantity entry)
                    when (or (null quantity) (plusp quantity))
                      collect (format nil "~(~A~)=~A"
                                      (luvcraft:block-kind-name (luvcraft:block-inventory-entry-block entry))
                                      (or quantity "unlimited")))
              (loop for (dx dy) in '((0 0) (1 0) (-1 0) (0 1) (0 -1) (3 0) (0 3) (-3 0) (0 -3))
                    for x = (+ (floor (vec3:vec3-x p)) dx)
                    for y = (+ (floor (vec3:vec3-y p)) dy)
                    when (ground-resident-p world x y)
                      collect (multiple-value-bind (face hx hy hz)
                                  (terrain:raycast-face
                                   (world-scene world) (vec3:make-vec3 (+ x 0.5) (+ y 0.5) 254.0)
                                   (vec3:make-vec3 0.0 0.0 -1.0) :max-distance 254.0)
                                (if face (format nil "(~D ~D ~D) above ~A" hx hy (1+ hz)
                                                 (cell-name world (agent-cell world hx hy hz)))
                                    "no ground")))))))

(clim:define-command (com-cell-at :command-table luvcraft-world :name "Cell At")
    ((x 'integer) (y 'integer) (z 'integer))
  "Read a resident integer cell's material; Z is up."
  (multiple-value-bind (world inhabitant) (companion-command-context)
    (declare (ignore inhabitant))
    (unless (cell-resident-p world x y) (error "Cell is not resident (not known air)."))
    (cell-name world (agent-cell world x y z))))

(defun begin-inhabitant-walk (world inhabitant x y z &optional fetch)
  (unless (ground-resident-p world x y) (error "Destination has no collision yet; walk closer to a player."))
  (when (inhabitant-journey inhabitant) (error "Already walking."))
  (let* ((route (terrain:start-character-route (inhabitant-character inhabitant)
                                              (world-scene world) x y z))
         (journey (make-instance 'inhabitant-journey :route route :fetch fetch)))
    (setf (inhabitant-journey inhabitant) journey)
    journey))

(clim:define-command (com-walk-to :command-table luvcraft-world :name "Walk To")
    ((x 'integer) (y 'integer) (z 'integer))
  "Walk your body through Luft terrain to the foot cell X Y Z; wait for arrival or failure."
  (multiple-value-bind (world inhabitant) (companion-command-context)
    (begin-inhabitant-walk world inhabitant x y z)))

(clim:define-command (com-place-cell :command-table luvcraft-world :name "Place Cell")
    ((x 'integer) (y 'integer) (z 'integer) (material 'string))
  "Place one named material from the player's inventory within six cells; air mines and collects."
  (multiple-value-bind (world inhabitant) (companion-command-context)
    (values (edit-agent-cell world inhabitant (agent-cell world x y z) (material-from-name material)))))

(clim:define-command (com-fetch-block :command-table luvcraft-world :name "Fetch Block")
    ((x 'integer) (y 'integer) (z 'integer))
  "Walk beside a solid block, mine it and add it to the player's persistent inventory."
  (multiple-value-bind (world inhabitant) (companion-command-context)
    (let ((cell (agent-cell world x y z))
          (character (inhabitant-character inhabitant)))
      (unless (and (ground-resident-p world x y) (not (string= "air" (cell-name world cell))))
        (error "No resident block to fetch."))
      (loop for (dx dy) in '((1 0) (-1 0) (0 1) (0 -1))
            do (multiple-value-bind (face hx hy hz)
                   (terrain:raycast-face (world-scene world)
                                         (vec3:make-vec3 (+ x dx 0.5) (+ y dy 0.5) 254.0)
                                         (vec3:make-vec3 0.0 0.0 -1.0) :max-distance 254.0)
                 (when (and face
                            (not (eq :failed (terrain:walking-route-status
                                              (terrain::find-walking-route
                                               character (world-scene world)
                                               (agent-cell world hx hy (1+ hz)))))))
                   (return-from com-fetch-block
                     (begin-inhabitant-walk world inhabitant hx hy (1+ hz) cell)))))
      (error "No walking route beside the block."))))

;;; Reuse the existing semantic approval lifecycle and off-canvas settlement.
;;; Only its validated payload and detachment are specific to this world.
(defclass companion-proposal (luvcraft.agent:tool-approval)
  ((reviewed-p :initform nil :accessor proposal-reviewed-p)
   (card :initform nil :accessor proposal-card)))

(defclass companion-edit-approval (companion-proposal)
  ((cell :initarg :cell :reader approval-cell)
   (placement :initarg :placement :reader approval-placement)
   (original :initarg :original :reader approval-original)))

(defmethod luvcraft.agent::validate-tool-approval ((approval companion-edit-approval))
  (let ((world (viewer-world (luvcraft.agent::tool-approval-session approval))))
    (unless (eq (approval-original approval)
                (terrain:scene-material-placement-at (world-scene world) (approval-cell approval)))
      (error "The proposed cell has changed; propose again."))
    (validate-agent-reach world (luvcraft.agent::tool-approval-presence approval) (approval-cell approval))))

(defmethod luvcraft.agent::commit-tool-approval ((approval companion-edit-approval))
  (multiple-value-bind (result status)
      (edit-agent-cell (viewer-world (luvcraft.agent::tool-approval-session approval))
                       (luvcraft.agent::tool-approval-presence approval)
                       (approval-cell approval) (approval-placement approval))
    (unless (eq status :edited) (error "~A" result))))

(defmethod luvcraft.agent::detach-tool-approval :after ((approval companion-proposal))
  (let ((viewer (luvcraft.agent::tool-approval-session approval))
        (inhabitant (luvcraft.agent::tool-approval-presence approval)))
    (when (and viewer (eq approval (render:viewer-modal-focus viewer)))
      (render:unfocus-viewer viewer))
    (when (eq approval (inhabitant-approval inhabitant))
      (setf (inhabitant-approval inhabitant) nil))))

(defun install-companion-approval (approval)
  (let ((inhabitant (luvcraft.agent::tool-approval-presence approval)))
    (when (inhabitant-approval inhabitant) (error "An edit is already proposed."))
    (setf (inhabitant-approval inhabitant) approval)))

(clim:define-presentation-method clim:present
    (object (type companion-edit-approval) stream (view clim:textual-view) &key)
  (format stream "Proposed cell: ~A ~A" (luvcraft.agent:tool-approval-state object)
          (or (luvcraft.agent:tool-approval-note object) "")))

(clim:define-command (com-propose-cell :command-table luvcraft-world :name "Propose Cell")
    ((x 'integer) (y 'integer) (z 'integer) (material 'string))
  "Propose a cell edit without changing terrain; wait for the player's Y/N decision."
  (multiple-value-bind (world inhabitant viewer) (companion-command-context)
    (when (inhabitant-approval inhabitant) (error "An edit is already proposed."))
    (let* ((cell (agent-cell world x y z))
           (approval (make-instance 'companion-edit-approval
                                    :session viewer :presence inhabitant
                                    :agent luv.application-agent:*current-agent*
                                    :cell cell :placement (material-from-name material)
                                    :original (terrain:scene-material-placement-at
                                               (world-scene world) cell))))
      (validate-agent-reach world inhabitant cell)
      (install-companion-approval approval))))

(defclass companion-box-approval (companion-proposal)
  ((changes :initarg :changes :reader box-approval-changes)))

(defmethod luvcraft.agent::validate-tool-approval ((approval companion-box-approval))
  (let* ((world (viewer-world (luvcraft.agent::tool-approval-session approval)))
         (scene (world-scene world))
         (inhabitant (luvcraft.agent::tool-approval-presence approval)))
    (dolist (change (box-approval-changes approval))
      (destructuring-bind (cell before after) change
        (declare (ignore before after))
        (validate-agent-reach world inhabitant cell)
        (when (or (companion-occupies-cell-p world cell)
                  (some (lambda (body) (terrain:body-overlaps-cell-p body cell))
                        (terrain:simulation-bodies (world-simulation world))))
          (error "A body is in the proposed box."))
        (multiple-value-bind (old known-p) (terrain:world-cell-value scene cell)
          (unless (and known-p (null old))
            (error "The proposed box needs empty, loaded cells.")))))))

(defmethod luvcraft.agent::commit-tool-approval ((approval companion-box-approval))
  (let* ((world (viewer-world (luvcraft.agent::tool-approval-session approval)))
         (inhabitant (luvcraft.agent::tool-approval-presence approval)))
    (multiple-value-bind (events status)
        (terrain:sequence-world-events
         (world-scene world) (box-approval-changes approval)
         :actor (inhabitant-actor inhabitant) :cause :edit)
      (unless events (error "Construction rejected: ~A." status))
      (sync-world-authored-blocks world)
      (setf (world-springs-initialized-p world) nil))))

(clim:define-presentation-method clim:present
    (object (type companion-box-approval) stream (view clim:textual-view) &key)
  (format stream "Construction proposal (~D blocks): ~A~@[. ~A~]"
          (length (box-approval-changes object))
          (luvcraft.agent:tool-approval-state object)
          (luvcraft.agent:tool-approval-note object)))

(clim:define-command (com-propose-block-box :command-table luvcraft-world
                                            :name "Propose Block Box")
    ((material 'string)
     (from-x 'integer) (from-y 'integer) (from-z 'integer)
     (to-x 'integer) (to-y 'integer) (to-z 'integer))
  "Preview filling the empty box between two inclusive corners with one ordinary, unlimited block material. The box must be within reach and contain at most 512 cells. Wait for the player's approval."
  (multiple-value-bind (world inhabitant viewer) (companion-command-context)
    (unless viewer (error "A construction preview needs an open world view."))
    (unless (member (inhabitant-kind inhabitant) '(:gnome :fantus))
      (error "This companion cannot propose construction."))
    (let* ((block (luvcraft:block-kind-named (intern (string-upcase material) :keyword)))
           (placement (game:block-placement block))
           (volume (* (1+ (abs (- from-x to-x)))
                      (1+ (abs (- from-y to-y)))
                      (1+ (abs (- from-z to-z))))))
      (unless (and placement (luvcraft::block-kind-placeable-p block)
                   (not (luvcraft:block-kind-carried-p block))
                   (eq :none (terrain:material-kind-body
                              (terrain:material-placement-kind placement))))
        (error "Box construction needs an ordinary block material."))
      (when (> volume luvcraft.agent::*maximum-construction-proposal-cells*)
        (error "That box has too many cells (~D)." volume))
      (let* ((changes
               (loop for z from (min from-z to-z) to (max from-z to-z) append
                 (loop for y from (min from-y to-y) to (max from-y to-y) append
                   (loop for x from (min from-x to-x) to (max from-x to-x)
                         collect (list (agent-cell world x y z) nil block)))))
             (approval (make-instance 'companion-box-approval
                                      :session viewer :presence inhabitant
                                      :agent luv.application-agent:*current-agent*
                                      :changes changes)))
        (luvcraft.agent::validate-tool-approval approval)
        (install-companion-approval approval)))))

(defun show-companion-message (viewer inhabitant)
  (let ((agent (ensure-inhabitant-presentation viewer inhabitant)))
    (when (inhabitant-said inhabitant)
      (luvcraft.agent:gnome-say agent (inhabitant-said inhabitant)))))

(defun call-companion-ui (viewer function)
  (handler-case (funcall function)
    (error (condition)
      (luv.workbench:show-workbench-message
       (luv.workbench:application-workbench viewer)
       (luvcraft.agent::wrap-words (format nil "~A" condition) 72)
       :error-p t :seconds 10))))

(defmethod mcluv:palette-hint :around ((viewer luvcraft-viewer))
  (let ((inhabitant (viewer-conversation-agent viewer)))
    (multiple-value-bind (activity tool) (if inhabitant (inhabitant-activity inhabitant) :idle)
      (cond
        ((render:viewer-modal-focus viewer) nil)
        ((player-ball-in-hand-p (viewer-local-player viewer))
         "Ball · left throw · right marbles · B put away")
        ((and inhabitant (inhabitant-approval inhabitant))
         "Proposed edit · T review · Y approve · N decline")
        ((eq activity :opening) "Opening conversation…")
        ((member activity '(:thinking :working))
         (format nil "~(~A~): ~A · T talk" (inhabitant-kind inhabitant) (or tool "thinking…")))
        ((eq :empty (render:viewer-last-edit-status viewer)) "No blocks left · mine to collect")
        (inhabitant (format nil "~(~A~) · T talk · F7 next · I inventory" (inhabitant-kind inhabitant)))
        (t (if (render:viewer-pointer-captured-p viewer)
               "B ball · G gnome · T talk · F1 keys"
               "Click to play · B ball · G gnome · F1 keys"))))))

(clim:define-command (com-summon-companion :command-table luvcraft-world
                                         :name "Summon Companion" :keystroke (:g)) ()
  (let* ((viewer (render:viewer-command-viewer))
         (companion (summon-inhabitant viewer)))
    (if companion
        (show-companion-message viewer companion)
        (luv.workbench:show-workbench-message (luv.workbench:application-workbench viewer)
                                             "Asking the host for a companion…"))))

(clim:define-command (com-summon-cat :command-table luvcraft-world :name "Summon Cat") ()
  (summon-inhabitant (render:viewer-command-viewer) :cat))

(clim:define-command (com-summon-fantus :command-table luvcraft-world :name "Summon Fantus") ()
  (summon-inhabitant (render:viewer-command-viewer) :fantus))

(clim:define-command (com-talk-to-companion :command-table luvcraft-world
                                          :name "Talk to Companion" :keystroke (:t)) ()
  (let* ((viewer (render:viewer-command-viewer))
         (inhabitant (or (viewer-conversation-agent viewer)
                         (first (world-agents (viewer-world viewer)))
                         (summon-inhabitant viewer))))
    (setf (viewer-conversation-agent viewer) inhabitant)
    (if (inhabitant-approval inhabitant)
        (render:focus-viewer viewer (inhabitant-approval inhabitant))
        (render:focus-viewer viewer (ensure-inhabitant-presentation viewer inhabitant)))))

(clim:define-command (com-next-companion :command-table luvcraft-world
                                       :name "Next Companion" :keystroke (:f7)) ()
  (let* ((viewer (render:viewer-command-viewer))
         (agents (world-agents (viewer-world viewer))))
    (setf (viewer-conversation-agent viewer)
          (or (second (member (viewer-conversation-agent viewer) agents)) (first agents)))
    (when (viewer-conversation-agent viewer)
      (show-companion-message viewer (viewer-conversation-agent viewer)))))

(clim:define-command (com-approve-companion :command-table luvcraft-world
                                          :name "Approve Companion" :keystroke (:y)) ()
  (let* ((viewer (render:viewer-command-viewer))
         (agent (viewer-conversation-agent viewer)))
    (when (and agent (inhabitant-approval agent))
      (luvcraft.agent:approve-tool-approval (inhabitant-approval agent)))))

(clim:define-command (com-deny-companion :command-table luvcraft-world
                                      :name "Decline Companion") ()
  (let* ((viewer (render:viewer-command-viewer))
         (agent (viewer-conversation-agent viewer)))
    (when (and agent (inhabitant-approval agent))
      (luvcraft.agent:deny-tool-approval (inhabitant-approval agent) "Player declined."))))

;; Conversation remains reachable after a modal prompt releases the pointer.
;; The workbench consumes typing first; these are only downstream game keys.
;; A focused companion is typing too, so its letters skip these bindings.
;; N still pages inventory outside a pending approval.
(defmethod luv:handle-canvas-event :around
    ((viewer luvcraft-viewer) canvas (event luv:canvas-key-press-event))
  (let* ((agent (viewer-conversation-agent viewer))
         (command
           (when (and (null (render:viewer-modal-focus viewer))
                      (null (luv:canvas-key-event-modifiers event))
                      (not (luv:canvas-key-event-repeat-p event)))
             (case (luv:canvas-key-event-key-name event)
               (:g '(com-summon-companion))
               (:t '(com-talk-to-companion))
               (:f7 '(com-next-companion))
               (:/ (when (world-session (viewer-world viewer)) '(com-chat)))
               (:y (when (and agent (inhabitant-approval agent)) '(com-approve-companion)))
               (:n (when (and agent (inhabitant-approval agent)) '(com-deny-companion)))))))
    (if command
        (call-companion-ui viewer (lambda () (clim:execute-frame-command viewer command)))
        (call-next-method))))
