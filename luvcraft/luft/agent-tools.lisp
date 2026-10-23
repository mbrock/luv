(in-package #:luvcraft.luft.game)

(defun companion-command-context ()
  (let ((agent luv.application-agent:*current-agent*))
    (unless (typep agent 'inhabitant-agent) (error "This tool needs an embodied companion."))
    (let ((inhabitant (agent-inhabitant agent)))
      (when (inhabitant-retired-p inhabitant) (error "The game has closed."))
      (values (luv.application-agent:application-agent-application agent) inhabitant))))

(defun material-from-name (name)
  (unless (string-equal name "air")
    (game:block-placement (luvcraft:block-kind-named (intern (string-upcase name) :keyword)))))

(defun cell-name (viewer cell)
  (let ((placement (terrain:scene-material-placement-at (render:viewer-source viewer) cell)))
    (if placement (string-downcase (symbol-name (terrain:material-placement-name placement))) "air")))

(defun validate-agent-reach (viewer inhabitant cell)
  (unless (cell-resident-p viewer (luft:site-x cell) (luft:site-y cell))
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
          (terrain:raycast-face (render:viewer-source viewer) eye
                                (vec3:make-vec3 (/ dx distance) (/ dy distance) (/ dz distance))
                                :max-distance distance)
        (declare (ignore point))
        (when (and face (< hit-distance (- distance 0.01))
                   (not (and (= x (luft:site-x cell)) (= y (luft:site-y cell))
                             (= z (luft:site-z cell)))))
          (error "Another solid cell blocks the companion's reach.")))))
  t)

(defun edit-agent-cell (viewer inhabitant cell placement)
  (validate-agent-reach viewer inhabitant cell)
  ;; Do not let the player's per-instance held block leak into a tool edit.
  (let ((held (viewer-held-block viewer)))
    (unwind-protect
         (progn
           (setf (viewer-held-block viewer) nil)
           (multiple-value-bind (edit status) (render:apply-viewer-world-edit viewer cell placement)
             (declare (ignore edit))
             (values (format nil "Cell (~D ~D ~D): ~A" (luft:site-x cell) (luft:site-y cell)
                             (luft:site-z cell) status)
                     status)))
      (setf (viewer-held-block viewer) held))))

(clim:define-command (com-look-around :command-table luvcraft-world :name "Look Around") ()
  "Report your feet, player, inventory, walking status and nearby standing surfaces. Z is up."
  (multiple-value-bind (viewer inhabitant) (companion-command-context)
    (let* ((p (inhabitant-position inhabitant))
           (player (terrain:body-position (terrain:character-body (render:viewer-player viewer)))))
      (format nil "~(~A~) feet (~,2F ~,2F ~,2F); player (~,2F ~,2F ~,2F). Z up.~%~
                   Creative: ~A. Inventory: ~{~A~^, ~}.~%Nearby foot cells: ~{~A~^; ~}"
              (inhabitant-kind inhabitant) (vec3:vec3-x p) (vec3:vec3-y p) (vec3:vec3-z p)
              (vec3:vec3-x player) (vec3:vec3-y player) (vec3:vec3-z player)
              (viewer-creative-p viewer)
              (loop for entry in (luvcraft:block-inventory-entries (viewer-inventory viewer))
                    when (plusp (luvcraft:block-inventory-entry-quantity entry))
                      collect (format nil "~(~A~)=~D"
                                      (luvcraft:block-kind-name (luvcraft:block-inventory-entry-block entry))
                                      (luvcraft:block-inventory-entry-quantity entry)))
              (loop for (dx dy) in '((0 0) (1 0) (-1 0) (0 1) (0 -1) (3 0) (0 3) (-3 0) (0 -3))
                    for x = (+ (floor (vec3:vec3-x p)) dx)
                    for y = (+ (floor (vec3:vec3-y p)) dy)
                    when (cell-resident-p viewer x y)
                      collect (multiple-value-bind (face hx hy hz)
                                  (terrain:raycast-face
                                   (render:viewer-source viewer) (vec3:make-vec3 (+ x 0.5) (+ y 0.5) 254.0)
                                   (vec3:make-vec3 0.0 0.0 -1.0) :max-distance 254.0)
                                (if face (format nil "(~D ~D ~D) above ~A" hx hy (1+ hz)
                                                 (cell-name viewer (agent-cell viewer hx hy hz)))
                                    "no ground")))))))

(clim:define-command (com-cell-at :command-table luvcraft-world :name "Cell At")
    ((x 'integer) (y 'integer) (z 'integer))
  "Read a resident integer cell's material; Z is up."
  (multiple-value-bind (viewer inhabitant) (companion-command-context)
    (declare (ignore inhabitant))
    (unless (cell-resident-p viewer x y) (error "Cell is not resident (not known air)."))
    (cell-name viewer (agent-cell viewer x y z))))

(defun begin-inhabitant-walk (viewer inhabitant x y z &optional fetch)
  (unless (cell-resident-p viewer x y) (error "Destination is not resident."))
  (when (inhabitant-journey inhabitant) (error "Already walking."))
  (let* ((route (terrain:start-character-route (inhabitant-character inhabitant)
                                              (render:viewer-source viewer) x y z))
         (journey (make-instance 'inhabitant-journey :route route :fetch fetch)))
    (setf (inhabitant-journey inhabitant) journey)
    journey))

(clim:define-command (com-walk-to :command-table luvcraft-world :name "Walk To")
    ((x 'integer) (y 'integer) (z 'integer))
  "Walk your body through Luft terrain to the foot cell X Y Z; wait for arrival or failure."
  (multiple-value-bind (viewer inhabitant) (companion-command-context)
    (begin-inhabitant-walk viewer inhabitant x y z)))

(clim:define-command (com-place-cell :command-table luvcraft-world :name "Place Cell")
    ((x 'integer) (y 'integer) (z 'integer) (material 'string))
  "Place one named material from the player's inventory within six cells; air mines and collects."
  (multiple-value-bind (viewer inhabitant) (companion-command-context)
    (values (edit-agent-cell viewer inhabitant (agent-cell viewer x y z) (material-from-name material)))))

(clim:define-command (com-fetch-block :command-table luvcraft-world :name "Fetch Block")
    ((x 'integer) (y 'integer) (z 'integer))
  "Walk beside a solid block, mine it and add it to the player's persistent inventory."
  (multiple-value-bind (viewer inhabitant) (companion-command-context)
    (let ((cell (agent-cell viewer x y z))
          (character (inhabitant-character inhabitant)))
      (unless (and (cell-resident-p viewer x y) (not (string= "air" (cell-name viewer cell))))
        (error "No resident block to fetch."))
      (loop for (dx dy) in '((1 0) (-1 0) (0 1) (0 -1))
            do (multiple-value-bind (face hx hy hz)
                   (terrain:raycast-face (render:viewer-source viewer)
                                         (vec3:make-vec3 (+ x dx 0.5) (+ y dy 0.5) 254.0)
                                         (vec3:make-vec3 0.0 0.0 -1.0) :max-distance 254.0)
                 (when (and face
                            (not (eq :failed (terrain:walking-route-status
                                              (terrain::find-walking-route
                                               character (render:viewer-source viewer)
                                               (agent-cell viewer hx hy (1+ hz)))))))
                   (return-from com-fetch-block
                     (begin-inhabitant-walk viewer inhabitant hx hy (1+ hz) cell)))))
      (error "No walking route beside the block."))))

;;; Reuse the existing semantic approval lifecycle and off-canvas settlement.
;;; Only its validated payload and detachment are specific to this world.
(defclass companion-edit-approval (luvcraft.agent:tool-approval)
  ((cell :initarg :cell :reader approval-cell)
   (placement :initarg :placement :reader approval-placement)
   (original :initarg :original :reader approval-original)))

(defmethod luvcraft.agent::validate-tool-approval ((approval companion-edit-approval))
  (let ((viewer (luvcraft.agent::tool-approval-session approval)))
    (unless (eq (approval-original approval)
                (terrain:scene-material-placement-at (render:viewer-source viewer) (approval-cell approval)))
      (error "The proposed cell has changed; propose again."))
    (validate-agent-reach viewer (luvcraft.agent::tool-approval-presence approval) (approval-cell approval))))

(defmethod luvcraft.agent::commit-tool-approval ((approval companion-edit-approval))
  (multiple-value-bind (result status)
      (edit-agent-cell (luvcraft.agent::tool-approval-session approval)
                       (luvcraft.agent::tool-approval-presence approval)
                       (approval-cell approval) (approval-placement approval))
    (unless (eq status :edited) (error "~A" result))))

(defmethod luvcraft.agent::detach-tool-approval ((approval companion-edit-approval))
  (setf (inhabitant-approval (luvcraft.agent::tool-approval-presence approval)) nil))

(clim:define-presentation-method clim:present
    (object (type companion-edit-approval) stream (view clim:textual-view) &key)
  (format stream "Proposed cell: ~A ~A" (luvcraft.agent:tool-approval-state object)
          (or (luvcraft.agent:tool-approval-note object) "")))

(clim:define-command (com-propose-cell :command-table luvcraft-world :name "Propose Cell")
    ((x 'integer) (y 'integer) (z 'integer) (material 'string))
  "Propose a cell edit without changing terrain; wait for the player's Y/N decision."
  (multiple-value-bind (viewer inhabitant) (companion-command-context)
    (when (inhabitant-approval inhabitant) (error "An edit is already proposed."))
    (let* ((cell (agent-cell viewer x y z))
           (approval (make-instance 'companion-edit-approval
                                    :session viewer :presence inhabitant
                                    :agent luv.application-agent:*current-agent*
                                    :cell cell :placement (material-from-name material)
                                    :original (terrain:scene-material-placement-at
                                               (render:viewer-source viewer) cell))))
      (validate-agent-reach viewer inhabitant cell)
      (setf (inhabitant-approval inhabitant) approval)
      (when (luv.workbench:application-workbench viewer)
        (luv.workbench:show-workbench-message
         (luv.workbench:application-workbench viewer)
         (list (format nil "~A proposes ~A at (~D ~D ~D)" (inhabitant-kind inhabitant) material x y z)
               "Y approve · N decline · T request changes") :seconds 120))
      approval)))

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
  (let* ((inhabitant (viewer-conversation-agent viewer))
         (provider (and inhabitant (inhabitant-provider inhabitant)))
         (turn (and provider (luv.application-agent:application-agent-current-turn provider))))
    (cond
      ((render:viewer-modal-focus viewer) nil)
      ((viewer-ball-in-hand-p viewer) (call-next-method))
      ((and inhabitant (inhabitant-approval inhabitant))
       "Proposed edit · Y approve · N decline · T revise")
      ((and inhabitant (inhabitant-opening inhabitant)) "Opening conversation…")
      ((and turn (member (luv.application-agent:turn-status turn) '(:thinking :working)))
       (format nil "~(~A~): ~A · T talk"
               (inhabitant-kind inhabitant)
               (let ((call (first (luv.application-agent:turn-calls turn))))
                 (if call (luv.application-agent:tool-call-name call) "thinking…"))))
      ((eq :empty (render:viewer-last-edit-status viewer)) "No blocks left · mine to collect")
      (inhabitant (format nil "~(~A~) · T talk · F7 next · I inventory" (inhabitant-kind inhabitant)))
      (t (if (render:viewer-pointer-captured-p viewer)
             "B ball · G gnome · T talk · F1 keys"
             "Click to play · B ball · G gnome · F1 keys")))))

(clim:define-command (com-summon-companion :command-table luvcraft-world
                                         :name "Summon Companion" :keystroke (:g)) ()
  (let* ((viewer (render:viewer-command-viewer))
         (companion (summon-inhabitant viewer)))
    (show-companion-message viewer companion)))

(clim:define-command (com-summon-cat :command-table luvcraft-world :name "Summon Cat") ()
  (summon-inhabitant (render:viewer-command-viewer) :cat))

(clim:define-command (com-summon-fantus :command-table luvcraft-world :name "Summon Fantus") ()
  (summon-inhabitant (render:viewer-command-viewer) :fantus))

(clim:define-command (com-talk-to-companion :command-table luvcraft-world
                                          :name "Talk to Companion" :keystroke (:t)) ()
  (let* ((viewer (render:viewer-command-viewer))
         (inhabitant (or (viewer-conversation-agent viewer)
                         (first (viewer-agents viewer)) (summon-inhabitant viewer))))
    (setf (viewer-conversation-agent viewer) inhabitant)
    (if (inhabitant-approval inhabitant)
        (luv.workbench:open-workbench-prompt
         (luv.workbench:application-workbench viewer)
         :label "What should change?"
         :function (lambda (text)
                     (call-companion-ui viewer
                      (lambda ()
                        (unless (string= "" (string-trim " " text))
                          (luvcraft.agent:steer-tool-approval (inhabitant-approval inhabitant) text))))))
        (render:focus-viewer viewer (ensure-inhabitant-presentation viewer inhabitant)))))

(clim:define-command (com-next-companion :command-table luvcraft-world
                                       :name "Next Companion" :keystroke (:f7)) ()
  (let* ((viewer (render:viewer-command-viewer))
         (agents (viewer-agents viewer)))
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
;; N still pages inventory outside a pending approval.
(defmethod luv:handle-canvas-event :around
    ((viewer luvcraft-viewer) canvas (event luv:canvas-key-press-event))
  (let* ((agent (viewer-conversation-agent viewer))
         (command
           (when (and (null (luv:canvas-key-event-modifiers event))
                      (not (luv:canvas-key-event-repeat-p event)))
             (case (luv:canvas-key-event-key-name event)
               (:g '(com-summon-companion))
               (:t '(com-talk-to-companion))
               (:f7 '(com-next-companion))
               (:y (when (and agent (inhabitant-approval agent)) '(com-approve-companion)))
               (:n (when (and agent (inhabitant-approval agent)) '(com-deny-companion)))))))
    (if command
        (call-companion-ui viewer (lambda () (clim:execute-frame-command viewer command)))
        (call-next-method))))
