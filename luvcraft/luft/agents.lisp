(in-package #:luvcraft.luft.game)

;;; Bodies and tools are game content, not renderer state (#NFNM1C).
;;; Provider turns, CLIM argument parsing, transcripts and socket lifetime
;;; belong to the same application-agent harness used by old Luvcraft.

(defclass inhabitant ()
  ((kind :initarg :kind :reader inhabitant-kind)
   (description :initarg :description :initform nil :reader inhabitant-original-description)
   (character :initarg :character :reader inhabitant-character)
   (simulation :initarg :simulation :reader inhabitant-simulation)
   (provider :initform nil :accessor inhabitant-provider)
   (conversation :initarg :conversation :initform nil :accessor inhabitant-conversation)
   (asked :initarg :asked :initform nil :accessor inhabitant-asked)
   (said :initarg :said :initform nil :accessor inhabitant-said)
   (approval :initform nil :accessor inhabitant-approval)
   (journey :initform nil :accessor inhabitant-journey)
   (lock :initform (sb-thread:make-mutex :name "inhabitant provider") :reader inhabitant-lock)
   (opening :initform nil :accessor inhabitant-opening)
   (retired-p :initform nil :accessor inhabitant-retired-p)
   (presentation :initform nil :accessor inhabitant-presentation)
   (presentation-observer :initform nil :accessor inhabitant-presentation-observer)
   (notified :initform nil :accessor inhabitant-notified)))

(defclass inhabitant-agent (luv.application-agent:application-agent)
  ((inhabitant :initarg :inhabitant :reader agent-inhabitant)))

(defun inhabitant-position (inhabitant)
  (terrain:body-position (terrain:character-body (inhabitant-character inhabitant))))

(defun companion-distance (a b)
  (sqrt (+ (expt (- (vec3:vec3-x a) (vec3:vec3-x b)) 2)
           (expt (- (vec3:vec3-y a) (vec3:vec3-y b)) 2)
           (expt (- (vec3:vec3-z a) (vec3:vec3-z b)) 2))))

(defun agent-cell (viewer x y z)
  (luft:make-site (terrain:world-source-domain (streaming-source viewer))
                  x y z luft:+cell-extent+ 1))

(defun cell-resident-p (viewer x y)
  (nth-value 1 (gethash (luft:chunk-key-at x y)
                        (terrain:streaming-scene-store (render:viewer-source viewer)))))

(defun make-inhabitant (viewer kind position &key description conversation asked said (yaw 0.0))
  (let* ((height (ecase kind (:gnome 1.5) (:cat 0.6) (:fantus 1.8)))
         (character (terrain:make-walking-character
                     :body (make-instance 'terrain:physical-body :position position
                                          :radius 0.28 :height height)
                     :heading-x (sin yaw) :heading-y (cos yaw) :speed 3.0))
         (simulation (terrain:make-world-simulation (render:viewer-source viewer))))
    (terrain:add-simulation-character simulation character)
    (make-instance 'inhabitant :kind kind :character character :simulation simulation :description description
                              :conversation conversation :asked asked :said said)))

(defun initialize-viewer-agents (viewer)
  (unless (viewer-agents-initialized-p viewer)
    (dolist (description (luvcraft-viewer-inhabitants viewer))
      (when (member (first description) '(:gnome :cat :fantus))
        ;; Use the old inhabitant format so either game can read the save.
        (destructuring-bind (x y z) (getf (rest description) :cell)
          (let ((cell (game:luvcraft-cell-site (streaming-source viewer) x y z)))
            (when cell
              (push (make-inhabitant
                     viewer (first description)
                     (vec3:make-vec3 (+ 0.5 (luft:site-x cell))
                                     (+ 0.5 (luft:site-y cell)) (luft:site-z cell))
                     :description description
                     :yaw (getf (rest description) :facing-yaw 0.0)
                     :conversation (getf (rest description) :conversation)
                     :asked (getf (rest description) :asked)
                     :said (getf (rest description) :said))
                    (viewer-agents viewer)))))))
    (setf (viewer-agents-initialized-p viewer) t)))

(defun inhabitant-description (viewer inhabitant)
  (let* ((p (inhabitant-position inhabitant))
         (character (inhabitant-character inhabitant))
         (provider (inhabitant-provider inhabitant)))
    (list (inhabitant-kind inhabitant)
          :cell (game:site-luvcraft-cell
                 (streaming-source viewer)
                 (agent-cell viewer (floor (vec3:vec3-x p))
                             (floor (vec3:vec3-y p)) (floor (+ 0.01 (vec3:vec3-z p)))))
          :facing-yaw (atan (terrain:character-heading-x character)
                            (terrain:character-heading-y character))
          :conversation (or (and provider (openai:agent-response-id provider))
                            (inhabitant-conversation inhabitant))
          :asked (inhabitant-asked inhabitant) :said (inhabitant-said inhabitant))))

(defun agent-inhabitant-descriptions (viewer descriptions)
  ;; ALLOCATE-INSTANCE save tests deliberately don't initialize game services.
  (if (and (slot-boundp viewer 'agents-initialized-p) (viewer-agents-initialized-p viewer))
      (append (mapcar (lambda (agent) (inhabitant-description viewer agent))
                      (reverse (viewer-agents viewer)))
              (remove-if (lambda (description)
                           (find description (viewer-agents viewer)
                                 :key #'inhabitant-original-description :test #'eq))
                         descriptions))
      descriptions))

(defun summon-inhabitant (viewer &optional (kind :gnome))
  "Summon a companion onto clear, resident ground near the player."
  (initialize-viewer-agents viewer)
  (let* ((p (terrain:body-position (terrain:character-body (render:viewer-player viewer))))
         (scene (render:viewer-source viewer)))
    (loop for (dx dy) in '((3 0) (0 3) (-3 0) (0 -3) (4 2) (-4 2))
          for x = (+ (floor (vec3:vec3-x p)) dx)
          for y = (+ (floor (vec3:vec3-y p)) dy)
          when (cell-resident-p viewer x y) do
            (multiple-value-bind (face hx hy hz point)
                (terrain:raycast-face scene (vec3:make-vec3 (+ x 0.5) (+ y 0.5) 254.0)
                                      (vec3:make-vec3 0.0 0.0 -1.0) :max-distance 254.0)
              (declare (ignore hx hy hz))
              (when (and face (terrain::body-clear-at-p
                              (terrain::inspection-source-solid scene)
                              (vec3:vec3-x point) (vec3:vec3-y point) (vec3:vec3-z point)
                              (ecase kind (:gnome 1.5) (:cat 0.6) (:fantus 1.8)) 0.28)
                         (notany (lambda (other)
                                   (< (companion-distance (inhabitant-position other) point) 1.0))
                                 (viewer-agents viewer)))
                (let ((agent (make-inhabitant viewer kind point)))
                  (push agent (viewer-agents viewer))
                  (setf (viewer-conversation-agent viewer) agent)
                  (return-from summon-inhabitant agent)))))
    (error "No clear resident ground nearby; wait for terrain or move into the open.")))

(defparameter *inhabitant-tools*
  '(com-look-around com-cell-at com-walk-to com-place-cell com-fetch-block com-propose-cell))

(defparameter *inhabitant-instructions*
  "You are a friendly embodied companion in Luvcraft on Luft. Your tools act
through your body, not the player's camera. Coordinates are integer cells;
X and Y are horizontal, Z is UP. Look around first. Walk to a clear foot cell
before building or fetching. Walking waits for arrival. Fetch mines a nearby
block after walking to it and puts it in the player's inventory. Place uses
the player's inventory (or creative mode). Never claim an error succeeded.
Propose-cell asks the player to approve an edit; ordinary single-cell placing
is immediate. Build larger things one cell at a time. Keep replies short.")

(defun open-inhabitant-provider (viewer inhabitant)
  (let ((agent (luv.application-agent:make-application-agent
                :class 'inhabitant-agent :application viewer
                :initargs (list :inhabitant inhabitant)
                :model luvcraft.agent::*default-agent-model*
                :commands *inhabitant-tools* :instructions *inhabitant-instructions*
                :reasoning-effort "medium" :api-key (render::viewer-openai-api-key viewer))))
    (setf (openai:agent-response-id agent) (inhabitant-conversation inhabitant))
    agent))

(defvar *inhabitant-provider-factory* #'open-inhabitant-provider
  "Provider seam: tests replace only the network, not turns, tools or bodies.")

(defgeneric prepare-inhabitant-provider (inhabitant provider)
  (:method (inhabitant provider) (declare (ignore inhabitant provider)) nil))

(defun tell-inhabitant (viewer inhabitant text)
  "Queue a message without doing network I/O on the canvas."
  (check-type text string)
  (let ((factory *inhabitant-provider-factory*))
    (sb-thread:with-mutex ((inhabitant-lock inhabitant))
      (when (inhabitant-retired-p inhabitant) (error "This companion has left."))
      (when (inhabitant-opening inhabitant) (error "Still opening the conversation; try again shortly."))
      (setf (inhabitant-asked inhabitant) text)
      (if (inhabitant-provider inhabitant)
          (progn
            (prepare-inhabitant-provider inhabitant (inhabitant-provider inhabitant))
            (luv.application-agent:ask text :agent (inhabitant-provider inhabitant)))
          (setf (inhabitant-opening inhabitant)
                (sb-thread:make-thread
                 (lambda ()
                   (handler-case
                       (let ((provider (funcall factory viewer inhabitant)))
                         (sb-thread:with-mutex ((inhabitant-lock inhabitant))
                           (setf (inhabitant-provider inhabitant) provider)
                           (prepare-inhabitant-provider inhabitant provider)
                           (unless (inhabitant-retired-p inhabitant)
                             (luv.application-agent:ask text :agent provider))))
                     (error (condition)
                       (setf (inhabitant-said inhabitant) (format nil "~A" condition))))
                   (sb-thread:with-mutex ((inhabitant-lock inhabitant))
                     (setf (inhabitant-opening inhabitant) nil)))
                 :name "Open companion conversation")))))
  inhabitant)

(defclass inhabitant-journey ()
  ((route :initarg :route :reader journey-route)
   (started :initform (get-internal-real-time) :reader journey-started)
   (fetch :initarg :fetch :initform nil :reader journey-fetch)
   (result :initform nil :accessor journey-result)
   (mailbox :initform (sb-concurrency:make-mailbox) :reader journey-mailbox)))

(defun finish-inhabitant-journey (inhabitant text)
  (let ((journey (inhabitant-journey inhabitant)))
    (when journey
      (setf (journey-result journey) text (inhabitant-journey inhabitant) nil)
      (sb-concurrency:send-message (journey-mailbox journey) text))))

(defmethod luv.application-agent:settle-command-result ((command t) (journey inhabitant-journey))
  (sb-concurrency:receive-message (journey-mailbox journey)))

(defmethod render:advance-viewer-game :after ((viewer luvcraft-viewer) seconds)
  (initialize-viewer-agents viewer)
  (dolist (inhabitant (viewer-agents viewer))
    (let* ((p (inhabitant-position inhabitant))
           (journey (inhabitant-journey inhabitant)))
      ;; Each companion's clock freezes independently outside published ground.
      (when (cell-resident-p viewer (floor (vec3:vec3-x p)) (floor (vec3:vec3-y p)))
        (terrain:advance-world-simulation (inhabitant-simulation inhabitant) seconds))
      (when journey
        (when (> (- (get-internal-real-time) (journey-started journey))
                 (* 120 internal-time-units-per-second))
          (terrain:cancel-character-route (inhabitant-character inhabitant) "walk timed out"))
        (let ((status (terrain:walking-route-status (journey-route journey))))
          (unless (eq status :running)
            (finish-inhabitant-journey
             inhabitant
             (if (and (eq status :arrived) (journey-fetch journey))
                 (handler-case (edit-agent-cell viewer inhabitant (journey-fetch journey) nil)
                   (error (condition) (format nil "Fetch failed: ~A" condition)))
                 (format nil "Walk ~A: ~A" status
                         (terrain:walking-route-detail (journey-route journey))))))))
      (let* ((provider (inhabitant-provider inhabitant))
             (turn (and provider (luv.application-agent:application-agent-current-turn provider))))
        (when (and turn (member (luv.application-agent:turn-status turn) '(:done :failed))
                   (not (eq turn (inhabitant-notified inhabitant))))
          (setf (inhabitant-notified inhabitant) turn
                (inhabitant-said inhabitant)
                (if (luv.application-agent:turn-error turn)
                    (format nil "~A" (luv.application-agent:turn-error turn))
                    (luv.application-agent:turn-text turn)))
          (show-companion-message viewer inhabitant))
        (when (and (null provider) (null (inhabitant-opening inhabitant))
                   (inhabitant-said inhabitant)
                   (not (equal (inhabitant-notified inhabitant) (inhabitant-said inhabitant))))
          (setf (inhabitant-notified inhabitant) (inhabitant-said inhabitant))
          (show-companion-message viewer inhabitant))))))

(defun companion-occupies-cell-p (viewer cell)
  (and (slot-boundp viewer 'agents)
       (some (lambda (agent)
               (terrain:body-overlaps-cell-p
                (terrain:character-body (inhabitant-character agent)) cell))
             (viewer-agents viewer))))

(defmethod render::quiesce-viewer-services :before ((viewer luvcraft-viewer))
  ;; Wake parked tools before waiting for provider workers. No provider can
  ;; outlive the canvas on which its commands run, including a late opener.
  (dolist (inhabitant (viewer-agents viewer))
    (let ((opening nil))
      (sb-thread:with-mutex ((inhabitant-lock inhabitant))
        (setf (inhabitant-retired-p inhabitant) t
              opening (inhabitant-opening inhabitant)))
      (when opening (sb-thread:join-thread opening))
      (luv.application-agent:call-in-application-frame
       viewer (lambda ()
                (when (inhabitant-approval inhabitant)
                  (luvcraft.agent:deny-tool-approval (inhabitant-approval inhabitant) "Game closed"))
                (terrain:cancel-character-route (inhabitant-character inhabitant) "Game closed")
                (finish-inhabitant-journey inhabitant "Walk cancelled: game closed")))
      (let ((provider (inhabitant-provider inhabitant)))
        (when provider
          (luv.application-agent:release-application-agent provider)
          (luv.application-agent:wait-for-application-agent-release provider))))))
