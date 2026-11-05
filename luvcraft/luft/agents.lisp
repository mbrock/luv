(in-package #:luvcraft.luft.game)

;;; Bodies and tools are game content, not renderer state (#NFNM1C).
;;; Provider turns, CLIM argument parsing, transcripts and socket lifetime
;;; belong to the same application-agent harness used by old Luvcraft.

(defclass inhabitant ()
  ;; Locomotion quantities belong to CHARACTER's declared terrain body, not
  ;; duplicate agent slots. These slots are identities, dialogue and handles.
  ((id :initarg :id :initform (fresh-companion-id :companion) :reader inhabitant-id
       :documentation "A stable identity, saved with the companion: its journal actor is (:INHABITANT ID).")
   (kind :initarg :kind :reader inhabitant-kind)
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
   (notified :initarg :notified :initform nil :accessor inhabitant-notified
             :documentation "The line of SAID a view last showed; saved dialogue starts shown.")
   (said-serial :initform 0 :accessor inhabitant-said-serial
                :documentation "How many lines the companion has said here, so a repeated line is news.")
   (heard-turn :initform nil :accessor inhabitant-heard-turn
               :documentation "The provider turn whose words SAID holds.")
   (attending :initform nil :accessor inhabitant-attending
              :documentation "The id of the player the companion attends to: the one who spoke to it last.")
   (owner :initarg :owner :initform nil :accessor inhabitant-owner
          :documentation "The id of the player who summoned it, whose stock it builds
from; NIL for the world's owner (company.lisp).")
   ;; A guest's copy of its host's companion (companion-session.lisp).
   (track :initform nil :accessor inhabitant-track
          :documentation "The host's poses of this companion's body; NIL where it lives.")
   (activity :initform :idle :accessor inhabitant-mirrored-activity)
   (tool :initform nil :accessor inhabitant-mirrored-tool)
   (destination :initform nil :accessor inhabitant-mirrored-destination)))

(defvar *companion-id-random-state* (make-random-state t))

(defun fresh-companion-id (kind)
  "A new stable companion id, such as \"gnome-k3x9q2\"."
  (format nil "~(~A~)-~(~36,6,'0R~)" kind (random (expt 36 6) *companion-id-random-state*)))

(defun inhabitant-actor (inhabitant)
  "The journal principal of INHABITANT's edits, stable across saves."
  (list :inhabitant (inhabitant-id inhabitant)))

(defun inhabitant-mirror-p (inhabitant)
  "Whether INHABITANT is a guest's copy of a companion its host keeps."
  (and (inhabitant-track inhabitant) t))

(defun find-world-inhabitant (world id)
  (find id (world-agents world) :key #'inhabitant-id :test #'equal))

(defclass inhabitant-agent (luv.application-agent:application-agent)
  ((inhabitant :initarg :inhabitant :reader agent-inhabitant)))

(defun inhabitant-position (inhabitant)
  (terrain:body-position (terrain:character-body (inhabitant-character inhabitant))))

(defun companion-distance (a b)
  (sqrt (+ (expt (- (vec3:vec3-x a) (vec3:vec3-x b)) 2)
           (expt (- (vec3:vec3-y a) (vec3:vec3-y b)) 2)
           (expt (- (vec3:vec3-z a) (vec3:vec3-z b)) 2))))

(defun agent-cell (world x y z)
  (luft:make-site (terrain:world-source-domain (world-source world))
                  x y z luft:+cell-extent+ 1))

(defun make-inhabitant (world kind position &key (id (fresh-companion-id kind)) owner
                                                 description conversation asked said (yaw 0.0))
  (let* ((height (luvcraft.agent::companion-body-height kind))
         (character (terrain:make-walking-character
                     :body (make-instance 'terrain:physical-body :position position
                                          :radius 0.28 :height height)
                     :heading-x (sin yaw) :heading-y (cos yaw)
                     :speed (luvcraft.agent::companion-walk-speed kind)))
         (simulation (terrain:make-world-simulation (world-scene world))))
    (terrain:add-simulation-character simulation character)
    (make-instance 'inhabitant :id id :kind kind :character character :simulation simulation
                               :description description :owner owner
                              :conversation conversation :asked asked :said said
                              ;; Saved dialogue is history, not a newly completed turn.
                              :notified said)))

(defun initialize-world-agents (world)
  (unless (world-agents-initialized-p world)
    (dolist (description (world-saved-inhabitants world))
      (when (member (first description) '(:gnome :cat :fantus))
        ;; Use the old inhabitant format so either game can read the save.
        (destructuring-bind (x y z) (getf (rest description) :cell)
          (let ((cell (game:luvcraft-cell-site (world-source world) x y z)))
            (when cell
              (push (make-inhabitant
                     world (first description)
                     (vec3:make-vec3 (+ 0.5 (luft:site-x cell))
                                     (+ 0.5 (luft:site-y cell)) (luft:site-z cell))
                     ;; A companion saved before ids gets one, kept from then on.
                     :id (or (getf (rest description) :id)
                             (fresh-companion-id (first description)))
                     :owner (getf (rest description) :owner)
                     :description description
                     :yaw (getf (rest description) :facing-yaw 0.0)
                     :conversation (getf (rest description) :conversation)
                     :asked (getf (rest description) :asked)
                     :said (getf (rest description) :said))
                    (world-agents world)))))))
    (setf (world-agents-initialized-p world) t)))

(defun inhabitant-description (world inhabitant)
  (let* ((p (inhabitant-position inhabitant))
         (character (inhabitant-character inhabitant))
         (provider (inhabitant-provider inhabitant)))
    (list (inhabitant-kind inhabitant)
          :id (inhabitant-id inhabitant)
          :owner (inhabitant-owner inhabitant)
          :cell (game:site-luvcraft-cell
                 (world-source world)
                 (agent-cell world (floor (vec3:vec3-x p))
                             (floor (vec3:vec3-y p)) (floor (+ 0.01 (vec3:vec3-z p)))))
          :facing-yaw (atan (terrain:character-heading-x character)
                            (terrain:character-heading-y character))
          :conversation (or (and provider (openai:agent-response-id provider))
                            (inhabitant-conversation inhabitant))
          :asked (inhabitant-asked inhabitant) :said (inhabitant-said inhabitant))))

(defun describable-inhabitant-description (world inhabitant)
  "INHABITANT's description, or NIL, logged, when it cannot be described, as
when it has fallen out of the world: one broken companion never stops a save."
  (handler-case (inhabitant-description world inhabitant)
    (error (condition)
      (format *error-output* "~&;; Luvcraft: not saving ~(~A~) ~A at ~A: ~A~%"
              (inhabitant-kind inhabitant) (inhabitant-id inhabitant)
              (ignore-errors (vec3:vec3-list (inhabitant-position inhabitant))) condition)
      (finish-output *error-output*)
      nil)))

(defun agent-inhabitant-descriptions (world descriptions)
  (if (world-agents-initialized-p world)
      (append (loop for agent in (reverse (world-agents world))
                    for description = (and (not (inhabitant-mirror-p agent))
                                           (describable-inhabitant-description world agent))
                    when description collect description)
              (remove-if (lambda (description)
                           (find description (world-agents world)
                                 :key #'inhabitant-original-description :test #'eq))
                         descriptions))
      descriptions))

(defun world-inhabitant-descriptions (world)
  "Every inhabitant of WORLD as its save describes them, including ones this game cannot host."
  (agent-inhabitant-descriptions world (animal-inhabitant-descriptions world)))

(defun summon-world-inhabitant (world position &optional (kind :gnome) owner)
  "Summon a companion onto clear, resident ground near POSITION, to build
from the stock of the player OWNER, an id, or of WORLD's owner.  A guest's
world asks its host and returns NIL (company.lisp)."
  (when (replica-world-p world)
    (return-from summon-world-inhabitant (ask-host-to-summon world position kind)))
  (initialize-world-agents world)
  (let ((scene (world-scene world)))
    (loop for (dx dy) in '((3 0) (0 3) (-3 0) (0 -3) (4 2) (-4 2))
          for x = (+ (floor (vec3:vec3-x position)) dx)
          for y = (+ (floor (vec3:vec3-y position)) dy)
          when (ground-resident-p world x y) do
            (multiple-value-bind (face hx hy hz point)
                (terrain:raycast-face scene (vec3:make-vec3 (+ x 0.5) (+ y 0.5) 254.0)
                                      (vec3:make-vec3 0.0 0.0 -1.0) :max-distance 254.0)
              (declare (ignore hx hy hz))
              (when (and face (terrain::body-clear-at-p
                              (terrain::inspection-source-solid scene)
                              (vec3:vec3-x point) (vec3:vec3-y point) (vec3:vec3-z point)
                              (luvcraft.agent::companion-body-height kind) 0.28)
                         (notany (lambda (other)
                                   (< (companion-distance (inhabitant-position other) point) 1.0))
                                 (world-agents world)))
                (let ((agent (make-inhabitant world kind point :owner owner)))
                  (push agent (world-agents world))
                  (return-from summon-world-inhabitant agent)))))
    (error "No clear resident ground nearby; wait for terrain or move into the open.")))

(defun summon-inhabitant (viewer &optional (kind :gnome))
  "Summon a companion near VIEWER's player, and address it.  A guest's
companion arrives from its host and is addressed then: NIL meanwhile."
  (let ((companion (summon-world-inhabitant
                    (viewer-world viewer)
                    (terrain:body-position (terrain:character-body (render:viewer-player viewer)))
                    kind (world-local-player-id (viewer-world viewer)))))
    (when companion
      (setf (viewer-conversation-agent viewer) companion))))

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

(defun tell-inhabitant (application inhabitant text &key asker)
  "Queue TEXT for INHABITANT from the player ASKER, an id, without doing
network I/O on the canvas.  APPLICATION is the view whose frame its tools
run in.  A guest's companion is its host's: the guest asks the host."
  (check-type text string)
  (when (inhabitant-mirror-p inhabitant)
    (return-from tell-inhabitant (ask-host-companion application inhabitant text)))
  (let ((factory *inhabitant-provider-factory*))
    (sb-thread:with-mutex ((inhabitant-lock inhabitant))
      (when (inhabitant-retired-p inhabitant) (error "This companion has left."))
      (when (inhabitant-opening inhabitant) (error "Still opening the conversation; try again shortly."))
      (setf (inhabitant-asked inhabitant) text
            (inhabitant-attending inhabitant) asker)
      (if (inhabitant-provider inhabitant)
          (progn
            (prepare-inhabitant-provider inhabitant (inhabitant-provider inhabitant))
            (luv.application-agent:ask text :agent (inhabitant-provider inhabitant)))
          (setf (inhabitant-opening inhabitant)
                (sb-thread:make-thread
                 (lambda ()
                   (handler-case
                       (let ((provider (funcall factory application inhabitant)))
                         (sb-thread:with-mutex ((inhabitant-lock inhabitant))
                           (setf (inhabitant-provider inhabitant) provider)
                           (prepare-inhabitant-provider inhabitant provider)
                           (unless (inhabitant-retired-p inhabitant)
                             (luv.application-agent:ask text :agent provider))))
                     (error (condition)
                       (sb-thread:with-mutex ((inhabitant-lock inhabitant))
                         (say-for-inhabitant inhabitant (format nil "~A" condition)))))
                   (sb-thread:with-mutex ((inhabitant-lock inhabitant))
                     (setf (inhabitant-opening inhabitant) nil)))
                 :name "Open companion conversation")))))
  inhabitant)

(defclass inhabitant-journey ()
  ((route :initarg :route :reader journey-route)
   (started :initform (get-internal-real-time) :reader journey-started
            :quantity (:quantity luft.quantities::monotonic-time
                       :unit luft.quantities::internal-time-tick))
   (fetch :initarg :fetch :initform nil :reader journey-fetch)
   (result :initform nil :accessor journey-result)
   (mailbox :initform (sb-concurrency:make-mailbox) :reader journey-mailbox))
  (:metaclass luv.arithmetic.records:quantity-class))

(defun finish-inhabitant-journey (inhabitant text)
  (let ((journey (inhabitant-journey inhabitant)))
    (when journey
      (setf (journey-result journey) text (inhabitant-journey inhabitant) nil)
      (sb-concurrency:send-message (journey-mailbox journey) text))))

(defmethod luv.application-agent:settle-command-result ((command t) (journey inhabitant-journey))
  (sb-concurrency:receive-message (journey-mailbox journey)))

(defun say-for-inhabitant (inhabitant text)
  "INHABITANT now says TEXT: a new line, even when it repeats the last."
  (setf (inhabitant-said inhabitant) (copy-seq text))
  (incf (inhabitant-said-serial inhabitant))
  text)

(defun hear-inhabitant-provider (inhabitant)
  "Take the words of INHABITANT's newly finished provider turn as what it says."
  (let* ((provider (inhabitant-provider inhabitant))
         (turn (and provider (luv.application-agent:application-agent-current-turn provider))))
    (when (and turn (member (luv.application-agent:turn-status turn) '(:done :failed))
               (not (eq turn (inhabitant-heard-turn inhabitant))))
      (setf (inhabitant-heard-turn inhabitant) turn)
      (say-for-inhabitant inhabitant
                          (if (luv.application-agent:turn-error turn)
                              (format nil "~A" (luv.application-agent:turn-error turn))
                              (or (luv.application-agent:turn-text turn) ""))))))

(defun inhabitant-activity (inhabitant)
  "What INHABITANT is doing -- :IDLE, :OPENING its conversation, :THINKING,
or :WORKING -- and the name of the tool it is using, if any."
  (if (inhabitant-mirror-p inhabitant)
      (values (inhabitant-mirrored-activity inhabitant) (inhabitant-mirrored-tool inhabitant))
      (let* ((provider (inhabitant-provider inhabitant))
             (turn (and provider (luv.application-agent:application-agent-current-turn provider)))
             (status (and turn (luv.application-agent:turn-status turn))))
        (cond ((inhabitant-opening inhabitant) (values :opening nil))
              ((member status '(:thinking :working))
               (let ((call (first (luv.application-agent:turn-calls turn))))
                 (values status (and call (luv.application-agent:tool-call-name call)))))
              (t (values :idle nil))))))

(defun inhabitant-destination (inhabitant)
  "The foot cell INHABITANT walks to, or NIL."
  (if (inhabitant-mirror-p inhabitant)
      (inhabitant-mirrored-destination inhabitant)
      (let ((journey (inhabitant-journey inhabitant)))
        (and journey (terrain:walking-route-destination (journey-route journey))))))

(defun advance-world-companions (world seconds)
  (initialize-world-agents world)
  (dolist (inhabitant (world-agents world))
    (if (inhabitant-mirror-p inhabitant)
        ;; A guest's copy walks where the host's poses say.
        (advance-companion-mirror inhabitant seconds)
        (let* ((p (inhabitant-position inhabitant))
               (journey (inhabitant-journey inhabitant)))
          (hear-inhabitant-provider inhabitant)
          ;; Each companion's clock freezes independently where its ground has no
          ;; collision, so it never falls through ground that is only drawn (#I6O4J6).
          (when (ground-resident-p world (floor (vec3:vec3-x p)) (floor (vec3:vec3-y p)))
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
                     (handler-case (edit-agent-cell world inhabitant (journey-fetch journey) nil)
                       (error (condition) (format nil "Fetch failed: ~A" condition)))
                     (format nil "Walk ~A: ~A" status
                             (terrain:walking-route-detail (journey-route journey))))))))))))

(defun advance-viewer-companions (viewer seconds)
  "Show what VIEWER's world's companions have newly said."
  (declare (ignore seconds))
  (dolist (inhabitant (world-agents (viewer-world viewer)))
    (let ((said (inhabitant-said inhabitant)))
      (when (and said (not (eq said (inhabitant-notified inhabitant))))
        (setf (inhabitant-notified inhabitant) said)
        (show-companion-message viewer inhabitant)))))

(defun companion-occupies-cell-p (world cell)
  (some (lambda (agent)
          (terrain:body-overlaps-cell-p
           (terrain:character-body (inhabitant-character agent)) cell))
        (world-agents world)))

;;; Lifetimes.  A companion belongs to its world, but its provider runs its
;;; tools in one view's frame, and its presentation is that view's overlay.
;;; Closing a view lets go of both and leaves the companion, whose
;;; conversation goes on with its next provider; the world's shutdown
;;; retires its companions.

(defun settle-inhabitant-provider (inhabitant application)
  "Wake INHABITANT's parked tools, then release its provider and wait for
it.  APPLICATION, when given, is where its tools run: the wakeups go there."
  (let ((opening nil))
    (sb-thread:with-mutex ((inhabitant-lock inhabitant))
      (setf opening (inhabitant-opening inhabitant)))
    (when opening (sb-thread:join-thread opening))
    (flet ((wake ()
             (when (inhabitant-approval inhabitant)
               (luvcraft.agent:deny-tool-approval (inhabitant-approval inhabitant) "Game closed"))
             (terrain:cancel-character-route (inhabitant-character inhabitant) "Game closed")
             (finish-inhabitant-journey inhabitant "Walk cancelled: game closed")))
      (if application
          (luv.application-agent:call-in-application-frame application #'wake)
          (wake)))
    (let ((provider (inhabitant-provider inhabitant)))
      (when provider
        (luv.application-agent:release-application-agent provider)
        (luv.application-agent:wait-for-application-agent-release provider)
        (sb-thread:with-mutex ((inhabitant-lock inhabitant))
          ;; The next provider picks up the conversation where this one was.
          (setf (inhabitant-conversation inhabitant)
                (or (openai:agent-response-id provider) (inhabitant-conversation inhabitant))
                (inhabitant-provider inhabitant) nil
                (inhabitant-heard-turn inhabitant) nil))))))

(defun detach-world-view (world view)
  "Let VIEW go of WORLD's companions: the providers whose tools run in its
frame, and its presentations of them.  The companions stay."
  (setf (world-views world) (remove view (world-views world)))
  (dolist (inhabitant (world-agents world))
    (let ((provider (inhabitant-provider inhabitant)))
      (when (or (inhabitant-opening inhabitant)
                (and provider (eq view (luv.application-agent:application-agent-application
                                        provider))))
        ;; No opener may ask while the view goes.
        (sb-thread:with-mutex ((inhabitant-lock inhabitant))
          (setf (inhabitant-retired-p inhabitant) t))
        (settle-inhabitant-provider inhabitant view)
        (sb-thread:with-mutex ((inhabitant-lock inhabitant))
          (setf (inhabitant-retired-p inhabitant) nil))))
    (when (and (inhabitant-presentation inhabitant)
               (eq view (luvcraft.agent::gnome-session (inhabitant-presentation inhabitant))))
      (setf (inhabitant-presentation inhabitant) nil
            (inhabitant-presentation-observer inhabitant) nil
            (inhabitant-notified inhabitant) (inhabitant-said inhabitant)))))

(defun retire-world-companions (world)
  "Retire every companion of WORLD: no provider outlives it."
  (dolist (inhabitant (world-agents world))
    (sb-thread:with-mutex ((inhabitant-lock inhabitant))
      (setf (inhabitant-retired-p inhabitant) t))
    (let* ((provider (inhabitant-provider inhabitant))
           (application (and provider (luv.application-agent:application-agent-application
                                       provider))))
      ;; Tools parked in an open view wake in its frame; otherwise here.
      (settle-inhabitant-provider
       inhabitant (and (member application (world-views world)) application)))))

(defun shutdown-luvcraft-world (world)
  "End WORLD: retire its companions, and say goodbye to its peers."
  (retire-world-companions world)
  (when (world-session world)
    (close-world-session (world-session world)))
  world)

(defmethod render::quiesce-viewer-services :before ((viewer luvcraft-viewer))
  ;; Wake parked tools before waiting for provider workers. No provider can
  ;; outlive the canvas on which its commands run, including a late opener;
  ;; the companions themselves stay with the world (game.lisp shuts it down
  ;; with its last view).
  (detach-world-view (viewer-world viewer) viewer))
