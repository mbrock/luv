;;;; Two players build one house (#OLWSWF): a session joins a world to a wire
;;;; endpoint.
;;;;
;;;; A HOST-SESSION's world is the authority for authored change (#8EZK9F).
;;;; It answers a guest's hello with a welcome -- the recipe, the material
;;;; vocabulary, the journal watermark, the edit set compacted there, and the
;;;; guest's player record -- then sequences the guest's edit requests in its
;;;; own journal and broadcasts every event recorded after the watermark,
;;;; whoever made it.  Its own player's edits take the same path through the
;;;; same sequencer; it simply needs no request.
;;;;
;;;; A GUEST-SESSION raises its world from the welcome's recipe, checks the
;;;; world tag, installs the edit set at the watermark, and records the
;;;; host's events strictly after it (#2SGDU2), so its journal's sequence
;;;; numbers are the host's.  Its own edits become requests; they show once
;;;; the host confirms them.
;;;;
;;;; Each player is authoritative for its body, sent as pose snapshots at
;;;; 30 Hz stamped with the simulation tick and drawn on the other side
;;;; through a REMOTE-TRACK.  Peers trade chunk hashes at the same journal
;;;; point and log any that differ (#RELI34).
;;;;
;;;; A session never touches a socket: it drains its endpoint's mailbox at a
;;;; frame boundary, in ADVANCE-WORLD-SESSION, on the thread that owns the
;;;; world.
(in-package #:luvcraft.luft.game)

(defparameter *pose-period* 1/30
  "Seconds between pose snapshots of the local player.")

(defparameter *chunk-hash-period* 2
  "Seconds between chunk-hash reports.")

(defparameter *chunk-hashes-per-report* 2
  "How many resident chunks one report hashes; the rest wait their turn.")

(defun wall-seconds ()
  (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

(defun world-tick (world)
  "WORLD's simulation tick: whole 1/120-second steps since it began."
  (terrain:simulation-tick (world-simulation world)))

(defun default-player-identity ()
  "This machine's stable player identity: $LUV_PLAYER_ID, or user@machine."
  (or (uiop:getenv "LUV_PLAYER_ID")
      (format nil "~A@~A" (or (uiop:getenv "USER") "someone")
              (or (ignore-errors (machine-instance)) "somewhere"))))

(defun default-player-name ()
  (or (uiop:getenv "LUV_PLAYER_NAME") (uiop:getenv "USER") "someone"))

;;; Sessions.

(defclass world-session ()
  ((world :initarg :world :reader session-world)
   (endpoint :initarg :endpoint :reader session-endpoint)
   (local-player :initarg :local-player :reader session-local-player)
   (viewer :initform nil :accessor session-viewer
           :documentation "The view whose frames apply queued events and mesh
them; without one, the session applies them itself.")
   (state :initform :open :accessor session-state)
   (pose-due :initform 0d0 :accessor session-pose-due)
   (hash-due :initform 0d0 :accessor session-hash-due)
   (hash-cursor :initform 0 :accessor session-hash-cursor)
   (hash-job :initform nil :accessor session-hash-job
             :documentation "The worker hashing chunks, or NIL.")
   (hash-queue :initform nil :accessor session-hash-queue
               :documentation "(RESIDENTS . FINISH) jobs waiting for the worker, oldest first.")
   (hash-results :initform (sb-concurrency:make-mailbox :name "chunk hashes")
                 :reader session-hash-results)
   (matched-chunks :initform 0 :accessor session-matched-chunks
                   :documentation "Chunk hashes compared equal with a peer.")
   (mismatched-chunks :initform nil :accessor session-mismatched-chunks
                      :documentation "(KEY OURS THEIRS WATERMARK) for every chunk hash that differed.")
   (actor-names :initform (make-hash-table :test #'eq) :reader session-actor-names)
   (log :initform nil :accessor session-log-lines))
  (:documentation "A world joined to others through a wire endpoint."))

(defclass host-session (world-session)
  ((broadcast-mark :initarg :broadcast-mark :accessor session-broadcast-mark
                   :documentation "The journal sequence every guest has been sent.")
   (guests :initform nil :accessor session-guests
           :documentation "(PEER . PLAYER) for every welcomed guest.")
   (departed :initform (make-hash-table :test #'equal) :reader session-departed
             :documentation "The last pose of every guest that has left, by identity.")
   (remap :initform (own-remap) :reader session-wire-remap
          :documentation "This image's own vocabulary, which the host's offsets name."))
  (:documentation "The session of the world that sequences everyone's edits."))

(defclass guest-session (world-session)
  ((peer :initarg :peer :accessor session-peer)
   (remap :initarg :remap :reader session-remap
          :documentation "What each of the host's vocabulary offsets means here.")
   (host-offsets :initarg :host-offsets :reader session-host-offsets
                 :documentation "Material name -> the host's offset for it.")
   (watermark :initarg :watermark :reader session-watermark
              :documentation "The sequence the welcome's edit set was compacted at.")
   (applied :initarg :applied :accessor session-applied
            :documentation "The host's sequence of the last event recorded here.")
   (transactions :initform (make-hash-table) :reader session-transactions
                 :documentation "The host's transaction ids -> this journal's.")
   (next-request :initform 0 :accessor session-next-request)
   (replies :initform nil :accessor session-replies
            :documentation "The host's edit replies, newest first."))
  (:documentation "The session of a world that asks its host to sequence edits."))

(defmethod print-object ((session world-session) stream)
  (print-unreadable-object (session stream :type t)
    (format stream "~(~A~) ~A at ~D" (session-state session)
            (player-id (session-local-player session))
            (session-journal-head session))))

(defun session-scene (session) (world-scene (session-world session)))

(defun session-journal (session)
  (terrain:streaming-scene-journal (session-scene session)))

(defun session-journal-head (session)
  (terrain:world-journal-head (session-journal session)))

(defun session-log (session control &rest arguments)
  "Note one line in SESSION's log and on the error output."
  (let ((line (apply #'format nil control arguments)))
    (push line (session-log-lines session))
    (when (> (length (session-log-lines session)) 200)
      (setf (session-log-lines session) (subseq (session-log-lines session) 0 100)))
    (format *error-output* "~&;; Luvcraft ~(~A~): ~A~%" (type-of session) line)
    (finish-output *error-output*)
    line))

(defun session-remote-players (session)
  (remove-if-not #'player-track (world-players (session-world session))))

;;; Values on the wire.  Offsets name the host's vocabulary: the host's own,
;;; and the table its welcome shipped to a guest.

(defun identity-wire (identity &optional offsets)
  "Block IDENTITY as a wire block identity: its offset, by OFFSETS (material
name -> offset) or else this image's vocabulary, and its entity's save
description.  NIL when OFFSETS cannot name its material."
  (multiple-value-bind (offset entity) (luvcraft:block-identity-offset identity)
    (let ((offset (if offsets
                      (gethash (luvcraft::saved-material-name identity) offsets)
                      offset)))
      (and offset
           (wire:make-block-identity
            :offset offset
            :entity (and entity (not (typep entity 'luvcraft::missing-material))
                         (luvcraft:block-save-description entity)))))))

(defun wire-identity (block remap)
  "The block identity that wire BLOCK names through REMAP."
  (let ((material (svref remap (wire:block-identity-offset block)))
        (description (wire:block-identity-entity block)))
    (if (null description)
        material
        (let ((entity (luvcraft::restore-world-edit-block-value description)))
          (unless (and (typep entity 'luvcraft:block-entity)
                       (eq (luvcraft:block-identity-material entity) material))
            (error "~S does not describe an entity over ~S." description material))
          entity))))

(defun own-remap ()
  (luvcraft:resolve-material-vocabulary (luvcraft:material-vocabulary-table)))

(defun site-wire (site)
  (wire:make-site (luft:site-x site) (luft:site-y site) (luft:site-z site)))

(defun wire-cell (world site)
  "The Luft cell of wire SITE in WORLD's domain."
  (luft:make-site (terrain:scene-domain (world-scene world))
                  (wire:site-x site) (wire:site-y site) (wire:site-z site)
                  luft:+cell-extent+ 1))

(defun actor-principal (session actor)
  "Journal ACTOR as a wire principal.  A companion is named by its kind and
the order the host first saw it act."
  (cond ((stringp actor) (wire:make-principal :kind :player :id actor))
        ((eq actor :import) (wire:make-principal :kind :import))
        ((keywordp actor) (wire:make-principal :kind :host :id (symbol-name actor)))
        ((and (consp actor) (member (first actor) '(:inhabitant :host)))
         (wire:make-principal :kind (first actor) :id (second actor)))
        (t (let ((names (session-actor-names session)))
             (wire:make-principal
              :kind :inhabitant
              :id (or (gethash actor names)
                      (setf (gethash actor names)
                            (format nil "~(~A~)-~D"
                                    (if (typep actor 'inhabitant) (inhabitant-kind actor) "actor")
                                    (1+ (hash-table-count names))))))))))

(defun principal-actor (principal)
  "The journal actor a wire PRINCIPAL names here."
  (let ((id (wire:principal-id principal)))
    (ecase (wire:principal-kind principal)
      (:player id)
      (:import :import)
      (:host (if (string= id "LOCAL") :local (list :host id)))
      (:inhabitant (list :inhabitant id)))))

(defun wire-cause (cause)
  "CAUSE if the wire can carry it, else its printed form."
  (labels ((carried-p (value depth)
             (and (< depth 8)
                  (typecase value
                    ((or null (eql t) keyword string (signed-byte 64) float) t)
                    (cons (and (null (cdr (last value)))
                               (every (lambda (element) (carried-p element (1+ depth))) value)))))))
    (if (carried-p cause 0) cause (princ-to-string cause))))

(defun event-wire (session event)
  (flet ((wire-value (value) (identity-wire (values (journal-value-block value)))))
    (wire:make-event
     :sequence (terrain:world-event-sequence event)
     :tick (or (terrain:world-event-tick event) 0)
     :actor (actor-principal session (terrain:world-event-actor event))
     :transaction (terrain:world-event-transaction event)
     :site (site-wire (terrain:world-event-site event))
     :before (wire-value (terrain:world-event-before event))
     :after (wire-value (terrain:world-event-after event))
     :cause (wire-cause (terrain:world-event-cause event)))))

(defun journal-value-block (value)
  (if (eq value :unrecorded)
      (values nil nil)
      (game::journal-value-block value)))

(defun world-wire-recipe (source)
  "SOURCE's recipe for the wire: its generator, version, seed, and parameters."
  (destructuring-bind (generator version &key seed relief height little-class offset domain)
      (terrain:world-source-recipe source)
    (wire:make-recipe
     :generator (string-downcase (symbol-name generator)) :version version :seed seed
     :parameters (list :relief relief :height height
                       :little-class (format nil "~A::~A"
                                             (package-name (symbol-package little-class))
                                             (symbol-name little-class))
                       :offset offset :domain domain))))

(defun recipe-little-class (name)
  "The little-world class a recipe names, refusing anything else."
  (let* ((colon (or (search "::" name) (error "~S names no class." name)))
         (package (find-package (subseq name 0 colon)))
         (symbol (and package (find-symbol (subseq name (+ colon 2)) package)))
         (class (and symbol (find-class symbol nil))))
    (unless (and class (subtypep class 'luvcraft:little-world-source))
      (error "The host's world is a ~A, which this image cannot raise." name))
    symbol))

(defun tag-hex (octets)
  (format nil "~(~{~2,'0X~}~)" (coerce octets 'list)))

;;; Poses.

(defun local-pose (session &key yaw pitch)
  "SESSION's local player's body as a wire pose, or NIL without one."
  (let* ((world (session-world session))
         (player (session-local-player session))
         (character (player-character player)))
    (when character
      (let* ((body (terrain:character-body character))
             (p (terrain:body-position body))
             (v (terrain:body-velocity body))
             (held (luvcraft:player-selected-block player)))
        (wire:make-pose
         :tick (world-tick world)
         :position (vector (vec3:vec3-x p) (vec3:vec3-y p) (vec3:vec3-z p))
         :velocity (vector (vec3:vec3-x v) (vec3:vec3-y v) (vec3:vec3-z v))
         :yaw (wire:wrap-yaw (or yaw (atan (terrain:character-heading-y character)
                                           (terrain:character-heading-x character))))
         :pitch (max (- (/ pi 2)) (min (/ pi 2) (or pitch 0d0)))
         :gait (coerce (terrain:character-gait character) 'double-float)
         :flying (terrain:character-flying-p character)
         :held (and held (ignore-errors
                          (identity-wire held (and (typep session 'guest-session)
                                                   (session-host-offsets session))))))))))

(defun pose-wire-snapshot (session pose)
  (make-pose-snapshot
   :sent (/ (wire:pose-tick pose) (float +simulation-ticks-per-second+ 1d0))
   :position (map 'simple-vector (lambda (x) (coerce x 'double-float)) (wire:pose-position pose))
   :velocity (map 'simple-vector (lambda (x) (coerce x 'double-float)) (wire:pose-velocity pose))
   :yaw (coerce (wire:pose-yaw pose) 'double-float)
   :pitch (coerce (wire:pose-pitch pose) 'double-float)
   :gait (coerce (wire:pose-gait pose) 'double-float)
   :flying (wire:pose-flying pose)
   :held (let ((held (wire:pose-held pose)))
           (and held (ignore-errors (wire-identity held (session-wire-remap session)))))))

(defgeneric session-wire-remap (session)
  (:method ((session guest-session)) (session-remap session)))

(defun ensure-remote-player (session id &optional (name id))
  "WORLD's remote player ID, admitted with a fresh track if new."
  (let ((world (session-world session)))
    (or (find-world-player world id)
        (add-world-player world (make-instance 'luvcraft-player
                                               :id id :name name :track (make-remote-track)
                                               :inventory (luvcraft:make-block-inventory)
                                               :hotbar nil)))))

(defun forget-remote-player (session id)
  (let ((player (find-world-player (session-world session) id)))
    (when (and player (player-track player))
      (remove-world-player (session-world session) player))
    player))

(defun receive-pose (session message)
  "Take a peer's pose snapshot onto its player's track."
  (let ((id (wire:pose-message-player message)))
    (unless (equal id (player-id (session-local-player session)))
      (let ((player (ensure-remote-player session id)))
        (when (player-track player)
          (push-pose-snapshot (player-track player)
                              (pose-wire-snapshot session (wire:pose-message-pose message))
                              (wall-seconds)))))))

(defparameter *remote-body-radius* 0.3d0)
(defparameter *remote-body-height* 1.8d0)

(defun remote-player-occupies-cell-p (session cell &key except)
  "Whether a remote player other than EXCEPT stands where CELL would be."
  (some (lambda (player)
          (let ((shown (and (not (equal (player-id player) except))
                            (track-shown (player-track player)))))
            (and shown
                 (let ((p (pose-snapshot-position shown))
                       (r *remote-body-radius*))
                   (and (< (- (luft:site-x cell) r) (svref p 0) (+ (luft:site-x cell) 1 r))
                        (< (- (luft:site-y cell) r) (svref p 1) (+ (luft:site-y cell) 1 r))
                        (< (- (luft:site-z cell) *remote-body-height*) (svref p 2)
                           (+ (luft:site-z cell) 1)))))))
        (session-remote-players session)))

(defun advance-remote-tracks (session seconds)
  (let ((now (wall-seconds)))
    (dolist (player (session-remote-players session))
      (advance-remote-track (player-track player) now seconds))))

(defun remote-player-figures (world)
  "The figure lanes of WORLD's remote players, as their tracks draw them."
  (loop for player in (world-players world)
        for track = (player-track player)
        for shown = (and track (track-shown track))
        for previous = (and shown (or (track-previous track) shown))
        when shown
          collect (flet ((vec (pose)
                           (let ((p (pose-snapshot-position pose)))
                             (vec3:make-vec3 (coerce (svref p 0) 'single-float)
                                             (coerce (svref p 1) 'single-float)
                                             (coerce (svref p 2) 'single-float)))))
                    (render:figure-lanes
                     (vec shown) (vec previous)
                     (coerce (pose-snapshot-gait shown) 'single-float)
                     (coerce (pose-snapshot-gait previous) 'single-float)
                     (coerce (cos (pose-snapshot-yaw shown)) 'single-float)
                     (coerce (sin (pose-snapshot-yaw shown)) 'single-float)
                     (coerce (cos (pose-snapshot-yaw previous)) 'single-float)
                     (coerce (sin (pose-snapshot-yaw previous)) 'single-float)))))

;;; Edits.

(defgeneric session-edit-world-cell (session player cell block scene-edit)
  (:documentation "Edit CELL to BLOCK as PLAYER in SESSION's world: return an
edit and its status as SCENE-EDIT does, or NIL and why not."))

(defmethod session-edit-world-cell ((session world-session) player cell block scene-edit)
  (if (and block (remote-player-occupies-cell-p session cell))
      (values nil :occupied)
      (funcall scene-edit block)))

(defun request-cause ()
  "The cause of the edit in progress: :UNDO or :REDO of a view, else :EDIT."
  (let ((cause render::*viewer-world-edit-cause*))
    (if (member cause '(:undo :redo)) cause :edit)))

(defmethod session-edit-world-cell ((session guest-session) player cell block scene-edit)
  (declare (ignore scene-edit))
  (let ((world (session-world session))
        (cause (request-cause)))
    (cond
      ((not (eq (session-state session) :open)) (values nil :disconnected))
      ((and block (eq cause :edit)
            (or (remote-player-occupies-cell-p session cell)
                (some (lambda (body) (terrain:body-overlaps-cell-p body cell))
                      (terrain:simulation-bodies (world-simulation world)))))
       (values nil :occupied))
      ((not (eq cause :edit))
       (send-edit-request session nil cause))
      (t
       (multiple-value-bind (before known-p) (terrain:world-visible-value (world-scene world) cell)
         (let ((before (and known-p (identity-wire (values (journal-value-block before))
                                                   (session-host-offsets session))))
               (after (identity-wire block (session-host-offsets session))))
           (cond ((not known-p) (values nil :nonresident))
                 ((not (and before after)) (values nil :unknown-material))
                 (t (send-edit-request
                     session (list (wire:make-edit :site (site-wire cell)
                                                   :before before :after after))
                     cause)))))))))

(defun send-edit-request (session edits cause)
  (let ((request (incf (session-next-request session))))
    (net:send-message (session-endpoint session)
                      (wire:make-edit-request-message :request request :edits edits :cause cause)
                      (session-peer session))
    (values nil :requested request)))

(defun session-undo (session)
  "Ask for the local player's latest transaction to be undone."
  (if (typep session 'guest-session)
      (send-edit-request session nil :undo)
      (terrain:undo-world-events (session-scene session)
                                 (player-id (session-local-player session)))))

(defun session-production (session)
  "The production system and bevel width a sequenced edit settles with."
  (let ((viewer (session-viewer session)))
    (if viewer
        (values (render::viewer-production-system viewer) (render::viewer-bevel-width viewer))
        (values nil nil))))

(defun sequence-guest-request (session player message)
  "Sequence a guest's edit request in the host's journal; return the events,
or NIL and the reason."
  (let* ((world (session-world session))
         (scene (world-scene world))
         (actor (player-id player))
         (tick (world-tick world))
         (cause (wire:edit-request-message-cause message))
         (remap (session-wire-remap session)))
    (handler-case
        (if (member cause '(:undo :redo))
            (multiple-value-bind (production-system bevel-width) (session-production session)
              (funcall (if (eq cause :undo) #'terrain:undo-world-events #'terrain:redo-world-events)
                       scene actor :tick tick :production-system production-system
                                   :bevel-width bevel-width))
            (let ((changes (loop for edit in (wire:edit-request-message-edits message)
                                 collect (list (wire-cell world (wire:edit-site edit))
                                               (wire-identity (wire:edit-before edit) remap)
                                               (wire-identity (wire:edit-after edit) remap)))))
              (cond
                ((null changes) (values nil :empty))
                ((some (lambda (change)
                         (let ((cell (first change)))
                           (and (third change)
                                (or (companion-occupies-cell-p world cell)
                                    (remote-player-occupies-cell-p session cell :except actor)
                                    (some (lambda (body) (terrain:body-overlaps-cell-p body cell))
                                          (terrain:simulation-bodies (world-simulation world)))))))
                       changes)
                 (values nil :occupied))
                (t (terrain:sequence-world-events scene changes :actor actor :tick tick
                                                                :cause :edit)))))
      (error (condition)
        (session-log session "Refused ~A's request: ~A" actor condition)
        (values nil :malformed)))))

;;; The host.

(defun host-luvcraft-world (world player &key (port wire:+default-port+) loopback
                                          (name (player-name player)) announce)
  "Open WORLD to guests as PLAYER's host and return the session.

PORT (TCP and UDP, 47475 by default; 0 picks one) or LOOPBACK, a loopback
network, is where guests find it; ANNOUNCE, a world name, broadcasts a
beacon on UDP 47474."
  (let* ((source (world-source world))
         (endpoint (net:start-host :name name :port port :loopback loopback
                                   :world-tag (terrain:world-source-tag source)
                                   :announce announce))
         (session (make-instance 'host-session
                                 :world world :endpoint endpoint :local-player player
                                 :broadcast-mark (terrain:world-journal-head
                                                  (terrain:world-source-journal source)))))
    (setf (world-session world) session)
    session))

(defun guest-peer-player (session peer)
  (cdr (assoc peer (session-guests session))))

(defun player-wire-record (session player pose)
  (declare (ignore session))
  (wire:make-player-record :identity (player-id player) :name (player-name player)
                           :pose (or pose (wire:make-pose))
                           :creative (and (luvcraft:player-creative-p player) t)))

(defun spawn-pose (session identity)
  "Where a guest named IDENTITY starts: where it left, or the world's spawn."
  (or (gethash identity (session-departed session))
      (let ((world (session-world session)))
        (multiple-value-bind (x y z yaw) (terrain:world-source-spawn (world-source world))
          (wire:make-pose :tick (world-tick world)
                          :position (vector (+ x 0.5d0) (+ y 0.5d0) (coerce z 'double-float))
                          :yaw (wire:wrap-yaw (coerce yaw 'double-float)))))))

(defun player-last-pose (session player)
  "PLAYER's latest pose as a wire pose, local or remote."
  (if (player-track player)
      (let ((shown (or (track-shown (player-track player))
                       (let ((snapshots (track-snapshots (player-track player))))
                         (and (plusp (length snapshots))
                              (aref snapshots (1- (length snapshots))))))))
        (and shown
             (wire:make-pose :tick (round (* (pose-snapshot-sent shown) +simulation-ticks-per-second+))
                             :position (pose-snapshot-position shown)
                             :yaw (wire:wrap-yaw (pose-snapshot-yaw shown))
                             :pitch (pose-snapshot-pitch shown)
                             :flying (pose-snapshot-flying shown))))
      (local-pose session)))

(defun compacted-edit-set (session)
  "The host journal's edit set at its head, as wire placements by site."
  (let ((placements nil))
    (maphash (lambda (site value)
               (let ((identity (identity-wire (values (journal-value-block value)))))
                 (when identity
                   (push (wire:make-placement :site (site-wire site) :block identity)
                         placements))))
             (terrain:world-journal-edits (session-journal session)))
    (sort placements (lambda (a b)
                       (let ((a (wire:placement-site a)) (b (wire:placement-site b)))
                         (or (< (wire:site-x a) (wire:site-x b))
                             (and (= (wire:site-x a) (wire:site-x b))
                                  (or (< (wire:site-y a) (wire:site-y b))
                                      (and (= (wire:site-y a) (wire:site-y b))
                                           (< (wire:site-z a) (wire:site-z b)))))))))))

(defun admit-guest (session peer hello)
  "Welcome the guest saying HELLO on PEER, or turn it away."
  (let* ((endpoint (session-endpoint session))
         (identity (wire:hello-message-identity hello))
         (name (wire:hello-message-name hello))
         (world (session-world session)))
    (when (or (zerop (length identity)) (find-world-player world identity))
      (session-log session "Turned ~A away: ~S is already playing." name identity)
      (net:send-message endpoint (wire:make-bye-message :reason :identity-taken
                                                        :text "That player is already here.")
                        peer)
      (net:close-peer endpoint peer)
      (return-from admit-guest nil))
    ;; Everyone has every event up to the watermark: the guest's catch-up is
    ;; the edit set there, and every later event follows on the same stream.
    (broadcast-journal session)
    (let* ((pose (spawn-pose session identity))
           (player (ensure-remote-player session identity name))
           (record (player-wire-record session player pose)))
      (net:send-message
       endpoint
       (wire:make-welcome-message
        :recipe (world-wire-recipe (world-source world))
        :vocabulary (map 'list #'symbol-name (luvcraft:material-vocabulary-table))
        :watermark (session-journal-head session)
        :edits (compacted-edit-set session)
        :entities (wire:make-entity-snapshot :tick (world-tick world))
        :player record)
       peer)
      (dolist (other (world-players world))
        (unless (eq other player)
          (net:send-message endpoint
                            (wire:make-player-update-message
                             :presence :present
                             :player (player-wire-record session other
                                                         (player-last-pose session other)))
                            peer)))
      (dolist (guest (session-guests session))
        (net:send-message endpoint (wire:make-player-update-message :presence :joined :player record)
                          (car guest)))
      (push (cons peer player) (session-guests session))
      (session-log session "~A (~A) joined at sequence ~D." name identity
                   (session-journal-head session))
      player)))

(defun part-guest (session peer reason)
  (let ((player (guest-peer-player session peer)))
    (when player
      (setf (session-guests session) (remove peer (session-guests session) :key #'car))
      (terrain:remove-streaming-scene-interest (session-scene session) (player-id player))
      (let ((pose (player-last-pose session player)))
        (when pose (setf (gethash (player-id player) (session-departed session)) pose))
        (forget-remote-player session (player-id player))
        (dolist (guest (session-guests session))
          (net:send-message (session-endpoint session)
                            (wire:make-player-update-message
                             :presence :left :player (player-wire-record session player pose))
                            (car guest))))
      (session-log session "~A left (~A)." (player-name player) reason))))

(defun broadcast-journal (session)
  "Send every guest the events recorded since the last broadcast."
  (let* ((journal (session-journal session))
         (head (terrain:world-journal-head journal))
         (mark (session-broadcast-mark session)))
    (when (> head mark)
      (when (session-guests session)
        (let* ((events (terrain:world-journal-events journal))
               (message (wire:make-events-message
                         :events (loop for index from (- mark (terrain:world-journal-watermark journal))
                                         below (length events)
                                       collect (event-wire session (aref events index))))))
          (dolist (guest (session-guests session))
            (net:send-message (session-endpoint session) message (car guest)))))
      (setf (session-broadcast-mark session) head))))

(defun reply-to-request (session peer message events reason)
  (net:send-message
   (session-endpoint session)
   (if events
       (wire:make-edit-reply-message :request (wire:edit-request-message-request message)
                                     :outcome :accepted
                                     :sequence (terrain:world-event-sequence (first events)))
       (wire:make-edit-reply-message :request (wire:edit-request-message-request message)
                                     :outcome :rejected :reason (wire-cause reason)))
   peer))

(defgeneric handle-session-message (session peer message)
  (:documentation "Act on one MESSAGE from PEER."))

(defmethod handle-session-message ((session world-session) peer message)
  (declare (ignore peer))
  (session-log session "Ignored a ~(~A~)." (type-of message)))

(defmethod handle-session-message ((session world-session) peer (message wire:chat-message))
  (declare (ignore peer))
  (session-log session "~A: ~A" (wire:chat-message-speaker message) (wire:chat-message-text message)))

(defmethod handle-session-message ((session host-session) peer (message wire:hello-message))
  (if (guest-peer-player session peer)
      (session-log session "Ignored a second hello on ~A." peer)
      (admit-guest session peer message)))

(defmethod handle-session-message ((session host-session) peer (message wire:edit-request-message))
  (let ((player (guest-peer-player session peer)))
    (when player
      (multiple-value-bind (events reason) (sequence-guest-request session player message)
        (when events
          (setf (world-springs-initialized-p (session-world session)) nil))
        ;; The events go out before their reply, on the same stream.
        (broadcast-journal session)
        (reply-to-request session peer message events reason)))))

(defmethod handle-session-message ((session host-session) peer (message wire:pose-message))
  (let ((player (guest-peer-player session peer)))
    ;; A guest speaks only for its own body.
    (when (and player (equal (player-id player) (wire:pose-message-player message)))
      (receive-pose session message)
      ;; The host simulates around every guest: its collision follows each
      ;; guest's latest pose (#I6O4J6).  A guest keeps only its own focus.
      (let ((position (wire:pose-position (wire:pose-message-pose message))))
        (terrain:set-streaming-scene-interest
         (session-scene session) (player-id player) (aref position 0) (aref position 1)))
      (dolist (guest (session-guests session))
        (unless (eq (car guest) peer)
          (net:send-message (session-endpoint session) message (car guest)))))))

(defmethod handle-session-message ((session host-session) peer (message wire:bye-message))
  (part-guest session peer (wire:bye-message-reason message))
  (net:close-peer (session-endpoint session) peer))

(defun handle-session-notice (session notice)
  (case (net:notice-kind notice)
    (:disconnected
     (if (typep session 'host-session)
         (part-guest session (net:notice-peer notice) (net:notice-detail notice))
         (progn
           (unless (eq (session-state session) :closed)
             (setf (session-state session) :disconnected))
           (session-log session "The host closed the connection (~A)." (net:notice-detail notice)))))
    (:refused (session-log session "Refused a message: ~A." (net:notice-detail notice)))))

;;; The guest.

(defun join-luvcraft-world (target &key (identity (default-player-identity))
                                     (name (default-player-name)) loopback (datagrams t)
                                     (timeout 15) scene-initargs)
  "Join the host at TARGET, \"host[:port]\" or a loopback host's name.

Raise the world from the host's welcome and return it, the local player
(admitted, without a character), the session, and the pose the host gave
the player, as a wire pose."
  (let ((endpoint (net:start-guest target :name name :loopback loopback :datagrams datagrams)))
    (handler-bind ((error (lambda (condition)
                            (declare (ignore condition))
                            (net:stop-endpoint endpoint))))
      (net:send-message endpoint (wire:make-hello-message :identity identity :name name))
      (let ((deadline (+ (wall-seconds) timeout)))
        (loop
          (let ((item (net:next-mailbox-item endpoint :timeout 1/4)))
            (typecase item
              (net:arrival
               (let ((message (net:arrival-message item)))
                 (typecase message
                   (wire:welcome-message
                    (return (start-guest-session endpoint (net:arrival-peer item) message
                                                 identity name scene-initargs)))
                   (wire:bye-message
                    (error "The host turned us away: ~A (~A)."
                           (wire:bye-message-text message) (wire:bye-message-reason message))))))
              (net:notice
               (when (member (net:notice-kind item) '(:disconnected :refused))
                 (error "Joining ~A failed: ~A ~A." target (net:notice-kind item)
                        (net:notice-detail item))))))
          (when (> (wall-seconds) deadline)
            (error "No welcome from ~A within ~A seconds." target timeout)))))))

(defun raise-welcomed-world (welcome)
  "The Luvcraft block world and Luft source WELCOME describes, with its edit
set installed at its watermark, and the remap of the host's vocabulary."
  (let* ((recipe (wire:welcome-message-recipe welcome))
         (parameters (wire:recipe-parameters recipe)))
    (unless (and (string= (wire:recipe-generator recipe) "luvcraft-little-world")
                 (= (wire:recipe-version recipe) game::+luvcraft-generator-version+))
      (error "The host's world is ~A version ~D; this image raises luvcraft-little-world ~D."
             (wire:recipe-generator recipe) (wire:recipe-version recipe)
             game::+luvcraft-generator-version+))
    (destructuring-bind (&key relief height little-class offset domain) parameters
      (declare (ignore domain))
      (let* ((seed (wire:recipe-seed recipe))
             (little (make-instance (recipe-little-class little-class)
                                    :seed seed :relief relief))
             (block-world (luvcraft::make-block-world
                           :id (list :little-world seed) :chunk-width 16 :chunk-height height
                           :chunk-depth 16 :source little))
             (remap (luvcraft:resolve-material-vocabulary
                     (map 'simple-vector (lambda (name) (intern name '#:keyword))
                          (wire:welcome-message-vocabulary welcome))))
             (overlay (luvcraft:little-world-source-edits little)))
        (dolist (placement (wire:welcome-message-edits welcome))
          (let ((site (wire:placement-site placement)))
            (luvcraft::record-block-edit overlay (wire-identity (wire:placement-block placement) remap)
                                         (- (wire:site-x site) offset) (wire:site-z site)
                                         (- (wire:site-y site) offset))))
        (let ((source (game:luvcraft-world-luft-source
                       block-world :offset offset
                                   :watermark (wire:welcome-message-watermark welcome))))
          (unless (string= (terrain:world-source-tag source)
                           (tag-hex (wire:message-world-tag welcome)))
            (error "This image raises world ~A from the host's recipe, but the host plays ~A."
                   (terrain:world-source-tag source) (tag-hex (wire:message-world-tag welcome))))
          (values block-world source remap))))))

(defun start-guest-session (endpoint peer welcome identity name scene-initargs)
  (multiple-value-bind (block-world source remap) (raise-welcomed-world welcome)
    (let* ((scene (apply #'game:make-luvcraft-streaming-scene source scene-initargs))
           (world (make-luvcraft-world :block-world block-world :scene scene))
           (player (add-world-player world (make-luvcraft-player :id identity :name name)))
           (offsets (make-hash-table :test #'eq))
           (watermark (wire:welcome-message-watermark welcome)))
      (loop for name in (wire:welcome-message-vocabulary welcome)
            for offset from 0
            do (setf (gethash (intern name '#:keyword) offsets) offset))
      (let ((session (make-instance 'guest-session
                                    :world world :endpoint endpoint :local-player player
                                    :peer peer :remap remap :host-offsets offsets
                                    :watermark watermark :applied watermark)))
        (setf (world-session world) session)
        (session-log session "Joined ~A at sequence ~D with ~D edits."
                     (net::endpoint-target endpoint) watermark
                     (length (wire:welcome-message-edits welcome)))
        (values world player session
                (wire:player-record-pose (wire:welcome-message-player welcome)))))))

(defun translate-cause (session cause)
  "CAUSE with the host's transaction ids in an undo or redo made this journal's."
  (if (and (consp cause) (member (first cause) '(:undo :redo)) (integerp (second cause)))
      (list (first cause) (gethash (second cause) (session-transactions session) (second cause)))
      cause))

(defun account-confirmed-events (session events)
  "Spend and pick up the carried blocks of the local player's confirmed EVENTS."
  (let* ((player (session-local-player session))
         (inventory (luvcraft:player-inventory player)))
    (dolist (event events)
      (when (equal (terrain:world-event-actor event) (player-id player))
        (let ((before (values (journal-value-block (terrain:world-event-before event))))
              (after (values (journal-value-block (terrain:world-event-after event)))))
          (when (and after (luvcraft:block-kind-carried-p after))
            (luvcraft:remove-block-from-inventory inventory after))
          (when (and before (luvcraft:block-kind-carried-p before))
            (luvcraft:add-block-to-inventory inventory before)))))))

(defun record-host-events (session events)
  "Record the host's EVENTS, oldest first, strictly after what this journal has."
  (let* ((scene (session-scene session))
         (journal (terrain:streaming-scene-journal scene))
         (remap (session-remap session))
         (world (session-world session)))
    (loop while events
          do (let* ((transaction (wire:event-transaction (first events)))
                    (batch (loop while (and events (= transaction (wire:event-transaction (first events))))
                                 collect (pop events)))
                    (sequence (wire:event-sequence (first batch))))
               (cond
                 ((<= sequence (session-applied session))
                  (session-log session "Dropped event ~D, which is already here." sequence))
                 ((/= sequence (1+ (session-applied session)))
                  (error "The host sent event ~D after ~D: the journal would have a gap."
                         sequence (session-applied session)))
                 (t
                  (let* ((first (first batch))
                         (recorded (terrain:record-world-events
                                    journal
                                    (loop for event in batch
                                          collect (list (wire-cell world (wire:event-site event))
                                                        (wire-identity (wire:event-before event) remap)
                                                        (wire-identity (wire:event-after event) remap)))
                                    :actor (principal-actor (wire:event-actor first))
                                    :tick (let ((tick (wire:event-tick first))) (and (plusp tick) tick))
                                    :cause (translate-cause session (wire:event-cause first)))))
                    (assert (= (terrain:world-event-sequence (first recorded)) sequence))
                    (setf (gethash transaction (session-transactions session))
                          (terrain:world-event-transaction (first recorded))
                          (terrain:streaming-scene-pending-events scene)
                          (append (terrain:streaming-scene-pending-events scene) recorded)
                          (session-applied session)
                          (terrain:world-event-sequence (car (last recorded))))
                    (account-confirmed-events session recorded))))))))

(defmethod handle-session-message ((session guest-session) peer (message wire:events-message))
  (declare (ignore peer))
  (record-host-events session (wire:events-message-events message)))

(defmethod handle-session-message ((session guest-session) peer (message wire:edit-reply-message))
  (declare (ignore peer))
  (push message (session-replies session))
  (when (eq (wire:edit-reply-message-outcome message) :rejected)
    (let ((viewer (session-viewer session)))
      (when viewer
        (setf (render:viewer-last-edit-status viewer) (wire:edit-reply-message-reason message))))))

(defmethod handle-session-message ((session guest-session) peer (message wire:player-update-message))
  (declare (ignore peer))
  (let* ((record (wire:player-update-message-player message))
         (id (wire:player-record-identity record)))
    (unless (equal id (player-id (session-local-player session)))
      (ecase (wire:player-update-message-presence message)
        ((:joined :present) (ensure-remote-player session id (wire:player-record-name record)))
        (:left (forget-remote-player session id))))))

(defmethod handle-session-message ((session guest-session) peer (message wire:pose-message))
  (declare (ignore peer))
  (receive-pose session message))

(defmethod handle-session-message ((session guest-session) peer (message wire:bye-message))
  (declare (ignore peer))
  (setf (session-state session) :closed)
  (session-log session "The host said goodbye: ~A." (wire:bye-message-reason message)))

;;; Chunk hashes (#RELI34).  Hashing one chunk takes tens of milliseconds,
;;; so the frame only captures resident chunks, which are immutable values,
;;; and a worker hashes them and posts the result back for the next frame.

(defun session-watermark-now (session)
  "The host sequence this world stands at, or NIL while events are queued."
  (and (null (terrain:streaming-scene-pending-events (session-scene session)))
       (session-journal-head session)))

(defun chunk-hash-octets (hex)
  (let ((octets (wire:make-octets (/ (length hex) 2))))
    (dotimes (index (length octets) octets)
      (setf (aref octets index) (parse-integer hex :start (* 2 index) :end (+ 2 (* 2 index))
                                                   :radix 16)))))

(defun resident-chunk (scene key)
  (let ((value (gethash key (terrain:streaming-scene-store scene))))
    (and (typep value 'terrain:resident-cell-chunk) value)))

(defun hash-in-background (session residents finish)
  "Hash RESIDENTS, a list of (KEY . RESIDENT), on a worker; a later frame
calls FINISH with the list of (KEY . HEX).  While the worker is busy the job
waits its turn: a peer's report that arrives then is still compared."
  (if (session-hash-job session)
      (setf (session-hash-queue session)
            (append (session-hash-queue session) (list (cons residents finish))))
      (setf (session-hash-job session)
            (sb-thread:make-thread
             (lambda ()
               (let ((hashes (handler-case
                                 (loop for (key . resident) in residents
                                       collect (cons key (terrain:resident-cell-chunk-hash resident)))
                               (error (condition) condition))))
                 (sb-concurrency:send-message (session-hash-results session)
                                              (cons finish hashes))))
             :name "luvcraft chunk hashes"))))

(defun finish-chunk-hashes (session)
  "Act on what the hashing worker has posted."
  (loop for (finish . hashes) = (sb-concurrency:receive-message-no-hang
                                 (session-hash-results session))
        while finish
        do (setf (session-hash-job session) nil)
           (if (typep hashes 'error)
               (session-log session "Chunk hashing failed: ~A" hashes)
               (funcall finish hashes))
           (let ((next (pop (session-hash-queue session))))
             (when next (hash-in-background session (car next) (cdr next))))))

(defun report-chunk-hashes (session)
  "Send peers the hashes of a few resident chunks, in turn, at this world's
journal point.  NIL while events wait to be applied."
  (let* ((scene (session-scene session))
         (watermark (session-watermark-now session))
         (keys (and watermark
                    (sort (loop for key being the hash-keys of (terrain:streaming-scene-store scene)
                                when (resident-chunk scene key) collect key)
                          #'<))))
    (when keys
      (hash-in-background
       session
       (loop repeat (min *chunk-hashes-per-report* (length keys))
             for key = (nth (mod (session-hash-cursor session) (length keys)) keys)
             do (incf (session-hash-cursor session))
             collect (cons key (resident-chunk scene key)))
       (lambda (hashes)
         (session-broadcast
          session (wire:make-chunk-hashes-message
                   :watermark watermark
                   :hashes (loop for (key . hex) in hashes
                                 collect (wire:make-chunk-hash
                                          :chunk (wire:make-site (luft:chunk-key-x key)
                                                                 (luft:chunk-key-y key) 0)
                                          :digest (chunk-hash-octets hex)))))))
      t)))

(defun compare-chunk-hashes (session message)
  "Compare a peer's chunk hashes with this world's own at the same journal
point, and log every one that differs."
  (let ((scene (session-scene session))
        (watermark (wire:chunk-hashes-message-watermark message))
        (theirs (make-hash-table)))
    (when (eql watermark (session-watermark-now session))
      (let ((residents
              (loop for hash in (wire:chunk-hashes-message-hashes message)
                    for chunk = (wire:chunk-hash-chunk hash)
                    for key = (luft:chunk-key-at (* luft:+chunk-size+ (wire:site-x chunk))
                                                 (* luft:+chunk-size+ (wire:site-y chunk)))
                    for resident = (resident-chunk scene key)
                    when resident
                      do (setf (gethash key theirs) (tag-hex (wire:chunk-hash-digest hash)))
                      and collect (cons key resident))))
        (when residents
          (hash-in-background
           session residents
           (lambda (hashes)
             (loop for (key . ours) in hashes
                   for other = (gethash key theirs)
                   do (if (string= ours other)
                          (incf (session-matched-chunks session))
                          (progn
                            (push (list key ours other watermark)
                                  (session-mismatched-chunks session))
                            (session-log session "Chunk ~D ~D differs from the peer's at ~
                                                  sequence ~D: ours ~A, theirs ~A.  A ~
                                                  determinism bug (#RELI34)."
                                         (luft:chunk-key-x key) (luft:chunk-key-y key)
                                         watermark ours other)))))))))))

(defmethod handle-session-message ((session world-session) peer (message wire:chunk-hashes-message))
  (declare (ignore peer))
  (compare-chunk-hashes session message))

;;; The frame boundary.

(defgeneric session-broadcast (session message)
  (:documentation "Send MESSAGE to every peer SESSION speaks with."))

(defmethod session-broadcast ((session host-session) message)
  (dolist (guest (session-guests session))
    (net:send-message (session-endpoint session) message (car guest))))

(defmethod session-broadcast ((session guest-session) message)
  (when (eq (session-state session) :open)
    (net:send-message (session-endpoint session) message (session-peer session))))

(defun drain-session (session)
  "Act on everything SESSION's endpoint has received."
  (dolist (item (net:drain-mailbox (session-endpoint session)))
    (etypecase item
      (net:arrival (handle-session-message session (net:arrival-peer item) (net:arrival-message item)))
      (net:notice (handle-session-notice session item)))))

(defun advance-world-session (session seconds &key yaw pitch)
  "At a frame boundary: act on what arrived, broadcast new events, send the
local pose when due, and move remote players' tracks on by SECONDS.  YAW
and PITCH are the local player's look, when a view knows it."
  (drain-session session)
  (finish-chunk-hashes session)
  (when (typep session 'host-session)
    (broadcast-journal session))
  (unless (session-viewer session)
    ;; A view applies queued events in its own frame and meshes them.
    (terrain:apply-queued-world-events (session-scene session) nil nil))
  (let ((now (wall-seconds)))
    (when (>= now (session-pose-due session))
      (setf (session-pose-due session) (+ (max now (session-pose-due session)) *pose-period*))
      (let ((pose (local-pose session :yaw yaw :pitch pitch)))
        (when pose
          (session-broadcast session
                             (wire:make-pose-message
                              :player (player-id (session-local-player session)) :pose pose)))))
    (when (>= now (session-hash-due session))
      (setf (session-hash-due session) (+ now *chunk-hash-period*))
      (report-chunk-hashes session)))
  (advance-remote-tracks session seconds)
  session)

(defun close-world-session (session &key (reason :bye))
  "Say goodbye, stop the endpoint, and detach SESSION from its world."
  (unless (eq (session-state session) :closed)
    (ignore-errors
     (session-broadcast session (wire:make-bye-message :reason reason :text "Goodbye.")))
    (setf (session-state session) :closed))
  (net:stop-endpoint (session-endpoint session))
  (dolist (player (session-remote-players session))
    (terrain:remove-streaming-scene-interest (session-scene session) (player-id player))
    (remove-world-player (session-world session) player))
  (when (eq (world-session (session-world session)) session)
    (setf (world-session (session-world session)) nil))
  session)

(defun session-status (session)
  "A plist describing SESSION for ./sly status."
  (list :role (if (typep session 'host-session) :host :guest)
        :state (session-state session)
        :player (player-id (session-local-player session))
        :port (net:endpoint-port (session-endpoint session))
        :peers (mapcar #'player-id (session-remote-players session))
        :head (session-journal-head session)
        :matched-chunks (session-matched-chunks session)
        :mismatched-chunks (length (session-mismatched-chunks session))))
