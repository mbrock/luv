;;;; The players table and shared companions (#EA88ON), over the loopback
;;;; network.  These use the session tests' worlds and pumps.
(in-package #:luvcraft.luft.game.tests)

(defun world-from-save (pathname)
  "A host world raised from the save at PATHNAME, its first chunk resident,
with Ada, its owner, standing where her record says."
  (multiple-value-bind (block-world resume inhabitants journal) (luvcraft:read-luvcraft-save pathname)
    (multiple-value-bind (record others)
        (host::adopt-saved-players (getf journal :players) resume "ada")
      (let* ((source (game:luvcraft-world-luft-source block-world :journal journal))
             (world (host:make-luvcraft-world
                     :block-world block-world :inhabitants inhabitants :player-records others
                     :scene (make-resident (game:make-luvcraft-streaming-scene source)
                                           (list *session-chunk*)))))
        (host:add-world-player
         world (host:make-luvcraft-player
                :id "ada" :name "Ada" :record record
                :character (terrain:make-walking-character
                            :position (apply #'v:make-vec3
                                             (game:luvcraft-resume-luft-pose source record)))))
        world))))

(defun body-place (character)
  (v:vec3-list (terrain:body-position (terrain:character-body character))))

(defun distance (a b)
  (sqrt (reduce #'+ (mapcar (lambda (x y) (expt (- x y) 2)) a b))))

(define-test a-guest-finds-its-stock-where-it-left-it-across-a-host-reload
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (let ((film (luvcraft::ensure-film "players-table-rejoin" :title "Bea's film"))
          (hotbar nil) (where nil))
      (with-host (world session network)
        (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
          (setf (host:player-character bea)
                (terrain:make-walking-character
                 :position (v:make-vec3 1050.5d0 1044.5d0 (float (ground guest-world 1050 1044) 1d0))))
          (host:add-world-player guest-world bea)
          ;; Bea carries two films, plays creatively, and holds a film,
          ;; which she has put on her number row.
          (luvcraft:add-block-to-inventory (luvcraft:player-inventory bea) film 2)
          (setf (luvcraft:player-creative-p bea) t)
          (host::put-on-hotbar bea (game:block-placement film))
          (setf (host:player-held-block bea) film
                hotbar (host:player-hotbar bea))
          (true (member (game:block-placement film) hotbar))
          ;; The host learns her record, and sees her body.
          (true (pump (list session guest) :advance t
                      :until (lambda ()
                               (let ((there (host:find-world-player world "bea")))
                                 (and there (host:track-shown (host:player-track there))
                                      (find film (getf (host::player-saved-record there) :inventory)
                                            :key #'first))))))
          (setf where (body-place (host:player-character bea)))
          (host:close-world-session guest)
          (true (pump (list session) :until (lambda () (null (host:find-world-player world "bea")))))
          ;; The host saves with Bea gone; her record stays in its table.
          (host:save-luvcraft-world world (host:find-world-player world "ada") pathname)))
      (multiple-value-bind (block-world resume inhabitants journal) (luvcraft:read-luvcraft-save pathname)
        (declare (ignore block-world inhabitants))
        (is eq nil resume)
        (let ((players (getf journal :players)))
          (is equal '("ada" "bea") (mapcar (lambda (record) (getf record :id)) players))
          ;; In the file, blocks are offsets into the save's own tables.
          (with-open-file (stream pathname)
            (let* ((*read-eval* nil)
                   (written (find "bea" (getf (rest (read stream)) :players)
                                  :key (lambda (record) (getf record :id)) :test #'equal)))
              (true (every (lambda (stack) (typep (first stack) '(or integer (cons integer))))
                           (getf written :inventory)))
              (true (consp (getf written :selected-block)))))))
      ;; The host comes back from its save, and Bea joins again.
      (let* ((network (luvcraft.wire.net:make-loopback-network))
             (world (world-from-save pathname))
             (session (host:host-luvcraft-world world (host:find-world-player world "ada")
                                                :loopback network :name "host")))
        (unwind-protect
             (multiple-value-bind (guest-world bea guest pose)
                 (join-resident session network "bea" "Bea")
               (declare (ignore guest-world))
               (let ((entry (luvcraft:block-inventory-entry-for (luvcraft:player-inventory bea) film)))
                 (true entry)
                 (is = 2 (luvcraft:block-inventory-entry-quantity entry)))
               (true (luvcraft:player-creative-p bea))
               (is eq film (host:player-held-block bea))
               (is equal hotbar (host:player-hotbar bea))
               (true (< (distance (map 'list #'identity (luvcraft.wire:pose-position pose)) where)
                        0.5))
               ;; Her record left the absent table: she is here.
               (is eq nil (host::absent-player-record world "bea"))
               (host:close-world-session guest))
          (host:close-world-session session))))))

;;; Companions.

(defun scripted-companion-factory (prompts)
  "A provider seam whose companion answers every prompt at once, noting it in
the cons PROMPTS; only the network is replaced."
  (lambda (application inhabitant)
    (make-instance 'host::inhabitant-agent
                   :application application :inhabitant inhabitant :model "scripted"
                   :close-function (lambda (agent) (declare (ignore agent)) nil)
                   :turn-function
                   (lambda (agent prompt)
                     (push prompt (car prompts))
                     (setf (openai:agent-response-id agent) "scripted-response")
                     (make-instance 'openai:agent-response
                                    :id "scripted-response" :usage nil :reasoning ""
                                    :text (format nil "Hej! You said ~S." prompt))))))

(defun summon-test-companion (world x y)
  (host::initialize-world-agents world)
  (let ((companion (host::make-inhabitant
                    world :gnome (v:make-vec3 (+ x 0.5d0) (+ y 0.5d0) (float (ground world x y) 1d0)))))
    (push companion (host::world-agents world))
    companion))

(defun pump-companions (sessions &rest arguments &key (until (constantly nil)) &allow-other-keys)
  "PUMP SESSIONS, moving every world's companions along with them."
  (apply #'pump sessions
         :until (lambda ()
                  (dolist (session sessions)
                    (host::advance-world-companions (host:session-world session) 1/60))
                  (funcall until))
         arguments))

(defun last-actor (world)
  (let ((events (terrain:world-journal-events
                 (terrain:world-source-journal (host:world-source world)))))
    (terrain:world-event-actor (aref events (1- (length events))))))

(define-test a-guest-talks-to-the-hosts-companion-and-both-see-it-answer
  (let* ((prompts (list nil))
         (host::*inhabitant-provider-factory* (scripted-companion-factory prompts)))
    (with-host (world session network)
      (let ((gnome (summon-test-companion world 1040 1040)))
        (multiple-value-bind (guest-world bea guest) (join-resident session network "bea" "Bea")
          (declare (ignore bea))
          ;; The guest sees the host's companion, under the same id.
          (let ((mirror nil))
            (true (pump-companions (list session guest)
                                   :until (lambda ()
                                            (setf mirror (host::find-world-inhabitant
                                                          guest-world (host::inhabitant-id gnome))))))
            (true (host::inhabitant-mirror-p mirror))
            (is eq :gnome (host::inhabitant-kind mirror))
            ;; Bea asks; the host's companion hears her, and both see its reply.
            (host::tell-inhabitant guest-world mirror "Hello there")
            (true (pump-companions
                   (list session guest)
                   :until (lambda ()
                            (equal "Hej! You said \"Hello there\"." (host::inhabitant-said mirror)))))
            (is equal '("Hello there") (car prompts))
            (is equal (host::inhabitant-said gnome) (host::inhabitant-said mirror))
            (is equal "bea" (host::inhabitant-attending gnome))
            (is equal "bea" (host::inhabitant-attending mirror))
            (is equal "Hello there" (host::inhabitant-asked mirror))
            (true (pump-companions (list session guest)
                                   :until (lambda () (eq :idle (host::inhabitant-activity mirror)))))
            ;; Its body comes to where the host's is, a little behind time.
            (true (pump-companions
                   (list session guest)
                   :until (lambda ()
                            (< (distance (body-place (host::inhabitant-character gnome))
                                         (body-place (host::inhabitant-character mirror)))
                               0.05))))
            ;; Its edit is the companion's on both sides, by its stable id.
            (let ((cell (air-cell world 1041 1042 0))
                  (stone (game:block-placement luvcraft::*stone-block*)))
              (is eq :edited (nth-value 1 (host::edit-agent-cell world gnome cell stone)))
              (true (pump-companions (list session guest)
                                     :until (lambda ()
                                              (eq stone (terrain:scene-material-placement-at
                                                         (host:world-scene guest-world) cell)))))
              (is equal (list :inhabitant (host::inhabitant-id gnome)) (last-actor world))
              (is equal (last-actor world) (last-actor guest-world))))
          (host:close-world-session guest)))
      (host:shutdown-luvcraft-world world))))

(define-test a-companions-edits-keep-their-actor-across-a-reload
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (let ((stone (game:block-placement luvcraft::*stone-block*))
          (actor nil))
      (let* ((world (session-test-world))
             (gnome (summon-test-companion world 1040 1040)))
        (is eq :edited (nth-value 1 (host::edit-agent-cell world gnome (air-cell world 1041 1042 0)
                                                           stone)))
        (setf actor (last-actor world))
        (is equal (list :inhabitant (host::inhabitant-id gnome)) actor)
        (host:save-luvcraft-world world (host:find-world-player world "ada") pathname))
      (let ((world (world-from-save pathname)))
        (host::initialize-world-agents world)
        (let ((gnome (first (host::world-agents world))))
          (is equal (second actor) (host::inhabitant-id gnome))
          (is eq :edited (nth-value 1 (host::edit-agent-cell world gnome (air-cell world 1042 1041 0)
                                                             stone)))
          (is equal actor (last-actor world))
          ;; Undo is the companion's own, across the reload.
          (is eq :edited (nth-value 1 (terrain:undo-world-events (host:world-scene world) actor)))
          (is eq nil (terrain:scene-material-placement-at (host:world-scene world)
                                                          (air-cell world 1042 1041 0))))))))

(define-test a-companion-that-fell-out-of-the-world-does-not-stop-a-save
  (uiop:with-temporary-file (:pathname pathname :type "sexp")
    (let* ((world (session-test-world))
           (fine (summon-test-companion world 1040 1040))
           (fallen (summon-test-companion world 1044 1040)))
      ;; As one did after hours of falling below published ground.
      (setf (terrain:body-position (terrain:character-body (host::inhabitant-character fallen)))
            (v:make-vec3 1044.5d0 1040.5d0 -6.4348533811d10))
      (let ((*error-output* (make-string-output-stream)))
        (host:save-luvcraft-world world (host:find-world-player world "ada") pathname)
        (true (search (host::inhabitant-id fallen) (get-output-stream-string *error-output*))))
      (multiple-value-bind (block-world resume inhabitants) (luvcraft:read-luvcraft-save pathname)
        (declare (ignore block-world resume))
        (is equal (list (host::inhabitant-id fine))
            (loop for description in inhabitants
                  when (eq :gnome (first description))
                    collect (getf (rest description) :id)))))))

(defclass test-view () ()
  (:documentation "Where a companion's tools would run: here, at once."))

(defmethod luv.application-agent:call-in-application-frame ((view test-view) function)
  (funcall function))

(define-test closing-a-view-lets-go-of-companions-and-the-world-retires-them
  (let* ((prompts (list nil))
         (host::*inhabitant-provider-factory* (scripted-companion-factory prompts))
         (world (session-test-world))
         (view (make-instance 'test-view))
         (gnome (summon-test-companion world 1040 1040)))
    (push view (host::world-views world))
    (host::tell-inhabitant view gnome "First" :asker "ada")
    (true (loop repeat 400
                do (host::advance-world-companions world 1/60)
                   (sleep 0.005)
                thereis (equal "Hej! You said \"First\"." (host::inhabitant-said gnome))))
    ;; The view closes: its provider goes, the companion stays, and its
    ;; conversation goes on with the next provider.
    (host::detach-world-view world view)
    (is eq nil (host::world-views world))
    (is eq nil (host::inhabitant-provider gnome))
    (is eq nil (host::inhabitant-retired-p gnome))
    (is equal "scripted-response" (host::inhabitant-conversation gnome))
    (host::tell-inhabitant world gnome "Second" :asker "ada")
    (true (loop repeat 400
                do (host::advance-world-companions world 1/60)
                   (sleep 0.005)
                thereis (equal "Hej! You said \"Second\"." (host::inhabitant-said gnome))))
    ;; The world's shutdown retires it.
    (host:shutdown-luvcraft-world world)
    (true (host::inhabitant-retired-p gnome))
    (is eq nil (host::inhabitant-provider gnome))
    (fail (host::tell-inhabitant world gnome "Third" :asker "ada"))))
