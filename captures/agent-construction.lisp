(in-package #:luv.showcase)

;;; One paid, network-backed gnome turn becomes a durable source-owned cassette.
;;; Routine gallery renders replay its typed transcript and approved world edits;
;;; they never counterfeit a provider run and never silently spend API credit.
;;; #DT23EK #YY6EYZ

(defparameter +classic-arch-agent-prompt+
  "Build a classic stone-brick arch in the clearing. Use exactly three propose-block-box calls, one at a time: left pillar from (6,7,11) to (6,10,11), right pillar from (10,7,11) to (10,10,11), then lintel from (6,11,11) to (10,11,11). Use stone-bricks. Wait for each approval. Then use say to tell me what you made. Do not use eval or place-block-at.")

(defun authentic-agent-call-record (call)
  "Reduce one real TOOL-CALL to stable, readable replay data."
  (list :tool (agent::tool-call-name call)
        :arguments (copy-tree (agent:tool-call-arguments call))
        :status (agent:tool-call-status call)
        :output (agent:tool-call-output call)
        :elapsed-seconds
        (coerce (agent:tool-call-elapsed-seconds call) 'single-float)))

(defun authentic-agent-turn-record (turn)
  "Reduce a completed real TURN without retaining provider/socket objects."
  (list :prompt (agent:turn-prompt turn)
        :thought (agent:turn-thought turn)
        :text (agent:turn-text turn)
        :status (agent:turn-status turn)
        :calls (mapcar #'authentic-agent-call-record
                       (agent::turn-calls-in-order turn))))

(defun approve-authentic-agent-proposal (session approval)
  "Approve APPROVAL on the hidden session's real canvas owner."
  (luv:request-canvas-frame
   (luvcraft:luvcraft-session-canvas session)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (agent:approve-tool-approval approval))))

(defun deny-authentic-agent-proposal (session approval note)
  "Deny APPROVAL on the hidden session's real canvas owner."
  (luv:request-canvas-frame
   (luvcraft:luvcraft-session-canvas session)
   (lambda (timestamp)
     (declare (ignore timestamp))
     (agent:deny-tool-approval approval note))))

(defun wait-for-authentic-agent-turn (session gnome turn &key (timeout 180))
  "Drive one real agent turn, approving each distinct proposed construction."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second)))
        (next-report (get-internal-real-time))
        (last-approval nil)
        (approval-count 0))
    (loop
      (let ((approval (agent:embodied-agent-pending-approval gnome)))
        (when (and approval
                   (not (eq approval last-approval))
                   (eq :proposed (agent:tool-approval-state approval)))
          (setf last-approval approval)
          (incf approval-count)
          (format t "authentic agent: approving proposal ~D...~%"
                  approval-count)
          (finish-output)
          (approve-authentic-agent-proposal session approval)))
      (when (member (agent:turn-status turn) '(:done :failed))
        (return))
      (when (>= (get-internal-real-time) next-report)
        (format t "authentic agent: ~(~A~), ~D tool call~:P, ~D approval~:P...~%"
                (agent:turn-status turn) (length (agent:turn-calls turn))
                approval-count)
        (finish-output)
        (incf next-report internal-time-units-per-second))
      (when (>= (get-internal-real-time) deadline)
        (alexandria:when-let
            ((approval (agent:embodied-agent-pending-approval gnome)))
          (deny-authentic-agent-proposal
           session approval
           "The authored capture timed out waiting for the model."))
        (error "Authentic classic-arch turn exceeded ~D seconds." timeout))
      (sleep 0.05))
    (sb-thread:join-thread (agent::turn-thread turn) :default nil :timeout 2)
    (when (eq :failed (agent:turn-status turn))
      (error "Authentic classic-arch turn failed: ~A"
             (agent::turn-error turn)))
    (unless (= 3 approval-count)
      (error "Authentic classic-arch turn made ~D approved proposal~:P, not 3."
             approval-count))
    turn))

(defun run-authentic-classic-arch-agent-turn ()
  "Make the one explicitly requested real API turn and return its cassette.

This is intentionally not a capture renderer.  Invoke it deliberately with an
OPENAI_API_KEY; copy the returned stable record into the replay fixture only
after reviewing the actual calls and approved edits."
  (call-with-character-instrument-session
   (lambda (session)
     (let* ((gnome (spawn-capture-gnome session))
            (agent (agent::ensure-embodied-agent-agent gnome)))
       (luvcraft:focus-luvcraft-session session gnome)
       (agent:open-agent-hud :agent agent)
       (let ((turn (agent::gnome-ask gnome +classic-arch-agent-prompt+)))
         (wait-for-authentic-agent-turn session gnome turn)
         (agent:print-transcript turn)
         (authentic-agent-turn-record turn))))
   :title "authentic gnome builds a classic arch"
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :day-fraction 0.42))

(defparameter +classic-arch-agent-trace+
  '(:model "gpt-5.6"
    :captured-at "2026-08-21"
    :prompt
    "Build a classic stone-brick arch in the clearing. Use exactly three propose-block-box calls, one at a time: left pillar from (6,7,11) to (6,10,11), right pillar from (10,7,11) to (10,10,11), then lintel from (6,11,11) to (10,11,11). Use stone-bricks. Wait for each approval. Then use say to tell me what you made. Do not use eval or place-block-at."
    :thought
    "**Planning sequential calls**

I'm thinking about the need to perform three sequential calls, doing them one at a time and waiting between each. It’s important to ensure that my approach is organized and methodical. I'll focus on executing each call successfully before moving on to the next. That way, I can make sure everything runs smoothly without any hiccups. There's something satisfying about completing tasks in a sequential manner!"
    :calls
    ((:tool "propose-block-box"
      :arguments ((:kind . "stone-bricks")
                  (:x-1 . 6) (:y-1 . 7) (:z-1 . 11)
                  (:x-2 . 6) (:y-2 . 10) (:z-2 . 11))
      :status :ok
      :output
      "Construction proposal approved: 4 blocks in [6,7,11]..[6,10,11] #BABA"
      :elapsed-seconds 0.020418)
     (:tool "propose-block-box"
      :arguments ((:kind . "stone-bricks")
                  (:x-1 . 10) (:y-1 . 7) (:z-1 . 11)
                  (:x-2 . 10) (:y-2 . 10) (:z-2 . 11))
      :status :ok
      :output
      "Construction proposal approved: 4 blocks in [10,7,11]..[10,10,11] #CABA"
      :elapsed-seconds 0.020891)
     (:tool "propose-block-box"
      :arguments ((:kind . "stone-bricks")
                  (:x-1 . 6) (:y-1 . 11) (:z-1 . 11)
                  (:x-2 . 10) (:y-2 . 11) (:z-2 . 11))
      :status :ok
      :output
      "Construction proposal approved: 5 blocks in [6,11,11]..[10,11,11] #DABA"
      :elapsed-seconds 0.025046)
     (:tool "say"
      :arguments
      ((:text .
        "There we are—a classic stone-brick arch, with two stout pillars and a solid lintel!"))
      :status :ok :output "said" :elapsed-seconds 0.012685)))
  "Reviewed cassette from the successful real provider turn above. #DT23EK")

(defconstant +classic-arch-replay-seconds+ 11)
(defconstant +classic-arch-replay-frame-rate+ 20)

(defun classic-arch-trace-command (call-record)
  (cond ((string= "propose-block-box" (getf call-record :tool))
         'agent:com-propose-block-box)
        ((string= "say" (getf call-record :tool)) 'agent:com-say)
        (t (error "Unknown classic-arch cassette tool ~S."
                  (getf call-record :tool)))))

(defun make-classic-arch-replay-call (call-record)
  (let* ((tool
           (agent::make-command-tool
            (classic-arch-trace-command call-record)))
         (call
           (make-instance 'agent:tool-call :tool tool
                           :arguments (copy-tree
                                       (getf call-record :arguments)))))
    (setf (agent:tool-call-command call)
          (agent::command-tool-parse
           tool (agent:tool-call-arguments call))
          ;; The real call elapsed values are part of the cassette.  Supplying
          ;; FINISHED while STATUS is still :RUNNING keeps offline encoder wall
          ;; time from leaking into the HUD before the replayed completion.
          (agent::tool-call-finished call)
          (+ (agent::tool-call-started call)
             (round (* (getf call-record :elapsed-seconds)
                       internal-time-units-per-second))))
    call))

(defun classic-arch-replay-camera-pose ()
  "Frame the gnome on the left, the arch behind it, and the HUD on the right."
  (gallery-look-pose
   3.2d0 (+ +gallery-stage-floor-y+ 3.4d0) 2.5d0
   8.0d0 (+ +gallery-stage-floor-y+ 2.2d0) 10.0d0
   (* 0.92d0 luvcraft::+luvcraft-camera-vertical-field-of-view+)))

(defun attach-classic-arch-replay-agent (session gnome)
  "Attach a credential-free agent whose typed records are cassette-driven."
  (let ((agent
          (make-instance
           'agent:world-agent
           :session session :model (getf +classic-arch-agent-trace+ :model)
           :instructions agent::*gnome-instructions*
           :tools (mapcar #'agent::make-command-tool agent::*gnome-tools*)
           ;; The cassette never calls AGENT-TURN.  NIL is an explicit inert
           ;; transport, avoiding MAKE-WORLD-AGENT's eager WebSocket entirely.
           :socket nil)))
    (setf (agent:world-agent-presence agent) gnome
          (agent::gnome-agent gnome) agent
          (agent::gnome-observer gnome) (agent::make-gnome-observer gnome)
          agent:*agent* agent)
    (agent::add-agent-observer agent (agent::gnome-observer gnome))
    agent))

(defun start-classic-arch-replay-call (agent gnome call)
  "Publish CALL and install its real possible-world approval when applicable."
  (agent::note-tool-call agent call)
  (when (eq 'agent:com-propose-block-box
            (car (agent:tool-call-command call)))
    (destructuring-bind (kind x1 y1 z1 x2 y2 z2)
        (cdr (agent:tool-call-command call))
      (let ((approval
              (make-instance
               'agent::construction-approval
               :agent agent :presence gnome
               :session (agent:world-agent-session agent)
               :change-set
               (agent::make-additive-box-change-set
                (luvcraft:luvcraft-session-world
                 (agent:world-agent-session agent))
                kind x1 y1 z1 x2 y2 z2))))
        (agent::install-construction-approval approval)
        approval))))

(defun finish-classic-arch-replay-call
    (agent gnome call call-record approval)
  "Apply the cassette's recorded successful terminal state to CALL."
  (when approval
    (agent:approve-tool-approval approval))
  (when (eq 'agent:com-say (car (agent:tool-call-command call)))
    (agent:gnome-say gnome
                     (cdr (assoc :text (getf call-record :arguments)))))
  (setf (agent:tool-call-output call) (getf call-record :output)
        (agent:tool-call-status call) (getf call-record :status)
        (agent::tool-call-finished call)
        (+ (agent::tool-call-started call)
           (round (* (getf call-record :elapsed-seconds)
                     internal-time-units-per-second))))
  (agent::note-tool-call-finished agent call)
  call)

(defun film-classic-arch-agent-replay (session pathname gnome agent)
  "Film the reviewed provider cassette through the real game surfaces."
  (let* ((trace-calls (getf +classic-arch-agent-trace+ :calls))
         (calls (mapcar #'make-classic-arch-replay-call trace-calls))
         (turn (make-instance 'agent:turn
                              :prompt (getf +classic-arch-agent-trace+
                                            :prompt)))
         (approvals (make-array (length calls) :initial-element nil))
         (start-frames #(40 80 120 170))
         (finish-frames #(64 104 144 184)))
    (setf (agent:world-agent-current-turn agent) turn
          (agent:world-agent-turns agent) (list turn))
    (agent::notify-agent-observers agent :turn-started turn)
    (agent:open-agent-hud :agent agent)
    (luvcraft:focus-luvcraft-session session gnome)
    (luvcraft:film-luvcraft-session
     session pathname
     :seconds +classic-arch-replay-seconds+
     :frame-rate +classic-arch-replay-frame-rate+
     :include-hud-p t :include-viewmodel-p nil
     :before-frame
     (lambda (frame)
       (when (= frame 18)
         (setf (agent:turn-thought turn)
               (getf +classic-arch-agent-trace+ :thought))
         (agent::notify-agent-observers agent :thought turn))
       (dotimes (index (length calls))
         (when (= frame (aref start-frames index))
           (setf (aref approvals index)
                 (start-classic-arch-replay-call
                  agent gnome (nth index calls))))
         (when (= frame (aref finish-frames index))
           (finish-classic-arch-replay-call
            agent gnome (nth index calls) (nth index trace-calls)
            (aref approvals index))))
       (when (= frame 194)
         (setf (agent:turn-status turn) :done
               (agent::turn-finished turn) (get-internal-real-time))
         (agent::notify-agent-observers agent :turn-finished turn))
       (luvcraft::set-camera-pose
        (luvcraft:luvcraft-session-camera session)
        (or (alexandria:when-let
                ((approval (agent:embodied-agent-pending-approval gnome)))
              ;; Pin proposal orbit time to the film clock rather than encoder
              ;; wall time before asking its ordinary focus owner for a pose.
              (setf (slot-value approval 'agent::created-at)
                    (- (get-internal-real-time)
                       (round (* (/ frame
                                    (coerce +classic-arch-replay-frame-rate+
                                            'double-float))
                                 internal-time-units-per-second))))
              (agent::tool-approval-focus-camera-pose
               approval gnome session))
            (classic-arch-replay-camera-pose)))
       (when (zerop (mod frame +classic-arch-replay-frame-rate+))
         (format t "capture gnome-builds-classic-arch: second ~D/~D~%"
                 (1+ (/ frame +classic-arch-replay-frame-rate+))
                 +classic-arch-replay-seconds+)
         (finish-output))))))

(luv:define-capture gnome-builds-classic-arch
    (:figure DT23EK :kind :video :extension "mp4" :section :play
     :description
     "A reviewed real gpt-5.6 turn: three proposal calls, approvals, and an arch.")
    (pathname)
  (call-with-character-instrument-session
   (lambda (session)
     (let ((old-agent agent:*agent*)
           (gnome (spawn-capture-gnome session)))
       (unwind-protect
            (let ((agent (attach-classic-arch-replay-agent session gnome)))
              (film-classic-arch-agent-replay
               session pathname gnome agent))
         (alexandria:when-let
             ((approval (agent:embodied-agent-pending-approval gnome)))
           (agent:deny-tool-approval approval "Capture teardown."))
         (alexandria:when-let ((replay-agent (agent:gnome-agent gnome)))
           (alexandria:when-let ((observer (agent::gnome-observer gnome)))
             (agent::remove-agent-observer replay-agent observer))
           ;; The ordinary embodied-agent release closes a live provider.
           ;; This object is only the reviewed cassette's typed surface.
           (setf (agent::gnome-agent gnome) nil))
         (setf agent:*agent* old-agent))))
   :title "gnome builds a classic arch"
   :width +gallery-landscape-width+ :height +gallery-landscape-height+
   :day-fraction 0.42))
