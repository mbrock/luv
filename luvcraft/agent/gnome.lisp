(in-package #:luvcraft.agent)

;;; The gnome: an agent with a body (#5JDNKF), not a block (#V9DHV5).
;;;
;;; A gnome occupies a discrete world cell but is not a block.  The cell is
;;; semantic state -- where this agent stands -- while its animated,
;;; camera-facing figure is an independently rendered scene object.  Focusing
;;; it is how you talk to it: the view leans in the way it does for a wall, the
;;; keys become a prompt line at the bottom of the screen, and RET asks.  The
;;; gnome then thinks, and its thinking shows above its head as a bubble: a
;;; matte rounded cassette standing in the world, drawn through McCLIM the way
;;; the Telegram panel is (a world widget, in the scene, with relief).  Each
;;; tool call is the next cassette, and pushes the ones before it upward.
;;; When the gnome talks -- the SAY command, which is a tool like any other --
;;; it is not a cassette but a title at the bottom of the view, the way an NPC
;;; line is subtitled.

(defparameter *gnome-name* "gnome")

;;; The lengths below, like everything in the shader that draws him, are in
;;; figure units: the frame the gnome is laid out in, where his hat reaches
;;; GNOME-HAT-HEIGHT whatever his stature.  GNOME-STATURE is the one place
;;; the world's scale enters, so it multiplies all of them.

(defparameter *gnome-body-centre* 0.85
  "The height above his feet of the centre of the gnome's bounding sphere.

The shader's marcher agrees with this number; moving one moves both.")

(defparameter *gnome-body-margin* 0.05
  "Slack added to the computed bounding radius, for the rim light and for
the fillets that push a weld a little outside the parts it joins.")

(defparameter *gnome-face-height* 0.94
  "The height of the gnome's eyes above his feet.

What looks at a gnome is aimed here.  It is not the top of him: aiming a
conversation at the top of a gnome frames his hat.")

(defun gnome-stature ()
  "How many world cells one figure unit is, from the GNOME-STATURE knob."
  (float luvcraft.shaders::*gnome-stature* 1.0))

(defun gnome-figure-radius ()
  "The radius, in figure units, of a sphere about *GNOME-BODY-CENTRE* that
holds every part of the gnome the knobs are currently asking for.

The billboard is a conservative proxy and nothing outside it is drawn, so
this has to be recomputed rather than assumed: a hat raised past a fixed
radius does not overflow, it comes out sliced flat across the top.  Each
term is the extreme of one part -- how far out it reaches and how high it
sits -- and the sphere is the largest of them."
  (macrolet ((knob (name) `(float (symbol-value ',name) 1.0)))
    (let ((hat-height (knob luvcraft.shaders::*gnome-hat-height*))
          (hat-flare (knob luvcraft.shaders::*gnome-hat-flare*))
          (hat-lean (knob luvcraft.shaders::*gnome-hat-lean*))
          (brim (knob luvcraft.shaders::*gnome-brim-width*))
          (head (knob luvcraft.shaders::*gnome-head-size*))
          (nose (knob luvcraft.shaders::*gnome-nose-size*))
          (beard-width (knob luvcraft.shaders::*gnome-beard-width*))
          (beard-length (knob luvcraft.shaders::*gnome-beard-length*))
          (belly (knob luvcraft.shaders::*gnome-belly*))
          (shoulder (knob luvcraft.shaders::*gnome-shoulder*))
          (mitten (knob luvcraft.shaders::*gnome-mitten-reach*)))
      (flet ((reach (radial height)
               (let ((rise (- height *gnome-body-centre*)))
                 (sqrt (+ (* radial radial) (* rise rise))))))
        (+ *gnome-body-margin*
           (max (reach (abs hat-lean) hat-height)     ; the tip, wherever it leans
                (reach hat-flare 1.04)                ; the cone's skirt
                (reach brim 1.06)                     ; the brim's edge
                (reach (+ (* head 0.90) nose) 0.855)  ; the end of the nose
                (reach head 0.90)                     ; the back of the head
                (reach beard-width 0.605)             ; the whiskers
                (reach 0.115 (- beard-length 0.095))  ; the beard's point
                (reach belly 0.30)                    ; the hem at its widest
                (reach 0.0 (- 0.30 belly))            ; the hem at the ground
                (reach shoulder 0.80)                 ; the shoulders
                (reach (+ mitten 0.09) 0.50)          ; the mittens
                (reach 0.35 0.0)))))))                ; the toes

(defparameter +clear-ink+ (compose-in (make-rgb-color 0 0 0) (make-opacity 0.0))
  "A pane background that paints nothing: colour with zero opacity, which the
GPU medium spells as a fully transparent fill.")

(defclass embodied-agent ()
  ((session :initarg :session :reader gnome-session)
   (x :initarg :x :reader gnome-x)
   (y :initarg :y :reader gnome-y)
   (z :initarg :z :reader gnome-z)
   (agent :initform nil :accessor gnome-agent)
   (body :initform nil :accessor gnome-body)
   (bubbles :initform '() :accessor gnome-bubbles
            :documentation "Bubble overlays, newest first.")
   (dialogue :initform nil :accessor gnome-dialogue
             :documentation "The HUD line: the prompt being typed, or what was said.")
   (draft :initform "" :accessor gnome-draft)
   (said :initform nil :accessor gnome-said
         :documentation "The gnome's last line, or NIL.")
   (said-at :initform 0 :accessor gnome-said-at)
   (observer :initform nil :accessor gnome-observer)
   (notes :initform (sb-concurrency:make-mailbox :name "gnome notes")
          :reader gnome-notes
          :documentation "Transcript events waiting for the canvas thread.")
   (turn-finished-at :initform nil :accessor gnome-turn-finished-at))
  (:documentation
   "One embodied agent at a discrete cell: its thread, figure, and voice."))

(defclass gnome (embodied-agent) ()
  (:documentation "A small SDF-rendered garden gnome carrying a WORLD-AGENT."))

(defgeneric embodied-agent-name (agent)
  (:documentation "The short speaker name shown for AGENT."))

(defmethod embodied-agent-name ((gnome gnome))
  (declare (ignore gnome))
  *gnome-name*)

(defgeneric embodied-agent-body-height (agent)
  (:documentation "AGENT's visible height above its occupied cell, in blocks."))

(defgeneric embodied-agent-head-position (agent &optional lift)
  (:documentation "The world point above AGENT, optionally LIFT blocks higher."))

(defgeneric embodied-agent-face-position (agent &optional lift)
  (:documentation "The world point AGENT looks from, optionally LIFT blocks higher."))

(defgeneric embodied-agent-ray-distance (agent origin direction max-distance)
  (:documentation "Where a ray enters AGENT's conservative body, or NIL."))

(defgeneric embodied-agent-audience-distance (agent)
  (:documentation "How far away the focused camera stands from AGENT."))

(defgeneric ensure-embodied-agent-agent (agent)
  (:documentation "Return AGENT's WORLD-AGENT, creating it when necessary."))

(defgeneric ensure-embodied-agent-body (agent)
  (:documentation "Return AGENT's render body, creating it when necessary."))

(defgeneric embodied-agent-body-sphere (agent)
  (:documentation
   "Return AGENT's conservative SDF proxy as world-space X, Y, Z, RADIUS."))

(defmethod print-object ((gnome embodied-agent) stream)
  (print-unreadable-object (gnome stream :type t)
    (format stream "~D ~D ~D" (gnome-x gnome) (gnome-y gnome) (gnome-z gnome))))

(defvar *agents* '()
  "The embodied agents in live sessions.

These are semantic individuals, not members of the world's critter population
and not readings of its block field.  Releasing their session overlays removes
them from this registry.")

(defun agents-in-session (session)
  "Return SESSION's embodied agents, newest first."
  (remove-if-not (lambda (agent) (eq session (gnome-session agent))) *agents*))

(defun agent-at (session x y z)
  "The embodied agent occupying X,Y,Z in SESSION, or NIL."
  (find-if (lambda (agent)
             (and (eq session (gnome-session agent))
                  (= x (gnome-x agent))
                  (= y (gnome-y agent))
                  (= z (gnome-z agent))))
           *agents*))

(defun attach-embodied-agent (class session x y z)
  "Make one CLASS at X,Y,Z and attach its semantic and render overlays."
  (let ((agent (make-instance class :session session :x x :y y :z z)))
    (ensure-embodied-agent-body agent)
    (push agent *agents*)
    (luvcraft:add-luvcraft-overlay session agent)
    agent))

(defun find-agent (session x y z &optional (make-p t))
  "Find the embodied agent occupying integer cell X,Y,Z in SESSION.

When MAKE-P is true, create and attach one if the cell is unoccupied."
  (or (agent-at session x y z)
      (and make-p
           (attach-embodied-agent 'gnome session x y z))))

(defun gnome-body-height ()
  "The height of the hat's tip above his feet, in figure units.

What is above a gnome -- his speech bubbles -- is measured from here, so a
taller hat pushes them up rather than growing through them."
  (float luvcraft.shaders::*gnome-hat-height* 1.0))

(defmethod embodied-agent-body-height ((gnome gnome))
  (declare (ignore gnome))
  (* (gnome-body-height) (gnome-stature)))

(defmethod embodied-agent-head-position ((gnome gnome) &optional (lift 0.0))
  "The point above the gnome's hat, LIFT blocks higher."
  (luvcraft::make-vec3 (+ (gnome-x gnome) 0.5)
                       (+ (gnome-y gnome)
                          (embodied-agent-body-height gnome)
                          lift)
                       (+ (gnome-z gnome) 0.5)))

(defun gnome-head-position (gnome &optional (lift 0.0))
  "Compatibility name for EMBODIED-AGENT-HEAD-POSITION."
  (embodied-agent-head-position gnome lift))

(defmethod embodied-agent-face-position ((gnome gnome) &optional (lift 0.0))
  "The point between the gnome's eyes, LIFT blocks higher."
  (luvcraft::make-vec3 (+ (gnome-x gnome) 0.5)
                       (+ (gnome-y gnome)
                          (* *gnome-face-height* (gnome-stature))
                          lift)
                       (+ (gnome-z gnome) 0.5)))

(defun gnome-face-position (gnome &optional (lift 0.0))
  "Compatibility name for EMBODIED-AGENT-FACE-POSITION."
  (embodied-agent-face-position gnome lift))

;;; ---------------------------------------------------------------------
;;; The embodied agent as an overlay and a focus

(defmethod luvcraft:luvcraft-overlay-stage ((gnome embodied-agent))
  (declare (ignore gnome))
  :hud)

(defmethod luvcraft:luvcraft-focus-score ((gnome embodied-agent) session)
  "Targetable by TAB when the view ray enters the agent before terrain."
  (let ((camera (luvcraft:luvcraft-session-camera session)))
    (multiple-value-bind (right up forward) (luvcraft:camera-basis camera)
      (declare (ignore right up))
      (let ((distance
              (embodied-agent-ray-distance
               gnome (luvcraft:camera-position camera)
               forward luvcraft::+luvcraft-target-reach+)))
        (when distance
          (let ((hit (luvcraft::luvcraft-session-target
                      session :max-distance luvcraft::+luvcraft-target-reach+)))
            (unless (and hit (< (luvcraft::block-ray-hit-distance hit) distance))
              distance)))))))

(defmethod embodied-agent-ray-distance
    ((gnome gnome) origin direction max-distance)
  "Return where a ray enters GNOME's upright body, or NIL when it misses."
  (let* ((near 0d0)
         (far (coerce max-distance 'double-float))
         (stature (coerce (gnome-stature) 'double-float))
         (half-width (* 0.36d0 stature)))
    (flet ((slab (origin direction minimum maximum)
             (let ((origin (coerce origin 'double-float))
                   (direction (coerce direction 'double-float)))
               (if (zerop direction)
                   (<= minimum origin maximum)
                   (let ((entering (/ (- minimum origin) direction))
                         (leaving (/ (- maximum origin) direction)))
                     (when (> entering leaving)
                       (rotatef entering leaving))
                     (setf near (max near entering)
                           far (min far leaving))
                     (<= near far))))))
      (let ((center-x (+ (gnome-x gnome) 0.5d0))
            (center-z (+ (gnome-z gnome) 0.5d0)))
        (and (slab (luvcraft::vec3-x origin) (luvcraft::vec3-x direction)
                   (- center-x half-width) (+ center-x half-width))
             (slab (luvcraft::vec3-y origin) (luvcraft::vec3-y direction)
                   (gnome-y gnome)
                   (+ (gnome-y gnome)
                      (* (coerce (gnome-body-height) 'double-float)
                         stature)))
             (slab (luvcraft::vec3-z origin) (luvcraft::vec3-z direction)
                   (- center-z half-width) (+ center-z half-width))
             near)))))

(defun gnome-ray-distance (gnome origin direction max-distance)
  "Compatibility name for EMBODIED-AGENT-RAY-DISTANCE."
  (embodied-agent-ray-distance gnome origin direction max-distance))

(defparameter *gnome-audience-distance* 3.2
  "How far from the gnome the focused camera stands, in blocks.")

(defmethod embodied-agent-audience-distance ((gnome gnome))
  (declare (ignore gnome))
  *gnome-audience-distance*)

(defmethod luvcraft:luvcraft-focus-camera-pose ((gnome embodied-agent) session)
  "Step back to a conversational distance, look the gnome in the face, and
leave room above its hat for the bubbles."
  (let* ((camera (luvcraft:luvcraft-session-camera session))
         (eye (luvcraft:camera-position camera))
         (face (embodied-agent-face-position gnome))
         (dx (- (luvcraft::vec3-x face) (luvcraft::vec3-x eye)))
         (dz (- (luvcraft::vec3-z face) (luvcraft::vec3-z eye)))
         (flat (max (sqrt (+ (* dx dx) (* dz dz))) 0.001))
         (ux (/ dx flat)) (uz (/ dz flat))
         (distance (embodied-agent-audience-distance gnome))
         (position (luvcraft::make-vec3
                    (- (luvcraft::vec3-x face) (* ux distance))
                    (+ (luvcraft::vec3-y face) 0.10)
                    (- (luvcraft::vec3-z face) (* uz distance))))
         (target (embodied-agent-face-position gnome 0.02))
         (tdx (- (luvcraft::vec3-x target) (luvcraft::vec3-x position)))
         (tdy (- (luvcraft::vec3-y target) (luvcraft::vec3-y position)))
         (tdz (- (luvcraft::vec3-z target) (luvcraft::vec3-z position)))
         (tflat (max (sqrt (+ (* tdx tdx) (* tdz tdz))) 0.001)))
    (luvcraft::make-camera-pose
     position
     (atan tdx tdz)
     (atan tdy tflat)
     luvcraft::+luvcraft-camera-focused-vertical-field-of-view+)))

(defmethod luvcraft:luvcraft-focus-entered ((gnome embodied-agent) session)
  (ensure-gnome-dialogue gnome)
  (setf (gnome-draft gnome) "")
  (repaint-gnome-dialogue gnome)
  gnome)

(defmethod luvcraft:luvcraft-focus-left ((gnome embodied-agent) session)
  (setf (gnome-draft gnome) "")
  (when (gnome-dialogue gnome)
    (repaint-gnome-dialogue gnome))
  gnome)

(defun gnome-ask (gnome text)
  (let ((agent (or (gnome-agent gnome)
                   (ensure-embodied-agent-agent gnome))))
    (ask text :agent agent)))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((gnome embodied-agent) session canvas (event luv:canvas-key-press-event))
  (let ((key (luv:canvas-key-event-key-name event))
        (character (luv:canvas-key-event-character event))
        (paste-p (and (eq (luv:canvas-key-event-key-name event) :v)
                      (intersection '(:super :control)
                                    (luv:canvas-key-event-modifiers event)))))
    (case key
      ((:escape :tab)
       (luvcraft:unfocus-luvcraft-session session))
      ((:return :keypad-enter)
       (let ((prompt (string-trim " " (gnome-draft gnome))))
         (setf (gnome-draft gnome) "")
         (if (string= prompt "")
             (luvcraft:unfocus-luvcraft-session session)
             (handler-case (gnome-ask gnome prompt)
               (error (condition)
                 (gnome-say gnome (format nil "(~A)" condition)))))))
      (:backspace
       (let ((draft (gnome-draft gnome)))
         (when (plusp (length draft))
           (setf (gnome-draft gnome) (subseq draft 0 (1- (length draft)))))))
      (t
       (cond
         (paste-p
          (alexandria:when-let ((text (luv:canvas-clipboard-text canvas)))
            (setf (gnome-draft gnome)
                  (concatenate 'string (gnome-draft gnome)
                               (substitute #\Space #\Newline text)))))
         ((and character (graphic-char-p character)
               (< (length (gnome-draft gnome)) 400))
          (setf (gnome-draft gnome)
                (concatenate 'string (gnome-draft gnome) (string character)))))))
    (when (gnome-dialogue gnome)
      (repaint-gnome-dialogue gnome))
    t))

(defmethod luvcraft:handle-luvcraft-focus-event
    ((gnome embodied-agent) session canvas (event luv:canvas-event))
  (declare (ignore gnome session canvas event))
  t)

;;; ---------------------------------------------------------------------
;;; The agent behind the gnome

(defparameter *gnome-instructions*
  "You are a gnome: a small, cheerful, slightly gruff embodied agent in
luvcraft, a block world running inside a live Common Lisp
image.  A player is talking to you.  Your tools are the game's own commands.
Coordinates are integer block cells: x and z are horizontal, y is up; you
stand at the cell given below.  To talk to the player you MUST use the say
tool -- only what you say is heard; your final message is not shown.  Keep
what you say short and in character.  Results may mention #ABCD handles; pass
one back to describe-handle to read more.")

(defparameter *gnome-tools*
  '(com-say com-where-am-i com-block-at com-place-block-at com-describe-handle com-eval))

(defmethod ensure-embodied-agent-agent ((gnome gnome))
  (or (gnome-agent gnome)
      (let ((agent (make-world-agent
                    :session (gnome-session gnome)
                    :commands *gnome-tools*
                    :instructions (format nil "~A~%~%You stand at x=~D y=~D z=~D."
                                          *gnome-instructions*
                                          (gnome-x gnome) (gnome-y gnome) (gnome-z gnome)))))
        (setf (world-agent-presence agent) gnome
              (gnome-observer gnome) (make-gnome-observer gnome)
              (gnome-agent gnome) agent)
        (add-agent-observer agent (gnome-observer gnome))
        agent)))

(defun ensure-gnome-agent (gnome)
  "Compatibility name for ENSURE-EMBODIED-AGENT-AGENT."
  (ensure-embodied-agent-agent gnome))

(defun make-gnome-observer (gnome)
  (lambda (agent kind object)
    (declare (ignore agent))
    ;; Observers run in the turn's thread; they only queue intentions the
    ;; canvas thread realizes in REFRESH-LUVCRAFT-OVERLAY.
    (case kind
      (:turn-started
       (setf (gnome-turn-finished-at gnome) nil)
       (gnome-note gnome (list :turn object)))
      (:thought (gnome-note gnome (list :thought object)))
      (:call-started
       (unless (eq 'com-say (command-tool-command (tool-call-tool object)))
         (gnome-note gnome (list :call object))))
      (:call-finished (gnome-note gnome (list :call-finished object)))
      (:turn-finished
       (setf (gnome-turn-finished-at gnome) (get-internal-real-time))
       (gnome-note gnome (list :turn-finished object))))))

(defun gnome-note (gnome note)
  (sb-concurrency:send-message (gnome-notes gnome) note))

;;; ---------------------------------------------------------------------
;;; Saying

(defun gnome-say (gnome text)
  (setf (gnome-said gnome) text
        (gnome-said-at gnome) (get-internal-real-time))
  (ensure-gnome-dialogue gnome)
  (repaint-gnome-dialogue gnome)
  text)

(define-command (com-say :command-table luvcraft-agent :name "Say")
    ((text 'string :prompt "text"))
  "Say TEXT aloud to the player.  This is the only way the player hears you; keep it to a sentence or two."
  (let ((gnome (and *current-agent* (world-agent-presence *current-agent*))))
    (if gnome
        (gnome-say gnome text)
        (format nil "(nobody is here to hear: ~A)" text)))
  "said")

(defmethod command-ink ((command (eql 'com-say)))
  (make-rgb-color 0.95 0.85 0.55))

;;; ---------------------------------------------------------------------
;;; The dialogue line: a HUD title at the bottom of the view.

(defparameter *dialogue-width* 1100)
(defparameter *dialogue-height* 150)
(defparameter *dialogue-columns* 70)

(defclass clear-top-level-sheet-pane
    (climi::never-repaint-background-mixin climi::top-level-sheet-pane) ()
  (:documentation
   "A frame's top-level sheet that paints no background, so a floating
title or bubble shows only what its pane draws."))

(defclass gnome-dialogue-pane (climi::never-repaint-background-mixin application-pane) ()
  (:documentation "A pane that paints nothing but its text: the view shows through."))

(define-application-frame gnome-dialogue ()
  ((gnome :initarg :gnome :reader dialogue-gnome))
  (:menu-bar nil)
  ;; The pane is the whole frame: a layout pane around it would paint its
  ;; own background, and there is nothing to lay out.
  (:panes (sheet (make-pane 'gnome-dialogue-pane :background +clear-ink+
                            :width *dialogue-width* :height *dialogue-height*
                            :min-width *dialogue-width* :min-height *dialogue-height*
                            :max-width *dialogue-width* :max-height *dialogue-height*)))
  (:layouts (default sheet)))

(defun dialogue-lines (gnome)
  "What the line shows: (SPEAKER TEXT INK) or NIL."
  (let* ((session (gnome-session gnome))
         (focused-p (eq gnome (luvcraft:luvcraft-session-modal-focus session)))
         (draft (gnome-draft gnome))
         (said (gnome-said gnome))
         (said-age (/ (- (get-internal-real-time) (gnome-said-at gnome))
                      (float internal-time-units-per-second 1.0))))
    (cond ((and focused-p (or (plusp (length draft)) (null said) (> said-age 20)))
           (list "you" (concatenate 'string draft "_") *hud-text-ink*))
          ((and said (or focused-p (< said-age 20)))
           (list (embodied-agent-name gnome) said
                 (make-rgb-color 0.97 0.93 0.80)))
          (t nil))))

(defmethod handle-repaint ((pane gnome-dialogue-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (gnome (dialogue-gnome frame))
         (line (dialogue-lines gnome)))
    (with-sheet-medium (medium pane)
      (when (typep medium 'mcluv:luv-raster-medium)
        (mcluv::clear-raster-medium-reliefs medium)))
    (when line
      (destructuring-bind (speaker text ink) line
        (let* ((lines (wrap-words text *dialogue-columns*))
               (lines (if (> (length lines) 4) (last lines 4) lines))
               (line-height 30)
               (height (+ 18 (* line-height (length lines)) 14))
               (top (- *dialogue-height* height))
               (left 40) (right (- *dialogue-width* 40)))
          (mcluv:draw-analytic-rounded-rectangle* pane left top right *dialogue-height*
                                            :radius 10
                                            :ink (make-rgb-color 0.06 0.06 0.06))
          (draw-text* pane speaker (+ left 24) (+ top 22)
                      :align-y :center :text-size 15 :text-face :bold
                      :ink (if (string= speaker "you") *hud-muted-ink* (command-ink 'com-say)))
          (let ((y (+ top 18 14)))
            (dolist (text-line lines)
              (draw-text* pane text-line (+ left 24 70) (+ y 2)
                          :align-y :center :text-size 22 :ink ink)
              (incf y line-height))))))))

(defclass gnome-dialogue-overlay (mcluv:luvcraft-hud-widget-overlay)
  ((gnome :initarg :gnome :reader dialogue-overlay-gnome)
   (visible-state :initform nil :accessor dialogue-visible-state)))

(defmethod luvcraft:luvcraft-overlay-stage ((overlay gnome-dialogue-overlay))
  (declare (ignore overlay)) :hud)

(defmethod luvcraft:luvcraft-focus-score ((overlay gnome-dialogue-overlay) session)
  (declare (ignore overlay session)) nil)

(defun dialogue-screen-state (overlay)
  "Centred at the bottom of the viewport."
  (let* ((source-size (mcluv:widget-overlay-logical-size overlay))
         (viewport-size
           (luv:canvas-extent
            (luvcraft:luvcraft-session-context (mcluv:widget-overlay-session overlay))))
         (source-width (first source-size))
         (source-height (second source-size))
         (viewport-width (first viewport-size))
         (viewport-height (second viewport-size))
         (scale (min 1.0 (/ (- viewport-width 24.0) source-width)))
         (half-width (/ (* source-width scale) viewport-width))
         (half-height (/ (* source-height scale) viewport-height))
         ;; NDC spans two units over the viewport, so this is half the pixels.
         (bottom-margin (/ 300.0 viewport-height))
         (center-y (- 1.0 bottom-margin half-height)))
    (make-array 12 :element-type 'single-float
                   :initial-contents
                   (mapcar (lambda (v) (coerce v 'single-float))
                           (list 0.0 center-y 0.0 1.0
                                 half-width 0.0 0.0 0.0
                                 0.0 half-height 0.0 0.0)))))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay gnome-dialogue-overlay) session pass surface-texture)
  (declare (ignore pass))
  (mcluv:prepare-direct-widget-overlay
   overlay session surface-texture (dialogue-screen-state overlay))
  overlay)

(defun repaint-gnome-dialogue (gnome)
  (alexandria:when-let ((overlay (gnome-dialogue gnome)))
    (let* ((frame (mcluv:widget-overlay-frame overlay))
           (mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
      (mcluv:repaint-gpu-mirror mirror)
      (setf (dialogue-visible-state overlay) (dialogue-lines gnome)))))

(defmethod luvcraft:refresh-luvcraft-overlay ((overlay gnome-dialogue-overlay) session)
  (declare (ignore session))
  (unless (equal (dialogue-lines (dialogue-overlay-gnome overlay))
                 (dialogue-visible-state overlay))
    (repaint-gnome-dialogue (dialogue-overlay-gnome overlay)))
  overlay)

(defun make-embedded-frame (session frame-class &rest initargs)
  "Make an enabled McCLIM frame embedded in SESSION's canvas; return it."
  (let* ((port (find-port :server-path '(:luv-gpu)))
         (manager (or (first (climi::frame-managers port))
                      (make-instance 'mcluv:luv-frame-manager :port port))))
    (let ((mcluv:*embedded-mirror-target* (luvcraft:luvcraft-session-canvas session))
          (mcluv:*embedded-mirror-context* (luvcraft:luvcraft-session-context session))
          (mcluv:*embedded-mirror-device* (luvcraft:luvcraft-session-device session)))
      (let ((frame (apply #'make-application-frame frame-class
                          :frame-manager manager :enable t initargs)))
        ;; Nothing behind the pane either: the top-level sheet would otherwise
        ;; fill the frame's rectangle with its own white.
        (change-class (frame-top-level-sheet frame) 'clear-top-level-sheet-pane)
        frame))))

;;; ---------------------------------------------------------------------
;;; The visible body: a sphere-traced scene object, not terrain.

(defclass sdf-agent-body-overlay ()
  ((agent :initarg :agent :initarg :gnome :reader body-overlay-gnome)
   (pipeline :initarg :pipeline :accessor gnome-body-pipeline)
   (vertex-buffer :initarg :vertex-buffer :accessor gnome-body-vertex-buffer)
   (instance-buffer :initarg :instance-buffer :accessor gnome-body-instance-buffer)
   (instance-data :initarg :instance-data :reader gnome-body-instance-data))
  (:documentation
   "The shared conservative billboard and GPU resources for one SDF agent."))

(defclass gnome-body-overlay (sdf-agent-body-overlay) ()
  (:documentation "The gnome's SDF body overlay."))

(defmethod luvcraft:luvcraft-focus-score
    ((overlay sdf-agent-body-overlay) session)
  (declare (ignore overlay session))
  nil)

(defmethod luvcraft::luvcraft-overlay-live-shader-pipelines
    ((overlay sdf-agent-body-overlay))
  (list (gnome-body-pipeline overlay)))

(defmethod embodied-agent-body-sphere ((gnome gnome))
  "The gnome's current conservative proxy, including his visual bob."
  (let* ((phase (/ (get-internal-real-time)
                   (float internal-time-units-per-second 1.0)))
         (bob (* 0.025 (sin (* phase 2.2))))
         (stature (gnome-stature)))
    (values (+ (gnome-x gnome) 0.5)
            (+ (gnome-y gnome) (* *gnome-body-centre* stature) bob)
            (+ (gnome-z gnome) 0.5)
            (* (gnome-figure-radius) stature))))

(defun place-gnome-body (overlay)
  "Publish OVERLAY's agent-specific conservative center and radius."
  (let ((data (gnome-body-instance-data overlay)))
    (multiple-value-bind (x y z radius)
        (embodied-agent-body-sphere (body-overlay-gnome overlay))
      (setf (aref data 0) (coerce x 'single-float)
            (aref data 1) (coerce y 'single-float)
            (aref data 2) (coerce z 'single-float)
            (aref data 3) (coerce radius 'single-float)))
    (luv:write-buffer (gnome-body-instance-buffer overlay) data))
  overlay)

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay sdf-agent-body-overlay) session pass surface-texture)
  (place-gnome-body overlay)
  (let ((frame (luvcraft::luvcraft-frame-state session surface-texture)))
    (luv:set-pipeline
     pass
     (luvcraft::live-shader-pipeline-native-pipeline
      (gnome-body-pipeline overlay)))
    (luv:set-vertex-buffer pass 0 (gnome-body-vertex-buffer overlay))
    (luv:set-vertex-buffer pass 1 (gnome-body-instance-buffer overlay))
    (luv:set-bind-group pass 0 (luvcraft::luvcraft-frame-scene-bind-group frame))
    (luv:draw pass 6 1))
  overlay)

(defmethod luvcraft:release-luvcraft-overlay ((overlay sdf-agent-body-overlay))
  (when (gnome-body-pipeline overlay)
    (luvcraft::release-live-shader-pipeline (gnome-body-pipeline overlay))
    (setf (gnome-body-pipeline overlay) nil))
  (dolist (resource (list (gnome-body-instance-buffer overlay)
                          (gnome-body-vertex-buffer overlay)))
    (when resource (luv:destroy resource)))
  (setf (gnome-body-instance-buffer overlay) nil
        (gnome-body-vertex-buffer overlay) nil)
  (when (eq overlay (gnome-body (body-overlay-gnome overlay)))
    (setf (gnome-body (body-overlay-gnome overlay)) nil))
  (values))

(defun make-sdf-agent-body (agent role label &key (class 'sdf-agent-body-overlay))
  "Make and attach one ROLE pipeline body for AGENT."
  (let* ((session (gnome-session agent))
         (device (luvcraft:luvcraft-session-device session))
         (vertex-data (luvcraft::make-world-text-quad-vertices))
         (instance-data (make-array 4 :element-type 'single-float))
         (vertex-buffer nil)
         (instance-buffer nil)
         (pipeline nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf vertex-buffer
                     (luv:create
                      device
                      (luv:make-buffer-descriptor
                       :label (format nil "~A proxy" label)
                       :size (* 4 (length vertex-data))
                       :usage '(:vertex :copy-dst)))
                     instance-buffer
                     (luv:create
                      device
                      (luv:make-buffer-descriptor
                       :label (format nil "~A instance" label)
                       :size (* 4 (length instance-data))
                       :usage '(:vertex :copy-dst)))
                     pipeline
                     (luvcraft::make-live-shader-pipeline
                      :role role
                      :vertex-role role
                      :label (format nil "~A pipeline" label)
                      :device device
                      :layout
                      (luvcraft::live-shader-pipeline-layout
                       (luvcraft:luvcraft-session-block-pipeline session))
                      :vertex-buffers
                      '((:array-stride 12
                         :attributes
                         ((:shader-location 0 :offset 0 :format :float32x3)))
                        (:array-stride 16 :step-mode :instance
                         :attributes
                         ((:shader-location 1 :offset 0 :format :float32x4))))
                      :target-format luvcraft::+luvcraft-scene-color-format+
                      :target-blend :premultiplied-alpha
                      :primitive '(:topology :triangle-list)
                      :depth-stencil
                      '(:format :depth32-float
                        :depth-write-enabled nil
                        :depth-compare :less)))
           (luv:write-buffer vertex-buffer vertex-data)
           (let ((overlay (make-instance class
                                         :agent agent :pipeline pipeline
                                         :vertex-buffer vertex-buffer
                                         :instance-buffer instance-buffer
                                         :instance-data instance-data)))
             (place-gnome-body overlay)
             (setf (gnome-body agent) overlay
                   completed-p t)
             (luvcraft:add-luvcraft-overlay session overlay)
             overlay))
      (unless completed-p
        (when pipeline
          (ignore-errors
            (luvcraft::release-live-shader-pipeline pipeline)))
        (when instance-buffer (ignore-errors (luv:destroy instance-buffer)))
        (when vertex-buffer (ignore-errors (luv:destroy vertex-buffer)))))))

(defun ensure-gnome-body (gnome)
  (or (gnome-body gnome)
      (make-sdf-agent-body gnome :gnome-sdf "gnome SDF"
                          :class 'gnome-body-overlay)))

(defmethod ensure-embodied-agent-body ((gnome gnome))
  (ensure-gnome-body gnome))

(defun ensure-gnome-dialogue (gnome)
  (or (gnome-dialogue gnome)
      (let* ((session (gnome-session gnome))
             (frame (make-embedded-frame session 'gnome-dialogue :gnome gnome))
             (mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
             (overlay (make-instance 'gnome-dialogue-overlay
                                     :session session :frame frame :mirror mirror
                                     :gnome gnome)))
        (setf (frame-pretty-name frame) "gnome dialogue"
              (mcluv:mirror-compositor mirror) overlay
              (gnome-dialogue gnome) overlay)
        (luvcraft:add-luvcraft-overlay session overlay)
        overlay)))

;;; ---------------------------------------------------------------------
;;; Bubbles: cassettes in the world above the gnome's head.

(defparameter *bubble-width* 520)
(defparameter *bubble-height* 150)
(defparameter *bubble-world-width* 1.7
  "How wide a bubble is in blocks.")
(defparameter *bubble-lift* 0.55
  "How far above the hat the newest bubble's centre floats, in blocks.")
(defparameter *bubble-spacing* 0.52
  "The vertical distance between stacked bubbles, in blocks.")
(defparameter *bubble-count* 4
  "How many bubbles stay up before the oldest is dropped.")
(defparameter *bubble-linger-seconds* 14
  "How long after a turn ends its bubbles stay.")

(defclass gnome-bubble-pane (climi::never-repaint-background-mixin application-pane) ())

(define-application-frame gnome-bubble ()
  ((gnome :initarg :gnome :reader bubble-gnome)
   (item :initarg :item :accessor bubble-item
         :documentation "A TURN (its thought) or a TOOL-CALL."))
  (:menu-bar nil)
  (:panes (sheet (make-pane 'gnome-bubble-pane :background +clear-ink+
                            :width *bubble-width* :height *bubble-height*
                            :min-width *bubble-width* :min-height *bubble-height*
                            :max-width *bubble-width* :max-height *bubble-height*)))
  (:layouts (default sheet)))

(defun bubble-state (frame)
  "What the bubble shows, as a key: repaint when it changes."
  (let ((item (bubble-item frame)))
    (etypecase item
      (turn (list :thought (length (turn-thought item))))
      (tool-call (list (tool-call-status item)
                       (round (tool-call-elapsed-seconds item) 0.5)
                       (length (tool-call-output item)))))))

(defparameter *bubble-margin* 20)
(defparameter *bubble-columns* 48)

(defun bubble-thought-lines (turn)
  (let ((lines (wrap-words (remove #\* (turn-thought turn)) *bubble-columns*)))
    (or (if (> (length lines) 4) (last lines 4) lines) (list "..."))))

(defun bubble-content-height (item)
  "How tall the slab must be for ITEM."
  (let ((*cassette-output-lines* 3))
    (etypecase item
      (turn (+ 8 (* *cassette-line-height* (length (bubble-thought-lines item)))))
      (tool-call (tool-call-cassette-height item (- *bubble-columns* 2))))))

(defmethod handle-repaint ((pane gnome-bubble-pane) region)
  (declare (ignore region))
  (let* ((frame (pane-frame pane))
         (item (bubble-item frame))
         (*cassette-output-lines* 3)
         (*agent-hud-margin* *bubble-margin*)
         (*agent-hud-columns* *bubble-columns*)
         (height (+ (* 2 *bubble-margin*) (bubble-content-height item)))
         (top (max 2 (- *bubble-height* height))))
    (with-sheet-medium (medium pane)
      (when (typep medium 'mcluv:luv-raster-medium)
        (mcluv::clear-raster-medium-reliefs medium)))
    ;; The cassette: a matte rounded slab standing off the world, as tall as
    ;; its contents and sitting at the bottom of the pane, nearest the head.
    (mcluv:draw-analytic-rounded-rectangle*
     pane 2 top (- *bubble-width* 2) (- *bubble-height* 2) :radius 18
     :ink (mcluv:make-relief-design *hud-panel-ink* 2.0))
    (setf (stream-cursor-position pane) (values *bubble-margin* (+ top *bubble-margin*)))
    (etypecase item
      (turn
       (draw-cassette-lines pane (bubble-thought-lines item) (+ top *bubble-margin* 2)
                            :ink *hud-thought-ink* :face :italic))
      (tool-call
       (funcall-presentation-generic-function
        present item 'tool-call pane +cassette-view+)))))

(defclass gnome-bubble-overlay (mcluv:luvcraft-world-widget-overlay)
  ((gnome :initarg :gnome :reader bubble-overlay-gnome)
   (lift :initform 0.0 :accessor bubble-lift
         :documentation "Where the bubble floats now, in blocks above the hat.")
   (target-lift :initform 0.0 :accessor bubble-target-lift)
   (visible-state :initform nil :accessor bubble-visible-state)))

(defmethod luvcraft:luvcraft-focus-score ((overlay gnome-bubble-overlay) session)
  (declare (ignore overlay session)) nil)

(defun place-bubble (overlay session)
  "Face the camera from above the gnome, at the bubble's current lift."
  (let* ((gnome (bubble-overlay-gnome overlay))
         (camera (luvcraft:luvcraft-session-camera session))
         (center (embodied-agent-head-position gnome (bubble-lift overlay)))
         (aspect (/ *bubble-width* *bubble-height*))
         (half-width (/ *bubble-world-width* 2.0))
         (half-height (/ half-width aspect)))
    (multiple-value-bind (right up forward) (luvcraft:camera-basis camera)
      (declare (ignore up))
      (let ((flat-right (luvcraft::make-vec3 (luvcraft::vec3-x right) 0.0
                                             (luvcraft::vec3-z right))))
        (setf (mcluv::widget-overlay-center overlay) center
              (mcluv::widget-overlay-right-axis overlay)
              (luvcraft::vec3-scale flat-right half-width)
              (mcluv::widget-overlay-up-axis overlay)
              (luvcraft::make-vec3 0.0 (- half-height) 0.0)
              (mcluv::widget-overlay-normal-axis overlay)
              (luvcraft::vec3-scale forward -1.0))))))

(defmethod luvcraft:encode-luvcraft-overlay
    ((overlay gnome-bubble-overlay) session pass surface-texture)
  "Draw the bubble flat, in the scene, facing the player."
  (declare (ignore pass))
  (place-bubble overlay session)
  (let ((viewport-size
          (luv:canvas-extent (luvcraft:luvcraft-session-context session))))
    (mcluv:prepare-direct-widget-overlay
     overlay session surface-texture
     (mcluv::world-device-clip-state
      overlay session (first viewport-size) (second viewport-size))))
  overlay)

(defun repaint-bubble (overlay)
  (let* ((frame (mcluv:widget-overlay-frame overlay))
         (mirror (sheet-direct-mirror (frame-top-level-sheet frame))))
    (mcluv:repaint-gpu-mirror mirror)
    (setf (bubble-visible-state overlay) (bubble-state frame))))

(defmethod luvcraft:refresh-luvcraft-overlay ((overlay gnome-bubble-overlay) session)
  (declare (ignore session))
  ;; Ease toward the stack position the newer bubbles have pushed it to.
  (let ((lift (bubble-lift overlay)) (target (bubble-target-lift overlay)))
    (setf (bubble-lift overlay) (+ lift (* 0.18 (- target lift)))))
  (let ((frame (mcluv:widget-overlay-frame overlay)))
    (unless (equal (bubble-state frame) (bubble-visible-state overlay))
      (repaint-bubble overlay)))
  overlay)

(defun add-gnome-bubble (gnome item)
  "Put a new bubble for ITEM above GNOME's head, pushing the others up."
  (let* ((session (gnome-session gnome))
         (frame (make-embedded-frame session 'gnome-bubble :gnome gnome :item item))
         (mirror (sheet-direct-mirror (frame-top-level-sheet frame)))
         (overlay (make-instance 'gnome-bubble-overlay
                                 :session session :frame frame :mirror mirror
                                 :gnome gnome :height-scale 0.35)))
    (setf (frame-pretty-name frame) "gnome bubble"
          (mcluv:mirror-compositor mirror) overlay
          (bubble-lift overlay) (- *bubble-lift* 0.3)
          (bubble-target-lift overlay) *bubble-lift*)
    (push overlay (gnome-bubbles gnome))
    (restack-gnome-bubbles gnome)
    (luvcraft:add-luvcraft-overlay session overlay)
    (repaint-bubble overlay)
    overlay))

(defun restack-gnome-bubbles (gnome)
  "Newest lowest; the ones past the count are taken down."
  (loop for overlay in (gnome-bubbles gnome)
        for index from 0
        do (setf (bubble-target-lift overlay)
                 (+ *bubble-lift* (* index *bubble-spacing*))))
  (loop while (> (length (gnome-bubbles gnome)) *bubble-count*)
        do (let ((oldest (car (last (gnome-bubbles gnome)))))
             (setf (gnome-bubbles gnome) (butlast (gnome-bubbles gnome)))
             (luvcraft:remove-luvcraft-overlay (gnome-session gnome) oldest))))

(defun clear-gnome-bubbles (gnome)
  (dolist (overlay (gnome-bubbles gnome))
    (luvcraft:remove-luvcraft-overlay (gnome-session gnome) overlay))
  (setf (gnome-bubbles gnome) '()))

(defun bubble-for-item (gnome item)
  (find item (gnome-bubbles gnome)
        :key (lambda (overlay) (bubble-item (mcluv:widget-overlay-frame overlay)))))

;;; ---------------------------------------------------------------------
;;; Realizing the transcript, on the canvas thread, once a frame.

(defun realize-gnome-note (gnome note)
  (destructuring-bind (kind object) note
    (ecase kind
      (:turn (clear-gnome-bubbles gnome))
      (:thought
       (unless (bubble-for-item gnome object)
         (add-gnome-bubble gnome object)))
      (:call (add-gnome-bubble gnome object))
      (:call-finished nil)
      (:turn-finished
       (unless (string= (turn-text object) "")
         ;; A gnome that forgot to SAY still gets heard.
         (unless (and (gnome-said gnome)
                      (> (gnome-said-at gnome) (turn-started object)))
           (gnome-say gnome (turn-text object))))))))

(defmethod luvcraft:refresh-luvcraft-overlay ((gnome embodied-agent) session)
  (declare (ignore session))
  ;; The gnome itself draws nothing; its refresh is where the turn thread's
  ;; notes become bubbles and lines, and where old bubbles are taken down.
  (loop for (note received-p) = (multiple-value-list
                                  (sb-concurrency:receive-message-no-hang (gnome-notes gnome)))
        while received-p
        do (realize-gnome-note gnome note))
  (alexandria:when-let ((finished (gnome-turn-finished-at gnome)))
    (when (and (gnome-bubbles gnome)
               (> (/ (- (get-internal-real-time) finished)
                     (float internal-time-units-per-second 1.0))
                  *bubble-linger-seconds*))
      (clear-gnome-bubbles gnome)))
  gnome)

(defmethod luvcraft:release-luvcraft-overlay :after ((gnome embodied-agent))
  (setf *agents* (delete gnome *agents* :test #'eq))
  (when (and (gnome-agent gnome) (gnome-observer gnome))
    (remove-agent-observer (gnome-agent gnome) (gnome-observer gnome)))
  (when (gnome-agent gnome)
    (openai:close-agent (gnome-agent gnome))))

;;; ---------------------------------------------------------------------
;;; Spawning one

(defun spawn-embodied-agent (class &key (session luvcraft:*session*) x y z)
  "Spawn an embodied agent of CLASS at integer cell X,Y,Z and return it.

With no coordinate, choose an empty cell a few steps ahead of the player and
stand the agent on the first supporting block.  The cell is authoritative;
the figure's bobbing and any future step interpolation are only presentation."
  (let* ((player (luvcraft:luvcraft-session-player session))
         (camera (luvcraft:luvcraft-session-camera session))
         (world (luvcraft:luvcraft-session-world session)))
    (unless x
      (multiple-value-bind (right up forward) (luvcraft:camera-basis camera)
        (declare (ignore right up))
        (setf x (floor (+ (luvcraft:player-x player) (* 3.5 (luvcraft::vec3-x forward))))
              z (floor (+ (luvcraft:player-z player) (* 3.5 (luvcraft::vec3-z forward))))
              y (floor (luvcraft:player-y player)))
        ;; Stand on the ground, not in it.
        (loop while (luvcraft:world-block-at world x y z) do (incf y))
        (loop while (and (> y 0) (null (luvcraft:world-block-at world x (1- y) z)))
              do (decf y))))
    (unless (and (integerp x) (integerp y) (integerp z))
      (error "An agent needs an integer cell, got (~S ~S ~S)." x y z))
    (when (luvcraft:world-block-at world x y z)
      (error "Cell (~D ~D ~D) is occupied by terrain." x y z))
    (let ((existing (agent-at session x y z)))
      (cond ((null existing)
             (attach-embodied-agent class session x y z))
            ((typep existing class) existing)
            (t (error "Cell (~D ~D ~D) is occupied by ~A."
                      x y z (embodied-agent-name existing)))))))

(defun spawn-agent (&key (session luvcraft:*session*) x y z)
  "Spawn a visible gnome agent, choosing a supported cell when omitted."
  (spawn-embodied-agent 'gnome :session session :x x :y y :z z))

(define-command (com-spawn-agent :command-table luvcraft.clim::luvcraft-world
                                  :name "Spawn Gnome")
    ()
  "Spawn a visible gnome agent a few discrete cells ahead of the player."
  (spawn-agent :session (luvcraft.clim::luvcraft-command-session)))
