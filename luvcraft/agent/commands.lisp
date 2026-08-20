(in-package #:luvcraft.agent)

;;; The agent's first verbs, as CLIM commands in a table of their own.
;;;
;;; Each one returns an object, and what the model reads is that object under
;;; the model view; what the HUD draws is the same object under the cassette
;;; view.  The docstring is the tool description the provider sees, so it is
;;; written to the model.

(define-command-table luvcraft-agent)

(defgeneric embodied-agent-name (agent)
  (:documentation "The short speaker name shown for an embodied AGENT."))

;;; ---------------------------------------------------------------------
;;; Where am I

(defclass body-place ()
  ((x :initarg :x :reader place-x)
   (y :initarg :y :reader place-y)
   (z :initarg :z :reader place-z)
   (subject :initarg :subject :reader place-subject)
   (yaw :initarg :yaw :initform nil :reader place-yaw)
   (pitch :initarg :pitch :initform nil :reader place-pitch)
   (looking-p :initarg :looking-p :initform nil :reader place-looking-p)
   (target :initarg :target :initform nil :reader place-target
           :documentation "A BLOCK-REPORT for the block under the crosshair, or NIL."))
  (:documentation "Where one command subject stands and, when applicable, looks."))

(defclass player-place (body-place) ()
  (:documentation "Compatibility specialization for an explicitly player place."))

(defun command-subject (session)
  "The embodied object whose command is running: agent presence, then player."
  (or (and *current-agent* (world-agent-presence *current-agent*))
      (luvcraft:luvcraft-session-player session)))

(defun command-subject-name (subject session)
  (if (eq subject (luvcraft:luvcraft-session-player session))
      "you"
      (embodied-agent-name subject)))

(defun command-subject-bearing (subject session)
  "Return SUBJECT's YAW and PITCH, or NIL values when it has no facing yet."
  (if (eq subject (luvcraft:luvcraft-session-player session))
      (let ((camera (luvcraft:luvcraft-session-camera session)))
        (values (luvcraft:camera-yaw camera) (luvcraft:camera-pitch camera)))
      (let* ((velocity (luvcraft:body-velocity subject))
             (x (luvcraft::vec3-x velocity))
             (z (luvcraft::vec3-z velocity)))
        (if (> (+ (* x x) (* z z)) 1d-4)
            (values (atan x z) 0d0)
            (values nil nil)))))

(defun compass-word (yaw)
  "A rough compass word for YAW in radians, with +z as north."
  (let* ((turns (/ (mod yaw (* 2 pi)) (* 2 pi)))
         (eighths (mod (round (* turns 8)) 8)))
    (nth eighths '("north" "north-east" "east" "south-east"
                   "south" "south-west" "west" "north-west"))))

(define-presentation-method present
    (object (type body-place) stream (view textual-view) &key)
  (format stream "~:(~A~) stand~:[s~;~] at x=~D y=~D z=~D"
          (place-subject object) (string= (place-subject object) "you")
          (place-x object) (place-y object) (place-z object))
  (when (place-yaw object)
    (format stream " facing ~A" (compass-word (place-yaw object))))
  (when (place-looking-p object)
    (if (place-target object)
        (progn (write-string ", looking at " stream)
               (present (place-target object) 'block-report :stream stream :view view))
        (write-string ", looking at nothing in reach." stream))))

(define-command (com-where-am-i :command-table luvcraft-agent
                                :name "Where Am I")
    ()
  "Report your body's block position and, when you have one, facing and target."
  (let* ((session (luvcraft.clim:luvcraft-command-session))
         (subject (command-subject session))
         (player-p (eq subject (luvcraft:luvcraft-session-player session)))
         (hit (and player-p (luvcraft::luvcraft-session-target session))))
    (multiple-value-bind (x y z) (luvcraft:body-cell subject)
      (multiple-value-bind (yaw pitch) (command-subject-bearing subject session)
        (make-instance 'body-place
                       :subject (command-subject-name subject session)
                       :x x :y y :z z :yaw yaw :pitch pitch
                       :looking-p player-p
                       :target
                       (and hit
                            (let ((c (luvcraft::block-ray-hit-coordinate hit)))
                              (block-report-at
                               session
                               (luvcraft::world-coordinate-x c)
                               (luvcraft::world-coordinate-y c)
                               (luvcraft::world-coordinate-z c)))))))))

;;; ---------------------------------------------------------------------
;;; Moving to a place

(define-command (com-move-to :command-table luvcraft-agent :name "Move To")
    ((x 'integer :prompt "x" :documentation "nearby destination x cell")
     (y 'integer :prompt "y" :documentation "supported destination y cell")
     (z 'integer :prompt "z" :documentation "nearby destination z cell"))
  "Move your body to the nearby supported cell x y z.  The call waits until you arrive or movement fails; basic wayfinding can step one block up or down and route around solid blocks."
  (let* ((session (luvcraft.clim:luvcraft-command-session))
         (subject (command-subject session)))
    (unless subject
      (error "No embodied command subject is present."))
    (luvcraft:start-body-move-to
     subject (luvcraft:luvcraft-session-world session) x y z)))

(defmethod settle-command-result
    ((command (eql 'com-move-to)) (action luvcraft:body-move-action))
  (declare (ignore command))
  (luvcraft:await-body-move-action action))

(defmethod command-result-presentation-type
    ((command (eql 'com-move-to)) value)
  (declare (ignore value))
  'luvcraft:body-move-action)

(define-presentation-method present
    (action (type luvcraft:body-move-action) stream (view textual-view) &key)
  (destructuring-bind (x y z) (luvcraft:body-move-action-destination action)
    (format stream "Move To x=~D y=~D z=~D: ~(~A~) after ~,2Fs"
            x y z (luvcraft:body-move-action-status action)
            (luvcraft:body-move-action-elapsed action)))
  (when (luvcraft:body-move-action-detail action)
    (format stream " (~A)" (luvcraft:body-move-action-detail action))))

;;; ---------------------------------------------------------------------
;;; Blocks

(defclass block-report ()
  ((x :initarg :x :reader block-report-x)
   (y :initarg :y :reader block-report-y)
   (z :initarg :z :reader block-report-z)
   (kind :initarg :kind :reader block-report-kind
         :documentation "A BLOCK-KIND, or NIL for air.")
   (residency :initarg :residency :reader block-report-residency))
  (:documentation "One cell of the world as it was when asked."))

(defun block-report-at (session x y z)
  (multiple-value-bind (kind residency)
      (luvcraft:world-block-at (luvcraft:luvcraft-session-world session) x y z)
    (make-instance 'block-report :x x :y y :z z :kind kind :residency residency)))

(define-presentation-method present
    (object (type block-report) stream (view textual-view) &key)
  (format stream "~A at x=~D y=~D z=~D~@[ (~(~A~))~]"
          (if (block-report-kind object)
              (present-to-string (block-report-kind object) 'luvcraft:block-kind :view view)
              "air")
          (block-report-x object) (block-report-y object) (block-report-z object)
          (and (not (eq :resident (block-report-residency object)))
               (block-report-residency object))))

(define-command (com-block-at :command-table luvcraft-agent
                              :name "Block At")
    ((x 'integer :prompt "x") (y 'integer :prompt "y") (z 'integer :prompt "z"))
  "Report which block kind is at integer cell x y z (air when empty)."
  (block-report-at (luvcraft.clim:luvcraft-command-session) x y z))

(define-command (com-place-block-at :command-table luvcraft-agent
                                    :name "Place Block At")
    ((kind 'luvcraft:block-kind :prompt "kind")
     (x 'integer :prompt "x") (y 'integer :prompt "y") (z 'integer :prompt "z"))
  "Place a block of the given kind at integer cell x y z, replacing what is there."
  (let* ((session (luvcraft.clim:luvcraft-command-session))
         (world (luvcraft:luvcraft-session-world session)))
    (multiple-value-bind (old residency) (luvcraft:world-block-at world x y z)
      (declare (ignore old))
      (unless (eq residency :resident)
        (error "Cell ~D ~D ~D is not loaded (~(~A~)); stay nearer the player."
               x y z residency))
      (when (luvcraft::luvcraft-session-physics session)
        (luvcraft::wake-physics-bodies-near
         (luvcraft::luvcraft-session-physics session)
         (+ x 0.5) (+ y 0.5) (+ z 0.5) 2.5))
      (luvcraft:edit-block-at kind world x y z)
      (block-report-at session x y z))))

;;; ---------------------------------------------------------------------
;;; Handles and the evaluator

(define-command (com-describe-handle :command-table luvcraft-agent
                                     :name "Describe Handle")
    ((thing 'handle :prompt "handle"))
  "Read the whole of something an earlier result named by #ABCD handle."
  (if (stringp thing)
      thing
      (with-output-to-string (stream)
        (describe thing stream))))

(defmethod command-output-line-limit ((command (eql 'com-describe-handle)))
  400)

(defmethod command-tool-runs-on-canvas-p ((command (eql 'com-eval)))
  nil)

(defmethod command-result-presentation-type ((command (eql 'com-eval)) value)
  (declare (ignore value))
  'expression)

(define-command (com-eval :command-table luvcraft-agent
                          :name "Eval")
    ((form 'string :prompt "form"))
  "Evaluate one Common Lisp form in the live image (package LUVCRAFT) and return its values.  luvcraft:*session* is the game."
  (let ((*package* (find-package '#:luvcraft)))
    (eval (read-from-string form))))
