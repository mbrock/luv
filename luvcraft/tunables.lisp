;;; Tunables: the live knobs of the game, named so a gadget can turn them.
;;;
;;; The grading specials in RENDER.LISP and the terminal's emissions in
;;; TERMINAL-WALL.LISP have always been meant to be retuned from a live SLY
;;; eval.  A tunable is that knob given a name, a range, a step, and -- the
;;; part that matters -- one definite answer to "and then what": what has to
;;; happen for the change to be seen.  Most of the post chain reads its
;;; specials every frame and needs nothing; the terminal bakes its emission
;;; into glyph instances and must be told to rebuild them.  DEFINE-TUNABLE
;;; says both in one place, so a control in the metabar (MCLIM/METABAR.LISP)
;;; or a line in a SLY buffer changes the value the same way.

(in-package #:luvcraft)

(defclass tunable ()
  ((name :initarg :name :reader tunable-name)
   (label :initarg :label :reader tunable-label)
   (group :initarg :group :initform nil :reader tunable-group)
   (unit :initarg :unit :initform "" :reader tunable-unit)
   (minimum :initarg :minimum :reader tunable-minimum)
   (maximum :initarg :maximum :reader tunable-maximum)
   (step :initarg :step :reader tunable-step)
   (reader :initarg :reader :reader tunable-reader)
   (writer :initarg :writer :reader tunable-writer)
   (realizer :initarg :realizer :initform nil :reader tunable-realizer))
  (:documentation
   "One live parameter: where its value lives, its sensible range and step,
and what SESSION must do after a change for the change to show."))

(defvar *tunables* '()
  "Every defined tunable, in definition order.")

(defmacro define-tunable (name (&key label (group :grading) (unit "")
                                     minimum maximum step)
                          place &body realize)
  "Define NAME as a tunable over the setf-able PLACE.

PLACE may refer to SESSION, so a knob can live on the session -- its sky
clock -- as well as in a special.  MINIMUM, MAXIMUM, and STEP bound and
quantize what a control may set.  REALIZE, if given, is a body run with
SESSION bound after every change; it is the tunable's own account of how a
new value reaches the screen.  Leave it out for a place the renderer reads
every frame."
  (let ((value (gensym "VALUE")))
    `(register-tunable
      (make-instance
       'tunable
       :name ',name :label ,(or label (string-downcase name))
       :group ,group :unit ,unit
       :minimum ,minimum :maximum ,maximum :step ,step
       :reader (lambda (session) (declare (ignorable session)) ,place)
       :writer (lambda (,value session)
                 (declare (ignorable session))
                 (setf ,place ,value))
       :realizer ,(when realize
                    `(lambda (session)
                       (declare (ignorable session))
                       ,@realize))))))

(defun register-tunable (tunable)
  "Add or replace TUNABLE in *TUNABLES*, keeping the definition order."
  (let ((existing (position (tunable-name tunable) *tunables*
                            :key #'tunable-name)))
    (if existing
        (setf (nth existing *tunables*) tunable)
        (setf *tunables* (append *tunables* (list tunable)))))
  tunable)

(defun find-tunable (name)
  "The tunable named NAME, or NIL."
  (find name *tunables* :key #'tunable-name))

(defun tunable-value (tunable session)
  "TUNABLE's current value in SESSION."
  (funcall (tunable-reader tunable) session))

(defun set-tunable-value (tunable value session)
  "Set TUNABLE to VALUE, clamped to its range, and realize it in SESSION.

Returns the value actually set."
  (let ((clamped (max (tunable-minimum tunable)
                      (min (tunable-maximum tunable) value))))
    (funcall (tunable-writer tunable) clamped session)
    (alexandria:when-let ((realizer (tunable-realizer tunable)))
      (funcall realizer session))
    clamped))

(defun step-tunable (tunable session direction &optional (multiplier 1))
  "Move TUNABLE by DIRECTION (+1 or -1) times MULTIPLIER steps in SESSION,
landing on a multiple of the step so a run of nudges stays tidy."
  (let* ((step (tunable-step tunable))
         (current (tunable-value tunable session))
         (target (* step (round (+ (/ current step) (* direction multiplier))))))
    (set-tunable-value tunable (coerce target (type-of current)) session)))

(defun tunable-fraction (tunable session)
  "Where TUNABLE's value sits in its range, 0 to 1."
  (let ((minimum (tunable-minimum tunable))
        (maximum (tunable-maximum tunable)))
    (max 0.0 (min 1.0 (/ (- (tunable-value tunable session) minimum)
                         (max 1e-9 (- maximum minimum)))))))

(defun format-tunable-value (tunable session)
  "TUNABLE's value as the control shows it, at the precision of its step."
  (let* ((step (tunable-step tunable))
         (decimals (max 0 (ceiling (- (log step 10))))))
    (format nil "~,vF~A" decimals (tunable-value tunable session)
            (tunable-unit tunable))))

;;; ---------------------------------------------------------------------
;;; Actions: things the game can be told to do, given a name and a label
;;; so a gadget can offer them as buttons.

(defclass action ()
  ((name :initarg :name :reader action-name)
   (label :initarg :label :reader action-label)
   (function :initarg :function :reader action-function))
  (:documentation "One named verb over a session."))

(defvar *actions* '()
  "Every defined action, in definition order.")

(defmacro define-action (name (&key label) &body body)
  "Define NAME as an action: BODY runs with SESSION bound."
  `(register-action
    (make-instance 'action
                   :name ',name :label ,(or label (string-downcase name))
                   :function (lambda (session)
                               (declare (ignorable session))
                               ,@body))))

(defun register-action (action)
  (let ((existing (position (action-name action) *actions*
                            :key #'action-name)))
    (if existing
        (setf (nth existing *actions*) action)
        (setf *actions* (append *actions* (list action)))))
  action)

(defun find-action (name)
  (find name *actions* :key #'action-name))

(defun run-action (action session)
  "Do ACTION in SESSION."
  (funcall (action-function action) session))

;;; ---------------------------------------------------------------------
;;; Realizers.

(defun mark-terminal-displays-dirty (session)
  "Ask every terminal display in SESSION to rebuild its glyphs next frame."
  (dolist (overlay (luvcraft-session-overlays session))
    (when (typep overlay 'terminal-display)
      (setf (terminal-display-dirty-p overlay) t)))
  session)

;;; ---------------------------------------------------------------------
;;; The knobs.

(define-tunable terminal-ink-emission
    (:label "terminal ink" :group :terminal :unit "×"
     :minimum 0.2 :maximum 6.0 :step 0.1)
    *terminal-ink-emission*
  ;; The emission is baked into each glyph instance's colour lanes, so
  ;; every display must repopulate.
  (mark-terminal-displays-dirty session))

(define-tunable terminal-background-emission
    (:label "terminal background" :group :terminal :unit "×"
     :minimum 0.0 :maximum 3.0 :step 0.05)
    *terminal-background-emission*
  (mark-terminal-displays-dirty session))

(define-tunable bloom-threshold
    (:label "bloom threshold" :unit "" :minimum 0.2 :maximum 6.0 :step 0.1)
    *luvcraft-bloom-threshold*)

(define-tunable bloom-gain
    (:label "bloom gain" :minimum 0.0 :maximum 2.0 :step 0.05)
    *luvcraft-bloom-gain*)

(define-tunable exposure
    (:label "exposure" :minimum 0.2 :maximum 3.0 :step 0.05)
    *luvcraft-exposure*)

(define-tunable shaft-gain
    (:label "light shafts" :minimum 0.0 :maximum 2.0 :step 0.05)
    *luvcraft-shaft-gain*)

(define-tunable vignette
    (:label "vignette" :minimum 0.0 :maximum 0.6 :step 0.02)
    *luvcraft-vignette*)

(define-tunable time-of-day
    (:label "time of day" :group :sky :unit "h"
     :minimum 0.0 :maximum 24.0 :step 0.25)
    (sky-clock-hour (luvcraft-session-sky-clock session)))

(define-tunable day-length
    (:label "day length" :group :sky :unit " min"
     :minimum 0.5 :maximum 60.0 :step 0.5)
    (sky-clock-minutes-per-day (luvcraft-session-sky-clock session)))

;;; ---------------------------------------------------------------------
;;; The verbs.

(define-action focus-target (:label "focus what the crosshair is on")
  ;; Whatever gadget offered this button is itself the focus at the moment
  ;; it is pressed; it must stand down before the world can be looked at.
  (unfocus-luvcraft-session session)
  (toggle-luvcraft-session-focus session))

(define-action quit (:label "quit the game")
  ;; The event thread cannot tear itself down: STOP-PLAYING waits for a
  ;; frame boundary, so it runs beside the game, not inside it.
  (sb-thread:make-thread
   (lambda ()
     (when (eq session *session*)
       (stop-playing)))
   :name "luvcraft quit"))

;;; ---------------------------------------------------------------------
;;; The metabar hook.

(defgeneric toggle-luvcraft-metabar (session)
  (:documentation
   "Slide SESSION's metabar of tunable controls in or out, returning true
when one is available.  The presentation extension supplies the method."))

(defmethod toggle-luvcraft-metabar ((session t))
  (declare (ignore session))
  nil)
