;;; Knobs: the live values of the game, as objects a gadget can turn.
;;;
;;; The grading specials in RENDER.LISP, the sky's clock, the player's stride,
;;; the terminal's emissions, a literal in a shader: each has always been
;;; meant to be retuned from a live SLY eval.  A knob is that value given a
;;; name, a declared quantity (so its unit is the one the arithmetic checks,
;;; and the one the metabar prints), a range and a step, a place to live --
;;; a special, or a slot reached through SESSION -- and, the part that
;;; matters, one definite answer to "and then what": what has to happen for
;;; the change to be seen.
;;;
;;; That answer is the knob's class.  A plain KNOB is read every frame and
;;; needs nothing.  A knob mixing in a REALIZATION -- the terminal's, which
;;; bakes its values into glyph instances and must rebuild them; the
;;; streaming window's, which must be re-centred -- says so by its class, and
;;; REALIZE-KNOB dispatches on it.  A knob folded into shader source needs no
;;; class of its own: the shader parser folds any knob's name into a literal
;;; through SHADER-SOURCE-VALUE and remembers that it did, and the live
;;; pipeline rebuilds when the value it folded no longer holds.
;;;
;;; The protocol lives here, early; the knobs themselves are defined beside
;;; what they tune -- DEFINE-KNOB in RENDER.LISP for grading, SKY.LISP for
;;; the clock, SHADERS.LISP for the folded literals, and so on -- so a knob
;;; is found where its value is used.  Reading a knob in a hot loop is a
;;; slot lookup and a funcall; take the reading once, at frame scope, and
;;; keep the dense loop dense.

(in-package #:luvcraft)

;;; ---------------------------------------------------------------------
;;; The knob.

(defclass knob ()
  ((name :initarg :name :reader knob-name)
   (label :initarg :label :reader knob-label)
   (group :initarg :group :initform :grading :reader knob-group)
   (documentation :initarg :documentation :initform nil
                  :reader knob-documentation)
   (declaration :initarg :declaration :initform nil :reader knob-declaration
                :documentation
                "The represented-value declaration: representation type and
quantity, and so the unit.")
   (unit-label :initarg :unit-label :initform nil
               :documentation
               "An explicit suffix for the value, overriding the unit's own.")
   (reader :initarg :reader :reader knob-reader)
   (writer :initarg :writer :reader knob-writer))
  (:documentation
   "One live value: where it lives, what it measures, and -- by its class --
what SESSION must do after a change for the change to show."))

(defclass scalar-knob (knob)
  ((minimum :initarg :minimum :reader knob-minimum)
   (maximum :initarg :maximum :reader knob-maximum)
   (step :initarg :step :reader knob-step))
  (:documentation "A knob over a real, bounded and quantized."))

(defclass switch-knob (knob)
  ()
  (:documentation "A knob over a generalized boolean: on or off."))

(defvar *knobs* '()
  "Every defined knob, in definition order.")

(defun register-knob (knob)
  "Add or replace KNOB in *KNOBS*, keeping the definition order."
  (let ((existing (position (knob-name knob) *knobs* :key #'knob-name)))
    (if existing
        (setf (nth existing *knobs*) knob)
        (setf *knobs* (append *knobs* (list knob)))))
  knob)

(defun find-knob (name)
  "The knob named NAME, or NIL."
  (find name *knobs* :key #'knob-name))

(defparameter *knob-group-order*
  '(:grading :sun :sky :shadows :camera :player :streaming :critters
    :riding :terminal)
  "The order the metabar shows groups in; a group not named here follows.")

(defun knob-groups ()
  "The groups the knobs fall in: those in *KNOB-GROUP-ORDER* first, in that
order, then the rest in order of first appearance."
  (let ((present (remove-duplicates (mapcar #'knob-group *knobs*)
                                    :from-end t)))
    (append (remove-if-not (lambda (group) (member group present))
                           *knob-group-order*)
            (remove-if (lambda (group) (member group *knob-group-order*))
                       present))))

(defun knobs-in-group (group)
  (remove group *knobs* :key #'knob-group :test-not #'eq))

;;; ---------------------------------------------------------------------
;;; Realization: what has to happen after a change.

(defgeneric realize-knob (knob session)
  (:documentation
   "Do what SESSION needs after KNOB's value changed for the change to show.
The primary method on KNOB does nothing: a value the renderer reads every
frame is realized by the next frame.  A realization mixin adds a method.")
  (:method-combination progn))

(defmethod realize-knob progn ((knob knob) session)
  (declare (ignore knob session))
  nil)

;;; The realizations themselves are defined beside what they realize:
;;; TERMINAL-REALIZATION in TERMINAL-WALL.LISP, RESIDENCY-REALIZATION in
;;; STREAMING.LISP.

;;; ---------------------------------------------------------------------
;;; Values.

(defgeneric knob-value (knob session)
  (:documentation "KNOB's current value in SESSION.")
  (:method ((knob knob) session)
    (funcall (knob-reader knob) session)))

(defgeneric coerce-knob-value (knob value session)
  (:documentation
   "VALUE made fit for KNOB's place: clamped and of the place's type.")
  (:method ((knob scalar-knob) value session)
    (let ((clamped (max (knob-minimum knob)
                        (min (knob-maximum knob) value)))
          (current (knob-value knob session)))
      (typecase current
        (integer (round clamped))
        (float (coerce clamped (type-of current)))
        (t clamped))))
  (:method ((knob switch-knob) value session)
    (declare (ignore session))
    (and value t)))

(defun set-knob-value (knob value session)
  "Set KNOB to VALUE, made fit, and realize it in SESSION.

Returns the value actually set."
  (let ((fit (coerce-knob-value knob value session)))
    (funcall (knob-writer knob) fit session)
    (realize-knob knob session)
    fit))

(defgeneric step-knob (knob session direction &optional multiplier)
  (:documentation
   "Move KNOB by DIRECTION (+1 or -1) times MULTIPLIER steps in SESSION.")
  (:method ((knob scalar-knob) session direction &optional (multiplier 1))
    ;; Land on a multiple of the step so a run of nudges stays tidy.
    (let* ((step (knob-step knob))
           (current (knob-value knob session))
           (target (* step (round (+ (/ current step)
                                     (* direction multiplier))))))
      (set-knob-value knob target session)))
  (:method ((knob switch-knob) session direction &optional multiplier)
    (declare (ignore multiplier))
    ;; Right turns it on, left turns it off, so a nudge is idempotent.
    (set-knob-value knob (plusp direction) session)))

(defun toggle-knob (knob session)
  "Flip a switch knob."
  (set-knob-value knob (not (knob-value knob session)) session))

(defgeneric knob-fraction (knob session)
  (:documentation "Where KNOB's value sits in its range, 0 to 1.")
  (:method ((knob scalar-knob) session)
    (let ((minimum (knob-minimum knob))
          (maximum (knob-maximum knob)))
      (max 0.0 (min 1.0 (/ (- (knob-value knob session) minimum)
                           (max 1e-9 (- maximum minimum)))))))
  (:method ((knob switch-knob) session)
    (if (knob-value knob session) 1.0 0.0)))

;;; ---------------------------------------------------------------------
;;; Units, as the metabar prints them.

(defun knob-quantity-specification (knob)
  (alexandria:when-let ((declaration (knob-declaration knob)))
    (luv.arithmetic:declaration-quantity-specification declaration)))

(defun knob-unit (knob)
  "The unit KNOB's value is measured in, as a designator: a unit name for
one named unit to the first power, else a list of (NAME EXPONENT), else NIL
for the identity."
  (alexandria:when-let* ((specification (knob-quantity-specification knob))
                         (unit (luv.arithmetic:quantity-specification-unit
                                specification)))
    (let ((factors (mapcar (lambda (factor)
                             (list (car factor) (cdr factor)))
                           (luv.arithmetic:unit-expression-factors unit))))
      (cond ((null factors) nil)
            ((and (null (rest factors)) (eql 1 (second (first factors))))
             (first (first factors)))
            (t factors)))))

(defgeneric unit-abbreviation (unit)
  (:documentation
   "The short spelling of the named UNIT, as it appears after a value or
inside a compound unit.  The default spells the unit's name; the common
ones abbreviate.")
  (:method ((unit symbol)) (string-downcase unit))
  (:method ((unit (eql :one))) "")
  (:method ((unit (eql :second))) "s")
  (:method ((unit (eql :minute))) "min")
  (:method ((unit (eql :hour))) "h")
  (:method ((unit (eql :radian))) "rad")
  (:method ((unit (eql :milliradian))) "mrad")
  (:method ((unit (eql :degree))) "°")
  (:method ((unit (eql :cell))) "cell")
  (:method ((unit (eql :percent))) "%"))

(defun unit-label (unit)
  "The suffix a value in UNIT is printed with, space included where one is
wanted.  UNIT is a unit designator: a name, or a list of (NAME EXPONENT)."
  (flet ((power (name exponent)
           (format nil "~A~A" (unit-abbreviation name)
                   (case (abs exponent)
                     (1 "") (2 "²") (3 "³")
                     (t (format nil "^~D" (abs exponent)))))))
    (cond ((null unit) "")
          ((eq unit :one) "")
          ((eq unit :degree) "°")
          ((symbolp unit) (format nil " ~A" (unit-abbreviation unit)))
          ((consp unit)
           (let ((above (remove-if-not #'plusp unit :key #'second))
                 (below (remove-if-not #'minusp unit :key #'second)))
             (format nil " ~{~A~^·~}~@[/~{~A~^·~}~]"
                     (mapcar (lambda (factor) (apply #'power factor)) above)
                     (and below
                          (mapcar (lambda (factor) (apply #'power factor))
                                  below)))))
          (t (format nil " ~(~A~)" unit)))))

(defun knob-unit-label (knob)
  "What follows KNOB's value when printed."
  (or (slot-value knob 'unit-label)
      (unit-label (knob-unit knob))))

(defgeneric format-knob-value (knob session)
  (:documentation "KNOB's value as the control shows it.")
  (:method ((knob scalar-knob) session)
    ;; At the precision of the step.
    (let* ((step (knob-step knob))
           (value (knob-value knob session))
           (label (knob-unit-label knob)))
      (if (or (integerp value) (= step (round step)))
          (format nil "~D~A" (round value) label)
          (format nil "~,vF~A" (max 0 (ceiling (- (log step 10))))
                  value label))))
  (:method ((knob switch-knob) session)
    (if (knob-value knob session) "on" "off")))

;;; ---------------------------------------------------------------------
;;; Defining one.

(defmacro define-knob (name (&key label (group :grading) documentation
                                  (class ''scalar-knob)
                                  quantity (type 'single-float)
                                  unit-label minimum maximum step)
                       place)
  "Define NAME as a knob of CLASS over the setf-able PLACE.

PLACE may refer to SESSION, so a knob can live on the session -- its sky
clock, its player -- as well as in a special.  QUANTITY is a quantity plist
as DEFINE-QUANTITY-CONSTANT takes, (:quantity ... :unit ...); it and TYPE
make the knob's declaration, which VALUE-DECLARATION-FOR also answers for
NAME.  MINIMUM, MAXIMUM, and STEP bound and quantize what a control may set
on a scalar knob; a switch knob wants none.  How a change is realized is
CLASS's business."
  (let ((value (gensym "VALUE"))
        (query (gensym "NAME"))
        (source-form
          `(define-knob ,name (:quantity ,quantity :type ,type) ,place)))
    `(progn
       ,@(when quantity
           `((defmethod luv.arithmetic:value-declaration-for
                 ((,query (eql ',name)))
               (declare (ignore ,query))
               (load-time-value
                (luv.arithmetic:make-represented-value-declaration
                 :representation-type ',type
                 :quantity-specification
                 (luv.arithmetic:make-declared-quantity-specification
                  ',quantity)
                 :source-form ',source-form)))))
       (register-knob
        (make-instance
         ,class
         :name ',name :label ,(or label (substitute #\Space #\- (string-downcase name)))
         :group ,group :documentation ,documentation
         :declaration ,(when quantity
                         `(luv.arithmetic:value-declaration-for ',name))
         :unit-label ,unit-label
         ,@(when minimum `(:minimum ,minimum))
         ,@(when maximum `(:maximum ,maximum))
         ,@(when step `(:step ,step))
         :reader (lambda (session) (declare (ignorable session)) ,place)
         :writer (lambda (,value session)
                   (declare (ignorable session))
                   (setf ,place ,value)))))))

;;; ---------------------------------------------------------------------
;;; Knobs in shader source.
;;;
;;; A knob named in a shader body folds to a literal of the knob's quantity;
;;; the pipeline that folded it rebuilds when the value moves.  Only a knob
;;; whose place needs no session can stand in a shader, since the shader is
;;; parsed with none.

(defmethod luv.spir-v:shader-source-value ((name symbol))
  (let ((knob (find-knob name)))
    (if (and knob (typep knob 'scalar-knob))
        (values (knob-value knob nil) (knob-declaration knob) t)
        (values nil nil nil))))

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
;;; The metabar hook.

(defgeneric toggle-luvcraft-metabar (session)
  (:documentation
   "Slide SESSION's metabar of knobs in or out, returning true when one is
available.  The presentation extension supplies the method."))

(defmethod toggle-luvcraft-metabar ((session t))
  (declare (ignore session))
  nil)
