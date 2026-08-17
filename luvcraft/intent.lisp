(in-package #:luvcraft)

;;; What a body is trying to do, as distinct from what someone pressed.
;;;
;;; A hash of held keys answered "is W down".  An intent answers "is this body
;;; trying to go forward", which is a question a keyboard, a gamepad, a script,
;;; a recorded demo, and an animal's rider can all answer, and which the
;;; physics step can read without knowing that keyboards exist.
;;;
;;; Every direction is held separately rather than summed into one number,
;;; because pressing both W and S is standing still and then letting go of S
;;; must leave the player walking forward again.  A single axis cannot
;;; remember that; a set of urges can.

(defclass movement-intent ()
  ((urges
    :initform (make-hash-table :test #'eq)
    :reader movement-intent-urges
    :documentation "The urges currently held, as a set of keywords.")
   (jump-requested-p
    :initform nil
    :accessor movement-intent-jump-requested-p
    :documentation "Whether a jump is owed, cleared by whoever performs it."))
  (:documentation
   "One body's movement will: which directions it is urging, and whether it
owes a jump.

The urges are named after the body's own frame -- :FORWARD, :BACKWARD, :LEFT,
:RIGHT, and :SPRINT -- rather than after any key or axis, so the same intent
serves a walking player and a ridden animal."))

(defun make-movement-intent ()
  "Construct an intent urging nothing."
  (make-instance 'movement-intent))

(defun movement-urging-p (intent urge)
  "Whether INTENT currently holds URGE."
  (values (gethash urge (movement-intent-urges intent))))

(defun (setf movement-urging-p) (value intent urge)
  "Begin or end INTENT's URGE."
  (if value
      (setf (gethash urge (movement-intent-urges intent)) t)
      (remhash urge (movement-intent-urges intent)))
  value)

(defun movement-intent-axis (intent positive negative)
  "Return -1, 0, or 1 from the two opposed urges INTENT holds."
  (- (if (movement-urging-p intent positive) 1d0 0d0)
     (if (movement-urging-p intent negative) 1d0 0d0)))

(defun movement-intent-sprinting-p (intent)
  "Whether INTENT is urging its body to hurry."
  (movement-urging-p intent :sprint))

(defun movement-intent-still-p (intent)
  "Whether INTENT is urging nothing at all."
  (and (zerop (hash-table-count (movement-intent-urges intent)))
       (not (movement-intent-jump-requested-p intent))))

(defun clear-movement-intent (intent)
  "Let go of everything INTENT was holding."
  (clrhash (movement-intent-urges intent))
  (setf (movement-intent-jump-requested-p intent) nil)
  intent)
