(in-package #:luvcraft.luft.game)

;;; Keep the original walking motor; only its two terrain operations change.
(defclass luvcraft-character (terrain:walking-character) ()
  (:metaclass luv.arithmetic.records:quantity-class))

(defclass player-contact ()
  ((solid :initarg :solid :reader player-contact-solid)
   (body :initarg :body :reader player-contact-body)))

(defun player-contact-vector (v)
  "Exchange Y/Z at the old motor boundary, retaining double precision."
  (vec3:make-vec3 (coerce (vec3:vec3-x v) 'double-float)
                  (coerce (vec3:vec3-z v) 'double-float)
                  (coerce (vec3:vec3-y v) 'double-float)))

(defmethod luvcraft::move-body-axis (player (contact player-contact) axis distance)
  (let ((body (player-contact-body contact)))
    (setf (terrain:body-velocity body)
          (player-contact-vector (luvcraft::body-velocity player)))
    (multiple-value-bind (travel blocked)
        (terrain::move-body-axis body (player-contact-solid contact)
                                 (ecase axis (:x :x) (:y :z) (:z :y)) distance)
      (declare (ignore travel))
      ;; The motor holds the velocity vector across all three sweeps.
      (let ((p (terrain:body-position body)) (v (terrain:body-velocity body))
            (pp (luvcraft::body-position player)) (pv (luvcraft::body-velocity player)))
        (setf (vec3:vec3-x pp) (vec3:vec3-x p)
              (vec3:vec3-y pp) (vec3:vec3-z p)
              (vec3:vec3-z pp) (vec3:vec3-y p)
              (vec3:vec3-x pv) (vec3:vec3-x v)
              (vec3:vec3-y pv) (vec3:vec3-z v)
              (vec3:vec3-z pv) (vec3:vec3-y v)))
      (when (and blocked (eq axis :y) (minusp distance))
        (setf (luvcraft::body-grounded-p player) t))
      blocked)))

(defmethod luvcraft::body-position-clear-p (player (contact player-contact) x y z)
  (terrain:body-clear-at-p (player-contact-solid contact) x z y
                          (luvcraft::body-height player) (luvcraft::body-half-width player)))

(defmethod terrain:advance-character-motion ((character luvcraft-character) solid seconds)
  (if (terrain:character-flying-p character)
      (call-next-method)
      (let* ((body (terrain:character-body character))
             (proxy (make-instance
                     'luvcraft::block-world-player
                     :position (player-contact-vector (terrain:body-position body))
                     :velocity (player-contact-vector (terrain:body-velocity body))
                     :grounded-p (terrain:body-grounded-p body)
                     :half-width (coerce (terrain:body-radius body) 'double-float)
                     :height (coerce (terrain:body-height body) 'double-float)
                     :ground-acceleration (coerce (terrain:character-ground-acceleration character) 'double-float)
                     :air-acceleration (coerce (terrain:character-air-acceleration character) 'double-float)
                     :gravity (coerce (- (terrain:body-gravity body)) 'double-float)
                     :jump-speed (coerce (terrain:character-jump-speed character) 'double-float)))
             (contact (make-instance 'player-contact :solid solid :body body)))
        (multiple-value-bind (x y jump)
            (terrain:controller-desire (terrain:character-controller character) character seconds)
          (luvcraft:step-walking-body proxy contact
                                     (or x (vec3:vec3-x (terrain:body-velocity body)))
                                     (or y (vec3:vec3-y (terrain:body-velocity body)))
                                     seconds :jump-p jump))
        (setf (terrain:body-grounded-p body) (luvcraft::body-grounded-p proxy)))))

(defun install-luvcraft-motor (player)
  (when (and player (not (typep player 'luvcraft-character)))
    (change-class player 'luvcraft-character)))

(defmethod (setf render:viewer-player) :after (player (viewer luvcraft-viewer))
  (install-luvcraft-motor player))
