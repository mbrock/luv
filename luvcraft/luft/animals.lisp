(in-package #:luvcraft.luft.game)

;;; Reuse the turtle's deterministic mind and box model, but never ask the
;;; old world for contact. Its Y-up body crosses this one Z-up seam (#CS2DOF).
;;; Positions, velocities, yaw, dimensions and gait retain CRITTER/TURTLE's
;;; quantity declarations. CONTACT owns only a categorical occupancy handle;
;;; the translated figure rows declare Luft's quantities at PACK-FIGURE-INSTANCES.
(defclass animal-contact ()
  ((solid :initarg :solid :reader animal-contact-solid)))

(defun animal-luft-position (animal)
  (let ((p (luvcraft::critter-position animal)))
    (vec3:make-vec3 (+ 1024 (vec3:vec3-x p)) (+ 1024 (vec3:vec3-z p)) (vec3:vec3-y p))))

(defmethod luvcraft::move-critter-axis
    ((animal luvcraft::critter) (contact animal-contact) axis amount)
  (let* ((p (luvcraft::critter-position animal))
         (v (luvcraft::critter-velocity animal))
         (body (make-instance 'terrain:physical-body
                              :position (animal-luft-position animal)
                              :velocity (vec3:make-vec3 (vec3:vec3-x v) (vec3:vec3-z v) (vec3:vec3-y v))
                              :radius (luvcraft::critter-half-width animal)
                              :height (luvcraft::critter-height animal)))
         (luft-axis (ecase axis (:x :x) (:y :z) (:z :y))))
    (multiple-value-bind (travel blocked)
        (terrain::move-body-axis body (animal-contact-solid contact) luft-axis amount)
      (declare (ignore travel))
      (let ((new (terrain:body-position body)) (velocity (terrain:body-velocity body)))
        (setf (vec3:vec3-x p) (coerce (- (vec3:vec3-x new) 1024) 'double-float)
              (vec3:vec3-z p) (coerce (- (vec3:vec3-y new) 1024) 'double-float)
              (vec3:vec3-y p) (coerce (vec3:vec3-z new) 'double-float)
              (vec3:vec3-x v) (coerce (vec3:vec3-x velocity) 'double-float)
              (vec3:vec3-z v) (coerce (vec3:vec3-y velocity) 'double-float)
              (vec3:vec3-y v) (coerce (vec3:vec3-z velocity) 'double-float))
        (when (eq axis :y)
          (setf (luvcraft::critter-grounded-p animal) (terrain:body-grounded-p body))))
      blocked)))

(defun animal-description (animal)
  (let ((p (luvcraft::critter-position animal)) (v (luvcraft::critter-velocity animal)))
    (list :species (luvcraft::critter-species animal) :seed (luvcraft::critter-seed animal)
          :position (list (vec3:vec3-x p) (vec3:vec3-y p) (vec3:vec3-z p))
          :velocity (list (vec3:vec3-x v) (vec3:vec3-y v) (vec3:vec3-z v))
          :grounded (luvcraft::critter-grounded-p animal)
          :yaw (luvcraft::critter-yaw animal) :heading (luvcraft::turtle-heading animal)
          :decisions (luvcraft::critter-decision-count animal)
          :resting (luvcraft::turtle-resting-p animal) :mood (luvcraft::turtle-mood-seconds animal)
          :gait (luvcraft::turtle-gait-phase animal))))

(defun restore-animal (description)
  (unless (eq :turtle (getf description :species))
    (error "Unknown Luft animal species: ~S" (getf description :species)))
  (let ((animal (make-instance 'luvcraft::turtle
                               :position (apply #'vec3:make-vec3 (getf description :position))
                               :velocity (apply #'vec3:make-vec3 (getf description :velocity))
                               :grounded-p (getf description :grounded)
                               :seed (getf description :seed)
                               :yaw (getf description :yaw) :heading (getf description :heading))))
    (setf (luvcraft::critter-decision-count animal) (getf description :decisions)
          (luvcraft::turtle-resting-p animal) (getf description :resting)
          (luvcraft::turtle-mood-seconds animal) (getf description :mood)
          (luvcraft::turtle-gait-phase animal) (getf description :gait))
    animal))

(defun viewer-inhabitant-descriptions (viewer)
  ;; Before the first resident frame preserve the original save verbatim.
  ;; Other inhabitants always pass through, including ones this game cannot host.
  (if (viewer-animals-initialized-p viewer)
      (cons (list :luft-critters :animals (mapcar #'animal-description (viewer-animals viewer)))
            (remove :luft-critters (luvcraft-viewer-inhabitants viewer) :key #'first))
      (luvcraft-viewer-inhabitants viewer)))

(defun initialize-viewer-animals (viewer)
  (when (viewer-animals-initialized-p viewer)
    (return-from initialize-viewer-animals))
  (let* ((saved (assoc :luft-critters (luvcraft-viewer-inhabitants viewer)))
         (player (render:viewer-player viewer))
         (scene (render:viewer-source viewer)))
    (cond
      (saved
       (setf (viewer-animals viewer) (mapcar #'restore-animal (getf (rest saved) :animals))
             (viewer-animals-initialized-p viewer) t))
      ((and player (render::viewer-player-ground-resident-p viewer))
       (let ((p (terrain:body-position (terrain:character-body player))))
         (loop for seed from 1 to (* 8 (viewer-animal-count viewer))
               for dx = (- (mod (* seed 7) 17) 8)
               for dy = (- (mod (* seed 11) 19) 9)
               while (< (length (viewer-animals viewer)) (viewer-animal-count viewer))
               do (multiple-value-bind (face x y z point)
                      (terrain:raycast-face
                       scene (vec3:make-vec3 (+ (vec3:vec3-x p) dx) (+ (vec3:vec3-y p) dy) 254.0)
                       (vec3:make-vec3 0.0 0.0 -1.0) :max-distance 254.0)
                    (when (and face
                               (member (terrain:scene-material-placement-at
                                        scene (luft:make-site
                                               (luft:fiber-store-domain (terrain:scene-solid scene))
                                               x y z luft:+cell-extent+ 1))
                                       (mapcar #'game:block-placement
                                               (list luvcraft::*grass-block* luvcraft::*sand-block*
                                                     luvcraft::*moss-block*)))
                               (terrain::body-clear-at-p
                                (terrain::inspection-source-solid scene)
                                (vec3:vec3-x point) (vec3:vec3-y point) (vec3:vec3-z point)
                                luvcraft::+turtle-height+ luvcraft::+turtle-half-width+))
                      (push (make-instance 'luvcraft::turtle :seed seed
                                           :yaw (* 2d0 pi (luvcraft::critter-noise seed 0 7))
                                           :heading (* 2d0 pi (luvcraft::critter-noise seed 0 7))
                                           :position (vec3:make-vec3
                                                      (coerce (- (vec3:vec3-x point) 1024) 'double-float)
                                                      (coerce (vec3:vec3-z point) 'double-float)
                                                      (coerce (- (vec3:vec3-y point) 1024) 'double-float)))
                            (viewer-animals viewer)))))
         (when (or (viewer-animals viewer) (zerop (viewer-animal-count viewer)))
           (setf (viewer-animals-initialized-p viewer) t)))))))

(defclass turtle-population-knob (luv.knobs:scalar-knob) ())

(defmethod luv.knobs:realize-knob progn
    ((knob turtle-population-knob) (session luvcraft-viewer))
  ;; Re-seed the requested population on the next resident ground frame.
  (setf (viewer-animals session) nil
        (viewer-animals-initialized-p session) nil
        (slot-value session 'inhabitants)
        (remove :luft-critters (luvcraft-viewer-inhabitants session) :key #'first))
  (clrhash (viewer-animal-previous-figures session))
  (initialize-viewer-animals session))

(luv.knobs:define-knob turtle-count
    (:group :critters :label "turtles" :class 'turtle-population-knob
     :quantity (:quantity :body-count :unit :one)
     :type integer :minimum 0 :maximum 32 :step 1)
  (viewer-animal-count session))

(defmethod render:advance-viewer-game ((viewer luvcraft-viewer) seconds)
  (initialize-viewer-animals viewer)
  (let* ((scene (render:viewer-source viewer))
         (contact (make-instance 'animal-contact :solid (terrain::inspection-source-solid scene))))
    (dolist (animal (viewer-animals viewer))
      (setf (gethash animal (viewer-animal-previous-figures viewer)) (animal-figures animal))
      (let ((p (animal-luft-position animal)))
        ;; Unknown chunks are air to queries. Freeze inhabitants there rather
        ;; than dropping them through the absent world or forgetting them.
        (when (nth-value 1 (gethash (luft:chunk-key-at (floor (vec3:vec3-x p)) (floor (vec3:vec3-y p)))
                                    (terrain:streaming-scene-store scene)))
          (luvcraft::advance-critter animal contact seconds))))))

(defun animal-figures (animal)
  (let ((p (animal-luft-position animal))
        (yaw (luvcraft::critter-yaw animal))
        (figures nil))
    (luvcraft::map-critter-boxes
     animal
     (lambda (x y z hx hy hz tile)
       (let ((center (vec3:make-vec3 (+ (vec3:vec3-x p) (* (cos yaw) x) (* (sin yaw) z))
                                    (+ (vec3:vec3-y p) (- (* (sin yaw) x)) (* (cos yaw) z))
                                    (+ (vec3:vec3-z p) y)))
             (figure nil))
         (setf figure (render:box-figure center center (sin yaw) (cos yaw)
                                         (list hx hz hy) '(0.0 0.0 0.0))
               (fourth (fourth figure)) 4.0
               (fifth figure) (list 0.0 0.0 0.0 (float (luvcraft::block-atlas-tile-offset tile) 1.0)))
         (push figure figures))))
    (nreverse figures)))

(defmethod render:viewer-figures ((viewer luvcraft-viewer))
  (append (call-next-method)
          (loop for animal in (viewer-animals viewer)
                for figures = (animal-figures animal)
                for previous = (gethash animal (viewer-animal-previous-figures viewer))
                append (progn
                         (loop for figure in figures for old in previous
                               do (setf (second figure) (first old)
                                        (third figure) (append (subseq (third figure) 0 2)
                                                               (subseq (third old) 0 2))))
                         figures))))
