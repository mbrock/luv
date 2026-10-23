(in-package #:luft.wizard)

;;; A deliberately small, session-local journey (#DLH49J). The ember is an
;;; authored terrain edit: residency may discard its mesh, not its meaning.

(defun wizard-next-beacon (wizard)
  (nth (wizard-embers wizard) *wizard-beacons*))

(defun wizard-beacon-distance (wizard beacon)
  (let ((feet (terrain:body-position
               (terrain:character-body (wizard-character wizard)))))
    (sqrt (+ (expt (- (vec3-x feet) (+ 0.5 (second beacon))) 2)
             (expt (- (vec3-y feet) (+ 0.5 (third beacon))) 2)))))

(defun cast-wizard-ember (wizard)
  "Kindle the next plinth within four cells; failed casts spend nothing."
  (let ((beacon (wizard-next-beacon wizard)))
    (cond
      ((null beacon)
       (setf (wizard-message wizard) "THE ISLE IS AWAKE. THE EMBER IS YOURS."))
      ((> (wizard-beacon-distance wizard beacon) 4.0)
       (setf (wizard-message wizard) "MOVE CLOSER TO THE GOLD WAYMARK"))
      (t
       (destructuring-bind (name x y) beacon
         (declare (ignore name))
         (let* ((scene (wizard-scene wizard))
                (cell (luft:make-site
                       (terrain:scene-domain scene) x y
                       (1+ (wizard-ground-height x y)) luft:+cell-extent+ 1)))
           (multiple-value-bind (edit status key)
               (terrain:edit-streaming-scene-cell scene cell *ember*)
             (cond
               (edit
                (terrain:schedule-streaming-scene-edit
                 scene (wizard-production-system wizard) key *wizard-bevel-width*)
                (incf (wizard-embers wizard))
                (setf (wizard-spell-time wizard) (wizard-last-timestamp wizard)
                      (wizard-message wizard)
                      (if (wizard-next-beacon wizard)
                          "EMBER LEARNED. CARRY ITS LIGHT TO THE NEXT PLINTH."
                          "THREE LIGHTS. THE ISLE IS AWAKE. WELL WALKED.")))
               ((eq status :busy)
                (setf (wizard-message wizard) "THE GROUND IS SETTLING. CAST AGAIN."))
               (t (setf (wizard-message wizard) "THE EMBER CANNOT SETTLE HERE."))))))))))

(defun wizard-objective (wizard)
  (let ((beacon (wizard-next-beacon wizard)))
    (if beacon
        (format nil "~D/3 LIGHTS   ~A ~D CELLS   ~A"
                (wizard-embers wizard) (first beacon)
                (round (wizard-beacon-distance wizard beacon))
                (if (< (wizard-beacon-distance wizard beacon) 4.0)
                    "SPACE TO KINDLE" "FOLLOW THE GOLD WAYMARK"))
        "3/3 LIGHTS   THE EMBER WALK IS COMPLETE")))
