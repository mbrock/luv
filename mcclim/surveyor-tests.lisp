(defpackage #:mcluv.surveyor-tests
  (:use #:cl #:rove))

(in-package #:mcluv.surveyor-tests)

(deftest surveyor-captures-one-dense-terrain-product
  (let* ((world (luvcraft:make-empty-little-block-world :seed 121))
         (camera
           (make-instance 'luvcraft:fly-camera
                          :position (luvcraft::make-vec3 12d0 10d0 34d0)))
         (player (luvcraft:make-player-for-camera camera))
         (session
           (make-instance 'luvcraft:luvcraft-session
                          :world world :player player))
         (snapshot
           (mcluv::capture-surveyor-map-snapshot
            session :width 8 :depth 6)))
    (ok (= 48 (length (mcluv::surveyor-snapshot-heights snapshot))))
    (ok (= 48 (length (mcluv::surveyor-snapshot-materials snapshot))))
    (ok (= 48 (length (mcluv::surveyor-snapshot-lights snapshot))))
    (ok (= 12 (mcluv::surveyor-snapshot-center-x snapshot)))
    (ok (= 34 (mcluv::surveyor-snapshot-center-z snapshot)))
    (ok (<= (mcluv::surveyor-snapshot-minimum-height snapshot)
            (mcluv::surveyor-snapshot-maximum-height snapshot)))
    (ok (every (lambda (material) (typep material 'luvcraft:block-kind))
               (coerce (mcluv::surveyor-snapshot-materials snapshot) 'list)))))

(deftest unavailable-world-light-is-explicit
  (let ((world (luvcraft:make-empty-little-block-world :seed 121)))
    (multiple-value-bind (sky block state)
        (luvcraft:world-light-levels-at world 200 8 200)
      (ok (zerop sky))
      (ok (zerop block))
      (ok (eq state :unavailable)))))
