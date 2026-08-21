(in-package #:luvcraft.tests)

(defconstant +luft-frame-test-epsilon+ 2e-5)

(defun luft-frame-close-p (left right)
  (< (abs (- left right)) +luft-frame-test-epsilon+))

(defun luft-frame-vector-close-p (left right)
  (and (luft-frame-close-p (vec3-x left) (vec3-x right))
       (luft-frame-close-p (vec3-y left) (vec3-y right))
       (luft-frame-close-p (vec3-z left) (vec3-z right))))

(deftest luft-frame-adapter-reuses-one-camera-and-converts-the-complete-basis
  (let* ((source
           (make-instance 'fly-camera
                          :position (make-vec3 12.25d0 41.5d0 -7.75d0)
                          :yaw 0.61d0 :pitch -0.27d0
                          :field-of-view 0.93d0))
         (adapter (luvcraft::make-luft-frame-adapter))
         (target (luvcraft::luft-frame-adapter-camera adapter))
         (position (luft.render:camera-position target)))
    (ok (eq target
            (luvcraft::update-luft-frame-adapter-camera
             adapter source 17)))
    (ok (eq position (luft.render:camera-position target)))
    (ok (= 12.25 (vec3-x position)))
    (ok (= -7.75 (vec3-y position)))
    (ok (= 24.5 (vec3-z position)))
    (ok (luft-frame-close-p
         (luft.render:camera-yaw target) (- (/ pi 2) 0.61d0)))
    (ok (luft-frame-close-p (luft.render:camera-pitch target) -0.27d0))
    (ok (luft-frame-close-p
         (luft.render:camera-field-of-view target) 0.93d0))
    (multiple-value-bind (source-right source-up source-forward)
        (camera-basis source)
      (multiple-value-bind (target-right target-up target-forward)
          (luft.render:camera-basis target)
        (flet ((converted-direction (direction)
                 (make-vec3 (vec3-x direction)
                            (vec3-z direction)
                            (vec3-y direction))))
          (ok (luft-frame-vector-close-p
               (converted-direction source-right) target-right))
          (ok (luft-frame-vector-close-p
               (converted-direction source-up) target-up))
          (ok (luft-frame-vector-close-p
               (converted-direction source-forward) target-forward)))))
    ;; A second pose mutates the same camera and the same position vector.
    (setf (vec3-x (camera-position source)) -3.0
          (vec3-y (camera-position source)) 19.0
          (vec3-z (camera-position source)) 8.0)
    (luvcraft::update-luft-frame-adapter-camera adapter source 20)
    (ok (eq target (luvcraft::luft-frame-adapter-camera adapter)))
    (ok (eq position (luft.render:camera-position target)))
    (ok (equalp #(-3.0 8.0 -1.0)
                (vector (vec3-x position)
                        (vec3-y position)
                        (vec3-z position))))))

(deftest luft-frame-uniform-packs-position-basis-projection-and-domain
  (let* ((source
           (make-instance 'fly-camera
                          :position (make-vec3 13.0 23.0 -7.0)
                          :yaw (/ pi 2) :pitch 0.0
                          :field-of-view (/ pi 2)))
         (session (make-instance 'luvcraft-session :camera source))
         (materialization
           (luvcraft::make-luft-world-materialization
            (make-block-world) :horizontal-bits 6 :vertical-origin 17))
         (adapter (luvcraft::make-luft-frame-adapter))
         (data
           (luvcraft::luft-frame-adapter-uniform-data
            adapter session materialization 200 100 :sky-p nil)))
    (ok (= 104 (length data)))
    ;; Position (X,Z,Y-origin), followed by right, up, and forward.
    (ok (equalp #(13.0 -7.0 6.0)
                (subseq data 0 3)))
    (ok (every #'identity
               (map 'list #'luft-frame-close-p
                    #(0.0 -1.0 0.0) (subseq data 4 7))))
    (ok (every #'identity
               (map 'list #'luft-frame-close-p
                    #(0.0 0.0 1.0) (subseq data 8 11))))
    (ok (every #'identity
               (map 'list #'luft-frame-close-p
                    #(1.0 0.0 0.0) (subseq data 12 15))))
    ;; LUFT's own 90-degree projection: aspect two, near .1, far 400.
    (ok (luft-frame-close-p 0.5 (aref data 16)))
    (ok (luft-frame-close-p 1.0 (aref data 17)))
    (ok (luft-frame-close-p (/ 400.0 399.9) (aref data 18)))
    (ok (luft-frame-close-p (/ -40.0 399.9) (aref data 19)))
    ;; Both horizontal LUFT axes use the materialization's six-bit period.
    (ok (= 64.0 (aref data 28)))
    (ok (= 64.0 (aref data 29)))
    (ok (= luft.render:*chamfer-width* (aref data 30)))
    (ok (= luft.render:*arris-softness* (aref data 31)))
    ;; With no temporal predecessor, LUFT repeats the converted current eye.
    (ok (equalp (subseq data 0 4) (subseq data 76 80)))))

(deftest luft-frame-uniform-factors-luvcraft-sun-and-ambient-exactly
  (let* ((clock (make-instance 'luvcraft::sky-clock
                               :pinned-day-fraction 0.5))
         (profile (make-default-sky-profile))
         (source (make-instance 'fly-camera))
         (session
           (make-instance 'luvcraft-session
                          :camera source :sky-clock clock
                          :sky-profile profile))
         (materialization
           (luvcraft::make-luft-world-materialization (make-block-world)))
         (data
           (luvcraft::luft-frame-adapter-uniform-data
            (luvcraft::make-luft-frame-adapter)
            session materialization 320 180))
         (sky (sky-frame-parameters clock profile))
         (sun (luvcraft::sky-frame-parameters-sun-direction sky))
         (sun-colour (luvcraft::sky-frame-parameters-sun-color sky))
         (ambient (luvcraft::sky-frame-parameters-ambient-color sky))
         (strength (max (aref ambient 0)
                        (aref ambient 1)
                        (aref ambient 2))))
    ;; Direction conversion has no vertical-origin translation.
    (ok (luft-frame-close-p (vec3-x sun) (aref data 20)))
    (ok (luft-frame-close-p (vec3-z sun) (aref data 21)))
    (ok (luft-frame-close-p (vec3-y sun) (aref data 22)))
    (ok (luft-frame-close-p strength (aref data 23)))
    ;; Equal sky and ground factors make LUFT's two hemisphere weights sum
    ;; back to Luvcraft's exact isotropic ambient RGB.
    (dotimes (component 3)
      (ok (luft-frame-close-p
           (aref ambient component)
           (* (aref data 23) (aref data (+ 24 component)))))
      (ok (luft-frame-close-p
           (aref data (+ 24 component))
           (aref data (+ 40 component))))
      (ok (luft-frame-close-p
           (* (luvcraft::sky-frame-parameters-day-factor sky)
              (aref sun-colour component))
           (aref data (+ 32 component)))))
    (ok (zerop (aref data 39)))))
