;;;; The wizard plays through the engine's protocols: the isle answers the
;;;; world-source protocol, and a click is the renderer's screen ray, the
;;;; terrain's ray and route, and the simulation's step -- all without a GPU.

(defpackage #:luft.wizard.tests
  (:use #:cl)
  (:import-from #:parachute #:define-test #:true #:is)
  (:import-from #:luv.arithmetic.lisp.vec3
                #:make-vec3 #:vec3-x #:vec3-y #:vec3-z)
  (:local-nicknames (#:wizard #:luft.wizard)
                    (#:terrain #:luft.terrain)
                    (#:render #:luft.render)))

(in-package #:luft.wizard.tests)

(defun isle-scene (source x0 y0 x1 y1)
  "A finite scene of SOURCE's cells in columns [X0, X1) by [Y0, Y1)."
  (let ((builder (terrain:make-scene-builder
                  :horizontal-bits 8
                  :material-vocabulary
                  (terrain:world-source-material-vocabulary source)))
        (keys nil))
    (loop for x from x0 below x1 by luft:+chunk-size+ do
      (loop for y from y0 below y1 by luft:+chunk-size+ do
        (pushnew (luft:chunk-key-at x y) keys)))
    (pushnew (luft:chunk-key-at (1- x1) (1- y1)) keys)
    (pushnew (luft:chunk-key-at x0 (1- y1)) keys)
    (pushnew (luft:chunk-key-at (1- x1) y0) keys)
    (dolist (key keys)
      (terrain:generate-world-source-chunk
       source key
       (lambda (x y z placement)
         (when (and (<= x0 x) (< x x1) (<= y0 y) (< y y1))
           (terrain:scene-builder-cell builder x y z :material placement)))))
    (terrain:finish-scene-builder builder)))

(defun placement-at (scene x y z)
  (terrain:scene-material-placement-at
   scene (luft:make-site (terrain:scene-domain scene) x y z luft:+cell-extent+ 1)))

(define-test the-wizard-arrives-on-his-path
  (let* ((source (wizard:make-wizard-world-source))
         (scene (isle-scene source 80 90 140 150)))
    (multiple-value-bind (x y z) (terrain:world-source-spawn source)
      (is = 1 (terrain:scene-cell-bit scene x y (1- z)))
      (is = 0 (terrain:scene-cell-bit scene x y z))
      (is = 0 (terrain:scene-cell-bit scene x y (1+ z)))
      (is eq :flagstone
          (terrain:material-placement-name (placement-at scene x y (1- z)))))))

(defun screen-point (camera point width height isometric-height)
  "Where world POINT falls on CAMERA's isometric screen, derived by hand."
  (multiple-value-bind (right up) (render:camera-basis camera)
    (let* ((eye (render:camera-position camera))
           (dx (- (vec3-x point) (vec3-x eye)))
           (dy (- (vec3-y point) (vec3-y eye)))
           (dz (- (vec3-z point) (vec3-z eye)))
           (half (/ isometric-height 2.0))
           (ndc-x (/ (+ (* dx (vec3-x right)) (* dy (vec3-y right)) (* dz (vec3-z right)))
                     (* half (/ width height))))
           (ndc-y (/ (+ (* dx (vec3-x up)) (* dy (vec3-y up)) (* dz (vec3-z up)))
                     half)))
      (values (* width (/ (1+ ndc-x) 2.0))
              (* height (/ (- 1.0 ndc-y) 2.0))))))

(define-test a-click-walks-the-wizard-to-the-cell-it-names
  (let* ((source (wizard:make-wizard-world-source))
         (scene (isle-scene source 80 90 140 150))
         (simulation (terrain:make-world-simulation scene)))
    (multiple-value-bind (x y z) (terrain:world-source-spawn source)
      (let* ((character (terrain:make-walking-character
                         :position (make-vec3 (+ x 0.5) (+ y 0.5)
                                              (coerce z 'single-float))))
             ;; The game's camera, aimed at his chest from forty cells back.
             (camera (make-instance 'render:fly-camera
                                    :position (make-vec3 0.0 0.0 0.0)
                                    :yaw 2.2455373 :pitch -0.5165006))
             (forward (nth-value 2 (render:camera-basis camera)))
             ;; Six rows up the path, off the centre of the screen.
             (ty (+ y 6))
             (tx (round (wizard:wizard-path-centre-x ty)))
             (tz (wizard:wizard-ground-height tx ty)))
        (setf (render:camera-position camera)
              (make-vec3 (- (+ x 0.5) (* 40 (vec3-x forward)))
                         (- (+ y 0.5) (* 40 (vec3-y forward)))
                         (- (+ z 1.45) (* 40 (vec3-z forward)))))
        (terrain:add-simulation-character simulation character)
        (multiple-value-bind (px py)
            (screen-point camera (make-vec3 (+ tx 0.5) (+ ty 0.5) tz)
                          1100.0 800.0 18.0)
          (multiple-value-bind (origin direction)
              (let ((render:*projection* :isometric)
                    (render:*isometric-height* 18.0))
                (render:camera-pointer-ray camera 1100.0 800.0 px py))
            (is equal (list tx ty tz)
                (multiple-value-list
                 (terrain:raycast-standing-cell scene origin direction)))))
        (let ((route (terrain:start-character-route character scene tx ty tz)))
          (is eq :running (terrain:walking-route-status route))
          (loop repeat (* 60 20)
                until (eq :arrived (terrain:walking-route-status route))
                do (terrain:advance-world-simulation simulation (/ 1.0 60)))
          (is eq :arrived (terrain:walking-route-status route))
          (let ((feet (terrain:body-position (terrain:character-body character))))
            (true (< (abs (- (vec3-x feet) (+ tx 0.5))) 0.2))
            (true (< (abs (- (vec3-y feet) (+ ty 0.5))) 0.2))
            (true (< (abs (- (vec3-z feet) tz)) 0.2))))))))

(define-test a-wall-or-the-sky-names-nowhere-to-stand
  (let* ((source (wizard:make-wizard-world-source))
         (scene (isle-scene source 80 90 140 150)))
    (destructuring-bind (cx cy radius) wizard::*wizard-stone-circle*
      (let* ((ground (wizard:wizard-ground-height cx cy))
             (origin (make-vec3 (+ cx 0.5) (+ cy 0.5) (+ ground 1.5)))
             (east (make-vec3 1.0 0.0 0.0)))
        ;; Level from the green's middle, the first thing east is a stone.
        (multiple-value-bind (face x y z) (terrain:raycast-face scene origin east)
          (true face)
          (is equal (list (+ cx radius) cy (1+ ground)) (list x y z))
          (is eq :standing-stone
              (terrain:material-placement-name (placement-at scene x y z))))
        ;; Its west face is a wall, not a floor.
        (is eq nil (terrain:raycast-standing-cell scene origin east))
        (is eq nil (terrain:raycast-standing-cell
                    scene origin (make-vec3 0.0 0.0 1.0)))))))

(defun route-fixture (walls)
  (let ((builder (terrain:make-scene-builder
                  :horizontal-bits 5
                  :material-vocabulary (wizard:make-wizard-palette))))
    (dotimes (x 16)
      (dotimes (y 16)
        (terrain:scene-builder-cell builder x y 0 :material wizard::*flagstone*)))
    (dolist (xy walls)
      (loop for z from 1 to 4 do
        (terrain:scene-builder-cell builder (first xy) (second xy) z
                                    :material wizard::*standing-stone*)))
    (terrain:finish-scene-builder builder)))

(defun walk-route (character scene x y z)
  (let* ((simulation (terrain:make-world-simulation scene))
         (route (terrain:start-character-route character scene x y z)))
    (terrain:add-simulation-character simulation character)
    (loop repeat 18000 while (eq :running (terrain:walking-route-status route))
          do (terrain:advance-world-simulation simulation (/ 1.0 60)))
    route))

(define-test click-routes-detour-around-a-wall-and-actually-arrive
  (let* ((scene (route-fixture (loop for y from 2 to 12 collect (list 7 y))))
         (character (terrain:make-walking-character :position (make-vec3 4.5 7.5 1.0)))
         (route (terrain:start-character-route character scene 10 7 1)))
    (is eq :running (terrain:walking-route-status route))
    (true (some (lambda (cell) (or (< (luft:site-y cell) 2) (> (luft:site-y cell) 12)))
                (terrain:walking-route-cells route)))
    (is eq :arrived (terrain:walking-route-status (walk-route character scene 10 7 1)))))

(define-test routes-refuse-blocked-corners-and-enclosed-destinations
  (let* ((scene (route-fixture '((6 7) (7 6) (8 7) (7 8))))
         (character (terrain:make-walking-character :position (make-vec3 4.5 4.5 1.0))))
    (is eq :failed (terrain:walking-route-status
                   (terrain:start-character-route character scene 7 7 1)))
    (is eq :failed (terrain:walking-route-status
                   (terrain:start-character-route character scene 6 7 1)))
    (is eq :arrived (terrain:walking-route-status
                    (terrain:start-character-route character scene 4 4 1)))))

(define-test the-ember-journey-is-walkable-from-spawn
  (let* ((source (wizard:make-wizard-world-source))
         (scene (isle-scene source 80 90 180 165))
         (character (wizard::make-wizard-character
                     ;; MAKE-WIZARD-CHARACTER needs only the source spawn.
                     (terrain:make-world-source-streaming-scene source))))
    (dolist (beacon wizard::*wizard-beacons*)
      (let ((x (- (second beacon) 2)) (y (third beacon)))
        (is eq :arrived
            (terrain:walking-route-status
             (walk-route character scene x y (wizard:wizard-ground-height x y)))
            "The walker reaches ~A without teleporting" (first beacon))))))
