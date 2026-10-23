(in-package #:luft.wizard)

;;; The wizard's world: a small isle of meadow hills with a flagstone path
;;; winding north to south, a ring of standing stones on a levelled green,
;;; a pond, and scattered trees, in a sea.  It is a pure function of a cell's
;;; column, so it answers the terrain's world-source protocol directly and a
;;; streaming worker can raise any chunk of it alone.

;;; The palette. Nine kinds and their placements; the terrain says what a
;;; material means, this file says which ones the wizard's world has.

(defun wizard-kind (name &rest initargs)
  (apply #'make-instance 'terrain:material-kind :name name initargs))

(defun wizard-placement (name kind)
  (make-instance 'terrain:material-placement :name name :kind kind))

(defparameter *meadow*
  (wizard-placement
   :meadow
   (wizard-kind :meadow :base-tone '(0.34 0.25 0.16)
                        :top-tone '(0.27 0.43 0.13)
                        :side-tone '(0.34 0.25 0.16)
                        :bottom-tone '(0.22 0.17 0.12)
                        :relief :grass :relief-strength 0.8 :edge-wear 0.5)))

(defparameter *flagstone*
  (wizard-placement
   :flagstone
   (wizard-kind :flagstone :base-tone '(0.56 0.51 0.42)
                           :relief :stone :relief-strength 0.5 :edge-wear 0.6
                           :pattern '(:grid :courses 2 :aspect 1
                                      :mortar-width 0.10
                                      :mortar-tone (0.30 0.27 0.21)
                                      :variance 0.18))))

(defparameter *standing-stone*
  (wizard-placement
   :standing-stone
   (wizard-kind :standing-stone :base-tone '(0.36 0.37 0.40)
                                :relief :stone :relief-strength 0.9
                                :edge-wear 0.8)))

(defparameter *bedrock*
  (wizard-placement
   :bedrock
   (wizard-kind :bedrock :base-tone '(0.30 0.29 0.27)
                         :relief :stone :relief-strength 0.8 :edge-wear 0.7)))

(defparameter *sand*
  (wizard-placement
   :sand
   (wizard-kind :sand :base-tone '(0.70 0.60 0.40) :relief :grain)))

(defparameter *water*
  (wizard-placement
   :water
   (wizard-kind :water :base-tone '(0.08 0.20 0.32)
                       :relief :gloss :relief-strength 0.15 :roughness 0.1
                       :edge-wear 0.0)))

(defparameter *bark*
  (wizard-placement
   :bark
   (wizard-kind :bark :base-tone '(0.27 0.18 0.11) :relief :wood)))

(defparameter *leaves*
  (wizard-placement
   :leaves
   (wizard-kind :leaves :base-tone '(0.14 0.30 0.10) :relief :leaves
                        :relief-strength 0.7)))

(defparameter *ember*
  (wizard-placement
   :ember (wizard-kind :ember :base-tone '(1.0 0.32 0.035)
                             :surface-emission 4.0
                             :light-emission '(15 8 2))))

(defparameter *wizard-beacons*
  '(("GREEN" 96 107) ("RIDGE" 126 129) ("POND" 156 146))
  "The ember journey, in order. Each destination has a stone plinth.")

(defun make-wizard-palette ()
  "The wizard world's placement vocabulary."
  (domains:make-identity-vocabulary-domain
   :members (list *meadow* *flagstone* *standing-stone* *bedrock*
                  *sand* *water* *bark* *leaves* *ember*)
   :limit #xff))

;;; The isle.

(defconstant +wizard-world-bits+ 8
  "The isle is 256 cells square: four by four streamed chunks.")

(defparameter *wizard-sea-level* 3
  "Water fills every column whose ground lies below this height.")

(defparameter *wizard-stone-circle* '(96 104 7)
  "The standing stones' centre X and Y and their radius.")

(defparameter *wizard-pond* '(166 146 7)
  "The pond's centre X and Y and its radius.")

(defun wizard-hash (x y salt)
  "A deterministic hash of X, Y, and SALT in [0, 1)."
  (let ((h (logand #xffffffff
                   (+ (* x 374761393) (* y 668265263) (* salt 2246822519)))))
    (setf h (logand #xffffffff (* (logxor h (ash h -13)) 1274126177)))
    (/ (logand (logxor h (ash h -16)) #xffff) 65536.0)))

(defun wizard-path-centre-x (y)
  "Where the flagstone path crosses row Y."
  (+ 128.0 (* 20.0 (sin (* y 0.04))) (* 6.0 (sin (* y 0.11)))))

(defun wizard-distance (x y centre)
  (destructuring-bind (cx cy &rest rest) centre
    (declare (ignore rest))
    (sqrt (+ (expt (- x cx) 2) (expt (- y cy) 2)))))

(defun wizard-hill-height (x y)
  "The rolling ground before the green and the pond reshape it."
  (let* ((dx (/ (- x 128) 112.0))
         (dy (/ (- y 128) 112.0))
         (radius (sqrt (+ (* dx dx) (* dy dy))))
         (land (+ 9.0
                  (* 3.0 (sin (* x 0.061)) (cos (* y 0.047)))
                  (* 2.0 (sin (* (+ x y) 0.083)))
                  (cos (* (- x y) 0.13))))
         (shore (max 0.0 (* 4.0 (- radius 0.72)))))
    (- land (* (min 1.0 shore) (- land 1.0)))))

(defun wizard-ground-height (x y)
  "How many solid cells stand in column X, Y: its top solid cell is one less."
  (let* ((hills (wizard-hill-height x y))
         (green (wizard-distance x y *wizard-stone-circle*))
         (green-height
           (wizard-hill-height (first *wizard-stone-circle*)
                               (second *wizard-stone-circle*)))
         (pond (wizard-distance x y *wizard-pond*))
         (bank (wizard-hill-height (first *wizard-pond*)
                                   (second *wizard-pond*))))
    (max 1
         (floor
          (cond ((< pond (third *wizard-pond*)) (- bank 2.0))
                ((< green 11.0) green-height)
                ((< green 14.0)
                 (let ((blend (/ (- green 11.0) 3.0)))
                   (+ (* (- 1.0 blend) green-height) (* blend hills))))
                (t hills))))))

(defun wizard-column-top (x y height)
  "The placement on the top of column X, Y, HEIGHT cells high."
  (cond ((< (wizard-distance x y *wizard-pond*) (+ 2.0 (third *wizard-pond*)))
         *sand*)
        ((<= height (1+ *wizard-sea-level*)) *sand*)
        ((< (abs (- x (wizard-path-centre-x y))) 1.6) *flagstone*)
        (t *meadow*)))

(defun wizard-tree-at (i j)
  "The trunk X and Y of the tree in grid square I, J, or NIL when it has none."
  (when (< (wizard-hash i j 3) 0.5)
    (let ((x (+ (* 11 i) 3 (floor (* 5 (wizard-hash i j 1)))))
          (y (+ (* 11 j) 3 (floor (* 5 (wizard-hash i j 2))))))
      (when (and (> (abs (- x (wizard-path-centre-x y))) 4.0)
                 (> (wizard-distance x y *wizard-stone-circle*) 14.0)
                 (> (wizard-distance x y *wizard-pond*) 11.0)
                 (every (lambda (beacon)
                          (> (wizard-distance x y (cdr beacon)) 5.0))
                        *wizard-beacons*)
                 (> (wizard-ground-height x y) (+ 2 *wizard-sea-level*)))
        (values x y)))))

(defclass wizard-world-source (terrain:world-source) ()
  (:default-initargs
   :domain (luft:make-world-domain :x-bits +wizard-world-bits+
                                   :y-bits +wizard-world-bits+)
   :material-vocabulary (make-wizard-palette))
  (:documentation "The wizard's isle, as a deterministic Luft world source."))

(defun make-wizard-world-source (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'wizard-world-source initargs))

(defmethod terrain:generate-world-source-chunk
    ((source wizard-world-source) key place)
  (let* ((x0 (luft:chunk-origin-x key))
         (y0 (luft:chunk-origin-y key))
         (x1 (+ x0 luft:+chunk-size+))
         (y1 (+ y0 luft:+chunk-size+)))
    (flet ((place (x y z placement)
             ;; A tree near a seam lends its leaves only to its own chunk.
             (when (and (<= x0 x) (< x x1) (<= y0 y) (< y y1))
               (funcall place x y z placement))))
      (loop for x from x0 below x1 do
        (loop for y from y0 below y1 do
          (let ((height (wizard-ground-height x y)))
            (dotimes (z height)
              (place x y z
                     (cond ((= z (1- height)) (wizard-column-top x y height))
                           ((>= z (- height 3)) *meadow*)
                           (t *bedrock*))))
            (loop for z from height below *wizard-sea-level*
                  do (place x y z *water*))
            (when (< (wizard-distance x y *wizard-pond*) (third *wizard-pond*))
              (place x y height *water*)))))
      ;; The standing stones: eight around the green and one fallen altar.
      (destructuring-bind (cx cy radius) *wizard-stone-circle*
        (let ((ground (wizard-ground-height cx cy)))
          (dotimes (k 8)
            (let* ((angle (* k (/ pi 4)))
                   (sx (round (+ cx (* radius (cos angle)))))
                   (sy (round (+ cy (* radius (sin angle))))))
              (loop for z from ground below (+ ground 3)
                    do (place sx sy z *standing-stone*))
              (when (evenp k)
                (place sx sy (+ ground 3) *standing-stone*))))
          (loop for x from (1- cx) to (1+ cx) do
            (place x cy ground *standing-stone*))))
      ;; Low, recognizable destinations; leave the surrounding cells walkable.
      (dolist (beacon *wizard-beacons*)
        (destructuring-bind (name x y) beacon
          (declare (ignore name))
          (place x y (wizard-ground-height x y) *standing-stone*)))
      ;; Trees whose trunks may stand in this chunk or beside it.
      (loop for i from (1- (floor x0 11)) to (1+ (floor x1 11)) do
        (loop for j from (1- (floor y0 11)) to (1+ (floor y1 11)) do
          (multiple-value-bind (tx ty) (wizard-tree-at i j)
            (when tx
              (let ((ground (wizard-ground-height tx ty)))
                (loop for z from ground below (+ ground 3)
                      do (place tx ty z *bark*))
                (loop for dx from -1 to 1 do
                  (loop for dy from -1 to 1 do
                    (loop for z from (+ ground 3) below (+ ground 5)
                          do (place (+ tx dx) (+ ty dy) z *leaves*))))
                (place tx ty (+ ground 5) *leaves*))))))))
  (values))

(defmethod terrain:world-source-spawn ((source wizard-world-source))
  "On the path, south of the green, facing north along it."
  (let* ((y 118)
         (x (round (wizard-path-centre-x y))))
    (values x y (wizard-ground-height x y) (coerce (/ pi 2) 'single-float))))
