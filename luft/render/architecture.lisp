;;; Architecture: what a world of cells can be built into.
;;;
;;; A cube world's reputation is that it can only make boxes, and that is
;;; only true of a world without a vocabulary.  A round-headed opening is a
;;; column of cleared cells whose height follows a circle; a tower is a disc
;;; repeated up the Z axis; a corbel is a course that oversails the one
;;; below.  Every one of these is a loop of a dozen lines, and each becomes
;;; a whole architecture once the chamfer runs along its staircase and the
;;; stock says what it is cut from.
;;;
;;; So this file is two things.  First a small vocabulary -- discs, rings,
;;; arches with their voussoirs, stairs, corbels, crenellation, terrain --
;;; written against the WORLD of luft/render/render.lisp so that everything
;;; it fills is stamped with the stock in force.  Then a handful of pieces
;;; built out of that vocabulary, each with cameras chosen for it, because a
;;; building that is never photographed from the right place is a building
;;; nobody has seen.  #SY26PO #6QZYNB

(in-package #:luft.render)

;;; ------------------------------------------------------------------------
;;; A vocabulary

(defun fill-disc (world cx cy radius z0 z1 &optional (state t))
  "Set every cell within RADIUS of the axis through CX,CY between Z0 and Z1.

The disc is the set of cell centres inside the circle, which on a grid is
a staircase; the chamfer turns each of its steps into a facet, and a tower
of eight cells' radius reads as round from any distance at all."
  (let ((limit (* (+ radius 0.5) (+ radius 0.5))))
    (loop for x from (- cx (ceiling radius)) to (+ cx (ceiling radius))
          do (loop for y from (- cy (ceiling radius)) to (+ cy (ceiling radius))
                   for dx = (- (+ x 0.5) (+ cx 0.5))
                   for dy = (- (+ y 0.5) (+ cy 0.5))
                   when (<= (+ (* dx dx) (* dy dy)) limit)
                     do (loop for z from z0 to z1
                              do (setf (world-cell-p world x y z) state))))))

(defun fill-ring (world cx cy inner outer z0 z1 &optional (state t))
  "Set every cell between the INNER and OUTER radii: a tower's wall."
  (let ((low (* (+ inner 0.5) (+ inner 0.5)))
        (high (* (+ outer 0.5) (+ outer 0.5))))
    (loop for x from (- cx (ceiling outer)) to (+ cx (ceiling outer))
          do (loop for y from (- cy (ceiling outer)) to (+ cy (ceiling outer))
                   for dx = (- (+ x 0.5) (+ cx 0.5))
                   for dy = (- (+ y 0.5) (+ cy 0.5))
                   for distance = (+ (* dx dx) (* dy dy))
                   when (and (< low distance) (<= distance high))
                     do (loop for z from z0 to z1
                              do (setf (world-cell-p world x y z) state))))))

(defun arch-rise (offset radius)
  "How far above the springing a round arch of RADIUS reaches at OFFSET."
  (let ((square (- (* radius radius) (* offset offset))))
    (if (plusp square) (round (sqrt square)) 0)))

(defun carve-arch (world centre floor springing radius across
                   &key (axis :x))
  "Cut a round-headed opening through a wall.

The opening runs from FLOOR up to SPRINGING as a rectangle and closes above
it on a circle of RADIUS about CENTRE along AXIS; ACROSS is the inclusive
range of the other horizontal axis, the thickness the opening passes
through.  It is the shape a mason gets from a centring, and the shape a
grid gets from one square root.  #SY26PO"
  (destructuring-bind (near . far) across
    (loop for offset from (- radius) to radius
          for rise = (arch-rise offset radius)
          when (plusp rise)
            do (loop for z from floor below (+ springing rise)
                     do (loop for other from near to far
                              do (if (eq axis :x)
                                     (setf (world-cell-p
                                            world (+ centre offset) other z)
                                           nil)
                                     (setf (world-cell-p
                                            world other (+ centre offset) z)
                                           nil)))))))

(defun ring-arch (world centre floor springing radius across
                  &key (axis :x) (thickness 1))
  "Lay the voussoirs of an arch: the course of stock just outside its curve.

Over the opening the ring runs from the arch's own head up to the head of
a circle THICKNESS wider, and beside it the same course comes down to the
floor as the jamb.  The stock in force becomes the ring, so a limestone
wall can carry a granite arch and the opening reads as built rather than
as punched."
  (destructuring-bind (near . far) across
    (flet ((stone (offset z)
             (loop for other from near to far
                   do (if (eq axis :x)
                          (setf (world-cell-p world (+ centre offset) other z)
                                t)
                          (setf (world-cell-p world other (+ centre offset) z)
                                t)))))
      (loop for offset from (- (+ radius thickness)) to (+ radius thickness)
            for inner = (arch-rise offset radius)
            for outer = (arch-rise offset (+ radius thickness))
            ;; Never let a rounding leave a hole in the ring: a voussoir
            ;; is at least one cell wherever the arch has a head at all.
            for head = (max (+ springing outer) (+ springing inner 1))
            do (if (<= (abs offset) radius)
                   (loop for z from (+ springing inner) below head
                         do (stone offset z))
                   (loop for z from floor below head
                         do (stone offset z)))))))

(defun corbel (world x0 x1 y0 y1 z courses &key (oversail 1))
  "Build COURSES above Z, each oversailing the last by OVERSAIL cells.

A corbel is how a wall becomes a wider thing at the top without a beam:
the machicolation of a tower, the cornice of an arcade, the bracket under
a balcony."
  (loop for course from 0 below courses
        for out = (* oversail (1+ course))
        do (fill-box world (- x0 out) (+ x1 out) (- y0 out) (+ y1 out)
                     (+ z course) (+ z course))))

(defun crenellate (world x0 x1 y0 y1 z &key (period 2) (merlon 1))
  "Raise merlons around the rectangle at height Z, every PERIOD cells.

Only the rim is raised, so the walk behind it stays open."
  (loop for x from x0 to x1
        do (loop for y from y0 to y1
                 when (and (or (= x x0) (= x x1) (= y y0) (= y y1))
                           (zerop (mod (+ x y) period)))
                   do (loop for k from 0 below merlon
                            do (setf (world-cell-p world x y (+ z k)) t)))))

(defun crenellate-disc (world cx cy radius z &key (period 2) (merlon 1))
  "Raise merlons around a disc's rim, every PERIOD cells of its circuit."
  (let ((low (* (- radius 0.5) (- radius 0.5)))
        (high (* (+ radius 0.5) (+ radius 0.5))))
    (loop for x from (- cx (ceiling radius)) to (+ cx (ceiling radius))
          do (loop for y from (- cy (ceiling radius))
                     to (+ cy (ceiling radius))
                   for dx = (- (+ x 0.5) (+ cx 0.5))
                   for dy = (- (+ y 0.5) (+ cy 0.5))
                   for distance = (+ (* dx dx) (* dy dy))
                   when (and (< low distance) (<= distance high)
                             (zerop (mod (round (* 8 (/ (atan dy dx) pi)))
                                         period)))
                     do (loop for k from 0 below merlon
                              do (setf (world-cell-p world x y (+ z k)) t))))))

(defun fill-stairs (world x0 y0 y1 steps &key (base 0) (direction 1))
  "A flight of STEPS rising along X from X0, each step spanning Y0 to Y1."
  (loop for step from 0 below steps
        for x = (+ x0 (* direction step))
        do (fill-box world x x y0 y1 base (+ base step))))

(defun spiral-stair (world cx cy radius z0 z1 &key (turns 2))
  "A stair winding up the inside of a tower from Z0 to Z1."
  (let ((rise (max 1 (- z1 z0))))
    (loop for z from z0 to z1
          for angle = (* 2 pi turns (/ (- z z0) rise))
          do (loop for step from 0 below 3
                   for a = (+ angle (* 0.22 step))
                   for x = (round (+ cx (* radius (cos a))))
                   for y = (round (+ cy (* radius (sin a))))
                   do (setf (world-cell-p world x y z) t)))))

(defun ground-height (x y &key (base 4) (relief 2.5) (scale 9.0) (seed 0.0))
  "A gentle rolling ground: two sines, never below one."
  (max 1 (floor (+ base
                   (* relief (sin (+ seed (/ x scale))) (cos (/ y (* 1.3 scale))))
                   (* 0.4 relief (sin (/ (+ x y) (* 0.6 scale))))))))

(defun lay-ground (world &key (height #'ground-height) (stock :granite))
  "Fill WORLD's whole floor in STOCK from the column heights HEIGHT gives.

Nothing here decides what grows: the ground is laid as rock and GRASS-THE-
FLATS is what puts turf on it, because whether turf catches is a property
of the finished shape and not of the function that made it."
  (let ((period (luft:world-domain-x-period (world-domain world))))
    (with-stock (stock)
      (dotimes (x period world)
        (dotimes (y period)
          (let ((top (max 0 (floor (funcall height x y)))))
            (dotimes (z top)
              (setf (world-cell-p world x y z) t))))))))

(defun column-top (world x y &optional (ceiling 72))
  "The highest solid Z of the column at X,Y, or NIL if the column is empty."
  (loop for z from ceiling downto 0
        when (world-cell-p world x y z) return z))

(defun grass-the-flats (world &key (stock :turf) (flat 1) (depth 1)
                                   (ceiling 72) (below nil))
  "Repaint the top cells of every column level enough to hold turf.

Turf catches where the ground is within FLAT cells of all four neighbours,
and nowhere else: a cliff therefore comes out as bare rock and its top as
a field, which is the whole difference between a terrain and a landscape.
BELOW, when given, is a height above which nothing grows -- a treeline, or
the top of a wall one does not want turfed.  #JNJF28"
  (let* ((period (luft:world-domain-x-period (world-domain world)))
         (tops (make-array (list period period) :initial-element nil)))
    (dotimes (x period)
      (dotimes (y period)
        (setf (aref tops x y) (column-top world x y ceiling))))
    (dotimes (x period world)
      (dotimes (y period)
        (let ((top (aref tops x y)))
          (when (and top (or (null below) (<= top below)))
            (let ((slope (loop for (dx dy) in '((1 0) (-1 0) (0 1) (0 -1))
                               for other = (aref tops (mod (+ x dx) period)
                                                 (mod (+ y dy) period))
                               maximize (if other (abs (- top other)) 99))))
              (when (<= slope flat)
                (loop for k from 0 below depth
                      do (paint-cell world x y (- top k) stock))))))))))

(defun scatter-boulders (world places &key (stock :granite) (ceiling 72))
  "Set a boulder of the given radius on the ground at each X, Y, RADIUS."
  (with-stock (stock)
    (dolist (place places world)
      (destructuring-bind (x y radius) place
        (let ((top (or (column-top world x y ceiling) 0)))
          (fill-disc world x y radius (- top radius -1) (+ top radius -1))
          (fill-disc world x y (1- radius) (+ top radius -1) (+ top radius)))))))

;;; ------------------------------------------------------------------------
;;; The pieces
;;;
;;; Each is a generic function on its name so that a sheet can ask for all
;;; of them without knowing any of them, and so that a new piece is one
;;; method and one entry in ATELIER-PIECES.

(defgeneric atelier-scene (piece)
  (:documentation "A fresh scene of the piece called PIECE."))

(defgeneric atelier-cameras (piece)
  (:documentation "Named views of the piece called PIECE, as an alist."))

(defun atelier-pieces ()
  "Every piece the atelier can build, in the order a sheet would show them."
  '(:samples :arcade :turret :viaduct :grotto :headland :holm))

;;; The samples: one small object repeated in every stock there is.  A
;;; material sheet with one material a column tells you what each is; this
;;; tells you what they are next to each other, which is the only question
;;; a world of several stocks ever asks.  The object is chosen to give each
;;; of them everything it can show: a top face and a side, an arris the
;;; light can catch, a re-entrant corner for the weathering to sit in, and
;;; a thin tongue whose edges are almost all chamfer.

(defmethod atelier-scene ((piece (eql :samples)))
  (let* ((world (make-world :horizontal-bits 6))
         (grade 4)
         (names (material-names)))
    ;; Flat ground, with a bank across the back so the picture ends in a
    ;; hillside rather than at the edge of the world.  The bank wanders,
    ;; because a contour that does not is a flight of steps.
    (lay-ground world
                :height (lambda (x y)
                          (+ grade
                             (* 0.6 (sin (/ x 17.0)) (cos (/ y 13.0)))
                             ;; A low escarpment, steep enough that the
                             ;; slope rule leaves it bare: a plain grey
                             ;; backdrop, which is what a chart wants.
                             (min 13.0
                                  (max 0.0 (* 4.5 (+ (- y 50)
                                                     (* 0.8 (sin (/ x 6.0)))
                                                     (* 0.5 (sin (/ x 2.7)))))))))
                :stock :granite)
    (grass-the-flats world :flat 1 :depth 1)
    (loop for name in names
          for index from 0
          for x = (+ 13 (* 12 (mod index 4)))
          for y = (+ 18 (* 14 (floor index 4)))
          for base = (or (column-top world x y) grade)
          do (with-stock (name)
               ;; A plinth, a block on it, a step on that.
               (fill-box world (- x 4) (+ x 4) (- y 4) (+ y 4) base base)
               (fill-box world (- x 3) (+ x 3) (- y 3) (+ y 3)
                         (+ base 1) (+ base 2))
               (fill-box world (- x 2) (+ x 2) (- y 2) (+ y 2)
                         (+ base 3) (+ base 4))
               ;; A quarter cut out of it: two re-entrant corners and a
               ;; hollow deep enough for the weathering to collect in.
               (fill-box world (- x 3) (1- x) (- y 3) (1- y)
                         (+ base 1) (+ base 4) nil)
               ;; A tongue standing out over the plinth, nearly all arris.
               (fill-box world (1- x) (1+ x) (+ y 3) (+ y 6)
                         (+ base 1) (+ base 1))
               ;; And a post, so each stock shows a tall thin thing too.
               (fill-box world (+ x 3) (+ x 3) (- y 4) (- y 4)
                         (+ base 1) (+ base 5))))
    (world-scene world)))

(defmethod atelier-cameras ((piece (eql :samples)))
  (list
   ;; High and back, so every stock stands in one frame and each can be
   ;; judged against its neighbours under one light.
   (cons :all (studio-camera 31.0 1.0 24.0 :look-x 31.0 :look-y 34.0
                             :look-z 3.0 :field-of-view 1.06))
   ;; Obliquely across the rows at eye height, where the arrises catch the
   ;; light and the near stocks can be judged against the far ones.
   (cons :row (studio-camera 8.0 3.0 11.0 :look-x 52.0 :look-y 30.0
                             :look-z 6.0 :field-of-view 0.62))
   ;; Close on two of them, for the figure rather than the tone.
   (cons :close (studio-camera 17.0 6.0 10.0 :look-x 25.0 :look-y 19.0
                               :look-z 6.0 :field-of-view 0.62))))

;;; The arcade: a wall with round-headed openings, on a plinth, under a
;;; cornice.  It is the smallest thing that is unmistakably architecture:
;;; a rhythm, a wall, and a shadow inside every opening.

(defmethod atelier-scene ((piece (eql :arcade)))
  (let* ((world (make-world :horizontal-bits 6))
         (bays '(14 32 50))
         (grade 5)
         (floor (+ grade 1))
         (springing (+ floor 6))
         (radius 6)
         (head (+ springing radius))
         (across (cons 27 28)))
    (lay-ground world :height (lambda (x y)
                                (ground-height x y :base grade :relief 1.2))
                      :stock :limestone)
    (grass-the-flats world :below (1+ grade))
    ;; The stylobate: the arcade stands on three steps, which is the whole
    ;; difference between a wall and a building.
    (with-stock (:limestone)
      (fill-box world 4 60 20 40 grade grade)
      (fill-box world 5 59 18 19 (1- grade) (1- grade))
      (fill-box world 6 58 16 17 (- grade 2) (- grade 2))
      (fill-box world 4 60 (car across) (cdr across) floor (+ head 1)))
    (dolist (bay bays)
      (carve-arch world bay floor springing radius across))
    (with-stock (:granite)
      (dolist (bay bays)
        (ring-arch world bay floor springing radius across :thickness 1)))
    ;; A string course, a corbelled cornice, and a parapet with gaps.
    (with-stock (:granite)
      (fill-box world 4 60 (1- (car across)) (1+ (cdr across))
                (+ head 2) (+ head 2)))
    (with-stock (:limestone)
      (corbel world 4 60 (car across) (cdr across) (+ head 3) 2)
      (loop for x from 2 to 62
            unless (zerop (mod x 3))
              do (setf (world-cell-p world x 24 (+ head 5)) t
                       (world-cell-p world x 31 (+ head 5)) t)))
    ;; Behind the arcade, a cloister walk paved in slate, a bench along the
    ;; back wall, and a bronze well-head on the axis of the middle bay.
    (with-stock (:slate)
      (fill-box world 8 56 30 38 grade grade))
    (with-stock (:limestone)
      (fill-box world 8 56 39 40 floor (+ floor 8)))
    (with-stock (:oak)
      (fill-box world 12 26 37 38 (1+ grade) (1+ grade))
      (fill-box world 38 52 37 38 (1+ grade) (1+ grade)))
    (with-stock (:bronze)
      (fill-disc world 32 34 2 (1+ grade) (+ grade 2))
      (fill-disc world 32 34 1 (+ grade 3) (+ grade 3)))
    ;; Two low bollards on the turf, to give the eye a near thing to hold.
    (with-stock (:granite)
      (fill-box world 22 22 11 11 (1- grade) (+ grade 1))
      (fill-box world 42 42 13 13 (1- grade) grade))
    (world-scene world)))

(defmethod atelier-cameras ((piece (eql :arcade)))
  (list
   ;; Well back and a little above: the establishing view, where the whole
   ;; building sits in its ground rather than filling the frame.
   (cons :approach (studio-camera 60.0 2.0 18.0 :look-x 24.0 :look-y 30.0
                                  :look-z 11.0 :field-of-view 0.78))
   ;; Low and three-quarters on, so the arches run away to the left and the
   ;; stylobate leads the eye in along the bottom of the frame.
   (cons :raking (studio-camera 58.0 5.0 8.5 :look-x 20.0 :look-y 28.0
                                :look-z 13.0 :field-of-view 0.80))
   ;; From inside the walk, looking out through the arcade at the light.
   (cons :walk (studio-camera 54.0 33.5 8.0 :look-x 12.0 :look-y 30.5
                              :look-z 11.0 :field-of-view 0.85))))

;;; The turret: a round tower, machicolated and crenellated, with a curtain
;;; wall running off it and a stair inside.  Everything here is a disc.

(defmethod atelier-scene ((piece (eql :turret)))
  (let* ((world (make-world :horizontal-bits 6))
         (cx 30) (cy 32)
         (grade 5)
         (base (+ grade 1))
         (top (+ grade 20)))
    (lay-ground world :height (lambda (x y)
                                (ground-height x y :base grade :relief 1.6
                                                   :scale 11.0))
                      :stock :granite)
    (grass-the-flats world :below (+ grade 3))
    ;; A battered plinth: two courses wider than the shaft.
    (with-stock (:granite)
      (fill-disc world cx cy 8 grade (+ grade 1))
      (fill-disc world cx cy 7 (+ grade 2) (+ grade 3)))
    (with-stock (:limestone)
      (fill-ring world cx cy 4 6 base top)
      ;; The machicolation: three corbelled courses carrying the parapet.
      (fill-ring world cx cy 4 7 (+ top 1) (+ top 1))
      (fill-ring world cx cy 4 8 (+ top 2) (+ top 3))
      (fill-disc world cx cy 5 (+ top 3) (+ top 3)))
    (with-stock (:granite)
      (crenellate-disc world cx cy 8 (+ top 4) :period 2 :merlon 2))
    ;; A doorway, and arrow slits up the shaft.
    (carve-arch world cy base (+ base 2) 2 (cons (- cx 7) (+ cx 7)) :axis :y)
    (loop for z from (+ base 5) below top by 5
          do (fill-box world (- cx 7) (+ cx 7) cy cy z (1+ z) nil)
             (fill-box world cx cx (- cy 7) (+ cy 7) z (1+ z) nil))
    (with-stock (:limestone)
      (spiral-stair world cx cy 4 base top :turns 3))
    ;; The curtain wall running east, ending in a gate pier.
    (with-stock (:limestone)
      (fill-box world (+ cx 6) 58 (- cy 1) (+ cy 1) base (+ base 7))
      (corbel world (+ cx 6) 58 (- cy 1) (+ cy 1) (+ base 8) 1))
    (carve-arch world 50 base (+ base 3) 2 (cons (- cy 2) (+ cy 2)) :axis :x)
    (with-stock (:granite)
      (ring-arch world 50 base (+ base 3) 2 (cons (- cy 2) (+ cy 2))
                 :axis :x :thickness 1)
      (loop for x from (+ cx 6) to 58
            unless (zerop (mod x 2))
              do (setf (world-cell-p world x (- cy 2) (+ base 9)) t
                       (world-cell-p world x (+ cy 2) (+ base 9)) t)))
    ;; A timber hoard on the wall walk, and a bronze bell in the tower head.
    (with-stock (:oak)
      (fill-box world 40 46 (- cy 3) (- cy 3) (+ base 9) (+ base 11))
      (fill-box world 40 46 (- cy 3) (+ cy 3) (+ base 11) (+ base 11)))
    (with-stock (:bronze)
      (fill-disc world cx cy 2 (+ top 1) (+ top 2)))
    ;; A bailey east of the tower, so nothing stands between the tower and
    ;; the way one comes at it: the curtain wall turns south at the gate
    ;; pier and runs back west, enclosing a yard with a hall in it.
    (with-stock (:limestone)
      (fill-box world 47 49 14 (- cy 1) base (+ base 5))
      (fill-box world 36 49 14 16 base (+ base 5))
      (crenellate world 36 49 14 (- cy 1) (+ base 6) :period 2))
    ;; The hall: stone below, timber above, slate over.
    (with-stock (:limestone)
      (fill-box world 38 46 18 28 base (+ base 3))
      (fill-box world 39 45 19 27 (+ base 1) (+ base 3) nil))
    (with-stock (:oak)
      (fill-box world 38 46 18 28 (+ base 4) (+ base 6))
      (fill-box world 39 45 19 27 (+ base 4) (+ base 6) nil)
      (fill-box world 41 43 18 18 (+ base 4) (+ base 5) nil))
    (with-stock (:slate)
      (loop for course from 0 below 4
            do (fill-box world (+ 37 course) (- 47 course) 17 29
                         (+ base 7 course) (+ base 7 course))))
    (scatter-boulders world '((16 44 2) (20 50 1) (52 44 2) (56 52 1)
                             (14 20 2) (22 30 1) (24 12 2)))
    (world-scene world)))

(defmethod atelier-cameras ((piece (eql :turret)))
  (list
   ;; Far enough back that the whole tower stands in the frame with the
   ;; ground under it, which is the only view that says how tall it is.
   (cons :approach (studio-camera 20.0 2.0 12.0 :look-x 30.5 :look-y 30.0
                                  :look-z 19.0 :field-of-view 0.92))
   ;; Along the curtain wall from the gate end, the tower closing the view.
   (cons :wall (studio-camera 61.0 14.0 15.0 :look-x 33.0 :look-y 31.0
                              :look-z 15.0 :field-of-view 0.80))
   ;; Close under the machicolation, looking up past the corbels.
   (cons :under (studio-camera 20.0 22.0 7.0 :look-x 29.0 :look-y 31.0
                               :look-z 27.0 :field-of-view 1.05))))

;;; The viaduct: piers in a gorge, arches between them, a road on top.  The
;;; piece exists to be seen from below, where the arches repeat away from
;;; the eye and the shadow under each one is a different depth.

(defmethod atelier-scene ((piece (eql :viaduct)))
  (let* ((world (make-world :horizontal-bits 6))
         (rim 22)
         (bed 2)
         (deck 24)
         (springing 14)
         (radius 5)
         (piers '(13 25 37 49))
         (arches '(19 31 43))
         (across (cons 28 33)))
    ;; A box canyon running north and south, which the viaduct crosses.
    ;; The walls fall five cells for every one they step in, so the slope
    ;; rule leaves them bare and turfs only the rims and the canyon floor.
    (lay-ground world
                :height (lambda (x y)
                          (let* ((out (abs (- x 30)))
                                 (drop (if (<= out 16)
                                           (- rim bed)
                                           (max 0 (- (- rim bed)
                                                     (* 12 (- out 16)))))))
                            (max 1 (+ (- rim drop)
                                      (* 1.6 (sin (/ x 9.0)) (cos (/ y 12.0)))
                                      (* 0.8 (sin (/ (+ x y) 7.0)))))))
                :stock :granite)
    (grass-the-flats world :flat 1 :depth 1)
    ;; The piers, each on its own footing in the canyon floor.
    (with-stock (:granite)
      (dolist (x piers)
        (fill-box world (- x 2) (+ x 2) (1- (car across)) (1+ (cdr across))
                  bed (+ bed 1))
        (fill-box world (1- x) (1+ x) (car across) (cdr across)
                  bed springing)))
    ;; The spandrel wall the arches are cut from, and the abutments that
    ;; carry it back into the canyon walls.
    (with-stock (:limestone)
      (fill-box world 8 54 (car across) (cdr across) bed (1- deck))
      (fill-box world 2 9 (car across) (cdr across) 8 (1- deck))
      (fill-box world 53 60 (car across) (cdr across) 8 (1- deck)))
    (dolist (arch arches)
      (carve-arch world arch (+ bed 1) springing radius across))
    (with-stock (:granite)
      (dolist (arch arches)
        (ring-arch world arch (+ bed 1) springing radius across :thickness 1)))
    ;; A string course under the deck, the deck, and its parapets.
    (with-stock (:granite)
      (corbel world 2 58 (car across) (cdr across) (1- deck) 1))
    (with-stock (:slate)
      (fill-box world 0 63 27 34 deck deck))
    (with-stock (:limestone)
      (loop for x from 0 to 63
            do (setf (world-cell-p world x 27 (1+ deck)) t
                     (world-cell-p world x 34 (1+ deck)) t)
            when (zerop (mod x 6))
              do (setf (world-cell-p world x 27 (+ deck 2)) t
                       (world-cell-p world x 34 (+ deck 2)) t)))
    (with-stock (:oak)
      (fill-box world 0 63 29 32 (1+ deck) (1+ deck)))
    ;; A mill on the canyon floor, its wheel against the viaduct's shadow.
    (with-stock (:granite)
      (fill-box world 20 28 40 47 bed (+ bed 1)))
    (with-stock (:oak)
      (fill-box world 20 28 40 40 (+ bed 2) (+ bed 6))
      (fill-box world 20 28 47 47 (+ bed 2) (+ bed 6))
      (fill-box world 20 20 40 47 (+ bed 2) (+ bed 6))
      (fill-box world 28 28 40 47 (+ bed 2) (+ bed 6))
      (fill-box world 23 25 40 40 (+ bed 2) (+ bed 4) nil))
    (with-stock (:slate)
      (loop for course from 0 below 4
            do (fill-box world (+ 19 course) (- 29 course) 39 48
                         (+ bed 7 course) (+ bed 7 course))))
    (with-stock (:bronze)
      (fill-disc world 18 44 3 (+ bed 2) (+ bed 2)))
    (scatter-boulders world '((36 44 2) (40 22 2) (24 20 1) (44 48 2)
                             (17 26 1)))
    (world-scene world)))

(defmethod atelier-cameras ((piece (eql :viaduct)))
  (list
   ;; From the canyon floor, square to the flank: three arches in a row,
   ;; each with a different depth of shadow inside it.
   (cons :below (studio-camera 30.0 3.0 4.0 :look-x 30.0 :look-y 30.0
                               :look-z 15.0 :field-of-view 0.98))
   ;; From the canyon rim, so the span reads against the far wall.
   (cons :rim (studio-camera 59.0 50.0 31.0 :look-x 29.0 :look-y 32.0
                             :look-z 13.0 :field-of-view 0.88))
   ;; Straight through one arch to the mill standing on the floor beyond.
   (cons :through (studio-camera 19.0 12.0 7.0 :look-x 22.0 :look-y 44.0
                                 :look-z 11.0 :field-of-view 0.90))))

;;; The grotto: a cliff with a mouth cut into it, a chamber behind, and the
;;; rock left standing as columns.  The interest is the light: the field
;;; shadow reaches into the opening and stops, and the ambient occlusion
;;; does the rest.

(defmethod atelier-scene ((piece (eql :grotto)))
  (let* ((world (make-world :horizontal-bits 6))
         (grade 4)
         (floor 5)
         (springing 12)
         (radius 7)
         (mouth 32)
         (face 30))
    (flet ((cliff-face (x)
             ;; The cliff wanders in plan: bays and buttresses, kept
             ;; straight for the ten cells either side of the mouth.
             (if (<= (abs (- x mouth)) 10)
                 face
                 (+ face (round (+ (* 4.0 (sin (/ x 8.0)))
                                   (* 2.0 (sin (/ x 3.4)))))))))
      ;; A turf apron, and behind it a granite massif whose front is one
      ;; drop of twenty cells: the slope rule leaves the face bare and
      ;; turfs only the top, which is what a cliff looks like.
      (lay-ground world
                  :height (lambda (x y)
                            (if (< y (cliff-face x))
                                (ground-height x y :base grade :relief 1.0
                                                   :scale 12.0)
                                (+ 24 (* 3 (sin (/ x 7.3)))
                                   (* 2 (cos (/ y 6.1))))))
                  :stock :granite))
    ;; The hall behind the face, cleared out of the mass, and the columns
    ;; of rock left standing in it.
    (fill-box world 22 42 (+ face 2) 48 floor (+ springing 6) nil)
    (with-stock (:granite)
      (dolist (place '((26 36) (38 36) (26 44) (38 44)))
        (fill-disc world (first place) (second place) 1 floor (+ springing 6))
        (fill-disc world (first place) (second place) 2 floor (+ floor 1))
        (fill-disc world (first place) (second place) 2
                   (+ springing 4) (+ springing 6))))
    (with-stock (:slate)
      (fill-box world 23 41 (+ face 2) 47 (1- floor) (1- floor)))
    ;; The mouth, cut through the two courses of face left standing, and
    ;; the limestone ring that says a hand made it.
    (carve-arch world mouth floor springing radius (cons face (1+ face)))
    (with-stock (:limestone)
      (ring-arch world mouth floor springing radius (cons face (1+ face))
                 :thickness 1))
    ;; The way up to the threshold.
    (with-stock (:limestone)
      (loop for step from 0 below 5
            do (fill-box world (- 28 step) (+ 36 step)
                         (- face 1 step) (- face 1 step)
                         0 (- floor step 1))))
    (grass-the-flats world :flat 1 :depth 1)
    (scatter-boulders world '((17 22 2) (21 17 1) (46 23 2) (50 18 1)
                             (13 16 1) (42 20 1) (26 21 2) (39 25 1)))
    (world-scene world)))

(defmethod atelier-cameras ((piece (eql :grotto)))
  (list
   ;; Across the apron at the mouth, far enough back that the cliff has
   ;; its whole height in the frame and the opening is small in it.
   (cons :mouth (studio-camera 32.0 3.0 9.0 :look-x 32.0 :look-y 30.0
                               :look-z 15.0 :field-of-view 1.00))
   ;; From inside the hall, looking out: the columns as silhouettes and
   ;; the arch as the only light.
   (cons :inside (studio-camera 32.0 44.0 9.0 :look-x 32.0 :look-y 20.0
                                :look-z 11.0 :field-of-view 1.00))
   ;; Obliquely along the cliff, so the massif reads as a mass and the
   ;; mouth as something small cut into it.
   (cons :cliff (studio-camera 4.0 6.0 17.0 :look-x 40.0 :look-y 31.0
                               :look-z 12.0 :field-of-view 0.80))))

;;; The headland: no building at all.  Granite cut by weather, turf caught
;;; on every ledge flat enough to hold it, and a path worn along the top --
;;; the landscape the other pieces are supposed to stand in.

(defmethod atelier-scene ((piece (eql :headland)))
  (let ((world (make-world :horizontal-bits 6)))
    (flet ((coast (x)
             ;; A wandering coastline: where the plateau ends and falls.
             (+ 34.0 (* 6.0 (sin (/ x 9.3))) (* 3.0 (sin (/ x 4.1)))))
           (roll (x y)
             (+ (* 2.4 (sin (/ x 8.5)) (cos (/ y 11.0)))
                (* 1.1 (sin (/ (+ x (* 0.6 y)) 5.0))))))
      (lay-ground world
                  :height (lambda (x y)
                            (let ((edge (coast x)))
                              (if (>= y edge)
                                  ;; The plateau, rolling gently.
                                  (+ 17 (roll x y))
                                  ;; The face: eight cells down for every
                                  ;; one out, so it is a cliff and not a
                                  ;; staircase, ending in a talus slope.
                                  (max 2 (- 17 (* 8.0 (- edge y))
                                            (- (* 0.5 (roll x y))))))))
                  :stock :granite))
    ;; Turf catches on the plateau and on any ledge flat enough to hold it,
    ;; and on nothing else.
    (grass-the-flats world :flat 1 :depth 1)
    ;; Boulders fallen to the strand, and a few standing on the top.
    (scatter-boulders world '((14 22 2) (24 18 2) (30 24 1) (41 20 2)
                             (50 25 2) (56 19 1) (20 46 2) (46 48 2)))
    ;; A cairn on the highest ground, and two standing stones beside the
    ;; path that runs along the cliff top.
    (with-stock (:slate)
      (let ((top (or (column-top world 32 46) 17)))
        (fill-disc world 32 46 3 (1+ top) (1+ top))
        (fill-disc world 32 46 2 (+ top 2) (+ top 3))
        (fill-disc world 32 46 1 (+ top 4) (+ top 5))))
    (with-stock (:granite)
      (dolist (stone '((26 41) (38 42)))
        (let ((top (or (column-top world (first stone) (second stone)) 17)))
          (fill-box world (first stone) (first stone)
                    (second stone) (second stone) top (+ top 4)))))
    ;; The path: one worn line just back from the edge, in the soil the
    ;; turf has been walked off.  A path is a repainting, not a building.
    (loop for x from 2 to 61
          for y = (round (+ 3.5 (* 6.0 (sin (/ x 9.3)))
                            (* 3.0 (sin (/ x 4.1))) 34.0))
          do (let ((top (column-top world x y)))
               (when top (paint-cell world x y top :limestone))))
    (world-scene world)))

(defmethod atelier-cameras ((piece (eql :headland)))
  (list
   ;; Along the foot of the cliff, so it recedes rather than filling the
   ;; frame flat, with the fallen boulders giving the near ground a scale.
   (cons :strand (studio-camera 6.0 10.0 5.0 :look-x 44.0 :look-y 30.0
                                :look-z 13.0 :field-of-view 0.85))
   ;; Obliquely from above the strand: the cliff line running away, which
   ;; is the only view that says the headland has a shape in plan.
   (cons :cliff (studio-camera 4.0 12.0 27.0 :look-x 42.0 :look-y 40.0
                               :look-z 10.0 :field-of-view 0.76))
   ;; On the plateau by the standing stones, the cairn closing the view
   ;; and the edge falling away past it.
   (cons :top (studio-camera 50.0 58.0 25.0 :look-x 30.0 :look-y 42.0
                             :look-z 18.0 :field-of-view 0.80))))

;;; ------------------------------------------------------------------------
;;; Sheets of the pieces

(defun render-piece-sheet (pathname piece &key (light *light*)
                                               (style :stock)
                                               (width 480) (height 320)
                                               (supersample 2))
  "Render every camera of PIECE into one column at PATHNAME."
  (let ((*light* light))
    (render-contact-sheet pathname
                          :scene (atelier-scene piece)
                          :cameras (atelier-cameras piece)
                          :columns (list (list style))
                          :width width :height height
                          :supersample supersample)))

(defmethod atelier-scene ((piece (eql :holm)))
  "A rock island with a walled town on it and a bridge out to the shore.

The other pieces each say one thing.  This one is the argument that they
compose: a landscape whose cliffs come from the slope rule, a bridge whose
arches come from the mason's vocabulary, a curtain wall with a gate and two
turrets, and a handful of roofed houses -- all in one world, each cell of it
knowing what it is cut from."
  (let* ((world (make-world :horizontal-bits 6))
         (shore 11)
         (water 2)
         (plateau 19)
         (deck 13)
         (springing 7)
         (radius 4)
         (piers '(15 25 35))
         (arches '(20 30))
         ;; The bridge runs north across the channel; ACROSS is its width.
         (across (cons 27 32)))
    (lay-ground world
                :height (lambda (x y)
                          (cond
                            ;; The shore, falling to the water at its edge.
                            ((< y 14)
                             (max water (+ shore (* 1.4 (sin (/ x 11.0)))
                                           (- (* 1.6 (max 0 (- y 9)))))))
                            ;; The holm: a plateau inside a wandering coast.
                            ((>= y 36)
                             (let* ((edge (+ 2.0 (* 3.0 (sin (/ x 9.0)))
                                             (* 1.5 (sin (/ x 3.7)))))
                                    (inland (- y 36 edge)))
                               (if (>= inland 0)
                                   (+ plateau (* 1.3 (sin (/ x 12.0))
                                                 (cos (/ y 13.0))))
                                   (max water (+ plateau (* 9.0 inland))))))
                            (t water)))
                :stock :granite)
    (grass-the-flats world :flat 1 :depth 1)
    ;; The channel bed is dark stone, not turf: there is no water in this
    ;; world, and a slate floor at the bottom of a cut reads as one.
    (let ((period (luft:world-domain-x-period (world-domain world))))
      (dotimes (x period)
        (loop for y from 10 to 44
              for top = (column-top world x y)
              when (and top (<= top (1+ water)))
                do (paint-cell world x y top :slate))))
    ;; The bridge: piers standing in the channel, arches between them, a
    ;; stone deck, a timber roadway, and parapets either side.
    (with-stock (:granite)
      (dolist (y piers)
        (fill-box world (1- (car across)) (1+ (cdr across)) (1- y) (1+ y)
                  water (1+ water))
        (fill-box world (car across) (cdr across) (1- y) (1+ y)
                  water springing)))
    (with-stock (:limestone)
      (fill-box world (car across) (cdr across) 12 38 water (1- deck))
      (fill-box world (car across) (cdr across) 6 13 4 (1- deck))
      (fill-box world (car across) (cdr across) 37 44 4 (1- deck)))
    (dolist (arch arches)
      (carve-arch world arch (1+ water) springing radius across :axis :y))
    (with-stock (:granite)
      (dolist (arch arches)
        (ring-arch world arch (1+ water) springing radius across
                   :axis :y :thickness 1))
      (corbel world (car across) (cdr across) 8 42 (1- deck) 1))
    (with-stock (:slate)
      (fill-box world 25 34 6 44 deck deck))
    (with-stock (:oak)
      (fill-box world 27 32 6 44 (1+ deck) (1+ deck)))
    (with-stock (:limestone)
      (loop for y from 6 to 44
            do (setf (world-cell-p world 25 y (1+ deck)) t
                     (world-cell-p world 34 y (1+ deck)) t)
            when (zerop (mod y 5))
              do (setf (world-cell-p world 25 y (+ deck 2)) t
                       (world-cell-p world 34 y (+ deck 2)) t)))
    ;; The ramp up off the bridge into the holm, cut through its cliff.
    (fill-box world 26 33 38 47 deck (+ plateau 5) nil)
    (with-stock (:limestone)
      (loop for step from 0 to 6
            for y from 39
            do (fill-box world 26 33 y y 0 (+ deck step))))
    ;; The curtain wall around the town, with a gate over the ramp.
    (with-stock (:limestone)
      (fill-box world 14 46 45 47 plateau (+ plateau 6))
      (fill-box world 14 16 45 61 plateau (+ plateau 6))
      (fill-box world 44 46 45 61 plateau (+ plateau 6))
      (corbel world 14 46 45 61 (+ plateau 7) 1)
      (crenellate world 13 47 44 62 (+ plateau 8) :period 2 :merlon 2))
    (carve-arch world 30 plateau (+ plateau 3) 3 (cons 45 47))
    (with-stock (:granite)
      (ring-arch world 30 plateau (+ plateau 3) 3 (cons 45 45)
                 :thickness 1))
    ;; Two turrets on the corners that face the water.
    (dolist (corner '((15 46) (45 46)))
      (destructuring-bind (cx cy) corner
        (with-stock (:granite)
          (fill-disc world cx cy 5 (1- plateau) plateau))
        (with-stock (:limestone)
          (fill-ring world cx cy 2 4 plateau (+ plateau 9))
          (fill-ring world cx cy 2 5 (+ plateau 10) (+ plateau 11))
          (fill-disc world cx cy 3 (+ plateau 11) (+ plateau 11)))
        (with-stock (:granite)
          (crenellate-disc world cx cy 5 (+ plateau 12) :period 2 :merlon 2))))
    ;; The hall: an arcaded front on the yard, slate over.
    (with-stock (:limestone)
      (fill-box world 20 40 55 60 plateau (+ plateau 5))
      (fill-box world 21 39 56 59 (1+ plateau) (+ plateau 5) nil))
    (dolist (bay '(25 30 35))
      (carve-arch world bay (1+ plateau) (+ plateau 3) 2 (cons 55 55)))
    (with-stock (:granite)
      (dolist (bay '(25 30 35))
        (ring-arch world bay (1+ plateau) (+ plateau 3) 2 (cons 55 55)
                   :thickness 1)))
    (with-stock (:slate)
      (loop for course from 0 below 4
            do (fill-box world (+ 19 course) (- 41 course) 54 61
                         (+ plateau 6 course) (+ plateau 6 course))))
    ;; Houses along the walls, stone below and timber above, tiled over.
    (dolist (house '((18 49) (26 49) (34 49) (39 55)))
      (destructuring-bind (hx hy) house
        (with-stock (:limestone)
          (fill-box world hx (+ hx 5) hy (+ hy 4) plateau (+ plateau 2))
          (fill-box world (1+ hx) (+ hx 4) (1+ hy) (+ hy 3)
                    (1+ plateau) (+ plateau 2) nil))
        (with-stock (:oak)
          (fill-box world hx (+ hx 5) hy (+ hy 4) (+ plateau 3) (+ plateau 4))
          (fill-box world (1+ hx) (+ hx 4) (1+ hy) (+ hy 3)
                    (+ plateau 3) (+ plateau 4) nil))
        (with-stock (:terracotta)
          (loop for course from 0 below 3
                do (fill-box world (+ hx -1 course) (+ hx 6 (- course))
                             (1- hy) (+ hy 5)
                             (+ plateau 5 course) (+ plateau 5 course))))))
    ;; A bronze beacon on one turret, and rocks fallen into the channel.
    (with-stock (:bronze)
      (fill-disc world 45 46 2 (+ plateau 12) (+ plateau 13)))
    (scatter-boulders world '((10 20 2) (54 22 2) (18 32 1) (48 30 1)
                             (8 40 2) (58 36 2) (38 18 1) (20 26 1)))
    (world-scene world)))

(defmethod atelier-cameras ((piece (eql :holm)))
  (list
   ;; From the shore at the bridge's foot, the roadway leading the eye
   ;; across the channel to the gate and the town on the rock above it.
   (cons :approach (studio-camera 30.0 2.0 17.0 :look-x 30.0 :look-y 46.0
                                  :look-z 21.0 :field-of-view 0.86))
   ;; From the channel bed beside the bridge, looking along its flank at
   ;; the arches, with the cliff and the wall stacked up behind.
   (cons :channel (studio-camera 11.0 19.0 5.0 :look-x 31.0 :look-y 33.0
                                 :look-z 15.0 :field-of-view 0.92))
   ;; High and oblique over the town, so the plan of the walls and the
   ;; roofs can be read at once.
   (cons :town (studio-camera 58.0 6.0 46.0 :look-x 28.0 :look-y 50.0
                              :look-z 19.0 :field-of-view 0.76))))

(defun render-view (pathname piece camera &key (light *light*) (style :stock)
                                               (width 1200) (height 750)
                                               (supersample 2))
  "Render one named CAMERA of PIECE at PATHNAME: a picture, not a sheet."
  (let ((*light* light)
        (views (atelier-cameras piece)))
    (render-contact-sheet pathname
                          :scene (atelier-scene piece)
                          :cameras (list (or (assoc camera views)
                                             (error "~S has no ~S camera; it
has ~{~S~^, ~}." piece camera (mapcar #'car views))))
                          :columns (list (list style))
                          :width width :height height
                          :supersample supersample)))

(defun render-light-sheet (pathname piece
                           &key (lights '(:morning :afternoon :evening
                                          :overcast))
                                (style :stock)
                                (width 400) (height 280) (supersample 2))
  "Render PIECE's cameras down the rows and LIGHTS across the columns."
  (render-contact-sheet pathname
                        :scene (atelier-scene piece)
                        :cameras (atelier-cameras piece)
                        :columns (loop for light in lights
                                       collect (list style '*light* light))
                        :width width :height height
                        :supersample supersample))
