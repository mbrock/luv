(in-package #:luft.render)

(defparameter *wireframe* 0.0
  "Global construction-edge strength.  The atelier toggles it between 0 and 1.")

(defparameter *render-scale* 0.75
  "Linear internal resolution of the LUFT scene before temporal upscaling.")

(defparameter *temporal-upscaling-p* t
  "Whether LUFT uses MetalFX temporal reconstruction on Metal devices.")

(defconstant +exposure-probe-width+ 32)
(defconstant +exposure-probe-height+ 16)
(defconstant +exposure-probe-buffer-count+ 3)
(defconstant +exposure-probe-byte-count+
  (* 4 +exposure-probe-width+ +exposure-probe-height+))

(defconstant +sanctuary-origin-x+ 32)
(defconstant +sanctuary-origin-y+ 24)
(defparameter *sanctuary-beacon-x* 58)
(defparameter *sanctuary-beacon-y* 54)

(defclass scene ()
  ((solid :initarg :solid :reader scene-solid)
   (material-vocabulary :initarg :material-vocabulary
                        :reader scene-material-vocabulary)
   (material-cells :initarg :material-cells :reader scene-material-cells)
   (material-program :initarg :material-program
                     :reader scene-material-program)
   (player-p :initarg :player-p :initform nil :reader scene-player-p))
  (:documentation
   "One authored solid and its vocabulary-closed material-placement field.

The solid remains LUFT's topological truth. The sparse authored field stores
only dense vocabulary offsets; semantic material objects remain at the scene
boundary rather than being allocated per cell."))

(defclass scene-builder ()
  ((domain :initarg :domain :reader scene-builder-domain)
   (origin-x :initarg :origin-x :initform 0 :reader scene-builder-origin-x)
   (origin-y :initarg :origin-y :initform 0 :reader scene-builder-origin-y)
   (cells :initform (make-hash-table :test #'eql) :reader scene-builder-cells)
   (material-vocabulary :initform (make-scene-material-vocabulary)
                        :reader scene-builder-material-vocabulary)
   (material-cells :initform (make-hash-table :test #'eql)
                   :reader scene-builder-material-cells)))

(defun make-scene-builder (&key (horizontal-bits 6) (origin-x 0) (origin-y 0))
  (make-instance 'scene-builder
                 :domain (luft:make-world-domain
                          :x-bits horizontal-bits :y-bits horizontal-bits)
                 :origin-x origin-x :origin-y origin-y))

(defun scene-builder-cell
    (builder x y z &key (solid-p t) architecture-p (material nil material-p))
  (when (<= 0 z 254)
    (let ((site (luft:make-site
                 (scene-builder-domain builder)
                 (+ x (scene-builder-origin-x builder))
                 (+ y (scene-builder-origin-y builder))
                 z luft:+cell-extent+ 1)))
      (if solid-p
          (let ((placement
                  (if material-p material
                      (if architecture-p *sanctuary-material-placement*
                          *terrain-material-placement*))))
            (check-type placement material-placement)
            (setf (gethash site (scene-builder-cells builder)) t
                  (gethash site (scene-builder-material-cells builder))
                  (domains:identity-vocabulary-offset
                   (scene-builder-material-vocabulary builder) placement)))
          (progn
            (remhash site (scene-builder-cells builder))
            (remhash site (scene-builder-material-cells builder))))))
  builder)

(defun scene-builder-box
    (builder x0 x1 y0 y1 z0 z1
     &key (solid-p t) architecture-p
       (material (if architecture-p *sanctuary-material-placement*
                     *terrain-material-placement*)))
  (loop for z from z0 to z1 do
    (loop for y from y0 to y1 do
      (loop for x from x0 to x1 do
        (scene-builder-cell builder x y z :solid-p solid-p
                                           :material material))))
  builder)

(defun scene-builder-disc
    (builder cx cy radius z0 z1
     &key (solid-p t) architecture-p
       (material (if architecture-p *sanctuary-material-placement*
                     *terrain-material-placement*)))
  (let ((limit (expt (+ radius 0.5) 2)))
    (loop for x from (- cx (ceiling radius)) to (+ cx (ceiling radius)) do
      (loop for y from (- cy (ceiling radius)) to (+ cy (ceiling radius))
            for dx = (- (+ x 0.5) (+ cx 0.5))
            for dy = (- (+ y 0.5) (+ cy 0.5))
            when (<= (+ (* dx dx) (* dy dy)) limit)
              do (loop for z from z0 to z1 do
                   (scene-builder-cell builder x y z :solid-p solid-p
                                                      :material material)))))
  builder)

(defun scene-builder-ring
    (builder cx cy inner outer z0 z1
     &key (solid-p t) architecture-p
       (material (if architecture-p *sanctuary-material-placement*
                     *terrain-material-placement*)))
  (let ((low (expt (+ inner 0.5) 2))
        (high (expt (+ outer 0.5) 2)))
    (loop for x from (- cx (ceiling outer)) to (+ cx (ceiling outer)) do
      (loop for y from (- cy (ceiling outer)) to (+ cy (ceiling outer))
            for dx = (- (+ x 0.5) (+ cx 0.5))
            for dy = (- (+ y 0.5) (+ cy 0.5))
            for distance = (+ (* dx dx) (* dy dy))
            when (and (< low distance) (<= distance high))
              do (loop for z from z0 to z1 do
                   (scene-builder-cell builder x y z :solid-p solid-p
                                                      :material material)))))
  builder)

(defun arch-rise (offset radius)
  (let ((square (- (* radius radius) (* offset offset))))
    (if (plusp square) (round (sqrt square)) 0)))

(defun scene-builder-carve-arch
    (builder centre floor springing radius across &key (axis :x))
  (destructuring-bind (near . far) across
    (loop for offset from (- radius) to radius
          for rise = (arch-rise offset radius)
          when (plusp rise) do
            (loop for z from floor below (+ springing rise) do
              (loop for other from near to far do
                (if (eq axis :x)
                    (scene-builder-cell builder (+ centre offset) other z
                                        :solid-p nil)
                    (scene-builder-cell builder other (+ centre offset) z
                                        :solid-p nil))))))
  builder)

(defun scene-builder-corbel (builder x0 x1 y0 y1 z courses)
  (loop for course from 0 below courses
        for out = (1+ course)
        do (scene-builder-box builder (- x0 out) (+ x1 out)
                              (- y0 out) (+ y1 out)
                              (+ z course) (+ z course)
                              :architecture-p t))
  builder)

(defun scene-builder-crenellate (builder x0 x1 y0 y1 z)
  (loop for x from x0 to x1 do
    (loop for y from y0 to y1
          when (and (or (= x x0) (= x x1) (= y y0) (= y y1))
                    (zerop (mod (+ x y) 2)))
            do (scene-builder-cell builder x y z :architecture-p t)
               (scene-builder-cell builder x y (1+ z) :architecture-p t)))
  builder)

(defun scene-builder-staircase
    (builder x0 x1 y0 step-count base-top
     &key (boundary :open))
  "Author an ascending masonry stair and its optional support boundary.

BOUNDARY is a deliberately small modeling vocabulary.  :OPEN emits only the
treads, :BORDER adds a one-cell stone strip level with each tread, and
:LOW-WALL raises that strip one course above it.  The extra cells are ordinary
authored solid and material input; the bevel mesher receives no special-case
stair topology. #WSEK3C"
  (check-type boundary (member :open :border :low-wall))
  (let ((boundary-rise
          (ecase boundary
            (:open nil)
            (:border 0)
            (:low-wall 1))))
    (loop for step below step-count
          for y from y0
          for top = (+ base-top step)
          do (scene-builder-box builder x0 x1 y y 0 top
                                :architecture-p t)
             (when boundary-rise
               (scene-builder-box builder (1- x0) (1- x0) y y
                                   0 (+ top boundary-rise)
                                   :architecture-p t)
               (scene-builder-box builder (1+ x1) (1+ x1) y y
                                   0 (+ top boundary-rise)
                                   :architecture-p t))))
  builder)

(defun finish-scene-builder (builder &key player-p)
  (let* ((cells (scene-builder-cells builder))
         (chain-builder
           (luft:make-chain-builder (scene-builder-domain builder)
                                    :initial-capacity (hash-table-count cells))))
    (maphash (lambda (site present-p)
               (declare (ignore present-p))
               (luft:chain-builder-add-site chain-builder site))
             cells)
    (make-instance 'scene
                   :solid (luft:finish-chain-builder chain-builder)
                   :player-p player-p
                   :material-vocabulary
                   (scene-builder-material-vocabulary builder)
                   :material-program
                   (make-material-program
                    (scene-builder-material-vocabulary builder))
                   :material-cells (scene-builder-material-cells builder))))

(defun make-manifold-spike-scene ()
  "Three isolated singular-star fixtures for the manifold-sheet spike.

The plots exercise an edge-touching pair, a corner-touching pair, and the
four-sheet parity star.  Nothing else in the scene can hide their junctions."
  (let ((builder (make-scene-builder :horizontal-bits 6)))
    (labels ((place-star (mask centre-x)
               (dotimes (sample 8)
                 (when (logbitp sample mask)
                   (scene-builder-cell
                    builder
                    (+ centre-x (if (logbitp 0 sample) 0 -1))
                    (+ 10 (if (logbitp 1 sample) 0 -1))
                    (+ 6 (if (logbitp 2 sample) 0 -1)))))))
      (place-star #x06 10)
      (place-star #x18 16)
      (place-star #x69 22))
    (finish-scene-builder builder)))

(defconstant +sanctuary-plateau-height+ 19)

(defun mountain-sanctuary-terrain-height (x y)
  "The authored local-coordinate height of the sanctuary's mountain world."
  (let ((shore 11) (water 2) (plateau +sanctuary-plateau-height+))
    (floor
     (cond
       ((< y 14)
        (max water
             (+ shore
                (* 1.4 (sin (/ x 11.0)))
                (* 1.1 (sin (/ (+ x (* 0.75 y)) 9.0)))
                (- (* 1.6 (max 0 (- y 9))))
                ;; A long diagonal shoulder lifts the far eastern approach
                ;; without changing the bridge landing.
                (* 0.10 (max 0 (- (- x (* 0.9 y)) 58))))))
       ((>= y 36)
        (let* ((edge (+ 2.0 (* 3.0 (sin (/ x 9.0)))
                           (* 1.5 (sin (/ x 3.7)))))
               (inland (- y 36 edge)))
          (if (>= inland 0)
              (let* ((ridge
                       (min 11.0
                            (* 0.22 (max 0 (- (+ y (* 0.55 x)) 88)))))
                     (ravine
                       (max 0.0
                            (- 4.5
                               (* 1.35
                                  (abs (- y (+ 67 (* 0.34 x))))))))
                     (rolling
                       (* 1.3 (sin (/ x 12.0)) (cos (/ y 13.0)))))
                (+ plateau rolling ridge (- ravine)))
              (max water (+ plateau (* 9.0 inland))))))
       (t water)))))

(defun mountain-sanctuary-terrain-x-bounds (y)
  "Return the authored inclusive terrain span at local Y."
  (when (<= -15 y 81)
    (values
     (max -18 (+ -17 (round (* 1.8 (sin (/ y 6.0))))))
     (min 82 (- 81 (round (* 2.2 (cos (/ y 8.0))))))
     t)))

(defun scene-builder-mountain-border-wall (builder)
  "Guard the elevated authored rim with a limestone parapet.

The continuous two-course body exceeds the player's one-cell step.  Every
other column rises into a third course, making the finite scenery legible as
an intentional battlement rather than the accidental edge of a voxel field."
  (labels ((wall-column (x y)
             (let ((height (mountain-sanctuary-terrain-height x y)))
               (when (>= height +sanctuary-plateau-height+)
                 (scene-builder-box builder x x y y height (1+ height)
                                    :architecture-p t)
                 (when (evenp (+ x y))
                   (scene-builder-cell builder x y (+ height 2)
                                       :architecture-p t))))))
    ;; The side contours follow the terrain's authored west/east banks.
    (loop for y from -15 to 81 do
      (multiple-value-bind (west east present-p)
          (mountain-sanctuary-terrain-x-bounds y)
        (when present-p
          (wall-column west y)
          (wall-column east y))))
    ;; Close the elevated northern rim between those side contours.
    (multiple-value-bind (west east present-p)
        (mountain-sanctuary-terrain-x-bounds 81)
      (when present-p
        (loop for x from west to east do (wall-column x 81)))))
  builder)

(defun make-mountain-sanctuary-scene
    (&key (beacon-placement *beacon-material-placement*)
      (stair-boundary :low-wall))
  "A broad Lonely-Mountains world carrying a bridge and walled sanctuary.

This is the old Holm's architectural sentence with its material menagerie
removed: rolling approaches, a channel and rising high rock; a diagonal
processional way; a two-arched stone bridge; a gate, curtain wall, paired
turrets, an arcaded hall and a remote ridge beacon."
  (let* ((builder (make-scene-builder
                   :horizontal-bits 7
                   :origin-x +sanctuary-origin-x+
                   :origin-y +sanctuary-origin-y+))
         (water 2) (plateau +sanctuary-plateau-height+) (deck 13)
         (springing 7) (radius 4) (across (cons 27 32)))
    ;; Keep the packed world bounded away from its toroidal seam, while the
    ;; visible camera sees terrain continuing beyond every frame edge.
    (loop for y from -15 to 81 do
      (multiple-value-bind (west east present-p)
          (mountain-sanctuary-terrain-x-bounds y)
        (when present-p
          (loop for x from west to east do
            (loop for z below
                  (max 1 (mountain-sanctuary-terrain-height x y)) do
              (scene-builder-cell builder x y z))))))
    (scene-builder-mountain-border-wall builder)
    ;; A diagonal, gently climbing processional way makes the bridge belong
    ;; to the low country instead of beginning at the edge of the model.
    (loop for step from 0 below 20
          for x = (+ 6 step)
          for y = (+ -14 step)
          for top = (max (mountain-sanctuary-terrain-height x y)
                         (min deck (+ 10 (floor step 5))))
          do (scene-builder-box builder x (+ x 4) y (1+ y) 0 top
                                :architecture-p t)
             (when (zerop (mod step 4))
               (scene-builder-cell builder x y (1+ top)
                                   :architecture-p t)
               (scene-builder-cell builder (+ x 4) (1+ y) (1+ top)
                                   :architecture-p t)))
    (dolist (y '(15 25 35))
      (scene-builder-box builder 26 33 (1- y) (1+ y) water springing
                         :architecture-p t))
    (scene-builder-box builder 27 32 12 38 water (1- deck)
                       :architecture-p t)
    (dolist (arch '(20 30))
      (scene-builder-carve-arch builder arch (1+ water) springing radius
                                across :axis :y))
    (scene-builder-corbel builder 27 32 8 42 (1- deck) 1)
    (scene-builder-box builder 25 34 6 44 deck deck :architecture-p t)
    (check-type stair-boundary (member :open :border :low-wall))
    ;; The old bridge rail overlaps the first six stair courses.  Preserve it
    ;; for the open historical scene, but let authored stair boundaries own
    ;; that stretch so the comparison changes one modeling decision at a time.
    (loop for y from 6 to (if (eq stair-boundary :open) 44 38) do
      (scene-builder-cell builder 25 y (1+ deck) :architecture-p t)
      (scene-builder-cell builder 34 y (1+ deck) :architecture-p t)
      (when (zerop (mod y 5))
        (scene-builder-cell builder 25 y (+ deck 2) :architecture-p t)
        (scene-builder-cell builder 34 y (+ deck 2) :architecture-p t)))
    (scene-builder-box builder 26 33 38 47 deck (+ plateau 5) :solid-p nil)
    (scene-builder-staircase builder 26 33 39 7 deck
                             :boundary stair-boundary)
    ;; Bed the sanctuary into the uneven ridge before raising its walls.  The
    ;; two-course podium is shallow enough to disappear into the higher turf,
    ;; but where the mountain falls away it remains a continuous load path
    ;; instead of leaving the fixed-height curtain visibly suspended in air.
    ;; Its wider tower shoes also give the round keeps a deliberate transition
    ;; into the rectilinear retaining work.
    (scene-builder-box builder 13 47 44 48 (- plateau 2) (1- plateau)
                       :architecture-p t)
    (scene-builder-box builder 13 17 49 62 (- plateau 2) (1- plateau)
                       :architecture-p t)
    (scene-builder-box builder 43 47 49 62 (- plateau 2) (1- plateau)
                       :architecture-p t)
    (scene-builder-box builder 18 42 59 62 (- plateau 2) (1- plateau)
                       :architecture-p t)
    (dolist (corner '((15 46) (45 46)))
      (destructuring-bind (cx cy) corner
        (scene-builder-disc builder cx cy 6 (- plateau 2) (1- plateau)
                            :architecture-p t)))
    (scene-builder-box builder 14 46 45 47 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-box builder 14 16 45 61 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-box builder 44 46 45 61 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-corbel builder 14 46 45 61 (+ plateau 7) 1)
    (scene-builder-crenellate builder 13 47 44 62 (+ plateau 8))
    (scene-builder-carve-arch builder 30 plateau (+ plateau 3) 3
                              (cons 45 47))
    (dolist (corner '((15 46) (45 46)))
      (destructuring-bind (cx cy) corner
        (scene-builder-disc builder cx cy 5 (1- plateau) plateau
                            :architecture-p t)
        (scene-builder-ring builder cx cy 2 4 plateau (+ plateau 9)
                            :architecture-p t)
        (scene-builder-ring builder cx cy 2 5 (+ plateau 10) (+ plateau 11)
                            :architecture-p t)
        (scene-builder-disc builder cx cy 3 (+ plateau 11) (+ plateau 11)
                            :architecture-p t)))
    (scene-builder-box builder 20 40 55 60 plateau (+ plateau 5)
                       :architecture-p t)
    (scene-builder-box builder 21 39 56 59 (1+ plateau) (+ plateau 5)
                       :solid-p nil)
    (dolist (bay '(25 30 35))
      (scene-builder-carve-arch builder bay (1+ plateau) (+ plateau 3) 2
                                (cons 55 55)))
      ;; A hollow beacon on the eastern ridge gives the enlarged world a
      ;; distant inhabited landmark and a second architectural scale.
      (let* ((beacon-x *sanctuary-beacon-x*)
             (beacon-y *sanctuary-beacon-y*)
             (base (mountain-sanctuary-terrain-height beacon-x beacon-y)))
        (scene-builder-disc builder beacon-x beacon-y 3 base (1+ base)
                            :material beacon-placement)
        (scene-builder-ring builder beacon-x beacon-y 1 2 (+ base 2)
                            (+ base 6) :material beacon-placement)
        (scene-builder-ring builder beacon-x beacon-y 1 3 (+ base 7)
                            (+ base 8) :material beacon-placement)
        (scene-builder-disc builder beacon-x beacon-y 1 (+ base 8)
                            (+ base 8) :material beacon-placement))
      ;; An old eight-pillar sun court occupies the open lowland beside the
      ;; processional way.  Its diagonal stones echo the landscape's oblique
      ;; ridges without competing with the sanctuary's larger silhouette.
      (let* ((court-x 55)
             (court-y 4)
             (base (mountain-sanctuary-terrain-height court-x court-y)))
        (scene-builder-disc builder court-x court-y 5 base base
                            :architecture-p t)
        (dolist (offset '((-4 0) (-3 -3) (0 -4) (3 -3)
                          (4 0) (3 3) (0 4) (-3 3)))
          (destructuring-bind (dx dy) offset
            (scene-builder-box builder (+ court-x dx) (+ court-x dx)
                               (+ court-y dy) (+ court-y dy)
                               (1+ base) (+ base 4) :architecture-p t)
            (scene-builder-cell builder (+ court-x dx) (+ court-y dy)
                                (+ base 5) :architecture-p t))))
      (finish-scene-builder builder :player-p t)))

(defun make-traveler-study-scene ()
  "A bare limestone dais under the traveler, clear from every direction.

The sanctuary is the scene he belongs in, but it is also a scene in which
half the useful camera angles look through a parapet.  This fixture keeps
his exact world position and his exact deck height and removes everything
else, so a turnaround can be shot around him without moving him."
  (let ((builder (make-scene-builder :horizontal-bits 7)))
    (scene-builder-box builder 56 67 40 58 11 13 :architecture-p t)
    (finish-scene-builder builder :player-p t)))

(defun make-material-bevel-transition-study-scene ()
  "Build the five-cell medial T-junction regression. #WSEK3C

Two ascending architectural columns meet the corner of a two-cell terrain
column.  Widths one, two, and four occur in one tiny surface; the width-four
medial collapse leaves exactly one long-edge/short-edge T-junction for the
site-local contraction pass to resolve."
  (let ((builder (make-scene-builder :horizontal-bits 4)))
    (scene-builder-box builder 6 6 4 4 2 3)
    (scene-builder-cell builder 5 4 2 :architecture-p t)
    (scene-builder-box builder 5 5 5 5 2 3 :architecture-p t)
    (finish-scene-builder builder)))

(defun make-miter-study-scene ()
  "Build the wall-side stepped mountain used to judge mixed miters. #Z5NDTA

The two L-shaped terraces retain five-, six-, and seven-cell vertex stars in
one architectural context.  Their back edges meet a continuous wall so the
same view also retains the truncated wall miter preserved by #DJK8HW."
  (let ((builder (make-scene-builder :horizontal-bits 5)))
    ;; Broad plinth and continuous back wall.
    (scene-builder-box builder 2 14 2 8 0 1 :architecture-p t)
    (scene-builder-box builder 2 14 8 9 0 7 :architecture-p t)
    ;; One isolated terrain cell makes the ordinary boulder-on-ground contact
    ;; inspectable beside the architectural mixed-miter cases.
    (scene-builder-cell builder 13 4 2)
    ;; The lower L supplies the outie, straight six-cell run, innie, and the
    ;; first wall termination.  The upper L repeats them without isolation.
    (scene-builder-box builder 4 11 5 7 2 2 :architecture-p t)
    (scene-builder-box builder 4 8 3 4 2 2 :architecture-p t)
    (scene-builder-box builder 5 10 6 7 3 3 :architecture-p t)
    (scene-builder-box builder 5 7 4 5 3 3 :architecture-p t)
    (finish-scene-builder builder)))

(defun face-solid-cell (solid face)
  "Return the occupied cell incident to boundary FACE and which side it is on."
  (let* ((domain (luft:chain-domain solid))
         (extent (luft:site-extent face))
         (axis (cond ((= extent luft:+xy-face-extent+) :z)
                     ((= extent luft:+xz-face-extent+) :y)
                     (t :x)))
         (x (luft:site-x face))
         (y (luft:site-y face))
         (z (luft:site-z face))
         (back-x (if (eq axis :x) (1- x) x))
         (back-y (if (eq axis :y) (1- y) y))
         (back-z (if (eq axis :z) (1- z) z)))
    (if (= 1 (luft:chain-cell-occupancy-bit solid x y z))
        (values (luft:make-site domain x y z luft:+cell-extent+ 1)
                axis :forward)
        (values (luft:make-site domain back-x back-y back-z
                                luft:+cell-extent+ 1)
                axis :backward))))

(defun scene-foundation-cell-p (scene cell)
  "Whether architectural CELL is immediately borne by non-architectural earth."
  (let ((z (luft:site-z cell)))
    (when (plusp z)
      (let ((below
              (luft:make-site
               (luft:chain-domain (scene-solid scene))
               (luft:site-x cell) (luft:site-y cell) (1- z)
               luft:+cell-extent+ 1)))
        (and (= 1 (luft:chain-cell-occupancy-bit
                   (scene-solid scene)
                   (luft:site-x below) (luft:site-y below) (luft:site-z below)))
             (multiple-value-bind (offset present-p)
                 (gethash below (scene-material-cells scene))
               (and present-p
                    (logtest
                     +material-placement-earth-flag+
                     (aref
                      (material-program-placement-flags
                       (scene-material-program scene))
                      offset)))))))))

(defun scene-material-placement-at (scene cell)
  "Return the authored placement at occupied CELL in SCENE."
  (multiple-value-bind (offset present-p)
      (gethash cell (scene-material-cells scene))
    (unless present-p
      (error "Occupied scene cell ~S has no authored material placement." cell))
    (domains:identity-vocabulary-member
     (scene-material-vocabulary scene) offset)))

(defun scene-face-reading (scene face)
  "Derive FACE's semantic reading without allocating a per-face object."
  (multiple-value-bind (cell axis side)
      (face-solid-cell (scene-solid scene) face)
    (let ((placement (scene-material-placement-at scene cell)))
      (material-face-reading (material-placement-kind placement)
                             placement scene cell axis side))))

(defun make-scene-face-stock-function (scene)
  "Capture SCENE's dense material tables for repeated face classification."
  (let* ((solid (scene-solid scene))
         (domain (luft:chain-domain solid))
         (material-cells (scene-material-cells scene))
         (program (scene-material-program scene))
         (placement-flags
           (the (simple-array (unsigned-byte 8) (*))
                (material-program-placement-flags program)))
         (face-stocks
           (the (simple-array (unsigned-byte 16) (*))
                (material-program-placement-face-stocks program))))
    (labels ((foundation-p (cell)
               (let ((z (luft:site-z cell)))
                 (when (plusp z)
                   (let ((below
                           (luft:make-site
                            domain
                            (luft:site-x cell) (luft:site-y cell) (1- z)
                            luft:+cell-extent+ 1)))
                     (and (= 1 (luft:chain-cell-occupancy-bit
                                solid
                                (luft:site-x below)
                                (luft:site-y below)
                                (luft:site-z below)))
                          (multiple-value-bind (offset present-p)
                              (gethash below material-cells)
                            (and present-p
                                 (logtest
                                  +material-placement-earth-flag+
                                  (aref placement-flags offset))))))))))
      (lambda (face)
        (declare (optimize (speed 3) (safety 1)))
        (multiple-value-bind (cell axis side)
            (face-solid-cell solid face)
          (multiple-value-bind (placement-offset present-p)
              (gethash cell material-cells)
            (unless present-p
              (error "Occupied scene cell ~S has no authored material placement."
                     cell))
            (let* ((flags (aref placement-flags placement-offset))
                   (face-index
                     (if (and
                          (logtest +material-placement-architecture-flag+
                                   flags)
                          (foundation-p cell))
                         6
                         (+ (* (ecase axis (:x 0) (:y 1) (:z 2)) 2)
                            (if (eq side :forward) 1 0)))))
              (aref face-stocks
                    (+ (* placement-offset
                          +material-placement-face-stride+)
                       face-index)))))))))

(defun scene-face-stock (scene face)
  "The current packed assembly offset for FACE in SCENE."
  (funcall (make-scene-face-stock-function scene) face))

(defun scene-chamfer-stock (stocks &optional material-program)
  "Resolve one whole chamfer from its incident face STOCKS.

The paper palette's terrain top is grass, terrain side is soil, and terrain
underside is dark soil.  A unanimous closure continues that face material;
a mixed terrain chamfer exposes soil.  Stone--terrain chamfers retain the
deepest incident substrate, so the shader can weather a turf line differently
from an exposed or buried foundation without adding per-site material objects."
  (if material-program
      (compiled-material-chamfer-stock material-program stocks)
      (surface-assembly-offset
       (closure-surface-assembly (mapcar #'surface-assembly-at stocks)))))

(defun default-face-stock (face)
  (mod (+ (luft:site-x face) (* 2 (luft:site-y face))
          (* 3 (luft:site-z face)) (luft:site-extent face))
       4))

(defun make-render-mesh
    (source &key stock-function chamfer-stock-function
                 (bevel-width luft:+mesh-bevel-width+))
  "Classify SOURCE into the face, edge, and vertex template-instance ABI."
  (let* ((scene (and (typep source 'scene) source))
         (solid (if scene (scene-solid source) source))
         (material-program (and scene (scene-material-program scene)))
         (stock-function (or stock-function
                             (and scene (make-scene-face-stock-function scene))
                             #'default-face-stock))
         (chamfer-stock-function
           (or chamfer-stock-function
               (if scene
                   (make-compiled-material-chamfer-stock-function
                    material-program)
                   (lambda (stocks) (first stocks))))))
    (check-type solid luft:chain)
    (zone (:luft/rematerialize :value (luft:chain-count solid))
      (luft:make-surface-mesh solid :stock-function stock-function
                                   :chamfer-stock-function
                                   chamfer-stock-function
                                   :bevel-width bevel-width))))

(defun make-material-bevel-mesh (scene profile)
  "Build one watertight mesh with a semantic material width at each site.

The ordinary width-one mesher supplies one exact topology witness.  PROFILE is
compiled after that build into a dense stock-to-material-mask lane.  Each
canonical lattice vertex ORs the masks of all incident stocks: terrain-only,
architecture-only, and mixed stars select the profile's terrain, architecture,
and contact widths.  The unchanged witness triangles form the transitions.

The second value is a five-entry vector counting sites at widths zero through
four; it is diagnostic evidence and does not become retained mesh state."
  (check-type scene scene)
  (check-type profile material-bevel-profile)
  ;; The witness build also interns every authored material assembly reached by
  ;; the chamfer stock function.  Freeze the dense policy only afterward.
  (let ((witness (make-render-mesh scene :bevel-width 1)))
    (multiple-value-bind (stock-masks site-widths)
        (compile-material-bevel-site-policy profile)
      (luft:vary-surface-mesh-bevel-widths
       witness
       (lambda (x y z stocks)
         (declare (ignore x y z))
         (let ((site-mask 0))
           (dolist (stock stocks)
             (unless (< stock (length stock-masks))
               (error "Mesh stock ~D is outside the compiled material bevel policy of ~D entries."
                      stock (length stock-masks)))
             (setf site-mask (logior site-mask (aref stock-masks stock))))
           (unless (<= 1 site-mask 3)
             (error "Incident mesh stocks ~S compiled to invalid material bevel mask ~D."
                    stocks site-mask))
           (aref site-widths site-mask)))))))

(defun make-material-bevel-meshes (scene profile)
  "Return the single site-local material bevel mesh in renderer slot zero."
  (list (cons 0 (make-material-bevel-mesh scene profile))))

(defparameter *gallery*
  ;; Each entry is one isolated complex, named by the star configuration it
  ;; is there to exhibit.  Cells are offsets from the entry's plot origin.
  '((:one-cell
     ((0 0 0)))
    (:face-pair
     ((0 0 0) (1 0 0)))
    (:edge-pair
     ((0 0 0) (1 1 0)))
    (:corner-pair
     ((0 0 0) (1 1 1)))
    (:l-tromino
     ((0 0 0) (1 0 0) (0 1 0)))
    (:square
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)))
    (:stair
     ((0 0 0) (1 0 0) (1 0 1)))
    (:concave-vertex
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1) (0 1 1)))
    (:six-of-eight
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1)))
    (:full-block
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1) (0 1 1) (1 1 1)))
    (:tower
     ((0 0 0) (0 0 1) (0 0 2)))
    (:cross
     ((1 1 0) (0 1 0) (2 1 0) (1 0 0) (1 2 0) (1 1 1))))
  "Small unconnected complexes, one per interesting occupancy star.")

(defparameter *gallery-columns* 4)
(defparameter *gallery-pitch* 6)

(defun gallery-plot-origin (index)
  "Return the lattice origin of gallery entry INDEX, laid out on a grid."
  (multiple-value-bind (row column) (floor index *gallery-columns*)
    (values (+ 2 (* column *gallery-pitch*))
            (+ 2 (* row *gallery-pitch*))
            1)))

(defun make-gallery-solid (&key (entries *gallery*))
  "Build one chain holding every gallery complex on its own plot.

Nothing here touches anything else, so each patch shown is entirely the
consequence of its own occupancy star and can be read on its own."
  (let* ((domain (luft:make-world-domain :x-bits 6 :y-bits 6))
         (builder (luft:make-chain-builder domain :initial-capacity 128))
         (seen (make-hash-table :test #'eql)))
    (loop for entry in entries
          for index from 0
          do (multiple-value-bind (ox oy oz) (gallery-plot-origin index)
               (loop for (dx dy dz) in (second entry)
                     for site = (luft:make-site domain (+ ox dx) (+ oy dy)
                                                (+ oz dz)
                                                luft:+cell-extent+ 1)
                     unless (gethash site seen)
                       do (setf (gethash site seen) t)
                          (luft:chain-builder-add-site builder site))))
    (luft:finish-chain-builder builder)))

(defun gallery-plot-report (&key (entries *gallery*))
  "Print where each gallery complex sits, so a capture can be aimed at one."
  (loop for entry in entries
        for index from 0
        do (multiple-value-bind (x y z) (gallery-plot-origin index)
             (format t "~&~2D ~24A origin ~D,~D,~D~%"
                     index (first entry) x y z)))
  (values))

(defun make-demo-solid ()
  "Make a compact stair-and-bridge solid with convex and concave stars."
  (let* ((domain (luft:make-world-domain :x-bits 5 :y-bits 5))
         (builder (luft:make-chain-builder domain :initial-capacity 96))
         (seen (make-hash-table :test #'eql)))
    (labels ((cell (x y z)
               (let ((site
                       (luft:make-site domain x y z luft:+cell-extent+ 1)))
                 (unless (gethash site seen)
                   (setf (gethash site seen) t)
                   (luft:chain-builder-add-site builder site))))
             (box (x0 x1 y0 y1 z0 z1)
               (loop for z from z0 to z1 do
                 (loop for y from y0 to y1 do
                   (loop for x from x0 to x1 do (cell x y z))))))
      (box 4 11 4 11 1 1)
      (box 5 10 5 10 2 2)
      (box 6 6 6 6 3 5)
      (box 9 9 6 6 3 5)
      (box 6 6 9 9 3 5)
      (box 9 9 9 9 3 5)
      (box 6 9 6 6 5 5)
      (box 6 9 9 9 5 5)
      (box 7 8 7 8 3 3))
    (luft:finish-chain-builder builder)))

(defconstant +render-template-vertex-count+ 6)

(defstruct (render-population
             (:constructor %make-render-population
                 (template-words instance-words triangle-instance-count
                  quad-instance-count))
             (:copier nil))
  "One compact fixed-arity draw population for a surface-mesh cohort."
  (template-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (instance-words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (triangle-instance-count 0 :type (integer 0 *) :read-only t)
  (quad-instance-count 0 :type (integer 0 *) :read-only t))

(defstruct (resident-population
             (:constructor %make-resident-population
                 (population instance-buffer template-buffer bind-group
                  shadow-bind-group))
             (:copier nil))
  "One chunk's CPU population and independently retained GPU realization."
  (population nil :type render-population :read-only t)
  (instance-buffer nil :read-only t)
  (template-buffer nil :read-only t)
  (bind-group nil :read-only t)
  (shadow-bind-group nil :read-only t))

(defstruct (prepared-render-mesh
             (:constructor %make-prepared-render-mesh (mesh population))
             (:copier nil))
  "Worker-transferable CPU realization of one semantic surface mesh."
  (mesh nil :type luft:surface-mesh :read-only t)
  (population nil :type render-population :read-only t))

(zdefun (prepare-render-mesh :zone :luft/prepare-population) (mesh)
  "Canonicalize one MESH before it crosses to the renderer owner."
  (check-type mesh luft:surface-mesh)
  (%make-prepared-render-mesh mesh (make-render-population (list mesh))))

(defun make-render-population (meshes)
  "Canonicalize and concatenate MESHES into two fixed-arity instance runs.

Every source template contains either one triangle or one quad. Templates are
interned by their exact packed vertices, padded to a fixed six-vertex stride,
and selected by the low sixteen bits of each remapped instance record. The
returned population stores triangle instances first and quad instances second,
so the complete surface needs at most two direct instanced draws."
  (let ((template-index (make-hash-table :test #'equalp))
        (template-words
          (make-array 256 :element-type '(unsigned-byte 32)
                          :adjustable t :fill-pointer 0))
        (triangle-words
          (make-array 256 :element-type '(unsigned-byte 32)
                          :adjustable t :fill-pointer 0))
        (quad-words
          (make-array 256 :element-type '(unsigned-byte 32)
                          :adjustable t :fill-pointer 0))
        (assembly-count
          (length (domains:identity-vocabulary-members
                   *surface-assembly-vocabulary*)))
        (triangle-count 0)
        (quad-count 0))
    (labels ((intern-template (mesh template-id)
               (let* ((ranges (luft:surface-mesh-template-ranges mesh))
                      (vertices (luft:surface-mesh-template-vertex-words mesh))
                      (range-offset (* 2 template-id))
                      (vertex-start (aref ranges range-offset))
                      (vertex-count (aref ranges (1+ range-offset)))
                      (word-start
                        (* vertex-start
                           luft:+mesh-template-vertex-word-count+))
                      (word-count
                        (* vertex-count
                           luft:+mesh-template-vertex-word-count+))
                      (key (make-array (1+ word-count)
                                       :element-type '(unsigned-byte 32))))
                 (unless (member vertex-count '(3 6))
                   (error "LUFT render template ~D has unsupported arity ~D."
                          template-id vertex-count))
                 (setf (aref key 0) vertex-count)
                 (replace key vertices :start1 1 :start2 word-start
                                       :end2 (+ word-start word-count))
                 (multiple-value-bind (global-id present-p)
                     (gethash key template-index)
                   (unless present-p
                     (setf global-id
                           (/ (fill-pointer template-words)
                              (* +render-template-vertex-count+
                                 luft:+mesh-template-vertex-word-count+)))
                     (unless (typep global-id '(unsigned-byte 16))
                       (error "LUFT render template vocabulary exceeds 16 bits."))
                     (loop for index from 1 below (length key)
                           do (vector-push-extend (aref key index)
                                                  template-words))
                     (loop repeat (- (* +render-template-vertex-count+
                                        luft:+mesh-template-vertex-word-count+)
                                     word-count)
                           do (vector-push-extend 0 template-words))
                     (setf (gethash key template-index) global-id))
                   (values global-id vertex-count))))
             (append-stream (words global-ids vertex-counts)
               (loop for offset from 0 below (length words)
                       by luft:+mesh-instance-word-count+
                     for packed = (aref words (+ offset 3))
                     for local-id = (ldb (byte 16 0) packed)
                     for assembly-id =
                       (ldb (byte luft:+mesh-instance-stock-bit-count+ 16)
                            packed)
                     for global-id = (aref global-ids local-id)
                     for vertex-count = (aref vertex-counts local-id)
                     for destination = (if (= vertex-count 3)
                                           triangle-words
                                           quad-words)
                     do (unless (< assembly-id assembly-count)
                          (error "LUFT surface assembly ~D is outside the resident vocabulary of ~D entries."
                                 assembly-id assembly-count))
                        (loop for word-offset below 3
                              do (vector-push-extend
                                  (aref words (+ offset word-offset))
                                  destination))
                        (vector-push-extend
                         (logior global-id (logand packed #xffff0000))
                         destination)
                        (if (= vertex-count 3)
                            (incf triangle-count)
                            (incf quad-count))))
             (append-mesh (mesh)
               ;; Mesh-local template IDs are dense. Resolve each one exactly
               ;; once, then the large instance streams become a linear copy.
               (let* ((template-count
                        (/ (length (luft:surface-mesh-template-ranges mesh)) 2))
                      (global-ids
                        (make-array template-count
                                    :element-type '(unsigned-byte 16)))
                      (vertex-counts
                        (make-array template-count
                                    :element-type '(unsigned-byte 8))))
                 (dotimes (local-id template-count)
                   (multiple-value-bind (global-id vertex-count)
                       (intern-template mesh local-id)
                     (setf (aref global-ids local-id) global-id
                           (aref vertex-counts local-id) vertex-count)))
                 (append-stream
                  (luft:surface-mesh-face-instance-words mesh)
                  global-ids vertex-counts)
                 (append-stream
                  (luft:surface-mesh-band-instance-words mesh)
                  global-ids vertex-counts)
                 (append-stream
                  (luft:surface-mesh-fan-instance-words mesh)
                  global-ids vertex-counts))))
      (dolist (mesh meshes)
        (append-mesh mesh)))
    (%make-render-population
     (coerce template-words '(simple-array (unsigned-byte 32) (*)))
     (concatenate '(simple-array (unsigned-byte 32) (*))
                  triangle-words quad-words)
     triangle-count quad-count)))

(defstruct (mesh-slot (:constructor %make-mesh-slot) (:copier nil))
  "One mesh's semantic residency and optional construction-overlay resources."
  (mesh nil)
  (resident nil)
  (lattice-point-buffer nil)
  (lattice-point-count 0)
  (lattice-point-group nil))

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   ;; Chunk key (or any EQL key) to resident MESH-SLOT; SLOT-ORDER is the
   ;; sorted key list frames draw in.
   (mesh-slots :initform (make-hash-table :test #'eql)
               :reader renderer-mesh-slots)
   (slot-order :initform nil :accessor renderer-slot-order)
   (camera-buffer :initarg :camera-buffer :accessor renderer-camera-buffer)
   (material-buffer :initarg :material-buffer :accessor renderer-material-buffer)
   (layout :initarg :layout :accessor renderer-layout)
   (vertex-module :initarg :vertex-module :accessor renderer-vertex-module)
   (fragment-module :initarg :fragment-module :accessor renderer-fragment-module)
   (pipeline :initarg :pipeline :accessor renderer-pipeline)
   (shadow-texture :initarg :shadow-texture :accessor renderer-shadow-texture)
   (shadow-view :initarg :shadow-view :accessor renderer-shadow-view)
   (shadow-sampler :initarg :shadow-sampler :accessor renderer-shadow-sampler)
   (shadow-layout :initarg :shadow-layout :accessor renderer-shadow-layout)
   (shadow-vertex-module :initarg :shadow-vertex-module
                         :accessor renderer-shadow-vertex-module)
   (shadow-pipeline :initarg :shadow-pipeline
                    :accessor renderer-shadow-pipeline)
   (player-sdf-layout :initarg :player-sdf-layout
                      :accessor renderer-player-sdf-layout)
   (player-sdf-bind-group :initarg :player-sdf-bind-group
                          :accessor renderer-player-sdf-bind-group)
   (player-sdf-vertex-module :initarg :player-sdf-vertex-module
                             :accessor renderer-player-sdf-vertex-module)
   (player-sdf-fragment-module :initarg :player-sdf-fragment-module
                               :accessor renderer-player-sdf-fragment-module)
   (player-sdf-pipeline :initarg :player-sdf-pipeline
                        :accessor renderer-player-sdf-pipeline)
   (lattice-point-layout :initarg :lattice-point-layout
                         :accessor renderer-lattice-point-layout)
   (lattice-point-vertex-module :initarg :lattice-point-vertex-module
                                :accessor renderer-lattice-point-vertex-module)
   (lattice-point-fragment-module :initarg :lattice-point-fragment-module
                                  :accessor renderer-lattice-point-fragment-module)
   (lattice-point-pipeline :initarg :lattice-point-pipeline
                           :accessor renderer-lattice-point-pipeline)
   (sky-layout :initform nil :accessor renderer-sky-layout)
   (sky-bind-group :initform nil :accessor renderer-sky-bind-group)
   (sky-fragment-module :initform nil :accessor renderer-sky-fragment-module)
   (sky-pipeline :initform nil :accessor renderer-sky-pipeline)
   (color-format :initarg :color-format :reader renderer-color-format)
   (temporal-p :initarg :temporal-p :reader renderer-temporal-p)
   (depth-texture :initform nil :accessor renderer-depth-texture)
   (depth-view :initform nil :accessor renderer-depth-view)
   (scene-texture :initform nil :accessor renderer-scene-texture)
   (scene-view :initform nil :accessor renderer-scene-view)
   (motion-texture :initform nil :accessor renderer-motion-texture)
   (motion-view :initform nil :accessor renderer-motion-view)
   (resolved-texture :initform nil :accessor renderer-resolved-texture)
   (resolved-view :initform nil :accessor renderer-resolved-view)
   (temporal-scaler :initform nil :accessor renderer-temporal-scaler)
   (present-layout :initform nil :accessor renderer-present-layout)
   (present-bind-group :initform nil :accessor renderer-present-bind-group)
   (present-vertex-module :initform nil
                          :accessor renderer-present-vertex-module)
   (present-fragment-module :initform nil
                            :accessor renderer-present-fragment-module)
   (present-pipeline :initform nil :accessor renderer-present-pipeline)
   (exposure-probe-layout :initform nil
                          :accessor renderer-exposure-probe-layout)
   (exposure-probe-bind-group :initform nil
                              :accessor renderer-exposure-probe-bind-group)
   (exposure-probe-texture :initform nil
                           :accessor renderer-exposure-probe-texture)
   (exposure-probe-view :initform nil
                        :accessor renderer-exposure-probe-view)
   (exposure-probe-fragment-module
    :initform nil :accessor renderer-exposure-probe-fragment-module)
   (exposure-probe-pipeline :initform nil
                            :accessor renderer-exposure-probe-pipeline)
   (exposure-probe-buffers :initform #()
                           :accessor renderer-exposure-probe-buffers)
   (exposure-probe-submitted :initform (make-array 0 :element-type 'bit)
                             :accessor renderer-exposure-probe-submitted)
   (exposure-probe-frames :initform #()
                          :accessor renderer-exposure-probe-frames)
   (exposure :initform 1.0f0 :accessor renderer-exposure)
   (sampler :initform nil :accessor renderer-sampler)
   (extent :initform nil :accessor renderer-extent)
   (render-extent :initform nil :accessor renderer-render-extent)
   (frame-index :initform 0 :accessor renderer-frame-index)
   (previous-view :initform nil :accessor renderer-previous-view)
   (history-valid-p :initform nil :accessor renderer-history-valid-p)
   (history-used-p :initform nil :accessor renderer-history-used-p)))

(defun metal-temporal-device-p (device)
  #+darwin (and *temporal-upscaling-p* (typep device 'metal-gpu-device))
  #-darwin (declare (ignore device))
  #-darwin nil)

(defun destroy-renderer-targets (renderer)
  (dolist (resource
            (list (renderer-present-bind-group renderer)
                  (renderer-exposure-probe-bind-group renderer)
                  (renderer-temporal-scaler renderer)
                  (renderer-resolved-view renderer)
                  (renderer-resolved-texture renderer)
                  (renderer-motion-view renderer)
                  (renderer-motion-texture renderer)
                  (renderer-scene-view renderer)
                  (renderer-scene-texture renderer)
                  (renderer-depth-view renderer)
                  (renderer-depth-texture renderer)))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-present-bind-group renderer) nil
        (renderer-exposure-probe-bind-group renderer) nil
        (renderer-temporal-scaler renderer) nil
        (renderer-resolved-view renderer) nil
        (renderer-resolved-texture renderer) nil
        (renderer-motion-view renderer) nil
        (renderer-motion-texture renderer) nil
        (renderer-scene-view renderer) nil
        (renderer-scene-texture renderer) nil
        (renderer-depth-view renderer) nil
        (renderer-depth-texture renderer) nil))

(defun render-scale-extent (extent)
  "Return the even-sized internal render extent for output EXTENT."
  (mapcar (lambda (dimension)
            (max 2 (* 2 (round (* 0.5 *render-scale* dimension)))))
          extent))

(defun create-frame-targets (renderer extent)
  (let* ((device (renderer-device renderer))
         (temporal-p (renderer-temporal-p renderer))
         (render-extent (render-scale-extent extent))
         (scaler
           (and temporal-p
                (create device
                        (make-temporal-scaler-descriptor
                         :label "luft MetalFX temporal scaler"
                         :input-size render-extent :output-size extent))))
         (usage (lambda (base extra)
                  (remove-duplicates (append base extra))))
         (depth
           (create device
                   (make-texture-descriptor
                    :label "luft temporal depth" :size render-extent :dimensions :2d
                    :format :depth32-float
                    :usage (funcall usage '(:render-attachment :texture-binding)
                                    (and scaler
                                         (gpu-temporal-scaler-depth-usage
                                          scaler))))))
         (depth-view
           (create device (make-texture-view-descriptor :texture depth)))
         (scene
           (create device
                   (make-texture-descriptor
                    :label "luft HDR color" :size render-extent :dimensions :2d
                    :format :rgba16-float
                    :usage
                    (funcall usage '(:render-attachment :texture-binding)
                             (and scaler
                                  (gpu-temporal-scaler-color-usage scaler))))))
         (scene-view
           (create device (make-texture-view-descriptor :texture scene)))
         (motion
           (and temporal-p
                (create device
                        (make-texture-descriptor
                         :label "luft temporal motion" :size render-extent
                         :dimensions :2d :format :rg16-float
                         :usage
                         (funcall usage '(:render-attachment)
                                  (gpu-temporal-scaler-motion-usage scaler))))))
         (motion-view
           (and motion
                (create device
                        (make-texture-view-descriptor :texture motion))))
         (resolved
           (and temporal-p
                (create device
                        (make-texture-descriptor
                         :label "luft temporal resolve" :size extent
                         :dimensions :2d :format :rgba16-float
                         :usage
                         (funcall usage '(:texture-binding)
                                  (gpu-temporal-scaler-output-usage scaler))))))
         (resolved-view
           (and resolved
                (create device
                        (make-texture-view-descriptor :texture resolved))))
         (present-source-view (or resolved-view scene-view))
         (present-group
           (create
            device
            (make-bind-group-descriptor
             :label "luft HDR presentation"
             :layout (renderer-present-layout renderer)
             :entries
             `((:binding 0 :resource ,present-source-view)
               (:binding 1 :resource ,(renderer-sampler renderer))
               (:binding 2 :resource ,depth-view)
               (:binding 3
                :resource ,(renderer-camera-buffer renderer))))))
         (exposure-probe-group
           (create
            device
            (make-bind-group-descriptor
             :label "luft exposure probe source"
             :layout (renderer-exposure-probe-layout renderer)
             :entries
             `((:binding 0 :resource ,present-source-view)
               (:binding 1 :resource ,(renderer-sampler renderer)))))))
    (setf (renderer-temporal-scaler renderer) scaler
          (renderer-depth-texture renderer) depth
          (renderer-depth-view renderer) depth-view
          (renderer-scene-texture renderer) scene
          (renderer-scene-view renderer) scene-view
          (renderer-motion-texture renderer) motion
          (renderer-motion-view renderer) motion-view
          (renderer-resolved-texture renderer) resolved
          (renderer-resolved-view renderer) resolved-view
          (renderer-present-bind-group renderer) present-group
          (renderer-exposure-probe-bind-group renderer) exposure-probe-group
          (renderer-extent renderer) (copy-list extent)
          (renderer-render-extent renderer) (copy-list render-extent)
          (renderer-previous-view renderer) nil
          (renderer-history-valid-p renderer) nil
          (renderer-history-used-p renderer) nil))
  renderer)

(defun ensure-renderer-extent (renderer extent)
  (unless (equal extent (renderer-extent renderer))
    (destroy-renderer-targets renderer)
    (create-frame-targets renderer extent))
  renderer)

(zdefun (mesh-lattice-point-words :zone :luft/prepare-overlay) (mesh)
  "LUFT vertex sites, mesh vertices, and eighth-step boundary-edge samples."
  (let ((points (make-hash-table :test #'eql))
        (result (make-array 64 :element-type '(unsigned-byte 32)
                              :adjustable t :fill-pointer 0))
        (templates (luft:surface-mesh-template-vertex-words mesh))
        (ranges (luft:surface-mesh-template-ranges mesh)))
    (labels ((pack-point (x y z)
               ;; World coordinates are non-negative and comfortably below
               ;; twenty bits at the eighth-cell scale. One fixnum is a
               ;; cons-free hash key for the diagnostic point vocabulary.
               (unless (and (typep x '(unsigned-byte 20))
                            (typep y '(unsigned-byte 20))
                            (typep z '(unsigned-byte 20)))
                 (error "LUFT lattice point (~D ~D ~D) exceeds packed range."
                        x y z))
               (logior x (ash y 20) (ash z 40)))
             (remember (x y z marker-kind)
               (let ((key (pack-point x y z)))
                 (setf (gethash key points)
                       (max marker-kind (gethash key points 0)))))
             (template-coordinate (base vertex axis)
               (+ (* luft:+mesh-cell-size+ base)
                  (- (aref templates
                           (+ (* vertex luft:+mesh-template-vertex-word-count+)
                              axis))
                     luft:+mesh-template-coordinate-bias+)))
             (sample-axis-edge (ax ay az bx by bz)
               (cond
                 ((and (= ay by) (= az bz) (/= ax bx))
                  (loop for x from (min ax bx) to (max ax bx)
                        do (remember x ay az 0)))
                 ((and (= ax bx) (= az bz) (/= ay by))
                  (loop for y from (min ay by) to (max ay by)
                        do (remember ax y az 0)))
                 ((and (= ax bx) (= ay by) (/= az bz))
                  (loop for z from (min az bz) to (max az bz)
                        do (remember ax ay z 0)))))
             (visit-stream (words fan-p)
               (loop for instance-offset from 0 below (length words) by 4
                     for base-x = (aref words instance-offset)
                     for base-y = (aref words (+ instance-offset 1))
                     for base-z = (aref words (+ instance-offset 2))
                     for packed = (aref words (+ instance-offset 3))
                     for template-id = (ldb (byte 16 0) packed)
                     for vertex-start = (aref ranges (* 2 template-id))
                     for vertex-count = (aref ranges (1+ (* 2 template-id)))
                     do (when fan-p
                          (remember (* luft:+mesh-cell-size+ base-x)
                                    (* luft:+mesh-cell-size+ base-y)
                                    (* luft:+mesh-cell-size+ base-z) 2))
                        (loop for vertex from vertex-start
                                below (+ vertex-start vertex-count)
                              do (remember
                                  (template-coordinate base-x vertex 0)
                                  (template-coordinate base-y vertex 1)
                                  (template-coordinate base-z vertex 2) 1))
                        (loop for vertex from vertex-start
                                below (+ vertex-start vertex-count) by 3
                              for attributes =
                                (aref templates
                                      (+ (* vertex 4) 3))
                              for edge-mask = (ldb (byte 3 10) attributes)
                              for ax = (template-coordinate base-x vertex 0)
                              for ay = (template-coordinate base-y vertex 1)
                              for az = (template-coordinate base-z vertex 2)
                              for bx = (template-coordinate base-x (1+ vertex) 0)
                              for by = (template-coordinate base-y (1+ vertex) 1)
                              for bz = (template-coordinate base-z (1+ vertex) 2)
                              for cx = (template-coordinate base-x (+ vertex 2) 0)
                              for cy = (template-coordinate base-y (+ vertex 2) 1)
                              for cz = (template-coordinate base-z (+ vertex 2) 2)
                              when (logbitp 0 edge-mask)
                                do (sample-axis-edge bx by bz cx cy cz)
                              when (logbitp 1 edge-mask)
                                do (sample-axis-edge ax ay az cx cy cz)
                              when (logbitp 2 edge-mask)
                                do (sample-axis-edge ax ay az bx by bz)))))
      (visit-stream (luft:surface-mesh-face-instance-words mesh) nil)
      (visit-stream (luft:surface-mesh-band-instance-words mesh) nil)
      (visit-stream (luft:surface-mesh-fan-instance-words mesh) t))
    (maphash
     (lambda (point marker-kind)
       (vector-push-extend (ldb (byte 20 0) point) result)
       (vector-push-extend (ldb (byte 20 20) point) result)
       (vector-push-extend (ldb (byte 20 40) point) result)
       (vector-push-extend marker-kind result))
     points)
    (coerce result '(simple-array (unsigned-byte 32) (*)))))

(defun %destroy-mesh-slot (slot)
  (%destroy-resident-population (mesh-slot-resident slot))
  (dolist (resource (list (mesh-slot-lattice-point-group slot)
                          (mesh-slot-lattice-point-buffer slot)))
    (when resource (ignore-errors (destroy resource))))
  (values))

(defun mesh-slot-prepared-mesh (slot)
  "Borrow SLOT's immutable CPU realization for renderer reconstruction."
  (%make-prepared-render-mesh
   (mesh-slot-mesh slot)
   (resident-population-population (mesh-slot-resident slot))))

(defun %make-renderer-mesh-slot (renderer mesh-or-prepared)
  "Upload one independently retained chunk slot.

MESH-OR-PREPARED may carry worker-built dense population arrays. Construction
overlay data is deliberately absent until construction mode asks for it."
  (let* ((prepared
           (if (typep mesh-or-prepared 'prepared-render-mesh)
               mesh-or-prepared
               (prepare-render-mesh mesh-or-prepared)))
         (mesh (prepared-render-mesh-mesh prepared))
         (slot (%make-mesh-slot :mesh mesh))
         (completed-p nil))
    (unwind-protect
         (progn
           (setf (mesh-slot-resident slot)
                 (%upload-render-population
                  renderer (prepared-render-mesh-population prepared)))
           (setf completed-p t)
           slot)
      (unless completed-p
        (%destroy-mesh-slot slot)))))

(defun ensure-mesh-slot-lattice-points (renderer slot)
  "Create SLOT's diagnostic overlay on first use, never during normal streaming."
  (unless (mesh-slot-lattice-point-buffer slot)
    (let* ((device (renderer-device renderer))
           (camera-buffer (renderer-camera-buffer renderer))
           (lattice-point-words
             (mesh-lattice-point-words (mesh-slot-mesh slot)))
           (lattice-point-count (/ (length lattice-point-words) 4))
           (completed-p nil))
      (flet ((stream-buffer (label words)
               (let ((buffer (create device
                                     (make-buffer-descriptor
                                      :label label
                                      :size (max 16 (* 4 (length words)))
                                      :usage '(:storage :copy-dst)))))
                 (when (plusp (length words))
                   (write-buffer buffer words))
                 buffer)))
        (unwind-protect
             (progn
               (setf (mesh-slot-lattice-point-count slot) lattice-point-count
                     (mesh-slot-lattice-point-buffer slot)
                     (stream-buffer "luft unique eighth-cell lattice points"
                                    lattice-point-words))
               (setf (mesh-slot-lattice-point-group slot)
                     (create device
                             (make-bind-group-descriptor
                              :label "luft eighth-cell lattice points"
                              :layout (renderer-lattice-point-layout renderer)
                              :entries
                              `((:binding 0
                                 :resource ,(mesh-slot-lattice-point-buffer
                                             slot))
                                (:binding 1 :resource ,camera-buffer)))))
               (setf completed-p t))
          (unless completed-p
            (dolist (resource (list (mesh-slot-lattice-point-group slot)
                                    (mesh-slot-lattice-point-buffer slot)))
              (when resource (ignore-errors (destroy resource))))
            (setf (mesh-slot-lattice-point-group slot) nil
                  (mesh-slot-lattice-point-buffer slot) nil
                  (mesh-slot-lattice-point-count slot) 0))))))
  slot)

(defun %destroy-resident-population (resident)
  (when resident
    (dolist (resource (list (resident-population-bind-group resident)
                            (resident-population-shadow-bind-group resident)
                            (resident-population-template-buffer resident)
                            (resident-population-instance-buffer resident)))
      (when resource (ignore-errors (destroy resource)))))
  (values))

(zdefun (%upload-render-population :zone :luft/upload-slot)
    (renderer population)
  "Build and upload one candidate population without changing RENDERER."
  (let* ((device (renderer-device renderer))
         (instance-words (render-population-instance-words population))
         (template-words (render-population-template-words population))
         instance-buffer template-buffer bind-group
         shadow-bind-group
         (completed-p nil))
    (flet ((stream-buffer (label words)
             (let ((buffer
                     (create device
                             (make-buffer-descriptor
                              :label label
                              :size (max 16 (* 4 (length words)))
                              :usage '(:storage :copy-dst)))))
               (when (plusp (length words))
                 (write-buffer buffer words))
               buffer)))
      (unwind-protect
           (progn
             (setf instance-buffer
                   (stream-buffer "luft resident site instances" instance-words)
                   template-buffer
                   (stream-buffer "luft canonical site templates" template-words)
                   bind-group
                   (create device
                           (make-bind-group-descriptor
                            :label "luft resident site population"
                            :layout (renderer-layout renderer)
                            :entries
                            `((:binding 0 :resource ,instance-buffer)
                              (:binding 1 :resource ,template-buffer)
                              (:binding 2
                               :resource ,(renderer-camera-buffer renderer))
                              (:binding 3
                               :resource ,(renderer-material-buffer renderer))
                              (:binding 4
                               :resource ,(renderer-shadow-view renderer))
                              (:binding 5
                               :resource ,(renderer-shadow-sampler renderer)))))
                   shadow-bind-group
                   (create device
                           (make-bind-group-descriptor
                            :label "luft resident shadow population"
                            :layout (renderer-shadow-layout renderer)
                            :entries
                            `((:binding 0 :resource ,instance-buffer)
                              (:binding 1 :resource ,template-buffer)
                              (:binding 2
                               :resource ,(renderer-camera-buffer renderer))))))
             (let ((resident
                     (%make-resident-population
                      population instance-buffer template-buffer bind-group
                      shadow-bind-group)))
               (setf completed-p t)
               resident))
        (unless completed-p
          (dolist (resource
                    (list shadow-bind-group bind-group template-buffer
                          instance-buffer))
            (when resource (ignore-errors (destroy resource)))))))))

(defun %refresh-renderer-slot-order (renderer)
  (let ((keys '()))
    (loop for key being the hash-keys of (renderer-mesh-slots renderer)
          do (push key keys))
    (setf (renderer-slot-order renderer) (sort keys #'<))))

(defun renderer-set-mesh (renderer key mesh)
  "Make MESH resident under KEY, replacing any previous resident mesh."
  (cdar (renderer-set-meshes renderer (list (cons key mesh)))))

(defun renderer-set-meshes (renderer meshes)
  "Transactionally replace the keyed MESHES as one visible residency cohort.

MESHES is an alist of key to surface mesh. Every GPU slot is created before
the renderer's table changes; a failed upload therefore leaves the installed
cohort untouched. No frame can interleave with the owner-thread publication."
  (renderer-update-meshes renderer meshes nil))

(zdefun (renderer-update-meshes :zone :luft/publish-residency)
    (renderer meshes removed-keys)
  "Transactionally replace MESHES and remove REMOVED-KEYS as one cohort."
  (let ((candidates nil)
        (retired nil)
        (installed-p nil))
    (unwind-protect
         (progn
           (dolist (entry meshes)
             (push (cons (car entry)
                         (%make-renderer-mesh-slot renderer (cdr entry)))
                   candidates))
           (setf candidates (nreverse candidates))
           (dolist (entry candidates)
             (let ((old (gethash (car entry) (renderer-mesh-slots renderer))))
               (setf (gethash (car entry) (renderer-mesh-slots renderer))
                     (cdr entry))
               (when old (push old retired))))
           (dolist (key removed-keys)
             (unless (assoc key candidates :test #'eql)
               (let ((old (gethash key (renderer-mesh-slots renderer))))
                 (when old
                   (push old retired)
                   (remhash key (renderer-mesh-slots renderer))))))
           (%refresh-renderer-slot-order renderer)
           (setf installed-p t)
           (dolist (slot retired) (%destroy-mesh-slot slot))
           candidates)
      (unless installed-p
        (dolist (entry candidates) (%destroy-mesh-slot (cdr entry)))))))

(defun renderer-remove-mesh (renderer key)
  (renderer-update-meshes renderer nil (list key))
  (values))

(defun renderer-clear-meshes (renderer)
  (renderer-update-meshes renderer nil (copy-list (renderer-slot-order renderer)))
  (values))

(defun make-renderer (device color-format extent)
  "Create the shared LUFT pipeline state; meshes arrive via RENDERER-SET-MESH."
  (let* ((temporal-p (metal-temporal-device-p device))
         (target-formats (if temporal-p
                             '(:rgba16-float :rg16-float)
                             '(:rgba16-float)))
         camera-buffer material-buffer
         layout
         vertex-module fragment-module pipeline
         shadow-texture shadow-view shadow-sampler shadow-layout
         shadow-vertex-module shadow-pipeline
         player-sdf-layout player-sdf-bind-group player-sdf-vertex-module
         player-sdf-fragment-module player-sdf-pipeline
         lattice-point-layout lattice-point-vertex-module
         lattice-point-fragment-module lattice-point-pipeline
         present-layout present-bind-group present-vertex-module
         present-fragment-module present-pipeline sampler
         sky-layout sky-bind-group sky-fragment-module sky-pipeline
         exposure-probe-layout exposure-probe-bind-group
         exposure-probe-texture exposure-probe-view
         exposure-probe-fragment-module exposure-probe-pipeline
         exposure-probe-buffers
         renderer
         (completed-p nil))
    (unwind-protect
         (progn
           (setf camera-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft frame state"
                          :size 432 :usage '(:uniform :copy-dst)))
                 material-buffer
                 (let ((words (surface-assembly-descriptor-words)))
                   (let ((buffer
                           (create device
                                   (make-buffer-descriptor
                                    :label "luft surface assembly descriptors"
                                    :size (max 16 (* 4 (length words)))
                                    :usage '(:storage :copy-dst)))))
                     (when (plusp (length words))
                       (write-buffer buffer words))
                     buffer))
                 shadow-texture
                 (create device
                         (make-texture-descriptor
                          :label "luft sun shadow depth"
                          :size (list +shadow-map-size+ +shadow-map-size+)
                          :dimensions :2d :format :depth32-float
                          :usage '(:render-attachment :texture-binding)))
                 shadow-view
                 (create device
                         (make-texture-view-descriptor :texture shadow-texture))
                 shadow-sampler
                 (create device
                         (make-sampler-descriptor
                          :label "luft soft shadow comparison sampler"
                          :mag-filter :linear :min-filter :linear
                          :mipmap-filter :nearest :compare :less-or-equal)))
           (setf layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft mesh layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer)
                                     (:binding 3 :type :storage-buffer)
                                     (:binding 4 :type :texture)
                                     (:binding 5 :type :sampler))))
                 shadow-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft shadow layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer))))
                 vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft mesh vertex"
                          :language :mathematical
                          :code (shaders:mesh-vertex-specification)))
                 fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft mesh fragment" :language :mathematical
                          :code (shaders:mesh-fragment-specification)))
                 pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft site stream pipeline" :layout layout
                          :vertex `(:module ,vertex-module)
                          :fragment `(:module ,fragment-module
                                      :targets
                                      ,(mapcar (lambda (format)
                                                 `(:format ,format))
                                               target-formats))
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less)))
                 shadow-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft shadow vertex"
                          :language :mathematical
                          :code (shaders:shadow-vertex-specification)))
                 shadow-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft sun shadow pipeline"
                          :layout shadow-layout
                          :vertex `(:module ,shadow-vertex-module)
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less))))
           (setf player-sdf-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft player sdf layout"
                          :entries '((:binding 0 :type :uniform-buffer)
                                     (:binding 1 :type :texture)
                                     (:binding 2 :type :sampler))))
                 player-sdf-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft player sdf vertex"
                          :language :mathematical
                          :code (shaders:player-sdf-vertex-specification)))
                 player-sdf-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft player sdf fragment"
                          :language :mathematical
                          :code (shaders:player-sdf-fragment-specification)))
                 player-sdf-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft walking player sdf pipeline"
                          :layout player-sdf-layout
                          :vertex `(:module ,player-sdf-vertex-module)
                          :fragment
                          `(:module ,player-sdf-fragment-module
                            :targets
                            ,(loop for format in target-formats
                                   for first = t then nil
                                   collect `(:format ,format
                                             ,@(when first
                                                 '(:blend :premultiplied-alpha)))))
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled nil
                            :depth-compare :less)))
                 player-sdf-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft walking player sdf"
                          :layout player-sdf-layout
                          :entries
                          `((:binding 0 :resource ,camera-buffer)
                            (:binding 1 :resource ,shadow-view)
                            (:binding 2 :resource ,shadow-sampler)))))
           (setf lattice-point-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft lattice point layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :uniform-buffer))))
                 lattice-point-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft lattice point vertex"
                          :language :mathematical
                          :code (shaders:lattice-point-vertex-specification)))
                 lattice-point-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft lattice point fragment"
                          :language :mathematical
                          :code (shaders:lattice-point-fragment-specification)))
                 lattice-point-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft eighth-cell lattice point pipeline"
                          :layout lattice-point-layout
                          :vertex `(:module ,lattice-point-vertex-module)
                          :fragment
                          `(:module ,lattice-point-fragment-module
                            :targets
                            ,(loop for format in target-formats
                                   for first = t then nil
                                   collect `(:format ,format
                                             ,@(when first
                                                 '(:blend :premultiplied-alpha)))))
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled nil
                            :depth-compare :less))))
           (setf sampler
                 (create device
                         (make-sampler-descriptor
                          :label "luft presentation sampler"
                          :mag-filter :linear :min-filter :linear))
                 sky-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft HDR sky layout"
                          :entries '((:binding 0 :type :uniform-buffer))))
                 sky-bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft HDR sky"
                          :layout sky-layout
                          :entries `((:binding 0 :resource ,camera-buffer))))
                 exposure-probe-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft exposure probe layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :sampler))))
                 exposure-probe-texture
                 (create device
                         (make-texture-descriptor
                          :label "luft exposure log luminance"
                          :size (list +exposure-probe-width+
                                      +exposure-probe-height+)
                          :dimensions :2d :format :rgba8-unorm
                          :usage '(:render-attachment :copy-src)))
                 exposure-probe-view
                 (create device
                         (make-texture-view-descriptor
                          :texture exposure-probe-texture))
                 exposure-probe-buffers
                 (coerce
                  (loop repeat +exposure-probe-buffer-count+
                        collect
                        (create device
                                (make-buffer-descriptor
                                 :label "luft exposure readback"
                                 :size +exposure-probe-byte-count+
                                 :usage '(:copy-dst))))
                  'vector))
           (setf present-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft presentation layout"
                          :entries '((:binding 0 :type :texture)
                                     (:binding 1 :type :sampler)
                                     (:binding 2 :type :texture)
                                     (:binding 3 :type :uniform-buffer))))
                 present-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft presentation vertex"
                          :language :mathematical
                          :code (shaders:present-vertex-specification)))
                 present-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft presentation fragment"
                          :language :mathematical
                          :code (shaders:present-fragment-specification)))
                 present-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft HDR presentation pipeline"
                          :layout present-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,present-fragment-module
                                      :targets ((:format ,color-format)))
                          :primitive '(:topology :triangle-list)))
                 sky-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft HDR sky fragment"
                          :language :mathematical
                          :code (if temporal-p
                                    (shaders:sky-temporal-fragment-specification)
                                    (shaders:sky-fragment-specification))))
                 sky-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft HDR sky pipeline"
                          :layout sky-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,sky-fragment-module
                                      :targets
                                      ,(mapcar (lambda (format)
                                                 `(:format ,format))
                                               target-formats))
                          :primitive '(:topology :triangle-list)
                          ;; The sky is drawn inside the scene pass, whose
                          ;; depth attachment geometry subsequently owns.
                          ;; Match that pass without touching its depth.
                          :depth-stencil
                          '(:format :depth32-float
                            :depth-write-enabled nil
                            :depth-compare :always)))
                 exposure-probe-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft exposure probe fragment"
                          :language :mathematical
                          :code
                          (shaders:exposure-probe-fragment-specification)))
                 exposure-probe-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft exposure probe pipeline"
                          :layout exposure-probe-layout
                          :vertex `(:module ,present-vertex-module)
                          :fragment `(:module ,exposure-probe-fragment-module
                                      :targets ((:format :rgba8-unorm)))
                          :primitive '(:topology :triangle-list))))
           (setf renderer
                 (make-instance 'renderer
                                :device device
                                :color-format color-format
                                :temporal-p temporal-p
                                :camera-buffer camera-buffer
                                :material-buffer material-buffer
                                :layout layout
                                :vertex-module vertex-module
                                :fragment-module fragment-module
                                :pipeline pipeline
                                :shadow-texture shadow-texture
                                :shadow-view shadow-view
                                :shadow-sampler shadow-sampler
                                :shadow-layout shadow-layout
                                :shadow-vertex-module shadow-vertex-module
                                :shadow-pipeline shadow-pipeline
                                :player-sdf-layout player-sdf-layout
                                :player-sdf-bind-group player-sdf-bind-group
                                :player-sdf-vertex-module player-sdf-vertex-module
                                :player-sdf-fragment-module
                                player-sdf-fragment-module
                                :player-sdf-pipeline player-sdf-pipeline
                                :lattice-point-layout lattice-point-layout
                                :lattice-point-vertex-module
                                lattice-point-vertex-module
                                :lattice-point-fragment-module
                                lattice-point-fragment-module
                                :lattice-point-pipeline lattice-point-pipeline))
           (setf (renderer-present-layout renderer) present-layout
                 (renderer-sampler renderer) sampler
                 (renderer-present-vertex-module renderer)
                 present-vertex-module
                 (renderer-present-fragment-module renderer)
                 present-fragment-module
                 (renderer-present-pipeline renderer) present-pipeline)
           (setf (renderer-sky-layout renderer) sky-layout
                 (renderer-sky-bind-group renderer) sky-bind-group
                 (renderer-sky-fragment-module renderer) sky-fragment-module
                 (renderer-sky-pipeline renderer) sky-pipeline
                 (renderer-exposure-probe-layout renderer)
                 exposure-probe-layout
                 (renderer-exposure-probe-texture renderer)
                 exposure-probe-texture
                 (renderer-exposure-probe-view renderer) exposure-probe-view
                 (renderer-exposure-probe-fragment-module renderer)
                 exposure-probe-fragment-module
                 (renderer-exposure-probe-pipeline renderer)
                 exposure-probe-pipeline
                 (renderer-exposure-probe-buffers renderer)
                 exposure-probe-buffers
                 (renderer-exposure-probe-submitted renderer)
                 (make-array +exposure-probe-buffer-count+
                             :element-type 'bit :initial-element 0)
                 (renderer-exposure-probe-frames renderer)
                 (make-array +exposure-probe-buffer-count+
                             :element-type '(unsigned-byte 64)
                             :initial-element 0))
           (create-frame-targets renderer extent)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (dolist (resource (append (and exposure-probe-buffers
                                       (coerce exposure-probe-buffers 'list))
                                  (list present-pipeline present-fragment-module
                                        present-vertex-module sampler
                                        present-bind-group sky-pipeline
                                        sky-fragment-module sky-bind-group
                                        sky-layout exposure-probe-bind-group
                                        exposure-probe-pipeline
                                        exposure-probe-fragment-module
                                        exposure-probe-view exposure-probe-texture
                                        exposure-probe-layout
                                        present-layout lattice-point-pipeline
                                        lattice-point-fragment-module
                                        lattice-point-vertex-module
                                        lattice-point-layout
                                        player-sdf-bind-group player-sdf-pipeline
                                        player-sdf-fragment-module
                                        player-sdf-vertex-module player-sdf-layout
                                        shadow-pipeline shadow-vertex-module
                                        shadow-layout shadow-sampler shadow-view
                                        shadow-texture pipeline fragment-module
                                        vertex-module layout material-buffer
                                        camera-buffer)))
          (when resource (ignore-errors (destroy resource))))))))

(defun draw-resident-population (pass resident bind-group)
  "Issue the two fixed-arity draws shared by the sun and scene passes."
  (let* ((population (resident-population-population resident))
         (triangle-count
           (render-population-triangle-instance-count population))
         (quad-count (render-population-quad-instance-count population)))
    (when (plusp (+ triangle-count quad-count))
      (set-bind-group pass 0 bind-group)
      (when (plusp triangle-count)
        (draw pass 3 triangle-count))
      (when (plusp quad-count)
        (draw pass 6 quad-count 0 triangle-count)))))

(defun exposure-probe-average-luminance (bytes)
  "Decode the geometric-mean luminance encoded by the 32x16 GPU probe."
  (unless (= (length bytes) +exposure-probe-byte-count+)
    (error "LUFT exposure probe returned ~D bytes, expected ~D."
           (length bytes) +exposure-probe-byte-count+))
  (let ((sum 0d0))
    (loop for index from 0 below (length bytes) by 4
          do (incf sum (aref bytes index)))
    (let* ((count (* +exposure-probe-width+ +exposure-probe-height+))
           (encoded (/ sum (* count 255d0)))
           (average-log (- (* encoded 11.98293d0) 9.21034d0)))
      (exp average-log))))

(defun adapted-exposure (current average-luminance)
  "Take one Moppe-style asymmetric eye-adaptation step."
  (let* ((target (max 0.55f0
                      (min 1.9f0
                           (/ 0.16f0 (coerce average-luminance
                                            'single-float)))))
         (rate (if (< target current) 0.10f0 0.04f0)))
    (+ current (* (- target current) rate))))

(defun maintain-renderer-exposure (renderer)
  "Consume the oldest completed probe without waiting for newer GPU work."
  (let ((buffers (renderer-exposure-probe-buffers renderer))
        (submitted (renderer-exposure-probe-submitted renderer))
        (frames (renderer-exposure-probe-frames renderer))
        (oldest nil))
    ;; A live DEFCLASS update can add this chronology lane to an existing
    ;; renderer between frames. Preserve its in-flight bits and give those
    ;; older probes one common age until the next transactional refresh.
    (unless (= (length frames) (length buffers))
      (setf frames
            (make-array (length buffers) :element-type '(unsigned-byte 64)
                                         :initial-element
                                         (renderer-frame-index renderer))
            (renderer-exposure-probe-frames renderer) frames))
    (dotimes (index (length buffers))
      (when (and (= 1 (aref submitted index))
                 (or (null oldest)
                     (< (aref frames index) (aref frames oldest))))
        (setf oldest index)))
    (when oldest
      (multiple-value-bind (bytes ready-p)
          (read-buffer-if-ready (aref buffers oldest))
        (when ready-p
          ;; One adaptation step per rendered frame keeps a CPU pause from
          ;; collapsing several delayed measurements into one visible jump.
          (setf (aref submitted oldest) 0
                (renderer-exposure renderer)
                (adapted-exposure
                 (renderer-exposure renderer)
                 (exposure-probe-average-luminance bytes)))))))
  (renderer-exposure renderer))

(defun encode-exposure-probe (renderer encoder temporal-p)
  "Reduce unified HDR scene radiance and queue one nonblocking readback."
  (let* ((index (mod (renderer-frame-index renderer)
                     +exposure-probe-buffer-count+))
         (submitted (renderer-exposure-probe-submitted renderer)))
    ;; If the GPU is more than three frames behind, keep rendering and retain
    ;; the last exposure instead of overwriting an in-flight measurement.
    (when (zerop (aref submitted index))
      (let ((pass
              (begin-render-pass
               encoder
               (make-render-pass-descriptor
                :label "luft exposure probe"
                :color-attachments
                `((:view ,(renderer-exposure-probe-view renderer)
                   :load-op :clear :store-op :store
                   :clear-value #(0.0 0.0 0.0 1.0)))))))
        (when temporal-p
          (wait-temporal-scaler-output
           pass (renderer-temporal-scaler renderer)))
        (set-pipeline pass (renderer-exposure-probe-pipeline renderer))
        (set-bind-group pass 0
                        (renderer-exposure-probe-bind-group renderer))
        (draw pass 3)
        (end-pass pass))
      (encode encoder
              (make-gpu-copy-texture-to-buffer-command
               :source (renderer-exposure-probe-texture renderer)
               :destination
               (aref (renderer-exposure-probe-buffers renderer) index)))
      (setf (aref submitted index) 1
            (aref (renderer-exposure-probe-frames renderer) index)
            (renderer-frame-index renderer)))))

(defun encode-renderer-frame
    (renderer encoder surface-texture extent camera-uniform-data
     &key jitter view player-p construction-p overlay-encoder)
  (ensure-renderer-extent renderer extent)
  (write-buffer (renderer-camera-buffer renderer) camera-uniform-data)
  (let ((shadow-pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :label "luft sun shadow"
              :color-attachments nil
              :depth-stencil-attachment
              `(:view ,(renderer-shadow-view renderer)
                :depth-load-op :clear :depth-store-op :store
                :depth-clear-value 1.0)))))
      (set-pipeline shadow-pass (renderer-shadow-pipeline renderer))
      (dolist (key (renderer-slot-order renderer))
        (let ((resident
                (mesh-slot-resident
                 (gethash key (renderer-mesh-slots renderer)))))
          (draw-resident-population
           shadow-pass resident
           (resident-population-shadow-bind-group resident))))
      (end-pass shadow-pass))
  (prepare-texture encoder (renderer-shadow-texture renderer)
                   :texture-binding)
  (let* ((temporal-p (renderer-temporal-p renderer))
         (color-view (renderer-scene-view renderer))
         (color-attachments
           (if temporal-p
               `((:view ,color-view :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 1.0))
                 (:view ,(renderer-motion-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 0.0)))
               `((:view ,color-view :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 1.0)))))
         (pass
           (begin-render-pass
            encoder
            (make-render-pass-descriptor
             :label "luft site streams"
             :color-attachments color-attachments
             :depth-stencil-attachment
             `(:view ,(renderer-depth-view renderer)
               :depth-load-op :clear
               :depth-store-op :store
               :depth-clear-value 1.0)))))
    ;; The atmosphere is scene-linear world radiance: geometry overwrites it,
    ;; MetalFX reconstructs it, and the exposure probe meters the same pixels
    ;; presentation will grade.
    (set-pipeline pass (renderer-sky-pipeline renderer))
    (set-bind-group pass 0 (renderer-sky-bind-group renderer))
    (draw pass 3)
    (set-pipeline pass (renderer-pipeline renderer))
    (dolist (key (renderer-slot-order renderer))
      (let ((resident
              (mesh-slot-resident
               (gethash key (renderer-mesh-slots renderer)))))
        (draw-resident-population
         pass resident (resident-population-bind-group resident))))
    (when player-p
      (set-pipeline pass (renderer-player-sdf-pipeline renderer))
      (set-bind-group pass 0 (renderer-player-sdf-bind-group renderer))
      (draw pass 6 2))
    (when construction-p
      ;; Populate at most one diagnostic slot per frame. The overlay is a
      ;; debugging view, so progressive readiness is preferable to freezing
      ;; one frame while every resident chunk is scanned.
      (loop for key in (renderer-slot-order renderer)
            for slot = (gethash key (renderer-mesh-slots renderer))
            unless (mesh-slot-lattice-point-buffer slot)
              do (ensure-mesh-slot-lattice-points renderer slot)
                 (return))
      (set-pipeline pass (renderer-lattice-point-pipeline renderer))
      (dolist (key (renderer-slot-order renderer))
        (let ((slot (gethash key (renderer-mesh-slots renderer))))
          (when (plusp (mesh-slot-lattice-point-count slot))
            (set-bind-group pass 0 (mesh-slot-lattice-point-group slot))
            (draw pass 6 (mesh-slot-lattice-point-count slot))))))
    (when temporal-p
      (signal-temporal-scaler-inputs pass
                                     (renderer-temporal-scaler renderer)))
    (end-pass pass)
    (when temporal-p
      (let ((scaler (renderer-temporal-scaler renderer))
            (history-valid-p (renderer-history-valid-p renderer))
            (render-extent (renderer-render-extent renderer)))
        (encode-temporal-scale
         encoder scaler
         (renderer-scene-texture renderer)
         (renderer-depth-texture renderer)
         (renderer-motion-texture renderer)
         (renderer-resolved-texture renderer)
         ;; JITTER is clip-space at the internal scene resolution.  MetalFX
         ;; takes the same offset in input pixels—not output pixels—so using
         ;; EXTENT here overstates it whenever temporal upscaling is active.
         (vector (* 0.5 (first render-extent) (aref jitter 0))
                 (* 0.5 (second render-extent) (aref jitter 1)))
         (not history-valid-p))
        (setf (renderer-previous-view renderer) view
              (renderer-history-valid-p renderer) t
              (renderer-history-used-p renderer) history-valid-p)))
    (unless temporal-p
      (prepare-texture encoder (renderer-scene-texture renderer)
                       :texture-binding)
      (prepare-texture encoder (renderer-depth-texture renderer)
                       :texture-binding))
    (encode-exposure-probe renderer encoder temporal-p)
    (let ((present-pass
            (begin-render-pass
             encoder
             (make-render-pass-descriptor
              :label "luft HDR presentation"
              :color-attachments
              `((:view ,surface-texture :load-op :clear :store-op :store
                 :clear-value #(0.0 0.0 0.0 1.0)))))))
      (when temporal-p
        (wait-temporal-scaler-output
         present-pass (renderer-temporal-scaler renderer)))
      (set-pipeline present-pass (renderer-present-pipeline renderer))
      (set-bind-group present-pass 0 (renderer-present-bind-group renderer))
      (draw present-pass 3)
      ;; Both MetalFX and the direct HDR path publish here.  The atelier
      ;; overlay remains later than tone mapping and glow in either case.
      (when overlay-encoder
        (funcall overlay-encoder present-pass))
      (end-pass present-pass))
    ;; Character motion is presentation time, not a MetalFX capability.
    (incf (renderer-frame-index renderer)))
  renderer)

(defun destroy-renderer (renderer)
  (destroy-renderer-targets renderer)
  (loop for slot being the hash-values of (renderer-mesh-slots renderer)
        do (%destroy-mesh-slot slot))
  (clrhash (renderer-mesh-slots renderer))
  (setf (renderer-slot-order renderer) nil)
  (dolist (resource
            (append
             (coerce (renderer-exposure-probe-buffers renderer) 'list)
             (list (renderer-exposure-probe-pipeline renderer)
                  (renderer-exposure-probe-fragment-module renderer)
                  (renderer-exposure-probe-view renderer)
                  (renderer-exposure-probe-texture renderer)
                  (renderer-exposure-probe-layout renderer)
                  (renderer-sky-pipeline renderer)
                  (renderer-sky-fragment-module renderer)
                  (renderer-sky-bind-group renderer)
                  (renderer-sky-layout renderer)
                  (renderer-present-pipeline renderer)
                  (renderer-present-fragment-module renderer)
                  (renderer-present-vertex-module renderer)
                  (renderer-sampler renderer)
                  (renderer-present-layout renderer)
                  (renderer-lattice-point-pipeline renderer)
                  (renderer-lattice-point-fragment-module renderer)
                  (renderer-lattice-point-vertex-module renderer)
                  (renderer-lattice-point-layout renderer)
                  (renderer-player-sdf-bind-group renderer)
                  (renderer-player-sdf-pipeline renderer)
                  (renderer-player-sdf-fragment-module renderer)
                  (renderer-player-sdf-vertex-module renderer)
                  (renderer-player-sdf-layout renderer)
                  (renderer-shadow-pipeline renderer)
                  (renderer-shadow-vertex-module renderer)
                  (renderer-shadow-layout renderer)
                  (renderer-shadow-sampler renderer)
                  (renderer-shadow-view renderer)
                  (renderer-shadow-texture renderer)
                  (renderer-pipeline renderer) (renderer-fragment-module renderer)
                  (renderer-vertex-module renderer)
                  (renderer-layout renderer)
                  (renderer-material-buffer renderer)
                  (and (slot-boundp renderer 'camera-buffer)
                       (renderer-camera-buffer renderer)))))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-present-pipeline renderer) nil
        (renderer-exposure-probe-pipeline renderer) nil
        (renderer-exposure-probe-fragment-module renderer) nil
        (renderer-exposure-probe-view renderer) nil
        (renderer-exposure-probe-texture renderer) nil
        (renderer-exposure-probe-layout renderer) nil
        (renderer-exposure-probe-buffers renderer) #()
        (renderer-exposure-probe-submitted renderer)
        (make-array 0 :element-type 'bit)
        (renderer-exposure-probe-frames renderer) #()
        (renderer-sky-pipeline renderer) nil
        (renderer-sky-fragment-module renderer) nil
        (renderer-sky-bind-group renderer) nil
        (renderer-sky-layout renderer) nil
        (renderer-present-fragment-module renderer) nil
        (renderer-present-vertex-module renderer) nil
        (renderer-sampler renderer) nil
        (renderer-present-layout renderer) nil
        (renderer-lattice-point-pipeline renderer) nil
        (renderer-lattice-point-fragment-module renderer) nil
        (renderer-lattice-point-vertex-module renderer) nil
        (renderer-lattice-point-layout renderer) nil
        (renderer-player-sdf-bind-group renderer) nil
        (renderer-player-sdf-pipeline renderer) nil
        (renderer-player-sdf-fragment-module renderer) nil
        (renderer-player-sdf-vertex-module renderer) nil
        (renderer-player-sdf-layout renderer) nil
        (renderer-shadow-pipeline renderer) nil
        (renderer-shadow-vertex-module renderer) nil
        (renderer-shadow-layout renderer) nil
        (renderer-shadow-sampler renderer) nil
        (renderer-shadow-view renderer) nil
        (renderer-shadow-texture renderer) nil
        (renderer-pipeline renderer) nil
        (renderer-fragment-module renderer) nil
        (renderer-vertex-module renderer) nil
        (renderer-layout renderer) nil
        (renderer-material-buffer renderer) nil
        (renderer-camera-buffer renderer) nil)
  (values))

;;; ---------------------------------------------------------------------------
;;; Streaming chunk scenes
;;;
;;; A streaming scene is an ordinary authored scene whose solid is split into
;;; chunk chains. A bounded square window follows the camera. Each focus change
;;; installs the final desired residency first, then remeshes exactly the chunks
;;; whose 3 by 3 dependency neighborhoods changed. MESH-CHUNK's probes into
;;; non-resident neighbors signal MISSING-CHUNK; immutable worker snapshots
;;; answer USE-CHUNK for the captured neighborhood and TREAT-AS-AIR otherwise.
;;; The canvas owner publishes replacements and departures as one complete
;;; cohort, so no frame observes a mixed seam generation.

(defclass streaming-scene (scene)
  ((store :initform (make-hash-table :test #'eql)
          :reader streaming-scene-store)
   (loaded :initform (make-hash-table :test #'eql)
           :reader streaming-scene-loaded)
   (merged :initform (make-hash-table :test #'eql)
           :reader streaming-scene-merged)
   (outstanding :initform (make-hash-table :test #'eql)
                :reader streaming-scene-outstanding)
   (staged :initform (make-hash-table :test #'eql)
           :reader streaming-scene-staged)
   (cohort :initform nil :accessor streaming-scene-cohort)
   (removals :initform nil :accessor streaming-scene-removals)
   (production-errors :initform nil
                      :accessor streaming-scene-production-errors)
   (frames-per-load :initarg :frames-per-load :initform 15
                    :accessor streaming-scene-frames-per-load)
   (residency-radius :initarg :residency-radius :initform 1
                     :accessor streaming-scene-residency-radius)
   (lod-radius :initarg :lod-radius :initform nil
               :accessor streaming-scene-lod-radius)
   (merge-radius :initarg :merge-radius :initform nil
                 :accessor streaming-scene-merge-radius)
   (far-bevel-width :initarg :far-bevel-width :initform 4
                    :accessor streaming-scene-far-bevel-width)
   (focus :initform nil :accessor streaming-scene-focus)
   (frame-counter :initform 0 :accessor streaming-scene-frame-counter)))

(defstruct (streaming-mesh-snapshot
             (:constructor %make-streaming-mesh-snapshot
                 (scene key bevel-width coplanar-merge-p neighborhood stamp)))
  "Immutable CPU input for one chunk mesh request."
  (scene nil :read-only t)
  (key 0 :type luft:chunk-key :read-only t)
  (bevel-width luft:+mesh-bevel-width+ :read-only t)
  (coplanar-merge-p nil :type boolean :read-only t)
  (neighborhood nil :type hash-table :read-only t)
  (stamp nil :read-only t))

(defclass streaming-mesh-request (production:production-request)
  ((snapshot :initarg :snapshot :reader streaming-mesh-request-snapshot)))

(defun make-streaming-scene
    (scene &key (frames-per-load 15) (residency-radius 1) lod-radius merge-radius
                 (far-bevel-width 4))
  "Wrap SCENE in bounded camera-driven chunk residency.

When LOD-RADIUS is non-NIL, chunks beyond that Chebyshev distance use
FAR-BEVEL-WIDTH. When MERGE-RADIUS is non-NIL, chunks beyond it additionally
dissolve exactly coplanar edges without changing that bevel surface.
FAR-BEVEL-WIDTH names the medial tier in residency state."
  (let ((streaming (make-instance
                    'streaming-scene
                    :solid (scene-solid scene)
                    :material-vocabulary (scene-material-vocabulary scene)
                    :material-cells (scene-material-cells scene)
                    :material-program (scene-material-program scene)
                    :frames-per-load frames-per-load
                    :residency-radius residency-radius
                    :lod-radius lod-radius
                    :merge-radius merge-radius
                    :far-bevel-width far-bevel-width)))
    (luft:map-chain-chunks
     (lambda (key chain)
       (setf (gethash key (streaming-scene-store streaming)) chain))
     (scene-solid scene))
    streaming))

(defun streaming-scene-keys-near (scene focus-x focus-y)
  "Stored chunk keys inside SCENE's square residency window."
  (let ((radius (streaming-scene-residency-radius scene))
        (keys nil))
    (loop for key being the hash-keys of (streaming-scene-store scene)
          when (and (<= (abs (- (luft:chunk-key-x key) focus-x)) radius)
                    (<= (abs (- (luft:chunk-key-y key) focus-y)) radius))
            do (push key keys))
    (sort keys #'<)))

(defun chunk-keys-neighbor-p (left right)
  (and (<= (abs (- (luft:chunk-key-x left) (luft:chunk-key-x right))) 1)
       (<= (abs (- (luft:chunk-key-y left) (luft:chunk-key-y right))) 1)))

(defun streaming-scene-key-distance (key focus)
  "Chebyshev chunk distance between KEY and FOCUS."
  (max (abs (- (luft:chunk-key-x key) (car focus)))
       (abs (- (luft:chunk-key-y key) (cdr focus)))))

(defun streaming-scene-bevel-width-at (scene key focus near-bevel-width)
  "Select KEY's mesh tier under FOCUS."
  (let ((lod-radius (streaming-scene-lod-radius scene)))
    (if (and lod-radius
             (> (streaming-scene-key-distance key focus) lod-radius))
        (streaming-scene-far-bevel-width scene)
        near-bevel-width)))

(defun streaming-scene-coplanar-p-at (scene key focus)
  "Whether KEY uses SCENE's exact coplanar compression under FOCUS."
  (let ((merge-radius (streaming-scene-merge-radius scene)))
    (and focus merge-radius
         (> (streaming-scene-key-distance key focus) merge-radius))))

(defun retarget-streaming-scene
    (scene production-system bevel-width world-x world-y)
  "Batch SCENE's desired window around a camera position and mesh it once."
  (when (or (streaming-scene-cohort scene)
            (streaming-scene-removals scene))
    (return-from retarget-streaming-scene nil))
  (let* ((focus-key (luft:chunk-key-at (floor world-x) (floor world-y)))
         (focus (cons (luft:chunk-key-x focus-key)
                      (luft:chunk-key-y focus-key)))
         (desired (streaming-scene-keys-near scene (car focus) (cdr focus)))
         (loaded (streaming-scene-loaded scene))
         (merged (streaming-scene-merged scene))
         (desired-widths (make-hash-table :test #'eql))
         (desired-merged (make-hash-table :test #'eql))
         (arrivals
           (remove-if (lambda (key) (gethash key loaded)) desired))
         (departures
           (loop for key being the hash-keys of loaded
                 unless (member key desired :test #'eql)
                   collect key))
         (lod-changes nil))
    (dolist (key desired)
      (let ((width
              (streaming-scene-bevel-width-at
               scene key focus bevel-width))
            (merge-p (streaming-scene-coplanar-p-at scene key focus)))
        (setf (gethash key desired-widths) width
              (gethash key desired-merged) merge-p)
        (multiple-value-bind (old-width present-p) (gethash key loaded)
          (when (and present-p
                     (or (not (eql old-width width))
                         (not (eql (gethash key merged) merge-p))))
            (push key lod-changes)))))
    (let ((residency-changes (append arrivals departures))
          (changes (append arrivals departures lod-changes)))
      (setf (streaming-scene-focus scene) focus)
      (when changes
        (clrhash loaded)
        (clrhash merged)
        (dolist (key desired)
          (setf (gethash key loaded) (gethash key desired-widths)
                (gethash key merged) (gethash key desired-merged)))
        (let ((affected
                (remove-if-not
                 (lambda (key)
                   (or (member key lod-changes :test #'eql)
                       (some (lambda (changed)
                               (chunk-keys-neighbor-p key changed))
                             residency-changes)))
                 desired)))
          (setf (streaming-scene-cohort scene) affected
                (streaming-scene-removals scene) departures)
          (dolist (key affected)
            (schedule-streaming-scene-mesh
             scene production-system key (gethash key loaded)
             (streaming-scene-key-distance key focus))))
        t))))

(defun streaming-scene-neighborhood-keys (scene key)
  "Return the loaded 3 by 3 chunk neighborhood observed while meshing KEY."
  (let ((x (luft:chunk-key-x key))
        (y (luft:chunk-key-y key))
        (keys nil))
    (loop for candidate being the hash-keys of (streaming-scene-loaded scene)
          when (and (<= (abs (- (luft:chunk-key-x candidate) x)) 1)
                    (<= (abs (- (luft:chunk-key-y candidate) y)) 1))
            do (push candidate keys))
    (sort keys #'<)))

(defun streaming-scene-mesh-stamp (scene key bevel-width coplanar-merge-p)
  "Name the exact residency and geometry parameters observed by KEY's mesh."
  (list bevel-width coplanar-merge-p
        (gethash key (streaming-scene-loaded scene))
        (streaming-scene-neighborhood-keys scene key)))

(defun make-streaming-mesh-snapshot (scene key bevel-width)
  "Capture immutable chains for the neighborhood KEY currently observes."
  (let ((neighborhood (make-hash-table :test #'eql))
        (store (streaming-scene-store scene))
        (coplanar-merge-p (gethash key (streaming-scene-merged scene))))
    (dolist (neighbor (streaming-scene-neighborhood-keys scene key))
      (setf (gethash neighbor neighborhood) (gethash neighbor store)))
    (%make-streaming-mesh-snapshot
     scene key bevel-width coplanar-merge-p neighborhood
     (streaming-scene-mesh-stamp
      scene key bevel-width coplanar-merge-p))))

(defun mesh-streaming-snapshot (snapshot)
  "Mesh one worker-owned residency snapshot without reading owner state."
  (let* ((scene (streaming-mesh-snapshot-scene snapshot))
         (key (streaming-mesh-snapshot-key snapshot))
         (neighborhood (streaming-mesh-snapshot-neighborhood snapshot))
         (stock-function (make-scene-face-stock-function scene))
         (chamfer-stock-function
           (make-compiled-material-chamfer-stock-function
            (scene-material-program scene))))
    (handler-bind
        ((luft:missing-chunk
           (lambda (condition)
             (multiple-value-bind (chain present-p)
                 (gethash (luft:missing-chunk-key condition) neighborhood)
               (if present-p
                   (invoke-restart 'luft:use-chunk chain)
                   (invoke-restart 'luft:treat-as-air)))))
         (luft:outside-domain
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'luft:treat-as-air))))
      (let ((chain (gethash key neighborhood)))
        (zone (:luft/rematerialize :value (luft:chain-count chain))
          (luft:mesh-chunk chain key
                           :stock-function stock-function
                           :chamfer-stock-function chamfer-stock-function
                           :bevel-width
                           (streaming-mesh-snapshot-bevel-width snapshot)
                           :coplanar-merge-p
                           (streaming-mesh-snapshot-coplanar-merge-p
                            snapshot)))))))

(defun mesh-streaming-chunk (scene key bevel-width)
  "Synchronously mesh KEY from the same immutable snapshot workers receive."
  (mesh-streaming-snapshot
   (make-streaming-mesh-snapshot scene key bevel-width)))

(defmethod production:perform-production-request
    ((request streaming-mesh-request))
  (prepare-render-mesh
   (mesh-streaming-snapshot (streaming-mesh-request-snapshot request))))

(defun schedule-streaming-scene-mesh
    (scene production-system key bevel-width priority)
  (let* ((snapshot (make-streaming-mesh-snapshot scene key bevel-width))
         (request
           (make-instance 'streaming-mesh-request
                          :key key :priority priority :snapshot snapshot))
         (ticket
           (production:schedule-production-request production-system request)))
    (setf (gethash key (streaming-scene-outstanding scene)) ticket)
    request))

(defun current-streaming-mesh-request-p (scene request)
  (let ((snapshot (streaming-mesh-request-snapshot request)))
    (and (eq scene (streaming-mesh-snapshot-scene snapshot))
         (equal (streaming-mesh-snapshot-stamp snapshot)
                (streaming-scene-mesh-stamp
                 scene
                 (streaming-mesh-snapshot-key snapshot)
                 (streaming-mesh-snapshot-bevel-width snapshot)
                 (streaming-mesh-snapshot-coplanar-merge-p snapshot))))))

(defun accept-streaming-mesh-result (scene request mesh)
  "Stage MESH when REQUEST is still the latest description of its chunk."
  (let* ((key (production:production-request-key request))
         (ticket (gethash key (streaming-scene-outstanding scene))))
    (when (eql ticket (production:production-request-ticket request))
      (remhash key (streaming-scene-outstanding scene))
      (when (current-streaming-mesh-request-p scene request)
        (setf (gethash key (streaming-scene-staged scene))
              (cons request mesh))
        t))))

(defun ready-streaming-scene-meshes (scene)
  "Return the complete current cohort and a readiness flag."
  (let ((cohort (streaming-scene-cohort scene))
        (staged (streaming-scene-staged scene))
        (active-p (or (streaming-scene-cohort scene)
                      (streaming-scene-removals scene))))
    (if (and active-p
             (every (lambda (key)
                      (let ((entry (gethash key staged)))
                        (and entry
                             (current-streaming-mesh-request-p
                              scene (car entry)))))
                    cohort))
        (values
         (mapcar (lambda (key) (cons key (cdr (gethash key staged)))) cohort)
         t)
        (values nil nil))))

(defun publish-ready-streaming-scene (scene renderer)
  "Install a complete current mesh cohort at the canvas-owner boundary."
  (multiple-value-bind (meshes ready-p)
      (ready-streaming-scene-meshes scene)
    (when ready-p
      (renderer-update-meshes
       renderer meshes (streaming-scene-removals scene))
      (dolist (entry meshes)
        (remhash (car entry) (streaming-scene-staged scene)))
      (setf (streaming-scene-cohort scene) nil
            (streaming-scene-removals scene) nil)
      (length meshes))))

(defun drain-streaming-scene-production
    (scene renderer production-system &key (limit 2))
  "Drain and publish bounded worker results on the canvas owner thread."
  (loop repeat limit
        do (multiple-value-bind (result present-p)
               (production:receive-production-result-no-hang production-system)
             (unless present-p (return))
             (let* ((request (production:production-result-request result))
                    (key (production:production-request-key request))
                    (ticket (gethash key
                                     (streaming-scene-outstanding scene))))
               (when (eql ticket
                          (production:production-request-ticket request))
                 (if (production:production-result-condition result)
                     (progn
                       (remhash key (streaming-scene-outstanding scene))
                       (push result
                             (streaming-scene-production-errors scene))
                       (error "LUFT mesh production for chunk ~D failed: ~A"
                              key
                              (production:production-result-condition result)))
                     (accept-streaming-mesh-result
                      scene request (production:production-result-value result)))))))
  (publish-ready-streaming-scene scene renderer))

(defun landscape-hash-reading (x y seed salt)
  "Return a stable coordinate reading in [-1, 1]."
  (let ((value
          (logand #xffffffff
                  (+ seed (* x 374761393) (* y 668265263)
                     (* salt 2246822519)))))
    (setf value
          (logand #xffffffff
                  (* (logxor value (ash value -13)) 1274126177)))
    (- (* 2.0d0
          (/ (logand #xffffffff (logxor value (ash value -16)))
             #xffffffff))
       1.0d0)))

(defun smooth-landscape-reading (reading)
  (* reading reading (- 3.0d0 (* 2.0d0 reading))))

(defun interpolate-landscape-reading (left right amount)
  (+ left (* (- right left) amount)))

(defun landscape-value-noise (x y period seed salt)
  "Sample stable smooth value noise without allocating terrain objects."
  (let* ((sample-x (/ x (coerce period 'double-float)))
         (sample-y (/ y (coerce period 'double-float)))
         (cell-x (floor sample-x))
         (cell-y (floor sample-y))
         (tx (smooth-landscape-reading (- sample-x cell-x)))
         (ty (smooth-landscape-reading (- sample-y cell-y)))
         (near
           (interpolate-landscape-reading
            (landscape-hash-reading cell-x cell-y seed salt)
            (landscape-hash-reading (1+ cell-x) cell-y seed salt)
            tx))
         (far
           (interpolate-landscape-reading
            (landscape-hash-reading cell-x (1+ cell-y) seed salt)
            (landscape-hash-reading (1+ cell-x) (1+ cell-y) seed salt)
            tx)))
    (interpolate-landscape-reading near far ty)))

(defun landscape-ramp (low high reading)
  (smooth-landscape-reading
   (max 0.0d0 (min 1.0d0 (/ (- reading low) (- high low))))))

(defun highland-landscape-height (x y size &key (seed 121))
  "Height of the authored highland at X,Y.

The invariant is that every reading is a pure function of X, Y, SIZE, and
SEED. Low-frequency domain warping bends a zero-contour into mountain chains;
independent fields add foothills, a basin, a river valley, and a terraced
upland instead of repeating one periodic profile."
  (let* ((warp-x
           (+ x (* 23.0d0 (landscape-value-noise x y 113 seed 40))))
         (warp-y
           (+ y (* 23.0d0 (landscape-value-noise x y 127 seed 41))))
         (continent (landscape-value-noise warp-x warp-y 211 seed 0))
         (fold (abs (landscape-value-noise warp-x warp-y 103 seed 7)))
         (ridge (expt (max 0.0d0 (- 1.0d0 (* 1.45d0 fold))) 1.55d0))
         (mountain-country
           (+ 0.3d0
              (* 0.7d0
                 (landscape-ramp
                  -0.35d0 0.55d0
                  (landscape-value-noise x y 237 seed 8)))))
         (foothills (landscape-value-noise warp-x warp-y 47 seed 2))
         (detail (landscape-value-noise x y 17 seed 3))
         (western-distance
           (sqrt (+ (expt (/ (- x (* size 0.27d0)) (* size 0.31d0)) 2)
                    (expt (/ (- y (* size 0.58d0)) (* size 0.40d0)) 2))))
         (eastern-distance
           (sqrt (+ (expt (/ (- x (* size 0.72d0)) (* size 0.24d0)) 2)
                    (expt (/ (- y (* size 0.30d0)) (* size 0.30d0)) 2))))
         (massif
           (max (expt (max 0.0d0 (- 1.0d0 western-distance)) 1.4d0)
                (* 0.78d0
                   (expt (max 0.0d0 (- 1.0d0 eastern-distance)) 1.3d0))))
         (river-centre
           (+ (* size 0.46d0)
              (* size 0.13d0
                 (landscape-value-noise x 0 139 seed 19))))
         (river-width (+ 7.0d0 (* size 0.018d0)))
         (river
           (expt (max 0.0d0
                      (- 1.0d0 (/ (abs (- y river-centre)) river-width)))
                 2))
         (basin-x (* size 0.24d0))
         (basin-y (* size 0.73d0))
         (basin-distance
           (sqrt (+ (expt (- x basin-x) 2) (expt (- y basin-y) 2))))
         (basin
           (expt (max 0.0d0
                      (- 1.0d0 (/ basin-distance (* size 0.22d0))))
                 2))
         (raw
           (- (+ 10.0d0 (* 5.0d0 continent)
                 (* 30.0d0 ridge mountain-country)
                 (* massif (+ 22.0d0 (* 10.0d0 ridge)))
                 (* 4.5d0 foothills) (* 2.6d0 detail))
              (* 9.0d0 river) (* 7.0d0 basin)))
         (plateau
           (landscape-ramp
            0.18d0 0.62d0
            (+ (landscape-value-noise x y 151 seed 29)
               (* 0.55d0 (/ (- (+ x y) size) size)))))
         (terraced (* 3.0d0 (round (/ raw 3.0d0))))
         (height
           (interpolate-landscape-reading raw terraced (* 0.35d0 plateau))))
    (max 2 (min 72 (round height)))))

(defun build-highland-lookout (builder x y base &key (citadel-p nil))
  "Build one sparse limestone lookout; CITADEL-P makes the regional anchor."
  (let ((platform-radius (if citadel-p 13 5))
        (tower-inner (if citadel-p 5 2))
        (tower-outer (if citadel-p 8 4))
        (tower-height (if citadel-p 15 10)))
    (scene-builder-disc builder x y platform-radius
                        (1- base) base :architecture-p t)
    (scene-builder-ring builder x y tower-inner tower-outer
                        (1+ base) (+ base tower-height) :architecture-p t)
    (scene-builder-ring builder x y tower-inner (1+ tower-outer)
                        (+ base tower-height 1) (+ base tower-height 2)
                        :architecture-p t)
    (unless citadel-p
      (scene-builder-disc builder x y tower-outer
                          (+ base tower-height 2) (+ base tower-height 2)
                          :architecture-p t))
    (when citadel-p
      (scene-builder-carve-arch
       builder x (+ base 1) (+ base 5) 2
       (cons (- y tower-outer) (+ y tower-outer)) :axis :x)
      (scene-builder-carve-arch
       builder y (+ base 1) (+ base 5) 2
       (cons (- x tower-outer) (+ x tower-outer)) :axis :y))))

(defun make-highland-sanctuary-scene
    (&key (horizontal-bits 9) (seed 121) (streaming-p t))
  "Make a large varied landscape with mountain chains and sparse ruins.

The default 512 by 512 world spans sixty-four LUFT chunks. STREAMING-P wraps
it in a seven-by-seven camera-driven window: full bevel nearby, medial terrain
in the middle ring, and exact coplanar compression in the outer ring."
  (let* ((builder (make-scene-builder :horizontal-bits horizontal-bits))
         (size (ash 1 horizontal-bits)))
    (dotimes (x size)
      (dotimes (y size)
        (let* ((height (highland-landscape-height x y size :seed seed))
               (rock-depth (max 0 (min 6 (floor (- height 20) 3)))))
          (dotimes (z height)
            (scene-builder-cell
             builder x y z
             :material
             (if (>= z (- height rock-depth))
                 *highland-rock-material-placement*
                 *terrain-material-placement*))))))
    ;; Deliberately sparse, asymmetrical anchors replace the old tower in every
    ;; chunk. Their different scales make travel across the terrain legible.
    (dolist (landmark '((0.27d0 0.29d0 nil)
                        (0.71d0 0.68d0 t)
                        (0.79d0 0.24d0 nil)))
      (destructuring-bind (fraction-x fraction-y citadel-p) landmark
        (let* ((x (round (* size fraction-x)))
               (y (round (* size fraction-y)))
               (base (highland-landscape-height x y size :seed seed)))
          (build-highland-lookout builder x y base :citadel-p citadel-p))))
    (let ((scene (finish-scene-builder builder)))
      (if streaming-p
          (make-streaming-scene scene :residency-radius 3 :lod-radius 1
                                :merge-radius 2)
          scene))))
