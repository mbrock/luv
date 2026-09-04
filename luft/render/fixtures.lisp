(in-package #:luft.render)

;;; Authored fixture scenes: the miter and bevel studies, the mountain
;;; sanctuary, the star gallery, and the highland landscape.

(defconstant +sanctuary-origin-x+ 32)
(defconstant +sanctuary-origin-y+ 24)
(defparameter *sanctuary-beacon-x* 58)
(defparameter *sanctuary-beacon-y* 54)

(defun make-manifold-spike-scene ()
  "Three isolated singular-star fixtures for the manifold-sheet spike.

The plots exercise an edge-touching pair, a corner-touching pair, and the
four-sheet parity star.  Nothing else in the scene can hide their junctions.
#WSEK3C"
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
      (place-star #x18 14)
      (place-star #x69 18))
    (finish-scene-builder builder)))

(defun make-bevel-limit-study-scene ()
  "One isolated stone cell for comparing sub-medial and medial bevels."
  (let ((builder (make-scene-builder :horizontal-bits 4)))
    (scene-builder-cell builder 6 4 3 :architecture-p t)
    (finish-scene-builder builder)))

(defun make-voxel-light-shrine-scene (&key (voxel-light-propagation-p t))
  "A compact production fixture for colored propagation and face torches."
  (let ((builder (make-scene-builder :horizontal-bits 6)))
    ;; A pale receiving room with a dark backing visible through the crystal.
    (scene-builder-box builder 6 18 6 18 4 4 :architecture-p t)
    (scene-builder-box builder 6 18 18 18 5 13 :architecture-p t)
    (scene-builder-box builder 6 6 7 18 5 11 :architecture-p t)
    (scene-builder-box builder 7 17 9 11 12 12 :architecture-p t)
    (scene-builder-box builder 10 14 17 17 5 9
                       :material *highland-rock-material-placement*)
    ;; Medial crystal silhouettes are point-contact jewels, not glass cubes.
    (scene-builder-cell builder 12 13 5
                        :material *crystal-material-placement*)
    (scene-builder-cell builder 15 15 5
                        :material *crystal-material-placement*)
    ;; Floor, back-wall, side-wall, and ceiling attachments exercise four
    ;; normals while remaining the same geometry at every bevel width.
    (scene-builder-torch builder 8 10 4 :z :high)
    (scene-builder-torch builder 8 18 8 :y :low)
    (scene-builder-torch builder 6 14 7 :x :high)
    (scene-builder-torch builder 16 10 12 :z :low)
    (finish-scene-builder
     builder :voxel-light-propagation-p voxel-light-propagation-p)))

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
      (stair-boundary :low-wall) (player-p t))
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
            (let* ((height
                     (max 1 (mountain-sanctuary-terrain-height x y)))
                   ;; Keep one living-earth cap.  Only cells actually exposed
                   ;; above a lower cardinal neighbor become cliff rock; this
                   ;; is authored cell material, never material topology.
                   (exposed-base
                     (min (mountain-sanctuary-terrain-height (1- x) y)
                          (mountain-sanctuary-terrain-height (1+ x) y)
                          (mountain-sanctuary-terrain-height x (1- y))
                          (mountain-sanctuary-terrain-height x (1+ y)))))
              (loop for z below height do
                (scene-builder-cell
                 builder x y z
                 :material
                 (if (and (< z (1- height)) (>= z exposed-base))
                     *highland-rock-material-placement*
                     *terrain-material-placement*))))))))
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
      ;; The sun never reaches the south-facing curtain, so the gate approach
      ;; shows both luminous materials live: warm flames flanking the gate and
      ;; cool crystal jewels bedded on the podium ledge below the wall.
      (scene-builder-torch builder 26 45 22 :y :low)
      (scene-builder-torch builder 34 45 22 :y :low)
      (scene-builder-cell builder 22 44 19
                          :material *crystal-material-placement*)
      (scene-builder-cell builder 38 44 19
                          :material *crystal-material-placement*)
      ;; The arcaded hall keeps a pair of flames on its north wall and one
      ;; floor crystal, visible through the central bay.
      (scene-builder-torch builder 26 60 22 :y :low)
      (scene-builder-torch builder 34 60 22 :y :low)
      (scene-builder-cell builder 30 57 20
                          :material *crystal-material-placement*)
      ;; Scattered jewels probe the mesher's material transitions: point
      ;; contacts capping three bridge-rail posts, corner stones completing
      ;; the parapet ring where its diagonal merlons leave a gap, one block
      ;; at a gate-jamb base, and shards bedded into the south cliff face.
      (dolist (post '((25 10) (34 20) (25 30)))
        (destructuring-bind (px py) post
          (scene-builder-cell builder px py (+ deck 3)
                              :material *crystal-material-placement*)))
      (scene-builder-cell builder 13 44 (+ plateau 8)
                          :material *crystal-material-placement*)
      (scene-builder-cell builder 47 44 (+ plateau 8)
                          :material *crystal-material-placement*)
      (scene-builder-cell builder 26 45 plateau
                          :material *crystal-material-placement*)
      (dolist (shard '((50 37 4) (52 37 3)))
        (destructuring-bind (sx sy depth) shard
          (scene-builder-cell
           builder sx sy
           (max 3 (- (mountain-sanctuary-terrain-height sx sy) depth))
           :material *crystal-material-placement*)))
      (finish-scene-builder builder :player-p player-p)))

(defun make-traveler-study-scene ()
  "A bare limestone dais under the traveler, clear from every direction.

The sanctuary is the scene he belongs in, but it is also a scene in which
half the useful camera angles look through a parapet.  This fixture keeps
his exact world position and his exact deck height and removes everything
else, so a turnaround can be shot around him without moving him."
  (let ((builder (make-scene-builder :horizontal-bits 7)))
    (scene-builder-box builder 56 67 40 58 11 13 :architecture-p t)
    (finish-scene-builder builder :player-p t)))

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
it in a seven-by-seven camera-driven window whose owners share one exact bevel
policy; distance geometry awaits an explicit seam-transition representation."
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
          (make-streaming-scene scene :residency-radius 3)
          scene))))
