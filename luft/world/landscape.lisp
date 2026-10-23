(in-package #:luft.world)

;;; The atelier's large world: value-noise landscape readings, Luvcraft's
;;; alpine relief, and the deterministic road, river, and citadel that a
;;; world source generates chunk by chunk.

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

(defconstant +large-world-horizontal-bits+ 11)
(defconstant +large-world-seed+ 121)
(defconstant +large-world-spawn-x+ 32)

(defclass authored-world-source (world-source)
  ((seed :initarg :seed :initform +large-world-seed+
         :reader authored-world-source-seed))
  (:default-initargs :material-vocabulary (make-scene-material-vocabulary))
  (:documentation
   "The canonical large world: alpine relief, a road, a river, and a citadel."))

(defun large-world-road-centre-y (x)
  "Authored west-to-east route from the old sanctuary spawn to the citadel."
  (+ 48.0d0 (* 0.418d0 (- x 64))
     (* 18.0d0 (sin (/ (- x 64) 173.0d0)))))

(defun large-world-river-centre-x (y)
  "Authored north-to-south river course, independent of chunk partitioning."
  (+ 612.0d0 (* 58.0d0 (sin (/ y 149.0d0)))
     (* 17.0d0 (sin (/ y 43.0d0)))))

(defun large-world-road-height (x)
  (+ 15 (round (* 0.004d0 x))))

;;; The natural ground is Luvcraft's alpine relief (luvcraft/terrain.lisp),
;;; read through the same hash and value noise, so seed 121 raises the same
;;; ranges in both worlds.  A broad mask gathers ridged peaks into ranges
;;; with open lowland between them; narrow ravines cut through everything.
;;; What made the older composition flat was not its height but its slope:
;;; its one ridge rose over 260 cells, so no ground anywhere climbed two
;;; cells per cell, where the alpine relief has cliffs at walking scale.

(defparameter *large-world-relief-height* 88.0d0
  "Cells spanned by the alpine relief's unit height.  Lowland sits near 17.")

(defparameter *alpine-snow-line* 0.60d0
  "Relief fraction above which gentle ground carries snow.")

(defparameter *alpine-tree-line* 0.42d0
  "Relief fraction above which the surface is bare rock.")

(defparameter *alpine-beach-level* 0.13d0
  "Relief fraction at or below which the surface is sand.")

(defun alpine-landscape-relief (x y seed)
  "The alpine surface at X,Y as a fraction of the relief height."
  (flet ((noise (period salt) (landscape-value-noise x y period seed salt)))
    (let* ((lowland (+ 0.19d0
                       (* 0.050d0 (noise 96 0))
                       (* 0.028d0 (noise 28 1))
                       (* 0.012d0 (noise 11 2))))
           (range (landscape-ramp 0.08d0 0.42d0
                                  (+ (noise 176 3) (* 0.35d0 (noise 72 6)))))
           (ridge (- 1.0d0 (abs (noise 44 4))))
           (crag (- 1.0d0 (abs (noise 18 5))))
           (mountain (* range
                        (+ 0.10d0 (* 0.42d0 ridge ridge) (* 0.12d0 crag)
                           (* 0.03d0 (noise 7 8)))))
           (ravine (* 0.12d0
                      (landscape-ramp 0.92d0 0.985d0
                                      (- 1.0d0 (abs (noise 90 7)))))))
      (- (+ lowland mountain) ravine))))

(defun large-world-terrain-height (source x y)
  "Deterministic composed terrain under the route, river, and citadel."
  (let* ((seed (authored-world-source-seed source))
         (natural (* *large-world-relief-height*
                     (alpine-landscape-relief x y seed)))
         (road-y (large-world-road-centre-y x))
         (road-distance (abs (- y road-y)))
         (river-x (large-world-river-centre-x y))
         (river-distance (abs (- x river-x)))
         (river-bed (- natural
                       (* 12.0d0
                          (expt (max 0.0d0
                                     (- 1.0d0 (/ river-distance 18.0d0)))
                                2))))
         (road-height (large-world-road-height x))
         ;; Keep the masonry road itself level enough to walk and feather its
         ;; broad verge into natural terrain. Through a range the verge is a
         ;; cutting, which is what a road through mountains looks like.
         (road-blend
           (smooth-landscape-reading
            (max 0.0d0 (min 1.0d0 (/ (- 14.0d0 road-distance) 8.0d0)))))
         (routed (interpolate-landscape-reading
                  river-bed road-height road-blend))
         (citadel-distance
           (max (abs (- x 1500.0d0)) (abs (- y 650.0d0))))
         (citadel-blend (max 0.0d0 (- 1.0d0 (/ citadel-distance 52.0d0)))))
    (max 3 (min 92
                (round (interpolate-landscape-reading
                        routed 23.0d0 citadel-blend))))))

(defun large-world-surface-slope (source x y height)
  "The largest height step between X,Y's HEIGHT and its four neighbours."
  (loop for (dx dy) in '((-1 0) (1 0) (0 -1) (0 1))
        maximize (abs (- height (large-world-terrain-height
                                 source (+ x dx) (+ y dy))))))

;;; Where a new player stands.  The world's corner is the worst view it has:
;;; half the horizon is the world's edge.  Like Luvcraft's scenic spawn, walk
;;; the road inward until a snow-line summit stands within view, and face it.

(defparameter *large-world-spawn-view-distance* 88
  "How far from the spawn a summit may stand and still count as in view.")

(defvar *large-world-scenic-spawns* (make-hash-table :test #'equal)
  "Scenic spawns already found, keyed by seed and domain extent.")

(defparameter *large-world-spawn-clearing* 28
  "Cells around a spawn that must stay near road height, so it opens onto
a view rather than onto a hillside.")

(defparameter *large-world-spawn-summit-distance* 60
  "The nearest a spawn's summit may stand, so it does not fill the view.")

(defun large-world-summit-visible-p (source x y eye summit-x summit-y summit)
  "Whether the ground from X,Y stays below the sight line to a SUMMIT top.
The line runs from EYE height to two cells under the summit's top, so the
whole upper peak shows rather than a sliver of it."
  (let* ((dx (- summit-x x))
         (dy (- summit-y y))
         (distance (sqrt (+ (* dx dx) (* dy dy))))
         (target (- summit 2)))
    (loop for travel from 4 below (- distance 6) by 3
          for fraction = (/ travel distance)
          always (< (large-world-terrain-height
                     source
                     (round (+ x (* dx fraction)))
                     (round (+ y (* dy fraction))))
                    (+ eye (* (- target eye) fraction))))))

(defun large-world-spawn-view (source x y)
  "Return the yaw from road cell X,Y toward a summit it can see, or NIL.

The surroundings within *LARGE-WORLD-SPAWN-CLEARING* must be open, and a
peak above the alpine snow line must stand between *LARGE-WORLD-SPAWN-
SUMMIT-DISTANCE* and *LARGE-WORLD-SPAWN-VIEW-DISTANCE* in plain sight, so
the first view is across open land to a peak with sky above it."
  (let* ((reach *large-world-spawn-view-distance*)
         (clearing *large-world-spawn-clearing*)
         (ground (large-world-terrain-height source x y))
         (eye (+ ground 1.6))
         (summit-height (* *alpine-snow-line* *large-world-relief-height*))
         (summits nil))
    (loop for sx from (- x reach) to (+ x reach) by 4
          do (loop for sy from (- y reach) to (+ y reach) by 4
                   for distance = (sqrt (+ (expt (- sx x) 2) (expt (- sy y) 2)))
                   for height = (large-world-terrain-height source sx sy)
                   do (cond ((<= distance clearing)
                             (when (> height (+ ground 4))
                               (return-from large-world-spawn-view nil)))
                            ((and (<= *large-world-spawn-summit-distance*
                                      distance reach)
                                  (>= height summit-height))
                             (push (list height sx sy) summits)))))
    (loop for (height sx sy) in (subseq (sort summits #'> :key #'first)
                                        0 (min 8 (length summits)))
          when (large-world-summit-visible-p source x y eye sx sy height)
            return (coerce (atan (- sy y) (- sx x)) 'single-float))))

(defun large-world-scenic-spawn (source)
  "Return the road X and Y a player starts at, and the yaw facing a summit.

Candidates run along the road from well inside the corner; the first that
opens onto a snow-line peak (see LARGE-WORLD-SPAWN-VIEW) wins.  A world too
small for the search falls back to the road's first cell with yaw 0."
  (let* ((domain (world-source-domain source))
         (x-limit (luft:world-domain-x-limit domain))
         (y-limit (luft:world-domain-y-limit domain))
         (reach *large-world-spawn-view-distance*)
         (key (list (authored-world-source-seed source) x-limit y-limit)))
    (values-list
     (or (gethash key *large-world-scenic-spawns*)
         (setf (gethash key *large-world-scenic-spawns*)
               (or (loop for x from 192 below (- x-limit 192) by 8
                         for y = (round (large-world-road-centre-y x))
                         for yaw = (and (< reach y (- y-limit reach))
                                        (large-world-spawn-view source x y))
                         when yaw return (list x y yaw))
                   (list +large-world-spawn-x+
                         (round (large-world-road-centre-y +large-world-spawn-x+))
                         0.0)))))))

(defun large-world-column-placement (source x y z height slope)
  "Return the source placement of cell X,Y,Z in a column HEIGHT cells tall.

The surface reads altitude and steepness as Luvcraft's alpine relief does:
slate cliffs, snow over rock on gentle high ground, bare rock above the tree
line and on steps, sand at the shore, and earth (grass-topped) elsewhere.
SOURCE names the world the column belongs to; this rule reads only the
column itself."
  (declare (ignore source))
  (let* ((top (1- height))
         (depth (- top z))
         (level (/ height *large-world-relief-height*)))
    (cond
      ((and (= depth 0)
            (<= (abs (- y (large-world-road-centre-y x))) 3.0d0))
       *sanctuary-material-placement*)
      ((and (= depth 0)
            (<= (abs (- x (large-world-river-centre-x y))) 8.0d0))
       *highland-rock-material-placement*)
      ((> depth 3) *terrain-material-placement*)
      ((>= slope 4) *slate-material-placement*)
      ((and (>= level *alpine-snow-line*) (<= slope 2))
       (if (= depth 0)
           *snow-material-placement*
           *highland-rock-material-placement*))
      ((or (>= level *alpine-tree-line*) (>= slope 2))
       *highland-rock-material-placement*)
      ((<= level *alpine-beach-level*) *sand-material-placement*)
      (t *terrain-material-placement*))))

(defun large-world-citadel-cell-p (x y z)
  "Whether X/Y/Z is authored limestone in the eastern destination."
  (let* ((dx (- x 1500))
         (dy (- y 650))
         (square-distance (max (abs dx) (abs dy)))
         (corner-distance
           (min (sqrt (+ (expt (- dx 34) 2) (expt (- dy 34) 2)))
                (sqrt (+ (expt (+ dx 34) 2) (expt (- dy 34) 2)))
                (sqrt (+ (expt (- dx 34) 2) (expt (+ dy 34) 2)))
                (sqrt (+ (expt (+ dx 34) 2) (expt (+ dy 34) 2)))))
         (gate-p (and (< dx -32) (<= (abs dy) 3) (<= z 29))))
    (or
     ;; Long curtain walls, with an open road gate on the west.
     (and (<= 34 square-distance 38) (<= 24 z 34) (not gate-p))
     ;; Four round towers break the square silhouette.
     (and (<= corner-distance 8.0d0) (<= 24 z 40))
     ;; A keep and stair-stepped beacon at the destination.
     (and (<= 8 dx 26) (<= (abs dy) 13) (<= 24 z 38)
          (or (<= (abs dy) 9) (<= z 31)))
     (and (<= 13 dx 21) (<= (abs dy) 5) (<= 39 z 47))
     ;; Sparse crenels remain ordinary cells.
     (and (<= 34 square-distance 38) (= z 36)
          (evenp (+ x y))))))

(defun large-world-base-placement
    (source x y z &key (height (large-world-terrain-height source x y)))
  "Return the source-owned placement at one cell, or NIL for authored air."
  (cond
    ((large-world-citadel-cell-p x y z) *sanctuary-material-placement*)
    ((>= z height) nil)
    (t (large-world-column-placement
        source x y z height
        (large-world-surface-slope source x y height)))))

(defmethod generate-world-source-chunk
    ((source authored-world-source) key place)
  (let* ((domain (world-source-domain source))
         (x0 (luft:chunk-origin-x key))
         (y0 (luft:chunk-origin-y key))
         (x1 (min (+ x0 luft:+chunk-size+)
                  (luft:world-domain-x-limit domain)))
         (y1 (min (+ y0 luft:+chunk-size+)
                  (luft:world-domain-y-limit domain))))
    (loop for x from x0 below x1 do
      (loop for y from y0 below y1 do
        (let* ((height (large-world-terrain-height source x y))
               (slope (large-world-surface-slope source x y height)))
          (dotimes (z height)
            (funcall place x y z
                     (large-world-column-placement
                      source x y z height slope)))
          (when (and (<= (abs (- x 1500)) 45)
                     (<= (abs (- y 650)) 45))
            (loop for z from 24 to 47
                  when (large-world-citadel-cell-p x y z)
                    do (funcall place x y z
                                *sanctuary-material-placement*))))))))

(defun make-authored-world-streaming-scene
    (&key (horizontal-bits +large-world-horizontal-bits+)
      (seed +large-world-seed+) (frames-per-load 1) (residency-radius 1)
      mesh-preparer)
  "Make the canonical large demand world without materializing any chunk."
  (let* ((domain (luft:make-world-domain
                  :x-bits horizontal-bits :y-bits horizontal-bits))
         (source (make-instance 'authored-world-source
                                :domain domain :seed seed))
         (builder (make-world-scene-builder :horizontal-bits horizontal-bits))
         ;; Regional voxel light is deliberately not begun by this stage.
         (empty (finish-scene-builder
                 builder :player-p t :voxel-light-propagation-p nil)))
    (make-instance
     'streaming-scene
     :source source
     :solid (luft:make-fiber-store domain)
     :material-vocabulary (scene-material-vocabulary empty)
     :material-cells (make-material-store)
     :authored-light-sources #()
     :authored-light-opacity-table
     (scene-authored-light-opacity-table empty)
     :authored-light-revision 0
     :authored-light-provenance (scene-authored-light-provenance empty)
     :authored-light-generation (scene-authored-light-generation empty)
     :content-revision 0
     :voxel-light-propagation-p nil
     :light-generation (scene-authored-light-generation empty)
     :attachments #()
     :player-p t
     :frames-per-load frames-per-load
     :residency-radius residency-radius
     :mesh-preparer mesh-preparer)))
