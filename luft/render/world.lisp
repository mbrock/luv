(in-package #:luft.render)

;;; The authored large world: value-noise landscape readings, the
;;; deterministic terrain and citadel source, its sparse semantic edits, and
;;; the immutable resident chunk values a streaming scene materializes.

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
(defconstant +authored-world-gameplay-radius+ 1)
(defconstant +large-world-spawn-x+ 32)

(defclass authored-world-source ()
  ((domain :initarg :domain :reader authored-world-source-domain)
   (seed :initarg :seed :initform +large-world-seed+
         :reader authored-world-source-seed)
   ;; Presence is meaningful: NIL is an authored removal, while absence means
   ;; that the deterministic source still owns the cell.
   (edits :initform (make-hash-table :test #'eql)
          :reader authored-world-source-edits))
  (:documentation
   "The canonical large-world description and its sparse semantic edits."))

(defstruct (resident-cell-chunk
             (:constructor %make-resident-cell-chunk
                 (key incarnation fibers material-cells))
             (:copier nil))
  "One immutable, evictable materialization of an authored source chunk."
  (key 0 :type luft:chunk-key :read-only t)
  (incarnation 0 :type (integer 1 *) :read-only t)
  (fibers nil :type luft:chunk-fibers :read-only t)
  (material-cells nil :type hash-table :read-only t))

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

(defun large-world-terrain-height (source x y)
  "Deterministic composed terrain under the route, river, pass, and citadel."
  (let* ((seed (authored-world-source-seed source))
         (detail (+ (* 3.2d0 (landscape-value-noise x y 97 seed 2))
                    (* 1.5d0 (landscape-value-noise x y 31 seed 3))))
         (road-y (large-world-road-centre-y x))
         (road-distance (abs (- y road-y)))
         (river-x (large-world-river-centre-x y))
         (river-distance (abs (- x river-x)))
         (ridge-distance (abs (- x 970.0d0)))
         (pass-distance (abs (- y (large-world-road-centre-y 970))))
         (ridge (* 31.0d0
                   (max 0.0d0 (- 1.0d0 (/ ridge-distance 260.0d0)))
                   (min 1.0d0 (/ pass-distance 95.0d0))))
         (highlands (* 9.0d0
                       (max 0.0d0
                            (landscape-value-noise x y 311 seed 11))))
         (natural (+ 17.0d0 detail ridge highlands))
         (river-bed (- natural
                       (* 12.0d0
                          (expt (max 0.0d0
                                     (- 1.0d0 (/ river-distance 18.0d0)))
                                2))))
         (road-height (large-world-road-height x))
         ;; Keep the masonry road itself level enough to walk and feather its
         ;; broad verge into natural terrain. The former seven-cell linear cut
         ;; produced a narrow stepped trench which looked and played like a
         ;; river gorge at the default spawn.
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
  (let* ((top (1- height))
         (road-p (<= (abs (- y (large-world-road-centre-y x))) 3.0d0))
         (river-p
           (<= (abs (- x (large-world-river-centre-x y))) 8.0d0)))
    (cond
      ((large-world-citadel-cell-p x y z)
       *sanctuary-material-placement*)
      ((>= z height) nil)
      ((and (= z top) road-p) *sanctuary-material-placement*)
      ((and (= z top) river-p) *highland-rock-material-placement*)
      ((and (>= z (- top 3))
            (or (> height 31)
                (>= (abs (- height
                            (large-world-terrain-height source (1+ x) y)))
                    2)))
       *highland-rock-material-placement*)
      (t *terrain-material-placement*))))

(defun authored-world-edit-at (source cell)
  "Return a sparse edited placement and whether SOURCE owns an edit at CELL."
  (gethash cell (authored-world-source-edits source)))

(defun capture-authored-world-chunk-edits (source key)
  "Copy the sparse edits belonging to KEY for immutable worker use."
  (let ((edits nil))
    (maphash
     (lambda (cell placement)
       (when (= key (luft:site-chunk-key cell))
         (push (cons cell placement) edits)))
     (authored-world-source-edits source))
    edits))

(zdefun (materialize-authored-world-chunk :zone :luft/materialize-source-chunk
                                          :value key)
    (source key incarnation &key edits)
  "Build KEY bit-identically from SOURCE and an immutable sparse edit capture."
  (check-type source authored-world-source)
  (let* ((domain (authored-world-source-domain source))
         (x0 (luft:chunk-origin-x key))
         (y0 (luft:chunk-origin-y key))
         (x1 (min (+ x0 luft:+chunk-size+)
                  (luft:world-domain-x-limit domain)))
         (y1 (min (+ y0 luft:+chunk-size+)
                  (luft:world-domain-y-limit domain)))
         (vocabulary (make-scene-material-vocabulary))
         (terrain-offset
           (domains:identity-vocabulary-offset
            vocabulary *terrain-material-placement*))
         (rock-offset
           (domains:identity-vocabulary-offset
            vocabulary *highland-rock-material-placement*))
         (limestone-offset
           (domains:identity-vocabulary-offset
            vocabulary *sanctuary-material-placement*))
         (materials (make-hash-table :test #'eql :size 100000)))
    (zone :luft/generate-source-materials
      (loop for x from x0 below x1 do
        (loop for y from y0 below y1 do
          (let* ((height (large-world-terrain-height source x y))
                 (top (1- height))
                 (road-p
                   (<= (abs (- y (large-world-road-centre-y x))) 3.0d0))
                 (river-p
                   (<= (abs (- x (large-world-river-centre-x y))) 8.0d0))
                 (rock-p
                   (or (> height 31)
                       (>= (abs (- height
                                   (large-world-terrain-height source (1+ x) y)))
                           2))))
            (dotimes (z height)
              (setf (gethash
                     (luft:make-site domain x y z luft:+cell-extent+ 1)
                     materials)
                    (cond ((and (= z top) road-p) limestone-offset)
                          ((and (= z top) river-p) rock-offset)
                          ((and rock-p (>= z (- top 3))) rock-offset)
                          (t terrain-offset))))
            (when (and (<= (abs (- x 1500)) 45)
                       (<= (abs (- y 650)) 45))
              (loop for z from 24 to 47
                    when (large-world-citadel-cell-p x y z)
                      do (setf (gethash
                                (luft:make-site
                                 domain x y z luft:+cell-extent+ 1)
                                materials)
                               limestone-offset)))))))
    (zone :luft/replay-source-edits
      (dolist (edit edits)
        (if (cdr edit)
            (setf (gethash (car edit) materials)
                  (domains:identity-vocabulary-offset vocabulary (cdr edit)))
            (remhash (car edit) materials))))
    (let ((fibers (luft:make-chunk-fibers domain key)))
      (zone :luft/assemble-source-fibers
        (maphash (lambda (cell offset)
                   (declare (ignore offset))
                   (setf (luft:fibers-cell-bit
                          fibers (luft:site-x cell) (luft:site-y cell)
                          (luft:site-z cell))
                         1))
                 materials))
      (%make-resident-cell-chunk key incarnation fibers materials))))

(defclass authored-chunk-load-request (production:production-request)
  ((scene :initarg :scene :reader authored-chunk-load-request-scene)
   (source :initarg :source :reader authored-chunk-load-request-source)
   (chunk-key :initarg :chunk-key :reader authored-chunk-load-request-chunk-key)
   (demand-token :initarg :demand-token
                 :reader authored-chunk-load-request-demand-token)
   (incarnation :initarg :incarnation
                :reader authored-chunk-load-request-incarnation)
   (edits :initarg :edits :reader authored-chunk-load-request-edits)))

(zdefmethod (production:perform-production-request
             :zone :luft/produce-source-chunk)
    ((request authored-chunk-load-request))
  (materialize-authored-world-chunk
   (authored-chunk-load-request-source request)
   (authored-chunk-load-request-chunk-key request)
   (authored-chunk-load-request-incarnation request)
   :edits (authored-chunk-load-request-edits request)))
