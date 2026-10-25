;;; The Hollow Walk: an opt-in, finite composition, not a new generator.
;;; Surveyed on seed 7, alpine, height 64. See #DLH49J.

(in-package #:luvcraft)

(defun make-first-journey-world ()
  "Make a fresh Hollow Walk; never read or write a player's save.

Return WORLD, CAMERA, PLAYER, and a review plist. All coordinates in that
plist are Luvcraft X/Y/Z foot cells (Y up): :ANCHORS names destinations,
:ROUTE visits the cave and returns by the eastern trail, and :FOOT-CELLS
samples the entire route at cell spacing. The source remains ordinary
version-1 seed-7 alpine terrain; the composition is only sparse edits.
Save with MAKE-LUVCRAFT-SAVE-DESCRIPTION and an explicit new pathname.
Reload that save normally, without calling this constructor again."
  (let* ((world (make-empty-little-block-world :seed 7 :chunk-height 64))
         (source (block-world-source world))
         ;; Survey: grove surface 12, ruin 12--13, natural cave air 8--11
         ;; beneath rock at 12--13. The return crosses a ravine at Y=2.
         (route '((-48 12 8) (-56 13 24) (-48 13 40) (-32 14 56)
                  (-32 12 44) (-23 9 44) (-23 8 54) (-23 8 62) (-23 8 72)
                  (-23 8 62) (-23 8 54) (-23 9 44) (-32 12 44) (-32 14 56)
                  (-40 11 80) (-8 11 80) (8 11 88) (16 13 96) (24 14 98) (24 14 104)
                  (24 14 112) (40 14 112) (48 13 96) (48 13 80)
                  (40 12 48) (8 12 24) (-16 12 16) (-48 12 8)))
         (corridor (make-hash-table :test #'equal))
         (centres nil))
    (center-little-world-residency source world 0 4 :radius 4)
    (labels ((place (name x y z)
               (let ((block (and name (block-kind-named name))))
                 (unless (eq block (world-block-at world x y z))
                   (edit-block-at block world x y z))))
             (floor-column (name x foot z &optional bridge-p)
               (place name x (1- foot) z)
               (unless bridge-p
                 (loop for y downfrom (- foot 2) above 0
                       while (null (world-block-at world x y z))
                       do (place :stone x y z))))
             (clear-column (x foot z top)
               (loop for y from foot below top do (place nil x y z)))
             (bridge-p (x z)
               (and (<= -29 x -3) (<= 12 z 22))))
      ;; A three-cell-wide supercover ribbon, graded between surveyed levels.
      ;; Taking the higher overlapping tread prevents gaps at diagonal bends.
      (loop for (a b) on route while b do
        (let ((steps (max (abs (- (first b) (first a)))
                          (abs (- (third b) (third a))))))
          (loop for step from 0 to steps
                for point = (mapcar (lambda (lo hi)
                                      (round (+ lo (* (/ step steps) (- hi lo))))) a b)
                do (push point centres)
                   (destructuring-bind (x foot z) point
                     (loop for dx from -1 to 1 do
                       (loop for dz from -1 to 1
                             for key = (list (+ x dx) (+ z dz))
                             do (setf (gethash key corridor)
                                      (max foot (gethash key corridor 0)))))))))
      (with-world-change-transaction (world)
        ;; Seven Crowns: a grassy clearing, flowers in the ground, tall
        ;; asymmetric crowns. Leave the surrounding ravine and wilderness.
        (loop for x from -55 to -41 do
          (loop for z from 33 to 47
                when (<= (+ (expt (- x -48) 2) (expt (- z 40) 2)) 42)
                  do (floor-column (if (zerop (mod (+ x (* 3 z)) 11)) :flowers :grass)
                                   x 13 z)
                     (clear-column x 13 z 24)))
        (dolist (tree '((-55 35 7 3) (-53 44 6 3) (-48 47 8 3) (-42 32 6 2)
                        (-48 30 7 3) (-57 40 5 2) (-43 39 7 3)))
          (destructuring-bind (x z trunk radius) tree
            (let* ((foot (max 13 (1+ (little-world-surface-height source x z 64))))
                   (crown (+ foot trunk)))
              (floor-column :moss x foot z)
              (loop for y from foot below crown do (place :wood x y z))
              (loop for dy from 0 to 2 do
                (loop for dx from (- radius) to radius do
                  (loop for dz from (- radius) to radius
                        when (<= (+ (abs dx) (abs dz)) (- (1+ radius) dy))
                          do (place :leaves (+ x dx) (+ crown dy) (+ z dz))))))))
        (loop for x from -55 to -52 do (place :wood x 13 39))
        ;; Split-Mouth: supported four-cell-high air, a real chamber and
        ;; roof, and crystals embedded in its walls rather than in the route.
        (loop for x from -27 to -19 do
          (loop for z from 67 to 76 do
            (floor-column :stone x 8 z)
            (loop for y from 8 to 12 do
              (place (cond ((= y 12) :stone)
                           ((or (= x -27) (= x -19) (= z 76)) :slate)
                           (t nil))
                     x y z))))
        (loop for x from -26 to -20 do
          (loop for z from 60 to 66 do
            (floor-column :stone x 8 z)
            (loop for y from 8 to 13
                  do (place (cond ((>= y 12) :slate)
                                  ((or (= x -26) (= x -20)) :stone)
                                  (t nil))
                            x y z))))
        (place :stone -26 14 61)
        (place :slate -20 14 63)
        (place :crystal -27 9 71)
        (place :crystal -19 10 74)
        ;; Watchman's Rest: two open doorways, broken courses, one surviving
        ;; tall corner and a half lintel. The interior is deliberately empty.
        (loop for x from 19 to 29 do
          (loop for z from 100 to 108 do
            (floor-column :mossy-stone-bricks x 14 z)
            (clear-column x 14 z 24)
            (when (and (or (= x 19) (= x 29) (= z 100) (= z 108))
                       (not (and (<= 23 x 25) (or (= z 100) (= z 108)))))
              (let ((height (if (and (>= x 27) (>= z 106)) 6
                                (1+ (mod (+ x (* 3 z)) 3)))))
                (loop for y from 14 below (+ 14 height)
                      do (place (if (evenp (+ x y z)) :stone-bricks :cracked-stone-bricks)
                                x y z))))))
        (dolist (x '(22 26))
          (loop for y from 14 to 18 do (place :stone-bricks x y 100)))
        (loop for x from 22 to 24 do (place :cracked-stone-bricks x 18 100))
        ;; Dress the ribbon last so neither authored nor generated landmarks
        ;; block it. The hidden grove road still has clear supported ground.
        (maphash
         (lambda (key foot)
           (destructuring-bind (x z) key
             (floor-column (cond ((bridge-p x z) :planks)
                                 ((and (<= -54 x -42) (<= 34 z 46)) :grass)
                                 ((and (>= x 30) (not (zerop (mod (+ x z) 4)))) :dirt)
                                 (t :gravel))
                           x foot z (bridge-p x z))
             ;; The descending approach is open sky until the slate brow,
             ;; rather than an accidental tunnel hiding the cave entrance.
             (clear-column x foot z
                           (if (and (<= -33 x -22) (<= 43 z 59)) 64 (+ foot 4)))))
         corridor)
        (loop for x from -25 to -21 do
          (loop for z from 54 to 59 do
            (floor-column :gravel x 8 z)
            (clear-column x 8 z 64)))
        ;; Sparse bridge piers and trailhead waymarks, outside the walking lane.
        (dolist (xz '((-24 15) (-16 16) (-8 19)))
          (destructuring-bind (x z) xz
            (loop for y downfrom 10 above 0
                  while (null (world-block-at world x y z))
                  do (place :wood x y z))))
        (dolist (x '(-51 -45))
          (floor-column :stone x 12 8)
          (place :mossy-stone-bricks x 12 8)
          (place :stone x 13 8)))
      (let* ((position (make-vec3 -47.5d0 12d0 8.5d0))
             (camera (make-instance 'fly-camera :yaw (atan -8 16) :pitch -0.08))
             (player (make-instance 'block-world-player :position position)))
        (values world camera player
                (list :anchors '((:trailhead -48 12 8) (:seven-crowns -48 13 40)
                                 (:split-mouth -23 8 62) (:cave-chamber -23 8 72)
                                 (:watchmans-rest 24 14 104))
                      :route (copy-tree route)
                      :foot-cells
                      (mapcar (lambda (point)
                                (destructuring-bind (x foot z) point
                                  (declare (ignore foot))
                                  (list x (gethash (list x z) corridor) z)))
                              (nreverse centres))))))))
