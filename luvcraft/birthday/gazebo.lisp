;;; The gazebo: a big beautiful pavilion at the heart of the party.
;;;
;;; Contract:
;;;   (BUILD-BIRTHDAY-GAZEBO world &key center-x center-z ground-y radius)
;;;   builds the gazebo out of block edits (LUVCRAFT:EDIT-BLOCK-AT inside one
;;;   LUVCRAFT::WITH-WORLD-CHANGE-TRANSACTION; the caller relights) and
;;;   returns a plist describing the interesting anchor points for the other
;;;   decorations:
;;;     (:floor-y N                    ; the y of the floor slab blocks
;;;      :roof-apex (x y z)            ; above the finial, where a balloon
;;;                                    ;   bundle ties
;;;      :post-tops ((x y z) ...)      ; topmost wood block of each post,
;;;                                    ;   for balloon strings
;;;      :cake (x y z))                ; center block of the cake's base tier
;;;
;;; Construction is octagons all the way up.  One membership predicate -- a
;;; square chamfered at |dx|+|dz| <= round(R*sqrt2) -- draws the floor slab,
;;; the railing ring, and every roof tier, so the plan stays concentric by
;;; construction.  The eight posts stand where the octagon's vertices land:
;;; computed from the circumradius R/cos(pi/8) at the eight half-step angles
;;; and rounded onto the grid, rather than placed by hand, so a different
;;; RADIUS keeps its corners true.
;;;
;;; Vertically: a one-block plinth lifts the floor above the meadow (stone
;;; brick rim, planks field, cobblestone pads under the posts); five-block
;;; wood posts carry a stack of slate octagons that starts one cell wider
;;; than the floor -- a real eave -- and shrinks by one and two alternately
;;; to a cap, crowned with a glowing crystal finial.  Crystal garland lights
;;; hang under the eave at the rim-edge midpoints, so the covered interior
;;; glows warm instead of going dark under the solid roof.  Two flat sides
;;; (toward -z and +z) are left entirely open between their posts, with a
;;; sandstone path through the rim and a sandstone step out onto the grass,
;;; so a four-year-old can run straight in, past the cake, and out the other
;;; side.
;;;
;;; Blocks are the existing palette only: stone bricks and cobblestone for
;;; the base, planks for floor and railings, wood for posts, slate for the
;;; tiered roof, crystal for the finial and garland lights, snow for the
;;; cake's frosting.

(in-package #:luvcraft.birthday)

;;; The octagon vocabulary

(defun octagon-diagonal-limit (radius)
  "The |dx|+|dz| bound that chamfers a square of half-width RADIUS into a
near-regular octagon."
  (round (* radius (sqrt 2d0))))

(defun octagon-contains-p (dx dz radius)
  (and (<= (abs dx) radius)
       (<= (abs dz) radius)
       (<= (+ (abs dx) (abs dz)) (octagon-diagonal-limit radius))))

(defun octagon-rim-p (dx dz radius)
  "True on the one-cell boundary ring of the filled octagon."
  (and (octagon-contains-p dx dz radius)
       (not (and (octagon-contains-p (1+ dx) dz radius)
                 (octagon-contains-p (1- dx) dz radius)
                 (octagon-contains-p dx (1+ dz) radius)
                 (octagon-contains-p dx (1- dz) radius)))))

(defun map-octagon (function radius)
  "Call FUNCTION with (dx dz rim-p) over every cell of the filled octagon."
  (loop for dx from (- radius) to radius
        do (loop for dz from (- radius) to radius
                 when (octagon-contains-p dx dz radius)
                   do (funcall function dx dz
                               (octagon-rim-p dx dz radius)))))

(defun octagon-vertex-offsets (radius)
  "The eight vertex cells of the octagon, as (dx dz) pairs.

The vertices of the ideal octagon sit on the circumradius R/cos(pi/8) at
the eight half-step angles; rounding those puts each post exactly on the
rim ring where the flat sides meet the chamfers."
  (let ((circumradius (/ radius (cos (/ pi 8)))))
    (loop for k below 8
          for angle = (* (1+ (* 2 k)) (/ pi 8))
          collect (list (round (* circumradius (cos angle)))
                        (round (* circumradius (sin angle)))))))

(defun octagon-edge-midpoint-offsets (radius)
  "One cell near the middle of each of the eight rim edges.

The axis-aligned midpoints land on the rim directly; the diagonal ones
round to just outside the chamfer, so walk inward until the octagon holds
the cell -- one step, onto the row under the eave."
  (loop for k below 8
        for angle = (* k (/ pi 4))
        collect (let ((dx (round (* radius (cos angle))))
                     (dz (round (* radius (sin angle)))))
                 (loop until (octagon-contains-p dx dz radius)
                       do (decf dx (signum dx))
                          (decf dz (signum dz)))
                 (list dx dz))))

(defun gazebo-roof-radii (radius)
  "Tier radii from eave to cap: one wider than the floor, then shrinking
by one and two alternately so the silhouette curves instead of coning."
  (let ((radii nil)
        (r (1+ radius))
        (drop 1))
    (loop while (plusp r)
          do (push r radii)
             (decf r drop)
             (setf drop (if (= drop 1) 2 1)))
    (nreverse radii)))

;;; Reading the ground

(defun probe-gazebo-ground (world center-x center-z radius)
  "The highest solid surface y among five columns around the center.

Taking the maximum keeps the plinth proud of a gently uneven clearing
instead of half-buried in it.  Absent chunks read as air, so a wholly
unmaterialized site errors here, by name, rather than as a mysterious
CHUNK-NOT-RESIDENT mid-build."
  (let* ((shape (luvcraft::voxel-space-chunk-shape
                 (luvcraft::block-world-space world)))
         (top (1- (* 2 (luvcraft::chunk-shape-height shape))))
         (half (ceiling radius 2))
         (ground nil))
    (dolist (offset (list (list 0 0)
                          (list half 0) (list (- half) 0)
                          (list 0 half) (list 0 (- half))))
      (loop with x = (+ center-x (first offset))
            with z = (+ center-z (second offset))
            for y from top downto 0
            when (luvcraft:world-block-at world x y z)
              do (setf ground (if ground (max ground y) y))
                 (return)))
    (or ground
        (error "No solid ground near (~D, ~D); is the terrain resident?"
               center-x center-z))))

;;; The build

(defun build-birthday-gazebo (world &key (center-x 0) (center-z 0)
                                         ground-y (radius 7))
  "Build the party gazebo centered on CENTER-X, CENTER-Z and return its
anchor plist (see the file header).  GROUND-Y is the y of the surface
block to stand on; when NIL the terrain is probed for it.  All edits go
through the world's source overlay in one change transaction; the caller
relights."
  (let* ((ground (or ground-y
                     (probe-gazebo-ground world center-x center-z radius)))
         (floor-y (1+ ground))
         (post-height 5)
         (roof-base-y (+ floor-y post-height 1))
         (roof-radii (gazebo-roof-radii radius))
         (posts (octagon-vertex-offsets radius))
         ;; The flat side at |dz| = RADIUS spans the cells between two
         ;; posts; everything strictly between them is entrance.
         (opening-half (max 1 (- (octagon-diagonal-limit radius) radius 1))))
    (flet ((place (block-kind dx y dz)
             (luvcraft:edit-block-at block-kind world
                                     (+ center-x dx) y (+ center-z dz)))
           (air-p (dx y dz)
             (multiple-value-bind (block state)
                 (luvcraft:world-block-at world
                                          (+ center-x dx) y (+ center-z dz))
               (and (null block) (eq state :resident))))
           (occupied-p (dx y dz)
             (and (luvcraft:world-block-at world
                                           (+ center-x dx) y (+ center-z dz))
                  t))
           (post-p (dx dz)
             (member (list dx dz) posts :test #'equal))
           (path-p (dx dz)
             (and (<= (abs dx) 1) (<= (- radius 2) (abs dz) radius)))
           (entrance-p (dx dz)
             (and (= (abs dz) radius) (<= (abs dx) opening-half))))
      (luvcraft::with-world-change-transaction (world)
        ;; Clear the interior air band first, so a stray shrub or terrain
        ;; bump inside the footprint does not end up furnishing the party.
        (map-octagon
         (lambda (dx dz rim-p)
           (declare (ignore rim-p))
           (loop for y from (1+ floor-y) below roof-base-y
                 when (occupied-p dx y dz)
                   do (place nil dx y dz)))
         radius)
        ;; The plinth: stone brick rim, planks field, sandstone where the
        ;; path runs through, cobblestone pads under the posts, and a
        ;; cobblestone underpinning wherever the rim overhangs a dip.
        (map-octagon
         (lambda (dx dz rim-p)
           (place (cond ((post-p dx dz) luvcraft::*cobblestone-block*)
                        ((path-p dx dz) luvcraft::*sandstone-block*)
                        (rim-p luvcraft::*stone-bricks-block*)
                        (t luvcraft::*planks-block*))
                  dx floor-y dz)
           (when rim-p
             (loop for y from (1- floor-y) downto (- floor-y 4)
                   while (air-p dx y dz)
                   do (place luvcraft::*cobblestone-block* dx y dz))))
         radius)
        ;; Sandstone steps: a flush landing two cells deep outside each
        ;; entrance, underpinned where the meadow falls away.
        (dolist (side '(-1 1))
          (loop for dz from (1+ radius) to (+ radius 2)
                do (loop for dx from -1 to 1
                         do (place luvcraft::*sandstone-block*
                                   dx floor-y (* side dz))
                            (loop for y from (1- floor-y)
                                    downto (- floor-y 3)
                                  while (air-p dx y (* side dz))
                                  do (place luvcraft::*sandstone-block*
                                            dx y (* side dz))))))
        ;; Posts and railings.  Railings run one block high along the rim
        ;; between posts; the two entrance sides stay completely open.
        (dolist (post posts)
          (loop for y from (1+ floor-y) to (+ floor-y post-height)
                do (place luvcraft::*wood-block* (first post) y (second post))))
        (map-octagon
         (lambda (dx dz rim-p)
           (when (and rim-p
                      (not (post-p dx dz))
                      (not (entrance-p dx dz)))
             (place luvcraft::*planks-block* dx (1+ floor-y) dz)))
         radius)
        ;; The roof: filled slate octagons from the eave up, so there is
        ;; no hole to rain through, then the crystal finial on the cap.
        (loop for tier-radius in roof-radii
              for y from roof-base-y
              do (map-octagon
                  (lambda (dx dz rim-p)
                    (declare (ignore rim-p))
                    (place luvcraft::*slate-block* dx y dz))
                  tier-radius))
        (place luvcraft:*crystal-block*
               0 (+ roof-base-y (length roof-radii)) 0)
        ;; Garland lights tucked under the eave: a crystal hanging from
        ;; the overhang at each rim-edge midpoint, clear of the posts.
        (dolist (offset (octagon-edge-midpoint-offsets (first roof-radii)))
          (unless (post-p (first offset) (second offset))
            (place luvcraft:*crystal-block*
                   (first offset) (1- roof-base-y) (second offset))))
        ;; The cake on its table: a low 3x3 planks table, a full 3x3 snow
        ;; base tier (a clean white box reads as frosting; a cut-corner
        ;; tier read as a grey lump), a single snow top tier, one crystal
        ;; candle whose glow lights the party.
        (loop for dx from -1 to 1
              do (loop for dz from -1 to 1
                       do (place luvcraft::*planks-block*
                                 dx (1+ floor-y) dz)
                          (place luvcraft::*snow-block*
                                 dx (+ floor-y 2) dz)))
        (place luvcraft::*snow-block* 0 (+ floor-y 3) 0)
        (place luvcraft:*crystal-block* 0 (+ floor-y 4) 0)))
    (list :floor-y floor-y
          :roof-apex (list center-x
                           (+ roof-base-y (length roof-radii) 1)
                           center-z)
          :post-tops (loop for (dx dz) in posts
                           collect (list (+ center-x dx)
                                         (+ floor-y post-height)
                                         (+ center-z dz)))
          :cake (list center-x (+ floor-y 2) center-z))))
