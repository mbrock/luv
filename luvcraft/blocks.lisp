;;; The semantic block vocabulary and its procedural texture atlas.
;;;
;;; Blocks and faces are ordinary CLOS objects so the interesting semantic
;;; choices stay inspectable and redefinable at the REPL.  The material
;;; palette here is what the player places, what the mesher shades, and what
;;; the shader lab browses; the atlas is generated arithmetic, not an asset.

(in-package #:luvcraft)

(defclass block-kind ()
  ((name :initarg :name :reader block-kind-name)
   (face-tiles :initarg :face-tiles :reader block-kind-face-tiles)
   (categories :initarg :categories :initform nil
               :reader block-kind-categories)
   (display-color :initarg :display-color :initform '(0.5 0.5 0.5)
                  :reader block-kind-display-color)
   (placeable-p :initarg :placeable-p :initform t
                :reader block-kind-placeable-p)
   ;; Light behavior is distinct from collision: opacity is how much a cell
   ;; costs propagating light, emission seeds the propagated blocklight
   ;; field, and surface emission is the material's own visible radiance.
   ;; One glowing crystal may configure both emissions, but they are
   ;; different facts.
   (light-opacity :initarg :light-opacity :initform 15
                  :type (integer 0 15)
                  :quantity (:quantity :block-light-attenuation-step
                             :unit :one)
                  :reader block-kind-light-opacity)
   (light-emission :initarg :light-emission :initform 0
                   :type (integer 0 15)
                   :quantity (:quantity :block-light-emission-step
                              :unit :one)
                   :reader block-kind-light-emission)
   (surface-emission :initarg :surface-emission :initform 0.0
                     :type real
                     :quantity (:quantity :material-emission :unit :one)
                     :reader block-kind-surface-emission))
  (:metaclass luv.arithmetic.records:quantity-class))

(defmethod shared-initialize :after
    ((block block-kind) slot-names &key)
  (declare (ignore slot-names))
  (unless (keywordp (block-kind-name block))
    (error "A block kind name must be a keyword, not ~S."
           (block-kind-name block)))
  (let ((tiles (block-kind-face-tiles block)))
    (unless (and (listp tiles) (evenp (length tiles)))
      (error "Block kind ~S has invalid face tiles ~S."
             (block-kind-name block) tiles)))
  (unless (every #'keywordp (block-kind-categories block))
    (error "Block kind ~S has invalid categories ~S."
           (block-kind-name block) (block-kind-categories block)))
  (unless (and (= 3 (length (block-kind-display-color block)))
               (every (lambda (component)
                        (and (realp component) (<= 0 component 1)))
                      (block-kind-display-color block)))
    (error "Block kind ~S has invalid display color ~S."
           (block-kind-name block) (block-kind-display-color block))))

(defgeneric block-solid-p (block))
(defgeneric block-face-tile (block face))

(defgeneric block-light-opacity (block)
  (:documentation "How strongly BLOCK attenuates propagated light, 0..15."))
(defgeneric block-light-emission (block)
  (:documentation "The blocklight level BLOCK seeds into its own cell, 0..15."))
(defgeneric block-surface-emission (block)
  (:documentation "BLOCK's own linear material radiance, independent of
propagated light."))

(defmethod block-solid-p ((block null)) nil)
(defmethod block-solid-p ((block block-kind)) t)

(defmethod block-light-opacity ((block null)) 0)
(defmethod block-light-emission ((block null)) 0)
(defmethod block-surface-emission ((block null)) 0.0)

(defmethod block-light-opacity ((block block-kind))
  (block-kind-light-opacity block))
(defmethod block-light-emission ((block block-kind))
  (block-kind-light-emission block))
(defmethod block-surface-emission ((block block-kind))
  (block-kind-surface-emission block))

(defclass block-face ()
  ((name :initarg :name :reader block-face-name)
   (neighbor :initarg :neighbor :reader block-face-neighbor)
   (corners :initarg :corners :reader block-face-corners)))

(defun make-block-face (name neighbor corners)
  (make-instance 'block-face :name name :neighbor neighbor
                              :corners corners))

(defun block-face-local-uv (face corner)
  "Project CORNER onto FACE's plane as tile-local UV coordinates.

The rule is read from the face's own outward normal rather than its name:
horizontal faces map world X,Z straight across the tile, and lateral faces
keep the world-horizontal axis as U while flipping world Y into V so tiles
hang upright."
  (let ((normal (block-face-neighbor face)))
    (cond ((not (zerop (voxel-direction-dy normal)))
           (values (first corner) (third corner)))
          ((not (zerop (voxel-direction-dz normal)))
           (values (first corner) (- 1 (second corner))))
          (t (values (third corner) (- 1 (second corner)))))))

(defparameter *block-faces*
  (list
   (make-block-face :left +voxel-negative-x+
                    '((0 0 0) (0 0 1) (0 1 1) (0 1 0)))
   (make-block-face :right +voxel-positive-x+
                    '((1 0 1) (1 0 0) (1 1 0) (1 1 1)))
   (make-block-face :bottom +voxel-negative-y+
                    '((0 0 1) (0 0 0) (1 0 0) (1 0 1)))
   (make-block-face :top +voxel-positive-y+
                    '((0 1 0) (0 1 1) (1 1 1) (1 1 0)))
   (make-block-face :back +voxel-negative-z+
                    '((1 0 0) (0 0 0) (0 1 0) (1 1 0)))
   (make-block-face :front +voxel-positive-z+
                    '((0 0 1) (1 0 1) (1 1 1) (0 1 1)))))

(defmethod block-face-tile ((block block-kind) (face block-face))
  (let ((tiles (block-kind-face-tiles block)))
    (or (getf tiles (block-face-name face))
        (and (member (block-face-name face) '(:left :right :back :front))
             (getf tiles :side))
        (getf tiles :all)
        (error "No texture tile for ~S face ~S."
               (block-kind-name block) (block-face-name face)))))

(defvar *block-kinds* nil)

(defun ensure-block-kind
    (current name &key face-tiles categories
                       (display-color '(0.5 0.5 0.5)) (placeable-p t)
                       (light-opacity 15) (light-emission 0)
                       (surface-emission 0.0))
  "Define NAME while preserving CURRENT's identity across live redefinition."
  (let ((initargs
          (list :name name :face-tiles face-tiles :categories categories
                :display-color display-color :placeable-p placeable-p
                :light-opacity light-opacity :light-emission light-emission
                :surface-emission surface-emission)))
    (if (and (typep current 'block-kind)
             (eq name (block-kind-name current)))
        (apply #'reinitialize-instance current initargs)
        (apply #'make-instance 'block-kind initargs))))

(defmacro define-block-kinds (&body definitions)
  "Define the complete ordered block vocabulary.

Each definition is (VARIABLE NAME &rest BLOCK-KIND-INITARGS), optionally
followed by a variable documentation string.  Re-evaluation updates existing
objects in place, so resident worlds and live inventories observe the new
definition."
  (let ((variables (mapcar #'first definitions)))
    `(progn
       ,@(loop for (variable name . options) in definitions
               for documentation = (and (stringp (first options))
                                        (pop options))
               collect
               `(progn
                  (defvar ,variable nil ,@(when documentation
                                             (list documentation)))
                  (setf ,variable
                        (ensure-block-kind ,variable ,name ,@options))))
       (setf *block-kinds* (list ,@variables)))))

(define-block-kinds
  (*grass-block* :grass
   :face-tiles '(:top 0 :side 1 :bottom 2)
   :categories '(:natural) :display-color '(0.28 0.66 0.25))
  (*dirt-block* :dirt
   :face-tiles '(:all 2)
   :categories '(:natural) :display-color '(0.48 0.31 0.18))
  (*stone-block* :stone
   :face-tiles '(:all 3)
   :categories '(:building) :display-color '(0.49 0.52 0.54))
  (*wood-block* :wood
   :face-tiles '(:top 5 :bottom 5 :side 4)
   :categories '(:building) :display-color '(0.57 0.36 0.17))
  (*leaf-block* :leaves
   :face-tiles '(:all 6)
   :categories '(:natural) :display-color '(0.19 0.50 0.22))
  (*sand-block* :sand
   :face-tiles '(:all 7)
   :categories '(:natural) :display-color '(0.78 0.68 0.37))
  (*snow-block* :snow
   :face-tiles '(:top 8 :side 8 :bottom 2)
   :categories '(:natural) :display-color '(0.86 0.91 0.94))
  (*crystal-block* :crystal
   :face-tiles '(:all 9)
   :categories '(:luminous) :display-color '(0.18 0.86 0.88)
   :light-emission 12 :surface-emission 1.2)
  (*terminal-block* :terminal
   "The graphite display-block material used by world-native terminals."
   :face-tiles '(:all 10)
   :categories '(:building :luminous) :display-color '(0.13 0.31 0.34)
   :surface-emission 0.16)
  (*gravel-block* :gravel
   :face-tiles '(:all 11)
   :categories '(:natural) :display-color '(0.46 0.44 0.40))
  (*clay-block* :clay
   :face-tiles '(:all 12)
   :categories '(:natural) :display-color '(0.63 0.68 0.70))
  (*mud-block* :mud
   :face-tiles '(:all 13)
   :categories '(:natural) :display-color '(0.31 0.20 0.12))
  (*moss-block* :moss
   :face-tiles '(:all 14)
   :categories '(:natural) :display-color '(0.29 0.45 0.16))
  (*cactus-block* :cactus
   :face-tiles '(:top 16 :bottom 16 :side 15)
   :categories '(:natural) :display-color '(0.20 0.49 0.24))
  (*cobblestone-block* :cobblestone
   :face-tiles '(:all 17)
   :categories '(:building) :display-color '(0.42 0.43 0.41))
  (*stone-bricks-block* :stone-bricks
   :face-tiles '(:all 18)
   :categories '(:building) :display-color '(0.53 0.54 0.51))
  (*bricks-block* :bricks
   :face-tiles '(:all 19)
   :categories '(:building) :display-color '(0.63 0.28 0.19))
  (*planks-block* :planks
   :face-tiles '(:all 20)
   :categories '(:building) :display-color '(0.66 0.45 0.23))
  (*sandstone-block* :sandstone
   :face-tiles '(:all 21)
   :categories '(:building) :display-color '(0.79 0.68 0.43))
  (*slate-block* :slate
   :face-tiles '(:all 22)
   :categories '(:building) :display-color '(0.25 0.29 0.32)))

(defun placeable-block-kinds ()
  "Return the numbered material palette used by luvcraft and its tools."
  (remove-if-not #'block-kind-placeable-p *block-kinds*))

(defun block-kind-named (name &optional (error-p t))
  "Return the shared block kind whose durable semantic name is NAME.

When ERROR-P is true, signal an error rather than returning NIL for an
unknown name.  Save files and other external descriptions refer to block
kinds through this vocabulary instead of printing CLOS object identities."
  (or (find name *block-kinds* :key #'block-kind-name :test #'eq)
      (when error-p
        (error "No block kind is named ~S." name))))

(defconstant +block-atlas-tile-size+ 16)
(defconstant +block-atlas-tile-count+ 26)
(defconstant +block-atlas-texture-format+ :rgba8-unorm-srgb)
(defconstant +block-normal-atlas-texture-format+ :rgba8-unorm)

(defgeneric paint-block-atlas-relief (tile x y)
  (:documentation
   "Return the 0..255 surface height of atlas tile TILE at tile-local X,Y.

Like the colour, each numbered tile is one EQL method, so a live image can
re-sculpt a single material's micro-surface and rebuild both atlases without
touching the rest of the palette.

See #CHUKWD for why relief and normals are generated beside colour rather
than arriving as authored assets."))

(defun block-atlas-byte (value)
  (max 0 (min 255 (round value))))

(defun pack-block-atlas-rgba (red green blue)
  (logior (block-atlas-byte red)
          (ash (block-atlas-byte green) 8)
          (ash (block-atlas-byte blue) 16)
          #xff000000))

(defun block-atlas-variation (x y tile)
  "Fine colour grain modulated by TILE's shared procedural relief."
  (+ (- (mod (+ (* x 17) (* y 31) (* tile 43) (* x y 7)) 25) 12)
     (round (- (paint-block-atlas-relief tile x y) 128) 12)))

(defun shaded-block-atlas-pixel (red green blue &optional (variation 0))
  (pack-block-atlas-rgba (+ red variation)
                         (+ green variation)
                         (+ blue variation)))

(defun block-atlas-brick-mortar-p (x y brick-width course-height)
  "Whether X,Y lies in staggered mortar for a regular masonry bond."
  (let* ((course (floor y course-height))
         (offset (if (oddp course) (floor brick-width 2) 0)))
    (or (zerop (mod y course-height))
        (zerop (mod (+ x offset) brick-width)))))

(defun block-atlas-plate-distance (x y width height &key (stagger-p t))
  "Return 0..1 distance from X,Y to the centre of its WIDTH by HEIGHT plate.

The same bond BLOCK-ATLAS-BRICK-MORTAR-P reads as a mask, read as a shape
instead: zero in the middle of a panel and one along its boundary, so a
plated surface can be both coloured and domed from one expression."
  (let* ((course (floor y height))
         (offset (if (and stagger-p (oddp course)) (floor width 2) 0))
         (u (mod (+ x offset) width))
         (v (mod y height))
         (half-u (/ (1- width) 2.0))
         (half-v (/ (1- height) 2.0)))
    (max (/ (abs (- u half-u)) half-u)
         (/ (abs (- v half-v)) half-v))))

(defgeneric paint-block-atlas-tile (tile x y)
  (:documentation
   "Return the packed RGBA pixel of atlas tile TILE at tile-local X,Y.

Each numbered tile is one EQL method, so a live image can repaint a single
material and rebuild the atlas without touching the rest of the palette."))

(defmethod paint-block-atlas-tile (tile x y)
  (declare (ignore x y))
  (error "Unknown block atlas tile ~S." tile))

(defmethod paint-block-atlas-tile ((tile (eql 0)) x y)
  "Grass top."
  (shaded-block-atlas-pixel 91 171 68 (block-atlas-variation x y tile)))

(defmethod paint-block-atlas-tile ((tile (eql 1)) x y)
  "Grass side: a green fringe over dirt."
  (let ((variation (block-atlas-variation x y tile)))
    (if (< y 4)
        (shaded-block-atlas-pixel 86 158 61 variation)
        (shaded-block-atlas-pixel 123 82 48 (round variation 2)))))

(defmethod paint-block-atlas-tile ((tile (eql 2)) x y)
  "Dirt."
  (shaded-block-atlas-pixel 126 84 49 (block-atlas-variation x y tile)))

(defmethod paint-block-atlas-tile ((tile (eql 3)) x y)
  "Stone, with sparse bright flecks."
  (shaded-block-atlas-pixel
   126 132 136
   (+ (round (block-atlas-variation x y tile) 2)
      (if (zerop (mod (+ (* x 3) (* y 5)) 19)) 20 0))))

(defmethod paint-block-atlas-tile ((tile (eql 4)) x y)
  "Wood bark, with vertical grain stripes."
  (shaded-block-atlas-pixel
   116 76 39
   (+ (round (block-atlas-variation x y tile) 3)
      (if (zerop (mod x 5)) 18 0))))

(defmethod paint-block-atlas-tile ((tile (eql 5)) x y)
  "Wood end grain: concentric rings around the tile centre."
  (let* ((dx (- x 7.5))
         (dy (- y 7.5))
         (ring (mod (floor (+ (* dx dx) (* dy dy))) 18)))
    (shaded-block-atlas-pixel 133 91 49 (- ring 9))))

(defmethod paint-block-atlas-tile ((tile (eql 6)) x y)
  "Leaves: a strong checker so the canopy reads as foliage."
  (shaded-block-atlas-pixel
   51 132 58
   (+ (block-atlas-variation x y tile) (if (evenp (+ x y)) 8 -8))))

(defmethod paint-block-atlas-tile ((tile (eql 7)) x y)
  "Sand, with sparse darker grains."
  (shaded-block-atlas-pixel
   205 185 128
   (+ (round (block-atlas-variation x y tile) 2)
      (if (zerop (mod (+ x (* y 3)) 13)) 13 0))))

(defmethod paint-block-atlas-tile ((tile (eql 8)) x y)
  "Snow, with sparse glints."
  (shaded-block-atlas-pixel
   226 238 242
   (+ (round (block-atlas-variation x y tile) 3)
      (if (zerop (mod (+ (* x 5) (* y 7)) 23)) 14 0))))

(defmethod paint-block-atlas-tile ((tile (eql 9)) x y)
  "Blue crystal, with bright diagonal facets."
  (let* ((diagonal (abs (- x y)))
         (facet (if (or (<= diagonal 1)
                        (<= (abs (- (+ x y) 15)) 1))
                    42
                    0))
         (edge (if (or (zerop x) (zerop y)
                       (= x (1- +block-atlas-tile-size+))
                       (= y (1- +block-atlas-tile-size+)))
                   28
                   0))
         (variation (block-atlas-variation x y tile)))
    (pack-block-atlas-rgba
     (+ 72 (round variation 3) edge)
     (+ 176 (round variation 2) facet edge)
     (+ 235 variation facet edge))))

(defmethod paint-block-atlas-tile ((tile (eql 10)) x y)
  "Terminal graphite: a quiet face with a raised per-block bezel."
  (let* ((outer-edge
           (if (or (zerop x) (zerop y)
                   (= x (1- +block-atlas-tile-size+))
                   (= y (1- +block-atlas-tile-size+)))
               18
               0))
         (inner-edge
           (if (or (= x 1) (= y 1)
                   (= x (- +block-atlas-tile-size+ 2))
                   (= y (- +block-atlas-tile-size+ 2)))
               6
               0))
         (variation (round (block-atlas-variation x y tile) 5))
         (bezel (+ outer-edge inner-edge)))
    (pack-block-atlas-rgba
     (+ 18 variation bezel)
     (+ 25 variation bezel)
     (+ 36 variation bezel))))

(defmethod paint-block-atlas-tile ((tile (eql 11)) x y)
  "Gravel: mixed cool and warm stones."
  (let* ((grain (block-atlas-lattice-hash x y 111))
         (variation (+ (round (block-atlas-variation x y tile) 2)
                       (- (mod grain 43) 21))))
    (shaded-block-atlas-pixel 119 114 105 variation)))

(defmethod paint-block-atlas-tile ((tile (eql 12)) x y)
  "Clay: cool compact earth with faint horizontal bands."
  (shaded-block-atlas-pixel
   158 171 176
   (+ (round (block-atlas-variation x y tile) 4)
      (case (mod y 6) (0 -7) (1 -3) (t 2)))))

(defmethod paint-block-atlas-tile ((tile (eql 13)) x y)
  "Mud: dark wet clods with occasional glossy pockets."
  (let ((pocket (if (> (block-atlas-lattice-hash x y 132) 232) 20 0)))
    (shaded-block-atlas-pixel
     78 51 31 (+ pocket (round (block-atlas-variation x y tile) 2)))))

(defmethod paint-block-atlas-tile ((tile (eql 14)) x y)
  "Moss: dense green cushions mottled across the face."
  (let ((clump (- (block-atlas-clump x y 141 4) 128)))
    (pack-block-atlas-rgba
     (+ 67 (round clump 8))
     (+ 115 (round clump 4))
     (+ 35 (round clump 10)))))

(defmethod paint-block-atlas-tile ((tile (eql 15)) x y)
  "Cactus side: vertical ribs dotted with pale spines."
  (let* ((rib (case (mod x 5) (0 -18) (1 8) (2 18) (3 8) (t -10)))
         (spine (if (zerop (mod (+ (* x 3) (* y 5)) 17)) 34 0)))
    (pack-block-atlas-rgba (+ 42 rib spine)
                           (+ 125 rib spine)
                           (+ 55 (round rib 2) spine))))

(defmethod paint-block-atlas-tile ((tile (eql 16)) x y)
  "Cactus top: a radial cut surface around a dark green rind."
  (let* ((dx (- x 7.5))
         (dy (- y 7.5))
         (radius (sqrt (+ (* dx dx) (* dy dy))))
         (rind (if (> radius 5.5) -35 0))
         (ring (round (* 7 (sin (* radius 2.3))))))
    (pack-block-atlas-rgba (+ 83 rind ring)
                           (+ 151 rind ring)
                           (+ 68 rind (round ring 2)))))

(defmethod paint-block-atlas-tile ((tile (eql 17)) x y)
  "Cobblestone: irregular grey stones divided by dark joints."
  (let* ((joint (or (zerop (mod (+ x (* 2 y)) 7))
                    (zerop (mod (+ (* 3 x) y) 13))))
         (variation (round (block-atlas-variation x y tile) 2)))
    (if joint
        (shaded-block-atlas-pixel 64 66 64 variation)
        (shaded-block-atlas-pixel 123 126 120 variation))))

(defmethod paint-block-atlas-tile ((tile (eql 18)) x y)
  "Stone bricks in a pale staggered bond."
  (if (block-atlas-brick-mortar-p x y 8 5)
      (shaded-block-atlas-pixel 77 79 76)
      (shaded-block-atlas-pixel
       139 142 135 (round (block-atlas-variation x y tile) 3))))

(defmethod paint-block-atlas-tile ((tile (eql 19)) x y)
  "Fired red bricks with warm variation and deep mortar."
  (if (block-atlas-brick-mortar-p x y 8 5)
      (shaded-block-atlas-pixel 83 69 61)
      (shaded-block-atlas-pixel
       166 72 49 (round (block-atlas-variation x y tile) 2))))

(defmethod paint-block-atlas-tile ((tile (eql 20)) x y)
  "Sawn planks with horizontal seams and wandering grain."
  (let ((seam (zerop (mod y 5)))
        (grain (round (* 9 (sin (/ (+ (* x 2) y) 2.4))))))
    (if seam
        (shaded-block-atlas-pixel 91 58 28)
        (shaded-block-atlas-pixel 170 116 58 grain))))

(defmethod paint-block-atlas-tile ((tile (eql 21)) x y)
  "Cut sandstone with broad sediment bands."
  (let ((band (case (mod y 7) (0 -18) (1 -8) (5 7) (t 0))))
    (shaded-block-atlas-pixel
     205 178 111 (+ band (round (block-atlas-variation x y tile) 4)))))

(defmethod paint-block-atlas-tile ((tile (eql 22)) x y)
  "Slate: dark blue-grey layers with fine pale cleavage lines."
  (let ((seam (or (zerop (mod y 5))
                  (and (zerop (mod y 3)) (> (mod (+ x y) 7) 4)))))
    (if seam
        (shaded-block-atlas-pixel 91 103 111)
        (shaded-block-atlas-pixel
         58 69 77 (round (block-atlas-variation x y tile) 4)))))

;;; Not every atlas tile is a material the player places.  The last three
;;; belong to the animals which walk over the terrain rather than to the
;;; terrain itself: a critter model is built from small textured boxes drawn by
;;; the ordinary block surface pipeline, so its hide is painted by exactly the
;;; same per-tile arithmetic as stone or moss.  See CRITTERS.LISP.

(defconstant +turtle-carapace-tile+ 23)
(defconstant +turtle-skin-tile+ 24)
(defconstant +turtle-plastron-tile+ 25)

;;; A critter tile is stretched across a box face a fraction of a cell wide
;;; rather than tiled over a whole block, so its pattern is deliberately
;;; several times finer than a terrain material's: roughly three scutes across
;;; a shell rather than two plates across a wall.

(defconstant +turtle-scute-width+ 5)
(defconstant +turtle-scute-height+ 4)

(defmethod paint-block-atlas-tile ((tile (eql 23)) x y)
  "Turtle carapace: small staggered scutes with dark keratin joints."
  (let ((distance (block-atlas-plate-distance
                   x y +turtle-scute-width+ +turtle-scute-height+))
        (variation (round (block-atlas-variation x y tile) 3)))
    (if (block-atlas-brick-mortar-p
         x y +turtle-scute-width+ +turtle-scute-height+)
        (shaded-block-atlas-pixel 56 70 40 variation)
        (shaded-block-atlas-pixel
         84 108 50 (+ variation (round (* 22 (- 0.75 distance))))))))

(defmethod paint-block-atlas-tile ((tile (eql 24)) x y)
  "Turtle skin: dark olive hide pebbled with paler flecks."
  (let ((pebble (- (block-atlas-clump x y 241 3) 128))
        (fleck (if (> (block-atlas-lattice-hash x y 242) 232) 34 0)))
    (pack-block-atlas-rgba (+ 98 (round pebble 5) fleck)
                           (+ 116 (round pebble 4) fleck)
                           (+ 56 (round pebble 7) (round fleck 2)))))

(defun turtle-plastron-seam-p (x y)
  "Whether X,Y lies in the straight seam between two belly plates."
  (or (zerop (mod x 6)) (zerop (mod y 5))))

(defmethod paint-block-atlas-tile ((tile (eql 25)) x y)
  "Turtle plastron: pale horn plates in a straight, unstaggered bond."
  (let ((variation (round (block-atlas-variation x y tile) 4)))
    (if (turtle-plastron-seam-p x y)
        (shaded-block-atlas-pixel 148 130 88 variation)
        (shaded-block-atlas-pixel 202 188 132 variation))))

;;; Colour is only half of what a material looks like.  The other half is its
;;; micro-surface: whether it is granular, grooved, tufted, or faceted.  The
;;; normal atlas carries that as a height field and the unit normal derived
;;; from it, painted by the same per-tile arithmetic that paints the colour.
;;; The block shader reads that normal directly to perturb its shading normal.

(defun block-atlas-lattice-hash (x y salt)
  "A small deterministic hash of one integer lattice site into 0..255."
  (let ((value (+ (* x 374761393) (* y 668265263) (* salt 2654435761))))
    (setf value (logand (logxor value (ash value -13)) #xffffffff))
    (setf value (logand (* value 1274126177) #xffffffff))
    (ldb (byte 8 13) value)))

(defun block-atlas-clump (x y salt period)
  "Smooth 0..255 clumping at PERIOD, so a material can read as tufted."
  (let* ((cx (floor x period))
         (cy (floor y period))
         (fx (/ (mod x period) (float period)))
         (fy (/ (mod y period) (float period)))
         (sx (* fx fx (- 3.0 (* 2.0 fx))))
         (sy (* fy fy (- 3.0 (* 2.0 fy))))
         (a (block-atlas-lattice-hash cx cy salt))
         (b (block-atlas-lattice-hash (1+ cx) cy salt))
         (c (block-atlas-lattice-hash cx (1+ cy) salt))
         (d (block-atlas-lattice-hash (1+ cx) (1+ cy) salt)))
    (round (+ (* (+ a (* (- b a) sx)) (- 1.0 sy))
              (* (+ c (* (- d c) sx)) sy)))))

(defmethod paint-block-atlas-relief (tile x y)
  "A plausible default: fine grain, so a new material is never dead flat."
  (+ 128 (- (ash (block-atlas-lattice-hash x y (+ 91 tile)) -2) 32)))

(defmethod paint-block-atlas-relief ((tile (eql 0)) x y)
  "Grass top: blades clumped into tufts."
  (block-atlas-byte
   (+ (* 0.62 (block-atlas-clump x y 3 4))
      (* 0.38 (block-atlas-lattice-hash x y 11)))))

(defmethod paint-block-atlas-relief ((tile (eql 1)) x y)
  "Grass side: a ragged fringe over dirt clods."
  (if (< y 5)
      (block-atlas-byte
       (+ 60 (* 0.75 (block-atlas-lattice-hash x y 12))
          (* 40 (- 4 y))))
      (block-atlas-byte (* 0.85 (block-atlas-clump x y 13 5)))))

(defmethod paint-block-atlas-relief ((tile (eql 2)) x y)
  "Dirt: rounded clods with grit between them."
  (block-atlas-byte
   (+ (* 0.70 (block-atlas-clump x y 21 5))
      (* 0.30 (block-atlas-lattice-hash x y 22)))))

(defmethod paint-block-atlas-relief ((tile (eql 3)) x y)
  "Stone: granular, cut by a couple of shallow cracks."
  (let ((crack (if (or (zerop (mod (+ (* x 3) y) 11))
                       (zerop (mod (+ x (* y 5)) 13)))
                   -70
                   0)))
    (block-atlas-byte
     (+ 150 crack
        (* 0.45 (- (block-atlas-lattice-hash x y 31) 128))
        (* 0.40 (- (block-atlas-clump x y 32 4) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 4)) x y)
  "Wood bark: deep vertical grooves with grain between them."
  (let ((groove (case (mod x 5) (0 -80) (1 -30) (4 -25) (t 20))))
    (block-atlas-byte
     (+ 150 groove (* 0.30 (- (block-atlas-lattice-hash x y 41) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 5)) x y)
  "Wood end grain: raised concentric rings."
  (let* ((dx (- x 7.5))
         (dy (- y 7.5))
         (ring (mod (floor (+ (* dx dx) (* dy dy))) 18)))
    (block-atlas-byte
     (+ 128 (* 7 (- ring 9))
        (* 0.20 (- (block-atlas-lattice-hash x y 51) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 6)) x y)
  "Leaves: overlapping lobes with gaps between them."
  (block-atlas-byte
   (+ (* 0.80 (block-atlas-clump x y 61 4))
      (* 0.35 (block-atlas-lattice-hash x y 62))
      (if (evenp (+ x y)) 18 -18))))

(defmethod paint-block-atlas-relief ((tile (eql 7)) x y)
  "Sand: fine wind ripples over a soft dune."
  (block-atlas-byte
   (+ 128
      (* 26 (sin (/ (+ x (* 0.6 y)) 1.7)))
      (* 0.45 (- (block-atlas-clump x y 71 6) 128))
      (* 0.18 (- (block-atlas-lattice-hash x y 72) 128)))))

(defmethod paint-block-atlas-relief ((tile (eql 8)) x y)
  "Snow: soft drifts with the odd crystal glint standing proud."
  (block-atlas-byte
   (+ (* 0.80 (block-atlas-clump x y 81 6))
      (* 0.20 (block-atlas-lattice-hash x y 82))
      (if (zerop (mod (+ (* x 5) (* y 7)) 23)) 60 0))))

(defmethod paint-block-atlas-relief ((tile (eql 9)) x y)
  "Crystal: sharp faceted planes meeting along the tile diagonals."
  (let ((diagonal (abs (- x y)))
        (anti (abs (- (+ x y) 15))))
    (block-atlas-byte
     (+ 110 (* 9 (- 8 (min diagonal 8))) (* 6 (- 8 (min anti 8)))))))

(defmethod paint-block-atlas-relief ((tile (eql 10)) x y)
  "Terminal graphite: flat glass inside a raised per-block bezel."
  (let ((edge (min x y (- +block-atlas-tile-size+ 1 x)
                   (- +block-atlas-tile-size+ 1 y))))
    (block-atlas-byte
     (+ 96 (* 55 (max 0 (- 2 edge)))
        (* 0.10 (- (block-atlas-lattice-hash x y 101) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 11)) x y)
  "Gravel: sharply varying grains at nearly every texel."
  (block-atlas-byte
   (+ 112 (* 0.70 (- (block-atlas-lattice-hash x y 111) 96)))))

(defmethod paint-block-atlas-relief ((tile (eql 12)) x y)
  "Clay: compressed layers with very little loose grain."
  (block-atlas-byte
   (+ 132 (case (mod y 6) (0 -24) (1 -9) (t 4))
      (* 0.08 (- (block-atlas-lattice-hash x y 121) 128)))))

(defmethod paint-block-atlas-relief ((tile (eql 13)) x y)
  "Mud: rounded wet clods interrupted by sunken pockets."
  (let ((pocket (if (> (block-atlas-lattice-hash x y 132) 232) -70 0)))
    (block-atlas-byte
     (+ 132 pocket (* 0.42 (- (block-atlas-clump x y 131 5) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 14)) x y)
  "Moss: soft overlapping cushions."
  (block-atlas-byte
   (+ 112 (* 0.62 (block-atlas-clump x y 141 4))
      (* 0.14 (- (block-atlas-lattice-hash x y 142) 128)))))

(defmethod paint-block-atlas-relief ((tile (eql 15)) x y)
  "Cactus side: strong vertical ribs with standing spines."
  (block-atlas-byte
   (+ 118 (case (mod x 5) (0 -55) (1 5) (2 48) (3 5) (t -28))
      (if (zerop (mod (+ (* x 3) (* y 5)) 17)) 70 0))))

(defmethod paint-block-atlas-relief ((tile (eql 16)) x y)
  "Cactus top: gently domed flesh inside a lower rind."
  (let* ((dx (- x 7.5))
         (dy (- y 7.5))
         (radius (sqrt (+ (* dx dx) (* dy dy)))))
    (block-atlas-byte (+ 178 (- (* radius 8))
                         (if (> radius 5.5) -35 0)))))

(defmethod paint-block-atlas-relief ((tile (eql 17)) x y)
  "Cobblestone: proud stones separated by recessed irregular joints."
  (if (or (zerop (mod (+ x (* 2 y)) 7))
          (zerop (mod (+ (* 3 x) y) 13)))
      48
      (block-atlas-byte
       (+ 158 (* 0.28 (- (block-atlas-clump x y 171 4) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 18)) x y)
  "Stone bricks: shallow-cut mortar around worn faces."
  (if (block-atlas-brick-mortar-p x y 8 5)
      54
      (block-atlas-byte
       (+ 157 (* 0.16 (- (block-atlas-lattice-hash x y 181) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 19)) x y)
  "Fired bricks: deeper joints and rougher faces than stone masonry."
  (if (block-atlas-brick-mortar-p x y 8 5)
      42
      (block-atlas-byte
       (+ 164 (* 0.26 (- (block-atlas-lattice-hash x y 191) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 20)) x y)
  "Planks: recessed seams over gently ridged grain."
  (if (zerop (mod y 5))
      45
      (block-atlas-byte
       (+ 145 (* 18 (sin (/ (+ (* x 2) y) 2.4)))))))

(defmethod paint-block-atlas-relief ((tile (eql 21)) x y)
  "Sandstone: broad sediment layers with granular faces."
  (block-atlas-byte
   (+ 136 (case (mod y 7) (0 -42) (1 -18) (5 14) (t 0))
      (* 0.14 (- (block-atlas-lattice-hash x y 211) 128)))))

(defmethod paint-block-atlas-relief ((tile (eql 22)) x y)
  "Slate: thin raised sheets divided by sharp cleavage."
  (if (or (zerop (mod y 5))
          (and (zerop (mod y 3)) (> (mod (+ x y) 7) 4)))
      68
      (block-atlas-byte
       (+ 151 (* 0.12 (- (block-atlas-lattice-hash x y 221) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 23)) x y)
  "Turtle carapace: each scute domes up out of the joint around it."
  (let ((distance (block-atlas-plate-distance
                   x y +turtle-scute-width+ +turtle-scute-height+)))
    (block-atlas-byte
     (+ (if (block-atlas-brick-mortar-p
             x y +turtle-scute-width+ +turtle-scute-height+)
            58
            (+ 158 (* 52 (- 0.75 distance))))
        (* 0.10 (- (block-atlas-lattice-hash x y 231) 128))))))

(defmethod paint-block-atlas-relief ((tile (eql 24)) x y)
  "Turtle skin: small tight pebbles all over the hide."
  (block-atlas-byte
   (+ 118 (* 0.34 (- (block-atlas-clump x y 241 3) 128))
      (* 0.22 (- (block-atlas-lattice-hash x y 243) 128)))))

(defmethod paint-block-atlas-relief ((tile (eql 25)) x y)
  "Turtle plastron: flat belly plates with narrow sunken seams."
  (block-atlas-byte
   (+ (if (turtle-plastron-seam-p x y) 72 150)
      (* 0.09 (- (block-atlas-lattice-hash x y 251) 128)))))

(defun make-block-texture-atlas ()
  "Return the little world's horizontal RGBA8 atlas as packed pixel words.

RGB is the material's procedural colour and A is opaque coverage."
  (let* ((width (* +block-atlas-tile-size+ +block-atlas-tile-count+))
         (pixels (make-array (list +block-atlas-tile-size+ width)
                             :element-type '(unsigned-byte 32))))
    (dotimes (y +block-atlas-tile-size+)
      (dotimes (tile +block-atlas-tile-count+)
        (dotimes (x +block-atlas-tile-size+)
          (setf (aref pixels y (+ x (* tile +block-atlas-tile-size+)))
                (paint-block-atlas-tile tile x y)))))
    pixels))

(defun block-atlas-relief-at (tile x y)
  "Read TILE's relief with coordinates clamped to its own boundary."
  (paint-block-atlas-relief
   tile
   (max 0 (min (1- +block-atlas-tile-size+) x))
   (max 0 (min (1- +block-atlas-tile-size+) y))))

(defun pack-block-atlas-normal (x y z height)
  "Pack a signed tangent-space normal and its source HEIGHT into RGBA8."
  ;; PACK-BLOCK-ATLAS-RGBA supplies opaque alpha; replace it with the
  ;; inspectable height after reusing its channel encoding.
  (logior (logand (pack-block-atlas-rgba (* (+ x 1.0) 127.5)
                                         (* (+ y 1.0) 127.5)
                                         (* (+ z 1.0) 127.5))
                  #x00ffffff)
          (ash (block-atlas-byte height) 24)))

(defun paint-block-atlas-normal (tile x y)
  "Return TILE's packed tangent-space normal at X,Y.

The normal is the central difference of the exact procedural relief painted
beside the colour.  Boundary reads clamp to the tile, so neither the generated
map nor later texture filtering can borrow a neighbouring material's shape."
  (let* ((height (block-atlas-relief-at tile x y))
         (dx (* 1.15
                (/ (- (block-atlas-relief-at tile (1- x) y)
                      (block-atlas-relief-at tile (1+ x) y))
                   255.0)))
         (dy (* 1.15
                (/ (- (block-atlas-relief-at tile x (1- y))
                      (block-atlas-relief-at tile x (1+ y)))
                   255.0)))
         (length (sqrt (+ (* dx dx) (* dy dy) 1.0))))
    (pack-block-atlas-normal (/ dx length) (/ dy length) (/ length) height)))

(defun make-block-normal-atlas ()
  "Return the relief-correlated tangent-space normal atlas as packed RGBA8.

RGB is a linear encoded unit normal and A is the procedural height from which
it was derived.  This is a derived materialization, not an authored asset."
  (let* ((width (* +block-atlas-tile-size+ +block-atlas-tile-count+))
         (pixels (make-array (list +block-atlas-tile-size+ width)
                             :element-type '(unsigned-byte 32))))
    (dotimes (y +block-atlas-tile-size+)
      (dotimes (tile +block-atlas-tile-count+)
        (dotimes (x +block-atlas-tile-size+)
          (setf (aref pixels y (+ x (* tile +block-atlas-tile-size+)))
                (paint-block-atlas-normal tile x y)))))
    pixels))
