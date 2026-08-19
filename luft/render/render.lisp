;;; The atelier: a small solid world, its surface chain, and the GPU objects
;;; that draw it.
;;;
;;; Nothing here meshes.  The world is a 3-chain of solid cells; REFRESH-SCENE
;;; takes its boundary, orders the resulting face sites by chunk, pads them to
;;; whole bricks, and measures a bounding sphere per brick.  The renderer
;;; uploads exactly those two arrays and one frame block, then either draws a
;;; few vertices per site, each pulling its own site, or dispatches one task
;;; workgroup per brick: the TECHNIQUE below.

(in-package #:luft.render)

;;; ------------------------------------------------------------------------
;;; Scenes

(defconstant +brick-size+ shaders:+brick-size+)
(defconstant +chunk-bits+ 3
  "Sites are ordered by 8-cell chunk so a brick's faces stay close together.")

;;; ------------------------------------------------------------------------
;;; Worlds: a solid, and what every cell of it is cut from
;;;
;;; A LUFT chain says where the world is solid and nothing about what the
;;; solid is.  That was enough while every style shaded by face direction
;;; alone, and it stops being enough the moment a bridge wants stone piers
;;; under a timber deck.  A world is therefore a chain together with one
;;; stock slot per cell -- an index into a short palette, sixteen materials
;;; at most, because sixteen is what the packed site has room to carry.
;;; #PWMCOL
;;;
;;; Building code does not pass the material about.  It binds *STOCK* and
;;; fills, the way a shop works from one board at a time:
;;;
;;;   (with-stock (:limestone)
;;;     (fill-box world 26 27 44 45 0 8))

(defclass world ()
  ((domain
    :initarg :domain
    :reader world-domain)
   (solid
    :initarg :solid
    :reader world-solid
    :documentation "The solid world: a 3-chain of positive cells.")
   (stocks
    :initform (make-array 1 :adjustable t :fill-pointer 1
                            :initial-element :turf)
    :accessor world-stocks
    :documentation "The material of each slot, slot zero first.")
   (slots
    :initarg :slots
    :reader world-slots
    :documentation "One stock slot per cell, indexed as the cell bits are."))
  (:documentation "A solid world and the stock every cell of it is cut from."))

(defun make-world (&key (horizontal-bits 6)
                        (domain (luft:make-world-domain
                                 :horizontal-bits horizontal-bits)))
  "An empty world over DOMAIN, every cell of it slot zero."
  (make-instance 'world
                 :domain domain
                 :solid (luft:make-solid-chain domain)
                 :slots (make-array (luft:chain-cell-bit-count domain)
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)))

(defparameter *stock* :turf
  "The material FILL-BOX and its kin stamp on the cells they fill.")

(defmacro with-stock ((material) &body body)
  "Evaluate BODY with MATERIAL as the stock that filling stamps."
  `(let ((*stock* ,material)) ,@body))

(defun world-stock-slot (world material)
  "The slot MATERIAL occupies in WORLD's palette, adding it if it is new."
  (let ((stocks (world-stocks world)))
    (or (position material stocks)
        (progn
          (find-material material)
          (when (<= shaders:+stock-slots+ (fill-pointer stocks))
            (error "A world holds ~D stocks; ~S would be the ~:*~R."
                   shaders:+stock-slots+ material))
          (vector-push-extend material stocks)
          (1- (fill-pointer stocks))))))

(defun world-cell-p (world x y z)
  "Whether WORLD is solid at X,Y,Z."
  (luft:solid-cell-p (world-solid world) x y z))

(defun world-vertical-p (z)
  "Whether Z names a cell row a world can hold: the world has a floor and
a ceiling, and building code that runs off either end is building nothing
rather than making an error."
  (and (integerp z) (<= 0 z (- luft:+vertical-cell-rows+ 2))))

(defun (setf world-cell-p) (state world x y z)
  "Make WORLD solid or empty at X,Y,Z, stamping *STOCK* where it fills."
  (when (world-vertical-p z)
    (setf (luft:solid-cell-p (world-solid world) x y z) state)
    (when state
      (setf (aref (world-slots world)
                  (luft:cell-bit-index (world-domain world) x y z))
            (world-stock-slot world *stock*))))
  state)

(defun paint-cell (world x y z &optional (material *stock*))
  "Give the cell at X,Y,Z of WORLD the stock MATERIAL, solid or not."
  (when (world-vertical-p z)
    (setf (aref (world-slots world)
                (luft:cell-bit-index (world-domain world) x y z))
          (world-stock-slot world material)))
  material)

(defclass scene ()
  ((domain
    :initarg :domain
    :reader scene-domain)
   (solid
    :initarg :solid
    :reader scene-solid
    :documentation "The solid world: a 3-chain of positive cells.")
   (surface
    :initform nil
    :accessor scene-surface
    :documentation "The boundary of SOLID: exposed signed face sites.")
   (sites
    :initform nil
    :accessor scene-sites
    :documentation "Packed surface sites in brick order, zero-padded.")
   (bricks
    :initform nil
    :accessor scene-bricks
    :documentation "Four floats per brick: bounding sphere centre and radius.")
   (brick-count
    :initform 0
    :accessor scene-brick-count)
   (cell-bits
    :initform nil
    :accessor scene-cell-bits
    :documentation "The solid chain as dense (unsigned-byte 32) cell bits.")
   (slots
    :initarg :slots
    :initform nil
    :accessor scene-slots
    :documentation "One stock slot per cell, or NIL for a single-stock scene.")
   (stocks
    :initarg :stocks
    :initform nil
    :accessor scene-stocks
    :documentation "The material of each stock slot, or NIL for *MATERIAL*."))
  (:documentation "A solid world together with its drawable surface products."))

(defun make-scene (domain &key (solid (luft:make-solid-chain domain))
                               slots stocks)
  "Make a scene over DOMAIN and refresh its surface products once."
  (refresh-scene (make-instance 'scene :domain domain :solid solid
                                       :slots slots :stocks stocks)))

(defun world-scene (world)
  "The drawable scene of WORLD, carrying its stock slots and palette."
  (make-scene (world-domain world)
              :solid (world-solid world)
              :slots (world-slots world)
              :stocks (copy-seq (world-stocks world))))

(defun site-chunk-key (site)
  "A fixnum ordering key grouping signed site SITE by chunk, then by site."
  (logior (ash (ash (luft:site-z site) (- +chunk-bits+)) 45)
          (ash (ash (luft:site-y site) (- +chunk-bits+)) 24)
          (ash (luft:site-x site) (- +chunk-bits+))))

(defun order-sites-by-chunk (sites)
  "Return a fresh copy of the site-ordered SITES grouped by chunk."
  (stable-sort (copy-seq sites) #'< :key #'site-chunk-key))

(defun brick-spheres (sites brick-count)
  "Measure a bounding sphere for each brick of SITES as four floats each."
  (let ((spheres (make-array (* 4 brick-count) :element-type 'single-float
                                               :initial-element 0.0)))
    (dotimes (brick brick-count spheres)
      (let ((low-x nil) (low-y nil) (low-z nil)
            (high-x nil) (high-y nil) (high-z nil))
        (loop for index from (* brick +brick-size+)
                below (* (1+ brick) +brick-size+)
              for site = (aref sites index)
              unless (zerop site)
                do (let* ((x (luft:site-x site))
                          (y (luft:site-y site))
                          (z (luft:site-z site))
                          (x-high (+ x (if (luft:site-extends-p site :x) 1 0)))
                          (y-high (+ y (if (luft:site-extends-p site :y) 1 0)))
                          (z-high (+ z (if (luft:site-extends-p site :z) 1 0))))
                     (setf low-x (if low-x (min low-x x) x)
                           low-y (if low-y (min low-y y) y)
                           low-z (if low-z (min low-z z) z)
                           high-x (if high-x (max high-x x-high) x-high)
                           high-y (if high-y (max high-y y-high) y-high)
                           high-z (if high-z (max high-z z-high) z-high))))
        (when low-x
          (let* ((center-x (/ (+ low-x high-x) 2.0))
                 (center-y (/ (+ low-y high-y) 2.0))
                 (center-z (/ (+ low-z high-z) 2.0))
                 (radius (sqrt (+ (expt (- high-x center-x) 2)
                                  (expt (- high-y center-y) 2)
                                  (expt (- high-z center-z) 2)))))
            (setf (aref spheres (* 4 brick)) (coerce center-x 'single-float)
                  (aref spheres (+ 1 (* 4 brick))) (coerce center-y 'single-float)
                  (aref spheres (+ 2 (* 4 brick))) (coerce center-z 'single-float)
                  (aref spheres (+ 3 (* 4 brick)))
                  (coerce radius 'single-float))))))))

(defun site-solid-cell (site)
  "The X, Y, and Z of the solid cell a surface face SITE bounds.

The face's missing extent axis is its normal axis; its polarity says which
way that normal points, and the solid is the cell on the inward side."
  (let ((x (luft:site-x site))
        (y (luft:site-y site))
        (z (luft:site-z site))
        (extent (luft:site-extent site))
        ;; The canonical orientation of a face is +X, -Y, +Z by spanning
        ;; axis; polarity flips it, and the solid lies one cell back
        ;; whenever the outward normal runs along the positive axis.
        (negative-p (luft:site-negative-p site)))
    (cond ((= extent luft:+yz-face-extent+)
           (values (if negative-p x (1- x)) y z))
          ((= extent luft:+xz-face-extent+)
           (values x (if negative-p (1- y) y) z))
          (t (values x y (if negative-p z (1- z)))))))

(defun stamp-site-stocks (sites domain slots)
  "Set each site's stock bits from SLOTS, the stock slot of every cell."
  (dotimes (index (length sites) sites)
    (let ((site (aref sites index)))
      (unless (zerop site)
        (multiple-value-bind (x y z) (site-solid-cell site)
          (when (<= 0 z (1- luft:+vertical-cell-rows+))
            (setf (ldb (byte shaders:+site-stock-bits+
                             shaders:+site-stock-shift+)
                       (aref sites index))
                  (aref slots (luft:cell-bit-index domain x y z)))))))))

(defun refresh-scene (scene)
  "Recompute SCENE's surface chain, brick-ordered sites, and brick spheres."
  (let* ((surface (luft:surface-chain (scene-solid scene)))
         (ordered (order-sites-by-chunk (luft:chain-sites surface)))
         (brick-count (max 1 (ceiling (length ordered) +brick-size+)))
         (sites (make-array (* brick-count +brick-size+)
                            :element-type '(unsigned-byte 64)
                            :initial-element 0)))
    (replace sites ordered)
    (setf (scene-surface scene) surface
          (scene-sites scene) sites
          (scene-brick-count scene) brick-count
          ;; Measured before the stock bits go on: past bit sixty a packed
          ;; site is no longer a LUFT site, and nothing may read it as one.
          (scene-bricks scene) (brick-spheres sites brick-count)
          (scene-cell-bits scene) (luft:chain-cell-bits (scene-solid scene)))
    (when (scene-slots scene)
      (stamp-site-stocks sites (scene-domain scene) (scene-slots scene)))
    scene))

;;; ------------------------------------------------------------------------
;;; A demonstration world

(defun demo-height (x y)
  "Rolling ground with a plateau, in cells."
  (let ((rolling (+ 5.5
                    (* 2.4 (sin (/ x 6.0)) (cos (/ y 7.5)))
                    (* 1.2 (sin (/ (+ x y) 4.3)))))
        (plateau (if (and (<= 40 x 52) (<= 10 y 22)) 3.0 0.0)))
    (max 1 (floor (+ rolling plateau)))))

(defun fill-box (world x0 x1 y0 y1 z0 z1 &optional (state t))
  "Set every cell of the closed box to STATE, stamping *STOCK* where it fills."
  (loop for x from x0 to x1
        do (loop for y from y0 to y1
                 do (loop for z from z0 to z1
                          do (setf (world-cell-p world x y z) state)))))

(defun carve-ravine (world)
  "A gap for the bridge to cross, cut where the ground was continuous."
  (loop for y from 40 to 56
        do (let ((width (+ 3 (floor (abs (- y 48)) 3))))
             (loop for x from (- 30 width) to (+ 30 width)
                   do (loop for z from 0 to 12
                            do (setf (world-cell-p world x y z) nil))))))

(defun build-bridge (world)
  "A deck across the ravine on two piers, with a parapet either side.

Something has to span a gap before a world has anywhere to stand and look
down from, and a deck one cell thick with a parapet at its edge is the
smallest thing that reads as built rather than as terrain.  The piers are
stone because they stand in the water and the deck is oak because it is
walked on, which is the whole argument for a world knowing its stocks."
  (let ((deck 9))
    (with-stock (:granite)
      (dolist (x '(26 34))
        (fill-box world x (1+ x) 44 45 0 (1- deck))
        (fill-box world x (1+ x) 51 52 0 (1- deck))))
    (with-stock (:oak)
      (fill-box world 24 36 43 53 deck deck)
      (loop for x from 24 to 36
            unless (zerop (mod (- x 24) 4))
              do (setf (world-cell-p world x 43 (+ deck 1)) t
                       (world-cell-p world x 53 (+ deck 1)) t)))
    ;; Ramps up to the deck at either end, cut into the ground itself.
    (with-stock (:limestone)
      (loop for step from 0 to 8
            do (fill-box world (- 23 step) (- 23 step) 45 51
                         0 (max 0 (- deck 1 step)))
               (fill-box world (+ 37 step) (+ 37 step) 45 51
                         0 (max 0 (- deck 1 step)))))))

(defun build-balconies (world)
  "Three balconies off the tower, each a slab with a lip and a doorway."
  (loop for (z side) in '((6 :east) (12 :north) (17 :east))
        do (ecase side
             (:east
              (with-stock (:oak) (fill-box world 28 31 32 35 z z))
              (with-stock (:bronze)
                (fill-box world 31 31 32 35 (1+ z) (1+ z))
                (fill-box world 28 31 32 32 (1+ z) (1+ z))
                (fill-box world 28 31 35 35 (1+ z) (1+ z)))
              ;; The doorway it is reached through.
              (fill-box world 27 27 33 34 z (+ z 1) nil))
             (:north
              (with-stock (:oak) (fill-box world 22 25 38 41 z z))
              (with-stock (:bronze)
                (fill-box world 22 25 41 41 (1+ z) (1+ z))
                (fill-box world 22 22 38 41 (1+ z) (1+ z))
                (fill-box world 25 25 38 41 (1+ z) (1+ z)))
              (fill-box world 23 24 37 37 z (+ z 1) nil)))))

(defun build-terraces (world)
  "Stepped terraces below the tower: a hillside someone has taken in hand."
  (loop for step from 0 below 5
        for z = (+ 3 step)
        for near = (- 18 (* 2 step))
        do (with-stock (:turf)
             (fill-box world near (+ near 1) (- 24 step) (+ 33 step) 0 z))
           ;; A low retaining wall along the front of each terrace.
           (with-stock (:limestone)
             (loop for y from (- 24 step) to (+ 33 step)
                   unless (zerop (mod y 5))
                     do (setf (world-cell-p world near y (1+ z)) t)))))

(defun make-demo-scene (&key (horizontal-bits 6))
  "A small world: rolling turf, a stone tower, a timber bridge, terraces."
  (let* ((world (make-world :horizontal-bits horizontal-bits))
         (period (luft:world-domain-x-period (world-domain world))))
    (with-stock (:turf)
      (dotimes (x period)
        (dotimes (y period)
          (dotimes (z (demo-height x y))
            (setf (world-cell-p world x y z) t)))))
    ;; A hollow tower with a doorway.
    (with-stock (:limestone)
      (loop for z from 1 below 22
            do (loop for x from 20 to 27
                     do (loop for y from 30 to 37
                              when (and (or (= x 20) (= x 27)
                                            (= y 30) (= y 37))
                                        (not (and (= y 30) (<= 23 x 24)
                                                  (< z 9))))
                                do (setf (world-cell-p world x y z) t)))))
    ;; A floating slab, casting a clean shadow of empty air.
    (with-stock (:slate)
      (fill-box world 8 15 8 12 14 14))
    ;; A staircase up the plateau.
    (with-stock (:limestone)
      (loop for step from 0 below 6
            do (loop for y from 14 to 18
                     do (loop for z from 0 to (+ 4 step)
                              do (setf (world-cell-p world (- 39 step) y z)
                                       t)))))
    (carve-ravine world)
    (build-bridge world)
    (build-balconies world)
    (build-terraces world)
    (world-scene world)))

;;; ------------------------------------------------------------------------
;;; Camera

(defclass fly-camera ()
  ((position
    :initarg :position
    :accessor camera-position)
   (yaw
    :initarg :yaw
    :initform 0.0
    :accessor camera-yaw
    :documentation "Radians from +X toward +Y.")
   (pitch
    :initarg :pitch
    :initform 0.0
    :accessor camera-pitch
    :documentation "Radians above the horizon; Z is up.")
   (field-of-view
    :initarg :field-of-view
    :initform (* 70.0 (/ pi 180))
    :accessor camera-field-of-view)))

(defun make-fly-camera (&key (position (vec3:make-vec3 46.0 2.0 15.0))
                          (yaw 2.15) (pitch -0.22)
                          (field-of-view (* 70.0 (/ pi 180))))
  (make-instance 'fly-camera :position position :yaw yaw :pitch pitch
                             :field-of-view field-of-view))

(defun camera-basis (camera)
  "Return the camera's RIGHT, UP, and FORWARD unit vectors in a Z-up world."
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3:make-vec3 (* (cos yaw) (cos pitch))
                                  (* (sin yaw) (cos pitch))
                                  (sin pitch)))
         (right (vec3:make-vec3 (sin yaw) (- (cos yaw)) 0.0))
         (up (vec3:vec3-cross right forward)))
    (values right up forward)))

(defparameter *near-distance* 0.1)
(defparameter *far-distance* 400.0)
(defparameter *sun-direction*
  (vec3:vec3-normalize (vec3:make-vec3 0.52 0.30 0.62))
  "The direction toward the key light, low enough to model the terraces.")
(defparameter *sun-color* (vec3:make-vec3 1.05 0.96 0.82)
  "The key light's radiance, warm as afternoon sun.")
(defparameter *sheen-strength* 0.16
  "How brightly a face catches the sun's reflection; chamfers show it most.")
(defparameter *fill-direction*
  (vec3:vec3-normalize (vec3:make-vec3 -0.62 -0.55 0.24))
  "The direction toward the cool fill light opposite the sun.")
(defparameter *fill-strength* 0.30
  "The fill light's strength, which separates the faces the sun misses.")
(defparameter *ambient-light* 0.42
  "The strength of the ambient hemisphere: sky above, bounce below.")
(defparameter *ground-color* (vec3:make-vec3 0.34 0.30 0.24)
  "The bounce colour of the lower hemisphere.")
(defparameter *top-color* (vec3:make-vec3 0.17 0.36 0.11)
  "The material of an upward face: turf, in linear light.")
(defparameter *side-color* (vec3:make-vec3 0.42 0.32 0.21)
  "The material of a sideways face: the earth a cut exposes.")
(defparameter *bottom-color* (vec3:make-vec3 0.11 0.10 0.10)
  "The material of a downward face: an underside, seen rarely.")
(defparameter *shadow-strength* 1.0
  "How darkly the sun's walk shadows a point; zero switches shadows off.")
(defparameter *occlusion-strength* 0.75
  "How deeply the crowding of nearby cells darkens the ambient hemisphere.")
(defparameter *wear-strength* 0.6
  "How strongly the :FIELD style lightens ridges and darkens hollows.")
(defparameter *ink-width* 2.5
  "The :INK style's line width in pixels of the rendered frame.")
(defparameter *exposure* 1.15
  "Exposure of the 1 - exp(-x) curve the lit colour rolls off through.")
(defparameter *sky-color* (vec3:make-vec3 0.56 0.71 0.90)
  "The colour of the horizon, which the fog also converges to.

Deep enough that stone reads against it: a sky written at the value the eye
takes off a photograph leaves every building silhouetted on nothing.")
(defparameter *draw-sky* t
  "Whether the background is the gradient sky pass or the flat clear colour.")
(defparameter *focus-distance* 40.0
  "How far the lens is focused, in cells; also the alpha channel's scale.")
(defparameter *aperture* 0.0
  "How strongly the focus pass softens the distance; zero is a pinhole.")
(defparameter *fog-distance* 240.0
  "How far away the world has gone entirely to sky.

Far enough that a sixty-cell world does not dissolve at its own horizon:
fog is for saying that distance exists, not for hiding the far bank.")
(defparameter *bevel-radius* 0.22
  "The :BEVEL style's crease-rounding radius in cells, below one half.")
(defparameter *chamfer-width* 0.11
  "The :CHAMFER style's 45-degree crease relief in cells.

Wide enough that the planed facet reads as a face of its own and catches
the light as a band rather than a hairline, and still far short of the old
0.22-cell coves that made the world look carved.")
(defparameter *arris-softness* 0.004
  "The narrow shading transition where a chamfer meets its original face.")
(defparameter *field-vertical-radius* nil
  "The :FIELD style's tent half-width along Z, or NIL for *BEVEL-RADIUS*.

Wider than the horizontal radius, it rounds the edges of floors and roofs
more than the edges of walls, the way weather wears a top.")

;;; ------------------------------------------------------------------------
;;; Lights: the hour a picture is taken at
;;;
;;; The knobs above are the whole of the atelier's light, and setting eleven
;;; of them by hand is not how anyone chooses an hour.  A light names a set
;;; of them together -- where the sun is, what colour it is, what the sky
;;; does, how far one can see -- so that a contact sheet can put the same
;;; world at four times of day beside itself, and so that a picture can be
;;; composed by naming a light rather than by tuning a lamp.
;;;
;;; A slot left NIL keeps whatever the special above says, so :AFTERNOON,
;;; which names nothing, is exactly the atelier's own light and every knob
;;; still works by hand.  #KG0EG6

(defclass light ()
  ((name :initarg :name :reader light-name)
   (sun :initarg :sun :initform nil :reader light-sun
        :documentation "The direction toward the key light, as three floats.")
   (sun-color :initarg :sun-color :initform nil :reader light-sun-color)
   (sky :initarg :sky :initform nil :reader light-sky)
   (ground :initarg :ground :initform nil :reader light-ground)
   (fill :initarg :fill :initform nil :reader light-fill)
   (fill-strength :initarg :fill-strength :initform nil
                  :reader light-fill-strength)
   (ambient :initarg :ambient :initform nil :reader light-ambient)
   (exposure :initarg :exposure :initform nil :reader light-exposure)
   (sheen :initarg :sheen :initform nil :reader light-sheen)
   (fog :initarg :fog :initform nil :reader light-fog)
   (shadow :initarg :shadow :initform nil :reader light-shadow)
   (occlusion :initarg :occlusion :initform nil :reader light-occlusion))
  (:documentation "An hour of the day, as a set of the atelier's light knobs."))

(defvar *light-table* (make-hash-table :test 'eq)
  "Every defined light, by name.")

(defmacro define-light (name &body initargs)
  "Define or redefine the light called NAME from INITARGS."
  `(setf (gethash ,name *light-table*)
         (make-instance 'light :name ,name ,@initargs)))

(defun find-light (name)
  "The light called NAME, or an error naming what there is."
  (or (gethash name *light-table*)
      (error "No light ~S; there is ~{~S~^, ~}." name (light-names))))

(defun light-names ()
  "Every defined light's name, in alphabetical order."
  (sort (loop for name being the hash-keys of *light-table* collect name)
        #'string< :key #'symbol-name))

(defparameter *light* :afternoon
  "The light every frame is drawn under.")

(defun light-direction (list)
  "A unit direction from a list of three floats."
  (vec3:vec3-normalize
   (apply #'vec3:make-vec3
          (mapcar (lambda (x) (coerce x 'single-float)) list))))

(defun light-colour (list)
  (apply #'vec3:make-vec3
         (mapcar (lambda (x) (coerce x 'single-float)) list)))

(define-light :afternoon
  ;; The atelier's own light, named so that a sheet can ask for it: a warm
  ;; sun about thirty degrees up, a cool fill from the opposite quarter.
  )

(define-light :morning
  ;; Low from the east, the air still cool and clear, shadows long enough
  ;; to draw the plan of a building on the ground beside it.
  :sun '(-0.78 0.36 0.30) :sun-color '(1.02 0.94 0.86)
  :sky '(0.60 0.74 0.94) :fill '(0.55 -0.45 0.35) :fill-strength 0.26
  :ambient 0.40 :fog 320.0 :exposure 1.12)

(define-light :noon
  ;; Almost overhead: tops blaze, walls fall away, and every shadow is a
  ;; small hard pool underneath the thing that casts it.
  :sun '(0.18 0.12 0.97) :sun-color '(1.12 1.06 0.98)
  :sky '(0.55 0.72 0.96) :fill '(-0.4 -0.4 0.2) :fill-strength 0.22
  :ambient 0.46 :fog 360.0 :exposure 1.05 :sheen 0.20)

(define-light :evening
  ;; The sun nearly down and very warm; the sky behind it goes rose, the
  ;; shadows go blue, and the fog closes in the distance.
  :sun '(0.86 -0.28 0.16) :sun-color '(1.35 0.86 0.54)
  :sky '(0.62 0.60 0.72) :ground '(0.30 0.22 0.20)
  :fill '(-0.55 0.42 0.30) :fill-strength 0.34
  :ambient 0.34 :fog 170.0 :exposure 1.25 :sheen 0.26)

(define-light :overcast
  ;; No sun to speak of: everything is the sky, occlusion does all the
  ;; drawing, and the world reads as form rather than as light.
  :sun '(0.10 0.15 0.98) :sun-color '(0.26 0.27 0.29)
  :sky '(0.70 0.72 0.76) :ground '(0.30 0.30 0.29)
  :fill '(-0.3 -0.3 0.6) :fill-strength 0.34
  :ambient 0.86 :fog 210.0 :exposure 0.92 :shadow 0.18 :occlusion 1.0
  :sheen 0.04)

(defmacro with-light ((name) &body body)
  "Evaluate BODY under the light called NAME."
  `(let ((*light* ,name)) ,@body))

(defun material-albedo (material face)
  "MATERIAL's colour for an upward, sideways, or downward FACE.

A material that does not name a colour takes the world's own, so a material
may speak only of its finish and its figure."
  (flet ((colour (list) (apply #'vec3:make-vec3
                               (mapcar (lambda (x) (coerce x 'single-float))
                                       list))))
    (ecase face
      (:top (let ((own (material-top material)))
              (if own (colour own) *top-color*)))
      (:side (let ((own (material-side material)))
               (if own (colour own) *side-color*)))
      (:bottom (let ((own (material-bottom material)))
                 (if own (colour own) *bottom-color*))))))

(defun stock-table-data (stocks)
  "The table the :STOCK style indexes with a site's four stock bits.

Every slot takes +STOCK-LANES+ vec4s: the three albedos, then the five
lanes of MATERIAL-LANES.  STOCKS is a scene's palette of material names;
a scene with none is drawn wholly in *MATERIAL*, and slots past the end of
the palette repeat slot zero so a stale site can never read rubbish."
  (let* ((names (if (and stocks (plusp (length stocks)))
                    stocks
                    (vector *material*)))
         (data (make-array (* 4 shaders:+stock-lanes+ shaders:+stock-slots+)
                           :element-type 'single-float :initial-element 0.0))
         (index 0))
    (flet ((quad (floats)
             (loop for value in floats
                   do (setf (aref data index) (coerce value 'single-float))
                      (incf index))))
      (dotimes (slot shaders:+stock-slots+ data)
        (let* ((name (aref names (if (< slot (length names)) slot 0)))
               (material (find-material name)))
          ;; The albedo lanes' fourth components are spare; the first
          ;; carries the grain's pith spacing.
          (loop for face in '(:top :side :bottom)
                for spare in (list (material-spacing material) 0.0 0.0)
                for colour = (material-albedo material face)
                do (quad (list (vec3:vec3-x colour) (vec3:vec3-y colour)
                               (vec3:vec3-z colour) spare)))
          (mapc #'quad (material-lanes material)))))))

(defun frame-uniform-data
    (camera width height &optional domain (surface-width *bevel-radius*)
                                  (surface-detail *arris-softness*))
  "Pack the frame block: camera, basis, projection, sun, sky, and domain lanes.

SURFACE-WIDTH is the style's rounding radius or chamfer width, and
SURFACE-DETAIL its second lane: the arris softness of a chamfer, or the
vertical radius of the field."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (let* ((light (find-light *light*))
           (sun (if (light-sun light) (light-direction (light-sun light))
                    *sun-direction*))
           (sun-colour (if (light-sun-color light)
                           (light-colour (light-sun-color light)) *sun-color*))
           (sky-colour (if (light-sky light)
                           (light-colour (light-sky light)) *sky-color*))
           (ground-colour (if (light-ground light)
                              (light-colour (light-ground light))
                              *ground-color*))
           (fill-direction (if (light-fill light)
                               (light-direction (light-fill light))
                               *fill-direction*))
           (fill-strength (or (light-fill-strength light) *fill-strength*))
           (ambient (or (light-ambient light) *ambient-light*))
           (exposure (or (light-exposure light) *exposure*))
           (sheen (or (light-sheen light) *sheen-strength*))
           (fog (or (light-fog light) *fog-distance*))
           (shadow (or (light-shadow light) *shadow-strength*))
           (occlusion (or (light-occlusion light) *occlusion-strength*))
           (near *near-distance*)
           (far *far-distance*)
           (focal (/ (tan (/ (camera-field-of-view camera) 2.0))))
           (aspect (/ (coerce width 'single-float) height))
           (data (make-array 64 :element-type 'single-float))
           (index 0))
      (flet ((lane (vector fourth)
               (setf (aref data index) (coerce (vec3:vec3-x vector) 'single-float)
                     (aref data (+ index 1))
                     (coerce (vec3:vec3-y vector) 'single-float)
                     (aref data (+ index 2))
                     (coerce (vec3:vec3-z vector) 'single-float)
                     (aref data (+ index 3)) (coerce fourth 'single-float))
               (incf index 4)))
        (lane (camera-position camera) 0.0)
        (lane right 0.0)
        (lane up 0.0)
        (lane forward 0.0)
        (lane (vec3:make-vec3 (/ focal aspect) focal (/ far (- far near)))
              (/ (- (* far near)) (- far near)))
        (lane sun ambient)
        (lane sky-colour fog)
        (lane (vec3:make-vec3
               (if domain (luft:world-domain-x-period domain) 1)
               (if domain (luft:world-domain-y-period domain) 1)
               surface-width)
              surface-detail)
        (lane sun-colour sheen)
        (lane fill-direction fill-strength)
        (lane ground-colour exposure)
        (lane (vec3:make-vec3 occlusion shadow *wear-strength*)
              *ink-width*)
        (lane *top-color* 0.0)
        (lane *side-color* 0.0)
        (lane *bottom-color* 0.0)
        (lane (vec3:make-vec3 *focus-distance* *aperture*
                              (/ 1.0 (max 1 width)))
              (/ 1.0 (max 1 height))))
      data)))

;;; ------------------------------------------------------------------------
;;; Renderer

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   (owns-device-p :initarg :owns-device-p :initform nil
                  :reader renderer-owns-device-p)
   (scene :initarg :scene :accessor renderer-scene)
   (camera :initarg :camera :accessor renderer-camera)
   (extent :initarg :extent :accessor renderer-extent)
   (color-format :initarg :color-format :reader renderer-color-format)
   (color-texture :initform nil :accessor renderer-color-texture)
   (color-view :initform nil :accessor renderer-color-view)
   (depth-texture :initform nil :accessor renderer-depth-texture)
   (depth-view :initform nil :accessor renderer-depth-view)
   (scene-texture :initform nil :accessor renderer-scene-texture)
   (scene-view :initform nil :accessor renderer-scene-view)
   (sampler :initform nil :accessor renderer-sampler)
   (lens-layout :initform nil :accessor renderer-lens-layout)
   (lens-bind-group :initform nil :accessor renderer-lens-bind-group)
   (uniform-buffer :initform nil :accessor renderer-uniform-buffer)
   (sites-buffer :initform nil :accessor renderer-sites-buffer)
   (bricks-buffer :initform nil :accessor renderer-bricks-buffer)
   (cells-buffer :initform nil :accessor renderer-cells-buffer)
   (stocks-buffer :initform nil :accessor renderer-stocks-buffer)
   (sites-capacity :initform 0 :accessor renderer-sites-capacity)
   (bricks-capacity :initform 0 :accessor renderer-bricks-capacity)
   (cells-capacity :initform 0 :accessor renderer-cells-capacity)
   (layout :initform nil :accessor renderer-layout)
   (bind-group :initform nil :accessor renderer-bind-group)
   (modules :initform nil :accessor renderer-modules)
   (pipelines :initform nil :accessor renderer-pipelines
              :documentation "A plist from style or effect to its pipeline.")
   (pipeline-styles :initarg :pipeline-styles
                    :initform '(:flat :bevel :chamfer :paper)
                    :reader renderer-pipeline-styles
                    :documentation
                    "Surface styles whose shader modules and pipelines exist.")
   (effects :initarg :effects :initform '(:sky :lens)
            :reader renderer-effects
            :documentation
            "Optional passes whose shader modules and pipelines exist.")
   (style :initarg :style :initform :bevel :accessor renderer-style
          :documentation
          "Which pipeline draws: :FLAT, :BEVEL (rounded), :CHAMFER, or :PAPER.")
   (technique :initarg :technique :initform :vertex :reader renderer-technique
              :documentation
              "How sites become triangles: :VERTEX pulling or :MESH shaders.")
   (uploaded-scene :initform nil :accessor renderer-uploaded-scene))
  (:documentation "GPU resources drawing one scene from one camera."))

(defun frame-uniform-size ()
  (let ((size (spv:shader-uniform-block-byte-size (shaders:frame-uniform-block))))
    (unless (= size (* 4 (length (frame-uniform-data (make-fly-camera) 1 1))))
      (error "Frame block is ~D bytes but the host packs ~D."
             size (* 4 (length (frame-uniform-data (make-fly-camera) 1 1)))))
    size))

(defun renderer-pipeline (renderer &optional (style (renderer-style renderer)))
  (or (getf (renderer-pipelines renderer) style)
      (error "Renderer has no ~S pipeline." style)))

(defun renderer-effect-p (renderer effect)
  (not (null (member effect (renderer-effects renderer)))))

(defmacro with-renderer-creation-step ((zone label) &body body)
  "Trace and synchronously log one cold LUFT driver transaction.

The BEGIN breadcrumb is forced to the image log before BODY enters the driver.
COMPLETE proves that it returned; INTERRUPTED means an ordinary non-local exit
unwound through Lisp.  If the process or GPU disappears, BEGIN deliberately
remains the last durable line."
  (let ((completed-p (gensym "COMPLETED-P")))
    `(with-cpu-trace-zone (,zone)
       (let ((,completed-p nil))
         (log-event :luft "begin ~A" ,label)
         (unwind-protect
              (multiple-value-prog1 (progn ,@body)
                (setf ,completed-p t)
                (log-event :luft "complete ~A" ,label))
           (unless ,completed-p
             (log-event :luft "interrupted ~A" ,label)))))))

(zdefun (create-renderer-targets :zone :luft/create-renderer-targets) (renderer)
  (let* ((device (renderer-device renderer))
         (extent (renderer-extent renderer))
         (color (create device
                        (make-texture-descriptor
                         :label "luft surface color"
                         :size extent :dimensions :2d
                         :format (renderer-color-format renderer)
                         :usage '(:render-attachment :copy-src))))
         (depth (create device
                        (make-texture-descriptor
                         :label "luft surface depth"
                         :size extent :dimensions :2d
                         :format :depth32-float
                         :usage '(:render-attachment))))
         ;; The world is drawn here and read by the focus pass; COLOR is what
         ;; the focus pass writes and what a capture copies out.
         (scene (create device
                        (make-texture-descriptor
                         :label "luft scene color"
                         :size extent :dimensions :2d
                         :format (renderer-color-format renderer)
                         :usage '(:render-attachment :texture-binding)))))
    (setf (renderer-color-texture renderer) color
          (renderer-color-view renderer)
          (create device (make-texture-view-descriptor :texture color))
          (renderer-depth-texture renderer) depth
          (renderer-depth-view renderer)
          (create device (make-texture-view-descriptor :texture depth))
          (renderer-scene-texture renderer) scene
          (renderer-scene-view renderer)
          (create device (make-texture-view-descriptor :texture scene))
          (renderer-sampler renderer)
          (create device (make-sampler-descriptor
                          :label "luft scene sampler"
                          :mag-filter :linear :min-filter :linear)))))

(zdefun (ensure-renderer-extent :zone :luft/ensure-renderer-extent)
    (renderer extent)
  "Replace RENDERER's frame-sized targets when EXTENT has changed.

This runs inside the canvas frame callback, after drawable acquisition has
synchronized the presentation context's extent.  New resources are assembled
before publication; destroying the outgoing handles is safe while older GPU
work is in flight because the GPU abstraction defers their native teardown."
  (unless (equal extent (renderer-extent renderer))
    (log-event :luft "reframing ~{~D~^x~} to ~{~D~^x~}"
               (renderer-extent renderer) extent)
    (let ((device (renderer-device renderer))
          color color-view depth depth-view scene scene-view lens-bind-group
          (completed-p nil))
      (unwind-protect
           (progn
             (setf color
                   (create device
                           (make-texture-descriptor
                            :label "luft surface color"
                            :size extent :dimensions :2d
                            :format (renderer-color-format renderer)
                            :usage '(:render-attachment :copy-src)))
                   color-view
                   (create device (make-texture-view-descriptor
                                   :texture color))
                   depth
                   (create device
                           (make-texture-descriptor
                            :label "luft surface depth"
                            :size extent :dimensions :2d
                            :format :depth32-float
                            :usage '(:render-attachment)))
                   depth-view
                   (create device (make-texture-view-descriptor
                                   :texture depth))
                   scene
                   (create device
                           (make-texture-descriptor
                            :label "luft scene color"
                            :size extent :dimensions :2d
                            :format (renderer-color-format renderer)
                            :usage '(:render-attachment :texture-binding)))
                   scene-view
                   (create device (make-texture-view-descriptor
                                   :texture scene)))
             (when (renderer-effect-p renderer :lens)
               (setf lens-bind-group
                     (create
                      device
                      (make-bind-group-descriptor
                       :label "luft lens bindings"
                       :layout (renderer-lens-layout renderer)
                       :entries
                       `((:binding ,shaders:+scene-binding+
                          :resource ,scene-view)
                         (:binding ,shaders:+sampler-binding+
                          :resource ,(renderer-sampler renderer))
                         (:binding ,shaders:+lens-frame-binding+
                          :resource ,(renderer-uniform-buffer renderer)))))))
             (let ((old-resources
                     (list (renderer-lens-bind-group renderer)
                           (renderer-scene-view renderer)
                           (renderer-scene-texture renderer)
                           (renderer-color-view renderer)
                           (renderer-color-texture renderer)
                           (renderer-depth-view renderer)
                           (renderer-depth-texture renderer))))
               (setf (renderer-extent renderer) extent
                     (renderer-color-texture renderer) color
                     (renderer-color-view renderer) color-view
                     (renderer-depth-texture renderer) depth
                     (renderer-depth-view renderer) depth-view
                     (renderer-scene-texture renderer) scene
                     (renderer-scene-view renderer) scene-view
                     (renderer-lens-bind-group renderer) lens-bind-group
                     completed-p t)
               (dolist (resource old-resources)
                 (when resource (destroy resource)))))
        (unless completed-p
          (dolist (resource (list lens-bind-group scene-view scene color-view
                                  color depth-view depth))
            (when resource (ignore-errors (destroy resource))))))))
  renderer)

;;; ------------------------------------------------------------------------
;;; Techniques: how the surface chain becomes triangles
;;;
;;; Two ways exist to turn the uploaded site buffer into rasterized faces.
;;; The :MESH technique dispatches task and mesh workgroups over bricks of
;;; sites (#VAABY9); it needs VK_EXT_mesh_shader, which not every driver runs
;;; as well as it advertises.  The :VERTEX technique draws K vertices per
;;; site with no vertex buffer and lets each vertex shader invocation pull its
;;; own site out of the very same buffer (luft/render/vertex-shaders.lisp);
;;; it runs on any device.  The fragment stages, the frame block, the bind
;;; group, and every frame-sized target are shared, so a technique is only
;;; which modules are created, how a pipeline is described, and how a pass
;;; is told to draw.  The names are the identity, and the three protocols
;;; below are EQL methods on them.

(defparameter *default-technique* :vertex
  "The technique a renderer uses unless told otherwise.

:VERTEX pulls sites in a vertex shader and runs on any Vulkan device; :MESH
amplifies bricks with task and mesh shaders and needs VK_EXT_mesh_shader.")

(defgeneric technique-styles (technique)
  (:documentation
   "The surface styles TECHNIQUE can draw, in the order a menu would list."))

(defmethod technique-styles ((technique (eql :mesh)))
  '(:flat :bevel :chamfer :paper :stock))

(defmethod technique-styles ((technique (eql :vertex)))
  '(:flat :bevel :chamfer :paper :stock :field :soft :ink))

(defgeneric create-technique-pipelines (technique renderer)
  (:documentation
   "Create the shader modules and pipelines RENDERER's styles and effects need.

Modules are pushed onto RENDERER-MODULES and pipelines installed under their
style or effect name in RENDERER-PIPELINES; the bind group layouts already
exist.  Each driver call is one traced, logged creation step."))

(defgeneric draw-surface (technique pass scene style)
  (:documentation
   "Record into PASS the draw of SCENE's whole surface in STYLE.
The pipeline and bind group are already set."))

(defgeneric draw-screen (technique pass)
  (:documentation
   "Record into PASS the draw of one triangle covering the screen."))

(defun technique-style-p (technique style)
  (not (null (member style (technique-styles technique)))))

(defun create-renderer-module (renderer zone label code)
  "Create a shader module from CODE and publish it as one of RENDERER's.

Ownership is published as each driver call succeeds so MAKE-RENDERER's
unwind cleanup sees partial work."
  (let ((module (with-renderer-creation-step (zone label)
                  (create (renderer-device renderer)
                          (make-shader-module-descriptor
                           :label label
                           :language :mathematical
                           :code code)))))
    (push module (renderer-modules renderer))
    module))

(defun install-renderer-pipeline (renderer name zone label descriptor)
  "Create DESCRIPTOR's pipeline and install it as RENDERER's NAME pipeline."
  (setf (getf (renderer-pipelines renderer) name)
        (with-renderer-creation-step (zone label)
          (create (renderer-device renderer) descriptor))))

(defun surface-depth-state ()
  '(:format :depth32-float :depth-write-enabled t :depth-compare :less))

(defun background-depth-state ()
  "The sky's depth: written by nothing, tested against nothing, so the pass
may draw it first and the world still covers it."
  '(:format :depth32-float :depth-write-enabled nil :depth-compare :always))

(defun fragment-stage (renderer module)
  `(:module ,module :targets ((:format ,(renderer-color-format renderer)))))

(defun create-renderer-fragment-modules (renderer)
  "Create the fragment modules RENDERER's styles and effects share.

The values are the surface, chamfer, paper, sky, lens, field, ink, and
stock fragment modules, each NIL when nothing configured wants it."
  (let ((styles (renderer-pipeline-styles renderer)))
    (values
     (when (intersection styles '(:flat :bevel))
       (create-renderer-module renderer :luft/shader/surface-fragment
                               "luft surface fragment"
                               (shaders:surface-fragment-shader)))
     (when (member :chamfer styles)
       (create-renderer-module renderer :luft/shader/chamfer-fragment
                               "luft chamfer fragment"
                               (shaders:chamfer-fragment-shader)))
     (when (member :paper styles)
       (create-renderer-module renderer :luft/shader/paper-fragment
                               "luft paper fragment"
                               (shaders:paper-fragment-shader)))
     (when (renderer-effect-p renderer :sky)
       (create-renderer-module renderer :luft/shader/sky-fragment
                               "luft sky fragment"
                               (shaders:sky-fragment-shader)))
     (when (renderer-effect-p renderer :lens)
       (create-renderer-module renderer :luft/shader/lens-fragment
                               "luft lens fragment"
                               (shaders:lens-fragment-shader)))
     (when (intersection styles '(:field :soft))
       (create-renderer-module renderer :luft/shader/field-fragment
                               "luft field fragment"
                               (shaders:field-fragment-shader)))
     (when (member :ink styles)
       (create-renderer-module renderer :luft/shader/ink-fragment
                               "luft ink fragment"
                               (shaders:ink-fragment-shader)))
     (when (member :stock styles)
       (create-renderer-module renderer :luft/shader/stock-fragment
                               "luft stock fragment"
                               (shaders:stock-fragment-shader))))))

;;; The mesh technique: one task workgroup per brick, one mesh lane per site.

(defmethod create-technique-pipelines ((technique (eql :mesh)) renderer)
  (let ((styles (renderer-pipeline-styles renderer))
        task mesh bevel chamfer sky-mesh)
    (when styles
      (setf task (create-renderer-module renderer :luft/shader/surface-task
                                         "luft surface task"
                                         (shaders:surface-task-shader))))
    (when (member :flat styles)
      (setf mesh (create-renderer-module renderer :luft/shader/surface-mesh
                                         "luft surface mesh"
                                         (shaders:surface-mesh-shader))))
    (when (member :bevel styles)
      (setf bevel (create-renderer-module renderer :luft/shader/bevel-mesh
                                          "luft bevel mesh"
                                          (shaders:bevel-mesh-shader))))
    (when (intersection styles '(:chamfer :paper :stock))
      (setf chamfer (create-renderer-module renderer :luft/shader/chamfer-mesh
                                            "luft chamfer mesh"
                                            (shaders:chamfer-mesh-shader))))
    (when (or (renderer-effect-p renderer :sky)
              (renderer-effect-p renderer :lens))
      (setf sky-mesh (create-renderer-module renderer :luft/shader/sky-mesh
                                             "luft sky mesh"
                                             (shaders:sky-mesh-shader))))
    (multiple-value-bind (fragment chamfer-fragment paper-fragment
                          sky-fragment lens-fragment field-fragment
                          ink-fragment stock-fragment)
        (create-renderer-fragment-modules renderer)
      (declare (ignore field-fragment ink-fragment))
      (flet ((pipeline (name zone label mesh-module fragment-module
                        &key (task-module task)
                             (layout (renderer-layout renderer))
                             (depth (surface-depth-state)))
               (install-renderer-pipeline
                renderer name zone label
                (make-mesh-render-pipeline-descriptor
                 :label label
                 :layout layout
                 :task (and task-module `(:module ,task-module))
                 :mesh `(:module ,mesh-module)
                 :fragment (fragment-stage renderer fragment-module)
                 :max-mesh-workgroups 1
                 :depth-stencil depth))))
        (when (member :flat styles)
          (pipeline :flat :luft/pipeline/flat "luft surface pipeline"
                    mesh fragment))
        (when (member :bevel styles)
          (pipeline :bevel :luft/pipeline/bevel "luft bevel pipeline"
                    bevel fragment))
        (when (member :chamfer styles)
          (pipeline :chamfer :luft/pipeline/chamfer "luft chamfer pipeline"
                    chamfer chamfer-fragment))
        ;; The paper material draws the chamfered geometry: the glint it
        ;; exists for lives on the planed facets.
        (when (member :paper styles)
          (pipeline :paper :luft/pipeline/paper "luft paper pipeline"
                    chamfer paper-fragment))
        (when (member :stock styles)
          (pipeline :stock :luft/pipeline/stock "luft stock pipeline"
                    chamfer stock-fragment))
        ;; The background: no task stage to amplify, no depth to write, and
        ;; it runs before anything that would hide it.
        (when (renderer-effect-p renderer :sky)
          (pipeline :sky :luft/pipeline/sky "luft sky pipeline"
                    sky-mesh sky-fragment
                    :task-module nil
                    :depth (background-depth-state)))
        ;; The lens draws the frame the world was drawn into, so it binds a
        ;; group of textures rather than the world's sites.
        (when (renderer-effect-p renderer :lens)
          (pipeline :lens :luft/pipeline/lens "luft lens pipeline"
                    sky-mesh lens-fragment
                    :task-module nil
                    :layout (renderer-lens-layout renderer)
                    :depth nil))))))

(defmethod draw-surface ((technique (eql :mesh)) pass scene style)
  (declare (ignore style))
  (draw-mesh-workgroups pass (scene-brick-count scene)))

(defmethod draw-screen ((technique (eql :mesh)) pass)
  (draw-mesh-workgroups pass 1))

;;; The vertex technique: K vertices a site, each pulling its own site.

(defmethod create-technique-pipelines ((technique (eql :vertex)) renderer)
  (let ((styles (renderer-pipeline-styles renderer))
        surface bevel chamfer field screen)
    (when (intersection styles '(:flat :soft :ink))
      (setf surface (create-renderer-module
                     renderer :luft/shader/surface-vertex
                     "luft surface vertex"
                     (shaders:surface-vertex-shader))))
    (when (member :bevel styles)
      (setf bevel (create-renderer-module
                   renderer :luft/shader/bevel-vertex
                   "luft bevel vertex"
                   (shaders:bevel-vertex-shader))))
    (when (intersection styles '(:chamfer :paper :stock))
      (setf chamfer (create-renderer-module
                     renderer :luft/shader/chamfer-vertex
                     "luft chamfer vertex"
                     (shaders:chamfer-vertex-shader))))
    (when (member :field styles)
      (setf field (create-renderer-module
                   renderer :luft/shader/field-vertex
                   "luft field vertex"
                   (shaders:field-vertex-shader))))
    (when (or (renderer-effect-p renderer :sky)
              (renderer-effect-p renderer :lens))
      (setf screen (create-renderer-module
                    renderer :luft/shader/sky-vertex
                    "luft sky vertex"
                    (shaders:sky-vertex-shader))))
    (multiple-value-bind (fragment chamfer-fragment paper-fragment
                          sky-fragment lens-fragment field-fragment
                          ink-fragment stock-fragment)
        (create-renderer-fragment-modules renderer)
      (flet ((pipeline (name zone label vertex-module fragment-module
                        &key (layout (renderer-layout renderer))
                             (depth (surface-depth-state)))
               (install-renderer-pipeline
                renderer name zone label
                (make-render-pipeline-descriptor
                 :label label
                 :layout layout
                 :vertex `(:module ,vertex-module)
                 :fragment (fragment-stage renderer fragment-module)
                 :primitive '(:topology :triangle-list)
                 :depth-stencil depth))))
        (when (member :flat styles)
          (pipeline :flat :luft/pipeline/flat "luft surface pipeline"
                    surface fragment))
        (when (member :bevel styles)
          (pipeline :bevel :luft/pipeline/bevel "luft bevel pipeline"
                    bevel fragment))
        (when (member :chamfer styles)
          (pipeline :chamfer :luft/pipeline/chamfer "luft chamfer pipeline"
                    chamfer chamfer-fragment))
        (when (member :paper styles)
          (pipeline :paper :luft/pipeline/paper "luft paper pipeline"
                    chamfer paper-fragment))
        ;; The stock draws the chamfered geometry too: its materials are
        ;; about what a planed arris does with the light.
        (when (member :stock styles)
          (pipeline :stock :luft/pipeline/stock "luft stock pipeline"
                    chamfer stock-fragment))
        (when (member :field styles)
          (pipeline :field :luft/pipeline/field "luft field pipeline"
                    field field-fragment))
        ;; Soft: the flat quads under the field's shading, rounding as
        ;; light alone.
        (when (member :soft styles)
          (pipeline :soft :luft/pipeline/soft "luft soft pipeline"
                    surface field-fragment))
        (when (member :ink styles)
          (pipeline :ink :luft/pipeline/ink "luft ink pipeline"
                    surface ink-fragment))
        (when (renderer-effect-p renderer :sky)
          (pipeline :sky :luft/pipeline/sky "luft sky pipeline"
                    screen sky-fragment
                    :depth (background-depth-state)))
        (when (renderer-effect-p renderer :lens)
          (pipeline :lens :luft/pipeline/lens "luft lens pipeline"
                    screen lens-fragment
                    :layout (renderer-lens-layout renderer)
                    :depth nil))))))

(defmethod draw-surface ((technique (eql :vertex)) pass scene style)
  ;; The site vector is padded to whole bricks; a zero site collapses its
  ;; vertices in the shader, so the padding costs a few degenerate triangles
  ;; and no bookkeeping.
  (draw pass (* (shaders:surface-vertices-per-face style)
                (length (scene-sites scene)))))

(defmethod draw-screen ((technique (eql :vertex)) pass)
  (draw pass 3))

;;; ------------------------------------------------------------------------
;;; Creating the renderer's pipelines

(zdefun (create-renderer-layouts :zone :luft/create-renderer-layouts) (renderer)
  (let ((device (renderer-device renderer)))
    (setf (renderer-lens-layout renderer)
          (with-renderer-creation-step
              (:luft/layout/lens "luft lens layout")
            (create device
                    (make-bind-group-layout-descriptor
                     :label "luft lens layout"
                     :entries `((:binding ,shaders:+scene-binding+
                                 :type :texture)
                                (:binding ,shaders:+sampler-binding+
                                 :type :sampler)
                                (:binding ,shaders:+lens-frame-binding+
                                 :type :uniform-buffer)))))
          (renderer-layout renderer)
          (with-renderer-creation-step
              (:luft/layout/surface "luft surface layout")
            (create device
                    (make-bind-group-layout-descriptor
                     :label "luft surface layout"
                     :entries
                     `((:binding ,shaders:+frame-binding+
                        :type :uniform-buffer)
                       (:binding ,shaders:+sites-binding+
                        :type :storage-buffer)
                       (:binding ,shaders:+bricks-binding+
                        :type :storage-buffer)
                       (:binding ,shaders:+cells-binding+
                        :type :storage-buffer)
                       (:binding ,shaders:+stocks-binding+
                        :type :storage-buffer))))))))

(defun create-lens-bind-group (renderer)
  (with-renderer-creation-step (:luft/bindings/lens "luft lens bindings")
    (create (renderer-device renderer)
            (make-bind-group-descriptor
             :label "luft lens bindings"
             :layout (renderer-lens-layout renderer)
             :entries
             `((:binding ,shaders:+scene-binding+
                :resource ,(renderer-scene-view renderer))
               (:binding ,shaders:+sampler-binding+
                :resource ,(renderer-sampler renderer))
               (:binding ,shaders:+lens-frame-binding+
                :resource ,(renderer-uniform-buffer renderer)))))))

(zdefun (create-renderer-pipeline :zone :luft/create-renderer-pipeline) (renderer)
  (create-renderer-layouts renderer)
  (create-technique-pipelines (renderer-technique renderer) renderer)
  (when (renderer-effect-p renderer :lens)
    (setf (renderer-lens-bind-group renderer)
          (create-lens-bind-group renderer))))

(zdefun (make-renderer :zone :luft/make-renderer)
    (&key scene camera device
          (provider *gpu-provider*)
          (width 1280) (height 800)
          (color-format :rgba8-unorm-srgb)
          (technique *default-technique*)
          (style (if (technique-style-p technique :bevel) :bevel :chamfer))
          (pipeline-styles (technique-styles technique))
          (effects '(:sky :lens)))
  "Create every GPU object needed to draw SCENE from CAMERA at WIDTH by HEIGHT.

TECHNIQUE is :VERTEX, which pulls sites in a vertex shader on any device, or
:MESH, which dispatches task and mesh shaders where VK_EXT_mesh_shader works.
STYLE is :FLAT, :BEVEL (rounded), :CHAMFER (subtle planar crease
relief), or :PAPER (the chamfered geometry in a matte, toothed material), and
may be changed later to a member of PIPELINE-STYLES, which defaults to every
style the technique draws.  Only those surface pipelines and the optional
:SKY and :LENS EFFECTS are created; NIL/NIL is a clear-only renderer useful
for reducing a suspect GPU frame to its presentation core.  Without DEVICE,
one is requested from PROVIDER and owned by the renderer."
  (unless (or (null pipeline-styles) (member style pipeline-styles))
    (error "Renderer style ~S is absent from PIPELINE-STYLES ~S."
           style pipeline-styles))
  (let ((foreign (set-difference pipeline-styles (technique-styles technique))))
    (when foreign
      (error "The ~S technique cannot draw ~S; it draws ~S."
             technique foreign (technique-styles technique))))
  (let* ((owns-device-p (null device))
         (device (or device
                     (request-gpu-device
                      provider (make-device-descriptor :label "luft atelier"))))
         (renderer (make-instance 'renderer
                                  :device device :owns-device-p owns-device-p
                                  :scene scene :camera camera
                                  :extent (list width height)
                                  :color-format color-format
                                  :technique technique
                                  :style style
                                  :pipeline-styles pipeline-styles
                                  :effects effects))
         (completed-p nil))
    (unwind-protect
         (progn
           (create-renderer-targets renderer)
           (setf (renderer-uniform-buffer renderer)
                 (create device
                         (make-buffer-descriptor
                          :label "luft frame block"
                          :size (frame-uniform-size)
                          :usage '(:uniform)))
                 ;; The stock table is small, rewritten every frame, and
                 ;; bound whether or not the drawing style reads it.
                 (renderer-stocks-buffer renderer)
                 (create device
                         (make-buffer-descriptor
                          :label "luft stock table"
                          :size (* 4 4 shaders:+stock-lanes+
                                   shaders:+stock-slots+)
                          :usage '(:storage))))
           (create-renderer-pipeline renderer)
           (upload-scene renderer)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (destroy-renderer renderer)))))

(defun destroy-renderer (renderer)
  "Release every GPU object of RENDERER, and its device when it owns one."
  (dolist (resource (list* (renderer-bind-group renderer)
                           (renderer-layout renderer)
                           (loop for (nil pipeline) on (renderer-pipelines renderer)
                                   by #'cddr
                                 collect pipeline)))
    (when resource (ignore-errors (destroy resource))))
  (dolist (module (renderer-modules renderer))
    (ignore-errors (destroy module)))
  (dolist (resource (list (renderer-lens-bind-group renderer)
                          (renderer-lens-layout renderer)
                          (renderer-sampler renderer)
                          (renderer-scene-view renderer)
                          (renderer-scene-texture renderer)
                          (renderer-sites-buffer renderer)
                          (renderer-bricks-buffer renderer)
                          (renderer-cells-buffer renderer)
                          (renderer-stocks-buffer renderer)
                          (renderer-uniform-buffer renderer)
                          (renderer-color-view renderer)
                          (renderer-color-texture renderer)
                          (renderer-depth-view renderer)
                          (renderer-depth-texture renderer)))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-bind-group renderer) nil
        (renderer-pipelines renderer) nil
        (renderer-layout renderer) nil
        (renderer-modules renderer) nil
        (renderer-sites-buffer renderer) nil
        (renderer-bricks-buffer renderer) nil
        (renderer-cells-buffer renderer) nil
        (renderer-stocks-buffer renderer) nil
        (renderer-uniform-buffer renderer) nil)
  (when (renderer-owns-device-p renderer)
    (ignore-errors (destroy (renderer-device renderer))))
  (values))

(defun ensure-storage-buffer (renderer accessor capacity-accessor
                              needed label)
  "Return a storage buffer of at least NEEDED bytes, recreating on growth.

The second value is true when a new buffer was created."
  (let ((buffer (funcall accessor renderer)))
    (if (and buffer (<= needed (funcall capacity-accessor renderer)))
        (values buffer nil)
        (let ((new (create (renderer-device renderer)
                           (make-buffer-descriptor
                            :label label :size needed
                            :usage '(:storage)))))
          (when buffer (destroy buffer))
          (funcall (fdefinition `(setf ,accessor)) new renderer)
          (funcall (fdefinition `(setf ,capacity-accessor)) needed renderer)
          (values new t)))))

(zdefun (upload-scene :zone :luft/upload-scene
                       :value (scene-brick-count scene))
    (renderer &optional (scene (renderer-scene renderer)))
  "Upload SCENE's sites and brick spheres, rebinding when buffers grow."
  (let* ((sites (scene-sites scene))
         (bricks (scene-bricks scene))
         (rebind-p (null (renderer-bind-group renderer))))
    (multiple-value-bind (sites-buffer new-p)
        (ensure-storage-buffer renderer 'renderer-sites-buffer
                               'renderer-sites-capacity
                               (* 8 (length sites)) "luft surface sites")
      (when new-p (setf rebind-p t))
      (write-buffer sites-buffer sites))
    (multiple-value-bind (bricks-buffer new-p)
        (ensure-storage-buffer renderer 'renderer-bricks-buffer
                               'renderer-bricks-capacity
                               (* 4 (length bricks)) "luft surface bricks")
      (when new-p (setf rebind-p t))
      (write-buffer bricks-buffer bricks))
    (multiple-value-bind (cells-buffer new-p)
        (ensure-storage-buffer renderer 'renderer-cells-buffer
                               'renderer-cells-capacity
                               (* 4 (length (scene-cell-bits scene)))
                               "luft solid cells")
      (when new-p (setf rebind-p t))
      (write-buffer cells-buffer (scene-cell-bits scene)))
    (when rebind-p
      (when (renderer-bind-group renderer)
        (destroy (renderer-bind-group renderer)))
      (setf (renderer-bind-group renderer)
            (create (renderer-device renderer)
                    (make-bind-group-descriptor
                     :label "luft surface bindings"
                     :layout (renderer-layout renderer)
                     :entries
                     `((:binding ,shaders:+frame-binding+
                        :resource ,(renderer-uniform-buffer renderer))
                       (:binding ,shaders:+sites-binding+
                        :resource ,(renderer-sites-buffer renderer))
                       (:binding ,shaders:+bricks-binding+
                        :resource ,(renderer-bricks-buffer renderer))
                       (:binding ,shaders:+cells-binding+
                        :resource ,(renderer-cells-buffer renderer))
                       (:binding ,shaders:+stocks-binding+
                        :resource ,(renderer-stocks-buffer renderer)))))))
    (setf (renderer-scene renderer) scene
          (renderer-uploaded-scene renderer) scene)
    renderer))

(zdefun (encode-frame :zone :luft/encode-frame) (renderer encoder)
  "Encode one frame of RENDERER's scene into its color texture on ENCODER."
  (let* ((extent (renderer-extent renderer))
         (scene (renderer-scene renderer))
         (technique (renderer-technique renderer))
         (light (find-light *light*))
         (sky (if (light-sky light) (light-colour (light-sky light))
                  *sky-color*)))
    (unless (eq scene (renderer-uploaded-scene renderer))
      (upload-scene renderer scene))
    (write-buffer (renderer-stocks-buffer renderer)
                  (stock-table-data (scene-stocks scene)))
    (write-buffer (renderer-uniform-buffer renderer)
                  (frame-uniform-data (renderer-camera renderer)
                                      (first extent) (second extent)
                                      (scene-domain scene)
                                      (if (member (renderer-style renderer)
                                                  '(:chamfer :paper :stock))
                                          *chamfer-width*
                                          *bevel-radius*)
                                      (if (member (renderer-style renderer)
                                                  '(:field :soft :ink))
                                          (or *field-vertical-radius*
                                              *bevel-radius*)
                                          *arris-softness*)))
    (let* ((surface-pipeline
             (getf (renderer-pipelines renderer) (renderer-style renderer)))
           (sky-pipeline (getf (renderer-pipelines renderer) :sky))
           (lens-pipeline (getf (renderer-pipelines renderer) :lens))
           (lens-p (and lens-pipeline (plusp *aperture*)))
           ;; With a lens the world is drawn into the scene texture and the
           ;; focus pass writes the capture target; without one the world
           ;; draws straight into it and no frame is copied twice.
           (target (if lens-p
                       (renderer-scene-view renderer)
                       (renderer-color-view renderer)))
           (pass (begin-render-pass
                  encoder
                  (make-render-pass-descriptor
                   :label "luft surface pass"
                   :color-attachments
                   `((:view ,target
                      :load-op :clear :store-op :store
                      :clear-value ,(vector (vec3:vec3-x sky) (vec3:vec3-y sky)
                                            (vec3:vec3-z sky) 1.0)))
                   :depth-stencil-attachment
                   `(:view ,(renderer-depth-view renderer)
                     :depth-load-op :clear :depth-store-op :discard
                     :depth-clear-value 1.0)))))
      (when (and *draw-sky* sky-pipeline)
        (set-pipeline pass sky-pipeline)
        (set-bind-group pass 0 (renderer-bind-group renderer))
        (draw-screen technique pass))
      (when surface-pipeline
        (set-pipeline pass surface-pipeline)
        (set-bind-group pass 0 (renderer-bind-group renderer))
        (draw-surface technique pass scene (renderer-style renderer)))
      (end-pass pass)
      (when lens-p
        (prepare-texture encoder (renderer-scene-texture renderer)
                         :texture-binding)
        (let ((lens (begin-render-pass
                     encoder
                     (make-render-pass-descriptor
                      :label "luft lens pass"
                      :color-attachments
                      `((:view ,(renderer-color-view renderer)
                         :load-op :clear :store-op :store
                         :clear-value #(0.0 0.0 0.0 1.0)))))))
          (set-pipeline lens lens-pipeline)
          (set-bind-group lens 0 (renderer-lens-bind-group renderer))
          (draw-screen technique lens)
          (end-pass lens))))
    (renderer-color-texture renderer)))

(defun render-pixels (renderer)
  "Render one frame headlessly and return its packed pixel bytes.

The further values are the width, height, and colour format of the pixels."
  (let* ((device (renderer-device renderer))
         (extent (renderer-extent renderer))
         (readback (create device
                           (make-buffer-descriptor
                            :label "luft surface readback"
                            :size (* 4 (first extent) (second extent))
                            :usage '(:copy-dst))))
         (encoder nil)
         (command-buffer nil))
    (unwind-protect
         (progn
           (setf encoder (create device
                                 (make-command-encoder-descriptor
                                  :label "luft surface frame")))
           (encode-frame renderer encoder)
           (encode encoder
                   (make-gpu-copy-texture-to-buffer-command
                    :source (renderer-color-texture renderer)
                    :destination readback))
           (setf command-buffer (finish encoder))
           (submit (device-queue device) command-buffer)
           (values (read-buffer readback)
                   (first extent) (second extent)
                   (renderer-color-format renderer)))
      (when command-buffer (destroy command-buffer))
      (when encoder (destroy encoder))
      (destroy readback))))

(defparameter *srgb-to-linear*
  (let ((table (make-array 256 :element-type 'single-float)))
    (dotimes (index 256 table)
      (let ((value (/ (float index 1.0) 255.0)))
        (setf (aref table index)
              (if (<= value 0.04045)
                  (/ value 12.92)
                  (expt (/ (+ value 0.055) 1.055) 2.4))))))
  "One byte to its linear value: a 256-entry table beats a per-pixel EXPT.")

(defun linear-to-srgb-byte (value)
  (let ((clamped (min 1.0 (max 0.0 value))))
    (round (* 255.0
              (if (<= clamped 0.0031308)
                  (* 12.92 clamped)
                  (- (* 1.055 (expt clamped (/ 1.0 2.4))) 0.055))))))

(defun downsample-pixels (pixels width height factor &key (srgb-p t))
  "Average FACTOR by FACTOR blocks of PIXELS, in linear light.

Supersampling is the whole of the antialiasing here: there is no multisample
path, and averaging a rendered frame is the same thing one box filter later.
The average must be taken in linear light -- averaging sRGB bytes darkens
every edge, which is precisely where the eye is looking."
  (let* ((out-width (floor width factor))
         (out-height (floor height factor))
         (out (make-array (* 4 out-width out-height)
                          :element-type '(unsigned-byte 8)))
         (weight (/ 1.0 (* factor factor))))
    (dotimes (y out-height (values out out-width out-height))
      (dotimes (x out-width)
        (let ((red 0.0) (green 0.0) (blue 0.0) (alpha 0.0))
          (dotimes (dy factor)
            (dotimes (dx factor)
              (let ((offset (* 4 (+ (* (+ (* y factor) dy) width)
                                    (+ (* x factor) dx)))))
                (flet ((channel (index)
                         (let ((byte (aref pixels (+ offset index))))
                           (if srgb-p
                               (aref *srgb-to-linear* byte)
                               (/ (float byte 1.0) 255.0)))))
                  (incf red (channel 0))
                  (incf green (channel 1))
                  (incf blue (channel 2))
                  (incf alpha (/ (float (aref pixels (+ offset 3)) 1.0)
                                 255.0))))))
          (let ((offset (* 4 (+ (* y out-width) x))))
            (flet ((store (index value)
                     (setf (aref out (+ offset index))
                           (if srgb-p
                               (linear-to-srgb-byte (* value weight))
                               (round (* 255.0 (min 1.0 (* value weight))))))))
              (store 0 red)
              (store 1 green)
              (store 2 blue)
              (setf (aref out (+ offset 3))
                    (round (* 255.0 (min 1.0 (* alpha weight))))))))))))

(defun render-to-png (renderer pathname &key (downsample 1))
  "Render one frame headlessly and write it to PATHNAME as a PNG.

With DOWNSAMPLE above one the renderer is presumed to have been made that
many times oversize, and the frame is box-filtered down on the way out."
  (multiple-value-bind (pixels width height format) (render-pixels renderer)
    (ensure-directories-exist pathname)
    (if (> downsample 1)
        (multiple-value-bind (small small-width small-height)
            (downsample-pixels pixels width height downsample
                               :srgb-p (eq format :rgba8-unorm-srgb))
          (write-rgba-png pathname small small-width small-height format))
        (write-rgba-png pathname pixels width height format))))

(defun capture-demo-png (pathname &key (width 1280) (height 800)
                                    (camera (make-fly-camera)))
  "Render the demonstration scene once to PATHNAME and release everything."
  (let ((renderer (make-renderer :scene (make-demo-scene) :camera camera
                                 :width width :height height)))
    (unwind-protect
         (render-to-png renderer pathname)
      (destroy-renderer renderer))))

;;; ------------------------------------------------------------------------
;;; Viewer: a window with a fly camera

(defvar *viewer* nil "The most recently started viewer.")

(defclass viewer (canvas-event-handler)
  ((canvas :initarg :canvas :reader viewer-canvas)
   (context :initarg :context :reader viewer-context)
   (renderer :initarg :renderer :accessor viewer-renderer)
   (pressed-keys :initform (make-hash-table :test #'eq)
                 :reader viewer-pressed-keys)
   (pointer-captured-p :initform nil :accessor viewer-pointer-captured-p)
   (running-p :initform t :accessor viewer-running-p)
   (tracy-thread-named-p :initform nil :accessor viewer-tracy-thread-named-p)
   (last-timestamp :initform nil :accessor viewer-last-timestamp)
   (speed :initarg :speed :initform 12.0 :accessor viewer-speed)
   (sensitivity :initarg :sensitivity :initform 0.0032
                :accessor viewer-sensitivity)))

(defun viewer-key-down-p (viewer &rest names)
  (some (lambda (name) (gethash name (viewer-pressed-keys viewer))) names))

(defun advance-viewer-camera (viewer timestamp)
  (let* ((last (viewer-last-timestamp viewer))
         (dt (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0))
         (camera (renderer-camera (viewer-renderer viewer)))
         (step (* dt (viewer-speed viewer)
                  (if (viewer-key-down-p viewer :left-shift :right-shift)
                      3.0 1.0))))
    (setf (viewer-last-timestamp viewer) timestamp)
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore up))
      (flet ((move (direction amount)
               (setf (camera-position camera)
                     (let ((position (camera-position camera)))
                       (vec3:make-vec3
                        (+ (vec3:vec3-x position)
                           (* amount (vec3:vec3-x direction)))
                        (+ (vec3:vec3-y position)
                           (* amount (vec3:vec3-y direction)))
                        (+ (vec3:vec3-z position)
                           (* amount (vec3:vec3-z direction))))))))
        (when (viewer-key-down-p viewer :w :up) (move forward step))
        (when (viewer-key-down-p viewer :s :down) (move forward (- step)))
        (when (viewer-key-down-p viewer :d :right) (move right step))
        (when (viewer-key-down-p viewer :a :left) (move right (- step)))
        (when (viewer-key-down-p viewer :space :e)
          (move (vec3:make-vec3 0 0 1) step))
        (when (viewer-key-down-p viewer :left-control :q :c)
          (move (vec3:make-vec3 0 0 1) (- step)))))))

(zdefun (render-viewer-frame :zone :luft/viewer-frame) (viewer timestamp)
  (declare (ignore timestamp))
  (unless (viewer-running-p viewer)
    (return-from render-viewer-frame nil))
  (when (and *tracy* (not (viewer-tracy-thread-named-p viewer)))
    (name-tracy-thread "luft canvas")
    (setf (viewer-tracy-thread-named-p viewer) t))
  (prog1
      (present-canvas-frame
       (viewer-context viewer)
       (lambda (surface-texture encoder presentation-time)
         (ensure-renderer-extent
          (viewer-renderer viewer)
          (canvas-extent (viewer-context viewer)))
         (advance-viewer-camera viewer presentation-time)
         (let ((color (encode-frame (viewer-renderer viewer) encoder)))
           (encode encoder
                   (make-gpu-copy-texture-command
                    :source color :destination surface-texture)))))
    ;; Keep LUFT's frame boundary distinct from other canvases sharing this
    ;; durable image.  A wedged frame intentionally remains open in Tracy.
    (tracy-frame-mark "luft")))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (if (eq key :escape)
        (when (viewer-pointer-captured-p viewer)
          (set-canvas-relative-pointer-mode canvas nil)
          (setf (viewer-pointer-captured-p viewer) nil))
        (setf (gethash key (viewer-pressed-keys viewer)) t)))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (remhash (canvas-key-event-key-name event) (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-button-press-event))
  (when (and (not (viewer-pointer-captured-p viewer))
             (eq :left (canvas-pointer-event-button event)))
    (set-canvas-relative-pointer-mode canvas t)
    (setf (viewer-pointer-captured-p viewer) t))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (when (viewer-pointer-captured-p viewer)
    (let ((camera (renderer-camera (viewer-renderer viewer)))
          (sensitivity (viewer-sensitivity viewer)))
      (decf (camera-yaw camera)
            (* (canvas-pointer-event-delta-x event) sensitivity))
      (setf (camera-pitch camera)
            (max -1.5 (min 1.5
                           (- (camera-pitch camera)
                              (* (canvas-pointer-event-delta-y event)
                                 sensitivity)))))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-focus-lost-event))
  (declare (ignore canvas))
  (clrhash (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  ;; Stop drawing; STOP-VIEWER releases the window from outside the event.
  (setf (viewer-running-p viewer) nil)
  nil)

(defmethod handle-canvas-event ((viewer viewer) canvas event)
  (declare (ignore viewer canvas event))
  nil)

(defun standalone-render-technique
    (&optional (name (uiop:getenv "LUFT_RENDER_TECHNIQUE")))
  "Return the technique standalone NAME asks for: vertex unless told mesh."
  (let ((technique (string-downcase (or name "vertex"))))
    (cond ((string= technique "vertex") :vertex)
          ((string= technique "mesh") :mesh)
          (t (error "Unknown LUFT_RENDER_TECHNIQUE ~S; use vertex or mesh."
                    name)))))

(defun standalone-render-options
    (&optional (name (uiop:getenv "LUFT_RENDER_MODE"))
               (technique (standalone-render-technique)))
  "Return MODE, STYLE, PIPELINE-STYLES, EFFECTS, and TECHNIQUE for standalone NAME."
  (let ((mode (string-downcase (or name "full")))
        (styles (technique-styles technique)))
    (cond ((string= mode "clear")
           (values :clear :flat nil nil technique))
          ((string= mode "sky")
           (values :sky :flat nil '(:sky) technique))
          ((member mode '("flat" "bevel" "chamfer" "paper" "stock" "field"
                          "soft" "ink")
                   :test #'string=)
           (let ((style (intern (string-upcase mode) :keyword)))
             (unless (member style styles)
               (error "The ~(~A~) technique does not draw ~A; it draws ~
~{~(~A~)~^, ~}." technique mode styles))
             (values style style (list style) nil technique)))
          ((string= mode "full")
           ;; The stock style is what the world is meant to be seen in: the
           ;; crisp chamfered geometry, the field's soft light, and every
           ;; cell drawn in whatever it is cut from.
           (values :full (if (member :stock styles) :stock :chamfer)
                   styles '(:sky :lens) technique))
          (t
           (error "Unknown LUFT_RENDER_MODE ~S; use clear, sky, flat, bevel, ~
chamfer, paper, stock, field, soft, ink, or full." name)))))

(zdefun (start-viewer :zone :luft/start-viewer)
    (&key (scene (make-demo-scene))
          (camera (make-fly-camera))
          (title "luft atelier")
          (width 1280) (height 800)
          (frames-per-second 60)
          (technique *default-technique*)
          (style :flat)
          (pipeline-styles nil pipeline-styles-p)
          (effects nil)
          (provider *gpu-provider*))
  "Open a window flying through SCENE and return the running VIEWER.

Click to capture the pointer, Escape to release it; WASD, Space, and C move.
The renderer stays available as (VIEWER-RENDERER *VIEWER*) for live tinkering.
By default the viewer creates only the flat surface pipeline: pass explicit
PIPELINE-STYLES and EFFECTS to add the complex geometry, sky, or lens.
TECHNIQUE chooses vertex pulling or mesh shaders, as for MAKE-RENDERER."
  (let ((canvas (make-sdl-canvas
                 :title title :width width :height height :visible-p nil
                 :presentation-api (sdl-presentation-api-for provider)))
        (device nil)
        (renderer nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (let* ((device* (setf device
                               (request-gpu-device
                                provider
                                (make-device-descriptor :label title))))
                (context (make-canvas-context
                          canvas provider
                          (make-canvas-configuration :device device*)))
                (extent (canvas-extent context))
                (renderer* (setf renderer
                                 (make-renderer
                                  :scene scene :camera camera :device device*
                                  :width (first extent) :height (second extent)
                                  :color-format (canvas-format context)
                                  :technique technique
                                  :style style
                                  :pipeline-styles
                                  (if pipeline-styles-p
                                      pipeline-styles
                                      (list style))
                                  :effects effects)))
                (viewer (make-instance 'viewer :canvas canvas :context context
                                               :renderer renderer*)))
           (setf (canvas-event-handler canvas) viewer)
           (request-canvas-frame
            canvas (lambda (timestamp) (render-viewer-frame viewer timestamp)))
           (show-canvas canvas)
           (setf (canvas-clock canvas)
                 (make-cadence-clock
                  (lambda (native-canvas timestamp)
                    (declare (ignore native-canvas))
                    (render-viewer-frame viewer timestamp))
                  :frames-per-second frames-per-second))
           (setf completed-p t
                 *viewer* viewer)
           viewer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (close-canvas canvas)
        (when device (destroy device))))))

(defun stop-viewer (&optional (viewer *viewer*))
  "Close VIEWER's window and release its renderer and device."
  (when viewer
    (setf (viewer-running-p viewer) nil)
    (let* ((canvas (viewer-canvas viewer))
           (renderer (viewer-renderer viewer))
           (device (and renderer (renderer-device renderer))))
      (when (eq :open (canvas-state canvas))
        (setf (canvas-clock canvas) (make-demand-clock)))
      (when renderer
        (destroy-renderer renderer)
        (setf (viewer-renderer viewer) nil))
      (when (eq :open (canvas-state canvas))
        (close-canvas canvas))
      (when device
        (ignore-errors (destroy device))))
    (when (eq viewer *viewer*)
      (setf *viewer* nil)))
  (values))
