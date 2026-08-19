;;; Bending the lattice, and chamfers that differ within one world.
;;;
;;; Two experiments that turn out to be the same experiment.
;;;
;;; The first: nothing anywhere says the lattice must be square.  Every
;;; shaped point is placed in the lattice by a rule over its star, and only
;;; then turned into a clip position; put a smooth map W between those two
;;; steps and the whole world bends, without a single rule changing.  The
;;; surface stays watertight for the same reason it was watertight before:
;;; the faces incident to a site all compute the same rest point, and W is a
;;; function of position alone, so they all land on the same bent point.  A
;;; deformation cannot open a crack that the lattice did not already have.
;;;
;;; The normal follows from W by construction rather than by a Jacobian
;;; worked out by hand.  Push the point and two neighbours a hair away
;;; along its tangent plane through W; the cross product of the two
;;; differences is the bent normal.  Three evaluations of W, and a new
;;; deformation is one function.
;;;
;;; The second: the chamfer width was a single number for the whole world,
;;; and it need not be.  But it cannot be a property of a /face/: two faces
;;; meet along a crease, and if they disagree about how far to move the
;;; point they share, the surface tears.  The width has to belong to the
;;; site, which means it has to be a function of the star -- the same four
;;; or eight cells every incident face already reads.  Two such functions
;;; are interesting: the star's own shape (a convex arris planed wide, a
;;; concave one left nearly sharp), and the stocks of its solid cells (a
;;; timber post planed, the stone it stands on pecked).  Both are symmetric
;;; over the star, so both are coherent, and the watertightness test of
;;; #RUAWR5 is what says so out loud.  #V0YCZA #NW1KEM

(in-package #:luft.render.shaders)

;;; ------------------------------------------------------------------------
;;; The maps

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *deformations*
    '(:none :lean :taper :bend :twist :swirl :billow :globe)
    "The deformations the lattice shader can apply, in frame-lane order.

A deformation's index is its position here; the host writes that index into
the frame block, so all of them are compiled into one shader and chosen by
a uniform rather than by a pipeline apiece.  That is what lets a contact
sheet put eight of them side by side."))

(defun deformation-index (name)
  "The lane index of the deformation called NAME."
  (or (position name *deformations*)
      (error "No deformation ~S; there is ~{~S~^, ~}." name *deformations*)))

(define-shader-function deform-point (rest kind strength scale centre)
  "Where the rest position REST goes under deformation KIND.

Every branch is evaluated and one selected, which costs a few dozen ALU
operations a vertex and buys the ability to change the deformation without
changing the pipeline.  Each map fixes CENTRE: at zero strength, and at the
centre itself, every one of them is the identity, so nothing jumps when the
knob leaves zero."
  (let* ((d (- rest centre))
         (dx (swizzle d :x))
         (dy (swizzle d :y))
         (dz (swizzle d :z))
         (cx (swizzle centre :x))
         (cy (swizzle centre :y))
         (cz (swizzle centre :z))
         (span (max scale 0.001))
         ;; Lean: a shear, everything leaning east as it rises.
         (lean (+ rest (vec3 (* strength dz) 0.0 0.0)))
         ;; Taper: the plan shrinks toward the centre as it rises, which is
         ;; the batter of a wall taken to the whole world.
         (pull (- 1.0 (* strength dz)))
         (taper (vec3 (+ cx (* dx pull)) (+ cy (* dy pull)) (swizzle rest :z)))
         ;; Bend: the X axis wraps onto a circle of radius 1/STRENGTH in
         ;; the XZ plane, so a straight arcade becomes a curved one.
         (bend-radius (/ 1.0 (max (abs strength) 0.00001)))
         (bend-angle (* dx strength))
         (bend-reach (+ bend-radius dz))
         (bend (vec3 (+ cx (* bend-reach (sin bend-angle)))
                     (swizzle rest :y)
                     (+ cz (- (* bend-reach (cos bend-angle)) bend-radius))))
         ;; Twist: rotation about the vertical axis, by height.
         (twist-angle (* strength dz))
         (twist (vec3 (+ cx (- (* dx (cos twist-angle))
                               (* dy (sin twist-angle))))
                      (+ cy (+ (* dx (sin twist-angle))
                               (* dy (cos twist-angle))))
                      (swizzle rest :z)))
         ;; Swirl: the same rotation, but falling off with distance in plan
         ;; instead of rising with height.  A vortex in the ground.
         (plan (+ (* dx dx) (* dy dy)))
         (swirl-angle (* strength (exp (- (/ plan (* span span))))))
         (swirl (vec3 (+ cx (- (* dx (cos swirl-angle))
                               (* dy (sin swirl-angle))))
                      (+ cy (+ (* dx (sin swirl-angle))
                               (* dy (cos swirl-angle))))
                      (swizzle rest :z)))
         ;; Billow: a standing wave lifted out of the ground.
         (billow (+ rest
                    (vec3 0.0 0.0
                          (* strength (* (sin (/ dx span))
                                         (cos (/ dy span)))))))
         ;; Globe: the whole horizontal plane wrapped onto a sphere of
         ;; radius 1/STRENGTH, height becoming altitude above it.  A world
         ;; sixty cells across becomes a small planet.
         (globe-radius (/ 1.0 (max (abs strength) 0.00001)))
         (lon (* dx strength))
         (lat (* dy strength))
         (globe-reach (+ globe-radius dz))
         (globe (vec3 (+ cx (* globe-reach (* (cos lat) (sin lon))))
                      (+ cy (* globe-reach (sin lat)))
                      (+ cz (- (* globe-reach (* (cos lat) (cos lon)))
                               globe-radius)))))
    (if (< kind 0.5) rest
        (if (< kind 1.5) lean
            (if (< kind 2.5) taper
                (if (< kind 3.5) bend
                    (if (< kind 4.5) twist
                        (if (< kind 5.5) swirl
                            (if (< kind 6.5) billow globe)))))))))

(define-shader-function deform-normal
    (rest normal kind strength scale centre)
  "The bent normal at REST, from three evaluations of the map.

A frame is built in the rest surface's own tangent plane, its two vectors
pushed through the deformation a hundredth of a cell away, and the cross
product of the differences taken.  This is the inverse transpose of the
Jacobian without anyone having had to write the Jacobian down, and it is
right for whatever map DEFORM-POINT grows next."
  (let* ((away (if (< (abs (swizzle normal :z)) 0.9)
                   (vec3 0.0 0.0 1.0)
                   (vec3 1.0 0.0 0.0)))
         (tangent (normalize (cross-product normal away)))
         (bitangent (cross-product normal tangent))
         (step 0.01)
         (origin (deform-point rest kind strength scale centre))
         (along (deform-point (+ rest (* tangent step))
                              kind strength scale centre))
         (across (deform-point (+ rest (* bitangent step))
                               kind strength scale centre))
         (bent (cross-product (- along origin) (- across origin)))
         (length (sqrt (dot bent bent))))
    (if (> length 0.000001)
        (/ bent length)
        normal)))

;;; ------------------------------------------------------------------------
;;; A width that belongs to the site
;;;
;;; Whether a crease is convex or concave is a property of its star, and it
;;; has to be read off the star as such.  The obvious shortcut -- the sign
;;; of the minority's dot with the face's outward normal -- is right at an
;;; edge and wrong at a mixed corner, where the same eight cells give one
;;; face a positive dot and another a negative one, the two faces then
;;; disagree about how far to move the point they share, and the surface
;;; tears along the seam.  It tears visibly: that is what the watertightness
;;; test is for, and it caught this.
;;;
;;; So the classification is counted instead.  An edge's four cells are a
;;; crease exactly when the two across it agree, and then their common
;;; solidity says which kind; a vertex's eight are convex when three or
;;; fewer are solid and concave when five or more.  Both are symmetric
;;; functions of the star, so every incident face computes the same number.

(define-shader-function edge-relief (solid-c solid-e)
  "Minus one for a convex arris, plus one for a concave cove, zero else.

SOLID-C and SOLID-E are the two cells across the edge, as EDGE-MINORITY
takes them.  They agree exactly when the star is a crease, and what they
agree on is whether the crease has three solid cells or one."
  (if (< (abs (- solid-c solid-e)) 0.5)
      (if (> solid-c 0.5) 1.0 -1.0)
      0.0))

(define-shader-function vertex-relief
    (solid-cu solid-eu solid-cv solid-ev solid-cuv solid-euv)
  "The same for a vertex star, from how many of its eight cells are solid.

The face's own solid cell is one and its air cell is none; the other six
are the arguments.  Three or fewer solid is a corner sticking out, five or
more a corner cut in, and four is a saddle or a plane, which no rule moves."
  (let* ((count (+ 1.0 solid-cu solid-eu solid-cv solid-ev solid-cuv
                   solid-euv)))
    (if (< count 3.5) -1.0 (if (> count 4.5) 1.0 0.0))))

(define-shader-function site-relief-width (relief radius convex concave)
  "RADIUS scaled by CONVEX on an outward arris and CONCAVE on an inward one."
  (if (< relief -0.5)
      (* radius convex)
      (if (> relief 0.5) (* radius concave) radius)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun cell-stock-bindings (name position-form)
    "LET* bindings reading the stock slot of the cell at POSITION-FORM.

The slots buffer holds one nibble a cell in the same dense order as the
occupancy bits, so eight cells share a word."
    (let ((x0 (intern (format nil "~A-SX0" name)))
          (y0 (intern (format nil "~A-SY0" name)))
          (x (intern (format nil "~A-SX" name)))
          (y (intern (format nil "~A-SY" name)))
          (z (intern (format nil "~A-SZ" name)))
          (index (intern (format nil "~A-SINDEX" name)))
          (word (intern (format nil "~A-SWORD" name)))
          (slot (intern (format nil "~A-SLOT" name))))
      `((,x0 (swizzle ,position-form :x))
        (,y0 (swizzle ,position-form :y))
        (,x (if (< ,x0 0.0) (+ ,x0 period-x)
                (if (< ,x0 period-x) ,x0 (- ,x0 period-x))))
        (,y (if (< ,y0 0.0) (+ ,y0 period-y)
                (if (< ,y0 period-y) ,y0 (- ,y0 period-y))))
        (,z (max 0.0 (min (swizzle ,position-form :z) 255.0)))
        (,index (uint (+ ,x (* period-x (+ ,y (* period-y ,z))))))
        (,word (buffer-element slots (/ ,index (uint 8.0))))
        (,slot (float (ldb (byte 4 (* (uint 4.0) (mod ,index (uint 8.0))))
                           ,word))))))

  (defun stock-chamfer-form (slot-form)
    "A form reading the chamfer multiplier of the stock in SLOT-FORM.

It rides in the fourth component of the third lane of the stock table, one
of the three the albedos left spare."
    `(swizzle (buffer-element
               stocks (+ (* (uint (+ ,slot-form 0.25))
                            (uint ,(float +stock-lanes+)))
                         (uint 2.0)))
              :w))

  (defun cell-chamfer-bindings (name position-form solid-form)
    "Bindings giving NAME-CHAMFER: the chamfer multiplier of the cell at
POSITION-FORM when SOLID-FORM says it is solid, and a large number when it
is air, so that a minimum over a star ignores the air in it."
    (let ((chamfer (intern (format nil "~A-CHAMFER" name))))
      (append (cell-stock-bindings name position-form)
              `((,chamfer
                 (if (> ,solid-form 0.5)
                     ,(stock-chamfer-form (intern (format nil "~A-SLOT" name)))
                     99.0)))))))

(define-shader-function site-stock-width (radius own a b c d e f)
  "RADIUS scaled by the least chamfer any solid cell of the star asks for.

A minimum over the star is symmetric in it, so every face incident to the
site computes the same width from the same cells, whichever of them it
happens to be.  A crease between a planed timber and a pecked stone comes
out as sharp as the stone: the sharper stock wins, which is what a mason
would do with a stone that will not take an arris."
  (* radius (min (min (min own a) (min b c)) (min (min d e) f))))

;;; ------------------------------------------------------------------------
;;; The stock stage
;;;
;;; One vertex shader carries all of it: the chamfer rule of #RUAWR5 with a
;;; width per site, the map, and the rest position the fragment stage needs
;;; in order to keep reading the lattice while the world is bent.

#.(grid-vertex-shader-definition 'stock-vertex-shader 1 :chamfer
                                 :widths :site :deform t)

;;; ------------------------------------------------------------------------
;;; The knobs

(in-package #:luft.render)

(defparameter *deformation* :none
  "The map the lattice is drawn through.

One of SHADERS:*DEFORMATIONS*.  Every one of them fixes *DEFORM-CENTRE* and
is the identity at zero strength, so a sheet may sweep the strength from
nothing and watch a world bend.")

(defparameter *deform-strength* 0.0
  "How hard the deformation pulls, in whatever unit its map takes.

:LEAN and :TAPER read it as a fraction per cell of height, :BEND, :TWIST and
:GLOBE as radians per cell, :SWIRL as radians at its centre, and :BILLOW as
an amplitude in cells.")

(defparameter *deform-scale* 12.0
  "The deformation's second length: a falloff for :SWIRL, a wavelength for
:BILLOW, and nothing at all for the rest.")

(defparameter *deform-centre* nil
  "The point every deformation fixes, as a list of three floats, or NIL for
the middle of the world's floor.")

(defparameter *chamfer-rule* :uniform
  "How wide each site is chamfered: :UNIFORM, :RELIEF, or :STOCK.

:UNIFORM gives the whole world *CHAMFER-WIDTH*.  :RELIEF asks the star what
kind of crease it is and scales by *RELIEF-CONVEX* or *RELIEF-CONCAVE*.
:STOCK takes the least chamfer any solid cell of the star asks for, so a
crease between a planed timber and a pecked stone comes out as sharp as the
stone.  All three are functions of the star alone, which is what keeps the
two faces along a crease from disagreeing.")

(defparameter *relief-convex* 1.0
  "What the :RELIEF rule multiplies an outward arris's width by.

*CHAMFER-WIDTH* is the band the grid reserves and no rule may exceed it, so
one is the whole of it and the useful range is at most one.")

(defparameter *relief-concave* 0.25
  "What the :RELIEF rule multiplies an inward cove's width by.

Weather and hands reach an outside corner and not an inside one, so the
one is planed and the other left nearly sharp.")

(defun deform-lane ()
  "The frame block's deformation lane: index, strength, scale, spare."
  (list (coerce (shaders:deformation-index *deformation*) 'single-float)
        (coerce *deform-strength* 'single-float)
        (coerce *deform-scale* 'single-float)
        0.0))

(defun deform-centre-lane (domain)
  "The point every deformation fixes: the middle of DOMAIN's floor unless
*DEFORM-CENTRE* names another."
  (if *deform-centre*
      (append (mapcar (lambda (x) (coerce x 'single-float)) *deform-centre*)
              (list 0.0))
      (list (if domain
                (* 0.5 (coerce (luft:world-domain-x-period domain)
                               'single-float))
                0.0)
            (if domain
                (* 0.5 (coerce (luft:world-domain-y-period domain)
                               'single-float))
                0.0)
            0.0 0.0)))

(defun arris-lane ()
  "The frame block's chamfer-rule lane: the rule, and the relief factors."
  (list (ecase *chamfer-rule* (:uniform 0.0) (:relief 1.0) (:stock 2.0))
        (coerce *relief-convex* 'single-float)
        (coerce *relief-concave* 'single-float)
        0.0))
