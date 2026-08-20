;;; Shared shader vocabulary for a solid world drawn from packed sites.
;;;
;;; Ordinary vertex stages pull one signed u64 site per face and derive every
;;; point directly.  The host also binds dense occupancy and stock tables plus
;;; one frame block shared by geometry, shading, motion, and temporal resolve.
;;; There is deliberately one geometry path now.  #AGVXGM #VAABY9

(in-package #:luft.render.shaders)

(defconstant +frame-binding+ 0)
(defconstant +sites-binding+ 1)
(defconstant +cells-binding+ 2)
(defconstant +stocks-binding+ 3)
(defconstant +slots-binding+ 4)

;;; A LUFT site occupies sixty bits and travels to the GPU in sixty-four, so
;;; four bits are free above it.  The packed site spends them on which stock
;;; the solid behind the face is cut from: sixteen materials in a world, at
;;; no cost in bandwidth and none in a second lookup.  #KE4P5F
(defconstant +site-stock-shift+ 60)
(defconstant +site-stock-bits+ 4)
(defconstant +stock-slots+ (ash 1 +site-stock-bits+))
(defconstant +stock-lanes+ 9
  "Vec4 lanes a stock occupies in the table.

Three albedos, five material lanes, and one for the lattice: what a stock
does to the shape of the world rather than to its surface.")

;;; The focus pass reads a drawn frame rather than the world, and so binds a
;;; group of its own.
(defconstant +scene-binding+ 0)
(defconstant +sampler-binding+ 1)
(defconstant +lens-frame-binding+ 2)

;;; Temporal resolve owns a separate group.  Its history texture changes each
;;; frame while the current colour and motion attachments stay put.
(defconstant +current-binding+ 0)
(defconstant +motion-binding+ 1)
(defconstant +history-binding+ 2)
(defconstant +temporal-sampler-binding+ 3)
(defconstant +temporal-frame-binding+ 4)

;;; Every stage declares the same frame block at the same binding: identical
;;; member order and offsets are the ABI, written once and spliced at read
;;; time.  The members are deliberately unannotated raw lanes.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *occlusion-steps* 6
    "Cell faces an occlusion walk crosses: how far away the world still
crowds a point, and four times this many cell reads per pixel.")

  (defvar *shadow-steps* 28
    "Cell boundaries a sun ray crosses before it gives up and calls itself
lit: both the reach of shadows in cells and their cost per pixel.")

  (defparameter *frame-uniform-members*
    '((camera-vector :vec4)      ; camera position, w unused
      (right-vector :vec4)       ; camera basis
      (up-vector :vec4)
      (forward-vector :vec4)
      (projection-vector :vec4)  ; x scale, y scale, z scale, z offset
      (sun-vector :vec4)         ; direction toward the sun, ambient light
      (sky-vector :vec4)         ; sky colour, fog distance
      (domain-vector :vec4)      ; x/y period, fillet radius/chamfer width, arris
      (sun-colour-vector :vec4)  ; sun radiance, sheen strength
      (fill-vector :vec4)        ; direction toward the fill light, strength
      (ground-vector :vec4)      ; ground bounce colour, exposure
      (occlusion-vector :vec4)   ; crowding, shadow, wear, ink width
      (top-vector :vec4)         ; the material of an upward face
      (side-vector :vec4)        ; the material of a sideways face
      (bottom-vector :vec4)      ; the material of a downward face
      (lens-vector :vec4)        ; focus distance, aperture, texel width/height
      ;; The lattice: how it is bent, and how wide its creases are planed.
      (deform-vector :vec4)      ; deformation index, strength, scale, spare
      (deform-centre-vector :vec4) ; the point every deformation fixes
      (arris-vector :vec4)       ; chamfer rule, convex and concave factors
      ;; A frozen predecessor and the sampling state of this frame.  Motion
      ;; is current-to-previous in UV units and excludes jitter; the resolve
      ;; adds the jitter delta when it addresses history.
      (previous-camera-vector :vec4)
      (previous-right-vector :vec4)
      (previous-up-vector :vec4)
      (previous-forward-vector :vec4)
      (previous-projection-vector :vec4)
      (jitter-vector :vec4)      ; current clip XY, previous clip XY
      (temporal-vector :vec4)))) ; inverse extent, history valid, history weight

(define-shader-function view-clip (point camera right up forward projection)
  "Homogeneous clip position of the world POINT.

Clip Y is framebuffer-oriented like every luv camera graph; the Metal
lowering owns the target's flip."
  (let* ((relative (- point camera))
         (view-x (dot relative right))
         (view-y (dot relative up))
         (view-z (dot relative forward)))
    (vec4 (* view-x (swizzle projection :x))
          (- (* view-y (swizzle projection :y)))
          (+ (* view-z (swizzle projection :z)) (swizzle projection :w))
          view-z)))

(define-shader-function jitter-clip (clip jitter)
  "Move homogeneous CLIP by a two-component clip-space JITTER."
  (vec4 (+ (swizzle clip :x) (* (swizzle jitter :x) (swizzle clip :w)))
        (+ (swizzle clip :y) (* (swizzle jitter :y) (swizzle clip :w)))
        (swizzle clip :z)
        (swizzle clip :w)))

(define-shader-function clip-uv (clip)
  "Map a homogeneous framebuffer-oriented clip position to texture UV."
  (+ (* (/ (swizzle clip :xy) (swizzle clip :w)) 0.5)
     (vec2 0.5 0.5)))

(define-shader-function point-motion
    (current-point previous-point
     current-camera current-right current-up current-forward current-projection
     previous-camera previous-right previous-up previous-forward
     previous-projection)
  "Unjittered current-to-previous UV displacement between two world points."
  (let* ((current
           (view-clip current-point current-camera current-right current-up
                      current-forward current-projection))
         (previous
           (view-clip previous-point previous-camera previous-right previous-up
                      previous-forward previous-projection)))
    (- (clip-uv previous) (clip-uv current))))

(define-shader-abstraction world-motion (point)
  "Motion of a static world POINT under the frame block's two views."
  `(point-motion
    ,point ,point
    (swizzle camera-vector :xyz)
    (swizzle right-vector :xyz) (swizzle up-vector :xyz)
    (swizzle forward-vector :xyz) projection-vector
    (swizzle previous-camera-vector :xyz)
    (swizzle previous-right-vector :xyz) (swizzle previous-up-vector :xyz)
    (swizzle previous-forward-vector :xyz) previous-projection-vector))

(define-shader-abstraction direction-motion (direction)
  "Motion of a world-space sky DIRECTION under the frame block's two views."
  `(point-motion
    (+ (swizzle camera-vector :xyz) ,direction)
    (+ (swizzle previous-camera-vector :xyz) ,direction)
    (swizzle camera-vector :xyz)
    (swizzle right-vector :xyz) (swizzle up-vector :xyz)
    (swizzle forward-vector :xyz) projection-vector
    (swizzle previous-camera-vector :xyz)
    (swizzle previous-right-vector :xyz) (swizzle previous-up-vector :xyz)
    (swizzle previous-forward-vector :xyz) previous-projection-vector))

(define-shader-function cross-product (a b)
  "The right-handed cross product of two vec3 values."
  (vec3 (- (* (swizzle a :y) (swizzle b :z)) (* (swizzle a :z) (swizzle b :y)))
        (- (* (swizzle a :z) (swizzle b :x)) (* (swizzle a :x) (swizzle b :z)))
        (- (* (swizzle a :x) (swizzle b :y)) (* (swizzle a :y) (swizzle b :x)))))

;;; ------------------------------------------------------------------------
;;; Shadow: a ray walked cell by cell through the occupancy bits
;;;
;;; The cells the solid chain fills are already on the GPU, one bit each, so
;;; the truest cheap shadow is to walk the grid from the shaded point toward
;;; the sun and ask whether any cell on the way is solid.  The walk is an
;;; Amanatides-Woo traversal folded over a counted range: the state is the
;;; current cell and whether anything has been hit, and the parameter at which
;;; the ray leaves that cell along each axis is recomputed from that cell's
;;; own boundary rather than carried, which keeps the state one vec4.  The
;;; nearest boundary picks the axis to step, by masks rather than branches.
;;; Each iteration crosses exactly one cell face, so the count is the reach in
;;; cells; past it the ray is called lit and the shadow simply ends.  The
;;; horizontal axes wrap with the domain, as the world does, and everything
;;; below or above the cell array is air.
;;;
;;; It is an abstraction rather than a shader function because it reads a
;;; storage buffer, which only a name in the shader's own resources can do.

(define-shader-abstraction marched-cell-walk
    (origin direction cells period-x period-y steps)
  "Walk STEPS cell faces from ORIGIN along DIRECTION through the CELLS bits.

The value is the vec4 of the cell the walk ended in and, in W, how near the
first solid cell was: zero when the walk met none, and otherwise STEPS less
the distance from ORIGIN to that cell's centre.  Nearness rather than a flag
lets one walk answer both questions asked of it, whether the sun is hidden and
how closely the world crowds this point, and it is read only once, which
matters because the walk is spliced into its caller's expression.  Measuring
by distance rather than by iteration is what keeps occlusion smooth: the cell
is quantised but ORIGIN is not, so the shade slides across a face instead of
stepping from one square to the next."
  `(counted-fold (%walk-step ,(float steps) %walk-state
                  (vec4 (floor (swizzle ,origin :x))
                        (floor (swizzle ,origin :y))
                        (floor (swizzle ,origin :z))
                        0.0))
     (let* ((%walk-here (swizzle %walk-state :xyz))
            (%walk-near (swizzle %walk-state :w))
            ;; An axis the ray does not travel must never be the nearest
            ;; boundary, so give it a direction it can never cross in reach.
            (%walk-toward
              (+ ,direction
                 (* (- (vec3 1.0 1.0 1.0) (abs (signum ,direction)))
                    0.00001)))
            (%walk-sign (signum %walk-toward))
            ;; The boundary each axis leaves its cell by: the far side when
            ;; the ray climbs that axis, the near side when it falls.
            (%walk-ahead (max %walk-sign (vec3 0.0 0.0 0.0)))
            (%walk-time (/ (- (+ %walk-here %walk-ahead) ,origin)
                           %walk-toward))
            (%walk-tx (swizzle %walk-time :x))
            (%walk-ty (swizzle %walk-time :y))
            (%walk-tz (swizzle %walk-time :z))
            (%walk-ax (if (<= %walk-tx %walk-ty)
                          (if (<= %walk-tx %walk-tz) 1.0 0.0)
                          0.0))
            (%walk-ay (* (- 1.0 %walk-ax)
                         (if (<= %walk-ty %walk-tz) 1.0 0.0)))
            (%walk-az (- 1.0 (+ %walk-ax %walk-ay)))
            (%walk-next (+ %walk-here
                           (* %walk-sign (vec3 %walk-ax %walk-ay %walk-az))))
            (%walk-z (swizzle %walk-next :z))
            (%walk-inside (if (< %walk-z 0.0) 0.0
                              (if (> %walk-z 255.0) 0.0 1.0)))
            ;; The language's MOD is unsigned, so wrap the horizontal
            ;; axes the way a float must: x - period * floor(x / period).
            (%walk-x (swizzle %walk-next :x))
            (%walk-y (swizzle %walk-next :y))
            (%walk-wrapped-x
              (- %walk-x (* ,period-x (floor (/ %walk-x ,period-x)))))
            (%walk-wrapped-y
              (- %walk-y (* ,period-y (floor (/ %walk-y ,period-y)))))
            (%walk-index (uint (+ %walk-wrapped-x
                                  (* ,period-x
                                     (+ %walk-wrapped-y
                                        (* ,period-y
                                           (clamp %walk-z 0.0 255.0)))))))
            (%walk-word (buffer-element ,cells (/ %walk-index (uint 32.0))))
            (%walk-solid
              (* %walk-inside
                 (float (ldb (byte 1 (mod %walk-index (uint 32.0)))
                             %walk-word))))
            (%walk-reach (- (+ %walk-next (vec3 0.5 0.5 0.5)) ,origin))
            (%walk-distance (sqrt (dot %walk-reach %walk-reach))))
       (vec4 (swizzle %walk-next :x)
             (swizzle %walk-next :y)
             (swizzle %walk-next :z)
             ;; The first solid cell wins; later ones must not overwrite it.
             (max %walk-near
                  (* %walk-solid
                     (max 0.0 (- ,(float steps) %walk-distance))))))))

(define-shader-abstraction crowded-sky
    (origin normal cells period-x period-y steps)
  "How much of the sky the world hides from ORIGIN, zero to one.

Four walks leave the surface at forty-five degrees to NORMAL, one into each
quadrant, and each returns nearness: a cell right against the point counts
fully, one at the end of the reach hardly at all.  Their mean is the crowding,
which darkens the ambient hemisphere and no other light.  All of it is one
expression, since an abstraction cannot bind, so the tangents are written out
per ray and left to the backend's common subexpressions."
  (flet ((ray (u v)
           ;; A tangent pair from a vector no axis-aligned face normal can be
           ;; parallel to; the face normals here are always axis-aligned.
           (let* ((tangent `(normalize
                             (cross-product ,normal (vec3 0.577 0.577 0.577))))
                  (bitangent `(cross-product ,normal ,tangent)))
             `(/ (swizzle
                  (marched-cell-walk
                   ,origin
                   (normalize (+ ,normal
                                 (* (+ (* ,tangent ,u) (* ,bitangent ,v))
                                    0.85)))
                   ,cells ,period-x ,period-y ,steps)
                  :w)
                 ,(float steps)))))
    `(* 0.25 (+ (+ ,(ray 1.0 1.0) ,(ray 1.0 -1.0))
                (+ ,(ray -1.0 1.0) ,(ray -1.0 -1.0))))))

;;; ------------------------------------------------------------------------
;;; Light: one warm key, one cool fill, a hemisphere, and a little sheen
;;;
;;; Six axis-aligned face directions want six distinguishable tones, which a
;;; single sun and a constant ambient cannot give: the three faces turned away
;;; from the sun all land on the ambient floor.  So the atelier lights like a
;;; studio.  The sun is the key; a dimmer fill from another quarter separates
;;; the shaded faces from each other; the ambient is a hemisphere, sky above
;;; and bounce below, which separates up from down; and a low sheen picks out
;;; the chamfers, whose normals sweep through angles no flat face has.
;;; Occlusion darkens only the ambient, as occlusion does.  The sum is
;;; exposed through 1 - exp(-x), so bright faces roll off instead of clipping.

(define-shader-function surface-lighting
    (base normal world occlusion shade camera-vector sun-vector
     sun-colour-vector fill-vector sky-vector ground-vector)
  "The lit, exposed, fogged colour of BASE material under the frame's lights.

OCCLUSION scales the ambient hemisphere and SHADE the direct sun; both are
one where nothing is hidden."
  (let* ((sun (swizzle sun-vector :xyz))
         (ambient (swizzle sun-vector :w))
         (sky (swizzle sky-vector :xyz))
         (ground (swizzle ground-vector :xyz))
         (exposure (swizzle ground-vector :w))
         (radiance (swizzle sun-colour-vector :xyz))
         (sheen (swizzle sun-colour-vector :w))
         (camera (swizzle camera-vector :xyz))
         (delta (- world camera))
         (distance (sqrt (dot delta delta)))
         (view (/ delta (- distance)))
         (facing (max (dot normal sun) 0.0))
         (key (* facing (* shade (mix 1.0 occlusion 0.5))))
         (fill (* (swizzle fill-vector :w)
                  (max (dot normal (swizzle fill-vector :xyz)) 0.0)))
         (upness (swizzle normal :z))
         (sky-weight (* occlusion (+ 0.5 (* 0.5 upness))))
         (ground-weight (* occlusion (- 0.5 (* 0.5 upness))))
         (half (normalize (+ sun view)))
         (gloss (* sheen (* key (expt (max (dot normal half) 0.0) 48.0))))
         (light (+ (* radiance key)
                   (+ (* sky (+ (* ambient sky-weight) fill))
                      (* ground (* ambient ground-weight)))))
         (lit (+ (* base light) (* radiance gloss)))
         (exposed (- (vec3 1.0 1.0 1.0) (exp (* lit (- exposure)))))
         (fog-far (swizzle sky-vector :w))
         (fog (smoothstep (* 0.45 fog-far) fog-far distance)))
    (mix exposed sky fog)))

(defmacro define-temporal-fragment-shaders
    ((ordinary-name temporal-name motion-form) options &body body)
  "Define matching ordinary and motion-writing fragment shaders.

BODY must be one LET or LET* form.  The temporal shader gets a location-one
motion output and writes MOTION-FORM in that form's lexical scope; the
ordinary shader remains valid for a pipeline with only one colour target."
  (unless (and (= (length body) 1)
               (consp (first body))
               (member (first (first body)) '(let let*)))
    (error "A temporal fragment shader body must be one LET or LET* form."))
  (let* ((temporal-options (copy-tree options))
         (temporal-body (copy-tree body))
         (scope (first temporal-body)))
    (setf (getf temporal-options :outputs)
          (append (getf temporal-options :outputs)
                  '((motion :vec2 :location 1))))
    (setf (cdr (last scope))
          (list `(set-output motion ,motion-form)))
    `(progn
       (define-shader ,ordinary-name ,options ,@body)
       (define-shader ,temporal-name ,temporal-options ,@temporal-body))))

(define-temporal-fragment-shaders
    (surface-fragment-shader temporal-surface-fragment-shader
     (world-motion world))
    (:stage :fragment
     :inputs ((normal :vec3 :location 0)
              (world :vec3 :location 1)
              (uv :vec2 :location 2))
     :outputs ((color :vec4 :location 0))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)
                 (cells :storage-buffer :binding #.+cells-binding+
                        :element :uint)))
  (let* ((upness (swizzle normal :z))
         (base (if (> upness 0.5)
                   (swizzle top-vector :xyz)
                   (if (< upness -0.5)
                       (swizzle bottom-vector :xyz)
                       (swizzle side-vector :xyz))))
         ;; Textureless, but not featureless: a soft line where faces meet.
         (u (swizzle uv :x))
         (v (swizzle uv :y))
         (edge (min (min u (- 1.0 u)) (min v (- 1.0 v))))
         (line (mix 0.70 1.0 (smoothstep 0.0 0.06 edge)))
         ;; A rounded surface lies inside its own cell, by up to the
         ;; radius, so clear that much before trusting the grid.
         (walk-origin
           (+ world (* normal (+ (* 2.0 (swizzle domain-vector :z)) 0.1))))
         (walk (marched-cell-walk walk-origin (swizzle sun-vector :xyz)
                                  cells (swizzle domain-vector :x)
                                  (swizzle domain-vector :y)
                                  #.*shadow-steps*))
         (shade (mix 1.0 (if (> (swizzle walk :w) 0.5) 0.0 1.0)
                     (swizzle occlusion-vector :y)))
         (crowding (crowded-sky walk-origin normal cells
                                (swizzle domain-vector :x)
                                (swizzle domain-vector :y)
                                #.*occlusion-steps*))
         (open (* line (- 1.0 (* (swizzle occlusion-vector :x) crowding))))
         (final (surface-lighting base normal world open shade
                                  camera-vector sun-vector sun-colour-vector
                                  fill-vector sky-vector ground-vector)))
    (set-output color (vec4 final 1.0))))


;;; ------------------------------------------------------------------------
;;; Bevelled and filleted faces
;;;
;;; The rounding of a crease belongs to the crease's site, not to either face
;;; beside it.  An edge site has four cells around it, a vertex site eight; the
;;; solidity of that star classifies the crease, and one rule moves the shared
;;; point toward the star's minority (the lone solid cell of a convex crease,
;;; the lone air cell of a concave one) by r(sqrt(m) - 1) along the minority
;;; direction, m being its nonzero axes.  Every face incident to the site
;;; computes the same point from the same star, so the subdivided faces meet
;;; exactly.  The rule reproduces the fillet cylinder point at edges and the
;;; sphere point at corners, and vanishes on flat and saddle stars.

(define-shader-function site-centre (minority radius)
  "The centre of the rounding sphere or cylinder a site's star selects,
relative to the site: RADIUS along each nonzero axis of MINORITY."
  (* (clamp minority (vec3 -1.0 -1.0 -1.0) (vec3 1.0 1.0 1.0)) radius))

(define-shader-function rounded-point (point centre minority radius)
  "Project POINT onto the sphere or cylinder of RADIUS about CENTRE, or
return it unchanged when the MINORITY is zero and the site is flat."
  (let* ((offset (- point centre))
         (length (sqrt (dot offset offset))))
    (if (> (dot minority minority) 0.5)
        (+ centre (* offset (/ radius length)))
        point)))

(define-shader-function rounded-normal (point centre minority normal)
  "The rounded surface normal at POINT: away from CENTRE on a convex star,
toward it on a concave one, and NORMAL where the star is flat."
  (let* ((offset (- point centre))
         (length (sqrt (dot offset offset)))
         (sign (if (> (dot minority normal) 0.0) -1.0 1.0)))
    (if (> (dot minority minority) 0.5)
        (* offset (/ sign length))
        normal)))

(define-shader-function edge-minority (solid-c solid-e normal tangent)
  "The minority direction of an edge star, or zero when it is flat or saddle.

The face's own solid cell and air cell are known; SOLID-C and SOLID-E are the
solidity of the two cells across the edge in TANGENT direction, beside the
solid cell and the air cell respectively.  A convex crease moves into the
solid, a concave crease out of it, and both move inward along the face."
  (if (< (abs (- solid-c solid-e)) 0.5)
      (- (* normal (- (* 2.0 solid-c) 1.0)) tangent)
      (vec3 0.0 0.0 0.0)))

(define-shader-function vertex-minority
    (normal u v solid-cu solid-eu solid-cv solid-ev solid-cuv solid-euv)
  "The minority direction of a vertex star from the six unknown solidities.

The face's solid cell sits at (-N,-U,-V) and its air cell at (+N,-U,-V);
the six arguments are the cells across U, across V, and across both, each
beside the solid cell and beside the air cell."
  (let* ((count (+ 1.0 solid-cu solid-eu solid-cv solid-ev solid-cuv
                   solid-euv))
         (sum-n (+ -1.0 (- solid-cu) solid-eu (- solid-cv) solid-ev
                   (- solid-cuv) solid-euv))
         (sum-u (+ -1.0 solid-cu solid-eu (- solid-cv) (- solid-ev)
                   solid-cuv solid-euv))
         (sum-v (+ -1.0 (- solid-cu) (- solid-eu) solid-cv solid-ev
                   solid-cuv solid-euv))
         (solid-sum (+ (* normal sum-n) (* u sum-u) (* v sum-v))))
    (if (< count 3.5)
        solid-sum
        (if (> count 4.5)
            (- solid-sum)
            (vec3 0.0 0.0 0.0)))))

(define-shader-function chamfer-point (point minority width)
  "Move a shared point toward its star's minority, onto the chamfer.

Along a crease the shared point moves half the chamfer width, to the midline
of the flat 45-degree facet between the two offset faces.  At a pure corner,
where three facets of one sign meet, the corner is truncated by a plane
across all three axes: the three facets end in an equilateral triangle whose
vertices are the offset faces' corners and whose edge midpoints are the
facets' midlines.  Each face's corner grid square becomes one planar kite of
that triangle when the corner point moves to its centroid, two thirds of
the width along every axis.  #RUAWR5"
  (let* ((q (clamp minority (vec3 -1.0 -1.0 -1.0) (vec3 1.0 1.0 1.0)))
         (three-p (> (dot q q) 2.5))
         (pure-p (< (dot minority minority) 3.5))
         (reach (if three-p (if pure-p 0.6666667 0.5) 0.5)))
    (+ point (* q (* width reach)))))

(define-shader-function chamfer-normal (minority normal)
  "The facet normal at a chamfered shared point, or NORMAL when flat."
  (let* ((q (clamp minority (vec3 -1.0 -1.0 -1.0) (vec3 1.0 1.0 1.0)))
         (length (sqrt (dot q q)))
         (sign (if (> (dot minority normal) 0.0) 1.0 -1.0)))
    (if (> length 0.5)
        (* q (/ sign length))
        normal)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *paper-grain* 0.085
    "How deeply the paper material's tooth modulates its tone.")

  (defvar *paper-variation* 0.11
    "How far one cell's tone may drift from its neighbour's, in value and
in warmth.  Lonely Mountains: Downhill varies each object's colour a little
against a shared palette so no two trees are the same object twice; a block
world has the same problem in a starker form.")

  (defvar *paper-roundness* 0.40
    "How far a facet's normal is blended toward the smooth normal of its own
cell.  Flat-shaded triangles are perfectly flat, and a little of the smooth
variant across each face is what gives a low-poly surface its volume.")

  (defvar *bevel-rings* 2
    "Subdivision rings inside the rounding radius while a shaped stage is
generated: one is a chamfer, two a fillet.  The grid has 2*(rings+1) points
per side.")

  (defvar *bevel-rule* :round
    "How generated shared points move: :ROUND projects
onto fillet cylinders and corner spheres, :CHAMFER onto flat 45-degree facets.")

  (defun bevel-grid-side ()
    (* 2 (1+ *bevel-rings*)))

  (defun bevel-grid-index (i j)
    (+ (* i (bevel-grid-side)) j))

  (defun bevel-grid-parameter (k)
    "The in-plane parameter of grid line K: 0, then RADIUS in RINGS steps,
mirrored from the far side."
    (let ((side (bevel-grid-side)))
      (cond ((= k 0) 0.0)
            ((= k (1- side)) 1.0)
            ((<= k *bevel-rings*)
             `(* radius ,(/ (float k) *bevel-rings*)))
            (t
             `(- 1.0 (* radius ,(/ (float (- side 1 k)) *bevel-rings*)))))))

  (defun cell-solid-bindings (name position-form)
    "LET* bindings that read the solidity of the cell at POSITION-FORM."
    (let ((x0 (intern (format nil "~A-X0" name)))
          (y0 (intern (format nil "~A-Y0" name)))
          (x (intern (format nil "~A-X" name)))
          (y (intern (format nil "~A-Y" name)))
          (z (intern (format nil "~A-Z" name)))
          (index (intern (format nil "~A-INDEX" name)))
          (word (intern (format nil "~A-WORD" name))))
      `((,x0 (swizzle ,position-form :x))
        (,y0 (swizzle ,position-form :y))
        (,x (if (< ,x0 0.0) (+ ,x0 period-x)
                (if (< ,x0 period-x) ,x0 (- ,x0 period-x))))
        (,y (if (< ,y0 0.0) (+ ,y0 period-y)
                (if (< ,y0 period-y) ,y0 (- ,y0 period-y))))
        (,z (max 0.0 (min (swizzle ,position-form :z) 255.0)))
        (,index (uint (+ ,x (* period-x (+ ,y (* period-y ,z))))))
        (,word (buffer-element cells (/ ,index (uint 32.0))))
        (,name (float (ldb (byte 1 (mod ,index (uint 32.0))) ,word))))))

  (defun bevel-corner-bindings ()
    "Per corner: the clamped minority Q, the sphere centre, and whether the
star is a pure single-cell corner or a genuine three-axis one."
    (loop for (u-side v-side) in '(("NEG" "NEG") ("NEG" "POS")
                                   ("POS" "NEG") ("POS" "POS"))
          for corner = (intern (format nil "CORNER-~A-~A" u-side v-side))
          for minority = (intern (format nil "~A-MINORITY" corner))
          for q = (intern (format nil "~A-Q" corner))
          append
          `((,q (clamp ,minority (vec3 -1.0 -1.0 -1.0) (vec3 1.0 1.0 1.0)))
            (,(intern (format nil "~A-CENTRE" corner))
             (+ anchor
                (* edge-a ,(if (string= u-side "NEG") 0.0 1.0))
                (* edge-b ,(if (string= v-side "NEG") 0.0 1.0))
                (* ,q radius)))
            (,(intern (format nil "~A-PURE" corner))
             (if (> (dot ,minority ,minority) 0.5)
                 (< (dot ,minority ,minority) 3.5)
                 (> 0.0 1.0)))
            (,(intern (format nil "~A-THREE" corner))
             (> (dot ,q ,q) 2.5)))))

  (defun bevel-point-bindings (i j)
    "Bindings placing grid point I,J on the rounded surface.

Every point projects onto the sphere or cylinder its nearest crease selects.
Vertices use their own star.  Other points start from the nearest creased
edge's cylinder, or stay flat, and blend toward the sphere of the nearest
vertex by their distance to it whenever that vertex is a genuine three-axis
corner; a pure corner star, one solid or one air cell, is the sphere
outright, which is exactly the rounded box.  Inner points stay on the plane."
    (let* ((side (bevel-grid-side))
           (last (1- side))
           (half (floor side 2))
           (name (format nil "~D~D" i j))
           (flat (intern (format nil "FLAT-~A" name)))
           (point (intern (format nil "POINT-~A" name)))
           (normal (intern (format nil "NORMAL-~A" name)))
           (clip (intern (format nil "CLIP-~A" name)))
           (s (bevel-grid-parameter i))
           (t* (bevel-grid-parameter j))
           (u-side (if (< i half) "NEG" "POS"))
           (v-side (if (< j half) "NEG" "POS"))
           ;; Ring steps from the nearest vertex along each axis, or NIL when
           ;; the point is beyond the radius along that axis.
           (u-steps (cond ((< i *bevel-rings*) i)
                          ((> i (- last *bevel-rings*)) (- last i))))
           (v-steps (cond ((< j *bevel-rings*) j)
                          ((> j (- last *bevel-rings*)) (- last j))))
           (u-edge (and u-steps (intern (format nil "EDGE-U-~A" u-side))))
           (v-edge (and v-steps (intern (format nil "EDGE-V-~A" v-side))))
           (corner (intern (format nil "CORNER-~A-~A" u-side v-side)))
           (u-minority (and u-edge (intern (format nil "~A-MINORITY" u-edge))))
           (v-minority (and v-edge (intern (format nil "~A-MINORITY" v-edge))))
           (corner-minority (intern (format nil "~A-MINORITY" corner)))
           (corner-centre (intern (format nil "~A-CENTRE" corner)))
           (corner-pure (intern (format nil "~A-PURE" corner)))
           (corner-three (intern (format nil "~A-THREE" corner)))
           (u-foot `(+ anchor (* edge-a ,(if (string= u-side "NEG") 0.0 1.0))
                       (* edge-b ,t*)))
           (v-foot `(+ anchor (* edge-a ,s)
                       (* edge-b ,(if (string= v-side "NEG") 0.0 1.0))))
           (base-minority (intern (format nil "BASE-MINORITY-~A" name)))
           (base-centre (intern (format nil "BASE-CENTRE-~A" name)))
           (base-point (intern (format nil "BASE-POINT-~A" name)))
           (base-normal (intern (format nil "BASE-NORMAL-~A" name)))
           (sphere-point (intern (format nil "SPHERE-POINT-~A" name)))
           (sphere-normal (intern (format nil "SPHERE-NORMAL-~A" name)))
           (weight (intern (format nil "WEIGHT-~A" name)))
           (mixed-normal (intern (format nil "MIXED-NORMAL-~A" name)))
           (flat-form `(+ anchor (* edge-a ,s) (* edge-b ,t*))))
      (append
       `((,flat ,flat-form))
       (cond
         ;; Woodworking: shared points move to the meeting of flat facets;
         ;; with one ring there are no other moving points.
         ((and (eq *bevel-rule* :chamfer)
               (or (member i (list 0 last)) (member j (list 0 last))))
          (let ((site-minority
                  (cond ((and (member i (list 0 last)) (member j (list 0 last)))
                         corner-minority)
                        ((member i (list 0 last)) u-minority)
                        (t v-minority))))
            `((,point (+ anchor
                         (* (- (chamfer-point ,flat ,site-minority radius)
                               anchor)
                            scale)))
              (,normal (chamfer-normal ,site-minority normal)))))
         ((and (eq *bevel-rule* :chamfer)
               (or u-steps v-steps))
          (error "Chamfer geometry has no ring points; use one ring."))
         ;; A shared vertex: its own star, and nothing else.
         ((and (member i (list 0 last)) (member j (list 0 last)))
          `((,point (+ anchor
                       (* (- (rounded-point ,flat ,corner-centre
                                            ,corner-minority radius)
                             anchor)
                          scale)))
            (,normal (rounded-normal ,flat ,corner-centre ,corner-minority
                                     normal))))
         ;; Inner points stay on the plane.
         ((and (null u-steps) (null v-steps))
          `((,point (+ anchor (* (- ,flat anchor) scale)))
            (,normal normal)))
         (t
          (let* ((on-u-boundary (member i (list 0 last)))
                 (on-v-boundary (member j (list 0 last)))
                 ;; The base projection: the point's own edge when it lies on
                 ;; one, else the nearer creased edge's cylinder, else flat.
                 (base
                   (cond
                     (on-u-boundary `((,base-minority ,u-minority)
                                      (,base-centre
                                       (+ ,flat (site-centre ,u-minority
                                                             radius)))))
                     (on-v-boundary `((,base-minority ,v-minority)
                                      (,base-centre
                                       (+ ,flat (site-centre ,v-minority
                                                             radius)))))
                     ((and u-edge v-edge)
                      (let ((u-first (<= u-steps v-steps)))
                        `((,base-minority
                           (if (> (dot ,u-minority ,u-minority) 0.5)
                               (if (> (dot ,v-minority ,v-minority) 0.5)
                                   ,(if u-first u-minority v-minority)
                                   ,u-minority)
                               ,v-minority))
                          (,base-centre
                           (if (> (dot ,u-minority ,u-minority) 0.5)
                               (if (> (dot ,v-minority ,v-minority) 0.5)
                                   (+ ,(if u-first u-foot v-foot)
                                      (site-centre ,(if u-first u-minority
                                                        v-minority)
                                                   radius))
                                   (+ ,u-foot (site-centre ,u-minority radius)))
                               (+ ,v-foot (site-centre ,v-minority radius)))))))
                     (u-edge `((,base-minority ,u-minority)
                               (,base-centre
                                (+ ,u-foot (site-centre ,u-minority radius)))))
                     (t `((,base-minority ,v-minority)
                          (,base-centre
                           (+ ,v-foot (site-centre ,v-minority radius)))))))
                 ;; Distance to the nearest vertex in ring steps: along the
                 ;; edge for boundary points, the larger axis otherwise.
                 (steps (cond (on-u-boundary (or v-steps *bevel-rings*))
                              (on-v-boundary (or u-steps *bevel-rings*))
                              (t (max (or u-steps *bevel-rings*)
                                      (or v-steps *bevel-rings*)))))
                 (falloff (max 0.0 (- 1.0 (/ (float steps) *bevel-rings*)))))
            `(,@base
              (,base-point (rounded-point ,flat ,base-centre ,base-minority
                                          radius))
              (,base-normal (rounded-normal ,flat ,base-centre ,base-minority
                                            normal))
              (,sphere-point (rounded-point ,flat ,corner-centre
                                            ,corner-minority radius))
              (,sphere-normal (rounded-normal ,flat ,corner-centre
                                              ,corner-minority normal))
              (,weight (if ,corner-pure 1.0
                           (if ,corner-three ,falloff 0.0)))
              (,point (+ anchor
                         (* (- (mix ,base-point ,sphere-point ,weight) anchor)
                            scale)))
              (,mixed-normal (mix ,base-normal ,sphere-normal ,weight))
              (,normal (normalize ,mixed-normal))))))
       `((,clip (view-clip ,point camera right up forward
                           projection-vector)))))))

;;; Fine woodworking shading: facets are flat, read from the screen-space
;;; derivatives of the world position, and only the arris between a face and
;;; its chamfer is sanded, by blending toward the face normal within a small
;;; band on either side of the chamfer line.
(define-temporal-fragment-shaders
    (chamfer-fragment-shader temporal-chamfer-fragment-shader
     (world-motion world))
    (:stage :fragment
     :inputs ((normal :vec3 :location 0)
              (world :vec3 :location 1)
              (uv :vec2 :location 2)
              (face-normal :vec3 :location 3))
     :outputs ((color :vec4 :location 0))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)
                 (cells :storage-buffer :binding #.+cells-binding+
                        :element :uint)))
  (let* ((raw-facet (cross-product (derivative-x world) (derivative-y world)))
         (facet (normalize raw-facet))
         (face (normalize face-normal))
         (oriented (if (< (dot facet face) 0.0) (- facet) facet))
         (width (swizzle domain-vector :z))
         (softness (max (swizzle domain-vector :w) 0.0005))
         (u (swizzle uv :x))
         (v (swizzle uv :y))
         (inset (min (min u (- 1.0 u)) (min v (- 1.0 v))))
         ;; Signed distance from the chamfer line: negative on the chamfer.
         (arris (- inset width))
         (sanded (normalize (mix oriented face
                                 (smoothstep (- softness) softness arris))))
         (upness (swizzle face :z))
         (base (if (> upness 0.5)
                   (swizzle top-vector :xyz)
                   (if (< upness -0.5)
                       (swizzle bottom-vector :xyz)
                       (swizzle side-vector :xyz))))
         ;; A chamfered surface lies inside its own cell, by up to the
         ;; width along every axis its facet cuts, so leave along the facet
         ;; normal, which is the one direction certain to point out of the
         ;; solid, and by more than the width.
         (walk-origin (+ world (* oriented (+ (* 2.0 width) 0.1))))
         (walk (marched-cell-walk walk-origin (swizzle sun-vector :xyz)
                                  cells (swizzle domain-vector :x)
                                  (swizzle domain-vector :y)
                                  #.*shadow-steps*))
         (shade (mix 1.0 (if (> (swizzle walk :w) 0.5) 0.0 1.0)
                     (swizzle occlusion-vector :y)))
         (crowding (crowded-sky walk-origin face cells
                                (swizzle domain-vector :x)
                                (swizzle domain-vector :y)
                                #.*occlusion-steps*))
         (open (- 1.0 (* (swizzle occlusion-vector :x) crowding)))
         (final (surface-lighting base sanded world open shade
                                  camera-vector sun-vector sun-colour-vector
                                  fill-vector sky-vector ground-vector)))
    (set-output color (vec4 final 1.0))))


;;; ------------------------------------------------------------------------
;;; Sky: one triangle behind everything
;;;
;;; The clear colour is a single flat tone, which is honest and which no
;;; photograph has.  This draws the background instead: one full-screen
;;; triangle from the screen vertex stage, shaded by the direction each pixel
;;; looks along.  It carries the same SKY-VECTOR the fog converges to, so
;;; the horizon of the gradient and the far end of the fog are the same
;;; colour by construction, and the zenith is free to be deeper.

(define-shader-function sky-radiance (direction sun-vector sun-colour-vector
                                      sky-vector ground-vector)
  "The background along DIRECTION: gradient, horizon haze, and the sun.

The values are display-referred like SKY-VECTOR itself, not scene radiance:
what the fog converges to and what the background paints must agree."
  (let* ((sky (swizzle sky-vector :xyz))
         (ground (swizzle ground-vector :xyz))
         (sun (swizzle sun-vector :xyz))
         (radiance (swizzle sun-colour-vector :xyz))
         (upness (swizzle direction :z))
         (height (clamp upness 0.0 1.0))
         ;; A real sky darkens and saturates with height; the horizon keeps
         ;; the fog's colour exactly so distance dissolves into it.
         (zenith (* sky (vec3 0.50 0.64 0.92)))
         (above (mix sky zenith (expt height 0.45)))
         ;; Below the horizon the ground's bounce takes over, hazed.
         (below (mix sky (* ground 1.35) (clamp (* -4.0 upness) 0.0 1.0)))
         (band (mix below above (smoothstep -0.02 0.06 upness)))
         (toward (clamp (dot direction sun) 0.0 1.0))
         ;; A wide halo the eye reads as air, and a small disc it reads as
         ;; the sun; both tinted by the light's own colour.
         (halo (* 0.22 (expt toward 6.0)))
         (disc (* 0.85 (smoothstep 0.9994 0.99975 toward)))
         (glow (* radiance (+ halo disc))))
    (clamp (+ band glow) (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))))

(define-temporal-fragment-shaders
    (sky-fragment-shader temporal-sky-fragment-shader
     (direction-motion direction))
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color :vec4 :location 0))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)))
  ;; Invert VIEW-CLIP for a ray: clip x is view-x times the projection's x
  ;; scale over view-z, and clip y is that negated on the y scale.
  (let* ((sample-ndc (- ndc (swizzle jitter-vector :xy)))
         (right (swizzle right-vector :xyz))
         (up (swizzle up-vector :xyz))
         (forward (swizzle forward-vector :xyz))
         (x (/ (swizzle sample-ndc :x) (swizzle projection-vector :x)))
         (y (/ (swizzle sample-ndc :y) (swizzle projection-vector :y)))
         (direction (normalize (+ forward (- (* right x) (* up y)))))
         (final (sky-radiance direction sun-vector sun-colour-vector
                              sky-vector ground-vector)))
    (set-output color (vec4 final 1.0))))

;;; ------------------------------------------------------------------------
;;; The lens: a focus pass over the drawn frame
;;;
;;; luvcraft's focus post (#RLXR7B) blurs by comparing each pixel's depth to
;;; the depth under the centre of the frame, disperses colour toward the
;;; corners, and darkens them.  This is that pass with its bloom and light
;;; shafts left behind, reading the view distance out of alpha rather than a
;;; depth attachment.

(define-shader lens-fragment-shader
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color :vec4 :location 0))
     :resources ((scene :texture-2d :binding #.+scene-binding+
                        :sample-transfer :identity)
                 (scene-sampler :sampler :binding #.+sampler-binding+)
                 (frame :uniform-block :binding #.+lens-frame-binding+
                        :members #.*frame-uniform-members*)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (texel (vec2 (swizzle lens-vector :z) (swizzle lens-vector :w)))
         (aperture (swizzle lens-vector :y))
         (centered (- uv (vec2 0.5 0.5)))
         (radial (dot centered centered))
         ;; A lens does not focus every wavelength on one circle, and the
         ;; error grows off axis: three taps a fraction of a texel apart.
         (dispersion (* centered (* radial (* 0.0055 aperture))))
         (sharp (sample scene scene-sampler uv))
         (fringed (vec4 (swizzle (sample scene scene-sampler
                                         (+ uv dispersion)) :x)
                        (swizzle sharp :y)
                        (swizzle (sample scene scene-sampler
                                         (- uv dispersion)) :z)
                        (swizzle sharp :w)))
         (depth (swizzle sharp :w))
         (focus (swizzle (sample scene scene-sampler (vec2 0.5 0.5)) :w))
         ;; Only what lies behind the subject softens; the near field of a
         ;; landscape photograph is where the planed edges are.
         (defocus (clamp (* aperture (smoothstep 0.012 0.30 (- depth focus)))
                         0.0 1.0))
         ;; The nine-tap tent is written out rather than factored into a
         ;; shader function: SAMPLE reads a declared resource, and a texture
         ;; passed as a parameter is not one.
         (spread (* texel (+ 1.0 (* 4.0 aperture))))
         (bx (* spread (vec2 1.0 0.0)))
         (by (* spread (vec2 0.0 1.0)))
         (wide (* (+ (+ (* (sample scene scene-sampler uv) 4.0)
                        (+ (+ (* (sample scene scene-sampler (+ uv bx)) 2.0)
                              (* (sample scene scene-sampler (- uv bx)) 2.0))
                           (+ (* (sample scene scene-sampler (+ uv by)) 2.0)
                              (* (sample scene scene-sampler (- uv by)) 2.0))))
                     (+ (+ (sample scene scene-sampler (+ (+ uv bx) by))
                           (sample scene scene-sampler (+ (- uv bx) by)))
                        (+ (sample scene scene-sampler (- (+ uv bx) by))
                           (sample scene scene-sampler (- (- uv bx) by)))))
                  0.0625))
         (mixed (mix fringed wide defocus))
         ;; The corners of a real frame fall off; keep it gentle enough to
         ;; read as a lens rather than as an effect.
         (vignette (- 1.0 (* 0.42 (* radial radial))))
         (shaded (* (swizzle mixed :xyz) vignette))
         ;; A grade, in the sense a colourist means: hold the shadows toward
         ;; the sky's blue, let the highlights run warm, and open the
         ;; saturation a little.  Split toning is what separates a rendered
         ;; frame from a photographed one more than any single other step.
         (luma (dot shaded (vec3 0.2126 0.7152 0.0722)))
         (saturated (mix (vec3 luma luma luma) shaded 1.10))
         (lifted (+ saturated (* (vec3 0.010 0.016 0.030) (- 1.0 luma))))
         (final (clamp (* lifted (mix (vec3 1.0 1.0 1.0)
                                      (vec3 1.045 1.008 0.955)
                                      luma))
                       (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))))
    (set-output color (vec4 final 1.0))))

;;; ------------------------------------------------------------------------
;;; Temporal resolve and presentation
;;; #C7WIN4 #C4ED2V

(define-shader-function rgb-to-ycocg (rgb)
  "Put RGB into a luminance/chroma space whose box is a useful colour clip."
  (vec3 (+ (* (swizzle rgb :x) 0.25)
           (* (swizzle rgb :y) 0.50)
           (* (swizzle rgb :z) 0.25))
        (* 0.5 (- (swizzle rgb :x) (swizzle rgb :z)))
        (+ (* (swizzle rgb :x) -0.25)
           (* (swizzle rgb :y) 0.50)
           (* (swizzle rgb :z) -0.25))))

(define-shader-function ycocg-to-rgb (value)
  "Invert RGB-TO-YCOCG."
  (vec3 (+ (swizzle value :x) (swizzle value :y)
           (- (swizzle value :z)))
        (+ (swizzle value :x) (swizzle value :z))
        (+ (swizzle value :x) (- (swizzle value :y))
           (- (swizzle value :z)))))

(define-shader temporal-resolve-fragment-shader
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color :vec4 :location 0))
     :resources
     ((current :texture-2d :binding #.+current-binding+
               :sample-transfer :identity)
      (motion-texture :texture-2d :binding #.+motion-binding+
                      :sample-transfer :identity)
      (history :texture-2d :binding #.+history-binding+
               :sample-transfer :identity)
      (temporal-sampler :sampler :binding #.+temporal-sampler-binding+)
      (frame :uniform-block :binding #.+temporal-frame-binding+
             :members #.*frame-uniform-members*)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (texel (swizzle temporal-vector :xy))
         (dx (vec2 (swizzle texel :x) 0.0))
         (dy (vec2 0.0 (swizzle texel :y)))
         (centre (sample current temporal-sampler uv))
         (c00 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (- (- uv dx) dy))
                        :xyz)))
         (c10 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (- uv dy)) :xyz)))
         (c20 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ (- uv dy) dx))
                        :xyz)))
         (c01 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (- uv dx)) :xyz)))
         (c11 (rgb-to-ycocg (swizzle centre :xyz)))
         (c21 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ uv dx)) :xyz)))
         (c02 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ (- uv dx) dy))
                        :xyz)))
         (c12 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ uv dy)) :xyz)))
         (c22 (rgb-to-ycocg
               (swizzle (sample current temporal-sampler (+ (+ uv dx) dy))
                        :xyz)))
         (neighbourhood-min (min c00 c10 c20 c01 c11 c21 c02 c12 c22))
         (neighbourhood-max (max c00 c10 c20 c01 c11 c21 c02 c12 c22))
         ;; Motion deliberately excludes jitter.  History contains last
         ;; frame's jittered samples, so its address includes the clip-space
         ;; jitter delta converted to UV units here, in one visible place.
         ;; Velocity is categorical at geometry edges; filtering it would
         ;; invent a third surface between foreground and background.  The
         ;; shader language already has exact TEXEL-LOAD, so this needs no
         ;; extra sampler or HAL operation.
         (pixel (uvec2 (uint (/ (swizzle uv :x) (swizzle texel :x)))
                       (uint (/ (swizzle uv :y) (swizzle texel :y)))))
         (velocity (swizzle (texel-load motion-texture pixel) :xy))
         (jitter-delta
           (* (- (swizzle jitter-vector :zw) (swizzle jitter-vector :xy)) 0.5))
         (history-uv (+ (+ uv velocity) jitter-delta))
         (inside
           (* (* (step 0.0 (swizzle history-uv :x))
                 (step (swizzle history-uv :x) 1.0))
              (* (step 0.0 (swizzle history-uv :y))
                 (step (swizzle history-uv :y) 1.0))))
         (old (rgb-to-ycocg
               (swizzle (sample history temporal-sampler history-uv) :xyz)))
         (clipped (clamp old neighbourhood-min neighbourhood-max))
         ;; A rapid reprojection or a colour discontinuity should be more
         ;; responsive.  The shaped AABB above remains the primary rejection
         ;; mechanism; these factors make its edge cases settle promptly.
         (speed (clamp (* (sqrt (dot velocity velocity)) 48.0) 0.0 1.0))
         (difference
           (clamp (* (abs (- (swizzle clipped :x) (swizzle c11 :x))) 4.0)
                  0.0 1.0))
         (configured-weight (swizzle temporal-vector :w))
         (history-weight
           (* (* (swizzle temporal-vector :z) inside)
              (* configured-weight
                 (* (- 1.0 (* speed 0.35)) (- 1.0 difference)))))
         (resolved
           (ycocg-to-rgb (mix c11 clipped history-weight))))
    ;; Alpha is per-frame focus metadata, never temporal colour history.
    (set-output color (vec4 resolved (swizzle centre :w)))))

(define-shader present-fragment-shader
    (:stage :fragment
     :inputs ((ndc :vec2 :location 0))
     :outputs ((color :vec4 :location 0))
     :resources ((scene :texture-2d :binding #.+scene-binding+
                        :sample-transfer :identity)
                 (scene-sampler :sampler :binding #.+sampler-binding+)
                 (frame :uniform-block :binding #.+lens-frame-binding+
                        :members #.*frame-uniform-members*)))
  (let* ((uv (+ (* ndc 0.5) (vec2 0.5 0.5)))
         (value (sample scene scene-sampler uv)))
    (set-output color (vec4 (swizzle value :xyz) 1.0))))

;;; ------------------------------------------------------------------------
;;; Paper: a matte material for photographing the chamfers

;;; The hash and its value noise come from luvcraft's sky (#9SSXDJ); they are
;;; ordinary lattice mathematics with no luvcraft in them, and the atelier
;;; wants a paper tooth rather than a cloud.
(define-shader-function paper-hash (site)
  "Hash one integer lattice site into the unit interval, without a sine."
  (let* ((scattered (fract (* site 0.1031)))
         (shift (dot scattered
                     (+ (swizzle scattered :zyx)
                        (vec3 31.32 31.32 31.32))))
         (folded (+ scattered (vec3 shift shift shift))))
    (fract (* (+ (swizzle folded :x) (swizzle folded :y))
              (swizzle folded :z)))))

(define-shader-function paper-noise (point)
  "Smoothly interpolated value noise over an integer lattice."
  (let* ((lattice (floor point))
         (offset (fract point))
         (weight (* offset (* offset (- (vec3 3.0 3.0 3.0) (* offset 2.0)))))
         (u (swizzle weight :x))
         (v (swizzle weight :y))
         (w (swizzle weight :z))
         (near-low (mix (paper-hash lattice)
                        (paper-hash (+ lattice (vec3 1.0 0.0 0.0))) u))
         (near-high (mix (paper-hash (+ lattice (vec3 0.0 1.0 0.0)))
                         (paper-hash (+ lattice (vec3 1.0 1.0 0.0))) u))
         (far-low (mix (paper-hash (+ lattice (vec3 0.0 0.0 1.0)))
                       (paper-hash (+ lattice (vec3 1.0 0.0 1.0))) u))
         (far-high (mix (paper-hash (+ lattice (vec3 0.0 1.0 1.0)))
                        (paper-hash (+ lattice (vec3 1.0 1.0 1.0))) u)))
    (mix (mix near-low near-high v) (mix far-low far-high v) w)))

(define-shader-function paper-tooth (world)
  "The paper's grain at WORLD: two octaves, centred on one."
  (let* ((coarse (paper-noise (* world 3.7)))
         (fine (paper-noise (* world 19.0))))
    (+ 1.0 (* #.*paper-grain* (- (+ (* 0.65 coarse) (* 0.35 fine)) 0.5)))))

;;; ACES's filmic fit, as luvcraft tonemaps with (#IC14P3).  The exponential
;;; roll-off of SURFACE-LIGHTING desaturates as it clips; this keeps a lit
;;; chamfer's glint white without bleaching the tone beside it.
(define-shader-function paper-tonemap (radiance)
  "Map linear scene radiance to display-linear [0,1] through the ACES fit."
  (let* ((numerator
           (* radiance (+ (* radiance 2.51) (vec3 0.03 0.03 0.03))))
         (denominator
           (+ (* radiance (+ (* radiance 2.43) (vec3 0.59 0.59 0.59)))
              (vec3 0.14 0.14 0.14))))
    (clamp (/ numerator denominator)
           (vec3 0.0 0.0 0.0) (vec3 1.0 1.0 1.0))))

(define-shader-function paper-lighting
    (base normal world occlusion shade glint camera-vector sun-vector
     sun-colour-vector fill-vector sky-vector ground-vector)
  "The lit colour of a matte, slightly toothed surface.

Unlike SURFACE-LIGHTING this wraps the key light around the terminator
rather than clipping it at the horizon, which is what makes a paper model
read as paper: no face goes black merely because it turned away.  GLINT is
the narrow specular the chamfers carry, and it alone is allowed to reach
the white the tonemap defends."
  (let* ((sun (swizzle sun-vector :xyz))
         (ambient (swizzle sun-vector :w))
         (sky (swizzle sky-vector :xyz))
         (ground (swizzle ground-vector :xyz))
         (exposure (swizzle ground-vector :w))
         (radiance (swizzle sun-colour-vector :xyz))
         (sheen (swizzle sun-colour-vector :w))
         (camera (swizzle camera-vector :xyz))
         (delta (- world camera))
         (distance (sqrt (dot delta delta)))
         (view (/ delta (- distance)))
         ;; Half-Lambert: the terminator becomes a long soft gradient across
         ;; the whole hemisphere instead of a hard edge at ninety degrees.
         (facing (dot normal sun))
         (wrapped (expt (clamp (+ 0.5 (* 0.5 facing)) 0.0 1.0) 1.55))
         (key (* wrapped (* shade (mix 1.0 occlusion 0.45))))
         (fill (* (swizzle fill-vector :w)
                  (max (dot normal (swizzle fill-vector :xyz)) 0.0)))
         (upness (swizzle normal :z))
         (sky-weight (* occlusion (+ 0.5 (* 0.5 upness))))
         (ground-weight (* occlusion (- 0.5 (* 0.5 upness))))
         (half (normalize (+ sun view)))
         ;; A hand-planed facet is not a mirror: the lobe is wide enough to
         ;; survive a moving camera, and narrow enough to stay a line.
         (specular (* sheen (* glint (* (max (dot normal half) 0.0)
                                        (expt (max (dot normal half) 0.0)
                                              90.0)))))
         (light (+ (* radiance key)
                   (+ (* sky (+ (* ambient sky-weight) fill))
                      (* ground (* ambient ground-weight)))))
         (tinted (* base (* light (paper-tooth world))))
         (lit (+ tinted (* radiance (* specular (* shade (max facing 0.0))))))
         (exposed (paper-tonemap (* lit exposure)))
         (fog-far (swizzle sky-vector :w))
         (fog (smoothstep (* 0.55 fog-far) fog-far distance)))
    (mix exposed sky fog)))

(define-temporal-fragment-shaders
    (paper-fragment-shader temporal-paper-fragment-shader
     (world-motion world))
    (:stage :fragment
     :inputs ((normal :vec3 :location 0)
              (world :vec3 :location 1)
              (uv :vec2 :location 2)
              (face-normal :vec3 :location 3))
     :outputs ((color :vec4 :location 0))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)
                 (cells :storage-buffer :binding #.+cells-binding+
                        :element :uint)))
  (let* ((raw-facet (cross-product (derivative-x world) (derivative-y world)))
         (facet (normalize raw-facet))
         (face (normalize face-normal))
         (oriented (if (< (dot facet face) 0.0) (- facet) facet))
         (width (swizzle domain-vector :z))
         (softness (max (swizzle domain-vector :w) 0.0005))
         (u (swizzle uv :x))
         (v (swizzle uv :y))
         (inset (min (min u (- 1.0 u)) (min v (- 1.0 v))))
         (arris (- inset width))
         (sanded (normalize (mix oriented face
                                 (smoothstep (- softness) softness arris))))
         ;; What glints is what actually tilts.  A face's UV border is not a
         ;; crease: two coplanar cells share one, and masking by inset drew a
         ;; bright grid across every flat wall and floor.  The facet's own
         ;; departure from the face it cuts is the honest measure, and it is
         ;; exactly zero wherever the surface continues flat.
         (tilt (- 1.0 (abs (dot oriented face))))
         (crease (smoothstep 0.015 0.30 tilt))
         (glint crease)
         (upness (swizzle face :z))
         (tone (if (> upness 0.5)
                   (swizzle top-vector :xyz)
                   (if (< upness -0.5)
                       (swizzle bottom-vector :xyz)
                       (swizzle side-vector :xyz))))
         ;; The cell behind this face, and two independent hashes of it: one
         ;; moves the tone's value, the other its warmth, so a wall of cells
         ;; stops reading as one painted surface.
         (cell (floor (- world (* oriented 0.25))))
         ;; Most of the drift belongs to patches several cells across, or the
         ;; world reads as a quilt: one cell, one square.  The per-cell hash
         ;; only breaks up the patches' own edges.
         (patch (- (paper-noise (* cell 0.21)) 0.5))
         (jitter (- (paper-hash cell) 0.5))
         (warm-patch (paper-noise (+ (* cell 0.13) (vec3 19.7 7.3 3.1))))
         (value (+ 1.0 (* #.*paper-variation*
                          (+ (* 1.35 patch) (* 0.45 jitter)))))
         (warmth (mix (vec3 0.965 0.99 1.04) (vec3 1.04 1.01 0.96)
                      warm-patch))
         (base (* tone (* warmth value)))
         ;; A cell's own smooth normal: the direction out of its middle.  A
         ;; little of it across each facet is the low-poly volume trick.
         (middle (+ cell (vec3 0.5 0.5 0.5)))
         (round-normal (normalize (- world middle)))
         (walk-origin (+ world (* oriented (+ (* 2.0 width) 0.1))))
         (walk (marched-cell-walk walk-origin (swizzle sun-vector :xyz)
                                  cells (swizzle domain-vector :x)
                                  (swizzle domain-vector :y)
                                  #.*shadow-steps*))
         ;; A paper model's shadow is soft and never quite black.
         (shade (mix 1.0 (if (> (swizzle walk :w) 0.5) 0.30 1.0)
                     (swizzle occlusion-vector :y)))
         (crowding (crowded-sky walk-origin face cells
                                (swizzle domain-vector :x)
                                (swizzle domain-vector :y)
                                #.*occlusion-steps*))
         (open (- 1.0 (* (swizzle occlusion-vector :x) crowding)))
         ;; The same measure governs the rounding: blending every cell toward
         ;; the normal of its own middle turns a flat wall into a wall of
         ;; tiles, so only a creased cell is allowed to round.
         (modelled (normalize (mix sanded round-normal
                                   (* #.*paper-roundness* crease))))
         (final (paper-lighting base modelled world open shade glint
                                camera-vector sun-vector sun-colour-vector
                                fill-vector sky-vector ground-vector))
         ;; Alpha carries this fragment's distance, scaled by the lens lane's
         ;; focus, so the focus pass needs no depth attachment of its own:
         ;; one texture is the whole of what it reads.
         (delta (- world (swizzle camera-vector :xyz)))
         (range (max (swizzle lens-vector :x) 1.0))
         (reach (clamp (/ (sqrt (dot delta delta)) (* 2.0 range)) 0.0 1.0))
         ;; Only a lens reads alpha.  Without one the frame is a picture, and
         ;; a picture is opaque: a captured PNG must not come out of here
         ;; carrying distance in its alpha channel.
         (depth (if (> (swizzle lens-vector :y) 0.0) reach 1.0)))
    (set-output color (vec4 final depth))))

(defun frame-uniform-block ()
  "The frame uniform block as declared by the surface fragment stage."
  (find-if (lambda (resource) (typep resource 'shader-uniform-block))
           (shader-specification-resources (surface-fragment-shader))))
