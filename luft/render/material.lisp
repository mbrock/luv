;;; The stock: what the world is cut from.
;;;
;;; Every style so far has shaded the world in one substance -- turf above,
;;; earth at a cut, dark underneath -- because a face's direction was the
;;; only thing a fragment knew about the matter behind it.  That is enough
;;; to read a solid and not enough to read a building.  A stone pier and an
;;; oak deck differ in colour, in figure, in how they take a highlight, and
;;; above all in what their planed arris does with the light: the chamfer on
;;; a mahogany board is paler than its face because the cut crosses the
;;; fibre, and the chamfer on a bronze rail is brighter than its face
;;; because a handled edge polishes.
;;;
;;; So a material here is not a shader.  It is a short list of numbers -- an
;;; albedo, a finish, a grain, a mottle, a way of weathering -- carried in
;;; five lanes of the frame block and read by one fragment stage.  Adding a
;;; stone is editing a table, not writing a shader, and a contact sheet can
;;; put eight of them side by side under one light because a column of the
;;; sheet is just a binding of *MATERIAL*.
;;;
;;; The :STOCK style draws the chamfered geometry, whose crisp planed facets
;;; are what these materials are for, and lights it from the occupancy field:
;;; a soft shadow and a smooth crowding, so the edges stay sharp while the
;;; light stays gentle.  #ADEAKZ #TI9NJP

(in-package #:luft.render.shaders)

;;; ------------------------------------------------------------------------
;;; Figure: grain, fibre, and mottle
;;;
;;; Wood is the hard case and the one that pays.  A board's figure is the
;;; intersection of the cut with a bundle of concentric growth rings around
;;; the pith, so the honest model is a pith line, not a texture: a face
;;; across the grain meets the rings as circles and a face along it as long
;;; streaks, and no face has to be told which it is.  One trunk in a
;;; thirty-cell world would put the pith so far away that every ring read
;;; straight, so the trunks repeat on a loose lattice across the grain --
;;; which is also, conveniently, what a laminated butcher block is.

(define-shader-function stock-turbulence (point)
  "Two octaves of value noise about zero: a slow warp for the rings."
  (+ (- (paper-noise point) 0.5)
     (* 0.5 (- (paper-noise (* point 2.9)) 0.5))))

(define-shader-function stock-grain
    (world axis spacing rings wander ring-contrast fibre-contrast)
  "How much darker than its albedo the stock is at WORLD, one for none.

AXIS is the direction the grain runs and SPACING the distance across it
between pith lines.  The point's distance from the nearest pith, jittered
per trunk and warped by STOCK-TURBULENCE by WANDER cells, is banded into
RINGS growth rings to the cell; RING-CONTRAST darkens the latewood and
FIBRE-CONTRAST the finer noise stretched along the grain between them."
  (let* ((along (dot world axis))
         (across (- world (* axis along)))
         (mask (- (vec3 1.0 1.0 1.0) (abs axis)))
         (period (max spacing 0.25))
         (half (* period 0.5))
         (shifted (+ across (vec3 half half half)))
         (trunk (floor (/ shifted period)))
         (centre (- shifted (* trunk period)))
         ;; Each trunk sits a little off its lattice point, in the plane
         ;; across the grain so the rings stay constant along it.
         (drift (* mask (* (- (paper-hash trunk) 0.5) (* period 0.55))))
         (local (- centre (+ (vec3 half half half) drift)))
         (radius (sqrt (max (dot local local) 0.000001)))
         (warp (stock-turbulence (+ (* local 0.8) (* axis (* along 0.15)))))
         ;; A different trunk begins its rings at a different radius.
         (phase (+ (* (+ radius (* wander warp)) rings)
                   (* 7.0 (paper-hash (+ trunk (vec3 3.7 1.9 8.3))))))
         (ring (- 0.5 (* 0.5 (cos (* phase 6.2831855)))))
         ;; Latewood is a narrow dark line, not half the board.
         (late (expt (clamp ring 0.0 1.0) 2.6))
         (fibre (- (paper-noise (+ (* local 22.0) (* axis (* along 2.3))))
                   0.5)))
    (- 1.0 (+ (* ring-contrast late) (* fibre-contrast fibre)))))

(define-shader-function stock-fold (u)
  "How far U is from the nearest whole number, doubled: zero on a joint."
  (let* ((f (fract u)))
    (* 2.0 (min f (- 1.0 f)))))

(define-shader-function stock-courses (world face courses width)
  "The mortar grid of a coursed wall at WORLD, one between the joints.

COURSES is how many courses of brick go to the cell and WIDTH how wide the
joint is as a fraction of a course.  Bed joints run level, so they are read
off Z; perpends run between them and are staggered half a brick every
course, which is the whole of what a stretcher bond is.  A face whose
normal is vertical is a paving rather than a wall, and takes its two
directions from the plan instead."
  (let* ((flat (< (abs (swizzle face :z)) 0.5))
         (v (if flat (swizzle world :z) (swizzle world :y)))
         (h (if flat
                (if (> (abs (swizzle face :x)) 0.5)
                    (swizzle world :y)
                    (swizzle world :x))
                (swizzle world :x)))
         (course (* v courses))
         (row (floor course))
         ;; A brick is twice as long as it is high, and every other course
         ;; begins half a brick along.
         (stagger (* 0.5 (- (/ row 2.0) (floor (/ row 2.0)))))
         (along (+ (* h (* courses 0.5)) stagger))
         (bed (smoothstep 0.0 width (stock-fold course)))
         (perpend (smoothstep 0.0 (* width 1.4) (stock-fold along)))
         (joint (min bed perpend))
         ;; Each brick a little different from its neighbour, which is what
         ;; keeps a coursed wall from reading as graph paper.
         (brick (vec3 (floor along) (float row) 0.0))
         (shade (+ 0.88 (* 0.24 (paper-hash brick)))))
    (mix 0.62 shade joint)))

(define-shader-function stock-mottle (world scale contrast)
  "The patchiness of a stone at WORLD, one for none: two octaves about one."
  (let* ((coarse (- (paper-noise (* world scale)) 0.5))
         (fine (- (paper-noise (* world (* scale 3.4))) 0.5)))
    (+ 1.0 (* contrast (+ (* 0.7 coarse) (* 0.3 fine))))))

;;; ------------------------------------------------------------------------
;;; Light on a finished surface
;;;
;;; PAPER-LIGHTING wraps the key light around the terminator so that a face
;;; turned from the sun does not go black, which is what makes a model read
;;; as a made object rather than a lit polygon.  This keeps that and gives
;;; the highlight over to the material: its strength, its tightness, whether
;;; it takes the colour of the metal under it, and a rim term that lets a
;;; stone's silhouette catch the sky.

(define-shader-function stock-lighting
    (base normal world occlusion shade finish tint camera-vector sun-vector
     sun-colour-vector fill-vector sky-vector ground-vector)
  "The lit, tonemapped, fogged colour of BASE under the frame's lights.

FINISH is the gloss strength, its power, the rim strength, and how metallic
the stock is; TINT is what the reflections are multiplied by, white for a
dielectric and the albedo for a metal.  OCCLUSION scales the ambient
hemisphere and SHADE the direct sun.

A metal is not a brown surface with a white spot on it.  What makes bronze
read as bronze is that it returns the whole sky along the mirror direction
and almost nothing diffusely, so the hemisphere is sampled a second time
about the reflected view and the diffuse term is turned down as the metal
goes up."
  (let* ((sun (swizzle sun-vector :xyz))
         (ambient (swizzle sun-vector :w))
         (sky (swizzle sky-vector :xyz))
         (ground (swizzle ground-vector :xyz))
         (exposure (swizzle ground-vector :w))
         (radiance (swizzle sun-colour-vector :xyz))
         (camera (swizzle camera-vector :xyz))
         (delta (- world camera))
         (distance (sqrt (dot delta delta)))
         (view (/ delta (- distance)))
         (facing (dot normal sun))
         ;; A little wrap past the terminator, not the half-Lambert of a
         ;; paper model: a face turned from the sun must stay clearly
         ;; darker than one facing it, or the solid stops reading as one.
         (wrapped (expt (clamp (+ 0.16 (* 0.84 facing)) 0.0 1.0) 1.35))
         (key (* wrapped (* shade (mix 1.0 occlusion 0.45))))
         (fill (* (swizzle fill-vector :w)
                  (max (dot normal (swizzle fill-vector :xyz)) 0.0)))
         (upness (swizzle normal :z))
         (sky-weight (* occlusion (+ 0.5 (* 0.5 upness))))
         (ground-weight (* occlusion (- 0.5 (* 0.5 upness))))
         (half (normalize (+ sun view)))
         (gloss (swizzle finish :x))
         (power (max (swizzle finish :y) 1.0))
         (metallic (swizzle finish :w))
         (grazing (clamp (- 1.0 (max (dot normal view) 0.0)) 0.0 1.0))
         (rim (* (swizzle finish :z) (expt grazing 4.0)))
         (specular (* gloss (expt (max (dot normal half) 0.0) power)))
         ;; The hemisphere is the sky's own colour, but the fill and the
         ;; rim are not: a wall turned from the sun is lit by the whole
         ;; landscape bouncing, not by a second patch of zenith, and a fill
         ;; taken at full saturation turns every shaded stone navy.
         (fill-colour (mix sky (vec3 1.0 1.0 1.0) 0.55))
         (light (+ (* radiance key)
                   (+ (* sky (* ambient sky-weight))
                      (+ (* fill-colour (+ fill rim))
                         (* ground (* ambient ground-weight))))))
         ;; The hemisphere about the mirror direction: sky above, bounce
         ;; below, brightened a little at grazing angles as a mirror is.
         (bounce (- (* normal (* 2.0 (dot normal view))) view))
         (mirror-up (clamp (+ 0.5 (* 0.55 (swizzle bounce :z))) 0.0 1.0))
         (environment (mix (* ground 0.8) sky mirror-up))
         (reflected (* (* environment tint)
                       (* metallic (* occlusion (+ 1.0 (* 0.8 grazing))))))
         (lit (+ (+ (* base (* light (- 1.0 (* 0.85 metallic)))) reflected)
                 (* (* radiance tint)
                    (* specular (* shade (max facing 0.0))))))
         (exposed (paper-tonemap (* lit exposure)))
         (fog-far (swizzle sky-vector :w))
         (fog (smoothstep (* 0.55 fog-far) fog-far distance)))
    (mix exposed sky fog)))

;;; ------------------------------------------------------------------------
;;; The stock fragment stage
;;;
;;; Chamfered geometry, field light.  The chamfer band is found the way the
;;; paper material finds its glint: not from the face's UV border, which two
;;; coplanar cells also share, but from the facet's own departure from the
;;; face it cuts, which is exactly zero wherever the surface continues flat.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun stock-fragment-shader-definition ()
    "The stock fragment shader, spliced around one wide field sample."
    `(define-shader stock-fragment-shader
         (:stage :fragment
          :inputs ((normal :vec3 :location 0)
                   (world :vec3 :location 1)
                   (uv :vec2 :location 2)
                   (face-normal :vec3 :location 3)
                   (stock :float :location 4)
                   ;; The rest position: where this fragment is in the
                   ;; lattice, whatever the lattice has been bent into.
                   ;; Everything looked up in the world -- the occupancy
                   ;; field, the grain, the cell's own tone, the shadow --
                   ;; is looked up there, and only the view is taken from
                   ;; the bent position.
                   (rest :vec3 :location 5)
                   (bent-normal :vec3 :location 6))
          :outputs ((color :vec4 :location 0))
          :resources ((frame :uniform-block :binding ,+frame-binding+
                             :members ,*frame-uniform-members*)
                      (cells :storage-buffer :binding ,+cells-binding+
                             :element :uint)
                      (stocks :storage-buffer :binding ,+stocks-binding+
                              :element :vec4)))
       (let* ((period-x (swizzle domain-vector :x))
              (period-y (swizzle domain-vector :y))
              (softness (max (swizzle domain-vector :w) 0.0005))
              ;; Two facets, one in each space.  The rest facet says what
              ;; the geometry is -- flat wall or planed arris -- and cannot
              ;; be fooled by a bent lattice; the bent facet is what the
              ;; light actually falls on.
              (rest-facet (normalize (cross-product (derivative-x rest)
                                                    (derivative-y rest))))
              (face (normalize face-normal))
              (oriented-rest (if (< (dot rest-facet face) 0.0)
                                 (- rest-facet) rest-facet))
              (bent-facet (normalize (cross-product (derivative-x world)
                                                    (derivative-y world))))
              (bent (normalize bent-normal))
              (oriented (if (< (dot bent-facet bent) 0.0)
                            (- bent-facet) bent-facet))
              (u (swizzle uv :x))
              (v (swizzle uv :y))
              (inset (min (min u (- 1.0 u)) (min v (- 1.0 v))))
              ;; The arris band is measured against the band the grid
              ;; reserves, not against what each site took of it: the UV
              ;; the fragment interpolates is the parameter /before/ the
              ;; shared points moved, and that runs from nothing to the
              ;; reserved radius whatever the site did with it.
              (width (swizzle domain-vector :z))
              (arris (- inset width))
              (sanding (smoothstep (- softness) softness arris))
              (sanded (normalize (mix oriented bent sanding)))
              (sanded-rest (normalize (mix oriented-rest face sanding)))
              ;; What is really planed: the tilt away from the face, zero on
              ;; a flat wall however its cells are divided, and measured in
              ;; the lattice so that a bend does not read as a chamfer.
              (tilt (- 1.0 (abs (dot oriented-rest face))))
              (planed (smoothstep 0.015 0.30 tilt))
              ;; The stock the solid behind this face is cut from: the
              ;; site's own four bits, and its nine lanes of the table.
              ;; The value is one number over the whole face, so however
              ;; the rasterizer interpolates it, it arrives unchanged.
              (slot (* (uint (+ stock 0.25)) (uint ,(float +stock-lanes+))))
              (top-lane (buffer-element stocks slot))
              (side-lane (buffer-element stocks (+ slot (uint 1.0))))
              (bottom-lane (buffer-element stocks (+ slot (uint 2.0))))
              (finish-lane (buffer-element stocks (+ slot (uint 3.0))))
              (grain-lane (buffer-element stocks (+ slot (uint 4.0))))
              (figure-lane (buffer-element stocks (+ slot (uint 5.0))))
              (mottle-lane (buffer-element stocks (+ slot (uint 6.0))))
              (patina-lane (buffer-element stocks (+ slot (uint 7.0))))
              (gloss (swizzle finish-lane :x))
              (power (swizzle finish-lane :y))
              (metallic (swizzle finish-lane :z))
              (lift (swizzle finish-lane :w))
              (axis (swizzle grain-lane :xyz))
              (rings (swizzle grain-lane :w))
              (ring-contrast (swizzle figure-lane :x))
              (wander (swizzle figure-lane :y))
              (fibre-contrast (swizzle figure-lane :z))
              (drift-strength (swizzle figure-lane :w))
              (mottle-scale (swizzle mottle-lane :x))
              (mottle-contrast (swizzle mottle-lane :y))
              (wear (swizzle mottle-lane :z))
              (patina (swizzle mottle-lane :w))
              (patina-colour (swizzle patina-lane :xyz))
              (rim (swizzle patina-lane :w))
              (upness (swizzle face :z))
              (albedo (if (> upness 0.5)
                          (swizzle top-lane :xyz)
                          (if (< upness -0.5)
                              (swizzle bottom-lane :xyz)
                              (swizzle side-lane :xyz))))
              ;; The albedo lanes have a spare fourth component each; the
              ;; first carries how far apart the pith lines run, which is
              ;; the difference between a plank and a whole beam.
              (spacing (max (swizzle top-lane :w) 0.25))
              (grain (stock-grain rest axis spacing rings wander
                                  ring-contrast fibre-contrast))
              (mottle (stock-mottle rest mottle-scale mottle-contrast))
              ;; The second spare component says how many courses of brick
              ;; go to the cell; zero, and the wall is not coursed at all.
              (courses (swizzle side-lane :w))
              (bond (if (> courses 0.0)
                        (stock-courses rest face courses 0.16)
                        1.0))
              ;; The cell behind this face, and two hashes of it: a wall of
              ;; cells should not read as one painted surface.
              (cell (floor (- rest (* oriented-rest 0.25))))
              (patch (- (paper-noise (* cell 0.21)) 0.5))
              (jitter (- (paper-hash cell) 0.5))
              (drift (+ 1.0 (* drift-strength
                               (+ (* 1.35 patch) (* 0.45 jitter)))))
              ;; Weathering: under a half-cell tent a ridge holds less than
              ;; half the solid around it and a hollow more.  Ridges wear
              ;; pale and hollows keep the dirt -- and, for a metal, the
              ;; patina that a handled edge never gets to grow.
              ;; A third-cell tent, not a half-cell one: the band must hug
              ;; the arris, or the wear is a soft gradient down the whole
              ;; face and the crispness the chamfer was cut for is gone.
              ,@(occupancy-field-bindings 'wide 'rest 0.32)
              (relief (- (swizzle wide :w) 0.5))
              (ridge (smoothstep 0.02 0.34 (- relief)))
              (hollow (smoothstep 0.02 0.30 relief))
              (worn (+ 1.0 (* wear (- (* 0.55 ridge) (* 0.95 hollow)))))
              (tone (* albedo (* (* grain (* mottle bond)) (* drift worn))))
              (aged (mix tone patina-colour (* patina hollow)))
              ;; The planed facet: paler on wood, where the cut crosses the
              ;; fibre; brighter on metal, where handling polishes it.
              (stock (* aged (mix 1.0 lift planed)))
              ;; The light lives in the lattice, not in the bent world:
              ;; the shadow ray is walked through the rest occupancy along
              ;; the sun's own direction, so shadows bend with the geometry
              ;; rather than being cast across it.
              (lost (field-shadow rest (swizzle sun-vector :xyz)
                                  ,*field-shadow-steps*
                                  ,*field-shadow-reach*))
              (shade (mix 1.0 (- 1.0 lost) (swizzle occlusion-vector :y)))
              (crowding (field-occlusion rest sanded-rest
                                         ,*field-occlusion-steps*
                                         ,*field-occlusion-reach*))
              (open (- 1.0 (* (swizzle occlusion-vector :x) crowding)))
              (tint (mix (vec3 1.0 1.0 1.0) stock metallic))
              ;; An arris is the one line on a made thing that is always
              ;; polished, whatever the rest of the surface is doing.
              (finish (vec4 (* gloss (mix 1.0 2.2 planed)) power rim metallic))
              (final (stock-lighting stock sanded world open shade
                                     finish tint camera-vector sun-vector
                                     sun-colour-vector fill-vector
                                     sky-vector ground-vector))
              ;; Alpha carries distance for the focus pass, as paper's does.
              (delta (- world (swizzle camera-vector :xyz)))
              (range (max (swizzle lens-vector :x) 1.0))
              (reach (clamp (/ (sqrt (dot delta delta)) (* 2.0 range))
                            0.0 1.0))
              (depth (if (> (swizzle lens-vector :y) 0.0) reach 1.0)))
         (set-output color (vec4 final depth))))))

#.(stock-fragment-shader-definition)

;;; ------------------------------------------------------------------------
;;; The materials themselves

(in-package #:luft.render)

(defclass material ()
  ((name
    :initarg :name
    :reader material-name)
   (top
    :initarg :top :initform nil :reader material-top
    :documentation "The albedo of an upward face, or NIL for *TOP-COLOR*.")
   (side
    :initarg :side :initform nil :reader material-side
    :documentation "The albedo of a sideways face, or NIL for *SIDE-COLOR*.")
   (bottom
    :initarg :bottom :initform nil :reader material-bottom
    :documentation "The albedo of a downward face, or NIL for *BOTTOM-COLOR*.")
   (gloss
    :initarg :gloss :initform 0.0 :reader material-gloss
    :documentation "How brightly the surface returns the sun's reflection.")
   (polish
    :initarg :polish :initform 40.0 :reader material-polish
    :documentation "The specular power: how tight that reflection is.")
   (metallic
    :initarg :metallic :initform 0.0 :reader material-metallic
    :documentation "One when the highlight takes the colour of the metal.")
   (lift
    :initarg :lift :initform 1.0 :reader material-lift
    :documentation "What the planed chamfer multiplies the tone by.

Below one for a wood whose end grain drinks the finish, above one for a
metal or a stone whose arris is the first thing a hand or the weather
reaches.")
   (grain-axis
    :initarg :grain-axis :initform '(0.0 0.0 1.0) :reader material-grain-axis
    :documentation "The direction the grain runs, as a list of three floats.")
   (grit
    :initarg :grit :initform 0.0 :reader material-grit
    :documentation "How far the lattice wanders where this stock is.

Zero leaves the lattice square.  A rock has grit and a planed board has
none, so the same noise that turns granite into rubble leaves the timber
standing in it dead straight.  The value is smoothed over the cells around
a point before it is used, which is what keeps it a function of position
and so keeps the surface closed.")
   (chamfer
    :initarg :chamfer :initform 1.0 :reader material-chamfer
    :documentation "What the :STOCK chamfer rule multiplies a crease by.

A stock that takes a clean arris -- a planed board, a cast rail -- asks for
more than one; a stock that crumbles at an edge asks for less.  The rule
takes the least any solid cell of a star asks for, so the sharper stock
always wins.")
   (courses
    :initarg :courses :initform 0.0 :reader material-courses
    :documentation "Courses of brick to the cell, or zero for uncoursed.")
   (spacing
    :initarg :spacing :initform 2.7 :reader material-spacing
    :documentation "Cells between pith lines across the grain.

Small for a world of planks laminated together, large for one cut from
whole trees; it decides how often a face meets a ring's centre.")
   (rings
    :initarg :rings :initform 0.0 :reader material-rings
    :documentation "Growth rings to the cell across the grain.")
   (ring-contrast
    :initarg :ring-contrast :initform 0.0 :reader material-ring-contrast)
   (wander
    :initarg :wander :initform 0.0 :reader material-wander
    :documentation "How many cells the turbulence warps the rings by.")
   (fibre
    :initarg :fibre :initform 0.0 :reader material-fibre
    :documentation "The contrast of the fine fibre between the rings.")
   (drift
    :initarg :drift :initform 0.06 :reader material-drift
    :documentation "How far one cell's tone drifts from its neighbour's.")
   (mottle-scale
    :initarg :mottle-scale :initform 0.6 :reader material-mottle-scale)
   (mottle
    :initarg :mottle :initform 0.0 :reader material-mottle
    :documentation "The contrast of the patchiness of a stone.")
   (wear
    :initarg :wear :initform 0.0 :reader material-wear
    :documentation "How strongly ridges lighten and hollows darken.")
   (patina
    :initarg :patina :initform 0.0 :reader material-patina
    :documentation "How strongly a sheltered hollow ages toward PATINA-COLOR.")
   (patina-color
    :initarg :patina-color :initform '(0.10 0.20 0.16)
    :reader material-patina-color)
   (rim
    :initarg :rim :initform 0.0 :reader material-rim
    :documentation "How much sky a grazing view adds at the silhouette."))
  (:documentation "The stock a world is cut from: albedo, finish, figure.

#ADEAKZ"))

(defvar *material-table* (make-hash-table :test 'eq)
  "Every defined material, by name.")

(defmacro define-material (name &body initargs)
  "Define or redefine the material called NAME from INITARGS.

The slots are plain numbers and colours; see the MATERIAL class.  A colour
left out takes the world's own *TOP-COLOR*, *SIDE-COLOR*, or *BOTTOM-COLOR*,
so a material may speak only of its finish and its figure."
  `(setf (gethash ,name *material-table*)
         (make-instance 'material :name ,name ,@initargs)))

(defun find-material (name)
  "The material called NAME, or an error naming what there is."
  (or (gethash name *material-table*)
      (error "No material ~S; there is ~{~S~^, ~}." name (material-names))))

(defun material-names ()
  "Every defined material's name, in alphabetical order."
  (sort (loop for name being the hash-keys of *material-table* collect name)
        #'string< :key #'symbol-name))

(defparameter *material* :turf
  "The material a scene with no stocks of its own is drawn wholly in.

A scene built from a WORLD carries a palette and this is only its
fallback; a scene made straight from a chain has no palette, and then this
is the whole of what it is cut from.  #PWMCOL")

;;; A palette.  The colours are linear, which is to say roughly the sRGB
;;; value squared: 0.25 here is a mid brown on the screen, not a dark one.

(define-material :turf
  ;; Grass over soil.  The side colour matters more than the top one: on a
  ;; landscape whose turf is one cell deep, every ledge shows its own edge,
  ;; and a bright earth there turns a green hillside into a beige one.
  :top '(0.108 0.205 0.058) :side '(0.130 0.092 0.052)
  :bottom '(0.062 0.045 0.026)
  :grit 0.75
  :chamfer 0.2
  :gloss 0.02 :polish 12.0 :lift 1.06
  :mottle-scale 0.55 :mottle 0.34 :wear 0.35 :drift 0.10 :rim 0.05)

;;; Wood.  The albedos are low because wood is dark: a mid-brown board is
;;; about a fifth of the light back, and anything written at the value the
;;; eye reads off a photograph comes out of the tonemap as terracotta.

(define-material :sapele
  ;; The board in the photograph: a warm red-brown ribbon-striped mahogany,
  ;; oiled rather than lacquered, its chamfer paler than its face.
  :top '(0.115 0.040 0.020) :side '(0.104 0.035 0.017)
  :bottom '(0.058 0.020 0.010)
  :grit 0.06
  :chamfer 1.0
  :gloss 0.55 :polish 90.0 :lift 1.30
  :grain-axis '(0.0 1.0 0.0) :spacing 3.4 :rings 14.0
  :ring-contrast 0.30 :wander 0.08
  :fibre 0.11 :drift 0.04 :mottle-scale 0.9 :mottle 0.09 :rim 0.04)

(define-material :oak
  ;; Paler, cooler, coarser: a floor rather than a table top.
  :top '(0.185 0.118 0.058) :side '(0.168 0.104 0.050)
  :bottom '(0.088 0.055 0.026)
  :grit 0.1
  :chamfer 1.0
  :gloss 0.22 :polish 45.0 :lift 1.22
  :grain-axis '(1.0 0.0 0.0) :spacing 3.9 :rings 10.0
  :ring-contrast 0.23 :wander 0.12
  :fibre 0.14 :drift 0.05 :mottle-scale 0.8 :mottle 0.08 :rim 0.05)

(define-material :walnut
  ;; The dark stripe of the butcher block, nearly black in the latewood.
  :top '(0.052 0.026 0.015) :side '(0.046 0.023 0.013)
  :bottom '(0.026 0.013 0.007)
  :grit 0.07
  :chamfer 1.0
  :gloss 0.42 :polish 70.0 :lift 1.50
  :grain-axis '(0.0 0.0 1.0) :spacing 3.1 :rings 12.0
  :ring-contrast 0.32 :wander 0.10
  :fibre 0.13 :drift 0.04 :mottle-scale 0.9 :mottle 0.09 :rim 0.04)

;;; Stone.  No grain; a mottle at the scale of a cell or two, wear at every
;;; arris, and dirt kept in the hollows.

(define-material :limestone
  ;; A warm pale building stone, matte, weathered at every edge.
  :top '(0.322 0.280 0.203) :side '(0.290 0.250 0.180)
  :bottom '(0.146 0.126 0.090)
  :grit 0.45
  :chamfer 0.4
  :gloss 0.04 :polish 16.0 :lift 1.10
  :mottle-scale 1.3 :mottle 0.22 :wear 0.55 :drift 0.07
  :patina 0.22 :patina-color '(0.085 0.095 0.068) :rim 0.10)

(define-material :granite
  ;; Cool, dense, speckled: the cliff of the inspiration photographs, whose
  ;; facets read blue in the shade and near-white in the sun.
  :top '(0.178 0.178 0.184) :side '(0.152 0.152 0.158)
  :bottom '(0.074 0.074 0.078)
  :grit 1.0
  :chamfer 0.15
  :gloss 0.14 :polish 30.0 :lift 1.26
  :mottle-scale 2.2 :mottle 0.42 :wear 0.48 :drift 0.17 :rim 0.11)

(define-material :slate
  ;; Nearly black, faintly blue, split in courses: a roof or a paving.
  :top '(0.058 0.063 0.076) :side '(0.048 0.052 0.063)
  :bottom '(0.024 0.026 0.032)
  ;; No rings: bedding planes are not growth rings, and a wood figure laid
  ;; on a slate floor comes out as a target painted on the paving.
  :grit 0.3
  :chamfer 0.6
  :gloss 0.30 :polish 55.0 :lift 1.40
  :mottle-scale 1.8 :mottle 0.20 :wear 0.30 :drift 0.06 :rim 0.20)

(define-material :brick
  ;; Fired clay laid in a stretcher bond: the one material here whose
  ;; figure is not a grain or a mottle but a way of putting it together.
  :top '(0.140 0.055 0.034) :side '(0.126 0.048 0.030)
  :bottom '(0.064 0.024 0.015)
  :courses 3.0
  :grit 0.18
  :chamfer 0.25
  :gloss 0.05 :polish 18.0 :lift 1.14
  :mottle-scale 3.2 :mottle 0.22 :wear 0.35 :drift 0.06 :rim 0.07)

(define-material :marble
  ;; The grain machinery with the rings almost turned off and the wander
  ;; turned up is veining: a pale stone with a dark seam wandering through.
  :top '(0.480 0.470 0.445) :side '(0.440 0.430 0.408)
  :bottom '(0.215 0.210 0.200)
  :grit 0.05
  :chamfer 0.8
  :gloss 0.75 :polish 150.0 :lift 1.20
  :grain-axis '(0.0 0.0 1.0) :spacing 9.0 :rings 2.0
  :ring-contrast 0.26 :wander 2.8 :fibre 0.04
  :mottle-scale 1.4 :mottle 0.10 :wear 0.15 :drift 0.04 :rim 0.14)

(define-material :terracotta
  ;; Fired earth: warm, slightly chalky, lighter where it has been rubbed.
  :top '(0.185 0.068 0.036) :side '(0.166 0.060 0.032)
  :bottom '(0.086 0.031 0.016)
  :grit 0.35
  :chamfer 0.35
  :gloss 0.06 :polish 20.0 :lift 1.18
  :mottle-scale 1.1 :mottle 0.20 :wear 0.40 :drift 0.08 :rim 0.08)

(define-material :bronze
  ;; A cast metal: dark and warm where it is polished, verdigris where the
  ;; weather is allowed to sit.  The highlight takes the colour of the
  ;; metal, which is what makes a metal look like one.
  :top '(0.098 0.058 0.022) :side '(0.086 0.051 0.019)
  :bottom '(0.045 0.026 0.010)
  :grit 0.04
  :chamfer 1.0
  :gloss 1.80 :polish 120.0 :metallic 1.0 :lift 1.90
  :mottle-scale 1.6 :mottle 0.14 :wear 0.50
  :patina 0.70 :patina-color '(0.042 0.098 0.076) :rim 0.22)

(define-material :conifer
  ;; Not a stock at all, strictly: the dark blue-green mass of a fir, dense
  ;; enough that the chamfer on every step of its skirt is the only thing
  ;; that says where one branch layer ends and the next begins.
  :top '(0.036 0.072 0.042) :side '(0.026 0.054 0.032)
  :bottom '(0.013 0.026 0.016)
  :grit 0.85
  :chamfer 0.3
  :gloss 0.06 :polish 22.0 :lift 1.14
  :mottle-scale 2.8 :mottle 0.36 :wear 0.0 :drift 0.16 :rim 0.08)

(define-material :leaf
  ;; A broadleaf crown: yellower, lighter, and more broken up than a fir.
  :top '(0.078 0.118 0.038) :side '(0.060 0.092 0.030)
  :bottom '(0.030 0.046 0.015)
  :grit 1.0
  :chamfer 0.3
  :gloss 0.05 :polish 18.0 :lift 1.12
  :mottle-scale 2.0 :mottle 0.46 :wear 0.0 :drift 0.20 :rim 0.08)

(define-material :plaster
  ;; Limewash on render: the flattest, quietest thing in the palette, and
  ;; the one that shows what the light alone is doing.
  :top '(0.400 0.382 0.342) :side '(0.372 0.354 0.316)
  :bottom '(0.186 0.177 0.158)
  :grit 0.15
  :chamfer 0.5
  :gloss 0.03 :polish 14.0 :lift 1.05
  :mottle-scale 0.7 :mottle 0.10 :wear 0.25 :drift 0.05 :rim 0.06)

(defun material-lanes (material)
  "The five stock lanes of MATERIAL, each a list of four floats.

The order is the frame block's: finish, grain, figure, mottle, patina."
  (flet ((f (x) (coerce x 'single-float)))
    (let ((axis (material-grain-axis material)))
      (list (list (f (material-gloss material))
                  (f (material-polish material))
                  (f (material-metallic material))
                  (f (material-lift material)))
            (list (f (first axis)) (f (second axis)) (f (third axis))
                  (f (material-rings material)))
            (list (f (material-ring-contrast material))
                  (f (material-wander material))
                  (f (material-fibre material))
                  (f (material-drift material)))
            (list (f (material-mottle-scale material))
                  (f (material-mottle material))
                  (f (material-wear material))
                  (f (material-patina material)))
            (append (mapcar #'f (material-patina-color material))
                    (list (f (material-rim material))))))))
