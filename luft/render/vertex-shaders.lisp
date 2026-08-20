;;; The surface drawn by ordinary vertex shaders pulling packed sites.
;;;
;;; The host uploads the surface chain as one (unsigned-byte 64) site per face
;;; and builds no vertex buffer.  A draw of K vertices per face is issued, and
;;; each invocation divides its built-in index by K to find its site and takes
;;; the remainder to learn which corner of which triangle it is.  Fragment
;;; stages receive NORMAL, WORLD, UV, and for chamfers FACE-NORMAL.  #AGVXGM
;;; #VAABY9 #NV25VM
;;;
;;; An absent or back-facing face collapses every one of its vertices onto the
;;; anchor, where the rasterizer drops the degenerate triangle for free.

(in-package #:luft.render.shaders)

;;; ------------------------------------------------------------------------
;;; From a vertex index to a quad corner
;;;
;;; Six vertices draw one quad as two triangles around corner 0:
;;;
;;;     (0 1 2) (0 2 3)        positive polarity, counter-clockwise outward
;;;     (0 2 1) (0 3 2)        negative polarity, the same loop reversed
;;;
;;; Corners run anchor, +A, +A+B, +B, so corner C spans A when C is 1 or 2
;;; and spans B when C is 2 or 3.  Polarity reverses the second and third
;;; primitive vertices so either side remains counter-clockwise from outside.

(define-shader-function quad-corner (vertex-in-quad negative-p)
  "The corner (0..3) that vertex VERTEX-IN-QUAD (0..5) of a quad draws,
with the loop reversed when NEGATIVE-P."
  (let* ((triangle (float (/ vertex-in-quad (uint 3.0))))
         (slot (float (mod vertex-in-quad (uint 3.0)))))
    (if (< slot 0.5)
        0.0
        (if negative-p
            (- (+ 3.0 triangle) slot)
            (+ slot triangle)))))

(define-shader-function corner-spans-a (corner)
  "One when CORNER lies along the face's first spanning axis."
  (if (> corner 0.5) (if (< corner 2.5) 1.0 0.0) 0.0))

(define-shader-function corner-spans-b (corner)
  "One when CORNER lies along the face's second spanning axis."
  (if (> corner 1.5) 1.0 0.0))

;;; ------------------------------------------------------------------------
;;; Flat faces: six vertices a site

(defgeneric surface-vertices-per-face (style)
  (:documentation
   "How many vertices the vertex-pulling shader of STYLE draws per site.
The renderer multiplies this by the site count to size its draw."))

(defmethod surface-vertices-per-face ((style (eql :flat))) 6)
(defmethod surface-vertices-per-face ((style (eql :soft))) 6)
(defmethod surface-vertices-per-face ((style (eql :ink))) 6)

(define-shader surface-vertex-shader
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :resources ((frame :uniform-block :binding #.+frame-binding+
                        :members #.*frame-uniform-members*)
                 (sites :storage-buffer :binding #.+sites-binding+
                        :element :uint64))
     :outputs ((position :vec4 :built-in :position)
               (normal :vec3 :location 0)
               (world :vec3 :location 1)
               (uv :vec2 :location 2)))
  (let* ((site (buffer-element sites (/ vertex-index (uint 6.0))))
         (vertex-in-quad (mod vertex-index (uint 6.0)))
         ;; The signed site: XYZ extent in the low three bits, polarity in the
         ;; fourth, then X, Y, Z anchors.
         (extent (uint (ldb (byte luft:+extent-bits+ 0) site)))
         (anchor
           (vec3 (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                          luft:+x-shift+)
                                    site)))
                 (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                          luft:+y-shift+)
                                    site)))
                 (float (uint (ldb (byte luft:+vertical-coordinate-bits+
                                          luft:+z-shift+)
                                    site)))))
         (negative-p (= (uint (ldb (byte 1 luft:+site-sign-bit+) site))
                        (uint 1.0)))
         (present-p (> extent (uint 0.0)))
         (yz-face-p (= extent (uint luft:+yz-face-extent+)))
         (xz-face-p (= extent (uint luft:+xz-face-extent+)))
         (xy-face-p (= extent (uint luft:+xy-face-extent+)))
         ;; The two spanning axes in canonical X<Y<Z order, and their cross
         ;; product: the face's canonical orientation, which is also the
         ;; orientation convention of the boundary operator.
         (edge-a (if yz-face-p (vec3 0.0 1.0 0.0) (vec3 1.0 0.0 0.0)))
         (edge-b (if xy-face-p (vec3 0.0 1.0 0.0) (vec3 0.0 0.0 1.0)))
         (canonical (if yz-face-p
                        (vec3 1.0 0.0 0.0)
                        (if xz-face-p (vec3 0.0 -1.0 0.0) (vec3 0.0 0.0 1.0))))
         (orientation (if negative-p -1.0 1.0))
         (normal (* canonical orientation))
         (camera (swizzle camera-vector :xyz))
         (facing-p (> (dot normal (- camera anchor)) 0.0))
         ;; Absent or back-facing faces collapse onto their anchor.
         (scale (if present-p (if facing-p 1.0 0.0) 0.0))
         (corner (quad-corner vertex-in-quad negative-p))
         (a (corner-spans-a corner))
         (b (corner-spans-b corner))
         (point (+ anchor (* (+ (* edge-a a) (* edge-b b)) scale)))
         (clip (view-clip point camera
                          (swizzle right-vector :xyz)
                          (swizzle up-vector :xyz)
                          (swizzle forward-vector :xyz)
                          projection-vector)))
    (set-output position (jitter-clip clip (swizzle jitter-vector :xy)))
    (set-output normal normal)
    (set-output world point)
    (set-output uv (vec2 a b))))

;;; Shaped faces: a point grid per site, one vertex per grid point
;;;
;;; Each invocation generates one grid point and needs only the star of its
;;; own nearest corner: six cells
;;; beside the face's solid and air cells, across U, across V, and across
;;; both, toward that corner's side.  The same site rules then place it, and
;;; because every face incident to a site gathers the same star, the shaped
;;; faces still meet exactly.  The grid quads split along the diagonal toward
;;; the nearest corner, so a corner square is two flat chamfer pieces.
;;;
;;; One definition serves both rules.  The chamfer is one ring: a 4x4 grid,
;;; nine quads, fifty-four vertices a site, and only shared points move, by
;;; half the width toward their star's minority.  The rounding is two rings:
;;; a 6x6 grid, twenty-five quads, a hundred and fifty vertices a site; the
;;; shared points project onto their site's sphere or cylinder, the ring
;;; points onto the nearest crease's cylinder blended toward the corner's
;;; sphere, selected from the vertex's own grid position.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun grid-vertices-per-face (rings)
    "Six vertices for each quad of the grid RINGS rings make."
    (let ((side (let ((*bevel-rings* rings)) (bevel-grid-side))))
      (* 6 (1- side) (1- side))))

  (defun grid-parameter-form (index rings &optional (radius 'radius))
    "A form giving the in-plane parameter of grid line INDEX (a float
0..SIDE-1 at run time): 0, then RADIUS in RINGS steps, mirrored from the
far side, then 1.

RADIUS names the inset, which is the shader's own uniform width unless the
caller has a per-site one to hand."
    (let ((side (let ((*bevel-rings* rings)) (bevel-grid-side))))
      (labels ((chain (k)
                 (if (= k (1- side))
                     1.0
                     `(if (< ,index ,(+ k 0.5))
                          ,(subst radius 'radius
                                  (let ((*bevel-rings* rings))
                                    (bevel-grid-parameter k)))
                          ,(chain (1+ k))))))
        (chain 0))))

  (defun site-width-bindings (widths)
    "Bindings for the widths the two boundary sites of this vertex ask for,
and for the width of whichever of them this vertex is nearer.

With :UNIFORM these are the shader's own radius, which is what every style
before per-site chamfering did.  With :SITE they come from
[[lattice.lisp]]: the star's own shape or the stocks of its solid cells,
chosen by the arris lane, and in either case a function of the star alone
so that the two faces along a crease cannot disagree."
    (ecase widths
      (:uniform '((u-radius radius)
                  (v-radius radius)))
      (:site
       `((arris-rule (swizzle arris-vector :x))
         (arris-convex (swizzle arris-vector :y))
         (arris-concave (swizzle arris-vector :z))
         ;; The stocks of the star's cells: the face's own comes free on
         ;; the site, and the six around the nearest corner are read.
         (own-chamfer ,(stock-chamfer-form 'stock-slot))
         ,@(cell-chamfer-bindings 'stock-cu '(+ solid-anchor u) 'solid-cu)
         ,@(cell-chamfer-bindings 'stock-eu '(+ air-anchor u) 'solid-eu)
         ,@(cell-chamfer-bindings 'stock-cv '(+ solid-anchor v) 'solid-cv)
         ,@(cell-chamfer-bindings 'stock-ev '(+ air-anchor v) 'solid-ev)
         ,@(cell-chamfer-bindings 'stock-cuv '(+ solid-anchor (+ u v))
                                  'solid-cuv)
         ,@(cell-chamfer-bindings 'stock-euv '(+ air-anchor (+ u v))
                                  'solid-euv)
         ;; An edge star is four cells and a vertex star eight; each width
         ;; is the least any solid one of them asks for.
         (u-stock (site-stock-width radius own-chamfer
                                    stock-cu-chamfer stock-eu-chamfer
                                    99.0 99.0 99.0 99.0))
         (v-stock (site-stock-width radius own-chamfer
                                    stock-cv-chamfer stock-ev-chamfer
                                    99.0 99.0 99.0 99.0))
         (corner-stock (site-stock-width radius own-chamfer
                                         stock-cu-chamfer stock-eu-chamfer
                                         stock-cv-chamfer stock-ev-chamfer
                                         stock-cuv-chamfer
                                         stock-euv-chamfer))
         ;; The relief of each site, counted off its own star so that the
         ;; two faces along a crease cannot classify it differently.
         (u-relief (edge-relief solid-cu solid-eu))
         (v-relief (edge-relief solid-cv solid-ev))
         (corner-relief (vertex-relief solid-cu solid-eu solid-cv solid-ev
                                       solid-cuv solid-euv))
         ;; Never past the reserved band, whatever a rule asks for.
         (u-radius (min radius
                        (if (< arris-rule 0.5)
                            radius
                            (if (< arris-rule 1.5)
                                (site-relief-width u-relief radius
                                                   arris-convex arris-concave)
                                u-stock))))
         (v-radius (min radius
                        (if (< arris-rule 0.5)
                            radius
                            (if (< arris-rule 1.5)
                                (site-relief-width v-relief radius
                                                   arris-convex arris-concave)
                                v-stock))))
         (corner-radius (min radius
                             (if (< arris-rule 0.5)
                                 radius
                                 (if (< arris-rule 1.5)
                                     (site-relief-width corner-relief radius
                                                        arris-convex
                                                        arris-concave)
                                     corner-stock))))))))

  (defun grid-vertex-shader-definition (name rings rule
                                        &key (widths :uniform) (deform nil))
    "A vertex shader definition named NAME: the grid of RINGS rings, each
vertex placed by RULE, :CHAMFER or :ROUND, from its nearest corner's star.

WIDTHS is :UNIFORM for one chamfer width over the whole world or :SITE for
one per site.  With DEFORM the shaped point is passed through the lattice
map of [[lattice.lisp]] before it is projected, and the shader also carries
the undeformed rest position out to the fragment stage, which needs it for
everything it looks up in the lattice."
    (let* ((side (let ((*bevel-rings* rings)) (bevel-grid-side)))
           (quads (1- side))
           (last (float (1- side)))
           (half (float (floor side 2)))
           (vertices (grid-vertices-per-face rings)))
      `(define-shader ,name
           (:stage :vertex
            :inputs ((vertex-index :uint :built-in :vertex-index))
            :resources ((frame :uniform-block :binding ,+frame-binding+
                               :members ,*frame-uniform-members*)
                        (sites :storage-buffer :binding ,+sites-binding+
                               :element :uint64)
                        (cells :storage-buffer :binding ,+cells-binding+
                               :element :uint)
                        ,@(when (eq widths :site)
                            `((stocks :storage-buffer
                                      :binding ,+stocks-binding+
                                      :element :vec4)))
                        ,@(when (or (eq widths :site) (eq rule :clay))
                            `((slots :storage-buffer
                                     :binding ,+slots-binding+
                                     :element :uint))))
            :outputs ((position :vec4 :built-in :position)
                      (normal :vec3 :location 0)
                      (world :vec3 :location 1)
                      (uv :vec2 :location 2)
                      ,@(when (eq rule :chamfer)
                          '((face-normal :vec3 :location 3)
                            (stock :float :location 4)))
                      ,@(when deform
                          '((rest :vec3 :location 5)
                            (bent-normal :vec3 :location 6)))))
         (let* ((site (buffer-element
                       sites (/ vertex-index (uint ,(float vertices)))))
                ;; The four bits above the site: which stock the solid
                ;; behind this face is cut from.
                (stock-slot (float (uint (ldb (byte ,+site-stock-bits+
                                                    ,+site-stock-shift+)
                                              site))))
                (vertex-in-face (mod vertex-index (uint ,(float vertices))))
                ;; Which grid quad, and which vertex of its six.
                (quad (/ vertex-in-face (uint 6.0)))
                (vertex-in-quad (mod vertex-in-face (uint 6.0)))
                (quad-i (float (/ quad (uint ,(float quads)))))
                (quad-j (float (mod quad (uint ,(float quads)))))
                (extent (uint (ldb (byte luft:+extent-bits+ 0) site)))
                (anchor
                  (vec3 (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                                 luft:+x-shift+)
                                           site)))
                        (float (uint (ldb (byte luft:+horizontal-capacity-bits+
                                                 luft:+y-shift+)
                                           site)))
                        (float (uint (ldb (byte luft:+vertical-coordinate-bits+
                                                 luft:+z-shift+)
                                           site)))))
                (negative-p (= (uint (ldb (byte 1 luft:+site-sign-bit+) site))
                               (uint 1.0)))
                (present-p (> extent (uint 0.0)))
                (yz-face-p (= extent (uint luft:+yz-face-extent+)))
                (xz-face-p (= extent (uint luft:+xz-face-extent+)))
                (xy-face-p (= extent (uint luft:+xy-face-extent+)))
                (edge-a (if yz-face-p (vec3 0.0 1.0 0.0) (vec3 1.0 0.0 0.0)))
                (edge-b (if xy-face-p (vec3 0.0 1.0 0.0) (vec3 0.0 0.0 1.0)))
                (canonical (if yz-face-p
                               (vec3 1.0 0.0 0.0)
                               (if xz-face-p
                                   (vec3 0.0 -1.0 0.0)
                                   (vec3 0.0 0.0 1.0))))
                (orientation (if negative-p -1.0 1.0))
                (normal (* canonical orientation))
                (axis (abs canonical))
                (camera (swizzle camera-vector :xyz))
                ;; A shaped point's normal tilts well away from the face
                ;; normal, so only faces turned well past grazing can be
                ;; dropped with this deliberately relaxed facing test.
                (toward (- camera (+ anchor (* (+ edge-a edge-b) 0.5))))
                (toward-normal (dot normal toward))
                (facing-p (if (> toward-normal 0.0)
                              (> 1.0 0.0)
                              (< (* toward-normal toward-normal)
                                 (* 0.72 (dot toward toward)))))
                ;; The packed clay stocks claim whole faces for the clay
                ;; overlay: the clay rule draws exactly the claimed faces
                ;; and every other rule leaves them to it.  The lane is
                ;; (1+A) + 16(1+B); a slot matching either is claimed.
                (clay-lane (swizzle arris-vector :w))
                (clay-high (floor (/ clay-lane 16.0)))
                (clay-slot-a (- (- clay-lane (* clay-high 16.0)) 1.0))
                (clay-slot-b (- clay-high 1.0))
                (clay-claimed
                  (if (> clay-lane 0.5)
                      (if (< (abs (- stock-slot clay-slot-a)) 0.25)
                          1.0
                          (if (< (abs (- stock-slot clay-slot-b)) 0.25)
                              1.0 0.0))
                      0.0))
                ;; A bent lattice can turn a face toward the camera that
                ;; the rest lattice had turned away, so a deforming shader
                ;; keeps every present face and lets depth sort it out.
                (scale (* ,(if deform
                               '(if present-p
                                    (if (> (+ (abs (swizzle deform-vector
                                                            :y))
                                              (abs (swizzle deform-vector
                                                            :w)))
                                           0.0)
                                        1.0
                                        (if facing-p 1.0 0.0))
                                    0.0)
                               '(if present-p (if facing-p 1.0 0.0) 0.0))
                          ,(if (eq rule :clay)
                               'clay-claimed
                               '(- 1.0 clay-claimed))))
                (period-x (swizzle domain-vector :x))
                (period-y (swizzle domain-vector :y))
                ;; The in-plane band the grid reserves.  The clay rule's
                ;; knobs ride the material lanes' spares, so a hybrid
                ;; frame can carry another style's width here too.
                (radius ,(if (eq rule :clay)
                             '(swizzle top-vector :w)
                             '(swizzle domain-vector :z)))
                ;; The quad's diagonal points at the nearest corner of the
                ;; face: rotate the corner loop by one when it does not, so
                ;; that (p q r)(p r s) splits a b c d along b-d, not a-c.
                (toward-corner-p (if (< quad-i ,(- half 0.5))
                                     (< quad-j ,(- half 0.5))
                                     (> quad-j ,(- half 0.5))))
                (loop-corner (quad-corner vertex-in-quad negative-p))
                (corner (if toward-corner-p
                            loop-corner
                            (if (> loop-corner 2.5) 0.0 (+ loop-corner 1.0))))
                ;; The grid point this vertex is, 0..SIDE-1 along each
                ;; axis; the nearest corner's side along each; whether the
                ;; point lies on the face's boundary along each; and how
                ;; many ring steps it is from that corner along each.
                (grid-i (+ quad-i (corner-spans-a corner)))
                (grid-j (+ quad-j (corner-spans-b corner)))
                (su (if (< grid-i ,(- half 0.5)) -1.0 1.0))
                (sv (if (< grid-j ,(- half 0.5)) -1.0 1.0))
                (on-u-p (if (< grid-i 0.5) (> 1.0 0.0) (> grid-i ,(- last 0.5))))
                (on-v-p (if (< grid-j 0.5) (> 1.0 0.0) (> grid-j ,(- last 0.5))))
                (u-steps (min grid-i (- ,last grid-i)))
                (v-steps (min grid-j (- ,last grid-j)))
                (u-near-p (< u-steps ,(- rings 0.5)))
                (v-near-p (< v-steps ,(- rings 0.5)))
                (u (* edge-a su))
                (v (* edge-b sv))
                ;; The face's own solid cell and air cell anchors, and the
                ;; six cells of the nearest corner's star beside them.
                (positive-p (> (dot normal axis) 0.0))
                (solid-anchor (- anchor (* axis (if positive-p 1.0 0.0))))
                (air-anchor (- anchor (* axis (if positive-p 0.0 1.0))))
                ,@(cell-solid-bindings 'solid-cu '(+ solid-anchor u))
                ,@(cell-solid-bindings 'solid-eu '(+ air-anchor u))
                ,@(cell-solid-bindings 'solid-cv '(+ solid-anchor v))
                ,@(cell-solid-bindings 'solid-ev '(+ air-anchor v))
                ,@(cell-solid-bindings 'solid-cuv '(+ solid-anchor (+ u v)))
                ,@(cell-solid-bindings 'solid-euv '(+ air-anchor (+ u v)))
                ;; Site rules: the two edges meeting at the corner, and the
                ;; corner itself.  These come before the grid parameters
                ;; because a chamfer whose width differs from site to site
                ;; must set its inner grid line back by its own width, and
                ;; the width is a function of the star.
                (u-minority (edge-minority solid-cu solid-eu normal u))
                (v-minority (edge-minority solid-cv solid-ev normal v))
                (corner-minority
                  (vertex-minority normal u v
                                   solid-cu solid-eu solid-cv solid-ev
                                   solid-cuv solid-euv))
                ;; The in-plane parameters.  The inset is the shader's own
                ;; radius even where the sites ask for less, because a face's
                ;; boundary points along one edge are spaced by the /other/
                ;; edges' insets, and the neighbour across that edge spaces
                ;; them by different edges again: insets that vary tear the
                ;; seam even where the displacements agree.  So the radius
                ;; is the band the grid reserves, and a site may take up to
                ;; all of it and no more.
                ,@(site-width-bindings widths)
                (s ,(grid-parameter-form 'grid-i rings))
                (t* ,(grid-parameter-form 'grid-j rings))
                (flat (+ anchor (* edge-a s) (* edge-b t*)))
                ,@(ecase rule
                    (:clay
                     ;; The clay union: every point, boundary or inner,
                     ;; takes two Newton steps onto the zero set of the
                     ;; rounded-cell field (luft/render/clay.lisp), the
                     ;; gradient read by tetrahedron.
                     `((melt (swizzle side-vector :w))
                       ,@(clay-newton-bindings 'clay-a 'flat
                                               'radius 'melt)
                       ,@(clay-newton-bindings 'clay-b 'clay-a-point
                                               'radius 'melt)
                       (shaped clay-b-point)
                       (shaped-normal (if (> (dot clay-b-gradient
                                                  clay-b-gradient)
                                             0.0000001)
                                          (normalize clay-b-gradient)
                                          normal))))
                    (:field
                     ;; The level set: every point, boundary or inner, takes
                     ;; three Newton steps onto the half-level set of the
                     ;; tent-smoothed occupancy (luft/render/field.lisp) and
                     ;; its normal from the gradient there.
                     `((vertical-radius (swizzle domain-vector :w))
                       (reach (max radius vertical-radius))
                       ,@(occupancy-field-bindings 'field-0 'flat 'radius
                                                   'vertical-radius)
                       (point-1 (field-step flat field-0 reach))
                       ,@(occupancy-field-bindings 'field-1 'point-1 'radius
                                                   'vertical-radius)
                       (point-2 (field-step point-1 field-1 reach))
                       ,@(occupancy-field-bindings 'field-2 'point-2 'radius
                                                   'vertical-radius)
                       (shaped (field-step point-2 field-2 reach))
                       (shaped-normal (field-normal field-2 normal))))
                    (:chamfer
                     ;; Woodworking: shared points move to the meeting of
                     ;; flat facets, and nothing else moves.  Each moves by
                     ;; the width its own site asked for, which is why the
                     ;; grid line behind it was set back by the same.
                     `((minority (if on-u-p
                                     (if on-v-p corner-minority u-minority)
                                     (if on-v-p v-minority (vec3 0.0 0.0 0.0))))
                       (site-radius
                         (if on-u-p
                             (if on-v-p ,(if (eq widths :site)
                                             'corner-radius
                                             'radius)
                                 u-radius)
                             (if on-v-p v-radius radius)))
                       (shaped (chamfer-point flat minority site-radius))
                       (shaped-normal (chamfer-normal minority normal))))
                    (:round
                     ;; Every point projects onto the sphere or cylinder its
                     ;; nearest crease selects: a vertex onto its own star's,
                     ;; an inner point nowhere, and the rest onto the nearer
                     ;; creased edge's cylinder (their own edge's when on a
                     ;; boundary), blended toward the corner's sphere by
                     ;; their distance when that corner is a genuine
                     ;; three-axis star, or the sphere outright when the
                     ;; corner is a pure single-cell one.
                     `((corner-q (clamp corner-minority
                                        (vec3 -1.0 -1.0 -1.0) (vec3 1.0 1.0 1.0)))
                       (corner-foot (+ anchor
                                       (* edge-a (if (< su 0.0) 0.0 1.0))
                                       (* edge-b (if (< sv 0.0) 0.0 1.0))))
                       (corner-centre (+ corner-foot (* corner-q radius)))
                       (corner-weight (dot corner-minority corner-minority))
                       (corner-pure-p (if (> corner-weight 0.5)
                                          (< corner-weight 3.5)
                                          (> 0.0 1.0)))
                       (corner-three-p (> (dot corner-q corner-q) 2.5))
                       (u-creased-p (> (dot u-minority u-minority) 0.5))
                       (v-creased-p (> (dot v-minority v-minority) 0.5))
                       (u-foot (+ anchor
                                  (* edge-a (if (< su 0.0) 0.0 1.0))
                                  (* edge-b t*)))
                       (v-foot (+ anchor
                                  (* edge-a s)
                                  (* edge-b (if (< sv 0.0) 0.0 1.0))))
                       ;; The base projection's edge: the point's own when it
                       ;; lies on one, else the nearer creased edge.
                       (use-u-p (if on-u-p
                                    (> 1.0 0.0)
                                    (if on-v-p
                                        (> 0.0 1.0)
                                        (if u-near-p
                                            (if v-near-p
                                                (if u-creased-p
                                                    (if v-creased-p
                                                        (<= u-steps v-steps)
                                                        (> 1.0 0.0))
                                                    (> 0.0 1.0))
                                                (> 1.0 0.0))
                                            (> 0.0 1.0)))))
                       (base-minority (if use-u-p u-minority v-minority))
                       (base-foot (if on-u-p flat
                                      (if on-v-p flat
                                          (if use-u-p u-foot v-foot))))
                       (base-centre (+ base-foot
                                       (site-centre base-minority radius)))
                       (base-point (rounded-point flat base-centre
                                                  base-minority radius))
                       (base-normal (rounded-normal flat base-centre
                                                    base-minority normal))
                       (sphere-point (rounded-point flat corner-centre
                                                    corner-minority radius))
                       (sphere-normal (rounded-normal flat corner-centre
                                                      corner-minority normal))
                       ;; Distance to the corner in ring steps: along the
                       ;; edge for boundary points, the larger axis
                       ;; otherwise, and the full ring count past the rings.
                       (u-reach (if u-near-p u-steps ,(float rings)))
                       (v-reach (if v-near-p v-steps ,(float rings)))
                       (steps (if on-u-p v-reach
                                  (if on-v-p u-reach (max u-reach v-reach))))
                       (falloff (max 0.0 (- 1.0 (/ steps ,(float rings)))))
                       (weight (if corner-pure-p 1.0
                                   (if corner-three-p falloff 0.0)))
                       (mixed-point (mix base-point sphere-point weight))
                       (mixed-normal (normalize (mix base-normal sphere-normal
                                                     weight)))
                       (corner-p (if on-u-p on-v-p (> 0.0 1.0)))
                       (inner-p (if u-near-p (> 0.0 1.0)
                                    (if v-near-p (> 0.0 1.0) (> 1.0 0.0))))
                       (shaped (if corner-p sphere-point
                                   (if inner-p flat mixed-point)))
                       (shaped-normal (if corner-p sphere-normal
                                          (if inner-p normal mixed-normal))))))
                (rest-point (+ anchor (* (- shaped anchor) scale)))
                ,@(if deform
                      `((deform-kind (swizzle deform-vector :x))
                        (deform-strength (swizzle deform-vector :y))
                        (deform-scale (swizzle deform-vector :z))
                        (erode-strength (swizzle deform-vector :w))
                        (deform-centre (swizzle deform-centre-vector :xyz))
                        (erode-grain (max (swizzle deform-centre-vector :w)
                                          0.25))
                        ;; How far the lattice may wander here: the grit of
                        ;; the stock around this point, and how far out of
                        ;; the boundary the point stands.  Both are fields,
                        ;; so both are the same from every face that asks.
                        ,@(lattice-sample-bindings 'ground 'rest-point)
                        (exposure (smoothstep 0.80 0.30 ground-solid))
                        (amplitude (* erode-strength
                                      (* ground-grit
                                         (mix 1.0 exposure 0.65))))
                        (point (deform-point rest-point deform-kind
                                             deform-strength deform-scale
                                             deform-centre amplitude
                                             erode-grain))
                        (out-normal (deform-normal rest-point shaped-normal
                                                   deform-kind deform-strength
                                                   deform-scale deform-centre
                                                   amplitude erode-grain))
                        (bent-face (deform-normal rest-point normal
                                                  deform-kind deform-strength
                                                  deform-scale deform-centre
                                                  amplitude erode-grain)))
                      '((point rest-point)
                        (out-normal shaped-normal)))
                (clip (view-clip point camera
                                 (swizzle right-vector :xyz)
                                 (swizzle up-vector :xyz)
                                 (swizzle forward-vector :xyz)
                                 projection-vector)))
           (set-output position (jitter-clip clip (swizzle jitter-vector :xy)))
           (set-output normal out-normal)
           (set-output world point)
           (set-output uv (vec2 s t*))
           ,@(when (eq rule :chamfer)
               '((set-output face-normal normal)
                 (set-output stock stock-slot)))
           ,@(when deform
               '((set-output rest rest-point)
                 (set-output bent-normal bent-face))))))))

(defmethod surface-vertices-per-face ((style (eql :chamfer)))
  (grid-vertices-per-face 1))

(defmethod surface-vertices-per-face ((style (eql :paper)))
  (grid-vertices-per-face 1))

(defmethod surface-vertices-per-face ((style (eql :stock)))
  (grid-vertices-per-face 1))

(defmethod surface-vertices-per-face ((style (eql :bevel)))
  (grid-vertices-per-face 2))

#.(grid-vertex-shader-definition 'chamfer-vertex-shader 1 :chamfer)

#.(grid-vertex-shader-definition 'bevel-vertex-shader 2 :round)

;;; ------------------------------------------------------------------------
;;; The screen: one triangle behind everything, from three vertex indices

(define-shader sky-vertex-shader
    (:stage :vertex
     :inputs ((vertex-index :uint :built-in :vertex-index))
     :outputs ((position :vec4 :built-in :position)
               (ndc :vec2 :location 0)))
  ;; The oversized triangle covering clip space: (-1,-1), (3,-1), (-1,3).
  ;; Depth sits just inside the far plane so the pass may run first and the
  ;; world still draws over it under an ordinary less-than test.
  (let* ((index (float vertex-index))
         (x (if (< index 0.5) -1.0 (if (< index 1.5) 3.0 -1.0)))
         (y (if (< index 1.5) -1.0 3.0)))
    (set-output position (vec4 x y 0.99999 1.0))
    (set-output ndc (vec2 x y))))
