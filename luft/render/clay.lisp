;;; Clay: the star alphabet with a distance codomain.
;;;
;;; The site rules of the chamfer place each shared point by classifying its
;;; star; the tent field of [[field.lisp]] rounds everything, flats
;;; included, because a kernel cannot tell a crease from a continuation.
;;; This file keeps the alphabet -- the same edge and vertex stars, counted
;;; the same way -- and changes what an entry is: a signed distance
;;; combinator.  Every solid cell contributes its box distance, so a flat
;;; face is a plane and stays one, exactly; a convex half-edge star
;;; subtracts a cut prism, a concave one unions a fill wedge, and a pure
;;; one-cell corner is capped across its diagonal.  Two knobs drive the
;;; whole family: the width sets the 45-degree facet each crease is planed
;;; to, and the rounding reach blends every junction through smooth CSG.
;;; Width alone is the chamfer look; rounding alone planes nothing and
;;; fillets every crease; together they make wide soft mouldings.  Whatever
;;; the knobs say, a flat continuation is a box face untouched by any term,
;;; which is what the mesh styles promise and the tent field cannot.
;;;
;;; Coherence is structural rather than proved case by case: the field is
;;; a function of position alone, so two faces cannot disagree about it.
;;; The star of the nearest lattice vertex determines everything within
;;; the unit cube around that vertex -- its eight cells are the octants,
;;; its six half-edges the only creases, itself the only corner -- and on
;;; the cube boundary the features within reach are exactly the shared
;;; ones, so the pieces agree where they meet.  The field is trustworthy
;;; to about half a cell of the surface; min/max composition only ever
;;; underestimates distance, which is the safe direction for every use
;;; below.
;;;
;;; True distance is also the point of the shading.  The mesh is walked
;;; onto the field's own zero set by Newton steps, so the silhouette is
;;; the fillet and not a memory of the box; the sun shadow is sphere
;;; traced with a penumbra set by how narrowly the ray misses; ambient
;;; occlusion is read off distance taps along the normal.

(in-package #:luft.render.shaders)

;;; ------------------------------------------------------------------------
;;; Smooth CSG: the rounding knob

(define-shader-function clay-union (a b k)
  "The smooth union of distances A and B over reach K."
  (let* ((h (clamp (+ 0.5 (* 0.5 (/ (- b a) k))) 0.0 1.0)))
    (- (mix b a h) (* k (* h (- 1.0 h))))))

(define-shader-function clay-cut (a b k)
  "The smooth cut: A with the region whose removal field is B taken away."
  (let* ((h (clamp (+ 0.5 (* 0.5 (/ (- a b) k))) 0.0 1.0)))
    (+ (mix b a h) (* k (* h (- 1.0 h))))))

;;; ------------------------------------------------------------------------
;;; The field
;;;
;;; One evaluation reads the eight cells around the nearest lattice vertex
;;; and composes: box distances for the solid, then every fill, then every
;;; cut, so a cove's wedge can itself be planed where an arris runs into
;;; it, the way a mason resolves a mixed corner.  A half-edge whose
;;; collinear partner classifies identically drops the axial bound between
;;; them, so a long arris is one unbroken prism rather than a chain of
;;; half-edge prisms meeting in seams.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clay-axis-letter (axis) (ecase axis (0 "X") (1 "Y") (2 "Z")))

  (defun clay-field-bindings (name point-form width-form round-form)
    "LET* bindings computing the clay field at POINT-FORM as NAME.

The value is the signed distance to the star-carved solid: negative
inside, positive in air, exact to the alphabet near the surface and a
safe underestimate elsewhere.  WIDTH-FORM is the crease facet width in
cells and ROUND-FORM the smooth-CSG reach; both are read once.  The
bindings read CELLS, PERIOD-X, and PERIOD-Y from the enclosing shader."
    (let* ((p (intern (format nil "~A-POINT" name)))
           (w (intern (format nil "~A-WIDTH" name)))
           (k (intern (format nil "~A-REACH" name)))
           (v (intern (format nil "~A-VERTEX" name)))
           (q (intern (format nil "~A-Q" name)))
           (qx (intern (format nil "~A-QX" name)))
           (qy (intern (format nil "~A-QY" name)))
           (qz (intern (format nil "~A-QZ" name)))
           (solid (intern (format nil "~A-SOLID" name))))
      (labels ((cell (i j k) (intern (format nil "~A-CELL-~D~D~D" name i j k)))
               (local (format-string &rest arguments)
                 (intern (apply #'format nil format-string arguments)))
               (sgn (index) (if (= index 1) 1.0 -1.0))
               (q-axis (axis) (ecase axis (0 qx) (1 qy) (2 qz)))
               ;; The four cells around the half-edge along AXIS on side
               ;; SIDE, as (perpendicular-u perpendicular-v cell) triples.
               (edge-cells (axis side)
                 (loop for iu below 2
                       append (loop for iv below 2
                                    collect (list iu iv
                                                  (ecase axis
                                                    (0 (cell side iu iv))
                                                    (1 (cell iu side iv))
                                                    (2 (cell iu iv side)))))))
               (sum (terms) (reduce (lambda (a b) `(+ ,a ,b)) terms)))
        (let ((corners '((0 0 0) (1 0 0) (0 1 0) (1 1 0)
                         (0 0 1) (1 0 1) (0 1 1) (1 1 1))))
          (append
           `((,p ,point-form)
             (,w ,width-form)
             ;; The reach cap keeps every term either shared or exactly
             ;; inert at the vertex-cube boundary, so no knob setting can
             ;; draw cube seams on the surface.
             (,k (clamp ,round-form 0.0005
                        (max 0.001 (- 0.25 (* 0.35 ,w)))))
             (,v (floor (+ ,p (vec3 0.5 0.5 0.5))))
             (,q (- ,p ,v))
             (,qx (swizzle ,q :x))
             (,qy (swizzle ,q :y))
             (,qz (swizzle ,q :z)))
           ;; The eight cells of the star: indices 0/1 are the cells on
           ;; the negative/positive side of the vertex along each axis.
           (loop for (i j k) in corners
                 append (field-cell-bindings
                         (cell i j k)
                         `(+ ,v (vec3 ,(if (= i 1) 0.0 -1.0)
                                      ,(if (= j 1) 0.0 -1.0)
                                      ,(if (= k 1) 0.0 -1.0)))))
           ;; The flat solid: the least box distance any solid cell
           ;; offers.  A cell is the unit box about its own centre, half
           ;; a cell out from the vertex along each axis.
           (loop for (i j k) in corners
                 append (let ((b (local "~A-B-~D~D~D" name i j k))
                              (bo (local "~A-BO-~D~D~D" name i j k))
                              (d (local "~A-BD-~D~D~D" name i j k)))
                          `((,b (- (abs (- ,q (vec3 ,(* 0.5 (sgn i))
                                                    ,(* 0.5 (sgn j))
                                                    ,(* 0.5 (sgn k)))))
                                   (vec3 0.5 0.5 0.5)))
                            (,bo (max ,b (vec3 0.0 0.0 0.0)))
                            (,d (if (> ,(cell i j k) 0.5)
                                    (+ (sqrt (dot ,bo ,bo))
                                       (min (max (max (swizzle ,b :x)
                                                      (swizzle ,b :y))
                                                 (swizzle ,b :z))
                                            0.0))
                                    9.0)))))
           `((,solid
              (min (min (min ,(local "~A-BD-000" name)
                             ,(local "~A-BD-100" name))
                        (min ,(local "~A-BD-010" name)
                             ,(local "~A-BD-110" name)))
                   (min (min ,(local "~A-BD-001" name)
                             ,(local "~A-BD-101" name))
                        (min ,(local "~A-BD-011" name)
                             ,(local "~A-BD-111" name))))))
           ;; Each half-edge's star: its solid count and minority
           ;; direction in the perpendicular plane, exactly
           ;; EDGE-MINORITY's counting.
           (loop for axis below 3
                 for letter = (clay-axis-letter axis)
                 for (u-axis v-axis) = (ecase axis
                                         (0 '(1 2)) (1 '(0 2)) (2 '(0 1)))
                 append
                 (loop for side below 2
                       for tag = (format nil "~A~A" letter side)
                       for cells = (edge-cells axis side)
                       append
                       `((,(local "~A-CNT-~A" name tag)
                          ,(sum (mapcar #'third cells)))
                         (,(local "~A-MU-~A" name tag)
                          ,(sum (loop for (iu nil c) in cells
                                      collect `(* ,(sgn iu) ,c))))
                         (,(local "~A-MV-~A" name tag)
                          ,(sum (loop for (nil iv c) in cells
                                      collect `(* ,(sgn iv) ,c))))
                         (,(local "~A-QD-~A" name tag)
                          (+ (* ,(q-axis u-axis) ,(local "~A-MU-~A" name tag))
                             (* ,(q-axis v-axis)
                                ,(local "~A-MV-~A" name tag))))
                         (,(local "~A-QA-~A" name tag)
                          (* ,(sgn side) ,(q-axis axis)))
                         ;; The legs: how far the point stands into the
                         ;; minority's quadrant along each perpendicular
                         ;; axis.  They clip a prism to its own crease's
                         ;; quadrant, so a wedge cannot reach around a
                         ;; corner its cove never turned.
                         (,(local "~A-LU-~A" name tag)
                          (* ,(q-axis u-axis) ,(local "~A-MU-~A" name tag)))
                         (,(local "~A-LV-~A" name tag)
                          (* ,(q-axis v-axis) ,(local "~A-MV-~A" name tag)))
                         (,(local "~A-CONVEX-~A" name tag)
                          (if (< (abs (- ,(local "~A-CNT-~A" name tag) 1.0))
                                 0.25)
                              1.0 0.0))
                         (,(local "~A-CONCAVE-~A" name tag)
                          (if (< (abs (- ,(local "~A-CNT-~A" name tag) 3.0))
                                 0.25)
                              1.0 0.0)))))
           ;; The alphabet entries.  A cut's field is positive inside the
           ;; removed prism; a fill's is the wedge's own signed distance;
           ;; both are scaled to true distance by the minority's sqrt(2).
           ;; Each half-edge's prism overhangs its own vertex by one
           ;; blend reach, so the two half-edges of a straight crease
           ;; overlap seamlessly through the vertex while a crease that
           ;; ends dies off within the reach; at the shared cube boundary
           ;; the bound reads the same from both adjoining vertices --
           ;; the four-cell star is theirs jointly -- and at the opposite
           ;; boundary the term has gone exactly inert, the reach cap
           ;; being what guarantees it.
           (loop for axis below 3
                 for letter = (clay-axis-letter axis)
                 append
                 (loop for side below 2
                       for tag = (format nil "~A~A" letter side)
                       append
                       `((,(local "~A-CUT-~A" name tag)
                          (if (> ,(local "~A-CONVEX-~A" name tag) 0.5)
                              (min (min (* 0.70710678
                                           (- ,w ,(local "~A-QD-~A" name
                                                         tag)))
                                        (+ ,(local "~A-QA-~A" name tag) ,k))
                                   ;; Relaxed by the width so the removed
                                   ;; sliver stays solidly positive along
                                   ;; the old arris rather than reading
                                   ;; as a phantom near-zero ridge there.
                                   (+ (min ,(local "~A-LU-~A" name tag)
                                           ,(local "~A-LV-~A" name tag))
                                      ,w))
                              -9.0))
                         (,(local "~A-FILL-~A" name tag)
                          (if (> ,(local "~A-CONCAVE-~A" name tag) 0.5)
                              (max (max (* 0.70710678
                                           (- (- ,(local "~A-QD-~A" name
                                                         tag))
                                              ,w))
                                        (- (- 0.0 ,(local "~A-QA-~A" name
                                                          tag))
                                           ,k))
                                   ;; Relaxed by the width so the wedge
                                   ;; keeps its full depth over the face
                                   ;; strips its facet replaces.
                                   (- (max ,(local "~A-LU-~A" name tag)
                                           ,(local "~A-LV-~A" name tag))
                                      ,w))
                              9.0)))))
           ;; The vertex star: a pure one-cell corner is capped across
           ;; its diagonal at twice the width, CHAMFER-POINT's own plane;
           ;; a pure one-air corner is filled by that plane's mirror.
           ;; Mixed corners are left to their edges and the smooth
           ;; junctions.
           (let ((cnt8 (local "~A-CNT8" name))
                 (m3x (local "~A-M3X" name))
                 (m3y (local "~A-M3Y" name))
                 (m3z (local "~A-M3Z" name))
                 (qd3 (local "~A-QD3" name)))
             `((,cnt8 ,(sum (loop for (i j k) in corners
                                  collect (cell i j k))))
               (,m3x ,(sum (loop for (i j k) in corners
                                 collect `(* ,(sgn i) ,(cell i j k)))))
               (,m3y ,(sum (loop for (i j k) in corners
                                 collect `(* ,(sgn j) ,(cell i j k)))))
               (,m3z ,(sum (loop for (i j k) in corners
                                 collect `(* ,(sgn k) ,(cell i j k)))))
               (,qd3 (+ (+ (* ,qx ,m3x) (* ,qy ,m3y)) (* ,qz ,m3z)))
               ;; A corner cap is one cube's private term, so it is
               ;; clipped to its own cell's octant by three legs and
               ;; falls off conically, exactly inert by the boundary.
               (,(local "~A-QMAX" name)
                (max (max (abs ,qx) (abs ,qy)) (abs ,qz)))
               (,(local "~A-CUT3" name)
                (if (< (abs (- ,cnt8 1.0)) 0.25)
                    (min (min (* 0.57735027 (- (* 2.0 ,w) ,qd3))
                              (- (- 0.5 ,k) ,(local "~A-QMAX" name)))
                         (+ (min (min (* ,qx ,m3x) (* ,qy ,m3y))
                                 (* ,qz ,m3z))
                            ,w))
                    -9.0))
               (,(local "~A-FILL3" name)
                (if (< (abs (- ,cnt8 7.0)) 0.25)
                    (max (max (* 0.57735027 (- (- ,qd3) (* 2.0 ,w)))
                              (- ,(local "~A-QMAX" name) (- 0.5 ,k)))
                         (- (max (max (* ,qx ,m3x) (* ,qy ,m3y))
                                 (* ,qz ,m3z))
                            ,w))
                    9.0))))
           ;; Fills first, cuts second: an arris planes off the end of
           ;; the cove it runs into, not the other way around.
           (let ((step solid)
                 (bindings '()))
             (flet ((fold (function term)
                      (let ((next (local "~A-FOLD-~D" name
                                         (length bindings))))
                        (push `(,next (,function ,step ,term ,k)) bindings)
                        (setf step next))))
               (loop for axis below 3
                     for letter = (clay-axis-letter axis)
                     do (dotimes (side 2)
                          (fold 'clay-union
                                (local "~A-FILL-~A~A" name letter side))))
               (fold 'clay-union (local "~A-FILL3" name))
               (loop for axis below 3
                     for letter = (clay-axis-letter axis)
                     do (dotimes (side 2)
                          (fold 'clay-cut
                                (local "~A-CUT-~A~A" name letter side))))
               (fold 'clay-cut (local "~A-CUT3" name))
               (append (nreverse bindings) `((,name ,step)))))))))))

;;; ------------------------------------------------------------------------
;;; The :CLAY style: the grid projected onto the field, under its light
;;;
;;; The rasterized geometry is the two-ring grid with every point walked
;;; onto the clay zero set by Newton steps -- true distance makes Newton
;;; nearly exact, so the silhouette is the fillet itself -- and every
;;; fragment shades from the field at its own world position: normal from
;;; a tetrahedral gradient, sun shadow sphere traced through the field
;;; near the surface and handed to the cell walk beyond its trusted band,
;;; ambient occlusion from four distance taps along the normal.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clay-newton-bindings (prefix start-form width-form round-form)
    "Bindings taking one Newton step from START-FORM toward the zero set.

PREFIX-POINT is the stepped point and PREFIX-GRADIENT the tetrahedral
gradient read at START-FORM, reusable as an outward normal once the steps
have converged."
    (let ((start (intern (format nil "~A-START" prefix)))
          (a (intern (format nil "~A-A" prefix)))
          (b (intern (format nil "~A-B" prefix)))
          (c (intern (format nil "~A-C" prefix)))
          (d (intern (format nil "~A-D" prefix)))
          (gradient (intern (format nil "~A-GRADIENT" prefix)))
          (value (intern (format nil "~A-VALUE" prefix)))
          (slope (intern (format nil "~A-SLOPE" prefix)))
          (move (intern (format nil "~A-MOVE" prefix)))
          (reach (intern (format nil "~A-REACH" prefix)))
          (point (intern (format nil "~A-POINT" prefix))))
      `((,start ,start-form)
        ,@(clay-field-bindings a `(+ ,start (* (vec3 1.0 -1.0 -1.0) 0.02))
                               width-form round-form)
        ,@(clay-field-bindings b `(+ ,start (* (vec3 -1.0 1.0 -1.0) 0.02))
                               width-form round-form)
        ,@(clay-field-bindings c `(+ ,start (* (vec3 -1.0 -1.0 1.0) 0.02))
                               width-form round-form)
        ,@(clay-field-bindings d `(+ ,start (* (vec3 1.0 1.0 1.0) 0.02))
                               width-form round-form)
        ;; The tetrahedral sum over 1/(4 epsilon), so the gradient's
        ;; magnitude is a true slope and Newton's step a true distance.
        (,gradient (* (+ (+ (* (vec3 1.0 -1.0 -1.0) ,a)
                            (* (vec3 -1.0 1.0 -1.0) ,b))
                         (+ (* (vec3 -1.0 -1.0 1.0) ,c)
                            (* (vec3 1.0 1.0 1.0) ,d)))
                      12.5))
        (,value (* 0.25 (+ (+ ,a ,b) (+ ,c ,d))))
        (,slope (max (dot ,gradient ,gradient) 0.000001))
        ;; A vanishing gradient -- a junction ridge or a point on the
        ;; field's medial axis -- must not launch the vertex across the
        ;; world: the step is clamped by length, and never to more than
        ;; a couple of times the distance the value claims.
        (,move (* ,gradient (/ ,value ,slope)))
        (,reach (sqrt (dot ,move ,move)))
        (,point (- ,start
                   (* ,move (min 1.0 (/ (min 0.6 (+ (* 2.0 (abs ,value))
                                                    0.02))
                                        (max ,reach 0.000001))))))))))

(defmethod surface-vertices-per-face ((style (eql :clay)))
  (grid-vertices-per-face 2))

#.(grid-vertex-shader-definition 'clay-vertex-shader 2 :clay)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *clay-shadow-steps* 12
    "Sphere-trace steps of the clay sun shadow: its cost and its reach.")

  (defvar *clay-penumbra* 9.0
    "How hard the clay shadow's edge is: the penumbra is the narrowest
miss along the ray, scaled by this.")

  (defun clay-fragment-shader-definition ()
    "The clay fragment shader, spliced around its field evaluations."
    `(define-temporal-fragment-shaders
         (clay-fragment-shader temporal-clay-fragment-shader
          (world-motion world))
         (:stage :fragment
          :inputs ((normal :vec3 :location 0)
                   (world :vec3 :location 1)
                   (uv :vec2 :location 2))
          :outputs ((color :vec4 :location 0))
          :resources ((frame :uniform-block :binding ,+frame-binding+
                             :members ,*frame-uniform-members*)
                      (cells :storage-buffer :binding ,+cells-binding+
                             :element :uint)))
       (let* ((period-x (swizzle domain-vector :x))
              (period-y (swizzle domain-vector :y))
              (width (swizzle domain-vector :z))
              ;; The rounding reach as the field itself will cap it.
              (rounding (clamp (swizzle domain-vector :w)
                               0.0 (max 0.001 (- 0.25 (* 0.35 width)))))
              (face (normalize normal))
              ;; The gradient by tetrahedron: four probes a hair from
              ;; the fragment, summed along their directions.
              ,@(clay-field-bindings
                 'probe-a '(+ world (* (vec3 1.0 -1.0 -1.0) 0.02))
                 'width 'rounding)
              ,@(clay-field-bindings
                 'probe-b '(+ world (* (vec3 -1.0 1.0 -1.0) 0.02))
                 'width 'rounding)
              ,@(clay-field-bindings
                 'probe-c '(+ world (* (vec3 -1.0 -1.0 1.0) 0.02))
                 'width 'rounding)
              ,@(clay-field-bindings
                 'probe-d '(+ world (* (vec3 1.0 1.0 1.0) 0.02))
                 'width 'rounding)
              (gradient (+ (+ (* (vec3 1.0 -1.0 -1.0) probe-a)
                              (* (vec3 -1.0 1.0 -1.0) probe-b))
                           (+ (* (vec3 -1.0 -1.0 1.0) probe-c)
                              (* (vec3 1.0 1.0 1.0) probe-d))))
              (smooth (if (> (dot gradient gradient) 0.0000001)
                          (normalize gradient)
                          face))
              (here (* 0.25 (+ (+ probe-a probe-b) (+ probe-c probe-d))))
              ;; Wear: the field against a softer profile of itself,
              ;; capped by the same bound the field enforces.
              (wear-reach (min 0.18 (max 0.001 (- 0.25 (* 0.35 width)))))
              ,@(clay-field-bindings 'eased 'world 'width 'wear-reach)
              (relief (clamp (* 5.0 (- here eased)) -0.5 0.5))
              (wear (swizzle occlusion-vector :z))
              ;; Where the field swells past the mesh the fragment sits
              ;; inside the clay; every march below starts lifted onto
              ;; the surface along the normal.
              (lift (max 0.06 (- 0.06 here)))
              (surface (+ world (* smooth lift)))
              ;; The sun shadow: sphere traced, the penumbra set by the
              ;; narrowest miss.  A negative sample is a hit and darkens
              ;; fully; past the trusted band the cell walk takes over.
              (sun (swizzle sun-vector :xyz))
              (traced
                (swizzle
                 (counted-fold (%shadow-index ,(float *clay-shadow-steps*)
                                %shadow-state (vec4 0.12 1.0 0.0 0.0))
                   (let* ((%shadow-t (swizzle %shadow-state :x))
                          (%shadow-pen (swizzle %shadow-state :y))
                          (%shadow-point (+ surface (* sun %shadow-t)))
                          ,@(clay-field-bindings '%shadow-d '%shadow-point
                                                 'width 'rounding)
                          (%shadow-next
                            (min %shadow-pen
                                 (* ,*clay-penumbra*
                                    (/ %shadow-d (max %shadow-t 0.05)))))
                          (%shadow-step (clamp %shadow-d 0.04 0.30)))
                     (vec4 (+ %shadow-t %shadow-step) %shadow-next
                           0.0 0.0)))
                 :y))
              (near-shade (clamp traced 0.0 1.0))
              (walk-origin (+ surface (* smooth (* 2.0 (+ width rounding)))))
              (walk (marched-cell-walk walk-origin sun
                                       cells period-x period-y
                                       ,*shadow-steps*))
              (far-shade (if (> (swizzle walk :w) 0.5) 0.0 1.0))
              (shade (mix 1.0 (min near-shade far-shade)
                          (swizzle occlusion-vector :y)))
              ;; Occlusion: how much less room than distance asks for
              ;; the field leaves along the normal, at four taps.
              ,@(clay-field-bindings 'tap-a '(+ surface (* smooth 0.15))
                                     'width 'rounding)
              ,@(clay-field-bindings 'tap-b '(+ surface (* smooth 0.28))
                                     'width 'rounding)
              ,@(clay-field-bindings 'tap-c '(+ surface (* smooth 0.40))
                                     'width 'rounding)
              ,@(clay-field-bindings 'tap-d '(+ surface (* smooth 0.50))
                                     'width 'rounding)
              (crowding
                (clamp (* 0.9 (+ (+ (* 3.0 (max 0.0 (- 0.15 tap-a)))
                                    (* 1.8 (max 0.0 (- 0.28 tap-b))))
                                 (+ (* 1.2 (max 0.0 (- 0.40 tap-c)))
                                    (* 0.9 (max 0.0 (- 0.50 tap-d))))))
                       0.0 1.0))
              (open (- 1.0 (* (swizzle occlusion-vector :x) crowding)))
              ;; The materials meet across the fillet: turf thins as the
              ;; ground turns downward, as the field style's does.
              (upness (swizzle smooth :z))
              (tone (mix (mix (swizzle side-vector :xyz)
                              (swizzle bottom-vector :xyz)
                              (smoothstep -0.35 -0.75 upness))
                         (swizzle top-vector :xyz)
                         (smoothstep 0.25 0.75 upness)))
              (worn (* tone (+ 1.0 (* wear (- (* 0.45 (max 0.0 (- relief)))
                                              (* 0.8 (max 0.0 relief)))))))
              ;; One cell's tone drifts a little from its neighbour's.
              (cell (floor (- world (* smooth 0.25))))
              (patch (- (paper-noise (* cell 0.21)) 0.5))
              (jitter (- (paper-hash cell) 0.5))
              (drift (+ 1.0 (* 0.08 (+ (* 1.35 patch) (* 0.45 jitter)))))
              (warm-patch (paper-noise (+ (* cell 0.13) (vec3 19.7 7.3 3.1))))
              (warmth (mix (vec3 0.975 0.99 1.03) (vec3 1.03 1.01 0.97)
                           warm-patch))
              (final (surface-lighting (* worn (* warmth drift))
                                       smooth world open shade
                                       camera-vector sun-vector
                                       sun-colour-vector fill-vector
                                       sky-vector ground-vector)))
         (set-output color (vec4 final 1.0))))))

#.(clay-fragment-shader-definition)
