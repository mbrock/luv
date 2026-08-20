;;; Clay: the golf ball -- the style a distance field chooses for itself.
;;;
;;; The field is its own definition, exactly: the smooth union, at reach S,
;;; of one rounded box per solid cell -- each cell's box shrunk by the
;;; radius R and its distance lowered by R, so at a radius of one half a
;;; lone cell is exactly a sphere and a bar a string of pearls.  The
;;; surface is that field's zero set, nothing else; there is no target it
;;; approximates and no fence tuned against a picture.  Convex arrises
;;; round at R, coves fillet by the union's blend, diagonally touching
;;; cells grow a bridge, and coplanar neighbours meet in the shallow
;;; quilted seam that gives the style its nickname.  The quilting is not a
;;; defect but the honest look of this object: per-cell primitives cannot
;;; know two faces are one plane, which is why this is a style of its own
;;; and not a replacement for the chamfer family (see the wiki's account
;;; of the withdrawn classified-field experiment).
;;;
;;; One evaluation reads the eight cells around the nearest lattice
;;; vertex.  That local reading agrees with the global definition on and
;;; near the surface: a solid cell outside the star stands at least half a
;;; cell from the vertex's cube, so with S at most one half its term
;;; cannot move the union there.  Away from the surface the value is a
;;; safe underestimate, which is the direction every use below tolerates.
;;;
;;; True distance is also the point of the shading.  The mesh is walked
;;; onto the field's own zero set by Newton steps; the sun shadow is
;;; sphere traced with a penumbra set by how narrowly the ray misses --
;;; a contact shadow no cell walk can express -- and ambient occlusion is
;;; read off distance taps along the normal, so the quilting and the
;;; bridges darken exactly where they crowd themselves.

(in-package #:luft.render.shaders)

(define-shader-function clay-union (a b k)
  "The smooth union of distances A and B over reach K."
  (let* ((h (clamp (+ 0.5 (* 0.5 (/ (- b a) k))) 0.0 1.0)))
    (- (mix b a h) (* k (* h (- 1.0 h))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clay-field-bindings (name point-form radius-form melt-form)
    "LET* bindings computing the clay field at POINT-FORM as NAME.

The signed distance to the smooth union of the star's rounded cells:
negative inside, positive in air.  RADIUS-FORM is the cell rounding in
cells, up to one half; MELT-FORM the smooth-union reach.  The bindings
read CELLS, PERIOD-X, and PERIOD-Y from the enclosing shader."
    (let* ((p (intern (format nil "~A-POINT" name)))
           (r (intern (format nil "~A-RADIUS" name)))
           (s (intern (format nil "~A-MELT" name)))
           (half (intern (format nil "~A-HALF" name)))
           (v (intern (format nil "~A-VERTEX" name)))
           (q (intern (format nil "~A-Q" name))))
      (labels ((cell (i j k) (intern (format nil "~A-CELL-~D~D~D" name i j k)))
               (local (format-string &rest arguments)
                 (intern (apply #'format nil format-string arguments)))
               (sgn (index) (if (= index 1) 1.0 -1.0)))
        (let ((corners '((0 0 0) (1 0 0) (0 1 0) (1 1 0)
                         (0 0 1) (1 0 1) (0 1 1) (1 1 1))))
          (append
           `((,p ,point-form)
             (,r (clamp ,radius-form 0.03 0.5))
             (,s (clamp ,melt-form 0.0005 0.5))
             (,half (- 0.5 ,r))
             (,v (floor (+ ,p (vec3 0.5 0.5 0.5))))
             (,q (- ,p ,v)))
           (loop for (i j k) in corners
                 append (field-cell-bindings
                         (cell i j k)
                         `(+ ,v (vec3 ,(if (= i 1) 0.0 -1.0)
                                      ,(if (= j 1) 0.0 -1.0)
                                      ,(if (= k 1) 0.0 -1.0)))))
           ;; Each solid cell's rounded box: the box shrunk by R, its
           ;; distance lowered by R.  Air cells offer a distance no melt
           ;; reach can pull on, so they vanish from the union exactly.
           (loop for (i j k) in corners
                 append (let ((b (local "~A-B-~D~D~D" name i j k))
                              (bo (local "~A-BO-~D~D~D" name i j k))
                              (d (local "~A-D-~D~D~D" name i j k)))
                          `((,b (- (abs (- ,q (vec3 ,(* 0.5 (sgn i))
                                                    ,(* 0.5 (sgn j))
                                                    ,(* 0.5 (sgn k)))))
                                   (vec3 ,half ,half ,half)))
                            (,bo (max ,b (vec3 0.0 0.0 0.0)))
                            (,d (if (> ,(cell i j k) 0.5)
                                    (- (+ (sqrt (dot ,bo ,bo))
                                          (min (max (max (swizzle ,b :x)
                                                         (swizzle ,b :y))
                                                    (swizzle ,b :z))
                                               0.0))
                                       ,r)
                                    9.0)))))
           ;; The union, folded in one fixed order; smooth union is
           ;; commutative and associative enough that the seams cannot
           ;; tell.
           (let ((step nil)
                 (bindings '()))
             (loop for (i j k) in corners
                   for d = (local "~A-D-~D~D~D" name i j k)
                   do (if (null step)
                          (setf step d)
                          (let ((next (local "~A-U~D" name
                                             (length bindings))))
                            (push `(,next (clay-union ,step ,d ,s)) bindings)
                            (setf step next))))
             (append (nreverse bindings) `((,name ,step))))))))))

;;; ------------------------------------------------------------------------
;;; The :CLAY style: the grid projected onto the field, under its light
;;;
;;; The rasterized geometry is the two-ring grid with every point walked
;;; onto the clay zero set by Newton steps -- true distance makes Newton
;;; nearly exact, so a lone cell's mesh is genuinely its sphere -- and
;;; every fragment shades from the field at its own world position.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun clay-newton-bindings (prefix start-form radius-form melt-form)
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
                               radius-form melt-form)
        ,@(clay-field-bindings b `(+ ,start (* (vec3 -1.0 1.0 -1.0) 0.02))
                               radius-form melt-form)
        ,@(clay-field-bindings c `(+ ,start (* (vec3 -1.0 -1.0 1.0) 0.02))
                               radius-form melt-form)
        ,@(clay-field-bindings d `(+ ,start (* (vec3 1.0 1.0 1.0) 0.02))
                               radius-form melt-form)
        ;; The tetrahedral sum over 1/(4 epsilon), so the gradient's
        ;; magnitude is a true slope and Newton's step a true distance.
        (,gradient (* (+ (+ (* (vec3 1.0 -1.0 -1.0) ,a)
                            (* (vec3 -1.0 1.0 -1.0) ,b))
                         (+ (* (vec3 -1.0 -1.0 1.0) ,c)
                            (* (vec3 1.0 1.0 1.0) ,d)))
                      12.5))
        (,value (* 0.25 (+ (+ ,a ,b) (+ ,c ,d))))
        (,slope (max (dot ,gradient ,gradient) 0.000001))
        ;; A vanishing gradient -- a point on the field's medial axis --
        ;; must not launch the vertex across the world: the step is
        ;; clamped by length, and never to more than a couple of times
        ;; the distance the value claims.
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
              (radius (swizzle domain-vector :z))
              (melt (swizzle domain-vector :w))
              (face (normalize normal))
              ;; The gradient by tetrahedron: four probes a hair from the
              ;; fragment, summed along their directions.
              ,@(clay-field-bindings
                 'probe-a '(+ world (* (vec3 1.0 -1.0 -1.0) 0.02))
                 'radius 'melt)
              ,@(clay-field-bindings
                 'probe-b '(+ world (* (vec3 -1.0 1.0 -1.0) 0.02))
                 'radius 'melt)
              ,@(clay-field-bindings
                 'probe-c '(+ world (* (vec3 -1.0 -1.0 1.0) 0.02))
                 'radius 'melt)
              ,@(clay-field-bindings
                 'probe-d '(+ world (* (vec3 1.0 1.0 1.0) 0.02))
                 'radius 'melt)
              (gradient (+ (+ (* (vec3 1.0 -1.0 -1.0) probe-a)
                              (* (vec3 -1.0 1.0 -1.0) probe-b))
                           (+ (* (vec3 -1.0 -1.0 1.0) probe-c)
                              (* (vec3 1.0 1.0 1.0) probe-d))))
              (smooth (if (> (dot gradient gradient) 0.0000001)
                          (normalize gradient)
                          face))
              (here (* 0.25 (+ (+ probe-a probe-b) (+ probe-c probe-d))))
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
                                                 'radius 'melt)
                          (%shadow-next
                            (min %shadow-pen
                                 (* ,*clay-penumbra*
                                    (/ %shadow-d (max %shadow-t 0.05)))))
                          (%shadow-step (clamp %shadow-d 0.04 0.30)))
                     (vec4 (+ %shadow-t %shadow-step) %shadow-next 0.0 0.0)))
                 :y))
              (near-shade (clamp traced 0.0 1.0))
              (walk-origin (+ surface (* smooth (* 2.0 radius))))
              (walk (marched-cell-walk walk-origin sun
                                       cells period-x period-y
                                       ,*shadow-steps*))
              (far-shade (if (> (swizzle walk :w) 0.5) 0.0 1.0))
              (shade (mix 1.0 (min near-shade far-shade)
                          (swizzle occlusion-vector :y)))
              ;; Occlusion: how much less room than distance asks for the
              ;; field leaves along the normal, at four taps.
              ,@(clay-field-bindings 'tap-a '(+ surface (* smooth 0.15))
                                     'radius 'melt)
              ,@(clay-field-bindings 'tap-b '(+ surface (* smooth 0.28))
                                     'radius 'melt)
              ,@(clay-field-bindings 'tap-c '(+ surface (* smooth 0.40))
                                     'radius 'melt)
              ,@(clay-field-bindings 'tap-d '(+ surface (* smooth 0.50))
                                     'radius 'melt)
              (crowding
                (clamp (* 0.9 (+ (+ (* 3.0 (max 0.0 (- 0.15 tap-a)))
                                    (* 1.8 (max 0.0 (- 0.28 tap-b))))
                                 (+ (* 1.2 (max 0.0 (- 0.40 tap-c)))
                                    (* 0.9 (max 0.0 (- 0.50 tap-d))))))
                       0.0 1.0))
              (open (- 1.0 (* (swizzle occlusion-vector :x) crowding)))
              ;; The materials meet across the rounding: turf thins as
              ;; the ground turns downward, as the field style's does.
              (upness (swizzle smooth :z))
              (tone (mix (mix (swizzle side-vector :xyz)
                              (swizzle bottom-vector :xyz)
                              (smoothstep -0.35 -0.75 upness))
                         (swizzle top-vector :xyz)
                         (smoothstep 0.25 0.75 upness)))
              ;; One cell's tone drifts a little from its neighbour's.
              (cell (floor (- world (* smooth 0.25))))
              (patch (- (paper-noise (* cell 0.21)) 0.5))
              (jitter (- (paper-hash cell) 0.5))
              (drift (+ 1.0 (* 0.08 (+ (* 1.35 patch) (* 0.45 jitter)))))
              (warm-patch (paper-noise (+ (* cell 0.13) (vec3 19.7 7.3 3.1))))
              (warmth (mix (vec3 0.975 0.99 1.03) (vec3 1.03 1.01 0.97)
                           warm-patch))
              (final (surface-lighting (* tone (* warmth drift))
                                       smooth world open shade
                                       camera-vector sun-vector
                                       sun-colour-vector fill-vector
                                       sky-vector ground-vector)))
         (set-output color (vec4 final 1.0))))))

#.(clay-fragment-shader-definition)
