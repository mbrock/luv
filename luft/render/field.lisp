;;; The occupancy field: rounding as a level set.
;;;
;;; The site rules of #6TEFOS place each shared point by classifying its
;;; star, and the mixed stars -- a block standing on a floor, a bar's foot --
;;; show where a heuristic over eight cells runs out: the vertical fillet
;;; flares into a skirt, and the floor's points and the block's points, placed
;;; by different cases, fold into each other.  This file replaces the
;;; classification with a definition.  #TI9NJP
;;;
;;; Smooth the solid's indicator with a tent kernel of half-width R along
;;; each axis.  The result F is the fraction of solid under the tent: one deep
;;; inside, zero in open air, exactly one half on a flat wall, and a C1
;;; function everywhere because a tent integrated against a step is a
;;; quadratic ramp.  The rounded solid is the half-level set of F.  Every
;;; grid point of every face is projected onto that set along the gradient
;;; of F, and the surface normal is minus that gradient.  Faces incident to a
;;; site agree without any stitching because the projection is a function of
;;; position alone; a convex crease becomes a fillet and a concave one a cove
;;; of the same reach R; and a mixed star becomes whatever smooth blend the
;;; field makes of it, which is always one surface.
;;;
;;; With R at most one half, the tent overlaps two cells along each axis, so
;;; F and its gradient come from eight cell reads, each weighted by a product
;;; of one-dimensional tent integrals: the same cost as a trilinear sample.

(in-package #:luft.render.shaders)

(define-shader-function tent-cdf (u radius)
  "The integral of the unit-area tent of half-width RADIUS up to U."
  (if (< u (- radius))
      0.0
      (if (< u 0.0)
          (/ (* (+ u radius) (+ u radius)) (* 2.0 (* radius radius)))
          (if (< u radius)
              (- 1.0 (/ (* (- radius u) (- radius u))
                        (* 2.0 (* radius radius))))
              1.0))))

(define-shader-function tent (u radius)
  "The unit-area tent of half-width RADIUS at U."
  (/ (max 0.0 (- radius (abs u))) (* radius radius)))

(define-shader-function tent-layer (corners u v radius)
  "One cell layer's part of the field: its occupancy weighted along X and Y.

CORNERS holds the layer's four occupancies ordered 00 10 01 11 along X then
Y; U and V are the positions of the boundary between the two cells along
each axis relative to the point.  The value is the X slope, the Y slope, and
the weighted sum, still to be weighted along Z."
  (let* ((cx (tent-cdf u radius))
         (cy (tent-cdf v radius))
         (tx (tent u radius))
         (ty (tent v radius))
         (c00 (swizzle corners :x))
         (c10 (swizzle corners :y))
         (c01 (swizzle corners :z))
         (c11 (swizzle corners :w))
         (wx1 (- 1.0 cx))
         (wy1 (- 1.0 cy)))
    (vec3 (+ (* c00 (* (- tx) cy)) (* c10 (* tx cy))
             (* c01 (* (- tx) wy1)) (* c11 (* tx wy1)))
          (+ (* c00 (* cx (- ty))) (* c10 (* wx1 (- ty)))
             (* c01 (* cx ty)) (* c11 (* wx1 ty)))
          (+ (* c00 (* cx cy)) (* c10 (* wx1 cy))
             (* c01 (* cx wy1)) (* c11 (* wx1 wy1))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun field-cell-bindings (name position-form)
    "LET* bindings reading the occupancy of the cell at POSITION-FORM as
NAME: wrapped horizontally, air above and below the vertical world."
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
        (,z (swizzle ,position-form :z))
        (,index (uint (+ ,x (* period-x
                                (+ ,y (* period-y (clamp ,z 0.0 255.0)))))))
        (,word (buffer-element cells (/ ,index (uint 32.0))))
        (,name (if (< ,z 0.0) 0.0
                   (if (> ,z 255.0) 0.0
                       (float (ldb (byte 1 (mod ,index (uint 32.0)))
                                   ,word))))))))

  (defun occupancy-field-bindings (name point-form radius-form)
    "LET* bindings computing the tent-smoothed occupancy at POINT-FORM as
NAME: a vec4 of gradient and value.  RADIUS-FORM is the tent's half-width,
at most one half.  The bindings read CELLS, PERIOD-X, and PERIOD-Y from the
enclosing shader.

The lower and upper cell layers are summed separately by TENT-LAYER and
combined along Z here."
    (let ((p (intern (format nil "~A-POINT" name)))
          (r (intern (format nil "~A-RADIUS" name)))
          (a0 (intern (format nil "~A-A0" name)))
          (b0 (intern (format nil "~A-B0" name)))
          (c0 (intern (format nil "~A-C0" name)))
          (u (intern (format nil "~A-U" name)))
          (v (intern (format nil "~A-V" name)))
          (w (intern (format nil "~A-W" name)))
          (low (intern (format nil "~A-LOW" name)))
          (high (intern (format nil "~A-HIGH" name)))
          (cz (intern (format nil "~A-CZ" name)))
          (tz (intern (format nil "~A-TZ" name)))
          (below (intern (format nil "~A-BELOW" name)))
          (above (intern (format nil "~A-ABOVE" name))))
      (flet ((cell (i j k) (intern (format nil "~A-CELL-~D~D~D" name i j k))))
        `((,p ,point-form)
          (,r (clamp ,radius-form 0.01 0.5))
          ;; The lower cell along each axis, and the position of the
          ;; boundary between the two cells relative to the point.
          (,a0 (floor (- (swizzle ,p :x) ,r)))
          (,b0 (floor (- (swizzle ,p :y) ,r)))
          (,c0 (floor (- (swizzle ,p :z) ,r)))
          (,u (- (+ ,a0 1.0) (swizzle ,p :x)))
          (,v (- (+ ,b0 1.0) (swizzle ,p :y)))
          (,w (- (+ ,c0 1.0) (swizzle ,p :z)))
          ,@(loop for (i j k) in '((0 0 0) (1 0 0) (0 1 0) (1 1 0)
                                   (0 0 1) (1 0 1) (0 1 1) (1 1 1))
                  append (field-cell-bindings
                          (cell i j k)
                          `(vec3 (+ ,a0 ,(float i))
                                 (+ ,b0 ,(float j))
                                 (+ ,c0 ,(float k)))))
          (,low (vec4 ,(cell 0 0 0) ,(cell 1 0 0) ,(cell 0 1 0) ,(cell 1 1 0)))
          (,high (vec4 ,(cell 0 0 1) ,(cell 1 0 1) ,(cell 0 1 1) ,(cell 1 1 1)))
          ;; The lower layer's weight along Z and its slope; the upper
          ;; layer's are one less that weight and the negative slope.
          (,cz (tent-cdf ,w ,r))
          (,tz (tent ,w ,r))
          (,below (tent-layer ,low ,u ,v ,r))
          (,above (tent-layer ,high ,u ,v ,r))
          (,name (vec4 (+ (* (swizzle ,below :x) ,cz)
                          (* (swizzle ,above :x) (- 1.0 ,cz)))
                       (+ (* (swizzle ,below :y) ,cz)
                          (* (swizzle ,above :y) (- 1.0 ,cz)))
                       (* (- (swizzle ,above :z) (swizzle ,below :z)) ,tz)
                       (+ (* (swizzle ,below :z) ,cz)
                          (* (swizzle ,above :z) (- 1.0 ,cz))))))))))

(define-shader-function field-step (point field radius)
  "One Newton step of POINT toward the half-level set of FIELD, no longer
than RADIUS."
  (let* ((gradient (swizzle field :xyz))
         (slope (max (dot gradient gradient) 0.000001))
         (excess (- (swizzle field :w) 0.5))
         (step (* gradient (/ excess slope)))
         (length (sqrt (dot step step))))
    (- point (* step (min 1.0 (/ radius (max length 0.000001)))))))

(define-shader-function field-normal (field fallback)
  "The outward normal of the half-level set at FIELD: minus its gradient,
or FALLBACK where the gradient vanishes."
  (let* ((gradient (swizzle field :xyz)))
    (if (> (dot gradient gradient) 0.000001)
        (- (normalize gradient))
        fallback)))

;;; The vertex stage: the two-ring grid, every point projected.
(defmethod surface-vertices-per-face ((style (eql :field)))
  (grid-vertices-per-face 2))

#.(grid-vertex-shader-definition 'field-vertex-shader 2 :field)

;;; ------------------------------------------------------------------------
;;; Shading from the field
;;;
;;; The triangles are chords of the level set; the field knows the surface
;;; between them.  Each fragment takes its normal from the gradient at its
;;; own world position, so the rounding shades smoothly however coarse the
;;; grid, and reads the field once more under a wider tent to learn whether
;;; it sits in a hollow or on a ridge: the wear of a handled thing, edges
;;; lightened and cavities darkened, is the simplest honest use of that.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun field-fragment-shader-definition ()
    "The field fragment shader, spliced around two field samples."
    `(define-shader field-fragment-shader
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
              ,@(occupancy-field-bindings 'field 'world 'radius)
              (smooth (field-normal field (normalize normal)))
              (upness (swizzle smooth :z))
              (tone (if (> upness 0.5)
                        (swizzle top-vector :xyz)
                        (if (< upness -0.5)
                            (swizzle bottom-vector :xyz)
                            (swizzle side-vector :xyz))))
              ;; Wear: under a half-cell tent, a ridge has less than half
              ;; the solid around it and a hollow more.  The strength rides
              ;; in the occlusion lane's third component.
              ,@(occupancy-field-bindings 'wide 'world 0.5)
              (relief (- (swizzle wide :w) 0.5))
              (wear (swizzle occlusion-vector :z))
              (worn (* tone (+ 1.0 (* wear (- (* 0.8 (max 0.0 (- relief)))
                                              (* 1.4 (max 0.0 relief)))))))
              (walk-origin (+ world (* smooth (+ (* 2.0 radius) 0.1))))
              (walk (marched-cell-walk walk-origin (swizzle sun-vector :xyz)
                                       cells period-x period-y
                                       ,*shadow-steps*))
              (shade (mix 1.0 (if (> (swizzle walk :w) 0.5) 0.0 1.0)
                          (swizzle occlusion-vector :y)))
              (crowding (crowded-sky walk-origin smooth cells period-x period-y
                                     ,*occlusion-steps*))
              (open (- 1.0 (* (swizzle occlusion-vector :x) crowding)))
              (final (surface-lighting worn smooth world open shade
                                       camera-vector sun-vector
                                       sun-colour-vector fill-vector
                                       sky-vector ground-vector)))
         (set-output color (vec4 final 1.0))))))

#.(field-fragment-shader-definition)
