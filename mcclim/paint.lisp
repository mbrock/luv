;;; Paint designs evaluated by the direct McCLIM GPU backend.

(in-package #:mcluv)

(defclass relief-design (design)
  ((albedo :initarg :albedo :reader relief-albedo)
   (height :initarg :height :reader relief-height))
  (:documentation
   "A paint carrying signed height above its presentation surface.

Positive heights are raised and negative heights are recessed. Ordinary
McCLIM backends see ALBEDO; relief-aware backends may render the height."))

(defun make-relief-design (albedo height)
  (check-type albedo design)
  (check-type height real)
  (make-instance 'relief-design :albedo albedo :height height))

(defgeneric design-height (design)
  (:documentation "Return DESIGN's signed displacement in surface units."))

(defmethod design-height ((design design))
  (declare (ignore design))
  0)

(defmethod design-height ((design relief-design))
  (relief-height design))

(defmethod design-ink ((design relief-design) x y)
  (design-ink (relief-albedo design) x y))

(defmethod transform-region (transformation (design relief-design))
  (make-relief-design
   (transform-region transformation (relief-albedo design))
   (relief-height design)))

(defclass gradient-design (design)
  ((start-color :initarg :start-color :reader gradient-start-color)
   (end-color :initarg :end-color :reader gradient-end-color))
  (:documentation
   "A two-stop analytical paint. Geometry and paint remain independent."))

(defclass linear-gradient (gradient-design)
  ((x1 :initarg :x1 :reader linear-gradient-x1)
   (y1 :initarg :y1 :reader linear-gradient-y1)
   (x2 :initarg :x2 :reader linear-gradient-x2)
   (y2 :initarg :y2 :reader linear-gradient-y2)))

(defclass radial-gradient (gradient-design)
  ((center-x :initarg :center-x :reader radial-gradient-center-x)
   (center-y :initarg :center-y :reader radial-gradient-center-y)
   (radius :initarg :radius :reader radial-gradient-radius)))

(defun make-linear-gradient (x1 y1 x2 y2 start-color end-color)
  (make-instance 'linear-gradient
                 :x1 x1 :y1 y1 :x2 x2 :y2 y2
                 :start-color start-color :end-color end-color))

(defun make-radial-gradient (center-x center-y radius start-color end-color)
  (check-type radius (real 0 *))
  (make-instance 'radial-gradient
                 :center-x center-x :center-y center-y :radius radius
                 :start-color start-color :end-color end-color))

(defun clamp-unit (value)
  (min 1.0 (max 0.0 value)))

(defgeneric gradient-coordinate (gradient x y)
  (:documentation "Return GRADIENT's normalized coordinate at X,Y."))

(defmethod gradient-coordinate ((gradient linear-gradient) x y)
  (let* ((dx (- (linear-gradient-x2 gradient)
                (linear-gradient-x1 gradient)))
         (dy (- (linear-gradient-y2 gradient)
                (linear-gradient-y1 gradient)))
         (length-squared (+ (* dx dx) (* dy dy))))
    (if (zerop length-squared)
        0.0
        (/ (+ (* (- x (linear-gradient-x1 gradient)) dx)
              (* (- y (linear-gradient-y1 gradient)) dy))
           length-squared))))

(defmethod gradient-coordinate ((gradient radial-gradient) x y)
  (let ((radius (radial-gradient-radius gradient)))
    (if (zerop radius)
        1.0
        (/ (sqrt
            (+ (expt (- x (radial-gradient-center-x gradient)) 2)
               (expt (- y (radial-gradient-center-y gradient)) 2)))
           radius))))

(defgeneric gradient-render-coordinate (gradient x y)
  (:documentation
   "Return two interpolable shader coordinates and the gradient kind lane."))

(defmethod gradient-render-coordinate ((gradient linear-gradient) x y)
  (values (gradient-coordinate gradient x y) 0.0 0.0))

(defmethod gradient-render-coordinate ((gradient radial-gradient) x y)
  (let ((radius (radial-gradient-radius gradient)))
    (if (zerop radius)
        (values 1.0 0.0 1.0)
        (values (/ (- x (radial-gradient-center-x gradient)) radius)
                (/ (- y (radial-gradient-center-y gradient)) radius)
                1.0))))

(defun interpolate-gradient-ink (gradient coordinate)
  (let ((amount (clamp-unit coordinate)))
    (multiple-value-bind (r1 g1 b1 a1)
        (color-rgba (gradient-start-color gradient))
      (multiple-value-bind (r2 g2 b2 a2)
          (color-rgba (gradient-end-color gradient))
        (let ((color
                (make-rgb-color
                 (+ r1 (* amount (- r2 r1)))
                 (+ g1 (* amount (- g2 g1)))
                 (+ b1 (* amount (- b2 b1)))))
              (alpha (+ a1 (* amount (- a2 a1)))))
          (compose-in color (make-opacity alpha)))))))

(defmethod design-ink ((gradient gradient-design) x y)
  (interpolate-gradient-ink
   gradient (gradient-coordinate gradient x y)))

(shader:define-shader relief-roundrect-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (local-coordinate :vec3 :location 1)
              (half-size-radius :vec3 :location 2)
              (color :vec3 :location 3)
              (relief :vec3 :location 4))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-color :vec4 :location 2)
               (render-height :float :location 3)))
  (let* ((alpha (shader:swizzle position :z)))
    (shader:set-output
     clip-position (shader:vec4 (shader:swizzle position :xy) 0.0 1.0))
    (shader:set-output render-coordinate (shader:swizzle local-coordinate :xy))
    (shader:set-output render-half-size-radius half-size-radius)
    (shader:set-output render-color (shader:vec4 (* color alpha) alpha))
    (shader:set-output render-height (shader:swizzle relief :x))))

(shader:define-shader relief-roundrect-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size-radius :vec3 :location 1)
              (color :vec4 :location 2)
              (height :float :location 3))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((distance
           (luv.analytic:roundrect-signed-distance
            (shader:swizzle render-coordinate :x)
            (shader:swizzle render-coordinate :y)
            (shader:swizzle half-size-radius :x)
            (shader:swizzle half-size-radius :y)
            (shader:swizzle half-size-radius :z)))
         (distance-dx (shader:derivative-x distance))
         (distance-dy (shader:derivative-y distance))
         (normal-length
           (max (sqrt (+ (* distance-dx distance-dx)
                         (* distance-dy distance-dy)))
                (/ 1.0 65536.0)))
         (edge-light
           (* -0.70710678
              (+ (/ distance-dx normal-length)
                 (/ distance-dy normal-length))))
         (absolute-height (abs height))
         (rim-width (max 1.0 absolute-height))
         (rim (shader:clamp (+ 1.0 (/ distance rim-width)) 0.0 1.0))
         (direction (- (* 2.0 (shader:step 0.0 height)) 1.0))
         (strength (shader:clamp (* absolute-height 0.055) 0.0 0.36))
         (shade (+ 1.0 (* edge-light rim direction strength)))
         (coverage
           (luv.analytic:roundrect-coverage
            render-coordinate half-size-radius)))
    (shader:set-output
     color-output
     (* (shader:vec4
         (* (shader:swizzle color :rgb) shade)
         (shader:swizzle color :a))
        coverage))))

(shader:define-shader gradient-roundrect-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (local-coordinate :vec3 :location 1)
              (half-size-radius :vec3 :location 2)
              (paint-coordinate-kind :vec3 :location 3)
              (start-color :vec3 :location 4)
              (end-color :vec3 :location 5)
              (paint-alphas :vec3 :location 6))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-paint-coordinate-kind :vec3 :location 2)
               (render-start-color :vec3 :location 3)
               (render-end-color :vec3 :location 4)
               (render-paint-alphas :vec2 :location 5)))
  (let* ()
    (shader:set-output
     clip-position (shader:vec4 (shader:swizzle position :xy) 0.0 1.0))
    (shader:set-output render-coordinate (shader:swizzle local-coordinate :xy))
    (shader:set-output render-half-size-radius half-size-radius)
    (shader:set-output render-paint-coordinate-kind paint-coordinate-kind)
    (shader:set-output render-start-color start-color)
    (shader:set-output render-end-color end-color)
    (shader:set-output render-paint-alphas (shader:swizzle paint-alphas :xy))))

(shader:define-shader gradient-roundrect-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size-radius :vec3 :location 1)
              (paint-coordinate-kind :vec3 :location 2)
              (start-color :vec3 :location 3)
              (end-color :vec3 :location 4)
              (paint-alphas :vec2 :location 5))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((paint-coordinate (shader:swizzle paint-coordinate-kind :xy))
         (kind (shader:swizzle paint-coordinate-kind :z))
         (linear-coordinate (shader:swizzle paint-coordinate :x))
         (radial-coordinate
           (sqrt (shader:dot paint-coordinate paint-coordinate)))
         (amount
           (shader:clamp
            (shader:mix linear-coordinate radial-coordinate
                     (shader:step 0.5 kind))
            0.0 1.0))
         (alpha
           (shader:mix (shader:swizzle paint-alphas :x)
                    (shader:swizzle paint-alphas :y) amount))
         (rgb (shader:mix start-color end-color amount))
         (coverage
           (shader:mix
            (luv.analytic:roundrect-coverage
             render-coordinate half-size-radius)
            1.0
            (- 1.0
               (shader:step -0.5 (shader:swizzle half-size-radius :z))))))
    (shader:set-output color-output
                    (* (shader:vec4 (* rgb alpha) alpha) coverage))))

(shader:define-shader image-roundrect-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (local-coordinate :vec3 :location 1)
              (half-size-radius :vec3 :location 2)
              (texture-coordinate-opacity :vec3 :location 3))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)
               (render-half-size-radius :vec3 :location 1)
               (render-texture-coordinate :vec2 :location 2)
               (render-opacity :float :location 3)))
  (let* ()
    (shader:set-output
     clip-position (shader:vec4 (shader:swizzle position :xy) 0.0 1.0))
    (shader:set-output render-coordinate (shader:swizzle local-coordinate :xy))
    (shader:set-output render-half-size-radius half-size-radius)
    (shader:set-output
     render-texture-coordinate
     (shader:swizzle texture-coordinate-opacity :xy))
    (shader:set-output
     render-opacity (shader:swizzle texture-coordinate-opacity :z))))

(shader:define-shader image-roundrect-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size-radius :vec3 :location 1)
              (texture-coordinate :vec2 :location 2)
              (opacity :float :location 3))
     :resources
     ((image :texture-2d :binding 0 :sample-transfer :identity)
      (texture-sampler :sampler :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((u (shader:swizzle texture-coordinate :x))
         (v (shader:swizzle texture-coordinate :y))
         (inside-texture
           (* (shader:step 0.0 u) (shader:step u 1.0)
              (shader:step 0.0 v) (shader:step v 1.0)))
         (texel (shader:sample image texture-sampler texture-coordinate))
         (alpha (* (shader:swizzle texel :a) opacity))
         (coverage
           (shader:mix
            (luv.analytic:roundrect-coverage
             render-coordinate half-size-radius)
            1.0
            (- 1.0
               (shader:step -0.5 (shader:swizzle half-size-radius :z)))))
         (covered-alpha (* alpha coverage inside-texture)))
    ;; McCLIM image arrays store straight RGB and alpha. The render target's
    ;; blend contract is premultiplied, exactly like the solid paint path.
    (shader:set-output
     color-output
     (shader:vec4 (* (shader:swizzle texel :rgb) covered-alpha)
               covered-alpha))))
