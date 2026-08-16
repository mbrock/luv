;;; Paint designs evaluated by the direct McCLIM GPU backend.

(in-package #:mcluv)

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

(spv:define-shader gradient-roundrect-vertex-specification
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
    (spv:set-output
     clip-position (spv:vec4 (spv:swizzle position :xy) 0.0 1.0))
    (spv:set-output render-coordinate (spv:swizzle local-coordinate :xy))
    (spv:set-output render-half-size-radius half-size-radius)
    (spv:set-output render-paint-coordinate-kind paint-coordinate-kind)
    (spv:set-output render-start-color start-color)
    (spv:set-output render-end-color end-color)
    (spv:set-output render-paint-alphas (spv:swizzle paint-alphas :xy))))

(spv:define-shader gradient-roundrect-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size-radius :vec3 :location 1)
              (paint-coordinate-kind :vec3 :location 2)
              (start-color :vec3 :location 3)
              (end-color :vec3 :location 4)
              (paint-alphas :vec2 :location 5))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((paint-coordinate (spv:swizzle paint-coordinate-kind :xy))
         (kind (spv:swizzle paint-coordinate-kind :z))
         (linear-coordinate (spv:swizzle paint-coordinate :x))
         (radial-coordinate
           (sqrt (spv:dot paint-coordinate paint-coordinate)))
         (amount
           (spv:clamp
            (spv:mix linear-coordinate radial-coordinate
                     (spv:step 0.5 kind))
            0.0 1.0))
         (alpha
           (spv:mix (spv:swizzle paint-alphas :x)
                    (spv:swizzle paint-alphas :y) amount))
         (rgb (spv:mix start-color end-color amount))
         (coverage
           (spv:mix
            (luv.analytic:roundrect-coverage
             render-coordinate half-size-radius)
            1.0
            (- 1.0
               (spv:step -0.5 (spv:swizzle half-size-radius :z))))))
    (spv:set-output color-output
                    (* (spv:vec4 (* rgb alpha) alpha) coverage))))

(spv:define-shader image-roundrect-vertex-specification
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
    (spv:set-output
     clip-position (spv:vec4 (spv:swizzle position :xy) 0.0 1.0))
    (spv:set-output render-coordinate (spv:swizzle local-coordinate :xy))
    (spv:set-output render-half-size-radius half-size-radius)
    (spv:set-output
     render-texture-coordinate
     (spv:swizzle texture-coordinate-opacity :xy))
    (spv:set-output
     render-opacity (spv:swizzle texture-coordinate-opacity :z))))

(spv:define-shader image-roundrect-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0)
              (half-size-radius :vec3 :location 1)
              (texture-coordinate :vec2 :location 2)
              (opacity :float :location 3))
     :resources
     ((image :texture-2d :binding 0 :sample-transfer :identity)
      (sampler :sampler :binding 1))
     :outputs ((color-output :vec4 :location 0)))
  (let* ((u (spv:swizzle texture-coordinate :x))
         (v (spv:swizzle texture-coordinate :y))
         (inside-texture
           (* (spv:step 0.0 u) (spv:step u 1.0)
              (spv:step 0.0 v) (spv:step v 1.0)))
         (texel (spv:sample image sampler texture-coordinate))
         (alpha (* (spv:swizzle texel :a) opacity))
         (coverage
           (spv:mix
            (luv.analytic:roundrect-coverage
             render-coordinate half-size-radius)
            1.0
            (- 1.0
               (spv:step -0.5 (spv:swizzle half-size-radius :z)))))
         (covered-alpha (* alpha coverage inside-texture)))
    ;; McCLIM image arrays store straight RGB and alpha. The render target's
    ;; blend contract is premultiplied, exactly like the solid paint path.
    (spv:set-output
     color-output
     (spv:vec4 (* (spv:swizzle texel :rgb) covered-alpha)
               covered-alpha))))
