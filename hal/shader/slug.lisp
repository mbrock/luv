;;; A deliberately fixed-data proof of Slug's quadratic outline calculation.
;;;
;;; This proves the mathematical pixel stage in luv's shader language.  It is
;;; not yet the texture-banded, variable-length font renderer described by the
;;; reference shaders; that boundary is recorded in #OWR8OZ.

(in-package #:luv.slug)

(defconstant +slug-root-epsilon+ (/ 1.0 65536.0))

(defun slug-root-eligibility (y1 y2 y3)
  "Return the two Slug root-eligibility bits as numeric masks.

Only strict positivity matters.  The arithmetic form is Table 1 of Lengyel's
2017 paper without an integer lookup, and is the form generated into the proof
pixel shader.  #S2F8SA"
  (let ((s1 (if (plusp y1) 1 0))
        (s2 (if (plusp y2) 1 0))
        (s3 (if (plusp y3) 1 0)))
    (values (+ (* s1 (- 1 (* s2 s3)))
               (* (- 1 s1) s2 (- 1 s3)))
            (+ (* s3 (- 1 (* s1 s2)))
               (* (- 1 s3) s2 (- 1 s1))))))

(defun slug-root-eligibility-forms (s1 s2 s3)
  (values `(+ (* ,s1 (- 1.0 (* ,s2 ,s3)))
              (* (- 1.0 ,s1) ,s2 (- 1.0 ,s3)))
          `(+ (* ,s3 (- 1.0 (* ,s1 ,s2)))
              (* (- 1.0 ,s3) ,s2 (- 1.0 ,s1)))))

(defun slug-local (index axis part)
  (make-symbol
   (format nil "SLUG-~D-~A-~A" index (string-upcase (string axis))
           (string-upcase (string part)))))

(defun slug-point-form (point source-form)
  (unless (and (listp point) (= (length point) 2) (every #'realp point))
    (error 'spv:shader-language-error
           :form source-form :reason :invalid-slug-control-point
           :details point))
  `(spv:vec2 ,(first point) ,(second point)))

(defun slug-curve-bindings (index coordinate curve source-form)
  (unless (and (listp curve) (= (length curve) 3))
    (error 'spv:shader-language-error
           :form source-form :reason :invalid-slug-quadratic
           :details curve))
  (let ((p1 (slug-local index :curve :p1))
        (p2 (slug-local index :curve :p2))
        (p3 (slug-local index :curve :p3))
        (a (slug-local index :curve :a))
        (b (slug-local index :curve :b)))
    (values
     `((,p1 (- ,(slug-point-form (first curve) source-form) ,coordinate))
       (,p2 (- ,(slug-point-form (second curve) source-form) ,coordinate))
       (,p3 (- ,(slug-point-form (third curve) source-form) ,coordinate))
       (,a (+ (- ,p1 (* ,p2 2.0)) ,p3))
       (,b (- ,p1 ,p2)))
     (list :p1 p1 :p2 p2 :p3 p3 :a a :b b))))

(defun slug-axis-bindings (index axis curve pixels-per-em)
  (let* ((other-axis (ecase axis (:x :y) (:y :x)))
         (prefix (ecase axis (:x :vertical) (:y :horizontal)))
         (p1 (getf curve :p1))
         (p2 (getf curve :p2))
         (p3 (getf curve :p3))
         (a (getf curve :a))
         (b (getf curve :b))
         (pa (slug-local index prefix :p1-axis))
         (p2a (slug-local index prefix :p2-axis))
         (p3a (slug-local index prefix :p3-axis))
         (aa (slug-local index prefix :a-axis))
         (ba (slug-local index prefix :b-axis))
         (linear (slug-local index prefix :linear))
         (safe-a (slug-local index prefix :safe-a))
         (safe-b (slug-local index prefix :safe-b))
         (discriminant (slug-local index prefix :discriminant))
         (root-distance (slug-local index prefix :root-distance))
         (quadratic-t1 (slug-local index prefix :quadratic-t1))
         (quadratic-t2 (slug-local index prefix :quadratic-t2))
         (linear-t (slug-local index prefix :linear-t))
         (t1 (slug-local index prefix :t1))
         (t2 (slug-local index prefix :t2))
         (s1 (slug-local index prefix :s1))
         (s2 (slug-local index prefix :s2))
         (s3 (slug-local index prefix :s3))
         (eligible1 (slug-local index prefix :eligible1))
         (eligible2 (slug-local index prefix :eligible2))
         (other-p1 (slug-local index prefix :other-p1))
         (other-a (slug-local index prefix :other-a))
         (other-b (slug-local index prefix :other-b))
         (crossing1 (slug-local index prefix :crossing1))
         (crossing2 (slug-local index prefix :crossing2))
         (scaled1 (slug-local index prefix :scaled1))
         (scaled2 (slug-local index prefix :scaled2))
         (coverage1 (slug-local index prefix :coverage1))
         (coverage2 (slug-local index prefix :coverage2))
         (weight1 (slug-local index prefix :weight1))
         (weight2 (slug-local index prefix :weight2)))
    (multiple-value-bind (eligibility1-form eligibility2-form)
        (slug-root-eligibility-forms s1 s2 s3)
      (values
       `((,pa (spv:swizzle ,p1 ,axis))
         (,p2a (spv:swizzle ,p2 ,axis))
         (,p3a (spv:swizzle ,p3 ,axis))
         (,aa (spv:swizzle ,a ,axis))
         (,ba (spv:swizzle ,b ,axis))
         (,linear (- 1.0 (spv:step ,+slug-root-epsilon+ (abs ,aa))))
         (,safe-a (spv:mix ,aa 1.0 ,linear))
         (,safe-b
          (spv:mix ,ba 1.0
                   (- 1.0
                      (spv:step ,+slug-root-epsilon+ (abs ,ba)))))
         (,discriminant (max (- (* ,ba ,ba) (* ,aa ,pa)) 0.0))
         (,root-distance (sqrt ,discriminant))
         (,quadratic-t1 (/ (- ,ba ,root-distance) ,safe-a))
         (,quadratic-t2 (/ (+ ,ba ,root-distance) ,safe-a))
         (,linear-t (/ (* ,pa 0.5) ,safe-b))
         (,t1 (spv:mix ,quadratic-t1 ,linear-t ,linear))
         (,t2 (spv:mix ,quadratic-t2 ,linear-t ,linear))
         ;; FSign makes zero exactly "not positive", matching the eligibility
         ;; table without epsilon classification at shared control points.
         (,s1 (max (signum ,pa) 0.0))
         (,s2 (max (signum ,p2a) 0.0))
         (,s3 (max (signum ,p3a) 0.0))
         (,eligible1 ,eligibility1-form)
         (,eligible2 ,eligibility2-form)
         (,other-p1 (spv:swizzle ,p1 ,other-axis))
         (,other-a (spv:swizzle ,a ,other-axis))
         (,other-b (spv:swizzle ,b ,other-axis))
         (,crossing1
          (+ (* (- (* ,other-a ,t1) (* ,other-b 2.0)) ,t1)
             ,other-p1))
         (,crossing2
          (+ (* (- (* ,other-a ,t2) (* ,other-b 2.0)) ,t2)
             ,other-p1))
         (,scaled1
          (* ,crossing1 (spv:swizzle ,pixels-per-em ,other-axis)))
         (,scaled2
          (* ,crossing2 (spv:swizzle ,pixels-per-em ,other-axis)))
         (,coverage1
          (* ,eligible1 (spv:clamp (+ ,scaled1 0.5) 0.0 1.0)))
         (,coverage2
          (* ,eligible2 (spv:clamp (+ ,scaled2 0.5) 0.0 1.0)))
         (,weight1
          (* ,eligible1
             (spv:clamp (- 1.0 (* (abs ,scaled1) 2.0)) 0.0 1.0)))
         (,weight2
          (* ,eligible2
             (spv:clamp (- 1.0 (* (abs ,scaled2) 2.0)) 0.0 1.0))))
       (list :coverage1 coverage1 :coverage2 coverage2
             :weight1 weight1 :weight2 weight2)))))

(defun slug-outline-form
    (coordinate pixels-per-em color output curves source-form)
  (unless curves
    (error 'spv:shader-language-error
           :form source-form :reason :empty-slug-outline))
  (let ((bindings nil)
        (curve-values nil)
        (horizontal-values nil)
        (vertical-values nil))
    (loop for curve in curves
          for index from 0
          do (multiple-value-bind (curve-bindings curve-value)
                 (slug-curve-bindings index coordinate curve source-form)
               (setf bindings (nconc bindings curve-bindings))
               (push curve-value curve-values)))
    (setf curve-values (nreverse curve-values))
    (loop for curve-value in curve-values
          for index from 0
          do (multiple-value-bind (axis-bindings axis-value)
                 (slug-axis-bindings index :y curve-value pixels-per-em)
               (setf bindings (nconc bindings axis-bindings))
               (push axis-value horizontal-values))
             (multiple-value-bind (axis-bindings axis-value)
                 (slug-axis-bindings index :x curve-value pixels-per-em)
               (setf bindings (nconc bindings axis-bindings))
               (push axis-value vertical-values)))
    (setf horizontal-values (nreverse horizontal-values)
          vertical-values (nreverse vertical-values))
    (let ((xcov (make-symbol "SLUG-X-COVERAGE"))
          (ycov (make-symbol "SLUG-Y-COVERAGE"))
          (xweight (make-symbol "SLUG-X-WEIGHT"))
          (yweight (make-symbol "SLUG-Y-WEIGHT"))
          (coverage (make-symbol "SLUG-COVERAGE")))
      `(let*
           (,@bindings
            (,xcov
             (+ 0.0
                ,@(loop for value in horizontal-values
                        collect `(- ,(getf value :coverage1)
                                    ,(getf value :coverage2)))))
            (,ycov
             (+ 0.0
                ,@(loop for value in vertical-values
                        collect `(- ,(getf value :coverage2)
                                    ,(getf value :coverage1)))))
            (,xweight
             (max 0.0
                  ,@(loop for value in horizontal-values
                          append (list (getf value :weight1)
                                       (getf value :weight2)))))
            (,yweight
             (max 0.0
                  ,@(loop for value in vertical-values
                          append (list (getf value :weight1)
                                       (getf value :weight2)))))
            (,coverage
             (spv:clamp
              (max
               (/ (abs (+ (* ,xcov ,xweight) (* ,ycov ,yweight)))
                  (max (+ ,xweight ,yweight) ,+slug-root-epsilon+))
               (min (abs ,xcov) (abs ,ycov)))
              0.0 1.0)))
         (spv:set-output ,output (* ,color ,coverage))))))

(spv:define-shader-abstraction slug-quadratic-outline
    (coordinate pixels-per-em color output &rest curves)
  "Render one compile-time quadratic contour with Slug's two-ray pixel math.

The fixed curve list makes this an atelier proof, not the band-texture font
renderer.  Each curve is ((x1 y1) (x2 y2) (x3 y3)).  #OWR8OZ"
  (slug-outline-form
   coordinate pixels-per-em color output curves
   (list* 'slug-quadratic-outline
          coordinate pixels-per-em color output curves)))

(spv:define-shader slug-bezier-vertex-specification
    (:stage :vertex
     :inputs ((position :vec3 :location 0)
              (outline-coordinate :vec3 :location 1))
     :outputs ((clip-position :vec4 :built-in :position)
               (render-coordinate :vec2 :location 0)))
  (let* ((clip (spv:vec4 (spv:swizzle position :xy) 0.0 1.0)))
    (spv:set-output clip-position clip)
    (spv:set-output render-coordinate
                    (spv:swizzle outline-coordinate :xy))))

(spv:define-shader slug-bezier-fragment-specification
    (:stage :fragment
     :inputs ((render-coordinate :vec2 :location 0))
     :outputs ((color-output :vec4 :location 0)))
  ;; The quad spans 80 percent of a 256-pixel proof target, hence 204.8
  ;; pixels per em.  Keeping this literal is the boundary of the fixed proof.
  (slug-quadratic-outline
   render-coordinate (spv:vec2 204.8 204.8)
   (spv:vec4 0.96 0.32 0.48 1.0) color-output
   ((0.50 0.08) (0.08 0.38) (0.14 0.70))
   ((0.14 0.70) (0.18 0.98) (0.50 0.74))
   ((0.50 0.74) (0.82 0.98) (0.86 0.70))
   ((0.86 0.70) (0.92 0.38) (0.50 0.08))))
