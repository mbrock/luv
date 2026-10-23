;;; Semantic quantities shared by Luft's CPU frame writers and GPU arithmetic.
;;;
;;; The public names are ordinary symbols owned by LUFT.RENDER.QUANTITIES,
;;; rather than keywords.  Each is a self-designating constant because the
;;; arithmetic definition macros accept their semantic names as forms.  This
;;; keeps Luft's definitions distinct from the keyword vocabulary installed by
;;; Luvcraft when the full atelier is loaded in the same image.

(in-package #:luft.quantities)

(defconstant spatial-coordinate 'spatial-coordinate)
(defconstant unit-direction 'unit-direction)
(defconstant orientation-vector 'orientation-vector)
(defconstant normalized-coordinate 'normalized-coordinate)
(defconstant relative-color-signal 'relative-color-signal)
(defconstant control-signal 'control-signal)
(defconstant sample-count 'sample-count)
(defconstant cell 'cell)

(defconstant world-position 'world-position)
(defconstant world-x-position 'world-x-position)
(defconstant world-y-position 'world-y-position)
(defconstant world-z-position 'world-z-position)
(defconstant world-velocity 'world-velocity)
(defconstant world-acceleration 'world-acceleration)
(defconstant world-direction 'world-direction)
(defconstant world-x-direction 'world-x-direction)
(defconstant world-y-direction 'world-y-direction)
(defconstant world-z-direction 'world-z-direction)
(defconstant world-orientation 'world-orientation)
(defconstant world-x-orientation 'world-x-orientation)
(defconstant world-y-orientation 'world-y-orientation)
(defconstant world-z-orientation 'world-z-orientation)
(defconstant horizontal-direction 'horizontal-direction)
(defconstant horizontal-x-direction 'horizontal-x-direction)
(defconstant horizontal-y-direction 'horizontal-y-direction)
(defconstant world-distance 'world-distance)
(defconstant spatial-scale 'spatial-scale)
(defconstant gait-phase 'gait-phase)

(defconstant texture-coordinate 'texture-coordinate)
(defconstant texture-u-coordinate 'texture-u-coordinate)
(defconstant texture-v-coordinate 'texture-v-coordinate)
(defconstant temporal-jitter 'temporal-jitter)
(defconstant temporal-x-jitter 'temporal-x-jitter)
(defconstant temporal-y-jitter 'temporal-y-jitter)
(defconstant texel-extent 'texel-extent)
(defconstant texel-width 'texel-width)
(defconstant texel-height 'texel-height)
(defconstant shadow-coordinate 'shadow-coordinate)
(defconstant shadow-u-coordinate 'shadow-u-coordinate)
(defconstant shadow-v-coordinate 'shadow-v-coordinate)
(defconstant shadow-depth-coordinate 'shadow-depth-coordinate)
(defconstant shadow-bias 'shadow-bias)
(defconstant shadow-filter-radius 'shadow-filter-radius)
(defconstant bevel-proportion 'bevel-proportion)
(defconstant construction-line-strength 'construction-line-strength)
(defconstant inspection-ink-strength 'inspection-ink-strength)

(defconstant elapsed-time 'elapsed-time)
(defconstant scene-radiance 'scene-radiance)
(defconstant scene-red-radiance 'scene-red-radiance)
(defconstant scene-green-radiance 'scene-green-radiance)
(defconstant scene-blue-radiance 'scene-blue-radiance)
(defconstant scene-luminance 'scene-luminance)
(defconstant exposure 'exposure)
(defconstant presented-color 'presented-color)
(defconstant presented-red-color 'presented-red-color)
(defconstant presented-green-color 'presented-green-color)
(defconstant presented-blue-color 'presented-blue-color)

;;; Luft's lattice has a continuous coordinate measured in cells.  It is
;;; dimension one, but CELL is deliberately not the identity unit: values in
;;; the lattice are not silently interchangeable with arbitrary scalars.
(define-quantity-kind spatial-coordinate
  :dimension nil :parent :dimensionless)
(define-quantity-kind unit-direction
  :dimension nil :parent :dimensionless)
(define-quantity-kind orientation-vector
  :dimension nil :parent :dimensionless)
(define-quantity-kind normalized-coordinate
  :dimension nil :parent :dimensionless)
(define-quantity-kind relative-color-signal
  :dimension nil :parent :dimensionless)
(define-quantity-kind control-signal
  :dimension nil :parent :dimensionless)
(define-quantity-kind sample-count
  :dimension nil :parent :dimensionless)

(define-unit cell :dimension nil :quantity-kind spatial-coordinate)

(define-quantity world-position :kind spatial-coordinate
  :character :point
  :components (world-x-position world-y-position world-z-position))
;;; Rates are lattice rates, not SI speed/acceleration: a cell has no
;;; implicit conversion to a metre. Keep these kinds private to Luft too.
(define-quantity-kind 'spatial-velocity :dimension ((:duration -1)))
(define-quantity-kind 'spatial-acceleration :dimension ((:duration -2)))
(define-quantity world-velocity :kind 'spatial-velocity)
(define-quantity world-acceleration :kind 'spatial-acceleration)
(define-quantity world-direction :kind unit-direction
  :components (world-x-direction world-y-direction world-z-direction))
(define-quantity world-orientation :kind orientation-vector
  :components
  (world-x-orientation world-y-orientation world-z-orientation))
(define-quantity horizontal-direction :kind unit-direction
  :components (horizontal-x-direction horizontal-y-direction))
(define-quantity world-distance :kind spatial-coordinate
  :non-negative-p t)
(define-quantity spatial-scale :kind spatial-coordinate
  :non-negative-p t)
(define-quantity gait-phase :kind :angular-measure)

;;; Texture coordinates are points.  Jitter is a signed displacement and a
;;; texel extent is a non-negative displacement, even though all three use the
;;; same normalized-coordinate unit ONE at the shader boundary.
(define-quantity texture-coordinate :kind normalized-coordinate
  :character :point
  :components (texture-u-coordinate texture-v-coordinate))
(define-quantity temporal-jitter :kind normalized-coordinate
  :character :difference
  :components (temporal-x-jitter temporal-y-jitter))
(define-quantity texel-extent :kind normalized-coordinate
  :non-negative-p t
  :components (texel-width texel-height))
(define-quantity shadow-coordinate :kind normalized-coordinate
  :character :point
  :components
  (shadow-u-coordinate shadow-v-coordinate shadow-depth-coordinate))
(define-quantity shadow-bias :kind normalized-coordinate
  :non-negative-p t)
(define-quantity shadow-filter-radius :kind sample-count
  :non-negative-p t)
(define-quantity bevel-proportion :kind :proportion
  :non-negative-p t)
(define-quantity construction-line-strength :kind :proportion
  :non-negative-p t)
(define-quantity inspection-ink-strength :kind :proportion
  :non-negative-p t)

;;; Effect clocks are elapsed durations.  Luft's HDR values are a named,
;;; relative scene-linear signal rather than a claim of SI radiometry.  The
;;; presented colour remains distinct because a tone-mapped display signal is
;;; not valid input to scene-linear lighting even though both use unit ONE.
(define-quantity elapsed-time :kind :duration
  :non-negative-p t)
(define-quantity scene-radiance :kind relative-color-signal
  :non-negative-p t
  :components
  (scene-red-radiance scene-green-radiance scene-blue-radiance))
(define-quantity scene-luminance :kind relative-color-signal
  :non-negative-p t)
(define-quantity exposure :kind control-signal
  :non-negative-p t)
(define-quantity presented-color :kind relative-color-signal
  :non-negative-p t
  :components
  (presented-red-color presented-green-color presented-blue-color))

;;; A pigment is not radiance. Emission is a relative scene-linear gain;
;;; neither the RGB8 tones nor the emission/4 byte claim SI radiometry.
(defconstant surface-tone 'surface-tone)
(defconstant surface-emission 'surface-emission)
(defconstant surface-relief 'surface-relief)
(defconstant surface-roughness 'surface-roughness)
(defconstant surface-wear 'surface-wear)
(defconstant pattern-courses 'pattern-courses)
(defconstant pattern-aspect 'pattern-aspect)
(defconstant mortar-width 'mortar-width)
(defconstant tone-variance 'tone-variance)
(define-quantity surface-tone :kind relative-color-signal
  :non-negative-p t)
(define-quantity surface-emission :kind relative-color-signal
  :non-negative-p t)
(define-quantity surface-relief :kind :proportion :non-negative-p t)
(define-quantity surface-roughness :kind :proportion :non-negative-p t)
(define-quantity surface-wear :kind :proportion)
(define-quantity pattern-courses :kind sample-count :non-negative-p t)
(define-quantity pattern-aspect :kind :proportion :non-negative-p t)
(define-quantity mortar-width :kind :proportion :non-negative-p t)
(define-quantity tone-variance :kind :proportion :non-negative-p t)

(defconstant monotonic-time 'monotonic-time)
(defconstant simulation-time 'simulation-time)
(defconstant time-of-day 'time-of-day)
(define-quantity monotonic-time :kind :duration :character :point)
(define-quantity simulation-time :kind :duration :character :point)
(define-quantity time-of-day :kind :duration :character :point)

(defconstant ember-count 'ember-count)
(define-quantity ember-count :kind sample-count :non-negative-p t)

(defconstant fixture-position 'fixture-position)
(define-quantity fixture-position :kind spatial-coordinate :character :point)

;;; Encoded descriptor readings have a scale, not just a bit width.
(define-unit 'descriptor-unorm8 :reference :one :magnitude 1/255
  :quantity-kind :dimensionless)
(define-unit 'descriptor-unorm4 :reference :one :magnitude 1/15
  :quantity-kind :dimensionless)
(define-unit 'descriptor-half-row :reference :one :magnitude 1/2
  :quantity-kind :dimensionless)
(define-unit 'descriptor-emission-byte :reference :one :magnitude 4/255
  :quantity-kind :dimensionless)
(define-unit 'internal-time-tick :reference :second
  :magnitude (/ internal-time-units-per-second) :quantity-kind :duration)

(in-package #:luft.terrain)

;;; The fixed descriptor ABI, in bits within each u32. The code fields and
;;; spare bits are categorical representation, deliberately without quantities.
;;; (name offset width decoded-quantity decoding-divisor).
(defparameter *terrain-descriptor-fields*
  '((:red 0 8 quantities::surface-tone 255.0)
    (:green 8 8 quantities::surface-tone 255.0)
    (:blue 16 8 quantities::surface-tone 255.0)
    (:relief-code 0 4 nil 1.0)
    (:strength 4 4 quantities::surface-relief 15.0)
    (:roughness 8 8 quantities::surface-roughness 255.0)
    (:wear 16 8 quantities::surface-wear 255.0)
    (:pattern-code 0 4 nil 1.0)
    (:courses 4 4 quantities::pattern-courses 1.0)
    (:aspect 8 4 quantities::pattern-aspect 2.0)
    (:mortar-width 12 8 quantities::mortar-width 255.0)
    (:variance 20 8 quantities::tone-variance 255.0)
    (:emission 24 8 quantities::surface-emission 63.75)))

(defun terrain-descriptor-field (name)
  (or (assoc name *terrain-descriptor-fields*)
      (error "Unknown terrain descriptor field ~S." name)))

(defun terrain-descriptor-divisor (name)
  (fifth (terrain-descriptor-field name)))

(defun terrain-descriptor-byte (name)
  (destructuring-bind (name offset width quantity divisor)
      (terrain-descriptor-field name)
    (declare (ignore name quantity divisor))
    `(byte ,width ,offset)))

(defun terrain-descriptor-bit-layout ()
  "The two uvec4 rows, measured in bits rather than decoded float lanes."
  (let ((projections nil))
    (loop for word-fields in
          '((:red :green :blue) (:red :green :blue)
            (:red :green :blue) (:strength :roughness :wear)
            (:courses :aspect :mortar-width :variance)
            (:red :green :blue) (:red :green :blue :emission))
          for word from 0 do
      (dolist (name word-fields)
        (destructuring-bind (name offset width quantity divisor)
            (terrain-descriptor-field name)
          (declare (ignore name))
          (push (math:make-quantity-projection
                 (loop for bit below width collect (+ (* word 32) offset bit))
                 (math:make-declared-quantity-specification
                  (list :quantity quantity :unit
                        (ecase divisor
                          (1.0 :one)
                          (2.0 'quantities::descriptor-half-row)
                          (15.0 'quantities::descriptor-unorm4)
                          (255.0 'quantities::descriptor-unorm8)
                          (63.75 'quantities::descriptor-emission-byte)))))
                projections))))
    (math:make-quantity-layout 256 (nreverse projections))))

(defmethod math:value-declaration-for
    ((name (eql 'compile-terrain-material-descriptors)))
  (declare (ignore name))
  (load-time-value
   (math:make-represented-value-declaration
    :representation-type '(simple-array (unsigned-byte 32) (*))
    :quantity-layout
    (math:make-repeated-quantity-layout (terrain-descriptor-bit-layout))
    :source-form '(compile-terrain-material-descriptors :stride-bits 256))))

(defun terrain-varying-components (name)
  "Decoded products at the stage boundary; categorical lanes stay raw."
  (ecase name
    (:light
     '((:x :quantity luft::block-light-proportion :unit :one)
       (:y :quantity luft::block-light-proportion :unit :one)
       (:z :quantity luft::block-light-proportion :unit :one)
       (:w :quantity luft::sky-light-proportion :unit :one)))
    (:surface
     '((:y :quantity quantities::surface-relief :unit :one)
       (:z :quantity quantities::surface-roughness :unit :one)
       (:w :quantity quantities::surface-wear :unit :one)))
    (:pattern
     '((:y :quantity quantities::pattern-courses :unit :one)
       (:z :quantity quantities::pattern-aspect :unit :one)
       (:w :quantity quantities::mortar-width :unit :one)))
    (:accent
     '((:xyz :quantity quantities::surface-tone :unit :one)
       (:w :quantity quantities::surface-emission :unit :one)))
    (:mortar
     '((:xyz :quantity quantities::surface-tone :unit :one)
       (:w :quantity quantities::tone-variance :unit :one)))))

(defun make-terrain-float-product-declaration (name stride fields)
  "Describe repeated float rows while leaving unlisted categorical lanes raw."
  (math:make-represented-value-declaration
   :representation-type '(simple-array single-float (*))
   :quantity-layout
   (math:make-repeated-quantity-layout
    (math:make-quantity-layout
     stride
     (loop for (positions . options) in fields collect
       (math:make-quantity-projection
        positions
        (math:make-declared-quantity-specification
         options :default-tensor-order (if (= 1 (length positions)) 0 1))))))
   :source-form (list name :stride stride :fields fields)))

(defmethod math:value-declaration-for ((name (eql 'pack-fixture-bodies)))
  (load-time-value
   (make-terrain-float-product-declaration
    'pack-fixture-bodies 16
    '(((0 1 2) :quantity quantities:world-position :unit quantities:cell)
      ((4 5 6) :quantity quantities::surface-tone :unit :one)
      ((7) :quantity :angle :unit :radian)
      ((8 9 10) :quantity quantities::surface-tone :unit :one)
      ((12 13 14) :quantity luft::block-light-proportion :unit :one)))))
