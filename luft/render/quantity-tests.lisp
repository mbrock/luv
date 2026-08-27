(in-package #:luft.render.tests)

(deftest luft-quantity-names-retain-luft-ownership
  (let ((package (find-package '#:luft.render.quantities)))
    (ok (eq package
            (symbol-package luft.render.quantities:world-position)))
    (ok (eq luft.render.quantities:world-position
            (symbol-value 'luft.render.quantities:world-position)))
    (ok (not (eq luft.render.quantities:world-position :world-position)))
    ;; The full atelier loads both vocabularies.  Their same-spelled meanings
    ;; must remain independently inspectable EQL definitions.
    (ok (eq luft.render.quantities:world-position
            (luv.arithmetic:quantity-definition-name
             (luv.arithmetic:quantity-definition-for
              luft.render.quantities:world-position))))
    (ok (eq :world-position
            (luv.arithmetic:quantity-definition-name
             (luv.arithmetic:quantity-definition-for :world-position))))))

(deftest luft-frame-quantity-kinds-cover-the-declared-boundaries
  (dolist (kind (list luft.render.quantities:spatial-coordinate
                      luft.render.quantities:unit-direction
                      luft.render.quantities:orientation-vector
                      luft.render.quantities:normalized-coordinate
                      luft.render.quantities:relative-color-signal
                      luft.render.quantities:control-signal
                      luft.render.quantities:sample-count))
    (let ((definition (luv.arithmetic:quantity-kind-definition-for kind)))
      (ok definition)
      (ok (eq kind
              (luv.arithmetic:quantity-kind-definition-name definition)))
      (ok (eq :dimensionless
              (luv.arithmetic:quantity-kind-definition-parent definition)))))
  (ok (eq luft.render.quantities:spatial-coordinate
          (luv.arithmetic:unit-definition-quantity-kind
           (luv.arithmetic:unit-definition-for
            luft.render.quantities:cell))))
  (dolist (claim
           (list
            (cons luft.render.quantities:world-position
                  luft.render.quantities:spatial-coordinate)
            (cons luft.render.quantities:world-direction
                  luft.render.quantities:unit-direction)
            (cons luft.render.quantities:world-orientation
                  luft.render.quantities:orientation-vector)
            (cons luft.render.quantities:horizontal-direction
                  luft.render.quantities:unit-direction)
            (cons luft.render.quantities:world-distance
                  luft.render.quantities:spatial-coordinate)
            (cons luft.render.quantities:spatial-scale
                  luft.render.quantities:spatial-coordinate)
            (cons luft.render.quantities:gait-phase :angular-measure)
            (cons luft.render.quantities:spell-flash :proportion)
            (cons luft.render.quantities:texture-coordinate
                  luft.render.quantities:normalized-coordinate)
            (cons luft.render.quantities:temporal-jitter
                  luft.render.quantities:normalized-coordinate)
            (cons luft.render.quantities:texel-extent
                  luft.render.quantities:normalized-coordinate)
            (cons luft.render.quantities:shadow-coordinate
                  luft.render.quantities:normalized-coordinate)
            (cons luft.render.quantities:shadow-bias
                  luft.render.quantities:normalized-coordinate)
            (cons luft.render.quantities:shadow-filter-radius
                  luft.render.quantities:sample-count)
            (cons luft.render.quantities:bevel-proportion :proportion)
            (cons luft.render.quantities:construction-line-strength
                  :proportion)
            (cons luft.render.quantities:inspection-ink-strength
                  :proportion)
            (cons luft.render.quantities:elapsed-time :duration)
            (cons luft.render.quantities:scene-radiance
                  luft.render.quantities:relative-color-signal)
            (cons luft.render.quantities:scene-luminance
                  luft.render.quantities:relative-color-signal)
            (cons luft.render.quantities:exposure
                  luft.render.quantities:control-signal)
            (cons luft.render.quantities:presented-color
                  luft.render.quantities:relative-color-signal)))
    (let ((definition
            (luv.arithmetic:quantity-definition-for (car claim))))
      (ok definition)
      (ok (eq (cdr claim)
              (luv.arithmetic:quantity-definition-kind definition))))))

(deftest luft-frame-quantity-vocabulary-retains-character-and-shape
  (flet ((definition (name)
           (luv.arithmetic:quantity-definition-for name)))
    (let ((position (definition luft.render.quantities:world-position))
          (direction (definition luft.render.quantities:world-direction))
          (orientation
            (definition luft.render.quantities:world-orientation))
          (texture (definition luft.render.quantities:texture-coordinate))
          (jitter (definition luft.render.quantities:temporal-jitter))
          (texel (definition luft.render.quantities:texel-extent))
          (radiance (definition luft.render.quantities:scene-radiance))
          (luminance (definition luft.render.quantities:scene-luminance))
          (exposure (definition luft.render.quantities:exposure))
          (presented (definition luft.render.quantities:presented-color)))
      (ok (eq :point
              (luv.arithmetic:quantity-definition-character position)))
      (ok (equal (list luft.render.quantities:world-x-position
                       luft.render.quantities:world-y-position
                       luft.render.quantities:world-z-position)
                 (luv.arithmetic:quantity-definition-components position)))
      (ok (eq :difference
              (luv.arithmetic:quantity-definition-character direction)))
      (ok (eq :difference
              (luv.arithmetic:quantity-definition-character orientation)))
      (ok (equal (list luft.render.quantities:world-x-orientation
                       luft.render.quantities:world-y-orientation
                       luft.render.quantities:world-z-orientation)
                 (luv.arithmetic:quantity-definition-components orientation)))
      (ok (eq :point
              (luv.arithmetic:quantity-definition-character texture)))
      (ok (eq :difference
              (luv.arithmetic:quantity-definition-character jitter)))
      (ok (eq :absolute
              (luv.arithmetic:quantity-definition-character texel)))
      (ok (equal (list luft.render.quantities:scene-red-radiance
                       luft.render.quantities:scene-green-radiance
                       luft.render.quantities:scene-blue-radiance)
                 (luv.arithmetic:quantity-definition-components radiance)))
      (ok (null (luv.arithmetic:quantity-definition-components luminance)))
      (ok (null (luv.arithmetic:quantity-definition-components exposure)))
      (ok (equal (list luft.render.quantities:presented-red-color
                       luft.render.quantities:presented-green-color
                       luft.render.quantities:presented-blue-color)
                 (luv.arithmetic:quantity-definition-components presented))))))

(deftest luft-frame-quantity-units-form-checked-specifications
  (let ((position
          (luv.arithmetic:make-declared-quantity-specification
           `(:quantity ,luft.render.quantities:world-position
             :unit ,luft.render.quantities:cell
             :tensor-order 1)))
        (elapsed
          (luv.arithmetic:make-declared-quantity-specification
           `(:quantity ,luft.render.quantities:elapsed-time
             :unit :second)))
        (radiance
          (luv.arithmetic:make-declared-quantity-specification
           `(:quantity ,luft.render.quantities:scene-radiance
             :unit :one
             :tensor-order 1))))
    (ok (luv.arithmetic:unit-expression=
         (luv.arithmetic:make-unit-expression luft.render.quantities:cell)
         (luv.arithmetic:quantity-specification-unit position)))
    (ok (eq :point
            (luv.arithmetic:quantity-specification-character position)))
    (ok (= 1 (luv.arithmetic:quantity-specification-tensor-order position)))
    (ok (luv.arithmetic:unit-expression=
         (luv.arithmetic:make-unit-expression :second)
         (luv.arithmetic:quantity-specification-unit elapsed)))
    (ok (eq :absolute
            (luv.arithmetic:quantity-specification-character elapsed)))
    (ok (= 1 (luv.arithmetic:quantity-specification-tensor-order radiance)))))
