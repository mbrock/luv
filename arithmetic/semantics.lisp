;;; Semantic arithmetic independent of any one execution backend.
;;;
;;; Specifications are compile-time boundary objects.  Runtime scalar and
;;; vector lanes remain ordinary unboxed data; an arithmetic graph, field
;;; operation, or shader compiler asks this protocol whether its operations
;;; are meaningful before choosing a backend representation.

(in-package #:luv.arithmetic)

(defclass factor-product ()
  ((factors
    :initarg :factors
    :reader factor-product-factors))
  (:documentation
   "Internal canonical representation shared by dimensions and exact units."))

(defclass dimension (factor-product) ()
  (:documentation
   "A canonical product of symbolic base dimensions raised to rational powers."))

(defun dimension-factors (dimension)
  (factor-product-factors dimension))

(defun factor-name (factor)
  (let ((symbol (car factor)))
    (format nil "~A::~A"
            (or (and (symbol-package symbol)
                     (package-name (symbol-package symbol)))
                "")
            (symbol-name symbol))))

(defun canonical-factors (factors)
  (let ((powers (make-hash-table :test #'eq)))
    (dolist (factor factors)
      (let ((base (car factor))
            (exponent (if (and (consp (cdr factor))
                               (null (cddr factor)))
                          (second factor)
                          (cdr factor))))
        (unless (and (symbolp base) (rationalp exponent))
          (error "Invalid symbolic factor ~S." factor))
        (incf (gethash base powers 0) exponent)))
    (sort (loop for base being the hash-keys of powers
                  using (hash-value exponent)
                unless (zerop exponent)
                  collect (cons base exponent))
          #'string< :key #'factor-name)))

(defun make-dimension (&optional designator)
  "Return a canonical dimension from NIL, one base symbol, or factor pairs.

Each factor pair has the form (BASE EXPONENT), where EXPONENT is rational."
  (etypecase designator
    (null (make-instance 'dimension :factors nil))
    (dimension designator)
    (symbol (make-instance 'dimension
                           :factors (list (cons designator 1))))
    (list (make-instance 'dimension
                         :factors (canonical-factors designator)))))

(defmethod print-object ((product factor-product) stream)
  (print-unreadable-object (product stream :type t)
    (if (factor-product-factors product)
        (format stream "~{~A~^ ~}"
                (mapcar (lambda (factor)
                          (if (= (cdr factor) 1)
                              (car factor)
                              (format nil "~A^~A" (car factor) (cdr factor))))
                        (factor-product-factors product)))
        (write-string "1" stream))))

(defun factor-product= (left right)
  (equal (factor-product-factors left)
         (factor-product-factors right)))

(defun combined-factors (left right)
  (append (factor-product-factors left)
          (factor-product-factors right)))

(defun scaled-factors (product exponent)
  (mapcar (lambda (factor)
            (list (car factor) (* exponent (cdr factor))))
          (factor-product-factors product)))

(defun dimension= (left right)
  (factor-product= (make-dimension left) (make-dimension right)))

(defun dimensionless-p (dimension)
  (null (dimension-factors (make-dimension dimension))))

(defun multiply-dimensions (left right)
  (make-dimension (combined-factors (make-dimension left)
                                    (make-dimension right))))

(defun exponentiate-dimension (dimension exponent)
  (unless (rationalp exponent)
    (error "A dimension exponent must be rational, not ~S." exponent))
  (make-dimension (scaled-factors (make-dimension dimension) exponent)))

(defun divide-dimensions (numerator denominator)
  (multiply-dimensions numerator
                       (exponentiate-dimension denominator -1)))

(define-condition undefined-unit (error)
  ((name
    :initarg :name
    :reader undefined-unit-name))
  (:report
   (lambda (condition stream)
     (format stream "No semantic unit definition exists for ~S."
             (undefined-unit-name condition)))))

(defclass unit-definition ()
  ((name
    :initarg :name
    :reader unit-definition-name)
   (dimension
    :initarg :dimension
    :reader unit-definition-dimension)
   (magnitude
    :initarg :magnitude
    :reader unit-definition-magnitude)
   (basis
    :initarg :basis
    :reader unit-definition-basis)
   (identity-p
    :initarg :identity-p
    :initform nil
    :reader unit-definition-identity-p)
   (quantity-kind
    :initarg :quantity-kind
    :reader unit-definition-quantity-kind))
  (:documentation
   "A named linear unit with a dimension, canonical basis, scale, and kind."))

(defgeneric unit-definition-for (name)
  (:documentation
   "Return the semantic definition of unit NAME, or signal UNDEFINED-UNIT."))

(defmethod unit-definition-for (name)
  (error 'undefined-unit :name name))

(defclass unit-expression (factor-product) ()
  (:documentation
   "A canonical product of defined named units."))

(defun unit-expression-factors (unit)
  (factor-product-factors unit))

(defun raw-unit-expression (factors)
  (make-instance 'unit-expression
                 :factors (canonical-factors factors)))

(defun make-unit-expression (&optional designator)
  "Return a canonical unit expression from NIL, a defined unit, or factors.

Every symbolic factor must have a semantic unit definition.  Exact unit
identity remains visible here; conversions are requested separately."
  (etypecase designator
    (null (make-instance 'unit-expression :factors nil))
    (unit-expression designator)
    (symbol
     (let ((definition (unit-definition-for designator)))
       (if (unit-definition-identity-p definition)
           (raw-unit-expression nil)
           (raw-unit-expression (list (cons designator 1))))))
    (list
     (dolist (factor designator)
       (unit-definition-for (car factor)))
     (raw-unit-expression designator))))

(defun unit-expression= (left right)
  (factor-product= (make-unit-expression left) (make-unit-expression right)))

(defun unitless-p (unit)
  (null (unit-expression-factors (make-unit-expression unit))))

(defun map-unit-expression-factors (function unit initial-value)
  (reduce
   (lambda (result factor)
     (funcall function result
              (unit-definition-for (car factor))
              (cdr factor)))
   (unit-expression-factors (make-unit-expression unit))
   :initial-value initial-value))

(defun unit-expression-dimension (unit)
  "Return the physical dimension implied by UNIT's definitions."
  (map-unit-expression-factors
   (lambda (dimension definition exponent)
     (multiply-dimensions
      dimension
      (exponentiate-dimension
       (unit-definition-dimension definition) exponent)))
   unit (make-dimension)))

(defun unit-expression-magnitude (unit)
  "Return UNIT's scale relative to its canonical basis."
  (map-unit-expression-factors
   (lambda (magnitude definition exponent)
     (* magnitude (expt (unit-definition-magnitude definition) exponent)))
   unit 1))

(defun unit-expression-basis (unit)
  "Return UNIT expressed only in canonical basis units."
  (map-unit-expression-factors
   (lambda (basis definition exponent)
     (multiply-unit-expressions
      basis
      (exponentiate-unit-expression
       (unit-definition-basis definition) exponent)))
   unit (raw-unit-expression nil)))

(defun unit-conversion-factor (source target)
  "Return the numerical factor converting SOURCE values into TARGET values."
  (let ((source (make-unit-expression source))
        (target (make-unit-expression target)))
    (unless (and (dimension=
                  (unit-expression-dimension source)
                  (unit-expression-dimension target))
                 (unit-expression=
                  (unit-expression-basis source)
                  (unit-expression-basis target)))
      (quantity-operation-error 'convert-unit (list source target)
                                :incompatible-units))
    (/ (unit-expression-magnitude source)
       (unit-expression-magnitude target))))

(defun multiply-unit-expressions (left right)
  (make-unit-expression
   (combined-factors (make-unit-expression left)
                     (make-unit-expression right))))

(defun exponentiate-unit-expression (unit exponent)
  (unless (rationalp exponent)
    (error "A unit exponent must be rational, not ~S." exponent))
  (make-unit-expression
   (scaled-factors (make-unit-expression unit) exponent)))

(defun divide-unit-expressions (numerator denominator)
  (multiply-unit-expressions
   numerator (exponentiate-unit-expression denominator -1)))

(defclass quantity-kind-definition ()
  ((name
    :initarg :name
    :reader quantity-kind-definition-name)
   (parent
    :initarg :parent
    :initform nil
    :reader quantity-kind-definition-parent)
   (dimension
    :initarg :dimension
    :reader quantity-kind-definition-dimension))
  (:documentation
   "One node in the semantic hierarchy of kinds sharing physical dimensions."))

(defgeneric quantity-kind-definition-for (name)
  (:documentation "Return the inspectable definition of quantity kind NAME."))

(defmethod quantity-kind-definition-for (name)
  (declare (ignore name))
  nil)

(defmacro define-semantic-definition (function name value-form)
  "Define NAME as one inspectable value behind an EQL-specialized FUNCTION."
  (let ((argument (gensym "NAME")))
    `(defmethod ,function ((,argument (eql ,name)))
       (declare (ignore ,argument))
       (load-time-value ,value-form))))

(defun make-quantity-kind-definition (name dimension parent)
  (unless (symbolp name)
    (error "A quantity kind needs a symbolic name, not ~S." name))
  (let ((dimension (make-dimension dimension))
        (parent-definition
          (and parent (quantity-kind-definition-for parent))))
    (when (and parent
               (or (null parent-definition)
                   (not (dimension=
                         dimension
                         (quantity-kind-definition-dimension
                          parent-definition)))))
      (error "Quantity kind ~S needs a defined parent of the same dimension, not ~S."
             name parent))
    (make-instance 'quantity-kind-definition
                   :name name :dimension dimension :parent parent)))

(defmacro define-quantity-kind (name &key dimension parent)
  "Define a semantic quantity kind through an inspectable EQL method."
  `(define-semantic-definition quantity-kind-definition-for ,name
     (make-quantity-kind-definition ,name ',dimension ,parent)))

(defun quantity-kind-subkind-p (kind ancestor)
  "Whether KIND is ANCESTOR or reaches it through declared parent kinds."
  (loop with seen = nil
        for name = kind then (quantity-kind-definition-parent definition)
        for definition = (and name (quantity-kind-definition-for name))
        while name
        when (eq name ancestor) return t
        do (when (or (null definition) (member name seen))
             (return nil))
           (push name seen)
        finally (return nil)))

(deftype quantity-character ()
  "The affine role of a value: a location, a non-negative amount from a true
zero, or a signed difference between two of either."
  '(member :point :absolute :difference))

(defclass quantity-definition ()
  ((name
    :initarg :name
    :reader quantity-definition-name)
   (kind
    :initarg :kind
    :reader quantity-definition-kind)
   (components
    :initarg :components
    :initform nil
    :reader quantity-definition-components)
   ;; Non-negativity is a declared fact about the named quantity, never derived
   ;; from an equation: a defining equation captures dimension, not sign
   ;; domain.  It makes the quantity's default character :absolute and lets a
   ;; later interpretation promise a non-negative amount.  No lowering emits a
   ;; check for it.
   (non-negative-p
    :initarg :non-negative-p
    :initform nil
    :reader quantity-definition-non-negative-p)
   (character
    :initarg :character
    :initform :difference
    :reader quantity-definition-character))
  (:documentation
   "A domain quantity name, its unit kind, homogeneous components, and the
affine character its specifications take unless a use site says otherwise."))

(defgeneric quantity-definition-for (name)
  (:documentation "Return the inspectable definition of quantity NAME, or NIL."))

(defmethod quantity-definition-for (name)
  (declare (ignore name))
  nil)

(defun make-quantity-definition
    (name kind &key components non-negative-p (character nil character-supplied-p))
  (unless (and (symbolp name) (symbolp kind)
               (quantity-kind-definition-for kind))
    (error "Quantity ~S needs a defined symbolic kind, not ~S." name kind))
  (unless (and (every #'symbolp components)
               (= (length components) (length (remove-duplicates components)))
               (not (member name components)))
    (error "Quantity ~S needs distinct symbolic component names, not ~S."
           name components))
  (let ((character (cond (character-supplied-p character)
                         (non-negative-p :absolute)
                         (t :difference))))
    (unless (typep character 'quantity-character)
      (error "Quantity ~S needs a character of :POINT, :ABSOLUTE, or :DIFFERENCE, not ~S."
             name character))
    (when (and non-negative-p (eq character :point))
      (error "Quantity ~S cannot be both non-negative and a point." name))
    (make-instance 'quantity-definition
                   :name name :kind kind :components (copy-list components)
                   :non-negative-p (not (null non-negative-p))
                   :character character)))

(defmacro define-quantity (name &key kind components non-negative-p
                                     (character nil character-supplied-p))
  "Define quantity NAME and any homogeneous COMPONENTS as members of KIND.

NON-NEGATIVE-P declares a non-negative amount and defaults its character to
:ABSOLUTE.  CHARACTER may state :POINT, :ABSOLUTE, or :DIFFERENCE explicitly.
Components inherit both."
  (let ((character-options
          (and character-supplied-p `(:character ,character))))
    `(progn
       (define-semantic-definition quantity-definition-for ,name
         (make-quantity-definition ,name ,kind
                                   :components ',components
                                   :non-negative-p ,non-negative-p
                                   ,@character-options))
       ,@(loop for component in components
               collect
               `(define-semantic-definition quantity-definition-for ,component
                  (make-quantity-definition ,component ,kind
                                            :non-negative-p ,non-negative-p
                                            ,@character-options)))
       ',name)))

(defun unit-designator-quantity-kind (unit)
  "Return the kind constraint of one named UNIT, or NIL for a compound unit."
  (cond
    ((symbolp unit)
     (unit-definition-quantity-kind (unit-definition-for unit)))
    (t
     (let ((factors (unit-expression-factors (make-unit-expression unit))))
       (and (= (length factors) 1)
            (= (cdar factors) 1)
            (unit-definition-quantity-kind
             (unit-definition-for (caar factors))))))))

(defun validate-unit-admissibility (quantity-name unit unit-dimension)
  (let ((required-kind (unit-designator-quantity-kind unit)))
    (when quantity-name
      (let* ((quantity (quantity-definition-for quantity-name))
             (actual-kind (and quantity (quantity-definition-kind quantity)))
             (kind-definition
               (and actual-kind (quantity-kind-definition-for actual-kind))))
        (unless quantity
          (quantity-operation-error
           'make-quantity-specification (list quantity-name unit)
           :undefined-quantity-definition))
        (unless (and kind-definition
                     (dimension=
                      unit-dimension
                      (quantity-kind-definition-dimension kind-definition)))
          (quantity-operation-error
           'make-quantity-specification (list quantity-name unit)
           :quantity-kind-dimension-mismatch))
        (when (and required-kind
                   (not (quantity-kind-subkind-p
                         actual-kind required-kind)))
          (quantity-operation-error
           'make-quantity-specification (list quantity-name unit)
           :unit-not-admissible-for-quantity))))))

(defun make-unit-definition
    (name &key (dimension nil dimension-supplied-p)
               (reference nil reference-supplied-p)
               (magnitude 1) identity-p quantity-kind)
  (unless (and (symbolp name) (realp magnitude) (plusp magnitude))
    (error "A unit needs a symbolic name and positive real magnitude: ~S, ~S."
           name magnitude))
  (when (and dimension-supplied-p reference-supplied-p)
    (error "Unit ~S cannot define both a base dimension and a reference unit."
           name))
  (unless (or dimension-supplied-p reference-supplied-p)
    (error "Unit ~S needs either :DIMENSION or :REFERENCE." name))
  (when (and identity-p
             (or reference-supplied-p
                 (not (dimensionless-p (make-dimension dimension)))
                 (/= magnitude 1)))
    (error "Identity unit ~S must be a dimension-one base of magnitude one."
           name))
  (unless (and (symbolp quantity-kind)
               (quantity-kind-definition-for quantity-kind))
    (error "Unit ~S needs a defined :QUANTITY-KIND, not ~S."
           name quantity-kind))
  (let* ((reference-expression
           (and reference-supplied-p (make-unit-expression reference)))
         (effective-dimension
           (if reference-expression
               (unit-expression-dimension reference-expression)
               (make-dimension dimension)))
         (effective-magnitude
           (* magnitude
              (if reference-expression
                  (unit-expression-magnitude reference-expression)
                  1)))
         (basis
           (cond (reference-expression
                  (unit-expression-basis reference-expression))
                 (identity-p (raw-unit-expression nil))
                 (t (raw-unit-expression (list (cons name 1)))))))
    (make-instance 'unit-definition
                   :name name
                   :dimension effective-dimension
                   :magnitude effective-magnitude
                   :basis basis
                   :identity-p (not (null identity-p))
                   :quantity-kind quantity-kind)))

(defmacro define-unit (name &rest options)
  "Define NAME as a semantic linear unit through an inspectable EQL method."
  `(define-semantic-definition unit-definition-for ,name
     (make-unit-definition ,name ,@options)))

;; The roots are intentionally small: application domains add named quantities
;; beneath these kinds without teaching the arithmetic core their vocabulary.
(define-quantity-kind :dimensionless :dimension nil)
(define-quantity-kind :length :dimension :length)
(define-quantity-kind :duration :dimension :duration)
(define-quantity-kind :frequency :dimension ((:duration -1)))
(define-quantity-kind :proportion :dimension nil :parent :dimensionless)
(define-quantity-kind :angular-measure :dimension nil :parent :dimensionless)
(define-quantity-kind :solid-angular-measure
  :dimension nil :parent :dimensionless)

;; A compact ISQ/SI-inspired seed vocabulary.  More units extend the same open
;; protocol; an unknown spelling is an error rather than an anonymous factor.
(define-unit :one :dimension nil :identity-p t
  :quantity-kind :dimensionless)
(define-unit :percent :reference :one :magnitude 1/100
  :quantity-kind :dimensionless)
(define-unit :per-mille :reference :one :magnitude 1/1000
  :quantity-kind :dimensionless)
(define-unit :parts-per-million :reference :one :magnitude 1/1000000
  :quantity-kind :dimensionless)
(define-unit :metre :dimension :length :quantity-kind :length)
(define-unit :kilometre :reference :metre :magnitude 1000
  :quantity-kind :length)
(define-unit :second :dimension :duration :quantity-kind :duration)
(define-unit :hertz :reference '((:second -1)) :quantity-kind :frequency)
(define-unit :radian :reference :one :quantity-kind :angular-measure)
(define-unit :steradian :reference :one
  :quantity-kind :solid-angular-measure)

;; Canonical quantity names are useful at generic boundaries; applications
;; normally add narrower names (opacity, texture coordinates, world distance)
;; beneath the same kinds.
(define-quantity :dimensionless :kind :dimensionless)
(define-quantity :proportion :kind :proportion)
(define-quantity :distance :kind :length)
(define-quantity :height :kind :length)
(define-quantity :width :kind :length)
(define-quantity :duration :kind :duration)
(define-quantity :frequency :kind :frequency)
(define-quantity :angle :kind :angular-measure)
(define-quantity :solid-angle :kind :solid-angular-measure)

(defclass quantity-specification ()
  ((name
    :initarg :name
    :initform nil
    :reader quantity-specification-name)
   (dimension
    :initarg :dimension
    :reader quantity-specification-dimension)
   (unit
    :initarg :unit
    :reader quantity-specification-unit)
   (kind
    :initarg :kind
    :initform nil
    :reader quantity-specification-kind)
   (tensor-order
    :initarg :tensor-order
    :initform 0
    :reader quantity-specification-tensor-order)
   (character
    :initarg :character
    :initform :difference
    :reader quantity-specification-character))
  (:documentation
   "The semantic meaning of a value, separate from its machine representation."))

(defun quantity-specification-affine-p (specification)
  "Whether SPECIFICATION is an affine point; kept as the historical spelling."
  (eq (quantity-specification-character specification) :point))

(defun quantity-specification-absolute-p (specification)
  (eq (quantity-specification-character specification) :absolute))

(defun quantity-specification-difference-p (specification)
  (eq (quantity-specification-character specification) :difference))

(defun quantity-specification-non-negative-p (specification)
  "Whether SPECIFICATION's named definition declares a non-negative amount.

Only a named absolute can promise this; anonymous derived results never do."
  (let* ((name (quantity-specification-name specification))
         (definition (and name (quantity-definition-for name))))
    (and definition
         (quantity-specification-absolute-p specification)
         (quantity-definition-non-negative-p definition))))

(defun resolve-quantity-character
    (name character character-supplied-p affine-p affine-p-supplied-p)
  "Choose a specification character from an explicit CHARACTER, the historical
AFFINE-P spelling, or the named definition's default."
  (cond
    ((and character-supplied-p affine-p-supplied-p
          (not (eq (eq character :point) (not (null affine-p)))))
     (error "Quantity ~S was given contradictory :CHARACTER ~S and :AFFINE-P ~S."
            name character affine-p))
    (character-supplied-p
     (unless (typep character 'quantity-character)
       (error "A quantity character must be :POINT, :ABSOLUTE, or :DIFFERENCE, not ~S."
              character))
     character)
    ((and affine-p-supplied-p affine-p) :point)
    (t
     ;; The definition supplies the default.  An explicit :AFFINE-P NIL says
     ;; only "not a point": a declared absolute stays absolute, a declared
     ;; point becomes a difference of that quantity.
     (let* ((definition (and name (quantity-definition-for name)))
            (default (if definition
                         (quantity-definition-character definition)
                         :difference)))
       (if (and affine-p-supplied-p (eq default :point))
           :difference
           default)))))

(defun make-quantity-specification
    (name &key (dimension nil dimension-supplied-p) unit
               (tensor-order 0)
               (affine-p nil affine-p-supplied-p)
               (character nil character-supplied-p))
  (unless (typep tensor-order '(integer 0 *))
    (error "A tensor order must be a non-negative integer, not ~S."
           tensor-order))
  (let* ((character (resolve-quantity-character
                     name character character-supplied-p
                     affine-p affine-p-supplied-p))
         (unit-expression (make-unit-expression unit))
         (unit-declares-dimension-p
           (or (and unit (symbolp unit))
               (not (unitless-p unit-expression))))
         (unit-dimension
           (and unit-declares-dimension-p
                (unit-expression-dimension unit-expression)))
         (declared-dimension (make-dimension dimension))
         (effective-dimension (if unit-declares-dimension-p
                                  unit-dimension
                                  declared-dimension))
         (quantity-definition (and name (quantity-definition-for name)))
         (kind-definition
           (and quantity-definition
                (quantity-kind-definition-for
                 (quantity-definition-kind quantity-definition)))))
    (when (and unit-declares-dimension-p
               dimension-supplied-p
               (not (dimension= unit-dimension declared-dimension)))
      (quantity-operation-error
       'make-quantity-specification (list name dimension unit)
       :unit-dimension-mismatch))
    (when (and kind-definition
               (not (dimension=
                     effective-dimension
                     (quantity-kind-definition-dimension kind-definition))))
      (quantity-operation-error
       'make-quantity-specification (list name dimension unit)
       :quantity-kind-dimension-mismatch))
    (when unit-declares-dimension-p
      (validate-unit-admissibility name unit unit-dimension))
    (make-instance 'quantity-specification
                   :name name
                   :dimension effective-dimension
                   :unit unit-expression
                   :kind (and quantity-definition
                              (quantity-definition-kind quantity-definition))
                   :tensor-order tensor-order
                   :character character)))

(defmethod print-object ((specification quantity-specification) stream)
  (print-unreadable-object (specification stream :type t)
    (format stream "~S~@[ <~S>~] ~A [~A] order ~D~@[ ~(~A~)~]"
            (quantity-specification-name specification)
            (quantity-specification-kind specification)
            (quantity-specification-dimension specification)
            (quantity-specification-unit specification)
            (quantity-specification-tensor-order specification)
            (let ((character (quantity-specification-character specification)))
              (and (not (eq character :difference)) character)))))

(defun quantity-specification= (left right)
  (and (eq (quantity-specification-name left)
           (quantity-specification-name right))
       (dimension= (quantity-specification-dimension left)
                   (quantity-specification-dimension right))
       (unit-expression= (quantity-specification-unit left)
                         (quantity-specification-unit right))
       (= (quantity-specification-tensor-order left)
          (quantity-specification-tensor-order right))
       (eq (quantity-specification-character left)
           (quantity-specification-character right))))

(defun dimensionless-quantity-specification-p
    (specification &optional tensor-order)
  "Whether SPECIFICATION is linear, unitless, and optionally of TENSOR-ORDER."
  (and (dimensionless-p
        (quantity-specification-dimension specification))
       (unitless-p (quantity-specification-unit specification))
       (or (null tensor-order)
           (= tensor-order
              (quantity-specification-tensor-order specification)))
       (not (quantity-specification-affine-p specification))))

(defun copy-quantity-specification
    (source &key
              (name (quantity-specification-name source))
              (dimension (quantity-specification-dimension source))
              (unit (quantity-specification-unit source))
              (tensor-order (quantity-specification-tensor-order source))
              (character (quantity-specification-character source)
                         character-supplied-p)
              (affine-p nil affine-p-supplied-p))
  "Copy SOURCE, replacing only the explicitly supplied semantic fields.

CHARACTER is the general control; the historical :AFFINE-P keyword still
sets or clears the point character."
  (make-quantity-specification name
                               :dimension dimension
                               :unit unit
                               :tensor-order tensor-order
                               :character (cond (character-supplied-p character)
                                                ((and affine-p-supplied-p affine-p)
                                                 :point)
                                                ((and affine-p-supplied-p
                                                      (eq character :point))
                                                 :difference)
                                                (t character))))

(defclass quantity-projection ()
  ((positions
    :initarg :positions
    :reader quantity-projection-positions)
   (specification
    :initarg :specification
    :reader quantity-projection-specification))
  (:documentation
   "One quantity occupying selected positions of a composite representation."))

(defun make-quantity-projection (positions specification)
  (unless (and (consp positions)
               (every (lambda (position)
                        (typep position '(integer 0 *)))
                      positions)
               (= (length positions)
                  (length (remove-duplicates positions))))
    (error "Quantity projection positions must be distinct non-negative integers, not ~S."
           positions))
  (check-type specification quantity-specification)
  (make-instance 'quantity-projection
                 :positions (copy-list positions)
                 :specification specification))

(defclass quantity-layout ()
  ((extent
    :initarg :extent
    :reader quantity-layout-extent)
   (projections
    :initarg :projections
    :reader quantity-layout-projections))
  (:documentation
   "Semantic quantities packed into disjoint positions of one representation."))

(defun make-quantity-layout (extent projections)
  (unless (typep extent '(integer 1 *))
    (error "A quantity layout extent must be a positive integer, not ~S."
           extent))
  (let ((occupied nil))
    (dolist (projection projections)
      (check-type projection quantity-projection)
      (dolist (position (quantity-projection-positions projection))
        (unless (< position extent)
          (error "Quantity projection position ~D is outside extent ~D."
                 position extent))
        (when (member position occupied)
          (error "Quantity layout position ~D is specified more than once."
                 position))
        (push position occupied))))
  (make-instance 'quantity-layout
                 :extent extent
                 :projections (copy-list projections)))

(defmethod print-object ((layout quantity-layout) stream)
  (print-unreadable-object (layout stream :type t)
    (format stream "~D lanes: ~{~S~^, ~}"
            (quantity-layout-extent layout)
            (mapcar
             (lambda (projection)
               (list (quantity-projection-positions projection)
                     (quantity-specification-name
                      (quantity-projection-specification projection))))
             (quantity-layout-projections layout)))))

(defun quantity-layout= (left right)
  (and (= (quantity-layout-extent left) (quantity-layout-extent right))
       (= (length (quantity-layout-projections left))
          (length (quantity-layout-projections right)))
       (every
        (lambda (left-projection)
          (let ((right-projection
                  (find (quantity-projection-positions left-projection)
                        (quantity-layout-projections right)
                        :test #'equal
                        :key #'quantity-projection-positions)))
            (and right-projection
                 (quantity-specification=
                  (quantity-projection-specification left-projection)
                  (quantity-projection-specification right-projection)))))
        (quantity-layout-projections left))))

(defun project-quantity-layout (layout positions)
  "Return the quantity exactly occupying POSITIONS in LAYOUT, or NIL."
  (let ((projection
          (find positions (quantity-layout-projections layout)
                :test #'equal :key #'quantity-projection-positions)))
    (and projection (quantity-projection-specification projection))))

(defun quantity-component-names (quantity-name)
  "Return the ordered homogeneous components declared for QUANTITY-NAME."
  (let ((definition (quantity-definition-for quantity-name)))
    (and definition (quantity-definition-components definition))))

(defun project-quantity-specification (specification positions extent)
  "Derive the quantity selected from a homogeneous represented quantity.

Selecting every position in order preserves the whole.  A single component
of an anonymous quantity remains anonymous.  A named quantity must explicitly
publish ordered component names; no default silently calls one axis the whole."
  (check-type specification quantity-specification)
  (cond
    ((equal positions (loop for position below extent collect position))
     specification)
    ((= (length positions) 1)
     (let* ((whole-name (quantity-specification-name specification))
            (component-names
              (and whole-name (quantity-component-names whole-name)))
            (position (first positions))
            (component-name
              (and (< position (length component-names))
                   (nth position component-names))))
       (when (and whole-name (null component-name))
         (quantity-operation-error
          'project (list specification positions)
          :missing-quantity-component-definition))
       (copy-quantity-specification specification
                                    :name component-name
                                    :tensor-order 0)))
    (t
     (quantity-operation-error
      'project (list specification positions)
      :missing-quantity-projection-definition))))

(define-condition quantity-operation-error (error)
  ((operator
    :initarg :operator
    :reader quantity-operation-error-operator)
   (specifications
    :initarg :specifications
    :reader quantity-operation-error-specifications)
   (reason
    :initarg :reason
    :reader quantity-operation-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Cannot derive ~S over quantity specifications ~S: ~A."
             (quantity-operation-error-operator condition)
             (quantity-operation-error-specifications condition)
             (quantity-operation-error-reason condition)))))

(defun quantity-operation-error (operator specifications reason)
  (error 'quantity-operation-error
         :operator operator :specifications specifications :reason reason))

(defun same-quantity-space-p (left right)
  (and (eq (quantity-specification-name left)
           (quantity-specification-name right))
       (dimension= (quantity-specification-dimension left)
                   (quantity-specification-dimension right))
       (= (quantity-specification-tensor-order left)
          (quantity-specification-tensor-order right))))

(defun derived-specification (dimension unit tensor-order)
  (make-quantity-specification nil
                               :dimension dimension
                               :unit unit
                               :tensor-order tensor-order))

(defun scalar-number-specification-p (specification)
  (and (null (quantity-specification-name specification))
       (dimensionless-quantity-specification-p specification 0)))

(defun additive-pair (operator left right)
  (unless (same-quantity-space-p left right)
    (quantity-operation-error operator (list left right)
                              :different-quantity-spaces))
  (unless (unit-expression= (quantity-specification-unit left)
                            (quantity-specification-unit right))
    (quantity-operation-error operator (list left right)
                              :different-units))
  ;; The affine table over point, absolute, and difference.  A point is a
  ;; location; an absolute is an amount from a true zero, forming a cone; a
  ;; difference is signed.  Adding a difference to an absolute keeps the zero
  ;; anchor, so the result stays absolute; subtracting two absolutes yields a
  ;; signed difference, and only an explicit interpretation recovers an
  ;; absolute from it.
  (let ((left-character (quantity-specification-character left))
        (right-character (quantity-specification-character right)))
    (flet ((result (character)
             (copy-quantity-specification left :character character))
           (fail (reason)
             (quantity-operation-error operator (list left right) reason)))
      (ecase operator
        (+
         (cond ((and (eq left-character :point) (eq right-character :point))
                (fail :cannot-add-points))
               ((or (eq left-character :point) (eq right-character :point))
                (result :point))
               ((or (eq left-character :absolute) (eq right-character :absolute))
                (result :absolute))
               (t (result :difference))))
        (-
         (cond ((eq left-character :point)
                (result (if (eq right-character :point) :difference :point)))
               ((eq right-character :point)
                (fail :cannot-subtract-point-from-amount))
               ((and (eq left-character :absolute)
                     (eq right-character :difference))
                (result :absolute))
               (t (result :difference))))))))

(defun product-tensor-order (operator left right)
  (let ((left-order (quantity-specification-tensor-order left))
        (right-order (quantity-specification-tensor-order right)))
    (cond ((zerop left-order) right-order)
          ((zerop right-order) left-order)
          ((= left-order right-order) left-order)
          (t
           (quantity-operation-error operator (list left right)
                                     :incompatible-tensor-orders)))))

(defun product-character (left right)
  "Products and quotients of two absolutes remain absolute; any signed factor
makes the result a difference.  Points never enter these operations."
  (if (and (eq (quantity-specification-character left) :absolute)
           (eq (quantity-specification-character right) :absolute))
      :absolute
      :difference))

(defun multiplicative-pair (operator left right)
  (when (or (quantity-specification-affine-p left)
            (quantity-specification-affine-p right))
    (quantity-operation-error operator (list left right)
                              :cannot-scale-affine-point))
  ;; A bare number is a pure scale factor and preserves the other operand's
  ;; character, so twice an amount is still an amount.
  (cond ((scalar-number-specification-p left) right)
        ((scalar-number-specification-p right) left)
        (t
         (copy-quantity-specification
          (derived-specification
           (multiply-dimensions
            (quantity-specification-dimension left)
            (quantity-specification-dimension right))
           (multiply-unit-expressions
            (quantity-specification-unit left)
            (quantity-specification-unit right))
           (product-tensor-order operator left right))
          :character (product-character left right)))))

(defun compatible-pair (operator left right)
  (unless (quantity-specification= left right)
    (quantity-operation-error operator (list left right)
                              (if (unit-expression=
                                   (quantity-specification-unit left)
                                   (quantity-specification-unit right))
                                  :incompatible-quantities
                                  :different-units)))
  left)

(defun interpret-quantity-specification (derived interpretation)
  "Give a compatible anonymous DERIVED specification an explicit meaning.

This is a semantic interpretation, never a numerical unit conversion.  An
already named quantity may only retain its name; anonymous derived results may
acquire one when their dimension, exact unit, and tensor order agree with
INTERPRETATION.  Character must agree too, with one deliberate exception: a
signed difference may be interpreted as an absolute.  That is the explicit
promotion the affine algebra otherwise never performs — the author asserts
the amount is non-negative, and no lowering checks it (#PLRP3A).  Points
never cross to or from the other characters here."
  (unless (and derived
               (or (null (quantity-specification-name derived))
                   (eq (quantity-specification-name derived)
                       (quantity-specification-name interpretation)))
               (dimension=
                (quantity-specification-dimension derived)
                (quantity-specification-dimension interpretation))
               (unit-expression=
                (quantity-specification-unit derived)
                (quantity-specification-unit interpretation))
               (= (quantity-specification-tensor-order derived)
                  (quantity-specification-tensor-order interpretation))
               (let ((from (quantity-specification-character derived))
                     (to (quantity-specification-character interpretation)))
                 (or (eq from to)
                     (and (eq from :difference) (eq to :absolute)))))
    (quantity-operation-error 'interpret (list derived interpretation)
                              :incompatible-interpretation))
  interpretation)

(defun convert-quantity-specification-unit (source target-unit)
  "Return SOURCE expressed in TARGET-UNIT and its required numeric factor.

The operation preserves semantic name, dimension, tensor order, and affine
character.  It is a linear unit conversion, not a semantic interpretation."
  (check-type source quantity-specification)
  (let* ((target-unit (make-unit-expression target-unit))
         (factor
           (unit-conversion-factor
            (quantity-specification-unit source) target-unit))
         (target-dimension (unit-expression-dimension target-unit)))
    (unless (dimension= (quantity-specification-dimension source)
                        target-dimension)
      (quantity-operation-error
       'convert-unit (list source target-unit) :incompatible-dimensions))
    (values
     (copy-quantity-specification source
                                  :dimension target-dimension
                                  :unit target-unit)
     factor)))

(defgeneric derive-quantity-specification (operator &rest operands)
  (:documentation
   "Derive the semantic result of applying OPERATOR to quantity OPERANDS."))

(defmethod derive-quantity-specification (operator &rest operands)
  (quantity-operation-error operator operands :unknown-operator))

(defun reduce-quantity-specifications
    (operator operands pair-function &optional (minimum 1))
  (when (< (length operands) minimum)
    (quantity-operation-error operator operands :missing-operands))
  (reduce (lambda (left right)
            (funcall pair-function operator left right))
          operands))

(defmethod derive-quantity-specification ((operator (eql '+)) &rest operands)
  (reduce-quantity-specifications operator operands #'additive-pair))

(defmethod derive-quantity-specification ((operator (eql '-)) &rest operands)
  (unless operands
    (quantity-operation-error operator operands :missing-operands))
  (if (= (length operands) 1)
      (let ((operand (first operands)))
        ;; Negating an amount is never an amount.  The source leaves open
        ;; whether it errors or demotes to a difference; luv errors, so the
        ;; demotion is a visible mark (subtract from zero, or interpret) rather
        ;; than a silent character change.
        (case (quantity-specification-character operand)
          (:point
           (quantity-operation-error operator operands :cannot-negate-point))
          (:absolute
           (quantity-operation-error operator operands :cannot-negate-amount)))
        operand)
      (reduce (lambda (left right) (additive-pair operator left right))
              (rest operands) :initial-value (first operands))))

(defmethod derive-quantity-specification ((operator (eql '*)) &rest operands)
  (reduce-quantity-specifications operator operands #'multiplicative-pair))

(defmethod derive-quantity-specification ((operator (eql '/)) &rest operands)
  (unless (= (length operands) 2)
    (quantity-operation-error operator operands :division-arity))
  (destructuring-bind (numerator denominator) operands
    (when (or (quantity-specification-affine-p numerator)
              (quantity-specification-affine-p denominator))
      (quantity-operation-error operator operands :cannot-divide-affine-point))
    (if (scalar-number-specification-p denominator)
        numerator
        (copy-quantity-specification
         (derived-specification
          (divide-dimensions
           (quantity-specification-dimension numerator)
           (quantity-specification-dimension denominator))
          (divide-unit-expressions
           (quantity-specification-unit numerator)
           (quantity-specification-unit denominator))
          (product-tensor-order operator numerator denominator))
         :character (product-character numerator denominator)))))

(defmethod derive-quantity-specification ((operator (eql 'dot)) &rest operands)
  (unless (= (length operands) 2)
    (quantity-operation-error operator operands :dot-arity))
  (destructuring-bind (left right) operands
    (when (or (quantity-specification-affine-p left)
              (quantity-specification-affine-p right)
              (/= (quantity-specification-tensor-order left) 1)
              (/= (quantity-specification-tensor-order right) 1))
      (quantity-operation-error operator operands :dot-requires-vectors))
    (derived-specification
     (multiply-dimensions
      (quantity-specification-dimension left)
      (quantity-specification-dimension right))
     (multiply-unit-expressions
      (quantity-specification-unit left)
      (quantity-specification-unit right))
     0)))

(defmethod derive-quantity-specification ((operator (eql 'min)) &rest operands)
  (reduce-quantity-specifications operator operands #'compatible-pair 2))

(defmethod derive-quantity-specification ((operator (eql 'max)) &rest operands)
  (reduce-quantity-specifications operator operands #'compatible-pair 2))

;;; Portable numerical operators extend the same open semantic protocol as
;;; Common Lisp arithmetic.  Execution backends remain responsible for their
;;; representations and implementations; these methods state only the lawful
;;; quantity relationship.

(defmethod derive-quantity-specification
    ((operator (eql 'clamp)) &rest operands)
  (unless (= (length operands) 3)
    (quantity-operation-error operator operands :clamp-arity))
  (apply #'derive-quantity-specification 'max operands))

(defmethod derive-quantity-specification
    ((operator (eql 'step)) &rest operands)
  (unless (= (length operands) 2)
    (quantity-operation-error operator operands :step-arity))
  (let ((compatible
          (apply #'derive-quantity-specification 'max operands)))
    (make-quantity-specification
     nil :tensor-order
     (quantity-specification-tensor-order compatible))))

(defmethod derive-quantity-specification
    ((operator (eql 'mix)) &rest operands)
  (unless (= (length operands) 3)
    (quantity-operation-error operator operands :mix-arity))
  (destructuring-bind (from to amount) operands
    (let ((result (derive-quantity-specification 'max from to)))
      (unless (dimensionless-quantity-specification-p amount 0)
        (quantity-operation-error
         operator operands :mix-requires-dimensionless-scalar-amount))
      result)))

(defmethod derive-quantity-specification
    ((operator (eql 'smoothstep)) &rest operands)
  (unless (= (length operands) 3)
    (quantity-operation-error operator operands :smoothstep-arity))
  (let ((compatible
          (apply #'derive-quantity-specification 'max operands)))
    (make-quantity-specification
     nil :tensor-order
     (quantity-specification-tensor-order compatible))))

(defmethod derive-quantity-specification
    ((operator (eql 'normalize)) &rest operands)
  (unless (= (length operands) 1)
    (quantity-operation-error operator operands :normalize-arity))
  (let ((operand (first operands)))
    (unless (dimensionless-quantity-specification-p operand 1)
      (quantity-operation-error
       operator operands :normalize-requires-dimensionless-vector))
    operand))

(defmethod derive-quantity-specification
    ((operator (eql 'expt)) &rest operands)
  (unless (= (length operands) 2)
    (quantity-operation-error operator operands :expt-arity))
  (destructuring-bind (base exponent) operands
    (unless (and (dimensionless-quantity-specification-p base)
                 (dimensionless-quantity-specification-p exponent 0))
      (quantity-operation-error
       operator operands :expt-requires-dimensionless-operands))
    (make-quantity-specification
     nil :tensor-order
     (quantity-specification-tensor-order base))))
