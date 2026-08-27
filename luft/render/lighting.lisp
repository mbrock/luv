(in-package #:luft.render)

;;; Lighting has identity at the frame boundary.  The shader sees only the
;;; dense lanes packed from this object once per frame; fragments do not carry
;;; objects or dispatch through the lighting vocabulary.

(defparameter +shadow-map-size+ 1024
  "Resolution of LUFT's single sun-shadow map.")

(defclass light ()
  ((name :initarg :name :reader light-name)
   (sun-direction :initarg :sun-direction :reader light-sun-direction)
   (sun-color :initarg :sun-color :reader light-sun-color)
   (sky-color :initarg :sky-color :reader light-sky-color)
   (ground-color :initarg :ground-color :reader light-ground-color)
   (shadow-half-extent :initarg :shadow-half-extent
                       :reader light-shadow-half-extent)
   (shadow-depth-radius :initarg :shadow-depth-radius
                        :reader light-shadow-depth-radius)
   (shadow-base-bias :initarg :shadow-base-bias
                     :reader light-shadow-base-bias)
   (shadow-filter-radius :initarg :shadow-filter-radius
                         :reader light-shadow-filter-radius))
  (:documentation
   "One inspectable environment light, packed into raw per-frame GPU lanes."))

(defvar *light* nil)

(setf *light*
      (ensure-semantic-instance
       *light* 'light
       :name :dusk
       :sun-direction (vec3:vec3-normalize (vec3:make-vec3 -0.72 0.43 0.22))
       :sun-color #(1.85 0.82 0.38 0.92)
       :sky-color #(0.065 0.095 0.23 1.0)
       :ground-color #(0.23 0.115 0.16 1.0)
       :shadow-half-extent 96.0
       :shadow-depth-radius 160.0
       :shadow-base-bias 0.00075
       :shadow-filter-radius 5.0))

(defun light-shadow-rows (light center)
  "Return a texel-stable orthographic world-to-shadow transform.

The first two rows map the square light plane to clip [-1,1].  The third maps
the signed light depth around CENTER to [0,1].  CENTER is snapped in the light
plane so camera translation cannot slide a shadow edge by a fraction of a
texel."
  (let* ((sun (light-sun-direction light))
         (forward (vec3:vec3-scale sun -1.0))
         (world-up (vec3:make-vec3 0.0 0.0 1.0))
         (right (vec3:vec3-normalize (vec3:vec3-cross world-up forward)))
         (up (vec3:vec3-cross forward right))
         (extent (light-shadow-half-extent light))
         (depth-radius (light-shadow-depth-radius light))
         (world-units-per-texel (/ (* 2.0 extent) +shadow-map-size+))
         (center-right
           (* (round (/ (vec3:vec3-dot center right) world-units-per-texel))
              world-units-per-texel))
         (center-up
           (* (round (/ (vec3:vec3-dot center up) world-units-per-texel))
              world-units-per-texel))
         (center-forward (vec3:vec3-dot center forward)))
    (flet ((row (axis scale offset)
             (list (* (vec3:vec3-x axis) scale)
                   (* (vec3:vec3-y axis) scale)
                   (* (vec3:vec3-z axis) scale)
                   offset)))
      (append
       (row right (/ extent) (- (/ center-right extent)))
       (row up (/ extent) (- (/ center-up extent)))
       (row forward (/ (* 2.0 depth-radius))
            (- 0.5 (/ center-forward (* 2.0 depth-radius))))
       '(0.0 0.0 0.0 1.0)))))

(defun light-uniform-data (light center &optional (exposure 1.0f0))
  "Return LIGHT's nine vec4 lanes for the frame uniform ABI."
  (flet ((vec3-lane (value fourth)
           (list (vec3:vec3-x value) (vec3:vec3-y value)
                 (vec3:vec3-z value) fourth)))
    (append
     (vec3-lane (light-sun-direction light) 0.0)
     (coerce (light-sun-color light) 'list)
     (list (aref (light-sky-color light) 0)
           (aref (light-sky-color light) 1)
           (aref (light-sky-color light) 2)
           exposure)
     (coerce (light-ground-color light) 'list)
     (light-shadow-rows light center)
     (list (/ +shadow-map-size+) (/ +shadow-map-size+)
           (light-shadow-base-bias light)
           (light-shadow-filter-radius light)))))

;;; ---------------------------------------------------------------------------
;;; Realized torch light
;;;
;;; A torch is authored on a cubical face but emits from the wick of its final
;;; realized surface frame.  This layer translates that continuous point into
;;; the discrete max-plus sources consumed by LUFT's one voxel-light solver.
;;; Geometry, residency, and publication freshness remain owned by their
;;; callers; equal quantized sources intentionally name the same light result.

(deftype realized-light-site-vector ()
  '(simple-array luft:site (*)))

(deftype realized-light-rgb4-vector ()
  '(simple-array (unsigned-byte 12) (*)))

(defstruct (realized-light-seeds
             (:constructor %make-realized-light-seeds (sites lights))
             (:copier nil)
             (:conc-name %realized-light-seeds-))
  "Canonical parallel lanes for positive voxel-light sources.

SITES are strictly increasing positive cell sites.  LIGHTS are nonzero RGB4
values.  Repeated sites have already met by componentwise maximum.  The arrays
are owned by this value and are never mutated after construction."
  (sites (make-array 0 :element-type 'luft:site)
         :type realized-light-site-vector :read-only t)
  (lights (make-array 0 :element-type '(unsigned-byte 12))
          :type realized-light-rgb4-vector :read-only t))

(defstruct (realized-light-stamp
             (:constructor %make-realized-light-stamp
                 (authored-light-provenance authored-light-revision
                  seed-sites seed-lights))
             (:copier nil)
             (:conc-name %realized-light-stamp-))
  "Exact reusable identity of one realized torch-light solve.

AUTHORED-LIGHT-PROVENANCE is the caller's immutable finished-input token and is
compared by identity.  AUTHORED-LIGHT-REVISION names that input's non-torch
source and opacity revision.  The copied seed lanes name the geometry-dependent
torch input.  No hash stands in for any value."
  (authored-light-provenance nil :read-only t)
  (authored-light-revision 0 :type (integer 0 *) :read-only t)
  (seed-sites (make-array 0 :element-type 'luft:site)
              :type realized-light-site-vector :read-only t)
  (seed-lights (make-array 0 :element-type '(unsigned-byte 12))
               :type realized-light-rgb4-vector :read-only t))

(defstruct (realized-light-generation
             (:constructor %make-realized-light-generation (stamp field))
             (:copier nil)
             (:conc-name %realized-light-generation-))
  "One exact realized-source stamp and its immutable solved voxel-light field."
  (stamp nil :type realized-light-stamp :read-only t)
  (field nil :type luft:voxel-light-field :read-only t))

(define-condition unrealizable-torch-light-source (error)
  ((point :initarg :point :reader unrealizable-torch-light-source-point))
  (:report
   (lambda (condition stream)
     (format stream
             "Torch wick ~S has no positive in-domain authored-air light seed."
             (unrealizable-torch-light-source-point condition))))
  (:documentation
   "A realized torch wick is outside the source domain, occluded, or too dim."))

(declaim (inline %realized-light-finite-single-float
                 %quantize-max-plus-light-lane))

(defun %realized-light-finite-single-float (value role)
  (unless (realp value)
    (error "~A must be a real number, not ~S." role value))
  (let ((single (coerce value 'single-float)))
    (unless (and (= single single)
                 (<= (abs single) most-positive-single-float))
      (error "~A is not a finite single float: ~S." role value))
    single))

(defun %point-coordinate (point index role)
  (unless (and (typep point 'sequence) (<= 3 (length point)))
    (error "~A must contain at least three coordinates, not ~S." role point))
  (%realized-light-finite-single-float (elt point index) role))

(defun realized-torch-wick-point
    (origin normal scale &optional (wick-offset 0.5f0))
  "Return the exact single-float flame wick of a realized torch frame.

The result is ORIGIN + WICK-OFFSET * SCALE * NORMAL.  The default offset is
the canonical half-cell torch wick; a renderer may pass its shared flame
constant explicitly.  NORMAL must already be unit length, as required by the
body/flame frame ABI."
  (let* ((origin-x (%point-coordinate origin 0 "Torch-frame origin"))
         (origin-y (%point-coordinate origin 1 "Torch-frame origin"))
         (origin-z (%point-coordinate origin 2 "Torch-frame origin"))
         (normal-x (%point-coordinate normal 0 "Torch-frame normal"))
         (normal-y (%point-coordinate normal 1 "Torch-frame normal"))
         (normal-z (%point-coordinate normal 2 "Torch-frame normal"))
         (scale (%realized-light-finite-single-float scale "Torch-frame scale"))
         (wick-offset
           (%realized-light-finite-single-float
            wick-offset "Torch-frame wick offset"))
         (normal-length-squared
           (+ (* normal-x normal-x)
              (* normal-y normal-y)
              (* normal-z normal-z))))
    (unless (plusp scale)
      (error "Torch-frame scale must be positive, not ~S." scale))
    (unless (<= (abs (- normal-length-squared 1.0f0)) 2.0f-4)
      (error "Torch-frame normal is not unit length: ~S." normal))
    (let ((distance (* wick-offset scale)))
      (make-array
       3 :element-type 'single-float
       :initial-contents
       (list (+ origin-x (* distance normal-x))
             (+ origin-y (* distance normal-y))
             (+ origin-z (* distance normal-z)))))))

(defun %canonical-realized-light-seed-vectors (sites lights)
  "Copy, sort, remove zeroes, and componentwise-coalesce parallel lanes."
  (unless (= (length sites) (length lights))
    (error "Realized-light site and RGB4 lanes differ in length: ~D and ~D."
           (length sites) (length lights)))
  (let* ((capacity (length sites))
         (sorted-sites (make-array capacity :element-type 'luft:site))
         (sorted-lights
           (make-array capacity :element-type '(unsigned-byte 12)))
         (count 0))
    (dotimes (index capacity)
      (let ((site (elt sites index))
            (light (elt lights index)))
        (check-type site luft:site)
        (unless (and (= (luft:site-extent site) luft:+cell-extent+)
                     (luft:site-positive-p site))
          (error "A realized-light seed needs a positive cell site, not ~S."
                 site))
        (check-type light (unsigned-byte 12))
        (unless (zerop light)
          (setf (aref sorted-sites count) site
                (aref sorted-lights count) light)
          (incf count))))
    ;; Source construction is cold and sparse, but typed insertion sorting
    ;; keeps the retained representation and its ordering law conspicuous.
    (loop for index from 1 below count do
      (let ((site (aref sorted-sites index))
            (light (aref sorted-lights index))
            (destination index))
        (loop while (and (plusp destination)
                         (> (aref sorted-sites (1- destination)) site))
              do (setf (aref sorted-sites destination)
                       (aref sorted-sites (1- destination))
                       (aref sorted-lights destination)
                       (aref sorted-lights (1- destination)))
                 (decf destination))
        (setf (aref sorted-sites destination) site
              (aref sorted-lights destination) light)))
    (let ((unique-count 0))
      (dotimes (index count)
        (let ((site (aref sorted-sites index))
              (light (aref sorted-lights index)))
          (if (and (plusp unique-count)
                   (= site (aref sorted-sites (1- unique-count))))
              (setf (aref sorted-lights (1- unique-count))
                    (luft:voxel-light-componentwise-max
                     (aref sorted-lights (1- unique-count)) light))
              (progn
                (setf (aref sorted-sites unique-count) site
                      (aref sorted-lights unique-count) light)
                (incf unique-count)))))
      (let ((canonical-sites
              (make-array unique-count :element-type 'luft:site))
            (canonical-lights
              (make-array unique-count
                          :element-type '(unsigned-byte 12))))
        (replace canonical-sites sorted-sites :end2 unique-count)
        (replace canonical-lights sorted-lights :end2 unique-count)
        (values canonical-sites canonical-lights)))))

(defun make-realized-light-seeds (sites lights)
  "Own canonical typed copies of parallel SITE and packed-RGB4 sequences."
  (multiple-value-bind (canonical-sites canonical-lights)
      (%canonical-realized-light-seed-vectors sites lights)
    (%make-realized-light-seeds canonical-sites canonical-lights)))

(defun realized-light-seeds-count (seeds)
  (check-type seeds realized-light-seeds)
  (length (%realized-light-seeds-sites seeds)))

(defun realized-light-seeds-sites (seeds)
  "Return a copy of SEEDS' strictly increasing positive cell sites."
  (check-type seeds realized-light-seeds)
  (copy-seq (%realized-light-seeds-sites seeds)))

(defun realized-light-seeds-lights (seeds)
  "Return a copy of SEEDS' packed RGB4 lanes."
  (check-type seeds realized-light-seeds)
  (copy-seq (%realized-light-seeds-lights seeds)))

(defun merge-realized-light-seeds (&rest groups)
  "Join GROUPS by site with componentwise RGB4 maximum.

This is the duplicate-torch law: source order cannot change the solved field."
  (dolist (group groups)
    (check-type group realized-light-seeds))
  (let* ((count (reduce #'+ groups :key #'realized-light-seeds-count
                                   :initial-value 0))
         (sites (make-array count :element-type 'luft:site))
         (lights (make-array count :element-type '(unsigned-byte 12)))
         (offset 0))
    (dolist (group groups)
      (let ((group-sites (%realized-light-seeds-sites group))
            (group-lights (%realized-light-seeds-lights group))
            (group-count (realized-light-seeds-count group)))
        (replace sites group-sites :start1 offset)
        (replace lights group-lights :start1 offset)
        (incf offset group-count)))
    (make-realized-light-seeds sites lights)))

(defun %quantize-max-plus-light-lane (level distance)
  "Q(max(0, LEVEL-DISTANCE)), where Q(x)=floor(x+0.5)."
  (check-type level (integer 0 15))
  (let ((remaining (max 0.0d0 (- (coerce level 'double-float) distance))))
    (min luft:+maximum-voxel-light-level+
         (max 0 (floor (+ remaining 0.5d0))))))

(defun %attenuate-realized-light (light distance)
  (check-type light (unsigned-byte 12))
  (luft:pack-voxel-light
   (%quantize-max-plus-light-lane (luft:voxel-light-red light) distance)
   (%quantize-max-plus-light-lane (luft:voxel-light-green light) distance)
   (%quantize-max-plus-light-lane (luft:voxel-light-blue light) distance)))

(defun %map-realized-light-brackets
    (function domain point authored-occupied-p)
  "Call FUNCTION with every in-domain authored-air bracket cell and L1 range."
  (check-type domain luft:world-domain)
  (let* ((point-x (coerce (%point-coordinate point 0 "Light point")
                          'double-float))
         (point-y (coerce (%point-coordinate point 1 "Light point")
                          'double-float))
         (point-z (coerce (%point-coordinate point 2 "Light point")
                          'double-float)))
    (labels ((bracket (coordinate)
               (let ((cell-coordinate (- coordinate 0.5d0)))
                 (values (floor cell-coordinate)
                         (ceiling cell-coordinate))))
             (choices (low high)
               (if (= low high) (list low) (list low high))))
      (multiple-value-bind (x-low x-high) (bracket point-x)
        (multiple-value-bind (y-low y-high) (bracket point-y)
          (multiple-value-bind (z-low z-high) (bracket point-z)
            (dolist (x (choices x-low x-high))
              (dolist (y (choices y-low y-high))
                (dolist (z (choices z-low z-high))
                  (when (and (<= 0 x)
                             (< x (luft:world-domain-x-limit domain))
                             (<= 0 y)
                             (< y (luft:world-domain-y-limit domain))
                             (<= 0 z)
                             (< z luft:+top-z+))
                    (let ((cell
                            (luft:make-site
                             domain x y z luft:+cell-extent+ 1)))
                      (unless (funcall authored-occupied-p cell)
                        (funcall
                         function cell
                         (+ (abs (- point-x (+ x 0.5d0)))
                            (abs (- point-y (+ y 0.5d0)))
                            (abs (- point-z (+ z 0.5d0)))))))))))))))))

(defun realized-torch-light-seeds
    (domain authored-occupied-p wick-point authored-light)
  "Discretize one continuous torch wick for LUFT's max-plus light solver.

Each axis brackets WICK-POINT against cell centers I+0.5, so at most eight
cells are considered.  Only in-domain cells reported as air by the authored
semantic occupancy callback survive.  Each RGB4 lane receives
Q(max(0,L-L1(WICK,CENTER))).  A flat centered torch therefore produces the
exact same single source as the former adjacent-cell authoring path."
  (check-type authored-light (unsigned-byte 12))
  (unless (plusp authored-light)
    (error "A torch needs positive authored RGB4 emission, not ~S."
           authored-light))
  (let ((sites (make-array 8 :element-type 'luft:site))
        (lights (make-array 8 :element-type '(unsigned-byte 12)))
        (count 0))
    (%map-realized-light-brackets
     (lambda (cell distance)
       (let ((light (%attenuate-realized-light authored-light distance)))
         (unless (zerop light)
           (setf (aref sites count) cell
                 (aref lights count) light)
           (incf count))))
     domain wick-point authored-occupied-p)
    (when (zerop count)
      (error 'unrealizable-torch-light-source
             :point (copy-seq wick-point)))
    (make-realized-light-seeds
     (subseq sites 0 count) (subseq lights 0 count))))

(defun voxel-light-at-continuous-point
    (field point authored-occupied-p)
  "Max-plus sample immutable FIELD at a continuous authored-air point.

The same center brackets and quantized L1 law used to seed a realized torch
reconstruct its continuous light cone.  Occupied brackets are excluded by the
authored semantic occupancy callback; no settled field storage is mutated."
  (check-type field luft:voxel-light-field)
  (let ((answer 0))
    (%map-realized-light-brackets
     (lambda (cell distance)
       (setf answer
             (luft:voxel-light-componentwise-max
              answer
              (%attenuate-realized-light
               (luft:voxel-light-at-site field cell) distance))))
     (luft:voxel-light-field-domain field) point authored-occupied-p)
    answer))

(defun realized-torch-self-light
    (field wick-point authored-occupied-p authored-light)
  "Join the continuous field sample at WICK-POINT with the torch's own RGB4.

Self emission is a componentwise lower bound, while unrelated colored sources
may still contribute brighter lanes."
  (check-type authored-light (unsigned-byte 12))
  (luft:voxel-light-componentwise-max
   authored-light
   (voxel-light-at-continuous-point
    field wick-point authored-occupied-p)))

(defun make-realized-light-stamp
    (authored-light-provenance authored-light-revision seeds)
  "Own provenance, revision, and canonical realized seed lanes in a stamp."
  (unless authored-light-provenance
    (error "Realized light needs a non-NIL authored provenance token."))
  (check-type authored-light-revision (integer 0 *))
  (check-type seeds realized-light-seeds)
  (%make-realized-light-stamp
   authored-light-provenance
   authored-light-revision
   (copy-seq (%realized-light-seeds-sites seeds))
   (copy-seq (%realized-light-seeds-lights seeds))))

(defun realized-light-stamp-authored-light-provenance (stamp)
  (check-type stamp realized-light-stamp)
  (%realized-light-stamp-authored-light-provenance stamp))

(defun realized-light-stamp-authored-light-revision (stamp)
  (check-type stamp realized-light-stamp)
  (%realized-light-stamp-authored-light-revision stamp))

(defun realized-light-stamp-seed-sites (stamp)
  "Return a copy of STAMP's exact sorted cell lane."
  (check-type stamp realized-light-stamp)
  (copy-seq (%realized-light-stamp-seed-sites stamp)))

(defun realized-light-stamp-seed-lights (stamp)
  "Return a copy of STAMP's exact packed-RGB4 lane."
  (check-type stamp realized-light-stamp)
  (copy-seq (%realized-light-stamp-seed-lights stamp)))

(defun realized-light-stamp= (left right)
  "Whether two stamps justify reuse of the same realized torch-light solve."
  (and (typep left 'realized-light-stamp)
       (typep right 'realized-light-stamp)
       (eq (%realized-light-stamp-authored-light-provenance left)
           (%realized-light-stamp-authored-light-provenance right))
       (= (%realized-light-stamp-authored-light-revision left)
          (%realized-light-stamp-authored-light-revision right))
       (equalp (%realized-light-stamp-seed-sites left)
               (%realized-light-stamp-seed-sites right))
       (equalp (%realized-light-stamp-seed-lights left)
               (%realized-light-stamp-seed-lights right))))

(defun %realized-light-voxel-sources (authored-sources stamp)
  "Pack authored sources and STAMP's parallel realized lanes for the solver."
  (let* ((authored-count (length authored-sources))
         (sites (%realized-light-stamp-seed-sites stamp))
         (lights (%realized-light-stamp-seed-lights stamp))
         (sources
           (make-array (+ authored-count (length sites))
                       :element-type '(unsigned-byte 64)))
         (offset 0))
    (map nil
         (lambda (source)
           (check-type source (unsigned-byte 64))
           (setf (aref sources offset) source)
           (incf offset))
         authored-sources)
    (dotimes (index (length sites))
      (setf (aref sources offset)
            (luft:make-voxel-light-source
             (aref sites index) (aref lights index)))
      (incf offset))
    sources))

(defun make-realized-light-generation
    (authored-light-provenance authored-light-revision seeds field)
  "Own an exact realized-light stamp beside an already solved immutable FIELD."
  (check-type field luft:voxel-light-field)
  (let ((stamp
          (make-realized-light-stamp
           authored-light-provenance authored-light-revision seeds))
        (domain (luft:voxel-light-field-domain field)))
    (loop for site across (%realized-light-seeds-sites seeds)
          do (luft:checked-site domain site))
    (%make-realized-light-generation stamp field)))

(defun solve-realized-light-generation
    (domain material-cells opacity-table authored-sources
     authored-light-provenance authored-light-revision realized-seeds
     &key (field-revision authored-light-revision))
  "Solve and own one immutable realized torch-light generation.

AUTHORED-SOURCES, AUTHORED-LIGHT-PROVENANCE, and AUTHORED-LIGHT-REVISION name
the caller-owned non-torch light inputs.  REALIZED-SEEDS are the exact
sorted/coalesced torch sources.
FIELD-REVISION is only the legacy scalar carried by VOXEL-LIGHT-FIELD; reuse and
staleness decisions must compare the generation's exact stamp."
  (check-type domain luft:world-domain)
  (unless authored-light-provenance
    (error "Realized light needs a non-NIL authored provenance token."))
  (check-type authored-light-revision (integer 0 *))
  (check-type field-revision (integer 0 *))
  (check-type realized-seeds realized-light-seeds)
  (let* ((stamp
           (make-realized-light-stamp
            authored-light-provenance authored-light-revision realized-seeds))
         (sources (%realized-light-voxel-sources authored-sources stamp))
         (field
           (luft:solve-voxel-light
            domain material-cells opacity-table sources
            :revision field-revision)))
    (%make-realized-light-generation stamp field)))

(defun realized-light-generation-stamp (generation)
  (check-type generation realized-light-generation)
  (%realized-light-generation-stamp generation))

(defun realized-light-generation-field (generation)
  (check-type generation realized-light-generation)
  (%realized-light-generation-field generation))
