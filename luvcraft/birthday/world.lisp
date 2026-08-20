;;; The birthday meadow: a gentle world for a small person to run around in.
;;;
;;; A BIRTHDAY-WORLD-SOURCE is a LITTLE-WORLD-SOURCE with kinder geography:
;;; low, long-period grassy hills, every surface grass rather than a biome
;;; rule, and one flat party clearing around the origin, smoothstep-blended
;;; into the hills so the gazebo gets a natural lawn.  Flower blocks freckle
;;; the grass from the same coordinate hashes the little world uses -- never
;;; randomness -- densest in a wreath around the clearing.  Trees are sparser
;;; than stock and never root inside the clearing ring.
;;;
;;; Edits, residency, and lighting semantics are inherited from the little
;;; world.  Two inherited paths would quietly hand a subclass stock terrain,
;;; so both are specialized here: the off-thread chunk load (the stock
;;; request's PERFORM method rebuilds a LITTLE-WORLD-SOURCE by name, and its
;;; landmark capture calls the stock generator) gets its own request class,
;;; and persistence gets its own :BIRTHDAY-MEADOW save kind so a world saved
;;; under worlds/alex-birthday.sexp comes back as the same meadow plus edits.

(in-package #:luvcraft.birthday)

(defclass birthday-world-source (luvcraft:little-world-source)
  ())

;;; The meadow's shape, gathered as knobs so the party can be re-landscaped
;;; live.  All readings stay pure functions of the source's seed and these
;;; globals, so any chunk regenerates bit-identically on any thread.

(defparameter *clearing-radius* 14.0d0
  "Inside this distance from the origin the lawn is perfectly flat.")

(defparameter *clearing-blend-radius* 26.0d0
  "By this distance the lawn has fully blended into the hills.")

(defparameter *clearing-height* 6.0d0
  "The flat party lawn's surface height.")

(defparameter *meadow-base-height* 5.5d0
  "The mean surface height of the rolling meadow.")

(defparameter *meadow-hill-period* 96
  "The long period of the meadow's one broad hill octave.")

(defparameter *meadow-hill-amplitude* 2.0d0
  "How far the broad hills swell above and below the base height.")

(defparameter *meadow-swell-period* 40
  "The period of the faint secondary swell that keeps hills from reading
as a single sine.")

(defparameter *meadow-swell-amplitude* 0.6d0
  "The faint secondary swell's amplitude.")

(defparameter *clearing-flower-density* 0.05d0
  "Flower chance per grass column on the party lawn itself: a light
freckle that leaves the gazebo's ground open.")

(defparameter *ring-flower-density* 0.16d0
  "Flower chance in the wreath between the lawn and the hills.")

(defparameter *far-flower-density* 0.035d0
  "Flower chance far out in the hills.")

(defparameter *flower-fade-radius* 60.0d0
  "Distance by which the wreath's density has faded to the far density.")

(defparameter *tree-exclusion-radius* 26.0d0
  "No tree roots closer to the origin than this.  It matches the blend
ring, so the whole approach to the party stays open, not just the lawn.")

(defun birthday-meadow-smoothstep (edge0 edge1 value)
  "0 at or below EDGE0, 1 at or above EDGE1, smooth in between."
  (let ((amount (max 0.0d0 (min 1.0d0 (/ (- value edge0)
                                         (- edge1 edge0))))))
    (* amount amount (- 3.0d0 (* 2.0d0 amount)))))

(defun birthday-origin-distance (x z)
  (sqrt (float (+ (* x x) (* z z)) 1.0d0)))

(defun birthday-meadow-surface-height (source x z height)
  "The meadow surface at X,Z: gentle hills flattened into the party lawn."
  (let* ((hills (+ *meadow-base-height*
                   (* *meadow-hill-amplitude*
                      (luvcraft:little-world-value-noise
                       source x z *meadow-hill-period* 0))
                   (* *meadow-swell-amplitude*
                      (luvcraft:little-world-value-noise
                       source x z *meadow-swell-period* 1))))
         (blend (birthday-meadow-smoothstep
                 *clearing-radius* *clearing-blend-radius*
                 (birthday-origin-distance x z)))
         (reading (+ (* (- 1.0d0 blend) *clearing-height*)
                     (* blend hills))))
    (max 2 (min (- height 4) (round reading)))))

(defun birthday-flower-block-p (source x z)
  "Whether the grass at X,Z has burst into flowers.

Deterministic like every other terrain reading: a coordinate hash against
a density that peaks in the wreath just outside the party clearing and
fades with distance, leaving the far hills a sprinkle and the lawn itself
lightly freckled."
  (let* ((distance (birthday-origin-distance x z))
         (density
           (if (<= distance *clearing-radius*)
               *clearing-flower-density*
               (+ *far-flower-density*
                  (* (- *ring-flower-density* *far-flower-density*)
                     (- 1.0d0 (birthday-meadow-smoothstep
                               *clearing-blend-radius* *flower-fade-radius*
                               distance)))))))
    (< (/ (luvcraft::little-world-hash source x z 77) #xffffffff)
       density)))

(defun birthday-surface-block (source x z)
  (if (birthday-flower-block-p source x z)
      luvcraft::*flowers-block*
      luvcraft::*grass-block*))

(defun materialize-birthday-meadow-chunk (source world chunk-x chunk-z)
  "Materialize one meadow terrain chunk at vertical layer zero.

The column fill mirrors the little world's; only the readings differ:
meadow heights, and every surface grass or flowers."
  (luvcraft::with-world-change-transaction (world)
    (let* ((chunk (luvcraft::ensure-world-chunk world chunk-x 0 chunk-z))
           (shape (luvcraft::voxel-space-chunk-shape
                   (luvcraft::block-world-space world)))
           (width (luvcraft::chunk-shape-width shape))
           (height (luvcraft::chunk-shape-height shape))
           (depth (luvcraft::chunk-shape-depth shape))
           (origin (luvcraft::chunk-domain-origin
                    (luvcraft::block-chunk-domain chunk))))
      (dotimes (local-z depth)
        (dotimes (local-x width)
          (let* ((x (+ (luvcraft::world-coordinate-x origin) local-x))
                 (z (+ (luvcraft::world-coordinate-z origin) local-z))
                 (surface (birthday-meadow-surface-height
                           source x z height))
                 (surface-block (birthday-surface-block source x z)))
            (dotimes (y (1+ surface))
              (setf (luvcraft::chunk-block-at-offset
                     chunk
                     (+ local-x (* width (+ y (* height local-z)))))
                    (cond ((= y surface) surface-block)
                          ((or (zerop y) (< y (- surface 2)))
                           luvcraft::*stone-block*)
                          (t luvcraft::*dirt-block*)))))))
      chunk)))

(defmethod luvcraft:materialize-block-world-chunk
    ((source birthday-world-source) (world luvcraft::block-world)
     chunk-x chunk-y chunk-z)
  (unless (zerop chunk-y)
    (error "The birthday meadow materializes only chunk layer zero."))
  (materialize-birthday-meadow-chunk source world chunk-x chunk-z))

(defun map-birthday-tree-blocks (source width height depth
                                 owner-x owner-z emit)
  "Call EMIT with BLOCK X Y Z for the tree rooted in chunk OWNER-X,OWNER-Z.

At most one tree per chunk, in about a third of them, and never inside
the exclusion ring around the party.  The shape is the little world's
tree; the meadow's kindness is in where and how often it grows.  The
placement margin keeps trunk and canopy inside the owner chunk, so the
direct populate path and the streaming landmark capture agree exactly."
  (let* ((origin-x (* owner-x width))
         (origin-z (* owner-z depth))
         (hash (luvcraft::little-world-hash source owner-x owner-z))
         (tree-x (+ origin-x 3 (mod (ash hash -3) (- width 6))))
         (tree-z (+ origin-z 3 (mod (ash hash -11) (- depth 6))))
         (surface (birthday-meadow-surface-height
                   source tree-x tree-z height))
         (trunk-height (+ 3 (mod (ash hash -23) 2)))
         (crown (+ surface trunk-height)))
    (when (and (zerop (mod (ash hash -16) 3))
               (> (luvcraft:little-world-value-noise
                   source tree-x tree-z 48 29)
                  -0.1d0)
               (> (birthday-origin-distance tree-x tree-z)
                  *tree-exclusion-radius*)
               (< (+ crown 2) height))
      (loop for y from (1+ surface) to crown
            do (funcall emit luvcraft::*wood-block* tree-x y tree-z))
      (loop for x from (- tree-x 2) to (+ tree-x 2) do
        (loop for z from (- tree-z 2) to (+ tree-z 2)
              when (<= (+ (abs (- x tree-x)) (abs (- z tree-z))) 3)
                do (funcall emit luvcraft::*leaf-block* x crown z)))
      (loop for x from (1- tree-x) to (1+ tree-x) do
        (loop for z from (1- tree-z) to (1+ tree-z)
              do (funcall emit luvcraft::*leaf-block* x (1+ crown) z)))
      (funcall emit luvcraft::*leaf-block* tree-x (+ crown 2) tree-z))))

(defun populate-birthday-meadow-chunk (source world chunk-x chunk-z)
  "Plant this chunk's tree, if it has one, into resident terrain."
  (luvcraft::with-world-change-transaction (world)
    (let* ((shape (luvcraft::voxel-space-chunk-shape
                   (luvcraft::block-world-space world)))
           (width (luvcraft::chunk-shape-width shape))
           (height (luvcraft::chunk-shape-height shape))
           (depth (luvcraft::chunk-shape-depth shape)))
      (map-birthday-tree-blocks
       source width height depth chunk-x chunk-z
       (lambda (block x y z)
         (setf (luvcraft:world-block-at world x y z) block))))))

(defmethod luvcraft:populate-block-world-chunk
    ((source birthday-world-source) (world luvcraft::block-world)
     chunk-x chunk-y chunk-z)
  (unless (zerop chunk-y)
    (error "The birthday meadow populates only chunk layer zero."))
  (populate-birthday-meadow-chunk source world chunk-x chunk-z))

;;; Streaming.  The inherited MAKE-BLOCK-CHUNK-LOAD-REQUEST would capture
;;; stock landmarks, and its request's PERFORM method rebuilds a stock
;;; source in the worker, so the meadow carries its own request class.

(defun birthday-landmarks-for-chunk (source world key)
  "Capture the meadow landmarks whose owned sites lie inside chunk KEY."
  (destructuring-bind (chunk-x chunk-y chunk-z) key
    (declare (ignore chunk-y))
    (let* ((shape (luvcraft::voxel-space-chunk-shape
                   (luvcraft::block-world-space world)))
           (width (luvcraft::chunk-shape-width shape))
           (height (luvcraft::chunk-shape-height shape))
           (depth (luvcraft::chunk-shape-depth shape))
           (minimum-x (* chunk-x width))
           (minimum-z (* chunk-z depth))
           (landmarks nil))
      (map-birthday-tree-blocks
       source width height depth chunk-x chunk-z
       (lambda (block x y z)
         (when (and (<= minimum-x x) (< x (+ minimum-x width))
                    (<= minimum-z z) (< z (+ minimum-z depth))
                    (<= 0 y) (< y height))
           (push (list block x y z) landmarks))))
      (nreverse landmarks))))

(defclass birthday-world-load-request
    (luvcraft::little-world-load-request)
  ()
  (:documentation
   "A chunk load whose worker rebuilds a BIRTHDAY-WORLD-SOURCE, so
off-thread terrain matches the meadow rather than the stock hills."))

(defmethod luvcraft:make-block-chunk-load-request
    ((source birthday-world-source) world key demand-token priority)
  (let* ((shape (luvcraft::voxel-space-chunk-shape
                 (luvcraft::block-world-space world)))
         (width (luvcraft::chunk-shape-width shape))
         (height (luvcraft::chunk-shape-height shape))
         (depth (luvcraft::chunk-shape-depth shape))
         (captured-edits nil))
    (maphash
     (lambda (coordinate block)
       (destructuring-bind (x y z) coordinate
         (when (and (= (floor x width) (first key))
                    (= (floor y height) (second key))
                    (= (floor z depth) (third key)))
           (push (list block x y z) captured-edits))))
     (luvcraft::block-edit-overlay-entries
      (luvcraft:little-world-source-edits source)))
    (make-instance
     'birthday-world-load-request
     :key (list :load key)
     :priority priority
     :seed (luvcraft:little-world-source-seed source)
     :demand-token demand-token
     :width width :height height :depth depth
     :landmarks (birthday-landmarks-for-chunk source world key)
     :edits captured-edits)))

(luv:zdefmethod (luvcraft::perform-production-request
                 :zone :production/load-chunk)
    ((request birthday-world-load-request))
  "Generate one isolated meadow chunk and transfer its content columns."
  (destructuring-bind (chunk-x chunk-y chunk-z)
      (second (luvcraft::production-request-key request))
    (let* ((source (make-instance
                    'birthday-world-source
                    :seed (luvcraft::little-world-load-request-seed
                           request)))
           (world (luvcraft::make-block-world
                   :chunk-width (luvcraft::little-world-load-request-width
                                 request)
                   :chunk-height (luvcraft::little-world-load-request-height
                                  request)
                   :chunk-depth (luvcraft::little-world-load-request-depth
                                 request)
                   :source source)))
      (luvcraft:materialize-block-world-chunk
       source world chunk-x chunk-y chunk-z)
      (dolist (landmark (luvcraft::little-world-load-request-landmarks
                         request))
        (destructuring-bind (block x y z) landmark
          (setf (luvcraft:world-block-at world x y z) block)))
      (dolist (edit (luvcraft::little-world-load-request-edits request))
        (destructuring-bind (block x y z) edit
          (setf (luvcraft:world-block-at world x y z) block)))
      (let ((chunk (luvcraft::world-chunk-at
                    world chunk-x chunk-y chunk-z)))
        (make-instance 'luvcraft:block-chunk-load-payload
                       :key (list chunk-x chunk-y chunk-z)
                       :content (luvcraft::block-chunk-content chunk))))))

;;; Persistence.  Without these two methods a saved meadow would round-trip
;;; through the inherited :LITTLE-WORLD kind and wake up with stock hills.

(defconstant +birthday-world-source-version+ 1)

(defmethod luvcraft:world-source-save-description
    ((source birthday-world-source))
  (list :birthday-meadow
        :source-version +birthday-world-source-version+
        :seed (luvcraft:little-world-source-seed source)
        :edits (luvcraft::block-edit-overlay-save-descriptions
                (luvcraft:little-world-source-edits source))))

(defmethod luvcraft:restore-world-source-save-description
    ((kind (eql :birthday-meadow)) description)
  (let ((version (luvcraft::description-value
                  description :source-version "birthday-meadow source"))
        (seed (luvcraft::description-value
               description :seed "birthday-meadow source"))
        (edits (luvcraft::description-value
                description :edits "birthday-meadow source")))
    (unless (eql version +birthday-world-source-version+)
      (luvcraft::invalid-luvcraft-save
       "Birthday-meadow source version ~S is unsupported; expected ~D."
       version +birthday-world-source-version+))
    (unless (integerp seed)
      (luvcraft::invalid-luvcraft-save
       "A birthday-meadow seed must be an integer, not ~S." seed))
    (make-instance 'birthday-world-source
                   :seed seed
                   :edits (luvcraft::restore-block-edit-overlay edits))))

(defun make-birthday-world (&key (seed 121) (chunk-radius 4)
                                 (chunk-height 32))
  "Make the birthday meadow with resident chunks around the party lawn.

CHUNK-HEIGHT defaults to 32 so there is sky room for balloons and
fireworks.  CHUNK-RADIUS NIL leaves the resident set empty for streaming
to fill, the way MAKE-EMPTY-LITTLE-BLOCK-WORLD does."
  (check-type chunk-height (integer 8))
  (let* ((source (make-instance 'birthday-world-source :seed seed))
         (world (luvcraft::make-block-world
                 :id (list :birthday-meadow seed)
                 :chunk-height chunk-height
                 :source source)))
    (when chunk-radius
      (check-type chunk-radius (integer 0))
      (luvcraft:center-little-world-residency
       source world 0 0 :radius chunk-radius))
    world))
