;;; Rigid spheres in the block world: bodies, contacts, and the soft-step
;;; solver that moves them.
;;;
;;; #G7Q3XR is the design this grew from and the Box3D field notes
;;; (#W2M9FJ, #S5K3WM, #R7F2QH, #G3W7KD) are the landmark it steers by.  The
;;; shape of the thing is the seam #K2F6WD asks for: CLOS objects and generic
;;; functions own the world, its parameters, and its events; the bodies and
;;; contacts themselves are rows in generated columnar buffers, and every
;;; per-substep loop is a closed loop over borrowed specialized arrays.
;;;
;;; What is here, in the order the step runs it:
;;;
;;;   - Bodies are spheres.  A body has a stable handle (an index into an id
;;;     table plus a generation, #N7Q4XS) and lives as one row of whichever
;;;     SET currently holds it: the awake set the step iterates, or the
;;;     sleeping set it does not.  Sleep is relocation, not a flag; the id
;;;     table is the forwarding address every relocation patches (#B5N8JT).
;;;
;;;   - Terrain is the static tree (#D9W4CH): a sphere asks the voxel lattice
;;;     directly which exposed faces it is near, through a probe that keeps
;;;     one chunk's storage borrowed between neighbouring questions.  Moving
;;;     things that are not spheres -- the player, an animal -- enter as
;;;     KINEMATIC BOXES the client posts each step.
;;;
;;;   - Body–body pairs come from a uniform hash grid over both sets.  An
;;;     awake body which is moving wakes a sleeping one it reaches; one which
;;;     is merely resting on it leans on it as if it were terrain.
;;;
;;;   - Contacts are persistent rows keyed by their pair (#C6J9RW), found by
;;;     hash each step, pruned when their pair separates.  What persists is
;;;     the accumulated impulse -- the warm start -- and the touching state
;;;     the begin/end events derive from.
;;;
;;;   - Each step the contacts are COLOURED so that no two in one colour
;;;     share a dynamic body (#G3W7KD), and packed in colour order into a
;;;     constraint buffer.  That disjointness is what lets four contacts of a
;;;     colour be solved in one f32.4 lane group (PHYSICS-SIMD.LISP) with no
;;;     gather or scatter ever colliding.  Static contacts take the highest
;;;     colours so they are solved last in every iteration.
;;;
;;;   - The solver is the Soft Step skeleton at its minimum: substeps of
;;;     integrate velocities, warm start, solve with bias, integrate
;;;     positions, relax without bias; then restitution once, then the
;;;     impulses are stored back on their contacts.
;;;
;;;   - Events are buffered facts polled after the step (#F8H3PW): a contact
;;;     began or ended, a body hit something hard, expired, slept, or woke.
;;;
;;; Everything numeric here is single-float, and every column is a
;;; SIMPLE-ARRAY SINGLE-FLOAT (*) or of fixnums, so that the scalar reference
;;; kernels and the four-wide kernels read the same storage.  Reproducibility
;;; is of the same-image, fixed-code kind #S6T2MV settles on: the step's
;;; order is deterministic and a state hash can police it, and no more.

(in-package #:luvcraft)

;;; ---------------------------------------------------------------------
;;; Parameters.  Box3D's constants, in cells and seconds; each one is a
;;; special so it can be turned while a pile is settling.

(defparameter *physics-gravity* -24.0
  "Gravitational acceleration, cells per second squared, along Y.")
(defparameter *physics-substeps* 4
  "Substeps per step: the Soft Step loop runs this many times.")
(defparameter *physics-contact-hertz* 30.0
  "Contact stiffness as a frequency; zero would be rigid.")
(defparameter *physics-contact-damping-ratio* 10.0
  "Contact softness damping ratio.")
(defparameter *physics-contact-push-max-velocity* 3.0
  "The most a soft contact will push overlapping bodies apart, cells/s.")
(defparameter *physics-linear-slop* 0.005
  "How much overlap a contact tolerates before it pushes, in cells.")
(defparameter *physics-speculative-distance* 0.02
  "How far apart two things may be and still have a contact between them.")
(defparameter *physics-restitution-threshold* 1.0
  "Approach speed below which nothing bounces, cells/s.")
(defparameter *physics-sleep-speed* 0.05
  "Below this speed a body accumulates sleep time, cells/s.")
(defparameter *physics-sleep-seconds* 0.5
  "How long a body must be slow before it goes to sleep.")
(defparameter *physics-wake-speed* 0.2
  "An awake body faster than this wakes the sleeping bodies it reaches.")
(defparameter *physics-hit-speed* 2.0
  "Approach speed above which a contact reports a hit event, cells/s.")
(defparameter *physics-max-colors* 24
  "How many disjoint constraint colours to try before the overflow colour.")
(defparameter *physics-terrain-margin* 0.5
  "How far past its radius a body looks for terrain and boxes, in cells.
Wide enough that a fast body's contacts exist a step before it lands.")

;;; ---------------------------------------------------------------------
;;; Handles.  A body handle is a fixnum: the id-table index shifted up
;;; sixteen bits over a generation.  A destroyed body's slot is reused with
;;; the next generation, so a stale handle answers BODY-ALIVE-P with NIL
;;; rather than naming whatever took its place. #N7Q4XS

(defconstant +physics-handle-generation-bits+ 16)
(defconstant +physics-handle-generation-mask+ #xffff)

(declaim (inline physics-handle-index physics-handle-generation
                 make-physics-handle))
(defun physics-handle-index (handle)
  (declare (fixnum handle))
  (ash handle (- +physics-handle-generation-bits+)))
(defun physics-handle-generation (handle)
  (declare (fixnum handle))
  (logand handle +physics-handle-generation-mask+))
(defun make-physics-handle (index generation)
  (declare (fixnum index generation))
  (logior (ash index +physics-handle-generation-bits+)
          (logand generation +physics-handle-generation-mask+)))

(defconstant +physics-set-free+ 0
  "An id-table slot naming no body.")
(defconstant +physics-set-awake+ 1)
(defconstant +physics-set-sleeping+ 2)

(defconstant +physics-no-body+ -1
  "The local index that means 'no body': the static side of a contact.")

;;; The id table: three parallel fixnum columns plus a free list.  SET says
;;; which body set holds the body and LOCAL where in it; GENERATION is the
;;; handle's, incremented on every reuse of the slot.

(defstruct (physics-id-table (:constructor %make-physics-id-table))
  (set (make-array 64 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (local (make-array 64 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (generation (make-array 64 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (free (make-array 16 :element-type 'fixnum :fill-pointer 0 :adjustable t))
  (next 0 :type fixnum))

(defun make-physics-id-table ()
  (%make-physics-id-table))

(defun physics-id-table-grow (table minimum)
  (let* ((old (physics-id-table-set table))
         (capacity (max minimum (* 2 (length old)))))
    (flet ((grow (column)
             (let ((new (make-array capacity :element-type 'fixnum
                                             :initial-element 0)))
               (replace new column)
               new)))
      (setf (physics-id-table-set table) (grow (physics-id-table-set table))
            (physics-id-table-local table)
            (grow (physics-id-table-local table))
            (physics-id-table-generation table)
            (grow (physics-id-table-generation table)))))
  table)

(defun physics-id-table-allocate (table set local)
  "Claim an id slot for a body now at LOCAL in SET; return its handle."
  (declare (fixnum set local))
  (let* ((free (physics-id-table-free table))
         (index (if (plusp (fill-pointer free))
                    (vector-pop free)
                    (let ((next (physics-id-table-next table)))
                      (when (>= next (length (physics-id-table-set table)))
                        (physics-id-table-grow table (1+ next)))
                      (setf (physics-id-table-next table) (1+ next))
                      next))))
    (declare (fixnum index))
    (setf (aref (physics-id-table-set table) index) set
          (aref (physics-id-table-local table) index) local)
    (make-physics-handle index (aref (physics-id-table-generation table) index))))

(defun physics-id-table-release (table index)
  "Give INDEX's slot back; the next body to take it gets a new generation."
  (declare (fixnum index))
  (setf (aref (physics-id-table-set table) index) +physics-set-free+
        (aref (physics-id-table-generation table) index)
        (logand (1+ (aref (physics-id-table-generation table) index))
                +physics-handle-generation-mask+))
  (vector-push-extend index (physics-id-table-free table))
  table)

(declaim (inline physics-id-table-handle-live-p))
(defun physics-id-table-handle-live-p (table handle)
  (declare (fixnum handle))
  (let ((index (physics-handle-index handle)))
    (and (< index (physics-id-table-next table))
         (/= (aref (physics-id-table-set table) index) +physics-set-free+)
         (= (aref (physics-id-table-generation table) index)
            (physics-handle-generation handle)))))

;;; ---------------------------------------------------------------------
;;; Body sets.  One generated columnar layout serves both the awake and the
;;; sleeping set; a sleeping body simply keeps zero velocity in columns
;;; nobody iterates.  The X Y Z here are absolute; DX DY DZ accumulate the
;;; substeps' motion within one step and are folded into X Y Z at its end,
;;; so that a contact's separation is a small delta added to a base value
;;; and a static side needs no state (#S5K3WM).  Orientation is only for
;;; the eye: a sphere's contact geometry does not turn with it.

(records:define-columnar-buffer
    (physics-body-columns
     :quantities
     (((x y z) (:quantity :world-position :unit :cell :tensor-order 1))
      ((vx vy vz) (:quantity :world-velocity :unit ((:cell 1) (:second -1))
                             :tensor-order 1))))
  ;; The body's handle, so a moved row can patch its id-table entry.
  (handle 0 :type fixnum)
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (z 0f0 :type single-float)
  (dx 0f0 :type single-float)
  (dy 0f0 :type single-float)
  (dz 0f0 :type single-float)
  (vx 0f0 :type single-float)
  (vy 0f0 :type single-float)
  (vz 0f0 :type single-float)
  ;; Angular velocity, radians per second, world axes.
  (wx 0f0 :type single-float)
  (wy 0f0 :type single-float)
  (wz 0f0 :type single-float)
  ;; Orientation as a unit quaternion (x y z w), for the renderer.
  (qx 0f0 :type single-float)
  (qy 0f0 :type single-float)
  (qz 0f0 :type single-float)
  (qw 1f0 :type single-float)
  (radius 0.25f0 :type single-float)
  (inverse-mass 1f0 :type single-float)
  ;; A solid sphere: 1 / (2/5 m r^2).
  (inverse-inertia 1f0 :type single-float)
  (restitution 0.3f0 :type single-float)
  (friction 0.5f0 :type single-float)
  (rolling-resistance 0.01f0 :type single-float)
  ;; Linear damping, per second: air drag, or something thicker.
  (damping 0.05f0 :type single-float)
  ;; What the body looks like and is: an index into the client's palette
  ;; of body kinds.  The physics never reads it.
  (kind 0 :type fixnum)
  ;; See +PHYSICS-BODY-...+ below.
  (flags 0 :type fixnum)
  ;; Seconds left to live; negative means immortal.
  (lifetime -1f0 :type single-float)
  ;; How long the body has been slow, toward sleep.
  (sleep-time 0f0 :type single-float))

(defconstant +physics-body-collides-with-bodies+ 1
  "Set on bodies that meet other bodies; a spray droplet does not.")
(defconstant +physics-body-never-sleeps+ 2)
(defconstant +physics-body-hit-report+ 4
  "Set on bodies whose hard contacts should be reported as hit events.")

(defmacro define-columnar-remove-swap (name buffer-type)
  "Define (NAME BUFFER INDEX): move BUFFER's last row into INDEX and shrink.

Return the row index that was moved into INDEX, or NIL when INDEX was the
last row and nothing had to move.  The caller must then patch whatever
pointed at the moved row: this is the forwarding-address discipline of
#W2M9FJ, kept explicit at every call.  Every lane is moved through its
precise array type, so no float is boxed on the way."
  (let* ((definition (records:columnar-layout-definition-for buffer-type))
         (lanes (records:columnar-layout-definition-lanes definition))
         (length-reader (intern (format nil "~A-LENGTH" buffer-type)
                                (symbol-package buffer-type))))
    (flet ((lane-form (lane)
             `(the (simple-array
                    ,(upgraded-array-element-type
                      (luv.arithmetic:declaration-representation-type lane))
                    (*))
                   (,(intern (format nil "~A-~A-LANE" buffer-type
                                     (records:columnar-lane-definition-name lane))
                             (symbol-package buffer-type))
                    buffer))))
      `(defun ,name (buffer index)
         (declare (fixnum index) (optimize (speed 3) (safety 1)))
         (let ((last (1- (,length-reader buffer))))
           (declare (fixnum last))
           (unless (<= 0 index last)
             (error "Row ~D is not in ~S." index buffer))
           (unless (= index last)
             ,@(loop for lane in lanes
                     collect `(let ((lane ,(lane-form lane)))
                                (setf (aref lane index) (aref lane last)))))
           ,@(loop for lane in lanes
                   when (records:columnar-lane-definition-clear-on-remove-p lane)
                     collect `(setf (aref ,(lane-form lane) last)
                                    ,(records:columnar-lane-definition-initial-element lane)))
           (setf (,length-reader buffer) last)
           (if (= index last) nil last))))))

(define-columnar-remove-swap physics-body-columns-remove-swap
  physics-body-columns)

(defmacro define-columnar-copy-row (name buffer-type)
  "Define (NAME SOURCE INDEX DESTINATION): push SOURCE's row INDEX onto
DESTINATION and return its new index there."
  (let* ((definition (records:columnar-layout-definition-for buffer-type))
         (lanes (records:columnar-layout-definition-lanes definition))
         (push-name (intern (format nil "~A-PUSH" buffer-type)
                            (symbol-package buffer-type)))
         (length-reader (intern (format nil "~A-LENGTH" buffer-type)
                                (symbol-package buffer-type))))
    `(defun ,name (source index destination)
       (declare (fixnum index) (optimize (speed 3) (safety 1)))
       (,push-name
        destination
        ,@(loop for lane in lanes
                collect `(aref (the (simple-array
                                     ,(upgraded-array-element-type
                                       (luv.arithmetic:declaration-representation-type lane))
                                     (*))
                                    (,(intern (format nil "~A-~A-LANE" buffer-type
                                                      (records:columnar-lane-definition-name lane))
                                              (symbol-package buffer-type))
                                     source))
                               index)))
       (1- (,length-reader destination)))))

(define-columnar-copy-row physics-body-columns-copy-row physics-body-columns)

(defmacro %lane (buffer-type buffer lane)
  "BUFFER's LANE array with its precise specialized type, for the loops
that borrow one lane at a time rather than a whole row schema."
  (let* ((definition (records:columnar-layout-definition-for buffer-type))
         (definition-lane
           (or (records:columnar-layout-lane-definition definition lane)
               (error "There is no ~S lane in ~S." lane buffer-type))))
    `(the (simple-array
           ,(upgraded-array-element-type
             (luv.arithmetic:declaration-representation-type definition-lane))
           (*))
          (,(intern (format nil "~A-~A-LANE" buffer-type lane)
                    (symbol-package buffer-type))
           ,buffer))))

;;; ---------------------------------------------------------------------
;;; Kinematic boxes: the axis-aligned boxes of things that are not spheres
;;; and are moved by their own minds -- the player, an animal.  The client
;;; posts them afresh every step; a body that meets one is pushed as if by
;;; terrain moving at the box's velocity, and the box feels nothing.  OWNER
;;; is the client's tag, handed back in hit events.

(records:define-columnar-buffer physics-box-columns
  (min-x 0f0 :type single-float)
  (min-y 0f0 :type single-float)
  (min-z 0f0 :type single-float)
  (max-x 0f0 :type single-float)
  (max-y 0f0 :type single-float)
  (max-z 0f0 :type single-float)
  (vx 0f0 :type single-float)
  (vy 0f0 :type single-float)
  (vz 0f0 :type single-float)
  (owner nil :type t :clear-on-remove t))

;;; ---------------------------------------------------------------------
;;; Contacts.  One row per pair within speculative distance, persistent
;;; across steps so its impulses can warm-start the next solve.  KEY names
;;; the pair; KIND says what the other side is; LAST-STEP is when the pair
;;; was last within reach, so pruning is one comparison per row.

(defconstant +physics-contact-body+ 0)
(defconstant +physics-contact-terrain+ 1)
(defconstant +physics-contact-box+ 2)

(records:define-columnar-buffer physics-contact-columns
  (key 0 :type fixnum)
  (kind 0 :type fixnum)
  ;; Handles, never local indices: rows move, handles do not.
  (handle-a 0 :type fixnum)
  (handle-b 0 :type fixnum)
  ;; For a box contact, the box's owner, for hit events.
  (owner nil :type t :clear-on-remove t)
  (last-step 0 :type fixnum)
  (touching 0 :type fixnum)
  ;; The accumulated impulses this pair carries from step to step.
  (normal-impulse 0f0 :type single-float)
  (tangent-impulse-1 0f0 :type single-float)
  (tangent-impulse-2 0f0 :type single-float)
  (rolling-impulse-x 0f0 :type single-float)
  (rolling-impulse-y 0f0 :type single-float)
  (rolling-impulse-z 0f0 :type single-float)
  ;; The step's manifold: one point.  The normal points from A toward the
  ;; other side; P is the contact point on the other side, in the world; KV
  ;; is the other side's velocity when it is a kinematic box.
  (nx 0f0 :type single-float)
  (ny 0f0 :type single-float)
  (nz 0f0 :type single-float)
  (px 0f0 :type single-float)
  (py 0f0 :type single-float)
  (pz 0f0 :type single-float)
  (kvx 0f0 :type single-float)
  (kvy 0f0 :type single-float)
  (kvz 0f0 :type single-float)
  (separation 0f0 :type single-float))

(define-columnar-remove-swap physics-contact-columns-remove-swap
  physics-contact-columns)

;;; The per-step constraint buffer: the contacts that will be solved,
;;; written in colour order (#G3W7KD) with everything the solver iterates
;;; over precomputed, so that a substep touches only these lanes and the
;;; awake body columns.  A is always a dynamic body; B is a dynamic body,
;;; or the static dummy row past the end of the awake set.  Anchors are
;;; from the body centres to the contact point.  KV is the velocity of a
;;; kinematic other side (zero for terrain and bodies).

(records:define-columnar-buffer physics-constraint-columns
  ;; The persistent contact row whose impulses this constraint carries.
  (contact 0 :type fixnum)
  (body-a 0 :type fixnum)
  (body-b 0 :type fixnum)
  (nx 0f0 :type single-float)
  (ny 0f0 :type single-float)
  (nz 0f0 :type single-float)
  (t1x 0f0 :type single-float)
  (t1y 0f0 :type single-float)
  (t1z 0f0 :type single-float)
  (t2x 0f0 :type single-float)
  (t2y 0f0 :type single-float)
  (t2z 0f0 :type single-float)
  (rax 0f0 :type single-float)
  (ray 0f0 :type single-float)
  (raz 0f0 :type single-float)
  (rbx 0f0 :type single-float)
  (rby 0f0 :type single-float)
  (rbz 0f0 :type single-float)
  (kvx 0f0 :type single-float)
  (kvy 0f0 :type single-float)
  (kvz 0f0 :type single-float)
  ;; Separation at the start of the step, already less the linear slop.
  (separation 0f0 :type single-float)
  (normal-mass 0f0 :type single-float)
  (tangent-mass 0f0 :type single-float)
  (rolling-mass 0f0 :type single-float)
  (restitution 0f0 :type single-float)
  (friction 0f0 :type single-float)
  (rolling-resistance 0f0 :type single-float)
  ;; The approach speed before solving, which restitution answers to.
  (relative-velocity 0f0 :type single-float)
  ;; The normal impulses of every substep summed: restitution and hit
  ;; events only act on contacts that actually pushed.
  (total-normal-impulse 0f0 :type single-float)
  ;; The softness this contact solves with (#R7F2QH); a static contact is
  ;; stiffer than one between bodies.  BIAS-RATE already carries MASS-SCALE.
  (bias-rate 0f0 :type single-float)
  (mass-scale 1f0 :type single-float)
  (impulse-scale 0f0 :type single-float)
  (normal-impulse 0f0 :type single-float)
  (tangent-impulse-1 0f0 :type single-float)
  (tangent-impulse-2 0f0 :type single-float)
  (rolling-impulse-x 0f0 :type single-float)
  (rolling-impulse-y 0f0 :type single-float)
  (rolling-impulse-z 0f0 :type single-float))

;;; ---------------------------------------------------------------------
;;; Events: what the step found out, for the client to poll (#F8H3PW).
;;; A begin or end names two handles (the second is +PHYSICS-NO-BODY+ for
;;; terrain); a hit carries the contact point and approach speed; the rest
;;; name one body.  OWNER is a box contact's owner.

(records:define-columnar-buffer physics-event-columns
  (kind :none :type keyword)
  (handle-a 0 :type fixnum)
  (handle-b 0 :type fixnum)
  (owner nil :type t :clear-on-remove t)
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (z 0f0 :type single-float)
  (speed 0f0 :type single-float))

;;; ---------------------------------------------------------------------
;;; The world.

(defclass physics-world ()
  ((ids :initform (make-physics-id-table) :reader physics-world-ids)
   (awake :initform (make-physics-body-columns :capacity 256)
          :reader physics-world-awake)
   (sleeping :initform (make-physics-body-columns :capacity 64)
             :reader physics-world-sleeping)
   (boxes :initform (make-physics-box-columns :capacity 16)
          :reader physics-world-boxes)
   (contacts :initform (make-physics-contact-columns :capacity 512)
             :reader physics-world-contacts)
   ;; Contact key -> contact row.  Patched on every swap-remove.
   (contact-index :initform (make-hash-table :test #'eql)
                  :reader physics-world-contact-index)
   (constraints :initform (make-physics-constraint-columns :capacity 512)
                :reader physics-world-constraints)
   ;; Where each colour's constraints begin in CONSTRAINTS; one more entry
   ;; than colours, the overflow colour last.
   (color-starts :initform (make-array (+ 2 *physics-max-colors*)
                                       :element-type 'fixnum
                                       :initial-element 0)
                 :accessor physics-world-color-starts)
   ;; The colouring scratch: one bit-vector of awake bodies per colour, and
   ;; the colour each contact was given this step.
   (color-bits :initform (make-array (1+ *physics-max-colors*) :initial-element nil)
               :accessor physics-world-color-bits)
   (contact-colors :initform (make-array 512 :element-type 'fixnum
                                             :initial-element 0)
                   :accessor physics-world-contact-colors)
   (events :initform (make-physics-event-columns :capacity 64)
           :reader physics-world-events)
   (grid :initform (make-physics-grid) :reader physics-world-grid)
   (terrain :initarg :terrain :initform nil :accessor physics-world-terrain
            :documentation "The block world the bodies collide with, or NIL.")
   (probe :initform (make-terrain-probe) :reader physics-world-probe)
   (step-count :initform 0 :accessor physics-world-step-count)
   (step-seconds :initform (/ 1f0 60f0) :accessor physics-world-step-seconds)
   ;; The kernel family this world solves with; see PHYSICS-SIMD.LISP.
   (kernels :initform :scalar :accessor physics-world-kernels)
   ;; What the last step cost, so it can be read off the world.
   (last-step-real-seconds :initform 0d0
                           :accessor physics-world-last-step-real-seconds)
   (last-step-contact-count :initform 0
                            :accessor physics-world-last-step-contact-count)
   (last-step-color-count :initform 0
                          :accessor physics-world-last-step-color-count)
   ;; Contact rows made and dropped over the world's life: how much the
   ;; pairs churn, which is what the hash and the buffer pay for.
   (contacts-made :initform 0 :accessor physics-world-contacts-made)
   (contacts-dropped :initform 0 :accessor physics-world-contacts-dropped))
  (:documentation
   "Every sphere the block world is simulating, and how they touch."))

(defmethod print-object ((world physics-world) stream)
  (print-unreadable-object (world stream :type t)
    (format stream "~D awake, ~D asleep, ~D contacts, step ~D, ~A"
            (physics-body-columns-length (physics-world-awake world))
            (physics-body-columns-length (physics-world-sleeping world))
            (physics-contact-columns-length (physics-world-contacts world))
            (physics-world-step-count world)
            (physics-world-kernels world))))

(defun make-physics-world (&key terrain (kernels :fastest))
  (let ((world (make-instance 'physics-world :terrain terrain)))
    (setf (physics-world-kernels world)
          (if (eq kernels :fastest) (fastest-physics-kernels) kernels))
    world))

(defun physics-world-body-count (world)
  (+ (physics-body-columns-length (physics-world-awake world))
     (physics-body-columns-length (physics-world-sleeping world))))

;;; ---------------------------------------------------------------------
;;; Creating, finding, and destroying bodies.

(defun spawn-physics-body
    (world x y z &key (radius 0.25) (mass 1.0) (vx 0.0) (vy 0.0) (vz 0.0)
                      (restitution 0.3) (friction 0.5) (rolling-resistance 0.01)
                      (damping 0.05) (kind 0)
                      (collides-with-bodies-p t) (hit-report-p nil)
                      (lifetime nil))
  "Add a sphere to WORLD and return its handle."
  (let* ((awake (physics-world-awake world))
         (radius (coerce radius 'single-float))
         (mass (coerce mass 'single-float))
         (inverse-mass (if (plusp mass) (/ mass) 0f0))
         (inverse-inertia
           (if (plusp mass) (/ (* 0.4f0 mass radius radius)) 0f0))
         (local (physics-body-columns-length awake))
         (handle (physics-id-table-allocate
                  (physics-world-ids world) +physics-set-awake+ local)))
    (physics-body-columns-push
     awake handle
     (coerce x 'single-float) (coerce y 'single-float) (coerce z 'single-float)
     0f0 0f0 0f0
     (coerce vx 'single-float) (coerce vy 'single-float) (coerce vz 'single-float)
     0f0 0f0 0f0
     0f0 0f0 0f0 1f0
     radius inverse-mass inverse-inertia
     (coerce restitution 'single-float) (coerce friction 'single-float)
     (coerce rolling-resistance 'single-float) (coerce damping 'single-float)
     kind
     (logior (if collides-with-bodies-p +physics-body-collides-with-bodies+ 0)
             (if hit-report-p +physics-body-hit-report+ 0))
     (if lifetime (coerce lifetime 'single-float) -1f0)
     0f0)
    handle))

(defun physics-body-alive-p (world handle)
  "Whether HANDLE still names a body in WORLD."
  (and (typep handle 'fixnum)
       (physics-id-table-handle-live-p (physics-world-ids world) handle)))

(declaim (inline physics-body-set-and-local))
(defun physics-body-set-and-local (world handle)
  "Return the set constant and local row of HANDLE's body, unchecked."
  (declare (fixnum handle))
  (let ((ids (physics-world-ids world))
        (index (physics-handle-index handle)))
    (values (aref (physics-id-table-set ids) index)
            (aref (physics-id-table-local ids) index))))

(defun physics-body-columns-for-set (world set)
  (declare (fixnum set))
  (cond ((= set +physics-set-awake+) (physics-world-awake world))
        ((= set +physics-set-sleeping+) (physics-world-sleeping world))
        (t (error "Set ~D holds no bodies." set))))

(defun physics-body-position (world handle)
  "Return HANDLE's body centre as three single-floats, or NIL when dead."
  (when (physics-body-alive-p world handle)
    (multiple-value-bind (set local) (physics-body-set-and-local world handle)
      (let ((columns (physics-body-columns-for-set world set)))
        (values (aref (physics-body-columns-x-lane columns) local)
                (aref (physics-body-columns-y-lane columns) local)
                (aref (physics-body-columns-z-lane columns) local))))))

(defun physics-body-velocity (world handle)
  "Return HANDLE's body velocity as three single-floats, or NIL when dead."
  (when (physics-body-alive-p world handle)
    (multiple-value-bind (set local) (physics-body-set-and-local world handle)
      (let ((columns (physics-body-columns-for-set world set)))
        (values (aref (physics-body-columns-vx-lane columns) local)
                (aref (physics-body-columns-vy-lane columns) local)
                (aref (physics-body-columns-vz-lane columns) local))))))

(defun physics-body-sleeping-p (world handle)
  (and (physics-body-alive-p world handle)
       (= +physics-set-sleeping+
          (nth-value 0 (physics-body-set-and-local world handle)))))

(defun (setf physics-body-velocity) (velocity world handle)
  "Set HANDLE's velocity from a list of three reals, waking the body."
  (when (physics-body-alive-p world handle)
    (wake-physics-body world handle)
    (multiple-value-bind (set local) (physics-body-set-and-local world handle)
      (let ((columns (physics-body-columns-for-set world set)))
        (destructuring-bind (vx vy vz) velocity
          (setf (aref (physics-body-columns-vx-lane columns) local)
                (coerce vx 'single-float)
                (aref (physics-body-columns-vy-lane columns) local)
                (coerce vy 'single-float)
                (aref (physics-body-columns-vz-lane columns) local)
                (coerce vz 'single-float))))))
  velocity)

(defun %relocate-physics-body (world handle from-set to-set)
  "Move HANDLE's row from FROM-SET to TO-SET, patching every forwarding
address the move disturbs."
  (declare (fixnum handle from-set to-set))
  (let* ((ids (physics-world-ids world))
         (index (physics-handle-index handle))
         (local (aref (physics-id-table-local ids) index))
         (from (physics-body-columns-for-set world from-set))
         (to (physics-body-columns-for-set world to-set))
         (new-local (physics-body-columns-copy-row from local to))
         (moved (physics-body-columns-remove-swap from local)))
    (setf (aref (physics-id-table-set ids) index) to-set
          (aref (physics-id-table-local ids) index) new-local)
    (when moved
      ;; The row that filled the hole belongs to another body: tell it.
      (let ((moved-handle (aref (physics-body-columns-handle-lane from) local)))
        (setf (aref (physics-id-table-local ids)
                    (physics-handle-index moved-handle))
              local)))
    new-local))

(defun wake-physics-body (world handle)
  "Bring HANDLE's body into the awake set if it was asleep; return T if so."
  (when (physics-body-alive-p world handle)
    (multiple-value-bind (set local) (physics-body-set-and-local world handle)
      (declare (ignore local))
      (when (= set +physics-set-sleeping+)
        (let ((new-local
                (%relocate-physics-body world handle
                                        +physics-set-sleeping+
                                        +physics-set-awake+)))
          (setf (aref (physics-body-columns-sleep-time-lane
                       (physics-world-awake world))
                      new-local)
                0f0))
        (physics-event-columns-push (physics-world-events world)
                                    :woke handle +physics-no-body+ nil
                                    0f0 0f0 0f0 0f0)
        t))))

(defun sleep-physics-body (world handle)
  "Move HANDLE's body out of the awake set; return T if it was awake."
  (when (physics-body-alive-p world handle)
    (multiple-value-bind (set local) (physics-body-set-and-local world handle)
      (declare (ignore local))
      (when (= set +physics-set-awake+)
        (let* ((new-local
                 (%relocate-physics-body world handle
                                         +physics-set-awake+
                                         +physics-set-sleeping+))
               (sleeping (physics-world-sleeping world)))
          ;; A sleeper is still: its velocity is not merely unread.
          (setf (aref (physics-body-columns-vx-lane sleeping) new-local) 0f0
                (aref (physics-body-columns-vy-lane sleeping) new-local) 0f0
                (aref (physics-body-columns-vz-lane sleeping) new-local) 0f0
                (aref (physics-body-columns-wx-lane sleeping) new-local) 0f0
                (aref (physics-body-columns-wy-lane sleeping) new-local) 0f0
                (aref (physics-body-columns-wz-lane sleeping) new-local) 0f0
                (aref (physics-body-columns-dx-lane sleeping) new-local) 0f0
                (aref (physics-body-columns-dy-lane sleeping) new-local) 0f0
                (aref (physics-body-columns-dz-lane sleeping) new-local) 0f0))
        (physics-event-columns-push (physics-world-events world)
                                    :slept handle +physics-no-body+ nil
                                    0f0 0f0 0f0 0f0)
        t))))

(defun destroy-physics-body (world handle)
  "Remove HANDLE's body from WORLD; its contacts go at the next prune."
  (when (physics-body-alive-p world handle)
    (multiple-value-bind (set local) (physics-body-set-and-local world handle)
      (let* ((ids (physics-world-ids world))
             (columns (physics-body-columns-for-set world set))
             (moved (physics-body-columns-remove-swap columns local)))
        (when moved
          (let ((moved-handle
                  (aref (physics-body-columns-handle-lane columns) local)))
            (setf (aref (physics-id-table-local ids)
                        (physics-handle-index moved-handle))
                  local)))
        (physics-id-table-release ids (physics-handle-index handle))
        t))))

(defun clear-physics-bodies (world)
  "Remove every body and contact from WORLD."
  (physics-body-columns-reset (physics-world-awake world))
  (physics-body-columns-reset (physics-world-sleeping world))
  (physics-contact-columns-reset (physics-world-contacts world))
  (clrhash (physics-world-contact-index world))
  (let ((ids (physics-world-ids world)))
    (dotimes (index (physics-id-table-next ids))
      (unless (= (aref (physics-id-table-set ids) index) +physics-set-free+)
        (physics-id-table-release ids index))))
  world)

(defmacro do-physics-bodies ((handle world &key (sets '(:awake :sleeping)))
                             &body body)
  "Run BODY with HANDLE bound to each body's handle in the named SETS.
BODY must not create or destroy bodies."
  (let ((columns (gensym "COLUMNS")) (index (gensym "INDEX")))
    `(dolist (,columns (list ,@(loop for set in sets
                                     collect (ecase set
                                               (:awake `(physics-world-awake ,world))
                                               (:sleeping `(physics-world-sleeping ,world))))))
       (dotimes (,index (physics-body-columns-length ,columns))
         (let ((,handle (aref (physics-body-columns-handle-lane ,columns) ,index)))
           ,@body)))))

;;; ---------------------------------------------------------------------
;;; Kinematic boxes: posted by the client before each step.

(defun clear-physics-boxes (world)
  (physics-box-columns-reset (physics-world-boxes world))
  world)

(defun post-physics-box (world min-x min-y min-z max-x max-y max-z
                         &key (vx 0.0) (vy 0.0) (vz 0.0) owner)
  "Tell WORLD about a moving box that bodies must not enter this step."
  (physics-box-columns-push
   (physics-world-boxes world)
   (coerce min-x 'single-float) (coerce min-y 'single-float)
   (coerce min-z 'single-float)
   (coerce max-x 'single-float) (coerce max-y 'single-float)
   (coerce max-z 'single-float)
   (coerce vx 'single-float) (coerce vy 'single-float) (coerce vz 'single-float)
   owner)
  world)

;;; ---------------------------------------------------------------------
;;; The terrain probe: the voxel lattice asked one cell at a time, with the
;;; last chunk's storage kept borrowed, so a body's neighbourhood -- almost
;;; always inside one chunk -- costs one hash lookup, not twenty-seven.
;;; Missing terrain is a wall, as it is for the walking bodies (see
;;; WORLD-TERRAIN-SOLID-P): a body near ground that has not streamed in
;;; leans on the boundary rather than falling through the world.

(defconstant +terrain-probe-ways+ 8
  "How many chunks the probe keeps borrowed at once.  A body's
neighbourhood straddles at most eight chunks.")

(defstruct (terrain-probe (:constructor make-terrain-probe ()))
  (world nil)
  (width 16 :type fixnum)
  (height 16 :type fixnum)
  (depth 16 :type fixnum)
  (world-height 16 :type fixnum)
  ;; The way the last question landed in, checked first.
  (current 0 :type fixnum)
  ;; The next way to evict.
  (next 0 :type fixnum)
  ;; Per way: the chunk coordinate held, its borrowed indices (or NIL for a
  ;; chunk that is not resident), which palette entries are solid, and how
  ;; long the palette was when that was computed.
  (chunk-xs (make-array +terrain-probe-ways+ :element-type 'fixnum
                                             :initial-element most-positive-fixnum)
   :type (simple-array fixnum (*)))
  (chunk-ys (make-array +terrain-probe-ways+ :element-type 'fixnum
                                             :initial-element most-positive-fixnum)
   :type (simple-array fixnum (*)))
  (chunk-zs (make-array +terrain-probe-ways+ :element-type 'fixnum
                                             :initial-element most-positive-fixnum)
   :type (simple-array fixnum (*)))
  (indices (make-array +terrain-probe-ways+ :initial-element nil) :type simple-vector)
  (solids (let ((solids (make-array +terrain-probe-ways+)))
            (dotimes (way +terrain-probe-ways+ solids)
              (setf (aref solids way)
                    (make-array 16 :element-type 'bit :initial-element 0))))
   :type simple-vector)
  (palettes (make-array +terrain-probe-ways+ :initial-element nil) :type simple-vector)
  (palette-lengths (make-array +terrain-probe-ways+ :element-type 'fixnum
                                                    :initial-element 0)
   :type (simple-array fixnum (*))))

(defun reset-terrain-probe (probe world)
  "Point PROBE at WORLD and forget whatever chunks it was holding."
  (setf (terrain-probe-world probe) world
        (terrain-probe-current probe) 0
        (terrain-probe-next probe) 0)
  (fill (terrain-probe-chunk-xs probe) most-positive-fixnum)
  (fill (terrain-probe-indices probe) nil)
  (fill (terrain-probe-palettes probe) nil)
  (when world
    (let ((shape (voxel-space-chunk-shape (block-world-space world))))
      (setf (terrain-probe-width probe) (chunk-shape-width shape)
            (terrain-probe-height probe) (chunk-shape-height shape)
            (terrain-probe-depth probe) (chunk-shape-depth shape)
            (terrain-probe-world-height probe) (chunk-shape-height shape))))
  probe)

(defun terrain-probe-refresh-solid (probe way)
  "Recompute the palette solidity bits of the chunk in WAY."
  (declare (fixnum way))
  (let* ((palette (aref (terrain-probe-palettes probe) way))
         (count (length palette))
         (bits (aref (terrain-probe-solids probe) way)))
    (declare (simple-bit-vector bits))
    (when (< (length bits) count)
      (setf bits (make-array (max count (* 2 (length bits)))
                             :element-type 'bit :initial-element 0)
            (aref (terrain-probe-solids probe) way) bits))
    (dotimes (index count)
      (setf (sbit bits index)
            (if (block-solid-p (aref palette index)) 1 0)))
    (setf (aref (terrain-probe-palette-lengths probe) way) count)))

(defun terrain-probe-visit-chunk (probe chunk-x chunk-y chunk-z)
  "Borrow chunk CHUNK-X,Y,Z into a way of PROBE and return the way."
  (declare (fixnum chunk-x chunk-y chunk-z))
  (let ((way (terrain-probe-next probe))
        (world (terrain-probe-world probe)))
    (setf (terrain-probe-next probe) (mod (1+ way) +terrain-probe-ways+)
          (aref (terrain-probe-chunk-xs probe) way) chunk-x
          (aref (terrain-probe-chunk-ys probe) way) chunk-y
          (aref (terrain-probe-chunk-zs probe) way) chunk-z)
    (multiple-value-bind (chunk present-p)
        (and world (world-chunk-at world chunk-x chunk-y chunk-z))
      (cond (present-p
             (with-block-content-storage (domain palette indices) chunk
               (declare (ignore domain))
               (setf (aref (terrain-probe-indices probe) way) indices
                     (aref (terrain-probe-palettes probe) way) palette)
               (terrain-probe-refresh-solid probe way)))
            (t
             (setf (aref (terrain-probe-indices probe) way) nil
                   (aref (terrain-probe-palettes probe) way) nil))))
    way))

(declaim (inline terrain-probe-way))
(defun terrain-probe-way (probe chunk-x chunk-y chunk-z)
  "The way holding chunk CHUNK-X,Y,Z, borrowing it if no way does."
  (declare (fixnum chunk-x chunk-y chunk-z) (optimize (speed 3) (safety 0)))
  (let ((current (terrain-probe-current probe))
        (xs (terrain-probe-chunk-xs probe))
        (ys (terrain-probe-chunk-ys probe))
        (zs (terrain-probe-chunk-zs probe)))
    (declare (fixnum current))
    (if (and (= chunk-x (aref xs current))
             (= chunk-y (aref ys current))
             (= chunk-z (aref zs current)))
        current
        (let ((way (or (dotimes (way +terrain-probe-ways+ nil)
                         (when (and (= chunk-x (aref xs way))
                                    (= chunk-y (aref ys way))
                                    (= chunk-z (aref zs way)))
                           (return way)))
                       (terrain-probe-visit-chunk probe chunk-x chunk-y chunk-z))))
          (declare (fixnum way))
          (setf (terrain-probe-current probe) way)
          way))))

(declaim (inline terrain-probe-solid-p))
(defun terrain-probe-solid-p (probe x y z)
  "Whether the cell at world X,Y,Z is solid, or a boundary standing in."
  (declare (fixnum x y z) (optimize (speed 3) (safety 0)))
  (let ((width (terrain-probe-width probe))
        (height (terrain-probe-height probe))
        (depth (terrain-probe-depth probe)))
    (declare (fixnum width height depth))
    (multiple-value-bind (chunk-x local-x) (floor x width)
      (multiple-value-bind (chunk-y local-y) (floor y height)
        (multiple-value-bind (chunk-z local-z) (floor z depth)
          (declare (fixnum chunk-x chunk-y chunk-z local-x local-y local-z))
          (let* ((way (terrain-probe-way probe chunk-x chunk-y chunk-z))
                 (indices (aref (terrain-probe-indices probe) way)))
            (if indices
                (let ((palette-index
                        (aref (the (simple-array (unsigned-byte 16) (*)) indices)
                              (+ local-x
                                 (the fixnum
                                      (* width
                                         (the fixnum
                                              (+ local-y
                                                 (the fixnum
                                                      (* height local-z))))))))))
                  ;; A palette can grow under us between visits.
                  (when (>= palette-index (aref (terrain-probe-palette-lengths probe) way))
                    (terrain-probe-refresh-solid probe way))
                  (= 1 (sbit (the simple-bit-vector (aref (terrain-probe-solids probe) way))
                             palette-index)))
                ;; Absent: solid below the world's height, air above it.
                (< y (terrain-probe-world-height probe)))))))))

;;; ---------------------------------------------------------------------
;;; The hash grid: every body, awake or asleep, dropped into a cell of a
;;; uniform grid keyed by its centre, as a linked list threaded through a
;;; NEXT column.  Rebuilt from scratch every step; a query walks the
;;; twenty-seven cells around a body.  Entries are handles.

(defstruct (physics-grid (:constructor make-physics-grid ()))
  (cell-size 1f0 :type single-float)
  (heads (make-array 4096 :element-type 'fixnum :initial-element -1)
   :type (simple-array fixnum (*)))
  (mask 4095 :type fixnum)
  (nexts (make-array 256 :element-type 'fixnum :initial-element -1)
   :type (simple-array fixnum (*)))
  (handles (make-array 256 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (count 0 :type fixnum))

(declaim (inline physics-grid-cell))
(defun physics-grid-cell (grid x y z)
  (declare (single-float x y z) (optimize (speed 3) (safety 0)))
  (let* ((inverse (/ (physics-grid-cell-size grid)))
         (ix (the fixnum (floor (* x inverse))))
         (iy (the fixnum (floor (* y inverse))))
         (iz (the fixnum (floor (* z inverse)))))
    (declare (fixnum ix iy iz))
    ;; A constant mask keeps the multiplies in modular machine arithmetic.
    (logand (logand (logxor (* ix 73856093) (* iy 19349663) (* iz 83492791))
                    #x3fffffff)
            (physics-grid-mask grid))))

(defun rebuild-physics-grid (world)
  "Drop every body of WORLD into the grid, sized to the largest body."
  (let* ((grid (physics-world-grid world))
         (awake (physics-world-awake world))
         (sleeping (physics-world-sleeping world))
         (total (+ (physics-body-columns-length awake)
                   (physics-body-columns-length sleeping))))
    ;; Enough cells that the lists stay short, as a power of two.
    (let ((wanted (max 4096 (let ((n 1)) (loop while (< n (* 2 total)) do (setf n (* n 2))) n))))
      (unless (= wanted (length (physics-grid-heads grid)))
        (setf (physics-grid-heads grid)
              (make-array wanted :element-type 'fixnum :initial-element -1)
              (physics-grid-mask grid) (1- wanted))))
    (when (< (length (physics-grid-nexts grid)) total)
      (let ((capacity (max total (* 2 (length (physics-grid-nexts grid))))))
        (setf (physics-grid-nexts grid)
              (make-array capacity :element-type 'fixnum :initial-element -1)
              (physics-grid-handles grid)
              (make-array capacity :element-type 'fixnum :initial-element 0))))
    (fill (physics-grid-heads grid) -1)
    (let ((largest 0.05f0))
      (declare (single-float largest))
      (dolist (columns (list awake sleeping))
        (records:with-columnar-buffer-storage
            ((length row (radii radius)) columns physics-body-columns)
          (declare (ignore row))
          (dotimes (index length)
            (setf largest (max largest (aref radii index))))))
      ;; A cell holds the largest body with room to spare, so a query
      ;; needs only the neighbouring cells.
      (setf (physics-grid-cell-size grid) (* 2.5f0 largest)))
    (setf (physics-grid-count grid) 0)
    (let ((heads (physics-grid-heads grid))
          (nexts (physics-grid-nexts grid))
          (handles (physics-grid-handles grid))
          (slot 0))
      (declare (fixnum slot))
      (dolist (columns (list awake sleeping))
        (records:with-columnar-buffer-storage
            ((length row (xs x) (ys y) (zs z) (hs handle))
             columns physics-body-columns)
          (declare (ignore row))
          (dotimes (index length)
            (let ((cell (physics-grid-cell grid (aref xs index)
                                           (aref ys index) (aref zs index))))
              (setf (aref handles slot) (aref hs index)
                    (aref nexts slot) (aref heads cell)
                    (aref heads cell) slot)
              (incf slot)))))
      (setf (physics-grid-count grid) slot))
    grid))

(defmacro do-physics-grid-neighbours ((handle grid x y z) &body body)
  "Run BODY with HANDLE bound to each body in the grid cells around X,Y,Z."
  (let ((g (gensym "GRID")) (cell-size (gensym "CELL"))
        (cx (gensym)) (cy (gensym)) (cz (gensym))
        (dx (gensym)) (dy (gensym)) (dz (gensym))
        (slot (gensym "SLOT")))
    `(let* ((,g ,grid)
            (,cell-size (physics-grid-cell-size ,g))
            (,cx (- ,x ,cell-size)) (,cy (- ,y ,cell-size)) (,cz (- ,z ,cell-size)))
       (declare (single-float ,cell-size ,cx ,cy ,cz))
       (dotimes (,dx 3)
         (dotimes (,dy 3)
           (dotimes (,dz 3)
             (let ((,slot (aref (physics-grid-heads ,g)
                                (physics-grid-cell
                                 ,g (+ ,cx (* ,dx ,cell-size))
                                 (+ ,cy (* ,dy ,cell-size))
                                 (+ ,cz (* ,dz ,cell-size))))))
               (declare (fixnum ,slot))
               (loop until (minusp ,slot)
                     do (let ((,handle (aref (physics-grid-handles ,g) ,slot)))
                          (declare (fixnum ,handle))
                          ,@body)
                        (setf ,slot (aref (physics-grid-nexts ,g) ,slot))))))))))


;;; ---------------------------------------------------------------------
;;; Finding contacts.  The key packs the pair; the hash gives the row.

(declaim (inline physics-contact-key))
(defun physics-contact-key (kind index-a other)
  "Pack a contact key: KIND in the low byte, OTHER above it, INDEX-A on top."
  (declare (fixnum kind index-a other))
  (logior (the fixnum (ash index-a 32))
          (the fixnum (ash (logand other #xffffff) 8))
          kind))

(declaim (inline physics-cell-key))
(defun physics-cell-key (x y z)
  (declare (fixnum x y z))
  (logior (ash (logand x #xff) 16) (ash (logand y #xff) 8) (logand z #xff)))

(defun ensure-physics-contact (world key kind handle-a handle-b owner)
  "Return the row of the contact KEY, making it if the pair is new.
The row's LAST-STEP is stamped with the current step."
  (declare (fixnum key kind handle-a handle-b) (optimize (speed 3) (safety 1)))
  (let* ((contacts (physics-world-contacts world))
         (index (physics-world-contact-index world))
         (step (physics-world-step-count world))
         (row (gethash key index)))
    (declare (fixnum step))
    (cond
      ((and row
            (= handle-a (aref (%lane physics-contact-columns contacts handle-a) row))
            (= handle-b (aref (%lane physics-contact-columns contacts handle-b) row)))
       (setf (aref (%lane physics-contact-columns contacts last-step) row) step
             (aref (%lane physics-contact-columns contacts owner) row) owner)
       row)
      (row
       ;; The key survived its pair: a body index came back to life.  Keep
       ;; the row, forget what it carried.
       (setf (aref (%lane physics-contact-columns contacts handle-a) row) handle-a
             (aref (%lane physics-contact-columns contacts handle-b) row) handle-b
             (aref (%lane physics-contact-columns contacts owner) row) owner
             (aref (%lane physics-contact-columns contacts last-step) row) step
             (aref (%lane physics-contact-columns contacts touching) row) 0
             (aref (%lane physics-contact-columns contacts normal-impulse) row) 0f0
             (aref (%lane physics-contact-columns contacts tangent-impulse-1) row) 0f0
             (aref (%lane physics-contact-columns contacts tangent-impulse-2) row) 0f0
             (aref (%lane physics-contact-columns contacts rolling-impulse-x) row) 0f0
             (aref (%lane physics-contact-columns contacts rolling-impulse-y) row) 0f0
             (aref (%lane physics-contact-columns contacts rolling-impulse-z) row) 0f0)
       row)
      (t
       (let ((new (physics-contact-columns-length contacts)))
         (physics-contact-columns-push
          contacts key kind handle-a handle-b owner step 0
          0f0 0f0 0f0 0f0 0f0 0f0
          0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0 0f0)
         (setf (gethash key index) new)
         (incf (physics-world-contacts-made world))
         new)))))

(defun prune-physics-contacts (world)
  "Drop every contact whose pair was not within reach this step, ending
those that were touching."
  (let* ((contacts (physics-world-contacts world))
         (index (physics-world-contact-index world))
         (events (physics-world-events world))
         (step (physics-world-step-count world))
         (row (1- (physics-contact-columns-length contacts))))
    (declare (fixnum row))
    (loop while (>= row 0)
          do (let ((last (aref (physics-contact-columns-last-step-lane contacts) row)))
               (when (< last step)
                 (when (plusp (aref (physics-contact-columns-touching-lane contacts) row))
                   (physics-event-columns-push
                    events :end
                    (aref (physics-contact-columns-handle-a-lane contacts) row)
                    (aref (physics-contact-columns-handle-b-lane contacts) row)
                    (aref (physics-contact-columns-owner-lane contacts) row)
                    0f0 0f0 0f0 0f0))
                 (remhash (aref (physics-contact-columns-key-lane contacts) row) index)
                 (incf (physics-world-contacts-dropped world))
                 (let ((moved (physics-contact-columns-remove-swap contacts row)))
                   (when moved
                     (setf (gethash (aref (physics-contact-columns-key-lane contacts) row)
                                    index)
                           row)))))
             (decf row))
    world))

;;; ---------------------------------------------------------------------
;;; Contact generation.  Each generator writes one manifold into a contact
;;; row: normal from A toward the other side, the point on the other side,
;;; the other side's velocity, and the separation.

(declaim (inline %record-physics-manifold))
(defun %record-physics-manifold (world row nx ny nz px py pz kvx kvy kvz separation)
  (declare (fixnum row) (single-float nx ny nz px py pz kvx kvy kvz separation)
           (optimize (speed 3) (safety 0)))
  (let ((contacts (physics-world-contacts world)))
    (setf (aref (%lane physics-contact-columns contacts nx) row) nx
          (aref (%lane physics-contact-columns contacts ny) row) ny
          (aref (%lane physics-contact-columns contacts nz) row) nz
          (aref (%lane physics-contact-columns contacts px) row) px
          (aref (%lane physics-contact-columns contacts py) row) py
          (aref (%lane physics-contact-columns contacts pz) row) pz
          (aref (%lane physics-contact-columns contacts kvx) row) kvx
          (aref (%lane physics-contact-columns contacts kvy) row) kvy
          (aref (%lane physics-contact-columns contacts kvz) row) kvz
          (aref (%lane physics-contact-columns contacts separation) row) separation)
    row))

(defun generate-physics-body-pairs (world)
  "Find every awake body's neighbours in the grid and give each near pair a
contact.  A moving awake body wakes a sleeping neighbour first; a slow one
leans on it as on terrain.  Woken bodies join the awake set at its end and
are visited by this same pass."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((awake (physics-world-awake world))
         (sleeping (physics-world-sleeping world))
         (grid (physics-world-grid world))
         (ids (physics-world-ids world))
         (reach (coerce *physics-speculative-distance* 'single-float))
         (wake-speed (coerce *physics-wake-speed* 'single-float))
         (i 0))
    (declare (fixnum i) (single-float reach wake-speed))
    (macrolet ((awake-lane (name) `(%lane physics-body-columns awake ,name))
               (other-lane (name)
                 ;; The other body's set, chosen per neighbour.
                 `(if awake-b-p (%lane physics-body-columns awake ,name)
                      (%lane physics-body-columns sleeping ,name))))
      ;; A wake appends to the awake set and may reallocate its lanes, so
      ;; every lane is fetched afresh per body rather than bound once.
      (loop while (< i (physics-body-columns-length awake))
            do (when (logtest (aref (awake-lane flags) i) +physics-body-collides-with-bodies+)
                 (let* ((handle-a (aref (awake-lane handle) i))
                        (xa (aref (awake-lane x) i)) (ya (aref (awake-lane y) i))
                        (za (aref (awake-lane z) i))
                        (ra (aref (awake-lane radius) i))
                        (vx (aref (awake-lane vx) i))
                        (vy (aref (awake-lane vy) i))
                        (vz (aref (awake-lane vz) i))
                        (moving-p (> (+ (* vx vx) (* vy vy) (* vz vz))
                                     (* wake-speed wake-speed)))
                        (index-a (physics-handle-index handle-a)))
                   (declare (single-float xa ya za ra vx vy vz) (fixnum handle-a index-a))
                   (do-physics-grid-neighbours (handle-b grid xa ya za)
                     (unless (= handle-b handle-a)
                       (let* ((index-b (physics-handle-index handle-b))
                              (set-b (aref (physics-id-table-set ids) index-b))
                              (local-b (aref (physics-id-table-local ids) index-b)))
                         (declare (fixnum index-b set-b local-b))
                         (when (and (= set-b +physics-set-sleeping+) moving-p)
                           ;; Near enough to wake?  Check before relocating.
                           (let* ((dx (- (aref (%lane physics-body-columns sleeping x) local-b) xa))
                                  (dy (- (aref (%lane physics-body-columns sleeping y) local-b) ya))
                                  (dz (- (aref (%lane physics-body-columns sleeping z) local-b) za))
                                  (limit (+ ra (aref (%lane physics-body-columns sleeping radius) local-b)
                                            reach)))
                             (declare (single-float dx dy dz limit))
                             (when (< (+ (* dx dx) (* dy dy) (* dz dz)) (* limit limit))
                               (wake-physics-body world handle-b)
                               ;; Waking only appends to the awake set, so our
                               ;; own row I is where it was; B's is not.
                               (setf set-b (aref (physics-id-table-set ids) index-b)
                                     local-b (aref (physics-id-table-local ids) index-b)))))
                         (let ((awake-b-p (= set-b +physics-set-awake+)))
                           (when (and (logtest (aref (other-lane flags) local-b)
                                               +physics-body-collides-with-bodies+)
                                      ;; Each awake pair once: from the lower row.
                                      (or (not awake-b-p) (< i local-b)))
                             (let* ((xb (aref (other-lane x) local-b))
                                    (yb (aref (other-lane y) local-b))
                                    (zb (aref (other-lane z) local-b))
                                    (rb (aref (other-lane radius) local-b))
                                    (dx (- xb xa)) (dy (- yb ya)) (dz (- zb za))
                                    (limit (+ ra rb reach))
                                    (distance-squared (+ (* dx dx) (* dy dy) (* dz dz))))
                               (declare (single-float xb yb zb rb dx dy dz limit distance-squared))
                               (when (< distance-squared (* limit limit))
                                 (let* ((distance (sqrt (the (single-float 0f0) distance-squared)))
                                        (nx (if (> distance 1e-6) (/ dx distance) 0f0))
                                        (ny (if (> distance 1e-6) (/ dy distance) 1f0))
                                        (nz (if (> distance 1e-6) (/ dz distance) 0f0))
                                        (low (min index-a index-b))
                                        (high (max index-a index-b))
                                        (key (physics-contact-key +physics-contact-body+ low high))
                                        ;; A is the awake body of the pair, and
                                        ;; when both are awake, the lower row.
                                        (row (ensure-physics-contact
                                              world key +physics-contact-body+
                                              handle-a handle-b nil)))
                                   (declare (single-float distance nx ny nz) (fixnum row))
                                   (%record-physics-manifold
                                    world row nx ny nz
                                    (- xb (* rb nx)) (- yb (* rb ny)) (- zb (* rb nz))
                                    0f0 0f0 0f0
                                    (- distance ra rb))))))))))))
               (incf i)))
    world))

(defmacro %with-sphere-box-contact
    ((nx ny nz px py pz separation)
     (cx cy cz radius min-x min-y min-z max-x max-y max-z reach
      &key (accept-p t))
     &body body)
  "Run BODY when the sphere at CX,CY,CZ is within REACH of the box, binding
its contact normal, point, and separation.  ACCEPT-P is evaluated with
OUTSIDE-X OUTSIDE-Y OUTSIDE-Z bound to -1, 0, or 1 for the sphere centre's
position against each axis of the box, and may refuse the contact."
  (let ((qx (gensym)) (qy (gensym)) (qz (gensym))
        (dx (gensym)) (dy (gensym)) (dz (gensym))
        (distance-squared (gensym)) (distance (gensym)))
    `(let* ((,qx (max ,min-x (min ,max-x ,cx)))
            (,qy (max ,min-y (min ,max-y ,cy)))
            (,qz (max ,min-z (min ,max-z ,cz)))
            (,dx (- ,cx ,qx)) (,dy (- ,cy ,qy)) (,dz (- ,cz ,qz))
            (,distance-squared (+ (* ,dx ,dx) (* ,dy ,dy) (* ,dz ,dz))))
       (declare (single-float ,qx ,qy ,qz ,dx ,dy ,dz ,distance-squared))
       (let ((outside-x (cond ((< ,cx ,min-x) -1) ((> ,cx ,max-x) 1) (t 0)))
             (outside-y (cond ((< ,cy ,min-y) -1) ((> ,cy ,max-y) 1) (t 0)))
             (outside-z (cond ((< ,cz ,min-z) -1) ((> ,cz ,max-z) 1) (t 0))))
         (declare (fixnum outside-x outside-y outside-z)
                  (ignorable outside-x outside-y outside-z))
         (cond
           ((plusp ,distance-squared)
            ;; Outside the box: the closest point is on a face, edge, or corner.
            (when (and (< ,distance-squared (* ,reach ,reach)) ,accept-p)
              ;; The normal points from the sphere toward the box, as every
              ;; contact normal points from A toward its other side.
              (let* ((,distance (sqrt (the (single-float 0f0) ,distance-squared)))
                     (,nx (- (/ ,dx ,distance))) (,ny (- (/ ,dy ,distance)))
                     (,nz (- (/ ,dz ,distance)))
                     (,px ,qx) (,py ,qy) (,pz ,qz)
                     (,separation (- ,distance ,radius)))
                (declare (single-float ,distance ,nx ,ny ,nz ,px ,py ,pz ,separation))
                ,@body)))
           (t
            ;; The centre is inside the box: push out through the nearest
            ;; face.  ACCEPT-P then sees the face's direction.
            (let* ((face-x (if (< (- ,cx ,min-x) (- ,max-x ,cx)) -1 1))
                   (face-y (if (< (- ,cy ,min-y) (- ,max-y ,cy)) -1 1))
                   (face-z (if (< (- ,cz ,min-z) (- ,max-z ,cz)) -1 1))
                   (depth-x (if (minusp face-x) (- ,cx ,min-x) (- ,max-x ,cx)))
                   (depth-y (if (minusp face-y) (- ,cy ,min-y) (- ,max-y ,cy)))
                   (depth-z (if (minusp face-z) (- ,cz ,min-z) (- ,max-z ,cz))))
              (declare (fixnum face-x face-y face-z)
                       (single-float depth-x depth-y depth-z))
              (multiple-value-bind (outside-x outside-y outside-z ,px ,py ,pz ,separation)
                  (cond ((and (<= depth-y depth-x) (<= depth-y depth-z))
                         (values 0 face-y 0
                                 ,cx (if (minusp face-y) ,min-y ,max-y) ,cz
                                 (- (- depth-y) ,radius)))
                        ((<= depth-x depth-z)
                         (values face-x 0 0
                                 (if (minusp face-x) ,min-x ,max-x) ,cy ,cz
                                 (- (- depth-x) ,radius)))
                        (t
                         (values 0 0 face-z
                                 ,cx ,cy (if (minusp face-z) ,min-z ,max-z)
                                 (- (- depth-z) ,radius))))
                (declare (fixnum outside-x outside-y outside-z)
                         (single-float ,px ,py ,pz ,separation)
                         (ignorable outside-x outside-y outside-z))
                ;; Into the box: against the face's outward direction.
                (let ((,nx (- (float outside-x 0f0)))
                      (,ny (- (float outside-y 0f0)))
                      (,nz (- (float outside-z 0f0))))
                  (declare (single-float ,nx ,ny ,nz))
                  (when ,accept-p
                    ,@body))))))))))

(defun generate-physics-terrain-contacts (world)
  "Give every awake body a contact with each exposed terrain face it is near.

The voxel grid is the static tree (#D9W4CH).  A cell's face, edge, or corner
is only offered when no solid cell lies in the direction the sphere's centre
overshoots it, which is what keeps a ball rolling across a floor of cells
from catching on the seams between them: the neighbouring cell's own face
is always the nearer, truer contact, and it is the only one made."
  (let* ((awake (physics-world-awake world))
         (probe (physics-world-probe world))
         (margin (coerce *physics-terrain-margin* 'single-float)))
    (declare (single-float margin))
    (unless (physics-world-terrain world)
      (return-from generate-physics-terrain-contacts world))
    (records:with-columnar-buffer-storage
        ((length row (xs x) (ys y) (zs z) (radii radius) (handles handle))
         awake physics-body-columns)
      (declare (ignore row))
      (dotimes (i length)
        (let* ((cx (aref xs i)) (cy (aref ys i)) (cz (aref zs i))
               (radius (aref radii i))
               (handle (aref handles i))
               (index-a (physics-handle-index handle))
               ;; The neighbour rule needs the centre within one cell of the
               ;; cells it looks at.
               (reach (min 0.98f0 (+ radius margin)))
               (min-x (floor (- cx reach))) (max-x (floor (+ cx reach)))
               (min-y (floor (- cy reach))) (max-y (floor (+ cy reach)))
               (min-z (floor (- cz reach))) (max-z (floor (+ cz reach))))
          (declare (single-float cx cy cz radius reach)
                   (fixnum min-x max-x min-y max-y min-z max-z index-a))
          (loop for x fixnum from min-x to max-x do
            (loop for y fixnum from min-y to max-y do
              (loop for z fixnum from min-z to max-z do
                (when (terrain-probe-solid-p probe x y z)
                  (let ((box-min-x (float x 0f0)) (box-min-y (float y 0f0))
                        (box-min-z (float z 0f0)))
                    (declare (single-float box-min-x box-min-y box-min-z))
                    (%with-sphere-box-contact
                        (nx ny nz px py pz separation)
                        (cx cy cz radius
                         box-min-x box-min-y box-min-z
                         (+ box-min-x 1f0) (+ box-min-y 1f0) (+ box-min-z 1f0)
                         reach
                         :accept-p
                         ;; No solid neighbour in any overshoot direction.
                         (not (or (and (/= outside-x 0)
                                       (terrain-probe-solid-p probe (+ x outside-x) y z))
                                  (and (/= outside-y 0)
                                       (terrain-probe-solid-p probe x (+ y outside-y) z))
                                  (and (/= outside-z 0)
                                       (terrain-probe-solid-p probe x y (+ z outside-z)))
                                  (and (/= outside-x 0) (/= outside-y 0)
                                       (terrain-probe-solid-p probe (+ x outside-x) (+ y outside-y) z))
                                  (and (/= outside-x 0) (/= outside-z 0)
                                       (terrain-probe-solid-p probe (+ x outside-x) y (+ z outside-z)))
                                  (and (/= outside-y 0) (/= outside-z 0)
                                       (terrain-probe-solid-p probe x (+ y outside-y) (+ z outside-z)))
                                  (and (/= outside-x 0) (/= outside-y 0) (/= outside-z 0)
                                       (terrain-probe-solid-p probe (+ x outside-x) (+ y outside-y)
                                                              (+ z outside-z))))))
                      (let ((row (ensure-physics-contact
                                  world
                                  (physics-contact-key +physics-contact-terrain+ index-a
                                                       (physics-cell-key x y z))
                                  +physics-contact-terrain+ handle +physics-no-body+ nil)))
                        (%record-physics-manifold world row nx ny nz px py pz
                                                  0f0 0f0 0f0 separation)))))))))))
    world))

(defun generate-physics-box-contacts (world)
  "Give every awake body a contact with each kinematic box it is near."
  (let* ((awake (physics-world-awake world))
         (boxes (physics-world-boxes world))
         (margin (coerce *physics-terrain-margin* 'single-float)))
    (declare (single-float margin))
    (records:with-columnar-buffer-storage
        ((box-count box-row (box-min-xs min-x) (box-min-ys min-y) (box-min-zs min-z)
                            (box-max-xs max-x) (box-max-ys max-y) (box-max-zs max-z)
                            (box-vxs vx) (box-vys vy) (box-vzs vz))
         boxes physics-box-columns)
      (declare (ignore box-row))
      (when (zerop box-count)
        (return-from generate-physics-box-contacts world))
      (records:with-columnar-buffer-storage
          ((length row (xs x) (ys y) (zs z) (radii radius) (handles handle))
           awake physics-body-columns)
        (declare (ignore row))
        (dotimes (i length)
          (let* ((cx (aref xs i)) (cy (aref ys i)) (cz (aref zs i))
                 (radius (aref radii i))
                 (handle (aref handles i))
                 (index-a (physics-handle-index handle))
                 (reach (+ radius margin)))
            (declare (single-float cx cy cz radius reach) (fixnum index-a))
            (dotimes (b box-count)
              (let ((min-x (aref box-min-xs b)) (min-y (aref box-min-ys b))
                    (min-z (aref box-min-zs b))
                    (max-x (aref box-max-xs b)) (max-y (aref box-max-ys b))
                    (max-z (aref box-max-zs b)))
                (when (and (< (- cx reach) max-x) (> (+ cx reach) min-x)
                           (< (- cy reach) max-y) (> (+ cy reach) min-y)
                           (< (- cz reach) max-z) (> (+ cz reach) min-z))
                  (%with-sphere-box-contact
                      (nx ny nz px py pz separation)
                      (cx cy cz radius min-x min-y min-z max-x max-y max-z reach)
                    (let ((row (ensure-physics-contact
                                world
                                (physics-contact-key +physics-contact-box+ index-a b)
                                +physics-contact-box+ handle +physics-no-body+
                                (aref (physics-box-columns-owner-lane boxes) b))))
                      (%record-physics-manifold
                       world row nx ny nz px py pz
                       (aref box-vxs b) (aref box-vys b) (aref box-vzs b)
                       separation))))))))))
    world))

;;; ---------------------------------------------------------------------
;;; Softness (#R7F2QH): three numbers from a frequency, a damping ratio,
;;; and the substep.

(declaim (inline physics-softness))
(defun physics-softness (hertz zeta h)
  "Return BIAS-RATE, MASS-SCALE, and IMPULSE-SCALE for a soft constraint."
  (declare (single-float hertz zeta h))
  (if (zerop hertz)
      (values 0f0 1f0 0f0)
      (let* ((omega (* 2f0 (float pi 0f0) hertz))
             (a1 (+ (* 2f0 zeta) (* h omega)))
             (a2 (* h omega a1))
             (a3 (/ (+ 1f0 a2))))
        (values (/ omega a1) (* a2 a3) a3))))

;;; ---------------------------------------------------------------------
;;; Colouring and preparation.  Every live contact is given a colour such
;;; that no two contacts in one colour share an awake body, then the
;;; constraint buffer is filled in colour order with all the solver needs.

(defun ensure-physics-color-scratch (world awake-count contact-count)
  (let ((bits (physics-world-color-bits world))
        (colors (physics-world-contact-colors world)))
    (dotimes (color (length bits))
      (let ((vector (aref bits color)))
        (if (and vector (>= (length vector) awake-count))
            (fill vector 0 :end awake-count)
            (setf (aref bits color)
                  (make-array (max 64 (* 2 awake-count))
                              :element-type 'bit :initial-element 0)))))
    (when (< (length colors) contact-count)
      (setf (physics-world-contact-colors world)
            (make-array (max 64 (* 2 contact-count)) :element-type 'fixnum
                                                     :initial-element 0)))
    world))

(defun %ensure-physics-constraint-capacity (buffer count)
  (when (< (physics-constraint-columns-capacity buffer) count)
    (%physics-constraint-columns-grow buffer count))
  (setf (physics-constraint-columns-length buffer) count)
  buffer)

(defun prepare-physics-constraints (world h)
  "Colour the live contacts and fill the constraint buffer in colour order.
Return the number of colours in use, the overflow colour counted."
  (declare (single-float h) (optimize (speed 3) (safety 1)))
  (let* ((awake (physics-world-awake world))
         (contacts (physics-world-contacts world))
         (constraints (physics-world-constraints world))
         (ids (physics-world-ids world))
         (awake-count (physics-body-columns-length awake))
         (contact-count (physics-contact-columns-length contacts))
         (max-colors *physics-max-colors*)
         (dynamic-colors (- max-colors 4))
         (overflow max-colors)
         (starts (physics-world-color-starts world)))
    (declare (fixnum awake-count contact-count max-colors dynamic-colors overflow))
    (ensure-physics-color-scratch world awake-count contact-count)
    (let ((bits (physics-world-color-bits world))
          (colors (physics-world-contact-colors world))
          (handle-as (physics-contact-columns-handle-a-lane contacts))
          (handle-bs (physics-contact-columns-handle-b-lane contacts))
          (kinds (physics-contact-columns-kind-lane contacts)))
      (declare (type (simple-array fixnum (*)) colors handle-as handle-bs kinds))
      (fill starts 0)
      ;; Pass one: colour.
      (dotimes (row contact-count)
        (let* ((handle-a (aref handle-as row))
               (local-a (aref (physics-id-table-local ids) (physics-handle-index handle-a)))
               (dynamic-b-p
                 (and (= (aref kinds row) +physics-contact-body+)
                      (= (aref (physics-id-table-set ids)
                               (physics-handle-index (aref handle-bs row)))
                         +physics-set-awake+)))
               (local-b (if dynamic-b-p
                            (aref (physics-id-table-local ids)
                                  (physics-handle-index (aref handle-bs row)))
                            -1))
               (color overflow))
          (declare (fixnum local-a local-b color))
          (if dynamic-b-p
              (loop for c fixnum from 0 below dynamic-colors
                    do (let ((vector (aref bits c)))
                         (declare (simple-bit-vector vector))
                         (when (and (zerop (sbit vector local-a))
                                    (zerop (sbit vector local-b)))
                           (setf (sbit vector local-a) 1
                                 (sbit vector local-b) 1
                                 color c)
                           (return))))
              ;; Static contacts colour from the top down: solved last.
              (loop for c fixnum from (1- max-colors) downto 0
                    do (let ((vector (aref bits c)))
                         (declare (simple-bit-vector vector))
                         (when (zerop (sbit vector local-a))
                           (setf (sbit vector local-a) 1
                                 color c)
                           (return)))))
          (setf (aref colors row) color)
          (incf (aref starts (1+ color)))))
      ;; Prefix sums: STARTS[c] is where colour C begins.
      (loop for c from 1 to (1+ overflow)
            do (incf (aref starts c) (aref starts (1- c))))
      (%ensure-physics-constraint-capacity constraints contact-count)
      ;; Pass two: fill, with a moving cursor per colour.
      (let ((cursor (make-array (+ 2 max-colors) :element-type 'fixnum)))
        (declare (dynamic-extent cursor))
        (replace cursor starts)
        (multiple-value-bind (dynamic-bias dynamic-mass-scale dynamic-impulse-scale)
            (physics-softness (min (coerce *physics-contact-hertz* 'single-float)
                                   (* 0.125f0 (/ h)))
                              (coerce *physics-contact-damping-ratio* 'single-float) h)
          (multiple-value-bind (static-bias static-mass-scale static-impulse-scale)
              (physics-softness (min (* 2f0 (coerce *physics-contact-hertz* 'single-float))
                                     (* 0.125f0 (/ h)))
                                (* 0.5f0 (coerce *physics-contact-damping-ratio* 'single-float))
                                h)
            (let ((slop (coerce *physics-linear-slop* 'single-float))
                  (dummy awake-count))
              (declare (single-float slop) (fixnum dummy))
              (dotimes (row contact-count)
                (let* ((color (aref colors row))
                       (slot (aref cursor color))
                       (handle-a (aref handle-as row))
                       (local-a (aref (physics-id-table-local ids)
                                      (physics-handle-index handle-a)))
                       (dynamic-b-p
                         (and (= (aref kinds row) +physics-contact-body+)
                              (= (aref (physics-id-table-set ids)
                                       (physics-handle-index (aref handle-bs row)))
                                 +physics-set-awake+)))
                       (local-b (if dynamic-b-p
                                    (aref (physics-id-table-local ids)
                                          (physics-handle-index (aref handle-bs row)))
                                    dummy))
                       (nx (aref (%lane physics-contact-columns contacts nx) row))
                       (ny (aref (%lane physics-contact-columns contacts ny) row))
                       (nz (aref (%lane physics-contact-columns contacts nz) row))
                       (ra (aref (%lane physics-body-columns awake radius) local-a))
                       (rb (if dynamic-b-p (aref (%lane physics-body-columns awake radius) local-b) 0f0))
                       (ima (aref (%lane physics-body-columns awake inverse-mass) local-a))
                       (imb (aref (%lane physics-body-columns awake inverse-mass) local-b))
                       (iia (aref (%lane physics-body-columns awake inverse-inertia) local-a))
                       (iib (aref (%lane physics-body-columns awake inverse-inertia) local-b))
                       ;; Anchors: from each centre to the contact point.  A's
                       ;; runs along the normal; B's against it.
                       (rax (* ra nx)) (ray (* ra ny)) (raz (* ra nz))
                       (rbx (- (* rb nx))) (rby (- (* rb ny))) (rbz (- (* rb nz)))
                       (normal-mass (let ((k (+ ima imb))) (if (plusp k) (/ k) 0f0)))
                       (tangent-mass (let ((k (+ ima imb (* iia ra ra) (* iib rb rb))))
                                       (if (plusp k) (/ k) 0f0)))
                       (rolling-mass (let ((k (+ iia iib))) (if (plusp k) (/ k) 0f0)))
                       ;; A tangent frame from the normal.
                       (t1x 0f0) (t1y 0f0) (t1z 0f0))
                  (declare (fixnum color slot local-a local-b)
                           (single-float nx ny nz ra rb ima imb iia iib
                                         rax ray raz rbx rby rbz
                                         normal-mass tangent-mass rolling-mass
                                         t1x t1y t1z))
                  (if (> (abs ny) 0.9f0)
                      ;; The normal is near vertical: cross with X.
                      (let* ((cy nz) (cz (- ny))
                             (l (sqrt (the (single-float 0f0) (+ (* cy cy) (* cz cz))))))
                        (setf t1x 0f0 t1y (/ cy l) t1z (/ cz l)))
                      ;; Cross with Y.
                      (let* ((cx (- nz)) (cz nx)
                             (l (sqrt (the (single-float 0f0) (+ (* cx cx) (* cz cz))))))
                        (setf t1x (/ cx l) t1y 0f0 t1z (/ cz l))))
                  (let* ((t2x (- (* ny t1z) (* nz t1y)))
                         (t2y (- (* nz t1x) (* nx t1z)))
                         (t2z (- (* nx t1y) (* ny t1x)))
                         (kvx (aref (%lane physics-contact-columns contacts kvx) row))
                         (kvy (aref (%lane physics-contact-columns contacts kvy) row))
                         (kvz (aref (%lane physics-contact-columns contacts kvz) row))
                         ;; Approach speed now, for restitution later.
                         (vax (aref (%lane physics-body-columns awake vx) local-a))
                         (vay (aref (%lane physics-body-columns awake vy) local-a))
                         (vaz (aref (%lane physics-body-columns awake vz) local-a))
                         (vbx (+ (aref (%lane physics-body-columns awake vx) local-b) kvx))
                         (vby (+ (aref (%lane physics-body-columns awake vy) local-b) kvy))
                         (vbz (+ (aref (%lane physics-body-columns awake vz) local-b) kvz))
                         (relative-velocity
                           (+ (* (- vbx vax) nx) (* (- vby vay) ny) (* (- vbz vaz) nz)))
                         (restitution
                           (if dynamic-b-p
                               (max (aref (%lane physics-body-columns awake restitution) local-a)
                                    (aref (%lane physics-body-columns awake restitution) local-b))
                               (aref (%lane physics-body-columns awake restitution) local-a)))
                         (friction
                           (if dynamic-b-p
                               (sqrt (the (single-float 0f0)
                                          (* (aref (%lane physics-body-columns awake friction) local-a)
                                             (aref (%lane physics-body-columns awake friction) local-b))))
                               (aref (%lane physics-body-columns awake friction) local-a)))
                         (rolling
                           (if dynamic-b-p
                               (max (aref (%lane physics-body-columns awake rolling-resistance) local-a)
                                    (aref (%lane physics-body-columns awake rolling-resistance) local-b))
                               (aref (%lane physics-body-columns awake rolling-resistance) local-a))))
                    (declare (single-float t2x t2y t2z kvx kvy kvz vax vay vaz vbx vby vbz
                                           relative-velocity restitution friction rolling))
                    (macrolet ((put (lane value)
                                 `(setf (aref (%lane physics-constraint-columns constraints ,lane)
                                              slot)
                                        ,value)))
                      (put contact row)
                      (put body-a local-a)
                      (put body-b local-b)
                      (put nx nx) (put ny ny) (put nz nz)
                      (put t1x t1x) (put t1y t1y) (put t1z t1z)
                      (put t2x t2x) (put t2y t2y) (put t2z t2z)
                      (put rax rax) (put ray ray) (put raz raz)
                      (put rbx rbx) (put rby rby) (put rbz rbz)
                      (put kvx kvx) (put kvy kvy) (put kvz kvz)
                      (put separation
                           (+ (aref (%lane physics-contact-columns contacts separation) row) slop))
                      (put normal-mass normal-mass)
                      (put tangent-mass tangent-mass)
                      (put rolling-mass rolling-mass)
                      (put restitution restitution)
                      (put friction friction)
                      (put rolling-resistance rolling)
                      (put relative-velocity relative-velocity)
                      (put total-normal-impulse 0f0)
                      (if dynamic-b-p
                          (progn (put bias-rate (* dynamic-mass-scale dynamic-bias))
                                 (put mass-scale dynamic-mass-scale)
                                 (put impulse-scale dynamic-impulse-scale))
                          (progn (put bias-rate (* static-mass-scale static-bias))
                                 (put mass-scale static-mass-scale)
                                 (put impulse-scale static-impulse-scale)))
                      ;; Warm start from what the pair carried.
                      (put normal-impulse
                           (aref (%lane physics-contact-columns contacts normal-impulse) row))
                      (put tangent-impulse-1
                           (aref (%lane physics-contact-columns contacts tangent-impulse-1) row))
                      (put tangent-impulse-2
                           (aref (%lane physics-contact-columns contacts tangent-impulse-2) row))
                      (put rolling-impulse-x
                           (aref (%lane physics-contact-columns contacts rolling-impulse-x) row))
                      (put rolling-impulse-y
                           (aref (%lane physics-contact-columns contacts rolling-impulse-y) row))
                      (put rolling-impulse-z
                           (aref (%lane physics-contact-columns contacts rolling-impulse-z) row))))
                  (setf (aref cursor color) (1+ slot)))))))))
    ;; How many colours actually got contacts.
    (loop for c from 0 to overflow
          count (< (aref starts c) (aref starts (1+ c))))))

;;; ---------------------------------------------------------------------
;;; Kernels.  Each phase of a substep is a generic function on the world's
;;; kernel family; the methods here are the scalar reference kernels, one
;;; contact at a time over the borrowed columns.  PHYSICS-SIMD.LISP adds
;;; the four-wide families, which run the same arithmetic in the same order
;;; on four contacts of one colour at once and hand the tail back here.
;;; A kernel is given a half-open range of constraint rows and must touch
;;; no other; that is what the colouring guarantees is safe.

(defvar *fastest-physics-kernels* :scalar
  "The kernel family MAKE-PHYSICS-WORLD picks by default; PHYSICS-SIMD.LISP
sets it to a native family when one is available.")

(defun fastest-physics-kernels ()
  *fastest-physics-kernels*)

(defgeneric physics-integrate-velocities (kernels awake h)
  (:documentation "Apply gravity and damping to the awake bodies over H."))
(defgeneric physics-integrate-positions (kernels awake h)
  (:documentation "Advance the awake bodies' deltas and orientations by H."))
(defgeneric physics-warm-start (kernels constraints awake starts)
  (:documentation
   "Apply the carried impulses of every constraint, colour by colour.
STARTS gives each colour's first row, with one more entry than colours."))
(defgeneric physics-solve-contacts
    (kernels constraints awake starts inv-h use-bias-p elapsed push-max)
  (:documentation
   "One Gauss-Seidel iteration over every colour in turn: normal impulses
with soft or speculative bias when USE-BIAS-P, and without bias plus
friction and rolling resistance when not.  ELAPSED is how far into the step
the substep is, for a kinematic other side's motion."))
(defgeneric physics-apply-restitution (kernels constraints awake starts threshold)
  (:documentation "Bounce the constraints that hit hard enough, colour by colour."))

(defmacro do-physics-colors ((start end starts) &body body)
  "Run BODY with START and END bound to each non-empty colour's row range."
  (let ((color (gensym "COLOR")) (array (gensym "STARTS")))
    `(let ((,array ,starts))
       (declare (type (simple-array fixnum (*)) ,array))
       (loop for ,color fixnum from 0 below (1- (length ,array))
             do (let ((,start (aref ,array ,color))
                      (,end (aref ,array (1+ ,color))))
                  (declare (fixnum ,start ,end))
                  (when (< ,start ,end)
                    ,@body))))))

;;; The columns every contact kernel borrows, bound by name.

(defmacro with-physics-kernel-columns ((constraints awake) &body body)
  `(records:with-columnar-buffer-storage
       ((constraint-count constraint-row
         (body-as body-a) (body-bs body-b)
         (nxs nx) (nys ny) (nzs nz)
         (t1xs t1x) (t1ys t1y) (t1zs t1z)
         (t2xs t2x) (t2ys t2y) (t2zs t2z)
         (raxs rax) (rays ray) (razs raz)
         (rbxs rbx) (rbys rby) (rbzs rbz)
         (kvxs kvx) (kvys kvy) (kvzs kvz)
         (separations separation)
         (normal-masses normal-mass) (tangent-masses tangent-mass)
         (rolling-masses rolling-mass)
         (restitutions restitution) (frictions friction)
         (rolling-resistances rolling-resistance)
         (relative-velocities relative-velocity)
         (total-normal-impulses total-normal-impulse)
         (bias-rates bias-rate) (mass-scales mass-scale) (impulse-scales impulse-scale)
         (normal-impulses normal-impulse)
         (tangent-impulses-1 tangent-impulse-1) (tangent-impulses-2 tangent-impulse-2)
         (rolling-impulses-x rolling-impulse-x) (rolling-impulses-y rolling-impulse-y)
         (rolling-impulses-z rolling-impulse-z))
        ,constraints physics-constraint-columns)
     (declare (ignorable constraint-count constraint-row
                         body-as body-bs nxs nys nzs t1xs t1ys t1zs t2xs t2ys t2zs
                         raxs rays razs rbxs rbys rbzs kvxs kvys kvzs separations
                         normal-masses tangent-masses rolling-masses restitutions
                         frictions rolling-resistances relative-velocities
                         total-normal-impulses bias-rates mass-scales impulse-scales
                         normal-impulses tangent-impulses-1 tangent-impulses-2
                         rolling-impulses-x rolling-impulses-y rolling-impulses-z))
     (records:with-columnar-buffer-storage
         ((awake-count awake-row
           (vxs vx) (vys vy) (vzs vz)
           (wxs wx) (wys wy) (wzs wz)
           (dxs dx) (dys dy) (dzs dz)
           (inverse-masses inverse-mass) (inverse-inertias inverse-inertia))
          ,awake physics-body-columns)
       (declare (ignorable awake-count awake-row vxs vys vzs wxs wys wzs dxs dys dzs
                           inverse-masses inverse-inertias))
       ,@body)))

(defmethod physics-integrate-velocities ((kernels (eql :scalar)) awake h)
  (declare (single-float h))
  (records:with-columnar-buffer-storage
      ((count row (vxs vx) (vys vy) (vzs vz) (wxs wx) (wys wy) (wzs wz)
        (dampings damping) (inverse-masses inverse-mass))
       awake physics-body-columns)
    (declare (ignore row) (optimize (speed 3) (safety 0)))
    (let ((gravity (* h (coerce *physics-gravity* 'single-float))))
      (declare (single-float gravity))
      (dotimes (i count)
        (let ((scale (/ (+ 1f0 (* h (aref dampings i))))))
          (declare (single-float scale))
          (setf (aref vxs i) (* scale (aref vxs i))
                (aref vys i) (+ (* scale (aref vys i))
                                (if (plusp (aref inverse-masses i)) gravity 0f0))
                (aref vzs i) (* scale (aref vzs i))
                (aref wxs i) (* scale (aref wxs i))
                (aref wys i) (* scale (aref wys i))
                (aref wzs i) (* scale (aref wzs i))))))
    awake))

(defmethod physics-integrate-positions ((kernels (eql :scalar)) awake h)
  (declare (single-float h))
  (records:with-columnar-buffer-storage
      ((count row (vxs vx) (vys vy) (vzs vz) (dxs dx) (dys dy) (dzs dz))
       awake physics-body-columns)
    (declare (ignore row) (optimize (speed 3) (safety 0)))
    (dotimes (i count)
      (setf (aref dxs i) (+ (aref dxs i) (* h (aref vxs i)))
            (aref dys i) (+ (aref dys i) (* h (aref vys i)))
            (aref dzs i) (+ (aref dzs i) (* h (aref vzs i))))))
  (%physics-integrate-orientations awake h)
  awake)

;;; The orientation integration the wide position kernel shares with the
;;; scalar one: per body, a quaternion step and a normalization.

(defun %physics-integrate-orientations (awake h)
  (declare (single-float h))
  (records:with-columnar-buffer-storage
      ((count row (wxs wx) (wys wy) (wzs wz) (qxs qx) (qys qy) (qzs qz) (qws qw))
       awake physics-body-columns)
    (declare (ignore row) (optimize (speed 3) (safety 0)))
    (let ((half (* 0.5f0 h)))
      (declare (single-float half))
      (dotimes (i count)
        (let* ((wx (aref wxs i)) (wy (aref wys i)) (wz (aref wzs i))
               (qx (aref qxs i)) (qy (aref qys i)) (qz (aref qzs i)) (qw (aref qws i))
               (nx (+ qx (* half (+ (* wx qw) (* wy qz) (- (* wz qy))))))
               (ny (+ qy (* half (+ (* wy qw) (* wz qx) (- (* wx qz))))))
               (nz (+ qz (* half (+ (* wz qw) (* wx qy) (- (* wy qx))))))
               (nw (+ qw (* half (- (+ (* wx qx) (* wy qy) (* wz qz))))))
               (length (sqrt (the (single-float 0f0) (+ (* nx nx) (* ny ny) (* nz nz) (* nw nw)))))
               (inverse (if (> length 1e-12) (/ length) 1f0)))
          (declare (single-float wx wy wz qx qy qz qw nx ny nz nw length inverse))
          (setf (aref qxs i) (* nx inverse)
                (aref qys i) (* ny inverse)
                (aref qzs i) (* nz inverse)
                (aref qws i) (* nw inverse)))))
    awake))

(defun %physics-warm-start-scalar (constraints awake start end)
  (declare (fixnum start end))
  (with-physics-kernel-columns (constraints awake)
    (declare (optimize (speed 3) (safety 0)))
    (loop for c fixnum from start below end
          do (let* ((ia (aref body-as c)) (ib (aref body-bs c))
                    (nx (aref nxs c)) (ny (aref nys c)) (nz (aref nzs c))
                    (lambda-n (aref normal-impulses c))
                    (f1 (aref tangent-impulses-1 c))
                    (f2 (aref tangent-impulses-2 c))
                    (px (+ (* lambda-n nx) (* f1 (aref t1xs c)) (* f2 (aref t2xs c))))
                    (py (+ (* lambda-n ny) (* f1 (aref t1ys c)) (* f2 (aref t2ys c))))
                    (pz (+ (* lambda-n nz) (* f1 (aref t1zs c)) (* f2 (aref t2zs c))))
                    (rax (aref raxs c)) (ray (aref rays c)) (raz (aref razs c))
                    (rbx (aref rbxs c)) (rby (aref rbys c)) (rbz (aref rbzs c))
                    (ima (aref inverse-masses ia)) (imb (aref inverse-masses ib))
                    (iia (aref inverse-inertias ia)) (iib (aref inverse-inertias ib))
                    (rx (aref rolling-impulses-x c))
                    (ry (aref rolling-impulses-y c))
                    (rz (aref rolling-impulses-z c)))
               (declare (fixnum ia ib)
                        (single-float nx ny nz lambda-n f1 f2 px py pz
                                      rax ray raz rbx rby rbz ima imb iia iib rx ry rz))
               (setf (aref vxs ia) (- (aref vxs ia) (* ima px))
                     (aref vys ia) (- (aref vys ia) (* ima py))
                     (aref vzs ia) (- (aref vzs ia) (* ima pz))
                     (aref vxs ib) (+ (aref vxs ib) (* imb px))
                     (aref vys ib) (+ (aref vys ib) (* imb py))
                     (aref vzs ib) (+ (aref vzs ib) (* imb pz)))
               ;; Angular: rA x P and rB x P, plus the rolling impulse.
               (setf (aref wxs ia) (- (aref wxs ia) (* iia (+ (- (* ray pz) (* raz py)) rx)))
                     (aref wys ia) (- (aref wys ia) (* iia (+ (- (* raz px) (* rax pz)) ry)))
                     (aref wzs ia) (- (aref wzs ia) (* iia (+ (- (* rax py) (* ray px)) rz)))
                     (aref wxs ib) (+ (aref wxs ib) (* iib (+ (- (* rby pz) (* rbz py)) rx)))
                     (aref wys ib) (+ (aref wys ib) (* iib (+ (- (* rbz px) (* rbx pz)) ry)))
                     (aref wzs ib) (+ (aref wzs ib) (* iib (+ (- (* rbx py) (* rby px)) rz))))))
    constraints))

(defmethod physics-warm-start ((kernels (eql :scalar)) constraints awake starts)
  (do-physics-colors (start end starts)
    (%physics-warm-start-scalar constraints awake start end))
  constraints)

(defun %physics-solve-contacts-scalar
    (constraints awake start end inv-h use-bias-p elapsed push-max)
  (declare (fixnum start end) (single-float inv-h elapsed push-max))
  (with-physics-kernel-columns (constraints awake)
    (declare (optimize (speed 3) (safety 0)))
    (loop for c fixnum from start below end
          do (let* ((ia (aref body-as c)) (ib (aref body-bs c))
                    (nx (aref nxs c)) (ny (aref nys c)) (nz (aref nzs c))
                    (rax (aref raxs c)) (ray (aref rays c)) (raz (aref razs c))
                    (rbx (aref rbxs c)) (rby (aref rbys c)) (rbz (aref rbzs c))
                    (kvx (aref kvxs c)) (kvy (aref kvys c)) (kvz (aref kvzs c))
                    (ima (aref inverse-masses ia)) (imb (aref inverse-masses ib))
                    (iia (aref inverse-inertias ia)) (iib (aref inverse-inertias ib))
                    (vax (aref vxs ia)) (vay (aref vys ia)) (vaz (aref vzs ia))
                    (wax (aref wxs ia)) (way (aref wys ia)) (waz (aref wzs ia))
                    (vbx (aref vxs ib)) (vby (aref vys ib)) (vbz (aref vzs ib))
                    (wbx (aref wxs ib)) (wby (aref wys ib)) (wbz (aref wzs ib))
                    ;; Current separation from the accumulated deltas.
                    (dpx (+ (- (aref dxs ib) (aref dxs ia)) (* kvx elapsed)))
                    (dpy (+ (- (aref dys ib) (aref dys ia)) (* kvy elapsed)))
                    (dpz (+ (- (aref dzs ib) (aref dzs ia)) (* kvz elapsed)))
                    (s (+ (aref separations c) (* dpx nx) (* dpy ny) (* dpz nz)))
                    (bias 0f0) (mass-scale 1f0) (impulse-scale 0f0))
               (declare (fixnum ia ib)
                        (single-float nx ny nz rax ray raz rbx rby rbz kvx kvy kvz
                                      ima imb iia iib vax vay vaz wax way waz
                                      vbx vby vbz wbx wby wbz dpx dpy dpz s
                                      bias mass-scale impulse-scale))
               (cond ((> s 0f0)
                      ;; Speculative: may close the gap this substep, no more.
                      (setf bias (* s inv-h)))
                     (use-bias-p
                      (setf bias (max (* (aref bias-rates c) s) (- push-max))
                            mass-scale (aref mass-scales c)
                            impulse-scale (aref impulse-scales c))))
               ;; Relative velocity at the contact point.  A's anchor is
               ;; along the normal, so its spin adds nothing along it, but
               ;; the general form is kept so the wide kernel can mirror it.
               (let* ((vrax (+ vax (- (* way raz) (* waz ray))))
                      (vray (+ vay (- (* waz rax) (* wax raz))))
                      (vraz (+ vaz (- (* wax ray) (* way rax))))
                      (vrbx (+ vbx kvx (- (* wby rbz) (* wbz rby))))
                      (vrby (+ vby kvy (- (* wbz rbx) (* wbx rbz))))
                      (vrbz (+ vbz kvz (- (* wbx rby) (* wby rbx))))
                      (vn (+ (* (- vrbx vrax) nx) (* (- vrby vray) ny) (* (- vrbz vraz) nz)))
                      (old (aref normal-impulses c))
                      (delta (- (- (* (aref normal-masses c) (+ (* mass-scale vn) bias)))
                                (* impulse-scale old)))
                      (new (max (+ old delta) 0f0))
                      (applied (- new old))
                      (px (* applied nx)) (py (* applied ny)) (pz (* applied nz)))
                 (declare (single-float vrax vray vraz vrbx vrby vrbz vn old delta new
                                        applied px py pz))
                 (setf (aref normal-impulses c) new
                       (aref total-normal-impulses c) (+ (aref total-normal-impulses c) new))
                 (setf vax (- vax (* ima px)) vay (- vay (* ima py)) vaz (- vaz (* ima pz))
                       vbx (+ vbx (* imb px)) vby (+ vby (* imb py)) vbz (+ vbz (* imb pz)))
                 (setf wax (- wax (* iia (- (* ray pz) (* raz py))))
                       way (- way (* iia (- (* raz px) (* rax pz))))
                       waz (- waz (* iia (- (* rax py) (* ray px))))
                       wbx (+ wbx (* iib (- (* rby pz) (* rbz py))))
                       wby (+ wby (* iib (- (* rbz px) (* rbx pz))))
                       wbz (+ wbz (* iib (- (* rbx py) (* rby px)))))
                 (unless use-bias-p
                   ;; Rolling resistance: an angular impulse against the
                   ;; relative spin, bounded by the normal impulse.
                   (let ((resistance (aref rolling-resistances c)))
                     (declare (single-float resistance))
                     (when (plusp resistance)
                       (let* ((rolling-mass (aref rolling-masses c))
                              (ox (aref rolling-impulses-x c))
                              (oy (aref rolling-impulses-y c))
                              (oz (aref rolling-impulses-z c))
                              (nx2 (- ox (* rolling-mass (- wbx wax))))
                              (ny2 (- oy (* rolling-mass (- wby way))))
                              (nz2 (- oz (* rolling-mass (- wbz waz))))
                              (limit (* resistance new))
                              (magnitude-squared (+ (* nx2 nx2) (* ny2 ny2) (* nz2 nz2))))
                         (declare (single-float rolling-mass ox oy oz nx2 ny2 nz2 limit
                                                magnitude-squared))
                         (when (> magnitude-squared (+ (* limit limit) 1e-12))
                           (let ((scale (/ limit (sqrt (the (single-float 0f0) magnitude-squared)))))
                             (declare (single-float scale))
                             (setf nx2 (* nx2 scale) ny2 (* ny2 scale) nz2 (* nz2 scale))))
                         (setf (aref rolling-impulses-x c) nx2
                               (aref rolling-impulses-y c) ny2
                               (aref rolling-impulses-z c) nz2)
                         (let ((ax (- nx2 ox)) (ay (- ny2 oy)) (az (- nz2 oz)))
                           (declare (single-float ax ay az))
                           (setf wax (- wax (* iia ax)) way (- way (* iia ay)) waz (- waz (* iia az))
                                 wbx (+ wbx (* iib ax)) wby (+ wby (* iib ay)) wbz (+ wbz (* iib az)))))))
                   ;; Friction: one tangent impulse pair, bounded by the cone.
                   (let* ((t1x (aref t1xs c)) (t1y (aref t1ys c)) (t1z (aref t1zs c))
                          (t2x (aref t2xs c)) (t2y (aref t2ys c)) (t2z (aref t2zs c))
                          (vrax (+ vax (- (* way raz) (* waz ray))))
                          (vray (+ vay (- (* waz rax) (* wax raz))))
                          (vraz (+ vaz (- (* wax ray) (* way rax))))
                          (vrbx (+ vbx kvx (- (* wby rbz) (* wbz rby))))
                          (vrby (+ vby kvy (- (* wbz rbx) (* wbx rbz))))
                          (vrbz (+ vbz kvz (- (* wbx rby) (* wby rbx))))
                          (vrx (- vrbx vrax)) (vry (- vrby vray)) (vrz (- vrbz vraz))
                          (vt1 (+ (* vrx t1x) (* vry t1y) (* vrz t1z)))
                          (vt2 (+ (* vrx t2x) (* vry t2y) (* vrz t2z)))
                          (tangent-mass (aref tangent-masses c))
                          (o1 (aref tangent-impulses-1 c))
                          (o2 (aref tangent-impulses-2 c))
                          (n1 (- o1 (* tangent-mass vt1)))
                          (n2 (- o2 (* tangent-mass vt2)))
                          (limit (* (aref frictions c) new))
                          (length-squared (+ (* n1 n1) (* n2 n2))))
                     (declare (single-float t1x t1y t1z t2x t2y t2z vrax vray vraz
                                            vrbx vrby vrbz vrx vry vrz vt1 vt2
                                            tangent-mass o1 o2 n1 n2 limit length-squared))
                     (when (> length-squared (* limit limit))
                       (let ((scale (/ limit (sqrt (the (single-float 0f0) length-squared)))))
                         (declare (single-float scale))
                         (setf n1 (* n1 scale) n2 (* n2 scale))))
                     (setf (aref tangent-impulses-1 c) n1
                           (aref tangent-impulses-2 c) n2)
                     (let* ((d1 (- n1 o1)) (d2 (- n2 o2))
                            (px (+ (* d1 t1x) (* d2 t2x)))
                            (py (+ (* d1 t1y) (* d2 t2y)))
                            (pz (+ (* d1 t1z) (* d2 t2z))))
                       (declare (single-float d1 d2 px py pz))
                       (setf vax (- vax (* ima px)) vay (- vay (* ima py)) vaz (- vaz (* ima pz))
                             vbx (+ vbx (* imb px)) vby (+ vby (* imb py)) vbz (+ vbz (* imb pz)))
                       (setf wax (- wax (* iia (- (* ray pz) (* raz py))))
                             way (- way (* iia (- (* raz px) (* rax pz))))
                             waz (- waz (* iia (- (* rax py) (* ray px))))
                             wbx (+ wbx (* iib (- (* rby pz) (* rbz py))))
                             wby (+ wby (* iib (- (* rbz px) (* rbx pz))))
                             wbz (+ wbz (* iib (- (* rbx py) (* rby px))))))))
                 (setf (aref vxs ia) vax (aref vys ia) vay (aref vzs ia) vaz
                       (aref wxs ia) wax (aref wys ia) way (aref wzs ia) waz
                       (aref vxs ib) vbx (aref vys ib) vby (aref vzs ib) vbz
                       (aref wxs ib) wbx (aref wys ib) wby (aref wzs ib) wbz))))
    constraints))

(defmethod physics-solve-contacts
    ((kernels (eql :scalar)) constraints awake starts inv-h use-bias-p elapsed push-max)
  (declare (single-float inv-h elapsed push-max))
  (do-physics-colors (start end starts)
    (%physics-solve-contacts-scalar constraints awake start end
                                    inv-h use-bias-p elapsed push-max))
  constraints)

(defun %physics-apply-restitution-scalar (constraints awake start end threshold)
  (declare (fixnum start end) (single-float threshold))
  (with-physics-kernel-columns (constraints awake)
    (declare (optimize (speed 3) (safety 0)))
    (loop for c fixnum from start below end
          do (let ((relative (aref relative-velocities c))
                   (restitution (aref restitutions c)))
               (declare (single-float relative restitution))
               ;; Only a real hit bounces: fast enough, and it pushed.
               (when (and (plusp restitution)
                          (< relative (- threshold))
                          (plusp (aref total-normal-impulses c)))
                 (let* ((ia (aref body-as c)) (ib (aref body-bs c))
                        (nx (aref nxs c)) (ny (aref nys c)) (nz (aref nzs c))
                        (rax (aref raxs c)) (ray (aref rays c)) (raz (aref razs c))
                        (rbx (aref rbxs c)) (rby (aref rbys c)) (rbz (aref rbzs c))
                        (kvx (aref kvxs c)) (kvy (aref kvys c)) (kvz (aref kvzs c))
                        (ima (aref inverse-masses ia)) (imb (aref inverse-masses ib))
                        (iia (aref inverse-inertias ia)) (iib (aref inverse-inertias ib))
                        (vax (aref vxs ia)) (vay (aref vys ia)) (vaz (aref vzs ia))
                        (wax (aref wxs ia)) (way (aref wys ia)) (waz (aref wzs ia))
                        (vbx (aref vxs ib)) (vby (aref vys ib)) (vbz (aref vzs ib))
                        (wbx (aref wxs ib)) (wby (aref wys ib)) (wbz (aref wzs ib))
                        (vrax (+ vax (- (* way raz) (* waz ray))))
                        (vray (+ vay (- (* waz rax) (* wax raz))))
                        (vraz (+ vaz (- (* wax ray) (* way rax))))
                        (vrbx (+ vbx kvx (- (* wby rbz) (* wbz rby))))
                        (vrby (+ vby kvy (- (* wbz rbx) (* wbx rbz))))
                        (vrbz (+ vbz kvz (- (* wbx rby) (* wby rbx))))
                        (vn (+ (* (- vrbx vrax) nx) (* (- vrby vray) ny) (* (- vrbz vraz) nz)))
                        (old (aref normal-impulses c))
                        (delta (- (* (aref normal-masses c) (+ vn (* restitution relative)))))
                        (new (max (+ old delta) 0f0))
                        (applied (- new old))
                        (px (* applied nx)) (py (* applied ny)) (pz (* applied nz)))
                   (declare (fixnum ia ib)
                            (single-float nx ny nz rax ray raz rbx rby rbz kvx kvy kvz
                                          ima imb iia iib vax vay vaz wax way waz
                                          vbx vby vbz wbx wby wbz vrax vray vraz
                                          vrbx vrby vrbz vn old delta new applied px py pz))
                   (setf (aref normal-impulses c) new
                         (aref total-normal-impulses c) (+ (aref total-normal-impulses c) new))
                   (setf (aref vxs ia) (- vax (* ima px))
                         (aref vys ia) (- vay (* ima py))
                         (aref vzs ia) (- vaz (* ima pz))
                         (aref vxs ib) (+ vbx (* imb px))
                         (aref vys ib) (+ vby (* imb py))
                         (aref vzs ib) (+ vbz (* imb pz)))
                   (setf (aref wxs ia) (- wax (* iia (- (* ray pz) (* raz py))))
                         (aref wys ia) (- way (* iia (- (* raz px) (* rax pz))))
                         (aref wzs ia) (- waz (* iia (- (* rax py) (* ray px))))
                         (aref wxs ib) (+ wbx (* iib (- (* rby pz) (* rbz py))))
                         (aref wys ib) (+ wby (* iib (- (* rbz px) (* rbx pz))))
                         (aref wzs ib) (+ wbz (* iib (- (* rbx py) (* rby px)))))))))
    constraints))

(defmethod physics-apply-restitution
    ((kernels (eql :scalar)) constraints awake starts threshold)
  (declare (single-float threshold))
  (do-physics-colors (start end starts)
    (%physics-apply-restitution-scalar constraints awake start end threshold))
  constraints)

;;; ---------------------------------------------------------------------
;;; Storing impulses back, with the touching state machine and hit events.

(defun store-physics-impulses (world)
  (let* ((constraints (physics-world-constraints world))
         (contacts (physics-world-contacts world))
         (events (physics-world-events world))
         (awake (physics-world-awake world))
         (hit-speed (coerce *physics-hit-speed* 'single-float)))
    (declare (single-float hit-speed) (optimize (speed 3) (safety 1)))
    (records:with-columnar-buffer-storage
        ((count row (rows contact) (body-as body-a)
          (normal-impulses normal-impulse)
          (tangent-impulses-1 tangent-impulse-1) (tangent-impulses-2 tangent-impulse-2)
          (rolling-impulses-x rolling-impulse-x) (rolling-impulses-y rolling-impulse-y)
          (rolling-impulses-z rolling-impulse-z)
          (totals total-normal-impulse) (relative-velocities relative-velocity))
         constraints physics-constraint-columns)
      (declare (ignore row))
      (dotimes (c count)
        (let* ((row (aref rows c))
               (total (aref totals c))
               (touching (if (plusp total) 1 0))
               (was-touching (aref (%lane physics-contact-columns contacts touching) row)))
          (declare (fixnum row touching was-touching) (single-float total))
          (setf (aref (%lane physics-contact-columns contacts normal-impulse) row)
                (aref normal-impulses c)
                (aref (%lane physics-contact-columns contacts tangent-impulse-1) row)
                (aref tangent-impulses-1 c)
                (aref (%lane physics-contact-columns contacts tangent-impulse-2) row)
                (aref tangent-impulses-2 c)
                (aref (%lane physics-contact-columns contacts rolling-impulse-x) row)
                (aref rolling-impulses-x c)
                (aref (%lane physics-contact-columns contacts rolling-impulse-y) row)
                (aref rolling-impulses-y c)
                (aref (%lane physics-contact-columns contacts rolling-impulse-z) row)
                (aref rolling-impulses-z c)
                (aref (%lane physics-contact-columns contacts touching) row) touching)
          (unless (= touching was-touching)
            (physics-event-columns-push
             events (if (plusp touching) :begin :end)
             (aref (%lane physics-contact-columns contacts handle-a) row)
             (aref (%lane physics-contact-columns contacts handle-b) row)
             (aref (%lane physics-contact-columns contacts owner) row)
             (aref (%lane physics-contact-columns contacts px) row)
             (aref (%lane physics-contact-columns contacts py) row)
             (aref (%lane physics-contact-columns contacts pz) row)
             (- (aref relative-velocities c))))
          ;; A hit: it pushed, and it arrived fast, and someone wants to know.
          (when (and (plusp total)
                     (< (aref relative-velocities c) (- hit-speed))
                     (or (aref (%lane physics-contact-columns contacts owner) row)
                         (logtest (aref (%lane physics-body-columns awake flags)
                                        (aref body-as c))
                                  +physics-body-hit-report+)))
            (physics-event-columns-push
             events :hit
             (aref (%lane physics-contact-columns contacts handle-a) row)
             (aref (%lane physics-contact-columns contacts handle-b) row)
             (aref (%lane physics-contact-columns contacts owner) row)
             (aref (%lane physics-contact-columns contacts px) row)
             (aref (%lane physics-contact-columns contacts py) row)
             (aref (%lane physics-contact-columns contacts pz) row)
             (- (aref relative-velocities c)))))))
    world))

;;; ---------------------------------------------------------------------
;;; Finalizing a step: fold the deltas into positions, age the mortal,
;;; and put the still to sleep.

(defun finalize-physics-bodies (world dt)
  (declare (single-float dt))
  (let* ((awake (physics-world-awake world))
         (events (physics-world-events world))
         (sleep-speed (coerce *physics-sleep-speed* 'single-float))
         (sleep-seconds (coerce *physics-sleep-seconds* 'single-float))
         (inverse-dt (/ dt))
         (expired nil)
         (sleepy nil))
    (declare (single-float sleep-speed sleep-seconds inverse-dt))
    (records:with-columnar-buffer-storage
        ((count row (xs x) (ys y) (zs z) (dxs dx) (dys dy) (dzs dz)
          (vxs vx) (vys vy) (vzs vz) (wxs wx) (wys wy) (wzs wz)
          (radii radius) (lifetimes lifetime) (sleep-times sleep-time)
          (flags flags) (handles handle))
         awake physics-body-columns)
      (declare (ignore row) (optimize (speed 3) (safety 0)))
      (dotimes (i count)
        (let* ((dx (aref dxs i)) (dy (aref dys i)) (dz (aref dzs i))
               (vx (aref vxs i)) (vy (aref vys i)) (vz (aref vzs i))
               (wx (aref wxs i)) (wy (aref wys i)) (wz (aref wzs i))
               (speed (+ (sqrt (the (single-float 0f0) (+ (* vx vx) (* vy vy) (* vz vz))))
                         (* (aref radii i) (sqrt (the (single-float 0f0) (+ (* wx wx) (* wy wy) (* wz wz)))))))
               (correction (* 0.5f0 inverse-dt (sqrt (the (single-float 0f0) (+ (* dx dx) (* dy dy) (* dz dz))))))
               (sleep-velocity (max speed correction)))
          (declare (single-float dx dy dz vx vy vz wx wy wz speed correction sleep-velocity))
          (setf (aref xs i) (+ (aref xs i) dx)
                (aref ys i) (+ (aref ys i) dy)
                (aref zs i) (+ (aref zs i) dz)
                (aref dxs i) 0f0 (aref dys i) 0f0 (aref dzs i) 0f0)
          (let ((lifetime (aref lifetimes i)))
            (declare (single-float lifetime))
            (when (>= lifetime 0f0)
              (let ((left (- lifetime dt)))
                (setf (aref lifetimes i) left)
                (when (minusp left)
                  (push (aref handles i) expired)))))
          (if (or (> sleep-velocity sleep-speed)
                  (logtest (aref flags i) +physics-body-never-sleeps+))
              (setf (aref sleep-times i) 0f0)
              (let ((slept (+ (aref sleep-times i) dt)))
                (declare (single-float slept))
                (setf (aref sleep-times i) slept)
                (when (> slept sleep-seconds)
                  (push (aref handles i) sleepy)))))))
    ;; The sleepers age too, or a mortal body could sleep for ever.
    (records:with-columnar-buffer-storage
        ((count row (lifetimes lifetime) (handles handle))
         (physics-world-sleeping world) physics-body-columns)
      (declare (ignore row))
      (dotimes (i count)
        (let ((lifetime (aref lifetimes i)))
          (when (>= lifetime 0f0)
            (let ((left (- lifetime dt)))
              (setf (aref lifetimes i) left)
              (when (minusp left)
                (push (aref handles i) expired)))))))
    ;; Relocations after the loop: they move rows.
    (dolist (handle expired)
      (physics-event-columns-push events :expired handle +physics-no-body+ nil
                                  0f0 0f0 0f0 0f0)
      (destroy-physics-body world handle))
    (dolist (handle sleepy)
      (when (physics-body-alive-p world handle)
        (sleep-physics-body world handle)))
    world))

(defun wake-physics-bodies-near (world x y z radius)
  "Wake every sleeping body within RADIUS of X,Y,Z: the ground moved."
  (let ((sleeping (physics-world-sleeping world))
        (x (coerce x 'single-float)) (y (coerce y 'single-float))
        (z (coerce z 'single-float)) (radius (coerce radius 'single-float))
        (woken nil))
    (records:with-columnar-buffer-storage
        ((count row (xs x) (ys y) (zs z) (radii radius) (handles handle))
         sleeping physics-body-columns)
      (declare (ignore row))
      (dotimes (i count)
        (let ((dx (- (aref xs i) x)) (dy (- (aref ys i) y)) (dz (- (aref zs i) z))
              (limit (+ radius (aref radii i))))
          (when (< (+ (* dx dx) (* dy dy) (* dz dz)) (* limit limit))
            (push (aref handles i) woken)))))
    (dolist (handle woken)
      (wake-physics-body world handle))
    (length woken)))

;;; ---------------------------------------------------------------------
;;; The step.

(defun %write-physics-dummy-body (world)
  "Zero the row past the awake set's end: the static side of every contact."
  (let* ((awake (physics-world-awake world))
         (dummy (physics-body-columns-length awake)))
    (when (>= dummy (physics-body-columns-capacity awake))
      (%physics-body-columns-grow awake (1+ dummy)))
    (macrolet ((zero (&rest lanes)
                 `(setf ,@(loop for lane in lanes
                                append `((aref (,(intern (format nil "PHYSICS-BODY-COLUMNS-~A-LANE" lane))
                                                awake)
                                               dummy)
                                         0f0)))))
      (zero vx vy vz wx wy wz dx dy dz inverse-mass inverse-inertia radius
            restitution friction rolling-resistance))
    world))

(defun step-physics-world (world &optional dt)
  "Advance WORLD by DT seconds (its step by default) and return it.
Events from the step are then readable until the next step begins."
  (let* ((dt (coerce (or dt (physics-world-step-seconds world)) 'single-float))
         (substeps (max 1 *physics-substeps*))
         (h (/ dt substeps))
         (inv-h (/ h))
         (kernels (physics-world-kernels world))
         (awake (physics-world-awake world))
         (constraints (physics-world-constraints world))
         (starts (physics-world-color-starts world))
         (push-max (coerce *physics-contact-push-max-velocity* 'single-float))
         (threshold (coerce *physics-restitution-threshold* 'single-float))
         (started (get-internal-real-time)))
    (declare (single-float dt h inv-h push-max threshold) (fixnum substeps))
    (physics-event-columns-reset (physics-world-events world))
    (incf (physics-world-step-count world))
    (reset-terrain-probe (physics-world-probe world) (physics-world-terrain world))
    ;; Contacts.
    (rebuild-physics-grid world)
    (generate-physics-body-pairs world)
    (generate-physics-terrain-contacts world)
    (generate-physics-box-contacts world)
    (prune-physics-contacts world)
    (%write-physics-dummy-body world)
    (let ((color-count (prepare-physics-constraints world h)))
      ;; The Soft Step loop.
      (dotimes (substep substeps)
        (let ((elapsed (* h substep)))
          (declare (single-float elapsed))
          (physics-integrate-velocities kernels awake h)
          (physics-warm-start kernels constraints awake starts)
          (physics-solve-contacts kernels constraints awake starts
                                  inv-h t elapsed push-max)
          (physics-integrate-positions kernels awake h)
          (physics-solve-contacts kernels constraints awake starts
                                  inv-h nil (+ elapsed h) push-max)))
      (physics-apply-restitution kernels constraints awake starts threshold)
      (store-physics-impulses world)
      (finalize-physics-bodies world dt)
      (setf (physics-world-last-step-contact-count world)
            (physics-constraint-columns-length constraints)
            (physics-world-last-step-color-count world) color-count))
    (setf (physics-world-last-step-real-seconds world)
          (/ (- (get-internal-real-time) started)
             (coerce internal-time-units-per-second 'double-float)))
    world))

;;; ---------------------------------------------------------------------
;;; Reading the events and the world.

(defmacro do-physics-events ((kind handle-a handle-b owner x y z speed world) &body body)
  "Run BODY for each event of WORLD's last step."
  (let ((events (gensym "EVENTS")) (i (gensym "I")))
    `(let ((,events (physics-world-events ,world)))
       (dotimes (,i (physics-event-columns-length ,events))
         (let ((,kind (aref (physics-event-columns-kind-lane ,events) ,i))
               (,handle-a (aref (physics-event-columns-handle-a-lane ,events) ,i))
               (,handle-b (aref (physics-event-columns-handle-b-lane ,events) ,i))
               (,owner (aref (physics-event-columns-owner-lane ,events) ,i))
               (,x (aref (physics-event-columns-x-lane ,events) ,i))
               (,y (aref (physics-event-columns-y-lane ,events) ,i))
               (,z (aref (physics-event-columns-z-lane ,events) ,i))
               (,speed (aref (physics-event-columns-speed-lane ,events) ,i)))
           (declare (ignorable ,kind ,handle-a ,handle-b ,owner ,x ,y ,z ,speed))
           ,@body)))))

(defun physics-events (world)
  "The last step's events as a list of plists, for inspection."
  (let ((result nil))
    (do-physics-events (kind a b owner x y z speed world)
      (push (list :kind kind :a a :b b :owner owner :x x :y y :z z :speed speed)
            result))
    (nreverse result)))

(defun physics-world-state-hash (world)
  "A hash of every body's position and velocity, in set and row order, so
two runs of the same code can be compared.  Same-image reproducibility is
the claim (#S6T2MV); this is how it is policed."
  (let ((hash 0))
    (declare (type (unsigned-byte 62) hash))
    (flet ((mix (value)
             (setf hash (logand (+ (* hash 1099511628211)
                                   (sb-kernel:single-float-bits (coerce value 'single-float)))
                                #x3fffffffffffffff))))
      (dolist (columns (list (physics-world-awake world) (physics-world-sleeping world)))
        (records:with-columnar-buffer-storage
            ((count row (xs x) (ys y) (zs z) (vxs vx) (vys vy) (vzs vz))
             columns physics-body-columns)
          (declare (ignore row))
          (dotimes (i count)
            (mix (aref xs i)) (mix (aref ys i)) (mix (aref zs i))
            (mix (aref vxs i)) (mix (aref vys i)) (mix (aref vzs i))))))
    hash))

(defun validate-physics-world (world)
  "Check every forwarding address, the way B3VALIDATESOLVERSETS does: each
live id names a row whose handle names it back, and each contact row is
where its key says.  Signal on the first inconsistency; return T."
  (let ((ids (physics-world-ids world)))
    (dotimes (index (physics-id-table-next ids))
      (let ((set (aref (physics-id-table-set ids) index)))
        (unless (= set +physics-set-free+)
          (let* ((columns (physics-body-columns-for-set world set))
                 (local (aref (physics-id-table-local ids) index)))
            (unless (< local (physics-body-columns-length columns))
              (error "Body ~D points past its set: ~D." index local))
            (let ((handle (aref (physics-body-columns-handle-lane columns) local)))
              (unless (= (physics-handle-index handle) index)
                (error "Body ~D's row ~D in set ~D belongs to ~D."
                       index local set (physics-handle-index handle))))))))
    (dolist (columns (list (physics-world-awake world) (physics-world-sleeping world)))
      (dotimes (local (physics-body-columns-length columns))
        (let ((handle (aref (physics-body-columns-handle-lane columns) local)))
          (unless (physics-body-alive-p world handle)
            (error "Row ~D holds a dead handle ~D." local handle))
          (multiple-value-bind (set index) (physics-body-set-and-local world handle)
            (unless (and (eq columns (physics-body-columns-for-set world set))
                         (= index local))
              (error "Handle ~D at row ~D is filed at ~D/~D." handle local set index))))))
    (let ((contacts (physics-world-contacts world))
          (index (physics-world-contact-index world)))
      (dotimes (row (physics-contact-columns-length contacts))
        (unless (eql row (gethash (aref (physics-contact-columns-key-lane contacts) row) index))
          (error "Contact row ~D is not indexed by its key." row)))
      (unless (= (hash-table-count index) (physics-contact-columns-length contacts))
        (error "The contact index has ~D entries for ~D rows."
               (hash-table-count index) (physics-contact-columns-length contacts))))
    t))
