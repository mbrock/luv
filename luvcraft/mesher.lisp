;;; Turning resident block data into exposed-face triangle meshes.
;;;
;;; The result of meshing is deliberately mundane: interleaved position,
;;; UV/shade, and normal triples of single floats.  Meshing always starts by
;;; gathering: a chunk and its one-cell halo are copied into an immutable
;;; BLOCK-MESH-SNAPSHOT of u16 palette indices and u8 light levels (whether
;;; for a worker thread or for owner-side meshing), and the small closed block
;;; vocabulary is projected through its protocol generics into flat per-index
;;; tables once per job.  The dense loop then reads only those columns and
;;; tables; no block object, face object, or generic function is consulted
;;; per sample.

(in-package #:luvcraft)

(defclass block-mesher () ())
(defclass exposed-face-mesher (block-mesher)
  ((absent-neighbor-policy
    :initarg :absent-neighbor-policy
    :initform :air
    :reader exposed-face-mesher-absent-neighbor-policy)))

(defclass block-mesh ()
  ((vertex-declaration
    :initarg :vertex-declaration
    :initform (luv.arithmetic:value-declaration-for :block-mesh-vertices)
    :reader block-mesh-vertex-declaration)
   (vertices :initarg :vertices :reader block-mesh-vertices)
   (vertex-count :initarg :vertex-count :reader block-mesh-vertex-count)
   (face-count :initarg :face-count :reader block-mesh-face-count)))

(defmethod initialize-instance :after ((mesh block-mesh) &key)
  (let* ((declaration (block-mesh-vertex-declaration mesh))
         (layout (luv.arithmetic:declaration-quantity-layout declaration))
         (vertices (block-mesh-vertices mesh)))
    (unless (typep vertices
                   (luv.arithmetic:declaration-representation-type declaration))
      (error "Block mesh vertices ~S do not satisfy ~S."
             (type-of vertices)
             (luv.arithmetic:declaration-representation-type declaration)))
    (unless (typep layout 'luv.arithmetic:repeated-quantity-layout)
      (error "Block mesh declaration has no repeated vertex layout: ~S."
             declaration))
    (unless (= (length vertices)
               (* (block-mesh-vertex-count mesh)
                  (luv.arithmetic:repeated-quantity-layout-stride layout)))
      (error "Block mesh has ~D lanes for ~D declared vertices at stride ~D."
             (length vertices) (block-mesh-vertex-count mesh)
             (luv.arithmetic:repeated-quantity-layout-stride layout)))))

(defun merge-block-mesh-vertex-declaration (accumulator mesh)
  "Preserve one exact layout identity while concatenating MESH products."
  (let ((declaration (block-mesh-vertex-declaration mesh)))
    (when (and accumulator (not (eq accumulator declaration)))
      (error "Cannot combine block meshes carrying different vertex layouts."))
    declaration))

(defclass block-mesh-halo-domain ()
  ((chunk-domain :initarg :chunk-domain
                 :reader block-mesh-halo-domain-chunk-domain))
  (:documentation
   "The sites in one chunk plus the one-cell sampling halo around it."))

(defun make-block-mesh-halo-domain (chunk-domain)
  (check-type chunk-domain chunk-domain)
  (make-instance 'block-mesh-halo-domain :chunk-domain chunk-domain))

(defmethod domains:domain-cardinality ((domain block-mesh-halo-domain))
  (let* ((chunk-domain (block-mesh-halo-domain-chunk-domain domain))
         (shape (voxel-space-chunk-shape
                 (chunk-domain-space chunk-domain))))
    (* (+ 2 (chunk-shape-width shape))
       (+ 2 (chunk-shape-height shape))
       (+ 2 (chunk-shape-depth shape)))))

(records:define-columnar-materialization block-mesh-halo-fields
  ;; CONTENT-INDEX is a projection through PALETTE, not the logical
  ;; :BLOCK-CONTENT declaration itself.  The light lanes already have direct
  ;; represented field declarations and bind those below.
  (content-index 0 :type (unsigned-byte 16))
  (sky-level 0 :type (unsigned-byte 8))
  (block-level 0 :type (unsigned-byte 8)))

(defclass block-mesh-snapshot ()
  ((key :initarg :key :reader block-mesh-snapshot-key)
   (dependency-stamp :initarg :dependency-stamp
                     :reader block-mesh-snapshot-dependency-stamp)
   (domain :initarg :domain :reader block-mesh-snapshot-domain)
   (content-definition :initarg :content-definition
                       :reader block-mesh-snapshot-content-definition)
   ;; A frozen copy of the world vocabulary's members at capture time: the
   ;; halo's u16 indices are closed by it.  Its length is the ABSENT index,
   ;; one past the last member, so a halo sample outside resident terrain is
   ;; a distinct member of the snapshot's own vocabulary.
   (palette :initarg :palette :reader block-mesh-snapshot-palette)
   (halo-fields :initarg :halo-fields
                :reader block-mesh-snapshot-halo-fields))
  (:documentation
   "An immutable dense chunk plus one-cell halo transferred to a CPU worker.

The snapshot owns compact u16 columns whose indices are the world vocabulary's
own offsets, frozen by copying the member vector; the copy's length stands for
an absent sample.  Cell identity does not cross the thread boundary, and the
worker never observes live chunk storage."))

(declaim (inline block-mesh-snapshot-absent-index))
(defun block-mesh-snapshot-absent-index (snapshot)
  (length (the simple-vector (block-mesh-snapshot-palette snapshot))))

(declaim
 (inline block-mesh-snapshot-halo-domain
         block-mesh-snapshot-sample-indices
         block-mesh-snapshot-sky-samples
         block-mesh-snapshot-block-light-samples))

(defun block-mesh-snapshot-halo-domain (snapshot)
  (block-mesh-halo-fields-domain
   (block-mesh-snapshot-halo-fields snapshot)))

(defun block-mesh-snapshot-sample-indices (snapshot)
  (block-mesh-halo-fields-content-index-lane
   (block-mesh-snapshot-halo-fields snapshot)))

(defun block-mesh-snapshot-sky-samples (snapshot)
  (block-mesh-halo-fields-sky-level-lane
   (block-mesh-snapshot-halo-fields snapshot)))

(defun block-mesh-snapshot-block-light-samples (snapshot)
  (block-mesh-halo-fields-block-level-lane
   (block-mesh-snapshot-halo-fields snapshot)))

(defun block-mesh-snapshot-sky-definition (snapshot)
  (records:columnar-row-lane-declaration
   (block-mesh-halo-fields-row-declaration
    (block-mesh-snapshot-halo-fields snapshot))
   'sky-level))

(defun block-mesh-snapshot-block-light-definition (snapshot)
  (records:columnar-row-lane-declaration
   (block-mesh-halo-fields-row-declaration
    (block-mesh-snapshot-halo-fields snapshot))
   'block-level))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((snapshot block-mesh-snapshot) (field-name (eql :block-content)))
  (declare (ignore field-name))
  (block-mesh-snapshot-content-definition snapshot))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((snapshot block-mesh-snapshot) (field-name (eql :sky-light)))
  (declare (ignore field-name))
  (block-mesh-snapshot-sky-definition snapshot))

(defmethod luvcraft.world.fields:materialized-field-definition
    ((snapshot block-mesh-snapshot) (field-name (eql :block-light)))
  (declare (ignore field-name))
  (block-mesh-snapshot-block-light-definition snapshot))

(defgeneric mesh-block-world (mesher world))
(defgeneric mesh-block-chunk (mesher world chunk))
(defgeneric mesh-block-snapshot (mesher snapshot))
(defgeneric emit-block-face (mesher world vertices block face x y z))

(defconstant +block-mesh-floats-per-vertex+ 14)
(defconstant +block-mesh-vertices-per-face+ 6)
(defconstant +block-mesh-floats-per-face+
  (* +block-mesh-floats-per-vertex+ +block-mesh-vertices-per-face+))

(declaim (inline push-block-vertex-components))
(defun push-block-vertex-components
    (vertices px py pz local-u local-v shade nx ny nz
     sky-level block-level emission tile
     edge-u-low edge-u-high edge-v-low edge-v-high)
  "Append one interleaved vertex without constructing tuple objects.

The fourth lane carries normalized raw light readings, not an
art-directed bake: shader edits can change the response curve without
remeshing the world.  The final two scalars carry the atlas tile offset under
the atlas mapping and the face's four ternary edge classifications.  Atlas UV
resolution and edge unpacking belong to the shaders, not vertex producers."
  (vector-push (coerce px 'single-float) vertices)
  (vector-push (coerce py 'single-float) vertices)
  (vector-push (coerce pz 'single-float) vertices)
  ;; The half-texel inset is local to a tile, independent of which atlas lane
  ;; owns it or how wide the materialization is.
  (vector-push (coerce (/ (+ 0.5 (* local-u 15)) 16) 'single-float) vertices)
  (vector-push (coerce (/ (+ 0.5 (* local-v 15)) 16) 'single-float) vertices)
  (vector-push (coerce shade 'single-float) vertices)
  (vector-push (coerce nx 'single-float) vertices)
  (vector-push (coerce ny 'single-float) vertices)
  (vector-push (coerce nz 'single-float) vertices)
  (vector-push (coerce sky-level 'single-float) vertices)
  (vector-push (coerce block-level 'single-float) vertices)
  (vector-push (coerce emission 'single-float) vertices)
  (vector-push (coerce tile 'single-float) vertices)
  (vector-push
   (coerce (+ (* (+ edge-u-low 1) 27)
              (* (+ edge-u-high 1) 9)
              (* (+ edge-v-low 1) 3)
              (+ edge-v-high 1))
           'single-float)
   vertices)
  vertices)

(defun block-color-variation (x y z)
  (+ 0.93 (* 0.07 (/ (mod (+ (* x 17) (* y 31) (* z 13)) 7) 6.0))))

(defstruct (block-mesh-neighborhood
             (:constructor %make-block-mesh-neighborhood))
  "The 3x3x3 resident chunk neighborhood needed by one chunk mesh."
  (domain nil :type (or null chunk-domain))
  (chunks (make-array 27 :initial-element nil) :type simple-vector))

(defun block-mesh-neighborhood-index (dx dy dz)
  (+ (1+ dx) (* 3 (+ (1+ dy) (* 3 (1+ dz))))))

(defun make-block-mesh-neighborhood (world chunk)
  "Resolve once the chunks every visibility and AO sample can reach."
  (let* ((domain (block-chunk-domain chunk))
         (coordinate (chunk-domain-coordinate domain))
         (chunk-x (chunk-coordinate-x coordinate))
         (chunk-y (chunk-coordinate-y coordinate))
         (chunk-z (chunk-coordinate-z coordinate))
         (chunks (make-array 27 :initial-element nil)))
    (loop for dz from -1 to 1 do
      (loop for dy from -1 to 1 do
        (loop for dx from -1 to 1 do
          (setf (aref chunks (block-mesh-neighborhood-index dx dy dz))
                (world-chunk-at world
                                (+ chunk-x dx)
                                (+ chunk-y dy)
                                (+ chunk-z dz))))))
    (%make-block-mesh-neighborhood :domain domain :chunks chunks)))

(declaim (inline block-mesh-neighborhood-locate))
(defun block-mesh-neighborhood-locate (neighborhood x y z)
  "Resolve one nearby world site to its resident chunk and dense offset.

The 3x3x3 window owns availability; voxel-space and chunk-domain own the
decomposition and storage order beneath it.  See #K3KZTG."
  (let* ((domain (block-mesh-neighborhood-domain neighborhood))
         (space (chunk-domain-space domain))
         (center (chunk-domain-coordinate domain)))
    (multiple-value-bind
          (chunk-x chunk-y chunk-z local-x local-y local-z)
        (voxel-space-decompose-components space x y z)
      (let ((dx (- chunk-x (chunk-coordinate-x center)))
            (dy (- chunk-y (chunk-coordinate-y center)))
            (dz (- chunk-z (chunk-coordinate-z center))))
        (when (and (<= -1 dx 1) (<= -1 dy 1) (<= -1 dz 1))
          (let ((chunk
                  (aref (block-mesh-neighborhood-chunks neighborhood)
                        (block-mesh-neighborhood-index dx dy dz))))
            (when chunk
              (values chunk
                      (chunk-domain-offset-components
                       (block-chunk-domain chunk)
                       local-x local-y local-z)))))))))

(defmethod locate-chunk-window-site
    ((neighborhood block-mesh-neighborhood) x y z)
  (multiple-value-bind (chunk offset)
      (block-mesh-neighborhood-locate neighborhood x y z)
    (if chunk
        (values chunk offset :available)
        (values nil nil :unavailable))))

(declaim (inline block-mesh-neighborhood-block-at))
(defun block-mesh-neighborhood-block-at (neighborhood x y z)
  "Read a nearby world site with no coordinate objects or hash-key consing."
  (multiple-value-bind (chunk offset)
      (block-mesh-neighborhood-locate neighborhood x y z)
    (if chunk
        (values (block-content-at-offset
                 (block-chunk-content chunk) offset)
                :resident)
        (values nil :absent))))

(declaim (inline block-mesh-halo-offset-components))
(defun block-mesh-halo-offset-components (halo-domain x y z)
  "Return the dense one-cell-halo offset for a world site, or NIL outside."
  (let* ((domain (block-mesh-halo-domain-chunk-domain halo-domain))
         (shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (sample-width (+ 2 (chunk-shape-width shape)))
         (sample-height (+ 2 (chunk-shape-height shape)))
         (sample-depth (+ 2 (chunk-shape-depth shape))))
    (multiple-value-bind (origin-x origin-y origin-z)
        (chunk-domain-world-components domain 0 0 0)
      (let ((sample-x (1+ (- x origin-x)))
            (sample-y (1+ (- y origin-y)))
            (sample-z (1+ (- z origin-z))))
        (when (and (<= 0 sample-x) (< sample-x sample-width)
                   (<= 0 sample-y) (< sample-y sample-height)
                   (<= 0 sample-z) (< sample-z sample-depth))
          (+ sample-x
             (* sample-width
                (+ sample-y (* sample-height sample-z)))))))))

(declaim (inline block-mesh-snapshot-locate block-mesh-snapshot-block-at))
(defun block-mesh-snapshot-locate (snapshot x y z)
  "Resolve one halo site without aggregate dispatch in the meshing loop."
  (let ((offset (block-mesh-halo-offset-components
                 (block-mesh-snapshot-halo-domain snapshot) x y z)))
    (if (and offset
             (/= (aref (block-mesh-snapshot-sample-indices snapshot) offset)
                 (block-mesh-snapshot-absent-index snapshot)))
        (values snapshot offset :available)
        (values nil nil :unavailable))))

(defun block-mesh-snapshot-block-at (snapshot x y z)
  (multiple-value-bind (materialization offset availability)
      (block-mesh-snapshot-locate snapshot x y z)
    (declare (ignore materialization))
    (ecase availability
      (:available
       (values
        (aref (block-mesh-snapshot-palette snapshot)
              (aref (block-mesh-snapshot-sample-indices snapshot) offset))
        :resident))
      (:unavailable (values nil :absent)))))

(defmethod locate-chunk-window-site ((snapshot block-mesh-snapshot) x y z)
  (block-mesh-snapshot-locate snapshot x y z))

(defgeneric sample-block-at (samples x y z)
  (:documentation
   "Read one world site from a meshing sample source.

Return (VALUES BLOCK STATUS) where STATUS is :RESIDENT or :ABSENT.  Meshing
itself never chooses a representation: the live world, a preresolved chunk
neighborhood, and an immutable worker snapshot each answer through their own
method, and further sample sources only need to add one."))

(defmethod sample-block-at ((samples block-world) x y z)
  (world-block-at samples x y z))

(defmethod sample-block-at ((samples block-mesh-neighborhood) x y z)
  (block-mesh-neighborhood-block-at samples x y z))

(defmethod sample-block-at ((samples block-mesh-snapshot) x y z)
  (block-mesh-snapshot-block-at samples x y z))

(defgeneric sample-light-at (samples x y z)
  (:documentation
   "Read one site's published light from a meshing sample source.

Return (VALUES SKY BLOCK STATUS) with raw 0..15 levels.  STATUS is
:RESIDENT or :ABSENT; an absent or unlit sample answers zeros, and the
corner-averaging rules decide what that means rather than any caller
falling through to BLOCK-SOLID-P."))

(defmethod sample-light-at ((samples block-world) x y z)
  (multiple-value-bind (sky block state) (world-light-at samples x y z)
    (if (eq state :absent)
        (values 0 0 :absent)
        (values sky block :resident))))

(defmethod sample-light-at ((samples block-mesh-neighborhood) x y z)
  (multiple-value-bind (chunk offset)
      (block-mesh-neighborhood-locate samples x y z)
    (if chunk
        (let ((field (block-chunk-light-field chunk)))
          (if field
              (values
               (aref (chunk-light-field-sky-levels field) offset)
               (aref (chunk-light-field-block-levels field) offset)
               :resident)
              (values 0 0 :resident)))
        (values 0 0 :absent))))

(defmethod sample-light-at ((samples block-mesh-snapshot) x y z)
  (multiple-value-bind (materialization offset availability)
      (block-mesh-snapshot-locate samples x y z)
    (declare (ignore materialization))
    (ecase availability
      (:available
       (values
        (aref (block-mesh-snapshot-sky-samples samples) offset)
        (aref (block-mesh-snapshot-block-light-samples samples) offset)
        :resident))
      (:unavailable (values 0 0 :absent)))))

;;; What a fragment cannot know about its own face is what lies beyond its
;;; edges.  The mesher does know, so it classifies each of a face's four
;;; in-plane boundaries once and hands the answer to the surface shader.
;;;
;;; This is the difference between a block world that reads as carved solids
;;; and one that reads as a lit grid: rounding every face edge over would draw
;;; a seam across the middle of an open plain, where the face merely continues
;;; into an identically oriented neighbour and there is no edge at all.
;;;
;;; -1  concave: a block rises across this edge, so the surface fillets into
;;;              the inner corner and gathers a little occlusion there.
;;;  0  flush:   the same plane continues; leave the surface alone.
;;;  1  convex:  nothing beyond the edge, so the surface rounds over it.

(defconstant +block-face-edge-concave+ -1.0)
(defconstant +block-face-edge-flush+ 0.0)
(defconstant +block-face-edge-convex+ 1.0)

;;; Gathering.
;;;
;;; The mesher starts by collecting everything it will ask of the block
;;; vocabulary, once per job, into a few flat tables indexed by palette
;;; position: occupancy, surface emission, and one atlas tile offset per
;;; face.  The loop below then reads u16 palette indices and u8 light levels
;;; straight out of the halo columns and never consults a block object, a
;;; face object, or a generic function per sample.  The vocabulary is small
;;; and closed -- it changes only when someone redefines a kind at the REPL
;;; -- so the gathering is cheap and the protocol generics still decide what
;;; each kind means.  See #FIGJ9R.

(defstruct (block-mesh-kind-tables (:constructor %make-block-mesh-kind-tables))
  "Per-palette-index answers the mesher gathered before its dense loop."
  (solid (make-array 0 :element-type 'bit)
   :type (simple-array bit (*)))
  (emission (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)))
  ;; Six tile offsets per palette index, in *BLOCK-FACES* order.
  (tiles (make-array 0 :element-type '(unsigned-byte 16))
   :type (simple-array (unsigned-byte 16) (*))))

(defun gather-block-mesh-kind-tables (mesher palette)
  "Project PALETTE through the block protocol into flat per-index tables.

The tables have one more row than PALETTE: the final row is the absent
sample, whose occupancy comes from the mesher's absent-neighbor policy.
Index zero is air (NIL); every other entry is a block kind.  Faces are
numbered in *BLOCK-FACES* order."
  (let* ((count (1+ (length palette)))
         (absent (length palette))
         (solid (make-array count :element-type 'bit :initial-element 0))
         (emission (make-array count :element-type 'single-float
                                     :initial-element 0.0))
         (tiles (make-array (* 6 count) :element-type '(unsigned-byte 16)
                                        :initial-element 0))
         (policy (exposed-face-mesher-absent-neighbor-policy mesher)))
    (setf (sbit solid absent)
          (ecase policy
            (:air 0)
            (:solid (if (block-solid-p *stone-block*) 1 0))
            (:error 0)))
    (loop for index from 0 below absent
          for block = (aref palette index)
          do (when (block-solid-p block)
               (setf (sbit solid index) 1))
             (setf (aref emission index)
                   (coerce (block-surface-emission block) 'single-float))
             (when block
               (loop for face in *block-faces*
                     for face-index from 0
                     do (setf (aref tiles (+ (* 6 index) face-index))
                              (block-atlas-tile-offset
                               (block-face-tile block face))))))
    (%make-block-mesh-kind-tables :solid solid :emission emission
                                  :tiles tiles)))

(defstruct (block-mesh-face-table (:constructor %make-block-mesh-face-table))
  "One face's geometry, gathered once from its BLOCK-FACE object."
  (nx 0 :type fixnum) (ny 0 :type fixnum) (nz 0 :type fixnum)
  ;; Four corners as (cx cy cz) triples, then local UV per corner.
  (corners (make-array 12 :element-type 'fixnum)
   :type (simple-array fixnum (12)))
  (uvs (make-array 8 :element-type 'fixnum)
   :type (simple-array fixnum (8))))

(defun gather-block-mesh-face-tables ()
  (map 'simple-vector
       (lambda (face)
         (let* ((normal (block-face-neighbor face))
                (corners (make-array 12 :element-type 'fixnum))
                (uvs (make-array 8 :element-type 'fixnum)))
           (loop for corner in (block-face-corners face)
                 for i from 0
                 do (setf (aref corners (* 3 i)) (first corner)
                          (aref corners (+ 1 (* 3 i))) (second corner)
                          (aref corners (+ 2 (* 3 i))) (third corner))
                    (multiple-value-bind (u v) (block-face-local-uv face corner)
                      (setf (aref uvs (* 2 i)) u
                            (aref uvs (+ 1 (* 2 i))) v)))
           (%make-block-mesh-face-table
            :nx (voxel-direction-dx normal)
            :ny (voxel-direction-dy normal)
            :nz (voxel-direction-dz normal)
            :corners corners :uvs uvs)))
       *block-faces*))

(defvar *block-mesh-face-tables* nil
  "Face geometry gathered from *BLOCK-FACES*; NIL until first gathered.")

(defun block-mesh-face-tables ()
  (or *block-mesh-face-tables*
      (setf *block-mesh-face-tables* (gather-block-mesh-face-tables))))

;;; The dense loop.

(deftype block-mesh-halo-index () '(integer 0 #.(ash 1 24)))
(deftype block-mesh-halo-extent () '(integer 0 256))
(deftype block-mesh-halo-step () '(integer #.(- (ash 1 24)) #.(ash 1 24)))

(declaim (inline block-mesh-corner-shade-and-light))
(defun block-mesh-corner-shade-and-light
    (solid indices sky block-light absent base nstep s1step s2step)
  "Return (VALUES AO SKY BLOCK) for one vertex corner.

BASE is the halo offset of the face's own cell; NSTEP steps across the face
normal and S1STEP/S2STEP along the two in-plane directions the corner leans
toward.  Occupancy and light averaging follow exactly the rules of the
former per-sample accessors: a solid cell holds no light, an absent sample
contributes nothing, and the diagonal is unreachable behind two sides."
  (declare (type (simple-array bit (*)) solid)
           (type (simple-array (unsigned-byte 16) (*)) indices)
           (type (simple-array (unsigned-byte 8) (*)) sky block-light)
           (type (unsigned-byte 16) absent)
           (type block-mesh-halo-index base)
           (type block-mesh-halo-step nstep s1step s2step)
           (optimize (speed 3) (safety 0)))
  (let* ((n (+ base nstep))
         (a (+ n s1step))
         (b (+ n s2step))
         (d (+ a s2step))
         (ia (aref indices a))
         (ib (aref indices b))
         (id (aref indices d))
         (in (aref indices n))
         (solid-a (= 1 (sbit solid ia)))
         (solid-b (= 1 (sbit solid ib)))
         (solid-d (= 1 (sbit solid id)))
         (sky-sum 0) (block-sum 0) (count 0))
    (declare (type block-mesh-halo-index n a b d)
             (type fixnum sky-sum block-sum count))
    (flet ((consider (index offset)
             (unless (= index absent)
               (incf sky-sum (aref sky offset))
               (incf block-sum (aref block-light offset))
               (incf count))))
      (declare (inline consider))
      (consider in n)
      (unless solid-a (consider ia a))
      (unless solid-b (consider ib b))
      (unless (and solid-a solid-b)
        (unless solid-d (consider id d))))
    (values (if (and solid-a solid-b)
                0.56
                (- 1.0 (* 0.14 (+ (if solid-a 1 0)
                                  (if solid-b 1 0)
                                  (if solid-d 1 0)))))
            (if (plusp count) (/ sky-sum (* 15.0 count)) 0.0)
            (if (plusp count) (/ block-sum (* 15.0 count)) 0.0))))

(defun emit-block-mesh-face
    (tables faces vertices fill solid indices sky block-light
     width height depth base palette-index face-index x y z)
  "Append one face's six vertices at FILL and return the new fill position.

BASE is the halo offset of the cell at world (X Y Z); WIDTH/HEIGHT/DEPTH
are the halo dimensions.  Vertex order and every lane value match what the
per-sample accessors produced, so existing meshes and tests agree."
  (declare (type block-mesh-kind-tables tables)
           (type simple-vector faces)
           (type (simple-array single-float (*)) vertices)
           (type fixnum fill)
           (type block-mesh-halo-index base)
           (type (simple-array bit (*)) solid)
           (type (simple-array (unsigned-byte 16) (*)) indices)
           (type (simple-array (unsigned-byte 8) (*)) sky block-light)
           (type block-mesh-halo-extent width height depth)
           (type fixnum x y z)
           (type (unsigned-byte 16) palette-index)
           (type (integer 0 5) face-index)
           (ignore depth)
           (optimize (speed 3) (safety 0)))
  (let* ((face (svref faces face-index))
         (absent (1- (length solid)))
         (nx (block-mesh-face-table-nx face))
         (ny (block-mesh-face-table-ny face))
         (nz (block-mesh-face-table-nz face))
         (corners (block-mesh-face-table-corners face))
         (uvs (block-mesh-face-table-uvs face))
         (xstep 1)
         (ystep width)
         (zstep (* width height))
         (nstep (+ (* nx xstep) (* ny ystep) (* nz zstep)))
         (tile (coerce (aref (block-mesh-kind-tables-tiles tables)
                             (+ (* 6 palette-index) face-index))
                       'single-float))
         (emission (aref (block-mesh-kind-tables-emission tables)
                         palette-index))
         (variation (block-color-variation x y z))
         ;; In-plane axes as the fragment stage chooses them: U is X unless
         ;; the normal is X (then Z); V is Y unless the normal is Y (then Z).
         (ustep (if (zerop nx) xstep zstep))
         (vstep (if (zerop ny) ystep zstep))
         ;; The two side axes a corner leans toward, in ascending axis order.
         (s1step (if (zerop nx) xstep ystep))
         (s2step (if (zerop nz) zstep ystep))
         (s1axis (if (zerop nx) 0 1))
         (s2axis (if (zerop nz) 2 1)))
    (declare (type (simple-array fixnum (12)) corners)
             (type (simple-array fixnum (8)) uvs)
             (type (integer -1 1) nx ny nz)
             (type (unsigned-byte 16) absent)
             (type block-mesh-halo-step xstep ystep zstep nstep ustep vstep
                   s1step s2step)
             (type single-float tile emission variation))
    (flet ((solid-at (offset)
             (declare (type block-mesh-halo-index offset))
             (= 1 (sbit solid (aref indices offset))))
           (emit (value)
             (declare (type single-float value))
             (setf (aref vertices fill) value)
             (incf fill)))
      (declare (inline solid-at emit))
      (flet ((edge (step)
               (declare (type block-mesh-halo-step step))
               (cond ((solid-at (+ base step nstep)) +block-face-edge-concave+)
                     ((solid-at (+ base step)) +block-face-edge-flush+)
                     (t +block-face-edge-convex+))))
        (declare (inline edge))
        (let* ((edge-u-low (edge (- ustep)))
               (edge-u-high (edge ustep))
               (edge-v-low (edge (- vstep)))
               (edge-v-high (edge vstep))
               (edge-code (coerce (+ (* (+ edge-u-low 1) 27)
                                     (* (+ edge-u-high 1) 9)
                                     (* (+ edge-v-low 1) 3)
                                     (+ edge-v-high 1))
                                  'single-float))
               (fnx (coerce nx 'single-float))
               (fny (coerce ny 'single-float))
               (fnz (coerce nz 'single-float)))
          (declare (type single-float edge-u-low edge-u-high
                         edge-v-low edge-v-high edge-code))
          (macrolet ((corner-vertex (i)
                       `(let* ((cx (aref corners (* 3 ,i)))
                               (cy (aref corners (+ 1 (* 3 ,i))))
                               (cz (aref corners (+ 2 (* 3 ,i))))
                               (c1 (ecase s1axis (0 cx) (1 cy)))
                               (c2 (ecase s2axis (1 cy) (2 cz)))
                               (sign1 (if (zerop c1) (- s1step) s1step))
                               (sign2 (if (zerop c2) (- s2step) s2step))
                               (local-u (aref uvs (* 2 ,i)))
                               (local-v (aref uvs (+ 1 (* 2 ,i)))))
                          (declare (type (integer 0 1) cx cy cz c1 c2
                                         local-u local-v)
                                   (type block-mesh-halo-step sign1 sign2))
                          (multiple-value-bind (ao sky-level block-level)
                              (block-mesh-corner-shade-and-light
                               solid indices sky block-light absent
                               base nstep sign1 sign2)
                            (declare (type single-float ao sky-level
                                           block-level))
                            (emit (coerce (+ x cx) 'single-float))
                            (emit (coerce (+ y cy) 'single-float))
                            (emit (coerce (+ z cz) 'single-float))
                            (emit (coerce (/ (+ 0.5 (* local-u 15)) 16)
                                          'single-float))
                            (emit (coerce (/ (+ 0.5 (* local-v 15)) 16)
                                          'single-float))
                            (emit (* variation ao))
                            (emit fnx) (emit fny) (emit fnz)
                            (emit sky-level)
                            (emit block-level)
                            (emit emission)
                            (emit tile)
                            (emit edge-code)))))
            (corner-vertex 0) (corner-vertex 1) (corner-vertex 2)
            (corner-vertex 0) (corner-vertex 2) (corner-vertex 3)))))
    fill))

(defun mesh-block-halo
    (mesher palette indices sky block-light width height depth
     origin-x origin-y origin-z)
  "Mesh the interior of one halo'd dense chunk.

INDICES, SKY and BLOCK-LIGHT are the halo columns whose dimensions are
WIDTH x HEIGHT x DEPTH (the chunk plus one cell on every side); the origin is
the world coordinate of the chunk's first interior cell."
  (declare (type (simple-array (unsigned-byte 16) (*)) indices)
           (type (simple-array (unsigned-byte 8) (*)) sky block-light)
           (type block-mesh-halo-extent width height depth)
           (type fixnum origin-x origin-y origin-z)
           (optimize (speed 3) (safety 1)))
  (when (eq :error (exposed-face-mesher-absent-neighbor-policy mesher))
    (when (find (length palette) indices)
      (error "Meshing reached absent terrain around chunk at (~D ~D ~D)."
             origin-x origin-y origin-z)))
  (let* ((tables (gather-block-mesh-kind-tables mesher palette))
         (faces (block-mesh-face-tables))
         (solid (block-mesh-kind-tables-solid tables))
         (xstep 1)
         (ystep width)
         (zstep (* width height))
         (interior (* (- width 2) (- height 2) (- depth 2)))
         (masks (make-array interior :element-type '(unsigned-byte 8)
                                     :initial-element 0))
         (face-count 0))
    (declare (type block-mesh-halo-step xstep ystep zstep)
             (type fixnum interior face-count))
    (flet ((solid-at (offset)
             (declare (type block-mesh-halo-index offset))
             (= 1 (sbit solid (aref indices offset)))))
      (declare (inline solid-at))
      ;; Pass one: exposed-face masks and the exact face count.
      (let ((site 0))
        (declare (type fixnum site))
        (loop for lz of-type block-mesh-halo-extent from 1 below (1- depth) do
          (loop for ly of-type block-mesh-halo-extent from 1 below (1- height) do
            (loop for lx of-type block-mesh-halo-extent from 1 below (1- width) do
              (let ((base (+ lx (* width (+ ly (* height lz))))))
                (declare (type block-mesh-halo-index base))
                (when (solid-at base)
                  (let ((mask 0))
                    (declare (type (unsigned-byte 8) mask))
                    ;; *BLOCK-FACES* order: -x +x -y +y -z +z.
                    (unless (solid-at (- base xstep)) (setf mask (logior mask 1)))
                    (unless (solid-at (+ base xstep)) (setf mask (logior mask 2)))
                    (unless (solid-at (- base ystep)) (setf mask (logior mask 4)))
                    (unless (solid-at (+ base ystep)) (setf mask (logior mask 8)))
                    (unless (solid-at (- base zstep)) (setf mask (logior mask 16)))
                    (unless (solid-at (+ base zstep)) (setf mask (logior mask 32)))
                    (incf face-count (logcount mask))
                    (setf (aref masks site) mask))))
              (incf site)))))
      ;; Pass two: emit every exposed face into an exactly sized array.
      (let ((vertices (make-array (* face-count +block-mesh-floats-per-face+)
                                  :element-type 'single-float))
            (fill 0)
            (site 0))
        (declare (type fixnum fill site))
        (loop for lz of-type block-mesh-halo-extent from 1 below (1- depth) do
          (loop for ly of-type block-mesh-halo-extent from 1 below (1- height) do
            (loop for lx of-type block-mesh-halo-extent from 1 below (1- width) do
              (let ((mask (aref masks site)))
                (unless (zerop mask)
                  (let ((base (+ lx (* width (+ ly (* height lz))))))
                    (declare (type block-mesh-halo-index base))
                    (dotimes (face-index 6)
                      (when (logbitp face-index mask)
                        (setf fill
                              (emit-block-mesh-face
                               tables faces vertices fill solid indices
                               sky block-light width height depth
                               base (aref indices base) face-index
                               (+ origin-x (1- lx))
                               (+ origin-y (1- ly))
                               (+ origin-z (1- lz)))))))))
              (incf site))))
        (assert (= fill (length vertices)))
        (make-instance 'block-mesh
                       :vertices vertices
                       :vertex-count (* face-count
                                        +block-mesh-vertices-per-face+)
                       :face-count face-count)))))

(defun mesh-block-snapshot-halo (mesher snapshot)
  (let* ((domain (block-mesh-snapshot-domain snapshot))
         (shape (voxel-space-chunk-shape (chunk-domain-space domain))))
    (multiple-value-bind (origin-x origin-y origin-z)
        (chunk-domain-world-components domain 0 0 0)
      (mesh-block-halo
       mesher
       (block-mesh-snapshot-palette snapshot)
       (block-mesh-snapshot-sample-indices snapshot)
       (block-mesh-snapshot-sky-samples snapshot)
       (block-mesh-snapshot-block-light-samples snapshot)
       (+ 2 (chunk-shape-width shape))
       (+ 2 (chunk-shape-height shape))
       (+ 2 (chunk-shape-depth shape))
       origin-x origin-y origin-z))))

(defmethod mesh-block-snapshot
    ((mesher exposed-face-mesher) (snapshot block-mesh-snapshot))
  (mesh-block-snapshot-halo mesher snapshot))

(defmethod mesh-block-chunk
    ((mesher exposed-face-mesher) (world block-world) (chunk block-chunk))
  "Mesh a live chunk by gathering it into a halo snapshot first.

Owner-side and worker-side meshing share one dense loop, so they are equal
by construction rather than by parallel maintenance."
  (mesh-block-snapshot-halo
   mesher (make-block-mesh-snapshot world chunk nil)))

(defmethod emit-block-face
    ((mesher exposed-face-mesher) (world block-world) vertices
     (block block-kind) (face block-face) x y z)
  "Compatibility entry point for tools emitting an individual world face.

The face is emitted as if BLOCK stood at (X Y Z) within the surrounding
world, with neighbours and light read from a fresh snapshot of its chunk."
  (multiple-value-bind (chunk-x chunk-y chunk-z local-x local-y local-z)
      (voxel-space-decompose-components (block-world-space world) x y z)
    (let ((chunk (world-chunk-at world chunk-x chunk-y chunk-z)))
      (unless chunk
        (error "Cannot emit a face from absent chunk (~D ~D ~D)."
               chunk-x chunk-y chunk-z))
      ;; BLOCK may be one the world has never held; interning it first keeps
      ;; the snapshot's frozen vocabulary able to name it.
      (let* ((palette-index
               (block-vocabulary-offset (block-world-vocabulary world) block))
             (snapshot (make-block-mesh-snapshot world chunk nil))
             (palette (block-mesh-snapshot-palette snapshot))
             (domain (block-mesh-snapshot-domain snapshot))
             (shape (voxel-space-chunk-shape (chunk-domain-space domain)))
             (width (+ 2 (chunk-shape-width shape)))
             (height (+ 2 (chunk-shape-height shape)))
             (depth (+ 2 (chunk-shape-depth shape)))
             (base (+ (1+ local-x)
                      (* width (+ (1+ local-y) (* height (1+ local-z))))))
             (tables (gather-block-mesh-kind-tables mesher palette))
             (face-index (position face *block-faces*))
             (scratch (make-array +block-mesh-floats-per-face+
                                  :element-type 'single-float)))
        (unless face-index
          (error "~S is not one of the block faces." face))
        (emit-block-mesh-face
         tables (block-mesh-face-tables) scratch 0
         (block-mesh-kind-tables-solid tables)
         (block-mesh-snapshot-sample-indices snapshot)
         (block-mesh-snapshot-sky-samples snapshot)
         (block-mesh-snapshot-block-light-samples snapshot)
         width height depth base palette-index face-index x y z)
        (loop for value across scratch do (vector-push value vertices))
        vertices))))

(defun make-block-mesh-snapshot (world chunk dependency-stamp)
  "Copy CHUNK and its one-cell halo into immutable worker-owned columns.

Every resident chunk's indices are already offsets under WORLD's vocabulary,
so the halo is gathered by plain slab copies from the up to 27 neighbours,
with no per-cell decomposition, translation, or block object lookup.  The
vocabulary's members are frozen into the snapshot by copying; their count is
the index written for absent halo samples."
  (let* ((domain (block-chunk-domain chunk))
         (halo-domain (make-block-mesh-halo-domain domain))
         (shape (voxel-space-chunk-shape (chunk-domain-space domain)))
         (width (chunk-shape-width shape))
         (height (chunk-shape-height shape))
         (depth (chunk-shape-depth shape))
         (sample-width (+ width 2))
         (sample-height (+ height 2))
         (sample-depth (+ depth 2))
         (vocabulary (block-world-vocabulary world))
         (palette (coerce (block-vocabulary-members vocabulary) 'simple-vector))
         (absent (length palette))
         (content-definition
           (fields:materialized-field-definition
            (block-chunk-content chunk) :block-content))
         (light-field (block-chunk-light-field chunk))
         (sky-definition
           (if light-field
               (fields:materialized-field-definition light-field :sky-light)
               (fields:field-definition-for :sky-light)))
         (block-light-definition
           (if light-field
               (fields:materialized-field-definition light-field :block-light)
               (fields:field-definition-for :block-light)))
         (halo-fields
           (make-block-mesh-halo-fields
            halo-domain
            :declarations `((sky-level . ,sky-definition)
                            (block-level . ,block-light-definition))))
         (neighborhood (make-block-mesh-neighborhood world chunk)))
    (declare (type (integer 1 4096) width height depth
                   sample-width sample-height sample-depth))
    (unless (< absent #x10000)
      (error "The block vocabulary is too large to leave an absent index."))
    (records:with-columnar-materialization-storage
        ((borrowed-domain extent row
                          (sample-indices content-index)
                          (sky-samples sky-level)
                          (block-light-samples block-level))
         halo-fields block-mesh-halo-fields)
      (declare (ignore row))
      (assert (eq borrowed-domain halo-domain))
      (assert (= extent (* sample-width sample-height sample-depth)))
      (check-type sample-indices (simple-array (unsigned-byte 16) (*)))
      (check-type sky-samples (simple-array (unsigned-byte 8) (*)))
      (check-type block-light-samples (simple-array (unsigned-byte 8) (*)))
      (fill sample-indices absent)
      (flet ((slab (dx dy dz)
               "Copy neighbour (DX DY DZ)'s cells that fall inside the halo."
               (let ((neighbour
                       (aref (block-mesh-neighborhood-chunks neighborhood)
                             (block-mesh-neighborhood-index dx dy dz))))
                 (when neighbour
                   (with-block-content-storage
                       (neighbour-domain neighbour-palette neighbour-indices)
                       neighbour
                     (declare (ignore neighbour-domain))
                     (unless (eq neighbour-palette
                                 (block-vocabulary-members vocabulary))
                       (error "Chunk ~S is not under world ~S's vocabulary."
                              neighbour world))
                     (check-type neighbour-indices
                                 (simple-array (unsigned-byte 16) (*)))
                     (let* ((field (block-chunk-light-field neighbour))
                            (sky (and field (chunk-light-field-sky-levels field)))
                            (block-light
                              (and field (chunk-light-field-block-levels field)))
                            ;; Which local range of the neighbour lands where.
                            (lx0 (if (= dx -1) (1- width) 0))
                            (lx1 (if (= dx 1) 0 (1- width)))
                            (ly0 (if (= dy -1) (1- height) 0))
                            (ly1 (if (= dy 1) 0 (1- height)))
                            (lz0 (if (= dz -1) (1- depth) 0))
                            (lz1 (if (= dz 1) 0 (1- depth)))
                            ;; Halo sample coordinate of local (lx0 ly0 lz0).
                            (sx0 (1+ (+ (* dx width) lx0)))
                            (sy0 (1+ (+ (* dy height) ly0)))
                            (sz0 (1+ (+ (* dz depth) lz0)))
                            (run (1+ (- lx1 lx0))))
                       (declare (type (or null (simple-array (unsigned-byte 8) (*)))
                                      sky block-light)
                                (type fixnum lx0 lx1 ly0 ly1 lz0 lz1 sx0 sy0 sz0
                                      run))
                       (loop for lz of-type fixnum from lz0 to lz1
                             for sz of-type fixnum from sz0 do
                         (loop for ly of-type fixnum from ly0 to ly1
                               for sy of-type fixnum from sy0 do
                           (let ((source (+ lx0 (* width (+ ly (* height lz)))))
                                 (target (+ sx0 (* sample-width
                                                   (+ sy (* sample-height sz))))))
                             (declare (type fixnum source target))
                             (replace sample-indices neighbour-indices
                                      :start1 target :end1 (+ target run)
                                      :start2 source)
                             (when sky
                               (replace sky-samples sky
                                        :start1 target :end1 (+ target run)
                                        :start2 source)
                               (replace block-light-samples block-light
                                        :start1 target :end1 (+ target run)
                                        :start2 source)))))))))))
        (loop for dz from -1 to 1 do
          (loop for dy from -1 to 1 do
            (loop for dx from -1 to 1 do
              (slab dx dy dz))))))
    (make-instance
     'block-mesh-snapshot
     :key (block-chunk-key chunk) :dependency-stamp dependency-stamp
     :domain domain
     :content-definition content-definition
     :palette palette
     :halo-fields halo-fields)))

(defmethod mesh-block-world
    ((mesher exposed-face-mesher) (world block-world))
  "Make a combined compatibility mesh from independently meshed chunks."
  (let ((vertices (make-array 0 :element-type 'single-float
                                :adjustable t :fill-pointer 0))
        (vertex-declaration nil)
        (vertex-count 0)
        (face-count 0))
    (dolist (chunk (resident-world-chunks world))
      (let ((mesh (mesh-block-chunk mesher world chunk)))
        (setf vertex-declaration
              (merge-block-mesh-vertex-declaration vertex-declaration mesh))
        (loop for component across (block-mesh-vertices mesh)
              do (vector-push-extend component vertices))
        (incf vertex-count (block-mesh-vertex-count mesh))
        (incf face-count (block-mesh-face-count mesh))))
    (make-instance 'block-mesh
                               :vertex-declaration
                               (or vertex-declaration
                                   (luv.arithmetic:value-declaration-for
                                    :block-mesh-vertices))
                               :vertices vertices
                               :vertex-count vertex-count
                               :face-count face-count)))
