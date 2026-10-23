(in-package #:luft.terrain)

;;; Streamed voxel light (#BQMYNU)
;;;
;;; Diffuse light loses at least one level per step and starts at most at
;;; fifteen, so it never travels fifteen cells. Direct vertical sky is an
;;; implicit column sweep; residency always includes the full height.
;;; A chunk column is sixty-four
;;; cells wide, so the exact light over one column depends only on the
;;; emitters, attachments, and opacities of its own column and its eight
;;; neighbours.  That bound makes streamed light a pure function of resident
;;; values, solved one column at a time:
;;;
;;;   inputs(K)  = K's immutable material table and its emitting attachments
;;;   tile(T)    = LUFT's one solver over guard(T)'s inputs, pruned to T's
;;;                columns by SOLVE-VOXEL-LIGHT's REGION and kept only there
;;;   light(O)   = the join of the tiles over guard(O), since an owner's
;;;                stars sample cells one step past its seams
;;;
;;; Tiles are solved on the mesh worker beside the meshes and cached by the
;;; exact identity of their inputs. Edits invalidate only page columns within
;;; fourteen cells of changed evidence; residency changes solve fresh tiles. A
;;; seam is consistent because both owners meeting there read the same tile.
;;; The owner thread decides which published owners must be relit before it
;;; schedules a cohort (STREAMING-LIGHT-DIRTY-OWNERS); no GPU is involved.

(defconstant +light-tile-reach+ luft:+maximum-voxel-light-level+
  "No voxel light travels this many cells from the cell that admitted it.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (> luft:+chunk-size+ luft:+maximum-voxel-light-level+)
    (error "A light tile needs chunks wider than the light's reach.")))

(defstruct (light-tile
             (:constructor %make-light-tile (key inputs field source-count sky))
             (:copier nil))
  "The exact light over chunk KEY's columns, and the inputs that justify it.

INPUTS is the list of (NEIGHBOUR TABLE ATTACHMENTS) for KEY's 3 by 3
neighbourhood: each neighbour's immutable material table, compared by
identity, and its emitting attachment semantics, compared by EQUAL.  FIELD is
NIL when nothing in reach emits, which is most of any world."
  (key 0 :type luft:chunk-key :read-only t)
  (inputs nil :type list :read-only t)
  (field nil :type (or null luft:voxel-light-field) :read-only t)
  (sky nil :type (or null luft:voxel-light-field) :read-only t)
  (source-count 0 :type (integer 0 *) :read-only t))

(defun scene-regional-light-p (scene)
  "Whether SCENE publishes streamed regional light rather than one field."
  (and (typep scene 'streaming-scene)
       (not (null (streaming-scene-light-tiles scene)))))

;;; Emitters

(defvar *chunk-emitter-sources*
  (make-hash-table :test #'eq :weakness :key :synchronized t)
  "Packed voxel-light sources of each immutable chunk material table.")

(defun placement-emissions (vocabulary)
  "A vector of each placement offset's packed RGB4 emission."
  (map 'simple-vector
       (lambda (placement)
         (material-kind-packed-light-emission
          (material-placement-kind placement)))
       (domains:identity-vocabulary-members vocabulary)))

(defun chunk-emitter-sources (table vocabulary)
  "TABLE's emitting cells as packed voxel-light sources, memoized by TABLE.

Material tables are replaced, never mutated, so their identity names their
emitters; a worker may warm this memo when it materializes a chunk."
  (or (gethash table *chunk-emitter-sources*)
      (setf (gethash table *chunk-emitter-sources*)
            (let ((emissions (placement-emissions vocabulary))
                  (sources nil))
              (when (some #'plusp emissions)
                (maphash (lambda (cell offset)
                           (let ((emission (aref emissions offset)))
                             (unless (zerop emission)
                               (push (luft:make-voxel-light-source cell emission)
                                     sources))))
                         table))
              (coerce (sort sources #'<)
                      '(simple-array (unsigned-byte 64) (*)))))))

(defun attachment-light-signature (attachments key)
  "Exact semantics of KEY's emitting attachments, the part light reads."
  (loop for attachment across attachments
        for support = (surface-attachment-support-cell attachment)
        for emission = (surface-attachment-light-emission attachment)
        when (and (= key (luft:site-chunk-key support)) (plusp emission))
          collect (list support
                        (surface-attachment-clearance-cell attachment)
                        (surface-attachment-chart-u attachment)
                        (surface-attachment-chart-v attachment)
                        emission
                        (material-kind-attachment-form
                         (surface-attachment-material attachment)))))

(defun attachment-canonical-wick (support clearance u v)
  "The wick of an attachment drawn flat on its face at chart point U, V.

The star mesher does not realize attachment frames yet, so light uses the
authored face: the support cell's face centre, moved half a cell along the
face's tangents by U and V, and the canonical half-cell wick off it."
  (let* ((normal (list (- (luft:site-x clearance) (luft:site-x support))
                       (- (luft:site-y clearance) (luft:site-y support))
                       (- (luft:site-z clearance) (luft:site-z support))))
         (axis (position-if-not #'zerop normal))
         (tangents (remove axis '(0 1 2)))
         (origin (list (+ (luft:site-x support) 0.5 (* 0.5 (first normal)))
                       (+ (luft:site-y support) 0.5 (* 0.5 (second normal)))
                       (+ (luft:site-z support) 0.5 (* 0.5 (third normal))))))
    (incf (nth (first tangents) origin) (* 0.5 u))
    (incf (nth (second tangents) origin) (* 0.5 v))
    (realized-attachment-wick-point origin normal 1.0f0)))

(defun attachment-signature-sources (domain signature occupied-p)
  "Discretize each attachment of SIGNATURE into packed voxel-light sources."
  (loop for (support clearance u v emission form) in signature
        nconc
        ;; A realized lantern occupies its clearance cell, standing or
        ;; hanging. It seeds that air cell, never the solid support or an
        ;; additional unoccluded renderer light.
        (if (eq form :lantern)
            (unless (funcall occupied-p clearance)
              (list (luft:make-voxel-light-source clearance emission)))
            (handler-case
                (let ((seeds (realized-attachment-light-seeds
                              domain occupied-p
                              (attachment-canonical-wick support clearance u v)
                              emission)))
                  (map 'list #'luft:make-voxel-light-source
                       (realized-light-seeds-sites seeds)
                       (realized-light-seeds-lights seeds)))
              (unrealizable-attachment-light-source () nil)))))

;;; Inputs

(defun streaming-scene-chunk-material-table (scene key)
  "KEY's immutable material table in SCENE, or NIL when it has none."
  (if (and (typep scene 'streaming-scene) (streaming-scene-source scene))
      (let ((resident (gethash key (streaming-scene-store scene))))
        (and resident (resident-cell-chunk-material-cells resident)))
      (let ((materials (scene-material-cells scene)))
        (and (typep materials 'material-store)
             (gethash key (material-store-chunks materials))))))

(defun chunk-light-inputs (scene key)
  "The exact inputs of KEY's tile: its neighbourhood's tables and attachments."
  (loop for neighbour in (streaming-scene-dependency-guard-keys scene (list key))
        collect (list neighbour
                      (streaming-scene-chunk-material-table scene neighbour)
                      (attachment-light-signature (scene-attachments scene) neighbour))))

(defun light-inputs= (left right)
  (and (= (length left) (length right))
       (every (lambda (a b)
                (and (eql (first a) (first b))
                     (eq (second a) (second b))
                     (equal (third a) (third b))))
              left right)))

(defun changed-light-columns (old new memo)
  "Bounds of changed cells, memoized for this immutable capture only."
  (let ((pair (cons old new)))
    (multiple-value-bind (bounds present-p) (gethash pair memo)
      (if present-p bounds
          (setf (gethash pair memo)
                (let ((x0 most-positive-fixnum) (y0 most-positive-fixnum)
                      (x1 0) (y1 0))
                  (labels ((changed (cell)
                             (setf x0 (min x0 (luft:site-x cell))
                                   y0 (min y0 (luft:site-y cell))
                                   x1 (max x1 (1+ (luft:site-x cell)))
                                   y1 (max y1 (1+ (luft:site-y cell))))))
                    (maphash (lambda (cell value)
                               (multiple-value-bind (other found) (gethash cell new)
                                 (unless (and found (= value other)) (changed cell)))) old)
                    (maphash (lambda (cell value)
                               (declare (ignore value))
                               (unless (nth-value 1 (gethash cell old)) (changed cell))) new))
                  (when (< x0 x1) (list x0 y0 x1 y1))))))))

(defun light-edit-region (domain key old new memo)
  "Page-aligned influence bounds, NIL if disjoint, or :FULL for residency.

Sky changes can reach the bottom of their column, so the invalidation spans
the full height, but never unrelated horizontal columns. Changed attachments
include every possible bracket seed, even ones currently blocked by solids."
  (unless (equal (mapcar #'first old) (mapcar #'first new))
    (return-from light-edit-region :full))
  (let ((bounds nil))
    (flet ((include (box)
             (when box
               (setf bounds (if bounds
                                (list (min (first bounds) (first box))
                                      (min (second bounds) (second box))
                                      (max (third bounds) (third box))
                                      (max (fourth bounds) (fourth box))) box)))))
      (loop for (nil before attachments-before) in old
            for (nil after attachments-after) in new do
        (unless (eq before after)
          (unless (and before after) (return-from light-edit-region :full))
          (include (changed-light-columns before after memo)))
        (unless (equal attachments-before attachments-after)
          (dolist (source (attachment-signature-sources
                           domain (append attachments-before attachments-after)
                           (constantly nil)))
            (let ((cell (ldb (byte 48 0) source)))
              (include (list (luft:site-x cell) (luft:site-y cell)
                             (1+ (luft:site-x cell)) (1+ (luft:site-y cell)))))))))
    (when bounds
      (destructuring-bind (tx0 ty0 tx1 ty1) (light-tile-region key)
        (destructuring-bind (x0 y0 x1 y1) bounds
          (let ((box (list (max tx0 (* 8 (floor (- x0 14) 8)))
                           (max ty0 (* 8 (floor (- y0 14) 8)))
                           (min tx1 (* 8 (ceiling (+ x1 14) 8)))
                           (min ty1 (* 8 (ceiling (+ y1 14) 8))))))
            (when (and (< (first box) (third box)) (< (second box) (fourth box)))
              box)))))))

;;; Solving

(defun light-tile-region (key)
  (let ((x (luft:chunk-origin-x key))
        (y (luft:chunk-origin-y key)))
    (list x y (+ x luft:+chunk-size+) (+ y luft:+chunk-size+))))

(defvar *chunk-sky-columns*
  (make-hash-table :test #'eq :weakness :key :synchronized t))

(defun chunk-sky-columns (table opacity)
  "Direct sky below each column's highest attenuator; NIL columns are open.

These immutable byte columns are shared by every tile reading TABLE. Direct
vertical sky loses only opacity; light which leaves a column loses one per
step as well. Unknown residency is handled by the tile, never as open sky."
  (or (gethash table *chunk-sky-columns*)
      (setf (gethash table *chunk-sky-columns*)
            (let ((columns (make-array 4096 :initial-element nil)))
              (maphash
               (lambda (cell material)
                 (when (plusp (aref opacity material))
                   (let* ((index (+ (logand (luft:site-x cell) 63)
                                    (* 64 (logand (luft:site-y cell) 63))))
                          (column (or (aref columns index)
                                      (setf (aref columns index)
                                            (make-array luft:+top-z+
                                             :element-type '(unsigned-byte 8)
                                             :initial-element 0)))))
                     (setf (aref column (luft:site-z cell))
                           (aref opacity material)))))
               table)
              (dotimes (index 4096)
                (let* ((column (aref columns index))
                       (top (and column (position-if #'plusp column :from-end t))))
                  (when top
                    (let ((direct (make-array (1+ top)
                                   :element-type '(unsigned-byte 8)))
                          (level 15))
                      (loop for z from top downto 0 do
                        (setf level (max 0 (- level (aref column z)))
                              (aref direct z) level))
                      (setf (aref columns index) direct)))))
              columns))))

(defvar *chunk-sky-interfaces*
  (make-hash-table :test #'eq :weakness :key :synchronized t))

(defun chunk-sky-interfaces (table opacity domain)
  "Chunk-local diffuse seeds and shadowed seam cells, prepared with TABLE.

Interior interfaces cannot change when a neighbour arrives. Only the small
seam frontier must read the regional guard on the mesh worker."
  (or (gethash table *chunk-sky-interfaces*)
      (setf (gethash table *chunk-sky-interfaces*)
            (let* ((columns (chunk-sky-columns table opacity))
                   (first (loop for cell being the hash-keys of table return cell))
                   (x0 (if first (luft:chunk-origin-x (luft:site-chunk-key first)) 0))
                   (y0 (if first (luft:chunk-origin-y (luft:site-chunk-key first)) 0))
                   (sources nil) (fringe nil))
              (labels ((direct (x y z)
                         (if (and (<= 0 x 63) (<= 0 y 63) (<= 0 z) (< z luft:+top-z+))
                             (let ((column (aref columns (+ x (* 64 y)))))
                               (if (or (null column) (>= z (length column)))
                                   15 (aref column z)))
                             0)))
                (dotimes (y 64)
                  (dotimes (x 64)
                    (let ((column (aref columns (+ x (* 64 y)))))
                      (when column
                        (dotimes (z (length column))
                          (let* ((cell (luft:make-site domain (+ x0 x) (+ y0 y) z luft:+cell-extent+ 1))
                                 (level (aref column z))
                                 (loss (1+ (multiple-value-bind (index present-p) (gethash cell table)
                                             (if present-p (aref opacity index) 0)))))
                            (when (and (< loss 15) (< level 14))
                              (when (or (= x 0) (= y 0) (= x 63) (= y 63))
                                (push cell fringe))
                              (let ((candidate
                                      (luft:voxel-light-transfer
                                       (max (direct (1- x) y z) (direct (1+ x) y z)
                                            (direct x (1- y) z) (direct x (1+ y) z)
                                            (direct x y (1- z)) (direct x y (1+ z)))
                                       (1- loss))))
                                (when (> candidate level)
                                  (push (luft:make-voxel-light-source cell candidate) sources)))))))))))
              (cons sources fringe)))))

(zdefun (solve-sky-tile :zone :luft/solve-sky-tile :value key)
    (scene key inputs &optional solve-region)
  "Exact sky over KEY, with closed unknown columns and open domain top.

Only direct/diffuse interfaces seed the ordinary bounded frontier. Open
atmosphere is an implicit column reading, not millions of queued cells."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((domain (scene-domain scene))
         (opacity (scene-authored-light-opacity-table scene))
         (region (light-tile-region key))
         (x0 (max 0 (- (first region) 14)))
         (y0 (max 0 (- (second region) 14)))
         (x1 (min (luft:world-domain-x-limit domain) (+ (third region) 14)))
         (y1 (min (luft:world-domain-y-limit domain) (+ (fourth region) 14)))
         (width (- x1 x0))
         (columns (make-array (* width (- y1 y0)) :initial-element :unknown))
         (reader (material-cell-reader (scene-material-cells scene)))
         (closed-opacity (concatenate '(vector (unsigned-byte 8)) opacity #(15)))
         (interfaces nil)
         (sources (make-array 0 :element-type '(unsigned-byte 64)
                               :adjustable t :fill-pointer 0)))
    (declare (type fixnum x0 y0 x1 y1 width))
    (loop for (neighbour table) in inputs when table do
      (let ((sky (chunk-sky-columns table opacity))
            (cx (luft:chunk-origin-x neighbour))
            (cy (luft:chunk-origin-y neighbour)))
        (push (chunk-sky-interfaces table opacity domain) interfaces)
        (loop for y from (max y0 cy) below (min y1 (+ cy 64)) do
          (loop for x from (max x0 cx) below (min x1 (+ cx 64)) do
            (setf (aref columns (+ (- x x0) (* width (- y y0))))
                  (aref sky (+ (- x cx) (* 64 (- y cy)))))))))
    (labels ((known-p (x y)
               (and (<= x0 x) (< x x1) (<= y0 y) (< y y1)
                    (not (eq :unknown
                             (aref columns (+ (- x x0) (* width (- y y0))))))))
             (material (cell)
               (if (known-p (luft:site-x cell) (luft:site-y cell))
                   (funcall reader cell)
                   (values (length opacity) t)))
             (direct (x y z)
               (declare (type fixnum x y z))
               (if (and (<= x0 x) (< x x1) (<= y0 y) (< y y1)
                        (<= 0 z) (< z luft:+top-z+))
                   (let ((column (aref columns (+ (- x x0) (* width (- y y0))))))
                     (cond ((eq column :unknown) 0)
                           ((or (null column)
                                (>= z (length (the (simple-array (unsigned-byte 8) (*)) column)))) 15)
                           (t (aref (the (simple-array (unsigned-byte 8) (*)) column) z))))
                   0)))
      (dolist (interface interfaces)
        (dolist (source (car interface))
          (let ((cell (ldb (byte 48 0) source)))
            (when (known-p (luft:site-x cell) (luft:site-y cell))
              (vector-push-extend source sources))))
        (dolist (cell (cdr interface))
          (let ((x (luft:site-x cell)) (y (luft:site-y cell)) (z (luft:site-z cell)))
            (when (known-p x y)
              (let* ((level (direct x y z))
                     (incoming (max (direct (1- x) y z) (direct (1+ x) y z)
                                    (direct x (1- y) z) (direct x (1+ y) z)
                                    (direct x y (1- z)) (direct x y (1+ z)))))
                (when (> incoming (1+ level))
                  (let* ((opacity (multiple-value-bind (index present-p) (material cell)
                                    (if present-p (aref closed-opacity index) 0)))
                         (candidate (luft:voxel-light-transfer incoming opacity)))
                    (when (> candidate level)
                      (vector-push-extend
                       (luft:make-voxel-light-source cell candidate) sources)))))))))
      (let* ((solved (luft:solve-voxel-light
                      domain #'material closed-opacity sources
                      :field-name :luft-sky
                      :region (or solve-region region) :hashed-p t :baseline #'direct))
             (field (luft:join-voxel-light-fields domain (list solved)
                                                 :region (or solve-region region))))
        (setf (luft:voxel-light-field-baseline field) #'direct
              (luft::voxel-light-field-baseline-column field)
              (lambda (x y)
                (if (and (<= x0 x) (< x x1) (<= y0 y) (< y y1))
                    (aref columns (+ (- x x0) (* width (- y y0))))
                    :unknown)))
        field))))

(zdefun (solve-light-tile :zone :luft/solve-light-tile :value key)
    (scene key inputs &optional cached solve-region)
  "Solve KEY's exact tile from INPUTS over SCENE's captured materials."
  (let* ((domain (scene-domain scene))
         (vocabulary (scene-material-vocabulary scene))
         (materials (scene-material-cells scene))
         (region (or solve-region (light-tile-region key)))
         (occupied-p (lambda (cell) (nth-value 1 (material-cell-at materials cell))))
         (sources nil))
    ;; Retain the tile's complete source census even for a small repair;
    ;; the realization itself prunes seeds which cannot reach REGION.
    (destructuring-bind (x0 y0 x1 y1) (light-tile-region key)
      (flet ((consider (source)
               ;; Sources too far or too dim to reach the tile seed nothing.
               (let* ((cell (ldb (byte 48 0) source))
                      (light (ldb (byte 12 48) source))
                      (x (luft:site-x cell))
                      (y (luft:site-y cell))
                      (distance (+ (max 0 (- x0 x) (- x (1- x1)))
                                   (max 0 (- y0 y) (- y (1- y1))))))
                 (when (> (max (luft:voxel-light-red light)
                               (luft:voxel-light-green light)
                               (luft:voxel-light-blue light))
                          distance)
                   (push source sources)))))
        (loop for (nil table attachments) in inputs do
          (when table
            (map nil #'consider (chunk-emitter-sources table vocabulary)))
          (mapc #'consider
                (attachment-signature-sources domain attachments occupied-p)))))
    (let ((field
            (when sources
              (luft:join-voxel-light-fields
               domain
               (list (luft:solve-voxel-light
                      domain (material-cell-reader materials)
                      (scene-authored-light-opacity-table scene)
                      (coerce sources '(simple-array (unsigned-byte 64) (*)))
                      :region region :hashed-p t))
               :region region)))
          (sky (solve-sky-tile scene key inputs region)))
      (%make-light-tile
       key inputs
       (if (and cached (light-tile-field cached))
           (luft:replace-voxel-light-region domain (light-tile-field cached) field region)
           field)
       (length sources)
       (if cached
           (luft:replace-voxel-light-region domain (light-tile-sky cached) sky region)
           sky)))))

(zdefun (realize-streaming-light-tiles :zone :luft/realize-light-tiles
                                       :value (length keys))
    (scene keys cache)
  "Return the tiles of KEYS over frozen SCENE, reusing CACHE's exact ones."
  (let ((changes (make-hash-table :test #'equal)))
    (loop for key in keys
          for inputs = (chunk-light-inputs scene key)
          for cached = (and cache (gethash key cache))
          collect
          (cond ((and cached (light-inputs= inputs (light-tile-inputs cached))) cached)
                ((null cached) (solve-light-tile scene key inputs))
                (t (let ((region (light-edit-region (scene-domain scene) key
                                                   (light-tile-inputs cached) inputs changes)))
                     (cond ((eq region :full) (solve-light-tile scene key inputs))
                           (region (solve-light-tile scene key inputs cached region))
                           (t (%make-light-tile key inputs (light-tile-field cached)
                                                (light-tile-source-count cached)
                                                (light-tile-sky cached))))))))))

(defun light-tiles-generation (scene tiles revision)
  "One realized-light generation whose field joins TILES' fields."
  (declare (optimize (speed 3) (safety 1)))
  (let* ((domain (scene-domain scene))
         (x0 (reduce #'min tiles :key (lambda (tile) (luft:chunk-key-x (light-tile-key tile)))))
         (y0 (reduce #'min tiles :key (lambda (tile) (luft:chunk-key-y (light-tile-key tile)))))
         (width (1+ (- (reduce #'max tiles :key (lambda (tile) (luft:chunk-key-x (light-tile-key tile)))) x0)))
         (height (1+ (- (reduce #'max tiles :key (lambda (tile) (luft:chunk-key-y (light-tile-key tile)))) y0)))
         (baselines (make-array (* width height) :initial-element nil))
         (columns (make-array (* width height) :initial-element nil))
         (sky (luft:join-voxel-light-fields domain (mapcar #'light-tile-sky tiles)
                                          :revision revision))
         (field (luft:join-voxel-light-fields
                 domain (remove nil (mapcar #'light-tile-field tiles))
                 :revision revision)))
    (declare (type fixnum x0 y0 width height))
    (dolist (tile tiles)
      (setf (aref baselines (+ (- (luft:chunk-key-x (light-tile-key tile)) x0)
                              (* width (- (luft:chunk-key-y (light-tile-key tile)) y0))))
            (luft:voxel-light-field-baseline (light-tile-sky tile))))
    (dolist (tile tiles)
      (setf (aref columns (+ (- (luft:chunk-key-x (light-tile-key tile)) x0)
                            (* width (- (luft:chunk-key-y (light-tile-key tile)) y0))))
            (luft::voxel-light-field-baseline-column (light-tile-sky tile))))
    (setf (luft:voxel-light-field-baseline sky)
          (lambda (x y z)
            (declare (type fixnum x y z))
            (let ((cx (- (ash x -6) x0)) (cy (- (ash y -6) y0)))
              (if (and (<= 0 cx) (< cx width) (<= 0 cy) (< cy height))
                  (let ((baseline (aref baselines (+ cx (* width cy)))))
                    (if baseline (funcall baseline x y z) 0))
                  0)))
          (luft::voxel-light-field-baseline-column sky)
          (lambda (x y)
            (let ((cx (- (ash x -6) x0)) (cy (- (ash y -6) y0)))
              (if (and (<= 0 cx) (< cx width) (<= 0 cy) (< cy height))
                  (let ((column (aref columns (+ cx (* width cy)))))
                    (if column (funcall column x y) :unknown))
                  :unknown)))
          (luft:voxel-light-field-skylight field) sky)
    (make-realized-light-generation
     (scene-authored-light-provenance scene) revision
     (make-realized-light-seeds #() #()) field)))

;;; Publication

(defun streaming-light-dirty-owners (scene owner-keys)
  "OWNER-KEYS whose published light may differ from their current inputs.

A tile is dirty whenever its inputs change: roofs and unknown boundaries
matter even when nothing emits. Every owner whose 3 by 3 neighbourhood holds
a dirty tile is relit, using its unchanged cached geometry."
  (let ((tiles (streaming-scene-light-tiles scene))
        (dirty (make-hash-table :test #'eql)))
    (dolist (key (streaming-scene-dependency-guard-keys scene owner-keys))
      (let ((installed (gethash key tiles))
            (inputs (chunk-light-inputs scene key)))
        (when (or (null installed)
                  (not (light-inputs= inputs (light-tile-inputs installed))))
          (setf (gethash key dirty) t))))
    (remove-if-not
     (lambda (owner)
       (some (lambda (key) (gethash key dirty))
             (streaming-scene-dependency-guard-keys scene (list owner))))
     owner-keys)))

(defun install-streaming-light-tiles (scene tiles)
  "Retain published TILES and forget tiles no published owner can read."
  (let ((installed (streaming-scene-light-tiles scene)))
    (when installed
      (dolist (tile tiles)
        (setf (gethash (light-tile-key tile) installed) tile))
      (let ((keep (make-hash-table :test #'eql)))
        (dolist (key (streaming-scene-dependency-guard-keys
                      scene
                      (streaming-scene-canonical-owner-closure
                       scene
                       (loop for key being the hash-keys of
                             (streaming-scene-loaded scene)
                             collect key))))
          (setf (gethash key keep) t))
        (loop for key in (loop for key being the hash-keys of installed collect key)
              unless (gethash key keep)
                do (remhash key installed))))
    scene))
