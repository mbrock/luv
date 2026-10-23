(in-package #:luft.terrain)

;;; Streamed voxel light (#BQMYNU)
;;;
;;; A light value loses at least one level per step and starts at most at
;;; fifteen, so it never travels fifteen cells.  A chunk column is sixty-four
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
;;; exact identity of their inputs, so an edit re-solves the nine tiles around
;;; it and a residency change only the tiles whose neighbourhood changed.  A
;;; seam is consistent because both owners meeting there read the same tile.
;;; The owner thread decides which published owners must be relit before it
;;; schedules a cohort (STREAMING-LIGHT-DIRTY-OWNERS); no GPU is involved.

(defconstant +light-tile-reach+ luft:+maximum-voxel-light-level+
  "No voxel light travels this many cells from the cell that admitted it.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (> luft:+chunk-size+ luft:+maximum-voxel-light-level+)
    (error "A light tile needs chunks wider than the light's reach.")))

(defstruct (light-tile
             (:constructor %make-light-tile (key inputs field source-count))
             (:copier nil))
  "The exact light over chunk KEY's columns, and the inputs that justify it.

INPUTS is the list of (NEIGHBOUR TABLE ATTACHMENTS) for KEY's 3 by 3
neighbourhood: each neighbour's immutable material table, compared by
identity, and its emitting attachment semantics, compared by EQUAL.  FIELD is
NIL when nothing in reach emits, which is most of any world."
  (key 0 :type luft:chunk-key :read-only t)
  (inputs nil :type list :read-only t)
  (field nil :type (or null luft:voxel-light-field) :read-only t)
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
                        emission)))

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
  (loop for (support clearance u v emission) in signature
        nconc
        (handler-case
            (let ((seeds (realized-attachment-light-seeds
                          domain occupied-p
                          (attachment-canonical-wick support clearance u v)
                          emission)))
              (map 'list #'luft:make-voxel-light-source
                   (realized-light-seeds-sites seeds)
                   (realized-light-seeds-lights seeds)))
          (unrealizable-attachment-light-source () nil))))

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

(defun light-inputs-emit-p (inputs vocabulary)
  "Whether anything in INPUTS emits, so its tile may hold light."
  (loop for (nil table attachments) in inputs
        thereis (or attachments
                    (and table
                         (plusp (length (chunk-emitter-sources table vocabulary)))))))

;;; Solving

(defun light-tile-region (key)
  (let ((x (luft:chunk-origin-x key))
        (y (luft:chunk-origin-y key)))
    (list x y (+ x luft:+chunk-size+) (+ y luft:+chunk-size+))))

(zdefun (solve-light-tile :zone :luft/solve-light-tile :value key)
    (scene key inputs)
  "Solve KEY's exact tile from INPUTS over SCENE's captured materials."
  (let* ((domain (scene-domain scene))
         (vocabulary (scene-material-vocabulary scene))
         (materials (scene-material-cells scene))
         (region (light-tile-region key))
         (occupied-p (lambda (cell) (nth-value 1 (material-cell-at materials cell))))
         (sources nil))
    (destructuring-bind (x0 y0 x1 y1) region
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
    (%make-light-tile
     key inputs
     (when sources
       (luft:join-voxel-light-fields
        domain
        (list (luft:solve-voxel-light
               domain (material-cell-reader materials)
               (scene-authored-light-opacity-table scene)
               (coerce sources '(simple-array (unsigned-byte 64) (*)))
               :region region :hashed-p t))
        :region region))
     (length sources))))

(zdefun (realize-streaming-light-tiles :zone :luft/realize-light-tiles
                                       :value (length keys))
    (scene keys cache)
  "Return the tiles of KEYS over frozen SCENE, reusing CACHE's exact ones."
  (loop for key in keys
        for inputs = (chunk-light-inputs scene key)
        for cached = (and cache (gethash key cache))
        collect (if (and cached (light-inputs= inputs (light-tile-inputs cached)))
                    cached
                    (solve-light-tile scene key inputs))))

(defun light-tiles-generation (scene tiles revision)
  "One realized-light generation whose field joins TILES' fields."
  (make-realized-light-generation
   (scene-authored-light-provenance scene) revision
   (make-realized-light-seeds #() #())
   (luft:join-voxel-light-fields
    (scene-domain scene)
    (remove nil (mapcar #'light-tile-field tiles))
    :revision revision)))

;;; Publication

(defun streaming-light-dirty-owners (scene owner-keys)
  "OWNER-KEYS whose published light may differ from their current inputs.

A tile is dirty when its inputs changed and it either held light or now has
an emitter in reach; a tile that was dark and has nothing to emit stays dark.
Every owner whose 3 by 3 neighbourhood holds a dirty tile is relit."
  (let ((tiles (streaming-scene-light-tiles scene))
        (vocabulary (scene-material-vocabulary scene))
        (dirty (make-hash-table :test #'eql)))
    (dolist (key (streaming-scene-dependency-guard-keys scene owner-keys))
      (let ((installed (gethash key tiles))
            (inputs (chunk-light-inputs scene key)))
        (when (if installed
                  (and (not (light-inputs= inputs (light-tile-inputs installed)))
                       (or (light-tile-field installed)
                           (light-inputs-emit-p inputs vocabulary)))
                  (light-inputs-emit-p inputs vocabulary))
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
