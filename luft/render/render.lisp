;;; The atelier: a small solid world, its surface chain, and the GPU objects
;;; that draw it.
;;;
;;; Nothing here meshes.  The world is a 3-chain of solid cells; REFRESH-SCENE
;;; takes its boundary and orders the resulting face sites by chunk.  The
;;; renderer uploads that array and one frame block, then draws a few vertices
;;; per site, each pulling its own site.

(in-package #:luft.render)

;;; ------------------------------------------------------------------------
;;; Scenes

(defconstant +chunk-bits+ 3
  "Sites are ordered by 8-cell chunk so nearby faces stay close together.")

(defconstant +chunk-size+ (ash 1 +chunk-bits+))

(defconstant +surface-chunk-capacity+
  (* 3 +chunk-size+ +chunk-size+ +chunk-size+)
  "The largest number of face geometries anchored in one site chunk.

Each of the three face extents has 8 cubed possible anchors.  A surface of a
binary solid has at most one polarity at each geometry, so 3*8^3 is an exact
page bound rather than a guessed reserve.  #3QNZC1")

(defconstant +vertical-chunk-count+
  (ceiling luft:+vertical-cell-rows+ +chunk-size+))

(defstruct (surface-chunk (:constructor make-surface-chunk (chain)))
  "One chunk-local summand of a scene's surface chain."
  chain
  page
  (count 0 :type fixnum))

(defstruct (scene-change
             (:constructor make-scene-change
                 (revision chunks cell-words slot-words)))
  "The dense ranges changed by one published chain edit."
  (revision 0 :type fixnum :read-only t)
  (chunks #() :type vector :read-only t)
  (cell-words #() :type vector :read-only t)
  (slot-words #() :type vector :read-only t))

(defparameter *scene-change-history-limit* 128
  "How many incremental scene publications remain available to renderers.")

;;; ------------------------------------------------------------------------
;;; Worlds: a solid, and what every cell of it is cut from
;;;
;;; A LUFT chain says where the world is solid and nothing about what the
;;; solid is.  That was enough while every style shaded by face direction
;;; alone, and it stops being enough the moment a bridge wants stone piers
;;; under a timber deck.  A world is therefore a chain together with one
;;; stock slot per cell -- an index into a short palette, sixteen materials
;;; at most, because sixteen is what the packed site has room to carry.
;;; #PWMCOL
;;;
;;; Building code does not pass the material about.  It binds *STOCK* and
;;; fills, the way a shop works from one board at a time:
;;;
;;;   (with-stock (:limestone)
;;;     (fill-box world 26 27 44 45 0 8))

(defclass world ()
  ((domain
    :initarg :domain
    :reader world-domain)
   (solid
    :initarg :solid
    :reader world-solid
    :documentation "The solid world: a 3-chain of positive cells.")
   (stocks
    :initform (make-array 1 :adjustable t :fill-pointer 1
                            :initial-element :turf)
    :accessor world-stocks
    :documentation "The material of each slot, slot zero first.")
   (slots
    :initarg :slots
    :reader world-slots
    :documentation "One stock slot per cell, indexed as the cell bits are."))
  (:documentation "A solid world and the stock every cell of it is cut from."))

(defun make-world (&key (horizontal-bits 6)
                        (domain (luft:make-world-domain
                                 :horizontal-bits horizontal-bits)))
  "An empty world over DOMAIN, every cell of it slot zero."
  (make-instance 'world
                 :domain domain
                 :solid (luft:make-solid-chain domain)
                 :slots (make-array (luft:chain-cell-bit-count domain)
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)))

(defparameter *stock* :turf
  "The material FILL-BOX and its kin stamp on the cells they fill.")

(defmacro with-stock ((material) &body body)
  "Evaluate BODY with MATERIAL as the stock that filling stamps."
  `(let ((*stock* ,material)) ,@body))

(defun world-stock-slot (world material)
  "The slot MATERIAL occupies in WORLD's palette, adding it if it is new."
  (let ((stocks (world-stocks world)))
    (or (position material stocks)
        (progn
          (find-material material)
          (when (<= shaders:+stock-slots+ (fill-pointer stocks))
            (error "A world holds ~D stocks; ~S would be the ~:*~R."
                   shaders:+stock-slots+ material))
          (vector-push-extend material stocks)
          (1- (fill-pointer stocks))))))

(defun world-cell-p (world x y z)
  "Whether WORLD is solid at X,Y,Z."
  (luft:solid-cell-p (world-solid world) x y z))

(defun world-vertical-p (z)
  "Whether Z names a cell row a world can hold: the world has a floor and
a ceiling, and building code that runs off either end is building nothing
rather than making an error."
  (and (integerp z) (<= 0 z (- luft:+vertical-cell-rows+ 2))))

(defun (setf world-cell-p) (state world x y z)
  "Make WORLD solid or empty at X,Y,Z, stamping *STOCK* where it fills."
  (when (world-vertical-p z)
    (setf (luft:solid-cell-p (world-solid world) x y z) state)
    (when state
      (setf (aref (world-slots world)
                  (luft:cell-bit-index (world-domain world) x y z))
            (world-stock-slot world *stock*))))
  state)

(defun paint-cell (world x y z &optional (material *stock*))
  "Give the cell at X,Y,Z of WORLD the stock MATERIAL, solid or not."
  (when (world-vertical-p z)
    (setf (aref (world-slots world)
                (luft:cell-bit-index (world-domain world) x y z))
          (world-stock-slot world material)))
  material)

(defclass scene ()
  ((domain
    :initarg :domain
    :reader scene-domain)
   (solid
    :initarg :solid
    :reader scene-solid
    :documentation "The solid world: a 3-chain of positive cells.")
   (surface
    :initform nil
    :accessor scene-surface
    :documentation "The boundary of SOLID: exposed signed face sites.")
   (surface-chunks
    :initform nil
    :accessor scene-surface-chunks
    :documentation "The surface partitioned into stable chunk-local ledgers.")
   (site-pages
    :initform nil
    :accessor scene-site-pages
    :documentation "Packed u64 face pages addressed by SURFACE-CHUNK-PAGE.")
   (page-count
    :initform 0
    :accessor scene-page-count
    :documentation "How many stable face pages have been assigned.")
   (page-capacity
    :initform 0
    :accessor scene-page-capacity
    :documentation "How many pages SITE-PAGES currently has room for.")
   (cell-bits
    :initform nil
    :accessor scene-cell-bits
    :documentation "The solid chain as dense (unsigned-byte 32) cell bits.")
   (slots
    :initarg :slots
    :initform nil
    :accessor scene-slots
    :documentation "One stock slot per cell, or NIL for a single-stock scene.")
   (stocks
    :initarg :stocks
    :initform nil
    :accessor scene-stocks
    :documentation "The material of each stock slot, or NIL for *MATERIAL*.")
   (slot-words
    :initform nil
    :accessor scene-slot-words
    :documentation "SLOTS packed eight nibbles to a word, as the GPU reads
them: the same dense cell order as the occupancy bits.")
   (revision
    :initform 0
    :accessor scene-revision
    :documentation "Monotonic publication number of the derived GPU products.")
   (changes
    :initform nil
    :accessor scene-changes
    :documentation "Newest-first bounded history of incremental publications."))
  (:documentation "A solid world together with its drawable surface products."))

(defun make-scene (domain &key (solid (luft:make-solid-chain domain))
                               slots stocks)
  "Make a scene over DOMAIN and refresh its surface products once."
  (refresh-scene (make-instance 'scene :domain domain :solid solid
                                       :slots slots :stocks stocks)))

(defun world-scene (world)
  "The drawable scene of WORLD, carrying its stock slots and palette."
  (make-scene (world-domain world)
              :solid (world-solid world)
              :slots (world-slots world)
              :stocks (copy-seq (world-stocks world))))

(defun horizontal-chunk-count (period)
  (ceiling period +chunk-size+))

(defun domain-surface-chunk-count (domain)
  (* (horizontal-chunk-count (luft:world-domain-x-period domain))
     (horizontal-chunk-count (luft:world-domain-y-period domain))
     +vertical-chunk-count+))

(defun site-chunk-index (domain site)
  "The dense chunk index of SITE's canonical anchor in DOMAIN."
  (let ((x-count (horizontal-chunk-count
                  (luft:world-domain-x-period domain)))
        (y-count (horizontal-chunk-count
                  (luft:world-domain-y-period domain))))
    (+ (ash (luft:site-x site) (- +chunk-bits+))
       (* x-count
          (+ (ash (luft:site-y site) (- +chunk-bits+))
             (* y-count (ash (luft:site-z site) (- +chunk-bits+))))))))

(defun page-capacity-for (page-count)
  (loop for capacity = 1 then (* 2 capacity)
        when (<= page-count capacity)
          return capacity))

(defun ensure-scene-page-capacity (scene needed)
  "Grow SCENE's CPU face-page arena geometrically to hold NEEDED pages."
  (when (< (scene-page-capacity scene) needed)
    (let* ((capacity (page-capacity-for needed))
           (pages (make-array (* capacity +surface-chunk-capacity+)
                              :element-type '(unsigned-byte 64)
                              :initial-element 0)))
      (when (scene-site-pages scene)
        (replace pages (scene-site-pages scene)))
      (setf (scene-site-pages scene) pages
            (scene-page-capacity scene) capacity)))
  scene)

(defun ensure-surface-chunk (scene index)
  "Return SCENE's chunk at INDEX, assigning it one stable face page."
  (or (aref (scene-surface-chunks scene) index)
      (let ((chunk (make-surface-chunk
                    (luft:make-chain (scene-domain scene))))
            (page (scene-page-count scene)))
        (ensure-scene-page-capacity scene (1+ page))
        (setf (surface-chunk-page chunk) page
              (aref (scene-surface-chunks scene) index) chunk)
        (incf (scene-page-count scene))
        chunk)))

(defun index-set-vector (set)
  (let ((indices (make-array (hash-table-count set)
                             :element-type 'fixnum))
        (cursor 0))
    (maphash (lambda (index present-p)
               (declare (ignore present-p))
               (setf (aref indices cursor) index)
               (incf cursor))
             set)
    (sort indices #'<)))

(defun reserve-scene-edit-pages (scene cells)
  "Assign stable pages for every chunk any signed cell in CELLS can touch.

This is a cold preparation operation for a known edit sequence, such as a
film.  It prevents page-arena growth from turning otherwise incremental frame
updates into buffer replacements."
  (let ((indices (make-hash-table :test #'eql))
        (domain (scene-domain scene)))
    (loop for cell across cells
          do (luft:map-site-boundary
              (lambda (face axis side)
                (declare (ignore axis side))
                (setf (gethash (site-chunk-index domain face) indices) t))
              domain cell))
    (let ((missing 0))
      (maphash (lambda (index present-p)
                 (declare (ignore present-p))
                 (unless (aref (scene-surface-chunks scene) index)
                   (incf missing)))
               indices)
      (ensure-scene-page-capacity scene (+ (scene-page-count scene) missing)))
    (loop for index across (index-set-vector indices)
          do (ensure-surface-chunk scene index))
    scene))

(defun site-solid-cell (site)
  "The X, Y, and Z of the solid cell a surface face SITE bounds.

The face's missing extent axis is its normal axis; its polarity says which
way that normal points, and the solid is the cell on the inward side."
  (let ((x (luft:site-x site))
        (y (luft:site-y site))
        (z (luft:site-z site))
        (extent (luft:site-extent site))
        ;; The canonical orientation of a face is +X, -Y, +Z by spanning
        ;; axis; polarity flips it, and the solid lies one cell back
        ;; whenever the outward normal runs along the positive axis.
        (negative-p (luft:site-negative-p site)))
    (cond ((= extent luft:+yz-face-extent+)
           (values (if negative-p x (1- x)) y z))
          ((= extent luft:+xz-face-extent+)
           (values x (if negative-p (1- y) y) z))
          (t (values x y (if negative-p z (1- z)))))))

(defun stamp-site-stocks (sites domain slots)
  "Set each site's stock bits from SLOTS, the stock slot of every cell."
  (dotimes (index (length sites) sites)
    (let ((site (aref sites index)))
      (unless (zerop site)
        (multiple-value-bind (x y z) (site-solid-cell site)
          (when (<= 0 z (1- luft:+vertical-cell-rows+))
            (setf (ldb (byte shaders:+site-stock-bits+
                             shaders:+site-stock-shift+)
                       (aref sites index))
                  (aref slots (luft:cell-bit-index domain x y z)))))))))

(defun pack-slot-words (scene)
  "SCENE's stock slots packed eight nibbles to a 32-bit word.

The vertex stage asks a cell what it is cut from in order to decide how
wide the creases around it are planed, so the slots have to be on the GPU
in the same dense order the occupancy bits use.  A scene with no slots of
its own packs to zeros, which is slot zero, which is *MATERIAL*."
  (let* ((domain (scene-domain scene))
         (count (luft:chain-cell-bit-count domain))
         (words (make-array (ceiling count 8)
                            :element-type '(unsigned-byte 32)
                            :initial-element 0))
         (slots (scene-slots scene)))
    (when slots
      (dotimes (index count)
        (let ((slot (aref slots index)))
          (unless (zerop slot)
            (setf (ldb (byte 4 (* 4 (mod index 8)))
                       (aref words (floor index 8)))
                  (logand slot #xf))))))
    words))

(defun rebuild-surface-chunk (scene index)
  "Reorder and restamp only SCENE's surface chunk at INDEX."
  (let* ((chunk (aref (scene-surface-chunks scene) index))
         (raw (if chunk (luft:chain-sites (surface-chunk-chain chunk)) #()))
         (count (length raw)))
    (when (> count +surface-chunk-capacity+)
      (error "Surface chunk ~D has ~D faces, beyond its exact ~D-site bound."
             index count +surface-chunk-capacity+))
    (when chunk
      (let ((sites (make-array count :element-type '(unsigned-byte 64)))
            (start (* (surface-chunk-page chunk)
                      +surface-chunk-capacity+)))
        (replace sites raw)
        (when (scene-slots scene)
          (stamp-site-stocks sites (scene-domain scene) (scene-slots scene)))
        (replace (scene-site-pages scene) sites :start1 start)
        (setf (surface-chunk-count chunk) count)))
    count))

(defun scene-sites (scene)
  "Return SCENE's current packed surface sites in chunk order.

The renderer reads stable chunk pages directly; this compact vector is the
inspection and reference form, allocated only when a caller asks for it."
  (let ((sites (make-array (luft:chain-count (scene-surface scene))
                           :element-type '(unsigned-byte 64)))
        (cursor 0))
    (loop for chunk across (scene-surface-chunks scene)
          when (and chunk (plusp (surface-chunk-count chunk)))
            do (let* ((count (surface-chunk-count chunk))
                      (start (* (surface-chunk-page chunk)
                                +surface-chunk-capacity+)))
                 (replace sites (scene-site-pages scene)
                          :start1 cursor :start2 start :end2 (+ start count))
                 (incf cursor count)))
    sites))

(defun refresh-scene (scene)
  "Recompute every derived product of SCENE from its solid reference chain."
  (let* ((domain (scene-domain scene))
         (surface (luft:surface-chain (scene-solid scene)))
         (chunks (make-array (domain-surface-chunk-count domain)
                             :initial-element nil)))
    ;; Partition first, then number nonempty pages in chunk order.  The stable
    ;; page address is what lets later edits upload one reordered summand.
    (luft:map-chain
     (lambda (site)
       (let* ((index (site-chunk-index domain site))
              (chunk (or (aref chunks index)
                         (setf (aref chunks index)
                               (make-surface-chunk
                                (luft:make-chain domain))))))
         (luft:add-chain-site (surface-chunk-chain chunk) site)))
     surface)
    (let ((page-count 0))
      (loop for chunk across chunks
            when chunk
              do (setf (surface-chunk-page chunk) page-count)
                 (incf page-count))
      (let ((page-capacity (page-capacity-for page-count)))
        (setf (scene-surface scene) surface
              (scene-surface-chunks scene) chunks
              (scene-page-count scene) page-count
              (scene-page-capacity scene) page-capacity
              (scene-site-pages scene)
              (make-array (* page-capacity +surface-chunk-capacity+)
                          :element-type '(unsigned-byte 64)
                          :initial-element 0)
              (scene-cell-bits scene)
              (luft:chain-cell-bits (scene-solid scene))
              (scene-slot-words scene) (pack-slot-words scene))
        (dotimes (index (length chunks))
          (when (aref chunks index)
            (rebuild-surface-chunk scene index)))))
    ;; A full refresh breaks the incremental revision chain.  Renderers which
    ;; did not consume it must perform one coherent full upload.
    (incf (scene-revision scene))
    (setf (scene-changes scene) nil)
    scene))

(defun publish-scene-change (scene chunks cell-words &optional slot-words)
  "Publish one complete incremental materialization revision of SCENE."
  (let* ((revision (incf (scene-revision scene)))
         (change (make-scene-change revision
                                    (index-set-vector chunks)
                                    (index-set-vector cell-words)
                                    (index-set-vector
                                     (or slot-words
                                         (make-hash-table :test #'eql))))))
    (push change (scene-changes scene))
    (let ((tail (nthcdr (1- *scene-change-history-limit*)
                        (scene-changes scene))))
      (when tail (setf (cdr tail) nil)))
    change))

(defun validate-scene-edit (scene edit)
  "Return EDIT's sites after proving it is a binary cell-chain delta."
  (check-type edit luft:chain)
  (unless (equalp (scene-domain scene) (luft:chain-domain edit))
    (error "Scene and edit use different world domains."))
  (let ((sites (luft:chain-sites edit))
        (seen (make-hash-table :test #'eql)))
    (loop for site across sites
          do (unless (= (luft:site-extent site) luft:+cell-extent+)
               (error "A scene edit contains non-cell site ~S." site))
             (unless (world-vertical-p (luft:site-z site))
               (error "A scene edit contains out-of-world cell ~S." site))
             (when (gethash (luft:site-geometry site) seen)
               (error "A scene edit contains repeated cell ~S." site))
             (setf (gethash (luft:site-geometry site) seen) t)
             (let ((solid-p (luft:solid-cell-p
                             (scene-solid scene)
                             (luft:site-x site) (luft:site-y site)
                             (luft:site-z site))))
               (when (eql solid-p (luft:site-positive-p site))
                 (error "Scene edit ~S would ~:[remove empty~;fill solid~] cell."
                        site solid-p))))
    sites))

(defun set-scene-cell-bit (scene site solid-p)
  "Set SITE's dense occupancy bit and return its changed word index."
  (let* ((bit (luft:cell-bit-index
               (scene-domain scene)
               (luft:site-x site) (luft:site-y site) (luft:site-z site)))
         (word (floor bit 32)))
    (setf (ldb (byte 1 (mod bit 32)) (aref (scene-cell-bits scene) word))
          (if solid-p 1 0))
    word))

(defun apply-scene-edit (scene edit)
  "Apply signed cell-chain EDIT to SCENE without remeshing the world.  #3QNZC1

If C is the solid and S its surface, EDIT is a sparse 3-chain delta and the
publication is exactly

    C' = C + EDIT,        S' = S + boundary(EDIT) = boundary(C').

Only chunk summands touched by BOUNDARY(EDIT) are reordered.  Inputs are
validated completely before SCENE is mutated; repeated fills, repeated
removals, non-cell sites, and foreign domains are errors."
  (let ((sites (validate-scene-edit scene edit)))
    (when (zerop (length sites))
      (return-from apply-scene-edit scene))
    (let ((surface-change (luft:boundary-chain edit))
          (chunks (make-hash-table :test #'eql))
          (cell-words (make-hash-table :test #'eql)))
      (luft:add-chain (scene-solid scene) edit)
      (loop for site across sites
            do (setf (gethash
                      (set-scene-cell-bit
                       scene site (luft:site-positive-p site))
                      cell-words)
                     t))
      (luft:add-chain (scene-surface scene) surface-change)
      (luft:map-chain
       (lambda (face)
         (let* ((index (site-chunk-index (scene-domain scene) face))
                (chunk (ensure-surface-chunk scene index)))
           (luft:add-chain-site (surface-chunk-chain chunk) face)
           (setf (gethash index chunks) t)))
       surface-change)
      (maphash (lambda (index present-p)
                 (declare (ignore present-p))
                 (rebuild-surface-chunk scene index))
               chunks)
      (publish-scene-change scene chunks cell-words)
      scene)))

(defun scene-cell-p (scene x y z)
  "Whether SCENE's solid contains the cell at X,Y,Z."
  (and (world-vertical-p z)
       (luft:solid-cell-p (scene-solid scene) x y z)))

(defun (setf scene-cell-p) (state scene x y z)
  "Set one SCENE cell through an exact signed-chain edit."
  (when (and (world-vertical-p z)
             (not (eql (not (null state)) (scene-cell-p scene x y z))))
    (let* ((edit (luft:make-chain (scene-domain scene)))
           (cell (luft:make-site (scene-domain scene) x y z
                                 luft:+cell-extent+
                                 (if state 1 -1))))
      (luft:add-chain-site edit cell)
      (apply-scene-edit scene edit)))
  state)

(defun scene-changes-since (scene revision)
  "Return changed chunk/cell/slot indices since REVISION and availability.

The fourth value is false when the bounded history cannot bridge REVISION to
the current scene, in which case a renderer must upload a full cohort."
  (block changes
    (unless (and (integerp revision)
                 (<= 0 revision (scene-revision scene)))
      (return-from changes (values nil nil nil nil)))
    (let ((expected (scene-revision scene))
          (chunks (make-hash-table :test #'eql))
          (cell-words (make-hash-table :test #'eql))
          (slot-words (make-hash-table :test #'eql)))
      (dolist (change (scene-changes scene))
        (when (<= (scene-change-revision change) revision)
          (return))
        (unless (= (scene-change-revision change) expected)
          (return-from changes (values nil nil nil nil)))
        (loop for index across (scene-change-chunks change)
              do (setf (gethash index chunks) t))
        (loop for index across (scene-change-cell-words change)
              do (setf (gethash index cell-words) t))
        (loop for index across (scene-change-slot-words change)
              do (setf (gethash index slot-words) t))
        (decf expected))
      (if (= expected revision)
          (values (index-set-vector chunks)
                  (index-set-vector cell-words)
                  (index-set-vector slot-words)
                  t)
          (values nil nil nil nil)))))

;;; ------------------------------------------------------------------------
;;; A demonstration world

(defun demo-height (x y)
  "Rolling ground with a plateau, in cells."
  (let ((rolling (+ 5.5
                    (* 2.4 (sin (/ x 6.0)) (cos (/ y 7.5)))
                    (* 1.2 (sin (/ (+ x y) 4.3)))))
        (plateau (if (and (<= 40 x 52) (<= 10 y 22)) 3.0 0.0)))
    (max 1 (floor (+ rolling plateau)))))

(defun fill-box (world x0 x1 y0 y1 z0 z1 &optional (state t))
  "Set every cell of the closed box to STATE, stamping *STOCK* where it fills."
  (loop for x from x0 to x1
        do (loop for y from y0 to y1
                 do (loop for z from z0 to z1
                          do (setf (world-cell-p world x y z) state)))))

(defun carve-ravine (world)
  "A gap for the bridge to cross, cut where the ground was continuous."
  (loop for y from 40 to 56
        do (let ((width (+ 3 (floor (abs (- y 48)) 3))))
             (loop for x from (- 30 width) to (+ 30 width)
                   do (loop for z from 0 to 12
                            do (setf (world-cell-p world x y z) nil))))))

(defun build-bridge (world)
  "A deck across the ravine on two piers, with a parapet either side.

Something has to span a gap before a world has anywhere to stand and look
down from, and a deck one cell thick with a parapet at its edge is the
smallest thing that reads as built rather than as terrain.  The piers are
stone because they stand in the water and the deck is oak because it is
walked on, which is the whole argument for a world knowing its stocks."
  (let ((deck 9))
    (with-stock (:granite)
      (dolist (x '(26 34))
        (fill-box world x (1+ x) 44 45 0 (1- deck))
        (fill-box world x (1+ x) 51 52 0 (1- deck))))
    (with-stock (:oak)
      (fill-box world 24 36 43 53 deck deck)
      (loop for x from 24 to 36
            unless (zerop (mod (- x 24) 4))
              do (setf (world-cell-p world x 43 (+ deck 1)) t
                       (world-cell-p world x 53 (+ deck 1)) t)))
    ;; Ramps up to the deck at either end, cut into the ground itself.
    (with-stock (:limestone)
      (loop for step from 0 to 8
            do (fill-box world (- 23 step) (- 23 step) 45 51
                         0 (max 0 (- deck 1 step)))
               (fill-box world (+ 37 step) (+ 37 step) 45 51
                         0 (max 0 (- deck 1 step)))))))

(defun build-balconies (world)
  "Three balconies off the tower, each a slab with a lip and a doorway."
  (loop for (z side) in '((6 :east) (12 :north) (17 :east))
        do (ecase side
             (:east
              (with-stock (:oak) (fill-box world 28 31 32 35 z z))
              (with-stock (:bronze)
                (fill-box world 31 31 32 35 (1+ z) (1+ z))
                (fill-box world 28 31 32 32 (1+ z) (1+ z))
                (fill-box world 28 31 35 35 (1+ z) (1+ z)))
              ;; The doorway it is reached through.
              (fill-box world 27 27 33 34 z (+ z 1) nil))
             (:north
              (with-stock (:oak) (fill-box world 22 25 38 41 z z))
              (with-stock (:bronze)
                (fill-box world 22 25 41 41 (1+ z) (1+ z))
                (fill-box world 22 22 38 41 (1+ z) (1+ z))
                (fill-box world 25 25 38 41 (1+ z) (1+ z)))
              (fill-box world 23 24 37 37 z (+ z 1) nil)))))

(defun build-terraces (world)
  "Stepped terraces below the tower: a hillside someone has taken in hand."
  (loop for step from 0 below 5
        for z = (+ 3 step)
        for near = (- 18 (* 2 step))
        do (with-stock (:turf)
             (fill-box world near (+ near 1) (- 24 step) (+ 33 step) 0 z))
           ;; A low retaining wall along the front of each terrace.
           (with-stock (:limestone)
             (loop for y from (- 24 step) to (+ 33 step)
                   unless (zerop (mod y 5))
                     do (setf (world-cell-p world near y (1+ z)) t)))))

(defun make-demo-scene (&key (horizontal-bits 6))
  "A small world: rolling turf, a stone tower, a timber bridge, terraces."
  (let* ((world (make-world :horizontal-bits horizontal-bits))
         (period (luft:world-domain-x-period (world-domain world))))
    (with-stock (:turf)
      (dotimes (x period)
        (dotimes (y period)
          (dotimes (z (demo-height x y))
            (setf (world-cell-p world x y z) t)))))
    ;; A hollow tower with a doorway.
    (with-stock (:limestone)
      (loop for z from 1 below 22
            do (loop for x from 20 to 27
                     do (loop for y from 30 to 37
                              when (and (or (= x 20) (= x 27)
                                            (= y 30) (= y 37))
                                        (not (and (= y 30) (<= 23 x 24)
                                                  (< z 9))))
                                do (setf (world-cell-p world x y z) t)))))
    ;; A floating slab, casting a clean shadow of empty air.
    (with-stock (:slate)
      (fill-box world 8 15 8 12 14 14))
    ;; A staircase up the plateau.
    (with-stock (:limestone)
      (loop for step from 0 below 6
            do (loop for y from 14 to 18
                     do (loop for z from 0 to (+ 4 step)
                              do (setf (world-cell-p world (- 39 step) y z)
                                       t)))))
    (carve-ravine world)
    (build-bridge world)
    (build-balconies world)
    (build-terraces world)
    (world-scene world)))

;;; ------------------------------------------------------------------------
;;; Camera

(defclass fly-camera ()
  ((position
    :initarg :position
    :accessor camera-position)
   (yaw
    :initarg :yaw
    :initform 0.0
    :accessor camera-yaw
    :documentation "Radians from +X toward +Y.")
   (pitch
    :initarg :pitch
    :initform 0.0
    :accessor camera-pitch
    :documentation "Radians above the horizon; Z is up.")
   (field-of-view
    :initarg :field-of-view
    :initform (* 70.0 (/ pi 180))
    :accessor camera-field-of-view)))

(defun make-fly-camera (&key (position (vec3:make-vec3 46.0 2.0 15.0))
                          (yaw 2.15) (pitch -0.22)
                          (field-of-view (* 70.0 (/ pi 180))))
  (make-instance 'fly-camera :position position :yaw yaw :pitch pitch
                             :field-of-view field-of-view))

(defun camera-basis (camera)
  "Return the camera's RIGHT, UP, and FORWARD unit vectors in a Z-up world."
  (let* ((yaw (camera-yaw camera))
         (pitch (camera-pitch camera))
         (forward (vec3:make-vec3 (* (cos yaw) (cos pitch))
                                  (* (sin yaw) (cos pitch))
                                  (sin pitch)))
         (right (vec3:make-vec3 (sin yaw) (- (cos yaw)) 0.0))
         (up (vec3:vec3-cross right forward)))
    (values right up forward)))

(defparameter *near-distance* 0.1)
(defparameter *far-distance* 400.0)

(defstruct (frame-view (:constructor %make-frame-view))
  "An immutable camera sample used by every pass of one encoded frame.

The interactive camera remains mutable, but temporal rendering must not read
it twice: CURRENT and PREVIOUS only mean something when one coherent basis,
projection, and jitter is frozen at the frame boundary.  #VATCML"
  position right up forward projection jitter)

(defun frame-projection (camera width height)
  "Return CAMERA's four projection coefficients for WIDTH by HEIGHT."
  (let* ((near *near-distance*)
         (far *far-distance*)
         (focal (/ (tan (/ (camera-field-of-view camera) 2.0))))
         (aspect (/ (coerce width 'single-float) height)))
    (vector (coerce (/ focal aspect) 'single-float)
            (coerce focal 'single-float)
            (coerce (/ far (- far near)) 'single-float)
            (coerce (/ (- (* far near)) (- far near)) 'single-float))))

(defun halton (index base)
  "The radical-inverse sample INDEX in BASE, as a single float."
  (check-type index (integer 1 *))
  (check-type base (integer 2 *))
  (loop with fraction = (/ 1.0 base)
        with value = 0.0
        while (plusp index)
        do (incf value (* fraction (mod index base)))
           (setf index (floor index base)
                 fraction (/ fraction base))
        finally (return (coerce value 'single-float))))

(defun temporal-jitter (frame-index width height)
  "Return sample FRAME-INDEX's Halton(2,3) offset in clip coordinates."
  (let ((sample (1+ (mod frame-index 8))))
    (vector (coerce (/ (* 2.0 (- (halton sample 2) 0.5))
                       (max width 1))
                    'single-float)
            (coerce (/ (* 2.0 (- (halton sample 3) 0.5))
                       (max height 1))
                    'single-float))))

(defun capture-frame-view (camera width height &optional (jitter #(0.0 0.0)))
  "Freeze CAMERA and JITTER into the semantic view of one frame."
  (multiple-value-bind (right up forward) (camera-basis camera)
    (flet ((copy-vec3 (value)
             (vec3:make-vec3 (vec3:vec3-x value)
                             (vec3:vec3-y value)
                             (vec3:vec3-z value))))
      (%make-frame-view
       :position (copy-vec3 (camera-position camera))
       :right (copy-vec3 right)
       :up (copy-vec3 up)
       :forward (copy-vec3 forward)
       :projection (frame-projection camera width height)
       :jitter (map 'vector (lambda (value) (coerce value 'single-float))
                    jitter)))))

(defun frame-views-continuous-p (previous current)
  "Whether PREVIOUS and CURRENT form motion, rather than a camera cut."
  (and previous
       (let* ((new-position (frame-view-position current))
              (old-position (frame-view-position previous))
              (delta (vec3:make-vec3
                      (- (vec3:vec3-x new-position)
                         (vec3:vec3-x old-position))
                      (- (vec3:vec3-y new-position)
                         (vec3:vec3-y old-position))
                      (- (vec3:vec3-z new-position)
                         (vec3:vec3-z old-position))))
              (distance-squared (vec3:vec3-dot delta delta))
              (facing (vec3:vec3-dot (frame-view-forward previous)
                                     (frame-view-forward current)))
              (old-projection (frame-view-projection previous))
              (new-projection (frame-view-projection current)))
         (and (< distance-squared 64.0)
              (> facing 0.5)
              (< (abs (- (aref old-projection 1)
                         (aref new-projection 1)))
                 0.25)))))

(defparameter *sun-direction*
  (vec3:vec3-normalize (vec3:make-vec3 0.52 0.30 0.62))
  "The direction toward the key light, low enough to model the terraces.")
(defparameter *sun-color* (vec3:make-vec3 1.05 0.96 0.82)
  "The key light's radiance, warm as afternoon sun.")
(defparameter *sheen-strength* 0.16
  "How brightly a face catches the sun's reflection; chamfers show it most.")
(defparameter *fill-direction*
  (vec3:vec3-normalize (vec3:make-vec3 -0.62 -0.55 0.24))
  "The direction toward the cool fill light opposite the sun.")
(defparameter *fill-strength* 0.30
  "The fill light's strength, which separates the faces the sun misses.")
(defparameter *ambient-light* 0.42
  "The strength of the ambient hemisphere: sky above, bounce below.")
(defparameter *ground-color* (vec3:make-vec3 0.34 0.30 0.24)
  "The bounce colour of the lower hemisphere.")
(defparameter *top-color* (vec3:make-vec3 0.17 0.36 0.11)
  "The material of an upward face: turf, in linear light.")
(defparameter *side-color* (vec3:make-vec3 0.42 0.32 0.21)
  "The material of a sideways face: the earth a cut exposes.")
(defparameter *bottom-color* (vec3:make-vec3 0.11 0.10 0.10)
  "The material of a downward face: an underside, seen rarely.")
(defparameter *shadow-strength* 1.0
  "How darkly the sun's walk shadows a point; zero switches shadows off.")
(defparameter *occlusion-strength* 0.75
  "How deeply the crowding of nearby cells darkens the ambient hemisphere.")
(defparameter *wear-strength* 0.6
  "How strongly the :FIELD style lightens ridges and darkens hollows.")
(defparameter *ink-width* 2.5
  "The :INK style's line width in pixels of the rendered frame.")
(defparameter *exposure* 1.15
  "Exposure of the 1 - exp(-x) curve the lit colour rolls off through.")
(defparameter *sky-color* (vec3:make-vec3 0.56 0.71 0.90)
  "The colour of the horizon, which the fog also converges to.

Deep enough that stone reads against it: a sky written at the value the eye
takes off a photograph leaves every building silhouetted on nothing.")
(defparameter *draw-sky* t
  "Whether the background is the gradient sky pass or the flat clear colour.")
(defparameter *focus-distance* 40.0
  "How far the lens is focused, in cells; also the alpha channel's scale.")
(defparameter *aperture* 0.0
  "How strongly the focus pass softens the distance; zero is a pinhole.")
(defparameter *fog-distance* 240.0
  "How far away the world has gone entirely to sky.

Far enough that a sixty-cell world does not dissolve at its own horizon:
fog is for saying that distance exists, not for hiding the far bank.")
(defparameter *bevel-radius* 0.22
  "The :BEVEL style's crease-rounding radius in cells, below one half.")
(defparameter *chamfer-width* 0.11
  "The :CHAMFER style's 45-degree crease relief in cells.

Wide enough that the planed facet reads as a face of its own and catches
the light as a band rather than a hairline, and still far short of the old
0.22-cell coves that made the world look carved.")
(defparameter *arris-softness* 0.004
  "The narrow shading transition where a chamfer meets its original face.")
(defparameter *field-vertical-radius* nil
  "The :FIELD style's tent half-width along Z, or NIL for *BEVEL-RADIUS*.

Wider than the horizontal radius, it rounds the edges of floors and roofs
more than the edges of walls, the way weather wears a top.")
(defparameter *clay-radius* 0.30
  "The :CLAY style's cell rounding in cells, up to one half.

Every solid cell is a rounded box of this radius before the smooth union
takes them: at one half a lone cell is exactly a sphere and a bar a string
of pearls.  A frame-block lane, so a film can turn the knob mid-reel.")
(defparameter *clay-melt* 0.18
  "The :CLAY style's smooth-union reach in cells, at most one half.

Small leaves every coplanar seam a tight quilted dimple; larger deepens
the quilting and grows bridges between diagonal contacts.")
(defparameter *clay-stocks* nil
  "Stock names drawn as clay while another style draws the rest, or NIL.

With names here and :CLAY among the renderer's pipeline styles, every
face whose stock is named becomes the clay overlay's and leaves the main
style's draw: a chamfered world whose tree crowns are clay blobs.  At
most two stocks fit the lane; the first two found in the scene's palette
are the ones taken.")

(defun clay-stock-lane (stocks)
  "Pack the first two *CLAY-STOCKS* slots of the palette STOCKS as a float."
  (let ((found (and stocks
                    (loop for name in *clay-stocks*
                          for slot = (position name stocks)
                          when slot collect slot))))
    (if found
        (coerce (+ 1 (first found)
                   (* 16 (if (rest found) (+ 1 (second found)) 0)))
                'single-float)
        0.0)))

;;; ------------------------------------------------------------------------
;;; Lights: the hour a picture is taken at
;;;
;;; The knobs above are the whole of the atelier's light, and setting eleven
;;; of them by hand is not how anyone chooses an hour.  A light names a set
;;; of them together -- where the sun is, what colour it is, what the sky
;;; does, how far one can see -- so that a contact sheet can put the same
;;; world at four times of day beside itself, and so that a picture can be
;;; composed by naming a light rather than by tuning a lamp.
;;;
;;; A slot left NIL keeps whatever the special above says, so :AFTERNOON,
;;; which names nothing, is exactly the atelier's own light and every knob
;;; still works by hand.  #KG0EG6

(defclass light ()
  ((name :initarg :name :reader light-name)
   (sun :initarg :sun :initform nil :reader light-sun
        :documentation "The direction toward the key light, as three floats.")
   (sun-color :initarg :sun-color :initform nil :reader light-sun-color)
   (sky :initarg :sky :initform nil :reader light-sky)
   (ground :initarg :ground :initform nil :reader light-ground)
   (fill :initarg :fill :initform nil :reader light-fill)
   (fill-strength :initarg :fill-strength :initform nil
                  :reader light-fill-strength)
   (ambient :initarg :ambient :initform nil :reader light-ambient)
   (exposure :initarg :exposure :initform nil :reader light-exposure)
   (sheen :initarg :sheen :initform nil :reader light-sheen)
   (fog :initarg :fog :initform nil :reader light-fog)
   (shadow :initarg :shadow :initform nil :reader light-shadow)
   (occlusion :initarg :occlusion :initform nil :reader light-occlusion))
  (:documentation "An hour of the day, as a set of the atelier's light knobs."))

(defvar *light-table* (make-hash-table :test 'eq)
  "Every defined light, by name.")

(defmacro define-light (name &body initargs)
  "Define or redefine the light called NAME from INITARGS."
  `(setf (gethash ,name *light-table*)
         (make-instance 'light :name ,name ,@initargs)))

(defun find-light (name)
  "The light called NAME, or an error naming what there is."
  (or (gethash name *light-table*)
      (error "No light ~S; there is ~{~S~^, ~}." name (light-names))))

(defun light-names ()
  "Every defined light's name, in alphabetical order."
  (sort (loop for name being the hash-keys of *light-table* collect name)
        #'string< :key #'symbol-name))

(defparameter *light* :afternoon
  "The light every frame is drawn under.")

(defun light-direction (list)
  "A unit direction from a list of three floats."
  (vec3:vec3-normalize
   (apply #'vec3:make-vec3
          (mapcar (lambda (x) (coerce x 'single-float)) list))))

(defun light-colour (list)
  (apply #'vec3:make-vec3
         (mapcar (lambda (x) (coerce x 'single-float)) list)))

(define-light :afternoon
  ;; The atelier's own light, named so that a sheet can ask for it: a warm
  ;; sun about thirty degrees up, a cool fill from the opposite quarter.
  )

(define-light :morning
  ;; Low from the east, the air still cool and clear, shadows long enough
  ;; to draw the plan of a building on the ground beside it.
  :sun '(-0.78 0.36 0.30) :sun-color '(1.02 0.94 0.86)
  :sky '(0.60 0.74 0.94) :fill '(0.55 -0.45 0.35) :fill-strength 0.26
  :ambient 0.40 :fog 320.0 :exposure 1.12)

(define-light :noon
  ;; Almost overhead: tops blaze, walls fall away, and every shadow is a
  ;; small hard pool underneath the thing that casts it.
  :sun '(0.18 0.12 0.97) :sun-color '(1.12 1.06 0.98)
  :sky '(0.55 0.72 0.96) :fill '(-0.4 -0.4 0.2) :fill-strength 0.22
  :ambient 0.46 :fog 360.0 :exposure 1.05 :sheen 0.20)

(define-light :evening
  ;; The sun nearly down and very warm; the sky behind it goes rose, the
  ;; shadows go blue, and the fog closes in the distance.
  :sun '(0.86 -0.28 0.16) :sun-color '(1.35 0.86 0.54)
  :sky '(0.62 0.60 0.72) :ground '(0.30 0.22 0.20)
  :fill '(-0.55 0.42 0.30) :fill-strength 0.34
  :ambient 0.34 :fog 170.0 :exposure 1.25 :sheen 0.26)

(define-light :overcast
  ;; No sun to speak of: everything is the sky, occlusion does all the
  ;; drawing, and the world reads as form rather than as light.
  :sun '(0.10 0.15 0.98) :sun-color '(0.26 0.27 0.29)
  :sky '(0.70 0.72 0.76) :ground '(0.30 0.30 0.29)
  :fill '(-0.3 -0.3 0.6) :fill-strength 0.34
  :ambient 0.86 :fog 210.0 :exposure 0.92 :shadow 0.18 :occlusion 1.0
  :sheen 0.04)

(define-light :daybreak
  ;; The first minutes of direct sun: a fire-coloured key barely off the
  ;; horizon, the air still blue in every hollow, fog holding the
  ;; distances close.  The most dramatic hour the atelier has.
  :sun '(0.80 0.42 0.14) :sun-color '(1.55 0.80 0.42)
  :sky '(0.48 0.58 0.80) :ground '(0.24 0.19 0.17)
  :fill '(-0.5 0.35 0.30) :fill-strength 0.36
  :ambient 0.30 :fog 140.0 :exposure 1.32 :sheen 0.34)

(define-light :golden
  ;; The hour the mountain games are graded for: the sun low and honeyed
  ;; out of the west, shadows long and gone a little blue, the sky still
  ;; bright enough to hold the tops.  Lonely Mountains: Downhill lives
  ;; about here all day.
  :sun '(-0.62 0.38 0.34) :sun-color '(1.22 1.00 0.72)
  :sky '(0.60 0.75 0.96) :ground '(0.40 0.34 0.26)
  :fill '(0.55 -0.40 0.32) :fill-strength 0.30
  :ambient 0.44 :fog 300.0 :exposure 1.16 :sheen 0.22)

(defmacro with-light ((name) &body body)
  "Evaluate BODY under the light called NAME."
  `(let ((*light* ,name)) ,@body))

(defun material-albedo (material face)
  "MATERIAL's colour for an upward, sideways, or downward FACE.

A material that does not name a colour takes the world's own, so a material
may speak only of its finish and its figure."
  (flet ((colour (list) (apply #'vec3:make-vec3
                               (mapcar (lambda (x) (coerce x 'single-float))
                                       list))))
    (ecase face
      (:top (let ((own (material-top material)))
              (if own (colour own) *top-color*)))
      (:side (let ((own (material-side material)))
               (if own (colour own) *side-color*)))
      (:bottom (let ((own (material-bottom material)))
                 (if own (colour own) *bottom-color*))))))

(defun stock-table-data (stocks)
  "The table the :STOCK style indexes with a site's four stock bits.

Every slot takes +STOCK-LANES+ vec4s: the three albedos, the five lanes of
MATERIAL-LANES, and one lattice lane.  STOCKS is a scene's palette of
material names; a scene with none is drawn wholly in *MATERIAL*, and slots
past the end of the palette repeat slot zero so a stale site can never read
rubbish."
  (let* ((names (if (and stocks (plusp (length stocks)))
                    stocks
                    (vector *material*)))
         (data (make-array (* 4 shaders:+stock-lanes+ shaders:+stock-slots+)
                           :element-type 'single-float :initial-element 0.0))
         (index 0))
    (flet ((quad (floats)
             (loop for value in floats
                   do (setf (aref data index) (coerce value 'single-float))
                      (incf index))))
      (dotimes (slot shaders:+stock-slots+ data)
        (let* ((name (aref names (if (< slot (length names)) slot 0)))
               (material (find-material name)))
          ;; The albedo lanes' fourth components are spare; the first
          ;; carries the grain's pith spacing.
          (loop for face in '(:top :side :bottom)
                for spare in (list (material-spacing material)
                                   (material-courses material)
                                   (material-chamfer material))
                for colour = (material-albedo material face)
                do (quad (list (vec3:vec3-x colour) (vec3:vec3-y colour)
                               (vec3:vec3-z colour) spare)))
          (mapc #'quad (material-lanes material))
          ;; The lattice lane: what this stock does to the shape.
          (quad (list (material-grit material) 0.0 0.0 0.0)))))))

(defun frame-uniform-data
    (camera-or-view width height
     &optional domain (surface-width *bevel-radius*)
                      (surface-detail *arris-softness*)
                      previous-view history-valid-p (history-weight 0.9))
  "Pack the shared frame block from one frozen current and previous view.

CAMERA-OR-VIEW accepts a mutable camera for callers outside the frame loop,
but ENCODE-FRAME passes a FRAME-VIEW captured exactly once.  SURFACE-WIDTH is
the style's rounding radius or chamfer width, and SURFACE-DETAIL its second
lane: the arris softness of a chamfer, or the vertical radius of the field."
  (let* ((view (if (frame-view-p camera-or-view)
                   camera-or-view
                   (capture-frame-view camera-or-view width height)))
         (previous (or previous-view view))
         (right (frame-view-right view))
         (up (frame-view-up view))
         (forward (frame-view-forward view))
         (projection (frame-view-projection view))
         (previous-projection (frame-view-projection previous))
         (jitter (frame-view-jitter view))
         (previous-jitter (frame-view-jitter previous)))
    (let* ((light (find-light *light*))
           (sun (if (light-sun light) (light-direction (light-sun light))
                    *sun-direction*))
           (sun-colour (if (light-sun-color light)
                           (light-colour (light-sun-color light)) *sun-color*))
           (sky-colour (if (light-sky light)
                           (light-colour (light-sky light)) *sky-color*))
           (ground-colour (if (light-ground light)
                              (light-colour (light-ground light))
                              *ground-color*))
           (fill-direction (if (light-fill light)
                               (light-direction (light-fill light))
                               *fill-direction*))
           (fill-strength (or (light-fill-strength light) *fill-strength*))
           (ambient (or (light-ambient light) *ambient-light*))
           (exposure (or (light-exposure light) *exposure*))
           (sheen (or (light-sheen light) *sheen-strength*))
           (fog (or (light-fog light) *fog-distance*))
           (shadow (or (light-shadow light) *shadow-strength*))
           (occlusion (or (light-occlusion light) *occlusion-strength*))
           (data (make-array (* 4 (length shaders:*frame-uniform-members*))
                             :element-type 'single-float))
           (index 0))
      (flet ((lane (vector fourth)
               (setf (aref data index) (coerce (vec3:vec3-x vector) 'single-float)
                     (aref data (+ index 1))
                     (coerce (vec3:vec3-y vector) 'single-float)
                     (aref data (+ index 2))
                     (coerce (vec3:vec3-z vector) 'single-float)
                     (aref data (+ index 3)) (coerce fourth 'single-float))
               (incf index 4))
             (quad (floats)
               (map nil
                    (lambda (value)
                      (setf (aref data index) (coerce value 'single-float))
                      (incf index))
                    floats)))
        (lane (frame-view-position view) 0.0)
        (lane right 0.0)
        (lane up 0.0)
        (lane forward 0.0)
        (quad projection)
        (lane sun ambient)
        (lane sky-colour fog)
        (lane (vec3:make-vec3
               (if domain (luft:world-domain-x-period domain) 1)
               (if domain (luft:world-domain-y-period domain) 1)
               surface-width)
              surface-detail)
        (lane sun-colour sheen)
        (lane fill-direction fill-strength)
        (lane ground-colour exposure)
        (lane (vec3:make-vec3 occlusion shadow *wear-strength*)
              *ink-width*)
        ;; The clay knobs ride the material lanes' spare components, so
        ;; the clay overlay keeps its own radius and melt while another
        ;; style owns the domain lane's width and detail.
        (lane *top-color* (coerce *clay-radius* 'single-float))
        (lane *side-color* (coerce *clay-melt* 'single-float))
        (lane *bottom-color* 0.0)
        (lane (vec3:make-vec3 *focus-distance* *aperture*
                              (/ 1.0 (max 1 width)))
              (/ 1.0 (max 1 height)))
        (quad (deform-lane))
        (quad (deform-centre-lane domain))
        (quad (arris-lane))
        ;; Temporal lanes are append-only: the established material ABI above
        ;; remains stable while motion and resolve share this same block.
        (lane (frame-view-position previous) 0.0)
        (lane (frame-view-right previous) 0.0)
        (lane (frame-view-up previous) 0.0)
        (lane (frame-view-forward previous) 0.0)
        (quad previous-projection)
        (quad (list (aref jitter 0) (aref jitter 1)
                    (aref previous-jitter 0) (aref previous-jitter 1)))
        (quad (list (/ 1.0 (max 1 width)) (/ 1.0 (max 1 height))
                    (if history-valid-p 1.0 0.0) history-weight)))
      data)))

;;; ------------------------------------------------------------------------
;;; Renderer

(defclass frame-surfaces ()
  ((extent :initarg :extent :reader frame-surfaces-extent)
   (color-texture :initarg :color-texture
                  :reader frame-surfaces-color-texture)
   (color-view :initarg :color-view :reader frame-surfaces-color-view)
   (depth-texture :initarg :depth-texture
                  :reader frame-surfaces-depth-texture)
   (depth-view :initarg :depth-view :reader frame-surfaces-depth-view)
   (scene-texture :initarg :scene-texture
                  :reader frame-surfaces-scene-texture)
   (scene-view :initarg :scene-view :reader frame-surfaces-scene-view)
   (motion-texture :initarg :motion-texture :initform nil
                   :reader frame-surfaces-motion-texture)
   (motion-view :initarg :motion-view :initform nil
                :reader frame-surfaces-motion-view)
   (temporal-scaler :initarg :temporal-scaler :initform nil
                    :reader frame-surfaces-temporal-scaler)
   (resolved-texture :initarg :resolved-texture :initform nil
                     :reader frame-surfaces-resolved-texture)
   (resolved-view :initarg :resolved-view :initform nil
                  :reader frame-surfaces-resolved-view)
   (history-textures :initarg :history-textures :initform #()
                     :reader frame-surfaces-history-textures)
   (history-views :initarg :history-views :initform #()
                  :reader frame-surfaces-history-views)
   (temporal-bind-groups :initarg :temporal-bind-groups :initform #()
                         :reader frame-surfaces-temporal-bind-groups)
   (post-bind-groups :initarg :post-bind-groups :initform #()
                     :reader frame-surfaces-post-bind-groups))
  (:documentation
   "One extent-sized, transactionally published cohort of frame resources.
#T7RQTI"))

(defclass surface-technique ()
  ((device :initarg :device :reader surface-technique-device)
   (layout :initform nil :accessor surface-technique-layout)
   (modules :initform nil :accessor surface-technique-modules)
   (pipelines :initform nil :accessor surface-technique-pipelines)
   (frame-states :initform nil :accessor surface-technique-frame-states)
   (pipeline-styles :initarg :pipeline-styles
                    :reader surface-technique-pipeline-styles)
   (target-formats :initarg :target-formats
                   :reader surface-technique-target-formats)
   (temporal-p :initarg :temporal-p :initform nil
               :reader surface-technique-temporal-p)
   (output-space :initarg :output-space :initform :presented
                 :reader surface-technique-output-space)
   (orthographic-shadow-depth-format
    :initarg :orthographic-shadow-depth-format :initform nil
    :reader surface-technique-orthographic-shadow-depth-format)
   (orthographic-shadow-layout
    :initform nil :accessor surface-technique-orthographic-shadow-layout)
   (orthographic-shadow-module
    :initform nil :accessor surface-technique-orthographic-shadow-module)
   (orthographic-shadow-pipeline
    :initform nil :accessor surface-technique-orthographic-shadow-pipeline))
  (:documentation
   "The immutable GPU technique shared by surface draws on one DEVICE.

It owns the exact bind-group layout, shader modules, and style pipelines, and
tracks every dependent SURFACE-FRAME-STATE until that state's owned resources
have all been released.  Mutable frame and scene buffers deliberately live in
those states, so several acquired frames can use this technique without
sharing mapped state.  OUTPUT-SPACE says whether its fragment programs own
presentation or return linear radiance to an enclosing frame owner."))

(defclass surface-frame-state ()
  ((technique :initarg :technique :reader surface-frame-state-technique)
   (uniform-buffer :initform nil :accessor surface-frame-state-uniform-buffer)
   (sites-buffer :initform nil :accessor surface-frame-state-sites-buffer)
   (cells-buffer :initform nil :accessor surface-frame-state-cells-buffer)
   (stocks-buffer :initform nil :accessor surface-frame-state-stocks-buffer)
   (slots-buffer :initform nil :accessor surface-frame-state-slots-buffer)
   (slots-capacity :initform 0 :accessor surface-frame-state-slots-capacity)
   (sites-capacity :initform 0 :accessor surface-frame-state-sites-capacity)
   (cells-capacity :initform 0 :accessor surface-frame-state-cells-capacity)
   (bind-group :initform nil :accessor surface-frame-state-bind-group)
   (orthographic-shadow-projector-buffer
    :initform nil
    :accessor surface-frame-state-orthographic-shadow-projector-buffer)
   (orthographic-shadow-bind-group
    :initform nil
    :accessor surface-frame-state-orthographic-shadow-bind-group)
   (retirements :initform nil :accessor surface-frame-state-retirements)
   (uploaded-scene :initform nil :accessor surface-frame-state-uploaded-scene)
   (uploaded-scene-revision :initform nil
                            :accessor surface-frame-state-uploaded-scene-revision)
   (last-scene-upload-kind :initform nil
                           :accessor surface-frame-state-last-scene-upload-kind)
   (last-scene-upload-bytes :initform 0
                            :accessor surface-frame-state-last-scene-upload-bytes)
   (last-scene-upload-writes :initform 0
                             :accessor surface-frame-state-last-scene-upload-writes))
  (:documentation
   "One acquired frame's mutable LUFT surface buffers and upload cursor.

Every instance owns a distinct uniform, stock, site, cell, and slot buffer.
When its technique has the optional orthographic shadow pass, it also owns
that frame's projector and shadow bind group.  Its uploaded revision is
therefore an independent consumer cursor into a SCENE's bounded change
history."))

(defstruct (surface-frame-retirement
             (:constructor make-surface-frame-retirement (label resource)))
  "One labelled GPU handle whose destruction failed after it left a cohort."
  label resource)

(define-condition surface-release-error (error)
  ((owner :initarg :owner :reader surface-release-error-owner)
   (failures :initarg :failures :reader surface-release-error-failures))
  (:documentation
   "Owned surface resources which could not all be released.")
  (:report
   (lambda (condition stream)
     (let ((failures (surface-release-error-failures condition)))
       (format stream "~D ~A release step~:P failed.~
                       ~:{~2%~S:~%  ~A~}"
               (length failures)
               (type-of (surface-release-error-owner condition))
               (mapcar (lambda (failure)
                         (list (car failure) (cdr failure)))
                       failures))))))

(define-condition surface-technique-construction-error (error)
  ((cause :initarg :cause
          :reader surface-technique-construction-cause)
   (technique :initarg :technique
              :reader surface-technique-construction-retry-owner))
  (:documentation
   "A failed technique construction whose partially released owner is live.")
  (:report
   (lambda (condition stream)
     (format stream "Surface technique construction failed: ~A~%~
                     The partial technique remains available for release retry."
             (surface-technique-construction-cause condition)))))

(defun call-surface-release-step (name function)
  "Attempt one named release, returning success and its labelled failure."
  (handler-case
      (progn
        (funcall function)
        (values t nil))
    (error (condition)
      (values nil (cons name condition)))))

(defun signal-surface-release-failures (owner failures)
  (when failures
    (error 'surface-release-error
           :owner owner :failures (nreverse failures)))
  (values))

(defmacro best-effort-surface-release (&body body)
  "Attempt unwind cleanup without replacing the error already in flight."
  `(handler-case
       (progn ,@body)
     (error () (values))))

(defun retire-surface-frame-resources (state labelled-resources)
  "Attempt LABELLED-RESOURCES and backlog every handle which fails.

Each element is (LABEL . RESOURCE).  Successful handles are forgotten;
failed ones become explicit STATE ownership and their labelled failures are
returned without signalling, so unwind cleanup cannot replace its cause."
  (let ((failures nil))
    (dolist (entry labelled-resources)
      (let ((label (car entry))
            (resource (cdr entry)))
        (when resource
          (multiple-value-bind (released-p failure)
              (call-surface-release-step label (lambda () (destroy resource)))
            (unless released-p
              (push (make-surface-frame-retirement label resource)
                    (surface-frame-state-retirements state))
              (push failure failures))))))
    (nreverse failures)))

(defun attempt-surface-frame-retirement-backlog (state)
  "Retry every abandoned generation owned by STATE and return failures."
  (let ((failures nil)
        (retained nil))
    (dolist (retirement (surface-frame-state-retirements state))
      (multiple-value-bind (released-p failure)
          (call-surface-release-step
           (surface-frame-retirement-label retirement)
           (lambda ()
             (destroy (surface-frame-retirement-resource retirement))))
        (unless released-p
          (push retirement retained)
          (push failure failures))))
    (setf (surface-frame-state-retirements state) (nreverse retained))
    (nreverse failures)))

(defun service-surface-frame-retirements (state)
  "Clear STATE's retirement debt before recording further GPU mutation."
  (signal-surface-release-failures
   state (attempt-surface-frame-retirement-backlog state))
  state)

(defun register-surface-frame-state (state)
  "Keep STATE reachable from its technique until complete release."
  (pushnew state
           (surface-technique-frame-states
            (surface-frame-state-technique state))
           :test #'eq)
  state)

(defun unregister-surface-frame-state (state)
  (let ((technique (surface-frame-state-technique state)))
    (setf (surface-technique-frame-states technique)
          (remove state (surface-technique-frame-states technique)
                  :test #'eq)))
  state)

(defun surface-frame-state-resources-live-p (state)
  (some #'identity
        (list (surface-frame-state-orthographic-shadow-bind-group state)
              (surface-frame-state-bind-group state)
              (surface-frame-state-sites-buffer state)
              (surface-frame-state-cells-buffer state)
              (surface-frame-state-stocks-buffer state)
              (surface-frame-state-slots-buffer state)
              (surface-frame-state-orthographic-shadow-projector-buffer state)
              (surface-frame-state-retirements state)
              (surface-frame-state-uniform-buffer state))))

(defun surface-technique-resources-live-p (technique)
  "Whether TECHNIQUE still owns any handle requiring release or retry."
  (some #'identity
        (list (surface-technique-frame-states technique)
              (surface-technique-orthographic-shadow-pipeline technique)
              (surface-technique-pipelines technique)
              (surface-technique-orthographic-shadow-module technique)
              (surface-technique-modules technique)
              (surface-technique-orthographic-shadow-layout technique)
              (surface-technique-layout technique))))

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   (owns-device-p :initarg :owns-device-p :initform nil
                  :accessor renderer-owns-device-p)
   (scene :initarg :scene :accessor renderer-scene)
   (camera :initarg :camera :accessor renderer-camera)
   (extent :initarg :extent :accessor renderer-extent)
   (color-format :initarg :color-format :reader renderer-color-format)
   (surfaces :initform nil :accessor renderer-surfaces)
   (sampler :initform nil :accessor renderer-sampler)
   (lens-layout :initform nil :accessor renderer-lens-layout)
   (temporal-layout :initform nil :accessor renderer-temporal-layout)
   (surface-technique :initform nil :accessor renderer-surface-technique)
   (surface-frame-state :initform nil :accessor renderer-surface-frame-state)
   (modules :initform nil :accessor renderer-modules)
   (pipelines :initform nil :accessor renderer-pipelines
              :documentation "A plist from post effect to its pipeline.")
   (pipeline-styles :initarg :pipeline-styles
                    :initform '(:flat :bevel :chamfer :paper)
                    :reader renderer-pipeline-styles
                    :documentation
                    "Surface styles whose shader modules and pipelines exist.")
   (effects :initarg :effects :initform '(:sky :lens)
            :reader renderer-effects
            :documentation
            "Optional passes whose shader modules and pipelines exist.")
   (style :initarg :style :initform :bevel :accessor renderer-style
          :documentation
          "Which pipeline draws: :FLAT, :BEVEL (rounded), :CHAMFER, or :PAPER.")
   (frame-index :initform 0 :accessor renderer-frame-index)
   (previous-view :initform nil :accessor renderer-previous-view)
   (history-index :initform 0 :accessor renderer-history-index)
   (history-valid-p :initform nil :accessor renderer-history-valid-p)
   (history-used-p :initform nil :accessor renderer-history-used-p)
   (history-key :initform nil :accessor renderer-history-key))
  (:documentation "GPU resources drawing one scene from one camera."))

;;; Compatibility readers keep the standalone renderer's established API and
;;; diagnostics while making the new ownership graph literal.

(defun renderer-layout (renderer)
  (surface-technique-layout (renderer-surface-technique renderer)))

(defun renderer-bind-group (renderer)
  (surface-frame-state-bind-group (renderer-surface-frame-state renderer)))

(defun renderer-uniform-buffer (renderer)
  (surface-frame-state-uniform-buffer
   (renderer-surface-frame-state renderer)))

(defun renderer-sites-buffer (renderer)
  (surface-frame-state-sites-buffer (renderer-surface-frame-state renderer)))

(defun renderer-cells-buffer (renderer)
  (surface-frame-state-cells-buffer (renderer-surface-frame-state renderer)))

(defun renderer-stocks-buffer (renderer)
  (surface-frame-state-stocks-buffer
   (renderer-surface-frame-state renderer)))

(defun renderer-slots-buffer (renderer)
  (surface-frame-state-slots-buffer (renderer-surface-frame-state renderer)))

(defun renderer-slots-capacity (renderer)
  (surface-frame-state-slots-capacity
   (renderer-surface-frame-state renderer)))

(defun renderer-sites-capacity (renderer)
  (surface-frame-state-sites-capacity
   (renderer-surface-frame-state renderer)))

(defun renderer-cells-capacity (renderer)
  (surface-frame-state-cells-capacity
   (renderer-surface-frame-state renderer)))

(defun renderer-uploaded-scene (renderer)
  (surface-frame-state-uploaded-scene
   (renderer-surface-frame-state renderer)))

(defun renderer-uploaded-scene-revision (renderer)
  (surface-frame-state-uploaded-scene-revision
   (renderer-surface-frame-state renderer)))

(defun renderer-last-scene-upload-kind (renderer)
  (surface-frame-state-last-scene-upload-kind
   (renderer-surface-frame-state renderer)))

(defun renderer-last-scene-upload-bytes (renderer)
  (surface-frame-state-last-scene-upload-bytes
   (renderer-surface-frame-state renderer)))

(defun renderer-last-scene-upload-writes (renderer)
  (surface-frame-state-last-scene-upload-writes
   (renderer-surface-frame-state renderer)))

(defgeneric temporal-resolve-kind (device)
  (:documentation
   "The temporal reconstruction implementation DEVICE gives Luft."))

(defmethod temporal-resolve-kind ((device gpu-device))
  (declare (ignore device))
  :shader)

#+darwin
(defmethod temporal-resolve-kind ((device metal-gpu-device))
  (declare (ignore device))
  :metalfx)

(defun renderer-temporal-resolve-kind (renderer)
  (and (renderer-effect-p renderer :taa)
       (temporal-resolve-kind (renderer-device renderer))))

(defun renderer-metalfx-temporal-p (renderer)
  (eq :metalfx (renderer-temporal-resolve-kind renderer)))

(defun renderer-shader-temporal-p (renderer)
  (eq :shader (renderer-temporal-resolve-kind renderer)))

(defun renderer-color-texture (renderer)
  (frame-surfaces-color-texture (renderer-surfaces renderer)))

(defun renderer-color-view (renderer)
  (frame-surfaces-color-view (renderer-surfaces renderer)))

(defun renderer-depth-texture (renderer)
  (frame-surfaces-depth-texture (renderer-surfaces renderer)))

(defun renderer-depth-view (renderer)
  (frame-surfaces-depth-view (renderer-surfaces renderer)))

(defun renderer-scene-texture (renderer)
  (frame-surfaces-scene-texture (renderer-surfaces renderer)))

(defun renderer-scene-view (renderer)
  (frame-surfaces-scene-view (renderer-surfaces renderer)))

(defun renderer-motion-texture (renderer)
  (frame-surfaces-motion-texture (renderer-surfaces renderer)))

(defun renderer-motion-view (renderer)
  (frame-surfaces-motion-view (renderer-surfaces renderer)))

(defun frame-uniform-size ()
  (let ((size (shader:shader-uniform-block-byte-size (shaders:frame-uniform-block))))
    (unless (= size (* 4 (length (frame-uniform-data (make-fly-camera) 1 1))))
      (error "Frame block is ~D bytes but the host packs ~D."
             size (* 4 (length (frame-uniform-data (make-fly-camera) 1 1)))))
    size))

(defun renderer-pipeline (renderer &optional (style (renderer-style renderer)))
  (or (getf (renderer-pipelines renderer) style)
      (and (renderer-surface-technique renderer)
           (getf (surface-technique-pipelines
                  (renderer-surface-technique renderer))
                 style))
      (error "Renderer has no ~S pipeline." style)))

(defun surface-technique-pipeline (technique style)
  "Return TECHNIQUE's STYLE pipeline, or report that it was not built."
  (or (getf (surface-technique-pipelines technique) style)
      (error "Surface technique has no ~S pipeline; it has ~S."
             style (surface-technique-pipeline-styles technique))))

(defun renderer-effect-p (renderer effect)
  (not (null (member effect (renderer-effects renderer)))))

(defmacro with-renderer-creation-step ((zone label) &body body)
  "Trace and synchronously log one cold LUFT driver transaction.

The BEGIN breadcrumb is forced to the image log before BODY enters the driver.
COMPLETE proves that it returned; INTERRUPTED means an ordinary non-local exit
unwound through Lisp.  If the process or GPU disappears, BEGIN deliberately
remains the last durable line."
  (let ((completed-p (gensym "COMPLETED-P")))
    `(with-cpu-trace-zone (,zone)
       (let ((,completed-p nil))
         (log-event :luft "begin ~A" ,label)
         (unwind-protect
              (multiple-value-prog1 (progn ,@body)
                (setf ,completed-p t)
                (log-event :luft "complete ~A" ,label))
           (unless ,completed-p
             (log-event :luft "interrupted ~A" ,label)))))))

(defun frame-surfaces-resources (surfaces)
  "Return SURFACES' dependents before their views and textures."
  (when surfaces
    (append (list (frame-surfaces-temporal-scaler surfaces))
            (coerce (frame-surfaces-temporal-bind-groups surfaces) 'list)
            (coerce (frame-surfaces-post-bind-groups surfaces) 'list)
            (list (frame-surfaces-motion-view surfaces))
            (coerce (frame-surfaces-history-views surfaces) 'list)
            (list (frame-surfaces-resolved-view surfaces))
            (list (frame-surfaces-scene-view surfaces)
                  (frame-surfaces-color-view surfaces)
                  (frame-surfaces-depth-view surfaces)
                  (frame-surfaces-motion-texture surfaces))
            (coerce (frame-surfaces-history-textures surfaces) 'list)
            (list (frame-surfaces-resolved-texture surfaces))
            (list (frame-surfaces-scene-texture surfaces)
                  (frame-surfaces-color-texture surfaces)
                  (frame-surfaces-depth-texture surfaces)))))

(defun destroy-frame-surfaces (surfaces)
  "Release one whole extent cohort of SURFACES."
  (dolist (resource (frame-surfaces-resources surfaces))
    (when resource (ignore-errors (destroy resource))))
  (values))

(defun create-post-bind-group (renderer source label)
  (create (renderer-device renderer)
          (make-bind-group-descriptor
           :label label
           :layout (renderer-lens-layout renderer)
           :entries
           `((:binding ,shaders:+scene-binding+ :resource ,source)
             (:binding ,shaders:+sampler-binding+
              :resource ,(renderer-sampler renderer))
             (:binding ,shaders:+lens-frame-binding+
              :resource ,(renderer-uniform-buffer renderer))))))

(defun create-temporal-bind-group (renderer surfaces history-view label)
  (create (renderer-device renderer)
          (make-bind-group-descriptor
           :label label
           :layout (renderer-temporal-layout renderer)
           :entries
           `((:binding ,shaders:+current-binding+
              :resource ,(frame-surfaces-scene-view surfaces))
             (:binding ,shaders:+motion-binding+
              :resource ,(frame-surfaces-motion-view surfaces))
             (:binding ,shaders:+history-binding+ :resource ,history-view)
             (:binding ,shaders:+temporal-sampler-binding+
              :resource ,(renderer-sampler renderer))
             (:binding ,shaders:+temporal-frame-binding+
              :resource ,(renderer-uniform-buffer renderer))))))

(defun make-frame-surfaces (renderer extent)
  "Assemble RENDERER's complete EXTENT-sized frame cohort transactionally."
  (let* ((device (renderer-device renderer))
         (temporal-p (renderer-effect-p renderer :taa))
         (temporal-kind (renderer-temporal-resolve-kind renderer))
         (shader-temporal-p (eq temporal-kind :shader))
         (metalfx-p (eq temporal-kind :metalfx))
         (post-p (or temporal-p (renderer-effect-p renderer :lens)))
         (scene-format (if temporal-p :rgba16-float
                           (renderer-color-format renderer)))
         (history-textures (if shader-temporal-p
                               (make-array 2 :initial-element nil) #()))
         (history-views (if shader-temporal-p
                            (make-array 2 :initial-element nil) #()))
         (temporal-bind-groups (if shader-temporal-p
                                   (make-array 2 :initial-element nil) #()))
         (post-bind-groups
           (if shader-temporal-p
               (make-array 2 :initial-element nil)
               (if post-p (make-array 1 :initial-element nil) #())))
         color color-view depth depth-view scene scene-view motion motion-view
         temporal-scaler resolved resolved-view
         surfaces (completed-p nil))
    (labels ((usage (&rest groups)
               (remove-duplicates (apply #'append groups)))
             (texture (label format usage)
               (create device
                       (make-texture-descriptor
                        :label label :size extent :dimensions :2d
                        :format format :usage usage)))
             (view (texture)
               (create device (make-texture-view-descriptor
                               :texture texture))))
      (unwind-protect
           (progn
             (when metalfx-p
               (setf temporal-scaler
                     (create
                      device
                      (make-temporal-scaler-descriptor
                       :label "Luft MetalFX temporal scaler"
                       :input-size extent :output-size extent))))
             (setf color (texture "luft frame color"
                                  (renderer-color-format renderer)
                                  '(:render-attachment :copy-src))
                   color-view (view color)
                   depth (texture "luft frame depth" :depth32-float
                                  (usage
                                   '(:render-attachment)
                                   (when temporal-scaler
                                     (gpu-temporal-scaler-depth-usage
                                      temporal-scaler))))
                   depth-view (view depth))
             (when post-p
               (setf scene (texture "luft current color" scene-format
                                    (usage
                                     '(:render-attachment :texture-binding)
                                     (when temporal-scaler
                                       (gpu-temporal-scaler-color-usage
                                        temporal-scaler))))
                     scene-view (view scene)))
             (when temporal-p
               (setf motion (texture "luft current motion" :rg16-float
                                     (usage
                                      '(:render-attachment :texture-binding)
                                      (when temporal-scaler
                                        (gpu-temporal-scaler-motion-usage
                                         temporal-scaler))))
                     motion-view (view motion))
               (when shader-temporal-p
                 (dotimes (index 2)
                   (setf (aref history-textures index)
                         (texture (format nil "luft history ~D" index)
                                  :rgba16-float
                                  '(:render-attachment :texture-binding
                                    :copy-dst))
                         (aref history-views index)
                         (view (aref history-textures index)))))
               (when metalfx-p
                 (setf resolved
                       (texture
                        "luft MetalFX resolved color" :rgba16-float
                        (usage
                         '(:texture-binding)
                         (gpu-temporal-scaler-output-usage temporal-scaler)))
                       resolved-view (view resolved))))
             ;; Publish the texture cohort locally before its bind groups are
             ;; made; they refer only to this candidate, never to the live one.
             (setf surfaces
                   (make-instance
                    'frame-surfaces :extent extent
                    :color-texture color :color-view color-view
                    :depth-texture depth :depth-view depth-view
                    :scene-texture scene :scene-view scene-view
                    :motion-texture motion :motion-view motion-view
                    :temporal-scaler temporal-scaler
                    :resolved-texture resolved :resolved-view resolved-view
                    :history-textures history-textures
                    :history-views history-views
                    :temporal-bind-groups temporal-bind-groups
                    :post-bind-groups post-bind-groups))
             (when shader-temporal-p
               (dotimes (write-index 2)
                 (let ((read-index (mod (1+ write-index) 2)))
                   (setf (aref temporal-bind-groups write-index)
                         (create-temporal-bind-group
                          renderer surfaces (aref history-views read-index)
                          (format nil "luft temporal bindings ~D" write-index))
                         (aref post-bind-groups write-index)
                         (create-post-bind-group
                          renderer (aref history-views write-index)
                          (format nil "luft history presentation ~D"
                                  write-index))))))
             (when metalfx-p
               (setf (aref post-bind-groups 0)
                     (create-post-bind-group
                      renderer resolved-view
                      "luft MetalFX presentation bindings")))
             (when (and post-p (not temporal-p))
               (setf (aref post-bind-groups 0)
                     (create-post-bind-group renderer scene-view
                                             "luft lens bindings")))
             (setf completed-p t)
             surfaces)
        (unless completed-p
          (if surfaces
              (destroy-frame-surfaces surfaces)
              (dolist (resource
                        (list temporal-scaler motion-view motion resolved-view
                              resolved scene-view scene color-view color
                              depth-view depth))
                (when resource (ignore-errors (destroy resource))))))))))

(zdefun (create-renderer-targets :zone :luft/create-renderer-targets) (renderer)
  (setf (renderer-surfaces renderer)
        (make-frame-surfaces renderer (renderer-extent renderer))))

(zdefun (ensure-renderer-extent :zone :luft/ensure-renderer-extent)
    (renderer extent)
  "Replace RENDERER's frame-sized targets when EXTENT has changed.

This runs inside the canvas frame callback, after drawable acquisition has
synchronized the presentation context's extent.  New resources are assembled
before publication; destroying the outgoing handles is safe while older GPU
work is in flight because the GPU abstraction defers their native teardown."
  (unless (equal extent (renderer-extent renderer))
    (log-event :luft "reframing ~{~D~^x~} to ~{~D~^x~}"
               (renderer-extent renderer) extent)
    (let* ((old (renderer-surfaces renderer))
           (new (make-frame-surfaces renderer extent)))
      (setf (renderer-extent renderer) extent
            (renderer-surfaces renderer) new
            (renderer-history-valid-p renderer) nil
            (renderer-history-used-p renderer) nil
            (renderer-history-key renderer) nil
            (renderer-previous-view renderer) nil
            (renderer-history-index renderer) 0)
      (destroy-frame-surfaces old)))
  renderer)

;;; ------------------------------------------------------------------------
;;; The surface chain becomes triangles by vertex pulling: no vertex buffer,
;;; one storage-buffer site per face, and a style-dependent number of vertex
;;; shader invocations per site.

(defparameter *surface-styles*
  '(:flat :bevel :chamfer :paper :stock :field :soft :ink :clay)
  "Surface styles Luft can draw, in the order a menu would list them.")

(defun default-renderer-effects ()
  "The native atelier's full post stack.

Vulkan resolves through Luft's inspectable shader path; Metal resolves the
same jittered color, depth, and motion contract through Metal4FX.  #D7GZA6"
  '(:sky :lens :taa))

(defun create-renderer-module (renderer zone label code)
  "Create a shader module from CODE and publish it as one of RENDERER's.

Ownership is published as each driver call succeeds so MAKE-RENDERER's
unwind cleanup sees partial work."
  (let ((module (with-renderer-creation-step (zone label)
                  (create (renderer-device renderer)
                          (make-shader-module-descriptor
                           :label label
                           :language :mathematical
                           :code code)))))
    (push module (renderer-modules renderer))
    module))

(defun install-renderer-pipeline (renderer name zone label descriptor)
  "Create DESCRIPTOR's pipeline and install it as RENDERER's NAME pipeline."
  (setf (getf (renderer-pipelines renderer) name)
        (with-renderer-creation-step (zone label)
          (create (renderer-device renderer) descriptor))))

(defun create-surface-technique-module (technique zone label code)
  "Create a shader module and publish it into TECHNIQUE immediately."
  (let ((module (with-renderer-creation-step (zone label)
                  (create (surface-technique-device technique)
                          (make-shader-module-descriptor
                           :label label
                           :language :mathematical
                           :code code)))))
    (push module (surface-technique-modules technique))
    module))

(defun install-surface-technique-pipeline
    (technique name zone label descriptor)
  (setf (getf (surface-technique-pipelines technique) name)
        (with-renderer-creation-step (zone label)
          (create (surface-technique-device technique) descriptor))))

(defun surface-depth-state ()
  '(:format :depth32-float :depth-write-enabled t :depth-compare :less))

(defun background-depth-state ()
  "The sky's depth: written by nothing, tested against nothing, so the pass
may draw it first and the world still covers it."
  '(:format :depth32-float :depth-write-enabled nil :depth-compare :always))

(defun surface-target-formats (renderer)
  (if (renderer-effect-p renderer :taa)
      '(:rgba16-float :rg16-float)
      (list (renderer-color-format renderer))))

(defun fragment-stage (module target-formats)
  `(:module ,module
    :targets ,(mapcar (lambda (format) `(:format ,format)) target-formats)))

(defun create-surface-fragment-modules (technique)
  "Create TECHNIQUE's style fragment modules.

The values are surface, chamfer, paper, field, ink, stock, and clay, each NIL
when no selected style needs it."
  (let ((styles (surface-technique-pipeline-styles technique))
        (temporal-p (surface-technique-temporal-p technique))
        (output-space (surface-technique-output-space technique)))
    (values
     (when (intersection styles '(:flat :bevel))
       (create-surface-technique-module
        technique :luft/shader/surface-fragment "luft surface fragment"
        (if temporal-p
            (shaders:temporal-surface-fragment-shader)
            (shaders:surface-fragment-shader))))
     (when (member :chamfer styles)
       (create-surface-technique-module
        technique :luft/shader/chamfer-fragment "luft chamfer fragment"
        (if temporal-p
            (shaders:temporal-chamfer-fragment-shader)
            (shaders:chamfer-fragment-shader))))
     (when (member :paper styles)
       (create-surface-technique-module
        technique :luft/shader/paper-fragment "luft paper fragment"
        (if temporal-p
            (shaders:temporal-paper-fragment-shader)
            (shaders:paper-fragment-shader))))
     (when (intersection styles '(:field :soft))
       (create-surface-technique-module
        technique :luft/shader/field-fragment "luft field fragment"
        (if temporal-p
            (shaders:temporal-field-fragment-shader)
            (shaders:field-fragment-shader))))
     (when (member :ink styles)
       (create-surface-technique-module
        technique :luft/shader/ink-fragment "luft ink fragment"
        (if temporal-p
            (shaders:temporal-ink-fragment-shader)
            (shaders:ink-fragment-shader))))
     (when (member :stock styles)
       (create-surface-technique-module
        technique :luft/shader/stock-fragment "luft stock fragment"
        (cond
          (temporal-p
           (shaders:temporal-stock-fragment-shader))
          ((eq :linear output-space)
           (shaders:linear-stock-fragment-shader))
          (t
           (shaders:stock-fragment-shader)))))
     (when (member :clay styles)
       (create-surface-technique-module
        technique :luft/shader/clay-fragment "luft clay fragment"
        (if temporal-p
            (shaders:temporal-clay-fragment-shader)
            (shaders:clay-fragment-shader)))))))

(defun create-surface-technique-pipelines (technique)
  "Create TECHNIQUE's selected vertex-pulling surface pipelines."
  (let ((styles (surface-technique-pipeline-styles technique))
        surface bevel chamfer stock field clay)
    (when (intersection styles '(:flat :soft :ink))
      (setf surface (create-surface-technique-module
                     technique :luft/shader/surface-vertex
                     "luft surface vertex" (shaders:surface-vertex-shader))))
    (when (member :bevel styles)
      (setf bevel (create-surface-technique-module
                   technique :luft/shader/bevel-vertex
                   "luft bevel vertex" (shaders:bevel-vertex-shader))))
    (when (intersection styles '(:chamfer :paper))
      (setf chamfer (create-surface-technique-module
                     technique :luft/shader/chamfer-vertex
                     "luft chamfer vertex" (shaders:chamfer-vertex-shader))))
    ;; The stock has a chamfer stage of its own: one width per site, and
    ;; the lattice bent between the site rules and the projection.
    (when (member :stock styles)
      (setf stock (create-surface-technique-module
                   technique :luft/shader/stock-vertex
                   "luft stock vertex"
                   (shaders:stock-vertex-shader))))
    (when (member :field styles)
      (setf field (create-surface-technique-module
                   technique :luft/shader/field-vertex
                   "luft field vertex"
                   (shaders:field-vertex-shader))))
    (when (member :clay styles)
      (setf clay (create-surface-technique-module
                  technique :luft/shader/clay-vertex
                  "luft clay vertex"
                  (shaders:clay-vertex-shader))))
    (multiple-value-bind (fragment chamfer-fragment paper-fragment
                          field-fragment ink-fragment stock-fragment
                          clay-fragment)
        (create-surface-fragment-modules technique)
      (flet ((pipeline (name zone label vertex-module fragment-module
                        &key (depth (surface-depth-state)))
               (install-surface-technique-pipeline
                technique name zone label
                (make-render-pipeline-descriptor
                 :label label
                 :layout (surface-technique-layout technique)
                 :vertex `(:module ,vertex-module)
                 :fragment
                 (fragment-stage fragment-module
                                 (surface-technique-target-formats technique))
                 :primitive '(:topology :triangle-list)
                 :depth-stencil depth))))
        (when (member :flat styles)
          (pipeline :flat :luft/pipeline/flat "luft surface pipeline"
                    surface fragment))
        (when (member :bevel styles)
          (pipeline :bevel :luft/pipeline/bevel "luft bevel pipeline"
                    bevel fragment))
        (when (member :chamfer styles)
          (pipeline :chamfer :luft/pipeline/chamfer "luft chamfer pipeline"
                    chamfer chamfer-fragment))
        (when (member :paper styles)
          (pipeline :paper :luft/pipeline/paper "luft paper pipeline"
                    chamfer paper-fragment))
        ;; The stock draws the chamfered geometry too: its materials are
        ;; about what a planed arris does with the light.
        (when (member :stock styles)
          (pipeline :stock :luft/pipeline/stock "luft stock pipeline"
                    stock stock-fragment))
        (when (member :field styles)
          (pipeline :field :luft/pipeline/field "luft field pipeline"
                    field field-fragment))
        ;; Soft: the flat quads under the field's shading, rounding as
        ;; light alone.
        (when (member :soft styles)
          (pipeline :soft :luft/pipeline/soft "luft soft pipeline"
                    surface field-fragment))
        (when (member :ink styles)
          (pipeline :ink :luft/pipeline/ink "luft ink pipeline"
                    surface ink-fragment))
        ;; Clay: the grid projected onto the rounded-cell union, under
        ;; its own true-distance light.
        (when (member :clay styles)
          (pipeline :clay :luft/pipeline/clay "luft clay pipeline"
                    clay clay-fragment))))))

(defun create-surface-technique-layout (technique)
  (setf (surface-technique-layout technique)
        (with-renderer-creation-step
            (:luft/layout/surface "luft surface layout")
          (create
           (surface-technique-device technique)
           (make-bind-group-layout-descriptor
            :label "luft surface layout"
            :entries
            `((:binding ,shaders:+frame-binding+ :type :uniform-buffer)
              (:binding ,shaders:+sites-binding+ :type :storage-buffer)
              (:binding ,shaders:+cells-binding+ :type :storage-buffer)
              (:binding ,shaders:+stocks-binding+ :type :storage-buffer)
              (:binding ,shaders:+slots-binding+ :type :storage-buffer)))))))

(defun create-surface-technique-orthographic-shadow (technique)
  "Create and publish TECHNIQUE's optional stock-shaped depth sub-technique."
  (when (surface-technique-orthographic-shadow-depth-format technique)
    (let ((device (surface-technique-device technique)))
      (setf (surface-technique-orthographic-shadow-layout technique)
            (with-renderer-creation-step
                (:luft/layout/orthographic-shadow
                 "luft orthographic shadow layout")
              (create
               device
               (make-bind-group-layout-descriptor
                :label "luft orthographic shadow layout"
                :entries
                `((:binding ,shaders:+frame-binding+ :type :uniform-buffer)
                  (:binding ,shaders:+sites-binding+ :type :storage-buffer)
                  (:binding ,shaders:+cells-binding+ :type :storage-buffer)
                  (:binding ,shaders:+stocks-binding+ :type :storage-buffer)
                  (:binding ,shaders:+slots-binding+ :type :storage-buffer)
                  (:binding ,shaders:+orthographic-shadow-projector-binding+
                   :type :uniform-buffer))))))
      (setf (surface-technique-orthographic-shadow-module technique)
            (with-renderer-creation-step
                (:luft/shader/orthographic-stock-shadow-vertex
                 "luft orthographic stock shadow vertex")
              (create
               device
               (make-shader-module-descriptor
                :label "luft orthographic stock shadow vertex"
                :language :mathematical
                :code
                (shaders:orthographic-stock-shadow-vertex-shader)))))
      (setf (surface-technique-orthographic-shadow-pipeline technique)
            (with-renderer-creation-step
                (:luft/pipeline/orthographic-stock-shadow
                 "luft orthographic stock shadow pipeline")
              (create
               device
               (make-render-pipeline-descriptor
                :label "luft orthographic stock shadow pipeline"
                :layout
                (surface-technique-orthographic-shadow-layout technique)
                :vertex
                `(:module
                  ,(surface-technique-orthographic-shadow-module technique))
                :fragment nil
                :primitive '(:topology :triangle-list)
                :depth-stencil
                `(:format
                  ,(surface-technique-orthographic-shadow-depth-format technique)
                  :depth-write-enabled t :depth-compare :less))))))))

(zdefun (make-surface-technique :zone :luft/make-surface-technique)
    (device &key
              (pipeline-styles *surface-styles*)
              (target-formats '(:rgba8-unorm-srgb))
              temporal-p
              (output-space :presented)
              orthographic-shadow-depth-format)
  "Build a shareable LUFT surface technique for DEVICE.

PIPELINE-STYLES selects the vertex-pulled styles.  TARGET-FORMATS are the
exact render-pass color targets; TEMPORAL-P selects the matching two-output
fragment variants.  OUTPUT-SPACE is :PRESENTED when LUFT owns tone mapping
and fog, or :LINEAR when an enclosing HDR frame owns presentation.  The
currently exact linear contract is one non-temporal :STOCK pipeline.  The
optional ORTHOGRAPHIC-SHADOW-DEPTH-FORMAT adds a vertex-only depth pipeline
over the same shaped stock geometry.  The caller owns TECHNIQUE and may
create any number of independent SURFACE-FRAME-STATE instances from it."
  (let ((foreign (set-difference pipeline-styles *surface-styles*)))
    (when foreign
      (error "Luft cannot draw ~S; its surface styles are ~S."
             foreign *surface-styles*)))
  (unless (member output-space '(:presented :linear))
    (error "A Luft surface technique output space is :PRESENTED or :LINEAR, not ~S."
           output-space))
  (when (eq :linear output-space)
    (unless (equal pipeline-styles '(:stock))
      (error "Linear Luft output currently requires exactly the :STOCK pipeline, not ~S."
             pipeline-styles))
    (when temporal-p
      (error "Linear Luft output is not yet a temporal surface technique.")))
  (when (and temporal-p (/= 2 (length target-formats)))
    (error "A temporal surface technique needs color and motion targets, not ~S."
           target-formats))
  (let ((technique (make-instance 'surface-technique
                                  :device device
                                  :pipeline-styles (copy-list pipeline-styles)
                                  :target-formats (copy-list target-formats)
                                  :temporal-p temporal-p
                                  :output-space output-space
                                  :orthographic-shadow-depth-format
                                  orthographic-shadow-depth-format)))
    (handler-case
        (progn
          (create-surface-technique-layout technique)
          (create-surface-technique-pipelines technique)
          (create-surface-technique-orthographic-shadow technique)
          technique)
      (error (cause)
        (best-effort-surface-release
          (destroy-surface-technique technique))
        (if (surface-technique-resources-live-p technique)
            (error 'surface-technique-construction-error
                   :cause cause :technique technique)
            (error cause))))))

(defun destroy-surface-technique (technique)
  "Release TECHNIQUE and any dependent frame states still registered with it.

Every frame state is attempted first.  A state which retains any failed
member prevents the technique resources it names from being destroyed.  Each
successful technique member is forgotten immediately; failed handles remain
installed for a later retry, and all failures from one tier are reported
together."
  (let ((failures nil))
    (loop for state in (copy-list
                        (surface-technique-frame-states technique))
          for index from 0
          do (multiple-value-bind (released-p failure)
                 (call-surface-release-step
                  (list :frame-state index)
                  (lambda () (destroy-surface-frame-state state)))
               (unless released-p (push failure failures))))
    ;; A failed state still names this layout.  Do not invalidate any
    ;; technique member until every such dependent can forget its handles.
    (when (surface-technique-frame-states technique)
      (signal-surface-release-failures technique failures))
    (let ((pipeline
            (surface-technique-orthographic-shadow-pipeline technique)))
      (when pipeline
        (multiple-value-bind (released-p failure)
            (call-surface-release-step
             :orthographic-shadow-pipeline (lambda () (destroy pipeline)))
          (if released-p
              (setf (surface-technique-orthographic-shadow-pipeline technique)
                    nil)
              (push failure failures)))))
    (let ((retained nil))
      (loop for (style pipeline)
              on (surface-technique-pipelines technique) by #'cddr
            when pipeline
              do (multiple-value-bind (released-p failure)
                     (call-surface-release-step
                      (list :pipeline style) (lambda () (destroy pipeline)))
                   (unless released-p
                     (push failure failures)
                     (push (cons style pipeline) retained))))
      (setf (surface-technique-pipelines technique)
            (loop for (style . pipeline) in (nreverse retained)
                  append (list style pipeline))))
    (let ((retained nil))
      (loop for module in (surface-technique-modules technique)
            for index from 0
            do (multiple-value-bind (released-p failure)
                   (call-surface-release-step
                    (list :module index) (lambda () (destroy module)))
                 (unless released-p
                   (push failure failures)
                   (push module retained))))
      (setf (surface-technique-modules technique) (nreverse retained)))
    (let ((module
            (surface-technique-orthographic-shadow-module technique)))
      (when module
        (multiple-value-bind (released-p failure)
            (call-surface-release-step
             :orthographic-shadow-module (lambda () (destroy module)))
          (if released-p
              (setf (surface-technique-orthographic-shadow-module technique)
                    nil)
              (push failure failures)))))
    (let ((layout
            (surface-technique-orthographic-shadow-layout technique)))
      (when layout
        (multiple-value-bind (released-p failure)
            (call-surface-release-step
             :orthographic-shadow-layout (lambda () (destroy layout)))
          (if released-p
              (setf (surface-technique-orthographic-shadow-layout technique)
                    nil)
              (push failure failures)))))
    (let ((layout (surface-technique-layout technique)))
      (when layout
        (multiple-value-bind (released-p failure)
            (call-surface-release-step :layout (lambda () (destroy layout)))
          (if released-p
              (setf (surface-technique-layout technique) nil)
              (push failure failures)))))
    (signal-surface-release-failures technique failures)))

(defun create-renderer-effect-pipelines (renderer)
  "Create only the standalone renderer's sky, lens, and temporal passes."
  (when (renderer-effects renderer)
    (let* ((temporal-p (renderer-effect-p renderer :taa))
           (screen
             (create-renderer-module renderer :luft/shader/sky-vertex
                                     "luft sky vertex"
                                     (shaders:sky-vertex-shader)))
           (sky-fragment
             (when (renderer-effect-p renderer :sky)
               (create-renderer-module
                renderer :luft/shader/sky-fragment "luft sky fragment"
                (if temporal-p
                    (shaders:temporal-sky-fragment-shader)
                    (shaders:sky-fragment-shader)))))
           (lens-fragment
             (when (renderer-effect-p renderer :lens)
               (create-renderer-module
                renderer :luft/shader/lens-fragment "luft lens fragment"
                (shaders:lens-fragment-shader))))
           (temporal-fragment
             (when (renderer-shader-temporal-p renderer)
               (create-renderer-module
                renderer :luft/shader/temporal-fragment
                "luft temporal resolve fragment"
                (shaders:temporal-resolve-fragment-shader))))
           (present-fragment
             (when temporal-p
               (create-renderer-module
                renderer :luft/shader/present-fragment
                "luft presentation fragment"
                (shaders:present-fragment-shader)))))
      (flet ((pipeline (name zone label fragment layout target-formats
                        &optional depth)
               (install-renderer-pipeline
                renderer name zone label
                (make-render-pipeline-descriptor
                 :label label :layout layout
                 :vertex `(:module ,screen)
                 :fragment (fragment-stage fragment target-formats)
                 :primitive '(:topology :triangle-list)
                 :depth-stencil depth))))
        (when sky-fragment
          (pipeline :sky :luft/pipeline/sky "luft sky pipeline"
                    sky-fragment (renderer-layout renderer)
                    (surface-target-formats renderer)
                    (background-depth-state)))
        (when lens-fragment
          (pipeline :lens :luft/pipeline/lens "luft lens pipeline"
                    lens-fragment (renderer-lens-layout renderer)
                    (list (renderer-color-format renderer))))
        (when temporal-fragment
          (pipeline :taa :luft/pipeline/taa "luft temporal resolve pipeline"
                    temporal-fragment (renderer-temporal-layout renderer)
                    '(:rgba16-float)))
        (when present-fragment
          (pipeline :present :luft/pipeline/present
                    "luft presentation pipeline"
                    present-fragment (renderer-lens-layout renderer)
                    (list (renderer-color-format renderer))))))))

(defun draw-surface (pass scene style)
  "Record one vertex-pulled draw for each nonempty surface chunk of SCENE."
  (let ((vertices-per-face (shaders:surface-vertices-per-face style)))
    (loop for chunk across (scene-surface-chunks scene)
          when (and chunk (plusp (surface-chunk-count chunk)))
            do (draw pass
                     (* vertices-per-face (surface-chunk-count chunk))
                     1
                     (* vertices-per-face +surface-chunk-capacity+
                        (surface-chunk-page chunk))))))

(defun draw-screen (pass)
  "Record one triangle covering the screen."
  (draw pass 3))

;;; ------------------------------------------------------------------------
;;; Creating the renderer's pipelines

(zdefun (create-renderer-layouts :zone :luft/create-renderer-layouts) (renderer)
  (let ((device (renderer-device renderer)))
    (setf (renderer-lens-layout renderer)
          (with-renderer-creation-step
              (:luft/layout/lens "luft lens layout")
            (create
             device
             (make-bind-group-layout-descriptor
              :label "luft lens layout"
              :entries `((:binding ,shaders:+scene-binding+ :type :texture)
                         (:binding ,shaders:+sampler-binding+ :type :sampler)
                         (:binding ,shaders:+lens-frame-binding+
                          :type :uniform-buffer))))))
    (when (renderer-shader-temporal-p renderer)
      (setf (renderer-temporal-layout renderer)
            (with-renderer-creation-step
                (:luft/layout/temporal "luft temporal layout")
              (create
               device
               (make-bind-group-layout-descriptor
                :label "luft temporal layout"
                :entries
                `((:binding ,shaders:+current-binding+ :type :texture)
                  (:binding ,shaders:+motion-binding+ :type :texture)
                  (:binding ,shaders:+history-binding+ :type :texture)
                  (:binding ,shaders:+temporal-sampler-binding+
                   :type :sampler)
                  (:binding ,shaders:+temporal-frame-binding+
                   :type :uniform-buffer)))))))))

(zdefun (create-renderer-pipeline :zone :luft/create-renderer-pipeline) (renderer)
  (handler-case
      (setf (renderer-surface-technique renderer)
            (make-surface-technique
             (renderer-device renderer)
             :pipeline-styles (renderer-pipeline-styles renderer)
             :target-formats (surface-target-formats renderer)
             :temporal-p (renderer-effect-p renderer :taa)))
    (surface-technique-construction-error (condition)
      ;; Publish the partial owner before MAKE-RENDERER's unwind retries it.
      (setf (renderer-surface-technique renderer)
            (surface-technique-construction-retry-owner condition))
      (error condition)))
  (create-renderer-effect-pipelines renderer)
  renderer)

(zdefun (make-renderer :zone :luft/make-renderer)
    (&key scene camera device
          (provider *gpu-provider*)
          (width 1280) (height 800)
          (color-format :rgba8-unorm-srgb)
          (style :bevel)
          (pipeline-styles *surface-styles*)
          (effects (default-renderer-effects)))
  "Create every GPU object needed to draw SCENE from CAMERA at WIDTH by HEIGHT.

Sites are pulled from storage by vertex shaders on every supported device.
STYLE is :FLAT, :BEVEL (rounded), :CHAMFER (subtle planar crease
relief), or :PAPER (the chamfered geometry in a matte, toothed material), and
may be changed later to a member of PIPELINE-STYLES, which defaults to every
Luft style.  Only those surface pipelines and the optional :SKY, :LENS, and
:TAA EFFECTS are created; NIL/NIL is a clear-only renderer useful
for reducing a suspect GPU frame to its presentation core.  Without DEVICE,
one is requested from PROVIDER and owned by the renderer."
  (unless (or (null pipeline-styles) (member style pipeline-styles))
    (error "Renderer style ~S is absent from PIPELINE-STYLES ~S."
           style pipeline-styles))
  (let ((foreign (set-difference pipeline-styles *surface-styles*)))
    (when foreign
      (error "Luft cannot draw ~S; its surface styles are ~S."
             foreign *surface-styles*)))
  (let ((foreign (set-difference effects '(:sky :lens :taa))))
    (when foreign
      (error "Luft has no ~S effects; choose from :SKY, :LENS, and :TAA."
             foreign)))
  (let* ((owns-device-p (null device))
         (device (or device
                     (request-gpu-device
                      provider (make-device-descriptor :label "luft atelier"))))
         (renderer (make-instance 'renderer
                                  :device device :owns-device-p owns-device-p
                                  :scene scene :camera camera
                                  :extent (list width height)
                                  :color-format color-format
                                  :style style
                                  :pipeline-styles pipeline-styles
                                  :effects effects))
         (completed-p nil))
    (unwind-protect
         (progn
           (setf (renderer-sampler renderer)
                 (create device
                         (make-sampler-descriptor
                          :label "luft frame sampler"
                          :mag-filter :linear :min-filter :linear)))
           (create-renderer-layouts renderer)
           (create-renderer-pipeline renderer)
           (setf (renderer-surface-frame-state renderer)
                 (make-surface-frame-state
                  (renderer-surface-technique renderer) :scene scene))
           (create-renderer-targets renderer)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (best-effort-surface-release
          (destroy-renderer renderer))))))

(defun destroy-renderer (renderer)
  "Attempt every GPU owner of RENDERER and retain failed surface ownership."
  ;; Tear dependents down before what they reference.  Backend retirement is
  ;; deferred while work is in flight, but the Lisp ownership graph should
  ;; still say exactly which generation is live.
  (let ((failures nil))
    (destroy-frame-surfaces (renderer-surfaces renderer))
    (loop for (nil pipeline) on (renderer-pipelines renderer) by #'cddr
          when pipeline do (ignore-errors (destroy pipeline)))
    (dolist (module (renderer-modules renderer))
      (ignore-errors (destroy module)))
    (dolist (layout (list (renderer-temporal-layout renderer)
                          (renderer-lens-layout renderer)))
      (when layout (ignore-errors (destroy layout))))
    (when (renderer-sampler renderer)
      (ignore-errors (destroy (renderer-sampler renderer))))
    (let ((state (renderer-surface-frame-state renderer)))
      (when state
        (multiple-value-bind (released-p failure)
            (call-surface-release-step
             :surface-frame-state
             (lambda () (destroy-surface-frame-state state)))
          (if released-p
              (setf (renderer-surface-frame-state renderer) nil)
              (push failure failures)))))
    ;; Even when the direct state step failed, the technique remains its
    ;; durable retry owner and gets a chance to finish it in this same pass.
    (let ((technique (renderer-surface-technique renderer)))
      (when technique
        (multiple-value-bind (released-p failure)
            (call-surface-release-step
             :surface-technique
             (lambda () (destroy-surface-technique technique)))
          (if released-p
              (setf (renderer-surface-technique renderer) nil)
              (push failure failures)))))
    ;; A successful technique retry may have drained the state whose direct
    ;; step failed.  Forget that empty wrapper, but never a retryable handle.
    (let ((state (renderer-surface-frame-state renderer)))
      (when (and state (not (surface-frame-state-resources-live-p state)))
        (setf (renderer-surface-frame-state renderer) nil)))
    (setf (renderer-pipelines renderer) nil
          (renderer-lens-layout renderer) nil
          (renderer-temporal-layout renderer) nil
          (renderer-modules renderer) nil
          (renderer-surfaces renderer) nil
          (renderer-sampler renderer) nil)
    ;; The device is the ultimate owner.  A persistent surface failure keeps
    ;; it live; a failed device release likewise remains explicitly retryable.
    (when (and (renderer-owns-device-p renderer)
               (null (renderer-surface-frame-state renderer))
               (null (renderer-surface-technique renderer)))
      (multiple-value-bind (released-p failure)
          (call-surface-release-step
           :device (lambda () (destroy (renderer-device renderer))))
        (if released-p
            (setf (renderer-owns-device-p renderer) nil)
            (push failure failures))))
    (signal-surface-release-failures renderer failures)))

(defun make-surface-frame-state (technique &key scene)
  "Create one independent mutable frame state for TECHNIQUE.

When SCENE is supplied it is synchronized before publication.  No buffer is
borrowed from another state, including the per-frame uniform and stock table."
  (let ((state (make-instance 'surface-frame-state :technique technique))
        (completed-p nil))
    ;; Registration begins before allocation: if construction and its cleanup
    ;; both fail, the technique remains the retry owner of the partial state.
    (register-surface-frame-state state)
    (unwind-protect
         (progn
           (let ((device (surface-technique-device technique)))
             (setf (surface-frame-state-uniform-buffer state)
                   (create device
                           (make-buffer-descriptor
                            :label "luft frame block"
                            :size (frame-uniform-size)
                            :usage '(:uniform)))
                   (surface-frame-state-stocks-buffer state)
                   (create device
                           (make-buffer-descriptor
                            :label "luft stock table"
                            :size (* 4 4 shaders:+stock-lanes+
                                     shaders:+stock-slots+)
                            :usage '(:storage))))
             (when
                 (surface-technique-orthographic-shadow-depth-format technique)
               (setf
                (surface-frame-state-orthographic-shadow-projector-buffer state)
                (create
                 device
                 (make-buffer-descriptor
                  :label "luft orthographic shadow projector"
                  :size (* 4 4 4)
                  :usage '(:uniform))))))
           (when scene (synchronize-surface-frame-state state scene))
           (setf completed-p t)
           state)
      (unless completed-p
        (best-effort-surface-release
          (destroy-surface-frame-state state))))))

(defun destroy-surface-frame-state (state)
  "Attempt every resource owned by STATE and retain only failed handles.

The state remains registered with its technique until every member is gone,
so a failed release can be retried even after an enclosing frame cache has
forgotten the state."
  (let ((failures nil))
    (dolist (failure (attempt-surface-frame-retirement-backlog state))
      (push failure failures))
    (labels ((release (name reader writer)
               (let ((resource (funcall reader state)))
                 (when resource
                   (multiple-value-bind (released-p failure)
                       (call-surface-release-step
                        name (lambda () (destroy resource)))
                     (if released-p
                         (funcall writer nil state)
                         (push failure failures)))))))
      (release :orthographic-shadow-bind-group
               #'surface-frame-state-orthographic-shadow-bind-group
               #'(setf surface-frame-state-orthographic-shadow-bind-group))
      (release :bind-group #'surface-frame-state-bind-group
               #'(setf surface-frame-state-bind-group))
      (release :sites-buffer #'surface-frame-state-sites-buffer
               #'(setf surface-frame-state-sites-buffer))
      (release :cells-buffer #'surface-frame-state-cells-buffer
               #'(setf surface-frame-state-cells-buffer))
      (release :stocks-buffer #'surface-frame-state-stocks-buffer
               #'(setf surface-frame-state-stocks-buffer))
      (release :slots-buffer #'surface-frame-state-slots-buffer
               #'(setf surface-frame-state-slots-buffer))
      (release :orthographic-shadow-projector-buffer
               #'surface-frame-state-orthographic-shadow-projector-buffer
               #'(setf
                  surface-frame-state-orthographic-shadow-projector-buffer))
      (release :uniform-buffer #'surface-frame-state-uniform-buffer
               #'(setf surface-frame-state-uniform-buffer)))
    (unless (surface-frame-state-resources-live-p state)
      (setf (surface-frame-state-sites-capacity state) 0
            (surface-frame-state-cells-capacity state) 0
            (surface-frame-state-slots-capacity state) 0
            (surface-frame-state-uploaded-scene state) nil
            (surface-frame-state-uploaded-scene-revision state) nil
            (surface-frame-state-last-scene-upload-kind state) nil
            (surface-frame-state-last-scene-upload-bytes state) 0
            (surface-frame-state-last-scene-upload-writes state) 0)
      (unregister-surface-frame-state state))
    (signal-surface-release-failures state failures)))

(defun storage-buffer-candidate (state capacity needed label)
  "Create one unpublished full-upload buffer with reusable CAPACITY."
  (let ((candidate-capacity
          (if (> needed capacity)
              (max needed (* 2 capacity))
              capacity)))
    (values (create (surface-technique-device
                     (surface-frame-state-technique state))
                    (make-buffer-descriptor
                     :label label :size candidate-capacity
                     :usage '(:storage)))
            candidate-capacity)))

(defun create-surface-bind-group
    (state sites-buffer cells-buffer slots-buffer)
  (create
   (surface-technique-device (surface-frame-state-technique state))
   (make-bind-group-descriptor
    :label "luft surface bindings"
    :layout (surface-technique-layout
             (surface-frame-state-technique state))
    :entries
    `((:binding ,shaders:+frame-binding+
       :resource ,(surface-frame-state-uniform-buffer state))
      (:binding ,shaders:+sites-binding+ :resource ,sites-buffer)
      (:binding ,shaders:+cells-binding+ :resource ,cells-buffer)
      (:binding ,shaders:+stocks-binding+
       :resource ,(surface-frame-state-stocks-buffer state))
      (:binding ,shaders:+slots-binding+ :resource ,slots-buffer)))))

(defun create-surface-orthographic-shadow-bind-group
    (state sites-buffer cells-buffer slots-buffer)
  "Create the shadow view of one complete surface-buffer generation."
  (let ((technique (surface-frame-state-technique state)))
    (create
     (surface-technique-device technique)
     (make-bind-group-descriptor
      :label "luft orthographic shadow bindings"
      :layout (surface-technique-orthographic-shadow-layout technique)
      :entries
      `((:binding ,shaders:+frame-binding+
         :resource ,(surface-frame-state-uniform-buffer state))
        (:binding ,shaders:+sites-binding+ :resource ,sites-buffer)
        (:binding ,shaders:+cells-binding+ :resource ,cells-buffer)
        (:binding ,shaders:+stocks-binding+
         :resource ,(surface-frame-state-stocks-buffer state))
        (:binding ,shaders:+slots-binding+ :resource ,slots-buffer)
        (:binding ,shaders:+orthographic-shadow-projector-binding+
         :resource
         ,(surface-frame-state-orthographic-shadow-projector-buffer state)))))))

(zdefun (upload-surface-scene :zone :luft/upload-scene
                       :value (luft:chain-count (scene-surface scene)))
    (state scene)
  "Upload SCENE into an unpublished cohort, then publish it coherently.

Every full upload is copy-on-publish, even when the current buffers have
enough capacity.  Cross-scene and history-gap writes can therefore fail
without modifying the generation which STATE still advertises."
  (let* ((sites (scene-site-pages scene))
         (sites-needed (* 8 (length sites)))
         (cells-needed (* 4 (length (scene-cell-bits scene))))
         (slots-needed (* 4 (length (scene-slot-words scene))))
         (old-sites (surface-frame-state-sites-buffer state))
         (old-cells (surface-frame-state-cells-buffer state))
         (old-slots (surface-frame-state-slots-buffer state))
         (old-bind-group (surface-frame-state-bind-group state))
         (shadow-p
           (not (null
                 (surface-technique-orthographic-shadow-depth-format
                  (surface-frame-state-technique state)))))
         (old-shadow-bind-group
           (surface-frame-state-orthographic-shadow-bind-group state))
         sites-buffer sites-capacity
         cells-buffer cells-capacity
         slots-buffer slots-capacity
         bind-group shadow-bind-group (completed-p nil))
    (unwind-protect
         (progn
           (multiple-value-setq (sites-buffer sites-capacity)
             (storage-buffer-candidate
              state (surface-frame-state-sites-capacity state)
              sites-needed "luft surface sites"))
           (multiple-value-setq (cells-buffer cells-capacity)
             (storage-buffer-candidate
              state (surface-frame-state-cells-capacity state)
              cells-needed "luft solid cells"))
           (multiple-value-setq (slots-buffer slots-capacity)
             (storage-buffer-candidate
              state (surface-frame-state-slots-capacity state)
              slots-needed "luft cell stocks"))
           (write-buffer sites-buffer sites)
           (write-buffer cells-buffer (scene-cell-bits scene))
           (write-buffer slots-buffer (scene-slot-words scene))
           (setf bind-group
                 (create-surface-bind-group
                  state sites-buffer cells-buffer slots-buffer))
           (when shadow-p
             (setf shadow-bind-group
                   (create-surface-orthographic-shadow-bind-group
                    state sites-buffer cells-buffer slots-buffer)))
           (setf (surface-frame-state-sites-buffer state) sites-buffer
                 (surface-frame-state-sites-capacity state) sites-capacity
                 (surface-frame-state-cells-buffer state) cells-buffer
                 (surface-frame-state-cells-capacity state) cells-capacity
                 (surface-frame-state-slots-buffer state) slots-buffer
                 (surface-frame-state-slots-capacity state) slots-capacity
                 (surface-frame-state-bind-group state)
                 bind-group
                 (surface-frame-state-orthographic-shadow-bind-group state)
                 shadow-bind-group
                 (surface-frame-state-uploaded-scene state) scene
                 (surface-frame-state-uploaded-scene-revision state)
                 (scene-revision scene)
                 (surface-frame-state-last-scene-upload-kind state) :full
                 (surface-frame-state-last-scene-upload-bytes state)
                 (+ sites-needed cells-needed slots-needed)
                 (surface-frame-state-last-scene-upload-writes state) 3
                 completed-p t)
           ;; Only now can the previous generation stop being reachable.
           (signal-surface-release-failures
            state
            (retire-surface-frame-resources
             state
             (list (cons '(:retired-generation :shadow-bind-group)
                         old-shadow-bind-group)
                   (cons '(:retired-generation :bind-group) old-bind-group)
                   (cons '(:retired-generation :sites-buffer) old-sites)
                   (cons '(:retired-generation :cells-buffer) old-cells)
                   (cons '(:retired-generation :slots-buffer) old-slots))))
           state)
      (unless completed-p
        (retire-surface-frame-resources
         state
         (list (cons '(:unpublished :shadow-bind-group) shadow-bind-group)
               (cons '(:unpublished :bind-group) bind-group)
               (cons '(:unpublished :sites-buffer) sites-buffer)
               (cons '(:unpublished :cells-buffer) cells-buffer)
               (cons '(:unpublished :slots-buffer) slots-buffer)))))))

(defun write-buffer-index-ranges (buffer data indices element-size)
  "Write sorted INDICES from DATA in maximal adjacent ranges.

Return the number of bytes and write calls issued."
  (let ((bytes 0)
        (writes 0))
    (labels ((write-range (start end)
               (let ((slice (subseq data start end)))
                 (write-buffer buffer slice :offset (* element-size start))
                 (incf bytes (* element-size (- end start)))
                 (incf writes))))
      (when (plusp (length indices))
        (let ((start (aref indices 0))
              (previous (aref indices 0)))
          (loop for cursor from 1 below (length indices)
                for index = (aref indices cursor)
                do (if (= index (1+ previous))
                       (setf previous index)
                       (progn
                         (write-range start (1+ previous))
                         (setf start index previous index))))
          (write-range start (1+ previous)))))
    (values bytes writes)))

(zdefun (upload-surface-scene-changes :zone :luft/upload-scene-changes)
    (state scene chunks cell-words slot-words)
  "Upload only the stable pages and dense words named by a scene revision."
  (let ((bytes 0)
        (writes 0)
        (pages (scene-site-pages scene)))
    (loop for index across chunks
          for chunk = (aref (scene-surface-chunks scene) index)
          when (and chunk (plusp (surface-chunk-count chunk)))
            do (let* ((count (surface-chunk-count chunk))
                      (start (* (surface-chunk-page chunk)
                                +surface-chunk-capacity+))
                      (sites (subseq pages start (+ start count))))
                 (write-buffer (surface-frame-state-sites-buffer state) sites
                               :offset (* 8 start))
                 (incf bytes (* 8 count))
                 (incf writes)))
    (multiple-value-bind (range-bytes range-writes)
        (write-buffer-index-ranges
         (surface-frame-state-cells-buffer state)
         (scene-cell-bits scene) cell-words 4)
      (incf bytes range-bytes)
      (incf writes range-writes))
    (multiple-value-bind (range-bytes range-writes)
        (write-buffer-index-ranges
         (surface-frame-state-slots-buffer state)
         (scene-slot-words scene) slot-words 4)
      (incf bytes range-bytes)
      (incf writes range-writes))
    (setf (surface-frame-state-uploaded-scene-revision state)
          (scene-revision scene)
          (surface-frame-state-last-scene-upload-kind state) :incremental
          (surface-frame-state-last-scene-upload-bytes state) bytes
          (surface-frame-state-last-scene-upload-writes state) writes)
    state))

(defun synchronize-surface-frame-state (state scene)
  "Bring STATE to SCENE's published revision, incrementally when possible.

Each state advances from its own uploaded revision.  If that revision has
fallen out of SCENE's bounded history, a coherent full upload is used."
  (service-surface-frame-retirements state)
  (cond
    ((not (eq scene (surface-frame-state-uploaded-scene state)))
     (upload-surface-scene state scene))
    ((eql (scene-revision scene)
          (surface-frame-state-uploaded-scene-revision state))
     state)
    ((or (> (* 8 (length (scene-site-pages scene)))
            (surface-frame-state-sites-capacity state))
         (> (* 4 (length (scene-cell-bits scene)))
            (surface-frame-state-cells-capacity state))
         (> (* 4 (length (scene-slot-words scene)))
            (surface-frame-state-slots-capacity state)))
     (upload-surface-scene state scene))
    (t
     (multiple-value-bind (chunks cell-words slot-words available-p)
         (scene-changes-since
          scene (surface-frame-state-uploaded-scene-revision state))
       (if available-p
           (upload-surface-scene-changes
            state scene chunks cell-words slot-words)
           (upload-surface-scene state scene))))))

(defun write-surface-frame-state (state frame-data stock-data)
  "Write this acquired frame's uniform block and stock table into STATE."
  (write-buffer (surface-frame-state-stocks-buffer state) stock-data)
  (write-buffer (surface-frame-state-uniform-buffer state) frame-data)
  state)

(defun write-surface-shadow-projector (state projector-data)
  "Write STATE's orthographic world-to-shadow projector.

PROJECTOR-DATA is sixteen consecutive single floats: four row vectors whose
dots with (X Y Z 1) produce clip X, Y, Z, and W."
  (unless
      (surface-technique-orthographic-shadow-depth-format
       (surface-frame-state-technique state))
    (error "This Luft surface technique has no orthographic shadow pass."))
  (unless (typep projector-data '(simple-array single-float (16)))
    (error "An orthographic shadow projector must be a simple 16-element ~
            SINGLE-FLOAT array, not ~S."
           (type-of projector-data)))
  (let ((buffer
          (surface-frame-state-orthographic-shadow-projector-buffer state)))
    (unless buffer
      (error "This Luft surface frame has no live shadow projector buffer."))
    (write-buffer buffer projector-data))
  state)

(defun draw-surface-frame (pass state scene style)
  "Bind STATE and draw SCENE through its shared technique's STYLE pipeline.

Synchronization is explicit: drawing a different scene or stale revision is
an error, preventing a caller from accidentally pairing buffers from one
acquired frame with another scene publication."
  (unless (and (eq scene (surface-frame-state-uploaded-scene state))
               (eql (scene-revision scene)
                    (surface-frame-state-uploaded-scene-revision state)))
    (error "Surface frame state is at ~S revision ~S, not ~S revision ~S."
           (surface-frame-state-uploaded-scene state)
           (surface-frame-state-uploaded-scene-revision state)
           scene (scene-revision scene)))
  (set-pipeline pass
                (surface-technique-pipeline
                 (surface-frame-state-technique state) style))
  (set-bind-group pass 0 (surface-frame-state-bind-group state))
  (draw-surface pass scene style)
  state)

(defun draw-surface-shadow-frame (pass state scene)
  "Bind STATE's orthographic sub-technique and draw stock-shaped SCENE depth."
  (let ((technique (surface-frame-state-technique state)))
    (unless
        (surface-technique-orthographic-shadow-depth-format technique)
      (error "This Luft surface technique has no orthographic shadow pass.")))
  (unless (and (eq scene (surface-frame-state-uploaded-scene state))
               (eql (scene-revision scene)
                    (surface-frame-state-uploaded-scene-revision state)))
    (error "Surface frame state is at ~S revision ~S, not ~S revision ~S."
           (surface-frame-state-uploaded-scene state)
           (surface-frame-state-uploaded-scene-revision state)
           scene (scene-revision scene)))
  (let* ((technique (surface-frame-state-technique state))
         (pipeline
           (surface-technique-orthographic-shadow-pipeline technique))
         (bind-group
           (surface-frame-state-orthographic-shadow-bind-group state)))
    (unless (and pipeline bind-group)
      (error "This Luft surface frame has no live orthographic shadow resources."))
    (set-pipeline pass pipeline)
    (set-bind-group pass 0 bind-group))
  (draw-surface pass scene :stock)
  state)

(defun upload-scene (renderer &optional (scene (renderer-scene renderer)))
  "Compatibility entry point: fully upload SCENE into RENDERER's frame state."
  (upload-surface-scene (renderer-surface-frame-state renderer) scene)
  (setf (renderer-scene renderer) scene
        (renderer-history-valid-p renderer) nil)
  renderer)

(defun synchronize-renderer-scene (renderer scene)
  "Bring RENDERER's owned frame state to SCENE's published revision."
  (let ((before (renderer-uploaded-scene-revision renderer)))
    (synchronize-surface-frame-state
     (renderer-surface-frame-state renderer) scene)
    (unless (eql before (renderer-uploaded-scene-revision renderer))
      (setf (renderer-history-valid-p renderer) nil))
    (setf (renderer-scene renderer) scene)
    renderer))

(defun renderer-surface-width (renderer)
  (if (member (renderer-style renderer) '(:chamfer :paper :stock))
      *chamfer-width*
      *bevel-radius*))

(defun renderer-surface-detail (renderer)
  (if (member (renderer-style renderer) '(:field :soft :ink))
      (or *field-vertical-radius* *bevel-radius*)
      *arris-softness*))

(defparameter *temporal-surface-drift-p* nil
  "Whether the surface knobs in the domain lane are drifting continuously.

A film that breathes a radius or a melt moves the surface by far less
than a pixel per frame -- exactly the slowly varying content temporal
accumulation exists for -- so while this is bound true the domain lane
leaves the history key and history survives the drift.  A discrete knob
step is still a cut and should not hide under this flag.")

(defun temporal-history-key (renderer scene frame-data stock-data)
  "The rendered state across which temporal history is semantically reusable.

The first five frame lanes are the camera and projection; lanes five through
eighteen are every light, material, lens, domain, and deformation value used
to shade the frame.  The appended temporal lanes are deliberately excluded,
and the domain lane is excluded while *TEMPORAL-SURFACE-DRIFT-P* declares
its motion continuous and sub-pixel.  #OWG6ZD"
  (list scene (scene-revision scene) (renderer-style renderer) *draw-sky*
        (if *temporal-surface-drift-p*
            (concatenate 'vector
                         (subseq frame-data (* 4 5) (* 4 12))
                         (subseq frame-data (* 4 14) (* 4 19)))
            (subseq frame-data (* 4 5) (* 4 19)))
        stock-data))

(defun frame-color-attachment (view clear)
  `(:view ,view :load-op :clear :store-op :store :clear-value ,clear))

(defun encode-post-pass
    (renderer encoder pipeline bind-group label &optional temporal-scaler)
  (let ((pass
          (begin-render-pass
           encoder
           (make-render-pass-descriptor
            :label label
            :color-attachments
            (list (frame-color-attachment
                   (renderer-color-view renderer) #(0.0 0.0 0.0 1.0)))))))
    (when temporal-scaler
      (wait-temporal-scaler-output pass temporal-scaler))
    (set-pipeline pass pipeline)
    (set-bind-group pass 0 bind-group)
    (draw-screen pass)
    (end-pass pass)))

(zdefun (encode-frame :zone :luft/encode-frame) (renderer encoder)
  "Encode one coherent jittered frame and, when requested, its TAA resolve.
#4I4Y3Z"
  (let* ((extent (renderer-extent renderer))
         (width (first extent))
         (height (second extent))
         (scene (renderer-scene renderer))
         (temporal-p (renderer-effect-p renderer :taa))
         (surfaces (renderer-surfaces renderer))
         (temporal-scaler
           (and temporal-p (frame-surfaces-temporal-scaler surfaces)))
         (light (find-light *light*))
         (sky (if (light-sky light) (light-colour (light-sky light))
                  *sky-color*)))
    (synchronize-renderer-scene renderer scene)
    (let* ((*clay-stock-lane* (clay-stock-lane (scene-stocks scene)))
           (jitter (if temporal-p
                       (temporal-jitter (renderer-frame-index renderer)
                                        width height)
                       #(0.0 0.0)))
           (view (capture-frame-view (renderer-camera renderer)
                                     width height jitter))
           (previous (or (renderer-previous-view renderer) view))
           (stock-data (stock-table-data (scene-stocks scene)))
           ;; HISTORY-VALID-P is not known until the semantic key exists.
           ;; Its lane is outside the key's stable slice and is patched below.
           (frame-data
             (frame-uniform-data view width height (scene-domain scene)
                                 (renderer-surface-width renderer)
                                 (renderer-surface-detail renderer)
                                 previous nil 0.9))
           (history-key (and temporal-p
                             (temporal-history-key
                              renderer scene frame-data stock-data)))
           (history-valid-p
             (and temporal-p
                  (renderer-history-valid-p renderer)
                  (equalp history-key (renderer-history-key renderer))
                  (frame-views-continuous-p previous view))))
      (setf (aref frame-data (- (length frame-data) 2))
            (if history-valid-p 1.0 0.0))
      (write-surface-frame-state
       (renderer-surface-frame-state renderer) frame-data stock-data)
      (let* ((surface-pipeline
               (getf (surface-technique-pipelines
                      (renderer-surface-technique renderer))
                     (renderer-style renderer)))
             (sky-pipeline (getf (renderer-pipelines renderer) :sky))
             (lens-pipeline (getf (renderer-pipelines renderer) :lens))
             (lens-p (and lens-pipeline (plusp *aperture*)))
             (surface-target
               (if (or temporal-p lens-p)
                   (renderer-scene-view renderer)
                   (renderer-color-view renderer)))
             (attachments
               (if temporal-p
                   (list
                    (frame-color-attachment
                     surface-target
                     (vector (vec3:vec3-x sky) (vec3:vec3-y sky)
                             (vec3:vec3-z sky) 1.0))
                    (frame-color-attachment
                     (renderer-motion-view renderer) #(0.0 0.0 0.0 0.0)))
                   (list
                    (frame-color-attachment
                     surface-target
                     (vector (vec3:vec3-x sky) (vec3:vec3-y sky)
                             (vec3:vec3-z sky) 1.0)))))
             (pass
               (begin-render-pass
                encoder
                (make-render-pass-descriptor
                 :label "luft surface pass"
                 :color-attachments attachments
                 :depth-stencil-attachment
                 `(:view ,(renderer-depth-view renderer)
                   :depth-load-op :clear
                   :depth-store-op ,(if temporal-scaler :store :discard)
                   :depth-clear-value 1.0)))))
        (when (and *draw-sky* sky-pipeline)
          (set-pipeline pass sky-pipeline)
          (set-bind-group pass 0 (renderer-bind-group renderer))
          (draw-screen pass))
        (when surface-pipeline
          (draw-surface-frame pass (renderer-surface-frame-state renderer)
                              scene (renderer-style renderer)))
        ;; The clay overlay: the faces the stock mask claimed left the
        ;; main draw, and this second draw is theirs alone.
        (let ((clay-pipeline
                (getf (surface-technique-pipelines
                       (renderer-surface-technique renderer))
                      :clay)))
          (when (and clay-pipeline
                     (plusp *clay-stock-lane*)
                     (not (eq (renderer-style renderer) :clay)))
            (draw-surface-frame pass (renderer-surface-frame-state renderer)
                                scene :clay)))
        (when temporal-scaler
          (signal-temporal-scaler-inputs pass temporal-scaler))
        (end-pass pass)
        (cond
          (temporal-p
           (if temporal-scaler
               (progn
                 (encode-temporal-scale
                  encoder temporal-scaler
                  (renderer-scene-texture renderer)
                  (renderer-depth-texture renderer)
                  (renderer-motion-texture renderer)
                  (frame-surfaces-resolved-texture surfaces)
                  (vector (* 0.5 width (aref jitter 0))
                          (* 0.5 height (aref jitter 1)))
                  (not history-valid-p))
                 (encode-post-pass
                  renderer encoder
                  (if lens-p lens-pipeline
                      (renderer-pipeline renderer :present))
                  (aref (frame-surfaces-post-bind-groups surfaces) 0)
                  (if lens-p "luft lens pass" "luft presentation pass")
                  temporal-scaler)
                 (setf (renderer-previous-view renderer) view
                       (renderer-history-valid-p renderer) t
                       (renderer-history-used-p renderer) history-valid-p
                       (renderer-history-key renderer) history-key
                       (renderer-history-index renderer) 0)
                 (incf (renderer-frame-index renderer)))
               (let* ((write-index (renderer-history-index renderer))
                      (read-index (mod (1+ write-index) 2))
                      (history-textures
                        (frame-surfaces-history-textures surfaces))
                      (history-views
                        (frame-surfaces-history-views surfaces))
                      (temporal-groups
                        (frame-surfaces-temporal-bind-groups surfaces))
                      (post-groups
                        (frame-surfaces-post-bind-groups surfaces)))
                 (prepare-texture encoder (renderer-scene-texture renderer)
                                  :texture-binding)
                 (prepare-texture encoder (renderer-motion-texture renderer)
                                  :texture-binding)
                 (unless history-valid-p
                   (encode encoder
                           (make-gpu-clear-texture-command
                            :texture (aref history-textures read-index)
                            :color #(0.0 0.0 0.0 0.0))))
                 (prepare-texture encoder (aref history-textures read-index)
                                  :texture-binding)
                 (let ((resolve
                         (begin-render-pass
                          encoder
                          (make-render-pass-descriptor
                           :label "luft temporal resolve"
                           :color-attachments
                           (list (frame-color-attachment
                                  (aref history-views write-index)
                                  #(0.0 0.0 0.0 0.0)))))))
                   (set-pipeline resolve (renderer-pipeline renderer :taa))
                   (set-bind-group resolve 0
                                   (aref temporal-groups write-index))
                   (draw-screen resolve)
                   (end-pass resolve))
                 (prepare-texture encoder (aref history-textures write-index)
                                  :texture-binding)
                 (encode-post-pass
                  renderer encoder
                  (if lens-p lens-pipeline
                      (renderer-pipeline renderer :present))
                  (aref post-groups write-index)
                  (if lens-p "luft lens pass" "luft presentation pass"))
                 ;; Publish only after surface, resolve, and post all encode.
                 (setf (renderer-previous-view renderer) view
                       (renderer-history-valid-p renderer) t
                       (renderer-history-used-p renderer) history-valid-p
                       (renderer-history-key renderer) history-key
                       (renderer-history-index renderer) read-index)
                 (incf (renderer-frame-index renderer)))))
          (lens-p
           (prepare-texture encoder (renderer-scene-texture renderer)
                            :texture-binding)
           (encode-post-pass
            renderer encoder lens-pipeline
            (aref (frame-surfaces-post-bind-groups
                   (renderer-surfaces renderer))
                  0)
            "luft lens pass"))))
      (renderer-color-texture renderer))))

(defun render-pixels (renderer)
  "Render one frame headlessly and return its packed pixel bytes.

The further values are the width, height, and colour format of the pixels."
  (let* ((device (renderer-device renderer))
         (extent (renderer-extent renderer))
         (readback (create device
                           (make-buffer-descriptor
                            :label "luft surface readback"
                            :size (* 4 (first extent) (second extent))
                            :usage '(:copy-dst))))
         (encoder nil)
         (command-buffer nil))
    (unwind-protect
         (progn
           (setf encoder (create device
                                 (make-command-encoder-descriptor
                                  :label "luft surface frame")))
           (encode-frame renderer encoder)
           (encode encoder
                   (make-gpu-copy-texture-to-buffer-command
                    :source (renderer-color-texture renderer)
                    :destination readback))
           (setf command-buffer (finish encoder))
           (submit (device-queue device) command-buffer)
           (values (read-buffer readback)
                   (first extent) (second extent)
                   (renderer-color-format renderer)))
      (when command-buffer (destroy command-buffer))
      (when encoder (destroy encoder))
      (destroy readback))))

(defparameter *srgb-to-linear*
  (let ((table (make-array 256 :element-type 'single-float)))
    (dotimes (index 256 table)
      (let ((value (/ (float index 1.0) 255.0)))
        (setf (aref table index)
              (if (<= value 0.04045)
                  (/ value 12.92)
                  (expt (/ (+ value 0.055) 1.055) 2.4))))))
  "One byte to its linear value: a 256-entry table beats a per-pixel EXPT.")

(defun linear-to-srgb-byte (value)
  (let ((clamped (min 1.0 (max 0.0 value))))
    (round (* 255.0
              (if (<= clamped 0.0031308)
                  (* 12.92 clamped)
                  (- (* 1.055 (expt clamped (/ 1.0 2.4))) 0.055))))))

(defun downsample-pixels (pixels width height factor &key (srgb-p t))
  "Average FACTOR by FACTOR blocks of PIXELS, in linear light.

Supersampling is the whole of the antialiasing here: there is no multisample
path, and averaging a rendered frame is the same thing one box filter later.
The average must be taken in linear light -- averaging sRGB bytes darkens
every edge, which is precisely where the eye is looking."
  (let* ((out-width (floor width factor))
         (out-height (floor height factor))
         (out (make-array (* 4 out-width out-height)
                          :element-type '(unsigned-byte 8)))
         (weight (/ 1.0 (* factor factor))))
    (dotimes (y out-height (values out out-width out-height))
      (dotimes (x out-width)
        (let ((red 0.0) (green 0.0) (blue 0.0) (alpha 0.0))
          (dotimes (dy factor)
            (dotimes (dx factor)
              (let ((offset (* 4 (+ (* (+ (* y factor) dy) width)
                                    (+ (* x factor) dx)))))
                (flet ((channel (index)
                         (let ((byte (aref pixels (+ offset index))))
                           (if srgb-p
                               (aref *srgb-to-linear* byte)
                               (/ (float byte 1.0) 255.0)))))
                  (incf red (channel 0))
                  (incf green (channel 1))
                  (incf blue (channel 2))
                  (incf alpha (/ (float (aref pixels (+ offset 3)) 1.0)
                                 255.0))))))
          (let ((offset (* 4 (+ (* y out-width) x))))
            (flet ((store (index value)
                     (setf (aref out (+ offset index))
                           (if srgb-p
                               (linear-to-srgb-byte (* value weight))
                               (round (* 255.0 (min 1.0 (* value weight))))))))
              (store 0 red)
              (store 1 green)
              (store 2 blue)
              (setf (aref out (+ offset 3))
                    (round (* 255.0 (min 1.0 (* alpha weight))))))))))))

(defun render-to-png (renderer pathname &key (downsample 1))
  "Render one frame headlessly and write it to PATHNAME as a PNG.

With DOWNSAMPLE above one the renderer is presumed to have been made that
many times oversize, and the frame is box-filtered down on the way out."
  (multiple-value-bind (pixels width height format) (render-pixels renderer)
    (ensure-directories-exist pathname)
    (if (> downsample 1)
        (multiple-value-bind (small small-width small-height)
            (downsample-pixels pixels width height downsample
                               :srgb-p (eq format :rgba8-unorm-srgb))
          (write-rgba-png pathname small small-width small-height format))
        (write-rgba-png pathname pixels width height format))))

(defun capture-demo-png (pathname &key (width 1280) (height 800)
                                    (camera (make-fly-camera)))
  "Render the demonstration scene once to PATHNAME and release everything."
  (let ((renderer (make-renderer :scene (make-demo-scene) :camera camera
                                 :width width :height height)))
    (unwind-protect
         (render-to-png renderer pathname)
      (destroy-renderer renderer))))

;;; ------------------------------------------------------------------------
;;; Viewer: a window with a fly camera

(defvar *viewer* nil "The most recently started viewer.")

(defclass viewer (canvas-event-handler)
  ((canvas :initarg :canvas :reader viewer-canvas)
   (context :initarg :context :reader viewer-context)
   (renderer :initarg :renderer :accessor viewer-renderer)
   (pressed-keys :initform (make-hash-table :test #'eq)
                 :reader viewer-pressed-keys)
   (pointer-captured-p :initform nil :accessor viewer-pointer-captured-p)
   (running-p :initform t :accessor viewer-running-p)
   (tracy-thread-named-p :initform nil :accessor viewer-tracy-thread-named-p)
   (last-timestamp :initform nil :accessor viewer-last-timestamp)
   (speed :initarg :speed :initform 12.0 :accessor viewer-speed)
   (sensitivity :initarg :sensitivity :initform 0.0032
                :accessor viewer-sensitivity)))

(defun viewer-key-down-p (viewer &rest names)
  (some (lambda (name) (gethash name (viewer-pressed-keys viewer))) names))

(defun advance-viewer-camera (viewer timestamp)
  (let* ((last (viewer-last-timestamp viewer))
         (dt (if last (min 0.1 (max 0.0 (- timestamp last))) 0.0))
         (camera (renderer-camera (viewer-renderer viewer)))
         (step (* dt (viewer-speed viewer)
                  (if (viewer-key-down-p viewer :left-shift :right-shift)
                      3.0 1.0))))
    (setf (viewer-last-timestamp viewer) timestamp)
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore up))
      (flet ((move (direction amount)
               (setf (camera-position camera)
                     (let ((position (camera-position camera)))
                       (vec3:make-vec3
                        (+ (vec3:vec3-x position)
                           (* amount (vec3:vec3-x direction)))
                        (+ (vec3:vec3-y position)
                           (* amount (vec3:vec3-y direction)))
                        (+ (vec3:vec3-z position)
                           (* amount (vec3:vec3-z direction))))))))
        (when (viewer-key-down-p viewer :w :up) (move forward step))
        (when (viewer-key-down-p viewer :s :down) (move forward (- step)))
        (when (viewer-key-down-p viewer :d :right) (move right step))
        (when (viewer-key-down-p viewer :a :left) (move right (- step)))
        (when (viewer-key-down-p viewer :space :e)
          (move (vec3:make-vec3 0 0 1) step))
        (when (viewer-key-down-p viewer :left-control :q :c)
          (move (vec3:make-vec3 0 0 1) (- step)))))))

(defun ray-axis-crossings (position direction)
  "Return grid STEP, first crossing time, and crossing interval for one axis."
  (cond ((plusp direction)
         (values 1 (/ (- (1+ (floor position)) position) direction)
                 (/ 1.0 direction)))
        ((minusp direction)
         (values -1 (/ (- position (floor position)) (- direction))
                 (/ 1.0 (- direction))))
        (t (values 0 1.0e30 1.0e30))))

(defun raycast-scene (scene origin direction &key (max-distance 24.0))
  "Return the first solid cell met by a grid ray and the empty cell before it.

The first two values are three-element coordinate lists or NIL; the third is
the parametric distance.  Tied edge and corner crossings advance together, so
a ray never claims a cell it merely touches."
  (let ((x (floor (vec3:vec3-x origin)))
        (y (floor (vec3:vec3-y origin)))
        (z (floor (vec3:vec3-z origin)))
        step-x step-y step-z
        next-x next-y next-z
        delta-x delta-y delta-z
        (distance 0.0)
        (previous nil))
    (multiple-value-setq (step-x next-x delta-x)
      (ray-axis-crossings (vec3:vec3-x origin) (vec3:vec3-x direction)))
    (multiple-value-setq (step-y next-y delta-y)
      (ray-axis-crossings (vec3:vec3-y origin) (vec3:vec3-y direction)))
    (multiple-value-setq (step-z next-z delta-z)
      (ray-axis-crossings (vec3:vec3-z origin) (vec3:vec3-z direction)))
    (loop
      (when (scene-cell-p scene x y z)
        (return (values (list x y z) previous distance)))
      (let ((next (min next-x next-y next-z)))
        (when (> next max-distance)
          (return (values nil nil nil)))
        (setf previous (list x y z)
              distance next)
        (when (<= next-x (+ next 1.0e-6))
          (incf x step-x)
          (incf next-x delta-x))
        (when (<= next-y (+ next 1.0e-6))
          (incf y step-y)
          (incf next-y delta-y))
        (when (<= next-z (+ next 1.0e-6))
          (incf z step-z)
          (incf next-z delta-z))))))

(defun edit-viewer-facing-cell (viewer solid-p)
  "Remove the facing cell, or add one to the empty site immediately before it."
  (let* ((renderer (viewer-renderer viewer))
         (camera (renderer-camera renderer)))
    (multiple-value-bind (hit before)
        (multiple-value-bind (right up forward) (camera-basis camera)
          (declare (ignore right up))
          (raycast-scene (renderer-scene renderer)
                         (camera-position camera) forward))
      (let ((cell (if solid-p before hit)))
        (when cell
          (setf (scene-cell-p (renderer-scene renderer)
                              (first cell) (second cell) (third cell))
                solid-p)
          cell)))))

(zdefun (render-viewer-frame :zone :luft/viewer-frame) (viewer timestamp)
  (declare (ignore timestamp))
  (unless (viewer-running-p viewer)
    (return-from render-viewer-frame nil))
  (when (and *tracy* (not (viewer-tracy-thread-named-p viewer)))
    (name-tracy-thread "luft canvas")
    (setf (viewer-tracy-thread-named-p viewer) t))
  (prog1
      (present-canvas-frame
       (viewer-context viewer)
       (lambda (surface-texture encoder presentation-time)
         (ensure-renderer-extent
          (viewer-renderer viewer)
          (canvas-extent (viewer-context viewer)))
         (advance-viewer-camera viewer presentation-time)
         (let ((color (encode-frame (viewer-renderer viewer) encoder)))
           (encode encoder
                   (make-gpu-copy-texture-command
                    :source color :destination surface-texture)))))
    ;; Keep LUFT's frame boundary distinct from other canvases sharing this
    ;; durable image.  A wedged frame intentionally remains open in Tracy.
    (tracy-frame-mark "luft")))

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-press-event))
  (let ((key (canvas-key-event-key-name event)))
    (if (eq key :escape)
        (when (viewer-pointer-captured-p viewer)
          (set-canvas-relative-pointer-mode canvas nil)
          (setf (viewer-pointer-captured-p viewer) nil))
        (setf (gethash key (viewer-pressed-keys viewer)) t)))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-key-release-event))
  (declare (ignore canvas))
  (remhash (canvas-key-event-key-name event) (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-button-press-event))
  (let ((button (canvas-pointer-event-button event)))
    (cond ((not (viewer-pointer-captured-p viewer))
           (when (eq :left button)
             (set-canvas-relative-pointer-mode canvas t)
             (setf (viewer-pointer-captured-p viewer) t)))
          ((eq :left button)
           (edit-viewer-facing-cell viewer nil))
          ((eq :right button)
           (edit-viewer-facing-cell viewer t))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-pointer-motion-event))
  (declare (ignore canvas))
  (when (viewer-pointer-captured-p viewer)
    (let ((camera (renderer-camera (viewer-renderer viewer)))
          (sensitivity (viewer-sensitivity viewer)))
      (decf (camera-yaw camera)
            (* (canvas-pointer-event-delta-x event) sensitivity))
      (setf (camera-pitch camera)
            (max -1.5 (min 1.5
                           (- (camera-pitch camera)
                              (* (canvas-pointer-event-delta-y event)
                                 sensitivity)))))))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-focus-lost-event))
  (declare (ignore canvas))
  (clrhash (viewer-pressed-keys viewer))
  nil)

(defmethod handle-canvas-event
    ((viewer viewer) canvas (event canvas-window-close-request-event))
  (declare (ignore canvas event))
  ;; Stop drawing; STOP-VIEWER releases the window from outside the event.
  (setf (viewer-running-p viewer) nil)
  nil)

(defmethod handle-canvas-event ((viewer viewer) canvas event)
  (declare (ignore viewer canvas event))
  nil)

(defun standalone-render-options
    (&optional (name (uiop:getenv "LUFT_RENDER_MODE")))
  "Return MODE, STYLE, PIPELINE-STYLES, and EFFECTS for standalone NAME."
  (let ((mode (string-downcase (or name "full")))
        (styles *surface-styles*))
    (cond ((string= mode "clear")
           (values :clear :flat nil nil))
          ((string= mode "sky")
           (values :sky :flat nil '(:sky)))
          ((member mode '("flat" "bevel" "chamfer" "paper" "stock" "field"
                          "soft" "ink" "clay")
                   :test #'string=)
           (let ((style (intern (string-upcase mode) :keyword)))
             (values style style (list style) nil)))
          ((string= mode "full")
           ;; The stock style is what the world is meant to be seen in: the
           ;; crisp chamfered geometry, the field's soft light, and every
           ;; cell drawn in whatever it is cut from.
           (values :full (if (member :stock styles) :stock :chamfer)
                   styles (default-renderer-effects)))
          (t
           (error "Unknown LUFT_RENDER_MODE ~S; use clear, sky, flat, bevel, ~
chamfer, paper, stock, field, soft, ink, clay, or full." name)))))

(zdefun (start-viewer :zone :luft/start-viewer)
    (&key (scene (make-demo-scene))
          (camera (make-fly-camera))
          (title "luft atelier")
          (width 1280) (height 800)
          (frames-per-second 60)
          (style :flat)
          (pipeline-styles nil pipeline-styles-p)
          (effects nil)
          (provider *gpu-provider*))
  "Open a window flying through SCENE and return the running VIEWER.

  Click to capture the pointer, then left-click to remove the facing cell and
right-click to place one beside it.  Escape releases the pointer; WASD, Space,
and C move.
The renderer stays available as (VIEWER-RENDERER *VIEWER*) for live tinkering.
By default the viewer creates only the flat surface pipeline: pass explicit
PIPELINE-STYLES and EFFECTS to add the complex geometry, sky, or lens.
Every surface style uses vertex pulling, as for MAKE-RENDERER."
  (let ((canvas (make-sdl-canvas
                 :title title :width width :height height :visible-p nil
                 :presentation-api (sdl-presentation-api-for provider)))
        (device nil)
        (renderer nil)
        (completed-p nil))
    (open-canvas canvas)
    (unwind-protect
         (let* ((device* (setf device
                               (request-gpu-device
                                provider
                                (make-device-descriptor :label title))))
                (context (make-canvas-context
                          canvas provider
                          (make-canvas-configuration :device device*)))
                (extent (canvas-extent context))
                (renderer* (setf renderer
                                 (make-renderer
                                  :scene scene :camera camera :device device*
                                  :width (first extent) :height (second extent)
                                  :color-format (canvas-format context)
                                  :style style
                                  :pipeline-styles
                                  (if pipeline-styles-p
                                      pipeline-styles
                                      (list style))
                                  :effects effects)))
                (viewer (make-instance 'viewer :canvas canvas :context context
                                               :renderer renderer*)))
           (setf (canvas-event-handler canvas) viewer)
           (request-canvas-frame
            canvas (lambda (timestamp) (render-viewer-frame viewer timestamp)))
           (show-canvas canvas)
           (setf (canvas-clock canvas)
                 (make-cadence-clock
                  (lambda (native-canvas timestamp)
                    (declare (ignore native-canvas))
                    (render-viewer-frame viewer timestamp))
                  :frames-per-second frames-per-second))
           (setf completed-p t
                 *viewer* viewer)
           viewer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (close-canvas canvas)
        (when device (destroy device))))))

(defun stop-viewer (&optional (viewer *viewer*))
  "Close VIEWER's window and release its renderer and device."
  (when viewer
    (setf (viewer-running-p viewer) nil)
    (let* ((canvas (viewer-canvas viewer))
           (renderer (viewer-renderer viewer))
           (device (and renderer (renderer-device renderer))))
      (when (eq :open (canvas-state canvas))
        (setf (canvas-clock canvas) (make-demand-clock)))
      (when renderer
        (destroy-renderer renderer)
        (setf (viewer-renderer viewer) nil))
      (when (eq :open (canvas-state canvas))
        (close-canvas canvas))
      (when device
        (ignore-errors (destroy device))))
    (when (eq viewer *viewer*)
      (setf *viewer* nil)))
  (values))
