(in-package #:luvcraft.luft)

(defconstant +luvcraft-chunk-size+ 16)

;;; Luvcraft's little world as a Luft world source.  A Luft chunk is 64 by 64
;;; columns, sixteen Luvcraft chunks; the worker builds each of those through
;;; Luvcraft's own load request for the source's class -- terrain, caves, and
;;; landmarks, or the birthday meadow's lawn and trees -- and hands every solid
;;; block to the terrain, which replays the source's sparse edits on top.

(defclass luvcraft-world-source (terrain:world-source)
  ((seed :initarg :seed :initform 121 :reader luvcraft-world-source-seed)
   (relief :initarg :relief :initform :alpine
           :type (member :meadow :alpine :biomes)
           :reader luvcraft-world-source-relief)
   (homeland :initarg :homeland :initform nil
             :reader luvcraft-world-source-homeland
             :documentation "Optional meadow homeland preservation spec for rebased :BIOMES worlds.")
   (distant-edit-cache :initform nil :accessor luvcraft-world-source-distant-edit-cache
                       :documentation "Cached (journal-head . sector-hash) of authored column tops for distant landmarks.")
   (little-class :initarg :little-class :initform 'luvcraft:little-world-source
                 :reader luvcraft-world-source-little-class
                 :documentation "The class of Luvcraft source that generates it.")
   (height :initarg :height :initform luvcraft::*little-world-height*
           :reader luvcraft-world-source-height
           :documentation "Luvcraft's chunk height, which its reliefs scale with.")
   (offset :initarg :offset :initform 1024 :reader luvcraft-world-source-offset
           :documentation "Luft X and Y of Luvcraft's X and Z origin."))
  (:default-initargs
   :domain (luft:make-world-domain :x-bits 11 :y-bits 11)
   :material-vocabulary (luvcraft-palette-vocabulary *luvcraft-palette*))
  (:documentation "A Luvcraft little world, Y up, laid into Luft's Z-up cells."))

(defmethod initialize-instance :after ((source luvcraft-world-source) &key)
  (unless (zerop (mod (luvcraft-world-source-offset source) +luvcraft-chunk-size+))
    (error "A Luvcraft offset must align its chunks, not ~D."
           (luvcraft-world-source-offset source)))
  (unless (<= 4 (luvcraft-world-source-height source) 254)
    (error "A Luvcraft height of ~D does not fit a Luft column."
           (luvcraft-world-source-height source))))

(defun make-luvcraft-world-source (&rest initargs &key &allow-other-keys)
  (apply #'make-instance 'luvcraft-world-source initargs))

(defun luvcraft-cell-site (source x y z)
  "The Luft site of Luvcraft cell X,Y,Z in SOURCE, or NIL outside its domain."
  (let ((domain (terrain:world-source-domain source))
        (lx (+ x (luvcraft-world-source-offset source)))
        (ly (+ z (luvcraft-world-source-offset source))))
    (and (< -1 lx (luft:world-domain-x-limit domain))
         (< -1 ly (luft:world-domain-y-limit domain))
         (<= 0 y 254)
         (luft:make-site domain lx ly y luft:+cell-extent+ 1))))

(defun site-luvcraft-cell (source site)
  "The Luvcraft X, Y, and Z of Luft SITE in SOURCE, as a list."
  (let ((offset (luvcraft-world-source-offset source)))
    (list (- (luft:site-x site) offset)
          (luft:site-z site)
          (- (luft:site-y site) offset))))

(defun luvcraft-little-world-source (source)
  "A fresh, edit-free Luvcraft source of SOURCE's class, seed, relief, and homeland."
  (let ((homeland (luvcraft-world-source-homeland source)))
    (if homeland
        (make-instance (luvcraft-world-source-little-class source)
                       :seed (luvcraft-world-source-seed source)
                       :relief (luvcraft-world-source-relief source)
                       :homeland homeland)
        (make-instance (luvcraft-world-source-little-class source)
                       :seed (luvcraft-world-source-seed source)
                       :relief (luvcraft-world-source-relief source)))))

(defun luvcraft-private-world (source little)
  (luvcraft::make-block-world
   :chunk-width +luvcraft-chunk-size+
   :chunk-height (luvcraft-world-source-height source)
   :chunk-depth +luvcraft-chunk-size+
   :source little))

(defun load-luvcraft-chunk (little world chunk-x chunk-z)
  "Raise Luvcraft chunk CHUNK-X, CHUNK-Z into WORLD as Luvcraft streams it.

The load request is the source's own, so every kind of little world -- the
stock reliefs and the birthday meadow alike -- generates here exactly as it
does in Luvcraft."
  (let* ((key (list chunk-x 0 chunk-z))
         (request (luvcraft:make-block-chunk-load-request little world key 1 0))
         (payload (luv.production:perform-production-request request)))
    (luvcraft::install-world-chunk-storage
     world chunk-x 0 chunk-z
     (luvcraft::block-chunk-load-payload-content payload))
    (luvcraft::world-chunk-at world chunk-x 0 chunk-z)))

(defparameter *luvcraft-generation-threads*
  ;; On Darwin, SBCL can deadlock when GC signals a thread while another
  ;; thread is reaping recently exited pthreads. Avoid making transient
  ;; helpers here: terrain generation already runs on the mesh producer
  ;; during streaming.
  #+darwin 1
  #-darwin
  (max 1 (1- (serapeum:count-cpus :default 2)))
  "How many threads raise the sixteen Luvcraft chunks of one Luft chunk.

Each Luvcraft chunk is a pure function of its source, so they are computed
apart and their cells handed to the terrain in the same order as from one
thread; 1 generates them all on the calling thread.")

(defun luvcraft-chunk-content (little world chunk-x chunk-z)
  "The dense content Luvcraft's own load request raises for CHUNK-X, CHUNK-Z.
WORLD is only read, for its chunk shape, so threads may share it."
  (luvcraft::block-chunk-load-payload-content
   (luv.production:perform-production-request
    (luvcraft:make-block-chunk-load-request
     little world (list chunk-x 0 chunk-z) 1 0))))

(defun map-luvcraft-chunk-indices (function count)
  "A vector of FUNCTION's values for 0 below COUNT, computed by up to
*LUVCRAFT-GENERATION-THREADS* threads; the first error is resignalled."
  (let ((results (make-array count))
        (next (list 0))
        (threads (max 1 (min count *luvcraft-generation-threads*))))
    (flet ((work ()
             (handler-case
                 ;; ATOMIC-INCF returns the value before its increment.
                 (loop for index = (sb-ext:atomic-incf (car next))
                       while (< index count)
                       do (setf (aref results index) (funcall function index))
                       finally (return nil))
               (error (condition)
                 ;; Stop the others early; the caller resignals this.
                 (setf (car next) count)
                 condition))))
      (let* ((helpers (loop repeat (1- threads)
                            collect (sb-thread:make-thread
                                     #'work :name "Luvcraft chunk generator")))
             (failure (work)))
        (dolist (helper helpers)
          (let ((condition (sb-thread:join-thread helper)))
            (setf failure (or failure condition))))
        (when failure (error failure))
        results))))

(defmethod terrain:generate-world-source-chunk
    ((source luvcraft-world-source) key place)
  (let* ((offset (luvcraft-world-source-offset source))
         (height (luvcraft-world-source-height source))
         (little (luvcraft-little-world-source source))
         (world (luvcraft-private-world source little))
         (x0 (- (luft:chunk-origin-x key) offset))
         (z0 (- (luft:chunk-origin-y key) offset))
         (span (floor luft:+chunk-size+ +luvcraft-chunk-size+))
         (contents
           (map-luvcraft-chunk-indices
            (lambda (index)
              (multiple-value-bind (i j) (floor index span)
                (luvcraft-chunk-content
                 little world
                 (+ (floor x0 +luvcraft-chunk-size+) i)
                 (+ (floor z0 +luvcraft-chunk-size+) j))))
            (* span span)))
         (placements (make-hash-table :test #'eq)))
    (flet ((placement (block)
             (or (gethash block placements)
                 (setf (gethash block placements) (block-placement block)))))
      (dotimes (i span)
        (dotimes (j span)
          (let* ((chunk-x (+ (floor x0 +luvcraft-chunk-size+) i))
                 (chunk-z (+ (floor z0 +luvcraft-chunk-size+) j))
                 (base-x (+ offset (* chunk-x +luvcraft-chunk-size+)))
                 (base-y (+ offset (* chunk-z +luvcraft-chunk-size+))))
            ;; Read the dense storage directly; X varies fastest, then Y.
            (luvcraft.world:with-block-content-storage (domain members indices)
                (aref contents (+ (* i span) j))
              (declare (ignore domain))
              (let ((palette (coerce members 'simple-vector)))
                (declare (type (simple-array (unsigned-byte 16) (*)) indices)
                         (type fixnum height base-x base-y))
                (dotimes (lx +luvcraft-chunk-size+)
                  (dotimes (lz +luvcraft-chunk-size+)
                    (dotimes (y height)
                      (let ((block (svref palette
                                          (aref indices
                                                (+ lx (* +luvcraft-chunk-size+
                                                         (+ y (* height lz))))))))
                        (when block
                          (funcall place (+ base-x lx) (+ base-y lz) y
                                   (placement block))))))))))))))
  (values))

(defun luvcraft-block-material-code (vocabulary block)
  "1-based terrain descriptor code for Luvcraft BLOCK in VOCABULARY, or 0."
  (let ((placement (and block (block-placement block))))
    (if placement
        (1+ (domains:identity-vocabulary-offset vocabulary placement))
        0)))

(defmethod terrain:world-source-distant-terrain-p ((source luvcraft-world-source))
  (subtypep (luvcraft-world-source-little-class source) 'luvcraft:little-world-source))

(defmethod terrain:world-source-surface-column ((source luvcraft-world-source) x y)
  (let* ((offset (luvcraft-world-source-offset source))
         (height (luvcraft-world-source-height source))
         (little (luvcraft-little-world-source source))
         (vocab (terrain:world-source-material-vocabulary source))
         (lx (- x offset))
         (lz (- y offset))
         (surface (luvcraft::little-world-surface-height little lx lz height))
         (surface-material (luvcraft::little-world-surface-material
                            little lx lz surface height))
         (below-material (luvcraft::little-world-column-material
                          little (max 0 (1- surface)) surface surface-material)))
    (values surface
            (luvcraft-block-material-code vocab surface-material)
            (luvcraft-block-material-code vocab below-material))))

(defun luvcraft-distant-edit-sectors (source)
  "Return a hash table mapping 64-cell sector (SX . SY) to lists of (LX LY Z CODE)
for solid authored edits in SOURCE, rebuilt only when the journal advances."
  (let* ((journal (terrain:world-source-journal source))
         (head (terrain:world-journal-head journal))
         (cached (luvcraft-world-source-distant-edit-cache source)))
    (when (and cached (eql (car cached) head))
      (return-from luvcraft-distant-edit-sectors (cdr cached)))
    (let* ((edits (terrain:world-source-edits source))
           (vocab (terrain:world-source-material-vocabulary source))
           (columns (make-hash-table :test #'equal))
           (sectors (make-hash-table :test #'equal)))
      ;; Keep the highest solid authored block per Luft (LX, LY) column.
      (maphash (lambda (site value)
                 (when value
                   (let* ((placement (terrain:world-value-placement value))
                          (code (and placement
                                     (1+ (domains:identity-vocabulary-offset vocab placement)))))
                     (when code
                       (let* ((lx (luft:site-x site))
                              (ly (luft:site-y site))
                              (lz (luft:site-z site))
                              (col-key (cons lx ly))
                              (prev (gethash col-key columns)))
                         (when (or (null prev) (> lz (car prev)))
                           (setf (gethash col-key columns) (cons lz code))))))))
               edits)
      (maphash (lambda (col-key entry)
                 (let* ((lx (car col-key))
                        (ly (cdr col-key))
                        (skey (cons (ash lx -6) (ash ly -6))))
                   (push (list lx ly (car entry) (cdr entry))
                         (gethash skey sectors))))
               columns)
      (setf (luvcraft-world-source-distant-edit-cache source) (cons head sectors))
      sectors)))

(defmethod terrain:sample-world-source-surface-grid
    ((source luvcraft-world-source) origin-x origin-y stride count heights top-codes side-codes)
  (declare (type fixnum origin-x origin-y stride count)
           (type (simple-array (signed-byte 32) (*)) heights)
           (type (simple-array (unsigned-byte 8) (*)) top-codes side-codes))
  (let* ((offset (luvcraft-world-source-offset source))
         (height (luvcraft-world-source-height source))
         (little (luvcraft-little-world-source source))
         (relief (luvcraft-world-source-relief source))
         (biomes-p (eq relief :biomes))
         (rock-cliffs-p (member relief '(:alpine :biomes)))
         (vocab (terrain:world-source-material-vocabulary source))
         (grass-code (luvcraft-block-material-code vocab luvcraft::*grass-block*))
         (dirt-code (luvcraft-block-material-code vocab luvcraft::*dirt-block*))
         (stone-code (luvcraft-block-material-code vocab luvcraft::*stone-block*))
         (sand-code (luvcraft-block-material-code vocab luvcraft::*sand-block*))
         (sandstone-code (luvcraft-block-material-code vocab luvcraft::*sandstone-block*))
         (snow-code (luvcraft-block-material-code vocab luvcraft::*snow-block*))
         (slate-code (luvcraft-block-material-code vocab luvcraft::*slate-block*))
         (leaf-code (luvcraft-block-material-code vocab luvcraft::*leaf-block*))
         (moss-code (luvcraft-block-material-code vocab luvcraft::*moss-block*))
         (terracotta-code (luvcraft-block-material-code vocab luvcraft::*terracotta-block*))
         (granite-code (luvcraft-block-material-code vocab luvcraft::*granite-block*))
         (basalt-code (luvcraft-block-material-code vocab luvcraft::*basalt-block*))
         (cactus-code (luvcraft-block-material-code vocab luvcraft::*cactus-block*))
         (pitch (+ count 2))
         (half (ash stride -1)))
    (declare (type fixnum offset height pitch half)
             (type (unsigned-byte 8) grass-code dirt-code stone-code
                   sand-code sandstone-code snow-code slate-code leaf-code
                   moss-code terracotta-code granite-code basalt-code cactus-code))
    ;; Pass 1: analytical surface heights including the 1-cell neighbor border.
    (dotimes (row pitch)
      (let ((lz (- (+ origin-y (* (1- row) stride) half) offset))
            (row-base (* row pitch)))
        (declare (type fixnum lz row-base))
        (dotimes (col pitch)
          (let ((lx (- (+ origin-x (* (1- col) stride) half) offset)))
            (declare (type fixnum lx))
            (setf (aref heights (+ row-base col))
                  (luvcraft::little-world-surface-height little lx lz height))))))
    ;; Pass 2: biome surface & cliff materials from ground slopes.
    (loop for row fixnum from 1 to count
          for lz fixnum = (- (+ origin-y (* (1- row) stride) half) offset)
          for row-base fixnum = (* row pitch) do
      (loop for col fixnum from 1 to count
            for lx fixnum = (- (+ origin-x (* (1- col) stride) half) offset)
            for idx fixnum = (+ row-base col)
            for h fixnum = (aref heights idx)
            for max-diff fixnum = (max (abs (- h (aref heights (1+ idx))))
                                       (abs (- h (aref heights (1- idx))))
                                       (abs (- h (aref heights (+ idx pitch))))
                                       (abs (- h (aref heights (- idx pitch)))))
            for slope fixnum = (floor (+ max-diff half) stride)
            for mat = (luvcraft::little-world-surface-material
                       little lx lz h height slope)
            for biome = (and biomes-p (luvcraft::little-world-biome-at little lx lz))
            do (cond
                 ((or (eq mat luvcraft::*grass-block*)
                      (eq mat luvcraft::*moss-block*)
                      (eq mat luvcraft::*flowers-block*))
                  (let* ((base-code (cond ((eq mat luvcraft::*moss-block*) moss-code)
                                          ((eq mat luvcraft::*grass-block*) grass-code)
                                          (t (luvcraft-block-material-code vocab mat))))
                         (dense-forest-p (eq biome :forest))
                         (forested-p
                           (or dense-forest-p
                               (> (luvcraft::little-world-value-noise little lx lz 48 29)
                                  -0.35d0)))
                         (side (if (> max-diff 3) stone-code dirt-code)))
                    (if forested-p
                        (let* ((hash (luvcraft::little-world-hash
                                      little
                                      (floor lx (max 4 stride))
                                      (floor lz (max 4 stride))))
                               (mod-close (if dense-forest-p 3 5))
                               (mod-far (if dense-forest-p 2 4)))
                          (cond
                            ((and (<= stride 4) (zerop (mod hash mod-close)))
                             (setf (aref top-codes idx) leaf-code
                                   (aref side-codes idx) leaf-code))
                            ((and (> stride 4) (zerop (mod hash mod-far)))
                             (setf (aref top-codes idx) leaf-code
                                   (aref side-codes idx) side))
                            (t
                             (setf (aref top-codes idx) base-code
                                   (aref side-codes idx) side))))
                        (setf (aref top-codes idx) base-code
                              (aref side-codes idx) side))))
                 ((eq mat luvcraft::*snow-block*)
                  (setf (aref top-codes idx) snow-code
                        (aref side-codes idx)
                        (if (and rock-cliffs-p (> max-diff 2)) stone-code snow-code)))
                 ((eq mat luvcraft::*stone-block*)
                  (setf (aref top-codes idx) stone-code
                        (aref side-codes idx) stone-code))
                 ((eq mat luvcraft::*slate-block*)
                  (setf (aref top-codes idx) slate-code
                        (aref side-codes idx) stone-code))
                 ((eq mat luvcraft::*basalt-block*)
                  (setf (aref top-codes idx) basalt-code
                        (aref side-codes idx) basalt-code))
                 ((eq mat luvcraft::*terracotta-block*)
                  (setf (aref top-codes idx) terracotta-code
                        (aref side-codes idx)
                        (if (> max-diff 3)
                            (luvcraft-block-material-code
                             vocab (luvcraft::little-world-mesa-strata-block (max 0 (- h 2))))
                            terracotta-code)))
                 ((eq mat luvcraft::*granite-block*)
                  (setf (aref top-codes idx) granite-code
                        (aref side-codes idx)
                        (if (eq biome :mesa) terracotta-code granite-code)))
                 ((eq mat luvcraft::*sandstone-block*)
                  (setf (aref top-codes idx) sandstone-code
                        (aref side-codes idx)
                        (if (eq biome :mesa) terracotta-code sandstone-code)))
                 ((eq mat luvcraft::*sand-block*)
                  (let ((cactus-p
                          (and biomes-p (eq biome :desert) (<= stride 2)
                               (zerop (mod (luvcraft::little-world-hash
                                            little (floor lx 4) (floor lz 4))
                                           19)))))
                    (if cactus-p
                        (setf (aref top-codes idx) cactus-code
                              (aref side-codes idx) cactus-code)
                        (setf (aref top-codes idx) sand-code
                              (aref side-codes idx)
                              (if (> max-diff 2) sandstone-code sand-code)))))
                 (t
                  (let ((code (luvcraft-block-material-code vocab mat)))
                    (setf (aref top-codes idx) code
                          (aref side-codes idx) code))))))
    ;; Pass 3: raise close-LOD forested canopy tufts and desert cacti.
    (when (<= stride 4)
      (loop for row fixnum from 1 to count
            for row-base fixnum = (* row pitch) do
        (loop for col fixnum from 1 to count
              for idx fixnum = (+ row-base col)
              when (or (and (= (aref top-codes idx) leaf-code)
                            (= (aref side-codes idx) leaf-code))
                       (and (= (aref top-codes idx) cactus-code)
                            (= (aref side-codes idx) cactus-code)))
                do (let ((h (aref heights idx)))
                     (when (< (+ h 4) height)
                       (setf (aref heights idx) (+ h 3)))))))
    ;; Pass 4: project authored landmarks (houses, towers, temples) into distant
    ;; columns so player builds stay visible across kilometers.
    (let ((sectors (luvcraft-distant-edit-sectors source)))
      (when (plusp (hash-table-count sectors))
        (let* ((tile-span (* count stride))
               (sx0 (ash (- origin-x stride) -6))
               (sx1 (ash (+ origin-x tile-span stride -1) -6))
               (sy0 (ash (- origin-y stride) -6))
               (sy1 (ash (+ origin-y tile-span stride -1) -6)))
          (loop for sy fixnum from sy0 to sy1 do
            (loop for sx fixnum from sx0 to sx1
                  for entries = (gethash (cons sx sy) sectors)
                  when entries
                    do (dolist (entry entries)
                         (destructuring-bind (elx ely ez ecode) entry
                           (let ((col (1+ (floor (- elx origin-x) stride)))
                                 (row (1+ (floor (- ely origin-y) stride))))
                             (when (and (<= 0 col (1+ count))
                                        (<= 0 row (1+ count)))
                               (let ((idx (+ (* row pitch) col)))
                                 (when (> ez (aref heights idx))
                                   (setf (aref heights idx) ez
                                         (aref top-codes idx) ecode
                                         (aref side-codes idx) ecode)))))))))))))
  (values))

(defun luvcraft-column-top (source x z)
  "One above the highest generated block of Luvcraft column X, Z in SOURCE."
  (let* ((little (luvcraft-little-world-source source))
         (world (luvcraft-private-world source little))
         (chunk (load-luvcraft-chunk little world
                                     (floor x +luvcraft-chunk-size+)
                                     (floor z +luvcraft-chunk-size+))))
    (loop for y from (1- (luvcraft-world-source-height source)) downto 0
          when (luvcraft::chunk-block-at chunk (mod x +luvcraft-chunk-size+) y
                                         (mod z +luvcraft-chunk-size+))
            return (1+ y)
          finally (return 0))))

(defmethod terrain:world-source-spawn ((source luvcraft-world-source))
  "Luvcraft's own spawn, from its origin, as Luft cells and yaw."
  (let* ((little (luvcraft-little-world-source source))
         (height (luvcraft-world-source-height source))
         (offset (luvcraft-world-source-offset source))
         (scenic-p (and (member (luvcraft-world-source-relief source) '(:alpine :biomes))
                        (null (luvcraft-world-source-homeland source))
                        (eq (class-of little)
                            (find-class 'luvcraft:little-world-source)))))
    (multiple-value-bind (x z)
        (if scenic-p
            (luvcraft::little-world-scenic-spawn little 0 0 height)
            (values 0 0))
      (values (+ x offset) (+ z offset)
              ;; Above whatever a save built there, such as the party's gazebo.
              (loop with edits = (terrain:world-source-edits source)
                    for y from (if scenic-p
                                   (1+ (luvcraft::little-world-ground-height
                                        little x z height))
                                   (luvcraft-column-top source x z))
                    unless (or (gethash (luvcraft-cell-site source x y z) edits)
                               (gethash (luvcraft-cell-site source x (1+ y) z) edits))
                      return y)
              ;; Luvcraft looks along (sin yaw, cos yaw) in X/Z; Luft along
              ;; (cos yaw, sin yaw) in X/Y.
              (if scenic-p
                  (luvcraft-yaw-to-luft
                   (luvcraft::little-world-summit-yaw little x z height))
                  0.0)))))

(defun luvcraft-yaw-to-luft (yaw)
  (coerce (- (/ pi 2) yaw) 'single-float))

(defun luft-yaw-to-luvcraft (yaw)
  (coerce (- (/ pi 2) yaw) 'double-float))

(defun saved-provenance (author)
  "The terrain provenance of a save's AUTHOR plist, or NIL."
  (and author
       (terrain:make-cell-provenance :actor (getf author :actor) :sequence (getf author :sequence)
                                     :tick (getf author :tick) :time (getf author :time))))

(defun little-world-luft-source (little height &key (offset 1024) (watermark 0)
                                                    events provenance
                                                    (edits (luvcraft::block-edit-overlay-entries
                                                            (luvcraft:little-world-source-edits
                                                             little))))
  "A Luft source over Luvcraft source LITTLE, carrying its edits across.

EDITS, a hash table from coordinates to blocks, by default LITTLE's, are the
source journal's edit set, compacted at WATERMARK: the journal sequence its
save was written at.  PROVENANCE, a hash table of the same coordinates to
author plists, says who made them, and EVENTS are the save's journal tail
after WATERMARK, as RESTORE-LUVCRAFT-SAVE-DESCRIPTION reads them; the
journal replays them to its head (#4D3QFJ).  Edits outside the Luft domain
are dropped and counted: the second value."
  (let* ((source (make-luvcraft-world-source
                  :seed (luvcraft:little-world-source-seed little)
                  :relief (luvcraft:little-world-source-relief little)
                  :homeland (luvcraft::little-world-source-homeland little)
                  :little-class (class-name (class-of little))
                  :height height :offset offset))
         (journal (terrain:world-source-journal source))
         (installed nil)
         (dropped 0))
    (maphash (lambda (coordinate block)
               (destructuring-bind (x y z) coordinate
                 (let ((site (luvcraft-cell-site source x y z)))
                   (if site
                       (push (list site block
                                   (and provenance
                                        (saved-provenance (gethash coordinate provenance))))
                             installed)
                       (incf dropped)))))
             edits)
    (terrain:install-world-edit-set journal installed watermark)
    (terrain:replay-world-events
     journal
     (loop for event in events
           collect (terrain:make-world-event
                    :sequence (getf event :sequence) :transaction (getf event :transaction)
                    :actor (getf event :actor) :tick (getf event :tick) :time (getf event :time)
                    :site (or (apply #'luvcraft-cell-site source (getf event :coordinate))
                              (error "Journal event ~S is outside the Luft world." event))
                    :before (getf event :before) :after (getf event :after)
                    :cause (getf event :cause))))
    (values source dropped)))

(defmethod terrain:world-source-baseline-value
    ((source luvcraft-world-source) placement)
  ;; A generated cell is a plain material; only authored cells hold entities.
  (and placement
       (luvcraft:block-kind-named (terrain:material-placement-name placement))))

(defun luvcraft-world-height (world)
  (luvcraft::chunk-shape-height
   (luvcraft::voxel-space-chunk-shape (luvcraft::block-world-space world))))

(defun luvcraft-world-luft-source (world &key (offset 1024) journal
                                              (watermark (getf journal :watermark 0)))
  "A Luft source over Luvcraft block WORLD's little-world source and edits,
compacted at journal sequence WATERMARK.  JOURNAL, the plist a save's
reader returns, supplies the watermark, the edit set there, the tail after
it, and who authored the edits; WORLD itself stands at the tail's end."
  (let ((little (luvcraft:block-world-source world)))
    (unless (typep little 'luvcraft:little-world-source)
      (error "~S is not a little world; its source is ~S." world little))
    (apply #'little-world-luft-source little (luvcraft-world-height world)
           :offset offset :watermark watermark
           :events (getf journal :journal)
           :provenance (getf journal :provenance)
           (and (getf journal :edits) (list :edits (getf journal :edits))))))

(defun luvcraft-save-world-source (pathname &key (offset 1024))
  "Read the Luvcraft save at PATHNAME as a Luft source with its edits.

Return the source, the dropped edit count, Luvcraft's world, its resume
description, and its inhabitants."
  (multiple-value-bind (world resume inhabitants journal)
      (luvcraft:read-luvcraft-save pathname)
    (multiple-value-bind (source dropped)
        (luvcraft-world-luft-source world :offset offset :journal journal)
      (values source dropped world resume inhabitants))))

;;; Saving back, as format 2 (#Y3S82T): the world's recipe, the material
;;; vocabulary, its entities, and the sparse edits as columns.  Format 1
;;; saves are read through the same remap and healed when written.  The Luft
;;; source's edits are the truth for every cell it can hold; Luvcraft's own
;;; edit at such a cell is kept when it is already that material, so a film
;;; keeps its entity.  Edits Luft could not hold and inhabitants it does not
;;; know are kept as read.  The game supplies its current inventory and
;;; carried instances in RESUME.

(defun luvcraft-block-for-placement (placement)
  "Luvcraft's block for PLACEMENT, NIL for air, and whether there is one.
A material whole only with an entity, such as the film reel every film
stands on, names no block by itself."
  (if (null placement)
      (values nil t)
      (let ((block (luvcraft:block-kind-named
                    (terrain:material-placement-name placement) nil)))
        (if (and block (not (luvcraft:block-kind-entity-required-p block)))
            (values block t)
            (values nil nil)))))

(defun merged-luvcraft-edit-overlay (source original
                                     &optional (edits (terrain:world-source-edits source)))
  "Luvcraft's edit overlay for SOURCE's journal over ORIGINAL little source's.

Inside the Luft domain the journal's block identities, EDITS, are the edits:
by default its edit set at its head.  Outside it ORIGINAL's are kept as read."
  (let ((overlay (luvcraft::make-block-edit-overlay)))
    (maphash (lambda (coordinate block)
               (unless (apply #'luvcraft-cell-site source coordinate)
                 (apply #'luvcraft::record-block-edit overlay block coordinate)))
             (luvcraft::block-edit-overlay-entries
              (luvcraft:little-world-source-edits original)))
    (maphash (lambda (site value)
               (let ((coordinate (site-luvcraft-cell source site)))
                 (multiple-value-bind (block known-p) (journal-value-block value)
                   (if known-p
                       (apply #'luvcraft::record-block-edit overlay block coordinate)
                       (warn "Not saving ~S at ~S: Luvcraft has no such block."
                             value coordinate)))))
             edits)
    overlay))

(defun journal-value-block (value)
  "The Luvcraft block identity of journal VALUE, and whether it names one.
An edit sequenced as a bare placement is read as its material."
  (if (typep value 'terrain:material-placement)
      (luvcraft-block-for-placement value)
      (values value t)))

(defun luft-pose-resume-fields (source position yaw pitch flying-p)
  "A resume's pose fields for feet at Luft POSITION in SOURCE, looking along
Luft camera YAW and PITCH: Luvcraft's coordinates, as format 1 kept them.
LUVCRAFT-RESUME-LUFT-POSE reads them back."
  (destructuring-bind (x y z) position
    (let ((offset (luvcraft-world-source-offset source)))
      (list :player-position (list (coerce (- x offset) 'double-float)
                                   (coerce z 'double-float)
                                   (coerce (- y offset) 'double-float))
            :look (list :yaw (luft-yaw-to-luvcraft yaw)
                        :pitch (coerce pitch 'double-float))
            ;; Without it, a save made in the air drops the player out of
            ;; the sky at the next start.
            :flying (and flying-p t)))))

(defun saved-journal-value-p (value)
  "Whether a save can write journal VALUE: a block, air, or :UNRECORDED."
  (or (eq value :unrecorded) (nth-value 1 (journal-value-block value))))

(defun luvcraft-save-journal (source)
  "SOURCE's journal as a save keeps it: the watermark its edit set is compacted
at, that set, who authored it, and the tail of event plists after it.

The tail is the newest TERRAIN:*JOURNAL-TAIL-EVENTS* events, cut at a
transaction boundary, and after the last event a save could not write."
  (let* ((journal (terrain:world-source-journal source))
         (watermark (terrain:world-journal-compaction-watermark journal))
         (events (terrain:world-journal-events journal))
         (unsaved (position-if-not (lambda (event)
                                     (and (saved-journal-value-p (terrain:world-event-before event))
                                          (saved-journal-value-p (terrain:world-event-after event))))
                                   events :from-end t)))
    (when (and unsaved (> (+ (terrain:world-journal-watermark journal) unsaved 1) watermark))
      ;; Past the end of that event's transaction.
      (let ((transaction (terrain:world-event-transaction (aref events unsaved))))
        (setf watermark (+ (terrain:world-journal-watermark journal)
                           (or (position-if (lambda (event)
                                              (/= transaction (terrain:world-event-transaction event)))
                                            events :start unsaved)
                               (length events))))))
    (multiple-value-bind (edits provenance tail) (terrain:world-journal-compacted journal watermark)
      (let ((authors (make-hash-table :test #'equal)))
        (maphash (lambda (site author)
                   (setf (gethash (site-luvcraft-cell source site) authors)
                         (list :actor (terrain:cell-provenance-actor author)
                               :sequence (terrain:cell-provenance-sequence author)
                               :tick (terrain:cell-provenance-tick author)
                               :time (terrain:cell-provenance-time author))))
                 provenance)
        (values watermark edits authors
                (loop for event in tail
                      collect (flet ((value (value)
                                       (if (eq value :unrecorded)
                                           value
                                           (values (journal-value-block value)))))
                                (list :sequence (terrain:world-event-sequence event)
                                      :transaction (terrain:world-event-transaction event)
                                      :actor (terrain:world-event-actor event)
                                      :tick (terrain:world-event-tick event)
                                      :time (terrain:world-event-time event)
                                      :coordinate (site-luvcraft-cell source
                                                                      (terrain:world-event-site event))
                                      :before (value (terrain:world-event-before event))
                                      :after (value (terrain:world-event-after event))
                                      :cause (terrain:world-event-cause event)))))))))

(defun luvcraft-save-description
    (source world &key resume players inhabitants position yaw pitch flying-p
                    selected selected-block)
  "The format-2 save of WORLD with SOURCE's edits: its journal compacted as
LUVCRAFT-SAVE-JOURNAL keeps it.

PLAYERS is the players table, records of block identities (#Y3S82T).  A
RESUME is written only when given, as older saves kept the player: POSITION
is the player's feet in Luft cells, YAW and PITCH the Luft camera's,
FLYING-P whether the player was in flight there, and SELECTED the placement
in hand. SELECTED-BLOCK may name its exact carried instance. Without
POSITION the supplied RESUME stays unchanged."
  (multiple-value-bind (watermark edits provenance journal) (luvcraft-save-journal source)
    (luvcraft::make-luvcraft-format-2-save-description
     world
     :overlay (merged-luvcraft-edit-overlay source (luvcraft:block-world-source world) edits)
     :watermark watermark
     :journal journal
     :provenance provenance
     :players players
     :inhabitants inhabitants
     :resume
     (if (and resume position)
         (let ((updated (copy-list resume)))
           (loop for (key value) on (luft-pose-resume-fields source position yaw pitch flying-p)
                   by #'cddr
                 do (setf (getf updated key) value))
           (setf (getf updated :selected-block)
                 (luvcraft::block-save-description
                  (or selected-block
                      (and selected (luvcraft-block-for-placement selected))
                      luvcraft::*stone-block*)))
           updated)
         resume))))

(defun luvcraft-resume-luft-pose (source resume)
  "The Luft player position, yaw, pitch, and flight of Luvcraft RESUME, or NIL."
  (let ((position (getf resume :player-position))
        (look (getf resume :look))
        (offset (luvcraft-world-source-offset source)))
    (when (and (= 3 (length position)) (every #'realp position))
      (destructuring-bind (x y z) position
        (values (list (+ x offset) (+ z offset) y)
                (luvcraft-yaw-to-luft (getf look :yaw 0))
                (coerce (getf look :pitch 0) 'single-float)
                (getf resume :flying))))))

(defun make-luvcraft-streaming-scene (source &rest initargs &key &allow-other-keys)
  "A demand-streamed Luft scene over Luvcraft world SOURCE."
  (apply #'terrain:make-world-source-streaming-scene source initargs))

(defmethod terrain:sample-stride-four-materials
    ((source luvcraft-world-source) origin-x origin-y materials)
  "Sample the stock little world's relief and cave field in 3D (#AQCYEH).
Four footprint corners filter each roof. Use the center column for strata and caves."
  (unless (eq (luvcraft-world-source-little-class source) 'luvcraft:little-world-source)
    (error "Stride-four sampling requires the stock little-world generator."))
  (let* ((little (luvcraft-little-world-source source))
         (relief (luvcraft-world-source-relief source))
         (height (luvcraft-world-source-height source))
         (offset (luvcraft-world-source-offset source))
         (vocab (terrain:world-source-material-vocabulary source))
         (codes (make-hash-table :test #'eq)))
    (dotimes (cx 32)
      (dotimes (cy 32)
        (let* ((x (- (+ origin-x (* 4 cx)) offset))
               (y (- (+ origin-y (* 4 cy)) offset))
               (mx (+ x 2)) (my (+ y 2))
               (surface (round (+ (luvcraft::little-world-surface-height little x y height)
                                  (luvcraft::little-world-surface-height little (+ x 3) y height)
                                  (luvcraft::little-world-surface-height little x (+ y 3) height)
                                  (luvcraft::little-world-surface-height little (+ x 3) (+ y 3) height))
                               4))
               (skin (luvcraft::little-world-surface-material little mx my surface height))
               (biome (and (eq relief :biomes) (luvcraft::little-world-biome-at little mx my)))
               (caves-p (or (eq relief :alpine)
                            (and biome (<= (luvcraft::little-world-homeland-weight little mx my)
                                           0.15d0))))
               (top (floor (- surface 1) 4)))
          (loop for cz from 0 to top
                for z = (min surface (+ (* cz 4) 2))
                unless (and caves-p (luvcraft::little-world-cave-p little mx z my surface))
                  do (let* ((block (if (= cz top) skin
                                       (luvcraft::little-world-column-material
                                        little z surface skin biome)))
                            (code (or (gethash block codes)
                                      (setf (gethash block codes)
                                            (luvcraft-block-material-code vocab block)))))
                       (setf (aref materials (+ cz (* 64 (+ cy (* 32 cx))))) code))))))))
