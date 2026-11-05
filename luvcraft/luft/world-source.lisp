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
           :type (member :meadow :alpine)
           :reader luvcraft-world-source-relief)
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
  "A fresh, edit-free Luvcraft source of SOURCE's class, seed, and relief."
  (make-instance (luvcraft-world-source-little-class source)
                 :seed (luvcraft-world-source-seed source)
                 :relief (luvcraft-world-source-relief source)))

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
  (max 1 (1- (serapeum:count-cpus :default 2)))
  "How many threads raise the sixteen Luvcraft chunks of one Luft chunk:
every core but one, which stays with the window.

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
         (alpine-p (and (eq (luvcraft-world-source-relief source) :alpine)
                        (eq (class-of little)
                            (find-class 'luvcraft:little-world-source)))))
    (multiple-value-bind (x z)
        (if alpine-p
            (luvcraft::little-world-scenic-spawn little 0 0 height)
            (values 0 0))
      (values (+ x offset) (+ z offset)
              ;; Above whatever a save built there, such as the party's gazebo.
              (loop with edits = (terrain:world-source-edits source)
                    for y from (if alpine-p
                                   (1+ (luvcraft::little-world-ground-height
                                        little x z height))
                                   (luvcraft-column-top source x z))
                    unless (or (gethash (luvcraft-cell-site source x y z) edits)
                               (gethash (luvcraft-cell-site source x (1+ y) z) edits))
                      return y)
              ;; Luvcraft looks along (sin yaw, cos yaw) in X/Z; Luft along
              ;; (cos yaw, sin yaw) in X/Y.
              (if alpine-p
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
