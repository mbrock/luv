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

(defmethod terrain:generate-world-source-chunk
    ((source luvcraft-world-source) key place)
  (let* ((offset (luvcraft-world-source-offset source))
         (height (luvcraft-world-source-height source))
         (little (luvcraft-little-world-source source))
         (world (luvcraft-private-world source little))
         (x0 (- (luft:chunk-origin-x key) offset))
         (z0 (- (luft:chunk-origin-y key) offset))
         (span (floor luft:+chunk-size+ +luvcraft-chunk-size+))
         (placements (make-hash-table :test #'eq)))
    (flet ((placement (block)
             (or (gethash block placements)
                 (setf (gethash block placements) (block-placement block)))))
      (dotimes (i span)
        (dotimes (j span)
          (let* ((chunk-x (+ (floor x0 +luvcraft-chunk-size+) i))
                 (chunk-z (+ (floor z0 +luvcraft-chunk-size+) j))
                 (chunk (load-luvcraft-chunk little world chunk-x chunk-z))
                 (base-x (+ offset (* chunk-x +luvcraft-chunk-size+)))
                 (base-y (+ offset (* chunk-z +luvcraft-chunk-size+))))
            (dotimes (lx +luvcraft-chunk-size+)
              (dotimes (lz +luvcraft-chunk-size+)
                (dotimes (y height)
                  (let ((block (luvcraft::chunk-block-at chunk lx y lz)))
                    (when block
                      (funcall place (+ base-x lx) (+ base-y lz) y
                               (placement block))))))))))))
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

(defun little-world-luft-source (little height &key (offset 1024))
  "A Luft source over Luvcraft source LITTLE, carrying its edits across.

Edits outside the Luft domain are dropped and counted: the second value."
  (let* ((source (make-luvcraft-world-source
                  :seed (luvcraft:little-world-source-seed little)
                  :relief (luvcraft:little-world-source-relief little)
                  :little-class (class-name (class-of little))
                  :height height :offset offset))
         (edits (terrain:world-source-edits source))
         (dropped 0))
    (maphash (lambda (coordinate block)
               (destructuring-bind (x y z) coordinate
                 (let ((site (luvcraft-cell-site source x y z)))
                   (if site
                       (setf (gethash site edits) (block-placement block))
                       (incf dropped)))))
             (luvcraft::block-edit-overlay-entries
              (luvcraft:little-world-source-edits little)))
    (values source dropped)))

(defun luvcraft-world-height (world)
  (luvcraft::chunk-shape-height
   (luvcraft::voxel-space-chunk-shape (luvcraft::block-world-space world))))

(defun luvcraft-world-luft-source (world &key (offset 1024))
  "A Luft source over Luvcraft block WORLD's little-world source and edits."
  (let ((little (luvcraft:block-world-source world)))
    (unless (typep little 'luvcraft:little-world-source)
      (error "~S is not a little world; its source is ~S." world little))
    (little-world-luft-source little (luvcraft-world-height world)
                              :offset offset)))

(defun luvcraft-save-world-source (pathname &key (offset 1024))
  "Read the Luvcraft save at PATHNAME as a Luft source with its edits.

Return the source, the dropped edit count, Luvcraft's world, its resume
description, and its inhabitants."
  (multiple-value-bind (world resume inhabitants)
      (luvcraft:read-luvcraft-save pathname)
    (multiple-value-bind (source dropped)
        (luvcraft-world-luft-source world :offset offset)
      (values source dropped world resume inhabitants))))

;;; Saving back.  A Luvcraft save is its world's description -- the source
;;; kind, seed, and sparse edits -- and a resume.  The Luft source's edits are
;;; the truth for every cell it can hold; Luvcraft's own edit at such a cell
;;; is kept when it is already that block, so a film keeps its reel.  Edits
;;; Luft could not hold and inhabitants it does not know are kept as read.
;;; The game supplies its current inventory and carried instances in RESUME.

(defun luvcraft-block-for-placement (placement)
  "Luvcraft's block for PLACEMENT, NIL for air, and whether there is one.
A film's reel stands for every film and names no block of its own."
  (if (null placement)
      (values nil t)
      (let ((block (luvcraft:block-kind-named
                    (terrain:material-placement-name placement) nil)))
        (values block (not (null block))))))

(defun merged-luvcraft-edit-overlay (source original)
  "Luvcraft's edit overlay for SOURCE's edits over ORIGINAL little source's."
  (let ((overlay (luvcraft::make-block-edit-overlay))
        (old (luvcraft::block-edit-overlay-entries
              (luvcraft:little-world-source-edits original))))
    (maphash (lambda (coordinate block)
               (unless (apply #'luvcraft-cell-site source coordinate)
                 (apply #'luvcraft::record-block-edit overlay block coordinate)))
             old)
    (maphash (lambda (site placement)
               (let ((coordinate (site-luvcraft-cell source site)))
                 (multiple-value-bind (kept present-p) (gethash coordinate old)
                   (multiple-value-bind (block known-p)
                       (if (and present-p (eq placement (block-placement kept)))
                           (values kept t)
                           (luvcraft-block-for-placement placement))
                     (if known-p
                         (apply #'luvcraft::record-block-edit
                                overlay block coordinate)
                         (warn "Not saving ~S at ~S: Luvcraft has no such block."
                               (terrain:material-placement-name placement)
                               coordinate))))))
             (terrain:world-source-edits source))
    overlay))

(defun luvcraft-save-description
    (source world &key resume inhabitants position yaw pitch selected selected-block)
  "Luvcraft's save of WORLD with SOURCE's edits, as Luvcraft writes it.

POSITION is the player's feet in Luft cells, YAW and PITCH the Luft camera's,
and SELECTED the placement in hand. SELECTED-BLOCK may name its exact carried
instance. Without POSITION the supplied RESUME stays unchanged."
  (let* ((description (luvcraft::world-save-description world))
         (source-description (copy-list (getf (rest description) :source)))
         (overlay (merged-luvcraft-edit-overlay
                   source (luvcraft:block-world-source world)))
         (offset (luvcraft-world-source-offset source)))
    (setf (getf (rest source-description) :edits)
          (luvcraft::block-edit-overlay-save-descriptions overlay))
    (setf description (list* (first description)
                             (let ((fields (copy-list (rest description))))
                               (setf (getf fields :source) source-description)
                               fields)))
    (append
     (list :luvcraft-world
           :format-version luvcraft::+luvcraft-save-format-version+
           :world description
           :resume
           (if position
               (destructuring-bind (x y z) position
                 (let ((updated (copy-list resume)))
                   (setf (getf updated :player-position)
                         (list (coerce (- x offset) 'double-float)
                               (coerce z 'double-float)
                               (coerce (- y offset) 'double-float))
                         (getf updated :look)
                         (list :yaw (luft-yaw-to-luvcraft yaw)
                               :pitch (coerce pitch 'double-float))
                         (getf updated :selected-block)
                         (luvcraft::block-save-description
                          (or selected-block
                              (and selected
                                   (luvcraft:block-kind-named
                                    (terrain:material-placement-name selected) nil))
                              luvcraft::*stone-block*)))
                   updated))
               resume))
     (when inhabitants
       (list :inhabitants inhabitants)))))

(defun luvcraft-resume-luft-pose (source resume)
  "The Luft player position, yaw, and pitch of Luvcraft RESUME, or NIL."
  (let ((position (getf resume :player-position))
        (look (getf resume :look))
        (offset (luvcraft-world-source-offset source)))
    (when (and (= 3 (length position)) (every #'realp position))
      (destructuring-bind (x y z) position
        (values (list (+ x offset) (+ z offset) y)
                (luvcraft-yaw-to-luft (getf look :yaw 0))
                (coerce (getf look :pitch 0) 'single-float))))))

(defun make-luvcraft-streaming-scene (source &rest initargs &key &allow-other-keys)
  "A demand-streamed Luft scene over Luvcraft world SOURCE."
  (apply #'terrain:make-world-source-streaming-scene source initargs))
