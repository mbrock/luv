(in-package #:luvcraft.luft)

;;; Luvcraft's little world as a Luft world source.  A Luft chunk is 64 by 64
;;; columns, sixteen Luvcraft chunks; the worker builds those in a private
;;; block world exactly as Luvcraft's own load request does -- terrain and
;;; caves, then the landmarks each chunk owns -- and hands every solid block
;;; to the terrain, which replays the source's sparse edits on top.

(defconstant +luvcraft-chunk-size+ 16)

(defclass luvcraft-world-source (terrain:world-source)
  ((seed :initarg :seed :initform 121 :reader luvcraft-world-source-seed)
   (relief :initarg :relief :initform :alpine
           :type (member :meadow :alpine)
           :reader luvcraft-world-source-relief)
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

(defun luvcraft-little-world-source (source)
  "A fresh, edit-free Luvcraft source with SOURCE's seed and relief."
  (make-instance 'luvcraft:little-world-source
                 :seed (luvcraft-world-source-seed source)
                 :relief (luvcraft-world-source-relief source)))

(defmethod terrain:generate-world-source-chunk
    ((source luvcraft-world-source) key place)
  (let* ((offset (luvcraft-world-source-offset source))
         (height (luvcraft-world-source-height source))
         (little (luvcraft-little-world-source source))
         (world (luvcraft::make-block-world
                 :chunk-width +luvcraft-chunk-size+
                 :chunk-height height
                 :chunk-depth +luvcraft-chunk-size+
                 :source little))
         (x0 (- (luft:chunk-origin-x key) offset))
         (z0 (- (luft:chunk-origin-y key) offset))
         (span (floor luft:+chunk-size+ +luvcraft-chunk-size+))
         (placements (make-hash-table :test #'eq)))
    (flet ((placement (block)
             (or (gethash block placements)
                 (setf (gethash block placements) (block-placement block)))))
      (dotimes (i span)
        (dotimes (j span)
          (let ((chunk-x (+ (floor x0 +luvcraft-chunk-size+) i))
                (chunk-z (+ (floor z0 +luvcraft-chunk-size+) j)))
            (luvcraft::materialize-block-world-chunk
             little world chunk-x 0 chunk-z)
            (dolist (landmark (luvcraft::little-world-landmarks-for-chunk
                               little world (list chunk-x 0 chunk-z)))
              (destructuring-bind (block x y z) landmark
                (setf (luvcraft::world-block-at world x y z) block)))
            (let ((chunk (luvcraft::world-chunk-at world chunk-x 0 chunk-z))
                  (base-x (+ offset (* chunk-x +luvcraft-chunk-size+)))
                  (base-y (+ offset (* chunk-z +luvcraft-chunk-size+))))
              (dotimes (lx +luvcraft-chunk-size+)
                (dotimes (lz +luvcraft-chunk-size+)
                  (dotimes (y height)
                    (let ((block (luvcraft::chunk-block-at chunk lx y lz)))
                      (when block
                        (funcall place (+ base-x lx) (+ base-y lz) y
                                 (placement block)))))))))))))
  (values))

(defmethod terrain:world-source-spawn ((source luvcraft-world-source))
  "Luvcraft's own spawn, from its origin, as Luft cells and yaw."
  (let* ((little (luvcraft-little-world-source source))
         (height (luvcraft-world-source-height source))
         (offset (luvcraft-world-source-offset source))
         (alpine-p (eq (luvcraft-world-source-relief source) :alpine)))
    (multiple-value-bind (x z)
        (if alpine-p
            (luvcraft::little-world-scenic-spawn little 0 0 height)
            (values 0 0))
      (values (+ x offset) (+ z offset)
              (1+ (luvcraft::little-world-ground-height little x z height))
              ;; Luvcraft looks along (sin yaw, cos yaw) in X/Z; Luft along
              ;; (cos yaw, sin yaw) in X/Y.
              (if alpine-p
                  (coerce (- (/ pi 2)
                             (luvcraft::little-world-summit-yaw
                              little x z height))
                          'single-float)
                  0.0)))))

(defun little-world-luft-source (little height &key (offset 1024))
  "A Luft source over Luvcraft source LITTLE, carrying its edits across.

Edits outside the Luft domain are dropped and counted: the second value."
  (let* ((source (make-luvcraft-world-source
                  :seed (luvcraft:little-world-source-seed little)
                  :relief (luvcraft:little-world-source-relief little)
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

(defun luvcraft-save-world-source (pathname &key (offset 1024))
  "Read the Luvcraft save at PATHNAME as a Luft source with its edits."
  (let* ((world (luvcraft:read-luvcraft-save pathname))
         (little (luvcraft:block-world-source world))
         (height (luvcraft::chunk-shape-height
                  (luvcraft::voxel-space-chunk-shape
                   (luvcraft::block-world-space world)))))
    (unless (typep little 'luvcraft:little-world-source)
      (error "~A is not a little world; its source is ~S." pathname little))
    (little-world-luft-source little height :offset offset)))

(defun make-luvcraft-streaming-scene (source &rest initargs &key &allow-other-keys)
  "A demand-streamed Luft scene over Luvcraft world SOURCE."
  (apply #'terrain:make-world-source-streaming-scene source initargs))
