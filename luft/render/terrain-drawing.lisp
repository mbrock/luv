(in-package #:luft.render)

;;; Terrain contributes the same resident sites to the scene and the sun's
;;; shadow pass. It owns the immutable star tables and those two programs;
;;; resident owners supply sites sorted by star and their appearance, frames
;;; supply the camera.  Each star is one indexed patch instanced over its
;;; run of sites (#8TUEPF).

(defclass terrain-drawing (gpu-resource-owner)
  (;; Per star: scene first index, scene index count, shadow first index,
   ;; shadow index count, into the two shared index buffers.
   (star-ranges :initform nil :accessor terrain-star-ranges)
   (scene-vertices :initform nil :accessor terrain-scene-vertices)
   (normals :initform nil :accessor terrain-normals)
   (scene-indices :initform nil :accessor terrain-scene-indices)
   (shadow-vertices :initform nil :accessor terrain-shadow-vertices)
   (shadow-indices :initform nil :accessor terrain-shadow-indices)
   (scene-program :accessor terrain-scene-program)
   (shadow-program :accessor terrain-shadow-program)))

(defgeneric make-terrain-binding
    (drawing device sites camera
     &key appearances descriptors shadow-map shadow-sampler shadow-p))
(defgeneric encode-terrain (drawing pass binding resident &key shadow-p))

(defmethod make-terrain-binding
    ((drawing terrain-drawing) device sites camera
     &key appearances descriptors shadow-map shadow-sampler shadow-p)
  (apply #'make-program-binding
         (if shadow-p (terrain-shadow-program drawing) (terrain-scene-program drawing))
         device :sites sites :camera-state camera
         (if shadow-p
             (list :terrain-vertices (terrain-shadow-vertices drawing))
             (list :terrain-vertices (terrain-scene-vertices drawing)
                   :terrain-normals (terrain-normals drawing)
                   :terrain-appearances appearances :material-descriptors descriptors
                   :shadow-map shadow-map :shadow-sampler shadow-sampler))))

(defmethod encode-terrain ((drawing terrain-drawing) pass binding resident &key shadow-p)
  (let ((program (if shadow-p (terrain-shadow-program drawing) (terrain-scene-program drawing)))
        (runs (resident-population-star-runs resident))
        (ranges (terrain-star-ranges drawing))
        (indices (if shadow-p
                     (terrain-shadow-indices drawing)
                     (terrain-scene-indices drawing)))
        (offset (if shadow-p 2 0)))
    (when runs
      (set-pipeline pass (program-pipeline program))
      (set-bind-group pass 0 binding)
      (loop for (star first count) in runs
            for range = (aref ranges star)
            for index-count = (aref range (1+ offset))
            when (plusp index-count)
              do (draw-indexed pass indices :uint32 index-count count
                               (aref range offset) 0 first)))))

;;; Star-table coordinates are mesh ticks in [-8, 8], biased to keep the
;;; GPU records unsigned.

(defconstant +star-patch-coordinate-bias+ 8)

;;; The star tables as indexed patches.

(defun star-triangle-normal-key (triangle)
  "TRIANGLE's outward normal as the smallest integer vector along it."
  (destructuring-bind (a b c) triangle
    (let* ((u (mapcar #'- b a))
           (v (mapcar #'- c a))
           (n (list (- (* (second u) (third v)) (* (third u) (second v)))
                    (- (* (third u) (first v)) (* (first u) (third v)))
                    (- (* (first u) (second v)) (* (second u) (first v)))))
           (g (max 1 (reduce #'gcd n))))
      (mapcar (lambda (x) (/ x g)) n))))

(defun star-shadow-triangles (star bevels-p)
  "The triangles STAR casts into the sun's depth map.

Bevelled terrain casts from its plain faces, each moved one tick into the
solid: a seventh of the triangles, and the bands and junctions barely move a
shadow's outline at shadow-map resolution.  Every face still reaches its
cell's edges, so neighbouring faces overshoot one another at an edge rather
than parting.  The shrink keeps every bevelled surface, bands and junctions
included, outside its own caster, so nothing shades itself.
*BEVELLED-SHADOWS-P* casts the bevels themselves."
  (if (and bevels-p (not *bevelled-shadows-p*))
      (loop for triangle in (luft:star-atlas-flat-owned-triangles star)
            for inward = (mapcar #'- (star-triangle-normal-key triangle))
            collect (loop for point in triangle
                          collect (mapcar #'+ point inward)))
      (if bevels-p
          (luft:star-atlas-owned-triangles star)
          (luft:star-atlas-flat-owned-triangles star))))

(defun star-patch-tables (bevels-p)
  "Index each star's owned triangles into shared scene and shadow tables.

Returns the scene vertex words, the normal table, the scene indices, the
shadow vertex words, the shadow indices, and a 256-vector of per-star
(scene-first scene-count shadow-first shadow-count) ranges.  A scene vertex
is shared only between triangles with the same normal and material mask, so
flat per-triangle shading is unchanged; a shadow vertex wherever positions
meet.  The shadow table indexes STAR-SHADOW-TRIANGLES."
  (let ((scene-keys (make-hash-table :test #'equal))
        (scene-words (make-array 0 :element-type '(unsigned-byte 32)
                                   :adjustable t :fill-pointer 0))
        (normal-keys (make-hash-table :test #'equal))
        (normals (make-array 0 :element-type 'single-float
                               :adjustable t :fill-pointer 0))
        (scene-indices (make-array 0 :element-type '(unsigned-byte 32)
                                     :adjustable t :fill-pointer 0))
        (shadow-keys (make-hash-table :test #'equal))
        (shadow-words (make-array 0 :element-type '(unsigned-byte 32)
                                    :adjustable t :fill-pointer 0))
        (shadow-indices (make-array 0 :element-type '(unsigned-byte 32)
                                      :adjustable t :fill-pointer 0))
        (ranges (make-array 256)))
    (flet ((record (words point fourth)
             (destructuring-bind (x y z) point
               (vector-push-extend (+ x +star-patch-coordinate-bias+) words)
               (vector-push-extend (+ y +star-patch-coordinate-bias+) words)
               (vector-push-extend (+ z +star-patch-coordinate-bias+) words)
               (vector-push-extend fourth words)
               (1- (/ (fill-pointer words) 4))))
           (normal-index (key)
             (or (gethash key normal-keys)
                 (let ((length (sqrt (reduce #'+ (mapcar (lambda (x) (* x x)) key)))))
                   (dolist (x key) (vector-push-extend (/ x length) normals))
                   (vector-push-extend 0.0 normals)
                   (setf (gethash key normal-keys) (1- (/ (fill-pointer normals) 4)))))))
      (dotimes (star 256)
        (let ((triangles (if bevels-p
                             (luft:star-atlas-owned-triangles star)
                             (luft:star-atlas-flat-owned-triangles star)))
              (appearance (if bevels-p
                              (luft:star-atlas-owned-appearance-masks star)
                              (luft:star-atlas-flat-owned-appearance-masks star)))
              (scene-first (fill-pointer scene-indices))
              (shadow-first (fill-pointer shadow-indices)))
          (loop for triangle in triangles
                for (material-mask light-mask) in appearance
                for fourth = (logior material-mask (ash light-mask 8)
                                     (ash (normal-index
                                           (star-triangle-normal-key triangle))
                                          16))
                do (dolist (point triangle)
                     (let ((scene-key (list star point fourth)))
                       (vector-push-extend
                        (or (gethash scene-key scene-keys)
                            (setf (gethash scene-key scene-keys)
                                  (record scene-words point fourth)))
                        scene-indices))))
          (dolist (triangle (star-shadow-triangles star bevels-p))
            (dolist (point triangle)
              (let ((shadow-key (list star point)))
                (vector-push-extend
                 (or (gethash shadow-key shadow-keys)
                     (setf (gethash shadow-key shadow-keys)
                           (record shadow-words point 0)))
                 shadow-indices))))
          (setf (aref ranges star)
                (vector scene-first (- (fill-pointer scene-indices) scene-first)
                        shadow-first (- (fill-pointer shadow-indices) shadow-first))))))
    (flet ((simple (array type)
             (make-array (length array) :element-type type :initial-contents array)))
      (values (simple scene-words '(unsigned-byte 32))
              (simple normals 'single-float)
              (simple scene-indices '(unsigned-byte 32))
              (simple shadow-words '(unsigned-byte 32))
              (simple shadow-indices '(unsigned-byte 32))
              ranges))))

(defun make-terrain-drawing (device target-formats sample-count)
  (let ((drawing (make-instance 'terrain-drawing)))
    (with-gpu-construction (drawing)
      (multiple-value-bind (scene-words normals scene-indices shadow-words
                            shadow-indices ranges)
          (star-patch-tables *terrain-bevels-p*)
        (flet ((upload (label words usage)
                 (let ((buffer (own-gpu-resource
                                drawing device
                                (make-buffer-descriptor
                                 :label label :size (max 16 (* 4 (length words)))
                                 :usage (list usage :copy-dst)))))
                   (write-buffer buffer words)
                   buffer)))
          (setf (terrain-star-ranges drawing) ranges
                (terrain-scene-vertices drawing)
                (upload "luft star patch vertices" scene-words :storage)
                (terrain-normals drawing)
                (upload "luft star patch normals" normals :storage)
                (terrain-scene-indices drawing)
                (upload "luft star patch indices" scene-indices :index)
                (terrain-shadow-vertices drawing)
                (upload "luft star shadow vertices" shadow-words :storage)
                (terrain-shadow-indices drawing)
                (upload "luft star shadow indices" shadow-indices :index))))
      (setf (terrain-scene-program drawing)
            (own-gpu-object
             drawing
             (make-drawing-program
              device :label "luft site streams"
              :vertex (shaders:terrain-vertex-specification)
              :fragment (shaders:star-fragment-specification)
              :targets (mapcar (lambda (format) `(:format ,format)) target-formats)
              :sample-count sample-count
              :depth-stencil '(:format :depth32-float :depth-write-enabled t
                               :depth-compare :less)))
            (terrain-shadow-program drawing)
            (own-gpu-object
             drawing
             (make-drawing-program
              device :label "luft sun shadow sites"
              :vertex (shaders:terrain-shadow-vertex-specification)
              :depth-stencil '(:format :depth32-float :depth-write-enabled t
                               :depth-compare :less)))))))
