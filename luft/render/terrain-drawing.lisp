(in-package #:luft.render)

;;; Terrain contributes the same resident sites to the scene and the sun's
;;; shadow pass. It owns the immutable star atlas and those two programs;
;;; resident owners supply sites, appearance, and the triangle list, frames
;;; supply the camera.  The programs pull vertices from a plain draw of
;;; three vertices per listed triangle, or, with *TERRAIN-MESH-SHADERS-P*,
;;; run the earlier one-workgroup-per-site mesh stages.

(defclass terrain-drawing (gpu-resource-owner)
  ((templates :accessor terrain-templates)
   (star-triangle-counts :initform nil :accessor %terrain-star-triangle-counts)
   (mesh-shaders-p :initform nil :accessor terrain-mesh-shaders-p)
   (scene-program :accessor terrain-scene-program)
   (shadow-program :accessor terrain-shadow-program)))

(defgeneric make-terrain-binding
    (drawing device sites camera
     &key appearances descriptors triangles shadow-map shadow-sampler shadow-p))
(defgeneric encode-terrain (drawing pass binding resident &key shadow-p))
(defgeneric terrain-star-triangle-counts (drawing)
  (:documentation "Triangles per star for a triangle list, or NIL for none.")
  (:method (drawing)
    (declare (ignore drawing))
    nil))

(defmethod terrain-star-triangle-counts ((drawing terrain-drawing))
  (unless (terrain-mesh-shaders-p drawing)
    (%terrain-star-triangle-counts drawing)))

(defmethod make-terrain-binding
    ((drawing terrain-drawing) device sites camera
     &key appearances descriptors triangles shadow-map shadow-sampler shadow-p)
  (apply #'make-program-binding
         (if shadow-p (terrain-shadow-program drawing) (terrain-scene-program drawing))
         device :sites sites :star-templates (terrain-templates drawing) :camera-state camera
         (append
          (unless (terrain-mesh-shaders-p drawing)
            (list :terrain-triangles triangles))
          (unless shadow-p
            (list :terrain-appearances appearances :material-descriptors descriptors
                  :shadow-map shadow-map :shadow-sampler shadow-sampler)))))

(defmethod encode-terrain ((drawing terrain-drawing) pass binding resident &key shadow-p)
  (let ((program (if shadow-p (terrain-shadow-program drawing) (terrain-scene-program drawing))))
    (if (terrain-mesh-shaders-p drawing)
        (let ((workgroups (render-population-mesh-workgroup-count
                           (resident-population-population resident))))
          (when (plusp workgroups)
            (encode-program program pass binding
                            (make-gpu-draw-mesh-command :x workgroups))))
        (let ((triangles (resident-population-triangle-count resident)))
          (when (plusp triangles)
            (encode-program program pass binding
                            (make-gpu-draw-command :vertex-count (* 3 triangles))))))))

(defun make-terrain-drawing (device target-formats sample-count)
  (let ((drawing (make-instance 'terrain-drawing)))
    (with-gpu-construction (drawing)
      (setf (terrain-mesh-shaders-p drawing) *terrain-mesh-shaders-p*)
      (let ((words (star-meshlet-template-words)))
        (setf (%terrain-star-triangle-counts drawing)
              (let ((counts (make-array 256 :element-type '(unsigned-byte 8))))
                (dotimes (star 256 counts)
                  (setf (aref counts star)
                        (aref words (* star +star-meshlet-record-count+ 4))))))
        (setf (terrain-templates drawing)
              (own-gpu-resource drawing device
                                (make-buffer-descriptor :label "luft 256 star meshlets"
                                                        :size (* 4 (length words))
                                                        :usage '(:storage :copy-dst))))
        (write-buffer (terrain-templates drawing) words))
      (let ((mesh-p (terrain-mesh-shaders-p drawing)))
        (setf (terrain-scene-program drawing)
              (own-gpu-object
               drawing
               (apply #'make-drawing-program
                      device :label "luft site streams"
                      :fragment (shaders:star-fragment-specification)
                      :targets (mapcar (lambda (format) `(:format ,format)) target-formats)
                      :sample-count sample-count
                      :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                       :depth-compare :less)
                      (if mesh-p
                          (list :mesh (shaders:terrain-mesh-specification))
                          (list :vertex (shaders:terrain-vertex-specification)))))
              (terrain-shadow-program drawing)
              (own-gpu-object
               drawing
               (apply #'make-drawing-program
                      device :label "luft sun shadow sites"
                      :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                       :depth-compare :less)
                      (if mesh-p
                          (list :mesh (shaders:terrain-shadow-mesh-specification))
                          (list :vertex (shaders:terrain-shadow-vertex-specification))))))))))



;;; Fixed star-meshlet representation consumed by both programs.

(defconstant +render-template-vertex-count+ 6)

(defconstant +star-meshlet-triangle-capacity+ 25)
(defconstant +star-meshlet-vertex-capacity+
  (* 3 +star-meshlet-triangle-capacity+))
(defconstant +star-meshlet-record-count+
  (1+ +star-meshlet-vertex-capacity+))
(defconstant +star-meshlet-coordinate-bias+ 8)

(defun star-meshlet-template-words ()
  "Return the fixed 256-record triangle-soup atlas consumed by mesh shaders.

Each star owns one fixed-size block (#0UAD9N).  Its first uvec4 contains the triangle
count; the remaining records are the three vertices of each triangle in
outward order.  Coordinates are biased only to keep this first ABI unsigned."
  (let ((words
          (make-array (* 256 +star-meshlet-record-count+ 4)
                      :element-type '(unsigned-byte 32)
                      :initial-element 0)))
    (dotimes (star 256 words)
      (let* ((triangles (if *terrain-bevels-p*
                            (luft:star-atlas-owned-triangles star)
                            (luft:star-atlas-flat-owned-triangles star)))
             (appearance (if *terrain-bevels-p*
                             (luft:star-atlas-owned-appearance-masks star)
                             (luft:star-atlas-flat-owned-appearance-masks star)))
             (triangle-count (length triangles))
             (block (* star +star-meshlet-record-count+ 4)))
        (when (> triangle-count +star-meshlet-triangle-capacity+)
          (error "Star #x~2,'0X owns ~D triangles; the meshlet ABI admits ~D."
                 star triangle-count +star-meshlet-triangle-capacity+))
        (setf (aref words block) triangle-count)
        (loop for triangle in triangles
              for (material-mask light-mask) in appearance
              for triangle-index from 0
              do (loop for point in triangle
                       for corner from 0
                       for record = (+ 1 (* 3 triangle-index) corner)
                       for offset = (+ block (* 4 record))
                       do (destructuring-bind (x y z) point
                            (setf (aref words offset)
                                  (+ x +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 1))
                                  (+ y +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 2))
                                  (+ z +star-meshlet-coordinate-bias+)
                                  (aref words (+ offset 3))
                                  (if (zerop corner)
                                      (logior material-mask
                                              (ash light-mask 8))
                                      0)))))))))

