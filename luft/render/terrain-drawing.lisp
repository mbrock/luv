(in-package #:luft.render)

;;; Terrain contributes the same resident sites to the scene and the sun's
;;; shadow pass. It owns the immutable star atlas and those two mesh programs;
;;; resident owners supply sites and appearance, frames supply the camera.

(defclass terrain-drawing (gpu-resource-owner)
  ((templates :accessor terrain-templates)
   (scene-program :accessor terrain-scene-program)
   (shadow-program :accessor terrain-shadow-program)))

(defgeneric make-terrain-binding
    (drawing device sites camera &key appearances descriptors shadow-map shadow-sampler shadow-p))
(defgeneric encode-terrain (drawing pass binding workgroups &key shadow-p))

(defmethod make-terrain-binding
    ((drawing terrain-drawing) device sites camera
     &key appearances descriptors shadow-map shadow-sampler shadow-p)
  (apply #'make-program-binding
         (if shadow-p (terrain-shadow-program drawing) (terrain-scene-program drawing))
         device :sites sites :star-templates (terrain-templates drawing) :camera-state camera
         (unless shadow-p
           (list :terrain-appearances appearances :material-descriptors descriptors
                 :shadow-map shadow-map :shadow-sampler shadow-sampler))))

(defmethod encode-terrain ((drawing terrain-drawing) pass binding workgroups &key shadow-p)
  (when (plusp workgroups)
    (encode-program (if shadow-p (terrain-shadow-program drawing) (terrain-scene-program drawing))
                    pass binding (make-gpu-draw-mesh-command :x workgroups))))

(defun make-terrain-drawing (device target-formats sample-count)
  (let ((drawing (make-instance 'terrain-drawing)))
    (with-gpu-construction (drawing)
      (let ((words (star-meshlet-template-words)))
        (setf (terrain-templates drawing)
              (own-gpu-resource drawing device
                                (make-buffer-descriptor :label "luft 256 star meshlets"
                                                        :size (* 4 (length words))
                                                        :usage '(:storage :copy-dst))))
        (write-buffer (terrain-templates drawing) words))
      (setf (terrain-scene-program drawing)
            (own-gpu-object drawing
                            (make-drawing-program
                             device :label "luft site streams"
                             :mesh (shaders:terrain-mesh-specification)
                             :fragment (shaders:star-fragment-specification)
                             :targets (mapcar (lambda (format) `(:format ,format)) target-formats)
                             :sample-count sample-count
                             :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                              :depth-compare :less)))
            (terrain-shadow-program drawing)
            (own-gpu-object drawing
                            (make-drawing-program
                             device :label "luft sun shadow sites"
                             :mesh (shaders:terrain-shadow-mesh-specification)
                             :depth-stencil '(:format :depth32-float :depth-write-enabled t
                                              :depth-compare :less)))))))



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
      (let* ((triangles (luft:star-atlas-owned-triangles star))
             (appearance (luft:star-atlas-owned-appearance-masks star))
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

