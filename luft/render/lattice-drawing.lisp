(in-package #:luft.render)

;;; Optional construction markers derived from the same star atlas as terrain.
;;; Generate at most one resident overlay per frame; normal drawing needs none.

(defun make-lattice-drawing (device target-formats sample-count)
  "The optional diagnostic drawing uses the same checked program protocol."
  (make-drawing-program
   device :label "luft eighth-cell lattice points"
   :vertex (shaders:lattice-point-vertex-specification)
   :fragment (shaders:lattice-point-fragment-specification)
   :targets (loop for format in target-formats
                  for first = t then nil
                  collect `(:format ,format ,@(when first '(:blend :premultiplied-alpha))))
   :sample-count sample-count
   :depth-stencil '(:format :depth32-float :depth-write-enabled nil :depth-compare :less)))

(zdefun (mesh-lattice-point-words :zone :luft/prepare-overlay) (mesh)
  "Unique active sites and triangle vertices of the current star mesh.
Each uvec4 stores eighth-cell XYZ and marker kind (1 vertex, 2 active site).
The overlay consumes the same star atlas as terrain, including companions."
  (let ((points (make-hash-table :test #'equal)))
    (labels ((remember (point kind)
               (unless (every (lambda (coordinate) (typep coordinate '(unsigned-byte 32))) point)
                 (error "Lattice point ~S cannot be represented by the shader's uvec4 input." point))
               (setf (gethash point points) (max kind (gethash point points 0))))
             (visit (mesh)
               (let ((words (luft:surface-mesh-star-site-words mesh)))
                 (loop for offset from 0 below (length words) by 4
                       for origin = (loop for axis below 3
                                          collect (* luft:+mesh-cell-size+ (aref words (+ offset axis))))
                       for star = (aref words (+ offset 3)) do
                         (remember origin 2)
                         (dolist (triangle (luft:star-atlas-owned-triangles star))
                           (dolist (point triangle) (remember (mapcar #'+ origin point) 1)))))
               (dolist (companion (luft:surface-mesh-companions mesh)) (visit companion))))
      (visit mesh))
    (let ((result (make-array (* 4 (hash-table-count points)) :element-type '(unsigned-byte 32)))
          (offset 0))
      (maphash (lambda (point kind)
                 (dolist (coordinate point) (setf (aref result offset) coordinate) (incf offset))
                 (setf (aref result offset) kind)
                 (incf offset))
               points)
      result)))

(defun ensure-mesh-slot-lattice-points (renderer slot)
  "Lazily upload markers; retain ownership even if upload or cleanup fails."
  (unless (mesh-slot-lattice-point-buffer slot)
    (let* ((words (mesh-lattice-point-words (mesh-slot-mesh slot)))
           (buffer (own-gpu-resource
                    slot (renderer-device renderer)
                    (make-buffer-descriptor :label "luft star construction markers"
                                            :size (max 16 (* 4 (length words)))
                                            :usage '(:storage :copy-dst))))
           (completed-p nil))
      (unwind-protect
           (progn
             (when (plusp (length words)) (write-buffer buffer words))
             (setf (mesh-slot-lattice-point-buffer slot) buffer
                   (mesh-slot-lattice-point-count slot) (/ (length words) 4)
                   completed-p t))
        (unless completed-p
          (with-release-warnings
            (releasing :construction-upload (release-owned-gpu-object slot buffer)))))))
  slot)
