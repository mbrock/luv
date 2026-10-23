(in-package #:luvcraft.luft.game)

;;; The old effect owns the fragment motion; only its presentation changes
;;; coordinate systems and takes the material's Luft tone instead of an atlas.
;;; The rim shares the original pixel feathering, not the voxel square (#7O90OS).
(defmethod render:advance-viewer-game :after ((viewer feedback-game-state) seconds)
  (luvcraft::advance-block-particles (viewer-particles viewer) seconds))

(defmethod render:viewer-figures :around ((viewer feedback-game-state))
  (append
   (call-next-method)
   (loop with particles = (luvcraft::block-particle-system-particles (viewer-particles viewer))
         with offset = (if (plusp (length particles))
                           (game::luvcraft-world-source-offset (streaming-source viewer)) 0)
         for p across particles
         for center = (vec3:make-vec3
                       (+ offset (luvcraft::block-particle-x p))
                       (+ offset (luvcraft::block-particle-z p))
                       (luvcraft::block-particle-y p))
         for half = (* 0.5 (luvcraft::block-particle-size p))
         collect (let ((figure
                         (render:box-figure
                          center center 0.0 1.0 (list half half half)
                          (nth-value 1
                            (terrain:material-kind-oriented-tones
                             (terrain:material-placement-kind
                              (game:block-placement (luvcraft::block-particle-block p))))))))
                   (setf (fourth (fourth figure)) 3.0)
                   figure))))

(defun target-face-boundary (scene face cell)
  "The boundary of the actual atlas face, not the uncut voxel square.
Cancel interior triangle edges, retaining concave joins as well as bevels."
  (let* ((domain (terrain:scene-domain scene))
         (solid (terrain::inspection-source-solid scene))
         (base (list (luft:site-x cell) (luft:site-y cell) (luft:site-z cell)))
         (normal (multiple-value-list (luft:face-oriented-normal face)))
         (axis (position-if-not #'zerop normal))
         (plane (+ (nth axis base) (if (plusp (nth axis normal)) 1 0)))
         (edges (make-hash-table :test #'equal)))
    ;; The old game marks the cell face even for shaped blocks. Keep that
    ;; convention for fixtures, whose geometry is not a star-atlas surface.
    (let ((placement (terrain:scene-material-placement-at scene cell)))
      (when (and placement
                 (not (eq :none (terrain:material-kind-body
                                (terrain:material-placement-kind placement)))))
        (let ((others (remove axis '(0 1 2))))
          (return-from target-face-boundary
            (loop for (u v) in '((0 0) (1 0) (1 1) (0 1))
                  collect (let ((point (copy-list base)))
                            (setf (nth axis point) plane)
                            (incf (nth (first others) point) u)
                            (incf (nth (second others) point) v)
                            (apply #'vec3:make-vec3 point)))))))
    (dotimes (corner 8)
      (let* ((anchor (loop for value in base for axis below 3
                           collect (+ value (ldb (byte 1 axis) corner))))
             (site (apply #'luft:make-site domain
                          (append anchor (list luft:+vertex-extent+ 1))))
             (star (luft:site-star-occupancy-mask
                    domain site
                    (lambda (x y z)
                      ;; Meshing treats fixtures as air, though picking and
                      ;; walking still see their solid cells.
                      (if (zerop (terrain:inspection-cell-bit solid x y z)) 0
                          (let ((placement
                                  (terrain:scene-material-placement-at
                                   scene (luft:make-site domain x y z luft:+cell-extent+ 1))))
                            (if (and placement
                                     (not (eq :none (terrain:material-kind-body
                                                    (terrain:material-placement-kind placement)))))
                                0 1)))))))
        (dolist (triangle (if render::*terrain-bevels-p*
                             (luft:star-atlas-owned-triangles star)
                             (luft:star-atlas-flat-owned-triangles star)))
          (when (equal normal (render::star-triangle-normal-key triangle))
            (let ((points (loop for point in triangle
                                collect (mapcar (lambda (a b) (+ a (/ b 8))) anchor point))))
              (when (and (every (lambda (p) (= plane (nth axis p))) points)
                         (every (lambda (p) (every (lambda (p b) (<= b p (1+ b))) p base)) points))
                (loop for a in points for b in (append (rest points) (list (first points)))
                      do (if (gethash (list b a) edges)
                             (remhash (list b a) edges)
                             (setf (gethash (list a b) edges) t)))))))))
    (when (plusp (hash-table-count edges))
      (let* ((first (first (loop for edge being the hash-keys of edges collect edge)))
             (point (first first))
             (boundary nil))
        (loop repeat (hash-table-count edges)
              do (push (apply #'vec3:make-vec3 point) boundary)
                 (let ((edge (loop for edge being the hash-keys of edges
                                   when (equal (first edge) point) return edge)))
                   (unless edge (return-from target-face-boundary nil))
                   (setf point (second edge)))
              when (equal point (first first)) return (nreverse boundary))))))

(defclass target-marker (render::gpu-resource-owner)
  ((pipeline :accessor marker-pipeline)
   (buffers :initform (make-hash-table :test #'eq) :reader marker-buffers)))

(defmethod render:viewer-overlay-stage ((marker target-marker)) :hud)

(defmethod render:refresh-viewer-overlay ((marker target-marker) viewer)
  (declare (ignore viewer))
  (luv:refresh-live-shader-pipeline (marker-pipeline marker)))

(defmethod render:attach-viewer-overlay ((marker target-marker) viewer)
  (let ((device (render::viewer-device viewer)))
    (render::with-gpu-construction (marker)
      (let ((layout (render::own-gpu-resource
                     marker device
                     ;; Vulkan's HAL supports nonempty layouts; like the old
                     ;; crosshair this shader does not read the reserved row.
                     (luv:make-bind-group-layout-descriptor
                      :entries '((:binding 0 :type :uniform-buffer))))))
        (setf (marker-pipeline marker)
              (luv:make-live-shader-pipeline
               :device device :layout layout :role :block-crosshair :vertex-role :block-crosshair
               :label "Luvcraft target face on Luft"
               :vertex-buffers '((:array-stride 28
                                  :attributes ((:shader-location 0 :offset 0 :format :float32x3)
                                               (:shader-location 1 :offset 12 :format :float32x4))))
               :target-format (luv:canvas-format (render::viewer-context viewer))
               :target-blend :premultiplied-alpha :primitive '(:topology :triangle-list)))))))

(defmethod render:release-viewer-overlay ((marker target-marker))
  (when (slot-boundp marker 'pipeline)
    (luv:release-live-shader-pipeline (marker-pipeline marker)))
  (maphash (lambda (key buffer) (declare (ignore key)) (luv:destroy buffer)) (marker-buffers marker))
  (clrhash (marker-buffers marker))
  (render::release-owned-gpu-resources marker))

(defmethod render:evict-viewer-overlay-frame-key ((marker target-marker) key)
  (let ((buffer (gethash key (marker-buffers marker))))
    (when buffer (luv:destroy buffer) (remhash key (marker-buffers marker)))))

(defmethod render:encode-viewer-overlay ((marker target-marker) viewer pass surface)
  (when (and (render:viewer-pointer-captured-p viewer)
             (null (render:viewer-modal-focus viewer)))
    (let* ((inspection (render::viewer-inspection viewer))
           (corners (and inspection
                         (target-face-boundary (render:viewer-source viewer)
                                               (render:site-inspection-site inspection)
                                               (render:site-inspection-cell inspection))))
           (camera (render:viewer-camera viewer)))
      (when corners
        (destructuring-bind (width height &rest ignored) (luv:gpu-texture-size surface)
          (declare (ignore ignored))
          (multiple-value-bind (right up forward) (render:camera-basis camera)
            (let* ((vertices (luvcraft::make-face-marker-vertices
                              corners (render:camera-position camera) right up forward
                              (/ (tan (/ (render:camera-field-of-view camera) 2))) width height))
                   (key (luv:canvas-frame-resource-key (render::viewer-context viewer) surface))
                   (buffer (or (gethash key (marker-buffers marker))
                               (setf (gethash key (marker-buffers marker))
                                     (luv:create (render::viewer-device viewer)
                                                 (luv:make-buffer-descriptor
                                                  :label "target rim" :size (* 4 7 30 64)
                                                  :usage '(:vertex :copy-dst)))))))
              (when (plusp (length vertices))
                (luv:write-buffer buffer vertices)
                (luv:set-pipeline pass (luv:live-shader-pipeline-native-pipeline (marker-pipeline marker)))
                (luv:set-vertex-buffer pass 0 buffer)
                (luv:draw pass (/ (length vertices) 7))))))))))

(defmethod initialize-instance :after ((viewer feedback-game-state) &key)
  (when (render:viewer-canvas viewer)
    (render:add-viewer-overlay viewer (make-instance 'target-marker))))

(in-package #:luft.render.shaders)

(define-live-shader particle-fragment-specification
    (:stage :fragment
     :inputs ((normal :vec3 :location 0)
              (tone :vec3 :location 1 :interpolation :flat)
              (current :vec4 :location 2) (previous :vec4 :location 3))
     :outputs ((color-output :vec4 :location 0) (motion-output :vec2 :location 1))
     :resources ((camera-state :uniform-block :binding 0 :members #.*scene-uniform-members*)))
  ;; Fragments must read as their material even with the sun behind them.
  ;; The old effect's near-unit per-fragment shade similarly prevents debris
  ;; from becoming black silhouettes. Bodies retain their ordinary lighting.
  (let* ((n (normalize normal))
         (sun (representation (swizzle sun-vector :xyz)))
         (direct (smoothstep 0.0 0.72 (max 0.0 (dot n sun))))
         (light (+ (* (representation (swizzle sun-color-vector :xyz)) direct)
                   (* (representation (swizzle sky-color-vector :xyz)) 0.5)
                   (* (representation (swizzle ground-color-vector :xyz)) 0.2)))
         (shade (max light (vec3 0.45 0.45 0.45))))
    (set-output color-output (vec4 (* tone shade) 1.0))
    (set-output motion-output (mesh-temporal-motion previous current))))
