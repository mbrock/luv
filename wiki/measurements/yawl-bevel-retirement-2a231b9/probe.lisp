;;;; Disposable shared-corner bevel retirement; never load in a managed image.
(in-package #:luft.render.shaders)

(define-shader-function retirement-world
    (world eye right up forward projection inspection limit)
  ;; The radius encloses every atlas offset from its canonical lattice corner.
  ;; Bound perspective displacement in pixels, including off-axis enlargement.
  ;; Using one corner value for all incident vertices preserves correspondence.
  (let* ((point (representation world))
         (anchor (floor (+ point (vec3 0.5 0.5 0.5))))
         (relative (- anchor (swizzle (representation eye) :xyz)))
         (z (dot relative (swizzle (representation forward) :xyz)))
         (safe-z (max 0.001 z))
         (x (dot relative (swizzle (representation right) :xyz)))
         (y (dot relative (swizzle (representation up) :xyz)))
         (focal (/ (abs (swizzle projection :xy))
                   (* (swizzle (representation inspection) :zw) 2.0)))
         ;; ||[I,-view.xy/view.z]|| = sqrt(1 + |view.xy/view.z|²).
         (radial (/ (vec2 x y) safe-z))
         (bound (/ (* 0.176777 (max (swizzle focal :x) (swizzle focal :y))
                      (sqrt (+ 1.0 (dot radial radial))))
                   (max 0.001 (- z 0.176777))))
         (weight (clamp (/ (- bound limit) limit) 0.0 1.0)))
    (assume-quantity (+ anchor (* (- point anchor) weight))
                     :quantity quantities:world-position :unit quantities:cell)))

(defparameter *retirement-original-vertex* (symbol-function 'terrain-vertex-specification))

(defun retirement-vertex (limit)
  (let* ((original (funcall *retirement-original-vertex*))
         (source (luv.shader:shader-object-source-form original))
         (body (copy-tree (cdddr source)))
         (bindings (second (first body)))
         (world (assoc 'world bindings)))
    (assert world)
    ;; Candidate site W is the source index in the existing sorted sidecars.
    (setf (second (assoc 'appearance bindings))
          '(buffer-element terrain-appearances (swizzle site :w))
          (second (assoc 'lights bindings))
          '(buffer-element terrain-lights (swizzle site :w))
          (second world)
          `(retirement-world ,(second world) camera-position camera-right camera-up
                             camera-forward camera-projection inspection-parameters ,limit))
    (luv.shader:parse-shader-specification (second source) (third source) body)))

(in-package #:luft.render)

(defvar *retirement-limit* nil)
(defvar *retirement-residents* (make-hash-table :test #'eq))
(defvar *retirement-drawings* (make-hash-table :test #'eq))
(defvar *retirement-last-data* nil)

(defstruct retirement-resident source words buffer counts)

(declaim (inline retirement-bound))
(defun retirement-bound (x y z data)
  "Upper bound on every corner's pixel displacement in this site's unit cube."
  (declare (type single-float x y z)
           (type (simple-array single-float (*)) data)
           (optimize (speed 3) (safety 1)))
  (let* ((rx (- (+ x 0.5) (aref data 0)))
         (ry (- (+ y 0.5) (aref data 1)))
         (rz (- (+ z 0.5) (aref data 2)))
         (vx (+ (* rx (aref data 4)) (* ry (aref data 5)) (* rz (aref data 6))))
         (vy (+ (* rx (aref data 8)) (* ry (aref data 9)) (* rz (aref data 10))))
         (vz (- (+ (* rx (aref data 12)) (* ry (aref data 13)) (* rz (aref data 14)))
                0.866026))
         (safe-z (max 0.001 vz))
         (sx (/ (+ (abs vx) 0.866026) safe-z))
         (sy (/ (+ (abs vy) 0.866026) safe-z))
         (focal (max (/ (abs (aref data 16)) (* 2.0 (aref data 50)))
                     (/ (abs (aref data 17)) (* 2.0 (aref data 51))))))
    (/ (* 0.176777 focal (sqrt (+ 1.0 (* sx sx) (* sy sy))))
       (max 0.001 (- vz 0.176777)))))

(defun retirement-partition (state runs data limit)
  "Partition each star run, preserving original sorted sidecar indices."
  (declare (type single-float limit)
           (type (simple-array single-float (*)) data)
           (optimize (speed 3) (safety 1)))
  (let ((source (retirement-resident-source state))
        (words (retirement-resident-words state))
        (counts (retirement-resident-counts state)))
    (declare (type (simple-array (unsigned-byte 32) (*)) source words)
             (type (simple-array fixnum (256)) counts))
    (dolist (run runs)
      (destructuring-bind (star first count) run
        (declare (type fixnum star first count))
        (let ((near first) (far (+ first count -1)))
          (declare (type fixnum near far))
          (loop for index fixnum from first below (+ first count)
                for base fixnum = (* 4 index)
                for flat-p = (< (retirement-bound (float (aref source base))
                                                 (float (aref source (+ base 1)))
                                                 (float (aref source (+ base 2))) data)
                               (- limit 0.001))
                for destination fixnum = (* 4 (if flat-p (prog1 far (decf far))
                                                  (prog1 near (incf near))))
                do (dotimes (axis 3)
                     (setf (aref words (+ destination axis)) (aref source (+ base axis))))
                   (setf (aref words (+ destination 3)) index))
          (setf (aref counts star) (- near first)))))
    (write-buffer (retirement-resident-buffer state) words)))

(let ((original (symbol-function '%upload-render-population)))
  (setf (symbol-function '%upload-render-population)
        (lambda (renderer population)
          (let ((resident (funcall original renderer population)))
            (multiple-value-bind (source)
                (sort-sites-by-star (render-population-instance-words population)
                                    (render-population-appearance-words population)
                                    (render-population-light-words population))
              (let ((state
                      (make-retirement-resident
                       :source source
                       :words (make-array (length source) :element-type '(unsigned-byte 32))
                       :counts (make-array 256 :element-type 'fixnum :initial-element 0)
                       :buffer (own-gpu-resource
                                resident (renderer-device renderer)
                                (make-buffer-descriptor :label "bevel retirement site partition"
                                                        :size (max 16 (* 4 (length source)))
                                                        :usage '(:storage :copy-dst))))))
                (setf (gethash (resident-population-instance-buffer resident) *retirement-residents*) state
                      (gethash resident *retirement-residents*) state)))
            resident))))

(defmethod make-terrain-binding :around
    ((drawing terrain-drawing) device sites camera
     &rest arguments &key shadow-p &allow-other-keys)
  (if (and *retirement-limit* (not shadow-p))
      (apply #'call-next-method drawing device
             (retirement-resident-buffer (gethash sites *retirement-residents*)) camera arguments)
      (call-next-method)))

(defmethod encode-terrain :around
    ((drawing terrain-drawing) pass binding resident &key shadow-p)
  (if (or shadow-p (null *retirement-limit*))
      (call-next-method)
      (let* ((state (gethash resident *retirement-residents*))
             (ranges (terrain-star-ranges drawing))
             (indices (terrain-scene-indices drawing)))
        ;; Per-frame CPU selection and upload are deliberately inside encoding.
        (setf *retirement-last-data* *frame-uniform-data*)
        (retirement-partition state (resident-population-star-runs resident)
                              *frame-uniform-data* *retirement-limit*)
        (set-pipeline pass (program-pipeline (terrain-scene-program drawing)))
        (set-bind-group pass 0 binding)
        (loop for (star first count) in (resident-population-star-runs resident)
              for near = (aref (retirement-resident-counts state) star)
              do (loop for tier below 2
                       for start in (list first (+ first near))
                       for instances in (list near (- count near))
                       for range = (aref ranges (+ star (* 256 tier)))
                       when (and (plusp instances) (plusp (aref range 1)))
                         do (draw-indexed pass indices :uint32 (aref range 1) instances
                                          (aref range 0) 0 start))))))

(defun retirement-tables ()
  "Join the two existing scene atlases; retain original shadow data."
  (let* ((control (multiple-value-list (star-patch-tables t)))
         (flat (multiple-value-list (star-patch-tables nil)))
         (vertices (concatenate '(simple-array (unsigned-byte 32) (*)) (first control) (first flat)))
         (normals (concatenate '(simple-array single-float (*)) (second control) (second flat)))
         (indices (concatenate '(simple-array (unsigned-byte 32) (*)) (third control) (third flat)))
         (ranges (concatenate 'vector (sixth control) (sixth flat))))
    (loop for i from (+ (length (first control)) 3) below (length vertices) by 4
          do (incf (aref vertices i) (ash (/ (length (second control)) 4) 16)))
    (loop for i from (length (third control)) below (length indices)
          do (incf (aref indices i) (/ (length (first control)) 4)))
    (loop for star from 256 below 512
          do (incf (aref (aref ranges star) 0) (length (third control))))
    (values vertices normals indices (fourth control) (fifth control) ranges)))
