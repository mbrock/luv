(in-package #:luft.render)

(defparameter *wireframe* 0.0
  "Global construction-edge strength.  The atelier toggles it between 0 and 1.")

(defclass scene ()
  ((solid :initarg :solid :reader scene-solid)
   (architecture-cells :initarg :architecture-cells
                       :reader scene-architecture-cells))
  (:documentation
   "One authored solid and the cells whose faces read as cut stone.

The solid remains LUFT's dense topological truth.  The sparse architectural
set is only the semantic distinction the four-bit face stock needs; it does
not put objects or material records on every cell."))

(defclass scene-builder ()
  ((domain :initarg :domain :reader scene-builder-domain)
   (cells :initform (make-hash-table :test #'eql) :reader scene-builder-cells)
   (architecture-cells :initform (make-hash-table :test #'eql)
                       :reader scene-builder-architecture-cells)))

(defun make-scene-builder (&key (horizontal-bits 6))
  (make-instance 'scene-builder
                 :domain (luft:make-world-domain
                          :x-bits horizontal-bits :y-bits horizontal-bits)))

(defun scene-builder-cell (builder x y z &key (solid-p t) architecture-p)
  (when (<= 0 z 254)
    (let ((site (luft:make-site (scene-builder-domain builder) x y z
                                luft:+cell-extent+ 1)))
      (if solid-p
          (progn
            (setf (gethash site (scene-builder-cells builder)) t)
            (when architecture-p
              (setf (gethash site (scene-builder-architecture-cells builder))
                    t)))
          (progn
            (remhash site (scene-builder-cells builder))
            (remhash site (scene-builder-architecture-cells builder))))))
  builder)

(defun scene-builder-box (builder x0 x1 y0 y1 z0 z1
                           &key (solid-p t) architecture-p)
  (loop for z from z0 to z1 do
    (loop for y from y0 to y1 do
      (loop for x from x0 to x1 do
        (scene-builder-cell builder x y z :solid-p solid-p
                                           :architecture-p architecture-p))))
  builder)

(defun scene-builder-disc (builder cx cy radius z0 z1
                            &key (solid-p t) architecture-p)
  (let ((limit (expt (+ radius 0.5) 2)))
    (loop for x from (- cx (ceiling radius)) to (+ cx (ceiling radius)) do
      (loop for y from (- cy (ceiling radius)) to (+ cy (ceiling radius))
            for dx = (- (+ x 0.5) (+ cx 0.5))
            for dy = (- (+ y 0.5) (+ cy 0.5))
            when (<= (+ (* dx dx) (* dy dy)) limit)
              do (loop for z from z0 to z1 do
                   (scene-builder-cell builder x y z :solid-p solid-p
                                                      :architecture-p
                                                      architecture-p)))))
  builder)

(defun scene-builder-ring (builder cx cy inner outer z0 z1
                            &key (solid-p t) architecture-p)
  (let ((low (expt (+ inner 0.5) 2))
        (high (expt (+ outer 0.5) 2)))
    (loop for x from (- cx (ceiling outer)) to (+ cx (ceiling outer)) do
      (loop for y from (- cy (ceiling outer)) to (+ cy (ceiling outer))
            for dx = (- (+ x 0.5) (+ cx 0.5))
            for dy = (- (+ y 0.5) (+ cy 0.5))
            for distance = (+ (* dx dx) (* dy dy))
            when (and (< low distance) (<= distance high))
              do (loop for z from z0 to z1 do
                   (scene-builder-cell builder x y z :solid-p solid-p
                                                      :architecture-p
                                                      architecture-p)))))
  builder)

(defun arch-rise (offset radius)
  (let ((square (- (* radius radius) (* offset offset))))
    (if (plusp square) (round (sqrt square)) 0)))

(defun scene-builder-carve-arch
    (builder centre floor springing radius across &key (axis :x))
  (destructuring-bind (near . far) across
    (loop for offset from (- radius) to radius
          for rise = (arch-rise offset radius)
          when (plusp rise) do
            (loop for z from floor below (+ springing rise) do
              (loop for other from near to far do
                (if (eq axis :x)
                    (scene-builder-cell builder (+ centre offset) other z
                                        :solid-p nil)
                    (scene-builder-cell builder other (+ centre offset) z
                                        :solid-p nil))))))
  builder)

(defun scene-builder-corbel (builder x0 x1 y0 y1 z courses)
  (loop for course from 0 below courses
        for out = (1+ course)
        do (scene-builder-box builder (- x0 out) (+ x1 out)
                              (- y0 out) (+ y1 out)
                              (+ z course) (+ z course)
                              :architecture-p t))
  builder)

(defun scene-builder-crenellate (builder x0 x1 y0 y1 z)
  (loop for x from x0 to x1 do
    (loop for y from y0 to y1
          when (and (or (= x x0) (= x x1) (= y y0) (= y y1))
                    (zerop (mod (+ x y) 2)))
            do (scene-builder-cell builder x y z :architecture-p t)
               (scene-builder-cell builder x y (1+ z) :architecture-p t)))
  builder)

(defun finish-scene-builder (builder)
  (let* ((cells (scene-builder-cells builder))
         (chain-builder
           (luft:make-chain-builder (scene-builder-domain builder)
                                    :initial-capacity (hash-table-count cells))))
    (maphash (lambda (site present-p)
               (declare (ignore present-p))
               (luft:chain-builder-add-site chain-builder site))
             cells)
    (make-instance 'scene
                   :solid (luft:finish-chain-builder chain-builder)
                   :architecture-cells
                   (scene-builder-architecture-cells builder))))

(defun make-manifold-spike-scene ()
  "Three isolated singular-star fixtures for the manifold-sheet spike.

The plots exercise an edge-touching pair, a corner-touching pair, and the
four-sheet parity star.  Nothing else in the scene can hide their junctions."
  (let ((builder (make-scene-builder :horizontal-bits 6)))
    (labels ((place-star (mask centre-x)
               (dotimes (sample 8)
                 (when (logbitp sample mask)
                   (scene-builder-cell
                    builder
                    (+ centre-x (if (logbitp 0 sample) 0 -1))
                    (+ 10 (if (logbitp 1 sample) 0 -1))
                    (+ 6 (if (logbitp 2 sample) 0 -1)))))))
      (place-star #x06 10)
      (place-star #x18 16)
      (place-star #x69 22))
    (finish-scene-builder builder)))

(defun make-mountain-sanctuary-scene ()
  "A Lonely-Mountains landscape carrying a bridge and walled sanctuary.

This is the old Holm's architectural sentence with its material menagerie
removed: a shore, channel and high rock; a two-arched stone bridge; a gate,
curtain wall, paired turrets and an arcaded hall."
  (let* ((builder (make-scene-builder :horizontal-bits 6))
         (shore 11) (water 2) (plateau 19) (deck 13)
         (springing 7) (radius 4) (across (cons 27 32)))
    (dotimes (x 64)
      (dotimes (y 64)
        (let* ((west (+ 3 (round (* 1.8 (sin (/ y 6.0))))))
               (east (- 60 (round (* 2.2 (cos (/ y 8.0))))))
               (land-p (and (<= 3 y 61) (<= west x east)))
               (height
                (floor
                 (cond
                   ((< y 14)
                    (max water
                         (+ shore (* 1.4 (sin (/ x 11.0)))
                            (- (* 1.6 (max 0 (- y 9)))))))
                   ((>= y 36)
                    (let* ((edge (+ 2.0 (* 3.0 (sin (/ x 9.0)))
                                       (* 1.5 (sin (/ x 3.7)))))
                           (inland (- y 36 edge)))
                      (if (>= inland 0)
                          (+ plateau (* 1.3 (sin (/ x 12.0))
                                           (cos (/ y 13.0))))
                          (max water (+ plateau (* 9.0 inland))))))
                   (t water)))))
          (when land-p
            (loop for z below (max 1 height) do
              (scene-builder-cell builder x y z))))))
    (dolist (y '(15 25 35))
      (scene-builder-box builder 26 33 (1- y) (1+ y) water springing
                         :architecture-p t))
    (scene-builder-box builder 27 32 12 38 water (1- deck)
                       :architecture-p t)
    (dolist (arch '(20 30))
      (scene-builder-carve-arch builder arch (1+ water) springing radius
                                across :axis :y))
    (scene-builder-corbel builder 27 32 8 42 (1- deck) 1)
    (scene-builder-box builder 25 34 6 44 deck deck :architecture-p t)
    (loop for y from 6 to 44 do
      (scene-builder-cell builder 25 y (1+ deck) :architecture-p t)
      (scene-builder-cell builder 34 y (1+ deck) :architecture-p t)
      (when (zerop (mod y 5))
        (scene-builder-cell builder 25 y (+ deck 2) :architecture-p t)
        (scene-builder-cell builder 34 y (+ deck 2) :architecture-p t)))
    (scene-builder-box builder 26 33 38 47 deck (+ plateau 5) :solid-p nil)
    (loop for step from 0 to 6 for y from 39 do
      (scene-builder-box builder 26 33 y y 0 (+ deck step)
                         :architecture-p t))
    (scene-builder-box builder 14 46 45 47 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-box builder 14 16 45 61 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-box builder 44 46 45 61 plateau (+ plateau 6)
                       :architecture-p t)
    (scene-builder-corbel builder 14 46 45 61 (+ plateau 7) 1)
    (scene-builder-crenellate builder 13 47 44 62 (+ plateau 8))
    (scene-builder-carve-arch builder 30 plateau (+ plateau 3) 3
                              (cons 45 47))
    (dolist (corner '((15 46) (45 46)))
      (destructuring-bind (cx cy) corner
        (scene-builder-disc builder cx cy 5 (1- plateau) plateau
                            :architecture-p t)
        (scene-builder-ring builder cx cy 2 4 plateau (+ plateau 9)
                            :architecture-p t)
        (scene-builder-ring builder cx cy 2 5 (+ plateau 10) (+ plateau 11)
                            :architecture-p t)
        (scene-builder-disc builder cx cy 3 (+ plateau 11) (+ plateau 11)
                            :architecture-p t)))
    (scene-builder-box builder 20 40 55 60 plateau (+ plateau 5)
                       :architecture-p t)
    (scene-builder-box builder 21 39 56 59 (1+ plateau) (+ plateau 5)
                       :solid-p nil)
    (dolist (bay '(25 30 35))
      (scene-builder-carve-arch builder bay (1+ plateau) (+ plateau 3) 2
                                (cons 55 55)))
    (finish-scene-builder builder)))

(defun make-miter-study-scene ()
  "Build the wall-side stepped mountain used to judge mixed miters. #Z5NDTA

The two L-shaped terraces retain five-, six-, and seven-cell vertex stars in
one architectural context.  Their back edges meet a continuous wall so the
same view also retains the truncated wall miter preserved by #DJK8HW."
  (let ((builder (make-scene-builder :horizontal-bits 5)))
    ;; Broad plinth and continuous back wall.
    (scene-builder-box builder 2 14 2 8 0 1 :architecture-p t)
    (scene-builder-box builder 2 14 8 9 0 7 :architecture-p t)
    ;; One isolated terrain cell makes the ordinary boulder-on-ground contact
    ;; inspectable beside the architectural mixed-miter cases.
    (scene-builder-cell builder 13 4 2)
    ;; The lower L supplies the outie, straight six-cell run, innie, and the
    ;; first wall termination.  The upper L repeats them without isolation.
    (scene-builder-box builder 4 11 5 7 2 2 :architecture-p t)
    (scene-builder-box builder 4 8 3 4 2 2 :architecture-p t)
    (scene-builder-box builder 5 10 6 7 3 3 :architecture-p t)
    (scene-builder-box builder 5 7 4 5 3 3 :architecture-p t)
    (finish-scene-builder builder)))

(defun face-solid-cell (solid face)
  "Return the occupied cell incident to boundary FACE and which side it is on."
  (let* ((domain (luft:chain-domain solid))
         (extent (luft:site-extent face))
         (axis (cond ((= extent luft:+xy-face-extent+) :z)
                     ((= extent luft:+xz-face-extent+) :y)
                     (t :x)))
         (x (luft:site-x face))
         (y (luft:site-y face))
         (z (luft:site-z face))
         (back-x (if (eq axis :x) (1- x) x))
         (back-y (if (eq axis :y) (1- y) y))
         (back-z (if (eq axis :z) (1- z) z)))
    (if (= 1 (luft:chain-cell-occupancy-bit solid x y z))
        (values (luft:make-site domain x y z luft:+cell-extent+ 1)
                axis :forward)
        (values (luft:make-site domain back-x back-y back-z
                                luft:+cell-extent+ 1)
                axis :backward))))

(defun scene-face-stock (scene face)
  "The four-colour paper palette slot for FACE in SCENE."
  (multiple-value-bind (cell axis side)
      (face-solid-cell (scene-solid scene) face)
    (cond ((gethash cell (scene-architecture-cells scene)) 3)
          ((not (eq axis :z)) 1)
          ((eq side :backward) 0)
          (t 2))))

(defun scene-chamfer-stock (stocks)
  "Resolve one whole chamfer from its incident face STOCKS.

The paper palette's terrain top is grass (0), terrain side is soil (1), and
terrain underside is dark soil (2).  A chamfer faces out through the terrain
side, so every non-architectural chamfer is soil regardless of which subset
of terrain faces generated its triangles.  Architecture (3) is an explicit
authored material and wins at a terrain--architecture join."
  (if (member 3 stocks) 3 1))

(defun default-face-stock (face)
  (mod (+ (luft:site-x face) (* 2 (luft:site-y face))
          (* 3 (luft:site-z face)) (luft:site-extent face))
       4))

(defun make-render-mesh
    (source &key stock-function chamfer-stock-function
                 (bevel-width luft:+mesh-bevel-width+))
  "Classify SOURCE into the face, edge, and vertex template-instance ABI."
  (let* ((scene (and (typep source 'scene) source))
         (solid (if scene (scene-solid source) source))
         (stock-function (or stock-function
                             (and scene
                                  (lambda (face) (scene-face-stock scene face)))
                             #'default-face-stock))
         (chamfer-stock-function
           (or chamfer-stock-function
               (if scene
                   #'scene-chamfer-stock
                   (lambda (stocks) (first stocks))))))
    (check-type solid luft:chain)
    (zone (:luft/rematerialize :value (luft:chain-count solid))
      (luft:make-surface-mesh solid :stock-function stock-function
                                   :chamfer-stock-function
                                   chamfer-stock-function
                                   :bevel-width bevel-width))))

(defparameter *gallery*
  ;; Each entry is one isolated complex, named by the star configuration it
  ;; is there to exhibit.  Cells are offsets from the entry's plot origin.
  '((:one-cell
     ((0 0 0)))
    (:face-pair
     ((0 0 0) (1 0 0)))
    (:edge-pair
     ((0 0 0) (1 1 0)))
    (:corner-pair
     ((0 0 0) (1 1 1)))
    (:l-tromino
     ((0 0 0) (1 0 0) (0 1 0)))
    (:square
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)))
    (:stair
     ((0 0 0) (1 0 0) (1 0 1)))
    (:concave-vertex
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1) (0 1 1)))
    (:six-of-eight
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1)))
    (:full-block
     ((0 0 0) (1 0 0) (0 1 0) (1 1 0)
      (0 0 1) (1 0 1) (0 1 1) (1 1 1)))
    (:tower
     ((0 0 0) (0 0 1) (0 0 2)))
    (:cross
     ((1 1 0) (0 1 0) (2 1 0) (1 0 0) (1 2 0) (1 1 1))))
  "Small unconnected complexes, one per interesting occupancy star.")

(defparameter *gallery-columns* 4)
(defparameter *gallery-pitch* 6)

(defun gallery-plot-origin (index)
  "Return the lattice origin of gallery entry INDEX, laid out on a grid."
  (multiple-value-bind (row column) (floor index *gallery-columns*)
    (values (+ 2 (* column *gallery-pitch*))
            (+ 2 (* row *gallery-pitch*))
            1)))

(defun make-gallery-solid (&key (entries *gallery*))
  "Build one chain holding every gallery complex on its own plot.

Nothing here touches anything else, so each patch shown is entirely the
consequence of its own occupancy star and can be read on its own."
  (let* ((domain (luft:make-world-domain :x-bits 6 :y-bits 6))
         (builder (luft:make-chain-builder domain :initial-capacity 128))
         (seen (make-hash-table :test #'eql)))
    (loop for entry in entries
          for index from 0
          do (multiple-value-bind (ox oy oz) (gallery-plot-origin index)
               (loop for (dx dy dz) in (second entry)
                     for site = (luft:make-site domain (+ ox dx) (+ oy dy)
                                                (+ oz dz)
                                                luft:+cell-extent+ 1)
                     unless (gethash site seen)
                       do (setf (gethash site seen) t)
                          (luft:chain-builder-add-site builder site))))
    (luft:finish-chain-builder builder)))

(defun gallery-plot-report (&key (entries *gallery*))
  "Print where each gallery complex sits, so a capture can be aimed at one."
  (loop for entry in entries
        for index from 0
        do (multiple-value-bind (x y z) (gallery-plot-origin index)
             (format t "~&~2D ~24A origin ~D,~D,~D~%"
                     index (first entry) x y z)))
  (values))

(defun make-demo-solid ()
  "Make a compact stair-and-bridge solid with convex and concave stars."
  (let* ((domain (luft:make-world-domain :x-bits 5 :y-bits 5))
         (builder (luft:make-chain-builder domain :initial-capacity 96))
         (seen (make-hash-table :test #'eql)))
    (labels ((cell (x y z)
               (let ((site
                       (luft:make-site domain x y z luft:+cell-extent+ 1)))
                 (unless (gethash site seen)
                   (setf (gethash site seen) t)
                   (luft:chain-builder-add-site builder site))))
             (box (x0 x1 y0 y1 z0 z1)
               (loop for z from z0 to z1 do
                 (loop for y from y0 to y1 do
                   (loop for x from x0 to x1 do (cell x y z))))))
      (box 4 11 4 11 1 1)
      (box 5 10 5 10 2 2)
      (box 6 6 6 6 3 5)
      (box 9 9 6 6 3 5)
      (box 6 6 9 9 3 5)
      (box 9 9 9 9 3 5)
      (box 6 9 6 6 5 5)
      (box 6 9 9 9 5 5)
      (box 7 8 7 8 3 3))
    (luft:finish-chain-builder builder)))

(defstruct (mesh-slot (:constructor %make-mesh-slot) (:copier nil))
  "One mesh's GPU residency: buffers and bind groups for its three streams."
  (mesh nil)
  (template-buffer nil)
  (face-buffer nil)
  (band-buffer nil)
  (fan-buffer nil)
  (face-group nil)
  (band-group nil)
  (fan-group nil)
  (lattice-point-buffer nil)
  (lattice-point-count 0)
  (lattice-point-group nil))

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   ;; Chunk key (or any EQL key) to resident MESH-SLOT; SLOT-ORDER is the
   ;; sorted key list frames draw in.
   (mesh-slots :initform (make-hash-table :test #'eql)
               :reader renderer-mesh-slots)
   (slot-order :initform nil :accessor renderer-slot-order)
   (camera-buffer :initarg :camera-buffer :accessor renderer-camera-buffer)
   (layout :initarg :layout :accessor renderer-layout)
   (vertex-module :initarg :vertex-module :accessor renderer-vertex-module)
   (fragment-module :initarg :fragment-module :accessor renderer-fragment-module)
   (pipeline :initarg :pipeline :accessor renderer-pipeline)
   (lattice-point-layout :initarg :lattice-point-layout
                         :accessor renderer-lattice-point-layout)
   (lattice-point-vertex-module :initarg :lattice-point-vertex-module
                                :accessor renderer-lattice-point-vertex-module)
   (lattice-point-fragment-module :initarg :lattice-point-fragment-module
                                  :accessor renderer-lattice-point-fragment-module)
   (lattice-point-pipeline :initarg :lattice-point-pipeline
                           :accessor renderer-lattice-point-pipeline)
   (color-format :initarg :color-format :reader renderer-color-format)
   (temporal-p :initarg :temporal-p :reader renderer-temporal-p)
   (depth-texture :initform nil :accessor renderer-depth-texture)
   (depth-view :initform nil :accessor renderer-depth-view)
   (scene-texture :initform nil :accessor renderer-scene-texture)
   (scene-view :initform nil :accessor renderer-scene-view)
   (motion-texture :initform nil :accessor renderer-motion-texture)
   (motion-view :initform nil :accessor renderer-motion-view)
   (resolved-texture :initform nil :accessor renderer-resolved-texture)
   (resolved-view :initform nil :accessor renderer-resolved-view)
   (temporal-scaler :initform nil :accessor renderer-temporal-scaler)
   (present-layout :initform nil :accessor renderer-present-layout)
   (present-bind-group :initform nil :accessor renderer-present-bind-group)
   (present-vertex-module :initform nil
                          :accessor renderer-present-vertex-module)
   (present-fragment-module :initform nil
                            :accessor renderer-present-fragment-module)
   (present-pipeline :initform nil :accessor renderer-present-pipeline)
   (sampler :initform nil :accessor renderer-sampler)
   (extent :initform nil :accessor renderer-extent)
   (frame-index :initform 0 :accessor renderer-frame-index)
   (previous-view :initform nil :accessor renderer-previous-view)
   (history-valid-p :initform nil :accessor renderer-history-valid-p)
   (history-used-p :initform nil :accessor renderer-history-used-p)))

(defun metal-temporal-device-p (device)
  #+darwin (typep device 'metal-gpu-device)
  #-darwin (declare (ignore device))
  #-darwin nil)

(defun destroy-renderer-targets (renderer)
  (dolist (resource
            (list (renderer-present-bind-group renderer)
                  (renderer-temporal-scaler renderer)
                  (renderer-resolved-view renderer)
                  (renderer-resolved-texture renderer)
                  (renderer-motion-view renderer)
                  (renderer-motion-texture renderer)
                  (renderer-scene-view renderer)
                  (renderer-scene-texture renderer)
                  (renderer-depth-view renderer)
                  (renderer-depth-texture renderer)))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-present-bind-group renderer) nil
        (renderer-temporal-scaler renderer) nil
        (renderer-resolved-view renderer) nil
        (renderer-resolved-texture renderer) nil
        (renderer-motion-view renderer) nil
        (renderer-motion-texture renderer) nil
        (renderer-scene-view renderer) nil
        (renderer-scene-texture renderer) nil
        (renderer-depth-view renderer) nil
        (renderer-depth-texture renderer) nil))

(defun create-frame-targets (renderer extent)
  (let* ((device (renderer-device renderer))
         (temporal-p (renderer-temporal-p renderer))
         (scaler
           (and temporal-p
                (create device
                        (make-temporal-scaler-descriptor
                         :label "luft MetalFX temporal scaler"
                         :input-size extent :output-size extent))))
         (usage (lambda (base extra)
                  (remove-duplicates (append base extra))))
         (depth
           (create device
                   (make-texture-descriptor
                    :label "luft temporal depth" :size extent :dimensions :2d
                    :format :depth32-float
                    :usage (funcall usage '(:render-attachment)
                                    (and scaler
                                         (gpu-temporal-scaler-depth-usage
                                          scaler))))))
         (depth-view
           (create device (make-texture-view-descriptor :texture depth))))
    (setf (renderer-temporal-scaler renderer) scaler
          (renderer-depth-texture renderer) depth
          (renderer-depth-view renderer) depth-view
          (renderer-extent renderer) (copy-list extent)
          (renderer-frame-index renderer) 0
          (renderer-previous-view renderer) nil
          (renderer-history-valid-p renderer) nil
          (renderer-history-used-p renderer) nil)
    (when temporal-p
      (let* ((scene
               (create device
                       (make-texture-descriptor
                        :label "luft temporal color" :size extent :dimensions :2d
                        :format :rgba16-float
                        :usage (funcall usage '(:render-attachment)
                                        (gpu-temporal-scaler-color-usage scaler)))))
             (motion
               (create device
                       (make-texture-descriptor
                        :label "luft temporal motion" :size extent :dimensions :2d
                        :format :rg16-float
                        :usage (funcall usage '(:render-attachment)
                                        (gpu-temporal-scaler-motion-usage scaler)))))
             (resolved
               (create device
                       (make-texture-descriptor
                        :label "luft temporal resolve" :size extent :dimensions :2d
                        :format :rgba16-float
                        :usage (funcall usage '(:texture-binding)
                                        (gpu-temporal-scaler-output-usage scaler)))))
             (scene-view
               (create device (make-texture-view-descriptor :texture scene)))
             (motion-view
               (create device (make-texture-view-descriptor :texture motion)))
             (resolved-view
               (create device (make-texture-view-descriptor :texture resolved)))
             (present-group
               (create device
                       (make-bind-group-descriptor
                        :label "luft temporal presentation"
                        :layout (renderer-present-layout renderer)
                        :entries `((:binding 0 :resource ,resolved-view)
                                   (:binding 1
                                    :resource ,(renderer-sampler renderer)))))))
        (setf (renderer-scene-texture renderer) scene
              (renderer-scene-view renderer) scene-view
              (renderer-motion-texture renderer) motion
              (renderer-motion-view renderer) motion-view
              (renderer-resolved-texture renderer) resolved
              (renderer-resolved-view renderer) resolved-view
              (renderer-present-bind-group renderer) present-group))))
  renderer)

(defun ensure-renderer-extent (renderer extent)
  (unless (equal extent (renderer-extent renderer))
    (destroy-renderer-targets renderer)
    (create-frame-targets renderer extent))
  renderer)

(defun mesh-lattice-point-words (mesh)
  "LUFT vertex sites, mesh vertices, and eighth-step boundary-edge samples."
  (let ((points (make-hash-table :test #'equal))
        (result (make-array 64 :element-type '(unsigned-byte 32)
                              :adjustable t :fill-pointer 0))
        (templates (luft:surface-mesh-template-vertex-words mesh))
        (ranges (luft:surface-mesh-template-ranges mesh)))
    (labels ((remember (point marker-kind)
               (setf (gethash point points)
                     (max marker-kind (gethash point points 0))))
             (template-position (base vertex)
               (let ((offset (* vertex
                                luft:+mesh-template-vertex-word-count+)))
                 (loop for axis below 3
                       collect (+ (* luft:+mesh-cell-size+ (nth axis base))
                                  (- (aref templates (+ offset axis))
                                     luft:+mesh-template-coordinate-bias+)))))
             (sample-axis-edge (left right)
               (let ((different
                       (loop for axis below 3
                             unless (= (nth axis left) (nth axis right))
                               collect axis)))
                 (when (= 1 (length different))
                   (let* ((axis (first different))
                          (low (min (nth axis left) (nth axis right)))
                          (high (max (nth axis left) (nth axis right))))
                     (loop for coordinate from low to high do
                       (let ((point (copy-list left)))
                         (setf (nth axis point) coordinate)
                         (remember point 0)))))))
             (visit-stream (words fan-p)
               (loop for instance-offset from 0 below (length words) by 4
                     for base = (list (aref words instance-offset)
                                      (aref words (+ instance-offset 1))
                                      (aref words (+ instance-offset 2)))
                     for packed = (aref words (+ instance-offset 3))
                     for template-id = (ldb (byte 16 0) packed)
                     for vertex-start = (aref ranges (* 2 template-id))
                     for vertex-count = (aref ranges (1+ (* 2 template-id)))
                     do (when fan-p
                          (remember
                           (mapcar (lambda (x)
                                     (* luft:+mesh-cell-size+ x))
                                   base)
                           2))
                        (loop for vertex from vertex-start
                                below (+ vertex-start vertex-count)
                              do (remember (template-position base vertex) 1))
                        (loop for vertex from vertex-start
                                below (+ vertex-start vertex-count) by 3
                              for attributes =
                                (aref templates
                                      (+ (* vertex 4) 3))
                              for edge-mask = (ldb (byte 3 10) attributes)
                              for a = (template-position base vertex)
                              for b = (template-position base (1+ vertex))
                              for c = (template-position base (+ vertex 2))
                              when (logbitp 0 edge-mask)
                                do (sample-axis-edge b c)
                              when (logbitp 1 edge-mask)
                                do (sample-axis-edge a c)
                              when (logbitp 2 edge-mask)
                                do (sample-axis-edge a b)))))
      (visit-stream (luft:surface-mesh-face-instance-words mesh) nil)
      (visit-stream (luft:surface-mesh-band-instance-words mesh) nil)
      (visit-stream (luft:surface-mesh-fan-instance-words mesh) t))
    (maphash
     (lambda (point marker-kind)
       (dolist (coordinate point) (vector-push-extend coordinate result))
       (vector-push-extend marker-kind result))
     points)
    (coerce result '(simple-array (unsigned-byte 32) (*)))))

(defun %destroy-mesh-slot (slot)
  (dolist (resource (list (mesh-slot-face-group slot)
                          (mesh-slot-band-group slot)
                          (mesh-slot-fan-group slot)
                          (mesh-slot-lattice-point-group slot)
                          (mesh-slot-template-buffer slot)
                          (mesh-slot-face-buffer slot)
                          (mesh-slot-band-buffer slot)
                          (mesh-slot-fan-buffer slot)
                          (mesh-slot-lattice-point-buffer slot)))
    (when resource (ignore-errors (destroy resource))))
  (values))

(defun %make-renderer-mesh-slot (renderer mesh)
  "Upload MESH's streams and bind them against the renderer's layouts."
  (let* ((device (renderer-device renderer))
         (layout (renderer-layout renderer))
         (camera-buffer (renderer-camera-buffer renderer))
         (lattice-point-words (mesh-lattice-point-words mesh))
         (lattice-point-count (/ (length lattice-point-words) 4))
         (slot (%make-mesh-slot :mesh mesh
                                :lattice-point-count lattice-point-count))
         (completed-p nil))
    (flet ((stream-buffer (label words)
             (let ((buffer (create device
                                   (make-buffer-descriptor
                                    :label label
                                    :size (max 16 (* 4 (length words)))
                                    :usage '(:storage :copy-dst)))))
               (when (plusp (length words))
                 (write-buffer buffer words))
               buffer))
           (stream-group (label buffer template-buffer)
             (create device
                     (make-bind-group-descriptor
                      :label label :layout layout
                      :entries `((:binding 0 :resource ,buffer)
                                 (:binding 1 :resource ,template-buffer)
                                 (:binding 2 :resource ,camera-buffer))))))
      (unwind-protect
           (progn
             (setf (mesh-slot-template-buffer slot)
                   (stream-buffer "luft site template vertices"
                                  (luft:surface-mesh-template-vertex-words
                                   mesh))
                   (mesh-slot-face-buffer slot)
                   (stream-buffer "luft face site instances"
                                  (luft:surface-mesh-face-instance-words mesh))
                   (mesh-slot-band-buffer slot)
                   (stream-buffer "luft edge site instances"
                                  (luft:surface-mesh-band-instance-words mesh))
                   (mesh-slot-fan-buffer slot)
                   (stream-buffer "luft vertex site instances"
                                  (luft:surface-mesh-fan-instance-words mesh))
                   (mesh-slot-lattice-point-buffer slot)
                   (stream-buffer "luft unique eighth-cell lattice points"
                                  lattice-point-words))
             (setf (mesh-slot-face-group slot)
                   (stream-group "luft face sites"
                                 (mesh-slot-face-buffer slot)
                                 (mesh-slot-template-buffer slot))
                   (mesh-slot-band-group slot)
                   (stream-group "luft edge sites"
                                 (mesh-slot-band-buffer slot)
                                 (mesh-slot-template-buffer slot))
                   (mesh-slot-fan-group slot)
                   (stream-group "luft vertex sites"
                                 (mesh-slot-fan-buffer slot)
                                 (mesh-slot-template-buffer slot))
                   (mesh-slot-lattice-point-group slot)
                   (create device
                           (make-bind-group-descriptor
                            :label "luft eighth-cell lattice points"
                            :layout (renderer-lattice-point-layout renderer)
                            :entries
                            `((:binding 0
                               :resource ,(mesh-slot-lattice-point-buffer
                                           slot))
                              (:binding 1 :resource ,camera-buffer)))))
             (setf completed-p t)
             slot)
        (unless completed-p
          (%destroy-mesh-slot slot))))))

(defun %refresh-renderer-slot-order (renderer)
  (let ((keys '()))
    (loop for key being the hash-keys of (renderer-mesh-slots renderer)
          do (push key keys))
    (setf (renderer-slot-order renderer) (sort keys #'<))))

(defun renderer-set-mesh (renderer key mesh)
  "Make MESH resident under KEY, replacing any previous resident mesh."
  (let ((old (gethash key (renderer-mesh-slots renderer)))
        (slot (%make-renderer-mesh-slot renderer mesh)))
    (setf (gethash key (renderer-mesh-slots renderer)) slot)
    (%refresh-renderer-slot-order renderer)
    (when old (%destroy-mesh-slot old))
    slot))

(defun renderer-remove-mesh (renderer key)
  (let ((slot (gethash key (renderer-mesh-slots renderer))))
    (when slot
      (remhash key (renderer-mesh-slots renderer))
      (%refresh-renderer-slot-order renderer)
      (%destroy-mesh-slot slot))
    (values)))

(defun renderer-clear-meshes (renderer)
  (loop for key in (copy-list (renderer-slot-order renderer))
        do (renderer-remove-mesh renderer key)))

(defun make-renderer (device color-format extent)
  "Create the shared LUFT pipeline state; meshes arrive via RENDERER-SET-MESH."
  (let* ((temporal-p (metal-temporal-device-p device))
         (target-formats (if temporal-p
                             '(:rgba16-float :rg16-float)
                             (list color-format)))
         camera-buffer
         layout
         vertex-module fragment-module pipeline
         lattice-point-layout lattice-point-vertex-module
         lattice-point-fragment-module lattice-point-pipeline
         present-layout present-bind-group present-vertex-module
         present-fragment-module present-pipeline sampler
         renderer
         (completed-p nil))
    (unwind-protect
         (progn
           (setf camera-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft inspection camera"
                          :size 208 :usage '(:uniform :copy-dst))))
           (setf layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft mesh layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :storage-buffer)
                                     (:binding 2 :type :uniform-buffer))))
                 vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft mesh vertex"
                          :language :mathematical
                          :code (shaders:mesh-vertex-specification)))
                 fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft mesh fragment" :language :mathematical
                          :code (shaders:mesh-fragment-specification)))
                 pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft site stream pipeline" :layout layout
                          :vertex `(:module ,vertex-module)
                          :fragment `(:module ,fragment-module
                                      :targets
                                      ,(mapcar (lambda (format)
                                                 `(:format ,format))
                                               target-formats))
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less))))
           (setf lattice-point-layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft lattice point layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :uniform-buffer))))
                 lattice-point-vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft lattice point vertex"
                          :language :mathematical
                          :code (shaders:lattice-point-vertex-specification)))
                 lattice-point-fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft lattice point fragment"
                          :language :mathematical
                          :code (shaders:lattice-point-fragment-specification)))
                 lattice-point-pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft eighth-cell lattice point pipeline"
                          :layout lattice-point-layout
                          :vertex `(:module ,lattice-point-vertex-module)
                          :fragment
                          `(:module ,lattice-point-fragment-module
                            :targets
                            ,(loop for format in target-formats
                                   for first = t then nil
                                   collect `(:format ,format
                                             ,@(when first
                                                 '(:blend :premultiplied-alpha)))))
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled nil
                            :depth-compare :less))))
           (setf sampler
                 (create device
                         (make-sampler-descriptor
                          :label "luft presentation sampler"
                          :mag-filter :linear :min-filter :linear)))
           (when temporal-p
             (setf present-layout
                   (create device
                           (make-bind-group-layout-descriptor
                            :label "luft presentation layout"
                            :entries '((:binding 0 :type :texture)
                                       (:binding 1 :type :sampler))))
                   present-vertex-module
                   (create device
                           (make-shader-module-descriptor
                            :label "luft presentation vertex"
                            :language :mathematical
                            :code (shaders:present-vertex-specification)))
                   present-fragment-module
                   (create device
                           (make-shader-module-descriptor
                            :label "luft presentation fragment"
                            :language :mathematical
                            :code (shaders:present-fragment-specification)))
                   present-pipeline
                   (create device
                           (make-render-pipeline-descriptor
                            :label "luft temporal presentation pipeline"
                            :layout present-layout
                            :vertex `(:module ,present-vertex-module)
                            :fragment `(:module ,present-fragment-module
                                        :targets ((:format ,color-format)))
                            :primitive '(:topology :triangle-list)))))
           (setf renderer
                 (make-instance 'renderer
                                :device device
                                :color-format color-format
                                :temporal-p temporal-p
                                :camera-buffer camera-buffer
                                :layout layout
                                :vertex-module vertex-module
                                :fragment-module fragment-module
                                :pipeline pipeline
                                :lattice-point-layout lattice-point-layout
                                :lattice-point-vertex-module
                                lattice-point-vertex-module
                                :lattice-point-fragment-module
                                lattice-point-fragment-module
                                :lattice-point-pipeline lattice-point-pipeline))
           (setf (renderer-present-layout renderer) present-layout
                 (renderer-sampler renderer) sampler
                 (renderer-present-vertex-module renderer)
                 present-vertex-module
                 (renderer-present-fragment-module renderer)
                 present-fragment-module
                 (renderer-present-pipeline renderer) present-pipeline)
           (create-frame-targets renderer extent)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (dolist (resource (list present-pipeline present-fragment-module
                                present-vertex-module sampler present-bind-group
                                present-layout lattice-point-pipeline
                                lattice-point-fragment-module
                                lattice-point-vertex-module
                                lattice-point-layout
                                pipeline fragment-module
                                vertex-module layout camera-buffer))
          (when resource (ignore-errors (destroy resource))))))))

(defun draw-site-stream (pass bind-group draws)
  (when draws
    (set-bind-group pass 0 bind-group)
    (dolist (draw-record draws)
      (destructuring-bind
          (template-id vertex-start vertex-count instance-start instance-count)
          draw-record
        (declare (ignore template-id))
        (draw pass vertex-count instance-count vertex-start instance-start)))))

(defun encode-renderer-frame
    (renderer encoder surface-texture extent camera-uniform-data
     &key jitter view construction-p overlay-encoder)
  (ensure-renderer-extent renderer extent)
  (write-buffer (renderer-camera-buffer renderer) camera-uniform-data)
  (let* ((temporal-p (renderer-temporal-p renderer))
         (color-view (if temporal-p
                         (renderer-scene-view renderer)
                         surface-texture))
         (color-attachments
           (if temporal-p
               `((:view ,color-view :load-op :clear :store-op :store
                  :clear-value #(0.60 0.75 0.96 1.0))
                 (:view ,(renderer-motion-view renderer)
                  :load-op :clear :store-op :store
                  :clear-value #(0.0 0.0 0.0 0.0)))
               `((:view ,color-view :load-op :clear :store-op :store
                  :clear-value #(0.60 0.75 0.96 1.0)))))
         (pass
           (begin-render-pass
            encoder
            (make-render-pass-descriptor
             :label "luft site streams"
             :color-attachments color-attachments
             :depth-stencil-attachment
             `(:view ,(renderer-depth-view renderer)
               :depth-load-op :clear
               :depth-store-op ,(if temporal-p :store :discard)
               :depth-clear-value 1.0)))))
    (set-pipeline pass (renderer-pipeline renderer))
    (dolist (key (renderer-slot-order renderer))
      (let* ((slot (gethash key (renderer-mesh-slots renderer)))
             (mesh (mesh-slot-mesh slot)))
        (draw-site-stream pass (mesh-slot-face-group slot)
                          (luft:surface-mesh-face-draws mesh))
        (draw-site-stream pass (mesh-slot-band-group slot)
                          (luft:surface-mesh-band-draws mesh))
        (draw-site-stream pass (mesh-slot-fan-group slot)
                          (luft:surface-mesh-fan-draws mesh))))
    (when construction-p
      (set-pipeline pass (renderer-lattice-point-pipeline renderer))
      (dolist (key (renderer-slot-order renderer))
        (let ((slot (gethash key (renderer-mesh-slots renderer))))
          (when (plusp (mesh-slot-lattice-point-count slot))
            (set-bind-group pass 0 (mesh-slot-lattice-point-group slot))
            (draw pass 6 (mesh-slot-lattice-point-count slot))))))
    (when temporal-p
      (signal-temporal-scaler-inputs pass
                                     (renderer-temporal-scaler renderer)))
    (when (and (not temporal-p) overlay-encoder)
      (funcall overlay-encoder pass))
    (end-pass pass)
    (when temporal-p
      (let ((scaler (renderer-temporal-scaler renderer))
            (history-valid-p (renderer-history-valid-p renderer)))
        (encode-temporal-scale
         encoder scaler
         (renderer-scene-texture renderer)
         (renderer-depth-texture renderer)
         (renderer-motion-texture renderer)
         (renderer-resolved-texture renderer)
         (vector (* 0.5 (first extent) (aref jitter 0))
                 (* 0.5 (second extent) (aref jitter 1)))
         (not history-valid-p))
        (let ((present-pass
                (begin-render-pass
                 encoder
                 (make-render-pass-descriptor
                  :label "luft temporal presentation"
                  :color-attachments
                  `((:view ,surface-texture :load-op :clear :store-op :store
                     :clear-value #(0.0 0.0 0.0 1.0)))))))
          (wait-temporal-scaler-output present-pass scaler)
          (set-pipeline present-pass (renderer-present-pipeline renderer))
          (set-bind-group present-pass 0
                          (renderer-present-bind-group renderer))
          (draw present-pass 3)
          ;; MetalFX publishes into this pass.  Keeping the atelier overlay in
          ;; the same final pass makes it unambiguously later than the resolve.
          (when overlay-encoder
            (funcall overlay-encoder present-pass))
          (end-pass present-pass))
        (setf (renderer-previous-view renderer) view
              (renderer-history-valid-p renderer) t
              (renderer-history-used-p renderer) history-valid-p)
        (incf (renderer-frame-index renderer)))))
  renderer)

(defun destroy-renderer (renderer)
  (destroy-renderer-targets renderer)
  (renderer-clear-meshes renderer)
  (dolist (resource
            (list (renderer-present-pipeline renderer)
                  (renderer-present-fragment-module renderer)
                  (renderer-present-vertex-module renderer)
                  (renderer-sampler renderer)
                  (renderer-present-layout renderer)
                  (renderer-lattice-point-pipeline renderer)
                  (renderer-lattice-point-fragment-module renderer)
                  (renderer-lattice-point-vertex-module renderer)
                  (renderer-lattice-point-layout renderer)
                  (renderer-pipeline renderer) (renderer-fragment-module renderer)
                  (renderer-vertex-module renderer)
                  (renderer-layout renderer)
                  (and (slot-boundp renderer 'camera-buffer)
                       (renderer-camera-buffer renderer))))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-present-pipeline renderer) nil
        (renderer-present-fragment-module renderer) nil
        (renderer-present-vertex-module renderer) nil
        (renderer-sampler renderer) nil
        (renderer-present-layout renderer) nil
        (renderer-lattice-point-pipeline renderer) nil
        (renderer-lattice-point-fragment-module renderer) nil
        (renderer-lattice-point-vertex-module renderer) nil
        (renderer-lattice-point-layout renderer) nil
        (renderer-pipeline renderer) nil
        (renderer-fragment-module renderer) nil
        (renderer-vertex-module renderer) nil
        (renderer-layout renderer) nil
        (renderer-camera-buffer renderer) nil)
  (values))

;;; ---------------------------------------------------------------------------
;;; Streaming chunk scenes
;;;
;;; A streaming scene is an ordinary authored scene whose solid is split into
;;; its chunk chains.  Chunks become resident one at a time -- the mock of a
;;; real async store -- and each residency change remeshes the arrivals and
;;; their already-resident neighbors.  MESH-CHUNK's probes into non-resident
;;; neighbors signal MISSING-CHUNK; the scene's handler answers USE-CHUNK for
;;; resident chunks and TREAT-AS-AIR otherwise, so the loading frontier shows
;;; honest cliff walls that heal as neighbors arrive.

(defclass streaming-scene (scene)
  ((store :initform (make-hash-table :test #'eql)
          :reader streaming-scene-store)
   (pending :initform nil :accessor streaming-scene-pending)
   (loaded :initform (make-hash-table :test #'eql)
           :reader streaming-scene-loaded)
   (frames-per-load :initarg :frames-per-load :initform 15
                    :accessor streaming-scene-frames-per-load)
   (frame-counter :initform 0 :accessor streaming-scene-frame-counter)))

(defun make-streaming-scene (scene &key (frames-per-load 15))
  "Wrap SCENE for chunk-at-a-time residency, loading nearest chunks first."
  (let ((streaming (make-instance
                    'streaming-scene
                    :solid (scene-solid scene)
                    :architecture-cells (scene-architecture-cells scene)
                    :frames-per-load frames-per-load))
        (keys '()))
    (luft:map-chain-chunks
     (lambda (key chain)
       (setf (gethash key (streaming-scene-store streaming)) chain)
       (push key keys))
     (scene-solid scene))
    (let* ((domain (luft:chain-domain (scene-solid scene)))
           (centre-x (/ (luft:world-domain-x-limit domain) 2))
           (centre-y (/ (luft:world-domain-y-limit domain) 2)))
      (setf (streaming-scene-pending streaming)
            (sort keys #'<
                  :key (lambda (key)
                         (let ((dx (- (+ (luft:chunk-origin-x key)
                                         (/ luft:+chunk-size+ 2))
                                      centre-x))
                               (dy (- (+ (luft:chunk-origin-y key)
                                         (/ luft:+chunk-size+ 2))
                                      centre-y)))
                           (+ (* dx dx) (* dy dy)))))))
    streaming))

(defun mesh-streaming-chunk (scene key bevel-width)
  "Mesh one resident chunk against the scene's current residency."
  (let ((store (streaming-scene-store scene))
        (loaded (streaming-scene-loaded scene)))
    (handler-bind
        ((luft:missing-chunk
           (lambda (condition)
             (let ((neighbor-key (luft:missing-chunk-key condition)))
               (if (gethash neighbor-key loaded)
                   (invoke-restart 'luft:use-chunk
                                   (gethash neighbor-key store))
                   (invoke-restart 'luft:treat-as-air)))))
         (luft:outside-domain
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'luft:treat-as-air))))
      (let ((chain (gethash key store)))
        (zone (:luft/rematerialize :value (luft:chain-count chain))
          (luft:mesh-chunk chain key
                           :stock-function
                           (lambda (face) (scene-face-stock scene face))
                           :chamfer-stock-function #'scene-chamfer-stock
                           :bevel-width bevel-width))))))

(defun advance-streaming-scene (scene renderer bevel-width)
  "Make the next pending chunk resident; heal resident neighbor seams.

Returns the newly resident chunk key, or NIL when everything is resident."
  (let ((key (pop (streaming-scene-pending scene))))
    (when key
      (setf (gethash key (streaming-scene-loaded scene)) t)
      (renderer-set-mesh renderer key
                         (mesh-streaming-chunk scene key bevel-width))
      (let ((grid-x (luft:chunk-key-x key))
            (grid-y (luft:chunk-key-y key)))
        (loop for dx from -1 to 1 do
          (loop for dy from -1 to 1 do
            (unless (and (zerop dx) (zerop dy))
              (let ((nx (+ grid-x dx)) (ny (+ grid-y dy)))
                (when (and (<= 0 nx) (<= 0 ny))
                  (let ((neighbor (luft:chunk-key-at
                                   (ash nx luft:+chunk-bits+)
                                   (ash ny luft:+chunk-bits+))))
                    (when (gethash neighbor (streaming-scene-loaded scene))
                      (renderer-set-mesh
                       renderer neighbor
                       (mesh-streaming-chunk scene neighbor
                                             bevel-width))))))))))
      key)))

(defun make-highland-sanctuary-scene (&key (horizontal-bits 8))
  "Rolling highlands spanning many chunks, studded with watchtowers."
  (let* ((builder (make-scene-builder :horizontal-bits horizontal-bits))
         (size (ash 1 horizontal-bits)))
    (flet ((terrain-height (x y)
             (max 1 (floor (+ 9.0
                              (* 5.0 (sin (* x 0.043)) (cos (* y 0.037)))
                              (* 3.0 (sin (+ (* x 0.11) (* y 0.073))))
                              (* 1.5 (cos (+ (* x 0.021) (* y 0.19)))))))))
      (dotimes (x size)
        (dotimes (y size)
          (let ((height (terrain-height x y)))
            (dotimes (z height)
              (scene-builder-cell builder x y z)))))
      ;; A watchtower near the middle of every chunk.
      (loop for tower-x from 32 below size by luft:+chunk-size+ do
        (loop for tower-y from 32 below size by luft:+chunk-size+ do
          (let ((base (terrain-height tower-x tower-y)))
            (scene-builder-disc builder tower-x tower-y 4
                                (1- base) base :architecture-p t)
            (scene-builder-ring builder tower-x tower-y 2 3
                                (1+ base) (+ base 7) :architecture-p t)
            (scene-builder-ring builder tower-x tower-y 2 4
                                (+ base 8) (+ base 9) :architecture-p t)
            (scene-builder-disc builder tower-x tower-y 3
                                (+ base 9) (+ base 9)
                                :architecture-p t)))))
    (finish-scene-builder builder)))
