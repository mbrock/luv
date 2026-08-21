(in-package #:luft.render)

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

(defstruct (face-materialization
             (:constructor %make-face-materialization
                 (domain words positive-count negative-count))
             (:copier nil))
  (domain nil :type luft:world-domain :read-only t)
  (words #() :type (simple-array (unsigned-byte 32) (*)) :read-only t)
  (positive-count 0 :type (integer 0 *) :read-only t)
  (negative-count 0 :type (integer 0 *) :read-only t))

(defun default-face-stock (face)
  (mod (+ (luft:site-x face) (* 2 (luft:site-y face))
          (* 3 (luft:site-z face)) (luft:site-extent face))
       4))

(defun make-face-materialization-from-surface
    (surface occupancy &key (stock-function #'default-face-stock))
  "Lower an oriented SURFACE through OCCUPANCY to dense face records.

SURFACE owns topology while OCCUPANCY supplies the stable cell window needed
to classify its edge and corner stars.  Keeping this boundary explicit lets a
game use dense resident occupancy without changing LUFT's immutable chains."
  (check-type surface luft:chain)
  (check-type occupancy function)
  (check-type stock-function function)
  (let* ((domain (luft:chain-domain surface))
         (sites (luft:chain-sites surface))
         (face-count (length sites))
         (positive-count
           (zone (:luft/count-polarities :value face-count)
             (loop for face across sites count (luft:site-positive-p face))))
         (negative-count (- face-count positive-count))
         (words
           (zone (:luft/allocate-face-records :value face-count)
             (luft:make-face-record-array face-count)))
         (write 0))
    (zone (:luft/classify-and-pack :value face-count)
      (flet ((publish-polarity (positive-p)
               (loop for face across sites
                     when (eq positive-p (luft:site-positive-p face))
                       do (luft:store-face-record
                           words write domain face
                           (luft:face-shape-word domain face occupancy)
                           (funcall stock-function face))
                          (incf write))))
        (publish-polarity t)
        (publish-polarity nil)))
    (%make-face-materialization domain words positive-count negative-count)))

(defun make-face-materialization (source &key stock-function)
  "Lower SOLID's boundary to positive then negative dense face records."
  (let* ((scene (and (typep source 'scene) source))
         (solid (if scene (scene-solid source) source))
         (stock-function (or stock-function
                             (and scene
                                  (lambda (face) (scene-face-stock scene face)))
                             #'default-face-stock)))
    (check-type solid luft:chain)
    (zone (:luft/rematerialize :value (luft:chain-count solid))
      (let ((surface
              (zone (:luft/surface :value (luft:chain-count solid))
                (luft:surface-chain solid)))
            (occupancy
              (lambda (x y z)
                (luft:chain-cell-occupancy-bit solid x y z))))
        (make-face-materialization-from-surface
         surface occupancy :stock-function stock-function)))))

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

(defclass renderer ()
  ((device :initarg :device :reader renderer-device)
   (materialization :initarg :materialization :reader renderer-materialization)
   (face-buffer :initarg :face-buffer :accessor renderer-face-buffer)
   (camera-buffer :initarg :camera-buffer :accessor renderer-camera-buffer)
   (positive-index-buffer :initarg :positive-index-buffer
                          :accessor renderer-positive-index-buffer)
   (negative-index-buffer :initarg :negative-index-buffer
                          :accessor renderer-negative-index-buffer)
   (layout :initarg :layout :accessor renderer-layout)
   (bind-group :initarg :bind-group :accessor renderer-bind-group)
   (vertex-module :initarg :vertex-module :accessor renderer-vertex-module)
   (fragment-module :initarg :fragment-module :accessor renderer-fragment-module)
   (pipeline :initarg :pipeline :accessor renderer-pipeline)
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

(defun make-renderer (device materialization color-format extent)
  (let* ((temporal-p (metal-temporal-device-p device))
         (target-formats (if temporal-p
                             '(:rgba16-float :rg16-float)
                             (list color-format)))
         face-buffer camera-buffer positive-index-buffer negative-index-buffer
         layout bind-group vertex-module fragment-module pipeline
         present-layout present-bind-group present-vertex-module
         present-fragment-module present-pipeline sampler renderer
         (completed-p nil))
    (unwind-protect
         (progn
           (setf face-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft face records"
                          :size (max luft:+face-record-byte-size+
                                     (* luft:+face-record-byte-size+
                                        (+ (face-materialization-positive-count
                                            materialization)
                                           (face-materialization-negative-count
                                            materialization))))
                          :usage '(:storage :copy-dst)))
                 positive-index-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft positive winding"
                          :size (* 2 luft:+face-index-count+)
                          :usage '(:index :copy-dst)))
                 camera-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft inspection camera"
                          :size 192 :usage '(:uniform :copy-dst)))
                 negative-index-buffer
                 (create device
                         (make-buffer-descriptor
                          :label "luft negative winding"
                          :size (* 2 luft:+face-index-count+)
                          :usage '(:index :copy-dst))))
           (write-buffer face-buffer (face-materialization-words materialization))
           (write-buffer positive-index-buffer (luft:positive-face-index-template))
           (write-buffer negative-index-buffer (luft:negative-face-index-template))
           (setf layout
                 (create device
                         (make-bind-group-layout-descriptor
                          :label "luft face layout"
                          :entries '((:binding 0 :type :storage-buffer)
                                     (:binding 1 :type :uniform-buffer))))
                 bind-group
                 (create device
                         (make-bind-group-descriptor
                          :label "luft face records" :layout layout
                          :entries `((:binding 0 :resource ,face-buffer)
                                     (:binding 1 :resource ,camera-buffer))))
                 vertex-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft face realization vertex"
                          :language :mathematical
                          :code (shaders:face-vertex-specification)))
                 fragment-module
                 (create device
                         (make-shader-module-descriptor
                          :label "luft face fragment" :language :mathematical
                          :code (shaders:face-fragment-specification)))
                 pipeline
                 (create device
                         (make-render-pipeline-descriptor
                          :label "luft indexed face pipeline" :layout layout
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
           (when temporal-p
             (setf present-layout
                   (create device
                           (make-bind-group-layout-descriptor
                            :label "luft presentation layout"
                            :entries '((:binding 0 :type :texture)
                                       (:binding 1 :type :sampler))))
                   sampler
                   (create device
                           (make-sampler-descriptor
                            :label "luft temporal sampler"
                            :mag-filter :linear :min-filter :linear))
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
                                :device device :materialization materialization
                                :color-format color-format
                                :temporal-p temporal-p
                                :face-buffer face-buffer
                                :camera-buffer camera-buffer
                                :positive-index-buffer positive-index-buffer
                                :negative-index-buffer negative-index-buffer
                                :layout layout :bind-group bind-group
                                :vertex-module vertex-module
                                :fragment-module fragment-module
                                :pipeline pipeline))
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
                                present-layout pipeline fragment-module
                                vertex-module bind-group layout negative-index-buffer
                                positive-index-buffer camera-buffer face-buffer))
          (when resource (ignore-errors (destroy resource))))))))

(defun encode-renderer-frame
    (renderer encoder surface-texture extent camera-uniform-data
     &key jitter view)
  (ensure-renderer-extent renderer extent)
  (write-buffer (renderer-camera-buffer renderer) camera-uniform-data)
  (let* ((materialization (renderer-materialization renderer))
         (positive-count (face-materialization-positive-count materialization))
         (negative-count (face-materialization-negative-count materialization))
         (temporal-p (renderer-temporal-p renderer))
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
             :label "luft indexed faces"
             :color-attachments color-attachments
             :depth-stencil-attachment
             `(:view ,(renderer-depth-view renderer)
               :depth-load-op :clear
               :depth-store-op ,(if temporal-p :store :discard)
               :depth-clear-value 1.0)))))
    (set-pipeline pass (renderer-pipeline renderer))
    (set-bind-group pass 0 (renderer-bind-group renderer))
    (when (plusp positive-count)
      (draw-indexed pass (renderer-positive-index-buffer renderer)
                    :uint16 luft:+face-index-count+ positive-count))
    (when (plusp negative-count)
      (draw-indexed pass (renderer-negative-index-buffer renderer)
                    :uint16 luft:+face-index-count+ negative-count 0 0
                    positive-count))
    (when temporal-p
      (signal-temporal-scaler-inputs pass
                                     (renderer-temporal-scaler renderer)))
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
          (end-pass present-pass))
        (setf (renderer-previous-view renderer) view
              (renderer-history-valid-p renderer) t
              (renderer-history-used-p renderer) history-valid-p)
        (incf (renderer-frame-index renderer)))))
  renderer)

(defun destroy-renderer (renderer)
  (destroy-renderer-targets renderer)
  (dolist (resource
            (list (renderer-present-pipeline renderer)
                  (renderer-present-fragment-module renderer)
                  (renderer-present-vertex-module renderer)
                  (renderer-sampler renderer)
                  (renderer-present-layout renderer)
                  (renderer-pipeline renderer) (renderer-fragment-module renderer)
                  (renderer-vertex-module renderer) (renderer-bind-group renderer)
                  (renderer-layout renderer)
                  (renderer-negative-index-buffer renderer)
                  (renderer-positive-index-buffer renderer)
                  (and (slot-boundp renderer 'camera-buffer)
                       (renderer-camera-buffer renderer))
                  (renderer-face-buffer renderer)))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-present-pipeline renderer) nil
        (renderer-present-fragment-module renderer) nil
        (renderer-present-vertex-module renderer) nil
        (renderer-sampler renderer) nil
        (renderer-present-layout renderer) nil
        (renderer-pipeline renderer) nil
        (renderer-fragment-module renderer) nil
        (renderer-vertex-module renderer) nil
        (renderer-bind-group renderer) nil
        (renderer-layout renderer) nil
        (renderer-negative-index-buffer renderer) nil
        (renderer-positive-index-buffer renderer) nil
        (renderer-camera-buffer renderer) nil
        (renderer-face-buffer renderer) nil)
  (values))
