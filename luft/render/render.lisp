(in-package #:luft.render)

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

(defun make-face-materialization (solid &key (stock-function #'default-face-stock))
  "Lower SOLID's boundary to positive then negative dense face records."
  (check-type solid luft:chain)
  (let* ((domain (luft:chain-domain solid))
         (surface (luft:surface-chain solid))
         (sites (luft:chain-sites surface))
         (positive-count
           (loop for face across sites count (luft:site-positive-p face)))
         (negative-count (- (length sites) positive-count))
         (words (luft:make-face-record-array (length sites)))
         (occupancy (lambda (x y z)
                      (luft:chain-cell-occupancy-bit solid x y z)))
         (write 0))
    (flet ((publish-polarity (positive-p)
             (loop for face across sites
                   when (eq positive-p (luft:site-positive-p face))
                     do (luft:store-face-record
                         words write domain face
                         (luft:face-shape-word domain face occupancy)
                         (funcall stock-function face))
                        (incf write))))
      (publish-polarity t)
      (publish-polarity nil))
    (%make-face-materialization domain words positive-count negative-count)))

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
   (depth-texture :initform nil :accessor renderer-depth-texture)
   (depth-view :initform nil :accessor renderer-depth-view)
   (extent :initform nil :accessor renderer-extent)))

(defun create-depth-target (renderer extent)
  (let* ((device (renderer-device renderer))
         (texture
           (create device
                   (make-texture-descriptor
                    :label "luft depth" :size extent :dimensions :2d
                    :format :depth32-float :usage '(:render-attachment))))
         (view (create device (make-texture-view-descriptor :texture texture))))
    (setf (renderer-depth-texture renderer) texture
          (renderer-depth-view renderer) view
          (renderer-extent renderer) (copy-list extent)))
  renderer)

(defun ensure-renderer-extent (renderer extent)
  (unless (equal extent (renderer-extent renderer))
    (when (renderer-depth-view renderer) (destroy (renderer-depth-view renderer)))
    (when (renderer-depth-texture renderer)
      (destroy (renderer-depth-texture renderer)))
    (setf (renderer-depth-view renderer) nil
          (renderer-depth-texture renderer) nil)
    (create-depth-target renderer extent))
  renderer)

(defun make-renderer (device materialization color-format extent)
  (let (face-buffer camera-buffer positive-index-buffer negative-index-buffer layout bind-group
        vertex-module fragment-module pipeline renderer (completed-p nil))
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
                          :size 80 :usage '(:uniform :copy-dst)))
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
                                      :targets ((:format ,color-format)))
                          :primitive '(:topology :triangle-list)
                          :depth-stencil
                          '(:format :depth32-float :depth-write-enabled t
                            :depth-compare :less))))
           (setf renderer
                 (make-instance 'renderer
                                :device device :materialization materialization
                                :face-buffer face-buffer
                                :camera-buffer camera-buffer
                                :positive-index-buffer positive-index-buffer
                                :negative-index-buffer negative-index-buffer
                                :layout layout :bind-group bind-group
                                :vertex-module vertex-module
                                :fragment-module fragment-module
                                :pipeline pipeline))
           (create-depth-target renderer extent)
           (setf completed-p t)
           renderer)
      (unless completed-p
        (when renderer (destroy-renderer renderer))
        (dolist (resource (list pipeline fragment-module vertex-module bind-group
                                layout negative-index-buffer
                                positive-index-buffer camera-buffer face-buffer))
          (when resource (ignore-errors (destroy resource))))))))

(defun encode-renderer-frame
    (renderer encoder surface-texture extent camera-uniform-data)
  (ensure-renderer-extent renderer extent)
  (write-buffer (renderer-camera-buffer renderer) camera-uniform-data)
  (let* ((materialization (renderer-materialization renderer))
         (positive-count (face-materialization-positive-count materialization))
         (negative-count (face-materialization-negative-count materialization))
         (pass
           (begin-render-pass
            encoder
            (make-render-pass-descriptor
             :label "luft indexed faces"
             :color-attachments
             `((:view ,surface-texture :load-op :clear :store-op :store
                :clear-value #(0.055 0.065 0.085 1.0)))
             :depth-stencil-attachment
             `(:view ,(renderer-depth-view renderer)
               :depth-load-op :clear :depth-store-op :discard
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
    (end-pass pass))
  renderer)

(defun destroy-renderer (renderer)
  (dolist (resource
            (list (renderer-depth-view renderer) (renderer-depth-texture renderer)
                  (renderer-pipeline renderer) (renderer-fragment-module renderer)
                  (renderer-vertex-module renderer) (renderer-bind-group renderer)
                  (renderer-layout renderer)
                  (renderer-negative-index-buffer renderer)
                  (renderer-positive-index-buffer renderer)
                  (and (slot-boundp renderer 'camera-buffer)
                       (renderer-camera-buffer renderer))
                  (renderer-face-buffer renderer)))
    (when resource (ignore-errors (destroy resource))))
  (setf (renderer-depth-view renderer) nil
        (renderer-depth-texture renderer) nil
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
