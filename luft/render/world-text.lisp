(in-package #:luft.render)

;;; World text: flat panels and Slug glyphs standing on world surfaces.
;;;
;;; The component owns two programs.  What they draw arrives as batches,
;;; each one surface's panel records, glyph records, and the glyph atlas
;;; those records address.  Whoever owns a surface (a terminal wall, a sign)
;;; builds its batch and publishes the current list; the renderer only draws
;;; it, inside the scene pass after the opaque world, so walls hide text
;;; behind them and text writes motion like any other static geometry.

(defclass world-text-drawing (gpu-resource-owner)
  ((glyph-program :accessor world-text-glyph-program)
   (panel-program :accessor world-text-panel-program)))

(defun make-world-text-drawing (device target-formats sample-count)
  "Build the glyph and panel programs as one transactional GPU owner."
  (let ((drawing (make-instance 'world-text-drawing))
        (targets (loop for format in target-formats
                       for first = t then nil
                       collect `(:format ,format
                                 ,@(when first '(:blend :premultiplied-alpha))))))
    (with-gpu-construction (drawing)
      (flet ((program (label vertex fragment)
               (own-gpu-object
                drawing
                (make-drawing-program
                 device :label label :vertex vertex :fragment fragment
                 :targets targets :sample-count sample-count
                 ;; Coplanar layers sit a few thousandths of a cell apart in
                 ;; front of their wall; none of them occludes the world.
                 :depth-stencil '(:format :depth32-float
                                  :depth-write-enabled nil
                                  :depth-compare :less)))))
        (setf (world-text-panel-program drawing)
              (program "luft world panels"
                       (shaders:world-panel-vertex-specification)
                       (shaders:world-panel-fragment-specification))
              (world-text-glyph-program drawing)
              (program "luft world Slug glyphs"
                       (shaders:world-glyph-vertex-specification)
                       (shaders:world-glyph-fragment-specification)))))))

(defmethod destroy ((drawing world-text-drawing))
  (release-owned-gpu-resources drawing))

(defstruct (world-text-batch (:constructor %make-world-text-batch))
  "One surface's drawable text: panels behind, glyphs over them."
  (panel-buffer nil)
  (panel-count 0 :type (integer 0))
  (glyph-buffer nil)
  (glyph-count 0 :type (integer 0))
  (atlas nil))

(defun world-text-record-buffer (device label records)
  "Upload RECORDS, a single-float vector, as a fresh storage buffer."
  (let ((buffer (create device
                        (make-buffer-descriptor
                         :label label
                         :size (max 16 (* 4 (length records)))
                         :usage '(:storage :copy-dst)))))
    (when (plusp (length records))
      (write-buffer buffer records))
    buffer))

(defun make-world-text-batch (device panel-records glyph-records atlas)
  "Upload one surface's 16-float panel and 24-float glyph records."
  (%make-world-text-batch
   :panel-buffer (world-text-record-buffer
                  device "luft world panel records" panel-records)
   :panel-count (floor (length panel-records) 16)
   :glyph-buffer (world-text-record-buffer
                  device "luft world glyph records" glyph-records)
   :glyph-count (floor (length glyph-records) 24)
   :atlas atlas))

(defun release-world-text-batch (batch)
  "Release BATCH's buffers; destruction waits for submitted frames."
  (when (world-text-batch-panel-buffer batch)
    (destroy (world-text-batch-panel-buffer batch))
    (setf (world-text-batch-panel-buffer batch) nil))
  (when (world-text-batch-glyph-buffer batch)
    (destroy (world-text-batch-glyph-buffer batch))
    (setf (world-text-batch-glyph-buffer batch) nil))
  (values))

(defun publish-renderer-world-text (renderer batches)
  "Make BATCHES the surfaces RENDERER draws from its next frame on.

Every slot's cached bindings name the previous buffers, so a changed list
invalidates them the way a residency publication does."
  (unless (and (= (length batches)
                  (length (renderer-world-text-batches renderer)))
               (every #'eq batches (renderer-world-text-batches renderer)))
    (setf (renderer-world-text-batches renderer) (copy-list batches))
    (clear-renderer-frame-bind-groups renderer))
  renderer)

(defun encode-renderer-world-text (renderer frame pass)
  "Draw every published batch into the open scene PASS."
  (let ((drawing (renderer-world-text renderer))
        (camera (renderer-frame-state-camera-buffer frame)))
    (when drawing
      (dolist (batch (renderer-world-text-batches renderer))
        (when (and (plusp (world-text-batch-panel-count batch))
                   (world-text-batch-panel-buffer batch))
          (encode-program
           (world-text-panel-program drawing) pass
           (renderer-frame-program-binding
            renderer frame
            (list :world-panels (world-text-batch-panel-buffer batch))
            (world-text-panel-program drawing)
            :panel-records (world-text-batch-panel-buffer batch)
            :camera-state camera)
           (make-gpu-draw-command
            :vertex-count 6
            :instance-count (world-text-batch-panel-count batch))))
        (when (and (plusp (world-text-batch-glyph-count batch))
                   (world-text-batch-glyph-buffer batch)
                   (world-text-batch-atlas batch))
          (let ((atlas (world-text-batch-atlas batch)))
            (encode-program
             (world-text-glyph-program drawing) pass
             (renderer-frame-program-binding
              renderer frame
              (list :world-glyphs (world-text-batch-glyph-buffer batch) atlas)
              (world-text-glyph-program drawing)
              :glyph-records (world-text-batch-glyph-buffer batch)
              :camera-state camera
              :band-data (luv.slug:slug-glyph-atlas-band-view atlas)
              :curve-data (luv.slug:slug-glyph-atlas-curve-view atlas))
             (make-gpu-draw-command
              :vertex-count 6
              :instance-count (world-text-batch-glyph-count batch)))))))))
