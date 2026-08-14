;;; The luvcraft application object and its direct interactions.
;;;
;;; A CUBE-WORLD-DEMO owns the canvas, GPU device, world, camera, player, and
;;; every derived GPU artifact.  This file defines that object plus the
;;; player-facing verbs: aiming along the view ray, selecting materials, and
;;; placing or removing blocks.  Chunk streaming lives in streaming.lisp and
;;; frame rendering in render.lisp.

(in-package #:luv)

(defclass cube-world-demo (canvas-event-handler)
  ((canvas :initarg :canvas :reader cube-world-demo-canvas)
   (device :initarg :device :reader cube-world-demo-device)
   (context :initarg :context :reader cube-world-demo-context)
   (world :initarg :world :reader cube-world-demo-world)
   (mesher :initarg :mesher :reader cube-world-demo-mesher)
   (production-system :initarg :production-system :initform nil
                      :reader cube-world-demo-production-system)
   (desired-chunks :initform (make-hash-table :test #'equal)
                   :reader cube-world-demo-desired-chunks)
   (next-residency-demand :initform 0
                          :accessor cube-world-demo-next-residency-demand)
   (outstanding-production :initform (make-hash-table :test #'equal)
                           :reader cube-world-demo-outstanding-production)
   (production-errors :initform nil
                      :accessor cube-world-demo-production-errors)
   (publication-limit :initarg :publication-limit :initform 2
                      :reader cube-world-demo-publication-limit)
   (load-schedule-limit :initarg :load-schedule-limit :initform 4
                        :reader cube-world-demo-load-schedule-limit)
   (mesh-capture-limit :initarg :mesh-capture-limit :initform 1
                       :reader cube-world-demo-mesh-capture-limit)
   (chunk-products :initform (make-hash-table :test #'equal)
                   :reader cube-world-demo-chunk-products)
   (meshed-world-revision
    :initarg :meshed-world-revision
    :initform -1
    :accessor cube-world-demo-meshed-world-revision)
   (camera :initarg :camera :reader cube-world-demo-camera)
   (player :initarg :player :initform nil :reader cube-world-demo-player)
   (residency-radius :initarg :residency-radius :initform 4
                     :reader cube-world-demo-residency-radius)
   (residency-center :initform nil
                     :accessor cube-world-demo-residency-center)
   (selected-block :initarg :selected-block :initform *stone-block*
                   :accessor cube-world-demo-selected-block)
   (title-base :initarg :title-base :initform "luvcraft"
               :reader cube-world-demo-title-base)
   (atlas-texture :initarg :atlas-texture
                  :reader cube-world-demo-atlas-texture)
   (atlas-view :initarg :atlas-view :reader cube-world-demo-atlas-view)
   (atlas-sampler :initarg :atlas-sampler
                  :reader cube-world-demo-atlas-sampler)
   (color-texture :initarg :color-texture
                  :reader cube-world-demo-color-texture)
   (color-view :initarg :color-view :reader cube-world-demo-color-view)
   (depth-texture :initarg :depth-texture
                  :reader cube-world-demo-depth-texture)
   (depth-view :initarg :depth-view :reader cube-world-demo-depth-view)
   (layout :initarg :layout :reader cube-world-demo-layout)
   (block-pipeline :initarg :block-pipeline
                   :reader cube-world-demo-block-pipeline)
   (crosshair-vertex-buffer
    :initarg :crosshair-vertex-buffer
    :reader cube-world-demo-crosshair-vertex-buffer)
   (crosshair-pipeline :initarg :crosshair-pipeline
                       :reader cube-world-demo-crosshair-pipeline)
   (frame-states :initform (make-hash-table :test #'eq)
                 :reader cube-world-demo-frame-states)
   (resources :initarg :resources :initform nil
              :accessor cube-world-demo-resources)
   (pressed-keys :initform (make-hash-table :test #'eq)
                 :reader cube-world-demo-pressed-keys)
   (pointer-captured-p :initform nil
                       :accessor cube-world-demo-pointer-captured-p)
   (last-frame-time :initform nil :accessor cube-world-demo-last-frame-time)
   (physics-accumulator :initform 0d0
                        :accessor cube-world-demo-physics-accumulator)
   (jump-requested-p :initform nil
                     :accessor cube-world-demo-jump-requested-p)
   (running-p :initform t :accessor cube-world-demo-running-p)))

(defun cube-world-demo-pipeline (demo)
  (live-shader-pipeline-native-pipeline
   (cube-world-demo-block-pipeline demo)))

(defun cube-world-demo-crosshair-native-pipeline (demo)
  (live-shader-pipeline-native-pipeline
   (cube-world-demo-crosshair-pipeline demo)))

(defun refresh-cube-world-shaders (demo)
  "Install any successfully redefined block-world shader methods."
  (refresh-live-shader-pipeline (cube-world-demo-block-pipeline demo))
  (refresh-live-shader-pipeline (cube-world-demo-crosshair-pipeline demo))
  demo)

(defun cube-world-demo-target (demo &key (max-distance 8d0))
  "Raycast from DEMO's camera through resident block terrain."
  (let ((camera (cube-world-demo-camera demo)))
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore right up))
      (raycast-block-world
       (cube-world-demo-world demo)
       (vector (camera-x camera) (camera-y camera) (camera-z camera))
       forward #'block-solid-p :max-distance max-distance))))

(defun update-cube-world-demo-title (demo)
  (let* ((block (cube-world-demo-selected-block demo))
         (number (position block *placeable-block-kinds* :test #'eq)))
    (when (slot-boundp demo 'canvas)
      (setf (canvas-title (cube-world-demo-canvas demo))
            (format nil "~A — [~A] ~(~A~)  ·  1–7 select  ·  shift sprint"
                    (cube-world-demo-title-base demo)
                    (if number (1+ number) "?")
                    (block-kind-name block)))))
  demo)

(defun select-cube-world-block (demo number)
  "Select the one-based numbered placeable material and update the title."
  (check-type number (integer 1))
  (let ((block (nth (1- number) *placeable-block-kinds*)))
    (when block
      (setf (cube-world-demo-selected-block demo) block)
      (update-cube-world-demo-title demo))
    block))

(defun pick-cube-world-block (demo)
  "Select the material currently under the centre crosshair."
  (multiple-value-bind (hit status) (cube-world-demo-target demo)
    (when hit
      (setf (cube-world-demo-selected-block demo)
            (block-ray-hit-block hit))
      (update-cube-world-demo-title demo))
    (values (and hit (block-ray-hit-block hit)) status)))

(defun edit-cube-world-block (demo action)
  "Apply ACTION (:REMOVE or :PLACE) along DEMO's centre view ray."
  (multiple-value-bind (hit status) (cube-world-demo-target demo)
    (unless hit
      (return-from edit-cube-world-block (values nil status)))
    (let* ((world (cube-world-demo-world demo))
           (coordinate
             (ecase action
               (:remove (block-ray-hit-coordinate hit))
               (:place (block-ray-hit-adjacent-coordinate hit)))))
      (unless coordinate
        (return-from edit-cube-world-block (values nil :blocked)))
      (let ((x (world-coordinate-x coordinate))
            (y (world-coordinate-y coordinate))
            (z (world-coordinate-z coordinate)))
        (multiple-value-bind (old-block residency) (describe-block-allocatingly world x y z)
          (unless (eq residency :resident)
            (return-from edit-cube-world-block (values nil :absent)))
          (ecase action
            (:remove
             (edit-block-at nil world x y z))
            (:place
             (when old-block
               (return-from edit-cube-world-block (values nil :blocked)))
             (when (player-overlaps-block-p
                    (cube-world-demo-player demo) x y z)
               (return-from edit-cube-world-block (values nil :blocked)))
             (edit-block-at
              (cube-world-demo-selected-block demo) world x y z)))
          (values coordinate :edited))))))
