;;; The luvcraft application object and its direct interactions.
;;;
;;; A LUVCRAFT-SESSION owns the canvas, GPU device, world, camera, player, and
;;; every derived GPU artifact.  This file defines that object plus the
;;; player-facing verbs: aiming along the view ray, selecting materials, and
;;; placing or removing blocks.  Chunk streaming lives in streaming.lisp and
;;; frame rendering in render.lisp.

(in-package #:luvcraft)

(luv.arithmetic:define-quantity-constant
    +luvcraft-target-reach+ 8d0
  :type double-float
  :quantity (:quantity :ray-distance :unit :cell))

(defclass luvcraft-session (canvas-event-handler)
  ((canvas :initarg :canvas :reader luvcraft-session-canvas)
   (device :initarg :device :reader luvcraft-session-device)
   (context :initarg :context :reader luvcraft-session-context)
   (world :initarg :world :reader luvcraft-session-world)
   (checkpoint-writer :initarg :checkpoint-writer :initform nil
                      :reader luvcraft-session-checkpoint-writer)
   (mesher :initarg :mesher :reader luvcraft-session-mesher)
   (production-system :initarg :production-system :initform nil
                      :reader luvcraft-session-production-system)
   (desired-chunks :initform (make-hash-table :test #'equal)
                   :reader luvcraft-session-desired-chunks)
   (next-residency-demand :initform 0
                          :accessor luvcraft-session-next-residency-demand)
   (outstanding-production :initform (make-hash-table :test #'equal)
                           :reader luvcraft-session-outstanding-production)
   (production-errors :initform nil
                      :accessor luvcraft-session-production-errors)
   (publication-limit :initarg :publication-limit :initform 2
                      :reader luvcraft-session-publication-limit)
   (load-schedule-limit :initarg :load-schedule-limit :initform 4
                        :reader luvcraft-session-load-schedule-limit)
   (mesh-capture-limit :initarg :mesh-capture-limit :initform 1
                       :reader luvcraft-session-mesh-capture-limit)
   (chunk-products :initform (make-hash-table :test #'equal)
                   :reader luvcraft-session-chunk-products)
   (staged-chunk-products :initform (make-hash-table :test #'equal)
                          :reader luvcraft-session-staged-chunk-products)
   (meshed-world-revision
    :initarg :meshed-world-revision
    :initform -1
    :accessor luvcraft-session-meshed-world-revision)
   (camera :initarg :camera :reader luvcraft-session-camera)
   (lighting-state :initarg :lighting-state :initform nil
                   :reader luvcraft-session-lighting-state)
   (sky-clock :initarg :sky-clock :initform (make-instance 'sky-clock)
              :accessor luvcraft-session-sky-clock)
   (sky-profile :initarg :sky-profile :initform (make-default-sky-profile)
                :accessor luvcraft-session-sky-profile)
   (shadow-diagnostic-p :initarg :shadow-diagnostic-p :initform nil
                        :reader luvcraft-session-shadow-diagnostic-p)
   (player :initarg :player :initform nil :reader luvcraft-session-player)
   (residency-radius :initarg :residency-radius :initform 4
                     :reader luvcraft-session-residency-radius)
   (residency-center :initform nil
                     :accessor luvcraft-session-residency-center)
   (selected-block :initarg :selected-block :initform *stone-block*
                   :accessor luvcraft-session-selected-block)
   (title-base :initarg :title-base :initform "luvcraft"
               :reader luvcraft-session-title-base)
   (atlas-texture :initarg :atlas-texture
                  :reader luvcraft-session-atlas-texture)
   (atlas-view :initarg :atlas-view :reader luvcraft-session-atlas-view)
   (atlas-sampler :initarg :atlas-sampler
                  :reader luvcraft-session-atlas-sampler)
   (color-texture :initarg :color-texture
                  :reader luvcraft-session-color-texture)
   (color-view :initarg :color-view :reader luvcraft-session-color-view)
   (depth-texture :initarg :depth-texture
                  :reader luvcraft-session-depth-texture)
   (depth-view :initarg :depth-view :reader luvcraft-session-depth-view)
   (shadow-depth-texture :initarg :shadow-depth-texture
                         :reader luvcraft-session-shadow-depth-texture)
   (shadow-depth-view :initarg :shadow-depth-view
                      :reader luvcraft-session-shadow-depth-view)
   (shadow-depth-sampler :initarg :shadow-depth-sampler
                         :reader luvcraft-session-shadow-depth-sampler)
   (shadow-comparison-sampler :initarg :shadow-comparison-sampler
                              :reader luvcraft-session-shadow-comparison-sampler)
   (layout :initarg :layout :reader luvcraft-session-layout)
   (shadow-layout :initarg :shadow-layout
                  :reader luvcraft-session-shadow-layout)
   (block-pipeline :initarg :block-pipeline
                   :reader luvcraft-session-block-pipeline)
   (shadow-pipeline :initarg :shadow-pipeline
                    :reader luvcraft-session-shadow-pipeline)
   (sky-vertex-buffer :initarg :sky-vertex-buffer :initform nil
                      :reader luvcraft-session-sky-vertex-buffer)
   (sky-pipeline :initarg :sky-pipeline :initform nil
                 :reader luvcraft-session-sky-pipeline)
   (crosshair-vertex-buffer
    :initarg :crosshair-vertex-buffer
    :reader luvcraft-session-crosshair-vertex-buffer)
   (crosshair-pipeline :initarg :crosshair-pipeline
                       :reader luvcraft-session-crosshair-pipeline)
   (frame-states :initform (make-hash-table :test #'eql)
                 :reader luvcraft-session-frame-states)
   (resources :initarg :resources :initform nil
              :accessor luvcraft-session-resources)
   (pressed-keys :initform (make-hash-table :test #'eq)
                 :reader luvcraft-session-pressed-keys)
   (pointer-captured-p :initform nil
                       :accessor luvcraft-session-pointer-captured-p)
   (last-frame-time :initform nil
                    :type (or null double-float)
                    :quantity (:quantity :monotonic-frame-time :unit :second)
                    :accessor luvcraft-session-last-frame-time)
   (physics-accumulator :initform 0d0
                        :type double-float
                        :quantity (:quantity :physics-accumulated-duration
                                   :unit :second)
                        :accessor luvcraft-session-physics-accumulator)
   (jump-requested-p :initform nil
                     :accessor luvcraft-session-jump-requested-p)
   (running-p :initform t :accessor luvcraft-session-running-p))
  (:metaclass luv.arithmetic.records:quantity-class))

(defun request-luvcraft-session-checkpoint (session)
  "Capture SESSION's durable state and submit it to its asynchronous writer."
  (let ((writer (luvcraft-session-checkpoint-writer session)))
    (when writer
      (request-world-checkpoint
       writer
       (make-luvcraft-save-description
        (luvcraft-session-world session)
        :camera (luvcraft-session-camera session)
        :player (luvcraft-session-player session)
        :selected-block (luvcraft-session-selected-block session))))))

(defun luvcraft-session-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-block-pipeline session)))

(defun luvcraft-session-shadow-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-shadow-pipeline session)))

(defun luvcraft-session-crosshair-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-crosshair-pipeline session)))

(defun luvcraft-session-sky-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-sky-pipeline session)))

(defun refresh-luvcraft-shaders (session)
  "Install any successfully redefined block-world shader methods."
  (refresh-live-shader-pipeline (luvcraft-session-block-pipeline session))
  (refresh-live-shader-pipeline (luvcraft-session-shadow-pipeline session))
  (refresh-live-shader-pipeline (luvcraft-session-sky-pipeline session))
  (refresh-live-shader-pipeline (luvcraft-session-crosshair-pipeline session))
  session)

(defun luvcraft-session-target
    (session &key (max-distance +luvcraft-target-reach+))
  "Raycast from SESSION's camera through resident block terrain."
  (let ((camera (luvcraft-session-camera session)))
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore right up))
      (raycast-block-world
       (luvcraft-session-world session)
       (camera-position camera)
       forward #'block-solid-p :max-distance max-distance))))

(defun update-luvcraft-session-title (session)
  (let* ((blocks (placeable-block-kinds))
         (block (luvcraft-session-selected-block session))
         (number (position block blocks :test #'eq)))
    (when (slot-boundp session 'canvas)
      (setf (canvas-title (luvcraft-session-canvas session))
            (format nil "~A — [~A] ~(~A~)  ·  1–~D select  ·  shift sprint"
                    (luvcraft-session-title-base session)
                    (if number (1+ number) "?")
                    (block-kind-name block)
                    (length blocks)))))
  session)

(defun select-luvcraft-block (session number)
  "Select the one-based numbered placeable material and update the title."
  (check-type number (integer 1))
  (let ((block (nth (1- number) (placeable-block-kinds))))
    (when block
      (setf (luvcraft-session-selected-block session) block)
      (update-luvcraft-session-title session))
    block))

(defun pick-luvcraft-block (session)
  "Select the material currently under the centre crosshair."
  (multiple-value-bind (hit status) (luvcraft-session-target session)
    (when hit
      (setf (luvcraft-session-selected-block session)
            (block-ray-hit-block hit))
      (update-luvcraft-session-title session))
    (values (and hit (block-ray-hit-block hit)) status)))

(defun edit-luvcraft-block (session action)
  "Apply ACTION (:REMOVE or :PLACE) along SESSION's centre view ray."
  (multiple-value-bind (hit status) (luvcraft-session-target session)
    (unless hit
      (return-from edit-luvcraft-block (values nil status)))
    (let* ((world (luvcraft-session-world session))
           (coordinate
             (ecase action
               (:remove (block-ray-hit-coordinate hit))
               (:place (block-ray-hit-adjacent-coordinate hit)))))
      (unless coordinate
        (return-from edit-luvcraft-block (values nil :blocked)))
      (let ((x (world-coordinate-x coordinate))
            (y (world-coordinate-y coordinate))
            (z (world-coordinate-z coordinate)))
        (multiple-value-bind (old-block residency) (world-block-at world x y z)
          (unless (eq residency :resident)
            (return-from edit-luvcraft-block (values nil :absent)))
          (ecase action
            (:remove
             (edit-block-at nil world x y z))
            (:place
             (when old-block
               (return-from edit-luvcraft-block (values nil :blocked)))
             (when (player-overlaps-block-p
                    (luvcraft-session-player session) x y z)
               (return-from edit-luvcraft-block (values nil :blocked)))
             (edit-block-at
              (luvcraft-session-selected-block session) world x y z)))
          (request-luvcraft-session-checkpoint session)
          (values coordinate :edited))))))
