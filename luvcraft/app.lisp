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

;;; Every scene stage writes linear radiance into a floating-point attachment
;;; instead of an already-encoded display image.  A sun disc, a specular
;;; glint, or an emissive crystal is then allowed to be far brighter than
;;; display white, which is exactly the signal the bloom chain feeds on and
;;; the tonemapper rolls off.  Only the final presentation image is an sRGB
;;; eight-bit surface.
(defconstant +luvcraft-scene-color-format+ :rgba16-float
  "The linear HDR attachment format shared by every scene-stage pipeline.")

(defconstant +luvcraft-bloom-color-format+ :rgba16-float
  "The reduced-resolution attachment format of the bloom and shaft chain.")

(defconstant +luvcraft-bloom-divisor+ 4
  "How much smaller each bloom chain attachment is than the frame.")

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
   (inventory :initarg :inventory :initform (make-block-inventory)
              :reader luvcraft-session-inventory)
   (particle-system :initarg :particle-system
                    :initform (make-instance 'block-particle-system)
                    :reader luvcraft-session-particle-system)
   (critters :initarg :critters
             :initform (make-instance 'critter-population)
             :reader luvcraft-session-critters)
   ;; The player's own arms and hands, and what they hold.  See BODY.LISP.
   (body :initarg :body :initform (make-instance 'player-body)
         :reader luvcraft-session-body)
   (title-base :initarg :title-base :initform "luvcraft"
               :reader luvcraft-session-title-base)
   (atlas-texture :initarg :atlas-texture
                  :reader luvcraft-session-atlas-texture)
   (atlas-view :initarg :atlas-view :reader luvcraft-session-atlas-view)
   (atlas-sampler :initarg :atlas-sampler
                  :reader luvcraft-session-atlas-sampler)
   (normal-atlas-texture :initarg :normal-atlas-texture
                         :reader luvcraft-session-normal-atlas-texture)
   (normal-atlas-view :initarg :normal-atlas-view
                      :reader luvcraft-session-normal-atlas-view)
   ;; The attachments below are the frame's own size rather than the world's,
   ;; so a window resize replaces all of them together.  RENDER-EXTENT records
   ;; the size they were made for; a frame that finds the drawable disagreeing
   ;; with it rebuilds them before encoding anything.
   (render-extent :initarg :render-extent :initform nil
                  :accessor luvcraft-session-render-extent)
   (color-texture :initarg :color-texture
                  :accessor luvcraft-session-color-texture)
   (color-view :initarg :color-view :accessor luvcraft-session-color-view)
   (depth-texture :initarg :depth-texture
                  :accessor luvcraft-session-depth-texture)
   (depth-view :initarg :depth-view :accessor luvcraft-session-depth-view)
   (presentation-texture :initarg :presentation-texture
                         :accessor luvcraft-session-presentation-texture)
   (presentation-view :initarg :presentation-view
                      :accessor luvcraft-session-presentation-view)
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
   (post-layout :initarg :post-layout :reader luvcraft-session-post-layout)
   (bloom-layout :initarg :bloom-layout :initform nil
                 :reader luvcraft-session-bloom-layout)
   (linear-sampler :initarg :linear-sampler :initform nil
                   :reader luvcraft-session-linear-sampler)
   ;; The lens chain ping-pongs between two reduced attachments: the blurred
   ;; bloom ends on the primary one and the light shafts on the secondary.
   (bloom-primary-texture :initarg :bloom-primary-texture :initform nil
                          :accessor luvcraft-session-bloom-primary-texture)
   (bloom-primary-view :initarg :bloom-primary-view :initform nil
                       :accessor luvcraft-session-bloom-primary-view)
   (bloom-secondary-texture :initarg :bloom-secondary-texture :initform nil
                            :accessor luvcraft-session-bloom-secondary-texture)
   (bloom-secondary-view :initarg :bloom-secondary-view :initform nil
                         :accessor luvcraft-session-bloom-secondary-view)
   (bloom-bright-pipeline :initarg :bloom-bright-pipeline :initform nil
                          :reader luvcraft-session-bloom-bright-pipeline)
   (bloom-horizontal-pipeline :initarg :bloom-horizontal-pipeline :initform nil
                              :reader luvcraft-session-bloom-horizontal-pipeline)
   (bloom-vertical-pipeline :initarg :bloom-vertical-pipeline :initform nil
                            :reader luvcraft-session-bloom-vertical-pipeline)
   (sun-shaft-pipeline :initarg :sun-shaft-pipeline :initform nil
                       :reader luvcraft-session-sun-shaft-pipeline)
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
   (post-pipeline :initarg :post-pipeline
                  :reader luvcraft-session-post-pipeline)
   (particle-vertex-buffer :initarg :particle-vertex-buffer
                           :reader luvcraft-session-particle-vertex-buffer)
   (critter-vertex-buffer :initarg :critter-vertex-buffer
                          :reader luvcraft-session-critter-vertex-buffer)
   (body-vertex-buffer :initarg :body-vertex-buffer :initform nil
                       :reader luvcraft-session-body-vertex-buffer)
   (world-text :initarg :world-text :initform nil
               :reader luvcraft-session-world-text)
   (world-text-glyph-cache :initarg :world-text-glyph-cache :initform nil
                           :reader luvcraft-session-world-text-glyph-cache)
   (video-screen :initarg :video-screen :initform nil
                 :accessor luvcraft-session-video-screen)
   (overlays :initform nil :accessor luvcraft-session-overlays)
   (modal-focus :initform nil :accessor luvcraft-session-modal-focus)
   (focus-camera-origin :initform nil
                        :accessor luvcraft-session-focus-camera-origin)
   (focus-toggle-tab-down-p
    :initform nil :accessor luvcraft-session-focus-toggle-tab-down-p)
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

(defgeneric encode-luvcraft-overlay (overlay session pass surface-texture)
  (:documentation
   "Encode OVERLAY into SESSION's open scene PASS for SURFACE-TEXTURE."))

(defmethod encode-luvcraft-overlay (overlay session pass surface-texture)
  (declare (ignore overlay session pass surface-texture))
  nil)

(defgeneric luvcraft-overlay-stage (overlay)
  (:documentation
   "Return :SCENE for depth-bearing overlays or :HUD for crisp presentation."))

(defmethod luvcraft-overlay-stage (overlay)
  (declare (ignore overlay))
  :scene)

(defgeneric refresh-luvcraft-overlay (overlay session)
  (:documentation
   "Publish any complete pending render state for OVERLAY at a frame boundary."))

(defmethod refresh-luvcraft-overlay (overlay session)
  (declare (ignore overlay session))
  nil)

(defgeneric release-luvcraft-overlay (overlay)
  (:documentation "Release resources owned by an object attached to luvcraft."))

(defmethod release-luvcraft-overlay (overlay)
  (declare (ignore overlay))
  nil)

(defgeneric handle-luvcraft-overlay-event (overlay session canvas event)
  (:documentation
   "Handle EVENT projected onto OVERLAY, returning true when consumed."))

(defmethod handle-luvcraft-overlay-event (overlay session canvas event)
  (declare (ignore overlay session canvas event))
  nil)

(defgeneric luvcraft-focus-entered (focus session)
  (:documentation "Notify FOCUS that SESSION has entered its interaction mode."))

(defmethod luvcraft-focus-entered (focus session)
  (declare (ignore focus session))
  nil)

(defgeneric luvcraft-focus-left (focus session)
  (:documentation "Notify FOCUS that SESSION has left its interaction mode."))

(defmethod luvcraft-focus-left (focus session)
  (declare (ignore focus session))
  nil)

(defgeneric handle-luvcraft-focus-event (focus session canvas event)
  (:documentation
   "Handle EVENT while FOCUS owns SESSION's modal player interaction."))

(defgeneric handle-luvcraft-focus-control-event (focus session canvas event)
  (:documentation
   "Offer EVENT to controls which belong to focused FOCUS, returning true when consumed.

This is narrower than the ordinary overlay event path: a focused world object
may expose a HUD control without allowing an unconsumed click to fall through
to block editing or to unrelated overlays."))

(defmethod handle-luvcraft-focus-control-event
    (focus session canvas event)
  (declare (ignore focus session canvas event))
  nil)

(defgeneric luvcraft-focus-score (focus session)
  (:documentation
   "Return a non-negative targeting score when FOCUS can be entered by TAB."))

(defmethod luvcraft-focus-score (focus session)
  (declare (ignore focus session))
  nil)

(defgeneric luvcraft-focus-camera-pose (focus session)
  (:documentation
   "Return FOCUS's desired CAMERA-POSE in SESSION, or NIL for an FOV-only cue."))

(defmethod luvcraft-focus-camera-pose (focus session)
  (declare (ignore focus session))
  nil)

(defgeneric luvcraft-focus-carries-player-p (focus)
  (:documentation
   "Whether FOCUS moves the player itself instead of the player controller.

A terminal on a wall leaves the player standing in front of it and the ordinary
scalar controller keeps running.  A mount does not: it carries the player, so
the controller must stand down for as long as the interaction lasts."))

(defmethod luvcraft-focus-carries-player-p (focus)
  (declare (ignore focus))
  nil)

(defgeneric advance-luvcraft-focus (focus session seconds)
  (:documentation
   "Let FOCUS take its own turn in SESSION's simulation, before the player's.

A focus which only reads input needs nothing here; a moving one -- a mount, a
vehicle, a cutscene -- does its own work in this method."))

(defmethod advance-luvcraft-focus (focus session seconds)
  (declare (ignore focus session seconds))
  nil)

(defgeneric luvcraft-overlay-focus-insets (overlay session)
  (:documentation
   "Return left, top, right, and bottom pixel insets obscured by OVERLAY."))

(defmethod luvcraft-overlay-focus-insets (overlay session)
  (declare (ignore overlay session))
  (values 0.0 0.0 0.0 0.0))

(defun luvcraft-session-focus-insets (session)
  (let ((left 0.0) (top 0.0) (right 0.0) (bottom 0.0))
    (dolist (overlay (luvcraft-session-overlays session))
      (multiple-value-bind
          (overlay-left overlay-top overlay-right overlay-bottom)
          (luvcraft-overlay-focus-insets overlay session)
        (setf left (max left overlay-left)
              top (max top overlay-top)
              right (max right overlay-right)
              bottom (max bottom overlay-bottom))))
    (values left top right bottom)))

(defun luvcraft-session-focus-camera-active-p (session)
  (not (null (luvcraft-session-focus-camera-origin session))))

(defun player-camera-position (player)
  (make-vec3 (player-x player)
             (+ (player-y player) (player-eye-height player))
             (player-z player)))

(defun luvcraft-session-return-camera-pose (session)
  (let* ((origin (luvcraft-session-focus-camera-origin session))
         (player (and (slot-boundp session 'player)
                      (luvcraft-session-player session))))
    (when origin
      (make-camera-pose
       (if player
           (player-camera-position player)
           (copy-camera-position (camera-pose-position origin)))
       (camera-pose-yaw origin)
       (camera-pose-pitch origin)
       (camera-pose-field-of-view origin)))))

(defun advance-luvcraft-focus-camera (session seconds)
  "Advance SESSION's cinematic focus pose after ordinary player simulation."
  (let ((origin (luvcraft-session-focus-camera-origin session)))
    (when (and origin (slot-boundp session 'camera))
      (let* ((camera (luvcraft-session-camera session))
             (focus (luvcraft-session-modal-focus session))
             (target
               (if focus
                   (or (luvcraft-focus-camera-pose focus session)
                       (make-camera-pose
                        (copy-camera-position (camera-position camera))
                        (camera-yaw camera) (camera-pitch camera)
                        +luvcraft-camera-focused-vertical-field-of-view+))
                   (luvcraft-session-return-camera-pose session)))
             (error (advance-camera-focus camera target seconds)))
        (when (and (null focus) (< error 1e-4))
          (set-camera-pose camera target)
          (setf (luvcraft-session-focus-camera-origin session) nil)))))
  session)

(defgeneric activate-luvcraft-target (block session hit)
  (:documentation
   "Create and return a focusable interaction for targeted BLOCK, or NIL."))

(defmethod activate-luvcraft-target (block session hit)
  (declare (ignore block session hit))
  nil)

(defgeneric activate-luvcraft-critter (critter session)
  (:documentation
   "Create and return a focusable interaction for targeted CRITTER, or NIL.

The animal counterpart of ACTIVATE-LUVCRAFT-TARGET: an animal which can be
mounted answers with the ride, and one which cannot answers NIL and is simply
looked at."))

(defmethod activate-luvcraft-critter (critter session)
  (declare (ignore critter session))
  nil)

(defgeneric attach-luvcraft-hud (session)
  (:documentation
   "Attach the player HUD supplied by the loaded presentation system.

The base game deliberately has no presentation-system dependency.  An
extension such as MCLUV may specialize this at the session boundary and
return its attached overlay."))

(defmethod attach-luvcraft-hud ((session t))
  (declare (ignore session))
  nil)

(defun clear-luvcraft-player-input (session)
  (clrhash (luvcraft-session-pressed-keys session))
  (setf (luvcraft-session-jump-requested-p session) nil
        (player-body-brandishing-p (luvcraft-session-body session)) nil)
  (when (luvcraft-session-pointer-captured-p session)
    (set-canvas-relative-pointer-mode
     (luvcraft-session-canvas session) nil)
    (setf (luvcraft-session-pointer-captured-p session) nil))
  session)

(defun unfocus-luvcraft-session (session)
  "Leave SESSION's modal interaction, returning the object which was focused."
  (let ((focus (luvcraft-session-modal-focus session)))
    (when focus
      ;; Publish the unfocused state before the callback so a callback may
      ;; safely establish another focus without being cleared afterward.
      (setf (luvcraft-session-modal-focus session) nil)
      (clear-luvcraft-player-input session)
      (luvcraft-focus-left focus session))
    focus))

(defun focus-luvcraft-session (session focus)
  "Enter modal interaction with FOCUS, suspending ordinary player input.

This is the common session transition behind using a world terminal or book,
mounting a vehicle, and other interactions described by #8JCMA5."
  (check-type session luvcraft-session)
  (when (null focus)
    (error "Use UNFOCUS-LUVCRAFT-SESSION to leave modal focus."))
  (unless (eq focus (luvcraft-session-modal-focus session))
    (when (and (slot-boundp session 'camera)
               (null (luvcraft-session-focus-camera-origin session)))
      (setf (luvcraft-session-focus-camera-origin session)
            (camera-pose-from-camera (luvcraft-session-camera session))))
    (unfocus-luvcraft-session session)
    (clear-luvcraft-player-input session)
    (setf (luvcraft-session-modal-focus session) focus)
    (handler-case
        (luvcraft-focus-entered focus session)
      (error (condition)
        (setf (luvcraft-session-modal-focus session) nil)
        (error condition))))
  focus)

(defun dispatch-luvcraft-focus-event (session canvas event)
  (let ((focus (luvcraft-session-modal-focus session)))
    (when focus
      (handle-luvcraft-focus-event focus session canvas event)
      t)))

(defun luvcraft-session-targeted-critter
    (session &key (max-distance +luvcraft-target-reach+))
  "Return the animal SESSION's centre view ray reaches first, and how far.

An animal standing behind a wall is not targeted: whichever of the animal and
the terrain the ray meets first is what the player is looking at."
  (let ((camera (luvcraft-session-camera session)))
    (multiple-value-bind (right up forward) (camera-basis camera)
      (declare (ignore right up))
      (multiple-value-bind (critter distance)
          (critter-along-ray (luvcraft-session-critters session)
                             (camera-position camera) forward max-distance)
        (when critter
          (let ((hit (luvcraft-session-target
                      session :max-distance max-distance)))
            (unless (and hit (< (block-ray-hit-distance hit) distance))
              (values critter distance))))))))

(defun luvcraft-session-focus-candidate (session)
  "Return or activate SESSION's best currently targeted focusable object."
  (or (loop with best = nil
            with best-score = nil
            for overlay in (luvcraft-session-overlays session)
            for score = (luvcraft-focus-score overlay session)
            when (and score
                      (or (null best-score) (< score best-score)))
              do (setf best overlay
                       best-score score)
            finally (return best))
      (alexandria:when-let
          ((critter (luvcraft-session-targeted-critter session)))
        (activate-luvcraft-critter critter session))
      (multiple-value-bind (hit status) (luvcraft-session-target session)
        (declare (ignore status))
        (when hit
          (activate-luvcraft-target
           (block-ray-hit-block hit) session hit)))))

(defun toggle-luvcraft-session-focus (session)
  "Leave modal focus, or enter the best overlay currently targeted by SESSION."
  (if (luvcraft-session-modal-focus session)
      (unfocus-luvcraft-session session)
      (alexandria:when-let
          ((candidate (luvcraft-session-focus-candidate session)))
        (focus-luvcraft-session session candidate))))

(defun dispatch-luvcraft-overlay-event (session canvas event)
  "Offer EVENT to SESSION's frontmost overlay and report consumption."
  (some (lambda (overlay)
          (handle-luvcraft-overlay-event overlay session canvas event))
        (luvcraft-session-overlays session)))

(defun add-luvcraft-overlay (session overlay)
  "Draw OVERLAY in subsequent SESSION frames and return it."
  (pushnew overlay (luvcraft-session-overlays session) :test #'eq)
  overlay)

(defun remove-luvcraft-overlay (session overlay &key (release-p t))
  "Stop drawing OVERLAY in SESSION, optionally releasing it."
  (setf (luvcraft-session-overlays session)
        (delete overlay (luvcraft-session-overlays session) :test #'eq))
  (when (eq overlay (luvcraft-session-modal-focus session))
    (unfocus-luvcraft-session session))
  (when release-p
    (release-luvcraft-overlay overlay))
  overlay)

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

(defun luvcraft-session-post-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-post-pipeline session)))

(defun refresh-luvcraft-shaders (session)
  "Install any successfully redefined block-world shader methods."
  (refresh-live-shader-pipeline (luvcraft-session-block-pipeline session))
  (refresh-live-shader-pipeline (luvcraft-session-shadow-pipeline session))
  (refresh-live-shader-pipeline (luvcraft-session-sky-pipeline session))
  (refresh-live-shader-pipeline (luvcraft-session-crosshair-pipeline session))
  (refresh-live-shader-pipeline (luvcraft-session-post-pipeline session))
  (dolist (pipeline (list (luvcraft-session-bloom-bright-pipeline session)
                          (luvcraft-session-bloom-horizontal-pipeline session)
                          (luvcraft-session-bloom-vertical-pipeline session)
                          (luvcraft-session-sun-shaft-pipeline session)))
    (when pipeline
      (refresh-live-shader-pipeline pipeline)))
  (when (luvcraft-session-world-text session)
    (refresh-live-shader-pipeline
     (world-text-run-pipeline (luvcraft-session-world-text session))))
  (when (luvcraft-session-video-screen session)
    (refresh-live-shader-pipeline
     (video-screen-pipeline (luvcraft-session-video-screen session))))
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
  (let* ((blocks (block-inventory-quickbar-blocks
                  (luvcraft-session-inventory session)))
         (block (luvcraft-session-selected-block session))
         (number (position block blocks :test #'eq))
         (item (player-body-hand-item (luvcraft-session-body session))))
    (when (slot-boundp session 'canvas)
      (setf (canvas-title (luvcraft-session-canvas session))
            (format nil
                    "~A — [~A] ~(~A~)~@[  ·  holding ~A~]  ·  1–~D select  ·  I inventory  ·  F phone  ·  G brandish  ·  shift sprint  ·  tab focus"
                    (luvcraft-session-title-base session)
                    (if number (1+ number) "inventory")
                    (block-kind-name block)
                    (and item (hand-item-name item))
                    (length blocks)))))
  session)

(defun select-luvcraft-block (session number)
  "Select the one-based numbered placeable material and update the title."
  (check-type number (integer 1))
  (let ((block
          (nth (1- number)
               (block-inventory-blocks
                (luvcraft-session-inventory session)))))
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
             (edit-block-at nil world x y z)
             (smash-block-particles
              (luvcraft-session-particle-system session)
              old-block coordinate))
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
