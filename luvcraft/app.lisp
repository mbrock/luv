;;; The luvcraft application object and its direct interactions.
;;;
;;; A LUVCRAFT-SESSION coordinates the canvas, GPU device, world, camera,
;;; player, and a LUVCRAFT-RENDERER which solely owns the frame GPU artifacts.
;;; This file defines that coordinator plus the player-facing verbs: aiming
;;; along the view ray, selecting materials, and placing or removing blocks.
;;; Chunk streaming lives in streaming.lisp and frame rendering in render.lisp.

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
                      :accessor luvcraft-session-publication-limit)
   (load-schedule-limit :initarg :load-schedule-limit :initform 4
                        :accessor luvcraft-session-load-schedule-limit)
   (mesh-capture-limit :initarg :mesh-capture-limit :initform 1
                       :accessor luvcraft-session-mesh-capture-limit)
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
                        :accessor luvcraft-session-shadow-diagnostic-p)
   (shadow-anchor :initform nil
                  :accessor luvcraft-session-shadow-anchor
                  :documentation
                  "The world point the shadow texel lattice pivots about;
SHADOW-FRAME-ROWS walks it after the camera in whole texels.")
   (player :initarg :player :initform nil :reader luvcraft-session-player)
   (residency-radius :initarg :residency-radius :initform 6
                     :accessor luvcraft-session-residency-radius)
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
   ;; The things with weight: balls, drops, gobbets.  See PHYSICS.LISP for
   ;; the world and BALLS.LISP for what the game does with it.  Made on the
   ;; first frame, when there is a block world to collide with.
   (physics :initarg :physics :initform nil
            :accessor luvcraft-session-physics)
   (physics-clock :initform 0d0 :type double-float
                  :accessor luvcraft-session-physics-clock)
   ;; The CPU-side vertex stream the bodies are drawn from each frame, and
   ;; how much of it the last build filled.
   (physics-vertex-stream :initform nil
                          :accessor luvcraft-session-physics-vertex-stream)
   (physics-vertex-count :initform 0
                         :accessor luvcraft-session-physics-vertex-count)
   ;; Where the world's springs stand, found from its authored edits.
   (springs :initform nil :accessor luvcraft-session-springs)
   (title-base :initarg :title-base :initform "luvcraft"
               :reader luvcraft-session-title-base)
   (renderer :initarg :renderer :initform (make-instance 'luvcraft-renderer)
             :reader luvcraft-session-renderer)
   (world-text :initarg :world-text :initform nil
               :reader luvcraft-session-world-text)
   (world-text-glyph-cache :initarg :world-text-glyph-cache :initform nil
                           :reader luvcraft-session-world-text-glyph-cache)
   (video-screen :initarg :video-screen :initform nil
                 :accessor luvcraft-session-video-screen)
   ;; When set, every presented frame is also copied into this texture, so
   ;; something outside the window -- another process, through an IOSurface --
   ;; can watch the game.
   (frame-mirror :initform nil :accessor luvcraft-session-frame-mirror)
   (overlays :initform nil :accessor luvcraft-session-overlays)
   (lobby-client :initform nil :accessor luvcraft-session-lobby-client
                 :documentation
                 "The reconnecting tailnet radio owned by this session, or NIL.")
   (modal-focus :initform nil :accessor luvcraft-session-modal-focus)
   (focus-camera-origin :initform nil
                        :accessor luvcraft-session-focus-camera-origin)
   (focus-toggle-tab-down-p
    :initform nil :accessor luvcraft-session-focus-toggle-tab-down-p)
   (movement-intent
    :initform (make-movement-intent)
    :reader luvcraft-session-movement-intent
    :documentation "What the player is trying to do, set by the input layer.")
   (look-intent
    :initform (make-movement-intent)
    :reader luvcraft-session-look-intent
    :documentation "Where the player is trying to turn the camera, urged by
the arrow keys: a mouse for the mouseless console.")
   (pointer-captured-p :initform nil
                       :accessor luvcraft-session-pointer-captured-p)
   (pointer-x :initform nil :accessor luvcraft-session-pointer-x)
   (pointer-y :initform nil :accessor luvcraft-session-pointer-y)
   (pointer-dirty-p :initform t :accessor luvcraft-session-pointer-dirty-p)
   (software-cursor-p :initarg :software-cursor-p :initform nil
                      :reader luvcraft-session-software-cursor-p)
   (quit-function :initarg :quit-function :initform nil
                  :reader luvcraft-session-quit-function
                  :documentation
                  "An optional PLAY-level teardown run by the stop owner.")
   (stop-controller
    :initarg :stop-controller
    :initform (make-stop-controller :name "luvcraft session")
    :reader luvcraft-session-stop-controller
    :documentation
    "The separate one-shot owner and result publication for session teardown.")
   (pointer-capture-suspended-p
    :initform nil
    :accessor luvcraft-session-pointer-capture-suspended-p
    :documentation
    "Whether mouse look was captured when the current modal focus was entered.

A modal interaction wants an ordinary cursor -- a terminal's own UI is
clicked at a place on the screen, which relative pointer mode does not have
-- so entering focus releases the capture.  Remembering that it was held is
what lets leaving focus put the player straight back into mouse look instead
of making them click the world again.")
   (last-frame-time :initform nil
                    :type (or null double-float)
                    :quantity (:quantity :monotonic-frame-time :unit :second)
                    :accessor luvcraft-session-last-frame-time)
   (physics-accumulator :initform 0d0
                        :type double-float
                        :quantity (:quantity :physics-accumulated-duration
                                   :unit :second)
                        :accessor luvcraft-session-physics-accumulator)
   (running-p :initform t :accessor luvcraft-session-running-p))
  (:metaclass luv.arithmetic.records:quantity-class))

;;; Keep the established session-facing renderer vocabulary while making its
;;; storage and lifecycle belong to one semantic owner.  These are methods,
;;; rather than captured closures, so a live class redefinition replaces the
;;; former slot-accessor methods at their ordinary CLOS coordinates.
(defmacro define-luvcraft-renderer-forwarder
    (session-accessor renderer-accessor &key setf)
  `(progn
     (defmethod ,session-accessor ((session luvcraft-session))
       (,renderer-accessor (luvcraft-session-renderer session)))
     ,@(when setf
         `((defmethod (setf ,session-accessor)
               (value (session luvcraft-session))
             (setf (,renderer-accessor (luvcraft-session-renderer session))
                   value))))))

(define-luvcraft-renderer-forwarder
    luvcraft-session-atlas-texture luvcraft-renderer-atlas-texture :setf t)
(define-luvcraft-renderer-forwarder
    luvcraft-session-atlas-view luvcraft-renderer-atlas-view :setf t)
(define-luvcraft-renderer-forwarder
    luvcraft-session-atlas-sampler luvcraft-renderer-atlas-sampler)
(define-luvcraft-renderer-forwarder
    luvcraft-session-normal-atlas-texture
    luvcraft-renderer-normal-atlas-texture :setf t)
(define-luvcraft-renderer-forwarder
    luvcraft-session-normal-atlas-view luvcraft-renderer-normal-atlas-view
    :setf t)
(define-luvcraft-renderer-forwarder
    luvcraft-session-render-extent luvcraft-renderer-render-extent)
(define-luvcraft-renderer-forwarder
    luvcraft-session-presentation-extent
    luvcraft-renderer-presentation-extent)
(define-luvcraft-renderer-forwarder
    luvcraft-session-color-texture luvcraft-renderer-color-texture)
(define-luvcraft-renderer-forwarder
    luvcraft-session-color-view luvcraft-renderer-color-view)
(define-luvcraft-renderer-forwarder
    luvcraft-session-depth-texture luvcraft-renderer-depth-texture)
(define-luvcraft-renderer-forwarder
    luvcraft-session-depth-view luvcraft-renderer-depth-view)
(define-luvcraft-renderer-forwarder
    luvcraft-session-world-panel-color-texture
    luvcraft-renderer-world-panel-color-texture)
(define-luvcraft-renderer-forwarder
    luvcraft-session-world-panel-color-view
    luvcraft-renderer-world-panel-color-view)
(define-luvcraft-renderer-forwarder
    luvcraft-session-world-panel-depth-texture
    luvcraft-renderer-world-panel-depth-texture)
(define-luvcraft-renderer-forwarder
    luvcraft-session-world-panel-depth-view
    luvcraft-renderer-world-panel-depth-view)
(define-luvcraft-renderer-forwarder
    luvcraft-session-presentation-texture
    luvcraft-renderer-presentation-texture)
(define-luvcraft-renderer-forwarder
    luvcraft-session-presentation-view luvcraft-renderer-presentation-view)
(define-luvcraft-renderer-forwarder
    luvcraft-session-shadow-depth-texture
    luvcraft-renderer-shadow-depth-texture)
(define-luvcraft-renderer-forwarder
    luvcraft-session-shadow-depth-view luvcraft-renderer-shadow-depth-view)
(define-luvcraft-renderer-forwarder
    luvcraft-session-shadow-depth-sampler
    luvcraft-renderer-shadow-depth-sampler)
(define-luvcraft-renderer-forwarder
    luvcraft-session-shadow-comparison-sampler
    luvcraft-renderer-shadow-comparison-sampler)
(define-luvcraft-renderer-forwarder
    luvcraft-session-layout luvcraft-renderer-layout)
(define-luvcraft-renderer-forwarder
    luvcraft-session-shadow-layout luvcraft-renderer-shadow-layout)
(define-luvcraft-renderer-forwarder
    luvcraft-session-post-layout luvcraft-renderer-post-layout)
(define-luvcraft-renderer-forwarder
    luvcraft-session-bloom-layout luvcraft-renderer-bloom-layout)
(define-luvcraft-renderer-forwarder
    luvcraft-session-linear-sampler luvcraft-renderer-linear-sampler)
(define-luvcraft-renderer-forwarder
    luvcraft-session-bloom-primary-texture
    luvcraft-renderer-bloom-primary-texture)
(define-luvcraft-renderer-forwarder
    luvcraft-session-bloom-primary-view luvcraft-renderer-bloom-primary-view)
(define-luvcraft-renderer-forwarder
    luvcraft-session-bloom-secondary-texture
    luvcraft-renderer-bloom-secondary-texture)
(define-luvcraft-renderer-forwarder
    luvcraft-session-bloom-secondary-view
    luvcraft-renderer-bloom-secondary-view)
(define-luvcraft-renderer-forwarder
    luvcraft-session-bloom-bright-pipeline
    luvcraft-renderer-bloom-bright-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-bloom-horizontal-pipeline
    luvcraft-renderer-bloom-horizontal-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-bloom-vertical-pipeline
    luvcraft-renderer-bloom-vertical-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-sun-shaft-pipeline luvcraft-renderer-sun-shaft-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-block-pipeline luvcraft-renderer-block-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-shadow-pipeline luvcraft-renderer-shadow-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-sky-vertex-buffer luvcraft-renderer-sky-vertex-buffer)
(define-luvcraft-renderer-forwarder
    luvcraft-session-sky-pipeline luvcraft-renderer-sky-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-crosshair-vertex-buffer
    luvcraft-renderer-crosshair-vertex-buffer)
(define-luvcraft-renderer-forwarder
    luvcraft-session-cursor-vertex-buffer
    luvcraft-renderer-cursor-vertex-buffer)
(define-luvcraft-renderer-forwarder
    luvcraft-session-crosshair-pipeline luvcraft-renderer-crosshair-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-cursor-pipeline luvcraft-renderer-cursor-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-post-pipeline luvcraft-renderer-post-pipeline)
(define-luvcraft-renderer-forwarder
    luvcraft-session-frame-states luvcraft-renderer-frame-states)

(defparameter +retired-session-renderer-slot-initargs+
  '((atlas-texture . :atlas-texture)
    (atlas-view . :atlas-view)
    (atlas-sampler . :atlas-sampler)
    (normal-atlas-texture . :normal-atlas-texture)
    (normal-atlas-view . :normal-atlas-view)
    (shadow-depth-texture . :shadow-depth-texture)
    (shadow-depth-view . :shadow-depth-view)
    (shadow-depth-sampler . :shadow-depth-sampler)
    (shadow-comparison-sampler . :shadow-comparison-sampler)
    (layout . :layout)
    (shadow-layout . :shadow-layout)
    (post-layout . :post-layout)
    (bloom-layout . :bloom-layout)
    (linear-sampler . :linear-sampler)
    (bloom-bright-pipeline . :bloom-bright-pipeline)
    (bloom-horizontal-pipeline . :bloom-horizontal-pipeline)
    (bloom-vertical-pipeline . :bloom-vertical-pipeline)
    (sun-shaft-pipeline . :sun-shaft-pipeline)
    (block-pipeline . :block-pipeline)
    (shadow-pipeline . :shadow-pipeline)
    (sky-vertex-buffer . :sky-vertex-buffer)
    (sky-pipeline . :sky-pipeline)
    (crosshair-vertex-buffer . :crosshair-vertex-buffer)
    (cursor-vertex-buffer . :cursor-vertex-buffer)
    (crosshair-pipeline . :crosshair-pipeline)
    (cursor-pipeline . :cursor-pipeline)
    (post-pipeline . :post-pipeline)
    (frame-states . :frame-states)
    (resources . :resources))
  "The former session slots transferred into a renderer by live migration.")

(defparameter +retired-session-frame-attachment-slots+
  '((render-extent . :render-extent)
    (color-texture . :color-texture)
    (color-view . :color-view)
    (depth-texture . :depth-texture)
    (depth-view . :depth-view)
    (presentation-texture . :presentation-texture)
    (presentation-view . :presentation-view)
    (bloom-primary-texture . :bloom-primary-texture)
    (bloom-primary-view . :bloom-primary-view)
    (bloom-secondary-texture . :bloom-secondary-texture)
    (bloom-secondary-view . :bloom-secondary-view))
  "The former session slots published together as one attachment cohort.")

(defun make-luvcraft-renderer-from-retired-session-slots
    (session retired-values)
  "Adopt a pre-renderer SESSION's discarded slot values into one owner.

RETIRED-VALUES is the property list supplied by
UPDATE-INSTANCE-FOR-REDEFINED-CLASS.  Unbound old slots remain unbound in the
new renderer; owned resource and frame-state collections retain identity."
  (let ((missing (gensym "MISSING"))
        (initargs nil)
        (attachments nil))
    (dolist (mapping +retired-session-renderer-slot-initargs+)
      (let ((value (getf retired-values (car mapping) missing)))
        (unless (eq value missing)
          (setf initargs (list* (cdr mapping) value initargs)))))
    (dolist (mapping +retired-session-frame-attachment-slots+)
      (let ((value (getf retired-values (car mapping) missing)))
        (unless (eq value missing)
          (setf attachments (list* (cdr mapping) value attachments)))))
    (when attachments
      (setf initargs (list* :frame-attachments attachments initargs)))
    (dolist (mapping '((device . :device) (context . :context)))
      (when (slot-boundp session (car mapping))
        (setf initargs
              (list* (cdr mapping)
                     (slot-value session (car mapping))
                     initargs))))
    (apply #'make-instance 'luvcraft-renderer initargs)))

(defmethod update-instance-for-redefined-class :after
    ((session luvcraft-session) added-slots discarded-slots retired-values
     &rest initargs)
  (declare (ignore initargs))
  (when (and (member 'renderer added-slots)
             (intersection discarded-slots
                           (append
                            (mapcar #'car
                                    +retired-session-renderer-slot-initargs+)
                            (mapcar #'car
                                    +retired-session-frame-attachment-slots+))))
    ;; A running game is valuable state.  Moving the ownership boundary must
    ;; not require abandoning it or leave its old GPU objects unreachable.
    ;; See #V9VH79.
    (setf (slot-value session 'renderer)
          (make-luvcraft-renderer-from-retired-session-slots
           session retired-values))))

(defgeneric encode-luvcraft-overlay (overlay session pass surface-texture)
  (:documentation
   "Encode OVERLAY into SESSION's open scene PASS for SURFACE-TEXTURE."))

(defmethod encode-luvcraft-overlay (overlay session pass surface-texture)
  (declare (ignore overlay session pass surface-texture))
  nil)

(defgeneric luvcraft-overlay-stage (overlay)
  (:documentation
   "Return where OVERLAY draws: :SCENE, :WORLD-PANEL, :VIEWMODEL, :HUD, or
:NONE.

Scene overlays inhabit world depth; viewmodels are first-person geometry above
the world but below held items and the crosshair.  World panels retain world
projection and depth while drawing analytic application graphics at native
presentation density.  HUD overlays follow scene postprocessing.  :NONE
participates in no render pass."))

(defmethod luvcraft-overlay-stage (overlay)
  (declare (ignore overlay))
  :scene)

(defgeneric luvcraft-world-panel-depth (overlay session)
  (:documentation
   "Return OVERLAY's positive camera-space depth for world-panel ordering.

Native-density panels are alpha-composited far to near before their completed
depth layer meets the game scene.  Non-spatial panel implementations may keep
the stable attachment order by returning the default zero depth."))

(defmethod luvcraft-world-panel-depth (overlay session)
  (declare (ignore overlay session))
  0.0)

(defgeneric luvcraft-overlay-live-shader-pipelines (overlay)
  (:documentation
   "The live shader pipelines OVERLAY owns, so the session can count them
among its own; none by default.")
  (:method (overlay)
    (declare (ignore overlay))
    nil))

(defgeneric refresh-luvcraft-overlay (overlay session)
  (:documentation
   "Publish any complete pending render state for OVERLAY at a frame boundary."))

(defmethod refresh-luvcraft-overlay (overlay session)
  (declare (ignore overlay session))
  nil)

(defgeneric advance-luvcraft-overlay (overlay session seconds)
  (:documentation
   "Advance OVERLAY's simulation state by SECONDS at the frame boundary."))

(defmethod advance-luvcraft-overlay (overlay session seconds)
  (declare (ignore overlay session seconds))
  nil)

(defgeneric release-luvcraft-overlay (overlay)
  (:documentation "Release resources owned by an object attached to luvcraft."))

(defmethod release-luvcraft-overlay (overlay)
  (declare (ignore overlay))
  nil)

(defgeneric evict-luvcraft-overlay-frame-key (overlay frame-key)
  (:documentation
   "Evict OVERLAY resources retained for one canvas FRAME-KEY.

Offscreen captures use a fresh target, so an overlay which caches per-frame GPU
state must detach that entry before the shared capture target is destroyed."))

(defmethod evict-luvcraft-overlay-frame-key (overlay frame-key)
  (declare (ignore frame-key))
  overlay)

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

LUVCRAFT/CORE deliberately has no presentation-system dependency.
LUVCRAFT/MCCLIM specializes this at the session boundary and returns its
attached overlay."))

(defmethod attach-luvcraft-hud ((session t))
  (declare (ignore session))
  nil)

(defgeneric start-luvcraft-lobby (session)
  (:documentation
   "Start SESSION's optional background lobby service and return it."))

(defmethod start-luvcraft-lobby ((session t))
  (declare (ignore session))
  nil)

(defgeneric stop-luvcraft-lobby (session)
  (:documentation "Cooperatively stop SESSION's lobby service, if loaded."))

(defmethod stop-luvcraft-lobby ((session t))
  (declare (ignore session))
  nil)

(defgeneric perform-luvcraft-stop (session)
  (:documentation
   "Release SESSION after its stop controller has granted sole ownership."))

(defun request-luvcraft-quit (session)
  "Request SESSION's one orderly teardown beside its native canvas thread.

Native close and commands may call this without waiting.  Exactly one worker
runs the PLAY-level quit callback when present, or the session teardown itself
otherwise; repeated requests still defer native close to that same owner."
  ;; Close capture admission on the native caller before its teardown worker
  ;; exists.  A capture which won the race remains safe to finish; no later
  ;; request can enter behind the owner's eventual frame-boundary barrier.
  (request-application-capture-shutdown session)
  (setf (luvcraft-session-running-p session) nil)
  (request-controlled-stop
   (luvcraft-session-stop-controller session)
   (lambda ()
     (let ((quit-function (luvcraft-session-quit-function session)))
       (if quit-function
           (funcall quit-function session)
           (perform-luvcraft-stop session))))
   :thread-name "luvcraft session stop")
  t)

(defgeneric luvcraft-key-hint (thing)
  (:documentation
   "Return a short string naming the keystroke that reaches THING, or NIL.

A control which draws its own key hint would be inventing one: the keys are
decided in the command layer above, so the drawing asks rather than guesses,
and a rebound key changes the label on the button.")
  (:method (thing)
    (declare (ignore thing))
    nil))

(defun clear-luvcraft-player-input (session)
  (clear-movement-intent (luvcraft-session-movement-intent session))
  (clear-movement-intent (luvcraft-session-look-intent session))
  (when (luvcraft-session-pointer-captured-p session)
    (set-canvas-relative-pointer-mode
     (luvcraft-session-canvas session) nil)
    (setf (luvcraft-session-pointer-captured-p session) nil))
  session)

(defun resume-luvcraft-pointer-capture (session)
  "Take mouse look back if the modal interaction just left had suspended it."
  (when (shiftf (luvcraft-session-pointer-capture-suspended-p session) nil)
    (when (and (luvcraft-session-running-p session)
               (not (luvcraft-session-pointer-captured-p session)))
      ;; Mouse input and the cinematic return cannot both own the camera.
      ;; Finish the return before publishing capture, otherwise every mouse
      ;; delta is followed by ADVANCE-LUVCRAFT-FOCUS-CAMERA pulling yaw and
      ;; pitch back toward the pose saved on entry.
      (alexandria:when-let
          ((pose (luvcraft-session-return-camera-pose session)))
        (set-camera-pose (luvcraft-session-camera session) pose)
        (setf (luvcraft-session-focus-camera-origin session) nil))
      (set-canvas-relative-pointer-mode (luvcraft-session-canvas session) t)
      (setf (luvcraft-session-pointer-captured-p session) t)))
  session)

(defun unfocus-luvcraft-session (session)
  "Leave SESSION's modal interaction, returning the object which was focused."
  (let ((focus (luvcraft-session-modal-focus session)))
    (when focus
      ;; Publish the unfocused state before the callback so a callback may
      ;; safely establish another focus without being cleared afterward.
      (setf (luvcraft-session-modal-focus session) nil)
      (clear-luvcraft-player-input session)
      (resume-luvcraft-pointer-capture session)
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
    ;; Return the previous interaction all the way to the player's pose first.
    ;; A direct focus-to-focus transition then borrows that same pose anew;
    ;; recording before UNFOCUS would let its resume path consume the origin
    ;; and leave the second interaction with nothing to return to.
    (unfocus-luvcraft-session session)
    (when (and (slot-boundp session 'camera)
               (null (luvcraft-session-focus-camera-origin session)))
      (setf (luvcraft-session-focus-camera-origin session)
            (camera-pose-from-camera (luvcraft-session-camera session))))
    ;; Read the capture back after the previous focus has returned it, so a
    ;; focus-to-focus move keeps mouse look owed to the player rather than
    ;; forgetting it halfway.
    (let ((captured (luvcraft-session-pointer-captured-p session)))
      (clear-luvcraft-player-input session)
      (setf (luvcraft-session-pointer-capture-suspended-p session) captured))
    (setf (luvcraft-session-modal-focus session) focus)
    (handler-case
        (luvcraft-focus-entered focus session)
      (error (condition)
        (setf (luvcraft-session-modal-focus session) nil)
        (error condition))))
  focus)

(defmacro guarding-luvcraft-overlay ((session overlay phase) &body body)
  "Run BODY for OVERLAY; an error is OVERLAY's alone.  It is retained on the
session's canvas with its backtrace, logged, and the overlay is taken out of
the session -- a paint bug in a gadget must not take the world with it.
Returns BODY's values, or NIL when the overlay failed."
  (let ((backtrace (gensym "BACKTRACE")))
    `(let ((,backtrace nil))
       (handler-case
           (handler-bind ((error (lambda (condition)
                                   (declare (ignore condition))
                                   (setf ,backtrace
                                         (luv:capture-backtrace-string)))))
             ,@body)
         (error (condition)
           (fuse-luvcraft-overlay ,session ,overlay ,phase condition ,backtrace)
           nil)))))

(defun fuse-luvcraft-overlay (session overlay phase condition backtrace)
  "OVERLAY failed in PHASE with CONDITION: retain the failure and drop it.
The overlay's resources are not released here -- this may be mid-frame,
with its buffers still in the command stream -- so they are leaked, which
a development image can afford and a frame cannot."
  (luv:retain-canvas-failure (luvcraft-session-canvas session)
                             phase condition backtrace)
  (luv:log-event :luvcraft "overlay ~A is fused after failing in ~(~A~)"
                 (type-of overlay) phase)
  (remove-luvcraft-overlay session overlay :release-p nil)
  overlay)

(zdefun (dispatch-luvcraft-focus-event :zone :luvcraft/focus-event)
    (session canvas event)
  (let ((focus (luvcraft-session-modal-focus session)))
    (when focus
      (if (member focus (luvcraft-session-overlays session))
          (guarding-luvcraft-overlay (session focus :focus-event)
            (handle-luvcraft-focus-event focus session canvas event))
          (handle-luvcraft-focus-event focus session canvas event))
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
          (guarding-luvcraft-overlay (session overlay :overlay-event)
            (handle-luvcraft-overlay-event overlay session canvas event)))
        (luvcraft-session-overlays session)))

(defun luvcraft-overlay-mutation-canvas (session)
  "Return SESSION's live canvas, or NIL while its native owner does not exist."
  (when (slot-boundp session 'canvas)
    (luvcraft-session-canvas session)))

(defun call-with-luvcraft-overlay-mutation (session function)
  "Run FUNCTION at SESSION's native frame boundary when one exists.

Startup and post-quiescence cleanup still execute directly.  Once the canvas
is open, REQUEST-CANVAS-FRAME makes attachment mutation and GPU release part
of the native owner stream, after any frame which borrowed the overlay list,
and synchronously returns FUNCTION's values or condition to the caller."
  (check-type function function)
  (let ((canvas (luvcraft-overlay-mutation-canvas session)))
    (if (and canvas (eq :open (canvas-state canvas)))
        (request-canvas-frame
         canvas
         (lambda (timestamp)
           (declare (ignore timestamp))
           (funcall function)))
        (funcall function))))

(defun %add-luvcraft-overlay (session overlay)
  (let ((installed-p nil))
    (unwind-protect-releasing
        (progn
          (call-with-running-stop-controller
           (luvcraft-session-stop-controller session)
           (lambda ()
             ;; Recheck at publication: two callers may both have reached this
             ;; boundary before either became visible.
             (pushnew overlay (luvcraft-session-overlays session) :test #'eq)
             overlay)
           :attachment overlay
           :already-attached-p
           (lambda ()
             (member overlay (luvcraft-session-overlays session) :test #'eq)))
          (setf installed-p t)
          overlay)
      ;; ADD consumes a newly offered attachment on either outcome.  A
      ;; rejected constructor therefore unwinds with no frame, pipeline,
      ;; worker, or other application resource stranded beside the game.
      (unless installed-p
        (releasing :rejected-overlay
          (release-luvcraft-overlay overlay))))))

(defun add-luvcraft-overlay (session overlay)
  "Attach OVERLAY at SESSION's next native frame boundary and return it.

ADD consumes a newly offered OVERLAY on both success and terminal rejection.
Once SESSION begins stopping, the rejected overlay is released exactly once
before APPLICATION-ATTACHMENT-CLOSED is signalled."
  ;; An EQ object already owned by this session is not a fresh offer.  Resolve
  ;; that idempotent case before a closing canvas can reject the owner request
  ;; and make the caller mistake registered state for unpublished state.
  (when (member overlay (luvcraft-session-overlays session) :test #'eq)
    (return-from add-luvcraft-overlay overlay))
  (let ((consumed-p nil))
    (unwind-protect-releasing
        (progn
          (handler-case
              (call-with-luvcraft-overlay-mutation
               session
               (lambda ()
                 (setf consumed-p t)
                 (%add-luvcraft-overlay session overlay)))
            (application-attachment-closed (condition)
              (error condition))
            (error (condition)
              ;; A native request can lose its closing-canvas race before the
              ;; mutation starts.  Once the application gate is terminal, name
              ;; that semantic rejection instead of leaking the backend detail
              ;; through ADD's ownership boundary.
              (let* ((controller
                       (luvcraft-session-stop-controller session))
                     (state (stop-controller-state controller)))
                (cond
                  ;; Another serialized add may have published this same EQ
                  ;; object before our native request lost its close race.
                  ((and (not consumed-p)
                        (member overlay
                                (luvcraft-session-overlays session)
                                :test #'eq))
                   (setf consumed-p t)
                   overlay)
                  ((and (not consumed-p) (not (eq :running state)))
                   (error 'application-attachment-closed
                          :controller controller
                          :attachment overlay
                          :state state))
                  (t (error condition))))))
          overlay)
      ;; REQUEST-CANVAS-FRAME can itself reject a closing canvas before the
      ;; mutation callback begins.  The registry has not consumed OVERLAY in
      ;; that case, so no frame can have borrowed it.  Finish the transfer on
      ;; this caller: GPU DESTROY is synchronized by the backend retirement
      ;; ledger and is therefore safe even while terminal device teardown wins
      ;; the race beside us.
      (unless consumed-p
        (releasing :unpublished-overlay
          (release-luvcraft-overlay overlay))))))

(defun %remove-luvcraft-overlay (session overlay release-p)
  (setf (luvcraft-session-overlays session)
        (delete overlay (luvcraft-session-overlays session) :test #'eq))
  (when (eq overlay (luvcraft-session-modal-focus session))
    (unfocus-luvcraft-session session))
  (when release-p
    (release-luvcraft-overlay overlay))
  overlay)

(defun remove-luvcraft-overlay (session overlay &key (release-p t))
  "Detach OVERLAY at a frame boundary and optionally release it there.

The no-release path is intentionally preserved for FUSE-LUVCRAFT-OVERLAY:
that path may run inside a frame whose command stream still borrows OVERLAY."
  (call-with-luvcraft-overlay-mutation
   session
   (lambda () (%remove-luvcraft-overlay session overlay release-p))))

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
        :selected-block (luvcraft-session-selected-block session)
        :carried (block-inventory-carried-blocks
                  (luvcraft-session-inventory session)))))))

(defun luvcraft-session-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-block-pipeline session)))

(defun luvcraft-session-shadow-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-shadow-pipeline session)))

(defun luvcraft-session-crosshair-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-crosshair-pipeline session)))

(defun luvcraft-session-cursor-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-cursor-pipeline session)))

(defun luvcraft-session-sky-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-sky-pipeline session)))

(defun luvcraft-session-post-native-pipeline (session)
  (live-shader-pipeline-native-pipeline
   (luvcraft-session-post-pipeline session)))

(defun luvcraft-session-live-shader-pipelines (session)
  "Every live shader pipeline coordinated by SESSION, optional ones included."
  (remove nil
          (append
           (luvcraft-renderer-pipelines
            (luvcraft-session-renderer session))
           (list (and (luvcraft-session-world-text session)
                      (world-text-run-pipeline
                       (luvcraft-session-world-text session)))
                 (and (luvcraft-session-video-screen session)
                      (video-screen-pipeline
                       (luvcraft-session-video-screen session))))
           (loop for overlay in (luvcraft-session-overlays session)
                 append (luvcraft-overlay-live-shader-pipelines overlay)))))

(defmethod application-live-artifacts ((session luvcraft-session))
  "Enumerate Luvcraft's per-pipeline artifacts for shared developer tools."
  (luvcraft-session-live-shader-pipelines session))

(defun refresh-luvcraft-shaders (session)
  "Install any successfully redefined block-world shader methods."
  (refresh-application-live-artifacts session))

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
                    "~A — [~A] ~(~A~)~@[  ·  holding ~A~]  ·  ~A select  ·  e place  ·  x mine  ·  c pick  ·  I inventory  ·  F phone  ·  shift sprint  ·  arrows look  ·  tab focus  ·  ctrl-q quit"
                    (luvcraft-session-title-base session)
                    ;; The tenth slot is the 0 key, so its chip says 0 rather
                    ;; than advertising a 10 key that does not exist.
                    (if number (mod (1+ number) 10) "inventory")
                    (block-kind-name block)
                    (and item (hand-item-name item))
                    (if (= 10 (length blocks))
                        "1–9,0"
                        (format nil "1–~D" (length blocks)))))))
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

(defun refresh-luvcraft-inventory (session)
  "Give SESSION any newly defined placeable materials, in number-key order.

The live half of adding a material: REFRESH-BLOCK-ATLAS repaints the wall,
and this puts the new block on the number row of a running session.
Existing entries keep their identity and quantities; entries the palette no
longer names stay reachable at the end, past the number keys."
  (let* ((inventory (luvcraft-session-inventory session))
         (entries (block-inventory-entries inventory))
         (palette (placeable-block-kinds)))
    (setf (block-inventory-entries inventory)
          (append
           (loop for block in palette
                 collect (or (find block entries
                                   :key #'block-inventory-entry-block)
                             (make-instance 'block-inventory-entry
                                            :block block :quantity nil)))
           (remove-if (lambda (entry)
                        (member (block-inventory-entry-block entry) palette))
                      entries))))
  (update-luvcraft-session-title session)
  session)

(defparameter +luvcraft-keyboard-look-rate+ 2.2d0
  "Radians per second of camera turn while an arrow key is held.")

(defun advance-luvcraft-keyboard-look (session seconds)
  "Turn SESSION's camera as its look intent urges: a mouse for the mouseless."
  (let* ((intent (luvcraft-session-look-intent session))
         (camera (luvcraft-session-camera session))
         (yaw-amount (movement-intent-axis intent :right :left))
         (pitch-amount (movement-intent-axis intent :up :down)))
    (unless (zerop yaw-amount)
      (incf (camera-yaw camera)
            (* yaw-amount +luvcraft-keyboard-look-rate+ seconds)))
    (unless (zerop pitch-amount)
      (setf (camera-pitch camera)
            (max -1.5
                 (min 1.5
                      (+ (camera-pitch camera)
                         (* pitch-amount +luvcraft-keyboard-look-rate+
                            seconds)))))))
  session)

(defun pick-luvcraft-block (session)
  "Select the material currently under the centre crosshair."
  (multiple-value-bind (hit status) (luvcraft-session-target session)
    (when hit
      (setf (luvcraft-session-selected-block session)
            (block-ray-hit-block hit))
      (update-luvcraft-session-title session))
    (values (and hit (block-ray-hit-block hit)) status)))

(defgeneric luvcraft-block-placed (block session x y z)
  (:documentation
   "BLOCK has just been put down at X,Y,Z by the player in SESSION.

Most blocks are inert and the default does nothing; a block that does
something where it stands -- a film beside a wall -- answers here.")
  (:method (block session x y z)
    (declare (ignore block session x y z))
    nil))

(defgeneric luvcraft-block-removed (block session x y z)
  (:documentation
   "BLOCK has just been taken away from X,Y,Z by the player in SESSION.")
  (:method (block session x y z)
    (declare (ignore block session x y z))
    nil))

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
          ;; The ground is about to move: whatever was asleep on it must
          ;; find out.
          (when (luvcraft-session-physics session)
            (wake-physics-bodies-near (luvcraft-session-physics session)
                                      (+ x 0.5) (+ y 0.5) (+ z 0.5) 2.5))
          (ecase action
            (:remove
             (edit-block-at nil world x y z)
             (smash-block-particles
              (luvcraft-session-particle-system session)
              old-block coordinate)
             ;; A block worth carrying is picked up rather than smashed away.
             (when (block-kind-carried-p old-block)
               (add-block-to-inventory
                (luvcraft-session-inventory session) old-block 1)
               (setf (luvcraft-session-selected-block session) old-block)
               (update-luvcraft-session-title session))
             (luvcraft-block-removed old-block session x y z))
            (:place
             (when old-block
               (return-from edit-luvcraft-block (values nil :blocked)))
             (when (player-overlaps-block-p
                    (luvcraft-session-player session) x y z)
               (return-from edit-luvcraft-block (values nil :blocked)))
             (let ((block (luvcraft-session-selected-block session)))
               ;; Creative materials are unlimited; a carried one is spent.
               (when (and (block-kind-carried-p block)
                          (not (remove-block-from-inventory
                                (luvcraft-session-inventory session) block 1)))
                 (return-from edit-luvcraft-block (values nil :blocked)))
               (edit-block-at block world x y z)
               (luvcraft-block-placed block session x y z))))
          (request-luvcraft-session-checkpoint session)
          (values coordinate :edited))))))

;;; ---------------------------------------------------------------------
;;; The session's knobs: values that live on the session, its camera, its
;;; player, its sky clock, its animals.  All are read every frame.

(defun camera-field-of-view-degrees (camera)
  (* (camera-field-of-view camera) (/ 180 pi)))

(defun (setf camera-field-of-view-degrees) (degrees camera)
  (setf (camera-field-of-view camera) (coerce (* degrees (/ pi 180)) 'single-float))
  degrees)

(defun camera-sensitivity-milliradians (camera)
  (* 1000 (camera-sensitivity camera)))

(defun (setf camera-sensitivity-milliradians) (milliradians camera)
  (setf (camera-sensitivity camera) (coerce (/ milliradians 1000) 'single-float))
  milliradians)

(define-knob time-of-day
    (:group :sky :quantity (:quantity :time-of-day :unit :hour)
     :minimum 0.0 :maximum 24.0 :step 0.25)
    (sky-clock-hour (luvcraft-session-sky-clock session)))
(define-knob day-length
    (:group :sky :quantity (:quantity :day-length :unit :minute)
     :minimum 0.5 :maximum 60.0 :step 0.5)
    (sky-clock-minutes-per-day (luvcraft-session-sky-clock session)))
(define-knob freeze-time
    (:group :sky :class 'switch-knob :label "freeze the clock"
     :quantity (:quantity :switch :unit :one))
    (sky-clock-paused-p (luvcraft-session-sky-clock session)))

(define-knob field-of-view
    (:group :camera :quantity (:quantity :camera-field-of-view :unit :degree)
     :type double-float :minimum 30.0 :maximum 140.0 :step 1.0)
    (camera-field-of-view-degrees (luvcraft-session-camera session)))
(define-knob look-sensitivity
    (:group :camera :label "mouse sensitivity"
     :quantity (:quantity :look-sensitivity :unit :milliradian)
     :unit-label " mrad/px" :minimum 0.2 :maximum 20.0 :step 0.1)
    (camera-sensitivity-milliradians (luvcraft-session-camera session)))
(define-knob shadow-diagnostic
    (:group :shadows :class 'switch-knob :label "shadow diagnostic view"
     :quantity (:quantity :switch :unit :one))
    (luvcraft-session-shadow-diagnostic-p session))

(define-knob walk-speed
    (:group :player
     :quantity (:quantity :player-walk-speed :unit ((:cell 1) (:second -1)))
     :type double-float :minimum 0.5 :maximum 30.0 :step 0.5)
    (player-walk-speed (luvcraft-session-player session)))
(define-knob jump-speed
    (:group :player
     :quantity (:quantity :player-jump-speed :unit ((:cell 1) (:second -1)))
     :type double-float :minimum 0.0 :maximum 30.0 :step 0.5)
    (player-jump-speed (luvcraft-session-player session)))
(define-knob gravity
    (:group :player
     :quantity (:quantity :gravity-magnitude :unit ((:cell 1) (:second -2)))
     :type double-float :minimum 0.0 :maximum 100.0 :step 1.0)
    (player-gravity (luvcraft-session-player session)))
(define-knob eye-height
    (:group :player :quantity (:quantity :player-eye-height :unit :cell)
     :type double-float :minimum 0.2 :maximum 3.0 :step 0.05)
    (player-eye-height (luvcraft-session-player session)))

(define-knob critter-count
    (:group :critters :label "animals about"
     :quantity (:quantity :critter-count :unit :one)
     :minimum 0 :maximum 12 :step 1)
    (critter-population-target-count (luvcraft-session-critters session)))
