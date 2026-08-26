;;; The GPU-owned half of a running luvcraft session.

(in-package #:luvcraft)

(defgeneric release-luvcraft-component (component)
  (:documentation
   "Release every resource owned by COMPONENT.

Component release is an owner boundary: callers name the component once,
rather than reproducing its private resource inventory."))

(defstruct (luvcraft-frame-extents
            (:constructor make-luvcraft-frame-extents
                (render presentation)))
  "The two resolution domains installed together at a frame boundary."
  render
  presentation)

(defgeneric resize-luvcraft-component (component extents)
  (:documentation
   "Replace COMPONENT's frame-sized state at a frame boundary.

EXTENTS names both the expensive game-scene resolution and the final
drawable-matched resolution where native-density application UI is composed."))

(defclass luvcraft-renderer ()
  ((device :initarg :device :reader luvcraft-renderer-device)
   (context :initarg :context :reader luvcraft-renderer-context)
   (atlas-texture :initarg :atlas-texture
                  :accessor luvcraft-renderer-atlas-texture)
   (atlas-view :initarg :atlas-view :accessor luvcraft-renderer-atlas-view)
   (atlas-sampler :initarg :atlas-sampler
                  :reader luvcraft-renderer-atlas-sampler)
   (normal-atlas-texture :initarg :normal-atlas-texture
                         :accessor luvcraft-renderer-normal-atlas-texture)
   (normal-atlas-view :initarg :normal-atlas-view
                      :accessor luvcraft-renderer-normal-atlas-view)
   ;; One slot is the publication point for the complete extent-sized cohort.
   ;; Readers can therefore never observe a new colour image with an old depth
   ;; or presentation image while a live resize is being installed.
   (frame-attachments :initarg :frame-attachments :initform nil
                      :accessor luvcraft-renderer-frame-attachments)
   (shadow-depth-texture :initarg :shadow-depth-texture
                         :reader luvcraft-renderer-shadow-depth-texture)
   (shadow-depth-view :initarg :shadow-depth-view
                      :reader luvcraft-renderer-shadow-depth-view)
   (shadow-depth-sampler :initarg :shadow-depth-sampler
                         :reader luvcraft-renderer-shadow-depth-sampler)
   (shadow-comparison-sampler
    :initarg :shadow-comparison-sampler
    :reader luvcraft-renderer-shadow-comparison-sampler)
   (layout :initarg :layout :reader luvcraft-renderer-layout)
   (shadow-layout :initarg :shadow-layout
                  :reader luvcraft-renderer-shadow-layout)
   (post-layout :initarg :post-layout :reader luvcraft-renderer-post-layout)
   (bloom-layout :initarg :bloom-layout :initform nil
                 :reader luvcraft-renderer-bloom-layout)
   (linear-sampler :initarg :linear-sampler :initform nil
                   :reader luvcraft-renderer-linear-sampler)
   (block-pipeline :initarg :block-pipeline
                   :reader luvcraft-renderer-block-pipeline)
   (shadow-pipeline :initarg :shadow-pipeline
                    :reader luvcraft-renderer-shadow-pipeline)
   (sky-vertex-buffer :initarg :sky-vertex-buffer :initform nil
                      :reader luvcraft-renderer-sky-vertex-buffer)
   (sky-pipeline :initarg :sky-pipeline :initform nil
                 :reader luvcraft-renderer-sky-pipeline)
   (crosshair-vertex-buffer
    :initarg :crosshair-vertex-buffer
    :reader luvcraft-renderer-crosshair-vertex-buffer)
   (cursor-vertex-buffer :initarg :cursor-vertex-buffer :initform nil
                         :reader luvcraft-renderer-cursor-vertex-buffer)
   (crosshair-pipeline :initarg :crosshair-pipeline
                       :reader luvcraft-renderer-crosshair-pipeline)
   (cursor-pipeline :initarg :cursor-pipeline
                    :reader luvcraft-renderer-cursor-pipeline)
   (post-pipeline :initarg :post-pipeline
                  :reader luvcraft-renderer-post-pipeline)
   (bloom-bright-pipeline :initarg :bloom-bright-pipeline :initform nil
                          :reader luvcraft-renderer-bloom-bright-pipeline)
   (bloom-horizontal-pipeline :initarg :bloom-horizontal-pipeline :initform nil
                              :reader luvcraft-renderer-bloom-horizontal-pipeline)
   (bloom-vertical-pipeline :initarg :bloom-vertical-pipeline :initform nil
                            :reader luvcraft-renderer-bloom-vertical-pipeline)
   (sun-shaft-pipeline :initarg :sun-shaft-pipeline :initform nil
                       :reader luvcraft-renderer-sun-shaft-pipeline)
   ;; The cache and both inventories are private implementation details of
   ;; this owner.  A construction-time rollback list may precede it, but a
   ;; completed session has no second resource ledger.  See #V9VH79.
   (frame-states :initarg :frame-states
                 :initform (make-hash-table :test #'eql)
                 :reader luvcraft-renderer-frame-states)
   (frame-resource-cache :initform nil
                         :accessor luvcraft-renderer-frame-resource-cache)
   (resources :initarg :resources :initform nil
              :accessor luvcraft-renderer-resources))
  (:documentation
   "The sole owner of a session's frame attachments, layouts, pipelines, and
per-drawable GPU state.  The session coordinates this owner with simulation,
streaming, overlays, and presentation; it does not duplicate its inventory."))

(defmethod initialize-instance :after ((renderer luvcraft-renderer) &key)
  ;; Keep FRAME-STATES as an inspectable compatibility view while the Luv
  ;; cache owns its creation/eviction protocol.
  (setf (luvcraft-renderer-frame-resource-cache renderer)
        (make-canvas-frame-resource-cache
         :entries (luvcraft-renderer-frame-states renderer))))

(defmacro define-luvcraft-frame-attachment-reader (name key)
  `(defmethod ,name ((renderer luvcraft-renderer))
     (getf (luvcraft-renderer-frame-attachments renderer) ,key)))

(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-render-extent :render-extent)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-presentation-extent :presentation-extent)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-color-texture :color-texture)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-color-view :color-view)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-depth-texture :depth-texture)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-depth-view :depth-view)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-world-panel-color-texture :world-panel-color-texture)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-world-panel-color-view :world-panel-color-view)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-world-panel-depth-texture :world-panel-depth-texture)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-world-panel-depth-view :world-panel-depth-view)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-presentation-texture :presentation-texture)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-presentation-view :presentation-view)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-bloom-primary-texture :bloom-primary-texture)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-bloom-primary-view :bloom-primary-view)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-bloom-secondary-texture :bloom-secondary-texture)
(define-luvcraft-frame-attachment-reader
    luvcraft-renderer-bloom-secondary-view :bloom-secondary-view)

(defparameter +luvcraft-renderer-pipeline-slots+
  '(block-pipeline shadow-pipeline sky-pipeline crosshair-pipeline
    cursor-pipeline post-pipeline bloom-bright-pipeline
    bloom-horizontal-pipeline bloom-vertical-pipeline sun-shaft-pipeline)
  "The named live-pipeline slots whose values form the renderer inventory.")

(defun luvcraft-renderer-pipelines (renderer)
  "Return RENDERER's live pipelines from their one authoritative slot set."
  (loop for slot in +luvcraft-renderer-pipeline-slots+
        for pipeline = (and (slot-boundp renderer slot)
                            (slot-value renderer slot))
        when pipeline collect pipeline))
