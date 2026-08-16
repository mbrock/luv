;;; Live GPU pipelines that follow redefinable shader methods.
;;;
;;; A live shader definition and its installed GPU derivative are deliberately
;;; different objects.  DEFMETHOD owns source identity.  This artifact owns the
;;; last pipeline that successfully crossed parsing, lowering, assembly, and
;;; Vulkan creation, plus a MOP dependent which merely announces newer source.
;;; A broken edit is retained as diagnostic state while the last good pipeline
;;; continues rendering.

(in-package #:luvcraft)

(defclass live-shader-pipeline ()
  ((role :initarg :role :reader live-shader-pipeline-role)
   (stage :initarg :stage :reader live-shader-pipeline-stage)
   (vertex-role :initarg :vertex-role :initform nil
                :reader live-shader-pipeline-vertex-role)
   (label :initarg :label :reader live-shader-pipeline-label)
   (device :initarg :device :reader live-shader-pipeline-device)
   (layout :initarg :layout :reader live-shader-pipeline-layout)
   (vertex-module :initarg :vertex-module :initform nil
                  :accessor live-shader-pipeline-vertex-module)
   (vertex-buffers :initarg :vertex-buffers
                   :reader live-shader-pipeline-vertex-buffers)
   (target-format :initarg :target-format
                  :reader live-shader-pipeline-target-format)
   (target-blend :initarg :target-blend :initform nil
                 :reader live-shader-pipeline-target-blend)
   (primitive :initarg :primitive :reader live-shader-pipeline-primitive)
   (depth-stencil :initarg :depth-stencil
                  :reader live-shader-pipeline-depth-stencil)
   (dependent :initarg :dependent :reader live-shader-pipeline-dependent)
   (vertex-dependent :initarg :vertex-dependent :initform nil
                     :reader live-shader-pipeline-vertex-dependent)
   (vertex-specification :initform nil
                         :accessor live-shader-pipeline-vertex-specification)
   (vertex-lowering :initform nil
                    :accessor live-shader-pipeline-vertex-lowering)
   (specification :initform nil
                  :accessor live-shader-pipeline-specification)
   (lowering :initform nil :accessor live-shader-pipeline-lowering)
   (fragment-module :initform nil
                    :accessor live-shader-pipeline-fragment-module)
   (pipeline :initform nil :accessor live-shader-pipeline-native-pipeline)
   (status :initform :building :accessor live-shader-pipeline-status)
   (diagnostic :initform nil :accessor live-shader-pipeline-diagnostic)
   (installed-revision :initform 0
                       :accessor live-shader-pipeline-installed-revision)
   (installed-abstraction-revision :initform 0
     :accessor live-shader-pipeline-installed-abstraction-revision)
   (attempted-abstraction-revision :initform 0
     :accessor live-shader-pipeline-attempted-abstraction-revision)))

(defun build-live-shader-pipeline-candidate (artifact)
  "Build a complete candidate without mutating ARTIFACT's installed state."
  (let* ((vertex-only-p
           (eq (live-shader-pipeline-stage artifact) :vertex))
         (vertex-specification
           (cond
             ((live-shader-pipeline-vertex-role artifact)
              (spv:shader-specification-for
               (live-shader-pipeline-vertex-role artifact) :vertex))
             (vertex-only-p
              (spv:shader-specification-for
               (live-shader-pipeline-role artifact) :vertex))))
         (vertex-lowering
           (when vertex-specification
             (spv:compile-shader-specification vertex-specification)))
         (specification
           (unless vertex-only-p
             (spv:shader-specification-for
              (live-shader-pipeline-role artifact)
              (live-shader-pipeline-stage artifact))))
         (lowering
           (and specification
                (spv:compile-shader-specification specification)))
         (device (live-shader-pipeline-device artifact))
         (vertex-module nil)
         (fragment-module nil)
         (pipeline nil)
         (completed-p nil))
    (unwind-protect
         (progn
           (setf vertex-module
                 (if vertex-lowering
                     (create
                      device
                       (make-shader-module-descriptor
                       :label (format nil "~A vertex module"
                                       (live-shader-pipeline-label artifact))
                       :language :mathematical
                       :code vertex-specification))
                     (live-shader-pipeline-vertex-module artifact))
                 fragment-module
                 (and specification
                      (create
                       device
                       (make-shader-module-descriptor
                        :label (format nil "~A fragment module"
                                       (live-shader-pipeline-label artifact))
                        :language :mathematical
                        :code specification)))
                 pipeline
                 (create
                  device
                  (make-render-pipeline-descriptor
                   :label (live-shader-pipeline-label artifact)
                   :layout (live-shader-pipeline-layout artifact)
                   :vertex
                   `(:module ,vertex-module
                     :buffers ,(live-shader-pipeline-vertex-buffers artifact))
                   :fragment
                   (and fragment-module
                        `(:module ,fragment-module
                          :targets
                          ((:format
                            ,(live-shader-pipeline-target-format artifact)
                           :blend
                            ,(live-shader-pipeline-target-blend artifact)))))
                   :primitive (live-shader-pipeline-primitive artifact)
                   :depth-stencil
                   (live-shader-pipeline-depth-stencil artifact)))
                 completed-p t)
           (values vertex-specification vertex-lowering vertex-module
                   specification lowering fragment-module pipeline))
      (unless completed-p
        (when pipeline (ignore-errors (destroy pipeline)))
        (when fragment-module (ignore-errors (destroy fragment-module)))
        (when (and vertex-lowering vertex-module)
          (ignore-errors (destroy vertex-module)))))))

(defun install-live-shader-pipeline-candidate
    (artifact revision abstraction-revision vertex-specification
     vertex-lowering vertex-module specification lowering fragment-module
     pipeline)
  (let ((old-pipeline (live-shader-pipeline-native-pipeline artifact))
        (old-vertex-module
          (and (or (live-shader-pipeline-vertex-role artifact)
                   (eq (live-shader-pipeline-stage artifact) :vertex))
               (live-shader-pipeline-vertex-module artifact)))
        (old-fragment-module
          (live-shader-pipeline-fragment-module artifact))
        (retirement-errors nil))
    ;; Publish the complete replacement as one Lisp-side state transition.
    ;; Command encoding after this point can observe only the new pipeline.
    (setf (live-shader-pipeline-vertex-specification artifact)
          vertex-specification
          (live-shader-pipeline-vertex-lowering artifact) vertex-lowering
          (live-shader-pipeline-vertex-module artifact) vertex-module
          (live-shader-pipeline-specification artifact) specification
          (live-shader-pipeline-lowering artifact) lowering
          (live-shader-pipeline-fragment-module artifact) fragment-module
          (live-shader-pipeline-native-pipeline artifact) pipeline
          (live-shader-pipeline-status artifact) :installed
          (live-shader-pipeline-diagnostic artifact) nil
          (live-shader-pipeline-installed-revision artifact) revision
          (live-shader-pipeline-installed-abstraction-revision artifact)
          abstraction-revision
          (live-shader-pipeline-attempted-abstraction-revision artifact)
          abstraction-revision)
    ;; Vulkan resource destruction is submission-aware.  Encoders retain the
    ;; old pipeline, and its native handles cross the completion frontier before
    ;; the backend actually destroys them.
    (dolist (resource (list old-pipeline old-vertex-module
                            old-fragment-module))
      (when resource
        (handler-case (destroy resource)
          (error (condition)
            ;; The replacement is already installed.  Preserve that fact while
            ;; retaining the exceptional retirement as useful diagnostic state.
            (push condition retirement-errors)))))
    (when retirement-errors
      (setf (live-shader-pipeline-diagnostic artifact)
            (first retirement-errors))))
  artifact)

(defun make-live-shader-pipeline
    (&key role (stage :fragment) vertex-role label device layout vertex-module
          vertex-buffers target-format target-blend primitive depth-stencil)
  (let* ((generic-function (fdefinition 'spv:shader-specification-for))
         (abstraction-revision (spv:shader-abstraction-revision))
         (dependent
           (spv:make-shader-definition-dependent
            generic-function (list role stage)))
         (vertex-dependent
           (when vertex-role
             (spv:make-shader-definition-dependent
              generic-function (list vertex-role :vertex))))
         (artifact
           (make-instance
            'live-shader-pipeline
            :role role :stage stage :label label :device device
            :vertex-role vertex-role :vertex-dependent vertex-dependent
            :layout layout :vertex-module vertex-module
            :vertex-buffers vertex-buffers :target-format target-format
            :target-blend target-blend
            :primitive primitive :depth-stencil depth-stencil
            :dependent dependent))
         (completed-p nil))
    (unwind-protect
           (multiple-value-bind
               (vertex-specification vertex-lowering candidate-vertex-module
                specification lowering fragment-module pipeline)
             (build-live-shader-pipeline-candidate artifact)
           (install-live-shader-pipeline-candidate
            artifact 0 abstraction-revision
            vertex-specification vertex-lowering
            candidate-vertex-module specification lowering
            fragment-module pipeline)
           (setf completed-p t)
           artifact)
      (unless completed-p
        (spv:release-shader-definition-dependent dependent)
        (when vertex-dependent
          (spv:release-shader-definition-dependent vertex-dependent))))))

(defun refresh-live-shader-pipeline (artifact)
  "Attempt the newest pending definition, retaining the last good pipeline."
  (let ((dependent (live-shader-pipeline-dependent artifact))
        (vertex-dependent (live-shader-pipeline-vertex-dependent artifact))
        (abstraction-revision (spv:shader-abstraction-revision)))
    (when (or (spv:shader-definition-change-pending-p dependent)
              (and vertex-dependent
                   (spv:shader-definition-change-pending-p vertex-dependent))
              (> abstraction-revision
                 (live-shader-pipeline-attempted-abstraction-revision
                  artifact)))
      (multiple-value-bind (revision event)
          (spv:shader-definition-change-snapshot dependent)
        (declare (ignore event))
        (multiple-value-bind (vertex-revision vertex-event)
            (if vertex-dependent
                (spv:shader-definition-change-snapshot vertex-dependent)
                (values 0 nil))
          (declare (ignore vertex-event))
          (setf (live-shader-pipeline-status artifact) :building)
          (handler-case
              (multiple-value-bind
                    (vertex-specification vertex-lowering vertex-module
                     specification lowering fragment-module pipeline)
                  (build-live-shader-pipeline-candidate artifact)
                (install-live-shader-pipeline-candidate
                 artifact (1+ (live-shader-pipeline-installed-revision artifact))
                 abstraction-revision
                 vertex-specification vertex-lowering vertex-module
                 specification lowering fragment-module pipeline))
            (error (condition)
              ;; A failed edit is diagnostic state, not a rendering outage.
              (setf (live-shader-pipeline-status artifact) :failed
                    (live-shader-pipeline-diagnostic artifact) condition
                    (live-shader-pipeline-attempted-abstraction-revision
                     artifact)
                    abstraction-revision)))
            (spv:acknowledge-shader-definition-change dependent revision)
            (when vertex-dependent
              (spv:acknowledge-shader-definition-change
               vertex-dependent vertex-revision))))))
  artifact)

(defun release-live-shader-pipeline (artifact)
  (spv:release-shader-definition-dependent
   (live-shader-pipeline-dependent artifact))
  (let ((dependent (live-shader-pipeline-vertex-dependent artifact)))
    (when dependent
      (spv:release-shader-definition-dependent dependent)))
  (let ((pipeline (live-shader-pipeline-native-pipeline artifact)))
    (when pipeline
      (destroy pipeline)
      (setf (live-shader-pipeline-native-pipeline artifact) nil)))
  (let ((module (live-shader-pipeline-fragment-module artifact)))
    (when module
      (destroy module)
      (setf (live-shader-pipeline-fragment-module artifact) nil)))
  (when (or (live-shader-pipeline-vertex-role artifact)
            (eq (live-shader-pipeline-stage artifact) :vertex))
    (let ((module (live-shader-pipeline-vertex-module artifact)))
      (when module
        (destroy module)
        (setf (live-shader-pipeline-vertex-module artifact) nil))))
  (setf (live-shader-pipeline-status artifact) :released)
  nil)
