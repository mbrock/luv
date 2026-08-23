;;; Live application artifacts and shader pipelines.
;;;
;;; A live artifact is an application-owned derivative of redefinable source.
;;; The protocol deliberately says nothing about the artifact's internal
;;; grain: a Luvcraft render pipeline and a LUFT renderer cohort can both be
;;; one artifact.  Each implementation builds its candidate away from the
;;; installed state, publishes only a complete replacement at its owning
;;; application boundary, and keeps the last-known-good value on failure.

(in-package #:luv)

;;; Application-level protocol ------------------------------------------------

(defgeneric application-live-artifacts (application)
  (:documentation
   "Return APPLICATION's live artifacts as a fresh list.

The list is an enumeration, not a transfer of ownership.  Applications with
no live artifacts inherit the empty default.  An artifact may represent one
GPU pipeline, a complete renderer cohort, or another transactionally replaced
application derivative; the protocol does not impose a resource grain."))

(defmethod application-live-artifacts ((application t))
  (declare (ignore application))
  nil)

(defgeneric live-artifact-label (artifact)
  (:documentation "Return a concise human-readable label for ARTIFACT."))

(defgeneric live-artifact-status (artifact)
  (:documentation
   "Return ARTIFACT's lifecycle status.

Concrete artifacts normally use :BUILDING, :INSTALLED, :FAILED, :RELEASING,
and :RELEASED.  :FAILED means that the last attempt failed while the previous
installed artifact, when any, remains usable."))

(defgeneric live-artifact-diagnostic (artifact)
  (:documentation
   "Return ARTIFACT's newest build or retirement diagnostic, or NIL."))

(defgeneric live-artifact-installed-revision (artifact)
  (:documentation
   "Return ARTIFACT's monotonically increasing installed revision."))

(defgeneric refresh-live-artifact (artifact)
  (:documentation
   "Attempt pending source for ARTIFACT at its owning application boundary.

Implementations must build away from installed state, publish only a complete
candidate, preserve the last-known-good artifact on failure, and coalesce
newer invalidations without performing GPU work in definition callbacks.
Explicitly released artifacts must not be resurrected."))

(defgeneric release-live-artifact (artifact)
  (:documentation
   "Explicitly and idempotently release ARTIFACT and its subscriptions.

Release is terminal.  Application owners remain responsible for calling this
at a boundary where no new encoding can borrow the installed GPU resources."))

(defun refresh-application-live-artifacts (application)
  "Refresh the current live-artifact enumeration of APPLICATION."
  (dolist (artifact (application-live-artifacts application))
    (refresh-live-artifact artifact))
  application)

;;; Mathematical shader pipelines -------------------------------------------

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
   (vertex-module-owned-p :initform nil
                          :accessor live-shader-pipeline-vertex-module-owned-p)
   (vertex-buffers :initarg :vertex-buffers
                   :reader live-shader-pipeline-vertex-buffers)
   (target-format :initarg :target-format
                  :reader live-shader-pipeline-target-format)
   (target-blend :initarg :target-blend :initform nil
                 :reader live-shader-pipeline-target-blend)
   (primitive :initarg :primitive :reader live-shader-pipeline-primitive)
   (depth-stencil :initarg :depth-stencil
                  :reader live-shader-pipeline-depth-stencil)
   (dependent :initarg :dependent :accessor live-shader-pipeline-dependent)
   (vertex-dependent :initarg :vertex-dependent :initform nil
                     :accessor live-shader-pipeline-vertex-dependent)
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
   (installed-source-revision :initform 0
     :accessor live-shader-pipeline-installed-source-revision)
   (attempted-source-revision :initform 0
     :accessor live-shader-pipeline-attempted-source-revision)
   ;; The live named values folded into the installed and most recently
   ;; attempted sources, as (NAME . VALUE).  Failed attempts deliberately
   ;; replace ATTEMPTED-SOURCE-VALUES so a value which repairs the failed
   ;; source requests another attempt without disturbing the installed value.
   (installed-source-values :initform nil
     :accessor live-shader-pipeline-installed-source-values)
   (attempted-source-values :initform nil
     :accessor live-shader-pipeline-attempted-source-values)
   ;; Source notifications never acquire this lock.  It serializes only the
   ;; owner-side build/install/release transition, preventing concurrent
   ;; refresh or teardown from publishing or retiring the same resources twice.
   (mutation-lock
    :initform (sb-thread:make-mutex :name "luv live shader pipeline")
    :reader live-shader-pipeline-mutation-lock))
  (:documentation
   "A last-known-good GPU render pipeline following live shader definitions.

The owning application calls REFRESH-LIVE-ARTIFACT at a frame boundary.
Definition callbacks only advance thread-safe revision sources; compilation,
GPU creation, publication, and retirement happen here, outside MOP locks."))

(defmethod live-artifact-label ((artifact live-shader-pipeline))
  (live-shader-pipeline-label artifact))

(defmethod live-artifact-status ((artifact live-shader-pipeline))
  (live-shader-pipeline-status artifact))

(defmethod live-artifact-diagnostic ((artifact live-shader-pipeline))
  (live-shader-pipeline-diagnostic artifact))

(defmethod live-artifact-installed-revision ((artifact live-shader-pipeline))
  (live-shader-pipeline-installed-revision artifact))

(defstruct (live-shader-pipeline-candidate
            (:constructor %make-live-shader-pipeline-candidate))
  "A complete unpublished shader-pipeline candidate and its owned resources."
  vertex-specification
  vertex-lowering
  vertex-module
  (vertex-module-owned-p nil)
  specification
  lowering
  fragment-module
  pipeline
  source-values)

(defun release-live-shader-pipeline-candidate (candidate)
  "Release CANDIDATE resources which have not transferred to an artifact."
  (dolist (resource
            (remove-duplicates
             (remove nil
                     (list
                      (live-shader-pipeline-candidate-pipeline candidate)
                      (live-shader-pipeline-candidate-fragment-module candidate)
                      (and
                       (live-shader-pipeline-candidate-vertex-module-owned-p
                        candidate)
                       (live-shader-pipeline-candidate-vertex-module
                        candidate))))
             :test #'eq))
    ;; Preserve the primary build condition while still giving each resource
    ;; an independent retirement attempt.  GPU backends retain failed native
    ;; retirements in their durable queue-owned ledgers.
    (ignore-errors (destroy resource)))
  (setf (live-shader-pipeline-candidate-pipeline candidate) nil
        (live-shader-pipeline-candidate-fragment-module candidate) nil
        (live-shader-pipeline-candidate-vertex-module candidate) nil
        (live-shader-pipeline-candidate-vertex-module-owned-p candidate) nil)
  nil)

(defun build-live-shader-pipeline-candidate (artifact)
  "Build and return a complete candidate without replacing ARTIFACT's state."
  (let* ((vertex-only-p
           (eq (live-shader-pipeline-stage artifact) :vertex))
         (source-values (list nil))
         (luv.shader:*shader-source-value-references* source-values)
         (candidate (%make-live-shader-pipeline-candidate))
         (completed-p nil))
    (unwind-protect
         (let* ((vertex-specification
                  (cond
                    ((live-shader-pipeline-vertex-role artifact)
                     (luv.shader:shader-specification-for
                      (live-shader-pipeline-vertex-role artifact) :vertex))
                    (vertex-only-p
                     (luv.shader:shader-specification-for
                      (live-shader-pipeline-role artifact) :vertex))))
                (vertex-lowering
                  (when vertex-specification
                    (luv.spir-v:compile-shader-specification
                     vertex-specification)))
                (specification
                  (unless vertex-only-p
                    (luv.shader:shader-specification-for
                     (live-shader-pipeline-role artifact)
                     (live-shader-pipeline-stage artifact))))
                (lowering
                  (and specification
                       (luv.spir-v:compile-shader-specification specification)))
                (device (live-shader-pipeline-device artifact)))
           (setf
            (live-shader-pipeline-candidate-vertex-specification candidate)
            vertex-specification
            (live-shader-pipeline-candidate-vertex-lowering candidate)
            vertex-lowering
            (live-shader-pipeline-candidate-specification candidate)
            specification
            (live-shader-pipeline-candidate-lowering candidate) lowering
            (live-shader-pipeline-candidate-vertex-module candidate)
            (if vertex-lowering
                (create
                 device
                 (make-shader-module-descriptor
                  :label (format nil "~A vertex module"
                                 (live-shader-pipeline-label artifact))
                  :language :mathematical
                  :code vertex-specification))
                (live-shader-pipeline-vertex-module artifact))
            (live-shader-pipeline-candidate-vertex-module-owned-p candidate)
            (not (null vertex-lowering)))
           (when specification
             (setf
              (live-shader-pipeline-candidate-fragment-module candidate)
              (create
               device
               (make-shader-module-descriptor
                :label (format nil "~A fragment module"
                               (live-shader-pipeline-label artifact))
                :language :mathematical
                :code specification))))
           (setf
            (live-shader-pipeline-candidate-pipeline candidate)
            (create
             device
             (make-render-pipeline-descriptor
              :label (live-shader-pipeline-label artifact)
              :layout (live-shader-pipeline-layout artifact)
              :vertex
              `(:module
                ,(live-shader-pipeline-candidate-vertex-module candidate)
                :buffers ,(live-shader-pipeline-vertex-buffers artifact))
              :fragment
              (let ((module
                      (live-shader-pipeline-candidate-fragment-module
                       candidate)))
                (when module
                  `(:module ,module
                    :targets
                    ((:format ,(live-shader-pipeline-target-format artifact)
                      :blend ,(live-shader-pipeline-target-blend artifact))))))
              :primitive (live-shader-pipeline-primitive artifact)
              :depth-stencil (live-shader-pipeline-depth-stencil artifact)))
            (live-shader-pipeline-candidate-source-values candidate)
            (copy-list (car source-values))
            completed-p t)
           candidate)
      ;; Record dependencies even when parsing, lowering, or GPU creation
      ;; fails.  A live value which changes after the failed attempt can then
      ;; request a retry without waiting for an unrelated source edit.
      (setf (live-shader-pipeline-attempted-source-values artifact)
            (copy-list (car source-values)))
      (unless completed-p
        (release-live-shader-pipeline-candidate candidate)))))

(defun install-live-shader-pipeline-candidate
    (artifact revision source-revision candidate)
  "Publish CANDIDATE into ARTIFACT and retire the previous complete artifact."
  (let* ((old-pipeline (live-shader-pipeline-native-pipeline artifact))
         (old-vertex-module
           (and (live-shader-pipeline-vertex-module-owned-p artifact)
                (live-shader-pipeline-vertex-module artifact)))
         (old-fragment-module
           (live-shader-pipeline-fragment-module artifact))
         (new-pipeline
           (live-shader-pipeline-candidate-pipeline candidate))
         (new-vertex-module
           (live-shader-pipeline-candidate-vertex-module candidate))
         (new-fragment-module
           (live-shader-pipeline-candidate-fragment-module candidate))
         (new-owned-resources
           (remove nil
                   (list new-pipeline new-fragment-module
                         (and
                          (live-shader-pipeline-candidate-vertex-module-owned-p
                           candidate)
                          new-vertex-module))))
         (retirement-errors nil))
    ;; Publish the complete replacement as one owner-side state transition.
    ;; The application calls at a frame boundary, so command encoding before
    ;; this transition observes the old cohort and encoding after it the new.
    (setf (live-shader-pipeline-vertex-specification artifact)
          (live-shader-pipeline-candidate-vertex-specification candidate)
          (live-shader-pipeline-vertex-lowering artifact)
          (live-shader-pipeline-candidate-vertex-lowering candidate)
          (live-shader-pipeline-vertex-module artifact) new-vertex-module
          (live-shader-pipeline-vertex-module-owned-p artifact)
          (live-shader-pipeline-candidate-vertex-module-owned-p candidate)
          (live-shader-pipeline-specification artifact)
          (live-shader-pipeline-candidate-specification candidate)
          (live-shader-pipeline-lowering artifact)
          (live-shader-pipeline-candidate-lowering candidate)
          (live-shader-pipeline-fragment-module artifact) new-fragment-module
          (live-shader-pipeline-native-pipeline artifact) new-pipeline
          (live-shader-pipeline-status artifact) :installed
          (live-shader-pipeline-diagnostic artifact) nil
          (live-shader-pipeline-installed-revision artifact) revision
          (live-shader-pipeline-installed-source-revision artifact)
          source-revision
          (live-shader-pipeline-attempted-source-revision artifact)
          source-revision
          (live-shader-pipeline-installed-source-values artifact)
          (copy-list
           (live-shader-pipeline-candidate-source-values candidate)))
    ;; Ownership has crossed to ARTIFACT.  Detach before retirement so cleanup
    ;; after any future change to this function cannot destroy installed state.
    (setf (live-shader-pipeline-candidate-pipeline candidate) nil
          (live-shader-pipeline-candidate-fragment-module candidate) nil
          (live-shader-pipeline-candidate-vertex-module candidate) nil
          (live-shader-pipeline-candidate-vertex-module-owned-p candidate) nil)
    ;; Backends retire resources across their actual submission-completion
    ;; frontier.  Do not retire an object a backend deliberately reused for the
    ;; new cohort, and try every distinct old owner even if one retirement errs.
    (dolist (resource
              (remove-duplicates
               (remove-if
                (lambda (resource)
                  (or (null resource)
                      (member resource new-owned-resources :test #'eq)))
                (list old-pipeline old-vertex-module old-fragment-module))
               :test #'eq))
      (handler-case (destroy resource)
        (error (condition)
          (push condition retirement-errors))))
    (when retirement-errors
      ;; The replacement remains installed.  Preserve the exceptional native
      ;; retirement as diagnostic state; backend ledgers retain retry custody.
      (setf (live-shader-pipeline-diagnostic artifact)
            (first retirement-errors))))
  artifact)

(defun make-live-shader-pipeline
    (&key role (stage :fragment) vertex-role label device layout vertex-module
          vertex-buffers target-format target-blend primitive depth-stencil)
  "Create and install a live mathematical render pipeline.

VERTEX-MODULE is borrowed when VERTEX-ROLE is NIL and STAGE is not :VERTEX.
Every module compiled from ROLE or VERTEX-ROLE is owned by the returned
artifact and retired on replacement or explicit release."
  (let ((generic-function
          (fdefinition 'luv.shader:shader-specification-for))
        (dependent nil)
        (vertex-dependent nil)
        (artifact nil)
        (completed-p nil))
    ;; Begin the cleanup boundary before subscribing: failed initialization
    ;; must never leave an invisible dependent attached to the shader generic.
    (unwind-protect
         (let ((source-revision (luv.shader:shader-source-revision)))
           (setf dependent
                 (luv.shader:make-shader-definition-dependent
                  generic-function (list role stage))
                 vertex-dependent
                 (when vertex-role
                   (luv.shader:make-shader-definition-dependent
                    generic-function (list vertex-role :vertex)))
                 artifact
                 (make-instance
                  'live-shader-pipeline
                  :role role :stage stage :label label :device device
                  :vertex-role vertex-role
                  :vertex-dependent vertex-dependent
                  :layout layout :vertex-module vertex-module
                  :vertex-buffers vertex-buffers
                  :target-format target-format :target-blend target-blend
                  :primitive primitive :depth-stencil depth-stencil
                  :dependent dependent))
           (let ((candidate
                   (build-live-shader-pipeline-candidate artifact)))
             (unwind-protect
                  (install-live-shader-pipeline-candidate
                   artifact 0 source-revision candidate)
               (release-live-shader-pipeline-candidate candidate)))
           (setf completed-p t)
           artifact)
      (unless completed-p
        (when dependent
          (luv.shader:release-shader-definition-dependent dependent))
        (when vertex-dependent
          (luv.shader:release-shader-definition-dependent
           vertex-dependent))))))

(defun live-shader-pipeline-refresh-p (artifact source-revision)
  (or (luv.shader:shader-definition-change-pending-p
       (live-shader-pipeline-dependent artifact))
      (let ((dependent (live-shader-pipeline-vertex-dependent artifact)))
        (and dependent
             (luv.shader:shader-definition-change-pending-p dependent)))
      (> source-revision
         (live-shader-pipeline-attempted-source-revision artifact))
      (not
       (luv.shader:shader-source-value-references-current-p
        (live-shader-pipeline-attempted-source-values artifact)))))

(defun %refresh-live-shader-pipeline (artifact)
  "Owner-side implementation; caller holds ARTIFACT's mutation lock."
  (unless (member (live-shader-pipeline-status artifact)
                  '(:releasing :released))
    (let ((source-revision (luv.shader:shader-source-revision)))
      (when (live-shader-pipeline-refresh-p artifact source-revision)
        (multiple-value-bind (revision event)
            (luv.shader:shader-definition-change-snapshot
             (live-shader-pipeline-dependent artifact))
          (declare (ignore event))
          (multiple-value-bind (vertex-revision vertex-event)
              (let ((dependent
                      (live-shader-pipeline-vertex-dependent artifact)))
                (if dependent
                    (luv.shader:shader-definition-change-snapshot dependent)
                    (values 0 nil)))
            (declare (ignore vertex-event))
            (setf (live-shader-pipeline-status artifact) :building)
            (handler-case
                (let ((candidate
                        (build-live-shader-pipeline-candidate artifact)))
                  (unwind-protect
                       (install-live-shader-pipeline-candidate
                        artifact
                        (1+
                         (live-shader-pipeline-installed-revision artifact))
                        source-revision candidate)
                    (release-live-shader-pipeline-candidate candidate)))
              (error (condition)
                ;; A failed edit is diagnostic state, not a rendering outage.
                (setf (live-shader-pipeline-status artifact) :failed
                      (live-shader-pipeline-diagnostic artifact) condition
                      (live-shader-pipeline-attempted-source-revision artifact)
                      source-revision)))
            ;; A definition which races this attempt remains pending because
            ;; acknowledgement is capped at the snapshots taken above.
            (luv.shader:acknowledge-shader-definition-change
             (live-shader-pipeline-dependent artifact) revision)
            (let ((dependent
                    (live-shader-pipeline-vertex-dependent artifact)))
              (when dependent
                (luv.shader:acknowledge-shader-definition-change
                 dependent vertex-revision))))))))
  artifact)

(defmethod refresh-live-artifact ((artifact live-shader-pipeline))
  (sb-thread:with-mutex
      ((live-shader-pipeline-mutation-lock artifact))
    (%refresh-live-shader-pipeline artifact)))

(defun refresh-live-shader-pipeline (artifact)
  "Compatibility spelling for REFRESH-LIVE-ARTIFACT."
  (refresh-live-artifact artifact))

(defun %release-live-shader-pipeline (artifact)
  "Owner-side implementation; caller holds ARTIFACT's mutation lock."
  (unless (eq (live-shader-pipeline-status artifact) :released)
    (let* ((dependent (live-shader-pipeline-dependent artifact))
           (vertex-dependent
             (live-shader-pipeline-vertex-dependent artifact))
           (pipeline (live-shader-pipeline-native-pipeline artifact))
           (fragment-module
             (live-shader-pipeline-fragment-module artifact))
           (vertex-module
             (and (live-shader-pipeline-vertex-module-owned-p artifact)
                  (live-shader-pipeline-vertex-module artifact)))
           (resources
             (remove-duplicates
              (remove nil (list pipeline fragment-module vertex-module))
              :test #'eq)))
      ;; Relinquish every application-visible ownership claim before native
      ;; retirement.  A backend keeps custody when destruction reports an
      ;; error; retaining the wrappers here would permit resurrection or a
      ;; second retirement attempt by a later RELEASE call.
      (setf (live-shader-pipeline-status artifact) :releasing
            (live-shader-pipeline-dependent artifact) nil
            (live-shader-pipeline-vertex-dependent artifact) nil
            (live-shader-pipeline-native-pipeline artifact) nil
            (live-shader-pipeline-fragment-module artifact) nil
            (live-shader-pipeline-vertex-module artifact) nil
            (live-shader-pipeline-vertex-module-owned-p artifact) nil)
      (unwind-protect
           (handler-case
               (with-release-report
                 (releasing :shader-definition-dependent
                   (when dependent
                     (luv.shader:release-shader-definition-dependent
                      dependent)))
                 (releasing :vertex-shader-definition-dependent
                   (when vertex-dependent
                     (luv.shader:release-shader-definition-dependent
                      vertex-dependent)))
                 (loop for resource in resources
                       for index from 0
                       do (releasing (list :live-shader-resource index)
                            (destroy resource))))
             (error (condition)
               (setf (live-shader-pipeline-diagnostic artifact) condition)
               (error condition)))
        ;; RELEASED is terminal even when reporting retirement failure.
        (setf (live-shader-pipeline-status artifact) :released))))
  nil)

(defmethod release-live-artifact ((artifact live-shader-pipeline))
  (sb-thread:with-mutex
      ((live-shader-pipeline-mutation-lock artifact))
    (%release-live-shader-pipeline artifact)))

(defun release-live-shader-pipeline (artifact)
  "Compatibility spelling for RELEASE-LIVE-ARTIFACT."
  (release-live-artifact artifact))
