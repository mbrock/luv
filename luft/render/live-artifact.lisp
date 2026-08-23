;;; LUFT's renderer is one live artifact cohort.
;;;
;;; Its pipelines share layouts, targets, and meshes, so replacing individual
;;; stages would publish an ABI mixture.  Build a complete renderer beside the
;;; installed one, copy its semantic meshes, and cross the viewer boundary only
;;; after every shader module and pipeline exists.

(in-package #:luft.render)

(define-condition renderer-source-changed-during-build (error)
  ((before :initarg :before :reader renderer-build-source-before)
   (after :initarg :after :reader renderer-build-source-after))
  (:report
   (lambda (condition stream)
     (format stream
             "LUFT shader source changed while renderer revision ~D was ~
              building (now ~D); the unpublished cohort was discarded."
             (renderer-build-source-before condition)
             (renderer-build-source-after condition)))))

(defstruct (renderer-live-candidate
            (:constructor make-renderer-live-candidate (renderer)))
  renderer)

(defclass renderer-live-artifact ()
  ((viewer :initarg :viewer :accessor renderer-live-artifact-viewer)
   (status :initarg :status :initform :installed
           :accessor renderer-live-artifact-status)
   (diagnostic :initform nil :accessor renderer-live-artifact-diagnostic)
   (installed-revision :initform 0
                       :accessor renderer-live-artifact-installed-revision)
   (installed-source-revision :initarg :source-revision
                              :accessor renderer-live-installed-source-revision)
   (attempted-source-revision :initarg :source-revision
                              :accessor renderer-live-attempted-source-revision)
   (installed-source-values :initarg :source-values :initform nil
                            :accessor renderer-live-installed-source-values)
   (attempted-source-values :initarg :source-values :initform nil
                            :accessor renderer-live-attempted-source-values)
   (force-p :initform nil :accessor renderer-live-artifact-force-p)
   (lock :initform (sb-thread:make-mutex
                    :name "LUFT live renderer cohort")
         :reader renderer-live-artifact-lock))
  (:documentation
   "The complete last-known-good renderer cohort owned by one LUFT viewer."))

(defvar *viewer-live-artifacts*
  (make-hash-table :test #'eq :weakness :key))

(defvar *viewer-live-artifacts-lock*
  (sb-thread:make-mutex :name "LUFT viewer live artifacts"))

(defmethod live-artifact-label ((artifact renderer-live-artifact))
  (declare (ignore artifact))
  "LUFT renderer cohort")

(defmethod live-artifact-status ((artifact renderer-live-artifact))
  (renderer-live-artifact-status artifact))

(defmethod live-artifact-diagnostic ((artifact renderer-live-artifact))
  (renderer-live-artifact-diagnostic artifact))

(defmethod live-artifact-installed-revision
    ((artifact renderer-live-artifact))
  (renderer-live-artifact-installed-revision artifact))

(defgeneric note-renderer-live-diagnostic (viewer diagnostic)
  (:documentation
   "Mirror DIAGNOSTIC into VIEWER's presentation state without owning it."))

(defmethod note-renderer-live-diagnostic ((viewer t) diagnostic)
  (declare (ignore viewer diagnostic))
  nil)

(defmethod note-renderer-live-diagnostic ((viewer viewer) diagnostic)
  (setf (viewer-shader-diagnostic viewer) diagnostic))

(defun report-renderer-live-diagnostic (artifact diagnostic)
  "Best-effort observer update outside ARTIFACT's ownership outcome."
  (alexandria:when-let
      ((viewer (renderer-live-artifact-viewer artifact)))
    ;; A diagnostic surface is secondary to publication.  Its failure must
    ;; never turn an already-installed cohort into an apparent failed build.
    (ignore-errors (note-renderer-live-diagnostic viewer diagnostic)))
  diagnostic)

(defun viewer-live-artifact (viewer)
  (sb-thread:with-mutex (*viewer-live-artifacts-lock*)
    (gethash viewer *viewer-live-artifacts*)))

(defmethod application-live-artifacts ((viewer viewer))
  (alexandria:when-let ((artifact (viewer-live-artifact viewer)))
    (list artifact)))

(defun call-with-renderer-source-values (function)
  "Call FUNCTION while collecting its folded live shader values.

Return FUNCTION's primary value, the collected references, and source
revisions immediately before and after the call."
  (let ((references (list nil))
        (before (luv.shader:shader-source-revision)))
    (let ((luv.shader:*shader-source-value-references* references))
      (let ((value (funcall function)))
        (values value (copy-list (car references)) before
                (luv.shader:shader-source-revision))))))

(defun make-tracked-renderer (device color-format extent)
  "MAKE-RENDERER with live source values and revision boundaries."
  (call-with-renderer-source-values
   (lambda () (make-renderer device color-format extent))))

(defun attach-viewer-live-artifact
    (viewer source-values source-revision)
  "Attach VIEWER's already-installed renderer as its one live cohort."
  (sb-thread:with-mutex (*viewer-live-artifacts-lock*)
    (or (gethash viewer *viewer-live-artifacts*)
        (setf (gethash viewer *viewer-live-artifacts*)
              (make-instance
               'renderer-live-artifact
               :viewer viewer
               :source-values (copy-list source-values)
               :source-revision source-revision)))))

(defgeneric build-renderer-live-candidate (artifact)
  (:documentation
   "Build ARTIFACT's complete unpublished renderer and semantic mesh cohort."))

(defmethod build-renderer-live-candidate
    ((artifact renderer-live-artifact))
  (let* ((viewer (renderer-live-artifact-viewer artifact))
         (old (viewer-renderer viewer))
         (candidate nil)
         (completed-p nil))
    (unless old
      (error "Cannot refresh a released LUFT renderer cohort."))
    (unwind-protect
         (progn
           (setf candidate
                 (make-renderer
                  (viewer-device viewer)
                  (canvas-format (viewer-context viewer))
                  (canvas-extent (viewer-context viewer))))
           (dolist (key (renderer-slot-order old))
             (renderer-set-mesh
              candidate key
              (mesh-slot-prepared-mesh
               (gethash key (renderer-mesh-slots old)))))
           (setf completed-p t)
           (make-renderer-live-candidate candidate))
      (unless completed-p
        (when candidate (destroy-renderer candidate))))))

(defgeneric publish-renderer-live-candidate (artifact candidate)
  (:documentation
   "Publish CANDIDATE and return a non-fatal retirement diagnostic, or NIL."))

(defmethod publish-renderer-live-candidate
    ((artifact renderer-live-artifact) candidate)
  (let* ((viewer (renderer-live-artifact-viewer artifact))
         (old (viewer-renderer viewer))
         (new (renderer-live-candidate-renderer candidate))
         (retirement-diagnostic nil))
    (setf (viewer-renderer viewer) new
          (renderer-live-candidate-renderer candidate) nil)
    (handler-case (destroy-renderer old)
      (error (condition)
        ;; Publication succeeded.  Backends retain failed native retirement
        ;; custody; expose it without rolling the viewer back to a dead cohort.
        (setf retirement-diagnostic condition)))
    retirement-diagnostic))

(defgeneric release-renderer-live-candidate (artifact candidate)
  (:documentation "Release an unpublished renderer CANDIDATE."))

(defmethod release-renderer-live-candidate
    ((artifact renderer-live-artifact) candidate)
  (declare (ignore artifact))
  (alexandria:when-let ((renderer
                         (renderer-live-candidate-renderer candidate)))
    (destroy-renderer renderer)
    (setf (renderer-live-candidate-renderer candidate) nil))
  nil)

(defun renderer-live-artifact-refresh-p (artifact source-revision)
  (or (renderer-live-artifact-force-p artifact)
      (> source-revision
         (renderer-live-attempted-source-revision artifact))
      (not
       (luv.shader:shader-source-value-references-current-p
        (renderer-live-attempted-source-values artifact)))))

(defun %refresh-renderer-live-artifact (artifact)
  "Refresh ARTIFACT while its mutation lock is held."
  (unless (member (renderer-live-artifact-status artifact)
                  '(:releasing :released))
    (let ((source-revision (luv.shader:shader-source-revision)))
      (when (renderer-live-artifact-refresh-p artifact source-revision)
        (let ((candidate nil)
              (references (list nil)))
          (setf (renderer-live-artifact-force-p artifact) nil
                (renderer-live-artifact-status artifact) :building)
          (let ((luv.shader:*shader-source-value-references* references))
            (unwind-protect
                 (handler-case
                     (progn
                       (setf candidate
                             (build-renderer-live-candidate artifact))
                       (let ((after (luv.shader:shader-source-revision)))
                         (unless (= source-revision after)
                           (error 'renderer-source-changed-during-build
                                  :before source-revision :after after)))
                       (let ((retirement
                               (publish-renderer-live-candidate
                                artifact candidate)))
                         (report-renderer-live-diagnostic artifact retirement)
                         (setf (renderer-live-artifact-status artifact)
                               :installed
                               (renderer-live-artifact-diagnostic artifact)
                               retirement
                               (renderer-live-artifact-installed-revision
                                artifact)
                               (1+
                                (renderer-live-artifact-installed-revision
                                 artifact))
                               (renderer-live-installed-source-revision artifact)
                               source-revision
                               (renderer-live-installed-source-values artifact)
                               (copy-list (car references)))))
                   (error (condition)
                     ;; Failed source is inspectable state, not an outage.
                     (report-renderer-live-diagnostic artifact condition)
                     (setf (renderer-live-artifact-status artifact) :failed
                           (renderer-live-artifact-diagnostic artifact)
                           condition)))
              (setf (renderer-live-attempted-source-revision artifact)
                    source-revision
                    (renderer-live-attempted-source-values artifact)
                    (copy-list (car references)))
              (when candidate
                (handler-case
                    (release-renderer-live-candidate artifact candidate)
                  (error (condition)
                    ;; Never replace the source/build condition while
                    ;; unwinding an unpublished cohort.  Native backends keep
                    ;; failed retirement custody; retain the cleanup detail on
                    ;; the artifact when it is the only diagnostic.
                    (unless (renderer-live-artifact-diagnostic artifact)
                      (setf (renderer-live-artifact-diagnostic artifact)
                            condition)))))))))))
  artifact)

(defmethod refresh-live-artifact ((artifact renderer-live-artifact))
  (sb-thread:with-mutex ((renderer-live-artifact-lock artifact))
    (%refresh-renderer-live-artifact artifact)))

(defun force-viewer-live-artifact-refresh (viewer)
  "Request and synchronously perform one complete renderer-cohort rebuild."
  (alexandria:when-let ((artifact (viewer-live-artifact viewer)))
    (sb-thread:with-mutex ((renderer-live-artifact-lock artifact))
      (setf (renderer-live-artifact-force-p artifact) t))
    (refresh-live-artifact artifact)))

(defun note-viewer-renderer-replacement
    (viewer source-values source-revision)
  "Adopt a complete renderer installed by an explicit scene rebuild."
  (alexandria:when-let ((artifact (viewer-live-artifact viewer)))
    (sb-thread:with-mutex ((renderer-live-artifact-lock artifact))
      (setf (renderer-live-artifact-status artifact) :installed
            (renderer-live-artifact-diagnostic artifact) nil
            (renderer-live-artifact-installed-revision artifact)
            (1+ (renderer-live-artifact-installed-revision artifact))
            (renderer-live-installed-source-revision artifact) source-revision
            (renderer-live-attempted-source-revision artifact) source-revision
            (renderer-live-installed-source-values artifact)
            (copy-list source-values)
            (renderer-live-attempted-source-values artifact)
            (copy-list source-values))
      (report-renderer-live-diagnostic artifact nil)))
  viewer)

(defgeneric release-renderer-live-resources (artifact)
  (:documentation "Release ARTIFACT's installed renderer cohort."))

(defmethod release-renderer-live-resources
    ((artifact renderer-live-artifact))
  (alexandria:when-let ((viewer (renderer-live-artifact-viewer artifact)))
    (alexandria:when-let ((renderer (viewer-renderer viewer)))
      ;; Relinquish the application-visible borrow before native retirement.
      ;; Backends retain custody of a failed native destruction, while leaving
      ;; the renderer installed here would invite a later frame or release to
      ;; use or retire the same cohort twice.
      (setf (viewer-renderer viewer) nil)
      (destroy-renderer renderer)))
  nil)

(defmethod release-live-artifact ((artifact renderer-live-artifact))
  (sb-thread:with-mutex ((renderer-live-artifact-lock artifact))
    (unless (eq :released (renderer-live-artifact-status artifact))
      (setf (renderer-live-artifact-status artifact) :releasing)
      (unwind-protect
           (release-renderer-live-resources artifact)
        ;; Release is terminal even when native retirement reports a failure:
        ;; the backend owns that failed retirement and the application must not
        ;; resurrect or double-destroy the detached cohort.
        (alexandria:when-let
            ((viewer (renderer-live-artifact-viewer artifact)))
          (sb-thread:with-mutex (*viewer-live-artifacts-lock*)
            (when (eq artifact (gethash viewer *viewer-live-artifacts*))
              (remhash viewer *viewer-live-artifacts*))))
        (setf (renderer-live-artifact-viewer artifact) nil
              (renderer-live-artifact-status artifact) :released))))
  nil)

(defun release-viewer-live-artifact (viewer)
  (alexandria:when-let ((artifact (viewer-live-artifact viewer)))
    (release-live-artifact artifact))
  nil)
