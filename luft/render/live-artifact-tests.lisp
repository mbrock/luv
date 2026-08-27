(in-package #:luft.render.tests)

(defclass renderer-live-artifact-probe
    (render::renderer-live-artifact)
  ((build-count :initform 0 :accessor probe-build-count)
   (publish-count :initform 0 :accessor probe-publish-count)
   (release-count :initform 0 :accessor probe-release-count)
   (fail-p :initform nil :accessor probe-fail-p)
   (fail-release-p :initform nil :accessor probe-fail-release-p)
   (change-source-p :initform nil :accessor probe-change-source-p)))

(defmethod render::build-renderer-live-candidate
    ((artifact renderer-live-artifact-probe))
  (incf (probe-build-count artifact))
  (when (probe-change-source-p artifact)
    (setf (probe-change-source-p artifact) nil)
    (luv.shader::note-shader-source-redefinition :artifact-race-probe))
  (when (probe-fail-p artifact)
    (error "deliberate renderer build failure"))
  (render::make-renderer-live-candidate
   (gensym "RENDERER-CANDIDATE")))

(defmethod render::publish-renderer-live-candidate
    ((artifact renderer-live-artifact-probe) candidate)
  (incf (probe-publish-count artifact))
  (setf (render::renderer-live-candidate-renderer candidate) nil)
  nil)

(defmethod render::release-renderer-live-candidate
    ((artifact renderer-live-artifact-probe) candidate)
  (declare (ignore artifact))
  (setf (render::renderer-live-candidate-renderer candidate) nil)
  nil)

(defmethod render::release-renderer-live-resources
    ((artifact renderer-live-artifact-probe))
  (incf (probe-release-count artifact))
  (when (probe-fail-release-p artifact)
    (error "deliberate renderer retirement failure"))
  nil)

(defclass failing-renderer-diagnostic-viewer () ())

(defmethod render::note-renderer-live-diagnostic
    ((viewer failing-renderer-diagnostic-viewer) diagnostic)
  (declare (ignore viewer diagnostic))
  (error "deliberate diagnostic observer failure"))

(defun make-renderer-live-artifact-probe
    (&optional (viewer (gensym "VIEWER")))
  (make-instance
   'renderer-live-artifact-probe
   :viewer viewer
   :source-values nil
   :source-revision (luv.shader:shader-source-revision)))

(define-test renderer-live-artifact-keeps-last-good-and-release-is-terminal
  (let ((artifact (make-renderer-live-artifact-probe)))
    (setf (render::renderer-live-artifact-force-p artifact) t)
    (luv:refresh-live-artifact artifact)
    (true (eq :installed (luv:live-artifact-status artifact)))
    (true (= 1 (luv:live-artifact-installed-revision artifact)))
    (true (= 1 (probe-build-count artifact)))
    (true (= 1 (probe-publish-count artifact)))
    (setf (probe-fail-p artifact) t
          (render::renderer-live-artifact-force-p artifact) t)
    (luv:refresh-live-artifact artifact)
    (true (eq :failed (luv:live-artifact-status artifact)))
    (true (typep (luv:live-artifact-diagnostic artifact) 'error))
    ;; Failed source never crosses the publication boundary.
    (true (= 2 (probe-build-count artifact)))
    (true (= 1 (probe-publish-count artifact)))
    (luv:release-live-artifact artifact)
    (luv:release-live-artifact artifact)
    (true (eq :released (luv:live-artifact-status artifact)))
    (true (= 1 (probe-release-count artifact)))
    (setf (render::renderer-live-artifact-force-p artifact) t)
    (luv:refresh-live-artifact artifact)
    (true (= 2 (probe-build-count artifact)))))

(define-test renderer-live-artifact-release-is-terminal-after-retirement-failure
  (let ((artifact (make-renderer-live-artifact-probe)))
    (setf (probe-fail-release-p artifact) t)
    (fail (luv:release-live-artifact artifact) 'error)
    (true (eq :released (luv:live-artifact-status artifact)))
    (true (null (render::renderer-live-artifact-viewer artifact)))
    (true (= 1 (probe-release-count artifact)))
    ;; A reporting failure does not turn release into a repeatable ownership
    ;; claim over the already-detached native cohort.
    (true (null (luv:release-live-artifact artifact)))
    (true (= 1 (probe-release-count artifact)))))

(define-test renderer-publication-ignores-a-secondary-diagnostic-observer-failure
  (let ((artifact
          (make-renderer-live-artifact-probe
           (make-instance 'failing-renderer-diagnostic-viewer))))
    (setf (render::renderer-live-artifact-force-p artifact) t)
    (luv:refresh-live-artifact artifact)
    (true (eq :installed (luv:live-artifact-status artifact)))
    (true (= 1 (probe-publish-count artifact)))
    (true (null (luv:live-artifact-diagnostic artifact)))
    (luv:release-live-artifact artifact)))

(define-test renderer-live-artifact-discards-a-source-raced-candidate
  (let ((artifact (make-renderer-live-artifact-probe)))
    (setf (probe-change-source-p artifact) t
          (render::renderer-live-artifact-force-p artifact) t)
    (luv:refresh-live-artifact artifact)
    (true (eq :failed (luv:live-artifact-status artifact)))
    (true (typep (luv:live-artifact-diagnostic artifact)
                 'render::renderer-source-changed-during-build))
    (true (= 1 (probe-build-count artifact)))
    (true (zerop (probe-publish-count artifact)))
    ;; The newer global revision remains pending and succeeds next boundary.
    (luv:refresh-live-artifact artifact)
    (true (eq :installed (luv:live-artifact-status artifact)))
    (true (= 2 (probe-build-count artifact)))
    (true (= 1 (probe-publish-count artifact)))
    (luv:release-live-artifact artifact)))
