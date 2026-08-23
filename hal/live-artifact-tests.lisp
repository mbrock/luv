(in-package #:luv.tests)

;;; The generic protocol deliberately requires no common superclass.  This is
;;; the adapter shape a whole LUFT renderer cohort can use without pretending
;;; to be one per-pipeline artifact.

(defclass protocol-live-artifact ()
  ((refresh-count :initform 0 :accessor protocol-live-artifact-refresh-count)
   (released-p :initform nil :accessor protocol-live-artifact-released-p)))

(defmethod luv::live-artifact-label ((artifact protocol-live-artifact))
  (declare (ignore artifact))
  "protocol cohort")

(defmethod luv::live-artifact-status ((artifact protocol-live-artifact))
  (if (protocol-live-artifact-released-p artifact) :released :installed))

(defmethod luv::live-artifact-diagnostic ((artifact protocol-live-artifact))
  (declare (ignore artifact))
  nil)

(defmethod luv::live-artifact-installed-revision
    ((artifact protocol-live-artifact))
  (protocol-live-artifact-refresh-count artifact))

(defmethod luv::refresh-live-artifact ((artifact protocol-live-artifact))
  (unless (protocol-live-artifact-released-p artifact)
    (incf (protocol-live-artifact-refresh-count artifact)))
  artifact)

(defmethod luv::release-live-artifact ((artifact protocol-live-artifact))
  (setf (protocol-live-artifact-released-p artifact) t)
  nil)

(defclass protocol-live-application ()
  ((artifact :initarg :artifact :reader protocol-live-application-artifact)))

(defmethod luv::application-live-artifacts
    ((application protocol-live-application))
  (list (protocol-live-application-artifact application)))

(deftest live-artifact-protocol-adapts-whole-cohorts-without-a-base-class
  (let* ((artifact (make-instance 'protocol-live-artifact))
         (application
           (make-instance 'protocol-live-application :artifact artifact)))
    (ok (null (luv::application-live-artifacts :application-with-none)))
    (ok (string= "protocol cohort" (luv::live-artifact-label artifact)))
    (ok (eq :installed (luv::live-artifact-status artifact)))
    (ok (eq application
            (luv::refresh-application-live-artifacts application)))
    (ok (= 1 (luv::live-artifact-installed-revision artifact)))
    (luv::release-live-artifact artifact)
    (ok (eq :released (luv::live-artifact-status artifact)))
    (luv::refresh-application-live-artifacts application)
    (ok (= 1 (luv::live-artifact-installed-revision artifact)))))

;;; Backend-independent live-pipeline ownership probe -------------------------

(defparameter *live-artifact-test-color* 0.2f0)

(defmethod luv.shader:shader-source-value
    ((name (eql 'live-artifact-test-color)))
  (declare (ignore name))
  (values *live-artifact-test-color* nil t))

(luv.shader:define-shader-method
    luv.shader:shader-specification-for
    live-artifact-test-vertex-specification
    ((role (eql :live-artifact-test)) (stage (eql :vertex)))
    (:stage :vertex
     :inputs ((position :vec3 :location 0))
     :outputs ((clip-position :vec4 :built-in :position)))
  (let* ((clip (luv.shader:vec4 position 1.0)))
    (luv.shader:set-output clip-position clip)))

(luv.shader:define-shader-method
    luv.shader:shader-specification-for
    live-artifact-test-fragment-specification
    ((role (eql :live-artifact-test)) (stage (eql :fragment)))
    (:stage :fragment
     :outputs ((color :vec4 :location 0)))
  (let* ((rgba
           (luv.shader:vec4 live-artifact-test-color 0.25 0.75 1.0)))
    (luv.shader:set-output color rgba)))

(defclass live-artifact-test-resource ()
  ((destroy-count :initform 0 :accessor live-artifact-test-destroy-count)
   (fail-destroy-p :initform nil
                   :accessor live-artifact-test-fail-destroy-p)))

(defclass live-artifact-test-module
    (luv:gpu-shader-module live-artifact-test-resource) ())

(defclass live-artifact-test-pipeline
    (luv:gpu-render-pipeline live-artifact-test-resource) ())

(defmethod luv:destroy ((resource live-artifact-test-resource))
  (incf (live-artifact-test-destroy-count resource))
  (when (live-artifact-test-fail-destroy-p resource)
    (error "Deliberate live artifact retirement failure."))
  nil)

(defclass live-artifact-test-device (luv:gpu-device)
  ((modules :initform nil :accessor live-artifact-test-modules)
   (pipelines :initform nil :accessor live-artifact-test-pipelines)
   (fail-pipeline-p :initform nil
                    :accessor live-artifact-test-fail-pipeline-p)))

(defmethod luv:create
    ((device live-artifact-test-device)
     (descriptor luv::shader-module-descriptor))
  (let ((module
          (make-instance 'live-artifact-test-module
                         :label (luv::gpu-descriptor-label descriptor))))
    (push module (live-artifact-test-modules device))
    module))

(defmethod luv:create
    ((device live-artifact-test-device)
     (descriptor luv::render-pipeline-descriptor))
  (declare (ignore descriptor))
  (when (live-artifact-test-fail-pipeline-p device)
    (error "Deliberate live pipeline candidate failure."))
  (let ((pipeline (make-instance 'live-artifact-test-pipeline)))
    (push pipeline (live-artifact-test-pipelines device))
    pipeline))

(defun live-artifact-test-reference-value (artifact)
  (cdr (assoc 'live-artifact-test-color
              (luv::live-shader-pipeline-attempted-source-values artifact))))

(defun redefine-live-artifact-test-fragment (green)
  "Replace the probe's fragment method at its ordinary DEFMETHOD coordinate."
  (eval
   `(luv.shader:define-shader-method
        luv.shader:shader-specification-for
        live-artifact-test-fragment-specification
        ((role (eql :live-artifact-test)) (stage (eql :fragment)))
        (:stage :fragment
         :outputs ((color :vec4 :location 0)))
      (let* ((rgba
               (luv.shader:vec4 live-artifact-test-color ,green 0.75 1.0)))
        (luv.shader:set-output color rgba)))))

(deftest live-pipeline-keeps-last-good-cleans-candidates-and-release-is-terminal
  (let* ((*live-artifact-test-color* 0.2f0)
         (device (make-instance 'live-artifact-test-device))
         (artifact nil))
    (unwind-protect
         (progn
           (setf artifact
                 (luv::make-live-shader-pipeline
                  :role :live-artifact-test
                  :vertex-role :live-artifact-test
                  :label "live artifact ownership probe"
                  :device device :layout nil
                  :vertex-buffers
                  '((:array-stride 12
                     :attributes
                     ((:shader-location 0 :offset 0 :format :float32x3))))
                  :target-format :rgba8-unorm
                  :primitive '(:topology :triangle-list)
                  :depth-stencil nil))
           (let ((first-pipeline
                   (luv::live-shader-pipeline-native-pipeline artifact))
                 (first-modules (copy-list (live-artifact-test-modules device))))
             (ok (eq :installed (luv::live-artifact-status artifact)))
             (ok (= 0 (luv::live-artifact-installed-revision artifact)))
             (ok (= 0.2f0 (live-artifact-test-reference-value artifact)))
             ;; The changed live source value requests an attempt.  Pipeline
             ;; creation fails after both candidate modules exist: the old
             ;; installed cohort survives and both candidates are reclaimed.
             (setf *live-artifact-test-color* 0.8f0
                   (live-artifact-test-fail-pipeline-p device) t)
             (luv::refresh-live-artifact artifact)
             (ok (eq :failed (luv::live-artifact-status artifact)))
             (ok (typep (luv::live-artifact-diagnostic artifact) 'error))
             (ok (eq first-pipeline
                     (luv::live-shader-pipeline-native-pipeline artifact)))
             (ok (= 0 (luv::live-artifact-installed-revision artifact)))
             (ok (= 0.8f0 (live-artifact-test-reference-value artifact)))
             (ok (= 4 (length (live-artifact-test-modules device))))
             (ok (every (lambda (resource)
                          (= 1 (live-artifact-test-destroy-count resource)))
                        (subseq (live-artifact-test-modules device) 0 2)))
             (ok (every (lambda (resource)
                          (zerop (live-artifact-test-destroy-count resource)))
                        first-modules))
             (ok (zerop (live-artifact-test-destroy-count first-pipeline)))
             ;; A second value change retries without any method edit.  The
             ;; complete candidate installs, then the old cohort retires.
             (setf *live-artifact-test-color* 0.6f0
                   (live-artifact-test-fail-pipeline-p device) nil)
             (luv::refresh-live-shader-pipeline artifact)
             (ok (eq :installed (luv::live-artifact-status artifact)))
             (ok (null (luv::live-artifact-diagnostic artifact)))
             (ok (= 1 (luv::live-artifact-installed-revision artifact)))
             (ok (= 0.6f0
                    (cdr
                     (assoc
                      'live-artifact-test-color
                      (luv::live-shader-pipeline-installed-source-values
                       artifact)))))
             (ok (= 1 (live-artifact-test-destroy-count first-pipeline)))
             (ok (every (lambda (resource)
                          (= 1 (live-artifact-test-destroy-count resource)))
                        first-modules))
             ;; Ordinary method replacement is a narrow MOP dependency, quite
             ;; separate from live values and the global reusable-source stamp.
             (let ((value-pipeline
                     (luv::live-shader-pipeline-native-pipeline artifact))
                   (value-modules
                     (subseq (live-artifact-test-modules device) 0 2)))
               (redefine-live-artifact-test-fragment 0.4)
               (ok
                (luv.shader:shader-definition-change-pending-p
                 (luv::live-shader-pipeline-dependent artifact)))
               (luv::refresh-live-artifact artifact)
               (ok (= 2 (luv::live-artifact-installed-revision artifact)))
               (ok (not
                    (luv.shader:shader-definition-change-pending-p
                     (luv::live-shader-pipeline-dependent artifact))))
               (ok (= 1 (live-artifact-test-destroy-count value-pipeline)))
               (ok (every
                    (lambda (resource)
                      (= 1 (live-artifact-test-destroy-count resource)))
                    value-modules)))
             ;; Reusable shader functions and abstractions carry a conservative
             ;; global revision because their call graph is deliberately late
             ;; bound.  It rebuilds the complete candidate and stamps success.
             (let ((method-pipeline
                     (luv::live-shader-pipeline-native-pipeline artifact))
                   (method-modules
                     (subseq (live-artifact-test-modules device) 0 2)))
               (luv.shader::note-shader-source-redefinition
                'live-artifact-test-abstraction)
               (let ((source-revision (luv.shader:shader-source-revision)))
                 (luv::refresh-live-artifact artifact)
                 (ok (= 3 (luv::live-artifact-installed-revision artifact)))
                 (ok (= source-revision
                        (luv::live-shader-pipeline-installed-source-revision
                         artifact))))
               (ok (= 1 (live-artifact-test-destroy-count method-pipeline)))
               (ok (every
                    (lambda (resource)
                      (= 1 (live-artifact-test-destroy-count resource)))
                    method-modules)))
             (let ((installed-pipeline
                     (luv::live-shader-pipeline-native-pipeline artifact))
                   (installed-modules
                     (subseq (live-artifact-test-modules device) 0 2)))
               (luv::release-live-artifact artifact)
               (ok (eq :released (luv::live-artifact-status artifact)))
               (ok (null
                    (luv::live-shader-pipeline-native-pipeline artifact)))
               (ok (= 1
                      (live-artifact-test-destroy-count installed-pipeline)))
               (ok (every
                    (lambda (resource)
                      (= 1 (live-artifact-test-destroy-count resource)))
                    installed-modules))
               ;; A global reusable-source revision used to resurrect a
               ;; released pipeline.  Release is now terminal and idempotent.
               (let ((module-count
                       (length (live-artifact-test-modules device)))
                     (pipeline-count
                       (length (live-artifact-test-pipelines device))))
                 (luv.shader::note-shader-source-redefinition
                  'released-live-artifact-test)
                 (luv::refresh-live-artifact artifact)
                 (luv::release-live-artifact artifact)
                 (ok (= module-count
                        (length (live-artifact-test-modules device))))
                 (ok (= pipeline-count
                        (length (live-artifact-test-pipelines device))))
                 (ok (= 1
                        (live-artifact-test-destroy-count
                         installed-pipeline))))))))
      (when artifact
        (ignore-errors (luv::release-live-artifact artifact)))))

(deftest live-pipeline-release-detaches-first-and-attempts-every-retirement
  (let* ((*live-artifact-test-color* 0.2f0)
         (device (make-instance 'live-artifact-test-device))
         (artifact
           (luv::make-live-shader-pipeline
            :role :live-artifact-test
            :vertex-role :live-artifact-test
            :label "live artifact failing retirement probe"
            :device device :layout nil
            :vertex-buffers
            '((:array-stride 12
               :attributes
               ((:shader-location 0 :offset 0 :format :float32x3))))
            :target-format :rgba8-unorm
            :primitive '(:topology :triangle-list)
            :depth-stencil nil))
         (pipeline (luv::live-shader-pipeline-native-pipeline artifact))
         (modules (copy-list (live-artifact-test-modules device)))
         (condition nil))
    (setf (live-artifact-test-fail-destroy-p pipeline) t
          (live-artifact-test-fail-destroy-p (first modules)) t)
    (handler-case (luv:release-live-artifact artifact)
      (luv:release-error (failure)
        (setf condition failure)))
    (ok (typep condition 'luv:release-error))
    (ok (= 2 (length (luv:release-error-failures condition))))
    (ok (eq :released (luv:live-artifact-status artifact)))
    (ok (null (luv::live-shader-pipeline-dependent artifact)))
    (ok (null (luv::live-shader-pipeline-vertex-dependent artifact)))
    (ok (null (luv::live-shader-pipeline-native-pipeline artifact)))
    (ok (null (luv::live-shader-pipeline-fragment-module artifact)))
    (ok (null (luv::live-shader-pipeline-vertex-module artifact)))
    (ok (= 1 (live-artifact-test-destroy-count pipeline)))
    (ok (every (lambda (module)
                 (= 1 (live-artifact-test-destroy-count module)))
               modules))
    ;; The error is a published diagnostic, not a claim that teardown may be
    ;; replayed against wrappers the application has already relinquished.
    (ok (eq condition (luv:live-artifact-diagnostic artifact)))
    (ok (null (luv:release-live-artifact artifact)))
    (ok (= 1 (live-artifact-test-destroy-count pipeline)))
    (ok (every (lambda (module)
                 (= 1 (live-artifact-test-destroy-count module)))
               modules))))
