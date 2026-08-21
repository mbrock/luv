(in-package #:luvcraft.tests)

(defclass renderer-release-probe ()
  ((failures-remaining :initarg :failures-remaining :initform 0
                       :accessor renderer-release-probe-failures-remaining)
   (attempts :initform 0 :accessor renderer-release-probe-attempts)))

(defmethod destroy ((probe renderer-release-probe))
  (incf (renderer-release-probe-attempts probe))
  (when (plusp (renderer-release-probe-failures-remaining probe))
    (decf (renderer-release-probe-failures-remaining probe))
    (error "Synthetic renderer resource release failure."))
  probe)

(defun make-renderer-test-surface-technique (&key shadow-p layout)
  "Make a resource-free LUFT technique fixture with an optional release probe."
  (let ((technique
          (make-instance
           'luft.render:surface-technique
           :device nil
           :pipeline-styles '(:stock)
           :target-formats '(:rgba16-float)
           :output-space :linear
           :orthographic-shadow-depth-format
           (and shadow-p :depth32-float))))
    (when layout
      (setf (luft.render:surface-technique-layout technique) layout))
    technique))

(deftest session-coordinates-one-renderer-owner
  (let* ((pipeline (gensym "PIPELINE"))
         (resource (gensym "RESOURCE"))
         (replacement (gensym "REPLACEMENT"))
         (renderer
           (make-instance 'luvcraft-renderer
                          :block-pipeline pipeline
                          :resources (list resource)))
         (session (make-instance 'luvcraft-session :renderer renderer)))
    (ok (eq renderer (luvcraft-session-renderer session)))
    (ok (equal (list pipeline)
               (luvcraft::luvcraft-renderer-pipelines renderer)))
    (ok (equal (list resource)
               (luvcraft::luvcraft-renderer-resources renderer)))
    (setf (luvcraft::luvcraft-renderer-resources renderer)
          (list replacement))
    (ok (equal (list replacement)
               (luvcraft::luvcraft-renderer-resources renderer)))))

(deftest frame-attachments-publish-as-one-session-facing-cohort
  (let* ((old-color (gensym "OLD-COLOR"))
         (old-depth (gensym "OLD-DEPTH"))
         (new-color (gensym "NEW-COLOR"))
         (new-depth (gensym "NEW-DEPTH"))
         (old-attachments
           (list :render-extent '(640 480)
                 :color-texture old-color
                 :depth-texture old-depth))
         (new-attachments
           (list :render-extent '(1280 720)
                 :color-texture new-color
                 :depth-texture new-depth))
         (renderer
           (make-instance 'luvcraft-renderer
                          :frame-attachments old-attachments
                          :resources (list old-color old-depth)))
         (session (make-instance 'luvcraft-session :renderer renderer)))
    (ok (eq old-attachments
            (luvcraft::luvcraft-renderer-frame-attachments renderer)))
    (ok (eq old-color (luvcraft::luvcraft-session-color-texture session)))
    (ok (eq old-depth (luvcraft::luvcraft-session-depth-texture session)))
    (luvcraft::install-luvcraft-frame-attachments renderer new-attachments)
    (ok (eq new-attachments
            (luvcraft::luvcraft-renderer-frame-attachments renderer)))
    (ok (equal '(1280 720)
               (luvcraft::luvcraft-session-render-extent session)))
    (ok (eq new-color (luvcraft::luvcraft-session-color-texture session)))
    (ok (eq new-depth (luvcraft::luvcraft-session-depth-texture session)))
    (ok (equal (list new-color new-depth old-color old-depth)
               (luvcraft::luvcraft-renderer-resources renderer)))))

(deftest live-session-migration-adopts-the-old-render-inventory
  (let* ((atlas (gensym "ATLAS"))
         (color (gensym "COLOR"))
         (depth (gensym "DEPTH"))
         (pipeline (gensym "PIPELINE"))
         (resource (gensym "RESOURCE"))
         (states (make-hash-table))
         (session (make-instance 'luvcraft-session))
         (renderer
           (luvcraft::make-luvcraft-renderer-from-retired-session-slots
            session
            (list 'luvcraft::atlas-texture atlas
                  'luvcraft::render-extent '(1024 768)
                  'luvcraft::color-texture color
                  'luvcraft::depth-texture depth
                  'luvcraft::block-pipeline pipeline
                  'luvcraft::frame-states states
                  'luvcraft::resources (list resource)))))
    (ok (eq atlas (luvcraft::luvcraft-renderer-atlas-texture renderer)))
    (ok (equal '(1024 768)
               (luvcraft::luvcraft-renderer-render-extent renderer)))
    (ok (eq color (luvcraft::luvcraft-renderer-color-texture renderer)))
    (ok (eq depth (luvcraft::luvcraft-renderer-depth-texture renderer)))
    (ok (equal (list pipeline)
               (luvcraft::luvcraft-renderer-pipelines renderer)))
    (ok (eq states (luvcraft::luvcraft-renderer-frame-states renderer)))
    (ok (equal (list resource)
               (luvcraft::luvcraft-renderer-resources renderer)))))

(deftest live-session-luft-adoption-retires-pre-seam-frame-states
  (let* ((resource (make-instance 'renderer-release-probe))
         (state
           (make-instance
            'luvcraft::luvcraft-frame-state
            :uniform-buffer resource
            :particle-vertex-buffer resource
            :critter-vertex-buffer resource
            :physics-vertex-buffer resource
            :physics-instance-buffer resource
            :body-vertex-buffer resource
            :scene-bind-group resource
            :shadow-bind-group resource
            :post-uniform-buffer resource
            :post-bind-group resource
            :bloom-scene-bind-group resource
            :bloom-primary-bind-group resource
            :bloom-secondary-bind-group resource))
         (states (make-hash-table))
         (materialization (gensym "MATERIALIZATION"))
         (adapter (gensym "ADAPTER"))
         (technique (make-renderer-test-surface-technique :shadow-p t))
         (renderer
           (make-instance 'luvcraft-renderer
                          :surface-technique technique
                          :frame-states states
                          :resources (list resource)))
         (session
           (make-instance 'luvcraft-session
                          :renderer renderer
                          :luft-world-materialization materialization
                          :luft-frame-adapter adapter)))
    (setf (gethash :retired-drawable states) state)
    (multiple-value-bind (actual-materialization actual-adapter
                          actual-technique)
        (luvcraft::ensure-luvcraft-luft-integration session)
      (ok (eq materialization actual-materialization))
      (ok (eq adapter actual-adapter))
      (ok (eq technique actual-technique)))
    (ok (zerop (hash-table-count states)))
    (ok (= 1 (renderer-release-probe-attempts resource)))
    (ok (null (luvcraft::luvcraft-renderer-resources renderer)))))

(deftest complete-live-session-luft-integration-keeps-frame-cache
  (let* ((technique (make-renderer-test-surface-technique :shadow-p t))
         (surface-state
           (make-instance 'luft.render:surface-frame-state
                          :technique technique))
         (state
           (make-instance 'luvcraft::luvcraft-frame-state
                          :surface-frame-state surface-state))
         (states (make-hash-table))
         (materialization (gensym "MATERIALIZATION"))
         (adapter (gensym "ADAPTER"))
         (renderer
           (make-instance 'luvcraft-renderer
                          :surface-technique technique
                          :frame-states states))
         (session
           (make-instance 'luvcraft-session
                          :renderer renderer
                          :luft-world-materialization materialization
                          :luft-frame-adapter adapter)))
    (setf (gethash :current-drawable states) state)
    (multiple-value-bind (actual-materialization actual-adapter
                          actual-technique)
        (luvcraft::ensure-luvcraft-luft-integration session)
      (ok (eq materialization actual-materialization))
      (ok (eq adapter actual-adapter))
      (ok (eq technique actual-technique)))
    (ok (eq state (gethash :current-drawable states)))
    (ok (eq surface-state
            (luvcraft::luvcraft-frame-surface-frame-state state)))))

(deftest live-session-recovers-a-lost-technique-from-its-frame-state
  (let* ((technique (make-renderer-test-surface-technique :shadow-p t))
         (surface-state
           (make-instance 'luft.render:surface-frame-state
                          :technique technique))
         (state
           (make-instance 'luvcraft::luvcraft-frame-state
                          :surface-frame-state surface-state))
         (states (make-hash-table))
         (materialization (gensym "MATERIALIZATION"))
         (adapter (gensym "ADAPTER"))
         (renderer
           (make-instance 'luvcraft-renderer :frame-states states))
         (session
           (make-instance 'luvcraft-session
                          :renderer renderer
                          :luft-world-materialization materialization
                          :luft-frame-adapter adapter)))
    (setf (gethash :current-drawable states) state)
    (multiple-value-bind (actual-materialization actual-adapter
                          actual-technique)
        (luvcraft::ensure-luvcraft-luft-integration session)
      (ok (eq materialization actual-materialization))
      (ok (eq adapter actual-adapter))
      (ok (eq technique actual-technique)))
    (ok (eq technique
            (luvcraft::luvcraft-renderer-surface-technique renderer)))
    (ok (eq state (gethash :current-drawable states)))))

(deftest live-session-shadow-technique-upgrade-keeps-old-owner-until-retry
  (let* ((old-layout
           (make-instance 'renderer-release-probe :failures-remaining 1))
         (old-technique
           (make-renderer-test-surface-technique :layout old-layout))
         (surface-state
           (make-instance 'luft.render:surface-frame-state
                          :technique old-technique))
         (state
           (make-instance 'luvcraft::luvcraft-frame-state
                          :uniform-buffer nil
                          :particle-vertex-buffer nil
                          :critter-vertex-buffer nil
                          :physics-vertex-buffer nil
                          :physics-instance-buffer nil
                          :body-vertex-buffer nil
                          :scene-bind-group nil
                          :shadow-bind-group nil
                          :post-uniform-buffer nil
                          :post-bind-group nil
                          :surface-frame-state surface-state))
         (states (make-hash-table))
         (first-candidate-layout (make-instance 'renderer-release-probe))
         (first-candidate
           (make-renderer-test-surface-technique
            :shadow-p t :layout first-candidate-layout))
         (second-candidate-layout (make-instance 'renderer-release-probe))
         (second-candidate
           (make-renderer-test-surface-technique
            :shadow-p t :layout second-candidate-layout))
         (candidates (list first-candidate second-candidate))
         (renderer
           (make-instance 'luvcraft-renderer
                          :surface-technique old-technique
                          :frame-states states))
         (materialization (gensym "MATERIALIZATION"))
         (adapter (gensym "ADAPTER"))
         (session
           (make-instance 'luvcraft-session
                          :device nil
                          :renderer renderer
                          :luft-world-materialization materialization
                          :luft-frame-adapter adapter))
         (constructor 'luvcraft::make-luvcraft-surface-technique)
         (original-constructor (symbol-function constructor)))
    (luft.render::register-surface-frame-state surface-state)
    (setf (gethash :old-drawable states) state)
    (unwind-protect
         (progn
           (setf (symbol-function constructor)
                 (lambda (device)
                   (declare (ignore device))
                   (or (pop candidates)
                       (error "No synthetic LUFT technique candidate remains."))))
           (ok (signals
                (luvcraft::ensure-luvcraft-luft-integration session)
                'luft.render::surface-release-error))
           (ok (eq old-technique
                   (luvcraft::luvcraft-renderer-surface-technique renderer)))
           (ok (zerop (hash-table-count states)))
           (ok (= 1 (renderer-release-probe-attempts old-layout)))
           (ok (= 1
                  (renderer-release-probe-attempts first-candidate-layout)))
           (ok (null
                (luft.render:surface-technique-layout first-candidate)))
           (ok (null
                (luvcraft::luvcraft-renderer-surface-technique-candidate
                 renderer)))
           (multiple-value-bind (actual-materialization actual-adapter
                                 actual-technique)
               (luvcraft::ensure-luvcraft-luft-integration session)
             (ok (eq materialization actual-materialization))
             (ok (eq adapter actual-adapter))
             (ok (eq second-candidate actual-technique)))
           (ok (eq second-candidate
                   (luvcraft::luvcraft-renderer-surface-technique renderer)))
           (ok (null
                (luvcraft::luvcraft-renderer-surface-technique-candidate
                 renderer)))
           (ok (= 2 (renderer-release-probe-attempts old-layout)))
           (ok (null (luft.render:surface-technique-layout old-technique)))
           (ok (zerop
                (renderer-release-probe-attempts second-candidate-layout)))
           (ok (null candidates)))
      (setf (symbol-function constructor) original-constructor)
      (let ((published
              (luvcraft::luvcraft-renderer-surface-technique renderer)))
        (when (typep published 'luft.render:surface-technique)
          (ignore-errors
            (luft.render:destroy-surface-technique published)))))))

(deftest partial-luft-technique-construction-keeps-old-publication-until-retry
  (let* ((old-technique (make-renderer-test-surface-technique))
         (surface-state
           (make-instance 'luft.render:surface-frame-state
                          :technique old-technique))
         (state
           (make-instance 'luvcraft::luvcraft-frame-state
                          :uniform-buffer nil
                          :particle-vertex-buffer nil
                          :critter-vertex-buffer nil
                          :physics-vertex-buffer nil
                          :physics-instance-buffer nil
                          :body-vertex-buffer nil
                          :scene-bind-group nil
                          :shadow-bind-group nil
                          :post-uniform-buffer nil
                          :post-bind-group nil
                          :surface-frame-state surface-state))
         (states (make-hash-table))
         (partial-layout
           (make-instance 'renderer-release-probe :failures-remaining 1))
         (partial-candidate
           (make-renderer-test-surface-technique :layout partial-layout))
         (replacement
           (make-renderer-test-surface-technique :shadow-p t))
         (cause
           (make-condition 'simple-error
                           :format-control
                           "Synthetic LUFT technique construction failure."))
         (construction-attempts 0)
         (retried-before-new-construction-p nil)
         (renderer
           (make-instance 'luvcraft-renderer
                          :surface-technique old-technique
                          :frame-states states))
         (materialization (gensym "MATERIALIZATION"))
         (adapter (gensym "ADAPTER"))
         (session
           (make-instance 'luvcraft-session
                          :device nil
                          :renderer renderer
                          :luft-world-materialization materialization
                          :luft-frame-adapter adapter))
         (constructor 'luvcraft::make-luvcraft-surface-technique)
         (original-constructor (symbol-function constructor)))
    (luft.render::register-surface-frame-state surface-state)
    (setf (gethash :old-drawable states) state)
    (unwind-protect
         (progn
           (setf (symbol-function constructor)
                 (lambda (device)
                   (declare (ignore device))
                   (incf construction-attempts)
                   (if (= 1 construction-attempts)
                       (error
                        'luft.render:surface-technique-construction-error
                        :cause cause :technique partial-candidate)
                       (progn
                         (setf retried-before-new-construction-p
                               (and
                                (= 2
                                   (renderer-release-probe-attempts
                                    partial-layout))
                                (null
                                 (luvcraft::luvcraft-renderer-surface-technique-candidate
                                  renderer))))
                         replacement))))
           (let ((condition
                   (handler-case
                       (progn
                         (luvcraft::ensure-luvcraft-luft-integration session)
                         nil)
                     (luft.render:surface-technique-construction-error
                         (condition)
                       condition))))
             (ok condition)
             (ok (eq cause
                     (luft.render:surface-technique-construction-cause
                      condition)))
             (ok (eq partial-candidate
                     (luft.render:surface-technique-construction-retry-owner
                      condition))))
           (ok (= 1 construction-attempts))
           (ok (eq old-technique
                   (luvcraft::luvcraft-renderer-surface-technique renderer)))
           (ok (eq state (gethash :old-drawable states)))
           (ok (eq partial-candidate
                   (luvcraft::luvcraft-renderer-surface-technique-candidate
                    renderer)))
           (ok (= 1 (renderer-release-probe-attempts partial-layout)))
           (ok (eq partial-layout
                   (luft.render:surface-technique-layout partial-candidate)))
           (multiple-value-bind (actual-materialization actual-adapter
                                 actual-technique)
               (luvcraft::ensure-luvcraft-luft-integration session)
             (ok (eq materialization actual-materialization))
             (ok (eq adapter actual-adapter))
             (ok (eq replacement actual-technique)))
           (ok retried-before-new-construction-p)
           (ok (= 2 construction-attempts))
           (ok (= 2 (renderer-release-probe-attempts partial-layout)))
           (ok (null
                (luft.render:surface-technique-layout partial-candidate)))
           (ok (eq replacement
                   (luvcraft::luvcraft-renderer-surface-technique renderer)))
           (ok (null
                (luvcraft::luvcraft-renderer-surface-technique-candidate
                 renderer)))
           (ok (zerop (hash-table-count states))))
      (setf (symbol-function constructor) original-constructor)
      (ignore-errors (release-luvcraft-component renderer))
      (ignore-errors (release-luvcraft-component renderer)))))

(deftest renderer-release-retries-both-luft-technique-owners
  (let* ((candidate-layout
           (make-instance 'renderer-release-probe :failures-remaining 1))
         (candidate
           (make-renderer-test-surface-technique :layout candidate-layout))
         (published-layout
           (make-instance 'renderer-release-probe :failures-remaining 1))
         (published
           (make-renderer-test-surface-technique :layout published-layout))
         (renderer
           (make-instance 'luvcraft-renderer :surface-technique published)))
    (setf (luvcraft::luvcraft-renderer-surface-technique-candidate renderer)
          candidate)
    (ok (signals (release-luvcraft-component renderer)
                 'luvcraft::luvcraft-release-error))
    (ok (= 1 (renderer-release-probe-attempts candidate-layout)))
    (ok (= 1 (renderer-release-probe-attempts published-layout)))
    (ok (eq candidate
            (luvcraft::luvcraft-renderer-surface-technique-candidate
             renderer)))
    (ok (eq published
            (luvcraft::luvcraft-renderer-surface-technique renderer)))
    (release-luvcraft-component renderer)
    (ok (= 2 (renderer-release-probe-attempts candidate-layout)))
    (ok (= 2 (renderer-release-probe-attempts published-layout)))
    (ok (null
         (luvcraft::luvcraft-renderer-surface-technique-candidate renderer)))
    (ok (null (luvcraft::luvcraft-renderer-surface-technique renderer)))
    (release-luvcraft-component renderer)
    (ok (= 2 (renderer-release-probe-attempts candidate-layout)))
    (ok (= 2 (renderer-release-probe-attempts published-layout)))))

(deftest renderer-resource-release-retains-a-failed-handle-for-retry
  (let* ((resource
           (make-instance 'renderer-release-probe :failures-remaining 1))
         (renderer
           (make-instance 'luvcraft-renderer :resources (list resource))))
    (ok (signals (release-luvcraft-component renderer)
                 'luvcraft::luvcraft-release-error))
    (ok (= 1 (renderer-release-probe-attempts resource)))
    (ok (equal (list resource)
               (luvcraft::luvcraft-renderer-resources renderer)))
    (release-luvcraft-component renderer)
    (ok (= 2 (renderer-release-probe-attempts resource)))
    (ok (null (luvcraft::luvcraft-renderer-resources renderer)))
    ;; Once the owner has forgotten a successful release, repeating teardown
    ;; cannot destroy the same native handle again.
    (release-luvcraft-component renderer)
    (ok (= 2 (renderer-release-probe-attempts resource)))))

(deftest renderer-release-deduplicates-its-resource-inventory
  (let* ((resource (make-instance 'renderer-release-probe))
         (renderer
           (make-instance 'luvcraft-renderer
                          :resources (list resource resource))))
    (release-luvcraft-component renderer)
    (ok (= 1 (renderer-release-probe-attempts resource)))
    (ok (null (luvcraft::luvcraft-renderer-resources renderer)))))
