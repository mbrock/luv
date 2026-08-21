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
         (technique (gensym "TECHNIQUE"))
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
  (let* ((technique (gensym "TECHNIQUE"))
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
  (let* ((technique (gensym "TECHNIQUE"))
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
