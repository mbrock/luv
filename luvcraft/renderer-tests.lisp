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

(defclass world-panel-order-probe ()
  ((depth :initarg :depth :reader world-panel-order-probe-depth)))

(defmethod luvcraft-overlay-stage ((probe world-panel-order-probe))
  (declare (ignore probe))
  :world-panel)

(defmethod luvcraft-world-panel-depth
    ((probe world-panel-order-probe) session)
  (declare (ignore session))
  (world-panel-order-probe-depth probe))

(define-test native-density-world-panels-order-back-to-front
  (let* ((near (make-instance 'world-panel-order-probe :depth 1.0))
         (middle (make-instance 'world-panel-order-probe :depth 2.0))
         (far (make-instance 'world-panel-order-probe :depth 3.0))
         (scene (gensym "SCENE"))
         (session (make-instance 'luvcraft-session)))
    (setf (luvcraft-session-overlays session)
          (list near scene far middle))
    (true (equal (list far middle near)
                 (luvcraft::luvcraft-world-panels-back-to-front session)))))

(define-test session-coordinates-one-renderer-owner
  (let* ((pipeline (gensym "PIPELINE"))
         (resource (gensym "RESOURCE"))
         (replacement (gensym "REPLACEMENT"))
         (renderer
           (make-instance 'luvcraft-renderer
                          :block-pipeline pipeline
                          :resources (list resource)))
         (session (make-instance 'luvcraft-session :renderer renderer)))
    (true (eq renderer (luvcraft-session-renderer session)))
    (true (equal (list pipeline)
                 (luvcraft::luvcraft-renderer-pipelines renderer)))
    (true (equal (list resource)
                 (luvcraft::luvcraft-renderer-resources renderer)))
    (setf (luvcraft::luvcraft-renderer-resources renderer)
          (list replacement))
    (true (equal (list replacement)
                 (luvcraft::luvcraft-renderer-resources renderer)))))

(define-test frame-attachments-publish-as-one-session-facing-cohort
  (let* ((old-color (gensym "OLD-COLOR"))
         (old-depth (gensym "OLD-DEPTH"))
         (old-panel-color (gensym "OLD-PANEL-COLOR"))
         (old-panel-depth (gensym "OLD-PANEL-DEPTH"))
         (new-color (gensym "NEW-COLOR"))
         (new-depth (gensym "NEW-DEPTH"))
         (new-panel-color (gensym "NEW-PANEL-COLOR"))
         (new-panel-depth (gensym "NEW-PANEL-DEPTH"))
         (old-attachments
           (list :render-extent '(640 480)
                 :presentation-extent '(1280 960)
                 :color-texture old-color
                 :depth-texture old-depth
                 :world-panel-color-texture old-panel-color
                 :world-panel-depth-texture old-panel-depth))
         (new-attachments
           (list :render-extent '(1280 720)
                 :presentation-extent '(2560 1440)
                 :color-texture new-color
                 :depth-texture new-depth
                 :world-panel-color-texture new-panel-color
                 :world-panel-depth-texture new-panel-depth))
         (renderer
           (make-instance 'luvcraft-renderer
                          :frame-attachments old-attachments
                          :resources (list old-color old-depth
                                           old-panel-color old-panel-depth)))
         (session (make-instance 'luvcraft-session :renderer renderer)))
    (true (eq old-attachments
              (luvcraft::luvcraft-renderer-frame-attachments renderer)))
    (true (eq old-color (luvcraft::luvcraft-session-color-texture session)))
    (true (eq old-depth (luvcraft::luvcraft-session-depth-texture session)))
    (luvcraft::install-luvcraft-frame-attachments renderer new-attachments)
    (true (eq new-attachments
              (luvcraft::luvcraft-renderer-frame-attachments renderer)))
    (true (equal '(1280 720)
                 (luvcraft::luvcraft-session-render-extent session)))
    (true (equal '(2560 1440)
                 (luvcraft::luvcraft-session-presentation-extent session)))
    (true (eq new-color (luvcraft::luvcraft-session-color-texture session)))
    (true (eq new-depth (luvcraft::luvcraft-session-depth-texture session)))
    (true (eq new-panel-color
              (luvcraft::luvcraft-session-world-panel-color-texture session)))
    (true (eq new-panel-depth
              (luvcraft::luvcraft-session-world-panel-depth-texture session)))
    (true (equal (list new-color new-depth new-panel-color new-panel-depth
                       old-color old-depth old-panel-color old-panel-depth)
                 (luvcraft::luvcraft-renderer-resources renderer)))))

(define-test presentation-depth-composes-native-density-world-panels
  (let* ((specification
           (luvcraft.shaders:focus-post-fragment-specification))
         (resources (shader:shader-specification-resources specification))
         (panel-color
           (find 'world-panel-color resources
                 :key #'shader:shader-object-name :test #'string-equal))
         (panel-depth
           (find 'world-panel-depth resources
                 :key #'shader:shader-object-name :test #'string-equal)))
    (true panel-color)
    (true panel-depth)
    (true (= 7 (shader:shader-resource-binding panel-color)))
    (true (= 8 (shader:shader-resource-binding panel-depth)))
    (true (find 'panel-visible
                (shader:shader-specification-bindings specification)
                :key #'shader:shader-object-name :test #'string-equal))
    (true (> (length (spv:assemble-shader-specification specification)) 5))))

(define-test live-session-migration-adopts-the-old-render-inventory
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
    (true (eq atlas (luvcraft::luvcraft-renderer-atlas-texture renderer)))
    (true (equal '(1024 768)
                 (luvcraft::luvcraft-renderer-render-extent renderer)))
    (true (eq color (luvcraft::luvcraft-renderer-color-texture renderer)))
    (true (eq depth (luvcraft::luvcraft-renderer-depth-texture renderer)))
    (true (equal (list pipeline)
                 (luvcraft::luvcraft-renderer-pipelines renderer)))
    (true (eq states (luvcraft::luvcraft-renderer-frame-states renderer)))
    (true (equal (list resource)
                 (luvcraft::luvcraft-renderer-resources renderer)))))

(define-test renderer-resource-release-retains-a-failed-handle-for-retry
  (let* ((resource
           (make-instance 'renderer-release-probe :failures-remaining 1))
         (renderer
           (make-instance 'luvcraft-renderer :resources (list resource))))
    (fail (release-luvcraft-component renderer)
          'luv:release-error)
    (true (= 1 (renderer-release-probe-attempts resource)))
    (true (equal (list resource)
                 (luvcraft::luvcraft-renderer-resources renderer)))
    (release-luvcraft-component renderer)
    (true (= 2 (renderer-release-probe-attempts resource)))
    (true (null (luvcraft::luvcraft-renderer-resources renderer)))
    ;; Once the owner has forgotten a successful release, repeating teardown
    ;; cannot destroy the same native handle again.
    (release-luvcraft-component renderer)
    (true (= 2 (renderer-release-probe-attempts resource)))))

(define-test renderer-release-deduplicates-its-resource-inventory
  (let* ((resource (make-instance 'renderer-release-probe))
         (renderer
           (make-instance 'luvcraft-renderer
                          :resources (list resource resource))))
    (release-luvcraft-component renderer)
    (true (= 1 (renderer-release-probe-attempts resource)))
    (true (null (luvcraft::luvcraft-renderer-resources renderer)))))
