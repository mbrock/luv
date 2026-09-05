(in-package #:luft.render.tests)

(define-test scene-drawings-roll-back-every-construction-prefix
  (dolist (factory '(render:make-sky-drawing render:make-player-drawing))
    (let* ((device (make-instance 'gpu-test-device))
           (drawing (funcall factory device '(:rgba16-float :rg16-float) 4))
           (allocations (test-gpu-attempts device)))
      (render:release-scene-drawing drawing)
      (render:release-scene-drawing drawing)
      (dolist (resource (test-gpu-resources device))
        (true (= 1 (test-gpu-release-attempts resource))))
      (loop for failure from 1 to allocations do
        (let ((device (make-instance 'gpu-test-device :fail-at failure)))
          (fail (funcall factory device '(:rgba16-float :rg16-float) 4))
          (dolist (resource (test-gpu-resources device))
            (true (= 1 (test-gpu-release-attempts resource)))))))))

(define-test renderer-can-omit-both-scene-drawings
  (let* ((device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :sky-factory nil :player-factory nil)))
    (unwind-protect
         (progn
           (true (null (render::renderer-sky renderer)))
           (true (null (render::renderer-player renderer)))
           (true (notany
                  (lambda (resource)
                    (let ((label (or (luv::gpu-descriptor-label
                                      (test-gpu-descriptor resource)) "")))
                      (or (search "sky" label) (search "player" label))))
                  (test-gpu-resources device)))
           (let ((pass (make-instance 'gpu-test-encoder)))
             (render:encode-scene-drawing nil pass nil)
             (true (null (test-gpu-commands pass)))))
      (render:destroy-renderer renderer))))

;;; A replacement uses no built-in pipeline/layout accessors. It exercises
;;; the actual scene composition, including per-frame binding reuse and order.

(defclass test-scene-drawing (render:scene-drawing)
  ((name :initarg :name :reader test-drawing-name)
   (bindings :initform 0 :accessor test-drawing-bindings)
   (releases :initform 0 :accessor test-drawing-releases)))

(defmethod render:make-scene-drawing-binding
    ((drawing test-scene-drawing) device camera-buffer shadow-view shadow-sampler)
  (true camera-buffer)
  (true shadow-view)
  (true shadow-sampler)
  (incf (test-drawing-bindings drawing))
  (luv:create device (luv:make-bind-group-descriptor :label "replacement drawing")))

(defmethod render:encode-scene-drawing ((drawing test-scene-drawing) pass binding)
  (true binding)
  (luv:encode pass (test-drawing-name drawing)))

(defmethod render:release-scene-drawing ((drawing test-scene-drawing))
  (incf (test-drawing-releases drawing)))

(define-test renderer-composes-replacement-drawings-and-reuses-frame-bindings
  (let ((render::*temporal-upscaling-p* nil)
        (device (make-instance 'gpu-test-device))
        (sky (make-instance 'test-scene-drawing :name :test-sky))
        (player (make-instance 'test-scene-drawing :name :test-player)))
    (flet ((factory (drawing)
             (lambda (device formats samples)
               (declare (ignore device))
               (true (equal '(:rgba16-float) formats))
               (true (= 4 samples))
               drawing)))
      (let* ((renderer (render:make-renderer
                        device :bgra8-unorm '(640 480)
                        :sky-factory (factory sky) :player-factory (factory player)))
             (frame (render::make-renderer-frame-state renderer))
             (pass (make-instance 'gpu-test-encoder)))
        (unwind-protect
             (progn
               (dotimes (iteration 2)
                 (render::encode-renderer-scene renderer frame pass t nil))
               (true (= 1 (test-drawing-bindings sky)))
               (true (= 1 (test-drawing-bindings player)))
               (true (equal '(:test-sky :test-player :test-sky :test-player)
                            (remove-if-not #'keywordp
                                           (remove :end (reverse (test-gpu-commands pass))))))
               (render::replace-renderer-target-generation renderer '(800 600))
               (true (eq sky (render::renderer-sky renderer)))
               (true (eq player (render::renderer-player renderer))))
          (render::destroy-renderer-frame-state frame)
          (render:destroy-renderer renderer)))
      (true (= 1 (test-drawing-releases sky)))
      (true (= 1 (test-drawing-releases player))))))

(define-test renderer-rolls-back-earlier-components-when-a-later-factory-fails
  (let ((device (make-instance 'gpu-test-device)))
    (fail (render:make-renderer
           device :bgra8-unorm '(640 480)
           :player-factory (lambda (&rest inputs)
                             (declare (ignore inputs))
                             (error "Replacement player could not be built."))))
    (dolist (resource (test-gpu-resources device))
      (true (= 1 (test-gpu-release-attempts resource))))))

(defclass binding-free-test-drawing (test-scene-drawing) ())

(defmethod render:make-scene-drawing-binding
    ((drawing binding-free-test-drawing) device camera-buffer shadow-view shadow-sampler)
  (declare (ignore device camera-buffer shadow-view shadow-sampler))
  (incf (test-drawing-bindings drawing))
  nil)

(define-test frame-caches-a-binding-free-drawing-without-owning-a-phantom-resource
  (let* ((drawing (make-instance 'binding-free-test-drawing :name :none))
         (device (make-instance 'gpu-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :sky-factory nil :player-factory nil))
         (frame (render::make-renderer-frame-state renderer)))
    (unwind-protect
         (progn
           (dotimes (iteration 2)
             (true (null (render::renderer-frame-drawing-binding renderer frame drawing))))
           (true (= 1 (test-drawing-bindings drawing))))
      (render::destroy-renderer-frame-state frame)
      (render:destroy-renderer renderer)
      (render:release-scene-drawing drawing))))
