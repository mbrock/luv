(in-package #:luft.render.tests)

;;; Exercise the real component through the HAL with recorded allocations,
;;; submissions, completion, and failure. No native GPU is needed here.

(defclass exposure-test-device (luv:gpu-device)
  ((resources :initform nil :accessor test-exposure-resources)
   (attempts :initform 0 :accessor test-exposure-attempts)
   (fail-at :initarg :fail-at :initform nil :reader test-exposure-fail-at)))

(defclass exposure-test-resource ()
  ((descriptor :initarg :descriptor :reader test-exposure-descriptor)
   (release-attempts :initform 0 :accessor test-exposure-release-attempts)
   (fail-release-p :initform nil :accessor test-exposure-fail-release-p)
   (ready-p :initform nil :accessor test-exposure-ready-p)
   (bytes :initform nil :accessor test-exposure-bytes)))

(defmethod luv:create ((device exposure-test-device) descriptor)
  (when (eql (incf (test-exposure-attempts device)) (test-exposure-fail-at device))
    (error "Injected exposure allocation failure."))
  (let ((resource (make-instance 'exposure-test-resource :descriptor descriptor)))
    (push resource (test-exposure-resources device))
    resource))

(defmethod luv:destroy ((resource exposure-test-resource))
  (incf (test-exposure-release-attempts resource))
  (when (test-exposure-fail-release-p resource)
    (setf (test-exposure-fail-release-p resource) nil)
    (error "Injected exposure release failure.")))

(defmethod luv:write-buffer ((buffer exposure-test-resource) data &key offset)
  (declare (ignore data offset)))

(defmethod luv:read-buffer-if-ready ((buffer exposure-test-resource) &key offset size)
  (declare (ignore offset size))
  (values (test-exposure-bytes buffer) (test-exposure-ready-p buffer)))

(defclass exposure-test-encoder ()
  ((commands :initform nil :accessor test-exposure-commands)))

(defmethod luv:encode ((encoder exposure-test-encoder) command)
  (push command (test-exposure-commands encoder)))

(defmethod luv:begin-render-pass ((encoder exposure-test-encoder) descriptor)
  (push descriptor (test-exposure-commands encoder))
  encoder)

(defmethod luv:end-pass ((encoder exposure-test-encoder))
  (push :end (test-exposure-commands encoder)))

(define-test fixed-exposure-omits-measurement
  (let ((control (render:make-fixed-exposure 1.25))
        (device (make-instance 'exposure-test-device))
        (encoder (make-instance 'exposure-test-encoder)))
    (true (= 1.25 (render:advance-exposure control)))
    (true (null (render:make-exposure-binding control device :image :sampler)))
    (render:encode-exposure control encoder nil 0)
    (render:release-exposure control)
    (render:release-exposure control)
    (true (null (test-exposure-resources device)))
    (true (null (test-exposure-commands encoder))))
  (dolist (invalid '(0 -1 :automatic))
    (fail (render:make-fixed-exposure invalid))))

(define-test automatic-exposure-rolls-back-every-construction-prefix
  (let* ((device (make-instance 'exposure-test-device))
         (control (render:make-automatic-exposure device))
         (allocations (test-exposure-attempts device)))
    (render:release-exposure control)
    (loop for failure from 1 to allocations do
      (let ((device (make-instance 'exposure-test-device :fail-at failure)))
        (fail (render:make-automatic-exposure device))
        (true (= (1- failure) (length (test-exposure-resources device))))
        (dolist (resource (test-exposure-resources device))
          (true (= 1 (test-exposure-release-attempts resource))))))))

(define-test automatic-exposure-retains-only-failed-release-handles
  (let* ((device (make-instance 'exposure-test-device))
         (control (render:make-automatic-exposure device))
         (failed (first (test-exposure-resources device))))
    (setf (test-exposure-fail-release-p failed) t)
    (fail (render:release-exposure control))
    (true (equal (list failed) (render::exposure-resources control)))
    (render:release-exposure control)
    (render:release-exposure control)
    (true (null (render::exposure-resources control)))
    (dolist (resource (test-exposure-resources device))
      (true (= (if (eq resource failed) 2 1)
               (test-exposure-release-attempts resource))))))

(define-test automatic-exposure-keeps-inflight-measurements-and-consumes-in-order
  (let* ((device (make-instance 'exposure-test-device))
         (control (render:make-automatic-exposure device))
         (encoder (make-instance 'exposure-test-encoder))
         (entries (render::exposure-readbacks control))
         (oldest (render::exposure-readback-buffer (aref entries 0)))
         (newest (render::exposure-readback-buffer (aref entries 2))))
    (unwind-protect
         (progn
           (dotimes (frame 3) (render:encode-exposure control encoder :binding frame))
           (let ((commands (copy-list (test-exposure-commands encoder))))
             (render:encode-exposure control encoder :binding 3)
             (true (equal commands (test-exposure-commands encoder))))
           (setf (test-exposure-ready-p newest) t
                 (test-exposure-bytes newest)
                 (make-array render::+exposure-probe-byte-count+
                             :element-type '(unsigned-byte 8) :initial-element 255))
           (true (= 1.0 (render:advance-exposure control)))
           (setf (test-exposure-ready-p oldest) t
                 (test-exposure-bytes oldest)
                 (make-array render::+exposure-probe-byte-count+
                             :element-type '(unsigned-byte 8) :initial-element 0))
           (true (< (abs (- 1.036 (render:advance-exposure control))) 0.00001))
           (true (null (render::exposure-readback-frame (aref entries 0))))
           (true (= 1 (render::exposure-readback-frame (aref entries 1))))
           (true (= 2 (render::exposure-readback-frame (aref entries 2))))
           (render:encode-exposure control encoder :replacement-binding 3)
           (true (= 3 (render::exposure-readback-frame (aref entries 0)))))
      (render:release-exposure control))))

(define-test renderer-composes-fixed-exposure-through-resize
  (let* ((device (make-instance 'exposure-test-device))
         (factory (lambda (device)
                    (declare (ignore device))
                    (render:make-fixed-exposure 1.25)))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)
                                         :exposure-factory factory)))
    (unwind-protect
         (progn
           (true (eq factory (render::renderer-exposure-factory renderer)))
           (render::replace-renderer-target-generation renderer '(800 600))
           (true (= 1.25 (render::renderer-exposure renderer)))
           (true (null (render::renderer-exposure-binding renderer)))
           (true (notany
                  (lambda (resource)
                    (search "exposure"
                            (or (luv::gpu-descriptor-label
                                 (test-exposure-descriptor resource)) "")))
                  (test-exposure-resources device))))
      (render:destroy-renderer renderer))))

(define-test renderer-resize-rebinds-exposure-without-replacing-its-queue
  (let* ((device (make-instance 'exposure-test-device))
         (renderer (render:make-renderer device :bgra8-unorm '(640 480)))
         (control (render::renderer-exposure-control renderer))
         (queue (render::exposure-readbacks control))
         (binding (render::renderer-exposure-binding renderer)))
    (unwind-protect
         (progn
           (setf (render::exposure-readback-frame (aref queue 0)) 7)
           (render::replace-renderer-target-generation renderer '(800 600))
           (true (eq control (render::renderer-exposure-control renderer)))
           (true (eq queue (render::exposure-readbacks control)))
           (true (= 7 (render::exposure-readback-frame (aref queue 0))))
           (true (not (eq binding (render::renderer-exposure-binding renderer))))
           (true (= 1 (test-exposure-release-attempts binding))))
      (render:destroy-renderer renderer))))
