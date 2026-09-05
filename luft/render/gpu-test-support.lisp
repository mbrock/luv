(in-package #:luft.render.tests)

;;; Exercise the real component through the HAL with recorded allocations,
;;; submissions, completion, and failure. No native GPU is needed here.

(defclass gpu-test-device (luv:gpu-device)
  ((resources :initform nil :accessor test-gpu-resources)
   (attempts :initform 0 :accessor test-gpu-attempts)
   (fail-at :initarg :fail-at :initform nil :reader test-gpu-fail-at)))

(defclass gpu-test-resource ()
  ((descriptor :initarg :descriptor :reader test-gpu-descriptor)
   (release-attempts :initform 0 :accessor test-gpu-release-attempts)
   (fail-release-p :initform nil :accessor test-gpu-fail-release-p)
   (ready-p :initform nil :accessor test-gpu-ready-p)
   (bytes :initform nil :accessor test-gpu-bytes)))

(defmethod luv:create ((device gpu-test-device) descriptor)
  (when (eql (incf (test-gpu-attempts device)) (test-gpu-fail-at device))
    (error "Injected GPU allocation failure."))
  (let ((resource (make-instance 'gpu-test-resource :descriptor descriptor)))
    (push resource (test-gpu-resources device))
    resource))

(defmethod luv:destroy ((resource gpu-test-resource))
  (incf (test-gpu-release-attempts resource))
  (when (test-gpu-fail-release-p resource)
    (setf (test-gpu-fail-release-p resource) nil)
    (error "Injected GPU release failure.")))

(defmethod luv:write-buffer ((buffer gpu-test-resource) data &key offset)
  (declare (ignore data offset)))

(defmethod luv:read-buffer-if-ready ((buffer gpu-test-resource) &key offset size)
  (declare (ignore offset size))
  (values (test-gpu-bytes buffer) (test-gpu-ready-p buffer)))

(defclass gpu-test-encoder ()
  ((commands :initform nil :accessor test-gpu-commands)))

(defmethod luv:encode ((encoder gpu-test-encoder) command)
  (push command (test-gpu-commands encoder)))

(defmethod luv:begin-render-pass ((encoder gpu-test-encoder) descriptor)
  (push descriptor (test-gpu-commands encoder))
  encoder)

(defmethod luv:end-pass ((encoder gpu-test-encoder))
  (push :end (test-gpu-commands encoder)))

