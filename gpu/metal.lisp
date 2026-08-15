;;;; The deliberately small Metal implementation needed by canvas clearing.

(in-package #:luv)

(define-condition metal-gpu-error (gpu-error)
  ((reason :initarg :reason :reader metal-gpu-error-reason)
   (details :initarg :details :initform nil :reader metal-gpu-error-details))
  (:report
   (lambda (condition stream)
     (format stream "Metal GPU operation ~S failed: ~S~@[ (~S)~]."
             (gpu-error-operation condition)
             (metal-gpu-error-reason condition)
             (metal-gpu-error-details condition)))))

(defclass metal-gpu-provider (gpu-provider) ()
  (:documentation "A provider for the system's preferred Metal device."))

(defclass metal-gpu-object ()
  ((native-object :initarg :native-object :reader metal-native-object)
   (destroyed-p :initform nil :accessor metal-object-destroyed-p)))

(defclass metal-gpu-device (gpu-device metal-gpu-object)
  ((queue :initform nil :accessor metal-device-queue)))

(defclass metal-gpu-queue (gpu-queue metal-gpu-object)
  ((device :initarg :device :reader metal-queue-device)))

(defclass metal-gpu-texture (gpu-texture metal-gpu-object)
  ((device :initarg :device :reader metal-texture-device)))

(defclass metal-frame-command-encoder (gpu-command-encoder)
  ((context :initarg :context :reader metal-encoder-context)
   (texture :initarg :texture :reader metal-encoder-texture)
   (command-buffer :initarg :command-buffer
                   :reader metal-encoder-command-buffer)
   (encoded-p :initform nil :accessor metal-encoder-encoded-p)))

(defun ensure-live-metal-object (object operation)
  (when (metal-object-destroyed-p object)
    (error 'gpu-object-destroyed-error :object object :operation operation))
  object)

(defun check-metal-device-descriptor (descriptor)
  (unless (typep descriptor 'device-descriptor)
    (error 'gpu-request-error :operation :request-device
           :descriptor descriptor :reason :invalid-descriptor))
  (when (or (device-descriptor-required-features descriptor)
            (device-descriptor-required-limits descriptor))
    (error 'gpu-request-error :operation :request-device
           :descriptor descriptor :reason :unsupported-requirements)))

(defmethod request-gpu-device
    ((provider metal-gpu-provider) &optional descriptor)
  (declare (ignore provider))
  (let ((descriptor (or descriptor (make-device-descriptor))))
    (check-metal-device-descriptor descriptor)
    (let ((native-device (luv.metal:make-system-default-device)))
      (unless native-device
        (error 'metal-gpu-error :operation :request-device
               :reason :no-system-device))
      (handler-case
          (let ((native-queue
                  (luv.metal:new-metal-4-command-queue native-device)))
            (unless native-queue
              (error 'metal-gpu-error :operation :request-device
                     :reason :metal-4-unavailable))
            (let* ((device
                     (make-instance 'metal-gpu-device
                                    :label (gpu-descriptor-label descriptor)
                                    :native-object native-device))
                   (queue
                     (make-instance 'metal-gpu-queue
                                    :label "default Metal 4 queue"
                                    :native-object native-queue
                                    :device device)))
              (setf (metal-device-queue device) queue)
              device))
        (error (condition)
          (unless (luv.objective-c:objective-c-object-released-p native-device)
            (luv.objective-c:release-objective-c-object native-device))
          (error condition))))))

(defmethod device-queue ((device metal-gpu-device))
  (ensure-live-metal-object device :device-queue)
  (metal-device-queue device))

(defmethod destroy ((device metal-gpu-device))
  (unless (metal-object-destroyed-p device)
    (let ((queue (metal-device-queue device)))
      (when (and queue (not (metal-object-destroyed-p queue)))
        (luv.objective-c:release-objective-c-object
         (metal-native-object queue))
        (setf (metal-object-destroyed-p queue) t)))
    (luv.objective-c:release-objective-c-object (metal-native-object device))
    (setf (metal-object-destroyed-p device) t))
  (values))

(defmethod encode
    ((encoder metal-frame-command-encoder)
     (command gpu-clear-texture-command))
  (when (metal-encoder-encoded-p encoder)
    (error 'gpu-invalid-state-error :object encoder :operation :encode
           :state :clear-encoded :expected-state :empty))
  (let ((texture (gpu-clear-texture-command-texture command))
        (color (gpu-clear-texture-command-color command)))
    (unless (typep texture 'metal-gpu-texture)
      (error 'gpu-request-error :operation :encode :descriptor command
             :reason :foreign-texture))
    (ensure-live-metal-object texture :encode)
    (unless (eq texture (metal-encoder-texture encoder))
      (error 'gpu-invalid-state-error :object texture :operation :encode
             :state :outside-frame :expected-state :current-frame))
    (unless (and (= (length color) 4)
                 (every #'realp color))
      (error 'gpu-request-error :operation :encode :descriptor command
             :reason :invalid-clear-color :details color))
    (luv.metal:encode-clear-pass
     (metal-encoder-command-buffer encoder)
     (metal-native-object texture)
     color)
    (setf (metal-encoder-encoded-p encoder) t)
    command))
