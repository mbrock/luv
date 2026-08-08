;;;; The Vulkan ABI that luv's WebGPU-shaped backend actually uses.
;;;;
;;;; This is intentionally an ordinary, incomplete CFFI binding.  New Vulkan
;;;; declarations belong here only when a concrete GPU backend operation needs
;;;; them.

(in-package #:luv.vulkan)

(defconstant +success+ 0)
(defconstant +incomplete+ 5)

(defconstant +structure-type-application-info+ 0)
(defconstant +structure-type-instance-create-info+ 1)
(defconstant +structure-type-device-queue-create-info+ 2)
(defconstant +structure-type-device-create-info+ 3)

(defconstant +instance-create-enumerate-portability-bit+ #x1)
(defconstant +queue-graphics-bit+ #x1)

(defparameter +portability-enumeration-extension-name+
  "VK_KHR_portability_enumeration")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library vulkan-loader
    (:darwin (:or "libvulkan.1.dylib" "libvulkan.dylib"))
    (:unix (:or "libvulkan.so.1" "libvulkan.so"))
    (:windows "vulkan-1.dll")))

(cffi:use-foreign-library vulkan-loader)

(cffi:defcstruct extension-properties
  (extension-name (:array :char 256))
  (spec-version :uint32))

(cffi:defcstruct application-info
  (s-type :uint32)
  (p-next :pointer)
  (p-application-name :pointer)
  (application-version :uint32)
  (p-engine-name :pointer)
  (engine-version :uint32)
  (api-version :uint32))

(cffi:defcstruct instance-create-info
  (s-type :uint32)
  (p-next :pointer)
  (flags :uint32)
  (p-application-info :pointer)
  (enabled-layer-count :uint32)
  (pp-enabled-layer-names :pointer)
  (enabled-extension-count :uint32)
  (pp-enabled-extension-names :pointer))

(cffi:defcstruct extent-3d
  (width :uint32)
  (height :uint32)
  (depth :uint32))

(cffi:defcstruct queue-family-properties
  (queue-flags :uint32)
  (queue-count :uint32)
  (timestamp-valid-bits :uint32)
  (min-image-transfer-granularity (:struct extent-3d)))

(cffi:defcstruct device-queue-create-info
  (s-type :uint32)
  (p-next :pointer)
  (flags :uint32)
  (queue-family-index :uint32)
  (queue-count :uint32)
  (p-queue-priorities :pointer))

(cffi:defcstruct device-create-info
  (s-type :uint32)
  (p-next :pointer)
  (flags :uint32)
  (queue-create-info-count :uint32)
  (p-queue-create-infos :pointer)
  (enabled-layer-count :uint32)
  (pp-enabled-layer-names :pointer)
  (enabled-extension-count :uint32)
  (pp-enabled-extension-names :pointer)
  (p-enabled-features :pointer))

(cffi:defcfun ("vkEnumerateInstanceExtensionProperties"
               %enumerate-instance-extension-properties
               :library vulkan-loader)
    :int32
  (layer-name :pointer)
  (property-count :pointer)
  (properties :pointer))

(cffi:defcfun ("vkCreateInstance" %create-instance :library vulkan-loader)
    :int32
  (create-info :pointer)
  (allocator :pointer)
  (instance :pointer))

(cffi:defcfun ("vkDestroyInstance" %destroy-instance :library vulkan-loader)
    :void
  (instance :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkEnumeratePhysicalDevices"
               %enumerate-physical-devices
               :library vulkan-loader)
    :int32
  (instance :pointer)
  (device-count :pointer)
  (devices :pointer))

(cffi:defcfun ("vkGetPhysicalDeviceQueueFamilyProperties"
               %get-physical-device-queue-family-properties
               :library vulkan-loader)
    :void
  (physical-device :pointer)
  (property-count :pointer)
  (properties :pointer))

(cffi:defcfun ("vkCreateDevice" %create-device :library vulkan-loader)
    :int32
  (physical-device :pointer)
  (create-info :pointer)
  (allocator :pointer)
  (device :pointer))

(cffi:defcfun ("vkDestroyDevice" %destroy-device :library vulkan-loader)
    :void
  (device :pointer)
  (allocator :pointer))

(cffi:defcfun ("vkGetDeviceQueue" %get-device-queue :library vulkan-loader)
    :void
  (device :pointer)
  (queue-family-index :uint32)
  (queue-index :uint32)
  (queue :pointer))

(cffi:defcfun ("vkDeviceWaitIdle" %device-wait-idle :library vulkan-loader)
    :int32
  (device :pointer))

(define-condition vulkan-call-error (luv:gpu-error)
  ((result
    :initarg :result
    :reader vulkan-call-error-result))
  (:report
   (lambda (condition stream)
     (format stream "Vulkan call ~S failed with VkResult ~D."
             (luv:gpu-error-operation condition)
             (vulkan-call-error-result condition)))))

(defstruct queue-family
  (flags 0 :type (unsigned-byte 32))
  (count 0 :type (unsigned-byte 32)))

(defun make-version (major minor patch)
  (logior (ash major 22) (ash minor 12) patch))

(defun check-result (result operation &rest accepted-results)
  (unless (member result (or accepted-results (list +success+)))
    (error 'vulkan-call-error :operation operation :result result))
  result)

(defun clear-foreign-object (pointer type &optional (count 1))
  (loop for index below (* count (cffi:foreign-type-size type))
        do (setf (cffi:mem-aref pointer :uint8 index) 0))
  pointer)

(defun call-with-foreign-string-array (strings function)
  (if (null strings)
      (funcall function (cffi:null-pointer) 0)
      (let ((pointers nil))
        (unwind-protect
             (progn
               (dolist (string strings)
                 (push (cffi:foreign-string-alloc string) pointers))
               (setf pointers (nreverse pointers))
               (cffi:with-foreign-object
                   (array :pointer (length pointers))
                 (loop for pointer in pointers
                       for index from 0
                       do (setf (cffi:mem-aref array :pointer index) pointer))
                 (funcall function array (length pointers))))
          (mapc #'cffi:foreign-string-free pointers)))))

(defun extension-property-name (properties index)
  (let ((property
          (cffi:mem-aptr
           properties '(:struct extension-properties) index)))
    (cffi:foreign-string-to-lisp
     (cffi:foreign-slot-pointer
      property '(:struct extension-properties) 'extension-name))))

(defun enumerate-instance-extension-names ()
  (cffi:with-foreign-object (count :uint32)
    (setf (cffi:mem-ref count :uint32) 0)
    (check-result
     (%enumerate-instance-extension-properties
      (cffi:null-pointer) count (cffi:null-pointer))
     :enumerate-instance-extension-properties
     +success+ +incomplete+)
    (loop
      for capacity = (cffi:mem-ref count :uint32)
      when (zerop capacity)
        return nil
      do (cffi:with-foreign-object
             (properties '(:struct extension-properties) capacity)
           (clear-foreign-object
            properties '(:struct extension-properties) capacity)
           (let ((result
                   (%enumerate-instance-extension-properties
                    (cffi:null-pointer) count properties)))
             (unless (= result +incomplete+)
               (check-result result :enumerate-instance-extension-properties)
               (return-from enumerate-instance-extension-names
                 (loop for index below (cffi:mem-ref count :uint32)
                       collect (extension-property-name properties index)))))))))

(defun create-instance
    (&key
       (application-name "luv")
       ((:application-version application-version-value)
        (make-version 0 0 1))
       (engine-name "luv")
       ((:engine-version engine-version-value) (make-version 0 0 1))
       ((:api-version api-version-value) (make-version 1 0 0))
       ((:flags flags-value) 0)
       enabled-extension-names)
  (cffi:with-foreign-string (application-name-pointer application-name)
    (cffi:with-foreign-string (engine-name-pointer engine-name)
      (cffi:with-foreign-object
          (application-info '(:struct application-info))
        (clear-foreign-object application-info '(:struct application-info))
        (cffi:with-foreign-slots
            ((s-type p-next p-application-name application-version
              p-engine-name engine-version api-version)
             application-info
             (:struct application-info))
          (setf s-type +structure-type-application-info+
                p-next (cffi:null-pointer)
                p-application-name application-name-pointer
                application-version application-version-value
                p-engine-name engine-name-pointer
                engine-version engine-version-value
                api-version api-version-value))
        (call-with-foreign-string-array
         enabled-extension-names
         (lambda (extension-names extension-count)
           (cffi:with-foreign-object
               (create-info '(:struct instance-create-info))
             (clear-foreign-object create-info '(:struct instance-create-info))
             (cffi:with-foreign-slots
                 ((s-type p-next flags p-application-info
                   enabled-layer-count pp-enabled-layer-names
                   enabled-extension-count pp-enabled-extension-names)
                  create-info
                  (:struct instance-create-info))
               (setf s-type +structure-type-instance-create-info+
                     p-next (cffi:null-pointer)
                     flags flags-value
                     p-application-info application-info
                     enabled-layer-count 0
                     pp-enabled-layer-names (cffi:null-pointer)
                     enabled-extension-count extension-count
                     pp-enabled-extension-names extension-names))
             (cffi:with-foreign-object (instance :pointer)
               (check-result
                (%create-instance
                 create-info (cffi:null-pointer) instance)
                :create-instance)
               (cffi:mem-ref instance :pointer)))))))))

(defun destroy-instance (instance)
  (%destroy-instance instance (cffi:null-pointer))
  (values))

(defun enumerate-physical-devices (instance)
  (cffi:with-foreign-object (count :uint32)
    (setf (cffi:mem-ref count :uint32) 0)
    (check-result
     (%enumerate-physical-devices instance count (cffi:null-pointer))
     :enumerate-physical-devices
     +success+ +incomplete+)
    (loop
      for capacity = (cffi:mem-ref count :uint32)
      when (zerop capacity)
        return nil
      do (cffi:with-foreign-object (devices :pointer capacity)
           (let ((result
                   (%enumerate-physical-devices instance count devices)))
             (unless (= result +incomplete+)
               (check-result result :enumerate-physical-devices)
               (return-from enumerate-physical-devices
                 (loop for index below (cffi:mem-ref count :uint32)
                       collect (cffi:mem-aref devices :pointer index)))))))))

(defun physical-device-queue-families (physical-device)
  (cffi:with-foreign-object (count :uint32)
    (setf (cffi:mem-ref count :uint32) 0)
    (%get-physical-device-queue-family-properties
     physical-device count (cffi:null-pointer))
    (let ((capacity (cffi:mem-ref count :uint32)))
      (if (zerop capacity)
          nil
          (cffi:with-foreign-object
              (properties '(:struct queue-family-properties) capacity)
            (clear-foreign-object
             properties '(:struct queue-family-properties) capacity)
            (%get-physical-device-queue-family-properties
             physical-device count properties)
            (loop for index below
                    (min capacity (cffi:mem-ref count :uint32))
                  for property =
                    (cffi:mem-aptr
                     properties '(:struct queue-family-properties) index)
                  collect
                  (cffi:with-foreign-slots
                      ((queue-flags queue-count)
                       property
                       (:struct queue-family-properties))
                    (make-queue-family
                     :flags queue-flags
                     :count queue-count))))))))

(defun create-device (physical-device family-index)
  (cffi:with-foreign-object (queue-priority :float)
    (setf (cffi:mem-ref queue-priority :float) 1.0)
    (cffi:with-foreign-object
        (queue-info '(:struct device-queue-create-info))
      (clear-foreign-object queue-info '(:struct device-queue-create-info))
      (cffi:with-foreign-slots
          ((s-type p-next flags queue-family-index queue-count
            p-queue-priorities)
           queue-info
           (:struct device-queue-create-info))
        (setf s-type +structure-type-device-queue-create-info+
              p-next (cffi:null-pointer)
              flags 0
              queue-family-index family-index
              queue-count 1
              p-queue-priorities queue-priority))
      (cffi:with-foreign-object
          (create-info '(:struct device-create-info))
        (clear-foreign-object create-info '(:struct device-create-info))
        (cffi:with-foreign-slots
            ((s-type p-next flags queue-create-info-count
              p-queue-create-infos enabled-layer-count pp-enabled-layer-names
              enabled-extension-count pp-enabled-extension-names
              p-enabled-features)
             create-info
             (:struct device-create-info))
          (setf s-type +structure-type-device-create-info+
                p-next (cffi:null-pointer)
                flags 0
                queue-create-info-count 1
                p-queue-create-infos queue-info
                enabled-layer-count 0
                pp-enabled-layer-names (cffi:null-pointer)
                enabled-extension-count 0
                pp-enabled-extension-names (cffi:null-pointer)
                p-enabled-features (cffi:null-pointer)))
        (cffi:with-foreign-object (device :pointer)
          (check-result
           (%create-device
            physical-device create-info (cffi:null-pointer) device)
           :create-device)
          (cffi:mem-ref device :pointer))))))

(defun destroy-device (device)
  (%destroy-device device (cffi:null-pointer))
  (values))

(defun get-device-queue (device queue-family-index &optional (queue-index 0))
  (cffi:with-foreign-object (queue :pointer)
    (%get-device-queue device queue-family-index queue-index queue)
    (cffi:mem-ref queue :pointer)))

(defun device-wait-idle (device)
  (check-result (%device-wait-idle device) :device-wait-idle)
  (values))
