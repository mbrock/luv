;;;; The Vulkan ABI that luv's WebGPU-shaped backend actually uses.
;;;;
;;;; This is intentionally an ordinary, incomplete CFFI binding.  New Vulkan
;;;; declarations belong here only when a concrete GPU backend operation needs
;;;; them.

(in-package #:luv.vulkan)

(defparameter +portability-enumeration-extension-name+
  "VK_KHR_portability_enumeration")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library vulkan-loader
    (:darwin (:or "libvulkan.1.dylib" "libvulkan.dylib"))
    (:unix (:or "libvulkan.so.1" "libvulkan.so"))
    (:windows "vulkan-1.dll")))

(cffi:use-foreign-library vulkan-loader)

;;; Symbolic pieces of the Vulkan vocabulary we currently speak.

(cffi:defcenum (result :int32)
  (:success 0)
  (:incomplete 5))

(cffi:defcenum (structure-type :uint32)
  (:application-info 0)
  (:instance-create-info 1)
  (:device-queue-create-info 2)
  (:device-create-info 3))

(cffi:defbitfield (instance-create-flags :uint32)
  (:enumerate-portability #x1))

(cffi:defbitfield (queue-flags :uint32)
  (:graphics #x1)
  (:compute #x2)
  (:transfer #x4)
  (:sparse-binding #x8))

;;; Conditions and result translation.

(define-condition vulkan-call-error (luv:gpu-error)
  ((result
    :initarg :result
    :reader vulkan-call-error-result))
  (:report
   (lambda (condition stream)
     (format stream "Vulkan call ~S failed with VkResult ~D."
             (luv:gpu-error-operation condition)
             (vulkan-call-error-result condition)))))

(defvar *vulkan-operation* :unknown-vulkan-operation)
(defvar *accepted-results* '(:success))

(cffi:define-foreign-type checked-result-type ()
  ()
  (:actual-type :int32)
  (:simple-parser checked-result))

(defmethod cffi:translate-from-foreign
    (value (type checked-result-type))
  (declare (ignore type))
  (let ((result (cffi:foreign-enum-keyword 'result value :errorp nil)))
    (unless (member result *accepted-results*)
      (error 'vulkan-call-error
             :operation *vulkan-operation*
             :result value))
    result))

(defmacro with-vulkan-results ((operation &rest accepted-results) &body body)
  `(let ((*vulkan-operation* ,operation)
         (*accepted-results* ',(or accepted-results '(:success))))
     ,@body))

;;; Struct declarations remain explicit treaty text.  DEFVKSTRUCT supplies the
;;; standard tagged-struct header and retains the declaration as Lisp data for
;;; increasingly capable fillers later.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *struct-descriptions* (make-hash-table)))

(defmacro defvkstruct (name (&key s-type) &body slots)
  (let ((all-slots
          (append (when s-type
                    '((s-type structure-type)
                      (p-next :pointer)))
                  slots)))
    `(progn
       (cffi:defcstruct ,name ,@all-slots)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash ',name *struct-descriptions*)
               ',(list :s-type s-type :slots (mapcar #'first all-slots)))))))

(defvkstruct extension-properties ()
  (extension-name (:array :char 256))
  (spec-version :uint32))

(defvkstruct application-info (:s-type :application-info)
  (p-application-name :pointer)
  (application-version :uint32)
  (p-engine-name :pointer)
  (engine-version :uint32)
  (api-version :uint32))

(defvkstruct instance-create-info (:s-type :instance-create-info)
  (flags instance-create-flags)
  (p-application-info :pointer)
  (enabled-layer-count :uint32)
  (pp-enabled-layer-names :pointer)
  (enabled-extension-count :uint32)
  (pp-enabled-extension-names :pointer))

(defvkstruct extent-3d ()
  (width :uint32)
  (height :uint32)
  (depth :uint32))

(defvkstruct queue-family-properties ()
  (queue-flags queue-flags)
  (queue-count :uint32)
  (timestamp-valid-bits :uint32)
  (min-image-transfer-granularity (:struct extent-3d)))

(defvkstruct device-queue-create-info (:s-type :device-queue-create-info)
  (flags :uint32)
  (queue-family-index :uint32)
  (queue-count :uint32)
  (p-queue-priorities :pointer))

(defvkstruct device-create-info (:s-type :device-create-info)
  (flags :uint32)
  (queue-create-info-count :uint32)
  (p-queue-create-infos :pointer)
  (enabled-layer-count :uint32)
  (pp-enabled-layer-names :pointer)
  (enabled-extension-count :uint32)
  (pp-enabled-extension-names :pointer)
  (p-enabled-features :pointer))

(defun clear-foreign-object (pointer type &optional (count 1))
  (loop for index below (* count (cffi:foreign-type-size type))
        do (setf (cffi:mem-aref pointer :uint8 index) 0))
  pointer)

(defun fill-vk (pointer type &rest fields)
  (let* ((description
           (or (gethash type *struct-descriptions*)
               (error "Unknown Vulkan struct ~S." type)))
         (foreign-type `(:struct ,type))
         (slots (getf description :slots)))
    (clear-foreign-object pointer foreign-type)
    (let ((s-type (getf description :s-type)))
      (when s-type
        (setf (cffi:foreign-slot-value pointer foreign-type 's-type) s-type
              (cffi:foreign-slot-value pointer foreign-type 'p-next)
              (cffi:null-pointer))))
    (loop for (field value) on fields by #'cddr
          for slot = (find (symbol-name field) slots
                           :key #'symbol-name :test #'string=)
          unless slot
            do (error "~S is not a slot of Vulkan struct ~S." field type)
          do (setf (cffi:foreign-slot-value pointer foreign-type slot) value))
    pointer))

(defmacro with-vk ((variable type &rest fields) &body body)
  `(cffi:with-foreign-object (,variable '(:struct ,type))
     (fill-vk ,variable ',type ,@fields)
     ,@body))

;;; Arguments which own temporary foreign storage.

(cffi:define-foreign-type string-list-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser string-list))

(defmethod cffi:translate-to-foreign
    (strings (type string-list-type))
  (declare (ignore type))
  (if (null strings)
      (values (cffi:null-pointer) nil)
      (let ((pointers nil)
            (array nil))
        (unwind-protect
             (progn
               (dolist (string strings)
                 (push (cffi:foreign-string-alloc string) pointers))
               (setf pointers (nreverse pointers)
                     array (cffi:foreign-alloc
                            :pointer :count (length pointers)))
               (loop for pointer in pointers
                     for index from 0
                     do (setf (cffi:mem-aref array :pointer index)
                              pointer))
               (values array pointers))
          (unless array
            (mapc #'cffi:foreign-string-free pointers))))))

(defmethod cffi:free-translated-object
    (pointer (type string-list-type) strings)
  (declare (ignore type))
  (mapc #'cffi:foreign-string-free strings)
  (unless (cffi:null-pointer-p pointer)
    (cffi:foreign-free pointer)))

(defmacro with-translated-values (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (variable value type) (first bindings)
        (let ((parameter (gensym "PARAMETER")))
          `(multiple-value-bind (,variable ,parameter)
               (cffi:convert-to-foreign ,value ',type)
             (unwind-protect
                  (with-translated-values ,(rest bindings) ,@body)
               (cffi:free-converted-object
                ,variable ',type ,parameter)))))))

;;; Raw calls.  Their declarations now express translation and checking.

(cffi:defcfun ("vkEnumerateInstanceExtensionProperties"
               %enumerate-instance-extension-properties
               :library vulkan-loader)
    checked-result
  (layer-name :pointer)
  (property-count :pointer)
  (properties :pointer))

(cffi:defcfun ("vkCreateInstance" %create-instance :library vulkan-loader)
    checked-result
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
    checked-result
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
    checked-result
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
    checked-result
  (device :pointer))

;;; The three ordinary Vulkan call shapes used so far.

(defmacro define-enumerator
    (name lambda-list call &key element extractor
                                 (operation (intern (symbol-name name) :keyword)))
  (let ((count (gensym "COUNT"))
        (capacity (gensym "CAPACITY"))
        (items (gensym "ITEMS"))
        (result (gensym "RESULT"))
        (index (gensym "INDEX")))
    `(defun ,name ,lambda-list
       (cffi:with-foreign-object (,count :uint32)
         (setf (cffi:mem-ref ,count :uint32) 0)
         (with-vulkan-results (,operation :success :incomplete)
           (,(first call) ,@(rest call) ,count (cffi:null-pointer)))
         (loop
           for ,capacity = (cffi:mem-ref ,count :uint32)
           when (zerop ,capacity) return nil
           do (cffi:with-foreign-object (,items ',element ,capacity)
                (clear-foreign-object ,items ',element ,capacity)
                (let ((,result
                        (with-vulkan-results
                            (,operation :success :incomplete)
                          (,(first call) ,@(rest call) ,count ,items))))
                  (unless (eq ,result :incomplete)
                    (return-from ,name
                      (loop for ,index below (cffi:mem-ref ,count :uint32)
                            collect
                            ,(if extractor
                                 `(,extractor ,items ,index)
                                 `(cffi:mem-aref
                                   ,items ',element ,index))))))))))))

(defmacro define-creator
    (name lambda-list call &key (element :pointer) checked
                                (operation (intern (symbol-name name) :keyword)))
  (let ((output (gensym "OUTPUT")))
    `(defun ,name ,lambda-list
       (cffi:with-foreign-object (,output ',element)
         ,(if checked
              `(with-vulkan-results (,operation)
                 (,(first call) ,@(rest call) ,output))
              `(,(first call) ,@(rest call) ,output))
         (cffi:mem-ref ,output ',element)))))

(defun extension-property-name (properties index)
  (let ((property
          (cffi:mem-aptr
           properties '(:struct extension-properties) index)))
    (cffi:foreign-string-to-lisp
     (cffi:foreign-slot-pointer
      property '(:struct extension-properties) 'extension-name))))

(define-enumerator enumerate-instance-extension-names ()
  (%enumerate-instance-extension-properties (cffi:null-pointer))
  :element (:struct extension-properties)
  :extractor extension-property-name
  :operation :enumerate-instance-extension-properties)

(define-enumerator enumerate-physical-devices (instance)
  (%enumerate-physical-devices instance)
  :element :pointer)

(define-creator create-instance-handle (create-info)
  (%create-instance create-info (cffi:null-pointer))
  :checked t
  :operation :create-instance)

(define-creator create-device-handle (physical-device create-info)
  (%create-device physical-device create-info (cffi:null-pointer))
  :checked t
  :operation :create-device)

(define-creator get-device-queue-handle
    (device queue-family-index queue-index)
  (%get-device-queue device queue-family-index queue-index))

;;; Public, Lisp-shaped operations.

(defstruct queue-family
  (flags nil :type list)
  (count 0 :type (unsigned-byte 32)))

(defun make-version (major minor patch)
  (logior (ash major 22) (ash minor 12) patch))

(defun create-instance
    (&key
       (application-name "luv")
       ((:application-version application-version-value)
        (make-version 0 0 1))
       (engine-name "luv")
       ((:engine-version engine-version-value) (make-version 0 0 1))
       ((:api-version api-version-value) (make-version 1 0 0))
       flags
       enabled-extension-names)
  (with-translated-values
      ((application-name-pointer application-name :string)
       (engine-name-pointer engine-name :string)
       (extension-names enabled-extension-names string-list))
    (with-vk (application-info application-info
              :p-application-name application-name-pointer
              :application-version application-version-value
              :p-engine-name engine-name-pointer
              :engine-version engine-version-value
              :api-version api-version-value)
      (with-vk (create-info instance-create-info
                :flags flags
                :p-application-info application-info
                :enabled-layer-count 0
                :pp-enabled-layer-names (cffi:null-pointer)
                :enabled-extension-count (length enabled-extension-names)
                :pp-enabled-extension-names extension-names)
        (create-instance-handle create-info)))))

(defun destroy-instance (instance)
  (%destroy-instance instance (cffi:null-pointer))
  (values))

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
    (with-vk (queue-info device-queue-create-info
              :flags 0
              :queue-family-index family-index
              :queue-count 1
              :p-queue-priorities queue-priority)
      (with-vk (create-info device-create-info
                :flags 0
                :queue-create-info-count 1
                :p-queue-create-infos queue-info
                :enabled-layer-count 0
                :pp-enabled-layer-names (cffi:null-pointer)
                :enabled-extension-count 0
                :pp-enabled-extension-names (cffi:null-pointer)
                :p-enabled-features (cffi:null-pointer))
        (create-device-handle physical-device create-info)))))

(defun destroy-device (device)
  (%destroy-device device (cffi:null-pointer))
  (values))

(defun get-device-queue (device queue-family-index &optional (queue-index 0))
  (get-device-queue-handle device queue-family-index queue-index))

(defun device-wait-idle (device)
  (with-vulkan-results (:device-wait-idle)
    (%device-wait-idle device))
  (values))
