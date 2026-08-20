;;;; The smallest real Metal object proof through the Objective-C boundary.

(in-package #:luv.metal)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library metal-framework
    (:darwin (:framework "Metal")))
  (cffi:define-foreign-library metal-fx-framework
    (:darwin (:framework "MetalFX"))))

(cffi:use-foreign-library metal-framework)
(cffi:use-foreign-library metal-fx-framework)

(cffi:defcfun ("MTLCreateSystemDefaultDevice" %make-system-default-device
               :library metal-framework)
    :pointer)

(objc:define-objective-c-message device-name
    ("name" :object :ownership :borrowed :class "NSString"))

(objc:define-objective-c-message device-registry-id
    ("registryID" :uint64))

(defun make-system-default-device ()
  "Return the preferred Metal device as one explicitly owned wrapper."
  (objc:wrap-objective-c-object
   (objc:with-objective-c-native-environment
     (%make-system-default-device))
   :ownership :owned
   :protocol-name "MTLDevice"))

(defun probe-system-default-device ()
  "Return bounded, printable evidence from one real native Metal device."
  (objc:with-autorelease-pool ()
    (let ((device (make-system-default-device)))
      (unless device
        (error "Metal did not provide a system default device."))
      (objc:with-owned-objective-c-object (owned-device device)
        (let ((name (device-name owned-device)))
          (list :class (objc:objective-c-object-class-name owned-device)
                :protocol (objc:objective-c-object-protocol-name owned-device)
                :name (objc:objective-c-string name)
                :registry-id (device-registry-id owned-device)))))))
