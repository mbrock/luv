(defpackage #:luv/metal/tests
  (:use #:cl #:rove #:luv)
  (:local-nicknames (#:objc #:luv.objective-c)
                    (#:metal #:luv.metal)))

(in-package #:luv/metal/tests)

(objc:define-objective-c-message make-test-metal-layer
    ("new" :object :ownership :owned :class "CAMetalLayer"))

(deftest metal-messages-retain-structure-abi
  (let ((size
          (objc:objective-c-message-description
           'metal::%set-layer-drawable-size))
        (clear
          (objc:objective-c-message-description
           'metal::%set-color-attachment-clear-color)))
    (ok (equal (getf size :selector) "setDrawableSize:"))
    (ok (equal (second (second (getf size :argument-types)))
               '(:struct metal::cg-size)))
    (ok (equal (getf clear :selector) "setClearColor:"))
    (ok (equal (second (second (getf clear :argument-types)))
               '(:struct metal::mtl-clear-color)))))

(deftest unchecked-messages-preserve-by-value-structure-abi
  (objc:with-autorelease-pool ()
    (objc:with-owned-objective-c-object
        (layer
          (make-test-metal-layer (objc:find-objective-c-class "CAMetalLayer")))
      (objc:with-unchecked-objective-c-messages ()
        (metal:set-layer-drawable-size layer 641 359)
        (multiple-value-bind (width height)
            (metal:layer-drawable-size layer)
          (ok (= width 641.0d0))
          (ok (= height 359.0d0)))))))

(deftest canvas-presentation-policy-is-explicit-and-provider-specific
  (ok (equal (luv::sdl-presentation-window-flags :vulkan)
             '(:vulkan :resizable :hidden)))
  (ok (equal (luv::sdl-presentation-window-flags :metal)
             '(:metal :high-pixel-density :resizable :hidden)))
  (let ((canvas (make-sdl-canvas :presentation-api :vulkan)))
    (ok (signals
         (make-canvas-context canvas (make-instance 'metal-gpu-provider))
         'canvas-error))))

(deftest metal-provider-owns-a-real-metal-4-queue
  (let ((device
          (request-gpu-device (make-instance 'metal-gpu-provider))))
    (unwind-protect
         (progn
           (ok (typep device 'metal-gpu-device))
           (ok (typep (device-queue device) 'metal-gpu-queue))
           (ok (equal
                (objc:objective-c-object-protocol-name
                 (luv::metal-native-object (device-queue device)))
                "MTL4CommandQueue")))
      (destroy device))))

(deftest metal-device-owns-a-real-metal-4-compiler
  (let ((device
          (request-gpu-device (make-instance 'metal-gpu-provider))))
    (unwind-protect
         (ok (equal
              (objc:objective-c-object-protocol-name
               (metal-device-compiler device))
              "MTL4Compiler"))
      (destroy device))))

(deftest luvcraft-shader-compiles-in-memory-on-the-metal-device
  (let ((specification
          (luv.spir-v:block-world-fragment-specification))
        (device
          (request-gpu-device (make-instance 'metal-gpu-provider)))
        (module nil))
    (unwind-protect
         (progn
           (setf module
                 (create
                  device
                  (make-shader-module-descriptor
                   :label "block fragment Metal library"
                   :language :mathematical
                   :code specification)))
           (ok (typep module 'metal-gpu-shader-module))
           (ok (eq
                specification
                (luv.msl:msl-document-specification
                 (metal-shader-module-document module))))
           (ok (string= (metal-shader-module-entry-point module)
                        "block_world_fragment_specification"))
           (ok (= (metal-shader-module-function-type module)
                  metal:+function-type-fragment+))
           (ok (equal
                (objc:objective-c-object-protocol-name
                 (luv::metal-native-object module))
                "MTLLibrary")))
      (when module (destroy module))
      (destroy device))))

(deftest failed-metal-library-keeps-the-device-compiler-usable
  (let* ((device
           (request-gpu-device (make-instance 'metal-gpu-provider)))
         (specification
           (luv.spir-v:block-world-fragment-specification))
         (bad-document (luv.msl:compile-msl specification))
         (module nil)
         (failure nil))
    (setf (luv.msl:msl-document-source bad-document)
          "this is deliberately not Metal source")
    (unwind-protect
         (progn
           (handler-case
               (create
                device
                (make-shader-module-descriptor
                 :language :msl :code bad-document))
             (luv::metal-gpu-error (condition)
               (setf failure condition)))
           (ok failure)
           (ok (eq (luv::metal-gpu-error-reason failure)
                   :library-compilation-failed))
           (ok (stringp
                (getf (luv::metal-gpu-error-details failure) :diagnostic)))
           (setf module
                 (create
                  device
                  (make-shader-module-descriptor
                   :language :mathematical :code specification)))
           (ok (typep module 'metal-gpu-shader-module)))
      (when module (destroy module))
      (destroy device))))
