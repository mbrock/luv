(defpackage #:luv/metal/tests
  (:use #:cl #:rove #:luv)
  (:local-nicknames (#:objc #:luv.objective-c)
                    (#:metal #:luv.metal)))

(in-package #:luv/metal/tests)

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
