;;;; The Metal 4 and CAMetalLayer vocabulary needed by the first frame.
;;;;
;;;; The two small structures cross as typed foreign storage through the
;;;; shared NSInvocation bridge.  They stay declarations in the same message
;;;; system as scalar and object calls; no per-selector shim is needed.

(in-package #:luv.metal)

(cffi:defcstruct cg-size
  (width :double)
  (height :double))

(cffi:defcstruct mtl-clear-color
  (red :double)
  (green :double)
  (blue :double)
  (alpha :double))

(defconstant +pixel-format-bgra8-unorm+ 80)
(defconstant +pixel-format-bgra8-unorm-srgb+ 81)
(defconstant +load-action-clear+ 2)
(defconstant +store-action-store+ 1)

;;; Device and Metal 4 submission.

(objc:define-objective-c-message new-metal-4-command-queue
    ("newMTL4CommandQueue" :object :ownership :owned
     :class "MTL4CommandQueue"))

(objc:define-objective-c-message new-command-allocator
    ("newCommandAllocator" :object :ownership :owned
     :class "MTL4CommandAllocator"))

(objc:define-objective-c-message new-command-buffer
    ("newCommandBuffer" :object :ownership :owned
     :class "MTL4CommandBuffer"))

(objc:define-objective-c-message begin-command-buffer
    ("beginCommandBufferWithAllocator:" :void)
  (allocator :object))

(objc:define-objective-c-message end-command-buffer
    ("endCommandBuffer" :void))

(objc:define-objective-c-message render-command-encoder
    ("renderCommandEncoderWithDescriptor:" :object :ownership :borrowed
     :class "MTL4RenderCommandEncoder")
  (descriptor :object))

(objc:define-objective-c-message end-encoding
    ("endEncoding" :void))

(objc:define-objective-c-message wait-for-drawable
    ("waitForDrawable:" :void)
  (drawable :object))

(objc:define-objective-c-message %commit-command-buffers
    ("commit:count:" :void)
  (command-buffers :pointer)
  (count :uint64))

(objc:define-objective-c-message signal-drawable
    ("signalDrawable:" :void)
  (drawable :object))

(objc:define-objective-c-message present-drawable
    ("present" :void))

(defun commit-command-buffer (queue command-buffer)
  "Commit one ended Metal 4 command buffer to QUEUE."
  (cffi:with-foreign-object (command-buffers :pointer)
    (setf (cffi:mem-ref command-buffers :pointer)
          (objc:objective-c-pointer command-buffer))
    (%commit-command-buffers queue command-buffers 1))
  (values))

;;; SDL's borrowed CAMetalLayer and its current drawable.

(objc:define-objective-c-message set-layer-device
    ("setDevice:" :void)
  (device :object))

(objc:define-objective-c-message set-layer-pixel-format
    ("setPixelFormat:" :void)
  (pixel-format :uint64))

(objc:define-objective-c-message layer-pixel-format
    ("pixelFormat" :uint64))

(objc:define-objective-c-message %set-layer-drawable-size
    ("setDrawableSize:" :void)
  (size (:struct cg-size)))

(objc:define-objective-c-message %layer-drawable-size
    ("drawableSize" (:struct cg-size)))

(objc:define-objective-c-message next-drawable
    ("nextDrawable" :object :ownership :borrowed :class "CAMetalDrawable"))

(objc:define-objective-c-message drawable-texture
    ("texture" :object :ownership :borrowed :class "MTLTexture"))

(defun set-layer-drawable-size (layer width height)
  "Set LAYER's drawable extent in physical pixels."
  (%set-layer-drawable-size
   layer (list 'width (coerce width 'double-float)
               'height (coerce height 'double-float)))
  (values))

(defun layer-drawable-size (layer)
  "Return LAYER's physical drawable width and height as two values."
  (let ((size (%layer-drawable-size layer)))
    (values (getf size 'width) (getf size 'height))))

;;; Empty render pass used as a deterministic clear.

(objc:define-objective-c-message %new-render-pass-descriptor
    ("new" :object :ownership :owned
     :class "MTL4RenderPassDescriptor"))

(objc:define-objective-c-message %render-pass-color-attachments
    ("colorAttachments" :object :ownership :borrowed
     :class "MTLRenderPassColorAttachmentDescriptorArray"))

(objc:define-objective-c-message %color-attachment-at
    ("objectAtIndexedSubscript:" :object :ownership :borrowed
     :class "MTLRenderPassColorAttachmentDescriptor")
  (index :uint64))

(objc:define-objective-c-message %set-color-attachment-texture
    ("setTexture:" :void)
  (texture :object))

(objc:define-objective-c-message %set-color-attachment-load-action
    ("setLoadAction:" :void)
  (action :uint64))

(objc:define-objective-c-message %set-color-attachment-store-action
    ("setStoreAction:" :void)
  (action :uint64))

(objc:define-objective-c-message %set-color-attachment-clear-color
    ("setClearColor:" :void)
  (color (:struct mtl-clear-color)))

(defun encode-clear-pass (command-buffer texture color)
  "Encode one empty Metal 4 render pass which clears TEXTURE to COLOR."
  (destructuring-bind (red green blue alpha) (coerce color 'list)
    (objc:with-owned-objective-c-object
        (descriptor
          (%new-render-pass-descriptor
           (objc:find-objective-c-class "MTL4RenderPassDescriptor")))
      (let* ((attachments (%render-pass-color-attachments descriptor))
             (attachment (%color-attachment-at attachments 0)))
        (%set-color-attachment-texture attachment texture)
        (%set-color-attachment-load-action attachment +load-action-clear+)
        (%set-color-attachment-store-action attachment +store-action-store+)
        (%set-color-attachment-clear-color
         attachment
         (list 'red (coerce red 'double-float)
               'green (coerce green 'double-float)
               'blue (coerce blue 'double-float)
               'alpha (coerce alpha 'double-float)))
        (let ((encoder (render-command-encoder command-buffer descriptor)))
          (unless encoder
            (error "Metal did not create a render command encoder."))
          (end-encoding encoder)
          encoder)))))
