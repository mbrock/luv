;;; VideoToolbox/CoreVideo frame adoption for the Metal HAL backend.

(in-package #:luvcraft)

#+darwin
(progn
  (cffi:define-foreign-library video-interop-core-video
    (:darwin (:framework "CoreVideo")))
  (cffi:define-foreign-library video-interop-core-foundation
    (:darwin (:framework "CoreFoundation")))
  (cffi:defcfun ("CVMetalTextureCacheCreate"
                 %video-metal-texture-cache-create) :int
    (allocator :pointer) (cache-attributes :pointer) (device :pointer)
    (texture-attributes :pointer) (cache :pointer))
  (cffi:defcfun ("CVMetalTextureCacheCreateTextureFromImage"
                 %video-metal-texture-from-image) :int
    (allocator :pointer) (cache :pointer) (image :pointer)
    (texture-attributes :pointer) (pixel-format :uint64)
    (width :size) (height :size) (plane :size) (texture :pointer))
  (cffi:defcfun ("CVMetalTextureGetTexture"
                 %video-metal-texture-get-texture) :pointer
    (texture :pointer))
  (cffi:defcfun ("CVPixelBufferGetPlaneCount"
                 %video-pixel-buffer-plane-count) :size
    (buffer :pointer))
  (cffi:defcfun ("CVPixelBufferGetWidthOfPlane"
                 %video-pixel-buffer-plane-width) :size
    (buffer :pointer) (plane :size))
  (cffi:defcfun ("CVPixelBufferGetHeightOfPlane"
                 %video-pixel-buffer-plane-height) :size
    (buffer :pointer) (plane :size)))

#+darwin
(defmethod video-decode-configuration
    ((device luv::metal-gpu-device) hardware-policy)
  (declare (ignore device))
  ;; LIBAV interprets :AUTO and :REQUIRED as VideoToolbox policies on Darwin.
  (values hardware-policy nil))

#+darwin
(defclass metal-video-frame-importer (video-frame-importer)
  ((texture-cache
    :initarg :texture-cache
    :accessor metal-video-frame-importer-texture-cache))
  (:documentation
   "A CVMetalTextureCache retained across all pictures of one video screen."))

#+darwin
(defmethod make-video-frame-importer ((device luv::metal-gpu-device))
  (cffi:use-foreign-library video-interop-core-video)
  (cffi:use-foreign-library video-interop-core-foundation)
  (cffi:with-foreign-object (cell :pointer)
    (setf (cffi:mem-ref cell :pointer) (cffi:null-pointer))
    (let ((status
            (%video-metal-texture-cache-create
             (cffi:null-pointer) (cffi:null-pointer)
             (luv.objective-c:objective-c-pointer
              (luv::metal-native-object device))
             (cffi:null-pointer) cell)))
      (unless (zerop status)
        (error "CVMetalTextureCacheCreate failed with status ~D." status))
      (let ((cache (cffi:mem-ref cell :pointer))
            (completed-p nil))
        (when (cffi:null-pointer-p cache)
          (error "CVMetalTextureCacheCreate returned no texture cache."))
        (unwind-protect
             (let ((importer
                     (make-instance 'metal-video-frame-importer
                                    :device device :texture-cache cache)))
               (setf completed-p t)
               importer)
          (unless completed-p
            (cffi:foreign-funcall "CFRelease" :pointer cache :void)))))))

#+darwin
(defmethod release-video-frame-importer-native-state
    ((importer metal-video-frame-importer))
  (when (metal-video-frame-importer-texture-cache importer)
    (cffi:foreign-funcall
     "CFRelease" :pointer
     (metal-video-frame-importer-texture-cache importer) :void)
    (setf (metal-video-frame-importer-texture-cache importer) nil))
  (values))

#+darwin
(defun make-metal-decoded-video-plane-texture
    (importer pixel-buffer plane format native-format)
  (let* ((device (video-frame-importer-device importer))
         (width (%video-pixel-buffer-plane-width pixel-buffer plane))
         (height (%video-pixel-buffer-plane-height pixel-buffer plane)))
    (cffi:with-foreign-object (cell :pointer)
      (setf (cffi:mem-ref cell :pointer) (cffi:null-pointer))
      (let ((status
              (%video-metal-texture-from-image
               (cffi:null-pointer)
               (metal-video-frame-importer-texture-cache importer)
               pixel-buffer (cffi:null-pointer) native-format
               width height plane cell)))
        (unless (zerop status)
          (let ((owner (cffi:mem-ref cell :pointer)))
            (unless (cffi:null-pointer-p owner)
              (cffi:foreign-funcall
               "CFRelease" :pointer owner :void)))
          (error "CVMetalTexture creation for plane ~D failed with status ~D."
                 plane status))
        (let ((owner (cffi:mem-ref cell :pointer))
              (release-owner nil)
              (adopted-p nil))
          (when (cffi:null-pointer-p owner)
            (error "CVMetalTexture creation for plane ~D returned no owner."
                   plane))
          (unwind-protect
               (progn
                 (setf release-owner
                       (make-video-frame-importer-owner-release
                        importer
                        (lambda ()
                          (cffi:foreign-funcall
                           "CFRelease" :pointer owner :void))))
                 (let* ((native-pointer
                        (%video-metal-texture-get-texture owner))
                      (_
                        (when (cffi:null-pointer-p native-pointer)
                          (error "CVMetalTexture plane ~D has no MTLTexture."
                                 plane)))
                      (native
                        (luv.objective-c:wrap-objective-c-object
                         native-pointer :ownership :borrowed
                         :protocol-name "MTLTexture"))
                      (texture
                        (adopt-native-texture
                         device native release-owner
                         (make-texture-descriptor
                          :label (format nil "VideoToolbox plane ~D" plane)
                          :size (list width height)
                          :dimensions :2d :format format
                          :usage '(:texture-binding)))))
                   (declare (ignore _))
                   (unless texture
                     (error "The HAL did not adopt Metal video plane ~D."
                            plane))
                   (setf adopted-p t)
                   texture))
            (unless adopted-p
              (if release-owner
                  (funcall release-owner)
                  (cffi:foreign-funcall
                   "CFRelease" :pointer owner :void)))))))))

#+darwin
(defmethod adopt-decoded-video-frame
    ((importer metal-video-frame-importer) frame width height)
  (declare (ignore width height))
  (let ((pixel-buffer (libav:frame-videotoolbox-pixel-buffer frame)))
    (unless (and pixel-buffer
                 (= 2 (%video-pixel-buffer-plane-count pixel-buffer)))
      (error "VideoToolbox did not return the expected two-plane NV12 surface."))
    (make-decoded-video-picture-from-planes
     (video-frame-importer-device importer) 2
     (lambda (plane)
       (ecase plane
         ;; MTLPixelFormatR8Unorm and MTLPixelFormatRG8Unorm.
         (0 (make-metal-decoded-video-plane-texture
             importer pixel-buffer 0 :r8-unorm 10))
         (1 (make-metal-decoded-video-plane-texture
             importer pixel-buffer 1 :rg8-unorm 30)))))))
