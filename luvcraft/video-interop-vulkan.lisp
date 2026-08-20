;;; FFmpeg Vulkan decode configuration and AVVkFrame plane adoption.

(in-package #:luvcraft)

#-darwin
(defun vulkan-video-queue-flags-value (flags)
  (loop for flag in flags
        sum (ecase flag
              (:graphics #x1) (:compute #x2) (:transfer #x4)
              (:sparse-binding #x8) (:video-decode #x20)
              (:video-encode #x40))))

#-darwin
(defmethod video-decode-configuration
    ((device luv::vulkan-gpu-device) hardware-policy)
  (declare (ignore hardware-policy))
  (let* ((graphics-family (luv::vulkan-device-queue-family device))
         (video-family (luv::vulkan-device-video-queue-family device))
         (families (luv.vulkan:physical-device-queue-families
                    (luv::vulkan-device-physical-device device)))
         (graphics-flags (luv.vulkan:queue-family-flags
                          (nth graphics-family families)))
         (video-flags (and video-family
                           (luv.vulkan:queue-family-flags
                            (nth video-family families))))
         (extensions (luv::vulkan-device-extension-names device)))
    (when (and video-family
               (member "VK_KHR_video_queue" extensions :test #'string=)
               (member "VK_KHR_video_decode_queue" extensions :test #'string=))
      (values
       :vulkan
       (list
        :instance (luv::vulkan-device-instance device)
        :physical-device (luv::vulkan-device-physical-device device)
        :device (luv::vulkan-handle device)
        :get-instance-proc-addr
        (cffi:foreign-symbol-pointer
         "vkGetInstanceProcAddr" :library 'luv.vulkan::vulkan-loader)
        :instance-extensions (luv::vulkan-device-instance-extension-names device)
        :device-extensions extensions
        :queue-families
        (list
         (list :index graphics-family
               :flags (vulkan-video-queue-flags-value graphics-flags))
         (list :index video-family
               :flags (vulkan-video-queue-flags-value video-flags)
               :video-capabilities
               (logior
                (if (member "VK_KHR_video_decode_h264" extensions
                            :test #'string=)
                    #x1 0)
                (if (member "VK_KHR_video_decode_h265" extensions
                            :test #'string=)
                    #x2 0)))))))))

#-darwin
(defclass vulkan-video-frame-importer (video-frame-importer) ()
  (:documentation "Stateless importer for AVVkFrame-backed pictures."))

#-darwin
(defmethod make-video-frame-importer ((device luv::vulkan-gpu-device))
  (make-instance 'vulkan-video-frame-importer :device device))

#-darwin
(defun vulkan-decoded-frame-element (pointer slot type index)
  (cffi:mem-aref
   (cffi:foreign-slot-pointer pointer '(:struct libav::av-vulkan-frame) slot)
   type index))

#-darwin
(defmethod adopt-decoded-video-frame
    ((importer vulkan-video-frame-importer) frame width height)
  (let ((vulkan-frame (libav:frame-vulkan-frame frame)))
    (unless vulkan-frame
      (error "FFmpeg did not return an AVVkFrame."))
    (let* ((device (video-frame-importer-device importer))
           (image
             (vulkan-decoded-frame-element
              vulkan-frame 'libav::images :pointer 0))
           (layout-value
             (vulkan-decoded-frame-element
              vulkan-frame 'libav::layouts :uint32 0))
           (layout
             (or (cffi:foreign-enum-keyword
                  'luv.vulkan::image-layout layout-value :errorp nil)
                 :general))
           (semaphore
             (vulkan-decoded-frame-element
              vulkan-frame 'libav::semaphores :pointer 0))
           (semaphore-value
             (vulkan-decoded-frame-element
              vulkan-frame 'libav::semaphore-values :uint64 0)))
      (labels
          ((make-plane (plane)
             (let ((retained (libav:clone-frame frame))
                   (release-owner nil)
                   (adopted-p nil))
               (unwind-protect
                    (progn
                      (setf release-owner
                            (make-video-frame-importer-owner-release
                             importer
                             (lambda () (libav:release-frame retained))))
                      (let* ((retained-vulkan-frame
                             (libav:frame-vulkan-frame retained))
                           (submitted
                             (lambda (new-layout new-value)
                               (setf
                                (cffi:mem-aref
                                 (cffi:foreign-slot-pointer
                                  retained-vulkan-frame
                                  '(:struct libav::av-vulkan-frame)
                                  'libav::layouts)
                                 :uint32 0)
                                (cffi:foreign-enum-value
                                 'luv.vulkan::image-layout new-layout)
                                (cffi:mem-aref
                                 (cffi:foreign-slot-pointer
                                  retained-vulkan-frame
                                  '(:struct libav::av-vulkan-frame)
                                  'libav::semaphore-values)
                                 :uint64 0)
                                new-value)))
                           (plane-width (if (zerop plane) width (ceiling width 2)))
                           (plane-height
                             (if (zerop plane) height (ceiling height 2)))
                           (descriptor-format
                             (if (zerop plane) :r8-unorm :rg8-unorm))
                           (native-format
                             (if (zerop plane) :r8-unorm :r8g8-unorm))
                           (texture
                             (adopt-native-texture
                              device
                              (list :image image
                                    :format native-format
                                    :aspect (if (zerop plane)
                                                :plane-0 :plane-1)
                                    :layout layout
                                    :semaphore semaphore
                                    :semaphore-value semaphore-value
                                    :submitted submitted)
                              release-owner
                              (make-texture-descriptor
                               :label (format nil "FFmpeg Vulkan plane ~D" plane)
                               :size (list plane-width plane-height)
                               :dimensions :2d
                               :format descriptor-format
                               :usage '(:texture-binding)))))
                        (unless texture
                          (error "The HAL did not adopt Vulkan video plane ~D."
                                 plane))
                        (setf adopted-p t)
                        texture))
                 (unless adopted-p
                   (if release-owner
                       (funcall release-owner)
                       (libav:release-frame retained)))))))
        (make-decoded-video-picture-from-planes device 2 #'make-plane)))))
