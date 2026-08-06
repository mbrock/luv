;; luv is going to be an experimental atelier for hacking on Vulkan
;; graphical stuff with Common Lisp.
;;
;; I thought let's start by setting up some kind of ASDF system for
;; it.
;;
;; Then we can start by depending on JolifantoBambla/vk and see if we
;; can get that to work at all; I suppose using the Vulkan SDK from
;; Nixpkgs.
;;
;; That would be a great start!
;;
;; -- mikael

(defpackage #:luv
  (:use #:cl)
  (:export #:probe
           #:surface-probe
           #:yellow-window
           #:main))

(in-package #:luv)

(cffi:defctype raw-vk-surface-khr
  #.(if (= 8 (cffi:foreign-type-size :pointer))
        :pointer
        :uint64))

(defun format-api-version (version)
  "Render a packed Vulkan API VERSION without its variant field."
  (format nil "~D.~D.~D"
          (vk:api-version-major version)
          (vk:api-version-minor version)
          (vk:api-version-patch version)))

(defun physical-device-info (device)
  "Return a small, printable description of a Vulkan physical DEVICE."
  (let ((properties (vk:get-physical-device-properties device)))
    (list :name (vk:device-name properties)
          :type (vk:device-type properties)
          :api-version (format-api-version (vk:api-version properties))
          :vendor-id (vk:vendor-id properties)
          :device-id (vk:device-id properties))))

(defun probe (&optional (stream *standard-output*))
  "Load Vulkan, create an instance, and report the visible physical devices.

The returned property list is also convenient for experimentation at the
REPL.  The Vulkan instance is destroyed before this function returns."
  (let ((loader-version (vk:enumerate-instance-version))
        (create-info
          (vk:make-instance-create-info
           :application-info
           (vk:make-application-info
            :application-name "luv"
            :application-version (vk:make-version 0 0 1)
            :engine-name "luv"
            :engine-version (vk:make-version 0 0 1)
            ;; Request only the baseline API for this loader smoke test.
            :api-version vk:+api-version-1-0+))))
    (vk-utils:with-instance (instance create-info)
      (let ((devices (mapcar #'physical-device-info
                             (vk:enumerate-physical-devices instance))))
        (format stream "Vulkan loader API: ~A~%"
                (format-api-version loader-version))
        (format stream "Physical devices: ~D~%" (length devices))
        (loop for device in devices
              for index from 0
              do (format stream "  [~D] ~A (~A, API ~A)~%"
                         index
                         (getf device :name)
                         (getf device :type)
                         (getf device :api-version)))
        (list :loader-api-version (format-api-version loader-version)
              :physical-devices devices)))))

(defun sdl-vulkan-instance-extensions ()
  "Return the Vulkan instance extensions required by SDL's video backend."
  (cffi:with-foreign-object (count :uint32)
    (let ((names (sdl3:vulkan-get-instance-extensions count)))
      (when (cffi:null-pointer-p names)
        (error "SDL could not report Vulkan instance extensions: ~A"
               (sdl3:get-error)))
      (loop for index below (cffi:mem-ref count :uint32)
            collect (cffi:foreign-string-to-lisp
                     (cffi:mem-aref names :pointer index))))))

(defun surface-capabilities-info (capabilities)
  "Return the immediately useful parts of Vulkan surface CAPABILITIES."
  (let* ((extent (vk:current-extent capabilities))
         (width (vk:width extent))
         (height (vk:height extent)))
    (list :min-image-count (vk:min-image-count capabilities)
          :max-image-count (vk:max-image-count capabilities)
          ;; UINT32_MAX means the swapchain chooses an extent within the
          ;; advertised bounds, which is the normal Wayland result here.
          :current-extent (if (and (= width #xffffffff)
                                   (= height #xffffffff))
                              :variable
                              (list width height)))))

(defun create-sdl-vulkan-surface (window instance)
  "Create an SDL-owned Vulkan surface and return its raw and vk handles."
  (cffi:with-foreign-object (surface-out 'raw-vk-surface-khr)
    (unless (sdl3:vulkan-create-surface
             window
             (vk:raw-handle instance)
             (cffi:null-pointer)
             surface-out)
      (error "SDL could not create a Vulkan surface: ~A" (sdl3:get-error)))
    (let ((raw-surface (cffi:mem-ref surface-out 'raw-vk-surface-khr)))
      (values raw-surface (vk:make-surface-khr-wrapper raw-surface)))))

(defun surface-probe (&optional (stream *standard-output*))
  "Create an SDL window and report the Vulkan surface visible through it.

SDL owns the native windowing details (Wayland on this machine); luv keeps
using vk directly for Vulkan objects and queries.  The window, surface, and
instance are all destroyed before this function returns."
  (unless (sdl3:init :video)
    (error "SDL video initialization failed: ~A" (sdl3:get-error)))
  (unwind-protect
       (let ((window (sdl3:create-window
                      "luv Vulkan surface probe" 800 600
                      '(:vulkan :resizable :hidden))))
         (when (cffi:null-pointer-p window)
           (error "SDL window creation failed: ~A" (sdl3:get-error)))
         (unwind-protect
              (let* ((extensions (sdl-vulkan-instance-extensions))
                     (create-info
                       (vk:make-instance-create-info
                        :application-info
                        (vk:make-application-info
                         :application-name "luv"
                         :application-version (vk:make-version 0 0 1)
                         :engine-name "luv"
                         :engine-version (vk:make-version 0 0 1)
                         :api-version vk:+api-version-1-0+)
                        :enabled-extension-names extensions)))
                (vk-utils:with-instance (instance create-info)
                  (multiple-value-bind (raw-surface surface)
                      (create-sdl-vulkan-surface window instance)
                    (unwind-protect
                         (let* ((device (first (vk:enumerate-physical-devices
                                               instance)))
                                (capabilities
                                  (and device
                                       (vk:get-physical-device-surface-capabilities-khr
                                        device surface)))
                                (formats
                                  (and device
                                       (vk:get-physical-device-surface-formats-khr
                                        device surface)))
                                (present-modes
                                  (and device
                                       (vk:get-physical-device-surface-present-modes-khr
                                        device surface)))
                                (present-queues
                                  (and device
                                       (loop for index below
                                               (length
                                                (vk:get-physical-device-queue-family-properties
                                                 device))
                                             when (vk:get-physical-device-surface-support-khr
                                                   device index surface)
                                               collect index)))
                                (result
                                  (list
                                   :video-driver (sdl3:get-current-video-driver)
                                   :instance-extensions extensions
                                   :device (and device (physical-device-info device))
                                   :capabilities
                                   (and capabilities
                                        (surface-capabilities-info capabilities))
                                   :formats
                                   (mapcar (lambda (surface-format)
                                             (list (vk:format surface-format)
                                                   (vk:color-space surface-format)))
                                           formats)
                                   :present-modes present-modes
                                   :present-queue-families present-queues)))
                           (format stream "SDL video driver: ~A~%"
                                   (getf result :video-driver))
                           (format stream "Vulkan instance extensions: ~{~A~^, ~}~%"
                                   extensions)
                           (format stream "Physical device: ~A~%"
                                   (getf (getf result :device) :name))
                           (let ((extent
                                   (getf (getf result :capabilities)
                                         :current-extent)))
                             (if (eq extent :variable)
                                 (format stream "Surface extent: chosen by swapchain~%")
                                 (format stream "Surface extent: ~{~D~^x~}~%"
                                         extent)))
                           (format stream "Surface formats: ~D; present modes: ~{~A~^, ~}~%"
                                   (length formats) present-modes)
                           (format stream "Present-capable queue families: ~{~D~^, ~}~%"
                                   present-queues)
                           result)
                      (sdl3:vulkan-destroy-surface
                       (vk:raw-handle instance)
                       raw-surface
                       (cffi:null-pointer))))))
           (sdl3:destroy-window window)))
    (sdl3:quit)))
