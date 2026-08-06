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
           #:main))

(in-package #:luv)

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

(defun main ()
  "Command-line entry point for the initial Vulkan probe."
  (probe)
  (values))
