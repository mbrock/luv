#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

(defun parse-copyright (vk.xml vk-spec)
  "Searches and parses the copyright notice from the top level comments in the given vk.xml and binds the result to the VULKAN-LICENCE-HEADER slot in the given VULKAN-SPEC instance.

E.g.:
...
<registry>
  <comment>
Copyright 2015-2021 The Khronos Group Inc.

SPDX-License-Identifier: Apache-2.0 OR MIT
  </comment>
</registry>
...

See VULKAN-LICENCE-HEADER
See VULKAN-SPEC
"
  (xpath:do-node-set (node (xpath:evaluate "/registry/comment" vk.xml))
    (let ((text (xps node)))
      (when (search "Copyright" text)
        (setf (vulkan-license-header vk-spec)
              (string-trim '(#\Space #\Tab #\Newline #\return)
                           (subseq text 0 (search "This file, vk.xml, is the" text)))))))
  (assert (vulkan-license-header vk-spec)
          () "no copyright notice found in vk.xml"))

(defun parse-platforms (vk.xml vk-spec)
  "Parses platform tags in the given vk.xml into PLATFORM instances and stores them in the hash map bound to the PLATFORMS slot of the given VULKAN-SPEC instance.

E.g.:
...
<platforms comment=\"...\">
  <platform name=\"xlib\" protect=\"VK_USE_PLATFORM_XLIB_KHR\" comment=\"X Window System, Xlib client library\"/>
  ...
</patforms>

See PLATFORM
See PLATFORMS
See VULKAN-SPEC
"
  (xpath:do-node-set (node (xpath:evaluate "/registry/platforms/platform" vk.xml))
    (let ((name (xps (xpath:evaluate "@name" node)))
          (protect (xps (xpath:evaluate "@protect" node))))
      (assert (not (find-if (lambda (p)
                              (string= (protect p) protect))
                            (alexandria:hash-table-values (platforms vk-spec))))
              () "platform protect <~a> already specified" protect)
      (assert (not (gethash name (platforms vk-spec)))
              () "platform name <~a> already specified" name)
      (setf (gethash name (platforms vk-spec))
            (make-instance 'platform
                           :name name
                           :protect protect)))))

(defun parse-tags (vk.xml vk-spec)
  "Parses all tags in the given vk.xml and stores them in the list of tags bound to the TAGS slot of the given VULKAN-SPEC instance.

See TAGS
See VULKAN-SPEC
"
  (xpath:do-node-set (node (xpath:evaluate "/registry/tags/tag" vk.xml))
    (let ((name (xps (xpath:evaluate "@name" node))))
      (assert (not (find name (tags vk-spec)))
              () "tag named <~a> has already been specified")
      (push name (tags vk-spec)))))

;; see VulkanHppGenerator::addMissingFlagBits
(defun add-missing-flag-bits (require-data referenced-in vk-spec)
  "Adds *FlagBits which are missing in the \"types\" section of the vk.xml to a given VULKAN-SPEC instance."
  (loop for require in require-data
        for new-types = nil do
        (loop for type in (types require)
              for bitmask = (gethash type (bitmasks vk-spec))
              when (and bitmask (not (requires bitmask))) do
              (let* ((mask-name (name bitmask))
                     (pos (search "Flags" mask-name)))
                (assert pos ()
                        "bitmask <~a> does not contain <Flags> as a substring" mask-name)
                (let ((flag-bits (format nil "~aBit~a"
                                         (subseq mask-name 0 (+ pos 4))
                                         (subseq mask-name (+ pos 4)))))
                  (setf (requires bitmask) flag-bits)
                  (if (not (gethash flag-bits (enums vk-spec)))
                      (progn
                        (setf (gethash flag-bits (enums vk-spec))
                              (make-instance 'enum
                                             :name flag-bits
                                             :is-bitmask-p t))
                        (assert (not (gethash flag-bits (types vk-spec))) ()
                                "bitmask <~a> already specified as a type" flag-bits)
                        (setf (gethash flag-bits (types vk-spec))
                              (make-instance 'vk-type
                                             :name flag-bits
                                             :category :bitmask
                                             :referenced-in referenced-in)))
                      (assert (gethash flag-bits (types vk-spec)) ()
                              "bitmask <~a> is not backed by a type" flag-bits))
                  (if (find-if (lambda (require-type)
                                 (string= require-type flag-bits))
                               (types require))
                      (warn "flag bits <~a> not specified in types section, but already present in require-data" flag-bits)
                      (push flag-bits new-types)))))
        (setf (types require)
              (concatenate 'list
                           (types require)
                           (reverse new-types)))))

(defun parse-vk-xml (version vk-xml-pathname)
  "Parses the vk.xml file at VK-XML-PATHNAME into a VK-SPEC instance."
  (let* ((vk.xml (cxml:parse-file vk-xml-pathname
                                  (cxml:make-whitespace-normalizer
                                   (stp:make-builder))))
         (vk-spec (make-instance 'vulkan-spec)))
    ;; insert default handle for create-instance and such
    (setf (gethash "" (handles vk-spec))
          (make-instance 'handle
                         :name ""))

    ;; parse vk.xml
    (parse-copyright vk.xml vk-spec)
    (parse-platforms vk.xml vk-spec)
    (parse-tags vk.xml vk-spec)
    (parse-types vk.xml vk-spec)
    (parse-enums vk.xml vk-spec)
    (parse-commands vk.xml vk-spec)
    (parse-features vk.xml vk-spec)
    (parse-extensions vk.xml vk-spec)

    (loop for feature being the hash-values of (features vk-spec)
          using (hash-key feature-name)
          do (add-missing-flag-bits (require-data feature) feature-name vk-spec))
    (loop for extension being the hash-values of (extensions vk-spec)
          using (hash-key extension-name)
          do (add-missing-flag-bits (require-data extension) extension-name vk-spec))
    
    ;; reverse order of lists 
    (setf (extended-structs vk-spec)
          (remove-duplicates (reverse (extended-structs vk-spec))))
    (setf (includes vk-spec)
          (reverse (includes vk-spec)))
    (setf (tags vk-spec)
          (reverse (tags vk-spec)))
    
    vk-spec))
