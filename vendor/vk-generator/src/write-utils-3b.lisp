;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; generate.lisp --- generate CFFI bindings from vk.xml file.
;;;
;;; Copyright (c) 2016, Bart Botta  <00003b@gmail.com>
;;;   All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package :vk-generator)

(defun fix-type-name (name vendor-ids)
  "Converts a given camel-cased NAME of a type from the C Vulkan API to a lispy name.

E.g.: \"VkPhysicalDevice\" becomes \"PHYSICAL-DEVICE\"

See *SPECIAL-WORDS*
See TAGS
"
  (if (string= "function" name)
      "function-handle"
      (if (not (stringp name))
             name
             (let* ((start (if (alexandria:starts-with-subseq "Vk" name) 2 0)))
               (when (zerop start)
                 (setf name (ppcre:regex-replace-all "PFN_vk" name "Pfn")))
               (cffi:translate-camelcase-name (subseq name start)
                                              :special-words (append *special-words*
                                                                     vendor-ids))))))

(defun fix-function-name (name vendor-ids)
  "Converts a given camel-cased NAME of a function from the C Vulkan API to a lispy name.

E.g.: \"vkGetPhysicalDeviceMemoryProperties\" becomes \"GET-PHYSICAL-DEVICE-MEMORY-PROPERTIES\"

See *SPECIAL-WORDS*
See TAGS
"
  (let* ((start (if (alexandria:starts-with-subseq "vk" name) 2 0)))
    (cffi:translate-camelcase-name (subseq name start)
                                   :special-words (append *special-words*
                                                          vendor-ids))))

(defun fix-bit-name (name vendor-ids &key (prefix "VK_"))
  "Converts a given camel-cased NAME of a special bit in a bitmask from the C Vulkan API to a lispy name.

E.g.: \"VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT\" becomes \"MEMORY-PROPERTY-DEVICE-LOCAL\"

See TAGS
"
  ;; fixme: cache compiled regex instead of rebuilding from string on every call
  (string-trim
   '(#\-)
   (substitute #\- #\_
               (ppcre:regex-replace-all (format nil "(^~a|_BIT(~{_~a~^|~})?$)"
                                                prefix vendor-ids)
                                        name ""))))

(defun find-enum-prefix (fixed-name enum-values tags)
  ;; find longest prefix out of VK_, name - vendor
  (let* ((prefix "VK_")
         (p (loop for v in tags
                  thereis (when (and
                                 (>= (length fixed-name) (length v))
                                 (string= v (subseq fixed-name (- (length fixed-name) (length v)))))
                            (search v fixed-name :from-end t))))
         ;; todo: if flagbits 2: <>_FLAG_BITS_2_ -> <>_2_
         (n (format nil "VK_~a"
                    (substitute #\_ #\-
                                (if p
                                    (subseq fixed-name 0 (- p 1))
                                    fixed-name))))
         (l (loop for enum-value in enum-values
                  minimize (or (mismatch n (name enum-value)) 0))))
    (when (> l (length prefix))
      (setf prefix (subseq n 0 l)))
    (when (alexandria:ends-with-subseq "FLAG_BITS_2" n)
      (setf prefix (format nil "~a2"
                           (subseq n 0 (search "FLAG_BITS_2" n)))))
    prefix))

(defun make-keyword (name)
  (alexandria:make-keyword (string-upcase name)))

(defun make-const-keyword (name)
  (let ((start (if (alexandria:starts-with-subseq "VK_" name) 3 0)))
    (alexandria:make-keyword
     (subseq (substitute #\- #\_ name) start))))
