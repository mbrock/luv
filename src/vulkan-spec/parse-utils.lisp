#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

;; todo: I think there is an alexandria function for this...
(defun tokenize (str)
  "Splits a comma-separated string."
  (let ((tokenized (split-sequence:split-sequence #\, str)))
    (if (member nil tokenized)
        nil
        tokenized)))

(defun parse-boolean (node)
  "Checks whether or not the given node NODE holds a string that equals 'true'."
  (let ((str (xps node)))
    (and str (string= str "true"))))

(defun str-not-empty-p (str)
  (and str (> (length str) 0)))

(defun to-upper-snake (str)
  "Transforms a given string to a snake-cased string, but all characters are uppercased.

E.g.: \"VkResult\" becomes \"VK_RESULT\". 
"
  (string-upcase (kebab:to-snake-case str)))

(defun is-vk-result (str)
  "Checks if a string is equal to \"VkResult\"."
  (string= str "VkResult"))

(defun find-tag (tags name postfix)
  "TODO"
  (or
   (find-if (lambda (tag)
              (alexandria:ends-with-subseq
               (concatenate 'string tag postfix)
               name))
            tags)
   ""))

(defun strip-prefix (str prefix)
  (if (and prefix
           (> (length prefix) 0)
           (alexandria:starts-with-subseq prefix str))
      (subseq str (length prefix))
      str))

(defun strip-postfix (str postfix)
  (if (and postfix
           (> (length postfix) 0)
           (alexandria:ends-with-subseq postfix str))
      (subseq str 0 (search postfix str))
      str))

(defun upper-snake-to-pascal-case (str)
  "Transforms a string in uppercased snake-case to a pascal-cased string.

E.g. \"VK_RESULT\" becomes \"VkResult\".
"
  (kebab:to-pascal-case (string-downcase str)))

