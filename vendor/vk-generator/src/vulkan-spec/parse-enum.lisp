#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

;; see VulkanHppGenerator::readEnumConstant
(defun parse-enum-constant (node vk-spec)
  "TODO"
  (let ((name (xps (xpath:evaluate "@name" node)))
        (alias (xps (xpath:evaluate "@alias" node)))
        (comment (xps (xpath:evaluate "@comment" node))))
    (if alias
        (let ((constant (gethash alias (constants vk-spec))))
          (assert constant
                  () "unknown enum constant alias <~a>" alias)
          (setf (gethash name (constants vk-spec))
                (make-instance 'api-constant
                               :name name
                               :alias alias
                               :comment comment
                               :number-value (number-value constant)
                               :string-value (string-value constant)
                               :vk-hpp-name (vk-hpp-name constant)
                               :single-bit-p (single-bit-p constant))))
        (let* ((string-value (xps (xpath:evaluate "@value" node)))
               (number-value (numeric-value string-value)))
          (assert (not (gethash name (constants vk-spec)))
                  () "already specified enum constant <~a>" name)
          (assert number-value
                  () "non-alias enum constant <~a> has no value" name)
          (setf (gethash name (constants vk-spec))
                (make-instance 'api-constant
                               :name name
                               :comment comment
                               :number-value number-value
                               :string-value string-value))))))

(defun get-enum-prefix (name is-bitmaks-p)
  "TODO"
  (cond
    ((is-vk-result name) "VK_")
    (is-bitmaks-p
     (let ((flag-bits-pos (search "FlagBits" name)))
       (assert flag-bits-pos
               () "bitmask <~a> does not contain <FlagBits> as substring")
       (concatenate 'string (to-upper-snake (subseq name 0 flag-bits-pos)) "_")))
    (t
     (concatenate 'string (to-upper-snake name) "_"))))

(defun get-enum-pre-and-postfix (name is-bitmask-p tags)
  (let ((prefix (get-enum-prefix name is-bitmask-p))
        (postfix ""))
    (unless (is-vk-result name)
      (let ((tag (find-if (lambda (tag)
                            (or (alexandria:ends-with-subseq (concatenate 'string tag "_") prefix)
                                (alexandria:ends-with-subseq tag name)))
                          tags)))
        (when tag
          (when (alexandria:ends-with-subseq (concatenate 'string tag "_") prefix)
            (setf prefix (subseq prefix 0 (- (length prefix) (length tag) 1))))
          (setf postfix (concatenate 'string "_" tag)))))
    (values prefix postfix)))

(defun create-enum-vk-hpp-name (name prefix postfix is-bitmask-p tag)
  "TODO"
  (let ((result (concatenate
                 'string
                 "e"
                 (upper-snake-to-pascal-case
                  (strip-postfix (strip-prefix name prefix) postfix)))))
    (when is-bitmask-p
      (setf result (subseq result 0 (search "Bit" result))))
    (when (and (> (length tag) 0)
               (string= (upper-snake-to-pascal-case tag)
                        (subseq result 0 (- (length result) (length tag)))))
      (setf result (subseq result 0 (- (length result) (length tag)))))
    result))

(defun add-enum-alias (enum name alias-name vk-hpp-name)
  "TODO"
  (let ((alias (gethash name (aliases enum))))
    (assert (or (not alias)
                (string= (first alias) alias-name))
            () "enum alias <~a> already listed for a different enum value" alias-name))
  ;; only list aliases that map to different vk-hpp-names
  (unless (find-if (lambda (alias-data)
                     (string= vk-hpp-name (second alias-data)))
                   (alexandria:hash-table-values (aliases enum)))
    (setf (gethash name (aliases enum))
          (list alias-name vk-hpp-name))))

;; see VulkanHppGenerator::readEnum & VulkanHppGenerator::readEnumAlias
(defun parse-enum-value (node enum vk-spec)
  "Parses an enum value node into an ENUM-VALUE instance and stores it in the given ENUM instance.

A node could look like this:
<enum value=\"0\" name=\"VK_FRONT_FACE_COUNTER_CLOCKWISE\"/>

Which could be a child of the following enum node:
<enums name=\"VkFrontFace\" type=\"enum\">
  <enum value=\"0\" name=\"VK_FRONT_FACE_COUNTER_CLOCKWISE\"/>
  <enum value=\"1\" name=\"VK_FRONT_FACE_CLOCKWISE\"/>
</enums>

Note that the created ENUM-VALUE instance is appended at the front of the ENUM-VALUES slot of the given ENUM instance.
Depending on what you want to do with it you might want to reverse the order of the list after parsing all values of an enum like it is done in PARSE-ENUMS.

See PARSE-ENUMS
See ENUM-VALUE
See ENUM
See VULKAN-SPEC
"
  (multiple-value-bind (prefix postfix)
      (get-enum-pre-and-postfix (name enum) (is-bitmask-p enum) (tags vk-spec))
    (let* ((name (xps (xpath:evaluate "@name" node)))
           (alias (xps (xpath:evaluate "@alias" node)))
           (tag (find-tag (tags vk-spec) name postfix))
           (vk-hpp-name
             (create-enum-vk-hpp-name name
                                      prefix
                                      postfix
                                      (is-bitmask-p enum)
                                      tag))
           (comment (xps (xpath:evaluate "@comment" node)))
           (protect (xps (xpath:evaluate "@protect" node))))
      (if alias
          (add-enum-alias enum name alias vk-hpp-name)
          (let* ((bitpos-string (xps (xpath:evaluate "@bitpos" node)))
                 (value-string (xps (xpath:evaluate "@value" node)))
                 (bitpos (numeric-value bitpos-string))
                 (value (numeric-value value-string))
                 (enum-value (find-if (lambda (e)
                                        (string= (vk-hpp-name e) vk-hpp-name))
                                      (enum-values enum))))
            (assert (or (and (not bitpos) value)
                        (and bitpos (not value)))
                    () "invalid set of attributes for enum <~a> name")
            (if enum-value
                (assert (string= (name enum-value) name)
                        () "enum value <~a> maps to same vk-hpp-name as <~a>" name (name enum-value))
                (push (make-instance 'enum-value
                                     :name name
                                     :comment comment
                                     :number-value (or value (ash 1 bitpos))
                                     :string-value (or value-string bitpos-string)
                                     :vk-hpp-name vk-hpp-name
                                     :single-bit-p (not value)
                                     :protect protect)
                      (enum-values enum))))))))

;; see VulkanHppGenerator::readEnums
(defun parse-enums (vk.xml vk-spec)
  "Parses an enum node into an ENUM instance and stores it in the given VULKAN-SPEC instance.

A node could look like this:
<enums name=\"VkFrontFace\" type=\"enum\">
  <enum value=\"0\" name=\"VK_FRONT_FACE_COUNTER_CLOCKWISE\"/>
  <enum value=\"1\" name=\"VK_FRONT_FACE_CLOCKWISE\"/>
</enums>

The enum's child nodes are parsed into ENUM-VALUE instances in the process.

See PARSE-ENUM-CONSTANT
See PARSE-ENUM-VALUE
See ENUM
See ENUM-VALUE
See VULKAN-SPEC
"
  (xpath:do-node-set (node (xpath:evaluate "/registry/enums[@name=\"API Constants\"]/enum" vk.xml))
    (parse-enum-constant node vk-spec))
  (xpath:do-node-set (node (xpath:evaluate "/registry/enums[not(@name=\"API Constants\")]" vk.xml))
    (let* ((name (xps (xpath:evaluate "@name" node)))
           (type (xps (xpath:evaluate "@type" node)))
           (comment (xps (xpath:evaluate "@comment" node)))
           (is-bitmask-p (string= type "bitmask"))
           (bitwidth (xps (xpath:evaluate "@bitwidth" node)))
           (enum (gethash name (enums vk-spec))))
      (unless enum
        (warn "enum <~a> is not listed as enum in the types section" name)
        (setf (gethash name (enums vk-spec))
              (make-instance 'enum
                             :name name
                             :is-bitmask-p is-bitmask-p
                             :bitwidth bitwidth
                             :comment comment))
        (assert (not (gethash name (types vk-spec)))
                () "enum <~a> already specified as a type" name)
        (setf (gethash name (types vk-spec))
              (make-instance 'vk-type
                             :name name
                             :category :enum))
        (setf enum (gethash name (enums vk-spec))))
      (assert (not (enum-values enum))
              () "enum <~a> already holds values" name)
      (setf (is-bitmask-p enum) is-bitmask-p)
      (setf (bitwidth enum) bitwidth)
      (setf (comment enum) comment)
      (when is-bitmask-p
        (assert (search "FlagBits" name)
                () "enum <~a> does not contain <FlagBits> as substring"))
      (xpath:do-node-set (enum-value-node (xpath:evaluate "enum" node))
          (parse-enum-value enum-value-node enum vk-spec))
      (setf (enum-values enum)
            (remove-duplicates (reverse (enum-values enum)))))))

