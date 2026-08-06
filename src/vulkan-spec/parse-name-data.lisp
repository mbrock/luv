#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)

(defun parse-modifiers (modifiers)
  "Parses a string of modifiers for a name node into a list of ARRAY-SIZES and a list of BIT-COUNT values.

Known formats are:
- bit count: \":<number>\"
E.g.:
<member>
  <type>uint32_t</type>
  <name>mask</name>
  :8
</member>

- array size(s): \"[<number>]+\"
E.g one-dimensional array size:
<param>
  const
  <type>float</type>
  <name>blendConstants</name>
  [4]
</param>

E.g. multidimensional array sizes:
<member>
  <type>float</type>
  <name>matrix</name>
  [3][4]
</member>

Ignored formats are:
- \")\"
- \";\"
- array size of a struct member referenced by a constant: \"[\"
E.g.:
<member>
  <type>char</type>
  <name>deviceName</name>
  [
  <enum>VK_MAX_PHYSICAL_DEVICE_NAME_SIZE</enum>
  ]
</member>

Note:array sizes of struct members are handled by PARSE-STRUCT-MEMBER

See PARSE-NAME-DATA
See NAME-DATA
See PARSE-STRUCT-MEMBER
See VULKAN-SPEC
"
  (let ((array-sizes nil)
        (bit-count nil)
        (value modifiers))
    (when (and value
               (> (length value) 0))
      (let ((first-char (subseq value 0 1)))
        (cond
          ((and (not (string= value "["))
                 (string= first-char "["))
           (let ((end-pos 0))
             (loop while (not (= (1+ end-pos) (length value)))
                   do (let ((start-pos (position #\[ value :start end-pos)))
                        (assert start-pos
                                () "could not find '[' in <~a>" value)
                        (setf end-pos (position #\] value :start start-pos))
                        (assert end-pos
                                () "could not find ']' in <~a>" value)
                        (assert (<= (+ start-pos 2) end-pos)
                                () "missing content between '[' and ']' in <~a>" value)                       
                        (push (subseq value (1+ start-pos) end-pos)
                              array-sizes)))))
          ((string= first-char ":")
           (setf bit-count (subseq value 1)))
          (t
           (assert (or (string= first-char "[")
                       (string= first-char ";")
                       (string= first-char ")"))
                   () "unknown modifier <~a>" value)))))
    (values array-sizes bit-count)))

(defun parse-name-data (node)
  "Parses name data from a node into a NAME-DATA instance.

E.g.:
<param>
  const
  <type>float</type>
  <name>blendConstants</name>
  [4]
</param>

See PARSE-MODIFIERS
See NAME-DATA
See VULKAN-SPEC
"
  ;; todo: check attributes
  (let ((name (xps (xpath:evaluate "name" node))))
    (multiple-value-bind (array-sizes bit-count)
        (parse-modifiers (xps (xpath:evaluate "name/following-sibling::text()" node)))
      (make-instance 'name-data
                     :name name
                     :array-sizes array-sizes
                     :bit-count bit-count))))

