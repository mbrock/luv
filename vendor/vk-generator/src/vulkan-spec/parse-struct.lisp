#|
 Copyright(c) 2020 - Lukas Herzberger <herzberger.lukas@gmail.com>
 SPDX-License-Identifier: MIT

 Copyright(c) 2015-2020 - NVIDIA CORPORATION
 SPDX-License-Identifier: Apache-2.0
|#

(in-package #:vulkan-spec)


(defun determine-sub-struct (structure vk-spec)
  "TODO"
  (loop for other-struct being each hash-values of (structures vk-spec)
        when (and (string= (name structure) (name other-struct))
                      (< (length (members other-struct))
                         (length (members structure)))
                      (not (string= (first (members other-struct))
                                    "sType"))
                      (every (lambda (m1 m2)
                               (and (string= (type-name m1)
                                             (type-name m2))
                                    (string= (name m1)
                                             (name m2))))
                             (members other-struct)
                             (subseq (members structure)
                                     0 (length (members other-struct)))))
        return (name other-struct)))

(defparameter *ignore-lens*
  '("null-terminated"
    "2*VK_UUID_SIZE"
    "2*ename:VK_UUID_SIZE"
    "latexmath:[2 \\times \\mathtt{VK\\_UUID\\_SIZE}]")
  "A list of <len> attributes in <member> tags.")

(defun parse-struct-member (node structure vk-spec)
  "TODO"
  (let* ((name-data (parse-name-data node))
         (type-info (parse-type-info node))
         (enum (xps (xpath:evaluate "enum" node)))
         (len (xps (xpath:evaluate "@len" node)))
         (no-autovalidity-p (parse-boolean (xpath:evaluate "@noautovalidity" node)))
         (optional-p (parse-boolean (xpath:evaluate "@optional" node)))
         (selection (xps (xpath:evaluate "@selection" node)))
         (selector (xps (xpath:evaluate "@selector" node)))
         (member-values (tokenize (xps (xpath:evaluate "@values" node))))
         (comment (xps (xpath:evaluate "comment" node)))
         (member-data (make-instance 'member-data
                                     :name (name name-data)
                                     :comment comment
                                     :array-sizes (array-sizes name-data)
                                     :bit-count (bit-count name-data)
                                     :type-info type-info
                                     :no-autovalidity-p no-autovalidity-p
                                     :optional-p optional-p
                                     :selection selection
                                     :selector selector
                                     :allowed-values member-values)))
    (assert (not (find-if (lambda (m) (string= (name member-data) (name m)))
                          (members structure)))
            () "structure member name <~a> already used" (name member-data))
    (when enum
      ;; this is fucked up: enum/preceding-sibling::text() is always NIL, so let's hope that <name> always comes before <enum>...
      (let ((enum-prefix (xps (xpath:evaluate "name/following-sibling::text()" node)))
            (enum-postfix (xps (xpath:evaluate "enum/following-sibling::text()" node))))
        (assert (and enum-prefix (string= enum-prefix "[")
                     enum-postfix (string= enum-postfix "]"))
                () "structure member array specification is ill-formatted: <~a>" enum)
        (push enum (array-sizes member-data))))
    (when len
      (setf (len member-data) (tokenize len))
      (assert (<= (length (len member-data)) 2)
              () "member attribute <len> holds unknown number of data: ~a" (length (len member-data)))
      (let* ((first-len (first (len member-data)))
             (len-member (find-if (lambda (m) (string= first-len (name m)))
                                  (members structure))))
        (assert (or len-member
                    (find first-len *ignore-lens* :test #'string=)
                    (string= first-len "latexmath:[\\textrm{codeSize} \\over 4]")
                    (string= first-len "latexmath:[\\lceil{\\mathit{rasterizationSamples} \\over 32}\\rceil]"))
                () "member attribute <len> holds unknown value <~a>" first-len)
        (when len-member
          (assert (and (not (prefix (type-info len-member)))
                       (not (postfix (type-info len-member))))
                  () "member attribute <len> references a member of unexpected type <~a>" (type-info len-member)))
        (when (< 1 (length (len member-data)))
          (assert (find (second (len member-data)) '("1" "null-terminated") :test #'string=)
                  () "member attribute <len> holds unknown second value <~a>" (second (len member-data))))))
    (when selection
      (assert (is-union-p structure)
              () "attribute <selection> is used with non-union structure."))
    (when selector
      (let ((member-selector (find-if (lambda (m) (string= selector (name m)))
                                      (members structure))))
        (assert member-selector
                () "member attribute <selector> holds unknown value <~a>" selector)
        (assert (gethash (get-type-name member-selector) (enums vk-spec))
                () "member attribute references unknown enum type <~a>" (get-type-name member-selector))))
    member-data))

(defun parse-struct (node vk-spec)
  "TODO"
  (let ((alias (xps (xpath:evaluate "@alias" node)))
        (name (xps (xpath:evaluate "@name" node))))
    (if alias
        (let ((struct (gethash alias (structures vk-spec))))
          (assert struct
                  () "missing alias <~a>" alias)
          (assert (not (find name (aliases struct)))
                  () "struct <~a> already uses alias <~a>" alias name)
          (push name (aliases struct))
          (assert (not (gethash name (structure-aliases vk-spec)))
                  () "structure alias <~a> already used" name)
          (setf (gethash name (structure-aliases vk-spec))
                alias)
          (assert (not (gethash name (types vk-spec)))
                  () "struct <~a> already specified as a type" name)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category :struct)))
        (let ((allow-duplicate-p (parse-boolean (xpath:evaluate "@allowduplicate" node)))
              (is-union-p (string= (xps (xpath:evaluate "@category" node)) "union"))
              (returned-only-p (parse-boolean (xpath:evaluate "@returnedonly" node)))
              (struct-extends (tokenize (xps (xpath:evaluate "@structextends" node)))))
          (assert name
                  () "struct has no name")
          ;; todo: this should be an assert in a future version
          (when (and allow-duplicate-p
                     (> (length struct-extends) 0))
            (warn "attribute <allowduplicate> is true, but no structures are listed in <structextends>"))
          (assert (not (gethash name (structures vk-spec)))
                  () "struct <~a> already specified" name)
          (setf (gethash name (structures vk-spec))
                (make-instance 'struct
                               :name name
                               :struct-extends struct-extends
                               :allow-duplicate-p allow-duplicate-p
                               :returned-only-p returned-only-p
                               :is-union-p is-union-p))
          (xpath:do-node-set (member-node (xpath:evaluate "member" node))
            (push (parse-struct-member member-node
                                       (gethash name (structures vk-spec))
                                       vk-spec)
                  (members (gethash name (structures vk-spec)))))
          (setf (members (gethash name (structures vk-spec)))
                (reverse (members (gethash name (structures vk-spec)))))
          (setf (sub-struct (gethash name (structures vk-spec)))
                (determine-sub-struct (gethash name (structures vk-spec))
                                      vk-spec))
          (setf (extended-structs vk-spec)
                (remove-duplicates
                 (append struct-extends (extended-structs vk-spec))
                 :test #'string=))
          (assert (not (gethash name (types vk-spec)))
                  () "struct <~a> already specified as a type" name)
          (setf (gethash name (types vk-spec))
                (make-instance 'vk-type
                               :name name
                               :category (if is-union-p
                                             :union
                                             :struct)))))))
